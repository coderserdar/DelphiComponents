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
// $Log:  17547: mcmImageFile.pas
//
//    Rev 1.64    2014-02-02 21:09:58  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.63    25-10-2009 17:27:00  mcm
// Support for Delphi 2010
//
//    Rev 1.62    26-08-2009 23:22:34  mcm    Version: IMG 3.2
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.61    11-08-2009 10:14:56  mcm
// Delphi 2009 support, modified use of String/AnsiString.
//
//    Rev 1.60    02-01-2009 18:58:20  mcm    Version: IMG 3.3
// Delphi 2009 Support
//
//    Rev 1.59    01-01-2009 19:02:00  mcm
// Initial JBIG implementation
//
//    Rev 1.58    24-11-2007 13:25:16  mcm
// Fix reading TIFF images, where the SHORT tags bit 31-16 were not zero.
// Fixed reading PNG images that doesn't set the "Final Data Set" flag.
//
//    Rev 1.57    20-08-2007 20:28:26  mcm
// Added support for Delphi 2007
//
//    Rev 1.56    05-11-2006 18:55:52  mcm    Version: IMG 3.1
// Added extra check when out of sub images in a multi-paged file.
// Changed FRestartCount from word to longword, fixes a problem in JPEG.
//
//    Rev 1.55    22-05-2006 20:23:30  mcm
// Corrected an endless loop occuring when reading faulty JPEG compressed TIFF
// files.
//
//    Rev 1.54    30-04-2006 19:18:00  mcm
// Corrected reading GIF files having a local palette.
//
//    Rev 1.53    24-04-2006 20:39:28  mcm
// Corrected reading large JPEG compressed TIFF files.
//
//    Rev 1.52    02/04/2006 11:45:04  mcm
// Fixed an error occuring when reading GIF.
//
//    Rev 1.51    18-03-2006 18:14:46  mcm    Version: IMG 2.16
// Re-enabled reading image files having unknown extensions.
// Corrected a potential problem using the Copy() method.
//
//    Rev 1.50    10-03-2006 18:46:56  mcm    Version: IMG 2.16
// Corrected a PCX verson 3 BW palette initialization error.
//
//    Rev 1.49    18-12-2005 15:51:52  mcm    Version: IMG 2.11
// Fixed saving a milti-paged TIFF file using JPEG compression.
//
//    Rev 1.48    12-12-2005 23:32:16  mcm    Version: IMG 2.11
// Correction to TIFF/JPEG compression of 8 bit grey scale images
//
//    Rev 1.47    12-12-2005 20:31:20  mcm    Version: IMG 2.11
// Added TAG's required for JPEG compression in TIFF.
//
//    Rev 1.46    11-12-2005 20:54:08  mcm
// Implemented JPEG compresson in TIFF writer.
//
//    Rev 1.45    03-12-2005 20:02:36  mcm    Version: IMG 2.11
// Added support for reading JPEG compressed TIFF files (post TIFF v/6.0)
//
//    Rev 1.44    30-11-2005 21:38:36  mcm
// Added support for TIFF, Modified Huffman, Reverse ordered bits.
//
//    Rev 1.43    22/11/2005 18:31:14  mcm
// Enabled reading BMP files with a 64 byte large header.
//
//    Rev 1.42    20/11/2005 09:34:54  mcm    Version: IMG 2.10
// Corrected reading ASCII Portable PBM and PGM.
//
//    Rev 1.41    18-10-2005 19:59:18  mcm
// TmcmPCXImage correction - saves 1 and 4 bit images in version 2.8 w/palette
// format.
//
//    Rev 1.40    14/10/2005 22:45:56  mcm
// Added a fix for PCX 1 bit images with incorrect palette data stored in file.
//
//    Rev 1.39    08/10/2005 10:49:56  mcm
// Added support for reading TIFF files embedding "Old JPEG" compressed images.
//
//   Rev 1.38    27-09-2005 18:54:18  mcm    Version: IMG 2.9
// Fixed override warning.

//
//   Rev 1.37    04/09/2005 10:19:52  mcm    Version: IMG 2.9
// Changed UnregisterAllFormat to UnregisterAllFormats.

//
//   Rev 1.36    23-08-2005 20:33:42  mcm    Version: IMG 2.9
// Added support for registering external file formats.
// Added new class TmcmImageFileClass, TmcmFileFormatItem and TmcmFileFormatList.
// Added and removed methods on TmcmImageFileMgr to support external file
// formats. Remove Get/SetSupportedFormats which are replaced by new methods
// RegisterFileFormat, UnregisterAllFileFormats and UnregisterFileformats.
// Modified GetFormatsFromColor and GetCompressionFromColor and added
// GetCompressionName and GetFormatList to adjust to the new format register
// concept.
// Fixed zero'ing the image data when creating a new FDibHandle in
// TmcmImageFile.CreateDib.
// Split TmcmPxMImage into TmcmPBMImage, TmcmPGMImage and TmcmPPMImage classes.
// Added static methods GetColorFormats, GetCompressionFromColor and
// GetCompressionName to TmcmImageFile and all descendent classes.

//
//   Rev 1.35    31-07-2005 21:35:28  mcm
// Initial VerifyImageFile implementation.

//
//   Rev 1.34    11-06-2005 09:56:46  mcm    Version: IMG 2.9
// Added support for reading the TIFF Orientation TAG into TmcmImageInfo.

//
//   Rev 1.33    15-05-2005 09:57:22  mcm
// Corrected reading 16/32 bit PNG files with alpha channel but without a
// defined background color.

//
//   Rev 1.32    09-04-2005 17:29:24  mcm    Version: IMG 2.9
// Corrected an error reading Portable bitmap header.

//
//   Rev 1.31    29-03-2005 20:19:22  mcm    Version: IMG 2.9
// Corrected reading grey scale JPEG (Horiz and VertFreq set to 1).

//
//   Rev 1.30    28-03-2005 20:27:26  mcm    Version: IMG 2.9
// Corrected error occuring in PNG.
// Corrected reading palette in PCX header.

//
//   Rev 1.29    20-03-2005 19:32:28  mcm
// Corrected an error reading 4 bit non planar PCX images.

//
//   Rev 1.28    25-02-2005 19:52:38  mcm
// PNG check of buffer read size being negative.

//
//   Rev 1.27    14-01-2005 19:52:04  mcm    Version: IMG 2.7
// Correction to reading GIF image files where header in file is incorrect.

//
//   Rev 1.26    07-01-2005 13:45:32  mcm
// Added correction ensuring that GetFormatFromColor doesn't return excluded
// formats.

//
//   Rev 1.25    03-01-2005 18:33:10  mcm    Version: IMG 2.7
// Added support for Delphi 2005.
// LZW is now always supported.

//
//   Rev 1.24    28-10-2004 19:13:42  mcm    Version: IMG 2.6
// Added modifications to support C++Builder (BCB).
// Added property methods replacing direct access to member variables.
// Included ReadImage method on TmcmImageFileMgr replacing LoadImage when using
// BCB. LoadImage is somehow not translated correctly in BCB.
// Renamed CP_NONE to CP_NOCOMP (BCB issue).
// Changed TmcmNEFImage to use TmcmTAG control.
// Renamed PNG filter constants to avoid conflicts with TmcmImageFilter.

//
//   Rev 1.23    26-08-2004 23:51:36  mcm    Version: IMG 2.6
// Modified interface to TIFF Tags introducing a TAG lsit and TAG class. Solves
// property problem in C++Builder.

//
//   Rev 1.22    19-07-2004 20:26:22  mcm    Version: IMG 2.5
// Fixed a bug reading multi-tiff files via streams introduced in version 2.4.

//
//   Rev 1.21    25-06-2004 20:47:26  mcm    Version: IMG 2.5
// Modified TmcmBmpImage.IsFormatValid to support TBitmapV4Header's.

//
//   Rev 1.20    01-06-2004 00:02:44  mcm    Version: IMG 2.5
// Added GetDefaultExtension on TmcmImageFileMgr.
// Corrected a potential error when reading TIFF files.

//
//   Rev 1.19    14-03-2004 09:09:04  mcm    Version: IMG 2.4
// Included reading Exif and GPS information in TIFF and JPEG.
// Modified TIFF implementation by seperating TAG into "their" own class.
// Initial implementation of NEF format.

//
//   Rev 1.18    30-01-2004 20:27:54  mcm    Version: IMG 2.3
// Corrected an error occuring when saving 1 & 4 bit images TIFF using LZW
// compression.

//
//   Rev 1.17    21-01-2004 12:26:44  mcm    Version: IMG 2.3
// Implemented MMX code to perform the YCbCr to RGB conversion when reading JPEG
// images.

//
//   Rev 1.16    20-12-2003 19:54:32  mcm
// Ensured that FImageInfo isn't accessed in any class before checking that
// FImageInfo is assigned.
// Added a correction allowing multiple paged TIFF's to be read from a Stream.

//
//   Rev 1.15    24-11-2003 20:24:34  mcm
// Added two helper functions: GetFormatFromColor and GetCompressionFromColor.
// Both help determine the file formats and compressions that support the color
// resolution/bit depth.

//
//   Rev 1.14    13-11-2003 23:17:38  mcm    Version: IMG 2.0
// Corrected case sensitive matching in file association.

//
//   Rev 1.13    03-10-2003 14:57:28  mcm
// Fixed memory leak in TmcmTIFFImage.

//
//   Rev 1.12    29-09-2003 18:56:04  mcm    Version: IMG 1.6
// Fixed not loading local palette in GIF.
// Added option to disable Range check.

//
//   Rev 1.11    29-08-2003 18:17:44  mcm    Version: IMG 1.4
// Implemented GIF read and writer.

//
//   Rev 1.10    25-07-2003 10:41:36  mcm
// Added LZW incl. Difference Predictor for 8, 24 and 32 bit TIFF images.
// Fixed reading/writing 32 bit RGBA TIFF images.

//
//   Rev 1.9    06-07-2003 10:44:56  mcm    Version: IMG 1.3.4
// Modified to work in BCB.
// Initial steps to include LZW

//
//   Rev 1.8    01-04-2003 19:49:24  mcm    Version: IMG 1.3.3

//
//   Rev 1.7    27-03-2003 16:21:38  mcm    Version: IMG 1.3.3
// Added FileFormat parameter to LoadNext.

//
//   Rev 1.6    25-03-2003 22:31:56  mcm    Version: IMG 1.3.3
// Added support for reading/writing Multi image/page TIFF files.

//
//   Rev 1.5    11-03-2003 00:12:50  mcm    Version: IMG 1.3.2
// Added property TmcmTIFFImage.SafeBiLevel to ensure that less compilant TIFF
// reader can read bi-level images from ITFD. SafeBiLevel defaults to True which
// causes bi-level images to be stored with White as "Zero".

//
//   Rev 1.4    05-02-03 16:22:04  mcm
// Added ability to save 8 bit palette and 32 bit RGB images in TmcmJPEGImage
// (auto converted to 24 bit RGB).

//
//   Rev 1.3    29-01-2003 15:42:58  mcm
// Replaced JPEG Engine.
// Applied TmcmBufferStream to minimise disk access.

//
//   Rev 1.2    27-09-2002 13:04:16  mcm    Version: IMG 1.2
// Added SGI image file import and export.
// Fixed omitted export of palette at certain photometric values in TIFF.

//
//   Rev 1.1    07-08-2002 14:41:26  mcm    Version: IMG 1.1
// Added CCITT Group 3 and 4 support in TIFF.
// Added TmcmGIFImage and LZW compression to TIFF. Both, image read only.
// Added compression level to PNG for saving images.
// Fixed 32 bit support in TIFF.
// Added Detect file format in read filter to enable showing all supported file
// formats in Open dialogue.
// Fixed PNG CRC check error when using Delphi 3.

//
//   Rev 1.0    27-05-2002 16:22:02  mcm

unit mcmImageFile;

//------------------------------------------------------------------------------
// General info.
//
// Compile with Conditional define = IMGDEBUG to list file read/write errors in 
// the Event Log.
//
// An instance of TmcmImageFileMgr is automatically created and access provided
// via the global variable ImageFileManager.
//
// **** NOTES ****
// ICON - save option is not implemented.
// PNG 16 bit images are not supported.
// PNG, UpdateAdler uses 28 % for the time spend to read the image.
// Implement DICOM
//      -    GIF
//      -    TIFF, LZW compression.
//------------------------------------------------------------------------------

interface

{$Include 'mcmDefines.pas'}
{$B-} // Don't perform complete boolean evaluations.
{ $DEFINE mcmJBIG}
{ $DEFINE mcmJPEG2K}
{$DEFINE mcmJPEGEnginee} // Disabling this define, causes Borland's native
                         // TJPEGImage component to be used instead.

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
       {$IFDEF GE_DXE4}
         System.AnsiStrings,
       {$ENDIF}
     {$ENDIF}
     mcmFileStream, mcmImage, mcmImageCompress, mcmImageTypeDef,
     DefExif, DefTIFF, DefTarga, DefIcon, {$IFDEF mcmJBIG} DefJBIG, {$ENDIF} DefJPEG,
     DefPCX, DefPNG, DefSGI, DefGIF,
     mcmLZW, mcmDCT;

type

  //----------------------------------------------------------------------------
  TOnFileProgress = procedure(    Sender  : TObject;
                                  Percent : word;
                              var Break   : boolean) of object;
  //----------------------------------------------------------------------------
  // Photometric Interpretation (TIFF).
  // The first 9 entries are included according to TIFF's Photometric definition.
  TmcmPhotometric  = (PM_INVGREY,   // Bilevel and grayscale image, white = 0 and black = 255.
                      PM_GREY,      // Bilevel and grayscale image, white = 255 and black = 0.
                      PM_RGB,       // RGB color image
                      PM_PALETTE,   // Palette color image.
                      PM_MASK,      // Transparency mask
                      PM_CMYK,      // Color separation
                      PM_YCBCR,     // CCIR 601
                      PM_DONTUSE,   // Inserted to align PM_CIELAB to the value 8.
                      PM_CIELAB,    // CIE L*a*b* (1976)
                      // None TIFF Definitions.
                      PM_RGBA,      // RGB color image
                      PM_RGB16,     // RGB 16 (15) bit color image.
                      // PM_CFA = $8023, // NEF file format.
                      PM_CUSTOM);
const
  PM_CFA = $8023; // NEF file format.

//------------------------------------------------------------------------------
// TmcmImageFile
//
// Base class for image file classes.
//------------------------------------------------------------------------------
type

  TmcmImageFile = class(TPersistent)
  private
    FCompObj      : TmcmImageCompress;// Compression class.
    FCounterLock  : TRTLCriticalSection;
    FLock         : TRTLCriticalSection;
    FLockCount    : integer;
    FMMX          : boolean;
  protected
    FProgress     : integer;          // Read / write progress.
    FError        : TmcmErrorCode;    // Error code
    FWinError     : longword;         // Last occured Windows error.
    FStream       : TmcmBufferStream;
    FMemStream    : TMemoryStream;    //
    FCanAppend    : boolean;          // If true one or more images can be appended.
    FNumImages    : integer;          // Number of images in file.
                                      //  1 -> There is only one image.
                                      // -1 -> There are more pages but the
                                      //       count is not determined yet.
    FIndex        : integer;          // Index to image up for reading.
                                      // Range [0..n] where n is a positive value.
    FDIBSection   : TDIBSection;      // DIB section.
    FDibHandle    : HBITMAP;          // Handle to current DIB image.
    FDibBits      : Pointer;          // Pointer to DIB image data.
    FDibInfo      : PBitmapInfo;      // Pointer to Bitmap info header.
    FLongWidth    : longint;          // Long line width (bitmap)
    FBitCount     : integer;          // Bits per pixel.
    FNoColors     : longword;         // Number of colors.
    FImageInfo    : TmcmImageInfo;    // Pointer to image info class.
    FCompress     : TmcmCompress;     // Compression format.
    FQuality      : word;             // Compression quality.
    FInterlaced   : boolean;          // Image is interlaced - Progressive
    FStreamStart  : {$IFNDEF DCB3_5}
                    int64;            // Start position of image in stream.
                    {$ELSE}
                    longint;
                    {$ENDIF}
    FOnProgress   : TOnFileProgress;

    function    AddOffset(AOffset : Cardinal) : Cardinal;
    function    CreateDib : boolean;
    function    GetDibData : boolean;
    function    GetDibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    GetImageInfo : TmcmImageInfo;
    function    GetNumColors(pDibInfo : PBitmapInfo) : integer;
    function    GetNoImages : integer; virtual;
    function    GetPalFromDibHandle(var ColorTable : array of TRGBQuad) : integer;
    function    GetPhotometric : TmcmPhotometric;
    procedure   InvertImage;
    procedure   Lock;
    function    ReadStreamData(Stream : TStream; pData : pointer; DataSize : longint) : boolean;
    procedure   SetBmiPalToDibHandle;
    procedure   SetDibHandle(Value : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
    procedure   SetImageIndex(Value : integer);
    procedure   SetImageInfo(Value : TmcmImageInfo);
    procedure   SetStreamPos(Pos : {$IFNDEF DCB3_5}
                                   int64
                                   {$ELSE}
                                   longint
                                   {$ENDIF});
    procedure   SwapByteRB;
    function    WriteStreamData(Stream : TStream; pData : pointer; DataSize : longint) : boolean;
    procedure   Unlock;
  public
    class function GetColorFormats : TmcmImageFormats; virtual;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; virtual;
    class function GetCompressionName(Compression : TmcmCompress) : string; virtual;
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   AppendImage(Filename : string; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF}); virtual;
    procedure   CloseFile(var Stream : TStream); virtual;
    procedure   CreateGreyPalette(Photometric : TmcmPhotometric);
    function    CreatePalette : HPalette;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; virtual;
    procedure   LoadFromStream(Stream : TStream); virtual; abstract;
    function    LoadImage(Filename : string) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF}; virtual;
    // function    LoadNext(Filename : string; Index : integer) : HBITMAP; virtual;
    function    OpenFile(Filename : string; Mode : word; var Stream : TStream) : boolean; virtual;
    procedure   SaveImage(Filename : string; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF}); virtual;
    procedure   SaveToStream(Stream : TStream); virtual; abstract;

    property    Compression : TmcmCompress
      read      FCompress
      write     FCompress;
    property    DibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF}
      read      GetDibHandle
      write     SetDibHandle;
    property    Error : TmcmErrorCode
      read      FError;
    property    ImageIndex : integer
      read      FIndex
      write     SetImageIndex;
    property    ImageInfo : TmcmImageInfo
      read      GetImageInfo
      write     SetImageInfo;
    property    Interlaced : boolean
      read      FInterlaced
      write     FInterlaced;
    property    NoImages : integer
      read      GetNoImages;
    property    Quality : word
      read      FQuality
      write     FQuality;
    property    WinError : longword
      read      FWinError;

    property    OnProgress : TOnFileProgress
      read      FOnProgress
      write     FOnProgress;
  published
  end;

  TmcmImageFileRef = class of TmcmImageFile;


//------------------------------------------------------------------------------
// TmcmBmpImage
//
// Windows Bitmap file format (ver. 3.0) - (Uses TBITMAPINFOHEADER and TRGBQUAD)
// Windows Device Independent Bitmap - do.
// OS/2 Bitmap - (Uses TBITMAPCOREHEADER and TRGBTRIPLET)
//
// BI_BITFIELDS data package is not supported.
//------------------------------------------------------------------------------
  TmcmBmpImage = class(TmcmImageFile)
  private
    FBmpFileHeader : TBitmapFileHeader;
    FIsOS2         : boolean;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode; virtual;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode; virtual;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmDICOMImage
//------------------------------------------------------------------------------
{$IFDEF mcmDICOM}
  TmcmDICOMImage = class(TmcmImageFile)
  private
  protected
  public
    class function GetColorFormats : TmcmImageFormats; virtual;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; virtual;
    class function GetCompressionName(Compression : TmcmCompress) : string; virtual;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmGIFImage
//------------------------------------------------------------------------------
  TmcmGIFImage = class(TmcmImageFile)
  private
    FGIFVersion     : word;
    FGIFHeader      : TGIFHeader;
    FGIFImageHeader : TGIFImageHeader;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadPalette(Stream : TStream; BitFields : byte) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmMCFImage
// Test image file format - all compression methods are allowed.
//------------------------------------------------------------------------------

{$IFDEF MCMFILEFORMAT}
  TmcmMCFImage = class(TmcmImageFile)
  private
    FDistanceShift     : word;
    FLengthMask        : word;
    FSlidingWindowSize : word;
    FMaxMatchLength    : word;
    FLookAheadSize     : word;
  protected
    procedure   OnCompressData(Sender : TObject; Buffer : pointer; BufferSize : cardinal);
    procedure   OnDecompressData(Sender : TObject; Buffer : pointer; var BufferSize : cardinal);
    function    ReadHeader(Stream : TStream) : TmcmErrorCode; virtual;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode; virtual;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; virtual;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; virtual;
    class function GetCompressionName(Compression : TmcmCompress) : string; virtual;
    constructor Create; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmSGIImage
//------------------------------------------------------------------------------
  TmcmSGIImage = class(TmcmImageFile)
  private
    FSGIHeader : TSGIHeader;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmICONImage
//------------------------------------------------------------------------------
  TmcmICONImage = class(TmcmImageFile)
  private
    FIconDir       : TIconDir;
    FIconDirectory : TList;
    // FIconDirEntry  : TIconDirEntry;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmPxMImage
// Portable Bit Map, supports 1 bit BW.
// Portable Grey Map, supports 8 bit, 256 levels of grey.
// Portable Pixel Map, supports  24 bit true color.
//------------------------------------------------------------------------------
  TmcmPxMImage = class(TmcmImageFile)
  private
    FFileFormat : TmcmFileFormat;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override; // (FileFormat : TmcmFileFormat); {$IFNDEF DCB3} reintroduce; {$ENDIF}
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

  TmcmPBMImage = class(TmcmPxMImage)
  private
  protected
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    constructor Create; override;
  published
  end;

  TmcmPGMImage = class(TmcmPxMImage)
  private
  protected
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    constructor Create; override;
  published
  end;

  TmcmPPMImage = class(TmcmPxMImage)
  private
  protected
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    constructor Create; override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmPNGImage
//------------------------------------------------------------------------------
  TPNGDecompressByte = function : byte of object;
  TPNGHuffmanEncode  = procedure(Index : cardinal;
                                 Code  : cardinal;
                                 Extra : cardinal;
                                 Value : Cardinal) of object;

  TmcmPNGImage = class(TmcmImageFile)
  private
    FCRCTable        : array[0..255] of longword; // cardinal;
    FPNGHeader       : TPNGHeader;
    FChunk           : TPNGChunk;

    FBufBit          : integer;
    FBufByte         : byte;
    FBufIndex        : longword; // cardinal;

    FWindowSize      : longword; // cardinal;
    FCompressMethod  : byte;
    FPresetDir       : byte;
    FCompressLevel   : byte;

    FSearchLimit     : word;
    FStreamAdler     : longword; // cardinal;

    FWinPos          : longword; // cardinal; // Slinding Window position.
    FCopyPos         : longword; // cardinal; // Copy data at position
    FCopyCount       : longword; // cardinal; // Copy data count
    FLZWin           : PVectorB;
    FHashTable       : TList;
    FHashValues      : TList;
    FLookAheadBuffer : PVectorW;
    FLookAheadPos    : longword; // cardinal;

    FCompressType    : TPNGCompressType;
    FFinalDataSet    : boolean;
    FLiteralMode     : boolean;
    FLiteralCount    : longword; // cardinal;

    FLiteralCode     : TmcmHuffmanPNG;
    FDistanceCode    : TmcmHuffmanPNG;
    FLengthCode      : TmcmHuffmanPNG;

    FInterlacePass   : word;     // Current interlace pass-through.

    FFilterOffset    : longword; // cardinal; // Distance between corresponding channel items.
    FFilterCurrent   : TPNGFilters;
    FFilterMask      : longword; // cardinal;
    FPreFilterBuffer : array[0..1] of PVectorB;
    FFilterBuffer    : array[0..4] of PVectorB;

    FBytesPerRow     : word;     // Bytes per row in the image.
    FFltRow          : boolean;  // Filter / RowBuffer index.
    FRowBuffer       : array[boolean] of PVectorB;

    FBkRed           : word;
    FBkGreen         : word;
    FBkBlue          : word;
    FBkGrey          : word;

    FBlockBuffer     : PVectorW;
    FBlockBufferPos  : longword;
    FBlockBufferSize : longword;

    FRow             : longint;
    FCol             : longint;
    FEndOfImage      : boolean;
  protected
    function    CalcCRC(Buffer : PVectorB; Count : longint) : longword; // cardinal;
    function    CheckAdler : TmcmErrorCode;
    function    CreateCRCTable : TmcmErrorCode;
    function    DecodeCompressed : byte;
    function    DecodeByte : byte;
    function    DecodeLiteral : byte;
    procedure   DeFilterRow(Filter : TPNGFilters);
    procedure   ExtractText(Chunk : TPNGChunk);
    procedure   FillLookAheadBuffer(Count : longword); // cardinal);
    procedure   FilterRow(Row : longint);
    procedure   FindLengthCodes(Huffman : TmcmHuffmanPNG;
                                Count   : longint;
                                Encoder : TPNGHuffmanEncode);
    procedure   FlushBits;
    function    GetHashValue(Index : cardinal) : cardinal;
    function    GetIDataByte : byte;
    procedure   GetLengthCounts(Index : cardinal;
                                 Code  : cardinal;
                                 Extra : cardinal;
                                 Value : Cardinal);
    function    GetNBits(Count : word) : cardinal;
    procedure   ImageRowToNonInterlaced(Row : longint);
    procedure   InitializeHashTable;
    procedure   InterlacedRowToImage(Row, RowWidth : cardinal; Pass : word);
    procedure   LongestMatch(var BestLength : cardinal;
                             var BestOffset : cardinal);
    procedure   MoveHashEntry(Entry : cardinal; HashValue : cardinal);
    function    NewCompressBlock : TmcmErrorCode;
    procedure   NonInterlacedRowToImage(Row : cardinal);

    function    ReadChunk(Stream : TStream; var Chunk : TPNGChunk) : TmcmErrorCode;
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadImageChunk(Stream : TStream; var Chunk : TPNGChunk) : TmcmErrorCode;
    function    ReadImageData : TmcmErrorCode;
    function    ReadInterlaced(Pass : word) : TmcmErrorCode;
    function    ReadLength(Decoder    : TmcmHuffmanPNG;
                           LengthCode : TmcmHuffmanPNG;
                           Count      : cardinal) : TmcmErrorCode;
    function    ReadNonInterlaced : TmcmErrorCode;
    function    ReadPalette(Chunk : TPNGChunk) : TmcmErrorCode;
    function    PaethPredictor(Left      : integer;
                               Above     : integer;
                               AboveLeft : integer) : integer;

    function    ProcessImageData : boolean;

    procedure   SetIDataByte(Value : byte);
    procedure   SetNBits(Value : longword; Count : longword);

    function    WriteBlockBuffer(Stream : TStream) : TmcmErrorCode;
    function    WriteChunk(Stream : TStream) : TmcmErrorCode;
    procedure   WriteDeflateHeader(IsLastBlock : boolean);
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WriteImageData(Stream : TStream) : TmcmErrorCode;
    procedure   WriteLengthCounts(Index : cardinal;
                                  Code  : cardinal;
                                  Extra : cardinal;
                                  Value : Cardinal);
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
    function    WritePixelSize(Stream : TStream) : TmcmErrorCode;
    function    WriteText(Stream : TStream) : TmcmErrorCode;
    function    WriteTime(Stream : TStream) : TmcmErrorCode;
    procedure   UpdateAdler(const Value : byte);
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmPCXImage
//------------------------------------------------------------------------------
  TmcmPCXImage = class(TmcmImageFile)
  private
    FPCXHeader : TPCXHeader;
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmTIFFTAG - A TIFF TAG
//------------------------------------------------------------------------------

  TmcmTIFFTAG = class(TPersistent)
  private
  protected
    FTag       : word;     // Tag for the field.
    FFieldType : word;     // Field type.
    FLength    : longint;  // Length of the field.
    FValOffset : longword; // Value offset. Contains the
                           // value, if this is less than
                           // or equal to four bytes.
    FValPtr    : word;     // Index pointer to ValRat og ValStr arrays.
    FFilePos   : longint;  // Tags position in file. Used during write only.

    FRational  : TRational;
    FString    : string;

    function    GetDenom : integer;
    function    GetNum : integer;
    procedure   SetDenom(Value : integer);
    procedure   SetNum(Value : integer);
  public
    constructor Create;
    procedure   Clear;
    destructor  Destroy; override;
    property    Tag : word
      read      FTag
      write     FTag;
    property    FieldType : word
      read      FFieldType
      write     FFieldType;
    property    Length : longint
      read      FLength
      write     FLength;
    property    ValOffset : longword
      read      FValOffset
      write     FValOffset;
    property    ValPtr : word
      read      FValPtr
      write     FValPtr;
    property    FilePos : longint
      read      FFilePos
      write     FFilePos;
    property    Num : longint
      read      GetNum
      write     SetNum;
    property    Denom : longint
      read      GetDenom
      write     SetDenom;
    property    Text : string
      read      FString
      write     FString;
  published
  end;

//------------------------------------------------------------------------------
// TmcmTAGList
//------------------------------------------------------------------------------

  TmcmTAGList = class(TList)
  private
  protected
    function Get(Index : integer) : TmcmTIFFTAG;
    procedure Put(Index : integer; Item : TmcmTIFFTAG);
  public
    property Items[Index : Integer] : TmcmTIFFTAG
      read   Get
      write  Put; default;
  end;

//------------------------------------------------------------------------------
// TmcmTAGMgr
//
// Used by TIFF & JPEG to create, read and write TAG's to stream.
//------------------------------------------------------------------------------

  TOnWritePalette = function(Stream : TStream) : integer of object;
  TOnWriteStrips  = procedure(Stream : TStream) of object;

  TmcmTAGMgr = class(TPersistent)
  private
    FError           : TmcmErrorCode;    // Error code
    FStream          : TmcmBufferStream;
    FImageInfo       : TmcmImageInfo;    // Pointer to image info class.

    // TAG's
    FEC              : word;        // Entry Count, number of tags.
    FTagList         : TmcmTAGList; // TIFF Directorie Entry, list of tags.
    FNextIFD         : longword;    // Offset of next IFD, if FNextIFD <> 0 an
                                    // extra image follows.
    FValStr          : TValueS;     // Value string array.
    FNrOfStr         : integer;     // Number of value strings.
    FValRat          : TValueR;     // Value rational array.
    FNrOfRat         : integer;     // Number of value rationals.
    FIsTIFFTag       : boolean;
  protected
    FOnWritePalette      : TOnWritePalette;
    FOnWriteStripOffsets : TOnWriteStrips;
    FOnWriteStripCounts  : TOnWriteStrips;
    FOnWriteJPEGTables   : TOnWriteStrips;
    procedure   ConvByteASCIIZ(Index : byte);
    procedure   ConvByteRational(Index : byte);
    procedure   ConvByteUndefined(Index : byte);
    function    Convert2DateTime(DateStr : string) : TDateTime;
    function    CreateTag : TmcmTIFFTAG;
    procedure   ErrorTag;
    function    GetImageInfo : TmcmImageInfo;
    function    GetItem(Index : word) : TmcmTIFFTAG;
    function    GetStream : TmcmBufferStream;
    function    GetTagIndex(TagNr : word) : word;
    function    GetTagStr(Index : word) : AnsiString;
    function    GetTagVal(Index, Sub : word) : TRational;
    procedure   SetImageInfo(Value : TmcmImageInfo);
    procedure   SetStream(Value : TmcmBufferStream);
    procedure   SetTagInfo(TagNo : word; TagType : word; TagLength : longint; TagVal : longint; TagRat : array of longint; TagStr : string);
  public
    constructor Create(AStream : TmcmBufferStream; AImageInfo : TmcmImageInfo; IsTIFFTag : boolean);
    procedure   Clear;
    destructor  Destroy; override;
    procedure   ImageInfoToTags(PageIndex : integer);
    procedure   ReadTags(Offset : longword);
    procedure   WriteCalcOffsets(var Offset : longword);
    procedure   WriteTags;
    procedure   WriteTagsData;
    procedure   TagsToImageInfo;
    property    EC : word
      read      FEC;
    property    ImageInfo : TmcmImageInfo
      read      GetImageInfo
      write     SetImageInfo;
    property    Index[TAG : word] : word
      read      GetTagIndex;
    property    IsTIFFTag : boolean
      read      FIsTIFFTag
      write     FIsTIFFTag;
    property    NextIFD : longword
      read      FNextIFD;
    property    Stream : TmcmBufferStream
      read      GetStream
      write     SetStream;
    property    Item[Index : word] : TmcmTIFFTAG
      read      GetItem;
    property    ItemStr[Index : word] : AnsiString
      read      GetTagStr;
    property    ItemVal[Index, Sub : word] : TRational
      read      GetTagVal;
    property    List : TmcmTAGList
      read      FTagList;
  published
  end;

//------------------------------------------------------------------------------
// TmcmExifImage
//
// Used by TIFF & JPEG read and write Exif attribute information.
//------------------------------------------------------------------------------
  TmcmExifImage = class(TmcmTAGMgr)
  private
  protected
    function    ConvertExifStr2Int(Data : string) : integer;
    procedure   ReadExifAttribute(IFDOffset, Offset : longword);
    procedure   ReadExifGPS(IFDOffset, Offset : longword);
    procedure   ReadExifInteroperability(IFDOffset, Offset : longword);
    procedure   SetExifAttribute(RGBMode : boolean);
    procedure   SetExifGPS;
    procedure   WriteExifInteroperability(IFDOffset, Offset : longword);
  public
  published
  end;

  TmcmJPEGImage = class; // Forward declaration - used by TmcmTIFFImage.
//------------------------------------------------------------------------------
// TmcmTIFFImage
//
// TIFF, Taged Image File Format (ver 5.0 / 6.0)
//------------------------------------------------------------------------------
  TmcmTIFFImage = class(TmcmImageFile)
  private
    FIsRGB           : boolean;
    FSafeBiLevel     : boolean;   // TIFF bi-level only.
    FHeader          : TIFH;      // TIFF Image File Header.

    FTags            : TmcmExifImage;
    FExifTags        : TmcmExifImage;
    FExifInterTags   : TmcmExifImage;
    FExifGPSTags     : TmcmExifImage;

    // Strip's, offset positions
    FNrOfStrip       : word;      // Number of image strips.
    FStripDskOfs     : PVectorLW; // Strip offset in file.
    FStripSizes      : PVectorLW; // Strip size in file.
    FRowsPerStrip    : PVectorLW; // Rows (lines) per strip.
    FRowWidth        : longword;  // Stored line width in bytes.
    FStripNr         : word;      // Current image strip.
    FStripSize       : longint;   // Image strip size.
    FImageOfs        : longint;   // (Image) offset in file.

    // Image configuration
    FPlanarConfig    : word;      // Planar or chunky storage.
    FBitPerSample    : word;      // Bits per Sample.
    FSamplePerPixel  : word;      // Samples per Pixel.
    FPhotometric     : TmcmPhotometric; // Photometric interpretation
    FInvertLogPal    : boolean;
    FResolutionUnit  : word;      // Resolution Unit
    FFillOrder       : word;      // Fill order of bits in a byte.
                                  // 1 - First bit is stored in MSB
                                  // 2 - First bit is stored in LSB.
    // Compression section
    FDiffPredictor   : boolean;
    FT4Option        : longint;   // Fax group 3 (CCITT T.4) option.
    FT6Option        : longint;   // Fax group 4 (CCITT T.6) option.

    // ReferenceBlackWhite
    FCodingRange     : double;
    FRefBlack        : array[0..2] of double;
    FRefWhite        : array[0..2] of double;

    // References to JPEG (TmcmJPEGImage)
    FYCbCrCoef       : array[0..5] of longint;
    FJPEGImage       : TmcmJPEGImage;
    FYCbCrMode       : TmcmJPEGYCbCr;
  protected
    procedure   Clear; virtual;
    procedure   ClearStripTable;
    function    GetNoImages : integer; override;
    function    GotoSubImage(var Index : integer) : TmcmErrorCode;
    procedure   GotoStripOfs(Stream : TStream; StripNr : word; var StripCount : longint; NrOfStrip  : word);
    procedure   InitRowsPerStrip(Tags : TmcmTAGMgr; Height : longword);
    procedure   LoadChunkyData(Stream : TStream; var ImageOfs : longint; ImageSize : longint; BytesToRead : longint; IsRGB : boolean);
    procedure   LoadPlanarData(Stream : TStream; ImageOfs : longint; ImageSize : longint; BytesToRead : longint; PixelWidth : longint; IsRGB : boolean);
    procedure   ReadHeader(Stream : TStream); virtual;
    function    ReadPalette(Stream : TStream) : integer;
    procedure   ReadPredictor;
    procedure   ReadStripTable(Tags : TmcmTAGMgr);
    function    RowWidth : longword;
    procedure   SaveChunkyData(Stream : TStream; BitsByteSize : longint; cWidth : longint; PixelWidth : longint; IsRGB : boolean);
    procedure   SetStripOffsets(Offset : longword);
    procedure   WriteStripOffsets(Stream : TStream);
    procedure   WriteStripCounts(Stream : TStream);
    procedure   WriteHeader(Stream : TStream);
    procedure   WriteJPEGTables(Stream : TStream);
    function    WritePalette(Stream : TStream) : integer;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    procedure   CloseFile(var Stream : TStream); override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
    property    SafeBiLevel : boolean
      read      FSafeBiLevel
      write     FSafeBiLevel default True;
    {$IFDEF mcmJPEGEnginee}
    property    YCbCrMode : TmcmJPEGYCbCr
      read      FYCbCrMode
      write     FYCbCrMode default JYCC_411;
    {$ENDIF}
  published
  end;

{ $DEFINE MCM_USE_NEF}
{$IFDEF MCM_USE_NEF}
//------------------------------------------------------------------------------
// TmcmNEFImage - Requires more info from Nikon to complete format.
//
// NEF, Nikon Raw Image Format (D100 & D1X/D1H)
//------------------------------------------------------------------------------
  TmcmNEFImage = class(TmcmTIFFImage)
  private
    FNEFTags         : TmcmTAGMgr;
    FPhotometric     : word; // Photometric interpretation
    FEvenLine        : PVectorW;
    FOddLine         : PVectorW;
  protected
    procedure   Clear; override;
  public
    class function GetColorFormats : TmcmImageFormats; virtual;
    constructor Create; override;
    procedure   LoadCFAData(var ImageOfs : longint; ImageSize : longint; BytesToRead : longint);
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   ReadHeader(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;
{$ENDIF}


//------------------------------------------------------------------------------
// TmcmJPEGImage
//------------------------------------------------------------------------------

  TJPEGACFunction     = procedure(Value, Uncoded, Bits : integer) of object;
  TJPEGDCFunction     = procedure(Value, Bits : integer) of object;
  TJPEGEncodeFunction = procedure(McuRow, McuCol : integer) of object;

  TJPEGEncodeMethod   = (JPG_SEQ,
                         JPG_DC_FIRST,
                         JPG_DC_REFINE,
                         JPG_AC_FIRST,
                         JPG_AC_REFINE);
{$IFDEF mcmJPEGEnginee}
  TmcmJPEGComponent = class(TPersistent)
  private
    FID            : byte;     // Identifier.
    FDCT           : TmcmDCT;
    FCoef          : PJPEGCoefficient;
    FDCValue       : integer;  // Last encoded DC value.

    // NOTE: Both the AC & DC Table is owned by the parent object, therefore do
    // not destroy these in this class.
    FAC            : TmcmHuffmanJPEG; // AC Huffman table.
    FDC            : TmcmHuffmanJPEG; // DC Huffman table.

    FEobRun        : word;     // End Of Band Run.
    FEobRow        : word;
    FEobCol        : word;
    FEobPos        : word;
    FHorizFreq     : integer;  // Components horizontal frequency.
    FVertFreq      : integer;  // Components vertical frequency.
    FMaxHorizFreq  : longint;  // Maximum horizontal sampling frequency.
    FMaxVertFreq   : longint;  // Maximum vertical sampling frequency.
    FHorizSample   : integer;  //
    FVertSample    : integer;  //
    FDataRows      : longint;  // Number of data rows.
    FDataCols      : longint;  // Number of data columns.
    FMcuRows       : longint;  // Number of MCU rows.
    FMcuCols       : longint;  // Number of MCU columns.
    FNilRows       : longint;  // Number of non-interleaved rows.
    FNilCols       : longint;  // Number of non-interleaved columns.

    FMcuData       : PVectorB; // Decompressed data from an MCU.
    FAllocSize     : cardinal; //
    FDataLineSize  : longint;  // .
    FDataRowSize   : longint;  // .
    FDataBlockSize : longint;  // Size of MCU's data.

    FMcuPixHoriz   : longint;  // Number of horizontal pixels per MCU.
    FMcuPixVert    : longint;  // Number of vertical pixels per MCU.

    FImageHeight   : longint;  // Image height.
    FImageWidth    : longint;  // Image width.

    FApproxHigh    : byte;
    FApproxLow     : byte;

    FSpectralEnd   : byte;
    FSpectralStart : byte;

    FACFunction    : TJPEGACFunction;
    FDCFunction    : TJPEGDCFunction;
    FEncoder       : TJPEGEncodeFunction;

    FStream        : TmcmBufferStream;
  protected
    function    Extend(Bits, Count : integer) : integer;
    function    GetACDCTable(Index : byte) : TmcmHuffmanJPEG;
  public
    constructor Create(ID : byte; Stream : TmcmBufferStream); virtual;
    destructor  Destroy; override;

    procedure   AllocateBuffer(FullImage, Progressive : boolean);
    procedure   DecodeACFirstScan(McuRow, McuCol  : integer;
                                  SpecStart, SpecEnd, ApproxLow : byte);
    procedure   DecodeACRefineScan(McuRow, McuCol : integer;
                                   SpecStart, SpecEnd, ApproxLow : byte);
    procedure   DecodeDCFirstScan(McuRow, McuCol, ApproxLow : integer);
    procedure   DecodeDCRefineScan(McuRow, McuCol, ApproxLow : integer);
    procedure   DecodeSequential(McuRow, McuCol : integer);
    procedure   EncodeACFirstScan(McuRow, McuCol  : integer);
    procedure   EncodeACRefineScan(McuRow, McuCol : integer);
    procedure   EncodeDCFirstScan(McuRow, McuCol : integer);
    procedure   EncodeDCRefineScan(McuRow, McuCol : integer);
    procedure   EncodeEobRun;
    procedure   EncodeRefineEobRun;
    procedure   EncodeSequential(McuRow, McuCol : integer);
    procedure   ProgressiveDCT;
    procedure   ProgressiveIDCT;
    procedure   RefineACCoefficient(var Coef : smallint; ApproxLow : integer);
    procedure   Resample;
    procedure   ResetDCDifference;
    procedure   ResetEobRun;
    procedure   Sample;
    procedure   SetACDCTable(Index : byte; Value : TmcmHuffmanJPEG);
    procedure   SetQuant2DCT(Table : TJFIFDQT);
    procedure   SetQuant2IDCT(Table : TJFIFDQT);
    procedure   SetWriteMode(DoWrite : boolean; EncodeMethod : TJPEGEncodeMethod);
    procedure   StatACData(Value, Uncoded, Bits : integer);
    procedure   StatDCData(Value, Bits : integer);
    procedure   WriteACData(Value, Uncoded, Bits : integer);
    procedure   WriteDCData(Value, Bits : integer);
    property    ApproxHigh : byte
      read      FApproxHigh
      write     FApproxHigh;
    property    ApproxLow : byte
      read      FApproxLow
      write     FApproxLow;
    property    Data : PVectorB
      read      FMcuData;
    property    DataLineSize : longint
      read      FDataLineSize;
    property    DataBlockSize : longint
      read      FDataBlockSize;
    property    DataRowSize : longint
      read      FDataRowSize;
    property    DataCols : longint
      read      FDataCols
      write     FDataCols;
    property    DataRows : longint
      read      FDataRows
      write     FDataRows;
    property    Encoder : TJPEGEncodeFunction
      read      FEncoder;
    property    HorizFreq : integer
      read      FHorizFreq
      write     FHorizFreq;
    property    HorizSample : integer
      read      FHorizSample
      write     FHorizSample;
    property    HuffmanTable[Index : byte] : TmcmHuffmanJPEG
      read      GetACDCTable
      write     SetACDCTable;
    property    ID : byte
      read      FID;
    property    ImageHeight : longint
      read      FImageHeight
      write     FImageHeight;
    property    ImageWidth : longint
      read      FImageWidth
      write     FImageWidth;
    property    MaxHorizFreq : longint
     read       FMaxHorizFreq
     write      FMaxHorizFreq;
    property    MaxVertFreq : longint
     read       FMaxVertFreq
     write      FMaxVertFreq;
    property    McuCols : longint
      read      FMcuCols
      write     FMcuCols;
    property    McuData : PVectorB
      read      FMcuData;
    property    McuPixHoriz : longint
      read      FMcuPixHoriz;
    property    McuPixVert   : longint
      read      FMcuPixVert;
    property    McuRows : longint
      read      FMcuRows
      write     FMcuRows;
    property    NilCols : longint
      read      FNilCols
      write     FNilCols;
    property    NilRows : longint
      read      FNilRows
      write     FNilRows;
    property    SpectralEnd   : byte
      read      FSpectralEnd
      write     FSpectralEnd;
    property    SpectralStart : byte
      read      FSpectralStart
      write     FSpectralStart;
    property    VertFreq : integer
      read      FVertFreq
      write     FVertFreq;
    property    VertSample : integer
      read      FVertSample
      write     FVertSample;
  published
  end;
{$ENDIF}

  TmcmJPEGImage = class(TmcmImageFile)
  private
    {$IFDEF mcmJPEGEnginee}
    FDidAPP0         : boolean;
    FDidSOF          : boolean;
    FPhotometric     : TmcmPhotometric; // Photometric interpretation, by default
                                        // PM_YCBCR, TIFF may set this to PM_RGB.
    FJFIFAPP0Header  : TJFIFAPP0Header;
    FJFIFDHT         : array[0..1,0..3] of TJFIFDHT; // DC = 0, table ID = 0..3
                                                     // AC = 1, table ID = 0..3
    FJFIFDQT         : array[0..JPEGMaxQuantizationTables-1] of TJFIFDQT;
    FJFIFSOF         : TJFIFSOF;
    FJFIFSOS         : TJFIFSOS;
    FRowsPerRestart  : word;
    FRestartMarker   : word;
    FRestartInterval : word;
    FRestartCount    : longword;
    FApproxHigh      : byte;
    FApproxLow       : byte;
    FYCbCrIndex      : integer; // Currently selected component
                                // (Index into FYCbCrComp).
    FYCbCrComp       : array[0..JPEGMaxHuffmanTables-1] of TmcmJPEGComponent;
    FCurrentScan     : array[0..JPEGMaxHuffmanTables-1] of TmcmJPEGComponent;
    FHuffman         : array[0..1,0..JPEGMaxHuffmanTables-1] of TmcmHuffmanJPEG;
    FMaxHorizFreq    : longint; // Maximum horizontal sampling frequency.
    FMaxVertFreq     : longint; // Maximum vertical sampling frequency.
    FMCUHeight       : longint; // MCU Height.
    FMCUWidth        : longint; // MCU Width.
    FMcuRows         : longint; // Number of MCU rows.
    FMcuCols         : longint; // Number of MCU columns.
    FComponentCount  : byte;    // Number of components (Y, Cb and Cr).
                                // Must be 1 or 3 in JFIF format.
    FScanCount       : integer; // Start Of Scan (SOS) count.
                                // Must be 1 or 3 in JFIF format.
    FScanIndex       : integer; // Start Of Scan (SOS) Index.
    FInterleavedScan : boolean; // True  -> Interleaved (YCbCr).
                                // False -> Non-Interleaved (Y only).
    // YToRGB           : array [0..255] of integer;
    CbToB            : array [0..255] of integer;
    CbToG            : array [0..255] of integer;
    CrToR            : array [0..255] of integer;
    CrToG            : array [0..255] of integer;
    ByteClip         : array [-255..511] of integer;

    FYCbCrMode       : TmcmJPEGYCbCr;
    FGlobalMcuRow    : integer; // Used to support JPEG compression in TIFF files.
    {$ENDIF}
  protected
    {$IFDEF mcmJPEGEnginee}
    procedure   CalcMCUDimension;
    procedure   CopyFromRGB(McuRow : integer);
    procedure   CopyFromGrey(McuRow : integer);
    procedure   CopyToRGB(McuRow : integer);
    procedure   CopyToGrey(McuRow : integer);
    function    CreateHuffmanTables(TableID : integer; DC, AC : boolean) : integer;
    procedure   CreateQuantificationTables;
    function    GetScanComponent(Index : byte) : TmcmJPEGComponent;
    procedure   InitYCbCrTables;


    procedure   ReadInitialize;
    function    ReadMarkers(IsTIFF : boolean) : TmcmErrorCode;
    procedure   CombineComponents;

    procedure   WriteInitialise(IsTIFF : boolean);
    procedure   WriteSOI;
    procedure   WriteEOI;
    procedure   WriteSOFn;
    function    WriteTIFFInitialise : integer;
    function    WriteTIFFData(Stream : TmcmBufferStream) : integer;

    // ------------- In the following 5 procedures remove Index parameter and
    //  replace FYCbCrComp[Index] with FCurrentScan[0]. -----------------------
    procedure   ProcessACFirstScan(InWriteMode : boolean);
    procedure   ProcessACRefineScan(InWriteMode : boolean);
    procedure   ReadACFirstScan(Index : integer; ss, se, al : byte);
    procedure   ReadACRefineScan(Index : integer; ss, se, al : byte);
    procedure   ReadDCFirstScan(Index : integer; al : byte);
    procedure   ReadDCRefineScan(Index : integer; al : byte);
    procedure   ReadTiffAndExif;
    procedure   ReadProgressiveData(Index : integer);
    procedure   ReadRestartMarker;
    procedure   ReadSequentialData;
    procedure   ResetDCDifference;
    procedure   WriteACFirstScan(Index : integer);
    procedure   WriteACRefineScan(Index : integer);
    procedure   WriteDCFirstScan(Index : integer);
    procedure   WriteDCRefineScan(Index : integer);
    procedure   WriteHuffmanTables(TableID : integer; DC, AC : boolean);
    procedure   WriteQuantification;
    procedure   WriteInterleaved(InWriteMode : boolean);
    procedure   WriteNonInterleaved(InWriteMode : boolean);
    procedure   WriteSequentialData;
    procedure   WriteProgressiveData(Index : integer);
    procedure   WriteProgressiveFrame;
    procedure   WriteRestartInterval(RestartInterval : word);
    procedure   WriteRestartMarker;
    procedure   WriteTiffAndExif;

    property    McuRows : longint
      read      FMcuRows
      write     FMcuRows;
    {$ENDIF}
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
    {$IFDEF mcmJPEGEnginee}
    property    YCbCrMode : TmcmJPEGYCbCr
      read      FYCbCrMode
      write     FYCbCrMode default JYCC_411;
    {$ENDIF}
  published
  end;

//------------------------------------------------------------------------------
// TmcmJBIGImage
//------------------------------------------------------------------------------

{$IFDEF mcmJBIG}
  TmcmJBIGImage = class(TmcmImageFile)
  private
  protected
    FHeader : TJBIGHeader;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmJPEG2KImage
//------------------------------------------------------------------------------

{$IFDEF mcmJPEG2K}
  TmcmJPEG2KImage = class(TmcmImageFile)
  private
  protected
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmTargaImage
//------------------------------------------------------------------------------
  TmcmTargaImage = class(TmcmImageFile)
  private
    FTargaHeader    : TTargaHeader;
    FTargaExtension : TTargaExtension;
    FTargaFooter    : TTargaFooter;
    FImageID        : array[0..256] of AnsiChar; // Optional field contains identifying
                                             // information about the image.
    FRowWidth       : longword; // Stored line width in bytes.
  protected
    function    ReadHeader(Stream : TStream) : TmcmErrorCode;
    function    ReadFooter(Stream : TStream) : TmcmErrorCode;
    function    ReadExtension(Stream : TStream) : TmcmErrorCode;
    function    ReadPalette(Stream : TStream) : TmcmErrorCode;
    function    WriteHeader(Stream : TStream) : TmcmErrorCode;
    function    WriteFooter(Stream : TStream) : TmcmErrorCode;
    function    WriteExtension(Stream : TStream) : TmcmErrorCode;
    function    WritePalette(Stream : TStream) : TmcmErrorCode;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmImageFileClass
//------------------------------------------------------------------------------

  TmcmImageFileClass = class of TmcmImageFile;

//------------------------------------------------------------------------------
// TmcmImageFileClass
//------------------------------------------------------------------------------

  TmcmFileFormatItem = Class
  protected
    FmcmFileClass : TmcmImageFileClass;
    FFileClassID  : TmcmFileFormat;
    FExtension    : string;
    FDescription  : string;
    FReadEnabled  : boolean;
    FWriteEnabled : boolean;
  public
    function GetFilter : string;
    Property mcmFileClass : TmcmImageFileClass
      read   FmcmFileClass
      write  FmcmFileClass;
    Property FileClassID : TmcmFileFormat
      read   FFileClassID
      write  FFileClassID;
    Property Extension : string
      read   FExtension
      write  FExtension;
    Property Description : string
      read   FDescription
      write  FDescription;
    Property ReadEnabled : boolean
      read   FReadEnabled
      write  FReadEnabled;
    Property WriteEnabled : boolean
      read   FWriteEnabled
      write  FWriteEnabled;
  end;


//------------------------------------------------------------------------------
// TmcmFileFormatsList
//------------------------------------------------------------------------------

  TmcmFileFormatsList = class(TList)
  private
  protected
    function GetItems(Index : integer) : TmcmFileFormatItem;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(const Ext, Desc : string; ClassID : TmcmFileFormat; AClass : TmcmImageFileClass; CanRead, CanWrite : boolean);
    procedure   Clear; {$IFNDEF DCB3} override; {$ENDIF}
    function    FindID(ID : TmcmFileFormat) : TmcmFileFormatItem;
    function    FindExt(Ext : string) : TmcmFileFormatItem;
    function    FindClassName(const Classname : string) : TmcmImageFileClass;
    procedure   Remove(FileClass : TmcmImageFileClass);
    procedure   BuildFilterStrings(    FileClass    : TmcmImageFileClass;
                                   var Descriptions : string;
                                   var Filters      : string;
                                       ReadFilter   : boolean);
    property    Items[Index : integer] : TmcmFileFormatItem
      read      GetItems;
  end;


//------------------------------------------------------------------------------
// TmcmImageFileMgr.
//
// Image file manager.
// Used to load and save images using any of the included formats.
//------------------------------------------------------------------------------
  TOnExtFileFormat = procedure(    Sender   : TObject;
                                   FileName : string;
                               var AHandle  : {$IFDEF BCB} TmcmHBITMAP
                                              {$ELSE} HBITMAP {$ENDIF}) of object;

  TOnExtVerifyFileFormat = procedure(    Sender   : TObject;
                                         FileName : string;
                                     var Valid    : boolean) of object;

  TmcmImageFileMgr = class(TPersistent)
  private
    FCounterLock  : TRTLCriticalSection;
    FError        : TmcmErrorCode;
    FLock         : TRTLCriticalSection;
    FLockCount    : integer;
  protected
    FFormatList   : TmcmFileFormatsList;
    FCompress     : TmcmCompress;    // Compression method CP_xxx
    FImageFile    : TmcmImageFile;   //
    FImageInfo    : TmcmImageInfo;   //
    FInterlaced   : boolean;         // Image is interlaced.
    FIndex        : integer;         // Index to image up for reading.
    FIsMulti      : boolean;
    FQuality      : word;            // Compression quality 1..100 per cent.
    FSafeBiLevel  : boolean;         // TIFF bi-level only.
    FYCbCrMode    : TmcmJPEGYCbCr;   // JPEG files only - Y, Cb and Cr ratio.
    FOnExtFormat  : TOnExtFileFormat;

    function    DoFileAssociate(Ext : string; FileType : string; Description : string; ExeName : string; IcoIndex : integer) : boolean;
    function    RemoveFileAssociate(Ext : string; FileType : string) : boolean;
    function    GetIsMultiImage : boolean;
    function    GetImageFileClass(Value : TmcmFileFormat) : TmcmImageFile;
    function    GetImageInfo : TmcmImageInfo;
    procedure   Lock;
    procedure   SetCompress(Value : TmcmCompress);
    procedure   SetInterlaced(Value : boolean);
    procedure   SetQuality(Value : word);
    procedure   Unlock;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AppendImage(Filename : string; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
    function    AssociateFileFormat(FileFormat : TmcmFileFormat; ExeName : string; IcoIndex : integer; Add : boolean) : boolean;
    function    FileFormatToStr(Value : TmcmFileFormat) : string;
    function    GetCompressionFromColor(FileFormat : TmcmFileFormat; ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
    function    GetCompressionName(FileFormat : TmcmFileFormat; Compression : TmcmCompress) : string;
    function    GetDefaultExtension(FileFormat  : TmcmFileFormat; ImageFormat : TmcmImageFormat) : string;
    function    GetExtensionFromFormat(Value : TmcmFileFormat) : string;
    function    GetFormatFromName(Value : string) : TmcmFileFormat;
    function    GetFormatList : TmcmFileFormatsList; // Internal use only.
    procedure   GetFormatsFromColor(ImageFormat : TmcmImageFormat; var FormatList : TmcmFileFormatsList); // : TmcmFileFormats;
    function    GetReadFilter : string;
    //    function    GetSupportedFormats : TmcmFileFormats;
    function    GetWriteFilter : string;
    function    IsFileFormatAssociated(FileFormat : TmcmFileFormat; ExeName : string) : boolean;
    function    LoadFromStream(Stream : TStream; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    LoadImage(Filename : string; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    LoadNext(Filename : string; FileFormat : TmcmFileFormat; Index : integer) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    ReadImage(Filename : string; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    RegisterFileFormat(FileClass : TmcmImageFileClass; ClassId : TmcmFileFormat; Extension, Description : string; CanRead, CanWrite : boolean) : integer;
    procedure   SaveImage(Filename : string; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
    procedure   SaveToStream(Stream : TStream; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
    //    procedure   SetSupportedFormats(Value : TmcmFileFormats);
    procedure   UnregisterAllFileFormats;
    procedure   UnregisterFileFormat(FileClass : TmcmImageFileClass);
    function    VerifyImageFile(FileName : string) : boolean;

    property    Compression  : TmcmCompress
      read      FCompress
      write     SetCompress;
    property    Error : TmcmErrorCode
      read      FError;
    property    ImageIndex : integer
      read      FIndex
      write     FIndex;
    property    ImageInfo : TmcmImageInfo
      read      GetImageInfo;
    property    Interlaced : boolean
      read      FInterlaced
      write     SetInterlaced;
    property    IsMultiImage : boolean
      read      GetIsMultiImage;
    property    Quality : word
      read      FQuality
      write     SetQuality;
    property    SafeBiLevel : boolean
      read      FSafeBiLevel
      write     FSafeBiLevel default True;
    property    YCbCrMode : TmcmJPEGYCbCr
      read      FYCbCrMode
      write     FYCbCrMode default JYCC_411;
    property    OnExternalFileFormat : TOnExtFileFormat
      read      FOnExtFormat
      write     FOnExtFormat;
  published
  end;

var ImageFileManager : TmcmImageFileMgr;

implementation

uses {$IFNDEF GE_DXE2}
      Registry, ShlObj, SysUtils, Dialogs, Graphics,
     {$ELSE}
      System.Win.Registry, WinApi.ShlObj, System.SysUtils, Vcl.Dialogs, Vcl.Graphics,
     {$ENDIF}

     {$IFNDEF mcmJPEGEnginee}
       JPEG,
     {$ENDIF}
     mcmImageColor,
     mcmImageResStr;

type
  EPNGException = class(Exception);
  EJPEGException = class(Exception);


function SwapWord(Value : word) : word; register;
asm
  XCHG  AL,AH
end; // SwapWord.


function SwapLong(Value : longword) : longword; register;
asm
  BSWAP EAX
end; // SwapLong.


function Maximum(A, B : longint) : longint;
asm
  cmp   EAX,EDX
  JLE   @ReturnA
  MOV   EAX,EDX
  @ReturnA:
end; // Maximum.


function Minimum(A, B : longint) : longint;
asm
  cmp   EAX,EDX
  JL   @ReturnA
  MOV   EAX,EDX
  @ReturnA:
end; // Minimum.


function Log(x : double) : double;
var Ln10 : double;
begin
  if (x < 0.0)
  then Log := 0.0
  else begin
       Ln10 := ln(10.0);
       Log  := ln(x) / Ln10;
  end;
end; // End Log.


//------------------------------------------------------------------------------
// TmcmImageFile
//------------------------------------------------------------------------------

class function TmcmImageFile.GetColorFormats : TmcmImageFormats;
begin
  Result := [];
end; // TmcmImageFile.GetColorFormats.


class function TmcmImageFile.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
begin
  if (High(Compress) >= 0)
  then Compress[0] := CP_NOCOMP;
  Result := 1;
end; // TmcmImageFile.GetCompressionFromColor.


class function TmcmImageFile.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP : Result := resCP_NOCOMP;
  end;
end; // TmcmImageFile.GetCompressionName.


constructor TmcmImageFile.Create;
var bMMX : boolean;
begin
  Inherited Create;
  FMMX := False;
  try
    asm
      mov   bMMX,$00
      mov   eax,1
      {$IFNDEF DCB3_5}
      cpuid
      {$ELSE}
      dw    CPUID
      {$ENDIF}
      test  edx,$800000
      jz    @NoMMX
      mov   bMMX,$01
      @NoMMX:
    end;
  except
  // do nothing.
  end;
  FMMX := bMMX;

  InitializeCriticalSection(FCounterLock);
  InitializeCriticalSection(FLock);
  FStream    := TmcmBufferStream.Create;
  FStream.MemorySize := $FFFF;
  FCompress  := CP_NOCOMP;
  FQuality   := 100;
  FProgress  := 0;

  FCanAppend := False;
  FIndex     := 0;    // Index to image up for reading.
  FNumImages := 1;

  FImageInfo := Nil;
  FDibHandle := 0;
  FLongWidth := 0;
  FStreamStart := 0;
  FStream.BigEndian := False;
  FMemStream := TMemoryStream.Create;
  GetMem(FDibInfo, SizeOf(TBitmapInfoHeader) + 256 * SizeOf(TRGBQuad));
  FillChar(FDibInfo^, SizeOf(TBitmapInfoHeader), 0);
end; // TmcmImageFile.Create.


destructor TmcmImageFile.Destroy;
begin
  FMemStream.Clear;
  FMemStream.Free;
  FStream.Free;
  FreeMem(FDibInfo);
  DeleteCriticalSection(FLock);
  DeleteCriticalSection(FCounterLock);
  Inherited Destroy;
end; // TmcmImageFile.Destroy.


procedure TmcmImageFile.Lock;
begin
  EnterCriticalSection(FCounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(FCounterLock);
  EnterCriticalSection(FLock);
end; // TmcmImageFile.Lock.


procedure TmcmImageFile.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(FCounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(FCounterLock);
end; // TmcmImageFile.Unlock.


function TmcmImageFile.CreateDib : boolean;
var dibDC : HDC;
begin
  // Check & set image size.
  with FDibInfo^.bmiHeader
  do begin
     biSize := SizeOf(TBitmapInfoHeader);
     FBitCount := biPlanes * biBitCount;
     FLongWidth := (((biWidth * FBitCount) + 31) div 32) * 4;
     FNoColors := GetNumColors(@FDibInfo^.bmiHeader);
     biSizeImage := DWORD(FLongWidth * Abs(biHeight));
     biCompression := BI_RGB;
  end;

  // Create device independent bitmap.
  dibDC := GetDC(0);
  try
    FDibHandle := CreateDIBSection(dibDC, FDibInfo^, DIB_RGB_COLORS, FDibBits, 0, 0);
    // Need to fill image with zero's as some decompression methods hereunder
    // Huffman, CCITT etc. may omit setting background pixels (Palette index 0).
    FillMemory(FDibBits, FDibInfo^.bmiHeader.biSizeImage, 0);
  finally
    ReleaseDC(0, dibDC);
    if (FDibHandle = 0)
    then begin
         FError := EC_WINDOWS;
         FWinError := GetLastError;
    end;
  end;
  Result := (FDibHandle <> 0);
end; // TmcmImageFile.CreateDib.


function TmcmImageFile.GetDibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
begin
  Result := FDibHandle;
end; // TmcmImageFile.GetDibHandle.


procedure TmcmImageFile.SetDibHandle(Value : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
begin
  FDibHandle := Value;
end; // TmcmImageFile.SetDibHandle.


function TmcmImageFile.GetDibData : boolean;
begin
  if (GetObject(FDibHandle, SizeOf(TDIBSection), @FDIBSection) <> 0)
  then begin
       // Copy BitmapInfoHeader and pointer to bitmap bits.
       FDibBits := FDIBSection.dsBm.bmBits;
       CopyMemory(@FDibInfo^.bmiHeader, @FDIBSection.dsBmih, SizeOf(TBitmapInfoHeader));
       with FDibInfo^.bmiHeader
       do begin
          FBitCount := biPlanes * biBitCount;
          FLongWidth := (((biWidth * FBitCount) + 31) div 32) * 4;
          biSizeImage := FLongWidth * Abs(biHeight);
       end;
       FNoColors := GetNumColors(@FDIBSection.dsBmih);
       if (FNoColors > 0)
       then GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);
       if Assigned(FImageInfo)
       then begin
            if FImageInfo.Modified
            then begin
                 with FDibInfo^.bmiHeader
                 do begin
                    FImageInfo.Units := UN_METERS;
                    biXPelsPerMeter := Round(FImageInfo.XResolution);
                    biYPelsPerMeter := Round(FImageInfo.YResolution);
                 end;

            end;
       end;
       Result := (FDibBits <> Nil);
  end
  else begin
       Result := False;
       FError := EC_BADIMAGEHANDLE;
  end;
end; // TmcmImageFile.GetDibData.


function TmcmImageFile.GetNumColors(pDibInfo : PBitmapInfo) : integer;
begin
  Result := 0;
  if (pDibInfo^.bmiHeader.biBitCount <= 8)
  then Result := 1 shl pDibInfo^.bmiHeader.biBitCount
  else if (pDibInfo^.bmiHeader.biClrUsed > 0)
       then Result := pDibInfo^.bmiHeader.biClrUsed;
end; // TmcmImageFile.GetNumColors.


function TmcmImageFile.GetPalFromDibHandle(var ColorTable : array of TRGBQuad) : integer;
var dibDC    : HDC;
    SaveDib  : THandle;
    PalSize  : integer;
begin
  Result := 0;
  PalSize := High(ColorTable) + 1;
  if (FDIBHandle <> 0) and (PalSize > 0)
  then begin
       dibDC := CreateCompatibleDC(0);
       SaveDib := SelectObject(dibDC, FDIBHandle);
       PalSize := GetDIBColorTable(dibDC, 0, PalSize, ColorTable);
       SelectObject(dibDC, SaveDib);
       DeleteDC(dibDC);
       Result := PalSize;
  end;
end; // TmcmImageFile.GetPalFromDibHandle.


function TmcmImageFile.GetPhotometric : TmcmPhotometric;
var NoColors   : integer;
    i, j       : integer;
//    ColourShft : integer;
begin
  with FDibInfo^
  do begin
     if (FBitCount <= 8)
     then begin
          {$R-}
          NoColors := 1 shl FBitCount;
          Result := PM_GREY;
          i := 0;
          while (i < NoColors) and (Result = PM_GREY)
          do begin
             if (bmiColors[i].rgbRed  <> bmiColors[i].rgbGreen) or
                (bmiColors[i].rgbBlue <> bmiColors[i].rgbGreen)
             then Result := PM_PALETTE;
             inc(i);
          end;
          if (Result = PM_GREY)
          then begin
               if (bmiColors[NoColors-1].rgbRed < bmiColors[0].rgbRed)
               then begin
                    Result := PM_INVGREY;
                    i := 0;
                    j := NoColors - 1;
                    while (i < NoColors) and (Result = PM_GREY)
                    do begin
                       if (bmiColors[i].rgbRed  <> j)
                       then Result := PM_PALETTE;
                       dec(j);
                       inc(i);
                    end;
               end
               else begin
               {
                    i := 0;
                    case bmiHeader.biBitCount of
                    1 : ColourShft := 8;
                    4 : ColourShft := 4;
                    8 : ColourShft := 0;
                    else ColourShft := 0;
                    end;
                    while (i < NoColors) and (Result = PM_GREY)
                    do begin
                       if (bmiColors[i].rgbRed  <> i shl ColourShft)
                       then Result := PM_PALETTE;
                       inc(i);
                    end;
               }
               end;
          end;
     end
     else begin
          case FBitCount of
          15,
          16 : Result := PM_RGB16;
          24 : Result := PM_RGB; // RGB 3 x 256 Colors.
          32 : Result := PM_RGBA; // RGB 4 x 256 Colors.
          else Result := PM_RGB;
          end;
     end
  end;
end; // TmcmImageFile.GetPhotometric.


function TmcmImageFile.GetImageInfo : TmcmImageInfo;
begin
  Result := FImageInfo;
end; // TmcmImageFile.GetImageInfo.


procedure TmcmImageFile.SetImageInfo(Value : TmcmImageInfo);
begin
  FImageInfo := Value;
end; // TmcmImageFile.SetImageInfo.


procedure TmcmImageFile.SetbmiPalToDibHandle;
var dibDC    : HDC;
    SaveDib  : THandle;
    Count    : integer;
    NoColors : integer;
begin
  if (FDIBHandle <> 0)
  then begin
       NoColors := GetNumColors(FDibInfo);
       if (0 < NoColors) and (NoColors <= 256)
       then begin
            dibDC := CreateCompatibleDC(0);
            SaveDib := SelectObject(dibDC, FDIBHandle);
            Count := SetDIBColorTable(dibDC, 0, NoColors, PRGBQuadArray(@FDibInfo^.bmiColors)^);
            if (Count <> NoColors)
            then ;
            SelectObject(dibDC, SaveDib);
            DeleteDC(dibDC);
       end;
  end;
end; // TmcmImageFile.SetPalToDibHandle.


function TmcmImageFile.CreatePalette : HPalette;
var pLogPal  : PLogPalette;
    i        : integer;
    NoColors : integer;
begin
  Result := 0;
  if (FDibHandle <> 0) and Assigned(FDibInfo)
  then begin
       NoColors := GetNumColors(FDibInfo);
       if (NoColors > 0)
       then begin
            GetMem(pLogPal, SizeOf(TLogPalette) + NoColors * SizeOf(TPaletteEntry));
            for i := 0 to Pred(NoColors)
            do begin
               with pLogPal^
               do begin
                  palNumEntries := NoColors;
                  palVersion    := $0300;
                  with palPalEntry[i], FDibInfo^.bmiColors[i]
                  do begin
                     peRed   := rgbRed;
                     peBlue  := rgbBlue;
                     peGreen := rgbGreen;
                     peFlags := 0;
                  end;
               end;
            end;
            Result := {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.CreatePalette(pLogPal^);
            FreeMem(pLogPal);
       end;
  end;
end; // TmcmImageFile.CreatePalette.


procedure TmcmImageFile.CreateGreyPalette(Photometric : TmcmPhotometric);
var i        : integer;
    NoColors : integer;
begin
  if Assigned(FDibInfo)
  then begin
       NoColors := GetNumColors(FDibInfo);
       case Photometric of
       PM_INVGREY   : // Bilevel and grayscale image, white = 0 and black = 255.
                      case FDibInfo^.bmiHeader.biBitCount of
                      1 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte((1 - i) * 255);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte((1 - i) * 255);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte((1 - i) * 255);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      2 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(255 - (85 * i));
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(255 - (85 * i));
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(255 - (85 * i));
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      4 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(255 - (17 * i));
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(255 - (17 * i));
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(255 - (17 * i));
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      8 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(255 - i);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(255 - i);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(255 - i);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      end;
       PM_GREY,       // Bilevel and grayscale image, white = 255 and black = 0.
       PM_PALETTE   : // Palette color image.
                      case FDibInfo^.bmiHeader.biBitCount of
                      1 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(255 * i);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(255 * i);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(255 * i);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      2 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(85 * i);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(85 * i);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(85 * i);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      4 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(17 * i);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(17 * i);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(17 * i);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      8 : begin
                            for i := 0 to (NoColors - 1)
                            do begin
                               FDibInfo^.bmiColors[i].rgbRed      := byte(i);
                               FDibInfo^.bmiColors[i].rgbGreen    := byte(i);
                               FDibInfo^.bmiColors[i].rgbBlue     := byte(i);
                               FDibInfo^.bmiColors[i].rgbReserved := 0;
                            end;
                          end;
                      end;
       end;
  end;
end; // TmcmImageFile.CreateGreyPalette.


function TmcmImageFile.OpenFile(Filename : string; Mode : word; var Stream : TStream) : boolean;
begin
  Stream := Nil; // In case of exception.
  try
    Lock;
    FStreamStart := 0;
    FStream.Source := Nil;
    Stream := TFileStream.Create(FileName, Mode);
    if Assigned(FImageInfo)
    then FImageInfo.DateTime := FileDateToDateTime(FileGetDate(TFileStream(Stream).Handle));
  except
    if Assigned(Stream)
    then TFileStream(Stream).Free;
    Stream := Nil;
    if ((Mode and fmCreate) = fmCreate)
    then FError := EC_CREATEFILE
    else FError := EC_OPENFILE;
  end;
  if Not(Assigned(Stream))
  then Unlock;
  Result := Assigned(Stream);
end; // TmcmImageFile.OpenFile.


procedure TmcmImageFile.CloseFile(var Stream : TStream);
begin
  if Assigned(FStream)
  then FStream.Source := Nil;
  if Assigned(FImageInfo)
  then FImageInfo.FileSize := Stream.Size;
  if Assigned(Stream)
  then TFileStream(Stream).Free;
  Stream := Nil;
  Unlock;
end; // TmcmImageFile.CloseFile.


procedure TmcmImageFile.SetStreamPos(Pos : {$IFNDEF DCB3_5} int64
                                           {$ELSE} longint {$ENDIF});
begin
  FStream.Position := Pos + FStreamStart;
end; // TmcmImageFile.SetStreamPos.


function TmcmImageFile.AddOffset(AOffset : Cardinal) : Cardinal;
begin
  Result := 0;
end; // TmcmImageFile.AddOffset.


procedure TmcmImageFile.SetImageIndex(Value : integer);
begin
  if (FIndex <> Value)
  then FIndex := Value;
end; // TmcmImageFile.SetImageIndex.


function TmcmImageFile.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := EC_BADFORMAT;
  if Assigned(Stream)
  then FStreamStart := Stream.Position
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmImageFile.IsFormatValid.


function TmcmImageFile.LoadImage(Filename : string) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
var Stream : TStream;
begin
  FDibHandle := 0;
  if OpenFile(Filename, fmOpenRead or fmShareDenyWrite, Stream)
  then begin
       try
         LoadFromStream(Stream);
       finally
         CloseFile(Stream);
       end;
  end;
  Result := FDibHandle;
end; // TmcmImageFile.LoadImage.


{
function TmcmImageFile.LoadNext(Filename : string; Index : integer) : HBITMAP;
begin
  FDibHandle := 0;
  if (FNumImages <> 1)
  then begin
       // Use index to determin which image to load.
       // If index is -1 then simply load successive image
       if (FStream.Source <> Nil)
       then LoadFromStream(FStream.Source);
  end;
  Result := FDibHandle;
end; // TmcmImageFile.LoadNext.
}


procedure TmcmImageFile.SaveImage(Filename : string; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
var Stream : TStream;
begin
  FDibHandle := Bitmap;
  if OpenFile(Filename, fmCreate or fmOpenWrite or fmShareExclusive, Stream)
  then begin
       try
         SaveToStream(Stream);
       finally
         CloseFile(Stream);
       end;
  end;
end; // TmcmImageFile.SaveImage.


procedure TmcmImageFile.AppendImage(Filename : string; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
var Stream : TStream;
begin
  if FCanAppend
  then begin
       FDibHandle := Bitmap;
       if OpenFile(Filename, fmOpenReadWrite or fmShareExclusive, Stream)
       then begin
            try
              FIndex := -1;
              Stream.Position := Stream.Size;
              SaveToStream(Stream);
            finally
              CloseFile(Stream);
            end;
       end;
  end
  else FError := EC_APPENDIMAGE;
end; // TmcmImageFile.AppendImage.


procedure TmcmImageFile.SwapByteRB;
// Swap Red and Blue colour component.
type TARGB  = array[0..0] of TRGBTriple;
var  i      : longword;
     Start  : TVecPointerRec;
     ToAddr : TVecPointerRec;
     Bits   : TVecPointerRec;
     RVal   : byte;
     GVal   : byte;
begin
  Start.Long := 0;
  Bits.Ptr   := FDibBits;
  if (Bits.Ptr <> nil)
  then begin
       while (DWORD(Start.Long) < FDibInfo^.bmiHeader.biSizeImage)
       do begin
          ToAddr.Long := Bits.Long + Start.Long;
          try
            for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
            do begin
               RVal := TARGB(ToAddr.Ptr^)[i].RGBtblue;
               // This line generates an access violation.
               // TARGB(ToAddr.Ptr^)[i].RGBtblue := TARGB(ToAddr.Ptr^)[i].RGBtred;
               // Therefore GVal is introduced, which does not generate the AV !?!
               GVal := TARGB(ToAddr.Ptr^)[i].RGBtred;
               TARGB(ToAddr.Ptr^)[i].RGBtblue := GVal;
               TARGB(ToAddr.Ptr^)[i].RGBtred  := RVal;
            end;
          except
          end;
          Start.Long := Start.Long + FLongWidth;
       end;
  end;
end; // TmcmImageFile.SwapByteRB.


procedure TmcmImageFile.InvertImage;
var mcmImageColor : TmcmImageColor;
begin
  mcmImageColor := TmcmImageColor.Create(Nil);
  mcmImageColor.SourceImage[0] := TmcmImage.Create;
  mcmImageColor.SourceImage[0].DibHandle := FDibHandle;
  mcmImageColor.SourceImage[0].ReleaseHandle;
  mcmImageColor.ResultImage := mcmImageColor.SourceImage[0];
  mcmImageColor.Invert;
  mcmImageColor.SourceImage[0].Free;
  mcmImageColor.Free;
end; // TmcmImageFile.InvertImage.


function TmcmImageFile.ReadStreamData(Stream : TStream; pData : pointer; DataSize : longint) : boolean;
var Count  : longint;
    Start  : TVecByteRec;
    ToAddr : TVecByteRec;
    Bits   : TVecByteRec;
begin
  Result     := False;
  Bits.Ptr   := pData;
  Start.Long := 0;
  if (Bits.Ptr <> nil)
  then begin
       Count := DataSize;
       while (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + Start.Long;
          if (Count > $8000)
          then Count := $8000;
          FStream.Read(ToAddr.Ptr^, Count);
          Start.Long := Start.Long + Count;
          Count := DataSize - Start.Long;
       end;
       Result := True;
  end;
end; // TmcmImageFile.ReadStreamData.


function TmcmImageFile.WriteStreamData(Stream : TStream; pData : pointer; DataSize : longint) : boolean;
var Count  : longint;
    Start  : TVecByteRec;
    ToAddr : TVecByteRec;
    Bits   : TVecByteRec;
begin
  Result     := False;
  Bits.Ptr   := pData;
  Start.Long := 0;
  if (Bits.Ptr <> nil)
  then begin
       Count := DataSize;
       while (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + Start.Long;
          if (Count > $8000)
          then Count := $8000;
          FStream.Write(ToAddr.Ptr^, Count);
          Start.Long := Start.Long + Count;
          Count := DataSize - Start.Long;
       end;
       Result := True;
  end;
end; // TmcmImageFile.WriteStreamData.


function TmcmImageFile.GetNoImages : integer;
begin
  Result := FNumImages;
end; // TmcmImageFile.GetNoImages.


//------------------------------------------------------------------------------
// TmcmBmpImage
//------------------------------------------------------------------------------

class function TmcmBmpImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB15,IF_RGB16,IF_RGB24,IF_RGBA32];
end; // TmcmBmpImage.GetColorFormats.


class function TmcmBmpImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := Inherited GetCompressionFromColor(ImageFormat, Compress);
  if (ImageFormat in [IF_NONE,IF_GREY4,IF_PAL4])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE4;
       inc(Count);
  end;
  if (ImageFormat in [IF_NONE,IF_GREY8,IF_PAL8])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE8;
       inc(Count);
  end;
  Result := Count;
end; // TmcmBmpImage.GetCompressionFromColor.


class function TmcmBmpImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP : Result := resCP_NOCOMP;
  CP_RLE4   : Result := resCP_RLE4;
  CP_RLE8   : Result := resCP_RLE8;
  else Result := '';
  end;
end; // TmcmBmpImage.GetCompressionName.


function TmcmBmpImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var TestWin30Bitmap : longint;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Read(FBmpFileHeader, SizeOf(TBitmapFileHeader));
         if (FBmpFileHeader.bfType = word(ord('M') shl 8 + ord('B')))
         then begin
              SetStreamPos(SizeOf(TBitmapFileHeader));
              FStream.Read(TestWin30Bitmap, SizeOf(TestWin30Bitmap));
              if (TestWin30Bitmap = SizeOf(TBitmapCoreHeader)) or // OS/2 Bitmap, version 2
                 (TestWin30Bitmap = SizeOf(TBitmapInfoHeader)) or // Windows Bitmap, version 3
                 (TestWin30Bitmap = SizeOf(TBitmapV4Header)) or // Windows Bitmap, version 4
                 (TestWin30Bitmap = 64) //
                 //(TestWin30Bitmap = SizeOf(TBitmapV5Header)) // Windows Bitmap, version 5
              then FError := EC_OK;
         end;
       except
         On EReadError
         do FError := EC_READFROMFILE;
         On Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmBmpImage.IsFormatValid.


function TmcmBmpImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
var CoreInfo   : TBitmapCoreHeader;
begin
  if (FDibInfo <> nil)
  then begin
       // Read bitmap info header.
       FIsOS2 := False;
       SetStreamPos(SizeOf(TBitmapFileHeader));
       FStream.Read(FDibInfo^, SizeOf(TBitmapInfoHeader));

       if (FDibInfo^.bmiHeader.biSize = 12) // OS/2 Bitmap
       then begin
            FIsOS2 := True;
            SetStreamPos(SizeOf(TBitmapFileHeader));
            FStream.Read(CoreInfo, SizeOf(TBitmapCoreHeader));
            with FDibInfo^.bmiHeader
            do begin
               biSize          := CoreInfo.bcSize;
               biWidth         := CoreInfo.bcWidth;
               biHeight        := CoreInfo.bcHeight;
               biPlanes        := 1;
               biBitCount      := CoreInfo.bcBitCount;
               biCompression   := BI_RGB; // OS/2 does not support compression.
               biXPelsPerMeter := 0; // round(30000.0 / 2.54);
               biYPelsPerMeter := 0; // round(30000.0 / 2.54);
               biClrUsed       := 0;
               biClrImportant  := 0;
            end;
       end;
       FDibInfo^.bmiHeader.biPlanes := 1;

       if Not(FDibInfo^.bmiHeader.biBitCount in [1, 4, 8, 15, 16, 24, 32])
       then FError := EC_BITDEPTH // Unsupported color depth.
       else begin
            with FDibInfo^.bmiHeader
            do begin
               case biCompression of
               BI_RGB  : FCompress := CP_NOCOMP;
               BI_RLE4 : FCompress := CP_RLE4;
               BI_RLE8 : FCompress := CP_RLE8;
               end;

               if Assigned(FImageInfo)
               then begin
                    FImageInfo.Units       := UN_METERS;
                    FImageInfo.XResolution := biXPelsPerMeter;
                    FImageInfo.YResolution := biYPelsPerMeter;
                    FImageInfo.XPosition   := 0;
                    FImageInfo.YPosition   := 0;
                    // FImageInfo.Compress    := FCompress;
               end;
            end;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmBmpImage.ReadHeader.


function TmcmBmpImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
var i        : word;
    CorePal  : array[0..255] of TRGBTRIPLE;
begin
  try
    if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
    then FNoColors := GetNumColors(FDibInfo)
    else FNoColors := 0;

    if (FNoColors > 0)
    then begin
         // Go to palette offset in file.
         SetStreamPos(SizeOf(TBitmapFileHeader) + longint(FDibInfo^.bmiHeader.biSize));
         if FIsOS2
         then begin
              FStream.Read(CorePal, (FNoColors * SizeOf(TRGBTRIPLE)));
              for i := 0 to (FNoColors - 1)
              do begin
                 with FDibInfo^.bmiColors[i]
                 do begin
                    rgbBlue     := CorePal[i].rgbtBlue;
                    rgbGreen    := CorePal[i].rgbtGreen;
                    rgbRed      := CorePal[i].rgbtRed;
                    rgbReserved := 0;
                 end;
              end;
         end
         else begin
              if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
              then FStream.Read(FDibInfo^.bmiColors, (FNoColors * SizeOf(TRGBQUAD)));
         end;
    end;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmBmpImage.ReadPalette.


procedure TmcmBmpImage.LoadFromStream(Stream : TStream);
var BufferSize : longword;
    DataCount  : longword;
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       try
         if (IsFormatValid(Stream) = EC_OK) // Read header and check format.
         then begin
              // Read Header.
              if (ReadHeader(Stream) = EC_OK)
              then begin
                   // Create & load/set Palette
                   if (ReadPalette(Stream) = EC_OK)
                   then begin
                        // Load image data.

                        // Set position to where the bitmap data is.
                        SetStreamPos(longint(FBmpFileHeader.bfOffBits));

                        if CreateDIB
                        then begin
                             case FCompress of
                             CP_RLE8 : FCompObj := TmcmImageRLE8.Create;
                             CP_RLE4 : FCompObj := TmcmImageRLE4.Create;
                             else FCompObj := Nil;
                             end;

                             if Assigned(FCompObj)
                             then begin
                                  FCompObj.Image := FDibHandle;
                                  FMemStream.Clear;
                                  FMemStream.SetSize(FStream.Size - FStream.Position);
                                  //FStream.MemorySize := FStream.Size - FStream.Position;
                             end;

                             try
                               case FCompress of
                               CP_NOCOMP    : FStream.Read(FDibBits^, FDibInfo^.bmiHeader.biSizeImage);
                               CP_RLE4,
                               CP_RLE8      : begin
                                                FStream.Read(FMemStream.Memory^, FMemStream.Size);
                                                BufferSize := FMemStream.Size;
                                                FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                              end;
                               // BI_BITFIELDS : ;
                               // Compression scheme not supported.
                               else FError := EC_COMPRESSION;
                               end;
                             except
                               On Exception
                               do FError := EC_ENDOFFILE;
                             end;
                             if Assigned(FCompObj)
                             then FCompObj.Free;
                        end;
                   end;
              end;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmBmpImage.LoadFromStream.


function TmcmBmpImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
begin
  FError := EC_OK;
  try
    // Write bitmap file header.
    with FBmpFileHeader, FDibInfo^.bmiHeader
    do begin
       bfOffBits   := SizeOf(TBitmapFileHeader) +
                      SizeOf(TBitmapInfoHeader) + FNoColors * SizeOf(TRGBQuad);
       bfReserved1 := 0;
       bfReserved2 := 0;
       bfSize      := bfOffBits + biSizeImage;
       bfType      := word(ord('M') shl 8 + ord('B'));
    end;
    FStream.Write(FBmpFileHeader, SizeOf(TBitmapFileHeader));

    // Write bitmap info header
    case FCompress of
    CP_NOCOMP : FDibInfo^.bmiHeader.biCompression := BI_RGB;
    CP_RLE4   : FDibInfo^.bmiHeader.biCompression := BI_RLE4;
    CP_RLE8   : FDibInfo^.bmiHeader.biCompression := BI_RLE8;
    else FError := EC_COMPRESSION;
    end;

    // Write header to file.
    FStream.Write(FDibInfo^, SizeOf(TBitmapInfoHeader));
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmBmpImage.WriteHeader.


function TmcmBmpImage.WritePalette(Stream : TStream) : TmcmErrorCode;
begin
  try
    FStream.Write(FDibInfo^.bmiColors, (FNoColors * SizeOf(TRGBQuad)));
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmBmpImage.WritePalette.


procedure TmcmBmpImage.SaveToStream(Stream : TStream);
var Count         : longint;
    Buffer        : Pointer;
    BufferSize    : longword;
    DataCount     : longword;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Palette.
                 if (FNoColors > 0)
                 then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                      // Write bitmap data
                      DataCount := FDibInfo^.bmiHeader.biSizeImage;

                      case FDibInfo^.bmiHeader.biCompression of
                      BI_RGB       : FStream.Write(FDibBits^, FDibInfo^.bmiHeader.biSizeImage);
                      BI_RLE4,
                      BI_RLE8      : begin
                                       if (FDibInfo^.bmiHeader.biCompression = BI_RLE4)
                                       then FCompObj := TmcmImageRLE4.Create
                                       else FCompObj := TmcmImageRLE8.Create;
                                       FCompObj.Image := FDibHandle;
                                       Count := 0;
                                       while (FError = EC_OK) and (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                                       do begin
                                          FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                                          if (FError = EC_OK)
                                          then begin
                                               FStream.Write(Buffer^, BufferSize);
                                               inc(Count, DataCount);
                                          end;
                                       end;
                                       if Assigned(FCompObj)
                                       then FCompObj.Free;
                                     end;
                      BI_BITFIELDS : FError := EC_COMPRESSION;
                      else FError := EC_COMPRESSION; // Compression scheme not supported.
                      end;
                 end;
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmBmpImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmMCFImage
//------------------------------------------------------------------------------

{$IFDEF MCMFILEFORMAT}

constructor TmcmMCFImage.Create;
begin
  Inherited Create;
  FDistanceShift     := 3; // or number of bits for Length.
  FLengthMask        := (1 shl FDistanceShift) - 1;
  FSlidingWindowSize := (1 shl (16 - FDistanceShift));
  FMaxMatchLength    := FLengthMask + 3;
  FLookAheadSize     := FMaxMatchLength;
end; // TmcmMCFImage.Create.


class function TmcmImageFile.ClassId : integer;
begin
  Result := integer(FF_NONE);
end; // TmcmImageFile.ClassId.


class function TmcmImageFile.ClassDesc : string;
begin
  Result := resUnknown;
end; // TmcmImageFile.ClassDesc.


class function TmcmImageFile.ClassExt : string;
begin
  Result := '';
end; // TmcmImageFile.ClassExt.


function TmcmMCFImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var mcmID : array[0..3] of AnsiChar;
begin
  Inherited IsFormatValid(Stream);
  if (FError <> EC_FILENOTOPEN)
  then begin
       try
         FStream.Read(mcmID, SizeOf(mcmID));
         if (mcmID = 'MCMF')
         then begin
              FError := EC_OK;
         end;
       except
         FError := EC_READFROMFILE;
       end;
  end;
  Result := FError;
end; // TmcmMCFImage.IsFormatValid.


function TmcmMCFImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK) // Read header and check format.
         then begin
              FStream.Read(FCompress, SizeOf(FCompress));
              FStream.Read(FQuality, SizeOf(FQuality));
              FStream.Read(FDistanceShift, SizeOf(FDistanceShift));
              FStream.Read(FDibInfo^, SizeOf(TBitmapInfoHeader));
              FDibInfo^.bmiHeader.biCompression := BI_RGB;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmMCFImage.ReadHeader.


function TmcmMCFImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
begin
  try
    if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
    then FNoColors := GetNumColors(FDibInfo)
    else FNoColors := 0;

    if (FNoColors > 0)
    then FStream.Read(FDibInfo^.bmiColors, (FNoColors * SizeOf(TRGBQUAD)));
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmMCFImage.ReadPalette.


function TmcmMCFImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var mcmID        : array[0..3] of AnsiChar;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;
       try
         mcmID := 'MCMF';
         FStream.Write(mcmID, SizeOf(mcmID));

         // Write header to file.
         FStream.Write(FCompress, SizeOf(FCompress));
         FStream.Write(FQuality,  SizeOf(FQuality));
         FStream.Write(FDistanceShift, SizeOf(FDistanceShift));
         FDibInfo^.bmiHeader.biCompression := BI_RGB;
         FStream.Write(FDibInfo^, SizeOf(TBitmapInfoHeader));
       except
         On E:Exception
         do FError := EC_WRITETOFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmMCFImage.WriteHeader.


function TmcmMCFImage.WritePalette(Stream : TStream) : TmcmErrorCode;
begin
  try
    if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
    then FNoColors := GetNumColors(FDibInfo)
    else FNoColors := 0;

    if (FNoColors > 0)
    then FStream.Write(FDibInfo^.bmiColors, (FNoColors * SizeOf(TRGBQUAD)));
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmMCFImage.WritePalette.


procedure TmcmMCFImage.OnDecompressData(Sender : TObject; Buffer : pointer; var BufferSize : cardinal);
var ActualRead : longint;
begin
  ActualRead := FStream.Read(Buffer^, BufferSize);
  BufferSize := ActualRead;
end; // TmcmMCFImage.OnDecompressData.


procedure TmcmMCFImage.LoadFromStream(Stream : TStream);
var Count      : cardinal;
    BufferSize : longword;
    DataCount  : longword;
begin
  FDibHandle := 0;
  FStream.Source := Stream;
  // Read Header.
  if (ReadHeader(Stream) = EC_OK)
  then begin
       // Create & load/set Palette
       if (ReadPalette(Stream) = EC_OK)
       then begin
            // Load image data.
            if CreateDIB
            then begin
                 case FCompress of
                 CP_RLE8    : FCompObj := TmcmImageRLE8.Create;
                 CP_RLE4    : FCompObj := TmcmImageRLE4.Create;
                 CP_HUFFMAN : FCompObj := TmcmImageHuffman.Create;
                 CP_GIF87A,
                 CP_LZM     : FCompObj := TmcmImageLZ.Create;
                 else FCompObj := Nil;
                 end;

                 if Assigned(FCompObj)
                 then begin
                      FCompObj.Image := FDibHandle;
                      FMemStream.Clear;
                      FMemStream.SetSize(FStream.Size - FStream.Position);


                      if (FCompress = CP_HUFFMAN)
                      then begin
                           BufferSize := FNoColors * SizeOf(Cardinal);

                           FMemStream.Size := BufferSize;
                           FStream.Read(FMemStream.Memory^, BufferSize);
                           TmcmImageHuffman(FCompObj).SetDistribution(FMemStream.Memory, BufferSize);
                      end;
                 end;

                 try
                   case FCompress of
                   CP_NOCOMP    : FStream.Read(FDibBits^, FDibInfo^.bmiHeader.biSizeImage);
                   CP_RLE4,
                   CP_RLE8,
                   CP_HUFFMAN   : begin
                                    Count := 0;
                                    while (Count < FDibInfo^.bmiHeader.biSizeImage) and (FError = EC_OK)
                                    do begin
                                       BufferSize := FStream.Read(FMemStream.Memory^, FMemStream.Size);
                                       FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                       FStream.Position := FStream.Position - longint(BufferSize);
                                       inc(Count, DataCount);
                                       if (DataCount <= 0)
                                       then Break;
                                    end;
                                  end;
                   CP_GIF87A,
                   CP_LZM       : begin
                                    BufferSize := 4096;
                                    FMemStream.Size := BufferSize;

                                    TmcmImageLZ(FCompObj).DistanceShift := FDistanceShift;
                                    TmcmImageLZ(FCompObj).OnNeedData := OnDecompressData;
                                    FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                  end;
                   // BI_BITFIELDS : ;
                   // Compression scheme not supported.
                   else FError := EC_COMPRESSION;
                   end;
                 except
                   On Exception
                   do FError := EC_ENDOFFILE;
                 end;
                 if Assigned(FCompObj)
                 then FCompObj.Free;
            end;
       end;
  end;
end; // TmcmMCFImage.LoadFromStream.


procedure TmcmMCFImage.OnCompressData(Sender : TObject; Buffer : pointer; BufferSize : cardinal);
begin
  FStream.Write(Buffer^, BufferSize);
end; // TmcmMCFImage.OnCompressData.


procedure TmcmMCFImage.SaveToStream(Stream : TStream);
var Count         : longint;
    Buffer        : Pointer;
    BufferSize    : longword;
    DataCount     : longword;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            FCompress := CP_LZM;
            // Write Targa file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Palette.
                 if (FNoColors > 0)
                 then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                      // Write bitmap data
                      DataCount := FDibInfo^.bmiHeader.biSizeImage;

                      case FCompress of
                      CP_RLE4    : FCompObj := TmcmImageRLE4.Create;
                      CP_RLE8    : FCompObj := TmcmImageRLE8.Create;
                      CP_HUFFMAN : FCompObj := TmcmImageHuffman.Create;
                      CP_LZM     : FCompObj := TmcmImageLZ.Create;
                      else FCompObj := Nil;
                      end;

                      case FCompress of
                      CP_NOCOMP  : FStream.Write(FDibBits^, FDibInfo^.bmiHeader.biSizeImage);
                      CP_RLE4,
                      CP_RLE8,
                      CP_HUFFMAN : begin
                                     FCompObj.Image := FDibHandle;
                                     if (FCompress = CP_HUFFMAN)
                                     then begin
                                          TmcmImageHuffman(FCompObj).GetDistribution(Buffer, BufferSize);
                                          FStream.Write(Buffer^, BufferSize);
                                     end;
                                     Count := 0;
                                     while (FError = EC_OK) and (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                                     do begin
                                        FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                                        if (FError = EC_OK)
                                        then begin
                                             FStream.Write(Buffer^, BufferSize);
                                             inc(Count, DataCount);
                                        end;
                                     end;
                                   end;
                      CP_LZM     : begin
                                     FCompObj.Image := FDibHandle;
                                     TmcmImageLZ(FCompObj).EncoderSize := 4096;
                                     TmcmImageLZ(FCompObj).DistanceShift := FDistanceShift;
                                     TmcmImageLZ(FCompObj).OnCompressData := OnCompressData;
                                     FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                                     // Compressed data is returned in OnCompressData.
                                   end;
                      else FError := EC_COMPRESSION; // Compression scheme not supported.
                      end;

                      if Assigned(FCompObj)
                      then FCompObj.Free;
                 end;
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmMCFImage.SaveToStream.

{$ENDIF}


//------------------------------------------------------------------------------
// TmcmTAGList
//------------------------------------------------------------------------------

function TmcmTAGList.Get(Index : integer) : TmcmTIFFTAG;
begin
  Result := Inherited Items[Index];
end; // TmcmTAGList.Get.


procedure TmcmTAGList.Put(Index : integer; Item : TmcmTIFFTAG);
begin
  Inherited Put(Index, Item);
end; // TmcmTAGList.Put.


//------------------------------------------------------------------------------
// TmcmTIFFTAG
//------------------------------------------------------------------------------

constructor TmcmTIFFTAG.Create;
begin
  Inherited Create;
  Clear;
end; // TmcmTIFFTAG.Create.


procedure TmcmTIFFTAG.Clear;
begin
  // Set default TAG values. Correspond to an "Error TAG".
  FTag       := 0;
  FFieldType := TAGF_SHORT;
  FLength    := 1;
  FValOffset := 0;
  FValPtr    := 0;
  FFilePos   := 0;
  FString    := '';
  FRational.Num   := 0;
  FRational.Denom := 1;
  FRational.ND    := 0;
end; // TmcmTIFFTAG.Clear.


destructor TmcmTIFFTAG.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmTIFFTAG.Destroy.


function TmcmTIFFTAG.GetDenom : integer;
begin
  Result := FRational.Denom;
end; // TmcmTIFFTAG.GetDenom.


function TmcmTIFFTAG.GetNum : integer;
begin
  Result := FRational.Num;
end; // TmcmTIFFTAG.GetNum.


procedure TmcmTIFFTAG.SetDenom(Value : integer);
begin
  FRational.Denom := Value;
end; // TmcmTIFFTAG.SetDenom.


procedure TmcmTIFFTAG.SetNum(Value : integer);
begin
  FRational.Num := Value;
end; // TmcmTIFFTAG.SetNum.

//------------------------------------------------------------------------------
// TmcmTAGMgr
//------------------------------------------------------------------------------

constructor TmcmTAGMgr.Create(AStream : TmcmBufferStream; AImageInfo : TmcmImageInfo; IsTIFFTag : boolean);
begin
  Inherited Create;
  FOnWritePalette      := Nil;
  FOnWriteStripOffsets := Nil;
  FOnWriteStripCounts  := Nil;
  FOnWriteJPEGTables   := Nil;

  FIsTIFFTag := IsTIFFTag;
  FImageInfo := AImageInfo;
  FStream    := AStream;

  FEC        := 0;
  FTagList   := TmcmTAGList.Create;
  FNrOfStr   := 0;
  FNrOfRat   := 0;
  FillChar(FValStr, SizeOf(TValueS), 0);
  FillChar(FValRat, SizeOf(TValueR), 0);

  // Create a default tag, used when the actual tag isn't found.
  // The default tag is indexed at position "0".
  ErrorTag;
  FError := EC_OK;
end; // TmcmTAGMgr.Create.


destructor TmcmTAGMgr.Destroy;
begin
  Clear;
  FTagList.Free;
  Inherited Destroy;
end; // TmcmTAGMgr.Destroy.


procedure TmcmTAGMgr.Clear;
var i : word;
begin
  FError := EC_OK;
  try
    while (FNrOfStr > 0)
    do begin
       if (FValStr[FNrOfStr] <> Nil)
       then Dispose(FValStr[FNrOfStr]);
       FValStr[FNrOfStr] := Nil;
       Dec(FNrOfStr);
    end;
  except
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
  try
    while (FNrOfRat > 0)
    do begin
       if (FValRat[FNrOfRat] <> Nil)
       then Dispose(FValRat[FNrOfRat]);
       FValRat[FNrOfRat] := Nil;
       Dec(FNrOfRat);
    end;
  except
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
  try
    for i := (FTagList.Count - 1) downto 0
    do begin
       if Assigned(FTagList[i])
       then begin
            FTagList[i].Free;
            FTagList[i] := Nil;
       end;
    end;
    FEC := 0;
  except
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
end; // TmcmTAGMgr.Clear.


function TmcmTAGMgr.GetImageInfo : TmcmImageInfo;
begin
  Result := FImageInfo;
end; // TmcmTAGMgr.GetImageInfo.


procedure TmcmTAGMgr.SetImageInfo(Value : TmcmImageInfo);
begin
  FImageInfo := Value;
end; // TmcmTAGMgr.SetImageInfo.


function TmcmTAGMgr.GetStream : TmcmBufferStream;
begin
  Result := FStream;
end; // TmcmTAGMgr.GetStream.


procedure TmcmTAGMgr.SetStream(Value : TmcmBufferStream);
begin
  FStream := Value;
end; // TmcmTAGMgr.SetStream.


function TmcmTAGMgr.GetItem(Index : word) : TmcmTIFFTAG;
begin
  if (Index <= FEC)
  then Result := FTagList[Index]
  else Result := Nil;
end; // TmcmTAGMgr.GetItem.


function TmcmTAGMgr.CreateTag : TmcmTIFFTAG;
var NewTAG : TmcmTIFFTAG;
begin
  NewTAG := TmcmTIFFTAG.Create;
  FTagList.Add(NewTAG);
  Result := FTagList[FTagList.Count-1];
end; // TmcmTAGMgr.CreateTag.


procedure TmcmTAGMgr.ErrorTag;
var NewTAG : TmcmTIFFTAG;
begin
  NewTAG := CreateTag;
  NewTAG.Tag       := 0;
  NewTAG.FieldType := TAGF_SHORT;
  NewTAG.Length    := 1;
  NewTAG.ValOffset := 0;
  NewTAG.ValPtr    := 0;
  NewTAG.FilePos   := 0;
end; // TmcmTAGMgr.ErrorTag.


function TmcmTAGMgr.GetTagIndex(TagNr : word) : word;
var i        : word;
    BitCount : word;
    ATag     : TmcmTIFFTAG;
    {$IFDEF IMGDEBUG}
      ErrorStr : string;
    {$ENDIF}
begin
  // Finds which Directory Entry the Tag is saved in.
  i := 1;
  while (i < FTagList.Count) and (FTagList[i].Tag <> TagNr)
  do inc(i);
  if (i > FTagList.Count - 1)
  then i := FTagList.Count - 1;
  if (FTagList[i].Tag = TagNr)
  then Result := i
  else begin
       // Set-up a default tag
       case TagNr of
       // TAG_STRIPBYTECOUNTS - Don't create default Tag entry.
       {
       TAG_NEWSUBFILETYPE       : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_LONG;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 1;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       TAG_SUBFILETYPE          : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 1;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       }
       TAG_BITSPERSAMPLE,
       TAG_COMPRESSION,
       TAG_PHOTOMETRIC,
       TAG_THRESHOLDING,
       TAG_FILLORDER,
       TAG_ORIENTATION,
       TAG_SAMPLESPERPIXEL,
       TAG_PLANARCONFIG,
       TAG_RESOLUTIONUNIT,
       TAG_PREDICTOR            : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 1;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       {
       TAG_ROWSPERSTRIP         : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 0; // $FFFFFFFF;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       }
       TAG_MINSAMPLEVALUE       : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 0;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       TAG_XRESOLUTION,
       TAG_YRESOLUTION          : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_RATIONAL;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 0;
                                    Result := FTagList.Count - 1;

                                    inc(FNrOfRat);
                                    new(FValRat[FNrOfRat]);
                                    FValRat[FNrOfRat]^.Num   := 0;
                                    FValRat[FNrOfRat]^.Denom := 1;
                                    FValRat[FNrOfRat]^.ND    := 0;
                                    ATag.ValPtr := FNrOfRat;
                                  end;
       TAG_XPOSITION,
       TAG_YPOSITION            : begin
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_RATIONAL;
                                    ATag.Length    := 1;
                                    ATag.ValOffset := 0;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;

                                    inc(FNrOfRat);
                                    new(FValRat[FNrOfRat]);
                                    FValRat[FNrOfRat]^.Num   := 0;
                                    FValRat[FNrOfRat]^.Denom := 1;
                                    FValRat[FNrOfRat]^.ND    := 0;
                                    ATag.ValPtr := FNrOfRat;
                                  end;
       TAG_MAXSAMPLEVALUE,
       TAG_GRAYRESPONSECURVE    : begin
                                    BitCount := FTagList[GetTagIndex(TAG_BITSPERSAMPLE)].ValOffset;
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := round(exp(BitCount * ln(2)));
                                    ATag.ValOffset := 0;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;

                                    if (TagNr = TAG_MAXSAMPLEVALUE)
                                    then ATag.Length := ATag.Length - 1;
                                  end;
       TAG_COLORRESPONSECURVES,
       TAG_COLORMAP             : begin
                                    BitCount := FTagList[GetTagIndex(TAG_BITSPERSAMPLE)].ValOffset;
                                    inc(FEC);
                                    ATag := CreateTAG;
                                    ATag.Tag       := TagNr;
                                    ATag.FieldType := TAGF_SHORT;
                                    ATag.Length    := 3 * round(exp(BitCount * ln(2)));
                                    ATag.ValOffset := 0;
                                    ATag.ValPtr    := 0;
                                    Result := FTagList.Count - 1;
                                  end;
       else begin // Un-supported or Unknown TIFF Tag.
            FError := EC_TIFFTAG;
            Result := 0;
            {$IFDEF IMGDEBUG}
              ErrorStr := 'TAG is not available: ' + IntToStr(TagNr);
              OutputDebugString(PChar(ErrorStr));
            {$ENDIF}
       end;
       end;
  end;
end; // TmcmTAGMgr.GetTagIndex.


function TmcmTAGMgr.GetTagStr(Index : word) : AnsiString;
var iv, j : word;
    vd    : array[0..4] of AnsiChar;
begin
  if (Index <= FEC)
  then begin
       iv := FTagList[Index].ValPtr;
       if (iv = 0)
       then begin
            if FStream.BigEndian
            then begin
                 for j := 0 to 3
                 do vd[3-j] := PAnsiChar(@FTagList[Index].ValOffset)[j];
            end
            else begin
                 for j := 0 to 3
                 do vd[j] := PAnsiChar(@FTagList[Index].ValOffset)[j];
            end;
            vd[4] := #0;
            Result := {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPas(vd);

            //Result := StrPas(PAnsiChar(@FTagList[Index].ValOffset))
       end
       else Result := {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPas(PAnsiChar(@FValStr[iv]^[1]));
  end
  else Result := '';
end; // TmcmTAGMgr.GetTagStr.


function TmcmTAGMgr.GetTagVal(Index, Sub : word) : TRational;
var iv : word;
begin
  if (Index <= FEC)
  then begin
       iv := FTagList[Index].ValPtr;
       if (iv = 0) or (iv > FNrOfRat)
       then begin
            Result.Num := 0;
            Result.Denom := 0;
            Result.ND := 0;
       end
       else Result := FValRat[iv + Sub]^;
  end
  else begin
       Result.Num := 0;
       Result.Denom := 0;
       Result.ND := 0;
  end;
end; // TmcmTAGMgr.GetTagVal.


procedure TmcmTAGMgr.ConvByteRational(Index : byte);
var i : word;
begin
  for i := 1 to FTagList[Index].Length
  do begin
     inc(FNrOfRat);
     try
       new(FValRat[FNrOfRat]);
       if (i = 1)
       then FTagList[Index].ValPtr := FNrOfRat;
       FValRat[FNrOfRat]^.Num   := FStream.ReadLong;
       FValRat[FNrOfRat]^.Denom := FStream.ReadLong;
       if (FValRat[FNrOfRat]^.Denom <> 0)
       then FValRat[FNrOfRat]^.ND := FValRat[FNrOfRat]^.Num div FValRat[FNrOfRat]^.Denom;
     except
       FError := EC_NOMEMORY;
     end;
  end;
end; // TmcmTAGMgr.ConvByteRational.


procedure TmcmTAGMgr.ConvByteASCIIZ(Index : byte);
var AnsiStr : AnsiString;
begin
  inc(FNrOfStr);
  new(FValStr[FNrOfStr]);
  FValStr[FNrOfStr]^ := '';
  SetLength(AnsiStr, FTagList[Index].Length);
  FStream.Read(AnsiStr[1], FTagList[Index].Length);
  FValStr[FNrOfStr]^ := String(AnsiStr);
  
  FTagList[Index].ValPtr := FNrOfStr;
end; // TmcmTAGMgr.ConvByteASCIIZ.


procedure TmcmTAGMgr.ConvByteUndefined(Index : byte);
begin
  inc(FNrOfStr);
  new(FValStr[FNrOfStr]);
  FValStr[FNrOfStr]^ := '';
  SetLength(FValStr[FNrOfStr]^, FTagList[Index].Length);
  FStream.Read(FValStr[FNrOfStr]^[1], FTagList[Index].Length);
  FTagList[Index].ValPtr := FNrOfStr;
end; // TmcmTAGMgr.ConvByteUndefined.


procedure TmcmTAGMgr.ReadTags(Offset : longword);
type lwRec = record
                 Low, High : word;
               end;
var i, si : word;
    lw    : longint;
    ATag  : TmcmTIFFTAG;
begin
  FError := EC_OK;
  if (FError = EC_OK) and Assigned(FStream)
  then begin
       si := FEC + 1;
       FEC := FEC + FStream.ReadWord;

       if (FEC > 256)
       then FEC := 256;

       // Read Tags from disk.
       for i := si to FEC
       do begin
          ATag := CreateTAG;
          //ATag.FilePos   := FStream.Position;
          ATag.Tag       := FStream.ReadWord;
          ATag.FieldType := FStream.ReadWord;
          ATag.Length    := FStream.ReadLong;
          ATag.ValOffset := FStream.ReadLong;
          ATag.ValPtr    := 0;
          ATag.FilePos   := 0; // Use only when writing & calculating file position.
       end; // End of Tags read in.

       {$IFDEF IMGDEBUG}
          for i := si to FEC
          do begin
             OutputDebugString(PChar('  Tag: '       + IntToStr(FTagList[i].Tag) +
                                     '  FieldType: ' + IntToStr(FTagList[i].FieldType) +
                                     '  Length: '    + IntToStr(FTagList[i].Length) +
                                     '  ValOffset: ' + IntToStr(FTagList[i].ValOffset) +
                                     '  ValPtr: '    + IntToStr(FTagList[i].ValPtr) +
                                     '  FilePos: '   + IntToStr(FTagList[i].FilePos) ));
         end;
       {$ENDIF}

       FNextIFD := FStream.ReadLong;

       // Search for ASCIIZ and Rational.
       for i := si to FEC
       do begin
          case FTagList[i].FieldType of
          TAGF_SHORT     : begin
                             if (FTagList[i].Length > 2)
                             then begin
                                  FTagList[i].Length := FTagList[i].Length * 2;
                                  FStream.Position := FTagList[i].ValOffset + Offset;
                                  ConvByteASCIIZ(i);
                                  FTagList[i].Length := FTagList[i].Length div 2;
                             end;
                             // else FTagList[i].ValOffset := FTagList[i].ValOffset and $FFFF;
                           end;
          TAGF_ASCII     : begin
                             if FIsTIFFTag or (FTagList[i].Length > 4)
                             then begin
                                  FStream.Position := FTagList[i].ValOffset + Offset;
                                  ConvByteASCIIZ(i);
                             end;
                           end;
          TAGF_RATIONAL,
          TAGF_SRATIONAL : begin
                             FStream.Position := FTagList[i].ValOffset + Offset;
                             ConvByteRational(i);
                           end;
          TAGF_UNDEFINED : begin
                             if (FTagList[i].Length > 4)
                             then begin
                                  FStream.Position := FTagList[i].ValOffset + Offset;
                                  ConvByteUndefined(i);
                             end;
                           end;
          end;
       end;

       // Check Shorts types if reversed read mode.
       if FStream.BigEndian
       then begin
            for i := 1 to FEC
            do begin
               // If a short type has length 1 (ValOffset = Value not offset),
               // then swap hi, lo word of ValOffset.
               if (FTagList[i].Length = 1)
               then case FTagList[i].FieldType of
                    TAGF_SHORT : begin
                                   lwRec(lw).Low  := HiWord(FTagList[i].ValOffset);
                                   lwRec(lw).High := LoWord(FTagList[i].ValOffset);
                                   FTagList[i].ValOffset := lw;
                                 end;
                    end;
            end;
       end;
  end;
end; // TmcmTAGMgr.ReadTags.


procedure TmcmTAGMgr.WriteCalcOffsets(var Offset : longword);
var i : integer;
{$IFDEF IMGDEBUG}
    ByteLength : cardinal;
    EndPos     : cardinal;
{$ENDIF}
begin
  // Adjust offset values for each TAG where data is stored "separatly" in file.
  for i := 1 to FEC
  do begin
     {$IFDEF IMGDEBUG}
       ByteLength := 0;
     {$ENDIF}
     case FTagList[i].FieldType of
     TAGF_ASCII     : begin
                        if FIsTIFFTag or (FTagList[i].Length > 4)
                        then begin
                             FTagList[i].ValOffset := Offset;
                             {$IFDEF IMGDEBUG}
                               ByteLength := FTagList[i].Length;
                             {$ENDIF}
                             inc(Offset, FTagList[i].Length);
                        end;
                      end;
     TAGF_SHORT     : begin
                        if (FTagList[i].Length > 2)
                        then begin
                             if (FTagList[i].Tag = TAG_COLORMAP)
                             then begin
                                  FTagList[i].ValOffset := Offset;
                                  {$IFDEF IMGDEBUG}
                                    ByteLength := FTagList[i].Length * SizeOf(word);
                                  {$ENDIF}
                                  inc(Offset, FTagList[i].Length * SizeOf(word));
                             end
                             else begin
                                  FTagList[i].ValOffset := Offset;
                                  {$IFDEF IMGDEBUG}
                                    ByteLength := FTagList[i].Length * SizeOf(word);
                                  {$ENDIF}
                                  inc(Offset, FTagList[i].Length * SizeOf(word));
                                  if Odd(FTagList[i].Length * SizeOf(word))
                                  then inc(Offset);
                             end;
                        end;
                      end;
     TAGF_LONG      : begin
                        if (FTagList[i].Length > 1)
                        then begin
                             FTagList[i].ValOffset := Offset;
                             {$IFDEF IMGDEBUG}
                               ByteLength := FTagList[i].Length * SizeOf(longword);
                             {$ENDIF}
                             inc(Offset, FTagList[i].Length * SizeOf(longword));
                             if Odd(FTagList[i].Length * SizeOf(longword))
                             then inc(Offset);
                        end;
                      end;
     TAGF_RATIONAL,
     TAGF_SRATIONAL : begin
                        FTagList[i].ValOffset := Offset;
                        {$IFDEF IMGDEBUG}
                          ByteLength := FTagList[i].Length * 2 * SizeOf(Longint);
                        {$ENDIF}
                        inc(Offset, FTagList[i].Length * 2 * SizeOf(Longint));
                      end;
     TAGF_UNDEFINED : begin
                        if FIsTIFFTag or (FTagList[i].Length > 4)
                        then begin
                             FTagList[i].ValOffset := Offset;
                             {$IFDEF IMGDEBUG}
                               ByteLength := FTagList[i].Length;
                             {$ENDIF}
                             inc(Offset, FTagList[i].Length);
                        end;
                      end;
     end;
     {$IFDEF IMGDEBUG}
       EndPos := 0;
       if (ByteLength > 0)
       then EndPos := ByteLength + FTagList[i].ValOffset;
       OutputDebugString(PChar('  Tag: '         + IntToStr(FTagList[i].Tag) +
                               '  FieldType: '   + IntToStr(FTagList[i].FieldType) +
                               '  Length: '      + IntToStr(FTagList[i].Length) +
                               '  ValOffset: '   + IntToStr(FTagList[i].ValOffset) +
                               '  ValPtr: '      + IntToStr(FTagList[i].ValPtr) +
                               '  FilePos: '     + IntToStr(FTagList[i].FilePos) +
                               '  Byte length: ' + IntToStr(ByteLength) +
                               '  End pos: '     + IntToStr(EndPos) ));
     {$ENDIF}
  end;
end; // TmcmTAGMgr.WriteCalcOffsets.


procedure TmcmTAGMgr.WriteTags;
var i : integer;
begin
  // Write number of TAG's for sub-image.
  FStream.Write(FEC, SizeOf(FEC));

  // Write Tag dictionary to file.
  for i := 1 to FEC
  do begin
      FTagList[i].FilePos := FStream.Position;
      FStream.Write(FTagList[i].Tag,       SizeOf(FTagList[i].Tag));
      FStream.Write(FTagList[i].FieldType, SizeOf(FTagList[i].FieldType));
      FStream.Write(FTagList[i].Length,    SizeOf(FTagList[i].Length));
      FStream.Write(FTagList[i].ValOffset, SizeOf(FTagList[i].ValOffset));
  end;

  FNextIFD := 0;
  FStream.Write(FNextIFD, SizeOf(FNextIFD)); // Set Next IFD to Non.
end; // TmcmTAGMgr.WriteTags.


procedure TmcmTAGMgr.WriteTagsData;
var h, i, j, k   : integer;
begin
  j := 0;
  k := 0;
  for i := 1 to FEC
  do begin
     case FTagList[i].FieldType of
     TAGF_ASCII     : begin
                        if FIsTIFFTag or (FTagList[i].Length > 4)
                        then begin
                             inc(j);
                             for h := 1 to FTagList[i].Length
                             do FStream.Write(AnsiChar(FValStr[j]^[h]), SizeOf(AnsiChar));
                        end;
                      end;
     TAGF_SHORT     : begin
                        if (FTagList[i].Length > 2)
                        then begin
                             if (FTagList[i].Tag = TAG_COLORMAP)
                             then begin
                                  // Write Palette to file.
                                  FOnWritePalette(Nil);
                             end
                             else begin
                                  inc(j);
                                  for h := 1 to (FTagList[i].Length * SizeOf(word))
                                  do FStream.Write(FValStr[j]^[h], SizeOf(byte));
                                  if Odd(FTagList[i].Length * SizeOf(word))
                                  then FStream.Write(FValStr[j]^[FTagList[i].Length+1], SizeOf(byte));
                             end;
                        end;
                      end;
     TAGF_LONG      : begin
                        if (FTagList[i].Length > 1)
                        then begin
                             case FTagList[i].Tag of
                             TAG_STRIPOFFSETS    : FOnWriteStripOffsets(Nil);
                             TAG_STRIPBYTECOUNTS : FOnWriteStripCounts(Nil);
                             end;
                        end;
                      end;
     TAGF_RATIONAL,
     TAGF_SRATIONAL : begin
                        for h := 1 to FTagList[i].Length
                        do begin
                           inc(k);
                           FStream.Write(FValRat[k]^.Num, SizeOf(FValRat[k]^.Num));
                           FStream.Write(FValRat[k]^.Denom, SizeOf(FValRat[k]^.Denom));
                        end;
                      end;
     TAGF_UNDEFINED : begin
                        case FTagList[i].Tag of
                        TAG_JPEGTABLES : FOnWriteJPEGTables(Nil);
                        else if (FTagList[i].Length > 4)
                             then begin
                                  inc(j);
                                  for h := 1 to (FTagList[i].Length * SizeOf(byte))
                                  do FStream.Write(FValStr[j]^[h], SizeOf(byte));
                                  if Odd(FTagList[i].Length * SizeOf(word))
                                  then FStream.Write(FValStr[j]^[FTagList[i].Length+1], SizeOf(byte));
                             end;
                        end;
                      end;
     end;
  end;
end; // TmcmTAGMgr.WriteTagsData.


procedure TmcmTAGMgr.SetTagInfo(TagNo     : word;
                                TagType   : word;
                                TagLength : longint;
                                TagVal    : longint;
                                TagRat    : array of longint;
                                TagStr    : string);
var i    : integer;
    ATag : TmcmTIFFTAG;
begin
  inc(FEC);
  ATag := CreateTAG;

  ATag.Tag         := TagNo;
  ATag.FieldType   := TagType;
  if (TagLength = 0)
  then ATag.Length := 1
  else ATag.Length := TagLength;
  ATag.ValPtr      := 0;
  case ATag.FieldType of
  TAGF_BYTE,
  TAGF_UNDEFINED : begin
                     if FIsTIFFTag or (TagLength > 4)
                     then begin
                          ATag.ValOffset := 0;
                          case ATag.Tag of
                          TAG_JPEGTABLES : ATag.ValOffset := TagVal;
                          else begin
                               inc(FNrOfStr);
                               new(FValStr[FNrOfStr]);
                               FValStr[FNrOfStr]^ := TagStr;
                               if Odd(SizeOf(FValStr[FNrOfStr]^))
                               then FValStr[FNrOfStr]^ := FValStr[FNrOfStr]^ + ' ';
                               ATag.Length := Length(FValStr[FNrOfStr]^);
                          end;
                          end;
                     end
                     else begin
                          if (TagStr <> '')
                          then ATag.ValOffset := Pinteger(TagStr)^
                          else ATag.ValOffset := TagVal;
                     end;
                   end;
  TAGF_SBYTE     : begin
                    //  ATag.ValOffset := TagVal;
                   end;
  TAGF_ASCII     : begin
                     if FIsTIFFTag or (TagLength > 4)
                     then begin
                          ATag.ValOffset := 0;
                          inc(FNrOfStr);
                          new(FValStr[FNrOfStr]);
                          FValStr[FNrOfStr]^ := TagStr;
                          if Odd(SizeOf(FValStr[FNrOfStr]^))
                          then FValStr[FNrOfStr]^ := FValStr[FNrOfStr]^ + ' ';
                          ATag.Length := Length(FValStr[FNrOfStr]^);
                     end
                     else ATag.ValOffset := Pinteger(TagStr)^;
                   end;
  TAGF_SHORT     : begin
                     if (ATag.Length > 2)
                     then begin
                          ATag.ValOffset := 0;
                          inc(FNrOfStr);
                          new(FValStr[FNrOfStr]);
                          FValStr[FNrOfStr]^ := TagStr;
                          if Odd(SizeOf(FValStr[FNrOfStr]^) * SizeOf(word))
                          then FValStr[FNrOfStr]^ := FValStr[FNrOfStr]^ + chr(0);
                     end
                     else ATag.ValOffset := longint(TagVal);
                   end;
  //TAGF_SSHORT    : FTagList[FEC]^.ValOffset := smallint(TagVal);
  TAGF_LONG,
  TAGF_SLONG     : ATag.ValOffset := TagVal;
  TAGF_RATIONAL,
  TAGF_SRATIONAL : begin
                     ATag.ValOffset := 0;
                     for i := 0 to (ATag.Length - 1)
                     do begin
                        inc(FNrOfRat);
                        new(FValRat[FNrOfRat]);
                        FValRat[FNrOfRat]^.Num   := TagRat[2*i];   // N
                        FValRat[FNrOfRat]^.Denom := TagRat[2*i+1]; // D
                     end;
                   end;
  end;
end; // TmcmTAGMgr.SetTagInfo.


procedure TmcmTAGMgr.TagsToImageInfo;
var i         : word;
    ResUnit   : word;
    xrNum     : longint;
    xrDenum   : longint;
    yrNum     : longint;
    yrDenum   : longint;
    xpNum     : longint;
    xpDenum   : longint;
    ypNum     : longint;
    ypDenum   : longint;
begin
  if (FError = EC_OK)
  then begin // TAG's were read
       if Assigned(FImageInfo)
       then begin

            ResUnit := LoWord(FTagList[GetTagIndex(TAG_RESOLUTIONUNIT)].ValOffset);
            if (ResUnit = 0) or (ResUnit = 1)
            then ResUnit := 2;

            // X resolution Num / Denum.
            i := GetTagIndex(TAG_XRESOLUTION);
            xrNum   := ItemVal[i,0].Num;
            xrDenum := ItemVal[i,0].Denom;

            // Y resolution Num / Denum.
            i := GetTagIndex(TAG_YRESOLUTION);
            yrNum   := ItemVal[i,0].Num;
            yrDenum := ItemVal[i,0].Denom;

            // X positon Num / Denum.
            i := GetTagIndex(TAG_XPOSITION);
            xpNum   := ItemVal[i,0].Num;
            xpDenum := ItemVal[i,0].Denom;

            // Y positon Num / Denum.
            i := GetTagIndex(TAG_YPOSITION);
            ypNum   := ItemVal[i,0].Num;
            ypDenum := ItemVal[i,0].Denom;

            // Set unit of measurement, resolution and position.
            case ResUnit of
            2  : begin // Resolution is given in Inchs.
                   if Assigned(FImageInfo)
                   then FImageInfo.Units := UN_INCHS;
                 end;
            3  : begin // Resolution is given in centimeters.
                   if Assigned(FImageInfo)
                   then FImageInfo.Units := UN_CENTIMETERS;
                 end;
            else begin // Resolution has no absolute measurement.
                   if Assigned(FImageInfo)
                   then FImageInfo.Units := UN_INCHS;
                   xrNum   := 0;
                   xrDenum := 1;
                   yrNum   := 0;
                   yrDenum := 1;
                   xpNum   := 0;
                   xpDenum := 1;
                   ypNum   := 0;
                   ypDenum := 1;
                 end;
            end;

            // Set resolution to ImageInfo and convert to Meters.
            if (xrDenum > 0)
            then FImageInfo.XResolution := xrNum / xrDenum
            else FImageInfo.XResolution := 0;
            if (yrDenum > 0)
            then FImageInfo.YResolution := yrNum / yrDenum
            else FImageInfo.YResolution := 0;
            if (xpDenum > 0)
            then FImageInfo.XPosition := xpNum / xpDenum
            else FImageInfo.XPosition := 0;
            if (ypDenum > 0)
            then FImageInfo.YPosition := ypNum / ypDenum
            else FImageInfo.YPosition := 0;
            FImageInfo.Units := UN_METERS;

            try
              FImageInfo.Orientation := 0;
              FImageInfo.Mirrored := False;
              i := GetTagIndex(TAG_ORIENTATION);
              if (i > 0)
              then begin
                   case LoWord(FTagList[GetTagIndex(TAG_ORIENTATION)].ValOffset) of
                   1 : FImageInfo.Orientation := 0;
                   2 : begin
                         FImageInfo.Orientation := 0; // Mirrowed
                         FImageInfo.Mirrored := True;
                       end;
                   3 : FImageInfo.Orientation := 180;
                   4 : begin
                         FImageInfo.Orientation := 180; // Mirrowed
                         FImageInfo.Mirrored := True;
                       end;
                   5 : begin
                         FImageInfo.Orientation := 90; // Mirrowed
                         FImageInfo.Mirrored := True;
                       end;
                   6 : FImageInfo.Orientation := 90;
                   7 : begin
                         FImageInfo.Orientation := 270; // Mirrowed
                         FImageInfo.Mirrored := True;
                       end;
                   8 : FImageInfo.Orientation := 270;
                   else FImageInfo.Orientation := 0;
                   end;
              end;
              i := GetTagIndex(TAG_DOCUMENTNAME);
              if (i > 0)
              then FImageInfo.DocumentName := String(ItemStr[i]);
              i := GetTagIndex(TAG_IMAGEDESCRIPTION);
              if (i > 0)
              then FImageInfo.Description := String(ItemStr[i]);
              i := GetTagIndex(TAG_MAKE);
              if (i > 0)
              then FImageInfo.Make := String(ItemStr[i]);
              i := GetTagIndex(TAG_MODEL);
              if (i > 0)
              then FImageInfo.Model := String(ItemStr[i]);
              i := GetTagIndex(TAG_PAGENAME);
              if (i > 0)
              then FImageInfo.PageName := String(ItemStr[i]);
              i := GetTagIndex(TAG_PAGENUMBER);
              if (i > 0)
              then FImageInfo.PageNumber := FTagList[i].ValOffset;
              i := GetTagIndex(TAG_SOFTWARE);
              if (i > 0)
              then FImageInfo.Software := String(ItemStr[i]);
              i := GetTagIndex(TAG_ARTIST);
              if (i > 0)
              then FImageInfo.Artist := String(ItemStr[i]);
              i := GetTagIndex(TAG_DATETIME);
              if (i > 0)
              then begin
                   // Format must be 'yyyy:mm:dd hh:mm:ss' or 'DATE yyyy:mm:dd TIME hh:mm:ss'
                   try
                     FImageInfo.DateTime := Convert2DateTime(String(ItemStr[i]));
                   except
                   end;
              end;
              i := GetTagIndex(TAG_HOSTCOMPUTER);
              if (i > 0)
              then FImageInfo.HostComputer := String(ItemStr[i]);
              i := GetTagIndex(TAG_COPYRIGHT);
              if (i > 0)
              then FImageInfo.Copyright := String(ItemStr[i]);
            except
            end;
       end;
  end;
end; // TmcmTAGMgr.TagsToImageInfo.


procedure TmcmTAGMgr.ImageInfoToTags(PageIndex : integer);
var TmpStr : string;
begin
  if Assigned(FImageInfo)
  then begin
       if FImageInfo.Mirrored
       then begin
            case FImageInfo.Orientation of
            0   : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 2, [0,0], '');
            90  : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 5, [0,0], '');
            180 : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 4, [0,0], '');
            270 : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 7, [0,0], '');
            end;
       end
       else begin
            case FImageInfo.Orientation of
            0   : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 1, [0,0], '');
            90  : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 6, [0,0], '');
            180 : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 3, [0,0], '');
            270 : SetTagInfo(TAG_ORIENTATION, TAGF_SHORT, 0, 8, [0,0], '');
            end;
       end;
       if (FImageInfo.DocumentName <> '')
       then SetTagInfo(TAG_DOCUMENTNAME, TAGF_ASCII, 0, 0, [0,0], FImageInfo.DocumentName);
       if (FImageInfo.Description <> '')
       then SetTagInfo(TAG_IMAGEDESCRIPTION, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Description);
       if (FImageInfo.Make <> '')
       then SetTagInfo(TAG_MAKE, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Make);
       if (FImageInfo.Model <> '')
       then SetTagInfo(TAG_MODEL, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Model);
       if (FImageInfo.PageName <> '')
       then SetTagInfo(TAG_PAGENAME, TAGF_ASCII, 0, 0, [0,0], FImageInfo.PageName);
       FImageInfo.PageNumber := PageIndex;
       if (FImageInfo.PageNumber >= 0)
       then SetTagInfo(TAG_PAGENUMBER, TAGF_SHORT, 2, FImageInfo.PageNumber, [0,0], '');
       if (FImageInfo.Software <> '')
       then SetTagInfo(TAG_SOFTWARE, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Software);
       if (FImageInfo.Artist <> '')
       then SetTagInfo(TAG_ARTIST, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Artist);
       try
         if (FImageInfo.DateTime > 0)
         then begin
              TmpStr := FormatDateTime('yyyy:mm:dd hh:mm:ss', FImageInfo.DateTime);
              TmpStr := TmpStr + Chr(0);
              if (TmpStr <> '')
              then SetTagInfo(TAG_DATETIME, TAGF_ASCII, 20, 0, [0,0], TmpStr);
         end;
       except
       end;
       if (FImageInfo.HostComputer <> '')
       then SetTagInfo(TAG_HOSTCOMPUTER, TAGF_ASCII, 0, 0, [0,0], FImageInfo.HostComputer);
       if (FImageInfo.Copyright <> '')
       then SetTagInfo(TAG_COPYRIGHT, TAGF_ASCII, 0, 0, [0,0], FImageInfo.Copyright);
  end;
end; // TmcmTAGMgr.ImageInfoToTags.


function TmcmTAGMgr.Convert2DateTime(DateStr : string) : TDateTime;
var IntStr    : string;
    TmpStr    : string;
    Year      : integer;
    Month     : integer;
    Day       : integer;
    Hour      : integer;
    Minute    : integer;
    Second    : integer;
begin
  Result := 0.0;
  try
    if (Pos(':', DateStr) <> 0) and
       (Pos('/', DateStr) =  0) and
       (Pos('-', DateStr) =  0)
    then begin
         if (Pos('DATE ', DateStr) <> 0)
         then begin
              TmpStr := Copy(DateStr, Pos('DATE ', DateStr) + 5, 100);
              DateStr := TmpStr;
         end;
         IntStr := Copy(DateStr, 1, Pos(':',DateStr)-1);
         Year   := StrToInt(IntStr);

         TmpStr := Copy(DateStr, Pos(':',DateStr)+1, 100);
         DateStr := TmpStr;
         IntStr := Copy(DateStr, 1, Pos(':',DateStr)-1);
         Month  := StrToInt(IntStr);

         TmpStr := Copy(DateStr, Pos(':',DateStr)+1, 100);
         DateStr := TmpStr;
         IntStr := Copy(DateStr, 1, Pos(' ',DateStr)-1);
         Day    := StrToInt(IntStr);

         try
           if (Year > 0) and (Month > 0) and (Day > 0)
           then Result := EncodeDate(Year, Month, Day);
           if (Year > Day) and (Length(DateStr) > 7)
           then begin
                if (Pos('TIME ', DateStr) <> 0)
                then begin
                     TmpStr := Copy(DateStr, Pos('TIME ', DateStr) + 5, 100);
                     DateStr := TmpStr;
                end;

                TmpStr := Copy(DateStr, Pos(' ',DateStr)+1, 100);
                DateStr := TmpStr;
                IntStr := Copy(DateStr, 1, Pos(':',DateStr)-1);
                Hour   := StrToInt(IntStr);

                TmpStr := Copy(DateStr, Pos(':',DateStr)+1, 100);
                DateStr := TmpStr;
                IntStr := Copy(DateStr, 1, Pos(':',DateStr)-1);
                Minute := StrToInt(IntStr);

                TmpStr := Copy(DateStr, Pos(':',DateStr)+1, 100);
                DateStr := TmpStr;
                IntStr := Copy(DateStr, 1, 2);
                Second := StrToInt(IntStr);
                try
                  Result := Result + EncodeTime(Hour, Minute, Second, 0);
                except
                // If the Date and Time isn't formated correctly an exception will occur.
                // But we don't care - in this case we will used the file's dat & time.
                end;
           end;
         except
         // If the Date and Time isn't formated correctly an exception will occur.
         // But we don't care - in this case we will used the file's dat & time.
         end;
    end;
  except
    // Failed to convert date and time.
  end;
end; // TmcmTAGMgr.Convert2DateTime.


//------------------------------------------------------------------------------
// TmcmExifImage
//------------------------------------------------------------------------------

Type
  TExifInfo = class(TmcmImageExifInfo); // Declared to access protected data directly on the class.
  TGPSInfo = class(TmcmImageGPSInfo);   // Do.
  TImageInfo = class(TmcmImageInfo);    // Do.

function TmcmExifImage.ConvertExifStr2Int(Data : string) : integer;
var i      : integer;
    TmpStr : string;
begin
  Result := 0;
  try
    if (Data <> '')
    then begin
         i := Pos(' ', Data);
         if (i > 0)
         then TmpStr := Copy(Data, 1, i)
         else TmpStr := Data;
         Result := StrToInt(TmpStr);
    end;
  except
  end;
end; // TmcmExifImage.ConvertExifStr2Int.


procedure TmcmExifImage.ReadExifAttribute(IFDOffset, Offset : longword);
var i, j   : word;
    VerStr : string;
begin
  if Assigned(FImageInfo)
  then begin
       FStream.Position := IFDOffset + Offset;
       FIsTIFFTag := False;
       i := FEC;
       ReadTags(Offset);
       if (FEC <> i) and (FError = EC_OK)
       then begin // TAG's were read
            TImageInfo(FImageInfo).FExif := TmcmImageExifInfo.Create;
            if Assigned(FImageInfo.Exif)
            then begin
                 with TExifInfo(FImageInfo.Exif)
                 do begin
                    try
                      i := GetTagIndex(TAG_EXIF_VERSION);
                      if (i > 0)
                      then begin
                           VerStr := String(ItemStr[i]);
                           if (VerStr <> '')
                           then FExifVersion := StrToInt(VerStr);
                      end;
                    except
                    end;
                    try
                      i := GetTagIndex(TAG_EXIF_FLASHPIXVERSION);
                      if (i > 0)
                      then begin
                           VerStr := String(ItemStr[i]);
                           if (VerStr <> '') and (Ord(VerStr[1]) > 30)
                           then FFlashPixVersion := StrToInt(VerStr);
                      end;
                    except
                    end;

                    // Color space info.
                    i := GetTagIndex(TAG_EXIF_COLORSPACEINFO);
                    if (i > 0)
                    then FColorSpace := FTagList[i].ValOffset;

                    // Image configuration.
                    i := GetTagIndex(TAG_EXIF_PIXELXDIMENSION);
                    if (i > 0)
                    then FXDimension := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_PIXELYDIMENSION);
                    if (i > 0)
                    then FYDimension := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_COMPONENTCONFIGURATION);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length = 4)
                         then begin
                              if FStream.BigEndian
                              then FTagList[i].ValOffset := SwapLong(FTagList[i].ValOffset);
                              for j := 0 to 3
                              do begin
                                 if (PVectorB(@FTagList[i].ValOffset)[j] >= $30)
                                 then FComponentConfig[3-j] := PVectorB(@FTagList[i].ValOffset)[j] - $30
                                 else FComponentConfig[j] := PVectorB(@FTagList[i].ValOffset)[j];
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_COMPRESSEDBITSPERPIXEL);
                    if (i > 0)
                    then begin
                         FCompressdBitPerPixel[0] := ItemVal[i,0].Num;
                         FCompressdBitPerPixel[1] := ItemVal[i,0].Denom;
                    end;

                    // User information.
                    i := GetTagIndex(TAG_EXIF_MAKERNOTE);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              SetLength(FMakerNote, FTagList[i].Length);
                              CopyMemory(@FMakerNote[1], @FValStr[FTagList[i].ValPtr]^[1], FTagList[i].Length);
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_USERCOMMENT);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              SetLength(FUserComment, FTagList[i].Length);
                              CopyMemory(@FUserComment[1], @FValStr[FTagList[i].ValPtr]^[1], FTagList[i].Length);
                         end;
                    end;

                    // Date & Time
                    i := GetTagIndex(TAG_EXIF_DATETIMEORIGINAL);
                    if (i > 0)
                    then begin
                         try
                           FDateTimeOriginal := Convert2DateTime(String(ItemStr[i]));
                         except
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_DATETIMEDIGITIZED);
                    if (i > 0)
                    then begin
                         try
                           FDateTimeDigitized := Convert2DateTime(String(ItemStr[i]));
                         except
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_SUBSECTIME);
                    if (i > 0)
                    then FSubSecTime := ConvertExifStr2Int(String(ItemStr[i]));

                    i := GetTagIndex(TAG_EXIF_SUBSECTIMEORIGINAL);
                    if (i > 0)
                    then FSubSecTimeOriginal := ConvertExifStr2Int(String(ItemStr[i]));

                    i := GetTagIndex(TAG_EXIF_SUBSECTIMEDIGITIZED);
                    if (i > 0)
                    then FSubSecTimeDigitized := ConvertExifStr2Int(String(ItemStr[i]));

                    // Other information.
                    i := GetTagIndex(TAG_EXIF_IMAGEUNIQUEID);
                    if (i > 0)
                    then FUniqueID := String(ItemStr[i]);

                    // Picture taking conditions
                    i := GetTagIndex(TAG_EXIF_ExposureTime);
                    if (i > 0)
                    then begin
                         FExposureTime[0] := ItemVal[i,0].Num;
                         FExposureTime[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_FNumber);
                    if (i > 0)
                    then begin
                         FFNumber[0] := ItemVal[i,0].Num;
                         FFNumber[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_ExposureProgram);
                    if (i > 0)
                    then FExposureProgram := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_SpectralSensitivity);
                    if (i > 0)
                    then FSpectralSensitivity := String(ItemStr[i]);

                    i := GetTagIndex(TAG_EXIF_ISOSpeedRatings);
                    if (i > 0)
                    then if (FTagList[i].Length = 1)
                         then FISOSpeedRating := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_ECF);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              FOECF_Size := FTagList[i].Length;
                              try
                                GetMem(FOECF, FOECF_Size);
                                CopyMemory(FOECF, @FValStr[FTagList[i].ValPtr]^[1], FOECF_Size);
                              except
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_ShutterSpeedValue);
                    if (i > 0)
                    then begin
                         FShutterSpeed[0] := ItemVal[i,0].Num;
                         FShutterSpeed[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_ApertureValue);
                    if (i > 0)
                    then begin
                         FAperture[0] := ItemVal[i,0].Num;
                         FAperture[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_BrightnessValue);
                    if (i > 0)
                    then begin
                         FBrightness[0] := ItemVal[i,0].Num;
                         FBrightness[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_ExposureBiasValue);
                    if (i > 0)
                    then begin
                         FExposureBias[0] := ItemVal[i,0].Num;
                         FExposureBias[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_MaxApertureValue);
                    if (i > 0)
                    then begin
                         FMaxAperture[0] := ItemVal[i,0].Num;
                         FMaxAperture[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_SubjectDistance);
                    if (i > 0)
                    then begin
                         FSubjectDistance[0] := ItemVal[i,0].Num;
                         FSubjectDistance[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_MeteringMode);
                    if (i > 0)
                    then FMeteringMode := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_LightSource);
                    if (i > 0)
                    then FLightSource := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_Flash);
                    if (i > 0)
                    then FFlash := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_FocalLength);
                    if (i > 0)
                    then begin
                         FFocalLength[0] := ItemVal[i,0].Num;
                         FFocalLength[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_SubjectArea);
                    if (i > 0)
                    then begin
                         FSubjectAreaLength := FTagList[i].Length;
                         // Length = 2 -> x,y
                         // Length = 3 -> x,y,d
                         // Length = 4 -> x,y,dx,dy
                         if (FTagList[i].Length > 2)
                         then begin
                              if FStream.BigEndian
                              then begin
                                   for j := 0 to (FTagList[i].Length - 1)
                                   do FSubjectArea[j] := SwapWord(PVectorW(@FValStr[FTagList[i].ValPtr]^[1])^[j]);
                              end
                              else begin
                                   for j := 0 to (FTagList[i].Length - 1)
                                   do FSubjectArea[j] := PVectorW(@FValStr[FTagList[i].ValPtr]^[1])^[j];
                              end;
                         end
                         else begin
                              if FStream.BigEndian
                              then begin
                                   FSubjectArea[0] := LoWord(FTagList[i].ValOffset);
                                   FSubjectArea[1] := HiWord(FTagList[i].ValOffset);
                              end
                              else begin
                                   FSubjectArea[0] := HiWord(FTagList[i].ValOffset);
                                   FSubjectArea[1] := LoWord(FTagList[i].ValOffset);
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_FlashEnergy);
                    if (i > 0)
                    then begin
                         FFlashEnergy[0] := ItemVal[i,0].Num;
                         FFlashEnergy[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_SpatialFrequencyResponse);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              FSFR_Size := FTagList[i].Length;
                              try
                                GetMem(FFrequencyResponse, FSFR_Size);
                                CopyMemory(FFrequencyResponse, @FValStr[FTagList[i].ValPtr]^[1], FSFR_Size);
                              except
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_FocalPlaneXResolution);
                    if (i > 0)
                    then begin
                         FFocalPlaneXResolution[0] := ItemVal[i,0].Num;
                         FFocalPlaneXResolution[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_FocalPlaneYResolution);
                    if (i > 0)
                    then begin
                         FFocalPlaneYResolution[0] := ItemVal[i,0].Num;
                         FFocalPlaneYResolution[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_FocalPlaneResolutionUnit);
                    if (i > 0)
                    then FFocalPlaneResolutionUnit := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_SubjectLocation);
                    if (i > 0)
                    then begin
                         if FStream.BigEndian
                         then begin
                              FSubjectLocation[0] := HiWord(FTagList[i].ValOffset); //
                              FSubjectLocation[1] := LoWord(FTagList[i].ValOffset); //
                         end
                         else begin
                              FSubjectLocation[0] := HiWord(FTagList[i].ValOffset); //
                              FSubjectLocation[1] := LoWord(FTagList[i].ValOffset); //
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_ExposureIndex);
                    if (i > 0)
                    then begin
                         FExposureIndex[0] := ItemVal[i,0].Num;
                         FExposureIndex[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_SensingMethod);
                    if (i > 0)
                    then FSensingMethod := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_FileSource);
                    if (i > 0)
                    then FFileSource := byte(FTagList[i].ValOffset);

                    i := GetTagIndex(TAG_EXIF_SceneType);
                    if (i > 0)
                    then FSceneType := Byte(FTagList[i].ValOffset);

                    i := GetTagIndex(TAG_EXIF_CFAPattern);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              FCFA_Size := FTagList[i].Length;
                              try
                                GetMem(FCFAPattern, FCFA_Size);
                                CopyMemory(FCFAPattern, @FValStr[FTagList[i].ValPtr]^[1], FCFA_Size);
                              except
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_CustomRendered);
                    if (i > 0)
                    then FCustomRendered := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_ExposureMode);
                    if (i > 0)
                    then FExposureMode := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_WhiteBalance);
                    if (i > 0)
                    then FWhiteBalance := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_DigitalZoomRatio);
                    if (i > 0)
                    then begin
                         FDigitalZoomRatio[0] := ItemVal[i,0].Num;
                         FDigitalZoomRatio[1] := ItemVal[i,0].Denom;
                    end;

                    i := GetTagIndex(TAG_EXIF_FocalLengthIn35mmFilm);
                    if (i > 0)
                    then FFocalLengthIn35mmFilm := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_SceneCaptureType);
                    if (i > 0)
                    then FSceneCaptureType := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_GainControl);
                    if (i > 0)
                    then FGainControl := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_Contrast);
                    if (i > 0)
                    then FContrast := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_Saturation);
                    if (i > 0)
                    then FSaturation := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_Sharpness);
                    if (i > 0)
                    then FSharpness := FTagList[i].ValOffset;

                    i := GetTagIndex(TAG_EXIF_DeviceSettingDescription);
                    if (i > 0)
                    then begin
                         if (FTagList[i].Length > 1)
                         then begin
                              FDSD_Size := FTagList[i].Length;
                              try
                                GetMem(FDeviceSettingDescription, FDSD_Size);
                                CopyMemory(FDeviceSettingDescription, @FValStr[FTagList[i].ValPtr]^[1], FDSD_Size);
                              except
                              end;
                         end;
                    end;

                    i := GetTagIndex(TAG_EXIF_SubjectDistanceRange);
                    if (i > 0)
                    then FSubjectDistanceRange := FTagList[i].ValOffset;

                    if (FError = EC_TIFFTAG)
                    then FError := EC_OK;
                 end;
            end;
       end;
  end;
end; // TmcmExifImage.ReadExifAttribute.


procedure TmcmExifImage.ReadExifGPS(IFDOffset, Offset : longword);
var i, j : word;
    UnStr : string;
begin
  if Assigned(FImageInfo)
  then begin
       FStream.Position := IFDOffset + Offset;
       i := FEC;
       ReadTags(Offset);
       if (FEC <> i) and (FError = EC_OK)
       then begin // TAG's were read
            TImageInfo(FImageInfo).FGPS := TmcmImageGPSInfo.Create;
            if Assigned(FImageInfo.GPS)
            then begin
                 with TGPSInfo(FImageInfo.GPS)
                 do begin
                    try
                      i := GetTagIndex(EXIF_GPSVersionID);
                      if (i > 0)
                      then begin
                           if FStream.BigEndian
                           then FTagList[i].ValOffset := SwapLong(FTagList[i].ValOffset);
                           SetLength(UnStr, 4);
                           if (PVectorB(@FTagList[i].ValOffset)[0] < $30)
                           then for j := 0 to 3
                                do inc(PVectorB(@FTagList[i].ValOffset)[j], $30);
                           CopyMemory(@UnStr[1], PAnsiChar(@FTagList[i].ValOffset), 4);
                           try
                             FVersionID := StrToInt(UnStr);
                           except
                             FVersionID := 0;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSLatitudeRef);
                      if (i > 0)
                      then FLatitudeRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSLatitude);
                      if (i > 0)
                      then begin
                           for j := 0 to 2
                           do begin
                              FLatitude[2*j+0] := ItemVal[i,j].Num;
                              FLatitude[2*j+1] := ItemVal[i,j].Denom;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSLongitudeRef);
                      if (i > 0)
                      then FLongitudeRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSLongitude);
                      if (i > 0)
                      then begin
                           for j := 0 to 2
                           do begin
                              FLongitude[2*j+0] := ItemVal[i,j].Num;
                              FLongitude[2*j+1] := ItemVal[i,j].Denom;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSAltitudeRef);
                      if (i > 0)
                      then FAltitudeRef := FTagList[i].ValOffset;

                      i := GetTagIndex(EXIF_GPSAltitude);
                      if (i > 0)
                      then begin
                           FAltitude[0] := ItemVal[i,0].Num;
                           FAltitude[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSTimeStamp);
                      if (i > 0)
                      then begin
                           for j := 0 to 2
                           do begin
                              FTimeStamp[2*j+0] := ItemVal[i,j].Num;
                              FTimeStamp[2*j+1] := ItemVal[i,j].Denom;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSSatellites);
                      if (i > 0)
                      then FSatellites := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSStatus);
                      if (i > 0)
                      then FStatus := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSMeasureMode);
                      if (i > 0)
                      then FMeasureMode := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDOP);
                      if (i > 0)
                      then begin
                           FDOP[0] := ItemVal[i,0].Num;
                           FDOP[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSSpeedRef);
                      if (i > 0)
                      then FSpeedRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSSpeed);
                      if (i > 0)
                      then begin
                           FSpeed[0] := ItemVal[i,0].Num;
                           FSpeed[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSTrackRef);
                      if (i > 0)
                      then FTrackRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSTrack);
                      if (i > 0)
                      then begin
                           FTrack[0] := ItemVal[i,0].Num;
                           FTrack[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSImgDirectionRef);
                      if (i > 0)
                      then FImgDirectionRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSImgDirection);
                      if (i > 0)
                      then begin
                           FImgDirection[0] := ItemVal[i,0].Num;
                           FImgDirection[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSMapDatum);
                      if (i > 0)
                      then FMapDatum := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDestLatitudeRef);
                      if (i > 0)
                      then FDestLatitudeRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDestLatitude);
                      if (i > 0)
                      then begin
                           for j := 0 to 2
                           do begin
                              FDestLatitude[2*j+0] := ItemVal[i,j].Num;
                              FDestLatitude[2*j+1] := ItemVal[i,j].Denom;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSDestLongitudeRef);
                      if (i > 0)
                      then FDestLongitudeRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDestLongitude);
                      if (i > 0)
                      then begin
                           for j := 0 to 2
                           do begin
                              FDestLongitude[2*j+0] := ItemVal[i,j].Num;
                              FDestLongitude[2*j+1] := ItemVal[i,j].Denom;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSDestBearingRef);
                      if (i > 0)
                      then FDestBearingRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDestBearing);
                      if (i > 0)
                      then begin
                           FDestBearing[0] := ItemVal[i,0].Num;
                           FDestBearing[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSDestDistanceRef);
                      if (i > 0)
                      then FDestDistanceRef := String(ItemStr[i]);

                      i := GetTagIndex(EXIF_GPSDestDistance);
                      if (i > 0)
                      then begin
                           FDestDistance[0] := ItemVal[i,0].Num;
                           FDestDistance[1] := ItemVal[i,0].Denom;
                      end;

                      i := GetTagIndex(EXIF_GPSProcessingMethod);
                      if (i > 0)
                      then begin
                           if (FTagList[i].Length > 1)
                           then begin
                                FProcessingSize := FTagList[i].Length;
                                try
                                  GetMem(FProcessingMethod, FProcessingSize);
                                  CopyMemory(FProcessingMethod, @FValStr[FTagList[i].ValPtr]^[1], FProcessingSize);
                                except
                                end;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSAreaInformation);
                      if (i > 0)
                      then begin
                           if (FTagList[i].Length > 1)
                           then begin
                                FAreaSize := FTagList[i].Length;
                                try
                                  GetMem(FAreaInformation, FAreaSize);
                                  CopyMemory(FAreaInformation, @FValStr[FTagList[i].ValPtr]^[1], FAreaSize);
                                except
                                end;
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSDateStamp);
                      if (i > 0)
                      then begin
                           try
                             FDateStamp := Convert2DateTime(String(ItemStr[i]));
                           except
                           end;
                      end;

                      i := GetTagIndex(EXIF_GPSDifferential);
                      if (i > 0)
                      then FDifferential := FTagList[i].ValOffset;
                    except
                    end;
                    if (FError = EC_TIFFTAG)
                    then FError := EC_OK;
                 end;
            end;
       end;
  end;
end; // TmcmExifImage.ReadExifGPS;


procedure TmcmExifImage.ReadExifInteroperability(IFDOffset, Offset : longword);
var i : word;
begin
  if Assigned(FImageInfo)
  then begin
       if Assigned(FImageInfo.Exif)
       then begin
            FStream.Position := IFDOffset + Offset;
            i := FEC;
            ReadTags(Offset);
            if (FEC <> i) and (FError = EC_OK)
            then begin // TAG's were read
                 with TExifInfo(FImageInfo.Exif)
                 do begin
                    try
                      i := GetTagIndex(1);
                      if (i > 0)
                      then FInteroperabilityIndex := String(ItemStr[i]);

                      i := GetTagIndex(2);
                      if (i > 0)
                      then begin
                           if (ItemStr[i] <> '')
                           then FInteroperabilityVersion := StrToInt(String(ItemStr[i]));
                      end;
                    except
                    end;
                    if (FError = EC_TIFFTAG)
                    then FError := EC_OK;
                 end;
            end;
       end;
  end;
end; // TmcmExifImage.ReadExifInteroperability;


procedure TmcmExifImage.SetExifAttribute(RGBMode : boolean);
var i, j     : word;
    DataStr  : string;
begin
  if Assigned(FImageInfo)
  then begin
       if Assigned(FImageInfo.Exif)
       then begin
            with TExifInfo(FImageInfo.Exif)
            do begin
               SetTagInfo(TAG_EXIF_VERSION, TAGF_UNDEFINED, 4, 0, [0,0], '0220');
               if (FFlashPixVersion <> 0)
               then begin
                    DataStr := IntToStr(FFlashPixVersion);
                    while (Length(DataStr) < 4)
                    do DataStr := '0' + DataStr;
                    SetTagInfo(TAG_EXIF_FLASHPIXVERSION, TAGF_UNDEFINED, 4, 0, [0,0], DataStr);
               end;

               if (FColorSpace = 1)
               then SetTagInfo(TAG_EXIF_COLORSPACEINFO, TAGF_SHORT, 1, FColorSpace, [0,0], '');

               if True // (FComponentConfig[0] <> 0)
               then begin
                    SetLength(DataStr, 4);
                    if RGBMode
                    then begin
                         DataStr[1] := #4;
                         DataStr[2] := #5;
                         DataStr[3] := #6;
                    end
                    else begin
                         DataStr[1] := #1;
                         DataStr[2] := #2;
                         DataStr[3] := #3;
                    end;
                    DataStr[4] := #0;
                    //for j := 0 to 3
                    //do DataStr[j+1] := Char(FComponentConfig[j]);
                    SetTagInfo(TAG_EXIF_COMPONENTCONFIGURATION, TAGF_UNDEFINED, 4, 0, [0,0], DataStr);
               end;

               if (FCompressdBitPerPixel[0] <> 0)
               then SetTagInfo(TAG_EXIF_COMPRESSEDBITSPERPIXEL, TAGF_RATIONAL, 1, 0, FCompressdBitPerPixel, '');

               if (FXDimension <> 0)
               then SetTagInfo(TAG_EXIF_PIXELXDIMENSION, TAGF_LONG, 1, FXDimension, [0,0], '');

               if (FYDimension <> 0)
               then SetTagInfo(TAG_EXIF_PIXELYDIMENSION, TAGF_LONG, 1, FYDimension, [0,0], '');

               if (Length(FMakerNote) > 0)
               then SetTagInfo(TAG_EXIF_MAKERNOTE, TAGF_UNDEFINED, Length(FMakerNote), 0, [0,0], FMakerNote);


               if (Length(FUserComment) > 0)
               then SetTagInfo(TAG_EXIF_USERCOMMENT, TAGF_UNDEFINED, Length(FUserComment), 0, [0,0], FUserComment);

               //if ( <> 0) Not used!
               //then SetTagInfo(TAG_EXIF_RELATEDSOUNDFILE, TAGF_ASCII, 13, 0, 0, 0, '');

               if (FDateTimeOriginal <> 0)
               then begin
                    DataStr := FormatDateTime('yyyy:mm:dd hh:mm:ss', FDateTimeOriginal) + #0;
                    if (DataStr <> '')
                    then SetTagInfo(TAG_EXIF_DATETIMEORIGINAL, TAGF_ASCII, 20, 0, [0,0], DataStr);
               end;

               if (FDateTimeDigitized <> 0)
               then begin
                    DataStr := FormatDateTime('yyyy:mm:dd hh:mm:ss', FDateTimeDigitized) + #0;
                    if (DataStr <> '')
                    then SetTagInfo(TAG_EXIF_DATETIMEDIGITIZED, TAGF_ASCII, 20, 0, [0,0], DataStr);
               end;

               if (FSubSecTime > 0)
               then begin
                    DataStr := IntToStr(FSubSecTime);
                    SetTagInfo(TAG_EXIF_SUBSECTIME, TAGF_ASCII, Length(DataStr), 0, [0,0], DataStr);
               end;

               if (FSubSecTimeOriginal > 0)
               then begin
                    DataStr := IntToStr(FSubSecTimeOriginal);
                    SetTagInfo(TAG_EXIF_SUBSECTIMEORIGINAL, TAGF_ASCII, Length(DataStr), 0, [0,0], DataStr);
               end;

               if (FSubSecTimeDigitized > 0)
               then begin
                    DataStr := IntToStr(FSubSecTimeDigitized);
                    SetTagInfo(TAG_EXIF_SUBSECTIMEDIGITIZED, TAGF_ASCII, Length(DataStr), 0, [0,0], DataStr);
               end;

               if (FUniqueID <> '')
               then begin
                    i := Length(FUniqueID);
                    if (i <> 33)
                    then begin
                         SetLength(FUniqueID, 33);
                         for j := i + 1 to 33
                         do FUniqueID[j] := #0;
                    end;
                    SetTagInfo(TAG_EXIF_IMAGEUNIQUEID, TAGF_ASCII, 33, 0, [0,0], FUniqueID);
               end;

               if (FExposureTime[1] <> 0)
               then SetTagInfo(TAG_EXIF_ExposureTime, TAGF_RATIONAL, 1, 0, FExposureTime, '');

               if (FFNumber[1] <> 0)
               then SetTagInfo(TAG_EXIF_FNumber, TAGF_RATIONAL, 1, 0, FFNumber, '');

               if (FExposureProgram > 0)
               then SetTagInfo(TAG_EXIF_ExposureProgram, TAGF_SHORT, 1, FExposureProgram, [0,0], '');

               if (FSpectralSensitivity <> '')
               then SetTagInfo(TAG_EXIF_SpectralSensitivity, TAGF_ASCII, Length(FSpectralSensitivity), 0, [0,0], FSpectralSensitivity);

               if (FISOSpeedRating <> 0)
               then SetTagInfo(TAG_EXIF_ISOSpeedRatings, TAGF_SHORT, 1, FISOSpeedRating, [0,0], '');

               if (FOECF_Size > 0)
               then begin
                    SetLength(DataStr, FOECF_Size);
                    CopyMemory(@DataStr[1], FOECF, FOECF_Size);
                    SetTagInfo(TAG_EXIF_ECF, TAGF_UNDEFINED, FOECF_Size, 0, [0,0], DataStr);
               end;

               if (FShutterSpeed[1] <> 0)
               then SetTagInfo(TAG_EXIF_ShutterSpeedValue, TAGF_SRATIONAL, 1, 0, FShutterSpeed, '');

               if (FAperture[1] <> 0)
               then SetTagInfo(TAG_EXIF_ApertureValue, TAGF_RATIONAL, 1, 0, FAperture, '');

               if (FBrightness[1] <> 0)
               then SetTagInfo(TAG_EXIF_BrightnessValue, TAGF_SRATIONAL, 1, 0, FBrightness, '');

               if (FExposureBias[1] <> 0)
               then SetTagInfo(TAG_EXIF_ExposureBiasValue, TAGF_SRATIONAL, 1, 0, FExposureBias, '');

               if (FMaxAperture[1] <> 0)
               then SetTagInfo(TAG_EXIF_MaxApertureValue, TAGF_RATIONAL, 1, 0, FMaxAperture, '');

               if (FSubjectDistance[0] <> 0) and (FSubjectDistance[1] <> -1)
               then SetTagInfo(TAG_EXIF_SubjectDistance, TAGF_RATIONAL, 1, 0, FSubjectDistance, '');

               if (FMeteringMode > 0)
               then SetTagInfo(TAG_EXIF_MeteringMode, TAGF_SHORT, 1, FMeteringMode, [0,0], '');

               if (FLightSource > 0)
               then SetTagInfo(TAG_EXIF_LightSource, TAGF_SHORT, 1, FLightSource, [0,0], '');

               if (FFlash <> $FFFF)
               then SetTagInfo(TAG_EXIF_Flash, TAGF_SHORT, 1, FFlash, [0,0], '');

               if (FFocalLength[1] <> 0)
               then SetTagInfo(TAG_EXIF_FocalLength, TAGF_RATIONAL, 1, 0, FFocalLength, '');

               if (FSubjectAreaLength > 0)
               then begin
                    if (FSubjectAreaLength = 2)
                    then SetTagInfo(TAG_EXIF_SubjectArea, TAGF_SHORT, FSubjectAreaLength, (FSubjectArea[0] shl 16) + FSubjectArea[1], [0,0], '')
                    else begin
                         SetLength(DataStr, FSubjectAreaLength * 2);
                         for j := 0 to (FSubjectAreaLength - 1)
                         do PVectorW(@DataStr[1])^[j] := FSubjectArea[j];
                         SetTagInfo(TAG_EXIF_SubjectArea, TAGF_SHORT, FSubjectAreaLength, 0, [0,0], DataStr);
                    end;
               end;

               if (FFlashEnergy[1] <> 0)
               then SetTagInfo(TAG_EXIF_FlashEnergy, TAGF_RATIONAL, 1, 0, FFlashEnergy, '');

               if (FSFR_Size > 0)
               then begin
                    SetLength(DataStr, FSFR_Size);
                    CopyMemory(@DataStr[1], FFrequencyResponse, FSFR_Size);
                    SetTagInfo(TAG_EXIF_SpatialFrequencyResponse, TAGF_UNDEFINED, FSFR_Size, 0, [0,0], DataStr);
               end;

               if (FFocalPlaneXResolution[1] <> 0)
               then SetTagInfo(TAG_EXIF_FocalPlaneXResolution, TAGF_RATIONAL, 1, 0, FFocalPlaneXResolution, '');

               if (FFocalPlaneYResolution[1] <> 0)
               then SetTagInfo(TAG_EXIF_FocalPlaneYResolution, TAGF_RATIONAL, 1, 0, FFocalPlaneYResolution, '');

               if (FFocalPlaneResolutionUnit <> 0)
               then SetTagInfo(TAG_EXIF_FocalPlaneResolutionUnit, TAGF_SHORT, 1, FFocalPlaneResolutionUnit, [0,0], '');

               if (FSubjectLocation[0] <> 0) or (FSubjectLocation[1] <> 0)
               then SetTagInfo(TAG_EXIF_SubjectLocation, TAGF_SHORT, 2, (FSubjectLocation[0] shl 16) + FSubjectLocation[1], [0,0], '');

               if (FExposureIndex[1] <> 0)
               then SetTagInfo(TAG_EXIF_ExposureIndex, TAGF_RATIONAL, 1, 0, FExposureIndex, '');

               if (FSensingMethod > 0)
               then SetTagInfo(TAG_EXIF_SensingMethod, TAGF_SHORT, 1, FSensingMethod, [0,0], '');

               if (FFileSource <> 0)
               then SetTagInfo(TAG_EXIF_FileSource, TAGF_UNDEFINED, 1, FFileSource, [0,0], '');

               if (FSceneType <> 0)
               then SetTagInfo(TAG_EXIF_SceneType, TAGF_UNDEFINED, 1, FSceneType, [0,0], '');

               if (FCFA_Size > 0)
               then begin
                    SetLength(DataStr, FCFA_Size);
                    CopyMemory(@DataStr[1], FCFAPattern, FCFA_Size);
                    SetTagInfo(TAG_EXIF_CFAPattern, TAGF_UNDEFINED, FCFA_Size, 0, [0,0], DataStr);
               end;

               if (FCustomRendered > 0)
               then SetTagInfo(TAG_EXIF_CustomRendered, TAGF_SHORT, 1, FCustomRendered, [0,0], '');

               if (FExposureMode <> $FFFF)
               then SetTagInfo(TAG_EXIF_ExposureMode, TAGF_SHORT, 1, FExposureMode, [0,0], '');

               if (FWhiteBalance <> $FFFF)
               then SetTagInfo(TAG_EXIF_WhiteBalance, TAGF_SHORT, 1, FWhiteBalance, [0,0], '');

               if (FDigitalZoomRatio[1] <> 0)
               then SetTagInfo(TAG_EXIF_DigitalZoomRatio, TAGF_RATIONAL, 1, 0, FDigitalZoomRatio, '');

               if (FFocalLengthIn35mmFilm > 0)
               then SetTagInfo(TAG_EXIF_FocalLengthIn35mmFilm, TAGF_SHORT, 1, FFocalLengthIn35mmFilm, [0,0], '');

               if (FSceneCaptureType <> $FFFF)
               then SetTagInfo(TAG_EXIF_SceneCaptureType, TAGF_SHORT, 1, FSceneCaptureType, [0,0], '');

               if (FGainControl > 0)
               then SetTagInfo(TAG_EXIF_GainControl, TAGF_RATIONAL, 1, FGainControl, [0,0], '');

               if (FContrast > 0)
               then SetTagInfo(TAG_EXIF_Contrast, TAGF_SHORT, 1, FContrast, [0,0], '');

               if (FSaturation > 0)
               then SetTagInfo(TAG_EXIF_Saturation, TAGF_SHORT, 1, FSaturation, [0,0], '');

               if (FSharpness > 0)
               then SetTagInfo(TAG_EXIF_Sharpness, TAGF_SHORT, 1, FSharpness, [0,0], '');

               if (FDSD_Size > 0)
               then begin
                    SetLength(DataStr, FDSD_Size);
                    CopyMemory(@DataStr[1], FDeviceSettingDescription, FDSD_Size);
                    SetTagInfo(TAG_EXIF_DeviceSettingDescription, TAGF_UNDEFINED, FDSD_Size, 0, [0,0], DataStr);
               end;

               if (FSubjectDistanceRange <> $FFFF)
               then SetTagInfo(TAG_EXIF_SubjectDistanceRange, TAGF_SHORT, 1, FSharpness, [0,0], '');
            end;
       end;
  end;
end; // TmcmExifImage.SetExifAttribute.


procedure TmcmExifImage.SetExifGPS;
var DataStr  : string;
begin
  if Assigned(FImageInfo)
  then begin
       if Assigned(FImageInfo.GPS)
       then begin
            with TGPSInfo(FImageInfo.GPS)
            do begin
               if (FVersionID <> 0) or true
               then SetTagInfo(EXIF_GPSVersionID, TAGF_BYTE, 4, 0, [0,0], #2+#2+#0+#0);

               if (FLatitudeRef <> '')
               then begin
                    FLatitudeRef := UpperCase(FLatitudeRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FLatitudeRef[1], ['N','S'])
                    {$ELSE}
                    if (FLatitudeRef[1] in ['N','S'])
                    {$ENDIF}
                    then begin
                         FLatitudeRef := FLatitudeRef[1] + #0;
                         SetTagInfo(EXIF_GPSLatitudeRef, TAGF_ASCII, 2, 0, [0,0], FLatitudeRef);
                    end;
               end;

               if (FLatitude[1] <> 0) and (FLatitude[3] <> 0) and (FLatitude[5] <> 0)
               then SetTagInfo(EXIF_GPSLatitude, TAGF_RATIONAL, 3, 0, FLatitude, '');

               if (FLongitudeRef <> '')
               then begin
                    FLongitudeRef := UpperCase(FLongitudeRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FLongitudeRef[1], ['E','W'])
                    {$ELSE} 
                    if (FLongitudeRef[1] in ['E','W'])
                    {$ENDIF}
                    then begin
                         FLongitudeRef := FLongitudeRef[1] + #0;
                         SetTagInfo(EXIF_GPSLongitudeRef, TAGF_ASCII, 2, 0, [0,0], FLongitudeRef);
                    end;
               end;

               if (FLongitude[1] <> 0) and (FLongitude[3] <> 0) and (FLongitude[5] <> 0)
               then SetTagInfo(EXIF_GPSLongitude, TAGF_RATIONAL, 3, 0, FLongitude, '');

               if (FAltitudeRef > 0)
               then SetTagInfo(EXIF_GPSAltitudeRef, TAGF_BYTE, 1, FAltitudeRef, [0,0], '');

               if (FAltitude[1] <> 0)
               then SetTagInfo(EXIF_GPSAltitude, TAGF_RATIONAL, 1, 0, FAltitude, '');

               if (FTimeStamp[1] <> 0) and (FTimeStamp[3] <> 0) and (FTimeStamp[5] <> 0)
               then SetTagInfo(EXIF_GPSTimeStamp, TAGF_RATIONAL, 3, 0, FTimeStamp, '');

               if (FSatellites <> '')
               then SetTagInfo(EXIF_GPSSatellites, TAGF_ASCII, Length(FSatellites) + 1, 0, [0,0], FSatellites + #0);

               if (FStatus <> '')
               then begin
                    FStatus := UpperCase(FStatus);
                    {$IFDEF UNICODE}
                    if CharInSet(FStatus[1], ['A','V'])
                    {$ELSE} 
                    if (FStatus[1] in ['A','V'])
                    {$ENDIF}
                    then begin
                         FStatus := FStatus[1] + #0;
                         SetTagInfo(EXIF_GPSStatus, TAGF_ASCII, 2, 0, [0,0], FStatus);
                    end;
               end;

               if (FMeasureMode <> '')
               then begin
                    {$IFDEF UNICODE}
                    if CharInSet(FMeasureMode[1], ['2','3'])
                    {$ELSE} 
                    if (FMeasureMode[1] in ['2','3'])
                    {$ENDIF}
                    then begin
                         FMeasureMode := FMeasureMode[1] + #0;
                         SetTagInfo(EXIF_GPSMeasureMode, TAGF_ASCII, 2, 0, [0,0], FMeasureMode);
                    end;
               end;

               if (FDOP[1] <> 0)
               then SetTagInfo(EXIF_GPSDOP, TAGF_RATIONAL, 1, 0, FDOP, '');

               if (FSpeedRef <> '')
               then begin
                    FSpeedRef := UpperCase(FSpeedRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FSpeedRef[1], ['K','M','N'])
                    {$ELSE} 
                    if (FSpeedRef[1] in ['K','M','N'])
                    {$ENDIF}
                    then begin
                         FSpeedRef := FSpeedRef[1] + #0;
                         SetTagInfo(EXIF_GPSSpeedRef, TAGF_ASCII, 2, 0, [0,0], FSpeedRef);
                    end;
               end;

               if (FSpeed[1] <> 0)
               then SetTagInfo(EXIF_GPSSpeed, TAGF_RATIONAL, 1, 0, FSpeed, '');

               if (FTrackRef <> '')
               then begin
                    FTrackRef := UpperCase(FTrackRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FTrackRef[1], ['T','M'])
                    {$ELSE} 
                    if (FTrackRef[1] in ['T','M'])
                    {$ENDIF}
                    then begin
                         FTrackRef := FTrackRef[1] + #0;
                         SetTagInfo(EXIF_GPSTrackRef, TAGF_ASCII, 2, 0, [0,0], FTrackRef);
                    end;
               end;

               if (FTrack[1] <> 0)
               then SetTagInfo(EXIF_GPSTrack, TAGF_RATIONAL, 1, 0, FTrack, '');

               if (FImgDirectionRef <> '')
               then begin
                    FImgDirectionRef := UpperCase(FImgDirectionRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FImgDirectionRef[1], ['T','M'])
                    {$ELSE} 
                    if (FImgDirectionRef[1] in ['T','M'])
                    {$ENDIF}
                    then begin
                         FImgDirectionRef := FImgDirectionRef[1] + #0;
                         SetTagInfo(EXIF_GPSImgDirectionRef, TAGF_ASCII, 2, 0, [0,0], FImgDirectionRef);
                    end;
               end;

               if (FImgDirection[1] <> 0)
               then SetTagInfo(EXIF_GPSImgDirection, TAGF_RATIONAL, 1, 0, FImgDirection, '');

               if (FMapDatum <> '')
               then SetTagInfo(EXIF_GPSMapDatum, TAGF_ASCII, Length(FMapDatum) + 1, 0, [0,0], FMapDatum + #0);

               if (FDestLatitudeRef <> '')
               then begin
                    FDestLatitudeRef := UpperCase(FDestLatitudeRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FDestLatitudeRef[1], ['N','S'])
                    {$ELSE} 
                    if (FDestLatitudeRef[1] in ['N','S'])
                    {$ENDIF}
                    then begin
                         FDestLatitudeRef := FDestLatitudeRef[1] + #0;
                         SetTagInfo(EXIF_GPSDestLatitudeRef, TAGF_ASCII, 2, 0, [0,0], FDestLatitudeRef);
                    end;
               end;

               if (FDestLatitude[1] <> 0) and (FDestLatitude[3] <> 0) and (FDestLatitude[5] <> 0)
               then SetTagInfo(EXIF_GPSDestLatitude, TAGF_RATIONAL, 3, 0, FDestLatitude, '');

               if (FDestLongitudeRef <> '')
               then begin
                    FDestLongitudeRef := UpperCase(FDestLongitudeRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FDestLongitudeRef[1], ['E','W'])
                    {$ELSE} 
                    if (FDestLongitudeRef[1] in ['E','W'])
                    {$ENDIF}
                    then begin
                         FDestLongitudeRef := FDestLongitudeRef[1] + #0;
                         SetTagInfo(EXIF_GPSDestLongitudeRef, TAGF_ASCII, 2, 0, [0,0], FDestLongitudeRef);
                    end;
               end;

               if (FDestLongitude[1] <> 0) and (FDestLongitude[3] <> 0) and (FDestLongitude[5] <> 0)
               then SetTagInfo(EXIF_GPSDestLongitude, TAGF_RATIONAL, 3, 0, FDestLongitude, '');

               if (FDestBearingRef <> '')
               then begin
                    FDestBearingRef := UpperCase(FDestBearingRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FDestBearingRef[1], ['T','M'])
                    {$ELSE} 
                    if (FDestBearingRef[1] in ['T','M'])
                    {$ENDIF}
                    then begin
                         FDestBearingRef := FDestBearingRef[1] + #0;
                         SetTagInfo(EXIF_GPSDestBearingRef, TAGF_ASCII, 2, 0, [0,0], FDestBearingRef);
                    end;
               end;

               if (FDestBearing[1] <> 0)
               then SetTagInfo(EXIF_GPSDestBearing, TAGF_RATIONAL, 1, 0, FDestBearing, '');

               if (FDestDistanceRef <> '')
               then begin
                    FDestDistanceRef := UpperCase(FDestDistanceRef);
                    {$IFDEF UNICODE}
                    if CharInSet(FDestDistanceRef[1], ['K','M','N'])
                    {$ELSE} 
                    if (FDestDistanceRef[1] in ['K','M','N'])
                    {$ENDIF}
                    then begin
                         FDestDistanceRef := FDestDistanceRef[1] + #0;
                         SetTagInfo(EXIF_GPSDestDistanceRef, TAGF_ASCII, 2, 0, [0,0], FDestDistanceRef);
                    end;
               end;

               if (FDestDistance[1] <> 0)
               then SetTagInfo(EXIF_GPSDestDistance, TAGF_RATIONAL, 1, 0, FDestDistance, '');

               if (FProcessingSize > 0)
               then begin
                    SetLength(DataStr, FProcessingSize);
                    CopyMemory(@DataStr[1], FProcessingMethod, FProcessingSize);
                    SetTagInfo(EXIF_GPSProcessingMethod, TAGF_UNDEFINED, FProcessingSize, 0, [0,0], DataStr);
               end;

               if (FAreaSize > 0)
               then begin
                    SetLength(DataStr, FAreaSize);
                    CopyMemory(@DataStr[1], FAreaInformation, FAreaSize);
                    SetTagInfo(EXIF_GPSAreaInformation, TAGF_UNDEFINED, FAreaSize, 0, [0,0], DataStr);
               end;

               if (FDateStamp <> 0)
               then begin
                    DataStr := FormatDateTime('yyyy:mm:dd', FDateStamp) + #0;
                    if (DataStr <> '')
                    then SetTagInfo(EXIF_GPSDateStamp, TAGF_ASCII, 11, 0, [0,0], DataStr);
               end;

               if (FDifferential > 0)
               then SetTagInfo(EXIF_GPSDifferential, TAGF_SHORT, 1, FDifferential, [0,0], '');
            end;
       end;
  end;
end; // TmcmExifImage.SetExifGPS.


procedure TmcmExifImage.WriteExifInteroperability(IFDOffset, Offset : longword);
begin
end; // TmcmExifImage.WriteExifInteroperability.


//------------------------------------------------------------------------------
// TmcmTIFFImage
//------------------------------------------------------------------------------

class function TmcmTIFFImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32];
end; // TmcmTIFFImage.GetColorFormats.


class function TmcmTIFFImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := Inherited GetCompressionFromColor(ImageFormat, Compress);

  if (ImageFormat in [IF_NONE,IF_BW])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_MODIFIED_HUFFMAN;
       inc(Count);
       // CP_MODIFIED_HUFFMAN_W
       if (High(Compress) > Count)
       then Compress[Count] := CP_CCITTGROUP3;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_CCITTGROUP3_2D;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_CCITTGROUP4;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_PACKBITS;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_LZW;
       inc(Count);
  end
  else if (ImageFormat in [IF_GREY4,IF_PAL4])
       then begin
            if (High(Compress) > Count)
            then Compress[Count] := CP_PACKBITS;
            inc(Count);
            if (High(Compress) > Count)
            then Compress[Count] := CP_LZW;
            inc(Count);
       end
       else if (ImageFormat in [IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32])
            then begin
                 if (High(Compress) > Count)
                 then Compress[Count] := CP_PACKBITS;
                 inc(Count);
                 if (High(Compress) > Count)
                 then Compress[Count] := CP_LZW;
                 inc(Count);
                 if (High(Compress) > Count)
                 then Compress[Count] := CP_JPEG_STD;
                 inc(Count);
            end;
  Result := Count;
end; // TmcmTIFFImage.GetCompressionFromColor.


class function TmcmTIFFImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP           : Result := resCP_NOCOMP;
  CP_MODIFIED_HUFFMAN : Result := resCP_MODIFIED_HUFFMAN;
  CP_CCITTGROUP3      : Result := resCP_CCITTGROUP3;
  CP_CCITTGROUP3_2D   : Result := resCP_CCITTGROUP3_2D;
  CP_CCITTGROUP4      : Result := resCP_CCITTGROUP4;
  CP_PACKBITS         : Result := resCP_PACKBITS;
  CP_LZW              : Result := resCP_LZW;
  CP_JPEG_STD         : Result := resCP_JPEG_STD;
  else Result := '';
  end;
end; // TmcmTIFFImage.GetCompressionName.


constructor TmcmTIFFImage.Create;
begin
  Inherited Create;
  FCanAppend     := True;
  FInvertLogPal  := False;
  FSafeBiLevel   := True;
  FDiffPredictor := False;
  FStream.BigEndian := True;
  FTags          := Nil;
  FExifTags      := Nil;
  FExifInterTags := Nil;
  FExifGPSTags   := Nil;

  FNrOfStrip     := 0;
  FStripDskOfs   := Nil;
  FStripSizes    := Nil;
  FRowsPerStrip  := Nil;
end; // TmcmTIFFImage.Create.


procedure TmcmTIFFImage.CloseFile(var Stream : TStream);
begin
  Clear;
  Inherited CloseFile(Stream);
end; // TmcmTIFFImage.CloseFile.


procedure TmcmTIFFImage.Clear;
begin
  if Assigned(FTags)
  then FTags.Free;
  FTags := Nil;
  if Assigned(FExifTags)
  then FExifTags.Free;
  FExifTags := Nil;
  if Assigned(FExifInterTags)
  then FExifInterTags.Free;
  FExifInterTags := Nil;
  if Assigned(FExifGPSTags)
  then FExifGPSTags.Free;
  FExifGPSTags := Nil;
  if Assigned(FJPEGImage)
  then FJPEGImage.Free;
  FJPEGImage := Nil;

  ClearStripTable;
end; // TmcmTIFFImage.Clear.


procedure TmcmTIFFImage.ClearStripTable;
begin
  try
    if (FStripDskOfs <> Nil) and (FNrOfStrip > 0)
    then FreeMem(FStripDskOfs);
    FStripDskOfs := Nil;
  except
  end;
  try
    if (FStripSizes <> Nil) and (FNrOfStrip > 0)
    then FreeMem(FStripSizes);
    FStripSizes := Nil;
  except
  end;
  try
    if (FRowsPerStrip <> Nil) and (FNrOfStrip > 0)
    then FreeMem(FRowsPerStrip);
    FRowsPerStrip := Nil;
  except
  end;
  FNrOfStrip := 0;
end; // TmcmTIFFImage.ClearStripTable.


function TmcmTIFFImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.BigEndian := True;
         FHeader.Order := FStream.ReadWord;
         if FHeader.Order = TIFF_LITTLEENDIAN
         then FStream.BigEndian := False
         else FStream.BigEndian := True;
         FHeader.Ver := FStream.ReadWord;
         if ((FHeader.Order = TIFF_LITTLEENDIAN) or (FHeader.Order = TIFF_BIGENDIAN)) and
            (FHeader.Ver = 0042)
         then begin
              FHeader.IFDaddr := FStream.ReadLong;
              FError := EC_OK;
         end;
       except
         On Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmTIFFImage.IsFormatValid.


function TmcmTIFFImage.GetNoImages : integer;
var SavePos   : {$IFNDEF DCB3_5} int64; {$ELSE} longint; {$ENDIF}
    BufSize   : integer;
    LEC       : word;
    LNextIFD  : longint;
    Count     : integer;
begin
  if (Abs(FNumImages) = 1)
  then begin
       if Assigned(FStream.Source)
       then begin
            try
              // Save stream position and bufer size.
              BufSize := FStream.MemorySize;
              FStream.MemorySize := 1;
              SavePos := FStream.Position;
              FStream.Position := FHeader.IFDaddr + FStreamStart;

              // Iterate through the TIFF file and get image count.
              Count := 1;
              repeat
                LEC := FStream.ReadWord;
                FStream.Position := FStream.Position + (LEC * 12);
                LNextIFD := FStream.ReadLong;
                if (LNextIFD <> 0)
                then begin
                     FStream.Position := longint(FStreamStart) + LNextIFD;
                     inc(Count);
                end;
              until (LNextIFD = 0) or (FStream.Position >= FStream.Size);
              FNumImages := Count;

              // Restore stream position and buffer size.
              FStream.MemorySize := BufSize;
              FStream.Position := SavePos;
            except
              On Exception
              do begin
                 FError := EC_UNKNOWN;
                 FNumImages := 1;
              end;
            end;
       end;
  end;
  Result := FNumImages;
end; // TmcmTIFFImage.GetNoImages.


function TmcmTIFFImage.GotoSubImage(var Index : integer) : TmcmErrorCode;
// GotoSubImage searches for the sub-image at Index, where the first image is
// at index zero.
// If Index = -1, GotoSubImage searches for the last image in the multiple image
// file, but does not change the Position marker of the stream to end-position.
// Instead it sets the position to the sub-image's EC position just preceding
// where LNextIFD is zero. When appending a new sub-image write the size of the
// stream to the position where LNextIFD is zero, and update the NewSubImage TAG.
// If successful, the position of the stream is changed to the sub-image's IFD
// position in the stream.
// If the FStream.Mode is not Read mode, this is set, but note GotoSubImage does
// not reset this mode back to it's original mode.
// It should be noted that multiple image tiff file can only be saved/read from
// a true tiff file, excluding a mix of streamed data.
var BufSize   : integer;
    LEC       : word;
    LECAddr   : {$IFNDEF DCB3_5} int64; {$ELSE} longint; {$ENDIF}
    LNextIFD  : longint;
    Count     : integer;
begin
  Result := EC_NOSUBIMAGE;
  if (Index = 0)
  then begin
       FStream.Position := FHeader.IFDaddr + FStreamStart;
       Result := EC_OK;
  end
  else begin
       if Assigned(FStream.Source)
       then begin
            BufSize := FStream.MemorySize;
            FStream.MemorySize := 1;
            if (FStream.Mode <> RWM_READ)
            then FStream.Mode := RWM_READ;

            if (FHeader.IFDaddr = 0)
            then begin
                 FStream.Position := 0;
                 IsFormatValid(FStream.Source);
            end;
            LECAddr := FHeader.IFDaddr + FStreamStart;
            FStream.Position := LECAddr;

            // Iterate through the TIFF file and get image count.
            Count := 0;
            repeat
              LEC := FStream.ReadWord;
              FStream.Position := FStream.Position + (LEC * 12);
              LNextIFD := FStream.ReadLong;
              if (LNextIFD <> 0)
              then begin
                   LECAddr := longint(FStreamStart) + LNextIFD;
                   FStream.Position := LECAddr;
                   inc(Count);
              end
              else FStream.Position := LECAddr;
            until (Index = Count) or
                  (LNextIFD = 0) or
                  (FStream.Position >= FStream.Size);

            // Restore stream position and buffer size.
            FStream.MemorySize := BufSize;

            if (Count = Index)
            then Result := EC_OK
            else if (Index = -1) and (LNextIFD = 0)
                 then begin
                      Index := Count + 1;
                      Result := EC_OK;
                 end;
       end;
  end;
end; // TmcmTIFFImage.GotoSubImage.


procedure TmcmTIFFImage.GotoStripOfs(    Stream     : TStream;
                                         StripNr    : word;
                                     var StripCount : longint;
                                         NrOfStrip  : word);
begin
  if (StripNr < NrOfStrip)
  then begin
       StripCount := FStripSizes^[StripNr];
       FStream.Position := FStripDskOfs^[StripNr];
  end
  else StripCount := 0;
end; // TmcmTIFFImage.GotoStripOfs.


procedure TmcmTIFFImage.ReadStripTable(Tags : TmcmTAGMgr);
var i, j, k : word;
    SOfs    : word;
begin
  ClearStripTable;

  // Get StripOffsets.
  j := Tags.Index[TAG_STRIPOFFSETS];
  if (Tags.List[j] <> Nil)
  then begin
       // Get number of strips.
       FNrOfStrip := Tags.Item[j].Length;
       GetMem(FStripDskOfs, FNrOfStrip * SizeOf(longword));
       GetMem(FStripSizes, FNrOfStrip * SizeOf(longword));
       GetMem(FRowsPerStrip, FNrOfStrip * SizeOf(longword));

       // Get strip offsets
       if (Tags.Item[j].Length = 1)
       then FStripDskOfs^[0] := Tags.Item[j].ValOffset
       else begin
            for k := 0 to (FNrOfStrip - 1)
            do begin
               // Goto Disk offset.
               if (Tags.Item[j].FieldType = TAGF_SHORT)
               then SOfs := 2
               else SOfs := 4;
               FStream.Position := Tags.Item[j].ValOffset + k * SOfs;

               // Get Strip offset.
               case Tags.Item[j].FieldType of
               TAGF_SHORT : FStripDskOfs^[k] := FStream.ReadWord;
               TAGF_LONG  : FStripDskOfs^[k] := FStream.ReadLong;
               end;
            end;
       end;
  end;

  // Get strip byte counts.
  i := Tags.Index[TAG_STRIPBYTECOUNTS];
  if (Tags.List[i] <> Nil)
  then begin
       if (Tags.Item[i].Length = 1)
       then begin
            FStripSizes^[0] := Tags.Item[i].ValOffset;
            if (i = 0)
            then FError := EC_OK; // StripByteCount missing.
            // FStripDskOfs^[0] := FTags.Item[i].ValOffset;
       end
       else begin // Get StripByteCount.
            for k := 0 to (FNrOfStrip - 1)
            do begin
               // Goto Disk offset.
               if (Tags.Item[i].FieldType = TAGF_SHORT)
               then SOfs := 2
               else SOfs := 4;
               FStream.Position := Tags.Item[i].ValOffset + k * SOfs;

               // Get Strip size.
               case Tags.Item[i].FieldType of
               TAGF_SHORT : FStripSizes^[k] := FStream.ReadWord;
               TAGF_LONG  : FStripSizes^[k] := FStream.ReadLong;
               end;
            end;
       end;
  end;
end; // TmcmTIFFImage.ReadStripTable.


procedure TmcmTIFFImage.SetStripOffsets(Offset : longword);
var I : word;
begin
  // Strip offset in file.
  i := FTags.Index[TAG_STRIPOFFSETS];
  if (i > 0)
  then begin
       if (FTags.Item[i].Length = 1)
       then begin
            FTags.Item[i].ValOffset := Offset;
            //inc(TagValOffset, SizeOf(longword));
       end
       else begin
            for i := 0 to (FNrOfStrip - 1)
            do begin
               FStripSizes^[i]  := FRowsPerStrip^[i] * FRowWidth;
               if (i = 0)
               then FStripDskOfs^[i] := Offset
               else FStripDskOfs^[i] := FStripDskOfs^[i-1] + FStripSizes^[i-1];
            end;
       end;
  end;

  // Strip byte count list.
  i := FTags.Index[TAG_STRIPBYTECOUNTS];
  if (i > 0)
  then begin
       if (FTags.Item[i].Length = 1)
       then begin
            FTags.Item[i].ValOffset := 0; // Initial guess!!!
            //inc(TagValOffset, SizeOf(longword));
       end
       else begin
            FillChar(FStripSizes^, FNrOfStrip * SizeOf(longword), 0);
            // FTagList[i]^.ValOffset := TagValOffset;
            // inc(TagValOffset, SizeOf(longword));
       end;
  end;
end; // TmcmTIFFImage.SetStripOffsets.


procedure TmcmTIFFImage.WriteStripOffsets(Stream : TStream);
// NOTE: This procedure changes the stream position!
var i, j : integer;
begin
  i := FTags.Index[TAG_STRIPOFFSETS];
  if (i > 0) and (FTags.Item[i] <> Nil) and (FNrOfStrip > 0)
  then begin
       // Write Strip disk offset's.
       if (FTags.Item[i].Length = 1)
       then begin
            FTags.Item[i].ValOffset := FStripDskOfs^[0];
            FStream.Position := FTags.Item[i].FilePos;
            FStream.Write(FTags.Item[i].Tag,       SizeOf(FTags.Item[i].Tag));
            FStream.Write(FTags.Item[i].FieldType, SizeOf(FTags.Item[i].FieldType));
            FStream.Write(FTags.Item[i].Length,    SizeOf(FTags.Item[i].Length));
            FStream.Write(FTags.Item[i].ValOffset, SizeOf(FTags.Item[i].ValOffset));
       end
       else begin
            FStream.Position := FTags.Item[i].ValOffset;
            for j := 0 to (FNrOfStrip - 1)
            do begin
               case FTags.Item[i].FieldType of
               TAGF_SHORT : FStream.Write(FStripDskOfs^[j], SizeOf(word));
               TAGF_LONG  : FStream.Write(FStripDskOfs^[j], SizeOf(longint));
               end;
            end;
       end;
  end
  else if (FError = EC_TIFFTAG)
       then FError := EC_OK;
end; // TmcmTIFFImage.WriteStripOffsets.


procedure TmcmTIFFImage.WriteStripCounts(Stream : TStream);
// NOTE: This procedure changes the stream position!
var i, j    : integer;
begin
  i := FTags.Index[TAG_STRIPBYTECOUNTS];
  if (i > 0) and (FTags.Item[i] <> Nil) and (FNrOfStrip > 0)
  then begin
       // Write Strip byte counts.
       if (FTags.Item[i].Length = 1)
       then begin
            FTags.Item[i].ValOffset := FStripSizes^[0];
            FStream.Position := FTags.Item[i].FilePos;
            FStream.Write(FTags.Item[i].Tag,       SizeOf(FTags.Item[i].Tag));
            FStream.Write(FTags.Item[i].FieldType, SizeOf(FTags.Item[i].FieldType));
            FStream.Write(FTags.Item[i].Length,    SizeOf(FTags.Item[i].Length));
            FStream.Write(FTags.Item[i].ValOffset, SizeOf(FTags.Item[i].ValOffset));
       end
       else begin
            FStream.Position := FTags.Item[i].ValOffset;
            for j := 0 to (FNrOfStrip - 1)
            do begin
               case FTags.Item[i].FieldType of
               TAGF_SHORT : FStream.Write(FStripSizes^[j], SizeOf(word));
               TAGF_LONG  : FStream.Write(FStripSizes^[j], SizeOf(longint));
               end;
            end;
       end;
  end
  else if (FError = EC_TIFFTAG)
       then FError := EC_OK;
end; // TmcmTIFFImage.WriteStripCounts.


procedure TmcmTIFFImage.InitRowsPerStrip(Tags : TmcmTAGMgr; Height : longword);
var i, j : word;
begin
  // Get RowsPerStrip.
  i := Tags.Index[TAG_ROWSPERSTRIP];
  if (FError = EC_TIFFTAG)
  then FError := EC_OK;
  if (Tags.Item[i] <> Nil)
  then begin
       j := 0;
       while (j < FNrOfStrip)
       do begin
          if (((j + 1) * Tags.Item[i].ValOffset) > Height) or (Tags.Item[i].ValOffset = 0)
          then FRowsPerStrip^[j] := longint(Height) - (j * longint(Tags.Item[i].ValOffset))
          else FRowsPerStrip^[j] := Tags.Item[i].ValOffset;
          inc(j);
       end;
  end;
end; // TmcmTIFFImage.InitRowsPerStrip.


function TmcmTIFFImage.RowWidth : longword;
var Value : longword;
begin
  with FDibInfo.bmiHeader
  do begin
     case biBitCount of
     1  : begin
            Value  := biWidth div 8;
            if ((8 * Value) < longword(biWidth))
            then inc(Value);
          end;
     4  : begin
            Value  := biWidth div 2;
            if ((2 * Value) < longword(biWidth))
            then inc(Value);
          end;
     8  : begin
            Value  := biWidth;
          end;
     16 : Value := 2 * biWidth;
     32 : Value := 4 * biWidth;
     else begin
            Value  := 3 * biWidth;
          end;
     end;
  end;
  Result := Value;
end; // TmcmTIFFImage.RowWidth.


function TmcmTIFFImage.ReadPalette(Stream : TStream) : integer;
type PTmpPal    = array[0..1024] of word;
var i, j, k     : integer;
    TmpPal      : ^PTmpPal;
    BytesToRead : integer;
begin
  Result := 0;
  // NoColors := GetNumColors(FDibInfo);
  if (FDibInfo^.bmiHeader.biBitCount < 24) // if <> RGB Image.
  then begin
       {$R-}
       case FPhotometric of
       PM_INVGREY,    // Bilevel and grayscale image, white = 0 and black = 255.
       PM_GREY,       // Bilevel and grayscale image, white = 255 and black = 0.
       PM_PALETTE   : // Palette color image.
                      CreateGreyPalette(FPhotometric);
       PM_RGB       : ; // RGB color image
       PM_MASK      : ; // Transparency mask
       PM_CMYK      : ; // CMYK Color separation
       PM_YCBCR     : ; // CCIR 601
       PM_CIELAB    : ; // CIE L*a*b* (1976)
       end;

       // Read Palette if present.
       if (FTags.Item[FTags.Index[TAG_COLORMAP]].ValOffset <> 0)
       then begin
            k := FTags.Index[TAG_COLORMAP];
            FStream.Position := FTags.Item[k].ValOffset;
            j := (FTags.Item[k].Length div 3 - 1);
            BytesToRead := 2 * (j + 1);
            GetMem(TmpPal, BytesToRead);

            if Not(FStream.BigEndian)
            then begin
                 // Read Red entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do FDibInfo^.bmiColors[i].rgbRed := LoWord(TmpPal^[i] shr 8);

                 // Read Green entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do FDibInfo^.bmiColors[i].rgbGreen := LoWord(TmpPal^[i] shr 8);

                 // Read Blue entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do begin
                    FDibInfo^.bmiColors[i].rgbBlue := LoWord(TmpPal^[i] shr 8);
                    FDibInfo^.bmiColors[i].rgbReserved := 0;
                 end;
            end
            else begin
                 // Read Red entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do FDibInfo^.bmiColors[i].rgbRed := LoWord(TmpPal^[i]);

                 // Read Green entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do FDibInfo^.bmiColors[i].rgbGreen := LoWord(TmpPal^[i]);

                 // Read Blue entries.
                 FStream.Read(TmpPal^[0], BytesToRead);
                 for i := 0 to j
                 do begin
                    FDibInfo^.bmiColors[i].rgbBlue := LoWord(TmpPal^[i]);
                    FDibInfo^.bmiColors[i].rgbReserved := 0;
                 end;
            end;
            FreeMem(TmpPal, BytesToRead);
       end;
  end;
end; // TmcmTIFFImage.ReadPalette.


function TmcmTIFFImage.WritePalette(Stream : TStream) : integer;
type TWordPal   = array[0..0] of word;
var NoColors, i : integer;
    pWordPal    : ^TWordPal;
begin
  Result := 0;
  try
    FPhotometric := GetPhotometric;
    if (FPhotometric = PM_GREY) or
       (FPhotometric = PM_INVGREY) or
       (FPhotometric = PM_PALETTE)
    then begin
         NoColors := GetNumColors(FDibInfo);
         GetMem(pWordPal, 2 * NoColors);
         try
           // Write Red entries.
           if FInvertLogPal
           then for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(255 - FDibInfo^.bmiColors[i].rgbRed) shl 8
           else for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(FDibInfo^.bmiColors[i].rgbRed) shl 8;
           FStream.Write(pWordPal^[0], 2 * NoColors);

           // Write Green entries.
           if FInvertLogPal
           then for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(255 - FDibInfo^.bmiColors[i].rgbGreen) shl 8
           else for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(FDibInfo^.bmiColors[i].rgbGreen) shl 8;
           FStream.Write(pWordPal^[0], 2 * NoColors);

           // Write Blue entries.
           if FInvertLogPal
           then for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(255 - FDibInfo^.bmiColors[i].rgbBlue) shl 8
           else for i := 0 to (NoColors - 1)
                do pWordPal^[i] := word(FDibInfo^.bmiColors[i].rgbBlue) shl 8;
           FStream.Write(pWordPal^[0], 2 * NoColors);
         finally
           FreeMem(pWordPal, 2 * NoColors);
         end;
    end;
  except
  end;
end; // TmcmTIFFImage.WritePalette.


procedure TmcmTIFFImage.WriteJPEGTables(Stream : TStream);
var Index         : integer;
    SaveStream    : TmcmBufferStream;
    SaveBigEndian : boolean;
begin
  if Assigned(FJPEGImage)
  then begin
       SaveStream := FJPEGImage.FStream;
       SaveBigEndian := FStream.BigEndian;
       FStream.BigEndian := True;
       FJPEGImage.FStream := FStream;

       FJPEGImage.WriteSOI;
       FJPEGImage.WriteQuantification;
       for Index := 0 to (FJPEGImage.FComponentCount - 1)
       do if (Index < 2)
          then FJPEGImage.WriteHuffmanTables(Index, True, True);
       FStream.ResetBitIndex;
       FJPEGImage.WriteEOI;

       FJPEGImage.FStream := SaveStream;
       FStream.BigEndian := SaveBigEndian;
  end;
end; // TmcmTIFFImage.WriteJPEGTables.


procedure TmcmTIFFImage.ReadHeader(Stream : TStream);
var i, j      : word;
    RedBits   : word;
    GreenBits : word;
    BlueBits  : word;
    // HScale    : word;
    // VScale    : word;
begin
  FStripNr   := 0;
  FStripSize := 0;
  FNrOfStrip := 0;

  FTags := TmcmExifImage.Create(FStream, FImageInfo, True);

  FError := EC_OK;
  FError := GotoSubImage(FIndex);
  if (FError = EC_OK)
  then FTags.ReadTags(0);
  // Extract information from TAG's
  if (FError = EC_OK)
  then begin
       if (FTags.NextIFD <> 0)
       then if (FNumImages = 1)
            then FNumImages := -1;

       // Examine sub-image type.
       i := FTags.Index[TAG_NEWSUBFILETYPE];
       if (i > 0)
       then begin
            case FTags.Item[i].ValOffset of
            // Thumb reduced resolution image
            1 : ;
            // Single page of a multi-page image
            2 : ;
            // Transparency mask
            4 : ;
            end;
       end
       else begin
            // Old Sub-file TAG
            i := FTags.Index[TAG_SUBFILETYPE];
            if (i > 0)
            then case FTags.Item[i].ValOffset of
                 // Full resolution image
                 1 : ;
                 // Thumb, reduced resolution image
                 2 : ;
                 // Single page of a multi-page image
                 3 : ;
                 end;
       end;
       if (FError = EC_TIFFTAG)
       then FError := EC_OK;

       i := FTags.Index[TAG_BITSPERSAMPLE];
       if (FTags.Item[i].Length = 1)
       then begin
            FBitPerSample := LoWord(FTags.Item[i].ValOffset);
            {
            if (FBitPerSample > 8)
            then FBitPerSample := 8;
            }
       end
       else begin
            FStream.Position := FTags.Item[i].ValOffset;
            RedBits   := FStream.ReadWord;
            GreenBits := FStream.ReadWord;
            BlueBits  := FStream.ReadWord;
            if (RedBits   <> 8) or
               (GreenBits <> 8) or
               (BlueBits  <> 8)
            then FError := EC_UNKNOWNFORMAT;

            FTags.Item[i].Length := 1;
            FTags.Item[i].ValOffset := 8;
            FBitPerSample := 8; // Bits per channel.
       end;
       FSamplePerPixel := LoWord(FTags.Item[FTags.Index[TAG_SAMPLESPERPIXEL]].ValOffset);

       // Reference Black & White.
       i := FTags.Index[TAG_REFERENCEBLACKWHITE];
       if (i > 0)
       then begin
            FCodingRange := round(exp(FBitPerSample * ln(2))) - 1;
            if (FTags.Item[i].Length = 6)
            then begin
                 for j := 0 to 2
                 do begin
                    FRefBlack[j] := FTags.ItemVal[i,2*j].Num / FTags.ItemVal[i,2*j].Denom;
                    FRefWhite[j] := FTags.ItemVal[i,2*j+1].Num / FTags.ItemVal[i,2*j+1].Denom;
                 end;
            end
            else begin
                 for j := 0 to 2
                 do begin
                    FRefBlack[j] := 0;
                    FRefWhite[j] := FCodingRange;
                 end;
            end;
       end;

       with FDibInfo^.bmiHeader
       do begin
          biWidth  := FTags.Item[FTags.Index[TAG_IMAGEWIDTH]].ValOffset;
          biHeight := FTags.Item[FTags.Index[TAG_IMAGELENGTH]].ValOffset;
       end;

       FPhotometric  := TmcmPhotometric(LoWord(FTags.Item[FTags.Index[TAG_PHOTOMETRIC]].ValOffset));
       case FPhotometric of
       PM_MASK,
       PM_CMYK,
       //PM_YCBCR,
       PM_DONTUSE,
       PM_CIELAB   : FError := EC_BADCOLORFORMAT;
       end;

       case LoWord(FTags.Item[FTags.Index[TAG_COMPRESSION]].ValOffset) of
       TIFCP_NONE          : FCompress := CP_NOCOMP;
       TIFCP_CCITTRLE      : FCompress := CP_MODIFIED_HUFFMAN;
       TIFCP_CCITTFAX3     : begin
                               //FPhotometric := PM_INVGREY;
                               i := FTags.Index[TAG_T4OPTIONS];
                               if (i > 0)
                               then FT4Option := FTags.Item[i].ValOffset
                               else FT4Option := $00;

                               if ((FT4Option and $01) = 0)
                               then FCompress := CP_CCITTGROUP3
                               else FCompress := CP_CCITTGROUP3_2D;

                               if (FError = EC_TIFFTAG)
                               then FError := EC_OK;
                             end;
       TIFCP_CCITTFAX4     : begin
                               //FPhotometric := PM_INVGREY;
                               FCompress := CP_CCITTGROUP4;
                               i := FTags.Index[TAG_T6OPTIONS];
                               if (i > 0)
                               then FT6Option := FTags.Item[i].ValOffset
                               else FT6Option := $00;
                               if (FError = EC_TIFFTAG)
                               then FError := EC_OK;
                             end;
       TIFCP_LZW           : FCompress := CP_LZW;
       TIFCP_OJPEG         : FCompress := CP_JPEG_STD;
       TIFCP_JPEG          : begin
                               FCompress := CP_JPEG_STD;
                               i := FTags.Index[TAG_JPEGTABLES];
                               if Not(i > 0) and False // A global JPEG table is not required.
                               then FError := EC_COMPRESSION;

                               // Get YCbCr Coefficients
                               i := FTags.GetTagIndex(TAG_YCBCRCOEFFICIENTS);
                               if (i > 0)
                               then begin
                                    for j := 0 to 2
                                    do begin
                                       FYCbCrCoef[2*j+0] := FTags.ItemVal[i,j].Num;
                                       FYCbCrCoef[2*j+1] := FTags.ItemVal[i,j].Denom;
                                    end;
                               end;
                               {
                               // This information is not used - Read in JPEG!
                               i := FTags.GetTagIndex(TAG_YCBCRSUBSAMPLING);
                               if (i > 0)
                               then begin
                                    HScale := FTags.Item[i].ValOffset shr 16;
                                    VScale := FTags.Item[i].ValOffset and $FFFF;
                               end;
                               }
                             end;
       (*
       TIFCP_ADOBE_DEFLATE :
       TIFCP_NEXT          :
       TIFCP_CCITTRLEW     : FCompress := CP_HUFFMAN_RLEW;
       *)
       TIFCP_PACKBITS      : FCompress := CP_PACKBITS;
       (*
       TIFCP_THUNDERSCAN   : FCompress := CP_THUNDERSCAN;
       *)
       (*
       // codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
       TIFCP_IT8CTPAD      : ;
       TIFCP_IT8LW         : ;
       TIFCP_IT8MP         : ;
       TIFCP_IT8BL         : ;
       // compression codes 32908-32911 are reserved for Pixar
       TIFCP_PIXARFILM     : ;
       TIFCP_PIXARLOG      : ;
       TIFCP_DEFLATE       : ;
       // compression 32947 is reserved for Oceana Matrix (dev@oceana.com)
       TIFCP_DCS           : ;
       TIFCP_JBIG          : ;
       *)
       else FError := EC_COMPRESSION;
       end;

       // Clear EC_TIFFTAG error.
       if (FTags.FError = EC_TIFFTAG)
       then FTags.FError := EC_OK;

       if (FError = EC_OK)
       then begin
            FTags.TagsToImageInfo;
            with FDibInfo^.bmiHeader
            do begin
               biPlanes       := 1;
               biBitCount     := FBitPerSample * FSamplePerPixel;
               biClrUsed      := 0;
               biClrImportant := 0;

               FRowWidth       := RowWidth;
               FPlanarConfig   := LoWord(FTags.Item[FTags.Index[TAG_PLANARCONFIG]].ValOffset);

               if Assigned(FImageInfo)
               then begin
                    FImageInfo.Units := UN_METERS;
                    biXPelsPerMeter  := Round(FImageInfo.XResolution);
                    biYPelsPerMeter  := Round(FImageInfo.YResolution);
               end;
            end;

            // Read Exit attibute information.
            i := FTags.Index[TAG_EXIF_IFD];
            if (i > 0)
            then FTags.ReadExifAttribute(FTags.Item[i].ValOffset, 0);

            i := FTags.Index[TAG_GPS_IFD];
            if (i > 0)
            then begin
                 FExifGPSTags := TmcmExifImage.Create(FStream, FImageInfo, False);
                 if Assigned(FExifGPSTags)
                 then begin
                      FExifGPSTags.ReadExifGPS(FTags.Item[i].ValOffset, 0);
                      FExifGPSTags.Free;
                 end
                 else FError := EC_NOMEMORY;
                 FExifGPSTags := Nil;
            end;

            i := FTags.Index[TAG_INTEROPERABILITY_IFD];
            if (i > 0)
            then begin
                 FExifInterTags := TmcmExifImage.Create(FStream, FImageInfo, False);
                 if Assigned(FExifInterTags)
                 then begin
                      FExifInterTags.ReadExifInteroperability(FTags.Item[i].ValOffset, 0);
                      FExifInterTags.Free;
                 end
                 else FError := EC_NOMEMORY;
                 FExifInterTags := Nil;
            end;

            if (FError = EC_TIFFTAG)
            then FError := EC_OK;

            i := FTags.Index[TAG_FILLORDER];
            FFillOrder := LoWord(FTags.Item[i].ValOffset);
            if (FFillOrder <> 1)
            then if (FCompress <> CP_CCITTGROUP4) and
                    (FCompress <> CP_CCITTGROUP3) and
                    (FCompress <> CP_CCITTGROUP3_2D) and
                    (FCompress <> CP_MODIFIED_HUFFMAN)
                 then FError := EC_DATAORDER;

            // Get number of strips and strip table.
            i := FTags.Index[TAG_STRIPOFFSETS];
            if (i > 0)
            then begin
                 FNrOfStrip := FTags.Item[i].Length;
                 ReadStripTable(FTags);
                 InitRowsPerStrip(FTags, FDibInfo.bmiHeader.biHeight);
            end;

            // Clear Tag errors occured in this "IF" sentence.
            // EC_TIFFTAG errors in this "IF" sentence are not critical!
            if (FError = EC_TIFFTAG)
            then FError := EC_OK;
       end;
  end;
end; // TmcmTIFFImage.ReadHeader.


procedure TmcmTIFFImage.WriteHeader(Stream : TStream);
var TmpStr       : string;
    xrNum        : longint;
    xrDenum      : longint;
    yrNum        : longint;
    yrDenum      : longint;
    xpNum        : longint;
    xpDenum      : longint;
    ypNum        : longint;
    ypDenum      : longint;
    TIFComp      : word;
    i, LEC       : word;
    HScale       : word;
    VScale       : word;
    SavePos      : {$IFNDEF DCB3_5} int64; {$ELSE} longint; {$ENDIF}
    BufSize      : integer;
    TagOffset    : longword;
    DefRowHeight : word;  // Defines row height when saving. Default is
                          // full image height, with the exception for
                          // LZW where is Round(8192 / FLongWidth) used.
    JPEGTableSize : integer;
begin
  FTags := TmcmExifImage.Create(FStream, FImageInfo, True);
  if (FIndex = -1)
  then begin
       // Append an image to an existing TIFF file.
       SavePos := FStreamStart;
       FStreamStart := 0;
       // FIndex is after the call assigned the page number.
       FError := GotoSubImage(FIndex);
       if (FError = EC_OK)
       then begin
            BufSize := FStream.MemorySize;
            FStream.MemorySize := 1;
            LEC := FStream.ReadWord;
            if (FStream.Mode <> RWM_WRITE)
            then FStream.Mode := RWM_WRITE;
            FStream.Position := FStream.Position + (LEC * 12);
            FStream.WriteLong(FStream.Size);
            FStream.Flush;
            FStream.MemorySize := BufSize;
            if (FStream.Position <> FStream.Size)
            then FStream.Position := FStream.Size;
            FStreamStart := SavePos;
       end;
  end;

  VScale := 1;

  with FDibInfo^.bmiHeader
  do begin
     case biPlanes * biBitCount of
     8  : begin
            if (FCompress = CP_JPEG_STD) and (FPhotometric <> PM_GREY)
            then FSamplePerPixel := 3
            else FSamplePerPixel := 1;
            FBitPerSample   := biBitCount div FSamplePerPixel;
          end;
     24 : begin
            FSamplePerPixel := 3;
            FPhotometric    := PM_RGB;
            FBitPerSample   := 8;
          end;
     32 : begin
            if (FCompress = CP_JPEG_STD)
            then FSamplePerPixel := 3
            else FSamplePerPixel := 4;
            FPhotometric    := PM_RGB;
            FBitPerSample   := 8;
          end;
     else begin
          FSamplePerPixel := 1;
          FBitPerSample   := biBitCount div FSamplePerPixel;
     end;
     end;
     FRowWidth := RowWidth;

     {if (FIndex = 0)
     then SetTagInfo(TAG_NEWSUBFILETYPE, TAGF_LONG, 0, 2, 0, 0, '')
     else} FTags.SetTagInfo(TAG_NEWSUBFILETYPE, TAGF_LONG, 0, 2, [0,0], '');
     // Old TIFF style.
     // SetTagInfo(TAG_SUBFILETYPE,    TAGF_SHORT, 0, 1, 0, 0, '');
     FTags.SetTagInfo(TAG_IMAGEWIDTH,     TAGF_LONG, 0, biWidth, [0,0], '');
     FTags.SetTagInfo(TAG_IMAGELENGTH,    TAGF_LONG, 0, biHeight, [0,0], '');

     case FSamplePerPixel of
     1 : FTags.SetTagInfo(TAG_BITSPERSAMPLE, TAGF_SHORT, 0, FBitPerSample, [0,0], '');
     3 : begin
          TmpStr := Chr(08) + Chr(00) +
                    Chr(08) + Chr(00) +
                    Chr(08) + Chr(00); // Red, Green, Blue = BitPerSample.
          FTags.SetTagInfo(TAG_BITSPERSAMPLE, TAGF_SHORT, 3, 0, [0,0], TmpStr);
         end;
     4 : begin
          TmpStr := Chr(08) + Chr(00) +
                    Chr(08) + Chr(00) +
                    Chr(08) + Chr(00) +
                    Chr(08) + Chr(00); // Red, Green, Blue = BitPerSample.
          FTags.SetTagInfo(TAG_BITSPERSAMPLE, TAGF_SHORT, 4, 0, [0,0], TmpStr);
         end;
     end;
     FTags.SetTagInfo(TAG_SAMPLESPERPIXEL, TAGF_SHORT, 0, FSamplePerPixel, [0,0], '');

     // Compression TAG
     case FCompress of
     CP_NOCOMP               : TIFComp := TIFCP_NONE;
     CP_MODIFIED_HUFFMAN     : TIFComp := TIFCP_CCITTRLE;
     CP_CCITTGROUP3          : begin
                                 TIFComp := TIFCP_CCITTFAX3;
                                 FT4Option := $00;
                               end;
     CP_CCITTGROUP3_2D       : begin
                                 TIFComp := TIFCP_CCITTFAX3;
                                 FT4Option := $01;
                               end;
     CP_CCITTGROUP4          : begin
                                 TIFComp := TIFCP_CCITTFAX4;
                                 FT6Option := $00;
                               end;
     CP_LZW                  : TIFComp := TIFCP_LZW;
     // TIFCP_OJPEG          : TIFComp := TIFCP_OJPEG;
     CP_JPEG_STD             : begin
                                 TIFComp := TIFCP_JPEG;
                                 // Force photometric to Y Cb Cr.
                                 if (FPhotometric <> PM_GREY)
                                 then FPhotometric := PM_YCBCR;
                                 // Photometric could when implemented in
                                 // TmcmJPEGImage also be PM_RGB
                               end;
     // TIFCP_ADOBE_DEFLATE  : TIFComp := TIFCP_ADOBE_DEFLATE;
     // TIFCP_NEXT           : TIFComp := TIFCP_NEXT;
     // CP_MODIFIED_HUFFMAN_RLEW : TIFComp := TIFCP_CCITTRLEW;
     CP_PACKBITS             : TIFComp := TIFCP_PACKBITS;
     // CP_THUNDERSCAN       : TIFComp := TIFCP_THUNDERSCAN;
     else TIFComp            := TIFCP_NONE;
     end;

     FTags.SetTagInfo(TAG_COMPRESSION, TAGF_SHORT, 0, TIFComp, [0,0], '');

     FTags.SetTagInfo(TAG_PHOTOMETRIC, TAGF_SHORT, 0, word(FPhotometric), [0,0], '');
     FTags.SetTagInfo(TAG_FILLORDER, TAGF_SHORT, 0, 1, [0,0], '');

     case FCompress of
     CP_CCITTGROUP3,
     CP_CCITTGROUP3_2D : FTags.SetTagInfo(TAG_T4OPTIONS, TAGF_LONG, 0, FT4Option, [0,0], '');
     CP_CCITTGROUP4    : FTags.SetTagInfo(TAG_T6OPTIONS, TAGF_LONG, 0, FT6Option, [0,0], '');
     CP_LZW            : begin // Determine if Difference Predictor should be applied.
                               // 8, 24 and 32 bit depth are supported.
                           if (biPlanes * biBitCount in [8,24,32]) and (FPhotometric <> PM_PALETTE)
                           then FDiffPredictor := True
                           else FDiffPredictor := False;
                           if FDiffPredictor
                           then FTags.SetTagInfo(TAG_PREDICTOR, TAGF_SHORT, 0, 2, [0,0], '')
                           else FTags.SetTagInfo(TAG_PREDICTOR, TAGF_SHORT, 0, 1, [0,0], '');
                         end;
     CP_JPEG_STD       : begin
                           FJPEGImage := TmcmJPEGImage.Create;
                           FJPEGImage.YCbCrMode := FYCbCrMode;
                           FJPEGImage.FQuality := FQuality;
                           FJPEGImage.SetDibHandle(FDibHandle);
                           if FJPEGImage.GetDibData
                           then begin
                                JPEGTableSize := FJPEGImage.WriteTIFFInitialise;

                                FTags.SetTagInfo(TAG_JPEGTABLES, TAGF_UNDEFINED, JPEGTableSize, 0, [0,0], '');
                                // Use Only when FPhotometric = PM_YCBCR
                                if (FPhotometric = PM_YCBCR)
                                then begin
                                     FYCbCrCoef[0] := 2989;  // LumaRed
                                     FYCbCrCoef[1] := 10000;
                                     FYCbCrCoef[2] := 5867;  // LumaGreen
                                     FYCbCrCoef[3] := 10000;
                                     FYCbCrCoef[4] := 1144;  // LumaBlue
                                     FYCbCrCoef[5] := 10000;
                                     FTags.SetTagInfo(TAG_YCBCRCOEFFICIENTS, TAGF_RATIONAL, 3, 0, FYCbCrCoef, '');
                                end;
                                // If FPhotometric <> PM_YCBCR then use no sub-sampling.
                                if (FPhotometric = PM_YCBCR)
                                then begin
                                     // Indirectly get values from TmcmJPEGImage.
                                     // Chrominance component
                                     HScale := FJPEGImage.FYCbCrComp[0].HorizFreq div FJPEGImage.FYCbCrComp[1].HorizFreq;
                                     VScale := FJPEGImage.FYCbCrComp[0].VertFreq div FJPEGImage.FYCbCrComp[1].VertFreq;
                                     FTags.SetTagInfo(TAG_YCBCRSUBSAMPLING, TAGF_SHORT, 2, HScale shl 16 + VScale, [0,0], '');

                                     // Use Only when FPhotometric = PM_YCBCR
                                     FTags.SetTagInfo(TAG_YCBCRPOSITIONING, TAGF_SHORT, 1, 1, [0,0], '');
                                end;
                                FTags.SetTagInfo(TAG_REFERENCEBLACKWHITE, TAGF_RATIONAL, 6, 0, [0,1, 255,1, 128,1, 255,1, 128,1, 255,1], '');

                                (* Old JPEG implementation (version 6).
                                TAG_JPEGPROC                      = 512;   // 200   Short          JPEGProc
                                TAG_JPEGINTERCHANGEFORMAT         = 513;   // 201   Long           JPEGInterchangeFormat
                                TAG_JPEGINTERCHANGEFORMATLENGTH   = 514;   // 202   Long           JPEGInterchangeFormatLength
                                TAG_JPEGRESTARTINTERVAL           = 515;   // 203   Short          JPEGRestartInterval
                                TAG_JPEGLOSSLESSPREDICTORS        = 517;   // 205   Short          JPEGLossLessPredictors
                                TAG_JPEGPOINTTRANSFORMS           = 518;   // 206   Short          JPEGPointTransforms
                                TAG_JPEGQTABLES                   = 519;   // 207   Long           JPEGQTables
                                TAG_JPEGDCTABLES                  = 520;   // 208   Long           JPEGDCTables
                                TAG_JPEGACTABLES                  = 521;   // 209   Long           JPEGACTables
                                *)
                           end
                           else FError := EC_BADIMAGEHANDLE;
                         end;
     end;

     xrNum   := round(254 * biXPelsPerMeter);
     xrDenum := 10000;
     yrNum   := round(254 * biYPelsPerMeter);
     yrDenum := 10000;
     xpNum   := 0;
     xpDenum := 1;
     ypNum   := 0;
     ypDenum := 1;
     FResolutionUnit := 2;

     FTags.SetTagInfo(TAG_XRESOLUTION, TAGF_RATIONAL, 0, 0, [xrNum, xrDenum], '');
     FTags.SetTagInfo(TAG_YRESOLUTION, TAGF_RATIONAL, 0, 0, [yrNum, yrDenum], '');
     FTags.SetTagInfo(TAG_XPOSITION, TAGF_RATIONAL, 0, 0, [xpNum, xpDenum], '');
     FTags.SetTagInfo(TAG_YPOSITION, TAGF_RATIONAL, 0, 0, [ypNum, ypDenum], '');
     FTags.SetTagInfo(TAG_RESOLUTIONUNIT, TAGF_SHORT, 0, FResolutionUnit, [0,0], '');

     FTags.ImageInfoToTags(FIndex);

     // Set-up strips
     case FCompress of
     CP_LZW      : begin
                     DefRowHeight := Round(8192 / FLongWidth);
                     if (DefRowHeight = 0)
                     then DefRowHeight := 1;
                   end;
     CP_JPEG_STD : begin
                     FNrOfStrip := FJPEGImage.McuRows; //
                     if (FPhotometric = PM_YCBCR)
                     then begin
                          if (VScale = 1)
                          then DefRowHeight := 8
                          else DefRowHeight := 16;
                     end
                     else DefRowHeight := 8;
                   end;
     else DefRowHeight := 16;
     end;

     if (FDibInfo^.bmiHeader.biHeight <= DefRowHeight) or
        (FCompress = CP_NOCOMP) or
        (FCompress = CP_CCITTGROUP3) or
        (FCompress = CP_CCITTGROUP3_2D) or
        (FCompress = CP_CCITTGROUP4)
     then begin
          FNrOfStrip := 1;
          DefRowHeight := FDibInfo^.bmiHeader.biHeight;
     end
     else begin
          FNrOfStrip := Trunc(FDibInfo^.bmiHeader.biHeight / DefRowHeight);
          if ((FDibInfo^.bmiHeader.biHeight mod DefRowHeight) <> 0)
          then inc(FNrOfStrip);
     end;

     GetMem(FStripDskOfs, FNrOfStrip * SizeOf(longint));
     GetMem(FStripSizes, FNrOfStrip * SizeOf(longint));
     GetMem(FRowsPerStrip, FNrOfStrip * SizeOf(longint));

     FTags.SetTagInfo(TAG_STRIPOFFSETS, TAGF_LONG, FNrOfStrip, 0, [0,0], '');
     if (FNrOfStrip >= 1)
     then begin
          FTags.SetTagInfo(TAG_STRIPBYTECOUNTS, TAGF_LONG, FNrOfStrip, 0, [0,0], '');
          FTags.SetTagInfo(TAG_ROWSPERSTRIP, TAGF_LONG, 1, DefRowHeight, [0,0], '');
          InitRowsPerStrip(FTags, FDibInfo.bmiHeader.biHeight);
     end
     else FRowsPerStrip^[0] := DefRowHeight;

     if (FPhotometric = PM_GREY) or
        (FPhotometric = PM_INVGREY) or
        (FPhotometric = PM_PALETTE)
     then begin
          // For PM_GREY and PM_INVGREY a check of the continuens of entries in
          // the palette could establish if the palette should or shouldn't be
          // included/saved!
          FTags.SetTagInfo(TAG_COLORMAP, TAGF_SHORT, 3 * round(exp(FBitPerSample * ln(2))){0}, 0, [0,0], '');
          //FTagList[FEC]^.Length := 3 * round(exp(FBitPerSample * ln(2)));
     end;
  end;

  // Add Exif & GPS information.
  if Assigned(FImageInfo)
  then begin
       // Exif
       if Assigned(FImageInfo.Exif)
       then begin
            FExifTags := TmcmExifImage.Create(FStream, FImageInfo, False);
            if Assigned(FExifTags)
            then begin
                 FExifTags.SetExifAttribute(Not(FCompress in [CP_JPEG_STD, CP_JPEG_PROG]));
                 FTags.SetTagInfo(TAG_EXIF_IFD, TAGF_LONG, 1, FExifTags.EC, [0,0], '');
            end
            else FError := EC_NOMEMORY;
       end;

       // GPS
       if Assigned(FImageInfo.GPS)
       then begin
            FExifGPSTags := TmcmExifImage.Create(FStream, FImageInfo, False);
            if Assigned(FExifGPSTags)
            then begin
                 FExifGPSTags.SetExifGPS;
                 FTags.SetTagInfo(TAG_GPS_IFD, TAGF_LONG, 1, FExifGPSTags.EC, [0,0], '');
            end
            else FError := EC_NOMEMORY;
       end;
  end;

  if (FError = EC_OK)
  then begin
       // Write TIFF header.
       if (FIndex = 0)
       then begin
            FHeader.order   := TIFF_LITTLEENDIAN;
            FHeader.ver     := 42;
            FHeader.IFDaddr := SizeOf(TIFH);
            FStream.Write(FHeader.order,   SizeOf(FHeader.order));
            FStream.Write(FHeader.ver,     SizeOf(FHeader.ver));
            FStream.Write(FHeader.IFDaddr, SizeOf(FHeader.IFDaddr));
       end;
       if FHeader.Order = TIFF_LITTLEENDIAN
       then FStream.BigEndian := False
       else FStream.BigEndian := True;

       // Calculate Offset to Tag's value field.
       if (FIndex = 0)
       then TagOffset := FHeader.IFDaddr + SizeOf(FTags.NextIFD) + (FTags.EC * 12) + SizeOf(FTags.EC) + FStreamStart
       else TagOffset := SizeOf(FTags.NextIFD) + (FTags.EC * 12) + SizeOf(FTags.EC) + FStreamStart;

       if Assigned(FExifTags)
       then begin
            i := FTags.Index[TAG_EXIF_IFD];
            if (i > 0)
            then FTags.Item[i].ValOffset := TagOffset;
            TagOffset := TagOffset + SizeOf(FExifTags.NextIFD) + (FExifTags.EC * 12) + SizeOf(FExifTags.EC);
       end;

       if Assigned(FExifGPSTags)
       then begin
            i := FTags.Index[TAG_GPS_IFD];
            if (i > 0)
            then FTags.Item[i].ValOffset := TagOffset;
            TagOffset := TagOffset + SizeOf(FExifGPSTags.NextIFD) + (FExifGPSTags.EC * 12) + SizeOf(FExifGPSTags.EC);
       end;

       // Calculate TAG data offset's.
       FTags.WriteCalcOffsets(TagOffset);
       if Assigned(FExifTags)
       then FExifTags.WriteCalcOffsets(TagOffset);
       if Assigned(FExifGPSTags)
       then FExifGPSTags.WriteCalcOffsets(TagOffset);

       // Write strip offset's.
       SetStripOffsets(TagOffset);

       FTags.FOnWritePalette      := WritePalette;
       FTags.FOnWriteStripOffsets := WriteStripOffsets;
       FTags.FOnWriteStripCounts  := WriteStripCounts;
       FTags.FOnWriteJPEGTables   := WriteJPEGTables;

       // Write TAG's
       FTags.WriteTags;
       if Assigned(FExifTags)
       then FExifTags.WriteTags;
       if Assigned(FExifGPSTags)
       then FExifGPSTags.WriteTags;

       // Write TAG data.
       FTags.WriteTagsData;
       if Assigned(FExifTags)
       then FExifTags.WriteTagsData;
       if Assigned(FExifGPSTags)
       then FExifGPSTags.WriteTagsData;
  end;
end; // TmcmTIFFImage.WriteHeader.


procedure TmcmTIFFImage.ReadPredictor;
// Add the intensity amount from the previous pixel to the current, and swap R
// and B color channels.
type T24RGB = array[0..0] of TRGBTriple;
     T32RGB = array[0..0] of TRGBQuad;
var  i      : longword;
     Start  : TVecPointerRec;
     ToAddr : TVecPointerRec;
     Bits   : TVecPointerRec;
     RVal   : byte;
     BVal   : byte;
begin
  Start.Long := 0;
  Bits.Ptr   := FDibBits;
  if (Bits.Ptr <> nil)
  then begin
       case FBitCount of
       8  : while (DWORD(Start.Long) < FDibInfo^.bmiHeader.biSizeImage)
            do begin
               ToAddr.Long := Bits.Long + Start.Long;
               try
                 for i := 1 to (FDibInfo^.bmiHeader.biWidth - 1)
                 do begin
                    RVal := TVectorB(ToAddr.Ptr^)[i];
                    TVectorB(ToAddr.Ptr^)[i] := RVal + TVectorB(ToAddr.Ptr^)[i-1];
                 end;
               except
               end;
               Start.Long := Start.Long + FLongWidth;
            end;
       24 : while (DWORD(Start.Long) < FDibInfo^.bmiHeader.biSizeImage)
            do begin
               ToAddr.Long := Bits.Long + Start.Long;
               try
                 RVal := T24RGB(ToAddr.Ptr^)[0].RGBtblue;
                 // This line generates an access violation.
                 // TARGB(ToAddr.Ptr^)[i].RGBtblue := TARGB(ToAddr.Ptr^)[i].RGBtred;
                 // Therefore GVal is introduced, which does not generate the AV !?!
                 BVal := T24RGB(ToAddr.Ptr^)[0].RGBtred;
                 T24RGB(ToAddr.Ptr^)[0].RGBtblue := BVal;
                 T24RGB(ToAddr.Ptr^)[0].RGBtred  := RVal;
                 for i := 1 to (FDibInfo^.bmiHeader.biWidth - 1)
                 do begin
                    RVal := T24RGB(ToAddr.Ptr^)[i].RGBtred;
                    BVal := T24RGB(ToAddr.Ptr^)[i].RGBtblue;
                    T24RGB(ToAddr.Ptr^)[i].RGBtblue  := RVal + T24RGB(ToAddr.Ptr^)[i-1].RGBtblue;
                    T24RGB(ToAddr.Ptr^)[i].rgbtGreen := T24RGB(ToAddr.Ptr^)[i].rgbtGreen + T24RGB(ToAddr.Ptr^)[i-1].rgbtGreen;
                    T24RGB(ToAddr.Ptr^)[i].RGBtred   := BVal + T24RGB(ToAddr.Ptr^)[i-1].RGBtred;
                 end;
               except
               end;
               Start.Long := Start.Long + FLongWidth;
            end;
       32 : while (DWORD(Start.Long) < FDibInfo^.bmiHeader.biSizeImage)
            do begin
               ToAddr.Long := Bits.Long + Start.Long;
               try
                 RVal := T32RGB(ToAddr.Ptr^)[0].rgbBlue;
                 // This line generates an access violation.
                 // TARGB(ToAddr.Ptr^)[i].RGBtblue := TARGB(ToAddr.Ptr^)[i].RGBtred;
                 // Therefore GVal is introduced, which does not generate the AV !?!
                 BVal := T32RGB(ToAddr.Ptr^)[0].rgbRed;
                 T32RGB(ToAddr.Ptr^)[0].rgbBlue := BVal;
                 T32RGB(ToAddr.Ptr^)[0].rgbRed  := RVal;
                 for i := 1 to (FDibInfo^.bmiHeader.biWidth - 1)
                 do begin
                    RVal := T32RGB(ToAddr.Ptr^)[i].rgbRed;
                    BVal := T32RGB(ToAddr.Ptr^)[i].rgbBlue;
                    T32RGB(ToAddr.Ptr^)[i].rgbBlue  := RVal + T32RGB(ToAddr.Ptr^)[i-1].rgbBlue;
                    T32RGB(ToAddr.Ptr^)[i].rgbGreen := T32RGB(ToAddr.Ptr^)[i].rgbGreen + T32RGB(ToAddr.Ptr^)[i-1].rgbGreen;
                    T32RGB(ToAddr.Ptr^)[i].rgbRed   := BVal + T32RGB(ToAddr.Ptr^)[i-1].rgbRed;
                    T32RGB(ToAddr.Ptr^)[i].rgbReserved := T32RGB(ToAddr.Ptr^)[i].rgbReserved + T32RGB(ToAddr.Ptr^)[i-1].rgbReserved;
                 end;
               except
               end;
               Start.Long := Start.Long + FLongWidth;
            end;
       else FError := EC_PREDICTORBIT;
       end;
  end;
end; // TmcmTIFFImage.ReadPredictor.


procedure TmcmTIFFImage.LoadPlanarData(Stream       : TStream;
                                       ImageOfs     : longint;
                                       ImageSize    : longint;
                                       BytesToRead  : longint;
                                       PixelWidth   : longint;
                                       IsRGB        : boolean);
type TARGB     = array[0..0] of TRGBTriple;
     TAByte    = array[0..0] of byte;
var  Count     : Longint;
     Start     : TVecByteRec;
     ToAddr    : TVecByteRec;
     Bits      : TVecByteRec;
     i, j      : longint;
     PlanarVec : PVectorB;
begin
  GetMem(PlanarVec, PixelWidth * SizeOf(byte));
  Count := BytesToRead;
  if (ImageOfs = ImageSize)
  then ImageOfs := ImageSize - FLongWidth;

  Start.Long := 0;
  Bits.Ptr   := FDibBits;
  if (Bits.Ptr <> nil) and (PlanarVec <> Nil)
  then begin
       while (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + ImageOfs;

          if (Count > PixelWidth)
          then Count := PixelWidth;

          if IsRGB
          then begin
               try
                 case (FStripNr mod 3) of // 01.01.2001, was: case FStripNr of
                 0 : begin // Red
                       FStream.Read(PlanarVec^, PixelWidth);
                       j := 2;
                       for i := 0 to (PixelWidth - 1)
                       do begin
                          PVectorB(ToAddr.Ptr)^[j] := PlanarVec^[i];
                          inc(j, 3);
                       end;
                     end;
                 1 : begin // Green
                       FStream.Read(PlanarVec^, PixelWidth);
                       j := 1;
                       for i := 0 to (PixelWidth - 1)
                       do begin
                          PVectorB(ToAddr.Ptr)^[j] := PlanarVec^[i];
                          inc(j, 3);
                       end;
                     end;
                 2 : begin // Blue
                       FStream.Read(PlanarVec^, PixelWidth);
                       j := 0;
                       for i := 0 to (PixelWidth - 1)
                       do begin
                          PVectorB(ToAddr.Ptr)^[j] := PlanarVec^[i];
                          inc(j, 3);
                       end;
                     end;
                 end;
               except
               end;
          end;
          ImageOfs := ImageOfs - FLongWidth;
          Start.Long := Start.Long + Count;
          Count := BytesToRead - Start.Long;
       end;
  end;
  if (PlanarVec <> Nil)
  then FreeMem(PlanarVec, PixelWidth * SizeOf(byte));
end; // TmcmTIFFImage.LoadPlanarData.


procedure TmcmTIFFImage.LoadChunkyData(    Stream       : TStream;
                                       var ImageOfs     : longint;
                                           ImageSize    : longint;
                                           BytesToRead  : longint;
                                           IsRGB        : boolean);
type T24RGB = array[0..0] of TRGBTriple;
     T32RGB = array[0..0] of TRGBQuad;
var  Count      : Longint;
     Start      : TVecPointerRec;
     ToAddr     : TVecPointerRec;
     Bits       : TVecPointerRec;
     RVal, BVal : byte;
     i, n       : longint;
begin
  Count := BytesToRead;
  if (ImageOfs = ImageSize)
  then ImageOfs := ImageSize - FLongWidth;
  Start.Long := 0;
  Bits.Ptr   := FDibBits;
  if (Bits.Ptr <> nil)
  then begin
       while (ImageOfs >= 0) and (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + ImageOfs;
          if (longword(Count) > FRowWidth)
          then Count := FRowWidth;

          case FSamplePerPixel of
          3 : begin
                FStream.Read(ToAddr.Ptr^, FRowWidth);
                try
                  n := (FRowWidth div 3) - 1;
                  for i := 0 to n
                  do begin
                     RVal := T24RGB(ToAddr.Ptr^)[i].RGBtblue;
                     // This line generates an access violation.
                     // TARGB(ToAddr.Ptr^)[i].RGBtblue := TARGB(ToAddr.Ptr^)[i].RGBtred;
                     // Therefore GVal is introduced, which does not generate the AV !?!
                     BVal := T24RGB(ToAddr.Ptr^)[i].RGBtred;
                     T24RGB(ToAddr.Ptr^)[i].RGBtblue := BVal;
                     T24RGB(ToAddr.Ptr^)[i].RGBtred  := RVal;
                  end;
                except
                end;
              end;
          4 : begin
                FStream.Read(ToAddr.Ptr^, FRowWidth);
                try
                  n := (FRowWidth div 4) - 1;
                  for i := 0 to n
                  do begin
                     RVal := T32RGB(ToAddr.Ptr^)[i].rgbBlue;
                     BVal := T32RGB(ToAddr.Ptr^)[i].rgbRed;
                     T32RGB(ToAddr.Ptr^)[i].rgbBlue := BVal;
                     T32RGB(ToAddr.Ptr^)[i].rgbRed  := RVal;
                  end;
                except
                end;
              end;
          else FStream.Read(ToAddr.Ptr^, FRowWidth);
          end;
          ImageOfs := ImageOfs - FLongWidth;
          Start.Long := Start.Long + Count;
          Count := BytesToRead - Start.Long;
       end;
  end;
end; // TmcmTIFFImage.LoadChunkyData.


procedure TmcmTIFFImage.SaveChunkyData(Stream       : TStream;
                                       BitsByteSize : longint;
                                       cWidth       : longint;
                                       PixelWidth   : longint;
                                       IsRGB        : boolean);
type TARGB     = array[0..0] of TRGBTriple;
     TARGBF    = array[0..0] of TRGBQUAD;
     TAByte    = array[0..0] of byte;

var  Count      : Longint;
     Start      : TVecPointerRec;
     CDown      : TVecPointerRec;
     ToAddr     : TVecPointerRec;
     Bits       : TVecPointerRec;
     i          : longint;
     pWData     : Pointer; //^TARGB;
begin
  Start.Long := 0;
  Count      := BitsByteSize;
  CDown.Long := BitsByteSize - cWidth;
  Bits.Ptr   := FDibBits;

  if IsRGB
  then begin
       GetMem(pWData, cWidth * SizeOf(Byte));
       if (pWData = Nil)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;
  end
  else pWData := Nil;

  if (Bits.Ptr <> Nil)
  then begin
       while (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + CDown.Long;
          if (Count > cWidth)
          then Count := cWidth;
          if IsRGB
          then begin
               try
                 // Swap B-G-R (Windwos bitmap storage format) to R-G-B
                 case FDibInfo.bmiHeader.biBitCount of
                 24 : begin
                        for i := 0 to (PixelWidth - 1)
                        do begin
                           TARGB(pWData^)[i].RGBtred   := TARGB(ToAddr.Ptr^)[i].RGBtBlue;
                           TARGB(pWData^)[i].RGBtgreen := TARGB(ToAddr.Ptr^)[i].RGBtGreen;
                           TARGB(pWData^)[i].RGBtblue  := TARGB(ToAddr.Ptr^)[i].RGBtRed;
                        end;
                      end;
                 32 : begin
                        for i := 0 to (PixelWidth - 1)
                        do begin
                           TARGBF(pWData^)[i].rgbRed   := TARGBF(ToAddr.Ptr^)[i].rgbBlue;
                           TARGBF(pWData^)[i].rgbGreen := TARGBF(ToAddr.Ptr^)[i].rgbGreen;
                           TARGBF(pWData^)[i].rgbBlue  := TARGBF(ToAddr.Ptr^)[i].rgbRed;
                           TARGBF(pWData^)[i].rgbReserved := TARGBF(ToAddr.Ptr^)[i].rgbReserved;
                        end;
                      end;
                 end;
               except
               end;
               FStream.Write(pWData^, FRowWidth);
          end
          else FStream.Write(ToAddr.Ptr^, FRowWidth);

          CDown.Long := CDown.Long - Count;
          Start.Long := Start.Long + Count;
          Count := BitsByteSize - Start.Long;
       end;
  end;

  if (pWData <> Nil)
  then FreeMem(pWData, cWidth * SizeOf(Byte));
end; // TmcmTIFFImage.SaveChunkyData.


procedure TmcmTIFFImage.LoadFromStream(Stream : TStream);
var BufferSize     : longword;
    i, DataCount   : longword;
    SaveBigEndian  : boolean;
    SaveJPEGStream : TmcmBufferStream;
begin
  FDibHandle := 0;
  FNrOfStrip := 0;
  FJPEGImage := Nil;
  SaveJPEGStream := Nil;
  if Assigned(Stream)
  then begin
       if (FStream.Source <> Stream) or (Stream.Position = 0)
       then FNumImages := 1;
       FStream.Source := Stream;
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              //FStream.Position := 0;
              if (FDibInfo <> nil)
              then begin
                   // Read TIFF header & Tags.
                   ReadHeader(Stream);
                   SaveBigEndian := FStream.BigEndian;

                   if (FError = EC_OK) and
                      (FDibInfo.bmiHeader.biBitCount in [1, 4, 8, 24, 32])
                   then begin
                        case FSamplePerPixel of
                        3, 4 : FIsRGB := True;
                        else FIsRGB := False;
                        end;

                        if (FDibInfo.bmiHeader.biBitCount in [1, 4, 8])
                        then ReadPalette(Stream);

                        // Predictor is used with LZW compression.
                        if (FCompress <> CP_NOCOMP)
                        then begin
                             // Predictor = 2 -> Difference filter:
                             //                  Pixel[x,y] - Pixel[x-1,y].
                             FDiffPredictor := (FTags.Item[FTags.Index[TAG_PREDICTOR]].ValOffset = 2);
                        end
                        else FDiffPredictor := False;

                        //------------------------------------------------------
                        // Compression.
                        FCompObj := Nil;
                        case LoWord(FTags.Item[FTags.Index[TAG_COMPRESSION]].ValOffset) of
                        // No compression.
                        TIFCP_NONE          : ;
                        // CCITT modified Huffman RLE
                        TIFCP_CCITTRLE      : begin
                                                FCompObj := TmcmImageModifiedHuffman.Create;
                                                TmcmImageModifiedHuffman(FCompObj).FillOrder := (FFillOrder <> 1);
                                              end;
                        // modified Huffman with word alignment
                        //TIFCP_CCITTRLEW     : ;
                        // CCITT Group 3 fax encoding
                        TIFCP_CCITTFAX3     : begin
                                                case FCompress of
                                                CP_CCITTGROUP3      : begin
                                                                        FCompObj := TmcmImageCCITT.Create;
                                                                        TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MH;
                                                                        TmcmImageCCITT(FCompObj).FillOrder := (FFillOrder <> 1);
                                                                      end;
                                                CP_CCITTGROUP3_2D   : begin
                                                                        FCompObj := TmcmImageCCITT.Create;
                                                                        TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MR;
                                                                        TmcmImageCCITT(FCompObj).FillOrder := (FFillOrder <> 1);
                                                                      end;
                                                end;
                                              end;
                        // CCITT Group 4 fax encoding
                        TIFCP_CCITTFAX4     : begin
                                                FCompObj := TmcmImageCCITT.Create;
                                                TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MMR;
                                                TmcmImageCCITT(FCompObj).FillOrder := (FFillOrder <> 1);
                                              end;
                        // Lempel-Ziv & Welch
                        TIFCP_LZW           : begin
                                                FCompObj := TmcmImageLZW.Create;
                                                TmcmImageLZW(FCompObj).CodeMode := LZWCM_TIF;
                                                TmcmImageLZW(FCompObj).BitEndian := True;
                                                TmcmImageLZW(FCompObj).CodeSize := 8;
                                              end;
                        // JPEG (Old version 6.0)
                        TIFCP_OJPEG         : begin
                                                i := FTags.Index[TAG_JPEGINTERCHANGEFORMAT];
                                                if (i > 0)
                                                then begin
                                                     // We'll read the entire JPEG image right here!
                                                     Stream.Position := FTags.Item[FTags.Index[TAG_JPEGINTERCHANGEFORMAT]].FValOffset;
                                                     FJPEGImage := TmcmJPEGImage.Create;
                                                     FJPEGImage.FPhotometric := FPhotometric;
                                                     FJPEGImage.LoadFromStream(Stream);
                                                     FError := FJPEGImage.FError;
                                                     FDibHandle  := FJPEGImage.DibHandle;
                                                     FCompress   := FJPEGImage.Compression;
                                                     FQuality    := FJPEGImage.Quality;
                                                     FInterlaced := FJPEGImage.Interlaced;

                                                     FJPEGImage.Free;
                                                     FJPEGImage := Nil;
                                                     Clear;
                                                     Exit; // Leave TIFF read
                                                end
                                                else FError := EC_COMPRESSION;
                                              end;
                        // JPEG DCT compression (New version, latter than 6.0)
                        TIFCP_JPEG          : begin
                                                FJPEGImage := TmcmJPEGImage.Create;
                                                SaveJPEGStream := FJPEGImage.FStream;
                                                FJPEGImage.FStream := FStream;
                                                FJPEGImage.ReadInitialize;
                                                FJPEGImage.FPhotometric := FPhotometric;

                                                // Read JPEGTables
                                                i := FTags.Index[TAG_JPEGTABLES];
                                                if (i > 0)
                                                then begin
                                                     FStream.Position := FTags.Item[FTags.Index[TAG_JPEGTABLES]].FValOffset;
                                                     FStream.BigEndian := True;
                                                     FError := FJPEGImage.ReadMarkers(True);
                                                     FStream.BigEndian := SaveBigEndian;
                                                end;
                                              end;
                        (*
                        // New id, same as CP_DEFLATE
                        TIFCP_ADOBE_DEFLATE : ;
                        TIFCP_NEXT          : ;
                        *)
                        // Macintosh RLE
                        TIFCP_PACKBITS      : begin
                                                FCompObj := TmcmImagePackBits.Create;
                                              end;
                        (*
                        // ThunderScan RLE
                        TIFCP_THUNDERSCAN   : ;
                        // codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
                        TIFCP_IT8CTPAD      : ;
                        TIFCP_IT8LW         : ;
                        TIFCP_IT8MP         : ;
                        TIFCP_IT8BL         : ;
                        // compression codes 32908-32911 are reserved for Pixar
                        TIFCP_PIXARFILM     : ;
                        TIFCP_PIXARLOG      : ;
                        // Deflate compression (LZ77)
                        TIFCP_DEFLATE       : ;
                        // compression 32947 is reserved for Oceana Matrix (dev@oceana.com)
                        TIFCP_DCS           : ;
                        TIFCP_JBIG          : ;
                        *)
                        // Else compression isn't supported.
                        else FError := EC_COMPRESSION;
                        end;

                        if (FError = EC_OK)
                        then begin
                             //------------------------------------------------------
                             // Create Device Independent Bitmap.
                             if CreateDIB
                             then begin
                                  if Assigned(FCompObj)
                                  then begin
                                       FCompObj.Image := FDibHandle;
                                       FMemStream.Clear;
                                  end;

                                  if Assigned(FJPEGImage)
                                  then begin
                                       // Set-up TmcmJPEGImage to use
                                       // TmcmTIFFImage's image data
                                       FJPEGImage.FDIBSection := FDIBSection;
                                       FJPEGImage.FDibHandle  := FDibHandle;
                                       FJPEGImage.FDibBits    := FDibBits;
                                       // Copy content of FDibInfo, not the pointer.
                                       FJPEGImage.FDibInfo^   := FDibInfo^;
                                       FJPEGImage.FLongWidth  := FLongWidth;
                                  end;

                                  if (FStripSize = 0)
                                  then begin
                                       FStripNr  := 0;
                                       FImageOfs := FDibInfo^.bmiHeader.biSizeImage;
                                       GotoStripOfs(Stream, FStripNr, FStripSize, FNrOfStrip);

                                       while ((FStripNr < FNrOfStrip) and (FError = EC_OK))
                                       do begin
                                          if (FStripSize = 0)
                                          then FStripSize := FDibInfo^.bmiHeader.biSizeImage;

                                          case FCompress of
                                          CP_NOCOMP   : begin
                                                          if (FPlanarConfig = TIFPC_PLANAR)
                                                          then LoadPlanarData(Stream,
                                                                              FImageOfs,
                                                                              FDibInfo^.bmiHeader.biSizeImage,
                                                                              FStripSize,
                                                                              FDibInfo^.bmiHeader.biWidth,
                                                                              FIsRGB)
                                                          else LoadChunkyData(Stream,
                                                                              FImageOfs,
                                                                              FDibInfo^.bmiHeader.biSizeImage,
                                                                              FStripSize,
                                                                              FIsRGB);
                                                        end;
                                          CP_JPEG_STD : begin
                                                          FStream.BigEndian := True;
                                                          FError := FJPEGImage.ReadMarkers(True);
                                                          FStream.BigEndian := SaveBigEndian;
                                                        end;
                                          else begin
                                               if (FStripSize > FMemStream.Size)
                                               then FMemStream.SetSize(FStripSize);

                                               BufferSize := FStream.Read(FMemStream.Memory^, FStripSize{FMemStream.Size});
                                               if Assigned(FCompObj)
                                               then FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                          end;
                                          end;

                                          inc(FStripNr);
                                          if (FStripNr < FNrOfStrip)
                                          then GotoStripOfs(Stream, FStripNr, FStripSize, FNrOfStrip);
                                       end;
                                  end
                                  else begin
                                       FImageOfs := FDibInfo^.bmiHeader.biSizeImage;
                                       LoadChunkyData(Stream,
                                                      FImageOfs,
                                                      FDibInfo^.bmiHeader.biSizeImage,
                                                      FDibInfo^.bmiHeader.biSizeImage,
                                                      FIsRGB);
                                  end;
                             end;
                        end;

                        if Assigned(FCompObj)
                        then FCompObj.Free;

                        if Assigned(FJPEGImage)
                        then begin
                             FJPEGImage.CombineComponents;
                             FJPEGImage.FStream := SaveJPEGStream;
                        end;

                        if (FCompress = CP_LZW)
                        then begin
                             if FDiffPredictor // Used with LZW compression.
                             then ReadPredictor
                             else if FIsRGB
                                  then SwapByteRB;
                        end;
                   end
                   else begin
                        if Not(FDibInfo.bmiHeader.biBitCount in [1, 4, 8, 24, 32]) and (FError = EC_OK)
                        then FError := EC_BITDEPTH;
                   end;
              end;
         end;
       except
       end;
       Clear; // Free allocated memory.
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN; // File not open.
end; // TmcmTIFFImage.LoadFromStream.


procedure TmcmTIFFImage.SaveToStream(Stream : TStream);
var Buffer        : Pointer;
    BufferSize    : longword;
    DataCount     : longword;
    Count         : longword;
    SavePos       : longint;
    SaveBigEndian : boolean;
begin
  if GetDibData
  then begin
       FJPEGImage := Nil;
       FNrOfStrip := 0;
       if Assigned(Stream)
       then begin
            FStream.Source := Stream;
            FStream.Mode := RWM_WRITE;
            FStreamStart := FStream.Position;
            FPlanarConfig := TIFPC_CHUNKY;

            // Get photometric interpretation of image.
            FPhotometric := GetPhotometric;

            // Convert bi-level images so that white is zero.
            FInvertLogPal := False;
            if FSafeBiLevel and (FDibInfo^.bmiHeader.biBitCount = 1)
            then begin
                 // Check if black is zero, if so invert image
                 if (FPhotometric = PM_GREY)
                 then begin
                      InvertImage;
                      FInvertLogPal := True;
                      FPhotometric := PM_INVGREY; // GetPhotometric;
                 end;
            end;

            // Write File header to file.
            WriteHeader(Stream);

            case FCompress of
            // No compression.
            CP_NOCOMP             : FCompObj := Nil;
            // CCITT modified Huffman RLE
            CP_MODIFIED_HUFFMAN   : FCompObj := TmcmImageModifiedHuffman.Create;
            {
            // modified Huffman with word alignment
            CP_MODIFIED_HUFFMAN_W : ;
            }
            // CCITT Group 3 1-Dimensional fax encoding
            CP_CCITTGROUP3        : begin
                                      FCompObj := TmcmImageCCITT.Create;
                                      //TmcmImageCCITT(FCompObj).ByteAligned  := False;
                                      //TmcmImageCCITT(FCompObj).FillOrder    := False;
                                      //TmcmImageCCITT(FCompObj).Uncompressed := False;
                                      TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MH;
                                    end;
            // CCITT Group 3 2-Dimensional fax encoding
            CP_CCITTGROUP3_2D     : begin
                                      FCompObj := TmcmImageCCITT.Create;
                                      TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MR;
                                    end;
            // CCITT Group 4 fax encoding
            CP_CCITTGROUP4        : begin
                                      FCompObj := TmcmImageCCITT.Create;
                                      TmcmImageCCITT(FCompObj).CCITTScheme := CCITT_MMR;
                                    end;
            // Lempel-Ziv & Welch
            CP_LZW                : begin
                                      FCompObj := TmcmImageLZW.Create;
                                      TmcmImageLZW(FCompObj).CodeMode := LZWCM_TIF;
                                      TmcmImageLZW(FCompObj).BitEndian := True;
                                      TmcmImageLZW(FCompObj).CodeSize := 8;
                                      TmcmImageLZW(FCompObj).DiffPredictor := FDiffPredictor;
                                    end;
            // 6.0 JPEG (Old version)
            //CP_OJPEG              : ; // Do not implement - old standard.
            // JPEG DCT compression (New version)
            CP_JPEG_STD           : begin
                                      FJPEGImage.McuRows := 1; // There is 1 MCU per strip.
                                    end;
            {
            // new id but same as COMPRESSION_DEFLATE
            CP_ADOBE_DEFLATE      : ;
            // Deflate compression (LZ77)
            CP_DEFLATE            : ;
            }
            // Macintosh RLE
            CP_PACKBITS           : FCompObj := TmcmImagePackBits.Create;
            {
            // ThunderScan RLE
            CP_THUNDERSCAN   : ;
            }
            // Else compression isn't supported.
            else FError := EC_COMPRESSION;
            end;

            // Write Data to file.
            if (FBitCount >= 24)
            then FIsRGB := True;

            if (FCompress = CP_NOCOMP)
            then begin
                 with FDibInfo^.bmiHeader
                 do SaveChunkyData(Stream, biSizeImage, FLongWidth, biWidth, FIsRGB);
            end
            else begin
                 if Assigned(FCompObj)
                 then begin
                      FCompObj.Image := FDibHandle;
                      Count := 0;
                      FStripNr := 0;
                      while (FError = EC_OK) and (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                      do begin
                         try
                           FStripDskOfs^[FStripNr] := FStream.Position;
                           DataCount := FRowsPerStrip^[FStripNr] * longword(FLongWidth);
                           FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                           if (FError = EC_OK)
                           then begin
                                WriteStreamData(Stream, Buffer, BufferSize);
                                inc(Count, DataCount);
                                FStripSizes^[FStripNr]  := BufferSize;
                                inc(FStripNr);
                           end;
                         except
                           FError := EC_UNKNOWN;
                         end;
                      end;

                      // Update strip file offset and size.
                      if (FError = EC_OK)
                      then begin
                           FStream.Flush;
                           SavePos := FStream.Position;
                           WriteStripOffsets(Stream);
                           FStream.Flush;
                           WriteStripCounts(Stream);
                           FStream.Position := SavePos;
                      end;
                      FCompObj.Free;
                 end;

                 if Assigned(FJPEGImage)
                 then begin
                      Count := 0;
                      FStripNr := 0;
                      FStream.Flush;
                      SaveBigEndian := FStream.BigEndian;
                      FStream.BigEndian := True;
                      while (FError = EC_OK) and (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                      do begin
                         try
                           FStripDskOfs^[FStripNr] := FStream.Position;
                           DataCount := FRowsPerStrip^[FStripNr] * longword(FLongWidth);
                           BufferSize := FJPEGImage.WriteTIFFData(FStream);
                          // FStream.Flush;
                          // FStream.Position := Stream.Position;
                           FError := FJPEGImage.Error;
                           if (FError = EC_OK)
                           then begin
                                inc(Count, DataCount);
                                FStripSizes^[FStripNr] := BufferSize;
                                inc(FStripNr);
                           end;
                         except
                           FError := EC_UNKNOWN;
                         end;
                      end;
                      FStream.BigEndian := SaveBigEndian;

                      // Update strip file offset and size.
                      if (FError = EC_OK)
                      then begin
                           FStream.Flush;
                           SavePos := FStream.Position;
                           WriteStripOffsets(Stream);
                           FStream.Flush;
                           WriteStripCounts(Stream);
                           FStream.Position := SavePos;
                      end;
                 end;
            end;
            Clear; // Free allocated memory.
            if (FError = EC_OK)
            then FStream.Flush;
            if FInvertLogPal
            then InvertImage;
       end
       else FError := EC_FILENOTOPEN; // File not open.
  end;
end; // TmcmTIFFImage.SaveToStream.


{$IFDEF MCM_USE_NEF}
//------------------------------------------------------------------------------
// TmcmNEFImage
//------------------------------------------------------------------------------

class function TmcmNEFImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB15,IF_RGB16,IF_RGB24,IF_RGBA32];
end; // TmcmNEFImage.GetColorFormats.


constructor TmcmNEFImage.Create;
begin
  Inherited Create;
  FNEFTags  := Nil;
  FEvenLine := Nil;
  FOddLine  := Nil;
end; // TmcmNEFImage.Create.


procedure TmcmNEFImage.Clear;
begin
  if Assigned(FNEFTags)
  then FNEFTags.Free;
  FNEFTags := Nil;
  if Assigned(FEvenLine)
  then FreeMem(FEvenLine);
  FEvenLine := Nil;
  if Assigned(FOddLine)
  then FreeMem(FOddLine);
  FOddLine  := Nil;
end; // TmcmNEFImage.Clear.


procedure TmcmNEFImage.ReadHeader(Stream : TStream);
var i         : integer;
    RedBits   : word;
    GreenBits : word;
    BlueBits  : word;
begin
  Inherited ReadHeader(Stream);

  i := FTags.Index[TAG_SUBIDF];
  if (i > 0)
  then begin
       FNEFTags := TmcmTAGMgr.Create(FStream, FImageInfo, False);
       FStream.Position := FTags.Item[i].ValOffset;
       FNEFTags.ReadTags(0);
  end
  else FError := EC_BADFORMAT;

  // Extract information from TAG's
  if (FError = EC_OK)
  then begin
       i := FNEFTags.Index[TAG_BITSPERSAMPLE];
       if (FNEFTags.Item[i].Length = 1)
       then begin
            FBitPerSample := LoWord(FNEFTags.Item[i].ValOffset);
            {
            if (FBitPerSample > 8)
            then FBitPerSample := 8;
            }
       end
       else begin
            FStream.Position := FNEFTags.Item[i].ValOffset;
            RedBits   := FStream.ReadWord;
            GreenBits := FStream.ReadWord;
            BlueBits  := FStream.ReadWord;
            if (RedBits   <> 8) or
               (GreenBits <> 8) or
               (BlueBits  <> 8)
            then ; //FError := EC_UNKNOWNFORMAT;

            FNEFTags.Item[i].Length := 1;
            FNEFTags.Item[i].ValOffset := 8;
            FBitPerSample := 8; // Bits per channel.
       end;
       FSamplePerPixel := LoWord(FNEFTags.Item[FNEFTags.Index[TAG_SAMPLESPERPIXEL]].ValOffset);

       with FDibInfo^.bmiHeader
       do begin
          biWidth  := FNEFTags.Item[FNEFTags.Index[TAG_IMAGEWIDTH]].ValOffset;
          biHeight := FNEFTags.Item[FNEFTags.Index[TAG_IMAGELENGTH]].ValOffset;
       end;

       FPhotometric  := FNEFTags.Item[FNEFTags.Index[TAG_PHOTOMETRIC]].ValOffset;
       if (FPhotometric <> PM_CFA)
       then FError := EC_BADCOLORFORMAT;

       case LoWord(FNEFTags.Item[FNEFTags.Index[TAG_COMPRESSION]].ValOffset) of
       TIFCP_NIKON_CFA     : ;
       else FError := EC_COMPRESSION;
       end;

       if (FError = EC_OK)
       then begin
            with FDibInfo^.bmiHeader
            do begin
               biPlanes       := 1;
               biBitCount     := 24; //FBitPerSample * FSamplePerPixel;
               biClrUsed      := 0;
               biClrImportant := 0;

               FRowWidth       := RowWidth;
               FPlanarConfig   := LoWord(FNEFTags.Item[FNEFTags.Index[TAG_PLANARCONFIG]].ValOffset);

               FNEFTags.TagsToImageInfo;
               if Assigned(FImageInfo)
               then begin
                    FImageInfo.Units := UN_METERS;
                    biXPelsPerMeter  := Round(FImageInfo.XResolution);
                    biYPelsPerMeter  := Round(FImageInfo.YResolution);
               end;
            end;

            if (FError = EC_TIFFTAG)
            then FError := EC_OK;

            i := FNEFTags.Index[TAG_FILLORDER];
            FFillOrder := LoWord(FNEFTags.Item[i].ValOffset);

            // Get number of strips and strip table.
            i := FNEFTags.Index[TAG_STRIPOFFSETS];
            if (i > 0)
            then begin
                 FNrOfStrip := FNEFTags.Item[i].Length;
                 ReadStripTable(FNEFTags);
                 InitRowsPerStrip(FNEFTags, FDibInfo.bmiHeader.biHeight);
            end;

            // Clear Tag errors occured in this "IF" sentence.
            // EC_TIFFTAG errors in this "IF" sentence are not critical!
            if (FError = EC_TIFFTAG)
            then FError := EC_OK;
       end;
  end;
end; // TmcmNEFImage.ReadHeader.


procedure TmcmNEFImage.LoadCFAData(var ImageOfs    : longint;
                                       ImageSize   : longint;
                                       BytesToRead : longint);
type T3Bytes = record
     a, b, c : byte;
     end;
     P3Bytes = ^T3Bytes;
     T24RGB = array[0..0] of TRGBTriple;
var i, j, n, p : longint;
    BytesPerLine : integer;
    wa, wb, wc   : word;

    Count      : Longint;
    Start      : TVecPointerRec;
    ToAddr     : TVecPointerRec;
    ToOld      : TVecPointerRec;
    Bits       : TVecPointerRec;
    Dummy      : byte;
    Line       : integer;
begin
  BytesPerLine := Round(3 * FDibInfo^.bmiHeader.biWidth / 2);

  Count := BytesToRead;
  if (ImageOfs = ImageSize)
  then ImageOfs := ImageSize - FLongWidth;
  Start.Long := 0;
  Bits.Ptr   := FDibBits;
  if (Bits.Ptr <> nil)
  then begin
       Line := 1;
       n := 0;

       ToOld.Long := Bits.Long + ImageOfs;

       while (ImageOfs >= 0) and (Count > 0)
       do begin
          ToAddr.Long := Bits.Long + ImageOfs;

          if (longword(Count) > BytesPerLine)
          then Count := BytesPerLine;


          i := 0;
          j := 0;
          wa := 0;
          wb := 0;
          wc := 0;
          while (i < FDibInfo^.bmiHeader.biWidth)
          do begin
             FStream.Read(wa, 1);
             FStream.Read(wb, 1);
             FEvenLine^[j] := (wa shl 4) or (wb shr 4);
             inc(j);
             FStream.Read(wc, 1);
             wb := (wb and $0F) shl 8;
             FEvenLine^[j] := wb or wc;
             inc(j);
             inc(n);
             inc(i, 2);
             if (n = 5)
             then begin
                  FStream.Read(Dummy, 1);
                  n := 0;
             end;
          end;

          for i := 0 to (16 - ((BytesPerLine) mod 16)) - 1
          do begin
             FStream.Read(Dummy, 1);
             if ((i mod 3) = 0)
             then begin
                  inc(n);
                  if (n = 5)
                  then begin
                       FStream.Read(Dummy, 1);
                       n := 0;
                  end;
             end;
          end;

          i := 0;
          try
            if Odd(Line)
            then begin
                 CopyMemory(ToAddr.Ptr, ToOld.Ptr, FLongWidth);

                 while (i < FDibInfo.bmiHeader.biWidth)
                 do begin
                    T24RGB(ToAddr.Ptr^)[i].rgbtGreen := byte(FEvenLine^[i] shr 4);
                    inc(i);
                    T24RGB(ToAddr.Ptr^)[i].rgbtRed := Trunc(2.25882352 * FEvenLine^[i]) shr 4;

                    T24RGB(ToAddr.Ptr^)[i].rgbtGreen := T24RGB(ToAddr.Ptr^)[i-1].rgbtGreen;
                    T24RGB(ToAddr.Ptr^)[i-1].rgbtRed := T24RGB(ToAddr.Ptr^)[i].rgbtRed;
                    inc(i);
                 end;
            end
            else begin
                 CopyMemory(ToAddr.Ptr, ToOld.Ptr, FLongWidth);

                 while (i < FDibInfo.bmiHeader.biWidth)
                 do begin
                    T24RGB(ToAddr.Ptr^)[i].rgbtBlue := Trunc(1.76078431 * FEvenLine^[i]) shr 4;
                    inc(i);
                    T24RGB(ToAddr.Ptr^)[i].rgbtGreen := byte(FEvenLine^[i] shr 4);

                    T24RGB(ToAddr.Ptr^)[i].rgbtBlue := T24RGB(ToAddr.Ptr^)[i-1].rgbtBlue;
                    T24RGB(ToAddr.Ptr^)[i-1].rgbtGreen := T24RGB(ToAddr.Ptr^)[i].rgbtGreen;

                    inc(i);
                 end;
            end;
          except
          end;
          inc(Line);
          ToOld.Long := ToAddr.Long;

          ImageOfs := ImageOfs - FLongWidth;
          Start.Long := Start.Long + FLongWidth;

          BytesToRead := BytesToRead - Count;
          Count := BytesToRead;
       end;
  end;
end; // TmcmNEFImage.LoadCFAData.


procedure TmcmNEFImage.LoadFromStream(Stream : TStream);
var BytesPerLine : integer;
begin
  // To read the thumbnail, simply call Inherited LoadFromStream.

  FDibHandle := 0;
  FNrOfStrip := 0;
  if Assigned(Stream)
  then begin
       if (FStream.Source <> Stream) or (Stream.Position = 0)
       then FNumImages := 1;
       FStream.Source := Stream;
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              if (FDibInfo <> nil)
              then begin
                   // Read TIFF header & Tags.
                   ReadHeader(Stream);

                   if (FError = EC_OK) and
                      (FDibInfo.bmiHeader.biBitCount in [24])
                   then begin
                        BytesPerLine := Round(3 * FDibInfo^.bmiHeader.biWidth / 2);

                        GetMem(FEvenLine, 2 * BytesPerLine);
                        GetMem(FOddLine, 2 * BytesPerLine);

                        //------------------------------------------------------
                        // Create Device Independent Bitmap.
                        if CreateDIB
                        then begin
                             if (FStripSize = 0)
                             then begin
                                  FStripNr  := 0;
                                  FImageOfs := FDibInfo^.bmiHeader.biSizeImage - FLongWidth;
                                  GotoStripOfs(Stream, FStripNr, FStripSize, FNrOfStrip);

                                  while ((FStripNr < FNrOfStrip) and (FError = EC_OK))
                                  do begin
                                     if (FStripSize = 0)
                                     then FStripSize := FDibInfo^.bmiHeader.biSizeImage;


                                     LoadCFAData(FImageOfs,
                                                 FDibInfo^.bmiHeader.biHeight * BytesPerLine,
                                                 FStripSize);

                                     inc(FStripNr);
                                     if (FStripNr < FNrOfStrip)
                                     then GotoStripOfs(Stream, FStripNr, FStripSize, FNrOfStrip);
                                  end;
                             end;
                        end;
                   end
                   else begin
                        if Not(FDibInfo.bmiHeader.biBitCount in [1, 4, 8, 24, 32]) and (FError = EC_OK)
                        then FError := EC_BITDEPTH;
                   end;
              end;
         end;
       except
       end;
       Clear; // Free allocated memory.
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN; // File not open.
end; // TmcmNEFImage.LoadFromStream.


procedure TmcmNEFImage.SaveToStream(Stream : TStream);
begin
  FError := EC_BADFORMAT;
end; // TmcmNEFImage.SaveToStream.
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmTargaImage
//------------------------------------------------------------------------------

class function TmcmTargaImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_GREY8,IF_PAL8,IF_RGB15,IF_RGB16,IF_RGB24,IF_RGBA32];
end; // TmcmTargaImage.GetColorFormats.


class function TmcmTargaImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := Inherited GetCompressionFromColor(ImageFormat, Compress);
  if (ImageFormat in [IF_NONE,IF_GREY8..IF_RGBA32])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE_TARGA;
       inc(Count);
  end;
  Result := Count;
end; // TmcmTargaImage.GetCompressionFromColor.


class function TmcmTargaImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP    : Result := resCP_NOCOMP;
  CP_RLE_TARGA : Result := resCP_RLE8_24;
  else Result := '';
  end;
end; // TmcmTargaImage.GetCompressionName.


constructor TmcmTargaImage.Create;
begin
  Inherited Create;
  FImageID[0] := #0;
end; // TmcmTargaImage.Create.


function TmcmTargaImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := Inherited IsFormatValid(Stream);
  if (FError <> EC_FILENOTOPEN)
  then begin
       try
         FStream.Read(FTargaHeader, SizeOf(TTargaHeader));
         if (FTargaHeader.ImageType in [TARGA_EMPTY,
                                        TARGA_INDEXED,
                                        TARGA_TRUECOLOR,
                                        TARGA_BW,
                                        TARGA_INDEXED_RLE,
                                        TARGA_TRUECOLOR_RLE,
                                        TARGA_BW_RLE])
         then begin
              if (FTargaHeader.ColorMapType in [TARGA_NOCOLORMAP, TARGA_COLORMAP])
              then if (FTargaHeader.PixelDepth in [1, 4, 8, 16, 24, 32])
                   then FError := EC_OK;
         end;
       except
         FError := EC_READFROMFILE;
       end;
  end;
  Result := FError;
end; // TmcmTargaImage.IsFormatValid.


function TmcmTargaImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK) // Read header and check format.
         then begin
              if (FTargaHeader.IDLength > 0)
              then FStream.Read(FImageID, FTargaHeader.IDLength * SizeOf(byte));

              if (FDibInfo <> Nil)
              then begin
                   with FDibInfo^.bmiHeader
                   do begin
                      biWidth         := FTargaHeader.Width;
                      biHeight        := FTargaHeader.Height;
                      biPlanes        := 1;
                      biBitCount      := FTargaHeader.PixelDepth;
                      biCompression   := BI_RGB;
                      biXPelsPerMeter := 0;
                      biYPelsPerMeter := 0;
                      biClrUsed       := 0;
                      biClrImportant  := 0;
                   end;

                   // Get compression.
                   if (FTargaHeader.ImageType in [TARGA_INDEXED_RLE,
                                                  TARGA_TRUECOLOR_RLE,
                                                  TARGA_BW_RLE])
                   then FCompress := CP_RLE_TARGA
                   else FCompress := CP_NOCOMP;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmTargaImage.ReadHeader.


function TmcmTargaImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
var i, j     : integer;
    ColorW   : word;
begin
  try
    ZeroMemory(@FDibInfo^.bmiColors, 256 * SizeOf(TRGBQuad));

    // Read colour map.
    FMemStream.Size := (FTargaHeader.ColorMapEntrySize div 8) * FTargaHeader.ColorMapLength;
    FStream.Read(FMemStream.Memory^, FMemStream.Size);

    // Temporary fix for FDibInfo^.bmiColors, to ensure that read palette will
    // not exceed 256 entries.
    if (FTargaHeader.ColorMapLength > 256)
    then FTargaHeader.ColorMapLength := 256;

    case FTargaHeader.ColorMapEntrySize of
    15,
    16 : begin
           for i := FTargaHeader.ColorMapStart to (FTargaHeader.ColorMapLength - 1)
           do begin
              ColorW := PVectorW(FMemStream.Memory)[i];
              FDibInfo^.bmiColors[i].rgbRed   := (ColorW and $7C00) shr 7;
              FDibInfo^.bmiColors[i].rgbGreen := (ColorW and $03E0) shr 2;
              FDibInfo^.bmiColors[i].rgbBlue  := (ColorW and $001F) shl 3;
           end;
         end;
    24 : begin
           j := 0;
           for i := FTargaHeader.ColorMapStart to (FTargaHeader.ColorMapLength - 1)
           do begin
              FDibInfo^.bmiColors[i].rgbBlue  := PVectorB(FMemStream.Memory)[j];
              inc(j);
              FDibInfo^.bmiColors[i].rgbGreen := PVectorB(FMemStream.Memory)[j];
              inc(j);
              FDibInfo^.bmiColors[i].rgbRed   := PVectorB(FMemStream.Memory)[j];
              inc(j);
           end;
         end;
    32 : begin
           j := 0;
           for i := FTargaHeader.ColorMapStart to (FTargaHeader.ColorMapLength - 1)
           do begin
              FDibInfo^.bmiColors[i].rgbBlue  := PVectorB(FMemStream.Memory)[j];
              inc(j);
              FDibInfo^.bmiColors[i].rgbGreen := PVectorB(FMemStream.Memory)[j];
              inc(j);
              FDibInfo^.bmiColors[i].rgbRed   := PVectorB(FMemStream.Memory)[j];
              inc(j, 2);
           end;
         end;
    end;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmTargaImage.ReadPalette.


function TmcmTargaImage.ReadFooter(Stream : TStream) : TmcmErrorCode;
var CurrentPos : longint;
begin
  if (FStreamStart = 0)
  then begin
       CurrentPos := FStream.Position;
       FStream.Position := FStream.Size - SizeOf(TTargaFooter);
       FStream.Read(FTargaFooter, SizeOf(TTargaFooter));

       if (FTargaFooter.Signature = TARGA2_SIGNATURE)
       then begin // Targa file version 2.
            if (FTargaFooter.ExtensionOffset > 0)
            then begin // Read Extension area.
                 FStream.Position := FTargaFooter.ExtensionOffset;
                 ReadExtension(Stream);
            end;
       end
       else ZeroMemory(@FTargaFooter, SizeOf(TTargaFooter)); // Targa file version 1.
       FStream.Position := CurrentPos;
  end
  else FError := EC_BADFORMAT;
  Result := FError;
end; // TmcmTargaImage.ReadFooter.


function TmcmTargaImage.ReadExtension(Stream : TStream) : TmcmErrorCode;
var DT : TDateTime;
begin
  try
    if Assigned(FImageInfo)
    then begin
         FStream.Read(FTargaExtension, SizeOf(TTargaExtension));
         FImageInfo.Artist := String(FTargaExtension.AuthorName);
         FImageInfo.Description := String(FTargaExtension.AuthorComments);
         try
           DT := 0;
           if (FTargaExtension.DateTimeStamp[0] <> 0) and
              (FTargaExtension.DateTimeStamp[1] <> 0) and
              (FTargaExtension.DateTimeStamp[2] <> 0)
           then DT := EncodeDate(FTargaExtension.DateTimeStamp[2],
                                 FTargaExtension.DateTimeStamp[0],
                                 FTargaExtension.DateTimeStamp[1]);
           if (FTargaExtension.DateTimeStamp[0] <> 0) and
              (FTargaExtension.DateTimeStamp[1] <> 0) and
              (FTargaExtension.DateTimeStamp[2] <> 0)
           then DT := DT + EncodeTime(FTargaExtension.DateTimeStamp[3],
                                      FTargaExtension.DateTimeStamp[4],
                                      FTargaExtension.DateTimeStamp[5], 0);
           FImageInfo.DateTime := DT;
         except
           On E:EConvertError
           do ;
         end;
         // FTargaExtension.JobNameID
         // FTargaExtension.JobTime
         FImageInfo.Software    := String(FTargaExtension.SoftwareID);
         FImageInfo.SoftwareVersion := FTargaExtension.SoftwareVersion.VersionNo / 100;
         // FTargaExtension.KeyColor
         FImageInfo.PixelWidth  := FTargaExtension.AspectRatioW;
         if (FImageInfo.PixelWidth = 0)
         then FImageInfo.PixelWidth := 1;
         FImageInfo.PixelHeight := FTargaExtension.AspectRatioH;
         if (FImageInfo.PixelHeight = 0)
         then FImageInfo.PixelHeight := 1;
         // FTargaExtension.GammaNum
         // FTargaExtension.GammaDeNom
         // FTargaExtension.ColorCorrectOfs
         // FTargaExtension.PostStampOffset
         // FTargaExtension.ScanLineOffset
         // FTargaExtension.AttributeType
    end;
  except
    On E:EReadError
    do FError := EC_ENDOFFILE;
  end;
  Result := FError;
end; // TmcmTargaImage.ReadExtension.


procedure TmcmTargaImage.LoadFromStream(Stream : TStream);
var i, j       : integer;
    FlipVert   : boolean;
    BufferSize : longword;
    DataCount  : longword;
    Count      : longword;
begin
  FStream.Source := Stream;
  // Read Header.
  if (ReadHeader(Stream) = EC_OK)
  then begin
       // Create & load/set Palette
       case FTargaHeader.ColorMapType of
       TARGA_NOCOLORMAP : begin // No colour map included.
                            case FTargaHeader.ImageType of
                            TARGA_BW,
                            TARGA_BW_RLE : CreateGreyPalette(PM_GREY);
                            end;
                          end;
       TARGA_COLORMAP   : FError := ReadPalette(Stream);
       end;

       if (FError = EC_OK)
       then begin
            // Load image data.
            FlipVert := ((FTargaHeader.ImageDescriptor and $20) <> 0);
            if CreateDIB
            then begin
                 case FTargaHeader.ImageType of
                 TARGA_EMPTY          : ; // No image data.
                 TARGA_INDEXED,
                 TARGA_TRUECOLOR,
                 TARGA_BW             : begin
                                          FRowWidth := FDibInfo^.bmiHeader.biWidth * (FTargaHeader.PixelDepth div 8);
                                          if FlipVert
                                          then j := (FDibInfo^.bmiHeader.biHeight - 1) * FLongWidth
                                          else j := 0;
                                          for i := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                                          do begin
                                             FStream.Read(PVectorB(FDibBits)^[j], FRowWidth);
                                             if FlipVert
                                             then dec(j, FLongWidth)
                                             else inc(j, FLongWidth);
                                          end;
                                        end;
                 TARGA_INDEXED_RLE,
                 TARGA_TRUECOLOR_RLE,
                 TARGA_BW_RLE         : begin
                                          FCompObj := TmcmImageTargaRLE.Create;
                                          try
                                            FCompObj.Image := FDibHandle;
                                            TmcmImageTargaRLE(FCompObj).FlipVert := FlipVert;
                                            FMemStream.Size := 8 * FLongWidth;

                                            Count := 0;
                                            while (Count < FDibInfo^.bmiHeader.biSizeImage) and (FError = EC_OK)
                                            do begin
                                               BufferSize := FStream.Read(FMemStream.Memory^, FMemStream.Size);
                                               FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                               FStream.Position := FStream.Position - longint(BufferSize);
                                               inc(Count, DataCount);
                                            end;
                                          finally
                                            if Assigned(FCompObj)
                                            then FCompObj.Free;
                                          end;
                                        end;
                 end;
            end;
       end;
       if (FError = EC_OK)
       then ReadFooter(Stream);
  end;
end; // TmcmTargaImage.LoadFromStream.


function TmcmTargaImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var Photometric : TmcmPhotometric;
    IDLen       : integer;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;
       try
         // Fill Targa header.
         Photometric := GetPhotometric;
         if (FCompress = CP_RLE_TARGA)
         then begin
              case Photometric of
              PM_GREY    : FTargaHeader.ImageType := TARGA_BW_RLE;
              PM_RGB,
              PM_RGBA,
              PM_RGB16   : FTargaHeader.ImageType := TARGA_TRUECOLOR_RLE;
              PM_INVGREY,
              PM_PALETTE : FTargaHeader.ImageType := TARGA_INDEXED_RLE;
              end;
         end
         else begin
              case Photometric of
              PM_GREY    : FTargaHeader.ImageType := TARGA_BW;
              PM_RGB,
              PM_RGBA,
              PM_RGB16   : FTargaHeader.ImageType := TARGA_TRUECOLOR;
              PM_INVGREY,
              PM_PALETTE : FTargaHeader.ImageType := TARGA_INDEXED;
              end;
         end;

         with FDibInfo^.bmiHeader
         do begin
            FTargaHeader.Width      := biWidth;
            FTargaHeader.Height     := biHeight;
            FTargaHeader.PixelDepth := FBitCount;

            if (FBitCount <= 8) and (FTargaHeader.ImageType in [TARGA_INDEXED, TARGA_INDEXED_RLE])
            then begin
                 FTargaHeader.ColorMapType      := TARGA_COLORMAP;
                 FTargaHeader.ColorMapStart     := 0;
                 FTargaHeader.ColorMapLength    := 1 shl FBitCount;
                 FTargaHeader.ColorMapEntrySize := 24;
            end
            else begin
                 FTargaHeader.ColorMapType      := TARGA_NOCOLORMAP;
                 FTargaHeader.ColorMapStart     := 0;
                 FTargaHeader.ColorMapLength    := 0;
                 FTargaHeader.ColorMapEntrySize := 0;
            end;
         end;
         FTargaHeader.ImageDescriptor := 0; // Image orientation is Bottom - Left.

         // Add information from TmcmImageInfo class.
         if Assigned(FImageInfo)
         then begin
              {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPCopy(FImageID, AnsiString(FImageInfo.Software));
              FTargaHeader.XOrigin := Round(FImageInfo.XPosition * FImageInfo.XResolution);
              FTargaHeader.YOrigin := Round(FImageInfo.YPosition * FImageInfo.YResolution);
         end
         else begin
              FTargaHeader.XOrigin := 0;
              FTargaHeader.YOrigin := 0;
         end;

         // Write header to file.
         IDLen := {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrLen(FImageID);
         if (IDLen < 256)
         then FTargaHeader.IDLength := IDLen
         else FTargaHeader.IDLength := 255;
         FStream.Write(FTargaHeader, SizeOf(TTargaHeader));
         if (FTargaHeader.IDLength > 0)
         then FStream.Write(FImageID, FTargaHeader.IDLength * SizeOf(byte));

       except
         On E:Exception
         do FError := EC_WRITETOFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmTargaImage.WriteHeader.


function TmcmTargaImage.WritePalette(Stream : TStream) : TmcmErrorCode;
var i, j, k   : integer;
    ColourMap : PVectorB;
    MapSize   : integer;
begin
  FError := EC_OK;
  try
    // Read colour map.
    MapSize := FTargaHeader.ColorMapEntrySize div 8 * FTargaHeader.ColorMapLength;
    GetMem(ColourMap, MapSize);
    try
      j := FTargaHeader.ColorMapStart;
      k := 0;
      for i := 0 to (FTargaHeader.ColorMapLength - 1)
      do begin
         ColourMap^[k] := FDibInfo^.bmiColors[j].rgbBlue;
         inc(k);
         ColourMap^[k] := FDibInfo^.bmiColors[j].rgbGreen;
         inc(k);
         ColourMap^[k] := FDibInfo^.bmiColors[j].rgbRed;
         inc(k);
         inc(j);
      end;
      FStream.Write(ColourMap^, MapSize);
    finally
      FreeMem(ColourMap);
    end;
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmTargaImage.WritePalette.


function TmcmTargaImage.WriteFooter(Stream : TStream) : TmcmErrorCode;
begin
  FError := EC_OK;
  if (FStreamStart = 0)
  then begin
       FTargaFooter.Signature := TARGA2_SIGNATURE;
       FTargaFooter.Signature[17] := #0;
       FStream.Write(FTargaFooter, SizeOf(TTargaFooter));
  end;
  Result := FError;
end; // TmcmTargaImage.WriteFooter.


function TmcmTargaImage.WriteExtension(Stream : TStream) : TmcmErrorCode;
var msec : word;
begin
  try
    if Assigned(FImageInfo)
    then begin
         // Clear all fields in Extension area.
         FillMemory(@FTargaExtension, SizeOf(TTargaExtension), 0);

         FTargaExtension.ExtensionSize := SizeOf(TTargaExtension);
         if (FImageInfo.Artist <> '')
         then begin
              {$IFNDEF UNICODE}
                StrPCopy(FTargaExtension.AuthorName, FImageInfo.Artist);
              {$ELSE}
                {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPCopy(FTargaExtension.AuthorName, AnsiString(FImageInfo.Artist));
              {$ENDIF}
         end;
         FTargaExtension.AuthorName[40] := #0;
         if (FImageInfo.Description <> '')
         then begin
              {$IFNDEF UNICODE}
                StrPCopy(FTargaExtension.AuthorComments, FImageInfo.Description);
              {$ELSE}
                {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPCopy(FTargaExtension.AuthorComments, AnsiString(FImageInfo.Description));
              {$ENDIF}
         end;
         FTargaExtension.AuthorComments[323] := #0;
         try
           DecodeDate(FImageInfo.DateTime,
                      FTargaExtension.DateTimeStamp[2],
                      FTargaExtension.DateTimeStamp[0],
                      FTargaExtension.DateTimeStamp[1]);
           DecodeTime(FImageInfo.DateTime,
                      FTargaExtension.DateTimeStamp[3],
                      FTargaExtension.DateTimeStamp[4],
                      FTargaExtension.DateTimeStamp[5], msec);
         except
           On E:EConvertError
           do ;
         end;
         // FTargaExtension.JobNameID
         // FTargaExtension.JobTime
         {$IFNDEF UNICODE}
           StrPCopy(FTargaExtension.SoftwareID, FImageInfo.Software);
         {$ELSE}
           {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPCopy(FTargaExtension.SoftwareID, AnsiString(FImageInfo.Software));
         {$ENDIF}
         FTargaExtension.SoftwareID[40] := #0;
         FTargaExtension.SoftwareVersion.VersionNo := Trunc(FImageInfo.SoftwareVersion * 100);
         // FTargaExtension.KeyColor
         FTargaExtension.AspectRatioW := FImageInfo.PixelWidth;
         FTargaExtension.AspectRatioH := FImageInfo.PixelHeight;
         // FTargaExtension.GammaNum
         // FTargaExtension.GammaDeNom
         // FTargaExtension.ColorCorrectOfs
         // FTargaExtension.PostStampOffset
         // FTargaExtension.ScanLineOffset
         // FTargaExtension.AttributeType

         FTargaFooter.ExtensionOffset := FStream.Position;
         FStream.Write(FTargaExtension, SizeOf(TTargaExtension));
    end;
  except
    On E:EReadError
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmTargaImage.WriteExtension.


procedure TmcmTargaImage.SaveToStream(Stream : TStream);
var i, j       : integer;
    Count      : longint;
    Buffer     : Pointer;
    BufferSize : longword;
    DataCount  : longword;
    FlipVert   : boolean;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write Targa file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Colour map (palette).
                 if (FTargaHeader.ColorMapType = TARGA_COLORMAP)
                 then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                      // Write image data.
                      FlipVert := ((FTargaHeader.ImageDescriptor and $20) <> 0);
                      case FCompress of
                      CP_NOCOMP    : begin
                                       FRowWidth := FDibInfo^.bmiHeader.biWidth * (FTargaHeader.PixelDepth div 8);
                                       if FlipVert
                                       then j := (FDibInfo^.bmiHeader.biHeight - 1) * FLongWidth
                                       else j := 0;
                                       for i := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                                       do begin
                                          FStream.Write(PVectorB(FDibBits)^[j], FRowWidth);
                                          if FlipVert
                                          then dec(j, FLongWidth)
                                          else inc(j, FLongWidth);
                                       end;
                                     end;
                      CP_RLE_TARGA : begin
                                       FCompObj := TmcmImageTargaRLE.Create;
                                       try
                                         FCompObj.Image := FDibHandle;
                                         TmcmImageTargaRLE(FCompObj).FlipVert := FlipVert;

                                         Count := 0;
                                         while (FError = EC_OK) and
                                               (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                                         do begin
                                            DataCount := FLongWidth;
                                            FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                                            if (FError = EC_OK)
                                            then begin
                                                 FStream.Write(Buffer^, BufferSize);
                                                 inc(Count, DataCount);
                                            end;
                                         end;
                                       finally
                                         if Assigned(FCompObj)
                                         then FCompObj.Free;
                                       end;
                                     end;
                      else FError := EC_COMPRESSION;
                      end;
                 end;
            end;

            // Write Developers Area.
            FTargaFooter.DeveloperOffset := 0; // FStream.Position

            // Write Extension Area.
            FTargaFooter.ExtensionOffset := 0; // FStream.Position;
            if Assigned(FImageInfo)
            then WriteExtension(Stream);

            // Write Scan Line Table.

            // Write Postage Stamp Image.

            // Write Color Correction Table.

            // Write Targa footer.
            WriteFooter(Stream);

            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmTargaImage.SaveToStream.                                        


//------------------------------------------------------------------------------
// TmcmDICOMImage.
//------------------------------------------------------------------------------
{$IFDEF mcmDICOM}

function TmcmDICOMImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var DId     : array[0..3] of AnsiChar;
    Group   : longword;
//    Element : longword;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
         FStream.BigEndian := False;
       try
         FStream.Position := 0;
         FStream.Read(DId, 4 * SizeOf(AnsiChar));
         if (DId[0] = 'D') and (DId[1] = 'I') and (DId[2] = 'C') and (DId[3] = 'M')
         then FError := EC_OK
         else begin
              FStream.Position := 128;
              FStream.Read(DId, 4 * SizeOf(AnsiChar));
              if (DId[0] = 'D') and (DId[1] = 'I') and (DId[2] = 'C') and (DId[3] = 'M')
              then FError := EC_OK
              else begin
                   // Not a proper DICOM file. Try DICOM without header!
                   FStream.Position := 0;
                   Group   := FStream.ReadWord;
	           // Element := FStream.ReadWord;
                   if (Group in [$0000, $0002, $0004, $0008])
                   then FError := EC_OK;
                   FStream.Position := 0;
              end;
         end;
       except
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmDICOMImage.IsFormatValid.                                       


procedure TmcmDICOMImage.LoadFromStream(Stream : TStream);
begin
  if Assigned(Stream)
  then begin
       FStream := Stream;
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              // FStream.Position := 0;
              if (FDibInfo <> nil)
              then begin
              end;
         end;
       except
         On E:EReadError
         do FError := EC_ENDOFFILE;
       end;
  end;
end; // TmcmDICOMImage.LoadFromStream.


procedure TmcmDICOMImage.SaveToStream(Stream : TStream);
begin
  if Assigned(Stream)
  then begin
       FStream := Stream;
       if GetDibData
       then begin
{
            // Write DICOM file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Colour map (palette).
                 if ()
                 then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                 end;
            FStream.Flush;
}
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmDICOMImage.SaveToStream.

{$ENDIF}

//------------------------------------------------------------------------------
// TmcmPNGImage
//------------------------------------------------------------------------------

class function TmcmPNGImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24];
end; // TmcmPNGImage.GetColorFormats.


class function TmcmPNGImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_BW..IF_PAL8,IF_RGB24])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_PNG;
       inc(Count);
  end;
  Result := Count;
end; // TmcmPNGImage.GetCompressionFromColor.


class function TmcmPNGImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_PNG : Result := resCP_PNG;
  else Result := '';
  end;
end; // TmcmPNGImage.GetCompressionName.


constructor TmcmPNGImage.Create;
var i : integer;
begin
  Inherited Create;
  FStreamAdler := $FFFFFFFF;
  FStream.BigEndian   := True;

  FLZWin := Nil;
  FBufIndex := 0;
  FBkRed    := $FFFF;
  FBkGreen  := $FFFF;
  FBkBlue   := $FFFF;
  FBkGrey   := $FFFF;

  FHashTable    := Nil;
  FHashValues   := Nil;
  FLiteralCode  := Nil;
  FDistanceCode := Nil;
  FLengthCode   := Nil;
  FBlockBuffer  := Nil;
  for i := 0 to 4
  do FFilterBuffer[i] := Nil;
end; // TmcmPNGImage.Create


destructor TmcmPNGImage.Destroy;
begin
  if Assigned(FLZWin)
  then FreeMem(FLZWin);
  FLZWin := Nil;
  Inherited Destroy
end; // TmcmPNGImage.Destroy.


function TmcmPNGImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var Signature : array[0..7] of byte;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;
         FStream.Read(Signature, 8);
         if (Signature[0] = PNGSignature[0]) and
            (Signature[1] = PNGSignature[1]) and
            (Signature[2] = PNGSignature[2]) and
            (Signature[3] = PNGSignature[3]) and
            (Signature[4] = PNGSignature[4]) and
            (Signature[5] = PNGSignature[5]) and
            (Signature[6] = PNGSignature[6]) and
            (Signature[7] = PNGSignature[7])
             // Criteria is OK
         then FError := EC_OK;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPNGImage.IsFormatValid.


function TmcmPNGImage.CreateCRCTable : TmcmErrorCode;
var i, j : word;
    c    : cardinal;
begin
  for i := 0 to 255
  do begin
     c := cardinal(i);
     for j := 0 to 7
     do begin
        if ((c and $01) <> $00)
        then c := cardinal($0EDB88320) xor (c shr 1)
        else c := c shr 1;
     end;
     FCRCTable[i] := c;
  end;
  Result := FError;
end; // TmcmPNGImage.CreateCRCTable.


function TmcmPNGImage.CalcCRC(Buffer : PVectorB; Count : longint) : longword; // cardinal;
{$IFDEF DCB3}
var i, CRC : longword;
    Index  : longword;
{$ELSE}
var i, CRC : cardinal;
    Index  : cardinal;
{$ENDIF}
begin
{$IFDEF DCB3}
  CRC := longword($FFFFFFFF);
{$ELSE}
  CRC := $FFFFFFFF;
{$ENDIF}
  for i := 0 to (Count - 1)
  do begin
     Index := (CRC xor Buffer^[i]) and $0FF;
     CRC := FCRCTable[Index] xor ((CRC shr 8) and $0FFFFFF);
  end;
  Result := Not(CRC);
end; // TmcmPNGImage.CalcCRC.


function TmcmPNGImage.ReadChunk(Stream : TStream; var Chunk : TPNGChunk) : TmcmErrorCode;
begin
  try
    // Read chunk size.
    Chunk.Length := FStream.ReadLong;
    FMemStream.Clear;
    if (longint(Chunk.Length) <= FStream.Size)
    then begin
         FMemStream.SetSize(Chunk.Length + 4);

         // Read chunk type.
         SetLength(Chunk.Name, 4);

         // Read chunk data.
         FStream.Read(FMemStream.Memory^, FMemStream.Size);
         Chunk.Data := FMemStream.Memory;
         Chunk.Name := Copy(PAnsiChar(Chunk.Data), 1, 4);

         if (Chunk.Name <> '')
         then begin
              inc(PAnsiChar(Chunk.Data), 4);

              // Read and check CRC.
              Chunk.CRC := FStream.ReadLong;

              if (Chunk.Name[3] <> AnsiString(AnsiLowerCase(string(Chunk.Name[3]))))
              then if (Chunk.CRC <> CalcCRC(FMemStream.Memory, FMemStream.Size))
                   then FError := EC_CRC;
         end
         else begin
              FError := EC_BADFORMAT;
              FMemStream.Clear;
         end;
    end
    else begin
         FError := EC_BADFORMAT;
         FMemStream.Clear;
    end;

    FBufBit := 0;
    FBufIndex := 0;
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmPNGImage.ReadChunk.


function TmcmPNGImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              CreateCRCTable;

              // Read header and check format.
              if (FDibInfo <> nil)
              then begin
                   FCompress := CP_PNG;

                   // Read header information.
                   ReadChunk(Stream, FChunk);

                   if (FChunk.Name = 'IHDR')
                   then begin
                        CopyMemory(@FPNGHeader, FChunk.Data, FChunk.Length);
                        FPNGHeader.Width  := SwapLong(FPNGHeader.Width);
                        FPNGHeader.Height := SwapLong(FPNGHeader.Height);

                        if (FPNGHeader.Interlaced = 0)
                        then FInterlaced := False
                        else FInterlaced := True;

                        with FDibInfo^.bmiHeader
                        do begin
                           biWidth  := FPNGHeader.Width;
                           biHeight := FPNGHeader.Height;
                           biPlanes := 1;
                           if (FPNGHeader.BitDepth in [1,2,4,8{16}])
                           then begin
                                case FPNGHeader.ColorType of
                                // Greyscale
                                0,
                                // Palette
                                3 : begin
                                      biBitCount := FPNGHeader.BitDepth;
                                      FFilterOffset := 1;
                                      FBytesPerRow := (biWidth * FPNGHeader.BitDepth + 7) div 8;
                                    end;
                                // RGB
                                2 : begin
                                      biBitCount := 3 * FPNGHeader.BitDepth;
                                      FFilterOffset := (biBitCount) div 8;
                                      FBytesPerRow := (biWidth * FPNGHeader.BitDepth * 3) div 8;
                                    end;
                                // Greyscale with alpha channel.
                                4 : begin
                                      biBitCount := FPNGHeader.BitDepth;
                                      FFilterOffset := 2 * biBitCount div 8;
                                      FBytesPerRow := (2 * biWidth * FPNGHeader.BitDepth) div 8;
                                    end;
                                // RGB with alpha channel.
                                6 : begin
                                      biBitCount := 4 * FPNGHeader.BitDepth;
                                      FFilterOffset := biBitCount div 8;
                                      FBytesPerRow := (biWidth * FPNGHeader.BitDepth * 4) div 8;
                                    end;
                                else FError := EC_UNKNOWNFORMAT;
                                end;
                           end
                           else FError := EC_BITDEPTH;

                           biXPelsPerMeter := 0;
                           biYPelsPerMeter := 0;
                           biClrUsed       := 0;
                           biClrImportant  := 0;
                        end;

                        if (FPNGHeader.ColorType in [0, 4])
                        then CreateGreyPalette(PM_GREY);

                        // If bit depth is 2 scale it up to 4 bits.
                        if (FPNGHeader.BitDepth = 2)
                        then FDibInfo^.bmiHeader.biBitCount := 4;
                   end
                   else FError := EC_BADFORMAT;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPNGImage.ReadHeader.


function TmcmPNGImage.ReadPalette(Chunk : TPNGChunk) : TmcmErrorCode;
var i, j : word;
begin
  try
    FNoColors := Chunk.Length div 3;
    if (FNoColors > 0)
    then begin
         // Read palette values.
         j := 0;
         for i := 0 to Pred(FNoColors)
         do begin
            with FDIBInfo^.bmiColors[i]
            do begin
               rgbRed := PVectorB(Chunk.Data)^[j];
               inc(j);
               rgbGreen := PVectorB(Chunk.Data)^[j];
               inc(j);
               rgbBlue := PVectorB(Chunk.Data)^[j];
               inc(j);
               rgbReserved := 0;
            end;
         end;
    end;
    FNoColors := GetNumColors(FDibInfo);
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmPNGImage.ReadPalette.


function TmcmPNGImage.ReadImageChunk(Stream : TStream; var Chunk : TPNGChunk) : TmcmErrorCode;
begin
  ReadChunk(Stream, Chunk);
  if (FChunk.Name <> 'IDAT')
  then begin
       FError := EC_BADFORMAT;
       raise EPNGException.Create('PNG, Bad code');
  end;
  FBufIndex := 0;
  FBufBit   := 0;
  Result := FError;
end; // TmcmPNGImage.ReadImageChunk.


function TmcmPNGImage.GetNBits(Count : word) : cardinal;
var Value : cardinal;
    i     : word;
begin
  Value := 0;
  for i := 0 to (Count - 1)
  do begin
     if (FBufBit >= 8)
     then begin
          FBufByte := GetIDataByte;
          FBufBit  := 0;
     end;
     if ((FBufByte and (1 shl FBufBit)) <> 0)
     then Value := Value or (1 shl i);
     inc(FBufBit);
  end;
  Result := Value;
end; // TmcmPNGImage.GetNBits.


function TmcmPNGImage.GetIDataByte : byte;
begin
  while (FBufIndex >= FChunk.Length)
  do begin
     ReadImageChunk(FStream.Source, FChunk);
     FBufIndex := 0;
  end;

  Result := PVectorB(FChunk.Data)^[FBufIndex];
  FBufBit := 8;
  inc(FBufIndex);
end; // TmcmPNGImage.GetIDataByte.


function TmcmPNGImage.DecodeCompressed : byte;
var Value    : cardinal;
    BitCount : word;
    Length   : word;
    Distance : word;
begin
  if (FCopyCount <> 0)
  then begin
       // Copy byte from LZ window
       Value := FLZWin^[FCopyPos];
       FLZWin^[FWinPos] := Value;
       dec(FCopyCount);

       // Advance copy and window position.
       FWinPos  := (FWinPos + 1) and $07FFF;
       FCopyPos := (FCopyPos + 1) and $07FFF;

       UpdateAdler(Value);
  end
  else begin
       Value := FLiteralCode.Decompress;
       if (Value < 256)
       then begin
            // Value read is a literal.
            FLZWin^[FWinPos] := Value;
            FWinPos  := (FWinPos + 1) and $07FFF;

            UpdateAdler(Value);
       end
       else begin
            if (Value = 256)
            then begin
                 // Value is "End marker".
                 if FFinalDataSet
                 then begin
                      FError := EC_DECOMPRESSION; // There should more more data!
                      raise EPNGException.Create('PNG, Bad code');
                 end
                 else begin
                      NewCompressBlock;
                      Value := DecodeByte;
                 end;
            end
            else begin
                 if (Value < 286)
                 then begin
                      // Value specifies a length code.
                      // Get Length code and add/read extra bits
                      BitCount := PNGLengthExtra[Value - 257];
                      Length := PNGLengthBase[Value - 257];
                      if (BitCount <> 0)
                      then Length := Length + GetNBits(BitCount);

                      // Get Distance code and add/read extra bits.
                      Value := FDistanceCode.Decompress;
                      if (Value < 30)
                      then begin
                           BitCount := PNGDistExtra[Value];
                           Distance := PNGDistBase[Value];
                           if (BitCount <> 0)
                           then Distance := Distance + GetNBits(BitCount);

                           // Set state variables used to find copied data.
                           FCopyPos := (FWindowSize + FWinPos - Distance) and $07FFF;
                           FCopyCount := Length;

                           // Return the first copy byte.
                           Value := FLZWin^[FCopyPos];
                           FLZWin^[FWinPos] := Value;

                           FCopyPos := (FCopyPos + 1) and $07FFF;
                           FWinPos := (FWinPos + 1) and $07FFF;

                           dec(FCopyCount);

                           UpdateAdler(Value);
                      end
                      else begin
                           FError := EC_DECOMPRESSION;
                           raise EPNGException.Create('PNG, Bad code');
                      end;
                 end
                 else begin
                      FError := EC_DECOMPRESSION;
                      raise EPNGException.Create('PNG, Bad code');
                 end;
            end;
       end;
  end;
  Result := Value;
end; // TmcmPNGImage.DecodeCompressed.


function TmcmPNGImage.DecodeLiteral : byte;
var Value : byte;
begin
  if (FLiteralCount = 0)
  then begin
       if FFinalDataSet
       then begin
            FError := EC_DECOMPRESSION;
            raise EPNGException.Create('PNG, Bad code');
       end
       else begin
            NewCompressBlock;
            Value := DecodeByte;
       end;
  end
  else begin
       dec(FLiteralCount);
       Value := GetIDataByte;
  end;
  UpdateAdler(Value);
  Result := Value;
end; // TmcmPNGImage.DecodeLiteral.


function TmcmPNGImage.DecodeByte : byte;
begin
  if FLiteralMode
  then Result := DecodeLiteral
  else Result := DecodeCompressed;
end; // TmcmPNGImage.DecodeByte.


function TmcmPNGImage.ReadLength(Decoder    : TmcmHuffmanPNG;
                                 LengthCode : TmcmHuffmanPNG;
                                 Count      : cardinal) : TmcmErrorCode;
var i, j        : cardinal;
    RepeatCount : cardinal;
    Command     : cardinal;
begin
  i := 0;
  while (i < Count) and (FError = EC_OK)
  do begin
     Command := Decoder.Decompress;
     case Command of
     0..15 : begin
               // Raw length
               LengthCode.HuffSize[i] := Command;
               inc(i);
             end;
     16    : begin
               // Repeat previous
               RepeatCount := GetNBits(2) + 3;
               for j := 0 to (RepeatCount - 1)
               do begin
                  if (i < Count)
                  then begin
                       LengthCode.HuffSize[i] := LengthCode.HuffSize[i-1];
                       inc(i);
                  end
                  else begin
                       FError := EC_DECOMPRESSION;
                       raise EPNGException.Create('PNG, Bad code');
                  end;
               end;
             end;
     17    : begin
               // Run of zeros
               RepeatCount := GetNBits(3) + 3;
               for j := 0 to (RepeatCount - 1)
               do begin
                  if (i < Count)
                  then begin
                       LengthCode.HuffSize[i] := 0;
                       inc(i);
                  end
                  else begin
                       FError := EC_DECOMPRESSION;
                       raise EPNGException.Create('PNG, Bad code');
                  end;
               end;
             end;
     18    : begin
               // longer run of zeros
               RepeatCount := GetNBits(7) + 11;
               for j := 0 to (RepeatCount - 1)
               do begin
                  if (i < Count)
                  then begin
                       LengthCode.HuffSize[i] := 0;
                       inc(i);
                  end
                  else begin
                       FError := EC_DECOMPRESSION;
                       raise EPNGException.Create('PNG, Bad code');
                  end;
               end;
             end;
     else begin
          FError := EC_DECOMPRESSION;
          raise EPNGException.Create('PNG, Bad code');
     end;
     end;
  end;
  Result := FError;
end; // TmcmPNGImage.ReadLength.


function TmcmPNGImage.NewCompressBlock : TmcmErrorCode;
var i, TestCount  : cardinal;
    NoOfLiterals  : cardinal;
    NoOfDistances : cardinal;
    NoOfLengths   : cardinal;
begin
  // Process new data set within a compressed stream.
  if (GetNBits(1) = 0)
  then FFinalDataSet := False
  else FFinalDataSet := True;

  FCompressType := TPNGCompressType(GetNBits(2));
  case FCompressType of
  CPPNG_NONE           : begin
                           FLiteralMode  := True;
                           FLiteralCount := GetIDataByte +
                                            (GetIDataByte shl 8);
                           TestCount     := GetIDataByte +
                                            (GetIDataByte shl 8);
                           if ((FLiteralCount and $FFFF) <> (TestCount xor $FFFF))
                           then begin
                                FError := EC_DECOMPRESSION;
                                raise EPNGException.Create('PNG, Bad code');
                           end;
                         end;
  CPPNG_FIXEDHUFFMAN   : begin
                           FLiteralMode   := False;

                           for i := 0 to 143
                           do FLiteralCode.HuffSize[i] := 8;
                           for i := 144 to 255
                           do FLiteralCode.HuffSize[i] := 9;
                           for i := 256 to 279
                           do FLiteralCode.HuffSize[i] := 7;
                           for i := 280 to 287
                           do FLiteralCode.HuffSize[i] := 8;
                           FLiteralCode.CreateReadTable(288);

                           for i := 0 to 31
                           do FDistanceCode.HuffSize[i] := 5;
                           FDistanceCode.CreateReadTable(32);
                         end;
  CPPNG_DYNAMICHUFMANN : begin
                           FLiteralMode  := False;

                           NoOfLiterals  := GetNBits(5);
                           NoOfDistances := GetNBits(5);
                           NoOfLengths   := GetNBits(4);

                           if (NoOfLengths + 4 <= PNGLengthSize)
                           then begin
                                FLengthCode.ClearTable;
                                for i := 0 to (NoOfLengths + 3)
                                do FLengthCode.HuffSize[PNGLengthOrder[i]] := GetNBits(3);
                                FLengthCode.CreateReadTable(PNGLengthSize);

                                FLiteralCode.ClearTable;
                                ReadLength(FLengthCode, FLiteralCode, NoOfLiterals + 257);
                                FLiteralCode.CreateReadTable(NoOfLiterals + 257);

                                FDistanceCode.ClearTable;
                                ReadLength(FLengthCode, FDistanceCode, NoOfDistances + 1);
                                FDistanceCode.CreateReadTable(NoOfDistances + 1);
                           end
                           else begin
                                FError := EC_DECOMPRESSION;
                                raise EPNGException.Create('PNG, Bad code');
                           end;
                         end;
  else begin
       FError := EC_DECOMPRESSION;
       raise EPNGException.Create('PNG, Bad code');
  end;
  end;
  Result := FError;
end; // TmcmPNGImage.NewCompressBlock.


function TmcmPNGImage.PaethPredictor(Left      : integer;
                                     Above     : integer;
                                     AboveLeft : integer) : integer;
var pp : integer;
    pa : integer;
    pb : integer;
    pc : integer;
begin
  pp := Left + Above - AboveLeft;
  if (pp > Left)
  then pa := pp - Left
  else pa := Left - pp;
  if (pp > Above)
  then pb := pp - Above
  else pb := Above - pp;
  if (pp > AboveLeft)
  then pc := pp - AboveLeft
  else pc := AboveLeft - pp;

  if (pa <= pb) and (pa <= pc)
  then Result := Left
  else if (pb <= pc)
       then Result := Above
       else Result := AboveLeft;
end; // TmcmPNGImage.PaethPredictor.


procedure TmcmPNGImage.DeFilterRow(Filter : TPNGFilters);
var i, j      : cardinal;
    PreRow    : boolean;
    Left      : integer;
    Above     : integer;
    AboveLeft : integer;
    pp, vv    : integer;
begin
  PreRow := Not FFltRow;
  case TPNGFilters(Filter) of
  PNGFLT_NONE    : ;
  PNGFLT_SUB     : begin
                     for i := FFilterOffset to (FBytesPerRow - 1)
                     do begin
                        j := i - FFilterOffset;
                        FRowBuffer[FFltRow]^[i] := word(FRowBuffer[FFltRow]^[i] +
                                                        FRowBuffer[FFltRow]^[j]) and $0FF;
                     end;
                   end;
  PNGFLT_UP      : begin
                     for i := 0 to (FBytesPerRow - 1)
                     do begin
                        FRowBuffer[FFltRow]^[i] := word(FRowBuffer[FFltRow]^[i] +
                                                        FRowBuffer[PreRow]^[i]) and $0FF;
                     end;
                   end;
  PNGFLT_AVERAGE : begin
                     for i := 0 to (FBytesPerRow - 1)
                     do begin
                        Above := FRowBuffer[PreRow]^[i];
                        if (i < FFilterOffset)
                        then Left := 0
                        else Left := FRowBuffer[FFltRow]^[i-FFilterOffset];
                        FRowBuffer[FFltRow]^[i] := word(FRowBuffer[FFltRow]^[i] +
                                                        (Left + Above) div 2) and $0FF;
                     end;
                   end;
  PNGFLT_PAETH   : begin
                     for i := 0 to (FBytesPerRow - 1)
                     do begin
                        Above := FRowBuffer[PreRow]^[i];
                        if (i < FFilterOffset)
                        then begin
                             Left := 0;
                             AboveLeft := 0;
                        end
                        else begin
                             j := i - FFilterOffset;
                             Left := FRowBuffer[FFltRow]^[j];
                             AboveLeft := FRowBuffer[PreRow]^[j];
                        end;
                        vv := FRowBuffer[FFltRow]^[i];
                        pp := PaethPredictor(Left, Above, AboveLeft);

                        FRowBuffer[FFltRow]^[i] := (pp + vv) and $0FF;
                     end;
                   end;
  else begin
       FError := EC_DECOMPRESSION;
       raise EPNGException.Create('PNG, Bad Filter');
  end;
  end;
end; // TmcmPNGImage.DeFilterRow.


procedure TmcmPNGImage.NonInterlacedRowToImage(Row : cardinal);
var ImageIndex : cardinal;
    i, j, Col  : cardinal;
    ByteOfs    : cardinal;
    BitOfs     : word;
    IncCol     : word;
    Value      : byte;
    Red        : byte;
    Green      : byte;
    Blue       : byte;
    Alpha      : byte;
begin
  ImageIndex := Row * cardinal(FLongWidth);
  case FPNGHeader.ColorType of
  // Greyscale
  0,
  // Palette
  3 : case FPNGHeader.BitDepth of
      1,
      4,
      8  : CopyMemory(@PVectorB(FDibBits)^[ImageIndex], FRowBuffer[FFltRow], FBytesPerRow);
      2  : for i := 0 to (FDIBInfo^.bmiHeader.biWidth - 1)
           do begin
              ByteOfs := i div 4;
              BitOfs  := i mod 4;
              Value := (FRowBuffer[FFltRow]^[ByteOfs] shr (2 * (3 - BitOfs))) and $03;

              ByteOfs := i div 2;
              BitOfs  := i mod 2;
              j := ImageIndex + ByteOfs;
              PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or (Value shl (4 * (1 - BitOfs)));
           end;
      16 : begin
           end;
      end;
  // RGB
  2 : case FPNGHeader.BitDepth of
      8  : begin
             IncCol := FPNGHeader.BitDepth div 8;
             i := 0;
             while (i < cardinal(FLongWidth - 2))
             do begin
                // Red
                PVectorB(FDibBits)^[ImageIndex+2] := FRowBuffer[FFltRow]^[i];
                inc(ImageIndex, IncCol);
                inc(i, IncCol);
                // Green
                PVectorB(FDibBits)^[ImageIndex] := FRowBuffer[FFltRow]^[i];
                inc(ImageIndex, IncCol);
                inc(i, IncCol);
                // Blue
                PVectorB(FDibBits)^[ImageIndex-2] := FRowBuffer[FFltRow]^[i];
                inc(ImageIndex, IncCol);
                inc(i, IncCol);
             end;
           end;
      16 : begin
           end;
      end;
  // Greyscale with alpha channel.
  4 : case FPNGHeader.BitDepth of
      8  : begin
             Col := 0;
             while (Col < FBytesPerRow)
             do begin
                Value := FRowBuffer[FFltRow]^[Col];
                inc(Col);
                Alpha := FRowBuffer[FFltRow]^[Col];
                inc(Col);
                if (FBkGrey = $FFFF)
                then PVectorB(FDibBits)^[ImageIndex] := Value
                else PVectorB(FDibBits)^[ImageIndex] := (Alpha * Value + (255 - Alpha) * FBkGrey) div 255;
                Inc(ImageIndex);
             end;
           end;
      16 : begin
           end;
      end;
  // RGB with alpha channel.
  6 : case FPNGHeader.BitDepth of
      8  : begin
             Col := 0;
             if (FBkBlue = $FFFF)
             then begin
                  while (Col < FBytesPerRow)
                  do begin
                     Red := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     Green := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     Blue := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     //Alpha := FRowBuffer[FFltRow]^[Col];
                     inc(Col);

                     PVectorB(FDibBits)^[ImageIndex]   := Blue;
                     inc(ImageIndex);
                     PVectorB(FDibBits)^[ImageIndex] := Green;
                     inc(ImageIndex);
                     PVectorB(FDibBits)^[ImageIndex] := Red;
                     inc(ImageIndex);
                     Inc(ImageIndex);
                  end;
             end
             else begin
                  while (Col < FBytesPerRow)
                  do begin
                     Red := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     Green := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     Blue := FRowBuffer[FFltRow]^[Col];
                     inc(Col);
                     Alpha := FRowBuffer[FFltRow]^[Col];
                     inc(Col);

                     PVectorB(FDibBits)^[ImageIndex]   := (Alpha * Blue + (255 - Alpha) * FBkBlue) div 255;
                     inc(ImageIndex);
                     PVectorB(FDibBits)^[ImageIndex] := (Alpha * Green + (255 - Alpha) * FBkGreen) div 255;
                     inc(ImageIndex);
                     PVectorB(FDibBits)^[ImageIndex] := (Alpha * Red + (255 - Alpha) * FBkRed) div 255;
                     inc(ImageIndex);
                     Inc(ImageIndex);
                  end;
             end;
           end;
      16 : begin
           end;
      end;
  end;
end; // TmcmPNGImage.NonInterlacedRowToImage.


function TmcmPNGImage.ReadNonInterlaced : TmcmErrorCode;
var x, y   : longint;
    Filter : word;
begin
  FFltRow := False;
  FillChar(FRowBuffer[False]^, FBytesPerRow * SizeOf(byte), 0);
  FillChar(FRowBuffer[True]^, FBytesPerRow * SizeOf(byte), 0);

  y := FDibInfo^.bmiHeader.biHeight - 1;
  while (y >= 0)
  do begin
     // Get filter used on this row.
     Filter := DecodeByte;

     // Decompress row.
     x := 0;
     while (x < FBytesPerRow)
     do begin
        FRowBuffer[FFltRow]^[x] := DecodeByte;
        inc(x);
     end;

     // Filter row
     DeFilterRow(TPNGFilters(Filter));

     // Copy row to image
     NonInterlacedRowToImage(y);

     // Change buffer
     FFltRow := Not(FFltRow);
     dec(y);
  end;
  Result := FError;
end; // TmcmPNGImage.ReadNonInterlaced.


procedure TmcmPNGImage.InterlacedRowToImage(Row, RowWidth : cardinal; Pass : word);
var ImageIndex : cardinal;
    i, j, Col  : longint;
    DestCol    : cardinal;
    Value      : byte;
    Red        : byte;
    Green      : byte;
    Blue       : byte;
    Alpha      : byte;
begin
  // Set initial index into image.
  ImageIndex := Row * cardinal(FLongWidth);

  // Start column
  DestCol := PNGInterlaceInfo[Pass][3];

  case FPNGHeader.ColorType of
  // Greyscale
  0,
  // Palette
  3 : case FPNGHeader.BitDepth of
      1  : begin
             for Col := 0 to (RowWidth - 1)
             do begin
                ImageIndex := Row * cardinal(FLongWidth);
                i := 7;
                while (i >= 0) and (longint(DestCol) < FDIBInfo^.bmiHeader.biWidth)
                do begin
                   Value := (FRowBuffer[FFltRow]^[Col] shr i) and $01;
                   j := ImageIndex + (DestCol shr 3);
                   PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or (Value shl (7 - (DestCol and $07)));

                   DestCol := DestCol + PNGInterlaceInfo[Pass][1];
                   dec(i);
                end;
             end;
           end;
      2  : begin
             for Col := 0 to (RowWidth - 1)
             do begin
                i := 6;
                while (i >= 0) and (DestCol < cardinal(FDIBInfo^.bmiHeader.biWidth))
                do begin
                   Value := (FRowBuffer[FFltRow]^[Col] shr i) and $03;
                   j := ImageIndex + (DestCol shr 1);

                   if Odd(DestCol)
                   then PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or Value
                   else PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or (Value shl 4);

                   DestCol := DestCol + PNGInterlaceInfo[Pass][1];
                   dec(i, 2);
                end;
             end;
           end;
      4  : begin
             for Col := 0 to (RowWidth - 1)
             do begin
                i := 4;
                while (i >= 0) and (DestCol < cardinal(FDIBInfo^.bmiHeader.biWidth))
                do begin
                   Value := (FRowBuffer[FFltRow]^[Col] shr i) and $0F;
                   j := ImageIndex + (DestCol shr 1);

                   if Odd(DestCol)
                   then PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or Value
                   else PVectorB(FDibBits)^[j] := PVectorB(FDibBits)^[j] or (Value shl 4);

                   DestCol := DestCol + PNGInterlaceInfo[Pass][1];
                   dec(i, 4);
                end;
             end;
           end;
      8  : begin
             ImageIndex := ImageIndex + DestCol;
             for Col := 0 to (RowWidth - 1)
             do begin
                PVectorB(FDibBits)^[ImageIndex] := FRowBuffer[FFltRow]^[Col];
                ImageIndex := ImageIndex + PNGInterlaceInfo[Pass][1];
             end;
           end;
      16 : for Col := 0 to (RowWidth - 1)
           do begin
           end;
      end;
  // RGB
  2 : begin
        case FPNGHeader.BitDepth of
        8  : begin
               ImageIndex := ImageIndex + 3 * DestCol;
               Col := 0;
               while (Col < longint(RowWidth))
               do begin
                  PVectorB(FDibBits)^[ImageIndex+2] := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  PVectorB(FDibBits)^[ImageIndex+1] := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  PVectorB(FDibBits)^[ImageIndex] := FRowBuffer[FFltRow]^[Col];
                  inc(Col);

                  ImageIndex := ImageIndex + 3 * PNGInterlaceInfo[Pass][1];
               end;
             end;
        16 : begin
             end;
        end;
      end;
  // Greyscale with alpha channel.
  4 : begin
        case FPNGHeader.BitDepth of
        8  : begin
               ImageIndex := ImageIndex + DestCol;
               Col := 0;
               while (Col < longint(RowWidth))
               // for Col := 0 to (RowWidth - 1)
               do begin
                  Value := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  Alpha := FRowBuffer[FFltRow]^[Col];
                  inc(Col);

                  PVectorB(FDibBits)^[ImageIndex] := (Alpha * Value + (255 - Alpha) * FBkGrey) div 255;
                  ImageIndex := ImageIndex + PNGInterlaceInfo[Pass][1];
               end;
             end;
        16 : begin
             end;
        end;
      end;
  // RGB with alpha channel.
  6 : begin
        case FPNGHeader.BitDepth of
        8  : begin
               ImageIndex := ImageIndex + 4 * DestCol;
               Col := 0;
               while (Col < longint(RowWidth))
               do begin
                  Red := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  Green := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  Blue := FRowBuffer[FFltRow]^[Col];
                  inc(Col);
                  Alpha := FRowBuffer[FFltRow]^[Col];
                  inc(Col);

                  PVectorB(FDibBits)^[ImageIndex+2] := (Alpha * Red + (255 - Alpha) * FBkRed) div 255;
                  PVectorB(FDibBits)^[ImageIndex+1] := (Alpha * Green + (255 - Alpha) * FBkGreen) div 255;
                  PVectorB(FDibBits)^[ImageIndex]   := (Alpha * Blue + (255 - Alpha) * FBkBlue) div 255;

                  ImageIndex := ImageIndex + 4 * PNGInterlaceInfo[Pass][1];
               end;
             end;
        16 : begin
             end;
        end;
      end;
  end;
end; // TmcmPNGImage.InterlacedRowToImage.


function TmcmPNGImage.ReadInterlaced(Pass : word) : TmcmErrorCode;
var PixelsInRow : cardinal;
    BytesPerRow : longint;
    x, y        : longint;
    Filter      : word;
begin
  // Check that the image is large enough for processing.
  if (PNGInterlaceInfo[Pass][2] < FDIBInfo^.bmiHeader.biHeight) and
     (PNGInterlaceInfo[Pass][3] < FDIBInfo^.bmiHeader.biWidth)
  then begin
       FFltRow := False;
       FillChar(FRowBuffer[False]^, FBytesPerRow * SizeOf(byte), 0);
       FillChar(FRowBuffer[True]^, FBytesPerRow * SizeOf(byte), 0);

       // PixelsInRow = (Image Width - Col Start + Col Interval + 1) / Col Interval.
       with FDIBInfo^.bmiHeader
       do PixelsInRow := (biWidth -
                          PNGInterlaceInfo[Pass][3] +
                          PNGInterlaceInfo[Pass][1] - 1) div
                          PNGInterlaceInfo[Pass][1];

       case FPNGHeader.ColorType of
       // Greyscale
       0,
       // Palette
       3 : BytesPerRow := (PixelsInRow * FPNGHeader.BitDepth + 7) div 8;
       // RGB
       2 : BytesPerRow := (PixelsInRow * FPNGHeader.BitDepth * 3) div 8;
       // Greyscale with alpha channel.
       4 : BytesPerRow := (PixelsInRow * FPNGHeader.BitDepth * 2) div 8;
       // RGB with alpha channel.
       6 : BytesPerRow := (PixelsInRow * FPNGHeader.BitDepth * 4) div 8;
       else BytesPerRow := 0;
       end;

       // Set y equal to "start row"
       y := PNGInterlaceInfo[Pass][2];
       while (y < FDIBInfo^.bmiHeader.biHeight)
       do begin
          // Get filter used on this row.
          Filter := DecodeByte;

          // Decompress row.
          x := 0;
          while (x < BytesPerRow)
          do begin
             FRowBuffer[FFltRow]^[x] := DecodeByte;
             inc(x);
          end;

          // Filter row
          DeFilterRow(TPNGFilters(Filter));

          // Copy row to image
          InterlacedRowToImage(FDIBInfo^.bmiHeader.biHeight - 1 - y, BytesPerRow, Pass);

          // Change buffer
          FFltRow := Not(FFltRow);

          // Increment y by "row interval".
          inc(y, PNGInterlaceInfo[Pass][0]);
       end;
  end;
  Result := FError;
end; // TmcmPNGImage.ReadInterlaced.


procedure TmcmPNGImage.UpdateAdler(const Value : byte);
var AdlerLo  : longword;
    AdlerHi  : longword;
begin
  // Add to Adler check.
  AdlerLo := (FStreamAdler and $0FFFF);
  AdlerHi := ((FStreamAdler shr 16) and $0FFFF);
  AdlerLo := (AdlerLo + Value) mod PNGAdlerPrime;
  AdlerHi := (AdlerLo + AdlerHi) mod PNGAdlerPrime;
  FStreamAdler := (AdlerHi shl 16) or AdlerLo;
end; // TmcmPNGImage.UpdateAdler.


function TmcmPNGImage.CheckAdler : TmcmErrorCode;
var CheckValue : longword;
    EndMarker  : integer;
begin
  repeat
    if FLiteralMode
    then begin
         if (FLiteralCount <> 0)
         then begin
              FError := EC_ENDOFDATA;
              raise EPNGException.Create('PNG, Bad code');
         end
         else begin
              Result := FError;
              exit;
         end;
    end
    else begin
         // Read the end of image marker.
         EndMarker := FLiteralCode.Decompress;
         if (EndMarker <> 256) 
         then begin
              FError := EC_BADFORMAT;
              raise EPNGException.Create('PNG, Bad code');
         end;
    end;
  until (FFinalDataSet) or (FError <> EC_OK);

  // Make sure that we read the last 4 byte for Adler check!
  FBufIndex := FChunk.Length - 4;

  // Read Adler CRC value
  CheckValue := (GetIDataByte shl 24) or
                (GetIDataByte shl 16) or
                (GetIDataByte shl 08) or
                GetIDataByte;

  // Compare CRC values
  if (CheckValue <> FStreamAdler)
  then begin
       FError := EC_CRC;
       raise EPNGException.Create('PNG, Adler CRC Error');
  end;
  Result := FError;
end; // TmcmPNGImage.CheckAdler.


function TmcmPNGImage.ReadImageData : TmcmErrorCode;
var HData1 : byte;
    HData2 : byte;
    i      : word;
begin
  FLZWin := AllocMem(PNGWindowSize);
  FLiteralCode  := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);
  FDistanceCode := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);
  FLengthCode   := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);

  try
    if Assigned(FLiteralCode) and
       Assigned(FDistanceCode) and
       Assigned(FLengthCode) and
       Assigned(FLZWin)
    then begin
         // Assign GetNBits to TmcmHuffmanPNG objects.
         FLiteralCode.GetNBits  := GetNBits;
         FDistanceCode.GetNBits := GetNBits;
         FLengthCode.GetNBits   := GetNBits;

         // Initialise Adler CRC.
         FStreamAdler := 1;

         // Initialise the LZ compression variables.
         FWinPos      := 0;
         FCopyPos     := 0;
         FCopyCount   := 0;

         // Read ZLIB Compression header.
         HData1 := GetIDataByte;
         FCompressMethod := HData1 and $0F;
         FWindowSize     := 1 shl (8 + ((HData1 and $F0) shr 4));

         HData2 := GetIDataByte;
         FPresetDir      := (HData2 and (1 shl 5));
         FCompressLevel  := (HData2 and $C0) shr 6;

         // Check that header is correct.
         if (FCompressMethod <> $08) or
            (((HData2 or (HData1 shl 8)) mod 31) <> 0) or
            (FPresetDir <> 0) or
            (FWindowSize > (1 shl 15))
         then FError := EC_DECOMPRESSION
         else begin
              // Allocate filter buffers.
              FRowBuffer[false] := AllocMem(FBytesPerRow * SizeOf(byte));
              FRowBuffer[true] := AllocMem(FBytesPerRow * SizeOf(byte));
              try
                if (NewCompressBlock = EC_OK) and
                   Assigned(FRowBuffer[false]) and
                   Assigned(FRowBuffer[true])
                then begin
                     // Read the actual image data.
                     if (FPNGHeader.Interlaced = 0)
                     then begin // Non-interlaced.
                          ReadNonInterlaced;
                     end
                     else begin // Adam 7 interlaced.
                          for i := 0 to (PNGInterlaceCount - 1)
                          do begin
                             FInterlacePass := i;
                             ReadInterlaced(i);
                          end;
                     end;
                end;
                FFinalDataSet := True;
                // Check that the decompressed data is correct.
                if (FError = EC_Ok)
                then FError := CheckAdler;
              finally
                // Release filter buffer.
                if Assigned(FRowBuffer[false])
                then FreeMem(FRowBuffer[false]);
                FRowBuffer[false] := Nil;
                if Assigned(FRowBuffer[true])
                then FreeMem(FRowBuffer[true]);
                FRowBuffer[true] := Nil;
              end;
         end;
    end
    else FError := EC_NOMEMORY;
  finally
    if Assigned(FLiteralCode)
    then FLiteralCode.Free;
    if Assigned(FDistanceCode)
    then FDistanceCode.Free;
    if Assigned(FLengthCode)
    then FLengthCode.Free;
    if Assigned(FLZWin)
    then FreeMem(FLZWin);
    FLZWin := Nil;
    Result := FError;
  end;
end; // TmcmPNGImage.ReadImageData.


procedure TmcmPNGImage.ExtractText(Chunk : TPNGChunk);
var Keyword : AnsiString;
    TextStr : AnsiString;
    i, Len  : word;
begin
  if Assigned(FImageInfo)
  then begin
       try
         i := {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrLen(PAnsiChar(Chunk.Data)) + 1;
         Len := Chunk.Length - i;
         if (Len > 0)
         then begin
              Keyword := {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPas(PAnsiChar(Chunk.Data));
              SetLength(TextStr, Len);
              {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(@TextStr[1]), PAnsiChar(Chunk.Data) + i, Len);
              if (Keyword = 'Author')
              then FImageInfo.Artist := String(TextStr);
              if (Keyword = 'Comment')
              then FImageInfo.Description := FImageInfo.Description + #13 +
                                             'COMMENT' + #13 +
                                             String(TextStr);
              if (Keyword = 'Copyright')
              then FImageInfo.Copyright := String(TextStr);
              if (Keyword = 'Creation Time')
              then begin
                   ; // FImageInfo.DateTime := StrPas(Chunk.Data);
              end;
              if (Keyword = 'Description')
              then FImageInfo.Description := FImageInfo.Description + String(TextStr);
              if (Keyword = 'Disclaimer')
              then FImageInfo.Description := FImageInfo.Description + #13 +
                                             'DISCLAIMER' + #13 +
                                             String(TextStr);
              if (Keyword = 'Software')
              then FImageInfo.Software := String(TextStr);
              if (Keyword = 'Source')
              then FImageInfo.HostComputer := String(TextStr);
              if (Keyword = 'Title')
              then FImageInfo.DocumentName := String(TextStr);
              if (Keyword = 'Warning')
              then FImageInfo.Description := FImageInfo.Description + #13 +
                                             'WARNING' + #13 +
                                             String(TextStr);
         end;
       except
       end;
  end;
end; // TmcmPNGImage.ExtractText.


procedure TmcmPNGImage.LoadFromStream(Stream : TStream);
var i : word;
begin
  FDibHandle := 0;
  FStream.Source := Stream;
  try
    // Read Header.
    if (ReadHeader(Stream) = EC_OK)
    then begin
         ReadChunk(Stream, FChunk);
         while (FChunk.Name <> 'IEND') and (FError = EC_OK)
         do begin
            // Create & load/set Palette
            if (FChunk.Name = 'PLTE')
            then ReadPalette(FChunk);

            // Background color, used if image is being displayed in a window
            // large than the image it self.
            if (FChunk.Name = 'bKGD') and (FChunk.Length > 0)
            then begin
                 case FPNGHeader.ColorType of
                 // Greyscale
                 0,
                 // Greyscale with alpha channel.
                 4 : FBkGrey := PWord(FChunk.Data)^;
                 // Palette
                 3 : begin
                       FBkGrey  := PByte(FChunk.Data)^;
                       FBkRed   := FDIBInfo^.bmiColors[FBkGrey].rgbRed;
                       FBkGreen := FDIBInfo^.bmiColors[FBkGrey].rgbGreen;
                       FBkBlue  := FDIBInfo^.bmiColors[FBkGrey].rgbRed;
                     end;
                 // RGB
                 2,
                 // RGB with alpha channel.
                 6 : begin
                       i := 0;
                       FBkRed   := PVectorB(FChunk.Data)^[i];
                       inc(i,2);
                       FBkGreen := PVectorB(FChunk.Data)^[i];
                       inc(i,2);
                       FBkBlue  := PVectorB(FChunk.Data)^[i];
                     end;
                 end;
            end;

            {
            // CIE, device independent color specifications.
            if (FChunk.Name = 'cHRM')
            then ;
            }

            {
            // Gamma value.
            if (FChunk.Name = 'gAMA')
            then ;
            }

            {
            // Histogram, color frequencies.
            if (FChunk.Name = 'hIST')
            then ;
            }

            // Physical pixel size.
            if (FChunk.Name = 'pHYs')
            then begin
                 if Assigned(FImageInfo)
                 then begin
                      with TPNGPixelSize(FChunk.Data^)
                      do begin
                         case UnitSpecifier of
                         0 : begin
                               FImageInfo.PixelWidth := SwapLong(XPixelPerUnit);
                               FImageInfo.PixelHeight := SwapLong(YPixelPerUnit);
                             end;
                         1 : begin
                               FImageInfo.Units := UN_METERS;
                               FDibInfo^.bmiHeader.biXPelsPerMeter := SwapLong(XPixelPerUnit);
                               FDibInfo^.bmiHeader.biYPelsPerMeter := SwapLong(YPixelPerUnit);
                               FImageInfo.XResolution := FDibInfo^.bmiHeader.biXPelsPerMeter;
                               FImageInfo.YResolution := FDibInfo^.bmiHeader.biYPelsPerMeter;
                             end;
                         end;
                      end;
                 end;
            end;

            {
            // Number of significant bits in original image
            if (FChunk.Name = 'sBIT')
            then ;
            }

            // Text, un-compressed
            if (FChunk.Name = 'tEXt')
            then ExtractText(FChunk);

            // Time when image was last modified.
            if (FChunk.Name = 'tIME')
            then begin
                 try
                   if Assigned(FImageInfo)
                   then with TPNGTime(FChunk.Data^)
                        do FImageInfo.DateTime := EncodeTime(Hour, Minute, Second, 0) +
                                                  EncodeDate(HiByte(Year) + (LoByte(Year) shl 8), Month, Day);
                 except
                   // Disregard date/time exceptions.
                 end;
            end;

            {
            // Transparency, when Alpha channel is not used.
            if (FChunk.Name = 'tRNS')
            then ;
            }

            {
            // Transparency, when Alpha channel is not used.
            if (FChunk.Name = 'tRNS')
            then ;
            }

            {
            // Text, compressed
            if (FChunk.Name = 'zTXt')
            then ;
            }

            if (FChunk.Name = 'IDAT')
            then begin
                 // Load image data.
                 if CreateDIB
                 then ReadImageData;
            end;
            ReadChunk(Stream, FChunk);
         end;
         if (Stream.Position <> FStream.Position)
         then Stream.Position := FStream.Position;
    end;
  except
    On E:EPNGException
    do ;
  end;
end; // TmcmPNGImage.LoadFromStream.


procedure TmcmPNGImage.SetNBits(Value : longword; Count : longword);
var i : integer;
begin
  for i := 0 to (Count - 1)
  do begin
     if (FBufBit >= 8)
     then FlushBits;

     if ((Value and BitMask[i]) <> 0)
     then FBufByte := FBufByte or BitMask[FBufBit];
     inc(FBufBit);
  end;
end; // TmcmPNGImage.SetNBits.


procedure TmcmPNGImage.SetIDataByte(Value : byte);
begin
  inc(FBufIndex);

  FBufBit   := 0;
  FBufByte  := 0;
end; // TmcmPNGImage.SetIDataByte.


procedure TmcmPNGImage.FlushBits;
begin
  if (FBufBit <> 0)
  then begin
       FMemStream.Write(FBufByte, 1);
       inc(FBufIndex);
       if (FBufIndex >= PNGOutputBufferSize)
       then begin
            WriteChunk(FStream.Source);
            FMemStream.Clear;
            FMemStream.Write('IDAT', 4);
       end;

       FBufBit   := 0;
       FBufByte  := 0;
  end;
end; // TmcmPNGImage.FlushBits.


procedure TmcmPNGImage.InitializeHashTable;
var i : cardinal;
begin
  for i := 0 to (FHashValues.Count - 1)
  do begin
     PPNGHashEntry(FHashValues.Items[i])^.Index := i;
     PPNGHashEntry(FHashValues.Items[i])^.Next     := Nil;
     PPNGHashEntry(FHashValues.Items[i])^.Previous := Nil;
  end;

  for i := 0 to (FHashTable.Count - 1)
  do begin
     PPNGHashEntry(FHashTable.Items[i])^.Index    := 0;
     PPNGHashEntry(FHashTable.Items[i])^.Next     := Nil;
     PPNGHashEntry(FHashTable.Items[i])^.Previous := Nil;
  end;

  PPNGHashEntry(FHashTable.Items[0])^.Next := FHashValues.Items[PNGWindowSize-1];
  PPNGHashEntry(FHashValues.Items[PNGWindowSize-1])^.Next := FHashTable.Items[0];


  i := (PNGWindowSize - 2);
  while (i > (PNGWindowSize - PNGLongestLength - 1))
  do begin
     PPNGHashEntry(FHashValues.Items[i+1])^.Next := FHashValues.Items[i];
     PPNGHashEntry(FHashValues.Items[i])^.Previous := FHashValues.Items[i+1];
     dec(i);
  end;
end; // TmcmPNGImage.InitializeHashTable.


function TmcmPNGImage.GetHashValue(Index : cardinal) : cardinal;
// Calculates Hash value from 3 bytes at Index in LookAheadBuffer.
var v1, v2, v3 : cardinal;
begin
  v1 := Index and PNGLookAheadMask;
  v2 := (Index + 1) and PNGLookAheadMask;
  v3 := (Index + 2) and PNGLookAheadMask;
  Result := (FLookAheadBuffer^[v1] and PNGHashMask) or
            ((FLookAheadBuffer^[v2] and PNGHashMask) shl PNGHashBits) or
            ((FLookAheadBuffer^[v3] and PNGHashMask) shl 2 * PNGHashBits);
end; // TmcmPNGImage.GetHashValue.


procedure TmcmPNGImage.LongestMatch(var BestLength : cardinal;
                                    var BestOffset : cardinal);
// Locate longest "string" of data in LZ window, that matches the data in the
// look ahead buffer.
var Value     : cardinal;
    Chain     : cardinal;
    Current   : PPNGHashEntry;
    Src       : cardinal;
    Dest      : cardinal;
    Len       : cardinal;
begin
  BestLength := 0;

  Value := GetHashValue(FLookAheadPos);

  Chain := 0;
  Current := PPNGHashEntry(FHashTable[Value])^.Next;
  while (Current <> Nil) and
        (Chain < FSearchLimit) and
        (BestLength < PNGLongestLength)
  do begin
     Src  := FLookAheadPos;
     Dest := Current^.Index;
     Len  := 0;
     while (Len < PNGLongestLength) and
           (((Current^.Index + Len) and PNGWindowMask) <> FWinPos)
     do begin
        if (FLookAheadBuffer^[Src] <> FLZWin^[Dest])
        then Break;

        inc(Len);
        Dest := (Dest + 1) and PNGWindowMask;
        Src  := (Src + 1) and PNGLookAheadMask;
     end;

     if (Len >= 3) and (Len > BestLength)
     then begin
          BestLength := Len;
          BestOffset := PNGWindowSize -
                        ((PNGWindowSize + Current^.Index - FWinPos) and PNGWindowMask);
     end;

     Current := Current^.Next;
     inc(Chain);
  end;
end; // TmcmPNGImage.LongestMatch.


procedure TmcmPNGImage.FillLookAheadBuffer(Count : longword); // cardinal);
var i, Index : cardinal;
begin
  Index := (FLookAheadPos + PNGLookAheadSize - Count) and PNGLookAheadMask;
  if (FDibInfo^.bmiHeader.biBitCount in [1, 4, 8])
  then begin
       i := 0;
       while (i < Count) and Not(FEndOfImage)
       do begin
          if (FCol >= FBytesPerRow)
          then begin
               FCol := 0;
               inc(FRow);
               if (FRow >= FDibInfo^.bmiHeader.biHeight)
               then begin
                    FLookAheadBuffer^[Index and PNGLookAheadMask] := PNGEndImage;
                    FEndOfImage := True;
               end
               else begin
                    FilterRow(FRow);
                    FLookAheadBuffer^[Index and PNGLookAheadMask] := byte(FFilterCurrent);
                    UpdateAdler(FLookAheadBuffer^[Index and PNGLookAheadMask]);
               end;
          end
          else begin
               FLookAheadBuffer^[Index and PNGLookAheadMask] := FFilterBuffer[byte(FFilterCurrent)]^[FCol];
               UpdateAdler(FLookAheadBuffer^[Index and PNGLookAheadMask]);
               inc(FCol);
          end;

          inc(Index);
          inc(i);
       end;
  end
  else begin // RGB
       i := 0;
       while (i < Count) and Not(FEndOfImage)
       do begin
          if (FCol >= FBytesPerRow)
          then begin
               FCol := 0;
               inc(FRow);
               if (FRow >= FDibInfo^.bmiHeader.biHeight)
               then begin
                    FLookAheadBuffer^[Index and PNGLookAheadMask] := PNGEndImage;
                    FEndOfImage := True;
               end
               else begin
                    FilterRow(FRow);
                    FLookAheadBuffer^[Index and PNGLookAheadMask] := byte(FFilterCurrent);
                    UpdateAdler(FLookAheadBuffer^[Index and PNGLookAheadMask]);
               end;
          end
          else begin
               FLookAheadBuffer^[Index and PNGLookAheadMask] := FFilterBuffer[byte(FFilterCurrent)]^[FCol];
               UpdateAdler(FLookAheadBuffer^[Index and PNGLookAheadMask]);
               inc(FCol);
          end;

          inc(Index);
          inc(i);
       end;
  end;
end; // TmcmPNGImage.FillLookAheadBuffer.


procedure TmcmPNGImage.MoveHashEntry(Entry : cardinal; HashValue : cardinal);
var HashEntry : PPNGHashEntry;
begin
  HashEntry := PPNGHashEntry(FHashValues[Entry]);
  if (HashEntry^.Previous <> Nil)
  then HashEntry^.Previous^.Next := HashEntry^.Next;

  if (HashEntry^.Next <> Nil)
  then HashEntry^.Next^.Previous := HashEntry^.Previous;

  HashEntry^.Next := PPNGHashEntry(FHashTable[HashValue])^.Next;
  HashEntry^.Previous := PPNGHashEntry(FHashTable[HashValue]);

  PPNGHashEntry(FHashTable[HashValue])^.Next := HashEntry;

  if (HashEntry^.Next <> Nil)
  then HashEntry^.Next^.Previous := HashEntry;
end; // TmcmPNGImage.MoveHashEntry.


procedure TmcmPNGImage.GetLengthCounts(Index : cardinal;
                                       Code  : cardinal;
                                       Extra : cardinal;
                                       Value : Cardinal);
begin
  FLengthCode.Frequency[Code] := FLengthCode.Frequency[Code] + 1;
end; // TmcmPNGImage.GetLengthCounts.


procedure TmcmPNGImage.WriteLengthCounts(Index : cardinal;
                                         Code  : cardinal;
                                         Extra : cardinal;
                                         Value : Cardinal);
var HuffmanCode : cardinal;
    HuffmanSize : cardinal;
begin
  HuffmanCode := FLengthCode.HuffCode[Code];
  HuffmanSize := FLengthCode.HuffSize[Code];
  SetNBits(HuffmanCode, HuffmanSize);

  if (Extra <> 0)
  then SetNBits(Value, Extra);
end; // TmcmPNGImage.WriteLengthCounts.


procedure TmcmPNGImage.FindLengthCodes(Huffman : TmcmHuffmanPNG;
                                       Count   : longint;
                                       Encoder : TPNGHuffmanEncode);
var i, j, k : longint;
begin
  i := 0;
  while (i < Count)
  do begin
     if (Huffman.HuffSize[i] <> 0)
     then begin
          Encoder(i, Huffman.HuffSize[i], 0, 0);
          j := i + 1;
     end
     else begin
          // Compact runs of zero length codes.
          // Find number of consecutive zero runs.
          j := i + 1;
          while (Huffman.HuffSize[j] = Huffman.HuffSize[j-1]) and (j < Count)
          do inc(j);

          // To compact, there must be 3 consecutive zero runs.
          case (j - i) of
          1 : Encoder(i, Huffman.HuffSize[i], 0, 0);
          2 : begin
                Encoder(i, Huffman.HuffSize[i], 0, 0);
                Encoder(i + 1, Huffman.HuffSize[i], 0, 0);
              end;
          else begin
               k := j - i;
               if (k > 138)
               then begin
                    k := 138;
                    j := i + k;
               end;
               if (k > 10)
               then Encoder(i, 18, 7, k - 11)
               else Encoder(i, 17, 3, k - 3);
          end;
          end;
     end;
     i := j;
  end;
//  Encoder(0, 0, 0, 0);
end; // TmcmPNGImage.FindLengthCodes.


procedure TmcmPNGImage.ImageRowToNonInterlaced(Row : longint);
var ImageIndex : cardinal;
    a, i, j    : longint;
begin
  a := Row and $01;
 // b := 1 - a;
  ImageIndex := (FDibInfo^.bmiHeader.biHeight - 1 - Row) * FLongWidth;

  case FPNGHeader.ColorType of
  // Greyscale
  0,
  // Palette
  3 : begin
      //  FPreFilterBuffer[b] := FPreFilterBuffer[a];
        FPreFilterBuffer[a] := @PVectorB(FDibBits)^[ImageIndex];
      end;
  // RGB
  2 : begin
        i := 0;
        j := ImageIndex;
        while (i < FBytesPerRow)
        do begin
           FPreFilterBuffer[a]^[i] := PVectorB(FDibBits)^[j+2];
           inc(i);
           FPreFilterBuffer[a]^[i] := PVectorB(FDibBits)^[j+1];
           inc(i);
           FPreFilterBuffer[a]^[i] := PVectorB(FDibBits)^[j];
           inc(i);
           inc(j, 3);
        end;
      end;
  // Greyscale with alpha channel.
  4 : begin
      end;
  // RGB with alpha channel.
  6 : begin
      end;
  end;
end; // TmcmPNGImage.ImageRowToNonInterlaced.


procedure TmcmPNGImage.FilterRow(Row : longint);
var //ImageIndex : cardinal;
    a, b       : longint;
    i, j       : cardinal;
    Mask       : cardinal;
    Last       : byte;
    Above      : byte;
    LastAbove  : byte;
    ThisRun    : cardinal;
    LongestRun : cardinal;
begin
  ImageRowToNonInterlaced(Row);
  a := Row and $01;
  b := 1 - a;

  // Filter None.
  Mask := 1 shl word(PNGFLT_NONE);
  for i := 0 to (FBytesPerRow - 1)
  do FFilterBuffer[word(PNGFLT_NONE)]^[i] := FPreFilterBuffer[a]^[i];

  // Subtract filter.
  if (FFilterMask and (1 shl word(PNGFLT_SUB)) <> 0)
  then begin
       Mask := Mask or (1 shl word(PNGFLT_SUB));
       j := 0;
       for i := 0 to (FBytesPerRow - 1)
       do begin
          if (i >= FFilterOffset)
          then begin
               Last := FPreFilterBuffer[a]^[j];
               inc(j);
          end
          else Last := 0;
          FFilterBuffer[word(PNGFLT_SUB)]^[i] := FPreFilterBuffer[a]^[i] - Last;
       end;
  end;

  // Up filter.
  if (FFilterMask and (1 shl word(PNGFLT_UP)) <> 0) and (Row > 0)
  then begin
       Mask := Mask or (1 shl word(PNGFLT_UP));
       for i := 0 to (FBytesPerRow - 1)
       do FFilterBuffer[word(PNGFLT_UP)]^[i] := FPreFilterBuffer[a]^[i] -
                                                FPreFilterBuffer[b]^[i];
  end;

  // Average filter.
  if (FFilterMask and (1 shl word(PNGFLT_AVERAGE)) <> 0) and (Row > 0)
  then begin
       Mask := Mask or (1 shl word(PNGFLT_AVERAGE));
       j := 0;
       for i := 0 to (FBytesPerRow - 1)
       do begin
          if (i >= FFilterOffset)
          then begin
               Last := FPreFilterBuffer[a]^[j];
               inc(j);
          end
          else Last := 0;
          FFilterBuffer[word(PNGFLT_AVERAGE)]^[i] := FPreFilterBuffer[a]^[i] -
                                                     (Last + FPreFilterBuffer[b]^[i]) div 2;
       end;
  end;

  // Paeth filter.
  if (FFilterMask and (1 shl word(PNGFLT_PAETH)) <> 0) and (Row > 0)
  then begin
       Mask := Mask or (1 shl word(PNGFLT_PAETH));
       j := 0;
       for i := 0 to (FBytesPerRow - 1)
       do begin
          if (i >= FFilterOffset)
          then begin
               Last := FPreFilterBuffer[a]^[j];
               LastAbove := FPreFilterBuffer[b]^[j];
               inc(j);
          end
          else begin
               Last      := 0;
               LastAbove := 0;
          end;
          Above := FPreFilterBuffer[b]^[i];

          FFilterBuffer[word(PNGFLT_PAETH)]^[i] := FPreFilterBuffer[a]^[i] -
                                                   PaethPredictor(Last, Above, LastAbove);
       end;
  end;

  FFilterCurrent := PNGFLT_NONE;
  if (Mask <> (1 shl word(PNGFLT_NONE)))
  then begin
       // Find the filter that results in the best compression.
       LongestRun := 0;
       for i := 0 to 4
       do begin
          if ((Mask and (1 shl i)) <> 0)
          then begin
               ThisRun := 0;
               for j := 4 to (FBytesPerRow - 1)
               do begin
                  if (FFilterBuffer[i]^[j] = FFilterBuffer[i]^[j-1]) and
                     (FFilterBuffer[i]^[j] = FFilterBuffer[i]^[j-2]) and
                     (FFilterBuffer[i]^[j] = FFilterBuffer[i]^[j-3]) and
                     (FFilterBuffer[i]^[j] = FFilterBuffer[i]^[j-4])
                  then inc(ThisRun);
               end;

               if (LongestRun < ThisRun)
               then begin
                    LongestRun := ThisRun;
                    FFilterCurrent := TPNGFilters(i);
               end;
          end;
       end;
  end;
end; // TmcmPNGImage.FilterRow.


function TmcmPNGImage.ProcessImageData : boolean;
var Length    : cardinal;
    Offset    : cardinal;
    Literal   : byte;
    HashValue : cardinal;
    Source    : cardinal;
    i         : cardinal;
    Code      : cardinal;
begin
  InitializeHashTable;

  FBlockBufferPos := 0;
  while (FBlockBufferPos < FBlockBufferSize - 1) and
        (FLookAheadBuffer^[FLookAheadPos] <> PNGEndImage)
  do begin
     LongestMatch(Length, Offset);

     if (Length = 0)
     then begin // Not match of min 3 bytes.
          Literal := FLookAheadBuffer^[FLookAheadPos];
          HashValue := GetHashValue(FLookAheadPos);
          MoveHashEntry(FWinPos, HashValue);

          FLZWin^[FWinPos] := Literal;

          FBlockBuffer^[FBlockBufferPos] := Literal;
          FLiteralCode.Frequency[Literal] := FLiteralCode.Frequency[Literal] + 1;

          FLookAheadPos := (FLookAheadPos + 1) and PNGLookAheadMask;
          FWinPos := (FWinPos + 1) and PNGWindowMask;

          FillLookAheadBuffer(1);
     end
     else begin
          Source := (PNGWindowSize + FWinPos - Offset) and PNGWindowMask;
          for i := 0 to (Length - 1)
          do begin
             HashValue := GetHashValue(FLookAheadPos);
             MoveHashEntry(FWinPos, HashValue);

             FLZWin^[FWinPos] := FLZWin^[Source and PNGWindowMask];
             FLookAheadPos := (FLookAheadPos + 1) and PNGLookAheadMask;
             FWinPos := (FWinPos + 1) and PNGWindowMask;

             inc(Source);
          end;
          FBlockBuffer^[FBlockBufferPos] := 256 + Length;
          inc(FBlockBufferPos);
          FBlockBuffer^[FBlockBufferPos] := Offset;

          // Get length code
          Code := PNGLengthCodes[Length-3];
          FLiteralCode.Frequency[Code] := FLiteralCode.Frequency[Code] + 1;

          // Get Distance code.
          Code := 0;
          while (Code < PNGMaxDistanceCode) and (Offset > PNGMaxDist[Code])
          do inc(Code);
          FDistanceCode.Frequency[Code] := FDistanceCode.Frequency[Code] + 1;

          FillLookAheadBuffer(Length);
     end;

     inc(FBlockBufferPos);
  end;
  FLiteralCode.Frequency[PNGEndCode] := FLiteralCode.Frequency[PNGEndCode] + 1;

  Result := (FLookAheadBuffer^[FLookAheadPos] <> PNGEndImage);
end; // TmcmPNGImage.ProcessImageData.


procedure TmcmPNGImage.WriteDeflateHeader(IsLastBlock : boolean);
var NoOfLiterals  : cardinal;
    NoOfDistances : cardinal;
    NoOfLengths   : cardinal;
    i             : cardinal;
begin
  // Get number of Literal/Length codes.
  NoOfLiterals := PNGMaxHuffmanCodes;
  while (NoOfLiterals > 0) and (FLiteralCode.HuffSize[NoOfLiterals-1] = 0)
  do dec(NoOfLiterals);

  // Get number of Distance codes.
  NoOfDistances := PNGMaxHuffmanCodes;
  while (NoOfDistances > 0) and (FDistanceCode.HuffSize[NoOfDistances-1] = 0)
  do dec(NoOfDistances);

  FLengthCode.ClearTable;
  FindLengthCodes(FLiteralCode, NoOfLiterals, Self.GetLengthCounts);
  FindLengthCodes(FDistanceCode, NoOfDistances, Self.GetLengthCounts);
  FLengthCode.CreateWriteTable(7 {PngMaxLengthLengthCodeSize });

  // Get number of Length codes to store.
  NoOfLengths := PNGLengthSize;
  while (NoOfLengths > 0) and (FLengthCode.HuffSize[PNGLengthOrder[NoOfLengths-1]] = 0)
  do dec(NoOfLengths);

  // Write deflate header to the IDAT block
  // Last data block ?
  if IsLastBlock
  then SetNBits(1, 1)
  else SetNBits(0, 1);

  // Write compression type - Dynamic Huffman codes is used!
  FCompressType := CPPNG_DYNAMICHUFMANN;
  SetNBits(word(FCompressType), 2);

  // Write number of respective Huffman codes that are used.
  SetNBits(NoOfLiterals - 257, 5);
  SetNBits(NoOfDistances - 1, 5);
  SetNBits(NoOfLengths - 4, 4);

  // Write Length - Huffman table lengths used to encode Length/Literal and
  // Distance Huffman tables.
  for i := 0 to (NoOfLengths - 1)
  do SetNBits(FLengthCode.HuffSize[PNGLengthOrder[i]], 3);

  // Huffman encode the Length/Literal Huffman table.
  FindLengthCodes(FLiteralCode, NoOfLiterals, Self.WriteLengthCounts);

  // Huffman encode the Distance Huffman table.
  FindLengthCodes(FDistanceCode, NoOfDistances, Self.WriteLengthCounts);
end; // TmcmPNGImage.WriteDeflateHeader.


function TmcmPNGImage.WriteBlockBuffer(Stream : TStream) : TmcmErrorCode;
// Huffman encodes and outputs data.
// Buffer data:
//     0..255 : Literal byte
//   256..514 : Length code of N - 256, followed by a distance code.
var HuffmanCode : word;
    HuffmanSize : word;
    i           : cardinal;
    Code        : cardinal;
    Extra       : cardinal;
    Value       : cardinal;
    Limit       : cardinal;
    Length      : cardinal;
    Distance    : cardinal;
begin
  i := 0;
  Limit := FBlockBufferPos;
  while (i < Limit)
  do begin
     if (FBlockBuffer^[i] < 256)
     then begin
          HuffmanCode := FLiteralCode.HuffCode[FBlockBuffer^[i]];
          HuffmanSize := FLiteralCode.HuffSize[FBlockBuffer^[i]];
          SetNBits(HuffmanCode, HuffmanSize);
          inc(i);
     end
     else begin
          Length := FBlockBuffer^[i] - 256;
          inc(i);
          Distance := FBlockBuffer^[i];
          inc(i);

          Code   := PNGLengthCodes[Length-3];
          Extra  := PNGLengthExtra[Code-PNGFirstLengthCode];
          Value  := Length - PNGLengthBase[Code-PNGFirstLengthCode];

          HuffmanCode := FLiteralCode.HuffCode[Code];
          HuffmanSize := FLiteralCode.HuffSize[Code];
          SetNBits(HuffmanCode, HuffmanSize);

          if (Extra <> 0)
          then SetNBits(Value, Extra);

          Code := 0;
          while (Code < PNGMaxDistanceCode) and (Distance > PNGMaxDist[Code])
          do inc(Code);
          Extra := PNGDistExtra[Code];
          Value := Distance - PNGDistBase[Code];

          HuffmanCode := FDistanceCode.HuffCode[Code];
          HuffmanSize := FDistanceCode.HuffSize[Code];

          SetNBits(HuffmanCode, HuffmanSize);
          if (Extra <> 0)
          then SetNBits(Value, Extra);
     end;
  end;
  // Output PNG end code.
  HuffmanCode := FLiteralCode.HuffCode[PNGEndCode];
  HuffmanSize := FLiteralCode.HuffSize[PNGEndCode];
  SetNBits(HuffmanCode, HuffmanSize);

  Result := FError;
end; // TmcmPNGImage.WriteBlockBuffer.


function TmcmPNGImage.WriteImageData(Stream : TStream) : TmcmErrorCode;
var i           : longint;
    HasMoreData : boolean;
    Cmf, Flag   : byte;
    Check       : word;
begin
  case FQuality of
  0..25  : FCompressLevel := 0;
  26..50 : FCompressLevel := 1;
  51..75 : FCompressLevel := 2;
  else FCompressLevel := 3;
  end;

  case FCompressLevel of
  0 : FSearchLimit := 1;
  1 : FSearchLimit := 64;
  2 : FSearchLimit := 128;
  3 : FSearchLimit := $7FFF;
  end;

  FBufBit   := 0;
  FBufByte  := 0;
  FBufIndex := 0;

  FHashTable        := TList.Create;
  FHashTable.Count  := PNGHashTableSize;
  for i := 0 to (FHashTable.Count - 1)
  do FHashTable.Items[i] := AllocMem(SizeOf(TPNGHashEntry));

  FHashValues       := TList.Create;
  FHashValues.Count := PNGWindowSize;
  for i := 0 to (FHashValues.Count - 1)
  do FHashValues.Items[i] := AllocMem(SizeOf(TPNGHashEntry));

  FLZWin           := AllocMem(PNGWindowSize);
  FLookAheadBuffer := AllocMem(PNGLookAheadSize * SizeOf(Word));

  FLiteralCode  := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);
  FDistanceCode := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);
  FLengthCode   := TmcmHuffmanPNG.Create(PNGMaxHuffmanCodes, PNGMaxLengthCodeSize);

  if (FDibInfo^.bmiHeader.biBitCount > 8)
  then begin
       FPreFilterBuffer[0] := AllocMem(FBytesPerRow);
       FPreFilterBuffer[1] := AllocMem(FBytesPerRow);
  end;

  FBlockBufferSize := 16384;
  FBlockBuffer := AllocMem(FBlockBufferSize * SizeOf(word));

  // Set-up use Filters
  FFilterMask := $FFFF; { -
                 BitMask[word(FLT_PAETH)] -
                 BitMask[word(FLT_AVERAGE)] -
                 BitMask[word(FLT_UP)] -
                 BitMask[word(FLT_SUB)]; }

  for i := 0 to 4
  do if (((1 shl i) and FFilterMask) <> 0)
     then FFilterBuffer[i] := AllocMem(FBytesPerRow)
     else FFilterBuffer[i] := Nil;

  try
    try
      if Assigned(FLiteralCode) and
         Assigned(FDistanceCode) and
         Assigned(FLengthCode)
      then begin
           // Initialise Adler CRC.
           FStreamAdler := 1;

           // Initial filling of LZ window
           FillChar(FLZWin^, PNGWindowSize, 0);
           FRow := -1;
           FCol := $7FFFFFFF;
           FLookAheadPos := 0;
           FWinPos := 0;
           FillLookAheadBuffer(PNGLookAheadSize);
           // FilterRow(0); Seems to cause an mis-interpretation !!!!!

           // Start first IDAT chunk
           FMemStream.Clear;
           FMemStream.Write('IDAT', 4);


           // Write ZLIB compression header.
           Cmf  := $078; // 7 -> 32 KBytes sliding window.
                         // 8 -> Deflate compression.
           Flag := FCompressLevel shl 6;
           Check := (Cmf shl 8) or Flag;
           Flag := Flag or (31 - (Check mod 31));
           SetNBits(Cmf, 8);
           SetNBits(Flag, 8);

           //
           HasMoreData := True;
           while HasMoreData
           do begin
              // Clear Huffman tables.
              FLiteralCode.ClearTable;
              FDistanceCode.ClearTable;

              // Process image data.
              HasMoreData := ProcessImageData;

              // Create Huffman tables for Literals/Length and Distances.
              FLiteralCode.CreateWriteTable(PNGMaxLengthCodeSize);
              FDistanceCode.CreateWriteTable(PNGMaxLengthCodeSize);

              // Write Deflate header.
              WriteDeflateHeader(Not(HasMoreData));

              // Write image data.
              WriteBlockBuffer(Stream);

           end;

           // Write Adler CRC value.
           FlushBits;
           FStreamAdler := SwapLong(FStreamAdler);
           SetNBits(FStreamAdler, 32);
           FlushBits;

           // Write current IDAT chunk to file.
           if (FMemStream.Size > 4)
           then WriteChunk(Stream);
      end
      else FError := EC_NOMEMORY;
    finally
      // Release TmcmHuffmanPNG instances.
      if Assigned(FLiteralCode)
      then FLiteralCode.Free;
      FLiteralCode := Nil;
      if Assigned(FDistanceCode)
      then FDistanceCode.Free;
      FDistanceCode := Nil;
      if Assigned(FLengthCode)
      then FLengthCode.Free;
      FLengthCode := Nil;

      // Release compression object.
      if Assigned(FCompObj)
      then FCompObj.Free;
      FCompObj := Nil;

      if (FDibInfo^.bmiHeader.biBitCount > 8)
      then begin
           FreeMem(FPreFilterBuffer[0]);
           FreeMem(FPreFilterBuffer[1]);
      end
      else begin
           FPreFilterBuffer[0] := Nil;
           FPreFilterBuffer[1] := Nil;
      end;

      // Release Filter buffers.
      for i := 0 to 4
      do begin
         if (FFilterBuffer[i] <> Nil)
         then FreeMem(FFilterBuffer[i]);
         FFilterBuffer[i] := Nil;
      end;

      if Assigned(FBlockBuffer)
      then FreeMem(FBlockBuffer);
      FBlockBuffer := Nil;

      if Assigned(FHashTable)
      then begin
           for i := 0 to (FHashTable.Count - 1)
           do FreeMem(FHashTable.Items[i], SizeOf(TPNGHashEntry));
           FHashTable.Free;
      end;
      FHashTable := Nil;

      if Assigned(FHashValues)
      then begin
           for i := 0 to (FHashValues.Count - 1)
           do FreeMem(FHashValues.Items[i], SizeOf(TPNGHashEntry));
           FHashValues.Free;
      end;
      FHashValues := Nil;

      if Assigned(FLZWin)
      then FreeMem(FLZWin);
      FLZWin := Nil;

      if Assigned(FLookAheadBuffer)
      then FreeMem(FLookAheadBuffer);
      FLookAheadBuffer := Nil;

      Result := FError;
    end;
  except
      Result := FError;
  end;
end; // TmcmPNGImage.WriteImageData.


function TmcmPNGImage.WriteChunk(Stream : TStream) : TmcmErrorCode;
begin
  try
    if (FError = EC_OK)
    then begin
         // Write chunk size.
         FChunk.Length := SwapLong(FMemStream.Size - 4);
         // FChunk.Name   := Name;
         // FChunk.Data   := FMemStream.Memory;
         FChunk.CRC    := SwapLong(CalcCRC(FMemStream.Memory, FMemStream.Size));

         FStream.Write(FChunk.Length, 4);
         if (FMemStream.Size > 0)
         then FStream.Write(FMemStream.Memory^, FMemStream.Size);
         FStream.Write(FChunk.CRC, 4);
    end;
    FMemStream.Clear;

    FBufBit   := 0;
    FBufByte  := 0;
    FBufIndex := 0;
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmPNGImage.WriteChunk.


function TmcmPNGImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;
       FCompress := CP_PNG;
       CreateCRCTable;

       FMemStream.Clear;
       with FDibInfo^.bmiHeader, FPNGHeader
       do begin
          // Fill file header.
          Width       := SwapLong(biWidth);
          Height      := SwapLong(biHeight);
          case biBitCount of
          1  : BitDepth := 1;
          4  : BitDepth := 4;
          8,
          24,
          32 : BitDepth := 8;
          15 : FError := EC_BITDEPTH;
          16 : FError := EC_BITDEPTH;
          end;

          case GetPhotometric of
          PM_INVGREY : ColorType := 3;
          PM_GREY    : ColorType := 0;
          PM_RGB     : ColorType := 2;
          PM_PALETTE : ColorType := 3;
          PM_RGBA    : ColorType := 6;
          end;

          case ColorType of
          // Greyscale
          0,
          // Palette
          3 : begin
                FFilterOffset := 1;
                FBytesPerRow := (biWidth * FPNGHeader.BitDepth + 7) div 8;
              end;
          // RGB
          2 : begin
                FFilterOffset := (biBitCount) div 8;
                FBytesPerRow := (biWidth * FPNGHeader.BitDepth * 3) div 8;
              end;
          // Greyscale with alpha channel.
          4 : begin
                FFilterOffset := 2 * biBitCount div 8;
                FBytesPerRow := (2 * biWidth * FPNGHeader.BitDepth) div 8;
              end;
          // RGB with alpha channel.
          6 : begin
                FFilterOffset := biBitCount div 8;
                FBytesPerRow := (biWidth * FPNGHeader.BitDepth * 4) div 8;
              end;
          end;

          Compression := 0;
          Filter      := 0;

          if FInterlaced and false
          then Interlaced  := 1
          else Interlaced  := 0;
       end;
       FMemStream.Write('IHDR', 4);
       FMemStream.Write(FPNGHeader, SizeOf(TPNGHeader));
       WriteChunk(Stream);
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPNGImage.WriteHeader.


function TmcmPNGImage.WritePalette(Stream : TStream) : TmcmErrorCode;
var i : word;
begin
  try
    FNoColors := GetNumColors(FDibInfo);
    if (FNoColors > 0)
    then begin
         // Write palette values.
         FMemStream.Clear;
         FMemStream.Write('PLTE', 4);
         for i := 0 to Pred(FNoColors)
         do begin
            with FDIBInfo^.bmiColors[i]
            do begin
               FMemStream.Write(rgbRed, 1);
               FMemStream.Write(rgbGreen, 1);
               FMemStream.Write(rgbBlue, 1);
            end;
         end;
         WriteChunk(Stream);
    end;
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmPNGImage.WritePalette.


function TmcmPNGImage.WritePixelSize(Stream : TStream) : TmcmErrorCode;
var PixelSize : TPNGPixelSize;
begin
  if Assigned(FImageInfo)
  then begin
       FMemStream.Clear;
       FMemStream.Write('pHYs', 4);
       FImageInfo.Units := UN_METERS;
       PixelSize.UnitSpecifier := 1;
       PixelSize.XPixelPerUnit := SwapLong(Round(FImageInfo.XResolution)); // SwapLong(FImageInfo.PixelWidth);
       PixelSize.YPixelPerUnit := SwapLong(Round(FImageInfo.YResolution)); // SwapLong(FImageInfo.PixelHeight);
       FMemStream.Write(PixelSize, SizeOf(TPNGPixelSize));
       WriteChunk(Stream);
  end;
  Result := FError;
end; // TmcmPNGImage.WritePixelSize.


function TmcmPNGImage.WriteText(Stream : TStream) : TmcmErrorCode;
var i : integer;
begin
  if Assigned(FImageInfo)
  then begin
       i := 0;
       if (FImageInfo.Artist <> '') // Author
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Author', 6);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.Artist[1], Length(FImageInfo.Artist));
            WriteChunk(Stream);
       end;
       if (FImageInfo.Copyright <> '') // Copyright
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Copyright', 9);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.Copyright[1], Length(FImageInfo.Copyright));
            WriteChunk(Stream);
       end;
       if (FImageInfo.Description <> '') // Description
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Description', 11);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.Description[1], Length(FImageInfo.Description));
            WriteChunk(Stream);
       end;
       if (FImageInfo.Software <> '') // Software
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Software', 8);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.Software[1], Length(FImageInfo.Software));
            WriteChunk(Stream);
       end;
       if (FImageInfo.HostComputer <> '') // Source
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Source', 6);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.HostComputer[1], Length(FImageInfo.HostComputer));
            WriteChunk(Stream);
       end;
       if (FImageInfo.DocumentName <> '') // Title
       then begin
            FMemStream.Clear;
            FMemStream.Write('tEXt', 4);
            FMemStream.Write('Title', 5);
            FMemStream.Write(i, 1);
            FMemStream.Write(FImageInfo.DocumentName[1], Length(FImageInfo.DocumentName));
            WriteChunk(Stream);
       end;
  end;
  Result := FError;
end; // TmcmPNGImage.WriteText.


function TmcmPNGImage.WriteTime(Stream : TStream) : TmcmErrorCode;
var Time : TPNGTime;
    DT   : TDateTime;
    y, m, d : word;
    h, s, ms : word;
begin
  FMemStream.Clear;
  FMemStream.Write('tIME', 4);
  DT := Now;
  DecodeDate(DT, y, m, d);
  with Time
  do begin
     Year   := HiByte(y) + (LoByte(y) shl 8);
     Month  := m;
     Day    := d;
  end;
  DecodeTime(DT, h, m, s, ms);
  with Time
  do begin
     Hour   := h;
     Minute := m;
     Second := s;
  end;
  FMemStream.Write(Time, SizeOf(TPNGTime));
  WriteChunk(Stream);
  Result := FError;
end; // TmcmPNGImage.WriteTime.


procedure TmcmPNGImage.SaveToStream(Stream : TStream);
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write signature
            FStream.Write(PNGSignature, 8);

            // Write file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Palette.
                 if (FPNGHeader.ColorType = 3)
                 then WritePalette(Stream);

                 // Physical pixel size.
                 WritePixelSize(Stream);

                 // Physical pixel size.
                 WriteTime(Stream);

                 // Write Text, un-compressed
                 WriteText(Stream);

                 if (FError = EC_OK)
                 then begin
                      // Write bitmap data
                      WriteImageData(Stream);
                 end;

                 // Write end of file.
                 FMemStream.Clear;
                 FMemStream.Write('IEND', 4);
                 WriteChunk(Stream);
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmPNGImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmPxMImage
// Portable Pixel Map, Grey Map and Bi-Level Map.
//------------------------------------------------------------------------------

class function TmcmPBMImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW];
end; // TmcmPBMImage.GetColorFormats.


class function TmcmPBMImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_BW])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_ASCII_PBM;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_BIN_PBM;
       inc(Count);
  end;
  Result := Count;
end; // TmcmPBMImage.GetCompressionFromColor.


constructor TmcmPBMImage.Create;
begin
  Inherited Create;
  FFileFormat := FF_PBM;
end; // TmcmPBMImage.Create.


class function TmcmPGMImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_GREY8];
end; // TmcmPGMImage.GetColorFormats.


class function TmcmPGMImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_GREY8])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_ASCII_PGM;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_BIN_PGM;
       inc(Count);
  end;
  Result := Count;
end; // TmcmPGMImage.GetCompressionFromColor.


constructor TmcmPGMImage.Create;
begin
  Inherited Create;
  FFileFormat := FF_PGM;
end; // TmcmPGMImage.Create.


class function TmcmPPMImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_RGB24];
end; // TmcmPPMImage.GetColorFormats.


class function TmcmPPMImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_RGB24])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_ASCII_PPM;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_BIN_PPM;
       inc(Count);
  end;
  Result := Count;
end; // TmcmPPMImage.GetCompressionFromColor.


constructor TmcmPPMImage.Create;
begin
  Inherited Create;
  FFileFormat := FF_PPM;
end; // TmcmPPMImage.Create.


class function TmcmPxMImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [];
end; // TmcmPxMImage.GetColorFormats.


class function TmcmPxMImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_ASCII_PBM,
  CP_ASCII_PGM,
  CP_ASCII_PPM : Result := resCP_ASCII;
  CP_BIN_PBM,
  CP_BIN_PGM,
  CP_BIN_PPM   : Result := resCP_BIN;
  else Result := '';
  end;
end; // TmcmPxMImage.GetCompressionName.


constructor TmcmPxMImage.Create;
begin
  Inherited Create;
end; // TmcmPxMImage.Create.


destructor TmcmPxMImage.Destroy;
begin
  Inherited Destroy;
end; // TmcmPxMImage.Destroy.


function TmcmPxMImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var TestPortable : array[0..1] of AnsiChar;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStreamStart := FStream.Position;
         FStream.Read(TestPortable, SizeOf(TestPortable));
         if (TestPortable[0] = 'P')
         then begin
              FCompress := CP_NOCOMP;
              case TestPortable[1] of
              '1' : FCompress := CP_ASCII_PBM; // Portable Bi-level ASCII
              '2' : FCompress := CP_ASCII_PGM; // Portable Greymap ASCII
              '3' : FCompress := CP_ASCII_PPM; // Portable Pixelmap ASCII
              '4' : FCompress := CP_BIN_PBM;   // Portable Bi-level ASCII
              '5' : FCompress := CP_BIN_PGM;   // Portable Greymap Binary
              '6' : FCompress := CP_BIN_PPM;   // Portable Pixelmap Binary
              else FCompress := CP_NOCOMP;
              end;
              if (FCompress <> CP_NOCOMP)
              then FError := EC_OK;
         end;
       except
         On EReadError
         do FError := EC_READFROMFILE;
         On Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPxMImage.IsFormatValid.


function TmcmPxMImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
var AsciiData   : Ansistring;
    pAsciiData  : PAnsiChar;
    i, j, k     : integer;
    OldPos      : longint;
    EndOfHeader : boolean;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;
       try
         if (IsFormatValid(Stream) = EC_OK) // Read header and check format.
         then begin
              if (FDibInfo <> nil)
              then begin
                   // Read header information.
                   case FCompress of
                   CP_BIN_PBM,
                   CP_BIN_PGM,
                   CP_BIN_PPM,
                   CP_ASCII_PBM,
                   CP_ASCII_PGM,
                   CP_ASCII_PPM : begin
                                    with FDibInfo.bmiHeader
                                    do begin
                                       case FCompress of
                                       CP_BIN_PBM,
                                       CP_ASCII_PBM : biBitCount := 1;
                                       CP_BIN_PGM,
                                       CP_ASCII_PGM : biBitCount := 8;
                                       CP_BIN_PPM,
                                       CP_ASCII_PPM : biBitCount := 24;
                                       end;
                                       biPlanes        := 1;
                                       biCompression   := BI_RGB;
                                       biXPelsPerMeter := 0;
                                       biYPelsPerMeter := 0;
                                       biClrUsed       := 0;
                                       biClrImportant  := 0;
                                    end;

                                    FStream.Position := 3 + FStreamStart;
                                    SetLength(AsciiData, 100);

                                    repeat
                                      OldPos := FStream.Position;
                                      FStream.Read(AsciiData[1], 99);
                                      AsciiData[100] := #0;
                                      if (AsciiData[1] = '#')
                                      then begin
                                           // Remove comments.
                                           j := Pos(AnsiChar(#10), AsciiData);
                                           FStream.Position := OldPos + j;
                                           EndOfHeader := False;
                                           if Assigned(FImageInfo)
                                           then FImageInfo.Description := Copy(String(AsciiData), 2, j - 2);
                                      end
                                      else begin
                                           EndOfHeader := True;
                                           with FDibInfo.bmiHeader
                                           do begin
                                              j := Pos(AnsiChar(#10), AsciiData);

                                              // Get Width
                                              pAsciiData := @AsciiData[1];
                                              i := Pos(AnsiChar(' '), pAsciiData) - 1;
                                              pAsciiData[i] := #0;
                                              biWidth := StrToInt(String(pAsciiData));

                                              // Get Height
                                              pAsciiData := pAsciiData + i + 1;
                                              i := Pos(AnsiChar(' '), pAsciiData) - 1;
                                              k := Pos(AnsiChar(#10), pAsciiData) - 1;
                                              if (i < 0) or (k < i)
                                              then i := k;
                                              pAsciiData[i] := #0;
                                              biHeight := StrToInt(String(pAsciiData));

                                              if (FCompress <> CP_BIN_PBM) and
                                                 (FCompress <> CP_ASCII_PBM)
                                              then begin
                                                   // Get max no of colors per channel.
                                                   pAsciiData := pAsciiData + i + 1;
                                                   i := Pos(AnsiChar(#10), pAsciiData) - 1;
                                                   pAsciiData[i] := #0;
                                                   // biClrImportant  := StrToInt(pAsciiData) + 1;
                                              end
                                              else i := -1;
                                              if (FCompress = CP_BIN_PPM) or
                                                 (FCompress = CP_BIN_PGM)
                                              then OldPos := OldPos + j + i + 1
                                              else OldPos := OldPos + j;
                                              FStream.Position := OldPos;
                                           end;
                                      end;
                                    until EndOfHeader
                                  end;
                   else FError := EC_DECOMPRESSION;
                   end;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPxMImage.ReadHeader.


function TmcmPxMImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var PortableID  : array[0..2] of AnsiChar;
    FormatStr   : AnsiString;
    Comment     : AnsiString;
    NoColor     : word;
    Photometric : TmcmPhotometric;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;

       with FDibInfo^.bmiHeader
       do begin
          FormatStr := AnsiString(IntToStr(biWidth)) + ' ' + AnsiString(IntToStr(biHeight)) + #10;
          NoColor := 0;
          Photometric := GetPhotometric;
          case Photometric of
          PM_INVGREY,
          PM_GREY : begin
                      case FBitCount of
                      1 : begin
                            NoColor := 1;
                            if (FCompress = CP_NOCOMP)
                            then FCompress := CP_BIN_PBM;
                          end;
                      8 : begin
                            NoColor := 255;
                            if (FCompress = CP_NOCOMP)
                            then FCompress := CP_BIN_PGM;
                          end;
                      else FError := EC_COMPRESSCOLOR;
                      end;
                    end;
          PM_RGB : begin
                     NoColor := 255;
                     if (FCompress = CP_NOCOMP)
                     then FCompress := CP_BIN_PPM;
                   end;
          else FError := EC_COMPRESSCOLOR;
          end;
       end;

       if ((FFileFormat = FF_PBM) and ((FCompress = CP_ASCII_PBM) or (FCompress = CP_BIN_PBM))) or
          ((FFileFormat = FF_PGM) and ((FCompress = CP_ASCII_PGM) or (FCompress = CP_BIN_PGM))) or
          ((FFileFormat = FF_PPM) and ((FCompress = CP_ASCII_PPM) or (FCompress = CP_BIN_PPM)))
       then begin
            case FCompress of
            CP_ASCII_PBM : PortableID := 'P1' + #10;
            CP_ASCII_PGM : PortableID := 'P2' + #10;
            CP_ASCII_PPM : PortableID := 'P3' + #10;
            CP_BIN_PBM   : PortableID := 'P4' + #10;
            CP_BIN_PGM   : PortableID := 'P5' + #10;
            CP_BIN_PPM   : PortableID := 'P6' + #10;
            else FError := EC_COMPRESSION;
            end;

            // Write header to file.
            if (FError = EC_OK)
            then begin
                 try
                   // Write identification.
                   FStream.Write(PortableID[1], Length(PortableID));

                   // Add information from TmcmImageInfo class.
                   if Assigned(FImageInfo)
                   then begin
                        Comment := '# ' + AnsiString(FImageInfo.Description) + #10;
                        FStream.Write(Comment[1], Length(Comment));
                   end;

                   // Write format string.
                   if (FFileFormat <> FF_PBM)
                   then FormatStr := FormatStr + AnsiString(IntToStr(NoColor)) + #10;
                   FStream.Write(FormatStr[1], Length(FormatStr));
                 except
                   On E:Exception
                   do FError := EC_WRITETOFILE;
                 end;
            end;
       end
       else FError := EC_COMPRESSION;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPxMImage.WriteHeader.


procedure TmcmPxMImage.LoadFromStream(Stream : TStream);
var LineWidth  : cardinal;
    AsciiData  : AnsiString;
    pAsciiData : PAnsiChar;
    i, j       : integer;
    Count      : cardinal;
    OldPos     : longint;
    pData      : PVectorB;
    Row        : longint;
    BitPos     : word;
    TheByte    : byte;
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;

       // Read Header.
       if (ReadHeader(Stream) = EC_OK)
       then begin
            // Create & load/set Palette
            if FCompress in [CP_ASCII_PBM, CP_ASCII_PGM, CP_BIN_PBM, CP_BIN_PGM]
            then CreateGreyPalette(PM_GREY);

            // Load image data.
            if CreateDIB
            then begin
                 try
                   Row   := FDibInfo.bmiHeader.biHeight - 1;
                   pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                   SetLength(AsciiData, 100);
                   case FCompress of
                   CP_ASCII_PBM : begin
                                    Count := 0;
                                    j := 1;
                                    BitPos   := $80;
                                    TheByte  := 0;
                                    LineWidth := FDibInfo.bmiHeader.biWidth;
                                    while (Row >= 0) and (j > 0)
                                    do begin
                                       OldPos := FStream.Position;
                                       FStream.Read(AsciiData[1], 99);
                                       AsciiData[100] := #0;

                                       if (AsciiData[1] = '#')
                                       then begin
                                            // Remove comments.
                                            j := Pos(AnsiChar(#10), AsciiData);
                                            FStream.Position := OldPos + j;
                                       end
                                       else begin
                                            j := Pos(AnsiChar(#10), AsciiData); // Get end of line.
                                            pAsciiData := @AsciiData[1];
                                            pAsciiData[j-1] := #0; // Mark end of line.

                                            i := 0;
                                            while (pAsciiData[i] <> #0)
                                            do begin
                                               case pAsciiData[i] of
                                               '0' : begin
                                                       Inc(TheByte, BitPos);
                                                       BitPos := BitPos shr 1;
                                                       dec(LineWidth);
                                                     end;
                                               '1' : begin
                                                       BitPos := BitPos shr 1;
                                                       dec(LineWidth);
                                                     end;
                                               ' ' : ;
                                               end;
                                               inc(i);

                                               if (BitPos = 0) or (LineWidth = 0)
                                               then begin
                                                    pData^[Count] := TheByte;
                                                    TheByte := 0;
                                                    BitPos := $80;
                                                    inc(Count);

                                                    if (LineWidth = 0)
                                                    then begin
                                                         // Set-up address to next line in bitmap.
                                                         Count := 0;
                                                         dec(Row);
                                                         if (Row < 0)
                                                         then break;
                                                         pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                                         LineWidth := FDibInfo.bmiHeader.biWidth;
                                                    end;
                                               end;
                                            end;
                                            FStream.Position := OldPos + j;
                                       end;
                                    end;
                                  end;
                   CP_ASCII_PGM,
                   CP_ASCII_PPM : begin
                                    Count := 0;
                                    LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                    j := 1;
                                    while (Row >= 0) and (j > 0)
                                    do begin
                                       OldPos := FStream.Position;
                                       FStream.Read(AsciiData[1], 99);
                                       AsciiData[100] := #0;

                                       if (AsciiData[1] = '#')
                                       then begin
                                            // Remove comments.
                                            j := Pos(AnsiChar(#10), AsciiData);
                                            FStream.Position := OldPos + j;
                                       end
                                       else begin
                                            j := Pos(AnsiChar(#10), AsciiData); // Get end of line.
                                            pAsciiData := @AsciiData[1];
                                            pAsciiData[j-1] := #0; // Mark end of line.
                                            while (pAsciiData[0] = ' ')
                                            do pAsciiData := pAsciiData + 1;

                                            i := Pos(AnsiChar(' '), pAsciiData) - 1;
                                            while (i > 0)
                                            do begin
                                               pAsciiData[i] := #0; // Remove space.
                                               pData^[Count] := StrToInt(String(pAsciiData));

                                               // Go to next pixel data.
                                               pAsciiData := pAsciiData + i + 1;
                                               while (pAsciiData[0] = ' ')
                                               do pAsciiData := pAsciiData + 1;
                                               i := Pos(AnsiChar(' '), pAsciiData) - 1;
                                               inc(Count);

                                               if (Count = LineWidth)
                                               then begin
                                                    // Set-up address to next line in bitmap.
                                                    Count := 0;
                                                    dec(Row);
                                                    if (Row < 0)
                                                    then break;
                                                    pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                               end;
                                            end;
                                            FStream.Position := OldPos + j;
                                       end;
                                    end;
                                  end;
                   CP_BIN_PBM   : begin
                                    LineWidth := (7 + FDibInfo.bmiHeader.biWidth) div 8;
                                    while (Row >= 0)
                                    do begin
                                       FStream.Read(pData^, LineWidth);
                                       for i := 0 to (LineWidth - 1)
                                       do pData^[i] := Not(pData^[i]);
                                       dec(Row);
                                       pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                    end;
                                  end;
                   CP_BIN_PGM,
                   CP_BIN_PPM   : begin
                                    LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                    while (Row >= 0)
                                    do begin
                                       FStream.Read(pData^, LineWidth);
                                       dec(Row);
                                       pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                    end;
                                  end;
                   end;
                   // Swap Red and Blue color components.
                   if (FCompress = CP_ASCII_PPM) or
                      (FCompress = CP_BIN_PPM)
                   then SwapByteRB;
                 except
                   On Exception
                   do FError := EC_ENDOFFILE;
                 end;
            end;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN; // File not open.
end; // TmcmPxMImage.LoadFromStream.


procedure TmcmPxMImage.SaveToStream(Stream : TStream);
var LineWidth  : cardinal;
    LineData   : PVectorB;
    pData      : PVectorB;
    i, j       : cardinal;
    Row        : longint;
    AsciiData  : AnsiString;
    BitPos     : word;
    BytePos    : cardinal;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write Portable file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write bitmap data.
                 Row   := FDibInfo.bmiHeader.biHeight - 1;
                 pData := @PVectorB(FDibBits)^[Row * FLongWidth];

                 LineData := AllocMem(FLongWidth);
                 try
                 case FCompress of
                 CP_ASCII_PBM : begin
                                  LineWidth := FDibInfo.bmiHeader.biWidth;
                                  j := 0;
                                  AsciiData := '';
                                  while (Row >= 0)
                                  do begin
                                     BitPos := $80;
                                     BytePos := 0;
                                     for i := 0 to (LineWidth - 1)
                                     do begin
                                        if ((pData^[BytePos] and BitPos) = 0)
                                        then AsciiData := AsciiData + '1 '
                                        else AsciiData := AsciiData + '0 ';

                                        BitPos := BitPos shr 1;
                                        if (BitPos = 0)
                                        then begin
                                             BitPos := $80;
                                             inc(BytePos);
                                        end;

                                        inc(j, 2);
                                        if (j >= 68)
                                        then begin
                                             AsciiData := AsciiData + #10;
                                             FStream.Write(PVectorB(@AsciiData[1])^, j + 1);
                                             j := 0;
                                             AsciiData := '';
                                        end;
                                     end;

                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;

                                  j := Length(AsciiData);
                                  if (j > 0)
                                  then begin
                                       AsciiData := AsciiData + #10;
                                       FStream.Write(AsciiData[1], j + 1);
                                  end;
                                end;
                 CP_ASCII_PGM : begin
                                  LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                  AsciiData := '';
                                  while (Row >= 0)
                                  do begin
                                     for i := 0 to (LineWidth - 1)
                                     do begin
                                        AsciiData := AsciiData + AnsiString(IntToStr(pData^[i])) + ' ';
                                        j := Length(AsciiData);
                                        if (j >= 66)
                                        then begin
                                             AsciiData := AsciiData + #10;
                                             FStream.Write(PVectorB(@AsciiData[1])^, j + 1);
                                             AsciiData := '';
                                        end;
                                     end;
                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;

                                  j := Length(AsciiData);
                                  if (j > 0)
                                  then begin
                                       AsciiData := AsciiData + #10;
                                       FStream.Write(AsciiData[1], j + 1);
                                  end;
                                end;
                 CP_ASCII_PPM : begin
                                  LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                  AsciiData := '';
                                  while (Row >= 0)
                                  do begin
                                     // Swap Red and Blue color components.
                                     for i := 0 to (FDibInfo.bmiHeader.biWidth - 1)
                                     do begin
                                        j := 3 * i;
                                        LineData[j] := pData^[j+2];
                                        LineData[j+1] := pData^[j+1];
                                        LineData[j+2] := pData^[j];
                                     end;

                                     for i := 0 to (LineWidth - 1)
                                     do begin
                                        AsciiData := AsciiData + AnsiString(IntToStr(LineData^[i])) + ' ';
                                        j := Length(AsciiData);
                                        if (j >= 66)
                                        then begin
                                             AsciiData := AsciiData + #10;
                                             FStream.Write(PVectorB(@AsciiData[1])^, j + 1);
                                             AsciiData := '';
                                        end;
                                     end;
                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;

                                  j := Length(AsciiData);
                                  if (j > 0)
                                  then begin
                                       AsciiData := AsciiData + #10;
                                       FStream.Write(AsciiData[1], j + 1);
                                  end;
                                end;
                 CP_BIN_PBM   : begin
                                  LineWidth := (7 + FDibInfo.bmiHeader.biWidth) div 8;
                                  while (Row >= 0)
                                  do begin
                                     // Invert line data (so that Black = 1 and White = 0).
                                     for i := 0 to (LineWidth - 1)
                                     do LineData^[i] := Not(pData^[i]);
                                     FStream.Write(LineData^, LineWidth);
                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;
                                end;
                 CP_BIN_PGM   : begin
                                  LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                  while (Row >= 0)
                                  do begin
                                     FStream.Write(pData^, LineWidth);
                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;
                                end;
                 CP_BIN_PPM   : begin
                                  LineWidth := FDibInfo.bmiHeader.biWidth * (FBitCount div 8);
                                  while (Row >= 0)
                                  do begin
                                     // Swap Red and Blue color components.
                                     for i := 0 to (FDibInfo.bmiHeader.biWidth - 1)
                                     do begin
                                        j := 3 * i;
                                        LineData[j] := pData^[j+2];
                                        LineData[j+1] := pData^[j+1];
                                        LineData[j+2] := pData^[j];
                                     end;
                                     FStream.Write(LineData^, LineWidth);
                                     dec(Row);
                                     pData := @PVectorB(FDibBits)^[Row * FLongWidth];
                                  end;
                                end;
                 end;
                 finally
                   FreeMem(LineData);
                 end;
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmPxMImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmGIFImage
//------------------------------------------------------------------------------

class function TmcmGIFImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8];
end; // TmcmGIFImage.GetColorFormats.


class function TmcmGIFImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_BW..IF_PAL8])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_GIF87A;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_GIF89A;
       inc(Count);
  end;
  Result := Count;
end; // TmcmGIFImage.GetCompressionFromColor.


class function TmcmGIFImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_GIF87A : Result := resCP_GIF87A;
  CP_GIF89A : Result := resCP_GIF89A;
  else Result := '';
  end;
end; // TmcmGIFImage.GetCompressionName.


function TmcmGIFImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var GIFSignature : array[0..2] of AnsiChar;
    GIFVersion   : array[0..2] of AnsiChar;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStreamStart := FStream.Position;
         FStream.Read(GIFSignature, SizeOf(GIFSignature));
         if (GIFSignature = 'GIF')
         then begin
              FStream.Read(GIFVersion, SizeOf(GIFVersion));
              if (GIFVersion = '87a')
              then begin
                   FGIFVersion := 87;
                   FCompress := CP_GIF87A;
                   FError := EC_OK;
              end;
              if (GIFVersion = '89a')
              then begin
                   FGIFVersion := 89;
                   FCompress := CP_GIF89A;
                   FError := EC_OK;
              end;
         end;
       except
         On EReadError
         do FError := EC_READFROMFILE;
         On Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmGIFImage.IsFormatValid.


function TmcmGIFImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
var PalBits : integer;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              // Read header and check format.
              if (FDibInfo <> nil)
              then begin
                   // Read header information.
                   FStream.Read(FGIFHeader, SizeOf(TGIFHeader));
                   with FDibInfo.bmiHeader
                   do begin
                      PalBits         := (1 + FGIFHeader.BitFields and $07);
                      biWidth         := FGIFHeader.ScreenWidth;
                      biHeight        := FGIFHeader.ScreenHeight;
                      biBitCount      := ((FGIFHeader.BitFields shr 4) and $07) + 1;
                      case biBitCount of
                      2..4 : biBitCount := 4;
                      5..8 : biBitCount := 8;
                      end;
                      if (biBitCount < PalBits)
                      then biBitCount := PalBits;
                      case biBitCount of
                      2..4 : biBitCount := 4;
                      5..8 : biBitCount := 8;
                      end;
                      biPlanes        := 1;
                      biCompression   := BI_RGB;
                      biXPelsPerMeter := 0;
                      biYPelsPerMeter := 0;
                      biClrUsed       := 0;
                      biClrImportant  := 0;
                   end;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmGIFImage.ReadHeader.


function TmcmGIFImage.ReadPalette(Stream : TStream; BitFields : byte) : TmcmErrorCode;
var i          : integer;
    GIFPalette : array[0..255] of TRGBTriple;
begin
  try
    FNoColors := (1 shl (1 + BitFields and $07));

    if (FNoColors > 0)
    then begin
         // Read palette values.
         FStream.Read(GIFPalette, (FNoColors * SizeOf(TRGBTriple)));

         for i := 0 to (FNoColors - 1)
         do begin
            with FDibInfo^.bmiColors[i]
            do begin
               rgbBlue     := GIFPalette[i].rgbtRed;   //
               rgbGreen    := GIFPalette[i].rgbtGreen;
               rgbRed      := GIFPalette[i].rgbtBlue;
               rgbReserved := 0;
            end;
         end;
    end;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmGIFImage.ReadPalette.


function TmcmGIFImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var GIFSignature : array[0..2] of AnsiChar;
    GIFVersion   : array[0..2] of AnsiChar;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;
       if (FDibInfo.bmiHeader.biBitCount in [1,4,8]) // If Image format is supported by file format
       then begin
            // Write signature.
            GIFSignature := 'GIF';
            FStream.Write(GIFSignature, SizeOf(GIFSignature));

            // Write version identifier
            case FCompress of
            CP_GIF87A : GIFVersion := '87a';
            CP_GIF89A : GIFVersion := '89a';
            else begin
                 FCompress := CP_GIF89A;
                 GIFVersion := '89a';
                 //FError := EC_COMPRESSION;
            end;
            end;

            // Write header to file.
            if (FError = EC_OK)
            then begin
                 FStream.Write(GIFVersion, SizeOf(GIFVersion));
                 try
                   with FDibInfo.bmiHeader
                   do begin
                      FGIFHeader.ScreenWidth  := biWidth;
                      FGIFHeader.ScreenHeight := biHeight;
                      // Number of 2^bits (colors) in global palette.
                      FGIFHeader.BitFields    := (biBitCount - 1) shl 4;
                      FGIFHeader.BitFields    := FGIFHeader.BitFields or (biBitCount - 1);

                      // Has pallette.
                      FGIFHeader.BitFields    := FGIFHeader.BitFields or $80;
                      // Set background color.
                      FGIFHeader.BKColor := 0;

                      {
                      if FInterlaced
                      then FGIFImageHeader.BitField := FGIFImageHeader.BitField or $40;
                      }
                      FGIFHeader.AspectRatio := 0;
                   end;

                   // Write header information.
                   FStream.Write(FGIFHeader, SizeOf(TGIFHeader));
                 except
                   On Exception
                   do FError := EC_WRITETOFILE;
                 end;
            end;
       end
       else FError := EC_BITDEPTH;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmGIFImage.WriteHeader.


function TmcmGIFImage.WritePalette(Stream : TStream) : TmcmErrorCode;
var i          : integer;
    GIFPalette : array[0..255] of TRGBTriple;
begin
  try
    FNoColors := (1 shl (1 + FGIFHeader.BitFields and $07));
    if (FNoColors > 0)
    then begin
         for i := 0 to (FNoColors - 1)
         do begin
            with FDibInfo^.bmiColors[i]
            do begin
               GIFPalette[i].rgbtRed   := rgbBlue;
               GIFPalette[i].rgbtGreen := rgbGreen;
               GIFPalette[i].rgbtBlue  := rgbRed;
            end;
         end;

         // Write palette values.
         FStream.Write(GIFPalette, (FNoColors * SizeOf(TRGBTriple)));
    end;
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmGIFImage.WritePalette.


procedure TmcmGIFImage.LoadFromStream(Stream : TStream);
var BlockCode     : byte;
    ExtensionCode : byte;
    ExtensionSize : byte;
    InitCodeSize  : byte;
    Count         : byte;
    BufferSize    : longword;
    DataCount     : longword;
    ReadImage     : boolean;
    MemIndex      : integer;
begin
  FDibHandle := 0;
  FStream.Source := Stream;

  // Read Header.
  if (ReadHeader(Stream) = EC_OK)
  then begin
       // Create & load/set global palette
       if ((FGIFHeader.BitFields and $80) <> 0)
       then ReadPalette(Stream, FGIFHeader.BitFields);

       // Load image data.
       if CreateDIB
       then begin
            ReadImage := True;

            // Fill background with described background color.
            if ((FGIFHeader.BitFields and $80) <> 0)
            then FillMemory(FDibBits, FDibInfo^.bmiHeader.biSizeImage, FGIFHeader.BKColor);

            FCompObj := TmcmImageLZW.Create;
            TmcmImageLZW(FCompObj).CodeMode := LZWCM_GIF;
            TmcmImageLZW(FCompObj).BitEndian := False;

            try
              FCompObj.Image := FDibHandle;

              FStream.Read(BlockCode, SizeOf(byte));
              while (BlockCode <> GIF_TERMINATOR) and (FError = EC_OK) and ReadImage
              do begin
                 case BlockCode of
                 // Image blocks
                 GIF_IMAGEBLOCK : begin
                                    // Load Image header.
                                    FStream.Read(FGIFImageHeader, SizeOf(TGIFImageHeader));
                                    FInterlaced := (FGIFImageHeader.BitField and $40) <> 0;

                                    TmcmImageLZW(FCompObj).Interlaced := FInterlaced;
                                    TmcmImageLZW(FCompObj).SetDataSize(FDibInfo.bmiHeader.biBitCount);

                                    // FGIFImageHeader.LeftPos
                                    // FGIFImageHeader.TopPos
                                    // FGIFImageHeader.ImageWidth
                                    // FGIFImageHeader.ImageHeight

                                    // Load local palette if available.
                                    if ((FGIFImageHeader.BitField and $80) <> 0)
                                    then begin
                                         // This will destroy a global palette!!!!!
                                         ReadPalette(Stream, FGIFImageHeader.BitField);

                                         // Assign palette to DIBSection handle.
                                         SetbmiPalToDibHandle;
                                    end;

                                    // Read minimum code size (1 byte).
                                    FStream.Read(InitCodeSize, SizeOf(InitCodeSize));
                                    TmcmImageLZW(FCompObj).CodeSize := InitCodeSize;

                                    // Read first sub-block/image data size.
                                    MemIndex := 0;
                                    FStream.Read(Count, SizeOf(Byte));
                                    BufferSize := Count;
                                    while (BufferSize <> 0) and (FError = EC_OK)
                                    do begin
                                      //FMemStream.SetSize(BufferSize);
                                      FMemStream.SetSize(longword(FMemStream.Size) + BufferSize);

                                      // Read data block size.

                                      // Read image data
                                      //FStream.Read(FMemStream.Memory^, FMemStream.Size);
                                      FStream.Read(PVectorB(FMemStream.Memory)^[MemIndex], Count); //FMemStream.Size);
                                      MemIndex := FmemStream.Size;

                                      // Decompress data.
                                      //FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);                                

                                      // Read next sub-block/image data size.
                                      // If block size is zero we reached the
                                      // end of this image.
                                      FStream.Read(Count, SizeOf(Byte));
                                      BufferSize := Count;
                                    end;
                                    
                                    BufferSize := FMemStream.Size;
                                    // Decompress data.
                                    FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                                    
                                    ReadImage := False;
                                  end;
                 // Extension blocks
                 GIF_EXTENSION : begin
                                   // Read extension code
                                   FStream.Read(ExtensionCode, SizeOf(ExtensionCode));

                                   // Read extension size.
                                   FStream.Read(ExtensionSize, SizeOf(ExtensionSize));
                                   FMemStream.Size := ExtensionSize;

                                   while (ExtensionSize > 0)
                                   do begin
                                      // Read extension data
                                      FStream.Read(FMemStream.Memory^, FMemStream.Size);

                                      // Case extension code of
                                      case ExtensionCode of
                                      // Plain text extension
                                      GIF_EXT_TEXT     : begin
                                                         end;
                                      // Graphics control extension
                                      GIF_EXT_GRAPHICS : begin
                                                         end;
                                      // Comment extension
                                      GIF_EXT_COMMENT  : begin
                                                         end;
                                      // Application extension
                                      GIF_EXT_APP      : begin
                                                         end;
                                      else ; // ? FError := EC_UNKNOWNFORMAT; ?
                                      end;

                                      // Read extension size.
                                      FStream.Read(ExtensionSize, SizeOf(ExtensionSize));
                                      FMemStream.Size := ExtensionSize;
                                   end;

                                  end;
                 else FError := EC_UNKNOWNFORMAT;
                 end;

                 // Read next block code.
                 FStream.Read(BlockCode, SizeOf(byte));
              end;

            except
              On Exception
              do FError := EC_ENDOFFILE;
            end;
            if Assigned(FCompObj)
            then begin
                 FCompObj.Free;
                 FCompObj := Nil;
            end;
       end;
  end;
end; // TmcmGIFImage.LoadFromStream.


procedure TmcmGIFImage.SaveToStream(Stream : TStream);
var BlockCode     : byte;
    //ExtensionCode : byte;
    //ExtensionSize : byte;
    InitCodeSize  : byte;
    Count         : longword;
    wCount        : byte;
    Buffer        : Pointer;
    wBuf          : Pointer;
    BufferPos     : longword;
    BufferSize    : longword;
    DataCount     : longword;
    NoImages      : integer;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            if (FDibInfo.bmiHeader.biBitCount in [1,4,8])
            then begin
                 // Write file header - Logical Screen Descriptor.
                 if (WriteHeader(Stream) = EC_OK)
                 then begin
                      // Write Palette - Global Color Table.
                      if ((FGIFHeader.BitFields and $80) <> 0)
                      then WritePalette(Stream);

                      if (FError = EC_OK)
                      then begin
                           // Save each sub-image and related information.
                           NoImages := 1;
                           while (NoImages > 0) and (FError = EC_OK)
                           do begin
                              dec(NoImages);

                              // Extension block.
                              if (FCompress = CP_GIF89A)
                              then begin
                                   // Check if extension blocks should be writen.

                                   // BlockCode := GIF_EXTENSION;
                                   // FStream.Write(BlockCode, SizeOf(byte));
                              end;

                              // Image block
                              BlockCode := GIF_IMAGEBLOCK;
                              FStream.Write(BlockCode, SizeOf(byte));

                              // Write local image header
                              FGIFImageHeader.LeftPos     := 0;
                              FGIFImageHeader.TopPos      := 0;
                              FGIFImageHeader.ImageWidth  := FDibInfo.bmiHeader.biWidth;
                              FGIFImageHeader.ImageHeight := FDibInfo.bmiHeader.biHeight;

                              // Number of color entrie in local color table.
                              FGIFImageHeader.BitField := (FDibInfo.bmiHeader.biBitCount - 1) and $07;

                              // Are color table entrie sorted by importance? NO
                              // if ???
                              // then FGIFImageHeader.BitField := FGIFImageHeader.BitField or $20;

                              // Is image to be stored interlaced?
                              if FInterlaced
                              then FGIFImageHeader.BitField := FGIFImageHeader.BitField or $40;

                              // Use local color palette? NO
                              // if ???
                              // then FGIFImageHeader.BitField := FGIFImageHeader.BitField or $80;

                              FStream.Write(FGIFImageHeader, SizeOf(TGIFImageHeader));

                              // If we have a local color table write this here.
                              // Not used.

                              // Write initial code size.
                              InitCodeSize := FDibInfo.bmiHeader.biBitCount;
                              if (InitCodeSize = 1)
                              then inc(InitCodeSize);
                              FStream.Write(InitCodeSize, SizeOf(InitCodeSize));

                              // Write LZW compressed bitmap data.
                              FCompObj := TmcmImageLZW.Create;

                              try
                                TmcmImageLZW(FCompObj).CodeMode := LZWCM_GIF;
                                FCompObj.Image := FDibHandle;
                                TmcmImageLZW(FCompObj).BitEndian := False;
                                TmcmImageLZW(FCompObj).DiffPredictor := False;
                                TmcmImageLZW(FCompObj).CodeSize := InitCodeSize;
                                TmcmImageLZW(FCompObj).Interlaced := FInterlaced;
                                TmcmImageLZW(FCompObj).SetDataSize(FDibInfo.bmiHeader.biBitCount);

                                if (FError = EC_OK)
                                then begin
                                     Count := FDibInfo^.bmiHeader.biSizeImage;
                                     while (FError = EC_OK) and (DWORD(Count) > 0)
                                     do begin
                                        try
                                          if (FDibInfo^.bmiHeader.biSizeImage <= 65536)
                                          then DataCount := FDibInfo^.bmiHeader.biSizeImage
                                          else begin
                                               DataCount := (longint(FDibInfo^.bmiHeader.biSizeImage div 4) div FLongWidth) * FLongWidth;
                                               if (DataCount < 16384)
                                               then DataCount := (16384 div FLongWidth) * FLongWidth;
                                               if (DataCount > 262144)
                                               then DataCount := (262144 div FLongWidth) * FLongWidth;
                                          end;
                                          if (DataCount > Count)
                                          then DataCount := Count;
                                          FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                                          Count := Count - DataCount;
                                        except
                                          FError := EC_UNKNOWN;
                                        end;

                                        // Write compressed data in 254 or less chuncks.
                                        if (FError = EC_OK)
                                        then begin
                                             BufferPos := 0;
                                             while (BufferSize > 0)
                                             do begin
                                                wCount := 254;
                                                if (wCount > BufferSize)
                                                then wCount := BufferSize;
                                                FStream.Write(wCount, SizeOf(Byte));
                                                wBuf := @PAnsiChar(Buffer)[BufferPos];
                                                FStream.Write(wBuf^, wCount);
                                                inc(BufferPos, wCount);
                                                dec(BufferSize, wCount);
                                             end;
                                        end;
                                     end;
                                end;

                                // Write ZERO value to indicate end of image data.
                                BufferSize := 0;
                                FStream.Write(BufferSize, SizeOf(Byte));
                              except
                                FError := EC_UNKNOWN;
                              end;

                              if Assigned(FCompObj)
                              then begin
                                   FCompObj.Free;
                                   FCompObj := Nil;
                              end;
                           end;

                           // Write Terminator block code.
                           BlockCode := GIF_TERMINATOR;
                           FStream.Write(BlockCode, SizeOf(byte));
                      end;
                 end;
                 FStream.Flush;
            end
            else FError := EC_BITDEPTH;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmGIFImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmSGIImage
//------------------------------------------------------------------------------

class function TmcmSGIImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_GREY8,IF_RGB24,IF_RGBA32];
end; // TmcmSGIImage.GetColorFormats.


class function TmcmSGIImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       Count := Inherited GetCompressionFromColor(ImageFormat, Compress);
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE_SGI;
       inc(Count);
  end;
  Result := Count;
end; // TmcmSGIImage.GetCompressionFromColor.


class function TmcmSGIImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP  : Result := resCP_NOCOMP;
  CP_RLE_SGI : Result := resCP_RLE8_32;
  else Result := '';
  end;
end; // TmcmSGIImage.GetCompressionName.


constructor TmcmSGIImage.Create;
begin
  Inherited Create;
  FStream.BigEndian := True;
end; // TmcmSGIImage.Create


destructor TmcmSGIImage.Destroy;
begin
  Inherited Destroy
end; // TmcmSGIImage.Destroy.


function TmcmSGIImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var Signature : word;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;
         Signature := FStream.ReadWord;

         if (Signature = SGI_SIGNATURE)
         then FError := EC_OK;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmSGIImage.IsFormatValid.


function TmcmSGIImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              // Read header and check format.
              FStream.Read(FSGIHeader, SizeOf(TSGIHeader));

              if (FDibInfo <> nil)
              then begin
                   // Set header information.
                   FSGIHeader.Width  := SwapWord(FSGIHeader.Width);
                   FSGIHeader.Height := SwapWord(FSGIHeader.Height);
                   with FDibInfo^
                   do begin
                      bmiHeader.biWidth    := FSGIHeader.Width;
                      bmiHeader.biHeight   := FSGIHeader.Height;

                      if (FSGIHeader.BytePerChannel > 1)
                      then FError := EC_BITDEPTH;

                      if (SwapWord(FSGIHeader.Dimension) < 2)
                      then FError := EC_UNKNOWNFORMAT;

                      if (SwapWord(FSGIHeader.ColorMap) <> 0)
                      then FError := EC_UNKNOWNFORMAT;

                      FSGIHeader.Planes := SwapWord(FSGIHeader.Planes);
                      case FSGIHeader.Planes of
                      1 : bmiHeader.biBitCount := 8;  // Grey scale image
                      3 : bmiHeader.biBitCount := 24; // RGB image
                      4 : bmiHeader.biBitCount := 32; // RGBA image
                      end;

                      bmiHeader.biPlanes   := 1;
                      bmiHeader.biXPelsPerMeter := 0;
                      bmiHeader.biYPelsPerMeter := 0;
                      if (SwapWord(FSGIHeader.Compress) <> 0)
                      then FCompress := CP_RLE_SGI;
                   end;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmSGIImage.ReadHeader.


function TmcmSGIImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
begin
  try // SGI Image files doesn't have palette !?!
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmSGIImage.ReadPalette.


function TmcmSGIImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var Signature : word;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;

       // If Image format is supported by file format?
       if (GetPhotometric in [PM_INVGREY, PM_GREY, PM_RGB, PM_RGBA])
       then begin
            FillChar(FSGIHeader, SizeOf(TSGIHeader), #0);
            with FDibInfo^.bmiHeader
            do begin
               // Fill SGI file header.
               FSGIHeader.BytePerChannel := 1;
               FSGIHeader.Dimension      := SwapWord(3);
               FSGIHeader.Width          := SwapWord(biWidth);
               FSGIHeader.Height         := SwapWord(biHeight);
               case biBitCount of
               8  : FSGIHeader.Planes    := SwapWord(1);
               24 : FSGIHeader.Planes    := SwapWord(3);
               32 : FSGIHeader.Planes    := SwapWord(4);
               else FError := EC_BADCOLORFORMAT;
               end;
               FSGIHeader.Minimum        := 0;
               FSGIHeader.Maximum        := 255;
               FSGIHeader.ColorMap       := 0;
            end;
            if (FCompress = CP_RLE_SGI)
            then FSGIHeader.Compress := 1;

            // Write header to file.
            if (FError = EC_OK)
            then begin
                 try
                   Signature := SwapWord(SGI_SIGNATURE);
                   FStream.Write(Signature, SizeOf(word));
                   FStream.Write(FSGIHeader, SizeOf(TSGIHeader));
                 except
                   On Exception
                   do FError := EC_WRITETOFILE;
                 end;
            end;
       end
       else FError := EC_BADCOLORFORMAT;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmSGIImage.WriteHeader.


function TmcmSGIImage.WritePalette(Stream : TStream) : TmcmErrorCode;
begin
  FError := EC_OK;
  try  // SGI Image files doesn't have palette !?!
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmSGIImage.WritePalette.


procedure TmcmSGIImage.LoadFromStream(Stream : TStream);
const RGBMix   : array[0..3] of word = (2, 1, 0, 3);
var i, j, x    : longword;
    Buffer     : PVectorB;
    BufferPos  : cardinal;
    Entries    : longword;  // Number of entries in Offset and Length tables.
    DataOffset : PVectorLW; // Row offset table into file.
    DataLength : PVectorLW; // Compressed row length table.
    DataCount  : longword;
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;

       // Read Header.
       if (ReadHeader(Stream) = EC_OK)
       then begin
            // Create & load/set Palette
            if False // SGI Image files doesn't have palette !?!
            then ReadPalette(Stream);
            if (FDibInfo^.bmiHeader.biBitCount = 8)
            then CreateGreyPalette(PM_GREY);

            // Load image data.
            if CreateDIB
            then begin
                 try
                   case FCompress of
                   CP_RLE_SGI : begin // Read compressed data.
                                  FCompObj := TmcmImageSGIRLE.Create;
                                  try
                                    FCompObj.Image := FDibHandle;

                                    Entries := FSGIHeader.Height * FSGIHeader.Planes;
                                    GetMem(DataOffset, Entries * SizeOf(longword));
                                    GetMem(DataLength, Entries * SizeOf(longword));

                                    FStream.Read(DataOffset^, Entries * SizeOf(longword));
                                    FStream.Read(DataLength^, Entries * SizeOf(longword));

                                    i := 0;
                                    while (i < Entries) and (FError = EC_OK)
                                    do begin
                                       FStream.Position := SwapLong(DataOffset^[i]);
                                       DataLength^[i] := SwapLong(DataLength^[i]);
                                       FMemStream.SetSize(DataLength^[i]);
                                       FStream.Read(FMemStream.Memory^, FMemStream.Size);

                                       // Decompress line.
                                       FError := FCompObj.Decompress(FMemStream.Memory, DataLength^[i], DataCount);
                                       inc(i);
                                    end;

                                    FreeMem(DataOffset);
                                    FreeMem(DataLength);
                                  except
                                    FError := EC_UNKNOWN;
                                  end;

                                  if Assigned(FCompObj)
                                  then begin
                                       FCompObj.Free;
                                       FCompObj := Nil;
                                  end;
                                end;
                   else begin
                        // Read uncompressed data
                        FMemStream.Clear;
                        FMemStream.SetSize(FSGIHeader.Width);

                        Buffer := FDibBits;
                        case FDibInfo^.bmiHeader.biBitCount of
                         8 : begin // 8 bit image
                               for i := 0 to (FSGIHeader.Height - 1)
                               do begin
                                  BufferPos := i * longword(FLongWidth);
                                  FStream.Read(Buffer^[BufferPos], FSGIHeader.Width);
                               end;
                             end;
                        else // 24 or 32 bit image
                             // Read each plane
                             for j := 0 to (FSGIHeader.Planes - 1)
                             do begin
                                // Read each line
                                for i := 0 to (FSGIHeader.Height - 1)
                                do begin
                                  FStream.Read(FMemStream.Memory^, FMemStream.Size);
                                  BufferPos := i * longword(FLongWidth) + RGBMix[j];
                                  for x := 0 to (FSGIHeader.Width - 1)
                                  do begin
                                     Buffer^[BufferPos] := PVectorB(FMemStream.Memory)^[x];
                                     inc(BufferPos, FSGIHeader.Planes);
                                  end;
                                end;
                             end;
                        end;
                        FMemStream.Clear; // Release memory.
                   end;
                   end;
                 except
                   On Exception
                   do FError := EC_ENDOFFILE;
                 end;
            end;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN; // File not open.
end; // TmcmSGIImage.LoadFromStream.


procedure TmcmSGIImage.SaveToStream(Stream : TStream);
const RGBMix   : array[0..3] of word = (2, 1, 0, 3);
var i, j, x    : longint;
    SavePos    : longint;
    Entries    : longint;  // Number of entries in Offset and Length tables.
    DataOffset : PVectorLW; // Row offset table into file.
    DataLength : PVectorLW; // Compressed row length table.
//    DataMatch  : PVectorLW; // Match table.
//    Match      : longword;
    Buffer     : PVectorB;
    CBuffer    : Pointer;
    BufferPos  : cardinal;
    BufferSize : longword;
    DataCount  : longword;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       FStreamStart := FStream.Position;
       if GetDibData
       then begin
            // Write file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Palette.
                 // if (FNoColors > 0)
                 // then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                      // Write bitmap data
                      try
                        case FCompress of
                        CP_RLE_SGI : begin // Write compressed data.
                                       FCompObj := TmcmImageSGIRLE.Create;
                                       try
                                         FCompObj.Image := FDibHandle;

                                         Entries := SwapWord(FSGIHeader.Height) *
                                                    SwapWord(FSGIHeader.Planes);
                                         GetMem(DataOffset, Entries * SizeOf(longword));
                                         GetMem(DataLength, Entries * SizeOf(longword));

                                         FStream.Write(DataOffset^, Entries * SizeOf(longword));
                                         FStream.Write(DataLength^, Entries * SizeOf(longword));

                                         // Compress one line / plane at a time.
                                         i := 0;
                                         BufferSize := 0;
                                         while (i < Entries) and (FError = EC_OK)
                                         do begin
                                            // Compress a data line.
                                            DataCount := FDibInfo^.bmiHeader.biWidth;
                                            FError := FCompObj.Compress(CBuffer, BufferSize, DataCount);
                                            if (FError = EC_OK)
                                            then begin
                                                 // Calculate match.

                                                 // Compare with previous data lines.

                                                 // If a match with a previous line
                                                 // exists, point to this instead.

                                                 // Otherwise write the compressed line
                                                 // to disk.
                                                 DataOffset^[i] := SwapLong(FStream.Position);
                                                 DataLength^[i] := SwapLong(BufferSize);
                                                 FStream.Write(CBuffer^, BufferSize);
                                            end;
                                            inc(i);
                                         end;

                                         // Now, write the correct Offset and
                                         // Length tables.
                                         SavePos := FStream.Position;
                                         FStream.Position := 512 + FStreamStart;
                                         FStream.Write(DataOffset^, Entries * SizeOf(longword));
                                         FStream.Write(DataLength^, Entries * SizeOf(longword));
                                         FStream.Position := SavePos;

                                         FreeMem(DataOffset);
                                         FreeMem(DataLength);
                                       except
                                         FError := EC_WRITETOFILE;
                                       end;
                                     end;
                        else begin
                             // Write uncompressed data
                             FMemStream.Clear;
                             FMemStream.SetSize(FDibInfo^.bmiHeader.biWidth);
                             FSGIHeader.Planes := SwapWord(FSGIHeader.Planes);

                             Buffer := FDibBits;
                             case FDibInfo^.bmiHeader.biBitCount of
                             8 : for i := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                                 do begin // 8 bit image
                                    BufferPos := i * FLongWidth;
                                    FStream.Write(Buffer^[BufferPos], FDibInfo^.bmiHeader.biWidth);
                                 end;
                             else // 24 or 32 bit image
                                  // Write each plane
                                  for j := 0 to (FSGIHeader.Planes - 1)
                                  do begin
                                     // Read each line
                                     for i := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                                     do begin
                                       BufferPos := i * FLongWidth + RGBMix[j];
                                       for x := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                                       do begin
                                          PVectorB(FMemStream.Memory)^[x] := Buffer^[BufferPos];
                                          inc(BufferPos, FSGIHeader.Planes);
                                       end;
                                       FStream.Write(FMemStream.Memory^, FMemStream.Size);
                                     end;
                                  end;
                             end; // case ...biBitCount
                             FMemStream.Clear; // Release memory.
                        end;
                        end; // case FCompress.
                      except
                        FError := EC_WRITETOFILE;
                      end;

                      if Assigned(FCompObj)
                      then begin
                           FCompObj.Free;
                           FCompObj := Nil;
                      end;
                 end;
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmSGIImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmICONImage
//------------------------------------------------------------------------------

class function TmcmICONImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [];
end; // TmcmICONImage.GetColorFormats.


constructor TmcmICONImage.Create;
begin
  Inherited Create;
  FIconDirectory := TList.Create;
end; // TmcmICONImage.Create.


destructor TmcmICONImage.Destroy;
var i : integer;
begin
  for i := 0 to (FIconDirectory.Count - 1)
  do FreeMem(PIconDirEntry(FIconDirectory.Items[i]));
  FIconDirectory.Free;
  Inherited Destroy;
end; // TmcmICONImage.Destroy.


function TmcmICONImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;
         FStream.Read(FIconDir, SizeOf(TIconDir));
         if (FIconDir.idReserved = 0) and
            (FIconDir.idType = 1) and
            (FIconDir.idCount >= 1)
         then FError := EC_OK;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmICONImage.IsFormatValid.


function TmcmICONImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
var i          : integer;
    pIconEntry : PIconDirEntry;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK) // Read header and check format.
         then begin
              FIconDir.idCount := 1; // Limit to 1 image now!!!!!!!

              for i := 0 to (FIconDir.idCount - 1)
              do begin
                 GetMem(pIconEntry, SizeOf(TIconDirEntry));
                 FStream.Read(pIconEntry^, SizeOf(TIconDirEntry));
                 FIconDirectory.Add(pIconEntry);
              end;

              // Read first Icon DIB header.
              FStream.Position := PIconDirEntry(FIconDirectory.Items[0])^.dwImageOffset;
              FStream.Read(FDibInfo^.bmiHeader, SizeOf(TBitmapInfoHeader));
              FDibInfo^.bmiHeader.biHeight := FDibInfo^.bmiHeader.biHeight div 2;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmICONImage.ReadHeader.


function TmcmICONImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
begin
  try
    if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
    then FNoColors := GetNumColors(FDibInfo)
    else FNoColors := 0;

    if (FNoColors > 0)
    then begin
         // Read palette values.
         FStream.Read(FDibInfo^.bmiColors, (FNoColors * SizeOf(TRGBQUAD)));
    end;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmICONImage.ReadPalette.


function TmcmICONImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
begin
  Result := FError;
end; // TmcmICONImage.WriteHeader.


function TmcmICONImage.WritePalette(Stream : TStream) : TmcmErrorCode;
begin
  Result := FError;
end; // TmcmICONImage.WritePalette.


procedure TmcmICONImage.LoadFromStream(Stream : TStream);
begin
  FDibHandle := 0;
  FStream.Source := Stream;
  // Read Header.
  if (ReadHeader(Stream) = EC_OK)
  then begin
       // Create & load/set Palette
       FError := ReadPalette(Stream);

       // Load image data.
       if (FError = EC_OK)
       then begin
            if CreateDIB
            then begin
                 ReadStreamData(Stream, FDibBits, FDibInfo^.bmiHeader.biSizeImage);
            end;
       end;
  end;
end; // TmcmICONImage.LoadFromStream.


procedure TmcmICONImage.SaveToStream(Stream : TStream);
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write DICOM file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 // Write Colour map (palette).
//                 if ()
//                 then WritePalette(Stream);

                 if (FError = EC_OK)
                 then begin
                 end;
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmICONImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmJPEGImage.
//------------------------------------------------------------------------------

{$IFDEF mcmJPEGEnginee}
constructor TmcmJPEGComponent.Create(ID : byte; Stream : TmcmBufferStream);
begin
  Inherited Create;
  FID          := ID;
  FDCT         := TmcmDCT.Create;
  FDC          := Nil;
  FAC          := Nil;
  FCoef        := Nil;
  FMcuData     := Nil;
  FEobRun      := 0;
  FEobRow      := 0;
  FEobCol      := 0;
  FEobPos      := 0;
  FHorizFreq   := 1;
  FVertFreq    := 1;
  FHorizSample := 0;
  FVertSample  := 0;
  FNilRows     := 0;
  FNilCols     := 0;
  FDCValue     := 0;
  FStream      := Stream;
  FEncoder     := Nil;
  FACFunction  := Nil;
  FDCFunction  := Nil;
end; // TmcmJPEGComponent.Create.


destructor TmcmJPEGComponent.Destroy;
begin
  // Both AC, DC and Quant Table is owned by the parent object, therefore do
  // not destroy here.
  FDC    := Nil;
  FAC    := Nil;

  // Free allocated memory and components.
  if (FCoef <> Nil)
  then FreeMem(FCoef);
  FCoef := Nil;
  if (FMcuData <> Nil)
  then FreeMem(FMcuData);
  FMcuData := Nil;
  if Assigned(FDCT)
  then FDCT.Free;
  FDCT := Nil;
  Inherited Destroy;
end; // TmcmJPEGComponent.Destroy.


procedure TmcmJPEGComponent.SetWriteMode(DoWrite      : boolean;
                                         EncodeMethod : TJPEGEncodeMethod);
begin
  if DoWrite
  then begin
       FACFunction := WriteACData;
       FDCFunction := WriteDCData;
  end
  else begin
       FACFunction := StatACData;
       FDCFunction := StatDCData;
  end;
  case EncodeMethod of
  JPG_SEQ       : FEncoder := EncodeSequential;
  JPG_DC_FIRST  : FEncoder := EncodeDCFirstScan;
  JPG_DC_REFINE : FEncoder := EncodeDCRefineScan;
  JPG_AC_FIRST  : FEncoder := EncodeACFirstScan;
  JPG_AC_REFINE : FEncoder := EncodeACRefineScan;
  end;
end; // TmcmJPEGComponent.SetWriteMode.


procedure TmcmJPEGComponent.AllocateBuffer(FullImage, Progressive : boolean);
var MemSize : longint;
begin
  if (FMcuData = Nil)
  then begin
       // Determine amount of stretching for the component.
       FHorizSample := FMaxHorizFreq div FHorizFreq;
       FVertSample  := FMaxVertFreq div FVertFreq;

       // Calculate components non-interleaved dimension.
       FNilCols     := (FImageWidth + FHorizSample * JPEGSampleWidth - 1) div (FHorizSample * JPEGSampleWidth);
       FNilRows     := (FImageHeight + FVertSample * JPEGSampleWidth - 1) div (FVertSample * JPEGSampleWidth);

       // Calculate number of data rows and columns.
       FDataCols    := FMcuCols * FHorizFreq;
       FDataRows    := FMcuRows * FVertFreq;

       // Allocate MCU data storage.
       FMcuPixHoriz  := FMaxHorizFreq * JPEGSampleWidth;
       FMcuPixVert   := FMaxVertFreq * JPEGSampleWidth;

       FDataLineSize  := FMcuCols * FMaxHorizFreq * JPEGSampleWidth;
       FDataRowSize   := FDataLineSize * JPEGSampleWidth;
       FDataBlockSize := FMaxVertFreq * FDataRowSize;

       if Progressive
       then MemSize := FDataCols * FDataRows * SizeOf(TJPEGCoefficient)
       else MemSize := SizeOf(TJPEGCoefficient);
       GetMem(FCoef, MemSize);
       FillChar(FCoef^, MemSize, 0);

       if FullImage or Progressive
       then FAllocSize := (FDataRows + 1) * FVertSample * FDataRowSize // FNilRows * FMaxVertFreq * FDataRowSize
       else FAllocSize := FDataBlockSize;
       GetMem(FMcuData, FAllocSize * SizeOf(Byte));
  end;
end; // TmcmJPEGComponent.AllocateBuffer.


function TmcmJPEGComponent.GetACDCTable(Index : byte) : TmcmHuffmanJPEG;
begin
  case Index of
  // DC Table
  0  : Result := FDC;
  // AC Table
  else Result := FAC;
  end;
end; // TmcmJPEGComponent.GetACDCTable.


procedure TmcmJPEGComponent.SetACDCTable(Index : byte; Value : TmcmHuffmanJPEG);
begin
  case Index of
  // DC Table
  0  : begin
         FDC := Value;
       end;
  // AC Table
  else begin
         FAC := Value;
       end;
  end;
end; // TmcmJPEGComponent.SetACDCTable.


procedure TmcmJPEGComponent.SetQuant2DCT(Table : TJFIFDQT);
begin
  FDCT.SetDCTTable(Table);
end; // TmcmJPEGComponent.SetQuant2DCT.


procedure TmcmJPEGComponent.SetQuant2IDCT(Table : TJFIFDQT);
begin
  FDCT.SetIDCTTable(Table);
end; // TmcmJPEGComponent.SetQuant2IDCT.


procedure TmcmJPEGComponent.ResetDCDifference;
begin
  FDCValue := 0;
end; // TmcmJPEGComponent.ResetDCDifference.


function TmcmJPEGComponent.Extend(Bits, Count : integer) : integer;
var Sign : integer;
begin
  // The Count'th bit of Bits is the sign bit, where "1" is a positive and "0"
  // a negative value.
  Sign := 1 shl (Count - 1);
  if (Bits < Sign)
  then begin
       Sign := (-1 shl Count) + 1;
       Result := Bits + Sign;
  end
  else Result := Bits;
end; // TmcmJPEGComponent.Extend.


procedure TmcmJPEGComponent.StatACData(Value, Uncoded, Bits : integer);
begin
  if (Value >= 0)
  then FAC.Frequency[Value] := FAC.Frequency[Value] + 1;
end; // TmcmJPEGComponent.StatACData.


procedure TmcmJPEGComponent.StatDCData(Value, Bits : integer);
begin
  FDC.Frequency[Value] := FDC.Frequency[Value] + 1;
end; // TmcmJPEGComponent.StatDCData


procedure TmcmJPEGComponent.WriteACData(Value, Uncoded, Bits : integer);
var Code, Size : cardinal;
begin
  if (Value >= 0)
  then begin
       if (Value >= JPEGMaxHuffmanCodes)
       then Raise Exception.Create('JPEG Fatal error, Value is too large > JPEGMaxHuffmanCodes');
       FAC.Compress(Value, Code, Size);
       if (Size = 0)
       then Raise Exception.Create('JPEG Encoding, Missing Huffman code!');
       FStream.WriteJPEGBits(Code, Size);
  end;
  if (Bits <> 0)
  then FStream.WriteJPEGBits(Uncoded, Bits);
end; // TmcmJPEGComponent.WriteACData.


procedure TmcmJPEGComponent.WriteDCData(Value, Bits : integer);
var Code, Size : cardinal;
begin
  if (Value >= JPEGMaxHuffmanCodes)
  then Raise Exception.Create('JPEG Fatal error, Value is too large > JPEGMaxHuffmanCodes');
  FDC.Compress(Value, Code, Size);
  if (Size = 0)
  then Raise Exception.Create('JPEG Encoding, Missing Huffman code!');
  FStream.WriteJPEGBits(Code, Size);
  if (Value <> 0)
  then FStream.WriteJPEGBits(Bits, Value);
end; // TmcmJPEGComponent.WriteDCData.


procedure TmcmJPEGComponent.ProgressiveDCT;
var pCoef     : PJPEGCoefficient;
    CoefIndex : cardinal;
    i, Value  : cardinal;
    k         : integer;
    McuCol    : longint;
    McuRow    : longint;
begin
  i := 0;
  for McuRow := 0 to (FDataRows - 1)
  do for McuCol := 0 to (FDataCols - 1)
     do begin
        CoefIndex := i * SizeOf(TJPEGCoefficient);
        pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);
        Value := McuRow * FDataRowSize + McuCol * JPEGSampleWidth;
        Value := Value mod FAllocSize;
        for k := 0 to 7
        do begin
           CopyMemory(@FDCT.Data[k,0], @FMcuData^[Value], 8);
           inc(Value, FDataLineSize);
        end;
        FDCT.ForwardDCT(pCoef);
        inc(i);
     end;
end; // TmcmJPEGComponent.ProgressiveDCT.


procedure TmcmJPEGComponent.ProgressiveIDCT;
// Progressive scan only.
var pCoef     : PJPEGCoefficient;
    CoefIndex : cardinal;
    i, Value  : cardinal;
    k         : integer;
    McuCol    : longint;
    McuRow    : longint;
begin
  i := 0;
  for McuRow := 0 to (FDataRows - 1)
  do for McuCol := 0 to (FDataCols - 1)
     do begin
        // CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
        CoefIndex := i * SizeOf(TJPEGCoefficient);
        pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);
        FDCT.InverseDCTInt(pCoef);

        Value := FVertSample * McuRow * FDataRowSize + McuCol * FHorizSample * JPEGSampleWidth;
        Value := Value mod FAllocSize;
        for k := 0 to 7
        do begin
           CopyMemory(@FMcuData^[Value], @FDCT.Data[k,0], 8);
           inc(Value, FDataLineSize);
        end;
        inc(i);
     end;
end; // TmcmJPEGComponent.ProgressiveIDCT.


procedure TmcmJPEGComponent.Resample;
var i, j, k : longint;
    McuCol  : integer;
    ys, yt  : longint;
    xs, xt  : longint;
begin
  // Horizontal resample.
  if (FHorizSample <> 1)
  then begin
       if (FAllocSize = cardinal(FDataRowSize))
       then xt := FDataRowSize - 1
       else if (FVertSample = 1)
            then xt := FAllocSize - 1
            else xt := FAllocSize - cardinal(FDataRowSize) - 1;

       while (xt > 0)
       do begin
          for k := 0 to (JPEGSampleWidth - 1)
          do begin
             for McuCol := FMcuCols downto 1
             do begin
                xs := xt - (FHorizSample - 1) * JPEGSampleWidth;
                // Iterate through each line.
                for i := 0 to (JPEGSampleWidth - 1)
                do begin
                   for j := 0 to (FHorizSample - 1)
                   do begin
                      FMcuData^[xt] := FMcuData^[xs];
                      dec(xt);
                   end;
                   dec(xs);
                end;
             end;
          end;
          xt := xt - (FVertSample - 1) * FDataRowSize;
       end;
  end;

  // Vertical resample.
  if (FVertSample <> 1)
  then begin
       yt := FAllocSize - cardinal(FDataLineSize);
       while (yt > 0)
       do begin
          ys := yt - FDataBlockSize + FDataRowSize;
          for i := 0 to (JPEGSampleWidth - 1)
          do begin
             for j := 0 to (FVertSample - 1)
             do begin
                CopyMemory(@FMcuData^[yt], @FMcuData^[ys], FDataLineSize);
                dec(yt, FDataLineSize);
             end;
             dec(ys, FDataLineSize);
          end;
       end;
  end;
end; // TmcmJPEGComponent.Resample.


procedure TmcmJPEGComponent.Sample;
var i, j      : longint;
    ys, yt    : longword;
    xs, xt    : longword;
    Value     : integer;
    pLine     : PVectorB;
    pSource   : array[1..8] of PVectorB;
    RowLength : longint;
begin
  // Horizontal sample.
  if (FHorizSample <> 1)
  then begin
       ys := 0;
       xs := 0;
       xt := 0;
       while (ys < FAllocSize)
       do begin
          pLine := @FMcuData^[ys];
          while (xs < longword(FDataLineSize))
          do begin
             Value := 0;
             for i := 1 to FHorizSample
             do begin
                Value := Value + pLine^[xs];
                inc(xs);
             end;
             pLine^[xt] := Value div FHorizSample;
             inc(xt);
          end;
          inc(ys, FDataLineSize);
          xs := 0;
          xt := 0;
       end;
  end;

  // Vertical resample.
  if (FVertSample <> 1)
  then begin
       RowLength := FDataCols * JPEGSampleWidth;
       ys := 0;
       yt := 0;
       while (ys < FAllocSize)
       do begin
          pLine := @FMcuData^[yt];
          for i := 1 to FVertSample
          do begin
             pSource[i] := @FMcuData^[ys];
             inc(ys, FDataLineSize);
          end;

          for i := 1 to RowLength
          do begin
             Value := 0;
             for j := 1 to FVertSample
             do Value := Value + pSource[j]^[i];
             pLine^[i] := Value div FVertSample;
          end;
          inc(yt, FDataLineSize);
       end;
  end;
end; // TmcmJPEGComponent.Sample.


procedure TmcmJPEGComponent.RefineACCoefficient(var Coef : smallint; ApproxLow : integer);
begin
  if (Coef > 0)
  then begin
       if (FStream.ReadJPEGBits(1) <> 0)
       then Coef := Coef + (1 shl ApproxLow);
  end
  else if (Coef < 0)
       then begin
            if (FStream.ReadJPEGBits(1) <> 0)
            then Coef := Coef + (-1 shl ApproxLow);
       end;
end; // TmcmJPEGComponent.RefineACCoefficient.


procedure TmcmJPEGComponent.DecodeACFirstScan(McuRow, McuCol : integer;
                                              SpecStart, SpecEnd, ApproxLow : byte);
// Progressive scan only.
// Decodes the AC coefficients for a data unit within the spectral range.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    Bits      : integer;
    AC        : word;
    Value     : cardinal;
    k         : integer;
    rr, ss    : byte;
begin
  if (FEobRun > 0)
  then dec(FEobRun)
  else begin
       CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
       pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

       // Decode AC coefficients.
       k := SpecStart;
       while (k <= SpecEnd)
       do begin
          AC := FAC.Decompress;
          ss := AC and $0F;
          rr := AC shr $04;
          if (ss = 0)
          then begin
               // When ss is zero and rr is 15, the next 16 coefficients are
               // zero, otherwise create End Of Band Run (FEobRun).
               if (rr = 15)
               then inc(k, 16)
               else begin
                    if (rr <> 0)
                    then begin
                         Bits    := FStream.ReadJPEGBits(rr);
                         FEobRun := (1 shl rr) + Bits - 1;
                    end;
                    Break;
               end;
          end
          else begin
               // If ss is non-zero then rr gives the number of zero coefficients to
               // skip.
               inc(k, rr);

               if (k >= JPEGSampleSize)
               then Raise Exception.Create('JPEG Fatal error, Value out of range');

               // Get and extend the additional bits.
               Bits  := FStream.ReadJPEGBits(ss);
               Value := Extend(Bits, ss);
               PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] := Value shl ApproxLow;

               inc(k);
          end;
       end;
  end;
end; // TmcmJPEGComponent.DecodeACFirstScan.


procedure TmcmJPEGComponent.DecodeACRefineScan(McuRow, McuCol : integer;
                                               SpecStart, SpecEnd, ApproxLow : byte);
// Progressive scan only.
// Decodes the refining AC coefficients for a data unit within the spectral range.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    Count     : longint;
    Bits      : integer;
    Value     : cardinal;
    AC        : integer;
    i, k      : integer;
    rr, ss    : byte;
begin
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  k := SpecStart;
  while (k <= SpecEnd)
  do begin
     if (FEobRun <> 0)
     then begin
          // The End Of Band Run skips the entire data unit, why non-zero
          // coefficients must be refined
          while (k <= SpecEnd)
          do begin
             if (PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] <> 0)
             then RefineACCoefficient(PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]], ApproxLow);
             inc(k);
          end;
          dec(FEobRun);
     end
     else begin
          AC := FAC.Decompress;
          ss := AC and $0F;
          rr := AC shr $04;
          case ss of
          0 : begin
                // With ss = 0 we either have an End Of Band Run or 16 zero
                // coefficients.
                if (rr = 15)
                then begin
                     // Skip 16 zero coefficients.
                     i := 0;
                     while (i < 16) and (k <= SpecEnd)
                     do begin
                        if (k > SpecEnd)
                        then Raise Exception.Create('JPEG, Fatal error, Value out of range');

                        if (PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] <> 0)
                        then RefineACCoefficient(PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]], ApproxLow)
                        else inc(i);
                        inc(k);
                     end;
                end
                else begin
                     if (rr = 0)
                     then FEobRun := 1
                     else begin
                          Bits    := FStream.ReadJPEGBits(rr);
                          FEobRun := (1 shl rr) + Bits;
                     end;
                end;
              end;
          1 : begin
                // ss = 1 implies creating a new non-zero coefficient, rr
                // provides the number of zero coefficients to skip before
                // reaching this one.
                // Save the value for the new coefficient.
                Value := FStream.ReadJPEGBits(1);

                Count := 0;
                while (k < JPEGSampleSize) and
                      ((Count < rr) or (PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] <> 0))
                do begin
                   if (k > SpecEnd)
                   then Raise Exception.Create('JPEG Error in progressive scan.');

                   if (PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] <> 0)
                   then RefineACCoefficient(PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]], ApproxLow)
                   else inc(Count);

                   inc(k);
                end;

                if (k > SpecEnd)
                then Raise Exception.Create('JPEG Error in progressive scan.');

                if (Value <> 0)
                then PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] := 1 shl ApproxLow
                else PVectorSI(pCoef)^[JPEGZigZagInputOrder[k]] := -1 shl ApproxLow;

                inc(k);
              end;
          else Raise Exception.Create('JPEG Invalid value in input stream.'); // FError := EC_BADFORMAT;
          end;
     end;
  end;
end; // TmcmJPEGComponent.DecodeACRefineScan.


procedure TmcmJPEGComponent.DecodeDCFirstScan(McuRow, McuCol, ApproxLow : integer);
// Progressive scan only.
// Decodes the DC coefficient for a data unit.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    Count     : cardinal;
    Bits      : cardinal;
    Diff      : integer;
    DC        : integer;
begin
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  // Decode DC difference value;
  Count := FDC.Decompress;
  Bits  := FStream.ReadJPEGBits(Count);
  Diff  := Extend(Bits, Count);

  // Calculate DC value based on the DC difference value and previous DC value.
  DC := Diff + FDCValue;
  FDCValue := DC;
  pCoef^[0,0] := DC shl ApproxLow;
end; // TmcmJPEGComponent.DecodeDCFirstScan.


procedure TmcmJPEGComponent.DecodeDCRefineScan(McuRow, McuCol, ApproxLow : integer);
// Progressive scan only.
// Decodes the refined DC coefficient for a data unit.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
begin
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  if (FStream.ReadJPEGBits(1) <> 0)
  then pCoef^[0,0] := pCoef^[0,0] or (1 shl ApproxLow);
end; // TmcmJPEGComponent.DecodeDCRefineScan.


procedure TmcmJPEGComponent.DecodeSequential(McuRow, McuCol : integer);
var Count  : longint;
    Bits   : integer;
    Diff   : integer;
    Value  : cardinal;
    DC, AC : integer;
    k      : integer;
    rr, ss : word;
begin
  FillChar(FCoef^, SizeOf(TJPEGCoefficient), 0);

  // Decode DC difference value;
  Count := FDC.Decompress;
  Bits  := FStream.ReadJPEGBits(Count);
  Diff  := Extend(Bits, Count);

  // Calculate DC value based on the DC difference value and previous DC value.
  DC := Diff + FDCValue;
  FDCValue := DC;
  FCoef^[0,0] := DC;

  // Decode AC coefficients.
  k := 1;
  while (k < JPEGSampleSize)
  do begin
     AC := FAC.Decompress;
     ss := AC and $0F;
     rr := AC shr $04;
     if (ss = 0)
     then begin
          // ss is zero then rr should either be 15 or zero, where "0" means
          // that the rest of the coeffiecients are zero, or "15" which means
          // that the next 16 coefficients are zero.
          if (rr <> 15)
          then Break
          else inc(k, 15); // Actually k is incremented by 16, see inc(k) below.
     end
     else begin
          // If ss is non-zero then rr gives the number of zero coefficients to
          // skip.
          inc(k, rr);
          if (k >= JPEGSampleSize)
          then Raise Exception.Create('JPEG Fatal error, Value out of range');

          // Get and extend the additional bits.
          Bits  := FStream.ReadJPEGBits(ss);
          Value := Extend(Bits, ss);
          TVectorSI(FCoef^[0,0])[JPEGZigZagInputOrder[k]] := Value;
     end;
     inc(k);
  end;

  // Inverset Discreate Cosinus Transformation.
  FDCT.InverseDCTInt(FCoef);
  Value := McuRow * FDataRowSize + McuCol * FHorizSample * JPEGSampleWidth;
  Value := Value mod FAllocSize;
  for k := 0 to 7
  do begin
     CopyMemory(@FMcuData^[Value], @FDCT.Data[k,0], 8);
     inc(Value, FDataLineSize);
  end;
end; // TmcmJPEGComponent.DecodeSequential.


procedure TmcmJPEGComponent.ResetEobRun;
begin
  FEobRun := 0;
  //FEobRow := FDataRows * FDataCols;
  //FEobCol := FDataRows * FDataCols;
  FEobRow := FNilRows * FNilCols;
  FEobCol := FNilRows * FNilCols;
  FEobPos := JPEGSampleSize;
end; // TmcmJPEGComponent.ResetEobRun.


procedure TmcmJPEGComponent.EncodeEobRun;
var Bits    : integer;
    Value   : integer;
    ss      : integer;
begin
  if (FEobRun <> 0)
  then begin
       Bits := FEobRun;
       Value := Bits shr 1;
       ss := 0;
       while (Value <> 0)
       do begin
          Value := Value shr 1;
          inc(ss);
       end;
       FACFunction(ss shl 4, Bits, ss);
       FEobRun := 0;
  end;
end; // TmcmJPEGComponent.EncodeEobRun.


procedure TmcmJPEGComponent.EncodeRefineEobRun;
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    Bits      : integer;
    Value     : integer;
    k, ss     : integer;
    Row, Col  : integer;
    Counter   : integer;
    ssa       : integer;
begin
  if (FEobRun <> 0)
  then begin
       ssa := 1 shl FApproxLow;

       Bits := FEobRun;
       Value := Bits shr 1;
       ss := 0;
       while (Value <> 0)
       do begin
          Value := Value shr 1;
          inc(ss);
       end;
       FACFunction(ss shl 4, Bits, ss);

       Counter := 0;
       Row := FEobRow;
       Col := FEobCol;
       while (Counter < FEobRun)
       do begin
          CoefIndex := (Row * FDataCols + Col) * SizeOf(TJPEGCoefficient);
          pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

          for k := FEobPos to FSpectralEnd
          do begin
             Value := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[k]];
             Value := abs(Value div ssa);
             if (Value > 1)
             then FACFunction(-1, (Value and $01), 1);
          end;

          FEobPos := FSpectralStart;
          inc(Counter);
          inc(Col);
          if (Col = FNilCols) // FDataCols)
          then begin
               Col := 0;
               inc(Row);
               if (Row >= FDataRows) and (Counter <> FEobRun)
               then Raise EJPEGException.Create('JPEG, Internal error: EOB run extends beyond last row.');
          end;
       end;

       ResetEobRun;
  end;
end; // TmcmJPEGComponent.EncodeRefineEobRun.


procedure TmcmJPEGComponent.EncodeACFirstScan(McuRow, McuCol  : integer);
// Progressive scan only.
// Encodes the AC coefficients for a data unit within the spectral range.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    AC, Bits  : integer;
    Value     : integer;
    k, rr, ss : byte;
    ssa       : integer;
begin 
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  ssa := 1 shl FApproxLow;
  rr := 0;
  for k := FSpectralStart to FSpectralEnd
  do begin
     Value := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[k]];
     Value := Value div ssa;
     if (Value = 0)
     then inc(rr)
     else begin
          EncodeEobRun;

          // 16 is the longest run of zeros that can be encoded except for the
          // final End Of Block.
          while (rr >= 16)
          do begin
             FACFunction($F0, 0, 0);
             dec(rr, 16);
          end;

          if (Value >= 0)
          then Bits := Value
          else begin
               Value := -Value;
               Bits := Not(Value);
          end;
          ss := 0;
          while (Value <> 0)
          do begin
             Value := Value shr 1;
             inc(ss);
          end;

          AC := (rr shl 4) or ss;
          FACFunction(AC, Bits, ss);
          rr := 0;

          if (k = FSpectralEnd)
          then Exit;
     end;
  end;
  inc(FEobRun);
  if (FEobRun = $7FFF)
  then EncodeEobRun;
end; // TmcmJPEGComponent.EncodeACFirstScan.


procedure TmcmJPEGComponent.EncodeACRefineScan(McuRow, McuCol : integer);
// Progressive scan only.
// Encodes the refining AC coefficients for a data unit within the spectral range.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    AC        : integer;
    Value     : integer;
    k, j      : integer;
    rr        : byte;
    ssa       : integer;
    rrStart   : integer;
    rrCount   : integer;
    CorrectCount : integer;
    OldValue  : integer;
begin 
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  ssa := 1 shl FApproxLow;
  rr := 0;
  rrStart := FSpectralStart;
  CorrectCount := 0;
  for k := FSpectralStart to FSpectralEnd
  do begin
     Value := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[k]];
     Value := Value div ssa;
     if (Value = 0)
     then inc(rr)
     else begin
          if (Value = 1) or (Value = -1)
          then begin
               EncodeRefineEobRun;

               // 16 is the longest run of zeros that can be encoded except for the
               // final End Of Block.
               while (rr >= 16)
               do begin
                  FACFunction($F0, 0, 0);
                  dec(rr, 16);

                  // Refine all coefficients skipped by the zero run.
                  rrCount := 0;
                  while (rrCount < 16)
                  do begin
                     if (rrStart < FSpectralStart) or
                        (rrStart > FSpectralEnd) or
                        (CorrectCount > JPEGSampleSize)
                     then Raise EJPEGException.Create('JPEG, Internal error: Invalid zero run.');

                     OldValue := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[rrStart]];
                     OldValue := abs(OldValue div ssa);
                     if (OldValue > 1)
                     then begin
                          FACFunction(-1, (OldValue and $01), 1);
                          dec(CorrectCount);
                     end
                     else begin
                          if (OldValue = 0)
                          then inc(rrCount)
                          else Raise EJPEGException.Create('JPEG, Internal error: Unexpected value.');
                     end;

                     inc(rrStart);
                  end;
               end;

               AC := (rr shl 4) or 1;
               if (Value > 0)
               then FACFunction(AC, 1, 1)
               else FACFunction(AC, 0, 1);
               rr := 0;

               for j := rrStart to (k - 1)
               do begin
                  OldValue := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[j]];
                  OldValue := abs(OldValue div ssa);

                  if (OldValue > 1)
                  then begin
                       FACFunction(-1, (OldValue and$01), 1);
                       dec(CorrectCount);
                  end;
               end;
               rrStart := k + 1;
               if (k = FSpectralEnd)
               then Exit;
          end
          else begin
               inc(CorrectCount);
          end;
     end;
  end;

  // If we reach this point, the data unit ends with a series of zero
  // coefficients or previously non-zero coefficients that were skipped.
  if (FEobRun = 0)
  then begin
       FEobRow := McuRow;
       FEobCol := McuCol;
       FEobPos := rrStart;
  end;

  inc(FEobRun);
  if (FEobRun = $7FFF)
  then EncodeEobRun;
end; // TmcmJPEGComponent.EncodeACRefineScan.


procedure TmcmJPEGComponent.EncodeDCFirstScan(McuRow, McuCol : integer);
// Progressive scan only.
// Encodes the first DC coefficient for a data unit.
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    ss        : integer;
    Bits      : integer;
    Diff      : integer;
    DC        : integer;
begin 
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  if (pCoef^[0,0] >= 0)
  then DC := pCoef^[0,0] shr FApproxLow
  else DC := -(-pCoef^[0,0] shr FApproxLow);
  Diff := DC - FDCValue;
  FDCValue := DC;

  if (Diff >= 0)
  then Bits := Diff
  else begin
       Diff := -Diff;
       Bits := Not(Diff);
  end;
  ss := 0;
  while (Diff <> 0)
  do begin
     Diff := Diff shr 1;
     inc(ss);
  end;
  FDCFunction(ss, Bits);
end; // TmcmJPEGComponent.EncodeDCFirstScan.


procedure TmcmJPEGComponent.EncodeDCRefineScan(McuRow, McuCol : integer);
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    DC        : integer;
begin
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);

  DC := (pCoef^[0,0] shr FApproxLow) and $01;
  FStream.WriteJPEGBits(DC, 1);
end; // TmcmJPEGComponent.EncodeDCRefineScan.


procedure TmcmJPEGComponent.EncodeSequential(McuRow, McuCol : integer);
var pCoef     : PJPEGCoefficient;
    CoefIndex : longint;
    //Index   : cardinal;
    Bits    : integer;
    Diff    : integer;
    Value   : integer;
    DC, AC  : integer;
    k       : integer;
    rr, ss  : integer;
begin
  CoefIndex := (McuRow * FDataCols + McuCol) * SizeOf(TJPEGCoefficient);
  pCoef := PJPEGCoefficient(PAnsiChar(FCoef) + CoefIndex);
  {
  // Discreate Cosinus Transformation.
  Index := McuRow * FDataRowSize + McuCol * JPEGSampleWidth;
  Index := Index mod FAllocSize;
  for k := 0 to 7
  do begin
     CopyMemory(@FDCT.Data[k,0], @FMcuData^[Index], 8);
     inc(Index, FDataLineSize);
  end;
  FDCT.ForwardDCT(FCoef);
  }

  // DC diffenrece calculation.
  DC := pCoef^[0,0];
  Diff := DC - FDCValue;
  FDCValue := DC;

  //
  if (Diff >= 0)
  then Bits := Diff
  else begin
       Diff := -Diff;
       Bits := Not(Diff);
  end;
  ss := 0;
  while (Diff <> 0)
  do begin
     inc(ss);
     Diff := Diff shr 1;
  end;
  FDCFunction(ss, Bits);

  // AC coefficient coding.
  rr := 0;
  for k := 1 to (JPEGSampleSize - 1)
  do begin
     if (TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[k]] <> 0)
     then begin
          // 16 is the longest run of zeros that can be encoded except for the
          // final End Of Block.
          while (rr >= 16)
          do begin
             FACFunction($F0, 0, 0);
             dec(rr, 16);
          end;

          Value := TVectorSI(pCoef^[0,0])[JPEGZigZagInputOrder[k]];
          if (Value >= 0)
          then Bits := Value
          else begin
               Value := -Value;
               Bits := Not(Value);
          end;
          ss := 0;
          while (Value <> 0)
          do begin
             Value := Value shr 1;
             inc(ss);
          end;

          AC := (rr shl 4) or ss;
          FACFunction(AC, Bits, ss);
          rr := 0;
     end
     else inc(rr);
  end;
  // A code "0" indicates that the remaining AC coefficients are zero.
  if (rr > 0)
  then FACFunction(0, 0, 0);
end; // TmcmJPEGComponent.EncodeSequential.


{$ENDIF}

class function TmcmJPEGImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32];
end; // TmcmJPEGImage.GetColorFormats.


class function TmcmJPEGImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32])
  then begin
       if (High(Compress) > Count)
       then Compress[Count] := CP_JPEG_STD;
       inc(Count);
       if (High(Compress) > Count)
       then Compress[Count] := CP_JPEG_PROG;
       inc(Count);
  end;
  Result := Count;
end; // TmcmJPEGImage.GetCompressionFromColor.


class function TmcmJPEGImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_JPEG_STD  : Result := resCP_JPEG_STD;
  CP_JPEG_PROG : Result := resCP_JPEG_PROG;
  else Result := '';
  end;
end; // TmcmJPEGImage.GetCompressionName.


constructor TmcmJPEGImage.Create;
{$IFDEF mcmJPEGEnginee}
var i, j : integer;
{$ENDIF}
begin
  Inherited Create;
  FStream.BigEndian := True;

  {$IFDEF mcmJPEGEnginee}
  FPhotometric     := PM_YCBCR;
  FYCbCrMode       := JYCC_411;
  FComponentCount  := 0;
  FRowsPerRestart  := 0;
  FRestartMarker   := 0;
  FRestartInterval := 0;
  for i := 0 to (JPEGMaxHuffmanTables - 1)
  do FYCbCrComp[i] := Nil;

  for i := 0 to 1
  do for j := 0 to (JPEGMaxHuffmanTables - 1)
     do FHuffman[i,j] := Nil;
  {$ENDIF}
end; // TmcmJPEGImage.Create.


destructor TmcmJPEGImage.Destroy;
{$IFDEF mcmJPEGEnginee}
var i, j : integer;
{$ENDIF}
begin
  {$IFDEF mcmJPEGEnginee}
  for i := 0 to (JPEGMaxHuffmanTables - 1)
  do begin
     if Assigned(FYCbCrComp[i])
     then FYCbCrComp[i].Free;
     FYCbCrComp[i] := Nil;
  end;

  for i := 0 to 1
  do for j := 0 to (JPEGMaxHuffmanTables - 1)
     do begin
        if Assigned(FHuffman[i,j])
        then FHuffman[i,j].Free;
        FHuffman[i,j] := Nil;
     end;
  {$ENDIF}
  Inherited Destroy;
end; // TmcmJPEGImage.Destroy.


function TmcmJPEGImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var SavePos : longint;
    Code    : word;
    // Count   : word;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         SavePos := FStream.Position;
         Code := FStream.ReadWord;
         if (Code = JPG_SOI)
         then begin
              FError := EC_OK;
              // Too many JFIF files does not follow standard -
              // so the check is reduced to matching JPG_SOI.
              (*
              Code := FStream.ReadWord;

              if (Code = JPG_COM) // Comment.
              then begin
                   Count := FStream.ReadWord - SizeOf(Count);
                   FStream.Position := FStream.Position + Count;
                   Code := FStream.ReadWord;
              end;

              if (Code = JPG_APP1) // Comment.
              then begin
                   Count := FStream.ReadWord - SizeOf(Count);
                   FStream.Position := FStream.Position + Count;
                   Code := FStream.ReadWord;
              end;

              if (Code = JPG_APP0)
              then begin // Read JFIF APP0 Header.
                   Count := FStream.ReadWord - SizeOf(Count);

                   if (Count = SizeOf(TJFIFAPP0Header))
                   then ;

                   FStream.Read(FJFIFAPP0Header, SizeOf(TJFIFAPP0Header));
                   if (FJFIFAPP0Header.Identifier = 'JFIF')
                   then FError := EC_OK; // I'm sure it's a JFIF format.
              end
              else FError := EC_BADFORMAT; // May be an OK JPEG file!
              *)
         end;
         FStream.Position := SavePos;
       except
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmJPEGImage.IsFormatValid.


{$IFDEF mcmJPEGEnginee}

function TmcmJPEGImage.GetScanComponent(Index : byte) : TmcmJPEGComponent;
var i : byte;
begin
  Result := Nil;
  i := 0;
  while (i < JPEGMaxHuffmanTables) and (Result = Nil)
  do begin
     if Assigned(FYCbCrComp[i])
     then begin
          if (FYCbCrComp[i].ID = Index)
          then begin
               FYCbCrIndex := i;
               Result := FYCbCrComp[i];
          end
          else inc(i);
     end
     else inc(i);
  end;

  if (Result = Nil)
  then begin
       i := 0;
       while (i < JPEGMaxHuffmanTables) and (Result = Nil)
       do begin
          if (FYCbCrComp[i] = Nil)
          then begin
               FYCbCrComp[i] := TmcmJPEGComponent.Create(Index, FStream);
               FYCbCrIndex := i;
               Result := FYCbCrComp[i];
          end;
          inc(i);
       end;
  end;
end; // TmcmJPEGImage.GetScanComponent.


procedure TmcmJPEGImage.ResetDCDifference;
var i : integer;
begin
  // Reset DC difference
  for i := 0 to (FComponentCount - 1)
  do FYCbCrComp[i].ResetDCDifference;
end; // TmcmJPEGImage.ResetDCDifference.


procedure TmcmJPEGImage.CalcMCUDimension;
var i             : integer;
    ScanComponent : TmcmJPEGComponent;
begin
  // Determin the largest horizontal and
  // vertical sample frequencies.
  FMaxHorizFreq := 0;
  FMaxVertFreq  := 0;
  for i := 0 to (FComponentCount - 1)
  do begin
     ScanComponent := GetScanComponent(FJFIFSOF.Components[i].Identifier);
     if (FMaxHorizFreq < ScanComponent.HorizFreq)
     then FMaxHorizFreq := ScanComponent.HorizFreq;
     if (FMaxVertFreq < ScanComponent.VertFreq)
     then FMaxVertFreq := ScanComponent.VertFreq;
     if (ScanComponent.HorizFreq = 0) or (ScanComponent.VertFreq = 0)
     then FError := EC_SAMPLEFREQ;
  end;

  FMCUHeight := FMaxVertFreq * JPEGSampleWidth;
  FMCUWidth  := FMaxHorizFreq * JPEGSampleWidth;
  try
    FMcuRows := (FJFIFSOF.ImageHeight + FMCUHeight - 1) div FMCUHeight;
    FMcuCols := (FJFIFSOF.ImageWidth + FMCUWidth - 1) div FMCUWidth;
  except
    FMcuRows := 0;
    FMcuCols := 0;
    FError := EC_SAMPLEFREQ;
  end;

  for i := 0 to (FComponentCount - 1)
  do begin
     ScanComponent := GetScanComponent(FJFIFSOF.Components[i].Identifier);
     ScanComponent.McuCols      := FMcuCols;
     ScanComponent.McuRows      := FMcuRows;
     ScanComponent.MaxHorizFreq := FMaxHorizFreq;
     ScanComponent.MaxVertFreq  := FMaxVertFreq;
     ScanComponent.ImageHeight  := FJFIFSOF.ImageHeight;
     ScanComponent.ImageWidth   := FJFIFSOF.ImageWidth;
  end;
end; // TmcmJPEGImage.CalcMCUDimension.


procedure TmcmJPEGImage.ReadRestartMarker;
var Code : byte;
begin
  FStream.Read(Code, SizeOf(byte));
  if (Code <> $FF)
  then FError := EC_DECOMPRESSION
  else begin
       while (Code = $FF)
       do FStream.Read(Code, SizeOf(byte));
       FStream.ResetBitIndex;
       //FBufBit := 0;
       case ($FF00 or Code) of
       JPG_RST0..
       JPG_RST7 : begin
                    // Check if restart mark is in sequence.
                    if (FRestartMarker = ($0F and Code))
                    then begin
                         // Move to next restart marker.
                         inc(FRestartMarker);
                         FRestartMarker := FRestartMarker mod 8;
                         ResetDCDifference;
                    end
                    else FError := EC_DECOMPRESSION; // Restart mark is out of sequence.
                  end;
       else FError := EC_DECOMPRESSION; // Restart marker missing
       end;
  end;
end; // TmcmJPEGImage.ReadRestartMarker.


procedure TmcmJPEGImage.WriteRestartInterval(RestartInterval : word);
begin
  if (FRestartInterval <> RestartInterval)
  then begin
       FRestartInterval := RestartInterval;
       FStream.WriteWord(JPG_DRI);
       FStream.WriteWord(4);
       FStream.WriteWord(FRestartInterval);
  end;
end; // TmcmJPEGImage.WriteRestartInterval.


procedure TmcmJPEGImage.WriteRestartMarker;
begin
  FStream.FlushBits;
  FStream.WriteWord(JPG_RST0 or FRestartMarker);
  inc(FRestartMarker);
  FRestartMarker := FRestartMarker mod 8;
end; // TmcmJPEGImage.WriteRestartMarker.


procedure TmcmJPEGImage.InitYCbCrTables;
var i : integer;
    j : single;
begin
  // Fill tables with multiplied values.
  // RGB <-> YCbCr formulas :
  //
  // Y  =  0.257R + 0.504G + 0.098B + 16
  // Cb = -0.148R - 0.291G + 0.439B + 128
  // Cr =  0.439R - 0.368G - 0.071B + 128
  //
  // R = 1.164 * (Y - 16) - 0.391 * (Cb - 128) - 0.813 * (Cr - 128)
  // G = 1.164 * (Y - 16) + 1.596 * (Cr - 128)
  // B = 1.164 * (Y - 16) + 2.018 * (Cb - 128)
  //
  // R, G and B range from 0 to 255.
  // Y ranges from 16 to 235.
  // Cb and Cr range from 16 to 240.
  //
  // The algorithm in this unit makes use of table lookups. It uses four tables
  // with pre-multiplied values:
  //
  // YToRGB : 1.164 * (Y - 16)
  // CrToR  : 1.596 * (Cr - 128)
  // CbToG  : 0.391 * (Cb - 128)
  // CrToG  : 0.813 * (Cr - 128)
  // CbToB  : 2.018 * (Cb - 128)
  //
  // Improved alogrithm
  // YToRGB : Y
  // CrToR  : 1.596 * (Cr - 127.5)
  // CbToG  : 0.391 * (Cb - 127.5)
  // CrToG  : 0.813 * (Cr - 127.5)
  // CbToB  : 2.018 * (Cb - 127.5)

  if {$IFNDEF DCB3_5} Not(FMMX) {$ELSE} true {$ENDIF}
  then begin
       for i := 0 to 255
       do begin
          // YToRGB[i] := Round((i - 16) * 1.164);
          j := i - 127.50;
          CbToB[i]  := Round(j * 1.7710);
          CbToG[i]  := Round(j * 0.3456);
          CrToR[i]  := Round(j * 1.4022);
          CrToG[i]  := Round(j * 0.7145);
       end;

       // Fill ByteClip array to clip values to the range of 0 to 255
       for i := Low(ByteClip) to High(ByteClip)
       do if (i < 0)
          then ByteClip[i] := 0
          else if (i > 255)
               then ByteClip[i] := 255
               else ByteClip[i] := i;
  end;
end; // TmcmJPEGImage.InitYCbCrTables.


procedure TmcmJPEGImage.CopyFromRGB(McuRow : integer);
var i, j, k : longint;
    x, y    : longint;
    pImage  : PVectorB;
    LastY   : longint;
    R, G, B : integer;
begin
  if (FYCbCrComp[0].Data <> Nil) and
     (FYCbCrComp[1].Data <> Nil) and
     (FYCbCrComp[2].Data <> Nil)
  then begin
       if (McuRow > -1)
       then begin
            y := FDibInfo^.bmiHeader.biHeight - McuRow * FYCbCrComp[0].McuPixVert - 1;
            LastY := y - JPEGSampleWidth * FYCbCrComp[0].MaxVertFreq;
       end
       else begin
            y := FDibInfo^.bmiHeader.biHeight - 1;
            LastY := -1;
       end;
       if (LastY < 0)
       then Lasty := -1;
       pImage := @PVectorB(FDibBits)^[y * FLongWidth];

       i := 0;
       j := 0;
       case FDibInfo^.bmiHeader.biBitCount of
       8  : while (y > LastY)
            do begin
               x := 0;
               while (x < FLongWidth)
               do begin
                  B := FDibInfo^.bmiColors[pImage^[x]].rgbBlue;
                  G := FDibInfo^.bmiColors[pImage^[x]].rgbGreen;
                  R := FDibInfo^.bmiColors[pImage^[x]].rgbRed;
                  inc(x);
                  // Y
                  FYCbCrComp[0].Data[i] := Round(0.2989 * R + 0.5867 * G + 0.1144 * B);
                  // Cb
                  FYCbCrComp[1].Data[i] := Round(-0.1687 * R - 0.3312 * G + 0.5000 * B + 127.50);
                  // Cr
                  FYCbCrComp[2].Data[i] := Round(0.5000 * R - 0.4183 * G - 0.0816 * B + 127.50);
                  inc(i);
               end;

               // Duplicate remaining values in last Mcu.
               x := i - 1;
               k := FDibInfo^.bmiHeader.biWidth - 1;
               while (k < FYCbCrComp[0].DataLineSize - 1)
               do begin
                  FYCbCrComp[0].Data^[i] := FYCbCrComp[0].Data^[x];
                  FYCbCrComp[1].Data^[i] := FYCbCrComp[1].Data^[x];
                  FYCbCrComp[2].Data^[i] := FYCbCrComp[2].Data^[x];
                  inc(i);
                  inc(k);
               end;

               inc(j, FYCbCrComp[0].DataLineSize);
               i := j;
               dec(y);
               pImage := @PVectorB(FDibBits)^[y * FLongWidth];
            end;
       24 : while (y > LastY)
            do begin
               x := 0;
               while (x < FLongWidth - 2)
               do begin
                  B := pImage^[x];
                  inc(x);
                  G := pImage^[x];
                  inc(x);
                  R := pImage^[x];
                  inc(x);
                  // Y
                  FYCbCrComp[0].Data[i] := Round(0.2989 * R + 0.5867 * G + 0.1144 * B);
                  // Cb
                  FYCbCrComp[1].Data[i] := Round(-0.1687 * R - 0.3312 * G + 0.5000 * B + 127.50);
                  // Cr
                  FYCbCrComp[2].Data[i] := Round(0.5000 * R - 0.4183 * G - 0.0816 * B + 127.50);

                  inc(i);
               end;

               // Duplicate remaining values in last Mcu.
               x := i - 1;
               k := FDibInfo^.bmiHeader.biWidth - 1;
               while (k < FYCbCrComp[0].DataLineSize - 1)
               do begin
                  FYCbCrComp[0].Data^[i] := FYCbCrComp[0].Data^[x];
                  FYCbCrComp[1].Data^[i] := FYCbCrComp[1].Data^[x];
                  FYCbCrComp[2].Data^[i] := FYCbCrComp[2].Data^[x];
                  inc(i);
                  inc(k);
               end;

               inc(j, FYCbCrComp[0].DataLineSize);
               i := j;
               dec(y);
               pImage := @PVectorB(FDibBits)^[y * FLongWidth];
            end;
       32 : while (y > LastY)
            do begin
               x := 0;
               while (x < FLongWidth - 3)
               do begin
                  B := pImage^[x];
                  inc(x);
                  G := pImage^[x];
                  inc(x);
                  R := pImage^[x];
                  inc(x);
                  inc(x);
                  // Y
                  FYCbCrComp[0].Data[i] := Round(0.2989 * R + 0.5867 * G + 0.1144 * B);
                  // Cb
                  FYCbCrComp[1].Data[i] := Round(-0.1687 * R - 0.3312 * G + 0.5000 * B + 127.50);
                  // Cr
                  FYCbCrComp[2].Data[i] := Round(0.5000 * R - 0.4183 * G - 0.0816 * B + 127.50);
                  inc(i);
               end;

               // Duplicate remaining values in last Mcu.
               x := i - 1;
               k := FDibInfo^.bmiHeader.biWidth - 1;
               while (k < FYCbCrComp[0].DataLineSize - 1)
               do begin
                  FYCbCrComp[0].Data^[i] := FYCbCrComp[0].Data^[x];
                  FYCbCrComp[1].Data^[i] := FYCbCrComp[1].Data^[x];
                  FYCbCrComp[2].Data^[i] := FYCbCrComp[2].Data^[x];
                  inc(i);
                  inc(k);
               end;

               inc(j, FYCbCrComp[0].DataLineSize);
               i := j;
               dec(y);
               pImage := @PVectorB(FDibBits)^[y * FLongWidth];
            end;
       end;

       // Duplicate line in last Mcu.
       y := i - FYCbCrComp[0].DataLineSize;
       while (cardinal(i) < FYCbCrComp[0].FAllocSize)
       do begin
          CopyMemory(@FYCbCrComp[0].Data^[i], @FYCbCrComp[0].Data^[y], FYCbCrComp[0].DataLineSize);
          CopyMemory(@FYCbCrComp[1].Data^[i], @FYCbCrComp[1].Data^[y], FYCbCrComp[0].DataLineSize);
          CopyMemory(@FYCbCrComp[2].Data^[i], @FYCbCrComp[2].Data^[y], FYCbCrComp[0].DataLineSize);
          inc(i, FYCbCrComp[0].DataLineSize);
       end;
  end;
end; // TmcmJPEGImage.CopyFromRGB.


procedure TmcmJPEGImage.CopyFromGrey(McuRow : integer);
// Copies grey scale date from image to "data unit".
var i, j   : longint;
    x, y   : longint;
    pImage : PVectorB;
    LastY  : longint;
begin
  if (FYCbCrComp[0].Data <> Nil)
  then begin
       if (McuRow > -1)
       then begin
            y := FDibInfo^.bmiHeader.biHeight - McuRow * FYCbCrComp[0].McuPixVert - 1;
            LastY := y - JPEGSampleWidth * FYCbCrComp[0].MaxVertFreq;
       end
       else begin
            y := FDibInfo^.bmiHeader.biHeight - 1;
            LastY := -1;
       end;
       if (LastY < 0)
       then Lasty := -1;
       pImage := @PVectorB(FDibBits)^[y * FLongWidth];
       i := 0;
       x := FDibInfo^.bmiHeader.biWidth - 1;

       if (GetPhotometric = PM_INVGREY)
       then begin
            while (y > LastY)
            do begin
               for j := 0 to x
               do PVectorB(@FYCbCrComp[0].Data^[i])^[j] := 255 - pImage^[j];
               inc(i, FYCbCrComp[0].DataLineSize);
               dec(y);
               pImage := @PVectorB(FDibBits)^[y * FLongWidth];
            end;
       end
       else begin
            while (y > LastY)
            do begin
               CopyMemory(@FYCbCrComp[0].Data^[i], pImage, FLongWidth);
               inc(i, FYCbCrComp[0].DataLineSize);
               dec(y);
               pImage := @PVectorB(FDibBits)^[y * FLongWidth];
            end;
       end;
  end;
end; // TmcmJPEGImage.CopyFromGrey.


procedure TmcmJPEGImage.CopyToRGB(McuRow : integer);
{
 const // Using constants causes MMX to become very slow!?!
    cr         : array[0..3] of smallint = (16384, 0, 22974, 0);
    cg         : array[0..3] of smallint = (16384, -5662, -11706, 8192);
    cb         : array[0..3] of smallint = (16384, 29016, 0, 8192);
    co         : array[0..3] of smallint = (0, -128, -128, 0);
}
var i, j    : longint;
    x, y    : longint;
    pImage  : PVectorB;
    LastY   : longint;
    CbB     : integer;
    CbG     : integer;
    CrR     : integer;
    CrG     : integer;
    YVal    : integer;

    {$IFNDEF DCB3_5}
    lWidth  : integer;
    pY, pCb : pointer;
    pCr     : pointer;
    cr      : array[0..3] of smallint;
    cg      : array[0..3] of smallint;
    cb      : array[0..3] of smallint;
    co      : array[0..3] of smallint;
    {$ENDIF}
begin
  try
    if (FYCbCrComp[0].Data <> Nil) and
       (FYCbCrComp[1].Data <> Nil) and
       (FYCbCrComp[2].Data <> Nil)
    then begin
         if (McuRow > -1)
         then begin
              y := FDibInfo^.bmiHeader.biHeight - McuRow * FYCbCrComp[0].McuPixVert - 1;
              LastY := y - JPEGSampleWidth * FYCbCrComp[0].MaxVertFreq;
         end
         else begin
              y := FDibInfo^.bmiHeader.biHeight - 1;
              LastY := -1;
         end;
         if (LastY < 0)
         then Lasty := -1;
         pImage := @PVectorB(FDibBits)^[y * FLongWidth];

         if (FPhotometric = PM_RGB)
         then begin
              i := 0;
              j := 0;
              while (y > LastY)
              do begin
                 x := 0;
                 while (x < FLongWidth - 2)
                 do begin
                    pImage^[x] := FYCbCrComp[2].Data[i]; // Blue.
                    inc(x);
                    pImage^[x] := FYCbCrComp[1].Data[i]; // Green.
                    inc(x);
                    pImage^[x] := FYCbCrComp[0].Data[i]; // Red.
                    inc(x);
                    inc(i);
                 end;
                 inc(j, FYCbCrComp[0].DataLineSize);
                 i := j;
                 dec(y);
                 pImage := @PVectorB(FDibBits)^[y * FLongWidth];
              end;
         end
         else begin // PM_YCBCR
              {$IFNDEF DCB3_5}
              if FMMX
              then begin
                   cr[0] := 16384; cr[1] := 0; cr[2] := 22974; cr[3] := 0;
                   cg[0] := 16384; cg[1] := -5662; cg[2] := -11706; cg[3] := 8192;
                   cb[0] := 16384; cb[1] := 29016; cb[2] := 0; cb[3] := 8192;
                   co[0] := 0; co[1] := -128; co[2] := -128; co[3] := 0;
                   lWidth  := FDibInfo^.bmiHeader.biWidth;
                   i := 0;
                   j := 0;
                   while (y > LastY)
                   do begin
                      pY  := @FYCbCrComp[0].Data[i];
                      pCb := @FYCbCrComp[1].Data[i];
                      pCr := @FYCbCrComp[2].Data[i];
                      asm
                        // Save registers to stack
                        push      ebx
                        push      edi
                        push      esi

                        // data points to process.
                        mov       ecx,lWidth
                        test      ecx,ecx
                        jz        @EndOfData   // Check that data count > zero bytes.

                        shr       ecx,2
                        dec       ecx
                        test      ecx,ecx
                        jz        @EndOfLine   // Check that data count > zero bytes.


                        // Set-up initial pointers to image line and coefficients.
                        mov       edi,pImage
                        mov       esi,pY
                        mov       ebx,pCb
                        mov       edx,pCr
                        xor       eax,eax

                        {$IFNDEF DCB3_5}
                        pxor      mm7,mm7
                        {$ELSE}
                        db   $0F,$EF,$FF
                        {$ENDIF}

                        // process pixels
                        @LoopQWORD:
                        {$IFNDEF DCB3_5}
                        movd      mm0,[esi] // Read X3 X2 X1 X0
                        punpcklbw mm0,mm7  // convert bytes to words
                        movd      mm1,[ebx] // Read Y3 Y2 Y1 Y0
                        punpcklbw mm1,mm7  // convert bytes to words
                        movd      mm2,[edx] // Read Z3 Z2 Z1 Z0
                        punpcklbw mm2,mm7  // convert bytes to words
                        {$ELSE}
                        (*
                        db   $0F,$6E,$06
                        db   $0F,$60,$C7
                        db   $0F,$6E,$0B
                        db   $0F,$60,$CF
                        db   $0F,$6E,$12
                        db   $0F,$60,$D7
                        *)
                        {$ENDIF}

                        add       esi,4
                        add       ebx,4

                        {$IFNDEF DCB3_5}

                        // Perform a 4x4 matrix transposition,
                        // from:
                        //   x3 x2 x1 x0
                        //   y3 y2 y1 y0
                        //   z3 z2 z1 z0
                        //   o3 o2 o1 o0
                        // to:
                        //  o0 z0 y0 x0
                        //  o1 z1 y1 x1
                        //  o2 z2 y2 x2
                        //  o3 z3 y3 x3
                        movq      mm4,mm0
                        pxor      mm3,mm3

                        punpckhwd mm4,mm1
                        punpcklwd mm0,mm1

                        movq      mm5,mm2
                        punpckhwd mm5,mm3
                        punpcklwd mm2,mm3

                        movq      mm1,mm0
                        punpckldq mm0,mm2  // mm0 = c0z0y0x0
                        punpckhdq mm1,mm2  // mm1 = c1z1y1x1

                        movq      mm2,mm4
                        movq      mm3,mm4
                        punpckldq mm2,mm5  // mm2 = c2z2y2x2
                        punpckhdq mm3,mm5  // mm3 = c3z3y3x3

                        // Add Offset
                        movq      mm4,qword ptr co   // Add offset
                        add       edx,4
                        paddsw    mm0,mm4
                        paddsw    mm1,mm4

                        // Compute color conversion.
                        movq      mm5,mm0
                        movq      mm6,mm0
                        pmaddwd   mm0,qword ptr cb   // mm0 = z0cb2, y0cb1+x0cb0
                        pmaddwd   mm5,qword ptr cg   // mm5 = z0cg2, y0cg1+x0cg0
                      paddsw    mm2,mm4
                      paddsw    mm3,mm4

                        // Sum mm0 & mm5's upper and lower dwords.
                        movq      mm4,mm0
                        punpckldq mm0,mm5  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                        punpckhdq mm4,mm5  // mm4 = z0cg2, z0cb2
                        paddd     mm0,mm4  // mm0 = 00 G0 00 B0
                      pmaddwd   mm6,qword ptr cr   // mm6 = z0cr2, y0cr1+x0cr0
                        psrad     mm0,14   // Scale dwords by 16384
                      movq      mm5,mm1  //
                      pmaddwd   mm1,qword ptr cb   // mm1 = z1cb2, y1cb1+x1cb1
                        packssdw  mm0,mm7  // Pack dwords to words, mm0 = 00 00 G0 B0

                        // Sum mm6 & mm1's upper and lower dwords.
                        movq      mm4,mm6
                        punpckldq mm6,mm1  // mm6 = y1cb1+x1cb1, y0cr1+x0cr0
                        punpckhdq mm4,mm1  // mm4 = z1cb2, z0cr2
                        paddd     mm6,mm4  // mm6 = 00 B1 00 R0
                        psrad     mm6,14   // Scale dwords by 16384
                        packssdw  mm6,mm7  // Pack dwords to words, mm6 = 00 00 B1 R0
                        psllq     mm6,32   // mm6 = B1 R0 00 00

                        por       mm0,mm6  // mm0 = B1 R0 G0 B0
                      movq      mm6,mm5
                      pmaddwd   mm5,qword ptr cg   // mm5 = z1cg2, y1cg1+x1cg0
                        packuswb  mm0,mm7  // Pack words to bytes

                        pmaddwd   mm6,qword ptr cr   // mm6 = z1cr2, y1cr1+x1cr0

                        movd      [edi+eax*4],mm0 // write B1 R0 G0 B0
                        inc       eax

                       //---

                        // Sum mm5 & mm6's upper and lower dwords.
                        movq      mm1,mm5
                        punpckldq mm5,mm6  // mm5 = y1cr1+x1cr0, y1cg1+x1cg0
                        punpckhdq mm1,mm6  // mm1 = z1cr2, z1cg2
                      movq      mm6,mm2
                        paddd     mm1,mm5  // mm1 = 00 R1 00 G1
                      movq      mm5,mm2
                        psrad     mm1,14   // Scale dwords by 16384
                      pmaddwd   mm2,qword ptr cb   // mm2 = z2cb2, y2cb1+x2cb1
                        packssdw  mm1,mm7  // mm1 = 00 00 R1 G1

                        pmaddwd   mm5,qword ptr cg   // mm5 = z2cg2, y2cg1+x2cg0

                        // Sum mm2 & mm5's upper and lower dwords.
                        movq      mm4,mm2
                        punpckldq mm2,mm5  // mm2 = y2cg1+x2cg0, y2cb1+x2cb1
                        punpckhdq mm4,mm5  // mm4 = z2cg2, z2cb2
                      pmaddwd   mm6,qword ptr cr   // mm6 = z2cr2, y2cr1+x2cr0
                        paddd     mm2,mm4  // mm2 = 00 G2 00 B2
                      movq      mm5,mm3
                        psrad     mm2,14   // Scale dwords by 16384
                      pmaddwd   mm3,qword ptr cb   // mm3 = z3cb2, y3cb1+x3cb1
                        packssdw  mm2,mm7  // mm2 = 00 00 G2 B2
                      // Sum mm6 & mm3's upper and lower dwords.
                      movq      mm0,mm6
                        psllq     mm2,32   // mm2 = G2 B2 00 00
                      punpckldq mm6,mm3  // mm6 = y3cb1+x3cb1, y2cr1+x2cr0
                        por       mm1,mm2  // mm1 = G2 B2 R1 G1
                      punpckhdq mm0,mm3  // mm0 = z3cb2, z2cr2
                        packuswb  mm1,mm7  // Pack words to bytes
                        movd      [edi+eax*4],mm1 // Write G2 B2 R1 G1
                        inc       eax

                      //---

                        paddd     mm0,mm6  // mm0 = 00 B3 00 R2
                      movq      mm6,mm5  // copy mm5 to mm6
                        psrad     mm0,14   // Scale dwords by 16384
                      pmaddwd   mm5,qword ptr cg   // mm5 = z3cg2, y3cg1+x3cg0
                        packssdw  mm0,mm7  // 00 00 B3 R2
                      pmaddwd   mm6,qword ptr cr   // mm6 = z3cr2, y3cr1+x3cr0

                        // Sum mm5 & mm6's upper and lower dwords.
                        movq      mm4,mm5
                        punpckldq mm5,mm6  // y3cr1+x3cr0, y3cg1+x3cg0
                        punpckhdq mm4,mm6  // z3cr2, z3cg2
                        paddd     mm5,mm4  // mm5 = 00 R3 00 G3
                        psrad     mm5,14   // Scale dwords by 16384
                        packssdw  mm5,mm7  // mm5 = 00 00 R3 G3
                        psllq     mm5,32   // mm5 = R3 G3 00 00

                        por       mm0,mm5  // mm0 = R3 G3 B3 R2

                        packuswb  mm0,mm7  // Pack words to bytes
                        movd      [edi+eax*4],mm0 // Write R3 G3 B3 R2
                        inc       eax
                        {$ELSE}
                        (*
                        db   $0F,$6F,$E0
                        db   $0F,$EF,$DB
                        db   $0F,$69,$E1
                        db   $0F,$61,$C1
                        db   $0F,$6F,$EA
                        db   $0F,$69,$EB
                        db   $0F,$61,$D3
                        db   $0F,$6F,$C8
                        db   $0F,$62,$C2
                        db   $0F,$6A,$CA
                        db   $0F,$6F,$D4
                        db   $0F,$6F,$DC
                        db   $0F,$62,$D5
                        db   $0F,$6A,$DD
                        db   $0F,$6F,$65,$E0
                        add  edx,4

                        db   $0F,$ED,$C4
                        db   $0F,$ED,$CC
                        db   $0F,$6F,$E8
                        db   $0F,$6F,$F0
                        db   $0F,$F5,$45,$D8
                        db   $0F,$F5,$6D,$D0
                        db   $0F,$ED,$D4
                        db   $0F,$ED,$DC
                        db   $0F,$6F,$E0
                        db   $0F,$62,$C5
                        db   $0F,$6A,$E5
                        db   $0F,$FE,$C4
                        db   $0F,$F5,$75,$C8
                        db   $0F,$72,$E0,$0E
                        db   $0F,$6F,$E9
                        db   $0F,$F5,$4D,$D8
                        db   $0F,$6B,$C7
                        db   $0F,$6F,$E6
                        db   $0F,$62,$F1
                        db   $0F,$6A,$E1
                        db   $0F,$FE,$F4
                        db   $0F,$72,$E6,$0E
                        db   $0F,$6B,$F7
                        db   $0F,$73,$F6,$20
                        db   $0F,$EB,$C6
                        db   $0F,$6F,$F5
                        db   $0F,$F5,$6D,$D0
                        db   $0F,$67,$C7
                        db   $0F,$F5,$75,$C8
                        db   $0F,$7E,$04,$87
                        inc  eax

                        db   $0F,$6F,$CD
                        db   $0F,$62,$EE
                        db   $0F,$6A,$CE
                        db   $0F,$6F,$F2
                        db   $0F,$FE,$CD
                        db   $0F,$6F,$EA
                        db   $0F,$72,$E1,$0E
                        db   $0F,$F5,$55,$D8
                        db   $0F,$6B,$CF
                        db   $0F,$F5,$6D,$D0
                        db   $0F,$6F,$E2
                        db   $0F,$62,$D5
                        db   $0F,$6A,$E5
                        db   $0F,$F5,$75,$C8
                        db   $0F,$FE,$D4
                        db   $0F,$6F,$EB
                        db   $0F,$72,$E2,$0E
                        db   $0F,$F5,$5D,$D8
                        db   $0F,$6B,$D7
                        db   $0F,$6F,$C6
                        db   $0F,$73,$F2,$20
                        db   $0F,$62,$F3
                        db   $0F,$EB,$CA
                        db   $0F,$6A,$C3
                        db   $0F,$67,$CF
                        db   $0F,$7E,$0C,$87
                        inc  eax
                   
                        db   $0F,$FE,$C6
                        db   $0F,$6F,$F5
                        db   $0F,$72,$E0,$0E
                        db   $0F,$F5,$6D,$D0
                        db   $0F,$6B,$C7
                        db   $0F,$F5,$75,$C8
                        db   $0F,$6F,$E5
                        db   $0F,$62,$EE
                        db   $0F,$6A,$E6
                        db   $0F,$FE,$EC
                        db   $0F,$72,$E5,$0E
                        db   $0F,$6B,$EF
                        db   $0F,$73,$F5,$20
                        db   $0F,$EB,$C5
                        db   $0F,$67,$C7
                        db   $0F,$7E,$04,$87
                        inc  eax
                        *)
                        {$ENDIF}

                        dec       ecx
                        jns       @LoopQWORD
                        @EndOfLine:

                        // Calc remaining byte in line.
                        push      eax
                        mov       ecx,lWidth
                        mov       eax,ecx
                        shr       eax,2
                        shl       eax,2
                        sub       ecx,eax
                        pop       eax
                        test      ecx,ecx
                        jz        @EndOfData   // Check that data count > zero bytes.

                        shl       eax,2
                        add       edi,eax

                        @LoopLast:
                        xor       eax,eax
                        mov       al,[edx]
                        shl       eax,16
                        mov       al,[esi]
                        mov       ah,[ebx]

                        {$IFNDEF DCB3_5}
                        movd      mm0,eax
                        punpcklbw mm0,mm7  // convert bytes to words

                        paddsw    mm0,qword ptr co   // Add offset
                        movq      mm5,mm0
                        movq      mm6,mm0
                        pmaddwd   mm0,qword ptr cb   // mm0 = z0cb2, y0cb1+x0cb0
                        pmaddwd   mm5,qword ptr cg   // mm5 = z0cg2, y0cg1+x0cg0
                        pmaddwd   mm6,qword ptr cr   // mm6 = z0cr2, y0cr1+x0cr0

                        movq      mm4,mm0
                        punpckldq mm0,mm5  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                        punpckhdq mm4,mm5  // mm4 = z0cg2, z0cb2
                        paddd     mm0,mm4  // mm0 = 00 G0 00 B0

                        movq      mm4,mm6
                        punpckhdq mm4,mm7  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                        paddd     mm6,mm4  // mm0 = 00 00 00 R0

                        psrad     mm0,14   // Scale dwords by 16384
                        psrad     mm6,14   // Scale dwords by 16384

                        packssdw  mm0,mm7  // Pack dwords to words,
                        packssdw  mm6,mm7  // Pack dwords to words,
                        psllq     mm6,32
                        por       mm0,mm6
                        packuswb  mm0,mm7  // Pack words to bytes

                        movd      eax,mm0
                        {$ELSE}
                        (*
                        db   $0F,$6E,$C0
                        db   $0F,$60,$C7
                        db   $0F,$ED,$45,$E0
                        db   $0F,$6F,$E8
                        db   $0F,$6F,$F0
                        db   $0F,$F5,$45,$D8
                        db   $0F,$F5,$6D,$D0
                        db   $0F,$F5,$75,$C8
                        db   $0F,$6F,$E0
                        db   $0F,$62,$C5
                        db   $0F,$6A,$E5
                        db   $0F,$FE,$C4
                        db   $0F,$6F,$E6
                        db   $0F,$6A,$E7
                        db   $0F,$FE,$F4
                        db   $0F,$72,$E0,$0E
                        db   $0F,$72,$E6,$0E
                        db   $0F,$6B,$C7
                        db   $0F,$6B,$F7
                        db   $0F,$73,$F6,$20
                        db   $0F,$EB,$C6
                        db   $0F,$67,$C7
                        db   $0F,$7E,$C0
                        *)
                        {$ENDIF}
                        mov       [edi],ax
                        add       edi,2
                        shr       eax,16
                        mov       [edi],al
                        inc       edi

                        inc       esi
                        inc       ebx
                        inc       edx
                        dec       ecx
                        jnz       @LoopLast

                        @EndOfData:

                        // Restore stack
                        pop       esi
                        pop       edi
                        pop       ebx

                        // Empty EMMS registers.
                        {$IFNDEF DCB3_5}
                        emms
                        {$ELSE}
                        db        $0F,$77  // emms - clean-up que.
                        {$ENDIF}
                      end;
                      inc(j, FYCbCrComp[0].DataLineSize);
                      i := j;
                      dec(y);
                      pImage := @PVectorB(FDibBits)^[y * FLongWidth];
                   end;
              end
              else begin
              {$ENDIF}
                   i := 0;
                   j := 0;
                   while (y > LastY)
                   do begin
                      x := 0;
                      while (x < FLongWidth - 2)
                      do begin
                         CbB := CbToB[FYCbCrComp[1].Data[i]];
                         CbG := CbToG[FYCbCrComp[1].Data[i]];
                         CrR := CrToR[FYCbCrComp[2].Data[i]];
                         CrG := CrToG[FYCbCrComp[2].Data[i]];
                         YVal := FYCbCrComp[0].Data[i];

                         pImage^[x] := ByteClip[YVal + CbB]; // Blue.
                         inc(x);
                         pImage^[x] := ByteClip[YVal - CbG - CrG]; // Green.
                         inc(x);
                         pImage^[x] := ByteClip[YVal + CrR]; // Red.
                         inc(x);
                         inc(i);
                      end;
                      inc(j, FYCbCrComp[0].DataLineSize);
                      i := j;
                      dec(y);
                      pImage := @PVectorB(FDibBits)^[y * FLongWidth];
                   end;
              {$IFNDEF DCB3_5}
              end;
              {$ENDIF}
         end;
    end;
  except
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
end; // TmcmJPEGImage.CopyToRGB.


procedure TmcmJPEGImage.CopyToGrey(McuRow : integer);
var i, y    : longint;
    pImage  : PVectorB;
    LastY   : longint;
begin
  try
    if (FYCbCrComp[0].Data <> Nil)
    then begin
         if (McuRow > -1)
         then begin
              y := FDibInfo^.bmiHeader.biHeight - McuRow * FYCbCrComp[0].McuPixVert - 1;
              LastY := y - JPEGSampleWidth * FYCbCrComp[0].MaxVertFreq;
              if (LastY < 0)
              then Lasty := -1;
         end
         else begin
              y := FDibInfo^.bmiHeader.biHeight - 1;
              LastY := -1;
         end;
         pImage := @PVectorB(FDibBits)^[y * FLongWidth];
         i := 0;
         while (y > LastY)
         do begin
            CopyMemory(pImage, @FYCbCrComp[0].Data^[i], FLongWidth);
            inc(i, FYCbCrComp[0].DataLineSize);
            dec(y);
            pImage := @PVectorB(FDibBits)^[y * FLongWidth];
         end;
    end;
  except
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
end; // TmcmJPEGImage.CopyToGrey.


procedure TmcmJPEGImage.ReadACFirstScan(Index : integer; ss, se, al : byte);
var McuRow, McuCol : integer; // MCU Row,Column.
    DoBreak        : boolean; // On true user breaked read process.
begin
  McuRow := 0;
  while (McuRow < FYCbCrComp[Index].NilRows)
  do begin
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FYCbCrComp[Index].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;

     if (FError = EC_OK)
     then begin
          for McuCol := 0 to (FYCbCrComp[Index].NilCols - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  ResetDCDifference; // Is this required ?
                  ReadRestartMarker;
                  FRestartCount := 0;
                  if (FError <> EC_OK)
                  then Break;
             end;

             // Decode progressive AC data.
             with FJFIFSOS
             do FYCbCrComp[Index].DecodeACFirstScan(McuRow, McuCol,
                                                    SpectralStart,
                                                    SpectralEnd,
                                                    FApproxLow);
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
end; // TmcmJPEGImage.ReadACFirstScan.


procedure TmcmJPEGImage.ReadACRefineScan(Index : integer; ss, se, al : byte);
var McuRow, McuCol : integer; // MCU Row,Column.
    DoBreak        : boolean; // On true user breaked read process.
begin
  McuRow := 0;
  while (McuRow < FYCbCrComp[Index].NilRows)
  do begin
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FYCbCrComp[Index].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;

     if (FError = EC_OK)
     then begin
          for McuCol := 0 to (FYCbCrComp[Index].NilCols - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  ResetDCDifference; // Is this required ?
                  ReadRestartMarker;
                  FRestartCount := 0;
                  if (FError <> EC_OK)
                  then Break;
             end;

             // Decode progressive AC data.
             with FJFIFSOS
             do FYCbCrComp[Index].DecodeACRefineScan(McuRow, McuCol,
                                                     SpectralStart,
                                                     SpectralEnd,
                                                     FApproxLow);
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
end; // TmcmJPEGImage.ReadACRefineScan.


procedure TmcmJPEGImage.ReadDCFirstScan(Index : integer; al : byte);
var c, cx, cy      : integer; // c - Component, cx and cy - Component x,y.
    McuRow, McuCol : integer; // MCU Row,Column.
    HorizFreq      : integer; // Components horizontal frequency.
    VertFreq       : integer; // Components vertical frequency.
    dCol, dRow     : integer; // Delta row and column.
    DoBreak        : boolean; // On true user breaked read process.
begin
  if FInterleavedScan
  then begin // Y, Cb, Cr data - Sequential interleaved
       McuRow := 0;
       while (McuRow < FMcuRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FMcuRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FMcuCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ResetDCDifference; // Is this required ?
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  // Process all components in the scan for MCU at [McuCol, McuRow]
                  for c := 0 to (FComponentCount - 1)
                  do begin
                     HorizFreq := FYCbCrComp[c].HorizFreq;
                     VertFreq  := FYCbCrComp[c].VertFreq;

                     for cy := 0 to (VertFreq - 1)
                     do begin
                        dRow := VertFreq * McuRow + cy;
                        for cx := 0 to (HorizFreq - 1)
                        do begin
                           dCol := HorizFreq * McuCol + cx;

                           // Decode progressive DC data.
                           FYCbCrComp[c].DecodeDCFirstScan(dRow, dCol, FApproxLow);
                        end;
                     end;
                  end;
                  inc(FRestartCount);
               end;
          end;
          inc(McuRow);
       end;
  end
  else begin
       McuRow := 0;
       while (McuRow < FYCbCrComp[Index].NilRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FYCbCrComp[Index].NilRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FYCbCrComp[Index].NilCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ResetDCDifference; // Is this required ?
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  // Decode progressive DC data.
                  FYCbCrComp[Index].DecodeDCFirstScan(McuRow, McuCol, FApproxLow);
                  inc(FRestartCount);
               end;
          end;
          inc(McuRow);
       end;
  end;
end; // TmcmJPEGImage.ReadDCFirstScan.


procedure TmcmJPEGImage.ReadDCRefineScan(Index : integer; al : byte);
var c, cx, cy      : integer; // c - Component, cx and cy - Component x,y.
    McuRow, McuCol : integer; // MCU Row,Column.
    HorizFreq      : integer; // Components horizontal frequency.
    VertFreq       : integer; // Components vertical frequency.
    dCol, dRow     : integer; // Delta row and column.
    DoBreak        : boolean; // On true user breaked read process.
begin
  if FInterleavedScan
  then begin // Y, Cb, Cr data - Sequential interleaved
       McuRow := 0;
       while (McuRow < FMcuRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FMcuRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FMcuCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ResetDCDifference; // Is this required ?
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  // Process all components in the scan for MCU at [McuCol, McuRow]
                  for c := 0 to (FComponentCount - 1)
                  do begin
                     HorizFreq := FYCbCrComp[c].HorizFreq;
                     VertFreq  := FYCbCrComp[c].VertFreq;

                     for cy := 0 to (VertFreq - 1)
                     do begin
                        dRow := VertFreq * McuRow + cy;
                        for cx := 0 to (HorizFreq - 1)
                        do begin
                           dCol := HorizFreq * McuCol + cx;

                           // Decode progressive DC data.
                           FYCbCrComp[c].DecodeDCRefineScan(dRow, dCol, FApproxLow);
                        end;
                     end;
                  end;
                  inc(FRestartCount);
               end;
          end;
          inc(McuRow);
       end;
  end
  else begin
       McuRow := 0;
       while (McuRow < FYCbCrComp[Index].NilRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FYCbCrComp[Index].NilRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FYCbCrComp[Index].NilCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ResetDCDifference; // Is this required ?
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  // Decode progressive DC data.
                  FYCbCrComp[Index].DecodeDCRefineScan(McuRow, McuCol, FApproxLow);
                  inc(FRestartCount);
               end;
          end;
          inc(McuRow);
       end;
  end;
end; // TmcmJPEGImage.ReadDCRefineScan.


procedure TmcmJPEGImage.ReadProgressiveData(Index : integer);
// Read progressive scan data.
begin
  FStream.ResetBitIndex;
  //FBufBit := 0;
  ResetDCDifference;
  FRestartCount := 0;

  // Determine if we are processing a DC scan (SpectralStart = 0, SpectralEnd = 0
  // and FApproxHigh = 0) or AC scan, and whether we are processing the first
  // scan of the spectral selection (FApproxHigh = 0) or successive scans.
  // SpectralStart range [0..63]
  // SpectralEnd range [SpectralStart..63]
  if (FJFIFSOS.SpectralStart = 0)
  then begin
       if (FJFIFSOS.SpectralEnd <> 0)
       then FError := EC_JPEGPROGACDC;

       if (FApproxHigh = 0)
       then ReadDCFirstScan(Index, FApproxLow)
       else ReadDCRefineScan(Index, FApproxLow);
  end
  else begin
       with FJFIFSOS
       do begin
          if (FApproxHigh = 0)
          then ReadACFirstScan(Index, SpectralStart, SpectralEnd, FApproxLow)
          else ReadACRefineScan(Index, SpectralStart, SpectralEnd, FApproxLow);
       end;
  end;
end; // TmcmJPEGImage.ReadProgressiveData.


procedure TmcmJPEGImage.ReadSequentialData;
// Read sequential scan data.
var c, cx, cy      : integer; // c - Component, cx and cy - Component x,y.
    McuRow, McuCol : integer; // MCU Row,Column.
    HorizFreq      : integer; // Components horizontal frequency.
    VertFreq       : integer; // Components vertical frequency.
    dCol           : integer; // Delta row and column.
    DoBreak        : boolean; // On true user breaked read process.
begin
  FStream.ResetBitIndex;
  ResetDCDifference;

  FRestartCount := 0;
  if FInterleavedScan
  then begin // Y, Cb, Cr data - Sequential interleaved
       McuRow := 0;
       while (McuRow < FMcuRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FMcuRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FMcuCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  // Process all components in the scan for MCU at [McuCol, McuRow]
                  for c := 0 to (FComponentCount - 1)
                  do begin
                     HorizFreq := FCurrentScan[c].HorizFreq;
                     VertFreq  := FCurrentScan[c].VertFreq;

                     for cy := 0 to (VertFreq - 1)
                     do begin
                        // dRow := VertFreq * McuRow + cy;
                        for cx := 0 to (HorizFreq - 1)
                        do begin
                           dCol := HorizFreq * McuCol + cx;

                           // Decode sequential data.
                           FCurrentScan[c].DecodeSequential(cy, dCol);
                        end;
                     end;
                  end;
                  inc(FRestartCount);
               end;

               // Scale image components.
               for c := 0 to (FComponentCount - 1)
               do FCurrentScan[c].Resample;

               // Convert YCbCr to BGR.
               CopyToRGB(FGlobalMcuRow + McuRow);
          end;
          inc(McuRow);
       end;
       FGlobalMcuRow := FGlobalMcuRow + McuRow;
  end
  else begin // Y data - Sequential non-interleaved
       McuRow := 0;
       while (McuRow < FCurrentScan[0].NilRows)
       do begin
          // Fire OnProgress.
          if Assigned(FOnProgress)
          then begin
               FOnProgress(Self, 100 * McuRow div FCurrentScan[0].NilRows, DoBreak);
               if DoBreak
               then FError := EC_USERCANCEL;
          end;

          if (FError = EC_OK)
          then begin
               for McuCol := 0 to (FCurrentScan[0].NilCols - 1)
               do begin
                  if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
                  then begin
                       ReadRestartMarker;
                       FRestartCount := 0;
                       if (FError <> EC_OK)
                       then Break;
                  end;

                  for c := 0 to (FScanCount - 1)
                  do begin
                     // Decode sequential data.
                     cy := McuRow * FCurrentScan[c].VertSample;
                     FCurrentScan[c].DecodeSequential(cy, McuCol);
                     inc(FRestartCount);
                  end;
               end;

               if (FComponentCount = 1)
               then begin
                    // Convert "Y" to Grey.
                    FCurrentScan[0].Resample;
                    CopyToGrey(FGlobalMcuRow + McuRow);
               end;
          end;
          inc(McuRow);
       end;
       FGlobalMcuRow := FGlobalMcuRow + McuRow;
  end;
end; // TmcmJPEGImage.ReadSequentialData.

{$ENDIF}

procedure TmcmJPEGImage.ReadTiffAndExif;
var FHeader        : TIFH;      // TIFF Image File Header.
    SaveEndian     : boolean;
    i              : integer;
    FTags          : TmcmExifImage; // TIFF & Exif tags.
    FExifInterTags : TmcmExifImage;
    FExifGPSTags   : TmcmExifImage;
    Offset         : longword;
begin
  Offset := FStream.Position;
  SaveEndian := FStream.BigEndian;
  try
    FStream.BigEndian := True;
    FHeader.Order := FStream.ReadWord;
    if FHeader.Order = TIFF_LITTLEENDIAN
    then FStream.BigEndian := False
    else FStream.BigEndian := True;
    FHeader.Ver := FStream.ReadWord;
    if ((FHeader.Order = TIFF_LITTLEENDIAN) or (FHeader.Order = TIFF_BIGENDIAN)) and
       (FHeader.Ver = 0042)
    then begin
         FHeader.IFDaddr := FStream.ReadLong;

         FStream.Position := FHeader.IFDaddr + FStreamStart + longint(Offset);
         FTags := TmcmExifImage.Create(FStream, FImageInfo, True);
         if Assigned(FTags)
         then begin
              FTags.ReadTags(Offset);
              FTags.TagsToImageInfo;
              if Assigned(FImageInfo)
              then begin
                   FImageInfo.Units := UN_METERS;
                   with FDibInfo^.bmiHeader
                   do begin
                      if (FImageInfo.XResolution > 0)
                      then biXPelsPerMeter  := Round(FImageInfo.XResolution);
                      if (FImageInfo.YResolution > 0)
                      then biYPelsPerMeter  := Round(FImageInfo.YResolution);
                   end;
              end;

              // Read Exit attibute information.
              i := FTags.Index[TAG_EXIF_IFD];
              if (i > 0)
              then FTags.ReadExifAttribute(FTags.Item[i].ValOffset, Offset);

              i := FTags.Index[TAG_GPS_IFD];
              if (i > 0)
              then begin
                   FExifGPSTags := TmcmExifImage.Create(FStream, FImageInfo, False);
                   FExifGPSTags.ReadExifGPS(FTags.Item[i].ValOffset, Offset);
                   FExifGPSTags.Free;
              end;

              i := FTags.Index[TAG_INTEROPERABILITY_IFD];
              if (i > 0)
              then begin
                   FExifInterTags := TmcmExifImage.Create(FStream, FImageInfo, False);
                   FExifInterTags.ReadExifInteroperability(FTags.Item[i].ValOffset, Offset);
                   FExifInterTags.Free;
              end;
              FTags.Free;
         end;
         FNumImages := 1;
    end;
  except
    On Exception
    do FError := EC_UNKNOWN;
  end;
  FStream.BigEndian := SaveEndian;
end; // TmcmJPEGImage.ReadTiffAndExif.


procedure TmcmJPEGImage.WriteTiffAndExif;
var FHeader        : TIFH;      // TIFF Image File Header.
    SaveEndian     : boolean;
    i              : integer;
    FTags          : TmcmExifImage; // TIFF & Exif tags.
    FExifTags      : TmcmExifImage;
    FExifGPSTags   : TmcmExifImage;
    Offset         : longword;
    Identifier     : array[0..5] of AnsiChar;
begin
  try
    FTags := TmcmExifImage.Create(FStream, FImageInfo, True);
    if Assigned(FTags)
    then begin
         FHeader.order   := TIFF_LITTLEENDIAN;
         FHeader.ver     := 42;
         FHeader.IFDaddr := SizeOf(TIFH);

         FTags.SetTagInfo(TAG_COMPRESSION, TAGF_SHORT, 0, TIFCP_JPEG, [0,0], '');
         FTags.SetTagInfo(TAG_RESOLUTIONUNIT, TAGF_SHORT, 0, 2, [0,0], '');
         with FDibInfo^.bmiHeader
         do begin
            FTags.SetTagInfo(TAG_XRESOLUTION, TAGF_RATIONAL, 0, 0, [round(254 * biXPelsPerMeter), 10000], '');
            FTags.SetTagInfo(TAG_YRESOLUTION, TAGF_RATIONAL, 0, 0, [round(254 * biYPelsPerMeter), 10000], '');
         end;

         FTags.ImageInfoToTags(0);

         // Exif
         if Assigned(FImageInfo.Exif)
         then begin
              FExifTags := TmcmExifImage.Create(FStream, FImageInfo, False);
              if Assigned(FExifTags)
              then begin
                   FExifTags.SetExifAttribute(Not(FCompress in [CP_JPEG_STD, CP_JPEG_PROG]));
                   FTags.SetTagInfo(TAG_EXIF_IFD, TAGF_LONG, 1, FExifTags.EC, [0,0], '');
              end
              else FError := EC_NOMEMORY;
         end
         else FExifTags := Nil;

         // GPS
         if Assigned(FImageInfo.GPS)
         then begin
              FExifGPSTags := TmcmExifImage.Create(FStream, FImageInfo, False);
              if Assigned(FExifGPSTags)
              then begin
                   FExifGPSTags.SetExifGPS;
                   FTags.SetTagInfo(TAG_GPS_IFD, TAGF_LONG, 1, FExifGPSTags.EC, [0,0], '');
              end
              else FError := EC_NOMEMORY;
         end
         else FExifGPSTags := Nil;

         // Calculate Offset to Tag's value field.
         Offset := FHeader.IFDaddr + SizeOf(FTags.NextIFD) + (FTags.EC * 12) + SizeOf(FTags.EC) + 0; //FStreamStart;

         if Assigned(FExifTags)
         then begin
              i := FTags.Index[TAG_EXIF_IFD];
              if (i > 0)
              then FTags.Item[i].ValOffset := Offset;
              Offset := Offset + SizeOf(FExifTags.NextIFD) + (FExifTags.EC * 12) + SizeOf(FExifTags.EC);
         end;

         if Assigned(FExifGPSTags)
         then begin
              i := FTags.Index[TAG_GPS_IFD];
              if (i > 0)
              then FTags.Item[i].ValOffset := Offset;
              Offset := Offset + SizeOf(FExifGPSTags.NextIFD) + (FExifGPSTags.EC * 12) + SizeOf(FExifGPSTags.EC);
         end;

         // Calculate TAG data offset's.
         FTags.WriteCalcOffsets(Offset);
         if Assigned(FExifTags)
         then FExifTags.WriteCalcOffsets(Offset);
         if Assigned(FExifGPSTags)
         then FExifGPSTags.WriteCalcOffsets(Offset);

         // JPEG Marker JPG_APP1.
         FStream.WriteWord(JPG_APP1);
         FStream.WriteWord(Offset + 2 + 6);  // Write marker size.
         Identifier := 'Exif';
         Identifier[4] := #0;
         Identifier[5] := #0;
         FStream.Write(Identifier, 6);

         // Write TIFF/Exif/GPS TAG's
         SaveEndian := FStream.BigEndian;
         FStream.BigEndian := True;

         // Write TIFF header.
         FStream.Write(FHeader.order,   SizeOf(FHeader.order));
         FStream.Write(FHeader.ver,     SizeOf(FHeader.ver));
         FStream.Write(FHeader.IFDaddr, SizeOf(FHeader.IFDaddr));

         // Write TAG's
         FTags.WriteTags;
         if Assigned(FExifTags)
         then FExifTags.WriteTags;
         if Assigned(FExifGPSTags)
         then FExifGPSTags.WriteTags;

         // Write TAG data.
         FTags.WriteTagsData;
         if Assigned(FExifTags)
         then FExifTags.WriteTagsData;
         if Assigned(FExifGPSTags)
         then FExifGPSTags.WriteTagsData;

         FStream.BigEndian := SaveEndian;
         FTags.Free;
         FExifTags.Free;
         FExifGPSTags.Free;
    end;
  except
    On Exception
    do FError := EC_UNKNOWN;
  end;
end; // TmcmJPEGImage.WriteTiffAndExif.


procedure TmcmJPEGImage.ReadInitialize;
begin
  FCompress := CP_JPEG_STD;
  FDidSOF  := False;
  FDidAPP0 := False;
  FScanIndex := 0;
  FGlobalMcuRow := 0;
end; // TmcmJPEGImage.ReadInitialize.


function TmcmJPEGImage.ReadMarkers(IsTIFF : boolean) : TmcmErrorCode;
var i             : integer;
    Code          : word;
    Count         : word;
    DescStr       : string;
    ByteValue     : byte;
    TableID       : byte;
    TableClass    : byte;
    SymbolCount   : integer;
    SavePos       : longint;

    Precision     : integer;
    HuffValues    : array[0..255] of byte;
    ScanComponent : TmcmJPEGComponent;
    Identifier    : array[0..5] of AnsiChar;
begin
  if Not(IsTIFF)
  then FGlobalMcuRow := 0;

  // Read Marker Type
  Code := FStream.ReadWord;
  if (Code <> JPG_SOI) // Start of image.
  then FError := EC_BADFORMAT;

  // Read next Marker Type.
  Code := FStream.ReadWord;

  while (Code <> JPG_EOI) and (FError = EC_OK)
  do begin
     case Code of
     JPG_TEM   : ; //
     JPG_RST0..
     JPG_RST7  : // Restart Table - Has no data and may only occur
                 // within compressed data.
                 FError := EC_BADFORMAT;
     JPG_EOI   : ; // End Of Image - never reached here though!
                //
     JPG_SOF0,
     JPG_SOF1,
     JPG_SOF2  : begin // Start Of Frame.
                   if (Code = JPG_SOF2)
                   then begin
                        FInterlaced := False;
                        FCompress := CP_JPEG_PROG;
                   end
                   else begin
                        FInterlaced := True;
                        FCompress := CP_JPEG_STD;
                   end;

                   // Note: There can be only one SOFn marker per
                   // JPEG file, and it must preceed SOS.
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   FDidSOF  := True;
                   FStream.Read(FJFIFSOF, SizeOf(TJFIFSOF_Minor));
                   FJFIFSOF.ImageHeight := SwapWord(FJFIFSOF.ImageHeight);
                   FJFIFSOF.ImageWidth := SwapWord(FJFIFSOF.ImageWidth);

                   FMaxHorizFreq := 0;
                   FMaxVertFreq  := 0;
                   if (FJFIFSOF.NoComponents in [1,3])
                   then begin
                        i := 0;
                        FComponentCount := Minimum(FJFIFSOF.NoComponents, 3);
                        while (i < FComponentCount) and
                              (FError = EC_OK)
                        do begin
                           FStream.Read(FJFIFSOF.Components[i], SizeOf(TJFIFSOFComponent));
                           {$IFDEF STRICT_JFIF}
                             // JFIF imposes restrictions on the
                             // component ID's (Y = 1, Cb = 2, Cr = 3)
                             // however JPEG does not.
                             if (FJFIFSOF.Components[i].Identifier <> i + 1)
                             then FError := EC_JFIFBADCOMPID;
                           {$ENDIF}

                           // Get scan component Y, Cb or Cr by Index (TableClass).
                           ScanComponent := GetScanComponent(FJFIFSOF.Components[i].Identifier);
                           ScanComponent.HorizFreq := FJFIFSOF.Components[i].Samples shr 4;
                           ScanComponent.VertFreq  := FJFIFSOF.Components[i].Samples and $0F;

                           // For grey-scale images, make sure to set HorizFreq and VertFreq to "1"
                           // if they differ in value.
                           if (FComponentCount = 1)
                           then begin
                                if (ScanComponent.HorizFreq = ScanComponent.VertFreq)
                                then begin
                                     ScanComponent.HorizFreq := 1;
                                     ScanComponent.VertFreq  := 1;
                                end;
                           end;

                           inc(i);
                        end;

                        if (FError = EC_OK)
                        then begin
                             CalcMCUDimension;

                             if Not(IsTIFF)
                             then begin
                                  with FDibInfo^.bmiHeader
                                  do begin
                                     biWidth  := FJFIFSOF.ImageWidth;
                                     biHeight := FJFIFSOF.ImageHeight;
                                     biPlanes := 1;
                                     if (FJFIFSOF.NoComponents = 1)
                                     then begin
                                          biBitCount := FJFIFSOF.Bits;
                                          CreateGreyPalette(PM_GREY);
                                     end
                                     else biBitCount := 3 * FJFIFSOF.Bits;
                                  end;
                             end;

                             // Only 8 bit JFIF images are supported.
                             if (FJFIFSOF.Bits <> 8)
                             then FError := EC_BITDEPTH;

                             if (FError = EC_OK) and Not(IsTIFF)
                             then CreateDIB;
                             if (FError = EC_OK) and
                                (FDibInfo^.bmiHeader.biBitCount = 24)
                             then InitYCbCrTables;
                        end;
                   end
                   else FError := EC_JFIFBADCOMPCOUNT;
                   FStream.Position := Count + SavePos;
                 end;
     //JPG_SOF2  : FError := EC_NOPROGRESSIVE;
     JPG_SOF3  : FError := EC_NOLOSSLESSHUFFMAN;
     JPG_SOF5  : FError := EC_NODIFFSEQHUFFMAN;
     JPG_SOF6  : FError := EC_NODIFFPRGHUFFNAM;
     JPG_SOF7  : FError := EC_NODIFFLOSSHUFFMAN;
     JPG_SOF9..
     JPG_SOF15 : FError := EC_PATENTS;
     JPG_DHT   : begin // Define Huffman Table
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   while (FStream.Position < Count + SavePos) and (FError = EC_OK)
                   do begin
                      FStream.Read(ByteValue, 1);
                      TableID    := ByteValue and $0F; // Huffman table ID.
                      TableClass := ByteValue shr 4;   //  0 -> DC, else AC.
                      if (TableClass <> 0)
                      then TableClass := 1;
                      FJFIFDHT[TableClass,TableID].ClassIdentifier := TableID;

                      if (TableID < JPEGMaxHuffmanTables)
                      then begin
                           FStream.Read(FJFIFDHT[TableClass,TableID].CodeLength, 16);

                           // Count number of codes following the CodeLength table.
                           SymbolCount := 0;
                           for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
                           do inc(SymbolCount, FJFIFDHT[TableClass,TableID].CodeLength[i]);

                           // Check if code count is OK.
                           if (SymbolCount > JPEGMaxHuffmanCodes)
                           then FError := EC_MAXHUFFMANCODES
                           else begin
                                FStream.Read(HuffValues, SymbolCount);

                                // Create JPEG Huffman table.
                                if (FHuffman[TableClass,TableID] = Nil)
                                then begin
                                     FHuffman[TableClass,TableID] := TmcmHuffmanJPEG.Create(JPEGMaxHuffmanCodes, JPEGMaxHuffmanCodeLength);
                                     FHuffman[TableClass,TableID].GetNBits := FStream.ReadJPEGBits;
                                end;
                                for i := 0 to (SymbolCount - 1)
                                do FHuffman[TableClass,TableID].HuffValue[i] := HuffValues[i];
                                FError := FHuffman[TableClass,TableID].CreateReadTable(SymbolCount, FJFIFDHT[TableClass,TableID].CodeLength);
                           end;
                      end
                      else FError := EC_MAXHUFFMANTABLES;
                   end;
                   FStream.Position := Count + SavePos;
                 end;
     JPG_SOS   : begin // Start Of Scan
                   Count := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.
                   if FDidSOF
                   then begin
                        FStream.Read(FJFIFSOS.Count, SizeOf(FJFIFSOS.Count));

                        FScanCount := Minimum(FJFIFSOS.Count, 3);
                        FInterleavedScan := FScanCount = 3;

                        for i := 0 to (FScanCount - 1)
                        do FStream.Read(FJFIFSOS.Component[i], SizeOf(TJFIFSOSComponent));

                        // Used with progressive scans.
                        FStream.Read(FJFIFSOS.SpectralStart, SizeOf(FJFIFSOS.SpectralStart));
                        FStream.Read(FJFIFSOS.SpectralEnd,   SizeOf(FJFIFSOS.SpectralEnd));
                        FStream.Read(FJFIFSOS.Approximation, SizeOf(FJFIFSOS.Approximation));
                        FApproxHigh := FJFIFSOS.Approximation shr 4;
                        FApproxLow  := FJFIFSOS.Approximation and $0F;

                        for i := 0 to (FScanCount - 1)
                        do begin
                           // Get scan component Y, Cb or Cr by Index (TableClass).
                           ScanComponent := GetScanComponent(FJFIFSOS.Component[i].Identifier);
                           FScanIndex := FJFIFSOS.Component[i].Identifier - 1;

                           ScanComponent.SpectralStart := FJFIFSOS.SpectralStart;
                           ScanComponent.SpectralEnd   := FJFIFSOS.SpectralEnd;
                           ScanComponent.ApproxLow     := FApproxLow;
                           ScanComponent.ApproxHigh    := FApproxHigh;

                           // Associate Quantification table.
                           if (FJFIFSOF.Components[FYCbCrIndex].DQTIdentifier < JPEGMaxQuantizationTables)
                           then ScanComponent.SetQuant2IDCT(FJFIFDQT[FJFIFSOF.Components[FYCbCrIndex].DQTIdentifier])
                           else FError := EC_JFIFBADCOMPID;

                           // Assign DC table
                           TableID := FJFIFSOS.Component[i].DCACHuffman shr 4;
                           ScanComponent.HuffmanTable[0] := FHuffman[0,TableID];

                           // Assign AC table
                           TableID := FJFIFSOS.Component[i].DCACHuffman and $0F;
                           ScanComponent.HuffmanTable[1] := FHuffman[1,TableID];

                           // Allocate component buffers.
                           ScanComponent.AllocateBuffer(FScanCount <> FComponentCount, Not(FInterlaced));

                           FCurrentScan[i] := ScanComponent;
                           inc(FScanIndex);
                        end;

                        FStream.Position := Count + SavePos;
                        if (FError = EC_OK)
                        then begin
                             // Read sequential scan data.
                             try
                               if FInterlaced
                               then ReadSequentialData
                               else ReadProgressiveData(FYCbCrIndex);
                             except
                               On EReadError
                               do FError := EC_READFROMFILE;
                               On E:Exception
                               do FError := EC_UNKNOWN;
                             end;
                        end;
                   end
                   else FError := EC_JPEGMISSOF;
                 end;
     JPG_DQT   : begin // Define quantization tables.
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   while (FStream.Position < Count + SavePos) and (FError = EC_OK)
                   do begin
                      FStream.Read(ByteValue, SizeOf(byte));
                      TableID := (ByteValue and $0F);
                      Precision := ByteValue shr 4;
                      FJFIFDQT[TableID].Identifier := TableID;
                      if (Precision = 0)
                      then begin
                           for i := 0 to 63
                           do FStream.Read(FJFIFDQT[TableID].Values[i], SizeOf(byte));
                      end
                      else FStream.Read(FJFIFDQT[TableID].Values, 64 * SizeOf(word));
                   end;
                   FStream.Position := Count + SavePos;
                 end;
     JPG_DRI   : begin // Define Restart Interval.
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.
                   FRestartInterval := FStream.ReadWord;
                   FStream.Position := Count + SavePos;
                 end;
     JPG_DHP   : FError := EC_JPEGDHPCODE;
     JPG_APP0  : begin
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   FStream.Read(FJFIFAPP0Header.Identifier, 5);
                   if (FJFIFAPP0Header.Identifier = 'JFIF') and Not(FDidAPP0)
                   then begin // APP0 JFIF Header.
                        FDidAPP0 := True;
                        FStream.Position := SavePos;

                        FStream.Read(FJFIFAPP0Header, SizeOf(TJFIFAPP0Header));
                        if Assigned(FImageInfo)
                        then begin
                             FImageInfo.SoftwareVersion := FJFIFAPP0Header.MajorID +
                                                           FJFIFAPP0Header.MinorID / 10.0;
                             case FJFIFAPP0Header.Units of
                             0 : begin // Aspect ratio
                                   FImageInfo.Units := UN_PIXELS;
                                   FImageInfo.PixelWidth  := SwapWord(FJFIFAPP0Header.XDensity);
                                   FImageInfo.PixelHeight := SwapWord(FJFIFAPP0Header.YDensity);
                                 end;
                             1 : begin // Inch - resolution
                                   FImageInfo.Units := UN_INCHS;
                                   FImageInfo.XResolution := SwapWord(FJFIFAPP0Header.XDensity);
                                   FImageInfo.YResolution := SwapWord(FJFIFAPP0Header.YDensity);
                                 end;
                             2 : begin // Centimeter - resolution
                                   FImageInfo.Units := UN_CENTIMETERS;
                                   FImageInfo.XResolution := SwapWord(FJFIFAPP0Header.XDensity);
                                   FImageInfo.YResolution := SwapWord(FJFIFAPP0Header.YDensity);
                                 end;
                             end;
                        end;

                        // Skip thumbnail.
                        if (FJFIFAPP0Header.XThumbnail <> 0) and
                           (FJFIFAPP0Header.YThumbnail <> 0)
                        then ;// Skipped by settig FStream.Position := Count + SavePos.
                             { FStream.Position := FStream.Position +
                                                FJFIFAPP0Header.XThumbnail *
                                                FJFIFAPP0Header.YThumbnail * 3; }

                   end;

                   if (FJFIFAPP0Header.Identifier = 'JFXX')
                   then begin // APP0 JFIF Extension Header.
                              // Thumbnail!
                        // Skip thumbnail.
                        FStream.Position := SavePos;
                   end;

                   FStream.Position := Count + SavePos;
                 end;
     JPG_APP1  : begin // Check for Exif attribute information.
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.
                   if (Count > 5)
                   then begin
                        FStream.Read(Identifier, 6);
                        // Read Exif information.
                        if (Identifier = 'Exif') and Assigned(FImageInfo)
                        then ReadTiffAndExif;
                   end;
                   FStream.Position := Count + SavePos;
                 end;
     JPG_APP2  : begin // Check for FlashPix extension data.
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   if (Count > 5)
                   then begin
                        FStream.Read(Identifier, 6);
                        //FStream.Position := FStream.Position + 1;
                        // Read Exif information.
                        if (Identifier = 'FPXR') and Assigned(FImageInfo)
                        then begin
                        end;
                   end;
                   FStream.Position := Count + SavePos;
                 end;
     JPG_APP3..
     JPG_APP15 : begin // Application specific data
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   FStream.Position := Count + SavePos;
                 end;
     JPG_COM   : begin // Comment
                   Count   := FStream.ReadWord - SizeOf(Count);
                   SavePos := FStream.Position; // Save stream position.

                   SetLength(DescStr, Count);
                   FStream.Read(DescStr[1], Count);
                   SetLength(DescStr, Count-1);
                   if Assigned(FImageInfo)
                   then FImageInfo.Description := DescStr;

                   FStream.Position := Count + SavePos;
                 end;
     $FFFF     : FStream.Position := FStream.Position - 1; // Fill in!
     else begin
          // Unknown marker - skip it!
          Count := FStream.ReadWord - SizeOf(Count);
          FStream.Position := FStream.Position + Count;
          if (FStream.Position >= FStream.Size)
          then FError := EC_BADFORMAT;
     end;
     end; // End case

     if (FError = EC_OK)
     then begin
          // Read next Marker Type.
          Code := FStream.ReadWord;
          while (Code = 0) and (FStream.Position < FStream.Size)
          do begin
             FStream.Position := FStream.Position;
             Code := FStream.ReadWord;
          end;
          if ((Code and $FF00) <> $FF00)
          then FError := EC_BADFORMAT;
     end;
  end; // End while
  
  Result := FError;
end; // TmcmJPEGImage.ReadMarkers.


procedure TmcmJPEGImage.CombineComponents;
var i : integer;
begin
  if Not(FInterlaced)
  then begin
       for i := 0 to (FComponentCount - 1)
       do FYCbCrComp[i].ProgressiveIDCT;
  end;

  if (FComponentCount = 3)
  then begin
       if (Not(FInterleavedScan) or Not(FInterlaced))
       then begin
            // Scale image components.
            for i := 0 to (FComponentCount - 1)
            do FYCbCrComp[i].Resample;

            // Convert YCbCr to BGR.
            CopyToRGB(-1);
       end;
  end
  else if (FComponentCount = 1) and (FCompress = CP_JPEG_PROG)
       then CopyToGrey(-1);
end; // TmcmJPEGImage.CombineComponents.


procedure TmcmJPEGImage.LoadFromStream(Stream : TStream);
{$IFDEF mcmJPEGEnginee}
//var
{$ELSE}
var dibDC         : HDC;
    BordBitmap    : Graphics.TBitmap;
    JPEGImage     : TJPEGImage;
    NoColors      : integer;
{$ENDIF}
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              {$IFDEF mcmJPEGEnginee}
              ReadInitialize;
              FStream.Position := FStreamStart;
              ReadMarkers(False);

              CombineComponents;

              // Set resolution information.
              with FDibInfo^.bmiHeader
              do begin
                 if Assigned(FImageInfo)
                 then begin
                      FImageInfo.Units := UN_METERS;
                      biXPelsPerMeter := Round(FImageInfo.XResolution);
                      biYPelsPerMeter := Round(FImageInfo.YResolution);
                 end;
              end;

              FStream.ResetBitIndex;
              FStream.FilePosAlign;

              {$ELSE} // Use borlands JPEG component.

              FStream.Position := 0;

              BordBitmap   := Nil;
              JPEGImage := TJPEGImage.Create;
              try
                JPEGImage.LoadFromStream(Stream);
                JPEGImage.DIBNeeded;
              except
                On E:Exception
                do ShowMessage(E.Message);
              end;

              if (JPEGImage.Width > 0) and (JPEGImage.Height > 0)
              then begin
                   BordBitmap := TBitmap.Create;
                   BordBitmap.Width  := JPEGImage.Width;
                   BordBitmap.Height := JPEGImage.Height;

                   if JPEGImage.ProgressiveEncoding
                   then begin
                        FInterlaced := False;
                        FCompress := CP_JPEG_PROG;
                   end
                   else begin
                        FInterlaced := True;
                        FCompress := CP_JPEG_STD;
                   end;

                   case JPEGImage.PixelFormat of
                   jf8Bit  : BordBitmap.PixelFormat := pf8bit;
                   jf24Bit : BordBitmap.PixelFormat := pf24bit;
                   end;
                   BordBitmap.Canvas.Draw(0, 0, JPEGImage);
              end;
              JPEGImage.Free;

              if Assigned(BordBitmap)
              then begin
                try
                  try
                    if (BordBitmap.HandleType <> bmDIB)
                    then BordBitmap.HandleType := bmDIB;

                    case BordBitmap.PixelFormat of
                    pf1bit  : FDibInfo^.bmiHeader.biBitCount := 1;
                    pf4bit  : FDibInfo^.bmiHeader.biBitCount := 4;
                    pf8bit  : FDibInfo^.bmiHeader.biBitCount := 8;
                    pf15bit : FDibInfo^.bmiHeader.biBitCount := 15;
                    pf16bit : FDibInfo^.bmiHeader.biBitCount := 16;
                    pf24bit : FDibInfo^.bmiHeader.biBitCount := 24;
                    pf32bit : FDibInfo^.bmiHeader.biBitCount := 32;
                    end;

                    with FDibInfo^.bmiHeader
                    do begin
                       biWidth         := BordBitmap.Width;
                       biHeight        := BordBitmap.Height;
                       biPlanes        := 1;
                       //biCompression   := BI_RGB;
                       biXPelsPerMeter := round(30000.0 / 2.54);
                       biYPelsPerMeter := round(30000.0 / 2.54);
                       biClrUsed       := 0;
                       biClrImportant  := 0;
                    end;
                    //LongWidth := (((FDibInfo^.bmiHeader.biWidth * FDibInfo^.bmiHeader.biBitCount) + 31) div 32) * 4;
                    //FDibInfo^.bmiHeader.biSizeImage := LongWidth * Abs(FDibInfo^.bmiHeader.biHeight);

                    with FDibInfo^.bmiHeader
                    do begin
                       if Assigned(FImageInfo)
                       then begin
                            FImageInfo.Units := UN_METERS;
                            FImageInfo.XResolution := biXPelsPerMeter;
                            FImageInfo.YResolution := biYPelsPerMeter;
                            FImageInfo.XPosition   := 0;
                            FImageInfo.YPosition   := 0;
                       end;
                    end;

                    if (FDibInfo^.bmiHeader.biBitCount <= 8)
                    then begin
                         try
                           NoColors := GetNumColors(FDibInfo);
                           if (BordBitmap.Palette <> 0)
                           then begin
                                GetPaletteEntries(BordBitmap.Palette, 0, NoColors, FDibInfo^.bmiColors);
                           end;
                         except
                         end;
                    end;

                    if CreateDIB
                    then begin
                         // Get bitmap pixel data.
                         dibDC := GetDC(0);
                         try
                           GetDIBits(dibDC, BordBitmap.Handle, 0, FDibInfo^.bmiHeader.biHeight, FDibBits, FDibInfo^, DIB_RGB_COLORS);
                         finally
                           ReleaseDC(0, dibDC);
                         end;
                    end;
                  except
                  // On Ejpeg do ;
                  end;
                finally
                  BordBitmap.Free;
                end;
              end;
              {$ENDIF}
         end;
       except
         On E:EReadError
         do FError := EC_ENDOFFILE;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmJPEGImage.LoadFromStream.


{$IFDEF mcmJPEGEnginee}


procedure TmcmJPEGImage.CreateQuantificationTables;
var i     : integer;
    Scale : double;
    Value : longword;
begin
  if (FQuality <= 0)
  then Scale := 1
  else if (FQuality >= 100)
       then Scale := 0.0001
       else Scale := 10.0 - 4.958675 * Log(FQuality);
       // else Scale := 10.10017525 - 0.10017525 * FQuality;

  // Luminance
  {$IFDEF IMGDEBUG}
    OutputDebugString('FJFIFDQT 0');
  {$ENDIF}
  FJFIFDQT[0].Identifier := 0;
  for i := 0 to (JPEGSampleSize - 1)
  do begin
     Value := Round(JPEGQuantLuminance[JPEGZigZagInputOrder[i]] * Scale);
     if (Value < JPEGMinQuantizationValue)
     then FJFIFDQT[0].Values[i] := JPEGMinQuantizationValue
     else if (Value > JPEGMax8BitQuantizationValue)
          then FJFIFDQT[0].Values[i] := JPEGMax8BitQuantizationValue
          else FJFIFDQT[0].Values[i] := Value;
     {$IFDEF IMGDEBUG}
       OutputDebugString(PChar(IntToStr(FJFIFDQT[0].Values[i]) + ', '));
     {$ENDIF}
  end;

  if (FComponentCount > 1)
  then begin
       // Chrominance
       {$IFDEF IMGDEBUG}
         OutputDebugString('FJFIFDQT 1');
       {$ENDIF}
       FJFIFDQT[1].Identifier := 1;
       for i := 0 to (JPEGSampleSize - 1)
       do begin
          Value := Round(JPEGQuantChrominance[JPEGZigZagInputOrder[i]] * Scale);
          if (Value < JPEGMinQuantizationValue)
          then FJFIFDQT[1].Values[i] := JPEGMinQuantizationValue
          else if (Value > JPEGMax8BitQuantizationValue)
               then FJFIFDQT[1].Values[i] := JPEGMax8BitQuantizationValue
               else FJFIFDQT[1].Values[i] := Value;
          {$IFDEF IMGDEBUG}
            OutputDebugString(PChar(IntToStr(FJFIFDQT[0].Values[i]) + ', '));
          {$ENDIF}
       end;
  end;
end; // TmcmJPEGImage.CreateQuantificationTables.


function TmcmJPEGImage.CreateHuffmanTables(TableID : integer; DC, AC : boolean) : integer;
var Count         : integer;
    ACSymbolCount : integer;
    DCSymbolCount : integer;
begin
  Count := 0;
  if DC
  then begin
       inc(Count, SizeOf(TJFIFDHT));
       FHuffman[0,TableID].CreateWriteTable(JPEGMaxHuffmanCodeLength); // DC

       DCSymbolCount := FHuffman[0,TableID].SymbolCount;
       inc(Count, DCSymbolCount);
  end;
  if AC
  then begin
       inc(Count, SizeOf(TJFIFDHT));
       FHuffman[1,TableID].CreateWriteTable(JPEGMaxHuffmanCodeLength); // AC

       ACSymbolCount := FHuffman[1,TableID].SymbolCount;
       inc(Count, ACSymbolCount);
  end;
  Result := Count;
end; // TmcmJPEGImage.CreateHuffmanTables.


procedure TmcmJPEGImage.WriteHuffmanTables(TableID : integer; DC, AC : boolean);
var i, Count      : integer;
    ACSymbolCount : integer;
    DCSymbolCount : integer;
    HuffValues    : array[0..255] of byte;
begin
  Count := 0;
  DCSymbolCount := 0;
  ACSymbolCount := 0;

  if DC
  then begin
       inc(Count, SizeOf(TJFIFDHT));
       //FHuffman[0,TableID].CreateWriteTable(JPEGMaxHuffmanCodeLength); // DC

       DCSymbolCount := FHuffman[0,TableID].SymbolCount;
       inc(Count, DCSymbolCount);
  end;
  if AC
  then begin
       inc(Count, SizeOf(TJFIFDHT));
       //FHuffman[1,TableID].CreateWriteTable(JPEGMaxHuffmanCodeLength); // AC

       ACSymbolCount := FHuffman[1,TableID].SymbolCount;
       inc(Count, ACSymbolCount);
  end;

  if (Count > 0)
  then begin
       FStream.WriteWord(JPG_DHT);
       FStream.WriteWord(Count + 2);

       if DC
       then begin
            // Write class identifier/code length's.
            FJFIFDHT[0,TableID].ClassIdentifier := (TableID and $0F);
            for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
            do FJFIFDHT[0,TableID].CodeLength[i] := FHuffman[0,TableID].HuffmanBits[i];
            FStream.Write(FJFIFDHT[0,TableID], SizeOf(TJFIFDHT));

            // Write Huffman values.
            for i := 0 to (DCSymbolCount - 1)
            do HuffValues[i] := FHuffman[0,TableID].HuffValue[i];
            FStream.Write(HuffValues, DCSymbolCount);
       end;

       if AC
       then begin
            FJFIFDHT[1,TableID].ClassIdentifier := (TableID and $0F) or $10;
            for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
            do FJFIFDHT[1,TableID].CodeLength[i] := FHuffman[1,TableID].HuffmanBits[i];
            FStream.Write(FJFIFDHT[1,TableID], SizeOf(TJFIFDHT));

            // Write Huffman values.
            for i := 0 to (ACSymbolCount - 1)
            do HuffValues[i] := FHuffman[1,TableID].HuffValue[i];
            FStream.Write(HuffValues, ACSymbolCount);
       end;
  end;
end; // TmcmJPEGImage.WriteHuffmanTables.


procedure TmcmJPEGImage.WriteQuantification;
var i, j      : integer;
    Precision : byte;
begin
  for i := 0 to Minimum(FComponentCount - 1, 1)
  do begin
     FStream.WriteWord(JPG_DQT);
     Precision := 0;
     j := JPEGSampleSize * (Precision + 1) + 1;
     FStream.WriteWord(j + 2);

     FJFIFDQT[i].Identifier := FJFIFDQT[i].Identifier or (Precision shl 4);
     FStream.write(FJFIFDQT[i].Identifier, 1);
     if (Precision = 0)
     then begin
          for j := 0 to (JPEGSampleSize - 1)
          do FStream.write(FJFIFDQT[i].Values[j], 1);
     end
     else FStream.write(FJFIFDQT[i].Values, JPEGSampleSize * 1);
  end;
end; // TmcmJPEGImage.WriteQuantification.


procedure TmcmJPEGImage.WriteInterleaved(InWriteMode : boolean);
// Write sequential scan data.
var c, cx, cy      : integer; // c - Component, cx and cy - Component x,y.
    McuRow, McuCol : integer; // MCU Row,Column.
    HorizFreq      : integer; // Components horizontal frequency.
    VertFreq       : integer; // Components vertical frequency.
    dCol, dRow     : integer; // Delta row and column.
    // DoBreak        : boolean; // On true user breaked read process.
begin
  ResetDCDifference;
  FRestartCount  := 0;
  FRestartMarker := 0;

  McuRow := 0;
  while (McuRow < FMcuRows)
  do begin
     {
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FCurrentScan[0].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;
     }
     if (FError = EC_OK)
     then begin
          for McuCol := 0 to (FMcuCols - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  if InWriteMode
                  then WriteRestartMarker;
                  ResetDCDifference;
                  FRestartCount := 0;
             end;

             for c := 0 to (FScanCount - 1)
             do begin
                HorizFreq := FCurrentScan[c].HorizFreq;
                VertFreq  := FCurrentScan[c].VertFreq;

                for cy := 0 to (VertFreq - 1)
                do begin
                   dRow := VertFreq * (McuRow + FGlobalMcuRow) + cy;
                   for cx := 0 to (HorizFreq - 1)
                   do begin
                      dCol := HorizFreq * McuCol + cx;

                      // Decode sequential data.
                      FCurrentScan[c].Encoder(dRow, dCol);
                   end;
                end;
             end;
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
  FGlobalMcuRow := FGlobalMcuRow + McuRow;
end; // TmcmJPEGImage.WriteInterleaved.


procedure TmcmJPEGImage.WriteNonInterleaved(InWriteMode : boolean);
var McuRow, McuCol : integer; // MCU Row,Column.
    // DoBreak        : boolean; // On true user breaked read process.
begin
  ResetDCDifference;
  FRestartCount  := 0;
  FRestartMarker := 0;
  McuRow := 0;
  while (McuRow < FMcuRows) //  FMcuRows is equal to FCurrentScan[0].NilRows
  do begin
     {
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FCurrentScan[0].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;
     }
     if (FError = EC_OK)
     then begin
          for McuCol := 0 to (FCurrentScan[0].NilCols {DataCols} - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  FCurrentScan[0].EncodeEobRun;
                  if InWriteMode
                  then WriteRestartMarker;
                  ResetDCDifference;
                  FRestartCount := 0;
             end;

             // Decode sequential data.
             FCurrentScan[0].Encoder(FGlobalMcuRow + McuRow, McuCol);
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
  FGlobalMcuRow := FGlobalMcuRow + McuRow;
end; // TmcmJPEGImage.WriteNonInterleaved


procedure TmcmJPEGImage.ProcessACFirstScan(InWriteMode : boolean);
var McuRow, McuCol : integer; // MCU Row,Column.
    // DoBreak        : boolean; // On true user breaked read process.
begin
  FCurrentScan[0].ResetEobRun;
  FRestartCount  := 0;
  FRestartMarker := 0;
  McuRow := 0;
  //while (McuRow < FCurrentScan[0].DataRows)
  while (McuRow < FCurrentScan[0].NilRows)
  do begin
     {
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FCurrentScan[0].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;
     }
     if (FError = EC_OK)
     then begin
          //for McuCol := 0 to (FCurrentScan[0].DataCols  - 1)
          for McuCol := 0 to (FCurrentScan[0].NilCols - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  FCurrentScan[0].EncodeEobRun;
                  if InWriteMode
                  then WriteRestartMarker;
                  FRestartCount := 0;
             end;

             // Decode sequential data.
             FCurrentScan[0].EncodeACFirstScan(McuRow, McuCol);
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
  FCurrentScan[0].EncodeEobRun;
end; // TmcmJPEGImage.ProcessACFirstScan.


procedure TmcmJPEGImage.ProcessACRefineScan(InWriteMode : boolean);
var McuRow, McuCol : integer; // MCU Row,Column.
    // DoBreak        : boolean; // On true user breaked read process.
begin
  FCurrentScan[0].ResetEobRun;
  FRestartCount  := 0;
  FRestartMarker := 0;
  McuRow := 0;
  //while (McuRow < FCurrentScan[0].DataRows)
  while (McuRow < FCurrentScan[0].NilRows)
  do begin
     {
     // Fire OnProgress.
     if Assigned(FOnProgress)
     then begin
          FOnProgress(Self, 100 * McuRow div FCurrentScan[0].NilRows, DoBreak);
          if DoBreak
          then FError := EC_USERCANCEL;
     end;
     }
     if (FError = EC_OK)
     then begin
          //for McuCol := 0 to (FCurrentScan[0].DataCols  - 1)
          for McuCol := 0 to (FCurrentScan[0].NilCols - 1)
          do begin
             if (FRestartInterval <> 0) and (FRestartInterval = FRestartCount)
             then begin
                  FCurrentScan[0].EncodeRefineEobRun;
                  if InWriteMode
                  then WriteRestartMarker;
                  FRestartCount := 0;
             end;

             // Decode sequential data.
             FCurrentScan[0].EncodeACRefineScan(McuRow, McuCol);
             inc(FRestartCount);
          end;
     end;
     inc(McuRow);
  end;
  FCurrentScan[0].EncodeRefineEobRun;
end; // TmcmJPEGImage.ProcessACRefineScan.


procedure TmcmJPEGImage.WriteACFirstScan(Index : integer);
begin
  // Reset AC Huffman tables.
  FCurrentScan[0].HuffmanTable[1].ClearTable;

  // Set restart interval.
  WriteRestartInterval(FRestartInterval);

  // Get statistics for Huffman tables.
  FCurrentScan[0].SetWriteMode(False, JPG_AC_FIRST);
  ProcessACFirstScan(False);

  // Build & write Huffman tables.
  CreateHuffmanTables(Minimum(FCurrentScan[0].ID - 1, 1), False, True);
  WriteHuffmanTables(Minimum(FCurrentScan[0].ID - 1, 1), False, True);

  // Set write mode.
  FCurrentScan[0].SetWriteMode(True, JPG_AC_FIRST);

  // Fill SOS marker.
  FJFIFSOS.Count := 1;
  FJFIFSOS.Component[0].Identifier := FCurrentScan[0].ID;
  if (FCurrentScan[0].ID = 1)
  then FJFIFSOS.Component[0].DCACHuffman := 0
  else FJFIFSOS.Component[0].DCACHuffman := $11;
  FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
  FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
  FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                             FCurrentScan[0].ApproxLow;

  // Write SOS marker.
  FStream.WriteWord(JPG_SOS);
  FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
  FStream.Write(FScanCount, 1);
  FStream.Write(FJFIFSOS.Component[0], SizeOf(TJFIFSOSComponent));
  FStream.Write(FJFIFSOS.SpectralStart, 1);
  FStream.Write(FJFIFSOS.SpectralEnd, 1);
  FStream.Write(FJFIFSOS.Approximation, 1);

  // Write compressed image data.
  ProcessACFirstScan(True);
end; // TmcmJPEGImage.WriteACFirstScan.


procedure TmcmJPEGImage.WriteACRefineScan(Index : integer);
begin
  // Reset AC Huffman tables.
  FCurrentScan[0].HuffmanTable[1].ClearTable;

  // Set restart interval.
  WriteRestartInterval(FRestartInterval);

  // Get statistics for Huffman tables.
  FCurrentScan[0].SetWriteMode(False, JPG_AC_REFINE);
  ProcessACRefineScan(False);

  // Build & write Huffman tables.
  CreateHuffmanTables(Minimum(FCurrentScan[0].ID - 1, 1), False, True);
  WriteHuffmanTables(Minimum(FCurrentScan[0].ID - 1, 1), False, True);

  // Set write mode.
  FCurrentScan[0].SetWriteMode(True, JPG_AC_REFINE);

  // Fill SOS marker.
  FJFIFSOS.Count := 1;
  FJFIFSOS.Component[0].Identifier := FCurrentScan[0].ID;
  if (FCurrentScan[0].ID = 1)
  then FJFIFSOS.Component[0].DCACHuffman := 0
  else FJFIFSOS.Component[0].DCACHuffman := $11;
  FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
  FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
  FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                             FCurrentScan[0].ApproxLow;

  // Write SOS marker.
  FStream.WriteWord(JPG_SOS);
  FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
  FStream.Write(FScanCount, 1);
  FStream.Write(FJFIFSOS.Component[0], SizeOf(TJFIFSOSComponent));
  FStream.Write(FJFIFSOS.SpectralStart, 1);
  FStream.Write(FJFIFSOS.SpectralEnd, 1);
  FStream.Write(FJFIFSOS.Approximation, 1);

  // Write compressed image data.
  ProcessACRefineScan(True);
end; // TmcmJPEGImage.WriteACRefineScan.


procedure TmcmJPEGImage.WriteDCFirstScan(Index : integer);
var c : integer;
begin
  for c := 0 to (FScanCount - 1)
  do if (c < 2)
     then FCurrentScan[c].HuffmanTable[0].ClearTable; // Reset DC tables.

  // Set restart interval.
  WriteRestartInterval(FRestartInterval);

  // Get statistics for Huffman tables.
  for c := 0 to (FScanCount - 1)
  do FCurrentScan[c].SetWriteMode(False, JPG_DC_FIRST);

  FGlobalMcuRow := 0;
  if FInterleavedScan
  then WriteInterleaved(False)     // Y, Cb, Cr data - Sequential interleaved
  else WriteNonInterleaved(False); // Y data - Sequential non-interleaved

  // Build & write Huffman tables.
  for c := 0 to (FScanCount - 1)
  do if (c < 2)
     then begin
          CreateHuffmanTables(Minimum(FCurrentScan[c].ID - 1, 1), True, False);
          WriteHuffmanTables(Minimum(FCurrentScan[c].ID - 1, 1), True, False);
     end;

  // Set write mode.
  for c := 0 to (FScanCount - 1)
  do FCurrentScan[c].SetWriteMode(True, JPG_DC_FIRST);

  // Fill SOS marker.
  FJFIFSOS.Count := FScanCount;
  for c := 0 to (FScanCount - 1)
  do begin
     FJFIFSOS.Component[c].Identifier := FCurrentScan[c].ID;
     if (FCurrentScan[c].ID = 1)
     then FJFIFSOS.Component[c].DCACHuffman := 0
     else FJFIFSOS.Component[c].DCACHuffman := $11;
  end;
  FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
  FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
  FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                             FCurrentScan[0].ApproxLow;

  // Write SOS marker.
  FStream.WriteWord(JPG_SOS);
  FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
  FStream.Write(FScanCount, 1);
  for c := 0 to (FScanCount - 1)
  do FStream.Write(FJFIFSOS.Component[c], SizeOf(TJFIFSOSComponent));
  FStream.Write(FJFIFSOS.SpectralStart, 1);
  FStream.Write(FJFIFSOS.SpectralEnd, 1);
  FStream.Write(FJFIFSOS.Approximation, 1);

  // Write compressed image data.
  FGlobalMcuRow := 0;
  if (FScanCount <> 1)
  then WriteInterleaved(True)     // Y, Cb, Cr data - Sequential interleaved
  else WriteNonInterleaved(True); // Y data - Sequential non-interleaved
end; // TmcmJPEGImage.WriteDCFirstScan.


procedure TmcmJPEGImage.WriteDCRefineScan(Index : integer);
// Write data to refine DC scan. Note this scan does not use Huffman tables.
var c : integer;
begin
  // Set write mode.
  for c := 0 to (FScanCount - 1)
  do FCurrentScan[c].SetWriteMode(True, JPG_DC_REFINE);

  // Fill SOS marker.
  FJFIFSOS.Count := FScanCount;
  for c := 0 to (FScanCount - 1)
  do begin
     FJFIFSOS.Component[c].Identifier := FCurrentScan[c].ID;
     // Huffman tables are not used!
     FJFIFSOS.Component[c].DCACHuffman := 0;
  end;
  FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
  FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
  FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                             FCurrentScan[0].ApproxLow;

  // Write SOS marker.
  FStream.WriteWord(JPG_SOS);
  FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
  FStream.Write(FScanCount, 1);
  for c := 0 to (FScanCount - 1)
  do FStream.Write(FJFIFSOS.Component[c], SizeOf(TJFIFSOSComponent));
  FStream.Write(FJFIFSOS.SpectralStart, 1);
  FStream.Write(FJFIFSOS.SpectralEnd, 1);
  FStream.Write(FJFIFSOS.Approximation, 1);

  // Write compressed image data.
  FGlobalMcuRow := 0;
  if (FScanCount <> 1)
  then WriteInterleaved(True)     // Y, Cb, Cr data - Sequential interleaved
  else WriteNonInterleaved(True); // Y data - Sequential non-interleaved
end; // TmcmJPEGImage.WriteDCRefineScan.


procedure TmcmJPEGImage.WriteProgressiveData(Index : integer);
begin
  if (FJFIFSOS.SpectralStart = 0)
  then begin
       if (FJFIFSOS.SpectralEnd <> 0)
       then FError := EC_JPEGPROGACDC;

       if (FApproxHigh = 0)
       then WriteDCFirstScan(Index)
       else WriteDCRefineScan(Index);
  end
  else begin
       // AC scans only contains one component.
       if (FScanCount = 1)
       then begin
            if (FApproxHigh = 0)
            then WriteACFirstScan(Index)
            else WriteACRefineScan(Index);
       end
       else FError := EC_JFIFBADCOMPCOUNT;
  end;

  // Flush written bits to file.
  FStream.FlushBits;
end; // TmcmJPEGImage.WriteProgressiveData.

type
  TProgressiveScan  = record
                        ID            : word; // ID = 7 -> Y, Cb and Cr components
                                              //      1 -> Y component
                                              //      2 -> Cb component
                                              //      4 -> Cr component
                        ApproxLow     : byte; //
                        ApproxHigh    : byte; //
                        SpectralStart : byte; //
                        SpectralEnd   : byte; //
                      end;

  TProgressiveFrame = record
                        Count : word;
                        Scan  : array[0..9] of TProgressiveScan;
                      end;

const
  GreyProgNonInterleaved1 : TProgressiveFrame
                          = (Count : 3;
                             Scan  : ((ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 5),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 6; SpectralEnd : 63),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0));
                            );
  RGBProgInterleaved1     : TProgressiveFrame
                          = (Count : 8;
                             Scan  : ((ID : 7; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 4; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 7; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 4; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0));
                            );
  {
  RGBProgInterleaved2     : TProgressiveFrame
                          = (Count : 10;
                             Scan  : ((ID : 7; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 2; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 5),
                                      (ID : 4; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 1; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 1; ApproxLow : 2; ApproxHigh : 0; SpectralStart : 6; SpectralEnd : 63),
                                      (ID : 1; ApproxLow : 1; ApproxHigh : 2; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 7; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 4; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 1; SpectralStart : 1; SpectralEnd : 63));
                            );
  RGBProgInterleaved3     : TProgressiveFrame
                          = (Count : 10;
                             Scan  : ((ID : 7; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 4; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0));
                            );
  }
  RGBProgNonInterleaved1  : TProgressiveFrame
                          = (Count : 6;
                             Scan  : ((ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 2; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 4; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 1; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 2; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 4; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 1; SpectralEnd : 63),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0),
                                      (ID : 0; ApproxLow : 0; ApproxHigh : 0; SpectralStart : 0; SpectralEnd : 0));
                            );

procedure TmcmJPEGImage.WriteProgressiveFrame;
// Write progressive frame.
var i, c : integer; // c - Component index.
    ps   : TProgressiveFrame;
begin
  if (FComponentCount = 1)
  then begin // Grey scale
       ps := GreyProgNonInterleaved1;
  end
  else begin // RGB.
       if FInterleavedScan
       then ps := RGBProgInterleaved1
       else ps := RGBProgNonInterleaved1;
  end;

  for i := 0 to (ps.Count - 1)
  do begin
     FApproxLow             := ps.Scan[i].ApproxLow;
     FApproxHigh            := ps.Scan[i].ApproxHigh;
     FJFIFSOS.SpectralStart := ps.Scan[i].SpectralStart;
     FJFIFSOS.SpectralEnd   := ps.Scan[i].SpectralEnd;

     FScanCount := 0;
     for c := 0 to 2
     do begin
        if ((ps.Scan[i].ID and (1 shl c)) <> 0)
        then begin
             FCurrentScan[FScanCount] := GetScanComponent(c + 1);
             FCurrentScan[FScanCount].ApproxLow     := ps.Scan[i].ApproxLow;
             FCurrentScan[FScanCount].ApproxHigh    := ps.Scan[i].ApproxHigh;
             FCurrentScan[FScanCount].SpectralStart := ps.Scan[i].SpectralStart;
             FCurrentScan[FScanCount].SpectralEnd   := ps.Scan[i].SpectralEnd;
             inc(FScanCount);
        end;
     end;
     WriteProgressiveData(0);
  end;
end; // TmcmJPEGImage.WriteProgressiveFrame.


procedure TmcmJPEGImage.WriteSequentialData;
// Write sequential scan data.
var c         : integer; // c - Component index.
//    ScanIndex : integer; //
begin
  if FInterleavedScan // True when FComponentCount = 3.
  then FScanCount := FComponentCount
  else FScanCount := 1;

//  ScanIndex := 0;
//  while (ScanIndex < FComponentCount)
//  do begin
     // Get next component(s) to process.
     for c := 0 to (FScanCount - 1)
     do begin
        FCurrentScan[c] := FYCbCrComp[{ScanIndex + }c];
        // Reset Huffman tables.
        if (c < 2)
        then begin
             FCurrentScan[c].HuffmanTable[0].ClearTable;
             FCurrentScan[c].HuffmanTable[1].ClearTable;
        end;
     end;

     // Set restart interval.
     if (FScanCount <> 1)
     then WriteRestartInterval(FRowsPerRestart * FMcuCols)
     else WriteRestartInterval(FRowsPerRestart * FCurrentScan[0].DataCols);

     // Get statistics for Huffman tables.
     for c := 0 to (FScanCount - 1)
     do FCurrentScan[c].SetWriteMode(False, JPG_SEQ);

     FGlobalMcuRow := 0;
     if FInterleavedScan
     then WriteInterleaved(False)     // Y, Cb, Cr data - Sequential interleaved
     else WriteNonInterleaved(False); // Y data - Sequential non-interleaved

     // Build & write Huffman tables.
     for c := 0 to (FScanCount - 1)
     do if (c < 2)
        then begin
             CreateHuffmanTables(Minimum(c{ + ScanIndex}, 1), True, True);
             WriteHuffmanTables(Minimum(c{ + ScanIndex}, 1), True, True);
        end;

     FStream.ResetBitIndex;
     ResetDCDifference;

     // Set write mode.
     for c := 0 to (FScanCount - 1)
     do FCurrentScan[c].SetWriteMode(True, JPG_SEQ);

     // Fill SOS marker.
     FJFIFSOS.Count := FScanCount;
     for c := 0 to (FScanCount - 1)
     do begin
        FJFIFSOS.Component[c].Identifier := FCurrentScan[c].ID;
        if (c{ + ScanIndex} = 0)
        then FJFIFSOS.Component[c].DCACHuffman := 0
        else FJFIFSOS.Component[c].DCACHuffman := $11;
     end;
     FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
     FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
     FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                                FCurrentScan[0].ApproxLow;

     // Write SOS marker.
     FStream.WriteWord(JPG_SOS);
     FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
     FStream.Write(FScanCount, 1);
     for c := 0 to (FScanCount - 1)
     do FStream.Write(FJFIFSOS.Component[c], SizeOf(TJFIFSOSComponent));
     FStream.Write(FJFIFSOS.SpectralStart, 1);
     FStream.Write(FJFIFSOS.SpectralEnd, 1);
     FStream.Write(FJFIFSOS.Approximation, 1);

     // Write compressed image data.
     FGlobalMcuRow := 0;
     if FInterleavedScan
     then WriteInterleaved(True)     // Y, Cb, Cr data - Sequential interleaved
     else WriteNonInterleaved(True); // Y data - Sequential non-interleaved

     // Flush written bits to file.
     FStream.FlushBits;

//     inc(ScanIndex, FScanCount);
//  end;
end; // TmcmJPEGImage.WriteSequentialData.


procedure TmcmJPEGImage.WriteSOI;
begin
  FStream.WriteWord(JPG_SOI);
end; // TmcmJPEGImage.WriteSOI.


procedure TmcmJPEGImage.WriteEOI;
begin
  FStream.WriteWord(JPG_EOI);
end; // TmcmJPEGImage.WriteEOI.


procedure TmcmJPEGImage.WriteSOFn;
var Count : word;
begin
  // Write Start Of Frame.
  if FInterlaced
  then FStream.WriteWord(JPG_SOF0)
  // SOF0 only supports 8 bit samples, and two DC and two AC
  // tables, where SOF1 supports 8 and 12 bit samples, and
  // four DC and four AC tables.
  else FStream.WriteWord(JPG_SOF2);
  FJFIFSOF.ImageHeight := SwapWord(FJFIFSOF.ImageHeight);
  FJFIFSOF.ImageWidth := SwapWord(FJFIFSOF.ImageWidth);
  Count := SizeOf(TJFIFSOF_Minor) + FComponentCount * SizeOf(TJFIFSOFComponent);
  FStream.WriteWord(Count + 2); // Write marker size.
  FStream.Write(FJFIFSOF, Count);
end; // TmcmJPEGImage.WriteSOFn.


procedure TmcmJPEGImage.WriteInitialise(IsTIFF : boolean);
{$IFDEF mcmJPEGEnginee}
var i, j          : integer;
    ScanComponent : TmcmJPEGComponent;
{$ENDIF}
begin
  FGlobalMcuRow := 0;

  if (FCompress = CP_JPEG_PROG)
  then FInterlaced := False
  else FInterlaced := True;
  {$IFDEF mcmJPEGEnginee}
    // Get/check color format.
    case GetPhotometric of
    PM_INVGREY,
    PM_GREY    : FComponentCount := 1;
    PM_PALETTE,
    PM_RGB,
    PM_RGBA    : FComponentCount := 3;
    else begin
         FComponentCount := 0;
         FError := EC_BITDEPTH;
    end;
    end;
    case FDibInfo^.bmiHeader.biBitCount of
    8, 24, 32 : ;
    else FError := EC_BITDEPTH;
    end;

    if (FError = EC_OK)
    then begin
         FInterleavedScan := (FComponentCount = 3);
         FRowsPerRestart  := 0;

         // Start Of Frame
         FJFIFSOF.Bits         := 8;
         FJFIFSOF.ImageHeight  := FDibInfo^.bmiHeader.biHeight;
         FJFIFSOF.ImageWidth   := FDibInfo^.bmiHeader.biWidth;
         FJFIFSOF.NoComponents := FComponentCount;

         // Uses FQuality to determind compression via quantification
         // tables.
         CreateQuantificationTables;

         // Create Huffman tables
         for i := 0 to 1 // DC or AC
         do for j := 0 to 1 // Table index
            do FHuffman[i,j] := TmcmHuffmanJPEG.Create(JPEGMaxHuffmanCodes, JPEGMaxHuffmanCodeLength);

         // Create scan components.
         for i := 0 to (FComponentCount - 1)
         do begin
            // Create scan components Y, Cb or Cr.
            FJFIFSOF.Components[i].Identifier := i + 1;
            ScanComponent := GetScanComponent(FJFIFSOF.Components[i].Identifier);

            if (FComponentCount = 3)
            then begin // RGB
                 case FYCbCrMode of
                 JYCC_AUTO  : begin
                                ScanComponent.HorizFreq := 2;
                                ScanComponent.VertFreq  := 2;
                                case FQuality of
                                 0..24 : if (i > 0)
                                         then begin
                                              ScanComponent.HorizFreq := 1;
                                              ScanComponent.VertFreq  := 1;
                                         end;
                                25..49 : if (i > 0)
                                         then begin
                                              if (i = 2) or IsTIFF
                                              then ScanComponent.HorizFreq := 2
                                              else ScanComponent.HorizFreq := 1;
                                              ScanComponent.VertFreq := 1;
                                         end;
                                50..74 : if (i > 0) and ((i = 1) or IsTIFF)
                                         then begin
                                              ScanComponent.HorizFreq := 2;
                                              ScanComponent.VertFreq  := 1;
                                         end;
                                else begin
                                     ScanComponent.HorizFreq := 1;
                                     ScanComponent.VertFreq  := 1;
                                     end;
                                end;
                              end;
                 JYCC_411   : if (i = 0)
                              then begin
                                   ScanComponent.HorizFreq := 2;
                                   ScanComponent.VertFreq  := 2;
                              end
                              else begin
                                   ScanComponent.HorizFreq := 1;
                                   ScanComponent.VertFreq  := 1;
                              end;
                 JYCC_421   : if (i = 0)
                              then begin
                                   ScanComponent.HorizFreq := 2;
                                   ScanComponent.VertFreq  := 2;
                              end
                              else begin
                                   ScanComponent.HorizFreq := 2;
                                   ScanComponent.VertFreq  := 1;
                              end;
                 end;
            end
            else begin // Greyscale - always 1
                 ScanComponent.HorizFreq := 1;
                 ScanComponent.VertFreq  := 1;
            end;

            FJFIFSOF.Components[i].Samples := ($F0 and (ScanComponent.HorizFreq shl 4)) or
                                              ($0F and ScanComponent.VertFreq);

            // Associate Quantification table.
            if (i = 0)
            then FJFIFSOF.Components[i].DQTIdentifier := 0
            else FJFIFSOF.Components[i].DQTIdentifier := 1;
            ScanComponent.SetQuant2DCT(FJFIFDQT[FJFIFSOF.Components[i].DQTIdentifier]);

            // Set-up default spectral range and successive approximation.
            ScanComponent.SpectralStart := 0;
            ScanComponent.SpectralEnd   := 63;
            ScanComponent.ApproxHigh    := 0;
            ScanComponent.ApproxLow     := 0;

            if (i = 0)
            then begin
                 ScanComponent.HuffmanTable[0] := FHuffman[0,0]; // Assign DC table
                 ScanComponent.HuffmanTable[1] := FHuffman[1,0]; // Assign AC table
            end
            else begin
                 ScanComponent.HuffmanTable[0] := FHuffman[0,1]; // Assign DC table
                 ScanComponent.HuffmanTable[1] := FHuffman[1,1]; // Assign AC table
            end;
         end;

         // Determind MCU size and maximum sampling frequency.
         CalcMCUDimension;
    end;
  {$ENDIF}
end; // TmcmJPEGImage.WriteInitialise.


function TmcmJPEGImage.WriteTIFFInitialise : integer;
var DQT_Size : integer;
    DHT_Size : integer;
    DRI_Size : integer;
    Index    : integer;
begin
  // --- Used only by TIFF Reader.
  WriteInitialise(True);

  if FInterleavedScan // True when FComponentCount = 3.
  then FScanCount := FComponentCount
  else FScanCount := 1;

  // Allocate memory for all of the Y, Cb and Cr image component.
  for Index := 0 to (FComponentCount - 1)
  do begin
     FYCbCrComp[Index].AllocateBuffer(True, True or Not(FInterlaced));
     if (FComponentCount = 1)
     then FMcuRows := FYCbCrComp[Index].NilRows;
  end;

  // Convert RGB image data to YCbCr.
  if (FComponentCount = 1)
  then CopyFromGrey(-1)
  else CopyFromRGB(-1);

  // Down sample components.
  for Index := 0 to (FComponentCount - 1)
  do FYCbCrComp[Index].Sample;

  if Not(FInterlaced) or True // Progressive mode
  then begin
       // Calculate DCT coefficients.
       for Index := 0 to (FComponentCount - 1)
       do FYCbCrComp[Index].ProgressiveDCT;
  end;

  // Returns the size of the quantification tables.
  DQT_Size := (1 + Minimum(FComponentCount - 1, 1)) * (JPEGSampleSize * (0{Precision} + 1) + 1 + 2 + 2);

  DHT_Size := 0;
  // Clear Huffman tables.
  for Index := 0 to (FComponentCount - 1)
  do begin
     FCurrentScan[Index] := FYCbCrComp[Index];
     if (Index < 2)
     then begin
          FYCbCrComp[Index].HuffmanTable[0].ClearTable;
          FYCbCrComp[Index].HuffmanTable[1].ClearTable;
     end;
  end;

  // Set restart interval.
  DRI_Size := 0;
  if (FScanCount <> 1)
  then begin
       if (FRestartInterval <> FRowsPerRestart * FMcuCols)
       then inc(DRI_Size, 4);
  end
  else begin
       if (FRestartInterval <> FRowsPerRestart * FCurrentScan[0].DataCols)
       then inc(DRI_Size, 4);
  end;

  // Get statistics for Huffman tables.
  for Index := 0 to (FScanCount - 1)
  do FCurrentScan[Index].SetWriteMode(False, JPG_SEQ);

  FGlobalMcuRow := 0;
  if FInterleavedScan
  then WriteInterleaved(False)     // Y, Cb, Cr data - Sequential interleaved
  else WriteNonInterleaved(False); // Y data - Sequential non-interleaved
  FGlobalMcuRow := 0;

  for Index := 0 to (FComponentCount - 1)
  do begin
     if (Index < 2)
     then DHT_Size := DHT_Size + CreateHuffmanTables(Index, True, True) + 2 + 2;
  end;

  // Set write mode.
  for Index := 0 to (FScanCount - 1)
  do FCurrentScan[Index].SetWriteMode(True, JPG_SEQ);

  Result := DQT_Size + DHT_Size + DRI_Size + 4;
end; // TmcmJPEGImage.WriteTIFFInitialise.


function TmcmJPEGImage.WriteTIFFData(Stream : TmcmBufferStream) : integer;
var SavePos    : {$IFNDEF DCB3_5} int64; {$ELSE} longint; {$ENDIF}
    BufferSize : longint;
    SaveStream : TmcmBufferStream;
    Index      : integer;
begin
  SaveStream := FStream;
  FStream := Stream;

  for Index := 0 to (FScanCount - 1)
  do FCurrentScan[Index].FStream := FStream;

  SavePos := FStream.Position;
  WriteSOI;

  // Need to limit the Image Width and Height to current MCU
  FJFIFSOF.ImageHeight  := FMCUHeight;
  FJFIFSOF.ImageWidth   := FDibInfo^.bmiHeader.biWidth;

  // Write Start Of Frame.
  WriteSOFn;

  // Write Non-interlaced image data.

  // Fill SOS marker.
  FJFIFSOS.Count := FScanCount;
  for Index := 0 to (FScanCount - 1)
  do begin
     FJFIFSOS.Component[Index].Identifier := FCurrentScan[Index].ID;
     if (Index = 0)
     then FJFIFSOS.Component[Index].DCACHuffman := 0
     else FJFIFSOS.Component[Index].DCACHuffman := $11;
  end;
  FJFIFSOS.SpectralStart := FCurrentScan[0].SpectralStart;
  FJFIFSOS.SpectralEnd   := FCurrentScan[0].SpectralEnd;
  FJFIFSOS.Approximation := (FCurrentScan[0].ApproxHigh shl 4) or
                             FCurrentScan[0].ApproxLow;

  // Write SOS marker.
  FStream.WriteWord(JPG_SOS);
  FStream.WriteWord(FScanCount * SizeOf(TJFIFSOSComponent) + 6);
  FStream.Write(FScanCount, 1);
  for Index := 0 to (FScanCount - 1)
  do FStream.Write(FJFIFSOS.Component[Index], SizeOf(TJFIFSOSComponent));
  FStream.Write(FJFIFSOS.SpectralStart, 1);
  FStream.Write(FJFIFSOS.SpectralEnd, 1);
  FStream.Write(FJFIFSOS.Approximation, 1);

  // Write compressed image data.
  if FInterleavedScan
  then WriteInterleaved(True)     // Y, Cb, Cr data - Sequential interleaved
  else WriteNonInterleaved(True); // Y data - Sequential non-interleaved

  // Flush written bits to file.
  FStream.FlushBits;

  WriteEOI;

  // Flush stream.
  FStream.Flush;

  BufferSize := FStream.Position - SavePos;

  Result := BufferSize;
  FStream := SaveStream;
end; // TmcmJPEGImage.WriteTIFFData.

{$ENDIF}


procedure TmcmJPEGImage.SaveToStream(Stream : TStream);
{$IFDEF mcmJPEGEnginee}
var i          : integer;
    Count      : word;
{$ELSE}
var BordBitmap : TBitmap;
    JPEGImage  : TJPEGImage;
    dibDC      : HDC;
{$ENDIF}
begin
  FError := EC_OK;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            WriteInitialise(False);

            {$IFDEF mcmJPEGEnginee}
              // FYCbCrComp[0] will contain Y component.
              // FYCbCrComp[1] will contain Cb component.
              // FYCbCrComp[2] will contain Cr component.

              if (FError = EC_OK)
              then begin
                   //-------------------
                   // Write file header.

                   // Write Start Of Image.
                   WriteSOI;

                   // Write JFIF / Application header.
                   with FJFIFAPP0Header
                   do begin
                      Identifier := 'JFIF';
                      MajorID    := 1;
                      MinorID    := 1;
                      if Assigned(FImageInfo)
                      then begin
                           if (FImageInfo.XResolution = 0) or
                              (FImageInfo.YResolution = 0)
                           then FImageInfo.Units := UN_PIXELS;
                           if Not(FImageInfo.Units in [UN_PIXELS,UN_INCHS,UN_CENTIMETERS])
                           then FImageInfo.Units := UN_INCHS;
                           case FImageInfo.Units of
                           UN_PIXELS      : begin
                                              Units      := 0;
                                              XDensity   := SwapWord(FImageInfo.PixelWidth);
                                              YDensity   := SwapWord(FImageInfo.PixelHeight);
                                            end;
                           UN_INCHS       : begin
                                              Units      := 1;
                                              XDensity   := SwapWord(Round(FImageInfo.XResolution));
                                              YDensity   := SwapWord(Round(FImageInfo.YResolution));
                                            end;
                           UN_CENTIMETERS : begin
                                              Units      := 2;
                                              XDensity   := SwapWord(Round(FImageInfo.XResolution));
                                              YDensity   := SwapWord(Round(FImageInfo.YResolution));
                                            end;
                           end;
                      end
                      else begin
                           Units      := 0;
                           XDensity   := 1;
                           YDensity   := 1;
                      end;
                      // This will skip a thumbnail.
                      XThumbnail := 0;
                      YThumbnail := 0;
                   end;
                   FStream.WriteWord(JPG_APP0);
                   Count := SizeOf(TJFIFAPP0Header);
                   FStream.WriteWord(Count + 2);  // Write marker size.
                   FStream.Write(FJFIFAPP0Header, Count);
                   //--

                   // JPG_APP1 - Exif & GPS data
                   if Assigned(FImageInfo)
                   then begin
                        if Assigned(FImageInfo.Exif)
                        then begin
                             // Write JPG_APP1 marker.
                             // Set and Write TIFF/Exif/GPS TAG's
                             WriteTiffAndExif;
                        end;
                   end;
                   //--

                   // JPG_COM - Comment
                   if Assigned(FImageInfo)
                   then begin
                        Count := length(FImageInfo.Description);
                        if (Count > 0)
                        then begin
                             FStream.WriteWord(JPG_COM);
                             FStream.WriteWord(Count + 1 + 2);  // Write marker size.
                             FStream.Write(FImageInfo.Description[1], Count);
                             i := 0;
                             FStream.Write(i, 1);
                        end;
                   end;
                   //--


                   // Write Start Of Frame.
                   WriteSOFn;

                   // Write Quantification tables.
                   WriteQuantification;

                   // Allocate memory for all of the Y, Cb and Cr image component.
                   for i := 0 to (FComponentCount - 1)
                   do begin
                      FYCbCrComp[i].AllocateBuffer(True, True or Not(FInterlaced));
                      if (FComponentCount = 1)
                      then FMcuRows := FYCbCrComp[i].NilRows;
                   end;

                   // Convert RGB image data to YCbCr.
                   if (FComponentCount = 1)
                   then CopyFromGrey(-1)
                   else CopyFromRGB(-1);

                   // Down sample components.
                   for i := 0 to (FComponentCount - 1)
                   do FYCbCrComp[i].Sample;

                   if Not(FInterlaced) or True // Progressive mode
                   then begin
                        // Calculate DCT coefficients.
                        for i := 0 to (FComponentCount - 1)
                        do FYCbCrComp[i].ProgressiveDCT;
                   end;

                   // Write Huffman tables & image data.
                   if FInterlaced
                   then WriteSequentialData
                   else WriteProgressiveFrame;

                   // JPG_EOI
                   WriteEOI;

                   // Flush stream.
                   FStream.Flush;
              end;

            {$ELSE}

            // Using Borland's JPEG implementation.
            BordBitmap := TBitmap.Create;
            try
              case FDibInfo^.bmiHeader.biBitCount of
              2  : BordBitmap.PixelFormat := pf1bit;
              4  : BordBitmap.PixelFormat := pf4bit;
              8  : BordBitmap.PixelFormat := pf8bit;
              24 : BordBitmap.PixelFormat := pf24bit;
              end;
              BordBitmap.Width  := FDibInfo^.bmiHeader.biWidth;
              BordBitmap.Height := FDibInfo^.bmiHeader.biHeight;
              BordBitmap.HandleType := bmDIB;

              dibDC := GetDC(0);
              try
                if (FDibInfo^.bmiHeader.biBitCount <= 8)
                then BordBitmap.Palette := CreatePalette;

                SetDIBits(dibDC, BordBitmap.Handle,
                          0, FDibInfo^.bmiHeader.biHeight,
                          FDibBits, FDibInfo^, DIB_RGB_COLORS);
              finally
                ReleaseDC(0, dibDC);
              end;
              if (BordBitmap.PixelFormat <> pf24bit)
              then BordBitmap.PixelFormat := pf24bit;
            except
            end;

            try
              JPEGImage := TJPEGImage.Create;
              try
                // TJPEGImage(BordImage.Picture.Graphic).JPEGNeeded;
              except
              end;
              JPEGImage.Assign(BordBitmap);
              JPEGImage.Grayscale := False;
              if FInterlaced
              then JPEGImage.ProgressiveEncoding := False
              else JPEGImage.ProgressiveEncoding := True;
              JPEGImage.CompressionQuality := FQuality;

              JPEGImage.SaveToStream(Stream);
              JPEGImage.Free;
            finally
              BordBitmap.Free;
            end;
            {$ENDIF}
       end;
  end;
end; // TmcmJPEGImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmJBIGImage
//------------------------------------------------------------------------------

{$IFDEF mcmJBIG}

class function TmcmJBIGImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,{IF_PAL4,}IF_GREY8{,IF_PAL8,IF_RGB24}];
end; // TmcmJBIGImage.GetColorFormats.


class function TmcmJBIGImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  {
  if (ImageFormat in [IF_NONE,IF_BW])
  then begin
       Count := 0; // Inherited GetCompressionFromColor(ImageFormat, Compress);
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE_PCX;
       inc(Count);
  end;
  }
  Result := Count;
end; // TmcmJBIGImage.GetCompressionFromColor.


class function TmcmJBIGImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP  : Result := resCP_NOCOMP;
  else Result := '';
  end;
end; // TmcmJBIGImage.GetCompressionName.


constructor TmcmJBIGImage.Create;
begin
  Inherited Create;
end; // TmcmJBIGImage.Create.


destructor TmcmJBIGImage.Destroy;
begin
  Inherited Destroy;
end; // TmcmJBIGImage.Destroy.


function TmcmJBIGImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;

         FStream.Read(FHeader, SizeOf(TJBIGHeader));
         FHeader.Width  := SwapLong(FHeader.Width);
         FHeader.Height := SwapLong(FHeader.Height);
         FHeader.l0     := SwapLong(FHeader.l0);

         if (FHeader.NotUsed = 0) and
            (FHeader.DL <= FHeader.D) and
            (FHeader.BitPlanes > 0) and
            (FHeader.Options and $80 = 0) and
            (FHeader.mx < 128) and
            (FHeader.Order in [0,$2,$3,$4,$5,$6,8,$A,$B,$C,$D,$E])
         then FError := EC_OK;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmJBIGImage.IsFormatValid.


procedure TmcmJBIGImage.LoadFromStream(Stream : TStream);
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       // Read header and check format.
       if (IsFormatValid(Stream) = EC_OK)
       then begin
            // FStream.Position := FStreamStart;
            if (FDibInfo <> nil)
            then begin
                 // Read header information.
                 with FDibInfo.bmiHeader, FHeader
                 do begin
                    biWidth         := Width;
                    biHeight        := Height;
                    biPlanes        := 1;
                    biBitCount      := BitPlanes;
                    biXPelsPerMeter := Round(100.0 * 72 / 2.54);
                    biYPelsPerMeter := Round(100.0 * 72 / 2.54);
                    biClrUsed       := 0;
                    biClrImportant  := 0;
                 end;

                 // Load image data.
                 if CreateDIB
                 then begin
                      try
                      
                      except
                        On Exception
                        do FError := EC_UNKNOWN;
                      end;
                 end;
            end;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end;
end; // TmcmJBIGImage.LoadFromStream.


procedure TmcmJBIGImage.SaveToStream(Stream : TStream);
begin
end; // TmcmJBIGImage.SaveToStream.

{$ENDIF}

//------------------------------------------------------------------------------
// TmcmJPEG2KImage
//------------------------------------------------------------------------------

{$IFDEF mcmJPEG2K}

class function TmcmJPEG2KImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [{IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,}IF_RGB24];
end; // TmcmJPEG2KImage.GetColorFormats.


class function TmcmJPEG2KImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  {
  if (ImageFormat in [IF_NONE,IF_BW..IF_PAL8,IF_RGB24])
  then begin
       Count := 0; // Inherited GetCompressionFromColor(ImageFormat, Compress);
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE_PCX;
       inc(Count);
  end;
  }
  Result := Count;
end; // TmcmJPEG2KImage.GetCompressionFromColor.


class function TmcmJPEG2KImage.GetCompressionName(Compression : TmcmCompress) : string; 
begin
  case Compression of
  CP_NOCOMP  : Result := resCP_NOCOMP;
  else Result := '';
  end;
end; // TmcmJPEG2KImage.GetCompressionName.


constructor TmcmJPEG2KImage.Create; 
begin
  Inherited Create;
end; // TmcmJPEG2KImage.Create.


destructor TmcmJPEG2KImage.Destroy;
begin
  Inherited Destroy;
end; // TmcmJPEG2KImage.Destroy.


function TmcmJPEG2KImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;

       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmJPEG2KImage.IsFormatValid.


procedure TmcmJPEG2KImage.LoadFromStream(Stream : TStream);
begin
end; // TmcmJPEG2KImage.LoadFromStream.


procedure TmcmJPEG2KImage.SaveToStream(Stream : TStream);
begin
end; // TmcmJPEG2KImage.SaveToStream.

{$ENDIF}

//------------------------------------------------------------------------------
// TmcmPCXImage
//------------------------------------------------------------------------------

class function TmcmPCXImage.GetColorFormats : TmcmImageFormats;
begin
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24];
end; // TmcmPCXImage.GetColorFormats.


class function TmcmPCXImage.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  Count := 0;
  if (ImageFormat in [IF_NONE,IF_BW..IF_PAL8,IF_RGB24])
  then begin
       Count := 0; // Inherited GetCompressionFromColor(ImageFormat, Compress);
       if (High(Compress) > Count)
       then Compress[Count] := CP_RLE_PCX;
       inc(Count);
  end;
  Result := Count;
end; // TmcmPCXImage.GetCompressionFromColor.


class function TmcmPCXImage.GetCompressionName(Compression : TmcmCompress) : string;
begin
  case Compression of
  CP_NOCOMP  : Result := resCP_NOCOMP;
  CP_RLE_PCX : Result := resCP_RLE1_24;
  else Result := '';
  end;
end; // TmcmPCXImage.GetCompressionName.


constructor TmcmPCXImage.Create;
begin
  Inherited Create;
end; // TmcmPCXImage.Create


destructor TmcmPCXImage.Destroy;
begin
  Inherited Destroy;
end; // TmcmPCXImage.Destroy.


function TmcmPCXImage.IsFormatValid(Stream : TStream) : TmcmErrorCode;
var PcxID  : word;
    PcxEnc : word;
begin
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         FStream.Position := FStreamStart;
         FStream.Read(PcxID, SizeOf(word));
         // PCX Flag = 10
         // Version = 0, 2.5
         //           2, 2.8 With palette
         //           3, 2.8 Without palette
         //           4, PC PaintBrush
         //           5, 3.0 and later of PC PaintBrush.
         if (Lo(PcxID) = 10) and
            (Hi(PcxID) in [0, 2, 3, 4, 5])
         then begin
              FStream.Read(PcxEnc, SizeOf(word));
              if (Lo(PcxEnc) = 1) and
                 (Hi(PcxEnc) in [1,{2,}4,8])
                 then FError := EC_OK;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPCXImage.IsFormatValid.


function TmcmPCXImage.ReadHeader(Stream : TStream) : TmcmErrorCode;
var i : word;
begin
  if Assigned(Stream)
  then begin
       try
         if (IsFormatValid(Stream) = EC_OK)
         then begin
              // Read header and check format.
              FStream.Position := FStreamStart;

              if (FDibInfo <> nil)
              then begin
                   // Read header information.
                   FStream.Read(FPCXHeader, SizeOf(TPCXHeader));

                   with FDibInfo.bmiHeader, FPCXHeader
                   do begin
                      biWidth         := Window.Xmax - Window.Xmin + 1;
                      biHeight        := Window.Ymax - Window.Ymin + 1;
                      biPlanes        := 1;
                      biBitCount      := NPlanes * BitsPerPixel;
                      biXPelsPerMeter := Round(100.0 * HDpi / 2.54);
                      biYPelsPerMeter := Round(100.0 * VDpi / 2.54);
                      biClrUsed       := 0;
                      biClrImportant  := 0;
                   end;

                   if ((FPCXHeader.PaletteInfo in [0,1]) or
                       ((FPCXHeader.PaletteInfo in [2]) and (FDibInfo.bmiHeader.biBitCount <= 4))) and
                      Not((FDibInfo.bmiHeader.biBitCount = 1) and (FPCXHeader.PaletteInfo = 1))
                   then begin
                        if (FPCXHeader.Version = 5) or
                           (FPCXHeader.Version = 2) or
                           (FPCXHeader.Version = 0)
                        then begin
                             // Convert PCX header palette
                             for i := 0 to 15
                             do begin
                                with FDibInfo^.bmiColors[i]
                                do begin
                                   rgbRed   := FPCXHeader.Colormap[3*i];
                                   rgbGreen := FPCXHeader.Colormap[3*i+1];
                                   rgbBlue  := FPCXHeader.Colormap[3*i+2];
                                   rgbReserved := 0;
                                end;
                             end;
                        end
                        else CreateGreyPalette(PM_GREY);
                   end
                   else CreateGreyPalette(PM_GREY);

                   // Get spatial resolution information.
                   if Assigned(FImageInfo)
                   then begin
                        FImageInfo.Units := UN_INCHS;
                        FImageInfo.XResolution := FPCXHeader.HDpi;
                        FImageInfo.YResolution := FPCXHeader.VDpi;
                   end;

                   if (FPCXHeader.Encoding = 1)
                   then FCompress := CP_RLE_PCX
                   else FCompress := CP_NOCOMP;
              end
              else FError := EC_NOMEMORY;
         end;
       except
         On E:Exception
         do FError := EC_READFROMFILE;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPCXImage.ReadHeader.


function TmcmPCXImage.ReadPalette(Stream : TStream) : TmcmErrorCode;
var PCXPalette : array[0..255] of TRGBTriple;
    PalMarker  : byte;
    i          : word;
begin
  try
    FStream.Read(PalMarker, SizeOf(byte));
    case PalMarker of
    12 : begin
           if (FDibInfo^.bmiHeader.biCompression <> BI_BITFIELDS)
           then FNoColors := GetNumColors(FDibInfo)
           else FNoColors := 0;

           if (FNoColors > 0)
           then begin
                // Read palette values.
                FStream.Read(PCXPalette, SizeOf(PCXPalette));

                // Convert R-G-B triple to B-G-R quad palette.
                if Not((FDibInfo.bmiHeader.biBitCount = 1) and (FPCXHeader.PaletteInfo = 1))
                then begin
                     for i := 0 to (FNoColors - 1)
                     do begin
                        with FDibInfo^.bmiColors[i]
                        do begin
                           rgbBlue  := PCXPalette[i].rgbtRed;
                           rgbGreen := PCXPalette[i].rgbtGreen;
                           rgbRed   := PCXPalette[i].rgbtBlue;
                           rgbReserved := 0;
                        end;
                     end;

                     // Assign palette to DIBSection handle.
                     SetbmiPalToDibHandle;
                end;
           end;
         end;
    13 : begin // ? PCX 16 color, 1 Plane format
         end
    else FError := EC_UNKNOWNFORMAT;
    end;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
  Result := FError;
end; // TmcmPCXImage.ReadPalette.


function TmcmPCXImage.WriteHeader(Stream : TStream) : TmcmErrorCode;
var i           : word;
    // NoColors    : cardinal;
    Photometric : TmcmPhotometric;
begin
  if Assigned(Stream)
  then begin
       FError := EC_OK;

       // NoColors := GetNumColors(FDibInfo);
       Photometric := GetPhotometric;

       FillChar(FPCXHeader, SizeOf(TPCXHeader), 0);
       with FDibInfo^.bmiHeader, FPCXHeader
       do begin
          // Fill file header.
          Manufacturer := 10;
          Version      := 5;
          Encoding     := 1;
          Window.Xmin  := 0;
          Window.Ymin  := 0;
          Window.Xmax  := biWidth - 1;
          Window.Ymax  := biHeight - 1;

          case biBitCount of
          1  : begin
                 Version := 2;
                 BitsPerPixel := 1;
                 NPlanes      := 1;
                 BytesPerLine := (7 + biWidth) div 8;
                 PaletteInfo  := 1;
               end;
          4  : begin
                 Version := 2;
                 BitsPerPixel := 1;
                 NPlanes      := 4;
                 BytesPerLine := (7 + biWidth) div 8;
                 if (Photometric = PM_GREY)
                 then PaletteInfo := 2
                 else PaletteInfo := 1;
               end;
          8  : begin
                 BitsPerPixel := 8;
                 NPlanes      := 1;
                 BytesPerLine := biWidth;
                 if (Photometric = PM_GREY)
                 then begin
                      PaletteInfo := 2;
                      if Odd(BytesPerLine)
                      then inc(BytesPerLine);
                 end
                 else PaletteInfo := 1;
               end;
          24 : begin
                 BitsPerPixel := 8;
                 NPlanes      := 3;
                 BytesPerLine := biWidth;
                 PaletteInfo  := 0;
               end;
          else FError := EC_COMPRESSCOLOR;
          end;

          HDpi := Round(2.54 * biXPelsPerMeter / 100.0);
          VDpi := Round(2.54 * biYPelsPerMeter / 100.0);

          if (FPCXHeader.PaletteInfo in [1,2])
          then begin
               // Convert PCX header palette
               for i := 0 to 15
               do begin
                  with FDibInfo^.bmiColors[i]
                  do begin
                     FPCXHeader.Colormap[3*i]   := rgbRed;
                     FPCXHeader.Colormap[3*i+1] := rgbGreen;
                     FPCXHeader.Colormap[3*i+2] := rgbBlue;
                  end;
               end;
          end;
       end;

       if (FError = EC_OK) // If Image format is supported by file format
       then begin
            // Write header to file.
            if (FError = EC_OK)
            then begin
                 try
                   FStream.Write(FPCXHeader, SizeOf(TPCXHeader));
                 except
                   On Exception
                   do FError := EC_WRITETOFILE;
                 end;
            end;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TmcmPCXImage.WriteHeader.


function TmcmPCXImage.WritePalette(Stream : TStream) : TmcmErrorCode;
var PCXPalette : array[0..255] of TRGBTriple;
    PalMarker  : byte;
    i          : word;
begin
  FError := EC_OK;
  try
    PalMarker := 12;
    FStream.Write(PalMarker, SizeOf(byte));

    // Convert B-G-R quad to R-G-B triple palette.
    for i := 0 to 255
    do begin
       with FDibInfo^.bmiColors[i]
       do begin
          PCXPalette[i].rgbtRed   := rgbBlue;
          PCXPalette[i].rgbtGreen := rgbGreen;
          PCXPalette[i].rgbtBlue  := rgbRed;
       end;
    end;

    // Read palette values.
    FStream.Write(PCXPalette, 256 * SizeOf(TRGBTriple));
  except
    On E:Exception
    do FError := EC_WRITETOFILE;
  end;
  Result := FError;
end; // TmcmPCXImage.WritePalette.


procedure TmcmPCXImage.LoadFromStream(Stream : TStream);
var Count      : longword;
    BufferSize : longword;
    DataCount  : longword;
begin
  FDibHandle := 0;
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;

       // Read Header.
       if (ReadHeader(Stream) = EC_OK)
       then begin
            // Load image data.
            if CreateDIB
            then begin
                 try
                   try
                     if (FCompress <> CP_RLE_PCX)
                     then FCompress := CP_RLE_PCX; // The only compression method available!

                     if (FCompress = CP_RLE_PCX)
                     then begin
                          FCompObj := TmcmImagePCXRLE.Create;
                          FCompObj.Image := FDibHandle;
                          FCompObj.FlipVert := True;
                          TmcmImagePCXRLE(FCompObj).SetNoPlanes(FPCXHeader.NPlanes);
                          TmcmImagePCXRLE(FCompObj).SetPackedLineWidth(FPCXHeader.BytesPerLine);
                          FMemStream.Size := 8 * FLongWidth;

                          Count := 0;
                          while (Count < FDibInfo^.bmiHeader.biSizeImage) and (FError = EC_OK)
                          do begin
                             BufferSize := FStream.Read(FMemStream.Memory^, FMemStream.Size);
                             FError := FCompObj.Decompress(FMemStream.Memory, BufferSize, DataCount);
                             FStream.Position := FStream.Position - longint(BufferSize);
                             inc(Count, DataCount);
                             if (DataCount <= 0)
                             then Break;
                          end;

                          // Load and set palette.
                          if (FPCXHeader.Version = 5) and
                             (FPCXHeader.NPlanes = 1) and
                             Not(FPCXHeader.PaletteInfo in [0])
                          then ReadPalette(Stream);
                     end;
                   finally
                     if Assigned(FCompObj)
                     then FCompObj.Free;
                   end;
                 except
                   On Exception
                   do FError := EC_ENDOFFILE;
                 end;
            end;
       end;
       if (Stream.Position <> FStream.Position)
       then Stream.Position := FStream.Position;
  end
  else FError := EC_FILENOTOPEN; // File not open.
end; // TmcmPCXImage.LoadFromStream.


procedure TmcmPCXImage.SaveToStream(Stream : TStream);
var Count         : longint;
    Buffer        : Pointer;
    BufferSize    : longword;
    DataCount     : longword;
begin
  if Assigned(Stream)
  then begin
       FStream.Source := Stream;
       FStream.Mode := RWM_WRITE;
       if GetDibData
       then begin
            // Write file header.
            if (WriteHeader(Stream) = EC_OK)
            then begin
                 if (FError = EC_OK)
                 then begin
                      // Write bitmap data
                      FCompObj := TmcmImagePCXRLE.Create;
                      FCompObj.Image := FDibHandle;
                      FCompObj.FlipVert := True;
                      TmcmImagePCXRLE(FCompObj).SetPackedLineWidth(FPCXHeader.BytesPerLine);

                      Count := 0;
                      DataCount := FDibInfo^.bmiHeader.biSizeImage;

                      while (FError = EC_OK) and (DWORD(Count) < DWORD(FDibInfo^.bmiHeader.biSizeImage))
                      do begin
                         FError := FCompObj.Compress(Buffer, BufferSize, DataCount);
                         if (FError = EC_OK)
                         then begin
                              FStream.Write(Buffer^, BufferSize);
                              inc(Count, DataCount);
                         end;
                      end;
                      if Assigned(FCompObj)
                      then FCompObj.Free;

                      // case FCompress of
                      // end;
                 end;

                 // Write Palette.
                 if (FPCXHeader.Version = 5) and
                    (FPCXHeader.NPlanes = 1)
                 then WritePalette(Stream);
            end;
            FStream.Flush;
       end;
  end
  else FError := EC_FILENOTOPEN;
end; // TmcmPCXImage.SaveToStream.


//------------------------------------------------------------------------------
// TmcmFileFormatItem
//------------------------------------------------------------------------------

function TmcmFileFormatItem.GetFilter : string;
var Ext    : string;
    SubExt : string;
    TmpExt : string;
    i      : integer;
begin
  Ext := FExtension;
  while (Pos(',', Ext) > 0)
  do begin
     i := Pos(',', Ext);
     SubExt := Copy(Ext, 1, i - 1);
     SubExt := SubExt + ';*.';
     TmpExt := SubExt + Copy(Ext, i + 1, Length(Ext));
     Ext := TmpExt;
  end;
  Result := '*.' + Ext;
end; // TmcmFileFormatItem.GetReadFilter.


//------------------------------------------------------------------------------
// TmcmFileFormatsList
//------------------------------------------------------------------------------

constructor TmcmFileFormatsList.Create;
begin
  Inherited Create;
  Add('cri', resCRI, FF_CRI,   TmcmTIFFImage, True, True);
  {$IFDEF mcmDICOM}
  Add('dic,dicom', resDICOM, integer(FF_DICOM), TmcmDICOMImage, True, True);
  {$ENDIF}
  Add('gif', resGIF, FF_GIF,   TmcmGIFImage, True, True);
  {$IFDEF mcmJBIG}
  Add('jbg,jbig', resJBIG, FF_JBIG, TmcmJBIGImage, True, False);
  {$ENDIF}
  {$IFDEF mcmJPEG2K}
  Add('jp2', resJPEG2K, FF_JPG2K, TmcmJPEG2KImage, True, False);
  {$ENDIF}
  Add('jpg,jpeg', resJPEG, FF_JPEG, TmcmJPEGImage, True, True);
  Add('pbm', resPBM, FF_PBM, TmcmPBMImage, True, True);
  Add('pgm', resPGM, FF_PGM, TmcmPGMImage, True, True);
  Add('png', resPNG, FF_PNG, TmcmPNGImage, True, True);
  Add('ppm', resPPM, FF_PPM, TmcmPPMImage, True, True);
  Add('sgi,bw,rgb,rgba', resSGI, FF_SGI, TmcmSGIImage, True, True);
  Add('tif,tiff,fax', resTIFF, FF_TIFF, TmcmTIFFImage, True, True);
  Add('tga,vda,icb,vst', resTarga, FF_TARGA, TmcmTargaImage, True, True);
  Add('bmp,rle', resWBMP, FF_BMP, TmcmBmpImage, True, True);
  Add('dib', resWOS2DIB, FF_DIB, TmcmBmpImage, True, True);
  Add('ico', resWIcon, FF_ICO, TmcmICONImage, True, False);
  Add('pcx', resPCX, FF_PCX, TmcmPCXImage, True, True);
  {$IFDEF MCM_USE_NEF}
  Add('nef', resNEF, FF_NEF, TmcmNEFImage, True, True);
  {$ENDIF}
  {$IFDEF MCMFILEFORMAT}
  Add('mcf', resMCF, FF_MCF, TmcmMCFImage, True, True);
  {$ENDIF}
end; // TmcmFileFormatsList.Create.


destructor TmcmFileFormatsList.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmFileFormatsList.Destroy.


procedure TmcmFileFormatsList.Clear;
var i : integer;
begin
  for i := 0 to (Count - 1)
  do begin
     TmcmFileFormatItem(Items[i]).Free;
  end;
  Inherited Clear;
end; // TmcmFileFormatsList.Clear.


function TmcmFileFormatsList.GetItems(Index : integer) : TmcmFileFormatItem;
begin
  Result := TmcmFileFormatItem(Inherited Items[Index]);
end; // TmcmFileFormatsList.GetItems.


procedure TmcmFileFormatsList.Add(const Ext, Desc : string; ClassID : TmcmFileFormat; AClass : TmcmImageFileClass; CanRead, CanWrite : boolean);
var NewRec : TmcmFileFormatItem;
begin
  NewRec := TmcmFileFormatItem.Create;
  with NewRec
  do begin
     Extension    := AnsiLowerCase(Ext);
     mcmFileClass := AClass;
     FileClassID  := ClassID;
     Description  := Desc;
     ReadEnabled  := CanRead;
     WriteEnabled := CanWrite;
  end;
  inherited Add(NewRec);
end; // TmcmFileFormatsList.Add.


function TmcmFileFormatsList.FindID(ID : TmcmFileFormat) : TmcmFileFormatItem;
var i : integer;
begin
  Result := Nil;
  for i := (Count - 1) downto 0
  do with TmcmFileFormatItem(Items[i])
     do if (FileClassID = ID)
        then begin
             Result := TmcmFileFormatItem(Items[i]);
             Exit;
        end;
end; // TmcmFileFormatsList.FindID.


function TmcmFileFormatsList.FindExt(Ext : string) : TmcmFileFormatItem;
var i : integer;
begin
  Result := Nil;
  Ext := AnsiLowerCase(Ext);
  for i := (Count - 1) downto 0
  do with TmcmFileFormatItem(Items[i])
     do if (Pos(Ext, Extension) > 0)
        then begin
             Result := TmcmFileFormatItem(Items[i]);
             Exit;
        end;
end; // TmcmFileFormatsList.FindExt.


function TmcmFileFormatsList.FindClassName(const Classname : string) : TmcmImageFileClass;
var i : integer;
begin
  for i := (Count - 1) downto 0
  do begin
     Result := TmcmFileFormatItem(Items[i]).mcmFileClass;
     if (Result.ClassName = Classname)
     then Exit;
  end;
  Result := Nil;
end; // TmcmFileFormatsList.FindClassName.


procedure TmcmFileFormatsList.Remove(FileClass : TmcmImageFileClass);
var i      : integer;
    pClass : TmcmFileFormatItem;
begin
  for i := (Count - 1) downto 0
  do begin
     pClass := TmcmFileFormatItem(Items[i]);
     if pClass.mcmFileClass.InheritsFrom(FileClass)
     then begin
          pClass.Free;
          Delete(i);
     end;
  end;
end; // TmcmFileFormatsList.Remove.


procedure TmcmFileFormatsList.BuildFilterStrings(    FileClass    : TmcmImageFileClass;
                                                 var Descriptions : string;
                                                 var Filters      : string;
                                                     ReadFilter   : boolean);
var i, j, c : integer;
    pClass  : TmcmFileFormatItem;
    AllExt  : string;
    FmtExt  : string;
    TmpExt  : string;
begin
  Descriptions := '';
  Filters := '';
  c := 0;
  for i := 0 to (Count - 1)
  do begin
     pClass := TmcmFileFormatItem(Items[i]);
     if pClass.mcmFileClass.InheritsFrom(FileClass) and (pClass.Extension <> '') and
        ((ReadFilter and pClass.ReadEnabled) or (Not(ReadFilter) and pClass.WriteEnabled))
     then begin
          with pClass
          do begin
             AllExt := Extension;
             FmtExt := '';

             // Translate comma separated extensions "abc,def" to "*.abc;*.def"
             while (Length(AllExt) > 0)
             do begin
                j := Pos(',', AllExt);
                if (j > 0)
                then begin
                     FmtExt := FmtExt + ';*.' + Copy(AllExt, 1, j-1);
                     TmpExt := Copy(AllExt, j+1, Length(AllExt));
                     AllExt := TmpExt;
                end
                else begin
                     FmtExt := FmtExt + ';*.' + AllExt;
                     AllExt := '';
                end;
             end;
             TmpExt := Copy(FmtExt, 2, Length(FmtExt));
             FmtExt := TmpExt;

             if (c <> 0)
             then begin
                  Descriptions := Descriptions + '|';
                  Filters := Filters + ';';
             end;
             FmtStr(Descriptions, '%s%s (%s)|%2:s', [Descriptions, Description, FmtExt]);
             FmtStr(Filters, '%s%s', [Filters, FmtExt]);
             inc(c);
          end;
     end;
  end;
  if (c > 1) and ReadFilter
  then FmtStr(Descriptions, '%s|%1:s|%s', [resDetect, Filters, Descriptions]);
end; // TmcmFileFormatsList.BuildFilterStrings.


//------------------------------------------------------------------------------
// TmcmImageFileMgr.
//------------------------------------------------------------------------------

constructor TmcmImageFileMgr.Create;
begin
  Inherited Create;
  InitializeCriticalSection(FCounterLock);
  InitializeCriticalSection(FLock);

  FFormatList   := TmcmFileFormatsList.Create;

  FImageFile    := Nil;
  FIsMulti      := False;
  FCompress     := CP_NOCOMP;
  FQuality      := 100;
  FYCbCrMode    := JYCC_411;
  FSafeBiLevel  := True;
  FImageInfo    := TmcmImageInfo.Create;
  FOnExtFormat  := Nil;
  FError := EC_OK;
end; // TmcmImageFileMgr.Create.


destructor TmcmImageFileMgr.Destroy;
begin
  if Assigned(FImageFile)
  then FImageFile.Free;
  FImageFile := Nil;
  if Assigned(FImageInfo)
  then FImageInfo.Free;

  UnregisterAllFileFormats;
  if Assigned(FFormatList)
  then FFormatList.Free;
  DeleteCriticalSection(FLock);
  DeleteCriticalSection(FCounterLock);
  Inherited Destroy;
end; // TmcmImageFileMgr.Destroy.


procedure TmcmImageFileMgr.Lock;
begin
  EnterCriticalSection(FCounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(FCounterLock);
  EnterCriticalSection(FLock);
end; // TmcmImageFileMgr.Lock.


procedure TmcmImageFileMgr.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(FCounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(FCounterLock);
end; // TmcmImageFileMgr.Unlock.


function TmcmImageFileMgr.DoFileAssociate(Ext         : string;
                                          FileType    : string;
                                          Description : string;
                                          ExeName     : string;
                                          IcoIndex    : integer) : boolean;
var Reg : TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;

      // Assign the extension name to the file type, and create this if extension
      // does not exist.
      // Open / create the key HKEY_CLASSES_ROOT\"Ext"
      Reg.OpenKey(Ext, True);

      // HKEY_CLASSES_ROOT\.Ext\(Default)="FileType"
      Reg.WriteString('', FileType);
      Reg.CloseKey;

      // Assign description of file type, and create the file type if this does
      // not exist.
      // Open / create the key HKEY_CLASSES_ROOT\"FileType"
      Reg.OpenKey(FileType, True);

      // HKEY_CLASSES_ROOT\"FileType"\(Default)="Description"
      Reg.WriteString('', Description);
      Reg.CloseKey;

      // Assign icon index for the file type, and create the key if this does not
      // exist.
      // Open / create the key HKCR\"FileType"\DefaultIcon
      Reg.OpenKey(FileType + '\DefaultIcon', True);

      // HKEY_CLASSES_ROOT\"FileType"\DefaultIcon\
      // (Default)="ExeName", "IcoIndex"
      Reg.WriteString('', ExeName + ',' + IntToStr(IcoIndex));
      Reg.CloseKey;

      // Write text for Open action in Windows Explorer, and create the action
      // if this does not exist.
      // Open / create the key HKCR\"FileType"\Shell\Open
      Reg.OpenKey(FileType + '\Shell\Open', True);

      // HKEY_CLASSES_ROOT\"FileType"\Shell\Open\(Default)="&Open"
      Reg.WriteString('', '&Open');
      Reg.CloseKey;

      // Assign the application that will be used to execute the action, and
      // create the corresponding key if this does not exist.

      // Open / create the key HKEY_CLASSES_ROOT\"FileType"\Shell\Open\Command
      Reg.OpenKey(FileType + '\Shell\Open\Command', True);

      // HKEY_CLASSES_ROOT\"FileType"\Shell\Open\Command\
      // (Default)="ExeName" "%1"
      // Application must scan the command line parameters (ParamCount & ParamStr)
      // to identify which file was passed.
      Reg.WriteString('', '"' + ExeName + '" "%1"');
      Reg.CloseKey;

      // Finally, Windows Explorer should realize we added
      // a file type, we do this by calling SHChangeNotify.
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
      Result := True;
    except
    end;
  finally
    Reg.Free;
  end;
end; // TmcmImageFileMgr.DoFileAssociate.


function TmcmImageFileMgr.RemoveFileAssociate(Ext : string; FileType : string) : boolean;
var Reg     : TRegistry;
    ReadVal : string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.KeyExists(FileType)
      then Reg.DeleteKey(FileType);
      if Reg.KeyExists(Ext)
      then begin
           if Reg.OpenKey(Ext, False)
           then begin
                ReadVal := Reg.ReadString('');
                if (ReadVal = FileType)
                then Reg.DeleteValue('');
                Reg.CloseKey;
           end;
      end;

      // Finally, Windows Explorer should realize we added
      // a file type, we do this by calling SHChangeNotify.
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
      Result := True;
    except
    end;
  finally
    Reg.Free;
  end;
end; // TmcmImageFileMgr.RemoveFileAssociate.


function TmcmImageFileMgr.AssociateFileFormat(FileFormat : TmcmFileFormat;
                                              ExeName    : string;
                                              IcoIndex   : integer;
                                              Add        : boolean) : boolean;
var Extension   : string;
    Extensions  : string;
    TmpStr      : string;
    FileType    : string;
    Description : string;
    i, j        : integer;
    FileItem    : TmcmFileFormatItem;
begin
  Result := True;
  try
    ExeName := LowerCase(ExeName);

    // Get primary extension.
    Extension := GetExtensionFromFormat(FileFormat);
    i := Pos(',', Extension);
    if (i > 0)
    then begin
         TmpStr := Copy(Extension, 1, i - 1);
         Extension := TmpStr;
    end;

    // File class type.
    FileType := LowerCase(ExtractFilename(ExeName));
    TmpStr   := Copy(FileType, 1, Pos('.exe', FileType) - 1) + '.' + Extension;
    FileType := TmpStr;

    FileItem := FFormatList.FindID(FileFormat);
    if (FileItem = Nil)
    then Exit;
    Description := FileItem.Description;

    Extensions := FFormatList.FindID(FileFormat).GetFilter;
    FileItem.GetFilter;
    while (Length(Extensions) > 0)
    do begin
       i := Pos('*', Extensions);
       j := Pos(';', Extensions);
       if (j = 0)
       then j := Length(Extensions) + 1;
       Extension := Copy(Extensions, i + 1, j - i - 1);
       TmpStr := Copy(Extensions, j + 1, Length(Extensions));
       Extensions := TmpStr;

       // Extension must include "." and may not be "*" = all file formats.
       if (Pos('.', Extension) > 0) and (Pos('*', Extension) = 0)
       then begin
            if Add
            then Result := Result and DoFileAssociate(Extension, FileType, Description, ExeName, IcoIndex)
            else Result := Result and RemoveFileAssociate(Extension, FileType);
       end;
    end;
  finally
  end;
end; // TmcmImageFileMgr.AssociateFileFormat.


function TmcmImageFileMgr.IsFileFormatAssociated(FileFormat : TmcmFileFormat;
                                                 ExeName    : string) : boolean;
var Reg      : TRegistry;
    ExtStr   : string;
    TmpStr   : string;
    FileType : string;
    ReadVal  : string;
    i        : integer;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      ExeName := LowerCase(ExeName);

      // Get primary extension.
      ExtStr := GetExtensionFromFormat(FileFormat);
      i := Pos(',', ExtStr);
      if (i > 0)
      then begin
           TmpStr := Copy(ExtStr, 1, i - 1);
           ExtStr := TmpStr;
      end;

      // File class type.
      ExtStr   := '.' + ExtStr;
      FileType := LowerCase(ExtractFilename(ExeName));
      TmpStr   := Copy(FileType, 1, Pos('.exe', FileType) - 1) + ExtStr;
      FileType := TmpStr;

      if Reg.KeyExists(FileType)
      then if Reg.KeyExists(ExtStr)
           then if Reg.OpenKey(ExtStr +'\', False)
                then begin
                     ReadVal := Reg.ReadString('');
                     Reg.CloseKey;
                     Result := (ReadVal = FileType);
                end;
    except
    end;
  finally
    Reg.Free;
  end;
end; // TmcmImageFileMgr.IsFileFormatAssociated.


function TmcmImageFileMgr.GetFormatList : TmcmFileFormatsList;
begin
  Result := FFormatList;
end; // TmcmImageFileMgr.GetFormatList.


function TmcmImageFileMgr.RegisterFileFormat(FileClass : TmcmImageFileClass;
                                             ClassId   : TmcmFileFormat;
                                             Extension, Description : string;
                                             CanRead, CanWrite      : boolean) : integer;
begin
  Result := 0;
  FFormatList.Add(Extension, Description, ClassId, FileClass, CanRead, CanWrite);
end; // TmcmImageFileMgr.RegisterFileFormat.


procedure TmcmImageFileMgr.UnregisterFileFormat(FileClass : TmcmImageFileClass);
begin
  FFormatList.Remove(FileClass);
end; // TmcmImageFileMgr.UnregisterFileFormat.


procedure TmcmImageFileMgr.UnregisterAllFileFormats;
begin
  FFormatList.Clear;
end; // TmcmImageFileMgr.UnregisterAllFileFormats.


function TmcmImageFileMgr.FileFormatToStr(Value : TmcmFileFormat) : string;
var FileItem : TmcmFileFormatItem;
begin
  FileItem := FFormatList.FindID(Value);
  if (FileItem <> Nil)
  then Result := FileItem.Description 
  else Result := '';
end; // TmcmImageFileMgr.FileFormatToStr.


function TmcmImageFileMgr.GetReadFilter : string;
var Filter      : string;
    Description : string;
begin
  FFormatList.BuildFilterStrings(TmcmImageFile, Description, Filter, True);
  Result := Description;
end; // TmcmImageFileMgr.GetReadFilter.


function TmcmImageFileMgr.GetWriteFilter : string;
var Filter      : string;
    Description : string;
begin
  FFormatList.BuildFilterStrings(TmcmImageFile, Description, Filter, False);
  Result := Description;
end; // TmcmImageFileMgr.GetWriteFilter.


function TmcmImageFileMgr.GetImageFileClass(Value : TmcmFileFormat) : TmcmImageFile;
var FileItem : TmcmFileFormatItem;
begin
  if Assigned(FImageFile)
  then FImageFile.Free;
  FImageFile := Nil;

  FileItem := FFormatList.FindID(Value);
  if (FileItem <> Nil)
  then begin
       FImageFile := FileItem.mcmFileClass.Create;
        {$J-}
       case Value of
       FF_JPEG  : begin
                    {$IFDEF mcmJPEGEnginee}
                      TmcmJPEGImage(FImageFile).YCbCrMode := FYCbCrMode;
                    {$ENDIF}
                  end;
       FF_TIFF  : begin
                    TmcmTIFFImage(FImageFile).SafeBiLevel := FSafeBiLevel;
                    {$IFDEF mcmJPEGEnginee}
                      TmcmTIFFImage(FImageFile).YCbCrMode := FYCbCrMode;
                    {$ENDIF}
                  end;
       end;
  end;
  if Assigned(FImageFile)
  then FImageFile.ImageInfo := FImageInfo;
  Result := FImageFile;
end; // TmcmImageFileMgr.GetImageFileClass.


function TmcmImageFileMgr.GetFormatFromName(Value : string) : TmcmFileFormat;
var ExtStr   : string;
    TmpStr   : string;
    Format   : TmcmFileFormat;
    FileItem : TmcmFileFormatItem;
begin
  ExtStr := LowerCase(ExtractFileExt(Value));
  TmpStr := Copy(ExtStr, 2, Length(ExtStr));
  ExtStr := TmpStr;
  Format := FF_NONE;

  FileItem := FFormatList.FindExt(ExtStr);
  if (FileItem <> Nil)
  then Format := TmcmFileFormat(FileItem.FileClassID);

  Result := Format;
end; // TmcmImageFileMgr.GetFormatFromName.


procedure TmcmImageFileMgr.GetFormatsFromColor(ImageFormat : TmcmImageFormat; var FormatList : TmcmFileFormatsList);
var i : integer;
begin
  FormatList.Clear;
  for i := 0 to (FFormatList.Count - 1)
  do begin
     if (ImageFormat in FFormatList.Items[i].FmcmFileClass.GetColorFormats) or (ImageFormat = IF_NONE)
     then with FFormatList.Items[i]
          do FormatList.Add(Extension, Description, FileClassID, mcmFileClass, ReadEnabled, WriteEnabled);
  end;
end; // TmcmImageFileMgr.GetFormatsFromColor.


function TmcmImageFileMgr.GetCompressionFromColor(FileFormat : TmcmFileFormat; ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var FileItem : TmcmFileFormatItem;
begin
  FileItem := FFormatList.FindID(FileFormat);
  if (FileItem <> Nil)
  then Result := FileItem.mcmFileClass.GetCompressionFromColor(ImageFormat, Compress)
  else Result := 0;
end; // TmcmImageFileMgr.GetCompressionFromColor.


function TmcmImageFileMgr.GetCompressionName(FileFormat : TmcmFileFormat; Compression : TmcmCompress) : string;
var FileItem : TmcmFileFormatItem;
begin
  FileItem := FFormatList.FindID(FileFormat);
  if (FileItem <> Nil)
  then Result := FileItem.mcmFileClass.GetCompressionName(Compression)
  else Result := '';
end; // TmcmImageFileMgr.GetCompressionName.


function TmcmImageFileMgr.GetExtensionFromFormat(Value : TmcmFileFormat) : string;
var FileItem : TmcmFileFormatItem;
begin
  Result := '';

  FileItem := FFormatList.FindId(Value);
  if (FileItem <> Nil)
  then Result := FileItem.Extension;
end; // TmcmImageFileMgr.GetExtensionFromFormat.


function TmcmImageFileMgr.GetDefaultExtension(FileFormat  : TmcmFileFormat;
                                              ImageFormat : TmcmImageFormat) : string;
var ExtName : string;
begin
  ExtName := GetExtensionFromFormat(FileFormat);
  // Handle special case where extension is related to the colour resolution.
  case FileFormat of
  FF_SGI : case ImageFormat of
           IF_GREY8  : ExtName := 'bw';
           IF_RGB24  : ExtName := 'rgb';
           IF_RGBA32 : ExtName := 'rgba';
           else ExtName := 'rgb';
           end;
  end;
  if (Pos(',', ExtName) > 0)
  then Result := Copy(ExtName, 1, Pos(',', ExtName) - 1)
  else Result := ExtName;
end; // TmcmImageFileMgr.GetDefaultExtension.

(*
function TmcmImageFileMgr.GetSupportedFormats : TmcmFileFormats;
begin
  Result := FFormatFilter;
end; // TmcmImageFileMgr.GetSupportedFormats.


procedure TmcmImageFileMgr.SetSupportedFormats(Value : TmcmFileFormats);
begin
  FFormatFilter := Value;
end; // TmcmImageFileMgr.SetSupportedFormats.
*)

function TmcmImageFileMgr.GetIsMultiImage : boolean;
begin
  Result := FIsMulti;
end; // TmcmImageFileMgr.GetIsMultiImage.


function TmcmImageFileMgr.VerifyImageFile(FileName : string) : boolean;
begin
  // Tries to determine if the image format is support.
  // This is not an exhaustive verification.
  Result := True;
end; // TmcmImageFileMgr.VerifyImageFile.


function TmcmImageFileMgr.LoadFromStream(Stream : TStream; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
var mcmStream  : TStream;
    StartPos   : longint;
    ImageRead  : HBITMAP;
    SaveFormat : TmcmFileFormat;
begin
  ImageRead := 0;
  if Not(Assigned(Stream))
  then begin
       FError := EC_NOSTREAM;
       Result := 0;
       Exit;
  end;
  mcmStream := TStream(Stream);
  StartPos := mcmStream.Position;
  Lock;
  try
    FImageFile := GetImageFileClass(FileFormat);
    if Assigned(FImageFile)
    then begin
         FImageInfo.Clear;
         FImageFile.LoadFromStream(mcmStream);
         ImageRead   := FImageFile.DibHandle;
         FCompress   := FImageFile.Compression;
         FQuality    := FImageFile.Quality;
         FInterlaced := FImageFile.Interlaced;

         FError := FImageFile.Error;
         FImageFile.Free;
         FImageFile := Nil;
    end;

    if (FileFormat = FF_DETECT)
    then begin
         ImageRead := 0;
         SaveFormat := FileFormat;
         FileFormat := TmcmFileFormat(0);
         FError := EC_BADFORMAT;
         while (ImageRead = 0) and (FileFormat < FF_NONE) and (FError = EC_BADFORMAT)
         do begin
            if (SaveFormat <> FileFormat)
            then begin
                 if (FileFormat < FF_NONE)
                 then FImageFile := GetImageFileClass(FileFormat);
                 if Assigned(FImageFile)
                 then begin
                      mcmStream.Position := StartPos;
                      FImageFile.LoadFromStream(mcmStream);
                      ImageRead   := FImageFile.DibHandle;
                      if (ImageRead <> 0)
                      then begin
                           FCompress   := FImageFile.Compression;
                           FQuality    := FImageFile.Quality;
                           FInterlaced := FImageFile.Interlaced;
                      end;
                      FError := FImageFile.Error;
                 end;
            end;
            if (ImageRead = 0)
            then FileFormat := TmcmFileFormat(word(FileFormat) + 1);
         end;
    end;

    if Assigned(FImageInfo) and (ImageRead <> 0)
    then begin
         FImageInfo.Filename   := '';
         FImageInfo.FileFormat := FileFormat;
         FImageInfo.FileSize   := mcmStream.Position - StartPos;
    end;
  finally
    Unlock;
  end;
  Result := ImageRead;
end; // TmcmImageFileMgr.LoadFromStream.


function TmcmImageFileMgr.LoadImage(Filename : string; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
var ImageRead  : HBITMAP;
    SaveFormat : TmcmFileFormat;
begin
  Lock;
  try
    ImageRead := 0;
    {$IFDEF IMGDEBUG}
      OutputDebugString(PChar(FileName));
    {$ENDIF}

    if FileExists(Filename)
    then begin
         // If FileFormat is IF_DETECT, first guess on the format based on the extension.
         if (FileFormat = FF_DETECT)
         then FileFormat := GetFormatFromName(Filename);

         if (FileFormat < FF_NONE) or (FileFormat > FF_USERDEFINED)
         then FImageFile := GetImageFileClass(FileFormat);

         if Assigned(FImageInfo)
         then FImageInfo.Clear;
         if Assigned(FImageFile)
         then begin
              ImageRead := FImageFile.LoadImage(FileName);
              FError := FImageFile.Error;
         end
         else begin
              // Check format with external file loader.
              {
              if Not(LoadExternalFormat())
              then FError := EC_BADFORMAT;
              }
              FError := EC_BADFORMAT; // We couldn't load image based on extension.
         end;
         {$IFDEF IMGDEBUG}
           if (ImageRead = 0) and (FError <> EC_OK)
           then begin
                OutputDebugString(PChar(FileName));
                OutputDebugString(PChar('Error reading image: ' + CErrorStrings[word(FError)]));
           end;
         {$ENDIF}

         if (ImageRead = 0)
         then begin
              SaveFormat := FileFormat;
              FileFormat := TmcmFileFormat(0);
              while (ImageRead = 0) and (FileFormat < FF_NONE) and (FError = EC_BADFORMAT)
              do begin
                 if (SaveFormat <> FileFormat)
                 then begin
                      if (FileFormat < FF_NONE) or (FileFormat > FF_USERDEFINED)
                      then FImageFile := GetImageFileClass(FileFormat);
                      if Assigned(FImageFile)
                      then begin
                           ImageRead := FImageFile.LoadImage(FileName);
                           FError := FImageFile.Error;
                      end;
                 end;
                 if (ImageRead = 0)
                 then FileFormat := TmcmFileFormat(word(FileFormat) + 1);
              end;
         end;

         if (ImageRead <> 0)
         then begin // Assign storage information.
              FCompress   := FImageFile.Compression;
              FQuality    := FImageFile.Quality;
              FInterlaced := FImageFile.Interlaced;
              if Assigned(FImageInfo)
              then begin
                   FImageInfo.Filename   := Filename;
                   FImageInfo.FileFormat := FileFormat;
              end;

              if Not(FImageFile.NoImages in [0,1])
              then begin
                   FIsMulti := True;
                   FIndex := FImageFile.ImageIndex;
              end
              else FIsMulti := False;
         end
         else FIsMulti := False;
    end
    else FError := EC_OPENFILE;
  finally
    if Assigned(FImageFile)
    then begin
         FError := FImageFile.Error;
         FImageFile.Free;
    end;
    FImageFile := Nil;
    Unlock;
  end;
  Result := ImageRead;
end; // TmcmImageFileMgr.LoadImage.


function TmcmImageFileMgr.ReadImage(Filename : string; FileFormat : TmcmFileFormat) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
begin
  Result := LoadImage(Filename, FileFormat);
end; // TmcmImageFileMgr.ReadImage.


function TmcmImageFileMgr.LoadNext(Filename : string; FileFormat : TmcmFileFormat; Index : integer) : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
var ImageRead : HBITMAP;
begin
  if (Index = -1)
  then inc(FIndex)
  else FIndex := Index;

  Lock;
  try
    ImageRead := 0;

    // If FileFormat is IF_DETECT, first guess on the format based on the extension.
    if (FileFormat = FF_DETECT)
    then FileFormat := GetFormatFromName(Filename);

    if (FileFormat < FF_NONE) or (FileFormat > FF_USERDEFINED)
    then FImageFile := GetImageFileClass(FileFormat);

    if Assigned(FImageFile)
    then begin
         FImageFile.ImageIndex := FIndex;
         ImageRead := FImageFile.LoadImage(FileName);
         FError := FImageFile.Error;
    end
    else FError := EC_BADFORMAT;
    {$IFDEF IMGDEBUG}
      if (ImageRead = 0) and (FError <> EC_OK)
      then begin
           OutputDebugString(PChar(FileName));
           OutputDebugString(PChar('Error reading image: ' + CErrorStrings[word(FError)]));
      end;
    {$ENDIF}

    if (ImageRead <> 0)
    then begin // Assign storage information.
         FCompress   := FImageFile.Compression;
         FQuality    := FImageFile.Quality;
         FInterlaced := FImageFile.Interlaced;
         if Assigned(FImageInfo)
         then begin
              FImageInfo.Filename   := Filename;
              FImageInfo.FileFormat := FileFormat;
         end;
    end;
  finally
    if Assigned(FImageFile)
    then FImageFile.Free;
    FImageFile := Nil;
    (*
    if (FError = EC_NOSUBIMAGE)
    then FError := EC_OK;
    *)
    Unlock;
  end;
  Result := ImageRead;
end; // TmcmImageFileMgr.LoadNext.


procedure TmcmImageFileMgr.SaveImage(Filename : string; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
begin
  if (Bitmap <> 0)
  then begin
       Lock;
       try
         if (FileFormat = FF_DETECT)
         then FileFormat := GetFormatFromName(Filename);
         if (FileFormat < FF_NONE) or (FileFormat > FF_USERDEFINED)
         then FImageFile := GetImageFileClass(FileFormat);
         if Assigned(FImageFile)
         then begin
              FImageFile.Compression := FCompress;
              FImageFile.Quality     := FQuality;
              FImageFile.Interlaced  := FInterlaced;
              FImageFile.SaveImage(FileName, Bitmap);
              if Assigned(FImageInfo)
              then FImageInfo.Clear;
              FError := FImageFile.Error;
              FImageFile.Free;
              FImageFile := Nil;
         end;
       finally
         Unlock;
       end;
  end;
end; // TmcmImageFileMgr.SaveImage.


procedure TmcmImageFileMgr.AppendImage(Filename : string; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
begin
  if (Bitmap <> 0)
  then begin
       Lock;
       try
         if (FileFormat = FF_DETECT)
         then FileFormat := GetFormatFromName(Filename);
         if (FileFormat < FF_NONE) or (FileFormat > FF_USERDEFINED)
         then FImageFile := GetImageFileClass(FileFormat);
         if Assigned(FImageFile)
         then begin
              FImageFile.Compression := FCompress;
              FImageFile.Quality     := FQuality;
              FImageFile.Interlaced  := FInterlaced;
              FImageFile.AppendImage(Filename, Bitmap);
              if Assigned(FImageInfo)
              then FImageInfo.Clear;
              FError := FImageFile.Error;
              FImageFile.Free;
              FImageFile := Nil;
         end;
       finally
         Unlock;
       end;
  end;
end; // TmcmImageFileMgr.AppendImage.


procedure TmcmImageFileMgr.SaveToStream(Stream : TStream; FileFormat : TmcmFileFormat; Bitmap : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
var mcmStream : TStream;
begin
  mcmStream := TStream(Stream);
  if (FileFormat < FF_NONE) and (Bitmap <> 0)
  then begin
       Lock;
       try
         FImageFile := GetImageFileClass(FileFormat);
         if Assigned(FImageFile)
         then begin
              FImageFile.DibHandle := Bitmap;
              FImageFile.Compression := FCompress;
              FImageFile.Quality     := FQuality;
              FImageFile.Interlaced  := FInterlaced;
              FImageFile.SaveToStream(mcmStream);
              if Assigned(FImageInfo)
              then FImageInfo.Clear;
              FError := FImageFile.Error;
              FImageFile.Free;
              FImageFile := Nil;
         end;
       finally
         Unlock;
       end;
  end;
end; // TmcmImageFileMgr.SaveToStream.


function TmcmImageFileMgr.GetImageInfo : TmcmImageInfo;
begin
  Result := FImageInfo;
end; // TmcmImageFileMgr.GetImageInfo.


procedure TmcmImageFileMgr.SetCompress(Value : TmcmCompress);
begin
  FCompress := Value;
end; // TmcmImageFileMgr.SetCompress.


procedure TmcmImageFileMgr.SetQuality(Value : word);
begin
  if (Value < 1)
  then Value := 1;
  if (Value > 100)
  then Value := 100;
  FQuality := Value;
end; // TmcmImageFileMgr.SetQuality.


procedure TmcmImageFileMgr.SetInterlaced(Value : boolean);
begin
  FInterlaced := Value;
end; // TmcmImageFileMgr.SetInterlaced.

{$IFDEF DCB3}
  {$UNDEF DCB3}
{$ENDIF}

{$IFDEF mcmJPEGEnginee}
  {$UNDEF mcmJPEGEnginee}
{$ENDIF}


Initialization
  // Automatically create an instance of TmcmImageFileMgr.
  ImageFileManager := TmcmImageFileMgr.Create;


Finalization
  ImageFileManager.Free;

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
