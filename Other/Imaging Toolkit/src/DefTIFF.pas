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
// $Log:  17519: DefTIFF.pas
//
//    Rev 1.5    03-12-2005 19:57:52  mcm    Version: IMG 2.11
// Added TAG_JPEGTABLES definition.
//
//   Rev 1.4    07-03-2004 14:28:46  mcm    Version: IMG 2.4
// Added NEF tags.

//
//   Rev 1.3    07-02-2004 18:59:22  mcm    Version: IMG 2.4
// Added Exif TAG's and structures.

//
//   Rev 1.2    25-03-2003 21:55:46  mcm    Version: IMG 1.3.3

//
//   Rev 1.1    01-08-2002 11:20:18  mcm    Version: IMG 1.1
// Added Huffman codes for CCITT group 3 & 4 (2-D).

//
//   Rev 1.0    27-05-2002 16:21:50  mcm

unit DefTIFF;

interface

uses mcmImageTypeDef, DefHuffman;

//------------------------------------------------------------------------------
// TIFF Field Type
const
  TAGF_BYTE      = 1;  // Byte            8-bit unsigned integer.
  TAGF_ASCII     = 2;  // ASCII           8-bit bytes storing ASCII codes, the
                       //                 last byte must be null.
  TAGF_SHORT     = 3;  // Short           2-byte unsigned integer (word)
  TAGF_LONG      = 4;  // Long            4-byte unsigned integer
  TAGF_RATIONAL  = 5;  // Rational        Two LONG's, the first represent the
                       //                 numerator, the second the denominator.
  TAGF_SBYTE     = 6;  // Signed Byte     8-bit (1 shortint) unsigned integer.
  TAGF_UNDEFINED = 7;  // Undefined       8-bit (1 byte) that may contain anything.
  TAGF_SSHORT    = 8;  // Signed short    16-bit (1 smallint) signed integer.
  TAGF_SLONG     = 9;  // Signed long     32-bit (1 integer) signed integer.
  TAGF_SRATIONAL = 10; // Signed Rational Two LONG's, the first represent the
                       //                 numerator, the second the denominator.
  TAGF_FLOAT     = 11; // Single          Single pression (4 byte) IEEE format.
  TAGF_DOUBLE    = 12; // Double          Double pression (8 byte) IEEE format.
  TAGF_IDF       = 13; // IDF             An IDF structure (8 bytes).
  //
  // Notes: 1) The entries for image file directories must be
  //           sorted in ascending order by value of the tag.
  //        2) Tags with a value of 8000H or higher are reserved
  //           for user-defined information.
  //

//------------------------------------------------------------------------------
// TIFF Data byte order.
// In the II format, byte order is always from least significant to most
// significant, for both 16-bit and 32-bit integers.
// The MM format is the reversed of the II format.
// The second word in IFH is the version number (Ver) is 42.
// This number verifyes that the file is a TIFF file.
// IFDaddr containes the offset to the first Image File Directory.
  TIFF_BIGENDIAN          = $4D4D;
  TIFF_LITTLEENDIAN       = $4949;

//------------------------------------------------------------------------------
// TIFF Tag Fields
// Tag                               Value            Data           Name
//                                   dec        hex   type
const
  TAG_NEWSUBFILETYPE                = 254;   //  FE   Long           NewSubfileType
  TAG_SUBFILETYPE                   = 255;   //  FF   Short          SubfileType
  TAG_IMAGEWIDTH                    = 256;   // 100   Short or Long  ImageWidth
  TAG_IMAGELENGTH                   = 257;   // 101   Short or Long  ImageLength
  TAG_BITSPERSAMPLE                 = 258;   // 102   Short          BitsPerSample
  TAG_COMPRESSION                   = 259;   // 103   Short          Compression (a basic tag)
  TAG_PHOTOMETRIC                   = 262;   // 106   Short          PhotometricInterpretation
  TAG_THRESHOLDING                  = 263;   // 107   Short          Thresholding
  TAG_CELLWIDTH                     = 264;   // 108   Short          CellWidth
  TAG_CELLLENGTH                    = 265;   // 109   Short          CellLength
  TAG_FILLORDER                     = 266;   // 10A   Short          FillOrder
  TAG_DOCUMENTNAME                  = 269;   // 10D   ASCIIZ         DocumentName
  TAG_IMAGEDESCRIPTION              = 270;   // 10E   ASCIIZ         ImageDescription
  TAG_MAKE                          = 271;   // 10F   ASCIIZ         Make
  TAG_MODEL                         = 272;   // 110   ASCIIZ         Model
  TAG_STRIPOFFSETS                  = 273;   // 111   Short or Long  StripOffsets
  TAG_ORIENTATION                   = 274;   // 112   Short          Orientation
  TAG_SAMPLESPERPIXEL               = 277;   // 115   Short          SamplesPerPixel
  TAG_ROWSPERSTRIP                  = 278;   // 116   Short or Long  RowsPerStrip
  TAG_STRIPBYTECOUNTS               = 279;   // 117   Short or Long  StripByteCounts
  TAG_MINSAMPLEVALUE                = 280;   // 118   Short          MinSampleValue
  TAG_MAXSAMPLEVALUE                = 281;   // 119   Short          MaxSampleValue
  TAG_XRESOLUTION                   = 282;   // 11A   Rational       XResolution
  TAG_YRESOLUTION                   = 283;   // 11B   Rational       YResolution
  TAG_PLANARCONFIG                  = 284;   // 11C   Short          PlanarConfiguration
  TAG_PAGENAME                      = 285;   // 11D   ASCIIZ         PageName
  TAG_XPOSITION                     = 286;   // 11E   Rational       XPosition
  TAG_YPOSITION                     = 287;   // 11F   Rational       YPosition
  TAG_FREEOFFSETS                   = 288;   // 120   Long           FreeOffsets
  TAG_FREEBYTECOUNTS                = 289;   // 121   Long           FreeByteCounts
  TAG_GRAYRESPONSEUNIT              = 290;   // 122   Short          GrayResponseUnit
  TAG_GRAYRESPONSECURVE             = 291;   // 123   Short          GrayResponseCurve
  TAG_T4OPTIONS                     = 292;   // 124   Long           T4Options
  TAG_T6OPTIONS                     = 293;   // 125   Long           T6Options
  TAG_RESOLUTIONUNIT                = 296;   // 128   Short          ResolutionUnit
  TAG_PAGENUMBER                    = 297;   // 129   Short          PageNumber
  TAG_COLORRESPONSECURVES           = 301;   // 12D   Short          ColorResponseCurves
  TAG_TRANSFERFUNCTION              = TAG_COLORRESPONSECURVES;
  TAG_SOFTWARE                      = 305;   // 131   ASCIIZ         Software
  TAG_DATETIME                      = 306;   // 132   ASCIIZ         DateTime
  TAG_ARTIST                        = 315;   // 13B   ASCIIZ         Artist
  TAG_HOSTCOMPUTER                  = 316;   // 13C   ASCIIZ         HostComputer
  TAG_PREDICTOR                     = 317;   // 13D   Short          Predictor
  TAG_WHITEPOINT                    = 318;   // 13E   Rational       WhitePoint
  TAG_PRIMARYCHROMATICITIES         = 319;   // 13F   Rational       PrimaryChromaticities
  TAG_COLORMAP                      = 320;   // 140   Short          ColorMap
  TAG_HALFTONEHINTS                 = 321;   // 141   Short          HalfToneHints
  TAG_TILEWIDTH                     = 322;   // 142   Short or Long  TileWidth
  TAG_TILELENGTH                    = 323;   // 143   Short or Long  TileLength
  TAG_TILEOFFSET                    = 324;   // 144   Long           TileOffset
  TAG_TILEBYTECOUNT                 = 325;   // 145   Short or Long  TileByteCount
  TAG_SUBIDF                        = 330;   // 14A   Long or IFD ( Type 13)
  TAG_INKSET                        = 332;   // 14C   Short          InkSet
  TAG_INKNAME                       = 333;   // 14D   ASCII          InkName
  TAG_NUMBEROFINKS                  = 334;   // 14E   Short          NumberOfInks
  TAG_DOTRANGE                      = 336;   // 150   Byte or Short  DotRange
  TAG_TARGETPRINTER                 = 337;   // 151   ASCII          TargetPrinter
  TAG_EXTRASAMPLES                  = 338;   // 152   Short          ExtraSamples
  TAG_SAMPLEFORMAT                  = 339;   // 153   Short          SampleFormat
  TAG_SMINSAMPLEVALUE               = 340;   // 154   ???            SMinSampleValue
  TAG_SMAXSAMPLEVALUE               = 341;   // 155   ???            SMaxSampleValue
  TAG_TRANSFERRANGE                 = 342;   // 156   Short          TransferRange
  TAG_JPEGTABLES                    = 347;   // 15B   Undefined      JPEGTables
  TAG_JPEGPROC                      = 512;   // 200   Short          JPEGProc
  TAG_JPEGINTERCHANGEFORMAT         = 513;   // 201   Long           JPEGInterchangeFormat
  TAG_JPEGINTERCHANGEFORMATLENGTH   = 514;   // 202   Long           JPEGInterchangeFormatLength
  TAG_JPEGRESTARTINTERVAL           = 515;   // 203   Short          JPEGRestartInterval
  TAG_JPEGLOSSLESSPREDICTORS        = 517;   // 205   Short          JPEGLossLessPredictors
  TAG_JPEGPOINTTRANSFORMS           = 518;   // 206   Short          JPEGPointTransforms
  TAG_JPEGQTABLES                   = 519;   // 207   Long           JPEGQTables
  TAG_JPEGDCTABLES                  = 520;   // 208   Long           JPEGDCTables
  TAG_JPEGACTABLES                  = 521;   // 209   Long           JPEGACTables
  TAG_YCBCRCOEFFICIENTS             = 529;   // 211   Rational       YCbCrCoefficients
  TAG_YCBCRSUBSAMPLING              = 530;   // 212   Short          YCbCrSubSampling
  TAG_YCBCRPOSITIONING              = 531;   // 213   Short          YCbCrPositioning
  TAG_REFERENCEBLACKWHITE           = 532;   // 214   Rational       ReferenceBlackWhite
  TAG_COPYRIGHT                     = 33432; // 8298  ASCII          Copyright

  //----------------------------------------------------------------------------
  // Private TAG's registered to

  // JEITA, Exif
  TAG_EXIF_IFD                      = 34665; // 8769  Long           Pointer to Exif IFD.
  TAG_GPS_IFD                       = 34853; // 8825  Long           Pointer to Exif GPS IFD.
  TAG_INTEROPERABILITY_IFD          = 40965; // A005  Long           Pointer to Exif Interoperability IFD.

  TAG_INTEROPERABILITY_INDEX        = 0;     // 0000  ASCII          Currently "R98".

  // Version tags
  TAG_EXIF_VERSION                  = 36864; // 9000  Undefined      ExifVersion
  TAG_EXIF_FLASHPIXVERSION          = 40960; // A000  UNDEFINED      Supported Flashpix Version
  // Image characteristics
  TAG_EXIF_COLORSPACEINFO           = 40961; // A001  SHORT          Color space information
  // Image configuration
  TAG_EXIF_COMPONENTCONFIGURATION   = 37121; // 9101  UNDEFINED      Meaning of each component
  TAG_EXIF_COMPRESSEDBITSPERPIXEL   = 37122; // 9102  RATIONAL       Image compression mode
  TAG_EXIF_PIXELXDIMENSION          = 40962; // A002  SHORT or LONG  Valid image width
  TAG_EXIF_PIXELYDIMENSION          = 40963; // A003  SHORT or LONG  Valid image height
  // User information
  TAG_EXIF_MAKERNOTE                = 37500; // 927C  UNDEFINED      Manufacturer notes
  TAG_EXIF_USERCOMMENT              = 37510; // 9286  UNDEFINED      User comments
  // Date and time
  TAG_EXIF_RELATEDSOUNDFILE         = 40964; // A004  ASCII          Related audio file
  TAG_EXIF_DATETIMEORIGINAL         = 36867; // 9003  ASCII          Date and time of original data generation
  TAG_EXIF_DATETIMEDIGITIZED        = 36868; // 9004  ASCII          Date and time of digital data generation
  TAG_EXIF_SUBSECTIME               = 37520; // 9290  ASCII          DateTime subseconds
  TAG_EXIF_SUBSECTIMEORIGINAL       = 37521; // 9291  ASCII          DateTimeOriginal subseconds
  TAG_EXIF_SUBSECTIMEDIGITIZED      = 37522; // 9292  ASCII          DateTimeDigitized subseconds
  // Other tags
  TAG_EXIF_IMAGEUNIQUEID            = 42016; // A420  ASCII          Unique image ID

  // Picture-taking condition tags.
  TAG_EXIF_ExposureTime             = 33434; // 829A  RATIONAL       Exposure time
  TAG_EXIF_FNumber                  = 33437; // 829D  RATIONAL       F number
  TAG_EXIF_ExposureProgram          = 34850; // 8822  SHORT          Exposure program
  TAG_EXIF_SpectralSensitivity      = 34852; // 8824  ASCII          Spectral sensitivity
  TAG_EXIF_ISOSpeedRatings          = 34855; // 8827  SHORT          ISO speed rating
  TAG_EXIF_ECF                      = 34856; // 8828  UNDEFINED      Optoelectric conversion factor
  TAG_EXIF_ShutterSpeedValue        = 37377; // 9201  SRATIONAL      Shutter speed
  TAG_EXIF_ApertureValue            = 37378; // 9202  RATIONAL       Aperture
  TAG_EXIF_BrightnessValue          = 37379; // 9203  SRATIONAL      Brightness
  TAG_EXIF_ExposureBiasValue        = 37380; // 9204  SRATIONAL      Exposure bias
  TAG_EXIF_MaxApertureValue         = 37381; // 9205  RATIONAL       Maximum lens aperture
  TAG_EXIF_SubjectDistance          = 37382; // 9206  RATIONAL       Subject distance
  TAG_EXIF_MeteringMode             = 37383; // 9207  SHORT          Metering mode
  TAG_EXIF_LightSource              = 37384; // 9208  SHORT          Light source
  TAG_EXIF_Flash                    = 37385; // 9209  SHORT          Flash
  TAG_EXIF_FocalLength              = 37386; // 920A  RATIONAL       Lens focal length
  TAG_EXIF_SubjectArea              = 37396; // 9214  SHORT          Subject area
  TAG_EXIF_FlashEnergy              = 41483; // A20B  RATIONAL       Flash energy
  TAG_EXIF_SpatialFrequencyResponse = 41484; // A20C  UNDEFINED      Spatial frequency response
  TAG_EXIF_FocalPlaneXResolution    = 41486; // A20E  RATIONAL       Focal plane X resolution
  TAG_EXIF_FocalPlaneYResolution    = 41487; // A20F  RATIONAL       Focal plane Y resolution
  TAG_EXIF_FocalPlaneResolutionUnit = 41488; // A210  SHORT          Focal plane resolution unit
  TAG_EXIF_SubjectLocation          = 41492; // A214  SHORT          Subject location
  TAG_EXIF_ExposureIndex            = 41493; // A215  RATIONAL       Exposure index
  TAG_EXIF_SensingMethod            = 41495; // A217  SHORT          Sensing method
  TAG_EXIF_FileSource               = 41728; // A300  UNDEFINED      File source
  TAG_EXIF_SceneType                = 41729; // A301  UNDEFINED      Scene type
  TAG_EXIF_CFAPattern               = 41730; // A302  UNDEFINED      CFA pattern
  TAG_EXIF_CustomRendered           = 41985; // A401  SHORT          Custom image processing
  TAG_EXIF_ExposureMode             = 41986; // A402  SHORT          Exposure mode
  TAG_EXIF_WhiteBalance             = 41987; // A403  SHORT          White balance
  TAG_EXIF_DigitalZoomRatio         = 41988; // A404  RATIONAL       Digital zoom ratio
  TAG_EXIF_FocalLengthIn35mmFilm    = 41989; // A405  SHORT          Focal length in 35 mm film
  TAG_EXIF_SceneCaptureType         = 41990; // A406  SHORT          Scene capture type
  TAG_EXIF_GainControl              = 41991; // A407  RATIONAL       Gain control
  TAG_EXIF_Contrast                 = 41992; // A408  SHORT          Contrast
  TAG_EXIF_Saturation               = 41993; // A409  SHORT          Saturation
  TAG_EXIF_Sharpness                = 41994; // A40A  SHORT          Sharpness
  TAG_EXIF_DeviceSettingDescription = 41995; // A40B  UNDEFINED      Device settings description
  TAG_EXIF_SubjectDistanceRange     = 41996; // A40C  SHORT          Subject distance range

  //----------------------------------------------------------------------------
  // Private TAG's registered to

  // Nikon,

  TAG_NIKON_CFAPATTERN              = 33422; // 828E
  TAG_NIKON_SENSINGMODE             = 37399; // 9217

(*
  // TAG's allocated for Advent (www.adventimaging.com)
  TAG_ADVENT_SCALE                = 33589; //       ASCII          Scale
  TAG_ADVENT_REVISION             = 33590; //       ASCII          Revision

  // tags 32952-32956 are private tags registered to Island Graphics
  TAG_REFPTS			  = 32953; // image reference points
  TAG_REGIONTACKPOINT		  = 32954; // region-xform tack point
  TAG_REGIONWARPCORNERS	          = 32955; // warp quadrilateral
  TAG_REGIONAFFINE		  = 32956; // affine transformation mat

  // tags 32995-32999 are private tags registered to SGI
  TAG_MATTEING	                  = 32995; // $use ExtraSamples
  TAG_DATATYPE                    = 32996; // $use SampleFormat
  TAG_IMAGEDEPTH                  = 32997; // z depth of image
  TAG_TILEDEPTH	                  = 32998; // z depth/data tile

  // tags 33300-33309 are private tags registered to Pixar
  //
  // TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH
  // are set when an image has been cropped out of a larger image.
  // They reflect the size of the original uncropped image.
  // The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used
  // to determine the position of the smaller image in the larger one.
  TAG_PIXAR_IMAGEFULLWIDTH        = 33300; // full image size in x
  TAG_PIXAR_IMAGEFULLLENGTH       = 33301; // full image size in y

  // Tags 33302-33306 are used to identify special image modes and data
  // used by Pixar's texture formats.
  TAG_PIXAR_TEXTUREFORMAT         = 33302; // texture map format
  TAG_PIXAR_WRAPMODES             = 33303; // s & t wrap modes
  TAG_PIXAR_FOVCOT                = 33304; // cotan(fov) for env. maps
  TAG_PIXAR_MATRIX_WORLDTOSCREEN  = 33305;
  TAG_PIXAR_MATRIX_WORLDTOCAMERA  = 33306;

  // tag 33405 is a private tag registered to Eastman Kodak
  TAG_WRITERSERIALNUMBER          = 33405; // device serial number

  // IPTC TAG from RichTIFF specifications
  TAG_RICHTIFFIPTC                = 33723;

  // 34016-34029 are reserved for ANSI IT8 TIFF/IT
  TAG_STONITS                     = 37439; // Sample value to Nits

  // TAG 34929 is a private tag registered to FedEx
  TAG_FEDEX_EDR                   = 34929; // unknown use

  // TAG 65535 is an undefined tag used by Eastman Kodak
  TAG_DCSHUESHIFTVALUES           = 65535; // Hue shift correction data
*)

//------------------------------------------------------------------------------
// TIFF Compression constants.
const
  TIFCP_NONE          = 1;     // No compression.
  TIFCP_CCITTRLE      = 2;     // CCITT modified Huffman RLE
  TIFCP_CCITTFAX3     = 3;     // CCITT Group 3 fax encoding
  TIFCP_CCITTFAX4     = 4;     // CCITT Group 4 fax encoding
  TIFCP_LZW           = 5;     // Lempel-Ziv & Welch
  TIFCP_OJPEG         = 6;     // 6.0 JPEG (old version)
  TIFCP_JPEG          = 7;     // JPEG DCT compression (new version)
  TIFCP_ADOBE_DEFLATE = 8;     // new id but same as COMPRESSION_DEFLATE
  TIFCP_NEXT          = 32766; // next 2-bit RLE
  TIFCP_CCITTRLEW     = 32771; // modified Huffman with word alignment
  TIFCP_PACKBITS      = 32773; // Macintosh RLE
  TIFCP_THUNDERSCAN   = 32809; // ThunderScan RLE
  TIFCP_NIKON_CFA     = 34713; // ThunderScan RLE

  // codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
  TIFCP_IT8CTPAD      = 32895; // IT8 CT w/padding
  TIFCP_IT8LW         = 32896; // IT8 Linework RLE
  TIFCP_IT8MP         = 32897; // IT8 Monochrome picture
  TIFCP_IT8BL         = 32898; // IT8 Binary line art

  // compression codes 32908-32911 are reserved for Pixar
  TIFCP_PIXARFILM     = 32908; // Pixar companded 10bit LZW
  TIFCP_PIXARLOG      = 32909; // Pixar companded 11bit ZIP
  TIFCP_DEFLATE       = 32946; // Deflate compression (LZ77)

  // compression 32947 is reserved for Oceana Matrix (dev@oceana.com)
  TIFCP_DCS           = 32947; // Kodak DCS encoding
  TIFCP_JBIG          = 34661; // ISO JBIG

  TIFCP_SGILOG	      = 34676; // SGI Log Luminance RLE
  TIFCP_SGILOG24      = 34677; // SGI Log 24-bit packed


//------------------------------------------------------------------------------
// TIFF Planar / Chunky data storage.
  TIFPC_CHUNKY = 1;
  TIFPC_PLANAR = 2;

type
//------------------------------------------------------------------------------
// TIFF, Rational numbers.
//------------------------------------------------------------------------------
  TRational = record
	        Num   : longint;
                Denom : longint;
                ND    : longint;
              end;

//------------------------------------------------------------------------------
// TIFF, Image File Header.
//------------------------------------------------------------------------------
  TIFH = record
           Order   : word;      // Format II = 4949h, MM = 4D4Dh
           Ver     : word;      // Version number = 42 (002Ah).
           IFDaddr : longint;   // Offset of the first Image File
         end;                   // Directory.

//------------------------------------------------------------------------------
// TIFF,
//
// In the II format, byte order is always from least significant to most
// significant, for both 16-bit and 32-bit integers.
// The MM format is the reversed of the II format.
// The second word in IFH is the version number (Ver) is 42.
// This number verifyes that the file is a TIFF file.
// IFDaddr containes the offset to the first Image File Directory.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TIFF, Directory Entry Format.
//------------------------------------------------------------------------------
  TDEFT   = packed record
              Tag       : word;     // Tag for the field.
              FieldType : word;     // Field type.
              Length    : longint;  // Length of the field.
              ValOffset : longword; // Value offset. Contains the
                                    // value, if this is less than
                                    // or equal to four bytes.
	      ValPtr    : word;     // Index pointer to ValRat og ValStr arrays.
              FilePos   : longint;  // Tags position in file. Used during write only.
            end;
  PDEFT   = ^TDEFT;
  TDEF    = array[0..256] of PDEFT;
  TValueR = array[1..50] of ^TRational;
  TValueS = array[1..50] of ^string;

//------------------------------------------------------------------------------
// Exif, structures.
//------------------------------------------------------------------------------
type
  TExif_OECF = record
  Cols     : word;
  Rows     : word;
  ItemName : PMatrixB;
  ValuesN  : PMatrixL;
  ValuesD  : PMatrixL;
  end;
  PExif_OECF = ^TExif_OECF;

  TExif_FreqResponse = record
  Cols       : word;
  Rows       : word;
  ItemName   : PMatrixB;
  SFRValuesN : PMatrixL;
  SFRValuesD : PMatrixL;
  end;
  PExif_FreqResponse = ^TExif_FreqResponse;

  TExif_CFAPattern = record
  Cols      : word;
  Rows      : word;
  CFAValues : PVectorB;
  end;
  PExif_CFAPattern = ^TExif_CFAPattern;

//------------------------------------------------------------------------------
// TIFF, T.4/6 Two-Dimensional direction codes.
//------------------------------------------------------------------------------
const
  ModeTermCodes : array[0..11] of
                  THuffmanCodeLen = ((Len :  4; Code : $1000),  // Pass
                                     (Len :  3; Code : $2000),  // Horizontal
                                     (Len :  1; Code : $8000),  // Vertical 0
                                     (Len :  3; Code : $6000),  // Vertical right 1
                                     (Len :  6; Code : $0C00),  // Vertical right 2
                                     (Len :  7; Code : $0600),  // Vertical right 3
                                     (Len :  3; Code : $4000),  // Vertical left 1
                                     (Len :  6; Code : $0800),  // Vertical left 2
                                     (Len :  7; Code : $0400),  // Vertical left 3
                                     (Len :  7; Code : $0200),  // 2-D Extension
                                     (Len :  9; Code : $0080),  // 1-D Extension
                                     (Len : 12; Code : $0010)); // EOL
//------------------------------------------------------------------------------
// TIFF, T.4 Terminating codes - White.
//------------------------------------------------------------------------------
const
  WhiteTermCodes : array[0..63] of
                  THuffmanCodeLen = ((Len :  8; Code : $3500),  // 0
                                     (Len :  6; Code : $1C00),  // 1
                                     (Len :  4; Code : $7000),  // 2
                                     (Len :  4; Code : $8000),  // 3
                                     (Len :  4; Code : $B000),  // 4
                                     (Len :  4; Code : $C000),  // 5
                                     (Len :  4; Code : $E000),  // 6
                                     (Len :  4; Code : $F000),  // 7
                                     (Len :  5; Code : $9800),  // 8
                                     (Len :  5; Code : $A000),  // 9
                                     (Len :  5; Code : $3800),  // 10
                                     (Len :  5; Code : $4000),  // 11
                                     (Len :  6; Code : $2000),  // 12
                                     (Len :  6; Code : $0C00),  // 13
                                     (Len :  6; Code : $D000),  // 14
                                     (Len :  6; Code : $D400),  // 15
                                     (Len :  6; Code : $A800),  // 16
                                     (Len :  6; Code : $AC00),  // 17
                                     (Len :  7; Code : $4E00),  // 18
                                     (Len :  7; Code : $1800),  // 19
                                     (Len :  7; Code : $1000),  // 20
                                     (Len :  7; Code : $2E00),  // 21
                                     (Len :  7; Code : $0600),  // 22
                                     (Len :  7; Code : $0800),  // 23
                                     (Len :  7; Code : $5000),  // 24
                                     (Len :  7; Code : $5600),  // 25
                                     (Len :  7; Code : $2600),  // 26
                                     (Len :  7; Code : $4800),  // 27
                                     (Len :  7; Code : $3000),  // 28
                                     (Len :  8; Code : $0200),  // 29
                                     (Len :  8; Code : $0300),  // 30
                                     (Len :  8; Code : $1A00),  // 31
                                     (Len :  8; Code : $1B00),  // 32
                                     (Len :  8; Code : $1200),  // 33
                                     (Len :  8; Code : $1300),  // 34
                                     (Len :  8; Code : $1400),  // 35
                                     (Len :  8; Code : $1500),  // 36
                                     (Len :  8; Code : $1600),  // 37
                                     (Len :  8; Code : $1700),  // 38
                                     (Len :  8; Code : $2800),  // 39
                                     (Len :  8; Code : $2900),  // 40
                                     (Len :  8; Code : $2A00),  // 41
                                     (Len :  8; Code : $2B00),  // 42
                                     (Len :  8; Code : $2C00),  // 43
                                     (Len :  8; Code : $2D00),  // 44
                                     (Len :  8; Code : $0400),  // 45
                                     (Len :  8; Code : $0500),  // 46
                                     (Len :  8; Code : $0A00),  // 47
                                     (Len :  8; Code : $0B00),  // 48
                                     (Len :  8; Code : $5200),  // 49
                                     (Len :  8; Code : $5300),  // 50
                                     (Len :  8; Code : $5400),  // 51
                                     (Len :  8; Code : $5500),  // 52
                                     (Len :  8; Code : $2400),  // 53
                                     (Len :  8; Code : $2500),  // 54
                                     (Len :  8; Code : $5800),  // 55
                                     (Len :  8; Code : $5900),  // 56
                                     (Len :  8; Code : $5A00),  // 57
                                     (Len :  8; Code : $5B00),  // 58
                                     (Len :  8; Code : $4A00),  // 59
                                     (Len :  8; Code : $4B00),  // 60
                                     (Len :  8; Code : $3200),  // 61
                                     (Len :  8; Code : $3300),  // 62
                                     (Len :  8; Code : $3400)); // 63

//------------------------------------------------------------------------------
// TIFF, T.4 Terminating codes - Black.
//------------------------------------------------------------------------------
const
  BlackTermCodes : array[0..63] of
                  THuffmanCodeLen = ((Len : 10; Code : $0DC0),  // 0
                                     (Len :  3; Code : $4000),  // 1
                                     (Len :  2; Code : $C000),  // 2
                                     (Len :  2; Code : $8000),  // 3
                                     (Len :  3; Code : $6000),  // 4
                                     (Len :  4; Code : $3000),  // 5
                                     (Len :  4; Code : $2000),  // 6
                                     (Len :  5; Code : $1800),  // 7
                                     (Len :  6; Code : $1400),  // 8
                                     (Len :  6; Code : $1000),  // 9
                                     (Len :  7; Code : $0800),  // 10
                                     (Len :  7; Code : $0A00),  // 11
                                     (Len :  7; Code : $0E00),  // 12
                                     (Len :  8; Code : $0400),  // 13
                                     (Len :  8; Code : $0700),  // 14
                                     (Len :  9; Code : $0C00),  // 15
                                     (Len : 10; Code : $05C0),  // 16
                                     (Len : 10; Code : $0600),  // 17
                                     (Len : 10; Code : $0200),  // 18
                                     (Len : 11; Code : $0CE0),  // 19
                                     (Len : 11; Code : $0D00),  // 20
                                     (Len : 11; Code : $0D80),  // 21
                                     (Len : 11; Code : $06E0),  // 22
                                     (Len : 11; Code : $0500),  // 23
                                     (Len : 11; Code : $02E0),  // 24
                                     (Len : 11; Code : $0300),  // 25
                                     (Len : 12; Code : $0CA0),  // 26
                                     (Len : 12; Code : $0CB0),  // 27
                                     (Len : 12; Code : $0CC0),  // 28
                                     (Len : 12; Code : $0CD0),  // 29
                                     (Len : 12; Code : $0680),  // 30
                                     (Len : 12; Code : $0690),  // 31
                                     (Len : 12; Code : $06A0),  // 32
                                     (Len : 12; Code : $06B0),  // 33
                                     (Len : 12; Code : $0D20),  // 34
                                     (Len : 12; Code : $0D30),  // 35
                                     (Len : 12; Code : $0D40),  // 36
                                     (Len : 12; Code : $0D50),  // 37
                                     (Len : 12; Code : $0D60),  // 38
                                     (Len : 12; Code : $0D70),  // 39
                                     (Len : 12; Code : $06C0),  // 40
                                     (Len : 12; Code : $06D0),  // 41
                                     (Len : 12; Code : $0DA0),  // 42
                                     (Len : 12; Code : $0DB0),  // 43
                                     (Len : 12; Code : $0540),  // 44
                                     (Len : 12; Code : $0550),  // 45
                                     (Len : 12; Code : $0560),  // 46
                                     (Len : 12; Code : $0570),  // 47
                                     (Len : 12; Code : $0640),  // 48
                                     (Len : 12; Code : $0650),  // 49
                                     (Len : 12; Code : $0520),  // 50
                                     (Len : 12; Code : $0530),  // 51
                                     (Len : 12; Code : $0240),  // 52
                                     (Len : 12; Code : $0370),  // 53
                                     (Len : 12; Code : $0380),  // 54
                                     (Len : 12; Code : $0270),  // 55
                                     (Len : 12; Code : $0280),  // 56
                                     (Len : 12; Code : $0580),  // 57
                                     (Len : 12; Code : $0590),  // 58
                                     (Len : 12; Code : $02B0),  // 59
                                     (Len : 12; Code : $02C0),  // 60
                                     (Len : 12; Code : $05A0),  // 61
                                     (Len : 12; Code : $0660),  // 62
                                     (Len : 12; Code : $0670)); // 63

//------------------------------------------------------------------------------
// TIFF, T.4 Make-up codes - White.
//------------------------------------------------------------------------------
const
  WhiteMakeUp    : array[0..27] of
                  THuffmanCodeLen = ((Len :  5; Code : $D800),  // 64
                                     (Len :  5; Code : $9000),  // 128
                                     (Len :  6; Code : $5C00),  // 192
                                     (Len :  7; Code : $6E00),  // 256
                                     (Len :  8; Code : $3600),  // 320
                                     (Len :  8; Code : $3700),  // 384
                                     (Len :  8; Code : $6400),  // 448
                                     (Len :  8; Code : $6500),  // 512
                                     (Len :  8; Code : $6800),  // 576
                                     (Len :  8; Code : $6700),  // 640
                                     (Len :  9; Code : $6600),  // 704
                                     (Len :  9; Code : $6680),  // 768
                                     (Len :  9; Code : $6900),  // 832
                                     (Len :  9; Code : $6980),  // 896
                                     (Len :  9; Code : $6A00),  // 960
                                     (Len :  9; Code : $6A80),  // 1024
                                     (Len :  9; Code : $6B00),  // 1088
                                     (Len :  9; Code : $6B80),  // 1152
                                     (Len :  9; Code : $6C00),  // 1216
                                     (Len :  9; Code : $6C80),  // 1280
                                     (Len :  9; Code : $6D00),  // 1344
                                     (Len :  9; Code : $6D80),  // 1408
                                     (Len :  9; Code : $4C00),  // 1472
                                     (Len :  9; Code : $4C80),  // 1536
                                     (Len :  9; Code : $4D00),  // 1600
                                     (Len :  6; Code : $6000),  // 1664
                                     (Len :  9; Code : $4D80),  // 1728
                                     (Len : 12; Code : $0010)); // EOL

//------------------------------------------------------------------------------
// TIFF, T.4 Make-up codes - Black.
//------------------------------------------------------------------------------
const
  BlackMakeUp    : array[0..27] of
                  THuffmanCodeLen = ((Len : 10; Code : $03C0),  // 64
                                     (Len : 12; Code : $0C80),  // 128
                                     (Len : 12; Code : $0C90),  // 192
                                     (Len : 12; Code : $05B0),  // 256
                                     (Len : 12; Code : $0330),  // 320
                                     (Len : 12; Code : $0340),  // 384
                                     (Len : 12; Code : $0350),  // 448
                                     (Len : 13; Code : $0360),  // 512
                                     (Len : 13; Code : $0368),  // 576
                                     (Len : 13; Code : $0250),  // 640
                                     (Len : 13; Code : $0258),  // 704
                                     (Len : 13; Code : $0260),  // 768
                                     (Len : 13; Code : $0268),  // 832
                                     (Len : 13; Code : $0390),  // 896
                                     (Len : 13; Code : $0398),  // 960
                                     (Len : 13; Code : $03A0),  // 1024
                                     (Len : 13; Code : $03A8),  // 1088
                                     (Len : 13; Code : $03B0),  // 1152
                                     (Len : 13; Code : $03B8),  // 1216
                                     (Len : 13; Code : $0290),  // 1280
                                     (Len : 13; Code : $0298),  // 1344
                                     (Len : 13; Code : $02A0),  // 1408
                                     (Len : 13; Code : $02A8),  // 1472
                                     (Len : 13; Code : $02D0),  // 1536
                                     (Len : 13; Code : $02D8),  // 1600
                                     (Len : 13; Code : $0320),  // 1664
                                     (Len : 13; Code : $0328),  // 1728
                                     (Len : 11; Code : $0000)); // EOL


//------------------------------------------------------------------------------
// TIFF, T.4 Make-up codes - White & Black.
//------------------------------------------------------------------------------
const
  WandBMakeUp    : array[0..12] of
                  THuffmanCodeLen = ((Len : 11; Code : $0100),  // 1792
                                     (Len : 11; Code : $0180),  // 1856
                                     (Len : 11; Code : $01A0),  // 1920
                                     (Len : 12; Code : $0120),  // 1984
                                     (Len : 12; Code : $0130),  // 2048
                                     (Len : 12; Code : $0140),  // 2112
                                     (Len : 12; Code : $0150),  // 2176
                                     (Len : 12; Code : $0160),  // 2240
                                     (Len : 12; Code : $0170),  // 2304
                                     (Len : 12; Code : $01C0),  // 2368
                                     (Len : 12; Code : $01D0),  // 2432
                                     (Len : 12; Code : $01E0),  // 2496
                                     (Len : 12; Code : $01F0)); // 2560


implementation

end.
