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
// $Log:  17541: mcmImage.pas 
//
//    Rev 1.58    2014-02-02 21:09:56  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.57    25-10-2009 17:25:36  mcm
// Support for Delphi 2010
//
//    Rev 1.56    31-08-2009 23:58:34  mcm
// Demo edition
//
//    Rev 1.55    26-08-2009 23:04:24  mcm    Version: IMG 3.2
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.54    11-08-2009 09:56:42  mcm    Version: IMG 3.3
//
//    Rev 1.53    03-01-2009 17:41:40  mcm
// Changed MaxBandSize to 4MB
//
//    Rev 1.52    20-08-2007 20:28:22  mcm
// Added support for Delphi 2007
//
//    Rev 1.51    17-10-2006 09:18:16  mcm    Version: IMG 3.1
// Added FilePageOpen on TmcmImage.
// Corrected displaying down-scaled images.
// Added OnPainted event on TmcmImageCtrl
//
//    Rev 1.50    01-07-2006 11:35:38  mcm    Version: IMG 3.0
// Enabled displaying "fax" images in correct dimension by using the horizontal
// and vertical resolution to scale the vertical direction additionally.
// Added two properties to obtain the scaled display height and width.
//
//    Rev 1.49    05-06-2006 22:30:30  mcm    Version: IMG 3.0
// Added TmcmImage.Error property.
//
//    Rev 1.48    21-05-2006 21:06:48  mcm
// Modified setting ImageFormat to accept IF_GREY16.
//
//    Rev 1.47    16-05-2006 20:53:12  mcm
// FileOpen, FileSave, FileAppend, LoadFromStream and SaveToStream receives the
// Error status from TmcmImageFileMgr (ImageFileManager instance).
//
//    Rev 1.46    09-05-2006 21:54:04  mcm
// Corrected AutoSizing when the image/scaling changes.
//
//    Rev 1.45    24-04-2006 20:38:40  mcm
// SetAutoSize override excluded for Delphi 3 - 5.
//
//    Rev 1.44    20-04-2006 21:27:06  mcm
// Added property AutoSize on TmcmImageCtrl.
//
//    Rev 1.43    04-03-2006 18:21:52  mcm    Version: IMG 2.16
// Modified TmcmImage.PastRegion to support pasting an Source image partly
// outside the target images boundaries.
//
//    Rev 1.42    22/02/2006 00:08:20  mcm
// Made method SwapRBValues public.
//
//    Rev 1.41    18-02-2006 20:02:34  mcm
// Modified FillRGB to support Alpha channel in 32 bit images.
// Improved speed of SetPixel when used with 24 and 32 bit images.
//
//    Rev 1.40    29-01-2006 20:10:44  mcm    Version: IMG 2.14
// Delphi 4, removed un-used variable.
//
//    Rev 1.39    28-01-2006 22:46:16  mcm    Version: IMG 2.14
// Added Alpha blending using 32 bit images.
//
//    Rev 1.38    22-12-2005 20:52:14  mcm    Version: IMG 2.12
// Delphi 2006 support.
//
//    Rev 1.37    22/11/2005 18:30:10  mcm    Version: IMG 2.10
// Added GetStretchMode method.
//
//    Rev 1.36    15/11/2005 21:42:56  mcm    Version: IMG 2.10
// Modified TmcmImage.CopyFormat to copy the ImageFormat property.
//
//    Rev 1.35    12-10-2005 23:25:18  mcm
// Modified GetPalette to eliminate resource leak.
//
//   Rev 1.34    21-08-2005 09:10:04  mcm    Version: IMG 2.9
// Added missing initialisation of FDragMode.

//
//   Rev 1.33    04-08-2005 18:47:06  mcm    Version: IMG 2.9
// TmcmImageCtrl, override methods WndProc and SetDragMode to support drag &
// drop.

//
//   Rev 1.32    24-07-2005 18:58:40  mcm    Version: IMG 2.9
// Modified copy & paste methods, HBITMAP.

//
//   Rev 1.31    11-06-2005 09:55:14  mcm    Version: IMG 2.9
// Added Mirror and Orientation properties on TmcmImageInfo that reflects the
// TIFF orientation TAG.

//
//   Rev 1.30    23-05-2005 21:39:42  mcm    Version: IMG 2.9
// Corrected an initialisation error of FImageFormat.

//
//   Rev 1.29    20-02-2005 13:25:06  mcm
// Removed un-used code in TmcmImage.Draw method.

//
//   Rev 1.28    03-02-2005 21:15:02  mcm
// Re-modified FillRGB.

//
//   Rev 1.27    20-01-2005 22:23:14  mcm
// Modified FillRGB to support BW images.

//
//   Rev 1.26    03-01-2005 18:32:10  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.25    27-10-2004 22:20:48  mcm    Version: IMG 2.6
// Added property methods to support C++Builder.

//
//   Rev 1.24    02-09-2004 21:36:12  mcm    Version: IMG 2.6
// Added TmcmImageMaxIntScale which returns the maximum scale factor supported
// when running on any Windows from 95 through XP ... compiled using Delphi 3
// through 7.
// Modified TmcmImage.Draw to increase the supported Scale factor (Zoom in).

//
//   Rev 1.23    25-06-2004 20:41:00  mcm    Version: IMG 2.5
// Included additional properties inherited from TmcmImageCtrl's ancestor
// TControl, herunder Anchors, Constaints, Drag.. etc.
// Improved handling resolution information between the TmcmImage and
// TmcmImageInfo classes.
// Improved copy from clipboard, when the bitmap is stored using the
// BI_BITFIELDS format.
// Fixed background filling of the right-most column and last rowTmcmImageCtrl.

//
//   Rev 1.22    14-03-2004 09:10:42  mcm    Version: IMG 2.4
// Included TmcmImageMetaInfo, TmcmImageExifInfo and TmcmImageGPSInfo, to handle
// Exif and GPS information in images.

//
//   Rev 1.21    30-01-2004 20:13:08  mcm    Version: IMG 2.3
// Extended Border styles on TmcmImageCtrl.

//
//   Rev 1.20    10-12-2003 15:33:00  mcm
// Added OnMouseWheel event.
// Corrected R & B color entry being swapped in GetPaletteEntry.

//
//   Rev 1.19    30-11-2003 01:39:28  mcm    Version: IMG 2.1
// Fixed SaveToStream setting compression to CP_NONE and palette.

//
//   Rev 1.18    24-11-2003 20:28:16  mcm
// Added LoadFromStreamEx and SaveToStreamEx. Both allows specifying the file
// format to use when storing image data.

//
//   Rev 1.17    17-11-2003 10:23:06  mcm    Version: IMG 2.0
// Corrected painting background defined colors.

//
//   Rev 1.16    06-11-2003 17:54:42  mcm
// Internal revision - TmcmImageCtrl

//
//   Rev 1.15    25-09-2003 23:30:32  mcm    Version: IMG 1.5
// Added FillRGB and GetNearestPaletteIndex to TmcmImage.

//
//   Rev 1.14    29-08-2003 11:12:44  mcm
// Corrected copying and pasting 1 & 4 bit images in CopyRegion and PasteRegion.

//
//   Rev 1.13    25-07-2003 09:38:46  mcm
// Corrected creation of 4 bit grey palette in CreateGreyPalette.

//
//   Rev 1.12    25-07-2003 00:21:22  mcm
// Fixed swap R-B error in palette images when calling CopyFormat and SetPalette.

//
//   Rev 1.11    06-07-2003 10:45:38  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.10    09-06-2003 22:39:24  mcm    Version: IMG 1.3.4
// TmcmMultiImageCtrl, Added Count property.

//
//   Rev 1.9    05-06-2003 21:52:30  mcm    Version: IMG 1.3.4
// Added the TmcmMultiImageCtrl component that handles N TmcmImage's, one of
// which is selected to be displayed.

//
//   Rev 1.8    26-03-2003 09:07:30  mcm    Version: IMG 1.3.3
// Added FileAppend that appends the image to a multiple image file (currently
// TIFF).

//
//   Rev 1.7    11-03-2003 00:07:16  mcm    Version: IMG 1.3.2
// Modified DrawImage to provide a less flickering screen update.
// Moved border drawing to buttom of Paint method.

//
//   Rev 1.6    05-02-03 16:16:34  mcm
// Demo protection.

//
//   Rev 1.5    27-01-2003 13:58:50  mcm
// Added PastRegion, past an image into specified region. 
// Modified ScanLine (removed call to FreeContext, speed up access to image
// lines).
// Fixed update problem in DrawImage.

//
//   Rev 1.4    20-08-2002 11:18:46  mcm
// Fixed swapping Red and Blue palette entries in CopyRegion.

//
//   Rev 1.3    07-08-2002 14:17:46  mcm    Version: IMG 1.1
// Added SetStretchMode. Changes how images are displayed when stretched (ref.
// SetStretchBltMode).

//
//   Rev 1.2    01-08-2002 11:27:16  mcm    Version: IMG 1.1
// Fixed error in (Get) ScanLine.

//
//   Rev 1.1    14-06-2002 15:52:54  mcm
// Added FlipY. This property only influence the way the bitmap is displayed.

//
//   Rev 1.0    27-05-2002 16:21:58  mcm

unit mcmImage;

interface

{$Include 'mcmDefines.pas'}
{ $DEFINE mcmGDIPlus}

uses {$IFNDEF GE_DXE2}
      Classes, Controls, Graphics, Windows, Messages,
     {$ELSE}
      System.Classes, WinApi.Windows, WinApi.Messages, Vcl.Controls, Vcl.Graphics,
     {$ENDIF}
     {$IFDEF mcmGDIPlus} mcmGdiPlusFlat2, {$ENDIF}
     mcmImageTypeDef;

type
{$IFDEF DCB3}
  TWMMouseWheel = packed record
  Msg : Cardinal;
  Keys : SmallInt;
  WheelDelta : SmallInt;
  case integer of
  0: (XPos : Smallint;
      YPos : Smallint);
  1: (Pos : TSmallPoint;
      Result : Longint);
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TmcmImageMetaInfo, base class for Exif and GPS metadata.
//------------------------------------------------------------------------------
  TmcmImageMetaInfo = class(TPersistent)
  private
  protected
    function    ConvertToDouble(Value : array of integer) : double;
    procedure   DoubleToFraction(Value : double; var N, D : longint);
  public
  published
  end;

//------------------------------------------------------------------------------
// TmcmImageExifInfo
//
// The TmcmImageExifInfo is accessed via the Exif proeprty on TmcmImageInfo class.
// If this property is Nil, the image file did not contain Exif information.
//------------------------------------------------------------------------------
  TmcmImageExifInfo = class(TmcmImageMetaInfo)
  private
  protected
    FModified                 : boolean;       // Is this objects data modified!
    // Version info.
    FExifVersion              : integer;
    FFlashPixVersion          : integer;
    // Color space info.
    FColorSpace               : word;
    // Image configuration.
    FXDimension               : longint;
    FYDimension               : longint;
    FComponentConfig          : array[0..3] of byte;
    FCompressdBitPerPixel     : array[0..1] of longint;
    // User information.
    FMakerNote                : string;
    FUserComment              : string;
    // Date & Time
    FDateTimeOriginal         : TDateTime;
    FDateTimeDigitized        : TDateTime;
    FSubSecTime               : integer;
    FSubSecTimeOriginal       : integer;
    FSubSecTimeDigitized      : integer;
    // Other information.
    FUniqueID                 : string;

    // Picture taking conditions
    FExposureTime             : array[0..1] of longint;
    FFNumber                  : array[0..1] of longint;
    FExposureProgram          : integer;
    FSpectralSensitivity      : string;
    FISOSpeedRating           : word;
    FOECF_Size                : integer;
    FOECF                     : PVectorB;
    FShutterSpeed             : array[0..1] of longint;
    FAperture                 : array[0..1] of longint;
    FBrightness               : array[0..1] of longint;
    FExposureBias             : array[0..1] of longint;
    FMaxAperture              : array[0..1] of longint;
    FSubjectDistance          : array[0..1] of longint;
    FMeteringMode             : word;
    FLightSource              : word;
    FFlash                    : word;
    FFocalLength              : array[0..1] of longint;
    FSubjectAreaLength        : word;          
    FSubjectArea              : array[0..3] of word;
    FFlashEnergy              : array[0..1] of longint;
    FSFR_Size                 : integer;
    FFrequencyResponse        : PVectorB;
    FFocalPlaneXResolution    : array[0..1] of longint;
    FFocalPlaneYResolution    : array[0..1] of longint;
    FFocalPlaneResolutionUnit : word;
    FSubjectLocation          : array[0..1] of word;
    FExposureIndex            : array[0..1] of longint;
    FSensingMethod            : word;
    FFileSource               : integer;
    FSceneType                : integer;
    FCFA_Size                 : integer;
    FCFAPattern               : PVectorB;
    FCustomRendered           : word;
    FExposureMode             : word;
    FWhiteBalance             : word;
    FDigitalZoomRatio         : array[0..1] of longint;
    FFocalLengthIn35mmFilm    : word;
    FSceneCaptureType         : word;
    FGainControl              : word;
    FContrast                 : word;
    FSaturation               : word;
    FSharpness                : word;
    FDSD_Size                 : integer;
    FDeviceSettingDescription : PVectorB;
    FSubjectDistanceRange     : word;

    // Interoperability
    FInteroperabilityIndex    : string;
    FInteroperabilityVersion  : integer;

    function    GetCompressdBitPerPixel : double;
    function    GetComponentConfig(Index : word) : byte;
    function    GetExposureTime : double;
    function    GetFNumber : double;
    function    GetShutterSpeed : double;
    function    GetAperture : double;
    function    GetBrightness : double;
    function    GetExposureBias : double;
    function    GetMaxAperture : double;
    function    GetSubjectDistance : double;
    function    GetFocalLength : double;
    function    GetFlashEnergy : double;
    function    GetFocalPlaneXResolution : double;
    function    GetFocalPlaneYResolution : double;
    function    GetExposureIndex : double;
    function    GetDigitalZoomRatio : double;
    function    GetSubjectArea(Index : word) : word;
    function    GetSubjectLocation(Index : word) : word;
    function    GetUserComment : WideString;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Assign(Source : TPersistent); override;
    procedure   Clear;

    property    ExifVersion : integer
      read      FExifVersion;
    property    FlashPixVersion : integer
      read      FFlashPixVersion;
    property    ColorSpace : word
      read      FColorSpace;
    property    XDimension : longint
      read      FXDimension;
    property    YDimension : longint
      read      FYDimension;
    property    ComponentConfig[Index : word] : byte
      read      GetComponentConfig;
    property    CompressdBitPerPixel : double
      read      GetCompressdBitPerPixel;
    property    MakerNote : string
      read      FMakerNote;
    property    UserComment : Widestring
      read      GetUserComment;
    property    DateTimeOriginal : TDateTime
      read      FDateTimeOriginal;
    property    DateTimeDigitized : TDateTime
      read      FDateTimeDigitized;
    property    SubSecTime : integer
      read      FSubSecTime;
    property    SubSecTimeOriginal : integer
      read      FSubSecTimeOriginal;
    property    SubSecTimeDigitized : integer
      read      FSubSecTimeDigitized;
    property    UniqueID : string
      read      FUniqueID;
    property    ExposureTime : double
      read      GetExposureTime;
    property    FNumber : double
      read      GetFNumber;
    property    ExposureProgram : integer
      read      FExposureProgram;
    property    SpectralSensitivity : string
      read      FSpectralSensitivity;
    property    ISOSpeedRating : word
      read      FISOSpeedRating;
    //    FOECF                     : string; // Use SetLength and copy as were it bytes in an array.
    property    ShutterSpeed : double
      read      GetShutterSpeed;
    property    Aperture : double
      read      GetAperture;
    property    Brightness : double
      read      GetBrightness;
    property    ExposureBias : double
      read      GetExposureBias;
    property    MaxAperture : double
      read      GetMaxAperture;
    property    SubjectDistance : double
      read      GetSubjectDistance;
    property    MeteringMode : word
      read      FMeteringMode;
    property    LightSource : word
      read      FLightSource;
    property    Flash : word
      read      FFlash;
    property    FocalLength : double
      read      GetFocalLength;
    property    SubjectArea[Index : word] : word
      read      GetSubjectArea;
    property    SubjectAreaLength : word
      read      FSubjectAreaLength;
    property    FlashEnergy : double
      read      GetFlashEnergy;
    //    FFrequencyResponse        : string; // Use SetLength and copy as were it bytes in an array.
    property    FocalPlaneXResolution : double
      read      GetFocalPlaneXResolution;
    property    FocalPlaneYResolution : double
      read      GetFocalPlaneYResolution;
    property    FocalPlaneResolutionUnit : word
      read      FFocalPlaneResolutionUnit;
    property    SubjectLocation[Index : word] : word
      read      GetSubjectLocation;
    property    ExposureIndex : double
      read      GetExposureIndex;
    property    SensingMethod : word
      read      FSensingMethod;
    property    FileSource : integer
      read      FFileSource;
    property    SceneType : integer
      read      FSceneType;
    //    FCFAPattern
    property    CustomRendered : word
      read      FCustomRendered;
    property    ExposureMode : word
      read      FExposureMode;
    property    WhiteBalance : word
      read      FWhiteBalance;
    property    DigitalZoomRatio : double
      read      GetDigitalZoomRatio;
    property    FocalLengthIn35mmFilm : word
      read      FFocalLengthIn35mmFilm;
    property    SceneCaptureType : word
      read      FSceneCaptureType;
    property    GainControl : word
      read      FGainControl;
    property    Contrast : word
      read      FContrast;
    property    Saturation : word
      read      FSaturation;
    property    Sharpness : word
      read      FSharpness;
    //    FDeviceSettingDescription :
    property    SubjectDistanceRange : word
      read      FSubjectDistanceRange;
    property    InteroperabilityIndex : string
      read      FInteroperabilityIndex;
    property    InteroperabilityVersion : integer
      read      FInteroperabilityVersion;
  published
  end;


  TmcmImageGPSInfo = class(TmcmImageMetaInfo)
  private
  protected
    FModified         : boolean;       // Is this objects data modified!
    FVersionID        : integer;
    FLatitudeRef      : string;
    FLatitude         : array[0..5] of longint;
    FLongitudeRef     : string;
    FLongitude        : array[0..5] of longint;
    FAltitudeRef      : integer;
    FAltitude         : array[0..1] of longint;
    FTimeStamp        : array[0..5] of longint;
    FSatellites       : string;
    FStatus           : string;
    FMeasureMode      : string;
    FDOP              : array[0..1] of longint;
    FSpeedRef         : string;
    FSpeed            : array[0..1] of longint;
    FTrackRef         : string;
    FTrack            : array[0..1] of longint;
    FImgDirectionRef  : string;
    FImgDirection     : array[0..1] of longint;
    FMapDatum         : string;
    FDestLatitudeRef  : string;
    FDestLatitude     : array[0..5] of longint;
    FDestLongitudeRef : string;
    FDestLongitude    : array[0..5] of longint;
    FDestBearingRef   : string;
    FDestBearing      : array[0..1] of longint;
    FDestDistanceRef  : string;
    FDestDistance     : array[0..1] of longint;
    FProcessingSize   : integer;
    FProcessingMethod : PVectorB;
    FAreaSize         : integer;
    FAreaInformation  : PVectorB;
    FDateStamp        : TDateTime; // Only date - string; // 11
    FDifferential     : word;

    function    GetLatitude(Index : word) : double;
    function    GetLongitude(Index : word) : double;
    function    GetAltitude : double;
    function    GetTimeStamp(Index : word) : double;
    function    GetDOP : double;
    function    GetSpeed : double;
    function    GetTrack : double;
    function    GetImgDirection : double;
    function    GetDestLatitude(Index : word) : double;
    function    GetDestLongitude(Index : word) : double;
    function    GetDestBearing : double;
    function    GetDestDistance : double;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Assign(Source : TPersistent); override;
    procedure   Clear;

    property    VersionID : integer
      read      FVersionID;
    property    LatitudeRef : string // 2
      read      FLatitudeRef;
    property    Latitude[Index : word] : double // 3
      read      GetLatitude;
    property    LongitudeRef : string // 2
      read      FLongitudeRef;
    property    Longitude[Index : word] : double // 3
      read      GetLongitude;
    property    AltitudeRef : integer
      read      FAltitudeRef;
    property    Altitude : double
      read      GetAltitude;
    property    TimeStamp[Index : word] : double // 3
      read      GetTimeStamp;
    property    Satellites : string
      read      FSatellites;
    property    Status : string // 2
      read      FStatus;
    property    MeasureMode : string // 2
      read      FMeasureMode;
    property    DOP : double
      read      GetDOP;
    property    SpeedRef : string // 2
      read      FSpeedRef;
    property    Speed : double
      read      GetSpeed;
    property    TrackRef : string // 2
      read      FTrackRef;
    property    Track : double
      read      GetTrack;
    property    ImgDirectionRef : string // 2
      read      FImgDirectionRef;
    property    ImgDirection : double
      read      GetImgDirection;
    property    MapDatum : string
      read      FMapDatum;
    property    DestLatitudeRef : string // 2
      read      FDestLatitudeRef;
    property    DestLatitude[Index : word] : double // 3
      read      GetDestLatitude;
    property    DestLongitudeRef : string // 2
      read      FDestLongitudeRef;
    property    DestLongitude[Index : word] : double // 3
      read      GetDestLongitude;
    property    DestBearingRef : string // 2
      read      FDestBearingRef;
    property    DestBearing : double
      read      GetDestBearing;
    property    DestDistanceRef : string
      read      FDestDistanceRef;
    property    DestDistance : double
      read      GetDestDistance;
//    FProcessingSize   : integer;
//    FProcessingMethod : PVectorB;
//    FAreaSize         : integer;
//    FAreaInformation  : PVectorB;
    property    DateStamp : TDateTime
      read      FDateStamp;
    property    Differential : word
      read      FDifferential;
  published
  end;

//------------------------------------------------------------------------------
// TmcmImageInfo
//
// Used to get & set additional information about the image file, that isn't
// contained in the HBITMAP handle.
//------------------------------------------------------------------------------
  TmcmImageInfo = class(TPersistent)
  private
    FCompQuality  : word;         // Compression quality.
    FCompress     : TmcmCompress; // Compression format.
    FArtist       : string;       // Artist name.
    FCopyright    : string;       // Copyright notice claiming thr right to the
                                  // image.
    FDescription  : string;       // Describes the subject of the image.
    FDocumentName : string;       // Name of the scanned document.
    FHostComputer : string;       // The computer/Operating system used when
                                  // images was created.
    FMake         : string;       // Manufacture of image capture device.
    FModel        : string;       // Model of capture device.
    FSoftware     : string;       // Name & version of software package used to
                                  // create the image.
    FSoftwareVer  : double;       // Version of software.
    FFileName     : string;       // Image file name.
    FFileFormat   : TmcmFileFormat;
    FFileSize     : cardinal;     // Image file size.
    FDateTime     : TDateTime;    // Storage date and time of image.
    FIndex        : integer;      // Sub-image index.
    FSubImage     : integer;      // Number of sub-images.
    FUnitRes      : TmcmUnit;     // Unit of measurement.
    FXRes         : double;       // Horizontal resolution.
    FYRes         : double;       // Vertical resolution.
    FXPos         : double;       // Horizontal position.
    FYPos         : double;       // Vertical position.
    FPageName     : string;       // Name of the scanned page.
    FPageNumber   : integer;      // The page number.
    FPixelWidth   : word;         // Width of pixel, Aspect ratio = FPixelWidth / FPixelHeight
    FPixelHeight  : word;         // Height of pixel, Aspect ratio = FPixelWidth / FPixelHeight
    FOrientation  : word;
    FMirrored     : boolean;
  protected
    FModified     : boolean;      // Is this objects data modified!
    FExif         : TmcmImageExifInfo;
    FGPS          : TmcmImageGPSInfo;

    function    ConvertFromMeter(Value : double) : double;
    function    ConvertToMeter(Value : double) : double;
    function    GetFilename : string;
    function    GetXRes : double;
    function    GetXPos : double;
    function    GetYRes : double;
    function    GetYPos : double;

    procedure   SetArtist(Value : string);
    procedure   SetCompress(Value : TmcmCompress);
    procedure   SetCompQuality(Value : word);
    procedure   SetCopyright(Value : string);
    procedure   SetDateTime(Value : TDateTime);
    procedure   SetDescription(Value : string);
    procedure   SetDocumentName(Value : string);
    procedure   SetFileFormat(Value : TmcmFileFormat);
    procedure   SetFileName(Value : string);
    procedure   SetFileSize(Value : cardinal);
    procedure   SetHostComputer(Value : string);
    procedure   SetMake(Value : string);
    procedure   SetMirrored(Value : boolean);
    procedure   SetModel(Value : string);
    procedure   SetOrientation(Value : word);
    procedure   SetPageName(Value : string);
    procedure   SetPageNumber(Value : integer);
    procedure   SetPixelHeight(Value : word);
    procedure   SetPixelWidth(Value : word);
    procedure   SetSoftware(Value : string);
    procedure   SetSoftwareVer(Value : double);
    procedure   SetUnitRes(Value : TmcmUnit);
    procedure   SetXRes(Value : double);
    procedure   SetXPos(Value : double);
    procedure   SetYRes(Value : double);
    procedure   SetYPos(Value : double);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Assign(Source : TPersistent); override;
    {
    property    Compress : TmcmCompress
      read      FCompress
      write     SetCompress;
    }
    property    Artist : string
      read      FArtist
      write     SetArtist;
    property    Copyright : string
      read      FCopyright
      write     SetCopyright;
    property    DateTime : TDateTime
      read      FDateTime
      write     SetDateTime;
    property    Description : string
      read      FDescription
      write     SetDescription;
    property    DocumentName : string
      read      FDocumentName
      write     SetDocumentName;
    property    Exif : TmcmImageExifInfo
      read      FExif;
    property    FileFormat : TmcmFileFormat
      read      FFileFormat
      write     SetFileFormat;
    property    FileName : string
      read      GetFilename
      write     SetFilename;
    property    FileSize : cardinal
      read      FFileSize
      write     SetFileSize;
    property    GPS : TmcmImageGPSInfo
      read      FGPS;
    property    HostComputer : string
      read      FHostComputer
      write     SetHostComputer;
    property    Make : string
      read      FMake
      write     SetMake;
    property    Mirrored : boolean
      read      FMirrored
      write     SetMirrored;
    property    Model : string
      read      FModel
      write     SetModel;
    property    Modified : boolean
      read      FModified;
    property    Orientation : word
      read      FOrientation
      write     SetOrientation;
    property    PageName : string
      read      FPageName
      write     SetPageName;
    property    PageNumber : integer
      read      FPageNumber
      write     SetPageNumber;
    property    PixelHeight : word
      read      FPixelHeight
      write     SetPixelHeight;
    property    PixelWidth : word
      read      FPixelWidth
      write     SetPixelWidth;
    {
    property    Quality : word
      read      FCompQuality
      write     SetCompQuality;
    }
    property    Software : string
      read      FSoftware
      write     SetSoftware;
    property    SoftwareVersion : double
      read      FSoftwareVer
      write     SetSoftwareVer;
    property    Units : TmcmUnit
      read      FUnitRes
      write     SetUnitRes;
    property    XPosition : double
      read      GetXPos
      write     SetXPos;
    property    XResolution : double
      read      GetXRes
      write     SetXRes;
    property    YPosition : double
      read      GetYPos
      write     SetYPos;
    property    YResolution : double
      read      GetYRes
      write     SetYRes;
  published
  end;

//------------------------------------------------------------------------------
// TmcmImage
//------------------------------------------------------------------------------
  {$IFDEF MCMDEMO}
    TmcmVector = Array[0..64] of byte;
    PmcmVector = ^TmcmVector;
  {$ENDIF}


  TmcmImage = class(TPersistent)
  private
    // Private declarations
    FOnChange     : TNotifyEvent;     // On image change event.
    FModified     : boolean;          // Image is changed if true.
    FImageInfo    : TmcmImageInfo;    // Pointer to image info class.
    FError        : TmcmErrorCode;    // Last error occured.
    FWinError     : {$IFDEF VER100} longint; {$ELSE} cardinal; {$ENDIF}
    FSBltMode     : integer;
  protected
    // Protected declarations
  {$IFDEF MCMDEMO}
    pBmp          : PmcmVector;
  {$ENDIF}
    FRefCount     : integer;
    FDibHandle    : HBITMAP;          // Handle to device independent bitmap.
    FDIBSection   : TDIBSection;
    FDibBits      : Pointer;
    FDibInfo      : PBitmapInfo;
    FDibLogPal    : TMaxLogPalette;
    FDibPal       : HPalette;
    FBitCount     : integer;          // Bits per pixel.
    FLongWidth    : longint;          // Long line width (bitmap)
    FImageFormat  : TmcmImageFormat;
    FTransparent  : boolean;
    FTransColor   : TColorRef;
    FMaskHandle   : HBITMAP;          // Device Dependent Bitmap handle
    FxOrg, FyOrg  : integer;
    FScale        : double;
    FScaleHVRes   : boolean;          // Correctly displaying image with different
                                      // horizontal and vertical resolution.
    FFlipY        : boolean;          // Filp image vertically when displaying.
    FCanvas       : TCanvas;
    FCompress     : TmcmCompress;     // Compression format.
    FQuality      : word;             // Compression quality.
    FInterlaced   : boolean;          // Image is stored interlaced.

    procedure   Changed(Sender : TObject); virtual;
    function    CreateDIBHandle : HBITMAP;
    procedure   FreeContext;
    function    GetBitCount : word;
    function    GetCanvas : TCanvas;
    function    GetColorTableFromPal(Pal : HPALETTE; var ColorTable : array of TRGBQuad) : integer;
    function    GetCompress : TmcmCompress;
    function    GetDibBits : pointer;
    function    GetDibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    function    GetDibInfo : PBitmapInfo;
    function    GetDispHeight : integer;
    function    GetDispWidth : integer;
    function    GetEmpty : boolean;
    function    GetHeight : longint;
    function    GetImageFormat : TmcmImageFormat;
    function    GetImageInfo : TmcmImageInfo;
    function    GetLongWidth : cardinal;
    function    GetNumColors(pDibInfo : PBitmapInfo) : integer;
    function    GetPalette : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF};
    function    GetPalFromDibHandle(var ColorTable : array of TRGBQuad) : HPALETTE;
    function    GetPixel(x, y : integer) : TColor;
    function    GetWidth : longint;
    function    GetUniqueColors : cardinal;
    function    GetXResolution : integer;
    function    GetYResolution : integer;
    function    GetScanLine(Row : longint) : pointer;
    procedure   GetWindowsErrorCode(ErrStr : string);
    procedure   GetWindowsGDIErrorCode(GDIResult : integer);

    procedure   SetBitCount(Value : word);
    procedure   SetCompress(Value : TmcmCompress);
    procedure   SetDIB(Value : HBITMAP);
    procedure   SetDibBits(Value : pointer);
    procedure   SetDibHandle(Value : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
    procedure   SetDibInfo(Value : PBitmapInfo);
    procedure   SetFlipY(Value : boolean);
    procedure   SetHeight(Value : longint);
    procedure   SetImageFormat(Value : TmcmImageFormat);
    procedure   SetInterlaced(Value : boolean);
    procedure   SetModified(Value : boolean);
    procedure   SetPalette(Value : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF});
    procedure   SetPalToDibHandle(ColorTable : array of TRGBQuad; NoColors : integer);
    procedure   SetPixel(x, y : integer; Value : TColor);
    procedure   SetQuality(Value : word);
    procedure   SetScaleHVRes(Value : boolean);
    procedure   SetTransparent(Value : boolean);
    procedure   SetTransparentColor(Value : TColorRef);
    procedure   SetWidth(Value : longint);
    procedure   SetXResolution(Value : integer);
    procedure   SetYResolution(Value : integer);
    property    DibInfo : PBitmapInfo
      read      GetDibInfo
      write     SetDibInfo;
  public
    // Public declarations
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Assign(Source : TPersistent); override;
    procedure   Clear;
    procedure   CopyFormat(Source : TmcmImage);
    procedure   CopyRegion(var Target : TmcmImage; Region : TRect);
    function    CreateGreyPalette : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF};
    function    AlphaBlend(DC : {$IFDEF BCB} TmcmHDC {$ELSE} HDC {$ENDIF}; DCScale : double) : integer;
    procedure   Draw(DC : {$IFDEF BCB} TmcmHDC {$ELSE} HDC {$ENDIF}; DCScale : double); virtual;
    procedure   FileAppend(Filename : string);
    procedure   FileOpen(Filename : string);
    procedure   FilePageOpen(Filename : string; PageNumber : integer);
    procedure   FileSave(Filename : string);
    procedure   FillAll(Value : byte);
    procedure   FillRGB(rgbColor : TColor);
    function    GetNearestPaletteIndex(rgbColor : TColor) : integer;
    function    GetOrigo : TPoint;
    function    GetPaletteEntries(Palette : PLogPalette) : integer;
    function    GetPaletteEntry(Index : integer; PalEntry : PPaletteEntry) : integer;
    function    GetStretchMode : integer;
    procedure   LoadFromClipboardFormat(AFormat : word; AData : THandle);
    procedure   LoadFromStream(Stream : TStream);
    function    LoadFromStreamEx(Stream : TStream; FileFormat : TmcmFileFormat) : TmcmErrorCode;
    function    MaxIntScale : integer;
    procedure   PasteRegion(Source : TmcmImage; const Region : TRect);
    function    ReleaseHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
    procedure   SaveToClipboardFormat(var AFormat : word; var AData : THandle);
    procedure   SaveToStream(Stream : TStream);
    function    SaveToStreamEx(Stream : TStream; FileFormat : TmcmFileFormat) : TmcmErrorCode;
    procedure   SetOrigo(xy : TPoint); virtual;
    procedure   SetPaletteEntries(Palette : PLogPalette);
    procedure   SetPaletteEntry(Index : integer; PalEntry : PPaletteEntry);
    procedure   SetStretchMode(Value : integer);
    function    SwapRBValues(const Value : TRGBQuad) : TRGBQuad;

    property    BitCount : word
      read      GetBitCount
      write     SetBitCount;
    property    Canvas : TCanvas
      read      GetCanvas;
    property    Compression  : TmcmCompress
      read      GetCompress
      write     SetCompress;
    property    DibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF}
      read      GetDibHandle
      write     SetDibHandle;
    property    DispHeight : integer
      read      GetDispHeight;
    property    DispWidth : integer
      read      GetDispWidth;
    property    Empty : boolean
      read      GetEmpty;
    property    Error : TmcmErrorCode
      read      FError;
    property    FlipY : boolean
      read      FFlipY
      write     SetFlipY default False;
    property    Height : longint
      read      GetHeight
      write     SetHeight;
    property    ImageFormat : TmcmImageFormat
      read      GetImageFormat
      write     SetImageFormat;
    property    ImageInfo : TmcmImageInfo
      read      GetImageInfo;
    property    Interlaced  : boolean
      read      FInterlaced
      write     SetInterlaced;
    property    LongLineWidth : cardinal
      read      GetLongWidth;
    property    Modified : boolean
      read      FModified
      write     SetModified;
    property    Palette : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF}
      read      GetPalette
      write     SetPalette;
    property    pDib : Pointer
      read      GetDibBits
      write     SetDibBits;
    property    Pixel[X, Y : integer] : TColor
      read      GetPixel
      write     SetPixel;
    property    Quality : word
      read      FQuality
      write     SetQuality;
    property    ScaleHVResolution : boolean
      read      FScaleHVRes
      write     SetScaleHVRes default False;
    property    ScanLine[Row : longint] : pointer
      read      GetScanLine;
    property    Transparent : boolean
      read      FTransparent
      write     SetTransparent;
    property    TransparentColor : TColorRef
      read      FTransColor
      write     SetTransparentColor;
    property    UniqueColors : cardinal
      read      GetUniqueColors;
    property    Width : longint
      read      GetWidth
      write     SetWidth;
    property    XResolution : integer
      read      GetXResolution
      write     SetXResolution;
    property    YResolution : integer
      read      GetYResolution
      write     SetYResolution;

    property    OnChange : TNotifyEvent
      read      FOnChange
      write     FOnChange;
  published
    // Published declarations
  end;


//------------------------------------------------------------------------------
// TmcmImageCanvas
//------------------------------------------------------------------------------

  TmcmImageCanvas = class(TCanvas)
  private
    FBitmap     : TmcmImage;
    FOldBitmap  : HBITMAP;
    FOldPalette : HPALETTE;
    procedure FreeContext;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AImage : TmcmImage);
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------
// TmcmImageCtrl
//------------------------------------------------------------------------------

  TMouseWheelEvent = procedure(Sender : TObject; Shift : TShiftState; WheelDelta : Integer; MousePos : TPoint; var Handled: Boolean) of object;

  TmcmImageCtrl = class(TControl)
  private
    // Private declarations
  protected
    // Protected declarations
    FDC           : HDC;
    FFakeVisible  : boolean;
    FImage        : TmcmImage;
    FAutoSize     : boolean;
    FCenter       : boolean;
    FFlat         : boolean;
    FScale        : double;
    FScaleToFit   : boolean;
    FTransparent  : boolean;
    FOnChange     : TNotifyEvent;     // Fired when the image has been modified.
    FOnPainted    : TNotifyEvent;     // Fired when the image has been painted.
    FBorderStyle  : TmcmBorderStyle;
    FOnMouseMove  : TMouseMoveEvent;
    FOnMouseWheel : TMouseWheelEvent;
    FDragPos      : TPoint;    // Start coordinate of drag operation.
    FDragMode     : TDragMode; // Drag mode, overrides Delphi's drag mode
                               // variable.
    procedure   Changed(Sender : TObject); virtual;
    function    GetBorderStyle : TmcmBorderStyle;
    function    GetCenter : boolean;
    function    GetColor : TColor;
    function    GetImage : TmcmImage; virtual;
    function    GetScale : double; virtual;
    function    GetScaleToFit : boolean;
    function    GetTransparent : boolean;
    function    GetTransparentColor : TColorRef;
    function    GetVisible : boolean;
    procedure   MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure   SetAutoSize(Value : boolean); {$IFNDEF DCB3_5} override; {$ENDIF}
    procedure   SetBorderStyle(Value : TmcmBorderStyle);
    procedure   SetCenter(Value : boolean);
    procedure   SetColor(Value : TColor);
    procedure   SetDragMode(Value: TDragMode); override;
    procedure   SetFlat(Value : boolean);
    procedure   SetImage(Value : TmcmImage); virtual;
    procedure   SetScale(Value : double); virtual;
    procedure   SetScaleToFit(Value : boolean);
    procedure   SetTransparent(Value : boolean);
    procedure   SetTransparentColor(Value : TColorRef);
    procedure   SetVisible(Value : boolean);
    procedure   Paint; virtual;
    procedure   WMPaint(var Message : TWMPaint); message WM_PAINT;
    procedure   WMMouseWheel(var Message : TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure   WndProc(var Message: TMessage); override;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    procedure   DrawImage;
    property    Image : TmcmImage
      read      GetImage
      write     SetImage;
  published
    // Published declarations
    property    Align;
    {$IFNDEF DCB3}
    property    Anchors;
    {$ENDIF}
    property    AutoSize : boolean
      read      FAutoSize
      write     SetAutoSize default false;
    property    BorderStyle : TmcmBorderStyle
      read      GetBorderStyle
      write     SetBorderStyle default BS_NONE;
    property    Center : boolean
      read      GetCenter
      write     SetCenter default False;
    property    Color : TColor
      read      GetColor
      write     SetColor default $1FFFFFFF; // <- clNone
    {$IFNDEF DCB3}
    property    Constraints;
    {$ENDIF}
    {$IFNDEF DCB3}
    property    DragKind;
    {$ENDIF}
    property    DragMode;
    property    DragCursor;
    property    Flat : boolean
      read      FFlat
      write     SetFlat default False;
    {$IFNDEF DCB3}
    property    ParentBiDiMode;
    {$ENDIF}
    property    ParentColor;
    property    ParentShowHint;
    property    Scale : double
      read      GetScale
      write     SetScale;
    property    ScaleToFit : boolean
      read      GetScaleToFit
      write     SetScaleToFit;
    property    Transparent : boolean
      read      GetTransparent
      write     SetTransparent default False;
    property    Visible : Boolean
      read      GetVisible
      write     SetVisible default True;

    property    OnChange : TNotifyEvent
      read      FOnChange
      write     FOnChange;
    property    OnClick;
    {$IFNDEF DCB3_4}
    property    OnContextPopup;
    {$ENDIF}
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    {$IFNDEF DCB3}
    property    OnEndDock;
    {$ENDIF}
    property    OnEndDrag;
//    property    OnEnter;
//    property    OnExit;
    property    OnMouseDown;
    property    OnMouseMove  : TMouseMoveEvent
      read      FOnMouseMove
      write     FOnMouseMove;
    property    OnMouseUp;
    property    OnMouseWheel : TMouseWheelEvent
      read      FOnMouseWheel
      write     FOnMouseWheel;
    property    OnPainted : TNotifyEvent
      read      FOnPainted
      write     FOnPainted;
    {$IFNDEF DCB3}
    property    OnStartDock;
    {$ENDIF}
    property    OnStartDrag;
  end;

//------------------------------------------------------------------------------
// TmcmMultiImageCtrl
//------------------------------------------------------------------------------

  TmcmMultiImageCtrl = class(TmcmImageCtrl)
  private
    // Private declarations
    FActiveImage : integer;
    FImageList   : TList;
  protected
    // Protected declarations
    function  GetCount : integer;
    function  GetImage : TmcmImage; override;
    function  GetImageItem(Index : word) : TmcmImage;
    procedure SetActiveImage(Index : integer);
    procedure SetImage(Value : TmcmImage); override;
    procedure SetImageItem(Index : word; NewImage : TmcmImage);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    AddImage : TmcmImage;
    procedure   Clear; override;
    property    ActiveImage : integer
      read      FActiveImage
      write     SetActiveImage;
    property    Count : integer
      read      GetCount;
    property    ImageList[Index : word] : TmcmImage
      read      GetImageItem
      write     SetImageItem;
  published
    // Published declarations
  end;

{$IFDEF MCMDEMO}
var pFileSup : pointer = pointer(0);
{$ENDIF}

implementation

uses {$IFNDEF GE_DXE2} SysUtils, {$ELSE} System.SysUtils, System.Types, {$ENDIF}
     {$IFDEF MCMDEMO}
      {$IFNDEF GE_DXE2} Dialogs, {$ELSE} Vcl.Dialogs, {$ENDIF}
      uImgAbout,
     {$ENDIF}
     mcmImageFile;


//------------------------------------------------------------------------------
// TmcmImageInfo
//------------------------------------------------------------------------------

constructor TmcmImageInfo.Create;
begin
  Inherited Create;
  FExif := Nil;
  FGPS  := Nil;
  Clear;
end; // TmcmImageInfo.Create.


destructor TmcmImageInfo.Destroy;
begin
  if Assigned(FExif)
  then FExif.Free;
  FExif := Nil;
  if Assigned(FGPS)
  then FGPS.Free;
  FGPS := Nil;
  Inherited Destroy;
end; // TmcmImageInfo.Destroy.


procedure TmcmImageInfo.Clear;
begin
  FModified := False;
  FArtist       := '';
  FCopyright    := '';
  FCompQuality  := 100;
  FUnitRes      := UN_METERS;
  FxRes         := 0.0;
  FyRes         := 0.0;
  FxPos         := 0.0;
  FyPos         := 0.0;
  FCompress     := CP_NOCOMP;
  FFileName     := '';
  FFileSize     := 0;
  FFileFormat   := FF_BMP;
  FDateTime     := 0;
  FIndex        := 0;
  FSubImage     := 0;
  FDescription  := '';
  FHostComputer := '';
  FMake         := '';
  FMirrored     := False;
  FModel        := '';
  FPixelHeight  := 1;
  FPixelWidth   := 1;
  FSoftware     := '';
  FSoftwareVer  := 0;
  FDocumentName := '';
  FOrientation  := 0;
  FPageName     := '';
  FPageNumber   := 0;
  if Assigned(FExif)
  then FExif.Free;
  FExif := Nil;
  if Assigned(FGPS)
  then FGPS.Free;
  FGPS  := Nil;
end; // TmcmImageInfo.Clear.


procedure TmcmImageInfo.Assign(Source : TPersistent);
begin
    // Inherited Assign(Source);
    FCompQuality  := TmcmImageInfo(Source).FCompQuality;
    FCompress     := TmcmImageInfo(Source).FCompress;

    FArtist       := TmcmImageInfo(Source).FArtist;
    FCopyright    := TmcmImageInfo(Source).FCopyright;
    FDescription  := TmcmImageInfo(Source).FDescription;
    FDocumentName := TmcmImageInfo(Source).FDocumentName;
    FHostComputer := TmcmImageInfo(Source).FHostComputer;
    FMake         := TmcmImageInfo(Source).FMake;
    FMirrored     := TmcmImageInfo(Source).FMirrored;
    FModel        := TmcmImageInfo(Source).FModel;
    FSoftware     := TmcmImageInfo(Source).FSoftware;
    FSoftwareVer  := TmcmImageInfo(Source).FSoftwareVer;
    FFileName     := TmcmImageInfo(Source).FFileName;
    FFileFormat   := TmcmImageInfo(Source).FFileFormat;
    FFileSize     := TmcmImageInfo(Source).FFileSize;
    FDateTime     := TmcmImageInfo(Source).FDateTime;
    FIndex        := TmcmImageInfo(Source).FIndex;
    FOrientation  := TmcmImageInfo(Source).FOrientation;
    FPageName     := TmcmImageInfo(Source).FPageName;
    FPageNumber   := TmcmImageInfo(Source).FPageNumber;
    FPixelHeight  := TmcmImageInfo(Source).FPixelHeight;
    FPixelWidth   := TmcmImageInfo(Source).FPixelWidth;
    FSubImage     := TmcmImageInfo(Source).FSubImage;
    FUnitRes      := TmcmImageInfo(Source).FUnitRes;
    FXRes         := TmcmImageInfo(Source).FXRes;
    FYRes         := TmcmImageInfo(Source).FYRes;
    FXPos         := TmcmImageInfo(Source).FXPos;
    FYPos         := TmcmImageInfo(Source).FYPos;

    // Copy Exif information if available.
    if Assigned(TmcmImageInfo(Source).Exif)
    then begin
         if Not(Assigned(FExif))
         then FExif := TmcmImageExifInfo.Create;
         FExif.Assign(TmcmImageInfo(Source).Exif);
    end
    else begin
         if Assigned(FExif)
         then FExif.Free;
         FExif := Nil;
    end;

    // Copy Exif information if available.
    if Assigned(TmcmImageInfo(Source).GPS)
    then begin
         if Not(Assigned(FGPS))
         then FGPS := TmcmImageGPSInfo.Create;
         FGPS.Assign(TmcmImageInfo(Source).GPS);
    end
    else begin
         if Assigned(FGPS)
         then FGPS.Free;
         FGPS := Nil;
    end;

    FModified := True;
end; // TmcmImageInfo.Assign.


function TmcmImageInfo.ConvertFromMeter(Value : double) : double;
begin
  case FUnitRes of
  UN_INCHS       : Result := Value * 0.0254;
  UN_CENTIMETERS : Result := Value / 100.0;
  UN_PICAS       : Result := Value * 0.0254 * 6.0;
  UN_POINTS      : Result := Value * 0.0254 * 72.0;
  UN_TWIPS       : Result := Value * 0.0254 * 1440.0;
  UN_METERS      : Result := Value;
  // UN_PIXELS
  else             Result := 0.0;
  end;
end; // TmcmImageInfo.ConvertFromMeter.


function TmcmImageInfo.ConvertToMeter(Value : double) : double;
begin
  case FUnitRes of
  UN_INCHS       : Result := Value / 0.0254;
  UN_CENTIMETERS : Result := Value * 100.0;
  UN_PICAS       : Result := Value * 6.0 / 0.0254;
  UN_POINTS      : Result := Value * 72.0 / 0.0254;
  UN_TWIPS       : Result := Value * 1440.0 / 0.0254;
  UN_METERS      : Result := Value;
  // UN_PIXELS
  else             Result := 0.0;
  end;
end; // TmcmImageInfo.ConvertToMeter.


function TmcmImageInfo.GetXRes : double;
begin
  Result := ConvertFromMeter(FXRes);
end; // TmcmImageInfo.GetXRes.


function TmcmImageInfo.GetXPos : double;
begin
  Result := ConvertFromMeter(FXPos);
end; // TmcmImageInfo.GetXPos.


function TmcmImageInfo.GetYRes : double;
begin
  Result := ConvertFromMeter(FYRes);
end; // TmcmImageInfo.GetYRes.


function TmcmImageInfo.GetYPos : double;
begin
  Result := ConvertFromMeter(FYPos);
end; // TmcmImageInfo.GetYPos.


procedure TmcmImageInfo.SetArtist(Value : string);
begin
  if (FArtist <> Value)
  then begin
       FArtist := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetArtist.


procedure TmcmImageInfo.SetCompress(Value : TmcmCompress);
begin
  if (FCompress <> Value)
  then begin
       FCompress := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetCompress.


procedure TmcmImageInfo.SetCompQuality(Value : word);
begin
  if (FCompQuality <> Value)
  then begin
       FCompQuality := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetCompQuality.


procedure TmcmImageInfo.SetCopyright(Value : string);
begin
  if (FCopyright <> Value)
  then begin
       FModified := True;
       FCopyright := Value;
  end;
end; // TmcmImageInfo.SetCopyright.


procedure TmcmImageInfo.SetDateTime(Value : TDateTime);
begin
  FModified := True;
  FDateTime := Value;
end; // TmcmImageInfo.SetDateTime.


procedure TmcmImageInfo.SetDescription(Value : string);
begin
  if (FDescription <> Value)
  then begin
       FModified := True;
       FDescription := Value;
  end;
end; // TmcmImageInfo.SetDescription.


procedure TmcmImageInfo.SetDocumentName(Value : string);
begin
  if (FDocumentName <> Value)
  then begin
       FModified := True;
       FDocumentName := Value;
  end;
end; // TmcmImageInfo.SetDocumentName.


function TmcmImageInfo.GetFilename : string;
begin
  Result := FFilename;
end; // TmcmImageInfo.GetFilename.


procedure TmcmImageInfo.SetFileFormat(Value : TmcmFileFormat);
begin
  if (FFileFormat <> Value)
  then begin
       FFileFormat := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetFileFormat.


procedure TmcmImageInfo.SetFilename(Value : string);
begin
  if (FFilename <> Value)
  then begin
       FFilename := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetFilename.


procedure TmcmImageInfo.SetFileSize(Value : cardinal);
begin
  if (FFileSize <> Value)
  then begin
       FFileSize := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetFileSize.


procedure TmcmImageInfo.SetHostComputer(Value : string);
begin
  if (FHostComputer <> Value)
  then begin
       FModified := True;
       FHostComputer := Value;
  end;
end; // TmcmImageInfo.SetHostComputer.


procedure TmcmImageInfo.SetMake(Value : string);
begin
  if (FMake <> Value)
  then begin
       FModified := True;
       FMake := Value;
  end;
end; // TmcmImageInfo.SetMake.


procedure TmcmImageInfo.SetMirrored(Value : boolean);
begin
  if (FMirrored <> Value)
  then begin
       FModified := True;
       FMirrored := Value;
  end;
end; // TmcmImageInfo.SetMirrored.


procedure TmcmImageInfo.SetModel(Value : string);
begin
  if (FModel <> Value)
  then begin
       FModified := True;
       FModel := Value;
  end;
end; // TmcmImageInfo.SetModel.


procedure TmcmImageInfo.SetPixelHeight(Value : word);
begin
  if (FPixelHeight <> Value)
  then begin
       FModified := True;
       FPixelHeight := Value;
  end;
end; // TmcmImageInfo.SetPixelHeight.


procedure TmcmImageInfo.SetOrientation(Value : word);
begin
  if (FOrientation <> Value)
  then begin
       FModified := True;
       FOrientation := Value;
  end;
end; // TmcmImageInfo.SetOrientation.


procedure TmcmImageInfo.SetPageName(Value : string);
begin
  if (FPageName <> Value)
  then begin
       FModified := True;
       FPageName := Value;
  end;
end; // TmcmImageInfo.SetPageName.


procedure TmcmImageInfo.SetPageNumber(Value : integer);
begin
  if (FPageNumber <> Value)
  then begin
       FModified := True;
       FPageNumber := Value;
  end;
end; // TmcmImageInfo.SetPageNumber.


procedure TmcmImageInfo.SetPixelWidth(Value : word);
begin
  if (FPixelWidth <> Value)
  then begin
       FModified := True;
       FPixelWidth := Value;
  end;
end; // TmcmImageInfo.SetPixelWidth.


procedure TmcmImageInfo.SetSoftware(Value : string);
begin
  if (FSoftware <> Value)
  then begin
       FModified := True;
       FSoftware := Value;
  end;
end; // TmcmImageInfo.SetSoftware.


procedure TmcmImageInfo.SetSoftwareVer(Value : double);
begin
  if (FSoftwareVer <> Value)
  then begin
       FModified := True;
       FSoftwareVer := Value;
  end;
end; // TmcmImageInfo.SetSoftware.


procedure TmcmImageInfo.SetUnitRes(Value : TmcmUnit);
begin
  if (FUnitRes <> Value)
  then begin
       FUnitRes := Value;
       FModified := True;
  end;
end; // TmcmImageInfo.SetUnitRes.


procedure TmcmImageInfo.SetXRes(Value : double);
begin
  if (FXRes <> ConvertToMeter(Value))
  then begin
       FXRes := ConvertToMeter(Value);
       FModified := True;
  end;
end; // TmcmImageInfo.SetXRes.


procedure TmcmImageInfo.SetYRes(Value : double);
begin
  if (FYRes <> ConvertToMeter(Value))
  then begin
       FYRes := ConvertToMeter(Value);
       FModified := True;
  end;
end; // TmcmImageInfo.SetYRes.


procedure TmcmImageInfo.SetXPos(Value : double);
begin
  if (FXPos <> ConvertToMeter(Value))
  then begin
       FXPos := ConvertToMeter(Value);
       FModified := True;
  end;
end; // TmcmImageInfo.SetXPos.


procedure TmcmImageInfo.SetYPos(Value : double);
begin
  if (FYPos <> ConvertToMeter(Value))
  then begin
       FYPos := ConvertToMeter(Value);
       FModified := True;
  end;
end; // TmcmImageInfo.SetYPos.


//------------------------------------------------------------------------------
// TmcmImageMetaInfo
//------------------------------------------------------------------------------

function TmcmImageMetaInfo.ConvertToDouble(Value : array of integer) : double;
begin
  if (Value[1] <> 0)
  then Result := Value[0] / Value[1]
  else Result := 0;
end; // TmcmImageMetaInfo.ConvertToDouble.


procedure TmcmImageMetaInfo.DoubleToFraction(Value : double; var N, D : longint);
var R : double;
begin
  if (Value = 0.0)
  then begin
       N := 0;
       D := 1;
  end
  else begin
       R := Value;
       N := Trunc(R);
       D := 1;
       while (D < 10000) and (N <> R)
       do begin
          D := D * 10;
          R := Value * D;
          N := Trunc(R);
       end;
  end;
end; // TmcmImageMetaInfo.DoubleToFraction.


//------------------------------------------------------------------------------
// TmcmImageExifInfo
//------------------------------------------------------------------------------

constructor TmcmImageExifInfo.Create;
begin
  Inherited Create;
  FOECF_Size                := 0;
  FOECF                     := Nil;
  FSFR_Size                 := 0;
  FFrequencyResponse        := Nil;
  FCFA_Size                 := 0;
  FCFAPattern               := Nil;
  FDSD_Size                 := 0;
  FDeviceSettingDescription := Nil;
  Clear;
end; // TmcmImageExifInfo.Create.


destructor TmcmImageExifInfo.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmImageExifInfo.Destroy.


procedure TmcmImageExifInfo.Clear;
begin
  FModified                 := False;       // Is this objects data modified!
  // Version info.
  FExifVersion              := 0;
  FFlashPixVersion          := 0;
  // Color space info.
  FColorSpace               := $FFFF;
  // Image configuration.
  FXDimension               := 0;
  FYDimension               := 0;
  FillChar(FComponentConfig, 4 * SizeOf(byte), 0);
  FillChar(FCompressdBitPerPixel, 2 * SizeOf(longint), 0);

  // User information.
  FMakerNote                := '';
  FUserComment              := '';
  // Date & Time
  FDateTimeOriginal         := 0.0;
  FDateTimeDigitized        := 0.0;
  FSubSecTime               := -1;
  FSubSecTimeOriginal       := -1;
  FSubSecTimeDigitized      := -1;
  // Other information.
  FUniqueID                 := '';

  // Picture taking conditions
  FExposureTime[1]          := 0;
  FFNumber[1]               := 0;
  FExposureProgram          := -1;
  FSpectralSensitivity      := '';
  FISOSpeedRating           := 0;
  FOECF_Size                := 0;
  if Assigned(FOECF)
  then FreeMem(FOECF);
  FOECF                     := Nil;
  FShutterSpeed[1]          := 0;
  FAperture[1]              := 0;
  FBrightness[1]            := 0;
  FExposureBias[1]          := 0;
  FMaxAperture[1]           := 0;
  FSubjectDistance[1]       := longint($FFFFFFFF);
  FMeteringMode             := 0;
  FLightSource              := 0;
  FFlash                    := $FFFF;
  FFocalLength[1]           := 0;
  FSubjectAreaLength        := 0;
  FillChar(FSubjectArea, 4 * SizeOf(word), 0);
  FFlashEnergy[1]           := 0;
  FSFR_Size                 := 0;
  if Assigned(FFrequencyResponse)
  then FreeMem(FFrequencyResponse);
  FFrequencyResponse        := Nil;
  FFocalPlaneXResolution[1] := 0;
  FFocalPlaneYResolution[1] := 0;
  FFocalPlaneResolutionUnit := 2;
  FillChar(FSubjectLocation, 2 * SizeOf(word), 0);
  FExposureIndex[1]         := 0;
  FSensingMethod            := 0;
  FFileSource               := 3;
  FSceneType                := 1;
  FCFA_Size                 := 0;
  if Assigned(FCFAPattern)
  then FreeMem(FCFAPattern);
  FCFAPattern               := Nil;
  FCustomRendered           := 0;
  FExposureMode             := $FFFF;
  FWhiteBalance             := $FFFF;
  FDigitalZoomRatio[1]      := 0;
  FFocalLengthIn35mmFilm    := 0;
  FSceneCaptureType         := $FFFF;
  FGainControl              := $FFFF;
  FContrast                 := 0;
  FSaturation               := 0;
  FSharpness                := 0;

  FDSD_Size                 := 0;
  if Assigned(FDeviceSettingDescription)
  then FreeMem(FDeviceSettingDescription);
  FDeviceSettingDescription := Nil;

  FSubjectDistanceRange     := $FFFF;

  FInteroperabilityIndex    := '';
end; // TmcmImageExifInfo.Clear.


procedure TmcmImageExifInfo.Assign(Source : TPersistent);
begin
  if Assigned(Source)
  then if (Source is TmcmImageExifInfo)
       then begin
            Clear;

            // Version info.
            FExifVersion              := TmcmImageExifInfo(Source).FExifVersion;
            FFlashPixVersion          := TmcmImageExifInfo(Source).FFlashPixVersion;
            // Color space info.
            FColorSpace               := TmcmImageExifInfo(Source).FColorSpace;
            // Image configuration.
            FXDimension               := TmcmImageExifInfo(Source).FXDimension;
            FYDimension               := TmcmImageExifInfo(Source).FYDimension;
            FComponentConfig          := TmcmImageExifInfo(Source).FComponentConfig;
            FCompressdBitPerPixel     := TmcmImageExifInfo(Source).FCompressdBitPerPixel;

            // User information.
            FMakerNote                := TmcmImageExifInfo(Source).FMakerNote;
            FUserComment              := TmcmImageExifInfo(Source).FUserComment;
            // Date & Time
            FDateTimeOriginal         := TmcmImageExifInfo(Source).FDateTimeOriginal;
            FDateTimeDigitized        := TmcmImageExifInfo(Source).FDateTimeDigitized;
            FSubSecTime               := TmcmImageExifInfo(Source).FSubSecTime;
            FSubSecTimeOriginal       := TmcmImageExifInfo(Source).FSubSecTimeOriginal;
            FSubSecTimeDigitized      := TmcmImageExifInfo(Source).FSubSecTimeDigitized;
            // Other information.
            FUniqueID                 := TmcmImageExifInfo(Source).FUniqueID;

            // Picture taking conditions
            FExposureTime             := TmcmImageExifInfo(Source).FExposureTime;
            FFNumber                  := TmcmImageExifInfo(Source).FFNumber;
            FExposureProgram          := TmcmImageExifInfo(Source).FExposureProgram;
            FSpectralSensitivity      := TmcmImageExifInfo(Source).FSpectralSensitivity;
            FISOSpeedRating           := TmcmImageExifInfo(Source).FISOSpeedRating;
            if (TmcmImageExifInfo(Source).FOECF_Size > 0)
            then begin
                 FOECF_Size := TmcmImageExifInfo(Source).FOECF_Size;
                 GetMem(FOECF, FOECF_Size * SizeOf(byte));
                 CopyMemory(FOECF, TmcmImageExifInfo(Source).FOECF, FOECF_Size * SizeOf(byte));
            end;
            FShutterSpeed             := TmcmImageExifInfo(Source).FShutterSpeed;
            FAperture                 := TmcmImageExifInfo(Source).FAperture;
            FBrightness               := TmcmImageExifInfo(Source).FBrightness;
            FExposureBias             := TmcmImageExifInfo(Source).FExposureBias;
            FMaxAperture              := TmcmImageExifInfo(Source).FMaxAperture;
            FSubjectDistance          := TmcmImageExifInfo(Source).FSubjectDistance;
            FMeteringMode             := TmcmImageExifInfo(Source).FMeteringMode;
            FLightSource              := TmcmImageExifInfo(Source).FLightSource;
            FFlash                    := TmcmImageExifInfo(Source).FFlash;
            FFocalLength              := TmcmImageExifInfo(Source).FFocalLength;
            FSubjectAreaLength        := TmcmImageExifInfo(Source).FSubjectAreaLength;
            FSubjectArea              := TmcmImageExifInfo(Source).FSubjectArea;

            FFlashEnergy              := TmcmImageExifInfo(Source).FFlashEnergy;

            if (TmcmImageExifInfo(Source).FSFR_Size > 0)
            then begin
                 FSFR_Size := TmcmImageExifInfo(Source).FSFR_Size;
                 GetMem(FFrequencyResponse, FSFR_Size * SizeOf(byte));
                 CopyMemory(FFrequencyResponse, TmcmImageExifInfo(Source).FFrequencyResponse, FSFR_Size * SizeOf(byte));
            end;
            FFocalPlaneXResolution    := TmcmImageExifInfo(Source).FFocalPlaneXResolution;
            FFocalPlaneYResolution    := TmcmImageExifInfo(Source).FFocalPlaneYResolution;
            FFocalPlaneResolutionUnit := TmcmImageExifInfo(Source).FFocalPlaneResolutionUnit;

            FSubjectLocation          := TmcmImageExifInfo(Source).FSubjectLocation;

            FExposureIndex            := TmcmImageExifInfo(Source).FExposureIndex;
            FSensingMethod            := TmcmImageExifInfo(Source).FSensingMethod;
            FFileSource               := TmcmImageExifInfo(Source).FFileSource;
            FSceneType                := TmcmImageExifInfo(Source).FSceneType;
            if (TmcmImageExifInfo(Source).FCFA_Size > 0)
            then begin
                 FCFA_Size := TmcmImageExifInfo(Source).FCFA_Size;
                 GetMem(FCFAPattern, FCFA_Size * SizeOf(byte));
                 CopyMemory(FCFAPattern, TmcmImageExifInfo(Source).FCFAPattern, FCFA_Size * SizeOf(byte));
            end;
            FCustomRendered           := TmcmImageExifInfo(Source).FCustomRendered;
            FExposureMode             := TmcmImageExifInfo(Source).FExposureMode;
            FWhiteBalance             := TmcmImageExifInfo(Source).FWhiteBalance;
            FDigitalZoomRatio         := TmcmImageExifInfo(Source).FDigitalZoomRatio;
            FFocalLengthIn35mmFilm    := TmcmImageExifInfo(Source).FFocalLengthIn35mmFilm;
            FSceneCaptureType         := TmcmImageExifInfo(Source).FSceneCaptureType;
            FGainControl              := TmcmImageExifInfo(Source).FGainControl;
            FContrast                 := TmcmImageExifInfo(Source).FContrast;
            FSaturation               := TmcmImageExifInfo(Source).FSaturation;
            FSharpness                := TmcmImageExifInfo(Source).FSharpness;
            if (TmcmImageExifInfo(Source).FDSD_Size > 0)
            then begin
                 FDSD_Size := TmcmImageExifInfo(Source).FDSD_Size;
                 GetMem(FDeviceSettingDescription, FDSD_Size * SizeOf(byte));
                 CopyMemory(FDeviceSettingDescription, TmcmImageExifInfo(Source).FDeviceSettingDescription, FDSD_Size * SizeOf(byte));
            end;
            FSubjectDistanceRange     := TmcmImageExifInfo(Source).FSubjectDistanceRange;

            FInteroperabilityIndex    := TmcmImageExifInfo(Source).FInteroperabilityIndex;
            FInteroperabilityVersion  := TmcmImageExifInfo(Source).FInteroperabilityVersion;
       end;
end; // TmcmImageExifInfo.Assign.


function TmcmImageExifInfo.GetComponentConfig(Index : word) : byte;
begin
  if (Index < 4)
  then Result := FComponentConfig[Index]
  else Result := 0;
end; // TmcmImageExifInfo.GetComponentConfig.


function TmcmImageExifInfo.GetCompressdBitPerPixel : double;
begin
  Result := ConvertToDouble(FCompressdBitPerPixel);
end; // TmcmImageExifInfo.GetCompressdBitPerPixel.


function TmcmImageExifInfo.GetExposureTime : double;
begin
  Result := ConvertToDouble(FExposureTime);
end; // TmcmImageExifInfo.GetExposureTime.


function TmcmImageExifInfo.GetFNumber : double;
begin
  Result := ConvertToDouble(FFNumber);
end; // TmcmImageExifInfo.GetFNumber.


function TmcmImageExifInfo.GetShutterSpeed : double;
begin
  Result := ConvertToDouble(FShutterSpeed);
end; // TmcmImageExifInfo.GetShutterSpeed.


function TmcmImageExifInfo.GetAperture : double;
begin
  Result := ConvertToDouble(FAperture);
end; // TmcmImageExifInfo.GetAperture.


function TmcmImageExifInfo.GetBrightness : double;
begin
  Result := ConvertToDouble(FBrightness);
end; // TmcmImageExifInfo.GetBrightness.


function TmcmImageExifInfo.GetExposureBias : double;
begin
  Result := ConvertToDouble(FExposureBias);
end; // TmcmImageExifInfo.GetExposureBias.


function TmcmImageExifInfo.GetMaxAperture : double;
begin
  Result := ConvertToDouble(FMaxAperture);
end; // TmcmImageExifInfo.GetMaxAperture.


function TmcmImageExifInfo.GetSubjectDistance : double;
begin
  Result := ConvertToDouble(FSubjectDistance);
end; // TmcmImageExifInfo.GetSubjectDistance.


function TmcmImageExifInfo.GetFocalLength : double;
begin
  Result := ConvertToDouble(FFocalLength);
end; // TmcmImageExifInfo.GetFocalLength.


function TmcmImageExifInfo.GetFlashEnergy : double;
begin
  Result := ConvertToDouble(FFlashEnergy);
end; // TmcmImageExifInfo.GetFlashEnergy.


function TmcmImageExifInfo.GetFocalPlaneXResolution : double;
begin
  Result := ConvertToDouble(FFocalPlaneXResolution);
end; // TmcmImageExifInfo.GetFocalPlaneXResolution.


function TmcmImageExifInfo.GetFocalPlaneYResolution : double;
begin
  Result := ConvertToDouble(FFocalPlaneYResolution);
end; // TmcmImageExifInfo.GetFocalPlaneYResolution.


function TmcmImageExifInfo.GetExposureIndex : double;
begin
  Result := ConvertToDouble(FExposureIndex);
end; // TmcmImageExifInfo.GetExposureIndex.


function TmcmImageExifInfo.GetDigitalZoomRatio : double;
begin
  Result := ConvertToDouble(FDigitalZoomRatio);
end; // TmcmImageExifInfo.GetDigitalZoomRatio.


function TmcmImageExifInfo.GetSubjectArea(Index : word) : word;
begin
  if (Index < 4)
  then Result := FSubjectArea[Index]
  else Result := 0;
end; // TmcmImageExifInfo.GetSubjectArea.


function TmcmImageExifInfo.GetSubjectLocation(Index : word) : word;
begin
  if (Index < 2)
  then Result := FSubjectLocation[Index]
  else Result := 0;
end; // TmcmImageExifInfo.GetSubjectLocation.


function TmcmImageExifInfo.GetUserComment : WideString;
var i : integer;
begin
  i := -1;
  if (FUserComment <> '')
  then if (Length(FUserComment) > 9)
       then case FUserComment[1] of
            #65 : if (byte(FUserComment[2]) = $53) and
                     (byte(FUserComment[3]) = $43) and
                     (byte(FUserComment[4]) = $49) and
                     (byte(FUserComment[5]) = $49) and
                     (byte(FUserComment[6]) = $0) and
                     (byte(FUserComment[7]) = $0) and
                     (byte(FUserComment[8]) = $0)
                  then i := 0;
            #74 : if (byte(FUserComment[2]) = $49) and
                     (byte(FUserComment[3]) = $53) and
                     (byte(FUserComment[4]) = $0) and
                     (byte(FUserComment[5]) = $0) and
                     (byte(FUserComment[6]) = $0) and
                     (byte(FUserComment[7]) = $0) and
                     (byte(FUserComment[8]) = $0)
                  then i := 1;
            #85 : if (byte(FUserComment[2]) = $4E) and
                     (byte(FUserComment[3]) = $49) and
                     (byte(FUserComment[4]) = $43) and
                     (byte(FUserComment[5]) = $4F) and
                     (byte(FUserComment[6]) = $44) and
                     (byte(FUserComment[7]) = $45) and
                     (byte(FUserComment[8]) = $0)
                  then i := 2;
            #0  : if (byte(FUserComment[2]) = $0) and
                     (byte(FUserComment[3]) = $0) and
                     (byte(FUserComment[4]) = $0) and
                     (byte(FUserComment[5]) = $0) and
                     (byte(FUserComment[6]) = $0) and
                     (byte(FUserComment[7]) = $0) and
                     (byte(FUserComment[8]) = $0)
                  then i := 3;
            end;
  case i of
  0 : begin // ASCII
        Result := PChar(@FUserComment[9])^;
      end;
  1 : begin // JIS
        Result := PWideChar(@FUserComment[9])^;
      end;
  2 : begin // Unicode
        Result := PWideChar(@FUserComment[9])^;
      end;
  3 : begin // Undefined
        Result := PChar(@FUserComment[9])^;
      end;
  else Result := '';
  end;
end; // TmcmImageExifInfo.GetUserComment.

//------------------------------------------------------------------------------
// TmcmImageGPSInfo
//------------------------------------------------------------------------------

constructor TmcmImageGPSInfo.Create;
begin
  Inherited Create;
  FProcessingSize   := 0;
  FAreaSize         := 0;
  FProcessingMethod := Nil;
  FAreaInformation  := Nil;
  Clear;
end; // TmcmImageGPSInfo.Create.


destructor TmcmImageGPSInfo.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmImageGPSInfo.Destroy.


procedure TmcmImageGPSInfo.Clear;
var i : word;
begin
  FModified         := False;       // Is this objects data modified!
  FVersionID        := 0;
  FLatitudeRef      := '';
  for i := 0 to 5
  do if Odd(i)
     then FLatitude[i] := 1
     else FLatitude[i] := -1;
  FLongitudeRef     := '';
  for i := 0 to 5
  do if Odd(i)
     then FLongitude[i] := 1
     else FLongitude[i] := -1;
  FAltitudeRef      := -1;
  FAltitude[0]      := -1;
  FAltitude[1]      := 1;
  for i := 0 to 5
  do if Odd(i)
     then FTimeStamp[i] := 1
     else FTimeStamp[i] := -1;
  FSatellites       := '';
  FStatus           := '';
  FMeasureMode      := '';
  FDOP[0]           := -1;
  FDOP[1]           := 1;
  FSpeedRef         := '';
  FSpeed[0]         := -1;
  FSpeed[1]         := 1;
  FTrackRef         := '';
  FTrack[0]         := -1;
  FTrack[1]         := 1;
  FImgDirectionRef  := '';
  FImgDirection[0]  := -1;
  FImgDirection[1]  := 1;
  FMapDatum         := '';
  FDestLatitudeRef  := '';
  for i := 0 to 5
  do if Odd(i)
     then FDestLatitude[i] := 1
     else FDestLatitude[i] := -1;
  FDestLongitudeRef := '';
  for i := 0 to 5
  do if Odd(i)
     then FDestLongitude[i] := 1
     else FDestLongitude[i] := -1;
  FDestBearingRef   := '';
  FDestBearing[1]   := 0;
  FDestDistanceRef  := '';
  FDestDistance[1]  := 0;
  FDateStamp        := 0;
  FDifferential     := 0;

  if Assigned(FProcessingMethod)
  then FreeMem(FProcessingMethod);
  FProcessingMethod := Nil;
  if Assigned(FAreaInformation)
  then FreeMem(FAreaInformation);
  FAreaInformation  := Nil;
  FProcessingSize   := 0;
  FAreaSize         := 0;
end; // TmcmImageGPSInfo.Clear.


procedure TmcmImageGPSInfo.Assign(Source : TPersistent);
begin
  if Assigned(Source)
  then if (Source is TmcmImageGPSInfo)
       then begin
            Clear;
            FVersionID        := TmcmImageGPSInfo(Source).FVersionID;
            FLatitudeRef      := TmcmImageGPSInfo(Source).FLatitudeRef;
            FLatitude         := TmcmImageGPSInfo(Source).FLatitude;
            FLongitudeRef     := TmcmImageGPSInfo(Source).FLongitudeRef;
            FLongitude        := TmcmImageGPSInfo(Source).FLongitude;
            FAltitudeRef      := TmcmImageGPSInfo(Source).FAltitudeRef;
            FAltitude         := TmcmImageGPSInfo(Source).FAltitude;
            FTimeStamp        := TmcmImageGPSInfo(Source).FTimeStamp;
            FSatellites       := TmcmImageGPSInfo(Source).FSatellites;
            FStatus           := TmcmImageGPSInfo(Source).FStatus;
            FMeasureMode      := TmcmImageGPSInfo(Source).FMeasureMode;
            FDOP              := TmcmImageGPSInfo(Source).FDOP;
            FSpeedRef         := TmcmImageGPSInfo(Source).FSpeedRef;
            FSpeed            := TmcmImageGPSInfo(Source).FSpeed;
            FTrackRef         := TmcmImageGPSInfo(Source).FTrackRef;
            FTrack            := TmcmImageGPSInfo(Source).FTrack;
            FImgDirectionRef  := TmcmImageGPSInfo(Source).FImgDirectionRef;
            FImgDirection     := TmcmImageGPSInfo(Source).FImgDirection;
            FMapDatum         := TmcmImageGPSInfo(Source).FMapDatum;
            FDestLatitudeRef  := TmcmImageGPSInfo(Source).FDestLatitudeRef;
            FDestLatitude     := TmcmImageGPSInfo(Source).FDestLatitude;
            FDestLongitudeRef := TmcmImageGPSInfo(Source).FDestLongitudeRef;
            FDestLongitude    := TmcmImageGPSInfo(Source).FDestLongitude;
            FDestBearingRef   := TmcmImageGPSInfo(Source).FDestBearingRef;
            FDestBearing      := TmcmImageGPSInfo(Source).FDestBearing;
            FDestDistanceRef  := TmcmImageGPSInfo(Source).FDestDistanceRef;
            FDestDistance     := TmcmImageGPSInfo(Source).FDestDistance;
            FProcessingSize   := TmcmImageGPSInfo(Source).FProcessingSize;
            if (FProcessingSize > 0)
            then begin
                 GetMem(FProcessingMethod, FProcessingSize * SizeOf(byte));
                 CopyMemory(FProcessingMethod, TmcmImageGPSInfo(Source).FProcessingMethod, FProcessingSize * SizeOf(byte));
            end;
            FAreaSize         := TmcmImageGPSInfo(Source).FAreaSize;
            if (FAreaSize > 0)
            then begin
                 GetMem(FAreaInformation, FAreaSize * SizeOf(byte));
                 CopyMemory(FAreaInformation, TmcmImageGPSInfo(Source).FAreaInformation, FAreaSize * SizeOf(byte));
            end;
            FDateStamp        := TmcmImageGPSInfo(Source).FDateStamp; // 11
            FDifferential     := TmcmImageGPSInfo(Source).FDifferential;
       end;
end; // TmcmImageGPSInfo.Assign.


function TmcmImageGPSInfo.GetLatitude(Index : word) : double;
begin
  Result := ConvertToDouble([FLatitude[2*index],FLatitude[2*index+1]]);
end; // TmcmImageGPSInfo.GetLatitude.


function TmcmImageGPSInfo.GetLongitude(Index : word) : double;
begin
  Result := ConvertToDouble([FLongitude[2*Index],FLongitude[2*Index+1]]);
end; // TmcmImageGPSInfo.GetLongitude.


function TmcmImageGPSInfo.GetAltitude : double;
begin
  if (FAltitude[1] = 0)
  then Result := -1
  else Result := ConvertToDouble(FAltitude);
end; // TmcmImageGPSInfo.GetAltitude.


function TmcmImageGPSInfo.GetTimeStamp(Index : word) : double;
begin
  Result := ConvertToDouble([FTimeStamp[2*Index],FTimeStamp[2*Index+1]]);
end; // TmcmImageGPSInfo.GetTimeStamp.


function TmcmImageGPSInfo.GetDOP : double;
begin
  Result := ConvertToDouble(FDOP);
end; // TmcmImageGPSInfo.GetDOP.


function TmcmImageGPSInfo.GetSpeed : double;
begin
  Result := ConvertToDouble(FSpeed);
end; // TmcmImageGPSInfo.GetSpeed.


function TmcmImageGPSInfo.GetTrack : double;
begin
  Result := ConvertToDouble(FTrack);
end; // TmcmImageGPSInfo.GetTrack.


function TmcmImageGPSInfo.GetImgDirection : double;
begin
  Result := ConvertToDouble(FImgDirection);
end; // TmcmImageGPSInfo.GetImgDirection.


function TmcmImageGPSInfo.GetDestLatitude(Index : word) : double;
begin
  Result := ConvertToDouble([FDestLatitude[2*Index],FDestLatitude[2*Index+1]]);
end; // TmcmImageGPSInfo.GetDestLatitude.


function TmcmImageGPSInfo.GetDestLongitude(Index : word) : double;
begin
  Result := ConvertToDouble([FDestLongitude[2*Index],FDestLongitude[2*Index+1]]);
end; // TmcmImageGPSInfo.GetDestLongitude.


function TmcmImageGPSInfo.GetDestBearing : double;
begin
  Result := ConvertToDouble(FDestBearing);
end; // TmcmImageGPSInfo.GetDestBearing.


function TmcmImageGPSInfo.GetDestDistance : double;
begin
  Result := ConvertToDouble(FDestDistance);
end; // TmcmImageGPSInfo.GetDestDistance.


//------------------------------------------------------------------------------

procedure FixupBitFields(var DIB : TDIBSection);
begin
  if ((DIB.dsbmih.biCompression and BI_BITFIELDS) <> 0) and
     (DIB.dsBitFields[0] = 0)
  then if (DIB.dsbmih.biBitCount = 16)
       then begin // fix buggy 16 bit color drivers
            DIB.dsBitFields[0] := $F800;
            DIB.dsBitFields[1] := $07E0;
            DIB.dsBitFields[2] := $001F;
       end
       else if (DIB.dsbmih.biBitCount = 32)
            then begin // fix buggy 32 bit color drivers
                 DIB.dsBitFields[0] := $00FF0000;
                 DIB.dsBitFields[1] := $0000FF00;
                 DIB.dsBitFields[2] := $000000FF;
            end;
end; // FixupBitFields.


function CopyBitmap(    Handle         : HBITMAP;
                        OldPal, NewPal : HPALETTE;
                    var DIB            : TDIBSection) : HBITMAP;
var OldScr     : HBITMAP;
    NewScr     : HBITMAP;
    ScreenDC   : HDC;
    NewImageDC : HDC;
    OldImageDC : HDC;
    BI         : PBitmapInfo;
    BitsMem    : Pointer;
    SrcDIB     : TDIBSection;
    Pal1, Pal2 : HPALETTE;
begin
  Result := 0;

  with DIB, dsbm, dsbmih
  do begin
     if (biSize <> 0) and ((biWidth = 0) or (biHeight = 0))
     then Exit;
     if (biSize = 0) and ((bmWidth = 0) or (bmHeight = 0))
     then Exit;
  end;
  SrcDIB.dsbmih.biSize := 0;

  if (Handle <> 0)
  then if GetObject(Handle, sizeof(SrcDIB), @SrcDIB) < sizeof(SrcDIB.dsbm)
       then begin
            Exit; // Bitmap is invalid
       end;

  ScreenDC := GetDC(0);
  NewImageDC := CreateCompatibleDC(ScreenDC);
  with DIB.dsbm
  do try
       if DIB.dsbmih.biSize < DWORD(sizeof(DIB.dsbmih))
       then if (bmPlanes or bmBitsPixel) = 1
            then Result := CreateBitmap(bmWidth, bmHeight, 1, 1, Nil) // monochrome
            else Result := CreateCompatibleBitmap(ScreenDC, bmWidth, bmHeight) // Create DDB
       else begin // Create DIB
            GetMem(BI, SizeOf(TBitmapInfo) + 256 * SizeOf(TRGBQuad));
            with DIB.dsbmih
            do try
                 biSize := sizeof(BI.bmiHeader);
                 biPlanes := 1;
                 if (biBitCount = 0)
                 then biBitCount := GetDeviceCaps(ScreenDC, BITSPIXEL) * GetDeviceCaps(ScreenDC, PLANES);
                 BI.bmiHeader := DIB.dsbmih;
                 bmWidth := biWidth;
                 bmHeight := biHeight;

                 if (biBitCount <= 8)
                 then begin
                      if (biBitCount = 1) and (SrcDIB.dsbm.bmBits = Nil)
                      then begin  // set mono DIB to white/black when converting from DDB.
                           integer(BI^.bmiColors[0]) := 0;
                           PInteger(Integer(@BI^.bmiColors) + sizeof(Integer))^ := $FFFFFF;
                      end
                      else if (NewPal <> 0)
                           then // MCM PaletteToDIBColorTable(NewPal, PRGBQuadArray(@BI.bmiColors)^)
                           else if (Handle <> 0)
                                then begin
                                     NewScr := SelectObject(NewImageDC, Handle);
                                     if (SrcDIB.dsbmih.biSize > 0) and (SrcDIB.dsbm.bmBits <> Nil)
                                     then biClrUsed := GetDIBColorTable(NewImageDC, 0, 256, BI^.bmiColors)
                                     else GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), nil, BI^, DIB_RGB_COLORS);
                                     SelectObject(NewImageDC, NewScr);
                                end;
                 end
                 else if ((biBitCount = 16) or (biBitCount = 32)) and
                         ((biCompression and BI_BITFIELDS) <> 0)
                      then begin
                           FixupBitFields(DIB);
                           Move(DIB.dsBitFields, BI.bmiColors, sizeof(DIB.dsBitFields));
                      end;

                 Result := CreateDIBSection(ScreenDC, BI^, DIB_RGB_COLORS, BitsMem, 0, 0);
                 if (BitsMem = nil)
                 then ;// GDI Error;

                 if (Handle <> 0) and
                    (SrcDIB.dsbm.bmWidth = biWidth) and
                    (SrcDIB.dsbm.bmHeight = biHeight) and
                    (biBitCount > 8)
                 then begin // shortcut bitblt steps
                      GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), BitsMem, BI^, DIB_RGB_COLORS);
                      Exit;
                 end;
               finally
                 FreeMem(BI);
               end;
       end;

       NewScr := SelectObject(NewImageDC, Result);
       try
         try
           Pal1 := 0;
           Pal2 := 0;
           if (NewPal <> 0)
           then begin
                Pal1 := SelectPalette(NewImageDC, NewPal, False);
                RealizePalette(NewImageDC);
           end;
           try
             PatBlt(NewImageDC, 0, 0, bmWidth, bmHeight, WHITENESS);
             if (Handle <> 0)
             then begin
                  OldImageDC := CreateCompatibleDC(ScreenDC);
                  try
                    OldScr := SelectObject(OldImageDC, Handle);
                    if (OldPal <> 0)
                    then begin
                         Pal2 := SelectPalette(OldImageDC, OldPal, False);
                         RealizePalette(OldImageDC);
                    end;
                    BitBlt(NewImageDC, 0, 0, bmWidth, bmHeight, OldImageDC, 0, 0, SRCCOPY);
                    if (OldPal <> 0)
                    then SelectPalette(OldImageDC, Pal2, True);
                    SelectObject(OldImageDC, OldScr);
                  finally
                    DeleteDC(OldImageDC);
                  end;
             end;
           finally
             if (NewPal <> 0)
             then SelectPalette(NewImageDC, Pal1, True);
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

       if (Result <> 0)
       then GetObject(Result, sizeof(DIB), @DIB);
     end;
end; // CopyBitmap.


function TransparentStretchBlt(DstDC  : HDC; DstX, DstY, DstW, DstH : integer;
                               SrcDC  : HDC; SrcX, SrcY, SrcW, SrcH : integer;
                               MaskDC : HDC; MaskX, MaskY : integer): boolean;
const ROP_DstCopy = $00AA0029;
var MemDC   : HDC;
    MemBmp  : HBITMAP;
    Save    : THandle;
    crText  : TColorRef;
    crBack  : TColorRef;
    SavePal : HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH)
  then begin
       MemBmp := CreateCompatibleBitmap(SrcDC, 1, 1);
       MemBmp := SelectObject(MaskDC, MemBmp);
       try
         MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY,
                 MemBmp, MaskX, MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
       finally
         MemBmp := SelectObject(MaskDC, MemBmp);
         DeleteObject(MemBmp);
       end;
       Exit;
  end;
  SavePal := 0;
  MemDC := CreateCompatibleDC(0);
  try
    MemBmp := CreateCompatibleBitmap(SrcDC, SrcW, SrcH);
    Save := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    SelectPalette(SrcDC, SavePal, False);
    if (SavePal <> 0)
    then SavePal := SelectPalette(MemDC, SavePal, True)
    else SavePal := SelectPalette(MemDC, SystemPalette16, True);
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);

    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);

    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if (Save <> 0)
    then SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if (SavePal <> 0)
    then SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end; // TransparentStretchBlt.                                              


function CopyBitmapAsMask(hBmp : HBITMAP; pBmpInfo : PBitmapInfo; hBmpPal : HPALETTE; TransparentColor : TColorRef) : HBITMAP;
var DIB        : TDIBSection;
    ScreenDC   : HDC;
    BitmapDC   : HDC;
    MonoDC     : HDC;
    BkColor    : TColorRef;
    Remove     : Boolean;
    SaveBitmap : HBITMAP;
    SaveMono   : HBITMAP;
    Handle     : HBITMAP;
begin
  Result := 0;
  if (GetObject(hBmp, SizeOf(DIB), @DIB) <> 0)
  then ;
  if (hBmp <> 0) // and (GetObject(Handle, SizeOf(DIB), @DIB) <> 0)
  then begin
       ScreenDC := 0;
       MonoDC   := 0;
       try
         ScreenDC := GetDC(0);
         MonoDC   := CreateCompatibleDC(ScreenDC);
         with pBmpInfo^.bmiHeader
         do begin
            Result := CreateBitmap(biWidth, biHeight, 1, 1, nil);
            if (Result <> 0)
            then begin
                 SaveMono := SelectObject(MonoDC, Result);
                 if TransparentColor = TColorRef(clNone)
                 then PatBlt(MonoDC, 0, 0, biWidth, biHeight, BLACKNESS)
                 else begin
                      BitmapDC := CreateCompatibleDC(ScreenDC);
                      try
                        // Convert DIB to DDB
                        if (DIB.dsBm.bmBits <> Nil)
                        then begin
                             Remove := True;
                             DIB.dsbmih.biSize := 0;
                             Handle := CopyBitmap(hBmp, hBmpPal, hBmpPal, DIB);
                        end
                        else begin
                             Remove := False;
                             Handle := hBmp;
                        end;
                        SaveBitmap := SelectObject(BitmapDC, Handle);
                        if (hBmpPal <> 0)
                        then begin
                             SelectPalette(BitmapDC, hBmpPal, False);
                             RealizePalette(BitmapDC);
                             SelectPalette(MonoDC, hBmpPal, False);
                             RealizePalette(MonoDC);
                        end;

                        BkColor := SetBkColor(BitmapDC, TransparentColor);
                        BitBlt(MonoDC, 0, 0, biWidth, biHeight, BitmapDC, 0, 0, SrcCopy);
                        SetBkColor(BitmapDC, BkColor);

                        if (SaveBitmap <> 0)
                        then SelectObject(BitmapDC, SaveBitmap);

                        if Remove
                        then DeleteObject(Handle);
                      finally
                        DeleteDC(BitmapDC);
                      end;
                 end;
                 if (SaveMono <> 0)
                 then SelectObject(MonoDC, SaveMono);
            end;
         end;
       finally
         if (MonoDC <> 0)
         then DeleteDC(MonoDC);
         if (ScreenDC <> 0)
         then ReleaseDC(0, ScreenDC);
       end;
  end;
end; // TGraphBmp.CopyBitmapAsMask.


//------------------------------------------------------------------------------
// TmcmImage.
//------------------------------------------------------------------------------

constructor TmcmImage.Create;
begin
  Inherited Create;
  {$IFDEF MCMDEMO}
    GetMem(pBmp, 24);
    pBmp^[1]  := $4D;
    pBmp^[2]  := $43;
    pBmp^[3]  := $4D;
    pBmp^[4]  := $20;
    pBmp^[5]  := $44;
    pBmp^[6]  := $45;
    pBmp^[7]  := $53;
    pBmp^[8]  := $49;
    pBmp^[9]  := $47;
    pBmp^[10] := $4E;
    pBmp^[11] := 0;
  {$ENDIF}

  FError       := EC_OK;
  FScale       := 1.0;
  FFlipY       := False;
  FDibHandle   := 0;
  FDibPal      := 0;
  FMaskHandle  := 0;
  FImageFormat := IF_NONE;
  FCompress    := CP_NOCOMP;
  FQuality     := 100;
  FImageInfo   := Nil;
  FSBltMode    := COLORONCOLOR;
  FScaleHVRes  := True;
  GetMem(FDibInfo, SizeOf(TBitmapInfo) + 256 * SizeOf(TRGBQuad));
  Clear;
end; // TmcmImage.Create.


destructor TmcmImage.Destroy;
begin
  {$IFDEF MCMDEMO}
    if (pBmp <> Nil)
    then FreeMem(pBmp, 24);
  {$ENDIF}
  Clear;
  FreeMem(FDibInfo);
  Inherited Destroy;
end; // TmcmImage.Destroy.


procedure TmcmImage.Assign(Source : TPersistent);
var NoColors : integer;
begin
  if (Source is TmcmImage)
  then begin
       FreeContext; 
       Clear;
       NoColors := (Source as TmcmImage).GetNumColors((Source as TmcmImage).FDibInfo);
       CopyMemory(@FDIBSection, @TmcmImage(Source).FDIBSection, SizeOf(FDIBSection));
       CopyMemory(FDibInfo, TmcmImage(Source).FDibInfo, SizeOf(TBitmapInfo) + NoColors * SizeOf(TRGBQuad));
       if (CreateDIBHandle <> 0)
       then CopyMemory(FDibBits, TmcmImage(Source).FDibBits, FDibInfo^.bmiHeader.biSizeImage);

       // Copy ImageInfo
       if ((Source as TmcmImage).FImageInfo <> Nil)
       then ImageInfo.Assign(TmcmImage(Source).FImageInfo);

       // Copy palette handle.
       if (NoColors > 0)
       then FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);

       // Copy Transparency parameters.
       SetTransparent(TmcmImage(Source).FTransparent);
       SetTransparentColor(TmcmImage(Source).FTransColor);
       
       FScaleHVRes := TmcmImage(Source).FScaleHVRes;

       Changed(Self);
  end;
end; // TmcmImage.Assign.


procedure TmcmImage.Clear;
begin
  FreeContext;
  if (FDIBHandle <> 0) and (FRefCount > 0)
  then DeleteObject(FDIBHandle);
  FDibHandle := 0;
  if (FDibPal <> 0)
  then DeleteObject(FDibPal);
  FDibPal := 0;

  if Assigned(FImageInfo)
  then FImageInfo.Free;
  FImageInfo := Nil;

  FillMemory(@FDIBSection, SizeOf(FDIBSection), 0);
  FillMemory(@FDibInfo^.bmiHeader, SizeOf(TBitmapInfo), 0);
  with FDibInfo^.bmiHeader
  do begin
     biSize          := SizeOf(TBitmapInfoHeader);
     biPlanes        := 1;
     biXPelsPerMeter := 0;
     biYPelsPerMeter := 0;
  end;
  FillMemory(@FDibLogPal, SizeOf(FDibLogPal), 0);

  // Canvas.
  if Assigned(FCanvas)
  then FCanvas.Free;
  FCanvas := Nil;

  // Transparent bitmap.
  FTransparent := False;
  FTransColor  := RGB(255, 255, 255);
  if (FMaskHandle <> 0)
  then DeleteObject(FMaskHandle);
  FMaskHandle := 0;

  FxOrg     := 0;
  FyOrg     := 0;
  FRefCount := 0;

  FImageFormat := IF_NONE;
end; // TmcmImage.Clear.


procedure TmcmImage.FreeContext;
begin
  if (FCanvas <> Nil)
  then TmcmImageCanvas(FCanvas).FreeContext;
end; // TmcmImage.FreeContext.


function TmcmImage.GetCanvas : TCanvas;
begin
  if (FCanvas = nil)
  then begin
       FCanvas := TmcmImageCanvas.Create(Self);
       // FCanvas.OnChange   := Changed;
       // FCanvas.OnChanging := Changing;
  end;
  Result := FCanvas;
end; // TmcmImage.GetCanvas.


procedure TmcmImage.Changed(Sender : TObject);
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
end; // TmcmImage.Changed.


function TmcmImage.GetEmpty : boolean;
begin
  Result := (FDibHandle = 0)
end; // TmcmImage.GetEmpty.


procedure TmcmImage.SetModified(Value : boolean);
begin
  FModified := Value;
  if FModified
  then Changed(Self);
end; // TmcmImage.SetModified.


function TmcmImage.CreateDIBHandle : HBITMAP;
var dibDC       : HDC;
    hNewSection : {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.HBITMAP;
begin
  if (FDibInfo^.bmiHeader.biHeight <> 0) and
     (FDibInfo^.bmiHeader.biWidth <> 0) and
     (FDibInfo^.bmiHeader.biBitCount <> 0)
  then begin
       with FDibInfo^.bmiHeader
       do begin
          biSize          := SizeOf(TBitmapInfoHeader);
          biPlanes        := 1;
          FBitCount       := biPlanes * biBitCount;
          FLongWidth      := ((longint(biWidth * FBitCount) + 31) div 32) * 4;
          biSizeImage     := DWORD(FLongWidth * Abs(biHeight));
          biCompression   := 0;
          biClrUsed       := 0;
          biClrImportant  := 0;
       end;

       dibDC := GetDC(0);
       try
         hNewSection := CreateDIBSection(dibDC, FDibInfo^, DIB_RGB_COLORS, FDibBits, 0, 0);
         if (FDibBits <> Nil)
         then begin
              if (FDibHandle <> 0)
              then DeleteObject(FDibHandle);
              FDibHandle := hNewSection;
              if (GetObject(FDibHandle, SizeOf(FDIBSection), @FDIBSection) <> 0)
              then begin
                   // FillMemory(FDibBits, FDIBSection.dsBmih.biSizeImage, 0);
                   inc(FRefCount);
              end
              else FError := EC_BADIMAGEHANDLE;
         end;
       finally
         ReleaseDC(0, dibDC);
         if (FDibHandle = 0)
         then begin
              FError := EC_WINDOWS;
              FWinError := GetLastError;
         end;
       end;
       Result := hNewSection;
  end
  else Result := 0;
end; // TmcmImage.CreateDIBHandle.


function TmcmImage.GetNumColors(pDibInfo : PBitmapInfo) : integer;
begin
  Result := 0;
  if (pDibInfo^.bmiHeader.biBitCount <= 8)
  then Result := 1 shl pDibInfo^.bmiHeader.biBitCount
  else if (pDibInfo^.bmiHeader.biClrUsed > 0)
       then Result := pDibInfo^.bmiHeader.biClrUsed;
end; // TmcmImage.GetNumColors.


procedure TmcmImage.SetDIB(Value : HBITMAP);
var NoColors : integer;
begin
  if (Value <> 0)
  then begin
       // Get bitmap information.
       if (GetObject(Value, SizeOf(TDIBSection), @FDIBSection) <> 0)
       then begin
            FDIBSection.dsBmih.biCompression := BI_RGB;
            FDibHandle := Value;

            // Copy BitmapInfoHeader and pointer to bitmap bits.
            CopyMemory(@FDibInfo^.bmiHeader, @FDIBSection.dsBmih, SizeOf(TBitmapInfoHeader));
            FDibBits := FDIBSection.dsBm.bmBits;
  {$IFDEF MCMDEMO} {$IFNDEF VER100}
  FDibBits := Pointer(integer(FDibBits) and integer(pFileSup));
  {$ENDIF} {$ENDIF}
            with FDibInfo^.bmiHeader
            do begin
               FBitCount := biPlanes * biBitCount;
               FLongWidth := (((biWidth * FBitCount) + 31) div 32) * 4;
               biSizeImage := DWORD(FLongWidth * Abs(biHeight));
            end;
            NoColors := GetNumColors(@FDIBSection.dsBmih);

            // Get palette handle.
            if (NoColors > 0)
            then FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);

            inc(FRefCount);
       end
       else Clear;
  end;
  Changed(Self);
end; // TmcmImage.SetDIB.


function TmcmImage.GetPalFromDibHandle(var ColorTable : array of TRGBQuad) : HPALETTE;
var dibDC       : HDC;
    SaveDib     : THandle;
    i, NoColors : integer;
begin
  Result := 0;
  FDibLogPal.palVersion := $300;
  NoColors := High(ColorTable) + 1;
  if {(FDIBHandle <> 0) and} (0 < NoColors) and (NoColors <= 256)
  then begin
       dibDC := CreateCompatibleDC(0);
       SaveDib := SelectObject(dibDC, FDIBHandle);
       FDibLogPal.palNumEntries := GetDIBColorTable(dibDC, 0, NoColors, ColorTable);
       SelectObject(dibDC, SaveDib);
       DeleteDC(dibDC);

       if (FDibLogPal.palNumEntries > 0)
       then begin
            if (NoColors > FDibLogPal.palNumEntries)
            then NoColors := FDibLogPal.palNumEntries;
            for i := 0 to (NoColors - 1)
            do begin
               FDibLogPal.palPalEntry[i].peRed   := ColorTable[i].rgbRed;
               FDibLogPal.palPalEntry[i].peGreen := ColorTable[i].rgbGreen;
               FDibLogPal.palPalEntry[i].peBlue  := ColorTable[i].rgbBlue;
            end;
            Result := CreatePalette(PLogPalette(@FDibLogPal)^);
       end;
  end;
end; // TmcmImage.GetPalFromDibHandle.


procedure TmcmImage.SetPalToDibHandle(ColorTable : array of TRGBQuad; NoColors : integer);
var dibDC    : HDC;
    SaveDib  : THandle;
    Count    : integer;
begin
  FreeContext;
  // NoColors := High(ColorTable) + 1;
  if (FDIBHandle <> 0) and (0 < NoColors) and (NoColors <= 256)
  then begin
       dibDC := CreateCompatibleDC(0);
       SaveDib := SelectObject(dibDC, FDIBHandle);
       Count := SetDIBColorTable(dibDC, 0, NoColors, ColorTable);
       if (Count <> NoColors)
       then ;
       SelectObject(dibDC, SaveDib);
       DeleteDC(dibDC);
  end;
  FImageFormat := IF_NONE;
end; // TmcmImage.SetPalToDibHandle.


function TmcmImage.GetColorTableFromPal(Pal : HPALETTE; var ColorTable : array of TRGBQuad) : integer;
begin
  Result := 0;
  // Get number of entries in palette.
  if (Pal = 0) or (GetObject(Pal, sizeof(Result), @Result) = 0)
  then Exit;
  // Check if ColorTable has a sufficient size, and copy the number of
  // entries that ColorTable can hold.
  if (Result > (High(ColorTable) + 1))
  then Result := High(ColorTable) + 1;
  Result := {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.GetPaletteEntries(Pal, 0, Result, ColorTable);
end; // TmcmImage.GetColorTableFromPal.


procedure TmcmImage.SetDibHandle(Value : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF});
begin
  Clear;
  SetDIB(Value);
end; // TmcmImage.SetDibHandle.


function TmcmImage.GetDibHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
begin
  // As the user might modify the contents of HBITMAP it must not be shared by
  // another TmcmImage when returned to the user nor should it be selected into
  //a DC.
  FreeContext;
  Result := FDibHandle;
end; // TmcmImage.GetDibHandle.


function TmcmImage.ReleaseHandle : {$IFDEF BCB} TmcmHBITMAP {$ELSE} HBITMAP {$ENDIF};
// ReleaseHandle releases the TmcmImage ownership of the HBITMAP handle.
begin
  // Release the bitmap handle from this instance.
  FRefCount := 0;
  FreeContext;

  // Return Bitmap handle.
  Result := FDibHandle;
end; // TmcmImage.ReleaseHandle.


function TmcmImage.GetDibBits : pointer;
begin
  Result := FDibBits;
end; // TmcmImage.GetDibBits.


procedure TmcmImage.SetDibBits(Value : pointer);
begin
  if (FDibHandle <> 0)
  then Clear;
  FDibBits := Value;
end; // TmcmImage.SetDibBits.


function TmcmImage.GetDibInfo : PBitmapInfo;
begin
  Result := FDibInfo;
end; // TmcmImage.GetDibInfo.


procedure TmcmImage.SetDibInfo(Value : PBitmapInfo);
begin
  if (FDibHandle <> 0)
  then Clear;
  FDibInfo := Value;
end; // TmcmImage.SetDibInfo.


procedure TmcmImage.FillAll(Value : byte);
begin
  // Fills the entire image with Value.
  if (FDibHandle <> 0)   and
     (FDibBits   <> Nil) and
     (FDibInfo   <> Nil)
  then FillMemory(FDibBits, FDibInfo^.bmiHeader.biSizeImage, Value);
end; // TmcmImage.FillAll.


procedure TmcmImage.FillRGB(rgbColor : TColor);
var i     : integer;
    pRGB  : PVectorRGB;
    pRGBA : PVectorRGBA;
    pT    : PVectorB;
    rgb   : TRGBTriple;
    rgba  : TRGBQuad;
begin
  case Self.ImageFormat of
  IF_BW   : begin
              // Find color index in palette and use this to
              // fill.
              i := GetNearestPaletteIndex(rgbColor);
              if (i = 0)
              then FillAll(0)
              else FillAll($FF);
            end;
  IF_GREY8,
  IF_PAL8 : begin
              // Find color index in palette and use this to
              // fill.
              i := GetNearestPaletteIndex(rgbColor);
              FillAll(i and $FF);
            end;
{
  IF_GREY8 : FillAll(((rgbColor and $FF) +
                      ((rgbColor shr 8) and $FF) +
                      ((rgbColor shr 16) and $FF)) div 3);
}
  IF_RGB24,
  IF_RGBA32 : if ((rgbColor and $FF) = ((rgbColor shr 8) and $FF)) and
                ((rgbColor and $FF) = ((rgbColor shr 16) and $FF))
              then FillAll(rgbColor and $FF) // R=G=B
              else begin
                   pRGB := pointer(ScanLine[0]);
                   pRGBA := pointer(pRGB);
                   if (FDibInfo^.bmiHeader.biBitCount = 24)
                   then begin
                        rgb.rgbtRed   := rgbColor and $FF;
                        rgb.rgbtGreen := (rgbColor shr 8) and $FF;
                        rgb.rgbtBlue  := (rgbColor shr 16) and $FF;
                        for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                        do pRGB^[i] := rgb;
                   end
                   else begin
                        rgba.rgbRed   := rgbColor and $FF;
                        rgba.rgbGreen := (rgbColor shr 8) and $FF;
                        rgba.rgbBlue  := (rgbColor shr 16) and $FF;
                        rgba.rgbReserved := (rgbColor shr 24) and $FF;
                        for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                        do pRGBA^[i] := rgba;
                   end;
                   for i := 1 to (FDibInfo^.bmiHeader.biHeight - 1)
                   do begin
                      pT := ScanLine[i];
                      CopyMemory(pT, pRGB, FLongWidth);
                   end;
             end;
  end;
end; // TmcmImage.FillRGB.


function TmcmImage.SwapRBValues(const Value : TRGBQuad) : TRGBQuad;
var r, b : byte;
    RGB  : TRGBQuad;
begin
  RGB := Value;
  b := RGB.rgbBlue;
  r := RGB.rgbRed;
  RGB.rgbBlue := r;
  RGB.rgbRed := b;
  Result := RGB;
end; // TmcmImage.SwapRBValues.


procedure TmcmImage.CopyFormat(Source : TmcmImage);
var NoColors, i : integer;
    bTemp       : byte;
begin
  if Assigned(Source)
  then begin
       FreeContext;
       
       FImageFormat := Source.FImageFormat;
       FDibInfo^.bmiHeader.biHeight := Source.Height;
       FDibInfo^.bmiHeader.biWidth := Source.Width;
       FDibInfo^.bmiHeader.biBitCount := Source.BitCount;
       FDibInfo^.bmiHeader.biPlanes := 1;
       if (CreateDIBHandle <> 0)
       then Changed(Self);

       XResolution := Source.XResolution;
       YResolution := Source.YResolution;

       // Get Palette entries from FDibPal.
       NoColors := GetColorTableFromPal(Source.Palette, PRGBQuadArray(@FDibInfo^.bmiColors)^);
       if (NoColors > 0)
       then begin
            // A windows TPaletteEntry structure was returned, where R & B are
            // swap compared to TRGBQuad.
            for i := 0 to (NoColors - 1)
            do begin
               bTemp := FDibInfo^.bmiColors[i].rgbBlue;
               FDibInfo^.bmiColors[i].rgbBlue := FDibInfo^.bmiColors[i].rgbRed;
               FDibInfo^.bmiColors[i].rgbRed := bTemp;
            end;

            // Set palette to Dib
            SetPalToDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^, NoColors);

            // Get palette handle.
            FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);
       end;
  end;
end; // TmcmImage.CopyFormat.


procedure TmcmImage.CopyRegion(var Target : TmcmImage; Region : TRect);
var NoColors   : integer;
    ys, yt     : cardinal;
    dx, dy     : longint;
    x, sx      : longint;
    i, j, k    : longint;
    jm         : byte;
    pS, pT     : PVectorB;
begin
  // Check that Region is within this image area.
  if (Region.Left < 0)
  then Region.Left := 0;
  if (Region.Top < 0)
  then Region.Top := 0;
  if (Region.Right >= Width)
  then Region.Right := Width - 1;
  if (Region.Bottom >= Height)
  then Region.Bottom := Height - 1;

  dx := abs(Region.Right - Region.Left) + 1;
  dy := abs(Region.Bottom - Region.Top) + 1;

  // If Target is unassigned create a new image.
  if (Target = Nil)
  then begin
       Target := TmcmImage.Create;
       Target.Width := dx;
       Target.Height := dy;
       Target.BitCount := Self.BitCount;
       Target.ImageFormat := Self.ImageFormat;

       // Get palette entries if any.
       NoColors := GetPaletteEntries(Nil);
       if (NoColors > 0)
       then CopyMemory(@Target.FDibInfo^.bmiColors, @FDibInfo^.bmiColors, NoColors * SizeOf(TRGBQuad));
  end
  else begin
       // Check that Target image has the correct physical dimention.
       if (Target.Width <> dx)
       then Target.Width := dx;
       if (Target.Height <> dy)
       then Target.Height := dy;

       // Check if Target has the same ImageFormat and if not set this image format.
       if (Target.ImageFormat <> Self.ImageFormat)
       then Target.ImageFormat := Self.ImageFormat;
  end;

  // Copy pixel data.
  yt := 0;
  if (Region.Left = 0) and (Region.Right = Width - 1)
  then k := 8
  else k := FBitCount;
  case k of
  1 : begin
        // Get byte index.
        x := Region.Left div 8;

        // Calc bit index and bit masks.
        i  := Region.Left mod 8;
        j := 8 - i;
        jm := (1 shl j) - 1;

        // Calc bytes per line to copy.
        if ((dx mod 8) = 0)
        then dx := dx div 8
        else dx := 1 + dx div 8;

        for ys := Region.Top to (Region.Bottom)
        do begin
           pS := ScanLine[ys];
           pT := Target.ScanLine[yt];
           sx := x;
           for k := 1 to dx
           do begin
              pT^[0] := (pS^[x] and jm) shl i;
              inc(x);
              pT^[0] := pT^[0] or (pS^[x] shr j);
              inc(pT);
           end;
           x := sx;
           inc(yt);
        end;
      end;
  4 : begin
        x := Region.Left div 2;
        i := dx;
        dx := dx div 2;
        for ys := Region.Top to (Region.Bottom)
        do begin
           pS := ScanLine[ys];
           pT := Target.ScanLine[yt];
           if Odd(Region.Left)
           then begin
                sx := x;
                for k := 1 to dx
                do begin
                   pT^[0] := (pS^[x] and $0F) shl 4;
                   inc(x);
                   pT^[0] := pT^[0] or (pS^[x] shr 4);
                   inc(pT);
                end;
                if Odd(Region.Right)
                then pT^[0] := (pS^[x] and $0F) shl 4;
                x := sx;
           end
           else begin
                CopyMemory(pT, @pS^[x], dx);
                if Odd(i)
                then pT^[dx] := pS^[x+dx] and $F0;
           end;
           inc(yt);
        end;
      end;
  else begin
       x := Region.Left * BitCount div 8;
       dx := dx * BitCount div 8;
       for ys := Region.Top to (Region.Bottom)
       do begin
          pS := ScanLine[ys];
          pT := Target.ScanLine[yt];
          CopyMemory(pT, @pS^[x], dx);
          inc(yt);
       end;
  end;
  end;
end; // TmcmImage.CopyRegion.


procedure TmcmImage.PasteRegion(Source : TmcmImage; const Region : TRect);
var ys, yt, y   : longint;
    dbx, dx, dy : longint;
    x, sx       : longint;
    i, j, k     : longint;
    im, jm      : byte;
    eb          : integer;
    eim, ejm    : byte;
    xOfs, yOfs  : longint;
    pS, pT      : PVectorB;
    LocalRegion : TRect;
begin
  if Assigned(Source)
  then begin
       if Not(Source.Empty)
       then begin
            LocalRegion := Region;

            if (LocalRegion.Left < 0)
            then LocalRegion.Left := 0;
            if (LocalRegion.Right < 0)
            then LocalRegion.Right := 0;
            if (LocalRegion.Left >= Width)
            then LocalRegion.Left := Width - 1;
            if (LocalRegion.Right >= Width)
            then LocalRegion.Right := Width - 1;

            if (LocalRegion.Top < 0)
            then LocalRegion.Top := 0;
            if (LocalRegion.Bottom < 0)
            then LocalRegion.Bottom := 0;
            if (LocalRegion.Top >= Height)
            then LocalRegion.Top := Height - 1;
            if (LocalRegion.Bottom >= Height)
            then LocalRegion.Bottom := Height - 1;

            if (LocalRegion.Left > LocalRegion.Right) or
               (LocalRegion.Top > LocalRegion.Bottom)
            then exit;

            // Get region width and height
            dx := LocalRegion.Right - LocalRegion.Left + 1;
            dy := LocalRegion.Bottom - LocalRegion.Top + 1;

            // Make sure that source Height and Width is not exceeded.
            if (dx > Source.Width)
            then dx := Source.Width;
            if (dy > Source.Height)
            then dy := Source.Height;

            // Make sure that destination Height and Width is not exceeded.
            if (dx > Width)
            then dx := Width;
            if (dy > Height)
            then dy := Height;

            // Adjust Source Image x and y offset.
            if (Region.Left < 0)
            then xOfs := Abs(Region.Left)
            else xOfs := 0;
            if (Region.Top < 0)
            then yOfs := Abs(Region.Top)
            else yOfs := 0;

            ys := yOfs;
            yt := LocalRegion.Top;

            if (Region.Left = 0) and (Region.Right = Width - 1)
            then k := 8
            else k := FBitCount;
            case k of
            1 : begin
                  // Get byte index.
                  x := LocalRegion.Left div 8;

                  // Calc bytes per line to copy.
                  dbx := dx div 8;

                  // Calc bit index and bit masks.
                  if (Region.Left < 0)
                  then i := 8 + (Region.Left mod 8)
                  else i := LocalRegion.Left mod 8;
                  im := (1 shl i) - 1;
                  j := 8 - i;
                  jm := (1 shl j) - 1;
                  im := im shl j;

                  eim := 0;
                  ejm := 0;
                  eb := (dx mod 8);

                  if (Region.Left < 0)
                  then begin // Source image is placed at negative x position.
                       // Calc bit indexs and masks for last "bit-pixels".
                       if (eb <> 0)
                       then begin
                            if (eb <= i)
                            then ejm := (1 shl (8 - eb)) - 1
                            else ejm := (1 shl j) - 1;
                            //dec(eb, i);
                            if (eb - i > 0)
                            then eim := ((1 shl i) - 1) shl j or ((1 shl (8 - eb)) - 1);
                       end;

                       for y := 0 to (dy - 1)
                       do begin
                          if (0 <= yt) and (yt < Height)
                          then begin
                               pT := ScanLine[yt];
                               pS := Source.ScanLine[ys];
                               inc(pS, xOfs div 8);
                               sx := x;

                               for k := 1 to dbx
                               do begin
                                  // Copy MSB from pS to LSB in pT
                                  pT^[x] := (pT^[x] and jm) or (pS^[0] shl j);
                                  inc(pS);

                                  // Copy LSB from pS to MSB in pT
                                  pT^[x] := (pT^[x] and im) or (pS^[0] shr i);
                                  inc(x);
                               end;

                               // Copy LSB from pS to MSB in pT
                               if (ejm <> 0)
                               then pT^[x] := (pT^[x] and ejm) or (Not(ejm) and (pS^[0] shl j));

                               // Copy last bits from pS
                               // Copy MSB from pS to LSB in pT
                               if (eim <> 0)
                               then begin
                                    inc(pS);
                                    pT^[x] := (pT^[x] and eim) or (Not(eim) and (pS^[0] shr i));

                               end;

                               x := sx;
                               inc(ys);
                               inc(yt);
                            end;
                       end;
                  end
                  else begin
                       // Calc bit indexs and masks for last "bit-pixels".
                       if (eb <> 0)
                       then begin
                            if (eb >= j)
                            then eim := im
                            else eim := im + ((1 shl (j - eb)) - 1);
                            dec(eb, j);
                            if (eb > 0)
                            then ejm := (1 shl (8 - eb)) - 1;
                       end;

                       for y := 0 to (dy - 1)
                       do begin
                          if (0 <= yt) and (yt < Height)
                          then begin
                               pT := ScanLine[yt];
                               pS := Source.ScanLine[ys];
                               sx := x;

                               for k := 1 to dbx
                               do begin
                                  // Copy MSB from pS to LSB in pT
                                  pT^[x] := (pT^[x] and im) or (pS^[0] shr i);
                                  inc(x);

                                  // Copy LSB from pS to MSB in pT
                                  pT^[x] := (pT^[x] and jm) or (pS^[0] shl j);
                                  inc(pS);
                               end;

                               // Copy last bits from pS
                               // Copy MSB from pS to LSB in pT
                               if (eim <> 0)
                               then pT^[x] := (pT^[x] and eim) or (Not(eim) and (pS^[0] shr i));

                               // Copy LSB from pS to MSB in pT
                               if (ejm <> 0)
                               then begin
                                    inc(x);
                                    pT^[x] := (pT^[x] and ejm) or (Not(ejm) and (pS^[0] shl j));
                               end;

                               x := sx;
                               inc(ys);
                               inc(yt);
                            end;
                       end;
                  end;
                end;
            4 : begin
                  x  := LocalRegion.Left div 2;
                  dbx := dx div 2;

                  // Copy pixel data.
                  for y := 0 to (dy - 1)
                  do begin
                     if (0 <= yt) and (yt < Height)
                     then begin
                          pT := ScanLine[yt];
                          pS := Source.ScanLine[ys];
                          inc(pS, (xOfs div 2));

                          if Odd(Region.Left)
                          then begin
                               sx := x;
                               if Odd(xOfs)
                               then begin
                                    pT^[x] := (pT^[x] and $0F) or (pS^[0] shl 4);
                                    inc(pS);
                               end;

                               for k := 1 to dbx
                               do begin
                                  pT^[x] := (pT^[x] and $F0) or (pS^[0] shr 4);
                                  inc(x);
                                  pT^[x] := (pT^[x] and $0F) or (pS^[0] shl 4);
                                  inc(pS);
                               end;

                               if Odd(dx) and Not(Odd(xOfs))
                               then pT^[x] := (pT^[x] and $F0) or (pS^[0] shr 4);
                               x := sx;
                          end
                          else begin
                               CopyMemory(@pT^[x], @pS^[0], dbx);
                               if Odd(dx)
                               then pT^[dbx+x] := (pT^[dbx+x] and $0F) or (pS^[dbx] and $F0);
                          end;
                     end;
                     inc(ys);
                     inc(yt);
                  end;
                end;
            else begin
                 x  := LocalRegion.Left * FBitCount div 8;
                 case FBitCount of
                 1 : begin
                       if (dx mod 8 = 0)
                       then dx := dx div 8
                       else dx := 1 + dx div 8;
                     end;
                 4 : if Odd(dx)
                     then dx := 1 + dx * FBitCount div 8
                     else dx := dx * FBitCount div 8;
                 else dx := dx * FBitCount div 8;
                 end;
                 xOfs := xOfs * FBitCount div 8;

                 // Copy pixel data.
                 for y := 0 to (dy - 1)
                 do begin
                    if (0 <= yt) and (yt < Height)
                    then begin
                         pT := ScanLine[yt];
                         pS := Source.ScanLine[ys];
                         CopyMemory(@pT^[x], @pS^[xOfs], dx);
                    end;
                    inc(ys);
                    inc(yt);
                 end;
                 end;
            end;
            Changed(Self);
       end;
  end;
end; // TmcmImage.PasteRegion.


function TmcmImage.GetCompress : TmcmCompress;
begin
  Result := FCompress;
end; // TmcmImage.GetCompress.


procedure TmcmImage.SetCompress(Value : TmcmCompress);
begin
  if (FCompress <> Value)
  then begin
       FCompress := Value;
       FModified := True;
  end;
end; // TmcmImage.SetCompress.


procedure TmcmImage.SetInterlaced(Value : boolean);
begin
  if (FInterlaced <> Value)
  then begin
       FInterlaced := Value;
       FModified := True;
  end;
end; // TmcmImage.SetInterlaced.


procedure TmcmImage.SetQuality(Value : word);
begin
  if (Value < 1)
  then Value := 1;
  if (Value > 100)
  then Value := 100;
  if (FQuality <> Value)
  then begin
       FQuality := Value;
       FModified := True;
  end;
end; // TmcmImage.SetQuality.


procedure TmcmImage.FileOpen(Filename : string);
var NewDib       : HBITMAP;
    SaveOnChange : TNotifyEvent;
begin
  NewDib := ImageFileManager.LoadImage(Filename, FF_DETECT);
  FError := ImageFileManager.Error;
  if (NewDib <> 0)
  then begin
       SaveOnChange := FOnChange;
       FOnChange := Nil;
       SetDibHandle(NewDib);
       ImageInfo.Assign(ImageFileManager.ImageInfo);
       FCompress := ImageFileManager.Compression;
       FQuality  := ImageFileManager.Quality;
       FInterlaced := ImageFileManager.Interlaced;
       if Assigned(FDibInfo)
       then begin
            with FDibInfo^.bmiHeader
            do begin
               ImageFileManager.ImageInfo.Units := UN_METERS;
               biXPelsPerMeter := Round(ImageFileManager.ImageInfo.xResolution);
               biYPelsPerMeter := Round(ImageFileManager.ImageInfo.yResolution);
            end;
       end;
       FOnChange := SaveOnChange;
       Changed(Self);
  end;
end; // TmcmImage.FileOpen.


procedure TmcmImage.FilePageOpen(Filename : string; PageNumber : integer);
var NewDib       : HBITMAP;
    SaveOnChange : TNotifyEvent;
begin
  NewDib := ImageFileManager.LoadNext(Filename, FF_DETECT, PageNumber);
  FError := ImageFileManager.Error;
  if (NewDib <> 0)
  then begin
       SaveOnChange := FOnChange;
       FOnChange := Nil;
       SetDibHandle(NewDib);
       ImageInfo.Assign(ImageFileManager.ImageInfo);
       FCompress := ImageFileManager.Compression;
       FQuality  := ImageFileManager.Quality;
       FInterlaced := ImageFileManager.Interlaced;
       if Assigned(FDibInfo)
       then begin
            with FDibInfo^.bmiHeader
            do begin
               ImageFileManager.ImageInfo.Units := UN_METERS;
               biXPelsPerMeter := Round(ImageFileManager.ImageInfo.xResolution);
               biYPelsPerMeter := Round(ImageFileManager.ImageInfo.yResolution);
            end;
       end;
       FOnChange := SaveOnChange;
       Changed(Self);
  end;
end; // TmcmImage.FilePageOpen.


procedure TmcmImage.FileSave(Filename : string);
begin
 {$IFDEF MCMDEMO}
   Canvas.Brush.Style := bsClear;
   Canvas.Font.Size := 16;
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(255, 255, 255);
   Canvas.TextOut(11, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(11, 9, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 9, PChar(@pBmp^[1]));
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(0, 0, 0);
   Canvas.TextOut(10, 10, PChar(@pBmp^[1]));
 {$ENDIF}
  if Assigned(FDibInfo)
  then begin
       if Assigned(FImageInfo)
       then begin
             with FDibInfo^.bmiHeader
             do begin
                FImageInfo.Units := UN_METERS;
                FImageInfo.xResolution := biXPelsPerMeter;
                FImageInfo.yResolution := biYPelsPerMeter;
             end;
            ImageFileManager.ImageInfo.Assign(ImageInfo);
       end;
  end;
  FreeContext;
  ImageFileManager.Compression := FCompress;
  ImageFileManager.Quality := FQuality;
  ImageFileManager.Interlaced := FInterlaced;
  ImageFileManager.SaveImage(Filename, FF_DETECT, FDibHandle);
  FError := ImageFileManager.Error;

  if (ImageFileManager.Error = EC_OK)
  then if Assigned(FImageInfo)
       then begin
            FImageInfo.FFileFormat := ImageFileManager.GetFormatFromName(Filename);
            FImageInfo.FFileName := FileName;
            FImageInfo.FCompress := FCompress;
            FImageInfo.FCompQuality := FQuality;
       end;
end; // TmcmImage.FileSave.


procedure TmcmImage.FileAppend(Filename : string);
begin
 {$IFDEF MCMDEMO}
   Canvas.Brush.Style := bsClear;
   Canvas.Font.Size := 16;
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(255, 255, 255);
   Canvas.TextOut(11, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(11, 9, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 9, PChar(@pBmp^[1]));
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(0, 0, 0);
   Canvas.TextOut(10, 10, PChar(@pBmp^[1]));
 {$ENDIF}
  if Assigned(FDibInfo)
  then begin
       if Assigned(FImageInfo)
       then ImageFileManager.ImageInfo.Assign(ImageInfo);
       with FDibInfo^.bmiHeader
       do begin
          ImageFileManager.ImageInfo.Units := UN_METERS;
          ImageFileManager.ImageInfo.xResolution := biXPelsPerMeter;
          ImageFileManager.ImageInfo.yResolution := biYPelsPerMeter;
       end;
  end;
  FreeContext;
  ImageFileManager.Compression := FCompress;
  ImageFileManager.Quality := FQuality;
  ImageFileManager.Interlaced := FInterlaced;
  ImageFileManager.AppendImage(Filename, FF_DETECT, FDibHandle);
  FError := ImageFileManager.Error;
end; // TmcmImage.FileAppend.


procedure TmcmImage.LoadFromStream(Stream : TStream);
begin
  LoadFromStreamEx(Stream, FF_BMP);
  FCompress := CP_NOCOMP;
end; // TmcmImage.LoadFromStream.


function TmcmImage.LoadFromStreamEx(Stream     : TStream;
                                    FileFormat : TmcmFileFormat) : TmcmErrorCode;
var NewDib       : HBITMAP;
    SaveOnChange : TNotifyEvent;
begin
  NewDib := ImageFileManager.LoadFromStream(Stream, FileFormat);
  if (NewDib <> 0)
  then begin
       SaveOnChange := FOnChange;
       FOnChange := Nil;
       SetDibHandle(NewDib);
       ImageInfo.Assign(ImageFileManager.ImageInfo);
       FCompress := ImageFileManager.Compression;
       FQuality  := ImageFileManager.Quality;
       FInterlaced := ImageFileManager.Interlaced;
       if Assigned(FDibInfo)
       then begin
            with FDibInfo^.bmiHeader
            do begin
               ImageFileManager.ImageInfo.Units := UN_METERS;
               biXPelsPerMeter := Round(ImageFileManager.ImageInfo.xResolution);
               biYPelsPerMeter := Round(ImageFileManager.ImageInfo.yResolution);
            end;
       end;
       FOnChange := SaveOnChange;
       Changed(Self);
  end;
  FError := ImageFileManager.Error;
  Result := FError;
  FModified := False;
end; // TmcmImage.LoadFromStreamEx.


procedure TmcmImage.SaveToStream(Stream : TStream);
begin
  FCompress := CP_NOCOMP;
  SaveToStreamEx(Stream, FF_BMP);
end; // TmcmImage.SaveToStream.


function TmcmImage.SaveToStreamEx(Stream     : TStream;
                                  FileFormat : TmcmFileFormat) : TmcmErrorCode;
begin
 {$IFDEF MCMDEMO}
   Canvas.Brush.Style := bsClear;
   Canvas.Font.Size := 16;
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(255, 255, 255);
   Canvas.TextOut(11, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(11, 9, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 9, PChar(@pBmp^[1]));
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(0, 0, 0);
   Canvas.TextOut(10, 10, PChar(@pBmp^[1]));
 {$ENDIF}
  if Assigned(FImageInfo)
  then ImageFileManager.ImageInfo.Assign(ImageInfo);
  if Assigned(FDibInfo)
  then begin
       with FDibInfo^.bmiHeader
       do begin
          ImageFileManager.ImageInfo.Units := UN_METERS;
          ImageFileManager.ImageInfo.xResolution := biXPelsPerMeter;
          ImageFileManager.ImageInfo.yResolution := biYPelsPerMeter;
       end;
  end;
  FreeContext;
  ImageFileManager.Compression := FCompress;
  ImageFileManager.Quality := FQuality;
  ImageFileManager.Interlaced := FInterlaced;
  ImageFileManager.SaveToStream(Stream, FileFormat, FDibHandle);
  FError := ImageFileManager.Error;
  if (FError = EC_OK)
  then FModified := False;
  Result := FError;
end; // TmcmImage.SaveToStreamEx.


procedure TmcmImage.LoadFromClipboardFormat(AFormat : word;
                                            AData   : THandle);
var TempDIB    : TDIBSection;
    NewHandle  : THandle;
    NewPalette : HPALETTE;         
    pPal       : Pointer;
    HeaderSize : integer;
    NoColors   : integer;
    pMem       : pointer;
    pBmpih     : pBitmapInfoHeader;
    pBmpch     : pBitmapCoreHeader;
begin
  // Supports CF_BITMAP and CF_DIB (indirectly CF_PALETTE).
  if ((AFormat <> CF_DIB) and (AFormat <> CF_BITMAP)) or (AData = 0)
  then // Error - unsupported format.
  else begin
       FreeContext;

       if (AFormat = CF_DIB) // Device independent bitmap.
       then begin
            pMem := GlobalLock(AData);
            if (pMem <> Nil)
            then begin
                 // Get palette size.
                 pBmpih := pBitmapInfoHeader(pMem);
                 pBmpch := pBitmapCoreHeader(pMem);

                 // Get number of bit per pixel.
                 FImageFormat := IF_NONE;
                 if (pBmpih^.biSize <> SizeOf(TBitmapCoreHeader))
                 then begin
                      BitCount := pBmpih^.biBitCount * pBmpih^.biPlanes;
                      Width    := pBmpih^.biWidth;
                      Height   := pBmpih^.biHeight;
                      XResolution := pBmpih^.biXPelsPerMeter;
                      YResolution := pBmpih^.biYPelsPerMeter;
                      pPal     := PAnsiChar(pBmpih) + pBmpih^.biSize;
                      HeaderSize := SizeOf(TBitmapInfoHeader);
                      if (pBmpih^.biCompression = BI_BITFIELDS) and
                         ((BitCount = 16) or (BitCount = 32))
                      then begin
                           HeaderSize := HeaderSize + 12;
                      end;
                 end
                 else begin
                      BitCount := pBmpch^.bcBitCount * pBmpch^.bcPlanes;
                      Width    := pBmpch^.bcWidth;
                      Height   := pBmpch^.bcHeight;
                      HeaderSize := SizeOf(TBitmapCoreHeader);
                      pPal     := PAnsiChar(pBmpih) + pBmpch^.bcSize;
                 end;

                 if (FDibBits <> Nil)
                 then begin
                      // Calc. Bitmap header size.
                      NoColors := GetNumColors(FDibInfo);
                      if (NoColors <> 0)
                      then begin
                           SetPalToDibHandle(PRGBQuadArray(pPal)^, NoColors);
                           FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);
                      end;
                      HeaderSize := HeaderSize + NoColors * SizeOf(TRGBQuad);
                      pMem := PAnsiChar(pMem) + HeaderSize;
                      CopyMemory(FDibBits, pMem, FDibInfo^.bmiHeader.biSizeImage);
                 end;

                 // Unlock returned bitmap handle.
                 GlobalUnlock(AData);
            end;
       end;

       if (AFormat = CF_BITMAP)
       then begin
            try
              FillChar(TempDIB, sizeof(TempDIB), 0);
              GetObject(AData, SizeOf(TempDIB), @TempDIB);
              if (TempDIB.dsbm.bmBits = Nil)
              then TempDIB.dsbmih.biSize := 0;
              NewPalette := 0;
              NewHandle := CopyBitmap(AData, NewPalette, NewPalette, TempDIB);
              if (NewPalette <> 0)
              then DeleteObject(NewPalette);
              SetDibHandle(NewHandle);
            finally
              if (AData = 0)
              then begin
                   FError := EC_WINDOWS;
                   FWinError := GetLastError;
              end;
            end;
            (*
            FillChar(TempDIB, sizeof(TempDIB), 0);
            GetObject(AData, SizeOf(TempDIB), @TempDIB);
            if (TempDIB.dsbm.bmBits = Nil)
            then TempDIB.dsbmih.biSize := 0;

            NewPalette := 0;
            NewHandle := CopyBitmap(AData, 0, NewPalette, TempDIB);
            SetDibHandle(NewHandle);
            //DibHandle := 0;
            *)
       end;

       FModified := True;
       Changed(Self);
  end;
end; // TmcmImage.LoadFromClipboardFormat.


procedure TmcmImage.SaveToClipboardFormat(var AFormat : word;
                                          var AData   : THandle);
var HeaderSize : cardinal;
    NoColors   : integer;
    pMem       : pointer;
    Dib        : TDIBSection;
    Pal        : HPALETTE;
begin
 {$IFDEF MCMDEMO}
   Canvas.Brush.Style := bsClear;
   Canvas.Font.Size := 16;
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(255, 255, 255);
   Canvas.TextOut(11, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(11, 9, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 11, PChar(@pBmp^[1]));
   Canvas.TextOut(9, 9, PChar(@pBmp^[1]));
   Canvas.Font.Style := [fsBold];
   Canvas.Font.Color := RGB(0, 0, 0);
   Canvas.TextOut(10, 10, PChar(@pBmp^[1]));
 {$ENDIF}
  if Not((AFormat = CF_DIB) or (AFormat = CF_BITMAP))
  then AFormat := CF_DIB;
  
  if (AFormat = CF_DIB) // Device independent bitmap.
  then begin
       if (FDibBits <> Nil)
       then begin
            // Calc. Bitmap header size.
            NoColors := GetNumColors(FDibInfo);
            HeaderSize := SizeOf(TBitmapInfoHeader) + NoColors * SizeOf(TRGBQuad);
            // Allocate memory
            AData := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, HeaderSize + FDibInfo^.bmiHeader.biSizeImage);
            if (AData <> 0)
            then begin
                 pMem := GlobalLock(AData);
                 if (pMem <> Nil)
                 then begin
                      // Copy header info and palette.
                      CopyMemory(pMem, FDibInfo, HeaderSize);
                      pMem := PAnsiChar(pMem) + HeaderSize;
                      // Copy bitmap data.
                      CopyMemory(pMem, FDibBits, FDibInfo^.bmiHeader.biSizeImage);
                      GlobalUnlock(AData);
                 end;
            end;
       end;
  end;

  if (AFormat = CF_BITMAP) // Device dependent bitmap.
  then begin
       FreeContext;
       try
         Dib := FDIBSection;
         Dib.dsbmih.biSize := 0;   // copy to device bitmap
         Dib.dsbm.bmBits := nil;
         Pal := 0;
         AData := CopyBitmap(FDibHandle, FDibPal, Pal, Dib);
         if (Pal <> 0)
         then DeleteObject(Pal);
       finally
         if (AData = 0)
         then begin
              FError := EC_WINDOWS;
              FWinError := GetLastError;
         end;
       end;
  end;
end; // TmcmImage.SaveToClipboardFormat.


function TmcmImage.GetHeight : longint;
begin
  Result := FDibInfo^.bmiHeader.biHeight;
end; // TmcmImage.GetHeight.


procedure TmcmImage.SetHeight(Value : longint);
begin
  if (FDibInfo^.bmiHeader.biHeight <> Value)
  then begin
       if (Abs(FDibInfo^.bmiHeader.biHeight) = Abs(Value))
       then FDibInfo^.bmiHeader.biHeight := Value
       else begin
            FDibInfo^.bmiHeader.biHeight := Value;
            if (CreateDIBHandle <> 0)
            then Changed(Self);
       end;
  end;
end; // TmcmImage.SetHeight.


function TmcmImage.GetWidth : longint;
begin
  Result := FDibInfo^.bmiHeader.biWidth;
end; // TmcmImage.GetWidth.


procedure TmcmImage.SetWidth(Value : longint);
begin
  if (FDibInfo^.bmiHeader.biWidth <> Value)
  then begin
       FDibInfo^.bmiHeader.biWidth := Value;
       if (CreateDIBHandle <> 0)
       then Changed(Self);
  end;
end; // TmcmImage.SetWidth.


function TmcmImage.GetLongWidth : cardinal;
begin
  Result := FLongWidth;
end; // TmcmImage.GetLongWidth.


function TmcmImage.GetBitCount : word;
begin
  Result := FBitCount; // is FDibInfo^.bmiHeader.biBitCount;
end; // TmcmImage.GetBitCount.


procedure TmcmImage.SetBitCount(Value : word);
begin
  if (FDibInfo^.bmiHeader.biBitCount <> Value)
  then begin
       FImageFormat := IF_NONE;
       FDibInfo^.bmiHeader.biBitCount := Value;
       if (CreateDIBHandle <> 0)
       then Changed(Self)
       else FBitCount := FDibInfo^.bmiHeader.biBitCount;
  end;
end; // TmcmImage.SetBitCount.


function TmcmImage.GetDispHeight : integer;
begin
  if FScaleHVRes and (FImageInfo <> Nil) and
     (FImageInfo.XResolution >  0.0) and (FImageInfo.YResolution >  0.0)
  then Result := Round(GetHeight * FImageInfo.XResolution / FImageInfo.YResolution)
  else Result := GetHeight;
end; // TmcmImage.GetDispHeight.


function TmcmImage.GetDispWidth : integer;
begin
  Result := GetWidth;
end; // TmcmImage.GetDispWidth.


function TmcmImage.GetImageInfo : TmcmImageInfo;
begin
 if Not(Assigned(FImageInfo))
 then begin
      FImageInfo := TmcmImageInfo.Create;
      FImageInfo.Units := UN_METERS;
      FImageInfo.XResolution := XResolution;
      FImageInfo.YResolution := YResolution;
 end;
 Result := FImageInfo;
end; // TmcmImage.GetImageInfo.                                             


function TmcmImage.GetImageFormat : TmcmImageFormat;
var i, NoColors : integer;
begin
  if (FImageFormat = IF_NONE)
  then begin
       case FDibInfo^.bmiHeader.biBitCount of
       1  : FImageFormat := IF_BW;
       4,
       8  : begin
              // Check palette to determin format.
              if (FDibInfo^.bmiHeader.biBitCount = 4)
              then FImageFormat := IF_GREY4
              else FImageFormat := IF_GREY8;

              NoColors := 1 shl FDibInfo^.bmiHeader.biBitCount;
              with FDibInfo^
              do begin
                 i := 0;
                 while (i < NoColors)// and ((FImageFormat = IF_GREY4) or (FImageFormat = IF_GREY8))
                 do begin
                    if (bmiColors[i].rgbRed  <> bmiColors[i].rgbGreen) or
                       (bmiColors[i].rgbBlue <> bmiColors[i].rgbGreen)
                    then begin
                         if (FDibInfo^.bmiHeader.biBitCount = 4)
                         then begin
                              FImageFormat := IF_PAL4;
                              Break;
                         end
                         else begin
                              FImageFormat := IF_PAL8;
                              Break;
                         end;
                    end;
                    inc(i);
                 end;
              end;
            end;
       15 : FImageFormat := IF_RGB15;
       16 : FImageFormat := IF_RGB16;
       24 : FImageFormat := IF_RGB24;
       32 : FImageFormat := IF_RGBA32;
       end;
  end;
  Result := FImageFormat;
end; // TmcmImage.GetImageFormat.


procedure TmcmImage.SetImageFormat(Value : TmcmImageFormat);
begin
  if (FImageFormat <> Value)
  then begin
       // SetBitCount will set FImageFormat to IF_NONE.
       case Value of
       IF_BW     : SetBitCount(1);
       IF_GREY4  : SetBitCount(4);
       IF_GREY8  : SetBitCount(8);
       IF_GREY16 : SetBitCount(16);
       IF_PAL4   : SetBitCount(4);
       IF_PAL8   : SetBitCount(8);
       IF_RGB15  : SetBitCount(16);
       IF_RGB16  : SetBitCount(16);
       IF_RGB24  : SetBitCount(24);
       IF_RGBA32 : SetBitCount(32);
       end;
       FImageFormat := Value;
  end;
end; // TmcmImage.SetImageFormat.


function TmcmImage.GetPalette : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF};
begin
  Result := 0;
  if True // (FDibPal = 0)
  then begin
       if (FDibPal <> 0)
       then DeleteObject(FDibPal);
       FDibPal := 0;
       if (0 < FDibInfo^.bmiHeader.biBitCount) and (FDibInfo^.bmiHeader.biBitCount <= 8)
       then begin
            FreeContext;
            FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);
            Result := FDibPal;
       end;
  end
  else Result := FDibPal;
end; // TmcmImage.GetPalette.                                               


procedure TmcmImage.SetPalette(Value : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF});
var i, NoColors : integer;
    bTemp       : byte;
begin
  if (Value <> 0)
  then begin
       FreeContext;

       // Cleare canvas.
       if Assigned(FCanvas)
       then FCanvas.Free;
       FCanvas := Nil;

       // Delete existing palette.
       if (FDibPal <> 0)
       then DeleteObject(FDibPal);
       FDibPal := Value;

       // Get Palette entries from FDibPal.
       NoColors := GetColorTableFromPal(FDibPal, PRGBQuadArray(@FDibInfo^.bmiColors)^);

       // A windows TPaletteEntry structure was returned, where R & B are
       // swap compared to TRGBQuad.
       for i := 0 to (NoColors - 1)
       do begin
          bTemp := FDibInfo^.bmiColors[i].rgbBlue;
          FDibInfo^.bmiColors[i].rgbBlue := FDibInfo^.bmiColors[i].rgbRed;
          FDibInfo^.bmiColors[i].rgbRed := bTemp;
       end;

       // Set palette to Dib
       SetPalToDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^, NoColors);
       FModified := True;
  end;
  Changed(Self);
end; // TmcmImage.SetPalette.


function TmcmImage.GetPaletteEntries(Palette : PLogPalette) : integer;
var i, NoColors : integer;
begin
  Result := 0;
  if (FDibPal <> 0)
  then begin
       if (FDIBSection.dsBmih.biBitCount <= 8)
       then NoColors := 1 shl FDIBSection.dsBmih.biBitCount
       else NoColors := FDIBSection.dsBmih.biClrUsed;
       if (0 < NoColors) and (NoColors <= 256)
       then begin
            if Assigned(Palette)
            then begin
                 Palette^.palVersion := $300;
                 Palette^.palNumEntries := NoColors;
                 for i := 0 to (NoColors - 1)
                 do begin
                    Palette^.palPalEntry[i].peRed   := FDibInfo^.bmiColors[i].rgbRed;
                    Palette^.palPalEntry[i].peGreen := FDibInfo^.bmiColors[i].rgbGreen;
                    Palette^.palPalEntry[i].peBlue  := FDibInfo^.bmiColors[i].rgbBlue;
                 end;
            end;
            Result := NoColors;
       end;
  end;
end; // TmcmImage.GetPaletteEntries.


function TmcmImage.GetPaletteEntry(Index : integer; PalEntry : PPaletteEntry) : integer;
var NoColors : integer;
begin
  Result := 0;
  if (FDibPal <> 0)
  then begin
       if (FDIBSection.dsBmih.biBitCount <= 8)
       then NoColors := 1 shl FDIBSection.dsBmih.biBitCount
       else NoColors := FDIBSection.dsBmih.biClrUsed;
       if (0 <= Index) and (Index < NoColors)
       then begin
            if Assigned(PalEntry)
            then begin
                 PalEntry^.peRed   := FDibInfo^.bmiColors[Index].rgbRed;
                 PalEntry^.peGreen := FDibInfo^.bmiColors[Index].rgbGreen;
                 PalEntry^.peBlue  := FDibInfo^.bmiColors[Index].rgbBlue;
                 PalEntry^.peFlags := FDibInfo^.bmiColors[Index].rgbReserved;
                 //CopyMemory(PalEntry, @FDibInfo^.bmiColors[Index], SizeOf(TPaletteEntry));
            end;
            Result := NoColors;
       end;
  end;
end; // TmcmImage.GetPaletteEntry.


procedure TmcmImage.SetPaletteEntries(Palette : PLogPalette);
var i : integer;
begin
  if Assigned(Palette)
  then begin
       FreeContext;

       // Set palette to Dib
       Palette^.palVersion := $300;
       for i := 0 to (Palette^.palNumEntries - 1)
       do begin
          FDibInfo^.bmiColors[i].rgbRed   := Palette^.palPalEntry[i].peRed;
          FDibInfo^.bmiColors[i].rgbGreen := Palette^.palPalEntry[i].peGreen;
          FDibInfo^.bmiColors[i].rgbBlue  := Palette^.palPalEntry[i].peBlue;
       end;
       SetPalToDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^, Palette^.palNumEntries);

       // Delete old palette and obtain the new palette.
       if (FDibPal <> 0)
       then DeleteObject(FDibPal);
       FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);

       FModified := True;
       Changed(Self);
  end;
end; // TmcmImage.SetPaletteEntries.                                        


procedure TmcmImage.SetPaletteEntry(Index : integer; PalEntry : PPaletteEntry);
var NoColors : integer;
begin
  if Assigned(PalEntry)
  then begin
       FreeContext;
       NoColors := GetNumColors(FDibInfo);
       if (0 <= Index) and (Index < NoColors)
       then begin
            with FDibInfo^.bmiColors[Index]
            do begin
               rgbRed      := PalEntry^.peRed;
               rgbGreen    := PalEntry^.peGreen;
               rgbBlue     := PalEntry^.peBlue;
               rgbReserved := 0;
            end;

            // Set palette to Dib
            SetPalToDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^, NoColors);

            // Delete old palette and obtain the new palette.
            if (FDibPal <> 0)
            then DeleteObject(FDibPal);
            FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);

            FModified := True;
            Changed(Self);
       end;
  end;
end; // TmcmImage.SetPaletteEntry.


function TmcmImage.CreateGreyPalette : {$IFDEF BCB} TmcmHPALETTE {$ELSE} HPALETTE {$ENDIF};
var i, iColor : integer;
    Step      : integer;
    NoColors  : integer;
begin
  FreeContext; // mcm - possibly a performance problem!
  NoColors := GetNumColors(FDibInfo);
  if (NoColors > 0)
  then begin
       // Delete existing palette.
       if (FDibPal <> 0)
       then DeleteObject(FDibPal);
       FDibPal := 0;

       case NoColors of
       2  : Step := 255;
       16 : Step := 17;
       else Step := 1;
       end;

       for i := 0 to (NoColors - 1)
       do begin
          iColor := i * Step;
          FDibInfo^.bmiColors[i].rgbRed      := iColor;
          FDibInfo^.bmiColors[i].rgbGreen    := iColor;
          FDibInfo^.bmiColors[i].rgbBlue     := iColor;
          FDibInfo^.bmiColors[i].rgbReserved := 0;
       end;

       CopyMemory(@FDibLogPal.palPalEntry, @FDibInfo^.bmiColors[0], NoColors * SizeOf(TRGBQuad));
       FDibLogPal.palVersion := $300;
       FDibLogPal.palNumEntries := NoColors;

       // Set palette to Dib
       SetPalToDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^, NoColors);

       // Delete old palette and obtain the new palette.
       if (FDibPal <> 0)
       then DeleteObject(FDibPal);
       FDibPal := GetPalFromDibHandle(PRGBQuadArray(@FDibInfo^.bmiColors)^);

       FModified := True;
       Changed(Self);
       Result := FDibPal;
  end
  else Result := 0;
end; // TmcmImage.CreateGreyPalette.


function TmcmImage.GetNearestPaletteIndex(rgbColor : TColor) : integer;
var i, Index   : integer;
    NoColors   : integer;
    ColorDif   : integer;
    ColorError : integer;
begin
  Result := -1;
  if (FDibInfo^.bmiHeader.biBitCount in [1,4,8])
  then begin
       NoColors := 1 shl FDibInfo^.bmiHeader.biBitCount;
       Index := -1;
       ColorError := 3 * 256 * 256;
       for i := 0 to (NoColors - 1)
       do begin
          ColorDif := sqr((rgbColor and $FF) - (FDibInfo^.bmiColors[i].rgbRed)) +
                      sqr(((rgbColor shr 8) and $FF) - (FDibInfo^.bmiColors[i].rgbGreen)) +
                      sqr((rgbColor shr 16) - (FDibInfo^.bmiColors[i].rgbBlue));
          if (ColorError > ColorDif)
          then begin
               ColorError := ColorDif;
               Index := i;
          end;
       end;
       Result := Index;
  end;
end; // TmcmImage.GetNearestPaletteIndex.


function TmcmImage.GetScanLine(Row : longint) : pointer;
begin
  try
  //  FreeContext; // May and then again may not cause a problem -
                   // but omitting call to FreeContext saves up to 50MS on
                   // image processing!
    with FDibInfo^.bmiHeader
    do begin
       if (Row < 0)
       then Row := 0
       else if (Row >= Abs(biHeight))
            then Row := Abs(biHeight) - 1;
       Row := Abs(biHeight) - Row - 1;
       cardinal(Result) := cardinal(FDibBits) +
                           cardinal(Row) * cardinal(FLongWidth);
    end;
  except
    Result := Nil;
  end;
end; // TmcmImage.GetScanLine.


function TmcmImage.GetPixel(x, y : integer) : TColor;
begin
  case FImageFormat of
  // IF_RGB24  : Result := TColor(SwapRBValues(PVectorRGB(ScanLine[y])^[x]));
  IF_RGBA32 : Result := TColor(SwapRBValues(PVectorRGBA(ScanLine[y])^[x]));
  else Result := {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.GetPixel(Canvas.Handle, X, Y);
  end;
end; // TmcmImage.GetPixel.


procedure TmcmImage.SetPixel(x, y : integer; Value : TColor);
var i      : integer;
    pLine  : PVectorB;
begin
  case FImageFormat of
  IF_RGB24  : begin
                pLine := ScanLine[y];
                i := 3 * x;
                pLine^[i] := (Value shr 16 and $FF); // Red
                inc(i);
                pLine^[i] := (Value shr 8 and $FF);  // Green
                inc(i);
                pLine^[i] := (Value and $FF);        // Blue
              end;
  IF_RGBA32 : begin
                pLine := ScanLine[y];
                i := 4 * x;
                pLine^[i] := (Value shr 16) and $FF; // Red
                inc(i);
                pLine^[i] := (Value shr 8) and $FF;  // Green
                inc(i);
                pLine^[i] := (Value and $FF);        // Blue
                inc(i);
                pLine^[i] := (Value shr 24) and $FF; // Alpha
              end;
  else {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetPixel(Canvas.Handle, X, Y, ColorToRGB(Value));
  end;
  Changed(Self);
end; // TmcmImage.SetPixel.


function TmcmImage.GetUniqueColors : cardinal;
// Count number of unique R-G-B triples or Palette/Grey values in a bitmap.
//
// Use 2D array of TBits objects -- when (R,G) combination occurs
// for the first time, create 256-bit array of bits in blue dimension.
// So, overall this is a fairly sparse matrix for most pictures.
// Tested with pictures created with a known number of colors, including
// a specially constructed image with 1024*1024 = 1,048,576 colors.
var i, j, k   : integer;
    Flags     : array[0..255,0..255] of TBits;
    PalFlags  : array[0..255] of boolean;
    Start     : TVecByteRec;
    ToAddr    : TVecByteRec;
    ToAddrW   : TVecWordRec;
    Bits      : TVecByteRec;
    Count     : integer;
    ColorB    : byte;
    RVal      : byte;
    GVal      : byte;
    BVal      : byte;
begin
  Count := 0;

  FLongWidth  := LongLineWidth;
  Bits.Ptr   := FDibBits;
  Start.Long := 0;
  case FBitCount of
  1  : begin
         FillMemory(@PalFlags, SizeOf(PalFlags), 0);
         for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
         do begin
            ToAddr.Long := Bits.Long + Start.Long;
            for i := 0 to (FLongWidth - 1)
            do for k := 0 to 7
               do begin
                  ColorB := (ToAddr.Ptr^[i] shr k) and $01;
                  if Not PalFlags[ColorB]
                  then PalFlags[ColorB] := True;
               end;
            Start.Long := Start.Long + FLongWidth;
         end;

         for i := 0 to 1
         do if PalFlags[i]
            then inc(Count);
       end;
  4  : begin
         FillMemory(@PalFlags, SizeOf(PalFlags), 0);
         for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
         do begin
            ToAddr.Long := Bits.Long + Start.Long;
            for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
            do begin
               k := i shr 1;
               if ((i and 1) = 0)
               then ColorB := (ToAddr.Ptr^[k] shr 4) and $0F
               else ColorB := ToAddr.Ptr^[k] and $0F;
               if Not PalFlags[ColorB]
               then PalFlags[ColorB] := True;
            end;
            Start.Long := Start.Long + FLongWidth;
         end;

         for i := 0 to 15
         do if PalFlags[i]
            then inc(Count);
       end;
  8  : begin
         FillMemory(@PalFlags, SizeOf(PalFlags), 0);
         for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
         do begin
            ToAddr.Long := Bits.Long + Start.Long;
            for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
            do if Not PalFlags[ToAddr.Ptr^[i]]
               then PalFlags[ToAddr.Ptr^[i]] := True;
            Start.Long := Start.Long + FLongWidth;
         end;

         for i := 0 to 255
         do if PalFlags[i]
            then inc(Count);
       end;
  15,
  16,
  24,
  32 : begin
         // Clear 2D array of TBits objects
         FillMemory(@Flags, SizeOf(Flags), 0);

         case FBitCount of
         15,
         16 : begin
                for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                do begin
                   ToAddrW.Long := Bits.Long + Start.Long;
                   for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                   do begin
                      RVal := (ToAddrW.Ptr^[i] and $001F) shl 3;
                      GVal := (ToAddrW.Ptr^[i] and $03E0) shr 2;
                      BVal := (ToAddrW.Ptr^[i] and $7C00) shr 7;
                      if Not Assigned(Flags[RVal,GVal])
                      then begin
                           // Create 3D column when needed
                           Flags[RVal,GVal] := TBits.Create;
                           Flags[RVal,GVal].Size := 256;
                      end;

                      // Mark this R-G-B triple
                      Flags[RVal,GVal].Bits[BVal] := True;
                   end;
                   Start.Long := Start.Long + FLongWidth;
                end;
              end;
         24 : begin
                for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                do begin
                   ToAddr.Long := Bits.Long + Start.Long;
                   for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                   do begin
                      k := 3 * i;
                      if Not Assigned(Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}])
                      then begin
                           // Create 3D column when needed
                           Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}] := TBits.Create;
                           Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}].Size := 256;
                      end;

                      // Mark this R-G-B triple
                      Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}].Bits[ToAddr.Ptr^[k]{Blue}] := True;
                   end;
                   Start.Long := Start.Long + FLongWidth;
                end;
              end;
         32 : begin
                for j := 0 to (FDibInfo^.bmiHeader.biHeight - 1)
                do begin
                   ToAddr.Long := Bits.Long + Start.Long;
                   for i := 0 to (FDibInfo^.bmiHeader.biWidth - 1)
                   do begin
                      k := 4 * i;
                      if Not Assigned(Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}])
                      then begin
                           // Create 3D column when needed
                           Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}] := TBits.Create;
                           Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}].Size := 256;
                      end;

                      // Mark this R-G-B triple
                      Flags[ToAddr.Ptr^[k+2]{Red},ToAddr.Ptr^[k+1]{Green}].Bits[ToAddr.Ptr^[k]{Blue}] := True;
                   end;
                   Start.Long := Start.Long + FLongWidth;
                end;
              end;
         end;

         // Count and Free TBits objects
         for j := 0 to 255
         do begin
            for i := 0 to 255
            do begin
               if Assigned(Flags[i,j])
               then begin
                    for k := 0 to 255
                    do if Flags[i,j].Bits[k]
                       then inc(Count);
                    Flags[i,j].Free;
               end;
            end;
         end;
       end;
  end;
  Result := Count;
end; // TmcmImage.GetUniqueColors.                                          


function TmcmImage.GetXResolution : integer;
var SaveUnit : TmcmUnit;
begin
  if (FDibInfo^.bmiHeader.biXPelsPerMeter = 0)
  then begin
       if Assigned(FImageInfo)
       then with FDibInfo^.bmiHeader
            do begin
               SaveUnit := FImageInfo.Units;
               FImageInfo.Units := UN_METERS;
               biXPelsPerMeter := Round(FImageInfo.xResolution);
               FImageInfo.Units := SaveUnit;
            end;
  end;
  Result := FDibInfo^.bmiHeader.biXPelsPerMeter;
end; // TmcmImage.GetXResolution.


procedure TmcmImage.SetXResolution(Value : integer);
begin
  FDibInfo^.bmiHeader.biXPelsPerMeter := Value;
end; // TmcmImage.SetXResolution.


function TmcmImage.GetYResolution : integer;
var SaveUnit : TmcmUnit;
begin
  if (FDibInfo^.bmiHeader.biYPelsPerMeter = 0)
  then begin
       if Assigned(FImageInfo)
       then with FDibInfo^.bmiHeader
            do begin
               SaveUnit := FImageInfo.Units;
               FImageInfo.Units := UN_METERS;
               biYPelsPerMeter := Round(FImageInfo.yResolution);
               FImageInfo.Units := SaveUnit;
            end;
  end;
  Result := FDibInfo^.bmiHeader.biYPelsPerMeter;
end; // TmcmImage.GetYResolution.


procedure TmcmImage.SetYResolution(Value : integer);
begin
  FDibInfo^.bmiHeader.biYPelsPerMeter := Value;
end; // TmcmImage.SetYResolution.


function TmcmImage.GetOrigo : TPoint;
begin
  Result := Point(FxOrg, FyOrg);
end; // TmcmImage.GetOrigo.


procedure TmcmImage.SetOrigo(xy : TPoint);
begin
  FxOrg := xy.x;
  FyOrg := xy.y;
end; // TmcmImage.SetOrigo.


function TmcmImage.MaxIntScale : integer;
var MaxScaleH : integer;
  {$IFDEF DCB3_5}
    MaxScaleV : integer;
  {$ENDIF}
begin
  {$IFDEF DCB3}
    // Delphi 3
    // Limit is Delphi's Scrollbar which cannot be dragged above 32767.
    MaxScaleH := Trunc(32767 / FDibInfo^.bmiHeader.biWidth);
    MaxScaleV := Trunc(32767 / FDibInfo^.bmiHeader.biHeight);
    if (MaxScaleH > MaxScaleV)
    then MaxScaleH := MaxScaleV;
  {$ELSE}
    {$IFDEF DCB3_5}
      // Delphi 4 & 5
      // Limit is coordinates returned by OnMouseMove. Using the correction in
      // TmcmImageCtrl's OnMouseMove this limit is 65535.
      MaxScaleH := Trunc(65536 / FDibInfo^.bmiHeader.biWidth);
      MaxScaleV := Trunc(65536 / FDibInfo^.bmiHeader.biHeight);
      if (MaxScaleH > MaxScaleV)
      then MaxScaleH := MaxScaleV;
    {$ELSE}
      // Delphi 6, 7 & latter
      MaxScaleH := 2048;
    {$ENDIF}
  {$ENDIF}
  if (Win32MajorVersion = 4)
  then begin
       // Using Windows 95, 98, Me and NT 4.0 Scale cannot exceed 31.
       if (MaxScaleH > 31)
       then MaxScaleH := 31;
  end;
  Result := MaxScaleH;
end; // TmcmImage.MaxIntScale.


procedure TmcmImage.SetTransparent(Value : boolean);
begin
  if (FTransparent <> Value)
  then begin
       FTransparent := Value;
       if (FMaskHandle <> 0)
       then begin
            DeleteObject(FMaskHandle);
            FMaskHandle := 0;
       end;
  end;
end; // TmcmImage.SetTransparent.


procedure TmcmImage.SetTransparentColor(Value : TColorRef);
begin
  if (FTransColor <> Value)
  then begin
       FTransColor := Value;
       if (FMaskHandle <> 0)
       then begin
            DeleteObject(FMaskHandle);
            FMaskHandle := 0;
       end;
  end;
end; // TmcmImage.SetTransparentColor.


procedure TmcmImage.SetFlipY(Value : boolean);
begin
  FFlipY := Value;
end; // TmcmImage.SetFlipY.


function TmcmImage.GetStretchMode : integer;
begin
  Result := FSBltMode;
end; // TmcmImage.GetStretchMode.


procedure TmcmImage.SetStretchMode(Value : integer);
begin
  FSBltMode := Value;
end; // TmcmImage.SetStretchMode.


procedure TmcmImage.SetScaleHVRes(Value : boolean);
begin
  if (FScaleHVRes <> Value)
  then FScaleHVRes := Value;
end; // TmcmImage.SetScaleHVRes.


procedure TmcmImage.GetWindowsErrorCode(ErrStr : string);
    {$IFDEF IMGDEBUG}
var lpMsgBuf       : pchar; // Error message variables.
    {$ENDIF}
begin
  FError := EC_WINDOWS;
  FWinError := GetLastError;
  {$IFDEF IMGDEBUG}
  lpMsgBuf := Nil;
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
                Nil,
                FWinError,
                0, //MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                lpMsgBuf,
                0,
                Nil);
  MessageBox(0, lpMsgBuf, PChar(ErrStr), MB_OK or MB_ICONINFORMATION);
  // Free the buffer.
  LocalFree(integer(lpMsgBuf));
  {$ENDIF}
end; // TmcmImage.GetWindowsErrorCode.


procedure TmcmImage.GetWindowsGDIErrorCode(GDIResult : integer);
begin
  if (GDIResult = integer(GDI_ERROR))
  then // We have a GDI problem - Windows error
       GetWindowsErrorCode('GDI Error')
  else begin
       // Zero lines copied - bitmap is too large.
       FError := EC_DISPLAYIMAGE;
       FWinError := 0;
       {$IFDEF IMGDEBUG}
       MessageBox(0, 'No bitmap line were copied.', 'GDI Error', MB_OK or MB_ICONINFORMATION);
       {$ENDIF}
  end;
end; // TmcmImage.GetWindowsGDIErrorCode.


function TmcmImage.AlphaBlend(DC : {$IFDEF BCB} TmcmHDC {$ELSE} HDC {$ENDIF}; DCScale : double) : integer;
{$IFDEF DCB3_5}
const AC_SRC_ALPHA = 1;
{$ENDIF}
var OldbiHeight    : longint;  // Bitmaps original height saved here during painting.

    // Transparent variables.
    MaskDC         : HDC;      // Device handle used by masks.
    SaveObj        : THandle;  // Object selected out of the device context.

    {$IFNDEF DCB3_4}
    BlendFunction  : TBlendFunction;
    {$ENDIF}

    // GDI Error variable.
    GDIResult      : integer; // Result of GDI method.
begin
  GDIResult := 0;
  if (GetImageFormat = IF_RGBA32) {$IFDEF DCB3_4} and False {$ENDIF}
  then begin
       {$IFNDEF DCB3_4}
         BlendFunction.BlendOp := AC_SRC_OVER;
         BlendFunction.BlendFlags := 0;
         BlendFunction.SourceConstantAlpha := 255;
         BlendFunction.AlphaFormat := AC_SRC_ALPHA;

         {$IFDEF GE_DXE2}WinApi.{$ENDIF}windows.AlphaBlend(DC,
                            Round(FxOrg),
                            Round(FyOrg),
                            Round(DCScale * FDibInfo^.bmiHeader.biWidth),
                            Round(DCScale * FDibInfo^.bmiHeader.biHeight),
                            Canvas.Handle,
                            0,
                            0,
                            FDibInfo^.bmiHeader.biWidth,
                            FDibInfo^.bmiHeader.biHeight,
                            BlendFunction);
       {$ENDIF}
  end
  else begin
       if (FCanvas <> Nil)
       then begin
            FCanvas.Free;
            FCanvas := Nil;
       end;

       SaveObj := 0;
       MaskDC  := 0;
       if (FMaskHandle = 0)
       then FMaskHandle := CopyBitmapAsMask(FDibHandle, FDibInfo, FDibPal, FTransColor);

       if (FMaskHandle <> 0)
       then begin
            try
              MaskDC := CreateCompatibleDC(0);
              SaveObj := SelectObject(MaskDC, FMaskHandle);
              OldbiHeight := FDibInfo^.bmiHeader.biHeight;
              if FFlipY
              then FDibInfo^.bmiHeader.biHeight := -FDibInfo^.bmiHeader.biHeight;
              TransparentStretchBlt(DC,
                                    Round(FxOrg),
                                    Round(FyOrg),
                                    Round(DCScale * FDibInfo^.bmiHeader.biWidth),
                                    Round(DCScale * FDibInfo^.bmiHeader.biHeight),
                                    Canvas.Handle, 0, 0,
                                    FDibInfo^.bmiHeader.biWidth,
                                    FDibInfo^.bmiHeader.biHeight,
                                    MaskDC, 0, 0);
              FDibInfo^.bmiHeader.biHeight := OldbiHeight;
            finally
              if (SaveObj <> 0)
              then SelectObject(MaskDC, SaveObj);
              if (MaskDC <> 0)
              then DeleteDC(MaskDC);
            end;
       end;
  end;
  Result := GDIResult;
end; // TmcmImage.AlphaBlend.


procedure TmcmImage.Draw(DC : {$IFDEF BCB} TmcmHDC {$ELSE} HDC {$ENDIF}; DCScale : double);
{$IFNDEF mcmGDIPlus}
type LongType = record
                case Word of
                0 : (Ptr  : pointer);
                1 : (Long : Longint);
                2 : (Lo   : Word;
                     Hi   : Word);
                end;

var Start          : LongType;
    Bits           : LongType;
    ToAddr         : LongType;
    hOldPal        : THandle;

    // Scaling variables.
    tBandHeight    : longint;
    tBandWidth     : longint;
    dBandHeight    : longint;  // Actual band height
    dBandWidth     : longint;  // Actual band width
    DoBands        : boolean;
    SqrScale       : double;   // DCScale squared.
    xSrc, ySrc     : longint;  // Source x,y
    xScale, yScale : double;
    yDIB           : longint;
    OldbiHeight    : longint;  // Bitmaps original height saved here during painting.
    OldbiSizeImage : longint;  // Bitmaps original size saved here during painting.

    BandFactor     : integer;
    MaxBandSize    : integer;
    nBandWidth     : longint;

    // GDI Error variable.
    GDIResult      : integer; // Result of GDI method.
    {$IFDEF DEBUGIMAGEDISPLAYSIZE}
      DebugStr       : string;
    {$ENDIF}
{$ELSE}
var fGraphics      : GPGRAPHICS; // GDI+ graphics "context"
    GDIPImage      : GpBitmap;
    GDIPState      : GRAPHICSCONTAINER;
    GDIPFormat     : PIXELFORMAT;
{$ENDIF}
begin
  {$IFNDEF mcmGDIPlus}
  // GDI
  if (FDibHandle <> 0)   and
     (FDibBits   <> Nil) and
     (FDibInfo   <> Nil)
  then begin // We have a valid bitmap.
       DoBands := True;  // Do divide bitmap into bands.

       if FScaleHVRes and (FImageInfo <> Nil) and
          (FImageInfo.XResolution > 0.0) and (FImageInfo.YResolution > 0.0)
       then begin
            xScale := DCScale;
            yScale := DCScale * FImageInfo.XResolution / FImageInfo.YResolution;
            SqrScale := xScale * yScale;
       end
       else begin
            xScale := DCScale;
            yScale := DCScale;
            SqrScale := DCScale * DCScale;
       end;

       // DIB StretchDIBits. (Methode 1.)
       SetStretchBltMode(DC, FSBltMode);
       if (FSBltMode = HALFTONE)
       then SetBrushOrgEx(DC, 0, 0, Nil);

       // Select palette into device context.
       hOldPal := 0;
       if (FDibPal <> 0)
       then begin
            hOldPal := SelectPalette(DC, FDibPal, False);
            UnrealizeObject(FDibPal);
            RealizePalette(DC);
       end;

       if FTransparent
       then begin // Transparent image display.
            SetStretchBltMode(DC, COLORONCOLOR);
            AlphaBlend(DC, DCScale);
       end
       else begin // Bitmap is not transparent.
            //SqrScale := DCScale * DCScale;
            with FDibInfo^.bmiHeader
            do begin
               if (Win32MajorVersion = 4)
               then begin // for Windows 95, 98, Me and NT 4.0
                    MaxBandSize := 65520;
                    if (biBitCount < 24)
                    then MaxBandSize := MaxBandSize div 2;
               end
               else begin // for Windows 2000, XP & later editions.
                    MaxBandSize := 2 * 2097152; // 2 MBytes
               end;

               if DoBands
               then begin
                    if (DCScale > 1.0)
                    then begin
                         tBandHeight := biHeight;
                         dBandHeight := MaxBandSize div Round(SqrScale * FLongWidth + 0.49);
                    end
                    else begin
                         tBandHeight := biHeight;
                         dBandHeight := MaxBandSize div FLongWidth;
                    end;

                    // Check dBandHeight for illegal sizes.
                    if (dBandHeight < 1)
                    then dBandHeight := 1;
                    if (dBandHeight > biHeight)
                    then dBandHeight := biHeight;
               end
               else begin
                    tBandHeight := biHeight;
                    dBandHeight := biHeight;
               end;

               if (xScale = 1.0) and (yScale = 1.0) // Display bitmap in ratio 1:1
               then begin
                    ySrc := 0;
                    if FFlipY
                    then yDIB := 0
                    else yDIB := tBandHeight - dBandHeight;

                    if (yDIB < 0)
                    then yDIB := 0;

                    // Setup DIB pointer.
                    Bits.Ptr       := FDibBits;
                    Start.Long     := yDIB * FLongWidth;

                    OldbiHeight    := biHeight;
                    OldbiSizeImage := biSizeImage;

                    while (tBandHeight > 0)
                    do begin
                       if FFlipY
                       then biHeight := -dBandHeight
                       else biHeight := dBandHeight;

                       biSizeImage := dBandHeight * FLongWidth;
                       ToAddr.Long := Bits.Long + Start.Long;

                       // Set bitmap to device.
                       GDIResult := SetDIBitsToDevice(DC,
                                         integer(FxOrg),
                                         integer(FyOrg + ySrc),
                                         word(FDibInfo^.bmiHeader.biWidth),
                                         word(dBandHeight),
                                         0,
                                         0,
                                         0,
                                         word(dBandHeight),
                                         ToAddr.Ptr, FDibInfo^,
                                         DIB_RGB_COLORS);

                       if (GDIResult <> word(dBandHeight))
                       then begin // Error, Lines were not copied.
                            // Restore DIB header information.
                            biHeight    := OldbiHeight;
                            biSizeImage := OldbiSizeImage;
                            GetWindowsGDIErrorCode(GDIResult);
                            Break; // Leave Draw method.
                       end;

                       // Increment source y by band height.
                       inc(ySrc, dBandHeight);

                       // Decrement DIB's y.
                       if FFlipY
                       then begin // Paint image up-side-down.
                            inc(yDIB, dBandHeight);
                            if (yDIB >= OldbiHeight)
                            then begin
                                 yDIB := OldbiHeight - 1;
                                 tBandHeight := -1;
                            end;

                            // Calculate rest lines to display/print.
                            dec(tBandHeight, dBandHeight);
                            if (dBandHeight > tBandHeight)
                            then dBandHeight := tBandHeight;
                       end
                       else begin
                            // Calculate rest lines to display/print.
                            dec(tBandHeight, dBandHeight);
                            if (dBandHeight > tBandHeight)
                            then dBandHeight := tBandHeight;

                            dec(yDIB, dBandHeight);
                            if (yDIB < 0)
                            then yDIB := 0;
                       end;

                       // Calculate new pointer address.
                       Start.Long := yDIB * FLongWidth;
                    end;
                    biHeight    := OldbiHeight;
                    biSizeImage := OldbiSizeImage;
               end
               else begin // Image is scaled up/down
                    ySrc := 0;

                    if FFlipY
                    then begin
                         yDIB := 0;
                    end
                    else begin
                         yDIB := tBandHeight - dBandHeight;
                    end;

                    // Setup DIB pointer.
                    Bits.Ptr       := FDibBits;
                    Start.Long     := yDIB * FLongWidth;
                    OldbiHeight    := biHeight;
                    OldbiSizeImage := biSizeImage;

                    BandFactor := Round((SqrScale * biWidth) / MaxBandSize) + 1;
                    nBandWidth := biWidth div BandFactor;
                    if (nBandWidth <= 0)
                    then nBandWidth := 1;

                    while (tBandHeight > 0)
                    do begin
                       if FFlipY
                       then biHeight := -dBandHeight
                       else biHeight := dBandHeight;
                       biSizeImage := dBandHeight * FLongWidth;
                       ToAddr.Long := Bits.Long + Start.Long;

                       xSrc := 0;
                       tBandWidth := biWidth;
                       dBandWidth := nBandWidth;

                       while (tBandWidth > 0)
                       do begin
                          {$IFDEF DEBUGIMAGEDISPLAYSIZE}
                          DebugStr := 'Dest: ' +
                                      IntToStr(integer(round(FxOrg + xScale * xSrc))) + ',' +
                                      IntToStr(integer(round(FyOrg + yScale * ySrc))) + ',' +
                                      IntToStr(integer(round(xScale * dBandWidth))) + ',' +
                                      IntToStr(integer(round(yScale * dBandHeight + 0.4999)));

                          DebugStr := DebugStr + '   Src: ' +
                                      IntToStr(integer(xSrc)) + ',' +
                                      IntToStr(integer(0)) + ',' +
                                      IntToStr(integer(dBandWidth)) + ',' +
                                      IntToStr(integer(dBandHeight));
                          OutputDebugString(PChar(DebugStr));
                          {$ENDIF}
                          // Set bitmap to device.
                          GDIResult := StretchDIBits(DC,
                                        // Destination coordinates (x, y, width, height)
                                        integer(round(FxOrg + xScale * xSrc)),
                                        integer(round(FyOrg + yScale * ySrc)),
                                        integer(round(xScale * dBandWidth)), // integer(round(DCScale * FDibInfo^.bmiHeader.biWidth)),
                                        integer(round(yScale * dBandHeight + 0.4999)),
                                        // Source coordinates (x, y, width, height)
                                        xSrc,
                                        0,
                                        integer(dBandWidth), //integer(biWidth),
                                        integer(dBandHeight),
                                        //
                                        ToAddr.Ptr, FDibInfo^,
                                        DIB_RGB_COLORS, SRCCOPY);

                          {$IFDEF DEBUGIMAGEDISPLAYSIZE}
                          DebugStr := 'GDIResult: ' + IntToStr(GDIResult);
                          OutputDebugString(PChar(DebugStr));
                          {$ENDIF}
                          // Increment device delta y.
                          inc(xSrc, dBandWidth);

                          dec(tBandWidth, dBandWidth);
                          if (dBandWidth > tBandWidth)
                          then dBandWidth := tBandWidth;

                          if (GDIResult <> word(dBandHeight))
                          then begin // Error, Lines were not copied.
                               // Restore DIB header information.
                               biHeight    := OldbiHeight;
                               biSizeImage := OldbiSizeImage;
                               GetWindowsGDIErrorCode(GDIResult);
                               Break; // Leave Draw method.
                          end;
                       end;

                       // Increment source y by band height.
                       inc(ySrc, dBandHeight);
                       if FFlipY
                       then begin // Paint image up-side-down.
                            inc(yDIB, dBandHeight);
                            if (yDIB >= OldbiHeight)
                            then begin
                                 yDIB := OldbiHeight - 1;
                                 tBandHeight := -1;
                            end;

                            // Calculate remaining lines to display/print.
                            dec(tBandHeight, dBandHeight);
                            if (dBandHeight > tBandHeight)
                            then dBandHeight := tBandHeight;
                       end
                       else begin
                            // Calculate remaining lines to display/print.
                            dec(tBandHeight, dBandHeight);
                            if (dBandHeight > tBandHeight)
                            then dBandHeight := tBandHeight;

                            // Decrement DIB's y.
                            dec(yDIB, dBandHeight);
                            if (yDIB < 0)
                            then yDIB  := 0;
                       end;

                       // Calculate new pointer address.
                       Start.Long := yDIB * FLongWidth;
                    end;

                    // Restore DIB header information.
                    biHeight    := OldbiHeight;
                    biSizeImage := OldbiSizeImage;
               end;
            end;
       end;

       // Un-select bitmap's palette from device context.
       if (FDibPal <> 0)
       then SelectPalette(DC, hOldPal, False);
  end;

  {$ELSE}

  // GDI+
  if (FDibHandle <> 0)   and
     (FDibBits   <> Nil) and
     (FDibInfo   <> Nil)
  then begin // We have a valid bitmap.
       fGraphics := 0;
       GdipCreateFromHDC(DC, fGraphics);
       try
         // Save the state of GDI+ container.
         GdipBeginContainer2(fGraphics, GDIPState);

         GdipSetPixelOffsetMode(fGraphics, PixelOffsetModeHalf);
         if (DCScale < 1.0) and (FSBltMode = HALFTONE)
         then begin
              case FImageFormat of
              IF_BW,
              IF_PAL4,
              IF_GREY4,
              IF_PAL8,
              IF_GREY8 : GdipSetInterpolationMode(fGraphics, InterpolationModeHighQualityBicubic);
              else GdipSetInterpolationMode(fGraphics, InterpolationModeNearestNeighbor);
              end;
              //GdipSetInterpolationMode(fGraphics, InterpolationModeHighQualityBicubic);
         end
         else begin
              case FImageFormat of
              IF_BW,
              IF_PAL4,
              IF_GREY4,
              IF_PAL8,
              IF_GREY8 : GdipSetInterpolationMode(fGraphics, InterpolationModeDefault);
              else GdipSetInterpolationMode(fGraphics, InterpolationModeNearestNeighbor);
              end;
         end;

         // Transparent image display.
         if FTransparent
         then GdipSetCompositingMode(fGraphics, CompositingModeSourceOver)
         else GdipSetCompositingMode(fGraphics, CompositingModeSourceCopy);
         // ColorAdjustTypeBitmap

         case FImageFormat of
         IF_BW     : GDIPFormat := PixelFormat1bppIndexed;
         IF_PAL4,
         IF_GREY4  : GDIPFormat := PixelFormat4bppIndexed;
         IF_PAL8,
         IF_GREY8  : GDIPFormat := PixelFormat8bppIndexed;
         IF_GREY16 : GDIPFormat := PixelFormat16bppGrayScale;
         IF_RGB15  : GDIPFormat := PixelFormat16bppRGB555;
         IF_RGB16  : GDIPFormat := PixelFormat16bppRGB565;
         // PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
         IF_RGB24  : GDIPFormat := PixelFormat24bppRGB;
          //PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
         //IF_RGBA32 : GDIPFormat := PixelFormat32bppARGB;
         IF_RGBA32 : GDIPFormat := PixelFormat32bppPARGB;
         IF_RGB48  : GDIPFormat := PixelFormat48bppRGB;
         //IF_RGB64  : GDIPFormat :=
          //PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha or PixelFormatCanonical or PixelFormatExtended);
          //PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatExtended);

         end;
         {
         GdipCreateBitmapFromScan0(FDibInfo^.bmiHeader.biWidth,
                                   FDibInfo^.bmiHeader.biHeight,
                                   FLongWidth,
                                   GDIPFormat,
                                   FDibBits, GdipImage);
         GdipImageRotateFlip(gdipImage, Rotate180FlipX);
         }
         GdipCreateBitmapFromGdiDib(FDibInfo, FDibBits, gdipImage);
         GdipDrawImageRect(fGraphics, gdipImage, FxOrg, FyOrg,
                           DCScale * FDibInfo^.bmiHeader.biWidth,
                           DCScale * FDibInfo^.bmiHeader.biHeight);
         GdipDisposeImage(gdipImage);

         // Restore the state of GDI+ container.
         GdipEndContainer(fGraphics, GDIPState);
       except
       end;

       if fGraphics <> 0
       then GdipDeleteGraphics(fGraphics);
  end;
  {$ENDIF}
end; // TmcmImage.Draw.


//------------------------------------------------------------------------------
// TmcmImageCanvas.
//------------------------------------------------------------------------------

constructor TmcmImageCanvas.Create(AImage : TmcmImage);
begin
  inherited Create;
  FBitmap     := AImage;
  FOldBitmap  := 0;
  FOldPalette := 0;
end; // TmcmImageCanvas.Create.                                             


destructor TmcmImageCanvas.Destroy;
begin
  FreeContext;
  inherited Destroy;
end; // TmcmImageCanvas.Destroy.                                            


procedure TmcmImageCanvas.FreeContext;
var H : HBITMAP;
begin
  if (Handle <> 0)
  then begin
       Lock;
       try
         if (FOldBitmap <> 0)
         then SelectObject(Handle, FOldBitmap);
         if (FOldPalette <> 0)
         then SelectPalette(Handle, FOldPalette, True);
         H := Handle;
         Handle := 0;
         DeleteDC(H);
       finally
         Unlock;
       end;
  end;
end; // TmcmImageCanvas.FreeContext.


procedure TmcmImageCanvas.CreateHandle;
var H : HBITMAP;
begin
  if (FBitmap <> Nil)
  then begin
       Lock;
       try
         H := CreateCompatibleDC(0);
         if (FBitmap.FDibHandle <> 0)
         then FOldBitmap := SelectObject(H, FBitmap.FDibHandle)
         else FOldBitmap := 0;
         if (FBitmap.FDibPal <> 0)
         then begin
              FOldPalette := SelectPalette(H, FBitmap.FDibPal, True);
              RealizePalette(H);
         end
         else FOldPalette := 0;
         Handle := H;
       finally
         Unlock;
       end;
  end;
end; // TmcmImageCanvas.CreateHandle.


//------------------------------------------------------------------------------
// TmcmImageCtrl.
//------------------------------------------------------------------------------

constructor TmcmImageCtrl.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReflector,csOpaque];

  FDC     := 0;
  FFakeVisible := True;
  FImage  := Nil;
  FAutoSize := False;
  FBorderStyle := BS_NONE;
  FScale  := 1.0;
  FCenter := False;
  FFlat   := False;
  Width   := 64;
  Height  := 64;
  Color   := $1FFFFFFF;
  FDragMode := dmManual;
  FOnPainted   := Nil;
end; // TmcmImageCtrl.Create.


destructor TmcmImageCtrl.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmImageCtrl.Destroy.


procedure TmcmImageCtrl.Clear;
begin
  try
    try
      if Assigned(FImage)
      then FImage.Free;
    except
    end;
  finally
    FImage := Nil;
  end;
end; // TmcmImageCtrl.Clear.


procedure TmcmImageCtrl.SetAutoSize(Value : boolean);
var h , w : integer;
begin
  FAutoSize := Value;
  if (FAutoSize and Assigned(FImage))
  then begin
       h := Round(FScale * FImage.Height);
       w := Round(FScale * FImage.Width);
       if (Height <> h)
       then Height := h;
       if (Width <> w)
       then Width := w;
  end;
end; // TmcmImageCtrl.SetAutoSize.


function TmcmImageCtrl.GetBorderStyle : TmcmBorderStyle;
begin
  Result := FBorderStyle;
end; // TmcmImageCtrl.GetBorderStyle.


procedure TmcmImageCtrl.SetBorderStyle(Value : TmcmBorderStyle);
begin
  if (FBorderStyle <> Value)
  then begin
       FBorderStyle := Value;
       if (FBorderStyle <> BS_NONE) 
       then ControlStyle := ControlStyle + [csFramed]
       else ControlStyle := ControlStyle - [csFramed];
       Invalidate;
  end;
end; // TmcmImageCtrl.SetBorderStyle.


procedure TmcmImageCtrl.Changed(Sender : TObject);
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
  if FAutoSize
  then SetAutoSize(FAutoSize)
  else DrawImage;
end; // TmcmImageCtrl.Changed.


function TmcmImageCtrl.GetCenter : boolean;
begin
  Result := FCenter;
end; // TmcmImageCtrl.GetCenter.


procedure TmcmImageCtrl.SetCenter(Value : boolean);
begin
  if (FCenter <> Value)
  then begin
       FCenter := Value;
       DrawImage;
  end;
  if Not(FCenter)
  then if Assigned(FImage)
       then FImage.SetOrigo(Point(0, 0));
end; // TmcmImageCtrl.SetCenter.


function TmcmImageCtrl.GetColor : TColor;
begin
  Result := Inherited Color;
end; // TmcmImageCtrl.GetColor.


procedure TmcmImageCtrl.SetColor(Value : TColor);
var OldColor : TColor;
begin
  OldColor := Inherited Color;
  Inherited Color := Value;
  if (OldColor = $1FFFFFFF) or (Color = $1FFFFFFF)
  then DrawImage;
end; // TmcmImageCtrl.SetColor.


procedure TmcmImageCtrl.SetFlat(Value : boolean);
begin
  if (FFlat <> Value)
  then begin
       FFlat := Value;
       Invalidate;
  end;
end; // TmcmImageCtrl.SetFlat.


function TmcmImageCtrl.GetImage : TmcmImage;
begin
  if Not(Assigned(FImage))
  then FImage := TmcmImage.Create;
  if Assigned(FImage)
  then FImage.FOnChange := Changed;
  Result := FImage;
end; // TmcmImageCtrl.GetImage.


procedure TmcmImageCtrl.SetImage(Value : TmcmImage);
begin
  // User should free existing TmcmImage before assigning a new TmcmImage,
  // unless the TmcmImage is maintained elsewhere.
  Clear;
  if Assigned(Value)
  then begin
       FImage := Value;
       if Assigned(FImage)
       then begin
            FImage.FOnChange := Changed;
            DrawImage;
       end;
  end;
end; // TmcmImageCtrl.SetImage.


function TmcmImageCtrl.GetScale : double;
var HScale : double;
    VScale : double;
begin
  if Assigned(FImage) and FScaleToFit
  then begin
       HScale := 1.0;
       VScale := 1.0;
       case FBorderStyle of
       BS_SINGLE,
       BS_SUNKEN,
       BS_RAISED,
       BS_BUMP,
       BS_ETCHED : begin
                     if (FImage.Width > 0.0)
                     then HScale := (Width - 4) / FImage.DispWidth;
                     if (FImage.Height > 0.0)
                     then VScale := (Height - 4) / FImage.DispHeight;
                   end;
       //BS_NONE :
       else        begin
                     if (FImage.Width > 0.0)
                     then HScale := Width / FImage.DispWidth;
                     if (FImage.Height > 0.0)
                     then VScale := Height / FImage.DispHeight;
                   end;
       end;
       if (HScale < VScale)
       then FScale := HScale
       else FScale := VScale;
  end
  else if Not(Assigned(FImage))
       then FScale := 1.0;
  Result := FScale;
end; // TmcmImageCtrl.GetScale.


procedure TmcmImageCtrl.SetScale(Value : double);
begin
  if (FScale <> Value)
  then begin
       FScale := Value;
       FScaleToFit := False;
       if FAutoSize
       then SetAutoSize(FAutoSize)
       else DrawImage;
  end;
end; // TmcmImageCtrl.SetScale.


function TmcmImageCtrl.GetScaleToFit : boolean;
begin
  Result := FScaleToFit;
end; // TmcmImageCtrl.GetScaleToFit.


procedure TmcmImageCtrl.SetScaleToFit(Value : boolean);
var HScale : double;
    VScale : double;
    OScale : double;
begin
  FScaleToFit := Value;
  if Assigned(FImage) and FScaleToFit
  then begin
       HScale := 1.0;
       VScale := 1.0;
       OScale := FScale;
       if (FImage.Width > 0.0)
       then HScale := Width / FImage.DispWidth;
       if (FImage.Height > 0.0)
       then VScale := Height / FImage.DispHeight;
       if (HScale < VScale)
       then FScale := HScale
       else FScale := VScale;
       if (FScale <> OScale)
       then DrawImage;
  end
  else if Assigned(FImage)
       then DrawImage;
end; // TmcmImageCtrl.SetScaleToFit.


function TmcmImageCtrl.GetTransparent : boolean;
begin
  if Assigned(FImage)
  then Result := FImage.Transparent
  else Result := False;
end; // TmcmImageCtrl.GetTransparent.


procedure TmcmImageCtrl.SetTransparent(Value : boolean);
begin
  if Assigned(FImage)
  then begin
       if (FImage.Transparent <> Value)
       then begin
            FImage.Transparent := Value;
            DrawImage;
       end;
  end;
end; // TmcmImageCtrl.SetTransparent.


function TmcmImageCtrl.GetTransparentColor : TColorRef;
begin
  if Assigned(FImage)
  then Result := FImage.TransparentColor
  else Result := 0;
end; // TmcmImageCtrl.GetTransparentColor.


procedure TmcmImageCtrl.SetTransparentColor(Value : TColorRef);
begin
  if Assigned(FImage)
  then begin
       if (FImage.TransparentColor <> Value)
       then begin
            FImage.TransparentColor := Value;
            DrawImage;
       end;
  end;
end; // TmcmImageCtrl.SetTransparentColor.


function TmcmImageCtrl.GetVisible : boolean;
begin
  Result := (Inherited Visible) and FFakeVisible;
end; // TmcmImageCtrl.GetVisible.


procedure TmcmImageCtrl.SetVisible(Value : boolean);
begin
  Inherited Visible := Value;
end; // TmcmImageCtrl.SetVisible.


procedure TmcmImageCtrl.DrawImage;
var Rect : TRect;
begin
  if Assigned(Parent)
  then begin
       Rect := BoundsRect;
       if (FBorderStyle <> BS_NONE)
       then begin
            inc(Rect.Left, 2);
            inc(Rect.Top, 2);
            dec(Rect.Right, 2);
            dec(Rect.Bottom, 2);
       end;
       if Parent.Showing
       then InvalidateRect(Parent.Handle, @Rect, False); // <- Only refresh own area.
  end;
end; // TmcmImageCtrl.DrawImage.


procedure TmcmImageCtrl.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  if Assigned(FOnMouseMove)
  then begin
       (*
       // PROBLEM - Does not work with TmcmImageDualView control. Mouse moves
       // are mis-interpretated outside client area as X,Y becomes negative,
       // and are as a result adjusted in the code below!
       {$IFDEF DCB3_5} // Delphi 3, 4 & 5.
         // Handle special case for [X,Y] position exceeding [32767,32767] and swaps to
         // [-32768,-32768] to succeding pixel position.
         // Limitation - Width and Height of window cannot exceed 65536.
         if (X < 0)
         then X := 65537 + X;
         if (Y < 0)
         then Y := 65537 + Y;
       {$ENDIF}
       *)
       if FCenter
       then FOnMouseMove(Self, Shift, X - FImage.FxOrg, Y - FImage.FyOrg)
       else FOnMouseMove(Self, Shift, X, Y);
  end;
end; // TmcmImageCtrl.MouseMove.


procedure TmcmImageCtrl.WMPaint(var Message : TWMPaint);
begin
  try
    FDC := Message.DC;
    Paint;
  finally
    FDC := 0;
  end;
  Message.Result := 0;
end; // TmcmImageCtrl.WMPaint.


procedure TmcmImageCtrl.Paint;
var ColorRGB     : longint;
    NewPen       : HPen;
    OldPen       : HPen;
    NewBrush     : HBrush;
    OldBrush     : HBrush;
    ALogBrush    : TLogBrush;
    x0, y0       : integer;
    x1, y1, bxy  : integer;
    ir, sr       : TRect;
    opt          : TPoint;
    BorderFlag   : word;

    (*
    rc : TRect;
    hdcMem : HDC;
    hbmMem, hbmOld : HBITMAP;
    hbrBkGnd : HBRUSH;
    *)
    WindowHandle : HWND;
begin
  if (FDC <> 0) and Visible
  then begin
       if (FBorderStyle <> BS_NONE)
       then bxy := 2
       else bxy := 0;
       ColorRGB := ColorToRGB(Color);
       if (csDesigning in ComponentState)
       then begin
            NewPen := CreatePen(PS_DASH, 1, 0);
            if (Color <> $1FFFFFFF) // <> clNone
            then begin
                 ALogBrush.lbStyle := BS_SOLID;
                 ALogBrush.lbColor := ColorRGB;
            end
            else begin
                 ALogBrush.lbStyle := BS_HOLLOW;
                 ALogBrush.lbColor := 0;
            end;
            ALogBrush.lbHatch := 0;
            NewBrush := CreateBrushIndirect(ALogBrush);
            OldPen   := SelectObject(FDC, NewPen);
            OldBrush := SelectObject(FDC, NewBrush);
            Rectangle(FDC, 0, 0, Width, Height);
            SelectObject(FDC, OldPen);
            SelectObject(FDC, OldBrush);
            DeleteObject(NewPen);
            DeleteObject(NewBrush);
       end;

       // Draw border.
       if (FBorderStyle <> BS_NONE)
       then begin
            ir.Left := 0;
            ir.Top  := 0;
            ir.Right := Width;
            ir.Bottom := Height;
            BorderFlag := BF_RECT;
            if FFlat
            then BorderFlag := BorderFlag or BF_FLAT;
            case FBorderStyle of
            BS_SINGLE : DrawEdge(FDC, ir, EDGE_SUNKEN, BF_RECT);
            BS_SUNKEN : DrawEdge(FDC, ir, EDGE_SUNKEN, BorderFlag);
            BS_RAISED : DrawEdge(FDC, ir, EDGE_RAISED, BorderFlag);
            BS_BUMP   : DrawEdge(FDC, ir, EDGE_BUMP,   BorderFlag);
            BS_ETCHED : DrawEdge(FDC, ir, EDGE_ETCHED, BorderFlag);
            end;

            // Make sure we have a border that's not flikering!
            // and not curropted by other
            ExcludeClipRect(FDC, ir.Left, ir.Top, ir.Left + bxy, ir.Bottom);
            ExcludeClipRect(FDC, ir.Right - bxy, ir.Top, ir.Right, ir.Bottom);
            ExcludeClipRect(FDC, ir.Left, ir.Top, ir.Right, ir.Top + bxy);
            ExcludeClipRect(FDC, ir.Left, ir.Bottom - bxy, ir.Right, ir.Bottom);
       end;

       if Assigned(FImage)
       then begin
            try
              opt := FImage.GetOrigo;
              if FScaleToFit
              then GetScale;
              if FCenter
              then begin
                   x0 := Round((Width - FScale * FImage.DispWidth) / 2);
                   y0 := Round((Height - FScale * FImage.DispHeight) / 2);
                   //inc(x0, bxy);
                   //inc(y0, bxy);
                   FImage.SetOrigo(Point(x0, y0));
              end
              else begin
                   x0 := opt.x + bxy;
                   y0 := opt.y + bxy;
                   FImage.SetOrigo(Point(x0, y0));
              end;
              x1 := Round(FScale * FImage.DispWidth) + x0;
              y1 := Round(FScale * FImage.DispHeight) + y0;

              if FImage.Transparent
              then begin
                   NewPen := CreatePen(PS_NULL, 0, ColorRGB);
                   ALogBrush.lbStyle := BS_SOLID;
                   ALogBrush.lbColor := ColorRGB;
                   ALogBrush.lbHatch := 0;
                   NewBrush := CreateBrushIndirect(ALogBrush);
                   OldPen   := SelectObject(FDC, NewPen);
                   OldBrush := SelectObject(FDC, NewBrush);

                   Rectangle(FDC, 0, 0, Width, Height);

                   SelectObject(FDC, OldPen);
                   SelectObject(FDC, OldBrush);
                   DeleteObject(NewPen);
                   DeleteObject(NewBrush);
                   (*
                   // Flicker free painting

                   // Get the size of the client rectangle.
                   //    GetClientRect(hWnd, &rc);

                   // Create a compatible DC.
                   hdcMem := CreateCompatibleDC(FDC);

                   // Create a bitmap big enough for our client rectangle.
                   hbmMem := CreateCompatibleBitmap(FDC, Width, Height);

                   // Select the bitmap into the off-screen DC.
                   hbmOld := SelectObject(hdcMem, hbmMem);

                   // Erase the background.
                   hbrBkGnd := CreateSolidBrush(Color);
                   FillRect(hdcMem, rc, hbrBkGnd);
                   DeleteObject(hbrBkGnd);

                   //    SetBkMode(hdcMem, TRANSPARENT);

                   FImage.Draw(hdcMem {FDC}, FScale);

                   //blits canvas on screen. Well, only part of it—the
                   // "dirty rectangle" part.
                   GetClipBox(FDC, ClipRect);
                   BitBlt(FDC, ClipRect.left, ClipRect.top, ClipRect.right, ClipRect.bottom, hdcMem,
                      ClipRect.left, ClipRect.top, SRCCOPY);

                   // Blt the changes to the screen DC.
                   // BitBlt(FDC, 0, 0, Width, Height, hdcMem, 0, 0, SRCCOPY);

                   // Done with off-screen bitmap and DC.
                   SelectObject(hdcMem, hbmOld);
                   DeleteObject(hbmMem);
                   DeleteDC(hdcMem);
                   *)
              end;

              // Paint FImage.
              // Image must be painted before erasing background in transparent
              // mode!
              FImage.Draw(FDC, FScale);

              // Paint background not covered by FImage.
              if (Color <> $1FFFFFFF) // <> clNone
              then begin
                   NewPen := CreatePen(PS_NULL, 0, ColorRGB);
                   ALogBrush.lbStyle := BS_SOLID;
                   ALogBrush.lbColor := ColorRGB;
                   ALogBrush.lbHatch := 0;
                   NewBrush := CreateBrushIndirect(ALogBrush);
                   OldPen   := SelectObject(FDC, NewPen);
                   OldBrush := SelectObject(FDC, NewBrush);

                   if (x0 > bxy)
                   then Rectangle(FDC, 0, 0, x0 + 1, Height - bxy + 1);
                   if (x1 < Width - bxy)
                   then Rectangle(FDC, x1, 0, Width - bxy + 1, Height - bxy + 1);
                   if (y0 > bxy)
                   then Rectangle(FDC, 0, 0, Width - bxy + 1, y0 + 1);
                   if (y1 < Height - bxy)
                   then Rectangle(FDC, 0, y1, Width - bxy + 1, Height - bxy + 1);

                   SelectObject(FDC, OldPen);
                   SelectObject(FDC, OldBrush);
                   DeleteObject(NewPen);
                   DeleteObject(NewBrush);
              end
              else begin
                   // Our control is transparent, the owner must therefore
                   // paint un-covered areas.
                   WindowHandle := WindowFromDC(FDC);
                   if (WindowHandle <> 0)
                   then begin
                        sr := BoundsRect;
                        inc(sr.Left, bxy);
                        inc(sr.Top, bxy);
                        dec(sr.Right, bxy);
                        dec(sr.Bottom, bxy);

                        FFakeVisible := False;
                        try
                          if (x0 > bxy)
                          then begin
                               ir := sr;
                               ir.Right := x0 + Left;
                               InvalidateRect(WindowHandle, @ir, True);
                          end;
                          if (x1 < Width - bxy)
                          then begin
                               ir := sr;
                               ir.Left := x1 + Left;
                               InvalidateRect(WindowHandle, @ir, True);
                          end;
                          if (y0 > bxy)
                          then begin
                               ir := sr;
                               ir.Bottom := y0 + Top;
                               InvalidateRect(WindowHandle, @ir, True);
                          end;
                          if (y1 < Height - bxy)
                          then begin
                               ir := sr;
                               ir.Top := y1 + Top;
                               InvalidateRect(WindowHandle, @ir, True);
                          end;
                          Update;

                        finally
                          FFakeVisible := True;
                        end;

                   end;
              end;
            finally
              FImage.SetOrigo(opt);
            end;
       end
       else begin
            // No image - fill background
            if (Color <> $1FFFFFFF) // <> clNone
            then begin
                 NewPen := CreatePen(PS_NULL, 0, ColorRGB);
                 ALogBrush.lbStyle := BS_SOLID;
                 ALogBrush.lbColor := ColorRGB;
                 ALogBrush.lbHatch := 0;
                 NewBrush := CreateBrushIndirect(ALogBrush);
                 OldPen   := SelectObject(FDC, NewPen);
                 OldBrush := SelectObject(FDC, NewBrush);
                 Rectangle(FDC, bxy, bxy, Width - bxy + 1, Height - bxy + 1);
                 SelectObject(FDC, OldPen);
                 SelectObject(FDC, OldBrush);
                 DeleteObject(NewPen);
                 DeleteObject(NewBrush);
            end;
       end;
  end;
  if Assigned(FOnPainted)
  then FOnPainted(Self);
end; // TmcmImageCtrl.Paint.


procedure TmcmImageCtrl.SetDragMode(Value : TDragMode);
begin
  // Don't let TControl start a Drag operation, ref. WndProc below.
  FDragMode := Value;
  Inherited SetDragMode(dmManual);
end; // TmcmImageCtrl.SetDragMode.


procedure TmcmImageCtrl.WndProc(var Message : TMessage);
begin
  Inherited WndProc(Message);

  // Get the coordinate on left mouse button down.
  if (Message.Msg = WM_LBUTTONDOWN)
  then FDragPos := Point(LOWORD(Message.lParam), HIWORD(Message.lParam));

  // Check if we should start the drag operation.
  if (Message.Msg = WM_MOUSEMOVE)
  then if (FDragMode = dmAutomatic)
       then if (csLButtonDown in ControlState) // Left Mouse button is down!
            then if (Sqr(FDragPos.x - LOWORD(Message.lParam)) +
                     Sqr(FDragPos.y - HIWORD(Message.lParam)) >
                     {$IFDEF DCB3} 9 {$ELSE} Sqr(Mouse.DragThreshold) {$ENDIF})
                 then begin
                      ControlState :=  ControlState - [csLButtonDown];
                      BeginDrag(True);
                 end;
end; // TmcmImageCtrl.WndProc.


procedure TmcmImageCtrl.WMMouseWheel(var Message : TWMMouseWheel);
var Handled : boolean;
    PT      : TPoint;
    Shift   : TShiftState;
begin
  if Assigned(FOnMouseWheel)
  then begin
       Shift := [];
       if ((Message.Keys and MK_SHIFT) = MK_SHIFT)
       then Shift := [ssShift];
       if ((Message.Keys and MK_CONTROL) = MK_CONTROL)
       then Shift := Shift + [ssCtrl];
       if ((Message.Keys and MK_LBUTTON) = MK_LBUTTON)
       then Shift := Shift + [ssLeft];
       if ((Message.Keys and MK_MBUTTON) = MK_MBUTTON)
       then Shift := Shift + [ssMiddle];
       if ((Message.Keys and MK_RBUTTON) = MK_RBUTTON)
       then Shift := Shift + [ssRight];

       PT.x  := Message.pos.x;
       PT.y  := Message.pos.y;
       pt := Self.ScreenToClient(pt);
       FOnMouseWheel(Self, Shift, Message.WheelDelta, pt, Handled);
  end;
  Message.Result := 0;
end; // TmcmImageCtrl.WMMouseWheel.


//------------------------------------------------------------------------------
// TmcmMultiImageCtrl.
//------------------------------------------------------------------------------

constructor TmcmMultiImageCtrl.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FImageList := TList.Create;
  FActiveImage := -1;
end; // TmcmMultiImageCtrl.Create.


destructor TmcmMultiImageCtrl.Destroy;
begin
  Clear;
  FImageList.Free;
  Inherited Destroy;
end; // TmcmMultiImageCtrl.Destroy.


procedure TmcmMultiImageCtrl.Clear;
var i : integer;
begin
  FImage := Nil;
  for i := (FImageList.Count - 1) downto 0
  do begin
     if Assigned(FImageList.Items[i])
     then TmcmImage(FImageList.Items[i]).Free;
     FImageList.Items[i] := Nil;
     FImageList.Delete(i);
  end;
  FActiveImage := -1;
end; // TmcmMultiImageCtrl.Clear.


function TmcmMultiImageCtrl.GetCount : integer;
begin
  if Assigned(FImageList)
  then Result := FImageList.Count
  else Result := 0;
end; // TmcmMultiImageCtrl.GetCount.


procedure TmcmMultiImageCtrl.SetActiveImage(Index : integer);
begin
  if Assigned(FImageList)
  then begin
       if (FActiveImage <> Index) and (Index < FImageList.Count)
       then begin
            FActiveImage := Index;
            if Assigned(FImage)
            then FImage.FOnChange := Nil;
            FImage := FImageList.Items[Index];
            FImage.FOnChange := Changed;
       end;
  end;
end; // TmcmMultiImageCtrl.SetActiveImage.


function TmcmMultiImageCtrl.AddImage : TmcmImage;
var NewImage : TmcmImage;
begin
  if Assigned(FImageList)
  then begin
       try
         NewImage := TmcmImage.Create;
       except
         NewImage := Nil;
       end;
       if Assigned(NewImage)
       then begin
            FImageList.Add(NewImage);
            if (FImageList.Count = 1)
            then ActiveImage := 0;
       end;
       Result := NewImage;
  end
  else Result := Nil;
end; // TmcmMultiImageCtrl.AddImage.


function TmcmMultiImageCtrl.GetImage : TmcmImage;
begin
  Result := FImage;
end; // TmcmMultiImageCtrl.GetImage.


procedure TmcmMultiImageCtrl.SetImage(Value : TmcmImage);
var i : integer;
begin
  if Assigned(Value)
  then begin
       i := FImageList.IndexOf(FImage);
       if (i >= 0)
       then begin
            TmcmImage(FImageList.Items[i]).Free;
            FImageList.Items[i] := Value;
            ActiveImage := i;
       end;
  end;
end; // TmcmMultiImageCtrl.SetImage.


function TmcmMultiImageCtrl.GetImageItem(Index : word) : TmcmImage;
begin
  Result := Nil;
  if Assigned(FImageList)
  then begin
       if (Index < FImageList.Count)
       then Result := FImageList.Items[Index];
  end;
end; // TmcmMultiImageCtrl.GetImageItem.


procedure TmcmMultiImageCtrl.SetImageItem(Index : word; NewImage : TmcmImage);
begin
  if Assigned(FImageList)
  then begin
       if (Index < FImageList.Count)
       then begin
            if Assigned(FImageList.Items[Index])
            then TmcmImage(FImageList.Items[Index]).Free;
            FImageList.Items[Index] := NewImage;
       end;
  end;
end; // TmcmMultiImageCtrl.SetImageItem.


{$UNDEF DCB3_4}

{$IFDEF mcmGDIPlus}
var GDIPError   : integer;
    GDIStartup  : GdiplusStartupInput;
    G_InitToken : dword;           // Token for GDI+ initialisation and shutdown.
{$ENDIF}
Initialization
  {$IFDEF MCMDEMO}
    mcmImgAboutBox := TmcmImgAboutBox.Create(Nil);
    mcmImgAboutBox.DemoVersion := True;
    mcmImgAboutBox.ShowModal;
    mcmImgAboutBox.Free;
    mcmImgAboutBox := Nil;
  {$ENDIF}

{$IFDEF mcmGDIPlus}
  FillChar(GDIStartup, SizeOf(GDIStartup), 0);
  GDIStartup.GdiplusVersion := 1;
  GDIPError := GdiPlusStartup(G_InitToken, @GDIStartup, Nil);
  if (GDIPError <> 0)
  then ;//raise EGDIPlus.Create(Format('GDI+ initialisation failed [%d]', [GDIPError]));
{$ENDIF}

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

Finalization
{$IFDEF mcmGDIPlus}
  GdiplusShutdown(G_InitToken);
{$ENDIF}
end.
