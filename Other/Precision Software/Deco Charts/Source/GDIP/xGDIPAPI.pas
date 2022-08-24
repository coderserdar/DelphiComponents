      {******************************************************************}
      { GDI+ API                                                         }
      {                                                                  }
      { home page : http://www.progdigy.com                              }
      { email     : hgourvest@progdigy.com                               }
      {                                                                  }
      { date      : 15-02-2002                                           }
      {                                                                  }
      { The contents of this file are used with permission, subject to   }
      { the Mozilla Public License Version 1.1 (the "License"); you may  }
      { not use this file except in compliance with the License. You may }
      { obtain a copy of the License at                                  }
      { http://www.mozilla.org/MPL/MPL-1.1.html                          }
      {                                                                  }
      { Software distributed under the License is distributed on an      }
      { "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
      { implied. See the License for the specific language governing     }
      { rights and limitations under the License.                        }
      {                                                                  }
      { *****************************************************************}
      {                                                                  }
      { GDI+ API with dynamic loading portion created by theMIROn        }
      {                                                                  }
      { home page : http://themiron.mirandaim.ru                         }
      { email     : themiron@mail.ru                                     }
      { date      : 28-03-2007                                           }
      {                                                                  }
      { *****************************************************************}

unit xGDIPAPI;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

(**************************************************************************\
*
*   GDI+ public header file
*
\**************************************************************************)

uses
  Windows,
  ActiveX,
  xDirectDraw,
  Math;

type
  INT16   = type Smallint;
  UINT16  = type Word;
  PUINT16 = ^UINT16;
  UINT32  = type Cardinal;
  TSingleDynArray = array of Single;

(**************************************************************************\
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)

const WINGDIPDLL = 'gdiplus.dll';

//----------------------------------------------------------------------------
// Memory Allocation APIs
//----------------------------------------------------------------------------

{$EXTERNALSYM GdipAlloc}
var GdipAlloc: function(size: ULONG): pointer; stdcall;
{$EXTERNALSYM GdipFree}
var GdipFree: procedure(ptr: pointer); stdcall;

(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

type
  TGdiplusBase = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

(**************************************************************************\
*
*   GDI+ Enumeration Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM GraphicsState}
  GraphicsState     = UINT;
  {$EXTERNALSYM GraphicsContainer}
  GraphicsContainer = UINT;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM FillMode}
  FillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );
  TFillMode = FillMode;

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM QualityMode}
  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );
  TQualityMode = QualityMode;
{$ELSE}
  {$EXTERNALSYM QualityMode}
  QualityMode = Integer;
  const
    QualityModeInvalid   = -1;
    QualityModeDefault   =  0;
    QualityModeLow       =  1; // Best performance
    QualityModeHigh      =  2; // Best rendering quality
{$ENDIF}

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM CompositingMode}
  CompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );
  TCompositingMode = CompositingMode;

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM CompositingQuality}
  CompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  TCompositingQuality = CompositingQuality;
{$ELSE}
  {$EXTERNALSYM CompositingQuality}
  CompositingQuality = Integer;
  const
    CompositingQualityInvalid          = QualityModeInvalid;
    CompositingQualityDefault          = QualityModeDefault;
    CompositingQualityHighSpeed        = QualityModeLow;
    CompositingQualityHighQuality      = QualityModeHigh;
    CompositingQualityGammaCorrected   = 3;
    CompositingQualityAssumeLinear     = 4;

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------

 // {$EXTERNALSYM Unit}
  Unit_ = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );
  TUnit = Unit_;

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel      = ord(UnitPixel),
    MetafileFrameUnitPoint      = ord(UnitPoint),
    MetafileFrameUnitInch       = ord(UnitInch),
    MetafileFrameUnitDocument   = ord(UnitDocument),
    MetafileFrameUnitMillimeter = ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  {$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = Integer;
  const
    MetafileFrameUnitPixel      = 2;
    MetafileFrameUnitPoint      = 3;
    MetafileFrameUnitInch       = 4;
    MetafileFrameUnitDocument   = 5;
    MetafileFrameUnitMillimeter = 6;
    MetafileFrameUnitGdi        = 7; // GDI compatible .01 MM units

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}
//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------

  {$EXTERNALSYM CoordinateSpace}
  CoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );
  TCoordinateSpace = CoordinateSpace;

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  {$EXTERNALSYM WrapMode}
  WrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TWrapMode = WrapMode;

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  {$EXTERNALSYM HatchStyle}
  HatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond,                // = 52,

    HatchStyleTotal                        // = 53,
  );

  const
    HatchStyleLargeGrid = HatchStyleCross; // 4
    HatchStyleMin       = HatchStyleHorizontal;
    HatchStyleMax       = HatchStyleSolidDiamond;

type
  THatchStyle = HatchStyle;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM DashStyle}
  DashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );
  TDashStyle = DashStyle;

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM DashCap}
  DashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );
  TDashCap = DashCap;
{$ELSE}
  {$EXTERNALSYM DashCap}
  DashCap = Integer;
  const
    DashCapFlat             = 0;
    DashCapRound            = 2;
    DashCapTriangle         = 3;

type
  TDashCap = DashCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM LineCap}
  LineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );
  TLineCap = LineCap;
{$ELSE}
  {$EXTERNALSYM LineCap}
  LineCap = Integer;
  const
    LineCapFlat             = 0;
    LineCapSquare           = 1;
    LineCapRound            = 2;
    LineCapTriangle         = 3;

    LineCapNoAnchor         = $10; // corresponds to flat cap
    LineCapSquareAnchor     = $11; // corresponds to square cap
    LineCapRoundAnchor      = $12; // corresponds to round cap
    LineCapDiamondAnchor    = $13; // corresponds to triangle cap
    LineCapArrowAnchor      = $14; // no correspondence

    LineCapCustom           = $ff; // custom cap

    LineCapAnchorMask       = $f0; // mask to check for anchor or not.

type
  TLineCap = LineCap;
{$ENDIF}

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM CustomLineCapType}
  CustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );
  TCustomLineCapType = CustomLineCapType;

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM LineJoin}
  LineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );
  TLineJoin = LineJoin;

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------

{$IFDEF DELPHI6_UP}
  {$Z1}
  {$EXTERNALSYM PathPointType}
  PathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  TPathPointType = PathPointType;
  {$Z4}
{$ELSE}
  {$EXTERNALSYM PathPointType}
  PathPointType = Byte;
  const
    PathPointTypeStart          : Byte = $00; // move
    PathPointTypeLine           : Byte = $01; // line
    PathPointTypeBezier         : Byte = $03; // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask   : Byte = $07; // type mask (lowest 3 bits).
    PathPointTypeDashMode       : Byte = $10; // currently in dash mode.
    PathPointTypePathMarker     : Byte = $20; // a marker for the path.
    PathPointTypeCloseSubpath   : Byte = $80; // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3        : Byte = $03;  // cubic Bezier

type
  TPathPointType = PathPointType;
{$ENDIF}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM WarpMode}
  WarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  TWarpMode = WarpMode;

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  {$EXTERNALSYM LinearGradientMode}
  LinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  TLinearGradientMode = LinearGradientMode;

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  {$EXTERNALSYM CombineMode}
  CombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  TCombineMode = CombineMode;

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageType}
  ImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );
  TImageType = ImageType;

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM InterpolationMode}
  InterpolationMode = (
    InterpolationModeInvalid          = ord(QualityModeInvalid),
    InterpolationModeDefault          = ord(QualityModeDefault),
    InterpolationModeLowQuality       = ord(QualityModeLow),
    InterpolationModeHighQuality      = ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  TInterpolationMode = InterpolationMode;
{$ELSE}
  {$EXTERNALSYM InterpolationMode}
  InterpolationMode = Integer;
  const
    InterpolationModeInvalid             = QualityModeInvalid;
    InterpolationModeDefault             = QualityModeDefault;
    InterpolationModeLowQuality          = QualityModeLow;
    InterpolationModeHighQuality         = QualityModeHigh;
    InterpolationModeBilinear            = 3;
    InterpolationModeBicubic             = 4;
    InterpolationModeNearestNeighbor     = 5;
    InterpolationModeHighQualityBilinear = 6;
    InterpolationModeHighQualityBicubic  = 7;

type
  TInterpolationMode = InterpolationMode;
{$ENDIF}

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------

  {$EXTERNALSYM PenAlignment}
  PenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );
  TPenAlignment = PenAlignment;

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  {$EXTERNALSYM BrushType}
  BrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient 
  );
  TBrushType = BrushType;

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM PenType}
  PenType = (
   PenTypeSolidColor       =  ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  ord(BrushTypeTextureFill),
   PenTypePathGradient     =  ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );
  TPenType = PenType;
{$ELSE}
  {$EXTERNALSYM PenType}
  PenType = Integer;
  const
    PenTypeSolidColor       =  0;
    PenTypeHatchFill        =  1;
    PenTypeTextureFill      =  2;
    PenTypePathGradient     =  3;
    PenTypeLinearGradient   =  4;
    PenTypeUnknown          = -1;

type
  TPenType = PenType;
{$ENDIF}

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------

  {$EXTERNALSYM MatrixOrder}
  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );
  TMatrixOrder = MatrixOrder;

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  {$EXTERNALSYM GenericFontFamily}
  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  TGenericFontFamily = GenericFontFamily;

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM FontStyle}
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TFontStyle = FontStyle;

//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM SmoothingMode}
  SmoothingMode = (
    SmoothingModeInvalid     = ord(QualityModeInvalid),
    SmoothingModeDefault     = ord(QualityModeDefault),
    SmoothingModeHighSpeed   = ord(QualityModeLow),
    SmoothingModeHighQuality = ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  );
  TSmoothingMode = SmoothingMode;
{$ELSE}
  {$EXTERNALSYM SmoothingMode}
  SmoothingMode = Integer;
  const
    SmoothingModeInvalid     = QualityModeInvalid;
    SmoothingModeDefault     = QualityModeDefault;
    SmoothingModeHighSpeed   = QualityModeLow;
    SmoothingModeHighQuality = QualityModeHigh;
    SmoothingModeNone        = 3;
    SmoothingModeAntiAlias   = 4;

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  {$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = Integer;
  const
    PixelOffsetModeInvalid     = QualityModeInvalid;
    PixelOffsetModeDefault     = QualityModeDefault;
    PixelOffsetModeHighSpeed   = QualityModeLow;
    PixelOffsetModeHighQuality = QualityModeHigh;
    PixelOffsetModeNone        = 3;    // No pixel offset
    PixelOffsetModeHalf        = 4;    // Offset by -0.5, -0.5 for fast anti-alias perf

type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------

  {$EXTERNALSYM TextRenderingHint}
  TextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );
  TTextRenderingHint = TextRenderingHint;

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  {$EXTERNALSYM MetafileType}
  MetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );
  TMetafileType = MetafileType;

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM EmfType}
  EmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
  TEmfType = EmfType;
{$ELSE}
  {$EXTERNALSYM EmfType}
  EmfType = Integer;
  const
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf);          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);   // both EMF+ and EMF

type
  TEmfType = EmfType;
{$ENDIF}

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

  {$EXTERNALSYM ObjectType}
  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

function ObjectTypeIsValid(type_: ObjectType): BOOL;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {$EXTERNALSYM GDIP_WMF_RECORD_BASE}

// macros
function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;


{$IFDEF DELPHI6_UP}
type
  {$EXTERNALSYM EmfPlusRecordType}
  EmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE),  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE),  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE),  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE),  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE),  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE),  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE),  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE),  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );
  TEmfPlusRecordType = EmfPlusRecordType;
{$ELSE}
type
  {$EXTERNALSYM EmfPlusRecordType}
  EmfPlusRecordType = Integer;
  // Since we have to enumerate GDI records right along with GDI+ records,
  // We list all the GDI records here so that they can be part of the
  // same enumeration type which is used in the enumeration callback.
  const
    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE);  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE);  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE);  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE);  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE);  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE);  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE);  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE);  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE);  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE);

    EmfRecordTypeHeader                  = EMR_HEADER;
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER;
    EmfRecordTypePolygon                 = EMR_POLYGON;
    EmfRecordTypePolyline                = EMR_POLYLINE;
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO;
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO;
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE;
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON;
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX;
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX;
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX;
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX;
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX;
    EmfRecordTypeEOF                     = EMR_EOF;
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV;
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS;
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE;
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE;
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE;
    EmfRecordTypeSetROP2                 = EMR_SETROP2;
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE;
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN;
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT;
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR;
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR;
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN;
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX;
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN;
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT;
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT;
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX;
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX;
    EmfRecordTypeSaveDC                  = EMR_SAVEDC;
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC;
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM;
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM;
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT;
    EmfRecordTypeCreatePen               = EMR_CREATEPEN;
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT;
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT;
    EmfRecordTypeAngleArc                = EMR_ANGLEARC;
    EmfRecordTypeEllipse                 = EMR_ELLIPSE;
    EmfRecordTypeRectangle               = EMR_RECTANGLE;
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT;
    EmfRecordTypeArc                     = EMR_ARC;
    EmfRecordTypeChord                   = EMR_CHORD;
    EmfRecordTypePie                     = EMR_PIE;
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE;
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE;
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES;
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE;
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE;
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL;
    EmfRecordTypeLineTo                  = EMR_LINETO;
    EmfRecordTypeArcTo                   = EMR_ARCTO;
    EmfRecordTypePolyDraw                = EMR_POLYDRAW;
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION;
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT;
    EmfRecordTypeBeginPath               = EMR_BEGINPATH;
    EmfRecordTypeEndPath                 = EMR_ENDPATH;
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE;
    EmfRecordTypeFillPath                = EMR_FILLPATH;
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH;
    EmfRecordTypeStrokePath              = EMR_STROKEPATH;
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH;
    EmfRecordTypeWidenPath               = EMR_WIDENPATH;
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH;
    EmfRecordTypeAbortPath               = EMR_ABORTPATH;
    EmfRecordTypeReserved_069            = 69;  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT;
    EmfRecordTypeFillRgn                 = EMR_FILLRGN;
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN;
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN;
    EmfRecordTypePaintRgn                = EMR_PAINTRGN;
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN;
    EmfRecordTypeBitBlt                  = EMR_BITBLT;
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT;
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT;
    EmfRecordTypePlgBlt                  = EMR_PLGBLT;
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE;
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS;
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW;
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA;
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW;
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16;
    EmfRecordTypePolygon16               = EMR_POLYGON16;
    EmfRecordTypePolyline16              = EMR_POLYLINE16;
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16;
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16;
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16;
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16;
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16;
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH;
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT;
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN;
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA;
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW;
    EmfRecordTypeSetICMMode              = 98;  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99;  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100; // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101; // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102; // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103; // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104; // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105; // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106; // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107; // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108; // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109; // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110; // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111; // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112; // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113; // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114; // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115; // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116; // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117; // Not Used
    EmfRecordTypeGradientFill            = 118; // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119; // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120; // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121; // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122; // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122;
    EmfRecordTypeMin                     = 1;

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid   = GDIP_EMFPLUS_RECORD_BASE;
    EmfPlusRecordTypeHeader    = GDIP_EMFPLUS_RECORD_BASE + 1;
    EmfPlusRecordTypeEndOfFile = GDIP_EMFPLUS_RECORD_BASE + 2;

    EmfPlusRecordTypeComment   = GDIP_EMFPLUS_RECORD_BASE + 3;

    EmfPlusRecordTypeGetDC     = GDIP_EMFPLUS_RECORD_BASE + 4;

    EmfPlusRecordTypeMultiFormatStart   = GDIP_EMFPLUS_RECORD_BASE + 5;
    EmfPlusRecordTypeMultiFormatSection = GDIP_EMFPLUS_RECORD_BASE + 6;
    EmfPlusRecordTypeMultiFormatEnd     = GDIP_EMFPLUS_RECORD_BASE + 7;

    // For all persistent objects

    EmfPlusRecordTypeObject = GDIP_EMFPLUS_RECORD_BASE + 8;

    // Drawing Records

    EmfPlusRecordTypeClear           = GDIP_EMFPLUS_RECORD_BASE + 9;
    EmfPlusRecordTypeFillRects       = GDIP_EMFPLUS_RECORD_BASE + 10;
    EmfPlusRecordTypeDrawRects       = GDIP_EMFPLUS_RECORD_BASE + 11;
    EmfPlusRecordTypeFillPolygon     = GDIP_EMFPLUS_RECORD_BASE + 12;
    EmfPlusRecordTypeDrawLines       = GDIP_EMFPLUS_RECORD_BASE + 13;
    EmfPlusRecordTypeFillEllipse     = GDIP_EMFPLUS_RECORD_BASE + 14;
    EmfPlusRecordTypeDrawEllipse     = GDIP_EMFPLUS_RECORD_BASE + 15;
    EmfPlusRecordTypeFillPie         = GDIP_EMFPLUS_RECORD_BASE + 16;
    EmfPlusRecordTypeDrawPie         = GDIP_EMFPLUS_RECORD_BASE + 17;
    EmfPlusRecordTypeDrawArc         = GDIP_EMFPLUS_RECORD_BASE + 18;
    EmfPlusRecordTypeFillRegion      = GDIP_EMFPLUS_RECORD_BASE + 19;
    EmfPlusRecordTypeFillPath        = GDIP_EMFPLUS_RECORD_BASE + 20;
    EmfPlusRecordTypeDrawPath        = GDIP_EMFPLUS_RECORD_BASE + 21;
    EmfPlusRecordTypeFillClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 22;
    EmfPlusRecordTypeDrawClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 23;
    EmfPlusRecordTypeDrawCurve       = GDIP_EMFPLUS_RECORD_BASE + 24;
    EmfPlusRecordTypeDrawBeziers     = GDIP_EMFPLUS_RECORD_BASE + 25;
    EmfPlusRecordTypeDrawImage       = GDIP_EMFPLUS_RECORD_BASE + 26;
    EmfPlusRecordTypeDrawImagePoints = GDIP_EMFPLUS_RECORD_BASE + 27;
    EmfPlusRecordTypeDrawString      = GDIP_EMFPLUS_RECORD_BASE + 28;

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin      = GDIP_EMFPLUS_RECORD_BASE + 29;
    EmfPlusRecordTypeSetAntiAliasMode        = GDIP_EMFPLUS_RECORD_BASE + 30;
    EmfPlusRecordTypeSetTextRenderingHint    = GDIP_EMFPLUS_RECORD_BASE + 31;
    EmfPlusRecordTypeSetTextContrast         = GDIP_EMFPLUS_RECORD_BASE + 32;
    EmfPlusRecordTypeSetInterpolationMode    = GDIP_EMFPLUS_RECORD_BASE + 33;
    EmfPlusRecordTypeSetPixelOffsetMode      = GDIP_EMFPLUS_RECORD_BASE + 34;
    EmfPlusRecordTypeSetCompositingMode      = GDIP_EMFPLUS_RECORD_BASE + 35;
    EmfPlusRecordTypeSetCompositingQuality   = GDIP_EMFPLUS_RECORD_BASE + 36;
    EmfPlusRecordTypeSave                    = GDIP_EMFPLUS_RECORD_BASE + 37;
    EmfPlusRecordTypeRestore                 = GDIP_EMFPLUS_RECORD_BASE + 38;
    EmfPlusRecordTypeBeginContainer          = GDIP_EMFPLUS_RECORD_BASE + 39;
    EmfPlusRecordTypeBeginContainerNoParams  = GDIP_EMFPLUS_RECORD_BASE + 40;
    EmfPlusRecordTypeEndContainer            = GDIP_EMFPLUS_RECORD_BASE + 41;
    EmfPlusRecordTypeSetWorldTransform       = GDIP_EMFPLUS_RECORD_BASE + 42;
    EmfPlusRecordTypeResetWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 43;
    EmfPlusRecordTypeMultiplyWorldTransform  = GDIP_EMFPLUS_RECORD_BASE + 44;
    EmfPlusRecordTypeTranslateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 45;
    EmfPlusRecordTypeScaleWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 46;
    EmfPlusRecordTypeRotateWorldTransform    = GDIP_EMFPLUS_RECORD_BASE + 47;
    EmfPlusRecordTypeSetPageTransform        = GDIP_EMFPLUS_RECORD_BASE + 48;
    EmfPlusRecordTypeResetClip               = GDIP_EMFPLUS_RECORD_BASE + 49;
    EmfPlusRecordTypeSetClipRect             = GDIP_EMFPLUS_RECORD_BASE + 50;
    EmfPlusRecordTypeSetClipPath             = GDIP_EMFPLUS_RECORD_BASE + 51;
    EmfPlusRecordTypeSetClipRegion           = GDIP_EMFPLUS_RECORD_BASE + 52;
    EmfPlusRecordTypeOffsetClip              = GDIP_EMFPLUS_RECORD_BASE + 53;

    EmfPlusRecordTypeDrawDriverString        = GDIP_EMFPLUS_RECORD_BASE + 54;

    EmfPlusRecordTotal                       = GDIP_EMFPLUS_RECORD_BASE + 55;

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1;
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader;

type
  TEmfPlusRecordType = EmfPlusRecordType;
{$ENDIF}
//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------

  {$EXTERNALSYM StringFormatFlags}
  StringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;

    StringFormatFlagsNoClip                      = $00004000;

Type
  TStringFormatFlags = StringFormatFlags;

//---------------------------------------------------------------------------
// StringTrimming
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringTrimming}
  StringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );
  TStringTrimming = StringTrimming;

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringDigitSubstitute}
  StringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  {$EXTERNALSYM HotkeyPrefix}
  HotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );
  THotkeyPrefix = HotkeyPrefix;

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringAlignment}
  StringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );
  TStringAlignment = StringAlignment;

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  {$EXTERNALSYM DriverStringOptions}
  DriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

type
  TDriverStringOptions = DriverStringOptions;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM FlushIntention}
  FlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );
  TFlushIntention = FlushIntention;

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameterValueType}
  EncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
type
  TEncoderParameterValueType = EncoderParameterValueType;

//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderValue}
  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );
  TEncoderValue = EncoderValue;

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ELSE}
  {$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = Integer;
  const
    EmfToWmfBitsFlagsDefault          = $00000000;
    EmfToWmfBitsFlagsEmbedEmf         = $00000001;
    EmfToWmfBitsFlagsIncludePlaceable = $00000002;
    EmfToWmfBitsFlagsNoXORClip        = $00000004;
    
type
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ENDIF}
(**************************************************************************\
*
*   GDI+ Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageAbort}
  ImageAbort = function: BOOL; stdcall;
  {$EXTERNALSYM DrawImageAbort}
  DrawImageAbort         = ImageAbort;
  {$EXTERNALSYM GetThumbnailImageAbort}
  GetThumbnailImageAbort = ImageAbort;


  // Callback for EnumerateMetafile methods.  The parameters are:

  //      recordType      WMF, EMF, or EMF+ record type
  //      flags           (always 0 for WMF/EMF records)
  //      dataSize        size of the record data (in bytes), or 0 if no data
  //      data            pointer to the record data, or NULL if no data
  //      callbackData    pointer to callbackData, if any

  // This method can then call Metafile::PlayRecord to play the
  // record that was just enumerated.  If this method  returns
  // FALSE, the enumeration process is aborted.  Otherwise, it continues.

  {$EXTERNALSYM EnumerateMetafileProc}
  EnumerateMetafileProc = function(recordType: EmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE; callbackData: pointer): BOOL; stdcall;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  FLT_MIN =  1.175494351e-38; // min positive value

  REAL_MAX           = FLT_MAX;
  {$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM Status}
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPSizeF = ^TGPSizeF;
  TGPSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

  function MakeSize(Width, Height: Single): TGPSizeF; overload;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPSize = ^TGPSize;
  TGPSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

  function MakeSize(Width, Height: Integer): TGPSize; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPPointF = ^TGPPointF;
  TGPPointF = packed record
    X : Single;
    Y : Single;
  end;
  TPointFDynArray = array of TGPPointF;

  function MakePoint(X, Y: Single): TGPPointF; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPPoint = ^TGPPoint;
  TGPPoint = packed record
    X : Integer;
    Y : Integer;
  end;
  TPointDynArray = array of TGPPoint;

  function MakePoint(X, Y: Integer): TGPPoint; overload;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPRectF = ^TGPRectF;
  TGPRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;
  TRectFDynArray = array of TGPRectF;

  function MakeRect(x, y, width, height: Single): TGPRectF; overload;
  function MakeRect(location: TGPPointF; size: TGPSizeF): TGPRectF; overload;

type
  PGPRect = ^TGPRect;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;
  TRectDynArray = array of TGPRect;

  function MakeRect(x, y, width, height: Integer): TGPRect; overload;
  function MakeRect(location: TGPPoint; size: TGPSize): TGPRect; overload;
  function MakeRect(const Rect: TRect): TGPRect; overload;

type
  TPathData = packed class
  public
    Count  : Integer;
    Points : PGPPointF;
    Types  : PBYTE;
    constructor Create;
    destructor destroy; override;
  end;

  PCharacterRange = ^TCharacterRange;
  TCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  {$EXTERNALSYM DebugEventLevel}
  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  {$EXTERNALSYM DebugEventProc}
  DebugEventProc = procedure(level: DebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  {$EXTERNALSYM NotificationHookProc}
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  {$EXTERNALSYM NotificationUnhookProc}
  NotificationUnhookProc = procedure(token: ULONG); stdcall;

  // Input structure for GdiplusStartup

  {$EXTERNALSYM GdiplusStartupInput}
  GdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;                                        // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  {$EXTERNALSYM GdiplusStartupOutput}
  GdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

  {$EXTERNALSYM GdiplusStartup}
 var GdiplusStartup: function(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput): Status; stdcall;

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {$EXTERNALSYM GdiplusShutdown}
  var GdiplusShutdown: procedure(token: ULONG); stdcall;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   Gdiplus Pixel Formats
* Abstract:
*   GDI+ Pixel Formats
*
\**************************************************************************)

type
  PARGB  = ^ARGB;
  ARGB   = DWORD;
  {$EXTERNALSYM ARGB}
  ARGB64 = Int64;
  {$EXTERNALSYM ARGB64}

const
  ALPHA_SHIFT = 24;
  {$EXTERNALSYM ALPHA_SHIFT}
  RED_SHIFT   = 16;
  {$EXTERNALSYM RED_SHIFT}
  GREEN_SHIFT = 8;
  {$EXTERNALSYM GREEN_SHIFT}
  BLUE_SHIFT  = 0;
  {$EXTERNALSYM BLUE_SHIFT}
  ALPHA_MASK  = (ARGB($ff) shl ALPHA_SHIFT);
  {$EXTERNALSYM ALPHA_MASK}

  // In-memory pixel data formats:
  // bits 0-7 = format index
  // bits 8-15 = pixel size (in bits)
  // bits 16-23 = flags
  // bits 24-31 = reserved

type
  PixelFormat = Integer;
  {$EXTERNALSYM PixelFormat}
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000; // Indexes into a palette
  {$EXTERNALSYM PixelFormatIndexed}
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  {$EXTERNALSYM PixelFormatGDI}
  PixelFormatAlpha       = $00040000; // Has an alpha component
  {$EXTERNALSYM PixelFormatAlpha}
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  {$EXTERNALSYM PixelFormatPAlpha}
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  {$EXTERNALSYM PixelFormatExtended}
  PixelFormatCanonical   = $00200000;
  {$EXTERNALSYM PixelFormatCanonical}

  PixelFormatUndefined      = 0;
  {$EXTERNALSYM PixelFormatUndefined}
  PixelFormatDontCare       = 0;
  {$EXTERNALSYM PixelFormatDontCare}

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat1bppIndexed}
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat4bppIndexed}
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat8bppIndexed}
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat16bppGrayScale}
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppRGB555}
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppRGB565}
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppARGB1555}
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat24bppRGB}
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat32bppRGB}
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  {$EXTERNALSYM PixelFormat32bppARGB}
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat32bppPARGB}
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat48bppRGB}
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat64bppARGB}
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat64bppPARGB}
  PixelFormatMax            = 15;
  {$EXTERNALSYM PixelFormatMax}

{$EXTERNALSYM GetPixelFormatSize}
function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
{$EXTERNALSYM IsIndexedPixelFormat}
function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
{$EXTERNALSYM IsAlphaPixelFormat}
function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
{$EXTERNALSYM IsExtendedPixelFormat}
function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

{$EXTERNALSYM IsCanonicalPixelFormat}
function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;

{$IFDEF DELPHI6_UP}
type
  {$EXTERNALSYM PaletteFlags}
  PaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
  TPaletteFlags = PaletteFlags;
{$ELSE}
type
  {$EXTERNALSYM PaletteFlags}
  PaletteFlags = Integer;
  const
    PaletteFlagsHasAlpha    = $0001;
    PaletteFlagsGrayScale   = $0002;
    PaletteFlagsHalftone    = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}

  {$EXTERNALSYM ColorPalette}
  ColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of ARGB ; // Palette color entries
  end;

  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

(**************************************************************************\
*
*   GDI+ Color Object
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMode}
  ColorMode = (
    ColorModeARGB32,
    ColorModeARGB64
  );
  TColorMode = ColorMode;

//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorChannelFlags}
  ColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );
  TColorChannelFlags = ColorChannelFlags;

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components
  AlphaShift  = 24;
  {$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {$EXTERNALSYM BlueMask}


type
{  TGPColor = class
  protected
     Argb: ARGB;
  public
    constructor Create; overload;
    constructor Create(r, g, b: Byte); overload;
    constructor Create(a, r, g, b: Byte); overload;
    constructor Create(Value: ARGB); overload;
    function GetAlpha: BYTE;
    function GetA: BYTE;
    function GetRed: BYTE;
    function GetR: BYTE;
    function GetGreen: Byte;
    function GetG: Byte;
    function GetBlue: Byte;
    function GetB: Byte;
    function GetValue: ARGB;
    procedure SetValue(Value: ARGB);
    procedure SetFromCOLORREF(rgb: COLORREF);
    function ToCOLORREF: COLORREF;
    function MakeARGB(a, r, g, b: Byte): ARGB;
  end;  }

  PGPColor = ^TGPColor;
  TGPColor = ARGB;
  TColorDynArray = array of TGPColor;

  function MakeColor(r, g, b: Byte): ARGB; overload;
  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  function GetAlpha(color: ARGB): BYTE;
  function GetRed(color: ARGB): BYTE;
  function GetGreen(color: ARGB): BYTE;
  function GetBlue(color: ARGB): BYTE;
  function ColorRefToARGB(rgb: COLORREF): ARGB;
  function ARGBToColorRef(Color: ARGB): COLORREF;


(**************************************************************************\
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)

type
  { from Windef.h }
  RECTL = Windows.TRect;
  SIZEL = Windows.TSize;

  {$EXTERNALSYM ENHMETAHEADER3}
  ENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : RECTL;  // Inclusive-inclusive bounds in device units
    rclFrame       : RECTL;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : SIZEL;  // Size of the reference device in pels
    szlMillimeters : SIZEL;  // Size of the reference device in millimeters
  end;
  TENHMETAHEADER3 = ENHMETAHEADER3;
  PENHMETAHEADER3 = ^TENHMETAHEADER3;

  // Placeable WMFs

  // Placeable Metafiles were created as a non-standard way of specifying how
  // a metafile is mapped and scaled on an output device.
  // Placeable metafiles are quite wide-spread, but not directly supported by
  // the Windows API. To playback a placeable metafile using the Windows API,
  // you will first need to strip the placeable metafile header from the file.
  // This is typically performed by copying the metafile to a temporary file
  // starting at file offset 22 (0x16). The contents of the temporary file may
  // then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
  // CopyMetaFile(), etc. GDI functions.

  // Each placeable metafile begins with a 22-byte header,
  //  followed by a standard metafile:

  {$EXTERNALSYM PWMFRect16}
  PWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  TPWMFRect16 = PWMFRect16;
  PPWMFRect16 = ^TPWMFRect16;

  {$EXTERNALSYM WmfPlaceableFileHeader}
  WmfPlaceableFileHeader = packed record
    Key         : UINT32;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : PWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : UINT32;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

  // Key contains a special identification value that indicates the presence
  // of a placeable metafile header and is always 0x9AC6CDD7.

  // Handle is used to stored the handle of the metafile in memory. When written
  // to disk, this field is not used and will always contains the value 0.

  // Left, Top, Right, and Bottom contain the coordinates of the upper-left
  // and lower-right corners of the image on the output device. These are
  // measured in twips.

  // A twip (meaning "twentieth of a point") is the logical unit of measurement
  // used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
  // twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

  // Inch contains the number of twips per inch used to represent the image.
  // Normally, there are 1440 twips per inch; however, this number may be
  // changed to scale the image. A value of 720 indicates that the image is
  // double its normal size, or scaled to a factor of 2:1. A value of 360
  // indicates a scale of 4:1, while a value of 2880 indicates that the image
  // is scaled down in size by a factor of two. A value of 1440 indicates
  // a 1:1 scale ratio.

  // Reserved is not used and is always set to 0.

  // Checksum contains a checksum value for the previous 10 WORDs in the header.
  // This value can be used in an attempt to detect if the metafile has become
  // corrupted. The checksum is calculated by XORing each WORD value to an
  // initial value of 0.

  // If the metafile was recorded with a reference Hdc that was a display.

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

type
  TMetafileHeader = packed class
  public
    Type_        : TMetafileType;
    Size         : UINT;           // Size of the metafile (in bytes)
    Version      : UINT;           // EMF+, EMF, or WMF version
    EmfPlusFlags : UINT;
    DpiX         : Single;
    DpiY         : Single;
    X            : Integer;        // Bounds in device units
    Y            : Integer;
    Width        : Integer;
    Height       : Integer;
    Header       : record
    case integer of
      0: (WmfHeader: TMETAHEADER;);
      1: (EmfHeader: TENHMETAHEADER3);
    end;
    EmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    LogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    LogicalDpiY       : Integer; // usually valid only for EMF+
  public
    property GetType: TMetafileType read Type_;
    property GetMetafileSize: UINT read Size;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    property GetVersion: UINT read Version;
     // Get the EMF+ flags associated with the metafile
    property GetEmfPlusFlags: UINT read EmfPlusFlags;
    property GetDpiX: Single read DpiX;
    property GetDpiY: Single read DpiY;
    procedure GetBounds(out Rect: TGPRect);
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf: BOOL;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable: BOOL;
    // Is this an EMF (not an EMF+)?
    function IsEmf: BOOL;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus: BOOL;
    // Is this an EMF+ file?
    function IsEmfPlus: BOOL;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual: BOOL;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly: BOOL;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay: BOOL;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader: PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader: PENHMETAHEADER3;
  end;

(**************************************************************************\
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {$EXTERNALSYM CodecIImageBytes}

type
  {$EXTERNALSYM IImageBytes}
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: pointer): HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageCodecInfo}
  ImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  {$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = Integer;
  const
    ImageCodecFlagsEncoder            = $00000001;
    ImageCodecFlagsDecoder            = $00000002;
    ImageCodecFlagsSupportBitmap      = $00000004;
    ImageCodecFlagsSupportVector      = $00000008;
    ImageCodecFlagsSeekableEncode     = $00000010;
    ImageCodecFlagsBlockingDecode     = $00000020;

    ImageCodecFlagsBuiltin            = $00010000;
    ImageCodecFlagsSystem             = $00020000;
    ImageCodecFlagsUser               = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}
//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

  {$EXTERNALSYM ImageLockMode}
  ImageLockMode = Integer;
  const
    ImageLockModeRead         = $0001;
    ImageLockModeWrite        = $0002;
    ImageLockModeUserInputBuf = $0004;
type
  TImageLockMode = ImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

  {$EXTERNALSYM BitmapData}
  BitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : PixelFormat;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM ImageFlags}
  ImageFlags = (
    ImageFlagsNone                = 0,

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,

    // High-word

    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000
  );
  TImageFlags = ImageFlags;
{$ELSE}
  {$EXTERNALSYM ImageFlags}
  ImageFlags = Integer;
  const
    ImageFlagsNone                = 0;

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001;
    ImageFlagsHasAlpha            = $0002;
    ImageFlagsHasTranslucent      = $0004;
    ImageFlagsPartiallyScalable   = $0008;

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010;
    ImageFlagsColorSpaceCMYK      = $0020;
    ImageFlagsColorSpaceGRAY      = $0040;
    ImageFlagsColorSpaceYCBCR     = $0080;
    ImageFlagsColorSpaceYCCK      = $0100;

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000;
    ImageFlagsHasRealPixelSize    = $2000;

    // High-word

    ImageFlagsReadOnly            = $00010000;
    ImageFlagsCaching             = $00020000;

type
  TImageFlags = ImageFlags;
{$ENDIF}


{$IFDEF DELPHI6_UP}
  {$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  TRotateFlipType = RotateFlipType;
{$ELSE}
  {$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone, // = 0,
    Rotate90FlipNone,   // = 1,
    Rotate180FlipNone,  // = 2,
    Rotate270FlipNone,  // = 3,

    RotateNoneFlipX,    // = 4,
    Rotate90FlipX,      // = 5,
    Rotate180FlipX,     // = 6,
    Rotate270FlipX      // = 7,
  );
  const
    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;

    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

type
  TRotateFlipType = RotateFlipType;
{$ENDIF}

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameter}
  EncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameters}
  EncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // Parameter values
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  {$EXTERNALSYM PropertyItem}
  PropertyItem = record // NOT PACKED !!
    id       : PROPID;  // ID of this property
    length   : ULONG;   // Length of the property value, in bytes
    type_    : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value    : Pointer; // property value
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  PropertyTagTypeByte      : Integer =  1;
  {$EXTERNALSYM PropertyTagTypeByte}
  PropertyTagTypeASCII     : Integer =  2;
  {$EXTERNALSYM PropertyTagTypeASCII}
  PropertyTagTypeShort     : Integer =  3;
  {$EXTERNALSYM PropertyTagTypeShort}
  PropertyTagTypeLong      : Integer =  4;
  {$EXTERNALSYM PropertyTagTypeLong}
  PropertyTagTypeRational  : Integer =  5;
  {$EXTERNALSYM PropertyTagTypeRational}
  PropertyTagTypeUndefined : Integer =  7;
  {$EXTERNALSYM PropertyTagTypeUndefined}
  PropertyTagTypeSLONG     : Integer =  9;
  {$EXTERNALSYM PropertyTagTypeSLONG}
  PropertyTagTypeSRational : Integer = 10;
  {$EXTERNALSYM PropertyTagTypeSRational}

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  PropertyTagExifIFD            = $8769;
  {$EXTERNALSYM PropertyTagExifIFD}
  PropertyTagGpsIFD             = $8825;
  {$EXTERNALSYM PropertyTagGpsIFD}

  PropertyTagNewSubfileType     = $00FE;
  {$EXTERNALSYM PropertyTagNewSubfileType}
  PropertyTagSubfileType        = $00FF;
  {$EXTERNALSYM PropertyTagSubfileType}
  PropertyTagImageWidth         = $0100;
  {$EXTERNALSYM PropertyTagImageWidth}
  PropertyTagImageHeight        = $0101;
  {$EXTERNALSYM PropertyTagImageHeight}
  PropertyTagBitsPerSample      = $0102;
  {$EXTERNALSYM PropertyTagBitsPerSample}
  PropertyTagCompression        = $0103;
  {$EXTERNALSYM PropertyTagCompression}
  PropertyTagPhotometricInterp  = $0106;
  {$EXTERNALSYM PropertyTagPhotometricInterp}
  PropertyTagThreshHolding      = $0107;
  {$EXTERNALSYM PropertyTagThreshHolding}
  PropertyTagCellWidth          = $0108;
  {$EXTERNALSYM PropertyTagCellWidth}
  PropertyTagCellHeight         = $0109;
  {$EXTERNALSYM PropertyTagCellHeight}
  PropertyTagFillOrder          = $010A;
  {$EXTERNALSYM PropertyTagFillOrder}
  PropertyTagDocumentName       = $010D;
  {$EXTERNALSYM PropertyTagDocumentName}
  PropertyTagImageDescription   = $010E;
  {$EXTERNALSYM PropertyTagImageDescription}
  PropertyTagEquipMake          = $010F;
  {$EXTERNALSYM PropertyTagEquipMake}
  PropertyTagEquipModel         = $0110;
  {$EXTERNALSYM PropertyTagEquipModel}
  PropertyTagStripOffsets       = $0111;
  {$EXTERNALSYM PropertyTagStripOffsets}
  PropertyTagOrientation        = $0112;
  {$EXTERNALSYM PropertyTagOrientation}
  PropertyTagSamplesPerPixel    = $0115;
  {$EXTERNALSYM PropertyTagSamplesPerPixel}
  PropertyTagRowsPerStrip       = $0116;
  {$EXTERNALSYM PropertyTagRowsPerStrip}
  PropertyTagStripBytesCount    = $0117;
  {$EXTERNALSYM PropertyTagStripBytesCount}
  PropertyTagMinSampleValue     = $0118;
  {$EXTERNALSYM PropertyTagMinSampleValue}
  PropertyTagMaxSampleValue     = $0119;
  {$EXTERNALSYM PropertyTagMaxSampleValue}
  PropertyTagXResolution        = $011A;   // Image resolution in width direction
  {$EXTERNALSYM PropertyTagXResolution}
  PropertyTagYResolution        = $011B;   // Image resolution in height direction
  {$EXTERNALSYM PropertyTagYResolution}
  PropertyTagPlanarConfig       = $011C;   // Image data arrangement
  {$EXTERNALSYM PropertyTagPlanarConfig}
  PropertyTagPageName           = $011D;
  {$EXTERNALSYM PropertyTagPageName}
  PropertyTagXPosition          = $011E;
  {$EXTERNALSYM PropertyTagXPosition}
  PropertyTagYPosition          = $011F;
  {$EXTERNALSYM PropertyTagYPosition}
  PropertyTagFreeOffset         = $0120;
  {$EXTERNALSYM PropertyTagFreeOffset}
  PropertyTagFreeByteCounts     = $0121;
  {$EXTERNALSYM PropertyTagFreeByteCounts}
  PropertyTagGrayResponseUnit   = $0122;
  {$EXTERNALSYM PropertyTagGrayResponseUnit}
  PropertyTagGrayResponseCurve  = $0123;
  {$EXTERNALSYM PropertyTagGrayResponseCurve}
  PropertyTagT4Option           = $0124;
  {$EXTERNALSYM PropertyTagT4Option}
  PropertyTagT6Option           = $0125;
  {$EXTERNALSYM PropertyTagT6Option}
  PropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  {$EXTERNALSYM PropertyTagResolutionUnit}
  PropertyTagPageNumber         = $0129;
  {$EXTERNALSYM PropertyTagPageNumber}
  PropertyTagTransferFuncition  = $012D;
  {$EXTERNALSYM PropertyTagTransferFuncition}
  PropertyTagSoftwareUsed       = $0131;
  {$EXTERNALSYM PropertyTagSoftwareUsed}
  PropertyTagDateTime           = $0132;
  {$EXTERNALSYM PropertyTagDateTime}
  PropertyTagArtist             = $013B;
  {$EXTERNALSYM PropertyTagArtist}
  PropertyTagHostComputer       = $013C;
  {$EXTERNALSYM PropertyTagHostComputer}
  PropertyTagPredictor          = $013D;
  {$EXTERNALSYM PropertyTagPredictor}
  PropertyTagWhitePoint         = $013E;
  {$EXTERNALSYM PropertyTagWhitePoint}
  PropertyTagPrimaryChromaticities = $013F;
  {$EXTERNALSYM PropertyTagPrimaryChromaticities}
  PropertyTagColorMap           = $0140;
  {$EXTERNALSYM PropertyTagColorMap}
  PropertyTagHalftoneHints      = $0141;
  {$EXTERNALSYM PropertyTagHalftoneHints}
  PropertyTagTileWidth          = $0142;
  {$EXTERNALSYM PropertyTagTileWidth}
  PropertyTagTileLength         = $0143;
  {$EXTERNALSYM PropertyTagTileLength}
  PropertyTagTileOffset         = $0144;
  {$EXTERNALSYM PropertyTagTileOffset}
  PropertyTagTileByteCounts     = $0145;
  {$EXTERNALSYM PropertyTagTileByteCounts}
  PropertyTagInkSet             = $014C;
  {$EXTERNALSYM PropertyTagInkSet}
  PropertyTagInkNames           = $014D;
  {$EXTERNALSYM PropertyTagInkNames}
  PropertyTagNumberOfInks       = $014E;
  {$EXTERNALSYM PropertyTagNumberOfInks}
  PropertyTagDotRange           = $0150;
  {$EXTERNALSYM PropertyTagDotRange}
  PropertyTagTargetPrinter      = $0151;
  {$EXTERNALSYM PropertyTagTargetPrinter}
  PropertyTagExtraSamples       = $0152;
  {$EXTERNALSYM PropertyTagExtraSamples}
  PropertyTagSampleFormat       = $0153;
  {$EXTERNALSYM PropertyTagSampleFormat}
  PropertyTagSMinSampleValue    = $0154;
  {$EXTERNALSYM PropertyTagSMinSampleValue}
  PropertyTagSMaxSampleValue    = $0155;
  {$EXTERNALSYM PropertyTagSMaxSampleValue}
  PropertyTagTransferRange      = $0156;
  {$EXTERNALSYM PropertyTagTransferRange}

  PropertyTagJPEGProc               = $0200;
  {$EXTERNALSYM PropertyTagJPEGProc}
  PropertyTagJPEGInterFormat        = $0201;
  {$EXTERNALSYM PropertyTagJPEGInterFormat}
  PropertyTagJPEGInterLength        = $0202;
  {$EXTERNALSYM PropertyTagJPEGInterLength}
  PropertyTagJPEGRestartInterval    = $0203;
  {$EXTERNALSYM PropertyTagJPEGRestartInterval}
  PropertyTagJPEGLosslessPredictors = $0205;
  {$EXTERNALSYM PropertyTagJPEGLosslessPredictors}
  PropertyTagJPEGPointTransforms    = $0206;
  {$EXTERNALSYM PropertyTagJPEGPointTransforms}
  PropertyTagJPEGQTables            = $0207;
  {$EXTERNALSYM PropertyTagJPEGQTables}
  PropertyTagJPEGDCTables           = $0208;
  {$EXTERNALSYM PropertyTagJPEGDCTables}
  PropertyTagJPEGACTables           = $0209;
  {$EXTERNALSYM PropertyTagJPEGACTables}

  PropertyTagYCbCrCoefficients  = $0211;
  {$EXTERNALSYM PropertyTagYCbCrCoefficients}
  PropertyTagYCbCrSubsampling   = $0212;
  {$EXTERNALSYM PropertyTagYCbCrSubsampling}
  PropertyTagYCbCrPositioning   = $0213;
  {$EXTERNALSYM PropertyTagYCbCrPositioning}
  PropertyTagREFBlackWhite      = $0214;
  {$EXTERNALSYM PropertyTagREFBlackWhite}

  PropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
  {$EXTERNALSYM PropertyTagICCProfile}
                                           // for embedded ICC in TIFF
  PropertyTagGamma                = $0301;
  {$EXTERNALSYM PropertyTagGamma}
  PropertyTagICCProfileDescriptor = $0302;
  {$EXTERNALSYM PropertyTagICCProfileDescriptor}
  PropertyTagSRGBRenderingIntent  = $0303;
  {$EXTERNALSYM PropertyTagSRGBRenderingIntent}

  PropertyTagImageTitle         = $0320;
  {$EXTERNALSYM PropertyTagImageTitle}
  PropertyTagCopyright          = $8298;
  {$EXTERNALSYM PropertyTagCopyright}

// Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  {$EXTERNALSYM PropertyTagResolutionXUnit}
  PropertyTagResolutionYUnit           = $5002;
  {$EXTERNALSYM PropertyTagResolutionYUnit}
  PropertyTagResolutionXLengthUnit     = $5003;
  {$EXTERNALSYM PropertyTagResolutionXLengthUnit}
  PropertyTagResolutionYLengthUnit     = $5004;
  {$EXTERNALSYM PropertyTagResolutionYLengthUnit}
  PropertyTagPrintFlags                = $5005;
  {$EXTERNALSYM PropertyTagPrintFlags}
  PropertyTagPrintFlagsVersion         = $5006;
  {$EXTERNALSYM PropertyTagPrintFlagsVersion}
  PropertyTagPrintFlagsCrop            = $5007;
  {$EXTERNALSYM PropertyTagPrintFlagsCrop}
  PropertyTagPrintFlagsBleedWidth      = $5008;
  {$EXTERNALSYM PropertyTagPrintFlagsBleedWidth}
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  {$EXTERNALSYM PropertyTagPrintFlagsBleedWidthScale}
  PropertyTagHalftoneLPI               = $500A;
  {$EXTERNALSYM PropertyTagHalftoneLPI}
  PropertyTagHalftoneLPIUnit           = $500B;
  {$EXTERNALSYM PropertyTagHalftoneLPIUnit}
  PropertyTagHalftoneDegree            = $500C;
  {$EXTERNALSYM PropertyTagHalftoneDegree}
  PropertyTagHalftoneShape             = $500D;
  {$EXTERNALSYM PropertyTagHalftoneShape}
  PropertyTagHalftoneMisc              = $500E;
  {$EXTERNALSYM PropertyTagHalftoneMisc}
  PropertyTagHalftoneScreen            = $500F;
  {$EXTERNALSYM PropertyTagHalftoneScreen}
  PropertyTagJPEGQuality               = $5010;
  {$EXTERNALSYM PropertyTagJPEGQuality}
  PropertyTagGridSize                  = $5011;
  {$EXTERNALSYM PropertyTagGridSize}
  PropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  {$EXTERNALSYM PropertyTagThumbnailFormat}
  PropertyTagThumbnailWidth            = $5013;
  {$EXTERNALSYM PropertyTagThumbnailWidth}
  PropertyTagThumbnailHeight           = $5014;
  {$EXTERNALSYM PropertyTagThumbnailHeight}
  PropertyTagThumbnailColorDepth       = $5015;
  {$EXTERNALSYM PropertyTagThumbnailColorDepth}
  PropertyTagThumbnailPlanes           = $5016;
  {$EXTERNALSYM PropertyTagThumbnailPlanes}
  PropertyTagThumbnailRawBytes         = $5017;
  {$EXTERNALSYM PropertyTagThumbnailRawBytes}
  PropertyTagThumbnailSize             = $5018;
  {$EXTERNALSYM PropertyTagThumbnailSize}
  PropertyTagThumbnailCompressedSize   = $5019;
  {$EXTERNALSYM PropertyTagThumbnailCompressedSize}
  PropertyTagColorTransferFunction     = $501A;
  {$EXTERNALSYM PropertyTagColorTransferFunction}
  PropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
  {$EXTERNALSYM PropertyTagThumbnailData}
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  {$EXTERNALSYM PropertyTagThumbnailImageWidth}
  PropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  {$EXTERNALSYM PropertyTagThumbnailImageHeight}
  PropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
  {$EXTERNALSYM PropertyTagThumbnailBitsPerSample}
                                                   // component
  PropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  {$EXTERNALSYM PropertyTagThumbnailCompression}
  PropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  {$EXTERNALSYM PropertyTagThumbnailPhotometricInterp}
  PropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  {$EXTERNALSYM PropertyTagThumbnailImageDescription}
  PropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
  {$EXTERNALSYM PropertyTagThumbnailEquipMake}
                                                   // Input equipment
  PropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
  {$EXTERNALSYM PropertyTagThumbnailEquipModel}
                                                   // equipment
  PropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  {$EXTERNALSYM PropertyTagThumbnailStripOffsets}
  PropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  {$EXTERNALSYM PropertyTagThumbnailOrientation}
  PropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  {$EXTERNALSYM PropertyTagThumbnailSamplesPerPixel}
  PropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  {$EXTERNALSYM PropertyTagThumbnailRowsPerStrip}
  PropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
  {$EXTERNALSYM PropertyTagThumbnailStripBytesCount}
                                                // strip
  PropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
  {$EXTERNALSYM PropertyTagThumbnailResolutionX}
                                                // direction
  PropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
  {$EXTERNALSYM PropertyTagThumbnailResolutionY}
                                                // direction
  PropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  {$EXTERNALSYM PropertyTagThumbnailPlanarConfig}
  PropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
  {$EXTERNALSYM PropertyTagThumbnailResolutionUnit}
                                                // Resolution
  PropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  {$EXTERNALSYM PropertyTagThumbnailTransferFunction}
  PropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  {$EXTERNALSYM PropertyTagThumbnailSoftwareUsed}
  PropertyTagThumbnailDateTime         = $5033;  // File change date and
  {$EXTERNALSYM PropertyTagThumbnailDateTime}
                                                 // time
  PropertyTagThumbnailArtist          = $5034;  // Person who created the
  {$EXTERNALSYM PropertyTagThumbnailArtist}
                                                // image
  PropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  {$EXTERNALSYM PropertyTagThumbnailWhitePoint}
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  {$EXTERNALSYM PropertyTagThumbnailPrimaryChromaticities}
                                                    // Chromaticities of
                                                    // primaries
  PropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
  {$EXTERNALSYM PropertyTagThumbnailYCbCrCoefficients}
                                                 // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
  {$EXTERNALSYM PropertyTagThumbnailYCbCrSubsampling}
                                                 // to C
  PropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  {$EXTERNALSYM PropertyTagThumbnailYCbCrPositioning}
  PropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
  {$EXTERNALSYM PropertyTagThumbnailRefBlackWhite}
                                                 // reference values
  PropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder
  {$EXTERNALSYM PropertyTagThumbnailCopyRight}

  PropertyTagLuminanceTable           = $5090;
  {$EXTERNALSYM PropertyTagLuminanceTable}
  PropertyTagChrominanceTable         = $5091;
  {$EXTERNALSYM PropertyTagChrominanceTable}

  PropertyTagFrameDelay               = $5100;
  {$EXTERNALSYM PropertyTagFrameDelay}
  PropertyTagLoopCount                = $5101;
  {$EXTERNALSYM PropertyTagLoopCount}

  PropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  {$EXTERNALSYM PropertyTagPixelUnit}
  PropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  {$EXTERNALSYM PropertyTagPixelPerUnitX}
  PropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  {$EXTERNALSYM PropertyTagPixelPerUnitY}
  PropertyTagPaletteHistogram  = $5113;  // Palette histogram
  {$EXTERNALSYM PropertyTagPaletteHistogram}

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  {$EXTERNALSYM PropertyTagExifExposureTime}
  PropertyTagExifFNumber       = $829D;
  {$EXTERNALSYM PropertyTagExifFNumber}

  PropertyTagExifExposureProg  = $8822;
  {$EXTERNALSYM PropertyTagExifExposureProg}
  PropertyTagExifSpectralSense = $8824;
  {$EXTERNALSYM PropertyTagExifSpectralSense}
  PropertyTagExifISOSpeed      = $8827;
  {$EXTERNALSYM PropertyTagExifISOSpeed}
  PropertyTagExifOECF          = $8828;
  {$EXTERNALSYM PropertyTagExifOECF}

  PropertyTagExifVer           = $9000;
  {$EXTERNALSYM PropertyTagExifVer}
  PropertyTagExifDTOrig        = $9003; // Date & time of original
  {$EXTERNALSYM PropertyTagExifDTOrig}
  PropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation
  {$EXTERNALSYM PropertyTagExifDTDigitized}

  PropertyTagExifCompConfig    = $9101;
  {$EXTERNALSYM PropertyTagExifCompConfig}
  PropertyTagExifCompBPP       = $9102;
  {$EXTERNALSYM PropertyTagExifCompBPP}

  PropertyTagExifShutterSpeed  = $9201;
  {$EXTERNALSYM PropertyTagExifShutterSpeed}
  PropertyTagExifAperture      = $9202;
  {$EXTERNALSYM PropertyTagExifAperture}
  PropertyTagExifBrightness    = $9203;
  {$EXTERNALSYM PropertyTagExifBrightness}
  PropertyTagExifExposureBias  = $9204;
  {$EXTERNALSYM PropertyTagExifExposureBias}
  PropertyTagExifMaxAperture   = $9205;
  {$EXTERNALSYM PropertyTagExifMaxAperture}
  PropertyTagExifSubjectDist   = $9206;
  {$EXTERNALSYM PropertyTagExifSubjectDist}
  PropertyTagExifMeteringMode  = $9207;
  {$EXTERNALSYM PropertyTagExifMeteringMode}
  PropertyTagExifLightSource   = $9208;
  {$EXTERNALSYM PropertyTagExifLightSource}
  PropertyTagExifFlash         = $9209;
  {$EXTERNALSYM PropertyTagExifFlash}
  PropertyTagExifFocalLength   = $920A;
  {$EXTERNALSYM PropertyTagExifFocalLength}
  PropertyTagExifMakerNote     = $927C;
  {$EXTERNALSYM PropertyTagExifMakerNote}
  PropertyTagExifUserComment   = $9286;
  {$EXTERNALSYM PropertyTagExifUserComment}
  PropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  {$EXTERNALSYM PropertyTagExifDTSubsec}
  PropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  {$EXTERNALSYM PropertyTagExifDTOrigSS}
  PropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds
  {$EXTERNALSYM PropertyTagExifDTDigSS}

  PropertyTagExifFPXVer        = $A000;
  {$EXTERNALSYM PropertyTagExifFPXVer}
  PropertyTagExifColorSpace    = $A001;
  {$EXTERNALSYM PropertyTagExifColorSpace}
  PropertyTagExifPixXDim       = $A002;
  {$EXTERNALSYM PropertyTagExifPixXDim}
  PropertyTagExifPixYDim       = $A003;
  {$EXTERNALSYM PropertyTagExifPixYDim}
  PropertyTagExifRelatedWav    = $A004;  // related sound file
  {$EXTERNALSYM PropertyTagExifRelatedWav}
  PropertyTagExifInterop       = $A005;
  {$EXTERNALSYM PropertyTagExifInterop}
  PropertyTagExifFlashEnergy   = $A20B;
  {$EXTERNALSYM PropertyTagExifFlashEnergy}
  PropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  {$EXTERNALSYM PropertyTagExifSpatialFR}
  PropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  {$EXTERNALSYM PropertyTagExifFocalXRes}
  PropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  {$EXTERNALSYM PropertyTagExifFocalYRes}
  PropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  {$EXTERNALSYM PropertyTagExifFocalResUnit}
  PropertyTagExifSubjectLoc    = $A214;
  {$EXTERNALSYM PropertyTagExifSubjectLoc}
  PropertyTagExifExposureIndex = $A215;
  {$EXTERNALSYM PropertyTagExifExposureIndex}
  PropertyTagExifSensingMethod = $A217;
  {$EXTERNALSYM PropertyTagExifSensingMethod}
  PropertyTagExifFileSource    = $A300;
  {$EXTERNALSYM PropertyTagExifFileSource}
  PropertyTagExifSceneType     = $A301;
  {$EXTERNALSYM PropertyTagExifSceneType}
  PropertyTagExifCfaPattern    = $A302;
  {$EXTERNALSYM PropertyTagExifCfaPattern}

  PropertyTagGpsVer            = $0000;
  {$EXTERNALSYM PropertyTagGpsVer}
  PropertyTagGpsLatitudeRef    = $0001;
  {$EXTERNALSYM PropertyTagGpsLatitudeRef}
  PropertyTagGpsLatitude       = $0002;
  {$EXTERNALSYM PropertyTagGpsLatitude}
  PropertyTagGpsLongitudeRef   = $0003;
  {$EXTERNALSYM PropertyTagGpsLongitudeRef}
  PropertyTagGpsLongitude      = $0004;
  {$EXTERNALSYM PropertyTagGpsLongitude}
  PropertyTagGpsAltitudeRef    = $0005;
  {$EXTERNALSYM PropertyTagGpsAltitudeRef}
  PropertyTagGpsAltitude       = $0006;
  {$EXTERNALSYM PropertyTagGpsAltitude}
  PropertyTagGpsGpsTime        = $0007;
  {$EXTERNALSYM PropertyTagGpsGpsTime}
  PropertyTagGpsGpsSatellites  = $0008;
  {$EXTERNALSYM PropertyTagGpsGpsSatellites}
  PropertyTagGpsGpsStatus      = $0009;
  {$EXTERNALSYM PropertyTagGpsGpsStatus}
  PropertyTagGpsGpsMeasureMode = $00A;
  {$EXTERNALSYM PropertyTagGpsGpsMeasureMode}
  PropertyTagGpsGpsDop         = $000B;  // Measurement precision
  {$EXTERNALSYM PropertyTagGpsGpsDop}
  PropertyTagGpsSpeedRef       = $000C;
  {$EXTERNALSYM PropertyTagGpsSpeedRef}
  PropertyTagGpsSpeed          = $000D;
  {$EXTERNALSYM PropertyTagGpsSpeed}
  PropertyTagGpsTrackRef       = $000E;
  {$EXTERNALSYM PropertyTagGpsTrackRef}
  PropertyTagGpsTrack          = $000F;
  {$EXTERNALSYM PropertyTagGpsTrack}
  PropertyTagGpsImgDirRef      = $0010;
  {$EXTERNALSYM PropertyTagGpsImgDirRef}
  PropertyTagGpsImgDir         = $0011;
  {$EXTERNALSYM PropertyTagGpsImgDir}
  PropertyTagGpsMapDatum       = $0012;
  {$EXTERNALSYM PropertyTagGpsMapDatum}
  PropertyTagGpsDestLatRef     = $0013;
  {$EXTERNALSYM PropertyTagGpsDestLatRef}
  PropertyTagGpsDestLat        = $0014;
  {$EXTERNALSYM PropertyTagGpsDestLat}
  PropertyTagGpsDestLongRef    = $0015;
  {$EXTERNALSYM PropertyTagGpsDestLongRef}
  PropertyTagGpsDestLong       = $0016;
  {$EXTERNALSYM PropertyTagGpsDestLong}
  PropertyTagGpsDestBearRef    = $0017;
  {$EXTERNALSYM PropertyTagGpsDestBearRef}
  PropertyTagGpsDestBear       = $0018;
  {$EXTERNALSYM PropertyTagGpsDestBear}
  PropertyTagGpsDestDistRef    = $0019;
  {$EXTERNALSYM PropertyTagGpsDestDistRef}
  PropertyTagGpsDestDist       = $001A;
  {$EXTERNALSYM PropertyTagGpsDestDist}

(**************************************************************************\
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  {$EXTERNALSYM ColorMatrix}
  ColorMatrix = packed array[0..4, 0..4] of Single;
  TColorMatrix = ColorMatrix;
  PColorMatrix = ^TColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMatrixFlags}
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );
  TColorMatrixFlags = ColorMatrixFlags;

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorAdjustType}
  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );
  TColorAdjustType = ColorAdjustType;

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMap}
  ColorMap = packed record
    oldColor: TGPColor;
    newColor: TGPColor;
  end;
  TColorMap = ColorMap;
  PColorMap = ^TColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics = Pointer;

  GpBrush = Pointer;
  GpTexture = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpPathGradient = Pointer;
  GpHatch =  Pointer;

  GpPen = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;

  GpImage = Pointer;
  GpBitmap = Pointer;
  GpMetafile = Pointer;
  GpImageAttributes = Pointer;

  GpPath = Pointer;
  GpRegion = Pointer;
  GpPathIterator = Pointer;

  GpFontFamily = Pointer;
  GpFont = Pointer;
  GpStringFormat = Pointer;
  GpFontCollection = Pointer;
  GpCachedBitmap = Pointer;

  GpStatus          = TStatus;
  GpFillMode        = TFillMode;
  GpWrapMode        = TWrapMode;
  GpUnit            = TUnit;
  GpCoordinateSpace = TCoordinateSpace;
  GpPointF          = PGPPointF;
  GpPoint           = PGPPoint;
  GpRectF           = PGPRectF;
  GpRect            = PGPRect;
  GpSizeF           = PGPSizeF;
  GpHatchStyle      = THatchStyle;
  GpDashStyle       = TDashStyle;
  GpLineCap         = TLineCap;
  GpDashCap         = TDashCap;

  GpPenAlignment    = TPenAlignment;

  GpLineJoin        = TLineJoin;
  GpPenType         = TPenType;

  GpMatrix          = Pointer; 
  GpBrushType       = TBrushType;
  GpMatrixOrder     = TMatrixOrder;
  GpFlushIntention  = TFlushIntention;
  GpPathData        = TPathData;

(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   GdiplusFlat.h
* Abstract:
*   Private GDI+ header file.
*
\**************************************************************************)

  var GdipCreatePath: function(brushMode: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePath}

  var GdipCreatePath2: function(v1: GPPOINTF; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePath2}

  var GdipCreatePath2I: function(v1: GPPOINT; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePath2I}

  var GdipClonePath: function(path: GPPATH;
    out clonePath: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipClonePath}

  var GdipDeletePath: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeletePath}

  var GdipResetPath: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetPath}

  var GdipGetPointCount: function(path: GPPATH;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPointCount}

  var GdipGetPathTypes: function(path: GPPATH; types: PBYTE;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathTypes}

  var GdipGetPathPoints: function(v1: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathPoints}

  var GdipGetPathPointsI: function(v1: GPPATH; points: GPPOINT;
             count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathPointsI}

  var GdipGetPathFillMode: function(path: GPPATH;
    var fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathFillMode}

  var GdipSetPathFillMode: function(path: GPPATH;
    fillmode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathFillMode}

  var GdipGetPathData: function(path: GPPATH;
    pathData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathData}

  var GdipStartPathFigure: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipStartPathFigure}

  var GdipClosePathFigure: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipClosePathFigure}

  var GdipClosePathFigures: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipClosePathFigures}

  var GdipSetPathMarker: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathMarker}

  var GdipClearPathMarkers: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipClearPathMarkers}

  var GdipReversePath: function(path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipReversePath}

  var GdipGetPathLastPoint: function(path: GPPATH;
    lastPoint: GPPOINTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathLastPoint}

  var GdipAddPathLine: function(path: GPPATH;
    x1, y1, x2, y2: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathLine}

  var GdipAddPathLine2: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathLine2}

  var GdipAddPathArc: function(path: GPPATH; x, y, width, height, startAngle,
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathArc}

  var GdipAddPathBezier: function(path: GPPATH;
    x1, y1, x2, y2, x3, y3, x4, y4: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathBezier}

  var GdipAddPathBeziers: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathBeziers}

  var GdipAddPathCurve: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurve}

  var GdipAddPathCurve2: function(path: GPPATH; points: GPPOINTF; count: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurve2}

  var GdipAddPathCurve3: function(path: GPPATH; points: GPPOINTF; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurve3}

  var GdipAddPathClosedCurve: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathClosedCurve}

  var GdipAddPathClosedCurve2: function(path: GPPATH; points: GPPOINTF;
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathClosedCurve2}

  var GdipAddPathRectangle: function(path: GPPATH; x: Single; y: Single;
    width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathRectangle}

  var GdipAddPathRectangles: function(path: GPPATH; rects: GPRECTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathRectangles}

  var GdipAddPathEllipse: function(path: GPPATH;  x: Single; y: Single;
    width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathEllipse}

  var GdipAddPathPie: function(path: GPPATH; x: Single; y: Single; width: Single;
    height: Single; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathPie}

  var GdipAddPathPolygon: function(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathPolygon}

  var GdipAddPathPath: function(path: GPPATH; addingPath: GPPATH;
    connect: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathPath}

  var GdipAddPathString: function(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRectF;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathString}

  var GdipAddPathStringI: function(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRect;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathStringI}

  var GdipAddPathLineI: function(path: GPPATH; x1: Integer; y1: Integer; x2: Integer;
    y2: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathLineI}

  var GdipAddPathLine2I: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathLine2I}

  var GdipAddPathArcI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathArcI}

  var GdipAddPathBezierI: function(path: GPPATH; x1: Integer; y1: Integer;
    x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer;
    y4: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathBezierI}

  var GdipAddPathBeziersI: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathBeziersI}

  var GdipAddPathCurveI: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurveI}

  var GdipAddPathCurve2I: function(path: GPPATH; points: GPPOINT; count: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurve2I}

  var GdipAddPathCurve3I: function(path: GPPATH; points: GPPOINT; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathCurve3I}

  var GdipAddPathClosedCurveI: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathClosedCurveI}

  var GdipAddPathClosedCurve2I: function(path: GPPATH; points: GPPOINT;
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathClosedCurve2I}

  var GdipAddPathRectangleI: function(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathRectangleI}

  var GdipAddPathRectanglesI: function(path: GPPATH; rects: GPRECT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathRectanglesI}

  var GdipAddPathEllipseI: function(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathEllipseI}

  var GdipAddPathPieI: function(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathPieI}

  var GdipAddPathPolygonI: function(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipAddPathPolygonI}

  var GdipFlattenPath: function(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFlattenPath}

  var GdipWindingModeOutline: function(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipWindingModeOutline}

  var GdipWidenPath: function(nativePath: GPPATH; pen: GPPEN; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipWidenPath}

  var GdipWarpPath: function(path: GPPATH; matrix: GPMATRIX; points: GPPOINTF;
    count: Integer; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; warpMode: WARPMODE; flatness: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipWarpPath}

  var GdipTransformPath: function(path: GPPATH; matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformPath}

  var GdipGetPathWorldBounds: function(path: GPPATH; bounds: GPRECTF;
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathWorldBounds}

  var GdipGetPathWorldBoundsI: function(path: GPPATH; bounds: GPRECT;
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathWorldBoundsI}

  var GdipIsVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisiblePathPoint}

  var GdipIsVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisiblePathPointI}

  var GdipIsOutlineVisiblePathPoint: function(path: GPPATH; x: Single; y: Single;
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsOutlineVisiblePathPoint}

  var GdipIsOutlineVisiblePathPointI: function(path: GPPATH; x: Integer; y: Integer;
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsOutlineVisiblePathPointI}

//----------------------------------------------------------------------------
// PathIterator APIs 
//----------------------------------------------------------------------------

  var GdipCreatePathIter: function(out iterator: GPPATHITERATOR;
    path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePathIter}

  var GdipDeletePathIter: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeletePathIter}

  var GdipPathIterNextSubpath: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; var startIndex: Integer; var endIndex: Integer;
    out isClosed: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterNextSubpath}

  var GdipPathIterNextSubpathPath: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; path: GPPATH;
    out isClosed: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterNextSubpathPath}

  var GdipPathIterNextPathType: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; pathType: PBYTE; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterNextPathType}

  var GdipPathIterNextMarker: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterNextMarker}

  var GdipPathIterNextMarkerPath: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterNextMarkerPath}

  var GdipPathIterGetCount: function(iterator: GPPATHITERATOR;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterGetCount}

  var GdipPathIterGetSubpathCount: function(iterator: GPPATHITERATOR;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterGetSubpathCount}

  var GdipPathIterIsValid: function(iterator: GPPATHITERATOR;
    out valid: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterIsValid}

  var GdipPathIterHasCurve: function(iterator: GPPATHITERATOR;
    out hasCurve: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterHasCurve}

  var GdipPathIterRewind: function(iterator: GPPATHITERATOR): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterRewind}

  var GdipPathIterEnumerate: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterEnumerate}

  var GdipPathIterCopyData: function(iterator: GPPATHITERATOR;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    startIndex: Integer; endIndex: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPathIterCopyData}

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

  var GdipCreateMatrix: function(out matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMatrix}

  var GdipCreateMatrix2: function(m11: Single; m12: Single; m21: Single; m22: Single;
    dx: Single; dy: Single; out matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMatrix2}

  var GdipCreateMatrix3: function(rect: GPRECTF; dstplg: GPPOINTF;
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMatrix3}

  var GdipCreateMatrix3I: function(rect: GPRECT; dstplg: GPPOINT;
    out matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMatrix3I}

  var GdipCloneMatrix: function(matrix: GPMATRIX;
    out cloneMatrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneMatrix}

  var GdipDeleteMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteMatrix}

  var GdipSetMatrixElements: function(matrix: GPMATRIX; m11: Single; m12: Single;
    m21: Single; m22: Single; dx: Single; dy: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetMatrixElements}

  var GdipMultiplyMatrix: function(matrix: GPMATRIX; matrix2: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyMatrix}

  var GdipTranslateMatrix: function(matrix: GPMATRIX; offsetX: Single;
    offsetY: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateMatrix}

  var GdipScaleMatrix: function(matrix: GPMATRIX; scaleX: Single; scaleY: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScaleMatrix}

  var GdipRotateMatrix: function(matrix: GPMATRIX; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotateMatrix}

  var GdipShearMatrix: function(matrix: GPMATRIX; shearX: Single; shearY: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipShearMatrix}

  var GdipInvertMatrix: function(matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipInvertMatrix}

  var GdipTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformMatrixPoints}

  var GdipTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformMatrixPointsI}

  var GdipVectorTransformMatrixPoints: function(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipVectorTransformMatrixPoints}

  var GdipVectorTransformMatrixPointsI: function(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipVectorTransformMatrixPointsI}

  var GdipGetMatrixElements: function(matrix: GPMATRIX;
    matrixOut: PSingle): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMatrixElements}

  var GdipIsMatrixInvertible: function(matrix: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsMatrixInvertible}

  var GdipIsMatrixIdentity: function(matrix: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsMatrixIdentity}

  var GdipIsMatrixEqual: function(matrix: GPMATRIX; matrix2: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsMatrixEqual}

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

  var GdipCreateRegion: function(out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegion}

  var GdipCreateRegionRect: function(rect: GPRECTF;
    out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegionRect}

  var GdipCreateRegionRectI: function(rect: GPRECT;
    out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegionRectI}

  var GdipCreateRegionPath: function(path: GPPATH;
    out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegionPath}

  var GdipCreateRegionRgnData: function(regionData: PBYTE; size: Integer;
    out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegionRgnData}

  var GdipCreateRegionHrgn: function(hRgn: HRGN;
    out region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateRegionHrgn}

  var GdipCloneRegion: function(region: GPREGION;
    out cloneRegion: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneRegion}

  var GdipDeleteRegion: function(region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteRegion}

  var GdipSetInfinite: function(region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetInfinite}

  var GdipSetEmpty: function(region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetEmpty}

  var GdipCombineRegionRect: function(region: GPREGION; rect: GPRECTF;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCombineRegionRect}

  var GdipCombineRegionRectI: function(region: GPREGION; rect: GPRECT;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCombineRegionRectI}

  var GdipCombineRegionPath: function(region: GPREGION; path: GPPATH;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCombineRegionPath}

  var GdipCombineRegionRegion: function(region: GPREGION; region2: GPREGION;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCombineRegionRegion}

  var GdipTranslateRegion: function(region: GPREGION; dx: Single;
    dy: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateRegion}

  var GdipTranslateRegionI: function(region: GPREGION; dx: Integer;
    dy: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateRegionI}

  var GdipTransformRegion: function(region: GPREGION;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformRegion}

  var GdipGetRegionBounds: function(region: GPREGION; graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionBounds}

  var GdipGetRegionBoundsI: function(region: GPREGION; graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionBoundsI}

  var GdipGetRegionHRgn: function(region: GPREGION; graphics: GPGRAPHICS;
    out hRgn: HRGN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionHRgn}

  var GdipIsEmptyRegion: function(region: GPREGION; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsEmptyRegion}

  var GdipIsInfiniteRegion: function(region: GPREGION; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsInfiniteRegion}

  var GdipIsEqualRegion: function(region: GPREGION; region2: GPREGION;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsEqualRegion}

  var GdipGetRegionDataSize: function(region: GPREGION;
    out bufferSize: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionDataSize}

  var GdipGetRegionData: function(region: GPREGION; buffer: PBYTE;
    bufferSize: UINT; sizeFilled: PUINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionData}

  var GdipIsVisibleRegionPoint: function(region: GPREGION; x: Single; y: Single;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRegionPoint}

  var GdipIsVisibleRegionPointI: function(region: GPREGION; x: Integer; y: Integer;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRegionPointI}

  var GdipIsVisibleRegionRect: function(region: GPREGION; x: Single; y: Single;
    width: Single; height: Single; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRegionRect}

  var GdipIsVisibleRegionRectI: function(region: GPREGION; x: Integer; y: Integer;
    width: Integer; height: Integer; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRegionRectI}

  var GdipGetRegionScansCount: function(region: GPREGION; out count: UINT;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionScansCount}

  var GdipGetRegionScans: function(region: GPREGION; rects: GPRECTF;
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionScans}

  var GdipGetRegionScansI: function(region: GPREGION; rects: GPRECT;
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRegionScansI}

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

  var GdipCloneBrush: function(brush: GPBRUSH;
    out cloneBrush: GPBRUSH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneBrush}

  var GdipDeleteBrush: function(brush: GPBRUSH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteBrush}

  var GdipGetBrushType: function(brush: GPBRUSH;
    out type_: GPBRUSHTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetBrushType}

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

  var GdipCreateHatchBrush: function(hatchstyle: Integer; forecol: ARGB;
    backcol: ARGB; out brush: GPHATCH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateHatchBrush}

  var GdipGetHatchStyle: function(brush: GPHATCH;
    out hatchstyle: GPHATCHSTYLE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetHatchStyle}

  var GdipGetHatchForegroundColor: function(brush: GPHATCH;
    out forecol: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetHatchForegroundColor}

  var GdipGetHatchBackgroundColor: function(brush: GPHATCH;
    out backcol: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetHatchBackgroundColor}

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------


  var GdipCreateTexture: function(image: GPIMAGE; wrapmode: GPWRAPMODE;
    var texture: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateTexture}

  var GdipCreateTexture2: function(image: GPIMAGE; wrapmode: GPWRAPMODE;
    x: Single; y: Single; width: Single; height: Single;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateTexture2}

  var GdipCreateTextureIA: function(image: GPIMAGE;
    imageAttributes: GPIMAGEATTRIBUTES; x: Single; y: Single; width: Single;
    height: Single; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateTextureIA}

  var GdipCreateTexture2I: function(image: GPIMAGE; wrapmode: GPWRAPMODE; x: Integer;
    y: Integer; width: Integer; height: Integer;
    out texture: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateTexture2I}

  var GdipCreateTextureIAI: function(image: GPIMAGE;
    imageAttributes: GPIMAGEATTRIBUTES; x: Integer; y: Integer; width: Integer;
    height: Integer; out texture: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateTextureIAI}

  var GdipGetTextureTransform: function(brush: GPTEXTURE;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetTextureTransform}

  var GdipSetTextureTransform: function(brush: GPTEXTURE;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetTextureTransform}

  var GdipResetTextureTransform: function(brush: GPTEXTURE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetTextureTransform}

  var GdipMultiplyTextureTransform: function(brush: GPTEXTURE; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyTextureTransform}

  var GdipTranslateTextureTransform: function(brush: GPTEXTURE; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateTextureTransform}

  var GdipScaleTextureTransform: function(brush: GPTEXTURE; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScaleTextureTransform}

  var GdipRotateTextureTransform: function(brush: GPTEXTURE; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotateTextureTransform}

  var GdipSetTextureWrapMode: function(brush: GPTEXTURE;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetTextureWrapMode}

  var GdipGetTextureWrapMode: function(brush: GPTEXTURE;
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetTextureWrapMode}

  var GdipGetTextureImage: function(brush: GPTEXTURE;
    out image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetTextureImage}

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

  var GdipCreateSolidFill: function(color: ARGB;
    out brush: GPSOLIDFILL): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateSolidFill}

  var GdipSetSolidFillColor: function(brush: GPSOLIDFILL;
    color: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetSolidFillColor}

  var GdipGetSolidFillColor: function(brush: GPSOLIDFILL;
    out color: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetSolidFillColor}

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

  var GdipCreateLineBrush: function(point1: GPPOINTF; point2: GPPOINTF; color1: ARGB;
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrush}

  var GdipCreateLineBrushI: function(point1: GPPOINT; point2: GPPOINT; color1: ARGB;
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrushI}

  var GdipCreateLineBrushFromRect: function(rect: GPRECTF; color1: ARGB;
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrushFromRect}

  var GdipCreateLineBrushFromRectI: function(rect: GPRECT; color1: ARGB;
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrushFromRectI}

  var GdipCreateLineBrushFromRectWithAngle: function(rect: GPRECTF; color1: ARGB;
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrushFromRectWithAngle}

  var GdipCreateLineBrushFromRectWithAngleI: function(rect: GPRECT; color1: ARGB;
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateLineBrushFromRectWithAngleI}

  var GdipSetLineColors: function(brush: GPLINEGRADIENT; color1: ARGB;
    color2: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineColors}

  var GdipGetLineColors: function(brush: GPLINEGRADIENT;
    colors: PARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineColors}

  var GdipGetLineRect: function(brush: GPLINEGRADIENT;
    rect: GPRECTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineRect}

  var GdipGetLineRectI: function(brush: GPLINEGRADIENT;
    rect: GPRECT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineRectI}

  var GdipSetLineGammaCorrection: function(brush: GPLINEGRADIENT;
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineGammaCorrection}

  var GdipGetLineGammaCorrection: function(brush: GPLINEGRADIENT;
    out useGammaCorrection: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineGammaCorrection}

  var GdipGetLineBlendCount: function(brush: GPLINEGRADIENT;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineBlendCount}

  var GdipGetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineBlend}

  var GdipSetLineBlend: function(brush: GPLINEGRADIENT; blend: PSingle;
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineBlend}

  var GdipGetLinePresetBlendCount: function(brush: GPLINEGRADIENT;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLinePresetBlendCount}

  var GdipGetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLinePresetBlend}

  var GdipSetLinePresetBlend: function(brush: GPLINEGRADIENT; blend: PARGB;
    positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLinePresetBlend}

  var GdipSetLineSigmaBlend: function(brush: GPLINEGRADIENT; focus: Single;
    scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineSigmaBlend}

  var GdipSetLineLinearBlend: function(brush: GPLINEGRADIENT; focus: Single;
    scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineLinearBlend}

  var GdipSetLineWrapMode: function(brush: GPLINEGRADIENT;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineWrapMode}

  var GdipGetLineWrapMode: function(brush: GPLINEGRADIENT;
    out wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineWrapMode}

  var GdipGetLineTransform: function(brush: GPLINEGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineTransform}

  var GdipSetLineTransform: function(brush: GPLINEGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetLineTransform}

  var GdipResetLineTransform: function(brush: GPLINEGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetLineTransform}

  var GdipMultiplyLineTransform: function(brush: GPLINEGRADIENT; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyLineTransform}

  var GdipTranslateLineTransform: function(brush: GPLINEGRADIENT; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateLineTransform}

  var GdipScaleLineTransform: function(brush: GPLINEGRADIENT; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScaleLineTransform}

  var GdipRotateLineTransform: function(brush: GPLINEGRADIENT; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotateLineTransform}

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

  var GdipCreatePathGradient: function(points: GPPOINTF; count: Integer;
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePathGradient}

  var GdipCreatePathGradientI: function(points: GPPOINT; count: Integer;
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePathGradientI}

  var GdipCreatePathGradientFromPath: function(path: GPPATH;
    out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePathGradientFromPath}

  var GdipGetPathGradientCenterColor: function(brush: GPPATHGRADIENT;
    out colors: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientCenterColor}

  var GdipSetPathGradientCenterColor: function(brush: GPPATHGRADIENT;
    colors: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientCenterColor}

  var GdipGetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientSurroundColorsWithCount}

  var GdipSetPathGradientSurroundColorsWithCount: function(brush: GPPATHGRADIENT;
    color: PARGB; var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientSurroundColorsWithCount}

  var GdipGetPathGradientPath: function(brush: GPPATHGRADIENT;
    path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientPath}

  var GdipSetPathGradientPath: function(brush: GPPATHGRADIENT;
    path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientPath}

  var GdipGetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;
    points: GPPOINTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientCenterPoint}

  var GdipGetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;
    points: GPPOINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientCenterPointI}

  var GdipSetPathGradientCenterPoint: function(brush: GPPATHGRADIENT;
    points: GPPOINTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientCenterPoint}

  var GdipSetPathGradientCenterPointI: function(brush: GPPATHGRADIENT;
    points: GPPOINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientCenterPointI}

  var GdipGetPathGradientRect: function(brush: GPPATHGRADIENT;
    rect: GPRECTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientRect}

  var GdipGetPathGradientRectI: function(brush: GPPATHGRADIENT;
    rect: GPRECT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientRectI}

  var GdipGetPathGradientPointCount: function(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientPointCount}

  var GdipGetPathGradientSurroundColorCount: function(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientSurroundColorCount}

  var GdipSetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;
    useGammaCorrection: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientGammaCorrection}

  var GdipGetPathGradientGammaCorrection: function(brush: GPPATHGRADIENT;
    var useGammaCorrection: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientGammaCorrection}

  var GdipGetPathGradientBlendCount: function(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientBlendCount}

  var GdipGetPathGradientBlend: function(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientBlend}

  var GdipSetPathGradientBlend: function(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientBlend}

  var GdipGetPathGradientPresetBlendCount: function(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientPresetBlendCount}

  var GdipGetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientPresetBlend}

  var GdipSetPathGradientPresetBlend: function(brush: GPPATHGRADIENT;
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientPresetBlend}

  var GdipSetPathGradientSigmaBlend: function(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientSigmaBlend}

  var GdipSetPathGradientLinearBlend: function(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientLinearBlend}

  var GdipGetPathGradientWrapMode: function(brush: GPPATHGRADIENT;
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientWrapMode}

  var GdipSetPathGradientWrapMode: function(brush: GPPATHGRADIENT;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientWrapMode}

  var GdipGetPathGradientTransform: function(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientTransform}

  var GdipSetPathGradientTransform: function(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientTransform}

  var GdipResetPathGradientTransform: function(
    brush: GPPATHGRADIENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetPathGradientTransform}

  var GdipMultiplyPathGradientTransform: function(brush: GPPATHGRADIENT;
    matrix: GPMATRIX; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyPathGradientTransform}

  var GdipTranslatePathGradientTransform: function(brush: GPPATHGRADIENT;
    dx: Single; dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslatePathGradientTransform}

  var GdipScalePathGradientTransform: function(brush: GPPATHGRADIENT;
    sx: Single; sy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScalePathGradientTransform}

  var GdipRotatePathGradientTransform: function(brush: GPPATHGRADIENT;
    angle: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotatePathGradientTransform}

  var GdipGetPathGradientFocusScales: function(brush: GPPATHGRADIENT;
    var xScale: Single; var yScale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPathGradientFocusScales}

  var GdipSetPathGradientFocusScales: function(brush: GPPATHGRADIENT;
    xScale: Single; yScale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPathGradientFocusScales}

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

  var GdipCreatePen1: function(color: ARGB; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePen1}

  var GdipCreatePen2: function(brush: GPBRUSH; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreatePen2}

  var GdipClonePen: function(pen: GPPEN; out clonepen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipClonePen}

  var GdipDeletePen: function(pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeletePen}

  var GdipSetPenWidth: function(pen: GPPEN; width: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenWidth}

  var GdipGetPenWidth: function(pen: GPPEN; out width: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenWidth}

  var GdipSetPenUnit: function(pen: GPPEN; unit_: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenUnit}

  var GdipGetPenUnit: function(pen: GPPEN; var unit_: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenUnit}

  var GdipSetPenLineCap197819: function(pen: GPPEN; startCap: GPLINECAP;
    endCap: GPLINECAP; dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenLineCap197819}

  var GdipSetPenStartCap: function(pen: GPPEN;
    startCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenStartCap}

  var GdipSetPenEndCap: function(pen: GPPEN; endCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenEndCap}

  var GdipSetPenDashCap197819: function(pen: GPPEN;
    dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenDashCap197819}

  var GdipGetPenStartCap: function(pen: GPPEN;
    out startCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenStartCap}

  var GdipGetPenEndCap: function(pen: GPPEN;
    out endCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenEndCap}

  var GdipGetPenDashCap197819: function(pen: GPPEN;
    out dashCap: GPDASHCAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenDashCap197819}

  var GdipSetPenLineJoin: function(pen: GPPEN;
    lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenLineJoin}

  var GdipGetPenLineJoin: function(pen: GPPEN;
    var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenLineJoin}

  var GdipSetPenCustomStartCap: function(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenCustomStartCap}

  var GdipGetPenCustomStartCap: function(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenCustomStartCap}

  var GdipSetPenCustomEndCap: function(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenCustomEndCap}

  var GdipGetPenCustomEndCap: function(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenCustomEndCap}

  var GdipSetPenMiterLimit: function(pen: GPPEN;
    miterLimit: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenMiterLimit}

  var GdipGetPenMiterLimit: function(pen: GPPEN;
    out miterLimit: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenMiterLimit}

  var GdipSetPenMode: function(pen: GPPEN;
    penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenMode}

  var GdipGetPenMode: function(pen: GPPEN;
    var penMode: GPPENALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenMode}

  var GdipSetPenTransform: function(pen: GPPEN;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenTransform}

  var GdipGetPenTransform: function(pen: GPPEN;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenTransform}

  var GdipResetPenTransform: function(pen: GPPEN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetPenTransform}

  var GdipMultiplyPenTransform: function(pen: GPPEN; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyPenTransform}

  var GdipTranslatePenTransform: function(pen: GPPEN; dx: Single; dy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslatePenTransform}

  var GdipScalePenTransform: function(pen: GPPEN; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScalePenTransform}

  var GdipRotatePenTransform: function(pen: GPPEN; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotatePenTransform}

  var GdipSetPenColor: function(pen: GPPEN; argb: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenColor}

  var GdipGetPenColor: function(pen: GPPEN; out argb: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenColor}

  var GdipSetPenBrushFill: function(pen: GPPEN; brush: GPBRUSH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenBrushFill}

  var GdipGetPenBrushFill: function(pen: GPPEN;
    out brush: GPBRUSH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenBrushFill}

  var GdipGetPenFillType: function(pen: GPPEN;
    out type_: GPPENTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenFillType}

  var GdipGetPenDashStyle: function(pen: GPPEN;
    out dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenDashStyle}

  var GdipSetPenDashStyle: function(pen: GPPEN;
    dashstyle: GPDASHSTYLE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenDashStyle}

  var GdipGetPenDashOffset: function(pen: GPPEN;
    out offset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenDashOffset}

  var GdipSetPenDashOffset: function(pen: GPPEN; offset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenDashOffset}

  var GdipGetPenDashCount: function(pen: GPPEN;
    var count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenDashCount}

  var GdipSetPenDashArray: function(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenDashArray}

  var GdipGetPenDashArray: function(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenDashArray}

  var GdipGetPenCompoundCount: function(pen: GPPEN;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenCompoundCount}

  var GdipSetPenCompoundArray: function(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPenCompoundArray}

  var GdipGetPenCompoundArray: function(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPenCompoundArray}

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

  var GdipCreateCustomLineCap: function(fillPath: GPPATH; strokePath: GPPATH;
    baseCap: GPLINECAP; baseInset: Single;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateCustomLineCap}

  var GdipDeleteCustomLineCap: function(
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteCustomLineCap}

  var GdipCloneCustomLineCap: function(customCap: GPCUSTOMLINECAP;
    out clonedCap: GPCUSTOMLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneCustomLineCap}

  var GdipGetCustomLineCapType: function(customCap: GPCUSTOMLINECAP;
    var capType: CUSTOMLINECAPTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapType}

  var GdipSetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;
    startCap: GPLINECAP; endCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCustomLineCapStrokeCaps}

  var GdipGetCustomLineCapStrokeCaps: function(customCap: GPCUSTOMLINECAP;
    var startCap: GPLINECAP; var endCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapStrokeCaps}

  var GdipSetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;
  lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCustomLineCapStrokeJoin}

  var GdipGetCustomLineCapStrokeJoin: function(customCap: GPCUSTOMLINECAP;
  var lineJoin: GPLINEJOIN): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapStrokeJoin}

  var GdipSetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;
  baseCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCustomLineCapBaseCap}

  var GdipGetCustomLineCapBaseCap: function(customCap: GPCUSTOMLINECAP;
  var baseCap: GPLINECAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapBaseCap}

  var GdipSetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;
  inset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCustomLineCapBaseInset}

  var GdipGetCustomLineCapBaseInset: function(customCap: GPCUSTOMLINECAP;
  var inset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapBaseInset}

  var GdipSetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;
  widthScale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCustomLineCapWidthScale}

  var GdipGetCustomLineCapWidthScale: function(customCap: GPCUSTOMLINECAP;
  var widthScale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCustomLineCapWidthScale}

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

  var GdipCreateAdjustableArrowCap: function(height: Single;
  width: Single;
  isFilled: Bool;
  out cap: GPADJUSTABLEARROWCAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateAdjustableArrowCap}

  var GdipSetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;
  height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetAdjustableArrowCapHeight}

  var GdipGetAdjustableArrowCapHeight: function(cap: GPADJUSTABLEARROWCAP;
  var height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetAdjustableArrowCapHeight}

  var GdipSetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;
  width: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetAdjustableArrowCapWidth}

  var GdipGetAdjustableArrowCapWidth: function(cap: GPADJUSTABLEARROWCAP;
  var width: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetAdjustableArrowCapWidth}

  var GdipSetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;
  middleInset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetAdjustableArrowCapMiddleInset}

  var GdipGetAdjustableArrowCapMiddleInset: function(cap: GPADJUSTABLEARROWCAP;
  var middleInset: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetAdjustableArrowCapMiddleInset}

  var GdipSetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;
  fillState: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetAdjustableArrowCapFillState}

  var GdipGetAdjustableArrowCapFillState: function(cap: GPADJUSTABLEARROWCAP;
  var fillState: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetAdjustableArrowCapFillState}

//---------------------------------------------------------------------------- 
// Image APIs
//----------------------------------------------------------------------------

  var GdipLoadImageFromStream: function(stream: ISTREAM;
  out image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipLoadImageFromStream}

  var GdipLoadImageFromFile: function(filename: PWCHAR;
  out image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipLoadImageFromFile}

  var GdipLoadImageFromStreamICM: function(stream: ISTREAM;
  out image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipLoadImageFromStreamICM}

  var GdipLoadImageFromFileICM: function(filename: PWCHAR;
  out image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipLoadImageFromFileICM}

  var GdipCloneImage: function(image: GPIMAGE;
  out cloneImage: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneImage}

  var GdipDisposeImage: function(image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDisposeImage}

  var GdipSaveImageToFile: function(image: GPIMAGE;
  filename: PWCHAR;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSaveImageToFile}

  var GdipSaveImageToStream: function(image: GPIMAGE;
  stream: ISTREAM;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSaveImageToStream}

  var GdipSaveAdd: function(image: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSaveAdd}

  var GdipSaveAddImage: function(image: GPIMAGE;
  newImage: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSaveAddImage}

  var GdipGetImageGraphicsContext: function(image: GPIMAGE;
  out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageGraphicsContext}

  var GdipGetImageBounds: function(image: GPIMAGE;
  srcRect: GPRECTF;
  var srcUnit: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageBounds}

  var GdipGetImageDimension: function(image: GPIMAGE;
  var width: Single;
  var height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageDimension}

  var GdipGetImageType: function(image: GPIMAGE;
  var type_: IMAGETYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageType}

  var GdipGetImageWidth: function(image: GPIMAGE;
  var width: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageWidth}

  var GdipGetImageHeight: function(image: GPIMAGE;
  var height: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageHeight}

  var GdipGetImageHorizontalResolution: function(image: GPIMAGE;
  var resolution: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageHorizontalResolution}

  var GdipGetImageVerticalResolution: function(image: GPIMAGE;
  var resolution: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageVerticalResolution}

  var GdipGetImageFlags: function(image: GPIMAGE;
  var flags: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageFlags}

  var GdipGetImageRawFormat: function(image: GPIMAGE;
  format: PGUID): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageRawFormat}

  var GdipGetImagePixelFormat: function(image: GPIMAGE;
  out format: TPIXELFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImagePixelFormat}

  var GdipGetImageThumbnail: function(image: GPIMAGE; thumbWidth: UINT;
    thumbHeight: UINT; out thumbImage: GPIMAGE;
    callback: GETTHUMBNAILIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageThumbnail}

  var GdipGetEncoderParameterListSize: function(image: GPIMAGE;
    clsidEncoder: PGUID; out size: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetEncoderParameterListSize}

  var GdipGetEncoderParameterList: function(image: GPIMAGE; clsidEncoder: PGUID;
    size: UINT; buffer: PENCODERPARAMETERS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetEncoderParameterList}

  var GdipImageGetFrameDimensionsCount: function(image: GPIMAGE;
    var count: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageGetFrameDimensionsCount}

  var GdipImageGetFrameDimensionsList: function(image: GPIMAGE; dimensionIDs: PGUID;
    count: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageGetFrameDimensionsList}

  var GdipImageGetFrameCount: function(image: GPIMAGE; dimensionID: PGUID;
    var count: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageGetFrameCount}

  var GdipImageSelectActiveFrame: function(image: GPIMAGE; dimensionID: PGUID;
    frameIndex: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageSelectActiveFrame}

  var GdipImageRotateFlip: function(image: GPIMAGE;
    rfType: ROTATEFLIPTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageRotateFlip}

  var GdipGetImagePalette: function(image: GPIMAGE; palette: PCOLORPALETTE;
    size: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImagePalette}

  var GdipSetImagePalette: function(image: GPIMAGE;
    palette: PCOLORPALETTE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImagePalette}

  var GdipGetImagePaletteSize: function(image: GPIMAGE;
    var size: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImagePaletteSize}

  var GdipGetPropertyCount: function(image: GPIMAGE;
    var numOfProperty: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPropertyCount}

  var GdipGetPropertyIdList: function(image: GPIMAGE; numOfProperty: UINT;
    list: PPROPID): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPropertyIdList}

  var GdipGetPropertyItemSize: function(image: GPIMAGE; propId: PROPID;
    var size: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPropertyItemSize}

  var GdipGetPropertyItem: function(image: GPIMAGE; propId: PROPID; propSize: UINT;
    buffer: PPROPERTYITEM): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPropertyItem}

  var GdipGetPropertySize: function(image: GPIMAGE; var totalBufferSize: UINT;
    var numProperties: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPropertySize}

  var GdipGetAllPropertyItems: function(image: GPIMAGE; totalBufferSize: UINT;
    numProperties: UINT; allItems: PPROPERTYITEM): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetAllPropertyItems}

  var GdipRemovePropertyItem: function(image: GPIMAGE;
    propId: PROPID): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRemovePropertyItem}

  var GdipSetPropertyItem: function(image: GPIMAGE;
    item: PPROPERTYITEM): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPropertyItem}

  var GdipImageForceValidation: function(image: GPIMAGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipImageForceValidation}

//---------------------------------------------------------------------------- 
// Bitmap APIs
//----------------------------------------------------------------------------

  var GdipCreateBitmapFromStream: function(stream: ISTREAM;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromStream}

  var GdipCreateBitmapFromFile: function(filename: PWCHAR;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromFile}

  var GdipCreateBitmapFromStreamICM: function(stream: ISTREAM;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromStreamICM}

  var GdipCreateBitmapFromFileICM: function(filename: PWCHAR;
    var bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromFileICM}

  var GdipCreateBitmapFromScan0: function(width: Integer; height: Integer;
    stride: Integer; format: PIXELFORMAT; scan0: PBYTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromScan0}

  var GdipCreateBitmapFromGraphics: function(width: Integer; height: Integer;
    target: GPGRAPHICS; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromGraphics}

  var GdipCreateBitmapFromDirectDrawSurface: function(surface: IDIRECTDRAWSURFACE7;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromDirectDrawSurface}

  var GdipCreateBitmapFromGdiDib: function(gdiBitmapInfo: PBitmapInfo;
    gdiBitmapData: Pointer; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromGdiDib}

  var GdipCreateBitmapFromHBITMAP: function(hbm: HBITMAP; hpal: HPALETTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromHBITMAP}

  var GdipCreateHBITMAPFromBitmap: function(bitmap: GPBITMAP; out hbmReturn: HBITMAP;
    background: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateHBITMAPFromBitmap}

  var GdipCreateBitmapFromHICON: function(hicon: HICON;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromHICON}

  var GdipCreateHICONFromBitmap: function(bitmap: GPBITMAP;
    out hbmReturn: HICON): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateHICONFromBitmap}

  var GdipCreateBitmapFromResource: function(hInstance: HMODULE;
    lpBitmapName: PWCHAR; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateBitmapFromResource}

  var GdipCloneBitmapArea: function(x: Single; y: Single; width: Single;
    height: Single; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneBitmapArea}

  var GdipCloneBitmapAreaI: function(x: Integer; y: Integer; width: Integer;
    height: Integer; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneBitmapAreaI}

  var GdipBitmapLockBits: function(bitmap: GPBITMAP; rect: GPRECT; flags: UINT;
    format: PIXELFORMAT; lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBitmapLockBits}

  var GdipBitmapUnlockBits: function(bitmap: GPBITMAP;
    lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBitmapUnlockBits}

  var GdipBitmapGetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;
    var color: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBitmapGetPixel}

  var GdipBitmapSetPixel: function(bitmap: GPBITMAP; x: Integer; y: Integer;
    color: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBitmapSetPixel}

  var GdipBitmapSetResolution: function(bitmap: GPBITMAP; xdpi: Single;
    ydpi: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBitmapSetResolution}

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

  var GdipCreateImageAttributes: function(
    out imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateImageAttributes}

  var GdipCloneImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;
    out cloneImageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneImageAttributes}

  var GdipDisposeImageAttributes: function(
    imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDisposeImageAttributes}

  var GdipSetImageAttributesToIdentity: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesToIdentity}

  var GdipResetImageAttributes: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetImageAttributes}

  var GdipSetImageAttributesColorMatrix: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorMatrix: PCOLORMATRIX;
    grayMatrix: PCOLORMATRIX; flags: COLORMATRIXFLAGS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesColorMatrix}

  var GdipSetImageAttributesThreshold: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    threshold: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesThreshold}

  var GdipSetImageAttributesGamma: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; gamma: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesGamma}

  var GdipSetImageAttributesNoOp: function(imageattr: GPIMAGEATTRIBUTES;
  type_: COLORADJUSTTYPE; enableFlag: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesNoOp}

  var GdipSetImageAttributesColorKeys: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorLow: ARGB;
    colorHigh: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesColorKeys}

  var GdipSetImageAttributesOutputChannel: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    channelFlags: COLORCHANNELFLAGS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesOutputChannel}

  var GdipSetImageAttributesOutputChannelColorProfile: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    colorProfileFilename: PWCHAR): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesOutputChannelColorProfile}

  var GdipSetImageAttributesRemapTable: function(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; mapSize: UINT;
    map: PCOLORMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesRemapTable}

  var GdipSetImageAttributesWrapMode: function(imageAttr: GPIMAGEATTRIBUTES;
    wrap: WRAPMODE; argb: ARGB; clamp: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesWrapMode}

  var GdipSetImageAttributesICMMode: function(imageAttr: GPIMAGEATTRIBUTES;
    on_: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetImageAttributesICMMode}

  var GdipGetImageAttributesAdjustedPalette: function(imageAttr: GPIMAGEATTRIBUTES;
    colorPalette: PCOLORPALETTE;
    colorAdjustType: COLORADJUSTTYPE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageAttributesAdjustedPalette}

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

  var GdipFlush: function(graphics: GPGRAPHICS;
    intention: GPFLUSHINTENTION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFlush}

  var GdipCreateFromHDC: function(hdc: HDC;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFromHDC}

  var GdipCreateFromHDC2: function(hdc: HDC; hDevice: THandle;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFromHDC2}

  var GdipCreateFromHWND: function(hwnd: HWND;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFromHWND}

  var GdipCreateFromHWNDICM: function(hwnd: HWND;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFromHWNDICM}

  var GdipDeleteGraphics: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteGraphics}

  var GdipGetDC: function(graphics: GPGRAPHICS; var hdc: HDC): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetDC}

  var GdipReleaseDC: function(graphics: GPGRAPHICS; hdc: HDC): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipReleaseDC}

  var GdipSetCompositingMode: function(graphics: GPGRAPHICS;
    compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCompositingMode}

  var GdipGetCompositingMode: function(graphics: GPGRAPHICS;
    var compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCompositingMode}

  var GdipSetRenderingOrigin: function(graphics: GPGRAPHICS; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetRenderingOrigin}

  var GdipGetRenderingOrigin: function(graphics: GPGRAPHICS; var x: Integer;
    var y: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetRenderingOrigin}

  var GdipSetCompositingQuality: function(graphics: GPGRAPHICS;
    compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetCompositingQuality}

  var GdipGetCompositingQuality: function(graphics: GPGRAPHICS;
    var compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCompositingQuality}

  var GdipSetSmoothingMode: function(graphics: GPGRAPHICS;
    smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetSmoothingMode}

  var GdipGetSmoothingMode: function(graphics: GPGRAPHICS;
    var smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetSmoothingMode}

  var GdipSetPixelOffsetMode: function(graphics: GPGRAPHICS;
    pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPixelOffsetMode}

  var GdipGetPixelOffsetMode: function(graphics: GPGRAPHICS;
    var pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPixelOffsetMode}

  var GdipSetTextRenderingHint: function(graphics: GPGRAPHICS;
    mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetTextRenderingHint}

  var GdipGetTextRenderingHint: function(graphics: GPGRAPHICS;
    var mode: TEXTRENDERINGHINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetTextRenderingHint}

  var GdipSetTextContrast: function(graphics: GPGRAPHICS;
    contrast: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetTextContrast}

  var GdipGetTextContrast: function(graphics: GPGRAPHICS;
    var contrast: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetTextContrast}

  var GdipSetInterpolationMode: function(graphics: GPGRAPHICS;
    interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetInterpolationMode}

  var GdipGetInterpolationMode: function(graphics: GPGRAPHICS;
    var interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetInterpolationMode}

  var GdipSetWorldTransform: function(graphics: GPGRAPHICS;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetWorldTransform}

  var GdipResetWorldTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetWorldTransform}

  var GdipMultiplyWorldTransform: function(graphics: GPGRAPHICS; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMultiplyWorldTransform}

  var GdipTranslateWorldTransform: function(graphics: GPGRAPHICS; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateWorldTransform}

  var GdipScaleWorldTransform: function(graphics: GPGRAPHICS; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipScaleWorldTransform}

  var GdipRotateWorldTransform: function(graphics: GPGRAPHICS; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRotateWorldTransform}

  var GdipGetWorldTransform: function(graphics: GPGRAPHICS;
    matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetWorldTransform}

  var GdipResetPageTransform: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetPageTransform}

  var GdipGetPageUnit: function(graphics: GPGRAPHICS;
    var unit_: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPageUnit}

  var GdipGetPageScale: function(graphics: GPGRAPHICS;
    var scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetPageScale}

  var GdipSetPageUnit: function(graphics: GPGRAPHICS;
    unit_: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPageUnit}

  var GdipSetPageScale: function(graphics: GPGRAPHICS;
    scale: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetPageScale}

  var GdipGetDpiX: function(graphics: GPGRAPHICS;
    var dpi: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetDpiX}

  var GdipGetDpiY: function(graphics: GPGRAPHICS;
    var dpi: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetDpiY}

  var GdipTransformPoints: function(graphics: GPGRAPHICS;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformPoints}

  var GdipTransformPointsI: function(graphics: GPGRAPHICS;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTransformPointsI}

  var GdipGetNearestColor: function(graphics: GPGRAPHICS;
    argb: PARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetNearestColor}

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors

  var GdipCreateHalftonePalette: function: HPALETTE; stdcall;
  {$EXTERNALSYM GdipCreateHalftonePalette}

  var GdipDrawLine: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawLine}

  var GdipDrawLineI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawLineI}

  var GdipDrawLines: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawLines}

  var GdipDrawLinesI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawLinesI}

  var GdipDrawArc: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;
    width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawArc}

  var GdipDrawArcI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawArcI}

  var GdipDrawBezier: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single;
    y4: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawBezier}

  var GdipDrawBezierI: function(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer;
    x4: Integer; y4: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawBezierI}

  var GdipDrawBeziers: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawBeziers}

  var GdipDrawBeziersI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawBeziersI}

  var GdipDrawRectangle: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawRectangle}

  var GdipDrawRectangleI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawRectangleI}

  var GdipDrawRectangles: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawRectangles}

  var GdipDrawRectanglesI: function(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawRectanglesI}

  var GdipDrawEllipse: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawEllipse}

  var GdipDrawEllipseI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawEllipseI}

  var GdipDrawPie: function(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;
    width: Single;  height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawPie}

  var GdipDrawPieI: function(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawPieI}

  var GdipDrawPolygon: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawPolygon}

  var GdipDrawPolygonI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawPolygonI}

  var GdipDrawPath: function(graphics: GPGRAPHICS; pen: GPPEN;
    path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawPath}

  var GdipDrawCurve: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurve}

  var GdipDrawCurveI: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurveI}

  var GdipDrawCurve2: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurve2}

  var GdipDrawCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurve2I}

  var GdipDrawCurve3: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurve3}

  var GdipDrawCurve3I: function(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCurve3I}

  var GdipDrawClosedCurve: function(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawClosedCurve}

  var GdipDrawClosedCurveI: function(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawClosedCurveI}

  var GdipDrawClosedCurve2: function(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINTF; count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawClosedCurve2}

  var GdipDrawClosedCurve2I: function(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINT; count: Integer; tension: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawClosedCurve2I}

  var GdipGraphicsClear: function(graphics: GPGRAPHICS;
    color: ARGB): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGraphicsClear}

  var GdipFillRectangle: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillRectangle}

  var GdipFillRectangleI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillRectangleI}

  var GdipFillRectangles: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    rects: GPRECTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillRectangles}

  var GdipFillRectanglesI: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    rects: GPRECT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillRectanglesI}

  var GdipFillPolygon: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPolygon}

  var GdipFillPolygonI: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPolygonI}

  var GdipFillPolygon2: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPolygon2}

  var GdipFillPolygon2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPolygon2I}

  var GdipFillEllipse: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillEllipse}

  var GdipFillEllipseI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillEllipseI}

  var GdipFillPie: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPie}

  var GdipFillPieI: function(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPieI}

  var GdipFillPath: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    path: GPPATH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillPath}

  var GdipFillClosedCurve: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillClosedCurve}

  var GdipFillClosedCurveI: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillClosedCurveI}

  var GdipFillClosedCurve2: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillClosedCurve2}

  var GdipFillClosedCurve2I: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillClosedCurve2I}

  var GdipFillRegion: function(graphics: GPGRAPHICS; brush: GPBRUSH;
    region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFillRegion}

  var GdipDrawImage: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;
    y: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImage}

  var GdipDrawImageI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImageI}

  var GdipDrawImageRect: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImageRect}

  var GdipDrawImageRectI: function(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImageRectI}

  var GdipDrawImagePoints: function(graphics: GPGRAPHICS; image: GPIMAGE;
    dstpoints: GPPOINTF; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePoints}

  var GdipDrawImagePointsI: function(graphics: GPGRAPHICS; image: GPIMAGE;
    dstpoints: GPPOINT; count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePointsI}

  var GdipDrawImagePointRect: function(graphics: GPGRAPHICS; image: GPIMAGE;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePointRect}

  var GdipDrawImagePointRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;
    x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
    srcheight: Integer; srcUnit: GPUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePointRectI}

  var GdipDrawImageRectRect: function(graphics: GPGRAPHICS; image: GPIMAGE;
    dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
    srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImageRectRect}

  var GdipDrawImageRectRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;
    dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
    srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImageRectRectI}

  var GdipDrawImagePointsRect: function(graphics: GPGRAPHICS; image: GPIMAGE;
    points: GPPOINTF; count: Integer; srcx: Single; srcy: Single;
    srcwidth: Single; srcheight: Single; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePointsRect}

  var GdipDrawImagePointsRectI: function(graphics: GPGRAPHICS; image: GPIMAGE;
    points: GPPOINT; count: Integer; srcx: Integer; srcy: Integer;
    srcwidth: Integer; srcheight: Integer; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawImagePointsRectI}

  var GdipEnumerateMetafileDestPoint: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PGPPointF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestPoint}

  var GdipEnumerateMetafileDestPointI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PGPPoint; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestPointI}

  var GdipEnumerateMetafileDestRect: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PGPRectF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestRect}

  var GdipEnumerateMetafileDestRectI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PGPRect; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestRectI}

  var GdipEnumerateMetafileDestPoints: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PGPPointF; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestPoints}

  var GdipEnumerateMetafileDestPointsI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PGPPoint; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileDestPointsI}

  var GdipEnumerateMetafileSrcRectDestPoint: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PGPPointF; srcRect: PGPRectF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoint}

  var GdipEnumerateMetafileSrcRectDestPointI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PGPPoint; srcRect: PGPRect; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointI}

  var GdipEnumerateMetafileSrcRectDestRect: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PGPRectF; srcRect: PGPRectF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRect}

  var GdipEnumerateMetafileSrcRectDestRectI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PGPRect; srcRect: PGPRect; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRectI}

  var GdipEnumerateMetafileSrcRectDestPoints: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PGPPointF; count: Integer; srcRect: PGPRectF;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoints}

  var GdipEnumerateMetafileSrcRectDestPointsI: function(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PGPPoint; count: Integer; srcRect: PGPRect;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointsI}

  var GdipPlayMetafileRecord: function(metafile: GPMETAFILE;
    recordType: EMFPLUSRECORDTYPE; flags: UINT; dataSize: UINT;
    data: PBYTE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPlayMetafileRecord}

  var GdipSetClipGraphics: function(graphics: GPGRAPHICS; srcgraphics: GPGRAPHICS;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipGraphics}

  var GdipSetClipRect: function(graphics: GPGRAPHICS; x: Single; y: Single;
    width: Single; height: Single; combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipRect}

  var GdipSetClipRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;
    width: Integer; height: Integer;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipRectI}

  var GdipSetClipPath: function(graphics: GPGRAPHICS; path: GPPATH;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipPath}

  var GdipSetClipRegion: function(graphics: GPGRAPHICS; region: GPREGION;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipRegion}

  var GdipSetClipHrgn: function(graphics: GPGRAPHICS; hRgn: HRGN;
    combineMode: COMBINEMODE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetClipHrgn}

  var GdipResetClip: function(graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipResetClip}

  var GdipTranslateClip: function(graphics: GPGRAPHICS; dx: Single;
    dy: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateClip}

  var GdipTranslateClipI: function(graphics: GPGRAPHICS; dx: Integer;
    dy: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipTranslateClipI}

  var GdipGetClip: function(graphics: GPGRAPHICS;
    region: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetClip}

  var GdipGetClipBounds: function(graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetClipBounds}

  var GdipGetClipBoundsI: function(graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetClipBoundsI}

  var GdipIsClipEmpty: function(graphics: GPGRAPHICS;
    result: PBool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsClipEmpty}

  var GdipGetVisibleClipBounds: function(graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetVisibleClipBounds}

  var GdipGetVisibleClipBoundsI: function(graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetVisibleClipBoundsI}

  var GdipIsVisibleClipEmpty: function(graphics: GPGRAPHICS;
    var result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleClipEmpty}

  var GdipIsVisiblePoint: function(graphics: GPGRAPHICS; x: Single; y: Single;
    var result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisiblePoint}

  var GdipIsVisiblePointI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;
    var result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisiblePointI}

  var GdipIsVisibleRect: function(graphics: GPGRAPHICS; x: Single; y: Single;
    width: Single; height: Single; var result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRect}

  var GdipIsVisibleRectI: function(graphics: GPGRAPHICS; x: Integer; y: Integer;
    width: Integer; height: Integer; var result: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsVisibleRectI}

  var GdipSaveGraphics: function(graphics: GPGRAPHICS;
    var state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSaveGraphics}

  var GdipRestoreGraphics: function(graphics: GPGRAPHICS;
    state: GRAPHICSSTATE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRestoreGraphics}

  var GdipBeginContainer: function(graphics: GPGRAPHICS; dstrect: GPRECTF;
    srcrect: GPRECTF; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBeginContainer}

  var GdipBeginContainerI: function(graphics: GPGRAPHICS; dstrect: GPRECT;
    srcrect: GPRECT; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBeginContainerI}

  var GdipBeginContainer2: function(graphics: GPGRAPHICS;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipBeginContainer2}

  var GdipEndContainer: function(graphics: GPGRAPHICS;
    state: GRAPHICSCONTAINER): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipEndContainer}

  var GdipGetMetafileHeaderFromWmf: function(hWmf: HMETAFILE;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    header: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileHeaderFromWmf}

  var GdipGetMetafileHeaderFromEmf: function(hEmf: HENHMETAFILE;
    header: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileHeaderFromEmf}

  var GdipGetMetafileHeaderFromFile: function(filename: PWCHAR;
    header: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileHeaderFromFile}

  var GdipGetMetafileHeaderFromStream: function(stream: ISTREAM;
    header: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileHeaderFromStream}

  var GdipGetMetafileHeaderFromMetafile: function(metafile: GPMETAFILE;
    header: Pointer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileHeaderFromMetafile}

  var GdipGetHemfFromMetafile: function(metafile: GPMETAFILE;
    var hEmf: HENHMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetHemfFromMetafile}

  var GdipCreateStreamOnFile: function(filename: PWCHAR; access: UINT;
    out stream: ISTREAM): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateStreamOnFile}

  var GdipCreateMetafileFromWmf: function(hWmf: HMETAFILE; deleteWmf: Bool;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMetafileFromWmf}

  var GdipCreateMetafileFromEmf: function(hEmf: HENHMETAFILE; deleteEmf: Bool;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMetafileFromEmf}

  var GdipCreateMetafileFromFile: function(file_: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMetafileFromFile}

  var GdipCreateMetafileFromWmfFile: function(file_: PWCHAR;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMetafileFromWmfFile}

  var GdipCreateMetafileFromStream: function(stream: ISTREAM;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateMetafileFromStream}

  var GdipRecordMetafile: function(referenceHdc: HDC; type_: EMFTYPE;
    frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafile}

  var GdipRecordMetafileI: function(referenceHdc: HDC; type_: EMFTYPE;
    frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT; description: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafileI}

  var GdipRecordMetafileFileName: function(fileName: PWCHAR; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafileFileName}

  var GdipRecordMetafileFileNameI: function(fileName: PWCHAR; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafileFileNameI}

  var GdipRecordMetafileStream: function(stream: ISTREAM; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafileStream}

  var GdipRecordMetafileStreamI: function(stream: ISTREAM; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipRecordMetafileStreamI}

  var GdipSetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;
    metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetMetafileDownLevelRasterizationLimit}

  var GdipGetMetafileDownLevelRasterizationLimit: function(metafile: GPMETAFILE;
    var metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetMetafileDownLevelRasterizationLimit}

  var GdipGetImageDecodersSize: function(out numDecoders: UINT;
    out size: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageDecodersSize}

  var GdipGetImageDecoders: function(numDecoders: UINT; size: UINT;
    decoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageDecoders}

  var GdipGetImageEncodersSize: function(out numEncoders: UINT;
    out size: UINT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageEncodersSize}

  var GdipGetImageEncoders: function(numEncoders: UINT; size: UINT;
    encoders: PIMAGECODECINFO): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetImageEncoders}

  var GdipComment: function(graphics: GPGRAPHICS; sizeData: UINT;
    data: PBYTE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipComment}

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

  var GdipCreateFontFamilyFromName: function(name: PWCHAR;
    fontCollection: GPFONTCOLLECTION;
    out FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFontFamilyFromName}

  var GdipDeleteFontFamily: function(FontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteFontFamily}

  var GdipCloneFontFamily: function(FontFamily: GPFONTFAMILY;
    out clonedFontFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneFontFamily}

  var GdipGetGenericFontFamilySansSerif: function(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetGenericFontFamilySansSerif}

  var GdipGetGenericFontFamilySerif: function(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetGenericFontFamilySerif}

  var GdipGetGenericFontFamilyMonospace: function(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetGenericFontFamilyMonospace}

  var GdipGetFamilyName: function(family: GPFONTFAMILY; name: PWideChar;
    language: LANGID): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFamilyName}

  var GdipIsStyleAvailable: function(family: GPFONTFAMILY; style: Integer;
    var IsStyleAvailable: Bool): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipIsStyleAvailable}

  var GdipFontCollectionEnumerable: function(fontCollection: GPFONTCOLLECTION;
    graphics: GPGRAPHICS; var numFound: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFontCollectionEnumerable}

  var GdipFontCollectionEnumerate: function(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: array of GPFONTFAMILY;
    var numFound: Integer; graphics: GPGRAPHICS): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipFontCollectionEnumerate}

  var GdipGetEmHeight: function(family: GPFONTFAMILY; style: Integer;
    out EmHeight: UINT16): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetEmHeight}

  var GdipGetCellAscent: function(family: GPFONTFAMILY; style: Integer;
    var CellAscent: UINT16): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCellAscent}

  var GdipGetCellDescent: function(family: GPFONTFAMILY; style: Integer;
    var CellDescent: UINT16): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetCellDescent}

  var GdipGetLineSpacing: function(family: GPFONTFAMILY; style: Integer;
    var LineSpacing: UINT16): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLineSpacing}

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

  var GdipCreateFontFromDC: function(hdc: HDC; out font: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFontFromDC}

  var GdipCreateFontFromLogfontA: function(hdc: HDC; logfont: PLOGFONTA;
    out font: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFontFromLogfontA}

  var GdipCreateFontFromLogfontW: function(hdc: HDC; logfont: PLOGFONTW;
    out font: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFontFromLogfontW}

  var GdipCreateFont: function(fontFamily: GPFONTFAMILY; emSize: Single;
    style: Integer; unit_: Integer; out font: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateFont}

  var GdipCloneFont: function(font: GPFONT;
    out cloneFont: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneFont}

  var GdipDeleteFont: function(font: GPFONT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteFont}

  var GdipGetFamily: function(font: GPFONT;
    out family: GPFONTFAMILY): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFamily}

  var GdipGetFontStyle: function(font: GPFONT;
    var style: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontStyle}

  var GdipGetFontSize: function(font: GPFONT; var size: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontSize}

  var GdipGetFontUnit: function(font: GPFONT; var unit_: TUNIT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontUnit}

  var GdipGetFontHeight: function(font: GPFONT; graphics: GPGRAPHICS;
    var height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontHeight}

  var GdipGetFontHeightGivenDPI: function(font: GPFONT; dpi: Single;
    var height: Single): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontHeightGivenDPI}

  var GdipGetLogFontA: function(font: GPFONT; graphics: GPGRAPHICS;
    var logfontA: LOGFONTA): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLogFontA}

  var GdipGetLogFontW: function(font: GPFONT; graphics: GPGRAPHICS;
    var logfontW: LOGFONTW): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetLogFontW}

  var GdipNewInstalledFontCollection: function(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipNewInstalledFontCollection}

  var GdipNewPrivateFontCollection: function(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipNewPrivateFontCollection}

  var GdipDeletePrivateFontCollection: function(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeletePrivateFontCollection}

  var GdipGetFontCollectionFamilyCount: function(fontCollection: GPFONTCOLLECTION;
    var numFound: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontCollectionFamilyCount}

  var GdipGetFontCollectionFamilyList: function(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: GPFONTFAMILY;
    var numFound: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetFontCollectionFamilyList}

  var GdipPrivateAddFontFile: function(fontCollection: GPFONTCOLLECTION;
    filename: PWCHAR): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPrivateAddFontFile}

  var GdipPrivateAddMemoryFont: function(fontCollection: GPFONTCOLLECTION;
    memory: Pointer; length: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipPrivateAddMemoryFont}

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

  var GdipDrawString: function(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; brush: GPBRUSH): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawString}

  var GdipMeasureString: function(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; boundingBox: PGPRectF;
    codepointsFitted: PInteger; linesFilled: PInteger): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMeasureString}

  var GdipMeasureCharacterRanges: function(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GPSTRINGFORMAT; regionCount: Integer;
    const regions: GPREGION): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMeasureCharacterRanges}

  var GdipDrawDriverString: function(graphics: GPGRAPHICS; const text: PUINT16;
    length: Integer; const font: GPFONT; const brush: GPBRUSH;
    const positions: PGPPointF; flags: Integer;
    const matrix: GPMATRIX): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawDriverString}

  var GdipMeasureDriverString: function(graphics: GPGRAPHICS; text: PUINT16;
    length: Integer; font: GPFONT; positions: PGPPointF; flags: Integer;
    matrix: GPMATRIX; boundingBox: PGPRectF): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipMeasureDriverString}

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

  var GdipCreateStringFormat: function(formatAttributes: Integer; language: LANGID;
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateStringFormat}

  var GdipStringFormatGetGenericDefault: function(
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipStringFormatGetGenericDefault}

  var GdipStringFormatGetGenericTypographic: function(
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipStringFormatGetGenericTypographic}

  var GdipDeleteStringFormat: function(format: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteStringFormat}

  var GdipCloneStringFormat: function(format: GPSTRINGFORMAT;
    out newFormat: GPSTRINGFORMAT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCloneStringFormat}

  var GdipSetStringFormatFlags: function(format: GPSTRINGFORMAT;
    flags: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatFlags}

  var GdipGetStringFormatFlags: function(format: GPSTRINGFORMAT;
    out flags: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatFlags}

  var GdipSetStringFormatAlign: function(format: GPSTRINGFORMAT;
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatAlign}

  var GdipGetStringFormatAlign: function(format: GPSTRINGFORMAT;
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatAlign}

  var GdipSetStringFormatLineAlign: function(format: GPSTRINGFORMAT;
    align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatLineAlign}

  var GdipGetStringFormatLineAlign: function(format: GPSTRINGFORMAT;
    out align: STRINGALIGNMENT): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatLineAlign}

  var GdipSetStringFormatTrimming: function(format: GPSTRINGFORMAT;
    trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatTrimming}

  var GdipGetStringFormatTrimming: function(format: GPSTRINGFORMAT;
    out trimming: STRINGTRIMMING): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatTrimming}

  var GdipSetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;
    hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatHotkeyPrefix}

  var GdipGetStringFormatHotkeyPrefix: function(format: GPSTRINGFORMAT;
    out hotkeyPrefix: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatHotkeyPrefix}

  var GdipSetStringFormatTabStops: function(format: GPSTRINGFORMAT;
    firstTabOffset: Single; count: Integer;
    tabStops: PSingle): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatTabStops}

  var GdipGetStringFormatTabStops: function(format: GPSTRINGFORMAT;
    count: Integer; firstTabOffset: PSingle;
    tabStops: PSingle): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatTabStops}

  var GdipGetStringFormatTabStopCount: function(format: GPSTRINGFORMAT;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatTabStopCount}

  var GdipSetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;
    language: LANGID;
    substitute: STRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatDigitSubstitution}

  var GdipGetStringFormatDigitSubstitution: function(format: GPSTRINGFORMAT;
    language: PUINT; substitute: PSTRINGDIGITSUBSTITUTE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatDigitSubstitution}

  var GdipGetStringFormatMeasurableCharacterRangeCount: function(format: GPSTRINGFORMAT;
    out count: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipGetStringFormatMeasurableCharacterRangeCount}

  var GdipSetStringFormatMeasurableCharacterRanges: function(format: GPSTRINGFORMAT;
    rangeCount: Integer; ranges: PCHARACTERRANGE): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipSetStringFormatMeasurableCharacterRanges}

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

  var GdipCreateCachedBitmap: function(bitmap: GPBITMAP; graphics: GPGRAPHICS;
    out cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipCreateCachedBitmap}

  var GdipDeleteCachedBitmap: function(
    cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDeleteCachedBitmap}

  var GdipDrawCachedBitmap: function(graphics: GPGRAPHICS;
    cachedBitmap: GPCACHEDBITMAP; x: Integer;
    y: Integer): GPSTATUS; stdcall;
  {$EXTERNALSYM GdipDrawCachedBitmap}

  var GdipEmfToWmfBits: function(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer; eFlags: Integer): UINT; stdcall;
  {$EXTERNALSYM GdipEmfToWmfBits}

function InitGDIPLibrary: Boolean;
procedure FreeGDIPLibrary;
function UseGDIP: Boolean;

implementation

uses SyncObjs;

var
  hWINGDIPDLL: THandle;
  ReferenceCount: Integer;
  Lock: TCriticalSection;

function InitGDIPLibrary: Boolean;
begin
  Lock.Enter;
  try
    Inc(ReferenceCount);
    if hWINGDIPDLL = 0 then begin
      hWINGDIPDLL := LoadLibrary(WINGDIPDLL);
      if hWINGDIPDLL > 0 then begin
        GdipAlloc := GetProcAddress(hWINGDIPDLL,'GdipAlloc');
        GdipFree := GetProcAddress(hWINGDIPDLL,'GdipFree');
        GdiplusStartup := GetProcAddress(hWINGDIPDLL,'GdiplusStartup');
        GdiplusShutdown := GetProcAddress(hWINGDIPDLL,'GdiplusShutdown');
        GdipCreatePath := GetProcAddress(hWINGDIPDLL,'GdipCreatePath');
        GdipCreatePath2 := GetProcAddress(hWINGDIPDLL,'GdipCreatePath2');
        GdipCreatePath2I := GetProcAddress(hWINGDIPDLL,'GdipCreatePath2I');
        GdipClonePath := GetProcAddress(hWINGDIPDLL,'GdipClonePath');
        GdipDeletePath := GetProcAddress(hWINGDIPDLL,'GdipDeletePath');
        GdipResetPath := GetProcAddress(hWINGDIPDLL,'GdipResetPath');
        GdipGetPointCount := GetProcAddress(hWINGDIPDLL,'GdipGetPointCount');
        GdipGetPathTypes := GetProcAddress(hWINGDIPDLL,'GdipGetPathTypes');
        GdipGetPathPoints := GetProcAddress(hWINGDIPDLL,'GdipGetPathPoints');
        GdipGetPathPointsI := GetProcAddress(hWINGDIPDLL,'GdipGetPathPointsI');
        GdipGetPathFillMode := GetProcAddress(hWINGDIPDLL,'GdipGetPathFillMode');
        GdipSetPathFillMode := GetProcAddress(hWINGDIPDLL,'GdipSetPathFillMode');
        GdipGetPathData := GetProcAddress(hWINGDIPDLL,'GdipGetPathData');
        GdipStartPathFigure := GetProcAddress(hWINGDIPDLL,'GdipStartPathFigure');
        GdipClosePathFigure := GetProcAddress(hWINGDIPDLL,'GdipClosePathFigure');
        GdipClosePathFigures := GetProcAddress(hWINGDIPDLL,'GdipClosePathFigures');
        GdipSetPathMarker := GetProcAddress(hWINGDIPDLL,'GdipSetPathMarker');
        GdipClearPathMarkers := GetProcAddress(hWINGDIPDLL,'GdipClearPathMarkers');
        GdipReversePath := GetProcAddress(hWINGDIPDLL,'GdipReversePath');
        GdipGetPathLastPoint := GetProcAddress(hWINGDIPDLL,'GdipGetPathLastPoint');
        GdipAddPathLine := GetProcAddress(hWINGDIPDLL,'GdipAddPathLine');
        GdipAddPathLine2 := GetProcAddress(hWINGDIPDLL,'GdipAddPathLine2');
        GdipAddPathArc := GetProcAddress(hWINGDIPDLL,'GdipAddPathArc');
        GdipAddPathBezier := GetProcAddress(hWINGDIPDLL,'GdipAddPathBezier');
        GdipAddPathBeziers := GetProcAddress(hWINGDIPDLL,'GdipAddPathBeziers');
        GdipAddPathCurve := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurve');
        GdipAddPathCurve2 := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurve2');
        GdipAddPathCurve3 := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurve3');
        GdipAddPathClosedCurve := GetProcAddress(hWINGDIPDLL,'GdipAddPathClosedCurve');
        GdipAddPathClosedCurve2 := GetProcAddress(hWINGDIPDLL,'GdipAddPathClosedCurve2');
        GdipAddPathRectangle := GetProcAddress(hWINGDIPDLL,'GdipAddPathRectangle');
        GdipAddPathRectangles := GetProcAddress(hWINGDIPDLL,'GdipAddPathRectangles');
        GdipAddPathEllipse := GetProcAddress(hWINGDIPDLL,'GdipAddPathEllipse');
        GdipAddPathPie := GetProcAddress(hWINGDIPDLL,'GdipAddPathPie');
        GdipAddPathPolygon := GetProcAddress(hWINGDIPDLL,'GdipAddPathPolygon');
        GdipAddPathPath := GetProcAddress(hWINGDIPDLL,'GdipAddPathPath');
        GdipAddPathString := GetProcAddress(hWINGDIPDLL,'GdipAddPathString');
        GdipAddPathStringI := GetProcAddress(hWINGDIPDLL,'GdipAddPathStringI');
        GdipAddPathLineI := GetProcAddress(hWINGDIPDLL,'GdipAddPathLineI');
        GdipAddPathLine2I := GetProcAddress(hWINGDIPDLL,'GdipAddPathLine2I');
        GdipAddPathArcI := GetProcAddress(hWINGDIPDLL,'GdipAddPathArcI');
        GdipAddPathBezierI := GetProcAddress(hWINGDIPDLL,'GdipAddPathBezierI');
        GdipAddPathBeziersI := GetProcAddress(hWINGDIPDLL,'GdipAddPathBeziersI');
        GdipAddPathCurveI := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurveI');
        GdipAddPathCurve2I := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurve2I');
        GdipAddPathCurve3I := GetProcAddress(hWINGDIPDLL,'GdipAddPathCurve3I');
        GdipAddPathClosedCurveI := GetProcAddress(hWINGDIPDLL,'GdipAddPathClosedCurveI');
        GdipAddPathClosedCurve2I := GetProcAddress(hWINGDIPDLL,'GdipAddPathClosedCurve2I');
        GdipAddPathRectangleI := GetProcAddress(hWINGDIPDLL,'GdipAddPathRectangleI');
        GdipAddPathRectanglesI := GetProcAddress(hWINGDIPDLL,'GdipAddPathRectanglesI');
        GdipAddPathEllipseI := GetProcAddress(hWINGDIPDLL,'GdipAddPathEllipseI');
        GdipAddPathPieI := GetProcAddress(hWINGDIPDLL,'GdipAddPathPieI');
        GdipAddPathPolygonI := GetProcAddress(hWINGDIPDLL,'GdipAddPathPolygonI');
        GdipFlattenPath := GetProcAddress(hWINGDIPDLL,'GdipFlattenPath');
        GdipWindingModeOutline := GetProcAddress(hWINGDIPDLL,'GdipWindingModeOutline');
        GdipWidenPath := GetProcAddress(hWINGDIPDLL,'GdipWidenPath');
        GdipWarpPath := GetProcAddress(hWINGDIPDLL,'GdipWarpPath');
        GdipTransformPath := GetProcAddress(hWINGDIPDLL,'GdipTransformPath');
        GdipGetPathWorldBounds := GetProcAddress(hWINGDIPDLL,'GdipGetPathWorldBounds');
        GdipGetPathWorldBoundsI := GetProcAddress(hWINGDIPDLL,'GdipGetPathWorldBoundsI');
        GdipIsVisiblePathPoint := GetProcAddress(hWINGDIPDLL,'GdipIsVisiblePathPoint');
        GdipIsVisiblePathPointI := GetProcAddress(hWINGDIPDLL,'GdipIsVisiblePathPointI');
        GdipIsOutlineVisiblePathPoint := GetProcAddress(hWINGDIPDLL,'GdipIsOutlineVisiblePathPoint');
        GdipIsOutlineVisiblePathPointI := GetProcAddress(hWINGDIPDLL,'GdipIsOutlineVisiblePathPointI');
        GdipCreatePathIter := GetProcAddress(hWINGDIPDLL,'GdipCreatePathIter');
        GdipDeletePathIter := GetProcAddress(hWINGDIPDLL,'GdipDeletePathIter');
        GdipPathIterNextSubpath := GetProcAddress(hWINGDIPDLL,'GdipPathIterNextSubpath');
        GdipPathIterNextSubpathPath := GetProcAddress(hWINGDIPDLL,'GdipPathIterNextSubpathPath');
        GdipPathIterNextPathType := GetProcAddress(hWINGDIPDLL,'GdipPathIterNextPathType');
        GdipPathIterNextMarker := GetProcAddress(hWINGDIPDLL,'GdipPathIterNextMarker');
        GdipPathIterNextMarkerPath := GetProcAddress(hWINGDIPDLL,'GdipPathIterNextMarkerPath');
        GdipPathIterGetCount := GetProcAddress(hWINGDIPDLL,'GdipPathIterGetCount');
        GdipPathIterGetSubpathCount := GetProcAddress(hWINGDIPDLL,'GdipPathIterGetSubpathCount');
        GdipPathIterIsValid := GetProcAddress(hWINGDIPDLL,'GdipPathIterIsValid');
        GdipPathIterHasCurve := GetProcAddress(hWINGDIPDLL,'GdipPathIterHasCurve');
        GdipPathIterRewind := GetProcAddress(hWINGDIPDLL,'GdipPathIterRewind');
        GdipPathIterEnumerate := GetProcAddress(hWINGDIPDLL,'GdipPathIterEnumerate');
        GdipPathIterCopyData := GetProcAddress(hWINGDIPDLL,'GdipPathIterCopyData');
        GdipCreateMatrix := GetProcAddress(hWINGDIPDLL,'GdipCreateMatrix');
        GdipCreateMatrix2 := GetProcAddress(hWINGDIPDLL,'GdipCreateMatrix2');
        GdipCreateMatrix3 := GetProcAddress(hWINGDIPDLL,'GdipCreateMatrix3');
        GdipCreateMatrix3I := GetProcAddress(hWINGDIPDLL,'GdipCreateMatrix3I');
        GdipCloneMatrix := GetProcAddress(hWINGDIPDLL,'GdipCloneMatrix');
        GdipDeleteMatrix := GetProcAddress(hWINGDIPDLL,'GdipDeleteMatrix');
        GdipSetMatrixElements := GetProcAddress(hWINGDIPDLL,'GdipSetMatrixElements');
        GdipMultiplyMatrix := GetProcAddress(hWINGDIPDLL,'GdipMultiplyMatrix');
        GdipTranslateMatrix := GetProcAddress(hWINGDIPDLL,'GdipTranslateMatrix');
        GdipScaleMatrix := GetProcAddress(hWINGDIPDLL,'GdipScaleMatrix');
        GdipRotateMatrix := GetProcAddress(hWINGDIPDLL,'GdipRotateMatrix');
        GdipShearMatrix := GetProcAddress(hWINGDIPDLL,'GdipShearMatrix');
        GdipInvertMatrix := GetProcAddress(hWINGDIPDLL,'GdipInvertMatrix');
        GdipTransformMatrixPoints := GetProcAddress(hWINGDIPDLL,'GdipTransformMatrixPoints');
        GdipTransformMatrixPointsI := GetProcAddress(hWINGDIPDLL,'GdipTransformMatrixPointsI');
        GdipVectorTransformMatrixPoints := GetProcAddress(hWINGDIPDLL,'GdipVectorTransformMatrixPoints');
        GdipVectorTransformMatrixPointsI := GetProcAddress(hWINGDIPDLL,'GdipVectorTransformMatrixPointsI');
        GdipGetMatrixElements := GetProcAddress(hWINGDIPDLL,'GdipGetMatrixElements');
        GdipIsMatrixInvertible := GetProcAddress(hWINGDIPDLL,'GdipIsMatrixInvertible');
        GdipIsMatrixIdentity := GetProcAddress(hWINGDIPDLL,'GdipIsMatrixIdentity');
        GdipIsMatrixEqual := GetProcAddress(hWINGDIPDLL,'GdipIsMatrixEqual');
        GdipCreateRegion := GetProcAddress(hWINGDIPDLL,'GdipCreateRegion');
        GdipCreateRegionRect := GetProcAddress(hWINGDIPDLL,'GdipCreateRegionRect');
        GdipCreateRegionRectI := GetProcAddress(hWINGDIPDLL,'GdipCreateRegionRectI');
        GdipCreateRegionPath := GetProcAddress(hWINGDIPDLL,'GdipCreateRegionPath');
        GdipCreateRegionRgnData := GetProcAddress(hWINGDIPDLL,'GdipCreateRegionRgnData');
        GdipCreateRegionHrgn := GetProcAddress(hWINGDIPDLL,'GdipCreateRegionHrgn');
        GdipCloneRegion := GetProcAddress(hWINGDIPDLL,'GdipCloneRegion');
        GdipDeleteRegion := GetProcAddress(hWINGDIPDLL,'GdipDeleteRegion');
        GdipSetInfinite := GetProcAddress(hWINGDIPDLL,'GdipSetInfinite');
        GdipSetEmpty := GetProcAddress(hWINGDIPDLL,'GdipSetEmpty');
        GdipCombineRegionRect := GetProcAddress(hWINGDIPDLL,'GdipCombineRegionRect');
        GdipCombineRegionRectI := GetProcAddress(hWINGDIPDLL,'GdipCombineRegionRectI');
        GdipCombineRegionPath := GetProcAddress(hWINGDIPDLL,'GdipCombineRegionPath');
        GdipCombineRegionRegion := GetProcAddress(hWINGDIPDLL,'GdipCombineRegionRegion');
        GdipTranslateRegion := GetProcAddress(hWINGDIPDLL,'GdipTranslateRegion');
        GdipTranslateRegionI := GetProcAddress(hWINGDIPDLL,'GdipTranslateRegionI');
        GdipTransformRegion := GetProcAddress(hWINGDIPDLL,'GdipTransformRegion');
        GdipGetRegionBounds := GetProcAddress(hWINGDIPDLL,'GdipGetRegionBounds');
        GdipGetRegionBoundsI := GetProcAddress(hWINGDIPDLL,'GdipGetRegionBoundsI');
        GdipGetRegionHRgn := GetProcAddress(hWINGDIPDLL,'GdipGetRegionHRgn');
        GdipIsEmptyRegion := GetProcAddress(hWINGDIPDLL,'GdipIsEmptyRegion');
        GdipIsInfiniteRegion := GetProcAddress(hWINGDIPDLL,'GdipIsInfiniteRegion');
        GdipIsEqualRegion := GetProcAddress(hWINGDIPDLL,'GdipIsEqualRegion');
        GdipGetRegionDataSize := GetProcAddress(hWINGDIPDLL,'GdipGetRegionDataSize');
        GdipGetRegionData := GetProcAddress(hWINGDIPDLL,'GdipGetRegionData');
        GdipIsVisibleRegionPoint := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRegionPoint');
        GdipIsVisibleRegionPointI := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRegionPointI');
        GdipIsVisibleRegionRect := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRegionRect');
        GdipIsVisibleRegionRectI := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRegionRectI');
        GdipGetRegionScansCount := GetProcAddress(hWINGDIPDLL,'GdipGetRegionScansCount');
        GdipGetRegionScans := GetProcAddress(hWINGDIPDLL,'GdipGetRegionScans');
        GdipGetRegionScansI := GetProcAddress(hWINGDIPDLL,'GdipGetRegionScansI');
        GdipCloneBrush := GetProcAddress(hWINGDIPDLL,'GdipCloneBrush');
        GdipDeleteBrush := GetProcAddress(hWINGDIPDLL,'GdipDeleteBrush');
        GdipGetBrushType := GetProcAddress(hWINGDIPDLL,'GdipGetBrushType');
        GdipCreateHatchBrush := GetProcAddress(hWINGDIPDLL,'GdipCreateHatchBrush');
        GdipGetHatchStyle := GetProcAddress(hWINGDIPDLL,'GdipGetHatchStyle');
        GdipGetHatchForegroundColor := GetProcAddress(hWINGDIPDLL,'GdipGetHatchForegroundColor');
        GdipGetHatchBackgroundColor := GetProcAddress(hWINGDIPDLL,'GdipGetHatchBackgroundColor');
        GdipCreateTexture := GetProcAddress(hWINGDIPDLL,'GdipCreateTexture');
        GdipCreateTexture2 := GetProcAddress(hWINGDIPDLL,'GdipCreateTexture2');
        GdipCreateTextureIA := GetProcAddress(hWINGDIPDLL,'GdipCreateTextureIA');
        GdipCreateTexture2I := GetProcAddress(hWINGDIPDLL,'GdipCreateTexture2I');
        GdipCreateTextureIAI := GetProcAddress(hWINGDIPDLL,'GdipCreateTextureIAI');
        GdipGetTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipGetTextureTransform');
        GdipSetTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipSetTextureTransform');
        GdipResetTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipResetTextureTransform');
        GdipMultiplyTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipMultiplyTextureTransform');
        GdipTranslateTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipTranslateTextureTransform');
        GdipScaleTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipScaleTextureTransform');
        GdipRotateTextureTransform := GetProcAddress(hWINGDIPDLL,'GdipRotateTextureTransform');
        GdipSetTextureWrapMode := GetProcAddress(hWINGDIPDLL,'GdipSetTextureWrapMode');
        GdipGetTextureWrapMode := GetProcAddress(hWINGDIPDLL,'GdipGetTextureWrapMode');
        GdipGetTextureImage := GetProcAddress(hWINGDIPDLL,'GdipGetTextureImage');
        GdipCreateSolidFill := GetProcAddress(hWINGDIPDLL,'GdipCreateSolidFill');
        GdipSetSolidFillColor := GetProcAddress(hWINGDIPDLL,'GdipSetSolidFillColor');
        GdipGetSolidFillColor := GetProcAddress(hWINGDIPDLL,'GdipGetSolidFillColor');
        GdipCreateLineBrush := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrush');
        GdipCreateLineBrushI := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrushI');
        GdipCreateLineBrushFromRect := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrushFromRect');
        GdipCreateLineBrushFromRectI := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrushFromRectI');
        GdipCreateLineBrushFromRectWithAngle := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrushFromRectWithAngle');
        GdipCreateLineBrushFromRectWithAngleI := GetProcAddress(hWINGDIPDLL,'GdipCreateLineBrushFromRectWithAngleI');
        GdipSetLineColors := GetProcAddress(hWINGDIPDLL,'GdipSetLineColors');
        GdipGetLineColors := GetProcAddress(hWINGDIPDLL,'GdipGetLineColors');
        GdipGetLineRect := GetProcAddress(hWINGDIPDLL,'GdipGetLineRect');
        GdipGetLineRectI := GetProcAddress(hWINGDIPDLL,'GdipGetLineRectI');
        GdipSetLineGammaCorrection := GetProcAddress(hWINGDIPDLL,'GdipSetLineGammaCorrection');
        GdipGetLineGammaCorrection := GetProcAddress(hWINGDIPDLL,'GdipGetLineGammaCorrection');
        GdipGetLineBlendCount := GetProcAddress(hWINGDIPDLL,'GdipGetLineBlendCount');
        GdipGetLineBlend := GetProcAddress(hWINGDIPDLL,'GdipGetLineBlend');
        GdipSetLineBlend := GetProcAddress(hWINGDIPDLL,'GdipSetLineBlend');
        GdipGetLinePresetBlendCount := GetProcAddress(hWINGDIPDLL,'GdipGetLinePresetBlendCount');
        GdipGetLinePresetBlend := GetProcAddress(hWINGDIPDLL,'GdipGetLinePresetBlend');
        GdipSetLinePresetBlend := GetProcAddress(hWINGDIPDLL,'GdipSetLinePresetBlend');
        GdipSetLineSigmaBlend := GetProcAddress(hWINGDIPDLL,'GdipSetLineSigmaBlend');
        GdipSetLineLinearBlend := GetProcAddress(hWINGDIPDLL,'GdipSetLineLinearBlend');
        GdipSetLineWrapMode := GetProcAddress(hWINGDIPDLL,'GdipSetLineWrapMode');
        GdipGetLineWrapMode := GetProcAddress(hWINGDIPDLL,'GdipGetLineWrapMode');
        GdipGetLineTransform := GetProcAddress(hWINGDIPDLL,'GdipGetLineTransform');
        GdipSetLineTransform := GetProcAddress(hWINGDIPDLL,'GdipSetLineTransform');
        GdipResetLineTransform := GetProcAddress(hWINGDIPDLL,'GdipResetLineTransform');
        GdipMultiplyLineTransform := GetProcAddress(hWINGDIPDLL,'GdipMultiplyLineTransform');
        GdipTranslateLineTransform := GetProcAddress(hWINGDIPDLL,'GdipTranslateLineTransform');
        GdipScaleLineTransform := GetProcAddress(hWINGDIPDLL,'GdipScaleLineTransform');
        GdipRotateLineTransform := GetProcAddress(hWINGDIPDLL,'GdipRotateLineTransform');
        GdipCreatePathGradient := GetProcAddress(hWINGDIPDLL,'GdipCreatePathGradient');
        GdipCreatePathGradientI := GetProcAddress(hWINGDIPDLL,'GdipCreatePathGradientI');
        GdipCreatePathGradientFromPath := GetProcAddress(hWINGDIPDLL,'GdipCreatePathGradientFromPath');
        GdipGetPathGradientCenterColor := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientCenterColor');
        GdipSetPathGradientCenterColor := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientCenterColor');
        GdipGetPathGradientSurroundColorsWithCount := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientSurroundColorsWithCount');
        GdipSetPathGradientSurroundColorsWithCount := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientSurroundColorsWithCount');
        GdipGetPathGradientPath := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientPath');
        GdipSetPathGradientPath := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientPath');
        GdipGetPathGradientCenterPoint := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientCenterPoint');
        GdipGetPathGradientCenterPointI := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientCenterPointI');
        GdipSetPathGradientCenterPoint := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientCenterPoint');
        GdipSetPathGradientCenterPointI := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientCenterPointI');
        GdipGetPathGradientRect := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientRect');
        GdipGetPathGradientRectI := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientRectI');
        GdipGetPathGradientPointCount := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientPointCount');
        GdipGetPathGradientSurroundColorCount := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientSurroundColorCount');
        GdipSetPathGradientGammaCorrection := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientGammaCorrection');
        GdipGetPathGradientGammaCorrection := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientGammaCorrection');
        GdipGetPathGradientBlendCount := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientBlendCount');
        GdipGetPathGradientBlend := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientBlend');
        GdipSetPathGradientBlend := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientBlend');
        GdipGetPathGradientPresetBlendCount := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientPresetBlendCount');
        GdipGetPathGradientPresetBlend := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientPresetBlend');
        GdipSetPathGradientPresetBlend := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientPresetBlend');
        GdipSetPathGradientSigmaBlend := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientSigmaBlend');
        GdipSetPathGradientLinearBlend := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientLinearBlend');
        GdipGetPathGradientWrapMode := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientWrapMode');
        GdipSetPathGradientWrapMode := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientWrapMode');
        GdipGetPathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientTransform');
        GdipSetPathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientTransform');
        GdipResetPathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipResetPathGradientTransform');
        GdipMultiplyPathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipMultiplyPathGradientTransform');
        GdipTranslatePathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipTranslatePathGradientTransform');
        GdipScalePathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipScalePathGradientTransform');
        GdipRotatePathGradientTransform := GetProcAddress(hWINGDIPDLL,'GdipRotatePathGradientTransform');
        GdipGetPathGradientFocusScales := GetProcAddress(hWINGDIPDLL,'GdipGetPathGradientFocusScales');
        GdipSetPathGradientFocusScales := GetProcAddress(hWINGDIPDLL,'GdipSetPathGradientFocusScales');
        GdipCreatePen1 := GetProcAddress(hWINGDIPDLL,'GdipCreatePen1');
        GdipCreatePen2 := GetProcAddress(hWINGDIPDLL,'GdipCreatePen2');
        GdipClonePen := GetProcAddress(hWINGDIPDLL,'GdipClonePen');
        GdipDeletePen := GetProcAddress(hWINGDIPDLL,'GdipDeletePen');
        GdipSetPenWidth := GetProcAddress(hWINGDIPDLL,'GdipSetPenWidth');
        GdipGetPenWidth := GetProcAddress(hWINGDIPDLL,'GdipGetPenWidth');
        GdipSetPenUnit := GetProcAddress(hWINGDIPDLL,'GdipSetPenUnit');
        GdipGetPenUnit := GetProcAddress(hWINGDIPDLL,'GdipGetPenUnit');
        GdipSetPenLineCap197819 := GetProcAddress(hWINGDIPDLL,'GdipSetPenLineCap197819');
        GdipSetPenStartCap := GetProcAddress(hWINGDIPDLL,'GdipSetPenStartCap');
        GdipSetPenEndCap := GetProcAddress(hWINGDIPDLL,'GdipSetPenEndCap');
        GdipSetPenDashCap197819 := GetProcAddress(hWINGDIPDLL,'GdipSetPenDashCap197819');
        GdipGetPenStartCap := GetProcAddress(hWINGDIPDLL,'GdipGetPenStartCap');
        GdipGetPenEndCap := GetProcAddress(hWINGDIPDLL,'GdipGetPenEndCap');
        GdipGetPenDashCap197819 := GetProcAddress(hWINGDIPDLL,'GdipGetPenDashCap197819');
        GdipSetPenLineJoin := GetProcAddress(hWINGDIPDLL,'GdipSetPenLineJoin');
        GdipGetPenLineJoin := GetProcAddress(hWINGDIPDLL,'GdipGetPenLineJoin');
        GdipSetPenCustomStartCap := GetProcAddress(hWINGDIPDLL,'GdipSetPenCustomStartCap');
        GdipGetPenCustomStartCap := GetProcAddress(hWINGDIPDLL,'GdipGetPenCustomStartCap');
        GdipSetPenCustomEndCap := GetProcAddress(hWINGDIPDLL,'GdipSetPenCustomEndCap');
        GdipGetPenCustomEndCap := GetProcAddress(hWINGDIPDLL,'GdipGetPenCustomEndCap');
        GdipSetPenMiterLimit := GetProcAddress(hWINGDIPDLL,'GdipSetPenMiterLimit');
        GdipGetPenMiterLimit := GetProcAddress(hWINGDIPDLL,'GdipGetPenMiterLimit');
        GdipSetPenMode := GetProcAddress(hWINGDIPDLL,'GdipSetPenMode');
        GdipGetPenMode := GetProcAddress(hWINGDIPDLL,'GdipGetPenMode');
        GdipSetPenTransform := GetProcAddress(hWINGDIPDLL,'GdipSetPenTransform');
        GdipGetPenTransform := GetProcAddress(hWINGDIPDLL,'GdipGetPenTransform');
        GdipResetPenTransform := GetProcAddress(hWINGDIPDLL,'GdipResetPenTransform');
        GdipMultiplyPenTransform := GetProcAddress(hWINGDIPDLL,'GdipMultiplyPenTransform');
        GdipTranslatePenTransform := GetProcAddress(hWINGDIPDLL,'GdipTranslatePenTransform');
        GdipScalePenTransform := GetProcAddress(hWINGDIPDLL,'GdipScalePenTransform');
        GdipRotatePenTransform := GetProcAddress(hWINGDIPDLL,'GdipRotatePenTransform');
        GdipSetPenColor := GetProcAddress(hWINGDIPDLL,'GdipSetPenColor');
        GdipGetPenColor := GetProcAddress(hWINGDIPDLL,'GdipGetPenColor');
        GdipSetPenBrushFill := GetProcAddress(hWINGDIPDLL,'GdipSetPenBrushFill');
        GdipGetPenBrushFill := GetProcAddress(hWINGDIPDLL,'GdipGetPenBrushFill');
        GdipGetPenFillType := GetProcAddress(hWINGDIPDLL,'GdipGetPenFillType');
        GdipGetPenDashStyle := GetProcAddress(hWINGDIPDLL,'GdipGetPenDashStyle');
        GdipSetPenDashStyle := GetProcAddress(hWINGDIPDLL,'GdipSetPenDashStyle');
        GdipGetPenDashOffset := GetProcAddress(hWINGDIPDLL,'GdipGetPenDashOffset');
        GdipSetPenDashOffset := GetProcAddress(hWINGDIPDLL,'GdipSetPenDashOffset');
        GdipGetPenDashCount := GetProcAddress(hWINGDIPDLL,'GdipGetPenDashCount');
        GdipSetPenDashArray := GetProcAddress(hWINGDIPDLL,'GdipSetPenDashArray');
        GdipGetPenDashArray := GetProcAddress(hWINGDIPDLL,'GdipGetPenDashArray');
        GdipGetPenCompoundCount := GetProcAddress(hWINGDIPDLL,'GdipGetPenCompoundCount');
        GdipSetPenCompoundArray := GetProcAddress(hWINGDIPDLL,'GdipSetPenCompoundArray');
        GdipGetPenCompoundArray := GetProcAddress(hWINGDIPDLL,'GdipGetPenCompoundArray');
        GdipCreateCustomLineCap := GetProcAddress(hWINGDIPDLL,'GdipCreateCustomLineCap');
        GdipDeleteCustomLineCap := GetProcAddress(hWINGDIPDLL,'GdipDeleteCustomLineCap');
        GdipCloneCustomLineCap := GetProcAddress(hWINGDIPDLL,'GdipCloneCustomLineCap');
        GdipGetCustomLineCapType := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapType');
        GdipSetCustomLineCapStrokeCaps := GetProcAddress(hWINGDIPDLL,'GdipSetCustomLineCapStrokeCaps');
        GdipGetCustomLineCapStrokeCaps := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapStrokeCaps');
        GdipSetCustomLineCapStrokeJoin := GetProcAddress(hWINGDIPDLL,'GdipSetCustomLineCapStrokeJoin');
        GdipGetCustomLineCapStrokeJoin := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapStrokeJoin');
        GdipSetCustomLineCapBaseCap := GetProcAddress(hWINGDIPDLL,'GdipSetCustomLineCapBaseCap');
        GdipGetCustomLineCapBaseCap := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapBaseCap');
        GdipSetCustomLineCapBaseInset := GetProcAddress(hWINGDIPDLL,'GdipSetCustomLineCapBaseInset');
        GdipGetCustomLineCapBaseInset := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapBaseInset');
        GdipSetCustomLineCapWidthScale := GetProcAddress(hWINGDIPDLL,'GdipSetCustomLineCapWidthScale');
        GdipGetCustomLineCapWidthScale := GetProcAddress(hWINGDIPDLL,'GdipGetCustomLineCapWidthScale');
        GdipCreateAdjustableArrowCap := GetProcAddress(hWINGDIPDLL,'GdipCreateAdjustableArrowCap');
        GdipSetAdjustableArrowCapHeight := GetProcAddress(hWINGDIPDLL,'GdipSetAdjustableArrowCapHeight');
        GdipGetAdjustableArrowCapHeight := GetProcAddress(hWINGDIPDLL,'GdipGetAdjustableArrowCapHeight');
        GdipSetAdjustableArrowCapWidth := GetProcAddress(hWINGDIPDLL,'GdipSetAdjustableArrowCapWidth');
        GdipGetAdjustableArrowCapWidth := GetProcAddress(hWINGDIPDLL,'GdipGetAdjustableArrowCapWidth');
        GdipSetAdjustableArrowCapMiddleInset := GetProcAddress(hWINGDIPDLL,'GdipSetAdjustableArrowCapMiddleInset');
        GdipGetAdjustableArrowCapMiddleInset := GetProcAddress(hWINGDIPDLL,'GdipGetAdjustableArrowCapMiddleInset');
        GdipSetAdjustableArrowCapFillState := GetProcAddress(hWINGDIPDLL,'GdipSetAdjustableArrowCapFillState');
        GdipGetAdjustableArrowCapFillState := GetProcAddress(hWINGDIPDLL,'GdipGetAdjustableArrowCapFillState');
        GdipLoadImageFromStream := GetProcAddress(hWINGDIPDLL,'GdipLoadImageFromStream');
        GdipLoadImageFromFile := GetProcAddress(hWINGDIPDLL,'GdipLoadImageFromFile');
        GdipLoadImageFromStreamICM := GetProcAddress(hWINGDIPDLL,'GdipLoadImageFromStreamICM');
        GdipLoadImageFromFileICM := GetProcAddress(hWINGDIPDLL,'GdipLoadImageFromFileICM');
        GdipCloneImage := GetProcAddress(hWINGDIPDLL,'GdipCloneImage');
        GdipDisposeImage := GetProcAddress(hWINGDIPDLL,'GdipDisposeImage');
        GdipSaveImageToFile := GetProcAddress(hWINGDIPDLL,'GdipSaveImageToFile');
        GdipSaveImageToStream := GetProcAddress(hWINGDIPDLL,'GdipSaveImageToStream');
        GdipSaveAdd := GetProcAddress(hWINGDIPDLL,'GdipSaveAdd');
        GdipSaveAddImage := GetProcAddress(hWINGDIPDLL,'GdipSaveAddImage');
        GdipGetImageGraphicsContext := GetProcAddress(hWINGDIPDLL,'GdipGetImageGraphicsContext');
        GdipGetImageBounds := GetProcAddress(hWINGDIPDLL,'GdipGetImageBounds');
        GdipGetImageDimension := GetProcAddress(hWINGDIPDLL,'GdipGetImageDimension');
        GdipGetImageType := GetProcAddress(hWINGDIPDLL,'GdipGetImageType');
        GdipGetImageWidth := GetProcAddress(hWINGDIPDLL,'GdipGetImageWidth');
        GdipGetImageHeight := GetProcAddress(hWINGDIPDLL,'GdipGetImageHeight');
        GdipGetImageHorizontalResolution := GetProcAddress(hWINGDIPDLL,'GdipGetImageHorizontalResolution');
        GdipGetImageVerticalResolution := GetProcAddress(hWINGDIPDLL,'GdipGetImageVerticalResolution');
        GdipGetImageFlags := GetProcAddress(hWINGDIPDLL,'GdipGetImageFlags');
        GdipGetImageRawFormat := GetProcAddress(hWINGDIPDLL,'GdipGetImageRawFormat');
        GdipGetImagePixelFormat := GetProcAddress(hWINGDIPDLL,'GdipGetImagePixelFormat');
        GdipGetImageThumbnail := GetProcAddress(hWINGDIPDLL,'GdipGetImageThumbnail');
        GdipGetEncoderParameterListSize := GetProcAddress(hWINGDIPDLL,'GdipGetEncoderParameterListSize');
        GdipGetEncoderParameterList := GetProcAddress(hWINGDIPDLL,'GdipGetEncoderParameterList');
        GdipImageGetFrameDimensionsCount := GetProcAddress(hWINGDIPDLL,'GdipImageGetFrameDimensionsCount');
        GdipImageGetFrameDimensionsList := GetProcAddress(hWINGDIPDLL,'GdipImageGetFrameDimensionsList');
        GdipImageGetFrameCount := GetProcAddress(hWINGDIPDLL,'GdipImageGetFrameCount');
        GdipImageSelectActiveFrame := GetProcAddress(hWINGDIPDLL,'GdipImageSelectActiveFrame');
        GdipImageRotateFlip := GetProcAddress(hWINGDIPDLL,'GdipImageRotateFlip');
        GdipGetImagePalette := GetProcAddress(hWINGDIPDLL,'GdipGetImagePalette');
        GdipSetImagePalette := GetProcAddress(hWINGDIPDLL,'GdipSetImagePalette');
        GdipGetImagePaletteSize := GetProcAddress(hWINGDIPDLL,'GdipGetImagePaletteSize');
        GdipGetPropertyCount := GetProcAddress(hWINGDIPDLL,'GdipGetPropertyCount');
        GdipGetPropertyIdList := GetProcAddress(hWINGDIPDLL,'GdipGetPropertyIdList');
        GdipGetPropertyItemSize := GetProcAddress(hWINGDIPDLL,'GdipGetPropertyItemSize');
        GdipGetPropertyItem := GetProcAddress(hWINGDIPDLL,'GdipGetPropertyItem');
        GdipGetPropertySize := GetProcAddress(hWINGDIPDLL,'GdipGetPropertySize');
        GdipGetAllPropertyItems := GetProcAddress(hWINGDIPDLL,'GdipGetAllPropertyItems');
        GdipRemovePropertyItem := GetProcAddress(hWINGDIPDLL,'GdipRemovePropertyItem');
        GdipSetPropertyItem := GetProcAddress(hWINGDIPDLL,'GdipSetPropertyItem');
        GdipImageForceValidation := GetProcAddress(hWINGDIPDLL,'GdipImageForceValidation');
        GdipCreateBitmapFromStream := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromStream');
        GdipCreateBitmapFromFile := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromFile');
        GdipCreateBitmapFromStreamICM := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromStreamICM');
        GdipCreateBitmapFromFileICM := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromFileICM');
        GdipCreateBitmapFromScan0 := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromScan0');
        GdipCreateBitmapFromGraphics := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromGraphics');
        GdipCreateBitmapFromDirectDrawSurface := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromDirectDrawSurface');
        GdipCreateBitmapFromGdiDib := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromGdiDib');
        GdipCreateBitmapFromHBITMAP := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromHBITMAP');
        GdipCreateHBITMAPFromBitmap := GetProcAddress(hWINGDIPDLL,'GdipCreateHBITMAPFromBitmap');
        GdipCreateBitmapFromHICON := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromHICON');
        GdipCreateHICONFromBitmap := GetProcAddress(hWINGDIPDLL,'GdipCreateHICONFromBitmap');
        GdipCreateBitmapFromResource := GetProcAddress(hWINGDIPDLL,'GdipCreateBitmapFromResource');
        GdipCloneBitmapArea := GetProcAddress(hWINGDIPDLL,'GdipCloneBitmapArea');
        GdipCloneBitmapAreaI := GetProcAddress(hWINGDIPDLL,'GdipCloneBitmapAreaI');
        GdipBitmapLockBits := GetProcAddress(hWINGDIPDLL,'GdipBitmapLockBits');
        GdipBitmapUnlockBits := GetProcAddress(hWINGDIPDLL,'GdipBitmapUnlockBits');
        GdipBitmapGetPixel := GetProcAddress(hWINGDIPDLL,'GdipBitmapGetPixel');
        GdipBitmapSetPixel := GetProcAddress(hWINGDIPDLL,'GdipBitmapSetPixel');
        GdipBitmapSetResolution := GetProcAddress(hWINGDIPDLL,'GdipBitmapSetResolution');
        GdipCreateImageAttributes := GetProcAddress(hWINGDIPDLL,'GdipCreateImageAttributes');
        GdipCloneImageAttributes := GetProcAddress(hWINGDIPDLL,'GdipCloneImageAttributes');
        GdipDisposeImageAttributes := GetProcAddress(hWINGDIPDLL,'GdipDisposeImageAttributes');
        GdipSetImageAttributesToIdentity := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesToIdentity');
        GdipResetImageAttributes := GetProcAddress(hWINGDIPDLL,'GdipResetImageAttributes');
        GdipSetImageAttributesColorMatrix := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesColorMatrix');
        GdipSetImageAttributesThreshold := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesThreshold');
        GdipSetImageAttributesGamma := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesGamma');
        GdipSetImageAttributesNoOp := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesNoOp');
        GdipSetImageAttributesColorKeys := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesColorKeys');
        GdipSetImageAttributesOutputChannel := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesOutputChannel');
        GdipSetImageAttributesOutputChannelColorProfile := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesOutputChannelColorProfile');
        GdipSetImageAttributesRemapTable := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesRemapTable');
        GdipSetImageAttributesWrapMode := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesWrapMode');
        GdipSetImageAttributesICMMode := GetProcAddress(hWINGDIPDLL,'GdipSetImageAttributesICMMode');
        GdipGetImageAttributesAdjustedPalette := GetProcAddress(hWINGDIPDLL,'GdipGetImageAttributesAdjustedPalette');
        GdipFlush := GetProcAddress(hWINGDIPDLL,'GdipFlush');
        GdipCreateFromHDC := GetProcAddress(hWINGDIPDLL,'GdipCreateFromHDC');
        GdipCreateFromHDC2 := GetProcAddress(hWINGDIPDLL,'GdipCreateFromHDC2');
        GdipCreateFromHWND := GetProcAddress(hWINGDIPDLL,'GdipCreateFromHWND');
        GdipCreateFromHWNDICM := GetProcAddress(hWINGDIPDLL,'GdipCreateFromHWNDICM');
        GdipDeleteGraphics := GetProcAddress(hWINGDIPDLL,'GdipDeleteGraphics');
        GdipGetDC := GetProcAddress(hWINGDIPDLL,'GdipGetDC');
        GdipReleaseDC := GetProcAddress(hWINGDIPDLL,'GdipReleaseDC');
        GdipSetCompositingMode := GetProcAddress(hWINGDIPDLL,'GdipSetCompositingMode');
        GdipGetCompositingMode := GetProcAddress(hWINGDIPDLL,'GdipGetCompositingMode');
        GdipSetRenderingOrigin := GetProcAddress(hWINGDIPDLL,'GdipSetRenderingOrigin');
        GdipGetRenderingOrigin := GetProcAddress(hWINGDIPDLL,'GdipGetRenderingOrigin');
        GdipSetCompositingQuality := GetProcAddress(hWINGDIPDLL,'GdipSetCompositingQuality');
        GdipGetCompositingQuality := GetProcAddress(hWINGDIPDLL,'GdipGetCompositingQuality');
        GdipSetSmoothingMode := GetProcAddress(hWINGDIPDLL,'GdipSetSmoothingMode');
        GdipGetSmoothingMode := GetProcAddress(hWINGDIPDLL,'GdipGetSmoothingMode');
        GdipSetPixelOffsetMode := GetProcAddress(hWINGDIPDLL,'GdipSetPixelOffsetMode');
        GdipGetPixelOffsetMode := GetProcAddress(hWINGDIPDLL,'GdipGetPixelOffsetMode');
        GdipSetTextRenderingHint := GetProcAddress(hWINGDIPDLL,'GdipSetTextRenderingHint');
        GdipGetTextRenderingHint := GetProcAddress(hWINGDIPDLL,'GdipGetTextRenderingHint');
        GdipSetTextContrast := GetProcAddress(hWINGDIPDLL,'GdipSetTextContrast');
        GdipGetTextContrast := GetProcAddress(hWINGDIPDLL,'GdipGetTextContrast');
        GdipSetInterpolationMode := GetProcAddress(hWINGDIPDLL,'GdipSetInterpolationMode');
        GdipGetInterpolationMode := GetProcAddress(hWINGDIPDLL,'GdipGetInterpolationMode');
        GdipSetWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipSetWorldTransform');
        GdipResetWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipResetWorldTransform');
        GdipMultiplyWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipMultiplyWorldTransform');
        GdipTranslateWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipTranslateWorldTransform');
        GdipScaleWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipScaleWorldTransform');
        GdipRotateWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipRotateWorldTransform');
        GdipGetWorldTransform := GetProcAddress(hWINGDIPDLL,'GdipGetWorldTransform');
        GdipResetPageTransform := GetProcAddress(hWINGDIPDLL,'GdipResetPageTransform');
        GdipGetPageUnit := GetProcAddress(hWINGDIPDLL,'GdipGetPageUnit');
        GdipGetPageScale := GetProcAddress(hWINGDIPDLL,'GdipGetPageScale');
        GdipSetPageUnit := GetProcAddress(hWINGDIPDLL,'GdipSetPageUnit');
        GdipSetPageScale := GetProcAddress(hWINGDIPDLL,'GdipSetPageScale');
        GdipGetDpiX := GetProcAddress(hWINGDIPDLL,'GdipGetDpiX');
        GdipGetDpiY := GetProcAddress(hWINGDIPDLL,'GdipGetDpiY');
        GdipTransformPoints := GetProcAddress(hWINGDIPDLL,'GdipTransformPoints');
        GdipTransformPointsI := GetProcAddress(hWINGDIPDLL,'GdipTransformPointsI');
        GdipGetNearestColor := GetProcAddress(hWINGDIPDLL,'GdipGetNearestColor');
        GdipCreateHalftonePalette := GetProcAddress(hWINGDIPDLL,'GdipCreateHalftonePalette');
        GdipDrawLine := GetProcAddress(hWINGDIPDLL,'GdipDrawLine');
        GdipDrawLineI := GetProcAddress(hWINGDIPDLL,'GdipDrawLineI');
        GdipDrawLines := GetProcAddress(hWINGDIPDLL,'GdipDrawLines');
        GdipDrawLinesI := GetProcAddress(hWINGDIPDLL,'GdipDrawLinesI');
        GdipDrawArc := GetProcAddress(hWINGDIPDLL,'GdipDrawArc');
        GdipDrawArcI := GetProcAddress(hWINGDIPDLL,'GdipDrawArcI');
        GdipDrawBezier := GetProcAddress(hWINGDIPDLL,'GdipDrawBezier');
        GdipDrawBezierI := GetProcAddress(hWINGDIPDLL,'GdipDrawBezierI');
        GdipDrawBeziers := GetProcAddress(hWINGDIPDLL,'GdipDrawBeziers');
        GdipDrawBeziersI := GetProcAddress(hWINGDIPDLL,'GdipDrawBeziersI');
        GdipDrawRectangle := GetProcAddress(hWINGDIPDLL,'GdipDrawRectangle');
        GdipDrawRectangleI := GetProcAddress(hWINGDIPDLL,'GdipDrawRectangleI');
        GdipDrawRectangles := GetProcAddress(hWINGDIPDLL,'GdipDrawRectangles');
        GdipDrawRectanglesI := GetProcAddress(hWINGDIPDLL,'GdipDrawRectanglesI');
        GdipDrawEllipse := GetProcAddress(hWINGDIPDLL,'GdipDrawEllipse');
        GdipDrawEllipseI := GetProcAddress(hWINGDIPDLL,'GdipDrawEllipseI');
        GdipDrawPie := GetProcAddress(hWINGDIPDLL,'GdipDrawPie');
        GdipDrawPieI := GetProcAddress(hWINGDIPDLL,'GdipDrawPieI');
        GdipDrawPolygon := GetProcAddress(hWINGDIPDLL,'GdipDrawPolygon');
        GdipDrawPolygonI := GetProcAddress(hWINGDIPDLL,'GdipDrawPolygonI');
        GdipDrawPath := GetProcAddress(hWINGDIPDLL,'GdipDrawPath');
        GdipDrawCurve := GetProcAddress(hWINGDIPDLL,'GdipDrawCurve');
        GdipDrawCurveI := GetProcAddress(hWINGDIPDLL,'GdipDrawCurveI');
        GdipDrawCurve2 := GetProcAddress(hWINGDIPDLL,'GdipDrawCurve2');
        GdipDrawCurve2I := GetProcAddress(hWINGDIPDLL,'GdipDrawCurve2I');
        GdipDrawCurve3 := GetProcAddress(hWINGDIPDLL,'GdipDrawCurve3');
        GdipDrawCurve3I := GetProcAddress(hWINGDIPDLL,'GdipDrawCurve3I');
        GdipDrawClosedCurve := GetProcAddress(hWINGDIPDLL,'GdipDrawClosedCurve');
        GdipDrawClosedCurveI := GetProcAddress(hWINGDIPDLL,'GdipDrawClosedCurveI');
        GdipDrawClosedCurve2 := GetProcAddress(hWINGDIPDLL,'GdipDrawClosedCurve2');
        GdipDrawClosedCurve2I := GetProcAddress(hWINGDIPDLL,'GdipDrawClosedCurve2I');
        GdipGraphicsClear := GetProcAddress(hWINGDIPDLL,'GdipGraphicsClear');
        GdipFillRectangle := GetProcAddress(hWINGDIPDLL,'GdipFillRectangle');
        GdipFillRectangleI := GetProcAddress(hWINGDIPDLL,'GdipFillRectangleI');
        GdipFillRectangles := GetProcAddress(hWINGDIPDLL,'GdipFillRectangles');
        GdipFillRectanglesI := GetProcAddress(hWINGDIPDLL,'GdipFillRectanglesI');
        GdipFillPolygon := GetProcAddress(hWINGDIPDLL,'GdipFillPolygon');
        GdipFillPolygonI := GetProcAddress(hWINGDIPDLL,'GdipFillPolygonI');
        GdipFillPolygon2 := GetProcAddress(hWINGDIPDLL,'GdipFillPolygon2');
        GdipFillPolygon2I := GetProcAddress(hWINGDIPDLL,'GdipFillPolygon2I');
        GdipFillEllipse := GetProcAddress(hWINGDIPDLL,'GdipFillEllipse');
        GdipFillEllipseI := GetProcAddress(hWINGDIPDLL,'GdipFillEllipseI');
        GdipFillPie := GetProcAddress(hWINGDIPDLL,'GdipFillPie');
        GdipFillPieI := GetProcAddress(hWINGDIPDLL,'GdipFillPieI');
        GdipFillPath := GetProcAddress(hWINGDIPDLL,'GdipFillPath');
        GdipFillClosedCurve := GetProcAddress(hWINGDIPDLL,'GdipFillClosedCurve');
        GdipFillClosedCurveI := GetProcAddress(hWINGDIPDLL,'GdipFillClosedCurveI');
        GdipFillClosedCurve2 := GetProcAddress(hWINGDIPDLL,'GdipFillClosedCurve2');
        GdipFillClosedCurve2I := GetProcAddress(hWINGDIPDLL,'GdipFillClosedCurve2I');
        GdipFillRegion := GetProcAddress(hWINGDIPDLL,'GdipFillRegion');
        GdipDrawImage := GetProcAddress(hWINGDIPDLL,'GdipDrawImage');
        GdipDrawImageI := GetProcAddress(hWINGDIPDLL,'GdipDrawImageI');
        GdipDrawImageRect := GetProcAddress(hWINGDIPDLL,'GdipDrawImageRect');
        GdipDrawImageRectI := GetProcAddress(hWINGDIPDLL,'GdipDrawImageRectI');
        GdipDrawImagePoints := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePoints');
        GdipDrawImagePointsI := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePointsI');
        GdipDrawImagePointRect := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePointRect');
        GdipDrawImagePointRectI := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePointRectI');
        GdipDrawImageRectRect := GetProcAddress(hWINGDIPDLL,'GdipDrawImageRectRect');
        GdipDrawImageRectRectI := GetProcAddress(hWINGDIPDLL,'GdipDrawImageRectRectI');
        GdipDrawImagePointsRect := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePointsRect');
        GdipDrawImagePointsRectI := GetProcAddress(hWINGDIPDLL,'GdipDrawImagePointsRectI');
        GdipEnumerateMetafileDestPoint := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestPoint');
        GdipEnumerateMetafileDestPointI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestPointI');
        GdipEnumerateMetafileDestRect := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestRect');
        GdipEnumerateMetafileDestRectI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestRectI');
        GdipEnumerateMetafileDestPoints := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestPoints');
        GdipEnumerateMetafileDestPointsI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileDestPointsI');
        GdipEnumerateMetafileSrcRectDestPoint := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestPoint');
        GdipEnumerateMetafileSrcRectDestPointI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestPointI');
        GdipEnumerateMetafileSrcRectDestRect := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestRect');
        GdipEnumerateMetafileSrcRectDestRectI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestRectI');
        GdipEnumerateMetafileSrcRectDestPoints := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestPoints');
        GdipEnumerateMetafileSrcRectDestPointsI := GetProcAddress(hWINGDIPDLL,'GdipEnumerateMetafileSrcRectDestPointsI');
        GdipPlayMetafileRecord := GetProcAddress(hWINGDIPDLL,'GdipPlayMetafileRecord');
        GdipSetClipGraphics := GetProcAddress(hWINGDIPDLL,'GdipSetClipGraphics');
        GdipSetClipRect := GetProcAddress(hWINGDIPDLL,'GdipSetClipRect');
        GdipSetClipRectI := GetProcAddress(hWINGDIPDLL,'GdipSetClipRectI');
        GdipSetClipPath := GetProcAddress(hWINGDIPDLL,'GdipSetClipPath');
        GdipSetClipRegion := GetProcAddress(hWINGDIPDLL,'GdipSetClipRegion');
        GdipSetClipHrgn := GetProcAddress(hWINGDIPDLL,'GdipSetClipHrgn');
        GdipResetClip := GetProcAddress(hWINGDIPDLL,'GdipResetClip');
        GdipTranslateClip := GetProcAddress(hWINGDIPDLL,'GdipTranslateClip');
        GdipTranslateClipI := GetProcAddress(hWINGDIPDLL,'GdipTranslateClipI');
        GdipGetClip := GetProcAddress(hWINGDIPDLL,'GdipGetClip');
        GdipGetClipBounds := GetProcAddress(hWINGDIPDLL,'GdipGetClipBounds');
        GdipGetClipBoundsI := GetProcAddress(hWINGDIPDLL,'GdipGetClipBoundsI');
        GdipIsClipEmpty := GetProcAddress(hWINGDIPDLL,'GdipIsClipEmpty');
        GdipGetVisibleClipBounds := GetProcAddress(hWINGDIPDLL,'GdipGetVisibleClipBounds');
        GdipGetVisibleClipBoundsI := GetProcAddress(hWINGDIPDLL,'GdipGetVisibleClipBoundsI');
        GdipIsVisibleClipEmpty := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleClipEmpty');
        GdipIsVisiblePoint := GetProcAddress(hWINGDIPDLL,'GdipIsVisiblePoint');
        GdipIsVisiblePointI := GetProcAddress(hWINGDIPDLL,'GdipIsVisiblePointI');
        GdipIsVisibleRect := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRect');
        GdipIsVisibleRectI := GetProcAddress(hWINGDIPDLL,'GdipIsVisibleRectI');
        GdipSaveGraphics := GetProcAddress(hWINGDIPDLL,'GdipSaveGraphics');
        GdipRestoreGraphics := GetProcAddress(hWINGDIPDLL,'GdipRestoreGraphics');
        GdipBeginContainer := GetProcAddress(hWINGDIPDLL,'GdipBeginContainer');
        GdipBeginContainerI := GetProcAddress(hWINGDIPDLL,'GdipBeginContainerI');
        GdipBeginContainer2 := GetProcAddress(hWINGDIPDLL,'GdipBeginContainer2');
        GdipEndContainer := GetProcAddress(hWINGDIPDLL,'GdipEndContainer');
        GdipGetMetafileHeaderFromWmf := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileHeaderFromWmf');
        GdipGetMetafileHeaderFromEmf := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileHeaderFromEmf');
        GdipGetMetafileHeaderFromFile := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileHeaderFromFile');
        GdipGetMetafileHeaderFromStream := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileHeaderFromStream');
        GdipGetMetafileHeaderFromMetafile := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileHeaderFromMetafile');
        GdipGetHemfFromMetafile := GetProcAddress(hWINGDIPDLL,'GdipGetHemfFromMetafile');
        GdipCreateStreamOnFile := GetProcAddress(hWINGDIPDLL,'GdipCreateStreamOnFile');
        GdipCreateMetafileFromWmf := GetProcAddress(hWINGDIPDLL,'GdipCreateMetafileFromWmf');
        GdipCreateMetafileFromEmf := GetProcAddress(hWINGDIPDLL,'GdipCreateMetafileFromEmf');
        GdipCreateMetafileFromFile := GetProcAddress(hWINGDIPDLL,'GdipCreateMetafileFromFile');
        GdipCreateMetafileFromWmfFile := GetProcAddress(hWINGDIPDLL,'GdipCreateMetafileFromWmfFile');
        GdipCreateMetafileFromStream := GetProcAddress(hWINGDIPDLL,'GdipCreateMetafileFromStream');
        GdipRecordMetafile := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafile');
        GdipRecordMetafileI := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafileI');
        GdipRecordMetafileFileName := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafileFileName');
        GdipRecordMetafileFileNameI := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafileFileNameI');
        GdipRecordMetafileStream := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafileStream');
        GdipRecordMetafileStreamI := GetProcAddress(hWINGDIPDLL,'GdipRecordMetafileStreamI');
        GdipSetMetafileDownLevelRasterizationLimit := GetProcAddress(hWINGDIPDLL,'GdipSetMetafileDownLevelRasterizationLimit');
        GdipGetMetafileDownLevelRasterizationLimit := GetProcAddress(hWINGDIPDLL,'GdipGetMetafileDownLevelRasterizationLimit');
        GdipGetImageDecodersSize := GetProcAddress(hWINGDIPDLL,'GdipGetImageDecodersSize');
        GdipGetImageDecoders := GetProcAddress(hWINGDIPDLL,'GdipGetImageDecoders');
        GdipGetImageEncodersSize := GetProcAddress(hWINGDIPDLL,'GdipGetImageEncodersSize');
        GdipGetImageEncoders := GetProcAddress(hWINGDIPDLL,'GdipGetImageEncoders');
        GdipComment := GetProcAddress(hWINGDIPDLL,'GdipComment');
        GdipCreateFontFamilyFromName := GetProcAddress(hWINGDIPDLL,'GdipCreateFontFamilyFromName');
        GdipDeleteFontFamily := GetProcAddress(hWINGDIPDLL,'GdipDeleteFontFamily');
        GdipCloneFontFamily := GetProcAddress(hWINGDIPDLL,'GdipCloneFontFamily');
        GdipGetGenericFontFamilySansSerif := GetProcAddress(hWINGDIPDLL,'GdipGetGenericFontFamilySansSerif');
        GdipGetGenericFontFamilySerif := GetProcAddress(hWINGDIPDLL,'GdipGetGenericFontFamilySerif');
        GdipGetGenericFontFamilyMonospace := GetProcAddress(hWINGDIPDLL,'GdipGetGenericFontFamilyMonospace');
        GdipGetFamilyName := GetProcAddress(hWINGDIPDLL,'GdipGetFamilyName');
        GdipIsStyleAvailable := GetProcAddress(hWINGDIPDLL,'GdipIsStyleAvailable');
        GdipFontCollectionEnumerable := GetProcAddress(hWINGDIPDLL,'GdipFontCollectionEnumerable');
        GdipFontCollectionEnumerate := GetProcAddress(hWINGDIPDLL,'GdipFontCollectionEnumerate');
        GdipGetEmHeight := GetProcAddress(hWINGDIPDLL,'GdipGetEmHeight');
        GdipGetCellAscent := GetProcAddress(hWINGDIPDLL,'GdipGetCellAscent');
        GdipGetCellDescent := GetProcAddress(hWINGDIPDLL,'GdipGetCellDescent');
        GdipGetLineSpacing := GetProcAddress(hWINGDIPDLL,'GdipGetLineSpacing');
        GdipCreateFontFromDC := GetProcAddress(hWINGDIPDLL,'GdipCreateFontFromDC');
        GdipCreateFontFromLogfontA := GetProcAddress(hWINGDIPDLL,'GdipCreateFontFromLogfontA');
        GdipCreateFontFromLogfontW := GetProcAddress(hWINGDIPDLL,'GdipCreateFontFromLogfontW');
        GdipCreateFont := GetProcAddress(hWINGDIPDLL,'GdipCreateFont');
        GdipCloneFont := GetProcAddress(hWINGDIPDLL,'GdipCloneFont');
        GdipDeleteFont := GetProcAddress(hWINGDIPDLL,'GdipDeleteFont');
        GdipGetFamily := GetProcAddress(hWINGDIPDLL,'GdipGetFamily');
        GdipGetFontStyle := GetProcAddress(hWINGDIPDLL,'GdipGetFontStyle');
        GdipGetFontSize := GetProcAddress(hWINGDIPDLL,'GdipGetFontSize');
        GdipGetFontUnit := GetProcAddress(hWINGDIPDLL,'GdipGetFontUnit');
        GdipGetFontHeight := GetProcAddress(hWINGDIPDLL,'GdipGetFontHeight');
        GdipGetFontHeightGivenDPI := GetProcAddress(hWINGDIPDLL,'GdipGetFontHeightGivenDPI');
        GdipGetLogFontA := GetProcAddress(hWINGDIPDLL,'GdipGetLogFontA');
        GdipGetLogFontW := GetProcAddress(hWINGDIPDLL,'GdipGetLogFontW');
        GdipNewInstalledFontCollection := GetProcAddress(hWINGDIPDLL,'GdipNewInstalledFontCollection');
        GdipNewPrivateFontCollection := GetProcAddress(hWINGDIPDLL,'GdipNewPrivateFontCollection');
        GdipDeletePrivateFontCollection := GetProcAddress(hWINGDIPDLL,'GdipDeletePrivateFontCollection');
        GdipGetFontCollectionFamilyCount := GetProcAddress(hWINGDIPDLL,'GdipGetFontCollectionFamilyCount');
        GdipGetFontCollectionFamilyList := GetProcAddress(hWINGDIPDLL,'GdipGetFontCollectionFamilyList');
        GdipPrivateAddFontFile := GetProcAddress(hWINGDIPDLL,'GdipPrivateAddFontFile');
        GdipPrivateAddMemoryFont := GetProcAddress(hWINGDIPDLL,'GdipPrivateAddMemoryFont');
        GdipDrawString := GetProcAddress(hWINGDIPDLL,'GdipDrawString');
        GdipMeasureString := GetProcAddress(hWINGDIPDLL,'GdipMeasureString');
        GdipMeasureCharacterRanges := GetProcAddress(hWINGDIPDLL,'GdipMeasureCharacterRanges');
        GdipDrawDriverString := GetProcAddress(hWINGDIPDLL,'GdipDrawDriverString');
        GdipMeasureDriverString := GetProcAddress(hWINGDIPDLL,'GdipMeasureDriverString');
        GdipCreateStringFormat := GetProcAddress(hWINGDIPDLL,'GdipCreateStringFormat');
        GdipStringFormatGetGenericDefault := GetProcAddress(hWINGDIPDLL,'GdipStringFormatGetGenericDefault');
        GdipStringFormatGetGenericTypographic := GetProcAddress(hWINGDIPDLL,'GdipStringFormatGetGenericTypographic');
        GdipDeleteStringFormat := GetProcAddress(hWINGDIPDLL,'GdipDeleteStringFormat');
        GdipCloneStringFormat := GetProcAddress(hWINGDIPDLL,'GdipCloneStringFormat');
        GdipSetStringFormatFlags := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatFlags');
        GdipGetStringFormatFlags := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatFlags');
        GdipSetStringFormatAlign := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatAlign');
        GdipGetStringFormatAlign := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatAlign');
        GdipSetStringFormatLineAlign := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatLineAlign');
        GdipGetStringFormatLineAlign := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatLineAlign');
        GdipSetStringFormatTrimming := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatTrimming');
        GdipGetStringFormatTrimming := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatTrimming');
        GdipSetStringFormatHotkeyPrefix := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatHotkeyPrefix');
        GdipGetStringFormatHotkeyPrefix := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatHotkeyPrefix');
        GdipSetStringFormatTabStops := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatTabStops');
        GdipGetStringFormatTabStops := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatTabStops');
        GdipGetStringFormatTabStopCount := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatTabStopCount');
        GdipSetStringFormatDigitSubstitution := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatDigitSubstitution');
        GdipGetStringFormatDigitSubstitution := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatDigitSubstitution');
        GdipGetStringFormatMeasurableCharacterRangeCount := GetProcAddress(hWINGDIPDLL,'GdipGetStringFormatMeasurableCharacterRangeCount');
        GdipSetStringFormatMeasurableCharacterRanges := GetProcAddress(hWINGDIPDLL,'GdipSetStringFormatMeasurableCharacterRanges');
        GdipCreateCachedBitmap := GetProcAddress(hWINGDIPDLL,'GdipCreateCachedBitmap');
        GdipDeleteCachedBitmap := GetProcAddress(hWINGDIPDLL,'GdipDeleteCachedBitmap');
        GdipDrawCachedBitmap := GetProcAddress(hWINGDIPDLL,'GdipDrawCachedBitmap');
        GdipEmfToWmfBits := GetProcAddress(hWINGDIPDLL,'GdipEmfToWmfBits');
      end;
    end;
    Result := hWINGDIPDLL > 0;
  finally
    Lock.Leave;
  end;
end;

procedure FreeGDIPLibrary;
begin
  Lock.Enter;
  try
    if ReferenceCount > 0 then Dec(ReferenceCount);
    if (hWINGDIPDLL <> 0) and (ReferenceCount = 0) then begin
      FreeLibrary(hWINGDIPDLL);
      hWINGDIPDLL := 0;
      GdipAlloc :=  nil;
      GdipFree :=  nil;
      GdiplusStartup :=  nil;
      GdiplusShutdown :=  nil;
      GdipCreatePath :=  nil;
      GdipCreatePath2 :=  nil;
      GdipCreatePath2I :=  nil;
      GdipClonePath :=  nil;
      GdipDeletePath :=  nil;
      GdipResetPath :=  nil;
      GdipGetPointCount :=  nil;
      GdipGetPathTypes :=  nil;
      GdipGetPathPoints :=  nil;
      GdipGetPathPointsI :=  nil;
      GdipGetPathFillMode :=  nil;
      GdipSetPathFillMode :=  nil;
      GdipGetPathData :=  nil;
      GdipStartPathFigure :=  nil;
      GdipClosePathFigure :=  nil;
      GdipClosePathFigures :=  nil;
      GdipSetPathMarker :=  nil;
      GdipClearPathMarkers :=  nil;
      GdipReversePath :=  nil;
      GdipGetPathLastPoint :=  nil;
      GdipAddPathLine :=  nil;
      GdipAddPathLine2 :=  nil;
      GdipAddPathArc :=  nil;
      GdipAddPathBezier :=  nil;
      GdipAddPathBeziers :=  nil;
      GdipAddPathCurve :=  nil;
      GdipAddPathCurve2 :=  nil;
      GdipAddPathCurve3 :=  nil;
      GdipAddPathClosedCurve :=  nil;
      GdipAddPathClosedCurve2 :=  nil;
      GdipAddPathRectangle :=  nil;
      GdipAddPathRectangles :=  nil;
      GdipAddPathEllipse :=  nil;
      GdipAddPathPie :=  nil;
      GdipAddPathPolygon :=  nil;
      GdipAddPathPath :=  nil;
      GdipAddPathString :=  nil;
      GdipAddPathStringI :=  nil;
      GdipAddPathLineI :=  nil;
      GdipAddPathLine2I :=  nil;
      GdipAddPathArcI :=  nil;
      GdipAddPathBezierI :=  nil;
      GdipAddPathBeziersI :=  nil;
      GdipAddPathCurveI :=  nil;
      GdipAddPathCurve2I :=  nil;
      GdipAddPathCurve3I :=  nil;
      GdipAddPathClosedCurveI :=  nil;
      GdipAddPathClosedCurve2I :=  nil;
      GdipAddPathRectangleI :=  nil;
      GdipAddPathRectanglesI :=  nil;
      GdipAddPathEllipseI :=  nil;
      GdipAddPathPieI :=  nil;
      GdipAddPathPolygonI :=  nil;
      GdipFlattenPath :=  nil;
      GdipWindingModeOutline :=  nil;
      GdipWidenPath :=  nil;
      GdipWarpPath :=  nil;
      GdipTransformPath :=  nil;
      GdipGetPathWorldBounds :=  nil;
      GdipGetPathWorldBoundsI :=  nil;
      GdipIsVisiblePathPoint :=  nil;
      GdipIsVisiblePathPointI :=  nil;
      GdipIsOutlineVisiblePathPoint :=  nil;
      GdipIsOutlineVisiblePathPointI :=  nil;
      GdipCreatePathIter :=  nil;
      GdipDeletePathIter :=  nil;
      GdipPathIterNextSubpath :=  nil;
      GdipPathIterNextSubpathPath :=  nil;
      GdipPathIterNextPathType :=  nil;
      GdipPathIterNextMarker :=  nil;
      GdipPathIterNextMarkerPath :=  nil;
      GdipPathIterGetCount :=  nil;
      GdipPathIterGetSubpathCount :=  nil;
      GdipPathIterIsValid :=  nil;
      GdipPathIterHasCurve :=  nil;
      GdipPathIterRewind :=  nil;
      GdipPathIterEnumerate :=  nil;
      GdipPathIterCopyData :=  nil;
      GdipCreateMatrix :=  nil;
      GdipCreateMatrix2 :=  nil;
      GdipCreateMatrix3 :=  nil;
      GdipCreateMatrix3I :=  nil;
      GdipCloneMatrix :=  nil;
      GdipDeleteMatrix :=  nil;
      GdipSetMatrixElements :=  nil;
      GdipMultiplyMatrix :=  nil;
      GdipTranslateMatrix :=  nil;
      GdipScaleMatrix :=  nil;
      GdipRotateMatrix :=  nil;
      GdipShearMatrix :=  nil;
      GdipInvertMatrix :=  nil;
      GdipTransformMatrixPoints :=  nil;
      GdipTransformMatrixPointsI :=  nil;
      GdipVectorTransformMatrixPoints :=  nil;
      GdipVectorTransformMatrixPointsI :=  nil;
      GdipGetMatrixElements :=  nil;
      GdipIsMatrixInvertible :=  nil;
      GdipIsMatrixIdentity :=  nil;
      GdipIsMatrixEqual :=  nil;
      GdipCreateRegion :=  nil;
      GdipCreateRegionRect :=  nil;
      GdipCreateRegionRectI :=  nil;
      GdipCreateRegionPath :=  nil;
      GdipCreateRegionRgnData :=  nil;
      GdipCreateRegionHrgn :=  nil;
      GdipCloneRegion :=  nil;
      GdipDeleteRegion :=  nil;
      GdipSetInfinite :=  nil;
      GdipSetEmpty :=  nil;
      GdipCombineRegionRect :=  nil;
      GdipCombineRegionRectI :=  nil;
      GdipCombineRegionPath :=  nil;
      GdipCombineRegionRegion :=  nil;
      GdipTranslateRegion :=  nil;
      GdipTranslateRegionI :=  nil;
      GdipTransformRegion :=  nil;
      GdipGetRegionBounds :=  nil;
      GdipGetRegionBoundsI :=  nil;
      GdipGetRegionHRgn :=  nil;
      GdipIsEmptyRegion :=  nil;
      GdipIsInfiniteRegion :=  nil;
      GdipIsEqualRegion :=  nil;
      GdipGetRegionDataSize :=  nil;
      GdipGetRegionData :=  nil;
      GdipIsVisibleRegionPoint :=  nil;
      GdipIsVisibleRegionPointI :=  nil;
      GdipIsVisibleRegionRect :=  nil;
      GdipIsVisibleRegionRectI :=  nil;
      GdipGetRegionScansCount :=  nil;
      GdipGetRegionScans :=  nil;
      GdipGetRegionScansI :=  nil;
      GdipCloneBrush :=  nil;
      GdipDeleteBrush :=  nil;
      GdipGetBrushType :=  nil;
      GdipCreateHatchBrush :=  nil;
      GdipGetHatchStyle :=  nil;
      GdipGetHatchForegroundColor :=  nil;
      GdipGetHatchBackgroundColor :=  nil;
      GdipCreateTexture :=  nil;
      GdipCreateTexture2 :=  nil;
      GdipCreateTextureIA :=  nil;
      GdipCreateTexture2I :=  nil;
      GdipCreateTextureIAI :=  nil;
      GdipGetTextureTransform :=  nil;
      GdipSetTextureTransform :=  nil;
      GdipResetTextureTransform :=  nil;
      GdipMultiplyTextureTransform :=  nil;
      GdipTranslateTextureTransform :=  nil;
      GdipScaleTextureTransform :=  nil;
      GdipRotateTextureTransform :=  nil;
      GdipSetTextureWrapMode :=  nil;
      GdipGetTextureWrapMode :=  nil;
      GdipGetTextureImage :=  nil;
      GdipCreateSolidFill :=  nil;
      GdipSetSolidFillColor :=  nil;
      GdipGetSolidFillColor :=  nil;
      GdipCreateLineBrush :=  nil;
      GdipCreateLineBrushI :=  nil;
      GdipCreateLineBrushFromRect :=  nil;
      GdipCreateLineBrushFromRectI :=  nil;
      GdipCreateLineBrushFromRectWithAngle :=  nil;
      GdipCreateLineBrushFromRectWithAngleI :=  nil;
      GdipSetLineColors :=  nil;
      GdipGetLineColors :=  nil;
      GdipGetLineRect :=  nil;
      GdipGetLineRectI :=  nil;
      GdipSetLineGammaCorrection :=  nil;
      GdipGetLineGammaCorrection :=  nil;
      GdipGetLineBlendCount :=  nil;
      GdipGetLineBlend :=  nil;
      GdipSetLineBlend :=  nil;
      GdipGetLinePresetBlendCount :=  nil;
      GdipGetLinePresetBlend :=  nil;
      GdipSetLinePresetBlend :=  nil;
      GdipSetLineSigmaBlend :=  nil;
      GdipSetLineLinearBlend :=  nil;
      GdipSetLineWrapMode :=  nil;
      GdipGetLineWrapMode :=  nil;
      GdipGetLineTransform :=  nil;
      GdipSetLineTransform :=  nil;
      GdipResetLineTransform :=  nil;
      GdipMultiplyLineTransform :=  nil;
      GdipTranslateLineTransform :=  nil;
      GdipScaleLineTransform :=  nil;
      GdipRotateLineTransform :=  nil;
      GdipCreatePathGradient :=  nil;
      GdipCreatePathGradientI :=  nil;
      GdipCreatePathGradientFromPath :=  nil;
      GdipGetPathGradientCenterColor :=  nil;
      GdipSetPathGradientCenterColor :=  nil;
      GdipGetPathGradientSurroundColorsWithCount :=  nil;
      GdipSetPathGradientSurroundColorsWithCount :=  nil;
      GdipGetPathGradientPath :=  nil;
      GdipSetPathGradientPath :=  nil;
      GdipGetPathGradientCenterPoint :=  nil;
      GdipGetPathGradientCenterPointI :=  nil;
      GdipSetPathGradientCenterPoint :=  nil;
      GdipSetPathGradientCenterPointI :=  nil;
      GdipGetPathGradientRect :=  nil;
      GdipGetPathGradientRectI :=  nil;
      GdipGetPathGradientPointCount :=  nil;
      GdipGetPathGradientSurroundColorCount :=  nil;
      GdipSetPathGradientGammaCorrection :=  nil;
      GdipGetPathGradientGammaCorrection :=  nil;
      GdipGetPathGradientBlendCount :=  nil;
      GdipGetPathGradientBlend :=  nil;
      GdipSetPathGradientBlend :=  nil;
      GdipGetPathGradientPresetBlendCount :=  nil;
      GdipGetPathGradientPresetBlend :=  nil;
      GdipSetPathGradientPresetBlend :=  nil;
      GdipSetPathGradientSigmaBlend :=  nil;
      GdipSetPathGradientLinearBlend :=  nil;
      GdipGetPathGradientWrapMode :=  nil;
      GdipSetPathGradientWrapMode :=  nil;
      GdipGetPathGradientTransform :=  nil;
      GdipSetPathGradientTransform :=  nil;
      GdipResetPathGradientTransform :=  nil;
      GdipMultiplyPathGradientTransform :=  nil;
      GdipTranslatePathGradientTransform :=  nil;
      GdipScalePathGradientTransform :=  nil;
      GdipRotatePathGradientTransform :=  nil;
      GdipGetPathGradientFocusScales :=  nil;
      GdipSetPathGradientFocusScales :=  nil;
      GdipCreatePen1 :=  nil;
      GdipCreatePen2 :=  nil;
      GdipClonePen :=  nil;
      GdipDeletePen :=  nil;
      GdipSetPenWidth :=  nil;
      GdipGetPenWidth :=  nil;
      GdipSetPenUnit :=  nil;
      GdipGetPenUnit :=  nil;
      GdipSetPenLineCap197819 :=  nil;
      GdipSetPenStartCap :=  nil;
      GdipSetPenEndCap :=  nil;
      GdipSetPenDashCap197819 :=  nil;
      GdipGetPenStartCap :=  nil;
      GdipGetPenEndCap :=  nil;
      GdipGetPenDashCap197819 :=  nil;
      GdipSetPenLineJoin :=  nil;
      GdipGetPenLineJoin :=  nil;
      GdipSetPenCustomStartCap :=  nil;
      GdipGetPenCustomStartCap :=  nil;
      GdipSetPenCustomEndCap :=  nil;
      GdipGetPenCustomEndCap :=  nil;
      GdipSetPenMiterLimit :=  nil;
      GdipGetPenMiterLimit :=  nil;
      GdipSetPenMode :=  nil;
      GdipGetPenMode :=  nil;
      GdipSetPenTransform :=  nil;
      GdipGetPenTransform :=  nil;
      GdipResetPenTransform :=  nil;
      GdipMultiplyPenTransform :=  nil;
      GdipTranslatePenTransform :=  nil;
      GdipScalePenTransform :=  nil;
      GdipRotatePenTransform :=  nil;
      GdipSetPenColor :=  nil;
      GdipGetPenColor :=  nil;
      GdipSetPenBrushFill :=  nil;
      GdipGetPenBrushFill :=  nil;
      GdipGetPenFillType :=  nil;
      GdipGetPenDashStyle :=  nil;
      GdipSetPenDashStyle :=  nil;
      GdipGetPenDashOffset :=  nil;
      GdipSetPenDashOffset :=  nil;
      GdipGetPenDashCount :=  nil;
      GdipSetPenDashArray :=  nil;
      GdipGetPenDashArray :=  nil;
      GdipGetPenCompoundCount :=  nil;
      GdipSetPenCompoundArray :=  nil;
      GdipGetPenCompoundArray :=  nil;
      GdipCreateCustomLineCap :=  nil;
      GdipDeleteCustomLineCap :=  nil;
      GdipCloneCustomLineCap :=  nil;
      GdipGetCustomLineCapType :=  nil;
      GdipSetCustomLineCapStrokeCaps :=  nil;
      GdipGetCustomLineCapStrokeCaps :=  nil;
      GdipSetCustomLineCapStrokeJoin :=  nil;
      GdipGetCustomLineCapStrokeJoin :=  nil;
      GdipSetCustomLineCapBaseCap :=  nil;
      GdipGetCustomLineCapBaseCap :=  nil;
      GdipSetCustomLineCapBaseInset :=  nil;
      GdipGetCustomLineCapBaseInset :=  nil;
      GdipSetCustomLineCapWidthScale :=  nil;
      GdipGetCustomLineCapWidthScale :=  nil;
      GdipCreateAdjustableArrowCap :=  nil;
      GdipSetAdjustableArrowCapHeight :=  nil;
      GdipGetAdjustableArrowCapHeight :=  nil;
      GdipSetAdjustableArrowCapWidth :=  nil;
      GdipGetAdjustableArrowCapWidth :=  nil;
      GdipSetAdjustableArrowCapMiddleInset :=  nil;
      GdipGetAdjustableArrowCapMiddleInset :=  nil;
      GdipSetAdjustableArrowCapFillState :=  nil;
      GdipGetAdjustableArrowCapFillState :=  nil;
      GdipLoadImageFromStream :=  nil;
      GdipLoadImageFromFile :=  nil;
      GdipLoadImageFromStreamICM :=  nil;
      GdipLoadImageFromFileICM :=  nil;
      GdipCloneImage :=  nil;
      GdipDisposeImage :=  nil;
      GdipSaveImageToFile :=  nil;
      GdipSaveImageToStream :=  nil;
      GdipSaveAdd :=  nil;
      GdipSaveAddImage :=  nil;
      GdipGetImageGraphicsContext :=  nil;
      GdipGetImageBounds :=  nil;
      GdipGetImageDimension :=  nil;
      GdipGetImageType :=  nil;
      GdipGetImageWidth :=  nil;
      GdipGetImageHeight :=  nil;
      GdipGetImageHorizontalResolution :=  nil;
      GdipGetImageVerticalResolution :=  nil;
      GdipGetImageFlags :=  nil;
      GdipGetImageRawFormat :=  nil;
      GdipGetImagePixelFormat :=  nil;
      GdipGetImageThumbnail :=  nil;
      GdipGetEncoderParameterListSize :=  nil;
      GdipGetEncoderParameterList :=  nil;
      GdipImageGetFrameDimensionsCount :=  nil;
      GdipImageGetFrameDimensionsList :=  nil;
      GdipImageGetFrameCount :=  nil;
      GdipImageSelectActiveFrame :=  nil;
      GdipImageRotateFlip :=  nil;
      GdipGetImagePalette :=  nil;
      GdipSetImagePalette :=  nil;
      GdipGetImagePaletteSize :=  nil;
      GdipGetPropertyCount :=  nil;
      GdipGetPropertyIdList :=  nil;
      GdipGetPropertyItemSize :=  nil;
      GdipGetPropertyItem :=  nil;
      GdipGetPropertySize :=  nil;
      GdipGetAllPropertyItems :=  nil;
      GdipRemovePropertyItem :=  nil;
      GdipSetPropertyItem :=  nil;
      GdipImageForceValidation :=  nil;
      GdipCreateBitmapFromStream :=  nil;
      GdipCreateBitmapFromFile :=  nil;
      GdipCreateBitmapFromStreamICM :=  nil;
      GdipCreateBitmapFromFileICM :=  nil;
      GdipCreateBitmapFromScan0 :=  nil;
      GdipCreateBitmapFromGraphics :=  nil;
      GdipCreateBitmapFromDirectDrawSurface :=  nil;
      GdipCreateBitmapFromGdiDib :=  nil;
      GdipCreateBitmapFromHBITMAP :=  nil;
      GdipCreateHBITMAPFromBitmap :=  nil;
      GdipCreateBitmapFromHICON :=  nil;
      GdipCreateHICONFromBitmap :=  nil;
      GdipCreateBitmapFromResource :=  nil;
      GdipCloneBitmapArea :=  nil;
      GdipCloneBitmapAreaI :=  nil;
      GdipBitmapLockBits :=  nil;
      GdipBitmapUnlockBits :=  nil;
      GdipBitmapGetPixel :=  nil;
      GdipBitmapSetPixel :=  nil;
      GdipBitmapSetResolution :=  nil;
      GdipCreateImageAttributes :=  nil;
      GdipCloneImageAttributes :=  nil;
      GdipDisposeImageAttributes :=  nil;
      GdipSetImageAttributesToIdentity :=  nil;
      GdipResetImageAttributes :=  nil;
      GdipSetImageAttributesColorMatrix :=  nil;
      GdipSetImageAttributesThreshold :=  nil;
      GdipSetImageAttributesGamma :=  nil;
      GdipSetImageAttributesNoOp :=  nil;
      GdipSetImageAttributesColorKeys :=  nil;
      GdipSetImageAttributesOutputChannel :=  nil;
      GdipSetImageAttributesOutputChannelColorProfile :=  nil;
      GdipSetImageAttributesRemapTable :=  nil;
      GdipSetImageAttributesWrapMode :=  nil;
      GdipSetImageAttributesICMMode :=  nil;
      GdipGetImageAttributesAdjustedPalette :=  nil;
      GdipFlush :=  nil;
      GdipCreateFromHDC :=  nil;
      GdipCreateFromHDC2 :=  nil;
      GdipCreateFromHWND :=  nil;
      GdipCreateFromHWNDICM :=  nil;
      GdipDeleteGraphics :=  nil;
      GdipGetDC :=  nil;
      GdipReleaseDC :=  nil;
      GdipSetCompositingMode :=  nil;
      GdipGetCompositingMode :=  nil;
      GdipSetRenderingOrigin :=  nil;
      GdipGetRenderingOrigin :=  nil;
      GdipSetCompositingQuality :=  nil;
      GdipGetCompositingQuality :=  nil;
      GdipSetSmoothingMode :=  nil;
      GdipGetSmoothingMode :=  nil;
      GdipSetPixelOffsetMode :=  nil;
      GdipGetPixelOffsetMode :=  nil;
      GdipSetTextRenderingHint :=  nil;
      GdipGetTextRenderingHint :=  nil;
      GdipSetTextContrast :=  nil;
      GdipGetTextContrast :=  nil;
      GdipSetInterpolationMode :=  nil;
      GdipGetInterpolationMode :=  nil;
      GdipSetWorldTransform :=  nil;
      GdipResetWorldTransform :=  nil;
      GdipMultiplyWorldTransform :=  nil;
      GdipTranslateWorldTransform :=  nil;
      GdipScaleWorldTransform :=  nil;
      GdipRotateWorldTransform :=  nil;
      GdipGetWorldTransform :=  nil;
      GdipResetPageTransform :=  nil;
      GdipGetPageUnit :=  nil;
      GdipGetPageScale :=  nil;
      GdipSetPageUnit :=  nil;
      GdipSetPageScale :=  nil;
      GdipGetDpiX :=  nil;
      GdipGetDpiY :=  nil;
      GdipTransformPoints :=  nil;
      GdipTransformPointsI :=  nil;
      GdipGetNearestColor :=  nil;
      GdipCreateHalftonePalette :=  nil;
      GdipDrawLine :=  nil;
      GdipDrawLineI :=  nil;
      GdipDrawLines :=  nil;
      GdipDrawLinesI :=  nil;
      GdipDrawArc :=  nil;
      GdipDrawArcI :=  nil;
      GdipDrawBezier :=  nil;
      GdipDrawBezierI :=  nil;
      GdipDrawBeziers :=  nil;
      GdipDrawBeziersI :=  nil;
      GdipDrawRectangle :=  nil;
      GdipDrawRectangleI :=  nil;
      GdipDrawRectangles :=  nil;
      GdipDrawRectanglesI :=  nil;
      GdipDrawEllipse :=  nil;
      GdipDrawEllipseI :=  nil;
      GdipDrawPie :=  nil;
      GdipDrawPieI :=  nil;
      GdipDrawPolygon :=  nil;
      GdipDrawPolygonI :=  nil;
      GdipDrawPath :=  nil;
      GdipDrawCurve :=  nil;
      GdipDrawCurveI :=  nil;
      GdipDrawCurve2 :=  nil;
      GdipDrawCurve2I :=  nil;
      GdipDrawCurve3 :=  nil;
      GdipDrawCurve3I :=  nil;
      GdipDrawClosedCurve :=  nil;
      GdipDrawClosedCurveI :=  nil;
      GdipDrawClosedCurve2 :=  nil;
      GdipDrawClosedCurve2I :=  nil;
      GdipGraphicsClear :=  nil;
      GdipFillRectangle :=  nil;
      GdipFillRectangleI :=  nil;
      GdipFillRectangles :=  nil;
      GdipFillRectanglesI :=  nil;
      GdipFillPolygon :=  nil;
      GdipFillPolygonI :=  nil;
      GdipFillPolygon2 :=  nil;
      GdipFillPolygon2I :=  nil;
      GdipFillEllipse :=  nil;
      GdipFillEllipseI :=  nil;
      GdipFillPie :=  nil;
      GdipFillPieI :=  nil;
      GdipFillPath :=  nil;
      GdipFillClosedCurve :=  nil;
      GdipFillClosedCurveI :=  nil;
      GdipFillClosedCurve2 :=  nil;
      GdipFillClosedCurve2I :=  nil;
      GdipFillRegion :=  nil;
      GdipDrawImage :=  nil;
      GdipDrawImageI :=  nil;
      GdipDrawImageRect :=  nil;
      GdipDrawImageRectI :=  nil;
      GdipDrawImagePoints :=  nil;
      GdipDrawImagePointsI :=  nil;
      GdipDrawImagePointRect :=  nil;
      GdipDrawImagePointRectI :=  nil;
      GdipDrawImageRectRect :=  nil;
      GdipDrawImageRectRectI :=  nil;
      GdipDrawImagePointsRect :=  nil;
      GdipDrawImagePointsRectI :=  nil;
      GdipEnumerateMetafileDestPoint :=  nil;
      GdipEnumerateMetafileDestPointI :=  nil;
      GdipEnumerateMetafileDestRect :=  nil;
      GdipEnumerateMetafileDestRectI :=  nil;
      GdipEnumerateMetafileDestPoints :=  nil;
      GdipEnumerateMetafileDestPointsI :=  nil;
      GdipEnumerateMetafileSrcRectDestPoint :=  nil;
      GdipEnumerateMetafileSrcRectDestPointI :=  nil;
      GdipEnumerateMetafileSrcRectDestRect :=  nil;
      GdipEnumerateMetafileSrcRectDestRectI :=  nil;
      GdipEnumerateMetafileSrcRectDestPoints :=  nil;
      GdipEnumerateMetafileSrcRectDestPointsI :=  nil;
      GdipPlayMetafileRecord :=  nil;
      GdipSetClipGraphics :=  nil;
      GdipSetClipRect :=  nil;
      GdipSetClipRectI :=  nil;
      GdipSetClipPath :=  nil;
      GdipSetClipRegion :=  nil;
      GdipSetClipHrgn :=  nil;
      GdipResetClip :=  nil;
      GdipTranslateClip :=  nil;
      GdipTranslateClipI :=  nil;
      GdipGetClip :=  nil;
      GdipGetClipBounds :=  nil;
      GdipGetClipBoundsI :=  nil;
      GdipIsClipEmpty :=  nil;
      GdipGetVisibleClipBounds :=  nil;
      GdipGetVisibleClipBoundsI :=  nil;
      GdipIsVisibleClipEmpty :=  nil;
      GdipIsVisiblePoint :=  nil;
      GdipIsVisiblePointI :=  nil;
      GdipIsVisibleRect :=  nil;
      GdipIsVisibleRectI :=  nil;
      GdipSaveGraphics :=  nil;
      GdipRestoreGraphics :=  nil;
      GdipBeginContainer :=  nil;
      GdipBeginContainerI :=  nil;
      GdipBeginContainer2 :=  nil;
      GdipEndContainer :=  nil;
      GdipGetMetafileHeaderFromWmf :=  nil;
      GdipGetMetafileHeaderFromEmf :=  nil;
      GdipGetMetafileHeaderFromFile :=  nil;
      GdipGetMetafileHeaderFromStream :=  nil;
      GdipGetMetafileHeaderFromMetafile :=  nil;
      GdipGetHemfFromMetafile :=  nil;
      GdipCreateStreamOnFile :=  nil;
      GdipCreateMetafileFromWmf :=  nil;
      GdipCreateMetafileFromEmf :=  nil;
      GdipCreateMetafileFromFile :=  nil;
      GdipCreateMetafileFromWmfFile :=  nil;
      GdipCreateMetafileFromStream :=  nil;
      GdipRecordMetafile :=  nil;
      GdipRecordMetafileI :=  nil;
      GdipRecordMetafileFileName :=  nil;
      GdipRecordMetafileFileNameI :=  nil;
      GdipRecordMetafileStream :=  nil;
      GdipRecordMetafileStreamI :=  nil;
      GdipSetMetafileDownLevelRasterizationLimit :=  nil;
      GdipGetMetafileDownLevelRasterizationLimit :=  nil;
      GdipGetImageDecodersSize :=  nil;
      GdipGetImageDecoders :=  nil;
      GdipGetImageEncodersSize :=  nil;
      GdipGetImageEncoders :=  nil;
      GdipComment :=  nil;
      GdipCreateFontFamilyFromName :=  nil;
      GdipDeleteFontFamily :=  nil;
      GdipCloneFontFamily :=  nil;
      GdipGetGenericFontFamilySansSerif :=  nil;
      GdipGetGenericFontFamilySerif :=  nil;
      GdipGetGenericFontFamilyMonospace :=  nil;
      GdipGetFamilyName :=  nil;
      GdipIsStyleAvailable :=  nil;
      GdipFontCollectionEnumerable :=  nil;
      GdipFontCollectionEnumerate :=  nil;
      GdipGetEmHeight :=  nil;
      GdipGetCellAscent :=  nil;
      GdipGetCellDescent :=  nil;
      GdipGetLineSpacing :=  nil;
      GdipCreateFontFromDC :=  nil;
      GdipCreateFontFromLogfontA :=  nil;
      GdipCreateFontFromLogfontW :=  nil;
      GdipCreateFont :=  nil;
      GdipCloneFont :=  nil;
      GdipDeleteFont :=  nil;
      GdipGetFamily :=  nil;
      GdipGetFontStyle :=  nil;
      GdipGetFontSize :=  nil;
      GdipGetFontUnit :=  nil;
      GdipGetFontHeight :=  nil;
      GdipGetFontHeightGivenDPI :=  nil;
      GdipGetLogFontA :=  nil;
      GdipGetLogFontW :=  nil;
      GdipNewInstalledFontCollection :=  nil;
      GdipNewPrivateFontCollection :=  nil;
      GdipDeletePrivateFontCollection :=  nil;
      GdipGetFontCollectionFamilyCount :=  nil;
      GdipGetFontCollectionFamilyList :=  nil;
      GdipPrivateAddFontFile :=  nil;
      GdipPrivateAddMemoryFont :=  nil;
      GdipDrawString :=  nil;
      GdipMeasureString :=  nil;
      GdipMeasureCharacterRanges :=  nil;
      GdipDrawDriverString :=  nil;
      GdipMeasureDriverString :=  nil;
      GdipCreateStringFormat :=  nil;
      GdipStringFormatGetGenericDefault :=  nil;
      GdipStringFormatGetGenericTypographic :=  nil;
      GdipDeleteStringFormat :=  nil;
      GdipCloneStringFormat :=  nil;
      GdipSetStringFormatFlags :=  nil;
      GdipGetStringFormatFlags :=  nil;
      GdipSetStringFormatAlign :=  nil;
      GdipGetStringFormatAlign :=  nil;
      GdipSetStringFormatLineAlign :=  nil;
      GdipGetStringFormatLineAlign :=  nil;
      GdipSetStringFormatTrimming :=  nil;
      GdipGetStringFormatTrimming :=  nil;
      GdipSetStringFormatHotkeyPrefix :=  nil;
      GdipGetStringFormatHotkeyPrefix :=  nil;
      GdipSetStringFormatTabStops :=  nil;
      GdipGetStringFormatTabStops :=  nil;
      GdipGetStringFormatTabStopCount :=  nil;
      GdipSetStringFormatDigitSubstitution :=  nil;
      GdipGetStringFormatDigitSubstitution :=  nil;
      GdipGetStringFormatMeasurableCharacterRangeCount :=  nil;
      GdipSetStringFormatMeasurableCharacterRanges :=  nil;
      GdipCreateCachedBitmap :=  nil;
      GdipDeleteCachedBitmap :=  nil;
      GdipDrawCachedBitmap :=  nil;
      GdipEmfToWmfBits :=  nil;
    end;
  finally
    Lock.Leave;
  end;
end;

function UseGDIP: Boolean;
begin
  Result := (hWINGDIPDLL > 0);
end;

// -----------------------------------------------------------------------------
// TGdiplusBase class
// -----------------------------------------------------------------------------

  class function TGdiplusBase.NewInstance: TObject;
  begin
    Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  end;

  procedure TGdiplusBase.FreeInstance;
  begin
    CleanupInstance;
    GdipFree(Self);
  end;

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: ObjectType): BOOL;
begin
  result :=  ((type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax));
end;

function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
begin
  result := (n or GDIP_WMF_RECORD_BASE);
end;

function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
begin
  result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;
begin
  result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TGPPoint Util
//--------------------------------------------------------------------------

  function MakePoint(X, Y: Integer): TGPPoint;
  begin
    result.X := X;
    result.Y := Y;
  end;

  function MakePoint(X, Y: Single): TGPPointF;
  begin
    Result.X := X;
    result.Y := Y;
  end;

//--------------------------------------------------------------------------
// TGPSize Util
//--------------------------------------------------------------------------

  function MakeSize(Width, Height: Single): TGPSizeF;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

  function MakeSize(Width, Height: Integer): TGPSize;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;
  begin
    result.First  := First;
    result.Length := Length;
  end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Single): TGPRectF; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPointF; size: TGPSizeF): TGPRectF; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Integer): TGPRect; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TGPPoint; size: TGPSize): TGPRect; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

  function MakeRect(const Rect: TRect): TGPRect;
  begin
    Result.X := rect.Left;
    Result.Y := Rect.Top;
    Result.Width := Rect.Right-Rect.Left;
    Result.Height:= Rect.Bottom-Rect.Top;
  end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

  constructor TPathData.Create;
  begin
    Count := 0;
    Points := nil;
    Types := nil;
  end;

  destructor TPathData.destroy;
  begin
    if assigned(Points) then freemem(Points);
    if assigned(Types) then freemem(Types);
  end;


function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
begin
  result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatCanonical) <> 0;
end;

// -----------------------------------------------------------------------------
// Color class
// -----------------------------------------------------------------------------

{  constructor TGPColor.Create;
  begin
    Argb := DWORD(Black);
  end;

  // Construct an opaque Color object with
  // the specified Red, Green, Blue values.
  //
  // Color values are not premultiplied.

  constructor TGPColor.Create(r, g, b: Byte);
  begin
    Argb := MakeARGB(255, r, g, b);
  end;

  constructor TGPColor.Create(a, r, g, b: Byte);
  begin
    Argb := MakeARGB(a, r, g, b);
  end;

  constructor TGPColor.Create(Value: ARGB);
  begin
    Argb := Value;
  end;

  function TGPColor.GetAlpha: BYTE;
  begin
    result := BYTE(Argb shr AlphaShift);
  end;

  function TGPColor.GetA: BYTE;
  begin
    result := GetAlpha;
  end;

  function TGPColor.GetRed: BYTE;
  begin
    result := BYTE(Argb shr RedShift);
  end;

  function TGPColor.GetR: BYTE;
  begin
    result := GetRed;
  end;

  function TGPColor.GetGreen: Byte;
  begin
    result := BYTE(Argb shr GreenShift);
  end;

  function TGPColor.GetG: Byte;
  begin
    result := GetGreen;
  end;

  function TGPColor.GetBlue: Byte;
  begin
    result := BYTE(Argb shr BlueShift);
  end;

  function TGPColor.GetB: Byte;
  begin
    result := GetBlue;
  end;

  function TGPColor.GetValue: ARGB;
  begin
    result := Argb;
  end;

  procedure TGPColor.SetValue(Value: ARGB);
  begin
    Argb := Value;
  end;

  procedure TGPColor.SetFromCOLORREF(rgb: COLORREF);
  begin
    Argb := MakeARGB(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function TGPColor.ToCOLORREF: COLORREF;
  begin
    result := RGB(GetRed, GetGreen, GetBlue);
  end;

  function TGPColor.MakeARGB(a, r, g, b: Byte): ARGB;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;  }

  function MakeColor(r, g, b: Byte): ARGB; overload;
  begin
    result := MakeColor(255, r, g, b);
  end;

  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;

  function GetAlpha(color: ARGB): BYTE;
  begin
    result := BYTE(color shr AlphaShift);
  end;

  function GetRed(color: ARGB): BYTE;
  begin
    result := BYTE(color shr RedShift);
  end;

  function GetGreen(color: ARGB): BYTE;
  begin
    result := BYTE(color shr GreenShift);
  end;

  function GetBlue(color: ARGB): BYTE;
  begin
    result := BYTE(color shr BlueShift);
  end;

  function ColorRefToARGB(rgb: COLORREF): ARGB;
  begin
    result := MakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function ARGBToColorRef(Color: ARGB): COLORREF;
  begin
    result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
  end;


// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

  procedure TMetafileHeader.GetBounds(out Rect: TGPRect);
  begin
    rect.X      := X;
    rect.Y      := Y;
    rect.Width  := Width;
    rect.Height := Height;
  end;

  function TMetafileHeader.IsWmf: BOOL;
  begin
    result :=  ((Type_ = MetafileTypeWmf) or (Type_ = MetafileTypeWmfPlaceable));
  end;

  function TMetafileHeader.IsWmfPlaceable: BOOL;
  begin
    result := (Type_ = MetafileTypeWmfPlaceable);
  end;

  function TMetafileHeader.IsEmf: BOOL;
  begin
    result := (Type_ = MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfOrEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsEmfPlusDual: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusDual)
  end;

  function TMetafileHeader.IsEmfPlusOnly: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsDisplay: BOOL;
  begin
    result := (IsEmfPlus and ((EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
  end;

  function TMetafileHeader.GetWmfHeader: PMetaHeader;
  begin
    if IsWmf then result :=  @Header.WmfHeader
             else result := nil;
  end;

  function TMetafileHeader.GetEmfHeader: PENHMETAHEADER3;
  begin
    if IsEmfOrEmfPlus then result := @Header.EmfHeader
                      else result := nil;
  end;

initialization
  Lock := TCriticalSection.Create;

finalization
  while ReferenceCount > 0 do
    FreeGDIPLibrary;
  Lock.Free;

end.
