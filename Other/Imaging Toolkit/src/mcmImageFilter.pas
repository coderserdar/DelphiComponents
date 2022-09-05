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
// $Log:  17549: mcmImageFilter.pas
//
//    Rev 1.18    2014-02-02 21:10:02  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.17    25-10-2009 17:25:04  mcm    Version: IMG 3.3
// Removed unused variables.
//
//    Rev 1.16    22-10-2009 21:27:40  mcm
// Improved speed of Median Filter.
//
//    Rev 1.15    20-08-2007 20:28:34  mcm
// Added support for Delphi 2007
//
//    Rev 1.14    02-04-2006 19:05:24  mcm
// Corrected Median filter.
//
//    Rev 1.13    18-03-2006 18:10:42  mcm    Version: IMG 2.16
// Clean-up
//
//    Rev 1.12    18-02-2006 20:03:46  mcm
// Added MeanHarmonic and MeanContraHarmonic filters.
//
//    Rev 1.11    22/11/2005 20:39:00  mcm
// Improved Shen-Castan, Canny and Marr-Hildreth filters.
//
//    Rev 1.10    15/11/2005 21:28:18  mcm    Version: IMG 2.10
// Implemented the Shen-Castan edge filter.
// Modified Marr-Hildreth filter, improving its results.
//
//   Rev 1.9    27-09-2005 18:52:40  mcm    Version: IMG 2.9
// Cleared typecast warning.

//
//   Rev 1.8    23-05-2005 22:04:40  mcm    Version: IMG 2.9
// Added initial implementation of Marr-Hildreth and Canny filter.
// Added initial implementation of despeckle filters.

//
//   Rev 1.7    28-10-2004 19:16:14  mcm    Version: IMG 2.6
// Modified Filter method to return ResultImage on success.
// Improved error handling in Filter method, and verfication of SourceImage and
// ResultImage.

//
//   Rev 1.6    24-11-2003 20:13:56  mcm

//
//   Rev 1.5    29-09-2003 18:44:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.4    12-05-2003 18:44:44  mcm    Version: IMG 1.3.4
// Added check for x and y index to Get/SetKernel. 

//
//   Rev 1.3    27-01-2003 13:40:10  mcm

//
//   Rev 1.2    09-09-2002 13:12:36  mcm    Version: IMG 1.2
// Added extra pre-defined filters: Degrain, Laplacian, Unsharpen, Median MaxMin
// & MinMax.
// Modified Kernel property, to access both possible kernels. 

//
//   Rev 1.1    01-08-2002 11:29:22  mcm    Version: IMG 1.1
// Added several new matrix filters, and support for RGB & RGBA images.

//
//   Rev 1.0    27-05-2002 16:22:04  mcm

unit mcmImageFilter;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmImageKernel;

type
  TmcmFilter = (FLT_NONE,
                FLT_AVERAGE,
                FLT_AVERAGEHEAVY,
                FLT_BLUR,
                FLT_BLURHEAVY,
                FLT_EDGE,
                FLT_EDGEPREWITT,
                FLT_DEGRAIN,
                FLT_EMBOSS,
                FLT_GAUSSBLUR,
                FLT_HIGHPASS,
                FLT_HIGHPASS2,
                FLT_KFILL,
                FLT_LAPLACIAN,
                FLT_MAXIMUM,
                FLT_MAXMIN,
                FLT_MEDIAN,
                FLT_MEDIANMAXMIN,
                FLT_MEDIANMINMAX,
                FLT_MINIMUM,
                FLT_MOSAIC,
                FLT_PREWITTNS,
                FLT_PREWITTEW,
                FLT_SHARPEN,
                FLT_SHARPENHEAVY,
                FLT_SMOOTH,
                FLT_SMOOTHCIRCLE,
                FLT_SMOOTHCONE,
                FLT_SMOOTHPYRAMIDAL,
                FLT_SOBELNS,
                FLT_SOBELEW,
                FLT_LINEMASKHORZ,
                FLT_LINEMASKVERT,
                FLT_UNSHARPMASK,

                FLT_MEANHARMONIC,
                FLT_MEANCONTRAHARMONIC,

                FLT_DESPECKLE_A,
                FLT_DESPECKLE_B,
                FLT_DESPECKLE_C,

                FLT_MARRHILDRETH,
                FLT_CANNY,
                FLT_SHENCASTAN,

                FLT_ISEF, // Shen-Castan's Infinite Symmetric Exponential Filter.

                FLT_USERDEF,
                FLT_USERABS,
                FLT_USERDBLRMS,
                FLT_USERDBLABS,
                FLT_USERDBLSUM,
                FLT_USERDBLDIF);

type
  TmcmImageFilter = class(TmcmImageKernel)
  private
    // Private declarations
    FIterate         : word;
    FMaxIterations   : word;
    FKernelHeight    : word;
    FKernelWidth     : word;
    FNeighbourHeight : word;
    FNeighbourWidth  : word;
    FKernelSize      : word;
    FKernel          : array[0..1] of PVectorI;
    FFactor          : integer;
    FBias            : integer;
    FFilters         : array[0..word(FLT_USERDBLDIF)] of pointer;
    FHysteresis      : longint;

    // kFill parameters.
    FBlx, FBby       : integer;
    FBrx, FBty       : integer;
    FNeighbourSize   : integer;
    FNeighbour       : PVectorNeighbour;

    // Gaussian parameters.
    FGSD             : double; // Gaussian Standard Deviation.
    FDeltaSD         : double;
    FTraceLo         : word;
    FTraceHi         : word;
    FTracePercent    : word;
    FTraceAuto       : boolean;

    // Shen-Castan
    FISEFFactor      : double; // 0.0 < FShenSmootFactor < 1.0

    // Harmonic parameters.
    FHarmonicOrder   : double;
  protected
    // Protected declarations
    procedure Average;

    procedure SetGaussSD(Value : double);
    procedure SetDeltaSD(Value : double);

    // Marr-Hildreth
    procedure Dbl2IntKernel(KernelIndex : integer; DblKernel : array of double; DblSize : integer);
    procedure ZeroCross(AImage : TmcmImage; ZeroValue : byte);
    procedure MarrFilter(Sigma : double);
    function  MarrHildreth : TmcmImage;

    // Canny
    procedure SuppressNoMax(XImage, YImage, MagImage : TKernelImage);
    procedure TracedThreshold(SrcImage : TKernelImage; TraceAuto : boolean; Percent : word; var Hi, Lo : word);
    procedure CannyFilter(Sigma : double; MagImage : TKernelImage);
    function  Canny : TmcmImage;

    // Shen-Castan
    function  Embed(const Src : TmcmImage; var Res : TKernelImage; const FilterWidth : integer) : boolean;
    function  Debed(const Src : TmcmImage; var Res : TKernelImage; const FilterWidth : integer) : boolean;
    // function  AdaptiveGradient(const BLIImage : TKernelImage; const ISEFImage : TKernelImage; const x, y : integer) : byte;
    // function  IsEdgeCandidate(const BLIImage : TKernelImage; const ISEFImage : TKernelImage;  const x, y : integer) : boolean;
    procedure LocateZeroCrossings(OrgImage : TKernelImage; const BLIImage : TKernelImage; const ISEFImage : TKernelImage; const FilterWidth : integer);
    function  IsefHorizontal(const Src : TmcmImage; var Res : TKernelImage; Causal, AntiCausal : TKernelImage) : boolean;
    function  IsefVertical(const Src : TmcmImage; var Res : TKernelImage; Causal, AntiCausal : TKernelImage) : boolean;
    function  IsefSmooth(const Src : TmcmImage; var Res : TKernelImage) : boolean;
    procedure ShenCastanFilter;
    function  ShenCastan : TmcmImage;
    function  ISEF : TmcmImage;

    procedure FilterKernel(Kernel : PVectorI);
    // procedure FilterRGBKernel(Kernel : PVectorI);
    procedure FilterAbsKernel(Kernel : PVectorI);
    procedure FilterDblRmsKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
    procedure FilterDblAbsKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
    procedure FilterDblSumKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
    procedure FilterDblDifKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
    procedure FilterEdgePrewittKernel;
    procedure FilterUnsharpMask;
    procedure FilterMeanHarmonic;
    procedure FilterMeanContraHarmonic;

    function  GetKernel(Index, x, y : integer) : integer;
    function  kFillFilter(Value, BkValue : byte) : cardinal;

    function  FilterDespeckleA(Value, BkValue, Count : byte) : cardinal;
    function  FilterDespeckleB(Value, BkValue, Count : byte) : cardinal;
    function  FilterDespeckleC(Value, BkValue, Count : byte) : cardinal;
    procedure DespeckleA;
    procedure DespeckleB;
    procedure DespeckleC;

    procedure kFillMark(AImage : TmcmImage; x, y : integer; Value, BkValue : byte);
    procedure kFill;

    procedure Maximum;
    procedure MaxMin(Kernel : PVectorI);
    procedure Median;
    procedure MedianMaxMin;
    procedure MedianMinMax;
    procedure Minimum;
    procedure Mosaic;
    procedure SetBias(Value : integer);
    procedure SetFactor(Value : integer);
    procedure SetFilterHeight(Value : word);
    procedure SetFilterWidth(Value : word);
    procedure SetFilterSize(Value : word);
    procedure SetKernel(Index, x, y : integer; Value : integer);
    procedure SetMaxIterations(Value : word);
    procedure SetSourceImage(Index : word; Image : TmcmImage); override;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Filter(Method : TmcmFilter) : TmcmImage;

    property    Bias : integer
      read      FBias
      write     SetBias default 0;
    property    DeltaSD : double // Gaussian Standard Deviation.
      read      FDeltaSD
      write     SetDeltaSD;
    property    FilterHeight : word
      read      FKernelHeight
      write     SetFilterHeight default 3;
    property    FilterSize : word
      read      FKernelWidth
      write     SetFilterSize;
    property    FilterWidth : word
      read      FKernelWidth
      write     SetFilterWidth default 3;
    property    GaussSD : double // Gaussian Standard Deviation.
      read      FGSD
      write     SetGaussSD;
    property    HarmonicOrder : double
      read      FHarmonicOrder
      write     FHarmonicOrder;
    property    Hysteresis : longint
      read      FHysteresis
      write     FHysteresis default 5;
    property    Iterate : word
      read      FIterate;
    property    MaxIterations : word
      read      FMaxIterations
      write     SetMaxIterations default 1;
    property    ScaleFactor : integer
      read      FFactor
      write     SetFactor default 1;
    property    Kernel[Index, x, y : integer] : integer
      read      GetKernel
      write     SetKernel;
    //
    property    SmoothFactor : double
      read      FISEFFactor
      write     FISEFFactor;
    // Canny filter properties.
    property    TraceAuto : boolean
      read      FTraceAuto
      write     FTraceAuto;
    property    TraceHigh : word
      read      FTraceHi
      write     FTraceHi;
    property    TraceLow : word
      read      FTraceLo
      write     FTraceLo;
    property    TracePercent : word
      read      FTracePercent
      write     FTracePercent;

  published
    // Published declarations
  end;

implementation

uses {$IFNDEF GE_DXE2}
      SysUtils, Math,
     {$ELSE}
      System.SysUtils, System.Math,
     {$ENDIF}
     mcmImageColor,
     mcmImageMath,
     mcmQSort,
     mcmImageResStr;

type TFilterSmallRecord = record
     Factor : integer;
     Bias   : integer;
     NxN    : integer;
     Filter : array[0..8] of integer;
     end;

     TFilterBigRecord = record
     Factor : integer;
     Bias   : integer;
     NxN    : integer;
     Filter : array[0..24] of integer;
     end;

     TFilter7x7Record = record
     Factor : integer;
     Bias   : integer;
     NxN    : integer;
     Filter : array[0..48] of integer;
     end;

const ZeroValue : integer = 128; // note: shortint range is -128 to 127

const
//------------------------------------------------------------------------------
// Filter definitions
//------------------------------------------------------------------------------
    DFLT_MAXSIZE = 50;

    DFLT_MAXSCWINSIZE = 25; // Shen-Castan, Max Window Size.

    // Smooth Filter, Factor = 11, Bias = 0.
    DFLT_SMOOTH       : TFilterSmallRecord =
                        (
                        Factor : 11;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 1, 1,
                                   1, 3, 1,
                                   1, 1, 1);
                        );

    // Averaging Heavy Filter, Factor = 9,  Bias = 0.
    DFLT_AVERAGE      : TFilterSmallRecord =
                        (
                        Factor : 9;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 1, 1,
                                   1, 1, 1,
                                   1, 1, 1);
                        );

    // Averaging Heavy Filter, Factor = 25,  Bias = 0.
    DFLT_AVERAGEHEAVY : TFilterBigRecord =
                        (
                        Factor : 25;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1);
                        );

    // Circular averaging Filter, Factor = 21,  Bias = 0.
    DFLT_SMOOTHCIRCLE : TFilterBigRecord =
                        (
                        Factor : 21;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 0, 1, 1, 1, 0,
                                   1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1,
                                   0, 1, 1, 1, 0);
                        );

    // Pyramidal Averaging Filter, Factor = 81,  Bias = 0.
    DFLT_SMOOTHPYRAMIDAL : TFilterBigRecord =
                        (
                        Factor : 81;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 1, 2, 3, 2, 1,
                                   2, 4, 6, 4, 2,
                                   3, 6, 9, 6, 3,
                                   2, 4, 6, 4, 2,
                                   1, 2, 3, 2, 1);
                        );

    // Cone Averaging Filter, Factor = 25,  Bias = 0.
    DFLT_SMOOTHCONE   : TFilterBigRecord =
                        (
                        Factor : 25;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 0, 0, 1, 0, 0,
                                   0, 2, 2, 2, 0,
                                   1, 2, 5, 2, 1,
                                   0, 2, 2, 2, 0,
                                   0, 0, 1, 0, 0);
                        );

    // Blur Filter, Factor = 14, Bias = 0.
    DFLT_BLUR         : TFilterSmallRecord =
                        (
                        Factor : 14;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 2, 1,
                                   2, 2, 2,
                                   1, 2, 1);
                        );

    // Blur Heavy Filter, Factor = 32, Bias = 0.
    DFLT_BLURHEAVY    : TFilterBigRecord =
                        (
                        Factor : 32;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 0, 1, 1, 1, 0,
                                   1, 2, 2, 2, 1,
                                   1, 2, 4, 2, 1,
                                   1, 2, 2, 2, 1,
                                   0, 1, 1, 1, 0);
                        );

    // Gauss Blur Filter, Factor = 331, Bias = 0.
    DFLT_GAUSSBLUR    : TFilterBigRecord =
                        (
                        Factor : 331;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 1,  4,  7 , 4, 1,
                                   4, 20, 33, 20, 4,
                                   7, 33, 55, 33, 7,
                                   4, 20, 33, 20, 4,
                                   1,  4,  7,  4, 1);
                        );

//------------------------------------------------------------------------------
    // Sharpen Filter, Factor = 1,  Bias = 0.
    // Laplacian.
    DFLT_LAPLACIAN    : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (0, 1, 0,
                                  1,-4, 1,
                                  0, 1, 0);
                        );

    // Sharpen Filter, Factor = 1,  Bias = 0.
    // Laplacian.
    DFLT_SHARPEN      : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 0,-1, 0,
                                  -1, 5,-1,
                                   0,-1, 0);
                        );

    // Sharpen Heavy Filter, Factor = 1,  Bias = 0.
    // Laplace.
    DFLT_SHARPENHEAVY : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (-1,-1,-1,
                                  -1, 9,-1,
                                  -1,-1,-1);
                        );

    DFLT_HIGHPASS     : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (-1,-1,-1,
                                  -1, 8,-1,
                                  -1,-1,-1);
                        );

    //
    DFLT_HIGHPASS2    : TFilterBigRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : ( 0,-1,-1,-1, 0,
                                  -1,-1,-2,-1,-1,
                                  -1,-2,24,-2,-1,
                                  -1,-1,-2,-1,-1,
                                   0,-1,-1,-1, 0);
                        );

    // Edge Filter, Factor = 1,  Bias = 0.
    DFLT_EDGE         : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (-1,-2,-1,
                                  -2,12,-2,
                                  -1,-2,-1);
                        );

    // Prewitt N-S Filter, Factor = 1,  Bias = 0.
    DFLT_PREWITTNS    : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 1, 1,
                                   0, 0, 0,
                                  -1,-1,-1);
                        );

    // Prewitt E-W Filter, Factor = 1,  Bias = 0.
    DFLT_PREWITTEW   : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 0,-1,
                                   1, 0,-1,
                                   1, 0,-1);
                        );

    // Sobel Filter, Factor = 1,  Bias = 0.
    DFLT_SOBELNS     : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 2, 1,
                                   0, 0, 0,
                                  -1,-2,-1);
                        );

    // Sobel Filter, Factor = 1,  Bias = 0.
    DFLT_SOBELEW     : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : ( 1, 0,-1,
                                   2, 0,-2,
                                   1, 0,-1);
                        );

//------------------------------------------------------------------------------
    // Emboss Filter, Factor = 1,  Bias = 127.
    DFLT_EMBOSS       : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 127;
                        NxN    : 3;
                        Filter : ( 0, 0,-1,
                                   0, 0, 0,
                                   1, 0, 0);
                        );

    DFLT_LINEMASKHORZ : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (-1,-1,-1,
                                   2, 2, 2,
                                  -1,-1,-1);
                        );

    DFLT_LINEMASKVERT : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (-1, 2,-1,
                                  -1, 2,-1,
                                  -1, 2,-1);
                        );

//------------------------------------------------------------------------------

    DFLT_MAXMIN3      : TFilterSmallRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 3;
                        Filter : (0, 0, 0,
                                  0, 1, 0,
                                  0, 0, 0);
                        );

    DFLT_MAXMIN5      : TFilterBigRecord =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 5;
                        Filter : (0, 0, 0, 0, 0,
                                  0, 1, 1, 1, 0,
                                  0, 1, 1, 1, 0,
                                  0, 1, 1, 1, 0,
                                  0, 0, 0, 0, 0);
                        );


    DFLT_MAXMIN7      : TFilter7x7Record =
                        (
                        Factor : 1;
                        Bias   : 0;
                        NxN    : 7;
                        Filter : (0, 0, 0, 0, 0, 0, 0,
                                  0, 1, 1, 1, 1, 1, 0,
                                  0, 1, 1, 1, 1, 1, 0,
                                  0, 1, 1, 1, 1, 1, 0,
                                  0, 1, 1, 1, 1, 1, 0,
                                  0, 1, 1, 1, 1, 1, 0,
                                  0, 0, 0, 0, 0, 0, 0);
                        );


constructor TmcmImageFilter.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FHysteresis   := 5;
  FKernelHeight := 3;
  FKernelWidth  := 3;
  FKernelSize   := FKernelHeight * FKernelWidth;
  FNeighbourHeight := FKernelHeight shr 1;
  FNeighbourWidth  := FKernelWidth shr 1;
  FBias            := 0;
  FFactor          := 1;

  FGSD             := 2.0;
  FDeltaSD         := 0.8;
  FTraceLo         := 100;
  FTraceHi         := 200;
  FTracePercent    := 50;
  FTraceAuto       := True;

  FISEFFactor      := 0.7;

  FHarmonicOrder   := 1.5;

  FMaxIterations := 1;

  FFilters[integer(FLT_NONE)]              := Nil;
  FFilters[integer(FLT_AVERAGE)]           := @DFLT_AVERAGE;
  FFilters[integer(FLT_AVERAGEHEAVY)]      := @DFLT_AVERAGEHEAVY;
  FFilters[integer(FLT_BLUR)]              := @DFLT_BLUR;
  FFilters[integer(FLT_BLURHEAVY)]         := @DFLT_BLURHEAVY;
  FFilters[integer(FLT_DEGRAIN)]           := Nil;
  FFilters[integer(FLT_EDGE)]              := @DFLT_EDGE;
  FFilters[integer(FLT_EDGEPREWITT)]       := Nil;
  FFilters[integer(FLT_EMBOSS)]            := @DFLT_EMBOSS;
  FFilters[integer(FLT_GAUSSBLUR)]         := @DFLT_GAUSSBLUR;
  FFilters[integer(FLT_HIGHPASS)]          := @DFLT_HIGHPASS;
  FFilters[integer(FLT_KFILL)]             := Nil;
  FFilters[integer(FLT_LAPLACIAN)]         := @DFLT_LAPLACIAN;
  FFilters[integer(FLT_MAXIMUM)]           := Nil;
  FFilters[integer(FLT_MAXMIN)]            := Nil;
//  FFilters[integer(FLT_MAXMIN5)]           := Nil;
  FFilters[integer(FLT_MEDIAN)]            := Nil;
  FFilters[integer(FLT_MEDIANMAXMIN)]      := Nil;
  FFilters[integer(FLT_MEDIANMINMAX)]      := Nil;

  FFilters[integer(FLT_MINIMUM)]           := Nil;
  FFilters[integer(FLT_MOSAIC)]            := Nil;
  FFilters[integer(FLT_PREWITTNS)]         := @DFLT_PREWITTNS;
  FFilters[integer(FLT_PREWITTEW)]         := @DFLT_PREWITTEW;
  FFilters[integer(FLT_SHARPEN)]           := @DFLT_SHARPEN;
  FFilters[integer(FLT_SHARPENHEAVY)]      := @DFLT_SHARPENHEAVY;
  FFilters[integer(FLT_SMOOTH)]            := @DFLT_SMOOTH;
  FFilters[integer(FLT_SMOOTHCIRCLE)]      := @DFLT_SMOOTHCIRCLE;
  FFilters[integer(FLT_SMOOTHCONE)]        := @DFLT_SMOOTHCONE;
  FFilters[integer(FLT_SMOOTHPYRAMIDAL)]   := @DFLT_SMOOTHPYRAMIDAL;
  FFilters[integer(FLT_SOBELNS)]           := @DFLT_SOBELNS;
  FFilters[integer(FLT_SOBELEW)]           := @DFLT_SOBELEW;
  FFilters[integer(FLT_LINEMASKHORZ)]      := @DFLT_LINEMASKHORZ;
  FFilters[integer(FLT_LINEMASKVERT)]      := @DFLT_LINEMASKVERT;
  FFilters[integer(FLT_UNSHARPMASK)]       := Nil;

  FFilters[integer(FLT_MEANHARMONIC)]      := Nil;
  FFilters[integer(FLT_MEANCONTRAHARMONIC)]:= Nil;

  FFilters[integer(FLT_USERDEF)]           := Nil;
  FFilters[integer(FLT_USERABS)]           := Nil;
  FFilters[integer(FLT_USERDBLRMS)]        := Nil;
  FFilters[integer(FLT_USERDBLABS)]        := Nil;
  FFilters[integer(FLT_USERDBLSUM)]        := Nil;
  FFilters[integer(FLT_USERDBLDIF)]        := Nil;

  
  FFilters[integer(FLT_DESPECKLE_A)]       := Nil;
  FFilters[integer(FLT_DESPECKLE_B)]       := Nil;
  FFilters[integer(FLT_DESPECKLE_C)]       := Nil;

  FFilters[integer(FLT_MARRHILDRETH)]      := Nil;
  FFilters[integer(FLT_CANNY)]             := Nil;
  FFilters[integer(FLT_SHENCASTAN)]        := Nil;

  FFilters[integer(FLT_ISEF)]              := Nil;

  GetMem(FKernel[0], DFLT_MAXSIZE * DFLT_MAXSIZE * SizeOf(integer));
  GetMem(FKernel[1], DFLT_MAXSIZE * DFLT_MAXSIZE * SizeOf(integer));
  FillChar(FKernel[0]^, DFLT_MAXSIZE * DFLT_MAXSIZE * SizeOf(integer), 0);
  FillChar(FKernel[1]^, DFLT_MAXSIZE * DFLT_MAXSIZE * SizeOf(integer), 0);
end; // TmcmImageFilter.Create.


destructor TmcmImageFilter.Destroy;
begin
  if Assigned(FKernel[0])
  then FreeMem(FKernel[0]);
  if Assigned(FKernel[1])
  then FreeMem(FKernel[1]);
  Inherited Destroy;
end; // TmcmImageFilter.Destroy.


function TmcmImageFilter.GetKernel(Index, x, y : integer) : integer;
var i : integer;
begin
  if (0 <= Index) and (Index < 2)
  then begin
       if (0 > x) or (x >= FKernelWidth)
       then Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [x, 0, FKernelWidth-1]);
       if (0 > y) or (y >= FKernelHeight)
       then Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [y, 0, FKernelHeight-1]);
       i := x + (y * FKernelWidth);
       Result := FKernel[Index]^[i];
  end
  else Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [Index, 0, 1]);
end; // TmcmImageFilter.GetKernel.


procedure TmcmImageFilter.SetKernel(Index, x, y : integer; Value : integer);
var i : integer;
begin
  if (0 <= Index) and (Index < 2)
  then begin
       if (0 > x) or (x >= FKernelWidth)
       then Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [x, 0, FKernelWidth-1]);
       if (0 > y) or (y >= FKernelHeight)
       then Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [y, 0, FKernelHeight-1]);
       i := x + (y * FKernelWidth);
       FKernel[Index]^[i] := Value;
  end
  else Raise ERangeError.CreateFmt(resEC_RANGEINDEX, [Index, 0, 1]);
end; // TmcmImageFilter.SetKernel.


procedure TmcmImageFilter.SetSourceImage(Index : word; Image : TmcmImage);
begin
  Inherited SetSourceImage(Index, Image);
end; // TmcmImageFilter.SetSourceImage.


procedure TmcmImageFilter.SetBias(Value : integer);
begin
  FBias := Value;
end; // TmcmImageFilter.SetBias.


procedure TmcmImageFilter.SetFactor(Value : integer);
begin
  if (Value <> 0)
  then FFactor := Value;
end; // TmcmImageFilter.SetFactor.


procedure TmcmImageFilter.SetFilterHeight(Value : word);
begin
  if (Value >= 1) and (FKernelHeight <> Value)
  then begin
       if (Value > DFLT_MAXSIZE)
       then FKernelHeight := DFLT_MAXSIZE
       else FKernelHeight := Value;
       FNeighbourHeight := FKernelHeight shr 1;
       FKernelSize := FKernelHeight * FKernelWidth;
  end;
end; // TmcmImageFilter.SetFilterHeight.


procedure TmcmImageFilter.SetFilterWidth(Value : word);
begin
  if (Value >= 1) and (FKernelWidth <> Value)
  then begin
       if (Value > DFLT_MAXSIZE)
       then FKernelWidth := DFLT_MAXSIZE
       else FKernelWidth := Value;
       FNeighbourWidth := FKernelWidth shr 1;
       FKernelSize := FKernelHeight * FKernelWidth;
  end;
end; // TmcmImageFilter.SetFilterWidth.


procedure TmcmImageFilter.SetFilterSize(Value : word);
begin
  if (Value > 1) and ((FKernelHeight <> Value) or (FKernelWidth <> Value))
  then begin
       if (Value > DFLT_MAXSIZE)
       then Value := DFLT_MAXSIZE;
       FKernelHeight := Value;
       FKernelWidth  := Value;
       FNeighbourHeight := FKernelHeight shr 1;
       FNeighbourWidth  := FKernelWidth shr 1;
       FKernelSize := FKernelHeight * FKernelWidth;
  end;
end; // TmcmImageFilter.SetFilterSize.


procedure TmcmImageFilter.SetMaxIterations(Value : word);
begin
  if (Value > 0)
  then FMaxIterations := Value;
end; // End TmcmImageFilter.SetMaxIterations.


function TmcmImageFilter.Filter(Method : TmcmFilter) : TmcmImage;
var pSaveS     : TKernelImage;
    pSaveR     : TKernelImage;
    ImageColor : TmcmImageColor;
    i, Runs    : integer;
    SaveSource : TmcmImage;
begin
  FError := EC_OK;
  
  // Check special edge filters.
  if (Method in [FLT_MARRHILDRETH,FLT_CANNY,FLT_SHENCASTAN,FLT_ISEF])
  then if Not(CheckSource(0, [IF_GREY8]))
       then FError := EC_BADCOLORFORMAT;

  if (FError = EC_OK) and CheckSource(0, [IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Result := Nil;
                 Exit;
            end;
       end;
       if (FSrcImage[0].ImageFormat <> FResImage.ImageFormat)
       then begin
            FError := EC_BADCOLORFORMAT;
            Result := Nil;
            Exit;
       end;

       if (FSrcImage[0].ImageFormat in [IF_RGB24,IF_RGBA32])
       then begin
            pSaveS := FSrcImage[0];
            pSaveR := FResImage;
            FSrcImage[0] := TKernelImage(TmcmImage.Create);
            FSrcImage[0].Height := pSaveS.Height;
            FSrcImage[0].Width  := pSaveS.Width;
            FSrcImage[0].ImageFormat := IF_GREY8;
            FSrcImage[0].CreateGreyPalette;
            FResImage := FSrcImage[0];
            ImageColor := TmcmImageColor.Create(Nil);
            Runs := 2;
       end
       else begin
            pSaveS := Nil;
            pSaveR := Nil;
            ImageColor := Nil;
            Runs := 0;
       end;

       for i := 0 to Runs
       do begin
          if Assigned(ImageColor)
          then begin
               ImageColor.SourceImage[0] := pSaveS;
               ImageColor.ResultImage    := FSrcImage[0];
               ImageColor.GetAChannel(i);
          end;

          case Method of
          FLT_AVERAGE      : Average;
          FLT_DEGRAIN      : begin
                               SaveSource := FSrcImage[0];
                               MedianMaxMin;
                               FSrcImage[0] := FResImage;
                               MedianMinMax;
                               FSrcImage[0] := TKernelImage(SaveSource);
                             end;
          FLT_KFILL        : kFill;
          FLT_DESPECKLE_A  : DespeckleA;
          FLT_DESPECKLE_B  : DespeckleB;
          FLT_DESPECKLE_C  : DespeckleC;
          FLT_MAXIMUM      : Maximum;
          FLT_MAXMIN       : begin
                               case FilterSize of
                               1..4 : MaxMin(@TFilterSmallRecord(DFLT_MAXMIN3).Filter);
                               5..6 : MaxMin(@TFilterBigRecord(DFLT_MAXMIN5).Filter);
                               7    : MaxMin(@TFilter7x7Record(DFLT_MAXMIN7).Filter);
                               else MaxMin(@TFilter7x7Record(DFLT_MAXMIN7).Filter);
                               end;
                             end;
          FLT_MEDIAN       : Median;
          FLT_MEDIANMAXMIN : MedianMaxMin;
          FLT_MEDIANMINMAX : MedianMinMax;
          FLT_MINIMUM      : Minimum;
          FLT_MOSAIC       : Mosaic;
          FLT_AVERAGEHEAVY,
          FLT_BLUR,
          FLT_BLURHEAVY,
          FLT_EDGE,
          FLT_EMBOSS,
          FLT_GAUSSBLUR,
          FLT_HIGHPASS,
          FLT_HIGHPASS2,
          FLT_LAPLACIAN,
          FLT_SHARPEN,
          FLT_SHARPENHEAVY,
          FLT_SMOOTH,
          FLT_SMOOTHCIRCLE,
          FLT_SMOOTHCONE,
          FLT_SMOOTHPYRAMIDAL,
          FLT_LINEMASKHORZ,
          FLT_LINEMASKVERT
                           : begin
                               FilterSize := TFilterSmallRecord(FFilters[word(Method)]^).NxN;
                               FBias      := TFilterSmallRecord(FFilters[word(Method)]^).Bias;
                               FFactor    := TFilterSmallRecord(FFilters[word(Method)]^).Factor;
                               case FSrcImage[0].ImageFormat of
                               IF_GREY8 : FilterKernel(@TFilterSmallRecord(FFilters[word(Method)]^).Filter);
                               // IF_RGB24 : FilterRGBKernel(@TFilterSmallRecord(FFilters[word(Method)]^).Filter);
                               end;
                             end;
          FLT_PREWITTNS,
          FLT_PREWITTEW,
          FLT_SOBELNS,
          FLT_SOBELEW      : begin
                               FilterSize := TFilterSmallRecord(FFilters[word(Method)]^).NxN;
                               FBias      := TFilterSmallRecord(FFilters[word(Method)]^).Bias;
                               FFactor    := TFilterSmallRecord(FFilters[word(Method)]^).Factor;
                               case FSrcImage[0].ImageFormat of
                               IF_GREY8 : FilterAbsKernel(@TFilterSmallRecord(FFilters[word(Method)]^).Filter);
                               // IF_RGB24 : FilterRGBKernel(@TFilterSmallRecord(FFilters[word(Method)]^).Filter);
                               end;
                             end;

          FLT_UNSHARPMASK  : FilterUnsharpMask;
          FLT_MEANHARMONIC : FilterMeanHarmonic;
          FLT_MEANCONTRAHARMONIC : FilterMeanContraHarmonic;

          // Special & Double kernel filters.
          FLT_EDGEPREWITT  : FilterEdgePrewittKernel;

          FLT_MARRHILDRETH : MarrHildreth;
          FLT_CANNY        : Canny;
          FLT_SHENCASTAN   : ShenCastan;
          FLT_ISEF         : ISEF;

          // User defined filters.
          FLT_USERDEF      : FilterKernel(FKernel[0]);
          FLT_USERABS      : FilterAbsKernel(FKernel[0]);
          FLT_USERDBLRMS   : FilterDblRmsKernel(FKernel[0], FKernel[1]);
          FLT_USERDBLABS   : FilterDblAbsKernel(FKernel[0], FKernel[1]);
          FLT_USERDBLSUM   : FilterDblSumKernel(FKernel[0], FKernel[1]);
          FLT_USERDBLDIF   : FilterDblDifKernel(FKernel[0], FKernel[1]);
          end;

          if Assigned(ImageColor)
          then begin
               ImageColor.SourceImage[0] := FResImage;
               ImageColor.ResultImage := pSaveR;
               ImageColor.SetAChannel(i);
          end;
       end;

       if Assigned(ImageColor)
       then begin
            FSrcImage[0].Free;
            FSrcImage[0] := pSaveS;
            FResImage := pSaveR;
            ImageColor.Free;
       end;
  end
  else FError := EC_BADCOLORFORMAT;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageFilter.Filter.


//------------------------------------------------------------------------------
// General filter kernel.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.FilterKernel(Kernel : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix   : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix := 0;
          l := m;
          k := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix := SumPix + pLines[l]^[i] * Kernel[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;
          SumPix := (SumPix div FFactor) + FBias;
          if (SumPix < 0)
          then pRes[x] := 0
          else if (SumPix > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterKernel.


procedure TmcmImageFilter.FilterUnsharpMask;
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     c        : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix   : longint;
     Kernel   : PVectorI;
begin
  Kernel  := @TFilterSmallRecord(FFilters[word(FLT_LAPLACIAN)]^).Filter;
  FFactor := TFilterSmallRecord(FFilters[word(FLT_LAPLACIAN)]^).Factor;
  FBias   := TFilterSmallRecord(FFilters[word(FLT_LAPLACIAN)]^).Bias;

  FFactor := FFactor * 2;

  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    c := FNeighbourHeight; // c = index to centre line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix := 0;
          l := m;
          k := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix := SumPix + pLines[l]^[i] * Kernel[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          SumPix := pLines[c]^[x+FNeighbourWidth] - (SumPix div FFactor);
          if (SumPix < 0)
          then pRes[x] := 0
          else if (SumPix > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(c);
       if (c = FKernelHeight)
       then c := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterUnsharpMask.


procedure TmcmImageFilter.FilterMeanHarmonic;
var  x, y, z   : longint;
     yi        : longint;
     i, j, k   : longint;
     m, n      : integer;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     Sum       : Single;
     MedianPos : word;
     Value     : integer;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);
  MedianPos := FKernelSize shr 1;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    n  := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          m := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                Flt[m] := byte(pLine[j]^[i]);
                inc(m);
             end;
          end;

          // Mean Harmonic calculation.
          z := 0;
          Sum := 0.0;
          for k := 0 to (FKernelSize - 1)
          do begin
             if (Flt^[k] = 0)
             then z := 1
             else Sum := Sum + 1.0 / Flt^[k];
          end;

          if (z = 1)
          then pRes[x] := Flt^[MedianPos]
          else begin
               Value := Round(FKernelSize / Sum  + 0.5);
               if (Value > 255)
               then pRes[x] := 255
               else pRes[x] := Value;
          end;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.FilterMeanHarmonic.


procedure TmcmImageFilter.FilterMeanContraHarmonic;
var  x, y, z    : longint;
     yi         : longint;
     i, j, k    : longint;
     m, n       : integer;
     pLine      : PMatrixB;
     pRes       : PVectorB;
     Fiw        : longint; // Image height - 1
     Fih        : longint; // Image width - 1
     Fkh        : longint; // Filter kernel height - 1
     Fkw        : longint;
     Flt        : PVectorB;
     SumNum     : Single;
     SumDenum   : Single;
     MedianPos  : word;
     Value      : integer;
     PowerValue : single;
     NumValue   : array[0..255] of single;
     DenumValue : array[0..255] of single;
begin
  PowerValue := FHarmonicOrder;

  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);
  MedianPos := FKernelSize shr 1;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    for i := 0 to 255
    do begin
       if (PowerValue < 0.0) and (i = 0)
       then begin
            NumValue[i]   := 0.0;
            DenumValue[i] := 0.0;
       end
       else begin
            NumValue[i]   := Power(i, PowerValue + 1.0);
            DenumValue[i] := Power(i, PowerValue);
       end;
    end;

    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    n  := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          m := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                Flt[m] := byte(pLine[j]^[i]);
                inc(m);
             end;
          end;

          // Mean Contra-Harmonic calculation.
          z := 0;
          SumNum := 0.0;
          SumDenum := 0.0;
          for k := 0 to (FKernelSize - 1)
          do begin
             if (Flt^[k] = 0) and (PowerValue < 0.0)
             then z := 1
             else begin
                  SumNum := SumNum + NumValue[Flt^[k]];
                  SumDenum := SumDenum + DenumValue[Flt^[k]];
             end;
          end;

          if (z = 1)
          then pRes[x] := Flt^[MedianPos]
          else begin
               if (SumDenum = 0.0)
               then pRes[x] := 0
               else begin
                    Value := Round(SumNum / SumDenum);
                    if (Value > 255)
                    then pRes[x] := 255
                    else pRes[x] := Value;
               end;
          end;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.FilterMeanContraHarmonic.


procedure TmcmImageFilter.FilterAbsKernel(Kernel : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix   : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix := 0;
          l := m;
          k := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix := SumPix + pLines[l]^[i] * Kernel[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;
          SumPix := Abs(SumPix);
          SumPix := (SumPix div FFactor) + FBias;
          if (SumPix < 0)
          then pRes[x] := 0
          else if (SumPix > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterAbsKernel.


procedure TmcmImageFilter.FilterDblRmsKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix1  : longint;
     SumPix2  : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix1 := 0;
          SumPix2 := 0;
          l := m;
          k := 0;

          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix1 := SumPix1 + pLines[l]^[i] * Kernel1[k];
                SumPix2 := SumPix2 + pLines[l]^[i] * Kernel2[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          // RMS
          SumPix1 := Round(Sqrt(Sqr(SumPix1) + Sqr(SumPix2)));
          SumPix1 := (SumPix1 div FFactor) + FBias;

          if (SumPix1 < 0)
          then pRes[x] := 0
          else if (SumPix1 > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix1;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterDblRmsKernel.


procedure TmcmImageFilter.FilterDblAbsKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix1  : longint;
     SumPix2  : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix1 := 0;
          SumPix2 := 0;
          l := m;
          k := 0;

          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix1 := SumPix1 + pLines[l]^[i] * Kernel1[k];
                SumPix2 := SumPix2 + pLines[l]^[i] * Kernel2[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          // ABS
          SumPix1 := Abs(SumPix1);
          SumPix2 := Abs(SumPix2);
          if (SumPix1 < SumPix2)
          then SumPix1 := SumPix2;
          SumPix1 := (SumPix1 div FFactor) + FBias;
          if (SumPix1 < 0)
          then pRes[x] := 0
          else if (SumPix1 > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix1;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterDblAbsKernel.


procedure TmcmImageFilter.FilterDblSumKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix1  : longint;
     SumPix2  : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix1 := 0;
          SumPix2 := 0;
          l := m;
          k := 0;

          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix1 := SumPix1 + pLines[l]^[i] * Kernel1[k];
                SumPix2 := SumPix2 + pLines[l]^[i] * Kernel2[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          // SUM
          SumPix1 := Round(SumPix1 + SumPix2);
          SumPix1 := (SumPix1 div FFactor) + FBias;
          if (SumPix1 < 0)
          then pRes[x] := 0
          else if (SumPix1 > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix1;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterDblSumKernel.


procedure TmcmImageFilter.FilterDblDifKernel(Kernel1 : PVectorI; Kernel2 : PVectorI);
var  x, y, yi : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     SumPix1  : longint;
     SumPix2  : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix1 := 0;
          SumPix2 := 0;
          l := m;
          k := 0;

          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                SumPix1 := SumPix1 + pLines[l]^[i] * Kernel1[k];
                SumPix2 := SumPix2 + pLines[l]^[i] * Kernel2[k];
                inc(k);
             end;

             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          // DIF
          SumPix1 := Abs(Round(SumPix1 - SumPix2));
          SumPix1 := (SumPix1 div FFactor) + FBias;
          if (SumPix1 < 0)
          then pRes[x] := 0
          else if (SumPix1 > 255)
               then pRes[x] := 255
               else pRes[x] := SumPix1;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterDblDifKernel.


procedure TmcmImageFilter.FilterEdgePrewittKernel;
var  x, y, yi : longint;
     i, j     : longint;
     c, m, n  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     //Fkw      : longint;
     SumPix1  : longint;
     SumPix2  : longint;
begin
  FilterSize := 3;

  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  //Fkw := FKernelWidth;
  //dec(Fkw);

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    c := FNeighbourHeight; // c = index to centre line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Optimised edge kernel, combines Prewitt North-West and South-East.
          SumPix1 := 0;
          inc(SumPix1, pLines[m]^[x]);
          dec(SumPix1, pLines[n]^[x]);
          inc(SumPix1, pLines[m]^[x+1]);
          dec(SumPix1, pLines[n]^[x+1]);
          inc(SumPix1, pLines[m]^[x+2]);
          dec(SumPix1, pLines[n]^[x+2]);

          SumPix2 := 0;
          inc(SumPix2, pLines[m]^[x]);
          dec(SumPix2, pLines[m]^[x+2]);
          inc(SumPix2, pLines[c]^[x]);
          dec(SumPix2, pLines[c]^[x+2]);
          inc(SumPix2, pLines[n]^[x]);
          dec(SumPix2, pLines[n]^[x+2]);

          SumPix1 := Round(Sqrt(1.0 * (SumPix1 * SumPix1 +
                                       SumPix2 * SumPix2)));
          if (SumPix1 > 255)
          then SumPix1 := 255;
          pRes[x] := Byte(SumPix1);
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(c);
       if (c = FKernelHeight)
       then c := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterEdgePrewittKernel.


(*

// Alternative RGB filter.

procedure TmcmImageFilter.FilterRGBKernel(Kernel : PVectorI);
var  x, y, yi : longint;
     i3, o3, x3 : longint;
     i, j, k  : longint;
     m, n, l  : integer;
     pLines   : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     Fiw3     : longint;
     SumPix   : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);
  Fiw3 := 3 * Fiw;

  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].LongLineWidth + 3 * FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       x3 := 3 * FNeighbourWidth;
       CopyMemory(@pLines[i]^[x3], FSrcImage[0].ScanLine[yi], FSrcImage[0].LongLineWidth * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          i3 := 3 * j;
          pLines[i]^[i3] := pLines[i]^[x3];
          pLines[i]^[i3+1] := pLines[i]^[x3+1];
          pLines[i]^[i3+2] := pLines[i]^[x3+2];

          pLines[i]^[x3+Fiw3+3+i3] := pLines[i]^[x3+Fiw3];
          pLines[i]^[x3+Fiw3+4+i3] := pLines[i]^[x3+Fiw3+1];
          pLines[i]^[x3+Fiw3+5+i3] := pLines[i]^[x3+Fiw3+2];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       x3 := 3 * FNeighbourWidth;
       CopyMemory(@pLines[n]^[x3], FSrcImage[0].ScanLine[yi], FSrcImage[0].LongLineWidth * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          i3 := 3 * j;
          pLines[n]^[i3] := pLines[n]^[x3];
          pLines[n]^[i3+1] := pLines[n]^[x3+1];
          pLines[n]^[i3+2] := pLines[n]^[x3+2];

          pLines[n]^[x3+Fiw3+3+i3] := pLines[n]^[x3+Fiw3];
          pLines[n]^[x3+Fiw3+4+i3] := pLines[n]^[x3+Fiw3+1];
          pLines[n]^[x3+Fiw3+5+i3] := pLines[n]^[x3+Fiw3+2];
       end;
       pRes := FResImage.ScanLine[y];

       for o3 := 0 to 2
       do begin
          x3 := o3;
          for x := 0 to Fiw  // All pixels in line
          do begin
             // Filter kernel.
             SumPix := 0;
             l := m;
             k := 0;
             for j := 0 to Fkh
             do begin
                i3 := o3 + 3 * x;
                for i := x to x + Fkw
                do begin
                   SumPix := SumPix + pLines[l]^[i3] * Kernel[k];
                   inc(k);
                   inc(i3, 3);
                end;
                inc(l);
                if (l = FKernelHeight)
                then l := 0;
             end;
             SumPix := (SumPix div FFactor) + FBias;
             if (SumPix < 0)
             then pRes[x3] := 0
             else if (SumPix > 255)
                  then pRes[x3] := 255
                  else pRes[x3] := SumPix;
             inc(x3, 3);
          end;
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
  end;
end; // TmcmImageFilter.FilterRGBKernel.
*)

//------------------------------------------------------------------------------
// Average filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Average;
var  x, y, yi : longint;
     i, j     : longint;
     m, n     : integer;
     pLine    : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint; // Filter kernel width - 1
     SumPix   : cardinal;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          SumPix := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do SumPix := SumPix + byte(pLine[j]^[i]);
          end;
          pRes[x] := byte(SumPix div FKernelSize);
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.Average.


//------------------------------------------------------------------------------
// Despeckle methods.
//------------------------------------------------------------------------------

function TmcmImageFilter.FilterDespeckleA(Value, BkValue, Count : byte) : cardinal;
var  x, y, yi : longint;
     i, j     : longint;
     n, l     : integer;
     pLines   : PMatrixB;
     pFilter  : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     NoPix    : integer;
     Counter  : cardinal;
begin
  Counter := 0;
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pFilter, (FKernelHeight + 1) * SizeOf(PVectorB));
  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];

       end;
       pFilter[i] := pLines[i];
       inc(yi);
    end;
    pFilter[Fkh] := pLines[Fkh];

    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          l := 1;
          if (pFilter[l]^[x+1] = Value) // Is centre pixel equal to Value?
          then begin
               // Filter kernel.
               NoPix := 0;
               for j := 0 to Fkh
               do begin
                  for i := x to x + Fkw
                  do begin
                     if (pFilter[j]^[i] = Value)
                     then inc(NoPix);
                  end;
               end;

               if (NoPix >= Count)
               then pRes[x] := Value
               else begin
                    pRes[x] := BkValue;
                    inc(Counter);
               end;
          end;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       // Rotate pointers in pFilter.
       pFilter[Fkh+1] := pFilter[0];
       for l := 0 to Fkh
       do pFilter[l] := pFilter[l+1];
       pFilter[Fkh] := pFilter[Fkh+1];

       inc(yi);
    end;
  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
    FreeMem(pFilter);
  end;
  Result := Counter;
end; // TmcmImageFilter.FilterDespeckleA.


procedure TmcmImageFilter.DespeckleA;
var Count  : cardinal;
    Source : TKernelImage;
begin
  if (FSrcImage[0].pDib <> FResImage.pDib)
  then begin
       Source := FSrcImage[0];
       try
         FResImage.Assign(FSrcImage[0]);

         if (FKernelWidth < 3)
         then FKernelWidth := 3;
         FilterHeight := FKernelWidth;

         FIterate := 0;
         Count := 1;
         while (Count > 0) and (FIterate < FMaxIterations)
         do begin
            inc(FIterate);
            Count := FilterDespeckleA(0, 255, 4);
            FSrcImage[0] := FResImage; // Temporarily set Result as Source image.
            Count := Count + FilterDespeckleA(255, 0, 4);
         end;
       finally
         FSrcImage[0] := Source;
       end;
  end
  else ;
end; // TmcmImageFilter.DespeckleA.


function TmcmImageFilter.FilterDespeckleB(Value, BkValue, Count : byte) : cardinal;
var  x, y, yi : longint;
     i, j     : longint;
     n, l     : integer;
     pLines   : PMatrixB;
     pFilter  : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     Fkw      : longint;
     NoPix    : integer;
     k        : word;
     a        : array[0..8] of boolean;
     Counter  : cardinal;
begin
  Counter := 0;
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pFilter, (FKernelHeight + 1) * SizeOf(PVectorB));
  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];

       end;
       pFilter[i] := pLines[i];
       inc(yi);
    end;
    pFilter[Fkh] := pLines[Fkh];

    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          l := 1;
          if (pFilter[l]^[x+1] <> Value) // Is centre pixel equal to Value?
          then begin
               // Filter kernel.
               k := 0;
               NoPix := 0;
               for j := 0 to Fkh
               do begin
                  for i := x to x + Fkw
                  do begin
                     if (pFilter[j]^[i] = Value)
                     then begin
                          a[k] := True;
                          inc(NoPix);
                     end
                     else a[k] := False;
                     inc(k);
                  end;
               end;

               // Pattern
               //
               // a[0]  a[1]  a[2]
               // a[3]  a[4]  a[5]
               // a[6]  a[7]  a[8]
               //
               // Bit pattern excluding centre pixel
               // MSB         LSB
               //  8 7 6 5 3 2 1 , Index's from a[]
               if (NoPix > 3) // Is centre pixel equal to Value?
               then begin
                    // . v .
                    // . . .
                    // . v .
                    if (a[1] and a[7]) and
                       ( ( (a[0] and a[3]) or (a[3] and a[6]) ) or
                         ( (a[2] and a[5]) or (a[5] and a[8]) ) )
                    then begin
                         pRes[x] := Value;
                         inc(Counter);
                    end
                    else begin
                         // . . .
                         // v . v
                         // . . .
                         if (a[3] and a[5]) and
                            ( ( (a[0] and a[1]) or (a[1] and a[2]) ) or
                              ( (a[6] and a[7]) or (a[7] and a[8]) ) )
                         then begin
                              pRes[x] := Value;
                              inc(Counter);
                         end;
                    end;
               end;
          end;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       // Rotate pointers in pFilter.
       pFilter[Fkh+1] := pFilter[0];
       for l := 0 to Fkh
       do pFilter[l] := pFilter[l+1];
       pFilter[Fkh] := pFilter[Fkh+1];

       inc(yi);
    end;

  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
    FreeMem(pFilter);
  end;
  Result := Counter;
end; // TmcmImageFilter.FilterDespeckleB.


procedure TmcmImageFilter.DespeckleB;
var Count  : cardinal;
    Source : TKernelImage;
begin
  if (FSrcImage[0].pDib <> FResImage.pDib)
  then begin
       Source := FSrcImage[0];
       try
         FResImage.Assign(FSrcImage[0]);

         if (FKernelWidth < 3)
         then FKernelWidth := 3;
         FilterHeight := FKernelWidth;

         FIterate := 0;
         Count := 1;
         while (Count > 0) and (FIterate < FMaxIterations)
         do begin
            inc(FIterate);
            Count := FilterDespeckleB(255, 0, 4);
            FSrcImage[0] := FResImage; // Temporarily set Result as Source image.
            Count := Count + FilterDespeckleB(0, 255, 4);
         end;
       finally
         FSrcImage[0] := Source;
       end;
  end
  else ;
end; // TmcmImageFilter.DespeckleB.


function TmcmImageFilter.FilterDespeckleC(Value, BkValue, Count : byte) : cardinal;
var  x, y, yi : longint;
     i, j     : longint;
     n, l     : integer;
     pLines   : PMatrixB;
     pFilter  : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     {
     Fkw      : longint;
     NoPix    : integer;
     k        : word;
     a        : array[0..8] of boolean;
     }
     Counter  : cardinal;
begin
  Counter := 0;
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
//  Fkw := FKernelWidth;
//  dec(Fkw);

  GetMem(pFilter, (FKernelHeight + 1) * SizeOf(PVectorB));
  GetMem(pLines, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLines[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[FNeighbourWidth];
          pLines[i]^[FNeighbourWidth+Fiw+1+j] := pLines[i]^[FNeighbourWidth+Fiw];

       end;
       pFilter[i] := pLines[i];
       inc(yi);
    end;
    pFilter[Fkh] := pLines[Fkh];

    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[FNeighbourWidth];
          pLines[n]^[FNeighbourWidth+Fiw+1+j] := pLines[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          l := 1;

          if (pFilter[l]^[x+1] = Value) // Is centre pixel equal to Value?
          then begin
{
               // Filter kernel.
               NoPix := 0;
               for j := 0 to Fkh
               do begin
                  for i := x to x + Fkw
                  do begin
                     if (pFilter[j]^[i] = Value)
                     then inc(NoPix);
                  end;
               end;

               if (NoPix >= Count)
               then pRes[x] := Value
               else ; //pRes[x] := BkValue;
}
          end
          else begin

               // Test diagonal pixel
               // v - Value is required
               // o - is option, but one must exist
               //
               // Pattern
               //  x =      0 1 2
               //
               //  l = 0,   . v .
               //  l = 1,   o . o
               //  l = 2,   . v .
               //
               if (pFilter[l-1]^[x+1] = Value) and (pFilter[l+1]^[x+1] = Value) and
                  ((pFilter[l]^[x] = Value) or (pFilter[l]^[x+2] = Value))
               then begin
                    pRes[x] := Value;
                    inc(Counter);
               end
               // Pattern
               //  l = 0,   . o .
               //  l = 1,   v . v
               //  l = 2,   . o .
               //
               else if (pFilter[l]^[x] = Value) and (pFilter[l]^[x+2] = Value) and
                       ((pFilter[l-1]^[x+1] = Value) or (pFilter[l+1]^[x+1] = Value))
                       then begin
                            pRes[x] := Value;
                            inc(Counter);
                       end
               // Pattern
               //  l = 0,   v . o
               //  l = 1,   . . .
               //  l = 2,   o . v
               //
                       else if (pFilter[l-1]^[x] = Value) and (pFilter[l+1]^[x+2] = Value) and
                               ((pFilter[l+1]^[x] = Value) or (pFilter[l-1]^[x+2] = Value))
                            then begin
                                 pRes[x] := Value;
                                 inc(Counter);
                            end
               // Pattern
               //  l = 0,   o . v
               //  l = 1,   . . .
               //  l = 2,   v . o
               //
                            else if (pFilter[l-1]^[x+2] = Value) and (pFilter[l+1]^[x] = Value) and
                                    ((pFilter[l]^[x] = Value) or (pFilter[l+1]^[x+2] = Value))
                                 then begin
                                      pRes[x] := Value;
                                      inc(Counter);
                                 end;

          end; // else..

          
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       // Rotate pointers in pFilter.
       pFilter[Fkh+1] := pFilter[0];
       for l := 0 to Fkh
       do pFilter[l] := pFilter[l+1];
       pFilter[Fkh] := pFilter[Fkh+1];

       inc(yi);
    end;

  finally
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
    FreeMem(pFilter);
  end;
  Result := Counter;
end; // TmcmImageFilter.FilterDespeckleC.


procedure TmcmImageFilter.DespeckleC;
var Count  : cardinal;
    Source : TKernelImage;
begin
  if (FSrcImage[0].pDib <> FResImage.pDib)
  then begin
       Source := FSrcImage[0];
       try
         FResImage.Assign(FSrcImage[0]);

         if (FKernelWidth < 3)
         then FKernelWidth := 3;
         FilterHeight := FKernelWidth;

         FIterate := 0;
         Count := 1;
         while (Count > 0) and (FIterate < FMaxIterations)
         do begin
            inc(FIterate);
            Count := FilterDespeckleC(255, 0, 4);
            FSrcImage[0] := FResImage; // Temporarily set Result as Source image.
            Count := Count + FilterDespeckleC(0, 255, 4);
         end;
       finally
         FSrcImage[0] := Source;
       end;
  end
  else ;
end; // TmcmImageFilter.DespeckleC.


//------------------------------------------------------------------------------
// kFill filter
// Description:
//   Removes salt and pepper noice from a B&W image. Suitable as preprocessor
//   for OCR.
// Algorithme developed by:
//   O´Gorman in 1992.
//------------------------------------------------------------------------------

const BairdMarker : byte = 2; // Used with Deskew.

procedure TmcmImageFilter.kFillMark(AImage : TmcmImage; x, y : integer; Value, BkValue : byte);
var LPixON  : byte;
    LPixOFF : byte;

     function GetPrevious(sx, sy : integer; Up : boolean) : byte;
     var a          : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Get current line source address.
       Apt := AImage.ScanLine[sy];

       // Search for features above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       a := sx;
       while (a < AImage.Width) and
             (Apt^[a] = LPixON) and
             ((Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF))
       do inc(a);

       if (Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF)
       then Result := LPixOFF
       else Result := Bpt^[a];
     end; // GetPrevious.


     function HasNeighbour(sx, sy, ex, ey : integer; Up : boolean; Value : byte) : byte;
     var a, am, ap  : integer;
         maxx       : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Calculate Current Line source address.
       Apt := AImage.ScanLine[sy];

       // Search for neighbours above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       // Start search one pixel "early".
       a := sx - 1;
       if (a < 0)
       then a := 0;

       am := a - 1;
       if (am < 0)
       then am := 0;

       ap := a + 1;
       if (ap >= AImage.Width)
       then ap := AImage.Width - 1;

       maxx := ex + 1;
       if (maxx > AImage.Width)
       then maxx := AImage.Width;

       // Search for neighbour pixel being "ON" next to a pixel having feature
       // "Value".
       while (a < maxx{AImage.Width}) and
             Not(((Apt^[a] = Value) or (Apt^[am] = Value) or (Apt^[ap] = Value)) and
                 (Bpt^[a] = LPixON))
       do begin
          inc(a);
          am := a - 1;
          inc(ap);
          if (ap >= AImage.Width)
          then ap := AImage.Width - 1;
       end;

       // Return pixel value.
       if (a < AImage.Width)
       then Result := Bpt^[a]
       else Result := LPixOFF;
     end; // HasNeighbour.


     function FirstLine(var sx, sy, ex, ey : integer) : longint;
     var a          : integer;
         Count      : longint;
         ALine      : PvectorB;
     begin
       Count := 0;

       // Calculate Current Line source address.
       ALine := AImage.ScanLine[sy];

       // Fill coherent "ON"-pixels with feature "Value".
       a := sx;
       while (0 <= a) and (a < AImage.Width) and (ALine^[a] = LPixON)
       do begin
          ALine^[a] := BairdMarker;
          inc(a);
          inc(Count);
       end;
       dec(a);
       if (a < 0)
       then a := 0;
       ex := a;
       Result := Count;
     end; // FirstLine.


     function FillLine(var sx, sy, ex, ey : integer; Up : boolean) : longint;
     var a, b, c, d : integer;
         bm, bp     : integer;
         Count      : longint;
         ALine      : PvectorB;
         BLine      : PvectorB;
     begin
       Count := 0;

       dec(sx);
       if (sx < 0)
       then sx := 0;
       inc(ex);
       if (ex > (AImage.Width - 1))
       then ex := AImage.Width - 1;

       // Get current line source address.
       ALine := AImage.ScanLine[sy];

       if Up
       then BLine := AImage.ScanLine[sy-1]
       else BLine := AImage.ScanLine[sy+1];

       a := sx;
       d := sx;
       while (a <= ex) and (ALine^[a] = LPixOFF)
       do inc(a);

       if (a <= ex)
       then begin
            b  := a;
            bm := b - 1;
            if (bm < 0)
            then bm := 0;

            bp := b + 1;
            if (bp >= AImage.Width)
            then bp := AImage.Width - 1;

            while (b <= ex)
            do begin
               if (ALine^[b] = LPixON)
               then begin
                    if (BLine^[b]  = BairdMarker) or
                       (BLine^[bm] = BairdMarker) or
                       (BLine^[bp] = BairdMarker)
                    then begin
                         c := b;
                         while (c >= 0) and (ALine^[c] = LPixON)
                         do begin
                            ALine^[c] := BairdMarker;
                            dec(c);
                            inc(Count);
                         end;
                         if (a > c)
                         then a := c + 1;
                         if (d <= c)
                         then d := c + 1;

                         inc(b);
                         while (b < AImage.Width) and (ALine^[b] = LPixON)
                         do begin
                            ALine^[b] := BairdMarker;
                            d := b;
                            inc(b);
                            inc(Count);
                         end;
                    end
                    else inc(b);
               end
               else inc(b);
               bm := b - 1;
               bp := b + 1;
               if (bp >= AImage.Width)
               then bp := AImage.Width - 1;
            end;
       end;

       if (Count = 0)
       then begin
            a := ex;
            d := 0;
       end;

       if (a < 0)
       then a := 0;
       sx := a;
       ex := d;
       ey := sy;
       Result := Count;
     end; // FillLine.


var NIndex     : longint;
    ix, iy     : integer;
    ox, oy     : integer;
    Dir        : boolean;
begin
  LPixON  := Value;
  LPixOFF := BkValue;

  NIndex := 0;

  ix := x;
  iy := y;
  ox := FSrcWidth;
  oy := y;

  // Feature fill current line segment.
  FirstLine(ix, iy, ox, oy);

  // Save x and/or y position if it marks a new boundary of the feature.
  if (FBlx > ox)
  then FBlx := ox;
  if (FBby < iy)
  then FBby := iy;
  if (FBrx < ox)
  then FBrx := ox;
  if (FBty > iy)
  then FBty := iy;

  inc(iy);

  Dir := True;
  repeat
    // If at end of feature (top or bottom) check for saved
    // lines to search for in opersite direction.
    if Not(ix <= ox) or (0 > iy) or (iy >= FSrcHeight)
    then begin
         inc(iy);
         if (NIndex > 0)
         then begin
              dec(NIndex);
              ix := FNeighbour[NIndex].x1;
              iy := FNeighbour[NIndex].y1;
              ox := FNeighbour[NIndex].x2;
              oy := FNeighbour[NIndex].y2;
              Dir := FNeighbour[NIndex].Up;
         end;
    end;

    while (ix <= ox) and (0 <= iy) and (iy < FSrcHeight)
    do begin
       // Fill line segments belonging to current feature.
       if (FillLine(ix, iy, ox, oy, Dir) = 0)
       then begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // If at end of feature (top or bottom) check for saved
            // lines to search for in opersite direction.
            inc(iy);
            if (NIndex > 0)
            then begin
                 dec(NIndex);
                 ix := FNeighbour[NIndex].x1;
                 iy := FNeighbour[NIndex].y1;
                 ox := FNeighbour[NIndex].x2;
                 oy := FNeighbour[NIndex].y2;
                 Dir := FNeighbour[NIndex].Up;
            end;
       end
       else begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // Check if line-segments in the line in opersite
            // direction (the line above or below) belongs to
            // the current feature (pixels = PixON).
            if (HasNeighbour(ix, iy, ox, oy, Not(Dir), BairdMarker) = LPixON)
            then begin
                 // Increase allocated memory if required.
                 if (NIndex >= FNeighbourSize)
                 then begin
                      ReallocMem(FNeighbour, (FNeighbourSize + FSrcHeight) * SizeOf(TNeighbour));
                      if (FNeighbour <> Nil)
                      then FNeighbourSize := FNeighbourSize + FSrcHeight
                      else FNeighbourSize := 0;
                 end;

                 // Save left- and rightmost x coordinate for
                 // "later" filling.
                 if (NIndex < FNeighbourSize)
                 then begin
                      FNeighbour[NIndex].x1 := ix;
                      FNeighbour[NIndex].x2 := ox;
                      if Dir
                      then begin
                           FNeighbour[NIndex].y1 := iy - 1;
                           FNeighbour[NIndex].y2 := iy - 1;
                           inc(iy);
                      end
                      else begin
                           FNeighbour[NIndex].y1 := iy + 1;
                           FNeighbour[NIndex].y2 := iy + 1;
                           dec(iy);
                      end;
                      FNeighbour[NIndex].Up := Not(Dir);

                      if (0 <= FNeighbour[NIndex].y1) and (FNeighbour[NIndex].y1 < FSrcHeight)
                      then Inc(NIndex);
                 end;
            end
            else begin
                 if Dir
                 then inc(iy)
                 else dec(iy);
            end;
       end;
    end;
  until (NIndex <= 0);
end; // TmcmImageFilter.kFillMark.


function TmcmImageFilter.kFillFilter(Value, BkValue : byte) : cardinal;
var  x, y       : longint;
     i, j       : longint;
     pLine      : PMatrixB;
     pRes       : PVectorB;
     Fiw        : longint; // Image height - 1
     Fih        : longint; // Image width - 1
     Fkh        : longint; // Filter kernel height - 1
     Fkw        : longint; // Filter kernel width - 1
     CoreHeight : integer;
     CoreWidth  : integer;
     CoreConst  : integer;
     //FltCenter  : integer;
     n, r, c    : longint;
     Count      : cardinal;

     procedure kMark(i, j, c : integer);
     var k  : byte;
         di : longint;
     begin
        // Find number of neighbour pixels of same value, in directions
        // N, S, E and W.
        di := i + x;
        k := pLine[j]^[di];
        pLine[j]^[di] := c;
        if (i+1 < FKernelWidth)
        then if (pLine[j]^[di+1] = k)
             then kMark(i+1, j, c);
        if (i-1 >= 0)
        then if (pLine[j]^[di-1] = k)
             then kMark(i-1, j, c);
        if (j+1 < FKernelHeight)
        then if (pLine[j+1]^[di] = k)
             then kMark(i, j+1, c);
        if (j-1 >= 0)
        then if (pLine[j-1]^[di] = k)
             then kMark(i, j-1, c);
     end;

 LABEL NextPixel;

var pThisLine : PVectorB;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  Count := 0;
  CoreConst  := 3 * FKernelWidth - FKernelWidth div 3;
  //FltCenter  := FKernelWidth div 2;
  CoreHeight := FKernelWidth - 4;
  CoreWidth  := FKernelWidth - 4;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  try
    for i := 0 to Fkh
    do pLine[i] := FSrcImage[0].ScanLine[i];

    for y := 0 to Fih - FKernelHeight // All lines in image
    do begin
       pRes := FResImage.ScanLine[y+2];

       for x := 0 to Fiw - FKernelWidth // All pixels in line
       do begin
          // Check Core pixels - are they all different from "Value".
          for j := 2 to (CoreHeight + 1)
          do begin
             for i := 2 to (CoreWidth + 1)
             do begin
                if (pLine[j]^[i+x] = Value)
                then goto NextPixel;
             end;
          end;

          // Compute neighbourhood parameters:
          // n - Number of pixels equal to "Value"
          n := 0;
          for j := 0 to Fkh
          do begin
             for i := 0 to Fkw
             do begin
                if (j < 2) or (j > FKernelHeight - 3) or
                   (i < 2) or (i > FKernelWidth - 3)
                then if (pLine[j]^[i+x] = Value)
                     then inc(n);
             end;
          end;

          // r - Number of corner pixels equal to "Value"
          r := 0;
          if (pLine[0]^[x] = Value)
          then inc(r);
          if (pLine[0]^[x+Fkw] = Value)
          then inc(r);
          if (pLine[Fkh]^[x] = Value)
          then inc(r);
          if (pLine[Fkh]^[x+Fkw] = Value)
          then inc(r);

          // c - Number of connected regions (pixels) with value equal to "Value"
          c := 2;

          for j := 0 to Fkh
          do begin
             for i := 0 to Fkw
             do begin
                if (j < 2) or (j > (FKernelHeight - 3)) or
                   (i < 2) or (i > (FKernelWidth - 3))
                then if (pLine[j]^[i+x] = Value)
                     then begin
                          // kMark(i, j, BairdMarker);  // c);
                          kFillMark(FSrcImage[0], i+x, j+y, Value, BkValue);
                          inc(c);
                     end;
             end;
          end;

          dec(c, 2);

          if (c > 0)
          then begin
               // Clear markers (from kMark).
               for j := 0 to (FSrcHeight - 1)
               do begin
                  pThisLine := FSrcImage[0].ScanLine[j];
                  for i := 0 to (FSrcWidth - 1)
                  do begin
                     if (pThisLine^[i] = BairdMarker)
                     then pThisLine^[i] := Value;
                  end;
               end;

               if (c = 1)
               then begin
                    if ((n > CoreConst) or ((n = CoreConst) and (r = 2)))
                    then begin
                         for j := 2 to (CoreHeight + 1)
                         do begin
                            for i := 2 to (CoreWidth + 1)
                            do pRes[i+x] := Value;
                         end;
                         inc(Count);
                    end;
               end;
          end;

          NextPixel:
       end;

       // Shift line pointers.
       for i := 0 to (FKernelWidth - 2)
       do pLine[i] := pLine[i+1];

       // Add new bottom line pointer
       pLine[Fkw] := FSrcImage[0].ScanLine[y+FKernelWidth];
    end;
  finally
    FreeMem(pLine);
  end;

  Result := Count;
end; // TmcmImageFilter.kFillFilter.


procedure TmcmImageFilter.kFill;
var Count : cardinal;
begin
  if (FSrcImage[0].pDib <> FResImage.pDib)
  then begin

  // FNeighbour is a stack of coordinates used to link parts of a feature.
  FNeighbourSize := 4 * FSrcHeight;
  GetMem(FNeighbour, FNeighbourSize * SizeOf(TNeighbour));
  try

       if (FKernelWidth < 5)
       then FKernelWidth := 5; // Make sure that the filter is min. 5x5.
       FilterHeight := FKernelWidth;



       FResImage.Assign(FSrcImage[0]);

       FIterate := 0;
       Count := 1;
//       FResImage.FillAll(255);
       while (Count > 0) and (FIterate < FMaxIterations)
       do begin
          inc(FIterate);



          Count := kFillFilter(0, 255);         // Remove pepper
          if (Count > 0)
          then FSrcImage[0].Assign(FResImage);
          Count := Count + kFillFilter(255, 0); // Remove salt

          if (Count > 0)
          then FSrcImage[0].Assign(FResImage);
       end;

  finally
    if (FNeighbour <> Nil)
    then FreeMem(FNeighbour);
  end;

  end
  else ;
end; // TmcmImageFilter.kFill.


//------------------------------------------------------------------------------
// Maximum filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Maximum;
var  x, y, yi  : longint;
     i, j, n   : longint;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     MaxVal    : word;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    n  := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          MaxVal := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                if (MaxVal < byte(pLine[j]^[i]))
                then MaxVal := byte(pLine[j]^[i]);
             end;
          end;
          pRes[x] := MaxVal;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.Maximum.


//------------------------------------------------------------------------------
// MaxMin filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.MaxMin(Kernel : PVectorI);
var  x, y, yi  : longint;
     i, j, k   : longint;
     m, n      : integer;
     l, c      : integer;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     AveVal    : integer;
     MinVal    : integer;
     MaxVal    : integer;
     dMin      : integer;
     dMax      : integer;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  FFactor := FKernelWidth * FKernelHeight;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;   // m = index to buffer for next line.
    n := Fkh; // n = index to last line in buffer.
    c := FNeighbourHeight; // c = index to centre line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          AveVal := 0;
          MaxVal := 0;
          MinVal := 32768;
          l := m;
          k := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                AveVal := AveVal + pLine[l]^[i];
                if (Kernel[k] = 0)
                then begin
                     if (MinVal > byte(pLine[l]^[i]))
                     then MinVal := byte(pLine[l]^[i]);
                     if (MaxVal < byte(pLine[l]^[i]))
                     then MaxVal := byte(pLine[l]^[i]);
                end;
                inc(k);
             end;
             inc(l);
             if (l = FKernelHeight)
             then l := 0;
          end;

          if ((MaxVal - MinVal) > FHysteresis)
          then begin
               dMin := byte(pLine[c]^[x+FNeighbourWidth]) - MinVal;
               dMax := MaxVal - byte(pLine[c]^[x+FNeighbourWidth]);
               if (dMin < dMax)
               then pRes[x] := Byte(MinVal)
               else pRes[x] := Byte(MaxVal);
          end
          else pRes[x] := Byte(AveVal div FFactor);
       end;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(c);
       if (c = FKernelHeight)
       then c := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.MaxMin.


//------------------------------------------------------------------------------
// Median filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Median;
var  x, y, yi  : longint;
     i, j      : longint;
     m, n      : integer;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     // MedianPos : word;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);
  // MedianPos := FKernelSize shr 1;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    n  := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          m := 0;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                Flt[m] := byte(pLine[j]^[i]);
                inc(m);
             end;
          end;
          //QSort(Flt^, 0, FKernelSize-1);
          //pRes[x] := Flt^[MedianPos];
          pRes[x] := QuickSelect(Flt, FKernelSize); // 63 % faster than QSort
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.Median.


procedure TmcmImageFilter.MedianMaxMin;
// This operator removes isolated bright spots. Preserves overall sharpness.
//
// ---------------------
// |   |   | f |   |   |
// ---------------------
// |   |   | g |   |   |
// ---------------------
// | a | b | c | d | e |
// ---------------------
// |   |   | h |   |   |
// ---------------------
// |   |   | i |   |   |
// ---------------------
//
// MaxMin = max(min(a,b,c), min(b,c,d), min(c,d,e),... min(c,h,i))
//
var  x, y, yi  : longint;
     i, j, k   : longint;
     c, f, fy  : integer;
     m, n      : integer;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     NoValues  : integer;
     Value     : word;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  NoValues := FKernelHeight + FKernelWidth - 2;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;    // m = index to buffer for next line.
    n  := Fkh; // n = index to last line in buffer.
    c := FNeighbourHeight; // c = index to centre line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          f := 0;

          // Get horizontal portion of cross.
          for i := x to x + Fkw
          do begin
             Flt[f] := byte(pLine[c]^[i]);
             inc(f);
          end;

          // Get vertical portion of cross.
          i := x + FNeighbourWidth;
          fy := m;
          for j := 0 to Fkh
          do begin
             Flt[f] := byte(pLine[fy]^[i]);
             inc(f);

             inc(fy);
             if (fy = FKernelHeight)
             then fy := 0;
          end;

          for i := 0 to NoValues
          do begin
             Value := 255;
             k := i;
             for j := 0 to 2
             do begin
                if (Value > Flt^[k])
                then Value := Flt^[k];
                inc(k);
             end;

             Flt^[i] := Value;
          end;
          QSort(Flt^, 0, NoValues);
          pRes[x] := Flt^[NoValues];
       end;

       inc(c);
       if (c = FKernelHeight)
       then c := 0;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.MedianMaxMin.


procedure TmcmImageFilter.MedianMinMax;
// This operator removes isolated dark spots. Preserves overall sharpness.
//
// ---------------------
// |   |   | f |   |   |
// ---------------------
// |   |   | g |   |   |
// ---------------------
// | a | b | c | d | e |
// ---------------------
// |   |   | h |   |   |
// ---------------------
// |   |   | i |   |   |
// ---------------------
//
// MinMax = min(max(a,b,c), max(b,c,d), max(c,d,e),... max(c,h,i))
//
var  x, y, yi  : longint;
     i, j, k   : longint;
     c, f, fy  : integer;
     m, n      : integer;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     NoValues  : integer;
     Value     : word;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  NoValues := FKernelHeight + FKernelWidth - 2;

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    m := 0;    // m = index to buffer for next line.
    n  := Fkh; // n = index to last line in buffer.
    c := FNeighbourHeight; // c = index to centre line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          f := 0;

          // Get horizontal portion of cross.
          for i := x to x + Fkw
          do begin
             Flt[f] := byte(pLine[c]^[i]);
             inc(f);
          end;

          // Get vertical portion of cross.
          i := x + FNeighbourWidth;
          fy := m;
          for j := 0 to Fkh
          do begin
             Flt[f] := byte(pLine[fy]^[i]);
             inc(f);

             inc(fy);
             if (fy = FKernelHeight)
             then fy := 0;
          end;

          for i := 0 to NoValues
          do begin
             Value := 0;
             k := i;
             for j := 0 to 2
             do begin
                if (Value < Flt^[k])
                then Value := Flt^[k];
                inc(k);
             end;

             Flt^[i] := Value;
          end;
          QSort(Flt^, 0, NoValues);
          pRes[x] := Flt^[0];
       end;

       inc(c);
       if (c = FKernelHeight)
       then c := 0;

       inc(m);
       if (m = FKernelHeight)
       then m := 0;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.MedianMinMax.


//------------------------------------------------------------------------------
// Minimum filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Minimum;
var  x, y, yi  : longint;
     i, j, n   : longint;
     pLine     : PMatrixB;
     pRes      : PVectorB;
     Fiw       : longint; // Image height - 1
     Fih       : longint; // Image width - 1
     Fkh       : longint; // Filter kernel height - 1
     Fkw       : longint;
     Flt       : PVectorB;
     MinVal    : word;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);

  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));
  for i := 0 to (FKernelHeight - 1)
  do GetMem(pLine[i], (FSrcImage[0].Width + FKernelWidth) * SizeOf(PVectorB));
  GetMem(Flt, FKernelSize * SizeOf(byte));

  try
    yi := -FNeighbourHeight;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLine[i]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[i]^[j] := pLine[i]^[FNeighbourWidth];
          pLine[i]^[FNeighbourWidth+Fiw+1+j] := pLine[i]^[FNeighbourWidth+Fiw];
       end;
       inc(yi);
    end;

    n  := Fkh; // n = index to last line in buffer.
    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLine[n]^[FNeighbourWidth], FSrcImage[0].ScanLine[yi], FSrcImage[0].Width * SizeOf(byte));
       for j := 0 to (FNeighbourWidth - 1)
       do begin
          pLine[n]^[j] := pLine[n]^[FNeighbourWidth];
          pLine[n]^[FNeighbourWidth+Fiw+1+j] := pLine[n]^[FNeighbourWidth+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          // Filter kernel.
          MinVal := $FFFF;
          for j := 0 to Fkh
          do begin
             for i := x to x + Fkw
             do begin
                if (MinVal > byte(pLine[j]^[i]))
                then MinVal := byte(pLine[j]^[i]);
             end;
          end;
          pRes[x] := MinVal;
       end;

       inc(n);
       if (n = FKernelHeight)
       then n := 0;

       inc(yi);
    end;
  finally
    FreeMem(Flt);
    for i := 0 to (FKernelHeight - 1)
    do FreeMem(pLine[i]);
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.Minimum.


//------------------------------------------------------------------------------
// Mosaic filter.
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Mosaic;
var  x, y    : longint;
     dx, dy  : longint;
     i, j    : longint;
     pLine   : PMatrixB;
     pRes    : PVectorB;
     pCopy   : PVectorB;
     SumPix  : cardinal;
     Fiw     : longint; // Image height - 1
     Fih     : longint; // Image width - 1
     Fkh     : longint; // Filter kernel height - 1
     Fkw     : longint;
begin
  Fih := FSrcImage[0].Height;
  dec(Fih);
  Fiw := FSrcImage[0].Width;
  dec(Fiw);
  Fkh := FKernelHeight;
  dec(Fkh);
  Fkw := FKernelWidth;
  dec(Fkw);
  GetMem(pLine, FKernelHeight * SizeOf(PVectorB));

  try
    y := 0;
    while (y < FSrcImage[0].Height)
    do begin
       for i := 0 to (FKernelHeight - 1)
       do pLine[i] := FSrcImage[0].ScanLine[y+i];

       pRes := FResImage.ScanLine[y];

       dy := Fkh;
       if ((y + dy) > Fih)
       then dy := Fih - y;

       x := 0;
       while (x < FSrcImage[0].Width)
       do begin
          // Filter kernel.
          dx := Fkw;
          if (x + dx > Fiw)
          then dx := Fiw - x;

          SumPix := 0;
          for j := 0 to dy
          do begin
             for i := x to x + dx
             do SumPix := SumPix + byte(pLine[j]^[i]);
          end;
          SumPix := SumPix div cardinal((dx + 1) * (dy + 1));

          for i := 0 to dx
          do begin
             pRes[x] := SumPix;
             inc(x);
          end;
       end;

       for i := 1 to dy
       do begin
          inc(y);
          pCopy := FResImage.ScanLine[y];
          CopyMemory(pCopy, pRes, FSrcImage[0].Width);
       end;

       inc(y);
    end;
  finally
    FreeMem(pLine);
  end;
end; // TmcmImageFilter.Mosaic.


//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

procedure TmcmImageFilter.Dbl2IntKernel(KernelIndex : integer;
                                        DblKernel   : array of double;
                                        DblSize     : integer);
// Converts double (float values) to integer values in the filter kernel.
var x, y    : integer;
    i, j    : integer;
    SumPos  : integer;
    SumNeg  : integer;
    AbsMin  : double;
    DiffMin : double;
    Diff    : double;
    dFactor : double;
begin
  SumPos := 0;
  SumNeg := 0;

  // Find the smallest absolut kernel value.
  AbsMin := 100000.0;
  for y := 0 to (FKernelHeight - 1)
  do begin
     for x := 0 to (FKernelWidth - 1)
     do begin
        if (DblSize > FKernelWidth)
        then i := y * FKernelWidth + x
        else begin
             if (FKernelHeight = 1)
             then i := x
             else i := y;
        end;
        if (Abs(DblKernel[i]) > 0.00001)
        then if (AbsMin > Abs(DblKernel[i]))
             then AbsMin := Abs(DblKernel[i]);
     end;
  end;

  DiffMin := 100000.0;
  for i := 0 to (DblSize - 1)
  do for j := 0 to (DblSize - 1)
     do if (i <> j)
        then begin
             Diff := abs(DblKernel[i] - DblKernel[j]);
             if (Diff <> 0.0) and (DiffMin > Diff)
             then DiffMin := Diff;
        end;
  if (AbsMin < DiffMin)
  then dFactor := (1.0 / AbsMin)
  else dFactor := (1.0 / DiffMin);

  for y := 0 to (FKernelHeight - 1)
  do begin
     for x := 0 to (FKernelWidth - 1)
     do begin
        if (DblSize > FKernelWidth)
        then i := y * FKernelWidth + x
        else begin
             if (FKernelHeight = 1)
             then i := x
             else i := y;
        end;
        Kernel[KernelIndex, x, y] := Round(dFactor * DblKernel[i]);

        if (Kernel[KernelIndex, x, y] > 0)
        then SumPos := SumPos + Kernel[KernelIndex, x, y];
        if (Kernel[0, x, y] < 0)
        then SumNeg := SumNeg + Kernel[KernelIndex, x, y];
     end;
  end;
  FFactor := Round(SumPos - SumNeg);
end; // TmcmImageFilter.Dbl2IntKernel.


//------------------------------------------------------------------------------
// Gauss methods.
//------------------------------------------------------------------------------

function Norm(x, y : double) : double;
begin
  Result := Sqrt(x * x + y * y);
end; // Norm.


function Distance(a, b, c, d : double) : double;
begin
  Result := Norm(a - c, b - d);
end; // Distance.


function Gauss(Distance, Sigma : double) : double;
begin
  Result := exp((-Distance * Distance) / (2.0 * Sigma * Sigma));
end; // Gauss.


function MeanGauss(Distance, Sigma : double) : double;
var Res : double;
begin
  Res := (Gauss(Distance, Sigma) +
          Gauss(Distance - 0.5, Sigma) +
          Gauss(Distance + 0.5, Sigma)) / 3.0;
  Res := Res / (pi * 2.0 * Sigma * Sigma);
  Result := Res;
end; // MeanGauss.


function DerivativeGauss(Distance, Sigma : double) : double;
begin
  // First derivative of Gauss.
  Result := - Distance * Gauss(Distance, Sigma) / (Sigma * Sigma);
end; // DerivativeGauss.


function LaplacianOfGauss(Distance, Sigma : double) : double;
var Res    : double;
    Dist2  : double;
    Sigma2 : double;
begin
  Res   := Gauss(Distance, Sigma);
  Dist2 := Distance * Distance;
  Sigma2 := Sigma * Sigma;
  Result := (Dist2 - 2 * Sigma2) / (Sigma2 * Sigma2) * Res;
end; // LaplacianOfGauss.


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TmcmImageFilter.SetGaussSD(Value : double);
begin
  if (0.2 <= Value) and (Value <= 5.0)
  then begin
       FGSD := Value;
       if ((FGSD - FDeltaSD) < 0.1)
       then FDeltaSD := FGSD - 0.1;
       if ((FGSD + FDeltaSD) > 5.1)
       then FDeltaSD := 5.1 - FGSD;
  end;
end; // TmcmImageFilter.SetGaussSD.


procedure TmcmImageFilter.SetDeltaSD(Value : double);
begin
  if (0.1 <= Value) and (Value <= 4.8)
  then begin
       FDeltaSD := Value;
       if ((FGSD - FDeltaSD) < 0.1)
       then FDeltaSD := FGSD - 0.1;
       if ((FGSD + FDeltaSD) > 5.1)
       then FDeltaSD := 5.1 - FGSD;
  end;
end; // TmcmImageFilter.SetDeltaSD.

//------------------------------------------------------------------------------
// Marr-Hildreth edge filter
//------------------------------------------------------------------------------

procedure TmcmImageFilter.ZeroCross(AImage : TmcmImage; ZeroValue : byte);
var  x, y, yi : longint;
     i, j     : longint;
     n, l     : integer;
     pLines   : PMatrixB;
     pFilter  : PMatrixB;
     pRes     : PVectorB;
     Fiw      : longint; // Image height - 1
     Fih      : longint; // Image width - 1
     Fkh      : longint; // Filter kernel height - 1
     One      : integer;
begin
  Fih := AImage.Height;
  dec(Fih);
  Fiw := AImage.Width;
  dec(Fiw);
  Fkh := 3;
  dec(Fkh);

  GetMem(pFilter, (3 + 1) * SizeOf(PVectorB));
  GetMem(pLines, 3 * SizeOf(PVectorB));
  for i := 0 to (3 - 1)
  do GetMem(pLines[i], (AImage.Width + 3) * SizeOf(PVectorB));

  One := 1;
  try
    yi := -1;
    for i := 0 to (Fkh - 1)
    do begin
       CopyMemory(@pLines[i]^[One], AImage.ScanLine[yi], AImage.Width * SizeOf(byte));
       for j := 0 to (1 - 1)
       do begin
          pLines[i]^[j] := pLines[i]^[One];
          pLines[i]^[1+Fiw+1+j] := pLines[i]^[1+Fiw];
       end;
       pFilter[i] := pLines[i];
       inc(yi);
    end;
    pFilter[Fkh] := pLines[Fkh];

    n := Fkh; // n = index to last line in buffer.

    for y := 0 to Fih // All lines in image
    do begin
       CopyMemory(@pLines[n]^[One], AImage.ScanLine[yi], AImage.Width * SizeOf(byte));
       for j := 0 to (1 - 1)
       do begin
          pLines[n]^[j] := pLines[n]^[One];
          pLines[n]^[1+Fiw+1+j] := pLines[n]^[1+Fiw];
       end;
       pRes := FResImage.ScanLine[y];

       for x := 0 to Fiw  // All pixels in line
       do begin
          l := 1;

          // Vertical, |
          if ((pFilter[l-1]^[x] < ZeroValue) xor (pFilter[l+1]^[x] < ZeroValue))
          then begin
               pRes[x] := 255;
               Continue;
          end;
          // Horizontal, -
          if ((pFilter[l]^[x] < ZeroValue) xor (pFilter[l]^[x+2] < ZeroValue))
          then begin
               pRes[x] := 255;
               Continue;
          end;
          // Diagonal, /
          if ((pFilter[l+1]^[x] < ZeroValue) xor (pFilter[l-1]^[x+2] < ZeroValue))
          then begin
               pRes[x] := 255;
               Continue;
          end;
          // Diagonal, \
          if ((pFilter[l-1]^[x] < ZeroValue) xor (pFilter[l+1]^[x+2] < ZeroValue))
          then begin
               pRes[x] := 255;
               Continue;
          end;
          pRes[x] := 0;
       end;

       inc(n);
       if (n = 3)
       then n := 0;

       // Rotate pointers in pFilter.
       pFilter[Fkh+1] := pFilter[0];
       for l := 0 to Fkh
       do pFilter[l] := pFilter[l+1];
       pFilter[Fkh] := pFilter[Fkh+1];

       inc(yi);
    end;
  finally
    for i := 0 to (3 - 1)
    do FreeMem(pLines[i]);
    FreeMem(pLines);
    FreeMem(pFilter);
  end;
end; // TmcmImageFilter.ZeroCross.


procedure TmcmImageFilter.MarrFilter(Sigma : double);
var i, j, k, n  : integer;
    GaussWidth  : integer;
    GaussKernel : array[0..DFLT_MAXSIZE*DFLT_MAXSIZE] of double;
begin
  // Calculate gaussian filter size.
  GaussWidth := Round(3.35 * Sigma + 0.33);
  n := 2 * GaussWidth + 1;
  if Not(Odd(n))
  then dec(n);
  SetFilterSize(n);

  for j := 0 to (n - 1)
  do begin
     for i := 0 to (n - 1)
     do begin
        k := j * n + i;
        GaussKernel[k] := LaplacianOfGauss(Distance(i, j, GaussWidth, GaussWidth), Sigma);
     end;
  end;
  Dbl2IntKernel(0, GaussKernel, n * n);

  // Apply Gaussian filter.
  //  FFactor := FFactor div 2;
  FBias := ZeroValue;
  FilterKernel(FKernel[0]);

  {$IFDEF MCMTEST}
    // Save X/Y component
    FResImage.FileSave(TestImageDir + 'MarrHildreth_X' {+ IntToStr(Round(100 * Sigma))} + '.bmp');
  {$ENDIF}

  // Find zero crossings.
  ZeroCross(FResImage, ZeroValue + 4);
  {$IFDEF MCMTEST}
    // Save X/Y component
    FResImage.FileSave(TestImageDir + 'MarrHildreth_X_Zero' {+ IntToStr(Round(100 * Sigma))} + '.bmp');
  {$ENDIF}
end; // TmcmImageFilter.MarrFilter.


function TmcmImageFilter.MarrHildreth;
var pSaveS : TKernelImage;
    pTemp  : TKernelImage;
    pA, pB : PVectorB;
    x, y   : integer;
begin
  FError := EC_OK;
  // FGSD, Gaussian standard deviation.
  if (FGSD > 5.0)
  then FGSD := 5.0;
  if (FGSD <= 5.0)
  then begin
       if CheckSource(0, [IF_GREY8])
       then begin
            CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

            if Assigned(FResImage)
            then begin
                 if (FSrcImage[0].Empty or FResImage.Empty)
                 then begin
                      FError := EC_NOMEMORY;
                      Result := Nil;
                      Exit;
                 end;
            end;
            if (FSrcImage[0].ImageFormat <> FResImage.ImageFormat)
            then begin
                 FError := EC_BADCOLORFORMAT;
                 Result := Nil;
                 Exit;
            end;

            MarrFilter(FGSD - FDeltaSD);

            pSaveS := FResImage;
            pTemp := TKernelImage(TmcmImage.Create);
            pTemp.CopyFormat(pSaveS);
            FResImage := pTemp;

            MarrFilter(FGSD + FDeltaSD);
            FResImage := pSaveS;

            // Merge the two filtered & zero crossed images.
            for y := 0 to (FResImage.Height - 1)
            do begin
               pA := pTemp.ScanLine[y];
               pB := FResImage.ScanLine[y];
               for x := 0 to (FResImage.Width - 1)
               do begin
                  if (pA^[x] > 0) and (pB^[x] > 0)
                  then pB^[x] := 255
                  else pB^[x] := 0;
               end;
            end;

            pTemp.Free;
       end;
  end
  else FError := EC_MARR_DEVIATION;

  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageFilter.MarrHildreth.


//------------------------------------------------------------------------------
// Traced Threshold
//------------------------------------------------------------------------------

procedure TmcmImageFilter.TracedThreshold(    SrcImage  : TKernelImage;
                                              TraceAuto : boolean;
                                              Percent   : word;
                                          var Hi, Lo    : word);
var ImageColor : TmcmImageColor;
    DontCare   : word;
    PctSize    : cardinal;
    PctCount   : cardinal;
    i          : integer;
    Count      : cardinal;
    Max, Min   : integer;
begin
  ImageColor := TmcmImageColor.Create(Nil);
  ImageColor.SourceImage[0] := SrcImage;
  ImageColor.GetHistogram;
  FError := ImageColor.Error;

  if TraceAuto and (FError = EC_OK)
  then begin
       Hi := 255;
       Lo := 0;

       Min := 0;
       Max := (1 shl SrcImage.BitCount) - 1;;

       PctSize := 0;
       for i := 0 to Max
       do PctSize := PctSize + ImageColor.IntHistogram[i];

       // PctSize := SrcImage.DibInfo^.bmiHeader.biSizeImage;
       if (ImageColor.IntHistogram[Max] > PctSize div 5) // more than 20%
       then begin
            PctSize := PctSize - ImageColor.IntHistogram[Max];
            dec(Max);
       end;
       if (ImageColor.IntHistogram[Min] > PctSize div 5) // more than 20%
       then begin
            PctSize := PctSize - ImageColor.IntHistogram[Min];
            inc(Min);
       end;

       if (Percent > 99)
       then Percent := 99;
       PctCount := (PctSize * cardinal(100 - Percent)) div 100;

       if (FError = EC_OK)
       then begin
            Count := 0;
            for i := Max downto Min
            do begin
               Hi := i;
               Count := Count + ImageColor.IntHistogram[i];
               if (Count > PctCount)
               then break;
            end;

            PctCount := (PctCount * (Percent)) div 100;
            Count := 0;
            for i := (Hi - 1) downto Min
            do begin
               Lo := i;
               Count := Count + ImageColor.IntHistogram[i];
               if (Count > PctCount)
               then break;
            end;

            // Make sure that Lo is always above the lowest present intensity, and
            // Hi is greater than Lo.
            i := 0;
            while (ImageColor.IntHistogram[i] = 0)
            do inc(i);

            if (Lo < i)
            then Lo := i;
            if (Hi <= Lo)
            then Hi := Lo + 1;
       end;
  end;

  if (FError = EC_OK)
  then begin
       ImageColor.TraceAuto := False;
       ImageColor.TraceHigh := Hi;
       ImageColor.TraceLow  := Lo;
       ImageColor.SourceImage[0] := SrcImage;
       ImageColor.ResultImage    := TmcmImage(FResImage);
       DontCare := 0;
       ImageColor.Threshold(TH_TRACE, DontCare, 0, 0);
       ImageColor.Free;
  end;
end; // TmcmImageFilter.TracedThreshold.


//------------------------------------------------------------------------------
// Canny edge filter
//------------------------------------------------------------------------------

procedure TmcmImageFilter.SuppressNoMax(XImage, YImage, MagImage : TKernelImage);
// X / YImage are input data, and Mag are output data.
var x, y     : integer;
    pX, pY   : array[-1..1] of PVectorB;
    pM       : PVectorB;
    xc, yc   : integer;
    xs, ys   : single;
    n        : single;
    n1, n2   : single;
    n3, n4   : single;
    MagScale : single;
    Value    : integer;
begin
  MagScale := 255.0 / sqrt(32768.0);

  for y := 0 to (FResImage.Height - 1)
  do begin
     pX[-1] := XImage.ScanLine[y-1];
     pX[0]  := XImage.ScanLine[y];
     pX[1]  := XImage.ScanLine[y+1];
     pY[-1] := YImage.ScanLine[y-1];
     pY[0]  := YImage.ScanLine[y];
     pY[1]  := YImage.ScanLine[y+1];

     pM := MagImage.ScanLine[y];
     for x := 1 to (FResImage.Width - 2)
     do begin
        xc := pX[0]^[x] - ZeroValue;
        yc := pY[0]^[x] - ZeroValue;

        // Treat pX^[x] and pY^[x] as components of a vector.
        if ((abs(xc) < 6) and (abs(yc) < 6))
        then begin
             pM^[x] := 0;
        end
        else begin
             // Follow the gradient direction, as indicated by the vector
             // (pX^[x], pY^[x]) and retain pixels that are local maximum.
             if (abs(yc) > abs(xc))
             then begin
                  // Y Direction has largest gradient (up/down).
                  xs := abs(xc / yc);

                  n2 := Norm(pX[-1]^[x] - ZeroValue, pY[-1]^[x] - ZeroValue);
                  n4 := Norm(pX[1]^[x] - ZeroValue, pY[1]^[x] - ZeroValue);
                  if (xc * yc > 0.0)
                  then begin
                       n1 := Norm(pX[-1]^[x-1] - ZeroValue, pY[-1]^[x-1] - ZeroValue);
                       n3 := Norm(pX[1]^[x+1] - ZeroValue, pY[1]^[x+1] - ZeroValue);
                  end
                  else begin
                       n1 := Norm(pX[-1]^[x+1] - ZeroValue, pY[-1]^[x+1] - ZeroValue);
                       n3 := Norm(pX[1]^[x-1] - ZeroValue, pY[1]^[x-1] - ZeroValue);
                  end;
             end
             else begin
                  // X Direction has largest gradient (left/right).
                  if (xc <> 0)
                  then xs := abs(yc / xc)
                  else xs := 0.0;

                  n2 := Norm(pX[0]^[x+1] - ZeroValue, pY[0]^[x+1] - ZeroValue);
                  n4 := Norm(pX[0]^[x-1] - ZeroValue, pY[0]^[x-1] - ZeroValue);
                  if (xc * yc > 0.0)
                  then begin
                       n1 := Norm(pX[1]^[x+1] - ZeroValue, pY[1]^[x+1] - ZeroValue);
                       n3 := Norm(pX[-1]^[x-1] - ZeroValue, pY[-1]^[x-1] - ZeroValue);
                  end
                  else begin
                       n1 := Norm(pX[1]^[x-1] - ZeroValue, pY[1]^[x-1] - ZeroValue);
                       n3 := Norm(pX[-1]^[x+1] - ZeroValue, pY[-1]^[x+1] - ZeroValue);
                  end;
             end;

             n := sqrt(xc * xc + yc * yc); // Magnitude of centre pixel.
             ys := 1.0 - xs;
             if (n + 0.5 >= xs * n1 + ys * n2) and
                (n + 0.5 >= xs * n3 + ys * n4)
             then begin
                  // Magnitude
                  Value := Round(MagScale * n);
                  if (Value > 255)
                  then pM^[x] := 255
                  else pM^[x] := Value;
             end;
             //else pM^[x] := 0; All pixels are already set to zero.
        end;
     end;
  end;
end; // TmcmImageFilter.SuppressNoMax.


procedure TmcmImageFilter.CannyFilter(Sigma : double; MagImage : TKernelImage);
var i, j, k        : integer;
    GaussWidth     : integer;
    GaussKernel    : array[0..DFLT_MAXSIZE] of double;
    Gauss1stKernel : array[0..DFLT_MAXSIZE] of double;
    SaveSource     : TKernelImage;
    SaveResult     : TKernelImage;
    YImage         : TKernelImage;
begin
  // Determine Gaussian kernel width/height.
  GaussWidth := 0;
  for i := 0 to 24
  do begin
     if (MeanGauss(i, Sigma) < 0.0025)
     then break;
     GaussWidth := i;
  end;

  for i := 0 to GaussWidth
  do begin
     j := GaussWidth - i;
     k := GaussWidth + i;
     //
     GaussKernel[j] := MeanGauss(i, Sigma);
     GaussKernel[k] := GaussKernel[j];
     //
     Gauss1stKernel[j] := DerivativeGauss(i, Sigma);
     Gauss1stKernel[k] := -Gauss1stKernel[j];
  end;

  // Calc true filter width/height.
  GaussWidth := 2 * GaussWidth + 1;

  //----------------------------------------------------------------------------
  // X - direction
  SetFilterHeight(1);
  SetFilterWidth(GaussWidth);

  // Set-up Gaussian filter.
  Dbl2IntKernel(0, GaussKernel, GaussWidth);
  FBias := 0;
  FilterKernel(FKernel[0]); // Smooth image data

  SaveSource := FSrcImage[0]; // Set result image as source image.
  FSrcImage[0] := FResImage;

  // Set-up derivative filter.
  Dbl2IntKernel(0, Gauss1stKernel, GaussWidth);
  FBias := ZeroValue;
  FilterKernel(FKernel[0]); // Find edges in smoothed image data.

  FSrcImage[0] := SaveSource; // Restore source image.

  //----------------------------------------------------------------------------
  // Create temporary result image.
  SaveResult := FResImage;
  YImage := TKernelImage(TmcmImage.Create);
  YImage.CopyFormat(SaveResult);
  FResImage := YImage;

  //----------------------------------------------------------------------------
  // Y - direction
  SetFilterHeight(GaussWidth);
  SetFilterWidth(1);

  // Set-up Gaussian filter.
  Dbl2IntKernel(0, GaussKernel, GaussWidth);
  FBias := 0;
  FilterKernel(FKernel[0]); // Smooth image data

  SaveSource := FSrcImage[0]; // Set result image as source image.
  FSrcImage[0] := FResImage;

  // Set-up derivative filter.
  Dbl2IntKernel(0, Gauss1stKernel, GaussWidth);
  FBias := ZeroValue;
  FilterKernel(FKernel[0]); // Find edges in smoothed image data.

  FSrcImage[0] := SaveSource; // Restore source image.
  FResImage := SaveResult;

  if (FError = EC_OK)
  then SuppressNoMax({X component}FResImage, YImage, MagImage);

  {$IFDEF MCMTEST}
    // Save X component
    FResImage.FileSave(TestImageDir + 'Canny_X.bmp');
    // Save Y component
    YImage.FileSave(TestImageDir + 'Canny_Y.bmp');
    // Save Magnitude component
    MagImage.FileSave(TestImageDir + 'Canny_Mag.bmp');
  {$ENDIF}

//  FResImage.Assign(MagImage);
  YImage.Free; // Free Y component.
  TracedThreshold(MagImage, FTraceAuto, FTracePercent, FTraceHi, FTraceLo);
end; // TmcmImageFilter.CannyFilter


function TmcmImageFilter.Canny : TmcmImage;
var MagImage : TKernelImage;
begin
  FError := EC_OK;
  // FGSD := 2.0;
  if (FGSD <= 5.0)
  then begin
       if CheckSource(0, [IF_GREY8])
       then begin
            CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

            if Assigned(FResImage)
            then begin
                 if (FSrcImage[0].Empty or FResImage.Empty)
                 then begin
                      FError := EC_NOMEMORY;
                      Result := Nil;
                      Exit;
                 end;
            end;
            if (FSrcImage[0].ImageFormat <> FResImage.ImageFormat)
            then begin
                 FError := EC_BADCOLORFORMAT;
                 Result := Nil;
                 Exit;
            end;

            MagImage := TKernelImage(TmcmImage.Create);
            MagImage.CopyFormat(FResImage);
            MagImage.FillAll(0);

            if Assigned(MagImage)
            then CannyFilter(FGSD, MagImage)
            else FError := EC_NOMEMORY;

            if Assigned(MagImage)
            then MagImage.Free;
       end;
  end
  else FError := EC_MARR_DEVIATION;

  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageFilter.Canny.


//------------------------------------------------------------------------------
// Shen-Castan edge filter
//------------------------------------------------------------------------------

function TmcmImageFilter.Embed(const Src : TmcmImage; var Res : TKernelImage; const FilterWidth : integer) : boolean;
var i, j   : integer;
    dx, dy : integer;
    wdx, y : integer;
    pRes   : PVectorB;
begin                  
  if (Res = Nil)
  then Res := TKernelImage.Create;
  if (Res <> Nil)
  then begin
       dx := Src.Width;
       dy := Src.Height;
       wdx := FilterWidth+dx;
       if (Res.ImageFormat <> IF_GREY8)
       then Res.ImageFormat := IF_GREY8;
       if (Res.Height <> dy + 2 * FilterWidth)
       then Res.Height := dy + 2 * FilterWidth;
       if (Res.Width <> dx + 2 * FilterWidth)
       then Res.Width := dx + 2 * FilterWidth;
       Res.CreateGreyPalette;

       try
         y := -FilterWidth;
         for i := 0 to (Res.Height - 1)
         do begin
            pRes := Res.ScanLine[i];
            CopyMemory(@pRes[FilterWidth], Src.ScanLine[y], Src.Width * SizeOf(byte));
            for j := 0 to (FilterWidth - 1)
            do begin
               pRes[j] := pRes[FilterWidth];
               pRes[wdx+j] := pRes[wdx-1];
            end;
            inc(y);
         end;
       except
         FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := (Res <> Nil);
end; // TmcmImageFilter.Embed


function TmcmImageFilter.Debed(const Src : TmcmImage; var Res : TKernelImage; const FilterWidth : integer) : boolean;
var i, y   : integer;
    dx, dy : integer;
    pSrc   : PVectorB;
begin                  
  if (Res = Nil)
  then Res := TKernelImage.Create;
  if (Res <> Nil)
  then begin
       dx := Src.Width - 2 * FilterWidth;
       dy := Src.Height - 2 * FilterWidth;

       if (Res.ImageFormat <> IF_GREY8)
       then Res.ImageFormat := IF_GREY8;
       if (Res.Height <> dy)
       then Res.Height := dy;
       if (Res.Width <> dx)
       then Res.Width := dx;
       Res.CreateGreyPalette;

       try
         y := FilterWidth;
         for i := 0 to (Res.Height - 1)
         do begin
            pSrc := Src.ScanLine[y];
            CopyMemory(Res.ScanLine[i], @pSrc[FilterWidth], dx * SizeOf(byte));
            inc(y);
         end;
       except
         FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := (Res <> Nil);
end; // TmcmImageFilter.Debed


function TmcmImageFilter.IsefHorizontal(const Src : TmcmImage; var Res : TKernelImage; Causal, AntiCausal : TKernelImage) : boolean;
var b1, b2     : integer;
    b          : integer;
    x, y       : integer;
    pASrc      : PVectorB;
    pCSrc      : PVectorB;
    pC         : PVectorB;
    pA         : PVectorB;
    pRes       : PVectorB;
    pALines    : PMatrixB;
    pCLines    : PMatrixB;
    pSrcLines  : PMatrixB;
    {$IFDEF MCMTEST}
    DebedImage : TKernelImage;
    {$ENDIF}
begin
  b  := Round(65536 * FISEFFactor);
  b1 := Round(65536 * (1.0 - FISEFFactor) / (1.0 + FISEFFactor));
  b2 := Round(FISEFFactor * b1);

  GetMem(pSrcLines, Src.Height * SizeOf(PVectorB));
  GetMem(pALines, Src.Height * SizeOf(PVectorB));
  GetMem(pCLines, Src.Height * SizeOf(PVectorB));
  if (pSrcLines <> Nil) and (pALines <> Nil) and (pCLines <> Nil)
  then begin
       for y := 0 to (Src.Height - 1)
       do begin
          pSrcLines[y] := Src.ScanLine[y];
          pALines[y]   := AntiCausal.ScanLine[y];
          pCLines[y]   := Causal.ScanLine[y];
       end;

       try
         // Compute boundary for Causal and Anti-Causal
         x := Src.Width - 1;
         for y := 0 to (Src.Height - 1)
         do begin
            pCSrc := pSrcLines[y]; // Src.ScanLine[y];
            pC    := pCLines[y]; // Causal.ScanLine[y];
            pA    := pALines[y]; // AntiCausal.ScanLine[y];

            pC[0] := (b1 * pCSrc[0]) shr 16;
            pA[x] := (b2 * pCSrc[x]) shr 16;
         end;

         // Compute Causal component
         for x := 1 to (Src.Width - 1)
         do begin
            for y := 0 to (Src.Height - 1)
            do begin
               pCSrc := pSrcLines[y]; // Src.ScanLine[y];
               pC    := pCLines[y]; // Causal.ScanLine[y];
               pC[x] := (b1 * pCSrc[x] + b * pC[x-1]) shr 16;
            end;
         end;

         // Compute Anti-Causal component
         for x := (Src.Width - 2) downto 0
         do begin
            for y := 0 to (Src.Height - 1)
            do begin
               pASrc := pSrcLines[y]; // Src.ScanLine[y];
               pA    := pALines[y]; // AntiCausal.ScanLine[y];
               pA[x] := (b2 * pASrc[x] + b * pA[x+1]) shr 16;
            end;
         end;

         // Compute boundary for Result image
         x := Src.Width - 1;
         for y := 0 to (Src.Height - 1)
         do begin
            pRes := Res.ScanLine[y];
            pC   := pCLines[y]; // Causal.ScanLine[y];
            pRes[x] := pC[x];
         end;

         // Compute Result image, adding the Causal and Anti-Causal components.
         for y := 0 to (Src.Height - 1)
         do begin
            pRes  := Res.ScanLine[y];
            pC    := pCLines[y]; // Causal.ScanLine[y];
            pA    := pALines[y]; // AntiCausal.ScanLine[y];
            for x := 0 to (Src.Width - 2)
            do pRes[x] := (pC[x] + pA[x+1]);
         end;
       except
         FError := EC_UNKNOWN;
       end;
       {$IFDEF MCMTEST}
         DebedImage := Nil;
         Debed(Res, DebedImage, DFLT_MAXSCWINSIZE);
         DebedImage.FileSave(TestImageDir + 'SC_ISEF_Horiz.bmp');
         DebedImage.Free;
         DebedImage := Nil;
       {$ENDIF}
  end
  else FError := EC_NOMEMORY;

  if (pCLines <> Nil)
  then FreeMem(pCLines);
  if (pALines <> Nil)
  then FreeMem(pALines);
  if (pSrcLines <> Nil)
  then FreeMem(pSrcLines);

  Result := (FError = EC_OK);
end; // TmcmImageFilter.IsefHorizontal.


function TmcmImageFilter.IsefVertical(const Src : TmcmImage; var Res : TKernelImage; Causal, AntiCausal : TKernelImage) : boolean;
var b1, b2 : integer;
    b      : integer;
    x, y   : integer;
    pASrc  : PVectorB;
    pCSrc  : PVectorB;
    pC     : PVectorB;
    pA     : PVectorB;
    pRes   : PVectorB;
    {$IFDEF MCMTEST}
    DebedImage : TKernelImage;
    {$ENDIF}
begin
  b  := Round(65536 * FISEFFactor);
  b1 := Round(65536 * (1.0 - FISEFFactor) / (1.0 + FISEFFactor));
  b2 := Round(FISEFFactor * b1);
  try
    // Compute boundary for Causal and Anti-Causal
    pCSrc := Src.ScanLine[0];
    pASrc := Src.ScanLine[Src.Height-1];
    pC    := Causal.ScanLine[0];
    pA    := AntiCausal.ScanLine[Src.Height-1];
    for x := 0 to (Src.Width - 1)
    do begin
       pC[x] := (b1 * pCSrc[x]) shr 16;
       pA[x] := (b2 * pASrc[x]) shr 16;
    end;

    // Compute Causal component
    pA := Causal.ScanLine[0];
    for y := 1 to (Src.Height - 1)
    do begin
       pCSrc := Src.ScanLine[y];
       pC    := Causal.ScanLine[y];
       // pA    := Causal.ScanLine[y-1];
       for x := 0 to (Src.Width - 1)
       do pC[x] := (b1 * pCSrc[x] + b * pA[x]) shr 16;
       pA := pC;
    end;

    // Compute Anti-Causal component
    pC    := AntiCausal.ScanLine[Src.Height-1];
    for y := (Src.Height - 2) downto 0
    do begin
       pASrc := Src.ScanLine[y];
       // pC    := AntiCausal.ScanLine[y+1];
       pA    := AntiCausal.ScanLine[y];
       for x := 0 to (Src.Width - 1)
       do pA[x] := (b2 * pASrc[x] + b * pC[x]) shr 16;
       pC := pA;
    end;

    // Compute boundary for Result image
    pRes := Res.ScanLine[Res.Height-1];
    pC   := Causal.ScanLine[Causal.Height-1];
    CopyMemory(pRes, pC, Res.Width);

    // Compute Result image, adding the Causal and Anti-Causal components.
    for y := 0 to (Src.Height - 2)
    do begin
       pRes  := Res.ScanLine[y];
       pC    := Causal.ScanLine[y];
       pA    := AntiCausal.ScanLine[y+1];
       for x := 0 to (Src.Width - 1)
       do pRes[x] := (pC[x] + pA[x]);
    end;
  except
    FError := EC_UNKNOWN;
  end;
  {$IFDEF MCMTEST}
    DebedImage := Nil;
    Debed(Res, DebedImage, DFLT_MAXSCWINSIZE);
    DebedImage.FileSave(TestImageDir + 'SC_ISEF_Vert.bmp');
    DebedImage.Free;
    DebedImage := Nil;
  {$ENDIF}

  Result := (FError = EC_OK);
end; // TmcmImageFilter.IsefVertical.


function TmcmImageFilter.IsefSmooth(const Src : TmcmImage; var Res : TKernelImage) : boolean;
// Smooth filter: Infinite Symmetric Exponential Filter (ISEF).
var Causal     : TKernelImage;
    AntiCausal : TKernelImage;
begin
  Causal := TKernelImage.Create;
  Causal.CopyFormat(Src);
  AntiCausal := TKernelImage.Create;
  AntiCausal.CopyFormat(Src);
  try
    // Apply filter in the vertical direction, to the rows.
    if IsefVertical(Src, Res, Causal, AntiCausal)
    // Now, apply filter in the horizontal direction, to the columns.
    then IsefHorizontal(Res, Res, Causal, AntiCausal);
  finally
    Causal.Free;
    AntiCausal.Free;
  end;
  Result := (FError = EC_OK);
end; // TmcmImageFilter.IsefSmooth.


function TmcmImageFilter.ISEF : TmcmImage;
var EmbedImage : TKernelImage;
    ISEFImage  : TKernelImage;
begin
  FError := EC_OK;
  if (true)
  then begin
       if CheckSource(0, [IF_GREY8])
       then begin
            CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

            if Assigned(FResImage)
            then begin
                 if (FSrcImage[0].Empty or FResImage.Empty)
                 then begin
                      FError := EC_NOMEMORY;
                      Result := Nil;
                      Exit;
                 end;
            end;
            if (FSrcImage[0].ImageFormat <> FResImage.ImageFormat)
            then begin
                 FError := EC_BADCOLORFORMAT;
                 Result := Nil;
                 Exit;
            end;

            // Process image
            if (FError = EC_OK)
            then begin
                 // Embed sourceimage - i.e. increase source image by filter width and height.
                 // This is done to overcome the darkened edges from the ISEF smooting filter.
                 EmbedImage := Nil;
                 Embed(FSrcImage[0], EmbedImage, DFLT_MAXSCWINSIZE);

                 // Smooth filter: Infinite Symmetric Exponential Filter (ISEF).
                 ISEFImage := TKernelImage.Create;
                 ISEFImage.CopyFormat(EmbedImage);
                 IsefSmooth(EmbedImage, ISEFImage);

                 Debed(ISEFImage, FResImage, DFLT_MAXSCWINSIZE);
            end;
       end;
  end
  else FError := EC_BADPARAMETER;

  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageFilter.ISEF.

(*
// Performance, AdaptiveGradient is inlined in LocateZeroCrossings.
function TmcmImageFilter.AdaptiveGradient(const BLIImage : TKernelImage; const ISEFImage : TKernelImage; const x, y : integer) : byte;
var i, j   : integer;
    ws     : integer;
    pBli   : PVectorB;
    pIsef  : PVectorB;
    SumOn  : integer;
    SumOff : integer;
    NumOn  : integer;
    NumOff : integer;
begin
  SumOn  := 0;
  SumOff := 0;
  NumOn  := 0;
  NumOff := 0;

  // Limit Window size to 1 <= ws <= DFLT_MAXSCWINSIZE.
  ws := FKernelWidth div 2;
  if (ws < 1)
  then ws := 1;
  if (ws > DFLT_MAXSCWINSIZE)
  then ws := DFLT_MAXSCWINSIZE;

  for j := (y - ws) to (y + ws)
  do begin
     pIsef := ISEFImage.ScanLine[j];
     pBli := BLIImage.ScanLine[j];
     for i := (x - ws) to (x + ws)
     do begin
        if (pBli[i] <> 0)
        then begin
             SumOn := SumOn + pIsef[i];
             inc(NumOn);
        end
        else begin
             SumOff := SumOff + pIsef[i];
             inc(NumOff);
        end;
     end;
  end;

  if (NumOff > 0)
  then SumOff := SumOff div NumOff;
  if (NumOn > 0)
  then SumOn := SumOn div NumOn;

  Result := abs(SumOff - SumOn);
end; // TmcmImageFilter.AdaptiveGradient.
*)
(*
// Performance, IsEdgeCandidate is inlined in LocateZeroCrossings.
function TmcmImageFilter.IsEdgeCandidate(const BLIImage : TKernelImage; const ISEFImage : TKernelImage;  const x, y : integer) : boolean;
var pBli  : array[-1..1] of PVectorB;
    pIsef : array[-1..1] of PVectorB;
begin
  Result := false;

  pBli[-1] := BLIImage.ScanLine[y-1];
  pBli[0]  := BLIImage.ScanLine[y];
  pBli[1]  := BLIImage.ScanLine[y+1];

  pIsef[-1] := ISEFImage.ScanLine[y-1];
  pIsef[0]  := ISEFImage.ScanLine[y];
  pIsef[1]  := ISEFImage.ScanLine[y+1];

  if (pBli[0][x] <> 0)
  then begin
       if (pBli[1][x] = 0)
       then begin
            if (pIsef[1][x] - pIsef[-1][x] > 0)
            then Result := True;
       end
       else if (pBli[0][x+1] = 0)
            then begin
                 if (pIsef[0][x+1] - pIsef[0][x-1] > 0)
                 then Result := True;
            end
            else if (pBli[-1][x] = 0)
                 then begin
                      if (pIsef[1][x] - pIsef[-1][x] < 0)
                      then Result := True;
                 end
                 else if (pBli[0][x-1] = 0)
                      then begin
                           if (pIsef[0][x+1] - pIsef[0][x-1] < 0)
                           then Result := True;
                      end;
  end;
end; // TmcmImageFilter.IsEdgeCandidate.
*)

procedure TmcmImageFilter.LocateZeroCrossings(OrgImage : TKernelImage; const BLIImage : TKernelImage; const ISEFImage : TKernelImage; const FilterWidth : integer);
var x, y        : integer;
    pOrg        : PVectorB;
    pBLILines   : PMatrixB;
    pISEFLines  : PMatrixB;
    IsCandidate : boolean;

    i, j        : integer;
    ws          : integer;
    SumOn       : integer;
    SumOff      : integer;
    NumOn       : integer;
    NumOff      : integer;
    Value       : integer;
begin
  GetMem(pBLILines, OrgImage.Height * SizeOf(PVectorB));
  GetMem(pISEFLines, OrgImage.Height * SizeOf(PVectorB));
  if (pBLILines <> Nil) and (pISEFLines <> Nil)
  then begin
       // Limit Window size to 1 <= ws <= DFLT_MAXSCWINSIZE.
       ws := FKernelWidth div 2;
       if (ws < 1)
       then ws := 1;
       if (ws > DFLT_MAXSCWINSIZE)
       then ws := DFLT_MAXSCWINSIZE;

       // Copy ScanLines into vectors - this way we only need to calculate the
       // line address once.
       for y := 0 to (OrgImage.Height - 1)
       do begin
          pBLILines[y]  := BLIImage.ScanLine[y];
          pISEFLines[y] := ISEFImage.ScanLine[y];
       end;

       for y := FilterWidth to (OrgImage.Height - FilterWidth - 1)
       do begin
          pOrg := OrgImage.ScanLine[y];
          for x := FilterWidth to (OrgImage.Width - FilterWidth - 1)
          do begin
             IsCandidate := False;
             if (pBLILines[y][x] <> 0)
             then begin
                  if (pBLILines[y+1][x] = 0)
                  then begin
                       if (pISEFLines[y+1][x] - pISEFLines[y-1][x] > 0)
                       then IsCandidate := True;
                  end
                  else if (pBLILines[y][x+1] = 0)
                       then begin
                            if (pISEFLines[y][x+1] - pISEFLines[y][x-1] > 0)
                            then IsCandidate := True;
                       end
                       else if (pBLILines[y-1][x] = 0)
                            then begin
                                 if (integer(pISEFLines[y+1][x]) - integer(pISEFLines[y-1][x]) < 0)
                                 then IsCandidate := True;
                            end
                            else if (pBLILines[y][x-1] = 0)
                                 then begin
                                      if (integer(pISEFLines[y][x+1]) - integer(pISEFLines[y][x-1]) < 0)
                                      then IsCandidate := True;
                                 end;
             end;

             if IsCandidate
             // if IsEdgeCandidate(BLIImage, ISEFImage, x, y)
             then begin
                  SumOn  := 0;
                  SumOff := 0;
                  NumOn  := 0;
                  NumOff := 0;
                  for j := (y - ws) to (y + ws)
                  do begin
                     // pIsef := ISEFImage.ScanLine[j];
                     // pBli := BLIImage.ScanLine[j];
                     for i := (x - ws) to (x + ws)
                     do begin
                        if (pBLILines[j][i] <> 0)
                        then begin
                             SumOn := SumOn + pISEFLines[j][i];
                             inc(NumOn);
                        end
                        else begin
                             SumOff := SumOff + pISEFLines[j][i];
                             inc(NumOff);
                        end;
                     end;
                  end;

                  if (NumOff > 0)
                  then SumOff := SumOff div NumOff;
                  if (NumOn > 0)
                  then SumOn := SumOn div NumOn;

                  Value := 4 * abs(SumOff - SumOn);

                  if (Value > 255)
                  then pOrg[x] := 255  // Will saturate... 4 * x is slightly too much.
                  else pOrg[x] := Value;
                  // pOrg[x] := AdaptiveGradient(BLIImage, ISEFImage, x, y);
             end
             else pOrg[x] := 0;

          end;
       end;
  end
  else FError := EC_NOMEMORY;
  
  if (pBLILines <> Nil)
  then FreeMem(pBLILines);
  if (pISEFLines <> Nil)
  then FreeMem(pISEFLines);
end; // TmcmImageFilter.LocateZeroCrossings.


procedure TmcmImageFilter.ShenCastanFilter;
var EmbedImage : TKernelImage;
    DebedImage : TKernelImage;
    ISEFImage  : TKernelImage;
    BLIImage   : TKernelImage;
    FltWidth   : integer;
    Math       : TmcmImageMath;
begin
  FError := EC_OK;

  // Embed sourceimage - i.e. increase source image by filter width and height.
  // This is done to overcome the darkened edges from the ISEF smooting filter.
  FltWidth := DFLT_MAXSCWINSIZE;
  EmbedImage := Nil;
  Embed(FSrcImage[0], EmbedImage, FltWidth);
  {$IFDEF MCMTEST}
    EmbedImage.FileSave(TestImageDir + 'SC_Embed.bmp');
  {$ENDIF}

  // Smooth filter: Infinite Symmetric Exponential Filter (ISEF).
  ISEFImage := TKernelImage.Create;
  ISEFImage.CopyFormat(EmbedImage);
  IsefSmooth(EmbedImage, ISEFImage);

  // Calculate Band Limited Image (Greater-Than).
  BLIImage := TKernelImage.Create;
  BLIImage.CopyFormat(EmbedImage);
  Math    := TmcmImageMath.Create(Nil);
  Math.SourceImage[0] := ISEFImage;
  Math.SourceImage[1] := EmbedImage;
  Math.ResultImage    := BLIImage;
  Math.Execute(IM_GT);
  Math.Free;
  {$IFDEF MCMTEST}
    DebedImage := Nil;
    Debed(BLIImage, DebedImage, FltWidth);
    DebedImage.FileSave(TestImageDir + 'SC_GreaterThan.bmp');
    DebedImage.Free;
    DebedImage := Nil;
  {$ENDIF}

  // Edge detection, Locate zero crossing
  LocateZeroCrossings(EmbedImage, BLIImage, ISEFImage, FltWidth);

  BLIImage.Free;
  ISEFImage.Free;
  DebedImage := Nil;
  Debed(EmbedImage, DebedImage, FltWidth);
  EmbedImage.Free;
  {$IFDEF MCMTEST} // Zero-crossing
    DebedImage.FileSave(TestImageDir + 'SC_ZeroCross.bmp');
  {$ENDIF}

  // Threshold edges
  // Debed result image, i.e. crop image back to original size.
  TracedThreshold(DebedImage, FTraceAuto, FTracePercent, FTraceHi, FTraceLo);
  DebedImage.Free;
end; // TmcmImageFilter.ShenCastanFilter


function TmcmImageFilter.ShenCastan : TmcmImage;
begin
  FError := EC_OK;
  if (true)
  then begin
       if CheckSource(0, [IF_GREY8])
       then begin
            CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

            if Assigned(FResImage)
            then begin
                 if (FSrcImage[0].Empty or FResImage.Empty)
                 then begin
                      FError := EC_NOMEMORY;
                      Result := Nil;
                      Exit;
                 end;
            end;
            if (FSrcImage[0].ImageFormat <> FResImage.ImageFormat)
            then begin
                 FError := EC_BADCOLORFORMAT;
                 Result := Nil;
                 Exit;
            end;

            // Process image
            if (FError = EC_OK)
            then ShenCastanFilter;
       end;
  end
  else FError := EC_BADPARAMETER;

  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageFilter.ShenCastan.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
