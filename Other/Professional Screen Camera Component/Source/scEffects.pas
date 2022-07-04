{ *********************************************************************** }
{                                                                         }
{ scEffects                                                                 }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (mail@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit scEffects;

{$B-}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Math, Classes, Graphics, Types, scShape, scGrayBitmap, scMemUtils;

type
  TChannel = (chBlue, chGreen, chRed, chAlpha);

  TChannels = set of TChannel;

  TPixel = array[TChannel] of Byte;
  PPixel = ^TPixel;
  TPixelRef = array[TChannel] of PByte;

  TSelection = array[TChannel] of Boolean;

  TLines = array of Pointer;

  TRangeType = (rtRect, rtEllipse, rtFrame, rtStar5, rtStar6, rtTriangle0,
    rtTriangle1, rtTriangle2, rtTriangle3, rtRhomb, rtCross);

  TPairData = record
    Lines: TLines;
    Data: Pointer;
  end;
  PPairData = ^TPairData;

  TRangeData = record
    Size: TSize;
    Data: Pointer;
  end;
  PRangeData = ^TRangeData;

  TScanEvent = procedure(PixelRef: TPixelRef; var HorzIndex,
    VertIndex: Integer; Data: Pointer; var Continue: Boolean) of object;

  TScanPairEvent = procedure(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
    Lines: TLines; Data: Pointer; var Continue: Boolean) of object;

  TScanRangeEvent = procedure(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
    RangeIndex: Integer; Rect: TRect; Data: Pointer; var Continue: Boolean) of object;

  TScanType = (stImport, stExport);
  TDistributionType = (dtEven, dtCustom);
  TFactor = 1..High(Integer);

  EStreamError = class(Exception);
  EBitmapError = class(Exception);

  TImportEvent = procedure(var Data: Byte; P: Pointer; Index, BitIndex: Integer) of object;
  TExportEvent = procedure(Data: Byte; P: Pointer; Index, BitIndex: Integer) of object;

  TCustomEffects = class(TComponent)
  private
    FPixelSize: Integer;
    FMasked: Boolean;
    FTransparentRange: Boolean;
    FMaskBitmap: TBitmap;
    FRangeBitmap: TBitmap;
    FOnExport: TExportEvent;
    FOnImport: TImportEvent;
    FPositivePixel: TPixel;
    FNegativePixel: TPixel;
    FRangeType: TRangeType;
    FOnScan: TScanEvent;
    FOnScanPair: TScanPairEvent;
    FOnScanRange: TScanRangeEvent;
    FShapePoints: TShapePoints;
  protected
    function GetPixelSize: TPixelFormat;
    procedure SetPixelSize(Value: TPixelFormat);

    procedure DoImport(var Data: Byte; P: Pointer; BitIndex: Integer); dynamic;
    procedure DoExport(Data: Byte; P: Pointer; BitIndex: Integer); dynamic;

    procedure DoScan(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean; out Modified: Boolean); dynamic;
    procedure ScanPairProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;
    procedure ScanRangeProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    property RangeBitmap: TBitmap read FRangeBitmap write FRangeBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ScanBitmap(Stream: TMemoryStream; Bitmap: TBitmap;
      ScanType: TScanType; DistributionType: TDistributionType;
      Factor: TFactor = 1; Start: Integer = 0); overload; virtual;

    function ScanBitmap(Bitmap: TBitmap; Data: Pointer = nil;
      Mask: TBitmap = nil): Boolean; overload; virtual;
    function ScanPair(SourceBitmap, TargetBitmap: TBitmap; Data: Pointer = nil;
      Mask: TBitmap = nil): Boolean; virtual;
    function ScanRange(Bitmap: TBitmap; ASize: TSize; AData: Pointer = nil;
      Range: TBitmap = nil; Mask: TBitmap = nil): Boolean; virtual;

    function Fill(Bitmap: TBitmap; Channel: TChannel;
      Value: Byte): Boolean; overload; virtual; abstract;
    function Fill(Bitmap: TBitmap; ASelection: TSelection;
      APixel: TPixel): Boolean; overload; virtual; abstract;

    function AdjustSize(SourceSize: TSize; var TargetSize: TSize): Boolean; virtual;
    function ComputeBitmap(SourceBitmap, TargetBitmap: TBitmap;
      TargetSize: TSize; var X, Y: Integer): Boolean; virtual;
    function ComputeSize(var SourceSize: TSize; TargetSize: TSize;
      var X, Y: Integer): Boolean; virtual;
    function CopyBitmap(SourceBitmap, TargetBitmap: TBitmap;
      TargetSize: TSize): Boolean; virtual;
    function GetBitmapCapacity(Bitmap: TBitmap): Integer;
    function ScaleBitmap(var Bitmap: TBitmap; Size: TSize): Boolean; virtual;
    procedure ComputeMask(Mask: TBitmap; SourceSize, TargetSize: TSize;
      X, Y: Integer); overload; virtual;
    procedure ComputeMask(TargetBitmap: TBitmap; TargetSize: TSize;
      Scale: Boolean = True); overload; virtual;
    procedure CorrectMask(Size: TSize; Scale: Boolean = True); virtual;

    property PositivePixel: TPixel read FPositivePixel write FPositivePixel;
    property NegativePixel: TPixel read FNegativePixel write FNegativePixel;
  published
    property PixelSize: TPixelFormat read GetPixelSize write SetPixelSize default pf32bit;
    property Masked: Boolean read FMasked write FMasked default False;
    property MaskBitmap: TBitmap read FMaskBitmap write FMaskBitmap;
    property ShapePoints: TShapePoints read FShapePoints write FShapePoints;
    property RangeType: TRangeType read FRangeType write FRangeType default rtRect;
    property TransparentRange: Boolean read FTransparentRange write FTransparentRange default True;

    property OnImport: TImportEvent read FOnImport write FOnImport;
    property OnExport: TExportEvent read FOnExport write FOnExport;
    property OnScan: TScanEvent read FOnScan write FOnScan;
    property OnScanPair: TScanPairEvent read FOnScanPair write FOnScanPair;
    property OnScanRange: TScanRangeEvent read FOnScanRange write FOnScanRange;
  end;

  TPosition = record
    X, Y: Extended;
  end;
  TQuarterType = (qt1, qt2, qt3, qt4);
  TQuarterRange = record
    Min, Max: Extended;
  end;

  TIncrement = -High(Byte)..High(Byte);
  TIncrements = array[TChannel] of TIncrement;

  TChangeType = (ctEqual, ctNotEqual);
  TChangeTypes = array[TChannel] of TChangeType;

  TSum = array[TChannel] of Integer;
  TRadius = 1..5;
  TSharpenType = (stBrightness, stContrast);

  TRowData = array[TChannel] of TIntegerDynArray;
  TPixelateType = (ptGet, ptSet);

  TGrayBitmaps = array[TChannel] of TGrayBitmap;

  TRotateData = record
    Size: TSize;
  end;
  PRotateData = ^TRotateData;

  TMirrorData = TRotateData;
  PMirrorData = ^TMirrorData;

  TRotateAndMirrorData = TRotateData;
  PRotateAndMirrorData = ^TRotateAndMirrorData;

  TFillData = record
    Selection: TSelection;
    Pixel: TPixel;
  end;
  PFillData = ^TFillData;

  TInvertData = record
    Selection: TSelection;
  end;
  PInvertData = ^TInvertData;

  TRotateCustomData = record
    QuarterType: TQuarterType;
    Radians: Extended;
    Size: TSize;
    SourceCenter, TargetCenter: TPoint;
  end;
  PRotateCustomData = ^TRotateCustomData;

  TIncreaseData = record
    Selection, IgnoreBorders: TSelection;
    Increments: TIncrements;
  end;
  PIncreaseData = ^TIncreaseData;

  TContrastData = record
    Selection: TSelection;
    Increments: TIncrements;
  end;
  PContrastData = ^TContrastData;

  TSaturationData = record
    Selection: TSelection;
    Factor: Byte;
  end;
  PSaturationData = ^TSaturationData;

  TSolarizeData = record
    Selection: TSelection;
    Factor: Byte;
  end;
  PSolarizeData = ^TSolarizeData;

  TPosterizeData = record
    Selection: TSelection;
    Factor: Byte;
  end;
  PPosterizeData = ^TPosterizeData;

  TBlurData = record
    Selection: TSelection;
    Radius: TRadius;
    Size: TSize;
  end;
  PBlurData = ^TBlurData;

  TSmartBlurData = record
    Selection: TSelection;
    Radius: TRadius;
    Difference: Byte;
    Size: TSize;
  end;
  PSmartBlurData = ^TSmartBlurData;

  TNoiseData = record
    Selection: TSelection;
    Factor: Byte;
  end;
  PNoiseData = ^TNoiseData;

  TChangeData = record
    Channel: TChannel;
    Min, Max: Byte;
    Pixel: TPixel;
    ChangeType: TChangeType;
  end;
  PChangeData = ^TChangeData;

  TChangeRangeData = record
    Min, Max, Pixel: TPixel;
    ChangeTypes: TChangeTypes;
    ChangeType: TChangeType;
    Alpha: Boolean;
  end;
  PChangeRangeData = ^TChangeRangeData;

  TSharpenData = record
    Selection: TSelection;
    Radius: TRadius;
    Difference: Byte;
    Percent: Extended;
    SharpenType: TSharpenType;
    Size: TSize;
  end;
  PSharpenData = ^TSharpenData;

  TPixelateData = record
    Selection: TSelection;
    PixelateType: TPixelateType;
    RowData: TRowData;
    Count: TIntegerDynArray;
  end;
  PPixelateData = ^TPixelateData;

  TRoughBlurData = record
    Selection: TSelection;
    Size: TSize;
    Radius: TRadius;
    Fast: Boolean;
    PixelateType: TPixelateType;
    RowData: TRowData;
    RangeCount: Integer;
    Count: TIntegerDynArray;
  end;
  PRoughBlurData = ^TRoughBlurData;

  TInsertData = record
    X, Y: Integer;
    Transparent: Boolean;
  end;
  PInsertData = ^TInsertData;

  TSeparateData = record
    GrayBitmaps: TGrayBitmaps;
  end;
  PSeparateData = ^TSeparateData;

  TAssembleData = record
    GrayBitmaps: TGrayBitmaps;
  end;
  PAssembleData = ^TAssembleData;

  TEffects = class(TCustomEffects)
  protected
    { Cryptography }

    procedure ImportProc(var Data: Byte; P: Pointer; Index, BitIndex: Integer); dynamic;
    procedure ExportProc(Data: Byte; P: Pointer; Index, BitIndex: Integer); dynamic;

    { Rotation }

    procedure RotateCustomProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;
    procedure RotateLeftProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;
    procedure RotateProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;
    procedure RotateRightProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;
    procedure MirrorProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;
    procedure RotateAndMirrorProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    { Fill }

    procedure FillProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Increase }

    procedure IncreaseProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Contrast }

    procedure ContrastProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Saturation }

    procedure SaturationProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Solarize }

    procedure SolarizeProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Posterize }

    procedure PosterizeProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Blur }

    procedure BlurProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    { SmartBlur }

    procedure SmartBlurProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    { Noise }

    procedure NoiseProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Invert }

    procedure InvertProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Change }

    procedure ChangeProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { ChangeRange }

    procedure ChangeRangeProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Sharpen }

    procedure SharpenProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    { Pixelate }

    procedure PixelateProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      RangeIndex: Integer; Rect: TRect; Data: Pointer; var Continue: Boolean); dynamic;

    { RoughBlur }

    procedure RoughBlurProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      RangeIndex: Integer; Rect: TRect; Data: Pointer; var Continue: Boolean); dynamic;

    { Insert }

    procedure InsertProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Lines: TLines; Data: Pointer; var Continue: Boolean); dynamic;

    { Separate }

    procedure SeparateProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Assemble }

    procedure AssembleProc(PixelRef: TPixelRef; var HorzIndex, VertIndex: Integer;
      Data: Pointer; var Continue: Boolean); dynamic;

    { Miscellaneous }

    function Simplify(var Radians: Extended): TQuarterType; dynamic;
    procedure CorrectRadians(var Radians: Extended); dynamic;
    function ComputeRotatedSize(SourceSize: TSize; Radians: Extended;
      out TargetSize: TSize): Boolean; dynamic;
    function ComputePoint(TargetPoint, SourceCenter, TargetCenter: TPoint;
      QuarterType: TQuarterType; Radians: Extended): TPoint; dynamic;
    procedure ComputeSum(HorzIndex, VertIndex: Integer; Size: TSize;
      Lines: TLines; Selection: TSelection; Radius: TRadius; out Rect: TRect;
      out Sum: TSum); dynamic;

    function CheckSelection(Selection: TSelection;
      SkipChannels: TChannels = []): Boolean; dynamic;
    function CheckValue(Value, Min, Max: Byte; ChangeType: TChangeType): Boolean; dynamic;
    function CheckPixel(PixelRef: TPixelRef; Min, Max: TPixel; ChangeTypes: TChangeTypes;
      ChangeType: TChangeType; Alpha: Boolean): Boolean; dynamic;

    function CreateRowData(var RowData: TRowData; Length: Integer;
      Selection: TSelection): Boolean; dynamic;
    procedure DeleteRowData(var RowData: TRowData); dynamic;

    procedure IncreaseBrightness(var Value: Byte; Increment: TIncrement); dynamic;
    procedure IncreaseContrast(var Value: Byte; Increment: TIncrement); dynamic;
  public
    { Cryptography }

    procedure Import(Stream: TMemoryStream; Bitmap: TBitmap;
      DistributionType: TDistributionType; Factor: TFactor = 1;
      Start: Integer = 0); virtual;
    procedure Export(Stream: TMemoryStream; Bitmap: TBitmap;
      DistributionType: TDistributionType; Size: Integer = -1;
      Factor: TFactor = 1; Start: Integer = 0); virtual;

    { Rotation }

    function RotateCustom(SourceBitmap, TargetBitmap: TBitmap;
      ARadians: Extended; Background: PPixel = nil): Boolean; virtual;
    function RotateLeft(SourceBitmap, TargetBitmap: TBitmap;
      Background: PPixel = nil): Boolean; virtual;
    function Rotate(SourceBitmap, TargetBitmap: TBitmap;
      Background: PPixel = nil): Boolean; virtual;
    function RotateRight(SourceBitmap, TargetBitmap: TBitmap;
      Background: PPixel = nil): Boolean; virtual;
    function Mirror(SourceBitmap, TargetBitmap: TBitmap;
      Background: PPixel = nil): Boolean; virtual;
    function RotateAndMirror(SourceBitmap, TargetBitmap: TBitmap;
      Background: PPixel = nil): Boolean; virtual;

    { Fill }

    function Fill(Bitmap: TBitmap; Channel: TChannel;
      Value: Byte): Boolean; overload; override;
    function Fill(Bitmap: TBitmap; ASelection: TSelection;
      APixel: TPixel): Boolean; overload; override;

    { Invert }

    function Invert(Bitmap: TBitmap; Channel: TChannel): Boolean; overload; virtual;
    function Invert(Bitmap: TBitmap; ASelection: TSelection): Boolean; overload; virtual;

    { Increase }

    function Increase(Bitmap: TBitmap; Channel: TChannel; Increment: TIncrement = 1;
      IgnoreBorder: Boolean = False): Boolean; overload; virtual;
    function Increase(Bitmap: TBitmap; ASelection, AIgnoreBorders: TSelection;
      AIncrements: TIncrements): Boolean; overload; virtual;

    { Contrast }

    function Contrast(Bitmap: TBitmap; Channel: TChannel;
      Increment: TIncrement): Boolean; overload; virtual;
    function Contrast(Bitmap: TBitmap; ASelection: TSelection;
      AIncrements: TIncrements): Boolean; overload; virtual;

    { Saturation }

    function Saturation(Bitmap: TBitmap; Channel: TChannel;
      Factor: Byte): Boolean; overload; virtual;
    function Saturation(Bitmap: TBitmap; ASelection: TSelection;
      AFactor: Byte): Boolean; overload; virtual;

    { Solarize }

    function Solarize(Bitmap: TBitmap; Channel: TChannel;
      Factor: Byte): Boolean; overload; virtual;
    function Solarize(Bitmap: TBitmap; ASelection: TSelection;
      AFactor: Byte): Boolean; overload; virtual;

    { Posterize }

    function Posterize(Bitmap: TBitmap; Channel: TChannel;
      Factor: Byte): Boolean; overload; virtual;
    function Posterize(Bitmap: TBitmap; ASelection: TSelection;
      AFactor: Byte): Boolean; overload; virtual;

    { Blur }

    function Blur(SourceBitmap, TargetBitmap: TBitmap; Radius: TRadius;
      Channel: TChannel): Boolean; overload; virtual;
    function Blur(SourceBitmap, TargetBitmap: TBitmap; ASelection: TSelection;
      ARadius: TRadius): Boolean; overload; virtual;

    { Smart blur }

    function SmartBlur(SourceBitmap, TargetBitmap: TBitmap;
      Radius, BlurRadius: TRadius; Difference: Byte;
      Channel: TChannel): Boolean; overload; virtual;
    function SmartBlur(SourceBitmap, TargetBitmap: TBitmap;
      ASelection: TSelection; ARadius, BlurRadius: TRadius;
      ADifference: Byte): Boolean; overload; virtual;

    { Noise }

    function Noise(Bitmap: TBitmap; Factor: Byte;
      Channel: TChannel): Boolean; overload; virtual;
    function Noise(Bitmap: TBitmap; ASelection: TSelection;
      AFactor: Byte): Boolean; overload; virtual;

    { Change }

    function Change(Bitmap: TBitmap; AChannel: TChannel; AMin, AMax: Byte;
      APixel: TPixel; AChangeType: TChangeType): Boolean; virtual;

    { ChangeRange }

    function ChangeRange(Bitmap: TBitmap; AMin, AMax, APixel: TPixel;
      AChangeTypes: TChangeTypes; AChangeType: TChangeType;
      AAlpha: Boolean = False): Boolean; virtual;

    { Sharpen }

    function Sharpen(SourceBitmap, TargetBitmap: TBitmap;
      Radius: TRadius; Difference: Byte; Percent: Extended;
      SharpenType: TSharpenType; Channel: TChannel): Boolean; overload; virtual;
    function Sharpen(SourceBitmap, TargetBitmap: TBitmap;
      ASelection: TSelection; ARadius: TRadius; ADifference: Byte;
      APercent: Extended; ASharpenType: TSharpenType): Boolean; overload; virtual;

    { Pixelate }

    function Pixelate(Bitmap: TBitmap; Size: TSize;
      Channel: TChannel): Boolean; overload; virtual;
    function Pixelate(Bitmap: TBitmap; ASelection: TSelection;
      ASize: TSize): Boolean; overload; virtual;

    { RoughBlur }

    function RoughBlur(Bitmap: TBitmap; Size: TSize; Radius: TRadius;
      Fast: Boolean; Channel: TChannel): Boolean; overload; virtual;
    function RoughBlur(Bitmap: TBitmap; ASelection: TSelection;
      ASize: TSize; ARadius: TRadius; AFast: Boolean): Boolean; overload; virtual;

    { Insert }

    function Insert(SourceBitmap: TBitmap; TargetBitmap: TBitmap;
      AX, AY: Integer; ATransparent: Boolean = False): Boolean; virtual;

    { Separate }

    function Separate(SourceBitmap: TBitmap; TargetBitmap: TGrayBitmap;
      Channel: TChannel): Boolean; overload; virtual;
    function Separate(Bitmap: TBitmap;
      AGrayBitmaps: TGrayBitmaps): Boolean; overload; virtual;

    { Assemble }

    function Assemble(SourceBitmap: TGrayBitmap; TargetBitmap: TBitmap;
      Channel: TChannel): Boolean; overload; virtual;
    function Assemble(AGrayBitmaps: TGrayBitmaps;
      Bitmap: TBitmap): Boolean; overload; virtual;
  end;

const
  PositiveSel: TSelection = (True, True, True, True);
  NegativeSel: TSelection = (False, False, False, False);

  MaxByte = High(Byte);
  MinByte = 0;
  Average = High(Byte) div 2;

  PositivePixel: TPixel = (MaxByte, MaxByte, MaxByte, MaxByte);
  NegativePixel: TPixel = (MinByte, MinByte, MinByte, MinByte);

  BitConst1 = $07;
  BitConst2 = $FE;
  BitConst3 = $01;
  BitConsts: array[0..7] of Byte = ($FE, $FD, $FB, $F7, $EF, $DF, $BF, $7F);

  ByteCounts: array[pf8Bit..pf32Bit] of Byte = (1, 2, 2, 3, 4);

  DefaultPF = pf32bit;
  RangeBitmapPF = pf24bit;

  EmptyColor = clWhite;

  Quarter1Range: TQuarterRange = (Min: 0; Max: Pi / 2);
  Quarter2Range: TQuarterRange = (Min: Pi / 2; Max: Pi);
  Quarter3Range: TQuarterRange = (Min: Pi; Max: 3 * Pi / 2);
  Quarter4Range: TQuarterRange = (Min: 3 * Pi / 2; Max: 2 * Pi);

  Pi2 = 2 * Pi;

  Hundred = 100;

resourcestring
  sSizeIsZero = 'The data size is zero';
  sOutOfSpace = 'Out of bitmap space';

function Pixel(R, G, B: Byte; A: Byte = 0): TPixel;
function Selection(R, G, B: Boolean; A: Boolean = False): TSelection;
function Increments(R, G, B: TIncrement; A: TIncrement = 0): TIncrements;
function ChangeTypes(R, G, B: TChangeType; A: TChangeType = ctEqual): TChangeTypes;
function Size(cx, cy: Integer): TSize;

implementation

function Pixel(R, G, B: Byte; A: Byte = 0): TPixel;
begin
  Result[chBlue] := B;
  Result[chGreen] := G;
  Result[chRed] := R;
  Result[chAlpha] := A;
end;

function Selection(R, G, B: Boolean; A: Boolean = False): TSelection;
begin
  Result[chBlue] := B;
  Result[chGreen] := G;
  Result[chRed] := R;
  Result[chAlpha] := A;
end;

function Increments(R, G, B: TIncrement; A: TIncrement = 0): TIncrements;
begin
  Result[chBlue] := B;
  Result[chGreen] := G;
  Result[chRed] := R;
  Result[chAlpha] := A;
end;

function ChangeTypes(R, G, B: TChangeType; A: TChangeType = ctEqual): TChangeTypes;
begin
  Result[chBlue] := B;
  Result[chGreen] := G;
  Result[chRed] := R;
  Result[chAlpha] := A;
end;

function Size(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

{ TCustomEffects }

function TCustomEffects.AdjustSize(SourceSize: TSize;
  var TargetSize: TSize): Boolean;
begin
  Result := (TargetSize.cx > 0) and (TargetSize.cy > 0);
  if not Result then Exit;
  if SourceSize.cx / TargetSize.cx < SourceSize.cy / TargetSize.cy then
    TargetSize.cx := SourceSize.cx * TargetSize.cy div SourceSize.cy
  else TargetSize.cy := SourceSize.cy * TargetSize.cx div SourceSize.cx;
  Result := (TargetSize.cx > 0) and (TargetSize.cy > 0);
end;

function TCustomEffects.ComputeBitmap(SourceBitmap, TargetBitmap: TBitmap;
  TargetSize: TSize; var X, Y: Integer): Boolean;
var
  AX, AY: Integer;
  SourceSize: TSize;
  ARect: TRect;
begin
  AX := X;
  AY := Y;
  SourceSize := Size(SourceBitmap.Width, SourceBitmap.Height);
  Result := ComputeSize(SourceSize, TargetSize, X, Y);
  if not Result then Exit;
  with TargetBitmap do
  begin
    PixelFormat := SourceBitmap.PixelFormat;
    Width := SourceSize.cx;
    Height := SourceSize.cy;
  end;
  with ARect do
  begin
    Left := IfThen(AX < 0, - AX, 0);
    Right := Left + SourceSize.cx;
    Top := IfThen(AY < 0, - AY, 0);
    Bottom := Top + SourceSize.cy;
  end;
  TargetBitmap.Canvas.CopyRect(Rect(0, 0, SourceSize.cx, SourceSize.cy),
    SourceBitmap.Canvas, ARect);
end;

procedure TCustomEffects.ComputeMask(TargetBitmap: TBitmap;
  TargetSize: TSize; Scale: Boolean);
var
  SourceSize: TSize;
  Flags: array[0..1] of Boolean;
  SourceRect, TargetRect: TRect;
  AMasked: Boolean;
begin
  SourceSize := scEffects.Size(FMaskBitmap.Width, FMaskBitmap.Height);
  with TargetBitmap do
  begin
    Width := TargetSize.cx;
    Height := TargetSize.cy;
  end;
  Flags[0] := TargetSize.cx > SourceSize.cx;
  Flags[1] := TargetSize.cy > SourceSize.cy;
  if Flags[0] or Flags[1] then
  begin
    AMasked := FMasked;
    FMasked := False;
    try
      Fill(TargetBitmap, PositiveSel, FNegativePixel);
    finally
      FMasked := AMasked;
    end;
  end;
  if Scale then
  begin
    with SourceRect do
    begin
      Left := 0;
      Right := SourceSize.cx;
      Top := 0;
      Bottom := SourceSize.cy;
    end;
    with TargetRect do
    begin
      Left := 0;
      Right := TargetSize.cx;
      Top := 0;
      Bottom := TargetSize.cy;
    end;
  end
  else begin
    with SourceRect, SourceSize do
    begin
      Left := EnsureRange((cx - TargetSize.cx) div 2, 0, cx);
      Right := EnsureRange(Left + TargetSize.cx, 0, cx);
      Top := EnsureRange((cy - TargetSize.cy) div 2, 0, cy);
      Bottom := EnsureRange(Top + TargetSize.cy, 0, cy);
    end;
    with TargetRect, TargetSize do
    begin
      Left := IfThen(Flags[0], (cx - SourceSize.cx) div 2, 0);
      Right := IfThen(Flags[0], Left + SourceSize.cx, cx);
      Top := IfThen(Flags[1], (cy - SourceSize.cy) div 2, 0);
      Bottom := IfThen(Flags[1], Top + SourceSize.cy, cy);
    end;
  end;
  TargetBitmap.Canvas.CopyRect(TargetRect, FMaskBitmap.Canvas, SourceRect);
end;

procedure TCustomEffects.ComputeMask(Mask: TBitmap; SourceSize,
  TargetSize: TSize; X, Y: Integer);
begin
  ComputeSize(SourceSize, TargetSize, X, Y);
  with Mask do
  begin
    Width := SourceSize.cx;
    Height := SourceSize.cy;
  end;
  Mask.Canvas.CopyRect(Rect(0, 0, SourceSize.cx, SourceSize.cy),
    FMaskBitmap.Canvas, Rect(X, Y, X + SourceSize.cx, Y + SourceSize.cy));
end;

function TCustomEffects.ComputeSize(var SourceSize: TSize;
  TargetSize: TSize; var X, Y: Integer): Boolean;
begin
  if X < 0 then
  begin
    SourceSize.cx := X + SourceSize.cx;
    X := 0;
    if SourceSize.cx < 0 then SourceSize.cx := 0;
  end;
  if Y < 0 then
  begin
    SourceSize.cy := Y + SourceSize.cy;
    Y := 0;
    if SourceSize.cy < 0 then SourceSize.cy := 0;
  end;
  Result := (TargetSize.cx > 0) and (TargetSize.cy > 0);
  if not Result then Exit;
  if X + SourceSize.cx > TargetSize.cx then
  begin
    SourceSize.cx := TargetSize.cx - X;
    if SourceSize.cx < 0 then SourceSize.cx := 0;
  end;
  if Y + SourceSize.cy > TargetSize.cy then
  begin
    SourceSize.cy := TargetSize.cy - Y;
    if SourceSize.cy < 0 then SourceSize.cy := 0;
  end;
  Result := (TargetSize.cx > 0) and (TargetSize.cy > 0);
end;

function TCustomEffects.CopyBitmap(SourceBitmap, TargetBitmap: TBitmap;
  TargetSize: TSize): Boolean;
var
  SourceSize: TSize;
begin
  Result := not SourceBitmap.Empty;
  if not Result then Exit;
  SourceSize := Size(SourceBitmap.Width, SourceBitmap.Height);
  Result := AdjustSize(SourceSize, TargetSize);
  with TargetBitmap do
  begin
    PixelFormat := SourceBitmap.PixelFormat;
    Width := TargetSize.cx;
    Height := TargetSize.cy;
    Canvas.StretchDraw(Rect(0, 0, TargetSize.cx, TargetSize.cy), SourceBitmap);
    {
    Canvas.CopyRect(Rect(0, 0, TargetSize.cx, TargetSize.cy), SourceBitmap.Canvas,
      Rect(0, 0, SourceSize.cx, SourceSize.cy));
    }
  end;
end;

procedure TCustomEffects.CorrectMask(Size: TSize; Scale: Boolean);
var
  Mask: TBitmap;
begin
  Mask := TBitmap.Create;
  try
    ComputeMask(Mask, Size, Scale);
    FMaskBitmap.Free;
    FMaskBitmap := Mask;
  except
    Mask.Free;
    raise;
  end;
end;

function TCustomEffects.GetPixelSize: TPixelFormat;
begin
  case FPixelSize of
    1  : Result := pf8Bit;
    2  : Result := pf16Bit;
    3  : Result := pf24Bit;
    4  : Result := pf32Bit;
    else Result := pf32Bit;
  end;
end;

procedure TCustomEffects.SetPixelSize(Value: TPixelFormat);
begin
  if FPixelSize <> ByteCounts[Value] then begin
    FPixelSize := ByteCounts[Value];
    end;
end;


constructor TCustomEffects.Create(AOwner: TComponent);
begin
  inherited;
  FMaskBitmap := TBitmap.Create;
  FShapePoints := TShapePoints.Create;
  FRangeType := rtRect;
  FTransparentRange := True;
  FRangeBitmap := TBitmap.Create;
  FPositivePixel := scEffects.PositivePixel;
  FNegativePixel := scEffects.NegativePixel;
  FPixelSize := ByteCounts[DefaultPF];
end;

destructor TCustomEffects.Destroy;
begin
  FMaskBitmap.Free;
  FShapePoints.Free;
  FRangeBitmap.Free;
  inherited;
end;

procedure TCustomEffects.DoExport(Data: Byte; P: Pointer;
  BitIndex: Integer);
var
  Index: Integer;
begin
  if not Assigned(FOnExport) then Exit;
  Index := BitIndex div 8;
  BitIndex := BitIndex and BitConst1;
  FOnExport(Data, P, Index, BitIndex);
end;

procedure TCustomEffects.DoImport(var Data: Byte; P: Pointer;
  BitIndex: Integer);
var
  Index: Integer;
begin
  if not Assigned(FOnImport) then Exit;
  Index := BitIndex div 8;
  BitIndex := BitIndex and BitConst1;
  FOnImport(Data, P, Index, BitIndex);
end;

procedure TCustomEffects.DoScan(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean;
  out Modified: Boolean);
var
  AHorzIndex, AVertIndex: Integer;
begin
  if Assigned(FOnScan) then
  begin
    AHorzIndex := HorzIndex;
    AVertIndex := VertIndex;
    FOnScan(PixelRef, HorzIndex, VertIndex, Data, Continue);
    Modified := (AHorzIndex <> HorzIndex) or (AVertIndex <> VertIndex);
  end
  else Continue := False;
end;

function TCustomEffects.GetBitmapCapacity(Bitmap: TBitmap): Integer;
begin
  with Bitmap do Result := Width * Height * ByteCounts[PixelFormat] div 8;
end;

function TCustomEffects.ScaleBitmap(var Bitmap: TBitmap;
  Size: TSize): Boolean;
var
  NewBitmap: TBitmap;
begin
  Result := not Bitmap.Empty and (Size.cx > 0) and (Size.cy > 0);
  if not Result or (Bitmap.Width = Size.cx) and
    (Bitmap.Height = Size.cy) then Exit;
  NewBitmap := TBitmap.Create;
  try
    CopyBitmap(Bitmap, NewBitmap, Size);
    Bitmap.Free;
    Bitmap := NewBitmap;
  except
    NewBitmap.Free;
    raise;
  end;
end;

function TCustomEffects.ScanBitmap(Bitmap: TBitmap; Data: Pointer;
  Mask: TBitmap): Boolean;
var
  I, J, K: Integer;
  L: TChannel;
  AMasked, Continue, Positive, Modified: Boolean;
  P1, P2: Pointer;
  PixelRef: TPixelRef;
begin
  Result := not Bitmap.Empty;
  if not Result then Exit;
  with Bitmap do
  begin
    PixelFormat := GetPixelSize;
    K := Width * FPixelSize;
  end;
  AMasked := FMasked;
  if AMasked then
  begin
    if (Mask = nil) and not FMaskBitmap.Empty then Mask := FMaskBitmap;
    AMasked := Assigned(Mask);
    if AMasked and ((Mask.Width < Bitmap.Width) or (Mask.Height < Bitmap.Height)) then
      if Mask = FMaskBitmap then AMasked := False
      else ComputeMask(Mask, Size(Bitmap.Width, Bitmap.Height));
  end;
  if AMasked then Mask.PixelFormat := GetPixelSize;
  P2 := nil;
  I := 0;
  while I < Bitmap.Height do
  begin
    P1 := Bitmap.ScanLine[I];
    if AMasked then P2 := Mask.ScanLine[I];
    J := 0;
    while J < K do
    begin
      for L := Low(TChannel) to High(TChannel) do
        PixelRef[L] := PByte(Integer(P1) + J + Ord(L));
      Continue := True;
      if AMasked then
      begin
        for L := Low(TChannel) to High(TChannel) do
        begin
          Positive := FPositivePixel[L] = PByte(Integer(P2) + J + Ord(L))^;
          if not Positive then Break;
        end;
        if Positive then DoScan(PixelRef, J, I, Data, Continue, Modified)
        else Modified := False;
      end
      else DoScan(PixelRef, J, I, Data, Continue, Modified);
      if not Continue then Exit
      else if Modified then
        if I < Bitmap.Height then
        begin
          P1 := Bitmap.ScanLine[I];
          if AMasked then P2 := Mask.ScanLine[I];
        end
        else Exit
      else Inc(J, FPixelSize);
    end;
    if not Modified then Inc(I);
  end;
end;

procedure TCustomEffects.ScanBitmap(Stream: TMemoryStream; Bitmap: TBitmap;
  ScanType: TScanType; DistributionType: TDistributionType;
  Factor: TFactor; Start: Integer);
var
  I, J, K, L, Size1, Size2: Integer;
  P1, P2: Pointer;
begin
  if Stream.Size = 0 then raise EStreamError.Create(sSizeIsZero);
  if Bitmap.PixelFormat < pf8bit then Bitmap.PixelFormat := pf8bit;
  Size1 := Stream.Size * 8;
  I := IfThen(DistributionType = dtEven, Size1, Size1 * Factor);
  with Bitmap do
  begin
    Size2 := Width * ByteCounts[PixelFormat];
    J := Size2 * Height - Start;
  end;
  if I > J then raise EBitmapError.Create(sOutOfSpace);
  if DistributionType = dtEven then Factor := J div I;
  L := Start div Size2;
  P1 := Stream.Memory;
  J := 0;
  K := 0;
  for I := L to Bitmap.Height - 1 do
  begin
    P2 := Bitmap.ScanLine[I];
    if J >= Size2 then Dec(J, Size2);
    while J < Size2 do
    begin
      // I * Size2 - количество предшествующих байт
      if I * Size2 + J >= Start then
      begin
        // Size1 - объем информации
        if K >= Size1 then Exit;
        if ScanType = stImport then DoImport(PByte(Integer(P2) + J)^, P1, K)
        else DoExport(PByte(Integer(P2) + J)^, P1, K);
        Inc(K);
      end;
      Inc(J, Factor);
    end;
  end;
end;

function TCustomEffects.ScanPair(SourceBitmap, TargetBitmap: TBitmap;
  Data: Pointer; Mask: TBitmap): Boolean;
var
  I: Integer;
  PairData: TPairData;
  ScanEvent: TScanEvent;
begin
  Result := not SourceBitmap.Empty and not TargetBitmap.Empty;
  if not Result then Exit;
  TargetBitmap.PixelFormat := GetPixelSize;
  SetLength(PairData.Lines, TargetBitmap.Height);
  try
    for I := 0 to TargetBitmap.Height - 1 do
      PairData.Lines[I] := TargetBitmap.ScanLine[I];
    PairData.Data := Data;
    ScanEvent := FOnScan;
    FOnScan := ScanPairProc;
    try
      Result := ScanBitmap(SourceBitmap, @PairData, Mask);
    finally
      FOnScan := ScanEvent;
    end;
  finally
    PairData.Lines := nil;
  end;
end;

procedure TCustomEffects.ScanPairProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
begin
  if Assigned(FOnScanPair) then FOnScanPair(PixelRef, HorzIndex, VertIndex,
    PPairData(Data).Lines, PPairData(Data).Data, Continue);
end;

function TCustomEffects.ScanRange(Bitmap: TBitmap; ASize: TSize;
  AData: Pointer; Range, Mask: TBitmap): Boolean;
var
  I, J, K, L, M, N: Integer;
  R, G, B: Byte;
  RangeData: TRangeData;
  ScanPairEvent: TScanPairEvent;
begin
  Result := not Bitmap.Empty;
  if not Result then Exit;
  if Assigned(Range) and ((Range.Width <> Bitmap.Width) or
    (Range.Height <> Bitmap.Height)) then Range := nil;
  if not Assigned(Range) then
  begin
    Range := FRangeBitmap;
    with Range do
    begin
      PixelFormat := RangeBitmapPF;
      Width := Bitmap.Width;
      Height := Bitmap.Height;
      with Canvas do
      begin
        if FTransparentRange then Brush.Color := EmptyColor
        else Brush.Color := 0;
        FillRect(Rect(0, 0, Width, Height));
      end;
    end;
    I := 0;
    if FTransparentRange then K := 0
    else K := 1;
    while I < Range.Height do
    begin
      J := 0;
      while J < Range.Width do
      begin
        B := GetBValue(K);
        G := GetGValue(K);
        R := GetRValue(K);
        with Range.Canvas do
        begin
          Brush.Color := RGB(R, G, B);
          Pen.Color := RGB(R, G, B);
          case FRangeType of
            rtRect: FillRect(Rect(J, I, J + ASize.cx, I + ASize.cy));
            rtEllipse: Ellipse(Rect(J, I, J + ASize.cx, I + ASize.cy));
            rtFrame: FrameRect(Rect(J, I, J + ASize.cx, I + ASize.cy));
            rtStar5: begin
              L := ASize.cx div 2;
              M := ASize.cy div 4;
              N := ASize.cx div 8;
              Polygon([Point(J + L, I), Point(J + ASize.cx - N, I + ASize.cy),
                Point(J + L, I + ASize.cy - M), Point(J + N, I + ASize.cy)]);
              N := ASize.cy div 3;
              Polygon([Point(J, I + N), Point(J + ASize.cx, I + N),
                Point(J + L, I + ASize.cy - M)]);
            end;
            rtStar6: begin
              L := ASize.cx div 2;
              M := ASize.cy div 4;
              Polygon([Point(J + L, I), Point(J + ASize.cx, I + ASize.cy - M),
                Point(J, I + ASize.cy - M)]);
              Polygon([Point(J, I + M), Point(J + ASize.cx, I + M),
                Point(J + L, I + ASize.cy)]);
            end;
          else
            with FShapePoints do
            begin
              Range := scShape.Range(J, I, ASize.cx, ASize.cy);
              ShapeType := TShapeType(Ord(FRangeType) - Ord(rtStar6) - 1);
              Polygon(Points);
            end;
          end;
        end;
        Inc(K);
        Inc(J, ASize.cx);
      end;
      Inc(I, ASize.cy);
    end;
  end;
  with RangeData do
  begin
    Size := ASize;
    Data := AData;
  end;
  ScanPairEvent := FOnScanPair;
  FOnScanPair := ScanRangeProc;
  try
    Result := ScanPair(Bitmap, Range, @RangeData, Mask);
  finally
    FOnScanPair := ScanPairEvent;
  end;
end;

procedure TCustomEffects.ScanRangeProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I, J, K: Integer;
  Rect: TRect;
begin
  if not Assigned(FOnScanRange) then Exit;
  I := HorzIndex div FPixelSize;
  J := Integer(Lines[VertIndex]) + HorzIndex;
  // K - Индекс текущей области
  K := RGB(PByte(J + 2)^, PByte(J + 1)^, PByte(J)^);
  if K = EmptyColor then Exit;
  with PRangeData(Data)^ do
  begin
    Rect.Left := I - I mod Size.cx;
    Rect.Right := Rect.Left + Size.cx;
    Rect.Top := VertIndex - VertIndex mod Size.cy;
    Rect.Bottom := Rect.Top + Size.cy;
    FOnScanRange(PixelRef, HorzIndex, VertIndex, K, Rect, Data, Continue);
  end;
end;

{ TEffects }

function TEffects.Assemble(AGrayBitmaps: TGrayBitmaps;
  Bitmap: TBitmap): Boolean;
var
  I: TChannel;
  Size: TSize;
  AssembleData: TAssembleData;
  ScanEvent: TScanEvent;
begin
  Size.cx := MaxInt;
  Size.cy := MaxInt;
  for I := Low(TChannel) to High(TChannel) do
    if Assigned(AGrayBitmaps[I]) then
    begin
      if Size.cx > AGrayBitmaps[I].Width then
        Size.cx := AGrayBitmaps[I].Width;
      if Size.cy > AGrayBitmaps[I].Height then
        Size.cy := AGrayBitmaps[I].Height;
      AGrayBitmaps[I].UpdatePalette;
    end;
  with Bitmap do
  begin
    Width := Size.cx;
    Height := Size.cy;
  end;
  AssembleData.GrayBitmaps := AGrayBitmaps;
  ScanEvent := FOnScan;
  FOnScan := AssembleProc;
  try
    Result := ScanBitmap(Bitmap, @AssembleData);
  finally
    FOnScan := ScanEvent;
  end;
end;

function TEffects.Assemble(SourceBitmap: TGrayBitmap;
  TargetBitmap: TBitmap; Channel: TChannel): Boolean;
var
  GrayBitmaps: TGrayBitmaps;
begin
  FillChar(GrayBitmaps, SizeOf(TGrayBitmaps), 0);
  GrayBitmaps[Channel] := SourceBitmap;
  Result := Assemble(GrayBitmaps, TargetBitmap);
end;

procedure TEffects.AssembleProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
  P: PByte;
begin
  J := HorzIndex div FPixelSize;
  for I := Low(TChannel) to High(TChannel) do
    with PSeparateData(Data)^ do
      if Assigned(GrayBitmaps[I]) then
      begin
        P := Pointer(Integer(GrayBitmaps[I].ScanLine[VertIndex]) + J);
        PixelRef[I]^ := P^;
      end;
end;

function TEffects.Blur(SourceBitmap, TargetBitmap: TBitmap;
  Radius: TRadius; Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Blur(SourceBitmap, TargetBitmap, Selection, Radius);
end;

function TEffects.Blur(SourceBitmap, TargetBitmap: TBitmap;
  ASelection: TSelection; ARadius: TRadius): Boolean;
var
  BlurData: TBlurData;
  ScanPairEvent: TScanPairEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  TargetBitmap.Assign(SourceBitmap);
  with BlurData do
  begin
    Selection := ASelection;
    Radius := ARadius;
    Size.cx := SourceBitmap.Width - 1;
    Size.cy := SourceBitmap.Height - 1;
  end;
  ScanPairEvent := FOnScanPair;
  FOnScanPair := BlurProc;
  try
    Result := ScanPair(TargetBitmap, SourceBitmap, @BlurData);
  finally
    FOnScanPair := ScanPairEvent;
  end;
end;

procedure TEffects.BlurProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J, K, L, M: Integer;
  Rect: TRect;
  Sum: TSum;
begin
  J := HorzIndex div FPixelSize;
  with PBlurData(Data)^ do
  begin
    with Rect do
    begin
      Left := IfThen(J > Radius, J - Radius, 0);
      Right := IfThen(J + Radius < Size.cx, J + Radius, Size.cx);
      Top := IfThen(VertIndex > Radius, VertIndex - Radius, 0);
      Bottom := IfThen(VertIndex + Radius < Size.cy, VertIndex + Radius, Size.cy);
    end;
    FillChar(Sum, SizeOf(Sum), 0);
    M := 0;
    for J := Rect.Top to Rect.Bottom do for K := Rect.Left to Rect.Right do
    begin
      L := Integer(Lines[J]) + K * FPixelSize;
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] then Inc(Sum[I], PByte(L + Ord(I))^);
      Inc(M);
    end;
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then PixelRef[I]^ := Sum[I] div M;
  end;
end;

function TEffects.Change(Bitmap: TBitmap; AChannel: TChannel; AMin,
  AMax: Byte; APixel: TPixel; AChangeType: TChangeType): Boolean;
var
  ChangeData: TChangeData;
  ScanEvent: TScanEvent;
begin
  with ChangeData do
  begin
    Channel := AChannel;
    Min := AMin;
    Max := AMax;
    Pixel := APixel;
    ChangeType := AChangeType;
  end;
  ScanEvent := FOnScan;
  FOnScan := ChangeProc;
  try
    Result := ScanBitmap(Bitmap, @ChangeData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.ChangeProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PChangeData(Data)^ do if CheckValue(PixelRef[Channel]^,
    Min, Max, ChangeType) then
      for I := Low(TChannel) to High(TChannel) do PixelRef[I]^ := Pixel[I];
end;

function TEffects.ChangeRange(Bitmap: TBitmap; AMin, AMax, APixel: TPixel;
  AChangeTypes: TChangeTypes; AChangeType: TChangeType;
  AAlpha: Boolean): Boolean;
var
  ChangeRangeData: TChangeRangeData;
  ScanEvent: TScanEvent;
begin
  with ChangeRangeData do
  begin
    Min := AMin;
    Max := AMax;
    Pixel := APixel;
    ChangeTypes := AChangeTypes;
    ChangeType := AChangeType;
    Alpha := AAlpha;
  end;
  ScanEvent := FOnScan;
  FOnScan := ChangeRangeProc;
  try
    Result := ScanBitmap(Bitmap, @ChangeRangeData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.ChangeRangeProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PChangeRangeData(Data)^ do if CheckPixel(PixelRef, Min, Max,
    ChangeTypes, ChangeType, Alpha) then
      for I := Low(TChannel) to High(TChannel) do PixelRef[I]^ := Pixel[I];
end;

function TEffects.CheckPixel(PixelRef: TPixelRef; Min, Max: TPixel;
  ChangeTypes: TChangeTypes; ChangeType: TChangeType;
  Alpha: Boolean): Boolean;
var
  I: TChannel;
  Selection: TSelection;
begin
  for I := Low(TChannel) to High(TChannel) do
    Selection[I] := (PixelRef[I]^ >= Min[I]) and (PixelRef[I]^ <= Max[I]) xor
      (ChangeTypes[I] = ctNotEqual);
  if Alpha then Result := CheckSelection(Selection) xor (ChangeType = ctNotEqual)
  else Result := CheckSelection(Selection, [chAlpha]) xor
    (ChangeType = ctNotEqual);
end;

function TEffects.CheckSelection(Selection: TSelection;
  SkipChannels: TChannels): Boolean;
var
  I: TChannel;
begin
  for I := Low(TChannel) to High(TChannel) do
    if not (I in SkipChannels) and Selection[I] then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TEffects.CheckValue(Value, Min, Max: Byte;
  ChangeType: TChangeType): Boolean;
begin
  Result := (Value >= Min) and (Value <= Max) xor (ChangeType = ctNotEqual);
end;

function TEffects.ComputePoint(TargetPoint, SourceCenter,
  TargetCenter: TPoint; QuarterType: TQuarterType;
  Radians: Extended): TPoint;
var
  SourcePoint: TPosition;
begin
  {
    Обратная формула
    TargetPoint.X := SourcePoint.X * Cos(Radians) - SourcePoint.Y * Sin(Radians);
    TargetPoint.Y := SourcePoint.X * Sin(Radians) + SourcePoint.Y * Cos(Radians);
  }
  with TargetPoint do if QuarterType = qt1 then
  begin
    X := X - TargetCenter.X;
    Y := TargetCenter.Y - Y;
    SourcePoint.X := Y * Csc(Radians) + (X - Y * Cotan(Radians)) * Cos(Radians);
    SourcePoint.Y := -(X - Y * Cotan(Radians)) * Sin(Radians);
  end else if QuarterType = qt2 then
  begin
    X := TargetCenter.X - X;
    Y := TargetCenter.Y - Y;
    SourcePoint.X := Y * Csc(Radians) + (X - Y * Cotan(Radians)) * Cos(Radians);
    SourcePoint.Y := (X - Y * Cotan(Radians)) * Sin(Radians);
  end else if QuarterType = qt3 then
  begin
    X := X - TargetCenter.X;
    Y := TargetCenter.Y - Y;
    SourcePoint.X := - Y * Csc(Radians) - (X - Y * Cotan(Radians)) * Cos(Radians);
    SourcePoint.Y := (X - Y * Cotan(Radians)) * Sin(Radians);
  end else if QuarterType = qt4 then
  begin
    X := TargetCenter.X - X;
    Y := TargetCenter.Y - Y;
    SourcePoint.X := - Y * Csc(Radians) - (X - Y * Cotan(Radians)) * Cos(Radians);
    SourcePoint.Y := -(X - Y * Cotan(Radians)) * Sin(Radians);
  end;
  Result.X := Round(SourcePoint.X + SourceCenter.X);
  Result.Y := Round(SourceCenter.Y - SourcePoint.Y);
end;

function TEffects.ComputeRotatedSize(SourceSize: TSize; Radians: Extended;
  out TargetSize: TSize): Boolean;
var
  RoundMode: TFPURoundingMode;
  Center: TPosition;
begin
  Result := (SourceSize.cx > 0) and (SourceSize.cy > 0);
  if not Result then Exit;
  RoundMode := GetRoundMode;
  SetRoundMode(rmNearest);
  try
    with Center do
    begin
      X := SourceSize.cx / 2;
      Y := SourceSize.cy / 2;
    end;
    TargetSize.cx := Round((Center.X * Cos(Radians) + Center.Y * Sin(Radians)) * 2);
    TargetSize.cy := Round((Center.X * Sin(Radians) + Center.Y * Cos(Radians)) * 2);
  finally
    SetRoundMode(RoundMode);
  end;
end;

procedure TEffects.ComputeSum(HorzIndex, VertIndex: Integer; Size: TSize;
  Lines: TLines; Selection: TSelection; Radius: TRadius; out Rect: TRect;
  out Sum: TSum);
var
  I: TChannel;
  J, K, L, Count: Integer;
begin
  with Rect do
  begin
    Left := IfThen(HorzIndex > Radius, HorzIndex - Radius, 0);
    Right := IfThen(HorzIndex + Radius < Size.cx, HorzIndex + Radius, Size.cx);
    Top := IfThen(VertIndex > Radius, VertIndex - Radius, 0);
    Bottom := IfThen(VertIndex + Radius < Size.cy, VertIndex + Radius, Size.cy);
  end;
  FillChar(Sum, SizeOf(TSum), 0);

  Count := 0;
  for J := Rect.Top to Rect.Bottom do for K := Rect.Left to Rect.Right do
    if (K <> HorzIndex) or (J <> VertIndex) then
    begin
      L := Integer(Lines[J]) + K * FPixelSize;
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] then Inc(Sum[I], PByte(L + Ord(I))^);
      Inc(Count);
    end;

  for I := Low(TChannel) to High(TChannel) do
    if Selection[I] then Sum[I] := Sum[I] div Count;
end;

function TEffects.Contrast(Bitmap: TBitmap; Channel: TChannel;
  Increment: TIncrement): Boolean;
var
  Selection: TSelection;
  Increments: TIncrements;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Increments[Channel] := Increment;
  Result := Contrast(Bitmap, Selection, Increments);
end;

function TEffects.Contrast(Bitmap: TBitmap; ASelection: TSelection;
  AIncrements: TIncrements): Boolean;
var
  ContrastData: TContrastData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with ContrastData do
  begin
    Selection := ASelection;
    Increments := AIncrements;
  end;
  ScanEvent := FOnScan;
  FOnScan := ContrastProc;
  try
    Result := ScanBitmap(Bitmap, @ContrastData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.ContrastProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PContrastData(Data)^ do for I := Low(TChannel) to High(TChannel) do
    if Selection[I] then IncreaseContrast(PixelRef[I]^, Increments[I]);
end;

procedure TEffects.CorrectRadians(var Radians: Extended);
begin
  if Radians < 0 then while Radians < 0 do Radians := Radians + Pi2
  else if Radians >= Pi2 then while Radians >= Pi2 do Radians := Radians - Pi2;
end;

function TEffects.CreateRowData(var RowData: TRowData; Length: Integer;
  Selection: TSelection): Boolean;
var
  I: TChannel;
begin
  Result := Length > 0;
  if not Result then Exit;
  for I := Low(TChannel) to High(TChannel) do
    if Selection[I] then
    begin
      SetLength(RowData[I], Length);
      FillChar(RowData[I][0], Length * SizeOf(Integer), 0);
    end;
end;

procedure TEffects.DeleteRowData(var RowData: TRowData);
var
  I: TChannel;
begin
  for I := Low(TChannel) to High(TChannel) do RowData[I] := nil;
end;

procedure TEffects.Export(Stream: TMemoryStream; Bitmap: TBitmap;
  DistributionType: TDistributionType; Size: Integer; Factor: TFactor;
  Start: Integer);
var
  ExportEvent: TExportEvent;
begin
  ExportEvent := FOnExport;
  FOnExport := ExportProc;
  try
    Stream.SetSize(IfThen(Size < 0, GetBitmapCapacity(Bitmap), Size));
    ScanBitmap(Stream, Bitmap, stExport, DistributionType, Factor, Start);
  finally
    FOnExport := ExportEvent;
  end;
end;

procedure TEffects.ExportProc(Data: Byte; P: Pointer; Index,
  BitIndex: Integer);
var
  I: Byte;
  J: PByte;
begin
  I := Data and BitConst3;
  J := PByte(Integer(P) + Index);
  J^ := (BitConsts[BitIndex] and J^) or (I shl BitIndex);
end;

function TEffects.Fill(Bitmap: TBitmap; Channel: TChannel;
  Value: Byte): Boolean;
var
  Selection: TSelection;
  Pixel: TPixel;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Pixel[Channel] := Value;
  Result := Fill(Bitmap, Selection, Pixel);
end;

function TEffects.Fill(Bitmap: TBitmap; ASelection: TSelection;
  APixel: TPixel): Boolean;
var
  FillData: TFillData;
  ScanEvent: TScanEvent;
begin
  with FillData do
  begin
    Selection := ASelection;
    Pixel := APixel;
  end;
  ScanEvent := FOnScan;
  FOnScan := FillProc;
  try
    Result := ScanBitmap(Bitmap, @FillData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.FillProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  for I := Low(TChannel) to High(TChannel) do
    if PFillData(Data)^.Selection[I] then
      PixelRef[I]^ := PFillData(Data)^.Pixel[I];
end;

procedure TEffects.Import(Stream: TMemoryStream; Bitmap: TBitmap;
  DistributionType: TDistributionType; Factor: TFactor; Start: Integer);
var
  ImportEvent: TImportEvent;
begin
  ImportEvent := FOnImport;
  FOnImport := ImportProc;
  try
    ScanBitmap(Stream, Bitmap, stImport, DistributionType, Factor, Start);
  finally
    FOnImport := ImportEvent;
  end;
end;

procedure TEffects.ImportProc(var Data: Byte; P: Pointer; Index,
  BitIndex: Integer);
var
  I: Byte;
begin
  I := (PByte(Integer(P) + Index)^ shr BitIndex) and BitConst3;
  Data := (Data and BitConst2) or I;
end;

function TEffects.Increase(Bitmap: TBitmap; Channel: TChannel;
  Increment: TIncrement; IgnoreBorder: Boolean): Boolean;
var
  Selection, IgnoreBorders: TSelection;
  Increments: TIncrements;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Increments[Channel] := Increment;
  IgnoreBorders[Channel] := IgnoreBorder;
  Result := Increase(Bitmap, Selection, IgnoreBorders, Increments);
end;

function TEffects.Increase(Bitmap: TBitmap; ASelection,
  AIgnoreBorders: TSelection; AIncrements: TIncrements): Boolean;
var
  IncreaseData: TIncreaseData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with IncreaseData do
  begin
    Selection := ASelection;
    IgnoreBorders := AIgnoreBorders;
    Increments := AIncrements;
  end;
  ScanEvent := FOnScan;
  FOnScan := IncreaseProc;
  try
    Result := ScanBitmap(Bitmap, @IncreaseData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.IncreaseBrightness(var Value: Byte;
  Increment: TIncrement);
begin
  Value := EnsureRange(Value + Increment, MinByte, MaxByte);
end;

procedure TEffects.IncreaseContrast(var Value: Byte;
  Increment: TIncrement);
var
  I: Integer;
begin
  I := (Average - Value) * Increment div MaxByte;
  Value := EnsureRange(Value - I, MinByte, MaxByte);
end;

procedure TEffects.IncreaseProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PIncreaseData(Data)^ do
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then if IgnoreBorders[I] then
        PixelRef[I]^ := PixelRef[I]^ + Increments[I]
      else PixelRef[I]^ := EnsureRange(PixelRef[I]^ +
        Increments[I], MinByte, MaxByte);
end;

function TEffects.Insert(SourceBitmap, TargetBitmap: TBitmap; AX,
  AY: Integer; ATransparent: Boolean): Boolean;
var
  InsertData: TInsertData;
  Bitmap, Mask: TBitmap;
  ScanPairEvent: TScanPairEvent;
begin
  Bitmap := TBitmap.Create;
  try
    Result := ComputeBitmap(SourceBitmap, Bitmap, Size(TargetBitmap.Width,
      TargetBitmap.Height), AX, AY);
    if not Result then Exit;
    if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
    else Mask := nil;
    try
      if Assigned(Mask) then ComputeMask(Mask, Size(Bitmap.Width, Bitmap.Height),
        Size(TargetBitmap.Width, TargetBitmap.Height), AX, AY);
      with InsertData do
      begin
        X := AX * FPixelSize;
        Y := AY;
        Transparent := ATransparent;
      end;
      ScanPairEvent := FOnScanPair;
      FOnScanPair := InsertProc;
      try
        Result := ScanPair(Bitmap, TargetBitmap, @InsertData, Mask);
      finally
        FOnScanPair := ScanPairEvent;
      end;
    finally
      Mask.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TEffects.InsertProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
  P: PByte;
begin
  with PInsertData(Data)^ do
  begin
    J := Integer(Lines[Y + VertIndex]) + X + HorzIndex;
    if Transparent then
      for I := Low(TChannel) to High(TChannel) do
      begin
        {
          A = A * F / MaxByte + B * (F / MaxByte - 1)
          A = (A * F + B * (MaxByte - F)) / MaxByte
        }
        P := PByte(J + Ord(I));
        if I = chAlpha then P^ := PixelRef[I]^
        else P^ := (P^ * PixelRef[chAlpha]^ + PixelRef[I]^ * (MaxByte -
          PixelRef[chAlpha]^)) div MaxByte;
      end
    else
      for I := Low(TChannel) to High(TChannel) do
        PByte(J + Ord(I))^ := PixelRef[I]^;
  end;
end;

function TEffects.Invert(Bitmap: TBitmap; Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Invert(Bitmap, Selection);
end;

function TEffects.Invert(Bitmap: TBitmap; ASelection: TSelection): Boolean;
var
  InvertData: TInvertData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  InvertData.Selection := ASelection;
  ScanEvent := FOnScan;
  FOnScan := InvertProc;
  try
    Result := ScanBitmap(Bitmap, @InvertData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.InvertProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PInvertData(Data)^ do for I := Low(TChannel) to High(TChannel) do
    if Selection[I] then PixelRef[I]^ := MaxByte - PixelRef[I]^;
end;

function TEffects.Mirror(SourceBitmap, TargetBitmap: TBitmap;
  Background: PPixel): Boolean;
var
  MirrorData: TMirrorData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  with TargetBitmap do
  begin
    Width := SourceBitmap.Width;
    Height := SourceBitmap.Height;
  end;
  MirrorData.Size.cx := TargetBitmap.Width * FPixelSize - FPixelSize;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) and Assigned(Background) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        Fill(TargetBitmap, PositiveSel, Background^);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := MirrorProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @MirrorData, Mask);
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

procedure TEffects.MirrorProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
begin
  J := Integer(Lines[VertIndex]) + PMirrorData(Data).Size.cx - HorzIndex;
  for I := Low(TChannel) to High(TChannel) do PByte(J + Ord(I))^ := PixelRef[I]^;
end;

function TEffects.Noise(Bitmap: TBitmap; Factor: Byte;
  Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Noise(Bitmap, Selection, Factor);
end;

function TEffects.Noise(Bitmap: TBitmap; ASelection: TSelection;
  AFactor: Byte): Boolean;
var
  NoiseData: TNoiseData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with NoiseData do
  begin
    Selection := ASelection;
    Factor := AFactor;
  end;
  ScanEvent := FOnScan;
  FOnScan := NoiseProc;
  try
    Result := ScanBitmap(Bitmap, @NoiseData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.NoiseProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PNoiseData(Data)^ do for I := Low(TChannel) to High(TChannel) do
    if Selection[I] then
      PixelRef[I]^ := EnsureRange(PixelRef[I]^ + Random(Factor) -
        Random(Factor), MinByte, MaxByte);
end;

function TEffects.Pixelate(Bitmap: TBitmap; Size: TSize;
  Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Pixelate(Bitmap, Selection, Size);
end;

function TEffects.Pixelate(Bitmap: TBitmap; ASelection: TSelection;
  ASize: TSize): Boolean;
var
  I: Integer;
  PixelateData: TPixelateData;
  ScanRangeEvent: TScanRangeEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  I := (Bitmap.Width div ASize.cx + Integer(Bitmap.Width mod ASize.cx > 0)) *
    (Bitmap.Height div ASize.cy + Integer(Bitmap.Height mod ASize.cy > 0)) +
    Ord(not FTransparentRange);
  Result := I > Ord(not FTransparentRange);
  if not Result then Exit;
  try
    with PixelateData do
    begin
      Selection := ASelection;
      PixelateType := ptGet;
      CreateRowData(RowData, I, Selection);
      SetLength(Count, I);
      FillChar(Count[0], I * SizeOf(Integer), 0);
    end;
    ScanRangeEvent := FOnScanRange;
    FOnScanRange := PixelateProc;
    try
      Result := ScanRange(Bitmap, ASize, @PixelateData);
      if not Result then Exit;
      PixelateData.PixelateType := ptSet;
      Result := ScanRange(Bitmap, ASize, @PixelateData, FRangeBitmap);
    finally
      FOnScanRange := ScanRangeEvent;
    end;
  finally
    with PixelateData do
    begin
      DeleteRowData(RowData);
      Count := nil;
    end;
  end;
end;

procedure TEffects.PixelateProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; RangeIndex: Integer; Rect: TRect; Data: Pointer;
  var Continue: Boolean);
var
  I: TChannel;
begin
  with PPixelateData(Data)^ do
    if PixelateType = ptGet then
    begin
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] and (RangeIndex < Length(RowData[I])) then
          Inc(RowData[I][RangeIndex], PixelRef[I]^);
      Inc(Count[RangeIndex]);
    end else
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] and (RangeIndex < Length(RowData[I])) then
          PixelRef[I]^ := RowData[I][RangeIndex] div Count[RangeIndex];
end;

function TEffects.Posterize(Bitmap: TBitmap; Channel: TChannel;
  Factor: Byte): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Posterize(Bitmap, Selection, Factor);
end;

function TEffects.Posterize(Bitmap: TBitmap; ASelection: TSelection;
  AFactor: Byte): Boolean;
var
  PosterizeData: TPosterizeData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with PosterizeData do
  begin
    Selection := ASelection;
    Factor := IfThen(AFactor = 0, 1, AFactor);
  end;
  ScanEvent := FOnScan;
  FOnScan := PosterizeProc;
  try
    Result := ScanBitmap(Bitmap, @PosterizeData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.PosterizeProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
begin
  with PPosterizeData(Data)^ do for I := Low(TChannel) to High(TChannel) do
    PixelRef[I]^ := EnsureRange(Round(PixelRef[I]^ / Factor) * Factor,
      MinByte, MaxByte);
end;

function TEffects.Rotate(SourceBitmap, TargetBitmap: TBitmap;
  Background: PPixel): Boolean;
var
  RotateData: TRotateData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  with TargetBitmap do
  begin
    Width := SourceBitmap.Width;
    Height := SourceBitmap.Height;
  end;
  RotateData.Size.cy := TargetBitmap.Height - 1;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) and Assigned(Background) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        Fill(TargetBitmap, PositiveSel, Background^);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := RotateProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @RotateData, Mask);
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

function TEffects.RotateAndMirror(SourceBitmap, TargetBitmap: TBitmap;
  Background: PPixel): Boolean;
var
  RotateAndMirrorData: TRotateAndMirrorData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  with TargetBitmap do
  begin
    Width := SourceBitmap.Width;
    Height := SourceBitmap.Height;
  end;
  with RotateAndMirrorData do
  begin
    Size.cx := TargetBitmap.Width - 1;
    Size.cy := TargetBitmap.Height - 1;
  end;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) and Assigned(Background) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        Fill(TargetBitmap, PositiveSel, Background^);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := RotateAndMirrorProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @RotateAndMirrorData, Mask);
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

procedure TEffects.RotateAndMirrorProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
begin
  with PMirrorData(Data)^ do J := Integer(Lines[Size.cy - VertIndex]) +
    Size.cx * FPixelSize - HorzIndex;
  for I := Low(TChannel) to High(TChannel) do PByte(J + Ord(I))^ := PixelRef[I]^;
end;

function TEffects.RotateCustom(SourceBitmap, TargetBitmap: TBitmap;
  ARadians: Extended; Background: PPixel): Boolean;
var
  Angle: Extended;
  AMasked: Boolean;
  RotateCustomData: TRotateCustomData;
  ASize: TSize;
  Mask: TBitmap;
  ScanPairEvent: TScanPairEvent;
  RoundMode: TFPURoundingMode;
begin
  Angle := ARadians;
  CorrectRadians(ARadians);
  if IsZero(ARadians) then
  begin
    with TargetBitmap do
    begin
      Width := SourceBitmap.Width;
      Height := SourceBitmap.Height;
    end;
    AMasked := FMasked;
    FMasked := False;
    try
      Fill(TargetBitmap, PositiveSel, Background^);
    finally
      FMasked := AMasked;
    end;
    Result := Insert(SourceBitmap, TargetBitmap, 0, 0, False);
    Exit;
  end else if SameValue(ARadians, Pi / 2) then
  begin
    Result := RotateLeft(SourceBitmap, TargetBitmap, Background);
    Exit;
  end else if SameValue(ARadians, Pi) then
  begin
    Result := RotateAndMirror(SourceBitmap, TargetBitmap, Background);
    Exit;
  end else if SameValue(ARadians, 3 * Pi / 2) then
  begin
    Result := RotateRight(SourceBitmap, TargetBitmap, Background);
    Exit;
  end;
  with RotateCustomData do
  begin
    QuarterType := Simplify(ARadians);
    Radians := ARadians;
    Size := scEffects.Size(SourceBitmap.Width, SourceBitmap.Height);
  end;
  ComputeRotatedSize(RotateCustomData.Size, ARadians, ASize);
  with TargetBitmap do
  begin
    PixelFormat := GetPixelSize;
    Width := ASize.cx;
    Height := ASize.cy;
  end;
  if Assigned(Background) then
  begin
    AMasked := FMasked;
    FMasked := False;
    try
      Fill(TargetBitmap, PositiveSel, Background^);
    finally
      FMasked := AMasked;
    end;
  end;
  with RotateCustomData do
  begin
    SourceCenter.X := Size.cx div 2;
    SourceCenter.Y := Size.cy div 2;
    TargetCenter.X := ASize.cx div 2;
    TargetCenter.Y := ASize.cy div 2;
  end;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        RotateCustom(FMaskBitmap, Mask, Angle, @FNegativePixel);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := RotateCustomProc;
    try
      RoundMode := GetRoundMode;
      SetRoundMode(rmNearest);
      try
        Result := ScanPair(TargetBitmap, SourceBitmap, @RotateCustomData, Mask);
      finally
        SetRoundMode(RoundMode);
      end;
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

procedure TEffects.RotateCustomProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
  APoint: TPoint;
begin
  with PRotateCustomData(Data)^ do
  begin
    APoint := ComputePoint(Point(HorzIndex div FPixelSize, VertIndex),
      SourceCenter, TargetCenter, QuarterType, Radians);
    if (APoint.X < 0) or (APoint.X >= Size.cx) or (APoint.Y < 0) or
      (APoint.Y >= Size.cy) then Exit;
  end;
  J := Integer(Lines[APoint.Y]) + APoint.X * FPixelSize;
  for I := Low(TChannel) to High(TChannel) do PixelRef[I]^ := PByte(J + Ord(I))^;
end;

function TEffects.RotateLeft(SourceBitmap, TargetBitmap: TBitmap;
  Background: PPixel): Boolean;
var
  RotateData: TRotateData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  with TargetBitmap do
  begin
    Width := SourceBitmap.Height;
    Height := SourceBitmap.Width;
  end;
  RotateData.Size.cy := TargetBitmap.Height - 1;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) and Assigned(Background) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        Fill(TargetBitmap, PositiveSel, Background^);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := RotateLeftProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @RotateData, Mask);
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

procedure TEffects.RotateLeftProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
begin
  J := Integer(Lines[PRotateData(Data).Size.cy -
    HorzIndex div FPixelSize]) + VertIndex * FPixelSize;
  for I := Low(TChannel) to High(TChannel) do PByte(J + Ord(I))^ := PixelRef[I]^;
end;

procedure TEffects.RotateProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
begin
  J := Integer(Lines[PRotateData(Data)^.Size.cy - VertIndex]) + HorzIndex;
  for I := Low(TChannel) to High(TChannel) do PByte(J + Ord(I))^ := PixelRef[I]^;
end;

function TEffects.RotateRight(SourceBitmap, TargetBitmap: TBitmap;
  Background: PPixel): Boolean;
var
  RotateData: TRotateData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  with TargetBitmap do
  begin
    Width := SourceBitmap.Height;
    Height := SourceBitmap.Width;
  end;
  RotateData.Size.cx := TargetBitmap.Width * FPixelSize - FPixelSize;
  if FMasked and not FMaskBitmap.Empty then Mask := TBitmap.Create
  else Mask := nil;
  try
    if Assigned(Mask) and Assigned(Background) then
    begin
      AMasked := FMasked;
      FMasked := False;
      try
        Fill(TargetBitmap, PositiveSel, Background^);
      finally
        FMasked := AMasked;
      end;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := RotateRightProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @RotateData, Mask);
    finally
      FOnScanPair := ScanPairEvent;
    end;
  finally
    Mask.Free;
  end;
end;

procedure TEffects.RotateRightProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
begin
  J := Integer(Lines[HorzIndex div FPixelSize]) +
    PRotateData(Data).Size.cx - VertIndex * FPixelSize;
  for I := Low(TChannel) to High(TChannel) do PByte(J + Ord(I))^ := PixelRef[I]^;
end;

function TEffects.RoughBlur(Bitmap: TBitmap; Size: TSize; Radius: TRadius;
  Fast: Boolean; Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := RoughBlur(Bitmap, Selection, Size, Radius, Fast);
end;

function TEffects.RoughBlur(Bitmap: TBitmap; ASelection: TSelection;
  ASize: TSize; ARadius: TRadius; AFast: Boolean): Boolean;
var
  RoughBlurData: TRoughBlurData;
  ScanRangeEvent: TScanRangeEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with RoughBlurData do
  begin
    Size := scEffects.Size(Bitmap.Width div ASize.cx +
      Integer(Bitmap.Width mod ASize.cx > 0), Bitmap.Height div ASize.cy +
      Integer(Bitmap.Height mod ASize.cy > 0));
    RangeCount := Size.cx * Size.cy + Ord(not FTransparentRange);
  end;
  Result := RoughBlurData.RangeCount > Ord(not FTransparentRange);
  if not Result then Exit;
  try
    with RoughBlurData do
    begin
      Selection := ASelection;
      Radius := ARadius;
      Fast := AFast;
      PixelateType := ptGet;
      CreateRowData(RowData, RangeCount, Selection);
      SetLength(Count, RangeCount);
      FillChar(Count[0], RangeCount * SizeOf(Integer), 0);
    end;
    ScanRangeEvent := FOnScanRange;
    FOnScanRange := RoughBlurProc;
    try
      Result := ScanRange(Bitmap, ASize, @RoughBlurData);
      if not Result then Exit;
      RoughBlurData.PixelateType := ptSet;
      Result := ScanRange(Bitmap, ASize, @RoughBlurData, FRangeBitmap);
    finally
      FOnScanRange := ScanRangeEvent;
    end;
  finally
    with RoughBlurData do
    begin
      DeleteRowData(RowData);
      Count := nil;
    end;
  end;
end;

procedure TEffects.RoughBlurProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; RangeIndex: Integer; Rect: TRect; Data: Pointer;
  var Continue: Boolean);
var
  I: TChannel;
  J, K, L, Step, Top, Bottom, Left, Right: Integer;
begin
  with PRoughBlurData(Data)^ do
    if PixelateType = ptGet then
    begin
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] and (RangeIndex < Length(RowData[I])) then
          Inc(RowData[I][RangeIndex], PixelRef[I]^);
      Inc(Count[RangeIndex]);
    end else
      for I := Low(TChannel) to High(TChannel) do
        if Selection[I] and (RangeIndex < Length(RowData[I])) then
        begin
          J := Radius * Size.cx;
          Top := RangeIndex - J;
          Bottom := RangeIndex + J;
          if Fast then Step := J // low-quality
          else Step := Size.cx; // high-quality
          K := 0; // total
          L := 0; // count
          while Top <= Bottom do
          begin
            Left := Top - Radius;
            Right := Top + Radius;
            for J := Left to Right do if J >= 0 then
            begin
              if J >= RangeCount then Break;
              if Count[J] = 0 then System.Continue;
              Inc(K, RowData[I][J] div Count[J]);
              Inc(L);
            end;
            Inc(Top, Step);
          end;
          PixelRef[I]^ := K div L;
        end;
end;

function TEffects.Saturation(Bitmap: TBitmap; Channel: TChannel;
  Factor: Byte): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Saturation(Bitmap, Selection, Factor);
end;

function TEffects.Saturation(Bitmap: TBitmap; ASelection: TSelection;
  AFactor: Byte): Boolean;
var
  SaturationData: TSaturationData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with SaturationData do
  begin
    Selection := ASelection;
    Factor := AFactor;
  end;
  ScanEvent := FOnScan;
  FOnScan := SaturationProc;
  try
    Result := ScanBitmap(Bitmap, @SaturationData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.SaturationProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J, K: Integer;
begin
  J := 0;
  K := 0;
  with PSaturationData(Data)^ do
  begin
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then
      begin
        Inc(J, PixelRef[I]^);
        Inc(K)
      end;
    J := J div K;
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then
        PixelRef[I]^ := J + ((PixelRef[I]^ - J) * Factor) div MaxByte;
  end;
end;

function TEffects.Separate(SourceBitmap: TBitmap;
  TargetBitmap: TGrayBitmap; Channel: TChannel): Boolean;
var
  GrayBitmaps: TGrayBitmaps;
begin
  FillChar(GrayBitmaps, SizeOf(TGrayBitmaps), 0);
  GrayBitmaps[Channel] := TargetBitmap;
  Result := Separate(SourceBitmap, GrayBitmaps);
end;

function TEffects.Separate(Bitmap: TBitmap;
  AGrayBitmaps: TGrayBitmaps): Boolean;
var
  I: TChannel;
  SeparateData: TSeparateData;
  ScanEvent: TScanEvent;
begin
  for I := Low(TChannel) to High(TChannel) do
    if Assigned(AGrayBitmaps[I]) then
    begin
      AGrayBitmaps[I].Width := Bitmap.Width;
      AGrayBitmaps[I].Height := Bitmap.Height;
      AGrayBitmaps[I].UpdatePalette;
    end;
  SeparateData.GrayBitmaps := AGrayBitmaps;
  ScanEvent := FOnScan;
  FOnScan := SeparateProc;
  try
    Result := ScanBitmap(Bitmap, @SeparateData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.SeparateProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
  P: PByte;
begin
  J := HorzIndex div FPixelSize;
  for I := Low(TChannel) to High(TChannel) do
    with PSeparateData(Data)^ do
      if Assigned(GrayBitmaps[I]) then
      begin
        P := Pointer(Integer(GrayBitmaps[I].ScanLine[VertIndex]) + J);
        P^ := PixelRef[I]^;
      end;
end;

function TEffects.Sharpen(SourceBitmap, TargetBitmap: TBitmap;
  Radius: TRadius; Difference: Byte; Percent: Extended;
  SharpenType: TSharpenType; Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Sharpen(SourceBitmap, TargetBitmap, Selection, Radius, Difference,
    Percent, SharpenType);
end;

function TEffects.Sharpen(SourceBitmap, TargetBitmap: TBitmap;
  ASelection: TSelection; ARadius: TRadius; ADifference: Byte;
  APercent: Extended; ASharpenType: TSharpenType): Boolean;
var
  SharpenData: TSharpenData;
  ScanPairEvent: TScanPairEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  TargetBitmap.Assign(SourceBitmap);
  with SharpenData do
  begin
    Selection := ASelection;
    Radius := ARadius;
    Difference := ADifference;
    Percent := APercent;
    SharpenType := ASharpenType;
    Size.cx := SourceBitmap.Width - 1;
    Size.cy := SourceBitmap.Height - 1;
  end;
  ScanPairEvent := FOnScanPair;
  FOnScanPair := SharpenProc;
  try
    Result := ScanPair(TargetBitmap, SourceBitmap, @SharpenData);
  finally
    FOnScanPair := ScanPairEvent;
  end;
end;

procedure TEffects.SharpenProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  Min, Max: Byte;
  Rect: TRect;
  Sum: TSum;
begin
  with PSharpenData(Data)^ do
  begin
    ComputeSum(HorzIndex div FPixelSize, VertIndex, Size, Lines, Selection,
      Radius, Rect, Sum);
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then
      begin
        Min := EnsureRange(Sum[I] - Difference, 0, MaxByte);
        Max := EnsureRange(Sum[I] + Difference, 0, MaxByte);
        if (PixelRef[I]^ < Min) or (PixelRef[I]^ > Max) then
          case SharpenType of
            stBrightness:
              if PixelRef[I]^ > Sum[I] then
                IncreaseBrightness(PixelRef[I]^, Round(PixelRef[I]^ * Percent / Hundred))
              else IncreaseBrightness(PixelRef[I]^, - Round(PixelRef[I]^ * Percent / Hundred));
            stContrast: IncreaseContrast(PixelRef[I]^, Round(MaxByte * Percent / Hundred));
          end;
      end;
  end;
end;

function TEffects.Simplify(var Radians: Extended): TQuarterType;
begin
  if (Radians >= Quarter2Range.Min) and (Radians < Quarter2Range.Max) then
  begin
    Radians := Pi - Radians;
    Result := qt2;
  end
  else if (Radians >= Quarter3Range.Min) and (Radians < Quarter3Range.Max) then
  begin
    Radians := Radians - Pi;
    Result := qt3;
  end
  else if (Radians >= Quarter4Range.Min) and (Radians < Quarter4Range.Max) then
  begin
    Radians := Pi2 - Radians;
    Result := qt4
  end
  else Result := qt1
end;

function TEffects.SmartBlur(SourceBitmap, TargetBitmap: TBitmap; Radius,
  BlurRadius: TRadius; Difference: Byte; Channel: TChannel): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := SmartBlur(SourceBitmap, TargetBitmap, Selection, Radius,
    BlurRadius, Difference);
end;

function TEffects.SmartBlur(SourceBitmap, TargetBitmap: TBitmap;
  ASelection: TSelection; ARadius, BlurRadius: TRadius;
  ADifference: Byte): Boolean;
var
  SmartBlurData: TSmartBlurData;
  Mask: TBitmap;
  AMasked: Boolean;
  ScanPairEvent: TScanPairEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  TargetBitmap.Assign(SourceBitmap);
  with SmartBlurData do
  begin
    Selection := ASelection;
    Radius := ARadius;
    Difference := ADifference;
    Size.cx := SourceBitmap.Width - 1;
    Size.cy := SourceBitmap.Height - 1;
  end;
  Mask := TBitmap.Create;
  try
    with FMaskBitmap do
    begin
      Width := SourceBitmap.Width;
      Height := SourceBitmap.Height;
    end;
    ScanPairEvent := FOnScanPair;
    FOnScanPair := SmartBlurProc;
    try
      Result := ScanPair(SourceBitmap, TargetBitmap, @SmartBlurData);
      if not Result then Exit;
    finally
      FOnScanPair := ScanPairEvent;
    end;
    Fill(FMaskBitmap, chAlpha, PositivePixel[chAlpha]);
    AMasked := FMasked;
    FMasked := True;
    try
      Result := Blur(SourceBitmap, TargetBitmap, ASelection, BlurRadius);
    finally
      FMasked := AMasked;
    end;
    FMaskBitmap.Free;
    FMaskBitmap := Mask;
  except
    Mask.Free;
    raise;
  end;
end;

procedure TEffects.SmartBlurProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Lines: TLines; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J: Integer;
  Min, Max: Byte;
  Rect: TRect;
  Sum: TSum;
  Positive: Boolean;
begin
  J := HorzIndex div FPixelSize;
  with PSmartBlurData(Data)^ do
  begin
    ComputeSum(J, VertIndex, Size, Lines, Selection, Radius, Rect, Sum);
    Positive := False;
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then
      begin
        Min := EnsureRange(Sum[I] - Difference, 0, MaxByte);
        Max := EnsureRange(Sum[I] + Difference, 0, MaxByte);
        Positive := (PixelRef[I]^ >= Min) and (PixelRef[I]^ <= Max);
        if not Positive then Break;
      end;
    if Positive then
      FMaskBitmap.Canvas.Pixels[J, VertIndex] := RGB(PositivePixel[chRed],
        PositivePixel[chGreen], PositivePixel[chBlue])
    else
      FMaskBitmap.Canvas.Pixels[J, VertIndex] := RGB(NegativePixel[chRed],
        NegativePixel[chGreen], NegativePixel[chBlue]);
  end;
end;

function TEffects.Solarize(Bitmap: TBitmap; Channel: TChannel;
  Factor: Byte): Boolean;
var
  Selection: TSelection;
begin
  Selection := NegativeSel;
  Selection[Channel] := True;
  Result := Solarize(Bitmap, Selection, Factor);
end;

function TEffects.Solarize(Bitmap: TBitmap; ASelection: TSelection;
  AFactor: Byte): Boolean;
var
  SolarizeData: TSolarizeData;
  ScanEvent: TScanEvent;
begin
  Result := CheckSelection(ASelection);
  if not Result then Exit;
  with SolarizeData do
  begin
    Selection := ASelection;
    Factor := AFactor;
  end;
  ScanEvent := FOnScan;
  FOnScan := SolarizeProc;
  try
    Result := ScanBitmap(Bitmap, @SolarizeData);
  finally
    FOnScan := ScanEvent;
  end;
end;

procedure TEffects.SolarizeProc(PixelRef: TPixelRef; var HorzIndex,
  VertIndex: Integer; Data: Pointer; var Continue: Boolean);
var
  I: TChannel;
  J, K: Integer;
begin
  J := 0;
  K := 0;
  with PSolarizeData(Data)^ do
  begin
    for I := Low(TChannel) to High(TChannel) do
      if Selection[I] then
      begin
        Inc(J, PixelRef[I]^);
        Inc(K)
      end;
    J := J div K;
    if J > Factor then for I := Low(TChannel) to High(TChannel) do
      PixelRef[I]^ := MaxByte - PixelRef[I]^
  end;
end;

end.
