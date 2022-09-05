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
// $Log:  22721: mcmGraphics.pas
//
//    Rev 1.8    2014-04-06 13:06:08  mcm
//
//    Rev 1.7    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.6    2013-08-16 22:03:44  mcm
// Removed un-used code.
//
//    Rev 1.6    30-07-2011 20:28:06  mcm
// Removed un-used code.
//
//    Rev 1.5    22-09-2006 21:03:04  mcm    Version: IMG 3.0
// Modified amd fixed problems in CalcConvexData, GetElongation, Paint and
// SetShowConvex.
//
//    Rev 1.4    05-06-2006 22:31:38  mcm
// Renamed Resize to ResizeData on TmcmCustomPolygon.
//
//    Rev 1.3    21-05-2006 21:05:56  mcm
// Added TmcmCustomPolygon and TmcmPolygon.
//
//   Rev 1.2    27-10-2004 22:21:36  mcm    Version: IMG 2.6
// Added property methods to support C++Builder.

//
//   Rev 1.1    30-01-2004 20:03:26  mcm    Version: IMG 2.3
// Modified property visibility on TmcmCustomProfile and TmcmProfile. Cleaned
// out unused code. Improved accuracy of marker position in paint method.
// Added fine-tuning of negative and positive transitions position.

//
//   Rev 1.0    21-01-2004 12:30:22  mcm    Version: IMG 2.3
// Initial version of TmcmGraphicsControl, TmcmCustomProfile and TmcmProfile.

unit mcmGraphics;

{$Include 'mcmDefines.pas'}

interface


{$IFOPT B+}{$DEFINE BOOLEAN_OFF}{$B-}{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Classes, Controls, Graphics, Messages, Windows, Forms,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Generics.Collections, 
     {$ENDIF}
     mcmImageTypeDef, mcmImage;

Type
// Black-White, White-Black, White-Black-White, Black-White-Black
  TmcmProfTransition  = (PT_POSITIVE, PT_NEGATIVE, PT_PEAK, PT_VALLEY, PT_NONE);
  TmcmProfTransitions = set of TmcmProfTransition;
  TmcmProfInterpolate = (PI_NONE, PI_BILINEAR);
  TmcmProfDispMarker  = (PDM_CROSS, PDM_LINE, PDM_BRACKETBEGIN, PDM_BRACKETEND{, PDM_ARROWSTART, PDM_ARROWEND});

  TmcmHitTest  = (THT_NONE, THT_EDGE, THT_INSIDE);   // Used by HitTest.

(*
  TPoint = record
  public
    class operator IntDivide(a : TPoint; r : integer) : TPoint; 
  end;
*)   
  TmcmGraphicControl = class(TGraphicControl)
  private
    // Private declarations
  protected
    // Protected declarations
    FMaxX, FMaxY : integer; // Maximum X and Y coordinate value.
    FLeft        : single;  // Unity left
    FTop         : single;  // Unity top
    FWidth       : single;  // Unity width
    FHeight      : single;  // Unity height
    FScale       : double;  // Should reflect image zoom factor.
    FPen         : TPen;    // Profile & end point's line
    FBrush       : TBrush;  // Filled the end nodes.

    function    CheckX(X : integer) : integer;
    function    CheckY(Y : integer) : integer;
    function    ControlXOffset : integer;
    function    ControlYOffset : integer;
    function    GetHeight : integer;
    function    GetLeft : integer;
    function    GetTop : integer;
    function    GetWidth : integer;
    procedure   SetBrush(Value : TBrush);
    procedure   SetHeight(Value : integer);
    procedure   SetLeft(Value : integer);
    procedure   SetPen(Value : TPen);
    procedure   SetScale(Value : double); virtual;
    procedure   SetTop(Value : integer);
    procedure   SetWidth(Value : integer);
    procedure   StyleChanged(Sender : TObject);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  published
    // Published declarations
    property    Brush : TBrush
      read      FBrush
      write     SetBrush;
    property    Height : integer
      read      GetHeight
      write     SetHeight;
    property    Left : integer
      read      GetLeft
      write     SetLeft;
    property    Pen : TPen
      read      FPen
      write     SetPen;
    property    Scale : double
      read      FScale
      write     SetScale;
    property    Top : integer
      read      GetTop
      write     SetTop;
    property    Width : integer
      read      GetWidth
      write     SetWidth;
  end;

//------------------------------------------------------------------------------
// TmcmCustomProfile
//------------------------------------------------------------------------------

  TmcmCustomProfile = class(TmcmGraphicControl)
  private
    // Private declarations
    Fmx, Fmy       : integer;  // Point where the line is grabbed.
    Fcx, Fcy       : double;   // Centre x,y of profile line.
    Fdx, Fdy       : double;
    Fox, Foy       : integer;
    Fdsx, Fdsy     : double;
    //Fsi, Fei       : integer;
    FRadius        : double;   // Lines radius, centered at Fcx,Fcy.
    FHitIndex      : integer;  // Cursor pressed at
                               //  0 -> Start node
                               //  1 -> End node
                               //  2 -> On line.
    FHitDist       : integer;  // Distance from line that constitute as a hit on
                               // the line.
    FScanLength    : integer;  // Length of profile (end - start coordinate).
    FSWCoorAx      : PVectorS; // Coordinates used when Scan width > 1
    FSWCoorAy      : PVectorS; // -
    FSWCoorBx      : PVectorS; // -
    FSWCoorBy      : PVectorS; // -
    FLBtnDown      : boolean; // Indicates if left mouse button is down.
  protected
    // Protected declarations
    FError         : TmcmErrorCode; // Error occured during last scan.
    FOnChange      : TNotifyEvent;  // On image change event.
    FModified      : boolean;       // Profile has been modified.
    FDoRescan      : boolean;       // Profile requires a re-scan.
    FImage         : TmcmImage;     // Image to scan.
    FAngle         : double;        // Angle of profile line.
    FSlope         : double;
    FLength        : double;        // Length of profile line.
    FDispMarker    : array[0..integer(PT_NONE)-1] of TmcmProfDispMarker;
    FShowEndPoints : boolean;       // Show end nodes when cursor is on/near the line.
    FDoShowEndPts  : boolean;       // -
    FNodeSize      : integer;       // Diameter of end node points.

    FMaxValue      : word;          // Max. intensity value, based on image. Internal!
    FAverage       : integer;       // Average scanned profile data - always odd size.
    FInterpolate   : TmcmProfInterpolate; // Pixel interpolation.
    FScanWidth     : integer;       // Width of profile scan.
    FSubPixel      : boolean;       // Enable sub-pixel scan.
    FStartPos      : TPoint;        // Profile start coordinate.
    FEndPos        : TPoint;        // Profile end coordinate.
    FScanData      : PVectorI;      // scanned intensity profile.

    procedure   AverageScan;
    procedure   Clear; virtual;
    procedure   CalcAngle;
    procedure   CalcEndPoints;
    function    CalcPerpendicular : TmcmErrorCode;
    procedure   Changed(Sender : TObject); virtual;
    function    GetAngle : double;
    function    GetEndX : integer;
    function    GetEndY : integer;
    function    GetImage : TmcmImage;
    function    GetDisplayMarker(Index : integer) : TmcmProfDispMarker;
    function    GetScanData(Index : integer) : integer;
    function    GetStartX : integer;
    function    GetStartY : integer;
    function    ScanBW(iLongWidth, iHeight : integer; pImageData : PVectorB)   : TmcmErrorCode;
    function    ScanGrey(iLongWidth, iHeight : integer; pImageData : PVectorB) : TmcmErrorCode;
    function    ScanPal(iLongWidth, iHeight : integer; pImageData : PVectorB; const Palette : array of TRGBQuad)  : TmcmErrorCode;
    function    ScanRGB(iLongWidth, iHeight : integer; pImageData : PVectorB; NoCh : integer)  : TmcmErrorCode;
    procedure   SetAngle(Value : double);
    procedure   SetAverage(Value : integer);
    procedure   SetEndX(Value : integer);
    procedure   SetEndY(Value : integer);
    procedure   SetImage(Value : TmcmImage);
    procedure   SetInterpolate(Value : TmcmProfInterpolate);
    procedure   SetLength(Value : double);
    procedure   SetDisplayMarker(Index : integer; Value : TmcmProfDispMarker);
    procedure   SetNodeSize(Value : integer);
    procedure   SetScanWidth(Value : integer);
    procedure   SetStartX(Value : integer);
    procedure   SetStartY(Value : integer);
    procedure   StyleChanged(Sender : TObject);

    property    HitDistance : integer
      read      FHitDist
      write     FHitDist default 3;
    property    MarkerNegative : TmcmProfDispMarker index 1
      read      GetDisplayMarker
      write     SetDisplayMarker;
    property    MarkerPeak : TmcmProfDispMarker index 2
      read      GetDisplayMarker
      write     SetDisplayMarker;
    property    MarkerPositive : TmcmProfDispMarker index 0
      read      GetDisplayMarker
      write     SetDisplayMarker;
    property    MarkerValley : TmcmProfDispMarker index 3
      read      GetDisplayMarker
      write     SetDisplayMarker;
    property    NodeSize : integer
      read      FNodeSize
      write     SetNodeSize default 3;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Invert;
    function    Scan : TmcmErrorCode; virtual;
    procedure   Stretch;

    property    Angle : double
      read      GetAngle
      write     SetAngle;
    property    Average : integer
      read      FAverage
      write     SetAverage default 1;
    property    EndX : integer
      read      GetEndX
      write     SetEndX;
    property    EndY : integer
      read      GetEndY
      write     SetEndY;
    property    Error : TmcmErrorCode
      read      FError;
    property    Image : TmcmImage
      read      GetImage
      write     SetImage;
    property    Interpolate : TmcmProfInterpolate
      read      FInterpolate
      write     SetInterpolate default PI_NONE;
    property    Length : double
      read      FLength
      write     SetLength;
    property    ScanData[Index : integer] : integer
      read      GetScanData;
    property    ScanLength : integer
      read      FScanLength;
    property    ScanWidth : integer
      read      FScanWidth
      write     SetScanWidth default 1;
    property    StartX : integer
      read      GetStartX
      write     SetStartX;
    property    StartY : integer
      read      GetStartY 
      write     SetStartY;
  published
    // Published declarations
    property    Align;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    ParentShowHint;
    property    Scale;
    property    ShowHint;
    property    Visible;
    property    OnChange : TNotifyEvent
      read      FOnChange
      write     FOnChange;
    property    OnClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;


//------------------------------------------------------------------------------
// TmcmProfile
//------------------------------------------------------------------------------

  TmcmProfile = class(TmcmCustomProfile)
  private
    FDerivative    : PVectorI; // Derivative of FScanData.
    FMinDerivative : integer;
    FHysteresis    : integer;
    FTransitions   : TmcmProfTransitions;
    FCenterPeaks   : boolean;
    FPoints        : TList;
    FMarkerPen     : TPen;
    FCursorOnLine  : TCursor;
    FCursorOnNode  : TCursor;

    procedure   CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure   CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure   WMMouseMove(var Msg : TMessage); message WM_MOUSEMOVE;
    procedure   WMLButtonDown(var Msg : TMessage); message WM_LBUTTONDOWN;
    procedure   WMLButtonUp(var Msg : TMessage); message WM_LBUTTONUP;
  protected
    procedure   ClearPoints;
    function    GetDerivative(Index : integer) : integer;
    function    GetNoTransitions : integer;
    function    GetTransitionIndex(Index : integer) : integer;
    function    GetTransitionType(Index : integer) : TmcmProfTransition;
    function    GetTransitionX(Index : integer) : double;
    function    GetTransitionY(Index : integer) : double;
    procedure   InvalidateNodes;
    procedure   InvalidateProfile;
    procedure   Paint; override;
    procedure   SetCenterPeaks(Value : boolean);
    procedure   SetHysteresis(Value : integer);
    procedure   SetMarkerPen(Value : TPen);
    procedure   SetMinDerivative(Value : integer);
    procedure   SetTransitions(Value : TmcmProfTransitions);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    LocateTransitions : TmcmErrorCode;
    function    Scan : TmcmErrorCode; override;
    property    Derivative[Index : integer] : integer
      read      GetDerivative;
    property    Image;
    property    NoTransitions : integer
      read      GetNoTransitions;
    property    ScanData;
    property    ScanLength;
    property    TransitionIndex[Index : integer] : integer
      read      GetTransitionIndex;
    property    TransitionType[Index : integer] : TmcmProfTransition
      read      GetTransitionType;
    property    TransitionX[Index : integer] : double
      read      GetTransitionX;
    property    TransitionY[Index : integer] : double
      read      GetTransitionY;
  published
    // Published declarations
    property    Align;
    property    Angle;
    property    Average;
    property    Brush;
    property    CenterPeaks : boolean
      read      FCenterPeaks
      write     SetCenterPeaks default False;
    property    CursorOnLine : TCursor
      read      FCursorOnLine
      write     FCursorOnLine default crHandPoint;
    property    CursorOnNode : TCursor
      read      FCursorOnNode
      write     FCursorOnNode default crCross;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    EndX;
    property    EndY;
    property    HitDistance default 3;
    property    Hysteresis : integer
      read      FHysteresis
      write     SetHysteresis default 25;
    property    Interpolate default PI_NONE;
    property    Length : double
      read      FLength
      write     SetLength;
    property    MarkerNegative;
    property    MarkerPeak;
    property    MarkerPen : TPen
      read      FMarkerPen
      write     SetMarkerPen;
    property    MarkerPositive;
    property    MarkerValley;
    property    MinDerivative : integer
      read      FMinDerivative
      write     SetMinDerivative default 10;
    property    NodeSize default 3;
    property    ParentShowHint;
    property    Pen;
    property    ScanWidth default 1;
    property    Scale;
    property    ShowHint;
    property    StartX;
    property    StartY;
    property    Transitions : TmcmProfTransitions
      read      FTransitions
      write     SetTransitions;
    property    Visible;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
    property    OnChange;
    property    OnClick;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;

(*
  TCircumscribedCircle = record
    Centre : TPointDouble;
    Radius : double;
    procedure Clear;
  end;
*)  
  TVectorPoint    = array[0..0] of TPoint;
  PVectorPoint    = ^TVectorPoint;

  TmcmCustomPolygon = class(TmcmGraphicControl)
  // Calculate object data
  private
  protected
    FImage           : TmcmImage; // The image from which density/intensity
                                  // data is extracted.
    FxRes            : double;    // The horizontal resolution.
    FyRes            : double;    // The vertical resolution.
    FOnChange        : TNotifyEvent; // On mcmTracePolygon change event.

    //FTraceType       : TmcmTraceType;
    FCount           : longint;
    FMemSize         : longint;
    FMemInc          : longint;
    FTrace           : PVectorPt;
    FDrawTrace       : PVectorPoint;
    FBoundRect       : TRect;
    FIsClosed        : boolean;

    // Members indicating that a shape factor has been calculated.
    FBoundingCalced  : boolean;
    FGeometricCalced : boolean;
    FDensCalced      : boolean;
    FOrientCalced    : boolean;
    FLengthCalced    : boolean;
    FCGCalced        : boolean;
    FMaxMinRadius    : boolean;
    FMaxFeretCalced  : boolean;
    FMinCircleCalced : boolean;

    // Chain code based shape results.
    FChainArea       : double;   // Area calculated based on chain code.
    FChainCG         : TPointDouble; // Centre of gravity based on chain code area.
    FPerimeter       : double;   // Perimeter of chain code.
    FLength          : double;   // The longest chord, max distance between two points.
    FBreadth         : double;   // Max. Width perpendicular to the longest chord.
    FAveFiberWidth   : double;   // Average width perpendicular to the longest chord.
    FMaxRadius       : double;
    FMinRadius       : double;
    FMaxFeret        : double;
    FFeretStart      : TmcmPoint;
    FFeretEnd        : TmcmPoint;

    // Density based shape results.
    FCG              : TPointDouble; // Centre of Gravity.
    FPixelArea       : longint;      // Area of summed contained pixels.
    FAveDensity      : double;       // Average density.
    FSumDensity      : double;       // Integrated density.
    FAveLuminance    : double;       // Average intensity.
    FSumLuminance    : double;       // Integrated intensity.
    FDensityGC       : TPointDouble; // Centre of gravity based on density.
    FLuminanceGC     : TPointDouble; // Centre of gravity based on luminance.
    FOrientation     : double;       // Orientation based on pixel area.

    // Convex polygon.
    FConvexObj       : TmcmCustomPolygon;

    FIndex           : integer;  // Object's index.
    FLengthStart     : integer;
    FLengthEnd       : integer;
    FBreadthStart    : integer;
    FBreadthEnd      : integer;
    //FSmallestCircle  : TCircumscribedCircle;

    procedure   SwapTraceData;
    function    GetBoundingRect : TRect;
    procedure   SetImage(Value : TmcmImage);
    procedure   SetXResolution(Value : double);
    procedure   SetYResolution(Value : double);

    function    GetLastIndex : integer;
    procedure   CalcConvexData;
    procedure   CalcDensityData;
    procedure   CalcGeometricData;
    procedure   CalcMaxMinRadius;
    function    InternRotate(pDblData : PVectorPtD; Radian : double) : boolean;
(*
    procedure   FindCircle(var Circle : TCircumscribedCircle; const Points : TList<TmcmPoint>; a, b : integer; var Indexes : TList<integer>; var Exclude : TList<integer>);
    function    CheckArcLength(var Circle : TCircumscribedCircle; const MinDist : double; const Points : TList<TmcmPoint>; const Indexes : TList<integer>; var a : integer; var b : integer; var Rad : TList<double>) : boolean;
    procedure   CalcSmallestCircle;
*)
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Add(x, y : integer);
    procedure   Changed(Sender : TObject); virtual;
    procedure   Clear;
    procedure   ClearCalculations;
    function    Count : longint;
    function    GetArea : double; virtual;
    function    GetAreaCentroid : TPointDouble; virtual;
    function    GetAspectRatio : double; virtual;
    function    GetAverageDensity : double; virtual;
    function    GetAverageLuminance : double; virtual;
    function    GetBreadth : double; virtual;
    function    GetCompactness : double; virtual;
    function    GetConvexHull : TmcmCustomPolygon;
    function    GetConvexity : double; virtual;
    //function    GetCurl : double; virtual;
    function    GetDensityCentroid : TPointDouble; virtual;
    function    GetElongation : double; virtual;
    //function    GetExtent : double; virtual;
    //function    GetFiberLength : double; virtual;
    //function    GetFiberWidth : double; virtual;
    function    GetFormFactor : double; virtual;
    function    GetHeywoodDiameter : double; virtual;
    function    GetLength : double; virtual;
    function    GetLuminanceCentroid : TPointDouble; virtual;
    function    GetMaxFeret : double;
    function    GetMaxRadius : double; virtual;
    function    GetMinRadius : double; virtual;
    //function    GetModificationRatio : double; virtual;
    function    GetOrientation : double; virtual;
    function    GetOutlineCentroid : TPointDouble; virtual;
    function    GetPerimeter : double; virtual;
    function    GetRoughness : double; virtual;
    function    GetRoundness : double; virtual;
    function    GetSolidity : double; virtual;
    function    GetSumDensity : double; virtual;
    function    GetSumLuminance : double; virtual;
    //function    GetSmallestCircle : TCircumscribedCircle; virtual;
    function    HitCrossingNumber(x, y : integer) : boolean;
    function    HitWindingNumber(x, y : integer) : boolean;
    function    HitTest(x, y : integer) : TmcmHitTest;
    function    IsClosed : boolean;
    function    IsLeft(P0, P1, P2 : TmcmPoint) : integer;
    procedure   Paint; override;
    function    ReleaseConvexHull : TmcmCustomPolygon;
    function    ResizeData(NewSize : longint) : longint;

    property    BoundingRect : TRect
      read      GetBoundingRect;
//    property    Coordinates[Index : integer] : TPoint
//      read      GetCoordinates;
    property    Image : TmcmImage
      read      FImage
      write     SetImage;
    property    Index : integer
      read      FIndex
      write     FIndex;
    property    TraceData : PVectorPt
      read      FTrace
      write     FTrace;
    property    XResolution : double
      read      FxRes
      write     SetXResolution;
    property    YResolution : double
      read      FyRes
      write     SetYResolution;

    property    PixelArea : integer
      read      FPixelArea;
  published
    // Published declarations
    property    Align;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    ParentShowHint;
    property    Scale;
    property    ShowHint;
    property    Visible;
    property    OnChange : TNotifyEvent
      read      FOnChange
      write     FOnChange;
    property    OnClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;


  TmcmPolygon = class(TmcmCustomPolygon)
  private
    // Private declarations
    FLBtnDown        : boolean;      // Indicates if left mouse button is down.
    FHitIndex        : TmcmHitTest;  // Indicates where the object was hit.
    Fmx, Fmy         : integer;      // Point where the polygon is grabbed.

    procedure   CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure   CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure   WMMouseMove(var Msg : TMessage); message WM_MOUSEMOVE;
    procedure   WMLButtonDown(var Msg : TMessage); message WM_LBUTTONDOWN;
    procedure   WMLButtonUp(var Msg : TMessage); message WM_LBUTTONUP;
  protected
    // Protected declarations
    FShapePen           : TPen;    // Profile & end point's line
    FShowAreaCenter     : boolean;
    FShowBreadth        : boolean;
    FShowConvex         : boolean;
    FShowDensCenter     : boolean;
    FShowIndex          : boolean;
    FShowLumCenter      : boolean;
    FShowLength         : boolean;
    FShowMaxFeret       : boolean;
    FShowMaxRadius      : boolean;
    FShowMinRadius      : boolean;
    FShowOutline        : boolean;
    FShowChainCenter    : boolean;
    FShowSmallestCircle : boolean;
    procedure   SetShapePen(Value : TPen);
    procedure   SetShowAreaCenter(Value : boolean);
    procedure   SetShowBreadth(Value : boolean);
    procedure   SetShowConvex(Value : boolean);
    procedure   SetShowDensCenter(Value : boolean);
    procedure   SetShowIndex(Value : boolean);
    procedure   SetShowLumCenter(Value : boolean);
    procedure   SetShowLength(Value : boolean);
    procedure   SetShowMaxFeret(Value : boolean);
    procedure   SetShowMaxRadius(Value : boolean);
    procedure   SetShowMinRadius(Value : boolean);
    procedure   SetShowOutline(Value : boolean);
    procedure   SetShowOutlineCenter(Value : boolean);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;

    property    ShapePen : TPen
      read      FShapePen
      write     SetShapePen;

    property    ShowAreaCenter : boolean
      read      FShowAreaCenter
      write     SetShowAreaCenter default False;
    property    ShowBreadth : boolean
      read      FShowBreadth
      write     SetShowBreadth default False;
    property    ShowConvex : boolean
      read      FShowConvex
      write     SetShowConvex default False;
    property    ShowDensityCenter : boolean
      read      FShowDensCenter
      write     SetShowDensCenter default False;
    property    ShowIndex : boolean
      read      FShowIndex
      write     SetShowIndex default False;
    property    ShowLuminanceCenter : boolean
      read      FShowLumCenter
      write     SetShowLumCenter default False;
    property    ShowLength : boolean
      read      FShowLength
      write     SetShowLength default False;
    property    ShowMaxFeret : boolean
      read      FShowMaxFeret
      write     SetShowMaxFeret default False;
    property    ShowMaxRadius : boolean
      read      FShowMaxRadius
      write     SetShowMaxRadius default False;
    property    ShowMinRadius : boolean
      read      FShowMinRadius
      write     SetShowMinRadius default False;
    property    ShowOutline : boolean
      read      FShowOutline
      write     SetShowOutline default True;
    property    ShowOutlineCenter : boolean
      read      FShowChainCenter
      write     SetShowOutlineCenter default False;
    property    ShowSmallestCircle : boolean
      read      FShowSmallestCircle
      write     FShowSmallestCircle default True;
  published
    // Published declarations
    property    Align;
    property    Brush;
    property    Cursor;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    HitIndex : TmcmHitTest
      read      FHitIndex;
    property    ParentShowHint;
    property    Pen;
    property    Scale;
    property    ShowHint;
    property    Visible;
    property    OnChange;
    property    OnClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;

implementation

uses {$IFNDEF GE_DXE2} SysUtils; {$ELSE} System.Math, System.SysUtils; {$ENDIF}

type
  // This class is added to access the hidden DibInfo property on TmcmImage.
  TmcmCastImage = class(TmcmImage)
  public
    property DibInfo;
  end;

  TmcmTransitPoint = record
  Index   : integer;
  x       : double;
  y       : double;
  Area    : integer;
  Transit : TmcmProfTransition;
  end;
  PmcmTransitPoint = ^TmcmTransitPoint;


(*
class operator TPoint.IntDivide(a : TPoint; r : integer) : TPoint;    
var b : TPoint;
begin
  Result.X := (a.X div r);
  Result.Y := a.Y div r;
end;
*)

//------------------------------------------------------------------------------
// TmcmGraphicControl
//------------------------------------------------------------------------------

constructor TmcmGraphicControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];

  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;

  FScale  := 1.0;
  Height := 105;
  Width  := 105;
end; // TmcmGraphicControl.Create.


destructor TmcmGraphicControl.Destroy;
begin
  if Assigned(FPen)
  then FPen.Free;
  FPen := Nil;
  if Assigned(FBrush)
  then FBrush.Free;
  FBrush := Nil;
  Inherited Destroy;
end; // TmcmGraphicControl.Destroy.


procedure TmcmGraphicControl.StyleChanged(Sender : TObject);
begin
  if (Parent <> Nil)
  then Invalidate;
end; // TmcmGraphicControl.StyleChanged.


procedure TmcmGraphicControl.SetBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end; // TmcmGraphicControl.SetBrush.


procedure TmcmGraphicControl.SetPen(Value : TPen);
begin
  FPen.Assign(Value);
end; // TmcmGraphicControl.SetPen.


function TmcmGraphicControl.GetHeight : integer;
begin
  Result := Inherited Height;
end; // TmcmGraphicControl.GetHeight.


function TmcmGraphicControl.GetLeft : integer;
begin
  Result := Inherited Left;
end; // TmcmGraphicControl.GetLeft.


function TmcmGraphicControl.GetTop : integer;
begin
  Result := Inherited Top;
end; // TmcmGraphicControl.GetTop.


function TmcmGraphicControl.GetWidth : integer;
begin
  Result := Inherited Width;
end; // TmcmGraphicControl.GetWidth.


procedure TmcmGraphicControl.SetHeight(Value : integer);
begin
  if (FHeight <> Value)
  then begin
       FHeight := Value / FScale;
       FMaxY := Round(FHeight);
       Inherited Height := Value;
  end;
end; // TmcmGraphicControl.SetHeight.


procedure TmcmGraphicControl.SetLeft(Value : integer);
begin
  if (FLeft <> Value)
  then begin
       FLeft := Value / FScale;
       Inherited Left := Value;
  end;
end; // TmcmGraphicControl.SetLeft.


procedure TmcmGraphicControl.SetScale(Value : double);
var hx, vy : integer;
begin
  if (FScale <> Value) and (Value > 0.0)
  then begin
       FScale := Value;
       // Get parent scroll position.
       hx := 0;
       vy := 0;
       if Assigned(Parent)
       then begin
            if (Parent is TScrollingWinControl)
            then begin
                 hx := TScrollingWinControl(Parent).HorzScrollBar.Position;
                 vy := TScrollingWinControl(Parent).VertScrollBar.Position;
            end;
       end;

       // Calculate new control coordinates.
       // Do not change assignment order!
       Inherited Width := Round(FWidth * FScale);
       Inherited Height := Round(FHeight * FScale);
       Inherited Left := Round(FLeft * FScale) - hx;
       Inherited Top := Round(FTop * FScale) - vy;
  end;
end; // TmcmGraphicControl.SetScale.


procedure TmcmGraphicControl.SetTop(Value : integer);
begin
  if (FTop <> Value)
  then begin
       FTop := Value / FScale;
       Inherited Top := Value;
  end;
end; // TmcmGraphicControl.SetTop.


procedure TmcmGraphicControl.SetWidth(Value : integer);
begin
  if (FWidth <> Value)
  then begin
       FWidth := Value / FScale;
       FMaxX := Round(FWidth);
       Inherited Width := Value; 
  end;
end; // TmcmGraphicControl.SetWidth.


function TmcmGraphicControl.CheckX(X : integer) : integer;
begin
  if (X < 0)
  then Result := 0
  else if (X >= FMaxX)
       then Result := FMaxX - 1
       else Result := X;
end; // TmcmGraphicControl.CheckX.


function TmcmGraphicControl.CheckY(Y : integer) : integer;
begin
  if (Y < 0)
  then Result := 0
  else if (Y >= FMaxY)
       then Result := FMaxY - 1
       else Result := Y;
end; // TmcmGraphicControl.CheckY.


function TmcmGraphicControl.ControlXOffset : integer;
// Returns the horizontal offset from the control's left side to the owners
// origin including horizontal scrollbar position.
begin
  Result := Left;
  // Get parent scroll position.
  if Assigned(Parent)
  then if (Parent is TScrollingWinControl)
       then Result := Round((Left + TScrollingWinControl(Parent).HorzScrollBar.Position) / FScale);
end; // TmcmGraphicControl.ControlXOffset.


function TmcmGraphicControl.ControlYOffset : integer;
// Returns the vertical offset from the control's upper side to the owners
// origin including vertical scrollbar position.
begin
  Result := Top;
  // Get parent scroll position.
  if Assigned(Parent)
  then if (Parent is TScrollingWinControl)
       then Result := Round((Top + TScrollingWinControl(Parent).VertScrollBar.Position) / FScale);
end; // TmcmGraphicControl.ControlYOffset.


//------------------------------------------------------------------------------
// TmcmCustomProfile
//------------------------------------------------------------------------------

constructor TmcmCustomProfile.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  FError := EC_OK;
  FOnChange := Nil;

  Fcx         := Width div 2;
  Fcy         := Height div 2;
  FAngle      := 0;
  FSlope      := 0;
  FStartPos.x := 25;
  FStartPos.y := Round(Fcy);
  FEndPos.x   := 75;
  FEndPos.y   := Round(Fcy);
  Fcy         := FStartPos.y;

  Fdx         := FEndPos.x - FStartPos.x;
  Fdy         := FEndPos.y - FStartPos.y;
  FLength     := Sqrt(Fdx * Fdx + Fdy * Fdy);
  FRadius     := FLength / 2.0;

  FImage := Nil;
  if (Owner is TmcmImageCtrl)
  then FImage := TmcmImageCtrl(AOwner).Image;

  FShowEndPoints  := True;
  FDoShowEndPts   := False;
  FNodeSize       := 3;
  FHitDist        := 3;
  FHitIndex       := -1;
  FLBtnDown       := False;

  FSubPixel       := False;
  FScanData       := Nil;
  FScanLength     := 0;
  FInterpolate    := PI_NONE;
  FAverage        := 1;
  FScanWidth      := 1;
  FMaxValue       := 255;

  FSWCoorAx       := Nil;
  FSWCoorAy       := Nil;
  FSWCoorBx       := Nil;
  FSWCoorBy       := Nil;

  FDispMarker[0]  := PDM_CROSS;
  FDispMarker[1]  := PDM_CROSS;
  FDispMarker[2]  := PDM_CROSS;
  FDispMarker[3]  := PDM_CROSS;

  FDoRescan       := False;
  FModified       := False;
end; // TmcmCustomProfile.Create.


procedure TmcmCustomProfile.Clear;
begin
  if (FSWCoorAx <> Nil)
  then FreeMem(FSWCoorAx);
  FSWCoorAx       := Nil;
  if (FSWCoorAy <> Nil)
  then FreeMem(FSWCoorAy);
  FSWCoorAy       := Nil;
  if (FSWCoorBx <> Nil)
  then FreeMem(FSWCoorBx);
  FSWCoorBx       := Nil;
  if (FSWCoorBy <> Nil)
  then FreeMem(FSWCoorBy);
  FSWCoorBy       := Nil;

  if Assigned(FScanData)
  then FreeMem(FScanData);
  FScanData := Nil;
end; // TmcmCustomProfile.Clear.


destructor TmcmCustomProfile.Destroy;
begin
  Clear;
  inherited Destroy;
end; // TmcmCustomProfile.Destroy.


procedure TmcmCustomProfile.Changed(Sender : TObject);
begin
  FModified := True;
  if Assigned(FOnChange)
  then FOnChange(Self);
end; // TmcmCustomProfile.Changed.


function TmcmCustomProfile.GetImage : TmcmImage;
begin
  Result := FImage;
end; // TmcmCustomProfile.GetImage.


procedure TmcmCustomProfile.SetImage(Value : TmcmImage);
begin
  if (FImage <> Value)
  then begin
       FImage := Value;
       if (FImage <> Nil)
       then begin
            case FImage.ImageFormat of
            IF_BW     : FMaxValue := 255;
            IF_GREY8,
            IF_PAL8,
            IF_RGB24,
            IF_RGBA32 : FMaxValue := 255;
            end;
            FDoRescan := True;
            Changed(Self);
       end;
  end;
end; // TmcmCustomProfile.SetImage.


procedure TmcmCustomProfile.CalcAngle;
var s : double;
begin
  Fdx := (FEndPos.x - FStartPos.x);
  Fdy := (FEndPos.y - FStartPos.y);

  FLength := sqrt(Fdx * Fdx + Fdy * Fdy);
  FRadius := FLength / 2.0;

  // Centre x,y of profile line.
  Fcx := FStartPos.x + Fdx / 2.0;
  Fcy := FStartPos.y + Fdy / 2.0;

  // calculate the angle of the profile line.
  // When dy < 0
  //    when dx > 0 -> Angle in (0..90)
  //    when dx < 0 -> Angle in (90..180)
  // When dy > 0
  //    when dx < 0 -> Angle in (180..270)
  //    when dx > 0 -> Angle in (270..360)
  if (Fdy <> 0.0)
  then begin
       if (Fdx <> 0.0)
       then begin
            FSlope := Fdy / Fdx;
            s := 180.0 * ArcTan(Fdy / Fdx) / pi;
            if (Fdy < 0.0)
            then begin
                 if (Fdx > 0.0)
                 then FAngle := -s
                 else FAngle := (180.0 - s);
            end
            else begin
                 if (Fdx < 0.0)
                 then FAngle := 180.0 - s
                 else FAngle := 270.0 + (90.0 - s);
            end;
       end
       else begin
            if (Fdy < 0.0)
            then FAngle := 90.0
            else FAngle := 270.0;
       end;
  end
  else begin
       FSlope := 0.0;
       if (Fdx < 0.0)
       then FAngle := 180.0
       else FAngle := 0;
  end;
  FDoRescan := True;
end; // TmcmCustomProfile.CalcAngle.


procedure TmcmCustomProfile.CalcEndPoints;
var s : double;
begin
  s := pi * FAngle / 180.0;
  // Calculate new start & end points.
  FStartPos.x := Round(Fcx - FRadius * cos(s));
  FStartPos.y := Round(Fcy + FRadius * sin(s));
  FEndPos.x := Round(Fcx + FRadius * cos(s));
  FEndPos.y := Round(Fcy - FRadius * sin(s));
  Fdx := (FEndPos.x - FStartPos.x);
  Fdy := (FEndPos.y - FStartPos.y);
  FDoRescan := True;
end; // TmcmCustomProfile.CalcEndPoints.


function TmcmCustomProfile.GetAngle : double;
begin
  Result := FAngle;
end; // TmcmCustomProfile.GetAngle;


procedure TmcmCustomProfile.SetAngle(Value : double);
begin
  if (FAngle <> Value)
  then begin
       FAngle := Value;
       // Calculate start & end points
       CalcEndPoints;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetAngle.


procedure TmcmCustomProfile.SetLength(Value : double);
begin
  if (FLength <> Value)
  then begin
       FLength := Value;
       // Calculate start & end points
       FRadius := FLength / 2.0;
       CalcEndPoints;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetLength.


function TmcmCustomProfile.GetDisplayMarker(Index : integer) : TmcmProfDispMarker;
begin
  Result := FDispMarker[Index];
end; // TmcmCustomProfile.GetDisplayMarker.


procedure TmcmCustomProfile.SetDisplayMarker(Index : integer; Value : TmcmProfDispMarker);
begin
  if (FDispMarker[Index] <> Value)
  then begin
       FDispMarker[Index] := Value;
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetDisplayMarker.


procedure TmcmCustomProfile.SetNodeSize(Value : integer);
begin
  if (FNodeSize <> Value)
  then begin
       FNodeSize := Value;
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetNodeSize.


procedure TmcmCustomProfile.StyleChanged(Sender : TObject);
begin
  if (Parent <> Nil)
  then Invalidate;
end; // TmcmCustomProfile.StyleChanged.


function TmcmCustomProfile.GetStartX : integer;
begin
  Result := FStartPos.x;
end; // TmcmCustomProfile.GetStartX.


procedure TmcmCustomProfile.SetStartX(Value : integer);
begin
  if (FStartPos.x <> Value)
  then begin
       FStartPos.x := CheckX(Value);
       CalcAngle;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetStartX.


function TmcmCustomProfile.GetStartY : integer;
begin
  Result := FStartPos.y;
end; // TmcmCustomProfile.GetStartY.


procedure TmcmCustomProfile.SetStartY(Value : integer);
begin
  if (FStartPos.y <> Value)
  then begin
       FStartPos.y := CheckY(Value);
       CalcAngle;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetStartY.


function TmcmCustomProfile.GetEndX : integer;
begin
  Result := FEndPos.x;
end; // TmcmCustomProfile.GetEndX.


procedure TmcmCustomProfile.SetEndX(Value : integer);
begin
  if (FEndPos.x <> Value)
  then begin
       FEndPos.x := CheckX(Value);
       CalcAngle;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetEndX.


function TmcmCustomProfile.GetEndY : integer;
begin
  Result := FEndPos.y;
end; // TmcmCustomProfile.GetEndY.


procedure TmcmCustomProfile.SetEndY(Value : integer);
begin
  if (FEndPos.y <> Value)
  then begin
       FEndPos.y := CheckY(Value);
       CalcAngle;
       Changed(Self);
       if (Parent <> Nil)
       then Invalidate;
  end;
end; // TmcmCustomProfile.SetEndY.


procedure TmcmCustomProfile.SetAverage(Value : integer);
begin
  if (FAverage <> Value) and Odd(Value)
  then begin
       FAverage := Value;
       FDoRescan := True;
       Changed(Self);
  end;
end; // TmcmCustomProfile.SetAverage.


procedure TmcmCustomProfile.SetScanWidth(Value : integer);
begin
  if (FScanWidth <> Value)
  then begin
       FScanWidth := Value;
       if (FScanWidth < 1)
       then FScanWidth := 1;
       FDoRescan := True;
       Changed(Self);
  end;
end; // TmcmCustomProfile.SetScanWidth.


procedure TmcmCustomProfile.SetInterpolate(Value : TmcmProfInterpolate);
begin
  if (FInterpolate <> Value)
  then begin
       FInterpolate := Value;
       FDoRescan := True;
       Changed(Self);
  end;
end; // TmcmCustomProfile.SetInterpolate.


procedure TmcmCustomProfile.AverageScan;
var i, k     : integer;
    Sum, d   : integer;
    TempData : PVectorI;
begin
  k := FAverage shr 1;
  try
    GetMem(TempData, (FScanLength + FAverage) * SizeOf(integer));
    try
      CopyMemory(@TempData^[k], FScanData, FScanLength * SizeOf(integer));
      // Repeat first pixel in temp k times
      for i := 0 to (k - 1)
      do TempData^[i] := TempData^[k];
      // Repeat last pixel in temp k times
      for i := (FScanLength + k) to (FScanLength + FAverage - 1)
      do TempData^[i] := TempData^[FScanLength+k-1];

      // Average profile.
      Sum := 0;
      for i := 0 to (FAverage - 2)
      do Sum := Sum + TempData^[i];

      d := FAverage - 1;
      for i := 0 to (FScanLength - 1)
      do begin
         Sum := Sum + TempData^[d];
         FScanData^[i] := Sum div FAverage;
         Sum := Sum - TempData^[i];
         inc(d);
      end;
    finally
      if (TempData <> Nil)
      then FreeMem(TempData);
    end;
  except
    On E:EOutOfMemory
    do begin
       FError := EC_NOMEMORY;
    end;
  end;
end; // TmcmCustomProfile.AverageScan.


function TmcmCustomProfile.CalcPerpendicular : TmcmErrorCode;
var i : integer;
    sx, sy     : single;
    a          : single;
    swa, swb   : single;
    atan       : single;
    dax, day   : single;
    dbx, dby   : single;
begin
  if (FSWCoorAx <> Nil)
  then FreeMem(FSWCoorAx);
  if (FSWCoorAy <> Nil)
  then FreeMem(FSWCoorAy);
  if (FSWCoorBx <> Nil)
  then FreeMem(FSWCoorBx);
  if (FSWCoorBy <> Nil)
  then FreeMem(FSWCoorBy);

  try
    GetMem(FSWCoorAx, FScanLength * SizeOf(Single));
    GetMem(FSWCoorAy, FScanLength * SizeOf(Single));
    GetMem(FSWCoorBx, FScanLength * SizeOf(Single));
    GetMem(FSWCoorBy, FScanLength * SizeOf(Single));

    if (FSWCoorAx <> Nil) and (FSWCoorBx <> Nil) and
       (FSWCoorAy <> Nil) and (FSWCoorBy <> Nil)
    then begin
         if Odd(FScanWidth)
         then begin
              swa := (FScanWidth / 2.0) - 0.4999;
              swb := swa;
         end
         else begin
              swa := FScanWidth / 2.0;
              swb := swa - 1.0;
         end;

         sx := Fox;
         sy := Foy;
         if (Fdx <> 0.0)
         then begin
              if (Fdy <> 0.0)
              then begin
                   a := -Fdx / Fdy;
                   atan := ArcTan(a);
                   day := Sin(atan) * swa;
                   dax := Cos(atan) * swa;
                   dby := Sin(atan) * swb;
                   dbx := Cos(atan) * swb;
                   for i := 0 to (FScanLength - 1)
                   do begin
                      FSWCoorAx^[i] := sx + dax;
                      FSWCoorAy^[i] := sy + day;
                      FSWCoorBx^[i] := sx - dbx;
                      FSWCoorBy^[i] := sy - dby;
                      sx := sx + Fdsx;
                      sy := sy + Fdsy;
                   end;
              end
              else begin
                   for i := 0 to (FScanLength - 1)
                   do begin
                      FSWCoorAx^[i] := sx;
                      FSWCoorAy^[i] := sy - swa;
                      FSWCoorBx^[i] := sx;
                      FSWCoorBy^[i] := sy + swb;
                      sx := sx + Fdsx;
                   end;
              end;
         end
         else begin
              for i := 0 to (FScanLength - 1)
              do begin
                 FSWCoorAx^[i] := sx - swa;
                 FSWCoorAy^[i] := sy;
                 FSWCoorBx^[i] := sx + swb;
                 FSWCoorBy^[i] := sy;
                 sy := sy + Fdsy;
              end;
         end;
    end
    else FError := EC_NOMEMORY;

    if (0 > FSWCoorAx^[0]) or (FSWCoorAx^[0] >= FImage.Width) or
       (0 > FSWCoorAy^[0]) or (FSWCoorAy^[0] >= FImage.Height) or
       (0 > FSWCoorBx^[0]) or (FSWCoorBx^[0] >= FImage.Width) or
       (0 > FSWCoorBy^[0]) or (FSWCoorBy^[0] >= FImage.Height)
    then FError := EC_CHECKXY;

    if (0 > FSWCoorAx^[FScanLength-1]) or (FSWCoorAx^[FScanLength-1] >= FImage.Width) or
       (0 > FSWCoorAy^[FScanLength-1]) or (FSWCoorAy^[FScanLength-1] >= FImage.Height) or
       (0 > FSWCoorBx^[FScanLength-1]) or (FSWCoorBx^[FScanLength-1] >= FImage.Width) or
       (0 > FSWCoorBy^[FScanLength-1]) or (FSWCoorBy^[FScanLength-1] >= FImage.Height)
    then FError := EC_CHECKXY;
  except
    On E:EOutOfMemory
    do FError := EC_NOMEMORY;
  end;
  Result := FError;
end; // TmcmCustomProfile.CalcPerpendicular.


function TmcmCustomProfile.ScanBW(iLongWidth, iHeight : integer; pImageData : PVectorB): TmcmErrorCode;
var i, j, k    : integer;
    x, y       : integer;
    modx       : word;
    dx, dy     : single;
    ddx, ddy   : single;
    sx, sy     : single;
    dsx, dsy   : single;
    pixu, pixl : single;
    Sum        : integer;
begin
  FError := EC_OK;
  if Assigned(FScanData)
  then begin
       iHeight := iHeight - 1;
       case FInterpolate of
       PI_NONE     : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Round(sx);
                             y := iHeight - Round(sy);
                             j := (x shr 3) + y * iLongWidth;
                             modx := 7 - (x mod 8);

                             FScanData^[i] := 255 * ((pImageData^[j] and BitMask[modx]) shr modx);
                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin // FScanWidth > 1
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Round(sx);
                                y := iHeight - Round(sy);
                                k := (x shr 3) + y * iLongWidth;
                                modx := 7 - (x mod 8);
                                Sum := Sum + 255 * integer((pImageData^[k] and BitMask[modx]) shr modx);
                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       PI_BILINEAR : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Trunc(sx);
                             y := Trunc(sy);
                             ddx := sx - x;
                             ddy := sy - y;

                             k := (x shr 3) + (iHeight - y) * iLongWidth;
                             modx := 7 - (x mod 8);
                             pixu := (1.0 - ddx) * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                             inc(x);
                             k := (x shr 3) + (iHeight - y) * iLongWidth;
                             modx := 7 - (x mod 8);
                             pixu := pixu + ddx * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                             inc(y);
                             dec(x);

                             k := (x shr 3) + (iHeight - y) * iLongWidth;
                             modx := 7 - (x mod 8);
                             pixl := (1.0 - ddx) * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                             inc(x);
                             k := (x shr 3) + (iHeight - y) * iLongWidth;
                             modx := 7 - (x mod 8);
                             pixl := pixl + ddx * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                             pixu := pixu * (1.0 - ddy) + pixl * ddy;

                             FScanData^[i] := Round(pixu);

                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Trunc(sx);
                                y := Trunc(sy);
                                ddx := sx - x;
                                ddy := sy - y;

                                k := (x shr 3) + (iHeight - y) * iLongWidth;
                                modx := 7 - (x mod 8);
                                pixu := (1.0 - ddx) * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                                inc(x);
                                k := (x shr 3) + (iHeight - y) * iLongWidth;
                                modx := 7 - (x mod 8);
                                pixu := pixu + ddx * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                                inc(y);
                                dec(x);

                                k := (x shr 3) + (iHeight - y) * iLongWidth;
                                modx := 7 - (x mod 8);
                                pixl := (1.0 - ddx) * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                                inc(x);
                                k := (x shr 3) + (iHeight - y) * iLongWidth;
                                modx := 7 - (x mod 8);
                                pixl := pixl + ddx * 255 * ((pImageData^[k] and BitMask[modx]) shr modx);

                                pixu := pixu * (1.0 - ddy) + pixl * ddy;
                                Sum := Sum + Round(pixu);

                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmCustomProfile.ScanBW.


function TmcmCustomProfile.ScanGrey(iLongWidth, iHeight : integer; pImageData : PVectorB) : TmcmErrorCode;
var i, j, k    : integer;
    x, y       : integer;
    dx, dy     : single;
    ddx, ddy   : single;
    sx, sy     : single;
    dsx, dsy   : single;
    pixu, pixl : single;
    Sum        : integer;
begin
  FError := EC_OK;
  if Assigned(FScanData)
  then begin
       iHeight := iHeight - 1;
       case FInterpolate of
       PI_NONE     : if (FScanWidth = 1)
                     then begin
                          {
                          sx := Fox + Fsi * Fdsx;
                          sy := Foy + Fsi * Fdsy;
                          }
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Round(sx);
                             y := iHeight - Round(sy);
                             j := x + y * iLongWidth;
                             FScanData^[i] := pImageData^[j];
                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin // FScanWidth > 1
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Round(sx);
                                y := iHeight - Round(sy);
                                k := x + y * iLongWidth;
                                Sum := Sum + pImageData^[k];
                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                          {
                          // Show scan border
                          for i := 0 to (FScanLength - 1)
                          do begin
                             if (FScanWidth > 1)
                             then begin
                                  FImage.Pixel[Round(FSWCoorAx^[i]),Round(FSWCoorAy^[i])] := RGB(255,255,255);
                                  FImage.Pixel[Round(FSWCoorBx^[i]),Round(FSWCoorBy^[i])] := RGB(192,192,192);
                             end;
                          end;
                          }
                     end;
       PI_BILINEAR : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Trunc(sx);
                             y := Trunc(sy);
                             ddx := sx - x;
                             ddy := sy - y;
                             k := x + (iHeight - y) * iLongWidth;

                             pixu := pImageData^[k] * (1.0 - ddx) + pImageData^[k+1] * ddx;
                             dec(k, iLongWidth);
                             pixl := pImageData^[k] * (1.0 - ddx) + pImageData^[k+1] * ddx;
                             pixu := pixu * (1.0 - ddy) + pixl * ddy;
                             FScanData^[i] := Round(pixu);

                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Trunc(sx);
                                y := Trunc(sy);
                                ddx := sx - x;
                                ddy := sy - y;
                                k := x + (iHeight - y) * iLongWidth;

                                pixu := pImageData^[k] * (1.0 - ddx) + pImageData^[k+1] * ddx;
                                dec(k, iLongWidth);
                                pixl := pImageData^[k] * (1.0 - ddx) + pImageData^[k+1] * ddx;
                                pixu := pixu * (1.0 - ddy) + pixl * ddy;
                                Sum := Sum + Round(pixu);

                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmCustomProfile.ScanGrey.


function TmcmCustomProfile.ScanPal(iLongWidth, iHeight : integer; pImageData : PVectorB; const Palette : array of TRGBQuad) : TmcmErrorCode;
var i, j, k    : integer;
    x, y       : integer;
    dx, dy     : single;
    ddx, ddy   : single;
    sx, sy     : single;
    dsx, dsy   : single;
    pixu, pixl : single;
    Sum        : integer;
begin
  FError := EC_OK;
  if Assigned(FScanData)
  then begin
       iHeight := iHeight - 1;
       case FInterpolate of
       PI_NONE     : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Round(sx);
                             y := iHeight - Round(sy);
                             j := x + y * iLongWidth;
                             FScanData^[i] := Round(0.2989 * Palette[pImageData^[j]].rgbBlue + // B
                                                    0.5867 * Palette[pImageData^[j]].rgbGreen + // G
                                                    0.1144 * Palette[pImageData^[j]].rgbRed);   // R
                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin // FScanWidth > 1
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Round(sx);
                                y := iHeight - Round(sy);
                                k := x + y * iLongWidth;
                                Sum := Sum + Round(0.2989 * Palette[pImageData^[k]].rgbBlue + // B
                                                   0.5867 * Palette[pImageData^[k]].rgbGreen + // G
                                                   0.1144 * Palette[pImageData^[k]].rgbRed);   // R
                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       PI_BILINEAR : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Trunc(sx);
                             y := Trunc(sy);
                             ddx := sx - x;
                             ddy := sy - y;
                             k := x + (iHeight - y) * iLongWidth;

                             pixu := (1.0 - ddx) * (0.2989 * Palette[pImageData^[k]].rgbBlue + // B
                                                    0.5867 * Palette[pImageData^[k]].rgbGreen + // G
                                                    0.1144 * Palette[pImageData^[k]].rgbRed) +  // R
                                     ddx         * (0.2989 * Palette[pImageData^[k+1]].rgbBlue + // B
                                                    0.5867 * Palette[pImageData^[k+1]].rgbGreen + // G
                                                    0.1144 * Palette[pImageData^[k+1]].rgbRed);   // R
                             dec(k, iLongWidth);
                             pixl := (1.0 - ddx) * (0.2989 * Palette[pImageData^[k]].rgbBlue + // B
                                                    0.5867 * Palette[pImageData^[k]].rgbGreen + // G
                                                    0.1144 * Palette[pImageData^[k]].rgbRed) +  // R
                                     ddx         * (0.2989 * Palette[pImageData^[k+1]].rgbBlue + // B
                                                    0.5867 * Palette[pImageData^[k+1]].rgbGreen + // G
                                                    0.1144 * Palette[pImageData^[k+1]].rgbRed);   // R
                             pixu := pixu * (1.0 - ddy) + pixl * ddy;
                             FScanData^[i] := Round(pixu);

                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Trunc(sx);
                                y := Trunc(sy);
                                ddx := sx - x;
                                ddy := sy - y;
                                k := x + (iHeight - y) * iLongWidth;

                                pixu := (1.0 - ddx) * (0.2989 * Palette[pImageData^[k]].rgbBlue + // B
                                                       0.5867 * Palette[pImageData^[k]].rgbGreen + // G
                                                       0.1144 * Palette[pImageData^[k]].rgbRed) +  // R
                                        ddx         * (0.2989 * Palette[pImageData^[k+1]].rgbBlue + // B
                                                       0.5867 * Palette[pImageData^[k+1]].rgbGreen + // G
                                                       0.1144 * Palette[pImageData^[k+1]].rgbRed);   // R
                                dec(k, iLongWidth);
                                pixl := (1.0 - ddx) * (0.2989 * Palette[pImageData^[k]].rgbBlue + // B
                                                       0.5867 * Palette[pImageData^[k]].rgbGreen + // G
                                                       0.1144 * Palette[pImageData^[k]].rgbRed) +  // R
                                        ddx         * (0.2989 * Palette[pImageData^[k+1]].rgbBlue + // B
                                                       0.5867 * Palette[pImageData^[k+1]].rgbGreen + // G
                                                       0.1144 * Palette[pImageData^[k+1]].rgbRed);   // R
                                pixu := pixu * (1.0 - ddy) + pixl * ddy;
                                Sum := Sum + Round(pixu);

                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmCustomProfile.ScanPal.


function TmcmCustomProfile.ScanRGB(iLongWidth, iHeight : integer; pImageData : PVectorB; NoCh : integer) : TmcmErrorCode;
var i, j, k    : integer;
    x, y       : integer;
    dx, dy     : single;
    ddx, ddy   : single;
    sx, sy     : single;
    dsx, dsy   : single;
    pixu, pixl : single;
    Sum        : integer;
begin
  FError := EC_OK;
  if Assigned(FScanData)
  then begin
       iHeight := iHeight - 1;
       case FInterpolate of
       PI_NONE     : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := NoCh * Round(sx);
                             y := iHeight - Round(sy);
                             j := x + y * iLongWidth;
                             FScanData^[i] := Round(0.2989 * pImageData^[j+2] + // B
                                                    0.5867 * pImageData^[j+1] + // G
                                                    0.1144 * pImageData^[j]);   // R
                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin // FScanWidth > 1
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := NoCh * Round(sx);
                                y := iHeight - Round(sy);
                                k := x + y * iLongWidth;
                                Sum := Sum + Round(0.2989 * pImageData^[k+2] + // B
                                                   0.5867 * pImageData^[k+1] + // G
                                                   0.1144 * pImageData^[k]);   // R
                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       PI_BILINEAR : if (FScanWidth = 1)
                     then begin
                          sx := Fox;
                          sy := Foy;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             x := Trunc(sx);
                             y := Trunc(sy);
                             ddx := sx - x;
                             ddy := sy - y;
                             k := NoCh * x + (iHeight - y) * iLongWidth;

                             pixu := (1.0 - ddx) * (0.2989 * pImageData^[k+2] + // B
                                                    0.5867 * pImageData^[k+1] + // G
                                                    0.1144 * pImageData^[k]) +  // R
                                     ddx         * (0.2989 * pImageData^[k+2+NoCh] + // B
                                                    0.5867 * pImageData^[k+1+NoCh] + // G
                                                    0.1144 * pImageData^[k+NoCh]);   // R
                             dec(k, iLongWidth);
                             pixl := (1.0 - ddx) * (0.2989 * pImageData^[k+2] + // B
                                                    0.5867 * pImageData^[k+1] + // G
                                                    0.1144 * pImageData^[k]) +  // R
                                     ddx         * (0.2989 * pImageData^[k+2+NoCh] + // B
                                                    0.5867 * pImageData^[k+1+NoCh] + // G
                                                    0.1144 * pImageData^[k+NoCh]);   // R
                             pixu := pixu * (1.0 - ddy) + pixl * ddy;
                             FScanData^[i] := Round(pixu);

                             sx := sx + Fdsx;
                             sy := sy + Fdsy;
                          end;
                     end
                     else begin
                          dx := FSWCoorAx^[0] - FSWCoorBx^[0];
                          dy := FSWCoorAy^[0] - FSWCoorBy^[0];
                          dsx := dx / FScanWidth;
                          dsy := dy / FScanWidth;
                          for i := 0 to (FScanLength - 1)
                          do begin
                             Sum := 0;
                             sx := FSWCoorAx^[i];
                             sy := FSWCoorAy^[i];
                             for j := 0 to (FScanWidth - 1)
                             do begin
                                x := Trunc(sx);
                                y := Trunc(sy);
                                ddx := sx - x;
                                ddy := sy - y;
                                k := NoCh * x + (iHeight - y) * iLongWidth;

                                pixu := (1.0 - ddx) * (0.2989 * pImageData^[k+2] + // B
                                                       0.5867 * pImageData^[k+1] + // G
                                                       0.1144 * pImageData^[k]) +  // R
                                        ddx         * (0.2989 * pImageData^[k+2+NoCh] + // B
                                                       0.5867 * pImageData^[k+1+NoCh] + // G
                                                       0.1144 * pImageData^[k+NoCh]);   // R
                                dec(k, iLongWidth);
                                pixl := (1.0 - ddx) * (0.2989 * pImageData^[k+2] + // B
                                                       0.5867 * pImageData^[k+1] + // G
                                                       0.1144 * pImageData^[k]) +  // R
                                        ddx         * (0.2989 * pImageData^[k+2+NoCh] + // B
                                                       0.5867 * pImageData^[k+1+NoCh] + // G
                                                       0.1144 * pImageData^[k+NoCh]);   // R
                                pixu := pixu * (1.0 - ddy) + pixl * ddy;
                                Sum := Sum + Round(pixu);

                                sx := sx + dsx;
                                sy := sy + dsy;
                             end;
                             FScanData^[i] := Sum div FScanWidth;
                          end;
                     end;
       end;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmCustomProfile.ScanRGB.


function TmcmCustomProfile.Scan : TmcmErrorCode;
var iLongWidth : cardinal;
    iHeight    : cardinal;
    pImageData : PVectorB;
    i          : integer;
    OldLength  : integer;
begin
  FError := EC_OK;
  try
    if (FImage <> Nil)
    then begin
         if Not(FImage.Empty)
         then begin
              OldLength   := FScanLength;
              FScanLength := Round(sqrt(sqr(Fdx) + sqr(Fdy)) + 0.49999) + 1;
              if (FScanLength > FAverage)
              then begin
                   if (OldLength <> FScanLength)
                   then begin
                        if Assigned(FScanData)
                        then FreeMem(FScanData);
                        GetMem(FScanData, FScanLength * SizeOf(integer));
                   end;
                   iLongWidth := FImage.LongLineWidth;
                   iHeight    := FImage.Height;
                   pImageData := FImage.pDib;

                   Fdsx := Fdx / FScanLength;
                   Fdsy := Fdy / FScanLength;

                   // Get parent scroll position.
                   Fox := FStartPos.x + ControlXOffset;
                   Foy := FStartPos.y + ControlYOffset;

                   // Determind start and end scan index.
                   if (0 > FStartPos.x) or (FStartPos.x >= FMaxX) or
                      (0 > FEndPos.x) or (FEndPos.x >= FMaxX) or
                      (0 > FStartPos.y) or (FStartPos.y >= FMaxY) or
                      (0 > FEndPos.y) or (FEndPos.y >= FMaxY)
                   then begin
                        FillChar(FScanData^, FScanLength * SizeOf(integer), 0);
                        FError := EC_CHECKXY;
                   end;

                   if (FScanWidth <> 1)
                   then CalcPerpendicular;

                   if (FError = EC_OK)
                   then begin
                        case FImage.ImageFormat of
                        IF_BW     : ScanBW(iLongWidth, iHeight, pImageData);
                        IF_GREY8  : begin
                                      ScanGrey(iLongWidth, iHeight, pImageData);
                                      i := 255;
                                      if (TmcmCastImage(FImage).DibInfo.bmiColors[0].rgbRed >
                                          TmcmCastImage(FImage).DibInfo.bmiColors[i].rgbRed)
                                      then Invert;
                                    end;
                        IF_PAL8   : ScanPal(iLongWidth, iHeight, pImageData, TmcmCastImage(FImage).DibInfo.bmiColors[0]);
                        IF_RGB24  : ScanRGB(iLongWidth, iHeight, pImageData, 3);
                        IF_RGBA32 : ScanRGB(iLongWidth, iHeight, pImageData, 4);
                        else FError := EC_BADCOLORFORMAT;
                        end;
                   end;
                   {
                   if (Fsi <> 0)
                   then for i := 0 to (Fsi - 1)
                        do FScanData^[i] := FScanData^[Fsi];
                   if (Fei <> FScanLength - 1)
                   then for i := (Fei + 1) to (FScanLength - 1)
                        do FScanData^[i] := FScanData^[Fei];
                   }
                   if (FSWCoorAx <> Nil)
                   then begin
                        FreeMem(FSWCoorAx);
                        FSWCoorAx := Nil;
                   end;
                   if (FSWCoorAy <> Nil)
                   then begin
                        FreeMem(FSWCoorAy);
                        FSWCoorAy := Nil;
                   end;
                   if (FSWCoorBx <> Nil)
                   then begin
                        FreeMem(FSWCoorBx);
                        FSWCoorBx := Nil;
                   end;
                   if (FSWCoorBy <> Nil)
                   then begin
                        FreeMem(FSWCoorBy);
                        FSWCoorBy := Nil;
                   end;

                   if (FError = EC_OK) and (FAverage > 1)
                   then AverageScan;

                   if (FError <> EC_OK)
                   then begin
                        if Assigned(FScanData)
                        then FreeMem(FScanData);
                        FScanData := Nil;
                        FScanLength := 0;
                   end
                   else FDoRescan := False;
              end
              else FError := EC_INVALIDSCANPARAM;
         end
         else FError := EC_MISSOURCEIMAGE;
    end
    else FError := EC_MISSOURCEIMAGE;
  except
    On E:EOutOfMemory
    do FError := EC_NOMEMORY;
  end;
  Result := FError;
end; // TmcmCustomProfile.Scan.


function TmcmCustomProfile.GetScanData(Index : integer) : integer;
begin
  if (0 <= Index) and (Index < FScanLength) and (FScanData <> Nil)
  then Result := FScanData^[Index]
  else Result := 0;
end; // TmcmCustomProfile.GetScanData.


procedure TmcmCustomProfile.Invert;
var i : integer;
begin
  if (FScanData <> Nil)
  then begin
       for i := 0 to (FScanLength - 1)
       do FScanData^[i] := FMaxValue - FScanData^[i];
  end;
end; // TmcmCustomProfile.Invert.


procedure TmcmCustomProfile.Stretch;
var i        : integer;
    Max, Min : integer;
begin
  if (FScanData <> Nil)
  then begin
       Min := FMaxValue;
       Max := 0;
       for i := 0 to (FScanLength - 1)
       do begin
          if (Max < FScanData^[i])
          then Max := FScanData^[i];
          if (Min > FScanData^[i])
          then Min := FScanData^[i];
       end;
       if (Max <> Min)
       then for i := 0 to (FScanLength - 1)
            do FScanData^[i] := Round((FMaxValue / (Max - Min)) * (FScanData^[i] - Min));
  end;
end; // TmcmCustomProfile.Stretch.


//------------------------------------------------------------------------------
// TmcmProfile
//------------------------------------------------------------------------------

constructor TmcmProfile.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FDerivative     := Nil;
  FPoints         := TList.Create;
  FHysteresis     := 25;
  FMinDerivative  := 10;
  FTransitions    := [];
  FCenterPeaks    := False;
  FMarkerPen := TPen.Create;
  FMarkerPen.OnChange := StyleChanged;
  FCursorOnLine := crHandPoint;
  FCursorOnNode := crCross;
end; // TmcmProfile.Create.


destructor TmcmProfile.Destroy;
begin
  if Assigned(FDerivative)
  then FreeMem(FDerivative);
  FDerivative := Nil;
  ClearPoints;
  if Assigned(FPoints)
  then FPoints.Free;
  FPoints := Nil;
  if Assigned(FMarkerPen)
  then FMarkerPen.Free;
  FMarkerPen := Nil;
  Inherited Destroy;
end; // TmcmProfile.Destroy.


procedure TmcmProfile.Clear;
begin
  ClearPoints;
  Inherited Clear;
end; // TmcmProfile.Clear.


procedure TmcmProfile.ClearPoints;
var i : integer;
begin
  if Assigned(FPoints)
  then begin
       for i := (FPoints.Count - 1) downto 0
       do begin
          if (FPoints.Items[i] <> Nil)
          then FreeMem(PmcmTransitPoint(FPoints.Items[i]));
          FPoints.Items[i] := Nil;
       end;
       FPoints.Clear;
  end;
end; // TmcmProfile.ClearPoints.


procedure TmcmProfile.SetMarkerPen(Value : TPen);
begin
  FMarkerPen.Assign(Value);
end; // TmcmProfile.SetMarkerPen.


procedure TmcmProfile.InvalidateNodes;
var UpdateRect : TRect;
begin
  if (Parent <> Nil)
  then if (Parent is TWinControl)
       then begin
            UpdateRect.Left   := Round(FScale * FStartPos.x) - FNodeSize;
            UpdateRect.Top    := Round(FScale * FStartPos.y) - FNodeSize;
            UpdateRect.Right  := Round(FScale * FStartPos.x) + FNodeSize;
            UpdateRect.Bottom := Round(FScale * FStartPos.y) + FNodeSize;
            InvalidateRect(TWinControl(Parent).Handle, @UpdateRect, False);
            UpdateRect.Left   := Round(FScale * FEndPos.x) - FNodeSize;
            UpdateRect.Top    := Round(FScale * FEndPos.y) - FNodeSize;
            UpdateRect.Right  := Round(FScale * FEndPos.x) + FNodeSize;
            UpdateRect.Bottom := Round(FScale * FEndPos.y) + FNodeSize;
            InvalidateRect(TWinControl(Parent).Handle, @UpdateRect, False);
       end;
end; // TmcmProfile.InvalidateNodes.


procedure TmcmProfile.InvalidateProfile;
var UpdateRect : TRect;
begin
  if (Parent <> Nil)
  then if (Parent is TWinControl)
       then begin
            if (FStartPos.x < FEndPos.x)
            then UpdateRect.Left := Round(FScale * FStartPos.x) - (FNodeSize + 5)
            else UpdateRect.Left := Round(FScale * FEndPos.x) - FNodeSize;

            if (FStartPos.y < FEndPos.y)
            then UpdateRect.Top := Round(FScale * FStartPos.y) - (FNodeSize + 5)
            else UpdateRect.Top := Round(FScale * FEndPos.y) - (FNodeSize + 5);

            if (FStartPos.x > FEndPos.x)
            then UpdateRect.Right := Round(FScale * FStartPos.x) + (FNodeSize + 5)
            else UpdateRect.Right := Round(FScale * FEndPos.x) + (FNodeSize + 5);

            if (FStartPos.y > FEndPos.y)
            then UpdateRect.Bottom := Round(FScale * FStartPos.y) + (FNodeSize + 5)
            else UpdateRect.Bottom := Round(FScale * FEndPos.y) + (FNodeSize + 5);

            InvalidateRect(TWinControl(Parent).Handle, @UpdateRect, False);
       end;
end; // TmcmProfile.InvalidateProfile.


procedure TmcmProfile.CMHitTest(var Message : TCMHitTest);
var x, y        : integer;
    sd, ed, ns  : integer;
    OldHitIndex : integer;
    a, b, c     : single;
begin
  if (csDesigning in ComponentState) or FLBtnDown
  then Message.Result := HTCLIENT
  else begin
       x := Round(Message.XPos / FScale);
       y := Round(Message.YPos / FScale);

       // Perform a hit test
       OldHitIndex := FHitIndex;
       FHitIndex := -1;

       // First check if cursor is near the end-points.
       sd := sqr(FStartPos.x - x) + sqr(FStartPos.y - y);
       ed := sqr(FEndPos.x - x) + sqr(FEndPos.y - y);
       if (FNodeSize > FHitDist)
       then ns := sqr(FNodeSize)
       else ns := sqr(FHitDist);
       if (sd < ns) or (ed < ns)
       then begin
            if (sd < ns)
            then FHitIndex := 0;
            if (ed < ns)
            then FHitIndex := 1;
       end
       else begin
            // Check if cursor is near profile line.
            if ((FStartPos.x - FHitDist <= x) and (x <= FEndPos.x + FHitDist)) or
               ((FEndPos.x - FHitDist <= x) and (x <= FStartPos.x + FHitDist))
            then begin
                 if ((FStartPos.y - FHitDist <= y) and (y <= FEndPos.y + FHitDist)) or
                    ((FEndPos.y - FHitDist <= y) and (y <= FStartPos.y + FHitDist))
                 then begin
                      if (Fdx <> 0) and (Fdy <> 0)
                      then begin
                           a := Fdy / Fdx;
                           b := a * FStartPos.x - FStartPos.y;

                           if (abs(Fdx) > abs(Fdy))
                           then c := (abs(a * x - b) - y)
                           else c := abs((y + b) / a) - x;

                           if (-FHitDist < c) and (c < FHitDist)
                           then FHitIndex := 2;
                      end
                      else begin
                           if (Fdx = 0) and (((FStartPos.x - FHitDist) <= x) and
                              (x <= (FStartPos.x + FHitDist)))
                           then FHitIndex := 2;
                           if (Fdy = 0) and (((FStartPos.y - FHitDist) <= y) and
                              (y <= (FStartPos.y + FHitDist)))
                           then FHitIndex := 2;
                      end;
                 end;
            end;
       end;

       if (OldHitIndex <> FHitIndex)
       then begin
            case FHitIndex of
            0,
            1 : Cursor := FCursorOnNode;
            2 : Cursor := FCursorOnLine;
            else Cursor := crDefault;
            end;
            if (Parent <> Nil)
            then Invalidate;
       end;

       if (FHitIndex >= 0) or (OldHitIndex <> FHitIndex)
       then begin
            FDoShowEndPts := FShowEndPoints;
            Message.Result := HTCLIENT;
       end
       else begin
            FDoShowEndPts := False;
            Message.Result := 0;
       end;
  end;
end; // TmcmProfile.CMHitTest.


procedure TmcmProfile.CMMouseLeave(var Message : TMessage);
begin
  if (FHitIndex >= 0) and Not(FLBtnDown)
  then begin
       FHitIndex := -1;
       if (Parent <> Nil)
       then Invalidate;
  end;
  if (Parent <> Nil)
  then Parent.Perform(CM_MOUSELEAVE, 0, Longint(Self));
end; // TmcmProfile.CMMouseLeave.


procedure TmcmProfile.WMLButtonDown(var Msg : TMessage);
begin
  Inherited ;
  if Not(FLBtnDown)
  then begin
       if (FHitIndex >= 0)
       then begin
            BringToFront;
            FLBtnDown := True;
       end;
       case FHitIndex of
       0 : begin
           end;
       1 : begin
           end;
       2 : begin
             Fmx := Round(LoWord(Msg.LParam) / FScale);
             Fmy := Round(HiWord(Msg.LParam) / FScale);
           end;
       end;
  end;
  Msg.Result := 0;
end; // TmcmProfile.WMLButtonDown.


procedure TmcmProfile.WMLButtonUp(var Msg : TMessage);
begin
  Inherited ;
  if (FLBtnDown)
  then begin
       Changed(Self);
       FHitIndex := -1;
       FLBtnDown := False;
       if (Parent <> Nil)
       then Invalidate;
  end;
  Msg.Result := 0;
end; // TmcmProfile.WMLButtonUp.


procedure TmcmProfile.WmMouseMove(var Msg : TMessage);
var x, y : integer;
    w, h : integer;
begin
  x := Round(smallint(LoWord(Msg.LParam)) / FScale);
  y := Round(smallint(HiWord(Msg.LParam)) / FScale);

  if FLBtnDown
  then begin
       x := CheckX(x);
       y := CheckY(y);
       if (FHitIndex >= 0)
       then begin
            if (Parent <> Nil)
            then InvalidateProfile;
       end;
       case FHitIndex of
       0 : begin // Start
             if (FStartPos.x = x) and (FStartPos.y = y)
             then Exit;
             FStartPos.x := x;
             FStartPos.y := y;
           end;
       1 : begin // End
             if (FEndPos.x = x) and (FEndPos.y = y)
             then Exit;
             FEndPos.x := x;
             FEndPos.y := y;
           end;
       2 : begin // Move
             if (Fmx > (FStartPos.x + x))
             then Fmx := FStartPos.x + x;
             if (Fmy > (FStartPos.y + y))
             then Fmy := FStartPos.y + y;
             if (Fmx > (FEndPos.x + x))
             then Fmx := FEndPos.x + x;
             if (Fmy > (FEndPos.y + y))
             then Fmy := FEndPos.y + y;

             if Assigned(FImage)
             then begin
             w := Round(Width / FScale) - 1;
             h := Round(Height / FScale) - 1;
                  if (Fmx < (FStartPos.x + x) - w)
                  then Fmx := FStartPos.x + x - w;
                  if (Fmy < (FStartPos.y + y) - h)
                  then Fmy := FStartPos.y + y - h;
                  if (Fmx < (FEndPos.x + x) - w)
                  then Fmx := FEndPos.x + x - w;
                  if (Fmy < (FEndPos.y + y) - h)
                  then Fmy := FEndPos.y + y - h;
             end;

             if (Fmx = x) and (Fmy = y)
             then Exit;
             FStartPos.x := FStartPos.x + (x - Fmx);
             FStartPos.y := FStartPos.y + (y - Fmy);
             FEndPos.x := FEndPos.x + (x - Fmx);
             FEndPos.y := FEndPos.y + (y - Fmy);
             Fmx := x;
             Fmy := y;
           end;
       end;

       if (FHitIndex >= 0)
       then begin
            CalcAngle;
            if (Parent <> Nil)
            then Invalidate;
       end;
  end;

  x := LoWord(Msg.LParam);
  y := HiWord(Msg.LParam);
  Msg.LParam := x + Left + ((y + Top) shl 16);

  Inherited ;
  Msg.Result := 0;
end; // TmcmProfile.WmMouseMove.


procedure TmcmProfile.Paint;
var x0, y0 : integer;
    x1, y1 : integer;
    i, d   : integer;
    atan   : single;
    px, py : single;
    bx, by : single;
    dx, dy : single; // Zoom offset.
    sx, sy : single;
begin
  if Visible or (csDesigning in ComponentState)
  then begin
       Canvas.Pen.Assign(FPen);
       Canvas.Brush.Assign(FBrush);
       with Canvas
       do begin
          x0 := Round(FScale * FStartPos.x);
          y0 := Round(FScale * FStartPos.y);
          x1 := Round(FScale * FEndPos.x);
          y1 := Round(FScale * FEndPos.y);
          MoveTo(x0, y0);
          LineTo(x1, y1);

          if (FDoShowEndPts and (FHitIndex >= 0)) or
             (csDesigning in ComponentState)
          then begin
               Rectangle(x0 - FNodeSize + 1, y0 - FNodeSize + 1, x0 + FNodeSize, y0 + FNodeSize);
               Rectangle(x1 - FNodeSize + 1, y1 - FNodeSize + 1, x1 + FNodeSize, y1 + FNodeSize);
          end;

          if Assigned(FPoints)
          then begin
               if (FPoints.Count > 0) and Not(FLBtnDown)
               then begin
                   if (FScanWidth < 10)
                   then d := Round(FScale * 5)
                   else d := Round(FScale * FScanWidth / 2.0);

                   if (Fdx <> 0.0)
                   then begin
                        if (Fdy <> 0.0)
                        then begin
                             atan := ArcTan(Fdy / Fdx);
                             dx := FScale * cos(atan) / 2.0;
                             dy := FScale * sin(atan) / 2.0;

                             atan := -ArcTan(-Fdx / Fdy);
                             px := d * cos(atan);
                             py := d * sin(atan);
                             atan := ArcTan(Fdy / Fdx);
                             if (Fdx < 0)
                             then begin
                                  bx := -FScale * 3 * cos(atan);
                                  by := -FScale * 3 * sin(atan);
                             end
                             else begin
                                  bx := FScale * 3 * cos(atan);
                                  by := FScale * 3 * sin(atan);
                             end;
                        end
                        else begin
                             dy := 0.0;
                             px := 0.0;
                             py := Round(d);

                             if (Fdx < 0.0)
                             then begin
                                  dx := -FScale / 2.0;
                                  bx := -FScale * 3;
                             end
                             else begin
                                  dx := FScale / 2.0;
                                  bx := FScale * 3;
                             end;
                             by := 0;
                        end;
                   end
                   else begin
                        dx := 0.0;
                        px := Round(d);
                        py := 0.0;
                        bx := 0.0;
                        if (Fdy < 0.0)
                        then begin
                             dy := -FScale / 2.0;
                             by := -FScale * 3;
                        end
                        else begin
                             dy := FScale / 2.0;
                             by := FScale * 3;
                        end;
                   end;

                   Canvas.Pen.Assign(FMarkerPen);
                   for i := 0 to (FPoints.Count - 1)
                   do begin
                      if Assigned(FPoints.Items[i])
                      then begin
                           sx := FScale * PmcmTransitPoint(FPoints.Items[i])^.x;
                           sy := FScale * PmcmTransitPoint(FPoints.Items[i])^.y;
                           if (FScale > 1.0)
                           then begin
                                sx := sx + dx;
                                sy := sy + dy;
                           end;

                           case FDispMarker[integer(PmcmTransitPoint(FPoints.Items[i])^.Transit)] of
                           PDM_CROSS        : begin
                                                d := Round(FScale * 5);
                                                MoveTo(Round(sx-d), Round(sy-d));
                                                LineTo(Round(sx+d), Round(sy+d));
                                                MoveTo(Round(sx+d), Round(sy-d));
                                                LineTo(Round(sx-d), Round(sy+d));
                                              end;
                           PDM_LINE         : begin
                                                MoveTo(Round(sx-px), Round(sy+py));
                                                LineTo(Round(sx+px), Round(sy-py));
                                              end;
                           PDM_BRACKETBEGIN : begin
                                                MoveTo(Round(sx-px+bx), Round(sy+py+by));
                                                LineTo(Round(sx-px),    Round(sy+py));
                                                LineTo(Round(sx+px),    Round(sy-py));
                                                LineTo(Round(sx+px+bx), Round(sy-py+by));
                                              end;
                           PDM_BRACKETEND   : begin
                                                MoveTo(Round(sx-px-bx), Round(sy+py-by));
                                                LineTo(Round(sx-px),    Round(sy+py));
                                                LineTo(Round(sx+px),    Round(sy-py));
                                                LineTo(Round(sx+px-bx), Round(sy-py-by));
                                              end;
                           (*
                           PDM_ARROWSTART   : begin
                                                Canvas.Brush.Color := FMarkerPen.Color;
                                                Poly[0].x := Round(sx);
                                                Poly[0].y := Round(sy);

                                                Poly[1].x := Round(sx-5);
                                                Poly[1].y := Round(sy-5);
                                                Poly[2].x := Round(sx+5);
                                                Poly[2].y := Round(sy-5);
                                                Polygon(Poly);
                                              end;
                           PDM_ARROWEND     : begin
                                                Canvas.Brush.Color := FMarkerPen.Color;
                                                Poly[0].x := Round(sx);
                                                Poly[0].y := Round(sy);
                                                Poly[1].x := Round(sx-px);
                                                Poly[1].y := Round(sy+py);
                                                Poly[2].x := Round(sx+px);
                                                Poly[2].y := Round(sy+py);
                                                Polygon(Poly);
                                              end;
                           *)
                           end;
                      end;
                   end;
               end;
          end;
       end;
  end
  else Inherited Paint;
end; // TmcmProfile.Paint.


procedure TmcmProfile.SetHysteresis(Value : integer);
begin
  if (FHysteresis <> Value)
  then begin
       FHysteresis := Value;
       Changed(Self);
  end;
end; // TmcmProfile.SetHysteresis.


procedure TmcmProfile.SetMinDerivative(Value : integer);
begin
  if (FMinDerivative <> Value)
  then begin
       FMinDerivative := Value;
       Changed(Self);
  end;
end; // TmcmProfile.SetMinDerivative


procedure TmcmProfile.SetTransitions(Value : TmcmProfTransitions);
begin
  if (FTransitions <> Value)
  then begin
       FTransitions := Value;
       Changed(Self);
  end;
end; // TmcmProfile.SetTransitions.


procedure TmcmProfile.SetCenterPeaks(Value : boolean);
begin
  if (FCenterPeaks <> Value)
  then begin
       FCenterPeaks := Value;
       Changed(Self);
  end;
end; // TmcmProfile.SetCenterPeaks.


function TmcmProfile.GetDerivative(Index : integer) : integer;
begin
  if (0 <= Index) and (Index < FScanLength) and (FDerivative <> Nil)
  then Result := FDerivative^[Index]
  else Result := 0;
end; // TmcmProfile.GetDerivative.


function TmcmProfile.Scan : TmcmErrorCode;
begin
  Inherited Scan;
  {
  if (FError = EC_OK)
  then LocateTransitions;
  }
  Result := FError;
end; // TmcmProfile.Scan.


function TmcmProfile.LocateTransitions : TmcmErrorCode;
var i, j, k, l : integer;
    maxv, minv : integer;
    maxd       : integer;
    p          : PmcmTransitPoint;
    dsx, dsy   : single;
    ox, oy     : double;
    bPositive  : boolean;
    bNegative  : boolean;
begin
  FError := EC_OK;
  try
    if (FScanData <> Nil) and (FTransitions <> [])
    then begin
         ClearPoints;

         if Assigned(FDerivative)
         then FreeMem(FDerivative);
         FDerivative := Nil;

         // Calculate 1st. derivative.
         if (FDerivative = Nil)
         then GetMem(FDerivative, (FScanLength + 1) * SizeOf(integer));
         for i := 1 to (FScanLength - 2)
         do FDerivative^[i] := (FScanData^[i+1] - FScanData^[i-1]) div 2;
         i := 1;
         FDerivative^[0] := FDerivative^[i];
         i := FScanLength - 1;
         FDerivative^[i] := FDerivative^[i-1];
         FDerivative^[i+1] := 0;

         // Initial coordinates
         dsx := Fdx / FScanLength;
         dsy := Fdy / FScanLength;
         ox := FStartPos.x;
         oy := FStartPos.y;

         bPositive  := (([PT_POSITIVE,PT_PEAK,PT_VALLEY] * FTransitions) <> []);
         bNegative  := (([PT_NEGATIVE,PT_PEAK,PT_VALLEY] * FTransitions) <> []);

         i := 0;
         while (i < FScanLength)
         do begin
            // Search for Negative transitions, White to Black.
            if bNegative and (FDerivative^[i] <= -FMinDerivative)
            then begin
                 maxd := FDerivative^[i];
                 minv := FScanData^[i];
                 maxv := minv;
                 // Backwards search for the locally highest intensity value.
                 j := i - 1;
                 while (j >= 0)
                 do begin
                    if (maxv < FScanData^[j])
                    then maxv := FScanData^[j]
                    else Break;
                    dec(j);
                 end;

                 // Forward search for highest -gradient and lowest intensity value.
                 j := i;
                 while (i < FScanLength) and (FDerivative^[i] < 0)
                 do begin
                    if (maxd > FDerivative^[i])
                    then begin
                         maxd := FDerivative^[i];
                         j := i;
                    end;
                    if (minv > FScanData^[i])
                    then minv := FScanData^[i];
                    inc(i);
                 end;


                 if ((maxv - minv) >= FHysteresis)
                 then begin
                      // Fine tune position of transition.
                      k := j;
                      while (FDerivative^[j] = FDerivative^[k+1])
                      do inc(k);
                      if (j <> k)
                      then j := Round((k - j) / 2.0) + j;

                      GetMem(P, SizeOf(TmcmTransitPoint));
                      P^.Index   := j;
                      P^.x       := ox + j * dsx;
                      P^.y       := oy + j * dsy;
                      P^.Area    := 0;
                      P^.Transit := PT_NEGATIVE;
                      FPoints.Add(p);
                 end;
            end;

            // Search for positive transitions, Black to White.
            if bPositive and (FDerivative^[i] >= FMinDerivative)
            then begin
                 maxd := FDerivative^[i];
                 minv := FScanData^[i];
                 maxv := minv;
                 // Backwards search for the locally lowest intensity value.
                 j := i - 1;
                 while (j >= 0)
                 do begin
                    if (minv > FScanData^[j])
                    then minv := FScanData^[j]
                    else Break;
                    dec(j);
                 end;

                 // Forward search for highest +gradient and lowest intensity value.
                 j := i;
                 while (i < FScanLength) and (FDerivative^[i] > 0)
                 do begin
                    if (maxd < FDerivative^[i])
                    then begin
                         maxd := FDerivative^[i];
                         j := i;
                    end;
                    if (maxv < FScanData^[i])
                    then maxv := FScanData^[i];
                    inc(i);
                 end;

                 if ((maxv - minv) >= FHysteresis)
                 then begin
                      // Fine tune position of transition.
                      k := j;
                      while (FDerivative^[j] = FDerivative^[k+1])
                      do inc(k);
                      if (j <> k)
                      then j := Round((k - j) / 2.0) + j;

                      GetMem(P, SizeOf(TmcmTransitPoint));
                      P^.Index   := j;
                      P^.x       := ox + j * dsx;
                      P^.y       := oy + j * dsy;
                      P^.Area    := 0;
                      P^.Transit := PT_POSITIVE;
                      FPoints.Add(p);
                 end;
            end;
            inc(i);
         end;

         if (([PT_PEAK,PT_VALLEY] * FTransitions) <> [])
         then begin
              i := 0;
              while (i < (FPoints.Count - 1))
              do begin
                 case PmcmTransitPoint(FPoints.Items[i])^.Transit of
                 PT_POSITIVE : if (PmcmTransitPoint(FPoints.Items[i+1])^.Transit = PT_NEGATIVE) and
                                  (PT_PEAK in FTransitions)
                               then begin
                                    if FCenterPeaks
                                    then j := PmcmTransitPoint(FPoints.Items[i])^.Index +
                                             (PmcmTransitPoint(FPoints.Items[i+1])^.Index -
                                              PmcmTransitPoint(FPoints.Items[i])^.Index) div 2
                                    else begin
                                         j := PmcmTransitPoint(FPoints.Items[i])^.Index;
                                         l := j + 1;
                                         k := PmcmTransitPoint(FPoints.Items[i+1])^.Index;
                                         while (l < k)
                                         do begin
                                            if (FScanData^[j] < FScanData^[l])
                                            then j := l;
                                            inc(l);
                                         end;
                                    end;
                                    GetMem(P, SizeOf(TmcmTransitPoint));
                                    P^.Index   := j;
                                    P^.x       := ox + j * dsx;
                                    P^.y       := oy + j * dsy;
                                    P^.Area    := 0;
                                    P^.Transit := PT_PEAK;
                                    FPoints.Insert(i+1, p);
                               end;
                 PT_NEGATIVE : if (PmcmTransitPoint(FPoints.Items[i+1])^.Transit = PT_POSITIVE) and
                                  (PT_VALLEY in FTransitions)
                               then begin
                                    if FCenterPeaks
                                    then j := PmcmTransitPoint(FPoints.Items[i])^.Index +
                                             (PmcmTransitPoint(FPoints.Items[i+1])^.Index -
                                              PmcmTransitPoint(FPoints.Items[i])^.Index) div 2
                                    else begin
                                         j := PmcmTransitPoint(FPoints.Items[i])^.Index;
                                         l := j + 1;
                                         k := PmcmTransitPoint(FPoints.Items[i+1])^.Index;
                                         while (l < k)
                                         do begin
                                            if (FScanData^[j] > FScanData^[l])
                                            then j := l;
                                            inc(l);
                                         end;
                                    end;
                                    GetMem(P, SizeOf(TmcmTransitPoint));
                                    P^.Index   := j;
                                    P^.x       := ox + j * dsx;
                                    P^.y       := oy + j * dsy;
                                    P^.Area    := 0;
                                    P^.Transit := PT_VALLEY;
                                    FPoints.Insert(i+1, p);
                               end;
                 end;
                 inc(i);
              end;

              for i := (FPoints.Count - 1) downto 0
              do begin
                 if Not(PmcmTransitPoint(FPoints.Items[i])^.Transit in FTransitions)
                 then begin
                      FreeMem(PmcmTransitPoint(FPoints.Items[i]));
                      FPoints.Items[i] := Nil;
                      FPoints.Delete(i);
                 end;
              end;
         end;
    end
    else begin
         if (FPoints.Count > 0)
         then ClearPoints;
    end;
  except
    On E:EOutOfMemory
    do FError := EC_NOMEMORY;
  end;
  Result := FError;
end; // TmcmProfile.LocateTransitions.


function TmcmProfile.GetNoTransitions : integer;
begin
  if Assigned(FPoints)
  then Result := FPoints.Count
  else Result := 0;
end; // TmcmProfile.GetNoTransitions.


function TmcmProfile.GetTransitionIndex(Index : integer) : integer;
begin
  Result := -1;
  if Assigned(FPoints)
  then begin
       if (0 <= Index) and (Index < FPoints.Count)
       then Result := PmcmTransitPoint(FPoints.Items[Index]).Index;
  end;
end; // TmcmProfile.GetTransitionIndex.


function TmcmProfile.GetTransitionType(Index : integer) : TmcmProfTransition;
begin
  Result := PT_NONE;
  if Assigned(FPoints)
  then begin
       if (0 <= Index) and (Index < FPoints.Count)
       then Result := PmcmTransitPoint(FPoints.Items[Index]).Transit;
  end;
end; // TmcmProfile.GetTransitionType.


function TmcmProfile.GetTransitionX(Index : integer) : double;
begin
  Result := -1.0;
  if Assigned(FPoints)
  then begin
       if (0 <= Index) and (Index < FPoints.Count)
       then Result := PmcmTransitPoint(FPoints.Items[Index]).x + ControlXOffset;
  end;
end; // TmcmProfile.GetTransitionX.


function TmcmProfile.GetTransitionY(Index : integer) : double;
begin
  Result := -1.0;
  if Assigned(FPoints)
  then begin
       if (0 <= Index) and (Index < FPoints.Count)
       then Result := PmcmTransitPoint(FPoints.Items[Index]).y + ControlYOffset;
  end;
end; // TmcmProfile.GetTransitionY.


//------------------------------------------------------------------------------
// TmcmCustomPolygon.
//------------------------------------------------------------------------------

constructor TmcmCustomPolygon.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FOnChange := Nil;

  FCount  := 0;
  FMemSize := 0;
  FMemInc := 1024;
  FTrace  := Nil;
  FDrawTrace := Nil;
  FBoundRect := Rect(0, 0, 0, 0);
  //FTraceType := TCT_8CONNECT;

  FxRes := 1.0;
  FyRes := 1.0;

  FConvexObj  := Nil;
  ClearCalculations;

  FIndex := 0;

  FPen.Color := RGB(255,0,0);
  FPen.Width := 1;
  FPen.Style := psSolid;
  FBrush.Color := RGB(0, 0, 0);
  FBrush.Style := bsClear;
end; // TmcmCustomPolygon.Create.


destructor TmcmCustomPolygon.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmCustomPolygon.Destroy.


procedure TmcmCustomPolygon.Changed(Sender : TObject);
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
end; // TmcmCustomPolygon.Changed.


procedure TmcmCustomPolygon.Clear;
begin
  FCount := 0;
  FMemInc := 1024;
  FMemSize := 0;
  try
    if Assigned(FDrawTrace)
    then FreeMem(FDrawTrace);
  except
  end;
  FDrawTrace  := Nil;
  try
    if Assigned(FTrace)
    then FreeMem(FTrace);
  except
  end;
  FTrace  := Nil;

  FImage := Nil;
  FxRes := 1.0;
  FyRes := 1.0;
  ClearCalculations;
end; // TmcmCustomPolygon.Clear.


procedure TmcmCustomPolygon.ClearCalculations;
begin
  FBoundingCalced  := False;
  FGeometricCalced := False;
  FDensCalced      := False;
  FOrientCalced    := False;
  FLengthCalced    := False;
  FCGCalced        := False;
  FMaxMinRadius    := False;
  FMaxFeretCalced  := False;
  FMinCircleCalced := False;
  if Assigned(FConvexObj)
  then FConvexObj.Free;
  FConvexObj  := Nil;
end; // TmcmCustomPolygon.ClearCalculations.


procedure TmcmCustomPolygon.Add(x, y : integer);
begin
  if (FCount >= FMemSize)
  then ResizeData(-1);
  if (FCount < FMemSize)
  then begin
       FTrace[FCount].x := x;
       FTrace[FCount].y := y;
       inc(FCount);
  end;
end; // TmcmCustomPolygon.Add.


function TmcmCustomPolygon.Count : longint;
begin
  // Returns the number of points in the polygon FTrace.
  Result := FCount;
end; // TmcmCustomPolygon.Count.


procedure TmcmCustomPolygon.SwapTraceData;
var i, j     : integer;
    TempData : PVectorPt;
begin
  // Exchanged the sequence of data points (x,y) in the polygon FTrace.
  if (FCount > 1)
  then begin
       TempData := FTrace;
       try
         try
           GetMem(FTrace, FMemSize * SizeOf(TPoint));
           j := FCount - 1;
           for i := 0 to (FCount - 1)
           do FTrace[i] := TempData[j-i];
         except
           On E:EOutOfMemory
           do Clear;
         end;
       finally
         FreeMem(TempData);
       end;
  end;
end; // TmcmCustomPolygon.SwapTraceData.


function TmcmCustomPolygon.IsClosed : boolean;
begin
  // Returns True if the Start and End point in the polygon are "equal",
  // otherwise False.
  if (FTrace <> Nil) and (FCount > 1)
  then begin
       FIsClosed := (Abs(FTrace[0].x - FTrace[Count-1].x) <= 1) and
                    (Abs(FTrace[0].y - FTrace[Count-1].y) <= 1);
  end
  else FIsClosed := False;
  Result := FIsClosed;
end; // TmcmCustomPolygon.IsClosed.


function TmcmCustomPolygon.ResizeData(NewSize : longint) : longint;
begin
  // Change the polygon data arrays size to NewSize.
  // If NewSize is "0" the array is doubled in size.
  if (NewSize >= 0)
  then FMemSize := NewSize
  else inc(FMemSize, FMemInc);
  try
    ReallocMem(FTrace, FMemSize * SizeOf(TPoint));
    FMemInc := FMemSize;
  except
    On E:EOutOfMemory
    do Clear;
  end;
  Result := FMemSize;
end; // TmcmCustomPolygon.Resize.


function TmcmCustomPolygon.GetBoundingRect : TRect;
var i : integer;
begin
  // Returns the rectangle enclosing the polygon data.
  if Not(FBoundingCalced)
  then begin
       FBoundRect := Rect($7FFFFFFF, $7FFFFFFF, -1, -1);
       for i := 0 to (FCount - 1)
       do begin
          if (FBoundRect.Left > FTrace[i].x)
          then FBoundRect.Left := FTrace[i].x;
          if (FBoundRect.Top > FTrace[i].y)
          then FBoundRect.Top := FTrace[i].y;
          if (FBoundRect.Right < FTrace[i].x)
          then FBoundRect.Right := FTrace[i].x;
          if (FBoundRect.Bottom < FTrace[i].y)
          then FBoundRect.Bottom := FTrace[i].y;

          (*
          if (FMinCircleCalced)
          then begin
               if (FBoundRect.Left > Round(FSmallestCircle.Centre.x - FSmallestCircle.Radius))
               then FBoundRect.Left := Round(FSmallestCircle.Centre.x - FSmallestCircle.Radius);
               if (FBoundRect.Top > Round(FSmallestCircle.Centre.y - FSmallestCircle.Radius))
               then FBoundRect.Top := Round(FSmallestCircle.Centre.y - FSmallestCircle.Radius);
               if (FBoundRect.Right < Round(FSmallestCircle.Centre.x + FSmallestCircle.Radius))
               then FBoundRect.Right := Round(FSmallestCircle.Centre.x + FSmallestCircle.Radius);
               if (FBoundRect.Bottom < Round(FSmallestCircle.Centre.y + FSmallestCircle.Radius))
               then FBoundRect.Bottom := Round(FSmallestCircle.Centre.y + FSmallestCircle.Radius);
          end;
          *)
       end;

       Canvas.Lock;
       SetLeft(FBoundRect.Left);
       SetTop(FBoundRect.Top);
       SetWidth(FPen.Width + FBoundRect.Right - FBoundRect.Left);
       SetHeight(FPen.Width + FBoundRect.Bottom - FBoundRect.Top);
       Canvas.Unlock;

       FBoundingCalced := True;
  end;
  Result := FBoundRect;
end; // TmcmCustomPolygon.GetBoundingRect.


function TmcmCustomPolygon.GetLastIndex : integer;
begin
  // Determines the last index to use when calculating shape descriptors.
  // Should only be used with closed objects.
  if (FTrace[FCount-1].x = FTrace[0].x) and
     (FTrace[FCount-1].y = FTrace[0].y)
  then Result := FCount - 2
  else Result := FCount - 1;
end; // TmcmCustomPolygon.GetLastIndex.


function TmcmCustomPolygon.IsLeft(P0, P1, P2 : TmcmPoint) : integer;
begin
  // Tests if a point is Left, On or Right of an infinite line.
  //    Input:  three points P0, P1, and P2.
  //    Return: > 0 for P2 left of the line through P0 and P1.
  //            = 0 for P2 on the line.
  //            < 0 for P2 right of the line.
  Result := (P1.x - P0.x) * (P2.y - P0.y) - (P2.x - P0.x) * (P1.y - P0.y);
end; // TmcmCustomPolygon.IsLeft.


function TmcmCustomPolygon.HitCrossingNumber(x, y : integer) : boolean;
var i, j      : integer;
    OddNodes  : boolean;
    LastIndex : integer;
begin
  // Test if we're indes the polygon. Crossing Number.
  OddNodes := False;
  if IsClosed
  then begin
       LastIndex := GetLastIndex;
       j := 0;
       for i := 0 to LastIndex
       do begin
          inc(j);
          if (j = FCount)
          then j := 0;
          if ((FTrace[i].y < y) and (FTrace[j].y >= y)) or
             ((FTrace[j].y < y) and (FTrace[i].y >= y))
          then begin
               if (FTrace[i].x + (y - FTrace[i].y) /
                                 (FTrace[j].y - FTrace[i].y) *
                                 (FTrace[j].x - FTrace[i].x) < x)
               then OddNodes := Not(OddNodes);
          end;
       end;
  end;
  Result := OddNodes;
end; // TmcmCustomPolygon.HitCrossingNumber;


function TmcmCustomPolygon.HitWindingNumber(x, y : integer) : boolean;
var i, j      : integer;
    WN        : integer;
    LastIndex : integer;
begin
  // Winding number test for a point in a polygon
  // Input: x,y = a point,
  //        FTrace[] = vertex points of a polygon FTrace[FCount] where V[FCount] = V[0]
  // Return: WN = the winding number, 0 only if (x,y) is outside FTrace[]
  WN := 0; // The winding number counter
  if IsClosed
  then begin
       // Loop through all edges of the polygon
       j := 0;
       LastIndex := GetLastIndex;
       for i := 0 to LastIndex
       do begin // Edge from FTrace[i] to FTrace[i+1]
          inc(j);
          if (j >= FCount)
          then j := 0;
          if (FTrace[i].y <= y)
          then begin
               if (FTrace[j].y > y) // An upward crossing.
               then if (IsLeft(FTrace[i], FTrace[j], mcmPoint(x, y)) > 0) // (x,y) left of edge
                    then inc(WN); // Have a valid up intersect.
          end
          else begin
               if (FTrace[j].y <= y) // A downward crossing.
               then if (IsLeft(FTrace[i], FTrace[j], mcmPoint(x, y)) < 0) // (x,y) right of edge
                    then dec(WN); // Have a valid down intersect.
          end;
       end;
  end;
  Result := (WN <> 0);
end; // TmcmCustomPolygon.HitWindingNumber.


function TmcmCustomPolygon.HitTest(x, y : integer) : TmcmHitTest;
var i, j     : integer;
    dx, dy   : integer;
    a, b     : single;
    Dist     : single;
    MaxDist  : integer;
begin
  // HitTest will determin if x,y is "inside" a closed polygon or "on" the edge.
  Result := THT_NONE; // We're not inside or on the edge of the polygon data.
  if (FCount > 0)
  then begin
       GetBoundingRect; // LATTER: May not be require.
       MaxDist := 0;

       // First test if we're inside the bounding rectangle.
       if (FBoundRect.Left - MaxDist <= x) and (x <= FBoundRect.Right + MaxDist) and
          (FBoundRect.Top - MaxDist <= y) and (y <= FBoundRect.Bottom + MaxDist)
       then begin
            if HitWindingNumber(x, y)
            then Result := THT_INSIDE;

            // Test if we're on the edge of the polygon.
            j := 0;
            for i := 0 to (FCount - 1)
            do begin
               inc(j);
               if (j = FCount)
               then begin
                    j := 0;
                    if Not(FIsClosed)
                    then break;
               end;

               // Test if x is between two x points
               if (((FTrace[j].x - MaxDist <= x) and (x <= FTrace[i].x + MaxDist)) or
                   ((FTrace[i].x - MaxDist <= x) and (x <= FTrace[j].x + MaxDist)))
               then begin
                    // Test if y is between two y points
                    if (((FTrace[j].y - MaxDist <= y) and (y <= FTrace[i].y + MaxDist)) or
                        ((FTrace[i].y - MaxDist <= y) and (y <= FTrace[j].y + MaxDist)))
                    then begin
                         dx := FTrace[j].x - FTrace[i].x;
                         dy := FTrace[j].y - FTrace[i].y;
                         if (dx <> 0) and (dy <> 0)
                         then begin
                              a := dy / dx;
                              b := a * FTrace[i].x - FTrace[i].y;

                              if (abs(dx) > abs(dy))
                              then Dist := abs(a * x - b) - y
                              else Dist := abs((y + b) / a) - x;

                              if (-MaxDist < Dist) and (Dist < MaxDist)
                              then begin
                                   Result := THT_EDGE;
                                   break;
                              end;
                         end
                         else begin
                              if (dx = 0) and (((FTrace[i].x - MaxDist) <= x) and (x <= (FTrace[i].x + MaxDist)))
                              then begin
                                   Result := THT_EDGE;
                                   break;
                              end;
                              if (dy = 0) and (((FTrace[i].y - MaxDist) <= y) and (y <= (FTrace[i].y + MaxDist)))
                              then begin
                                   Result := THT_EDGE;
                                   break;
                              end;
                         end;
                    end;
               end;
            end;
       end;
  end;
end; // TmcmCustomPolygon.HitTest.


procedure TmcmCustomPolygon.Paint;
var i, GDIResult : integer;
begin
  if (FCount > 0)
  then begin
       GetBoundingRect;

       Canvas.Pen.Assign(FPen);
       Canvas.Brush.Assign(FBrush);

       if (FDrawTrace = Nil)
       then GetMem(FDrawTrace, FCount * SizeOf(TPoint));
       if (FScale <> 1.0)
       then begin
            for i := 0 to (FCount - 1)
            do begin
               FDrawTrace[i].x := Round((FTrace[i].x - FBoundRect.Left) * FScale);
               FDrawTrace[i].y := Round((FTrace[i].y - FBoundRect.Top) * FScale);
            end;
       end
       else begin
            for i := 0 to (FCount - 1)
            do begin
               FDrawTrace[i].x := FTrace[i].x - FBoundRect.Left;
               FDrawTrace[i].y := FTrace[i].y - FBoundRect.Top;
            end;
       end;

       // Paint polygon.
       if IsClosed
       then GDIResult := integer({$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.Polygon(Canvas.Handle, FDrawTrace^[0], FCount))
       else GDIResult := integer({$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PolyLine(Canvas.Handle, FDrawTrace^[0], FCount));
       if (GDIResult <> 0)
       then ;

       // As non of the GDI line functions will draw the end-point, we have
       // to draw a line from the last point in FTrace to the second-last point.
       if (FCount >= 2)
       then begin
            MoveToEx(Canvas.Handle, FDrawTrace^[FCount-1].x, FDrawTrace^[FCount-1].y, Nil);
            LineTo(Canvas.Handle, FDrawTrace^[FCount-2].x, FDrawTrace^[FCount-2].y);
       end;  
  end;
end; // TmcmCustomPolygon.Paint.


function TmcmCustomPolygon.GetConvexHull : TmcmCustomPolygon;
begin
  if (FConvexObj = Nil)
  then CalcConvexData;
  Result := FConvexObj;
end; // TmcmCustomPolygon.GetConvexHull.


function TmcmCustomPolygon.ReleaseConvexHull : TmcmCustomPolygon;
begin
  Result := GetConvexHull;
  FConvexObj := Nil;
end; // TmcmCustomPolygon.ReleaseConvexHull.


//------------------------------------------------------------------------------
// TmcmPolygon, data calculations.
//------------------------------------------------------------------------------

procedure TmcmCustomPolygon.SetImage(Value : TmcmImage);
begin
  if (FImage <> Value)
  then begin
       ClearCalculations;
       FImage := Value;
  end;
  if Assigned(FImage)
  then begin
       XResolution := FImage.XResolution;
       YResolution := FImage.YResolution;
  end;
end; // TmcmCustomPolygon.SetImage.


procedure TmcmCustomPolygon.SetXResolution(Value : double);
begin
  if (FxRes <> Value)
  then ClearCalculations;
  if (Value > 0.0)
  then FxRes := Value
  else FxRes := 1.0;
end; // TmcmCustomPolygon.SetXResolution.


procedure TmcmCustomPolygon.SetYResolution(Value : double);
begin
  if (FyRes <> Value)
  then ClearCalculations;
  if (Value > 0.0)
  then FyRes := Value
  else FyRes := 1.0;
end; // TmcmCustomPolygon.SetYResolution.


procedure TmcmCustomPolygon.CalcGeometricData;
var i, j      : integer;
    LastIndex : integer;
    dx, dy    : double;
    dx2, dy2  : double;
    DblData   : PVectorPtD;
begin
  // This method calculates the objects "Centre of Gravety", "Area" and
  // "Perimeter". If the object isn't closed only the Perimeter is calculated.
  FChainCG.x := 0.0;
  FChainCG.y := 0.0;
  FChainArea := 0.0;
  FPerimeter := 0.0;

  GetMem(DblData, (FCount + 1) * SizeOf(TPointDouble));
  try
    // Scale polygon points to selected resolution.
    for i := 0 to (FCount - 1)
    do begin
       DblData[i].x := FTrace[i].x / FxRes;
       DblData[i].y := FTrace[i].y / FyRes;
    end;

    if FIsClosed
    then begin // Object is closed.
         LastIndex := GetLastIndex;
         j := 1;
         for i := 0 to LastIndex 
         do begin
            if (j = FCount)
            then j := 0;

            dx  := DblData[j].x - DblData[i].x;
            dy  := DblData[j].y - DblData[i].y;

            dx2 := sqr(dx);
            dy2 := sqr(dy);

            // Sum Area chain code.
            FChainArea := FChainArea + ((DblData[j].x + DblData[i].x) * dy);

            // Sum center or gravity.
            FChainCG.x := FChainCG.x + sqr(DblData[j].x + DblData[i].x) * dy;
            FChainCG.y := FChainCG.y + sqr(DblData[j].y + DblData[i].y) * dx;

            // Perimeter.
            FPerimeter := FPerimeter + sqrt(dx2 + dy2);

            inc(j);
         end;

         // Final chain code Area.
         if (FChainArea < 0.0)
         then FChainArea := -0.5 * FChainArea
         else FChainArea := 0.5 * FChainArea;

         // Final Centre of gravity.
         if (FChainCG.x < 0.0)
         then FChainCG.x := -FChainCG.x;
         if (FChainCG.y < 0.0)
         then FChainCG.y := -FChainCG.y;
         if (FChainArea > 0.0)
         then begin
              FChainCG.x := FChainCG.x / (FChainArea * 8.0);
              FChainCG.y := FChainCG.y / (FChainArea * 8.0);
         end;

         FGeometricCalced := True;
    end
    else begin // Object is not closed.
         for i := 1 to (FCount - 1)
         do begin
            dx  := DblData[i].x - DblData[i-1].x;
            dy  := DblData[i].y - DblData[i-1].y;
            dx2 := sqr(dx);
            dy2 := sqr(dy);

            // Perimeter.
            FPerimeter := FPerimeter + sqrt(dx2 + dy2);
         end;
    end;
  finally
    FreeMem(DblData);
  end;
end; // TmcmCustomPolygon.CalcGeometricData.


procedure TmcmCustomPolygon.CalcDensityData;
var x, y         : integer; // Coordinate x,y
    xRGB, xInc   : integer;
    xRegS, yRegS : integer; // Region start coordinate
    xRegE, yRegE : integer; // Region end coordinate.
    PixelCount   : longint; // Pixels counted inside the object.

    Luminance    : integer;  // Luminance at x,y
    Density      : integer;  // Density at x,y
    xWDensity    : extended; // X weighted densities.
    yWDensity    : extended; // Y weighted densities.
    xWLuminance  : extended; // X weighted Luminance.
    yWLuminance  : extended; // Y weighted Luminance.

    pImageLine   : PVectorB; // Byte vector into image.
    pPalette     : PRGBQuadArray;
begin
  FSumLuminance  := 0.0;
  FAveLuminance  := 0.0;
  FSumDensity    := 0.0;
  FAveDensity    := 0.0;
  FDensityGC.x   := 0.0;
  FDensityGC.y   := 0.0;
  FLuminanceGC.x := 0.0;
  FLuminanceGC.y := 0.0;

  PixelCount    := 0; // Number of pixels visited.
  xWDensity     := 0;
  yWDensity     := 0;
  xWLuminance   := 0;
  yWLuminance   := 0;

  if (FImage = Nil)
  then exit; // FImage is not valid - Exit
  if (FImage.Empty)
  then exit; // FImage doesn't have a bitmap - Exit

  // Get object region.
  GetBoundingRect;
  xRegS := FBoundRect.Left;
  yRegS := FBoundRect.Top;
  xRegE := FBoundRect.Right;
  yRegE := FBoundRect.Bottom;

  pPalette := @TmcmCastImage(FImage).DibInfo.bmiColors[0];

  for y := yRegS to yRegE
  do begin
     pImageLine := FImage.ScanLine[y];

     case FImage.ImageFormat of
     IF_GREY8  : begin
                   for x := xRegS to xRegE
                   do begin
                      if HitWindingNumber(x, y)
                      then begin
                           Luminance := pPalette[pImageLine[x]].rgbRed;

                           // Density
                           Density   := 255 - Luminance; // OD^[iValue];
                           FSumDensity := FSumDensity + Density;
                           xWDensity := xWDensity + x * Density;
                           yWDensity := yWDensity + y * Density;

                           // Luminance
                           FSumLuminance := FSumLuminance + Luminance;
                           xWLuminance := xWLuminance + x * Luminance;
                           yWLuminance := yWLuminance + y * Luminance;

                           inc(PixelCount);
                           //pImageLine[x] := 0; // REMOVE
                      end;
                   end;
                 end;
     IF_PAL8   : begin
                   for x := xRegS to xRegE
                   do begin
                      if HitWindingNumber(x, y)
                      then begin
                           Luminance := Round(0.1144 * pPalette[pImageLine[x]].rgbRed +
                                              0.5867 * pPalette[pImageLine[x]].rgbGreen +
                                              0.2989 * pPalette[pImageLine[x]].rgbBlue);

                           // Density
                           Density := 255 - Luminance; // OD^[iValue];
                           FSumDensity := FSumDensity + Density;
                           xWDensity := xWDensity + x * Density;
                           yWDensity := yWDensity + y * Density;

                           // Luminance
                           FSumLuminance := FSumLuminance + Luminance;
                           xWLuminance := xWLuminance + x * Luminance;
                           yWLuminance := yWLuminance + y * Luminance;

                           inc(PixelCount);
                      end;
                   end;
                 end;
     IF_RGB24,
     IF_RGBA32 : begin
                   if (Image.ImageFormat = IF_RGB24)
                   then xInc := 3
                   else xInc := 4;
                   xRGB := xRegS * xInc;
                   for x := xRegS to xRegE
                   do begin
                      if HitWindingNumber(x, y)
                      then begin
                           Luminance := Round(0.2989 * pImageLine[xRGB] +   // Blue
                                              0.5867 * pImageLine[xRGB+1] + // Green
                                              0.1144 * pImageLine[xRGB+2]); // Red

                           // Density
                           Density := 255 - Luminance; // OD^[iValue];
                           FSumDensity := FSumDensity + Density;
                           xWDensity := xWDensity + x * Density;
                           yWDensity := yWDensity + y * Density;

                           // Luminance
                           FSumLuminance := FSumLuminance + Luminance;
                           xWLuminance := xWLuminance + x * Luminance;
                           yWLuminance := yWLuminance + y * Luminance;

                           inc(PixelCount);
                      end;
                      inc(xRGB, xInc);
                   end;
                 end;
     end;
  end;

  if (PixelCount <> 0)
  then begin
       FAveDensity   := FSumDensity / PixelCount;
       FAveLuminance := FSumLuminance / PixelCount;
  end;

  if (FSumDensity > 0.0)
  then begin
       FDensityGC.x    := xWDensity / FSumDensity;
       FDensityGC.y    := yWDensity / FSumDensity;
  end;

  if (FSumLuminance > 0.0)
  then begin
       FLuminanceGC.x    := xWLuminance / FSumLuminance;
       FLuminanceGC.y    := yWLuminance / FSumLuminance;
  end;
  FPixelArea := PixelCount;

  FDensCalced   := True;
end; // TmcmCustomPolygon.CalcDensityData.


procedure PointXYQSort(Pt : PVectorPt; Lo, Hi : longint);

     procedure xySort(const l, r : longint);
     var i, j, k : longint;
         x, y    : integer;
     begin
       i := l;
       j := r;
       k := (l + r) shr 1;
       x := Pt[k].x shl 16 + Pt[k].y;
       repeat
         while (Pt[i].x shl 16 + Pt[i].y < x)
         do inc(i);
         while (x < Pt[j].x shl 16 + Pt[j].y)
         do dec(j);
         if (i <= j)
         then begin
              y       := Pt[i].y;
              Pt[i].y := Pt[j].y;
              Pt[j].y := y;
              y       := Pt[i].x;
              Pt[i].x := Pt[j].x;
              Pt[j].x := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then xySort(l, j);
       if (i < r)
       then xySort(i, r);
     end; // xySort.

begin // PointXYQSort
  if (Lo < Hi)
  then begin
       xySort(Lo, Hi);
  end;
end; // PointXYQSort.


procedure TmcmCustomPolygon.CalcConvexData;
var i              : integer;
    FSorted        : PVectorPt;
    Bottom, Top    : integer;
    MinMin, MinMax : integer;
    MaxMin, MaxMax : integer;
    xMin, xMax     : integer;
begin
  if IsClosed and Not(Assigned(FConvexObj))
  then begin
       if Not(FGeometricCalced)
       then CalcGeometricData;

       FConvexObj := TmcmPolygon.Create(Self);
       FConvexObj.Pen.Color := RGB(0, 255, 0);
       FConvexObj.ResizeData(FMemSize);
       FConvexObj.Image := FImage;
       FConvexObj.XResolution := FXRes;
       FConvexObj.YResolution := FYRes;
       GetMem(FSorted, FCount * SizeOf(TmcmPoint));
       try
         CopyMemory(FSorted, FTrace, FCount * SizeOf(TmcmPoint));

         // Sort all point by increasing X and Y coordinates.
         PointXYQSort(FSorted, 0, (FCount - 1));

         // Andrew's monotone chain 2D convex hull algorithm
         // The output array FConvexObj.FTrace[] will be used as the stack
         Top := -1;

         // Get the indices of points with min x-coord and min|max y-coord
         MinMin := 0;
         xMin := FSorted[0].x;
         for i := 1 to (FCount - 1)
         do if (FSorted[i].x <> xMin)
            then break;
         MinMax := i - 1;

         if (MinMax = FCount - 1)
         then begin // Degenerate case: all x-coords = xmin
              inc(Top);
              FConvexObj.FTrace[Top] := FSorted[MinMin];
              if (FSorted[MinMax].y <> FSorted[MinMin].y) // A nontrivial segment
              then begin
                   inc(Top);
                   FConvexObj.FTrace[Top] := FSorted[MinMax];
              end;
              (*
              inc(Top);
              FConvexObj.FTrace[Top] := FSorted[MinMin]; // Add polygon endpoint
              *)
              FConvexObj.FCount := Top + 1;
         end
         else begin
              // Get the indices of points with max x-coord and min|max y-coord
              MaxMax := FCount - 1;
              xMax := FSorted[FCount-1].x;
              for i := (FCount - 2) downto 0
              do if (FSorted[i].x <> xMax)
                 then break;
              MaxMin := i + 1;


              // Compute the lower hull on the stack FConvexObj.FTrace
              inc(Top);
              FConvexObj.FTrace[Top] := FSorted[MinMin];      // push minmin point onto stack
              i := MinMax;
              inc(i);
              while (i <= MaxMin)
              do begin
                  // the lower line joins FSorted[MinMin] with FSorted[MaxMin]
                  if (IsLeft(FSorted[MinMin], FSorted[MaxMin], FSorted[i]) >= 0) and (i < MaxMin)
                  then          // ignore FSorted[i] above or on the lower line
                  else begin
                       while (Top > 0)        // there are at least 2 points on the stack
                       do begin
                           // test if FSorted[i] is left of the line at the stack top
                           if (IsLeft(FConvexObj.FTrace[Top-1], FConvexObj.FTrace[Top], FSorted[i]) > 0)
                           then break     // FSorted[i] is a new hull vertex
                           else dec(Top); // pop top point off stack
                       end;
                       inc(Top);
                       FConvexObj.FTrace[Top] := FSorted[i];       // push FSorted[i] onto stack
                  end;
                  inc(i);
              end;

              // Next, compute the upper hull on the stack FConvexObj.FTrace
              // above the bottom hull
              if (MaxMax <> MaxMin) // If distinct xMax points
              then begin
                   inc(Top);
                   FConvexObj.FTrace[Top] := FSorted[MaxMax];  // Push MaxMax point onto stack
              end;
              Bottom := Top; // The bottom point of the upper hull stack
              i := MaxMin;
              dec(i);
              while (i >= MinMax) and (Top < FCount - 1)
              do begin
                 // The upper line joins FSorted[MaxMax] with FSorted[MinMax]
                 if (IsLeft(FSorted[MaxMax], FSorted[MinMax], FSorted[i]) >= 0) and (i > MinMax)
                 then // ignore FSorted[i] below or on the upper line
                 else begin
                      while (Top > Bottom) // At least 2 points on the upper stack
                      do begin
                          // Test if FSorted[i] is left of the line at the stack top
                          if (IsLeft(FConvexObj.FTrace[Top-1], FConvexObj.FTrace[Top], FSorted[i]) > 0)
                          then break     // FSorted[i] is a new hull vertex
                          else dec(Top); // Pop top point off stack
                      end;
                      inc(Top);
                      FConvexObj.FTrace[Top] := FSorted[i]; // Push FSorted[i] onto stack
                 end;
                 dec(i);
              end;
              (*
              if (MinMax <> MinMin)
              then begin
                   inc(Top);
                   FConvexObj.FTrace[Top] := FSorted[MinMin];  // Push joining endpoint onto stack
              end;
              *)
              FConvexObj.FCount := Top + 1;
         end;
       finally
         FreeMem(FSorted);
       end;
  end;
end; // TmcmCustomPolygon.CalcConvexData.


procedure Swap(var A, B : double);
var C: double;
begin
  C := A;
  A := B;
  B := C;
end; // Swap.

(*
procedure TCircumscribedCircle.Clear;
begin
  Centre.x := 0;
  Centre.y := 0;
  Radius   := 0;
end; // TCircumscribedCircle.Clear.


procedure TmcmCustomPolygon.FindCircle(var Circle : TCircumscribedCircle; const Points : TList<TmcmPoint>; a, b : integer; var Indexes : TList<integer>; var Exclude : TList<integer>);
var A1, B1      : double;
    A2, B2      : double;
    Mid         : TPointDouble;
    NotVertical : boolean;
    MaxX, MaxY  : double;
    MinX, MinY  : double;
    NewCircle   : TCircumscribedCircle;
    i           : integer;
    Test        : TPointDouble;
    Centre      : TPointDouble;
    Distance    : double;
begin
  Indexes.Clear();

  // Calc line going from centre of circle through middle point of point "a" and "b".
  Mid := PointDouble(Points[a] + Points[b]);
  Mid := Mid / 2.0;
  A1 := 0.0;
  B1 := 0.0;
  NotVertical := True;
  if (Circle.Centre.X <> Mid.X)
  then begin
       A1 := (Circle.Centre.Y - Mid.Y) / (Circle.Centre.X - Mid.X);
       B1 := Circle.Centre.Y - A1 * Circle.Centre.X;
  end
  else NotVertical := false;

  MaxX := Mid.X;
  MinX := Circle.Centre.X;
  MaxY := Mid.Y;
  MinY := Circle.Centre.Y;
  if (MaxX < MinX)
  then Swap(MaxX, MinX);
  if (MaxY < MinY)
  then Swap(MaxY, MinY);

  for i := 0 to (Points.Count - 1)
  do begin
     if ((i <> a) and (i <> b) and Not(Exclude.Contains(i)))
     then begin
          Test.x := (Points[a].x + Points[i].x) / 2.0;
          Test.y := (Points[a].y + Points[i].y) / 2.0;

          if (NotVertical)
          then begin
               if (Points[a].X <> Points[i].X)
               then begin
                    if (Points[a].Y <> Points[i].Y)
                    then begin
                         A2 := -(Points[a].X - Points[i].X) / (Points[a].Y - Points[i].Y);
                         B2 := Test.Y - A2 * Test.X;

                         Centre.X := ((B2 - B1) / (A1 - A2));
                         Centre.Y := (A1 * Centre.X + B1);
                    end
                    else begin
                         Centre.X := Test.X;
                         Centre.Y := (A1 * Centre.X + B1);
                    end;
               end
               else begin
                    Centre.Y := Test.Y;
                    Centre.X := ((Centre.Y - B1) / A1);
               end;
          end
          else begin
               Centre.X := Circle.Centre.X;

               if (Points[a].X <> Points[i].X)
               then begin
                    if (Points[a].Y <> Points[i].Y)
                    then begin
                         A2 := -(Points[a].X - Points[i].X) / (Points[a].Y - Points[i].Y);
                         B2 := Test.Y - A2 * Test.X;
                         Centre.Y := Round(A2 * Centre.X + B2);
                    end
                    else Centre.Y := Points[i].Y;
               end
               else Centre.Y := Test.Y;
          end;

          if ((MinX <= Centre.X) and (Centre.X <= MaxX) and (MinY <= Centre.Y) and (Centre.Y <= MaxY))
          then begin
               Distance := sqr(Centre.X - Points[i].X) + sqr(Centre.Y - Points[i].Y);
               if ((NewCircle.Radius <= Distance) and (Distance < Circle.Radius))
               then begin
                    if (NewCircle.Radius < Distance)
                    then Indexes.clear();
                    Indexes.add(i);
                    NewCircle.Radius := Distance;
                    NewCircle.Centre := Centre;
               end;
          end;
     end;
  end;
  if (Indexes.Count > 0)
  then begin
       Indexes.Add(a);
       if (a <> b)
       then Indexes.Add(b);
       Indexes.Sort;

       Circle := NewCircle;
  end;
end; // TmcmCustomPolygon.FindCircle.
*)
(*
function TmcmCustomPolygon.CheckArcLength(var Circle : TCircumscribedCircle; const MinDist : double; const Points : TList<TmcmPoint>; const Indexes : TList<integer>; var a : integer; var b : integer; var Rad : TList<double>) : boolean;
var Test     : double;
    Radian   : double;
    Distance : double;
    i, j     : integer;
begin
  Result := True;
  if (Indexes.Count > 1)
  then begin
       Rad.Clear;
       Rad.Capacity := Indexes.Count;

       for i := 0 to (Indexes.Count - 1)
       do begin
          j := Indexes[i];
          Rad.Add(ArcTan2((Circle.Centre.Y - Points[j].Y), (Circle.Centre.X - Points[j].X)) + Pi);
       end;
       j := 1;
       Test := 0.0;
       for i := 0 to (Indexes.Count - 1)
       do begin
          //Exclude.insert(Indexes[i]); // Do not test already tested points.
          if (j >= Indexes.Count)
          then j := 0;
          if (Rad[j] >= Rad[i])
          then Radian := Rad[j] - Rad[i]
          else Radian := 2.0 * Pi + Rad[j] - Rad[i];

          if (Test < Radian)
          then begin
               Test := Radian;
               a := Indexes[i];
               b := Indexes[j];
          end;
          inc(j);
       end;

       if (Test <= Pi)
       then Result := False // We're done
       else begin
            Distance := (sqr(Points[a].X - Points[b].X) + sqr(Points[a].Y - Points[b].Y)) / 4.0;
            if (abs(Circle.Radius - Distance) < MinDist)
            then Result := False; // We're done.
       end;
  end;
end; // TmcmCustomPolygon.CheckArcLength.


procedure TmcmCustomPolygon.CalcSmallestCircle;
var CircumscribedCircle : TCircumscribedCircle;
    Index      : integer;
    Points     : TList<TmcmPoint>;
    Indexes    : TList<integer>;
    Exclude    : TList<integer>;
    Rad        : TList<double>;
    a, b       : integer;
    old_a      : integer;
    old_b      : integer;
    Distance   : double;
    MinDist    : double;
    DoContinue : boolean;
begin
  if Not(FMinCircleCalced)
  then begin
       // Get convex hull.
       if (FConvexObj = Nil)
       then CalcConvexData;

       if Assigned(FConvexObj)
       then begin
            Points := TList<TmcmPoint>.Create();
            Points.Capacity := FConvexObj.Count;
            for Index := 0 to (FConvexObj.Count - 1)
            do Points.Add(mcmPoint(FConvexObj.FTrace[Index].x, FConvexObj.FTrace[Index].y));

            // If first and last point has same coordinates, remove last point.
            if (Points.First = Points.Last)
            then Points.Delete(Points.Count - 1);

            // Scale data to physical coordinates.
            //for (unsigned int Index = 0; Index < Points.size(); ++Index)
            // do Points[Index] = Scale * Points[Index] + Offset;

            // Finde first centre point.
            CircumscribedCircle.Clear();

            for Index := 0 to (Points.Count - 1)
            do CircumscribedCircle.Centre := CircumscribedCircle.Centre + PointDouble(Points[Index]);
            CircumscribedCircle.Centre := CircumscribedCircle.Centre / Points.Count;

            Indexes := TList<integer>.Create();
            Indexes.Capacity := Points.Count + 1;
            Rad := TList<double>.Create();
            Rad.Capacity := Points.Count + 1;

            // Find point(s) farthest from centre point
            old_a := Points.Count;
            old_b := Points.Count;
            a := 0;
            b := 0;

            CircumscribedCircle.Radius := 0;
            for Index := 0 to (Points.Count - 1)
            do begin
               Distance := Sqr(CircumscribedCircle.Centre.X - Points[Index].X) + Sqr(CircumscribedCircle.Centre.Y - Points[Index].Y);
               if (CircumscribedCircle.Radius <= Distance)
               then begin
                    if (CircumscribedCircle.Radius < Distance)
                    then begin
                         a := Index;
                         Indexes.clear();
                         CircumscribedCircle.Radius := Distance;
                    end;
                    Indexes.Add(Index);
               end;
            end;
            b := a;

            MinDist := 0; // sqrt(sqr(Scale.X) + sqr(Scale.Y));
            DoContinue := CheckArcLength(CircumscribedCircle, MinDist, Points, Indexes, a, b, Rad);

            Exclude := TList<integer>.Create();
            Exclude.Capacity := Points.Count + 1;

            while (DoContinue)
            do begin
               FindCircle(CircumscribedCircle, Points, a, b, Indexes, Exclude);

               DoContinue := CheckArcLength(CircumscribedCircle, MinDist, Points, Indexes, a, b, Rad);

               if (((a = old_a) and (b = old_b)) or ((a = old_b) and (b = old_a)))
               then DoContinue := false; // We're done

               old_a := a;
               old_b := b;
            end;

            CircumscribedCircle.Radius := sqrt(CircumscribedCircle.Radius);

            FSmallestCircle := CircumscribedCircle;
            FMinCircleCalced := True;
            FBoundingCalced := false;

            Points.Destroy;
            Indexes.Destroy;
            Exclude.Destroy;
            Rad.Destroy;
       end;
  end;
end; // TmcmCustomPolygon.CalcSmallestCircle.
*)

procedure TmcmCustomPolygon.CalcMaxMinRadius;
var i        : integer;
    dx, dy   : double;
    Radius   : double;
begin
  // Calculate the "max radius" of a circle centred at the centre of gravity
  // which encloses every point in the polygon.
  // Calculates the "min radius" of a circle centred at the centre of gravity
  // which is enclosed by the polygon.
  if IsClosed
  then begin
       if Not(FGeometricCalced)
       then CalcGeometricData;

       FMaxRadius := 0.0;
       FMinRadius := 1.7E10307;
       for i := 0 to (FCount - 1)
       do begin
          dx := (FTrace[i].x - FChainCG.x) / FxRes;
          dy := (FTrace[i].y - FChainCG.y) / FyRes;
          Radius := dx * dx + dy * dy;
          if (FMaxRadius < Radius)
          then FMaxRadius := Radius;
          if (FMinRadius > Radius)
          then FMinRadius := Radius;
       end;
       FMaxRadius := Sqrt(FMaxRadius);
       FMinRadius := Sqrt(FMinRadius);
       FMaxMinRadius := True;
  end
end; // TmcmCustomPolygon.CalcMaxMinRadius.


function TmcmCustomPolygon.InternRotate(pDblData  : PVectorPtD;
                                             Radian    : double) : boolean;
var i          : integer;
    xA, yA     : double;
    xC, yC     : double;
    cosA, sinA : double;
begin
  // Rotates the Traced polygon, outputting this in pDblData.
  if (FTrace <> Nil) and (pDblData <> Nil)
  then begin
       try
         xC := (FBoundRect.Right - FBoundRect.Left) / 2.0 + FBoundRect.Left;
         yC := (FBoundRect.Bottom - FBoundRect.Top) / 2.0 + FBoundRect.Top;

         cosA := Cos(Radian);
         sinA := Sin(Radian);
         for i := 0 to (FCount - 1)
         do begin
            xA := (FTrace[i].x - xC) * cosA - (FTrace[i].y - yC) * sinA;
            yA := (FTrace[i].x - xC) * sinA + (FTrace[i].y - yC) * cosA;
            pDblData[i].x := xA + xC;
            pDblData[i].y := yA + yC;
         end;
         Result := True;
       except
         Result := False;
       end
  end
  else Result := False;
end; // TmcmCustomPolygon.InternRotate.


function TmcmCustomPolygon.GetArea : double;
begin
  if IsClosed
  then begin
       if Not(FGeometricCalced)
       then CalcGeometricData;
  end
  else FChainArea := 0.0;
  Result := FChainArea;
end; // TmcmCustomPolygon.GetArea.


function TmcmCustomPolygon.GetAreaCentroid : TPointDouble;
var x, y   : integer;
    Sx, Sy : double;
begin
  if Not(FCGCalced)
  then begin
       if IsClosed
       then begin
            GetBoundingRect;
            Sx  := 0.0;
            Sy  := 0.0;
            FPixelArea := 0;
            for y := FBoundRect.Top to FBoundRect.Bottom
            do begin
               for x := FBoundRect.Left to FBoundRect.Right
               do begin
                  if HitWindingNumber(x, y)
                  then begin
                       Inc(FPixelArea);
                       Sx  := Sx + x;
                       Sy  := Sy + y;
                  end;
               end;
            end;

            if (FPixelArea > 0)
            then begin
                 FCG.x := Sx / (FPixelArea * FxRes);
                 FCG.y := Sy / (FPixelArea * FyRes);
            end;
       end
       else begin
            FCG.x := 0;
            FCG.y := 0;
       end;
       FCGCalced := True;
  end;
  Result := FCG;
end; // TmcmCustomPolygon.GetAreaCentroid.


function TmcmCustomPolygon.GetAspectRatio : double;
var MinDiameter : double;
begin
  MinDiameter := GetBreadth;
  if (MinDiameter > 0.0)
  then Result := GetLength / MinDiameter
  else Result := 0.0;
end; // TmcmCustomPolygon.GetAspectRatio.


function TmcmCustomPolygon.GetAverageDensity : double;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  Result := FAveDensity;
end; // TmcmCustomPolygon.GetAverageDensity.


function TmcmCustomPolygon.GetAverageLuminance : double;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  Result := FAveLuminance;
end; // TmcmCustomPolygon.GetAverageLuminance.


function TmcmCustomPolygon.GetBreadth : double;
begin
  if Not(FLengthCalced)
  then GetLength;
  Result := FBreadth;
end; // TmcmCustomPolygon.GetBreadth.


function TmcmCustomPolygon.GetCompactness : double;
var MaxDiameter : double;
begin
  if Not(FGeometricCalced) or (FChainArea <= 0.0)
  then GetArea;
  MaxDiameter := GetLength;
  if (MaxDiameter > 0.0) and (FChainArea > 0.0)
  then Result := Sqrt(4.0 * FChainArea / pi) / MaxDiameter
  else Result := 0.0;
end; // TmcmCustomPolygon.GetCompactness.


function TmcmCustomPolygon.GetConvexity : double;
begin
  if (GetConvexHull <> Nil) and (GetPerimeter <> 0.0)
  then Result := FConvexObj.GetPerimeter / GetPerimeter
  else Result := 0;
end; // TmcmCustomPolygon.GetConvexity.

(*
function TmcmCustomPolygon.GetSmallestCircle : TCircumscribedCircle;
begin
  if Not(FMinCircleCalced)
  then CalcSmallestCircle;
  if (FMinCircleCalced)
  then Result := FSmallestCircle;
end; // TmcmCustomPolygon.GetSmallestCircle.
*)
(*
function TmcmCustomPolygon.GetCurl : double;
begin
  Result := -1;
end; // TmcmCustomPolygon.GetCurl.
*)

function TmcmCustomPolygon.GetDensityCentroid : TPointDouble;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  if (FxRes <> 0.0)
  then Result.x := FDensityGC.x / FxRes
  else Result.x := 0;
  if (FyRes <> 0.0)
  then Result.y := FDensityGC.y / FyRes
  else Result.y := 0;
end; // TmcmCustomPolygon.GetDensityCentroid.


function TmcmCustomPolygon.GetElongation : double;
var ALength : double;
begin
  // - Elongation
  //   Sometimes defined as
  //   E = |w - h| / h
  //   Is zero for squares and circles
  ALength := GetLength;
  if (ALength > 0.0)
  then Result := Abs(ALength - GetBreadth) / ALength
  else Result := 1.0;
end; // TmcmCustomPolygon.GetElongation.


{
function TmcmCustomPolygon.GetExtent : double;
begin
  Result := -1;
end; // TmcmCustomPolygon.GetExtent.
}

{
function TmcmCustomPolygon.GetFiberLength : double;
begin
  // Problem -
  // 1. Create a skeleton of the shape
  // 2. trace the skeleton image/shape!
  // 3. Fiber length is obtained using GetLength
  Result := -1;
end; // TmcmCustomPolygon.GetFiberLength.
}

{
function TmcmCustomPolygon.GetFiberWidth : double;
begin
  Result := -1;
end; // TmcmCustomPolygon.GetFiberWidth.
}

function TmcmCustomPolygon.GetFormFactor : double;
begin
  if (GetPerimeter > 0.0)
  then Result := 4 * pi * GetArea / Sqr(FPerimeter)
  else Result := 0.0;
end; // TmcmCustomPolygon.GetFormFactor.


function TmcmCustomPolygon.GetHeywoodDiameter : double;
begin
  if (GetArea > 0.0)
  then Result := 2 * Sqrt(FChainArea / pi)
  else Result := 0.0;
end; // TmcmCustomPolygon.GetHeywoodDiameter.


function TmcmCustomPolygon.GetLength : double;
const pi180 = 57.2957795131;
var DblData  : PVectorPtD;
    xr, xl   : double;
    yt, yb   : double;
    i        : integer;
    Radian   : double;
    XRotRes  : double;
    YRotRes  : double;
begin
  if Not(FOrientCalced)
  then GetOrientation;

  if Not(FLengthCalced)
  then begin
       xl := 2147483647.0;
       xr := 0.0;
       yt := 2147483647.0;
       yb := 0.0;

       // Rotate object FOrientation degrees, x-axis <=> Length.
       if (FOrientation <> 0.0)
       then begin
            GetMem(DblData, FCount * SizeOf(TPointDouble));

            // Rotate polygon.
            Radian := FOrientation / pi180;
            InternRotate(DblData, Radian);

            for i := 0 to (FCount - 1)
            do begin
               if (xl > DblData[i].x)
               then begin
                    xl := DblData[i].x;
                    FLengthStart := i;
               end;
               if (xr < DblData[i].x)
               then begin
                    xr := DblData[i].x;
                    FLengthEnd := i;
               end;
               if (yt > DblData[i].y)
               then begin
                    yt := DblData[i].y;
                    FBreadthStart := i;
               end;
               if (yb < DblData[i].y)
               then begin
                    yb := DblData[i].y;
                    FBreadthEnd := i;
               end;
            end;
            FreeMem(DblData);
       end
       else begin
            Radian := 0.0;
            for i := 0 to (FCount - 1)
            do begin
               if (xl > FTrace[i].x)
               then begin
                    xl := FTrace[i].x;
                    FLengthStart := i;
               end;
               if (xr < FTrace[i].x)
               then begin
                    xr := FTrace[i].x;
                    FLengthEnd := i;
               end;
               if (yt > FTrace[i].y)
               then begin
                    yt := FTrace[i].y;
                    FBreadthStart := i;
               end;
               if (yb < FTrace[i].y)
               then begin
                    yb := FTrace[i].y;
                    FBreadthEnd := i;
               end;
            end;
       end;

       // Compensate for different x, y resolution.
       XRotRes := Sqrt(Sqr(FxRes * Cos(Radian)) + Sqr(FyRes * Sin(Radian)));
       YRotRes := Sqrt(Sqr(FyRes * Cos(Radian)) + Sqr(FxRes * Sin(Radian)));

       // Use original co-ordinates.
       FLength := abs(xr - xl) / XRotRes;
       FBreadth := abs(yb - yt) / YRotRes;

       FLengthCalced := True;
  end;
  Result := FLength;
end; // TmcmCustomPolygon.GetLength.


function TmcmCustomPolygon.GetLuminanceCentroid : TPointDouble;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  if (FxRes <> 0.0)
  then Result.x := FLuminanceGC.x / FxRes
  else Result.x := 0;
  if (FyRes <> 0.0)
  then Result.y := FLuminanceGC.y / FyRes
  else Result.y := 0;
end; // TmcmCustomPolygon.GetLuminanceCentroid.


function TmcmCustomPolygon.GetMaxFeret : double;
var i, j    : integer;
    a, b    : integer;
    dx, dy  : integer;
    cx, cy  : double;
    DstLR   : integer;
    DstTB   : integer;
    Ferets  : PVectorPt;
    Count   : integer;
    Start   : integer;
    Dist    : integer;
    MaxDist : integer;
begin
  // Calculate the longest coord inside the polygon.
  if Not(FMaxFeretCalced)
  then begin
       GetBoundingRect;
       dx := FBoundRect.Right - FBoundRect.Left;
       dy := FBoundRect.Bottom - FBoundRect.Top;

       cx := dx / 2.0;
       cy := dy / 2.0;
       cx := cx + FBoundRect.Left;
       cy := cy + FBoundRect.Top;

       DstLR := dx * dx - 1;
       DstTB := dy * dy - 1;

       GetMem(Ferets, FMemSize * SizeOf(TPoint));
       try
         j := 0;
         // Reduce number of points to check, by emilinating points:
         // Locate points that are outside or on the radius specified by
         // (FBoundRect.Left,cy), (cx,FBoundRect.Top), (FBoundRect.Right,cy) or
         // (cx,FBoundRect.Bottom)
         for i := 0 to (FCount - 1)
         do begin
            if ((Sqr(FTrace[i].x - FBoundRect.Left) + Sqr(FTrace[i].y - cy)) >= DstLR)
            then begin
                 Ferets[j] := FTrace[i];
                 inc(j);
            end
            else
            if ((Sqr(FTrace[i].x - FBoundRect.Right) + Sqr(FTrace[i].y - cy)) >= DstLR)
            then begin
                 Ferets[j] := FTrace[i];
                 inc(j);
            end
            else
            if ((Sqr(FTrace[i].x - cx) + Sqr(FTrace[i].y - FBoundRect.Top)) >= DstTB)
            then begin
                 Ferets[j] := FTrace[i];
                 inc(j);
            end
            else
            if ((Sqr(FTrace[i].x - cx) + Sqr(FTrace[i].y - FBoundRect.Bottom)) >= DstTB)
            then begin
                 Ferets[j] := FTrace[i];
                 inc(j);
            end;
         end;

         // Now, locate the two point farthest apart.
         Count := j;
         a := 0;
         b := 0;
         FMaxFeret := 0.0;
         MaxDist   := 0;
         Start     := 1;
         for i := 0 to (Count - 2)
         do begin
            for j := Start to (Count - 1)
            do begin
               Dist := Sqr(Ferets[i].x - Ferets[j].x) + Sqr(Ferets[i].y - Ferets[j].y);
               if (MaxDist < Dist)
               then begin
                    a := i;
                    b := j;
                    MaxDist := Dist;
               end;
            end;
            inc(Start);
         end;

         FFeretStart := Ferets[a];
         FFeretEnd   := Ferets[b];
         FMaxFeret := Sqrt(MaxDist);
         FMaxFeretCalced := True;
       finally
         FreeMem(Ferets);
       end;
  end;
  Result := FMaxFeret;
end; // TmcmCustomPolygon.GetMaxFeret.


function TmcmCustomPolygon.GetMaxRadius : double;
begin
  if Not(FMaxMinRadius)
  then CalcMaxMinRadius;
  Result := FMaxRadius;
end; // TmcmCustomPolygon.GetMaxRadius.


function TmcmCustomPolygon.GetMinRadius : double;
begin
  if Not(FMaxMinRadius)
  then CalcMaxMinRadius;
  Result := FMinRadius;
end; // TmcmCustomPolygon.GetMinRadius.


{
function TmcmCustomPolygon.GetModificationRatio : double;
begin
  // Modification Ration = Incribed diameter / Maximum diameter.
  Result := -1;
end; // TmcmCustomPolygon.GetModificationRatio.
}

function TmcmCustomPolygon.GetOrientation : double;
const pi180 = 57.2957795;
      pi_2  = pi / 2.0;
    // Closed object.
var x, y        : integer;
    Sx, Sy      : double;
    Sxx, Syy    : double;
    Sxy         : double;
    Mx, My, Mxy : double;
    Msqr        : double;
    // Not closed object.
    a, b, c, d : double;
    SumWeight : double;
    SumAngle  : double;
    i, j      : integer;
begin
  if Not(FOrientCalced)
  then begin
       if IsClosed
       then begin
            GetBoundingRect;

            Sx  := 0.0;
            Sy  := 0.0;
            Sxx := 0.0;
            Syy := 0.0;
            Sxy := 0.0;

            FPixelArea := 0;
            for y := FBoundRect.Top to FBoundRect.Bottom
            do begin
               for x := FBoundRect.Left to FBoundRect.Right
               do begin
                  if HitWindingNumber(x, y) // HitWindingNumber(x, y) // HitCrossingNumber(x, y)
                  then begin
                       Inc(FPixelArea);
                       Sx  := Sx + x;
                       Sy  := Sy + y;
                       Sxx := Sxx + x * x;
                       Syy := Syy + y * y;
                       Sxy := Sxy + x * y;
                  end;
               end;
            end;

            if (FPixelArea > 0)
            then begin
                 Mx  := Sxx - Sx * Sx / FPixelArea;
                 My  := Syy - Sy * Sy / FPixelArea;
                 Mxy := Sxy - Sx * Sy / FPixelArea;

                 Msqr := Sqr(Mx - My) + 4 * Mxy * Mxy;
                 if (Msqr < 0.0)
                 then Msqr := -Msqr;

                 if (Mxy <> 0.0)
                 then begin
                      FOrientation := 90.0 + ArcTan((Mx - My + Sqrt(Msqr)) / (2 * Mxy)) * pi180;
                      if (FOrientation = 90.0)
                      then if ((FBoundRect.Right - FBoundRect.Left) >= (FBoundRect.Bottom - FBoundRect.Top))
                           then FOrientation := 0.0;
                 end
                 else begin
                      if (abs(FBoundRect.Right - FBoundRect.Left) >= abs(FBoundRect.Bottom - FBoundRect.Top))
                      then FOrientation := 0.0
                      else FOrientation := 90.0;
                 end;
                 FOrientCalced := True;
            end;
       end
       else begin
            SumWeight := 0.0;
            SumAngle  := 0.0;

            j := 1;
            for i := 0 to (FCount - 1)
            do begin
               if (j >= FCount)
               then j := 0;

               b := FTrace[j].y - FTrace[i].y;
               a := FTrace[j].x - FTrace[i].x;

               c := sqrt(a * a + b * b);
               SumWeight := SumWeight + c;
               if (b <> 0.0)
               then d := pi_2 + arctan(a / b)
               else d := 0.0;
               SumAngle := SumAngle + c * d;
            end;

            if (SumWeight > 0.0)
            then FOrientation := (SumAngle / SumWeight) * pi180
            else FOrientation := 0.0;

            FOrientCalced := True;
       end;
  end;
  Result := FOrientation;
end; // TmcmCustomPolygon.GetOrientation.


function TmcmCustomPolygon.GetOutlineCentroid : TPointDouble;
begin
  if Not(FGeometricCalced)
  then CalcGeometricData;
  Result := FChainCG;
  (*
  Could be Procedure BoxedCentroid;
  GetBoundingRect;
  if (FxRes <> 0.0)
  then Result.x := (FBoundRect.Left + (FBoundRect.Right - FBoundRect.Left) / 2.0) / FxRes
  else Result.x := 0;
  if (FyRes <> 0.0)
  then Result.y := (FBoundRect.Bottom + (FBoundRect.Top - FBoundRect.Bottom) / 2.0) / FyRes
  else Result.y := 0;
  *)
end; // TmcmCustomPolygon.GetOutlineCentroid.


function TmcmCustomPolygon.GetPerimeter : double;
begin
  if Not(FGeometricCalced)
  then CalcGeometricData;
  Result := FPerimeter;
end; // TmcmCustomPolygon.GetPerimeter.


function TmcmCustomPolygon.GetRoughness : double;
begin
  // The roughness of the grains is calculated by the following formula (Janoo 1998):
  // Roughness = Perimeter / Convex Perimeter
  if (GetConvexHull <> Nil) and (FConvexObj.GetPerimeter <> 0.0)
  then Result := GetPerimeter / FConvexObj.FPerimeter
  else Result := 0.0;
end; // TmcmCustomPolygon.GetRoughness.


function TmcmCustomPolygon.GetRoundness : double;
begin
  if Not(FGeometricCalced) or (FChainArea <= 0.0)
  then GetArea;

  if (GetLength <> 0.0)
  then Result := (4.0 * FChainArea) / (pi * sqr(FLength))
  else Result := 0.0;
  (*
  GetSmallestCircle;
  if (FSmallestCircle.Radius <> 0.0)
  then Result := (4.0 * FChainArea) / (pi * sqr(2.0 * FSmallestCircle.Radius))
  else Result := 0.0;
  *)
end; // TmcmCustomPolygon.GetRoundness.


function TmcmCustomPolygon.GetSolidity : double;
begin
  CalcConvexData;
  if Assigned(FConvexObj) and (FConvexObj.GetArea <> 0.0)
  then Result := FChainArea / FConvexObj.GetArea
  else Result := 0;
end; // TmcmCustomPolygon.GetSolidity.


function TmcmCustomPolygon.GetSumDensity : double;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  Result := FSumDensity;
end; // TmcmCustomPolygon.GetSumDensity.


function TmcmCustomPolygon.GetSumLuminance : double;
begin
  if Not(FDensCalced)
  then CalcDensityData;
  Result := FSumLuminance;
end; // TmcmCustomPolygon.GetSumLuminance.


//------------------------------------------------------------------------------
// TmcmPolygon.
//------------------------------------------------------------------------------

constructor TmcmPolygon.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  FHitIndex := THT_NONE;
  FLBtnDown := False;

  FShapePen := TPen.Create;
  FShapePen.Color := RGB(0, 255, 0);
  FShapePen.OnChange := StyleChanged;

  FShowAreaCenter     := False;
  FShowBreadth        := False;
  FShowConvex         := False;
  FShowDensCenter     := False;
  FShowIndex          := False;
  FShowLumCenter      := False;
  FShowLength         := False;
  FShowMaxFeret       := False;
  FShowMaxRadius      := False;
  FShowMinRadius      := False;
  FShowOutline        := True;
  FShowSmallestCircle := True;
end; // TmcmPolygon.Create.


destructor TmcmPolygon.Destroy;
begin
  if Assigned(FShapePen)
  then FShapePen.Free;
  FShapePen := Nil;

  Inherited Destroy;
end; // TmcmPolygon.Destroy.


procedure TmcmPolygon.CMHitTest(var Message: TCMHitTest);
var x, y        : integer;
    OldHitIndex : TmcmHitTest;
begin
  if (csDesigning in ComponentState) or FLBtnDown
  then Message.Result := HTCLIENT
  else begin
       x := Round((Message.XPos + FBoundRect.Left) / FScale);
       y := Round((Message.YPos + FBoundRect.Top) / FScale);

       OldHitIndex := FHitIndex;
       FHitIndex := HitTest(x, y);

       if (OldHitIndex <> FHitIndex) and
          ((OldHitIndex = THT_NONE) or (FHitIndex = THT_NONE))
       then begin
            case FHitIndex of
            THT_EDGE   : Cursor := crCross;
            THT_INSIDE : Cursor := crCross;
            else Cursor := crDefault;
            end;
            if (Parent <> Nil)
            then Invalidate;
       end;

       if (FHitIndex <> THT_NONE) or (OldHitIndex <> FHitIndex)
       then Message.Result := HTCLIENT
       else Message.Result := 0;
  end;
end; // TmcmPolygon.CMHitTest.


procedure TmcmPolygon.CMMouseLeave(var Message: TMessage);
begin
  if (FHitIndex <> THT_NONE) and Not(FLBtnDown)
  then begin
       FHitIndex := THT_NONE;
       if (Parent <> Nil)
       then ; //Invalidate;
  end;
  if (Parent <> Nil)
  then Parent.Perform(CM_MOUSELEAVE, 0, Longint(Self));
end; // TmcmPolygon.CMMouseLeave.


procedure TmcmPolygon.WMMouseMove(var Msg : TMessage);
begin

  Inherited ;
  Msg.Result := 0;
end; // TmcmPolygon.WMMouseMove.


procedure TmcmPolygon.WMLButtonDown(var Msg : TMessage);
begin
  Inherited ;
  if Not(FLBtnDown)
  then begin
       if (FHitIndex <> THT_NONE)
       then begin
            BringToFront;
            FLBtnDown := True;
       end;
       case FHitIndex of
       THT_EDGE,
       THT_INSIDE : begin
                      Fmx := Round((LoWord(Msg.LParam) + FBoundRect.Left) / FScale);
                      Fmy := Round((HiWord(Msg.LParam) + FBoundRect.Top) / FScale);
                    end;
       end;
  end;
  Msg.Result := 0;
end; // TmcmPolygon.WMLButtonDown.


procedure TmcmPolygon.WMLButtonUp(var Msg : TMessage);
begin
  Inherited ;
  if (FLBtnDown)
  then begin
       Changed(Self);
       FHitIndex := THT_NONE;
       FLBtnDown := False;
       if (Parent <> Nil)
       then ; //Invalidate;
  end;
  Msg.Result := 0;
end; // TmcmPolygon.WMLButtonUp.


procedure TmcmPolygon.Paint;
const pi180 = 57.2957795131;
var FCrossWidth : integer;
    cx, cy      : integer;
    dx, dy      : integer;
    //Radius      : integer;
begin
  FCrossWidth := 3;

  if FShowOutline
  then Inherited;

  // Display additional shape features.
  if FShowIndex
  then begin
       // Paint object number.
       {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetBkMode(Canvas.Handle, TRANSPARENT);
       Canvas.Font.Color := RGB(0, 255, 0);
       cx := Round((FBoundRect.Right - FBoundRect.Left) * FScale / 2.0);
       cy := Round((FBoundRect.Bottom - FBoundRect.Top) * FScale / 2.0);
       Canvas.TextOut(cx, cy, IntToStr(FIndex));
  end;

  if FShowConvex 
  then begin
       if (FConvexObj <> Nil)
       then begin
            FConvexObj.Scale := FScale;
            if (FConvexObj.Parent <> Parent)
            then FConvexObj.Parent := Parent;
            ;//FConvexObj.Paint;
       end;
  end;
  if FShowAreaCenter
  then begin
       cx := Round((FCG.x - FBoundRect.Left) * FScale);
       cy := Round((FCG.y - FBoundRect.Top) * FScale);
       dx := Round(FCrossWidth * FScale);
       dy := dx;
       Canvas.MoveTo(cx - dx, cy);
       Canvas.LineTo(cx + dx + 1, cy);
       Canvas.MoveTo(cx, cy - dy);
       Canvas.LineTo(cx, cy + dy + 1);
  end;
  if FShowDensCenter
  then begin
       cx := Round((FDensityGC.x - FBoundRect.Left) * FScale);
       cy := Round((FDensityGC.y - FBoundRect.Top) * FScale);
       dx := Round(FCrossWidth * FScale);
       dy := dx;
       Canvas.MoveTo(cx - dx, cy);
       Canvas.LineTo(cx + dx + 1, cy);
       Canvas.MoveTo(cx, cy - dy);
       Canvas.LineTo(cx, cy + dy + 1);
  end;
  if FShowLumCenter
  then begin
       cx := Round((FLuminanceGC.x - FBoundRect.Left) * FScale);
       cy := Round((FLuminanceGC.y - FBoundRect.Top) * FScale);
       dx := Round(FCrossWidth * FScale);
       dy := dx;
       Canvas.MoveTo(cx - dx, cy);
       Canvas.LineTo(cx + dx + 1, cy);
       Canvas.MoveTo(cx, cy - dy);
       Canvas.LineTo(cx, cy + dy + 1);
  end;
  if FShowMaxFeret
  then begin
       Canvas.MoveTo(Round((FFeretStart.x - FBoundRect.Left) * FScale),
                     Round((FFeretStart.y - FBoundRect.Top) * FScale));
       Canvas.LineTo(Round((FFeretEnd.x - FBoundRect.Left) * FScale),
                     Round((FFeretEnd.y - FBoundRect.Top) * FScale));
  end;
  if FShowMaxRadius and (Parent is TCustomForm)
  then begin
       TCustomForm(Parent).Canvas.Pen.Assign(FShapePen);
       TCustomForm(Parent).Canvas.Brush.Style := bsClear;

       TCustomForm(Parent).Canvas.Ellipse(Round((FChainCG.x - FMaxRadius) * FScale),
                                          Round((FChainCG.y - FMaxRadius) * FScale),
                                          Round((FChainCG.x + FMaxRadius) * FScale) + 1,
                                          Round((FChainCG.y + FMaxRadius) * FScale) + 1);
  end;
  if FShowMinRadius
  then begin
       Canvas.Ellipse(Round((FChainCG.x - FMinRadius - FBoundRect.Left) * FScale),
                      Round((FChainCG.y - FMinRadius - FBoundRect.Top) * FScale),
                      Round((FChainCG.x + FMinRadius - FBoundRect.Left) * FScale) + 1,
                      Round((FChainCG.y + FMinRadius - FBoundRect.Top) * FScale) + 1);
  end;
  if FShowChainCenter or FShowMaxRadius or FShowMinRadius
  then begin
       cx := Round((FChainCG.x - FBoundRect.Left) * FScale);
       cy := Round((FChainCG.y - FBoundRect.Top) * FScale);
       dx := Round(FCrossWidth * FScale);
       dy := dx;
       Canvas.MoveTo(cx - dx, cy);
       Canvas.LineTo(cx + dx + 1, cy);
       Canvas.MoveTo(cx, cy - dy);
       Canvas.LineTo(cx, cy + dy + 1);
  end;
  if FShowLength and (Parent is TCustomForm) //or FShowOrientation
  then begin
       TCustomForm(Parent).Canvas.Pen.Assign(FShapePen);
       TCustomForm(Parent).Canvas.Brush.Style := bsClear;

       dx := Round(FLength * Cos(-FOrientation / pi180) + FTrace^[FLengthStart].x);
       dy := Round(FLength * Sin(-FOrientation / pi180) + FTrace^[FLengthStart].y);
       if ((dx - FBoundRect.Left) >= 0) and ((dy - FBoundRect.Top) >= 0)
       then begin
            TCustomForm(Parent).Canvas.MoveTo(Round((FTrace^[FLengthStart].x) * FScale),
                                              Round((FTrace^[FLengthStart].y) * FScale));
            TCustomForm(Parent).Canvas.LineTo(Round((dx) * FScale),
                                              Round((dy) * FScale));
       end
       else begin
            dx := Round(FTrace^[FLengthEnd].x - FLength * Cos(-FOrientation / pi180));
            dy := Round(FTrace^[FLengthEnd].y - FLength * Sin(-FOrientation / pi180));
            TCustomForm(Parent).Canvas.MoveTo(Round((FTrace^[FLengthEnd].x) * FScale),
                                              Round((FTrace^[FLengthEnd].y) * FScale));
            TCustomForm(Parent).Canvas.LineTo(Round((dx) * FScale),
                                              Round((dy) * FScale));
       end;
  end;
  if FShowBreadth and (Parent is TCustomForm)
  then begin
       TCustomForm(Parent).Canvas.Pen.Assign(FShapePen);
       TCustomForm(Parent).Canvas.Brush.Style := bsClear;

       dx := Round(-FBreadth * Sin(-FOrientation / pi180) + FTrace^[FBreadthStart].x);
       dy := Round(FBreadth * Cos(-FOrientation / pi180) + FTrace^[FBreadthStart].y);
       TCustomForm(Parent).Canvas.MoveTo(Round((FTrace^[FBreadthStart].x) * FScale),
                                         Round((FTrace^[FBreadthStart].y) * FScale));
       TCustomForm(Parent).Canvas.LineTo(Round((dx) * FScale),
                                         Round((dy) * FScale));
  end;
  (*
  if FShowSmallestCircle and FMinCircleCalced and (Parent is TCustomForm)
  then begin
      //{$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.Ellipse((Canvas.Handle,
       Canvas.Pen.Assign(FShapePen);
       Canvas.Brush.Style := bsClear;

       Canvas.Ellipse(Round((FSmallestCircle.Centre.x - FSmallestCircle.Radius - FBoundRect.Left) * FScale),
                      Round((FSmallestCircle.Centre.y - FSmallestCircle.Radius - FBoundRect.Top) * FScale),
                      Round((FSmallestCircle.Centre.x + FSmallestCircle.Radius - FBoundRect.Left) * FScale),
                      Round((FSmallestCircle.Centre.y + FSmallestCircle.Radius - FBoundRect.Top) * FScale));
  end;
  *)
end; // TmcmPolygon.Paint.


procedure TmcmPolygon.SetShapePen(Value : TPen);
begin
  FShapePen.Assign(Value);
end; // TmcmPolygon.SetShapePen.


procedure TmcmPolygon.SetShowAreaCenter(Value : boolean);
begin
  if (FShowAreaCenter <> Value)
  then begin
       FShowAreaCenter := Value;
       Invalidate;
  end;
end; //TmcmPolygon.SetShowAreaCenter.


procedure TmcmPolygon.SetShowBreadth(Value : boolean);
begin
  if (FShowBreadth <> Value)
  then begin
       FShowBreadth := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowBreadth.


procedure TmcmPolygon.SetShowConvex(Value : boolean);
begin
  if (FShowConvex <> Value)
  then begin
       FShowConvex := Value;
       if (FConvexObj <> Nil)
       then begin
            if FShowConvex
            then begin
                 if (FConvexObj.Parent <> Parent)
                 then FConvexObj.Parent := Parent;
            end
            else FConvexObj.Parent := Nil;
       end;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowConvex.


procedure TmcmPolygon.SetShowDensCenter(Value : boolean);
begin
  if (FShowDensCenter <> Value)
  then begin
       FShowDensCenter := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowDensCenter.


procedure TmcmPolygon.SetShowIndex(Value : boolean);
begin
  if (FShowIndex <> Value)
  then begin
       FShowIndex := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowIndex.


procedure TmcmPolygon.SetShowLumCenter(Value : boolean);
begin
  if (FShowLumCenter <> Value)
  then begin
       FShowLumCenter := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowLumCenter.


procedure TmcmPolygon.SetShowLength(Value : boolean);
begin
  if (FShowLength <> Value)
  then begin
       FShowLength := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowLength.


procedure TmcmPolygon.SetShowMaxFeret(Value : boolean);
begin
  if (FShowMaxFeret <> Value)
  then begin
       FShowMaxFeret := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowMaxFeret.


procedure TmcmPolygon.SetShowMaxRadius(Value : boolean);
begin
  if (FShowMaxRadius <> Value)
  then begin
       FShowMaxRadius := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowMaxRadius.


procedure TmcmPolygon.SetShowMinRadius(Value : boolean);
begin
  if (FShowMinRadius <> Value)
  then begin
       FShowMinRadius := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowMinRadius.


procedure TmcmPolygon.SetShowOutline(Value : boolean);
begin
  if (FShowOutline <> Value)
  then begin
       FShowOutline := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowOutline.


procedure TmcmPolygon.SetShowOutlineCenter(Value : boolean);
begin
  if (FShowChainCenter <> Value)
  then begin
       FShowChainCenter := Value;
       Invalidate;
  end;
end; // TmcmPolygon.SetShowOutlineCenter.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}
{$IFDEF BOOLEAN_OFF}{$UNDEF BOOLEAN_OFF}{$B+}{$ENDIF}

end.

