// ------------------------------------------------------------------------------
// DPF.iOS.MKMapView Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.MKMapView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Generics.Collections,
  System.TypInfo,
  System.Math,
  Data.DBXJSON,
{$IFDEF DELPHIXE6}
  System.JSON,
{$ENDIF}
  Data.DB,
  XML.xmldom,
  XML.XMLDoc,
  XML.XMLIntf,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.CoreFoundation,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  IOSapi.CoreLocation,
  FMX.Platform.iOS,
  DPF.iOS.MapKit,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms;

type

  TDPFMapKitMapType = ( mkmtStandard, mkmtSatellite, mkmtTypeHybrid );

  TDPFMapKitUserTrackMode = ( utmNone, utmFollow, utmFollowWithHeading );

{$IFNDEF IOS}
  CLLocationDegrees = Double;

  CLLocationCoordinate2D = record
    latitude: CLLocationDegrees;
    longitude: CLLocationDegrees;
  end;
{$ENDIF}
{$IFDEF IOS}

  TGeoAddressInfo = record
    Location: CLLocationCoordinate2D;
  end;

  TDPFMapView = class;

  // ------------------------------------------------------------------------------
  DPFMKMapView = interface( MKMapView )
    ['{9D39C617-8A12-4B35-A506-97C9D7B7C933}']
{$IFDEF IOS7}
    // function viewForOverlay( overlay: MKOverlay ): MKOverlayView; cdecl;
    // function rendererForOverlay( overlay: MKOverlay ): MKOverlayRenderer; cdecl; // iOS 7.0 and later
{$ENDIF}
    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
  end;

  TDPFMKMapView = class( TOCLocal )
  private
    LastTouchPoint   : NSPoint;
    LastMapPointTouch: MKMapPoint;
  protected
    FDPFMapView: TDPFMapView;
  public
    constructor Create( ADPFMapView: TDPFMapView );
    function GetObjectiveCClass: PTypeInfo; override;

{$IFDEF IOS7}
    // function viewForOverlay( overlay: MKOverlay ): MKOverlayView; cdecl;
    // function rendererForOverlay( overlay: MKOverlay ): MKOverlayRenderer; cdecl; // iOS 7.0 and later
{$ENDIF}
    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  DPFMKTileOverlayRenderer = interface( MKTileOverlayRenderer )
    ['{618391D5-11A7-452A-9FA3-D5892061FDE8}']

    procedure drawMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale; inContext: CGContextRef ); cdecl;
  end;

  TDPFMKTileOverlayRenderer = class( TOCLocal )
  protected
    FDPFMapView: TDPFMapView;
  public
    constructor Create( ADPFMapView: TDPFMapView; AMKTileOverlay: MKTileOverlay );
    function GetObjectiveCClass: PTypeInfo; override;

    procedure drawMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale; inContext: CGContextRef ); cdecl;

  end;

  POverlayInfo = ^TOverlayInfo;

  TOverlayInfo = record
    Title: string;
    SubTitle: string;
    FillColor: TAlphaColor;
    StrokeColor: TAlphaColor;
    LineWidth: Single;
    Alpha: Single;
  end;

  TKPinAnnotationColor = ( pcRed, pcGreen, pcPurple );
  PAnnotationInfo      = ^TAnnotationInfo;

  TAnnotationInfo = record
    PinImage: string;
    LeftCalloutImage: string;
    PinColor: TKPinAnnotationColor;
    Draggable: boolean;
    Alpha: Single;
    ShowCallout: Boolean;
    ShowCalloutButton: Boolean;
    AnimateDrop: Boolean;
    TagStr: string;
  end;

  // ------------------------------------------------------------------------------
  TMKMapViewDelegate = class( TOCLocal, IMKMapViewDelegate )
  private
    FMapView: TDPFMapView;
    //M       : TDPFMKTileOverlayRenderer;
  public
    constructor Create( aMapView: TDPFMapView );

    function mapView( mapView: MKMapView; viewForAnnotation: MKAnnotation ): MKAnnotationView; overload; cdecl;
    function mapView( mapView: MKMapView; viewForOverlay: MKOverlay ): MKOverlayView; overload; cdecl;
    procedure mapView( mapView: MKMapView; regionDidChangeAnimated: Boolean ); overload; cdecl;
    procedure mapView( mapView: MKMapView; didSelectAnnotationView: MKAnnotationView ); overload; cdecl;
    procedure mapView( mapView: MKMapView; annotationView: MKAnnotationView; calloutAccessoryControlTapped: UIControl ); overload; cdecl;

    // function mapView( mapView: MKMapView; rendererForOverlay: Pointer ): MKOverlayRenderer; overload; cdecl;

    procedure mapViewWillStartLocatingUser( mapView: MKMapView ); cdecl;
    procedure mapViewDidStopLocatingUser( mapView: MKMapView ); cdecl;
    procedure mapView( mapView: MKMapView; didUpdateUserLocation: MKUserLocation ); overload; cdecl;

    procedure mapViewWillStartLoadingMap( mapView: MKMapView ); cdecl;
    procedure mapViewDidFinishLoadingMap( mapView: MKMapView ); cdecl;
    procedure mapViewDidFailLoadingMap( mapView: MKMapView; withError: NSError ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  TDPFLocation = record
    altitude: Double;
    latitude: Double;
    longitude: Double;
    course: Double;
    horizontalAccuracy: Double;
    speed: Double;
    timestamp: TDateTime;
    verticalAccuracy: Double;
  end;
  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  TDPFMapViewOnPolygonClick           = procedure( sender: TObject; PolygonView: MKPolygonView; Polygon: MKPolygon ) of object;
  TDPFMapViewOnPolylineClick          = procedure( sender: TObject; PolylineView: MKPolylineView; Polyline: MKPolyline ) of object;
  TDPFMapViewOnCircleClick            = procedure( sender: TObject; CircleView: MKCircleView; Circle: MKCircle ) of object;
  TDPFMapViewOnAnnotationClick        = procedure( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo ) of object;
  TDPFMapViewOnAnnotationCalloutClick = procedure( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo ) of object;
{$ENDIF}
  TDPFMapViewOnZoomChanged           = procedure( sender: TObject ) of object;
  TDPFMapViewOnStartLoadingMap       = procedure( sender: TObject ) of object;
  TDPFMapViewOnDidFinishLoadingMap   = procedure( sender: TObject ) of object;
  TDPFMapViewOnDidFailLoadingMap     = procedure( sender: TObject ) of object;
  TDPFMapViewOnWillStartLocatingUser = procedure( sender: TObject ) of object;
  TDPFMapViewOnDidStopLocatingUser   = procedure( sender: TObject ) of object;
  TDPFMapViewOnDidUpdateUserLocation = procedure( sender: TObject; Location: TDPFLocation ) of object;
  TDPFMapViewOnDrawCustomImage       = procedure( sender: TObject; ZoomLevel: byte; var Image: string; var Alpha: Single ) of object;

  TDPFMapViewTileOverlayType = ( otApple, otGoogle, otOpenStreet );

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFMapView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFMKMapView     : TDPFMKMapView;
    FMKMapView        : MKMapView;
    FMKMapViewDelegate: TMKMapViewDelegate;
    FAnnotationsList  : TDictionary<Pointer, PAnnotationInfo>;
    FOverlaysList     : TDictionary<Pointer, POverlayInfo>;
    FFromRouteLoc     : CLLocationCoordinate2D;
    FToRouteLoc       : CLLocationCoordinate2D;

    FCustomOverlay: MKTileOverlay;

{$ENDIF}
    FBackgroundColor        : TAlphaColor;
    FShowUserLocation       : Boolean;
    FMapType                : TDPFMapKitMapType;
    FZoomEnabled            : Boolean;
    FScrollEnabled          : Boolean;
    FUserTrackingMode       : TDPFMapKitUserTrackMode;
    FZoomLevel              : byte;
    FLastZoomLevel          : byte;
    FOnZoomChanged          : TDPFMapViewOnZoomChanged;
    FOnStartLoadingMap      : TDPFMapViewOnStartLoadingMap;
    FOnDidFinishLoadingMap  : TDPFMapViewOnDidFinishLoadingMap;
    FOnDidFailLoadingMap    : TDPFMapViewOnDidFailLoadingMap;
    FOnWillStartLocatingUser: TDPFMapViewOnWillStartLocatingUser;
    FOnDidStopLocatingUser  : TDPFMapViewOnDidStopLocatingUser;
    FOnDidUpdateUserLocation: TDPFMapViewOnDidUpdateUserLocation;
    FTileOverlayType        : TDPFMapViewTileOverlayType;
    FOnDrawCustomImage      : TDPFMapViewOnDrawCustomImage;
{$IFDEF IOS}
    FOnPolygonClick          : TDPFMapViewOnPolygonClick;
    FOnPolylineClick         : TDPFMapViewOnPolylineClick;
    FOnCircleClick           : TDPFMapViewOnCircleClick;
    FOnAnnotationClick       : TDPFMapViewOnAnnotationClick;
    FOnAnnotationCalloutClick: TDPFMapViewOnAnnotationCalloutClick;
{$ENDIF}
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetShowUserLocation( const Value: Boolean );
    procedure SetMapType( const Value: TDPFMapKitMapType );
    procedure SetZoomEnabled( const Value: Boolean );
    procedure SetScrollEnabled( const Value: Boolean );
    procedure SetUserTrackingMode( const Value: TDPFMapKitUserTrackMode );
    function GetUserLocationVisible: Boolean;
    procedure SetZoomLevel( const Value: byte );
    procedure SetTileOverlayType( const Value: TDPFMapViewTileOverlayType );

  protected

    procedure Resize; override;
    procedure Move; override;
{$IFDEF IOS}
    procedure ParseJSONPairs( jo: TJSONObject );
    procedure ParseJSONArray( ja: TJSONArray );
    function XMLPars( XMLNode: IXMLNode ): TGeoAddressInfo;
    procedure DrawRoute( PolyStr: string; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTilte: string );
{$ELSE}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF IOS}
    procedure Loaded; override;
    procedure ReloadTileOverlay;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFDEF IOS}
    // Return Annotation ID
    function AddAnnotation( Title: string; SubTitle: string; LocationAddress: string; Zoom: byte = 0; TagStr: string = '' ): MKPointAnnotation; overload;
    function AddAnnotation( Title: string; SubTitle: string; Latitude: CLLocationDegrees; longitude: CLLocationDegrees; Zoom: byte = 0; TagStr: string = ''; Draggable: Boolean = false; Alpha: Single = 1.0; PinColor: TKPinAnnotationColor = pcRed; PinImage: string = ''; LeftCalloutImage: string = ''; ShowCallout: Boolean = true;
      ShowCalloutButton: Boolean = false; Animated: Boolean = true; CentreMapToAnnotation: Boolean = true ): MKPointAnnotation; overload;
    procedure RemoveAnnotation( Annotation: MKPointAnnotation );
    procedure RemoveAllAnnotation;

    function AddCircle( Latitude, longitude: CLLocationDegrees; radius: Double; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKCircle;
    procedure RemoveOverlay( overlay: MKOverlay );
    procedure RemoveAllOverlays;

    function AddPolyLine( const Poly: TArray<CLLocationCoordinate2D>; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKPolyline;
    function AddPolygon( const Poly: TArray<CLLocationCoordinate2D>; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKPolygon;

    function GetRoutePointFrom( FromPoint: CLLocationCoordinate2D; ToPoint: CLLocationCoordinate2D ): NSArray;
    function GetLatLngFromAddress( Addr: string ): CLLocationCoordinate2D;
    procedure ShowRoute( FromDir: string; ToDir: string );
    procedure setCenter( Latitude: CLLocationDegrees; longitude: CLLocationDegrees; Animate: Boolean );
    function GetCurrentLocation: TDPFLocation;
{$ENDIF}
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomBy( ZoomChange: Double );
    function PointToCoordinate( APoint: TPointF ): CLLocationCoordinate2D;
    function CoordinateToPoint( ACoordinate: CLLocationCoordinate2D ): TPointF;

  published
    property BackgroundColor    : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property ShowUserLocation   : Boolean read FShowUserLocation write SetShowUserLocation default True;
    property UserLocationVisible: Boolean read GetUserLocationVisible;
    property MapType            : TDPFMapKitMapType read FMapType write SetMapType default mkmtStandard;
    property ZoomEnabled        : Boolean read FZoomEnabled write SetZoomEnabled default True;
    property ScrollEnabled      : Boolean read FScrollEnabled write SetScrollEnabled default True;
    property UserTrackingMode   : TDPFMapKitUserTrackMode read FUserTrackingMode write SetUserTrackingMode default utmNone;
    property ZoomLevel          : byte read FZoomLevel write SetZoomLevel default 14;
    property TileOverlayType    : TDPFMapViewTileOverlayType read FTileOverlayType write SetTileOverlayType default TDPFMapViewTileOverlayType.otApple;

    property OnZoomChanged          : TDPFMapViewOnZoomChanged read FOnZoomChanged write FOnZoomChanged;
    property OnStartLoadingMap      : TDPFMapViewOnStartLoadingMap read FOnStartLoadingMap write FOnStartLoadingMap;
    property OnDidFinishLoadingMap  : TDPFMapViewOnDidFinishLoadingMap read FOnDidFinishLoadingMap write FOnDidFinishLoadingMap;
    property OnDidFailLoadingMap    : TDPFMapViewOnDidFailLoadingMap read FOnDidFailLoadingMap write FOnDidFailLoadingMap;
    property OnWillStartLocatingUser: TDPFMapViewOnWillStartLocatingUser read FOnWillStartLocatingUser write FOnWillStartLocatingUser;
    property OnDidStopLocatingUser  : TDPFMapViewOnDidStopLocatingUser read FOnDidStopLocatingUser write FOnDidStopLocatingUser;
    property OnDidUpdateUserLocation: TDPFMapViewOnDidUpdateUserLocation read FOnDidUpdateUserLocation write FOnDidUpdateUserLocation;
    property OnDrawCustomImage      : TDPFMapViewOnDrawCustomImage read FOnDrawCustomImage write FOnDrawCustomImage;

{$IFDEF IOS}
    property OnPolygonClick          : TDPFMapViewOnPolygonClick read FOnPolygonClick write FOnPolygonClick;
    property OnPolylineClick         : TDPFMapViewOnPolylineClick read FOnPolylineClick write FOnPolylineClick;
    property OnCircleClick           : TDPFMapViewOnCircleClick read FOnCircleClick write FOnCircleClick;
    property OnAnnotationClick       : TDPFMapViewOnAnnotationClick read FOnAnnotationClick write FOnAnnotationClick;
    property OnAnnotationCalloutClick: TDPFMapViewOnAnnotationCalloutClick read FOnAnnotationCalloutClick write FOnAnnotationCalloutClick;
{$ENDIF}
    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------

implementation

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function MKMapPointMake( x, y: double ): MKMapPoint;
begin
  result.x := x;
  result.y := y;
end;

// ------------------------------------------------------------------------------
function distanceOfPoint( pt: MKMapPoint; Poly: MKPolyline ): Double;
var
  u, distance   : double;
  n             : Integer;
  ptA, ptB      : MKMapPoint;
  xDelta, yDelta: double;
  ptClosest     : MKMapPoint;
  P             : PMKMapPoint;
  pnt           : MKMapPoint;
begin
  distance := MaxDouble;
  P        := poly.points;
  for n    := 0 to Poly.pointCount - 1 do
  begin
    Move( P^, pnt, sizeof( MKMapPoint ) );
    ptA := pnt;
    inc( p );
    Move( P^, pnt, sizeof( MKMapPoint ) );
    ptB := pnt;

    xDelta := ptB.x - ptA.x;
    yDelta := ptB.y - ptA.y;

    if ( xDelta = 0.0 ) and ( yDelta = 0.0 ) then
    begin
      // Points must not be equal
      continue;
    end;

    u := ( ( pt.x - ptA.x ) * xDelta + ( pt.y - ptA.y ) * yDelta ) / ( xDelta * xDelta + yDelta * yDelta );

    if ( u < 0.0 ) then
    begin
      ptClosest := ptA;
    end
    else if ( u > 1.0 ) then
    begin

      ptClosest := ptB;
    end
    else
    begin

      ptClosest := MKMapPointMake( ptA.x + u * xDelta, ptA.y + u * yDelta );
    end;

    distance := MIN( distance, MKMetersBetweenMapPoints( ptClosest, pt ) );
  end;

  result := distance;
end;

// ------------------------------------------------------------------------------
function metersFromPixel( mapView: MKMapView; px: NSUInteger; pt: CGPoint ): double;
var
  ptB           : CGPoint;
  coordA, coordB: CLLocationCoordinate2D;
begin
  ptB := CGPointMake( pt.x + px, pt.y );

  coordA := mapView.convertPoint( pt, mapView );
  coordB := mapView.convertPoint( ptB, mapView );

  result := MKMetersBetweenMapPoints( MKMapPointForCoordinate( coordA ), MKMapPointForCoordinate( coordB ) );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFMapView }
// ------------------------------------------------------------------------------
constructor TDPFMapView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption    := 'MapView';
  FZoomLevel        := 14;
  FLastZoomLevel    := 0;
  FBackgroundColor  := TAlphaColors.Null;
  FShowUserLocation := True;
  FMapType          := mkmtStandard;
  FZoomEnabled      := True;
  FScrollEnabled    := True;
  FUserTrackingMode := utmNone;
  FTileOverlayType  := TDPFMapViewTileOverlayType.otApple;
{$IFDEF IOS}
  FCustomOverlay     := nil;
  FAnnotationsList   := TDictionary<Pointer, PAnnotationInfo>.Create;
  FOverlaysList      := TDictionary<Pointer, POverlayInfo>.Create;
  FMKMapViewDelegate := TMKMapViewDelegate.Create( Self );
  // FMKMapView         := TMKMapView.Wrap( TMKMapView.alloc.initWithFrame( CGRectMake( 0, 0, 100, 100 ) ) );
  FDPFMKMapView := TDPFMKMapView.Create( self );

  // FMKMapView    := TMKMapView.Wrap( FDPFMKMapView.Super.init );
  FMKMapView := TMKMapView.Wrap( ( FDPFMKMapView as ILocalObject ).GetObjectID ); // SZ
  FMKMapView.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );

  FUIControl := FMKMapView;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFMapView.Destroy;
{$IFDEF IOS}
var
  ArrOverlays: TArray<Pointer>;
  I          : Integer;
{$ENDIF}
begin
{$IFDEF IOS}
  FMKMapView.setDelegate( nil );

  TNSObject.Wrap( ( FMKMapViewDelegate as ILocalObject ).GetObjectID ).release; // SZ: Fix memory leak

  if FMKMapViewDelegate <> nil then
    FMKMapViewDelegate.DisposeOf;

  if FAnnotationsList <> nil then
  begin
    ArrOverlays := FAnnotationsList.Keys.ToArray;
    for I       := 0 to high( ArrOverlays ) do
      RemoveAnnotation( TMKPointAnnotation.Wrap( ArrOverlays[I] ) );
    FAnnotationsList.Clear;

    FAnnotationsList.DisposeOf;
  end;

  if FOverlaysList <> nil then
  begin
    ArrOverlays := FOverlaysList.Keys.ToArray;
    for I       := 0 to high( ArrOverlays ) do
      RemoveOverlay( TMKOverlay.Wrap( ArrOverlays[I] ) );
    FOverlaysList.Clear;

    FOverlaysList.DisposeOf;
  end;
{$ENDIF}
  inherited;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFMapView.AddCircle( Latitude, longitude: CLLocationDegrees; radius: Double; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKCircle;
var
  Coordinate: CLLocationCoordinate2D;
  PCinfo    : POverlayInfo;
begin
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    Coordinate.Latitude  := Latitude;
    Coordinate.longitude := longitude;
    result               := TMKCircle.Wrap( TMKCircle.OCClass.circleWithCenterCoordinate( Coordinate, radius ) );
    result.setTitle( NSStr( Title ) );
    result.setSubtitle( NSStr( subTitle ) );

    result.retain;
    FMKMapView.addOverlay( Result );

    new( PCinfo );
    PCinfo^.Title       := Title;
    PCinfo^.SubTitle    := SubTitle;
    PCinfo^.FillColor   := FillColor;
    PCinfo^.StrokeColor := StrokeColor;
    PCinfo^.LineWidth   := LineWidth;
    PCinfo^.Alpha       := Alpha;
    FOverlaysList.AddOrSetValue( ( result as ILocalObject ).GetObjectID, PCinfo );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.setCenter( Latitude: CLLocationDegrees; longitude: CLLocationDegrees; Animate: Boolean );
var
  centerCoordinate: CLLocationCoordinate2D;
begin
  centerCoordinate.latitude  := Latitude;
  centerCoordinate.longitude := longitude; // Fixed by Paul
  FMKMapView.setCenterCoordinate( centerCoordinate, Animate );
end;

// ------------------------------------------------------------------------------
function TDPFMapView.GetCurrentLocation: TDPFLocation;
begin
  if FMKMapView.userLocation.location <> nil then // Fixed by Paul
  begin
    Result.latitude           := FMKMapView.userLocation.location.coordinate.latitude;
    Result.longitude          := FMKMapView.userLocation.location.coordinate.longitude;
    Result.altitude           := FMKMapView.userLocation.location.altitude;
    Result.speed              := FMKMapView.userLocation.location.speed;
    Result.course             := FMKMapView.userLocation.location.course;
    Result.timestamp          := NSDateToDateTime( FMKMapView.userLocation.location.timestamp );
    Result.horizontalAccuracy := FMKMapView.userLocation.location.horizontalAccuracy;
    Result.verticalAccuracy   := FMKMapView.userLocation.location.verticalAccuracy;
  end;
end;

// ------------------------------------------------------------------------------
function TDPFMapView.AddAnnotation( Title: string; SubTitle: string; LocationAddress: string; Zoom: Byte = 0; TagStr: string = '' ): MKPointAnnotation;
var
  GeoAddr: TGeoAddressInfo;
begin
  GeoAddr.Location := GetLatLngFromAddress( LocationAddress );
  result           := AddAnnotation( Title, SubTitle, GeoAddr.Location.latitude, GeoAddr.Location.longitude, Zoom, TagStr );
end;

// ------------------------------------------------------------------------------
function TDPFMapView.AddAnnotation( Title: string; SubTitle: string; Latitude: CLLocationDegrees; longitude: CLLocationDegrees; Zoom: byte = 0; TagStr: string = ''; Draggable: Boolean = false; Alpha: Single = 1.0; PinColor: TKPinAnnotationColor = pcRed; PinImage: string = ''; LeftCalloutImage: string = ''; ShowCallout: Boolean = true;
  ShowCalloutButton: Boolean = false; Animated: Boolean = true; CentreMapToAnnotation: Boolean = true ): MKPointAnnotation;
var
  Coordinate: CLLocationCoordinate2D;
  value     : PAnnotationInfo;
begin
  if FMKMapView <> nil then
  begin
    Result               := TMKPointAnnotation.Wrap( TMKPointAnnotation.Alloc.init );
    Coordinate.Latitude  := Latitude;
    Coordinate.longitude := longitude;
    Result.setTitle( NSStr( Title ) );
    Result.setSubTitle( NSStr( SubTitle ) );
    Result.setCoordinate( Coordinate );

    FMKMapView.setCenterCoordinate( Result.Coordinate, True );

    if Zoom <> 0 then
      setCenterCoordinate( FMKMapView, Result.Coordinate, Zoom, True );

    new( value );
    value^.PinImage          := PinImage;
    value^.LeftCalloutImage  := LeftCalloutImage;
    value^.PinColor          := PinColor;
    value^.Draggable         := Draggable;
    value^.Alpha             := Alpha;
    value^.ShowCallout       := ShowCallout;
    value^.ShowCalloutButton := ShowCalloutButton;
    value^.TagStr            := TagStr;
    value^.AnimateDrop       := ( Animated ) and ( PinImage = '' ); // By Paul
    FAnnotationsList.AddOrSetValue( ( Result as ILocalObject ).GetObjectID, Value );

    // By Paul
    FMKMapView.addAnnotation( Result );
    if ( CentreMapToAnnotation ) or ( Zoom <> 0 ) then
    begin
      if Zoom <> 0 then
        setCenterCoordinate( FMKMapView, Result.Coordinate, Zoom, Animated )
      else
        FMKMapView.setCenterCoordinate( Result.Coordinate, Animated );
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.RemoveAnnotation( Annotation: MKPointAnnotation );
var
  value: PAnnotationInfo;
  p    : Pointer;
begin
  if assigned( FMKMapView ) and assigned( Annotation ) and ( FAnnotationsList.Count > 0 ) then
  begin
    P := ( Annotation as ILocalObject ).GetObjectID;
    if FAnnotationsList.ContainsKey( P ) then
    begin
      FAnnotationsList.TryGetValue( ( Annotation as ILocalObject ).GetObjectID, value );
      Dispose( value );
      FMKMapView.removeAnnotation( Annotation );
      FAnnotationsList.Remove( P );
      Annotation.release;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.RemoveAllAnnotation;
var
  I          : integer;
  ArrOverlays: TArray<Pointer>;

begin
  if FAnnotationsList <> nil then
  begin
    ArrOverlays := FAnnotationsList.Keys.ToArray;
    for I       := 0 to high( ArrOverlays ) do
      RemoveAnnotation( TMKPointAnnotation.Wrap( ArrOverlays[I] ) );
    FAnnotationsList.Clear;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.RemoveOverlay( overlay: MKOverlay );
var
  value: POverlayInfo;
  p    : Pointer;
begin
  if assigned( FMKMapView ) and assigned( overlay ) and ( FOverlaysList.Count > 0 ) then
  begin
    P := ( Overlay as ILocalObject ).GetObjectID;
    if FOverlaysList.ContainsKey( P ) then
    begin
      FOverlaysList.TryGetValue( ( Overlay as ILocalObject ).GetObjectID, value );
      Dispose( value );
      FMKMapView.removeOverlay( overlay );
      FOverlaysList.Remove( P );
      overlay.release;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.RemoveAllOverlays;
var
  I          : integer;
  ArrOverlays: TArray<Pointer>;

begin
  if FOverlaysList <> nil then
  begin
    ArrOverlays := FOverlaysList.Keys.ToArray;
    for I       := 0 to high( ArrOverlays ) do
      RemoveOverlay( TMKOverlay.Wrap( ArrOverlays[I] ) );
    FOverlaysList.Clear;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFMapView.GetUserLocationVisible: Boolean;
begin
  Result := False;
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    // Fixed by Paul
    Result := ( FMKMapView.userLocation.location <> nil ) and ( FMKMapView.UserLocationVisible );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFMapView.Loaded;
begin

  FMKMapView.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  FMKMapView.setHidden( not Visible );
  SetShowUserLocation( FShowUserLocation );

  SetMapType( FMapType );
  SetZoomEnabled( FZoomEnabled );
  SetScrollEnabled( FScrollEnabled );
  SetUserTrackingMode( FUserTrackingMode );

  if FZoomLevel = 0 then
    FZoomLevel := DPF.iOS.MapKit.GetZoomLevel( FMKMapView );

  SetZoomLevel( FZoomLevel );
  SetBackgroundColor( FBackgroundColor );

  FMKMapView.setDelegate( FMKMapViewDelegate.GetObjectID );

  ReloadTileOverlay;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ReloadTileOverlay;
begin
  if not assigned( FMKMapView ) then
    exit;

  if assigned( FCustomOverlay ) then
  begin
    FMKMapView.removeOverlay( FCustomOverlay );
    FCustomOverlay.release;
  end;

  if FTileOverlayType = otApple then
  begin
    FCustomOverlay := nil;
  end
  else if FTileOverlayType = otGoogle then
  begin
    FCustomOverlay := TMKTileOverlay.Wrap( TMKTileOverlay.alloc.initWithURLTemplate( NSStr( 'http://mt0.google.com/vt/x={x}&y={y}&z={z}' ) ) );
  end
  else if FTileOverlayType = otOpenStreet then
  begin
    FCustomOverlay := TMKTileOverlay.Wrap( TMKTileOverlay.alloc.initWithURLTemplate( NSStr( 'http://tile.openstreetmap.org/{z}/{x}/{y}.png' ) ) );
  end;

  if assigned( FOnDrawCustomImage ) and not assigned( FCustomOverlay ) then
  begin
    FCustomOverlay := TMKTileOverlay.Wrap( TMKTileOverlay.Alloc.initWithURLTemplate( NSStr( 'http://tile.openstreetmap.org/1/1/1.png' ) ) );
  end;

  if assigned( FCustomOverlay ) then
  begin
    FCustomOverlay.setCanReplaceMapContent( true );
    FMKMapView.addOverlay( FCustomOverlay, MKOverlayLevelAboveLabels );
  end;
  FMKMapView.setNeedsLayout;
  FMKMapView.setNeedsDisplay;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFMapView.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FMKMapView.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FMKMapView.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetMapType( const Value: TDPFMapKitMapType );
begin
  FMapType := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    FMKMapView.setMapType( Integer( FMapType ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetScrollEnabled( const Value: Boolean );
begin
  FScrollEnabled := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    FMKMapView.setScrollEnabled( FScrollEnabled )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetShowUserLocation( const Value: Boolean );
begin
  FShowUserLocation := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    FMKMapView.setShowsUserLocation( FShowUserLocation )
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetTileOverlayType( const Value: TDPFMapViewTileOverlayType );
begin
  FTileOverlayType := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
    ReloadTileOverlay;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetUserTrackingMode( const Value: TDPFMapKitUserTrackMode );
begin
  FUserTrackingMode := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
    FMKMapView.setUserTrackingMode( Integer( FUserTrackingMode ), True );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetZoomEnabled( const Value: Boolean );
begin
  FZoomEnabled := Value;
{$IFDEF IOS}
  if FMKMapView <> nil then
    FMKMapView.setZoomEnabled( FZoomEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.SetZoomLevel( const Value: byte );
begin
  if ( Value < 2 ) or ( Value > 20 ) then
    exit;

{$IFDEF IOS}
  if FMKMapView <> nil then
    if not setCenterCoordinate( FMKMapView, FMKMapView.centerCoordinate, Value, True ) then
      exit;
{$ENDIF}
  FZoomLevel := Value;
end;

// ------------------------------------------------------------------------------
// by Paul
procedure TDPFMapView.ZoomBy( ZoomChange: Double );
{$IFDEF IOS}
var
  newRegion: MKCoordinateRegion;
{$ENDIF}
begin
{$IFDEF IOS}
  if FMKMapView <> nil then
  begin
    newRegion.center              := FMKMapView.region.center;
    newRegion.span.latitudeDelta  := Min( FMKMapView.region.span.latitudeDelta / ZoomChange, 180.0 );
    newRegion.span.longitudeDelta := Min( FMKMapView.region.span.longitudeDelta / ZoomChange, 180.0 );
    FMKMapView.setRegion( newRegion, True );
    FMKMapView.regionThatFits( newRegion );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ZoomIn;
begin
{$IFDEF IOS}
  ZoomBy( 1.2 ); // by Paul
  (*
    if FMKMapView <> nil then
    begin
    setCenterCoordinate( FMKMapView, FMKMapView.region.center, 10, True );
    end;
  *)
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ZoomOut;
{$IFDEF IOS}
// var newRegion: MKCoordinateRegion;
{$ENDIF}
begin
{$IFDEF IOS}
  ZoomBy( 0.8 ); // By Paul
  (*
    if FMKMapView <> nil then
    begin
    newRegion.center              := FMKMapView.region.center;
    newRegion.span.latitudeDelta  := Min( FMKMapView.region.span.latitudeDelta / 0.8, 180.0 );
    newRegion.span.longitudeDelta := Min( FMKMapView.region.span.longitudeDelta / 0.8, 180.0 );
    FMKMapView.setRegion( newRegion, True );
    FMKMapView.regionThatFits( newRegion );
    end;
  *)
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFMapView.Paint;
var
  x, r: integer;
  Rect: TRectF;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  Canvas.Fill.Color := $FFF0EBD3;
  Canvas.Fill.Kind  := TBrushKind.Solid;
  Canvas.FillRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
  r := 50;
  if Min( Width, Height ) < 50 then
    r                     := Round( Max( 5, Min( Width, Height ) / 3 ) );
  Canvas.Stroke.Color     := $FFE0D9BC;
  Canvas.Stroke.Cap       := TStrokeCap.Round;
  Canvas.Stroke.Join      := TStrokeJoin.Round;
  Canvas.Stroke.Dash      := TStrokeDash.Solid;
  Canvas.Stroke.Thickness := 1;
  // Vertical Lines
  x := 10;
  while x < Width do
  begin
    Canvas.DrawLine( PointF( x, 0 ), PointF( x, Height ), 1 );
    inc( x, r );
  end;
  // Horizontal Lines
  x := 10;
  while x < Height do
  begin
    Canvas.DrawLine( PointF( 0, x ), PointF( Width, x ), 1 );
    inc( x, r );
  end;
  // TrackingDot
  BitmapToPosition( Self, iOS_GUI_Bitmaps.MapView.TrackingDot, Width / 2 - iOS_GUI_Bitmaps.MapView.TrackingDot.Width / 2, Height / 2 - iOS_GUI_Bitmaps.MapView.TrackingDot.Height / 2 );
  BitmapToPosition( Self, iOS_GUI_Bitmaps.MapView.TrackingDotHalo, Width / 2 - iOS_GUI_Bitmaps.MapView.TrackingDotHalo.Width / 2, Height / 2 - iOS_GUI_Bitmaps.MapView.TrackingDotHalo.Height / 2 );
  // Component Name
  Rect              := ClipRect;
  Rect.Bottom       := Rect.Bottom / 2;
  Canvas.Font.Size  := 14;
  Canvas.Fill.Color := TAlphaColors.Black;
  Canvas.FillText( Rect, name, True, 1, [], TTextAlign.Center, TTextAlign.Center );
  Canvas.EndScene;
end;

{$ENDIF}

function TDPFMapView.PointToCoordinate( APoint: TPointF ): CLLocationCoordinate2D;
{$IFDEF IOS}
var
  Pt: NSPoint;
{$ENDIF}
begin
{$IFDEF IOS}
  PT.x   := APoint.X;
  PT.y   := APoint.Y;
  Result := FMKMapView.convertPoint( Pt, FDPFMKMapView.FDPFMapView.FMKMapView );
{$ELSE}
  Result.latitude  := 0.0;
  Result.longitude := 0.0;
{$ENDIF}
end;

function TDPFMapView.CoordinateToPoint( ACoordinate: CLLocationCoordinate2D ): TPointF;
{$IFDEF IOS}
var
  Pt: NSPoint;
{$ENDIF}
begin
{$IFDEF IOS}
  Pt       := FMKMapView.convertCoordinate( ACoordinate, FDPFMKMapView.FDPFMapView.FMKMapView );
  Result.X := Pt.x;
  Result.Y := Pt.y;
{$ELSE}
  Result := PointF( 0, 0 );
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFMapView.XMLPars( XMLNode: IXMLNode ): TGeoAddressInfo;
var
  i: integer;
  // lat, lng: string;
  S: string;
begin
  if ( XMLNode.NodeType = ntText ) then
  begin
    S := XMLNode.NodeValue;
  end
  else
  begin
    if sameText( XMLNode.NodeName, 'geometry' ) then
    begin
      S := XMLNode.ChildNodes.Nodes[0].NodeName;
      S := XMLNode.ChildNodes.Nodes[0].ChildNodes.Nodes[0].NodeName;

      Result.Location.latitude  := XMLNode.ChildNodes.Nodes[0].ChildNodes.Nodes[0].NodeValue;
      Result.Location.longitude := XMLNode.ChildNodes.Nodes[0].ChildNodes.Nodes[1].NodeValue;
      exit;
    end;
  end;
  for i := 0 to XMLNode.ChildNodes.Count - 1 do
  begin
    if Result.Location.longitude <> 0 then
      exit;
    Result := XMLPars( XMLNode.ChildNodes.Nodes[i] );
    if Result.Location.longitude <> 0 then
      exit;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ParseJSONArray( ja: TJSONArray );
var
  i: Integer;
  S: string;
begin
  for i := 0 to ja.Count - 1 do
  begin
    if ja.Items[ i ] is TJSONArray then
    begin
      ParseJSONArray( ja.Items[ i ] as TJSONArray );
    end
    else if ja.Items[ i ] is TJSONObject then
    begin
      ParseJSONPairs( ja.Items[ i ] as TJSONObject );
    end
    else
    begin
      S := ja.Items[ i ].Value;
      if SameText( S, 'points' ) then
        DrawRoute( S, TAlphaColors.Red, TAlphaColors.Black, 4, 0.7, '', '' );
    end
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ParseJSONPairs( jo: TJSONObject );
var
  i    : Integer;
  jv   : TJSONValue;
  S, S2: string;
begin
  for i := 0 to jo.Count - 1 do
  begin
    jv := jo.Pairs[ i ].JsonValue;
    S2 := jo.Pairs[ i ].JsonString.Value;
    if sametext( S2, 'overview_polyline' ) then
    begin
      S2 := jo.Pairs[ i ].JsonString.Value;
      Continue;
    end;

    if jv is TJSONOBject then
    begin
      ParseJSONPairs( jv as TJSONObject );
    end
    else if jv is TJSONArray then
    begin
      ParseJSONArray( jv as TJSONArray );
    end
    else
    begin
      S := jo.Pairs[ i ].JsonString.Value;
      if SameText( S, 'points' ) then
      begin
        S := jo.Pairs[ i ].JsonValue.Value;
        DrawRoute( S, TAlphaColors.Red, TAlphaColors.Black, 4, 0.7, '', '' );
      end;
    end
  end;
end;

// ------------------------------------------------------------------------------
function TDPFMapView.AddPolyLine( const Poly: TArray<CLLocationCoordinate2D>; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKPolyline;
var
  PCinfo: POverlayInfo;
begin
  result := TMKPolyline.OCClass.polylineWithCoordinates( @Poly[0], Length( Poly ) );
  result.setTitle( NSStr( Title ) );
  result.setSubtitle( NSStr( SubTitle ) );
  result.retain;
  FMKMapView.addOverlay( result );

  new( PCinfo );
  PCinfo^.Title       := Title;
  PCinfo^.SubTitle    := SubTitle;
  PCinfo^.FillColor   := FillColor;
  PCinfo^.StrokeColor := StrokeColor;
  PCinfo^.LineWidth   := LineWidth;
  PCinfo^.Alpha       := Alpha;
  FOverlaysList.AddOrSetValue( ( result as ILocalObject ).GetObjectID, PCinfo );

  FMKMapView.setVisibleMapRect( result.boundingMapRect );

  if FFromRouteLoc.latitude = 1000 then
  begin
    FFromRouteLoc.latitude  := Poly[0].latitude;
    FFromRouteLoc.longitude := Poly[0].longitude;
  end
  else
  begin
    FToRouteLoc.latitude  := Poly[high( Poly )].latitude;
    FToRouteLoc.longitude := Poly[high( Poly )].longitude;
  end
end;

// ------------------------------------------------------------------------------
function TDPFMapView.AddPolygon( const Poly: TArray<CLLocationCoordinate2D>; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTitle: string ): MKPolygon;
var
  PCinfo: POverlayInfo;
begin
  result := TMKPolygon.OCClass.polygonWithCoordinates( @Poly[0], Length( Poly ) );
  result.setTitle( NSStr( Title ) );
  result.setSubtitle( NSStr( SubTitle ) );
  result.retain;
  FMKMapView.addOverlay( result );

  new( PCinfo );
  PCinfo^.Title       := Title;
  PCinfo^.SubTitle    := SubTitle;
  PCinfo^.FillColor   := FillColor;
  PCinfo^.StrokeColor := StrokeColor;
  PCinfo^.LineWidth   := LineWidth;
  PCinfo^.Alpha       := Alpha;
  FOverlaysList.AddOrSetValue( ( result as ILocalObject ).GetObjectID, PCinfo );

  FMKMapView.setVisibleMapRect( result.boundingMapRect );
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.DrawRoute( PolyStr: string; FillColor: TAlphaColor; StrokeColor: TAlphaColor; LineWidth: Single; Alpha: Single; Title: string; SubTilte: string );
var
  Poly: TArray<CLLocationCoordinate2D>;
begin

  Poly := PolyDecode( PolyStr );
  AddPolyLine( Poly, FillColor, StrokeColor, LineWidth, Alpha, Title, SubTilte );
end;

// ------------------------------------------------------------------------------
procedure TDPFMapView.ShowRoute( FromDir: string; ToDir: string );
var
  escaped_address1, escaped_address2: NSString;
  requestString                     : string;
  url                               : NSURL;
  _result                           : NSString;
  jo                                : TJSONObject;
  Data                              : string;
begin

  escaped_address1 := NSStr( FromDir ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding );
  escaped_address2 := NSStr( ToDir ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding );
  requestString    := Format( 'http://maps.googleapis.com/maps/api/directions/json?origin=%s&destination=%s&sensor=false&dirflg=w&mode=driving', [escaped_address1.UTF8String, escaped_address2.UTF8String] );
  url              := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( requestString ) ) );
  _result          := TNSString.Wrap( TNSString.OCClass.stringWithContentsOfURL( url, NSUTF8StringEncoding, nil ) );

  Data := UTF8ToString( _result.UTF8String );
  if Data = '' then
    exit;

  FFromRouteLoc.latitude  := 1000;
  FFromRouteLoc.longitude := 1000;
  FToRouteLoc.latitude    := 1000;
  FToRouteLoc.longitude   := 1000;

  jo := TJSONObject.ParseJSONValue( TEncoding.UTF8.GetBytes( Data ), 0, true ) as TJSONObject;
  ParseJSONPairs( jo );

  if FFromRouteLoc.latitude <> 1000 then
    AddAnnotation( FromDir, '', FFromRouteLoc.latitude, FFromRouteLoc.longitude );

  if FToRouteLoc.latitude <> 1000 then
    AddAnnotation( ToDir, '', FToRouteLoc.latitude, FToRouteLoc.longitude );

end;

// ------------------------------------------------------------------------------
function TDPFMapView.GetLatLngFromAddress( Addr: string ): CLLocationCoordinate2D;
var
  escaped_address: NSString;
  requestString  : string;
  url            : NSURL;
  _result        : NSString;
  Doc            : IXMLDocument;
begin

  escaped_address := NSStr( Addr ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding );
  requestString   := Format( 'https://maps.googleapis.com/maps/api/geocode/xml?address=%s&sensor=false', [escaped_address.UTF8String] );
  url             := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( requestString ) ) );
  _result         := TNSString.Wrap( TNSString.OCClass.stringWithContentsOfURL( url, NSUTF8StringEncoding, nil ) );

  Doc    := LoadXMLData( UTF8ToString( _result.UTF8String ) );
  Result := XMLPars( Doc.DocumentElement ).Location;

end;

// ------------------------------------------------------------------------------
function TDPFMapView.GetRoutePointFrom( FromPoint: CLLocationCoordinate2D; ToPoint: CLLocationCoordinate2D ): NSArray;
var
  // saddr        : NSString;
  // daddr        : NSString;
  // apiUrlStr    : NSString;
  S1, S2     : string;
  apiUrl     : NSURL;
  error      : {$IFDEF DELPHIXE5}PPointer{$ELSE}NSError{$ENDIF};
  apiResponse: NSString;
  // encodedPoints: TNSString;
begin

  S1 := Format( '%f,%f', [FromPoint.latitude, FromPoint.longitude] );
  S2 := Format( '%f,%f', [ToPoint.latitude, ToPoint.longitude] );

  S1 := Format( 'http://maps.google.com/?addr=%s&daddr=%s', [S1, S2] );

  apiUrl := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( S1 ) ) );

  error       := nil;
  apiResponse := TNSString.Wrap( TNSString.OCClass.stringWithContentsOfURL( apiUrl, NSUTF8StringEncoding, error ) );
  // encodedPoints:= TNSString.Wrap( apiResponse ).stringByMatching:@"points:\\\"([^\\\"]*)\\\"" capture:1L];

  // return [self decodePolyLine:[encodedPoints mutableCopy]];
end;

// ----------------------------------------------------------------------------
{ TMKMapViewDelegate }
constructor TMKMapViewDelegate.Create( aMapView: TDPFMapView );
begin
  inherited Create;
  FMapView := aMapView;
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapView( mapView: MKMapView; annotationView: MKAnnotationView; calloutAccessoryControlTapped: UIControl ); cdecl;
var
  value: PAnnotationInfo;
begin
  if assigned( FMapView.FOnAnnotationCalloutClick ) then
  begin
    value := nil;
    if FMapView.FAnnotationsList.ContainsKey( ( annotationView.annotation as ILocalObject ).GetObjectID ) then
    begin
      FMapView.FAnnotationsList.TryGetValue( ( annotationView.annotation as ILocalObject ).GetObjectID, value );
    end;

    FMapView.FOnAnnotationCalloutClick( FMapView, annotationView, annotationView.annotation, Value );
  end;
end;

// ----------------------------------------------------------------------------
function TMKMapViewDelegate.mapView( mapView: MKMapView; viewForAnnotation: MKAnnotation ): MKAnnotationView; cdecl;
const
  ReuseID = 'DPFMKAnnotation';
var
  value         : PAnnotationInfo;
  AnnotationView: MKPinAnnotationView;
  P             : Pointer;
  iv            : UIImageView;
  //s             : string;
  Img           : UIImage;
begin
  result := nil;
  if viewForAnnotation.isKindOfClass( objc_getClass( 'MKUserLocation' ) ) then
    Exit;

  if FMapView.FAnnotationsList.ContainsKey( ( viewForAnnotation as ILocalObject ).GetObjectID ) then
  begin
    FMapView.FAnnotationsList.TryGetValue( ( viewForAnnotation as ILocalObject ).GetObjectID, value );
    P := FMapView.FMKMapView.dequeueReusableAnnotationViewWithIdentifier( NSStr( reuseId ) );
    if not Assigned( P ) then
    begin
      AnnotationView := TMKPinAnnotationView.Wrap( TMKPinAnnotationView.Alloc.initWithAnnotation( viewForAnnotation, NSStr( ReuseID ) ) )
    end
    else
    begin
      AnnotationView := TMKPinAnnotationView.Wrap( P );
    end;

    AnnotationView.setAnnotation( viewForAnnotation );
    AnnotationView.setAlpha( value^.Alpha );
    AnnotationView.setDraggable( value^.Draggable );
    AnnotationView.setAnimatesDrop( value^.AnimateDrop ); // By Paul

    AnnotationView.setCanShowCallout( value^.ShowCallout );
    if value^.ShowCalloutButton then
      AnnotationView.setRightCalloutAccessoryView( TUIButton.Wrap( TUIButton.OCClass.buttonWithType( UIButtonTypeDetailDisclosure ) ) );

    if Value^.LeftCalloutImage <> '' then
    begin
      if AnnotationView.leftCalloutAccessoryView = nil then
        iv := TUIImageView.Wrap( TUIImageView.Alloc.initWithFrame( AnnotationView.frame ) )
      else
        iv := TUIImageView.Wrap( ( AnnotationView.leftCalloutAccessoryView as ILocalObject ).GetObjectID );
      iv.setImage( TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( Value^.LeftCalloutImage ) ) ) );
      iv.setNeedsDisplay;
      AnnotationView.setLeftCalloutAccessoryView( iv );
    end;

    if value^.PinImage <> '' then
    begin
      Img := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( value^.PinImage ) ) );
      if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
        AnnotationView.setImage( Img )
      else
        AnnotationView.setImage( nil );
    end
    else
    begin
      AnnotationView.setImage( nil );
      AnnotationView.setPinColor( Integer( value^.PinColor ) );
    end;

    AnnotationView.setEnabled( true );
    AnnotationView.setNeedsDisplay;
    result := AnnotationView;
  end;
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapViewWillStartLocatingUser( mapView: MKMapView ); cdecl;
begin
  if assigned( FMapView.FOnWillStartLocatingUser ) then
    FMapView.FOnWillStartLocatingUser( FMapView );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapViewDidStopLocatingUser( mapView: MKMapView ); cdecl;
begin
  if assigned( FMapView.FOnDidStopLocatingUser ) then
    FMapView.FOnDidStopLocatingUser( FMapView );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapView( mapView: MKMapView; didUpdateUserLocation: MKUserLocation ); cdecl;
var
  Location: TDPFLocation;
begin
  Location.altitude           := didUpdateUserLocation.location.altitude;
  Location.latitude           := didUpdateUserLocation.location.coordinate.latitude;
  Location.longitude          := didUpdateUserLocation.location.coordinate.longitude;
  Location.course             := didUpdateUserLocation.location.course;
  Location.horizontalAccuracy := didUpdateUserLocation.location.horizontalAccuracy;
  Location.speed              := didUpdateUserLocation.location.speed;
  Location.timestamp          := NSDateToDateTime( didUpdateUserLocation.location.timestamp );
  Location.verticalAccuracy   := didUpdateUserLocation.location.verticalAccuracy;
  if assigned( FMapView.FOnDidUpdateUserLocation ) then
    FMapView.FOnDidUpdateUserLocation( FMapView, Location );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapViewWillStartLoadingMap( mapView: MKMapView ); cdecl;
begin
  if assigned( FMapView.FOnStartLoadingMap ) then
    FMapView.FOnStartLoadingMap( FMapView );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapViewDidFinishLoadingMap( mapView: MKMapView ); cdecl;
begin
  if assigned( FMapView.FOnDidFinishLoadingMap ) then
    FMapView.FOnDidFinishLoadingMap( FMapView );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapViewDidFailLoadingMap( mapView: MKMapView; withError: NSError ); cdecl;
begin
  if assigned( FMapView.FOnDidFailLoadingMap ) then
    FMapView.FOnDidFailLoadingMap( FMapView );
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapView( mapView: MKMapView; didSelectAnnotationView: MKAnnotationView ); cdecl;
var
  value: PAnnotationInfo;
begin
  if assigned( FMapView.FOnAnnotationClick ) then
  begin
    value := nil;
    if FMapView.FAnnotationsList.ContainsKey( ( didSelectAnnotationView.annotation as ILocalObject ).GetObjectID ) then
    begin
      FMapView.FAnnotationsList.TryGetValue( ( didSelectAnnotationView.annotation as ILocalObject ).GetObjectID, value );
    end;

    FMapView.FOnAnnotationClick( FMapView, didSelectAnnotationView, didSelectAnnotationView.annotation, Value );
  end;
end;

// ----------------------------------------------------------------------------
procedure TMKMapViewDelegate.mapView( mapView: MKMapView; regionDidChangeAnimated: Boolean ); cdecl;
begin
  FMapView.FZoomLevel := GetZoomLevel( FMapView.FMKMapView );
  if assigned( FMapView.FOnZoomChanged ) and ( FMapView.FLastZoomLevel <> FMapView.ZoomLevel ) then
    FMapView.FOnZoomChanged( FMapView );
  FMapView.FLastZoomLevel := FMapView.ZoomLevel;
end;

// ----------------------------------------------------------------------------
function TMKMapViewDelegate.mapView( mapView: MKMapView; viewForOverlay: MKOverlay ): MKOverlayView; cdecl;
var
  PolygonView : MKPolygonView;
  PolyLineView: MKPolylineView;
  CircleView  : MKCircleView;
  pinf        : POverlayInfo;
begin
  if FMapView.FOverlaysList.ContainsKey( ( viewForOverlay as ILocalObject ).GetObjectID ) then
  begin
    FMapView.FOverlaysList.TryGetValue( ( viewForOverlay as ILocalObject ).GetObjectID, pinf );
    if viewForOverlay.isKindOfClass( objc_getClass( 'MKCircle' ) ) then
    begin
      CircleView := TMKCircleView.Wrap( TMKCircleView.Alloc.initWithCircle( MKCircle( viewForOverlay ) ) );
      CircleView.setFillColor( TColorToUIColor( pinf^.FillColor ) );
      CircleView.setStrokeColor( TColorToUIColor( pinf^.StrokeColor ) );
      CircleView.setLineWidth( pinf^.LineWidth );
      CircleView.setAlpha( pinf^.Alpha );
      result := CircleView;
    end
    else if viewForOverlay.isKindOfClass( objc_getClass( 'MKPolyline' ) ) then
    begin
      PolyLineView := TMKPolylineView.Wrap( TMKPolylineView.Alloc.initWithPolyline( MKPolyline( viewForOverlay ) ) );
      PolyLineView.setFillColor( TColorToUIColor( pinf^.FillColor ) );
      PolyLineView.setStrokeColor( TColorToUIColor( pinf^.StrokeColor ) );
      PolyLineView.setLineWidth( pinf^.LineWidth );
      PolyLineView.setAlpha( pinf^.Alpha );
      result := PolyLineView;
    end
    else if viewForOverlay.isKindOfClass( objc_getClass( 'MKPolygon' ) ) then
    begin
      PolygonView := TMKPolygonView.Wrap( TMKPolygonView.Alloc.initWithPolygon( MKPolygon( viewForOverlay ) ) );
      PolygonView.setFillColor( TColorToUIColor( pinf^.FillColor ) );
      PolygonView.setStrokeColor( TColorToUIColor( pinf^.StrokeColor ) );
      PolygonView.setLineWidth( pinf^.LineWidth );
      PolygonView.setAlpha( pinf^.Alpha );
      result := PolygonView;
    end
  end
end;

// ----------------------------------------------------------------------------
(* function TMKMapViewDelegate.mapView( mapView: MKMapView; rendererForOverlay: Pointer ): MKOverlayRenderer; cdecl;
  var
  ov: MKOverlay;
  begin

  ov := TMKOverlay.Wrap( rendererForOverlay );

  if ov.isKindOfClass( objc_getClass( 'MKTileOverlay' ) ) then
  begin
  M      := TDPFMKTileOverlayRenderer.Create( FMapView, TMKTileOverlay.Wrap( rendererForOverlay ) );
  result := MKOverlayRenderer( M.Super );
  // result := TMKOverlayRenderer.Wrap( TMKTileOverlayRenderer.alloc.initWithTileOverlay( TMKTileOverlay.Wrap( rendererForOverlay ) ) )
  end
  else
  result := MKOverlayRenderer( self.mapView( mapView, ov ) );

  end; *)

// ----------------------------------------------------------------------------
constructor TDPFMKMapView.Create( ADPFMapView: TDPFMapView );
var
  V: Pointer;
begin
  inherited Create;
  FDPFMapView := ADPFMapView;
  V           := MKMapView( Super ).initWithFrame( CGRectMake( 0, 0, 0, 0 ) );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFMKMapView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFMKMapView );
end;

// ----------------------------------------------------------------------------
procedure TDPFMKMapView.singleTap( Sender: pointer ); cdecl;
const
  MAX_DISTANCE_PX = 22;
var
  i  : Integer;
  v  : UIView;
  ovr: MKOverlay;

  ViewPoint        : CGPoint;
  PathContainsPoint: Integer;
  path             : CGPathRef;

  PolygonView : MKPolygonView;
  PolyLineView: MKPolylineView;
  CircleView  : MKCircleView;

  distance { , nearestDistance , maxMeters } : double;
begin

  if assigned( FDPFMapView.FMKMapView.overlays ) then
    for i := 0 to FDPFMapView.FMKMapView.overlays.count - 1 do
    begin
      ovr := TMKOverlay.Wrap( FDPFMapView.FMKMapView.overlays.objectAtIndex( i ) );
      v   := FDPFMapView.FMKMapView.viewForOverlay( ovr );
      // CheckPoinOnOverlay( FDPFMapView.FMKMapView, mapPoint, ovr );

      if assigned( FDPFMapView.FOnPolygonClick ) and ovr.isKindOfClass( objc_getClass( 'MKPolygon' ) ) and v.isKindOfClass( objc_getClass( 'MKPolygonView' ) ) then
      begin
        polygonView := TMKPolygonView.Wrap( ( v as ILocalObject ).GetObjectID );
        ViewPoint   := polygonView.pointForMapPoint( LastMapPointTouch );

        PathContainsPoint := 0;
        polygonView.invalidatePath;
        path := polygonView.path;
        if path <> nil then
          PathContainsPoint := CGPathContainsPoint( path, nil, ViewPoint, 0 );
        if PathContainsPoint > 0 then
        begin
          FDPFMapView.FOnPolygonClick( FDPFMapView, PolygonView, PolygonView.polygon );
          // ShowAlert( 'Found! ' + UTF8ToString( polygon.title.UTF8String ) );
        end;
      end
      else if assigned( FDPFMapView.FOnPolylineClick ) and ovr.isKindOfClass( objc_getClass( 'MKPolyline' ) ) and v.isKindOfClass( objc_getClass( 'MKPolylineView' ) ) then
      begin
        PolyLineView := TMKPolylineView.Wrap( ( v as ILocalObject ).GetObjectID );
        // maxMeters       := metersFromPixel( FDPFMapView.FMKMapView, MAX_DISTANCE_PX, LastTouchPoint );
        // nearestDistance := MaxDouble;
        distance := distanceOfPoint( LastMapPointTouch, PolyLineView.polyline );

        if distance < 20 then
        begin
          // polyLine := TMKPolyline.Wrap( FDPFMapView.FMKMapView.overlays.objectAtIndex( i ) );
          FDPFMapView.FOnPolylineClick( FDPFMapView, PolyLineView, PolyLineView.polyline );
          // ShowAlert( 'Found! ' + UTF8ToString( polyLine.title.UTF8String ) );
        end;

        { polyLineView := TMKPolylineView.Wrap( ( v as ILocalObject ).GetObjectID );
          ViewPoint    := polyLineView.pointForMapPoint( mapPoint );

          PathContainsPoint := 0;
          polyLineView.invalidatePath;
          path := polyLineView.path;
          if path <> nil then
          PathContainsPoint := CGPathContainsPoint( path, nil, ViewPoint, 1 );
          if PathContainsPoint > 0 then
          begin
          polyLine := TMKPolyline.Wrap( FDPFMapView.FMKMapView.overlays.objectAtIndex( i ) );
          ShowAlert( 'Found! ' + UTF8ToString( polyLine.title.UTF8String ) );
          end; }
      end
      else if assigned( FDPFMapView.FOnCircleClick ) and ovr.isKindOfClass( objc_getClass( 'MKCircle' ) ) and v.isKindOfClass( objc_getClass( 'MKCircleView' ) ) then
      begin
        CircleView := TMKCircleView.Wrap( ( v as ILocalObject ).GetObjectID );
        ViewPoint  := CircleView.pointForMapPoint( LastMapPointTouch );

        PathContainsPoint := 0;
        CircleView.invalidatePath;
        path := CircleView.path;
        if path <> nil then
          PathContainsPoint := CGPathContainsPoint( path, nil, ViewPoint, 0 );
        if PathContainsPoint > 0 then
        begin
          // Circle := TMKCircle.Wrap( FDPFMapView.FMKMapView.overlays.objectAtIndex( i ) );
          FDPFMapView.FOnCircleClick( FDPFMapView, CircleView, CircleView.circle );
        end;
      end
    end;
end;

// ----------------------------------------------------------------------------
procedure TDPFMKMapView.doubleTap( Sender: pointer ); cdecl;
begin
end;

// ----------------------------------------------------------------------------
procedure TDPFMKMapView.touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
begin
  MKMapView( Super ).touchesBegan( touches, iOSapi.UIKit.UIEvent( withEvent ) );
end;

// ----------------------------------------------------------------------------
procedure TDPFMKMapView.touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
begin
  MKMapView( Super ).touchesMoved( touches, iOSapi.UIKit.UIEvent( withEvent ) );
end;

// ----------------------------------------------------------------------------
procedure TDPFMKMapView.touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch: UITouch;
  coord: CLLocationCoordinate2D;
begin
  MKMapView( Super ).touchesEnded( touches, iOSapi.UIKit.UIEvent( withEvent ) );
  Touch             := TUITouch.Wrap( touches.anyObject );
  LastTouchPoint    := Touch.locationInView( FDPFMapView.FMKMapView );
  coord             := FDPFMapView.FMKMapView.convertPoint( LastTouchPoint, FDPFMapView.FMKMapView );
  LastMapPointTouch := MKMapPointForCoordinate( coord );

  if Touch.tapCount = 1 then
    NSObject( self.Super ).performSelector( sel_getUid( 'singleTap:' ), nil, 1 / 1000 )
  else if Touch.tapCount > 1 then
  begin
    iOSapi.{$IFDEF DELPHIXE7}Foundation{$ELSE}CocoaTypes{$ENDIF}.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget( Self.GetObjectID );
    NSObject( self.Super ).performSelector( sel_getUid( 'doubleTap:' ), nil, 2 / 1000 );
  end;

end;

// ----------------------------------------------------------------------------
{$IFDEF IOS7}
(* function TDPFMKMapView.viewForOverlay( overlay: MKOverlay ): MKOverlayView; cdecl;
  begin
  end;

  // ----------------------------------------------------------------------------
  function TDPFMKMapView.rendererForOverlay( overlay: MKOverlay ): MKOverlayRenderer; cdecl; // iOS 7.0 and later
  begin
  result := nil;
  if overlay.isKindOfClass( objc_getClass( 'MKTileOverlay' ) ) then
  result := MKOverlayRenderer( TMKTileOverlayRenderer.Wrap( TMKTileOverlayRenderer.alloc.initWithTileOverlay( MKTileOverlay( overlay ) ) ) );

  end;
*)
{$ENDIF}

// ----------------------------------------------------------------------------
constructor TDPFMKTileOverlayRenderer.Create( ADPFMapView: TDPFMapView; AMKTileOverlay: MKTileOverlay );
var
  V: Pointer;
begin
  inherited Create;
  FDPFMapView := ADPFMapView;
  V           := MKTileOverlayRenderer( Super ).initWithTileOverlay( AMKTileOverlay );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFMKTileOverlayRenderer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFMKTileOverlayRenderer );
end;

// ----------------------------------------------------------------------------
procedure TDPFMKTileOverlayRenderer.drawMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale; inContext: CGContextRef ); cdecl;
var
  image     : UIImage;
  theMapRect: MKMapRect;
  theRect   : CGRect;
  ImageName : string;
  Alpha     : Single;
begin
  MKTileOverlayRenderer( Super ).drawMapRect( mapRect, zoomScale, inContext );
  if assigned( FDPFMapView.FOnDrawCustomImage ) then
  begin
    ImageName := '';
    Alpha     := 1.0;
    FDPFMapView.FOnDrawCustomImage( FDPFMapView, FDPFMapView.ZoomLevel, ImageName, Alpha );
    if ImageName <> '' then
    begin
      image      := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( ImageName ) ) );
      theMapRect := MKTileOverlayRenderer( Super ).overlay.boundingMapRect;
      theRect    := MKTileOverlayRenderer( Super ).rectForMapRect( theMapRect );

      UIGraphicsPushContext( inContext );
      // theRect.size.width  := FDPFMapView.Width;
      // theRect.size.height := FDPFMapView.Height;
      image.drawInRect( theRect, kCGBlendModeNormal, Alpha );
      UIGraphicsPopContext( );
    end;
  end;
end;

{$ENDIF}

// ----------------------------------------------------------------------------
end.
