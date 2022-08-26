// ------------------------------------------------------------------------------
// DPF.iOS.MapKit Wrapped Classes & Interfaces
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
unit DPF.iOS.MapKit;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
{$IFDEF IOS}
  iOSapi.UIKit,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.CoreLocation,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Dialogs;


// ===== External functions =====

const
  libMapKit = '/System/Library/Frameworks/MapKit.framework/MapKit';

  MERCATOR_RADIUS = 85445659.44705395;
  // MAX_LEVELS = 20;

  MKMapTypeStandard  = 0;
  MKMapTypeSatellite = 1;
  MKMapTypeHybrid    = 2;

  MKUserTrackingModeNone              = 0;
  MKUserTrackingModeFollow            = 1;
  MKUserTrackingModeFollowWithHeading = 2;

  MKPinAnnotationColorRed    = 0;
  MKPinAnnotationColorGreen  = 1;
  MKPinAnnotationColorPurple = 2;

  MKAnnotationViewDragStateNone      = 0;
  MKAnnotationViewDragStateStarting  = 1;
  MKAnnotationViewDragStateDragging  = 2;
  MKAnnotationViewDragStateCanceling = 3;
  MKAnnotationViewDragStateEnding    = 4;

{$IFDEF IOS}

type
{$M+}
  // ===== Forward declarations =====

  MKMapType                 = NSInteger;
  MKUserTrackingMode        = NSInteger;
  MKZoomScale               = CGFloat;
  MKPinAnnotationColor      = NSInteger;
  MKAnnotationViewDragState = NSInteger;

  MKOverlay     = interface;
  MKTileOverlay = interface;
  MKMapView     = interface;
  MKPolyline    = interface;
  MKPolygon     = interface;

  MKCoordinateSpan = record
    latitudeDelta: CLLocationDegrees;
    longitudeDelta: CLLocationDegrees;
  end;

  MKCoordinateRegion = record
    center: CLLocationCoordinate2D;
    span: MKCoordinateSpan;
  end;

  MKMapSize = record
    width: double;
    height: double;
  end;

  PMKMapPoint = ^MKMapPoint;

  MKMapPoint = record
    x: double;
    y: double;
  end;

  MKMapRect = record
    origin: MKMapPoint;
    size: MKMapSize;
  end;

  MKOverlayLevel = ( MKOverlayLevelAboveRoads = 0, MKOverlayLevelAboveLabels = 1 );

  // ===== Interface declarations =====

  // ----------------------------------------------------------------------------
  // MKOverlay
  // ----------------------------------------------------------------------------
  MKOverlayClass = interface( NSObjectClass )
    ['{0221DE4C-DDD3-47C6-B865-F96CD043D7DD}']
  end;

  MKOverlay = interface( NSObject )
    ['{B575B074-E26A-4272-B679-85731AC85191}']
    function coordinate: CLLocationCoordinate2D; cdecl;
    function boundingMapRect: MKMapRect; cdecl;
    function intersectsMapRect( mapRect: MKMapRect ): Boolean; cdecl;
    //function canReplaceMapContent: boolean; cdecl; // iOS 7.0 and later.
  end;

  TMKOverlay = class( TOCGenericImport<MKOverlayClass, MKOverlay> )
  end;

  // ----------------------------------------------------------------------------
  // MKOverlayRenderer  --> iOS 7.0 and later
  // ----------------------------------------------------------------------------
  MKOverlayRendererClass = interface( NSObjectClass )
    ['{65329C4F-010D-4C27-9315-7D4B56245770}']

  end;

  MKOverlayRenderer = interface( NSObject )
    ['{4E2E5627-17CC-4CD5-8100-E2B1FC938EC4}']

    function initWithOverlay( overlay: MKOverlay ): pointer; cdecl;
    function overlay: MKOverlay; cdecl;
    function alpha: CGFloat; cdecl;
    procedure setAlpha( alpha: CGFloat ); cdecl;
    function intersectsMapRect( mapRect: MKMapRect ): Boolean; cdecl;
    procedure drawMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale; inContext: CGContextRef ); cdecl;
    function canDrawMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale ): Boolean; cdecl;
    procedure setNeedsDisplay; cdecl;
    procedure setNeedsDisplayInMapRect( mapRect: MKMapRect ); overload; cdecl;
    procedure setNeedsDisplayInMapRect( mapRect: MKMapRect; zoomScale: MKZoomScale ); overload; cdecl;

    function rectForMapRect( mapRect: MKMapRect ): CGRect; cdecl;
    function pointForMapPoint( mapPoint: MKMapPoint ): CGPoint; cdecl;
    function mapPointForPoint( point: CGPoint ): MKMapPoint; cdecl;
    function mapRectForRect( rect: CGRect ): MKMapRect; cdecl;
  end;

  TMKOverlayRenderer = class( TOCGenericImport<MKOverlayRendererClass, MKOverlayRenderer> )
  end;

  // ----------------------------------------------------------------------------
  // MKAnnotation
  // ----------------------------------------------------------------------------
  MKAnnotationClass = interface( MKOverlayClass )
    ['{88C8FE51-B33F-4F84-B485-20CD5ACA8A50}']
    // Function getTag: NSInteger; Cdecl;
    // Procedure setTag( tag: NSInteger ); Cdecl;
    // Property tag: integer Read getTag Write setTag;
  end;

  MKAnnotation = interface( MKOverlay )
    ['{334CAE85-3BB7-4C8C-A870-1588C71ED6D6}']
    function coordinate: CLLocationCoordinate2D; cdecl; // iOS 3.0 and later.
    procedure setCoordinate( newCoordinate: CLLocationCoordinate2D ); cdecl; // iOS 4.0 and later.

    function title: NSString; cdecl;              // iOS 5.0 and later.
    procedure setTitle( title: NSString ); cdecl; // iOS 5.0 and later.

    function subtitle: NSString; cdecl;                 // iOS 5.0 and later.
    procedure setSubtitle( subtitle: NSString ); cdecl; // iOS 5.0 and later.

  end;

  TMKAnnotation = class( TOCGenericImport<MKAnnotationClass, MKAnnotation> )
  end;

  // ----------------------------------------------------------------------------
  // MKShape
  // ----------------------------------------------------------------------------
  MKShapeClass = interface( MKAnnotationClass )
    ['{9BCF51F6-3F4E-40AF-8947-ED9C64749161}']
  end;

  MKShape = interface( MKAnnotation )
    ['{929E7E0C-A662-442B-A712-EF76C996B492}']
    function title: NSString; cdecl;
    procedure setTitle( title: NSString ); cdecl;

    function subtitle: NSString; cdecl;
    procedure setSubtitle( subtitle: NSString ); cdecl;
  end;

  TMKShape = class( TOCGenericImport<MKShapeClass, MKShape> )
  end;

  // ----------------------------------------------------------------------------
  // MKMultiPoint
  // ----------------------------------------------------------------------------
  MKMultiPointClass = interface( MKShapeClass )
    ['{BC83F776-778F-473B-A0AA-05F31CFBD67C}']
  end;

  MKMultiPoint = interface( MKShape )
    ['{BE5EA526-D4CC-45EA-B1A2-EBAC16B91BA1}']
    function pointCount: NSUInteger; cdecl;
    function points: PMKMapPoint; cdecl;
    procedure getCoordinates( coords: CLLocationCoordinate2D; range: NSRange ); cdecl;
  end;

  TMKMultiPoint = class( TOCGenericImport<MKMultiPointClass, MKMultiPoint> )
  end;

  // ----------------------------------------------------------------------------
  // MKPolyline
  // ----------------------------------------------------------------------------
  MKPolylineClass = interface( MKMultiPointClass )
    ['{B4B8D515-CA61-4BA3-827F-EA45C8A1C938}']
    function polylineWithCoordinates( coords: pointer; count: NSUInteger ): MKPolyline; cdecl;
  end;

  MKPolyline = interface( MKMultiPoint )
    ['{5A854B60-AA43-48B2-9860-B298E957F1E7}']
    function polylineWithPoints( points: MKMapPoint; count: NSUInteger ): pointer; cdecl;
    function boundingMapRect: MKMapRect; cdecl;
  end;

  TMKPolyline = class( TOCGenericImport<MKPolylineClass, MKPolyline> )
  end;

  // ----------------------------------------------------------------------------
  // MKPolygon
  // ----------------------------------------------------------------------------
  MKPolygonClass = interface( MKMultiPointClass )
    ['{4CB4784D-4D29-414E-A0FB-F9A923528024}']
    function polygonWithCoordinates( coords: pointer; count: NSUInteger ): MKPolygon; cdecl;
    function polygonWithPoints( points: pointer; { (MKMapPoint *) } count: NSUInteger ): pointer; cdecl;
  end;

  MKPolygon = interface( MKMultiPoint )
    ['{27A02830-2A30-4B48-BDA1-7A19F8246244}']
  end;

  TMKPolygon = class( TOCGenericImport<MKPolygonClass, MKPolygon> )
  end;

  // ----------------------------------------------------------------------------
  // MKPointAnnotation
  // ----------------------------------------------------------------------------
  MKPointAnnotationClass = interface( MKShapeClass )
    ['{2825581D-C4E6-4AC1-A436-ACC038B418A3}']
  end;

  MKPointAnnotation = interface( MKShape )
    ['{E575DB9E-30A3-4B24-A56B-89066149D4E2}']
    function coordinate: CLLocationCoordinate2D; cdecl;
    procedure setCoordinate( coordinate: CLLocationCoordinate2D ); cdecl;

  end;

  TMKPointAnnotation = class( TOCGenericImport<MKPointAnnotationClass, MKPointAnnotation> )
  end;

  // ----------------------------------------------------------------------------
  // MKCircle
  // ----------------------------------------------------------------------------
  MKCircleClass = interface( MKShapeClass )
    ['{92B2A5AC-DF1B-40A7-B8C3-20484DBA89B9}']
    function circleWithCenterCoordinate( coord: CLLocationCoordinate2D; radius: CLLocationDistance ): Pointer; cdecl;
    function circleWithMapRect( mapRect: MKMapRect ): Pointer; cdecl;
  end;

  MKCircle = interface( MKShape )
    ['{BF084CDB-2032-4EA2-BA34-3B0054719FEF}']
    function coordinate: CLLocationCoordinate2D; cdecl;
    function radius: CLLocationDistance; cdecl;
    function boundingMapRect: MKMapRect; cdecl;
  end;

  TMKCircle = class( TOCGenericImport<MKCircleClass, MKCircle> )
  end;

  // ----------------------------------------------------------------------------
  // MKOverlayView
  // ----------------------------------------------------------------------------
  MKOverlayViewClass = interface( UIViewClass )
    ['{1F3084AF-3D84-4BE9-9DF4-FFC8AE231FDA}']
  end;

  MKOverlayView = interface( UIView )
    ['{BC82BA38-A1F9-4EF7-8BAE-1A70339D7848}']
    function initWithOverlay( overlay: MKOverlay ): pointer; cdecl;
    function overlay: MKOverlay; cdecl;
    function rectForMapRect( mapRect: MKMapRect ): CGRect; cdecl;
    function pointForMapPoint( mapPoint: MKMapPoint ): CGPoint; cdecl;
  end;

  TMKOverlayView = class( TOCGenericImport<MKOverlayViewClass, MKOverlayView> )
  end;

  // ----------------------------------------------------------------------------
  // MKOverlayPathView
  // ----------------------------------------------------------------------------
  MKOverlayPathViewClass = interface( MKOverlayViewClass )
    ['{B30BFCFF-005C-4470-A8E9-E6A9A1F2838A}']
  end;

  MKOverlayPathView = interface( MKOverlayView )
    ['{BBA09D4D-25C5-46D4-A622-D0A81525C3D4}']
    function fillColor: UIColor; cdecl;
    procedure setFillColor( fillColor: UIColor ); cdecl;

    function strokeColor: UIColor; cdecl;
    procedure setStrokeColor( strokeColor: UIColor ); cdecl;

    function lineWidth: CGFloat; cdecl;
    procedure setLineWidth( lineWidth: CGFloat ); cdecl;

    function lineCap: CGLineCap; cdecl;
    procedure setLineCap( lineCap: CGLineCap ); cdecl;

    function miterLimit: CGFloat; cdecl;
    procedure setMiterLimit( miterLimit: CGFloat ); cdecl;

    function lineDashPhase: CGFloat; cdecl;
    procedure setLineDashPhase( lineDashPhase: CGFloat ); cdecl;

    function lineDashPattern: NSArray; cdecl;
    procedure setLineDashPattern( lineDashPattern: NSArray ); cdecl;

    function path: CGPathRef; cdecl;
    procedure setPath( path: CGPathRef ); cdecl;

    function savedPath: CGPathRef; cdecl;
    procedure setSavedPath( savedPath: CGPathRef ); cdecl;

    procedure createPath; cdecl;
    procedure invalidatePath; cdecl;

    procedure applyStrokePropertiesToContext( context: CGContextRef; zoomScale: MKZoomScale ); cdecl;
    procedure applyFillPropertiesToContext( context: CGContextRef; zoomScale: MKZoomScale ); cdecl;
    procedure strokePath( inContext: CGPathRef; context: CGContextRef ); cdecl;
    procedure fillPath( path: CGPathRef; context: CGContextRef ); cdecl;

  end;

  TMKOverlayPathView = class( TOCGenericImport<MKOverlayPathViewClass, MKOverlayPathView> )
  end;

  // ----------------------------------------------------------------------------
  // MKPolylineView
  // ----------------------------------------------------------------------------
  MKPolylineViewClass = interface( MKOverlayPathViewClass )
    ['{C7BB68C3-966D-4C77-8E08-1013AD7C7E44}']
  end;

  MKPolylineView = interface( MKOverlayPathView )
    ['{7B62C95D-A35D-4374-8884-EA59030D5947}']
    function initWithPolyline( polyline: MKPolyline ): Pointer; cdecl;
    function polyline: MKPolyline; cdecl;
  end;

  TMKPolylineView = class( TOCGenericImport<MKPolylineViewClass, MKPolylineView> )
  end;

  // ----------------------------------------------------------------------------
  // MKPolygonView
  // ----------------------------------------------------------------------------
  MKPolygonViewClass = interface( MKOverlayPathViewClass )
    ['{6E1D8744-32BB-4B88-A2BB-3E94CF7E13C6}']
  end;

  MKPolygonView = interface( MKOverlayPathView )
    ['{94DA51EC-4E45-4A10-8758-E27A1711D849}']
    function initWithPolygon( Polygon: MKPolygon ): Pointer; cdecl;
    function polygon: MKPolygon; cdecl;
  end;

  TMKPolygonView = class( TOCGenericImport<MKPolygonViewClass, MKPolygonView> )
  end;

  // ----------------------------------------------------------------------------
  // MKCircleView
  // ----------------------------------------------------------------------------
  MKCircleViewClass = interface( MKOverlayPathViewClass )
    ['{EA875978-9B1C-4E7E-9463-7FE90CFA687C}']
  end;

  MKCircleView = interface( MKOverlayPathView )
    ['{6D29E1C5-78AA-464D-A31E-61FF2BF3B135}']
    function initWithCircle( circle: MKCircle ): pointer; cdecl;
    function circle: MKCircle; cdecl;
  end;

  TMKCircleView = class( TOCGenericImport<MKCircleViewClass, MKCircleView> )
  end;

  // ----------------------------------------------------------------------------
  // MKUserLocation
  // ----------------------------------------------------------------------------
  MKUserLocationClass = interface( NSObjectClass )
    ['{6DE3D3C2-D853-4276-A3C3-4C9D76C1A72F}']
  end;

  MKUserLocation = interface( NSObject )
    ['{784D6485-8EEE-4E86-AC5D-121234FCAC69}']
    function location: CLLocation; cdecl;
    function subtitle: NSString; cdecl;
    function title: NSString; cdecl;
    function updating: Boolean; cdecl;
    function heading: CLHeading; cdecl;
  end;

  // ----------------------------------------------------------------------------
  // MKMapView
  // ----------------------------------------------------------------------------
  MKAnnotationViewClass = interface( UIViewClass )
    ['{B3EF5479-2518-458E-8C2E-617077E6EC87}']
  end;

  MKAnnotationView = interface( UIView )
    ['{1EB5B753-07A3-4945-8FC9-B996AAF522BF}']

    function initWithAnnotation( annotation: MKAnnotation; reuseIdentifier: NSString ): Pointer; cdecl;
    procedure prepareForReuse; cdecl;

    function enabled: Boolean; cdecl;
    procedure setEnabled( enabled: Boolean ); cdecl;

    function highlighted: Boolean; cdecl;
    procedure setHighlighted( highlighted: Boolean ); cdecl;

    function image: UIImage; cdecl;
    procedure setImage( image: UIImage ); cdecl;

    function centerOffset: CGPoint; cdecl;
    procedure setCenterOffset( centerOffset: CGPoint ); cdecl;

    function calloutOffset: CGPoint; cdecl;
    procedure setCalloutOffset( calloutOffset: CGPoint ); cdecl;

    function reuseIdentifier: NSString; cdecl;
    procedure setReuseIdentifier( reuseIdentifier: NSString ); cdecl;

    function annotation: MKAnnotation; cdecl;
    procedure setAnnotation( annotation: MKAnnotation ); cdecl;

    function selected: Boolean; cdecl;
    procedure setSelected( selected: Boolean; animated: Boolean ); cdecl;

    function dragState: MKAnnotationViewDragState; cdecl;
    procedure setDragState( newDragState: MKAnnotationViewDragState; animated: Boolean ); cdecl;

    function canShowCallout: Boolean; cdecl;
    procedure setCanShowCallout( canShowCallout: Boolean ); cdecl;

    function draggable: Boolean; cdecl;
    procedure setDraggable( draggable: Boolean ); cdecl;

    function leftCalloutAccessoryView: UIView; cdecl;
    procedure setLeftCalloutAccessoryView( leftCalloutAccessoryView: UIView ); cdecl;

    function rightCalloutAccessoryView: UIView; cdecl;
    procedure setRightCalloutAccessoryView( rightCalloutAccessoryView: UIView ); cdecl;

  end;

  TMKAnnotationView = class( TOCGenericImport<MKAnnotationViewClass, MKAnnotationView> )
  end;

  // ----------------------------------------------------------------------------
  // MKPinAnnotationView
  // ----------------------------------------------------------------------------
  MKPinAnnotationViewClass = interface( MKAnnotationViewClass )
    ['{3E190A93-1816-4E7B-A847-B6D51EDE1B15}']
  end;

  MKPinAnnotationView = interface( MKAnnotationView )
    ['{48B903B6-99D2-476A-A20B-884504E3071F}']
    function pinColor: MKPinAnnotationColor; cdecl;
    procedure setPinColor( pinColor: MKPinAnnotationColor ); cdecl;

    function animatesDrop: Boolean; cdecl;
    procedure setAnimatesDrop( animatesDrop: Boolean ); cdecl;
  end;

  TMKPinAnnotationView = class( TOCGenericImport<MKPinAnnotationViewClass, MKPinAnnotationView> )
  end;

  // ----------------------------------------------------------------------------
  // MKMapView
  // ----------------------------------------------------------------------------
  MKMapViewClass = interface( UIViewClass )
    ['{2EDA6A0A-BFA2-43AC-9EDA-D640535C5C99}']

  end;

  MKMapView = interface( UIView )
    ['{5D0A2DE3-E59F-4E27-8BC0-C7C21EA9228C}']

    function mapType: MKMapType; cdecl;
    procedure setMapType( mapType: MKMapType ); cdecl;

    function zoomEnabled: Boolean; cdecl;
    procedure setZoomEnabled( zoomEnabled: Boolean ); cdecl;

    function showsUserLocation: Boolean; cdecl;
    procedure setShowsUserLocation( showsUserLocation: Boolean ); cdecl;

    function userLocation: MKUserLocation; cdecl;

    function userLocationVisible: Boolean; cdecl;

    function overlays: NSArray; cdecl;

    function region: MKCoordinateRegion; cdecl;
    procedure setRegion( region: MKCoordinateRegion; animated: Boolean ); cdecl;

    function visibleMapRect: MKMapRect; cdecl;
    procedure setVisibleMapRect( visibleMapRect: MKMapRect ); cdecl;

    function scrollEnabled: Boolean; cdecl;
    procedure setScrollEnabled( scrollEnabled: Boolean ); cdecl;

    function delegate: Pointer; cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;

    function annotations: NSArray; cdecl;

    procedure addAnnotation( annotation: MKAnnotation ); cdecl;
    procedure addAnnotations( annotations: NSArray ); cdecl;

    procedure removeAnnotation( annotations: MKAnnotation ); overload; cdecl;
    procedure removeAnnotation( annotations: MKPointAnnotation ); overload; cdecl;
    procedure removeAnnotations( annotations: NSArray ); cdecl;

    procedure addOverlay( overlay: MKTileOverlay; level: MKOverlayLevel ); overload; cdecl;
    procedure addOverlay( overlay: MKOverlay ); overload; cdecl;
    procedure addOverlays( overlays: NSArray ); cdecl;

    procedure removeOverlay( overlay: MKOverlay ); overload; cdecl;
    procedure removeOverlay( overlay: MKTileOverlay ); overload; cdecl;
    procedure removeOverlays( overlays: NSArray ); cdecl;

    function selectedAnnotations: NSArray; cdecl;
    procedure selectAnnotation( annotation: MKAnnotation; animated: Boolean ); cdecl;
    procedure deselectAnnotation( annotation: MKAnnotation; animated: Boolean ); cdecl;

    function userTrackingMode: MKUserTrackingMode; cdecl;
    procedure setUserTrackingMode( mode: MKUserTrackingMode; animated: Boolean ); cdecl;

    function regionThatFits( region: MKCoordinateRegion ): MKCoordinateRegion; cdecl;

    function annotationsInMapRect( mapRect: MKMapRect ): NSSet; cdecl;

    function annotationVisibleRect: CGRect; cdecl;

    function centerCoordinate: CLLocationCoordinate2D; cdecl;
    procedure setCenterCoordinate( centerCoordinate: CLLocationCoordinate2D ); cdecl; overload;
    procedure setCenterCoordinate( centerCoordinate: CLLocationCoordinate2D; animated: Boolean ); cdecl; overload;

    function dequeueReusableAnnotationViewWithIdentifier( identifier: NSString ): pointer; cdecl;

    function convertPoint( point: CGPoint; toCoordinateFromView: UIView ): CLLocationCoordinate2D; cdecl;
    function convertCoordinate(coordinate: CLLocationCoordinate2D; toPointToView: UIView): CGPoint; cdecl;

    function viewForOverlay( overlay: MKOverlay ): MKOverlayView; cdecl;
    function viewForAnnotation( annotation: MKAnnotation ): MKAnnotationView; cdecl;

    function rendererForOverlay( overlay: MKOverlay ): MKOverlayRenderer; cdecl; // iOS 7.0 and later
  end;

  TMKMapView = class( TOCGenericImport<MKMapViewClass, MKMapView> )
  end;

  // ----------------------------------------------------------------------------
  // MKMapViewDelegate
  // ----------------------------------------------------------------------------
  IMKMapViewDelegate = interface( IObjectiveC )
    ['{8C0E8249-72FA-4A4A-9404-5301EDF4C94A}']

    function mapView( mapView: MKMapView; viewForAnnotation: MKAnnotation ): MKAnnotationView; overload; cdecl;
    function mapView( mapView: MKMapView; viewForOverlay: MKOverlay ): MKOverlayView; overload; cdecl;
    procedure mapView( mapView: MKMapView; regionDidChangeAnimated: Boolean ); overload; cdecl;
    procedure mapView( mapView: MKMapView; didSelectAnnotationView: MKAnnotationView ); overload; cdecl;
    procedure mapView( mapView: MKMapView; annotationView: MKAnnotationView; calloutAccessoryControlTapped: UIControl ); overload; cdecl;

    //function mapView( mapView: MKMapView; rendererForOverlay: Pointer ): MKOverlayRenderer; overload; cdecl;

    procedure mapViewWillStartLocatingUser( mapView: MKMapView ); cdecl;
    procedure mapViewDidStopLocatingUser( mapView: MKMapView ); cdecl;
    procedure mapView( mapView: MKMapView; didUpdateUserLocation: MKUserLocation ); overload; cdecl;

    procedure mapViewWillStartLoadingMap( mapView: MKMapView ); cdecl;
    procedure mapViewDidFinishLoadingMap( mapView: MKMapView ); cdecl;
    procedure mapViewDidFailLoadingMap( mapView: MKMapView; withError: NSError ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TLoadTileAtPathCompletionBlock = procedure( tileData: NSData; error: NSError ) of object;

  MKTileOverlayPath = record
    x: NSInteger;
    y: NSInteger;
    z: NSInteger;
    contentScaleFactor: CGFloat;
  end;

  // ----------------------------------------------------------------------------
  // MKTileOverlay / iOS 7 and later only
  // ----------------------------------------------------------------------------

  MKTileOverlayClass = interface( NSObjectClass )
    ['{C7201AD7-E123-45FF-B3ED-5309194C29CE}']
  end;

  MKTileOverlay = interface( NSObject )
    ['{A8F84EF1-A7C5-4ADD-BCA1-3EB70294BBDB}']

    function initWithURLTemplate( URLTemplate: NSString ): pointer; cdecl;
    function tileSize: CGSize; cdecl;
    procedure setTileSize( tileSize: CGSize ); cdecl;
    function isGeometryFlipped: boolean; cdecl;
    procedure setGeometryFlipped( geometryFlipped: boolean ); cdecl;
    function minimumZ: NSInteger; cdecl;
    procedure setMinimumZ( minimumZ: NSInteger ); cdecl;
    function maximumZ: NSInteger; cdecl;
    procedure setMaximumZ( maximumZ: NSInteger ); cdecl;
    function canReplaceMapContent: boolean; cdecl;
    procedure setCanReplaceMapContent( canReplaceMapContent: boolean ); cdecl;
    function URLTemplate: NSString; cdecl;
    function URLForTilePath( path: MKTileOverlayPath ): NSURL; cdecl;
    procedure loadTileAtPath( path: MKTileOverlayPath; result: TLoadTileAtPathCompletionBlock ); cdecl;

  end;

  TMKTileOverlay = class( TOCGenericImport<MKTileOverlayClass, MKTileOverlay> )
  end;

  // ----------------------------------------------------------------------------
  // MKTileOverlayRenderer / iOS 7 and later only
  // ----------------------------------------------------------------------------

  MKTileOverlayRendererClass = interface( MKOverlayRendererClass )
    ['{78C1A62F-A13B-4C2F-8704-8EBC706D7772}']
  end;

  MKTileOverlayRenderer = interface( MKOverlayRenderer )
    ['{78C4D381-1432-421C-AC2E-F415EF22FD92}']

    function initWithTileOverlay( overlay: MKTileOverlay ): pointer; cdecl;
    procedure reloadData; cdecl;
  end;

  // TMKTileOverlayRenderer = class( TOCGenericImport<MKTileOverlayRendererClass, MKTileOverlayRenderer> ) end;

  // ----------------------------------------------------------------------------
function GetZoomLevel( mapView: MKMapView ): byte;
function CoordinateSpanWithMapView( mapView: MKMapView; centerCoordinate: CLLocationCoordinate2D; ZoomLevel: byte): MKCoordinateSpan;
function SetCenterCoordinate( mapView: MKMapView; centerCoordinate: CLLocationCoordinate2D; zoomLevel: byte; animated: Boolean ): Boolean;

function MKMapPointForCoordinate( coordinate: CLLocationCoordinate2D ): MKMapPoint; cdecl; external libMapKit name _PU + 'MKMapPointForCoordinate';
function MKMetersBetweenMapPoints( a: MKMapPoint; b: MKMapPoint ): CLLocationDistance; cdecl; external libMapKit name _PU + 'MKMetersBetweenMapPoints';
{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iMapKitModule: THandle;
{$ENDIF}

  // ----------------------------------------------------------------------------
function zoomScaleToZoomLevel( scale: MKZoomScale; MapSizeWorldwidth: Double; TILE_SIZE: Integer ): Integer;
var
  numTilesAt1_0 : Double;
  zoomLevelAt1_0: NSInteger;
  zoomLevel     : NSInteger;
begin
  numTilesAt1_0  := MapSizeWorldwidth / TILE_SIZE;
  zoomLevelAt1_0 := round( log2( numTilesAt1_0 ) ); // add 1 because the convention skips a virtual level with 1 tile.
  zoomLevel      := MAX( 0, zoomLevelAt1_0 + floor( log2( scale ) + 0.5 ) );
  result         := zoomLevel;
end;

// ----------------------------------------------------------------------------
function GetZoomLevel( mapView: MKMapView ): byte;
begin
  Result := Round( 21.00 - Log2( mapView.region.span.longitudeDelta * MERCATOR_RADIUS * PI / ( 180.0 * mapView.bounds.size.width ) ) );
end;

// ----------------------------------------------------------------------------
function CoordinateSpanWithMapView( mapView: MKMapView; centerCoordinate: CLLocationCoordinate2D; ZoomLevel: byte): MKCoordinateSpan;
var
  centerPixelX: double;
  centerPixelY: double;

  // determine the scale value from the zoom level
  zoomExponent: double;
  zoomScale   : double;

  mapSizeInPixels: CGSize;
  scaledMapWidth : double;
  scaledMapHeight: double;

  // figure out the position of the top-left pixel
  topLeftPixelX: double;
  topLeftPixelY: double;

  // find delta between left and right longitudes
  minLng        : CLLocationDegrees;
  maxLng        : CLLocationDegrees;
  longitudeDelta: CLLocationDegrees;

  // find delta between top and bottom latitudes
  minLat       : CLLocationDegrees;
  maxLat       : CLLocationDegrees;
  latitudeDelta: CLLocationDegrees;

begin

  // convert center coordiate to pixel space
  centerPixelX := longitudeToPixelSpaceX( centerCoordinate.longitude );
  centerPixelY := latitudeToPixelSpaceY( centerCoordinate.latitude );

  // determine the scale value from the zoom level
  zoomExponent := 20.0 - zoomLevel;
  zoomScale    := power( 2, zoomExponent );

  // scale the map’s size in pixel space
  mapSizeInPixels := mapView.bounds.size;
  scaledMapWidth  := mapSizeInPixels.width * zoomScale;
  scaledMapHeight := mapSizeInPixels.height * zoomScale;

  // figure out the position of the top-left pixel
  topLeftPixelX := centerPixelX - ( scaledMapWidth / 2 );
  topLeftPixelY := centerPixelY - ( scaledMapHeight / 2 );

  // find delta between left and right longitudes
  minLng         := pixelSpaceXToLongitude( topLeftPixelX );
  maxLng         := pixelSpaceXToLongitude( topLeftPixelX + scaledMapWidth );
  longitudeDelta := maxLng - minLng;

  // find delta between top and bottom latitudes
  minLat        := pixelSpaceYToLatitude( topLeftPixelY );
  maxLat        := pixelSpaceYToLatitude( topLeftPixelY + scaledMapHeight );
  latitudeDelta := -1 * ( maxLat - minLat );

  // create and return the lat/lng span
  Result.latitudeDelta  := latitudeDelta;
  Result.longitudeDelta := longitudeDelta;
end;

// ----------------------------------------------------------------------------
function SetCenterCoordinate( mapView: MKMapView; centerCoordinate: CLLocationCoordinate2D; zoomLevel: byte; animated: Boolean ): Boolean;
var
  region: MKCoordinateRegion;
begin
  // clamp large numbers to 28
  zoomLevel := MIN( zoomLevel, 28 );

  // use the zoom level to compute the region
  region.span   := coordinateSpanWithMapView( mapView, centerCoordinate, zoomLevel );
  region.center := centerCoordinate;

  result := ( region.span.latitudeDelta >= 0.0 ) and ( region.span.latitudeDelta <= 180.0 ) and ( region.span.longitudeDelta >= 0.0 ) and ( region.span.longitudeDelta <= 360.0 );

  if result then
    mapView.setRegion( region, animated );
end;
// ----------------------------------------------------------------------------

{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure LibMapKitFakeLoader; cdecl; external libMapKit;
{$ELSE}

initialization

iMapKitModule := dlopen( MarshaledAString( libMapKit ), RTLD_LAZY );

finalization

dlclose( iMapKitModule );
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
