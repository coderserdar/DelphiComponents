// ------------------------------------------------------------------------------
// DPF.Android.JMapView Component
//
// Developed By: Roman Yankovsky
//
// Email: roman@yankovsky.me
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
unit DPF.Android.JMapView;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

  System.TypInfo,
  Generics.Collections,
  DPF.Android.BaseControl,
  DPF.Android.Widget,
{$IFDEF ANDROID}
  Androidapi.JNI.Dalvik,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Os,
  FMX.Helpers.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type
  TDPFJMapView = class;

{$IFDEF ANDROID}
  TDPFGoogleMap_OnMarkerClickListener = class(TJavaLocal, JGoogleMap_OnMarkerClickListener)
  private
    FMapControl: TDPFJMapView;
  public
    constructor Create(MapControl: TDPFJMapView);
    function onMarkerClick(P1: JMarker): Boolean; cdecl;
  end;
{$ENDIF}
  TOnMarkerClick = procedure (Sender: TObject; MarkerId: Integer; var Handled: Boolean) of object;
  TOnMapLoaded = procedure (Sender: TObject) of object;

  [ComponentPlatformsAttribute(PidWin32 or pidAndroid)]
  TDPFJMapView = class(TDPFANDBaseControl)
  private
    procedure SetMyLocationEnabled(const Value: Boolean);
  protected
    FMyLocationEnabled: Boolean;
    FOnMarkerClick: TOnMarkerClick;
    FOnMapLoaded: TOnMapLoaded;
{$IFDEF ANDROID}
    FJMapView: JMapView;
    FMarkers: TDictionary<Integer, JMarker>;
    FGoogleMap_OnMarkerClickListener: JGoogleMap_OnMarkerClickListener;
    function MarkerClick(Marker: JMarker): Boolean;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJMapView: JMapView read FJMapView;

    procedure AddMarker(MarkerId: Integer; Lat, Lng: Double; Icon: JBitmapDescriptor = nil;
      const Title: string = '');
    procedure AddPolyline(Options: JPolylineOptions);
    procedure RemoveMarker(MarkerId: Integer);
    procedure SetCameraPosition(Lat, Lng: Double; Zoom: Single);

    procedure Clear;
    procedure Loaded; override;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MyLocationEnabled: Boolean read FMyLocationEnabled write SetMyLocationEnabled;
    property OnMarkerClick: TOnMarkerClick read FOnMarkerClick write FOnMarkerClick;
    property OnMapLoaded: TOnMapLoaded read FOnMapLoaded write FOnMapLoaded;
  end;

implementation

{ TDPFJMapView }

constructor TDPFJMapView.Create(AOwner: TComponent);
{$IFDEF ANDROID}
var
  MapOptions: JGoogleMapOptions;
{$ENDIF}
begin
  inherited;
  ControlCaption := 'MapView';
  FMyLocationEnabled := False;

{$IFDEF ANDROID}
  FMarkers := TDictionary<Integer, JMarker>.Create;

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      MapOptions := TJGoogleMapOptions.JavaClass.init;
      MapOptions.mapType(TJGoogleMap.JavaClass.MAP_TYPE_NORMAL);
      MapOptions.compassEnabled(False);
      MapOptions.rotateGesturesEnabled(True);
      MapOptions.tiltGesturesEnabled(True);

      // Use this class to initialize the Google Maps Android API if features
      // need to be used before obtaining a map. It must be called because
      // some classes such as BitmapDescriptorFactory and CameraUpdateFactory
      // need to be initialized.
      TJMapsInitializer.JavaClass.initialize(SharedActivity);

      FJMapView := TJMapView.JavaClass.init(SharedActivity, MapOptions);
      FJMapView.onCreate(TJBundle.JavaClass.init);
      FJMapView.onResume;
    end);
  JControl := FJMapView;
{$ENDIF}
end;

destructor TDPFJMapView.Destroy;
begin
{$IFDEF ANDROID}
  FJMapView.onPause;
  FJMapView.onDestroy;

  if Assigned(FGoogleMap_OnMarkerClickListener) then
    FGoogleMap_OnMarkerClickListener := nil;

  FMarkers.Free;
{$ENDIF}
  inherited;
end;

procedure TDPFJMapView.SetMyLocationEnabled(const Value: Boolean);
begin
  FMyLocationEnabled := Value;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FJMapView.getMap.setMyLocationEnabled(Value);
    end);
{$ENDIF}
end;

{$IFDEF ANDROID}
procedure TDPFJMapView.AddMarker(MarkerId: Integer; Lat, Lng: Double; Icon: JBitmapDescriptor;
  const Title: string);
begin
  CallInUIThread(
    procedure
    var
      Marker: JMarker;
      MarkerOptions: JMarkerOptions;
      LatLng: JLatLng;
    begin
      LatLng := TJLatLng.JavaClass.init(Lat, Lng);

      MarkerOptions := TJMarkerOptions.JavaClass.init;
      MarkerOptions.position(LatLng);

      if not Title.IsEmpty then
        MarkerOptions.title(StringToJString(Title));

      if Assigned(Icon) then
        MarkerOptions.icon(Icon);

      Marker := FJMapView.getMap.addMarker(MarkerOptions);

      FMarkers.Add(MarkerId, Marker);
    end);
end;

procedure TDPFJMapView.AddPolyline(Options: JPolylineOptions);
begin
  CallInUIThread(
    procedure
    begin
      FJMapView.getMap.addPolyline(Options);
    end);
end;

procedure TDPFJMapView.Clear;
begin
  CallInUIThread(
    procedure
    begin
      FJMapView.getMap.clear;
    end);

  FMarkers.Clear;
end;

procedure TDPFJMapView.RemoveMarker(MarkerId: Integer);
begin
  CallInUIThread(
    procedure
    begin
      FMarkers[MarkerId].remove;
    end);
  FMarkers.Remove(MarkerId);
end;

procedure TDPFJMapView.SetCameraPosition(Lat, Lng: Double; Zoom: Single);
begin
  CallInUIThread(
    procedure
    var
      CameraPosition: JCameraPosition;
      CameraUpdate: JCameraUpdate;
      LatLng: JLatLng;
    begin
      LatLng := TJLatLng.JavaClass.init(Lat, Lng);
      CameraPosition := TJCameraPosition.JavaClass.init(LatLng, Zoom, 0, 0);
      CameraUpdate := TJCameraUpdateFactory.JavaClass.newCameraPosition(CameraPosition);

      FJMapView.getMap.moveCamera(CameraUpdate);
    end);
end;

procedure TDPFJMapView.Loaded;
begin
  addSubview(Self, ParentControl);

  inherited;

  SetMyLocationEnabled(FMyLocationEnabled);

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      if not Assigned(FGoogleMap_OnMarkerClickListener) then
        FGoogleMap_OnMarkerClickListener := TDPFGoogleMap_OnMarkerClickListener.Create(Self);
      FJMapView.getMap.setOnMarkerClickListener(FGoogleMap_OnMarkerClickListener);
    end);

  if Assigned(FOnMapLoaded) then
    FOnMapLoaded(Self);
end;

function TDPFJMapView.MarkerClick(Marker: JMarker): Boolean;

  function GetMarkerIndex: Integer;
  var
    MarkerPair: TPair<Integer, JMarker>;
  begin
    Result := -1;
    for MarkerPair in FMarkers do
      if MarkerPair.Value.equals(Marker) then
      begin
        Result := MarkerPair.Key;
        Break;
      end;
  end;

begin
  Result := False;

  if Assigned(FOnMarkerClick) then
    FOnMarkerClick(Self, GetMarkerIndex, Result);
end;

{$ENDIF}

{$IFDEF ANDROID}
{ TGoogleMap_OnMarkerClickListener }

constructor TDPFGoogleMap_OnMarkerClickListener.Create(MapControl: TDPFJMapView);
begin
  inherited Create;
  FMapControl := MapControl;
end;

function TDPFGoogleMap_OnMarkerClickListener.onMarkerClick(P1: JMarker): Boolean;
begin
  Result := FMapControl.MarkerClick(P1);
end;
{$ENDIF}

end.
