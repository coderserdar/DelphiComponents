unit sensoredgemap;

{ GPS Location API component sample application
Updated by Angus Robertson, Magenta Systems Ltd, England, 21st January 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Display a Google map to plot GPS locations.

This unit is for Delphi 10.4 Sydney and later ujsing the TEdgeBrowser component,
for earlier Delphi version use sensormap.pas which uses the TWebBrowser component
instead, but with Windows 11 now tries to emulate a much older version of MSIE
with Google Maps saying is a obsolete browser.

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, ExtCtrls, ActiveX, IniFiles,
  magsubs4, Winapi.WebView2, Vcl.Edge;

const
  WM_MAP_UPDATE = WM_USER + 321 ;

type
  TFormMap = class(TForm)
    Panel1: TPanel;
    doClose: TButton;
    EditAddr: TEdit;
    doAddress: TButton;
    CheckRealTime: TCheckBox;
    doClear: TButton;
    LabelUpdate: TLabel;
    CheckRoute: TCheckBox;
    EdgeBrowser: TEdgeBrowser;
    procedure doCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doAddressClick(Sender: TObject);
    procedure CheckRealTimeClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
  private
    { Private declarations }
//    HTMLWindow2: IHTMLWindow2;
    procedure WMMAPUPDATE(var Msg : TMessage); message WM_MAP_UPDATE ;
  public
    { Public declarations }
    procedure PlotLatLong;
  end;

var
  FormMap: TFormMap;
  PendingLatLong: boolean = false ;

const

// warning marker icon fails, so temporarily removed
HTMLStr: AnsiString =
'<html> '+#13#10+
'<head> '+#13#10+
'<meta name="viewport" content="initial-scale=1.0, user-scalable=yes" /> '+#13#10+
'<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?key=AIzaSyBz0yAw3zU3cxEQRY1KBD65b8sr1b34uOo&callback=initialize" async defer></script>'+#13#10+
'<script type="text/javascript"> '+#13#10+
''+#13#10+
''+#13#10+
'  var geocoder; '+#13#10+
'  var map;  '+#13#10+
'  var markersArray = [];'+#13#10+
''+#13#10+
''+#13#10+
'  function initialize() { '+
'    geocoder = new google.maps.Geocoder();'+#13#10+
'    var latlng = new google.maps.LatLng(0,0); '+#13#10+
'    var myOptions = { '+#13#10+
'      zoom: 16, '+#13#10+
'      center: latlng, '+#13#10+
'      mapTypeId: google.maps.MapTypeId.ROADMAP '+#13#10+
'    }; '+#13#10+
'    map = new google.maps.Map(document.getElementById("map_canvas"), myOptions); '+#13#10+
'    map.set("streetViewControl", false);'+#13#10+
(*'    google.maps.event.addListener(map, "click", '+#13#10+
'         function(event) '+#13#10+
'                        {'+#13#10+
'                         document.getElementById("LatValue").value = event.latLng.lat(); '+#13#10+
'                         document.getElementById("LngValue").value = event.latLng.lng(); '+#13#10+
'                         PutMarker(document.getElementById("LatValue").value, document.getElementById("LngValue").value,"Clicked") '+#13#10+
'                        } '+#13#10+
'   ); '+#13#10+ *)
''+#13#10+
'  } '+#13#10+
''+#13#10+
''+#13#10+
'  function codeAddress(address) { '+#13#10+
'    if (geocoder) {'+#13#10+
'      geocoder.geocode( { address: address}, function(results, status) { '+#13#10+
'        if (status == google.maps.GeocoderStatus.OK) {'+#13#10+
'          map.setCenter(results[0].geometry.location);'+#13#10+
'          PutMarker(results[0].geometry.location.lat(), results[0].geometry.location.lng(), address);'+#13#10+
'        } else {'+#13#10+
'          alert("Geocode was not successful for the following reason: " + status);'+#13#10+
'        }'+#13#10+
'      });'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
''+#13#10+
''+#13#10+
'  function GotoLatLng(Lat, Lng, Msg) { '+#13#10+
'   var latlng = new google.maps.LatLng(Lat,Lng);'+#13#10+
'   map.setCenter(latlng);'+#13#10+
'   PutMarker2(Lat, Lng, Msg)'+#13#10+
//'   PutMarker(Lat, Lng, Msg)'+#13#10+
'  }'+#13#10+
''+#13#10+
''+#13#10+
'function ClearMarkers() {  '+#13#10+
'  if (markersArray) {        '+#13#10+
'    for (i in markersArray) {  '+#13#10+
'      markersArray[i].setMap(null); '+#13#10+
'    } '+#13#10+
'    markersArray = []; ' +
'  } '+#13#10+
'}  '+#13#10+
''+#13#10+
'  function PutMarker(Lat, Lang, Msg) { '+#13#10+
'   var latlng = new google.maps.LatLng(Lat,Lang);'+#13#10+
'   var marker = new google.maps.Marker({'+#13#10+
'      position: latlng, '+#13#10+
'      map: map,'+#13#10+
'      title: Msg+" ("+Lat+","+Lang+")",'+#13#10+
'      flat: true,'+#13#10+
'      zindex: 0'+#13#10+
'   });'+#13#10+
'   markersArray.push(marker); '+#13#10+
'  }'+#13#10+
''+#13#10+
'  function PutMarker2(Lat, Lang, Msg) { '+#13#10+
'   var latlng = new google.maps.LatLng(Lat,Lang);'+#13#10+
'   var marker = new google.maps.Marker({'+#13#10+
'      position: latlng, '+#13#10+
'//      icon: { path: google.maps.SymbolPath.CIRCLE, scale: 5, strokeColor: ''red'' },'+#13#10+
'      map: map,'+#13#10+
'      title: Msg+" ("+Lat+","+Lang+")",'+#13#10+
'      flat: true,'+#13#10+
'      zindex: 0'+#13#10+
'   });'+#13#10+
'   markersArray.push(marker); '+#13#10+
'  }'+#13#10+
''+#13#10+
''+#13#10+'</script> '+#13#10+
'</head> '+#13#10+
''+#13#10+
//'<body onload="initialize()"> '+#13#10+
'<body> '+#13#10+
'  <div id="map_canvas" style="width:100%; height:100%"></div> '+#13#10+
'  <div id="latlong"> '+#13#10+
'  <input type="hidden" id="LatValue" >'+#13#10+
'  <input type="hidden" id="LngValue" >'+#13#10+
'  </div>  '+#13#10+
''+#13#10+
'</body> '+#13#10+
'</html> '+#13#10;

implementation

{$R *.dfm}

uses sensormain ;

procedure TFormMap.FormCreate(Sender: TObject);
var
    IniFile : TMemIniFile;
    EdgeCacheDir: String;
begin
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        Top := ReadInteger ('MapWindow', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('MapWindow', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('MapWindow', 'Width', Width);
        Height := ReadInteger ('MapWindow', 'Height', Height);
    end;
    IniFile.Free;
    FormMain.Log.Lines.Add ('EdgeBrowserCreate') ;
    EdgeCacheDir := ExtractFilePath(FIniFileName) + '\EdgeCache';
    ForceDirectories(EdgeCacheDir);
    EdgeBrowser.UserDataFolder := EdgeCacheDir;
    EdgeBrowser.CreateWebView;
end;

procedure TFormMap.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TMemIniFile;
begin
    MapOpenFlag := false ;
    Action := caFree ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;


procedure TFormMap.WMMAPUPDATE(var Msg : TMessage);
begin
    PlotLatLong;
end;

procedure TFormMap.PlotLatLong;
begin
    if NOT MapOpenFlag then exit ;
    if (MagGpsLoc.Latitude = 0) and (MagGpsLoc.Longitude = 0) then exit ;
    PendingLatLong := false ;
    FormMain.Log.Lines.Add ('EdgeBrowserPlotLatLong - Plot on Map') ;
    if NOT CheckRoute.Checked then doClearClick (self) ;
    try
        EdgeBrowser.ExecuteScript('GotoLatLng (' + Double2EStr (MagGpsLoc.Latitude, 6) + ',' +
            Double2EStr (MagGpsLoc.Longitude, 6) + ',"' + TimeToStr (Time) + '")') ;
    except
        FormMain.Log.Lines.Add ('Failed ExecScript GotoLatLng');
    end;
end;

procedure TFormMap.CheckRealTimeClick(Sender: TObject);
begin
    if CheckRealTime.Checked then
    begin
        doClearClick (self) ;
        PlotLatLong;
    end;
end;

procedure TFormMap.doAddressClick(Sender: TObject);
var
    Addr: string;
begin
    CheckRealTime.Checked := false ;
    doClearClick (self) ;
    Addr := EditAddr.Text ;
    Addr := StringReplace (StringReplace (Trim(Addr), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]) ;
    try
        EdgeBrowser.ExecuteScript ('codeAddress (' + QuotedStr (Addr) + ')') ;
    except
        FormMain.Log.Lines.Add ('Failed ExecScript codeAddres');
    end;
end;

procedure TFormMap.doClearClick(Sender: TObject);
begin
    try
        EdgeBrowser.ExecuteScript ('ClearMarkers()')
    except
        FormMain.Log.Lines.Add ('Failed ExecScript ClearMarkers');
    end;
end;

procedure TFormMap.doCloseClick(Sender: TObject);
begin
    EdgeBrowser.CloseWebView;
    Close ;
end;

procedure TFormMap.EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
begin
   FormMain.Log.Lines.Add ('EdgeBrowser WebView Completed: ' + IntToStr(AResult));
   if NOT EdgeBrowser.NavigateToString(HTMLStr) then
        FormMain.Log.Lines.Add ('EdgeBrowser-NavigatetoString Failed')
   else if PendingLatLong then
   begin
        PendingLatLong := false ;
        PostMessage (Handle, WM_MAP_UPDATE, 0, 0) ;
        exit ;
    end;
end;


end.
