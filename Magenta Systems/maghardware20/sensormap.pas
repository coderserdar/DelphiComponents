unit sensormap;

{ Magenta GPS and Location Component sample application
Updated by Angus Robertson, Magenta Systems Ltd, England, 2nd February 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Display a Google map to plot GPS locations.

This sample includes a Google maps display for tracking GPS location movement
using the TWebBrowser component which uses the Internet Explorer engine.
Unfortunately Microsoft has removed Internet Explorer from Windows 11 so map
display is more problematic, currently it still works but Google displays a
warning about using non-supported browser and plans to remove support for
MSIE in August 2022. Delphi 10.4 and later has a new TEdgeBrowser component
that is used by the sensoredgemap.pas unit which should be used instead of
sensormap.pas, it is easier to use than TWebBrowser.

}


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, SHDocVw, ExtCtrls, ActiveX, MSHTML, IniFiles,
  magsubs4;

const
  WM_MAP_UPDATE = WM_USER + 321 ;

type
  TFormMap = class(TForm)
    Panel1: TPanel;
    WebBrowser: TWebBrowser;
    doClose: TButton;
    EditAddr: TEdit;
    doAddress: TButton;
    CheckRealTime: TCheckBox;
    doClear: TButton;
    LabelUpdate: TLabel;
    CheckRoute: TCheckBox;
    procedure WebBrowserCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
    procedure doCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doAddressClick(Sender: TObject);
    procedure CheckRealTimeClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
  private
    { Private declarations }
    HTMLWindow2: IHTMLWindow2;
    procedure WMMAPUPDATE(var Msg : TMessage); message WM_MAP_UPDATE ;
  public
    { Public declarations }
    procedure PlotLatLong;
  end;

var
  FormMap: TFormMap;
  PendingLatLong: boolean = false ;

const

(*
// Dec 2016 get script errors using PutMarker2 with circle icon

HTMLStr: AnsiString =
'<html> '+#13#10+
'<head> '+#13#10+
'<meta name="viewport" content="initial-scale=1.0, user-scalable=yes" /> '+#13#10+
//'<script type="text/javascript" src="http://maps.google.com/maps/api/js?v=3&amp;sensor=false&language=en"></script> '+#13#10+
//'<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?key=AIzaSyBz0yAw3zU3cxEQRY1KBD65b8sr1b34uOo"></script>' +#13#10+
//'<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?v=3&key=AIzaSyBz0yAw3zU3cxEQRY1KBD65b8sr1b34uOo&callback=initialize&sensor=true_OR_false" async defer></script>'+#13#10+
'<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?v=3&key=AIzaSyBz0yAw3zU3cxEQRY1KBD65b8sr1b34uOo&callback=initialize" async defer></script>'+#13#10+
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
'    google.maps.event.addListener(map, "click", '+#13#10+
'         function(event) '+#13#10+
'                        {'+#13#10+
'                         document.getElementById("LatValue").value = event.latLng.lat(); '+#13#10+
'                         document.getElementById("LngValue").value = event.latLng.lng(); '+#13#10+
'                         PutMarker(document.getElementById("LatValue").value, document.getElementById("LngValue").value,"Clicked") '+#13#10+
'                        } '+#13#10+
'   ); '+#13#10+
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
//         PutMarker2(results[0].geometry.location.lat(), results[0].geometry.location.lng(), address);'+#13#10+
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
//'   setTimeout (PutMarker, 1000, Lat, Lng, Msg);'+#13#10+
'   PutMarker(Lat, Lng, Msg)'+#13#10+
//  PutMarker2(Lat, Lng, Msg)'+#13#10+
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
'      icon: { path: google.maps.SymbolPath.CIRCLE, scale: 5, strokeColor: ''red'' },'+#13#10+
'      map: map,'+#13#10+
'      title: Msg+" ("+Lat+","+Lang+")",'+#13#10+
'      flat: true,'+#13#10+
'      zindex: 0'+#13#10+
'   });'+#13#10+
'   markersArray.push(marker); '+#13#10+
'  }'+#13#10+
''+#13#10+
''+#13#10+
(* '  function PutMarker3(Lat, Lang, Msg) { '+#13#10+
'   var latlng = new google.maps.LatLng(Lat,Lang);'+#13#10+
'   var mycircle = new { path: google.maps.SymbolPath.CIRCLE, scale: 5, strokeColor: ''red'' };'+#13#10+
'   var marker = new google.maps.Marker({'+#13#10+
'      position: latlng, '+#13#10+
'      icon: mycircle,'+#13#10+
'      map: map,'+#13#10+
'      title: Msg+" ("+Lat+","+Lang+")",'+#13#10+
'      flat: true,'+#13#10+
'      zindex: 0'+#13#10+
'   });'+#13#10+
'   markersArray.push(marker); '+#13#10+
'  }'+#13#10+  *)
(*
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
*)

// July 2017 following from ComCap4, better???
// Sept 2015, warning marker icon fails, so temporarily removed
HTMLStr: AnsiString =
'<html> '+#13#10+
'<head> '+#13#10+
'<meta name="viewport" content="initial-scale=1.0, user-scalable=yes" /> '+#13#10+
//'<script type="text/javascript" src="http://maps.google.com/maps/api/js?v=3.exp&amp;sensor=false&language=en"></script> '+#13#10+
//'<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?key=AIzaSyBz0yAw3zU3cxEQRY1KBD65b8sr1b34uOo"></script>' +#13#10+
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

procedure TFormMap.PlotLatLong;
begin
    if NOT MapOpenFlag then exit ;
    if WebBrowser.ReadyState <> READYSTATE_COMPLETE then
    begin
        FormMain.Log.Lines.Add ('WebBrowserPlotLatLong - No Ready') ;
        PendingLatLong := true ;
        exit ;
    end;
    if (MagGpsLoc.Latitude = 0) and (MagGpsLoc.Longitude = 0) then exit ;
    PendingLatLong := false ;
    FormMain.Log.Lines.Add ('WebBrowserPlotLatLong - Plot on Map') ;
    if NOT CheckRoute.Checked then doClearClick (self) ;
    try
        HTMLWindow2.ExecScript('GotoLatLng (' + Double2EStr (MagGpsLoc.Latitude, 6) + ',' +
            Double2EStr (MagGpsLoc.Longitude, 6) + ',"' + TimeToStr (Time) + '")', 'JavaScript') ;
    except
        FormMain.Log.Lines.Add ('Failed ExecScript GotoLatLng');
    end;
end;

procedure TFormMap.FormCreate(Sender: TObject);
var
    aStream: TMemoryStream;
    IniFile : TMemIniFile;
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
    FormMain.Log.Lines.Add ('WebBrowserNavigate - Blank') ;

// try and use Edge on Delphi 10.4 Sydney, needs extra DLL
// does not seem to work...
{$IF CompilerVersion >= 34}
//    WebBrowser.SelectedEngine := EdgeIfAvailable;
//    if WebBrowser.ActiveEngine = None then
//        FormMain.Log.Lines.Add ('Failed to Set Edge Browser') ;
{$IFEND}

 //   FormMain.Log.Lines.Add (HTMLStr) ;
    WebBrowser.Navigate('about:blank');
    if Assigned (WebBrowser.Document) then
    begin
        aStream := TMemoryStream.Create;
        try
            aStream.WriteBuffer (Pointer (HTMLStr)^, Length (HTMLStr)) ;
            aStream.Seek (0, soFromBeginning) ;
            (WebBrowser.Document as IPersistStreamInit).Load (TStreamAdapter.Create(aStream)) ;
        finally
            aStream.Free;
        end;
        HTMLWindow2 := (WebBrowser.Document as IHTMLDocument2).ParentWindow ;
    end;
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
    if WebBrowser.ReadyState <> READYSTATE_COMPLETE then exit ;
    CheckRealTime.Checked := false ;
    doClearClick (self) ;
    Addr := EditAddr.Text ;
    Addr := StringReplace (StringReplace (Trim(Addr), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]) ;
    try
        HTMLWindow2.ExecScript ('codeAddress (' + QuotedStr (Addr) + ')', 'JavaScript') ;
    except
        FormMain.Log.Lines.Add ('Failed ExecScript codeAddres');
    end;
end;

procedure TFormMap.doClearClick(Sender: TObject);
begin
    if WebBrowser.ReadyState <> READYSTATE_COMPLETE then exit ;
    try
        HTMLWindow2.ExecScript ('ClearMarkers()', 'JavaScript')
    except
        FormMain.Log.Lines.Add ('Failed ExecScript ClearMarkers');
    end;
end;

procedure TFormMap.doCloseClick(Sender: TObject);
begin
    Close ;
end;


// event when map clicked, so we can keep a position
// also called for event changes and mouse movement over map


procedure TFormMap.WebBrowserCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
var
    ADocument: IHTMLDocument2;
    ABody: IHTMLElement2;
    Lat, Long: string;

    function GetIdValue (const Id: string): string ;
    var
        Tag: IHTMLElement ;
        TagsList: IHTMLElementCollection ;
        Index: Integer ;
    begin
        Result := '';
        TagsList := ABody.getElementsByTagName ('input') ;
        for Index := 0 to TagsList.length-1 do
        begin
            Tag := TagsList.item (Index, EmptyParam) As IHTMLElement ;
            if CompareText (Tag.id,Id) = 0 then
                Result := Tag.getAttribute ('value', 0) ;
        end;
    end;

begin
    if NOT MapOpenFlag then exit ;
//    exit ;  // not using events
    if TOleEnum (Command) <> CSC_UPDATECOMMANDS then exit;
    ADocument := WebBrowser.Document as IHTMLDocument2;
//    FormMain.Log.Lines.Add ('WebBrowserCommandStateChange - state ' + IntToStr (WebBrowser.ReadyState)) ;
    if PendingLatLong and (WebBrowser.ReadyState = READYSTATE_COMPLETE) then
    begin
        PendingLatLong := false ;
        PostMessage (Handle, WM_MAP_UPDATE, 0, 0) ;
        exit ;
    end;

    if not Assigned(ADocument) then exit;
    if not Supports (ADocument.Body, IHTMLElement2, ABody) then exit;
    Lat := GetIdValue ('LatValue') ;
    Long := GetIdValue ('LngValue');
  // do something with them!!!
end;


end.
