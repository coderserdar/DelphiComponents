// ------------------------------------------------------------------------------
// DPF.Android.Net Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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

unit DPF.Android.Net;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type
  JURL              = interface;
  JURLStreamHandler = interface;
  JURLConnection    = interface;
  JURI              = interface;

  // ----------------------------------------------------------------------
  // JURLStreamHandler Class
  // ----------------------------------------------------------------------
  JURLStreamHandlerClass = interface( JObjectClass )
    ['{B4F1C7EE-5B0B-4D67-B4A2-D3D953D1C708}']
    function init: JURLConnection; cdecl; overload;
  end;

  [JavaSignature( 'java/net/URLStreamHandler' )]
  JURLStreamHandler = interface( JObject )
    ['{12829A3C-F1DF-4109-A28F-2CC89A007CF6}']
  end;

  TJURLStreamHandler = class( TJavaGenericImport<JURLStreamHandlerClass, JURLStreamHandler> )
  end;

  // ----------------------------------------------------------------------
  // JURI Class
  // ----------------------------------------------------------------------
  JURIClass = interface( JObjectClass )
    ['{8F3A8CD5-B782-479C-B02A-794C4F991907}']

    function init( spec: JString ): JURI; cdecl; overload;
    function init( scheme: JString; schemeSpecificPart: JString; fragment: JString ): JURI; cdecl; overload;
    function init( scheme: JString; userInfo: JString; host: JString; port: integer; path: JString; query: JString; fragment: JString ): JURI; cdecl; overload;
    function init( scheme: JString; host: JString; path: JString; fragment: JString ): JURI; cdecl; overload;
    function init( scheme: JString; authority: string; path: JString; query: JString; fragment: JString ): JURI; cdecl; overload;

    function encode( s: JString ): JString; cdecl; overload;
    function encode( s: JString; allow: JString ): JString; cdecl; overload;
    function decode( s: JString ): integer; cdecl;
  end;

  [JavaSignature( 'java/net/URI' )]
  JURI = interface( JObject )
    ['{9B674F6E-3E81-48E4-BB2E-6136ADB5533F}']
  end;

  TJURI = class( TJavaGenericImport<JURIClass, JURI> )
  end;

  // ----------------------------------------------------------------------
  // JURLConnection Class
  // ----------------------------------------------------------------------
  JURLConnectionClass = interface( JObjectClass )
    ['{D98D92F1-C184-40A3-91A5-C7366B32980E}']
    function init( url: JURL ): JURLConnection; cdecl; overload;
    function getAllowUserInteraction: boolean; cdecl;
  end;

  [JavaSignature( 'java/net/URLConnection' )]
  JURLConnection = interface( JObject )
    ['{B1312058-A490-4CE5-BFFF-1BD5BE114A4C}']

    procedure connect; cdecl;
    function getURL: JURL; cdecl;
    function getConnectTimeout: integer; cdecl;
    function getContent: JObject; cdecl;
    function getContentEncoding: JString; cdecl;
    function getRequestProperty( field: JString ): JString; cdecl;
    function getHeaderFieldKey( posn: Integer ): JString; cdecl;
    function getHeaderField( pos: integer ): JString; cdecl; overload;
    function getHeaderField( key: JString ): JString; cdecl; overload;
    function getContentLength: integer; cdecl;
    function getReadTimeout: integer; cdecl;
    function getHeaderFieldInt( field: JString; defaultValue: Integer ): integer; cdecl;
    function getContentType: JString; cdecl;
    function getDate: Int64; cdecl;
    function getLastModified: Int64; cdecl;
    function getHeaderFieldDate( field: JString; defaultValue: int64 ): Int64; cdecl;
    function getExpiration: Int64; cdecl;
    function getIfModifiedSince: Int64; cdecl;
    function getDefaultUseCaches: boolean; cdecl;
    function getDoInput: boolean; cdecl;
    function getUseCaches: boolean; cdecl;
    function getDoOutput: boolean; cdecl;
    function getInputStream: JInputStream; cdecl;

    procedure setAllowUserInteraction( newValue: boolean ); cdecl;
    procedure setConnectTimeout( timeoutMillis: integer ); cdecl;
    procedure setDoInput( newValue: boolean ); cdecl;
  end;

  TJURLConnection = class( TJavaGenericImport<JURLConnectionClass, JURLConnection> )
  end;

  // ----------------------------------------------------------------------
  // JURL Class
  // ----------------------------------------------------------------------
  JURLClass = interface( JObjectClass )
    ['{1BE949A5-9F11-4B67-8EDB-6D85FDC4666C}']

    function init( spec: JString ): JURL; cdecl; overload;
    function init( context: JURL; spec: JString ): JURL; cdecl; overload;
    function init( context: JURL; spec: JString; handler: JURLStreamHandler ): JURL; cdecl; overload;
    function init( protocol: JString; host: JString; &file: JString ): JURL; cdecl; overload;
    function init( protocol: JString; host: JString; port: integer; &file: JString ): JURL; cdecl; overload;
    function init( protocol: JString; host: JString; port: integer; &file: JString; handler: JURLStreamHandler ): JURL; cdecl; overload;
  end;

  [JavaSignature( 'java/net/URL' )]
  JURL = interface( JObject )
    ['{EB4F9273-48C6-40CE-A0CE-A01E202335E4}']

    function getDefaultPort: integer; cdecl;
    function getPort: integer; cdecl;
    function getHost: JString; cdecl;
    function getFile: JString; cdecl;
    function getPath: JString; cdecl;
    function getQuery: JString; cdecl;
    function getProtocol: JString; cdecl;
    function getRef: JString; cdecl;
    function getUserInfo: JString; cdecl;
    function getAuthority: JString; cdecl;
    function getContent: JObject; cdecl;
    function hashCode: integer; cdecl;
    function openConnection: JURLConnection; cdecl;
    function sameFile( otherURL: JURL ): boolean; cdecl;
    function toString: JString; cdecl;
    function toURI: JURI; cdecl;
  end;

  TJURL = class( TJavaGenericImport<JURLClass, JURL> )
  end;

  // ----------------------------------------------------------------------
  // JURL Class
  // ----------------------------------------------------------------------
  JWifiInfoClass = interface( JObjectClass )
    ['{ACEA65AC-CDBF-4103-8F6C-41962F275CB6}']

  end;

  [JavaSignature( 'android/net/wifi/WifiInfo' )]
  JWifiInfo = interface( JObject )
    ['{BE6354C4-378C-4489-8306-396A94C20ED0}']

    function getBSSID: JString; cdecl;
    function getHiddenSSID: boolean; cdecl;
    function getIpAddress: integer; cdecl;
    function getLinkSpeed: integer; cdecl;
    function getMacAddress: JString; cdecl;
    function getNetworkId: integer; cdecl;
    function getRssi: integer; cdecl;
    function getSSID: JString; cdecl;
    function toString: JString; cdecl;
  end;

  TJWifiInfo = class( TJavaGenericImport<JWifiInfoClass, JWifiInfo> )
  end;

  // ----------------------------------------------------------------------
  // JWifiManager Class
  // ----------------------------------------------------------------------
  JWifiManagerClass = interface( JObjectClass )
    ['{39D4FD06-9B1D-4BF3-A19C-678A8FF255EC}']

  end;

  [JavaSignature( 'android/net/wifi/WifiManager' )]
  JWifiManager = interface( JURLConnection )
    ['{DC8A5BBF-6F50-466D-B310-88762129CE8D}']

    function _GetACTION_PICK_WIFI_NETWORK: JString;
    function _GetACTION_REQUEST_SCAN_ALWAYS_AVAILABLE: JString;
    function _GetERROR_AUTHENTICATING: integer;
    function _GetEXTRA_BSSID: JString;
    function _GetEXTRA_NETWORK_INFO: JString;
    function _GetEXTRA_NEW_RSSI: JString;
    function _GetEXTRA_NEW_STATE: JString;
    function _GetEXTRA_PREVIOUS_WIFI_STATE: JString;
    function _GetEXTRA_SUPPLICANT_CONNECTED: JString;
    function _GetEXTRA_SUPPLICANT_ERROR: JString;
    function _GetEXTRA_WIFI_INFO: JString;
    function _GetEXTRA_WIFI_STATE: JString;
    function _GetNETWORK_IDS_CHANGED_ACTION: JString;
    function _GetNETWORK_STATE_CHANGED_ACTION: JString;
    function _GetRSSI_CHANGED_ACTION: JString;
    function _GetSCAN_RESULTS_AVAILABLE_ACTION: JString;
    function _GetSUPPLICANT_CONNECTION_CHANGE_ACTION: JString;
    function _GetSUPPLICANT_STATE_CHANGED_ACTION: JString;
    function _GetWIFI_MODE_FULL: integer;
    function _GetWIFI_MODE_FULL_HIGH_PERF: integer;
    function _GetWIFI_MODE_SCAN_ONLY: integer;
    function _GetWIFI_STATE_CHANGED_ACTION: JString;
    function _GetWIFI_STATE_DISABLED: integer;
    function _GetWIFI_STATE_DISABLING: integer;
    function _GetWIFI_STATE_ENABLED: integer;
    function _GetWIFI_STATE_ENABLING: integer;
    function _GetWIFI_STATE_UNKNOWN: integer;

    property ACTION_PICK_WIFI_NETWORK: JString read _GetACTION_PICK_WIFI_NETWORK;
    property ACTION_REQUEST_SCAN_ALWAYS_AVAILABLE: JString read _GetACTION_REQUEST_SCAN_ALWAYS_AVAILABLE;
    property ERROR_AUTHENTICATING: integer read _GetERROR_AUTHENTICATING;
    property EXTRA_BSSID: JString read _GetEXTRA_BSSID;
    property EXTRA_NETWORK_INFO: JString read _GetEXTRA_NETWORK_INFO;
    property EXTRA_NEW_RSSI: JString read _GetEXTRA_NEW_RSSI;
    property EXTRA_NEW_STATE: JString read _GetEXTRA_NEW_STATE;
    property EXTRA_PREVIOUS_WIFI_STATE: JString read _GetEXTRA_PREVIOUS_WIFI_STATE;
    property EXTRA_SUPPLICANT_CONNECTED: JString read _GetEXTRA_SUPPLICANT_CONNECTED;
    property EXTRA_WIFI_INFO: JString read _GetEXTRA_WIFI_INFO;
    property EXTRA_WIFI_STATE: JString read _GetEXTRA_WIFI_STATE;
    property NETWORK_IDS_CHANGED_ACTION: JString read _GetNETWORK_IDS_CHANGED_ACTION;
    property NETWORK_STATE_CHANGED_ACTION: JString read _GetNETWORK_STATE_CHANGED_ACTION;
    property RSSI_CHANGED_ACTION: JString read _GetRSSI_CHANGED_ACTION;
    property SCAN_RESULTS_AVAILABLE_ACTION: JString read _GetSCAN_RESULTS_AVAILABLE_ACTION;
    property SUPPLICANT_CONNECTION_CHANGE_ACTION: JString read _GetSUPPLICANT_CONNECTION_CHANGE_ACTION;
    property SUPPLICANT_STATE_CHANGED_ACTION: JString read _GetSUPPLICANT_STATE_CHANGED_ACTION;
    property WIFI_MODE_FULL: integer read _GetWIFI_MODE_FULL;
    property WIFI_MODE_FULL_HIGH_PERF: integer read _GetWIFI_MODE_FULL_HIGH_PERF;
    property WIFI_MODE_SCAN_ONLY: integer read _GetWIFI_MODE_SCAN_ONLY;
    property WIFI_STATE_CHANGED_ACTION: JString read _GetWIFI_STATE_CHANGED_ACTION;
    property WIFI_STATE_DISABLED: integer read _GetWIFI_STATE_DISABLED;
    property WIFI_STATE_DISABLING: integer read _GetWIFI_STATE_DISABLING;
    property WIFI_STATE_ENABLED: integer read _GetWIFI_STATE_ENABLED;
    property WIFI_STATE_ENABLING: integer read _GetWIFI_STATE_ENABLING;
    property WIFI_STATE_UNKNOWN: integer read _GetWIFI_STATE_UNKNOWN;

    function calculateSignalLevel( rssi: integer; numLevels: integer ): integer; cdecl;
    function disconnect: boolean; cdecl;
    function disableNetwork( netId: integer ): boolean; cdecl;
    function enableNetwork( netId: integer; disableOthers: boolean ): boolean; cdecl;
    function getWifiState: integer; cdecl;
    function isScanAlwaysAvailable: boolean; cdecl;
    function isWifiEnabled: boolean; cdecl;
    function pingSupplicant: boolean; cdecl;
    function reassociate: boolean; cdecl;
    function reconnect: boolean; cdecl;
    function removeNetwork( netId: integer ): boolean; cdecl;
    function saveConfiguration: boolean; cdecl;
    function setWifiEnabled( enabled: boolean ): boolean; cdecl;
    function startScan: boolean; cdecl;
    function getConnectionInfo: JWifiInfo; cdecl;
  end;

  TJWifiManager = class( TJavaGenericImport<JWifiManagerClass, JWifiManager> )
  end;

  // ----------------------------------------------------------------------
  // JHttpURLConnection Class
  // ----------------------------------------------------------------------
  JHttpURLConnectionClass = interface( JURLConnectionClass )
    ['{BA9FABB8-1444-46F3-9950-EE1054CECE61}']

  end;

  [JavaSignature( 'java/net/HttpURLConnection' )]
  JHttpURLConnection = interface( JURLConnection )
    ['{F5F417E2-EB99-4EA1-A391-243F2363D146}']
  end;

  TJHttpURLConnection = class( TJavaGenericImport<JHttpURLConnectionClass, JHttpURLConnection> )
  end;

implementation

begin

end.
