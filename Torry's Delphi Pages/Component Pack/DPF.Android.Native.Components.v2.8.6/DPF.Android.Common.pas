// ------------------------------------------------------------------------------
// DPF.Android.Common Tools & Classes
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
unit DPF.Android.Common;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.Classes,
  System.Generics.Collections,
  FMX.Consts,
  FMX.Types,
  FMX.Types3D,
  FMX.Platform,
{$IFDEF DELPHIXE5}
  FMX.Graphics,
{$ENDIF}
  DateUtils,
{$IFDEF ANDROID}
  DPF.Android.Net,
  DPF.Android.OS,
  DPF.Android.DPFUtils,
  Soap.EncdDecd,
  Androidapi.JNI.Location,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ENDIF}
  FMX.Forms;

{$IFDEF Android}

type
  TDeviceInfo = record
    DeviceID: string;
    DeviceSoftwareVersion: string;
    Line1Number: string;
    NetworkOperator: string;
    NetworkOperatorName: string;
    NetworkCountryIso: string;
    SimOperator: string;
    SimOperatorName: string;
    SimCountryIso: string;
    SimSerialNumber: string;
    SubscriberId: string;
  end;

  TDeviceLocation = record
    Longitude: single;
    Latitude: single;
    Altitude: single;
    Accuracy: single;
    Bearing: single;
    Speed: single;
    Time: Int64;
    Provider: string;
  end;

function GetAppVersion: string;
function GetAppFolder: string;
function GetDataDirectory: string;
function GetExternalStorageDirectory: string;
function isExternalStorageAvalable: Integer;
function GetUniqueViewID: Integer;
function CheckPermission( const permissionName: string ): boolean;
function GetDeviceInfo: TDeviceInfo;
function GetDeviceLocation: TDeviceLocation;
function GetWifiMacAddress: string;
function GetTime( ttype: byte = 0 ): string;
function GetIPAddress( useIPv4: Boolean ): string;
function ArrayToJArray( ar: array of byte ): TJavaArray<byte>;

{$ENDIF}

implementation

var
  UniqueValue: Integer = $00FFFFFF; // ID number larger than 0x00FFFFFF is reserved for static views defined in the /res xml files

{$IFDEF Android}

  // ------------------------------------------------------------------------------
function GetUniqueViewID: Integer;
begin
  result := UniqueValue;
  Dec( UniqueValue );
end;

// ------------------------------------------------------------------------------

function GetAppVersion: string;
begin

end;

// ------------------------------------------------------------------------------
// Deployment Remote path:
// assets\internal\
function GetAppFolder: string;
begin
  result := IncludeTrailingBackslash( JStringToString( SharedActivity.getApplicationContext.getFilesDir.getAbsolutePath ) );
end;

// ------------------------------------------------------------------------------
function GetDataDirectory: string;
begin
  result := IncludeTrailingPathDelimiter( JStringToString( TJEnvironmen.JavaClass.getDataDirectory.getPath ) );
end;

// ------------------------------------------------------------------------------
function GetExternalStorageDirectory: string;
begin
  result := IncludeTrailingPathDelimiter( JStringToString( TJEnvironmen.JavaClass.getExternalStorageDirectory.getPath ) );
end;

// ------------------------------------------------------------------------------
// Result = 0 : Not Available
// Result = 1 : Available & Writeable
// Result = 2 : Available & Readonly
function isExternalStorageAvalable: Integer;
var
  State: JString;
begin
  result := 0;
  State  := TJEnvironmen.JavaClass.getExternalStorageState;
  if ( TJEnvironmen.JavaClass.MEDIA_MOUNTED.equals( state ) ) then
    // We can read and write the media
    result := 1
  else if TJEnvironmen.JavaClass.MEDIA_MOUNTED_READ_ONLY.equals( state ) then
    // We can only read the media
    result := 2;
end;

// ------------------------------------------------------------------------------
// android.permission.ACCESS_CHECKIN_PROPERTIES
// android.permission.ACCESS_COARSE_LOCATION
// android.permission.ACCESS_FINE_LOCATION
// android.permission.ACCESS_LOCATION_EXTRA_COMMANDS
// android.permission.ACCESS_MOCK_LOCATION
// android.permission.ACCESS_NETWORK_STATE
// android.permission.ACCESS_SURFACE_FLINGER
// android.permission.ACCESS_WIFI_STATE
// android.permission.ACCOUNT_MANAGER
// android.permission.ADD_VOICEMAIL
// android.permission.AUTHENTICATE_ACCOUNTS
// android.permission.BATTERY_STATS
// android.permission.BIND_ACCESSIBILITY_SERVICE
// android.permission.BIND_APPWIDGET
// android.permission.BIND_DEVICE_ADMIN
// android.permission.BIND_INPUT_METHOD
// android.permission.BIND_NFC_SERVICE
// android.permission.BIND_NOTIFICATION_LISTENER_SERVICE
// android.permission.BIND_PRINT_SERVICE
// android.permission.BIND_REMOTEVIEWS
// android.permission.BIND_TEXT_SERVICE
// android.permission.BIND_VPN_SERVICE
// android.permission.BIND_WALLPAPER
// android.permission.BLUETOOTH
// android.permission.BLUETOOTH_ADMIN
// android.permission.BLUETOOTH_PRIVILEGED
// android.permission.BRICK
// android.permission.BROADCAST_PACKAGE_REMOVED
// android.permission.BROADCAST_SMS
// android.permission.BROADCAST_STICKY
// android.permission.BROADCAST_WAP_PUSH
// android.permission.CALL_PHONE
// android.permission.CALL_PRIVILEGED
// android.permission.CAMERA
// android.permission.CAPTURE_AUDIO_OUTPUT
// android.permission.CAPTURE_SECURE_VIDEO_OUTPUT
// android.permission.CAPTURE_VIDEO_OUTPUT
// android.permission.CHANGE_COMPONENT_ENABLED_STATE
// android.permission.CHANGE_CONFIGURATION
// android.permission.CHANGE_NETWORK_STATE
// android.permission.CHANGE_WIFI_MULTICAST_STATE
// android.permission.CHANGE_WIFI_STATE
// android.permission.CLEAR_APP_CACHE
// android.permission.CLEAR_APP_USER_DATA
// android.permission.CONTROL_LOCATION_UPDATES
// android.permission.DELETE_CACHE_FILES
// android.permission.DELETE_PACKAGES
// android.permission.DEVICE_POWER
// android.permission.DIAGNOSTIC
// android.permission.DISABLE_KEYGUARD
// android.permission.DUMP
// android.permission.EXPAND_STATUS_BAR
// android.permission.FACTORY_TEST
// android.permission.FLASHLIGHT
// android.permission.FORCE_BACK
// android.permission.GET_ACCOUNTS
// android.permission.GET_PACKAGE_SIZE
// android.permission.GET_TASKS
// android.permission.GET_TOP_ACTIVITY_INFO
// android.permission.GLOBAL_SEARCH
// android.permission.HARDWARE_TEST
// android.permission.INJECT_EVENTS
// android.permission.INSTALL_LOCATION_PROVIDER
// android.permission.INSTALL_PACKAGES
// android.permission.INSTALL_SHORTCUT
// android.permission.INTERNAL_SYSTEM_WINDOW
// android.permission.INTERNET
// android.permission.KILL_BACKGROUND_PROCESSES
// android.permission.LOCATION_HARDWARE
// android.permission.MANAGE_ACCOUNTS
// android.permission.MANAGE_APP_TOKENS
// android.permission.MANAGE_DOCUMENTS
// android.permission.MASTER_CLEAR
// android.permission.MEDIA_CONTENT_CONTROL
// android.permission.MODIFY_AUDIO_SETTINGS
// android.permission.MODIFY_PHONE_STATE
// android.permission.MOUNT_FORMAT_FILESYSTEMS
// android.permission.MOUNT_UNMOUNT_FILESYSTEMS
// android.permission.NFC
// android.permission.PERSISTENT_ACTIVITY
// android.permission.PROCESS_OUTGOING_CALLS
// android.permission.READ_CALENDAR
// android.permission.READ_CALL_LOG
// android.permission.READ_CONTACTS
// android.permission.READ_EXTERNAL_STORAGE
// android.permission.READ_FRAME_BUFFER
// android.permission.READ_HISTORY_BOOKMARKS
// android.permission.READ_INPUT_STATE
// android.permission.READ_LOGS
// android.permission.READ_PHONE_STATE
// android.permission.READ_PROFILE
// android.permission.READ_SMS
// android.permission.READ_SOCIAL_STREAM
// android.permission.READ_SYNC_SETTINGS
// android.permission.READ_SYNC_STATS
// android.permission.READ_USER_DICTIONARY
// android.permission.REBOOT
// android.permission.RECEIVE_BOOT_COMPLETED
// android.permission.RECEIVE_MMS
// android.permission.RECEIVE_SMS
// android.permission.RECEIVE_WAP_PUSH
// android.permission.RECORD_AUDIO
// android.permission.REORDER_TASKS
// android.permission.RESTART_PACKAGES
// android.permission.SEND_RESPOND_VIA_MESSAGE
// android.permission.SEND_SMS
// android.permission.SET_ACTIVITY_WATCHER
// android.permission.SET_ALARM
// android.permission.SET_ALWAYS_FINISH
// android.permission.SET_ANIMATION_SCALE
// android.permission.SET_DEBUG_APP
// android.permission.SET_ORIENTATION
// android.permission.SET_POINTER_SPEED
// android.permission.SET_PREFERRED_APPLICATIONS
// android.permission.SET_PROCESS_LIMIT
// android.permission.SET_TIME
// android.permission.SET_TIME_ZONE
// android.permission.SET_WALLPAPER
// android.permission.SET_WALLPAPER_HINTS
// android.permission.SIGNAL_PERSISTENT_PROCESSES
// android.permission.STATUS_BAR
// android.permission.SUBSCRIBED_FEEDS_READ
// android.permission.SUBSCRIBED_FEEDS_WRITE
// android.permission.SYSTEM_ALERT_WINDOW
// android.permission.TRANSMIT_IR
// android.permission.UNINSTALL_SHORTCUT
// android.permission.UPDATE_DEVICE_STATS
// android.permission.USE_CREDENTIALS
// android.permission.USE_SIP
// android.permission.VIBRATE
// android.permission.WAKE_LOCK
// android.permission.WRITE_APN_SETTINGS
// android.permission.WRITE_CALENDAR
// android.permission.WRITE_CALL_LOG
// android.permission.WRITE_CONTACTS
// android.permission.WRITE_EXTERNAL_STORAGE
// android.permission.WRITE_GSERVICES
// android.permission.WRITE_HISTORY_BOOKMARKS
// android.permission.WRITE_PROFILE
// android.permission.WRITE_SECURE_SETTINGS
// android.permission.WRITE_SETTINGS
// android.permission.WRITE_SMS
// android.permission.WRITE_SOCIAL_STREAM
// android.permission.WRITE_SYNC_SETTINGS
// android.permission.WRITE_USER_DICTIONARY
// ------------------------------------------------------------------------------
function CheckPermission( const permissionName: string ): boolean;
begin
  Result := SharedActivityContext.checkCallingOrSelfPermission( StringToJString( permissionName ) ) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
end;

// ------------------------------------------------------------------------------
function GetDeviceLocation: TDeviceLocation;
var
  dl : JLocationManager;
  l  : JLocation;
  obj: JObject;
begin
  try
    obj := SharedActivityContext.getSystemService( TJContext.JavaClass.LOCATION_SERVICE );
    if assigned( obj ) then
    begin
      dl               := TJLocationManager.Wrap( ( obj as ILocalObject ).GetObjectID );
      l                := dl.getLastKnownLocation( TJLocationManager.JavaClass.GPS_PROVIDER );
      result.Longitude := l.getLongitude;
      result.Longitude := l.getLatitude;
      result.Altitude  := l.getAltitude;
      result.Accuracy  := l.getAccuracy;
      result.Bearing   := l.getBearing;
      result.Speed     := l.getSpeed;
      result.Time      := l.getTime;
      result.Provider  := JStringToString( l.getProvider );
    end
  except
    result.Provider := 'Permission Error: Turn on Access coarse location & Access fine location !';
  end;
end;

// ------------------------------------------------------------------------------
function GetDeviceInfo: TDeviceInfo;
var
  TM: JTelephonyManager;
begin
  TM                           := TJTelephonyManager.Create;
  result.DeviceID              := JStringToString( TM.getDeviceId );
  result.DeviceSoftwareVersion := JStringToString( TM.getDeviceSoftwareVersion );
  result.Line1Number           := JStringToString( TM.getLine1Number );
  result.NetworkOperator       := JStringToString( TM.getNetworkOperator );
  result.NetworkOperatorName   := JStringToString( TM.getNetworkOperatorName );
  result.NetworkCountryIso     := JStringToString( TM.getNetworkCountryIso );
  result.SimOperator           := JStringToString( TM.getSimOperator );
  result.SimOperatorName       := JStringToString( TM.getSimOperatorName );
  result.SimCountryIso         := JStringToString( TM.getSimCountryIso );
  result.SimSerialNumber       := JStringToString( TM.getSimSerialNumber );
  result.SubscriberId          := JStringToString( TM.getSubscriberId );

end;

// ------------------------------------------------------------------------------
function GetWifiMacAddress: string;
var
  obj: JObject;
  w  : JWifiManager;
  ci : JWifiInfo;
begin
  result := '';
  try
    obj := SharedActivityContext.getSystemService( TJContext.JavaClass.WIFI_SERVICE );
    if assigned( obj ) then
    begin
      w      := TJWifiManager.Wrap( ( obj as ILocalObject ).GetObjectID );
      ci     := w.getConnectionInfo;
      result := JStringToString( ci.getMacAddress );
    end;
  except
    result := 'Permission Error: Turn on Access Wifi State !'
  end;
end;

// ------------------------------------------------------------------------------
function GetTime( ttype: byte = 0 ): string;
var
  c: JCalendar;
  d: JDate;
begin
  c := TJCalendar.JavaClass.getInstance( TJTimeZone.JavaClass.getDefault );
  d := C.getTime;
  case ttype of
    0:
      result := format( '%.2d:%.2d:%.2d', [d.getHours, d.getMinutes, d.getSeconds] );
    1:
      result := format( '%.2d:%.2d', [d.getMinutes, d.getSeconds] );
    2:
      result := format( '%.2d:%.2d:%.2d', [d.getHours, d.getMinutes] );
  end;

end;

// ------------------------------------------------------------------------------
function ArrayToJArray( ar: array of byte ): TJavaArray<byte>;
var
  I: Integer;
begin
  result := TJavaArray<byte>.Create( Length( ar ) );

  for I       := 0 to high( ar ) do
    result[i] := ar[i];
end;

// ------------------------------------------------------------------------------
function GetIPAddress( useIPv4: Boolean ): string;
begin
  result := JStringToString( TJDPFUtils.JavaClass.getIPAddress( useIPv4 ) );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
initialization

finalization

end.
