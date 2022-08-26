// ------------------------------------------------------------------------------
// DPF.Android.OS Component
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
unit DPF.Android.OS;

interface

{$I DPF.ANDROID.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
{$IFDEF ANDROID}
  Androidapi.JNI.OS,
  Androidapi.JNI.JavaTypes,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.Log,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$ELSE}
  DPF.ANDROID.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

{$IFDEF ANDROID}

type

  JSystemClock = interface;

  // ----------------------------------------------------------------------------
  // JChronometer Class
  // ----------------------------------------------------------------------------
  JSystemClockClass = interface( JObjectClass )
    ['{6994993B-B74C-48E5-8C4C-85C35777B499}']

    function currentTimeMillis: int64; cdecl;
    function uptimeMillis: int64; cdecl;
    function currentThreadTimeMillis: int64; cdecl;
    function elapsedRealtime: int64; cdecl;
    function elapsedRealtimeNanos: int64; cdecl;
    function setCurrentTimeMillis( millis: int64 ): boolean; cdecl;
    procedure sleep( ms: int64 ); cdecl;
  end;

  [JavaSignature( 'android/OS/SystemClock' )]
  JSystemClock = interface( JObject )
    ['{9CF240C7-5960-4202-A3EF-00B1BE4B26E2}']

  end;

  TJSystemClock = class( TJavaGenericImport<JSystemClockClass, JSystemClock> )
  end;

  // ----------------------------------------------------------------------------
  // JEnvironmen Class
  // ----------------------------------------------------------------------------
  JEnvironmentClass = interface( JObjectClass )
    ['{9090E798-E8CC-4959-99EA-D93C8FCF7C51}']

    function _GetMEDIA_BAD_REMOVAL: JString; cdecl;
    property MEDIA_BAD_REMOVAL: JString read _GetMEDIA_BAD_REMOVAL;

    function _GetMEDIA_CHECKING: JString; cdecl;
    property MEDIA_CHECKING: JString read _GetMEDIA_CHECKING;

    function _GetMEDIA_MOUNTED: JString; cdecl;
    property MEDIA_MOUNTED: JString read _GetMEDIA_MOUNTED;

    function _GetMEDIA_MOUNTED_READ_ONLY: JString; cdecl;
    property MEDIA_MOUNTED_READ_ONLY: JString read _GetMEDIA_MOUNTED_READ_ONLY;

    function _GetMEDIA_NOFS: JString; cdecl;
    property MEDIA_NOFS: JString read _GetMEDIA_NOFS;

    function _GetMEDIA_REMOVED: JString; cdecl;
    property MEDIA_REMOVED: JString read _GetMEDIA_REMOVED;

    function _GetMEDIA_SHARED: JString; cdecl;
    property MEDIA_SHARED: JString read _GetMEDIA_SHARED;

    function _GetMEDIA_UNKNOWN: JString; cdecl;
    property MEDIA_UNKNOWN: JString read _GetMEDIA_UNKNOWN;

    function _GetMEDIA_UNMOUNTABLE: JString; cdecl;
    property MEDIA_UNMOUNTABLE: JString read _GetMEDIA_UNMOUNTABLE;

    function _GetMEDIA_UNMOUNTED: JString; cdecl;
    property MEDIA_UNMOUNTED: JString read _GetMEDIA_UNMOUNTED;

    function getDataDirectory( ): JFile; cdecl;
    function getDownloadCacheDirectory( ): JFile; cdecl;
    function getExternalStorageDirectory( ): JFile; cdecl;
    function getRootDirectory( ): JFile; cdecl;
    function getExternalStorageState( ): JString; cdecl;
    function getStorageState( path: JFile ): JString; cdecl;
    function isExternalStorageEmulated( ): Boolean; cdecl;
    function isExternalStorageRemovable( ): Boolean; cdecl;
    function getExternalStoragePublicDirectory( &type: JString ): JFile; cdecl;
  end;

  [JavaSignature( 'android/os/Environment' )]
  JEnvironmen = interface( JObject )
    ['{84F9C03C-50C5-47BB-9E1C-1C2AD003E6B3}']

  end;

  TJEnvironmen = class( TJavaGenericImport<JEnvironmentClass, JEnvironmen> )
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
end.
