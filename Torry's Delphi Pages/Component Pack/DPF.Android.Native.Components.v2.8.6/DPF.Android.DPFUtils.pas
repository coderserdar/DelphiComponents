// ------------------------------------------------------------------------------
// DPF.Android.DPFUtils Component
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
unit DPF.Android.DPFUtils;

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

  // ----------------------------------------------------------------------------
  // JDPFUtils Class
  // ----------------------------------------------------------------------------
  JDPFUtilsClass = interface( JObjectClass )
    ['{9D7A04CD-9075-4A08-8C6C-F3E2E60A6308}']

    function bytesToHex( bytes: TJavaArray<byte> ): JString; cdecl;
    function getUTF8Bytes( bytes: JString ): TJavaArray<byte>; cdecl;
    function loadFileAsString( filename: JString ): JString; cdecl;

    // interfaceName eth0, wlan0 or NULL=use first interface
    function getMACAddress(interfaceName: JString ): JString; cdecl;

    // ipv4  true=return ipv4, false=return ipv6
    function getIPAddress(useIPv4: boolean ): JString; cdecl;

    // true = Internet active
    function isInternetActive: boolean; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/DPFUtils' )]
  JDPFUtils = interface( JObject )
    ['{333C5891-2CC8-455A-B828-6EA71B82609C}']

  end;

  TJDPFUtils = class( TJavaGenericImport<JDPFUtilsClass, JDPFUtils> )
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
end.
