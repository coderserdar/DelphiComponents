// ------------------------------------------------------------------------------
// DPF.Android.IO Java Classes
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
unit DPF.Android.IO;

interface

{$I DPF.Android.Defs.inc}

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
  DPF.Android.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

{$IFDEF ANDROID}

type

  JFilterInputStream   = interface;
  JBufferedInputStream = interface;

  // ----------------------------------------------------------------------------
  // JFilterInputStream Class
  // ----------------------------------------------------------------------------
  JFilterInputStreamClass = interface( JInputStreamClass )
    ['{98767C27-8846-4954-A88A-4EA60F318943}']

    function init( inp: JInputStream ): JFilterInputStream; cdecl;
  end;

  [JavaSignature( 'java/io/FilterInputStream' )]
  JFilterInputStream = interface( JInputStream )
    ['{A03662FB-D51D-4890-B5A3-427007E120DB}']

  end;

  TJFilterInputStream = class( TJavaGenericImport<JFilterInputStreamClass, JFilterInputStream> )
  end;

  // ----------------------------------------------------------------------------
  // JBufferedInputStream Class
  // ----------------------------------------------------------------------------
  JBufferedInputStreamClass = interface( JFilterInputStreamClass )
    ['{78155B44-8005-42AF-A3AC-697615C3EF5D}']

    function init( inp: JInputStream ): JBufferedInputStream; cdecl; overload;
    function init( inp: JInputStream; size: integer ): JBufferedInputStream; cdecl; overload;
  end;

  [JavaSignature( 'java/io/BufferedInputStream' )]
  JBufferedInputStream = interface( JFilterInputStream )
    ['{FD91B76C-6B9D-4C60-99F5-F07C28509B48}']
  end;

  TJBufferedInputStream = class( TJavaGenericImport<JBufferedInputStreamClass, JBufferedInputStream> )
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
end.
