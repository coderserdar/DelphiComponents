// ------------------------------------------------------------------------------
// DPF.iOS.MobileCoreServices Class
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
unit DPF.iOS.MobileCoreServices;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIImageView,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
  DPF.iOS.MFMailComposeViewController,
{$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
const
  libMobileCoreServices = '/System/Library/Frameworks/MobileCoreServices.framework/MobileCoreServices';

  // ------------------------------------------------------------------------------

  // ------------------------------------------------------------------------------
  // UTI Image Content Types
  // ------------------------------------------------------------------------------
function kUTTypeImage: NSString;
function kUTTypeJPEG: NSString;
function kUTTypeJPEG2000: NSString;
function kUTTypeTIFF: NSString;
function kUTTypePICT: NSString;
function kUTTypeGIF: NSString;
function kUTTypePNG: NSString;
function kUTTypeQuickTimeImage: NSString;
function kUTTypeAppleICNS: NSString;
function kUTTypeBMP: NSString;
function kUTTypeICO: NSString;

// ------------------------------------------------------------------------------
// UTI Audio Visual Content Types
// ------------------------------------------------------------------------------
function kUTTypeAudiovisualContent: NSString;
function kUTTypeMovie: NSString;
function kUTTypeVideo: NSString;
function kUTTypeAudio: NSString;
function kUTTypeQuickTimeMovie: NSString;
function kUTTypeMPEG: NSString;
function kUTTypeMPEG4: NSString;
function kUTTypeMP3: NSString;
function kUTTypeMPEG4Audio: NSString;
function kUTTypeAppleProtectedMPEG4Audio: NSString;

// ------------------------------------------------------------------------------
// UTI Directory Types
// ------------------------------------------------------------------------------
function kUTTypeFolder: NSString;
function kUTTypeVolume: NSString;
function kUTTypePackage: NSString;
function kUTTypeBundle: NSString;
function kUTTypeFramework: NSString;

{$ENDIF}

implementation

// ------------------------------------------------------------------------------

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

{$ENDIF}

  // ------------------------------------------------------------------------------
  // UTI Image Content Types
  // ------------------------------------------------------------------------------
function kUTTypeImage: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeImage' );
end;

// ------------------------------------------------------------------------------
function kUTTypeJPEG: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeJPEG' );
end;

// ------------------------------------------------------------------------------
function kUTTypeJPEG2000: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeJPEG2000' );
end;

// ------------------------------------------------------------------------------
function kUTTypeTIFF: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeTIFF' );
end;

// ------------------------------------------------------------------------------
function kUTTypePICT: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypePICT' );
end;

// ------------------------------------------------------------------------------
function kUTTypeGIF: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeGIF' );
end;

// ------------------------------------------------------------------------------
function kUTTypePNG: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypePNG' );
end;

// ------------------------------------------------------------------------------
function kUTTypeQuickTimeImage: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeQuickTimeImage' );
end;

// ------------------------------------------------------------------------------
function kUTTypeAppleICNS: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeAppleICNS' );
end;

// ------------------------------------------------------------------------------
function kUTTypeBMP: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeBMP' );
end;

// ------------------------------------------------------------------------------
function kUTTypeICO: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeICO' );
end;

// ------------------------------------------------------------------------------
// UTI Audio Visual Content Types
// ------------------------------------------------------------------------------
function kUTTypeAudiovisualContent: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeAudiovisualContent' );
end;

// ------------------------------------------------------------------------------
function kUTTypeMovie: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeMovie' );
end;

// ------------------------------------------------------------------------------
function kUTTypeVideo: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeVideo' );
end;

// ------------------------------------------------------------------------------
function kUTTypeAudio: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeAudio' );
end;

// ------------------------------------------------------------------------------
function kUTTypeQuickTimeMovie: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeQuickTimeMovie' );
end;

// ------------------------------------------------------------------------------
function kUTTypeMPEG: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeMPEG' );
end;

// ------------------------------------------------------------------------------
function kUTTypeMPEG4: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeMPEG4' );
end;

// ------------------------------------------------------------------------------
function kUTTypeMP3: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeMP3' );
end;

// ------------------------------------------------------------------------------
function kUTTypeMPEG4Audio: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeMPEG4Audio' );
end;

// ------------------------------------------------------------------------------
function kUTTypeAppleProtectedMPEG4Audio: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeAppleProtectedMPEG4Audio' );
end;

// ------------------------------------------------------------------------------
// UTI Directory Types
// ------------------------------------------------------------------------------
function kUTTypeFolder: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeFolder' );
end;

// ------------------------------------------------------------------------------
function kUTTypeVolume: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeVolume' );
end;

// ------------------------------------------------------------------------------
function kUTTypePackage: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypePackage' );
end;

// ------------------------------------------------------------------------------
function kUTTypeBundle: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeBundle' );
end;

// ------------------------------------------------------------------------------
function kUTTypeFramework: NSString;
begin
  Result := CocoaNSStringConst( libMobileCoreServices, 'kUTTypeFramework' );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
end.
