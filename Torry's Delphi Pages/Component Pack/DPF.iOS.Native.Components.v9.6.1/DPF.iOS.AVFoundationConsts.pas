// ------------------------------------------------------------------------------
// DPF.iOS.AVFoundationConsts Reference
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
unit DPF.iOS.AVFoundationConsts;

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
  iOSapi.AVFoundation,
{$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
// AV Foundation Constants Reference
// ------------------------------------------------------------------------------
function AVVideoAverageBitRateKey: NSString;
function AVVideoCodecH264: NSString;
function AVVideoCodecKey: NSString;
function AVVideoWidthKey: NSString;
function AVVideoHeightKey: NSString;
function AVMediaTypeVideo: NSString;
function AVChannelLayoutKey: NSString;
function AVNumberOfChannelsKey: NSString;
function AVVideoCompressionPropertiesKey: NSString;

// ------------------------------------------------------------------------------
// Export Preset Names for Device-Appropriate QuickTime Files
// ------------------------------------------------------------------------------
function AVAssetExportPresetLowQuality: NSString;
function AVAssetExportPresetMediumQuality: NSString;
function AVAssetExportPresetHighestQuality: NSString;

// ------------------------------------------------------------------------------
// File Format UTIs
// ------------------------------------------------------------------------------
function AVFileType3GPP: NSString;
function AVFileType3GPP2: NSString;
function AVFileTypeAIFC: NSString;
function AVFileTypeAIFF: NSString;
function AVFileTypeAMR: NSString;
function AVFileTypeAC3: NSString;
function AVFileTypeMPEGLayer3: NSString;
function AVFileTypeSunAU: NSString;
function AVFileTypeCoreAudioFormat: NSString;
function AVFileTypeAppleM4V: NSString;
function AVFileTypeMPEG4: NSString;
function AVFileTypeAppleM4A: NSString;
function AVFileTypeQuickTimeMovie: NSString;
function AVFileTypeWAVE: NSString;

// ------------------------------------------------------------------------------
// File Format UTIs
// ------------------------------------------------------------------------------
function AVEncoderAudioQualityKey: NSString;
function AVEncoderBitRateKey: NSString;
function AVEncoderBitRatePerChannelKey: NSString;
function AVEncoderBitRateStrategyKey: NSString;
function AVEncoderBitDepthHintKey: NSString;

// ------------------------------------------------------------------------------
// Video Settings
// ------------------------------------------------------------------------------
function AVVideoMaxKeyFrameIntervalKey: NSString;
function AVCaptureSessionPresetPhoto: NSString;
function AVCaptureSessionPresetHigh: NSString;
function AVCaptureSessionPresetMedium: NSString;
function AVCaptureSessionPresetLow: NSString;
function AVCaptureSessionPreset320x240: NSString;
function AVCaptureSessionPreset352x288: NSString;
function AVCaptureSessionPreset640x480: NSString;
function AVCaptureSessionPreset960x540: NSString;
function AVCaptureSessionPreset1280x720: NSString;
function AVCaptureSessionPreset1920x1080: NSString;
function AVCaptureSessionPresetiFrame960x540: NSString;
function AVCaptureSessionPresetiFrame1280x720: NSString;

// ------------------------------------------------------------------------------
// Video Gravity
// ------------------------------------------------------------------------------
function AVLayerVideoGravityResize: NSString;
function AVLayerVideoGravityResizeAspect: NSString;
function AVLayerVideoGravityResizeAspectFill: NSString;

{$ENDIF}

implementation

// ------------------------------------------------------------------------------

{$IFDEF IOS}

// ------------------------------------------------------------------------------
// AV Foundation Constants Reference
// ------------------------------------------------------------------------------
function AVVideoAverageBitRateKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoAverageBitRateKey' );
end;

// ------------------------------------------------------------------------------
function AVVideoCodecH264: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoCodecH264' );
end;

// ------------------------------------------------------------------------------
function AVVideoCodecKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoCodecKey' );
end;

// ------------------------------------------------------------------------------
function AVVideoWidthKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoWidthKey' );
end;

// ------------------------------------------------------------------------------
function AVVideoHeightKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoHeightKey' );
end;

// ------------------------------------------------------------------------------
function AVMediaTypeVideo: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMediaTypeVideo' );
end;

// ------------------------------------------------------------------------------
function AVChannelLayoutKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVChannelLayoutKey' );
end;

// ------------------------------------------------------------------------------
function AVNumberOfChannelsKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVNumberOfChannelsKey' );
end;

// ------------------------------------------------------------------------------
function AVVideoCompressionPropertiesKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoCompressionPropertiesKey' );
end;

// ------------------------------------------------------------------------------
// Export Preset Names for Device-Appropriate QuickTime Files
// ------------------------------------------------------------------------------
function AVAssetExportPresetLowQuality: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVAssetExportPresetLowQuality' );
end;

// ------------------------------------------------------------------------------
function AVAssetExportPresetMediumQuality: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVAssetExportPresetMediumQuality' );
end;

// ------------------------------------------------------------------------------
function AVAssetExportPresetHighestQuality: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVAssetExportPresetHighestQuality' );
end;

// ------------------------------------------------------------------------------
// File Format UTIs
// ------------------------------------------------------------------------------
function AVFileType3GPP: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileType3GPP' );
end;

// ------------------------------------------------------------------------------
function AVFileType3GPP2: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileType3GPP2' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAIFC: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAIFC' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAIFF: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAIFF' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAMR: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAMR' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAC3: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAC3' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeMPEGLayer3: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeMPEGLayer3' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeSunAU: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeSunAU' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeCoreAudioFormat: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeCoreAudioFormat' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAppleM4V: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAppleM4V' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeMPEG4: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeMPEG4' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeAppleM4A: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeAppleM4A' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeQuickTimeMovie: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeQuickTimeMovie' );
end;

// ------------------------------------------------------------------------------
function AVFileTypeWAVE: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeWAVE' );
end;

// ------------------------------------------------------------------------------
// File Format UTIs
// ------------------------------------------------------------------------------
function AVEncoderAudioQualityKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVEncoderAudioQualityKey' );
end;

// ------------------------------------------------------------------------------
function AVEncoderBitRateKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVEncoderBitRateKey' );
end;

// ------------------------------------------------------------------------------
function AVEncoderBitRatePerChannelKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVEncoderBitRatePerChannelKey' );
end;

// ------------------------------------------------------------------------------
function AVEncoderBitRateStrategyKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVEncoderBitRateStrategyKey' );
end;

// ------------------------------------------------------------------------------
function AVEncoderBitDepthHintKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVFileTypeWAVE' );
end;

// ------------------------------------------------------------------------------
// Video Settings
// ------------------------------------------------------------------------------
function AVVideoMaxKeyFrameIntervalKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVVideoMaxKeyFrameIntervalKey' );
end;

// ------------------------------------------------------------------------------
// Video Settings
// ------------------------------------------------------------------------------
function AVCaptureSessionPresetPhoto: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetPhoto' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPresetHigh: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetHigh' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPresetMedium: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetMedium' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPresetLow: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetLow' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset320x240: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset320x240' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset352x288: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset352x288' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset640x480: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset640x480' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset960x540: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset960x540' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset1280x720: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset1280x720' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPreset1920x1080: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPreset1920x1080' );
end;

// ------------------------------------------------------------------------------
function AVCaptureSessionPresetiFrame960x540: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetiFrame960x540' );
end;

function AVCaptureSessionPresetiFrame1280x720: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVCaptureSessionPresetiFrame1280x720' );
end;

// ------------------------------------------------------------------------------
// Video Gravity
// ------------------------------------------------------------------------------
function AVLayerVideoGravityResize: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVLayerVideoGravityResize' );
end;

// ------------------------------------------------------------------------------
function AVLayerVideoGravityResizeAspect: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVLayerVideoGravityResizeAspect' );
end;

// ------------------------------------------------------------------------------
function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVLayerVideoGravityResizeAspectFill' );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
end.
