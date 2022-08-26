// ------------------------------------------------------------------------------
// DPF.iOS.Media Camera Component (Capture Video + Audio With Native API)
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

unit DPF.iOS.Media;

interface

{$I DPF.iOS.Defs.inc}

uses
{$IFDEF IOS}
  Posix.Stdlib,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  Macapi.Mach,
  iOSapi.UIKit,
  iOSapi.AVFoundation,
  iOSapi.CoreMedia,
  iOSapi.CoreVideo,
  iOSapi.CoreGraphics,
  DPF.iOS.AVFoundationConsts,
  iOSapi.CoreAudio,
  DPF.iOS.Classes,
  DPF.iOS.Common,
  Posix.Unistd,
{$ENDIF}
  DPF.iOS.Dispatch,
  DPF.iOS.UIView,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.TypInfo,
  FMX.Types,
  // FMX.PixelFormats,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Dialogs;

// ------------------------------------------------------------------------------

type

  TDPFCamera = class;

  TSampleBufferReadyEvent = procedure( Sender: TObject; const ATime: Int64 ) of object;

  TCameraPosition      = ( cpUnspecified, cpBack, cpFront );
  TVideoCaptureQuality = ( vcqPhotoQuality, vcqHighQuality, vcqMediumQuality, vcqLowQuality, vcq320x240, vcq352x288Quality, vcq640x480Quality, vcq1280x720Quality, vcq1920x1080Quality, vcqiFrame960x540Quality, vcqiFrame1280x720Quality );
  TMP4ConvertQuality   = ( mcqLow, mcqMedium, mcqHighest );

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFExportSessionTimerDelegate = interface( IObjectiveC )
    ['{8A9E6410-B9D6-4368-B43E-0DA245E11C24}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  TDPFEexportSessionTimerDelegate = class( TOCLocal, IDPFExportSessionTimerDelegate )
  private
    FDPFCamera: TDPFCamera;
  public
    constructor Create( ADPFCamera: TDPFCamera );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ---------------------------------------------------------------------------
  TPermissionBlock = procedure( granted: pointer ) of object;

  AVAudioSessionClass = interface( NSObjectClass )
    ['{371FE451-90CD-4C57-BBC0-B53BE5ADF5A7}']
    function sharedInstance: Pointer; cdecl;
  end;

  AVAudioSession = interface( NSObject )
    ['{A0CBDDEF-B457-4A24-8EC2-4F2B1F610BF7}']
    function category: NSString; cdecl;
    function currentHardwareInputNumberOfChannels: NSInteger; cdecl;
    function currentHardwareOutputNumberOfChannels: NSInteger; cdecl;
    function currentHardwareSampleRate: double; cdecl;
    function delegate: Pointer; cdecl;
    function inputIsAvailable: Boolean; cdecl;
    function mode: NSString; cdecl;
    function preferredHardwareSampleRate: double; cdecl;
    function preferredIOBufferDuration: NSTimeInterval; cdecl;
    function setActive( beActive: Boolean; error: NSError ): Boolean; cdecl; overload;
    function setActive( beActive: Boolean; withFlags: NSInteger; error: NSError ): Boolean; cdecl; overload;
    function setCategory( theCategory: NSString; error: NSError ): Boolean; cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    function setMode( theMode: NSString; error: NSError ): Boolean; cdecl;
    function setPreferredHardwareSampleRate( sampleRate: double; error: NSError ): Boolean; cdecl;
    function setPreferredIOBufferDuration( duration: NSTimeInterval; error: NSError ): Boolean; cdecl;

    procedure requestRecordPermission( response: TPermissionBlock ); cdecl;
  end;

  TAVAudioSession = class( TOCGenericImport<AVAudioSessionClass, AVAudioSession> )
  end;

  TExportCompletionHandler = procedure of object;

  // ------------------------------------------------------------------------------
  ICaptureSessionStoppedRunningNotification = interface( IObjectiveC )
    ['{3361E266-FAE4-4BE8-B8EB-6A45C4C7843D}']
    procedure CaptureSessionStoppedRunningNotification( notification: NSNotification ); cdecl;
  end;

  TCaptureSessionStoppedRunningNotification = class( TOCLocal, ICaptureSessionStoppedRunningNotification )
  private
    FDPFCamera: TDPFCamera;
  public
    constructor create( ADPFCamera: TDPFCamera );

    procedure CaptureSessionStoppedRunningNotification( notification: NSNotification ); cdecl;
  end;

  // ------------------------------------------------------------------------------

  (*
    PCMTime = ^CMTime;

    AVAssetImageGeneratorClass = interface( NSObjectClass )
    ['{5C7794F2-361F-4E6C-8C9F-E00CF0246A15}']
    { class } function assetImageGeneratorWithAsset( asset: AVAsset ): Pointer; cdecl;
    end;

    AVAssetImageGenerator = interface( NSObject )
    ['{0B8393D2-55CC-49CE-AB2C-C6B8938CC180}']
    function apertureMode: NSString; cdecl;
    function appliesPreferredTrackTransform: Boolean; cdecl;
    procedure cancelAllCGImageGeneration; cdecl;
    function copyCGImageAtTime( requestedTime: CMTime; actualTime: PCMTime; error: PNSError ): CGImageRef; cdecl;
    function initWithAsset( asset: AVAsset ): Pointer; cdecl;
    function maximumSize: CGSize; cdecl;
    function requestedTimeToleranceAfter: CMTime; cdecl;
    function requestedTimeToleranceBefore: CMTime; cdecl;
    procedure setApertureMode( apertureMode: NSString ); cdecl;
    procedure setAppliesPreferredTrackTransform( appliesPreferredTrackTransform: Boolean ); cdecl;
    procedure setMaximumSize( maximumSize: CGSize ); cdecl;
    procedure setRequestedTimeToleranceAfter( requestedTimeToleranceAfter: CMTime ); cdecl;
    procedure setRequestedTimeToleranceBefore( requestedTimeToleranceBefore: CMTime ); cdecl;
    procedure setVideoComposition( videoComposition: AVVideoComposition ); cdecl;
    function videoComposition: AVVideoComposition; cdecl;
    end;

    TAVAssetImageGenerator = class( TOCGenericImport<AVAssetImageGeneratorClass, AVAssetImageGenerator> )
    end;
  *)

  // ------------------------------------------------------------------------------
  TFinishWritingWithCompletionHandler = procedure of object;

  AVAssetWriterClass = interface( NSObjectClass )
    ['{6D40766B-E7D4-4AB1-84C0-40854E31DBB1}']
    { class } function assetWriterWithURL( outputURL: NSURL; fileType: NSString; error: NSError ): Pointer; cdecl;
  end;

  AVAssetWriter = interface( NSObject )
    ['{3267019A-FB54-4919-B507-759FF0E92BEF}']
    procedure addInput( input: AVAssetWriterInput ); cdecl;
    function availableMediaTypes: NSArray; cdecl;
    function canAddInput( input: AVAssetWriterInput ): Boolean; cdecl;
    function canApplyOutputSettings( outputSettings: NSDictionary; forMediaType: NSString ): Boolean; cdecl;
    procedure cancelWriting; cdecl;
    procedure endSessionAtSourceTime( endTime: CMTime ); cdecl;
    function error: NSError; cdecl;
    // function finishWriting: Boolean; cdecl;
    procedure finishWritingWithCompletionHandler( handler: TFinishWritingWithCompletionHandler ); cdecl;
    function initWithURL( outputURL: NSURL; fileType: NSString; error: NSError ): Pointer; cdecl;
    function inputs: NSArray; cdecl;
    function metadata: NSArray; cdecl;
    function movieFragmentInterval: CMTime; cdecl;
    function movieTimeScale: CMTimeScale; cdecl;
    function outputFileType: NSString; cdecl;
    function outputURL: NSURL; cdecl;
    procedure setMetadata( metadata: NSArray ); cdecl;
    procedure setMovieFragmentInterval( movieFragmentInterval: CMTime ); cdecl;
    procedure setMovieTimeScale( movieTimeScale: CMTimeScale ); cdecl;
    procedure setShouldOptimizeForNetworkUse( shouldOptimizeForNetworkUse: Boolean ); cdecl;
    function shouldOptimizeForNetworkUse: Boolean; cdecl;
    procedure startSessionAtSourceTime( startTime: CMTime ); cdecl;
    function startWriting: Boolean; cdecl;
    function status: AVAssetWriterStatus; cdecl;
  end;

  TAVAssetWriter = class( TOCGenericImport<AVAssetWriterClass, AVAssetWriter> )
  end;

  // ------------------------------------------------------------------------------
  AVAssetExportSessionClass = interface( NSObjectClass )
    ['{27908DE7-98E1-4BD2-8443-79DD5D9682C2}']
    function allExportPresets: NSArray; cdecl;
    function exportPresetsCompatibleWithAsset( asset: AVAsset ): NSArray; cdecl;
    function exportSessionWithAsset( asset: AVAsset; presetName: NSString ): Pointer; cdecl;
  end;

  AVAssetExportSession = interface( NSObject )
    ['{3A571B34-10BE-49CE-87DB-1ED36C08B159}']
    function asset: AVAsset; cdecl;
    function audioMix: AVAudioMix; cdecl;
    procedure cancelExport; cdecl;
    function error: NSError; cdecl;
    function estimatedOutputFileLength: Int64; cdecl;
    function fileLengthLimit: Int64; cdecl;
    function initWithAsset( asset: AVAsset; presetName: NSString ): Pointer; cdecl;
    function maxDuration: CMTime; cdecl;
    function metadata: NSArray; cdecl;
    function outputFileType: NSString; cdecl;
    function outputURL: NSURL; cdecl;
    function presetName: NSString; cdecl;
    function progress: Single; cdecl;
    procedure setAudioMix( audioMix: AVAudioMix ); cdecl;
    procedure setFileLengthLimit( fileLengthLimit: Int64 ); cdecl;
    procedure setMetadata( metadata: NSArray ); cdecl;
    procedure setOutputFileType( outputFileType: NSString ); cdecl;
    procedure setOutputURL( outputURL: NSURL ); cdecl;
    procedure setShouldOptimizeForNetworkUse( shouldOptimizeForNetworkUse: Boolean ); cdecl;
    procedure setTimeRange( timeRange: CMTimeRange ); cdecl;
    procedure setVideoComposition( videoComposition: AVVideoComposition ); cdecl;
    function shouldOptimizeForNetworkUse: Boolean; cdecl;
    function status: AVAssetExportSessionStatus; cdecl;
    function supportedFileTypes: NSArray; cdecl;
    function timeRange: CMTimeRange; cdecl;
    function videoComposition: AVVideoComposition; cdecl;
    procedure exportAsynchronouslyWithCompletionHandler( exportCompletionHandler: TExportCompletionHandler ); cdecl;
  end;

  TAVAssetExportSession = class( TOCGenericImport<AVAssetExportSessionClass, AVAssetExportSession> )
  end;

  // ------------------------------------------------------------------------------
  TVideoAudioSampleDelegate = class( TOCLocal, AVCaptureVideoDataOutputSampleBufferDelegate )
  private
  public
    FDPFCamera: TDPFCamera;
    procedure captureOutput( captureOutput: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection ); cdecl;
  end;

{$ENDIF}

  // ----------------------------------------------------------------------------
  // TDPFCamera Component
  // ----------------------------------------------------------------------------

  TRecordingState  = ( rsNone, rsReady, rsStart, rsPause, rsStop, rsCancel );
  TCameraTorchMode = ( tmOff, tmOn, tmAuto );
  TFocusMode       = ( NoneFocus = -1, FocusLocked = 0 { AVCaptureFocusModeLocked } , AutoFocus = 1 { AVCaptureFocusModeAutoFocus } , ContinuousAutoFocus = 2 { AVCaptureFocusModeContinuousAutoFocus } );
  TExposureMode    = ( NoneExposure = -1, ExposureLocked = 0 { AVCaptureExposureModeLocked } , AutoExpose = 1 { AVCaptureExposureModeAutoExpose } , ContinuousAutoExposure = 2 { AVCaptureExposureModeContinuousAutoExposure } );

  TOnConvertComplete      = procedure( Sender: TObject; Status: Integer ) of object;
  TOnConvertProgressTimer = procedure( Sender: TObject; Progress: Single ) of object;
  TOnCaptureFinished      = procedure( Sender: TObject; var SaveToAlbumLibrary: Boolean ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFCamera = class( TComponent )
  private
    FOnSampleBufferReady     : TSampleBufferReadyEvent;
    FPreview                 : TDPFUIView;
    FOnConvertComplete       : TOnConvertComplete;
    FVideoCaptureQuality     : TVideoCaptureQuality;
    FCameraPosition          : TCameraPosition;
    FConvertToMP4            : Boolean;
    FOnConvertProgress       : TOnConvertProgressTimer;
    FConvertToMP4Quality     : TMP4ConvertQuality;
    FMaxRecordDuration       : Int64;
    FMaxRecordFrame          : Int64;
    FOnCaptureFinished       : TOnCaptureFinished;
    FTorchMode               : TCameraTorchMode;
    FAudioRecord             : Boolean;
    FOutputFileName          : string;
    FFocusMode               : TFocusMode;
    FFocusPointOfInterestX   : double;
    FFocusPointOfInterestY   : double;
    FExposurePointOfInterestX: double;
    FExposureMode            : TExposureMode;
    FExposurePointOfInterestY: double;

{$IFDEF IOS}
    FOutputMovFileName: string;
    FCurrentDevice    : UIDevice;

    exportSessionTimer             : NSTimer;
    FDPFEexportSessionTimerDelegate: TDPFEexportSessionTimerDelegate;

    _timeOffset: CMTime;
    _lastVideo : CMTime;
    _lastAudio : CMTime;
    _discont   : Boolean;

    FStartPresentationTimeStamp: CMTime;
    FRecordDuration            : Int64;

    FCaptureSessionStoppedRunningNotification: TCaptureSessionStoppedRunningNotification;
    FAVCaptureSession                        : AVCaptureSession;
    FAVCaptureDeviceVideoInput               : AVCaptureDeviceInput;
    FAVCaptureDeviceAudioInput               : AVCaptureDeviceInput;

    FVideoAudioSampleDelegate: TVideoAudioSampleDelegate;

    // FImageBuffer : CVImageBufferRef;
    FSampleBuffer: CMSampleBufferRef;

    VideoWriterInput: AVAssetWriterInput;
    AudioWriterInput: AVAssetWriterInput;

    videoOrientation: AVCaptureVideoOrientation;

    AssetWriter: AVAssetWriter;

    currentFrame: Int64;

    myLock : DPF.iOS.Classes.NSLock;
    myLock2: DPF.iOS.Classes.NSLock;

    movieWritingQueue: dispatch_queue_t;

    FAudioRecordPermission     : Boolean;
    FAVCaptureVideoPreviewLayer: AVCaptureVideoPreviewLayer;
    exportSession              : AVAssetExportSession;

    FRecordingState: TRecordingState;
    procedure SyncCallSampleBufferReady;
    procedure exportCompletionHandler;
    function SetupAssetWriters: Boolean;
    procedure DoConvertToMP4;
    procedure PermissionBlock( granted: pointer );
{$ENDIF}
    procedure SetTorchMode( const Value: TCameraTorchMode );
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFDEF IOS}
    procedure FinishWritingCompletionHandler;

    function DoSampleBufferToUIImage: UIImage;
    procedure DoSampleBufferToBitmap( ABitmap: TBitmap );
    procedure SwitchCameras;
    procedure SetupDevice;
    procedure ReleaseDevice;
    procedure StopRecording;
    procedure CancelRecording;
    procedure StartRecording;
    procedure PauseRecording;
    property RecordingState: TRecordingState read FRecordingState;
    property OutputMovFileName: string read FOutputMovFileName;
{$ENDIF}
  published
    property MaxRecordFrame     : Int64 read FMaxRecordFrame write FMaxRecordFrame default 0;
    property MaxRecordDuration  : Int64 read FMaxRecordDuration write FMaxRecordDuration default 0;
    property Preview            : TDPFUIView read FPreview write FPreview;
    property AudioRecord        : Boolean read FAudioRecord write FAudioRecord default true;
    property ConvertToMP4       : Boolean read FConvertToMP4 write FConvertToMP4 default true;
    property ConvertToMP4Quality: TMP4ConvertQuality read FConvertToMP4Quality write FConvertToMP4Quality default TMP4ConvertQuality.mcqHighest;
    property OutputFileName     : string read FOutputFileName write FOutputFileName;
    property VideoCaptureQuality: TVideoCaptureQuality read FVideoCaptureQuality write FVideoCaptureQuality default TVideoCaptureQuality.vcqMediumQuality;
    property CameraPosition     : TCameraPosition read FCameraPosition write FCameraPosition default TCameraPosition.cpBack;
    property TorchMode          : TCameraTorchMode read FTorchMode write SetTorchMode default TCameraTorchMode.tmAuto;

    property FocusMode            : TFocusMode read FFocusMode write FFocusMode default TFocusMode.NoneFocus;
    property FocusPointOfInterestX: double read FFocusPointOfInterestX write FFocusPointOfInterestX;
    property FocusPointOfInterestY: double read FFocusPointOfInterestY write FFocusPointOfInterestY;

    property ExposureMode            : TExposureMode read FExposureMode write FExposureMode default TExposureMode.NoneExposure;
    property ExposurePointOfInterestX: double read FExposurePointOfInterestX write FExposurePointOfInterestX;
    property ExposurePointOfInterestY: double read FExposurePointOfInterestY write FExposurePointOfInterestY;

    property OnSampleBufferReady: TSampleBufferReadyEvent read FOnSampleBufferReady write FOnSampleBufferReady;
    property OnConvertComplete  : TOnConvertComplete read FOnConvertComplete write FOnConvertComplete;
    property OnConvertProgress  : TOnConvertProgressTimer read FOnConvertProgress write FOnConvertProgress;
    property OnCaptureFinished  : TOnCaptureFinished read FOnCaptureFinished write FOnCaptureFinished;
  end;

  // ----------------------------------------------------------------------------

{$IFDEF IOS}

function ThumbnailImageForVideo( videoURL: string; atTime: CMTimeValue ): UIImage;
{$ENDIF}

implementation

{$IFDEF IOS}

var
  main_queue: dispatch_queue_t;

procedure UISaveVideoAtPathToSavedPhotosAlbum( videoPath: PNSString; completionTarget: Pointer; completionSelector: SEL; contextInfo: Pointer ); cdecl; external libUIKit name _PU + 'UISaveVideoAtPathToSavedPhotosAlbum';
function UIVideoAtPathIsCompatibleWithSavedPhotosAlbum( videoPath: PNSString ): Boolean; cdecl; external libUIKit name _PU + 'UIVideoAtPathIsCompatibleWithSavedPhotosAlbum';

// ------------------------------------------------------------------------------
function DegreesToRadians( degrees: CGFloat ): CGFloat;
begin
  result := ( degrees * PI ) / 180;
end;

// ------------------------------------------------------------------------------
function CocoaCMTimeConst( Fwk: string; ConstStr: string ): CMTime;
var
  Obj: Pointer;
begin
  FillChar( result, SizeOf( result ), 0 );
  Obj := Pointer( CocoaPointerConst( Fwk, ConstStr )^ );
  if Obj <> nil then
    Result := CMTime( Obj^ );
end;

// ------------------------------------------------------------------------------
function ThumbnailImageForVideo( videoURL: string; atTime: CMTimeValue ): UIImage;
var
  asset         : AVAsset;
  imageGenerator: AVAssetImageGenerator;
  UR            : NSURL;
  thumb         : CGImageRef;
  thumbnailTime : CMTime;
begin
  if videoURL.Contains( 'file://' ) then
    UR := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( videoURL ) ) )
  else
    UR := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( videoURL ) ) );

  asset := TAVAsset.Wrap( TAVAsset.OCClass.assetWithURL( UR ) );

  thumbnailTime       := asset.duration;
  thumbnailTime.value := atTime;
  imageGenerator      := TAVAssetImageGenerator.Wrap( TAVAssetImageGenerator.Alloc.initWithAsset( asset ) );
  imageGenerator.setAppliesPreferredTrackTransform( true );
  if Assigned( imageGenerator ) then
  begin
    thumb := imageGenerator.copyCGImageAtTime( thumbnailTime, thumbnailTime, nil );
    if Assigned( thumb ) then
    begin
      Result := TUIImage.Wrap( TUIImage.OCClass.imageWithCGImage( thumb ) );
      CGImageRelease( thumb );
    end;
    imageGenerator.release;
  end;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFCamera }
constructor TDPFCamera.Create( AOwner: TComponent );
begin
  inherited;
  FTorchMode           := TCameraTorchMode.tmAuto;
  FVideoCaptureQuality := TVideoCaptureQuality.vcqMediumQuality;
  FCameraPosition      := TCameraPosition.cpBack;
  FConvertToMP4        := true;
  FConvertToMP4Quality := TMP4ConvertQuality.mcqHighest;
  FAudioRecord         := True;

  FFocusMode             := TFocusMode.NoneFocus;
  FFocusPointOfInterestX := 0.5;
  FFocusPointOfInterestY := 0.5;

  FExposureMode             := TExposureMode.NoneExposure;
  FExposurePointOfInterestX := 0.5;
  FExposurePointOfInterestY := 0.5;

{$IFDEF IOS}
  main_queue         := dispatch_get_main_queue;
  exportSessionTimer := nil;
  _discont           := false;
  FRecordingState    := rsNone;
  FCurrentDevice     := TUIDevice.Wrap( TUIDevice.OCClass.currentDevice );
  myLock             := TNSLock.Create;
  myLock2            := TNSLock.Create;
  FMaxRecordFrame    := 0;
  FMaxRecordDuration := 0;

  FDPFEexportSessionTimerDelegate := TDPFEexportSessionTimerDelegate.Create( self );

  FCaptureSessionStoppedRunningNotification := TCaptureSessionStoppedRunningNotification.create( self );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FCaptureSessionStoppedRunningNotification.GetObjectID, sel_getUid( 'CaptureSessionStoppedRunningNotification:' ), ( NSSTR( 'AVCaptureSessionDidStopRunningNotification' ) as ILocalObject ).GetObjectID, nil );

  // FAudioRecordPermission := TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).respondsToSelector( sel_getUid( 'requestRecordPermission:' ) );
  // -----------------------------------------------
  // Check Microphone Permisseion in iOS7 and later
  if { FAudioRecordPermission and } ( TOSVersion.Major > 6 ) then
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).requestRecordPermission( PermissionBlock )
  else
    FAudioRecordPermission := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFCamera.Destroy;
begin
{$IFDEF IOS}
  RemoveFile( FOutputMovFileName );
  ReleaseDevice;

  myLock.release;
  myLock2.release;
  if assigned( FVideoAudioSampleDelegate ) then
    FVideoAudioSampleDelegate.DisposeOf;

  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FCaptureSessionStoppedRunningNotification );
  FCaptureSessionStoppedRunningNotification.DisposeOf;

  if Assigned( FDPFEexportSessionTimerDelegate ) then
    FDPFEexportSessionTimerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.SetTorchMode( const Value: TCameraTorchMode );
{$IFDEF IOS}
var
  device: AVCaptureDevice;
{$ENDIF}
begin
  FTorchMode := Value;
{$IFDEF IOS}
  device := TAVCaptureDevice.Wrap( TAVCaptureDevice.OCClass.defaultDeviceWithMediaType( AVMediaTypeVideo ) );
  if assigned( device ) and device.hasTorch and device.hasFlash then
  begin
    device.lockForConfiguration( nil );
    device.setTorchMode( Integer( Value ) );
    device.setFlashMode( Integer( Value ) );
    device.unlockForConfiguration;
  end;
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFCamera.PermissionBlock( granted: pointer );
begin
  FAudioRecordPermission := Boolean( granted );
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.SyncCallSampleBufferReady;
begin
  if Assigned( FOnSampleBufferReady ) then
    FOnSampleBufferReady( Self, FRecordDuration );
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.PauseRecording;
begin
  if Assigned( FAVCaptureSession ) and FAVCaptureSession.isRunning then
  begin
    FRecordingState := rsPause;
    _discont        := true;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.StartRecording;
begin
  if Assigned( FAVCaptureSession ) and FAVCaptureSession.isRunning then
  begin
    _timeOffset     := CMTimeMake( 0, 0 );
    FRecordingState := rsStart;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.CancelRecording;
begin
  if FRecordingState = rsStop then
    exit;

  if not myLock.tryLock then
    exit;
  try
    FRecordingState := rsCancel;
    currentFrame    := 0;
    FAVCaptureSession.stopRunning;
  finally
    myLock.unlock;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.StopRecording;
begin
  // if not( FRecordingState in [rsStart, rsPause] ) then exit;

  if not assigned( FAVCaptureSession ) or not FAVCaptureSession.isRunning then
    exit;

  if not myLock.tryLock then
    exit;
  try
    currentFrame := 0;
    FAVCaptureSession.stopRunning;
  finally
    myLock.unlock;
  end;
end;

// ------------------------------------------------------------------------------
function TDPFCamera.SetupAssetWriters: Boolean;
var
  error                      : NSError;
  videoCompressionProps      : NSDictionary;
  videoSettings              : NSMutableDictionary;
  rotationDegrees            : CGFloat;
  rotationRadians            : CGFloat;
  outputMovURL               : NSURL;
  acl                        : AudioChannelLayout;
  audioOutputSettings        : NSMutableDictionary;
  preferredHardwareSampleRate: Double;
  CWidth, CHeight            : Integer;
begin
  try
    CWidth  := 640;
    CHeight := 480;
    case FVideoCaptureQuality of
      vcqMediumQuality:
        begin
          CWidth  := 352;
          CHeight := 288;
        end;
      vcqLowQuality:
        begin
          CWidth  := 320;
          CHeight := 240;
        end;
      vcq320x240:
        begin
          CWidth  := 320;
          CHeight := 240;
        end;
      vcq352x288Quality:
        begin
          CWidth  := 352;
          CHeight := 288;
        end;
      vcq640x480Quality:
        begin
          CWidth  := 640;
          CHeight := 480;
        end;
      vcqiFrame960x540Quality:
        begin
          CWidth  := 960;
          CHeight := 540;
        end;
      vcqiFrame1280x720Quality:
        begin
          CWidth  := 1280;
          CHeight := 720;
        end;
      vcq1280x720Quality:
        begin
          CWidth  := 1280;
          CHeight := 720;
        end;
      vcq1920x1080Quality:
        begin
          CWidth  := 1920;
          CHeight := 1080;
        end;
    end;

    // ---------------------------------------------------------
    // Setup Asset Writer
    // ---------------------------------------------------------
    outputMovURL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FOutputMovFileName ) ) );
    AssetWriter  := TAVAssetWriter.Wrap( TAVAssetWriter.alloc.initWithURL( outputMovURL, AVFileTypeQuickTimeMovie, error ) );
    // Add video input
    videoCompressionProps := TNSDictionary.Wrap( TNSDictionary.OCClass.dictionaryWithObject( TNSNumber.OCClass.numberWithDouble( 512.0 * 1024.0 ), ( AVVideoAverageBitRateKey as ILocalObject ).GetObjectID ) );

    videoSettings := TNSMutableDictionary.Create;
    videoSettings.setObject( ( AVVideoCodecH264 as ILocalObject ).GetObjectID, ( AVVideoCodecKey as ILocalObject ).GetObjectID );
    videoSettings.setObject( TNSNumber.OCClass.numberWithInt( CWidth ), ( AVVideoWidthKey as ILocalObject ).GetObjectID );
    videoSettings.setObject( TNSNumber.OCClass.numberWithInt( CHeight ), ( AVVideoHeightKey as ILocalObject ).GetObjectID );
    // videoSettings.setObject( TNSNumber.OCClass.numberWithInt( 30 ), ( AVVideoMaxKeyFrameIntervalKey as ILocalObject ).GetObjectID );
    videoSettings.setObject( ( videoCompressionProps as ILocalObject ).GetObjectID, ( AVVideoCompressionPropertiesKey as ILocalObject ).GetObjectID );

    VideoWriterInput := TAVAssetWriterInput.Wrap( TAVAssetWriterInput.Alloc.initWithMediaType( AVMediaTypeVideo, videoSettings ) );
    VideoWriterInput.setExpectsMediaDataInRealTime( true );
    videoSettings.release;

    rotationDegrees := 0.0;
    // specify the prefered transform for the output file
    case TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).orientation of
      UIDeviceOrientationPortraitUpsideDown:
        rotationDegrees := -90.0;
      UIDeviceOrientationLandscapeLeft: // no rotation
        rotationDegrees := 0.0;
      UIDeviceOrientationLandscapeRight:
        rotationDegrees := 180.0;
      UIDeviceOrientationPortrait:
        rotationDegrees := 90.0;
      UIDeviceOrientationUnknown:
        rotationDegrees := 90.0;
      UIDeviceOrientationFaceUp:
        rotationDegrees := 90.0;
      UIDeviceOrientationFaceDown:
        rotationDegrees := 90.0;
    end;
    rotationRadians := DegreesToRadians( rotationDegrees );
    VideoWriterInput.setTransform( CGAffineTransformMakeRotation( rotationRadians ) );

    if FAudioRecordPermission and FAudioRecord then
    begin
      // Add the audio input
      FillChar( acl, SizeOf( acl ), 0 );

      acl.mChannelLayoutTag := kAudioChannelLayoutTag_Mono;

      preferredHardwareSampleRate := TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).currentHardwareSampleRate;

      audioOutputSettings := TNSMutableDictionary.Create;
      // Both type of audio inputs causes output video file to be corrupted.
      if true then
      begin
        // should work from iphone 3GS on and from ipod 3rd generation
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( kAudioFormatMPEG4AAC ), ( AVFormatIDKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithFloat( preferredHardwareSampleRate ), ( AVSampleRateKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( 64000 ), ( AVEncoderBitRateKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( 1 ), ( AVNumberOfChannelsKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSData.OCClass.dataWithBytes( @acl, sizeOf( AudioChannelLayout ) ), ( AVChannelLayoutKey as ILocalObject ).GetObjectID );
      end
      else
      begin
        // should work on any device requires more space
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( kAudioFormatAppleLossless ), ( AVFormatIDKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( 16 ), ( AVEncoderBitDepthHintKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithFloat( 44100.0 ), ( AVSampleRateKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSNumber.OCClass.numberWithInt( 1 ), ( AVNumberOfChannelsKey as ILocalObject ).GetObjectID );
        audioOutputSettings.setObject( TNSData.OCClass.dataWithBytes( @acl, sizeOf( acl ) ), ( AVChannelLayoutKey as ILocalObject ).GetObjectID );
      end;

      AudioWriterInput := TAVAssetWriterInput.Wrap( TAVAssetWriterInput.Alloc.initWithMediaType( AVMediaTypeAudio, audioOutputSettings ) );
      AudioWriterInput.setExpectsMediaDataInRealTime( True );
      audioOutputSettings.release;
    end;

    // add input
    AssetWriter.addInput( VideoWriterInput );
    if FAudioRecordPermission and FAudioRecord then
      AssetWriter.addInput( AudioWriterInput );

    // AssetWriter.startWriting;
    // AssetWriter.startSessionAtSourceTime( nextPTS );
    Result := true;
  except
    Result := false;
  end;

end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.ReleaseDevice;
var
  Coutput: AVCaptureOutput;
  Cinput : AVCaptureDeviceInput;
begin
  if assigned( FAVCaptureSession ) and Assigned( FAVCaptureSession.inputs ) then
    while FAVCaptureSession.inputs.count > 0 do
    begin
      Cinput := TAVCaptureDeviceInput.Wrap( FAVCaptureSession.inputs.objectAtIndex( 0 ) );
      FAVCaptureSession.removeInput( Cinput );
    end;

  if assigned( FAVCaptureSession ) and Assigned( FAVCaptureSession.outputs ) then
    while FAVCaptureSession.outputs.count > 0 do
    begin
      Coutput := TAVCaptureOutput.Wrap( FAVCaptureSession.outputs.objectAtIndex( 0 ) );
      FAVCaptureSession.removeOutput( Coutput );
    end;

  if Assigned( VideoWriterInput ) then
  begin
    VideoWriterInput.release;
    VideoWriterInput := nil;
  end;

  if FAudioRecordPermission and FAudioRecord and Assigned( AudioWriterInput ) then
  begin
    AudioWriterInput.release;
    AudioWriterInput := nil;
  end;

  if Assigned( AssetWriter ) then
  begin
    AssetWriter.release;
    AssetWriter := nil;
  end;

  if Assigned( FAVCaptureSession ) then
  begin
    FAVCaptureSession.release;
    FAVCaptureSession := nil;
  end;

  if movieWritingQueue > 0 then
  begin
    dispatch_release( movieWritingQueue );
    movieWritingQueue := 0;
  end;

  FRecordingState := rsNone;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.switchCameras;
var
  videoDevices      : NSArray;
  captureVideoDevice: AVCaptureDevice;
{$IFDEF DELPHIXE6}
  Error: Pointer;
{$ELSE}
  Error: NSError;
{$ENDIF}
  i            : Integer;
  NewVideoInput: AVCaptureDeviceInput;
begin

  if not Assigned( FAVCaptureSession ) then
    exit;

  if CameraPosition = cpFront then
    CameraPosition := cpBack
  else
    CameraPosition := cpFront;

  captureVideoDevice := nil;
  videoDevices       := TAVCaptureDevice.OCClass.devicesWithMediaType( AVMediaTypeVideo );
  for i              := 0 to videoDevices.count - 1 do
  begin
    captureVideoDevice := TAVCaptureDevice.Wrap( videoDevices.objectAtIndex( i ) );
    if captureVideoDevice.position = NSInteger( FCameraPosition ) then
    begin
{$IFDEF DELPHIXE6}
      Error := nil;
{$ENDIF}
      NewVideoInput := TAVCaptureDeviceInput.Wrap( TAVCaptureDeviceInput.OCClass.deviceInputWithDevice( captureVideoDevice, {$IFDEF DELPHIXE6}@{$ENDIF}Error ) );

      FAVCaptureSession.beginConfiguration;
      FAVCaptureSession.removeInput( FAVCaptureDeviceVideoInput );
      if FAVCaptureSession.canAddInput( NewVideoInput ) then
      begin
        FAVCaptureSession.addInput( NewVideoInput );
        FAVCaptureDeviceVideoInput := NewVideoInput;
      end
      else
        FAVCaptureSession.addInput( FAVCaptureDeviceVideoInput );
      FAVCaptureSession.commitConfiguration;
      Exit;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.SetupDevice;
var
  videoDevices      : NSArray;
  captureVideoDevice: AVCaptureDevice;
  captureAudioDevice: AVCaptureDevice;
  I                 : Integer;
{$IFDEF DELPHIXE6}
  Error: Pointer;
{$ELSE}
  Error: NSError;
{$ENDIF}
  Err       : NSError;
  VideoQueue: dispatch_queue_t;
  AudioQueue: dispatch_queue_t;
  VS        : NSDictionary;
  // outputMovURL             : NSURL;
  videoConnection          : AVCaptureConnection;
  FAVCaptureVideoDataOutput: AVCaptureVideoDataOutput;
  FAVCaptureAudioDataOutput: AVCaptureAudioDataOutput;
begin

  if ( FRecordingState = rsStart ) or assigned( FAVCaptureSession ) or ( assigned( FAVCaptureSession ) and FAVCaptureSession.isRunning ) then
    exit;

  _discont := false;
  if not myLock.tryLock then
    exit;
  try
    if FConvertToMP4 then
      FOutputMovFileName := GetTempDirectory + GetUUID + '.mov'
    else
      FOutputMovFileName := FOutputFileName;
    RemoveFile( FOutputMovFileName );
    // outputMovURL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FOutputMovFileName ) ) );

    currentFrame := 0;

    if not Assigned( FAVCaptureSession ) then
      FAVCaptureSession := TAVCaptureSession.Create;

    case FVideoCaptureQuality of
      vcqPhotoQuality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetPhoto );
      vcqHighQuality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetHigh );
      vcqMediumQuality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetMedium );
      vcqLowQuality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetLow );
      vcq320x240:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPreset320x240 );
      vcq352x288Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPreset352x288 );
      vcq640x480Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPreset640x480 );
      vcq1280x720Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPreset1280x720 );
      vcq1920x1080Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPreset1920x1080 );
      vcqiFrame960x540Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetiFrame960x540 );
      vcqiFrame1280x720Quality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetiFrame1280x720 );
      { vcqInputPriorityQuality:
        FAVCaptureSession.setSessionPreset( AVCaptureSessionPresetInputPriority ); }
    end;

    // ----------------------------------------------------
    // Find Video Device
    captureVideoDevice := nil;
    videoDevices       := TAVCaptureDevice.OCClass.devicesWithMediaType( AVMediaTypeVideo );
    for i              := 0 to videoDevices.count - 1 do
    begin
      if TAVCaptureDevice.Wrap( videoDevices.objectAtIndex( i ) ).position = NSInteger( FCameraPosition ) then
      begin
        captureVideoDevice := TAVCaptureDevice.Wrap( videoDevices.objectAtIndex( i ) );
        break;
      end;

      if captureVideoDevice = nil then
        captureVideoDevice := TAVCaptureDevice.Wrap( TAVCaptureDevice.OCClass.defaultDeviceWithMediaType( AVMediaTypeVideo ) );
    end;

    if not Assigned( captureVideoDevice ) then
      exit;

    // ----------------------------------------------------
    // Setup Capture Focus Mode
    // ----------------------------------------------------

    if ( FocusMode <> TFocusMode.NoneFocus ) and captureVideoDevice.isFocusModeSupported( Integer( FocusMode ) ) and captureVideoDevice.lockForConfiguration( Err ) then
    begin
      if captureVideoDevice.isFocusPointOfInterestSupported then
        captureVideoDevice.setFocusPointOfInterest( CGPointMake( FocusPointOfInterestX, FocusPointOfInterestY ) );
      captureVideoDevice.setFocusMode( Integer( FocusMode ) );
      captureVideoDevice.unlockForConfiguration;
    end;

    // ----------------------------------------------------
    // Setup Capture Exposure Mode
    // ----------------------------------------------------

    if ( ExposureMode <> TExposureMode.NoneExposure ) and captureVideoDevice.isExposureModeSupported( Integer( ExposureMode ) ) and captureVideoDevice.lockForConfiguration( Err ) then
    begin
      if captureVideoDevice.isFocusPointOfInterestSupported then
        captureVideoDevice.setExposurePointOfInterest( CGPointMake( ExposurePointOfInterestX, ExposurePointOfInterestY ) );

      captureVideoDevice.setExposureMode( Integer( FocusMode ) );
      captureVideoDevice.unlockForConfiguration;
    end;

    // ----------------------------------------------------
    // Setup Capture Video Device
    // ----------------------------------------------------
{$IFDEF DELPHIXE6}
    Error := nil;
{$ENDIF}
    FAVCaptureDeviceVideoInput := TAVCaptureDeviceInput.Wrap( TAVCaptureDeviceInput.OCClass.deviceInputWithDevice( captureVideoDevice, {$IFDEF DELPHIXE6}@{$ENDIF}Error ) );
    FAVCaptureSession.addInput( FAVCaptureDeviceVideoInput );

    FAVCaptureVideoDataOutput := TAVCaptureVideoDataOutput.Create;

    if not Assigned( FVideoAudioSampleDelegate ) then
    begin
      FVideoAudioSampleDelegate            := TVideoAudioSampleDelegate.Create;
      FVideoAudioSampleDelegate.FDPFCamera := Self;
    end;

    VideoQueue := dispatch_queue_create( 'Video Capture Queue', 0 );
    FAVCaptureVideoDataOutput.setSampleBufferDelegate( ILocalObject( FVideoAudioSampleDelegate ).GetObjectID, VideoQueue );
    dispatch_release( VideoQueue );

    VS := TNSDictionary.Wrap( TNSDictionary.OCClass.dictionaryWithObject( TNSNumber.OCClass.numberWithInt( kCVPixelFormatType_32BGRA ), Pointer( kCVPixelBufferPixelFormatTypeKey ) ) );
    FAVCaptureVideoDataOutput.setVideoSettings( VS );
    FAVCaptureVideoDataOutput.setAlwaysDiscardsLateVideoFrames( false );
    FAVCaptureSession.addOutput( FAVCaptureVideoDataOutput );
    videoConnection  := FAVCaptureVideoDataOutput.connectionWithMediaType( AVMediaTypeVideo );
    videoOrientation := videoConnection.videoOrientation;
    FAVCaptureVideoDataOutput.release;

    // ---------------------------------------------------------
    // Setup Preview Layer
    // ---------------------------------------------------------
    if Assigned( FPreview ) then
    begin

      FAVCaptureVideoPreviewLayer := TAVCaptureVideoPreviewLayer.Wrap( TAVCaptureVideoPreviewLayer.Alloc.initWithSession( FAVCaptureSession ) );

      FAVCaptureVideoPreviewLayer.setVideoGravity( AVLayerVideoGravityResizeAspectFill );
      FAVCaptureVideoPreviewLayer.setBounds( FPreview.GetUIView.bounds );
      FAVCaptureVideoPreviewLayer.setPosition( CGPointMake( CGRectGetMidX( FPreview.GetUIView.bounds ), CGRectGetMidY( FPreview.GetUIView.bounds ) ) );

      FPreview.GetUIView.layer.insertSublayer( FAVCaptureVideoPreviewLayer, 0 { FPreview.GetUIView.layer } );
    end;

    // ----------------------------------------------------
    // Setup Capture Audio Device
    // ----------------------------------------------------
    if FAudioRecordPermission and FAudioRecord then
    begin
      captureAudioDevice         := TAVCaptureDevice.Wrap( TAVCaptureDevice.OCClass.defaultDeviceWithMediaType( AVMediaTypeAudio ) );
      FAVCaptureDeviceAudioInput := TAVCaptureDeviceInput.Wrap( TAVCaptureDeviceInput.OCClass.deviceInputWithDevice( captureAudioDevice, error ) );
      FAVCaptureSession.addInput( FAVCaptureDeviceAudioInput );

      FAVCaptureAudioDataOutput := TAVCaptureAudioDataOutput.Create;

      AudioQueue := dispatch_queue_create( 'Audio Capture Queue', 0 );
      FAVCaptureAudioDataOutput.setSampleBufferDelegate( ILocalObject( FVideoAudioSampleDelegate ).GetObjectID, AudioQueue );
      dispatch_release( AudioQueue );
      FAVCaptureSession.addOutput( FAVCaptureAudioDataOutput );
      FAVCaptureAudioDataOutput.release;
    end;

    // --------------------------------------------------------
    SetTorchMode( FTorchMode );

    // --------------------------------------------------------
    SetupAssetWriters;

    movieWritingQueue := dispatch_queue_create( 'Movie Writing Queue', 0 );

    // --------------------------------------------------------
    FAVCaptureSession.startRunning;
    FRecordingState := rsReady;
  finally
    myLock.unlock;
  end;
end;

// ------------------------------------------------------------------------------
function TDPFCamera.DoSampleBufferToUIImage: UIImage;
var
  imageBuffer               : CVImageBufferRef;
  baseAddress               : Pointer;
  width, height, bytesPerRow: Longword;
  colorSpace                : CGColorSpaceRef;
  context                   : CGContextRef;
  quartzImage               : CGImageRef;
  ImgOri, OriBack, OriFront : NativeUInt;
begin
  if FSampleBuffer = nil then
    Exit;

  imageBuffer := CMSampleBufferGetImageBuffer( FSampleBuffer );
  CVPixelBufferLockBaseAddress( imageBuffer, 0 );
  baseAddress := CVPixelBufferGetBaseAddress( imageBuffer );
  bytesPerRow := CVPixelBufferGetBytesPerRow( imageBuffer );
  width       := CVPixelBufferGetWidth( imageBuffer );
  height      := CVPixelBufferGetHeight( imageBuffer );
  colorSpace  := CGColorSpaceCreateDeviceRGB( );
  context     := CGBitmapContextCreate( baseAddress, width, height, 8, bytesPerRow, colorSpace, kCGBitmapByteOrder32Little or kCGImageAlphaPremultipliedFirst );

  OriBack  := UIImageOrientationRight;
  OriFront := UIImageOrientationLeftMirrored;

  case FCurrentDevice.orientation of

    UIDeviceOrientationPortrait:
      begin
        OriBack  := UIImageOrientationRight;
        OriFront := UIImageOrientationLeftMirrored;
      end;

    UIDeviceOrientationLandscapeLeft:
      begin
        OriBack  := UIImageOrientationUp;
        OriFront := UIImageOrientationUpMirrored;
      end;

    UIDeviceOrientationLandscapeRight:
      begin
        OriBack  := UIImageOrientationDown;
        OriFront := UIImageOrientationDownMirrored;
      end;
    UIDeviceOrientationPortraitUpsideDown:
      begin
        OriBack  := UIImageOrientationUp;
        OriFront := UIImageOrientationUpMirrored;
      end;
  end;

  quartzImage := CGBitmapContextCreateImage( context );
  CVPixelBufferUnlockBaseAddress( imageBuffer, 0 );
  CGContextRelease( context );
  CGColorSpaceRelease( colorSpace );

  ImgOri := OriBack;
  if FCameraPosition = cpFront then
    ImgOri := OriFront;

  result := TUIImage.Wrap( TUIImage.Alloc.initWithCGImage( quartzImage, 1.0, ImgOri ) );
  CGImageRelease( quartzImage );
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.DoSampleBufferToBitmap( ABitmap: TBitmap );
begin
  ABitmap := UIImageToBitmap( DoSampleBufferToUIImage, 0 );
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.exportCompletionHandler;
begin
  dispatch_sync( main_queue,
    procedure
    var
      status: Integer;
    begin
      status := exportSession.status;

      if Assigned( exportSessionTimer ) then
      begin
        exportSessionTimer.invalidate;
        exportSessionTimer := nil;
      end;
      if Assigned( exportSession ) then
        exportSession.release;
      exportSession := nil;
      if Assigned( FOnConvertComplete ) then
        FOnConvertComplete( Self, status );
    end );
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.DoConvertToMP4;
var
  avAsset          : AVURLAsset;
  compatiblePresets: NSArray;
  outputMP4URL     : NSURL;
  outputMovURL     : NSURL;
  MP4Quality       : NSString;
begin
  RemoveFile( FOutputFileName );
  outputMP4URL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FOutputFileName ) ) );
  outputMovURL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FOutputMovFileName ) ) );

  case FConvertToMP4Quality of
    mcqLow:
      MP4Quality := AVAssetExportPresetLowQuality;
    mcqMedium:
      MP4Quality := AVAssetExportPresetMediumQuality;
    mcqHighest:
      MP4Quality := AVAssetExportPresetHighestQuality;
  else
    MP4Quality := AVAssetExportPresetHighestQuality;
  end;
  avAsset           := TAVURLAsset.Wrap( TAVURLAsset.OCClass.URLAssetWithURL( outputMovURL, nil ) );
  compatiblePresets := TAVAssetExportSession.OCClass.exportPresetsCompatibleWithAsset( avAsset );

  exportSessionTimer := nil;
  if compatiblePresets.containsObject( ( MP4Quality as ILocalObject ).GetObjectID ) then
  begin
    exportSession := TAVAssetExportSession.Wrap( TAVAssetExportSession.Alloc.initWithAsset( avAsset, MP4Quality ) );
    if Assigned( exportSession ) then
    begin
      exportSession.setOutputURL( outputMP4URL );
      exportSession.setShouldOptimizeForNetworkUse( True );
      exportSession.setOutputFileType( AVFileTypeMPEG4 );

      if Assigned( FOnConvertProgress ) then
        exportSessionTimer := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( 0.1, FDPFEexportSessionTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );

      exportSession.exportAsynchronouslyWithCompletionHandler( exportCompletionHandler );
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFCamera.FinishWritingCompletionHandler;
var
  SaveToAlbumLibrary: Boolean;
begin
  SaveToAlbumLibrary := false;
  if Assigned( FOnCaptureFinished ) then
    FOnCaptureFinished( self, SaveToAlbumLibrary );
  if SaveToAlbumLibrary then
  begin
    if UIVideoAtPathIsCompatibleWithSavedPhotosAlbum( PNSStr( FOutputMovFileName ) ) then
      UISaveVideoAtPathToSavedPhotosAlbum( PNSStr( FOutputMovFileName ), nil, nil, nil );
  end;
end;

// ------------------------------------------------------------------------------
function adjustTime( sample: CMSampleBufferRef; offset: CMTime ): CMSampleBufferRef;
var
  count: CMItemCount;
  pInfo: PCMSampleTimingInfo;
  P    : PCMSampleTimingInfo;
  I    : Integer;
begin
  CMSampleBufferGetSampleTimingInfoArray( sample, 0, nil, @count );
  pInfo := malloc( sizeof( CMSampleTimingInfo ) * count );
  try
    CMSampleBufferGetSampleTimingInfoArray( sample, count, pInfo, @count );
    for i := 0 to count - 1 do
    begin
      P                        := PCMSampleTimingInfo( NativeUInt( pInfo ) + i * sizeOf( CMSampleTimingInfo ) );
      P^.decodeTimeStamp       := CMTimeSubtract( P^.decodeTimeStamp, offset );
      P^.presentationTimeStamp := CMTimeSubtract( P^.presentationTimeStamp, offset );
    end;
    CMSampleBufferCreateCopyWithNewTiming( nil, sample, count, pInfo, @Result );
  finally
    free( pInfo );
  end;
end;

// ------------------------------------------------------------------------------
{ TVideoSampleDelegate }
procedure TVideoAudioSampleDelegate.captureOutput( captureOutput: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection );
var
  formatDescription        : CMFormatDescriptionRef;
  mediaType                : CMMediaType;
  offset, last, T, dur, pts: CMTime;
begin
  try
    if FDPFCamera.FRecordingState <> rsStart then
      exit;

    if FDPFCamera.assetWriter = nil then
    begin
      if not FDPFCamera.SetupAssetWriters then
      begin
        DPFNSLog( 'FDPFCamera.SetupAssetWriters: ' + UTF8ToString( FDPFCamera.AssetWriter.error.localizedDescription.UTF8String ) );
        Exit;
      end;
    end;

    formatDescription := CMSampleBufferGetFormatDescription( didOutputSampleBuffer );
    mediaType         := CMFormatDescriptionGetMediaType( formatDescription );

    if FDPFCamera.AssetWriter.status = AVAssetWriterStatusUnknown then
    begin
      if not FDPFCamera.myLock2.tryLock then
        exit;
      try
        if FDPFCamera.AssetWriter.status = AVAssetWriterStatusUnknown then
          if FDPFCamera.AssetWriter.startWriting then
          begin
            FDPFCamera.FStartPresentationTimeStamp := CMSampleBufferGetPresentationTimeStamp( didOutputSampleBuffer );
            FDPFCamera.AssetWriter.startSessionAtSourceTime( FDPFCamera.FStartPresentationTimeStamp );
          end
          else
          begin
            DPFNSLog( 'AssetWriter.startWriting: ' + UTF8ToString( FDPFCamera.AssetWriter.error.localizedDescription.UTF8String ) );
            exit;
          end
      finally
        FDPFCamera.myLock2.unlock;
      end;
    end;

    // Calc Pause Timing
    if FDPFCamera._discont then
    begin
      if mediaType = kCMMediaType_Audio then
        exit;

      FDPFCamera._discont := false;

      // calc adjustment
      pts  := CMSampleBufferGetPresentationTimeStamp( didOutputSampleBuffer );
      last := FDPFCamera._lastAudio;
      if ( last.flags and kCMTimeFlags_Valid ) <> 0 then
      begin
        if ( FDPFCamera._timeOffset.flags and kCMTimeFlags_Valid ) <> 0 then
        begin
          pts := CMTimeSubtract( pts, FDPFCamera._timeOffset );
        end;
        offset := CMTimeSubtract( pts, last );

        // this stops us having to set a scale for _timeOffset before we see the first video time
        if ( FDPFCamera._timeOffset.value = 0 ) then
        begin
          FDPFCamera._timeOffset := offset;
        end
        else
        begin
          FDPFCamera._timeOffset := CMTimeAdd( FDPFCamera._timeOffset, offset );
        end;
      end;
      FDPFCamera._lastVideo.flags := 0;
      FDPFCamera._lastAudio.flags := 0;

    end;

    CFRetain( didOutputSampleBuffer );
    if ( FDPFCamera._timeOffset.value > 0 ) then
    begin
      CFRelease( didOutputSampleBuffer );
      didOutputSampleBuffer := adjustTime( didOutputSampleBuffer, FDPFCamera._timeOffset );
    end;

    try
      pts := CMSampleBufferGetPresentationTimeStamp( didOutputSampleBuffer );
      dur := CMSampleBufferGetDuration( didOutputSampleBuffer );
      if dur.value > 0 then
        pts := CMTimeAdd( pts, dur );

      if mediaType = kCMMediaType_Video then
      begin

        FDPFCamera._lastVideo := pts;
        if ( pts.value > 0 ) and Assigned( FDPFCamera.FOnSampleBufferReady ) then
        begin
          T                          := CMTimeSubtract( pts, FDPFCamera.FStartPresentationTimeStamp );
          FDPFCamera.FRecordDuration := 0;
          if T.timescale > 0 then
            FDPFCamera.FRecordDuration := T.value div T.timescale;
          dispatch_sync( main_queue,
            procedure
            begin
              FDPFCamera.SyncCallSampleBufferReady;
            end );
        end;

        if FDPFCamera.AssetWriter = nil then
          exit;

        if FDPFCamera.AssetWriter.status = AVAssetWriterStatusWriting then
          if Assigned( FDPFCamera.VideoWriterInput ) and FDPFCamera.VideoWriterInput.isReadyForMoreMediaData then
            if not FDPFCamera.VideoWriterInput.appendSampleBuffer( didOutputSampleBuffer { sbufWithNewTiming } ) then
              DPFNSLog( 'VideoWriterInput.appendSampleBuffer: ' + UTF8ToString( FDPFCamera.AssetWriter.error.localizedDescription.UTF8String ) );

        FDPFCamera.currentFrame := FDPFCamera.currentFrame + 1;

        if ( ( FDPFCamera.MaxRecordFrame > 0 ) and ( FDPFCamera.currentFrame >= FDPFCamera.MaxRecordFrame ) ) or ( ( FDPFCamera.MaxRecordDuration > 0 ) and ( FDPFCamera.FRecordDuration > FDPFCamera.MaxRecordDuration ) ) then
        begin
          dispatch_sync( main_queue,
            procedure
            begin
              FDPFCamera.StopRecording;
            end );
          exit;
        end;
      end
      else if mediaType = kCMMediaType_Audio then
      begin
        FDPFCamera._lastAudio := pts;
        if FDPFCamera.AssetWriter.status = AVAssetWriterStatusWriting then
          if Assigned( FDPFCamera.AudioWriterInput ) and FDPFCamera.AudioWriterInput.isReadyForMoreMediaData then
            if not FDPFCamera.AudioWriterInput.appendSampleBuffer( didOutputSampleBuffer ) then
              DPFNSLog( 'AudioWriterInput.appendSampleBuffer: ' + UTF8ToString( FDPFCamera.AssetWriter.error.localizedDescription.UTF8String ) );
      end;
    finally
      CFRelease( didOutputSampleBuffer );
    end;

  except
    on E: Exception do
      DPFNSLog( 'TVideoAudioSampleDelegate.captureOutput: ' + E.Message );
  end;
end;

// ------------------------------------------------------------------------------
constructor TCaptureSessionStoppedRunningNotification.create( ADPFCamera: TDPFCamera );
begin
  inherited create;
  FDPFCamera := ADPFCamera;
end;

// ------------------------------------------------------------------------------
procedure TCaptureSessionStoppedRunningNotification.CaptureSessionStoppedRunningNotification( notification: NSNotification );
begin

  if not Assigned( FDPFCamera.FAVCaptureSession ) or not Assigned( FDPFCamera.AssetWriter ) or not Assigned( FDPFCamera.VideoWriterInput ) or ( FDPFCamera.FAudioRecordPermission and FDPFCamera.FAudioRecord and not Assigned( FDPFCamera.AudioWriterInput ) ) then
    exit;

  dispatch_async( FDPFCamera.movieWritingQueue,
    procedure
    var
      flag: boolean;
    begin
      try
        flag := FDPFCamera.FRecordingState in [rsStart, rsPause];
        if flag then
        begin
          FDPFCamera.VideoWriterInput.markAsFinished;
          if FDPFCamera.FAudioRecordPermission and FDPFCamera.FAudioRecord then
            FDPFCamera.AudioWriterInput.markAsFinished;

          FDPFCamera.AssetWriter.finishWritingWithCompletionHandler( FDPFCamera.FinishWritingCompletionHandler );
        end;

        FDPFCamera.ReleaseDevice;

        if flag and FDPFCamera.FConvertToMP4 then
        begin
          dispatch_async( main_queue,
            procedure
            begin
              FDPFCamera.DoConvertToMP4;
            end );
        end;

      finally
        FDPFCamera.FRecordingState := rsStop;
      end;

      if Assigned( FDPFCamera.FAVCaptureVideoPreviewLayer ) then
      begin
        FDPFCamera.FAVCaptureVideoPreviewLayer.removeFromSuperlayer;
        FDPFCamera.FAVCaptureVideoPreviewLayer.release;
        FDPFCamera.FAVCaptureVideoPreviewLayer := nil;
      end;
    end );

end;

// ------------------------------------------------------------------------------
{ TDPFEexportSessionTimerDelegate }
constructor TDPFEexportSessionTimerDelegate.Create( ADPFCamera: TDPFCamera );
begin
  inherited Create;
  FDPFCamera := ADPFCamera;
end;

// ------------------------------------------------------------------------------
procedure TDPFEexportSessionTimerDelegate.ondidTimer( timer: NSTimer );
begin
  try
    if Assigned( FDPFCamera.exportSession ) and Assigned( FDPFCamera.FOnConvertProgress ) then
      FDPFCamera.FOnConvertProgress( FDPFCamera, FDPFCamera.exportSession.progress );
    { if Assigned( FDPFCamera.exportSessionTimer ) and ( FDPFCamera.exportSession.progress > 0.99 ) then
      timer.invalidate; }
  except
  end;
end;

// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
end.
