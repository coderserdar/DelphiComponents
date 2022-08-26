// ------------------------------------------------------------------------------
// DPF.iOS.AVAudioRecorder Component
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
unit DPF.iOS.AVAudioRecorder;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  FMX.Types,
  FMX.Controls,
  System.TypInfo,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.MediaPlayer,
  DPF.iOS.Dispatch,
{$IFDEF IOS}
  iOSapi.CocoaTypes,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  IOSapi.MediaPlayer,
  IOSapi.CoreMedia,
  IOSapi.AVFoundation,
  IOSapi.Foundation,
  IOSapi.CoreGraphics,
  IOSapi.UIKit,
  iOSapi.CoreAudio,
  FMX.Platform.iOS,
  DPF.iOS.Classes,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Forms;

// ------------------------------------------------------------------------------

type

  TDPFAVAudioRecorder = class;

  TRecordEncoding = ( ENC_AAC = 1, ENC_ALAC = 2, ENC_IMA4 = 3, ENC_ILBC = 4, ENC_ULAW = 5, ENC_PCM = 6 );

  TOnFinishRecording = procedure( sender: TObject; currentTime: double ) of object;
  TOnRecording       = procedure( sender: TObject; currentTime: double ) of object;
{$IFDEF IOS}

  // ------------------------------------------------------------------------------

  IDPFAVRecorderTimerDelegate = interface( IObjectiveC )
    ['{5F8FE122-D58B-48E0-8D49-593BF17C3649}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFAVRecorderTimerDelegate = class( TOCLocal, IDPFAVRecorderTimerDelegate )
  private
    FDPFAVAudioRecorder: TDPFAVAudioRecorder;
  public
    constructor Create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  AVAudioRecorderDelegate = interface( IObjectiveC )
    ['{0674AA5E-3D2B-490D-84FC-4E2B9D6CDEC1}']
    procedure audioRecorderDidFinishRecording( recorder: AVAudioRecorder; successfully: Boolean ); cdecl;
    procedure audioRecorderEncodeErrorDidOccur( recorder: AVAudioRecorder; error: NSError ); cdecl;
  end;

  TDPFAVAudioRecorderDelegate = class( TOCLocal, AVAudioRecorderDelegate )
  private
    FDPFAVAudioRecorder: TDPFAVAudioRecorder;
  public
    constructor create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );

    procedure audioRecorderDidFinishRecording( recorder: AVAudioRecorder; successfully: Boolean ); cdecl;
    procedure audioRecorderEncodeErrorDidOccur( recorder: AVAudioRecorder; error: NSError ); cdecl;
  end;

  // ------------------------------------------------------------------------------

  // ------------------------------------------------------------------------------
  AVAudioRecordSessionDelegate = interface( IObjectiveC )
    ['{564F55C0-4BFA-4FFD-9691-D8E537BA006C}']
    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags( flags: NSUInteger ); cdecl;
    procedure inputIsAvailableChanged( isInputAvailable: Boolean ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFAVAudioRecordSessionDelegate = class( TOCLocal, AVAudioRecordSessionDelegate )
  private
    FDPFAVAudioRecorder: TDPFAVAudioRecorder;
  public
    constructor Create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );

    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags( flags: NSUInteger ); cdecl;
    procedure inputIsAvailableChanged( isInputAvailable: Boolean ); cdecl;
  end;

  // ---------------------------------------------------------------------------
{$ENDIF}

  // ---------------------------------------------------------------------------
  TDPFAVAudioRecorderStatus = ( AVPlayerStatusUnknown = 0, AVPlayerStatusReadyToPlay = 1, AVPlayerStatusFailed = 2 );

  // ----------------------------------------------------------------------------
  TDPFOnAudioError = procedure( sender: TObject; const Error: string ) of object;

  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFAVAudioRecorder = class( TComponent )
  private
    { Private declarations }
{$IFDEF IOS}
    FNSTimer                        : NSTimer;
    FAVAudioRecorder                : AVAudioRecorder;
    FDPFAVAudioRecorderDelegate     : TDPFAVAudioRecorderDelegate;
    FDPFAVAudioRecordSessionDelegate: TDPFAVAudioRecordSessionDelegate;
    FDPFTimerDelegate               : TDPFAVRecorderTimerDelegate;
{$ENDIF}
    FCurrentTime       : Double;
    FOnError           : TDPFOnAudioError;
    FRecordInBackground: Boolean;
    FAudioHWAvailable  : Boolean;
    FOnFinishRecording : TOnFinishRecording;
    FOnRecording       : TOnRecording;
  protected
    { Protected declarations }

    // FBlockObserv: Pointer;
    // procedure AVPlayerPeriodicTimeBlock( time: pointer );
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function Pause: Boolean;
    function StartRecording( const FileName: string; recordDuration: Double; RecordEncoding: TRecordEncoding = ENC_IMA4; NumberOfChannelsKey: Integer = 2; LinearPCMBitDepthKey: integer = 16; SampleRateKey: single = 44100.0; AudioQuality: Integer = $40 ): Boolean;
    procedure StopRecording;

  published
    { Published declarations }
    property OnError           : TDPFOnAudioError read FOnError write FOnError;
    property RecordInBackground: Boolean read FRecordInBackground write FRecordInBackground default false;
    property OnFinishRecording : TOnFinishRecording read FOnFinishRecording write FOnFinishRecording;
    property OnRecording       : TOnRecording read FOnRecording write FOnRecording;
  end;

{$IFDEF IOS}

function AVLinearPCMIsBigEndianKey: NSString;
function AVLinearPCMIsFloatKey: NSString;
function AVEncoderAudioQualityKey: NSString;
{$ENDIF}

implementation

{$IFDEF IOS}

function AVLinearPCMIsBigEndianKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVLinearPCMIsBigEndianKey' );
end;

// ------------------------------------------------------------------------------
function AVLinearPCMIsFloatKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVLinearPCMIsFloatKey' );
end;

// ------------------------------------------------------------------------------
function AVEncoderAudioQualityKey: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVEncoderAudioQualityKey' );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFAVAudioRecorder }
constructor TDPFAVAudioRecorder.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FCurrentTime := 0;
{$IFDEF IOS}
  FDPFTimerDelegate                := TDPFAVRecorderTimerDelegate.Create( Self );
  FDPFAVAudioRecorderDelegate      := TDPFAVAudioRecorderDelegate.create( Self );
  FDPFAVAudioRecordSessionDelegate := TDPFAVAudioRecordSessionDelegate.Create( Self );
  FAVAudioRecorder                 := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFAVAudioRecorder.Destroy;
begin
{$IFDEF IOS}
  if Assigned( TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).delegate ) then
  begin
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setDelegate( nil );
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setActive( false, nil );
    TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).endReceivingRemoteControlEvents;
  end;

  FDPFAVAudioRecorderDelegate.DisposeOf;
  FDPFAVAudioRecordSessionDelegate.DisposeOf;
  FDPFTimerDelegate.DisposeOf;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFAVAudioRecorder.Pause: Boolean;
begin
  result := false;
{$IFDEF IOS}
  if Assigned( FAVAudioRecorder ) then
  begin
    FAVAudioRecorder.pause;
    result := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
// Supported formats :
//
// 1. AAC
// 2. ALAC
// 3. IMA4
// 4. ILBC
// 5. ULAW
// 6. PCM
//
// LinearPCMBitDepthKey = 8, 16, 24, or 32 ;
// SampleRateKey =
// AudioQuality =(
// AVAudioQualityMin       = 0,
// AVAudioQualityLow       = $20,
// AVAudioQualityMedium    = $40,
// AVAudioQualityHigh      = $60,
// AVAudioQualityMax       = $7F
// );
function TDPFAVAudioRecorder.StartRecording( const FileName: string; recordDuration: Double; RecordEncoding: TRecordEncoding = ENC_IMA4; NumberOfChannelsKey: Integer = 2; LinearPCMBitDepthKey: integer = 16; SampleRateKey: single = 44100.0; AudioQuality: Integer = $40 ): Boolean;
{$IFDEF IOS}
var
  sessionError : NSError;
  recordSetting: NSMutableDictionary;
  url          : NSURL;
  err          : NSError;
{$ENDIF}
begin
  result       := true;
  FCurrentTime := 0;
{$IFDEF IOS}
  if assigned( FAVAudioRecorder ) and FAVAudioRecorder.isRecording then
    exit;
  if assigned( FAVAudioRecorder ) then
    FAVAudioRecorder.release;

  TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setCategory( AVAudioSessionCategoryPlayAndRecord, sessionError );
  if Assigned( FAVAudioRecorder ) then
  begin
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setDelegate( FDPFAVAudioRecordSessionDelegate.GetObjectID );
    TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).beginReceivingRemoteControlEvents;
  end;
  TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setActive( true, sessionError );

  recordSetting := TNSMutableDictionary.Wrap( TNSMutableDictionary.Alloc.init );
  case RecordEncoding of
    ENC_PCM:
      begin
        recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatLinearPCM ), AVFormatIDKey );
        recordSetting.setValue( TNSNumber.OCClass.numberWithBool( false ), AVLinearPCMIsBigEndianKey );
        recordSetting.setValue( TNSNumber.OCClass.numberWithBool( false ), AVLinearPCMIsFloatKey );
      end;
    ENC_ALAC:
      recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatAppleLossless ), AVFormatIDKey );
    ENC_IMA4:
      recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatAppleIMA4 ), AVFormatIDKey );
    ENC_ILBC:
      recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatiLBC ), AVFormatIDKey );
    ENC_ULAW:
      recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatULaw ), AVFormatIDKey );
  else
    recordSetting.setValue( TNSNumber.OCClass.numberWithInt( kAudioFormatULaw ), AVFormatIDKey );
  end;

  recordSetting.setValue( TNSNumber.OCClass.numberWithFloat( SampleRateKey ), AVSampleRateKey );
  recordSetting.setValue( TNSNumber.OCClass.numberWithInt( integer( NumberOfChannelsKey ) ), AVNumberOfChannelsKey );

  recordSetting.setValue( TNSNumber.OCClass.numberWithInt( integer( LinearPCMBitDepthKey ) ), AVLinearPCMBitDepthKey );
  recordSetting.setValue( TNSNumber.OCClass.numberWithInt( integer( AudioQuality ) ), AVEncoderAudioQualityKey );

  // --------------------------------
  url              := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSSTR( FileName ) ) );
  err              := nil;
  FAVAudioRecorder := TAVAudioRecorder.Wrap( TAVAudioRecorder.Alloc.initWithURL( url, recordSetting, err ) );
  recordSetting.release;
  FAVAudioRecorder.setDelegate( FDPFAVAudioRecorderDelegate.GetObjectID );
  result := result and FAVAudioRecorder.prepareToRecord;
  if result then
  begin
    FAVAudioRecorder.setMeteringEnabled( true );
    FAudioHWAvailable := TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).inputIsAvailable;
    Result            := FAudioHWAvailable and Result;
  end;

  if not Result then
  begin
    if assigned( FAVAudioRecorder ) then
    begin
      FAVAudioRecorder.release;
      FAVAudioRecorder := nil;
    end;
  end;

  if Result then
  begin
    FAVAudioRecorder.recordForDuration( RecordDuration );
    if not Assigned( FNSTimer ) then
    begin
      FNSTimer := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( 0.1, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecorder.StopRecording;
begin
{$IFDEF IOS}
  if not assigned( FAVAudioRecorder ) then
    exit;
  FAVAudioRecorder.stop;

  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFDEF IOS}

{ TDPFAVAudioRecorderDelegate }
constructor TDPFAVAudioRecorderDelegate.create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );
begin
  inherited create;
  FDPFAVAudioRecorder := ADPFAVAudioRecorder;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecorderDelegate.audioRecorderDidFinishRecording( recorder: AVAudioRecorder; successfully: Boolean );
begin
  if Assigned( FDPFAVAudioRecorder.FNSTimer ) then
  begin
    FDPFAVAudioRecorder.FNSTimer.invalidate;
    FDPFAVAudioRecorder.FNSTimer := nil;
  end;
  if assigned( FDPFAVAudioRecorder.FOnFinishRecording ) then
    FDPFAVAudioRecorder.FOnFinishRecording( FDPFAVAudioRecorder, FDPFAVAudioRecorder.FCurrentTime );
  if assigned( FDPFAVAudioRecorder.FAVAudioRecorder ) then
  begin
    FDPFAVAudioRecorder.FAVAudioRecorder.release;
    FDPFAVAudioRecorder.FAVAudioRecorder := nil;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecorderDelegate.audioRecorderEncodeErrorDidOccur( recorder: AVAudioRecorder; error: NSError );
begin
  if Assigned( FDPFAVAudioRecorder.FNSTimer ) then
  begin
    FDPFAVAudioRecorder.FNSTimer.invalidate;
    FDPFAVAudioRecorder.FNSTimer := nil;
  end;
  if Assigned( FDPFAVAudioRecorder ) and Assigned( FDPFAVAudioRecorder.FOnError ) then
    FDPFAVAudioRecorder.FOnError( FDPFAVAudioRecorder, UTF8ToString( error.localizedDescription.UTF8String ) );
end;

// ------------------------------------------------------------------------------
{ TDPFAVAudioRecordSessionDelegate }
// ------------------------------------------------------------------------------
constructor TDPFAVAudioRecordSessionDelegate.Create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );
begin
  inherited Create;
  FDPFAVAudioRecorder := ADPFAVAudioRecorder;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecordSessionDelegate.beginInterruption;
begin
  DPFNSLog( 'TDPFAVAudioRecordSessionDelegate.beginInterruption' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecordSessionDelegate.endInterruption;
begin
  DPFNSLog( 'TDPFAVAudioRecordSessionDelegate.endInterruption' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecordSessionDelegate.endInterruptionWithFlags( flags: NSUInteger );
begin
  DPFNSLog( 'TDPFAVAudioRecordSessionDelegate.endInterruptionWithFlags( flags: NSUInteger );' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioRecordSessionDelegate.inputIsAvailableChanged( isInputAvailable: Boolean );
begin
  DPFNSLog( 'TDPFAVAudioRecordSessionDelegate.inputIsAvailableChanged( isInputAvailable: Boolean );' );
end;

// ------------------------------------------------------------------------------
{ TDPFAVPlayerTimerDelegate }
constructor TDPFAVRecorderTimerDelegate.Create( ADPFAVAudioRecorder: TDPFAVAudioRecorder );
begin
  inherited create;
  FDPFAVAudioRecorder := ADPFAVAudioRecorder;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVRecorderTimerDelegate.ondidTimer( timer: NSTimer );
var
  ct: double;
begin
  if assigned( FDPFAVAudioRecorder.FOnRecording ) and assigned( FDPFAVAudioRecorder.FAVAudioRecorder ) and ( FDPFAVAudioRecorder.FAVAudioRecorder.isRecording ) then
  begin
    ct := FDPFAVAudioRecorder.FAVAudioRecorder.currentTime;
    if FDPFAVAudioRecorder.FCurrentTime <> ct then
      FDPFAVAudioRecorder.FCurrentTime := ct;
    FDPFAVAudioRecorder.FOnRecording( FDPFAVAudioRecorder, FDPFAVAudioRecorder.FCurrentTime );
  end;
end;
// ------------------------------------------------------------------------------
{$ENDIF}

end.
