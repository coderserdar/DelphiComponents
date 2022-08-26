// ------------------------------------------------------------------------------
// DPF.iOS.AVPlayer Component
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
unit DPF.iOS.AVPlayer;

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
  HTTPApp,

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
  FMX.Platform.iOS,
  DPF.iOS.Classes,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Forms;

// ------------------------------------------------------------------------------
const
  PLAY_STATUS_FIRST_VALUE = -1000;
  OPEN_STATUS_FIRST_VALUE = -1001;

type

  TDPFAVPlayer = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IAVPlayerDelegate = interface( IObjectiveC )
    ['{D52B4A66-7F1E-40B7-A770-AC95A8A1CFC0}']

    procedure itemDidFinishPlaying; cdecl;

    procedure itemFailedToPlay; cdecl;
    procedure itemTimeJumpedNotification; cdecl;
    procedure itemPlaybackStalled; cdecl;
    procedure itemNewAccessLogEntry; cdecl;
    procedure itemNewErrorLogEntry; cdecl;

  end;

  TDPFAVPlayerDelegate = class( TOCLocal, IAVPlayerDelegate )
  private
    FDPFAVPlayer: TDPFAVPlayer;
  public
    constructor create( ADPFAVPlayer: TDPFAVPlayer );

    procedure itemDidFinishPlaying; cdecl;

    procedure itemFailedToPlay; cdecl;
    procedure itemTimeJumpedNotification; cdecl;
    procedure itemPlaybackStalled; cdecl;
    procedure itemNewAccessLogEntry; cdecl;
    procedure itemNewErrorLogEntry; cdecl;
  end;

  // ------------------------------------------------------------------------------

  IDPFAVPlayerTimerDelegate = interface( IObjectiveC )
    ['{C54F12F1-8E42-4C96-A1B0-5DB83E2BF821}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFAVPlayerTimerDelegate = class( TOCLocal, IDPFAVPlayerTimerDelegate )
  private
    FDPFAVPlayer: TDPFAVPlayer;
  public
    constructor Create( ADPFAVPlayer: TDPFAVPlayer );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  AVAudioSessionDelegate = interface( IObjectiveC )
    ['{21D9508D-7491-44F4-AE69-2EDEEBA4F271}']
    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags( flags: NSUInteger ); cdecl;
    procedure inputIsAvailableChanged( isInputAvailable: Boolean ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFAVAudioSessionDelegate = class( TOCLocal, AVAudioSessionDelegate )
  private
    FDPFAVPlayer: TDPFAVPlayer;
  public
    constructor Create( ADPFAVPlayer: TDPFAVPlayer );

    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags( flags: NSUInteger ); cdecl;
    procedure inputIsAvailableChanged( isInputAvailable: Boolean ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFAVPlayerPeriodicTimeBlock = procedure( time: pointer ) of object;

  // ------------------------------------------------------------------------------
  AVPlayerClass = interface( NSObjectClass )
    ['{21117B6D-4876-4210-BBE6-8FD8BB6D4F63}']

    function playerWithPlayerItem( item: AVPlayerItem ): Pointer; cdecl;
    function playerWithURL( URL: NSURL ): Pointer; cdecl;
  end;

  AVPlayer = interface( NSObject )
    ['{027BD49D-FE78-464D-9B25-D8F148FC3CEA}']

    function volume: single; cdecl;               // Available in iOS 7.0 and later.
    procedure setVolume( volume: single ); cdecl; // Available in iOS 7.0 and later.

    function isMuted: boolean; cdecl;            // Available in iOS 7.0 and later.
    procedure setMuted( muted: boolean ); cdecl; // Available in iOS 7.0 and later.

    function actionAtItemEnd: AVPlayerActionAtItemEnd; cdecl;
    function allowsAirPlayVideo: Boolean; cdecl;
    function currentItem: AVPlayerItem; cdecl;
    function currentTime: CMTime; cdecl;
    function error: NSError; cdecl;
    function initWithPlayerItem( item: AVPlayerItem ): Pointer; cdecl;
    function initWithURL( URL: NSURL ): Pointer; cdecl;
    function isAirPlayVideoActive: Boolean; cdecl;
    function isClosedCaptionDisplayEnabled: Boolean; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    function rate: Single; cdecl;
    procedure removeTimeObserver( observer: Pointer ); cdecl;
    procedure replaceCurrentItemWithPlayerItem( item: AVPlayerItem ); cdecl;
    procedure seekToTime( time: CMTime ); cdecl; overload;
    procedure seekToTime( time: CMTime; toleranceBefore: CMTime; toleranceAfter: CMTime ); cdecl; overload;
    procedure setActionAtItemEnd( actionAtItemEnd: AVPlayerActionAtItemEnd ); cdecl;
    procedure setAllowsAirPlayVideo( allowsAirPlayVideo: Boolean ); cdecl;
    procedure setClosedCaptionDisplayEnabled( closedCaptionDisplayEnabled: Boolean ); cdecl;
    procedure setRate( rate: Single ); cdecl;
    procedure setUsesAirPlayVideoWhileAirPlayScreenIsActive( usesAirPlayVideoWhileAirPlayScreenIsActive: Boolean ); cdecl;
    function status: AVPlayerStatus; cdecl;
    function usesAirPlayVideoWhileAirPlayScreenIsActive: Boolean; cdecl;

    function addPeriodicTimeObserverForInterval( interval: CMTime; queue: dispatch_queue_t; usingBlock: TDPFAVPlayerPeriodicTimeBlock ): pointer; cdecl;
  end;

  TAVPlayer = class( TOCGenericImport<AVPlayerClass, AVPlayer> )
  end;

  // ------------------------------------------------------------------------------

{$ENDIF}

  // ----------------------------------------------------------------------------
  // TDPFPlayItem
  TDPFPlayItem = class( TCollectionItem )
  private
    // FOwner     : TCollection;
    FTitle     : string;
    FAlbumTitle: string;
    FArtist    : string;
    FURL       : string;
    FArtworkURL: string;
  protected
    function GetDisplayName: string; override;
  public
  published
    property URL       : string read FURL write FURL;
    property Title     : string read FTitle write FTitle;
    property AlbumTitle: string read FAlbumTitle write FAlbumTitle;
    property Artist    : string read FArtist write FArtist;
    property ArtworkURL: string read FArtworkURL write FArtworkURL;
  end;

  // ----------------------------------------------------------------------------
  // TDPFPlayItemCollection
  TDPFPlayItemCollection = class( TCollection )
  private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFPlayItem;
    procedure SetItem( Index: Integer; Value: TDPFPlayItem );

  public
    constructor Create( AOwner: TComponent );

    function Add( URL: string; Title: string; AlbumTitle: string; Artist: string; ArtworkURL: string ): TDPFPlayItem;
    function Insert( Index: Integer; URL: string; Title: string; AlbumTitle: string; Artist: string; ArtworkURL: string ): TDPFPlayItem;

    property Items[index: Integer]: TDPFPlayItem read GetItem write SetItem; default;
  end;

  // ---------------------------------------------------------------------------
  TDPFAVPlayerStatus = ( AVPlayerStatusUnknown = 0, AVPlayerStatusReadyToPlay = 1, AVPlayerStatusFailed = 2 );

  // ----------------------------------------------------------------------------
  TDPFOnAudioFinishPlaying                 = procedure( sender: TObject; var ContinuePlayList: Boolean ) of object;
  TDPFOnAudioPlayingEvent                  = procedure( sender: TObject; currentTime: Int64; duration: int64 ) of object;
  TDPFOnAudioStatusChanged                 = procedure( sender: TObject; Status: TDPFAVPlayerStatus ) of object;
  TDPFOnAudioPlaybackBufferEmptyChanged    = procedure( sender: TObject; const PlaybackBufferEmpty: Boolean ) of object;
  TDPFOnAudioPlaybackBufferFullChanged     = procedure( sender: TObject; const PlaybackBufferFull: Boolean ) of object;
  TDPFOnAudioPlaybackLikelyToKeepUpChanged = procedure( sender: TObject; const PlaybackLikelyToKeepUp: Boolean ) of object;
  TDPFOnAudioError                         = procedure( sender: TObject; const Error: string ) of object;
  TDPFOnAudioOpenStatus                    = procedure( sender: TObject; const Status: Integer ) of object;

  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFAVPlayer = class( TComponent )
  private
    { Private declarations }
{$IFDEF IOS}
    FNSTimer                  : NSTimer;
    FMPAVPlayer               : AVPlayer;
    FDPFAVPlayerDelegate      : TDPFAVPlayerDelegate;
    FDPFTimerDelegate         : TDPFAVPlayerTimerDelegate;
    FDPFAVAudioSessionDelegate: TDPFAVAudioSessionDelegate;

{$ENDIF}
    FNowPlayingInfoCalled           : Boolean;
    FOnFinishPlaying                : TDPFOnAudioFinishPlaying;
    FOnPlaying                      : TDPFOnAudioPlayingEvent;
    FPlayInBackground               : Boolean;
    FOpenStatus                     : Integer;
    FPlayStatus                     : Integer;
    FLastPlaybackBufferEmpty        : Boolean;
    FLastPlaybackBufferFull         : Boolean;
    FLastPlaybackLikelyToKeepUp     : Boolean;
    FOnStatusChanged                : TDPFOnAudioStatusChanged;
    FOnPlaybackBufferEmptyChanged   : TDPFOnAudioPlaybackBufferEmptyChanged;
    FOnPlaybackBufferFullChanged    : TDPFOnAudioPlaybackBufferFullChanged;
    FOnPlaybackLikelyToKeepUpChanged: TDPFOnAudioPlaybackLikelyToKeepUpChanged;
    FOnItemError                    : TDPFOnAudioError;
    FOnError                        : TDPFOnAudioError;
    FShuffle                        : Boolean;
    FRepeat                         : Boolean;
    FCurrentPlayIndex               : Integer;
    FPlayListStarted                : Boolean;
    FURL                            : string;
    FAutoPlay                       : Boolean;
    FTitle                          : string;
    FAlbumTitle                     : string;
    FArtist                         : string;
    FArtworkURL                     : string;
    FPlayItems                      : TDPFPlayItemCollection;
    FOnOpenStatus                   : TDPFOnAudioOpenStatus;
    procedure SetPlayItems( const Value: TDPFPlayItemCollection );
  protected
    { Protected declarations }

    // FBlockObserv: Pointer;
    // procedure AVPlayerPeriodicTimeBlock( time: pointer );
  public
    { Public declarations }
    RemoteControlEventsView: TDPFUIView;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function Open( ATitle: string; AAlbumTitle: string; AArtist: string; AArtworkURL: string; URL: string; AutoPlay: Boolean ): Boolean;
    function StartPlayList( const nextItem: Boolean = true ): Boolean;
    procedure Close;
    procedure setVolume( volume: single );
    function Play: Boolean;
    function Stop: Boolean;
    function Pause: Boolean;
    function CurrentTime: Int64;
    procedure Seek( currentTimeSecont: Double );
    function isPlaying: Boolean;
    function isStopped: Boolean;
    function isPaused: Boolean;

    property PlayStatus: Integer read FPlayStatus;
    property OpenStatus: Integer read FOpenStatus;

  published

    { Published declarations }
    property PlayItems                      : TDPFPlayItemCollection read FPlayItems write SetPlayItems;
    property Shuffle                        : Boolean read FShuffle write FShuffle default false;
    property &Repeat                        : Boolean read FRepeat write FRepeat default false;
    property PlayInBackground               : Boolean read FPlayInBackground write FPlayInBackground default false;
    property OnFinishPlaying                : TDPFOnAudioFinishPlaying read FOnFinishPlaying write FOnFinishPlaying;
    property OnPlaying                      : TDPFOnAudioPlayingEvent read FOnPlaying write FOnPlaying;
    property OnStatusChanged                : TDPFOnAudioStatusChanged read FOnStatusChanged write FOnStatusChanged;
    property OnPlaybackBufferEmptyChanged   : TDPFOnAudioPlaybackBufferEmptyChanged read FOnPlaybackBufferEmptyChanged write FOnPlaybackBufferEmptyChanged;
    property OnPlaybackBufferFullChanged    : TDPFOnAudioPlaybackBufferFullChanged read FOnPlaybackBufferFullChanged write FOnPlaybackBufferFullChanged;
    property OnPlaybackLikelyToKeepUpChanged: TDPFOnAudioPlaybackLikelyToKeepUpChanged read FOnPlaybackLikelyToKeepUpChanged write FOnPlaybackLikelyToKeepUpChanged;
    property OnError                        : TDPFOnAudioError read FOnError write FOnError;
    property OnItemError                    : TDPFOnAudioError read FOnItemError write FOnItemError;
    property OnOpenStatus                   : TDPFOnAudioOpenStatus read FOnOpenStatus write FOnOpenStatus;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFAVPlayer }
constructor TDPFAVPlayer.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FNowPlayingInfoCalled       := false;
  FPlayItems                  := TDPFPlayItemCollection.Create( Self );
  FShuffle                    := false;
  FRepeat                     := false;
  FPlayInBackground           := false;
  FLastPlaybackBufferEmpty    := False;
  FLastPlaybackBufferFull     := false;
  FLastPlaybackLikelyToKeepUp := False;
{$IFDEF IOS}
  FPlayListStarted           := False;
  FCurrentPlayIndex          := -1;
  FPlayStatus                := PLAY_STATUS_FIRST_VALUE;
  FDPFAVPlayerDelegate       := TDPFAVPlayerDelegate.create( Self );
  FDPFTimerDelegate          := TDPFAVPlayerTimerDelegate.Create( Self );
  FDPFAVAudioSessionDelegate := TDPFAVAudioSessionDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFAVPlayer.Destroy;
begin
{$IFDEF IOS}
  Close;

  if Assigned( TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).delegate ) then
  begin
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setDelegate( nil );
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setActive( false, nil );
    TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).endReceivingRemoteControlEvents;
    if Assigned( RemoteControlEventsView ) then
      RemoteControlEventsView.ResignFirstResponder;
  end;

  FDPFTimerDelegate.DisposeOf;
  FDPFAVPlayerDelegate.DisposeOf;

{$ENDIF}
  FPlayItems.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.isPaused: Boolean;
begin
{$IFDEF IOS}
  result := Assigned( FMPAVPlayer ) and Assigned( FMPAVPlayer.currentItem ) and ( FMPAVPlayer.rate = 0 ) and ( round( CMTimeGetSeconds( FMPAVPlayer.currentTime ) ) > 0 );
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.isPlaying: Boolean;
begin
{$IFDEF IOS}
  result := Assigned( FMPAVPlayer ) and Assigned( FMPAVPlayer.currentItem ) and ( FMPAVPlayer.rate <> 0 );
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.isStopped: Boolean;
begin
{$IFDEF IOS}
  result := Assigned( FMPAVPlayer ) and Assigned( FMPAVPlayer.currentItem ) and ( FMPAVPlayer.rate = 0 ) and ( round( CMTimeGetSeconds( FMPAVPlayer.currentTime ) ) = 0 );
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.Pause: Boolean;
begin
  result := false;
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) then
  begin
    FMPAVPlayer.pause;
    result := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.CurrentTime: Int64;
begin
  result := 0;
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) and CMTIME_IS_VALID( FMPAVPlayer.currentTime ) then
    result := round( CMTimeGetSeconds( FMPAVPlayer.currentTime ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayer.Seek( currentTimeSecont: Double );
{$IFDEF IOS}
var
  newTime: CMTime;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) then
  begin
    Stop;
    newTime := CMTimeMakeWithSeconds( currentTimeSecont, 1 );
    FMPAVPlayer.seekToTime( newTime );
    Play;
    SetNowPlayingInfo( FTitle, FAlbumTitle, FArtist, FArtworkURL, CMTimeGetSeconds( FMPAVPlayer.currentItem.duration ), Round( CMTimeGetSeconds( FMPAVPlayer.currentTime ) ) );
    FNowPlayingInfoCalled := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayer.SetPlayItems( const Value: TDPFPlayItemCollection );
begin
  FPlayItems.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayer.setVolume( volume: single );
{$IFDEF IOS}
var
  audioTracks     : NSArray;
  i               : integer;
  track           : AVAssetTrack;
  audioInputParams: AVMutableAudioMixInputParameters;
  allAudioParams  : NSMutableArray;
  audioMix        : AVMutableAudioMix;
{$ENDIF}
begin
{$IFDEF IOS}
  if not Assigned( FMPAVPlayer ) then
    exit;

  if TOSVersion.Major >= 7.0 then
  begin
    FMPAVPlayer.setVolume( volume );
    Exit;
  end;

  allAudioParams := TNSMutableArray.Create;
  audioTracks    := FMPAVPlayer.currentItem.asset.tracks;
  for I          := 0 to audioTracks.count - 1 do
  begin
    track            := TAVAssetTrack.Wrap( audioTracks.objectAtIndex( i ) );
    audioInputParams := TAVMutableAudioMixInputParameters.Wrap( TAVMutableAudioMixInputParameters.OCClass.audioMixInputParameters );
    audioInputParams.setVolume( volume, CMTimeMake( 0, 1000000000 ) );
    audioInputParams.setTrackID( track.trackID );
    allAudioParams.addObject( ( audioInputParams as ILocalObject ).GetObjectID );
  end;
  audioMix := TAVMutableAudioMix.Wrap( TAVMutableAudioMix.OCClass.audioMix );
  audioMix.setInputParameters( allAudioParams );

  FMPAVPlayer.currentItem.setAudioMix( audioMix );
  allAudioParams.release;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.StartPlayList( const nextItem: Boolean = true ): Boolean;
begin
  result := false;
  if FPlayItems.Count = 0 then
    exit;

  Randomize;
  if FShuffle then
    FCurrentPlayIndex := Random( FPlayItems.Count )
  else
  begin
    if nextItem then
    begin
      Inc( FCurrentPlayIndex );
      if FCurrentPlayIndex >= FPlayItems.Count then
      begin
        if FRepeat then
          FCurrentPlayIndex := 0
        else
        begin
          FCurrentPlayIndex := FPlayItems.Count - 1;
          Exit;
        end;
      end;
    end
    else
    begin
      Dec( FCurrentPlayIndex );
      if FCurrentPlayIndex < 0 then
      begin
        if FRepeat then
          FCurrentPlayIndex := FPlayItems.Count - 1
        else
        begin
          FCurrentPlayIndex := 0;
          Exit;
        end;
      end;
    end;

  end;
  try
    Open( FPlayItems[FCurrentPlayIndex].Title, FPlayItems[FCurrentPlayIndex].AlbumTitle, FPlayItems[FCurrentPlayIndex].Artist, FPlayItems[FCurrentPlayIndex].ArtworkURL, FPlayItems[FCurrentPlayIndex].URL, true );
    FPlayListStarted := true;
    result           := true;
  except

  end;
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.Stop: Boolean;
begin
  result := false;
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) then
  begin
    FMPAVPlayer.pause;
    FMPAVPlayer.seekToTime( CMTimeMake( 0, 1000000000 ) );
    result := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.Play: Boolean;
begin
  result := false;
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) and Assigned( FMPAVPlayer.currentItem ) then
  begin
    FMPAVPlayer.play;
    result := true;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAVPlayer.Open( ATitle: string; AAlbumTitle: string; AArtist: string; AArtworkURL: string; URL: string; AutoPlay: Boolean ): Boolean;
{$IFDEF IOS}
var
  FNSURL      : NSURL;
  playerItem  : AVPlayerItem;
  sessionError: NSError;
{$ENDIF}
begin
  result      := false;
  FAutoPlay   := AutoPlay;
  FTitle      := ATitle;
  FAlbumTitle := AAlbumTitle;
  FArtist     := AArtist;
  FURL        := URL;
  FArtworkURL := AArtworkURL;
{$IFDEF IOS}
  { FDPFNSOperationQueue.CancelAllOperations;
    FDPFNSOperationQueue.AddOperationBlock( DoOpen );
    exit; }

  URL          := HTTPDecode( URL );
  sessionError := nil;
  if Assigned( FMPAVPlayer ) then
  begin
    Close;
  end;

  if FPlayInBackground then
  begin
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setDelegate( FDPFAVAudioSessionDelegate.GetObjectID );
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setCategory( AVAudioSessionCategoryPlayback, sessionError );
    TAVAudioSession.Wrap( TAVAudioSession.OCClass.sharedInstance ).setActive( true, sessionError );
    TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).beginReceivingRemoteControlEvents;
    if Assigned( RemoteControlEventsView ) then
      RemoteControlEventsView.becomeFirstResponder;
  end;

  FNSURL     := GetNSURL( URL );
  playerItem := TAVPlayerItem.Wrap( TAVPlayerItem.OCClass.playerItemWithURL( FNSURL ) );

  FMPAVPlayer := TAVPlayer.Wrap( TAVPlayer.OCClass.playerWithPlayerItem( playerItem ) );
  FMPAVPlayer.retain;
  FMPAVPlayer.replaceCurrentItemWithPlayerItem( playerItem );

  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemDidFinishPlaying' ), ( NSSTR( 'AVPlayerItemDidPlayToEndTimeNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemFailedToPlay' ), ( NSSTR( 'AVPlayerItemFailedToPlayToEndTimeNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemTimeJumpedNotification' ), ( NSSTR( 'AVPlayerItemTimeJumpedNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemPlaybackStalled' ), ( NSSTR( 'AVPlayerItemPlaybackStalledNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemNewAccessLogEntry' ), ( NSSTR( 'AVPlayerItemNewAccessLogEntryNotification' ) as ILocalObject ).GetObjectID, nil );
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFAVPlayerDelegate.GetObjectID, sel_getUid( 'itemNewErrorLogEntry' ), ( NSSTR( 'AVPlayerItemNewErrorLogEntryNotification' ) as ILocalObject ).GetObjectID, nil );

  FLastPlaybackBufferEmpty    := False;
  FLastPlaybackBufferFull     := false;
  FLastPlaybackLikelyToKeepUp := False;
  FPlayStatus                 := PLAY_STATUS_FIRST_VALUE;
  FOpenStatus                 := OPEN_STATUS_FIRST_VALUE;
  FNowPlayingInfoCalled       := false;

  if AutoPlay then
    FMPAVPlayer.play;

  // FBlockObserv := FMPAVPlayer.addPeriodicTimeObserverForInterval( CMTimeMake( 1, 10 ), 0, AVPlayerPeriodicTimeBlock );

  if not Assigned( FNSTimer ) then
  begin
    FNSTimer := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( 0.1, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );
  end;
  FPlayListStarted := false;
  setVolume( 1 );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
(*
  procedure TDPFAVPlayer.AVPlayerPeriodicTimeBlock( time: pointer );
  var
  currentTime: int64;
  durationF  : Double;
  duration   : int64;
  begin

  if assigned( OnPlaying ) and Assigned( FMPAVPlayer ) and Assigned( FMPAVPlayer.currentItem ) then
  begin
  // currentTime := FDPFAVPlayer.FMPAVPlayer.currentTime.value div FDPFAVPlayer.FMPAVPlayer.currentTime.timescale;
  currentTime := round( CMTimeGetSeconds( FMPAVPlayer.currentTime ) );
  durationF   := CMTimeGetSeconds( FMPAVPlayer.currentItem.duration );
  if not IsNan( durationF ) then
  duration := round( durationF );

  if ( FMPAVPlayer.status = 1 ) and isPlaying then
  OnPlaying( self, currentTime, duration );
  end;

  end;
*)

// ------------------------------------------------------------------------------
procedure TDPFAVPlayer.Close;
begin
{$IFDEF IOS}
  if Assigned( FMPAVPlayer ) then
  begin
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemDidPlayToEndTimeNotification' ) as ILocalObject ).GetObjectID, nil );
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemFailedToPlayToEndTimeNotification' ) as ILocalObject ).GetObjectID, nil );
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemTimeJumpedNotification' ) as ILocalObject ).GetObjectID, nil );
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemPlaybackStalledNotification' ) as ILocalObject ).GetObjectID, nil );
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemNewAccessLogEntryNotification' ) as ILocalObject ).GetObjectID, nil );
    DPF.iOS.Classes.TNSNotificationCenter.Wrap( DPF.iOS.Classes.TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFAVPlayerDelegate.GetObjectID, ( NSSTR( 'AVPlayerItemNewErrorLogEntryNotification' ) as ILocalObject ).GetObjectID, nil );

    FMPAVPlayer.pause;
    FMPAVPlayer.release;
    FMPAVPlayer := nil;

  end;

  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

{ TDPFAVPlayerDelegate }
constructor TDPFAVPlayerDelegate.create( ADPFAVPlayer: TDPFAVPlayer );
begin
  inherited create;
  FDPFAVPlayer := ADPFAVPlayer;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemDidFinishPlaying;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemDidFinishPlaying' );
  if assigned( FDPFAVPlayer.OnFinishPlaying ) then
    FDPFAVPlayer.OnFinishPlaying( FDPFAVPlayer, FDPFAVPlayer.FPlayListStarted );

  if FDPFAVPlayer.FPlayListStarted then
    FDPFAVPlayer.StartPlayList;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemTimeJumpedNotification; cdecl;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemTimeJumpedNotification;' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemFailedToPlay; cdecl;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemFailedToPlay;' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemPlaybackStalled; cdecl;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemPlaybackStalled;' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemNewAccessLogEntry; cdecl;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemNewAccessLogEntry;' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerDelegate.itemNewErrorLogEntry; cdecl;
begin
  DPFNSLog( 'TDPFAVPlayerDelegate.itemNewErrorLogEntry;' );
end;

// ------------------------------------------------------------------------------
{ TDPFAVPlayerTimerDelegate }
constructor TDPFAVPlayerTimerDelegate.Create( ADPFAVPlayer: TDPFAVPlayer );
begin
  inherited create;
  FDPFAVPlayer := ADPFAVPlayer;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVPlayerTimerDelegate.ondidTimer( timer: NSTimer );
var
  currentTime: int64;
  durationF  : Double;
  duration   : int64;
begin
  duration := 0;
  try
    if Assigned( FDPFAVPlayer.FMPAVPlayer ) and Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) then
    begin
      currentTime := round( CMTimeGetSeconds( FDPFAVPlayer.FMPAVPlayer.currentTime ) );
      durationF   := CMTimeGetSeconds( FDPFAVPlayer.FMPAVPlayer.currentItem.duration );
      if not IsNan( durationF ) then
        duration := round( durationF );

      if not FDPFAVPlayer.FNowPlayingInfoCalled and ( duration > 0 ) then
      begin
        FDPFAVPlayer.FNowPlayingInfoCalled := true;
        SetNowPlayingInfo( FDPFAVPlayer.FTitle, FDPFAVPlayer.FAlbumTitle, FDPFAVPlayer.FArtist, FDPFAVPlayer.FArtworkURL, CMTimeGetSeconds( FDPFAVPlayer.FMPAVPlayer.currentItem.duration ), Round( CMTimeGetSeconds( FDPFAVPlayer.FMPAVPlayer.currentTime ) ) );
      end;

      if assigned( FDPFAVPlayer.OnPlaying ) and ( FDPFAVPlayer.FMPAVPlayer.status = 1 ) and FDPFAVPlayer.isPlaying then
        FDPFAVPlayer.OnPlaying( FDPFAVPlayer, currentTime, duration );
    end;

    if FDPFAVPlayer.FOpenStatus = OPEN_STATUS_FIRST_VALUE then
    begin
      FDPFAVPlayer.FOpenStatus := isURLExists( FDPFAVPlayer.FURL );
      if Assigned( FDPFAVPlayer.FOnOpenStatus ) then
        FDPFAVPlayer.FOnOpenStatus( FDPFAVPlayer, FDPFAVPlayer.FOpenStatus );
      exit;
    end;

    if Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) and ( FDPFAVPlayer.FLastPlaybackLikelyToKeepUp <> FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackLikelyToKeepUp ) then
    begin
      FDPFAVPlayer.FLastPlaybackLikelyToKeepUp := FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackLikelyToKeepUp;
      if Assigned( FDPFAVPlayer.FOnPlaybackLikelyToKeepUpChanged ) then
        FDPFAVPlayer.FOnPlaybackLikelyToKeepUpChanged( FDPFAVPlayer, FDPFAVPlayer.FLastPlaybackLikelyToKeepUp );
    end;

    if Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) and ( FDPFAVPlayer.FLastPlaybackBufferEmpty <> FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackBufferEmpty ) then
    begin
      FDPFAVPlayer.FLastPlaybackBufferEmpty := FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackBufferEmpty;
      if Assigned( FDPFAVPlayer.FOnPlaybackBufferEmptyChanged ) then
        FDPFAVPlayer.FOnPlaybackBufferEmptyChanged( FDPFAVPlayer, FDPFAVPlayer.FLastPlaybackBufferEmpty );
    end;

    if Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) and ( FDPFAVPlayer.FLastPlaybackBufferFull <> FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackBufferFull ) then
    begin
      FDPFAVPlayer.FLastPlaybackBufferFull := FDPFAVPlayer.FMPAVPlayer.currentItem.isPlaybackBufferFull;
      if Assigned( FDPFAVPlayer.FOnPlaybackBufferFullChanged ) then
        FDPFAVPlayer.FOnPlaybackBufferFullChanged( FDPFAVPlayer, FDPFAVPlayer.FLastPlaybackBufferFull );
    end;

    if ( FDPFAVPlayer.FPlayStatus <> FDPFAVPlayer.FMPAVPlayer.status ) then
    begin
      FDPFAVPlayer.FPlayStatus := FDPFAVPlayer.FMPAVPlayer.status;
      if Assigned( FDPFAVPlayer.FOnStatusChanged ) then
        FDPFAVPlayer.FOnStatusChanged( FDPFAVPlayer, TDPFAVPlayerStatus( FDPFAVPlayer.FPlayStatus ) );
    end;

    if Assigned( FDPFAVPlayer.FMPAVPlayer.error ) and Assigned( FDPFAVPlayer.FOnError ) then
      FDPFAVPlayer.FOnError( FDPFAVPlayer, UTF8ToString( FDPFAVPlayer.FMPAVPlayer.error.localizedDescription.UTF8String ) );

    if Assigned( FDPFAVPlayer.FOnItemError ) and Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) and Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem ) and Assigned( FDPFAVPlayer.FMPAVPlayer.currentItem.error ) then
      FDPFAVPlayer.FOnItemError( FDPFAVPlayer, UTF8ToString( FDPFAVPlayer.FMPAVPlayer.currentItem.error.localizedDescription.UTF8String ) );
  except
  end;
end;

// ------------------------------------------------------------------------------
{ TDPFAVAudioSessionDelegate }
// ------------------------------------------------------------------------------
constructor TDPFAVAudioSessionDelegate.Create( ADPFAVPlayer: TDPFAVPlayer );
begin
  inherited Create;
  FDPFAVPlayer := ADPFAVPlayer;
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioSessionDelegate.beginInterruption;
begin
  DPFNSLog( 'TDPFAVAudioSessionDelegate.beginInterruption' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioSessionDelegate.endInterruption;
begin
  DPFNSLog( 'TDPFAVAudioSessionDelegate.endInterruption' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioSessionDelegate.endInterruptionWithFlags( flags: NSUInteger );
begin
  DPFNSLog( 'TDPFAVAudioSessionDelegate.endInterruptionWithFlags( flags: NSUInteger );' );
end;

// ------------------------------------------------------------------------------
procedure TDPFAVAudioSessionDelegate.inputIsAvailableChanged( isInputAvailable: Boolean );
begin
  DPFNSLog( 'TDPFAVAudioSessionDelegate.inputIsAvailableChanged( isInputAvailable: Boolean );' );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFPlayItem.GetDisplayName: string;
begin
  Result := Format( 'Player Item %d', [index] );
end;

// ------------------------------------------------------------------------------
{ TDPFPlayItemCollection }
function TDPFPlayItemCollection.Add( URL: string; Title: string; AlbumTitle: string; Artist: string; ArtworkURL: string ): TDPFPlayItem;
begin
  Result            := inherited Add as TDPFPlayItem;
  Result.URL        := URL;
  Result.Title      := Title;
  Result.AlbumTitle := AlbumTitle;
  Result.Artist     := Artist;
  Result.ArtworkURL := ArtworkURL;
end;

// ------------------------------------------------------------------------------
constructor TDPFPlayItemCollection.Create( AOwner: TComponent );
begin
  inherited Create( TDPFPlayItem );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
function TDPFPlayItemCollection.GetItem( Index: Integer ): TDPFPlayItem;
begin
  Result := inherited Items[index] as TDPFPlayItem;
end;

// ------------------------------------------------------------------------------
function TDPFPlayItemCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------------------------------------------------------------------
function TDPFPlayItemCollection.Insert( Index: Integer; URL: string; Title: string; AlbumTitle: string; Artist: string; ArtworkURL: string ): TDPFPlayItem;
begin
  Result            := inherited insert( index ) as TDPFPlayItem;
  Result.URL        := URL;
  Result.Title      := Title;
  Result.AlbumTitle := AlbumTitle;
  Result.Artist     := Artist;
  Result.ArtworkURL := ArtworkURL;
end;

// ------------------------------------------------------------------------------
procedure TDPFPlayItemCollection.SetItem( Index: Integer; Value: TDPFPlayItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
end.
