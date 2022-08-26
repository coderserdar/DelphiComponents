// ------------------------------------------------------------------------------
// DPF.iOS.MPMoviePlayerViewController Component
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
unit DPF.iOS.MPMoviePlayerViewController;

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
  DPF.iOS.Classes,
{$IFDEF IOS}
  iOSapi.QuartzCore,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.Uikit,
  iOSapi.CoreGraphics,
  iOSapi.CoreImage,
  iOSapi.MediaPlayer,
  iOSapi.CoreMedia,
  iOSapi.AVFoundation,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
  DPF.iOS.UIView,
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
{$M+}
  TDPFMPMoviePlayerViewController = class;

{$IFDEF IOS}

  MPMoviePlayerController = interface( MPMediaPlayback )
    ['{25D3F08A-4488-46FB-AC9F-2B7D4BE97DB0}']

    function accessLog: MPMovieAccessLog; cdecl;
    function allowsAirPlay: Boolean; cdecl;
    function backgroundColor: UIColor; cdecl;
    function backgroundView: UIView; cdecl;
    procedure cancelAllThumbnailImageRequests; cdecl;
    function contentURL: NSURL; cdecl;
    function controlStyle: MPMovieControlStyle; cdecl;
    function duration: NSTimeInterval; cdecl;
    function endPlaybackTime: NSTimeInterval; cdecl;
    function errorLog: MPMovieErrorLog; cdecl;
    function initWithContentURL( url: NSURL ): Pointer; cdecl;
    function initialPlaybackTime: NSTimeInterval; cdecl;
    function isAirPlayVideoActive: Boolean; cdecl;
    function isFullscreen: Boolean; cdecl;
    function loadState: MPMovieLoadState; cdecl;
    function movieMediaTypes: MPMovieMediaTypeMask; cdecl;
    function movieSourceType: MPMovieSourceType; cdecl;
    function naturalSize: CGSize; cdecl;
    function playableDuration: NSTimeInterval; cdecl;
    function playbackState: MPMoviePlaybackState; cdecl;
    function repeatMode: MPMovieRepeatMode; cdecl;
    procedure requestThumbnailImagesAtTimes( playbackTimes: NSArray; timeOption: MPMovieTimeOption ); cdecl;
    function scalingMode: MPMovieScalingMode; cdecl;
    procedure setAllowsAirPlay( allowsAirPlay: Boolean ); cdecl;
    procedure setBackgroundColor( backgroundColor: UIColor ); cdecl;
    procedure setContentURL( contentURL: NSURL ); cdecl;
    procedure setControlStyle( controlStyle: MPMovieControlStyle ); cdecl;
    procedure setEndPlaybackTime( endPlaybackTime: NSTimeInterval ); cdecl;
    procedure setFullscreen( fullscreen: Boolean ); cdecl; overload;
    procedure setFullscreen( fullscreen: Boolean; animated: Boolean ); cdecl; overload;
    procedure setInitialPlaybackTime( initialPlaybackTime: NSTimeInterval ); cdecl;

    procedure setMovieSourceType( movieSourceType: MPMovieSourceType ); cdecl;
    procedure setRepeatMode( repeatMode: MPMovieRepeatMode ); cdecl;
    procedure setScalingMode( scalingMode: MPMovieScalingMode ); cdecl;
    procedure setShouldAutoplay( shouldAutoplay: Boolean ); cdecl;
    function shouldAutoplay: Boolean; cdecl;
    function timedMetadata: NSArray; cdecl;
    function view: UIView; cdecl;

    // function movieControlMode: MPMovieControlMode; cdecl; // deprecated
    // procedure setUseApplicationAudioSession( useApplicationAudioSession: Boolean ); cdecl; // Deprecated in iOS 6.0
    // function useApplicationAudioSession: Boolean; cdecl; // Deprecated in iOS 6.0
    // function thumbnailImageAtTime( playbackTime: NSTimeInterval; timeOption: MPMovieTimeOption ): UIImage; cdecl; // Deprecated in iOS 7.0
  end;

  // ----------------------------------------------------------------------------
  MPMoviePlayerViewControllerClass = interface( UIViewControllerClass )
    ['{6F7D170C-7737-4E69-AEE2-20B437CA06DB}']
  end;

  MPMoviePlayerViewController = interface( UIViewController )
    ['{807B2EAD-3CA9-4AE2-B21A-482FFFC019E3}']
    function initWithContentURL( contentURL: NSURL ): Pointer; cdecl;
    function moviePlayer: MPMoviePlayerController; cdecl;
    // function shouldAutorotateToInterfaceOrientation( toInterfaceOrientation: UIInterfaceOrientation ): Boolean; cdecl; //  Available in iOS 3.2 through iOS 6.1
  end;

  TMPMoviePlayerViewController = class( TOCGenericImport<MPMoviePlayerViewControllerClass, MPMoviePlayerViewController> )
  end;

  // ------------------------------------------------------------------------------
  IMoviePlayerNotificationHandler = interface( NSObject )
    ['{AA17AFA5-A1BB-462E-9379-223748513DF3}']
    procedure doneClicked( notification: NSNotification ); cdecl;
    procedure stateDidChange; cdecl;
    procedure didEnterFullScreen( sender: pointer ); cdecl;
    procedure didExitFullScreen( sender: pointer ); cdecl;
  end;

  TMoviePlayerNotificationHandler = class( TOCLocal )
  private
    FDPFMPPlayer: TDPFMPMoviePlayerViewController;
  public
    function GetObjectiveCClass: PTypeInfo; override;
    constructor create( Owner: TDPFMPMoviePlayerViewController );

    procedure doneClicked( notification: NSNotification ); cdecl;
    procedure stateDidChange; cdecl;
    procedure didEnterFullScreen( sender: pointer ); cdecl;
    procedure didExitFullScreen( sender: pointer ); cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------

  TDPFMoviePlayerState  = ( psStopped = 0, psPlaying = 1, psPaused = 2, psInterrupted = 3, psForward = 4, psBackward = 5 );
  TDPFMoviePathType     = ( mptFileName, mptURL );
  TDPFMovieSourceType   = ( mstUnknown, mstFile, mstStreaming );
  TDPFMovieScalingMode  = ( msmNone = 0, msmAspectFit = 1, msmAspectFill = 2, msmFill = 3 );
  TDPFMovieControlStyle = ( csStyleNone = 0, csEmbedded = 1, csFullscreen = 2 );

  TDPFMoviePlayerOnError                = procedure( Sender: TObject; Error: string ) of object;
  TDPFMoviePlayerOnDidExitFullScreen    = procedure( Sender: TObject ) of object;
  TDPFMoviePlayerOnDidEnterFullScreen   = procedure( Sender: TObject ) of object;
  TDPFMoviePlayerOnUserExited           = procedure( Sender: TObject ) of object;
  TDPFMoviePlayerOnPlaybackEnded        = procedure( Sender: TObject; var CanClose: Boolean ) of object;
  TDPFMoviePlayerOnPlaybackStateChanged = procedure( Sender: TObject; PlaybackState: TDPFMoviePlayerState ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFMPMoviePlayerViewController = class( TComponent )
  private
{$IFDEF IOS}
    FMoviePlayer                   : MPMoviePlayerViewController;
    FMovieURL                      : NSURL;
    FMoviePlayerNotificationHandler: TMoviePlayerNotificationHandler;
{$ENDIF}
    FMoviePath             : string;
    FMoviePathType         : TDPFMoviePathType;
    FMovieSourceType       : TDPFMovieSourceType;
    FBackgroundColor       : TAlphaColor;
    FOnUserExited          : TDPFMoviePlayerOnUserExited;
    FOnError               : TDPFMoviePlayerOnError;
    FOnPlaybackEnded       : TDPFMoviePlayerOnPlaybackEnded;
    FMovieControlStyle     : TDPFMovieControlStyle;
    FShowInView            : TDPFUIView;
    FMovieScalingMode      : TDPFMovieScalingMode;
    FOnPlaybackStateChanged: TDPFMoviePlayerOnPlaybackStateChanged;
    FOnDidExitFullScreen   : TDPFMoviePlayerOnDidExitFullScreen;
    FOnDidEnterFullScreen  : TDPFMoviePlayerOnDidEnterFullScreen;
    procedure setBackgroundColor( const Value: TAlphaColor );
  protected
{$IFDEF IOS}
    procedure Loaded; override;
    procedure dealocatePlayer;
{$ELSE}
{$ENDIF}
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Close;
    procedure Play;
    procedure Stop;
    procedure Pause;
    function MoviePlayState: TDPFMoviePlayerState;
    procedure ViewControllerCompletion;
  published
    property MoviePath        : string read FMoviePath write FMoviePath;
    property MovieSourceType  : TDPFMovieSourceType read FMovieSourceType write FMovieSourceType default mstFile;
    property MoviePathType    : TDPFMoviePathType read FMoviePathType write FMoviePathType default mptFileName;
    property BackgroundColor  : TAlphaColor read FBackgroundColor write setBackgroundColor default TAlphaColors.Black;
    property MovieControlStyle: TDPFMovieControlStyle read FMovieControlStyle write FMovieControlStyle default TDPFMovieControlStyle.csFullscreen;
    property MovieScalingMode : TDPFMovieScalingMode read FMovieScalingMode write FMovieScalingMode default TDPFMovieScalingMode.msmNone;

    property ShowInView: TDPFUIView read FShowInView write FShowInView;

    property OnError               : TDPFMoviePlayerOnError read FOnError write FOnError;
    property OnUserExited          : TDPFMoviePlayerOnUserExited read FOnUserExited write FOnUserExited;
    property OnPlaybackEnded       : TDPFMoviePlayerOnPlaybackEnded read FOnPlaybackEnded write FOnPlaybackEnded;
    property OnPlaybackStateChanged: TDPFMoviePlayerOnPlaybackStateChanged read FOnPlaybackStateChanged write FOnPlaybackStateChanged;
    property OnDidExitFullScreen   : TDPFMoviePlayerOnDidExitFullScreen read FOnDidExitFullScreen write FOnDidExitFullScreen;
    property OnDidEnterFullScreen  : TDPFMoviePlayerOnDidEnterFullScreen read FOnDidEnterFullScreen write FOnDidEnterFullScreen;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFMPMoviePlayerViewController }
// ------------------------------------------------------------------------------
constructor TDPFMPMoviePlayerViewController.create( AOwner: TComponent );
begin
  inherited create( AOwner );
  FMoviePathType     := mptFileName;
  FMovieSourceType   := mstFile;
  FBackgroundColor   := TAlphaColors.Black;
  FMovieControlStyle := TDPFMovieControlStyle.csFullscreen;
  FMovieScalingMode  := TDPFMovieScalingMode.msmNone;
  FShowInView        := nil;
end;

// ------------------------------------------------------------------------------
destructor TDPFMPMoviePlayerViewController.Destroy;
begin
{$IFDEF IOS}
  dealocatePlayer;

  if Assigned( FMoviePlayerNotificationHandler ) then
    FMoviePlayerNotificationHandler.DisposeOf;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFMPMoviePlayerViewController.MoviePlayState: TDPFMoviePlayerState;
begin
  result := psStopped;
{$IFDEF IOS}
  if assigned( FMoviePlayer ) and assigned( FMoviePlayer.moviePlayer ) then
    result := TDPFMoviePlayerState( FMoviePlayer.moviePlayer.playbackState );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFMPMoviePlayerViewController.Loaded;
begin
  // ----------------------------
  // Important
  inherited;
end;

// ------------------------------------------------------------------------------
{ TIMoviePlayerNotificationHandler }
constructor TMoviePlayerNotificationHandler.create( Owner: TDPFMPMoviePlayerViewController );
begin
  inherited create;
  FDPFMPPlayer := Owner;
end;

// ------------------------------------------------------------------------------
function TMoviePlayerNotificationHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IMoviePlayerNotificationHandler );
end;

// ------------------------------------------------------------------------------
procedure TMoviePlayerNotificationHandler.stateDidChange; cdecl;
begin
  if Assigned( FDPFMPPlayer.FOnPlaybackStateChanged ) then
    FDPFMPPlayer.FOnPlaybackStateChanged( FDPFMPPlayer, TDPFMoviePlayerState( FDPFMPPlayer.FMoviePlayer.moviePlayer.playbackState ) );
end;

// ------------------------------------------------------------------------------
procedure TMoviePlayerNotificationHandler.didEnterFullScreen( sender: pointer ); cdecl;
begin
  if Assigned( FDPFMPPlayer.FOnDidEnterFullScreen ) then
    FDPFMPPlayer.FOnDidEnterFullScreen( FDPFMPPlayer );
end;

// ------------------------------------------------------------------------------
procedure TMoviePlayerNotificationHandler.didExitFullScreen( sender: pointer ); cdecl;
begin
  if Assigned( FDPFMPPlayer.FOnDidExitFullScreen ) then
    FDPFMPPlayer.FOnDidExitFullScreen( FDPFMPPlayer );
end;

// ------------------------------------------------------------------------------
procedure TMoviePlayerNotificationHandler.doneClicked( notification: NSNotification ); cdecl;
var
  Val             : NSNumber;
  CanClose        : Boolean;
  mediaPlayerError: NSError;
begin

  CanClose := true;
  Val      := TNSNumber.Wrap( notification.UserInfo.valueForKey( NSSTR( 'MPMoviePlayerPlaybackDidFinishReasonUserInfoKey' ) ) );

  if Val.intValue = MPMovieFinishReasonPlaybackEnded then
  begin
    // movie finished playin
    if Assigned( FDPFMPPlayer.FOnPlaybackEnded ) then
    begin
      FDPFMPPlayer.FOnPlaybackEnded( FDPFMPPlayer, CanClose );
      if CanClose then
        FDPFMPPlayer.dealocatePlayer;
    end;
  end
  else if ( Val.intValue = MPMovieFinishReasonUserExited ) or ( Val.intValue = MPMovieFinishReasonPlaybackError ) and ( FDPFMPPlayer <> nil ) then
  begin
    // user hit the done button
    if ( Val.intValue = MPMovieFinishReasonUserExited ) and Assigned( FDPFMPPlayer.FOnUserExited ) then
      FDPFMPPlayer.FOnUserExited( FDPFMPPlayer )
    else if ( Val.intValue = MPMovieFinishReasonPlaybackError ) and Assigned( FDPFMPPlayer.FOnError ) then
    begin
      mediaPlayerError := TNSError.Wrap( notification.UserInfo.valueForKey( NSSTR( 'error' ) ) );
      FDPFMPPlayer.FOnError( FDPFMPPlayer, UTF8ToString( mediaPlayerError.localizedDescription.UTF8String ) );
      FDPFMPPlayer.dealocatePlayer;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.dealocatePlayer;
begin
  if Assigned( FMoviePlayerNotificationHandler ) then
    TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FMoviePlayerNotificationHandler.GetObjectID );

  if Assigned( FMoviePlayer ) then
  begin
    if FMoviePlayer.moviePlayer <> nil then
    begin
      FMoviePlayer.moviePlayer.setFullscreen( false, true );
      FMoviePlayer.moviePlayer.setInitialPlaybackTime( -1 );
      FMoviePlayer.moviePlayer.stop;
      FMoviePlayer.moviePlayer.setContentURL( nil );
      if Assigned( FShowInView ) and assigned( FMoviePlayer.view ) then
        FMoviePlayer.view.removeFromSuperview;
    end;

    FMoviePlayer.release;
  end;
  FMoviePlayer := nil;

  if Assigned( FMovieURL ) then
    FMovieURL.release;
  FMovieURL := nil;
end;

{$ENDIF }

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.Pause;
begin
{$IFDEF IOS}
  FMoviePlayer.moviePlayer.pause;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
// Notifications:
//
// MPMovieDurationAvailableNotification
// MPMovieMediaTypesAvailableNotification
// MPMovieNaturalSizeAvailableNotification
// MPMoviePlayerContentPreloadDidFinishNotification
// MPMoviePlayerDidEnterFullscreenNotification
// MPMoviePlayerDidExitFullscreenNotification
// MPMoviePlayerIsAirPlayVideoActiveDidChangeNotification
// MPMoviePlayerLoadStateDidChangeNotification
// MPMoviePlayerNowPlayingMovieDidChangeNotification
// MPMoviePlayerPlaybackDidFinishNotification
// MPMoviePlayerPlaybackStateDidChangeNotification
// MPMoviePlayerScalingModeDidChangeNotification
// MPMoviePlayerThumbnailImageRequestDidFinishNotification
// MPMoviePlayerWillEnterFullscreenNotification
// MPMoviePlayerWillExitFullscreenNotification
// MPMovieSourceTypeAvailableNotification
// MPMoviePlayerReadyForDisplayDidChangeNotification
// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.Play;
{$IFDEF IOS}
var
  s: string;
{$ENDIF}
begin
{$IFDEF IOS}
  Close;
  s := FMoviePath;
  if FMoviePath <> '' then
  begin
    if FMoviePathType = mptFileName then
    begin
      if FMoviePath.Contains( 'file://' ) then
        FMovieURL := TNSURL.Wrap( TNSURL.Alloc.initWithString( NSStr( FMoviePath ) ) )
      else
        FMovieURL := TNSURL.Wrap( TNSURL.Alloc.initFileURLWithPath( NSStr( FMoviePath ) ) );
    end
    else
      FMovieURL := TNSUrl.Wrap( TNSUrl.Alloc.initWithString( NSSTR( FMoviePath ) ) );

    FMoviePlayer := TMPMoviePlayerViewController.Wrap( TMPMoviePlayerViewController.Alloc.initWithContentURL( FMovieURL ) );

    if FBackgroundColor <> TAlphaColors.Null then
    begin
      FMoviePlayer.moviePlayer.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );
      FMoviePlayer.view.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );
    end;

    if not Assigned( FMoviePlayerNotificationHandler ) then
      FMoviePlayerNotificationHandler := TMoviePlayerNotificationHandler.create( Self );

    TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FMoviePlayerNotificationHandler.GetObjectID, sel_getUid( 'doneClicked:' ), ( NSSTR( 'MPMoviePlayerPlaybackDidFinishNotification' ) as ILocalObject ).GetObjectID, nil );
    TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FMoviePlayerNotificationHandler.GetObjectID, sel_getUid( 'stateDidChange' ), ( NSSTR( 'MPMoviePlayerPlaybackStateDidChangeNotification' ) as ILocalObject ).GetObjectID, nil );
    TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FMoviePlayerNotificationHandler.GetObjectID, sel_getUid( 'didEnterFullScreen:' ), ( NSSTR( 'MPMoviePlayerDidEnterFullscreenNotification' ) as ILocalObject ).GetObjectID, nil );
    TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FMoviePlayerNotificationHandler.GetObjectID, sel_getUid( 'didExitFullScreen:' ), ( NSSTR( 'MPMoviePlayerDidExitFullscreenNotification' ) as ILocalObject ).GetObjectID, nil );

    FMoviePlayer.moviePlayer.setMovieSourceType( Integer( FMovieSourceType ) );
  end;

  if not Assigned( FMoviePlayer ) then
    exit;

  FMoviePlayer.view.setFrame( CGRectMake( 0, 0, screen.Size.Width, screen.Size.Height ) );
  FMoviePlayer.moviePlayer.setControlStyle( Integer( FMovieControlStyle ) );
  FMoviePlayer.moviePlayer.setScalingMode( Integer( FMovieScalingMode ) );

  if FMovieControlStyle = TDPFMovieControlStyle.csFullscreen then
  begin
    FMoviePlayer.moviePlayer.setFullscreen( true, true );
    FMoviePlayer.setWantsFullScreenLayout( true );
    GetSharedApplication.setStatusBarHidden( true );
    GetSharedApplication.setStatusBarStyle( UIStatusBarStyleBlackOpaque );
  end;

  if Assigned( FShowInView ) then
  begin
    // FShowInView.GetUIView.setAutoresizesSubviews( true );
    FShowInView.GetUIView.addSubview( FMoviePlayer.view );
    FMoviePlayer.view.setFrame( FShowInView.GetUIView.bounds );
  end
  else
  begin
    GetSharedApplication.keyWindow.rootViewController.presentViewController( FMoviePlayer, false, ViewControllerCompletion );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.Close;
begin
{$IFDEF IOS}
  dealocatePlayer;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.setBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FMoviePlayer <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FMoviePlayer.moviePlayer.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.Stop;
begin
{$IFDEF IOS}
  if FMoviePlayer <> nil then
  begin
    FMoviePlayer.moviePlayer.setInitialPlaybackTime( -1 );
    FMoviePlayer.moviePlayer.stop;
  end
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMPMoviePlayerViewController.ViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------
end.
