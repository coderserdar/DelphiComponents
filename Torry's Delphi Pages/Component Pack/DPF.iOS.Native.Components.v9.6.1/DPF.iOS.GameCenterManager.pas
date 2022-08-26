// ------------------------------------------------------------------------------
// DPF.iOS.GameCenterManager Component
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

unit DPF.iOS.GameCenterManager;

interface

{$I DPF.iOS.Defs.inc}

uses
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  Macapi.Mach,
  iOSapi.UIKit,
  DPF.iOS.Common,
  DPF.iOS.UIView,
  DPF.iOS.GameKit,
{$ENDIF}
  DPF.iOS.UIViewController,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,
  System.Math,
  FMX.Dialogs;

// ------------------------------------------------------------------------------

type

  TDPFGameCenterManager = class;
{$IFDEF IOS}
  // ------------------------------------------------------------------------------

  TGKLeaderboardViewControllerDelegate = class( TOCLocal, GKLeaderboardViewControllerDelegate )
  private
    FDPFGameCenterManager: TDPFGameCenterManager;
  public
    constructor Create( ADPFGameCenterManager: TDPFGameCenterManager );
    procedure leaderboardViewControllerDidFinish( viewController: GKLeaderboardViewController ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TGKAchievementViewControllerDelegate = class( TOCLocal, GKAchievementViewControllerDelegate )
  private
    FDPFGameCenterManager: TDPFGameCenterManager;
  public
    constructor Create( ADPFGameCenterManager: TDPFGameCenterManager );
    procedure achievementViewControllerDidFinish( viewController: GKAchievementViewController ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TGKGameCenterControllerDelegate = class( TOCLocal, GKGameCenterControllerDelegate )
  private
    FDPFGameCenterManager: TDPFGameCenterManager;
  public
    constructor Create( ADPFGameCenterManager: TDPFGameCenterManager );
    procedure gameCenterViewControllerDidFinish( gameCenterViewController: GKGameCenterViewController ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TGKMatchmakerViewControllerDelegate = class( TOCLocal, GKMatchmakerViewControllerDelegate )
  private
    FDPFGameCenterManager: TDPFGameCenterManager;
  public
    constructor Create( ADPFGameCenterManager: TDPFGameCenterManager );

    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFindMatch: GKMatch ); overload; cdecl;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFindPlayers: NSArray ); overload; cdecl;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didReceiveAcceptFromHostedPlayer: NSString ); overload; cdecl;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFailWithError: NSError ); overload; cdecl;

    procedure matchmakerViewControllerWasCancelled( viewController: GKMatchmakerViewController ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  TGKFriendRequestComposeViewControllerDelegate = class( TOCLocal, GKFriendRequestComposeViewControllerDelegate )
  private
    FDPFGameCenterManager: TDPFGameCenterManager;
  public
    constructor Create( ADPFGameCenterManager: TDPFGameCenterManager );

    procedure friendRequestComposeViewControllerDidFinish( viewController: GKFriendRequestComposeViewController ); cdecl;
  end;
  // ----------------------------------------------------------------------------

{$ENDIF}

  TScore = record
    PlayerID: string;
    Value: Int64;
    Rank: Int64;
    ScoreDate: TDateTime;
  end;

  TScores = array of TScore;

  TAchievement = record
    PlayerID: string;
    Identifier: string;
    percentComplete: Single;
    isHidden: Boolean;
    isCompleted: Boolean;
  end;

  TAchievements = array of TAchievement;

  TDPFOnReportScrore      = procedure( Sender: TObject; const Submitted: Boolean; const Error: string; const ErrorCode: NativeUInt ) of object;
  TDPFOnReportAchievement = procedure( Sender: TObject; const Submitted: Boolean; const Error: string; const ErrorCode: NativeUInt ) of object;
  TDPFOnAuthenticate      = procedure( Sender: TObject; const isAuthenticated: Boolean; const error: string; const ErrorCode: NativeUInt ) of object;
  TDPFOnGetScores         = procedure( Sender: TObject; const Scores: TScores; const error: string; const ErrorCode: NativeUInt ) of object;
  TDPFOnGetAchievements   = procedure( Sender: TObject; const Achievements: TAchievements; const error: string; const ErrorCode: NativeUInt ) of object;

  // ----------------------------------------------------------------------------
  // TDPFGameCenterManager Component
  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFGameCenterManager = class( TComponent )
  private
    FViewController     : TDPFUIViewController;
    FOnReportScrore     : TDPFOnReportScrore;
    FOnReportAchievement: TDPFOnReportAchievement;
    FOnAuthenticate     : TDPFOnAuthenticate;
    FOnGetScores        : TDPFOnGetScores;
    FOnGetAchievements  : TDPFOnGetAchievements;
{$IFDEF IOS}
    FGKAchievementViewControllerDelegate         : TGKAchievementViewControllerDelegate;
    FGKLeaderboardViewControllerDelegate         : TGKLeaderboardViewControllerDelegate;
    FGKGameCenterControllerDelegate              : TGKGameCenterControllerDelegate;
    FGKMatchmakerViewControllerDelegate          : TGKMatchmakerViewControllerDelegate;
    FGKFriendRequestComposeViewControllerDelegate: TGKFriendRequestComposeViewControllerDelegate;
    // ---------------------------

    procedure ReportScoresCompletionHandler( error: NSError );
    procedure ReportAchievementCompletionHandler( error: NSError );
    procedure LoadScoresWithCompletionHandler( scores: NSArray; error: NSError );
    procedure LoadAchievementsWithCompletionHandler( achievements: NSArray; error: NSError );
    procedure OnUIViewControllerCompletion;
{$ENDIF}
  protected

    procedure ShowAchievements( ID: string );
    procedure ShowLeaderBoard( ID: string );
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function ReportScore( ID: string; highScore: Int64 ): Boolean;
    function ReportAchievement( const ID: string; const percentComplete: single; const ShowsCompletionBanner: Boolean ): Boolean;

    procedure ShowFriendRequest;
    procedure ShowMatchmaker;
    procedure ShowGameCenter( ID: string );
    function InitPlayer: boolean;
    procedure RequestScores( ID: string );
    procedure RequestAchievements;

{$IFDEF IOS}
    procedure AuthenticateHandlerComp( viewController: UIViewController; error: NSError );
    procedure CommonAuthenticateHandlerComp( error: NSError );
{$ENDIF}
  published
    property ViewController     : TDPFUIViewController read FViewController write FViewController;
    property OnReportScrore     : TDPFOnReportScrore read FOnReportScrore write FOnReportScrore;
    property OnReportAchievement: TDPFOnReportAchievement read FOnReportAchievement write FOnReportAchievement;
    property OnAuthenticate     : TDPFOnAuthenticate read FOnAuthenticate write FOnAuthenticate;
    property OnGetScores        : TDPFOnGetScores read FOnGetScores write FOnGetScores;
    property OnGetAchievements  : TDPFOnGetAchievements read FOnGetAchievements write FOnGetAchievements;
  end;

  // ----------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFGameCenterManager }
constructor TDPFGameCenterManager.Create( AOwner: TComponent );
begin
  inherited;
{$IFDEF IOS}
  FGKAchievementViewControllerDelegate := nil;
  FGKLeaderboardViewControllerDelegate := nil;
  FGKGameCenterControllerDelegate      := nil;
  FGKMatchmakerViewControllerDelegate  := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFGameCenterManager.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FGKLeaderboardViewControllerDelegate ) then
    FGKLeaderboardViewControllerDelegate.DisposeOf;

  if Assigned( FGKAchievementViewControllerDelegate ) then
    FGKAchievementViewControllerDelegate.DisposeOf;

  if Assigned( FGKGameCenterControllerDelegate ) then
    FGKGameCenterControllerDelegate.DisposeOf;

  if Assigned( FGKMatchmakerViewControllerDelegate ) then
    FGKMatchmakerViewControllerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ReportScoresCompletionHandler( error: NSError );
var
  Err      : string;
  ErrCode  : NativeUInt;
  Submitted: Boolean;
begin

  if Assigned( FOnReportScrore ) then
  begin
    Submitted := true;
    ErrCode   := 0;
    if assigned( error ) then
    begin
      Submitted := false;
      Err       := UTF8ToString( error.localizedDescription.UTF8String );
      ErrCode   := error.code;
    end;
    FOnReportScrore( self, Submitted, Err, ErrCode );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ReportAchievementCompletionHandler( error: NSError );
var
  Err      : string;
  ErrorCode: NativeUInt;
  Submitted: Boolean;
begin
  if Assigned( FOnReportAchievement ) then
  begin
    Submitted := true;
    ErrorCode := 0;
    if assigned( error ) then
    begin
      Submitted := false;
      Err       := UTF8ToString( error.localizedDescription.UTF8String );
      ErrorCode := error.code;
    end;
    FOnReportAchievement( self, Submitted, Err, ErrorCode );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.CommonAuthenticateHandlerComp( error: NSError );
begin
  AuthenticateHandlerComp( nil, error );
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.AuthenticateHandlerComp( viewController: UIViewController; error: NSError );
var
  er     : string;
  ErrCode: NativeUInt;
begin
  ErrCode := 0;
  if assigned( error ) then
  begin
    er      := NSStrToStr( error.localizedDescription );
    ErrCode := error.code;
  end
  else if assigned( viewController ) then
  begin
    if Assigned( FViewController ) then
      FViewController.FUIViewController.presentViewController( viewController, false, OnUIViewControllerCompletion )
    else
      GetSharedApplication.keyWindow.rootViewController.presentViewController( viewController, false, OnUIViewControllerCompletion )
  end;
  if assigned( FOnAuthenticate ) then
    FOnAuthenticate( self, TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer ).isAuthenticated, er, ErrCode );

end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.LoadScoresWithCompletionHandler( scores: NSArray; error: NSError );
var
  ErrStr : string;
  ErrCode: NativeUInt;
  I      : Integer;
  _Scores: TScores;
  Sc     : GKScore;
begin
  ErrStr  := '';
  ErrCode := 0;

  if assigned( scores ) then
    for I := 0 to scores.count - 1 do
    begin
      Sc := TGKScore.Wrap( scores.objectAtIndex( I ) );
      SetLength( _Scores, Length( _Scores ) + 1 );
      _Scores[I].PlayerID  := UTF8ToString( Sc.playerID.UTF8String );
      _Scores[I].Value     := Sc.value;
      _Scores[I].Rank      := Sc.rank;
      _Scores[I].ScoreDate := NSDateToDateTime( Sc.date );
    end
  else if error <> nil then
  begin
    if error.localizedDescription <> nil then
      ErrStr := NSStrToStr( error.localizedDescription );
    ErrCode  := error.code;
  end;

  if Assigned( FOnGetScores ) then
    FOnGetScores( Self, _Scores, ErrStr, ErrCode );

end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.LoadAchievementsWithCompletionHandler( achievements: NSArray; error: NSError );
var
  ErrStr       : string;
  ErrCode      : NativeUInt;
  I            : Integer;
  _Achievements: TAchievements;
  Ach          : GKAchievement;
begin
  ErrStr  := '';
  ErrCode := 0;

  if assigned( achievements ) then
    for I := 0 to achievements.count - 1 do
    begin
      Ach := TGKAchievement.Wrap( achievements.objectAtIndex( I ) );
      SetLength( _Achievements, Length( _Achievements ) + 1 );
      _Achievements[I].PlayerID        := UTF8ToString( Ach.playerID.UTF8String );
      _Achievements[I].percentComplete := Ach.percentComplete;
      _Achievements[I].isHidden        := Ach.isHidden;
      _Achievements[I].isCompleted     := Ach.isCompleted;
      _Achievements[I].Identifier      := UTF8ToString( Ach.identifier.UTF8String );
    end
  else if error <> nil then
  begin
    if error.localizedDescription <> nil then
      ErrStr := NSStrToStr( error.localizedDescription );
    ErrCode  := error.code;
  end;

  if Assigned( FOnGetAchievements ) then
    FOnGetAchievements( Self, _Achievements, ErrStr, ErrCode );

end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.RequestScores( ID: string );
{$IFNDEF IOS}
begin

end;
{$ELSE}

var
  FGKLeaderboard: GKLeaderboard;
begin
  if InitPlayer then
  begin
    FGKLeaderboard := TGKLeaderboard.Wrap( TGKLeaderboard.Alloc.init );
    FGKLeaderboard.setTimeScope( GKLeaderboardTimeScopeAllTime );
    if TOSVersion.Major > 6 then
      FGKLeaderboard.setIdentifier( NSSTR( ID ) )
    else
      FGKLeaderboard.setCategory( NSSTR( ID ) );
    FGKLeaderboard.loadScoresWithCompletionHandler( LoadScoresWithCompletionHandler );
    FGKLeaderboard.release;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.RequestAchievements;
{$IFNDEF IOS}
begin
end;
{$ELSE}

begin
  if InitPlayer then
  begin
    TGKAchievement.OCClass.loadAchievementsWithCompletionHandler( LoadAchievementsWithCompletionHandler );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFGameCenterManager.InitPlayer: Boolean;
{$IFNDEF IOS}
begin
  result := false;
end;
{$ELSE}

var
  LPlayer: GKLocalPlayer;
begin
  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  result  := LPlayer.isAuthenticated;
  if not result then
    try
      LPlayer.authenticateWithCompletionHandler( CommonAuthenticateHandlerComp );
    except
      LPlayer.setAuthenticateHandler( AuthenticateHandlerComp );
    end;
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.OnUIViewControllerCompletion;
begin

end;

{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFGameCenterManager.ReportScore( ID: string; highScore: Int64 ): Boolean;
{$IFNDEF IOS}
begin
  result := false;
end;
{$ELSE}

var
  score: GKScore;
begin
  result := false;
  if TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer ).isAuthenticated then
  begin
    score := TGKScore.Wrap( TGKScore.Alloc.initWithLeaderboardIdentifier( NSStr( ID ) ) );
    score.setValue( highScore );
    score.setContext( 0 );
    score.reportScoreWithCompletionHandler( ReportScoresCompletionHandler );
    score.release;
    result := true;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFGameCenterManager.ReportAchievement( const ID: string; const percentComplete: single; const ShowsCompletionBanner: Boolean ): Boolean;
{$IFNDEF IOS}
begin
  result := false;
end;
{$ELSE}

var
  achievement: GKAchievement;
begin
  result := false;
  if TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer ).isAuthenticated then
  begin
    achievement := TGKAchievement.Wrap( TGKAchievement.alloc.initWithIdentifier( NSStr( ID ) ) );
    if assigned( achievement ) then
    begin
      achievement.setPercentComplete( percentComplete );
      achievement.setShowsCompletionBanner( ShowsCompletionBanner );
      achievement.reportAchievementWithCompletionHandler( ReportAchievementCompletionHandler );
      achievement.release;
      result := true;
    end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ShowLeaderBoard( ID: string );
{$IFDEF IOS}
var
  LPlayer                   : GKLocalPlayer;
  FLeaderboardViewController: GKLeaderboardViewController;
{$ENDIF}
begin
{$IFDEF IOS}
  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  if not LPlayer.isAuthenticated then
    InitPlayer
  else
  begin
    if not assigned( FLeaderboardViewController ) then
    begin
      if not Assigned( FGKLeaderboardViewControllerDelegate ) then
        FGKLeaderboardViewControllerDelegate := TGKLeaderboardViewControllerDelegate.Create( self );

      FLeaderboardViewController := TGKLeaderboardViewController.Wrap( TGKLeaderboardViewController.Alloc.init );
      FLeaderboardViewController.setLeaderboardDelegate( FGKLeaderboardViewControllerDelegate.GetObjectID );
    end;
    FLeaderboardViewController.setCategory( NSStr( ID ) );
    FLeaderboardViewController.setTimeScope( GKLeaderboardTimeScopeAllTime );

    if Assigned( FViewController ) then
      FViewController.FUIViewController.presentViewController( FLeaderboardViewController, false, OnUIViewControllerCompletion )
    else
      GetSharedApplication.keyWindow.rootViewController.presentViewController( FLeaderboardViewController, false, OnUIViewControllerCompletion );
    FLeaderboardViewController.release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ShowMatchmaker;
{$IFNDEF IOS}
begin

end;
{$ELSE}

var
  request: GKMatchRequest;
  mmvc   : GKMatchmakerViewController;
  LPlayer: GKLocalPlayer;
begin
  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  if not LPlayer.isAuthenticated then
    InitPlayer
  else
  begin
    request := TGKMatchRequest.Wrap( TGKMatchRequest.alloc.init );
    request.setMinPlayers( 2 );
    request.setMaxPlayers( 2 );

    mmvc := TGKMatchmakerViewController.Wrap( TGKMatchmakerViewController.alloc.initWithMatchRequest( request ) );

    if FGKMatchmakerViewControllerDelegate = nil then
      FGKMatchmakerViewControllerDelegate := TGKMatchmakerViewControllerDelegate.Create( self );

    mmvc.setMatchmakerDelegate( FGKMatchmakerViewControllerDelegate.GetObjectID );

    if assigned( viewController ) then
    begin
      if Assigned( FViewController ) then
        FViewController.FUIViewController.presentViewController( mmvc, false, OnUIViewControllerCompletion )
      else
        GetSharedApplication.keyWindow.rootViewController.presentViewController( mmvc, false, OnUIViewControllerCompletion )
    end;
    mmvc.release;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ShowFriendRequest;
{$IFNDEF IOS}
begin

end;
{$ELSE}

var
  LPlayer: GKLocalPlayer;
  FRCV   : GKFriendRequestComposeViewController;
begin
  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  if not LPlayer.isAuthenticated then
    InitPlayer
  else
  begin
    FRCV := TGKFriendRequestComposeViewController.Wrap( TGKFriendRequestComposeViewController.alloc.init );

    if FGKFriendRequestComposeViewControllerDelegate = nil then
      FGKFriendRequestComposeViewControllerDelegate := TGKFriendRequestComposeViewControllerDelegate.Create( self );

    FRCV.setComposeViewDelegate( FGKFriendRequestComposeViewControllerDelegate.GetObjectID );

    if assigned( viewController ) then
    begin
      if Assigned( FViewController ) then
        FViewController.FUIViewController.presentViewController( FRCV, false, OnUIViewControllerCompletion )
      else
        GetSharedApplication.keyWindow.rootViewController.presentViewController( FRCV, false, OnUIViewControllerCompletion )
    end;
    FRCV.release;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ShowGameCenter( ID: string );
{$IFDEF IOS}
var
  LPlayer                    : GKLocalPlayer;
  FGKGameCenterViewController: GKGameCenterViewController;
{$ENDIF}
begin
{$IFDEF IOS}
  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  if not LPlayer.isAuthenticated then
    InitPlayer
  else
  begin
    if not assigned( FGKGameCenterViewController ) then
    begin
      if FGKGameCenterControllerDelegate = nil then
        FGKGameCenterControllerDelegate := TGKGameCenterControllerDelegate.Create( Self );

      FGKGameCenterViewController := TGKGameCenterViewController.Wrap( TGKGameCenterViewController.Alloc.init );
      FGKGameCenterViewController.setGameCenterDelegate( FGKGameCenterControllerDelegate.GetObjectID );
    end;

    if Assigned( FViewController ) then
      FViewController.FUIViewController.presentViewController( FGKGameCenterViewController, false, OnUIViewControllerCompletion )
    else
      GetSharedApplication.keyWindow.rootViewController.presentViewController( FGKGameCenterViewController, false, OnUIViewControllerCompletion );
    FGKGameCenterViewController.release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFGameCenterManager.ShowAchievements( ID: string );
{$IFDEF IOS}
var
  LPlayer                     : GKLocalPlayer;
  FGKAchievementViewController: GKAchievementViewController;

{$ENDIF}
begin
{$IFDEF IOS}
  if not assigned( FGKAchievementViewController ) then
  begin
    FGKAchievementViewControllerDelegate := TGKAchievementViewControllerDelegate.Create( Self );
    FGKAchievementViewController         := TGKAchievementViewController.Wrap( TGKAchievementViewController.Alloc.init );
    FGKAchievementViewController.setAchievementDelegate( FGKAchievementViewControllerDelegate );
  end;

  LPlayer := TGKLocalPlayer.Wrap( TGKLocalPlayer.OCClass.localPlayer );
  if not LPlayer.isAuthenticated then
    InitPlayer
  else
  begin
    if Assigned( FViewController ) then
      FViewController.FUIViewController.presentViewController( FGKAchievementViewController, false, OnUIViewControllerCompletion )
    else
      GetSharedApplication.keyWindow.rootViewController.presentViewController( FGKAchievementViewController, false, OnUIViewControllerCompletion );
  end;
  FGKAchievementViewController.release;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
// TGKLeaderboardViewControllerDelegate
// ------------------------------------------------------------------------------
constructor TGKLeaderboardViewControllerDelegate.Create( ADPFGameCenterManager: TDPFGameCenterManager );
begin
  inherited create;
  FDPFGameCenterManager := ADPFGameCenterManager;
end;

// ------------------------------------------------------------------------------
procedure TGKLeaderboardViewControllerDelegate.leaderboardViewControllerDidFinish( viewController: GKLeaderboardViewController ); cdecl;
begin
  viewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;

// ------------------------------------------------------------------------------
// TGKAchievementViewControllerDelegate
// ------------------------------------------------------------------------------
constructor TGKAchievementViewControllerDelegate.Create( ADPFGameCenterManager: TDPFGameCenterManager );
begin
  inherited create;
  FDPFGameCenterManager := ADPFGameCenterManager;
end;

// ------------------------------------------------------------------------------
procedure TGKAchievementViewControllerDelegate.achievementViewControllerDidFinish( viewController: GKAchievementViewController ); cdecl;
begin
  viewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;

// ------------------------------------------------------------------------------
// TGKGameCenterControllerDelegate
// ------------------------------------------------------------------------------
constructor TGKGameCenterControllerDelegate.Create( ADPFGameCenterManager: TDPFGameCenterManager );
begin
  inherited create;
  FDPFGameCenterManager := ADPFGameCenterManager;
end;

// ------------------------------------------------------------------------------
procedure TGKGameCenterControllerDelegate.gameCenterViewControllerDidFinish( gameCenterViewController: GKGameCenterViewController ); cdecl;
begin
  gameCenterViewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// TGKMatchmakerViewControllerDelegate
// ------------------------------------------------------------------------------
constructor TGKMatchmakerViewControllerDelegate.Create( ADPFGameCenterManager: TDPFGameCenterManager );
begin
  inherited create;
  FDPFGameCenterManager := ADPFGameCenterManager;
end;

// ------------------------------------------------------------------------------
procedure TGKMatchmakerViewControllerDelegate.matchmakerViewController( viewController: GKMatchmakerViewController; didFindMatch: GKMatch ); cdecl;
begin
  viewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;

// ------------------------------------------------------------------------------
procedure TGKMatchmakerViewControllerDelegate.matchmakerViewController( viewController: GKMatchmakerViewController; didFindPlayers: NSArray ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TGKMatchmakerViewControllerDelegate.matchmakerViewController( viewController: GKMatchmakerViewController; didReceiveAcceptFromHostedPlayer: NSString ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TGKMatchmakerViewControllerDelegate.matchmakerViewController( viewController: GKMatchmakerViewController; didFailWithError: NSError ); cdecl;
begin
end;

// ------------------------------------------------------------------------------

procedure TGKMatchmakerViewControllerDelegate.matchmakerViewControllerWasCancelled( viewController: GKMatchmakerViewController ); cdecl;
begin
  viewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;

// ------------------------------------------------------------------------------
// TGKFriendRequestComposeViewControllerDelegate
// ------------------------------------------------------------------------------
constructor TGKFriendRequestComposeViewControllerDelegate.Create( ADPFGameCenterManager: TDPFGameCenterManager );
begin
  inherited create;
  FDPFGameCenterManager := ADPFGameCenterManager;
end;

// ------------------------------------------------------------------------------
procedure TGKFriendRequestComposeViewControllerDelegate.friendRequestComposeViewControllerDidFinish( viewController: GKFriendRequestComposeViewController ); cdecl;
begin
  viewController.dismissViewControllerAnimated( true, FDPFGameCenterManager.OnUIViewControllerCompletion );
end;

// ------------------------------------------------------------------------------

{$ENDIF}
// ------------------------------------------------------------------------------

end.
