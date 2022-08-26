// ------------------------------------------------------------------------------
// DPF.iOS.GameKit Class
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

unit DPF.iOS.GameKit;

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

{$IFDEF IOS}

const
  GameKitFwk = '/System/Library/Frameworks/GameKit.framework/GameKit';

  // -------------------------
  // Game Kit Errors
  GKErrorUnknown                          = 1;
  GKErrorCancelled                        = 2;
  GKErrorCommunicationsFailure            = 3;
  GKErrorUserDenied                       = 4;
  GKErrorInvalidCredentials               = 5;
  GKErrorNotAuthenticated                 = 6;
  GKErrorAuthenticationInProgress         = 7;
  GKErrorInvalidPlayer                    = 8;
  GKErrorScoreNotSet                      = 9;
  GKErrorParentalControlsBlocked          = 10;
  GKErrorPlayerStatusExceedsMaximumLength = 11;
  GKErrorPlayerStatusInvalid              = 12;
  GKErrorMatchRequestInvalid              = 13;
  GKErrorUnderage                         = 14;
  GKErrorGameUnrecognized                 = 15;
  GKErrorNotSupported                     = 16;
  GKErrorInvalidParameter                 = 17;
  GKErrorUnexpectedConnection             = 18;
  GKErrorChallengeInvalid                 = 19;
  GKErrorTurnBasedMatchDataTooLarge       = 20;
  GKErrorTurnBasedTooManySessions         = 21;
  GKErrorTurnBasedInvalidParticipant      = 22;
  GKErrorTurnBasedInvalidTurn             = 23;
  GKErrorTurnBasedInvalidState            = 24;
  GKErrorInvitationsDisabled              = 26;

  // -----------------------------------------------------------
  // GKPeerPickerConnectionType
  // Network connections available to the peer picker dialog.
  GKPeerPickerConnectionTypeOnline = 1 shl 0;
  GKPeerPickerConnectionTypeNearby = 1 shl 1;

{$ENDIF}
{$IFDEF IOS}

type
  GKLocalPlayer                   = interface;
  GKMatch                         = interface;
  id                              = pointer;
  int64_t                         = Int64;
  uint32_t                        = UInt32;
  GKGameCenterControllerViewState = Integer;

  GKPhotoSize = ( GKPhotoSizeSmall = 0, GKPhotoSizeNormal = 1 );

  GKChallengeState = ( GKChallengeStateInvalid = 0, GKChallengeStatePending = 1, GKChallengeStateCompleted = 2, GKChallengeStateDeclined = 3 );

  GKLeaderboardTimeScope   = ( GKLeaderboardTimeScopeToday = 0, GKLeaderboardTimeScopeWeek = 1, GKLeaderboardTimeScopeAllTime = 2 );
  GKLeaderboardPlayerScope = ( GKLeaderboardPlayerScopeGlobal = 0, GKLeaderboardPlayerScopeFriendsOnly );

  GKMatchSendDataMode    = ( GKMatchSendDataReliable = 0, GKMatchSendDataUnreliable = 1 );
  GKVoiceChatPlayerState = ( GKVoiceChatPlayerConnected = 0, GKVoiceChatPlayerDisconnected = 1, GKVoiceChatPlayerSpeaking = 2, GKVoiceChatPlayerSilent = 3, GKVoiceChatPlayerConnecting = 4 );

  GKMatchType       = ( GKMatchTypePeerToPeer = 0, GKMatchTypeHosted = 1, GKMatchTypeTurnBased = 2 );
  GKInviteeResponse = ( GKInviteeResponseAccepted = 0, GKInviteeResponseDeclined = 1, GKInviteeResponseFailed = 2, GKInviteeResponseIncompatible = 3, GKInviteeResponseUnableToConnect = 4, GKInviteeResponseNoAnswer = 5 );

  GKPeerPickerConnectionType  = NSUInteger;
  GKPeerPickerConnectionType1 = word;

  GKSessionMode         = ( GKSessionModeServer = 0, GKSessionModeClient = 1, GKSessionModePeer = 2 );
  GKPeerConnectionState = ( GKPeerStateAvailable = 0, GKPeerStateUnavailable = 1, GKPeerStateConnected = 2, GKPeerStateDisconnected = 3, GKPeerStateConnecting = 4 );
  GKSendDataMode        = ( GKSendDataReliable = 0, GKSendDataUnreliable = 1 );

  // ------------------------------------------------------------------------------

  TcompletionHandler = procedure of object;

  TGKAchievementDescriptionLoadAchievementDescriptionsCompletionHandler = procedure( image: UIImage; error: NSError ) of object;
  TLoadAchievementDescriptionsCompletionHandler = procedure( descriptions: NSArray; error: NSError ) of object;
  TGKMatchmakerInviteeResponseHandler = procedure( playerID: NSString; response: GKInviteeResponse ) of object;
  TGKMatchmakerBrowsingForNearbyPlayersWithReachableHandler = procedure( playerID: NSString; reachable: Boolean ) of object;
  TGKMatchmakerQueryPlayerGroupActivityCompletionHandler = procedure( activity: NSInteger; error: NSError ) of object;
  TGKMatchmakerCompletionHandler                         = procedure( match: GKMatch; error: NSError ) of object;
  TGKMatchmakerRequestCompletionHandler                  = procedure( match: GKMatch; error: NSError ) of object;
  TGKMatchmakerRequestsCompletionHandler                 = procedure( playerIDs: NSArray; error: NSError ) of object;
  GKVoiceChatPlayerStateUpdateHandler                    = procedure( playerID: NSString; state: GKVoiceChatPlayerState ) of object;
  TGKMatchRematchCompletionHandler                       = procedure( match: GKMatch; error: NSError ) of object;
  TGKMatchCompletionHandler                              = procedure( playerID: NSString ) of object;
  TCommonCompletionHandler                               = procedure( error: NSError ) of object;
  TReportCompletionHandler                               = procedure( error: NSError ) of object;
  TGKAchievementsChallengeablePlayerIDsCompletionHandler = procedure( challengeablePlayerIDs: NSArray; error: NSError ) of object;

  TGKLeaderboardLoadScoresCompletionHandler           = procedure( scores: NSArray; error: NSError ) of object;
  TGKLeaderboardLoadImageCompletionHandler            = procedure( image: UIImage; error: NSError ) of object;
  TGKLeaderboardLoadLeaderboardsCompletionHandler     = procedure( leaderboards: NSArray; error: NSError ) of object;
  TGKChallengeLoadReceivedChallengesCompletionHandler = procedure( challenges: NSArray; error: NSError ) of object;

  TGKPlayerCompletionHandler          = procedure( players: NSArray; error: NSError ) of object;
  TGKAchievementsCompletionHandler    = procedure( achievements: NSArray; error: NSError ) of object;
  TGKPlayerLoadPhotoCompletionHandler = procedure( photo: UIImage; error: NSError ) of object;

  TGKChallengeComposeCompletionBlock = procedure( composeController: UIViewController; didIssueChallenge: Boolean; sentPlayerIDs: NSArray ) of object;

  GKPlayerClass = interface( NSObjectClass )
    ['{1CDC14B3-3EEC-4DA6-BF5E-055D54029BD0}']
    procedure loadPlayersForIdentifiers( identifiers: NSArray; withCompletionHandler: TGKPlayerCompletionHandler ); cdecl;
  end;

  GKPlayer = interface( NSObject )
    ['{4F9B7E4F-7EFF-4C5A-A902-8E27C6FA1596}']

    function playerID: NSString; cdecl;

    function alias: NSString; cdecl;
    function displayName: NSString; cdecl;
    function isFriend: Boolean; cdecl;
    procedure loadPhotoForSize( size: GKPhotoSize; withCompletionHandler: TGKPlayerLoadPhotoCompletionHandler ); cdecl;
  end;

  TGKPlayer = class( TOCGenericImport<GKPlayerClass, GKPlayer> )
  end;

  // ----------------------------------------------------------------------------
  TGKLocalPlayerGKChallengeComposeCompletionBlock = procedure( composeController: UIViewController; didIssueChallenge: Boolean; sentPlayerIDs: NSArray ) of object;

  GKScoreClass = interface( NSObjectClass )
    ['{4D2CE113-30FF-413F-81F1-5A16630087DC}']
    procedure reportScores( scores: NSArray; completionHandler: TReportCompletionHandler ); cdecl; overload;
    procedure reportScores( scores: NSArray; challenges: NSArray; completionHandler: TReportCompletionHandler ); cdecl; overload;

  end;

  GKScore = interface( NSObject )
    ['{EFE445C2-FF78-4EBA-8F71-4058A9F92012}']

    function initWithCategory( category: NSString ): id; cdecl; // Deprecated in iOS 7.0

    function playerID: NSString; cdecl;
    function date: NSDate; cdecl;

    function value: int64_t; cdecl;
    procedure setValue( value: int64_t ); cdecl;

    function context: uint64_t; cdecl;
    procedure setContext( context: uint64_t ); cdecl;

    function formattedValue: NSString; cdecl;
    function rank: NSInteger; cdecl;
    function category: NSString; cdecl; // Deprecated in iOS 7.0
    function shouldSetDefaultLeaderboard: Boolean; cdecl;
    function challengeComposeControllerWithPlayers( playerIDs: NSArray; message: NSString; completionHandler: TGKLocalPlayerGKChallengeComposeCompletionBlock ): UIViewController; cdecl;
    procedure issueChallengeToPlayers( playerIDs: NSArray; message: NSString ); cdecl; // Deprecated in iOS 7.0

    function initWithLeaderboardIdentifier( identifier: NSString ): id; cdecl; overload; // Available in iOS 7.0 and later
    function initWithLeaderboardIdentifier( identifier: NSString; playerID: NSString ): id; cdecl; overload; // Available in iOS 7.0 and later
    function leaderboardIdentifier: NSString; cdecl; // Available in iOS 7.0 and later
    procedure setLeaderboardIdentifier( leaderboardIdentifier: NSString ); cdecl; // Available in iOS 7.0 and later

    // Deprecated in iOS 7.0
    procedure reportScoreWithCompletionHandler( completionHandler: TReportCompletionHandler ); cdecl; // Deprecated in iOS 7.0

  end;

  TGKScore = class( TOCGenericImport<GKScoreClass, GKScore> )
  end;

  // ------------------------------------------------------------------------------
  // GKAchievement
  // ------------------------------------------------------------------------------
  GKAchievementClass = interface( NSObjectClass )
    ['{2C55689F-025F-4792-8D07-60D0EA23EF86}']

    procedure loadAchievementsWithCompletionHandler( completionHandler: TGKAchievementsCompletionHandler ); cdecl;
    procedure reportAchievements( achievements: NSArray; completionHandler: TReportCompletionHandler ); cdecl; overload;
    procedure reportAchievements( achievements: NSArray; challenges: NSArray; completionHandler: TReportCompletionHandler ); cdecl; overload;

    procedure resetAchievementsWithCompletionHandler( completionHandler: TReportCompletionHandler ); cdecl;
  end;

  GKAchievement = interface( NSObject )
    ['{92A5C7F5-5560-4137-8501-86D4C0615359}']

    function initWithIdentifier( identifier: NSString ): id; cdecl; overload;
    function initWithIdentifier( identifier: NSString; playerID: NSString ): id; cdecl; overload;

    function identifier: NSString; cdecl;
    procedure setIdentifier( identifier: NSString ); cdecl;

    function percentComplete: Double; cdecl;
    procedure setPercentComplete( percentComplete: Double ); cdecl;

    function isCompleted: Boolean; cdecl;
    function lastReportedDate: NSData; cdecl;

    function isHidden: Boolean; cdecl; // (Deprecated in iOS 6.0
    function playerID: NSString; cdecl;
    function showsCompletionBanner: Boolean; cdecl;
    procedure setShowsCompletionBanner( showsCompletionBanner: Boolean ); cdecl;
    procedure reportAchievementWithCompletionHandler( completionHandler: TReportCompletionHandler ); cdecl; // (Deprecated in iOS 7.0
    function challengeComposeControllerWithPlayers( playerIDs: NSArray; &message: NSString; completionHandler: TGKChallengeComposeCompletionBlock ): UIViewController; cdecl;
    procedure selectChallengeablePlayerIDs( playerIDs: NSArray; completionHandler: TGKAchievementsChallengeablePlayerIDsCompletionHandler ); cdecl;
    procedure issueChallengeToPlayers( playerIDs: NSArray; &message: NSString ); cdecl; // Deprecated in iOS 7.0
  end;

  TGKAchievement = class( TOCGenericImport<GKAchievementClass, GKAchievement> )
  end;

  // ------------------------------------------------------------------------------
  // GKAchievementDescription
  // ------------------------------------------------------------------------------
  GKAchievementDescriptionClass = interface( NSObjectClass )
    ['{F33DDED4-98C6-4B59-8165-88A8DE1DF890}']

    function incompleteAchievementImage: UIImage; cdecl;
    function placeholderCompletedAchievementImage: UIImage; cdecl;
    procedure loadAchievementDescriptionsWithCompletionHandler( completionHandler: TLoadAchievementDescriptionsCompletionHandler ); cdecl;
  end;

  GKAchievementDescription = interface( NSObject )
    ['{B675628E-9143-4272-9CD6-671BD161E08A}']

    function identifier: NSString; cdecl;            // Readonly
    function title: NSString; cdecl;                 // Readonly
    function unachievedDescription: NSString; cdecl; // Readonly
    function achievedDescription: NSString; cdecl;   // Readonly
    function maximumPoints: NSInteger; cdecl;        // Readonly
    function isHidden: Boolean; cdecl;               // Readonly
    function isReplayable: Boolean; cdecl;           // Readonly
    function image: UIImage; cdecl;                  // Readonly - Deprecated in iOS 7.0.

    procedure loadImageWithCompletionHandler( completionHandler: TGKAchievementDescriptionLoadAchievementDescriptionsCompletionHandler ); cdecl;
  end;

  TGKAchievementDescription = class( TOCGenericImport<GKAchievementDescriptionClass, GKAchievementDescription> )
  end;

  // ------------------------------------------------------------------------------
  // GKTurnBasedMatch
  // ------------------------------------------------------------------------------
  GKTurnBasedMatchClass = interface( NSObjectClass )
    ['{D2E710AB-0DF1-4B0B-A8E7-BFA5F3113BB3}']

  end;

  GKTurnBasedMatch = interface( NSObject )
    ['{CDB2443E-499E-4150-A988-1AF35FCDF19E}']

  end;

  TGKTurnBasedMatch = class( TOCGenericImport<GKTurnBasedMatchClass, GKTurnBasedMatch> )
  end;

  // ---------------------------------------------------------------------------
  GKTurnBasedMatch2 = interface( GKTurnBasedMatch )
  end;

  // ---------------------------------------------------------------------------
  // GKLeaderboard
  // ---------------------------------------------------------------------------
  GKLeaderboardClass = interface( NSObjectClass )
    ['{6A18BC5B-502C-47FF-B86D-CAD542AD12C1}']

    procedure loadLeaderboardsWithCompletionHandler( completionHandler: TGKLeaderboardLoadLeaderboardsCompletionHandler ); cdecl;
  end;

  GKLeaderboard = interface( NSObject )
    ['{F7C2E136-8A4C-435F-B9DB-66B96DD254D7}']

    function init: id; cdecl;
    function initWithPlayerIDs( playerIDs: NSArray ): id; cdecl;
    function playerScope: GKLeaderboardPlayerScope; cdecl;
    function range: NSRange; cdecl;
    function timeScope: GKLeaderboardTimeScope; cdecl;
    procedure setTimeScope( timeScope: GKLeaderboardTimeScope ); cdecl;
    function category: NSString; cdecl;                 // Deprecated in iOS 7.0
    procedure setCategory( category: NSString ); cdecl; // Deprecated in iOS 7.0
    function identifier: NSString; cdecl;               // Available in iOS 7.0 and later.
    procedure setIdentifier( identifier: NSString ); cdecl; // Available in iOS 7.0 and later.
    procedure loadImageWithCompletionHandler( completionHandler: TGKLeaderboardLoadImageCompletionHandler ); cdecl; // Available in iOS 7.0 and later.
    procedure loadScoresWithCompletionHandler( completionHandler: TGKLeaderboardLoadScoresCompletionHandler ); cdecl;
    function isLoading: boolean; cdecl;        // A Boolean value that indicates whether the leaderboard object is retrieving scores
    function title: NSString; cdecl;           // If you initialized a new leaderboard object, this property is invalid until a call to loadScoresWithCompletionHandler: is complete. Afterward, it contains the localized title for the leaderboard identified by the category property.
    function scores: NSArray; cdecl;           // This property is invalid until a call to loadScoresWithCompletionHandler: is complete. Afterward, it contains the same score objects that were returned to the completion handler
    function localPlayerScore: GKScore; cdecl; // This property is invalid until a call to loadScoresWithCompletionHandler: is completed. Afterward, it contains a score object representing the local player’s score on the leaderboard given the filters you applied to the query.
    function maxRange: NSUInteger; cdecl;      // The size of the leaderboard. (read-only). This property is invalid until a call to loadScoresWithCompletionHandler: is completed. Afterward, it contains the total number of entries available to return to your game given the filters you applied to the query.
    function groupIdentifier: NSString; cdecl; // Available in iOS 6.0 and later.
  end;

  TGKLeaderboard = class( TOCGenericImport<GKLeaderboardClass, GKLeaderboard> )
  end;

  // ---------------------------------------------------------------------------
  // GKChallenge
  // Available in iOS 6.0 and later.
  // ---------------------------------------------------------------------------
  GKChallengeClass = interface( NSObjectClass )
    ['{5534060B-78A8-4743-B8E3-4E2BD9BA51B9}']

    procedure loadReceivedChallengesWithCompletionHandler( completionHandler: TGKChallengeLoadReceivedChallengesCompletionHandler ); cdecl;
  end;

  GKChallenge = interface( NSObject )
    ['{787E423A-06A8-4F0E-B266-7326E62625F4}']

    function issueDate: NSDate; cdecl;
    function issuingPlayerID: NSString; cdecl;
    function receivingPlayerID: NSString; cdecl;
    function &message: NSString; cdecl;
    function state: GKChallengeState; cdecl;
    function completionDate: NSDate; cdecl;

    procedure decline; cdecl;

  end;

  TGKChallenge = class( TOCGenericImport<GKChallengeClass, GKChallenge> )
  end;

  // ---------------------------------------------------------------------------
  // GKVoiceChat
  // ---------------------------------------------------------------------------
  GKVoiceChatClass = interface( NSObjectClass )
    ['{72CE481D-9574-4BC3-9448-59DE2A965B54}']

    function isVoIPAllowed: Boolean; cdecl;
  end;

  GKVoiceChat = interface( NSObject )
    ['{7D989241-B946-4F0C-ADE3-259F75DBECB4}']

    procedure start; cdecl;
    procedure stop; cdecl;
    function isActive: Boolean; cdecl;
    procedure setPlayerStateUpdateHandler( playerStateUpdateHandler: GKVoiceChatPlayerStateUpdateHandler ); cdecl;
    procedure setMute( isMuted: Boolean; player: NSString ); cdecl;
    function volume: single; cdecl;
    procedure setVolume( volume: single ); cdecl;
    function name: NSString; cdecl;
    function playerIDs: NSArray; cdecl;
  end;

  TGKVoiceChat = class( TOCGenericImport<GKVoiceChatClass, GKVoiceChat> )
  end;

  // ---------------------------------------------------------------------------
  // GKInvite
  // ---------------------------------------------------------------------------
  GKInviteClass = interface( NSObjectClass )
    ['{EA3E2C99-0805-4A3F-9996-99D57A72CD0A}']

  end;

  GKInvite = interface( NSObject )
    ['{03849BC6-C748-46F5-B96E-2A9331AC82F9}']

    function isHosted: Boolean; cdecl;
    function inviter: NSString; cdecl;
    function playerAttributes: uint32_t; cdecl; // Available in iOS 6.0 and later
    function playerGroup: NSUInteger; cdecl;    // Available in iOS 6.0 and later
  end;

  TGKInvite = class( TOCGenericImport<GKInviteClass, GKInvite> )
  end;

  // ---------------------------------------------------------------------------
  // GKPeerPickerController
  // Deprecated in iOS 7.0. Use MultipeerConnectivity.framework instead.
  // ---------------------------------------------------------------------------
  GKPeerPickerControllerClass = interface( NSObjectClass )
    ['{E0934CFC-1195-4006-8D6D-C2C5BACFE491}']

  end;

  GKPeerPickerController = interface( NSObject )
    ['{D1B9580E-AE74-4A38-B178-142A1812D1FA}']

    function delegate: id; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
    procedure show; cdecl;
    procedure dismiss; cdecl;
    function isVisible: Boolean; cdecl;

    function connectionTypesMask: GKPeerPickerConnectionType; cdecl;
    procedure setConnectionTypesMask( connectionTypesMask: GKPeerPickerConnectionType ); cdecl;
  end;

  TGKPeerPickerController = class( TOCGenericImport<GKPeerPickerControllerClass, GKPeerPickerController> )
  end;

  // ---------------------------------------------------------------------------
  // GKSession
  // Deprecated in iOS 7.0. Use MultipeerConnectivity.framework instead.
  // ---------------------------------------------------------------------------
  GKSessionClass = interface( NSObjectClass )
    ['{A16232D8-76BB-4821-BA38-FEC382ACBEC3}']

  end;

  GKSession = interface( NSObject )
    ['{91ACD81D-5A74-469C-B4B1-B861774E7C0E}']

    function initWithSessionID( sessionID: NSString; displayName: NSString; sessionMode: GKSessionMode ): id; cdecl;
    function delegate: id; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
    function isAvailable: Boolean; cdecl;
    function peersWithConnectionState( state: GKPeerConnectionState ): NSArray; cdecl;
    function displayNameForPeer( peerID: NSString ): NSString; cdecl;
    procedure connectToPeer( peerID: NSString; withTimeout: NSTimeInterval ); cdecl;
    procedure cancelConnectToPeer( peerID: NSString ); cdecl;
    function acceptConnectionFromPeer( peerID: NSString; error: PNSError ): Boolean; cdecl;
    procedure denyConnectionFromPeer( peerID: NSString ); cdecl;
    procedure setDataReceiveHandler( handler: id; withContext: id ); cdecl;
    function sendData( data: NSData; toPeers: NSArray; withDataMode: GKSendDataMode; error: PNSError ): Boolean; cdecl;
    function sendDataToAllPeers( data: NSData; withDataMode: GKSendDataMode; error: PNSError ): Boolean; cdecl;
    function disconnectTimeout: NSTimeInterval; cdecl;
    procedure setDisconnectTimeout( disconnectTimeout: NSTimeInterval ); cdecl;
    procedure disconnectPeerFromAllPeers( peerID: NSString ); cdecl;
    procedure disconnectFromAllPeers; cdecl;

    function displayName: NSString; cdecl; // Read Only
    function peerID: NSString;             // Read Only
    function sessionID: NSString;          // Read Only
    function sessionMode: GKSessionMode;   // Read Only
  end;

  TGKSession = class( TOCGenericImport<GKSessionClass, GKSession> )
  end;

  // ---------------------------------------------------------------------------
  // GKMatchRequest
  // ---------------------------------------------------------------------------
  GKMatchRequestClass = interface( NSObjectClass )
    ['{FE02973F-0FC6-4219-96F4-F65B73C19183}']

    function maxPlayersAllowedForMatchOfType( matchType: GKMatchType ): NSUInteger; cdecl;
  end;

  GKMatchRequest = interface( NSObject )
    ['{34F573D3-F987-4B26-9F6B-8FDA528D1935}']

    function maxPlayers: NSUInteger; cdecl;
    procedure setMaxPlayers( maxPlayers: NSUInteger ); cdecl;

    function minPlayers: NSUInteger; cdecl;
    procedure setMinPlayers( minPlayers: NSUInteger ); cdecl;

    function defaultNumberOfPlayers: NSUInteger; cdecl;
    procedure setDefaultNumberOfPlayers( defaultNumberOfPlayers: NSUInteger ); cdecl;

    function inviteMessage: NSString; cdecl;
    procedure setInviteMessage( inviteMessage: NSString ); cdecl;

    function playerGroup: NSUInteger; cdecl;
    procedure setPlayerGroup( playerGroup: NSUInteger ); cdecl;

    function playerAttributes: uint32_t; cdecl;
    procedure setPlayerAttributes( playerAttributes: uint32_t ); cdecl;

    function playersToInvite: NSArray; cdecl;
    procedure setPlayersToInvite( playersToInvite: NSArray ); cdecl;

    procedure setInviteeResponseHandler( inviteeResponseHandler: TGKMatchmakerInviteeResponseHandler ); cdecl;
  end;

  TGKMatchRequest = class( TOCGenericImport<GKMatchRequestClass, GKMatchRequest> )
  end;

  // ---------------------------------------------------------------------------
  // GKMatchmaker
  // ---------------------------------------------------------------------------
  GKMatchmakerClass = interface( NSObjectClass )
    ['{139E3F4F-DB6B-403A-B307-27AE872D810C}']

    function sharedMatchmaker: id; cdecl;
  end;

  GKMatchmaker = interface( NSObject )
    ['{A0A54585-0C9B-46D0-8513-137B83C224C2}']

    procedure matchForInvite( invite: GKInvite; completionHandler: TGKMatchmakerCompletionHandler ); cdecl;
    procedure findMatchForRequest( request: GKMatchRequest; completionHandler: TGKMatchmakerRequestCompletionHandler ); cdecl;
    procedure findPlayersForHostedMatchRequest( request: GKMatchRequest; completionHandler: TGKMatchmakerRequestsCompletionHandler ); cdecl;
    procedure addPlayersToMatch( match: GKMatch; matchRequest: GKMatchRequest; completionHandler: TCommonCompletionHandler ); cdecl;
    procedure finishMatchmakingForMatch( match: GKMatch ); cdecl;
    procedure cancel; cdecl;
    procedure cancelInviteToPlayer( playerID: NSString ); cdecl;
    procedure queryPlayerGroupActivity( playerGroup: NSUInteger; completionHandler: TGKMatchmakerQueryPlayerGroupActivityCompletionHandler ); cdecl;
    procedure queryActivityWithCompletionHandler( completionHandler: TGKMatchmakerQueryPlayerGroupActivityCompletionHandler ); cdecl;
    procedure startBrowsingForNearbyPlayersWithReachableHandler( reachableHandler: TGKMatchmakerBrowsingForNearbyPlayersWithReachableHandler ); cdecl;
  end;

  TGKMatchmaker = class( TOCGenericImport<GKMatchmakerClass, GKMatchmaker> )
  end;

  // ---------------------------------------------------------------------------
  // GKMatchmakerViewController
  // ---------------------------------------------------------------------------
  GKMatchmakerViewControllerClass = interface( UINavigationControllerClass )
    ['{1CC0930A-A88C-4CBA-9760-ABC4C9266EC2}']

  end;

  GKMatchmakerViewController = interface( UINavigationController )
    ['{98833C88-A5C9-461B-ACFC-57B652C379C5}']

    function initWithInvite( invite: GKInvite ): id; cdecl;
    function initWithMatchRequest( request: GKMatchRequest ): id; cdecl;

    function matchmakerDelegate: id; cdecl;
    procedure setMatchmakerDelegate( matchmakerDelegate: id ); cdecl;

    function isHosted: id; cdecl;
    function matchRequest: GKMatchRequest; cdecl;

    procedure addPlayersToMatch( match: GKMatch ); cdecl;
    procedure setHostedPlayer( playerID: NSString; connected: Boolean ); cdecl;
  end;

  TGKMatchmakerViewController = class( TOCGenericImport<GKMatchmakerViewControllerClass, GKMatchmakerViewController> )
  end;

  // ---------------------------------------------------------------------------
  // GKMatch
  // ---------------------------------------------------------------------------
  GKMatchClass = interface( NSObjectClass )
    ['{EAB50C72-307B-4BAD-B020-C8285BFDE8BC}']

  end;

  GKMatch = interface( NSObject )
    ['{A1B8F20D-BA60-439E-BD9B-6CDC449D8820}']

    function delegate: id; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
    function playerIDs: NSArray; cdecl;
    function expectedPlayerCount: NSUInteger; cdecl;
    function sendData( data: NSData; playerIDs: NSArray; mode: GKMatchSendDataMode; error: PNSError ): Boolean; cdecl;
    function sendDataToAllPlayers( data: NSData; mode: GKMatchSendDataMode; error: PNSError ): Boolean; cdecl;
    procedure chooseBestHostPlayerWithCompletionHandler( completionHandler: TGKMatchCompletionHandler ); cdecl; // Available in iOS 6.0 and later
    function voiceChatWithName( name: NSString ): GKVoiceChat; cdecl;
    procedure disconnect; cdecl;
    procedure rematchWithCompletionHandler( completionHandler: TGKMatchRematchCompletionHandler ); cdecl; // Available in iOS 6.0 and later
  end;

  TGKMatch = class( TOCGenericImport<GKMatchClass, GKMatch> )
  end;

  // ---------------------------------------------------------------------------
  // GKNotificationBanner
  // The GKNotificationBanner class allows your game to display a notification banner that displays text to the player. The behavior of this banner is identical to other banners used by Game Kit.
  // Important: Your game must authenticate a local player before you can use any Game Center classes. If there is no authenticated player, your game receives a GKErrorNotAuthenticated error. For more information on authentication, see Game Center Programming Guide.
  // ---------------------------------------------------------------------------
  GKNotificationBannerClass = interface( NSObjectClass )
    ['{A593C95F-4141-4125-88EA-A37C4F086368}']

    procedure showBannerWithTitle( title: NSString; &message: NSString; completionHandler: TCompletionHandler ); cdecl; overload;
    procedure showBannerWithTitle( title: NSString; &message: NSString; duration: NSTimeInterval; completionHandler: TCompletionHandler ); cdecl; overload;
  end;

  GKNotificationBanner = interface( NSObject )
    ['{080E2B8C-F6AC-4029-A8C4-09CC6CEE3B75}']

  end;

  TGKNotificationBanner = class( TOCGenericImport<GKNotificationBannerClass, GKNotificationBanner> )
  end;

  // ---------------------------------------------------------------------------
  // Available in iOS 6.0 and later.
  // ---------------------------------------------------------------------------
  GKScoreChallengeClass = interface( GKChallengeClass )
    ['{F72B82FD-47E7-4C0A-85C8-2147D463DE2D}']
  end;

  GKScoreChallenge = interface( GKChallenge )
    ['{787E423A-06A8-4F0E-B266-7326E62625F4}']
    function score: GKScore; cdecl;
  end;

  TGKScoreChallenge = class( TOCGenericImport<GKScoreChallengeClass, GKScoreChallenge> )
  end;

  // ---------------------------------------------------------------------------
  // GKAchievementChallenge
  // Available in iOS 6.0 and later.
  // ---------------------------------------------------------------------------
  GKAchievementChallengeClass = interface( GKChallengeClass )
    ['{98EF788C-BBF4-4F66-9F72-D2C449CFBB3E}']
  end;

  GKAchievementChallenge = interface( GKChallenge )
    ['{68D0B88B-9B9B-40BA-B7FA-6E491A878778}']
    function achievement: GKAchievement; cdecl;
  end;

  TGKAchievementChallenge = class( TOCGenericImport<GKAchievementChallengeClass, GKAchievementChallenge> )
  end;

  // ------------------------------------------------------------------------------

  TGKLocalPlayerGenerateIdentityCompletionHandler       = procedure( publicKeyUrl: NSURL; signature: NSData; salt: NSData; timestamp: uint64_t; error: NSError ) of object;
  TGKLocalPlayerLoadFriendsCompletionHandler            = procedure( players: NSArray; error: NSError ) of object;
  TGKLocalPlayerLoadDefaultLeaderboardCompletionHandler = procedure( leaderboardIdentifier: NSString; error: NSError ) of object;
  TGKLocalPlayerSetDefaultLeaderboardCompletionHandler  = procedure( error: NSError ) of object;
  TGKLocalPlayerAuthenticateHandler                     = procedure( viewController: UIViewController; error: NSError ) of object;

  GKLocalPlayerClass = interface( GKPlayerClass )
    ['{F051209B-83AD-4808-8E72-81D76742F738}']
    function localPlayer: id { GKLocalPlayer }; cdecl;
  end;

  GKLocalPlayer = interface( GKPlayer )
    ['{5150248E-A1FC-46FE-897D-EACC5C9022CA}']

    procedure setAuthenticateHandler( authenticateHandler: TGKLocalPlayerAuthenticateHandler ); cdecl;
    function authenticateHandler: TGKLocalPlayerAuthenticateHandler; cdecl;
    function isAuthenticated: Boolean; cdecl;

    procedure authenticateWithCompletionHandler( completionHandler: TCommonCompletionHandler ); cdecl;

    procedure generateIdentityVerificationSignatureWithCompletionHandler( completionHandler: TGKLocalPlayerGenerateIdentityCompletionHandler ); cdecl; // Available in iOS 7.0 and later.
    procedure loadFriendsWithCompletionHandler( completionHandler: TGKLocalPlayerLoadFriendsCompletionHandler ); cdecl; // Available in iOS 7.0 and later.
    function friends: NSArray; cdecl;
    function underage: Boolean; cdecl;
    procedure loadDefaultLeaderboardIdentifierWithCompletionHandler( completionHandler: TGKLocalPlayerLoadDefaultLeaderboardCompletionHandler ); cdecl;
    procedure setDefaultLeaderboardIdentifier( leaderboardIdentifier: NSString; completionHandler: TGKLocalPlayerSetDefaultLeaderboardCompletionHandler ); cdecl;
    procedure registerListener( listener: id ); cdecl;   // Available in iOS 7.0 and later.
    procedure unregisterAllListeners; cdecl;             // Available in iOS 7.0 and later.
    procedure unregisterListener( listener: id ); cdecl; // Available in iOS 7.0 and later.
  end;

  TGKLocalPlayer = class( TOCGenericImport<GKLocalPlayerClass, GKLocalPlayer> )
  end;

  // ----------------------------------------------------------------------------
  // View Controllers
  // ----------------------------------------------------------------------------
  GKGameCenterViewControllerClass = interface( UINavigationControllerClass )
    ['{F3E6AA49-F5F2-4025-9180-0A7DDE45BF4F}']
  end;

  GKGameCenterViewController = interface( UINavigationController )
    ['{1856D0F5-FB20-4A50-945F-2DB01DCBD11D}']

    function gameCenterDelegate: id; cdecl;
    procedure setGameCenterDelegate( gameCenterDelegate: id ); cdecl;

    function viewState: GKGameCenterControllerViewState; cdecl;

    function leaderboardCategory: NSString; cdecl; // Deprecated in iOS 7.0
    function leaderboardTimeScope: GKLeaderboardTimeScope; cdecl; // Deprecated in iOS 7.0

  end;

  TGKGameCenterViewController = class( TOCGenericImport<GKGameCenterViewControllerClass, GKGameCenterViewController> )
  end;

  // ----------------------------------------------------------------------------
  GKLeaderboardViewControllerClass = interface( GKGameCenterViewControllerClass )
    ['{0E772FC5-BFFC-4F5F-9950-88D0ACAE8935}']
  end;

  GKLeaderboardViewController = interface( GKGameCenterViewController )
    ['{398EF0D0-FA65-4A3C-A595-34718D3EF22B}']

    function category: NSString; cdecl;
    procedure setCategory( category: NSString ); cdecl;

    function leaderboardDelegate: id; cdecl;
    procedure setLeaderboardDelegate( leaderboardDelegate: id ); cdecl;

    function timeScope: GKLeaderboardTimeScope; cdecl;
    procedure setTimeScope( timeScope: GKLeaderboardTimeScope ); cdecl;

  end;

  TGKLeaderboardViewController = class( TOCGenericImport<GKLeaderboardViewControllerClass, GKLeaderboardViewController> )
  end;

  // ----------------------------------------------------------------------------
  // GKAchievementViewController
  // ----------------------------------------------------------------------------
  GKAchievementViewControllerClass = interface( GKGameCenterViewControllerClass )
    ['{61125E80-4FE9-4039-AD14-F098CEC0E781}']
  end;

  GKAchievementViewController = interface( GKGameCenterViewController )
    ['{A02DC8D2-39DE-41BB-ADA5-94CE7251EAA9}']

    function achievementDelegate: id; cdecl;
    procedure setAchievementDelegate( achievementDelegate: id ); cdecl;

  end;

  TGKAchievementViewController = class( TOCGenericImport<GKAchievementViewControllerClass, GKAchievementViewController> )
  end;

  // ----------------------------------------------------------------------------
  // GKTurnBasedMatchmakerViewController
  // The GKTurnBasedMatchmakerViewController class displays a user interface that allows players to manage the turn-based matches that they are participating in.
  // Important: Your game must authenticate a local player before you can use any Game Center classes. If there is no authenticated player, your game receives a GKErrorNotAuthenticated error. For more information on authentication see Game Center Programming Guide.
  // ----------------------------------------------------------------------------
  GKTurnBasedMatchmakerViewControllerClass = interface( UINavigationControllerClass )
    ['{264B8086-B7CB-41F6-8DA0-CD301CAF2082}']
  end;

  GKTurnBasedMatchmakerViewController = interface( UINavigationController )
    ['{1CBF47FF-6E50-4263-9458-48E93ACC7962}']

    function initWithMatchRequest( request: GKMatchRequest ): id; cdecl;
    function turnBasedMatchmakerDelegate: id; cdecl;
    procedure setTurnBasedMatchmakerDelegate( turnBasedMatchmakerDelegate: id ); cdecl;
    function showExistingMatches: boolean; cdecl;
    procedure setShowExistingMatches( showExistingMatches: boolean ); cdecl;
  end;

  TGKTurnBasedMatchmakerViewController = class( TOCGenericImport<GKTurnBasedMatchmakerViewControllerClass, GKTurnBasedMatchmakerViewController> )
  end;

  // ----------------------------------------------------------------------------
  // GKFriendRequestComposeViewController
  // ----------------------------------------------------------------------------
  GKFriendRequestComposeViewControllerClass = interface( UINavigationControllerClass )
    ['{0938C8BF-6DD6-4B2B-8FDC-F6D41AAF5EE7}']

    function maxNumberOfRecipients: NSUInteger; cdecl;
  end;

  GKFriendRequestComposeViewController = interface( GKGameCenterViewController )
    ['{0EF49927-A1A8-496B-876F-F998C8BBC6BE}']

    procedure setComposeViewDelegate( composeViewDelegate: id ); cdecl;
    procedure addRecipientsWithEmailAddresses( emailAddresses: NSArray ); cdecl;
    procedure addRecipientsWithPlayerIDs( playerIDs: NSArray ); cdecl;
    procedure setMessage( &message: NSString ); cdecl;
  end;

  TGKFriendRequestComposeViewController = class( TOCGenericImport<GKFriendRequestComposeViewControllerClass, GKFriendRequestComposeViewController> )
  end;

  // ----------------------------------------------------------------------------
  // Delegates
  // ----------------------------------------------------------------------------
  GKFriendRequestComposeViewControllerDelegate = interface( IObjectiveC )
    ['{A9683618-C36C-4D01-9E3B-6FDE8E1FDCC2}']

    procedure friendRequestComposeViewControllerDidFinish( viewController: GKFriendRequestComposeViewController ); cdecl;
  end;

  GKTurnBasedMatchmakerViewControllerDelegate = interface( IObjectiveC )
    ['{0913D85F-0109-4A4F-A5DC-029E275C755E}']

    procedure turnBasedMatchmakerViewController( viewController: GKTurnBasedMatchmakerViewController; didFindMatch: GKTurnBasedMatch ); cdecl; overload;
    procedure turnBasedMatchmakerViewController( viewController: GKTurnBasedMatchmakerViewController; playerQuitForMatch: GKTurnBasedMatch2 ); cdecl; overload;
    procedure turnBasedMatchmakerViewController( viewController: GKTurnBasedMatchmakerViewController; didFailWithError: NSError ); cdecl; overload;
    procedure turnBasedMatchmakerViewControllerWasCancelled( viewController: GKTurnBasedMatchmakerViewController ); cdecl;
  end;

  GKLeaderboardViewControllerDelegate = interface( IObjectiveC )
    ['{AECEA52C-C717-48D8-AFF1-2391C19F6057}']

    procedure leaderboardViewControllerDidFinish( viewController: GKLeaderboardViewController ); cdecl;
  end;

  GKAchievementViewControllerDelegate = interface( IObjectiveC )
    ['{ABDECC20-5F2D-472E-8B76-BCD222204EA6}']

    procedure achievementViewControllerDidFinish( viewController: GKAchievementViewController ); cdecl;
  end;

  GKGameCenterControllerDelegate = interface( IObjectiveC )
    ['{8DA4EC2B-DF09-49ED-8EB6-C5F5DDF7F9FA}']

    procedure gameCenterViewControllerDidFinish( gameCenterViewController: GKGameCenterViewController ); cdecl;
  end;

  GKMatchmakerViewControllerDelegate = interface( IObjectiveC )
    ['{7E4E9D81-87D5-44B8-9D73-D70900278A30}']

    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFindMatch: GKMatch ); cdecl; overload;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFindPlayers: NSArray ); cdecl; overload;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didReceiveAcceptFromHostedPlayer: NSString ); cdecl; overload;
    procedure matchmakerViewController( viewController: GKMatchmakerViewController; didFailWithError: NSError ); cdecl; overload;

    procedure matchmakerViewControllerWasCancelled( viewController: GKMatchmakerViewController ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  GKPeerPickerControllerDelegate = interface( IObjectiveC )
    ['{EE1823EA-090A-4428-A4A3-8A37FC0DD2E2}']

    procedure peerPickerController( picker: GKPeerPickerController; didSelectConnectionType: GKPeerPickerConnectionType ); cdecl; overload;
    function peerPickerController( picker: GKPeerPickerController; sessionForConnectionType: GKPeerPickerConnectionType1 ): GKSession; cdecl; overload;
    procedure peerPickerController( picker: GKPeerPickerController; didConnectPeer: NSString; toSession: GKSession ); cdecl; overload;
    procedure peerPickerControllerDidCancel( picker: GKPeerPickerController ); cdecl;
  end;

  // ----------------------------------------------------------------------------

{$ENDIF}

  // ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  GameKitModule: THandle;
{$ENDIF}
{$ENDIF}
  // ------------------------------------------------------------------------------
{$IFDEF IOS}
procedure GameCenterFaceLoader; cdecl; external GameKitFwk;
{$IF defined(CPUARM)}
{$ELSE}

initialization

GameKitModule := dlopen( MarshaledAString( GameKitFwk ), RTLD_LAZY );

finalization

dlclose( GameKitModule );
{$ENDIF}
{$ENDIF}

end.
