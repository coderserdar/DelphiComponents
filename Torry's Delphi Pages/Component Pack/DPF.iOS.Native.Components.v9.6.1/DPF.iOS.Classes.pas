// ------------------------------------------------------------------------------
// DPF.iOS.Classes Wrapped Classes & Interfaces
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
unit DPF.iOS.Classes;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
{$IFDEF IOS}
  iOSapi.UIKit,
  iOSapi.AVFoundation,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.QuartzCore,
  iOSapi.CoreLocation,
  iOSapi.MediaPlayer,

  Macapi.ObjCRuntime, Macapi.CoreFoundation,
  iOSapi.CoreImage, iOSapi.CoreData,
{$ENDIF}
  DPF.iOS.Common,
  System.TypInfo;

const
  libUIKit        = '/System/Library/Frameworks/UIKit.framework/UIKit';
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  libMediaPlayer  = '/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer';

{$IFDEF IOS}

const
  UIKeyboardTypeDefault               = 0;
  UIKeyboardAppearanceDefault         = 0;
  UIKeyboardAppearanceAlert           = 1;
  UIKeyboardTypeASCIICapable          = 1;
  UIKeyboardTypeAlphabet              = 1;
  UIKeyboardTypeNumbersAndPunctuation = 2;
  UIKeyboardTypeURL                   = 3;
  UIKeyboardTypeNumberPad             = 4;
  UIKeyboardTypePhonePad              = 5;
  UIKeyboardTypeNamePhonePad          = 6;
  UIKeyboardTypeEmailAddress          = 7;
  UIKeyboardTypeDecimalPad            = 8;
  UIKeyboardTypeTwitter               = 9;

  kAudioSessionCategory_AmbientSound     = 'ambi';
  kAudioSessionCategory_SoloAmbientSound = 'solo';
  kAudioSessionCategory_MediaPlayback    = 'medi';
  kAudioSessionCategory_RecordAudio      = 'reca';
  kAudioSessionCategory_PlayAndRecord    = 'plar';
  kAudioSessionCategory_AudioProcessing  = 'proc';
  kAudioSessionProperty_AudioCategory    = 'acat';

type
{$M+}
  AudioSessionPropertyID = packed record
    case UINT32 of
      1:
        ( Int: UINT32 );
      2:
        ( B1, B2, B3, B4: byte );
      3:
        ( W1, W2: Word );
      4:
        ( Str: array [0 .. 3] of char );
  end;

  // ------------------------------------------------------------------------------
  NSIndexPathClass = interface( NSObjectClass )
    ['{3685CD9B-3E64-4772-A837-02D510F19502}']
    function indexPathWithIndex( index: NSUInteger ): Pointer; cdecl;
    function indexPathWithIndexes( indexes: NSUInteger; length: NSUInteger ): Pointer; cdecl;
    function indexPathForRow( row: NSUInteger; inSection: NSUInteger ): Pointer; cdecl;
    function indexPathForItem( item: NSUInteger; inSection: NSUInteger ): Pointer; cdecl;
  end;

  NSIndexPath = interface( NSObject )
    ['{880E1727-D116-4DD7-B92E-84B71399C424}']
    function compare( otherObject: NSIndexPath ): NSComparisonResult; cdecl;
    procedure getIndexes( indexes: NSUInteger ); cdecl;
    function indexAtPosition( position: NSUInteger ): NSUInteger; cdecl;
    function indexPathByAddingIndex( index: NSUInteger ): NSIndexPath; cdecl;
    function indexPathByRemovingLastIndex: NSIndexPath; cdecl;
    function initWithIndex( index: NSUInteger ): Pointer; cdecl;
    function initWithIndexes( indexes: NSUInteger; length: NSUInteger ): Pointer; cdecl;
    function length: NSUInteger; cdecl;

    function section: NSInteger; cdecl;
    function row: NSInteger; cdecl;
    function item: NSInteger; cdecl;
  end;

  TNSIndexPath = class( TOCGenericImport<NSIndexPathClass, NSIndexPath> )
  end;

  // ------------------------------------------------------------------------------
  NSNotificationCenterClass = interface( NSObjectClass )
    ['{99D3FE5A-696A-4506-B006-8A007B64F1DC}']
    function defaultCenter: Pointer; cdecl;
  end;

  NSNotificationCenter = interface( NSObject )
    ['{372504A0-65EB-4249-A313-A601CB509253}']
    procedure postNotification( notification: NSNotification ); cdecl;
    procedure removeObserver( observer: Pointer ); cdecl; overload;
    procedure removeObserver( observer: Pointer; name: Pointer; &object: Pointer ); cdecl; overload;
    procedure addObserver( notificationObserver: Pointer; selector: SEL; name: Pointer; &object: Pointer ); cdecl;
    procedure postNotificationName( notificationName: Pointer; &object: Pointer ); cdecl;
  end;

  TNSNotificationCenter = class( TOCGenericImport<NSNotificationCenterClass, NSNotificationCenter> )
  end;

  // ------------------------------------------------------------------------------
  UIEventClass = interface( NSObjectClass )
    ['{880C5B3F-C073-42B1-8C9D-8A6B70945125}']
  end;

  UIEvent = interface( NSObject )
    ['{EEA6BDEF-5C24-4E2A-A1C7-9206864D0B2F}']
    function allTouches: NSSet; cdecl;
    function &type: UIEventtype; cdecl;
    function subtype: UIEventSubtype; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function touchesForGestureRecognizer( gesture: UIGestureRecognizer ): NSSet; cdecl;
    function touchesForView( view: UIView ): NSSet; cdecl;
    function touchesForWindow( window: UIWindow ): NSSet; cdecl;
  end;

  TUIEvent = class( TOCGenericImport<UIEventClass, UIEvent> )
  end;

  // ------------------------------------------------------------------------------
{$IFDEF DELPHIXE4}

  uuid_t = pbyte;

  NSUUIDClass = interface( NSObjectClass )
    ['{C272929D-47A7-4719-8883-2D1B5393D7D4}']
    function UUID: Pointer; cdecl;
  end;

  NSUUID = interface( NSObject )
    ['{5CDFDACF-47B2-4A3E-9004-BD356D47BF0B}']
    function init: NSUUID; cdecl;
    function initWithUUIDBytes: NSUUID; cdecl;
    function initWithUUIDString( string_: NSString ): NSUUID; cdecl;
    function UUIDString: NSString; cdecl;
    procedure getUUIDBytes( uuid: uuid_t ); cdecl;
  end;

  TNSUUID = class( TOCGenericImport<NSUUIDClass, NSUUID> )
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  PNSURLResponse = ^NSURLResponse;

  NSURLConnectionClass = interface( NSObjectClass )
    ['{D0268204-2FFF-46D8-B45F-464A3B346E0D}']
    function canHandleRequest( request: NSURLRequest ): Boolean; cdecl;
    function connectionWithRequest( request: NSURLRequest; delegate: Pointer ): Pointer; cdecl;
    function sendSynchronousRequest( request: NSURLRequest; returningResponse: PPointer; error: PPointer ): NSData; cdecl;
  end;

  NSURLConnection = interface( NSObject )
    ['{4BADC29F-3F13-4FBE-822B-6B156B03B90F}']
    procedure cancel; cdecl;
    function initWithRequest( request: NSURLRequest; delegate: Pointer ): Pointer; cdecl; overload;
    function initWithRequest( request: NSURLRequest; delegate: Pointer; startImmediately: Boolean ): Pointer; cdecl; overload;
    procedure scheduleInRunLoop( aRunLoop: NSRunLoop; forMode: NSString ); cdecl;
    procedure setDelegateQueue( queue: NSOperationQueue ); cdecl;
    procedure start; cdecl;
    procedure unscheduleFromRunLoop( aRunLoop: NSRunLoop; forMode: NSString ); cdecl;
  end;

  TNSURLConnection = class( TOCGenericImport<NSURLConnectionClass, NSURLConnection> )
  end;

  // ------------------------------------------------------------------------------
  NSLockClass = interface( NSObjectClass )
    ['{DD30A910-42FA-455D-ADA8-576655E0E799}']
  end;

  NSLock = interface( NSObject )
    ['{15DCD242-9633-4902-9E6E-7B2948E75BFD}']
    function lockBeforeDate( limit: NSDate ): Boolean; cdecl;
    function name: NSString; cdecl;
    procedure setName( n: NSString ); cdecl;
    function tryLock: Boolean; cdecl;
    procedure unlock; cdecl;
  end;

  TNSLock = class( TOCGenericImport<NSLockClass, NSLock> )
  end;

  // ------------------------------------------------------------------------------
  UIDeviceClass = interface( NSObjectClass )
    ['{7380E473-5CFB-4752-970E-EA58C404989E}']
    function currentDevice: Pointer; cdecl;
  end;

  UIDevice = interface( NSObject )
    ['{253E55BF-74CA-4A12-A541-8C2D128826EE}']
    function batteryLevel: Single; cdecl;
    function batteryState: UIDeviceBatteryState; cdecl;
    procedure beginGeneratingDeviceOrientationNotifications; cdecl;
    procedure endGeneratingDeviceOrientationNotifications; cdecl;
    function isBatteryMonitoringEnabled: Boolean; cdecl;
    function isGeneratingDeviceOrientationNotifications: Boolean; cdecl;
    function isMultitaskingSupported: Boolean; cdecl;
    function isProximityMonitoringEnabled: Boolean; cdecl;
    function localizedModel: NSString; cdecl;
    function model: NSString; cdecl;
    function name: NSString; cdecl;
    function orientation: UIDeviceOrientation; cdecl;
    procedure playInputClick; cdecl;
    function proximityState: Boolean; cdecl;
    procedure setBatteryMonitoringEnabled( batteryMonitoringEnabled: Boolean ); cdecl;
    procedure setProximityMonitoringEnabled( proximityMonitoringEnabled: Boolean ); cdecl;
    function systemName: NSString; cdecl;
    function systemVersion: NSString; cdecl;
    function uniqueIdentifier: NSString; cdecl;
    function userInterfaceIdiom: UIUserInterfaceIdiom; cdecl;
    function identifierForVendor: NSUUID; cdecl;
  end;

  TUIDevice = class( TOCGenericImport<UIDeviceClass, UIDevice> )
  end;

  // -------------------------------------------------------------
  // CAMediaTiming Protocol Reference
  // -------------------------------------------------------------
  CAMediaTimingClass = interface( NSObjectClass )
    ['{288E5EA2-8BA1-4C2B-A636-BF95114C072A}']
  end;

  CAMediaTiming = interface( NSObject )
    ['{ADB5DEE3-61F3-416E-8B25-4AAB719665A5}']

    function duration: CFTimeInterval; cdecl;
    procedure setDuration( duration: CFTimeInterval ); cdecl;

    function beginTime: CFTimeInterval; cdecl;
    procedure setBeginTime( beginTime: CFTimeInterval ); cdecl;

    function timeOffset: CFTimeInterval; cdecl;
    procedure setTimeOffset( timeOffset: CFTimeInterval ); cdecl;

    function repeatCount: double; cdecl;
    procedure setRepeatCount( repeatCount: double ); cdecl;

    function speed: double; cdecl;
    procedure setSpeed( speed: double ); cdecl;

    function autoreverses: boolean; cdecl;
    procedure setAutoreverses( autoreverses: boolean ); cdecl;

    function fillMode: NSString; cdecl;
    procedure setFillMode( fillMode: NSString ); cdecl;

    function fromValue: pointer; cdecl;
    procedure setFromValue( fromValue: Pointer ); cdecl;

    function toValue: pointer; cdecl;
    procedure setToValue( toValue: Pointer ); cdecl;

  end;

  // -----------------------------------------------------------------------------
  // CAAnimation Class
  // -----------------------------------------------------------------------------
  CAAnimationClass = interface( CAMediaTimingClass )
    ['{0903E9D0-791A-483D-B900-04814B23E1C0}']
    function animation: Pointer; cdecl;
    function defaultValueForKey( key: NSString ): Pointer; cdecl;
  end;

  CAAnimation = interface( CAMediaTiming )
    ['{1F038103-BECA-4A60-89CC-E89CEFD1F06E}']
    function delegate: Pointer; cdecl;
    function isRemovedOnCompletion: Boolean; cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setRemovedOnCompletion( removedOnCompletion: Boolean ); cdecl;
    procedure setTimingFunction( timingFunction: CAMediaTimingFunction ); cdecl;
    function shouldArchiveValueForKey( key: NSString ): Boolean; cdecl;
    function timingFunction: CAMediaTimingFunction; cdecl;
  end;

  TCAAnimation = class( TOCGenericImport<CAAnimationClass, CAAnimation> )
  end;

  // -----------------------------------------------------------------------------
  // CATransition Class
  // -----------------------------------------------------------------------------
  CATransitionClass = interface( CAAnimationClass )
    ['{96875C05-A8AE-4333-B63F-845BE2BAAAD9}']
  end;

  CATransition = interface( CAAnimation )
    ['{4C940131-542C-485E-B2F1-C6B00484ECDB}']
    function endProgress: Single; cdecl;
    function filter: Pointer; cdecl;
    procedure setEndProgress( endProgress: Single ); cdecl;
    procedure setFilter( filter: Pointer ); cdecl;
    procedure setStartProgress( startProgress: Single ); cdecl;
    procedure setSubtype( subtype: NSString ); cdecl;
    procedure setType( type_: NSString ); cdecl;
    function startProgress: Single; cdecl;
    function subtype: NSString; cdecl;
  end;

  TCATransition = class( TOCGenericImport<CATransitionClass, CATransition> )
  end;

  // -----------------------------------------------------------------------------
  // CAPropertyAnimation Class
  // -----------------------------------------------------------------------------
  CAPropertyAnimationClass = interface( CAAnimationClass )
    ['{02701363-04E5-436A-BC96-D6FFB170CFE9}']
    function animationWithKeyPath( path: NSString ): Pointer; cdecl;
  end;

  CAPropertyAnimation = interface( CAAnimation )
    ['{92ADB3CB-1066-40B5-8D65-75F9321599DB}']
    function isAdditive: Boolean; cdecl;
    function isCumulative: Boolean; cdecl;
    function keyPath: NSString; cdecl;
    procedure setAdditive( additive: Boolean ); cdecl;
    procedure setCumulative( cumulative: Boolean ); cdecl;
    procedure setKeyPath( keyPath: NSString ); cdecl;
    procedure setValueFunction( valueFunction: CAValueFunction ); cdecl;
    function valueFunction: CAValueFunction; cdecl;
  end;

  TCAPropertyAnimation = class( TOCGenericImport<CAPropertyAnimationClass, CAPropertyAnimation> )
  end;

  // -----------------------------------------------------------------------------
  // CABasicAnimation Class
  // -----------------------------------------------------------------------------
  CABasicAnimationClass = interface( CAPropertyAnimationClass )
    ['{59689BBF-F21A-40B2-8A37-56D42C64B08C}']
  end;

  CABasicAnimation = interface( CAPropertyAnimation )
    ['{906FFA4D-9C47-490C-B0B3-7751EF1995B0}']
    function byValue: Pointer; cdecl;
    function fromValue: Pointer; cdecl;
    procedure setByValue( byValue: Pointer ); cdecl;
    procedure setFromValue( fromValue: Pointer ); cdecl;
    procedure setToValue( toValue: Pointer ); cdecl;
    function toValue: Pointer; cdecl;
  end;

  TCABasicAnimation = class( TOCGenericImport<CABasicAnimationClass, CABasicAnimation> )
  end;

  // -----------------------------------------------------------------------------
  // UIRefreshControl Class
  // -----------------------------------------------------------------------------
  UIRefreshControlClass = interface( UIControlClass )
    ['{1C193A3E-64FC-44F9-91CA-80E951BB45F2}']
  end;

  UIRefreshControl = interface( UIControl )
    ['{DF5CA716-B196-4090-B44D-0DA40C999AB2}']
    function tintColor: UIColor; cdecl;
    procedure setTintColor( tintColor: UIColor ); cdecl;

    function attributedTitle: NSAttributedString; cdecl;
    procedure setAttributedTitle( attributedTitle: NSAttributedString ); cdecl;

    procedure beginRefreshing; cdecl;
    procedure endRefreshing; cdecl;

    function refreshing: boolean; cdecl;
  end;

  TUIRefreshControl = class( TOCGenericImport<UIRefreshControlClass, UIRefreshControl> );

function UIGraphicsGetImageFromCurrentImageContext: PUIImage; cdecl; external libUIKit name _PU + 'UIGraphicsGetImageFromCurrentImageContext';
// ----------------------------------------------------------------------------
// Audio
function AVAudioSessionCategoryPlayAndRecord: NSString;
function AVAudioSessionCategoryPlayback: NSString;
function AVAudioSessionCategoryAmbient: NSString;
function AVAudioSessionCategorySoloAmbient: NSString;
function AVAudioSessionCategoryRecord: NSString;
function AVAudioSessionCategoryAudioProcessing: NSString;
function AVAudioSessionCategoryMultiRoute: NSString;

// ----------------------------------------------------------------------------
function kCAMediaTimingFunctionLinear: NSString;
function kCAMediaTimingFunctionEaseIn: NSString;
function kCAMediaTimingFunctionEaseOut: NSString;
function kCAMediaTimingFunctionEaseInEaseOut: NSString;
function kCAMediaTimingFunctionDefault: NSString;

// ------------------------------------------------------------------------------
// Common Transition Types
function kCATransitionFade: NSString;
function kCATransitionMoveIn: NSString;
function kCATransitionPush: NSString;
function kCATransitionReveal: NSString;

// ------------------------------------------------------------------------------
// Common Transition Subtypes
function kCATransitionFromRight: NSString;
function kCATransitionFromLeft: NSString;
function kCATransitionFromTop: NSString;
function kCATransitionFromBottom: NSString;

// ------------------------------------------------------------------------------
// Fill Modes
function kCAFillModeRemoved: NSString;
function kCAFillModeForwards: NSString;
function kCAFillModeBackwards: NSString;
function kCAFillModeBoth: NSString;
function kCAFillModeFrozen: NSString;

// ------------------------------------------------------------------------------
// Filter Parameter Keys
function kCIInputImageKey: NSString;

// ------------------------------------------------------------------------------
// Keys for Text Attributes Dictionaries
function UITextAttributeFont: NSString;
function UITextAttributeTextColor: NSString;
function UITextAttributeTextShadowColor: NSString;
function UITextAttributeTextShadowOffset: NSString;

// ------------------------------------------------------------------------------
// NSURLProtectionSpace Authentication Methods
function NSURLAuthenticationMethodDefault: NSString;
function NSURLAuthenticationMethodHTTPBasic: NSString;
function NSURLAuthenticationMethodHTTPDigest: NSString;
function NSURLAuthenticationMethodHTMLForm: NSString;
function NSURLAuthenticationMethodNegotiate: NSString;
function NSURLAuthenticationMethodNTLM: NSString;
function NSURLAuthenticationMethodClientCertificate: NSString;
function NSURLAuthenticationMethodServerTrust: NSString;

// ------------------------------------------------------------------------------
// PDF Creation Functions

procedure UIGraphicsBeginPDFContextToData( data: Pointer { NSMutableData }; bounds: CGRect; documentInfo: NSDictionary ); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToData';
function UIGraphicsBeginPDFContextToFile( path: PNSString; bounds: CGRect; documentInfo: NSDictionary ): Boolean; cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToFile';
procedure UIGraphicsBeginPDFPage; cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFPage';
procedure UIGraphicsBeginPDFPageWithInfo( bounds: CGRect; pageInfo: NSDictionary ); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFPageWithInfo';
procedure UIGraphicsEndPDFContext; cdecl; external libUIKit name _PU + 'UIGraphicsEndPDFContext';

// ------------------------------------------------------------------------------
procedure UISaveVideoAtPathToSavedPhotosAlbum( videoPath: PNSString; completionTarget: Pointer; completionSelector: SEL; contextInfo: Pointer ); cdecl; external libUIKit name _PU + 'UISaveVideoAtPathToSavedPhotosAlbum';
function UIVideoAtPathIsCompatibleWithSavedPhotosAlbum( videoPath: PNSString ): Boolean; cdecl; external libUIKit name _PU + 'UIVideoAtPathIsCompatibleWithSavedPhotosAlbum';
procedure UIImageWriteToSavedPhotosAlbum( image: PUIImage; completionTarget: Pointer; completionSelector: SEL; contextInfo: Pointer ); cdecl; external libUIKit name _PU + 'UIImageWriteToSavedPhotosAlbum';

// ------------------------------------------------------------------------------
function CreatePDFfromView( aView: UIView; FileName: string ): Boolean; overload;
function CreatePDFfromView( aViewController: UIViewController; FileName: string ): Boolean; overload;

// ------------------------------------------------------------------------------
// function AudioSessionSetProperty( inID: UInt32; inDataSize: UInt32; inData: Pointer ): OSStatus; cdecl; external libAudioToolbox name _PU + 'AudioSessionSetProperty';
// function AudioSessionSetActive( active: Boolean ): OSStatus; cdecl; external libAudioToolbox name _PU + 'AudioSessionSetActive';

// ------------------------------------------------------------------------------
// Metadata Query Search Scopes
function NSMetadataQueryUserHomeScope: NSString;
function NSMetadataQueryLocalComputerScope: NSString;
function NSMetadataQueryNetworkScope: NSString;
function NSMetadataQueryUbiquitousDocumentsScope: NSString;
function NSMetadataQueryUbiquitousDataScope: NSString;
function NSMetadataQueryIndexedLocalComputerScope: NSString;
function NSMetadataQueryIndexedNetworkScope: NSString;

// ------------------------------------------------------------------------------
// Attribute Keys
// Attribute keys that may be associated with an item.

function NSMetadataItemFSNameKey: NSString;
function NSMetadataItemDisplayNameKey: NSString;
function NSMetadataItemURLKey: NSString;
function NSMetadataItemPathKey: NSString;
function NSMetadataItemFSSizeKey: NSString;
function NSMetadataItemFSCreationDateKey: NSString;
function NSMetadataItemFSContentChangeDateKey: NSString;
// ------------------------------------------------------------------------------
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function kCAFillModeRemoved: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAFillModeRemoved' );
end;

// ------------------------------------------------------------------------------
function kCAFillModeForwards: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAFillModeForwards' );
end;

// ------------------------------------------------------------------------------
function kCAFillModeBackwards: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAFillModeBackwards' );
end;

// ------------------------------------------------------------------------------
function kCAFillModeBoth: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAFillModeBoth' );
end;

// ------------------------------------------------------------------------------
function kCAFillModeFrozen: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAFillModeFrozen' );
end;

// ------------------------------------------------------------------------------
function kCIInputImageKey: NSString;
begin
  result := CocoaNSStringConst( libCoreImage, 'kCIInputImageKey' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryPlayAndRecord: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryPlayAndRecord' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryPlayback: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryPlayback' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryAmbient: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryAmbient' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategorySoloAmbient: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategorySoloAmbient' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryRecord: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryRecord' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryAudioProcessing: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryAudioProcessing' );
end;

// ------------------------------------------------------------------------------
function AVAudioSessionCategoryMultiRoute: NSString;
begin
  result := CocoaNSStringConst( libAVFoundation, 'AVAudioSessionCategoryMultiRoute' );
end;

// ------------------------------------------------------------------------------
function kCAMediaTimingFunctionLinear: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAMediaTimingFunctionLinear' );
end;

// ------------------------------------------------------------------------------
function kCAMediaTimingFunctionEaseIn: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAMediaTimingFunctionEaseIn' );
end;

// ------------------------------------------------------------------------------
function kCAMediaTimingFunctionEaseOut: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAMediaTimingFunctionEaseOut' );
end;

// ------------------------------------------------------------------------------
function kCAMediaTimingFunctionEaseInEaseOut: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAMediaTimingFunctionEaseInEaseOut' );
end;

// ------------------------------------------------------------------------------
function kCAMediaTimingFunctionDefault: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCAMediaTimingFunctionDefault' );
end;

function kCATransitionFade: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionFade' );
end;

// ------------------------------------------------------------------------------
function kCATransitionMoveIn: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionMoveIn' );
end;

// ------------------------------------------------------------------------------
function kCATransitionPush: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionPush' );
end;

// ------------------------------------------------------------------------------
function kCATransitionReveal: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionReveal' );
end;

// ------------------------------------------------------------------------------
function UITextAttributeFont: NSString;
begin
  result := CocoaNSStringConst( libUIKit, 'UITextAttributeFont' );
end;

// ------------------------------------------------------------------------------
function kCATransitionFromRight: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionFromRight' );
end;

// ------------------------------------------------------------------------------
function kCATransitionFromLeft: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionFromLeft' );
end;

// ------------------------------------------------------------------------------
function kCATransitionFromTop: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionFromTop' );
end;

// ------------------------------------------------------------------------------
function kCATransitionFromBottom: NSString;
begin
  result := CocoaNSStringConst( libQuartzCore, 'kCATransitionFromBottom' );
end;

// ------------------------------------------------------------------------------
function UITextAttributeTextColor: NSString;
begin
  result := CocoaNSStringConst( libUIKit, 'UITextAttributeTextColor' );
end;

// ------------------------------------------------------------------------------
function UITextAttributeTextShadowColor: NSString;
begin
  result := CocoaNSStringConst( libUIKit, 'UITextAttributeTextShadowColor' );
end;

// ------------------------------------------------------------------------------
function UITextAttributeTextShadowOffset: NSString;
begin
  result := CocoaNSStringConst( libUIKit, 'UITextAttributeTextShadowOffset' );
end;

// ------------------------------------------------------------------------------
// NSURLProtectionSpace Authentication Methods
function NSURLAuthenticationMethodDefault: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodDefault' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodHTTPBasic: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodHTTPBasic' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodHTTPDigest: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodHTTPDigest' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodHTMLForm: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodHTMLForm' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodNegotiate: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodNegotiate' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodNTLM: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodNTLM' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodClientCertificate: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodClientCertificate' );
end;

// ------------------------------------------------------------------------------
function NSURLAuthenticationMethodServerTrust: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSURLAuthenticationMethodServerTrust' );
end;

// ------------------------------------------------------------------------------
function GeneratePDFThumbnails( FileName: string; ThumbWidth: Single; ThumbHeight: Single ): Boolean;
var
  pdfFileUrl    : NSURL;
  pdf           : CGPDFDocumentRef;
  page          : CGPDFPageRef;
  aRect         : CGRect;
  context       : CGContextRef;
  thumbnailImage: UIImage;
  pdfTransform  : CGAffineTransform;
  totalNum      : NSUInteger;
  i             : Integer;
begin

  result     := false;
  pdfFileUrl := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FileName ) ) );
  pdf        := CGPDFDocumentCreateWithURL( CFURLRef( pdfFileUrl ) );

  aRect := CGRectMake( 0, 0, ThumbWidth, ThumbHeight ); // thumbnail size
  UIGraphicsBeginImageContext( aRect.size );
  context := UIGraphicsGetCurrentContext( );

  totalNum := CGPDFDocumentGetNumberOfPages( pdf );

  for i := 0 to totalNum - 1 do
  begin

    CGContextSaveGState( context );
    CGContextTranslateCTM( context, 0.0, aRect.size.height );
    CGContextScaleCTM( context, 1.0, -1.0 );

    CGContextSetGrayFillColor( context, 1.0, 1.0 );
    CGContextFillRect( context, aRect );

    // Grab the first PDF page
    page         := CGPDFDocumentGetPage( pdf, i + 1 );
    pdfTransform := CGPDFPageGetDrawingTransform( page, kCGPDFMediaBox, aRect, 0, integer( true ) );
    // And apply the transform.
    CGContextConcatCTM( context, pdfTransform );

    CGContextDrawPDFPage( context, page );

    // Create the new UIImage from the context
    thumbnailImage := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );

    // Use thumbnailImage (e.g. drawing, saving it to a file, etc)

    CGContextRestoreGState( context );

  end;

  UIGraphicsEndImageContext( );
  CGPDFDocumentRelease( pdf );

end;

// ------------------------------------------------------------------------------
function CreatePDFfromUIView( aView: UIView; FileName: string ): Boolean;
var
  pdfData   : NSMutableData;
  pdfContext: CGContextRef;
begin
  result := false;
  // Creates a mutable data object for updating with binary data, like a byte array
  // pdfData := TNSMutableData.Wrap( TNSMutableData.OCClass.data );
  pdfData := TNSMutableData.Create;

  // Points the pdf converter to the mutable data object and to the UIView to be converted
  UIGraphicsBeginPDFContextToData( ( pdfData as ILocalObject ).GetObjectID, aView.bounds, nil );
  UIGraphicsBeginPDFPage( );
  pdfContext := UIGraphicsGetCurrentContext( );

  // draws rect to the view and thus this is captured by UIGraphicsBeginPDFContextToData
  aView.layer.renderInContext( pdfContext );

  // remove PDF rendering context
  UIGraphicsEndPDFContext( );
  RemoveFile( FileName );
  if pdfData.length > 0 then
    result := pdfData.writeToFile( NSStr( FileName ), false );
  pdfData.release;
end;

// ------------------------------------------------------------------------------
function CreatePDFfromView( aView: UIView; FileName: string ): Boolean;
begin
  result := CreatePDFfromUIView( aView, FileName );
end;

// ------------------------------------------------------------------------------
function CreatePDFfromView( aViewController: UIViewController; FileName: string ): Boolean;
begin
  result := CreatePDFfromUIView( aViewController.view, FileName );
end;

// ------------------------------------------------------------------------------
// Metadata Query Search Scopes
function NSMetadataQueryUserHomeScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryUserHomeScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryLocalComputerScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryLocalComputerScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryNetworkScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryNetworkScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryUbiquitousDocumentsScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryUbiquitousDocumentsScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryUbiquitousDataScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryUbiquitousDataScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryIndexedLocalComputerScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryIndexedLocalComputerScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataQueryIndexedNetworkScope: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataQueryIndexedNetworkScope' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemFSNameKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemFSNameKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemDisplayNameKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemDisplayNameKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemURLKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemURLKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemPathKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemPathKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemFSSizeKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemFSSizeKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemFSCreationDateKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemFSCreationDateKey' );
end;

// ------------------------------------------------------------------------------
function NSMetadataItemFSContentChangeDateKey: NSString;
begin
  result := CocoaNSStringConst( libFoundation, 'NSMetadataItemFSContentChangeDateKey' );
end;

// ------------------------------------------------------------------------------
initialization

{$ENDIF}

end.
