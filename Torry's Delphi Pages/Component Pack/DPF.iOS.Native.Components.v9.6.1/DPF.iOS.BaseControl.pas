// ------------------------------------------------------------------------------
// DPF.iOS.BaseControl Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: bayaghoobi@gmail.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: yaghoobi@dpfaragir.com
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
unit DPF.iOS.BaseControl;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.TypInfo,
  System.Math,
  System.DateUtils,

  DPF.iOS.UIFont,
  DPF.iOS.Classes,
{$IFDEF IOS}
  System.Mac.CFUtils,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  iOSapi.QuartzCore,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs;

const
  DBL_TAP_DELAY = 0.001;
  DPFAbout      = 'D.P.F Delphi iOS Native Components (by Babak Yaghoobi - b_yaghobi@yahoo.com / bayaghoobi@gmail.com)';
  DPFVersion    = '9.6.1';

type

  TDPFiOSBaseControl = class;
{$IFDEF IOSDEVICE}
  DPFNSPoint = NSPoint;

  DPFNSize = NSSize;

  DPFNSRect = NSRect;

{$ELSE}

  DPFNSPoint = record
    x: Single;
    y: Single;
  end;

  DPFNSize = record
    width: Single;
    height: Single;
  end;

  DPFNSRect = record
    origin: DPFNSPoint;
    size: DPFNSize;
  end;
{$ENDIF}
{$IFDEF IOS}

  NSInteger1 = record
    val: Integer;
  end;

  NSInteger2 = record
    val: Integer;
  end;

  NSInteger3 = record
    val: Integer;
  end;

  // ------------------------------------------------------------------------------
  (*
    IDPFAnimationDelegate = interface( IObjectiveC { NSObject } )
    ['{4B0EE35B-425B-49B5-BEC7-BD93D871B4FE}']
    procedure animationDidStop( animationID: NSString; finished: NSNumber; context: CGContextRef ); cdecl;
    procedure animationDidStart( animationID: NSString; context: CGContextRef ); cdecl;
    end;

    // ------------------------------------------------------------------------------
    TDPFAnimationDelegate = class( TOCLocal, IDPFAnimationDelegate )
    private
    FDPFiOSBaseControl: TDPFiOSBaseControl;
    public
    constructor Create( ADPFiOSBaseControl: TDPFiOSBaseControl );
    // function GetObjectiveCClass: PTypeInfo; override;

    procedure animationDidStop( animationID: NSString; finished: NSNumber; context: CGContextRef ); cdecl;
    procedure animationDidStart( animationID: NSString; context: CGContextRef ); cdecl;
    end;
  *)
  // ----------------------------------------------------------------------------
  UIGestureRecognizerDelegate = interface( IObjectiveC )
    ['{D80B955C-9F57-428C-9635-4991E1FF1F87}']
    // function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch ): Boolean; cdecl; overload;
    function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer ): Boolean; cdecl; overload;
    function gestureRecognizerShouldBegin( gestureRecognizer: UIGestureRecognizer ): Boolean; cdecl;
    // function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldBeRequiredToFailByGestureRecognizer: UIGestureRecognizer ): Boolean; cdecl; overload;
    procedure panGestureRecognizer( sender: UIPanGestureRecognizer ); cdecl;
  end;

  TDPFUIPanGestureRecognizerDelegate = class( TOCLocal, UIGestureRecognizerDelegate )
  private
    FDPFiOSBaseControl: TDPFiOSBaseControl;
  public
    constructor Create( ADPFiOSBaseControl: TDPFiOSBaseControl );

    // function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch ): Boolean; overload; cdecl;
    function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer ): Boolean; overload; cdecl;
    function gestureRecognizerShouldBegin( gestureRecognizer: UIGestureRecognizer ): Boolean; cdecl;
    // function gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldBeRequiredToFailByGestureRecognizer: UIGestureRecognizer ): Boolean; overload; cdecl;

    procedure panGestureRecognizer( sender: UIPanGestureRecognizer ); cdecl;
  end;

{$ENDIF}

  TDPFTransitionFinished = procedure( finished: Boolean ) of object;

  TDPFTextAutocorrectionType     = ( tatDefault = 0, tatNo = 1, tatYes = 2 );
  TDPFTextSpellCheckingType      = ( tsctDefault = 0, tsctNo = 1, tsctYes = 2 );
  TDPFKeyboardAppearance         = ( kaDefault = 0, kaDark = 1, kakLight = 2, aAlert = 3 );
  TDPFReturnKey                  = ( rkDefault = 0, rkGo = 1, rkGoogle = 2, rkJoin = 3, rkNext = 4, rkRoute = 5, rkSearch = 6, rkSend = 7, rkYahoo = 8, rkDone = 9, rkEmergencyCall = 10 );
  TDPFTextAutocapitalizationType = ( tapNone = 0, tapWords = 1, tapSentences = 2, tapAllCharacters = 3 );
  TDPFKeyboardType               = ( ktDefault, ktAlphabet, ktNumbersAndPunctuation, ktURL, ktNumberPad, ktPhonePad, ktNamePhonePad, ktEmailAddress, ktDecimalPad, ktTwitter );

  TDPFViewVerticalAlignment   = ( vaCenter, vaTop, vaBottom, vaFill );
  TDPFViewHorizontalAlignment = ( haCenter, haTop, haBottom, haFill );

  TDPFTextAlignment      = ( taLeft = 0, taCenter = 1, taRight = 2 );
  TDPFTextAutoSizeType   = ( astNone, astSizeToFit, astSizeToFitFixHeight );
  TDPFBaseLineAdjustment = ( blaAlignBaseLines, blaAlignCenters, blaNone );

  TDPFContentHorizontalAlignment = ( cchaCenter, cchaLeft, cchaRight, cchaFill );
  TDPFContentVerticalAlignment   = ( ccvaCenter, ccvaTop, ccvaBottom, ccvaFill );

  TDPFUIViewContentMode = ( vcmScaleToFill = 0, vcmScaleAspectFit = 1, vcmScaleAspectFill = 2, vcmRedraw = 3, vcmCenter = 4, vcmTop = 5, vcmBottom = 6, vcmLeft = 7, vcmRight = 8, vcmTopLeft = 9, vcmTopRight = 10, vcmBottomLeft = 11, vcmBottomRight = 12 );

  TDPFLineBreak = ( lbWordWrap, lbCharacterWrap, lbClip, lbHeadTruncation, lbTailTruncation, lbMiddleTruncation );

  TDPFBarStyle = ( bsDefault = 0, bsBlackOpaque = 1, bsBlackTranslucent = 2 );

  TDPFViewAnimationCurve      = ( vacEaseInOut = 0, vacEaseIn = 1, vacEaseOut = 2, vacLinear = 3 );
  TDPFViewAnimationTransition = ( vatNone = 0, vatFlipFromLeft = 1, vatFlipFromRight = 2, vatUp = 3, vatDown = 4 { , vat110 = 100 } );
  TDPFViewAnimationSlideDir   = ( vasLeftToRight, vasRightToLeft );

  TDPFActivityIndicatorViewStyle = ( aisWhiteLarge = 0, aisWhite = 1, aisGray = 2 );

  TDPFBarButtonKind      = ( bkCustomView, bkTitle, bkImage, bkSystem );
  TDPFBarButtonItemStyle = ( bbisPlain = 0, bbisBordered = 1, bbisDone = 2 );

  TDPFBarButtonSystemItem = ( bbsiDone = 0, bbsiCancel = 1, bbsiEdit = 2, bbsiSave = 3, bbsiAdd = 4, bbsiFlexibleSpace = 5, bbsiFixedSpace = 6, bbsiCompose = 7, bbsiReply = 8, bbsiAction = 9, bbsiOrganize = 10, bbsiBookmarks = 11, bbsiSearch = 12, bbsiRefresh = 13, bbsiStop = 14, bbsiCamera = 15, bbsiTrash = 16, bbsiPlay = 17, bbsiPause = 18,
    bbsiRewind = 19, bbsiFastForward = 20 );

  TDPFFormChanging = procedure( Sender: TObject; OldForm: TForm; NewForm: TForm; var CanChange: Boolean ) of object;
  TDPFFormChanged  = procedure( Sender: TObject; OldForm: TForm; NewForm: TForm ) of object;

  TDPFFrame = class of TFrame;

  TDPFFrameChanging = procedure( Sender: TObject; OldFrame: TDPFFrame; NewFrame: TDPFFrame; var CanChange: Boolean ) of object;
  TDPFFrameChanged  = procedure( Sender: TObject; OldFrame: TDPFFrame; NewFrame: TDPFFrame ) of object;

  TPanGestureDirection         = ( pgdUp, pgdDown, pgdLeft, pgdRight );
  TPanGestureDirections        = set of TPanGestureDirection;
  TDPFPanGestureRecognizeState = ( grsPossible = 0, grsBegan = 1, grsChanged = 2, grsEnded = 3, grsCancelled = 4, grsFailed = 5 );
  TDPFOnPanGestureRecognize    = procedure( Sender: TObject; CurPosition: DPFNSPoint; Translation: DPFNSPoint; Center: DPFNSPoint; Velocity: DPFNSPoint; State: TDPFPanGestureRecognizeState; var MoveCenter: Boolean ) of object;

  TOpenObject = class( TFmxObject );

  TDPFTouchesBegan = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesMoved = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesEnded = procedure( Sender: TObject; const TapCount: Integer; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;

  TDPFOnClicked       = procedure( Sender: TObject ) of object;
  TDPFOnDoubleClicked = procedure( Sender: TObject ) of object;
  TAnimationCallBack  = reference to procedure;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFiOSBaseControl = class( TControl )
  private
    FDestroying              : Boolean;
    FOldPositionEvent        : TNotifyEvent;
    FContentMode             : TDPFUIViewContentMode;
    FAlpha                   : Single;
    FUserInteraction         : Boolean;
    FFont                    : TDPFFont;
    FOriginParent            : TDPFiOSBaseControl;
    FShadowOffsetHeight      : Single;
    FShadowOpacity           : Single;
    FShadow                  : Boolean;
    FShadowRadius            : Single;
    FShadowOffsetWidth       : Single;
    FShadowColor             : TAlphaColor;
    FBorderColor             : TAlphaColor;
    FCornerRadius            : Single;
    FBorderWidth             : Integer;
    FisLoaded                : Boolean;
    FVisible                 : Boolean;
    FTagBoolean              : Boolean;
    FMasksToBounds           : Boolean;
    FClipsToBounds           : Boolean;
    FLockComponent           : Boolean;
    FAutoresizingMask        : Boolean;
    FTagDateTime             : TDateTime;
    FOnPanGestureRecognizer  : TDPFOnPanGestureRecognize;
    FPanGestureDirections    : TPanGestureDirections;
    FAddChildViewController  : Boolean;
    FPanVelocitySensitivity  : word;
    FPanSimultaneously       : Boolean;
    FPanRecognizerShouldBegin: Boolean;
    FTagInterface            : IInterface;
    FTagNative               : NativeInt;
    procedure SetContentMode( const Value: TDPFUIViewContentMode );
    procedure SetAlpha( const Value: Single );
    procedure PositionOnChange( Sender: TObject );
    procedure ScaleOnChange( Sender: TObject );
    procedure SetUserInteraction( const Value: Boolean );
    procedure SetFont( const Value: TDPFFont );
    procedure SetShadow( const Value: Boolean );
    procedure SetShadowColor( const Value: TAlphaColor );
    procedure SetShadowOffsetHeight( const Value: Single );
    procedure SetShadowOffsetWidth( const Value: Single );
    procedure SetShadowOpacity( const Value: Single );
    procedure SetShadowRadius( const Value: Single );
    procedure SetBorderWidth( const Value: Integer );
    procedure SetCornerRadius( const Value: Single );
    procedure SetBorderColor( const Value: TAlphaColor );
    procedure SetMasksToBounds( const Value: Boolean );
    procedure SetClipsToBounds( const Value: Boolean );
    procedure SetLockComponent( const Value: Boolean );
    procedure setAutoresizingMask( const Value: Boolean );
    procedure SetTagNative( const Value: NativeInt );
  protected
{$IFDEF IOS}
    FUIControl      : NSObject;
    FParentUIControl: NSObject;
    // FDPFAnimationDelegate: TDPFAnimationDelegate;
    FChiledsEmbbedTo: TDPFiOSBaseControl;
    FShadowLayer    : CALayer;
    isRootControl   : Boolean;

    FVelocityStarted                  : Boolean;
    FUIPanGestureRecognizer           : UIPanGestureRecognizer;
    FDPFUIPanGestureRecognizerDelegate: TDPFUIPanGestureRecognizerDelegate;

    procedure FormOnShow( Sender: TObject );
    function GetControlView( Control: NSObject ): UIView;
    function GetControlViewController( Control: NSObject ): UIViewController;
    procedure OnUIViewControllerCompletion;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
    procedure InternalPaint( Text: string; TextColor: TAlphaColor; TextAlign: TDPFTextAlignment; BackgroundColor: TAlphaColor );
{$IFNDEF IOS}
    procedure Paint; override;
    procedure Invalidate;
{$ENDIF}
    procedure DoAbsoluteChanged; override; // SZ
    procedure SetParent( const Value: TFmxObject ); override; // SZ
    procedure SetParentComponent( Value: TComponent ); override;
    procedure CreateUIControl; virtual; // SZ: create native UI control inside this function in derived component
    procedure DestroyUIControl; virtual;
    procedure UpdateUIControlPosition; virtual;
  public
{$IFDEF IOS}
    ThisView: UIView;
{$ENDIF}
    AutoRelease     : Boolean;
    AddThisToSubView: Boolean;
    AddSubViewToThis: Boolean;
    ControlCaption  : string;
    procedure Loaded; override;
{$IFDEF DELPHIXE7}
    procedure ParentChanged; override;
{$ELSE}
    procedure ChangeParent; override;
{$ENDIF}
{$IFDEF IOS}
    function ViewWithTag( const Value: NativeInt ): UIView;

    procedure SetCustomEffect( const AnimationDuration: Single; EffectName: string; SubEffectName: string; WaitTime: Single );

    procedure SetPulseEffect( const AnimationDuration: Single; const WaitTime: Single );
    procedure SetRippleEffect( const AnimationDuration: Single; const WaitTime: Single );

    procedure StartAnimation( const AnimationDuration: Single );
    procedure StopAnimation;

    procedure DrawLine( FromPoint: DPFNSPoint; ToPoint: DPFNSPoint; StrokeWidth: Single; Color: TAlphaColor );
    procedure DrawRect( Rect: DPFNSRect; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor );
    procedure DrawArc( x: Single; y: Single; radius: Single; startAngle: Single; endAngle: Single; clockwise: Boolean; StrokeWidth: Single; Color: TAlphaColor );
    procedure DrawEllipse( Rect: DPFNSRect; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor );
    procedure DrawText( Text: string; x: Single; y: Single; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor; RotationDeg: Single = 0; DrawingMode: Longword = kCGTextFill; CharacterSpacing: Longword = 0; FontName: string = 'Helvetica'; FontSize: Integer = 14 );

    property UIControl: NSObject read FUIControl write FUIControl;
    // property ParentUIControl: NSObject read FParentUIControl;

    procedure AddSubView( ThisControl: TDPFiOSBaseControl; ToParentControl: TControl; ViewAdded: UIView = nil );
    procedure clearAllSubviews( Control: NSObject = nil; isRelease: Boolean = false );
{$ENDIF}
    procedure BringToFront; override;
    procedure SetFocus;
    // No Need !
    // procedure DoRealign; // override; //SZ: don't add override - this creates a stack overflow which crashes the IDE
    procedure DoResize;
    procedure SetVisible( const Value: Boolean ); override;

    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; virtual;

    procedure SetAnimationTransition( AnimationTransition: TDPFViewAnimationTransition; AnimationDuration: double = 0.2 ); overload;
    procedure SetAnimationTransition( HideView: TDPFiOSBaseControl; VisibleView: TDPFiOSBaseControl; Width: Single; Height: Single; AnimationSlideDir: TDPFViewAnimationSlideDir; AnimationDuration: double = 0.2 ); overload;
    procedure SetAnimationTransition( FromX, ToX, FromY, ToY, FromW, ToW, FromH, ToH: Single; AnimationDuration: double = 0.2; AnimationCallBack: TAnimationCallBack = nil ); overload;

    property OriginParent: TDPFiOSBaseControl read FOriginParent;
    property isLoaded: Boolean read FisLoaded;
    procedure doCurl( toUp: Boolean );

    property TagDateTime: TDateTime read FTagDateTime write FTagDateTime;
    property TagInterface: IInterface read FTagInterface write FTagInterface;

  published
    property ContentMode    : TDPFUIViewContentMode read FContentMode write SetContentMode default vcmScaleToFill;
    property Alpha          : Single read FAlpha write SetAlpha;
    property Visible        : Boolean read FVisible write SetVisible default true;
    property UserInteraction: Boolean read FUserInteraction write SetUserInteraction default true;
    property Font           : TDPFFont read FFont write SetFont;

    property MasksToBounds: Boolean read FMasksToBounds write SetMasksToBounds default false;
    property ClipsToBounds: Boolean read FClipsToBounds write SetClipsToBounds default true;

    property LockComponent: Boolean read FLockComponent write SetLockComponent default false;

    property PanVelocitySensitivity  : word read FPanVelocitySensitivity write FPanVelocitySensitivity default 0;
    property PanSimultaneously       : Boolean read FPanSimultaneously write FPanSimultaneously default true;
    property PanRecognizerShouldBegin: Boolean read FPanRecognizerShouldBegin write FPanRecognizerShouldBegin default true;

    property Shadow            : Boolean read FShadow write SetShadow default false;
    property ShadowOffsetWidth : Single read FShadowOffsetWidth write SetShadowOffsetWidth;
    property ShadowOffsetHeight: Single read FShadowOffsetHeight write SetShadowOffsetHeight;
    property ShadowOpacity     : Single read FShadowOpacity write SetShadowOpacity;
    property ShadowRadius      : Single read FShadowRadius write SetShadowRadius;
    property ShadowColor       : TAlphaColor read FShadowColor write SetShadowColor default TAlphaColors.Black;

    property BorderWidth : Integer read FBorderWidth write SetBorderWidth default 0;
    property BorderColor : TAlphaColor read FBorderColor write SetBorderColor default TAlphaColors.Null;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;

    property AutoresizingMask: Boolean read FAutoresizingMask write setAutoresizingMask default false;

    property TagBoolean: Boolean read FTagBoolean write FTagBoolean default true;

    property TagNative: NativeInt read FTagNative write SetTagNative;

    property PanGestureDirections  : TPanGestureDirections read FPanGestureDirections write FPanGestureDirections default [TPanGestureDirection.pgdUp, TPanGestureDirection.pgdDown, TPanGestureDirection.pgdLeft, TPanGestureDirection.pgdRight];
    property OnPanGestureRecognizer: TDPFOnPanGestureRecognize read FOnPanGestureRecognizer write FOnPanGestureRecognizer;

    property Scale;
    property Anchors;
    property Margins;
    property Align;
    property Position;
    property Width;
    property Height;
    property Size;
  end;

procedure MoveChildsToOriginParent( Control: TDPFiOSBaseControl; removeFromSuperViw: Boolean = true );

function MakeDPFNSPoint( x: Single; y: Single ): DPFNSPoint;

const
  CTextAlign: array [TDPFTextAlignment] of TTextAlign = ( TTextAlign.Leading, TTextAlign.Center, TTextAlign.Trailing );
  ContentAlignToText: array [TDPFContentHorizontalAlignment] of TTextAlign = ( TTextAlign.Center, TTextAlign.Leading, TTextAlign.Trailing, TTextAlign.Center );

implementation

// ------------------------------------------------------------------------------
function MakeDPFNSPoint( x: Single; y: Single ): DPFNSPoint;
begin
  result.x := x;
  result.y := y;
end;

// ------------------------------------------------------------------------------
procedure MoveChildsToOriginParent( Control: TDPFiOSBaseControl; removeFromSuperViw: Boolean = true );
var
  I: Integer;
  C: TDPFiOSBaseControl;
begin
  I := 0;
  while I < Control.ChildrenCount do
  begin
    if Control.Children[I] is TDPFiOSBaseControl then
    begin
      C := Control.Children[I] as TDPFiOSBaseControl;
      if C.Parent <> C.OriginParent then
      begin
        C.Parent := C.OriginParent;
        C.Loaded;
        i := 0;
        Continue;
      end;
    end;
    inc( i );
  end;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFiOSBaseControl.GetControlViewController( Control: NSObject ): UIViewController;
begin
  Result := nil;
  if Control.isKindOfClass( objc_getClass( 'UIViewController' ) ) or Control.isMemberOfClass( objc_getClass( 'UIViewController' ) ) then
    Result := UIViewController( FUIControl );
end;

// ------------------------------------------------------------------------------
function TDPFiOSBaseControl.GetControlView( Control: NSObject ): UIView;
begin
  Result := nil;
  if Control = nil then
    exit;
  if Control.isKindOfClass( objc_getClass( 'UIViewController' ) ) or Control.isMemberOfClass( objc_getClass( 'UIViewController' ) ) then
  begin
    Result := UIViewController( Control ).view
  end
  else if Control.isKindOfClass( objc_getClass( 'UIView' ) ) or Control.isMemberOfClass( objc_getClass( 'UIView' ) ) then
  begin
    Result := UIView( Control );
  end
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.FormOnShow( Sender: TObject );
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.OnUIViewControllerCompletion;
begin
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.AddSubView( ThisControl: TDPFiOSBaseControl; ToParentControl: TControl; ViewAdded: UIView = nil );
var
  C   : NSObject;
  Flag: Boolean;
  V   : UIView;
  VC  : UIViewController;
begin
  isRootControl := false;
  if not AddThisToSubView then
    exit;

  if ( ThisControl = nil ) or ( ThisControl.FUIControl = nil ) then
    exit;

  if ViewAdded = nil then
    V := UIView( ThisControl.FUIControl )
  else
    V := ViewAdded;

  VC := GetControlViewController( ThisControl.FUIControl );

  if assigned( VC ) then
    V := VC.view;

  if ( ToParentControl <> nil ) and ( ToParentControl is TDPFiOSBaseControl ) then
  begin
    if not( ToParentControl as TDPFiOSBaseControl ).AddSubViewToThis then
      Exit;

    if not Assigned( ( ToParentControl as TDPFiOSBaseControl ).FUIControl ) and not( ToParentControl as TDPFiOSBaseControl ).isLoaded then
      ( ToParentControl as TDPFiOSBaseControl ).Loaded;

    C := ( ToParentControl as TDPFiOSBaseControl ).FUIControl;

    if not Assigned( C ) then
    begin
      ShowMessage( 'iOS parent control (FUIControl) not created: ' + ToParentControl.Name );
      exit;
    end;

    Flag := C.isKindOfClass( objc_getClass( 'UIPageViewController' ) ) or C.isMemberOfClass( objc_getClass( 'UIPageViewController' ) );
    if Flag then
      UIPageViewController( C ).view.addSubview( V )
    else
    begin
      Flag := C.isKindOfClass( objc_getClass( 'UIViewController' ) ) or C.isMemberOfClass( objc_getClass( 'UIViewController' ) );
      if Flag then
      begin
        // UIViewController( C ).addChildViewController( VC );
        UIViewController( C ).view.addSubview( V )
      end
      else
      begin
        Flag := C.isKindOfClass( objc_getClass( 'UIView' ) ) or C.isMemberOfClass( objc_getClass( 'UIView' ) );
        if Flag then
          UIView( C ).addSubview( V )
      end;
    end;
  end
  else
  begin
    if Assigned( Owner ) then
      if Owner.InheritsFrom( TFrame ) then
        exit;

    if Assigned( Parent ) then
      if Parent.InheritsFrom( TFrame ) then
        exit;

    if Assigned( Owner ) and Assigned( TCommonCustomForm( Owner ).Handle ) then
    begin
      if Assigned( VC ) then
      begin
        FAddChildViewController := true;
        V                       := VC.view;
        GetSharedApplication.keyWindow.rootViewController.addChildViewController( VC );
      end;

      isRootControl := true;
      WindowHandleToPlatform( TCommonCustomForm( Owner ).Handle ).View.addSubview( V );

      DPFNSLog( 'TDPFiOSBaseControl.AddSubView(Handle): ' + name );
    end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.BringToFront;
{$IFDEF IOS}
var
  C : UIView;
  CP: UIView;
{$ENDIF}
begin
  inherited BringToFront;
{$IFDEF IOS}
  if Assigned( FUIControl ) and Assigned( FParentUIControl ) then
  begin
    C  := GetControlView( FUIControl );
    CP := GetControlView( FParentUIControl );
    if Assigned( C ) and Assigned( CP ) then
      CP.bringSubviewToFront( C );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF DELPHIXE7}

procedure TDPFiOSBaseControl.ParentChanged;
{$ELSE}

procedure TDPFiOSBaseControl.ChangeParent;
{$ENDIF}
var
  Flag: Boolean;
  S   : string;
begin

{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.ChangeParent : ' + name );
{$ENDIF}
  inherited;
  if FDestroying then
    exit;

  Flag := False;
  if Assigned( Owner ) then
  begin
    Flag := Owner.InheritsFrom( TFrame );
    S    := Owner.Name;
  end;

  if Flag then
    exit;

  if Assigned( Parent ) then
  begin
    Flag := Parent.InheritsFrom( TFrame );
    S    := Parent.Name;
  end;

  if Flag then
    exit;

{$IFDEF IOS}
  FParentUIControl := nil;
  if Assigned( Parent ) and ( Parent is TDPFiOSBaseControl ) then
    FParentUIControl := ( Parent as TDPFiOSBaseControl ).FUIControl
  else if Assigned( Owner ) and ( Owner is TDPFiOSBaseControl ) then
    FParentUIControl := ( Owner as TDPFiOSBaseControl ).FUIControl
  else if Assigned( Owner ) and Assigned( TCommonCustomForm( Owner ).Handle ) then
    FParentUIControl := WindowHandleToPlatform( TCommonCustomForm( Owner ).Handle ).View;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFiOSBaseControl.Create( AOwner: TComponent );
begin
{$IFDEF IOSDEBUG}
  DPFNSLog( 'TDPFiOSBaseControl.Create: ' + ClassName );
{$ENDIF}
  inherited Create( AOwner );

  FPanVelocitySensitivity   := 0;
  FPanSimultaneously        := true;
  FPanRecognizerShouldBegin := true;
  FAddChildViewController   := false;
  FPanGestureDirections     := [TPanGestureDirection.pgdUp, TPanGestureDirection.pgdDown, TPanGestureDirection.pgdLeft, TPanGestureDirection.pgdRight];
  FAutoresizingMask         := false;
  AutoRelease               := False;
  FMasksToBounds            := false;
  FClipsToBounds            := true;
  FisLoaded                 := False;
  FOriginParent             := TDPFiOSBaseControl( AOwner );
  AddThisToSubView          := true;
  AddSubViewToThis          := true;
  ControlCaption            := 'BaseControl';
  FContentMode              := vcmScaleToFill;
  FAlpha                    := 1;
  FOldPositionEvent         := Position.OnChange;
  Position.OnChange         := PositionOnChange;
  Scale.OnChange            := ScaleOnChange;
  FVisible                  := true;
  FUserInteraction          := true;
  FFont                     := TDPFFont.Create;
  FVisible                  := true;
  FTagBoolean               := true;
  FBorderWidth              := 0;
  FBorderColor              := TAlphaColors.Null;
  FCornerRadius             := 0;

  FShadow             := false;
  FShadowOffsetWidth  := 0;
  FShadowOffsetHeight := 10;
  FShadowOpacity      := 1;
  FShadowRadius       := 10;
  FShadowColor        := TAlphaColors.Black;
  FDestroying         := false;
{$IFDEF IOS}
  isRootControl                      := false;
  FVelocityStarted                   := false;
  FUIPanGestureRecognizer            := nil;
  FDPFUIPanGestureRecognizerDelegate := nil;
  // FDPFAnimationDelegate := TDPFAnimationDelegate.Create( Self );
{$ENDIF}
end;

procedure TDPFiOSBaseControl.CreateUIControl;
begin
  // Create UI control in derived function. This function should be similar to VCL CreateWindowHandle
  //
  // Example:
  // if not Assigned(FUIControl) then
  // begin
  // FUIControl := TUIControl.Create;
  // end;
end;

// ------------------------------------------------------------------------------
destructor TDPFiOSBaseControl.Destroy;
{$IFDEF IOS}
var
  C : UIView;
  CV: UIViewController;
{$ENDIF}
begin
  FDestroying := true;
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.Destroy: ' + ClassName + ' Name: ' + name + ' Unit: ' + Self.UnitName );
  if Assigned( FShadowLayer ) then
    FShadowLayer.release;

  DestroyUIControl;

  C := GetControlView( FUIControl );
  if Assigned( FDPFUIPanGestureRecognizerDelegate ) then
  begin
    C.removeGestureRecognizer( FUIPanGestureRecognizer );
    FUIPanGestureRecognizer.release;
    FDPFUIPanGestureRecognizerDelegate.DisposeOf;
  end;

  if not AutoRelease and Assigned( FUIControl ) then
  begin
    // FUIControl.retainCount
    CV := GetControlViewController( FUIControl );
    if Assigned( CV ) then
    begin
      if assigned( CV.parentViewController ) then
      begin
        if FAddChildViewController then
          CV.dismissModalViewControllerAnimated( false );
        CV.willMoveToParentViewController( nil );
        CV.removeFromParentViewController;
      end;
      CV.view.removeFromSuperview;
      CV.release;
    end
    else
    begin
      if Assigned( C ) then
      begin
        C.removeFromSuperview;
        C.release;
      end;
    end;

    FUIControl := nil;
  end;

  try
    if Assigned( FUIControl ) then
    begin
      // rc := FUIControl.retainCount;
      // while AutoRelease and ( FUIControl.retainCount > 1 ) do
      (* if AutoRelease and ( FUIControl.retainCount > 1 ) Then
        begin
        FUIControl.release;
        FUIControl := nil ;
        end ; *)
    end;
  except
    on e: exception do
  end;

  // FDPFAnimationDelegate.DisposeOf;

{$ENDIF}
  FFont.Free;
  inherited;
end;

procedure TDPFiOSBaseControl.DestroyUIControl;
begin
  // to be overridden
end;

procedure TDPFiOSBaseControl.DoAbsoluteChanged;
begin
  inherited;
  UpdateUIControlPosition;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.clearAllSubviews( Control: NSObject = nil; isRelease: Boolean = false );
var
  C: UIView;
begin
  DPFNSLog( 'TDPFiOSBaseControl.clearAllSubviews' + name );
  if Control = nil then
    Control := FUIControl;

  C := GetControlView( Control );
  while C.subviews.count > 0 do
  begin
    TUIView.Wrap( C.subviews.objectAtIndex( 0 ) ).removeFromSuperview;
    if isRelease then
      TUIView.Wrap( C.subviews.objectAtIndex( 0 ) ).release;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.InternalPaint( Text: string; TextColor: TAlphaColor; TextAlign: TDPFTextAlignment; BackgroundColor: TAlphaColor );
{$IFNDEF IOS}
var
  R: TRectF;
begin
  Canvas.BeginScene;
  Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  Canvas.Font.Size   := Font.FontSize;
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Kind  := TBrushKind.Solid;
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, 0.5, 0.5, AllCorners, Alpha, TCornerType.InnerRound );
  end;
  // Canvas.DrawRect( ClipRect, 0, 0, [], 1 );
  // ---
  Canvas.Stroke.Kind := TBrushKind.Solid;
  if ( Text = '' ) and ( csDesigning in ComponentState ) and ( ChildrenCount = 0 ) then
  begin
    Canvas.Fill.Color := TAlphaColors.gray;
    Text              := name;
  end
  else
  begin
    Canvas.Fill.Color := TextColor;
  end;

  Canvas.Stroke.Color     := $B2005ACC;
  Canvas.Stroke.Cap       := TStrokeCap.Round;
  Canvas.Stroke.Join      := TStrokeJoin.Round;
  Canvas.Stroke.Dash      := TStrokeDash.Solid;
  Canvas.Stroke.Thickness := 1;
  R                       := LocalRect;
  InflateRect( R, -0.5, -0.5 );
  Canvas.DrawRect( R, 0.5, 0.5, AllCorners, Alpha, TCornerType.InnerRound );
  // ---

  Canvas.FillText( ClipRect, Text, true, Alpha, [], CTextAlign[TextAlign], TTextAlign.Center );
  Canvas.EndScene;
end;

{$ELSE}

begin
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.Loaded;
var
  s: string;
begin
  s := name;
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.Loaded: ' + ClassName + ' Name: ' + name + ' Unit: ' + Self.UnitName );
{$ENDIF}
  FisLoaded := true;
  inherited;
{$IFDEF IOS}
  ThisView := GetControlView( FUIControl );
  if ( index > -1 ) and assigned( ThisView.layer ) then
    ThisView.layer.setZPosition( index );

  SetBorderWidth( FBorderWidth );
  SetBorderColor( FBorderColor );
  SetCornerRadius( FCornerRadius );
  SetShadow( FShadow );
  SetContentMode( FContentMode );
  SetAlpha( FAlpha );
  SetVisible( FVisible );
  SetMasksToBounds( FMasksToBounds );

  SetClipsToBounds( FClipsToBounds );
  SetUserInteraction( FUserInteraction );
  SetTagNative( FTagNative );

  if Assigned( FOnPanGestureRecognizer ) and Assigned( ThisView ) then
  begin
    if not Assigned( FDPFUIPanGestureRecognizerDelegate ) then
    begin
      FDPFUIPanGestureRecognizerDelegate := TDPFUIPanGestureRecognizerDelegate.Create( Self );
      FUIPanGestureRecognizer            := TUIPanGestureRecognizer.Wrap( TUIPanGestureRecognizer.Alloc.initWithTarget( FDPFUIPanGestureRecognizerDelegate.GetObjectID, Sel_getUid( 'panGestureRecognizer:' ) ) );
      FUIPanGestureRecognizer.setDelegate( FDPFUIPanGestureRecognizerDelegate.GetObjectID );
      FUIPanGestureRecognizer.setMinimumNumberOfTouches( 1 );
      FUIPanGestureRecognizer.setMaximumNumberOfTouches( 1 );
      ThisView.addGestureRecognizer( FUIPanGestureRecognizer );
    end;
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFiOSBaseControl.Paint;
begin
  inherited Paint;
  InternalPaint( ControlCaption, TAlphaColors.Black, TDPFTextAlignment.taCenter, TAlphaColors.White );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.Invalidate;
begin
  InvalidateRect( RectF( 0, 0, width, height ) );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.PositionOnChange( Sender: TObject );
begin
  if Assigned( FOldPositionEvent ) then
    FOldPositionEvent( Sender );
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.ScaleOnChange( Sender: TObject );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
      C.setTransform( CGAffineTransformMakeScale( Scale.Point.X, Scale.Point.Y ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.RefreshNeeded;
begin
  // Must Be Empty !!!
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.Resize;
begin
  inherited Resize;
  UpdateUIControlPosition;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetAlpha( const Value: Single );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  if ( value > 1 ) or ( value < 0 ) then
    exit;
  FAlpha  := Value;
  Opacity := FAlpha;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
      C.setAlpha( FAlpha );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetAnimationTransition( HideView: TDPFiOSBaseControl; VisibleView: TDPFiOSBaseControl; Width: Single; Height: Single; AnimationSlideDir: TDPFViewAnimationSlideDir; AnimationDuration: double = 0.2 );
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.SetAnimationTransition : ' + name );
  HideView.Align      := TAlignLayout.None;
  VisibleView.Align   := TAlignLayout.None;
  VisibleView.Visible := true;
  HideView.Visible    := true;

  if AnimationSlideDir = TDPFViewAnimationSlideDir.vasRightToLeft then
  begin
    HideView.SetBounds( 0, 0, Width, Height );
    VisibleView.SetBounds( Width, 0, Width, Height );
  end
  else
  begin
    HideView.SetBounds( 0, 0, Width, Height );
    VisibleView.SetBounds( ( -1 * Width ), 0, Width, Height );
  end;

  TUIView.OCClass.beginAnimations( nil, nil );
  TUIView.OCClass.setAnimationDuration( AnimationDuration );
  if AnimationSlideDir = TDPFViewAnimationSlideDir.vasRightToLeft then
    HideView.SetBounds( ( -1 * Width ), 0, Width, Height )
  else
    HideView.SetBounds( Width, 0, Width, Height );

  VisibleView.SetBounds( 0, 0, Width, Height );
  TUIView.OCClass.commitAnimations;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetAnimationTransition( FromX, ToX, FromY, ToY, FromW, ToW, FromH, ToH: Single; AnimationDuration: double = 0.2; AnimationCallBack: TAnimationCallBack = nil );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.SetAnimationTransition : ' + name );
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
    begin
      // Align := TAlignLayout.None;

      SetBounds( ToX, ToY, ToW, ToH );
      C.setFrame( CGRectMake( FromX, FromY, FromW, FromH ) );

      TUIView.OCClass.beginAnimations( nil, nil );
      TUIView.OCClass.setAnimationDuration( AnimationDuration );
      // TUIView.OCClass.setAnimationDelay(0.1);
      // TUIView.OCClass.setAnimationDelegate( FDPFAnimationDelegate.GetObjectID );
      // TUIView.OCClass.setAnimationDidStopSelector( Sel_getUid( 'animationDidStop:' ) );
      // TUIView.OCClass.setAnimationWillStartSelector( Sel_getUid( 'animationDidStart:' ) );
      C.setFrame( CGRectMake( ToX, ToY, ToW, ToH ) );
      if Assigned( AnimationCallBack ) then
        AnimationCallBack;

      TUIView.OCClass.commitAnimations;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.setAutoresizingMask( const Value: Boolean );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FAutoresizingMask := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
    begin
      if Value then
        C.setAutoresizingMask( UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight )
      else
        C.setAutoresizingMask( UIViewAutoresizingNone );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.doCurl( toUp: Boolean );
{$IFDEF IOS}
var
  C        : UIView;
  animation: DPF.iOS.Classes.CATransition;
{$ENDIF}
begin
  Application.ProcessMessages;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
    begin
      animation := DPF.iOS.Classes.TCATransition.Wrap( TCATransition.OCClass.animation );

      // animation.setDelegate(self);
      animation.setDuration( 1.0 );
      animation.setTimingFunction( TCAMediaTimingFunction.Wrap( TCAMediaTimingFunction.OCClass.functionWithName( kCAMediaTimingFunctionEaseInEaseOut ) ) );

      if toUp then
      begin
        animation.setType( NSSTr( 'pageCurl' ) );
        animation.setEndProgress( 0.5 );
      end
      else
      begin
        animation.setType( NSSTr( 'pageUnCurl' ) );
        animation.setStartProgress( 0.5 );
      end;

      animation.setFillMode( kCAFillModeForwards );
      animation.setSubtype( kCATransitionFromBottom );
      animation.setRemovedOnCompletion( false );

      // C.exchangeSubviewAtIndex( 0, 1 );
      C.layer.addAnimation( CAAnimation( animation ), NSSTr( 'pageCurlAnimation' ) );

    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ No Need !
  procedure TDPFiOSBaseControl.DoRealign;
  begin
  Realign;
  end; }

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DoResize;
begin
  Resize;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
// - cameraIris
// - cameraIrisHollowClose
// - cameraIrisHollowOpen
// - genieEffect
// - unGenieEffect
// ? charminUltra
// - oglApplicationSuspend
// ? spewEffect
// - zoomyOut
// - zoomyIn
// - twist
// ? swirl
// ? tubey
// ? cube
// - flip
// - alignedFlip
// ? rotate
// - fade (kCATransitionFade)
// - moveIn (kCATransitionMoveIn)
// - oglFlip
// - pageCurl
// - pageUnCurl
// - push (kCATransitionPush)
// - reveal (kCATransitionReveal)
// - rippleEffect
// - suckEffect
// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetCustomEffect( const AnimationDuration: Single; EffectName: string; SubEffectName: string; WaitTime: Single );
var
  animation: DPF.iOS.Classes.CATransition;
  C        : UIView;
  dt       : TDateTime;
begin
  C := GetControlView( FUIControl );
  if C <> nil then
  begin
    animation := DPF.iOS.Classes.TCATransition.Wrap( TCATransition.OCClass.animation );
    animation.setDuration( AnimationDuration / 1000 );
    animation.setTimingFunction( TCAMediaTimingFunction.Wrap( TCAMediaTimingFunction.OCClass.functionWithName( kCAMediaTimingFunctionEaseInEaseOut ) ) );
    animation.setType( NSSTr( EffectName ) );
    if SubEffectName <> '' then
      animation.setSubtype( NSSTr( SubEffectName ) );
    animation.setRemovedOnCompletion( true );
    C.layer.addAnimation( CAAnimation( animation ), nil );

    if WaitTime > 0 then
    begin
      DT := now;
      while MilliSecondsBetween( Now, dt ) < WaitTime do
        Application.ProcessMessages;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetRippleEffect( const AnimationDuration: Single; const WaitTime: Single );
begin
  SetCustomEffect( AnimationDuration, 'rippleEffect', '', WaitTime );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetPulseEffect( const AnimationDuration: Single; const WaitTime: Single );
var
  animation: DPF.iOS.Classes.CABasicAnimation;
  C        : UIView;
  dt       : TDateTime;
begin
  C := GetControlView( FUIControl );
  if C <> nil then
  begin
    animation := DPF.iOS.Classes.TCABasicAnimation.Wrap( TCABasicAnimation.OCClass.animationWithKeyPath( NSStr( 'opacity' ) ) );
    animation.setDuration( AnimationDuration / 1000 );
    animation.setRepeatCount( 1000 );
    animation.setAutoreverses( true );
    animation.setFromValue( TNSNumber.OCClass.numberWithFloat( 1.0 ) );
    animation.setToValue( TNSNumber.OCClass.numberWithFloat( 0.0 ) );
    animation.setRemovedOnCompletion( true );
    C.layer.addAnimation( CAAnimation( animation ), NSStr( 'animateOpacity' ) );

    if WaitTime > 0 then
    begin
      DT := now;
      while MilliSecondsBetween( Now, dt ) < AnimationDuration do
        Application.ProcessMessages;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.StartAnimation( const AnimationDuration: Single );
begin
  TUIView.OCClass.beginAnimations( nil, nil );
  TUIView.OCClass.setAnimationDuration( AnimationDuration );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.StopAnimation;
begin
  TUIView.OCClass.commitAnimations;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DrawLine( FromPoint, ToPoint: DPFNSPoint; StrokeWidth: Single; Color: TAlphaColor );
var
  context: CGContextRef;
begin

  context := UIGraphicsGetCurrentContext( );
  CGContextSetStrokeColorWithColor( context, TColorToUIColor( Color ).CGColor );

  // Draw them with a stroke width
  CGContextSetLineWidth( context, StrokeWidth );
  CGContextMoveToPoint( context, FromPoint.x, FromPoint.y ); // start at this point
  CGContextAddLineToPoint( context, ToPoint.x, ToPoint.y ); // draw to this point

  // and now draw the Path!
  CGContextStrokePath( context );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DrawRect( Rect: DPFNSRect; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor );
var
  context: CGContextRef;
begin

  context := UIGraphicsGetCurrentContext( );
  CGContextSetStrokeColorWithColor( context, TColorToUIColor( StrokeColor ).CGColor );

  if FillColor <> TAlphaCOlors.Null then
  begin
    CGContextSetFillColorWithColor( context, TColorToUIColor( FillColor ).CGColor );
    CGContextFillRect( context, NSRect( Rect ) );
  end;

  // Draw them with a stroke width
  CGContextSetLineWidth( context, StrokeWidth );
  CGContextAddRect( context, NSRect( Rect ) ); //

  // and now draw the Path!
  CGContextStrokePath( context );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DrawArc( x: Single; y: Single; radius: Single; startAngle: Single; endAngle: Single; clockwise: Boolean; StrokeWidth: Single; Color: TAlphaColor );
var
  context: CGContextRef;
begin

  context := UIGraphicsGetCurrentContext( );
  CGContextSetStrokeColorWithColor( context, TColorToUIColor( Color ).CGColor );

  // Draw them with a stroke width
  CGContextSetLineWidth( context, StrokeWidth );
  CGContextAddArc( context, x, y, radius, DegToRad( startAngle ), DegToRad( endAngle ), Integer( clockwise ) );

  // and now draw the Path!
  CGContextStrokePath( context );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DrawEllipse( Rect: DPFNSRect; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor );
var
  context: CGContextRef;
begin

  context := UIGraphicsGetCurrentContext( );
  CGContextSetStrokeColorWithColor( context, TColorToUIColor( StrokeColor ).CGColor );

  // Draw them with a stroke width
  CGContextSetLineWidth( context, StrokeWidth );
  CGContextAddEllipseInRect( context, NSRect( Rect ) );

  if FillColor <> TAlphaCOlors.Null then
  begin
    CGContextSetFillColorWithColor( context, TColorToUIColor( FillColor ).CGColor );
    CGContextFillRect( context, NSRect( Rect ) );
  end;

  // and now draw the Path!
  CGContextStrokePath( context );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.DrawText( Text: string; x: Single; y: Single; StrokeWidth: Single; StrokeColor: TAlphaColor; FillColor: TAlphaColor; RotationDeg: Single = 0; DrawingMode: Longword = kCGTextFill; CharacterSpacing: Longword = 0; FontName: string = 'Helvetica'; FontSize: Integer = 14 );
var
  context  : CGContextRef;
  Transform: CGAffineTransform;
begin

  context := UIGraphicsGetCurrentContext( );
  CGContextSetStrokeColorWithColor( context, TColorToUIColor( StrokeColor ).CGColor );

  if FillColor <> TAlphaCOlors.Null then
    CGContextSetFillColorWithColor( context, TColorToUIColor( FillColor ).CGColor );

  CGContextSetLineWidth( context, StrokeWidth );
  CGContextSelectFont( context, NSStr( FontName ).UTF8String, FontSize, kCGEncodingMacRoman );
  CGContextSetCharacterSpacing( context, CharacterSpacing );
  CGContextSetTextDrawingMode( context, DrawingMode );

  Transform := CGAffineTransformMakeRotation( DegToRad( RotationDeg ) );
  CGContextSetTextMatrix( context, Transform );
  CGContextSetTextMatrix( context, CGAffineTransformMake( 1.0, 0.0, 0.0, -1.0, 0.0, 0.0 ) );
  CGContextShowTextAtPoint( context, x, y, NSStr( Text ).UTF8String, Length( Text ) );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetBorderColor( const Value: TAlphaColor );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  // if FBorderColor = Value then
  // exit;
  FBorderColor := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if isLoaded and Assigned( C ) and Assigned( C.layer ) then
    begin
      C.layer.setBorderWidth( FBorderWidth );
      C.layer.setBorderColor( TColorToUIColor( FBorderColor ).CGColor );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetBorderWidth( const Value: Integer );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  // if FBorderWidth = Value then
  // exit;
  FBorderWidth := Value;
{$IFDEF IOS}
  if isLoaded and Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) and Assigned( C.layer ) then
    begin
      C.layer.setBorderWidth( FBorderWidth );
      C.layer.setBorderColor( TColorToUIColor( FBorderColor ).CGColor );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetAnimationTransition( AnimationTransition: TDPFViewAnimationTransition; AnimationDuration: double = 0.2 );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.SetAnimationTransition : ' + name );
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if ( C <> nil ) and ( AnimationTransition <> vatNone ) then
    begin
      TUIView.OCClass.beginAnimations( nil, nil );
      TUIView.OCClass.setAnimationDuration( AnimationDuration );
      TUIView.OCClass.setAnimationTransition( Cardinal( AnimationTransition ), C, true );
      TUIView.OCClass.commitAnimations;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetClipsToBounds( const Value: Boolean );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FClipsToBounds := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
    begin
      C.setClipsToBounds( Value );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetContentMode( const Value: TDPFUIViewContentMode );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FContentMode := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if C <> nil then
      C.setContentMode( Integer( FContentMode ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetCornerRadius( const Value: Single );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  // if FCornerRadius = Value then
  // exit;
  FCornerRadius := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if isLoaded and Assigned( C ) and Assigned( C.layer ) then
    begin
      C.layer.setCornerRadius( FCornerRadius );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetFocus;
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
    begin
      C.becomeFirstResponder;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetFont( const Value: TDPFFont );
begin
  FFont.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetLockComponent( const Value: Boolean );
begin
  FLockComponent := Value;
  Self.Locked    := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetMasksToBounds( const Value: Boolean );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FMasksToBounds := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if isLoaded and Assigned( C ) and Assigned( C.layer ) then
    begin
      C.layer.setMasksToBounds( Value );
    end;
  end;
{$ENDIF}
end;

procedure TDPFiOSBaseControl.SetParent( const Value: TFmxObject );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  inherited SetParent( Value );

{$IFDEF IOS}
  if not Assigned( FUIControl ) and Assigned( Value ) then
  begin
    CreateUIControl;
  end;

  if Assigned( FUIControl ) then
  begin
    if Assigned( Value ) then
      AddSubView( Self, ParentControl )
    else
    begin
      C := GetControlView( FUIControl );
      C.removeFromSuperview;
    end;
  end;
{$ENDIF}
end;

procedure TDPFiOSBaseControl.SetParentComponent( Value: TComponent );
var
  R: IRoot;
begin
  // inherited SetParentComponent(Value);  //SZ: careful- removed inherited --> might need adjustments when upgrading Delphi version
  if Assigned( FParent ) then
    FParent.RemoveObject( Self );

  if Assigned( Value ) and ( Value is TFmxObject ) then
  begin
    // TFmxObject(Value).AddObject(Self);  // done in SetParent
    SetParent( TFmxObject( Value ) )
  end
  else if ( IInterface( Value ).QueryInterface( IRoot, R ) = 0 ) then
  begin
    R.AddObject( Self );
  end
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadow( const Value: Boolean );
{$IFDEF IOS}
var
  C : UIView;
  PC: UIView;
{$ENDIF}
begin
  FShadow := Value;
{$IFDEF IOS}
  C := GetControlView( FUIControl );
  // PC := GetControlView( FParentUIControl );
  if ( C <> nil ) and ( C.superview <> nil ) then
  begin
    if Value then
    begin
      if FBorderWidth = 0 then
      begin
        C.layer.setOpacity( Alpha );
        C.layer.setShadowColor( TColorToUIColor( FShadowColor ).CGColor );
        C.layer.setShadowOpacity( FShadowOpacity );
        C.layer.setShadowOffset( CGSizeMake( FShadowOffsetWidth, FShadowOffsetHeight ) );
        C.layer.setShadowRadius( FShadowRadius );
      end
      else
      begin
        if not Assigned( FShadowLayer ) then
          FShadowLayer := TCALayer.Create;
        FShadowLayer.setHidden( not Visible );
        FShadowLayer.setFrame( C.frame );
        FShadowLayer.setOpacity( Alpha );
        FShadowLayer.setCornerRadius( FShadowRadius );
        FShadowLayer.setBackgroundColor( TColorToUIColor( FShadowColor ).CGColor );
        FShadowLayer.setShadowColor( TColorToUIColor( FShadowColor ).CGColor );
        FShadowLayer.setShadowOpacity( FShadowOpacity );
        FShadowLayer.setShadowOffset( CGSizeMake( FShadowOffsetWidth, FShadowOffsetHeight ) );
        FShadowLayer.setShadowRadius( FShadowRadius );
        FShadowLayer.setZPosition( C.layer.zPosition - 1 );

        PC := TUIView.Wrap( C.superview );
        PC.layer.insertSublayer( FShadowLayer, C.layer );
      end;

    end
    else if not assigned( FShadowLayer ) then
    begin
      if FBorderWidth = 0 then
      begin
        // C.layer.setOpacity( 0 );
        C.layer.setShadowOpacity( 0 );
        C.layer.setCornerRadius( 0 );
        C.layer.setShadowColor( nil );
      end
    end
    else if assigned( FShadowLayer ) then
    begin
      if FBorderWidth = 0 then
      begin
        FShadowLayer.setOpacity( 0 );
        FShadowLayer.setCornerRadius( 0 );
        FShadowLayer.setShadowColor( nil );
      end
      else
      begin
        FShadowLayer.removeFromSuperlayer;
        FShadowLayer.release;
        FShadowLayer := nil;
      end;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadowColor( const Value: TAlphaColor );
begin
  FShadowColor := Value;
  SetShadow( FShadow );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadowOffsetHeight( const Value: Single );
begin
  FShadowOffsetHeight := Value;
  SetShadow( FShadow );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadowOffsetWidth( const Value: Single );
begin
  FShadowOffsetWidth := Value;
  SetShadow( FShadow );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadowOpacity( const Value: Single );
begin
  FShadowOpacity := Value;
  SetShadow( FShadow );
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetShadowRadius( const Value: Single );
begin
  FShadowRadius := Value;
  SetShadow( FShadow );
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

function TDPFiOSBaseControl.ViewWithTag( const Value: NativeInt ): UIView;
var
  C: UIView;
begin
  Result := nil;
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
      result := C.viewWithTag( Value );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetTagNative( const Value: NativeInt );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FTagNative := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
      C.setTag( value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetUserInteraction( const Value: Boolean );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FUserInteraction := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
      C.setUserInteractionEnabled( FUserInteraction );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.SetVisible( const Value: Boolean );
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
  FVisible := Value;
{$IFDEF IOS}
  if Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) and ( C.isHidden <> not FVisible ) then
    begin
      C.setHidden( not FVisible );
      SetShadow( FShadow );
    end;
  end;
{$ENDIF}
  inherited;
end;

procedure TDPFiOSBaseControl.UpdateUIControlPosition;
// SZ: moved this code from the Resize function in order to avoid triggering the "OnResize" event if only the iOS control position needs to be adjusted
{$IFDEF IOS}
var
  C: UIView;
{$ENDIF}
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFiOSBaseControl.UpdateUIControlPosition: ' + ClassName + ' Name: ' + name );
{$ENDIF}
{$IFDEF IOS}
  if { not FAutoresizingMask and } Assigned( FUIControl ) then
  begin
    C := GetControlView( FUIControl );
    if Assigned( C ) then
      C.setFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
    { if Shadow then
      SetShadow( true ); }
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFiOSBaseControl.Move;
begin
  inherited Move;
  UpdateUIControlPosition;
end;

// ------------------------------------------------------------------------------

{$IFDEF IOS}
// ------------------------------------------------------------------------------
(*
  { TDPFAnimationDelegate }
  procedure TDPFAnimationDelegate.animationDidStart( animationID: NSString; context: CGContextRef );
  begin
  DPFNSLog( 'TDPFAnimationDelegate.animationDidStart' );
  end;

  // ------------------------------------------------------------------------------
  procedure TDPFAnimationDelegate.animationDidStop( animationID: NSString; finished: NSNumber; context: CGContextRef );
  begin
  DPFNSLog( 'TDPFAnimationDelegate.animationDidStop' );
  end;

  // ------------------------------------------------------------------------------
  constructor TDPFAnimationDelegate.Create( ADPFiOSBaseControl: TDPFiOSBaseControl );
  begin
  inherited Create;
  FDPFiOSBaseControl := ADPFiOSBaseControl;
  end;

  // ------------------------------------------------------------------------------
  // function TDPFAnimationDelegate.GetObjectiveCClass: PTypeInfo;
  //  begin
  //  Result := TypeInfo( IDPFAnimationDelegate );
  //  end;
*)

// ------------------------------------------------------------------------------
constructor TDPFUIPanGestureRecognizerDelegate.Create( ADPFiOSBaseControl: TDPFiOSBaseControl );
begin
  inherited Create;
  FDPFiOSBaseControl := ADPFiOSBaseControl;
end;

// ------------------------------------------------------------------------------
{ function TDPFUIPanGestureRecognizerDelegate.gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldBeRequiredToFailByGestureRecognizer: UIGestureRecognizer ): Boolean; cdecl;
  begin
  result := true;
  end; }

// ------------------------------------------------------------------------------
{ function TDPFUIPanGestureRecognizerDelegate.gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch ): Boolean; cdecl;
  begin
  result := true;
  end; }

// ------------------------------------------------------------------------------
function TDPFUIPanGestureRecognizerDelegate.gestureRecognizer( gestureRecognizer: UIGestureRecognizer; shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer ): Boolean; cdecl;
begin
  result := FDPFiOSBaseControl.FPanSimultaneously;
end;

// ------------------------------------------------------------------------------
function TDPFUIPanGestureRecognizerDelegate.gestureRecognizerShouldBegin( gestureRecognizer: UIGestureRecognizer ): Boolean; cdecl;
begin
  result := FDPFiOSBaseControl.FPanRecognizerShouldBegin;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPanGestureRecognizerDelegate.panGestureRecognizer( sender: UIPanGestureRecognizer ); cdecl;
var
  velocity, center, translation: CGPoint;
  MoveCenter                   : Boolean;
begin
  if not Assigned( FDPFiOSBaseControl.ThisView ) then
    exit;

  velocity    := sender.velocityInView( FDPFiOSBaseControl.ThisView );
  translation := Sender.translationInView( FDPFiOSBaseControl.ThisView );

  if ( velocity.y > 0 ) and not( pgdDown in FDPFiOSBaseControl.PanGestureDirections ) then // panning down
    translation.y := 0;
  if ( velocity.y < 0 ) and not( pgdUp in FDPFiOSBaseControl.PanGestureDirections ) then // panning up
    translation.y := 0;

  if ( velocity.x > 0 ) and not( pgdRight in FDPFiOSBaseControl.PanGestureDirections ) then // panning right
    translation.x := 0;
  if ( velocity.x < 0 ) and not( pgdLeft in FDPFiOSBaseControl.PanGestureDirections ) then // panning left
    translation.x := 0;

  if ( Abs( velocity.x ) < FDPFiOSBaseControl.FPanVelocitySensitivity ) and not FDPFiOSBaseControl.FVelocityStarted then
    exit;

  FDPFiOSBaseControl.FVelocityStarted := true;

  sender.setTranslation( CGPointMake( 0, 0 ), FDPFiOSBaseControl.ThisView );

  center   := sender.view.center;
  center.y := center.y + translation.y;
  center.x := center.x + translation.x;

  MoveCenter := false;

  if Assigned( FDPFiOSBaseControl.FOnPanGestureRecognizer ) then
    FDPFiOSBaseControl.FOnPanGestureRecognizer( FDPFiOSBaseControl, DPFNSPoint( sender.view.frame.origin ), DPFNSPoint( translation ), DPFNSPoint( center ), DPFNSPoint( velocity ), TDPFPanGestureRecognizeState( sender.state ), MoveCenter );

  if ( sender.state = UIGestureRecognizerStateEnded ) or ( sender.state = UIGestureRecognizerStateCancelled ) or ( sender.state = UIGestureRecognizerStateFailed ) then
    FDPFiOSBaseControl.FVelocityStarted := False;

  if MoveCenter then
    sender.view.setCenter( center );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
initialization

RegisterClass( TDPFiOSBaseControl );
{$IFDEF IOS}
{$IFNDEF DEBUG}
NSLog( ( NSSTR( DPFAbout + ' v' + DPFVersion ) as ILocalObject ).GetObjectID );
{$ENDIF}
{$ENDIF}

// ------------------------------------------------------------------------------
end.
