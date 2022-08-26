// ------------------------------------------------------------------------------
// DPF.iOS.UIViewController Component
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
unit DPF.iOS.UIViewController;

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
  DPF.iOS.Classes,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  UIStatusBarStyleDefault          = 0;
  UIStatusBarStyleLightContent     = 1;
  UIStatusBarStyleBlackTranslucent = 2;
  UIStatusBarStyleBlackOpaque      = 3;

type

  TDPFUIViewController = class;

  TDPFViewControllerOrientation  = ( vcoPortrait, vcoLandscape, vcoInvertedPortrait, vcoInvertedLandscape );
  TDPFViewControllerOrientations = set of TDPFViewControllerOrientation;

  TDPFOnDidRotateFromInterfaceOrientation = procedure( Sender: TObject; Orientation: NativeUInt ) of object;

{$IFDEF IOS}

  // ----------------------------------------------------------------------------
  DPFViewController = interface( UIViewController )
    ['{E049B73C-FE7E-4931-8FBE-D88AA7199AA7}']
    function shouldAutorotate: Boolean; cdecl;
    function shouldAutorotateToInterfaceOrientation( AinterfaceOrientation: UIInterfaceOrientation ): Boolean; cdecl;
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    procedure didReceiveMemoryWarning; cdecl;
    procedure didRotateFromInterfaceOrientation( fromInterfaceOrientation: UIInterfaceOrientation ); cdecl;
    procedure viewDidAppear( animated: Boolean ); cdecl;
    procedure viewWillAppear( animated: Boolean ); cdecl;
    procedure viewWillDisappear( animated: Boolean ); cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    procedure willAnimateRotationToInterfaceOrientation( toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval ); cdecl;

    procedure viewDidLoad; cdecl;
    procedure viewDidUnload; cdecl;

    function statusBarStyle: UIStatusBarStyle; cdecl;

    function preferredStatusBarStyle: UIStatusBarStyle; cdecl; // iOS 7.0 and later
    function prefersStatusBarHidden: boolean; cdecl; // iOS 7.0 and later
    function preferredStatusBarUpdateAnimation: UIStatusBarAnimation; cdecl; // iOS 7.0 and later
  end;

  TDPFViewController = class( TOCLocal )
  private
    FDPFUIViewController: TDPFUIViewController;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create( ADPFUIViewController: TDPFUIViewController );

    function shouldAutorotate: Boolean; cdecl;
    function shouldAutorotateToInterfaceOrientation( AinterfaceOrientation: UIInterfaceOrientation ): Boolean; cdecl;
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    procedure didReceiveMemoryWarning; cdecl;
    procedure didRotateFromInterfaceOrientation( fromInterfaceOrientation: UIInterfaceOrientation ); cdecl;
    procedure viewDidAppear( animated: Boolean ); cdecl;
    procedure viewWillAppear( animated: Boolean ); cdecl;
    procedure viewWillDisappear( animated: Boolean ); cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    procedure willAnimateRotationToInterfaceOrientation( toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval ); cdecl;

    procedure viewDidLoad; cdecl;
    procedure viewDidUnload; cdecl;

    function statusBarStyle: UIStatusBarStyle; cdecl;

    function preferredStatusBarStyle: UIStatusBarStyle; cdecl; // iOS 7.0 and later
    function prefersStatusBarHidden: boolean; cdecl; // iOS 7.0 and later
    function preferredStatusBarUpdateAnimation: UIStatusBarAnimation; cdecl; // iOS 7.0 and later
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  TOnDidLoad                           = procedure( Sender: TObject ) of object;
  TOnDidUnload                         = procedure( Sender: TObject ) of object;
  TOnPrefersStatusBarHidden            = procedure( Sender: TObject; var isHidden: Boolean ) of object;
  TOnPreferredStatusBarStyle           = procedure( Sender: TObject; var StatusBarStyle: NativeUInt ) of object;
  TOnPreferredStatusBarUpdateAnimation = procedure( Sender: TObject; var StatusBarUpdateAnimation: NativeUInt ) of object;
  TOnReceiveMemoryWarning              = procedure( Sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIViewController = class( TDPFiOSBaseControl )
  private
    FEmbFrame                           : TFrame;
    FBackgroundColor                    : TAlphaColor;
    FForm                               : TForm;
    FBackgroundImage                    : string;
    FOnFormChanged                      : TDPFFormChanged;
    FOnFormChanging                     : TDPFFormChanging;
    FFrame                              : TDPFFrame;
    FOnFrameChanged                     : TDPFFrameChanged;
    FOnFrameChanging                    : TDPFFrameChanging;
    FOnDidLoad                          : TOnDidLoad;
    FOnDidUnload                        : TOnDidUnload;
    FOrientations                       : TDPFViewControllerOrientations;
    FOnDidRotateFromInterfaceOrientation: TDPFOnDidRotateFromInterfaceOrientation;
    FRootViewController                 : Boolean;
    FOnPrefersStatusBarHidden           : TOnPrefersStatusBarHidden;
    FOnPreferredStatusBarStyle          : TOnPreferredStatusBarStyle;
    FOnPreferredStatusBarUpdateAnimation: TOnPreferredStatusBarUpdateAnimation;
    FOnReceiveMemoryWarning             : TOnReceiveMemoryWarning;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetForm( const Value: TForm );
    procedure SetBackgroundImage( const Value: string );
    procedure SetFrame( const Value: TDPFFrame );

{$IFDEF IOS}
    // function GetUIViewController: DPFViewController;
{$ENDIF}
  protected
{$IFNDEF IOS}
    procedure Paint; override;
{$ENDIF}
    procedure Resize; override;
  public
{$IFDEF IOS}
    FOldRootViewController: UIViewController;
    FUIViewController     : UIViewController;
    FDPFViewController    : TDPFViewController;
    FScreenRect           : NSRect;
    UIWin                 : UIWindow;
    // property FUIViewController: DPFViewController read GetUIViewController;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function GetFrame: TFrame;
  published
    property BackgroundImage: string read FBackgroundImage write SetBackgroundImage;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;

    property RootViewController: Boolean read FRootViewController write FRootViewController default false;

    property Form        : TForm read FForm write SetForm;
    property Frame       : TDPFFrame read FFrame write SetFrame;
    property Orientations: TDPFViewControllerOrientations read FOrientations write FOrientations default [vcoPortrait, vcoLandscape, vcoInvertedPortrait, vcoInvertedLandscape];

    property OnFormChanging: TDPFFormChanging read FOnFormChanging write FOnFormChanging;
    property OnFormChanged : TDPFFormChanged read FOnFormChanged write FOnFormChanged;

    property OnFrameChanging: TDPFFrameChanging read FOnFrameChanging write FOnFrameChanging;
    property OnFrameChanged : TDPFFrameChanged read FOnFrameChanged write FOnFrameChanged;

    property OnDidRotateFromInterfaceOrientation: TDPFOnDidRotateFromInterfaceOrientation read FOnDidRotateFromInterfaceOrientation write FOnDidRotateFromInterfaceOrientation;
    property OnDidLoad                          : TOnDidLoad read FOnDidLoad write FOnDidLoad;
    property OnDidUnload                        : TOnDidUnload read FOnDidUnload write FOnDidUnload;
    property OnPrefersStatusBarHidden           : TOnPrefersStatusBarHidden read FOnPrefersStatusBarHidden write FOnPrefersStatusBarHidden;
    property OnPreferredStatusBarStyle          : TOnPreferredStatusBarStyle read FOnPreferredStatusBarStyle write FOnPreferredStatusBarStyle;
    property OnPreferredStatusBarUpdateAnimation: TOnPreferredStatusBarUpdateAnimation read FOnPreferredStatusBarUpdateAnimation write FOnPreferredStatusBarUpdateAnimation;
    property OnReceiveMemoryWarning             : TOnReceiveMemoryWarning read FOnReceiveMemoryWarning write FOnReceiveMemoryWarning;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

// ------------------------------------------------------------------------------
constructor TDPFUIViewController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption      := 'View Controller';
  FBackgroundColor    := TAlphaColors.Null;
  FOrientations       := [vcoPortrait, vcoLandscape, vcoInvertedPortrait, vcoInvertedLandscape];
  FRootViewController := false;

{$IFDEF IOS}
  UIWin                  := nil;
  FOldRootViewController := nil;
  if TOSVersion.Major >= 5 then
  begin
    FDPFViewController := TDPFViewController.Create( self );
    FUIViewController  := TUIViewController.Wrap( FDPFViewController.Super.init );
  end
  else
    FUIViewController := TUIViewController.Wrap( TUIViewController.Alloc.init );

  FUIViewController.setHidesBottomBarWhenPushed( false );
  FUIViewController.setWantsFullScreenLayout( true );

  FUIControl := FUIViewController;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIViewController.Destroy;
begin
  if Assigned( OnDidUnload ) then
    OnDidUnload( Self );
{$IFDEF IOS}
  if Assigned( FOldRootViewController ) then
  begin
    GetSharedApplication.keyWindow.setRootViewController( FOldRootViewController );
    GetSharedApplication.keyWindow.makeKeyAndVisible;
  end;

  FDPFViewController.DisposeOf;

  if Assigned( UIWin ) then
  begin
    UIWin.resignKeyWindow;
    UIWin.removeFromSuperview;
  end;
  FUIControl := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFUIViewController.GetFrame: TFrame;
begin
  result := FEmbFrame;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.Loaded;
begin
  FScreenRect := TUIScreen.Wrap( TUIScreen.OCClass.mainScreen ).bounds;
  Resize; // Very Important in Embbeded Forms, Frames
  if not isLoaded then
  begin
    if Assigned( OnDidLoad ) then
      OnDidLoad( Self );
    // FDPFViewController.viewDidLoad;
  end;

  FUIViewController.view.setHidden( not Visible );

  if FBackgroundImage <> '' then
  begin
    SetBackgroundImage( FBackgroundImage )
  end
  else
  begin
    SetBackgroundColor( FBackgroundColor );
  end;

  if FRootViewController then
  begin
    if not Assigned( UIWin ) then
      UIWin := TUIWindow.Wrap( TUIWindow.Alloc.initWithFrame( FScreenRect ) );
    UIWin.setAutoresizesSubviews( True );
    UIWin.setRootViewController( FUIViewController );
    UIWin.makeKeyWindow;
  end
  else
    addSubview( Self, ParentControl, FUIViewController.view );

  // ----------------------------
  // Important
  if isRootControl then
    resize;

  inherited;
end;
{$ENDIF}
{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.Resize;
{$IFDEF IOS}
{$IFDEF DELPHIXE7}
var
  LOrientation: Cardinal;
  ASize       : TSizeF;
  tmpHeight   : Single;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF IOS}
{$IFDEF DELPHIXE7}
  if isRootControl and ( Align = TAlignLayout.Client ) then
  begin
    if TOSVersion.Major > 6 then
    begin
      LOrientation := GetSharedApplication.statusBarOrientation;
      if ( LOrientation = UIDeviceOrientationLandscapeLeft ) or ( LOrientation = UIDeviceOrientationLandscapeRight ) then
      begin
        tmpHeight     := FScreenRect.size.width;
        MinClipHeight := FScreenRect.size.width
      end
      else
      begin
        tmpHeight     := FScreenRect.size.height;
        MinClipHeight := FScreenRect.size.height;
      end;
      ASize.Width  := Size.Width;
      ASize.Height := tmpHeight;
      Size.SetSizeWithoutNotification( ASize );
      if FUIViewController <> nil then
        FUIViewController.view.setFrame( CGRectMake( Position.X, Position.Y, ASize.Width * Scale.X, ASize.Height * Scale.Y ) );
      exit;
    end;
  end;
{$ELSE}
  if isRootControl and ( Align = TAlignLayout.Client ) then
  begin
    if TOSVersion.Major > 6 then
    begin
      LOrientation := GetSharedApplication.statusBarOrientation;
      if ( LOrientation = UIDeviceOrientationLandscapeLeft ) or ( LOrientation = UIDeviceOrientationLandscapeRight ) then
      begin
        FHeight       := FScreenRect.size.width;
        MinClipHeight := FScreenRect.size.width
      end
      else
      begin
        FHeight       := FScreenRect.size.height;
        MinClipHeight := FScreenRect.size.height;
      end;
      if FUIViewController <> nil then
        FUIViewController.view.setFrame( CGRectMake( Position.X, Position.Y, FWidth * Scale.X, FHeight * Scale.Y ) );
      exit;
    end;
  end;
{$ENDIF}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if ( FUIViewController <> nil ) and ( FBackgroundImage = '' ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIViewController.view.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIViewController.view.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Image2: UIImage;
{$IFDEF IOSDEVICE}
{$ELSE}
  Image1   : UIImage;
  transform: CGAffineTransform;
  context  : CGContextRef;
{$ENDIF}
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if FUIViewController <> nil then
  begin
    Image2 := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FBackgroundImage ) ) );

    // ----------------------------------------------------------
    // Mirror on Simulator
{$IFNDEF IOSDEVICE}
    Image1 := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );
    UIGraphicsBeginImageContext( FUIViewController.view.frame.size );
    context   := UIGraphicsGetCurrentContext( );
    transform := CGAffineTransformMakeTranslation( 0.0, Height );
    transform := CGAffineTransformScale( transform, 1.0, -1.0 );
    CGContextConcatCTM( context, transform );
    Image1.drawInRect( FUIViewController.view.bounds );

    Image2 := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
    UIGraphicsEndImageContext( );
{$ENDIF}
    FUIViewController.view.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.colorWithPatternImage( Image2 ) ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.SetForm( const Value: TForm );
{$IFDEF IOS}
var
  C        : TDPFiOSBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin

{$IFDEF IOS}
  if ( FUIViewController <> nil ) and ( ( FForm <> Value ) or ( Assigned( Value ) and not Assigned( Value.TagObject ) ) ) then
  begin
    CanChange := True;
    if Assigned( FOnFormChanging ) then
      FOnFormChanging( Self, FForm, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FForm ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews( );
      FForm.TagObject := nil;
    end;

    if Assigned( Value ) then
    begin

      if Value.TagObject <> nil then
        MoveChildsToOriginParent( Value.TagObject as TDPFiOSBaseControl );

      while Value.ChildrenCount <> 0 do
        if Value.Children[0] is TDPFiOSBaseControl then
        begin
          C        := Value.Children[0] as TDPFiOSBaseControl;
          C.Parent := Self;
          C.Loaded;
          Value.TagObject := Self;
        end;
    end;
    if Assigned( FOnFormChanged ) then
      FOnFormChanged( Self, FForm, Value );
  end;
{$ENDIF}
  FForm := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIViewController.SetFrame( const Value: TDPFFrame );
{$IFDEF IOS}
var
  C        : TFMXObject { TDPFiOSBaseControl };
  CanChange: Boolean;
{$ENDIF}
begin
  FFrame := Value;
{$IFDEF IOS}
  if Assigned( FEmbFrame ) and ( FEmbFrame is Value ) then
    exit;

  if ( FUIViewController <> nil ) then
  begin
    CanChange := True;
    if Assigned( FOnFrameChanging ) then
      FOnFrameChanging( Self, FFrame, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FEmbFrame ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews(  );
      FEmbFrame.TagObject := nil;
      FEmbFrame.DisposeOf;
      FEmbFrame := nil;
    end;
    if Assigned( Value ) then
    begin
      FEmbFrame := Value.Create( nil );

      while FEmbFrame.ChildrenCount <> 0 do
        if FEmbFrame.Children[0] is TFMXObject then // Fixed By Paul
        begin
          C        := FEmbFrame.Children[0] as TFMXObject; // Fixed By Paul
          C.Parent := Self;
          if C is TDPFiOSBaseControl then // Fixed By Paul
            ( C as TDPFiOSBaseControl ).Loaded;
        end;
    end;
    if Assigned( FOnFrameChanged ) then
      FOnFrameChanged( Self, FFrame, Value );
  end;
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFViewController }
constructor TDPFViewController.Create( ADPFUIViewController: TDPFUIViewController );
var
  V: Pointer;
begin
  inherited Create;
  FDPFUIViewController := ADPFUIViewController;
  V                    := UIViewController( Super ).initWithNibName( nil { NSSTR( FDPFUIViewController.Name ) SZ: declined: we don't use a *.nib file in delphi } , nil );
  if GetObjectID <> V then
    UpdateObjectID( V );

end;

// ------------------------------------------------------------------------------
function TDPFViewController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFViewController );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewDidLoad;
begin
  if Assigned( FDPFUIViewController.OnDidLoad ) then
    FDPFUIViewController.OnDidLoad( FDPFUIViewController );
  UIViewController( Super ).viewDidLoad;
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewDidUnload;
begin
  if Assigned( FDPFUIViewController.OnDidUnload ) then
    FDPFUIViewController.OnDidUnload( FDPFUIViewController );
  UIViewController( Super ).viewDidUnload;
end;

// ------------------------------------------------------------------------------
// Result :
// UIStatusBarStyleDefault = 0
// UIStatusBarStyleLightContent = 1
// UIStatusBarStyleBlackTranslucent = 2
// UIStatusBarStyleBlackOpaque = 3
// ------------------------------------------------------------------------------

function TDPFViewController.statusBarStyle: UIStatusBarStyle; cdecl;
begin
  DPFNSLog( 'TDPFViewController.statusBarStyle' );
  result := UIStatusBarStyleBlackOpaque;
  if Assigned( FDPFUIViewController.FOnPreferredStatusBarStyle ) then
    FDPFUIViewController.FOnPreferredStatusBarStyle( FDPFUIViewController, NativeUInt( result ) );
end;

// ------------------------------------------------------------------------------
// Result :
// UIStatusBarStyleDefault = 0
// UIStatusBarStyleLightContent = 1
// UIStatusBarStyleBlackTranslucent = 2
// UIStatusBarStyleBlackOpaque = 3
// ------------------------------------------------------------------------------

function TDPFViewController.preferredStatusBarStyle: UIStatusBarStyle; cdecl;
begin
  DPFNSLog( 'TDPFViewController.preferredStatusBarStyle' );
  result := UIStatusBarStyleDefault;
  if Assigned( FDPFUIViewController.FOnPreferredStatusBarStyle ) then
    FDPFUIViewController.FOnPreferredStatusBarStyle( FDPFUIViewController, NativeUInt( result ) );
end;

// ------------------------------------------------------------------------------
function TDPFViewController.prefersStatusBarHidden: boolean; cdecl; // iOS 7.0 and later
begin
  DPFNSLog( 'TDPFViewController.prefersStatusBarHidden' );
  result := false;
  if assigned( FDPFUIViewController.OnPrefersStatusBarHidden ) then
    FDPFUIViewController.OnPrefersStatusBarHidden( FDPFUIViewController, result );

end;

// ------------------------------------------------------------------------------
// Result:
// UIStatusBarAnimationNone,
// UIStatusBarAnimationFade,
// UIStatusBarAnimationSlide,
// ------------------------------------------------------------------------------
{$IFNDEF XE5}

const
  UIStatusBarAnimationSlide = 2;
{$ENDIF}

function TDPFViewController.preferredStatusBarUpdateAnimation: UIStatusBarAnimation; cdecl; // iOS 7.0 and later
begin
  DPFNSLog( 'TDPFViewController.preferredStatusBarUpdateAnimation' );
  result := UIStatusBarAnimationSlide;
  if assigned( FDPFUIViewController.FOnPreferredStatusBarUpdateAnimation ) then
    FDPFUIViewController.FOnPreferredStatusBarUpdateAnimation( FDPFUIViewController, NativeUInt( result ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.didReceiveMemoryWarning;
begin
  DPFNSLog( 'TDPFViewController.preferredStatusBarUpdateAnimation' );
  UIViewController( Super ).didReceiveMemoryWarning;
  if assigned( FDPFUIViewController.FOnReceiveMemoryWarning ) then
    FDPFUIViewController.FOnReceiveMemoryWarning( FDPFUIViewController );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.didRotateFromInterfaceOrientation( fromInterfaceOrientation: UIInterfaceOrientation );
begin
  UIViewController( Super ).didRotateFromInterfaceOrientation( fromInterfaceOrientation );
  if Assigned( FDPFUIViewController.FOnDidRotateFromInterfaceOrientation ) then
    FDPFUIViewController.FOnDidRotateFromInterfaceOrientation( FDPFUIViewController, fromInterfaceOrientation );
end;

// ------------------------------------------------------------------------------
function TDPFViewController.shouldAutorotateToInterfaceOrientation( AinterfaceOrientation: UIInterfaceOrientation ): Boolean;
begin
  UIViewController( Super ).shouldAutorotateToInterfaceOrientation( AinterfaceOrientation );
  case AinterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := vcoLandscape in FDPFUIViewController.Orientations;
    UIInterfaceOrientationLandscapeRight:
      Result := vcoInvertedLandscape in FDPFUIViewController.Orientations;
    UIInterfaceOrientationPortrait:
      Result := vcoPortrait in FDPFUIViewController.Orientations;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := vcoInvertedPortrait in FDPFUIViewController.Orientations;
  else
    Result := False;
  end;

end;

// ------------------------------------------------------------------------------
function TDPFViewController.shouldAutorotate: boolean;
begin
  result := true;
end;

// ------------------------------------------------------------------------------
function TDPFViewController.supportedInterfaceOrientations: NSUInteger;
begin
  Result := 0;
  if vcoLandscape in FDPFUIViewController.Orientations then
    Result := Result or UIInterfaceOrientationMaskLandscapeLeft;
  if vcoInvertedLandscape in FDPFUIViewController.Orientations then
    Result := Result or UIInterfaceOrientationMaskLandscapeRight;
  if vcoPortrait in FDPFUIViewController.Orientations then
    Result := Result or UIInterfaceOrientationMaskPortrait;
  if vcoInvertedPortrait in FDPFUIViewController.Orientations then
    Result := Result or UIInterfaceOrientationMaskPortraitUpsideDown;
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewDidAppear( animated: Boolean );
begin
  UIViewController( Super ).viewDidAppear( animated );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewDidLayoutSubviews;
begin
  UIViewController( Super ).viewDidLayoutSubviews;
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewWillAppear( animated: Boolean );
begin
  UIViewController( Super ).viewWillAppear( animated );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.viewWillDisappear( animated: Boolean );
begin
  UIViewController( Super ).viewWillDisappear( animated );
end;

// ------------------------------------------------------------------------------
procedure TDPFViewController.willAnimateRotationToInterfaceOrientation( toInterfaceOrientation: UIInterfaceOrientation; duration: NSTimeInterval );
begin
  UIViewController( Super ).willAnimateRotationToInterfaceOrientation( toInterfaceOrientation, duration );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
