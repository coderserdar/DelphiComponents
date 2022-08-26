// ------------------------------------------------------------------------------
// DPF.iOS.ADBanner Component
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
unit DPF.iOS.ADBanner;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Forms,
  FMX.Types;

const
  libiAdFwk = '/System/Library/Frameworks/iAd.framework/iAd';

type

  TDPFADBanner = class;

{$IFDEF IOS}

  // ----------------------------------------------------------------------------
  ADBannerViewClass = interface( UIViewClass )
    ['{0E797791-9111-4DBE-92B2-4C03D4A1B904}']
  end;

  ADBannerView = interface( UIView )
    ['{DABEB3AF-911B-46DA-A06E-059F1ED2E0CF}']
    procedure setDelegate( newValue: Pointer ); cdecl;
    function delegate: Pointer; cdecl;
    function isBannerLoaded: Boolean; cdecl;

    procedure setRequiredContentSizeIdentifiers( newValue: NSSet ); cdecl;
    function requiredContentSizeIdentifiers: NSSet; cdecl;

    procedure setCurrentContentSizeIdentifier( newValue: NSString ); cdecl;
    function currentContentSizeIdentifier: NSString; cdecl;

    function sizeFromBannerContentSizeIdentifier( contentSizeIdentifier: NSString ): CGSize; cdecl;
    procedure setAdvertisingSection( newValue: NSString ); cdecl;
    function advertisingSection: NSString; cdecl;
    function isBannerViewActionInProgress: Boolean; cdecl;
    procedure cancelBannerViewAction; cdecl;
  end;

  TADBannerView = class( TOCGenericImport<ADBannerViewClass, ADBannerView> )
  end;

  // ----------------------------------------------------------------------------
  ADBannerViewDelegate = interface( IObjectiveC )
    ['{EA88286A-4653-4E0F-9A88-1D6DE20F2DF6}']

    procedure bannerViewWillLoadAd( banner: ADBannerView ); cdecl;
    procedure bannerViewDidLoadAd( banner: ADBannerView ); cdecl;
    procedure bannerView( banner: ADBannerView; didFailToReceiveAdWithError: NSError ); cdecl;
    function bannerViewActionShouldBegin( banner: ADBannerView; willLeaveApplication: Boolean ): Boolean; cdecl;
    procedure bannerViewActionDidFinish( banner: ADBannerView ); cdecl;
  end;

  TADBannerViewDelegate = class( TOCLocal, ADBannerViewDelegate )
  private
    FADBannerView: TDPFADBanner;
  public
    constructor Create( AADBannerView: TDPFADBanner );
    procedure bannerViewWillLoadAd( banner: ADBannerView ); cdecl;
    procedure bannerViewDidLoadAd( banner: ADBannerView ); cdecl;
    procedure bannerView( banner: ADBannerView; didFailToReceiveAdWithError: NSError ); cdecl;
    function bannerViewActionShouldBegin( banner: ADBannerView; willLeaveApplication: Boolean ): Boolean; cdecl;
    procedure bannerViewActionDidFinish( banner: ADBannerView ); cdecl;
  end;

{$ENDIF}

  TDPFADBannerError = procedure( Sender: TObject; AMessage: string ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFADBanner = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FADBanner            : ADBannerView;
    FADBannerViewDelegate: TADBannerViewDelegate;
{$ENDIF}
    FOnDidLoad          : TNotifyEvent;
    FOnError            : TDPFADBannerError;
    FOnActionShouldBegin: TNotifyEvent;
    FOnActionDidFinish  : TNotifyEvent;

  protected
    procedure Move; override;
  public
    procedure Resize; override;
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; override;
    procedure sizeToFit;
  published
    property OnDidLoad          : TNotifyEvent read FOnDidLoad write FOnDidLoad;
    property OnError            : TDPFADBannerError read FOnError write FOnError;
    property OnActionShouldBegin: TNotifyEvent read FOnActionShouldBegin write FOnActionShouldBegin;
    property OnActionDidFinish  : TNotifyEvent read FOnActionDidFinish write FOnActionDidFinish;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // function ADBannerContentSizeIdentifier320x50: NSString; // deprecated 'in iOS 4.2 and later';
  // function ADBannerContentSizeIdentifier480x32: NSString; // deprecated 'in iOS 4.2 and later';

{$IFDEF IOS}

function ADErrorDomain: NSString;
function ADBannerContentSizeIdentifierPortrait: NSString;
function ADBannerContentSizeIdentifierLandscape: NSString;
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iAdModule: THandle;
{$ENDIF}

  // ------------------------------------------------------------------------------
function ADErrorDomain: NSString;
begin
  Result := CocoaNSStringConst( libiAdFwk, 'ADErrorDomain' );
end;

// ------------------------------------------------------------------------------
function ADBannerContentSizeIdentifierPortrait: NSString;
begin
  Result := CocoaNSStringConst( libiAdFwk, 'ADBannerContentSizeIdentifierPortrait' );
end;

// ------------------------------------------------------------------------------
function ADBannerContentSizeIdentifierLandscape: NSString;
begin
  Result := CocoaNSStringConst( libiAdFwk, 'ADBannerContentSizeIdentifierLandscape' );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFADBanner }
// ------------------------------------------------------------------------------
constructor TDPFADBanner.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'iAd';

{$IFDEF IOS}
  FADBanner             := TADBannerView.Wrap( TADBannerView.Alloc.init );
  FUIControl            := FADBanner;
  FADBannerViewDelegate := TADBannerViewDelegate.Create( Self );
  FADBanner.setDelegate( FADBannerViewDelegate.GetObjectID );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFADBanner.Destroy;
begin
{$IFDEF IOS}
  if assigned( FADBannerViewDelegate ) then
    FADBannerViewDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFADBanner.Loaded;
begin
  FADBanner.setCurrentContentSizeIdentifier( ADBannerContentSizeIdentifierPortrait );
  resize;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFADBanner.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFADBanner.sizeToFit;
begin
{$IFDEF IOS}
  FADBanner.sizeToFit;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFADBanner.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFADBanner.Resize;
{$IFDEF IOS}
var
  ori                         : UIDeviceOrientation;
  sc                          : UIScreen;
  UIA                         : UIApplication;
  flag                        : Boolean;
  x                           : Single;
  _left, _top, _width, _height: Single;
{$ENDIF}
begin
  inherited;
{$IFDEF IOS}
  if FADBanner <> nil then
  begin
    sc    := TUIScreen.Wrap( TUIScreen.OCClass.mainScreen );
    ori   := TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).orientation;
    UIA   := GetSharedApplication;
    x     := 20;
    _left := 0;
    _top  := 0;
{$IFNDEF DELPHIXE6}
    if TOSVersion.Major < 7 then
{$ENDIF}
      if UIA.isStatusBarHidden then
        x := 0;
    flag  := ori in [UIDeviceOrientationUnknown, UIDeviceOrientationPortrait, UIDeviceOrientationPortraitUpsideDown];
    if IsIPad then
    begin
      _height := 66;
      if flag then
        _width := 768
      else
        _width := 1024;

    end
    else
    begin
      if flag then
      begin
        _width  := 320;
        _height := 50;
      end
      else
      begin
        _width  := 480;
        _height := 32;
      end;
    end;

    if flag then
    begin
      FADBanner.setCurrentContentSizeIdentifier( ADBannerContentSizeIdentifierPortrait );
      if align = TAlignLayout.alBottom then
        _top := sc.bounds.size.height - x - _height;
    end
    else
    begin
      FADBanner.setCurrentContentSizeIdentifier( ADBannerContentSizeIdentifierLandscape );
      if align = TAlignLayout.alBottom then
        _top := sc.bounds.size.width - x - _height;
    end;

    FADBanner.setFrame( CGRectMake( _left, _top, _width, _height ) );
    SetBounds( _left, _top, _width, _height );
  end;
{$ELSE}
  height := 50;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFADBanner.Paint;
begin
  InternalPaint( '', TAlphaColors.White, TDPFTextAlignment.taCenter, TAlphaColors.White );
end;
{$ENDIF}
{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TADBannerViewDelegate }
constructor TADBannerViewDelegate.Create( AADBannerView: TDPFADBanner );
begin
  inherited create;
  FADBannerView := AADBannerView;
end;

// ------------------------------------------------------------------------------
procedure TADBannerViewDelegate.bannerView( banner: ADBannerView; didFailToReceiveAdWithError: NSError );
var
  Err: string;
begin
  Err := '';
  if Assigned( didFailToReceiveAdWithError.localizedDescription ) then
    Err := UTF8ToString( didFailToReceiveAdWithError.localizedDescription.UTF8String );
  if Assigned( FADBannerView.OnError ) then
    FADBannerView.OnError( FADBannerView, Err );
end;

// ------------------------------------------------------------------------------
procedure TADBannerViewDelegate.bannerViewActionDidFinish( banner: ADBannerView );
begin
  if Assigned( FADBannerView.OnActionDidFinish ) then
    FADBannerView.OnActionDidFinish( FADBannerView );
end;

// ------------------------------------------------------------------------------
function TADBannerViewDelegate.bannerViewActionShouldBegin( banner: ADBannerView; willLeaveApplication: Boolean ): Boolean;
begin
  if Assigned( FADBannerView.OnActionShouldBegin ) then
    FADBannerView.OnActionShouldBegin( FADBannerView );
  Result := True;
end;

// ------------------------------------------------------------------------------
procedure TADBannerViewDelegate.bannerViewDidLoadAd( banner: ADBannerView );
begin
  if Assigned( FADBannerView.OnDidLoad ) then
    FADBannerView.OnDidLoad( FADBannerView );
end;

// ------------------------------------------------------------------------------
procedure TADBannerViewDelegate.bannerViewWillLoadAd( banner: ADBannerView );
begin
  FADBannerView.Resize;
end;

{$ENDIF}
// ------------------------------------------------------------------------------
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure iAdLoader; cdecl; external LibiAdFwk;
{$ELSE}

initialization

iAdModule := dlopen( MarshaledAString( LibiAdFwk ), RTLD_LAZY );

finalization

dlclose( iAdModule );
{$ENDIF}
{$ENDIF}

// ------------------------------------------------------------------------------
end.
