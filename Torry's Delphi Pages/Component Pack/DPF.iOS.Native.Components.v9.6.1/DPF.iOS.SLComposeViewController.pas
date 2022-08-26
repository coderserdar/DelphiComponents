// ------------------------------------------------------------------------------
// DPF.iOS.SLComposeViewController Component
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
unit DPF.iOS.SLComposeViewController;

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
  DPF.iOS.UIView,
  DPF.iOS.UIImageView,
{$IFDEF IOS6}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  libSocialFwk                           = '/System/Library/Frameworks/Social.framework/Social';
  SLComposeViewControllerResultCancelled = 0;
  SLComposeViewControllerResultDone      = 1;

type

  TDPFSLComposeViewController = class;

{$IFDEF IOS6}
  SLComposeViewController                   = interface;
  TSLComposeViewControllerResult            = Pointer;
  TSLComposeViewControllerCompletionHandler = procedure( SLComposeViewControllerResult: TSLComposeViewControllerResult ) of object;

  // ----------------------------------------------------------------------------
  // Required iOS 6.0 and later.
  SLComposeViewControllerClass = interface( UIViewControllerClass )
    ['{19FEE46C-C0C0-4CBD-A85E-2A45C7C2505D}']
    function isAvailableForServiceType( serviceType: NSString ): Boolean; cdecl;
    function composeViewControllerForServiceType( serviceType: NSString ): Pointer; cdecl;
  end;

  // ----------------------------------------------------------------------------
  // Required iOS 6.0 and later.
  SLComposeViewController = interface( UIViewController )
    ['{7B199691-9F57-495B-937E-498879E7BF10}']

    function serviceType: NSString; cdecl;

    function completionHandler: TSLComposeViewControllerCompletionHandler; cdecl;
    procedure setCompletionHandler( completionHandler: TSLComposeViewControllerCompletionHandler ); cdecl;

    function setInitialText( text: NSString ): Boolean; cdecl;
    function addImage( image: UIImage ): Boolean; cdecl;
    function removeAllImages: Boolean; cdecl;
    function addURL( url: NSURL ): Boolean; cdecl;
    function removeAllURLs: Boolean; cdecl;

  end;

  TSLComposeViewController = class( TOCGenericImport<SLComposeViewControllerClass, SLComposeViewController> )
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  TDPFOnComposeEvent = procedure( Sender: TObject; SLComposeViewControllerResult: Integer ) of object;

  // ----------------------------------------------------------------------------
  // Required iOS 6.0 and later.
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSLComposeViewController = class( TComponent )
  private
    FOnComposeEvent: TDPFOnComposeEvent;
{$IFDEF IOS6}
    // FDPSLComposeViewController: SLComposeViewController;
    // FDPFPopover               : UIPopoverController;
{$ENDIF}
  protected
    procedure presentViewControllerCompletion;
{$IFDEF IOS6}
    procedure SLComposeViewControllerCompletion( SLComposeViewControllerResult: TSLComposeViewControllerResult );
    function ComposeViewController( ServiceType: NSString; Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
{$ENDIF}
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function FacebookAvailable: Boolean;
    function FacebookPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;

    function TwitterAvailable: Boolean;
    function TwitterPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;

    function SinaWeiboAvailable: Boolean;
    function SinaWeiboPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
  published
    property OnComposeEvent: TDPFOnComposeEvent read FOnComposeEvent write FOnComposeEvent;
  end;

  // ------------------------------------------------------------------------------

implementation

{$IFDEF IOS6}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  libSocialModule: THandle;
{$ENDIF}

  // ------------------------------------------------------------------------------
function SLServiceTypeFacebook: NSString;
begin
  result := CocoaNSStringConst( libSocialFwk, 'SLServiceTypeFacebook' );
end;

// ------------------------------------------------------------------------------
function SLServiceTypeTwitter: NSString;
begin
  result := CocoaNSStringConst( libSocialFwk, 'SLServiceTypeTwitter' );
end;

// ------------------------------------------------------------------------------
function SLServiceTypeSinaWeibo: NSString;
begin
  result := CocoaNSStringConst( libSocialFwk, 'SLServiceTypeSinaWeibo' );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFSLComposeViewController }
// ------------------------------------------------------------------------------
constructor TDPFSLComposeViewController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
end;

// ------------------------------------------------------------------------------
destructor TDPFSLComposeViewController.Destroy;
begin
{$IFDEF IOS6}
  { if Assigned( FDPSLComposeViewController ) then
    FDPSLComposeViewController.release; }
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFSLComposeViewController.presentViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------
{$IFDEF IOS6}

procedure TDPFSLComposeViewController.SLComposeViewControllerCompletion( SLComposeViewControllerResult: TSLComposeViewControllerResult );
begin
  GetSharedApplication.keyWindow.rootViewController.dismissViewControllerAnimated( true, presentViewControllerCompletion );

  if Assigned( SLComposeViewControllerResult ) and Assigned( FOnComposeEvent ) then
    FOnComposeEvent( Self, Integer( SLComposeViewControllerResult ) );    //!! TODO x64
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.ComposeViewController( ServiceType: NSString; Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
var
  FDPSLComposeViewController: SLComposeViewController;
  NData                     : NSData;
begin
  result := true;
  try
    FDPSLComposeViewController := TSLComposeViewController.Wrap( TSLComposeViewController.OCClass.composeViewControllerForServiceType( ServiceType ) );
    if Text <> '' then
      FDPSLComposeViewController.setInitialText( NSStr( Text ) );

    if Assigned( ImageView ) then
      FDPSLComposeViewController.addImage( ImageView.GetUIImage )
    else if Image <> '' then
    begin
      if ImageisURL then
      begin
        NData := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( Image ) ) ) ) );
        FDPSLComposeViewController.addImage( TUIImage.Wrap( TUIImage.OCClass.imageWithData( NData ) ) )
      end
      else
        FDPSLComposeViewController.addImage( TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( Image ) ) ) );
    end;

    if URL <> '' then
      FDPSLComposeViewController.addURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( URL ) ) ) );

    FDPSLComposeViewController.setCompletionHandler( SLComposeViewControllerCompletion );
    GetSharedApplication.keyWindow.rootViewController.presentViewController( FDPSLComposeViewController, true, presentViewControllerCompletion );
  except
    result := false;
  end
end;

{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.FacebookAvailable: Boolean;
begin
{$IFDEF IOS6}
  Result := TSLComposeViewController.OCClass.isAvailableForServiceType( SLServiceTypeFacebook );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.FacebookPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
begin
{$IFDEF IOS6}
  Result := ComposeViewController( SLServiceTypeFacebook, Text, URL, Image, ImageisURL, ImageView );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.TwitterAvailable: Boolean;
begin
{$IFDEF IOS6}
  Result := TSLComposeViewController.OCClass.isAvailableForServiceType( SLServiceTypeTwitter );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.TwitterPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
begin
{$IFDEF IOS6}
  Result := ComposeViewController( SLServiceTypeTwitter, Text, URL, Image, ImageisURL, ImageView );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.SinaWeiboAvailable: Boolean;
begin
{$IFDEF IOS6}
  Result := TSLComposeViewController.OCClass.isAvailableForServiceType( SLServiceTypeSinaWeibo );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFSLComposeViewController.SinaWeiboPost( Text: string; URL: string; Image: string; ImageisURL: Boolean; ImageView: TDPFImageView = nil ): Boolean;
begin
{$IFDEF IOS6}
  Result := ComposeViewController( SLServiceTypeSinaWeibo, Text, URL, Image, ImageisURL, ImageView );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------

{$IFDEF IOS6}
{$IF defined(CPUARM)}
procedure LibSocialFakeLoader; cdecl; external libSocialFwk;
{$ELSE}

initialization

libSocialModule := dlopen( MarshaledAString( libSocialFwk ), RTLD_LAZY );

finalization

dlclose( libSocialModule );
{$ENDIF}
{$ENDIF}

end.
