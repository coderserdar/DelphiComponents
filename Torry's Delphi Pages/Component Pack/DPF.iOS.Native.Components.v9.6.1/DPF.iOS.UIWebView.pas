// ------------------------------------------------------------------------------
// DPF.iOS.UIWebView Component
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
unit DPF.iOS.UIWebView;

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
  libWebKit = '/System/Library/Frameworks/WebKit.framework/WebKit';

type
  TDPFWeb = class;

  TDPFUIWebViewNavigationType = ( wntLinkClicked = 0, wntFormSubmitted = 1, wntBackForward = 2, wntTypeReload = 3, wntFormResubmitted = 4, wntTypeOther = 5 );

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  TDPFUIWebViewDelegate = class( TOCLocal, UIWebViewDelegate )
  private
    FDPFWeb: TDPFWeb;
  public
    constructor create( ADPFWeb: TDPFWeb );

    procedure webView( webView: UIWebView; didFailLoadWithError: NSError ); overload; cdecl;
    function webView( webView: UIWebView; shouldStartLoadWithRequest: NSURLRequest; navigationType: UIWebViewNavigationType ): Boolean; overload; cdecl;
    procedure webViewDidFinishLoad( webView: UIWebView ); cdecl;
    procedure webViewDidStartLoad( webView: UIWebView ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  TDPFOnFailLoad   = procedure( sender: TObject; Error: string ) of object;
  TDPFOnRequest    = procedure( sender: TObject; navigationType: TDPFUIWebViewNavigationType; RequestURL: string; var ShouldStart: Boolean ) of object;
  TDPFOnFinishLoad = procedure( sender: TObject ) of object;
  TDPFOnStartLoad  = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFWeb = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIWebView           : UIWebView;
    FDPFUIWebViewDelegate: TDPFUIWebViewDelegate;
{$ENDIF}
    FBackgroundColor: TAlphaColor;
    FOnFailLoad     : TDPFOnFailLoad;
    FOnRequest      : TDPFOnRequest;
    FOnFinishLoad   : TDPFOnFinishLoad;
    FOnStartLoad    : TDPFOnStartLoad;
    procedure SetBackgroundColor( const Value: TAlphaColor );

  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure LoadFromURL( URL: string );
    procedure LoadFromFile( FileName: string; MimeType: string; textEncoding: string );
    procedure LoadFromString( StringData: string );
  published
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;

    property OnFailLoad  : TDPFOnFailLoad read FOnFailLoad write FOnFailLoad;
    property OnRequest   : TDPFOnRequest read FOnRequest write FOnRequest;
    property OnFinishLoad: TDPFOnFinishLoad read FOnFinishLoad write FOnFinishLoad;
    property OnStartLoad : TDPFOnStartLoad read FOnStartLoad write FOnStartLoad;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFWeb }
// ------------------------------------------------------------------------------
constructor TDPFWeb.create( AOwner: TComponent );
begin
  inherited create( AOwner );
  ControlCaption   := 'Web View';
  FBackgroundColor := TAlphaColors.Null;

{$IFDEF IOS}
  FDPFUIWebViewDelegate := TDPFUIWebViewDelegate.create( self );
  FUIWebView            := TUIWebView.Wrap( TUIWebView.Alloc.initWithFrame( CGRectMake( Position.X, Position.Y, Width, Height ) ) );
  FUIControl            := FUIWebView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFWeb.Destroy;
begin
  inherited;
{$IFDEF IOS}
  FDPFUIWebViewDelegate.DisposeOf;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFWeb.LoadFromString( StringData: string );
{$IFDEF IOS}
{$ENDIF}
begin
{$IFDEF IOS}
  FUIWebView.loadHTMLString( NSStr( StringData ), nil );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFWeb.LoadFromURL( URL: string );
{$IFDEF IOS}
var
  NURL: NSURL;
{$ENDIF}
begin
{$IFDEF IOS}
  NURL := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( URL ) ) );
  FUIWebView.loadRequest( TNSURLRequest.Wrap( TNSURLRequest.OCClass.requestWithURL( NURL ) ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFWeb.LoadFromFile( FileName: string; MimeType: string; textEncoding: string );
{$IFDEF IOS}
var
  Data: NSData;
{$ENDIF}
begin
{$IFDEF IOS}
  Data := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfFile( NSStr( FileName ) ) );
  FUIWebView.loadData( Data, NSStr( MimeType ), NSStr( textEncoding ), TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( '/' ) ) ) );
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFWeb.Loaded;
begin
  FUIWebView.setHidden( not Visible );

  if FBackgroundColor = TAlphaColors.Null then
    FUIWebView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    FUIWebView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );


  FUIWebView.setDelegate( FDPFUIWebViewDelegate.GetObjectID );
  AddSubView( self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFWeb.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUIWebView <> nil then
    FUIWebView.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFWeb.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFWeb.Paint;
begin
  InternalPaint( '', TAlphaColors.White, TDPFTextAlignment.taCenter, FBackgroundColor );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFWeb.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIWebView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIWebView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIWebView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFUIWebViewDelegate }
constructor TDPFUIWebViewDelegate.create( ADPFWeb: TDPFWeb );
begin
  inherited create;
  FDPFWeb := ADPFWeb;
end;

procedure TDPFUIWebViewDelegate.webView( webView: UIWebView; didFailLoadWithError: NSError );
begin
  if Assigned( FDPFWeb.FOnFailLoad ) then
    FDPFWeb.FOnFailLoad( FDPFWeb, UTF8ToString( didFailLoadWithError.localizedDescription.UTF8String ) );
end;

// ------------------------------------------------------------------------------
function TDPFUIWebViewDelegate.webView( webView: UIWebView; shouldStartLoadWithRequest: NSURLRequest; navigationType: UIWebViewNavigationType ): Boolean;
var
  UR: string;
begin
  result := true;
  UR     := UTF8ToString( shouldStartLoadWithRequest.URL.absoluteString.UTF8String );
  // UR     := UTF8ToString( shouldStartLoadWithRequest.mainDocumentURL.absoluteString.UTF8String );
  if Assigned( FDPFWeb.FOnRequest ) then
    FDPFWeb.FOnRequest( FDPFWeb, TDPFUIWebViewNavigationType( navigationType ), UR, Result );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIWebViewDelegate.webViewDidFinishLoad( webView: UIWebView );
begin
  if Assigned( FDPFWeb.FOnFinishLoad ) then
    FDPFWeb.FOnFinishLoad( FDPFWeb );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIWebViewDelegate.webViewDidStartLoad( webView: UIWebView );
begin
  if Assigned( FDPFWeb.FOnStartLoad ) then
    FDPFWeb.FOnStartLoad( FDPFWeb );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
