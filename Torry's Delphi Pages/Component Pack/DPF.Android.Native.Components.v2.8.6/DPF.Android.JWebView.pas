// ------------------------------------------------------------------------------
// DPF.Android.JWebView Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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
unit DPF.Android.JWebView;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,
  System.TypInfo,

  DPF.Android.BaseControl,
  DPF.Android.Widget,
{$IFDEF ANDROID}
  Androidapi.JNI.Os,
  Androidapi.JNI.Net,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Webkit,
  FMX.Helpers.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

const
  Window_FEATURE_PROGRESS       = 2;
  Window_PROGRESS_VISIBILITY_ON = -1;
  MAX_VALUE                     = $7FFFFFFFFFFFFFFF;

type

  TDPFJWebView = class;

  TDPFWebHeaders = record
    Key: string;
    Value: string;
  end;

{$IFDEF ANDROID}

  TDPFOnWebViewListener = class( TJavaLocal, JDPFOnWebViewListener )
  private
    FDPFJWebView: TDPFJWebView;
  public
    constructor create( ADPFJWebView: TDPFJWebView );

    procedure doUpdateVisitedHistory( view: JWebView; url: JString; isReload: Boolean ); cdecl;
    procedure onFormResubmission( view: JWebView; dontResend: JMessage; resend: JMessage ); cdecl;
    procedure onLoadResource( view: JWebView; url: JString ); cdecl;
    procedure onPageFinished( view: JWebView; url: JString ); cdecl;
    procedure onPageStarted( view: JWebView; url: JString; favicon: JBitmap ); cdecl;
    procedure onReceivedError( view: JWebView; errorCode: Integer; description: JString; failingUrl: JString ); cdecl;
    procedure onReceivedHttpAuthRequest( view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString ); cdecl;
    procedure onReceivedSslError( view: JWebView; handler: JSslErrorHandler; error: JSslError ); cdecl;
    procedure onScaleChanged( view: JWebView; oldScale: Single; newScale: Single ); cdecl;
    procedure onUnhandledKeyEvent( view: JWebView; event: JKeyEvent ); cdecl;
    function shouldOverrideKeyEvent( view: JWebView; event: JKeyEvent ): Boolean; cdecl;
    function shouldOverrideUrlLoading( view: JWebView; url: JString ): Boolean; cdecl;
  end;
{$ENDIF}

  TDPFdoUpdateVisitedHistory    = procedure( Sender: TObject; url: string; isReload: Boolean ) of object;
  TDPFOnFormResubmission        = procedure( Sender: TObject ) of object;
  TDPFOnLoadResource            = procedure( Sender: TObject; url: string ) of object;
  TDPFOnPageFinished            = procedure( Sender: TObject; url: string ) of object;
  TDPFOnPageStarted             = procedure( Sender: TObject; url: string ) of object;
  TDPFOnReceivedError           = procedure( Sender: TObject; errorCode: Integer; description: string; failingUrl: string ) of object;
  TDPFOnReceivedHttpAuthRequest = procedure( Sender: TObject; host: string; realm: string ) of object;
  TDPFOnReceivedSslError        = procedure( Sender: TObject; error: integer ) of object;
  TDPFOnScaleChanged            = procedure( Sender: TObject; oldScale: Single; newScale: Single ) of object;
  TDPFonUnhandledKeyEvent       = procedure( Sender: TObject; keycode: integer; event: string ) of object;
  TDPFShouldOverrideKeyEvent    = function( Sender: TObject; keycode: integer; event: string ): Boolean of object;
  TDPFShouldOverrideUrlLoading  = function( Sender: TObject; url: string ): Boolean of object;

  // ----------------------------------------------------------------------------
  TDPFWebSettings = class( TPersistent )
  private
    FDPFJWebView                          : TDPFJWebView;
    FLoadsImagesAutomatically             : boolean;
    FDisplayZoomControls                  : boolean;
    FDefaultTextEncodingName              : string;
    FAllowContentAccess                   : boolean;
    FAppCacheEnabled                      : boolean;
    FMediaPlaybackRequiresUserGesture     : boolean;
    FBlockNetworkImage                    : boolean;
    FAllowFileAccessFromFileURLs          : boolean;
    FLoadWithOverviewMode                 : boolean;
    FJavaScriptCanOpenWindowsAutomatically: boolean;
    FAllowFileAccess                      : boolean;
    FDefaultFixedFontSize                 : Integer;
    FBuiltInZoomControls                  : boolean;
    FAllowUniversalAccessFromFileURLs     : boolean;
    FSaveFormData                         : boolean;
    FBlockNetworkLoads                    : boolean;
    FTextZoom                             : integer;
    FDefaultFontSize                      : Integer;
    FSupportMultipleWindows               : boolean;
    FUserAgentString                      : string;
    FSupportZoom                          : boolean;
    FJavaScriptEnabled                    : boolean;

  public
    constructor Create( ADPFJWebView: TDPFJWebView );
    destructor Destroy; override;
  published
    property AllowContentAccess                   : boolean read FAllowContentAccess write FAllowContentAccess default true;
    property AllowFileAccess                      : boolean read FAllowFileAccess write FAllowFileAccess default true;
    property AllowFileAccessFromFileURLs          : boolean read FAllowFileAccessFromFileURLs write FAllowFileAccessFromFileURLs default false;
    property AllowUniversalAccessFromFileURLs     : boolean read FAllowUniversalAccessFromFileURLs write FAllowUniversalAccessFromFileURLs default false;
    property AppCacheEnabled                      : boolean read FAppCacheEnabled write FAppCacheEnabled default false;
    property BlockNetworkImage                    : boolean read FBlockNetworkImage write FBlockNetworkImage default false;
    property BlockNetworkLoads                    : boolean read FBlockNetworkLoads write FBlockNetworkLoads default false;
    property BuiltInZoomControls                  : boolean read FBuiltInZoomControls write FBuiltInZoomControls default false;
    property DefaultFixedFontSize                 : Integer read FDefaultFixedFontSize write FDefaultFixedFontSize default 16;
    property DefaultFontSize                      : Integer read FDefaultFontSize write FDefaultFontSize default 16;
    property DefaultTextEncodingName              : string read FDefaultTextEncodingName write FDefaultTextEncodingName;
    property DisplayZoomControls                  : boolean read FDisplayZoomControls write FDisplayZoomControls default true;
    property JavaScriptCanOpenWindowsAutomatically: boolean read FJavaScriptCanOpenWindowsAutomatically write FJavaScriptCanOpenWindowsAutomatically default false;
    property JavaScriptEnabled                    : boolean read FJavaScriptEnabled write FJavaScriptEnabled default false;
    property LoadWithOverviewMode                 : boolean read FLoadWithOverviewMode write FLoadWithOverviewMode default false;
    property LoadsImagesAutomatically             : boolean read FLoadsImagesAutomatically write FLoadsImagesAutomatically default true;
    property MediaPlaybackRequiresUserGesture     : boolean read FMediaPlaybackRequiresUserGesture write FMediaPlaybackRequiresUserGesture default true;
    property SaveFormData                         : boolean read FSaveFormData write FSaveFormData default true;
    property SupportMultipleWindows               : boolean read FSupportMultipleWindows write FSupportMultipleWindows default false;
    property SupportZoom                          : boolean read FSupportZoom write FSupportZoom default true;
    property TextZoom                             : integer read FTextZoom write FTextZoom default 100;
    property UserAgentString                      : string read FUserAgentString write FUserAgentString;

    // property AppCacheMaxSize                      : int64 read FAppCacheMaxSize write FAppCacheMaxSize default $7FFFFFFFFFFFFFFF;
    // property AppCachePath : boolean read FAppCacheMaxSize Write FAppCacheMaxSize default  false;
    // property CacheMode : boolean read FCacheMode Write FCacheMode default  false;
    // property CursiveFontFamily(font: JString); cdecl;
    // property DatabaseEnabled(flag: Boolean); cdecl;
    // property DatabasePath(databasePath: JString); cdecl;
    // property DefaultZoom(zoom: JWebSettings_ZoomDensity); cdecl;
    // property LayoutAlgorithm(l: JWebSettings_LayoutAlgorithm); cdecl;
    // property DomStorageEnabled(flag: Boolean); cdecl;
    // property FantasyFontFamily(font: JString); cdecl;
    // property FixedFontFamily(font: JString); cdecl;
    // property GeolocationDatabasePath(databasePath: JString); cdecl;
    // property GeolocationEnabled(flag: Boolean); cdecl;
    // property LightTouchEnabled(enabled: Boolean); cdecl;
    // property UseWideViewPort : boolean read FSupportMultipleWindows Write FSupportMultipleWindows default true;
    // property TextSize(t: JWebSettings_TextSize); cdecl;//Deprecated
    // property SavePassword(save: Boolean); cdecl; deprecated in API level 18.
    // property SerifFontFamily(font: JString); cdecl;
    // property StandardFontFamily(font: JString); cdecl;
    // property MinimumFontSize(size: Integer); cdecl;
    // property MinimumLogicalFontSize(size: Integer); cdecl;
    // property NeedInitialFocus(flag: Boolean); cdecl;
    // property PluginState(state: JWebSettings_PluginState); cdecl; deprecated
    // property PluginsEnabled(flag: Boolean); cdecl;//Deprecated
    // property PluginsPath(pluginsPath: JString); cdecl;//Deprecated
    // property RenderPriority(priority: JWebSettings_RenderPriority); cdecl; deprecated;
    // property SansSerifFontFamily(font: JString); cdecl;
  end;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJWebView = class( TDPFANDBaseControl )
  private
    FOnScaleChanged           : TDPFOnScaleChanged;
    FOnReceivedSslError       : TDPFOnReceivedSslError;
    FOnPageFinished           : TDPFOnPageFinished;
    FShouldOverrideUrlLoading : TDPFShouldOverrideUrlLoading;
    FOnReceivedError          : TDPFOnReceivedError;
    FOnPageStarted            : TDPFOnPageStarted;
    FOnLoadResource           : TDPFOnLoadResource;
    FOnReceivedHttpAuthRequest: TDPFOnReceivedHttpAuthRequest;
    FOnFormResubmission       : TDPFOnFormResubmission;
    FonUnhandledKeyEvent      : TDPFonUnhandledKeyEvent;
    FShouldOverrideKeyEvent   : TDPFShouldOverrideKeyEvent;
    FdoUpdateVisitedHistory   : TDPFdoUpdateVisitedHistory;
    FWebSettings              : TDPFWebSettings;
    procedure SetWebSettings( const Value: TDPFWebSettings );

  protected
{$IFDEF ANDROID}
    FJDPFWebView         : JDPFWebView;
    FDPFOnWebViewListener: TDPFOnWebViewListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJWebView: JDPFWebView read FJDPFWebView;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure LoadURL( url: string; const WebHeaders: array of TDPFWebHeaders );
    procedure StopLoading;
    procedure GoBack;
    procedure GoForward;
  published
    property WebSettings: TDPFWebSettings read FWebSettings write SetWebSettings;

    property doUpdateVisitedHistory   : TDPFdoUpdateVisitedHistory read FdoUpdateVisitedHistory write FdoUpdateVisitedHistory;
    property OnFormResubmission       : TDPFOnFormResubmission read FOnFormResubmission write FOnFormResubmission;
    property OnLoadResource           : TDPFOnLoadResource read FOnLoadResource write FOnLoadResource;
    property OnPageFinished           : TDPFOnPageFinished read FOnPageFinished write FOnPageFinished;
    property OnPageStarted            : TDPFOnPageStarted read FOnPageStarted write FOnPageStarted;
    property OnReceivedError          : TDPFOnReceivedError read FOnReceivedError write FOnReceivedError;
    property OnReceivedHttpAuthRequest: TDPFOnReceivedHttpAuthRequest read FOnReceivedHttpAuthRequest write FOnReceivedHttpAuthRequest;
    property OnReceivedSslError       : TDPFOnReceivedSslError read FOnReceivedSslError write FOnReceivedSslError;
    property OnScaleChanged           : TDPFOnScaleChanged read FOnScaleChanged write FOnScaleChanged;
    property onUnhandledKeyEvent      : TDPFonUnhandledKeyEvent read FonUnhandledKeyEvent write FonUnhandledKeyEvent;
    property ShouldOverrideKeyEvent   : TDPFShouldOverrideKeyEvent read FShouldOverrideKeyEvent write FShouldOverrideKeyEvent;
    property ShouldOverrideUrlLoading : TDPFShouldOverrideUrlLoading read FShouldOverrideUrlLoading write FShouldOverrideUrlLoading;

    property Clickable;
    property Focusable;
    property FocusableInTouchMode;
    property BackgroundColor1;
    property BackgroundColor2;
    property BackgroundColor3;
    property BackgroundImage;
    property BorderWidth;
    property BorderColor;
    property BorderCornerRadius;
    property GradientOrientation;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
    property OnClick;
  end;

function MakeWebHeaders( Key: string; Value: string ): TDPFWebHeaders;

implementation

function MakeWebHeaders( Key: string; Value: string ): TDPFWebHeaders;
begin
  result.Key   := Key;
  result.Value := Value;
end;

// ------------------------------------------------------------------------------
{ TDPFJWebView }
constructor TDPFJWebView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'WebView';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FWebSettings     := TDPFWebSettings.Create( self );

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJDPFWebView := TJDPFWebView.JavaClass.init( SharedActivity );
      SharedActivity.getWindow.setFeatureInt( Window_FEATURE_PROGRESS, Window_PROGRESS_VISIBILITY_ON );
      FJDPFWebView.setFocusable( true );
      FJDPFWebView.setFocusableInTouchMode( true );

      FDPFOnWebViewListener := TDPFOnWebViewListener.create( self );
      FJDPFWebView.SetWebViewListener( FDPFOnWebViewListener );

      JControl := FJDPFWebView;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJWebView.Destroy;
begin
{$IFDEF ANDROID}
  FJDPFWebView := nil;
{$ENDIF}
  FWebSettings.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJWebView.Loaded;
begin
  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJWebView.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJWebView.SetWebSettings( const Value: TDPFWebSettings );
begin
  FWebSettings.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFJWebView.GoBack;
begin
{$IFDEF ANDROID}
  if assigned( FJDPFWebView ) then
    FJDPFWebView.goBack;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJWebView.GoForward;
begin
{$IFDEF ANDROID}
  if assigned( FJDPFWebView ) then
    FJDPFWebView.goForward;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJWebView.StopLoading;
begin
{$IFDEF ANDROID}
  if assigned( FJDPFWebView ) then
    FJDPFWebView.stopLoading;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJWebView.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJWebView.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJWebView.LoadURL( url: string; const WebHeaders: array of TDPFWebHeaders );
{$IFDEF ANDROID}
var
  WebSettings          : JWebSettings;
  additionalHttpHeaders: JHashMap;
  I                    : Integer;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if not Assigned( FJDPFWebView ) then
    exit;

  additionalHttpHeaders := nil;
  if Length( WebHeaders ) > 0 then
  begin
    additionalHttpHeaders := TJHashMap.JavaClass.init;

    for I := 0 to high( WebHeaders ) do
      additionalHttpHeaders.put( StringToJString( WebHeaders[I].Key ), StringToJString( WebHeaders[I].Value ) );
  end;

  CallInUIThread(
    procedure( )
    begin
      WebSettings := FJDPFWebView.getSettings;

      WebSettings.setLoadsImagesAutomatically( FWebSettings.FLoadsImagesAutomatically );
      WebSettings.setDisplayZoomControls( FWebSettings.DisplayZoomControls );
      WebSettings.setDefaultTextEncodingName( StringToJString( FWebSettings.DefaultTextEncodingName ) );
      WebSettings.setAllowContentAccess( FWebSettings.AllowContentAccess );
      WebSettings.setAppCacheEnabled( FWebSettings.AppCacheEnabled );
      WebSettings.setMediaPlaybackRequiresUserGesture( FWebSettings.MediaPlaybackRequiresUserGesture );
      WebSettings.setBlockNetworkImage( FWebSettings.BlockNetworkImage );
      WebSettings.setAllowFileAccessFromFileURLs( FWebSettings.AllowFileAccessFromFileURLs );
      WebSettings.setLoadWithOverviewMode( FWebSettings.LoadWithOverviewMode );
      WebSettings.setJavaScriptCanOpenWindowsAutomatically( FWebSettings.JavaScriptCanOpenWindowsAutomatically );
      WebSettings.setAllowFileAccess( FWebSettings.AllowFileAccess );
      WebSettings.setDefaultFixedFontSize( FWebSettings.DefaultFixedFontSize );
      WebSettings.setBuiltInZoomControls( FWebSettings.BuiltInZoomControls );
      WebSettings.setAllowUniversalAccessFromFileURLs( FWebSettings.AllowUniversalAccessFromFileURLs );
      WebSettings.setSaveFormData( FWebSettings.SaveFormData );
      WebSettings.setBlockNetworkLoads( FWebSettings.BlockNetworkLoads );
      WebSettings.setTextZoom( FWebSettings.TextZoom );
      WebSettings.setDefaultFontSize( FWebSettings.DefaultFontSize );
      WebSettings.setSupportMultipleWindows( FWebSettings.SupportMultipleWindows );
      WebSettings.setUserAgentString( StringToJString( FWebSettings.UserAgentString ) );
      WebSettings.setSupportZoom( FWebSettings.SupportZoom );
      WebSettings.setJavaScriptEnabled( FWebSettings.JavaScriptEnabled );

      if assigned( additionalHttpHeaders ) then
        FJDPFWebView.loadUrl( StringToJString( url ), JMap( additionalHttpHeaders ) )
      else
        FJDPFWebView.loadUrl( StringToJString( url ) );
      FJDPFWebView.requestFocusFromTouch;
    end );
{$ENDIF}
end;

{$IFDEF ANDROID}
// ------------------------------------------------------------------------------

{ TDPFOnWebViewListener }

constructor TDPFOnWebViewListener.create( ADPFJWebView: TDPFJWebView );
begin
  inherited create;
  FDPFJWebView := ADPFJWebView;
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.doUpdateVisitedHistory( view: JWebView; url: JString; isReload: Boolean );
begin
  if Assigned( FDPFJWebView.FdoUpdateVisitedHistory ) then
    FDPFJWebView.FdoUpdateVisitedHistory( FDPFJWebView, JStringToString( url ), isReload );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onFormResubmission( view: JWebView; dontResend, resend: JMessage );
begin
  if Assigned( FDPFJWebView.onFormResubmission ) then
    FDPFJWebView.onFormResubmission( FDPFJWebView );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onLoadResource( view: JWebView; url: JString );
begin
  if Assigned( FDPFJWebView.onLoadResource ) then
    FDPFJWebView.onLoadResource( FDPFJWebView, JStringToString( url ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onPageFinished( view: JWebView; url: JString );
begin
  if Assigned( FDPFJWebView.onPageFinished ) then
    FDPFJWebView.onPageFinished( FDPFJWebView, JStringToString( url ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onPageStarted( view: JWebView; url: JString; favicon: JBitmap );
begin
  if Assigned( FDPFJWebView.onPageStarted ) then
    FDPFJWebView.onPageStarted( FDPFJWebView, JStringToString( url ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onReceivedError( view: JWebView; errorCode: Integer; description, failingUrl: JString );
begin
  if Assigned( FDPFJWebView.onReceivedError ) then
    FDPFJWebView.onReceivedError( FDPFJWebView, errorCode, JStringToString( description ), JStringToString( failingUrl ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onReceivedHttpAuthRequest( view: JWebView; handler: JHttpAuthHandler; host, realm: JString );
begin
  if Assigned( FDPFJWebView.onReceivedHttpAuthRequest ) then
    FDPFJWebView.onReceivedHttpAuthRequest( FDPFJWebView, JStringToString( host ), JStringToString( realm ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onReceivedSslError( view: JWebView; handler: JSslErrorHandler; error: JSslError );
begin
  if Assigned( FDPFJWebView.onReceivedSslError ) then
    FDPFJWebView.onReceivedSslError( FDPFJWebView, error.getPrimaryError );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onScaleChanged( view: JWebView; oldScale, newScale: Single );
begin
  if Assigned( FDPFJWebView.onScaleChanged ) then
    FDPFJWebView.onScaleChanged( FDPFJWebView, oldScale, newScale );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnWebViewListener.onUnhandledKeyEvent( view: JWebView; event: JKeyEvent );
begin
  if Assigned( FDPFJWebView.onUnhandledKeyEvent ) then
    FDPFJWebView.onUnhandledKeyEvent( FDPFJWebView, event.getKeyCode, JStringToString( event.toString ) );
end;

// ------------------------------------------------------------------------------
function TDPFOnWebViewListener.shouldOverrideKeyEvent( view: JWebView; event: JKeyEvent ): Boolean;
begin
  result := false;
  if Assigned( FDPFJWebView.shouldOverrideKeyEvent ) then
    result := FDPFJWebView.shouldOverrideKeyEvent( FDPFJWebView, event.getKeyCode, JStringToString( event.toString ) );
end;

// ------------------------------------------------------------------------------
function TDPFOnWebViewListener.shouldOverrideUrlLoading( view: JWebView; url: JString ): Boolean;
begin
  result := false;
  if Assigned( FDPFJWebView.shouldOverrideUrlLoading ) then
    result := FDPFJWebView.shouldOverrideUrlLoading( FDPFJWebView, JStringToString( url ) );
end;

{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFWebSettings }

constructor TDPFWebSettings.Create( ADPFJWebView: TDPFJWebView );
begin
  inherited Create;
  FDPFJWebView                           := ADPFJWebView;
  FAllowContentAccess                    := true;
  FAllowFileAccess                       := true;
  FAllowFileAccessFromFileURLs           := false;
  FAllowUniversalAccessFromFileURLs      := false;
  FAppCacheEnabled                       := false;
  FBlockNetworkImage                     := false;
  FBlockNetworkLoads                     := false;
  FBuiltInZoomControls                   := false;
  FDefaultFixedFontSize                  := 16;
  FDefaultFontSize                       := 16;
  FDefaultTextEncodingName               := 'Latin-1';
  FDisplayZoomControls                   := true;
  FJavaScriptCanOpenWindowsAutomatically := false;
  FJavaScriptEnabled                     := false;
  FLoadWithOverviewMode                  := false;
  FLoadsImagesAutomatically              := true;
  FMediaPlaybackRequiresUserGesture      := true;
  FSaveFormData                          := true;
  FSupportMultipleWindows                := false;
  FSupportZoom                           := true;
  FTextZoom                              := 100;
  FUserAgentString                       := '';
end;

// ------------------------------------------------------------------------------
destructor TDPFWebSettings.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
end.
