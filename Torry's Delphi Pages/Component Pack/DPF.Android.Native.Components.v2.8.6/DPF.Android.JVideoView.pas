// ------------------------------------------------------------------------------
// DPF.Android.JVideoView Component
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
unit DPF.Android.JVideoView;

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
  DPF.Android.Common,
  DPF.Android.Widget,
{$IFDEF ANDROID}
  Androidapi.JNI.Util,
  Androidapi.JNI.Net,
  Androidapi.JNI.VideoView,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJVideoView = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJVideoView = class( TDPFANDBaseControl )
  private
  protected
{$IFDEF ANDROID}
    FJVideoView     : JDPFVideoView;
    //FMediaController: JMediaController;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJVideoView: JDPFVideoView read FJVideoView;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Play( const URL: string );
  published

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

implementation

// ------------------------------------------------------------------------------
{ TDPFJVideoView }
constructor TDPFJVideoView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'VideoView';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      //FDialog := TJDialog.JavaClass.init(SharedActivity,-1);
      FJVideoView := TJDPFVideoView.JavaClass.init( SharedActivity );
      //FMediaController := TJMediaController.JavaClass.init( SharedActivity );
      JControl := FJVideoView;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJVideoView.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJVideoView.Loaded;
begin
  Resize;
  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJVideoView.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJVideoView.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJVideoView.Paint;
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
procedure TDPFJVideoView.Play( const URL: string );
{$IFDEF ANDROID}
{$ENDIF}
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    var
      Ur: Jnet_Uri;
      dm: JDisplayMetrics;
      h, w: Integer;
    begin
      FJVideoView.playVideo ;
      exit;

      // if FJVideoView.isPlaying then FJVideoView.stopPlayback;

      // SharedActivity.getWindow.setFormat(-3);

      dm := TJDisplayMetrics.JavaClass.init;
      SharedActivity.getWindowManager.getDefaultDisplay.getMetrics( dm );
      h := dm.heightPixels;
      w := dm.widthPixels;

      FJVideoView.setMinimumWidth( w );
      FJVideoView.setMinimumHeight( h );


      // FMediaController.setAnchorView( FJVideoView );
      // FMediaController.setMediaPlayer( JMediaController_MediaPlayerControl( FJvideoView ) );

      // Ur := TJnet_Uri.JavaClass.parse( StringToJString( 'http://download.wavetlan.com/SVV/Media/HTTP/3GP/HelixMobileProducer/HelixMobileProducer_test5_3GPv5_MPEG4SP_24bit_176x144_AR1.22_30fps_KFx_320kbps_AAC-LC_Mono_11025Hz_24kbps.3gp' ) );
      //Ur := TJnet_Uri.JavaClass.parse( StringToJString( 'http://techslides.com/demos/sample-videos/small.3gp' ) );
      //FJVideoView.setMediaController( FMediaController );
      //FJVideoView.setVideoURI( Ur );
      // FJVideoView.setVideoPath( StringToJString( 'http://techslides.com/demos/sample-videos/small.3gp' ) );
      // FJVideoView.setVideoPath(StringToJString( URL ));
      //FJVideoView.setZOrderOnTop(true);
      //FJVideoView.setZOrderMediaOverlay(true);
      FJVideoView.requestFocus( );
      //FJVideoView.start;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------

end.
