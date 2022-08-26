// ------------------------------------------------------------------------------
// DPF.Android.JTextSwitcher Component
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
unit DPF.Android.JTextSwitcher;

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
  DPF.Android.JTextView,
{$IFDEF ANDROID}
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
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
  android_R_anim_anticipate_accelerate_decelerate_interpolator = 17432580;
  android_R_anim_anticipate_accelerate_interpolator = 17432581;
  android_R_anim_anticipate_anticipate_interpolator = 17432583;
  android_R_anim_anticipate_overshoot_interpolator  = 17432585;
  android_R_anim_bounce_interpolator                = 17432586;
  android_R_anim_cycle_interpolator                 = 17432588;
  android_R_anim_decelerate_interpolator            = 17432582;
  android_R_anim_fade_in                            = 17432576;
  android_R_anim_fade_out                           = 17432577;
  android_R_anim_linear_interpolator                = 17432587;
  android_R_anim_overshoot_interpolator             = 17432584;
  android_R_anim_slide_in_left                      = 17432578;
  android_R_anim_slide_out_right                    = 17432579;

type

  TDPFJTextSwitcher = class;

{$IFDEF ANDROID}
{$ENDIF}
  TDPFAnimationType = ( atFade, atSlide );

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJTextSwitcher = class( TDPFANDBaseControl )
  private
    FAnimationType: TDPFAnimationType;

  protected
{$IFDEF ANDROID}
    FJTextSwitcher : JTextSwitcher;
    AnimIn, AnimOut: jAnimation;
    FJTextView1    : JTextView;
    FJTextView2    : JTextView;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJTextSwitcher: JTextSwitcher read FJTextSwitcher;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetText( txt: string; TextSize: Single; TextColor: TAlphaColor; TextAlignment: TDPFTextAlignment; Duration: int64 );
  published
    property AnimationType: TDPFAnimationType read FAnimationType write FAnimationType default TDPFAnimationType.atSlide;

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
{ TDPFJTextSwitcher }
constructor TDPFJTextSwitcher.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'TextSwitcher';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FAnimationType   := TDPFAnimationType.atSlide;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJTextSwitcher := TJTextSwitcher.JavaClass.init( SharedActivity, nil );
    end );

  JControl := FJTextSwitcher;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJTextSwitcher.Destroy;
begin
{$IFDEF ANDROID}
  AnimIn      := nil;
  AnimOut     := nil;
  FJTextView1 := nil;
  FJTextView2 := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJTextSwitcher.Loaded;
begin

  FJTextView1 := TJTextView.JavaClass.init( SharedActivity );
  FJTextView1.setText( StrToJCharSequence( '' ) );
  FJTextSwitcher.addView( FJTextView1, 0, TJLinearLayout_LayoutParams.JavaClass.init( -1, -1 ) );

  FJTextView2 := TJTextView.JavaClass.init( SharedActivity );
  FJTextView2.setText( StrToJCharSequence( '' ) );
  FJTextSwitcher.addView( FJTextView2, 0, TJLinearLayout_LayoutParams.JavaClass.init( -1, -1 ) );

  if FAnimationType = atFade then
    AnimIn := TJAnimationUtils.JavaClass.loadAnimation( SharedActivity, android_R_anim_fade_in )
  else
    AnimIn := TJAnimationUtils.JavaClass.loadAnimation( SharedActivity, android_R_anim_slide_in_left );
  AnimIn.setDuration( 1000 );

  if FAnimationType = atFade then
    Animout := TJAnimationUtils.JavaClass.loadAnimation( SharedActivity, android_R_anim_fade_out )
  else
    Animout := TJAnimationUtils.JavaClass.loadAnimation( SharedActivity, android_R_anim_slide_out_right );
  Animout.setDuration( 1000 );

  FJTextSwitcher.setInAnimation( AnimIn );
  FJTextSwitcher.setOutAnimation( AnimOut );

  AddSubView( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJTextSwitcher.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextSwitcher.setText( txt: string; TextSize: Single; TextColor: TAlphaColor; TextAlignment: TDPFTextAlignment; Duration: int64 );
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FJTextView1.setGravity( TDPFTextAlignmentToGravity[TextAlignment] );
      FJTextView1.SetTextColor( TextColor );
      FJTextView1.setTextSize( TextSize );

      FJTextView2.setGravity( TDPFTextAlignmentToGravity[TextAlignment] );
      FJTextView2.SetTextColor( TextColor );
      FJTextView2.setTextSize( TextSize );

      AnimIn.setDuration( Duration );
      AnimOut.setDuration( Duration );

      FJTextSwitcher.setText( StrToJCharSequence( txt ) );
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextSwitcher.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJTextSwitcher.Paint;
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
end.
