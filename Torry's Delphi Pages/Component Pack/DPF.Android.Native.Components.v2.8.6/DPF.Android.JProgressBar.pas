// ------------------------------------------------------------------------------
// DPF.Android.JProgressBar Component
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
unit DPF.Android.JProgressBar;

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
{$IFDEF ANDROID}
  DPF.Android.Widget,
  Androidapi.JNI.Widget,
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

  TDPFJProgressBar = class;

{$IFDEF ANDROID}
{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJProgressBar = class( TDPFANDBaseControl )
  private
    FProgress: integer;
    FMax     : integer;
    procedure SetProgress( const Value: integer );
    procedure SetMax( const Value: integer );
  protected
{$IFDEF ANDROID}
    FJProgressBar: JProgressBar;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJProgressBar: JProgressBar read FJProgressBar;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property Progress: integer read FProgress write SetProgress default 0;
    property Max     : integer read FMax write SetMax default 100;

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
{ TDPFJProgressBar }
constructor TDPFJProgressBar.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'ProgressBar';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  FProgress        := 0;
  FMax             := 100;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      // -------------------------------------------------
      // progressBarStyleSmallTitle = 16843279
      // progressBarStyleSmallInverse = 16842873
      // progressBarStyleSmall = 16843400
      // progressBarStyleLargeInverse = 16843401
      // progressBarStyleLarge = 16842874
      // progressBarStyleInverse = 16843399
      // progressBarStyleHorizontal = 16842872
      // progressBarStyle = 16842871
      FJProgressBar := TJProgressBar.JavaClass.init( SharedActivity, nil, 16842872 );
      // FJProgressBar.setIndeterminate( true );
      FJProgressBar.setVisibility( 0 );
    end );
  JControl := FJProgressBar;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJProgressBar.Destroy;
begin
{$IFDEF ANDROID}
  { if Assigned( FTapGestureRecognizer ) then
    FJProgressBarGroup.removeGestureRecognizer( FTapGestureRecognizer );

    if Assigned( FDPFGestureRecognizerDelegate ) then
    FDPFGestureRecognizerDelegate.DisposeOf; }
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJProgressBar.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  SetMax( FMax );
  SetProgress( FProgress );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJProgressBar.Resize;
begin
{$IFDEF ANDROID}
  // if FJProgressBarGroup <> nil then FJProgressBarGroup.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressBar.SetMax( const Value: integer );
begin
  FMax := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressBar ) then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FJProgressBar.setMax( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressBar.SetProgress( const Value: integer );
begin
  FProgress := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressBar ) then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FJProgressBar.setProgress( value );
        FJProgressBar.setSecondaryProgress( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressBar.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJProgressBar.Paint;
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
