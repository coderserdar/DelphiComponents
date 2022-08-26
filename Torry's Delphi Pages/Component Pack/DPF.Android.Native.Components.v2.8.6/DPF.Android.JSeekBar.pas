// ------------------------------------------------------------------------------
// DPF.Android.JSeekBar Component
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
unit DPF.Android.JSeekBar;

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

  TDPFJSeekBar = class;

{$IFDEF ANDROID}

  TJSeekBarOnSeekBarChangeListener = class( TJavaLocal, JSeekBar_OnSeekBarChangeListener )
  private
    FDPFJSeekBar: TDPFJSeekBar;
  public
    constructor create( ADPFJSeekBar: TDPFJSeekBar );

    procedure onProgressChanged( seekBar: JSeekBar; progress: integer; fromUser: boolean ); cdecl;
    procedure onStartTrackingTouch( seekBar: JSeekBar ); cdecl;
    procedure onStopTrackingTouch( seekBar: JSeekBar ); cdecl;
  end;
{$ENDIF}

  TDPFSeekBarOnProgressChanged    = procedure( Sender: TObject; progress: integer; fromUser: boolean ) of object;
  TDPFSeekBarOnStartTrackingTouch = procedure( Sender: TObject ) of object;
  TDPFSeekBarOnStopTrackingTouch  = procedure( Sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJSeekBar = class( TDPFANDBaseControl )
  private
    FProgress            : integer;
    FMax                 : integer;
    FOnStopTrackingTouch : TDPFSeekBarOnStopTrackingTouch;
    FOnStartTrackingTouch: TDPFSeekBarOnStartTrackingTouch;
    FOnProgressChanged   : TDPFSeekBarOnProgressChanged;
    procedure SetProgress( const Value: integer );
    procedure SetMax( const Value: integer );
  protected
{$IFDEF ANDROID}
    FJSeekBar                       : JSeekBar;
    FJSeekBarOnSeekBarChangeListener: TJSeekBarOnSeekBarChangeListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJSeekBar: JSeekBar read FJSeekBar;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property Progress: integer read FProgress write SetProgress default 0;
    property Max     : integer read FMax write SetMax default 100;

    property OnProgressChanged   : TDPFSeekBarOnProgressChanged read FOnProgressChanged write FOnProgressChanged;
    property OnStartTrackingTouch: TDPFSeekBarOnStartTrackingTouch read FOnStartTrackingTouch write FOnStartTrackingTouch;
    property OnStopTrackingTouch : TDPFSeekBarOnStopTrackingTouch read FOnStopTrackingTouch write FOnStopTrackingTouch;

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
{ TDPFJSeekBar }
constructor TDPFJSeekBar.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'SeekBar';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  FProgress        := 0;
  FMax             := 100;
  Height           := 32;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJSeekBar := TJSeekBar.JavaClass.init( SharedActivity { , nil  , 16842872 } );
      FJSeekBarOnSeekBarChangeListener := TJSeekBarOnSeekBarChangeListener.create( self );
      FJSeekBar.setOnSeekBarChangeListener( FJSeekBarOnSeekBarChangeListener );
    end );
  JControl := FJSeekBar;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJSeekBar.Destroy;
begin
{$IFDEF ANDROID}
  FJSeekBarOnSeekBarChangeListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJSeekBar.Loaded;
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
procedure TDPFJSeekBar.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJSeekBar.SetMax( const Value: integer );
begin
  FMax := Value;
{$IFDEF ANDROID}
  if assigned( FJSeekBar ) then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FJSeekBar.setMax( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJSeekBar.SetProgress( const Value: integer );
begin
  FProgress := Value;
{$IFDEF ANDROID}
  if assigned( FJSeekBar ) then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        FJSeekBar.setProgress( value );
        FJSeekBar.setSecondaryProgress( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJSeekBar.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJSeekBar.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TJSeekBarOnSeekBarChangeListener.create( ADPFJSeekBar: TDPFJSeekBar );
begin
  inherited create;
  FDPFJSeekBar := ADPFJSeekBar;
end;

// ------------------------------------------------------------------------------
procedure TJSeekBarOnSeekBarChangeListener.onProgressChanged( seekBar: JSeekBar; progress: integer; fromUser: boolean ); cdecl;
begin
  if Assigned( FDPFJSeekBar.OnProgressChanged ) then
    FDPFJSeekBar.OnProgressChanged( FDPFJSeekBar, progress, fromUser );
end;

// ------------------------------------------------------------------------------
procedure TJSeekBarOnSeekBarChangeListener.onStartTrackingTouch( seekBar: JSeekBar ); cdecl;
begin
  if Assigned( FDPFJSeekBar.OnStartTrackingTouch ) then
    FDPFJSeekBar.OnStartTrackingTouch( FDPFJSeekBar );
end;

// ------------------------------------------------------------------------------
procedure TJSeekBarOnSeekBarChangeListener.onStopTrackingTouch( seekBar: JSeekBar ); cdecl;
begin
  if Assigned( FDPFJSeekBar.OnStopTrackingTouch ) then
    FDPFJSeekBar.OnStopTrackingTouch( FDPFJSeekBar );
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
