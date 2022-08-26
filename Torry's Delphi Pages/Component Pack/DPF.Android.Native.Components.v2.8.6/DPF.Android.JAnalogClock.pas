// ------------------------------------------------------------------------------
// DPF.Android.JAnalogClock Component
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
unit DPF.Android.JAnalogClock;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.Android.BaseControl,
{$IFDEF ANDROID}
  DPF.Android.Widget,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  FMX.Platform.Android,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJAnalogClock = class;

{$IFDEF ANDROID}
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJAnalogClock = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJAnalogClock: JAnalogClock;
    // FJAnalogClockTouchListener        : TJAnalogClockTouchListener;
    // FJAnalogClockOnFocusChangeListener: TJAnalogClockOnFocusChangeListener;
{$ENDIF}
  protected
    procedure FontOnChanged( Sender: TObject );
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; override;
    function sizeToFit: DPFNSize;
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
    property Enabled;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFJAnalogClock }
// ------------------------------------------------------------------------------
constructor TDPFJAnalogClock.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'AnalogClock';

{$IFDEF ANDROID}
  try
    CallInUIThreadAndWaitFinishing(
      procedure( )
      begin
        FJAnalogClock := TJAnalogClock.JavaClass.init( SharedActivity );
        JControl := FJAnalogClock;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJAnalogClock.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJAnalogClock.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJAnalogClock <> nil then
  begin
    // FJAnalogClock.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFJAnalogClock.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJAnalogClock.sizeToFit;
  result.width  := FJAnalogClock.getWidth;
  result.height := FJAnalogClock.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJAnalogClock.Loaded;
begin
  AddSubView( Self, ParentControl );

  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      // FJAnalogClock.setFocusable( true );
      // FJAnalogClock.setFocusableInTouchMode( true );
      FJAnalogClock.setClickable( true );
      FJAnalogClock.setEnabled( true );
    end );


  // FJAnalogClockOnFocusChangeListener := TJAnalogClockOnFocusChangeListener.create( self );
  // FJAnalogClock.setOnFocusChangeListener( FJAnalogClockOnFocusChangeListener );

  SetVisible( Visible );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJAnalogClock.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJAnalogClock.Paint;
var
  Caption    : string;
  CaptionRect: TRectF;

begin
  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  if BackgroundColor1 <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackgroundColor1;
    Canvas.FillRect( ClipRect, 0, 0, AllCorners, Alpha, TCornerType.ctInnerRound );
  end;
  Caption           := name;
  Canvas.Fill.Color := TAlphaColors.Gray;
  // Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style := [TFontStyle.fsBold];
  // Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  CaptionRect := ClipRect;
  PaintCaption( Self, Caption, CaptionRect, TDPFLineBreak.lbWordWrap, 1, TTextAlign.taCenter );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
