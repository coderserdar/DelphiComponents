// ------------------------------------------------------------------------------
// DPF.Android.JChronometer Component
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
unit DPF.Android.JChronometer;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.Android.BaseControl,
  DPF.Android.JButton,
{$IFDEF ANDROID}
  DPF.Android.Widget,
  DPF.Android.OS,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJChronometer = class;

{$IFDEF ANDROID}

  TDPFJChronometerTickListener = class( TJavaLocal, JChronometer_OnChronometerTickListener )
  private
    FDPFJChronometer: TDPFJChronometer;
  public
    constructor create( ADPFJChronometer: TDPFJChronometer );
    procedure onChronometerTick( chronometer: JChronometer ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJChronometer = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJChronometer: JChronometer;
{$ENDIF}
    FText             : string;
    FTextColor        : TAlphaColor;
    FNumberOfLines    : Integer;
    FTextAlignment    : TDPFTextAlignment;
    FTextSize         : Single;
    FAllCaps          : Boolean;
    FChronometerFormat: string;
    FOnChronometerTick: TNotifyEvent;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetTextSize( const Value: Single );
    procedure SetAllCaps( const Value: Boolean );
    procedure setChronometerFormat( const Value: string );
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
    procedure Start;
    procedure Stop;
  published
    property Text             : string read FText write SetText;
    property TextColor        : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize         : Single read FTextSize write SetTextSize;
    property NumberOfLines    : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment    : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps          : Boolean read FAllCaps write SetAllCaps default false;
    property ChronometerFormat: string read FChronometerFormat write setChronometerFormat;
    property OnChronometerTick: TNotifyEvent read FOnChronometerTick write FOnChronometerTick;

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
{ TDPFJChronometer }
// ------------------------------------------------------------------------------
constructor TDPFJChronometer.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption     := 'Chronometer';
  FTextColor         := TAlphaColors.Black;
  FNumberOfLines     := 1;
  FTextAlignment     := taAuto;
  FTextSize          := 21.0;
  FChronometerFormat := '%s';

{$IFDEF ANDROID}
  try
    CallInUIThreadAndWaitFinishing(
      procedure( )
      begin
        FJChronometer := TJChronometer.JavaClass.init( SharedActivity );
        JControl := FJChronometer;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJChronometer.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJChronometer <> nil then
  begin
    // FJChronometer.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFJChronometer.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJChronometer.sizeToFit;
  result.width  := FJChronometer.getWidth;
  result.height := FJChronometer.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.Start;
begin
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.start;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.Stop;
begin
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.stop;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJChronometer.Loaded;
begin
  SetText( FText );
  SetTextColor( FTextColor );
  SetVisible( Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );
  setChronometerFormat( FChronometerFormat );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
// %s
// Sample : curren (%s)
//
procedure TDPFJChronometer.setChronometerFormat( const Value: string );
begin
  FChronometerFormat := Value;
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setFormat( StringToJString( Value ) );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJChronometer ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJChronometer <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometer.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJChronometer <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJChronometer.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJChronometer.Paint;
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
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    Caption           := name;
    Canvas.Fill.Color := TAlphaColors.Gray;
  end
  else
  begin
    Caption           := Text;
    Canvas.Fill.Color := TextColor;
  end;
  // Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style := [TFontStyle.fsBold];
  // Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  CaptionRect := ClipRect;
  PaintCaption( Self, Caption, CaptionRect, TDPFLineBreak.lbWordWrap, NumberOfLines, CTextAlign[FTextAlignment] );
  Canvas.EndScene;
end;
{$ENDIF}
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TDPFJChronometerTickListener.create( ADPFJChronometer: TDPFJChronometer );
begin
  inherited create;
  FDPFJChronometer := ADPFJChronometer;
end;

// ------------------------------------------------------------------------------
procedure TDPFJChronometerTickListener.onChronometerTick( chronometer: JChronometer ); cdecl;
begin
  if Assigned( FDPFJChronometer.FOnChronometerTick ) then
    FDPFJChronometer.FOnChronometerTick( FDPFJChronometer );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
