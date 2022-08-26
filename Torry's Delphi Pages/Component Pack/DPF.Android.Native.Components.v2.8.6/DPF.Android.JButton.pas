// ------------------------------------------------------------------------------
// DPF.Android.JButton Component
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
unit DPF.Android.JButton;

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

  TDPFJButton = class;

{$IFDEF ANDROID}
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJButton = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJButton: JButton;
    // FJButtonTouchListener        : TJButtonTouchListener;
    // FJButtonOnFocusChangeListener: TJButtonOnFocusChangeListener;
{$ENDIF}
    FText         : string;
    FTextColor    : TAlphaColor;
    FNumberOfLines: Integer;
    FTextAlignment: TDPFTextAlignment;
    FTextSize     : Single;
    FAllCaps      : Boolean;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetTextSize( const Value: Single );
    procedure SetAllCaps( const Value: Boolean );

  protected
    procedure FontOnChanged( Sender: TObject );
  public
    procedure Resize; override;
    procedure Move; override;
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
    property Text         : string read FText write SetText;
    property TextColor    : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize     : Single read FTextSize write SetTextSize;
    property NumberOfLines: Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment: TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps      : Boolean read FAllCaps write SetAllCaps default false;

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
{ TDPFJButton }
// ------------------------------------------------------------------------------
constructor TDPFJButton.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption       := 'Button';
  FTextColor           := TAlphaColors.Black;
  FNumberOfLines       := 1;
  FTextAlignment       := taAuto;
  FTextSize            := 18.0;
  Focusable            := false;
  FocusableInTouchMode := false;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJButton := TJButton.JavaClass.init( SharedActivity );
    end );
  JControl := FJButton;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJButton.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJButton <> nil then
  begin
    // FJButton.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

procedure TDPFJButton.Move;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJButton.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJButton.sizeToFit;
  result.width  := FJButton.getWidth;
  result.height := FJButton.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJButton.Loaded;
begin

  SetText( FText );
  SetTextColor( FTextColor );
  SetVisible( Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJButton.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.Resize;
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      if FJButton <> nil then
      begin
        FJButton.setWidth( round( Width * ScreenScale ) );
        FJButton.setHeight( round( height * ScreenScale ) );
      end;
    end );

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJButton.Paint;
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

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJButton <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJButton.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJButton <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJButton.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
