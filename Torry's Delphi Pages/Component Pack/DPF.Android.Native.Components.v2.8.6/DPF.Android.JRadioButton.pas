// ------------------------------------------------------------------------------
// DPF.Android.JRadioButton Component
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
unit DPF.Android.JRadioButton;

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

  TDPFJRadioButton = class;

{$IFDEF ANDROID}

  TRadioButtonOnChangedListener = class( TJavaLocal, JCompoundButton_OnCheckedChangeListener )
  private
    FDPFJRadioButton: TDPFJRadioButton;
  public
    constructor create( ADPFJRadioButton: TDPFJRadioButton );
    procedure onCheckedChanged( buttonView: JCompoundButton; isChecked: boolean ); cdecl;
  end;
{$ENDIF}

  TDPFRadioButtonOnCheckedChanged = procedure( sender: TObject; isChecked: boolean ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJRadioButton = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJRadioButton                : JRadioButton;
    FRadioButtonOnChangedListener: TRadioButtonOnChangedListener;
{$ENDIF}
    FText            : string;
    FTextColor       : TAlphaColor;
    FNumberOfLines   : Integer;
    FTextAlignment   : TDPFTextAlignment;
    FTextSize        : Single;
    FAllCaps         : Boolean;
    FChecked         : Boolean;
    FOnCheckedChanged: TDPFRadioButtonOnCheckedChanged;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetTextSize( const Value: Single );
    procedure SetAllCaps( const Value: Boolean );
    procedure SetChecked( const Value: Boolean );
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
    procedure Toggle;
    function GetText: string;
  published
    property Text            : string read FText write SetText;
    property TextColor       : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize        : Single read FTextSize write SetTextSize;
    property NumberOfLines   : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment   : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps         : Boolean read FAllCaps write SetAllCaps default false;
    property Checked         : Boolean read FChecked write SetChecked default false;
    property OnCheckedChanged: TDPFRadioButtonOnCheckedChanged read FOnCheckedChanged write FOnCheckedChanged;

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
{ TDPFJRadioButton }
// ------------------------------------------------------------------------------
constructor TDPFJRadioButton.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'RadioButton';
  FTextColor     := TAlphaColors.Black;
  FNumberOfLines := 1;
  FTextAlignment := taAuto;
  FTextSize      := 21.0;
  FChecked       := false;

{$IFDEF ANDROID}
  try
    CallInUIThreadAndWaitFinishing(
      procedure( )
      begin
        FRadioButtonOnChangedListener := TRadioButtonOnChangedListener.create( self );
        FJRadioButton := TJRadioButton.JavaClass.init( SharedActivity );
        FJRadioButton.setOnCheckedChangeListener( FRadioButtonOnChangedListener );
        JControl := FJRadioButton;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJRadioButton.Destroy;
begin
{$IFDEF ANDROID}
  FRadioButtonOnChangedListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJRadioButton <> nil then
  begin
    // FJRadioButton.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

function TDPFJRadioButton.GetText: string;
begin
{$IFDEF ANDROID}
  result := {$IFDEF DELPHIXE6}JCharSequenceToStr{$ELSE}JCharSequenceToString{$ENDIF}( FJRadioButton.getText );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFJRadioButton.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJRadioButton.sizeToFit;
  result.width  := FJRadioButton.getWidth;
  result.height := FJRadioButton.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.Toggle;
begin
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setChecked( not FJRadioButton.isChecked );
      end );
  end;
  FChecked := FJRadioButton.isChecked;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJRadioButton.Loaded;
begin
  SetText( FText );
  SetTextColor( FTextColor );
  SetVisible( Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );
  SetChecked( FChecked );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

procedure TDPFJRadioButton.SetChecked( const Value: Boolean );
begin
  FChecked := Value;
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        // FJRadioButton.setChecked( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJRadioButton ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJRadioButton <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJRadioButton.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJRadioButton <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJRadioButton.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJRadioButton.Paint;
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
constructor TRadioButtonOnChangedListener.create( ADPFJRadioButton: TDPFJRadioButton );
begin
  inherited create;
  FDPFJRadioButton := ADPFJRadioButton;
end;

// ------------------------------------------------------------------------------
procedure TRadioButtonOnChangedListener.onCheckedChanged( buttonView: JCompoundButton; isChecked: boolean ); cdecl;
begin
  if assigned( FDPFJRadioButton.FOnCheckedChanged ) then
    FDPFJRadioButton.FOnCheckedChanged( FDPFJRadioButton, isChecked );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
