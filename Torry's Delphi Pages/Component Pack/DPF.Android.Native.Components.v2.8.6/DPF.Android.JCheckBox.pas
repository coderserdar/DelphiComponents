// ------------------------------------------------------------------------------
// DPF.Android.JCheckBox Component
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
unit DPF.Android.JCheckBox;

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
  Androidapi.Helpers,
  FMX.Platform.Android,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJCheckBox = class;

{$IFDEF ANDROID}

  TCheckBoxOnChangedListener = class( TJavaLocal, JCompoundButton_OnCheckedChangeListener )
  private
    FDPFJCheckBox: TDPFJCheckBox;
  public
    constructor create( ADPFJCheckBox: TDPFJCheckBox );
    procedure onCheckedChanged( buttonView: JCompoundButton; isChecked: boolean ); cdecl;
  end;
{$ENDIF}

  TDPFOnCheckedChanged = procedure( sender: TObject; isChecked: boolean ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJCheckBox = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJCheckBox                : JCheckBox;
    FCheckBoxOnChangedListener: TCheckBoxOnChangedListener;
{$ENDIF}
    FText            : string;
    FTextColor       : TAlphaColor;
    FNumberOfLines   : Integer;
    FTextAlignment   : TDPFTextAlignment;
    FTextSize        : Single;
    FAllCaps         : Boolean;
    FChecked         : Boolean;
    FOnCheckedChanged: TDPFOnCheckedChanged;
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
  published
    property Text            : string read FText write SetText;
    property TextColor       : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize        : Single read FTextSize write SetTextSize;
    property NumberOfLines   : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment   : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps         : Boolean read FAllCaps write SetAllCaps default false;
    property Checked         : Boolean read FChecked write SetChecked default false;
    property OnCheckedChanged: TDPFOnCheckedChanged read FOnCheckedChanged write FOnCheckedChanged;

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
{ TDPFJCheckBox }
// ------------------------------------------------------------------------------
constructor TDPFJCheckBox.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'CheckBox';
  FTextColor     := TAlphaColors.Black;
  FNumberOfLines := 1;
  FTextAlignment := taAuto;
  FTextSize      := 21.0;
  FChecked       := false;

{$IFDEF ANDROID}
  FCheckBoxOnChangedListener := TCheckBoxOnChangedListener.create( self );
  try
    CallInUIThreadAndWaitFinishing(
      procedure( )
      begin
        FJCheckBox := TJCheckBox.JavaClass.init( SharedActivity );
        FJCheckBox.setOnCheckedChangeListener( FCheckBoxOnChangedListener );
        JControl := FJCheckBox;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJCheckBox.Destroy;
begin
{$IFDEF ANDROID}
  FCheckBoxOnChangedListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJCheckBox <> nil then
  begin
    // FJCheckBox.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFJCheckBox.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJCheckBox.sizeToFit;
  result.width  := FJCheckBox.getWidth;
  result.height := FJCheckBox.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.Toggle;
begin
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.toggle;
      end );
  end;
  FChecked := FJCheckBox.isChecked;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJCheckBox.Loaded;
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
procedure TDPFJCheckBox.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetChecked( const Value: Boolean );
begin
  FChecked := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setChecked( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckBox ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJCheckBox <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckBox.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJCheckBox <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckBox.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJCheckBox.Paint;
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
constructor TCheckBoxOnChangedListener.create( ADPFJCheckBox: TDPFJCheckBox );
begin
  inherited create;
  FDPFJCheckBox := ADPFJCheckBox;
end;

// ------------------------------------------------------------------------------
procedure TCheckBoxOnChangedListener.onCheckedChanged( buttonView: JCompoundButton; isChecked: boolean ); cdecl;
begin
  if assigned( FDPFJCheckBox.FOnCheckedChanged ) then
    FDPFJCheckBox.FOnCheckedChanged( FDPFJCheckBox, isChecked );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
