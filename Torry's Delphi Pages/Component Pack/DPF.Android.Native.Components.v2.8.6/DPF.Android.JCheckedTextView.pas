// ------------------------------------------------------------------------------
// DPF.Android.JCheckedTextView Component
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
unit DPF.Android.JCheckedTextView;

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

  TDPFJCheckedTextView = class;

{$IFDEF ANDROID}
{$ENDIF}
  TDPFOnCheckedChanged = procedure( sender: TObject; isChecked: boolean ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJCheckedTextView = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJCheckedTextView: JCheckedTextView;
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
{ TDPFJCheckedTextView }
// ------------------------------------------------------------------------------
constructor TDPFJCheckedTextView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'CheckedTextView';
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
        FJCheckedTextView := TJCheckedTextView.JavaClass.init( SharedActivity );
        JControl := FJCheckedTextView;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJCheckedTextView.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJCheckedTextView <> nil then
  begin
    // FJCheckedTextView.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFJCheckedTextView.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJCheckedTextView.sizeToFit;
  result.width  := FJCheckedTextView.getWidth;
  result.height := FJCheckedTextView.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.Toggle;
begin
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.toggle;
      end );
  end;
  FChecked := FJCheckedTextView.isChecked;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}
procedure TDPFJCheckedTextView.Loaded;
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
procedure TDPFJCheckedTextView.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

procedure TDPFJCheckedTextView.SetChecked( const Value: Boolean );
begin
  FChecked := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setChecked( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJCheckedTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJCheckedTextView <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJCheckedTextView.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJCheckedTextView <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJCheckedTextView.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;
// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJCheckedTextView.Paint;
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
{$ENDIF}

// ------------------------------------------------------------------------------
end.
