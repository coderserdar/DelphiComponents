// ------------------------------------------------------------------------------
// DPF.Android.JTextView Component
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
unit DPF.Android.JTextView;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.Android.BaseControl,
  DPF.Android.Widget,
{$IFDEF ANDROID}
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
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJTextView = class;

{$IFDEF ANDROID}
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJTextView = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJTextView: JDPFTextView;
    // FJScrollingMovementMethod: JScrollingMovementMethod;

{$ENDIF}
    FText                 : TStrings;
    FTextColor            : TAlphaColor;
    FNumberOfLines        : Integer;
    FTextAlignment        : TDPFTextAlignment;
    FTextSize             : Single;
    FAllCaps              : Boolean;
    FVerticalScrollBar    : Boolean;
    FScrollable           : Boolean;
    FHorizontallyScrolling: Boolean;
    procedure TextChanged( Sender: TObject );
    procedure SetText( const Value: TStrings );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetTextSize( const Value: Single );
    procedure SetAllCaps( const Value: Boolean );

  protected
    procedure Resize; override;
    procedure Move; override;
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
{$IFDEF ANDROID}
    property FTextView: JDPFTextView read FJTextView;
{$ENDIF}
  published
    property Text                 : TStrings read FText write SetText;
    property TextColor            : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize             : Single read FTextSize write SetTextSize;
    property NumberOfLines        : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment        : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps              : Boolean read FAllCaps write SetAllCaps default false;
    property HorizontallyScrolling: Boolean read FHorizontallyScrolling write FHorizontallyScrolling default false;
    property VerticalScrollBar    : Boolean read FVerticalScrollBar write FVerticalScrollBar default false;
    property Scrollable           : Boolean read FScrollable write FScrollable default false;

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
    property JID;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFJTextView }
// ------------------------------------------------------------------------------
constructor TDPFJTextView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption                := 'TextView';
  FTextColor                    := TAlphaColors.Black;
  FNumberOfLines                := 1;
  FTextAlignment                := taAuto;
  FAllCaps                      := false;
  FTextSize                     := 16.0;
  FText                         := TStringList.Create;
  TStringList( FText ).OnChange := TextChanged;
  FHorizontallyScrolling        := false;
  FVerticalScrollBar            := false;
  FScrollable                   := false;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJTextView := TJDPFTextView.JavaClass.init( SharedActivity );
      JControl := FJTextView;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJTextView.Destroy;
begin
  FText.Free;
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJTextView <> nil then
  begin
    // FJTextView.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
function TDPFJTextView.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJTextView.sizeToFit;
  result.width  := FJTextView.getWidth;
  result.height := FJTextView.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.TextChanged( Sender: TObject );
begin
{$IFDEF ANDROID}
  if Assigned( FJTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, FText.Text ) ) );
      end );
  end;
{$ELSE}
  ControlCaption := FText.Text;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJTextView.Loaded;
begin
  if FScrollable then
  begin
    { FJScrollingMovementMethod := TJScrollingMovementMethod.JavaClass.init;
      FJTextView.setMovementMethod( JMovementMethod( FJScrollingMovementMethod ) ); }
  end;
  // FJTextView.setVerticalScrollBarEnabled( FVerticalScrollBar );
  // FJTextView.setHorizontallyScrolling( FHorizontallyScrolling );

  TextChanged( self );

  SetTextColor( FTextColor );

  SetVisible( Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );

  AddSubView( Self, ParentControl );

  CallInUIThread(
    procedure
    begin
      // FJTextView.setEllipsize( nil );
    end );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJTextView.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.Resize;
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    begin
      if FJTextView <> nil then
      begin
        // FJTextView.setWidth( round( Width * ScreenScale ) );
        // FJTextView.setHeight( round( height * ScreenScale ) );
      end;
    end );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJTextView.Paint;
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
  if ( Text.Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    Caption           := name;
    Canvas.Fill.Color := TAlphaColors.Gray;
  end
  else
  begin
    Caption           := Text.Text;
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
procedure TDPFJTextView.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.SetText( const Value: TStrings );
begin
  FText.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJTextView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJTextView <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTextView.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJTextView <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJTextView.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
