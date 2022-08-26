// ------------------------------------------------------------------------------
// DPF.iOS.CheckBox Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
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
unit DPF.iOS.CheckBox;

interface

{$I DPF.iOS.Defs.inc}
{$R DPF.iOS.CheckBox.res}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont,
  DPF.iOS.UIImageView,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
{$IFDEF IOS}
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
{$ENDIF}
  DPF.iOS.Common,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFCheckBoxKind = ( cbkBox, cbkBox2x, cbkDark, cbkDark2x, cbkGlossy, cbkGlossy2x, cbkGreen, cbkGreen2x, cbkMono, cbkMono2x );

  // ------------------------------------------------------------------------------

  TDPFImageCheckBox = class( TDPFImageView )
  public
    procedure Paint; override;
  end;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFCheckBox = class( TDPFUIView )
  private
    FChecked        : Boolean;
    FCheckedBitMap  : TBitMap;
    FUnCheckedBitMap: TBitMap;

    FImage              : TDPFImageCheckBox;
    FLabel              : TDPFLabel;
    FOnClick            : TDPFOnClicked;
    FCheckedKind        : TDPFCheckBoxKind;
    FTextColor          : TAlphaColor;
    FTextAlignment      : TDPFTextAlignment;
    FNumberOfLines      : Integer;
    FText               : string;
    FBackgroundColor    : TAlphaColor;
    FTextBackgroundImage: string;
    FLineBreak          : TDPFLineBreak;
    FImageAlignment     : TDPFTextAlignment;
    procedure SetChecked( const Value: Boolean );
    procedure SetCheckedKind( const Value: TDPFCheckBoxKind );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetText( const Value: string );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextBackgroundImage( const Value: string );
    procedure SetLineBreak( const Value: TDPFLineBreak );
    procedure SetImageAlignment( const Value: TDPFTextAlignment );
    procedure OnFontChanged( Sender: TObject );
    procedure SetNumberOfLines( const Value: Integer );
  protected
    procedure CheckBoxOnClick( Sender: TObject );
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property Checked            : Boolean read FChecked write SetChecked default false;
    property CheckedKind        : TDPFCheckBoxKind read FCheckedKind write SetCheckedKind default cbkBox;
    property Text               : string read FText write SetText;
    property TextColor          : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property BackgroundColor    : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property NumberOfLines      : Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment      : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taLeft;
    property TextBackgroundImage: string read FTextBackgroundImage write SetTextBackgroundImage;
    property LineBreak          : TDPFLineBreak read FLineBreak write SetLineBreak default lbCharacterWrap;
    property ImageAlignment     : TDPFTextAlignment read FImageAlignment write SetImageAlignment default taLeft;

    property OnClick: TDPFOnClicked read FOnClick write FOnClick;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

const
  TDPFCheckBoxKindOnStr: array [TDPFCheckBoxKind] of string = ( 'cb_box_on_png', 'cb_box_on_2x_png', 'cb_dark_on_png', 'cb_dark_on_2x_png', 'cb_glossy_on_png', 'cb_glossy_on_2x_png', 'cb_green_on_png', 'cb_green_on_2x_png', 'cb_mono_on_png', 'cb_mono_on_2x_png' );
  TDPFCheckBoxKindOffStr: array [TDPFCheckBoxKind] of string = ( 'cb_box_off_png', 'cb_box_off_2x_png', 'cb_dark_off_png', 'cb_dark_off_2x_png', 'cb_glossy_off_png', 'cb_glossy_off_2x_png', 'cb_green_off_png', 'cb_green_off_2x_png', 'cb_mono_off_png', 'cb_mono_off_2x_png' );

procedure LoadResourceToBitmap( var ABitmap: TBitmap; ResourceName: string );
var
  Stream: TResourceStream;
begin
  if not Assigned( ABitmap ) then
    ABitmap := TBitmap.Create( 0, 0 );
  Stream    := TResourceStream.Create( HInstance, ResourceName, RT_RCDATA );
  ABitmap.LoadFromStream( Stream );
  Stream.Free;
end;

// ------------------------------------------------------------------------------
{ TDPFCheckBox }
// ------------------------------------------------------------------------------
constructor TDPFCheckBox.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FText            := 'CheckBox';
  ControlCaption   := FText;
  FCheckedKind     := cbkBox;
  FTextColor       := TAlphaColors.Black;
  FBackgroundColor := TAlphaColors.Null;
  FNumberOfLines   := 1;
  FTextAlignment   := taLeft;
  FLineBreak       := lbCharacterWrap;
  FImageAlignment  := taLeft;

  LoadResourceToBitmap( FCheckedBitMap, TDPFCheckBoxKindOnStr[FCheckedKind] );
  LoadResourceToBitmap( FUnCheckedBitMap, TDPFCheckBoxKindOffStr[FCheckedKind] );

  Height := FCheckedBitMap.Height;

  FImage                 := TDPFImageCheckBox.Create( self );
  FImage.Parent          := self;
  FImage.Align           := TAlignLayout.Left;
  FImage.Width           := FCheckedBitMap.Width;
  FImage.ContentMode     := vcmCenter;
  FImage.UserInteraction := true;
  FImage.Locked          := true;
  FImage.Stored          := false;
  FImage.OnClick         := CheckBoxOnClick;

  FLabel                 := TDPFLabel.Create( self );
  FLabel.Parent          := self;
  FLabel.Align           := TAlignLayout.Client;
  FLabel.Text            := FText;
  FLabel.Locked          := true;
  FLabel.Stored          := false;
  FLabel.UserInteraction := true;
  FLabel.OnClick         := CheckBoxOnClick;

  BorderColor := TAlphaColors.Silver;

  Font.OnChanged := OnFontChanged;

  // CheckReceiveTouch := false;
  // inherited OnClick := CheckBoxOnClick;

{$IFDEF IOS}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFCheckBox.Destroy;
begin
  DisposeOfAndNil(FImage);
  DisposeOfAndNil(FLabel);
  FCheckedBitMap.Free; // SZ Added
  FUnCheckedBitMap.Free; // SZ Added
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFCheckBox.Loaded;
begin
  inherited BackgroundColor := FBackgroundColor;
  BackgroundColor           := TAlphaColors.Null;
  FImage.Loaded;
  FLabel.Loaded;

  SetImageAlignment( ImageAlignment );
  SetTextAlignment( FTextAlignment );
  SetText( FText );
  SetTextAlignment( FTextAlignment );
  SetBackgroundColor( FBackgroundColor );
  SetCheckedKind( FCheckedKind );
  SetNumberOfLines( FNumberOfLines );
  OnFontChanged( Self );
  inherited;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFCheckBox.Paint;
var
  R: TRectF;
begin
  Canvas.BeginScene;
  R.Left   := FImage.Position.X;
  R.Top    := FImage.Position.Y;
  R.Width  := FImage.Width;
  R.Height := FImage.Height;
  if FChecked then
    Canvas.DrawBitmap( FCheckedBitMap, RectF( 0, 0, FImage.Width, FImage.Height ), R, Alpha, true )
  else
    Canvas.DrawBitmap( FUnCheckedBitMap, RectF( 0, 0, FImage.Width, FImage.Height ), R, Alpha, true );
  if BorderWidth > 0 then
  begin
    Canvas.Stroke.Color     := BorderColor;
    Canvas.Stroke.Thickness := BorderWidth;
    R                       := LocalRect;
    InflateRect( R, -0.5, -0.5 );
    Canvas.DrawRect( R, 0.5, 0.5, AllCorners, Alpha, TCornerType.InnerRound );
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.CheckBoxOnClick( Sender: TObject );
begin
  Checked := not Checked;
  if Assigned( FOnClick ) then
    FOnClick( Self );
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor       := Value;
  FLabel.BackgroundColor := Value;
{$IFDEF IOS}
  if FUIView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetChecked( const Value: Boolean );
begin
  FChecked := Value;
{$IFNDEF IOS}
  Invalidate;
{$ELSE}
  if FChecked then
    FImage.SetImage( FCheckedBitMap )
  else
    FImage.SetImage( FUnCheckedBitMap );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetCheckedKind( const Value: TDPFCheckBoxKind );
begin
  if ( FCheckedKind = Value ) and not( csLoading in ComponentState ) then
    exit;
  FCheckedKind := Value;
  LoadResourceToBitmap( FCheckedBitMap, TDPFCheckBoxKindOnStr[value] );
  LoadResourceToBitmap( FUnCheckedBitMap, TDPFCheckBoxKindOffStr[value] );
  FImage.Width := FCheckedBitMap.Width;
  SetChecked( FChecked );
{$IFNDEF IOS}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetImageAlignment( const Value: TDPFTextAlignment );
const
  TAL: array [TDPFTextAlignment] of TAlignLayout = ( TAlignLayout.Left, TAlignLayout.Center, TAlignLayout.Right );
begin
  FImageAlignment := Value;
  FImage.Align    := TAL[Value];
{$IFNDEF IOS}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetLineBreak( const Value: TDPFLineBreak );
begin
  FLineBreak       := Value;
  FLabel.LineBreak := value;
end;

procedure TDPFCheckBox.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines       := value;
  FLabel.NumberOfLines := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetText( const Value: string );
begin
  FText       := Value;
  FLabel.Text := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment       := Value;
  FLabel.TextAlignment := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetTextBackgroundImage( const Value: string );
begin
  FTextBackgroundImage   := Value;
  FLabel.BackgroundImage := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor       := Value;
  FLabel.TextColor := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFCheckBox.OnFontChanged( Sender: TObject );
begin
  inherited;
{$IFDEF IOS}
  if FLabel <> nil then
  begin
    FLabel.Font.FontName           := Font.FontName;
    FLabel.Font.FontSize           := Font.FontSize;
    FLabel.Font.BoldSystemFontSize := Font.BoldSystemFontSize;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFImageCheckBox }
procedure TDPFImageCheckBox.Paint;
begin
  { }
end;

initialization

RegisterClass( TDPFImageView );
RegisterClass( TDPFLabel );

// ------------------------------------------------------------------------------
end.
