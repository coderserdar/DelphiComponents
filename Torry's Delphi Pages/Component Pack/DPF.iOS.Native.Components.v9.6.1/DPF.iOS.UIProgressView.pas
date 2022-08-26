// ------------------------------------------------------------------------------
// DPF.iOS.UIProgressView Component
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
unit DPF.iOS.UIProgressView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  System.Math,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFProgress = class;

  TDPFProgressKind = ( skNormal, skCustom );

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFProgress = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIProgressView: UIProgressView;

{$ENDIF}
    FBackgroundColor      : TAlphaColor;
    FProgress             : Single;
    FMinimumTrackTintColor: TAlphaColor;
    FTrackTintColor       : TAlphaColor;
    FAnimated             : Boolean;
    FProgressTintColor    : TAlphaColor;
    FProgressImage        : string;
    FTrackImage           : string;
    FSliderKind           : TDPFProgressKind;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetProgress( const Value: Single );

  protected
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
    property Animated             : Boolean read FAnimated write FAnimated default True;
    property BackgroundColor      : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property TrackTintColor       : TAlphaColor read FTrackTintColor write FTrackTintColor default TAlphaColors.White;
    property MinimumTrackTintColor: TAlphaColor read FMinimumTrackTintColor write FMinimumTrackTintColor default TAlphaColors.Greenyellow;
    property ProgressTintColor    : TAlphaColor read FProgressTintColor write FProgressTintColor default TAlphaColors.Gray;
    property TrackImage           : string read FTrackImage write FTrackImage;
    property ProgressImage        : string read FProgressImage write FProgressImage;
    property Progress             : Single read FProgress write SetProgress stored true;
    property ProgressKind         : TDPFProgressKind read FSliderKind write FSliderKind default skNormal;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFProgress }
// ------------------------------------------------------------------------------
constructor TDPFProgress.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption         := 'Slider';
  FBackgroundColor       := TAlphaColors.Null;
  FTrackTintColor        := TAlphaColors.White;
  FMinimumTrackTintColor := TAlphaColors.Greenyellow;
  FProgressTintColor     := TAlphaColors.Blue;
  FProgress              := 0.0;
  FAnimated              := True;
  FSliderKind            := skNormal;
{$IFDEF IOS}
  FUIProgressView := TUIProgressView.Create;
  FUIControl      := FUIProgressView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFProgress.Destroy;
begin
{$IFDEF IOS}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFProgress.Loaded;
var
  Image: UIImage;
begin

  SetProgress( FProgress );

  if FSliderKind = skCustom then
  begin
    if FTrackImage <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FTrackImage ) ) );
      FUIProgressView.setTrackImage( Image );
      // Image.release;
      Image := nil;
    end
    else
    begin
      if ( FTrackTintColor = TAlphaColors.Null ) then
      begin
        // if Assigned( FUIProgressView.TrackTintColor ) then
        FUIProgressView.setTrackTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) { nil } )
      end
      else
        FUIProgressView.setTrackTintColor( TColorToUIColor( FTrackTintColor ) );
    end;

    if FProgressImage <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FProgressImage ) ) );
      FUIProgressView.setProgressImage( Image );
      // Image.release;
      Image := nil;
    end
    else
    begin
      if ( FProgressTintColor = TAlphaColors.Null ) then
      begin
        // if Assigned( FUIProgressView.ProgressTintColor ) then
        FUIProgressView.setProgressTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) { nil } )
      end
      else
        FUIProgressView.setProgressTintColor( TColorToUIColor( FProgressTintColor ) );
    end;

    if FBackgroundColor = TAlphaColors.Null then
      FUIProgressView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUIProgressView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );
  end;

  Resize ;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFProgress.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUIProgressView <> nil then
    FUIProgressView.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
{$ELSE}
  // Added by Fenistil
  Height := iOS_GUI_Bitmaps.ProgressView.Left.Height;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFProgress.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFProgress.Paint;
var
  px: integer;
  R : TRectF;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // Background
  BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Left, 0, 0 );
  R := RectF( iOS_GUI_Bitmaps.ProgressView.Left.Width, 0, Width - iOS_GUI_Bitmaps.ProgressView.Right.Width, iOS_GUI_Bitmaps.ProgressView.Right.Height );
  BitmapToRect( Self, iOS_GUI_Bitmaps.ProgressView.BG, R );
  BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Right, Width - iOS_GUI_Bitmaps.ProgressView.Right.Width, 0 );
  // Fill
  if ( FProgress > 0 ) and ( FProgress <= 1 ) then
  begin
    px := Round( Max( Width * FProgress, iOS_GUI_Bitmaps.ProgressView.Fill_Left.Width + iOS_GUI_Bitmaps.ProgressView.Fill_Right.Width ) );
    BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Fill_Left, 0, 0 );
    R := RectF( iOS_GUI_Bitmaps.ProgressView.Fill_Left.Width, 0, px - iOS_GUI_Bitmaps.ProgressView.Fill_Right.Width, iOS_GUI_Bitmaps.ProgressView.Fill_Right.Height );
    if R.Width > 0 then
      BitmapToRect( Self, iOS_GUI_Bitmaps.ProgressView.Fill_BG, R );
    BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Fill_Right, px - iOS_GUI_Bitmaps.ProgressView.Fill_Right.Width, 0 );
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFProgress.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIProgressView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIProgressView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIProgressView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFProgress.SetProgress( const Value: Single );
begin
  if Value > 1 then
    exit;

  FProgress := Value;
{$IFDEF IOS}
  if FUIProgressView <> nil then
  begin
    FUIProgressView.setProgress( FProgress, FAnimated );
  end;
{$ELSE}
  // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
