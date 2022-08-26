// ------------------------------------------------------------------------------
// DPF.iOS.SlideDialog Component
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
unit DPF.iOS.SlideDialog;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Types,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UILabel,
  DPF.iOS.UIImageView,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  iOSapi.CocoaTypes,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  IOSapi.MediaPlayer,
  IOSapi.CoreMedia,
  IOSapi.AVFoundation,
  IOSapi.Foundation,
  IOSapi.CoreGraphics,
  IOSapi.UIKit,
  FMX.Platform.iOS,
  DPF.iOS.Classes,
{$ENDIF}
  FMX.Forms;

// ------------------------------------------------------------------------------
type

  TDPFSlideDialog = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFSlideDialogTimerDelegate = interface( IObjectiveC )
    ['{1999B90F-BE01-4657-A830-108EC94C0208}']
    procedure onDidTimer( timer: NSTimer ); cdecl;
  end;

  TDPFSlideDialogTimerDelegate = class( TOCLocal, DPFSlideDialogTimerDelegate )
  private
    FDPFSlideDialog: TDPFSlideDialog;
  public
    constructor Create( ADPFSlideDialog: TDPFSlideDialog );
    procedure onDidTimer( timer: NSTimer ); cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------

  TDPFSlideDialogDirection = ( sddTop, sddBottom, sddLeft, sddRight, sddCenter );

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSlideDialog = class( TComponent )
  private
    { Private declarations }
    FParentView     : TDPFiOSBaseControl;
    FBackgroundImage: string;
    FBackgroundColor: TAlphaColor;
    FBorderColor    : TAlphaColor;
    FCornerRadius   : Single;
    FBorderWidth    : Integer;
    FAlpha          : Single;
    FContentMode    : TDPFUIViewContentMode;
    FTextColor      : TAlphaColor;
    FFontName       : TDPFIOSFontList;
    FFontSize       : integer;
    FAnimateDuration: double;
    FDirection      : TDPFSlideDialogDirection;
    FTextPadding    : TBounds;
{$IFDEF IOS}
    FDPFLabel        : TDPFLabel;
    FDPFImageView    : TDPFImageView;
    FNSTimer         : NSTimer;
    FDPFTimerDelegate: TDPFSlideDialogTimerDelegate;
{$ENDIF}
  protected
    { Protected declarations }
{$IFDEF IOS}
{$ENDIF}
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Show( TextMessage: string; Width: Single; Height: Single; AnimateDuration: double = 1.0; CloseAfterSec: Double = 5; Direction: TDPFSlideDialogDirection = TDPFSlideDialogDirection.sddTop );
    procedure Close;
  published
    { Published declarations }
    property ParentView     : TDPFiOSBaseControl read FParentView write FParentView;
    property BackgroundImage: string read FBackgroundImage write FBackgroundImage;
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default TAlphaColors.Null;
    property TextColor      : TAlphaColor read FTextColor write FTextColor default TAlphaColors.Black;
    property TextFontName   : TDPFIOSFontList read FFontName write FFontName default ios_Helvetica;
    property TextFontSize   : integer read FFontSize write FFontSize default 16;
    property BorderWidth    : Integer read FBorderWidth write FBorderWidth default 0;
    property BorderColor    : TAlphaColor read FBorderColor write FBorderColor default TAlphaColors.Null;
    property CornerRadius   : Single read FCornerRadius write FCornerRadius;
    property ContentMode    : TDPFUIViewContentMode read FContentMode write FContentMode default vcmScaleToFill;
    property Alpha          : Single read FAlpha write FAlpha;
    property TextPadding    : TBounds read FTextPadding write FTextPadding;

  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFSlideDialog }
procedure TDPFSlideDialog.Close;
begin
{$IFDEF IOS}
  if assigned( FDPFImageView ) then
    FDPFImageView.Visible := false;
  if assigned( FDPFTimerDelegate ) and assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FDPFTimerDelegate.onDidTimer( nil );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
constructor TDPFSlideDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  TextPadding      := TBounds.Create( RectF( 0, 0, 0, 0 ) );
  FContentMode     := vcmScaleToFill;
  FAlpha           := 1;
  FBackgroundColor := TAlphaColors.Null;
  FBorderColor     := TAlphaColors.Null;
  FTextColor       := TAlphaColors.Black;
  FBorderWidth     := 0;
  FCornerRadius    := 0;
  FDirection       := TDPFSlideDialogDirection.sddTop;
  FFontName        := ios_Helvetica;
  FFontSize        := 16;
  FAnimateDuration := 0.5;
{$IFDEF IOS}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFSlideDialog.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;

  if assigned( FDPFTimerDelegate ) then
    FDPFTimerDelegate.DisposeOf;

  if Assigned( FDPFImageView ) then
    FDPFImageView.DisposeOf;

  if Assigned( FDPFLabel ) then
    FDPFLabel.DisposeOf;

  FreeAndNil( FTextPadding );

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFSlideDialog.Show( TextMessage: string; Width: Single; Height: Single; AnimateDuration: double = 1.0; CloseAfterSec: Double = 5; Direction: TDPFSlideDialogDirection = TDPFSlideDialogDirection.sddTop );
{$IFDEF IOS}
var
  X, Y: Single;
{$ENDIF}
begin
{$IFDEF IOS}
  if not Assigned( FParentView ) then
  begin
    ShowMessage( 'ParentView Not Assigned!' );
    Exit;
  end;

  FDirection := Direction;

  AnimateDuration := AnimateDuration;
  if not Assigned( FDPFImageView ) then
  begin
    FDPFImageView        := TDPFImageView.Create( FParentView );
    FDPFImageView.Parent := FParentView;
  end;

  FDPFImageView.ClipsToBounds := true;
  FDPFImageView.CornerRadius  := CornerRadius;
  FDPFImageView.ImageList.Clear;
  FDPFImageView.ImageList.Add( BackgroundImage );
  FDPFImageView.BackgroundColor := BackgroundColor;
  FDPFImageView.ContentMode     := ContentMode;
  FDPFImageView.Alpha           := Alpha;
  FDPFImageView.BorderColor     := BorderColor;
  FDPFImageView.BorderWidth     := BorderWidth;
  FDPFImageView.CornerRadius    := CornerRadius;
  FDPFImageView.Width           := Width;
  FDPFImageView.Height          := Height;
  FDPFImageView.Position.X      := -100;
  FDPFImageView.Position.Y      := -100;

  if FDPFImageView.isLoaded then
    FDPFImageView.ReloadImage
  else
    FDPFImageView.Loaded;

  if not Assigned( FDPFLabel ) then
  begin
    FDPFLabel               := TDPFLabel.Create( FDPFImageView );
    FDPFLabel.Parent        := FDPFImageView;
    FDPFLabel.Alpha         := 1.0;
    FDPFLabel.NumberOfLines := 0;
    FDPFLabel.Align         := TAlignLayout.alClient;
    FDPFLabel.TextAlignment := TDPFTextAlignment.taCenter;
    FDPFLabel.Loaded;
  end;

  if TextMessage <> '' then
  begin
    FDPFLabel.Font.FontName  := TextFontName;
    FDPFLabel.Font.FontSize  := TextFontSize;
    FDPFLabel.TextColor      := TextColor;
    FDPFLabel.Padding.Left   := TextPadding.Left;
    FDPFLabel.Padding.Right  := TextPadding.Right;
    FDPFLabel.Padding.Top    := TextPadding.Top;
    FDPFLabel.Padding.Bottom := TextPadding.Bottom;
  end;
  FDPFLabel.Text := TextMessage;

  FDPFImageView.Visible := true;
  FDPFImageView.BringToFront;

  case Direction of
    sddTop:
      begin
        X := ( FParentView.Width - FDPFImageView.Width ) / 2;
        Y := -FDPFImageView.Height;
        FDPFImageView.SetAnimationTransition( X, X, Y, 0, FDPFImageView.Width, FDPFImageView.Width, FDPFImageView.Height, FDPFImageView.Height, AnimateDuration,
          procedure( )
          begin
            FDPFLabel.Alpha := 1.0;
          end );

      end;
    sddBottom:
      begin
        X := ( FParentView.Width - FDPFImageView.Width ) / 2;
        Y := FParentView.Height + FParentView.Position.Y;
        FDPFImageView.SetAnimationTransition( X, X, Y, Y - FDPFImageView.Height, FDPFImageView.Width, FDPFImageView.Width, FDPFImageView.Height, FDPFImageView.Height, AnimateDuration,
          procedure( )
          begin
            FDPFLabel.Alpha := 1.0;
          end );
      end;
    sddRight:
      begin
        X := FParentView.Position.X + FParentView.Width;
        Y := ( FParentView.Height - FDPFImageView.Height ) / 2;
        FDPFImageView.SetAnimationTransition( X, X - FDPFImageView.Width, Y, Y, FDPFImageView.Width, FDPFImageView.Width, FDPFImageView.Height, FDPFImageView.Height, AnimateDuration,
          procedure( )
          begin
            FDPFLabel.Alpha := 1.0;
          end );
      end;
    sddLeft:
      begin
        X := FParentView.Position.X;
        Y := ( FParentView.Height - FDPFImageView.Height ) / 2;
        FDPFImageView.SetAnimationTransition( X - FDPFImageView.Width, X, Y, Y, FDPFImageView.Width, FDPFImageView.Width, FDPFImageView.Height, FDPFImageView.Height, AnimateDuration,
          procedure( )
          begin
            FDPFLabel.Alpha := 1.0;
          end );
      end;
    sddCenter:
      begin
        X := ( FParentView.Width - FDPFImageView.Width ) / 2;
        Y := ( FParentView.Height - FDPFImageView.Height ) / 2;
        FDPFImageView.SetAnimationTransition( X + ( FDPFImageView.Width / 2 ), X, Y + ( FDPFImageView.Height / 2 ), Y, 0, FDPFImageView.Width, 0, FDPFImageView.Height, AnimateDuration,
          procedure( )
          begin
            FDPFLabel.Alpha := 1.0;
          end );
      end;
  end;

  if not assigned( FDPFTimerDelegate ) then
    FDPFTimerDelegate := TDPFSlideDialogTimerDelegate.Create( Self );
  if Assigned( FNSTimer ) then
  begin
    TNSTimer.OCClass.cancelPreviousPerformRequestsWithTarget( FDPFTimerDelegate.GetObjectID );
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;

  FNSTimer := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( CloseAfterSec, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'onDidTimer:' ), nil, false ) );
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFSlideDialogDelegate }
constructor TDPFSlideDialogTimerDelegate.Create( ADPFSlideDialog: TDPFSlideDialog );
begin
  inherited Create;
  FDPFSlideDialog := ADPFSlideDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFSlideDialogTimerDelegate.onDidTimer( timer: NSTimer );
var
  X, Y: Single;
begin
  FDPFSlideDialog.FNSTimer := nil;
  case FDPFSlideDialog.FDirection of
    sddTop:
      begin
        X := ( FDPFSlideDialog.FParentView.Width - FDPFSlideDialog.FDPFImageView.Width ) / 2;
        Y := -FDPFSlideDialog.FDPFImageView.Height;
        FDPFSlideDialog.FDPFImageView.SetAnimationTransition( X, X, 0, Y, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FAnimateDuration,
          procedure( )
          begin
            FDPFSlideDialog.FDPFLabel.Alpha := 0.0;
          end );
      end;
    sddBottom:
      begin
        X := ( FDPFSlideDialog.FParentView.Width - FDPFSlideDialog.FDPFImageView.Width ) / 2;
        Y := FDPFSlideDialog.ParentView.Height + FDPFSlideDialog.ParentView.Position.Y;
        FDPFSlideDialog.FDPFImageView.SetAnimationTransition( X, X, Y - FDPFSlideDialog.FDPFImageView.Height, Y, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FAnimateDuration,
          procedure( )
          begin
            FDPFSlideDialog.FDPFLabel.Alpha := 0.0;
          end );
      end;
    sddRight:
      begin
        X := FDPFSlideDialog.FParentView.Position.X + FDPFSlideDialog.FParentView.Width;
        Y := ( FDPFSlideDialog.FParentView.Height - FDPFSlideDialog.FDPFImageView.Height ) / 2;
        FDPFSlideDialog.FDPFImageView.SetAnimationTransition( X - FDPFSlideDialog.FDPFImageView.Width, X, Y, Y, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FAnimateDuration,
          procedure( )
          begin
            FDPFSlideDialog.FDPFLabel.Alpha := 0.0;
          end );
      end;
    sddLeft:
      begin
        X := FDPFSlideDialog.FParentView.Position.X;
        Y := ( FDPFSlideDialog.FParentView.Height - FDPFSlideDialog.FDPFImageView.Height ) / 2;
        FDPFSlideDialog.FDPFImageView.SetAnimationTransition( X, X - FDPFSlideDialog.FDPFImageView.Width, Y, Y, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Width, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FDPFImageView.Height, FDPFSlideDialog.FAnimateDuration,
          procedure( )
          begin
            FDPFSlideDialog.FDPFLabel.Alpha := 0.0;
          end );
      end;
    sddCenter:
      begin
        X := ( FDPFSlideDialog.FParentView.Width - FDPFSlideDialog.FDPFImageView.Width ) / 2;
        Y := ( FDPFSlideDialog.FParentView.Height - FDPFSlideDialog.FDPFImageView.Height ) / 2;
        FDPFSlideDialog.FDPFImageView.SetAnimationTransition( X, X + ( FDPFSlideDialog.FDPFImageView.Width / 2 ), Y, Y + ( FDPFSlideDialog.FDPFImageView.Height / 2 ), FDPFSlideDialog.FDPFImageView.Width, 0, FDPFSlideDialog.FDPFImageView.Height, 0, FDPFSlideDialog.FAnimateDuration,
          procedure( )
          begin
            FDPFSlideDialog.FDPFLabel.Alpha := 0.0;
          end );
      end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
