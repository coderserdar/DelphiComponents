// ------------------------------------------------------------------------------
// DPF.iOS.MPVolume Component
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
unit DPF.iOS.MPVolume;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,

  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  IOSapi.MediaPlayer,
  IOSapi.CoreMedia,
  IOSapi.AVFoundation,
  IOSapi.Foundation,
  IOSapi.CoreGraphics,
  IOSapi.UIKit,
  FMX.Platform.iOS,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  System.Math,
{$ENDIF}
  System.Types;

// ------------------------------------------------------------------------------
type

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFMPVolume = class( TDPFiOSBaseControl )
  private
    { Private declarations }
{$IFDEF IOS}
    FMPVolumeView: MPVolumeView;
{$ENDIF}
  protected
    { Protected declarations }
    procedure Resize; override;
    procedure Move; override;
  public
    { Public declarations }
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Anchors;
    property Height;
    property Padding;
    property Margins;
    property Position;
    property Enabled default True;
    property Visible default True;
    property Width;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFMPVolume }
constructor TDPFMPVolume.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'Volume Control';
{$IFDEF IOS}
  FMPVolumeView := TMPVolumeView.Wrap( TMPVolumeView.Alloc.initWithFrame( CGRectMake( 20, 20, 200, 50 ) ) );
  FUIControl    := FMPVolumeView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFMPVolume.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFMPVolume.Loaded;
begin
  FMPVolumeView.setShowsRouteButton( True );
  FMPVolumeView.setShowsVolumeSlider( True );
  FMPVolumeView.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );

  AddSubView( Self, ParentControl );
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFMPVolume.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if Assigned( FMPVolumeView ) then FMPVolumeView.setFrame( CGRectMake( Position.X, Position.Y + 50, Width - 20, Height ) );
{$ELSE}
  // Height := Max( iOS_GUI_Bitmaps.VolumeControl.Knob.Height, 25 );
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFMPVolume.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFMPVolume.Paint;
var
  R: TRectF;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // Left
  BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Fill_Left, 0, 7 );
  // Blue - 50%
  R := RectF( iOS_GUI_Bitmaps.ProgressView.Left.Width, 7, trunc( Width / 2 ), 7 + iOS_GUI_Bitmaps.ProgressView.Right.Height );
  BitmapToRect( Self, iOS_GUI_Bitmaps.ProgressView.Fill_BG, R );
  // White - 50%
  R.Left  := R.Right;
  R.Right := Width - iOS_GUI_Bitmaps.ProgressView.Right.Width;
  BitmapToRect( Self, iOS_GUI_Bitmaps.ProgressView.BG, R );
  // Right
  BitmapToPosition( Self, iOS_GUI_Bitmaps.ProgressView.Right, Width - iOS_GUI_Bitmaps.ProgressView.Right.Width, 7 );
  // Knob
  BitmapToPosition( Self, iOS_GUI_Bitmaps.VolumeControl.Knob, trunc( Width / 2 ) - ( iOS_GUI_Bitmaps.VolumeControl.Knob.Width div 2 ), 0 );
  Canvas.EndScene;
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
