// ------------------------------------------------------------------------------
// DPF.iOS.UIActivityIndicatorView Component
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
unit DPF.iOS.UIActivityIndicatorView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
  DPF.iOS.Classes,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFActivityIndicatorView = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFActivityIndicatorViewTimerDelegate = interface( IObjectiveC )
    ['{23CDE003-B582-4005-9B5D-67DF49720AE7}']
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;

  TDPFActivityIndicatorViewTimerDelegate = class( TOCLocal, DPFActivityIndicatorViewTimerDelegate )
  private
    FDPFActivityIndicatorView: TDPFActivityIndicatorView;
  public
    constructor Create( ADPFActivityIndicatorView: TDPFActivityIndicatorView );
    procedure ondidTimer( timer: NSTimer ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFActivityIndicatorView = class( TDPFiOSBaseControl )
  strict private
{$IFDEF IOS}
    FUIActivityIndicatorView: UIActivityIndicatorView;
    FDPFTimerDelegate       : TDPFActivityIndicatorViewTimerDelegate;
    FNSTimer                : NSTimer;
{$ENDIF}
    FColor                 : TAlphaColor;
    FBackgroundColor       : TAlphaColor;
    FActivityIndicatorStyle: TDPFActivityIndicatorViewStyle;
    FHidesWhenStopped      : Boolean;
    procedure SetColor( const Value: TAlphaColor );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetActivityIndicatorStyle( const Value: TDPFActivityIndicatorViewStyle );
    function GetIsAnimating: Boolean;
{$IFDEF IOS}
    procedure SetHidesWhenStopped( const Value: Boolean );
{$ENDIF}
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
    procedure RefreshNeeded; override;
    procedure sizeToFit;
    procedure StartAnimating( HideAfterSec: Integer = 0 );
    procedure StopAnimating( isHide: Boolean = true );
    property isAnimating: Boolean read GetIsAnimating;
  published
    property ActivityIndicatorStyle: TDPFActivityIndicatorViewStyle read FActivityIndicatorStyle write SetActivityIndicatorStyle default TDPFActivityIndicatorViewStyle.aisWhiteLarge;
    property Color                 : TAlphaColor read FColor write SetColor default TAlphaColors.Black;
    property BackgroundColor       : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;

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

// ------------------------------------------------------------------------------
{ TDPFActivityIndicatorView }
// ------------------------------------------------------------------------------
constructor TDPFActivityIndicatorView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption          := 'Activity Indicator';
  FColor                  := TAlphaColors.Black;
  FBackgroundColor        := TAlphaColors.Null;
  FActivityIndicatorStyle := TDPFActivityIndicatorViewStyle.aisWhiteLarge;
  FHidesWhenStopped       := true;
{$IFDEF IOS}
  FUIActivityIndicatorView := TUIActivityIndicatorView.Create;
  FUIControl               := FUIActivityIndicatorView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFActivityIndicatorView.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FNSTimer ) then
  begin
    FNSTimer.invalidate;
    FNSTimer := nil;
  end;
  if Assigned( FDPFTimerDelegate ) then
    FDPFTimerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.sizeToFit;
begin
{$IFDEF IOS}
  FUIActivityIndicatorView.sizeToFit;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.StartAnimating( HideAfterSec: Integer = 0 );
begin
{$IFDEF IOS}
  if FUIActivityIndicatorView <> nil then
  begin
    if HideAfterSec > 0 then
    begin
      SetHidesWhenStopped( true );
      if not Assigned( FDPFTimerDelegate ) then
        FDPFTimerDelegate := TDPFActivityIndicatorViewTimerDelegate.Create( Self );

      FNSTimer := TNSTimer.Wrap( TNSTimer.OCClass.scheduledTimerWithTimeInterval( HideAfterSec, FDPFTimerDelegate.GetObjectID, Sel_getUid( 'ondidTimer:' ), nil, true ) );
    end;
    Visible := true;
//    Realign;
    BringToFront;
    FUIActivityIndicatorView.startAnimating;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFActivityIndicatorView.GetIsAnimating: Boolean;
begin
{$IFDEF IOS}
  result := false;
  if Assigned( FUIActivityIndicatorView ) then
    result := FUIActivityIndicatorView.isAnimating;
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.StopAnimating( isHide: Boolean = true );
begin
{$IFDEF IOS}
  if FUIActivityIndicatorView <> nil then
  begin
    SetHidesWhenStopped( isHide );
    if Assigned( FNSTimer ) then
    begin
      FNSTimer.invalidate;
      FNSTimer := nil;
    end;

    FUIActivityIndicatorView.stopAnimating;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFActivityIndicatorView.Loaded;
begin
  FUIActivityIndicatorView.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );

  FUIActivityIndicatorView.setHidden( not Visible );
  SetActivityIndicatorStyle( FActivityIndicatorStyle );
  SetHidesWhenStopped( FHidesWhenStopped );
  SetBackgroundColor( FBackgroundColor );
  SetColor( FColor );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.Resize;
begin
  inherited;
  (*
{$IFDEF IOS}
  if FUIActivityIndicatorView <> nil then
  begin
    FUIActivityIndicatorView.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  end;
{$ENDIF}
*)
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFActivityIndicatorView.Paint;
var
  Bmp: TBitmap;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // Background
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Kind  := TBrushKind.Solid;
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, CornerRadius, CornerRadius, AllCorners, Alpha, TCornerType.Round );
  end;
  // Icon
  Bmp := nil;
  case ActivityIndicatorStyle of
    aisGray:
      Bmp := iOS_GUI_Bitmaps.ActivityIndicator.Gray;
    aisWhite:
      Bmp := iOS_GUI_Bitmaps.ActivityIndicator.White;
    aisWhiteLarge:
      Bmp := iOS_GUI_Bitmaps.ActivityIndicator.WhiteLarge;
  end;
  if Bmp <> nil then
    BitmapToPosition( Self, Bmp, ClipRect.CenterPoint.X - Bmp.Width / 2, ClipRect.CenterPoint.Y - Bmp.Height / 2 );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.SetActivityIndicatorStyle( const Value: TDPFActivityIndicatorViewStyle );
begin
  FActivityIndicatorStyle := Value;
{$IFDEF IOS}
  if Assigned( FUIActivityIndicatorView ) then
  begin
    FUIActivityIndicatorView.setActivityIndicatorViewStyle( Integer( FActivityIndicatorStyle ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FUIActivityIndicatorView ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIActivityIndicatorView.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIActivityIndicatorView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorView.SetColor( const Value: TAlphaColor );
begin
  FColor := Value;
{$IFDEF IOS}
  if FUIActivityIndicatorView <> nil then
  begin
    if FColor <> TAlphaColors.Null then
      FUIActivityIndicatorView.setColor( TColorToUIColor( FColor ) )
    else
      FUIActivityIndicatorView.SetColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFActivityIndicatorView.SetHidesWhenStopped( const Value: Boolean );
begin
  FHidesWhenStopped := Value;
  if Assigned( FUIActivityIndicatorView ) then
  begin
    FUIActivityIndicatorView.setHidesWhenStopped( value );
  end;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFActivityIndicatorViewTimerDelegate }
{$IFDEF IOS}

constructor TDPFActivityIndicatorViewTimerDelegate.Create( ADPFActivityIndicatorView: TDPFActivityIndicatorView );
begin
  inherited Create;
  FDPFActivityIndicatorView := ADPFActivityIndicatorView;
end;

// ------------------------------------------------------------------------------
procedure TDPFActivityIndicatorViewTimerDelegate.ondidTimer( timer: NSTimer );
begin
  FDPFActivityIndicatorView.StopAnimating;
  FDPFActivityIndicatorView.SendToBack;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
initialization

RegisterClass( TDPFLabel );

end.
