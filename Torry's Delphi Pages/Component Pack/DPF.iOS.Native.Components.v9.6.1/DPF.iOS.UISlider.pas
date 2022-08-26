// ------------------------------------------------------------------------------
// DPF.iOS.UISlider Component
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
unit DPF.iOS.UISlider;

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
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFSlider = class;

  TDPFSliderKind         = ( skNormal, skCustom );
  TDPFSliderValueChanged = procedure( Sender: TObject; CurValue: Single ) of object;
  TDPFSliderTouchDown    = procedure( Sender: TObject ) of object;
  TDPFSliderTouchUp      = procedure( Sender: TObject ) of object;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFSliderValueChangedDelegate = interface( NSObject )
    ['{4131A3E8-F16A-47DA-B840-494263C7677C}']
    procedure valueChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFSliderValueChangedDelegate = class( TOCLocal )
  private
    FDPFSlider: TDPFSlider;
  public
    OnChanged: TDPFSliderValueChanged;
    constructor Create( ADPFSlider: TDPFSlider );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure valueChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------
  DPFSliderTouchDownDelegate = interface( NSObject )
    ['{C146E080-B049-4D18-912C-01C0CDFC972F}']
    procedure touchDown; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFSliderTouchDownDelegate = class( TOCLocal )
  private
    FDPFSlider: TDPFSlider;
  public
    OnChanged: TDPFSliderValueChanged;
    constructor Create( ADPFSlider: TDPFSlider );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure touchDown; cdecl;
  end;

  // ------------------------------------------------------------------------------
  DPFSliderTouchUpDelegate = interface( NSObject )
    ['{3CBE5A11-1C36-4822-B985-97CBCAC291A3}']
    procedure touchUp; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFSliderTouchUpDelegate = class( TOCLocal )
  private
    FDPFSlider: TDPFSlider;
  public
    OnChanged: TDPFSliderValueChanged;
    constructor Create( ADPFSlider: TDPFSlider );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure touchUp; cdecl;
  end;
{$ENDIF}

  TTrackBarOrientation = ( trHorizontal, trVertical );

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSlider = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUISlider                  : UISlider;
    FSliderValueChangedDelegate: TDPFSliderValueChangedDelegate;
    FSliderTouchDownDelegate   : TDPFSliderTouchDownDelegate;
    FSliderTouchUpDelegate     : TDPFSliderTouchUpDelegate;
{$ENDIF}
    FBackgroundColor      : TAlphaColor;
    FMaximumValue         : Integer;
    FMinimumValue         : Integer;
    FValue                : Single;
    FContinuous           : Boolean;
    FMinimumTrackTintColor: TAlphaColor;
    FMaximumTrackTintColor: TAlphaColor;
    FAnimated             : Boolean;
    FOnChanged            : TDPFSliderValueChanged;
    FThumbTintColor       : TAlphaColor;
    FMinimumTrackImage    : string;
    FThumbImage           : string;
    FMaximumTrackImage    : string;
    FSliderKind           : TDPFSliderKind;
    FOnTouchUp            : TDPFSliderTouchUp;
    FOnTouchDown          : TDPFSliderTouchDown;
    FEnabled              : Boolean;
    FIsTracking           : Boolean;
    FOrientation          : TTrackBarOrientation;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetMaximumValue( const cMaximumValue: Integer );
    procedure SetMinimumValue( const cMinimumValue: Integer );
    procedure SetValue( const cValue: Single );
    procedure SetOrientation( const Value: TTrackBarOrientation );
    procedure SetThumbImage( const Value: string );

  protected
    procedure Resize; override;
    procedure Move; override;
    procedure DoTouchDown; virtual; // SZ
    procedure DoTouchUp; virtual;   // SZ
    procedure SetEnabled( const Value: Boolean ); override; // SZ
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
    procedure MouseMove( Shift: TShiftState; X: Single; Y: Single ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single ); override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function IsTracking: Boolean;
  published
    property Animated       : Boolean read FAnimated write FAnimated default True;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;

    property MaximumTrackTintColor: TAlphaColor read FMaximumTrackTintColor write FMaximumTrackTintColor default TAlphaColors.Red;
    property MinimumTrackTintColor: TAlphaColor read FMinimumTrackTintColor write FMinimumTrackTintColor default TAlphaColors.Blue;
    property ThumbTintColor       : TAlphaColor read FThumbTintColor write FThumbTintColor default TAlphaColors.Gray;

    property MaximumTrackImage: string read FMaximumTrackImage write FMaximumTrackImage;
    property MinimumTrackImage: string read FMinimumTrackImage write FMinimumTrackImage;
    property ThumbImage       : string read FThumbImage write SetThumbImage;

    property MinimumValue: Integer read FMinimumValue write SetMinimumValue default 0;
    property MaximumValue: Integer read FMaximumValue write SetMaximumValue default 100;
    property Value       : Single read FValue write SetValue;
    property Continuous  : Boolean read FContinuous write FContinuous default True;
    property SliderKind  : TDPFSliderKind read FSliderKind write FSliderKind default skNormal;

    property OnChanged  : TDPFSliderValueChanged read FOnChanged write FOnChanged;
    property OnTouchDown: TDPFSliderTouchDown read FOnTouchDown write FOnTouchDown;
    property OnTouchUp  : TDPFSliderTouchUp read FOnTouchUp write FOnTouchUp;
    property Enabled    : Boolean read FEnabled write SetEnabled default True;

    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
{$IFDEF DELPHIXE7}
    property Size;
{$ENDIF}
  end;

implementation

uses
  System.Math, System.Math.Vectors;

// ------------------------------------------------------------------------------
{ TDPFSlider }
// ------------------------------------------------------------------------------
constructor TDPFSlider.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption         := 'Slider';
  FBackgroundColor       := TAlphaColors.Null;
  FMaximumTrackTintColor := TAlphaColors.Red;
  FMinimumTrackTintColor := TAlphaColors.Blue;
  FThumbTintColor        := TAlphaColors.Gray;
  FMinimumValue          := 0;
  FMaximumValue          := 100;
  FValue                 := 50.0;
  FContinuous            := True;
  FAnimated              := True;
  FSliderKind            := skNormal;
  FEnabled               := True;
{$IFDEF IOS}
  FSliderValueChangedDelegate := TDPFSliderValueChangedDelegate.Create( Self );
  FSliderTouchDownDelegate    := TDPFSliderTouchDownDelegate.Create( Self );
  FSliderTouchUpDelegate      := TDPFSliderTouchUpDelegate.Create( Self );
  FUISlider                   := TUISlider.Create;
  FUIControl                  := FUISlider;
{$ENDIF}
  AutoCapture := True; // SZ
end;

// ------------------------------------------------------------------------------
destructor TDPFSlider.Destroy;
begin
{$IFDEF IOS}
  FUISlider.removeTarget( FSliderValueChangedDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ), // action
    UIControlEventValueChanged ); // event

  FUISlider.removeTarget( FSliderTouchUpDelegate.GetObjectID, // target
    Sel_getUid( 'touchUp' ), // action
    UIControlEventTouchUpInside + UIControlEventTouchUpOutside ); // event

  FUISlider.removeTarget( FSliderTouchDownDelegate.GetObjectID, // target
    Sel_getUid( 'touchDown' ), // action
    UIControlEventTouchDown ); // event

  FSliderValueChangedDelegate.DisposeOf;
  FSliderTouchDownDelegate.DisposeOf;
  FSliderTouchUpDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

procedure TDPFSlider.DoTouchDown;
begin
  FIsTracking := True;
  if Assigned( OnTouchDown ) then
    OnTouchDown( Self );
end;

procedure TDPFSlider.DoTouchUp;
begin
  FIsTracking := False;
  if Assigned( OnTouchUp ) then
    OnTouchUp( Self );
end;

function TDPFSlider.IsTracking: Boolean;
begin
  Result := FIsTracking;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFSlider.Loaded;
var
  Image: UIImage;
begin

  SetEnabled( Enabled );
  FUISlider.setHidden( not Visible );

  FUISlider.setMinimumValue( FMinimumValue );
  FUISlider.setMaximumValue( FMaximumValue );
  FUISlider.setValue( FValue, FAnimated );
  FUISlider.setContinuous( FContinuous );

  if FSliderKind = skCustom then
  begin
    if FMaximumTrackImage <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FMaximumTrackImage ) ) );
      FUISlider.setMaximumTrackImage( Image, UIControlStateNormal );
      // Image.release;
      Image := nil;
    end
    else
    begin
      // TODO: There is a bug in iOS 7.1: // http://stackoverflow.com/questions/22345668/uislider-setmaximumtracktintcolor-in-ios-7-1
      if ( FMaximumTrackTintColor = TAlphaColors.Null ) then
      begin
        if Assigned( FUISlider.MaximumTrackTintColor ) then
          FUISlider.setMaximumTrackTintColor( nil )
      end
      else
        FUISlider.setMaximumTrackTintColor( TColorToUIColor( FMaximumTrackTintColor ) );
    end;

    if FMinimumTrackImage <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FMinimumTrackImage ) ) );
      FUISlider.setMinimumTrackImage( Image, UIControlStateNormal );
      // Image.release;
      Image := nil;
    end
    else
    begin
      if ( FMinimumTrackTintColor = TAlphaColors.Null ) then
      begin
        if Assigned( FUISlider.MinimumTrackTintColor ) then
          FUISlider.setMinimumTrackTintColor( nil )
      end
      else
        FUISlider.setMinimumTrackTintColor( TColorToUIColor( FMinimumTrackTintColor ) );
    end;

    if FThumbImage <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FThumbImage ) ) );
      FUISlider.setThumbImage( Image, UIControlStateNormal );
      FUISlider.setThumbImage( Image, UIControlStateHighlighted );
      // Image.release;
      Image := nil;
    end
    else
    begin
      if ( FThumbTintColor = TAlphaColors.Null ) then
      begin
        if Assigned( FUISlider.ThumbTintColor ) then
          FUISlider.setThumbTintColor( nil )
      end
      else
        FUISlider.setThumbTintColor( TColorToUIColor( FThumbTintColor ) );
    end;

    if FBackgroundColor = TAlphaColors.Null then
      FUISlider.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUISlider.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );

    // SZ
    case FOrientation of
      trHorizontal:
        FUISlider.setTransform( CGAffineTransformIdentity );
      trVertical:
        FUISlider.settransform( CGAffineTransformMakeRotation( pi * -0.5 ) );
    end;
  end;
  FUISlider.AddTarget( FSliderValueChangedDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ), // action
    UIControlEventValueChanged ); // event

  FUISlider.AddTarget( FSliderTouchUpDelegate.GetObjectID, // target
    Sel_getUid( 'touchUp' ),                               // action
    UIControlEventTouchUpInside + UIControlEventTouchUpOutside ); // event

  FUISlider.AddTarget( FSliderTouchDownDelegate.GetObjectID, // target
    Sel_getUid( 'touchDown' ), // action
    UIControlEventTouchDown ); // event

  AddSubView( Self, ParentControl );
  Resize;
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSlider.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if FUISlider <> nil then FUISlider.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ELSE}
  // Added by Fenistil
  // if FSliderKind = skNormal then
  // Height := iOS_GUI_Bitmaps.Slider.Tab.Height; // SZ removed - control is too small on iOS7 or with custom sliders
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFSlider.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  inherited;
  DoTouchDown;
  X := EnsureRange( X, 0, Width );
  Y := EnsureRange( Y, 0, Height );
{$IFDEF DELPHIXE7}
  case Orientation of
    trHorizontal:
      Value := EnsureRange( ( X / Size.Width ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
    trVertical:
      Value := EnsureRange( ( ( Size.Height - Y ) / Size.Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
  end;
{$ELSE}
  case Orientation of
    trHorizontal:
      Value := EnsureRange( ( X / Width ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
    trVertical:
      Value := EnsureRange( ( ( Height - Y ) / Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
  end;
{$ENDIF}
end;

procedure TDPFSlider.MouseMove( Shift: TShiftState; X, Y: Single );
begin
  inherited;

  X := EnsureRange( X, 0, Width );
  Y := EnsureRange( Y, 0, Height );

{$IFDEF XE7}
  if IsTracking then
    case Orientation of
      trHorizontal:
        Value := ( X / Size.Width ) * ( MaximumValue - MinimumValue );
      trVertical:
        Value := EnsureRange( ( ( Size.Height - Y ) / Size.Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
    end;
{$ELSE}
  if IsTracking then
    case Orientation of
      trHorizontal:
        Value := ( X / Width ) * ( MaximumValue - MinimumValue );
      trVertical:
        Value := EnsureRange( ( ( Height - Y ) / Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
    end;
{$ENDIF}
end;

procedure TDPFSlider.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  inherited;

  X := EnsureRange( X, 0, Width );
  Y := EnsureRange( Y, 0, Height );

{$IFDEF XE7}
  case Orientation of
    trHorizontal:
      Value := ( X / Size.Width ) * ( MaximumValue - MinimumValue );
    trVertical:
      Value := EnsureRange( ( ( Size.Height - Y ) / Size.Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
  end;
{$ELSE}
  case Orientation of
    trHorizontal:
      Value := ( X / Width ) * ( MaximumValue - MinimumValue );
    trVertical:
      Value := EnsureRange( ( ( Height - Y ) / Height ) * ( MaximumValue - MinimumValue ), MinimumValue, MaximumValue );
  end;
{$ENDIF}
  DoTouchUp;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSlider.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFSlider.Paint;
var
  m, v, px        : double;
  pos, top, Center: integer;
  Matrix          : TMatrix;
  w               : Single;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  Matrix := Canvas.Matrix;
  if Orientation = trVertical then
  begin
    Canvas.MultiplyMatrix( TMatrix.CreateRotation( pi * -0.5 ) );
    Canvas.MultiplyMatrix( TMatrix.CreateTranslation( -Height, 0 ) );
  end;
  try
    m := MaximumValue - MinimumValue;
    if m <= 0 then
      m := 1;
    v   := Value - MinimumValue;
    case Orientation of
      trHorizontal:
        begin
          px     := Width - iOS_GUI_Bitmaps.Slider.Tab.Width;
          Center := Round( Height / 2 );
          w      := Width;
          // h      := Height;
        end;
    else { trVertical: }
      begin
        px     := Height - iOS_GUI_Bitmaps.Slider.Tab.Width;
        Center := Round( Width / 2 );
        w      := Height;
        // h      := Width;
      end;
    end;

    pos := Round( v / m * px );
    // top := Round(iOS_GUI_Bitmaps.Slider.Tab.Height / 2 - iOS_GUI_Bitmaps.Slider.BarOff.Height / 2 );
    top := Round( Center - iOS_GUI_Bitmaps.Slider.BarOff.Height / 2 );
    // Blue bar ending
    Canvas.DrawBitmap( iOS_GUI_Bitmaps.Slider.BarOn, RectF( 0, 0, 4, iOS_GUI_Bitmaps.Slider.BarOn.Height ), RectF( 3, top, 7, top + iOS_GUI_Bitmaps.Slider.BarOn.Height ), 1 );
    // Blue bar
    if pos >= 7 then
      Canvas.DrawBitmap( iOS_GUI_Bitmaps.Slider.BarOn, RectF( 5, 0, 6, iOS_GUI_Bitmaps.Slider.BarOn.Height ), RectF( 7, top, pos + iOS_GUI_Bitmaps.Slider.Tab.Width / 2, top + iOS_GUI_Bitmaps.Slider.BarOn.Height ), 1 );
    // White bar
    Canvas.DrawBitmap( iOS_GUI_Bitmaps.Slider.BarOff, RectF( iOS_GUI_Bitmaps.Slider.BarOff.Width - 6, 0, iOS_GUI_Bitmaps.Slider.BarOff.Width - 5, iOS_GUI_Bitmaps.Slider.BarOff.Height ), RectF( pos + iOS_GUI_Bitmaps.Slider.Tab.Width / 2, top, w - 7, top + iOS_GUI_Bitmaps.Slider.BarOn.Height ), 1 );
    // White bar ending
    Canvas.DrawBitmap( iOS_GUI_Bitmaps.Slider.BarOff, RectF( iOS_GUI_Bitmaps.Slider.BarOff.Width - 4, 0, iOS_GUI_Bitmaps.Slider.BarOff.Width, iOS_GUI_Bitmaps.Slider.BarOff.Height ), RectF( w - 7, top, w - 3, top + iOS_GUI_Bitmaps.Slider.BarOn.Height ), 1 );
    // Tab
    top := Center - Round( iOS_GUI_Bitmaps.Slider.Tab.Height / 2 );
    Canvas.DrawBitmap( iOS_GUI_Bitmaps.Slider.Tab, RectF( 0, 0, iOS_GUI_Bitmaps.Slider.Tab.Width, iOS_GUI_Bitmaps.Slider.Tab.Height ), RectF( pos, Top, pos + iOS_GUI_Bitmaps.Slider.Tab.Width, Top + iOS_GUI_Bitmaps.Slider.Tab.Height ), 1 );
  finally
    Canvas.SetMatrix( Matrix );
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSlider.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUISlider <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUISlider.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUISlider.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSlider.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUISlider ) then
    FUISlider.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSlider.SetMaximumValue( const cMaximumValue: Integer );
begin
  FMaximumValue := cMaximumValue;
  if Value > FMaximumValue then
    Value := FMaximumValue;
  if FMinimumValue > FMaximumValue then
    FMinimumValue := FMaximumValue;
{$IFDEF IOS}
  if FUISlider <> nil then
  begin
    FUISlider.SetMinimumValue( FMinimumValue );
    FUISlider.SetMaximumValue( FMaximumValue );
    FUISlider.SetValue( FValue, FAnimated );
  end;
{$ELSE}
  // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSlider.SetMinimumValue( const cMinimumValue: Integer );
begin
  FMinimumValue := cMinimumValue;
  if Value < FMinimumValue then
    Value := FMinimumValue;
  if FMaximumValue < FMinimumValue then
    FMaximumValue := FMinimumValue;
{$IFDEF IOS}
  if FUISlider <> nil then
  begin
    FUISlider.SetMinimumValue( FMinimumValue );
    FUISlider.SetMaximumValue( FMaximumValue );
    FUISlider.SetValue( FValue, FAnimated );
  end;
{$ELSE}
  // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

procedure TDPFSlider.SetOrientation( const Value: TTrackBarOrientation );
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if ComponentState * [csLoading, csUpdating] = [] then
      SetBounds( Position.X, Position.Y, Height, Width );
{$IFDEF IOS}
    if FUISlider <> nil then
    begin
      case FOrientation of
        trHorizontal:
          FUISlider.setTransform( CGAffineTransformIdentity );
        trVertical:
          FUISlider.settransform( CGAffineTransformMakeRotation( pi * -0.5 ) );
      end;
    end;
{$ENDIF}
  end;
end;

procedure TDPFSlider.SetThumbImage( const Value: string ); // SZ added
{$IFDEF IOS}
var
  Image: UIImage;
{$ENDIF}
begin
  if FThumbImage <> Value then
  begin
    FThumbImage := Value;
{$IFDEF IOS}
    if Assigned( FUISlider ) then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FThumbImage ) ) );
      FUISlider.setThumbImage( Image, UIControlStateNormal );
      FUISlider.setThumbImage( Image, UIControlStateHighlighted );
      // Image.release;
      Image := nil;
    end;
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFSlider.SetValue( const cValue: Single );
begin
  if not SameValue( cValue, Value ) then
  begin
    if cValue < FMinimumValue then
      FValue := FMinimumValue
    else if cValue > FMaximumValue then
      FValue := FMaximumValue
    else
      FValue := cValue;

{$IFDEF IOS}
    if FUISlider <> nil then
    begin
      FUISlider.SetMinimumValue( FMinimumValue );
      FUISlider.SetMaximumValue( FMaximumValue );
      FUISlider.SetValue( FValue, FAnimated );
    end;
{$ELSE}
    if Assigned( FOnChanged ) then
      FOnChanged( Self, cValue );
    // Added by Fenistil
    Invalidate;
{$ENDIF}
  end;
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFSliderValueDelegate }

constructor TDPFSlidervalueChangedDelegate.Create( ADPFSlider: TDPFSlider );
begin
  inherited Create;
  FDPFSlider := ADPFSlider;
end;

// ------------------------------------------------------------------------------
function TDPFSliderValueChangedDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFSliderValueChangedDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSliderValueChangedDelegate.valueChanged; cdecl;
begin
  FDPFSlider.FValue := FDPFSlider.FUISlider.Value;
  if Assigned( FDPFSlider.OnChanged ) then
  begin
    FDPFSlider.OnChanged( FDPFSlider, FDPFSlider.FValue );
  end;
end;

// ------------------------------------------------------------------------------
{ TDPFSliderTouchDownDelegate }

constructor TDPFSliderTouchDownDelegate.Create( ADPFSlider: TDPFSlider );
begin
  inherited Create;
  FDPFSlider := ADPFSlider;
end;

// ------------------------------------------------------------------------------
function TDPFSliderTouchDownDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFSliderTouchDownDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSliderTouchDownDelegate.touchDown; cdecl;
begin
  FDPFSlider.DoTouchDown; // SZ
end;

// ------------------------------------------------------------------------------
{ TDPFSliderTouchUpDelegate }

constructor TDPFSliderTouchUpDelegate.Create( ADPFSlider: TDPFSlider );
begin
  inherited Create;
  FDPFSlider := ADPFSlider;
end;

// ------------------------------------------------------------------------------
function TDPFSliderTouchUpDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFSliderTouchUpDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSliderTouchUpDelegate.touchUp; cdecl;
begin
  FDPFSlider.DoTouchUp; // SZ
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
