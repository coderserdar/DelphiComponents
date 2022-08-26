// ------------------------------------------------------------------------------
// DPF.iOS.UISwitch Component
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
unit DPF.iOS.UISwitch;

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
  TDPFSwitchValueChanged = procedure( Sender: TObject; ISON: Boolean ) of object;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFSwitchDelegate = interface( NSObject )
    ['{262746F3-D5E3-4036-B247-44833D709C0C}']
    procedure valueChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFSwitchDelegate = class( TOCLocal )
  private
    FParent: TComponent;
  public
    OnChanged: TDPFSwitchValueChanged;
    constructor Create( Owner: TComponent );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure valueChanged; cdecl;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSwitch = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUISwitch      : UISwitch;
    FSwitchDelegate: TDPFSwitchDelegate;
{$ENDIF}
    FBackgroundColor           : TAlphaColor;
    FContentVerticalAlignment  : TDPFContentVerticalAlignment;
    FContentHorizontalAlignment: TDPFContentHorizontalAlignment;
    FOnChanged                 : TDPFSwitchValueChanged;
    FIsON                      : Boolean;
    FOnTintColor               : TAlphaColor;
    FEnabled                   : Boolean;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
    procedure SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
    procedure SetIsON( const Value: Boolean );
    procedure SetOnTintColor( const Value: TAlphaColor );
    procedure SetEnabled( const Value: Boolean );

  protected
{$IFDEF IOS}
    FOriginSize: CGrect;
{$ENDIF}
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
    property BackgroundColor           : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property OnTintColor               : TAlphaColor read FOnTintColor write SetOnTintColor default TAlphaColors.Null;
    property ContentHorizontalAlignment: TDPFContentHorizontalAlignment read FContentHorizontalAlignment write SetContentHorizontalAlignment default cchaCenter;
    property ContentVerticalAlignment  : TDPFContentVerticalAlignment read FContentVerticalAlignment write SetContentVerticalAlignment default ccvaCenter;
    property ISON                      : Boolean read FIsON write SetIsON default True;
    property OnChanged                 : TDPFSwitchValueChanged read FOnChanged write FOnChanged;
    property Enabled                   : Boolean read FEnabled write SetEnabled default True;

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
{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFSwitchDelegate }
constructor TDPFSwitchDelegate.Create( Owner: TComponent );
begin
  inherited Create;
  FParent := Owner;
end;

// ------------------------------------------------------------------------------
function TDPFSwitchDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFSwitchDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitchDelegate.valueChanged; cdecl;
begin
  TDPFSwitch( FParent ).ISON := TDPFSwitch( FParent ).FUISwitch.ISON;
  if Assigned( OnChanged ) then
  begin
    OnChanged( FParent, TDPFSwitch( FParent ).ISON );
  end;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFSwitch }
constructor TDPFSwitch.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption              := 'Switch';
  FContentHorizontalAlignment := cchaCenter;
  FContentVerticalAlignment   := ccvaCenter;
  FIsON                       := True;
  FEnabled                    := true;
{$IFDEF IOS}
  FSwitchDelegate := TDPFSwitchDelegate.Create( Self );
  FUISwitch       := TUISwitch.Wrap( TUISwitch.Alloc.initWithFrame( CGRectMake( 0, 0, 0, 0 ) ) );
  FOriginSize     := FUISwitch.frame;
  FUIControl      := FUISwitch;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFSwitch.Destroy;
begin
{$IFDEF IOS}
  FUISwitch.removeTarget( FSwitchDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ),                      // action
    UIControlEventValueChanged );                      // event
  FSwitchDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFSwitch.Loaded;
begin

  FUISwitch.SetContentVerticalAlignment( Integer( FContentVerticalAlignment ) );
  FUISwitch.SetContentHorizontalAlignment( Integer( FContentHorizontalAlignment ) );

  FUISwitch.setOn( FIsON );
  setEnabled( FEnabled );
  FUISwitch.setHidden( not Visible );

  if FOnTintColor = TAlphaColors.Null then
  begin
    if Assigned( FUISwitch.OnTintColor ) then
      FUISwitch.SetOnTintColor( nil )
  end
  else
    FUISwitch.SetOnTintColor( TColorToUIColor( FOnTintColor ) );

  if FBackgroundColor = TAlphaColors.Null then
    FUISwitch.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    FUISwitch.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );

  FSwitchDelegate.OnChanged := FOnChanged;

  FUISwitch.AddTarget( FSwitchDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ),                   // action
    UIControlEventValueChanged );                   // event

  Resize;
  FUISwitch.AutoResizingMask;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSwitch.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    FOriginSize.origin.x := Position.X + ( Width - FOriginSize.size.width ) / 2;
    FOriginSize.origin.y := Position.Y + ( Height - FOriginSize.size.height ) / 2;
    FUISwitch.SetFrame( FOriginSize );
  end;
{$ELSE}
  // Added by Fenistil
  Width  := iOS_GUI_Bitmaps.Switch.IsOn.Width;
  Height := iOS_GUI_Bitmaps.Switch.IsOn.Height;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFSwitch.Paint;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  if ISON then
    BitmapAsBackground( Self, iOS_GUI_Bitmaps.Switch.IsOn )
  else
    BitmapAsBackground( Self, iOS_GUI_Bitmaps.Switch.IsOff );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUISwitch.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUISwitch.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
begin
  FContentHorizontalAlignment := Value;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    FUISwitch.SetContentHorizontalAlignment( Integer( FContentHorizontalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
begin
  FContentVerticalAlignment := Value;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    FUISwitch.SetContentVerticalAlignment( Integer( FContentVerticalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUISwitch ) then
    FUISwitch.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetIsON( const Value: Boolean );
begin
  FIsON := Value;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    FUISwitch.setOn( FIsON );
  end;
{$ELSE}
  // Added by Fenistil
  InvalidateRect( RectF( 0, 0, Width, Height ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSwitch.SetOnTintColor( const Value: TAlphaColor );
begin
  FOnTintColor := Value;
{$IFDEF IOS}
  if FUISwitch <> nil then
  begin
    if FOnTintColor <> TAlphaColors.Null then
      FUISwitch.SetOnTintColor( TColorToUIColor( FOnTintColor ) )
    else
      FUISwitch.SetOnTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
