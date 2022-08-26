// ------------------------------------------------------------------------------
// DPF.iOS.UIStepper Component
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
unit DPF.iOS.UIStepper;

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
  TDPFStepperValueChanged = procedure( Sender: TObject; CurValue: double ) of object;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFStepperDelegate = interface( NSObject )
    ['{FC86E92A-8263-4E35-90E6-E02FF506E62F}']
    procedure valueChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFStepperDelegate = class( TOCLocal )
  private
    FParent: TComponent;
  public
    OnChanged: TDPFStepperValueChanged;
    constructor Create( Owner: TComponent );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure valueChanged; cdecl;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFStepper = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIStepper      : UIStepper;
    FStepperDelegate: TDPFStepperDelegate;
{$ENDIF}
    FBackgroundColor           : TAlphaColor;
    FContentVerticalAlignment  : TDPFContentVerticalAlignment;
    FContentHorizontalAlignment: TDPFContentHorizontalAlignment;
    FOnChanged                 : TDPFStepperValueChanged;
    FMaximumValue              : double;
    FMinimumValue              : double;
    FValue                     : double;
    FContinuous                : Boolean;
    FWraps                     : Boolean;
    FStepValue                 : double;
    FAutorepeat                : Boolean;
    FEnabled                   : Boolean;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
    procedure SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
    procedure SetMaximumValue( const Value: double );
    procedure SetMinimumValue( const Value: double );
    procedure SetValue( const Value: double );
    procedure SetWraps( const Value: Boolean );
    procedure SetStepValue( const Value: double );
    procedure SetAutorepeat( const Value: Boolean );
    procedure SetEnabled( const Value: Boolean );

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
    property BackgroundColor           : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property ContentHorizontalAlignment: TDPFContentHorizontalAlignment read FContentHorizontalAlignment write SetContentHorizontalAlignment default cchaCenter;
    property ContentVerticalAlignment  : TDPFContentVerticalAlignment read FContentVerticalAlignment write SetContentVerticalAlignment default ccvaCenter;
    property MinimumValue              : double read FMinimumValue write SetMinimumValue;
    property MaximumValue              : double read FMaximumValue write SetMaximumValue;
    property Value                     : double read FValue write SetValue;
    property Continuous                : Boolean read FContinuous write FContinuous default True;
    property Wraps                     : Boolean read FWraps write SetWraps default False;
    property Autorepeat                : Boolean read FAutorepeat write SetAutorepeat default True;
    property StepValue                 : double read FStepValue write SetStepValue;
    property OnChanged                 : TDPFStepperValueChanged read FOnChanged write FOnChanged;
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
{ TDPFStepperDelegate }
constructor TDPFStepperDelegate.Create( Owner: TComponent );
begin
  inherited Create;
  FParent := Owner;
end;

// ------------------------------------------------------------------------------
function TDPFStepperDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFStepperDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFStepperDelegate.valueChanged; cdecl;
begin
  TDPFStepper( FParent ).FValue := TDPFStepper( FParent ).FUIStepper.Value;
  if Assigned( OnChanged ) then
  begin
    OnChanged( FParent, TDPFStepper( FParent ).FValue );
  end;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFStepper }
constructor TDPFStepper.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption              := 'Stepper';
  FContentHorizontalAlignment := cchaCenter;
  FContentVerticalAlignment   := ccvaCenter;
  FMinimumValue               := 0.0;
  FMaximumValue               := 100.0;
  FValue                      := 50.0;
  FContinuous                 := True;
  FAutorepeat                 := True;
  FWraps                      := False;
  FStepValue                  := 1.0;
  FEnabled                    := true;

{$IFDEF IOS}
  FStepperDelegate := TDPFStepperDelegate.Create( Self );
  FUIStepper       := TUIStepper.Create;
  FUIControl       := FUIStepper;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFStepper.Destroy;
begin
{$IFDEF IOS}
  FUIStepper.removeTarget( FStepperDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ),                        // action
    UIControlEventValueChanged );                        // event
  FStepperDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFStepper.Loaded;
begin
  FUIStepper.setContentVerticalAlignment( Integer( FContentVerticalAlignment ) );
  FUIStepper.setContentHorizontalAlignment( Integer( FContentHorizontalAlignment ) );

  FUIStepper.setMinimumValue( FMinimumValue );
  FUIStepper.setMaximumValue( FMaximumValue );
  FUIStepper.setValue( FValue );
  FUIStepper.setContinuous( FContinuous );
  FUIStepper.setWraps( FWraps );
  FUIStepper.setStepValue( FStepValue );
  FUIStepper.setAutorepeat( FAutorepeat );
  setEnabled( Enabled );
  if FBackgroundColor = TAlphaColors.Null then
    FUIStepper.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
  else
    FUIStepper.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );

  FUIStepper.setHighlighted( True );

  FStepperDelegate.OnChanged := FOnChanged;

  FUIStepper.AddTarget( FStepperDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ),                     // action
    UIControlEventValueChanged );                     // event

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFStepper.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if FUIStepper <> nil then FUIStepper.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ELSE}
  // Added by Fenistil
  Width  := iOS_GUI_Bitmaps.Stepper.Width;
  Height := iOS_GUI_Bitmaps.Stepper.Height;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFStepper.Paint;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  BitmapAsBackground( Self, iOS_GUI_Bitmaps.Stepper );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetAutorepeat( const Value: Boolean );
begin
  FAutorepeat := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setAutorepeat( FAutorepeat );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIStepper.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIStepper.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetContentHorizontalAlignment( const Value: TDPFContentHorizontalAlignment );
begin
  FContentHorizontalAlignment := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setContentHorizontalAlignment( Integer( FContentHorizontalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetContentVerticalAlignment( const Value: TDPFContentVerticalAlignment );
begin
  FContentVerticalAlignment := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setContentVerticalAlignment( Integer( FContentVerticalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUIStepper ) then
    FUIStepper.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetMaximumValue( const Value: double );
begin
  FMaximumValue := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setMaximumValue( FMinimumValue );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetMinimumValue( const Value: double );
begin
  FMinimumValue := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setMinimumValue( FMinimumValue );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetStepValue( const Value: double );
begin
  if Value = 0.0 then
    exit;

  FStepValue := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setStepValue( FStepValue );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetValue( const Value: double );
begin
  FValue := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setValue( FValue );
    if Assigned( OnChanged ) then
      OnChanged( FParent, Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFStepper.SetWraps( const Value: Boolean );
begin
  FWraps := Value;
{$IFDEF IOS}
  if FUIStepper <> nil then
  begin
    FUIStepper.setWraps( FWraps );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
end.
