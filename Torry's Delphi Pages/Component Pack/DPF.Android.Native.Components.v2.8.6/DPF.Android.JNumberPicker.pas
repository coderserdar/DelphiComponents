// ------------------------------------------------------------------------------
// DPF.Android.JNumberPicker Component
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
unit DPF.Android.JNumberPicker;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

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
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJNumberPicker = class;

{$IFDEF ANDROID}

  TJNumberPickerOnValueChangeListener = class( TJavaLocal, JNumberPicker_OnValueChangeListener )
  private
    FDPFJNumberPicker: TDPFJNumberPicker;
  public
    constructor create( ADPFJNumberPicker: TDPFJNumberPicker );

    procedure onValueChange( picker: JNumberPicker; oldVal: Integer; newVal: Integer ); cdecl;
  end;
{$ENDIF}

  TDPFNumberPickerOnValueChange = procedure( sender: TObject; const oldVal: Integer; const newVal: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJNumberPicker = class( TDPFANDBaseControl )
  private
    FMinValue     : Integer;
    FValue        : Integer;
    FMaxValue     : Integer;
    FOnValueChange: TDPFNumberPickerOnValueChange;
    procedure SetMaxValue( const Value: Integer );
    procedure SetMinValue( const Value: Integer );
    procedure SetValue( const Value: Integer );

  protected
{$IFDEF ANDROID}
    FJNumberPicker                     : JNumberPicker;
    FJNumberPickerOnValueChangeListener: TJNumberPickerOnValueChangeListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJNumberPicker: JNumberPicker read FJNumberPicker;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property MinValue     : Integer read FMinValue write SetMinValue default 0;
    property MaxValue     : Integer read FMaxValue write SetMaxValue default 10;
    property Value        : Integer read FValue write SetValue default 1;
    property OnValueChange: TDPFNumberPickerOnValueChange read FOnValueChange write FOnValueChange;

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
    property Visible;
    property OnClick;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJNumberPicker }
constructor TDPFJNumberPicker.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'NumberPicker';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  FMinValue        := 0;
  FMaxValue        := 10;
  FValue           := 1;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJNumberPicker := TJNumberPicker.JavaClass.init( SharedActivity );
      FJNumberPickerOnValueChangeListener := TJNumberPickerOnValueChangeListener.create( self );
      FJNumberPicker.setOnValueChangedListener( FJNumberPickerOnValueChangeListener );
    end );
  JControl := FJNumberPicker;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJNumberPicker.Destroy;
begin
{$IFDEF ANDROID}
  FJNumberPickerOnValueChangeListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJNumberPicker.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  SetMinValue( FMinValue );
  SetMaxValue( FMaxValue );
  SetValue( FValue );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.SetMaxValue( const Value: Integer );
begin
  FMaxValue := Value;
{$IFDEF ANDROID}
  if assigned( FJNumberPicker ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJNumberPicker.setMaxValue( value );
      end );
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.SetMinValue( const Value: Integer );
begin
  FMinValue := Value;
{$IFDEF ANDROID}
  if assigned( FJNumberPicker ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJNumberPicker.setMinValue( value );
      end );
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.SetValue( const Value: Integer );
begin
  FValue := Value;
{$IFDEF ANDROID}
  if assigned( FJNumberPicker ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJNumberPicker.setValue( value );
      end );
  end;

{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJNumberPicker.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TJNumberPickerOnValueChangeListener.create( ADPFJNumberPicker: TDPFJNumberPicker );
begin
  inherited create;
  FDPFJNumberPicker := ADPFJNumberPicker;
end;

// ------------------------------------------------------------------------------
procedure TJNumberPickerOnValueChangeListener.onValueChange( picker: JNumberPicker; oldVal: Integer; newVal: Integer ); cdecl;
begin
  if Assigned( FDPFJNumberPicker.FOnValueChange ) then
    FDPFJNumberPicker.FOnValueChange( FDPFJNumberPicker, oldVal, newVal );
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
