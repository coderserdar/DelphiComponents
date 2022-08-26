// ------------------------------------------------------------------------------
// DPF.Android.JDatePicker Component
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
unit DPF.Android.JDatePicker;

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
{$IFDEF ANDROID}
  DPF.Android.Widget,
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

  TDPFJDatePicker = class;

{$IFDEF ANDROID}

  TDPFJDatePickerOnDateChangedListener = class( TJavaLocal, JDatePicker_OnDateChangedListener )
  private
    FDPFJDatePicker: TDPFJDatePicker;
  public
    constructor create( ADPFJDatePicker: TDPFJDatePicker );
    procedure onDateChanged( view: JDatePicker; year: Integer; monthOfYear: Integer; dayOfMonth: Integer ); cdecl;
  end;
{$ENDIF}

  TDPFOnDateChanged = procedure( Sender: TObject; const year: Integer; const monthOfYear: Integer; const dayOfMonth: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJDatePicker = class( TDPFANDBaseControl )
  private
    //FOnDateChanged: TDPFOnDateChanged;

  protected
{$IFDEF ANDROID}
    FJDatePicker                        : JDatePicker;
    FDPFJDatePickerOnDateChangedListener: TDPFJDatePickerOnDateChangedListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJDatePicker: JDatePicker read FJDatePicker;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    // property OnDateChanged: TDPFOnDateChanged read FOnDateChanged write FOnDateChanged;

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
{ TDPFJDatePicker }
constructor TDPFJDatePicker.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'DatePicker';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJDatePicker := TJDatePicker.JavaClass.init( SharedActivity );
    end );
  JControl := FJDatePicker;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJDatePicker.Destroy;
begin
{$IFDEF ANDROID}
  FDPFJDatePickerOnDateChangedListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJDatePicker.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  FDPFJDatePickerOnDateChangedListener := TDPFJDatePickerOnDateChangedListener.create( self );

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      try
        //FJDatePicker.init( 2013, 1, 1, FDPFJDatePickerOnDateChangedListener );
      except
        on E: Exception do
          DPFNSLog( E.Message );
      end;
    end );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJDatePicker.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJDatePicker.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJDatePicker.Paint;
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
constructor TDPFJDatePickerOnDateChangedListener.create( ADPFJDatePicker: TDPFJDatePicker );
begin
  inherited create;
  FDPFJDatePicker := ADPFJDatePicker;
end;

// ------------------------------------------------------------------------------
procedure TDPFJDatePickerOnDateChangedListener.onDateChanged( view: JDatePicker; year: Integer; monthOfYear: Integer; dayOfMonth: Integer ); cdecl;
begin
  {if assigned( FDPFJDatePicker.FOnDateChanged ) then
    FDPFJDatePicker.FOnDateChanged( FDPFJDatePicker, year, monthOfYear, dayOfMonth );}
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
