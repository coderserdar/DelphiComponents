// ------------------------------------------------------------------------------
// DPF.Android.JTimePickerDialog Component
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
unit DPF.Android.JTimePickerDialog;

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
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}

{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJTimePickerDialog = class;

{$IFDEF ANDROID}

  TDPFJTimePickerDialogOnTimeSetListener = class( TJavaLocal, JTimePickerDialog_OnTimeSetListener )
  private
    FDPFJTimePickerDialog: TDPFJTimePickerDialog;
  public
    constructor create( ADPFJTimePickerDialog: TDPFJTimePickerDialog );
    procedure onTimeSet( view: JTimePicker; hourOfDay: integer; minute: Integer ); cdecl;
  end;

  TDPFJTimePickerDialogOnCancelListener = class( TJavaLocal, JDialogInterface_OnCancelListener )
  private
    FDPFJTimePickerDialog: TDPFJTimePickerDialog;
  public
    constructor create( ADPFJTimePickerDialog: TDPFJTimePickerDialog );
    procedure onCancel( dialog: JDialogInterface ); cdecl;
  end;

{$ENDIF}

  TDPFTimePickerDialogOnTimeSet = procedure( sender: TObject; const HourOfDay: integer; const Minute: Integer ) of object;
  TDPFTimePickerDialogOnCancel  = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJTimePickerDialog = class( TComponent )
  private
    FOnTimeSet: TDPFTimePickerDialogOnTimeSet;
    FOnCancel : TDPFTimePickerDialogOnCancel;
  protected
{$IFDEF ANDROID}
    FJTimePickerDialog                    : JTimePickerDialog;
    FDPFJTimePickerDialogOnTimeSetListener: TDPFJTimePickerDialogOnTimeSetListener;
    FDPFJTimePickerDialogOnCancelListener : TDPFJTimePickerDialogOnCancelListener;
{$ENDIF}
    procedure FreeAndNilObjects;
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Show( const Title: string; const &Message: string; const HourOfDay: integer; const Minute: integer; const is24HourView: boolean );

  published
    property OnTimeSet: TDPFTimePickerDialogOnTimeSet read FOnTimeSet write FOnTimeSet;
    property OnCancel : TDPFTimePickerDialogOnCancel read FOnCancel write FOnCancel;

  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJTimePickerDialog }
constructor TDPFJTimePickerDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF ANDROID}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJTimePickerDialog.Destroy;
begin
{$IFDEF ANDROID}
  FreeAndNilObjects;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTimePickerDialog.FreeAndNilObjects;
begin
{$IFDEF ANDROID}
  FJTimePickerDialog                     := nil;
  FDPFJTimePickerDialogOnTimeSetListener := nil;
  FDPFJTimePickerDialogOnCancelListener  := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJTimePickerDialog.Loaded;
begin
  // Resize; // Very Important for Embbeded Forms, Frames

  // addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJTimePickerDialog.Show( const Title: string; const &Message: string; const HourOfDay: integer; const Minute: integer; const is24HourView: boolean );
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    begin
      FDPFJTimePickerDialogOnTimeSetListener := TDPFJTimePickerDialogOnTimeSetListener.create( self );
      FJTimePickerDialog := TJTimePickerDialog.JavaClass.init( SharedActivity, FDPFJTimePickerDialogOnTimeSetListener, HourOfDay, Minute, is24HourView );
      FJTimePickerDialog.setMessage( StrToJCharSequence( &Message ) );
      FJTimePickerDialog.setTitle( StrToJCharSequence( Title ) );

      FDPFJTimePickerDialogOnCancelListener := TDPFJTimePickerDialogOnCancelListener.create( self );
      FJTimePickerDialog.setOnCancelListener( FDPFJTimePickerDialogOnCancelListener );

      FJTimePickerDialog.Show;
    end );
{$ENDIF}
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TDPFJTimePickerDialogOnTimeSetListener.create( ADPFJTimePickerDialog: TDPFJTimePickerDialog );
begin
  inherited create;
  FDPFJTimePickerDialog := ADPFJTimePickerDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTimePickerDialogOnTimeSetListener.onTimeSet( view: JTimePicker; hourOfDay: integer; minute: Integer ); cdecl;
begin
  if assigned( FDPFJTimePickerDialog.FOnTimeSet ) then
    FDPFJTimePickerDialog.FOnTimeSet( FDPFJTimePickerDialog, hourOfDay, minute );

  CallInUIThread(
    procedure
    begin
      FDPFJTimePickerDialog.FreeAndNilObjects;
    end );
end;

// ------------------------------------------------------------------------------
constructor TDPFJTimePickerDialogOnCancelListener.create( ADPFJTimePickerDialog: TDPFJTimePickerDialog );
begin
  inherited create;
  FDPFJTimePickerDialog := ADPFJTimePickerDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTimePickerDialogOnCancelListener.onCancel( dialog: JDialogInterface ); cdecl;
begin
  if assigned( FDPFJTimePickerDialog.FOnCancel ) then
    FDPFJTimePickerDialog.FOnCancel( FDPFJTimePickerDialog );

  CallInUIThread(
    procedure
    begin
      FDPFJTimePickerDialog.FreeAndNilObjects;
    end );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
