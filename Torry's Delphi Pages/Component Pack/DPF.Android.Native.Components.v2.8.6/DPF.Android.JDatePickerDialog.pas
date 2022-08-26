// ------------------------------------------------------------------------------
// DPF.Android.JDatePickerDialog Component
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
unit DPF.Android.JDatePickerDialog;

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

  TDPFJDatePickerDialog = class;

{$IFDEF ANDROID}

  TDPFJDatePickerDialogOnDateSetListener = class( TJavaLocal, JDatePickerDialog_OnDateSetListener )
  private
    FDPFJDatePickerDialog: TDPFJDatePickerDialog;
  public
    constructor create( ADPFJDatePickerDialog: TDPFJDatePickerDialog );
    procedure onDateSet( view: JDatePicker; year: integer; monthOfYear: Integer; dayOfMonth: integer ); cdecl;
  end;

  TDPFJDatePickerDialogOnCancelListener = class( TJavaLocal, JDialogInterface_OnCancelListener )
  private
    FDPFJDatePickerDialog: TDPFJDatePickerDialog;
  public
    constructor create( ADPFJDatePickerDialog: TDPFJDatePickerDialog );
    procedure onCancel( dialog: JDialogInterface ); cdecl;
  end;

{$ENDIF}

  TDPFDatePickerDialogOnDateSet = procedure( sender: TObject; const year: integer; const monthOfYear: Integer; const dayOfMonth: integer ) of object;
  TDPFDatePickerDialogOnCancel  = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJDatePickerDialog = class( TComponent )
  private
    FOnDateSet: TDPFDatePickerDialogOnDateSet;
    FOnCancel : TDPFDatePickerDialogOnCancel;
  protected
{$IFDEF ANDROID}
    FJDatePickerDialog                    : JDatePickerDialog;
    FDPFJDatePickerDialogOnDateSetListener: TDPFJDatePickerDialogOnDateSetListener;
    FDPFJDatePickerDialogOnCancelListener : TDPFJDatePickerDialogOnCancelListener;
{$ENDIF}
    procedure FreeAndNilObjects;
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Show( const Title: string; const &Message: string; const year: Integer; const monthOfYear: Integer; const dayOfMonth: Integer );

  published
    property OnDateSet: TDPFDatePickerDialogOnDateSet read FOnDateSet write FOnDateSet;
    property OnCancel : TDPFDatePickerDialogOnCancel read FOnCancel write FOnCancel;

  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJDatePickerDialog }
constructor TDPFJDatePickerDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF ANDROID}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJDatePickerDialog.Destroy;
begin
{$IFDEF ANDROID}
  FreeAndNilObjects;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJDatePickerDialog.FreeAndNilObjects;
begin
{$IFDEF ANDROID}
  FJDatePickerDialog                     := nil;
  FDPFJDatePickerDialogOnDateSetListener := nil;
  FDPFJDatePickerDialogOnCancelListener  := nil;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJDatePickerDialog.Loaded;
begin
  // Resize; // Very Important for Embbeded Forms, Frames

  // addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJDatePickerDialog.Show( const Title: string; const &Message: string; const year: Integer; const monthOfYear: Integer; const dayOfMonth: Integer );
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    begin
      FDPFJDatePickerDialogOnDateSetListener := TDPFJDatePickerDialogOnDateSetListener.create( self );
      FJDatePickerDialog := TJDatePickerDialog.JavaClass.init( SharedActivity, FDPFJDatePickerDialogOnDateSetListener, year, monthOfYear, dayOfMonth );
      FJDatePickerDialog.setMessage( StrToJCharSequence( &Message ) );
      FJDatePickerDialog.setTitle( StrToJCharSequence( Title ) );

      FDPFJDatePickerDialogOnCancelListener := TDPFJDatePickerDialogOnCancelListener.create( self );
      FJDatePickerDialog.setOnCancelListener( FDPFJDatePickerDialogOnCancelListener );

      FJDatePickerDialog.updateDate( year, monthOfYear, dayOfMonth );
      FJDatePickerDialog.Show;
    end );
{$ENDIF}
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TDPFJDatePickerDialogOnDateSetListener.create( ADPFJDatePickerDialog: TDPFJDatePickerDialog );
begin
  inherited create;
  FDPFJDatePickerDialog := ADPFJDatePickerDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFJDatePickerDialogOnDateSetListener.onDateSet( view: JDatePicker; year: integer; monthOfYear: Integer; dayOfMonth: integer ); cdecl;
begin
  if assigned( FDPFJDatePickerDialog.FOnDateSet ) then
    FDPFJDatePickerDialog.FOnDateSet( FDPFJDatePickerDialog, year, monthOfYear, dayOfMonth );

  TThread.Queue( nil,
    procedure
    begin
      FDPFJDatePickerDialog.FreeAndNilObjects;
    end );
end;

// ------------------------------------------------------------------------------
constructor TDPFJDatePickerDialogOnCancelListener.create( ADPFJDatePickerDialog: TDPFJDatePickerDialog );
begin
  inherited create;
  FDPFJDatePickerDialog := ADPFJDatePickerDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFJDatePickerDialogOnCancelListener.onCancel( dialog: JDialogInterface ); cdecl;
begin
  if assigned( FDPFJDatePickerDialog.FOnCancel ) then
    FDPFJDatePickerDialog.FOnCancel( FDPFJDatePickerDialog );

  TThread.Queue( nil,
    procedure
    begin
      FDPFJDatePickerDialog.FreeAndNilObjects;
    end );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
