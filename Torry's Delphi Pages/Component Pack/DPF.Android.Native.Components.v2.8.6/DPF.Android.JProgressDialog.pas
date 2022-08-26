// ------------------------------------------------------------------------------
// DPF.Android.JProgressDialog Component
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
unit DPF.Android.JProgressDialog;

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

  TDPFJProgressDialog = class;

{$IFDEF ANDROID}

  TDPFProgressDialogOnCancelListener = class( TJavaLocal, JDialogInterface_OnCancelListener )
  private
    FDPFJProgressDialog: TDPFJProgressDialog;
  public
    constructor create( ADPFJProgressDialog: TDPFJProgressDialog );
    procedure onCancel( dialog: JDialogInterface ); cdecl;
  end;

{$ENDIF}

  TDPFProgressDialogStyle    = ( dsSPINNER, dsHORIZONTAL );
  TDPFProgressDialogOnCancel = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJProgressDialog = class( TComponent )
  private
    FProgress         : integer;
    FMax              : integer;
    FTitle            : string;
    FMessage          : string;
    FOnCancel         : TDPFProgressDialogOnCancel;
    FSecondaryProgress: integer;
    FCancelable       : Boolean;
    procedure SetProgress( const Value: integer );
    procedure SetMax( const Value: integer );
    procedure SetTitle( const Value: string );
    procedure SetMessage( const Value: string );
    procedure SetSecondaryProgress( const Value: integer );
  protected
{$IFDEF ANDROID}
    FJProgressDialog                  : JProgressDialog;
    FDPFProgressDialogOnCancelListener: TDPFProgressDialogOnCancelListener;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJProgressDialog: JProgressDialog read FJProgressDialog;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Show( DialogStyle: TDPFProgressDialogStyle; Cancelable: Boolean = true; CanceledOnTouchOutside: boolean = true );
    procedure Close;

  published
    property Progress         : integer read FProgress write SetProgress default 0;
    property SecondaryProgress: integer read FSecondaryProgress write SetSecondaryProgress default 0;
    property Max              : integer read FMax write SetMax default 100;
    property Title            : string read FTitle write SetTitle;
    property &Message         : string read FMessage write SetMessage;
    property OnCancel         : TDPFProgressDialogOnCancel read FOnCancel write FOnCancel;

  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJProgressDialog }
constructor TDPFJProgressDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FProgress   := 0;
  FMax        := 100;
  FCancelable := true;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJProgressDialog := TJProgressDialog.JavaClass.init( SharedActivity );
      FDPFProgressDialogOnCancelListener := TDPFProgressDialogOnCancelListener.create( self );
      FJProgressDialog.setOnCancelListener( FDPFProgressDialogOnCancelListener );
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJProgressDialog.Destroy;
begin
{$IFDEF ANDROID}
  { if Assigned( FTapGestureRecognizer ) then
    FJProgressDialogGroup.removeGestureRecognizer( FTapGestureRecognizer );

    if Assigned( FDPFGestureRecognizerDelegate ) then
    FDPFGestureRecognizerDelegate.DisposeOf; }
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJProgressDialog.Loaded;
begin
  // Resize; // Very Important for Embbeded Forms, Frames

  SetMax( FMax );
  SetProgress( FProgress );

  // addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.SetMax( const Value: integer );
begin
  FMax := Value;
  if Value > 10000 then
    FMax := 10000;

{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setMax( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.SetMessage( const Value: string );
begin
  FMessage := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setMessage( StrToJCharSequence( value ) );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.SetProgress( const Value: integer );
begin
  FProgress := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setProgress( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.SetSecondaryProgress( const Value: integer );
begin
  FSecondaryProgress := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setSecondaryProgress( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.SetTitle( const Value: string );
begin
  FTitle := Value;
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setTitle( StrToJCharSequence( FTitle ) );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.Close;
begin
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.dismiss;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJProgressDialog.Show( DialogStyle: TDPFProgressDialogStyle; Cancelable: Boolean = true; CanceledOnTouchOutside: boolean = true );
begin
{$IFDEF ANDROID}
  if assigned( FJProgressDialog ) then
  begin
    CallInUIThread(
      procedure
      begin
        FJProgressDialog.setProgressStyle( Integer( DialogStyle ) );
        FJProgressDialog.setCancelable( Cancelable );
        FJProgressDialog.setCanceledOnTouchOutside( CanceledOnTouchOutside );
        FJProgressDialog.Show;
      end );
  end;
{$ENDIF}
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TDPFProgressDialogOnCancelListener.create( ADPFJProgressDialog: TDPFJProgressDialog );
begin
  inherited create;
  FDPFJProgressDialog := ADPFJProgressDialog;
end;

// ------------------------------------------------------------------------------
procedure TDPFProgressDialogOnCancelListener.onCancel( dialog: JDialogInterface ); cdecl;
begin
  if assigned( FDPFJProgressDialog.FOnCancel ) then
    FDPFJProgressDialog.FOnCancel( FDPFJProgressDialog );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
