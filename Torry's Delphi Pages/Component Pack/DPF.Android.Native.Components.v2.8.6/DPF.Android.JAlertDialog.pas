// ------------------------------------------------------------------------------
// DPF.Android.JAlertDialog Component
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
unit DPF.Android.JAlertDialog;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,
  System.Math,
  System.SyncObjs,

  DPF.Android.BaseControl,
{$IFDEF ANDROID}
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFAlertViewStyle = ( avsDefault = 0, avsSecureTextInput = 1, avsPlainTextInput = 2, avsLoginAndPasswordInput = 3 );

const
  TDPFAlertVewNumText: array [TDPFAlertViewStyle] of Integer = ( 0, 1, 1, 2 );

type
  TDPFJAlertDialog = class;

  // ------------------------------------------------------------------------------
{$IFDEF ANDROID}

  TAlertDialogOnClickListener = class( TJavaLocal, JDialogInterface_OnClickListener )
  strict private[Weak]
    FDPFJAlertDialog: TDPFJAlertDialog;
  public
    constructor Create( ADPFJAlertDialog: TDPFJAlertDialog );
    procedure onClick( P1: JDialogInterface; P2: Integer ); cdecl;
  end;

  TDismissListener = class( TJavaLocal, JDialogInterface_OnDismissListener )
  strict private[Weak]
    FDPFJAlertDialog: TDPFJAlertDialog;
  public
    constructor Create( ADPFJAlertDialog: TDPFJAlertDialog );
    procedure onDismiss( dialog: JDialogInterface ); cdecl;
  end;

{$ENDIF}

  TArrayOfString   = array of string;
  TDPFAlertOnClick = procedure( Sender: TObject; ButtonIndex: Integer; TextResults: TArrayOfString ) of object;
  TDPFAlertOnShow  = procedure( Sender: TObject; var InputFocusedIndex: integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJAlertDialog = class( TComponent )
  private
{$IFDEF ANDROID}
    FJAlertDialog_Builder: JAlertDialog_Builder;
    Listeners            : array [0 .. 2] of TAlertDialogOnClickListener;
    DismissListener      : TDismissListener;
    FClickedIndex        : Integer;
{$ENDIF}
    FOnClick    : TDPFAlertOnClick;
    FViewStyle  : TDPFAlertViewStyle;
    FResultTexts: TArrayOfString;
    FResultIndex: Integer;
    FOnShow     : TDPFAlertOnShow;

  protected
    isClicked: Boolean;
  public
    function ShowMessageDialog( const Title: string; const Msg: string; const Buttons: array of string ): Integer;

    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    property ResultIndex: Integer read FResultIndex;
    property ResultTexts: TArrayOfString read FResultTexts;
  published
    property ViewStyle: TDPFAlertViewStyle read FViewStyle write FViewStyle default TDPFAlertViewStyle.avsDefault;
    property OnClick  : TDPFAlertOnClick read FOnClick write FOnClick;
    property OnShow   : TDPFAlertOnShow read FOnShow write FOnShow;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFJAlertDialog }
// ------------------------------------------------------------------------------
constructor TDPFJAlertDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FViewStyle := TDPFAlertViewStyle.avsDefault;
{$IFDEF ANDROID}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJAlertDialog.Destroy;
begin
{$IFDEF ANDROID}
  DismissListener := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJAlertDialog.ShowMessageDialog( const Title: string; const Msg: string; const Buttons: array of string ): Integer;
{$IFDEF ANDROID}
var
  I           : Integer;
  FAlertDialog: JAlertDialog;
{$ENDIF}
begin
  result := -1;
{$IFDEF ANDROID}
  FJAlertDialog_Builder := TJAlertDialog_Builder.JavaClass.init( SharedActivity );

  FJAlertDialog_Builder.setTitle( StrToJCharSequence( Title ) );
  FJAlertDialog_Builder.setMessage( StrToJCharSequence( MSg ) );
  FJAlertDialog_Builder.setCancelable( true );

  for I := 0 to high( Buttons ) do
  begin
    Listeners[I] := TAlertDialogOnClickListener.Create( Self );
    case I of
      0:
        FJAlertDialog_Builder.setPositiveButton( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Buttons[I] ) ), Listeners[I] );
      1:
        FJAlertDialog_Builder.setNegativeButton( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Buttons[I] ) ), Listeners[I] );
      2:
        FJAlertDialog_Builder.setNeutralButton( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Buttons[I] ) ), Listeners[I] );
    end;

    if I > 2 then
      Break;
  end;

  FClickedIndex   := -1;
  FAlertDialog    := FJAlertDialog_Builder.create;
  DismissListener := TDismissListener.Create( self );
  FAlertDialog.setOnDismissListener( DismissListener );
  FAlertDialog.show;
{$ENDIF}
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
{ TAlertDialogOnClickListener }
constructor TAlertDialogOnClickListener.Create( ADPFJAlertDialog: TDPFJAlertDialog );
begin
  inherited create;
  FDPFJAlertDialog := ADPFJAlertDialog;
end;

// ------------------------------------------------------------------------------
procedure TAlertDialogOnClickListener.onClick( P1: JDialogInterface; P2: Integer );
begin
  FDPFJAlertDialog.FClickedIndex := P2;
  P1.dismiss;
end;

// ------------------------------------------------------------------------------
{ TDismissListener }
constructor TDismissListener.Create( ADPFJAlertDialog: TDPFJAlertDialog );
begin
  inherited create;
  FDPFJAlertDialog := ADPFJAlertDialog;
end;

// ------------------------------------------------------------------------------
procedure TDismissListener.onDismiss( dialog: JDialogInterface );
begin
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
