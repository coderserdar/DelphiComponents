// ------------------------------------------------------------------------------
// DPF.iOS.UIAlertView Component
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
unit DPF.iOS.UIAlertView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  System.Math,
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
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFAlertViewStyle = ( avsDefault = 0, avsSecureTextInput = 1, avsPlainTextInput = 2, avsLoginAndPasswordInput = 3 );

const
  TDPFAlertVewNumText: array [TDPFAlertViewStyle] of Integer = ( 0, 1, 1, 2 );

type
  TDPFAlertView = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  NSObjectClass = interface( IObjectiveCClass )
    ['{84CDD025-E02A-4128-B1AC-35A7A5A4643B}']
    procedure cancelPreviousPerformRequestsWithTarget( target: Pointer ); cdecl;
  end;

  UIAlertViewClass = interface( UIViewClass )
    ['{E01C8960-2AEE-4639-88C5-551989062250}']
  end;

  UIAlertView = interface( UIView )
    ['{50BF1A44-4280-4505-AEC9-56C93A86C866}']
    function addButtonWithTitle( title: NSString ): NSInteger; cdecl;
    function alertViewStyle: UIAlertViewStyle; cdecl;
    function buttonTitleAtIndex( buttonIndex: NSInteger ): NSString; cdecl;
    function cancelButtonIndex: NSInteger; cdecl;
    function delegate: Pointer; cdecl;
    procedure dismissWithClickedButtonIndex( buttonIndex: NSInteger; animated: Boolean ); cdecl;
    function firstOtherButtonIndex: NSInteger; cdecl;
    function initWithTitle( title: NSString; &message: NSString; delegate: Pointer; cancelButtonTitle: NSString; otherButtonTitles: NSString ): Pointer; cdecl;
    function isVisible: Boolean; cdecl;
    function &message: NSString; cdecl;
    function numberOfButtons: NSInteger; cdecl;
    procedure setAlertViewStyle( alertViewStyle: UIAlertViewStyle ); cdecl;
    procedure setCancelButtonIndex( cancelButtonIndex: NSInteger ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setMessage( &message: NSString ); cdecl;
    procedure setTitle( title: NSString ); cdecl;
    procedure show; cdecl;
    function textFieldAtIndex( textFieldIndex: NSInteger ): Pointer; cdecl;
    function title: NSString; cdecl;
    function visible: boolean; cdecl;
  end;

  TUIAlertView = class( TOCGenericImport<UIAlertViewClass, UIAlertView> )
  end;

  DPFUIAlertViewDelegate = interface( IObjectiveC { NSObject } )
    ['{9A80C5EA-9E04-429C-A910-D550A1D81AED}']
    procedure alertView( alertView: UIAlertView; clickedButtonAtIndex: NSInteger ); cdecl; overload;
    procedure alertViewCancel( alertView: UIAlertView ); cdecl;
    procedure didPresentAlertView( alertView: UIAlertView ); cdecl;
  end;

  TDPFAlertViewDelegate = class( TOCLocal, DPFUIAlertViewDelegate )
  private
    FDPFAlertView: TDPFAlertView;
  public
    constructor Create( ADPFAlertView: TDPFAlertView );
    destructor Destroy; override;
    // function GetObjectiveCClass: PTypeInfo; override;

    procedure alertView( alertView: UIAlertView; clickedButtonAtIndex: NSInteger ); overload; cdecl;
    procedure alertViewCancel( alertView: UIAlertView ); cdecl;
    procedure didPresentAlertView( alertView: UIAlertView ); cdecl;
  end;
{$ENDIF}

  TArrayOfString   = array of string;
  TDPFAlertOnClick = procedure( Sender: TObject; ButtonIndex: Integer; TextResults: TArrayOfString ) of object;
  TDPFAlertOnShow  = procedure( Sender: TObject; var InputFocusedIndex: integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFAlertView = class( TComponent )
  private
{$IFDEF IOS}
    FUIAlertView         : UIAlertView;
    FDPFAlertViewDelegate: TDPFAlertViewDelegate;
{$ENDIF}
    FOnClick    : TDPFAlertOnClick;
    FViewStyle  : TDPFAlertViewStyle;
    FResultTexts: TArrayOfString;
    FResultIndex: Integer;
    FOnShow     : TDPFAlertOnShow;

  protected
    isClicked: Boolean;
  public
    function ShowMessageDialog( const Title: string; const Msg: string; const Buttons: array of string; const DefaultButtonIndex: Integer; PreDefineText: array of string; PlaceholderText: array of string; const isWaiting: Boolean = true ): Integer;

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
{ TDPFAlertView }
// ------------------------------------------------------------------------------
constructor TDPFAlertView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FViewStyle := TDPFAlertViewStyle.avsDefault;
{$IFDEF IOS}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFAlertView.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFAlertView.ShowMessageDialog( const Title: string; const Msg: string; const Buttons: array of string; const DefaultButtonIndex: Integer; PreDefineText: array of string; PlaceholderText: array of string; const isWaiting: Boolean = true ): Integer;
{$IFDEF IOS}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF IOS}
  FDPFAlertViewDelegate := TDPFAlertViewDelegate.Create( Self );
  // FUIAlertView          := TUIAlertView.Wrap( TUIAlertView.Alloc.initWithTitle( NSStr( Title ), NSStr( Msg ), FDPFAlertViewDelegate.GetObjectID, nil, NSStr( '' ) ) );
  FUIAlertView := TUIAlertView.Wrap( TUIAlertView.Alloc.init );
  FUIAlertView.setTitle( NSStr( Title ) );
  FUIAlertView.setMessage( NSStr( Msg ) );
  FUIAlertView.setDelegate( FDPFAlertViewDelegate.GetObjectID );

  FUIAlertView.setAlertViewStyle( Integer( FViewStyle ) );

  for I := low( Buttons ) to high( Buttons ) do
  begin
    FUIAlertView.addButtonWithTitle( NSStr( Buttons[I] ) );
  end;

  isClicked := False;

  for I := low( PreDefineText ) to Min( high( PreDefineText ), TDPFAlertVewNumText[FViewStyle] ) do
  begin
    TUITextField.Wrap( FUIAlertView.textFieldAtIndex( I ) ).setText( NSStr( PreDefineText[I] ) );
  end;

  for I := low( PlaceholderText ) to Min( high( PlaceholderText ), TDPFAlertVewNumText[FViewStyle] ) do
  begin
    TUITextField.Wrap( FUIAlertView.textFieldAtIndex( I ) ).setPlaceholder( NSStr( PlaceholderText[I] ) );
  end;

  FUIAlertView.show;

  if isWaiting then
  begin
    while not isClicked do
    begin
      TNSRunLoop.Wrap( TNSRunLoop.OCClass.currentRunLoop ).runMode( NSDefaultRunLoopMode, TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSinceNow( 1.5 ) ) );
    end;
  end;
  Result := ResultIndex;

  FUIAlertView.removeFromSuperview;
  FUIAlertView.release;
  FDPFAlertViewDelegate.DisposeOf;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFAlertViewDelegate }

constructor TDPFAlertViewDelegate.Create( ADPFAlertView: TDPFAlertView );
begin
  inherited Create;
  FDPFAlertView := ADPFAlertView;
end;

// ------------------------------------------------------------------------------
{ function TDPFAlertViewDelegate.GetObjectiveCClass: PTypeInfo;
  begin
  Result := TypeInfo( DPFUIAlertViewDelegate );
  end; }

// ------------------------------------------------------------------------------
destructor TDPFAlertViewDelegate.Destroy;
begin
  inherited Destroy;
end;

// ------------------------------------------------------------------------------
procedure TDPFAlertViewDelegate.alertView( alertView: UIAlertView; clickedButtonAtIndex: NSInteger );
var
  Idx: Integer;
begin
  FDPFAlertView.FResultIndex := clickedButtonAtIndex;

  SetLength( FDPFAlertView.FResultTexts, 0 );
  for Idx := 0 to TDPFAlertVewNumText[TDPFAlertViewStyle( alertView.alertViewStyle )] - 1 do
  begin
    SetLength( FDPFAlertView.FResultTexts, Length( FDPFAlertView.FResultTexts ) + 1 );
    FDPFAlertView.FResultTexts[high( FDPFAlertView.FResultTexts )] := UTF8ToString( TNSString.Wrap( TUITextField.Wrap( alertView.textFieldAtIndex( Idx ) ).text ).UTF8String );
  end;

  if Assigned( FDPFAlertView.FOnClick ) then
    FDPFAlertView.FOnClick( FDPFAlertView, FDPFAlertView.FResultIndex, FDPFAlertView.FResultTexts );

  FDPFAlertView.isClicked := true;
end;

// ------------------------------------------------------------------------------
procedure TDPFAlertViewDelegate.alertViewCancel( alertView: UIAlertView );
begin

end;

// ------------------------------------------------------------------------------
procedure TDPFAlertViewDelegate.didPresentAlertView( alertView: UIAlertView ); cdecl;
var
  Idx              : Integer;
  InputFocusedIndex: Integer;
begin
  TUIView.Wrap( FDPFAlertView.FUIAlertView.superview ).bringSubviewToFront( FDPFAlertView.FUIAlertView );
  if Assigned( FDPFAlertView.FOnShow ) then
  begin
    InputFocusedIndex := -1;
    FDPFAlertView.FOnShow( FDPFAlertView, InputFocusedIndex );
    Idx := TDPFAlertVewNumText[TDPFAlertViewStyle( alertView.alertViewStyle )];
    if ( InputFocusedIndex > -1 ) and ( InputFocusedIndex < Idx ) then
      TUITextField.Wrap( FDPFAlertView.FUIAlertView.textFieldAtIndex( InputFocusedIndex ) ).becomeFirstResponder;
  end;
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
