// ------------------------------------------------------------------------------
// DPF.Android.JEditText Component
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
unit DPF.Android.JEditText;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.Android.BaseControl,
  DPF.Android.Widget,
{$IFDEF ANDROID}
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Dialogs,
  FMX.Types;

type

  PPP           = class( TDPFANDBaseControl );
  TDPFJEditText = class;

{$IFDEF ANDROID}

  TJEditTextClickListener = class( TJavaLocal, JView_OnClickListener )
  private
    FTDPFJEditText: TDPFJEditText;
  public
    constructor create( ATDPFJEditText: TDPFJEditText );
    procedure onClick( P1: JView ); cdecl;
  end;

  TJEditTextOnFocusChangeListener = class( TJavaLocal, JView_OnFocusChangeListener )
  private
    FTDPFJEditText: TDPFJEditText;
  public
    constructor create( ATDPFJEditText: TDPFJEditText );
    procedure onFocusChange( v: JView; hasFocus: Boolean ); cdecl;
  end;

  TJEditTextWatcher = class( TJavaLocal, JTextWatcher )
  private
    FTDPFJEditText: TDPFJEditText;
  public
    constructor create( ATDPFJEditText: TDPFJEditText );

    procedure afterTextChanged( s: JEditable ); cdecl;
    procedure beforeTextChanged( s: JCharSequence; start: Integer; count: Integer; after: Integer ); cdecl;
    procedure onTextChanged( s: JCharSequence; start: Integer; before: Integer; count: Integer ); cdecl;
  end;

  TJEditTextOnEditorActionListener = class( TJavaLocal, JTextView_OnEditorActionListener )
  private
    FTDPFJEditText: TDPFJEditText;
  public
    constructor create( ATDPFJEditText: TDPFJEditText );
    function onEditorAction( v: JTextView; actionId: Integer; event: JKeyEvent ): Boolean; cdecl;
  end;

  // -----------------------------------------------------------------------
  TJEditTextOnKeyListenerClass = class( TJavaLocal, JView_OnKeyListener )
  private
    FTDPFJEditText: TDPFJEditText;
  public
    constructor create( ATDPFJEditText: TDPFJEditText );

    function onKey( v: JView; keyCode: Integer; event: JKeyEvent ): Boolean; cdecl;
  end;

{$ENDIF}

  TDPFTextViewEditorAction = ( eaDONE, eaGO, eaNEXT, eaNONE, eaPREVIOUS, eaSEARCH, eaSEND, eaUNSPECIFIED );
  TDPFTextOnChanged        = procedure( Sender: TObject ) of object;
  TDPFTextOnEditorAction   = procedure( Sender: TObject; Action: TDPFTextViewEditorAction; var Handled: Boolean ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJEditText = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJEditText                      : JEditText { JDPFEditText };
    FJEditTextClickListener         : TJEditTextClickListener;
    FJEditTextOnFocusChangeListener : TJEditTextOnFocusChangeListener;
    FJEditTextWatcher               : TJEditTextWatcher;
    FJEditTextOnEditorActionListener: TJEditTextOnEditorActionListener;
    FJEditTextOnKeyListenerClass    : TJEditTextOnKeyListenerClass;
{$ENDIF}
    FOnChanged     : TDPFTextOnChanged;
    FText          : string;
    FTextColor     : TAlphaColor;
    FNumberOfLines : Integer;
    FTextAlignment : TDPFTextAlignment;
    FTextSize      : Single;
    FAllCaps       : Boolean;
    FEditorAction  : TDPFTextViewEditorAction;
    FOnEditorAction: TDPFTextOnEditorAction;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetNumberOfLines( const Value: Integer );
    procedure SetTextSize( const Value: Single );
    procedure SetAllCaps( const Value: Boolean );
    procedure SetEditorAction( const Value: TDPFTextViewEditorAction );

  protected
    procedure Resize; override;
    procedure Move; override;
    procedure FontOnChanged( Sender: TObject );
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded; override;
    function sizeToFit: DPFNSize;
  published
    property Text         : string read FText write SetText;
    property TextColor    : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property TextSize     : Single read FTextSize write SetTextSize;
    property NumberOfLines: Integer read FNumberOfLines write SetNumberOfLines default 1;
    property TextAlignment: TDPFTextAlignment read FTextAlignment write SetTextAlignment default taAuto;
    property AllCaps      : Boolean read FAllCaps write SetAllCaps default false;
    property EditorAction : TDPFTextViewEditorAction read FEditorAction write SetEditorAction default eaDONE;

    property OnChanged     : TDPFTextOnChanged read FOnChanged write FOnChanged;
    property OnEditorAction: TDPFTextOnEditorAction read FOnEditorAction write FOnEditorAction;

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
    property Enabled;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

const
  TDPFTextViewEditorActionLabels: array [TDPFTextViewEditorAction] of string = ( 'DONE', 'GO', 'NEXT', 'NONE', 'PREVIOUS', 'SEARCH', 'SEND', 'UNSPECIFIED' );
  TDPFTextViewEditorActionValues: array [TDPFTextViewEditorAction] of Integer = ( 6, 2, 5, 1, 7, 3, 4, 0 );

  // ------------------------------------------------------------------------------
  { TDPFJEditText }
  // ------------------------------------------------------------------------------
constructor TDPFJEditText.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'EditText';
  FTextColor     := TAlphaColors.Black;
  FNumberOfLines := 1;
  FTextAlignment := taAuto;
  FTextSize      := 21.0;
  FEditorAction  := eaDONE;

{$IFDEF ANDROID}
  try
    CallInUIThreadAndWaitFinishing(
      procedure( )
      begin
        FJEditText := TJEditText.JavaClass.init( SharedActivity );
        FTextSize := FJEditText.getTextSize;
      end );
  except
    on E: Exception do
      DPFNSLog( E.Message );
  end;
  JControl := FJEditText;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJEditText.Destroy;
begin
{$IFDEF ANDROID}
  FJEditTextClickListener          := nil;
  FJEditTextOnFocusChangeListener  := nil;
  FJEditTextWatcher                := nil;
  FJEditTextOnEditorActionListener := nil;
  FJEditTextOnKeyListenerClass     := nil;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.FontOnChanged( Sender: TObject );
begin
  inherited;
{$IFDEF ANDROID}
  if FJEditText <> nil then
  begin
    // FJEditText.setFont( Font._UIFont );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.Move;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJEditText.sizeToFit: DPFNSize;
begin
{$IFDEF ANDROID}
  // FJEditText.sizeToFit;
  result.width  := FJEditText.getWidth;
  result.height := FJEditText.getHeight;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJEditText.Loaded;
begin
  // FJEditTextClickListener := TJEditTextClickListener.Create( self );
  // FJEditText.setOnClickListener( FJEditTextClickListener );

  FJEditTextOnFocusChangeListener := TJEditTextOnFocusChangeListener.create( self );
  FJEditText.setOnFocusChangeListener( FJEditTextOnFocusChangeListener );

  FJEditTextWatcher := TJEditTextWatcher.create( self );
  FJEditText.addTextChangedListener( FJEditTextWatcher );

  FJEditTextOnEditorActionListener := TJEditTextOnEditorActionListener.create( self );
  FJEditText.setOnEditorActionListener( FJEditTextOnEditorActionListener );

  FJEditTextOnKeyListenerClass := TJEditTextOnKeyListenerClass.create( self );
  FJEditText.setOnKeyListener( FJEditTextOnKeyListenerClass );

  // FJEditText.setTextDirection(3);
  // FJEditText.setTextAlignment(2);
  SetText( FText );
  SetTextColor( FTextColor );
  SetVisible( Visible );
  setNumberOfLines( FNumberOfLines );
  SetTextAlignment( FTextAlignment );
  SetEditorAction( FEditorAction );
  FJEditText.selectAll;
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      // FJEditText.setInputType( TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS );
      FJEditText.setInputType( TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS );
      FJEditText.setFocusableInTouchMode( true );
      FJEditText.setEnabled( true );

      FJEditText.requestFocusFromTouch;
    end );
  FJEditText.setClickable( true );

  AddSubView( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJEditText.RefreshNeeded;
begin
  inherited;
  Loaded;
end;

procedure TDPFJEditText.Resize;
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    begin
      if FJEditText <> nil then
      begin
        FJEditText.setWidth( round( Width * ScreenScale ) );
        FJEditText.setHeight( round( height * ScreenScale ) );
      end;
    end );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJEditText.Paint;
var
  Caption    : string;
  CaptionRect: TRectF;

begin
  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  if BackgroundColor1 <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackgroundColor1;
    Canvas.FillRect( ClipRect, 0, 0, AllCorners, Alpha, TCornerType.ctInnerRound );
  end;
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    Caption           := name;
    Canvas.Fill.Color := TAlphaColors.Gray;
  end
  else
  begin
    Caption           := Text;
    Canvas.Fill.Color := TextColor;
  end;
  // Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style := [TFontStyle.fsBold];
  // Canvas.Font.Family := GetDeviceFontName( Font.FontName );
  CaptionRect := ClipRect;
  PaintCaption( Self, Caption, CaptionRect, TDPFLineBreak.lbWordWrap, NumberOfLines, CTextAlign[FTextAlignment] );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetAllCaps( const Value: Boolean );
begin
  FAllCaps := Value;
{$IFDEF ANDROID}
  if Assigned( FJEditText ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setAllCaps( value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetEditorAction( const Value: TDPFTextViewEditorAction );
begin
  FEditorAction := Value;
{$IFDEF ANDROID}
  if Assigned( FJEditText ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setImeActionLabel( StrToJCharSequence( TDPFTextViewEditorActionLabels[Value] ), TDPFTextViewEditorActionValues[value] );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetNumberOfLines( const Value: Integer );
begin
  FNumberOfLines := Value;
{$IFDEF ANDROID}
  if Assigned( FJEditText ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setLines( Value );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetText( const Value: string );
begin
  FText := Value;
{$IFDEF ANDROID}
  if Assigned( FJEditText ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setText( TJCharSequence.Wrap( StringToJNIString( TJNIResolver.GetJNIEnv, Value ) ), TJTextView_BufferType.JavaClass.EDITABLE );
      end );
  end;
{$ELSE}
  ControlCaption := FText;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF ANDROID}
  if Assigned( FJEditText ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setGravity( TDPFTextAlignmentToGravity[Value] );
      end );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF ANDROID}
  if FJEditText <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.SetTextColor( FTextColor );
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJEditText.SetTextSize( const Value: Single );
begin
  FTextSize := Value;
{$IFDEF ANDROID}
  if FJEditText <> nil then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJEditText.setTextSize( Value );
      end );
  end;
{$ENDIF}
end;

{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
{ TJEditTextOnEditorActionListener }
constructor TJEditTextOnEditorActionListener.create( ATDPFJEditText: TDPFJEditText );
begin
  inherited create;
  FTDPFJEditText := ATDPFJEditText;
end;

// ------------------------------------------------------------------------------
function TJEditTextOnEditorActionListener.onEditorAction( v: JTextView; actionId: Integer; event: JKeyEvent ): Boolean; cdecl;
begin
  result := false;
  if Assigned( FTDPFJEditText.FOnEditorAction ) then
    FTDPFJEditText.FOnEditorAction( FTDPFJEditText, TDPFTextViewEditorAction( actionId ), Result );
end;

// ------------------------------------------------------------------------------
{ TJEditTextWatcher }
constructor TJEditTextWatcher.create( ATDPFJEditText: TDPFJEditText );
begin
  inherited create;
  FTDPFJEditText := ATDPFJEditText;
end;

// ------------------------------------------------------------------------------
procedure TJEditTextWatcher.afterTextChanged( s: JEditable ); cdecl;
begin
  FTDPFJEditText.FText := JStringToString( s.toString );
  if assigned( FTDPFJEditText.FOnChanged ) then
    FTDPFJEditText.FOnChanged( FTDPFJEditText );
end;

// ------------------------------------------------------------------------------
procedure TJEditTextWatcher.beforeTextChanged( s: JCharSequence; start: Integer; count: Integer; after: Integer ); cdecl;
begin
  { }
end;

// ------------------------------------------------------------------------------
procedure TJEditTextWatcher.onTextChanged( s: JCharSequence; start: Integer; before: Integer; count: Integer ); cdecl;
begin
  { }
end;

// ------------------------------------------------------------------------------
{ TJEditTextClickListener }

constructor TJEditTextClickListener.create( ATDPFJEditText: TDPFJEditText );
begin
  inherited create;
  FTDPFJEditText := ATDPFJEditText;
end;

// ------------------------------------------------------------------------------
procedure TJEditTextClickListener.onClick( P1: JView );
begin
  CallInUIThread(
    procedure( )
    begin
      // FTDPFJEditText.FJEditText.requestFocus;
    end );
end;

// ------------------------------------------------------------------------------
{ TJEditTextOnFocusChangeListener }

constructor TJEditTextOnFocusChangeListener.create( ATDPFJEditText: TDPFJEditText );
begin
  inherited create;
  FTDPFJEditText := ATDPFJEditText;
end;

// ------------------------------------------------------------------------------
procedure TJEditTextOnFocusChangeListener.onFocusChange( v: JView; hasFocus: Boolean ); cdecl;
var
  obj : JObject;
  imm : JInputMethodManager;
  flag: boolean;
begin
  CallInUIThread(
    procedure( )
    begin
      obj := SharedActivity.getSystemService( TJContext.JavaClass.INPUT_METHOD_SERVICE );
      imm := TJInputMethodManager.Wrap( ( obj as ILocalObject ).GetObjectID );
      if hasFocus and FTDPFJEditText.FJEditText.isFocusable and FTDPFJEditText.FJEditText.isEnabled then
      begin
        FTDPFJEditText.FJEditText.requestFocus;
        flag := imm.showSoftInput( FTDPFJEditText.FJEditText, TJInputMethodManager.JavaClass.SHOW_IMPLICIT );
        // imm.toggleSoftInput( 0, TJInputMethodManager.JavaClass.SHOW_IMPLICIT )
      end
      else
        imm.hideSoftInputFromWindow( v.getWindowToken, 0 );

    end );
end;

// ------------------------------------------------------------------------------
constructor TJEditTextOnKeyListenerClass.create( ATDPFJEditText: TDPFJEditText );
begin
  inherited create;
  FTDPFJEditText := ATDPFJEditText;
end;

// ------------------------------------------------------------------------------
function TJEditTextOnKeyListenerClass.onKey( v: JView; keyCode: Integer; event: JKeyEvent ): Boolean; cdecl;
begin
  // If result = true then Keyboard Delete key not work !!!!!
  result := false;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
