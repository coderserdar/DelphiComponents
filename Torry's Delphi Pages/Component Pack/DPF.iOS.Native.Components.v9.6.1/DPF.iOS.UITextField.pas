// ------------------------------------------------------------------------------
// DPF.iOS.UITextField Component
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
unit DPF.iOS.UITextField;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.Math,
  System.MaskUtils,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  iOSapi.QuartzCore,
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
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFTextField = class;

  TDPFTextBorderStyle              = ( bsNone, bsLine, bsBezel, bsRoundedRect );
  TDPFTextFieldViewMode            = ( vmNever, vmWhileEditing, vmUnlessEditing, vmModeAlways );
  TDPFTextFieldVerticalAlignment   = ( vaCenter, vaTop, vaBottom, vaFill );
  TDPFTextFieldHorizontalAlignment = ( haCenter = 0, haLeft = 1, haRight = 2, haFill = 3 );

{$IFDEF IOS}

  UITextFieldClass = interface( UIControlClass )
    ['{3B777AB3-CCFA-4D70-8EFA-5011A1160F55}']
  end;

  UITextField = interface( UIControl )
    ['{22EC1F8E-35A0-498B-9087-20034FE9616D}']
    function adjustsFontSizeToFitWidth: Boolean; cdecl;
    function background: UIImage; cdecl;
    function borderRectForBounds( bounds: CGRect ): CGRect; cdecl;
    function borderStyle: UITextBorderStyle; cdecl;
    function clearButtonMode: UITextFieldViewMode; cdecl;
    function clearButtonRectForBounds( bounds: CGRect ): CGRect; cdecl;
    function clearsOnBeginEditing: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function disabledBackground: UIImage; cdecl;
    procedure drawPlaceholderInRect( rect: CGRect ); cdecl;
    procedure drawTextInRect( rect: CGRect ); cdecl;
    function editingRectForBounds( bounds: CGRect ): CGRect; cdecl;
    function font: UIFont; cdecl;
    function inputAccessoryView: UIView; cdecl;
    function inputView: UIView; cdecl;
    function isEditing: Boolean; cdecl;
    function leftView: UIView; cdecl;
    function leftViewMode: UITextFieldViewMode; cdecl;
    function leftViewRectForBounds( bounds: CGRect ): CGRect; cdecl;
    function minimumFontSize: Single; cdecl;
    function placeholder: NSString; cdecl;
    function placeholderRectForBounds( bounds: CGRect ): CGRect; cdecl;
    function rightView: UIView; cdecl;
    function rightViewMode: UITextFieldViewMode; cdecl;
    function rightViewRectForBounds( bounds: CGRect ): CGRect; cdecl;
    procedure setAdjustsFontSizeToFitWidth( adjustsFontSizeToFitWidth: Boolean ); cdecl;
    procedure setBackground( background: UIImage ); cdecl;
    procedure setBorderStyle( borderStyle: UITextBorderStyle ); cdecl;
    procedure setClearButtonMode( clearButtonMode: UITextFieldViewMode ); cdecl;
    procedure setClearsOnBeginEditing( clearsOnBeginEditing: Boolean ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setDisabledBackground( disabledBackground: UIImage ); cdecl;
    procedure setFont( font: UIFont ); cdecl;
    procedure setInputAccessoryView( inputAccessoryView: UIView ); cdecl;
    procedure setInputView( inputView: UIView ); cdecl;
    procedure setLeftView( leftView: UIView ); cdecl;
    procedure setLeftViewMode( leftViewMode: UITextFieldViewMode ); cdecl;
    procedure setMinimumFontSize( minimumFontSize: Single ); cdecl;
    procedure setPlaceholder( placeholder: NSString ); cdecl;
    procedure setRightView( rightView: UIView ); cdecl;
    procedure setRightViewMode( rightViewMode: UITextFieldViewMode ); cdecl;
    procedure setText( text: NSString ); cdecl;
    procedure setTextAlignment( textAlignment: UITextAlignment ); cdecl;
    procedure setTextColor( textColor: UIColor ); cdecl;
    function text: Pointer; cdecl;
    function textAlignment: UITextAlignment; cdecl;
    function textColor: UIColor; cdecl;
    function textRectForBounds( bounds: CGRect ): CGRect; cdecl;

    procedure selectAll; cdecl; // Added by me

    function keyboardType: UIKeyboardType; cdecl; // Added by me
    procedure setKeyboardType( keyboardType: UIKeyboardType ); cdecl; // Added by me

    function autocapitalizationType: UITextAutocapitalizationType; cdecl; // Added by me
    procedure setAutocapitalizationType( autocapitalizationType: UITextAutocapitalizationType ); cdecl; // Added by me

    function autocorrectionType: UITextAutocorrectionType; cdecl; // Added by me
    procedure setAutocorrectionType( autocorrectionType: UITextAutocorrectionType ); cdecl; // Added by me

    function spellCheckingType: UITextSpellCheckingType; cdecl; // Added by me
    procedure setSpellCheckingType( spellCheckingType: UITextSpellCheckingType ); cdecl; // Added by me

    function enablesReturnKeyAutomatically: Boolean; cdecl; // Added by me
    procedure setEnablesReturnKeyAutomatically( enablesReturnKeyAutomatically: Boolean ); cdecl; // Added by me

    function keyboardAppearance: UIKeyboardAppearance; cdecl; // Added by me
    procedure setKeyboardAppearance( keyboardAppearance: UIKeyboardAppearance ); cdecl; // Added by me

    function returnKeyType: UIReturnKeyType; cdecl; // Added by me
    procedure setReturnKeyType( returnKeyType: UIReturnKeyType ); cdecl; // Added by me

    function secureTextEntry: Boolean; cdecl; // Added by me
    procedure setSecureTextEntry( secureTextEntry: Boolean ); cdecl; // Added by me



    function beginningOfDocument: UITextPosition; cdecl;
    function offsetFromPosition(from: UITextPosition; toPosition: UITextPosition): NSInteger; cdecl;
    function endOfDocument: UITextPosition; cdecl;
    function positionFromPosition(position: UITextPosition; offset: NSInteger): UITextPosition; cdecl; overload;
    function textRangeFromPosition(fromPosition: UITextPosition; toPosition: UITextPosition): UITextRange; cdecl;

  end;

  TUITextField = class( TOCGenericImport<UITextFieldClass, UITextField> )
  end;

  DPFUITextFieldDelegate = interface( IObjectiveC )
    ['{8F8E61E5-6664-4427-B144-17137E3359A7}']
    function textField( textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString ): Boolean; cdecl;
    procedure textFieldDidBeginEditing( textField: UITextField ); cdecl;
    procedure textFieldDidEndEditing( textField: UITextField ); cdecl;
    function textFieldShouldBeginEditing( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldClear( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldEndEditing( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldReturn( textField: UITextField ): Boolean; cdecl;
  end;

  TDPFTextFieldDelegate = class( TOCLocal, DPFUITextFieldDelegate )
  private
    FDPFTextField: TDPFTextField;
  public
    constructor Create( ADPFTextField: TDPFTextField );

    function textField( textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString ): Boolean; cdecl;
    procedure textFieldDidBeginEditing( textField: UITextField ); cdecl;
    procedure textFieldDidEndEditing( textField: UITextField ); cdecl;
    function textFieldShouldBeginEditing( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldClear( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldEndEditing( textField: UITextField ): Boolean; cdecl;
    function textFieldShouldReturn( textField: UITextField ): Boolean; cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------

  TDPFTextFieldKind = ( tfkText, tfkNumber, tfkFloat, tfkCurrency );

  TDPFTextFormat = class( TPersistent )
  private
    FDPFTextField: TDPFTextField;
    FTextKind    : TDPFTextFieldKind;
  public
    constructor Create( ADPFTextField: TDPFTextField );
    destructor Destroy; override;
  published
    property TextKind: TDPFTextFieldKind read FTextKind write FTextKind default TDPFTextFieldKind.tfkText;
  end;

  TDPFTextFieldBeginEditing    = procedure( Sender: TObject; isBeginEditing: Boolean ) of object;
  TDPFTextFieldEndEditing      = procedure( Sender: TObject; isEndEditing: Boolean ) of object;
  TDPFTextFieldShouldClear     = procedure( Sender: TObject; isShouldClear: Boolean ) of object;
  TDPFTextFieldShouldReturn    = procedure( Sender: TObject; isShouldReturn: Boolean ) of object;
  TDPFTextFieldDidBeginEditing = procedure( Sender: TObject ) of object;
  TDPFTextFieldDidEndEditing   = procedure( Sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFTextField = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUITextField      : UITextField;
    FTextFieldDelegate: TDPFTextFieldDelegate;
{$ENDIF}
    FText                         : string;
    FNormalImage                  : string;
    FHighlightImag                : string;
    FDisableImage                 : string;
    FTextColor                    : TAlphaColor;
    FBackgroundColor              : TAlphaColor;
    FBorderStyle                  : TDPFTextBorderStyle;
    FTextAlignment                : TDPFTextAlignment;
    FTextPlaceholder              : string;
    FTextViewMode                 : TDPFTextFieldViewMode;
    FTextVerticalAlignment        : TDPFTextFieldVerticalAlignment;
    FTextHorizontalAlignment      : TDPFTextFieldHorizontalAlignment;
    FKeyboardType                 : TDPFKeyboardType;
    FAutocapitalizationType       : TDPFTextAutocapitalizationType;
    FAutocorrectionType           : TDPFTextAutocorrectionType;
    FSpellCheckingType            : TDPFTextSpellCheckingType;
    FEnablesReturnKeyAutomatically: Boolean;
    FKeyboardAppearance           : TDPFKeyboardAppearance;
    FReturnKey                    : TDPFReturnKey;
    FSsecureTextEntry             : Boolean;
    FBackgroundImage              : string;
    FOnBeginEditing               : TDPFTextFieldBeginEditing;
    FOnEndEditing                 : TDPFTextFieldEndEditing;
    FOnShouldClear                : TDPFTextFieldShouldClear;
    FOnShouldReturn               : TDPFTextFieldShouldReturn;
    FDidEndEditing                : TDPFTextFieldDidEndEditing;
    FOnDidBeginEditing            : TDPFTextFieldDidBeginEditing;
    FHideKeyboardOnReturn         : Boolean;
    FAdjustsFontSizeToFitWidth    : Boolean;
    FAutoSizeType                 : TDPFTextAutoSizeType;
    FMaxlength                    : Longword;
    FTextFormat                   : TDPFTextFormat;
    FEnabled                      : Boolean;
    FEditable                     : Boolean;

    procedure setText( const Value: string );
    procedure setTextColor( const Value: TAlphaColor );

    procedure SetBackgroundColor( const Value: TAlphaColor );
    function GetText: string;
    procedure SetTextHorizontalAlignment( const Value: TDPFTextFieldHorizontalAlignment );
    procedure SetTextVerticalAlignment( const Value: TDPFTextFieldVerticalAlignment );
    procedure setKeyboardType( const Value: TDPFKeyboardType );
    procedure SetBackgroundImage( const Value: string );
    procedure SetTextPlaceholder( const Value: string );
    procedure SetTextAlignment( const Value: TDPFTextAlignment );
    procedure SetAdjustsFontSizeToFitWidth( const Value: Boolean );
    procedure SetAutoSizeType( const Value: TDPFTextAutoSizeType );
    procedure SetTextFormat( const Value: TDPFTextFormat );
    procedure setEnabled( const Value: Boolean );

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
    procedure SelectAll;
  published
    property AutocapitalizationType       : TDPFTextAutocapitalizationType read FAutocapitalizationType write FAutocapitalizationType default TDPFTextAutocapitalizationType.tapNone;
    property AutocorrectionType           : TDPFTextAutocorrectionType read FAutocorrectionType write FAutocorrectionType default TDPFTextAutocorrectionType.tatDefault;
    property SpellCheckingType            : TDPFTextSpellCheckingType read FSpellCheckingType write FSpellCheckingType default TDPFTextSpellCheckingType.tsctDefault;
    property EnablesReturnKeyAutomatically: Boolean read FEnablesReturnKeyAutomatically write FEnablesReturnKeyAutomatically default true;
    property KeyboardAppearance           : TDPFKeyboardAppearance read FKeyboardAppearance write FKeyboardAppearance default TDPFKeyboardAppearance.kaDefault;
    property ReturnKey                    : TDPFReturnKey read FReturnKey write FReturnKey default TDPFReturnKey.rkDefault;
    property SecureTextEntry              : Boolean read FSsecureTextEntry write FSsecureTextEntry default false;

    property BorderStyle            : TDPFTextBorderStyle read FBorderStyle write FBorderStyle default bsLine;
    property TextAlignment          : TDPFTextAlignment read FTextAlignment write SetTextAlignment default taLeft;
    property TextVerticalAlignment  : TDPFTextFieldVerticalAlignment read FTextVerticalAlignment write SetTextVerticalAlignment default vaCenter;
    property TextHorizontalAlignment: TDPFTextFieldHorizontalAlignment read FTextHorizontalAlignment write SetTextHorizontalAlignment default haCenter;
    property TextViewMode           : TDPFTextFieldViewMode read FTextViewMode write FTextViewMode default vmNever;
    property TextPlaceholder        : string read FTextPlaceholder write SetTextPlaceholder;
    property Text                   : string read GetText write setText;
    property TextColor              : TAlphaColor read FTextColor write setTextColor default TAlphaColors.Black;
    property BackgroundColor        : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.White;
    property BackgroundImage        : string read FBackgroundImage write SetBackgroundImage;
    property NormalImage            : string read FNormalImage write FNormalImage;
    property HighlightImage         : string read FHighlightImag write FHighlightImag;
    property DisableImage           : string read FDisableImage write FDisableImage;
    property Maxlength              : Longword read FMaxlength write FMaxlength default 0;
    property TextFormat             : TDPFTextFormat read FTextFormat write SetTextFormat;
    property Editable               : Boolean read FEditable write FEditable default true;

    property OnBeginEditing   : TDPFTextFieldBeginEditing read FOnBeginEditing write FOnBeginEditing;
    property OnEndEditing     : TDPFTextFieldEndEditing read FOnEndEditing write FOnEndEditing;
    property OnShouldClear    : TDPFTextFieldShouldClear read FOnShouldClear write FOnShouldClear;
    property OnShouldReturn   : TDPFTextFieldShouldReturn read FOnShouldReturn write FOnShouldReturn;
    property OnDidBeginEditing: TDPFTextFieldDidBeginEditing read FOnDidBeginEditing write FOnDidBeginEditing;
    property OnDidEndEditing  : TDPFTextFieldDidEndEditing read FDidEndEditing write FDidEndEditing;

    property AdjustsFontSizeToFitWidth: Boolean read FAdjustsFontSizeToFitWidth write SetAdjustsFontSizeToFitWidth default false;
    property AutoSizeType             : TDPFTextAutoSizeType read FAutoSizeType write SetAutoSizeType default TDPFTextAutoSizeType.astNone;
    property HideKeyboardOnReturn     : Boolean read FHideKeyboardOnReturn write FHideKeyboardOnReturn default false;
    property KeyboardType             : TDPFKeyboardType read FKeyboardType write setKeyboardType default ktDefault;
    property Enabled                  : Boolean read FEnabled write setEnabled default true;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Visible;
    property Align;
    property Position;
    property Width;
    property Height;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFTextField }
constructor TDPFTextField.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption             := 'TextField (TEdit)';
  FMaxlength                 := 0;
  FText                      := '';
  FTextColor                 := TAlphaColors.Black;
  FBackgroundColor           := TAlphaColors.White;
  FBorderStyle               := bsLine;
  FTextAlignment             := taLeft;
  FTextViewMode              := TDPFTextFieldViewMode.vmNever;
  FTextPlaceholder           := '';
  FTextHorizontalAlignment   := haCenter;
  FTextVerticalAlignment     := vaCenter;
  FKeyboardType              := ktDefault;
  FAdjustsFontSizeToFitWidth := false;
  FAutoSizeType              := TDPFTextAutoSizeType.astNone;
  FEnabled                   := true;
  FEditable                  := true;

  FAutocapitalizationType        := TDPFTextAutocapitalizationType.tapNone;
  FAutocapitalizationType        := TDPFTextAutocapitalizationType.tapNone;
  FAutocorrectionType            := TDPFTextAutocorrectionType.tatDefault;
  FSpellCheckingType             := TDPFTextSpellCheckingType.tsctDefault;
  FEnablesReturnKeyAutomatically := true;
  FKeyboardAppearance            := TDPFKeyboardAppearance.kaDefault;
  FReturnKey                     := TDPFReturnKey.rkDefault;
  FSsecureTextEntry              := false;
  FHideKeyboardOnReturn          := false;
  FTextFormat                    := TDPFTextFormat.Create( Self );
  FTextFormat.FTextKind          := TDPFTextFieldKind.tfkText;

{$IFDEF IOS}
  FUITextField := TUITextField.Create;
  FUIControl   := FUITextField;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFTextField.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FTextFieldDelegate ) then
    FTextFieldDelegate.DisposeOf;
{$ENDIF}
  FTextFormat.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFTextField.Loaded;
var
  Image: UIImage;
begin

  FUITextField.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  FUITextField.setTextColor( TColorToUIColor( FTextColor ) );

  FUITextField.setOpaque( true );
  FUITextField.setBorderStyle( Integer( FBorderStyle ) );
  FUITextField.setTextAlignment( Integer( FTextAlignment ) );
  FUITextField.setPlaceholder( NSStr( FTextPlaceholder ) );
  FUITextField.setClearButtonMode( Integer( FTextViewMode ) );

  FUITextField.setHidden( not Visible );
  FUITextField.setContentHorizontalAlignment( Integer( FTextHorizontalAlignment ) );
  FUITextField.setContentVerticalAlignment( Integer( FTextVerticalAlignment ) );

  FUITextField.setKeyboardType( Integer( FKeyboardType ) );
  FUITextField.setAutocapitalizationType( Integer( FAutocapitalizationType ) );
  FUITextField.setAutocorrectionType( Integer( FAutocorrectionType ) );
  FUITextField.setSpellCheckingType( Integer( FSpellCheckingType ) );
  FUITextField.setEnablesReturnKeyAutomatically( FEnablesReturnKeyAutomatically );
  FUITextField.setKeyboardAppearance( Integer( FKeyboardAppearance ) );
  FUITextField.setReturnKeyType( Integer( FReturnKey ) );
  FUITextField.setSecureTextEntry( FSsecureTextEntry );

  FUITextField.setFont( Font._UIFont );

  setEnabled( FEnabled );
  SetBackgroundColor( FBackgroundColor );

  if NormalImage <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( NormalImage ) ) );
    FUITextField.setBackground( Image );
    Image.release;
    Image := nil;
  end;

  if DisableImage <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( DisableImage ) ) );
    FUITextField.setDisabledBackground( Image );
    Image.release;
    Image := nil;
  end;

  if not Assigned( FTextFieldDelegate ) then
    FTextFieldDelegate := TDPFTextFieldDelegate.Create( Self );
  FUITextField.setDelegate( FTextFieldDelegate.GetObjectID );

  AddSubView( Self, ParentControl );

  SetAutoSizeType( FAutoSizeType );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTextField.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if FUITextField <> nil then FUITextField.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFTextField.Paint;
var
  Caption: string;
  R      : TRectF;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  case borderStyle of
    bsNone:
      begin
        Canvas.Fill.Kind   := TBrushKind.Solid;
        Canvas.Fill.Color  := BackgroundColor;
        Canvas.Stroke.Kind := TBrushKind.None;
        Canvas.FillRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
        R := ClipRect;
      end;
    bsLine:
      begin
        BitmapAsBorder( Self, iOS_GUI_Bitmaps.TextField.Line, 1 );
        Canvas.Fill.Kind   := TBrushKind.Solid;
        Canvas.Fill.Color  := BackgroundColor;
        Canvas.Stroke.Kind := TBrushKind.None;
        R                  := ClipRect;
        InflateRect( R, -1, -1 );
        Canvas.FillRect( R, 0, 0, AllCorners, 1, TCornerType.Round );
      end;
    bsBezel:
      begin
        BitmapAsBorder( Self, iOS_GUI_Bitmaps.TextField.Bezel, 2 );
        Canvas.Fill.Kind   := TBrushKind.Solid;
        Canvas.Fill.Color  := BackgroundColor;
        Canvas.Stroke.Kind := TBrushKind.None;
        R                  := ClipRect;
        R.Top              := R.Top + 2;
        R.Left             := R.Left + 2;
        R.Bottom           := R.Bottom - 1;
        R.Right            := R.Right - 1;
        Canvas.FillRect( R, 0, 0, AllCorners, 1, TCornerType.Round );
      end;
    bsRoundedRect:
      begin
        BitmapAsBorder( Self, iOS_GUI_Bitmaps.TextField.RoundedRect, 8, TAlphaColors.White );
        R := ClipRect;
        InflateRect( R, -5, 0 );
      end;
  end;
  Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := 'Helvetica';
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    if TextPlaceholder = '' then
    begin
      Caption           := name;
      Canvas.Fill.Color := TAlphaColors.Gray;
      Canvas.Font.Style := [TFontStyle.fsBold, TFontStyle.fsItalic];
    end
    else
    begin
      Caption           := TextPlaceHolder;
      Canvas.Fill.Color := TAlphaColors.Gray;
    end;
  end
  else
  begin
    Caption           := Text;
    Canvas.Fill.Color := TAlphaColors.Black;
  end;
  Canvas.FillText( R, Caption, False, 1, [], TTextAlign.Leading, TTextAlign.Center );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFTextField.GetText: string;
begin
  Result := FText;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    Result := UTF8ToString( TNSString.Wrap( FUITextField.text ).UTF8String );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.setText( const Value: string );
begin
  FText := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setText( NSStr( FText ) );
  end;
{$ENDIF}
  ControlCaption := FText;
  InvalidateRect( RectF( 0, 0, Width, Height ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetTextAlignment( const Value: TDPFTextAlignment );
begin
  FTextAlignment := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setTextAlignment( Integer( FTextAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.setTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setTextColor( TColorToUIColor( FTextColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetTextFormat( const Value: TDPFTextFormat );
begin
  FTextFormat.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetTextHorizontalAlignment( const Value: TDPFTextFieldHorizontalAlignment );
begin
  FTextHorizontalAlignment := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setContentHorizontalAlignment( Integer( FTextHorizontalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetTextPlaceholder( const Value: string );
begin
  FTextPlaceholder := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setPlaceholder( NSStr( FTextPlaceholder ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetTextVerticalAlignment( const Value: TDPFTextFieldVerticalAlignment );
begin
  FTextVerticalAlignment := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setContentVerticalAlignment( Integer( FTextVerticalAlignment ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SelectAll;
begin
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    //FUITextField.selectAll;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetAdjustsFontSizeToFitWidth( const Value: Boolean );
begin
  FAdjustsFontSizeToFitWidth := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setAdjustsFontSizeToFitWidth( FAdjustsFontSizeToFitWidth );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetAutoSizeType( const Value: TDPFTextAutoSizeType );
{$IFDEF IOS}
var
  NS: NSSize;
{$ENDIF}
begin
  FAutoSizeType := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    if FAutoSizeType = TDPFTextAutoSizeType.astSizeToFitFixHeight then
    begin
      NS := FUITextField.sizeThatFits( CGSizeMake( Width, Height ) );
      FUITextField.setFrame( CGRectMake( Position.X, Position.Y, NS.width, Max( NS.height, Height ) ) );
    end
    else if FAutoSizeType = TDPFTextAutoSizeType.astSizeToFit then
      FUITextField.sizeToFit;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    if ( FBackgroundColor = TAlphaColors.Null ) {$IFNDEF IOSDEVICE} or ( FBorderStyle = bsRoundedRect ) {$ENDIF} then
      FUITextField.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) )
    else
      FUITextField.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) );
  end;
{$ENDIF}
end;

procedure TDPFTextField.SetBackgroundImage( const Value: string );
{$IFDEF IOS}
var
  Img: UIImage;
{$ENDIF}
begin
  FBackgroundImage := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    if FBackgroundImage <> '' then
    begin
      Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FBackgroundImage ) ) );
      FUITextField.setBackground( Img );
      Img.release;
      Img := nil;
    end
    else
      FUITextField.setBackground( nil );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.setEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setEnabled( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextField.setKeyboardType( const Value: TDPFKeyboardType );
begin
  FKeyboardType := Value;
{$IFDEF IOS}
  if FUITextField <> nil then
  begin
    FUITextField.setKeyboardType( Integer( FKeyboardType ) );
  end;
{$ENDIF}
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TBtnDelegate }
constructor TDPFTextFieldDelegate.Create( ADPFTextField: TDPFTextField );
begin
  inherited Create;
  FDPFTextField := ADPFTextField;
end;

// ------------------------------------------------------------------------------
function TDPFTextFieldDelegate.textField( textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString ): Boolean;
var
  newLength: NSUInteger;
  ValueExt : Extended;
  ValueCurr: Currency;
  St       : string;
begin

  result := FDPFTextField.FEditable;
  if not FDPFTextField.FEditable then
    exit;

  St     := UTF8ToString( TNSString.Wrap( textField.text ).UTF8String ) + UTF8ToString( replacementString.UTF8String );
  Result := true;
  if FDPFTextField.TextFormat.FTextKind = TDPFTextFieldKind.tfkNumber then
    Result := IsNumeric( St )
  else if FDPFTextField.TextFormat.FTextKind = TDPFTextFieldKind.tfkFloat then
    Result := TryStrToFloat( St, ValueExt )
  else if FDPFTextField.TextFormat.FTextKind = TDPFTextFieldKind.tfkCurrency then
    Result := TryStrToCurr( St, ValueCurr );

  if not Result then
    exit;

  if Assigned( FDPFTextField.FOnDidBeginEditing ) then
    FDPFTextField.FOnDidBeginEditing( FDPFTextField );
  newLength := TNSString.Wrap( textField.text ).length + replacementString.length - shouldChangeCharactersInRange.length;
  result    := ( FDPFTextField.Maxlength = 0 ) or ( newLength < FDPFTextField.Maxlength + 1 );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextFieldDelegate.textFieldDidBeginEditing( textField: UITextField );
begin
  if Assigned( FDPFTextField.FOnDidBeginEditing ) then
    FDPFTextField.FOnDidBeginEditing( FDPFTextField );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextFieldDelegate.textFieldDidEndEditing( textField: UITextField );
begin
  if Assigned( FDPFTextField.FDidEndEditing ) then
    FDPFTextField.FDidEndEditing( FDPFTextField );
end;

// ------------------------------------------------------------------------------
function TDPFTextFieldDelegate.textFieldShouldBeginEditing( textField: UITextField ): Boolean;
begin
  result := true;
  if Assigned( FDPFTextField.OnBeginEditing ) then
    FDPFTextField.OnBeginEditing( FDPFTextField, result );
end;

// ------------------------------------------------------------------------------
function TDPFTextFieldDelegate.textFieldShouldEndEditing( textField: UITextField ): Boolean;
begin
  result := true;
  if Assigned( FDPFTextField.FOnEndEditing ) then
    FDPFTextField.FOnEndEditing( FDPFTextField, result );
end;

// ------------------------------------------------------------------------------
function TDPFTextFieldDelegate.textFieldShouldClear( textField: UITextField ): Boolean;
begin
  result := true;
  if Assigned( FDPFTextField.FOnShouldClear ) then
    FDPFTextField.FOnShouldClear( FDPFTextField, result );
end;

// ------------------------------------------------------------------------------
function TDPFTextFieldDelegate.textFieldShouldReturn( textField: UITextField ): Boolean;
begin
  result := true;
  if Assigned( FDPFTextField.FOnShouldReturn ) then
    FDPFTextField.FOnShouldReturn( FDPFTextField, result );

  if FDPFTextField.HideKeyboardOnReturn and result then
    textField.resignFirstResponder;
end;

// ------------------------------------------------------------------------------
{$ENDIF}
{ TKeyboardEventHandlerTextField }

{ function TKeyboardEventHandlerTextField.GetObjectiveCClass: PTypeInfo;
  begin
  Result := TypeInfo( IKeyboardEvents );
  end; }

// ------------------------------------------------------------------------------
{ TDPFTextFormat }

constructor TDPFTextFormat.Create( ADPFTextField: TDPFTextField );
begin
  inherited Create;
  FDPFTextField := ADPFTextField;
end;

// ------------------------------------------------------------------------------
destructor TDPFTextFormat.Destroy;
begin

  inherited;
end;

end.
