// ------------------------------------------------------------------------------
// DPF.iOS.UITextView Component
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
unit DPF.iOS.UITextView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.UIScrollView,
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
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
{$M+}
  TDPFTextView = class;

{$IFDEF IOS}

  UITextViewClass = interface( UIScrollViewClass )
    ['{94A993C8-9DEF-407F-B2AA-B1CC8C898511}']
  end;

  UITextView = interface( UIScrollView )
    ['{BCC358C2-93EB-430E-A25C-1C9E68241B21}']
    function dataDetectorTypes: UIDataDetectorTypes; cdecl;
    function delegate: Pointer; cdecl;
    function font: UIFont; cdecl;
    function hasText: Boolean; cdecl;
    function inputAccessoryView: UIView; cdecl;
    function inputView: UIView; cdecl;
    function isEditable: Boolean; cdecl;
    procedure scrollRangeToVisible( range: NSRange ); cdecl;
    function selectedRange: NSRange; cdecl;
    procedure setDataDetectorTypes( dataDetectorTypes: UIDataDetectorTypes ); cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;
    procedure setEditable( editable: Boolean ); cdecl;
    procedure setFont( font: UIFont ); cdecl;
    procedure setInputAccessoryView( inputAccessoryView: UIView ); cdecl;
    procedure setInputView( inputView: UIView ); cdecl;
    procedure setSelectedRange( selectedRange: NSRange ); cdecl;
    procedure setText( text: NSString ); cdecl;
    procedure setTextAlignment( textAlignment: UITextAlignment ); cdecl;
    procedure setTextColor( textColor: UIColor ); cdecl;
    function text: NSString; cdecl;
    function textAlignment: UITextAlignment; cdecl;
    function textColor: UIColor; cdecl;

    function selectedTextRange: UITextRange; cdecl;
    function textRangeFromPosition( fromPosition: UITextPosition; toPosition: UITextPosition ): UITextRange; cdecl;

    function beginningOfDocument: UITextPosition; cdecl;
    function endOfDocument: UITextPosition; cdecl;

    procedure replaceRange( range: UITextRange; withText: NSString ); cdecl;

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

  end;

  TUITextView = class( TOCGenericImport<UITextViewClass, UITextView> )
  end;

  DPFITextViewDelegate = interface( UIScrollViewDelegate )
    ['{49E0F81F-8450-43C1-945A-AA870573F1FF}']
    procedure textViewDidChange( textView: UITextView ); cdecl;
    function textView( textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; cdecl;

    procedure textViewDidBeginEditing( textView: UITextView ); cdecl;
    // procedure textViewDidChangeSelection( textView: UITextView ); cdecl;
    procedure textViewDidEndEditing( textView: UITextView ); cdecl;
    function textViewShouldBeginEditing( textView: UITextView ): Boolean; cdecl;
    function textViewShouldEndEditing( textView: UITextView ): Boolean; cdecl;

    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidZoom( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: Pointer ); cdecl;
    procedure scrollViewDidEndDragging( scrollView: UIScrollView; willDecelerate: Boolean ); cdecl;
    procedure scrollViewWillBeginDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndScrollingAnimation( scrollView: UIScrollView ); cdecl;
    function viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;
    procedure scrollViewWillBeginZooming( scrollView: UIScrollView; withView: UIView ); cdecl;
    procedure scrollViewDidEndZooming( scrollView: UIScrollView; withView: UIView; atScale: CGFloat ); cdecl;
    function scrollViewShouldScrollToTop( scrollView: UIScrollView ): Boolean; cdecl;
    procedure scrollViewDidScrollToTop( scrollView: UIScrollView ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  DPFUITextView = interface( UITextView )
    ['{D76C427B-887F-49B2-A06D-A77000B087CF}']

    function canPerformAction( action: SEL; withSender: Pointer ): Boolean; cdecl;
  end;

  TDPFUITextView = class( TOCLocal )
  private
  protected
    FDPFTextView: TDPFTextView;
  public
    constructor Create( ADPFTextView: TDPFTextView );
    function GetObjectiveCClass: PTypeInfo; override;

    function canPerformAction( action: SEL; withSender: Pointer ): Boolean; cdecl;
  end;

  // ----------------------------------------------------------------------------
  TDPFTextViewDelegate = class( TOCLocal, DPFITextViewDelegate )
  private
    FDPFTextView: TDPFTextView;
  public
    constructor Create( ADPFTextView: TDPFTextView );
    // function GetObjectiveCClass: PTypeInfo; override;

    procedure textViewDidChange( textView: UITextView ); cdecl;
    function textView( textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; cdecl;

    procedure scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;

    procedure scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
    function viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;

    procedure textViewDidBeginEditing( textView: UITextView ); cdecl;
    procedure textViewDidEndEditing( textView: UITextView ); cdecl;
    function textViewShouldBeginEditing( textView: UITextView ): Boolean; cdecl;
    function textViewShouldEndEditing( textView: UITextView ): Boolean; cdecl;

    // procedure textViewDidChangeSelection( textView: UITextView ); cdecl;

    procedure scrollViewDidZoom( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: Pointer ); cdecl;
    procedure scrollViewDidEndDragging( scrollView: UIScrollView; willDecelerate: Boolean ); cdecl;
    procedure scrollViewWillBeginDecelerating( scrollView: UIScrollView ); cdecl;
    procedure scrollViewDidEndScrollingAnimation( scrollView: UIScrollView ); cdecl;
    procedure scrollViewWillBeginZooming( scrollView: UIScrollView; withView: UIView ); cdecl;
    procedure scrollViewDidEndZooming( scrollView: UIScrollView; withView: UIView; atScale: CGFloat ); cdecl;
    function scrollViewShouldScrollToTop( scrollView: UIScrollView ): Boolean; cdecl;
    procedure scrollViewDidScrollToTop( scrollView: UIScrollView ); cdecl;
  end;
{$ENDIF}

  TDPFTextViewBeginEditing    = procedure( Sender: TObject; isBeginEditing: Boolean ) of object;
  TDPFTextViewEndEditing      = procedure( Sender: TObject; isEndEditing: Boolean ) of object;
  TDPFTextViewShouldClear     = procedure( Sender: TObject; isShouldClear: Boolean ) of object;
  TDPFTextViewShouldReturn    = procedure( Sender: TObject; isShouldReturn: Boolean ) of object;
  TDPFTextViewDidBeginEditing = procedure( Sender: TObject ) of object;
  TDPFTextViewDidEndEditing   = procedure( Sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFTextView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFUITextView      : TDPFUITextView;
    FUITextView         : UITextView;
    FDPFTextViewDelegate: TDPFTextViewDelegate;
{$ENDIF}
    FText                         : string;
    FTextColor                    : TAlphaColor;
    FBackgroundColor              : TAlphaColor;
    FTextAlignment                : TDPFTextAlignment;
    FScrollEnabled                : Boolean;
    FEditable                     : Boolean;
    FSsecureTextEntry             : Boolean;
    FKeyboardAppearance           : TDPFKeyboardAppearance;
    FEnablesReturnKeyAutomatically: Boolean;
    FSpellCheckingType            : TDPFTextSpellCheckingType;
    FAutocorrectionType           : TDPFTextAutocorrectionType;
    FAutocapitalizationType       : TDPFTextAutocapitalizationType;
    FReturnKey                    : TDPFReturnKey;
    FKeyboardType                 : TDPFKeyboardType;
    FOnTextChanged                : TNotifyEvent;
    FMaxlength                    : Integer;
    FDidEndEditing                : TDPFTextViewDidEndEditing;
    FOnShouldReturn               : TDPFTextViewShouldReturn;
    FOnShouldClear                : TDPFTextViewShouldClear;
    FOnEndEditing                 : TDPFTextViewEndEditing;
    FOnDidBeginEditing            : TDPFTextViewDidBeginEditing;
    FOnBeginEditing               : TDPFTextViewBeginEditing;
    FDisableActions               : Boolean;
    procedure SetText( const Value: string );
    procedure SetTextColor( const Value: TAlphaColor );
    procedure SetBackgroundColor( const Value: TAlphaColor );
    function GetText: string;
    procedure SetEditable( const Value: Boolean );
    procedure setKeyboardType( const Value: TDPFKeyboardType );
    function GetSelText: string;
    procedure SetSelText( const Value: string );

  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    property SelText: string read GetSelText write SetSelText;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property TextAlignment  : TDPFTextAlignment read FTextAlignment write FTextAlignment default taLeft;
    property Text           : string read GetText write SetText;
    property TextColor      : TAlphaColor read FTextColor write SetTextColor default TAlphaColors.Black;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.White;
    property ScrollEnabled  : Boolean read FScrollEnabled write FScrollEnabled default True;
    property Editable       : Boolean read FEditable write SetEditable default True;
    property Maxlength      : Integer read FMaxlength write FMaxlength default 0;
    property DisableActions : Boolean read FDisableActions write FDisableActions default false;

    property AutocapitalizationType       : TDPFTextAutocapitalizationType read FAutocapitalizationType write FAutocapitalizationType default TDPFTextAutocapitalizationType.tapNone;
    property AutocorrectionType           : TDPFTextAutocorrectionType read FAutocorrectionType write FAutocorrectionType default TDPFTextAutocorrectionType.tatDefault;
    property SpellCheckingType            : TDPFTextSpellCheckingType read FSpellCheckingType write FSpellCheckingType default TDPFTextSpellCheckingType.tsctDefault;
    property EnablesReturnKeyAutomatically: Boolean read FEnablesReturnKeyAutomatically write FEnablesReturnKeyAutomatically default true;
    property KeyboardAppearance           : TDPFKeyboardAppearance read FKeyboardAppearance write FKeyboardAppearance default TDPFKeyboardAppearance.kaDefault;
    property ReturnKey                    : TDPFReturnKey read FReturnKey write FReturnKey default TDPFReturnKey.rkDefault;
    property SecureTextEntry              : Boolean read FSsecureTextEntry write FSsecureTextEntry default false;
    property KeyboardType                 : TDPFKeyboardType read FKeyboardType write setKeyboardType default ktDefault;

    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;

    property OnBeginEditing   : TDPFTextViewBeginEditing read FOnBeginEditing write FOnBeginEditing;
    property OnEndEditing     : TDPFTextViewEndEditing read FOnEndEditing write FOnEndEditing;
    property OnShouldClear    : TDPFTextViewShouldClear read FOnShouldClear write FOnShouldClear;
    property OnShouldReturn   : TDPFTextViewShouldReturn read FOnShouldReturn write FOnShouldReturn;
    property OnDidBeginEditing: TDPFTextViewDidBeginEditing read FOnDidBeginEditing write FOnDidBeginEditing;
    property OnDidEndEditing  : TDPFTextViewDidEndEditing read FDidEndEditing write FDidEndEditing;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

// ------------------------------------------------------------------------------
constructor TDPFTextView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'Text View (Memo)';
  FMaxlength       := 0;
  FText            := '';
  FDisableActions  := false;
  FTextColor       := TAlphaColors.Black;
  FBackgroundColor := TAlphaColors.White;
  FTextAlignment   := taLeft;
  FEditable        := True;
  FScrollEnabled   := True;

  FAutocapitalizationType        := TDPFTextAutocapitalizationType.tapNone;
  FAutocapitalizationType        := TDPFTextAutocapitalizationType.tapNone;
  FAutocorrectionType            := TDPFTextAutocorrectionType.tatDefault;
  FSpellCheckingType             := TDPFTextSpellCheckingType.tsctDefault;
  FEnablesReturnKeyAutomatically := true;
  FKeyboardAppearance            := TDPFKeyboardAppearance.kaDefault;
  FReturnKey                     := TDPFReturnKey.rkDefault;
  FSsecureTextEntry              := false;

{$IFDEF IOS}
  FDPFUITextView := TDPFUITextView.Create( self );
  // FUITextView    := TUITextView.Wrap( FDPFUITextView.Super.init );
  FUITextView := UITextView( FDPFUITextView.Super );
  FUIControl  := FUITextView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFTextView.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FDPFTextViewDelegate ) then
    FDPFTextViewDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFTextView.Loaded;
begin

  if not Assigned( FDPFTextViewDelegate ) then
  begin
    FDPFTextViewDelegate := TDPFTextViewDelegate.Create( Self );
  end;

  SetText( FText );

  FUITextView.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  FUITextView.setTextColor( TColorToUIColor( FTextColor ) );

  SetBackgroundColor( FBackgroundColor );

  FUITextView.setTextAlignment( Integer( FTextAlignment ) );
  FUITextView.setHidden( not Visible );
  FUITextView.setScrollEnabled( FScrollEnabled );
  FUITextView.setShowsHorizontalScrollIndicator( True );
  FUITextView.setShowsVerticalScrollIndicator( True );
  FUITextView.setFont( Font._UIFont );

  FUITextView.setKeyboardType( Integer( FKeyboardType ) );
  FUITextView.setAutocapitalizationType( Integer( FAutocapitalizationType ) );
  FUITextView.setAutocorrectionType( Integer( FAutocorrectionType ) );
  FUITextView.setSpellCheckingType( Integer( FSpellCheckingType ) );
  FUITextView.setEnablesReturnKeyAutomatically( FEnablesReturnKeyAutomatically );
  FUITextView.setKeyboardAppearance( Integer( FKeyboardAppearance ) );
  FUITextView.setReturnKeyType( Integer( FReturnKey ) );
  FUITextView.setSecureTextEntry( FSsecureTextEntry );

  if Assigned( FDPFTextViewDelegate ) then
    FUITextView.setDelegate( FDPFTextViewDelegate.GetObjectID );

  SetEditable( FEditable );

  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTextView.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if FUITextView <> nil then FUITextView.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFTextView.Paint;
var
  Caption: string;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  Canvas.Fill.Kind  := TBrushKind.Solid;
  Canvas.Fill.Color := BackgroundColor;
  Canvas.FillRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
  Canvas.Font.Size   := Font.FontSize;
  Canvas.Font.Style  := [TFontStyle.fsBold];
  Canvas.Font.Family := 'Helvetica';
  if ( Text = '' ) and ( csDesigning in ComponentState ) then
  begin
    Caption           := name;
    Canvas.Fill.Color := TAlphaColors.Gray;
  end
  else
  begin
    Caption           := Text;
    Canvas.Fill.Color := TAlphaColors.Black;
  end;
  Canvas.FillText( ClipRect, Caption, True, 1, [], TTextAlign.Leading, TTextAlign.Leading );
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFTextView.GetSelText: string;
begin
{$IFDEF IOS}
  with FUITextView.selectedRange do
    Result := Copy( FText, location, length + 1 );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFTextView.GetText: string;
begin
  Result := FText;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
    Result := UTF8ToString( FUITextView.text.UTF8String );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.SetSelText( const Value: string );
{$IFDEF IOS}
var
  textRange: UITextRange;
  ns       : NSString;
{$ENDIF}
begin
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
{$IFNDEF DELPHIXE6}
    if Value = '' then
      ns := TNSString.Create
    else
      ns := NSStr( Value );
{$ELSE}
    ns := NSStr( Value );
{$ENDIF}
    textRange := FUITextView.selectedTextRange;
    FUITextView.replaceRange( textRange, ns );
    FUITextView.setText( FUITextView.text );
{$IFNDEF DELPHIXE6}
    if Value = '' then
      ns.release;
{$ENDIF}
  end;
{$ELSE}
  ControlCaption := FText;
  InvalidateRect( RectF( 0, 0, Width, Height ) );;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.SetText( const Value: string );
{$IFDEF IOS}
var
  textRange: UITextRange;
  ns       : NSString;
{$ENDIF}
begin
  FText := Value;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
{$IFNDEF DELPHIXE6}
    if Value = '' then
      ns := TNSString.Create
    else
      ns := NSStr( FText );
{$ELSE}
    ns := NSStr( FText );
{$ENDIF}
    textRange := FUITextView.textRangeFromPosition( FUITextView.beginningOfDocument, FUITextView.endOfDocument );
    FUITextView.replaceRange( textRange, ns );
    FUITextView.setText( ns );
{$IFNDEF DELPHIXE6}
    if Value = '' then
      ns.release;
{$ENDIF}
  end;
{$ELSE}
  ControlCaption := FText;
  InvalidateRect( RectF( 0, 0, Width, Height ) );;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.SetTextColor( const Value: TAlphaColor );
begin
  FTextColor := Value;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
    FUITextView.setTextColor( TColorToUIColor( FTextColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUITextView.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUITextView.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.SetEditable( const Value: Boolean );
begin
  FEditable := Value;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
    FUITextView.setEditable( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTextView.setKeyboardType( const Value: TDPFKeyboardType );
begin
  FKeyboardType := Value;
{$IFDEF IOS}
  if FUITextView <> nil then
  begin
    FUITextView.setKeyboardType( Integer( FKeyboardType ) );
  end;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFTextViewDelegate }

constructor TDPFTextViewDelegate.Create( ADPFTextView: TDPFTextView );
begin
  inherited Create;
  FDPFTextView := ADPFTextView;
end;

// ------------------------------------------------------------------------------
function TDPFTextViewDelegate.textView( textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString ): Boolean; cdecl;
var
  newLength: Integer;
begin
  result := FDPFTextView.Editable;
  if not result then
    exit;

  newLength := textView.text.length + replacementText.length - shouldChangeTextInRange.length;
  result    := ( FDPFTextView.Maxlength = 0 ) or ( newLength < FDPFTextView.Maxlength + 1 );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.textViewDidBeginEditing( textView: UITextView ); cdecl;
begin
  if Assigned( FDPFTextView.OnDidBeginEditing ) then
    FDPFTextView.OnDidBeginEditing( FDPFTextView );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.textViewDidChange( textView: UITextView ); cdecl;
begin
  if Assigned( FDPFTextView.OnTextChanged ) then
    FDPFTextView.OnTextChanged( FDPFTextView );
end;

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.scrollViewDidScroll( scrollView: UIScrollView ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.scrollViewWillBeginDragging( scrollView: UIScrollView ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.scrollViewDidEndDecelerating( scrollView: UIScrollView ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
function TDPFTextViewDelegate.viewForZoomingInScrollView( scrollView: UIScrollView ): UIView; cdecl;
begin
end;

// ------------------------------------------------------------------------------
(* procedure TDPFTextViewDelegate.textViewDidChangeSelection( textView: UITextView );
  begin

  end; *)

// ------------------------------------------------------------------------------
procedure TDPFTextViewDelegate.textViewDidEndEditing( textView: UITextView );
begin
  if Assigned( FDPFTextView.OnDidEndEditing ) then
    FDPFTextView.OnDidEndEditing( FDPFTextView );
end;

// ------------------------------------------------------------------------------
function TDPFTextViewDelegate.textViewShouldBeginEditing( textView: UITextView ): Boolean;
begin
  result := true;
  if Assigned( FDPFTextView.OnBeginEditing ) then
    FDPFTextView.OnBeginEditing( FDPFTextView, result );
end;

// ------------------------------------------------------------------------------
function TDPFTextViewDelegate.textViewShouldEndEditing( textView: UITextView ): Boolean;
begin
  result := true;
  // textView.setSelectedRange( NSMakeRange( 0, textView.text.length ) );
  if Assigned( FDPFTextView.FOnEndEditing ) then
    FDPFTextView.FOnEndEditing( FDPFTextView, result );
end;

procedure TDPFTextViewDelegate.scrollViewDidZoom( scrollView: UIScrollView );
begin
end;

procedure TDPFTextViewDelegate.scrollViewWillEndDragging( scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: Pointer ); cdecl;
begin
end;

procedure TDPFTextViewDelegate.scrollViewDidEndDragging( scrollView: UIScrollView; willDecelerate: Boolean ); cdecl;
begin
end;

procedure TDPFTextViewDelegate.scrollViewWillBeginDecelerating( scrollView: UIScrollView ); cdecl;
begin
end;

procedure TDPFTextViewDelegate.scrollViewDidEndScrollingAnimation( scrollView: UIScrollView ); cdecl;
begin
end;

procedure TDPFTextViewDelegate.scrollViewWillBeginZooming( scrollView: UIScrollView; withView: UIView ); cdecl;
begin
end;

procedure TDPFTextViewDelegate.scrollViewDidEndZooming( scrollView: UIScrollView; withView: UIView; atScale: CGFloat ); cdecl;
begin
end;

function TDPFTextViewDelegate.scrollViewShouldScrollToTop( scrollView: UIScrollView ): Boolean; cdecl;
begin
  result := false;
end;

procedure TDPFTextViewDelegate.scrollViewDidScrollToTop( scrollView: UIScrollView ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
{ TDPFUIImageView }

constructor TDPFUITextView.Create( ADPFTextView: TDPFTextView );
var
  V: Pointer;
begin
  inherited Create;
  FDPFTextView := ADPFTextView;
  V            := UITextView( Super ).initWithFrame( CGRectMake( 0, 0, 100, 100 ) );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFUITextView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFUITextView );
end;

// ------------------------------------------------------------------------------
function TDPFUITextView.canPerformAction( action: SEL; withSender: Pointer ): Boolean; cdecl;
begin
  // TUIMenuController.Wrap( TUIMenuController.OCClass.sharedMenuController ).setMenuVisible( false );
  if not FDPFTextView.FDisableActions then
    result := UITextView( Super ).canPerformAction( action, withSender )
  else
    result := false;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
