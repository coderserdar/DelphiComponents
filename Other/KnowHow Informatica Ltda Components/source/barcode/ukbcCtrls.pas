{
=======================================================================

    KLIB v100
    Serious Software Made in Brazil


    home-page: www.knowhow-online.com.br (sorry, just portuguese)
    authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

    Released under the Netscape Public License Version 1.0 
   (see license.txt)

    Unless otherwise noted, all materials provided in this release
    are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukbcCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  Classes, Graphics, Controls, ukbcClasses;

type

{ TKCustomBarcode }

	TKCustomBarcode = class( TGraphicControl )
	private
		FFontChangedLocked: Boolean;
		FBarcode: TKCustomBarcodeComponent;

		function GetAddOnCaption: TKAddOnCaption;
		function GetAddOnString: String;
		function GetAddOnStyle: TKAddOnStyle;
		function GetAlign: TAlign;
		function GetAlignment: TAlignment;
		function GetAutosize: Boolean;
		function GetBarCaption: TKBarCaption;
		function GetBarcode: String;
		function GetBarColor: TColor;
		function GetBarCrackColor: TColor;
		function GetBarDigit: TKBarDigit;
		function GetBarKind: TKBarKind;
		function GetBarString: String;
		function GetBarWidth: Byte;
		function GetFont: TFont;
		function GetHeight: Integer;
		function GetPaddingStyle: TKPaddingStyle;
		function GetShowCaption: Boolean;
		function GetUseCheckDigit: Boolean;
		function GetThickBarRatio: TKThickBar;
		function GetTransparent: Boolean;
		function GetStretched: Boolean;
		function GetWidth: Integer;

		procedure SetAlign( Value: TAlign );
		procedure SetAlignment( Value: TAlignment );
		procedure SetAutosize( Value: Boolean );
		procedure SetBarCaption( Value: TKBarCaption );
		procedure SetBarColor( Value: TColor );
		procedure SetBarCrackColor( Value: TColor );
		procedure SetBarDigit( Value: TKBarDigit );
		procedure SetBarKind( Value: TKBarKind );
		procedure SetBarString( const Value: String );
		procedure SetAddOnString( const Value: String );
		procedure SetBarWidth( Value: Byte );
		procedure SetFont( Value: TFont );
		procedure SetHeight( Value: Integer );
		procedure SetAddOnStyle( Value: TKAddOnStyle );
		procedure SetAddOnCaption( const Value: TKAddOnCaption );
		procedure SetPaddingStyle( Value: TKPaddingStyle );
		procedure SetShowCaption( Value: Boolean );
		procedure SetUseCheckDigit( Value: Boolean );
		procedure SetThickBarRatio( Value: TKThickBar );
		procedure SetTransparent( Value: Boolean );
		procedure SetStretched( Value: Boolean );
		procedure SetWidth( Value: Integer );

	protected
		procedure Paint; override;
		procedure FontChanged( Sender: TObject ); virtual;
		procedure LockFontChanged( Value: Boolean ); virtual;
		procedure AdjustFontPainting( Sender: TObject ); virtual;

{ these are supposed to be published }
		property AddOnCaption: TKAddOnCaption
						 read GetAddOnCaption write SetAddOnCaption;
		property BarCaption: TKBarCaption
						 read GetBarCaption write SetBarCaption;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure Setbounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

{ these do not have to be published }
		property Barcode: String
						 read GetBarcode;

		property AddOnStyle: TKAddOnStyle
						 read GetAddOnStyle write SetAddOnStyle default asNone;
		property AddOnString: String
						 read GetAddOnString write SetAddOnString;
		property Align: TAlign
						 read GetAlign write SetAlign;
		property Alignment: TAlignment
						 read GetAlignment write SetAlignment default taLeftJustify;
		property Autosize: Boolean
						 read GetAutosize write SetAutosize default true;
		property BarColor: TColor
						 read GetBarColor  write SetBarColor default clWindowText;
		property BarCrackColor: TColor
						 read GetBarCrackColor write SetBarCrackColor default clWindow;
		property BarDigit: TKBarDigit
						 read GetBarDigit write SetBarDigit;
		property BarKind: TKBarKind
						 read GetBarKind write SetBarKind default bkEAN13;
		property BarString: String
						 read GetBarString write SetBarString;
		property BarWidth: Byte
						 read GetBarWidth write SetBarWidth default 2;
		property Font: TFont
						 read GetFont write SetFont;
		property PaddingStyle: TKPaddingStyle
						 read GetPaddingStyle write SetPaddingStyle default psLeading;
		property ShowCaption: Boolean
						 read GetShowCaption write SetShowCaption default true;
		property Stretched: Boolean
						 read GetStretched write SetStretched default false;
		property UseCheckDigit: Boolean
						 read GetUseCheckDigit write SetUseCheckDigit default false;
		property ThickBarRatio: TKThickBar
						 read GetThickBarRatio write SetThickBarRatio default 3;
		property Transparent: Boolean
						 read GetTransparent write SetTransparent default true;

	published
		property Height: Integer
						 read GetHeight write SetHeight;
		property Width: Integer
						 read GetWidth write SetWidth;

	end;

{ TKBarcode }

	TKBarcode = class( TKCustomBarcode )
	published
{ these are supposed to be published }
		property AddOnCaption;
		property AddOnStyle;
		property AddOnString;
		property Align;
		property Alignment;
		property Autosize;
		property BarCaption;
		property BarColor;
		property BarCrackColor;
		property BarDigit;
		property BarKind;
		property BarString;
		property BarWidth;
		property Font;
		property PaddingStyle;
		property ShowCaption;
		property Stretched;
		property ThickBarRatio;
		property Transparent;
		property UseCheckDigit;

		property ShowHint;
		property Visible;

		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;

	end;

implementation

uses
  ukbcUtils;

{
-------------------------------------------------------------------------
---------------------------- TKCustomBarcode Class ----------------------------
-------------------------------------------------------------------------
}

constructor TKCustomBarcode.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBarCode := TKCustomBarcodeComponent.CreateLinked( Self );
	FFontChangedLocked := false;
	Font.OnChange := FontChanged;
	FBarcode.OnAdjustFontPainting := AdjustFontPainting;
end;

destructor TKCustomBarcode.Destroy;
begin
	FBarCode.Free;
	FBarCode := nil;
	inherited Destroy;
end;

procedure TKCustomBarcode.Paint;
begin
	FBarcode.Paint;
end;

procedure TKCustomBarcode.Setbounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
	if ( FBarcode <> nil ) then
		FBarcode.SetPropChanged( ( AWidth <> Width ) or ( AHeight <> Height ) );
	inherited SetBounds( ALeft, ATop, AWidth, AHeight );
end;

procedure TKCustomBarcode.LockFontChanged( Value: Boolean );
begin
	FFontChangedLocked := Value;
end;

procedure TKCustomBarcode.AdjustFontPainting( Sender: TObject );
begin
	LockFontChanged( true );
	try
		Font.Assign( FBarcode.Font );
	finally
		LockFontChanged( false );
	end;
end;

procedure TKCustomBarcode.FontChanged( Sender: TObject );
begin
	if ( FBarcode <> nil ) and ( not FFontChangedLocked ) then
		FBarcode.Font := Font;
end;

function TKCustomBarcode.GetAddOnCaption: TKAddOnCaption;
begin
	Result := FBarcode.AddOnCaption;
end;

function TKCustomBarcode.GetAddOnString: String;
begin
	Result := FBarcode.AddOnString;
end;

function TKCustomBarcode.GetAddOnStyle: TKAddOnStyle;
begin
	Result := FBarcode.AddOnStyle;
end;

function TKCustomBarcode.GetAlign: TAlign;
begin
	Result := FBarcode.Align;
end;

function TKCustomBarcode.GetAlignment: TAlignment;
begin
	Result := FBarcode.Alignment;
end;

{
function TKCustomBarcode.GetAngle: TKAngle;
begin
	Result := FBarcode.Angle;
end;
}

function TKCustomBarcode.GetAutosize: Boolean;
begin
	Result := FBarcode.Autosize;
end;

function TKCustomBarcode.GetBarCaption: TKBarCaption;
begin
	Result := FBarcode.BarCaption;
end;

function TKCustomBarcode.GetBarcode: String;
begin
  Result := FBarcode.Barcode;
end;

function TKCustomBarcode.GetBarColor: TColor;
begin
	Result := FBarcode.BarColor;
end;

function TKCustomBarcode.GetBarCrackColor: TColor;
begin
	Result := FBarcode.BarCrackColor;
end;

function TKCustomBarcode.GetBarDigit: TKBarDigit;
begin
	Result := FBarcode.BarDigit;
end;

function TKCustomBarcode.GetBarKind: TKBarKind;
begin
	Result := FBarcode.BarKind;
end;

function TKCustomBarcode.GetBarString: String;
begin
	Result := FBarcode.BarString;
end;

function TKCustomBarcode.GetBarWidth: Byte;
begin
	Result := FBarcode.BarWidth;
end;

function TKCustomBarcode.GetFont: TFont;
begin
	Result := FBarcode.Font;
end;

function TKCustomBarcode.GetHeight: Integer;
begin
	Result := inherited Height;
end;

function TKCustomBarcode.GetPaddingStyle: TKPaddingStyle;
begin
	Result := FBarcode.PaddingStyle;
end;

function TKCustomBarcode.GetShowCaption: Boolean;
begin
	Result := FBarcode.ShowCaption;
end;

function TKCustomBarcode.GetStretched: Boolean;
begin
	Result := FBarcode.Stretched;
end;

function TKCustomBarcode.GetUseCheckDigit: Boolean;
begin
	Result := FBarcode.UseCheckDigit;
end;

function TKCustomBarcode.GetThickBarRatio: TKThickBar;
begin
	Result := FBarcode.ThickBarRatio;
end;

function TKCustomBarcode.GetTransparent: Boolean;
begin
	Result := FBarcode.Transparent;
end;

function TKCustomBarcode.GetWidth: Integer;
begin
	Result := inherited Width;
end;

procedure TKCustomBarcode.SetAddOnStyle( Value: TKAddOnStyle );
begin
	FBarcode.AddOnStyle := Value;
end;

procedure TKCustomBarcode.SetAddOnCaption( const Value: TKAddOnCaption );
begin
	FBarcode.AddOnCaption := Value;
end;

procedure TKCustomBarcode.SetAlign( Value: TAlign );
begin
	FBarcode.Align := Value;
end;

procedure TKCustomBarcode.SetAlignment( Value: TAlignment );
begin
	FBarcode.Alignment := Value;
end;

{
procedure TKCustomBarcode.SetAngle( Value: TKAngle );
begin
	FBarcode.Angle := Value;
end;
}

procedure TKCustomBarcode.SetAutosize( Value: Boolean );
begin
	FBarcode.AutoSize := Value;
end;

procedure TKCustomBarcode.SetBarCaption( Value: TKBarCaption );
begin
	FBarcode.BarCaption := Value;
end;

procedure TKCustomBarcode.SetBarColor( Value: TColor );
begin
	FBarcode.BarColor := Value;
end;

procedure TKCustomBarcode.SetBarCrackColor( Value: TColor );
begin
	FBarcode.BarCrackColor := Value;
end;

procedure TKCustomBarcode.SetBarDigit( Value: TKBarDigit );
begin
end;

procedure TKCustomBarcode.SetBarKind( Value: TKBarKind );
begin
	FBarcode.BarKind := Value;
end;

procedure TKCustomBarcode.SetAddOnString( const Value: String );
begin
end;

procedure TKCustomBarcode.SetBarString( const Value: String );
begin
end;

procedure TKCustomBarcode.SetBarWidth( Value: Byte );
begin
	FBarcode.BarWidth := Value;
end;

procedure TKCustomBarcode.SetFont( Value: TFont );
begin
	inherited Font.Assign( Value );
	FBarcode.Font := Value;
end;

procedure TKCustomBarcode.SetHeight( Value: Integer );
begin
	FBarcode.SetPropChanged( true );
	inherited Height := Value;
end;

procedure TKCustomBarcode.SetWidth( Value: Integer );
begin
	FBarcode.SetPropChanged( true );
	inherited Width := Value;
end;

procedure TKCustomBarcode.SetPaddingStyle( Value: TKPaddingStyle );
begin
	FBarcode.PaddingStyle := Value;
end;

procedure TKCustomBarcode.SetShowCaption( Value: Boolean );
begin
	FBarcode.ShowCaption := Value;
end;

procedure TKCustomBarcode.SetUseCheckDigit( Value: Boolean );
begin
	FBarcode.UseCheckDigit := Value;
end;

procedure TKCustomBarcode.SetThickBarRatio( Value: TKThickBar );
begin
	FBarcode.ThickBarRatio := Value;
end;

procedure TKCustomBarcode.SetStretched( Value: Boolean );
begin
	FBarCode.Stretched := Value;
end;

procedure TKCustomBarcode.SetTransparent( Value: Boolean );
begin
	FBarcode.Transparent := Value;
end;

end.
