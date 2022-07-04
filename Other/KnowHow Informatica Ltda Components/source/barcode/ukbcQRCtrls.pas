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

unit ukbcQRCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

{Compile as Deny Package to avoid QuickReport Version problems. Free Source.
 But the user may put it in any internal package... }
{.$DENYPACKAGEUNIT ON}

uses
{$IFDEF DELPHI4}
	QuickRpt,
{$ELSE}
	{$IFDEF DELPHI3}
		QuickRpt,
	{$ELSE}
		{$IFDEF DELPHI2}
		QuickRep,
		{$ENDIF}
	{$ENDIF}
{$ENDIF}
	Classes, Controls, Messages, DB, DBTables, Graphics, DBCtrls,
	ukbcClasses;

type

{ QuickReport support for Delphi 2.0, 3.0 and 4.0 }

{ TKCustomQRBarcode }

{$IFDEF DELPHI4}
	TKCustomQRBarcode = class( TQRPrintable )
{$ELSE}
	{$IFDEF DELPHI3}
		TKCustomQRBarcode = class( TQRPrintable )
	{$ELSE}
		{$IFDEF DELPHI2}
		TKCustomQRBarcode = class( TQRCustomControl )
		{$ENDIF}
	{$ENDIF}
{$ENDIF}
	private
		FFontChangedLocked: Boolean;
		FBarcode: TKCustomBarcodeComponent;

	{ function GetAngle: TKAngle; }
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

	{	procedure SetAngle( Value: TKAngle ); }
		procedure SetAlign( Value: TAlign );
		procedure SetAlignment( Value: TAlignment ); override;
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
		procedure Print( X, Y: Integer ); override;
		procedure FontChanged( Sender: TObject ); virtual;
		procedure LockFontChanged( Value: Boolean ); virtual;
		procedure AdjustFontPainting( Sender: TObject ); virtual;

		property Barcode: String
						 read GetBarcode;
		property AddOnCaption: TKAddOnCaption
						 read GetAddOnCaption write SetAddOnCaption;
		property BarCaption: TKBarCaption
						 read GetBarCaption write SetBarCaption;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure Setbounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

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

{ TKQRBarcode }

	TKQRBarcode = class( TKCustomQRBarcode )
	published
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
  Windows, uksyUtils, ukbcResStr, ukbcUtils;

{
-------------------------------------------------------------------------
------------------------ TKCustomQRBarcode Class ------------------------
-------------------------------------------------------------------------
}

constructor TKCustomQRBarcode.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBarCode := TKCustomBarcodeComponent.CreateLinked( Self );
	FFontChangedLocked := false;
	Font.OnChange := FontChanged;
	FBarcode.OnAdjustFontPainting := AdjustFontPainting;
end;

destructor TKCustomQRBarcode.Destroy;
begin
	FreeClean( FBarCode );
	inherited Destroy;
end;

procedure TKCustomQRBarcode.Paint;
begin
	FBarcode.Paint;
end;

procedure TKCustomQRBarcode.Print( X, Y: Integer );
begin
	inherited Print( X, Y );
	if FBarcode.Transparent then
		BitBlt( QRPrinter.Canvas.Handle, X, Y, Width, Height, Canvas.Handle, 0, 0, SRCAND )
	else
		BitBlt( QRPrinter.Canvas.Handle, X, Y, Width, Height, Canvas.Handle, 0, 0, SRCCOPY );
end;

procedure TKCustomQRBarcode.Setbounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
	if CheckObject( FBarcode ) then
		FBarcode.SetPropChanged( ( AWidth <> Width ) or ( AHeight <> Height ) );
	inherited SetBounds( ALeft, ATop, AWidth, AHeight );
end;

procedure TKCustomQRBarcode.LockFontChanged( Value: Boolean );
begin
	FFontChangedLocked := Value;
end;

procedure TKCustomQRBarcode.AdjustFontPainting( Sender: TObject );
begin
	LockFontChanged( true );
	try
		Font.Assign( FBarcode.Font );
	finally
		LockFontChanged( false );
	end;
end;

procedure TKCustomQRBarcode.FontChanged( Sender: TObject );
begin
	if CheckObject( FBarcode ) and ( not FFontChangedLocked ) then
		FBarcode.Font := Font;
end;

function TKCustomQRBarcode.GetAddOnCaption: TKAddOnCaption;
begin
	Result := FBarcode.AddOnCaption;
end;

function TKCustomQRBarcode.GetAddOnString: String;
begin
	Result := FBarcode.AddOnString;
end;

function TKCustomQRBarcode.GetAddOnStyle: TKAddOnStyle;
begin
	Result := FBarcode.AddOnStyle;
end;

function TKCustomQRBarcode.GetAlign: TAlign;
begin
	Result := FBarcode.Align;
end;

function TKCustomQRBarcode.GetAlignment: TAlignment;
begin
	Result := FBarcode.Alignment;
end;

{
function TKCustomQRBarcode.GetAngle: TKAngle;
begin
	Result := FBarcode.Angle;
end;
}

function TKCustomQRBarcode.GetAutosize: Boolean;
begin
	Result := FBarcode.Autosize;
end;

function TKCustomQRBarcode.GetBarCaption: TKBarCaption;
begin
	Result := FBarcode.BarCaption;
end;

function TKCustomQRBarcode.GetBarcode: String;
begin
  Result := FBarcode.Barcode;
end;

function TKCustomQRBarcode.GetBarColor: TColor;
begin
	Result := FBarcode.BarColor;
end;

function TKCustomQRBarcode.GetBarCrackColor: TColor;
begin
	Result := FBarcode.BarCrackColor;
end;

function TKCustomQRBarcode.GetBarDigit: TKBarDigit;
begin
	Result := FBarcode.BarDigit;
end;

function TKCustomQRBarcode.GetBarKind: TKBarKind;
begin
	Result := FBarcode.BarKind;
end;

function TKCustomQRBarcode.GetBarString: String;
begin
	Result := FBarcode.BarString;
end;

function TKCustomQRBarcode.GetBarWidth: Byte;
begin
	Result := FBarcode.BarWidth;
end;

function TKCustomQRBarcode.GetFont: TFont;
begin
	Result := FBarcode.Font;
end;

function TKCustomQRBarcode.GetHeight: Integer;
begin
	Result := inherited Height;
end;

function TKCustomQRBarcode.GetPaddingStyle: TKPaddingStyle;
begin
	Result := FBarcode.PaddingStyle;
end;

function TKCustomQRBarcode.GetShowCaption: Boolean;
begin
	Result := FBarcode.ShowCaption;
end;

function TKCustomQRBarcode.GetStretched: Boolean;
begin
	Result := FBarcode.Stretched;
end;

function TKCustomQRBarcode.GetUseCheckDigit: Boolean;
begin
	Result := FBarcode.UseCheckDigit;
end;

function TKCustomQRBarcode.GetThickBarRatio: TKThickBar;
begin
	Result := FBarcode.ThickBarRatio;
end;

function TKCustomQRBarcode.GetTransparent: Boolean;
begin
	Result := FBarcode.Transparent;
end;

function TKCustomQRBarcode.GetWidth: Integer;
begin
	Result := inherited Width;
end;

procedure TKCustomQRBarcode.SetAddOnStyle( Value: TKAddOnStyle );
begin
	FBarcode.AddOnStyle := Value;
end;

procedure TKCustomQRBarcode.SetAddOnCaption( const Value: TKAddOnCaption );
begin
	FBarcode.AddOnCaption := Value;
end;

procedure TKCustomQRBarcode.SetAlign( Value: TAlign );
begin
	FBarcode.Align := Value;
end;

procedure TKCustomQRBarcode.SetAlignment( Value: TAlignment );
begin
	FBarcode.Alignment := Value;
end;

{
procedure TKCustomQRBarcode.SetAngle( Value: TKAngle );
begin
	FBarcode.Angle := Value;
end;
}

procedure TKCustomQRBarcode.SetAutosize( Value: Boolean );
begin
	FBarcode.AutoSize := Value;
end;

procedure TKCustomQRBarcode.SetBarCaption( Value: TKBarCaption );
begin
	FBarcode.BarCaption := Value;
end;

procedure TKCustomQRBarcode.SetBarColor( Value: TColor );
begin
	FBarcode.BarColor := Value;
end;

procedure TKCustomQRBarcode.SetBarCrackColor( Value: TColor );
begin
	FBarcode.BarCrackColor := Value;
end;

procedure TKCustomQRBarcode.SetBarDigit( Value: TKBarDigit );
begin
end;

procedure TKCustomQRBarcode.SetBarKind( Value: TKBarKind );
begin
	FBarcode.BarKind := Value;
end;

procedure TKCustomQRBarcode.SetAddOnString( const Value: String );
begin
end;

procedure TKCustomQRBarcode.SetBarString( const Value: String );
begin
end;

procedure TKCustomQRBarcode.SetBarWidth( Value: Byte );
begin
	FBarcode.BarWidth := Value;
end;

procedure TKCustomQRBarcode.SetFont( Value: TFont );
begin
	inherited Font.Assign( Value );
	FBarcode.Font := Value;
end;

procedure TKCustomQRBarcode.SetHeight( Value: Integer );
begin
	FBarcode.SetPropChanged( true );
	inherited Height := Value;
end;

procedure TKCustomQRBarcode.SetWidth( Value: Integer );
begin
	FBarcode.SetPropChanged( true );
	inherited Width := Value;
end;

procedure TKCustomQRBarcode.SetPaddingStyle( Value: TKPaddingStyle );
begin
	FBarcode.PaddingStyle := Value;
end;

procedure TKCustomQRBarcode.SetShowCaption( Value: Boolean );
begin
	FBarcode.ShowCaption := Value;
end;

procedure TKCustomQRBarcode.SetUseCheckDigit( Value: Boolean );
begin
	FBarcode.UseCheckDigit := Value;
end;

procedure TKCustomQRBarcode.SetThickBarRatio( Value: TKThickBar );
begin
	FBarcode.ThickBarRatio := Value;
end;

procedure TKCustomQRBarcode.SetStretched( Value: Boolean );
begin
	FBarCode.Stretched := Value;
end;

procedure TKCustomQRBarcode.SetTransparent( Value: Boolean );
begin
	FBarcode.Transparent := Value;
end;

end.
