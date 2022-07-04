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

unit ukbcClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	SysUtils, Windows, Classes, Graphics, Controls, uksyUtils;

type

	EKBCClasses = class( EKBarCode );

	TGraphicControlClass = class( TGraphicControl );

	TKThickBar = 2..3;
	TKBarDigit = type Byte;
	TKBarCaption = type string;
	TKAddOnCaption = string[5];

	TKPaintMode = ( pmBarcode, pmAddOn );
	TKAddOnStyle = ( asNone, asEAN2, asEAN5 );
	TKPaddingStyle = ( psLeading, psTrailing );

	TKBarKind = ( bkCodabar, bkCode128A, bkCode128B, bkCode128C,
		bkCode25, bkCode25i, bkCode39, bkCode39E, bkEAN13, bkEAN8,
		bkISBN, bkMSI, bkPlessey, bkUPCA, bkUPCE );

	TKGetPaintCanvasEvent = procedure( Sender: TObject; var Canvas: TCanvas ) of object;

{ TKCustomBarcodeComponent }

	TKCustomBarcodeComponent = class( TComponent )
	private
		FFont: TFont;
  { FAngle: TKAngle; }
		FBarWidth: Byte;
		FBarcode: string;
		FBarColor: TColor;
		FBarString: string;
		FAutosize: Boolean;
		FStretched: Boolean;
		FBarKind: TKBarKind;
		FBarDigit: TKBarDigit;
		FShowCaption: Boolean;
		FTransparent: Boolean;
		FAlignment: TAlignment;
		FBarCrackColor: TColor;
		FUseCheckDigit: Boolean;
		FBarCaption: TKBarCaption;
		FAddOnStyle: TKAddOnStyle;
		FThickBarRatio: TKThickBar;
		FAddOnCaption: TKAddOnCaption;
		FPaddingStyle: TKPaddingStyle;

		bmpAND: TBitmap;
		FOwner: TControl;
		FBitmap: TBitmap;
		FLoaded: Boolean;
		bmpAddOn: TBitmap;
		FPainting: Boolean;
		FTextInfo: TStrings;
		FPaintCount: Integer;
		bmpAddOnAND: TBitmap;
		FAddOnString: string;
		FAddOnBarcode: string;
		FPropChanged: Boolean;
		FAddOnTextInfo: TStrings;
		FAdjustFontPainting: TNotifyEvent;
		FGetPaintCanvas: TKGetPaintCanvasEvent;

		procedure BuildAddOn;
		procedure PropChanged;
		procedure ClearTextInfo;
		procedure NormalizeAddOn;
		procedure NormalizeCaption;
		procedure ClearAddOnTextInfo;
		procedure NormalizeBarString;
  {	procedure BuildBmpAngled( bmp: TBitmap ); }

		function GetCanvas: TCanvas;

		procedure GetMSIString;
		procedure GetEAN8String;
		procedure GetISBNString;
		procedure GetUPCAString;
		procedure GetUPCEString;
		procedure GetCode25String;
		procedure GetCode39String;
		procedure GetPlesseyString;
		procedure GetCodabarString;
		procedure GetCode25iString;
		procedure GetCode39eString;
		procedure GetCode128AString;
		procedure GetCode128BString;
		procedure GetCode128CString;
		procedure GetEAN13String( IsISBN: Boolean );

		function GetAlign: TAlign;

		procedure SetFont( Value: TFont );
		procedure SetAlign( Value: TAlign );
	{	procedure SetAngle( Value: TKAngle ); }
		procedure SetBarWidth( Value: Byte );
		procedure SetBarColor( Value: TColor );
		procedure SetAutosize( Value: Boolean );
		procedure SetBarKind( Value: TKBarKind );
		procedure SetStretched( Value: Boolean );
		procedure SetTransparent( Value: Boolean );
		procedure SetShowCaption( Value: Boolean );
		procedure SetBarDigit( Value: TKBarDigit );
		procedure SetAlignment( Value: TAlignment );
		procedure SetBarCrackColor( Value: TColor );
		procedure SetUseCheckDigit( Value: Boolean );
		procedure SetBarString( const Value: string );
		procedure SetAddOnString( const Value: string );
		procedure SetAddOnStyle( Value: TKAddOnStyle );
		procedure SetThickBarRatio( Value: TKThickBar );
		procedure SetPaddingStyle( Value: TKPaddingStyle );
		procedure SetBarCaption( const Value: TKBarCaption );
		procedure SetAddOnCaption( const Value: TKAddOnCaption );

	protected
		procedure Loaded; override;
		procedure Invalidate; virtual;
		procedure CanvasDraw( x, y: Integer ); virtual;
		procedure CanvasStretchDraw( IsAddOn: Boolean ); virtual;
		procedure TextDraw( IsAddOn: Boolean; x, y: Integer; const Text: string );
		procedure ColumnDraw( IsAddOn: Boolean; var Col: Integer; Y, AHeight, Mode: Integer ); virtual;

		property OwnerCanvas: TCanvas
							 read GetCanvas;
		property Owner: TControl
						 read FOwner;
		
	public
		destructor Destroy; override;
		constructor CreateLinked( AOwner: TControl ); virtual;

		procedure Paint; virtual;
		procedure SetPropChanged( Value: Boolean );
		procedure FontChanged( Sender: TObject ); virtual;

		property AddOnCaption: TKAddOnCaption
						 read FAddOnCaption write SetAddOnCaption;
		property AddOnStyle: TKAddOnStyle
						 read FAddOnStyle write SetAddOnStyle default asNone;
		property AddOnString: string
						 read FAddOnString write SetAddOnString;
		property Align: TAlign
						 read GetAlign write SetAlign;
		property Alignment: TAlignment
						 read FAlignment write SetAlignment default taLeftJustify;
	{ property Angle: TKAngle
						 read FAngle write SetAngle; }
		property Autosize: Boolean
						 read FAutosize write SetAutosize default true;
		property BarCaption: TKBarCaption
						 read FBarCaption write SetBarCaption;
		property Barcode: string
						 read FBarcode;
		property BarColor: TColor
						 read FBarColor  write SetBarColor default clWindowText;
		property BarCrackColor: TColor
						 read FBarCrackColor write SetBarCrackColor default clWindow;
		property BarDigit: TKBarDigit
						 read FBarDigit write SetBarDigit;
		property BarKind: TKBarKind
						 read FBarKind write SetBarKind default bkEAN13;
		property BarString: string
						 read FBarString write SetBarString;
		property BarWidth: Byte
						 read FBarWidth write SetBarWidth default 2;
		property Font: TFont
						 read FFont write SetFont;
		property PaddingStyle: TKPaddingStyle
						 read FPaddingStyle write SetPaddingStyle default psLeading;
		property ShowCaption: Boolean
						 read FShowCaption write SetShowCaption default true;
		property UseCheckDigit: Boolean
						 read FUseCheckDigit write SetUseCheckDigit default false;
		property Stretched: Boolean
						 read FStretched write SetStretched default false;
		property ThickBarRatio: TKThickBar
						 read FThickBarRatio write SetThickBarRatio default 3;
		property Transparent: Boolean
						 read FTransparent write SetTransparent default true;
		property OnAdjustFontPainting: TNotifyEvent
						 read FAdjustFontPainting write FAdjustFontPainting;
		property OnGetPaintCanvas: TKGetPaintCanvasEvent
						 read FGetPaintCanvas write FGetPaintCanvas;

	end;

implementation

uses
	ukbcConsts, ukbcTypes, ukbcResStr, ukbcUtils;

{

	Architecture Issues:
	--------------------

	. need to implement a smart property editor for the caption
		property: it must identify valid chars for every type of barcode;
	. need to create the following versions:
			DB, QR, QRDB, and Printer;
	. need to provide support for rotation to different angles;


	Comments:
	---------

	. print the checkdigit just for EAN and UPC codes; other codes
		have the checkdigit included in the barcode representation,
		but omit the string representation on the radable caption of
		the barcode...

}

procedure PadString( var Source: string; Size: Integer; Mode: TKPaddingStyle );
begin
	while ( Length( Source ) < Size ) do
		if ( Mode = psLeading ) then
			Source := '0' + Source
		else
			Source := Source + '0';
end;

function IsDigit( ch: Char ): Boolean;
begin
	Result := ( ch in ['0'..'9'] );
end;

function MakeStringEx( iNum: Integer ): string;
var
	i: Integer;
begin
	Result := '';
	for i := 1 to 32 do
		Result := IntToStr( ( iNum shr ( i - 1 ) ) and 1 ) + Result;
	while ( Result[1] = '0' ) do
		Delete( Result, 1, 1 );
end;

function MakeString( iNum: Integer ): string;
var
	i: Integer;
begin
	Result := '';
	for i := 1 to 8 do
		Result := IntToStr( ( iNum shr ( i - 1 ) ) and 1 ) + Result;
end;

{
-------------------------------------------------------------------------
---------------------- Converting Index to Symbol -----------------------
-------------------------------------------------------------------------
}

{$HINTS OFF}
function IndexToSymbolEAN( Index: Byte ): string;
begin
	if ( Index > ( EANCharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := EANTable[Index];
end;

function IndexToSymbolMSI( Index: Byte ): string;
begin
	if ( Index > ( MSICharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := MSITable[Index];
end;

function IndexToSymbolCode25( Index: Byte ): string;
begin
	if ( Index > ( Code25CharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code25Table[Index];
end;

function IndexToSymbolCode25i( Index: Byte ): string;
begin
	if ( Index > ( Code25iCharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code25iTable[Index];
end;

function IndexToSymbolCode39( Index: Byte ): string;
begin
	if ( Index > ( Code39CharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code39Table[Index];
end;

function IndexToSymbolCode39e( Index: Byte ): string;
begin
	if ( Index > ( Code39eCharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code39eTable[Index];
end;

function IndexToSymbolCodabar( Index: Byte ): string;
begin
	if ( Index > ( CodabarCharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := CodabarTable[Index];
end;

function IndexToSymbolCode128_A( Index: Byte ): string;
begin
	if ( Index > ( Code128CharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code128ATable[Index];
end;

function IndexToSymbolCode128_B( Index: Byte ): string;
begin
	if ( Index > ( Code128CharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code128BTable[Index];
end;

function IndexToSymbolCode128_C( Index: Byte ): string;
begin
	if ( Index > ( Code128CharCount - 1 ) ) then
		RaiseException( EKBCClasses, sErrBCInvCharConv );
	Result := Code128CTable[Index];
end;

{
-------------------------------------------------------------------------
---------------------- Converting Symbol to Index -----------------------
-------------------------------------------------------------------------
}

function SymbolToIndexEAN( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to EANCharCount - 1 do
		if ( EANTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexMSI( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to MSICharCount - 1 do
		if ( MSITable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode25( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Code25CharCount - 1 do
		if ( Code25Table[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode25i( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Code25iCharCount - 1 do
		if ( Code25iTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode39( const Code: string ): Byte;
var
	i: Integer;
begin
  Result := 0;
	for i := 0 to Code39CharCount - 1 do
		if ( Code39Table[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode39e( const Code: string ): Byte;
var
	i: Integer;
begin
  Result := 0;
	for i := 0 to Code39eCharCount - 1 do
		if ( Code39eTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCodabar( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to CodabarCharCount - 1 do
		if ( CodabarTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode128A( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Code128CharCount - 1 do
		if ( Code128ATable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode128B( const Code: string ): Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Code128CharCount - 1 do
		if ( Code128BTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

function SymbolToIndexCode128C( const Code: string ): Byte;
var
	i: Integer;
begin
  Result := 0;
	for i := 0 to Code128CharCount - 1 do
		if ( Code128CTable[i] = Code ) then
		begin
			Result := i;
			Exit;
		end;
	RaiseException( EKBCClasses, sErrBCInvCharConv );
end;

{
-------------------------------------------------------------------------
----------------------- Converting Index to Code ------------------------
-------------------------------------------------------------------------
}

function SymbolToEAN_A( const Symbol: string ): string;
begin
	Result := EANEncodingA[SymbolToIndexEAN( Symbol )];
end;

function SymbolToEAN_B( const Symbol: string ): string;
begin
	Result := EANEncodingB[SymbolToIndexEAN( Symbol )];
end;

function SymbolToEAN_C( const Symbol: string ): string;
begin
	Result := EANEncodingC[SymbolToIndexEAN( Symbol )];
end;

function SymbolToMSI( const Symbol: string ): string;
begin
	Result := MSIEncoding[SymbolToIndexMSI( Symbol )];
end;

function SymbolToCode25( const Symbol: string ): string;
begin
	Result := Code25Encoding[SymbolToIndexCode25( Symbol )];
end;

function SymbolToCode25i( const Symbol: string ): string;
begin
	Result := Code25iEncoding[SymbolToIndexCode25i( Symbol )];
end;

function SymbolToCode39( const Symbol: string ): string;
begin
	Result := Code39Encoding[SymbolToIndexCode39( Symbol )];
end;

function SymbolToCode39e( const Symbol: string ): string;
begin
	Result := Code39eEncoding[SymbolToIndexCode39e( Symbol )];
end;

function SymbolToCodabar( const Symbol: string ): string;
begin
	Result := CodabarEncoding[SymbolToIndexCodabar( Symbol )];
end;

function SymbolToCode128_A( const Symbol: string ): string;
begin
	Result := Code128Encoding[SymbolToIndexCode128A( Symbol )];
end;

function SymbolToCode128_B( const Symbol: string ): string;
begin
	Result := Code128Encoding[SymbolToIndexCode128B( Symbol )];
end;

function SymbolToCode128_C( const Symbol: string ): string;
begin
	Result := Code128Encoding[SymbolToIndexCode128C( Symbol )];
end;
{$HINTS OFF}

{
-------------------------------------------------------------------------
------------------------- CheckDigit Functions --------------------------
-------------------------------------------------------------------------
}

function BarcodeCheckDigitPlessey( const Code: string; var Bars: string ): Byte;

	function NormalizePlessey: string;
	var
		i: Integer;
	begin
		Result := '';
		for i := 1 to Length( Code ) do
			Result := Result + PlesseyEncoding[StrToInt( Code[i] )];
	end;

	function MakeInt( const sNum: string ): Integer;
	var
		i,
		iLen: Integer;
	begin
		Result := 0;
		iLen := Length( sNum );
		for i := 1 to iLen do
			Result := Result or ( StrToInt( sNum[i] ) shl ( iLen - i ) );
	end;

const
	Generator: Integer =  489; // x^8 + x^7 + x^6 + x^5 + x^3 + 1;
var
	s,
	sDiv: string;
	i,
	iDiv: Integer;
begin
	Result := 0;
	sDiv := '';
	Bars := '';
	s := NormalizePlessey;
{ shl 8 on the entire string }
	for i := 1 to 8 do
		s := s + '0';
	if ( Length( s ) < 9 ) then
		Exit;
	iDiv := MakeInt( Copy( s, 1, 9 ) );
	Delete( s, 1, 9 );
	while ( Length( s ) > 0 ) do
	begin
		if ( Length( MakeStringEx( iDiv ) ) >= 9 ) then
			iDiv := iDiv xor Generator;
		iDiv := MakeInt( MakeStringEx( iDiv ) + s[1] );
		Delete( s, 1, 1 );
	end;
	Result := iDiv xor Generator;
	sDiv := MakeString( Result );
	for i := 1 to Length( sDiv ) do
		if ( sDiv[i] = '0' ) then
			Bars := Bars + '12'
		else
			Bars := Bars + '30';
end;

function BarcodeCheckDigitMSI( const Code: string ): Byte;
{ Tricky modulus-10 checkdigit algorithm }
var
	i,
	iCheck: Integer;
	s: string;
begin
	s := '';
	iCheck := 0;
	Result := 0;
{ let 'iCheck' be sum the even-positioned symbols, and accumulate the
	odd-positioned symbols in 's' }
	for i := Length( Code ) downto 1 do
		if ( ( i and 1 ) = 1 ) then
			s := Code[i] + s
		else
			iCheck := iCheck + ( Ord( Code[i] ) - 48 );
	if ( s <> '' ) then
	begin
{ let 's' be the the value 2*( s ) }
		s := IntToStr( 2 * StrToInt( s ) );
{ update 'iCheck' by adding all the digits in 's' to it }
		for i := 1 to Length( s ) do
			iCheck := iCheck + ( Ord( Code[i] ) - 48 );
{ calculate the Modulus-10 of 'iCheck' }
		Result := ( iCheck mod 10 );
		if ( Result <> 0 ) then
			Result := 10 - Result;
	end;
end;

function BarcodeCheckDigitEAN( const Code: string ): Byte;
{ Tricky modulus-10 checkdigit algorithm:
{ For any UPC, EAN, or Code25i checkdigit calculation, the digits
	are numbered from 1 to n, where 1 is the ordinal number of the
	checkdigit; since this function calculates Code's checkdigit,
	it will loop through all the symbols in Code, starting at digit 2,
	up to digit n, sum then even and odd digits separately, and
	perform the	proper calculation }
var
	i,
	iOdd,
	iEven: Integer;
begin
	iOdd := 0;
	iEven := 0;
	for i := Length( Code ) downto 1 do
		if Boolean( ( Length( Code ) - i + 2 ) and 1 ) then
			inc( iOdd, SymbolToIndexEAN( Code[i] ) )
		else
			inc( iEven, SymbolToIndexEAN( Code[i] ) );
	Result := ( ( iOdd + 3 * iEven ) mod 10 );
	if ( Result <> 0 ) then
		Result := 10 - Result;
end;

function BarcodeCheckDigitCode128_A( const Code: string ): Byte;
{ Modulus-103 checkdigit algorithm }
var
	i,
	iSum: Integer;
begin
	iSum := 103;
	for i := 1 to Length( Code ) do
		iSum := iSum + i * SymbolToIndexCode128A( Code[i] );
	Result := iSum mod 103;
end;

function BarcodeCheckDigitCode128_B( const Code: string ): Byte;
{ Modulus-103 checkdigit algorithm }
var
	i,
	iSum: Integer;
begin
	iSum := 104;
	for i := 1 to Length( Code ) do
		iSum := iSum + i * SymbolToIndexCode128B( Code[i] );
	Result := iSum mod 103;
end;

function BarcodeCheckDigitCode128_C( const Code: string ): Byte;
{ Tricky modulus-103 checkdigit algorithm:
	The elements have to be taken from the Code string, 2 at a time
	(for digit characters), or one at a time (for control characters) }
var
	i,
	j,
	iPos,
	iSum: Integer;
	s,
	s1: string;
begin
	i := 1;
	s1 := '';
	iPos := 1;
	iSum := 105;
{ guarantee that the last block will always be normalized ;) }
	s := Code + #255;
{ are we finished yet with s ? }
	while ( s <> '' )  do
	begin
		if ( not IsDigit( s[i] ) ) then
		begin
			if ( i > 1 ) then
			begin
				s1 := Copy( s, 1, i - 1 );
				s := Copy( s, i, Length( s ) );
				for j := 1 to ( Length( s1 ) div 2 ) do
				begin
					iSum := iSum + iPos * SymbolToIndexCode128C( s1[j * 2 - 1] + s1[j * 2] );
					inc( iPos );
				end;
				i := 1;
			end
			else
			begin
				if ( s[i] <> #255 ) then
				begin
					iSum := iSum + iPos * SymbolToIndexCode128C( s[i] );
					inc( iPos );
				end;
				Delete( s, i, 1 );
			end;
		end
		else
			inc( i );
	end;
	Result := iSum mod 103;
end;

{
-------------------------------------------------------------------------
------------------------- TKCustomBarcodeComponent Class -------------------------
-------------------------------------------------------------------------
}

constructor TKCustomBarcodeComponent.CreateLinked( AOwner: TControl );
begin
	inherited Create( AOwner );
{ Control flags }
	FLoaded := false;
	FPaintCount := 0;
	FPainting := false;
	FBarcode := '';
	FBarWidth := 2;
	FBarString := '';
{ AddOn properties }
	FAddOnString := '';
	FAddOnCaption := '';
	FAddOnStyle := asNone;
{ Default values for properties }
	FOwner := AOwner;
	FAutosize := true;
	FStretched := false;
	FBarKind := bkEAN13;
	FThickBarRatio := 3;
	FShowCaption := true;
	FTransparent := true;
	FUseCheckDigit := false;
	FBarColor := clWindowText;
	FBarCrackColor := clWindow;
	FAlignment := taLeftJustify;
{ object creation }
	FFont := TFont.Create;
	bmpAND := TBitmap.Create;
	bmpAddOn := TBitmap.Create;
	bmpAddOnAND := TBitmap.Create;
	FTextInfo := TStringList.Create;
	FAddOnTextInfo := TStringList.Create;
	FBitmap := Graphics.TBitmap.Create;
	bmpAND.Canvas.Brush.Color := clWhite;
	with FFont do
	begin
		Name := 'Arial';
		Size := 10;
		Style := [fsBold];
		OnChange := FontChanged;
	end;
	Owner.SetBounds( Owner.Left, Owner.Top, 100, 100 );
	BarCaption := '123456789012';
	OwnerCanvas.Brush.Style := bsClear;
end;

destructor TKCustomBarcodeComponent.Destroy;
begin
	FFont.Free;
	bmpAND.Free;
	FBitmap.Free;
	bmpAddOn.Free;
	bmpAddOnAND.Free;
	ClearTextInfo;
	FTextInfo.Free;
	ClearAddOnTextInfo;
	FAddOnTextInfo.Free;
	inherited Destroy;
end;

type
  TCustomControlHack = class( TCustomControl );

function TKCustomBarcodeComponent.GetCanvas: TCanvas;
begin
	Result := nil;
	if Assigned( FGetPaintCanvas ) then     
		FGetPaintCanvas( Self, Result );
{ just to make sure we really have a canvas to paint on!!! }
	if ( not CheckObject( Result ) ) then
		if CheckObjectClass( Owner, TGraphicControl ) then
			Result := TGraphicControlClass( Owner ).Canvas
		else if CheckObjectClass( Owner, TCustomControl ) then
			Result := TCustomControlHack( Owner ).Canvas;
end;

procedure TKCustomBarcodeComponent.Loaded;
begin
	inherited Loaded;
	FLoaded := true;
	try
		SetBarCaption( FBarCaption );
	finally
		FLoaded := false;
	end;
end;

procedure TKCustomBarcodeComponent.Invalidate;
begin
	Owner.Invalidate;
end;

{
-------------------------------------------------------------------------
------------------ ISBN, EAN and UPC Support Functions ------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.BuildAddOn;

	function GetX: Integer;
	var
		i,
		iOdd,
		iEven,
		iSum: Integer;
	begin
		iOdd := 0;
		iEven := 0;
		for i := 1 to 5 do
			if ( ( i and 1 ) = 1 ) then
				iOdd := iOdd + SymbolToIndexEAN( FAddOnString[i] )
			else
				iEven := iEven + SymbolToIndexEAN( FAddOnString[i] );
		iSum := 3 * iOdd + 9 * iEven;
		Result := iSum mod 10;
	end;

var
  i,
	Index: Integer;
	lut: TKEANCodes;
	tb: array [tbA..tbB] of TKEANCodes;
begin
	tb[tbA] := EANEncodingA;
	tb[tbB] := EANEncodingB;
	if ( FAddOnStyle = asNone ) then
	  Exit;
	FAddOnString := FAddOnCaption;
{ 9 spaces should separate ISBN barcode from addon barcode }
	FAddOnBarcode := SAddSeparator + SAddOnGuard;
	if ( FAddOnStyle = asEAN2 ) then
	begin
		PadString( FAddOnString, 2, FPaddingStyle );
		Index := StrToInt( FAddOnString );
		for i := 1 to 2 do
		begin
			lut := tb[EAN2LookupTable[Index, i]];
			FAddOnBarcode := FAddOnBarcode + lut[SymbolToIndexEAN( FAddOnString[i] )];
			if ( i < 2 ) then
				FAddOnBarcode := FAddOnBarcode + SAddOnDelineator;
		end;
	end
	else
	begin
		PadString( FAddOnString, 5, FPaddingStyle );
		Index := GetX;
		for i := 1 to 5 do
		begin
			lut := tb[EAN5LookupTable[Index, i]];
			FAddOnBarcode := FAddOnBarcode + lut[SymbolToIndexEAN( FAddOnString[i] )];
			if ( i < 5 ) then
				FAddOnBarcode := FAddOnBarcode + SAddOnDelineator;
		end;
	end;
end;

procedure TKCustomBarcodeComponent.GetISBNString;
begin
	FBarString := FBarCaption;
{ Is padding necessary ? }
	PadString( FBarString, 10, FPaddingStyle );
	FBarString := '978' + Copy( FBarString, 1, 9 );
	GetEAN13String( true );
{ Build the addon barcode- if necessary }
	BuildAddOn;
end;

procedure TKCustomBarcodeComponent.GetEAN8String;
var
	i: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
{ Is padding necessary ? }
	PadString( FBarString, 7, FPaddingStyle );
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
{ Add the next 28 bytes of the barcode string }
	for i := 1 to 4 do
		FBarcode := FBarcode + SymbolToEAN_A( FBarString[i] );
{ Add the central (5 bytes) }
	FBarcode := FBarcode + SEANCentral;
{ Add the next 21 bytes of the barcode string }
	for i := 5 to 7 do
		FBarcode := FBarcode + SymbolToEAN_C( FBarString[i] );
	FBarDigit := BarcodeCheckDigitEAN( FBarString );
{ Add the checkdigit }
	FBarString := FBarString + IndexToSymbolEAN( FBarDigit );
	FBarcode := FBarcode + EANEncodingC[FBarDigit];
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
end;

procedure TKCustomBarcodeComponent.GetEAN13String( IsISBN: Boolean );
var
	i,
	iTableLookup: Integer;
	lut: TKEANCodes;
	tb: array [tbA..tbB] of TKEANCodes;
begin
	FBarcode := '';
	tb[tbA] := EANEncodingA;
	tb[tbB] := EANEncodingB;
	if ( not IsISBN ) then
	begin
		FBarString := FBarCaption;
{ Is padding necessary ? }
		PadString( FBarString, 12, FPaddingStyle );
	end;
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
{ the first data digit will be used for the lookup }
	iTableLookup := SymbolToIndexEAN( FBarString[1] );
{ Add the next 42 bytes of the barcode string }
	for i := 2 to 7 do
	begin
{ get the lookup table for the i-th digit }
		lut := tb[EANLookupTable[iTableLookup, i]];
{ use the lookup table to Get the next part of the barcode string }
		FBarcode := FBarcode + lut[SymbolToIndexEAN( FBarString[i] )];
	end;
{ Add the central (5 bytes) }
	FBarcode := FBarcode + SEANCentral;
{ Add the next 35 bytes of the barcode string }
	for i := 8 to 12 do
		FBarcode := FBarcode + SymbolToEAN_C( FBarString[i] );
	FBarDigit := BarcodeCheckDigitEAN( FBarString );
{ Add the checkdigit }
	FBarString := FBarString + IndexToSymbolEAN( FBarDigit );
	FBarcode := FBarcode + EANEncodingC[FBarDigit];
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
{ Build the addon barcode- if necessary }
	BuildAddOn;
end;

procedure TKCustomBarcodeComponent.GetUPCAString;
var
	i: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
{ Is padding necessary ? }
	PadString( FBarString, 11, FPaddingStyle );
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
{ Add the next 42 bytes of the barcode string }
	for i := 1 to 6 do
		FBarcode := FBarcode + SymbolToEAN_A( FBarString[i] );
{ Add the central (5 bytes) }
	FBarcode := FBarcode + SEANCentral;
{ Add the next 35 bytes of the barcode string }
	for i := 7 to 11 do
		FBarcode := FBarcode + SymbolToEAN_C( FBarString[i] );
	FBarDigit := BarcodeCheckDigitEAN( FBarString );
{ Add the checkdigit }
	FBarString := FBarString + IndexToSymbolEAN( FBarDigit );
	FBarcode := FBarcode + EANEncodingC[FBarDigit];
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SEANGuard;
{ Build the addon barcode- if necessary }
	BuildAddOn;
end;

procedure TKCustomBarcodeComponent.GetUPCEString;

	procedure NormalizeUPCEString;
	var
		sMan: string[5];
		sManLast2: string[2];
		sManLast3: string[3];
	begin
		sMan := Copy( FBarString, 1, 5 );
		sManLast2 := Copy( sMan, 4, 2 );
		sManLast3 := Copy( sMan, 3, 3 );
		if ( sManLast3 = '000' ) or ( sManLast3 = '100' ) or
			 ( sManLast3 = '200' ) then
			FBarString := Copy( sMan, 1, 2 ) + Copy( FBarString, 8, 3 ) + sMan[3]
		else if ( sManLast3 = '300' ) or ( sManLast3 = '400' ) or
						( sManLast3 = '500' ) or ( sManLast3 = '600' ) or
						( sManLast3 = '700' ) or ( sManLast3 = '800' ) or
						( sManLast3 = '900' ) then
			FBarString := Copy( sMan, 1, 3 ) + Copy( FBarString, 9, 2 ) + '3'
		else if ( sManLast2 = '10' ) or ( sManLast2 = '20' ) or
						( sManLast2 = '30' ) or ( sManLast2 = '40' ) or
						( sManLast2 = '50' ) or ( sManLast2 = '60' ) or
						( sManLast2 = '70' ) or ( sManLast2 = '80' ) or
						( sManLast2 = '90' ) then
			FBarString := Copy( sMan, 1, 4 ) + FBarString[10] + '4'
		else
			FBarString := sMan + FBarString[10];
	end;

var
	i: Integer;
	lut: TKEANCodes;
	tb: array [tbA..tbB] of TKEANCodes;
begin
	FBarcode := '';
	tb[tbA] := EANEncodingA;
	tb[tbB] := EANEncodingB;
	FBarString := FBarCaption;
{ Is padding necessary ? }
	PadString( FBarString, 10, FPaddingStyle );
{ the checkdigit must be calculated using the full 10 symbols }
	FBarDigit := BarcodeCheckDigitEAN( FBarString );
{ now, calculate the zero-supressed UPCE string }
	NormalizeUPCEString;
{ Add the guard }
	FBarcode := FBarcode + SUPCELeftGuard;
{ Add the next ?? bytes of the barcode string }
	for i := 1 to 6 do
	begin
{ get the lookup table for the i-th digit }
		lut := tb[UPCELookupTable[FBarDigit, i]];
{ use the lookup table to Get the next part of the barcode string }
		FBarcode := FBarcode + lut[SymbolToIndexEAN( FBarString[i] )];
	end;
{ Do not add the checkdigit }
//	FBarString := FBarString + IndexToSymbolEAN( FBarDigit );
//	FBarcode := FBarcode + EANEncodingC[FBarDigit];
{ Add the guard (3 bytes) }
	FBarcode := FBarcode + SUPCERightGuard;
end;

{
-------------------------------------------------------------------------
------------------ Code39 and Code39e Support Functions -----------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.GetCode39String;
var
	i,
	iLen,
	iSum,
	iDigit: Integer;
begin
	FBarcode := '';
	iSum := 0;
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Add the guard }
	FBarcode := FBarcode + SCode39GuardIndex;
{ Add extra space }
	FBarcode := FBarcode + '0';
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
	begin
		iDigit := SymbolToIndexCode39( FBarString[i] );
		FBarcode := FBarcode + Code39Encoding[iDigit];
		iSum := iSum + iDigit;
{ Add extra space }
		FBarcode := FBarcode + '0';
	end;
{ Add the checkdigit if necessary }
	if FUSeCheckDigit then
	begin
		FBarDigit := iSum mod 43;
		FBarString := FBarString + IndexToSymbolCode39( FBarDigit );
		FBarcode := FBarcode + Code39Encoding[FBarDigit];
{ Add extra space }
		FBarcode := FBarcode + '0';
	end
	else
		FBarDigit := 0;
{ Add the guard }
	FBarcode := FBarcode + SCode39GuardIndex;
end;

procedure TKCustomBarcodeComponent.GetCode39eString;
var
	i,
	iLen,
	iSum,
	iDigit: Integer;
begin
	FBarcode := '';
	iSum := 0;
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Add the original Code39 guard }
	FBarcode := FBarcode + SCode39GuardIndex;
{ Add extra space }
	FBarcode := FBarcode + '0';
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
	begin
		iDigit := SymbolToIndexCode39e( FBarString[i] );
		FBarcode := FBarcode + Code39eEncoding[iDigit];
		iSum := iSum + iDigit;
{ Add extra space }
		FBarcode := FBarcode + '0';
	end;
{ Add the checkdigit if necessary }
	if FUseCheckDigit then
	begin
		FBarDigit := iSum mod 128;
		FBarString := FBarString + IndexToSymbolCode39e( FBarDigit );
		FBarcode := FBarcode + Code39eEncoding[FBarDigit];
{ Add extra space }
		FBarcode := FBarcode + '0';
	end
	else
		FBarDigit := 0;
{ Add the original Code39 guard }
	FBarcode := FBarcode + SCode39GuardIndex;
end;

{
-------------------------------------------------------------------------
------------------- Plessey and MSI Support Functions -------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.GetPlesseyString;
var
	i,
	iLen: Integer;
	sCode: string;
begin
  sCode := '';
	FBarcode := '';
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
		FBarcode := FBarcode + SymbolToMSI( FBarString[i] );
	if ( FUseCheckDigit ) then
	begin
		FBarDigit := BarcodeCheckDigitPlessey( FBarString, sCode );
		FBarcode := FBarcode + sCode;
//		FBarString := FBarString + IndexToSymbolMSI( FBarDigit div 10 );
//		FBarString := FBarString + IndexToSymbolMSI( FBarDigit mod 10 );
	end;
{ Add the start and stop markers }
	FBarcode := SMSIStart + FBarcode + SMSIStop;
end;

procedure TKCustomBarcodeComponent.GetMSIString;
var
	i,
	iLen: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
	if FUseCheckDigit then
	begin
		FBarDigit := BarcodeCheckDigitMSI( FBarString );
		FBarString := FBarString + IndexToSymbolMSI( FBarDigit );
	end;
	iLen := Length( FBarString );
{ Add the start marker }
	FBarcode := FBarcode + SMSIStart;
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
		FBarcode := FBarcode + SymbolToMSI( FBarString[i] );
{ Add the stop marker }
	FBarcode := FBarcode + SMSIStop;
end;

{
-------------------------------------------------------------------------
--------------- Code25 and Code25i Support Functions --------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.GetCode25String;
var
	i,
	iLen: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Add the start marker }
	FBarcode := FBarcode + SCode25Start;
{ Add extra spacing }
	FBarcode := FBarcode + '0';
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
	begin
		FBarcode := FBarcode + SymbolToCode25( FBarString[i] );
{ Add extra spacing }
		FBarcode := FBarcode + '0';
	end;
{ Add the stop marker }
	FBarcode := FBarcode + SCode25Stop;
end;

procedure TKCustomBarcodeComponent.GetCode25iString;
var
	so,
	se: string;    // variable size
	i,
	j,
	iLen: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Is padding necessary ? }
	if ( ( iLen mod 2 = 0 ) and FUseCheckDigit ) or
		 ( ( iLen mod 2 = 1 ) and ( not FUseCheckDigit ) ) then
	begin
		if ( FPaddingStyle = psLeading ) then
			FBarString := '0' + FBarString
		else
			FBarString := FBarString + '0';
	end;
	if FUseCheckDigit then
	begin
		FBarDigit := BarcodeCheckDigitEAN( FBarString );
		FBarString := FBarString + IndexToSymbolCode25i( FBarDigit );
	end
	else
		FBarDigit := 0;
	iLen := Length( FBarString );
{ Add the start marker }
	FBarcode := FBarcode + SCode25iStart;
{ Add the data bytes of the barcode string }
	for i := 1 to ( iLen div 2 ) do
	begin
		so := SymbolToCode25i( FBarString[2*i - 1] );
		se := SymbolToCode25i( FBarString[2*i] );
		for j := 1 to 5 do
		begin
			if ( so[j] = 'N' ) then
				FBarcode := FBarcode + '1'
			else
				FBarcode := FBarcode + '3';
			if ( se[j] = 'N' ) then
				FBarcode := FBarcode + '0'
			else
				FBarcode := FBarcode + '2';
		end;
	end;
{ Add the stop marker }
	FBarcode := FBarcode + SCode25iStop;
end;

{
-------------------------------------------------------------------------
------------------------ Code 128 Support Functions ---------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.GetCode128AString;
var
	sBarcode: string;
	i,
	iLen: Integer;
begin
	FBarcode := '';
	FBarDigit := BarCodeCheckDigitCode128_A( FBarCaption );
	FBarString := FBarCaption + IndexToSymbolCode128_A( FBarDigit );
	iLen := Length( FBarString );
{ Add the start marker }
	sBarcode := SCode128AStart;
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
		sBarCode := sBarCode + SymbolToCode128_A( FBarString[i] );
	iLen := Length( sBarCode );
{ Add the stop marker }
	sBarcode := sBarcode + SCode128Stop;
	for i := 1 to SizeCode128Start do
		FBarCode := FBarCode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := SizeCode128Start + 1 to iLen do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := iLen + 1 to iLen + SizeCode128Stop do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
{ If the check digit is not printable, then remove it from the caption }
//	if ( FBarDigit > 63 ) then
	Delete( FBarString, Length( FBarString ), 1 );
end;

procedure TKCustomBarcodeComponent.GetCode128BString;
var
	sBarcode: string;
	i,
	iLen: Integer;
begin
	FBarcode := '';
	FBarDigit := BarCodeCheckDigitCode128_B( FBarCaption );
	FBarString := FBarCaption + IndexToSymbolCode128_B( FBarDigit );
	iLen := Length( FBarString );
{ Add the start marker }
	sBarcode := SCode128BStart;
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
		sBarCode := sBarCode + SymbolToCode128_B( FBarString[i] );
	iLen := Length( sBarCode );
{ Add the stop marker }
	sBarcode := sBarcode + SCode128Stop;
	for i := 1 to SizeCode128Start do
		FBarCode := FBarCode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := SizeCode128Start + 1 to iLen do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := iLen + 1 to iLen + SizeCode128Stop do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
{ If the check digit is not printable, then remove it from the caption }
//	if ( FBarDigit > 94 ) then
	Delete( FBarString, Length( FBarString ), 1 );
end;

procedure TKCustomBarcodeComponent.GetCode128CString;

	procedure NormalizeCode128C;
	var
		i: Integer;
		s,
		s1,
		s2: string;
	begin
		i := 1;
		s1 := '';
		s2 := '';
{ guarantee that the last block will always be normalized ;) }
		s := FBarCaption + #255;
{ are we finished yet with s ? }
		while ( s <> '' )  do
		begin
			if ( not IsDigit( s[i] ) ) then
			begin
				if ( i > 1 ) then
				begin
					s1 := Copy( s, 1, i - 1 );
					s := Copy( s, i, Length( s ) );
					if ( ( Length( s1 ) and $0001 ) = $0001 ) then
					begin
						if ( FPaddingStyle = psLeading ) then
							s1 := '0' + s1
						else
							s1 := s1 + '0';
					end;
					s2 := s2 + s1;
					i := 1;
				end
				else
				begin
					if ( s[i] <> #255 ) then
						s2 := s2 + s[i];
					Delete( s, i, 1 );
				end;
			end
			else
				inc( i );
		end;
		FBarCaption := s2;
	end;

var
	s,
	s1,
	sBarcode: string;
	i,
	j,
	iLen: Integer;
begin
	i := 1;
	FBarcode := '';
	NormalizeCode128C;
	FBarDigit := BarCodeCheckDigitCode128_C( FBarCaption );
	FBarString := FBarCaption + IndexToSymbolCode128_C( FBarDigit );
{ Add the start marker }
	sBarcode := SCode128CStart;
{ start of barcode }
	s := FBarString + #255;
{ are we finished yet with s ? }
	while ( s <> '' )  do
	begin
		if ( not IsDigit( s[i] ) ) then
		begin
			if ( i > 1 ) then
			begin
				s1 := Copy( s, 1, i - 1 );
				s := Copy( s, i, Length( s ) );
				for j := 1 to ( Length( s1 ) div 2 ) do
					sBarcode := sBarcode + SymbolToCode128_C( s1[j * 2 - 1] + s1[j * 2] );
				i := 1;
			end
			else
				Delete( s, i, 1 );
		end
		else
			inc( i );
	end;
{ end of barcode }
	iLen := Length( sBarCode );
{ Add the stop marker }
	sBarcode := sBarcode + SCode128Stop;
	for i := 1 to SizeCode128Start do
		FBarCode := FBarCode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := SizeCode128Start + 1 to iLen do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
	for i := iLen + 1 to iLen + SizeCode128Stop do
		FBarCode := FBarcode + Code128Mappings[( i and $0001 ), sBarcode[i]];
{ If the check digit is not printable, then remove it from the caption }
//	if ( FBarDigit > 99 ) then
	Delete( FBarString, Length( FBarString ) - 1, 2 );
end;

{
-------------------------------------------------------------------------
------------------------ Codabar Support Functions ----------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.GetCodabarString;
var
	i,
	iLen: Integer;
begin
	FBarcode := '';
	FBarString := FBarCaption;
	iLen := Length( FBarString );
{ Add the start character }
	FBarcode := FBarcode + SCodabarStart;
{ Add extra space }
	FBarcode := FBarcode + '0';
{ Add the data bytes of the barcode string }
	for i := 1 to iLen do
	begin
		FBarcode := FBarcode + SymbolToCodabar( FBarString[i] );
{ Add extra space }
		FBarcode := FBarcode + '0';
	end;
{ Add the stop character }
	FBarcode := FBarcode + SCodabarStop;
end;

{
-------------------------------------------------------------------------
---------------------------- Private Members ----------------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.PropChanged;
begin
	if FPainting then
	  Exit;
	FPropChanged := true;
	Invalidate;
end;

procedure TKCustomBarcodeComponent.ClearTextInfo;
var
	i: Integer;
begin
	for i := FTextInfo.Count - 1 downto 0 do
		if ( FTextInfo.Objects[i] <> nil ) then
			FreeMem( PPoint( FTextInfo.Objects[i] ), SizeOf( TPoint ) );
	FTextInfo.Clear;
end;

procedure TKCustomBarcodeComponent.ClearAddOnTextInfo;
var
	i: Integer;
begin
	for i := FAddOnTextInfo.Count - 1 downto 0 do
		if ( FAddOnTextInfo.Objects[i] <> nil ) then
			FreeMem( PPoint( FAddOnTextInfo.Objects[i] ), SizeOf( TPoint ) );
	FAddOnTextInfo.Clear;
end;

procedure TKCustomBarcodeComponent.NormalizeBarString;
begin
	FBarString := FBarCaption;
	try
		case FBarKind of
				 bkEAN13: GetEAN13String( false );
					bkEAN8: GetEAN8String;
					bkUPCA: GetUPCAString;
					bkUPCE: GetUPCEString;
			 bkCodabar: GetCodabarString;
					bkISBN: GetISBNString;
					 bkMSI: GetMSIString;
			 bkPlessey: GetPlesseyString;
				bkCode25: GetCode25String;
			 bkCode25i: GetCode25iString;
				bkCode39: GetCode39String;
			 bkCode39E: GetCode39eString;
			bkCode128A: GetCode128AString;
			bkCode128B: GetCode128BString;
		else
			GetCode128CString;
		end;
	except
		BarCaption := '';
	end;
end;

procedure TKCustomBarcodeComponent.NormalizeAddOn;
begin
	if ( Length( FAddOnCaption ) = 0 ) then
	begin
		case FAddOnStyle of
			asEAN5: FAddOnCaption := '00000';
		else
			FAddOnCaption := '00';
		end;
		BuildAddOn;
		Exit;
	end;
	if ( FAddOnStyle = asEAN2 ) and ( Length( FAddOnCaption ) > 2 ) then
		SetLength( FAddOnCaption, 2 );
	BuildAddOn;
end;

procedure TKCustomBarcodeComponent.NormalizeCaption;
begin
	if ( Length( FBarCaption ) = 0 ) then
	begin
		case FBarKind of
				 bkEAN8: FBarCaption := '0000000';
				 bkUPCA: FBarCaption := '00000000000';
				 bkUPCE: FBarCaption := '0000000000';
				 bkISBN: FBarCaption := '0000000000';
				bkEAN13: FBarCaption := '000000000000';
			bkCode25i: FBarCaption := '00';
		else
			FBarCaption := '0';
		end;
		Exit;
	end;
	if ( FBarKind = bkEAN13 ) and ( Length( FBarCaption ) > 12 ) then
		SetLength( FBarCaption, 12 )
	else if ( FBarKind = bkEAN8 ) and ( Length( FBarCaption ) > 7 ) then
		SetLength( FBarCaption, 7 )
	else if ( FBarKind = bkUPCA ) and ( Length( FBarCaption ) > 11 ) then
		SetLength( FBarCaption, 11 )
	else if ( FBarKind = bkUPCE ) and ( Length( FBarCaption ) > 10 ) then
		SetLength( FBarCaption, 10 )
	else if ( FBarKind = bkISBN ) and ( Length( FBarCaption ) > 10 ) then
		SetLength( FBarCaption, 10 );
end;

procedure TKCustomBarcodeComponent.SetAlign( Value: TAlign );
begin
	if ( ( Value <> alNone ) and FAutoSize ) then
		FAutoSize := false;
	FOwner.Align := Value;
	PropChanged;
end;

procedure TKCustomBarcodeComponent.SetAlignment( Value: TAlignment );
begin
	if ( FAlignment <> Value ) then
	begin
		FAlignment := Value;
		if ( not FAutoSize ) then
			PropChanged;
	end;
end;

{
procedure TKCustomBarcodeComponent.SetAngle( Value: TKAngle );
begin
	if ( FAngle <> Value ) and FAutoSize then
	begin
		FAngle := Value;
		Invalidate;
	end;
end;
}

procedure TKCustomBarcodeComponent.SetAutosize( Value: Boolean );
begin
	if ( FAutosize <> Value ) then
	begin
		FAutosize := Value;
		if FAutoSize then
		begin
			Align := alNone;
			FStretched := false;
		end;
	end;
end;

procedure TKCustomBarcodeComponent.SetBarCaption( const Value: TKBarCaption );
begin
	if ( csLoading in FOwner.ComponentState ) then
	begin
		FBarCaption := Value;
		Exit;
	end;
	if FLoaded or ( FBarCaption <> Value ) then
	begin
		if ( FBarKind in [bkCode39E, bkCode128A, bkCode128C] ) then
			FBarCaption := AnsiUpperCase( Value )
		else
			FBarCaption := Value;
		NormalizeCaption;
		NormalizeBarString;		// BarString and BarDigit might change
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetBarCrackColor( Value: TColor );
begin
	if ( FBarCrackColor <> Value ) then
	begin
		FBarCrackColor := Value;
		if ( not FTransparent ) then
		begin
			OwnerCanvas.Brush.Color := FBarCrackColor;
			PropChanged;
		end;
	end;
end;

procedure TKCustomBarcodeComponent.SetBarColor( Value: TColor );
begin
	if ( FBarColor <> Value ) then
	begin
		FBarColor := Value;
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetBarDigit( Value: TKBarDigit );
begin
end;

procedure TKCustomBarcodeComponent.SetBarKind( Value: TKBarKind );
begin
	if ( FBarKind <> Value ) then
	begin
		FBarKind := Value;
		if ( FBarKind in [bkCode39E, bkCode128A, bkCode128C] ) then
			FBarCaption := AnsiUpperCase( FBarCaption );
		if ( FBarKind in [bkCodabar, bkCode25, bkCode25i, bkCode39, bkCode39E] ) then
			FThickBarRatio := 3;
		if not ( FBarkind in [bkEAN13, bkUPCA, bkISBN] ) then
			FAddOnStyle := asNone;
		NormalizeCaption;
		NormalizeBarString;		// BarString and BarDigit might change
		PropChanged;
	end;
end;

procedure  TKCustomBarcodeComponent.SetBarString( const Value: string );
begin
end;

procedure TKCustomBarcodeComponent.SetAddOnString( const Value: string );
begin
end;

procedure TKCustomBarcodeComponent.SetBarWidth( Value: Byte );
var
	bChanged: Boolean;
begin
	if ( csLoading in FOwner.ComponentState ) then
	begin
		FBarWidth := Value;
		Exit;
	end;
	bChanged := false;
	if ( FBarWidth <> Value ) then
	begin
		FBarWidth := Value;
		bChanged := true;
	end;
	if ( FBarWidth = 0 ) then
	begin
		FBarWidth := 3;
		bChanged := true;
	end;
	if bChanged then
		PropChanged;
end;

procedure TKCustomBarcodeComponent.FontChanged( Sender: TObject );
begin
	if FPainting and Assigned( FAdjustFontPainting ) then
	begin
    FAdjustFontPainting( Self );
		Exit;
	end;
	if FShowCaption then
		PropChanged;
end;

function TKCustomBarcodeComponent.GetAlign: TAlign;
begin
	Result := FOwner.Align;
end;

procedure TKCustomBarcodeComponent.SetFont( Value: TFont );
begin
	FFont.Assign( Value );
	OwnerCanvas.Font.Assign( Value );
	if FShowCaption then
		PropChanged;
end;

procedure TKCustomBarcodeComponent.SetAddOnStyle( Value: TKAddOnStyle );
begin
	if ( FBarKind in [bkUPCA, bkEAN13, bkISBN] ) and ( FAddOnStyle <> Value ) then
	begin
		FAddOnStyle := Value;
		NormalizeAddOn;
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetPropChanged( Value: Boolean );
begin
	FPropChanged := Value;
end;

procedure TKCustomBarcodeComponent.SetAddOnCaption( const Value: TKAddOnCaption );
begin
	if ( FAddOnCaption <> Value ) then
	begin
		FAddOnCaption := Value;
		if ( FBarKind in [bkUPCA, bkEAN13, bkISBN] ) then
		begin
			NormalizeAddOn;
			PropChanged;
		end;
	end;
end;

procedure TKCustomBarcodeComponent.SetPaddingStyle( Value: TKPaddingStyle );
begin
	if ( FBarKind in [bkEAN8, bkEAN13, bkCode25i, bkUPCA, bkUPCE] ) and
		 ( FPaddingStyle <> Value ) then
	begin
		FPaddingStyle := Value;
		NormalizeBarString;			// BarString might change
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetTransparent( Value: Boolean );
begin
	if ( FTransparent <> Value ) then
	begin
		FTransparent := Value;
		with OwnerCanvas.Brush do
			if FTransparent then
				Style := bsClear
			else
			begin
				Style := bsSolid;
				Color := FBarCrackColor;
			end;
		OwnerCanvas.Font.Assign( FFont );
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetShowCaption( Value: Boolean );
begin
	if ( FShowCaption <> Value ) then
	begin
		FShowCaption := Value;
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetThickBarRatio( Value: TKThickBar );
begin
	if ( FThickBarRatio <> Value ) and
		 ( FBarKind in [bkCodabar, bkCode25i, bkCode39, bkCode39E] ) then
	begin
		FThickBarRatio := Value;
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetUseCheckDigit( Value: Boolean );
begin
	if ( FBarKind in [bkMSI, bkPlessey, bkCode25i, bkCode39, bkCode39E] ) and ( FUseCheckDigit <> Value ) then
	begin
		FUseCheckDigit := Value;
		NormalizeBarString; 		// BarString and BarDigit might change
		PropChanged;
	end;
end;

procedure TKCustomBarcodeComponent.SetStretched( Value: Boolean );
begin
	if ( FStretched <> Value ) and ( not FAutoSize ) then
	begin
		FStretched := Value;
		PropChanged;
	end;
end;

{
-------------------------------------------------------------------------
--------------------------- Painting Support ----------------------------
-------------------------------------------------------------------------
}

procedure TKCustomBarcodeComponent.TextDraw( IsAddOn: Boolean; x, y: Integer; const Text: string );

	procedure PaintText( Canvas: TCanvas; Operation: Byte );
	begin
		with Canvas do
		begin
			case Operation of
				poSRCDEFAULT: Font.Color := FFont.Color;
				poSRCAND: Font.Color := FFont.Color;
			end;
			TextOut( x, y, Text );
		end;
	end;

begin
	if ( not IsAddOn ) then
	begin
		if ( not FTransparent ) then
			PaintText( FBitmap.Canvas, poSRCDEFAULT )
		else
			PaintText( OwnerCanvas, poSRCAND );
	end
	else
	begin
		if ( not FTransparent ) then
			PaintText( bmpAddOn.Canvas, poSRCDEFAULT )
		else
			PaintText( OwnerCanvas, poSRCAND );
	end;
end;

procedure TKCustomBarcodeComponent.CanvasStretchDraw( IsAddOn: Boolean );
begin
	SetStretchBltMode( FBitmap.Canvas.Handle, COLORONCOLOR );
	if ( not FTransparent ) then
		StretchBlt( OwnerCanvas.Handle, 0, 0, FOwner.Width, FOwner.Height,
			FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height,
			SRCCOPY )
	else
		StretchBlt( OwnerCanvas.Handle, 0, 0, FOwner.Width, FOwner.Height,
			bmpAND.Canvas.Handle, 0, 0, bmpAND.Width, bmpAND.Height,
			SRCAND );
end;

procedure TKCustomBarcodeComponent.CanvasDraw( x, y: Integer );
begin
	if ( not FTransparent ) then
		OwnerCanvas.Draw( x, y, FBitmap )
	else
		BitBlt( OwnerCanvas.Handle, x, y, FOwner.Width, FOwner.Height,
			bmpAND.Canvas.Handle, 0, 0, SRCAND );
end;

procedure TKCustomBarcodeComponent.ColumnDraw( IsAddOn: Boolean; var Col: Integer; Y, AHeight, Mode: Integer );

	procedure PaintColumn( Canvas: TCanvas; Operation: Byte );
	var
		i: Integer;
	begin
		with Canvas do
		begin
			Pen.Width := FBarWidth;
			case Mode of
				bmBar,
				bmBarThick:
					case Operation of
						poSRCDEFAULT: Pen.Color := FBarColor;
						poSRCAND: Pen.Color := FBarColor;
					end;
				bmBarCrack,
				bmBarThickCrack:
					case Operation of
						poSRCDEFAULT: Pen.Color := FBarCrackColor;
						poSRCAND: Pen.Color := clWhite;
					end;
			end;
			if ( Mode in [bmBarThick, bmBarThickCrack] ) then
				for i := 1 to FThickBarRatio do
				begin
					MoveTo( Col, Y );
					LineTo( Col, Y + AHeight );
					inc( Col, FBarWidth );
				end
			else
			begin
				MoveTo( Col, Y );
				LineTo( Col, Y + AHeight );
				inc( Col, FBarWidth );
			end;
		end;
	end;

begin
	if ( not IsAddOn ) then
	begin
		if ( not FTransparent ) then
			PaintColumn( FBitmap.Canvas, poSRCDEFAULT )
		else
			PaintColumn( bmpAND.Canvas, poSRCAND );
	end
	else
	begin
		if ( not FTransparent ) then
			PaintColumn( bmpAddOn.Canvas, poSRCDEFAULT )
		else
			PaintColumn( bmpAddOnAND.Canvas, poSRCAND );
	end;
end;

procedure TKCustomBarcodeComponent.Paint;

	function MakeObject( x, y: Integer ): TObject;
	var
		p: PPoint;
	begin
		p := New( PPoint );
		p^ := Point( x, y );
		Result := TObject( p );
	end;

	function UpdateWidth( IsAddOn: Boolean; NewWidth: Integer ): Integer;
	begin
		if ( not IsAddOn ) then
		begin
			FBitmap.Width := NewWidth;
			bmpAND.Width := NewWidth;
		end
		else
		begin
			bmpAddOn.Width := NewWidth;
			bmpAddOnAND.Width := NewWidth;
		end;
		Result := NewWidth;
	end;

	function UpdateHeight( IsAddOn: Boolean; NewHeight: Integer ): Integer;
	begin
		if ( not IsAddOn ) then
		begin
			FBitmap.Height := NewHeight;
			bmpAND.Height := NewHeight;
		end
		else
		begin
			bmpAddOn.Height := NewHeight;
			bmpAddOnAND.Height := NewHeight;
		end;
		Result := NewHeight;
	end;

	function CharToDigit( ch: Char ): Byte;
	begin
		Result := Ord( ch )- 48;
	end;

	function CalcBitmapWidth( const s: string ): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 1 to Length( s ) do
			if ( s[i] > '1' ) then
				inc( Result, FThickBarRatio * FBarWidth )
			else
				inc( Result, FBarWidth );
	end;

var
	i,
	iCol,
	iCol1,
	iCol2,
	iCaptionTop,
	iBarHeightEx,
	iHeight,
	x0,
	pX,
	pY: Integer;
	s: string;
begin
	if ( ( FLoaded ) or ( FPainting ) or ( not FOwner.Visible ) or Loading( Self ) ) then
		Exit;
	iCol1 := 0;
	iCaptionTop := 0;
	iBarHeightEx := 0;
	inc( FPaintCount );
{ If this is not just a refresh, FPropChanged is TRUE !!! This is
	a very good workaround for a lazy painting routine with many
	calculations: if the input for the calculations doesn't change,
	then it is only necessary to repaint what is currently in cache;
	otherwise, the entire process has to be carried out. This has
	removed most of the visible delay of the painting of the barcodes. }
	if FPropChanged or ( FPaintCount = 1 ) then
	begin
		iCol := 1;
{ clear TextInfo ( string and position of caption fragments ) }
		ClearTextInfo;
{ reset the bitmaps before painting the new barcode }
		with FBitmap, Canvas do
		begin
{ initial estimates for width and height of the barcode }
			Width := UpdateWidth( false, CalcBitmapWidth( FBarcode ) + 2 );
			iHeight := UpdateHeight( false, FOwner.Height );
{ if the caption is to be displayed, update text format information }
			if FShowCaption then
			begin
				Font.Assign( FFont );
				while ( TextWidth( BarString ) > Width ) do
					Font.Size := Font.Size - 1;
				Self.Font.Size := Font.Size;
				iHeight := Height - TextHeight( BarString ) - 3;
				iCaptionTop := iHeight + 2;
				iBarHeightEx := iHeight + 3 + ( TextHeight( BarString ) div 2 );
				if ( iHeight <= 0 ) then
				begin
					FShowCaption := false;
					iHeight := Height;
				end;
			end;
			Brush.Color := FBarCrackColor;
			FillRect( Rect( 0, 0, Width, Height ) );
		end;
{ if the user chose transparent mode, a second bitmap will have to
	be used; it is also necessary to assign the correct font to the
	canvas owner }
		if FTransparent then
			with bmpAND, Canvas do
			begin
				Font.Assign( FBitmap.Canvas.Font );
				Brush.Color := clWhite;
				FillRect( Rect( 0, 0, Width, Height ) );
				if FShowCaption then
					OwnerCanvas.Font.Assign( FBitmap.Canvas.Font );
			end;
{
********************************************************************
********** Build the bitmap - Part I: EAN13, ISBN and UPCA *********
********************************************************************
}
		if ( FBarKind in [bkEAN13, bkISBN, bkUPCA] ) and FShowCaption then
		begin
			iCol := FBitmap.Canvas.TextWidth( BarString[1] ) + 4;
			UpdateWidth( false, FBitmap.Width + iCol );
			if ( FBarKind = bkUPCA ) then
				UpdateWidth( false, FBitmap.Width + FBitmap.Canvas.TextWidth( BarString[Length( BarString )] ) + 4 );
			FTextInfo.AddObject( BarString[1], MakeObject( 1, iCaptionTop ) );
			for i := 1 to LenEANGuard do
				ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			iCol1 := iCol;
{ in UPCA, the bars representing the system character should extend
	down, juse like the guard and center bars }
			if ( FBarkind = bkUPCA ) then
			begin
				for i := PosEANStart to PosEANStart + 6 do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
				iCol1 := iCol;
				for i := PosEANStart + 7 to PosEAN13FirstStop - 1 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
			end
			else
				for i := PosEANStart to PosEAN13FirstStop - 1 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
			iCol2 := iCol;
			if ( FBarKind = bkUPCA ) then
				s := Copy( BarString, 2, 5 )
			else
				s := Copy( BarString, 2, 6 );
			FTextInfo.AddObject( s, MakeObject( ( iCol1 + iCol2 - FBitmap.Canvas.TextWidth( s ) ) div 2, iCaptionTop ) );
			for i := PosEAN13FirstStop to PosEAN13SecondStop - 1 do
				ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			iCol1 := iCol;
{ in UPCA, the bars representing the checkdigit character should extend
	down, juse like the guard and center bars }
			if ( FBarkind = bkUPCA ) then
			begin
				for i := PosEAN13SecondStop to PosEAN13LastStop - 8 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
				iCol2 := iCol;
				for i := PosEAN13LastStop - 7 to PosEAN13LastStop - 1 do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			end
			else
			begin
				for i := PosEAN13SecondStop to PosEAN13LastStop - 1 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
				iCol2 := iCol;
			end;
			if ( FBarKind = bkUPCA ) then
				s := Copy( BarString, 7, 5 )
			else
				s := Copy( BarString, 8, 6 );
			FTextInfo.AddObject( s, MakeObject( ( iCol1 + iCol2 - FBitmap.Canvas.TextWidth( s ) ) div 2, iCaptionTop ) );
			for i := PosEAN13LastStop to Length( Barcode ) do
				ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			if ( FBarKind = bkUPCA ) then
				FTextInfo.AddObject( BarString[Length( BarString )], MakeObject( iCol + 2, iCaptionTop ) );
		end
{
*****************************************************
********** Build the bitmap - Part II: EAN8 *********
*****************************************************
}
		else if ( FBarKind = bkEAN8 ) and FShowCaption then
			with FBitmap do
			begin
				for i := 1 to LenEANGuard do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
				iCol1 := iCol;
				for i := PosEANStart to PosEAN8FirstStop - 1 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
				iCol2 := iCol;
				s := Copy( BarString, 1, 4 );
				FTextInfo.AddObject( s, MakeObject( ( iCol1 + iCol2 - FBitmap.Canvas.TextWidth( s ) ) div 2, iCaptionTop ) );
				for i := PosEAN8FirstStop to PosEAN8SecondStop - 1 do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
				iCol1 := iCol;
				for i := PosEAN8SecondStop to PosEAN8LastStop - 1 do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
				iCol2 := iCol;
				s := Copy( BarString, 5, 4 );
				FTextInfo.AddObject( s, MakeObject( ( iCol1 + iCol2 - FBitmap.Canvas.TextWidth( s ) ) div 2, iCaptionTop ) );
				for i := PosEAN8LastStop to Length( BarCode ) do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			end
{
******************************************************
********** Build the bitmap - Part III: UPCE *********
******************************************************
}
		else if ( FBarKind = bkUPCE ) and FShowCaption then
			with FBitmap do
			begin
				for i := 1 to LenEANGuard do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
				iCol1 := iCol;
				for i := LenEANGuard + 1 to PosUPCEStop do
					ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
				iCol2 := iCol;
				s := Copy( BarString, 1, 4 );
				FTextInfo.AddObject( FBarString, MakeObject( ( iCol1 + iCol2 - FBitmap.Canvas.TextWidth( FBarString ) ) div 2, iCaptionTop ) );
				for i := PosUPCEStop + 1 to Length( BarCode ) do
					ColumnDraw( false, iCol, 0, iBarHeightEx, CharToDigit( Barcode[i] ) );
			end
		else
{
****************************************************************
********** Build the bitmap - Part IV: all other kinds *********
****************************************************************
}
		begin
			for i := 1 to Length( Barcode ) do
				ColumnDraw( false, iCol, 0, iHeight, CharToDigit( Barcode[i] ) );
			if FShowCaption then
				FTextInfo.AddObject( BarString, MakeObject( ( FBitmap.Width - FBitmap.Canvas.TextWidth( BarString ) ) div 2, iCaptionTop ) );
		end;
{
***************************************************************
********** if there is an AddOn to paint, paint it ! **********
***************************************************************
}
		if ( FAddOnStyle <> asNone ) then
		begin
			iCol := 0;
	{ clear AddOnTextInfo ( string and position of caption fragments ) }
			ClearAddOnTextInfo;
	{ reset the bitmaps before painting the new barcode }
			with bmpADDON, Canvas do
			begin
	{ initial estimates for width and height of the barcode }
				Width := UpdateWidth( true, CalcBitmapWidth( FAddOnBarcode ) );
				Height := UpdateHeight( true, FBitmap.Height );
	{ if the caption is to be displayed, update text format information }
				if FShowCaption then
				begin
					Font.Assign( FFont );
					iHeight := iHeight - TextHeight( FAddOnString );
					iBarHeightEx := TextHeight( FAddOnString ) + 3;
					iCaptionTop := 1;
					if ( iHeight <= 0 ) then
					begin
						FShowCaption := false;
						iHeight := Height;
					end;
				end;
				Brush.Color := FBarCrackColor;
				FillRect( Rect( 0, 0, Width, Height ) );
			end;
			if FTransparent then
				with bmpAddOnAND, Canvas do
				begin
					Font.Assign( FBitmap.Canvas.Font );
					Brush.Color := clWhite;
					FillRect( Rect( 0, 0, Width, Height ) );
				end;
{ This is the time to build the addon barcode bitmap }
			for i := 1 to Length( FAddOnBarcode ) do
			begin
				ColumnDraw( true, iCol, iBarHeightEx, iHeight, CharToDigit( FAddOnBarcode[i] ) );
{ cache the position right after the guard }
				if ( i = 9 ) then
					iCol1 := iCol;
			end;
			if FShowCaption then
				FTextInfo.AddObject( FAddOnString, MakeObject( ( bmpAddOn.Width - bmpAddOn.Canvas.TextWidth( FAddOnString ) - iCol1 ) div 2 + FBitmap.Width + iCol1, iCaptionTop ) );
{ merge addon bitmap with the bitmap to send to the canvas }
			UpdateWidth( false, FBitmap.Width + bmpAddOn.Width );
			if FTransparent then
				bmpAND.Canvas.Draw( bmpAND.Width - bmpAddOnAND.Width, 0, bmpAddOnAND )
			else
				FBitmap.Canvas.Draw( FBitmap.Width - bmpAddOn.Width, 0, bmpAddOn );
		end;
		iCol := FBitmap.Width;
	end;
{
*****************************************************************
********** This is the time to really start painting ! **********
*****************************************************************
}
	FPainting := true;
	try
		x0 := 0;
{ First of all, draw the bitmap on the OwnerCanvas }
		if FAutoSize then
		begin
			if FPropChanged then
				FOwner.Width := iCol
			else
				FOwner.Width := FBitmap.Width;
			CanvasDraw( 0, 0 );
		end
		else if ( not FStretched ) then
		begin
			if ( FAlignment = taLeftJustify ) then
				CanvasDraw( 0, 0 )
			else if ( FAlignment = taRightJustify ) then
			begin
				x0 := FOwner.Width - FBitmap.Width;
				CanvasDraw( x0, 0 );
			end
			else
			begin
				x0 := ( ( FOwner.Width - FBitmap.Width ) div 2 );
				CanvasDraw( x0, 0 );
			end;
		end
		else
			CanvasStretchDraw( false );
{
*****************************************************************
********** Draw the captions of the barcode and addon ! *********
*****************************************************************
}
		if FShowCaption then
		begin
			with FTextInfo do
				for i := 0 to Count - 1 do
				begin
					Px := PPoint( Objects[i] )^.X;
					Py := PPoint( Objects[i] )^.Y;
					if FStretched then
					begin
						Px := MulDiv( Px, FOwner.Width, FBitmap.Width );
						Py := MulDiv( Py, FOwner.Height, FBitmap.Height );
					end;
					OwnerCanvas.TextOut( x0 + Px, Py, Strings[i] );
				end;
			if ( AddOnStyle <> asNone ) then
				with FAddOnTextInfo do
					for i := 0 to Count - 1 do
					begin
						Px := PPoint( Objects[i] )^.X;
						Py := PPoint( Objects[i] )^.Y;
						if FStretched then
						begin
							Px := MulDiv( Px, FOwner.Width, FBitmap.Width );
							Py := MulDiv( Py, FOwner.Height, FBitmap.Height );
						end;
						OwnerCanvas.TextOut( x0 + Px, Py, Strings[i] );
					end;
		end;
	finally
		FPainting := false;
		FPropChanged := false;
	end;
end;

end.
