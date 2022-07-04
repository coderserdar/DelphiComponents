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

unit uksyTypes;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, ActiveX, SysUtils, Classes, uksyConsts;

{
--------------------------------------------------------------------------------
----------------------- Generic set/enumerated types ---------------------------
--------------------------------------------------------------------------------
}

const
	SETSIZE_INTEGER  = BITS_PER_BYTE * SizeOf( Integer );

type

	TKAngle    = 0..360;
	TKBitEnum  = 0..SETSIZE_INTEGER-1;

	TKUpperAlphaEnum = 'A'..'Z';
	TKLowerAlphaEnum = 'a'..'z';
	TKDigitEnum = '0'..'9';

	TKASCIICtrlEnum    = #0..#31;
	TKASCIINoDispEnum  = #127..#159;

	TKDay = 1..31;
	TKMonth = 1..12;
	TKYear = 1..High( SmallInt ) - 1;

	TKMSeconds = 0..999;
	TKSeconds = 0..59;
	TKMinute = 0..59;
	TKHour = 0..23;

	TKCharSet           = set of Char;
	TKNumericCharSet    = set of TKDigitEnum;
	TKUpperAlphaCharSet = set of TKUpperAlphaEnum;
	TKLowerAlphaCharSet = set of TKLowerAlphaEnum;

	TKASCIICtrlSet   = set of TKASCIICtrlEnum;
	TKASCIINoDispSet = set of TKASCIINoDispEnum;

	TKByteSet    = set of Byte;
	TKBitSet     = set of TKBitEnum;
	TKIntegerSet = set of TKBitEnum;

{
	This interesting type can be used to hardly typecast a set with 1 or 2 bytes
	with this 4 byte structure (i.e Integer( PKIntegerSet( @My2ByteSet )^ ),
	Integer( TKIntegerSet( My2ByteSet ) ) generate an error).
}
	PKIntegerSet = ^TKIntegerSet;

{$M+}
  TKCharsetEnum = ( ceFull, ceASCIICtrl, ceUpperAlpha, ceLowerAlpha,
		ceDigit, ceASCIINoDisp, ceTextFormat, ceHexDigits, ceSymbols,
		ceNum, ceFloat, ceAlpha, ceAlphaNum, ceIdentifier, ceSourceCode,
		ceInvalid );

	TKMonthEnum = ( meJanuary, meFebruary, meMarch, meApril, meMay,
		meJune, meJuly, meAugust, meSeptember, meOctober, meNovember,
		meDecember );
	TKMonthEnums = set of TKMonthEnum;

	TKWeekEnum = ( weSunday, weMonday, weTuesday, weWednesday, weThursday, weFriday, weSaturday );
	TKWeekEnums = set of TKWeekEnum;
{$M-}

	TKFileAttribute = ( faReadOnly, faHidden, faSystem, faDirectory,
		faArchive, faNormal, faTemporary, faCompressed );
	TKFileAttributes = set of TKFileAttribute;

	TKFileInfoFlag = ( fifAttributes, fifDisplayName, fifIcon,
		fifIconLocation, fifLargeIcon, fifLinkOverlay, fifOpenIcon,
		{fifPIDL, } fifSelected, fifShellIconSize, fifSmallIcon,
		fifSysIconIndex, fifTypeName, fifUseFileAttributes );
	TKFileInfoFlags = set of TKFileInfoFlag;
	
	TKSpecialFolder = ( sfDesktop, sfPrograms, sfControls, sfPrinters,
		sfPersonal, sfFavorites, sfStartup, sfRecent, sfSendTo, sfBitBucket,
		sfStartMenu, sfDesktopDirectory, sfDrives, sfNetwork, sfNetHood,
		sfFonts, sfTemplates, sfCommonStartMenu, sfCommonPrograms,
		sfCommonStartup, sfCommonDesktopDirectory, sfAppData, sfPrintHood,
		sfCookies );

const
  MOTNTH_ENUMS: array[TKMonth] of TKMonthEnum = ( meJanuary, meFebruary, meMarch,
		meApril, meMay, meJune, meJuly, meAugust, meSeptember, meOctober, meNovember,
		meDecember );

var
	CHARSETS: array[TKCharsetEnum] of TKCharSet =
	(
		CHARSET_FULL,
		CHARSET_ASCIICTRL,
		CHARSET_UPPERALPHA,
		CHARSET_LOWERALPHA,
		CHARSET_DIGIT,
		CHARSET_ASCIINODISP,
		CHARSET_TEXTFORMAT,
		CHARSET_HEXDIGITS,
		CHARSET_SYMBOLS,
		CHARSET_NUM,
		CHARSET_FLOAT,
		CHARSET_ALPHA,
		CHARSET_ALPHANUM,
		CHARSET_IDENTIFIER,
		CHARSET_SOURCECODE,
		CHARSET_INVALID_TOKENS
	);

	DAYS_IN_MONTH: array[Boolean, TKMonth] of TKDay =
	( ( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ), { not LeapYear }
		( 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ) );

{
--------------------------------------------------------------------------------
--------------------------- Generic procedural types ---------------------------
--------------------------------------------------------------------------------
}

type
	TKGetStrFunc = function( const Source: string ): Boolean;
	TKGetPCharFunc = function( const Source: PChar ): Boolean;

{
--------------------------------------------------------------------------------
---------------------------- Generic pointer types -----------------------------
--------------------------------------------------------------------------------
}

	PShortInt   = ^ShortInt;
	PSmallInt   = ^SmallInt;
	PLongInt    = ^LongInt;
	PByte       = ^Byte;
	PWord       = ^Word;
	PInteger    = ^Integer;
	PCardinal   = ^Cardinal;
	PPointer    = ^Pointer;
	PBoolean    = ^Boolean;
	PReal       = ^Real;
	PSingle     = ^Single;
	PDouble     = ^Double;
	PExtended   = ^Extended;
	PComp       = ^Comp;
	PCurrency   = ^Currency;
	PDateTime   = ^TDateTime;
	PMethod     = ^TMethod;
	POleVariant = ^OleVariant;
	PVarData    = ^TVarData;
	PVoid       = type Pointer;

{
--------------------------------------------------------------------------------
----------------------------- Generic array types ------------------------------
--------------------------------------------------------------------------------
}

const

	MAXSIZE_SHORTINT   = SIZE_2MB div SizeOf( Shortint );
	MAXSIZE_SMALLINT   = SIZE_2MB div SizeOf( Smallint );
	MAXSIZE_LONGINT    = SIZE_2MB div SizeOf( Longint );
	MAXSIZE_CARDINAL   = SIZE_2MB div SizeOf( Cardinal );
	MAXSIZE_BYTE       = SIZE_2MB div SizeOf( Byte );
	MAXSIZE_WORD       = SIZE_2MB div SizeOf( Word );
	MAXSIZE_INTEGER    = SIZE_2MB div SizeOf( Integer );
	MAXSIZE_POINTER    = SIZE_2MB div SizeOf( Pointer );
	MAXSIZE_STRING     = SIZE_2MB div SizeOf( string );
	MAXSIZE_BOOLEAN    = SIZE_2MB div SizeOf( Boolean );
	MAXSIZE_REAL       = SIZE_2MB div SizeOf( Real );
	MAXSIZE_SINGLE     = SIZE_2MB div SizeOf( Single );
	MAXSIZE_DOUBLE     = SIZE_2MB div SizeOf( Double );
	MAXSIZE_EXTENDED   = SIZE_2MB div SizeOf( Extended );
	MAXSIZE_COMP       = SIZE_2MB div SizeOf( Comp );
	MAXSIZE_CURRENCY   = SIZE_2MB div SizeOf( Currency );
	MAXSIZE_DATETIME   = SIZE_2MB div SizeOf( TDatetime );
	MAXSIZE_METHOD     = SIZE_2MB div SizeOf( TMethod );
	MAXSIZE_VARIANT    = SIZE_2MB div SizeOf( Variant );
	MAXSIZE_VARDATA    = SIZE_2MB div SizeOf( TVarData );
	MAXSIZE_VARIANTARG = SIZE_2MB div SizeOf( TVariantArg );
	MAXSIZE_OLEVARIANT = SIZE_2MB div SizeOf( OleVariant );
	MAXSIZE_CHAR       = SIZE_2MB div SizeOf( Char );
	MAXSIZE_WIDECHAR   = SIZE_2MB div SizeOf( WideChar );
	MAXSIZE_OBJECT     = SIZE_2MB div SizeOf( TObject );
	MAXSIZE_CLASS      = SIZE_2MB div SizeOf( TClass );

	MAXSIZE_BIT      = 4 * KB; { Page align }

type

	PShortIntArray = ^TShortIntArray;
	TShortIntArray = array[0..MAXSIZE_SHORTINT-1] of ShortInt;

	PSmallIntArray = ^TSmallIntArray;
	TSmallIntArray = array[0..MAXSIZE_SMALLINT-1] of SmallInt;

	PLongIntArray = ^TLongIntArray;
	TLongIntArray = array[0..MAXSIZE_LONGINT-1] of LongInt;

	PCardinalArray = ^TCardinalArray;
	TCardinalArray = array[0..MAXSIZE_CARDINAL-1] of Cardinal;

	PByteArray = ^TByteArray;
	TByteArray = array[0..MAXSIZE_BYTE-1] of Byte;

	PWordArray = ^TWordArray;
	TWordArray = array[0..MAXSIZE_WORD-1] of Word;

	PIntegerArray = ^TIntegerArray;
	TIntegerArray = array[0..MAXSIZE_INTEGER-1] of Integer;

	PPointerArray = ^TPointerArray;
	TPointerArray = array[0..MAXSIZE_POINTER-1] of Pointer;

  PPCharArray = ^TPCharArray;
	TPCharArray = array[0..MAXSIZE_POINTER-1] of PChar;

	PStringArray = ^TStringArray;
	TStringArray = array[0..MAXSIZE_STRING-1] of string;

	PBooleanArray = ^TBooleanArray;
	TBooleanArray = array[0..MAXSIZE_BOOLEAN-1] of Boolean;

	PRealArray = ^TRealArray;
	TRealArray = array[0..MAXSIZE_REAL-1] of Real;

	PSingleArray = ^TSingleArray;
	TSingleArray = array[0..MAXSIZE_SINGLE-1] of Single;

	PDoubleArray = ^TDoubleArray;
	TDoubleArray = array[0..MAXSIZE_DOUBLE-1] of Double;

	PExtendedArray = ^TExtendedArray;
	TExtendedArray = array[0..MAXSIZE_EXTENDED-1] of Extended;

	PCompArray = ^TCompArray;
	TCompArray = array[0..MAXSIZE_COMP-1] of Comp;

	PCurrencyArray = ^TCurrencyArray;
	TCurrencyArray = array[0..MAXSIZE_CURRENCY-1] of Currency;

	PDateTimeArray = ^TDateTimeArray;
	TDateTimeArray = array[0..MAXSIZE_DATETIME-1] of TDateTime;

	PMethodArray = ^TMethodArray;
	TMethodArray = array[0..MAXSIZE_METHOD-1] of TMethod;

	PVariantArray = ^TVariantArray;
	TVariantArray = array[0..MAXSIZE_VARIANT-1] of Variant;

	PVarDataArray = ^TVarDataArray;
	TVarDataArray = array[0..MAXSIZE_VARDATA-1] of TVarData;

	PVariantArgArray = ^TVariantArgArray;
	TVariantArgArray = array[0..MAXSIZE_VARIANTARG-1] of TVariantArg;

	POleVariantArray = ^TOleVariantArray;
	TOleVariantArray = array[0..MAXSIZE_OLEVARIANT-1] of OleVariant;

	PCharArray = ^TCharArray;
	TCharArray = array[0..MAXSIZE_CHAR-1] of Char;

	PWideCharArray = ^TWideCharArray;
	TWideCharArray = array[0..MAXSIZE_WIDECHAR-1] of WideChar;

	PObjectArray = ^TObjectArray;
	TObjectArray = array[0..MAXSIZE_OBJECT-1] of TObject;

	PClassArray = ^TClassArray;
	TClassArray = array[0..MAXSIZE_CLASS-1] of TClass;

	PKBitArray = ^TKBitArray;
	TKBitArray = array[0..MAXSIZE_BIT-1] of TKBitSet;

  TKObjectOwnerShip = ( oosReference, oosOwned );
    
{
--------------------------------------------------------------------------------
--------------------------- Resource Support types -----------------------------
--------------------------------------------------------------------------------
}

	PKFileGroupItem = ^TKFileGroupItem;
	TKFileGroupItem = packed record
		bWidth: Byte;
		bHeight: Byte;
		bColorCount: Byte;
		bReserved: Byte;
		Other: packed record
			case Boolean of
			True: (
				wPlanes: Word;            { icon }
				wBitCount: Word;);
			False: (
				HotSpot: TSmallPoint;);   { cursor }
		end;
		dwSize: DWord;                { item size in bytes }
		dwOffset: DWord;              { start of item in the file }
	end;

	PKFileGroupHeader = ^TKFileGroupHeader;
	TKFileGroupHeader = packed record
		wReserved: Word;
		wType: Word; { 1 for icons, 2 for cursors }
		wCount: Word;
		Items: array[0..High( Word )] of TKFileGroupItem;
{ Items: array[1..wCount] of TFileGroupItem; }
	end;

	PKGroupItem = ^TKGroupItem;
	TKGroupItem = packed record
		Size: packed record
			case Boolean of
				True: (
					bWidth: Byte;             { icon size }
					bHeight: Byte;
					bColorCount: Byte;
					bReserved: Byte;);
				False: (
					wWidth: Word;             { cursor size }
					wHeight: Word;);
		end;
		wPlanes: Word;
		wBitCount: Word;
		dwBytesInRes: DWord;
		wNameOrdinal: Word;
	end;

	PKGroupHeader = ^TKGroupHeader;
	TKGroupHeader = packed record
		wReserved: Word;
		wType: Word; { 1 for icons, 2 for cursors }
		wCount: Word;
		Items: array[0..High( Word )] of TKGroupItem;
{ Items: array[1..wCount] of TGroupItem; }
	end;

{
--------------------------------------------------------------------------------
---------------------------- Generic string types ------------------------------
--------------------------------------------------------------------------------
}

	TFieldName   = string[MAXLEN_FIELDNAME];
	TCompanyName = string[MAXLEN_COMPANYNAME];
	TUserName    = string[MAXLEN_USERNAME];
	TPascalID    = string[MAXLEN_PASCALID];
	TStrMsg      = string[MAXLEN_STRMSG];

{
--------------------------------------------------------------------------------
----------------------- Generic Window/Processes Types -------------------------
--------------------------------------------------------------------------------
}

implementation

uses
	uksyUtils; { do not remove! force uksyUtils to be registered in unit running list }

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	Include( CHARSETS[ceFloat], DecimalSeparator );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
