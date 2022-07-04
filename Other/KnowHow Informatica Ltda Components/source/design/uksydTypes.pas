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

unit uksydTypes;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	SysUtils, EditIntf, uksyUtils;
	{##NI##}{ do not remove! force uksyUtils to be registered in unit running list }{##NI##}

type

	PSyntaxHighlighter = ^TSyntaxHighlighter;
	PBlockType = ^TBlockType;

	PCharPos = ^TCharPos;
	PEditPos = ^TEditPos;
  
	PResHeaderValue = ^TResHeaderValue;

	TKVarianData = array[0..SizeOf( Variant ) - 1] of Byte;

  PKPropertyValue = ^TKPropertyValue;
	TKPropertyValue = packed record
		case TPropertyType of
			ptUnknown : ( );
			ptInteger,
			ptChar,
			ptEnumeration,
			ptSet,
			ptWChar   : ( IntegerValue: LongInt );
			ptFloat   : ( FloatValue: Extended );
			ptMethod  : ( MethodValue: TMethod );
			ptClass,
			ptString,
			ptLString,
			ptLWString: ( PtrValue: Pointer );
			ptVariant : ( VariantValue: TKVarianData );
		end;

implementation

end.
