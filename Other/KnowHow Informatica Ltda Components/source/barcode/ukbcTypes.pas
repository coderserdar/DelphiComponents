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

unit ukbcTypes;

{$I s:\v100\include\iKLIB100.inc}

interface

const

	EANCharCount = 10;
	MSICharCount = 10;
	ISBNCharCount = 10;
	Code39CharCount = 43;
	Code25CharCount = 10;
	Code25iCharCount = 10;
	PlesseyCharCount = 10;
	CodabarCharCount = 16;
	Code39eCharCount = 128;
	Code128CharCount = 103;
	
type

	TKCode128WidthChar = '1'..'4';

	TKEANTable = array[0..EANCharCount - 1] of string[1];
	TKEANCodes = array[0..EANCharCount - 1] of string[7];

	TKPlesseyCodes = array[0..MSICharCount - 1] of string[4];

	TKMSITable = array[0..MSICharCount - 1] of string[1];
	TKMSICodes = array[0..MSICharCount - 1] of string[8];

	TKCode25Table = array[0..Code25CharCount - 1] of string[1];
	TKCode25Codes = array[0..Code25CharCount - 1] of string[9];

	TKCode25iTable = array[0..Code25iCharCount - 1] of string[1];
	TKCode25iCodes = array[0..Code25iCharCount - 1] of string[5];

	TKCode39Table = array[0..Code39CharCount - 1] of string[1];
	TKCode39Codes = array[0..Code39CharCount - 1] of string[9];

	TKCode39eTable = array[0..Code39eCharCount - 1] of string[1];
	TKCode39eCodes = array[0..Code39eCharCount - 1] of string[18];

	TKCodabarTable = array[0..CodabarCharCount - 1] of string[1];
	TKCodabarCodes = array[0..CodabarCharCount - 1] of string[7];

	TKCode128Table = array[0..Code128CharCount - 1] of string[2];
	TKCode128Codes = array[0..Code128CharCount - 1] of string[6];

implementation

end.
