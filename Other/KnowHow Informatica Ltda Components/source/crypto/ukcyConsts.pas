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

unit ukcyConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

const

{$IFDEF KLIB100}
	CRYPTO_VER = '1.00';
	CRYPTO_VER_INT = 100;
	CRYPTO_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	CRYPTO_VER = '?.??';
	CRYPTO_VER_INT = 0;
	CRYPTO_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
----------------------------- BlowFish Constants -------------------------------
--------------------------------------------------------------------------------
}

	BC_ERROR   = -1;
	BC_UNKNOWN =  0;
	BC_SUCCESS =  1;

	BF_INVALID_BUFFER_SIZE = 0;

	BF_BLOCK_SIZE = 8; 						// block size is 64 bits;
	BF_MAX_KEY_BLOCK_BYTES = 56;  // max key-length is 448 bits
	BF_MAX_ITERATIONS = 24;
	BF_MIN_ITERATIONS = 8;
	BF_DEF_ITERATIONS = 16;
	BF_DEF_PAD_BYTE = 32;

	BF_MIN_KEY_STRING_LEN = 5;
	BF_MIN_KEY_PChar_LEN = BF_MIN_KEY_STRING_LEN;
	BF_DEF_STRING_PADCHAR = Char( BF_DEF_PAD_BYTE );

{
---------------------------------------------------------------------------------
---------------------- Multiple Precision Integer Constants ---------------------
---------------------------------------------------------------------------------
}

	HEX_TO_OCT: array[0..15] of string[2] =
	(
		'00', '01', '02', '03', '04', '05', '06', '07',
		'10', '11', '12', '13', '14', '15', '16', '17'
	);
	
	HEX_TO_BIN: array[0..15] of string[4] =
	(
		'0000', '0001', '0010', '0011', '0100', '0101', '0110', '0111',
		'1000', '1001', '1010', '1011', '1100', '1101', '1110', '1111'
	);

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetCryptoRegistryInfo: LongInt;

{##NI##}

implementation

uses
	SysUtils, uksyTypes, uksyConsts;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
    Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}

function GetCryptoRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.
