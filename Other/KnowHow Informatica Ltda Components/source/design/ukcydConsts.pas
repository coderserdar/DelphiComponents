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

unit ukcydConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

	sRegCrypto = 'KnowHow Crypto';

	sSBFEncipher = 'Encip&her';
	sSBFDecipher = '&Decipher';

{
	TKStringBlowFishComp.OnCustomPad: TKStringPadEvent = procedure(
		Sender: TKStringBlowFishComp; Data: Pointer; DataSize: Integer; var PadCount:
		Byte; Padding: Boolean ) of object;
}
	sComStrBFCustomPad =
	'';

const

  STRING_BLOWFISH_VERBCOUNT = 2;	

implementation

end.
 