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

unit ukcyResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{---------------------------- ukcyBlowFish.pas ---------------------------------}

{ BlowFish Custom }
	sErrBFInvDeAlloc = 'Blowfish error: the requested pointer was not allocated by the AllocateOutPut method. Invalid size %d';
	sErrBFDeAllocError = 'Unexpected Blowfish error: error while restoring memory to the OS- OS Error: %.8x';
	sErrBFInvInputSize = 'Invalid Blowfish plaintext size (%d bytes)';
	sErrBFNotInitialized = 'Invalid Blowfish operation: the Blowfish algorithm has not been initialized correctly';

{ PointerBlowFish }
	sErrPBFInvPadType = 'Invalid Blowfish pad type: pad type %s not allowed';

{ StringBlowFish }
	sErrSBFInvNullKey = 'Invalid Blowfish key: the Blowfish key must be a non-empty string';
	sErrSBFKeyTooLong = 'Invalid Blowfish key size: the Blowfish key size must have at most %d bytes';
	sErrSBFKeyTooSmall = 'Invalid Blowfish key size: the Blowfish key size must have at least %d bytes';
	sErrSBFInvNullData = 'Invalid Blowfish plaintext: the Blowfish plaintext must be a non-empty string';
	sErrSBFInvDecipher_szIn = 'Invalid Blowfish ciphertext size: the Blowfish ciphertext size (%d bytes) must be a multiple of %d';

{ PChar BlowFish }
	sErrPCBFInvDeAlloc = 'Blowfish error: a memory block could not be completely freed: original size was %d and the freed size was %d';

{----------------------------- ukcyIntLib.pas ----------------------------------}

{ Multiple Precision Integer }
	sErrIntModZero = 'Integer Library error: mod-0 is an undefined operation';
	sErrIntDivByZero = 'Integer Library error: division by zero is an undefined operation';
	sErrIntNotDefined = 'Integer Library error: the operation 0÷0 is undefined over Z';
	sErrInvInverseModN = 'Integer Library error: b ($%s) has no inverse mod n ($%s)';
	sErrIntPowerBaseZero = 'Integer Library error: zero cannot be base of an exponent';
	sErrInvInverseModnState = 'Unexpected Integer Library error: could not compute result';
	sErrInvInverseModNParams = 'Integer Library error: InverseModN argument n ($%s) must be greater than argument b ($%s)';
	sErrInvInverseModNEqualParams = 'Integer Library error: InverseModN arguments must be different';

{----------------------------- ukcyClasses.pas ---------------------------------}

	sErrInvStrBFValues = 'Blowfish error: operation failed. Use a non-empty key and a valid plaintext or ciphertext';
	sErrInvStrBFBlockPadType = 'Blowfish error: you must implement an OnCustomPad event handler when working with custom BlockPadType';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

implementation

end.
