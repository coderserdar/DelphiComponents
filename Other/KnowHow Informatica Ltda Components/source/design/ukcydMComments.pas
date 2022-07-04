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

unit ukcydMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{---------------------- TKStringBlowFishCypher Comments ------------------------}

{
	TKStringBlowFishCypher.OnCustomPad: TKStringPadEvent = procedure( Sender:
		TKStringBlowFishCypher; Data: Pointer; DataSize: Integer; var PadCount:
		Byte; Padding: Boolean ) of object;
}
	sComStrBFCustomPad = '';

{
	TKStringBlowFishCypher.OnEncipher: TNotifyEvent
}
	sComStrBFEncipher = '';

{
	TKStringBlowFishCypher.OnDecipher: TNotifyEvent
}
	sComStrBFDecipher = '';

implementation

end.
