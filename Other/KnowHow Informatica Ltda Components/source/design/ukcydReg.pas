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

unit ukcydReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	Classes, DsgnIntf, uksydClasses, ukcyUtils, ukcyClasses, ukcydConsts,
	ukcydMComments, ukcydClasses;

procedure Register;
begin

{ Components }
	RegisterComponents( sRegCrypto, [TKStringBlowFishCypher] );

{ Component Editors }
	RegisterComponentEditor( TKStringBlowFishCypher, TKStringBlowFishEditor );

{ Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKStringBlowFishCypher, 'Key',
		TKStringBlowFishKeyProperty );
	RegisterPropertyEditor( TypeInfo( ukcyClasses.TKBlockPadType ), TKStringBlowFishCypher,
		'BlockPadType', TKStringBlowFishBlockPadTypeProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKStringBlowFishCypher, 'PlaintText',
		TKLongStringProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKStringBlowFishCypher, 'CypherText',
		TKLongStringProperty );

{ Method Comments }

{---------------------- TKStringBlowFishCypher Comments ------------------------}

	RegisterMethod( TypeInfo( TKStringPadEvent ), TKStringBlowFishCypher, 'OnCustomPad',
		sComStrBFCustomPad, TKStringBlowFishCustomPadProperty );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKStringBlowFishCypher, 'OnEncipher',
		sComStrBFEncipher );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKStringBlowFishCypher, 'OnDecipher',
		sComStrBFDecipher );

end;

end.
 