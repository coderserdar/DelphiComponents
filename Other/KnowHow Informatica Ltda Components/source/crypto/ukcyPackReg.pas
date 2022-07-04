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

unit ukcyPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestCryptoShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukcyUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterCryptoPackages;
begin
	if ( not IsCrypto_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perCrypto, $0BF54D1B ); { do not const... }
		UnregisterKnowHowPackage( pedCrypto, $AEB5512F );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestCryptoShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perCrypto ) ) then
		RegisterKnowHowPackage( perCrypto, $0BF54D1B ); { do not const... }
	if ( not CheckPackageRegistered( pedCrypto ) ) then
		RegisterKnowHowPackage( pedCrypto, $AEB5512F );
	Registered := True;
{$ENDIF}
	if ( IsCrypto_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perCrypto )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perCrypto ) ) then
			RegisterKnowHowPackage( perCrypto, $0BF54D1B ); { do not const... }
		if ( not CheckPackageRegistered( pedCrypto ) ) then
			RegisterKnowHowPackage( pedCrypto, $AEB5512F );
		Registered := True;
	end;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
end;

procedure Done;
begin
	UnregisterCryptoPackages;
end;

initialization
	Init;

finalization
	Done;
	
end.
