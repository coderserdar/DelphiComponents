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

unit ukbcPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestBarCodeShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukbcUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterBarCodePackages;
begin
	if ( not IsBarCode_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perBarCode, $58D773AB ); { do not const... }
		UnregisterKnowHowPackage( pedBarCode, $E51AAF7F );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestBarCodeShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perBarCode ) ) then
		RegisterKnowHowPackage( perBarCode, $58D773AB ); { do not const... }
	if ( not CheckPackageRegistered( pedBarCode ) ) then
		RegisterKnowHowPackage( pedBarCode, $E51AAF7F );
	Registered := True;
{$ENDIF}
	if ( IsBarCode_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perBarCode )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perBarCode ) ) then
			RegisterKnowHowPackage( perBarCode, $58D773AB ); { do not const... }
		if ( not CheckPackageRegistered( pedBarCode ) ) then
			RegisterKnowHowPackage( pedBarCode, $E51AAF7F );
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
	UnregisterBarCodePackages;
end;

initialization
	Init;

finalization
	Done;

end.
