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

unit ukwPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestWinAPIShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukwUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterWinAPIPackages;
begin
	if ( not IsWinAPI_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perWinAPI, $DB53C9A0 ); { do not const... }
		UnregisterKnowHowPackage( pedWinAPI, $42C76DD4 );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestWinAPIShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perWinAPI ) ) then
		RegisterKnowHowPackage( perWinAPI, $DB53C9A0 ); { do not const... }
	if ( not CheckPackageRegistered( pedWinAPI ) ) then
		RegisterKnowHowPackage( pedWinAPI, $42C76DD4 );
	Registered := True;
{$ENDIF}
	if ( IsWinAPI_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perWinAPI )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perWinAPI ) ) then
			RegisterKnowHowPackage( perWinAPI, $DB53C9A0 ); { do not const... }
		if ( not CheckPackageRegistered( pedWinAPI ) ) then
			RegisterKnowHowPackage( pedWinAPI, $42C76DD4 );
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
	UnregisterWinAPIPackages;
end;

initialization
	Init;

finalization
	Done;

end.
