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

unit ukdbPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukdbUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterDBPackages;
begin
	if ( not IsDB_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perDB, $DD34934A ); { do not const... }
		UnregisterKnowHowPackage( pedDB, $3840B95A );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestDBShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perDB ) ) then
		RegisterKnowHowPackage( perDB, $DD34934A ); { do not const... }
	if ( not CheckPackageRegistered( pedDB ) ) then
		RegisterKnowHowPackage( pedDB, $3840B95A );
	Registered := True;
{$ENDIF}
	if ( IsDB_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perDB )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perDB ) ) then
			RegisterKnowHowPackage( perDB, $DD34934A ); { do not const... }
		if ( not CheckPackageRegistered( pedDB ) ) then
			RegisterKnowHowPackage( pedDB, $3840B95A );
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
	UnregisterDBPackages;
end;

initialization
	Init;

finalization
	Done;

end.
