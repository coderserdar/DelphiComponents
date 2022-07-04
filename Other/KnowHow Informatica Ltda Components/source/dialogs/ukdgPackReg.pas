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

unit ukdgPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDialogsShareWareVersion;

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukdgUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterDialogsPackages;
begin
	if ( not IsDialogs_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perDialogs, $641D2225 ); { do not const... }
		UnregisterKnowHowPackage( pedDialogs, $99A3D439 );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestDialogsShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perDialogs ) ) then
		RegisterKnowHowPackage( perDialogs, $641D2225 ); { do not const... }
	if ( not CheckPackageRegistered( pedDialogs ) ) then
		RegisterKnowHowPackage( pedDialogs, $99A3D439 );
	Registered := True;
{$ENDIF}
	if ( IsDialogs_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perDialogs )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perDialogs ) ) then
			RegisterKnowHowPackage( perDialogs, $641D2225 ); { do not const... }
		if ( not CheckPackageRegistered( pedDialogs ) ) then
			RegisterKnowHowPackage( pedDialogs, $99A3D439 );
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
	UnregisterDialogsPackages;
end;

initialization
	Init;

finalization
	Done;

end.
