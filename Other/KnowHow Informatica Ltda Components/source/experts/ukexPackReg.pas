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

unit ukexPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestExpertShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukexUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterExpertPackages;
begin
	if ( not IsExpert_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perExperts, $14F7DCDD ); { do not const... }
		UnregisterKnowHowPackage( pedExperts, $8D591231 );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestExpertShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perExperts ) ) then
		RegisterKnowHowPackage( perExperts, $14F7DCDD ); { do not const... }
	if ( not CheckPackageRegistered( pedExperts ) ) then
		RegisterKnowHowPackage( pedExperts, $8D591231 );
	Registered := True;
{$ENDIF}
	if ( IsExpert_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perExperts )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perExperts ) ) then
			RegisterKnowHowPackage( perExperts, $14F7DCDD ); { do not const... }
		if ( not CheckPackageRegistered( pedExperts ) ) then
			RegisterKnowHowPackage( pedExperts, $8D591231 );
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
	UnregisterExpertPackages;
end;

initialization
	Init;

finalization
	Done;

end.
