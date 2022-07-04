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

unit uksPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestStdShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, uksUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterStdPackages;
begin
	if ( not IsStd_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perStd, $903B0485 ); { do not const... }
		UnregisterKnowHowPackage( pedStd, $0266F931 );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestStdShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perStd ) ) then
		RegisterKnowHowPackage( perStd, $903B0485 ); { do not const... }
	if ( not CheckPackageRegistered( pedStd ) ) then
		RegisterKnowHowPackage( pedStd, $0266F931 );
	Registered := True;
{$ENDIF}
	if ( IsStd_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perStd )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perStd ) ) then
			RegisterKnowHowPackage( perStd, $903B0485 ); { do not const... }
		if ( not CheckPackageRegistered( pedStd ) ) then
			RegisterKnowHowPackage( pedStd, $0266F931 );
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
	UnregisterStdPackages;
end;

initialization
	Init;

finalization
	Done;

end.
