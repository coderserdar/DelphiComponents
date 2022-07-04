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

unit ukePackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestExtShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukeUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterExtPackages;
begin
	if ( not IsExt_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perExt, $210538C5 ); { do not const... }
		UnregisterKnowHowPackage( pedExt, $198881F1 );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestExtShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perExt ) ) then
		RegisterKnowHowPackage( perExt, $210538C5 ); { do not const... }
	if ( not CheckPackageRegistered( pedExt ) ) then
		RegisterKnowHowPackage( pedExt, $198881F1 );
	Registered := True;
{$ENDIF}
	if ( IsExt_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perExt )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perExt ) ) then
			RegisterKnowHowPackage( perExt, $210538C5 ); { do not const... }
		if ( not CheckPackageRegistered( pedExt ) ) then
			RegisterKnowHowPackage( pedExt, $198881F1 );
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
	UnregisterExtPackages;
end;

initialization
	Init;

finalization
	Done;

end.
