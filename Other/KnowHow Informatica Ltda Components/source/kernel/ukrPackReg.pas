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

unit ukrPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestKernelShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukrUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterKernelPackages;
begin
	if ( not IsKernel_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perKernel, $5417668B ); { do not const... }
		UnregisterKnowHowPackage( pedKernel, $CD61A41F );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestKernelShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perKernel ) ) then
		RegisterKnowHowPackage( perKernel, $5417668B ); { do not const... }
	if ( not CheckPackageRegistered( pedKernel ) ) then
		RegisterKnowHowPackage( pedKernel, $CD61A41F );
	Registered := True;
{$ENDIF}
	if ( IsKernel_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perKernel )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perKernel ) ) then
			RegisterKnowHowPackage( perKernel, $5417668B ); { do not const... }
		if ( not CheckPackageRegistered( pedKernel ) ) then
			RegisterKnowHowPackage( pedKernel, $CD61A41F );
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
	UnregisterKernelPackages;
end;

initialization
	Init;

finalization
	Done;

end.
