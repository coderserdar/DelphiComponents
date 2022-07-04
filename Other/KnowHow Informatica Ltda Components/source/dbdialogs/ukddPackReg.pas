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

unit ukddPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBDialogShareWareVersion;

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukddUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterDBDialogPackages;
begin
	if ( not IsDBDialog_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perDBDialogs, $E5D288D1 ); { do not const... }
		UnregisterKnowHowPackage( pedDBDialogs, $F809D89D );
		Registered := False;                    
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestDBDialogShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perDBDialogs ) ) then
		RegisterKnowHowPackage( perDBDialogs, $E5D288D1 ); { do not const... }
	if ( not CheckPackageRegistered( pedDBDialogs ) ) then
		RegisterKnowHowPackage( pedDBDialogs, $F809D89D );
	Registered := True;
{$ENDIF}
	if ( IsDBDialog_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perDBDialogs )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perDBDialogs ) ) then
			RegisterKnowHowPackage( perDBDialogs, $E5D288D1 ); { do not const... }
		if ( not CheckPackageRegistered( pedDBDialogs ) ) then
			RegisterKnowHowPackage( pedDBDialogs, $F809D89D );
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
	UnregisterDBDialogPackages;
end;

initialization
	Init;

finalization
	Done;

end.
