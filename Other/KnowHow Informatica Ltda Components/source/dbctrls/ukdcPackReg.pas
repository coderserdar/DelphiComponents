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

unit ukdcPackReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBCtrlsShareWareVersion;

{##NI##}

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, ukdcUtils;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	Registered: Boolean = False;

procedure UnregisterDBCtrlsPackages;
begin
	if ( not IsDBCtrls_Shareware ) and ( Registered ) then
	begin
		UnregisterKnowHowPackage( perDBCtrls, $9B025547 ); { do not const... }
		UnregisterKnowHowPackage( pedDBCtrls, $A3E861BB );
		Registered := False;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure TestDBCtrlsShareWareVersion;
begin
{$IFDEF PACKREG_TESTING}
	if ( not CheckPackageRegistered( perDBCtrls ) ) then
		RegisterKnowHowPackage( perDBCtrls, $9B025547 ); { do not const... }
	if ( not CheckPackageRegistered( pedDBCtrls ) ) then
		RegisterKnowHowPackage( pedDBCtrls, $A3E861BB );
	Registered := True;
{$ENDIF}
	if ( IsDBCtrls_Shareware ) then
	begin
		if ( not CurrentDelphi32Running ) then
			FatalErrorFmt( sErrShareWare, [GetPackageName( perDBCtrls )] );
	end
	else if ( not Registered ) then
	begin
		if ( not CheckPackageRegistered( perDBCtrls ) ) then
			RegisterKnowHowPackage( perDBCtrls, $9B025547 ); { do not const... }
		if ( not CheckPackageRegistered( pedDBCtrls ) ) then
			RegisterKnowHowPackage( pedDBCtrls, $A3E861BB );
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
	UnregisterDBCtrlsPackages;
end;

initialization
	Init;

finalization
	Done;

end.
