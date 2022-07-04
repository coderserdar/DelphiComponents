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

unit ukdddUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBDialogDsgnShareWareVersion;

implementation

uses
	uksyResStr, uksyUtils, uksyPackReg, uksydUtils, ukddUtils;

type

	EKDBDialogDUtils = class( EKDDBDialogs );

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBDialogDsgnShareWareVersion;
begin
	if ( IsDBDialog_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKDBDialogDUtils, sErrShareWare, [GetPackageName( pedDBDialogs )] );
end;

end.
