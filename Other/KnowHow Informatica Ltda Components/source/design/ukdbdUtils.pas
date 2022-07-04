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

unit ukdbdUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBDsgnShareWareVersion;

{##NI##}

implementation

uses
  uksyResStr, uksyUtils, uksyPackReg, uksydUtils, ukdbUtils;

type
  EKDBDUtils = class( EKDDB );

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDBDsgnShareWareVersion;
begin
	if ( IsDB_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKDBDUtils, sErrShareWare, [GetPackageName( pedDB )] );
end;

end.
 