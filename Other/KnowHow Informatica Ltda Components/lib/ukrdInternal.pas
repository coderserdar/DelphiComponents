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

unit ukrdInternal;

interface

procedure Register;

implementation

uses
	DsgnIntf, FiltEdit, ukrUtils, ukrCtrls;

procedure Register;
begin

{ Delphi Hacked Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedFile, 'Filter', TFilterProperty );

end;

end.
