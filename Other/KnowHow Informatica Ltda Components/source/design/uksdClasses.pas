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

unit uksdClasses;

interface

uses
	uksydClasses;

type

	{ TKGradientProperty }

	TKGradientProperty = class( TKCustomGradientProperty )
	public
		procedure Edit; override;
		
	end;

implementation

uses
{ uses uksCtrls to install the correct EditGradient function handler }
	uksyClasses, uksCtrls;

{ TKGradientProperty }

procedure TKGradientProperty.Edit;
var
	gr: TKGradient;
begin
	gr := TKGradient( GetOrdValue );
	if EditGradient( gr ) then
	begin
		SetOrdValue( LongInt( gr ) ); { ?? }
		Modified;
	end;
end;

end.
 