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

unit uksydMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  uksyConsts;

resourcestring

{-------------------------- TKCustomMeasures Comments --------------------------}

{
	TKCustomMeasures.OnGetMeasures: TKGetMeasuresEvent = procedure( Sender: TObject;
		Index: TKScalingFlag; var Value: Single ) of object;

}
	sComGetMeasures = '';

{
	TKCustomMeasures.OnSetMeasures: TKSetMeasuresEvent = procedure( Sender: TObject;
		Index: TKScalingFlag; Value: Single ) of object;

}
	sComSetMeasures = '';

implementation

end.
