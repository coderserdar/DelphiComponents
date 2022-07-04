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

unit ukbcdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
  Classes, ukbcdConsts, ukbcUtils, ukbcCtrls, ukbcDBCtrls;

procedure Register;
begin
	RegisterComponents( sBCReg, [ TKBarCode, TKDBBarcode{,
		TKQRBarcode - the code is free, so the user can register by itself...} ] );
    
end;

end.
 