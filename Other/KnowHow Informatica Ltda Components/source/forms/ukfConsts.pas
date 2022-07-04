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

unit ukfConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  Windows, Messages;

const

	HITTEST_SET = [HTLEFT, HTRIGHT, HTBOTTOM, HTBOTTOMRIGHT,
								 HTBOTTOMLEFT, HTTOP, HTTOPRIGHT, HTTOPLEFT];

  { Unique Message DataBase }

	CM_ABOUT        = WM_USER + $01; 			   { uForms }
	KM_LINKCHILD    = WM_USER + $02;         { uDBChild }
	KM_UPDATEMENU   = WM_USER + $03;			   { uMDI }
	CM_SEPARATOR    = WM_USER + $07;				 { uForms }


implementation

end.
