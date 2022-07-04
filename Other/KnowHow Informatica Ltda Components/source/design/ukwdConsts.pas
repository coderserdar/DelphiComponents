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

unit ukwdConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	uksyConsts;

resourcestring

	sRegWinAPI = 'KnowHow WinAPI';

{ Mail Slot }

	sDMSDefaultDomain = '(Current)';
	
{ WinAPI Dialogs Editor Test }

	sWAPIDlgEditorTest = 'Test Dialog';

{ W95 Performance Objects }

	sErrW95InvBaseKey = 'Invalid registry key "%s" for retrieve values';

{ Shell Controls }

	sShellCaption = 'Select a Path';
	sShellAddPath = 'Add Path';
	sShellClear = 'Clear';

const

	PAGE_CONTROL_VERBCOUNT = 3;
	PAGE_CONTROL_VERB: array[0..PAGE_CONTROL_VERBCOUNT - 1] of string[13] =
	 ( 'New Page', 'Next Page', 'Previous Page' );

	WINAPI_DLGEDITOR_VERBCOUNT = 1; 

  SHELL_CONTROLS_VERB_COUNT = 2;

implementation

end.
