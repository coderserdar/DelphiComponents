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

unit ukrdConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Messages, uksyConsts;

resourcestring

{--------------------------------- ukrdClasses.pas -----------------------------}

	sErrMultiStrListNotOwned = 'Cannot use the editor in a non Owned StringsArray';

	sMultiStrListFilter = 'Strings Array Archieves (*.mst)|*.mst';
	sMultiStrListTitle = 'Strings Array List FileName';
	sMultiStrListDefExt = '.sal';
	sMultiStrListConfirmClear = 'Confirm clear operation?';
	sOverrideFile = 'Would you like to override the file ''%s''?';
	sMultiStrListConfirmLineDeletion = 'Confirm Strings Array deletion?';

	sCSFormCaption = 'Valid CharSet for ''%s''';

	sErrCSDuplicateEspecialCharSet = 'The character ''%s'' was previously registered.';
	sErrCSInvEspecialCharSet  = 'Invalid or null characters aren''t allowed ("%s").';
	sErrCSInvListBoxID = 'Invalid ListBox ID ''%d'' for Char Set Editor.';

const

{--------------------------------- ukrdClasses.pas -----------------------------}

	FLOAT_FORMATS = '0'#13#10'0.00'#13#10'#.##'#13#10'#,##0.00'#13#10 +
		'#,##0.00;(#,##0.00)'#13#10'#,##0.00;;Zero'#13#10'0.000E+00'#13#10'#.###E-0';
	MAX_CUR_FORMATS = 4;
	CURRENCY_FORMAT = '%s;%s';
	CURRENCY_FORMATS: array[0..MAX_CUR_FORMATS - 1] of string[9] =
		( '%0:s%1:s', '%1:s%0:s', '%0:s %1:s', '%1:s %0:s' );
	CURRENCY_NEG_FORMATS: array[0..MAX_CUR_FORMATS - 1] of string[11] =
		( '(%0:s%1:s)', '(%1:s%0:s)', '(%0:s %1:s)', '(%1:s %0:s)' );

	INTEGER_FORMATS = '%d'#13#10'%2.2d'#13#10'%.2d'#13#10'%.8d'#13#10'%-d';

	DATE_FORMATS = 'dd/mm/yyyy'#13#10'mm/dd/yyyy'#13#10'dd/mm/yy'#13#10'mm/dd/yy'#13#10 +
		'd/m/y'#13#10'm/d/y'#13#10'ddddd'#13#10'dddddd';

	TIME_FORMATS = 'hh:nn'#13#10'hh:nn:ss'#13#10'h:n'#13#10'h:n:s'#13#10't'#13#10'tt';

	DATETIME_FORMATS = 'dd/mm/yyyy hh:nn:ss'#13#10'dd/mm/yyyy hh:nn'#13#10 +
		'mm/dd/yyyy hh:nn'#13#10'mm/dd/yyyy hh:nn:ss'#13#10;

implementation

end.
