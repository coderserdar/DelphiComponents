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

unit ukdddReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	Classes, DsgnIntf, uksydClasses, ukddUtils, ukddClasses, ukdddConsts,
  ukdddMComments;

procedure Register;
begin
{ Registered Components }
	RegisterComponents( sRegDBDialogs, [TKDataDialog, TKDBSessionListDialog,
		TKDBDriverListDialog, TKAliasListDialog, TKTableListDialog,
		TKStoredProcListDialog, TKTableFieldsDialog, TKTableIndexDialog,
		TKStoredProcFieldsDialog, TKDBLocateDialog] );

{ Method Comments }
  
{-------------------------- TKDataDialog Comments ------------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnOkClick',
		sComDataDlgOk );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnCancel',
		sComDataDlgCancel );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnLoad',
		sComDataDlgLoad );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnSave',
		sComDataDlgSave );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnClear',
		sComDataDlgClear );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDataDialog, 'OnRevert',
		sComDataDlgRevert );

{------------------------ TKDBLocateDialog Comments ----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDBLocateDialogEvent ), TKDBLocateDialog, 'BeforeLocate',
		sComDBLocBeforeLoc );
	RegisterDefaultMethod( TypeInfo( TKDBLocateDialogEvent ), TKDBLocateDialog, 'AfterLocate',
		sComDBLocAfterLoc );

end;

end.
