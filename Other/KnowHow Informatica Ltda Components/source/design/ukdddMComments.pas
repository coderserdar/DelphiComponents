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

unit ukdddMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{-------------------------- TKDataDialog Comments ------------------------------}

{
	TKDataDialog.OnOkClick: TNotifyEvent;
}

	sComDataDlgOk = '';

{
	TKDataDialog.OnCancel: TNotifyEvent;
}

	sComDataDlgCancel = '';

{
	TKDataDialog.OnLoad: TNotifyEvent;
}

	sComDataDlgLoad = '';

{
	TKDataDialog.OnSave: TNotifyEvent;
}

	sComDataDlgSave = '';

{
	TKDataDialog.OnClear: TNotifyEvent;
}

	sComDataDlgClear = '';

{
	TKDataDialog.OnRevert: TNotifyEvent;
}

	sComDataDlgRevert = '';

{------------------------ TKDBLocateDialog Comments ----------------------------}

{
	TKDBLocateDialog.BeforeLocate: TKDBLocateDialogEvent = procedure( Sender: TKDBLocateDialog;
	 const DataSet: TDateSet; const Fields: string; Values: Variant ) of object;
}

	sComDBLocBeforeLoc = '';

{
	TKDBLocateDialog.AfterLocate: TKDBLocateDialogEvent = procedure( Sender: TKDBLocateDialog;
		const DataSet: TDateSet; const Fields: string; Values: Variant ) of object;
}

	sComDBLocAfterLoc = '';


implementation

end.
