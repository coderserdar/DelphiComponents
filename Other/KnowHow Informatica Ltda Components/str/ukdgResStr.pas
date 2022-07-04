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

unit ukdgResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

	sCapLogon = '&Logon';
	sCapPassword = '&Password';
	sCapPrint = '&Print';
	sCapPreview = '&Preview';

	sCapSource = 'Source: ';
	sCapDestination = 'Destination: ';
	sCapBackup = '&Backup';
	sCapRestore = '&Restore';
	sCapEncipher = '&Encipher';
	sCapDecipher = '&Decipher';

	sLogonDialog = 'Logon Dialog';
	sLogonRetry = 'Invalid logon. Try again.';
	sLogonLocked = 'Invalid logon. Access to the system has been locked.';


	sCapNewPassword = '&New Password';
	sCapConfirmPassword = '&Confirm Password';
	sCapChangePassword = 'Change Password Dialog';

	sPasswordRetry = 'Invalid password change. Try again.';
	sPasswordLocked = 'Password change failed. Password change feature has been locked.';
	sPasswordMismatch = 'Could not confirm password: new and confirmed passwords differ.';

	sCapSourceList = '&Source';
	sCapDestList = '&Destination';

	sSrcListHint = 'List of the available items';
	sDstListHint = 'List of the selected items';

	sErrDLEInvSelection = 'Cannot add a list item value to a unselected item';
	sErrDLEInvSourceList = 'Cannot add a source list item with a value field';

	sErrInvDlgExecuting =	'Could not perform this operation. Not in execute state';
	sErrInvDlgChecking = 'Could not perform this operation. Not in check state';
	sErrInvActionDlgCancelOp = 'Could not perform the cancel operation';

implementation

{

	sDSelFolder = 'Select a Folder';end.
	sDYes = 'Yes';  
	sDNo = 'No';  
	sDWarning = 'Warning !';  
	sDBug = 'Bug Report !';  
	sDError = 'Error Information';  
	sDUserN = 'User Name';
	sDInvUserN = 'Invalid user name';  
	sDNoErrorFeedBack = 'You must provide some feedback on the error condition';
	sDAll = 'All';  
	sDGreater = 'Greater than...';  
	sDLower = 'Lower than...';  
	sDEqual = 'Equal to...';
	sDContained = 'Caontained...';
	sDDifferentFrom = 'Diferrent from...';
	sDCriteria = 'Criteria';
	sDValue = 'Value';
	sDOpen = 'Open...';
	sDClear = 'Clear';
	sDRevert = 'Revert';
	sDAjust = 'Adjust';
	sDTxtFilter = 'Text Files (*.txt)|*.TXT|All Files (*.*)|*.*';
	sDBmpFilter = 'BitMap Files (*.bmp)|*.BMP|JPEG Files (*.jpg)|*.JPG|All Files(*.*)|*.*';
}

end.
