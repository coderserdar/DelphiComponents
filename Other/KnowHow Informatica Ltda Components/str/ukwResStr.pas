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

unit ukwResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{------------------------------- ukwClasses.pas --------------------------------}

{ MailSlot }
	sErrMSInvDataType = 'MailSlot error: invalid datagram data type';
	sErrMSUnableWrite = 'MailSlot error: could not write to MailSlot (%s)';
	sErrMSUnableClose = 'MailSlot error: could not close MailSlot';
	sErrCMSInvActiveOp = 'MailSlot error: cannot perform this operation on an active MailSlot';
	sErrCMSReadingError = 'MailSlot error: could not read from MailSlot';
	sErrMSInvHeaderData = 'MailSlot error: invalid header data';
	sErrCMSClosingError = 'MailSlot error: could not close MailSlot';
	sErrMSBroadCastError = 'MailSlot error: invalid broadcast package type';
	sErrMSUnableRetrieve = 'MailSlot error: could not retrieve MailSlot handle';
	sErrMSInvDatagramSize = 'MailSlot error: invalid datagram size (%d)';
	sErrCMSReadingErrorEx = 'MailSlot error: could not read from MailSlot (%d - %s)';
	sErrMSInvBroadCastAck = 'MailSlot error: ACK datagrams cannot be broadcast';

{ Application Control }
	sErrAPPOnlyOne = 'AppControl error: only one TKAppControl instance is allowed per application';
	sErrAPPIllegalRename = 'Fatal Error: This program has been illegally renamed. Application terminated';
	sErrAPPInvSimultInstances = 'Fatal Error: The number of simultaneous instances allowed (%d) has been reached. (%d) remote instances detected. (%d) local instances detected. Application terminated';

	APPCTRL_TITLE = 'Title';
	APPCTRL_ROOTDIR = 'RootDir';
	APPCTRL_LASTUSER = 'LastUser';
	APPCTRL_LASTUSED = 'LastUsed';
	APPCTRL_BUILTNAME = 'BuiltName';
	APPCTRL_INTERNALID = 'InternalID';

{ W95 Performance Objects }
	sErrW95PerfInvEnable = 'W95Performance error: to enable performance object/counter (%s), either the object or counter must be set';
	sErrW95InvObjectName  = 'W95Performance error: invalid performance object name (%s)';
	sErrW95InvCounterName = 'W95Performance error: invalid performance counter name (%s)';
	sErrW95InvCounterInfos = 'W95Performance error: invalid performance counter information (counter %s)';
	sErrW95PerfInvRunTimeProp = 'W95Performance error: property cannot be set at runtime';

{ Generic }
	sErrInvEventCreate = 'CreateEvent error: %s';
	sErrInvMutexCreate = 'CreateMutex error: %s';
	sErrInvThreadCreate = 'CreateThread error: %s';

{ FileMap }
	sErrFMInvAvail = 'FileMap error: no active FileMap object available';
	sErrFMInvCreate = 'FileMap error: could not create FileMap object';
	sErrFMInvOperation = 'FileMap error: cannot perform this operation on an active FileMap';

{ Monitor }
	sErrMFailFFCN = 'Monitor error: FindFirstChangeNotification failed- OS error message is "%s"';
	sErrMDirNotExists = 'Monitor error: directory %s does not exist';
	sErrMInvRunningChange = 'Monitor error: cannot change %s property while Monitor is active';
		sMDir = 'Directory';
		sErrMNotifyFilter = 'NotifyFilter';

{ Tray Icon }
	sErrTIInvRemove = 'TrayIcon error: could not remove the application from the icon tray';
	sErrTIInvCreate = 'TrayIcon error: could not place application on the icon tray';

{ Drag and drop }
	sErrDDInvHandle = 'DragDrop error: invalid DragDrop handle';
	sErrDDInvSource = 'DragDrop error: invalid source object class (%s) for default window procedure handling';
	sErrDDInvWndProc = 'DragDrop error: invalid DragDrop window procedure';
	sErrDDInvSourceForHandle = 'DragDrop error: invalid source object for window handle "$%.8x" (this handle may be of a non VCL window)';

{ NetWork Dialog }
	sNetDlgExtErrMsg = 'NetworkDialog error: a network error occurred on provider %s with message "%s"';

{---------------------------------- ukwCtrls.pas -----------------------------}

	 sErrTSUpdateError = 'Error updating TKTabSheet';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

	sErrInvEnumSvcStatus = 'Could not retrieve service status information for service %s';
	sErrEnumSvcObj = 'Could not access EnumService strings objects property';

	sErrProdConsRunning = 'Cannot perform this operation while running';
	sErrProdConsStopped = 'Cannot perform this operation while stopped';

implementation

end.
