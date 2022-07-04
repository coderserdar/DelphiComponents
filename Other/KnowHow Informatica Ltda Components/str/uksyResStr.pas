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

unit uksyResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

	sKnowHow = 'KnowHow® Informatica';
	sInternalUser = 'Internal User';
	
{common filters}
	sAllFilter = 'All files (*.*)|*.*';
	sExecutableFilter = 'Executable Files (*.exe; *.com; *.bat)|*.exe;*.com;*.bat|Library Files (*.dll)|*.dll|All files (*.*)|*.*';
	sImageFilter = 'Bitmap Files (*.bmp)|*.bmp|Icon Files (*.ico)|*.ico|Windows MetaFiles (*.wmf; *.emf)|*.wmf;*.emf' +
		'|JPEG Images (*.jpg)|*.jpg|All Image Files (*.bmp; *.ico; *.emf; *.wmf; *.jpg)|*.bmp;*.ico;*.emf;*.wmf;*.jpg|' +
		'All files (*.*)|*.*';
	sDelphiUnitFilter = 'Delphi source files (*.pas)|*.pas';
	sDelphiFormFilter = 'Delphi form files (*.dfm)|*.dfm';
	sDelphiProjectFilter = 'Delphi project files (*.dpr)|*.dpr';
	sDelphiAllFilter = 'Delphi source files (*.pas)|*.pas|Delphi form files (*.dfm)|*.dfm'+
		'Delphi project files (*.dpr)|*.dpr|All files (*.*)|*.*';

{common registry keys}
	sDelphiBaseInfoRegKey = 'Software\Borland\Delphi\'+
		{$IFDEF DELPHI4}'4.0'{$ELSE}
			{$IFDEF DELPHI3}'3.0'{$ELSE}
				{$IFDEF DELPHI2}'2.0'{$ENDIF}
			{$ENDIF}
		{$ENDIF};
	sKnowHowBaseInfoRegKey = 'Software\KnowHow\Delphi\KLib\'
		{$IFDEF KLIB100}+ '100'{$ENDIF};

{-------------------------------- uksyUtils.pas --------------------------------}

	sDelphi32 = 'DELPHI32.EXE';
	sDelphi32MM = 'DELPHIMM.DLL';
	sDelphi32DCC = 'DCC.DLL';
	sDelphi32VCL30 = 'VCL30.DPL';
	sDelphi32VCL40 = 'VCL40.BPL';

	sNil = 'nil';
	sSelForm = 'Select Form';
	sSelComp = 'Select Component';
	sSelControl = 'Select Control';
	sSelFormName = 'Form Name';
	sSelNamePath = 'Object Name Path';
	sSelDataModule = 'Select DataModule';

	sError = 'Error';
	sWarning = 'Warning';
	sFatalError = 'Fatal Error';
	sInformation = 'Information';
	sConfirmation = 'Confirmation';
	sInvalidFileName = 'Invalid file name "%s"';
	sNotImplemented = 'Not yet implemented...';
	sNotImplementedFmt = '%s not yet implemented...';

	sErrorProcessingFile = 'An error occurred while processing file "%s"';
	sVariantConvertError = 'Cannot convert TVarRec (array of const) into a Variant array. (Invalid VType:"%d")';

	sShellFileTypeUnDef = '(value not defined)';
	sErrKnowHowWin32Expt = '%s. OS Error (%.8x): %s';

	sErrInvHexDigit = 'Invalid digit: %s is not a valid hexadecimal digit';
	sErrFindLeak = 'Memory leak: could not clean FindForm structure correctly';
	sErrFindNext = 'Invalid call to FindNextForm- FindFirstForm has never been called';
	sErrFileReset = 'Could not reset file (%s)';

{force errors}
	sErrInvStr = 'Invalid string: empty string';
	sErrInvStrEqual = 'Invalid string match: string "%s" is not equal to string "%s"';
	sErrInvCaseStrEqual = 'Invalid string match: string "%s" is not equal (case sensitive) to string "%s"';
	sErrInvWinNTCall = 'Invalid Windows NT call: this function call is only allowed in the Windows NT environment';
	sErrInvNotWinNT = 'Invalid OS: Windows NT environment expected';
	sErrInvPChar = 'Invalid PChar: PChar is empty';
	sErrInvTrimStr = 'Invalid string: string is either empty or only contains spaces';
	sErrInvStrCharSet = 'Invalid string: string "%s" contains invalid characters';
	sErrInvFile = 'File "%s" does not exist';
	sErrInvPath = 'Path "%s" does not exist';
	sErrInvFileList = 'At least one of the files does not exist';
	sErrInvPathList = 'At least one of the paths does not exist';
	sErrInvDeleteFile = 'File delete error: could not delete existing file "%s"';
	sErrInvWindow = 'Invalid window: handle does not represent a valid window';
	sErrInvHandle = 'Invalid handle: the handle identifier is either NULL or invalid';
	sErrInvWin32LibHandle = 'Invalid Win32 library handle: the handle identifier is either NULL or invalid';
	sErrInvObject = 'Invalid object reference: the object parameter does not represent a valid object reference';
	sErrInvClass = 'Invalid class reference: the class parameter does not represent a valid class reference';
	sErrInvList = 'Invalid %s: the list object is empty';
	sErrInvStream = 'Invalid %s: the stream object is empty';
	sErrInvStrings = 'Invalid %s: the strings object is empty';
	sErrInvCollection = 'Invalid %s: the collection object is empty';
	sErrInvMethod = 'Invalid method reference: the method parameter does not represent a valid method reference';
	sErrInvObjectClass = 'Invalid object class: the object class (%s) is not of the expected type (%s)';
  sErrInvObjectClasses = 'Invalid object class: the object class (%s) is not of the expected type class types';
	sErrInvStreamCopy = 'Stream copy failure: final stream sizes do not match (Source: %s; Target: %s)';
	sErrInvClassReference = 'Invalid class reference: the class reference (%s) is not of the expected type (%s)';
	sErrInvObjectClassName = 'Invalid object class name: the object class name (%s) is not the one expected (%s)';
	sErrInvClassNameReference = 'Invalid class reference: the class reference class name (%s) is not the one expected (%s)';
	sErrInvInterface  = 'Invalid interface reference: the source interface parameter does not represent a valid interface reference';
	sErrInvInterfaceClass = 'Invalid interface class: the source interface class does not implement the specified GUID "%s"';
	sErrInvReference = 'Invalid reference: nil does represent a valid reference';
	sErrInvVarArray = 'Invalid variant array';
	sErrInvVarArrayType = 'Invalid variant array type. VarType $%.4x expected but VarType $%.4x found';
	sErrInvVarToStrCnv = 'Invalid variant array of variant: item %d of %d is not an OleStr';
	sErrInvVarRec = 'Invalid variant record type: VarRecType $%.4x expected';
	sErrInvVarType = 'Invalid variant type: VarType $%.4x expected but VarType $%.4x found';
	sErrInvVariant = 'Invalid variant: either the variant is NULL or EMPTY';
	sErrInvPointer = 'Invalid pointer: nil does represent a valid memory location';
	sErrInvElement = 'Invalid element in array of const at position %d';
	sErrInvStrContains = 'Invalid token: "%s" not found in string "%s"';
	sErrInvStrContainsAny = 'Invalid tokens: none of the tokens were found in string "%s"';
	sErrInvStrContainsAll = 'Invalid tokens: at least one of the tokens was not found in string "%s"';
	sErrInvRegPath = 'Invalid registry path: the path %s from root key %s does not exist';

	sErrInvDrive      = 'Invalid drive identifier "%s"';
	sErrDriveNotReady = 'Drive "%s:" not ready';

	sDriveNotExists = 'Drive does not exist';
	sDriveFloppy    = 'Floppy Drive';
	sDriveHard      = 'Fixed Drive';
	sDriveNet       = 'Network Drive';
	sDriveCD        = 'CD-ROM Unit';

	sProcessSeparator = '----- Process ID = $%.8x Process Name = %s -----';
	sErrInvProcID = 'The specified process ID ($%.8x) is not valid';
	sErrForceProcess = 'The specified process "%s" is not currently running';

	sErrInvPropTypeKind = '%s class invalid property type kind (%s): cannot set property "%s" to the specified value (Address: $%.8x)';
	sErrForceSingleton = 'Duplicate object instance: %s cannot own more than one %s object instance';

	sErrCannotOpenDelphiRegKey = 'Could not open Delphi Base Registry Key. The required version (%s) was not found';
	sErrInstallPackage = 'Could not install Delphi Package "%s" because Delphi is running';
	sErrSetLibPath = 'Could not set the library path because Delphi is running';

	sNTProcSystem = 'System';
	sNTProcAccessDenied = '(no name: access restrictions)';

	sErrFinDivergence = 'Could not find the interest rate after %d iterations with %g tolerance.';

	sErrInvPackageName = 'Invalid package file "%s": either the package does not exist or not is of the expected extension';

{------------------------------- uksyClasses.pas -------------------------------}

	sCapOK     = 'OK';
	sCapNo     = '&No';
	sCapYes    = '&Yes';
	sCapAbort  = '&Abort';
	sCapRetry  = '&Retry';
	sCapCancel = 'Cancel';
	sCapIgnore = '&Ignore';
	sCapNoAll  = '&No to &All';
	sCapYesAll = 'Yes to &All';
	sErrInvGradieBmp = 'Invalid bitmap for gradient';

	sErrMutexCreate = 'Unable to create mutex: OS Error Message "%s"';
	sErrEventCreate = 'Unable to create event: OS Error Message "%s"';
	sErrSemaphoreCreate = 'Unable to create semaphore: OS Error Message "%s"';
	
	sErrLibraryLoaded = 'SetLibrary failed because the library "%s" is already loaded';
	sErrLibraryNoName = 'No library name was specified';
	sErrLibraryLoad = 'Could not load library "%s"';
	sErrLibraryLoadProc = 'Could not load proc "%s.%s"';

	sErrPrnDCInfoInvConstructor = 'Invalid constructor for class %s: use CreateFromPrinter constructor instead';

	sErrColItemNotFound = 'Collection item "%s" not found';
	sErrDuplicateColItemName = 'A collection item named "%s" already exists';

	sErrSingletonClass = 'Duplicate object instance: cannot have more than one %s object instance per application';

	sErrSocketRead = 'Read';
	sErrSocketWrite = 'Write';
	sErrSocketIOError = '%s error %d, %s';
	sErrWinSocketError = 'Windows socket error: %s (%d), on API ''%s''';

{------------------------------ uksyRegCheck.pas -------------------------------}

	sErrShareWare = 'Shareware Version Error: this application was built using KnowHow® Package %s shareware version. Your software provider «MUST» register this software!';

{------------------------------- uksyPackReg.pas -------------------------------}

	sErrReadRootDir = 'Cannot retrieve application root directory';
	sErrReadResName = 'Cannot retrieve information for application';
	sErrWriteResName = 'Cannot register information for application';
	sErrRegPackInvKey = 'Invalid Key. Cannot register KnowHow package "%s"';
	sErrRegPackRegistered = 'Package "%s" has already been registered';
	sErrUnregPackInvKey = 'Invalid Key. Cannot unregister KnowHow package "%s"';
	sErrUnregPackRegistered = 'Package "%s" is not registered';
	sErrRegUnitInvKey = 'Invalid Key. Cannot register KnowHow unit "%s"';
	sErrRegUnitRegistered = 'Unit "%s" has already been registered';
	sErrUnregUnitInvKey = 'Invalid Key. Cannot unregister KnowHow unit "%s"';
	sErrUnregUnitRegistered = 'Unit "%s" is not registered';
	sErrClassNotReg = 'Cannot create a "%s" instace: this class must be internally registered by one of the following KnowHow® packages "%s"';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

  sErrUnknownProperty = 'Property %s does not exist';  { 06/08/1999 }
  sErrUnknownComponent = 'Component %s does not exist';{ 26/01/2000 }
	sErrInvPropPath = 'Invalid property path (%s)';      { 06/08/1999 }

	sErrInvIdent = 'Invalid identifier';                 { 06/08/1999 }

	sErrInvSocket = 'Invalid socket handle: the socket handle identifier is either NULL or invalid'; { 20/09/1999 }

  sErrInvAsyncReqPE = 'Could not call asynchronous db due a invalid parameter'; { 17/02/2000 }
  sErrInvAsyncReqWSE = 'Could not call asynchronous db due a Winsock error message %s'; { 17/02/2000 }
  sErrInvAsyncReqWS = 'Winsock error at asynchronous db request (Handle %d). Winsock Error: %s'; { 24/02/2000 }

  sErrWorkerMsgQueue = 'Could not create the message queue for worker thread. Wait error'; { 21/02/2000 }
  sErrInvPostQuitMessage = 'Could not post a message to a thread (tid: $%.8x) without a message queue'; { 21/02/2000 }



implementation

end.

