unit mcConst;

interface

uses Windows, SysUtils, Forms, Messages, CommDlg, DsgnIntf;

{************* Common Constants, Types and Variables *****************}
type
	TSearchOption = (soForward,	soGlobal,	soFromCursor);
	TSearchOptions = set of TSearchOption;

	TmcRegState				=	(rsClear, rsRead, rsWrite);
	TmcRegValue				= (rvEnviro, rvFiles, rvPrefs);

	TCellType				= (ctShape, ctQRShape);
	TFontsListType	= (fltFonts, fltSize, fltStyle);
	TEncryptAction	= (eaEncode, eaDecode);
	TPercentAction	= (paAdd, paSubtract);
	TSortItems			= (siUnsort, siSort);		// Sort string items in a string list
	THandleType			= (htHandle, htIcon, htDC);
	TIconAction			= (iaAdd, iaRemove);
	TTableState			= (tsBrowse, tsEdit, tsEditModes, tsInsert);
	TShutDownAction	=	(saLogOff, saPowerOff, saReboot, saShutDown);
	TKBTypeFlag			= (kbfType, kbfSubType, kbfFuncKeys);
	TSizeStrType		= (ssType1, ssType2);
	TDeviceIndex		= (diPlotter, diDisplay, diPrinter, diCamera, diStream, diMetaFile, diFile);

{************* APITools Constants, Types and Variables *****************}
var
	FontsListType: TFontsListType;

const
	shell32 = 'shell32.dll';
	// AdjustTokenPrivilege constants
	SE_SHUTDOWN_NAME				= 'SeShutdownPrivilege';

// ShellExecute() and ShellExecuteEx() error codes

// regular WinExec() codes
	SE_ERR_FNF							= 2;       // file not found
	SE_ERR_PNF              = 3;       // path not found
	SE_ERR_ACCESSDENIED     = 5;       // access denied
	SE_ERR_OOM              = 8;       // out of memory
	SE_ERR_DLLNOTFOUND      = 32;

// error values for ShellExecute() beyond the regular WinExec() codes
	SE_ERR_SHARE            = 26;
	SE_ERR_ASSOCINCOMPLETE	= 27;
	SE_ERR_DDETIMEOUT       = 28;
	SE_ERR_DDEFAIL          = 29;
	SE_ERR_DDEBUSY     			= 30;
	SE_ERR_NOASSOC          = 31;

// Note CLASSKEY overrides CLASSNAME
	SEE_MASK_CLASSNAME      = $00000001;
	SEE_MASK_CLASSKEY       = $00000003;
// Note INVOKEIDLIST overrides IDLIST
	SEE_MASK_IDLIST         = $00000004;
	SEE_MASK_INVOKEIDLIST   = $0000000C;
	SEE_MASK_ICON           = $00000010;
	SEE_MASK_HOTKEY         = $00000020;
	SEE_MASK_NOCLOSEPROCESS = $00000040;
	SEE_MASK_CONNECTNETDRV  = $00000080;
	SEE_MASK_FLAG_DDEWAIT   = $00000100;
	SEE_MASK_DOENVSUBST     = $00000200;
	SEE_MASK_FLAG_NO_UI     = $00000400;
	SEE_MASK_UNICODE        = $00004000;
	SEE_MASK_NO_CONSOLE     = $00008000;
	SEE_MASK_ASYNCOK        = $00100000;

	DivKByte	= $00000400;
	DivMByte	= $00100000;
	DivGByte	= $40000000;
	WIN32s		= VER_PLATFORM_WIN32s;				// Win32 on Windows 3.1
	WIN32			= VER_PLATFORM_WIN32_WINDOWS; // Windows 95
	WINNT			= VER_PLATFORM_WIN32_NT;			// Windows NT

	{ Following constants translated from WINNT.H. Requires WINNT >= 4.0 }
	FILE_ATTRIBUTE_COMPRESSED						= $00000800;
	FILE_ATTRIBUTE_OFFLINE							= $00001000;

	PROCESSOR_ARCHITECTURE_INTEL        = 0;
	PROCESSOR_ARCHITECTURE_MIPS         = 1;
	PROCESSOR_ARCHITECTURE_ALPHA        = 2;
  PROCESSOR_ARCHITECTURE_PPC          = 3;
	PROCESSOR_ARCHITECTURE_UNKNOWN      = $FFFF;

	{ Processor feature flags }
	PF_FLOATING_POINT_PRECISION_ERRATA  = 0;
	PF_FLOATING_POINT_EMULATED          = 1;
	PF_COMPARE_EXCHANGE_DOUBLE          = 2;
	PF_MMX_INSTRUCTIONS_AVAILABLE       = 3;


	{ Following constants translated from WINNLS.H. Requires WINNT >= 4.0 }
	LOCALE_FONTSIGNATURE			= 88;		// font signature
	LOCALE_SISO639LANGNAME		= 89;  	// ISO abbreviated language name
	LOCALE_SISO3166CTRYNAME		= 90;		// ISO abbreviated country name

	{ Following constants translated from WINUSER.H. Requires WINNT >= 4.0 }
	SM_MOUSEWHEELPRESENT			= 75;   // Windows NT only: TRUE or nonzero
																		// if a mouse with a wheel is installed;
																		// FALSE, or zero, otherwise

{$ifdef VER90}
	HW_PROFILE_GUIDLEN = 39;
	MAX_PROFILE_LEN = 80;
{$endif}
	
type
	// Hacked from Delphi 4.0 Windows unit for Delphi 2.0
{$ifdef VER90}
	PHWProfileInfoA = ^THWProfileInfoA;
	PHWProfileInfoW = ^THWProfileInfoW;
	PHWProfileInfo = PHWProfileInfoA;
	tagHW_PROFILE_INFOA = packed record
		dwDockInfo: DWORD;
		szHwProfileGuid: packed array[0..HW_PROFILE_GUIDLEN-1] of AnsiChar;
		szHwProfileName: packed array[0..MAX_PROFILE_LEN-1] of AnsiChar;
	end;

	tagHW_PROFILE_INFOW = packed record
		dwDockInfo: DWORD;
		szHwProfileGuid: packed array[0..HW_PROFILE_GUIDLEN-1] of WideChar;
		szHwProfileName: packed array[0..MAX_PROFILE_LEN-1] of WideChar;
	end;

	tagHW_PROFILE_INFO = tagHW_PROFILE_INFOA;
	THWProfileInfoA = tagHW_PROFILE_INFOA;
	THWProfileInfoW = tagHW_PROFILE_INFOW;
	THWProfileInfo = THWProfileInfoA;
	HW_PROFILE_INFOA = tagHW_PROFILE_INFOA;

	HW_PROFILE_INFOW = tagHW_PROFILE_INFOW;

	HW_PROFILE_INFO = HW_PROFILE_INFOA;
{$endif}

	szHwProfGuid= string[HW_PROFILE_GUIDLEN];
	szHwProfName= string[MAX_PROFILE_LEN];
	THintStr= array[0..63] of Char;

	TFileAttributes	= (fatArchive, fatCompressed, fatDirectory, fatHidden,
										 fatNormal, fatOffLine, fatReadOnly, fatSystem, fatTemporary);
	TFileTimeInfo		= (ftiCreated, ftiAccessed, ftiModified);
{$ifdef VER90}
	_GET_FILEEX_INFO_LEVELS	= (GetFileExInfoStandard);
	TGetFileExInfoLevels		= _GET_FILEEX_INFO_LEVELS;
	GET_FILEEX_INFO_LEVELS	= _GET_FILEEX_INFO_LEVELS;
{$endif}

{$ifdef WIN32}
	PShellExecuteInfo= ^TShellExecuteInfo;
	TShellExecuteInfo= record
		cbSize: DWORD;
		fMask: ULONG;
		hWin: HWND;
		lpVerb,
		lpFile,
		lpParameters,
		lpDirectory: LPCSTR;
		nShow: integer;
		hInstApp: LongInt;
		// Optional members
		lpIDList: Pointer;
		lpClass: LPCSTR;
		hkeyClass: HKEY;
		dwHotKey: DWORD;
		hIcon,
		hProcess: THandle;
	end;
{$endif}

	PULargeInteger= ^TULargeInteger;
	TULargeInteger= record
		case integer of
		0:(
			LowPart,
			HighPart: DWORD;
		);
		1:(
		{$ifndef VER90}
			QuadPart: Int64;
		{$else}
			QuadPart: Comp;
		{$endif}
		)
	end;

	PWin32FileAttributeData= ^TWin32FileAttributeData;
	TWin32FileAttributeData= record
		dwFileAttributes: DWORD;
		ftCreationTime,
		ftLastAccessTime,
		ftLastWriteTime: TFileTime;
		nFileSizeHigh,
		nFileSizeLow: DWORD;
	end;

var
	Win32FileAttributeData: TWin32FileAttributeData;
	DataBaseModified: BOOL;

{************* DBSpdBtn Constants, Types and Variables *****************}
type
	TDBSBEnumProperty= class(TEnumProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
	end;

	TDBSpdBtnType= (
		btFirst, btPrior, btNext, btLast, btInsert, btSave, btEdit, btDelete,
		btCancel, btRefresh);

const
	TDBSpdBtnCaption: array[TDBSpdBtnType] of ShortString= (
		'First', 'Prior', 'Next', 'Last', 'Insert',
		'Save',	'Edit', 'Delete', 'Cancel', 'Refresh');
	THintList: array[TDBSpdBtnType] of ShortString= (
		'First record', 'Previous record', 'Next record', 'Last record', 'Insert record',
		'Save edit', 'Edit record', 'Delete record', 'Cancel edit', 'Refresh data');

{************* DBTools Constants, Types and Variables *****************}
type
	TBackupConfirm= (bcNo, bcYes);
	TBackupOptions= (boBackup, boRestore);
	TMarkOption= (moGetMark, moGotoMark);

const
	BackupStr: array[TBackupOptions] of ShortString = ('Backup', 'Restore');
	
	{ ERRCAT_INTEGRITY - more listings in the IDAPI.H file. }
	ERRBASE_INTEGRITY							= $2600; // Integrity Violation
	ERRCODE_KEYVIOL             	= $01;     // Key violation
	ERRCODE_MINVALERR           	= $02;     // Min val check failed
	ERRCODE_MAXVALERR           	= $03;     // Max val check failed
	ERRCODE_REQDERR             	= $04;     // Field value required
	ERRCODE_FORIEGNKEYERR       	=	$05;     // Master record missing
	ERRCODE_DETAILRECORDSEXIST  	= $06;     // Cannot MODIFY or DELETE this Master record
	ERRCODE_MASTERTBLLEVEL      	= $07;     // Master Table Level is incorrect
	ERRCODE_LOOKUPTABLEERR      	= $08;     // Field value out of lookup tbl range
	ERRCODE_LOOKUPTBLOPENERR    	= $09;     // Lookup Table Open failed
	ERRCODE_DETAILTBLOPENERR   		= $0A;    // 0x0a Detail Table Open failed
	ERRCODE_MASTERTBLOPENERR   		= $0B;    // 0x0b Master Table Open failed
	ERRCODE_FIELDISBLANK       		= $0C;    // 0x0c Field is blank
	ERRCODE_MASTEREXISTS       		= $0D;    // 0x0d Master Table exists
	ERRCODE_MASTERTBLOPEN      		=	$0E;    // 0x0e Master Table is open
	ERRCODE_DETAILTABLESEXIST    	= $0F;   	// 0x0f Detail Tables exist ( cannot delete, rename ... )
	ERRCODE_DETAILRECEXISTEMPTY  	= $10;   	// 0x10 Cannot empty because details exist
	ERRCODE_MASTERREFERENCEERR   	= $11;   	// 0x11 Cannot modify while adding self referencing Referential Integrity
	ERRCODE_DETAILTBLOPEN        	= $12;   	// 0x12 Detail Table is opened
	ERRCODE_DEPENDENTSMUSTBEEMPTY	= $13; 		// 0x13 Cannot make a master a detail of another table if its details are not empty !
	ERRCODE_RINTREQINDEX         	= $14;   	// 0x14 Ref. integrity fields must be indexed
	ERRCODE_LINKEDTBLPROTECTED   	= $15;   	// 0x15 Master Table is protected ( requires password to open)
	ERRCODE_FIELDMULTILINKED     	= $16;   	// 0x16 Field has more than one master

{ UNIQUE DBI ERROR CODES }
	DBIERR_KEYVIOL             		= (ERRBASE_INTEGRITY + ERRCODE_KEYVIOL);
	DBIERR_MINVALERR           		= (ERRBASE_INTEGRITY + ERRCODE_MINVALERR);
	DBIERR_MAXVALERR           		= (ERRBASE_INTEGRITY + ERRCODE_MAXVALERR);
	DBIERR_REQDERR             		= (ERRBASE_INTEGRITY + ERRCODE_REQDERR);
	DBIERR_FORIEGNKEYERR       		= (ERRBASE_INTEGRITY + ERRCODE_FORIEGNKEYERR);
	DBIERR_DETAILRECORDSEXIST  		= (ERRBASE_INTEGRITY + ERRCODE_DETAILRECORDSEXIST);
	DBIERR_MASTERTBLLEVEL      		= (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLLEVEL);
	DBIERR_LOOKUPTABLEERR      		= (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTABLEERR);
	DBIERR_LOOKUPTBLOPENERR    		= (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTBLOPENERR);
	DBIERR_DETAILTBLOPENERR    		= (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPENERR);
	DBIERR_MASTERTBLOPENERR    		= (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPENERR);
	DBIERR_FIELDISBLANK        		= (ERRBASE_INTEGRITY + ERRCODE_FIELDISBLANK);
	DBIERR_MASTEREXISTS        		= (ERRBASE_INTEGRITY + ERRCODE_MASTEREXISTS);
	DBIERR_MASTERTBLOPEN       		= (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPEN);
	DBIERR_DETAILTABLESEXIST   		= (ERRBASE_INTEGRITY + ERRCODE_DETAILTABLESEXIST);
	DBIERR_DETAILRECEXISTEMPTY 		= (ERRBASE_INTEGRITY + ERRCODE_DETAILRECEXISTEMPTY);
	DBIERR_MASTERREFERENCEERR  		= (ERRBASE_INTEGRITY + ERRCODE_MASTERREFERENCEERR);
	DBIERR_DETAILTBLOPEN       		= (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPEN);
	DBIERR_DEPENDENTSMUSTBEEMPTY	= (ERRBASE_INTEGRITY + ERRCODE_DEPENDENTSMUSTBEEMPTY);
	DBIERR_RINTREQINDEX        		= (ERRBASE_INTEGRITY + ERRCODE_RINTREQINDEX);
	DBIERR_LINKEDTBLPROTECTED  		= (ERRBASE_INTEGRITY + ERRCODE_LINKEDTBLPROTECTED);
	DBIERR_FIELDMULTILINKED    		= (ERRBASE_INTEGRITY + ERRCODE_FIELDMULTILINKED);

	TDBERR_MISSING_PARAM					=	'%s: Missing or nil parameter';
	TDBERR_OPEN_ERROR							=	'Error opening database tables';
	{ Error strings }
	ERR_EDIT_DATA			 	=	'Cannot perform edit on critical data';
	ERR_INV_EMAIL				=	'Email address not specified';
	ERR_ITEM_USED	 			=	'Invoice item already used';
	ERR_INV_STARTDATE		=	'''Start Date'' date not specified';
	ERR_INV_ENDDATE			=	'''End Date'' not specified';
	ERR_INV_DISK				= 'Invalid disk, disk removed or write protected.';
	ERR_INV_REGNO				=	'Invalid Registration Number';
	ERR_INV_OPS_OR_CABS	=	'No ''Operators'' or ''TaxiCabs'' defined';

	{ Formatted error strings }
	ERR_DB_EXISTS				=	'Database ''%s%s'' already exists';
	ERR_DB_OPENED				=	'Database ''%s'' already open';
	ERR_DELETE_LINKED	 	=	'Cannot delete linked records'#13#13'''%s'' has linked records with ''%s''';
	ERR_DELETE_ACCNTENTS= 'Cannot delete reconcilied account entry';
	ERR_ALLOC_ACCOUNT		=	'Cannot allocate account for ''Open'' invoice';
	ERR_EDIT_LINKED		 	=	'Cannot edit linked records'#13#13'''%s'' has linked records with ''%s''';
	ERR_NOT_INV_ITEM		=	'Invoice item does not belong to ''%s''';
	ERR_INV_PAID				= 'Cannot modify paid invoices [%s]';
	ERR_ITEM_PRINTED		= 'Cannot delete paid invoice items';
	ERR_INV_DB_NAME			=	'Invalid database name ''%s%s''';
	ERR_INV_FIELD_VALUE	=	'Field ''%s'' must have a value';
	ERR_INV_NUMBER		 	=	'%s number not specified';
	ERR_INV_SECURITY_DEV=	'Security device not found.'#13+
		'Please insert ''Security Disk'' into drive ''A'' and then try again.';

	function AddFont(const Path: string): BOOL;
	function CopyFileEx(lpExistingFileName, lpNewFileName: PChar;
		lpProgressRoutine: TFarProc;
		lpData: Pointer;
		pbCancel: BOOL;
		dwCopyFlags: DWORD): BOOL; stdcall;
	function GetDiskFreeSpaceEx(lpDirectoryName: PChar;
		var lpFreeBytesAvailableToCaller: TULargeInteger;
		var lpTotalNumberOfBytes: TULargeInteger;
		var lpTotalNumberOfFreeBytes: TULargeInteger): BOOL; stdcall;
	function GetFileAttributesEx(lpFileName: PChar;
		fInfoLevelId: TGetFileExInfoLevels;
		lpFileInformation: Pointer): BOOL; stdcall;
	function IsProcessorFeaturePresent(ProcessorFeature: DWORD): BOOL; stdcall;
	function ShellExecuteEx(lpExecInfo: PShellExecuteInfo): BOOL; stdcall;
	function GetCurrentHwProfile(var lpHwProfileInfo: THWProfileInfo): BOOL; stdcall;

	function IsApi32FuncExists(const Lib32FileName, Api32FuncName: string;
		var Api32Func: TFarProc): BOOL; stdcall;
	function IsRunning: BOOL;
	function PrintSetupDlg(hWin: THandle): BOOL;
	function RemoveFont(const Path: string): BOOL;

	procedure mcError(const Msg: string); overload;
	procedure mcError(const Msg: string; const Arg: array of const); overload;
	procedure Register;

implementation

type
	// Callback functions
	TFNCopyFileEx =
		function(lpExistingFileName, lpNewFileName: PChar;
			lpProgressRoutine: TFarProc;
			lpData: Pointer;
			pbCancel: BOOL;	dwCopyFlags: DWORD): BOOL; stdcall;
	TFNGetDiskFreeSpaceEx =
		function(lpDirectoryName: PChar;
			var lpFreeBytesAvailableToCaller: TULargeInteger;
			var lpTotalNumberOfBytes: TULargeInteger;
			var lpTotalNumberOfFreeBytes: TULargeInteger): BOOL; stdcall;
	TFNGetFileAttributesEx =
		function(lpFileName: PChar;
			fInfoLevelId: TGetFileExInfoLevels;
			lpFileInformation: Pointer): BOOL; stdcall;
	TFNIsProcessorFeaturePresent=
		function(ProcessorFeature: DWORD): BOOL; stdcall;
	TFNShellExecuteEx=
		function(lpExecInfo: PShellExecuteInfo): BOOL; stdcall;
	TFNGetCurrentHwProfile=
		function(var lpHwProfileInfo: THWProfileInfo): BOOL; stdcall;

var
	_CopyFileEx								: TFNCopyFileEx = nil;
	_GetDiskFreeSpaceEx				: TFNGetDiskFreeSpaceEx = nil;
	_GetFileAttributesEx			: TFNGetFileAttributesEx = nil;
	_IsProcessorFeaturePresent: TFNIsProcessorFeaturePresent = nil;
	_ShellExecuteEx						: TFNShellExecuteEx = nil;
	_GetCurrentHwProfile			: TFNGetCurrentHwProfile = nil;

function TDBSBEnumProperty.GetAttributes: TPropertyAttributes;
begin
	result:= inherited GetAttributes - [paSortList];
end;

procedure mcError(const Msg: string);
begin
	raise Exception.Create(Msg);
end;

procedure mcError(const Msg: string; const Arg: array of const);
begin
	raise Exception.CreateFmt(Msg, Arg);
end;

function AddFont(const Path: string): BOOL;
begin
	result:= AddFontResource(PChar(Path)) <> 0;
	if result then
	SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) else
	result:= BOOL(GetLastError);
end;

{ IsApi32FuncExists dynamically link a DLL and check the function is exists in that library.
	If it is, then calls that function and assigns to a pointer variable - callback function

	Parameters				Meaning
	---------					--------
	LibFileName				Library file name eg. 'KERNEL32.DLL'
	ApiFuncName				API function name in DLL eg 'CopyFileExA'
	ApiFunc						API function callback function eg. @FCopyFileEx

	usage:
		if IsApi32FuncExists('KERNEL32.DLL','CopyFileExA',@FCopyFileEx) then
			.
			.
		else
			.
			.

	//To use this function you have to create few other functions and assign some variables
	//For example, if you want to call CopyFileEx() API function from kernel32.dll,
	//create a callback function as

	type
		TFNCopyFileEx = function(lpExistingFileName, lpNewFileName: PChar;
			lpProgressRoutine: TFarProc; lpData: Pointer; pbCancel: BOOL;
			dwCopyFlags: DWORD): BOOL stdcall;

		function CopyFileEx(lpExistingFileName, lpNewFileName: PChar;
			lpProgressRoutine: TFarProc; lpData: Pointer; pbCancel: BOOL;
			dwCopyFlags: DWORD): BOOL;

	var
		_CopyFileEx: TFNCopyFileEx;

	// after this your job is easy, just implement the function after the implementation
	// section of the unit

	implementation

	function CopyFileEx(lpExistingFileName, lpNewFileName: PChar;
		lpProgressRoutine: TFarProc; lpData: Pointer; pbCancel: BOOL;
		dwCopyFlags: DWORD): BOOL;
	begin
		if IsApi32FuncExists(kernel32,'CopyFileExA',@_CopyFileEx) then
		result:= _CopyFileEx(lpExistingFileName,lpNewFileName,lpProgressRoutine,lpData,
			pbCancel, dwCopyFlags) else
		result:= False; // or link to other DLL
	end;

	// after all you can use CopyFileEx function in a program may be like this;

	function FileCopy(const Src,Dest: string; Overwrite: BOOL): BOOL;
	// in this example Overwrite parameter is a boolean variable, you can make it DWORD
	// if you want to but you have type cast as BOOL(Overwrite) in CopyFile function.
	begin
		// Try to use extended version first...
		result:= CopyFileEx(PChar(Src), PChar(Dest), nil, nil, False, DWORD(Overwrite));
		if not result then	// Nope CopyFileEx() is not supported,
		// Windows 95/OSR 1.0 doesn't support CopyFileEx function so,
		// use good old CopyFile() function...
		result:= CopyFile(PChar(Src), PChar(Dest), Overwrite);
		if not result then
		ShowMessage(GetLastErrorMsg);
	end;

	// FileCopy() function ready to use in an action at anytime, and best of all
	// it supports both version of API file copying functions...
	// That's all folks!

	// For fun, you can create a function that copies multiple files and files with
	// wild arg, etc. or you can try other API functions to create more functional
	// programs with a minimal code... Have fun!
}
function IsApi32FuncExists(const Lib32FileName, Api32FuncName: string;
	var Api32Func: TFarProc): BOOL;
var
	hDLL: THandle;
begin
	result:= False;
	Api32Func:= nil;
	hDLL:= LoadLibrary(PChar(Lib32FileName));
	if hDLL < 32 then		// error loading .DLL
	Exit;		// ??? do not raise exception, coz we want to use another version if possible
	Api32Func:= GetProcAddress(hDLL,PChar(Api32FuncName));
	result:= Api32Func <> nil;
	if (hDLL >= 32) then		// free the library if there is no error
	FreeLibrary(hDLL);
end;

function CopyFileEx(lpExistingFileName, lpNewFileName: PChar; lpProgressRoutine: TFarProc;
	lpData: Pointer; pbCancel: BOOL;	dwCopyFlags: DWORD): BOOL;
begin
	if IsApi32FuncExists(kernel32, 'CopyFileExA', @_CopyFileEx) then
	result:= _CopyFileEx(lpExistingFileName, lpNewFileName, lpProgressRoutine, lpData,
		pbCancel, dwCopyFlags) else
	result:= False;
end;

function GetDiskFreeSpaceEx(lpDirectoryName: PChar;
	var lpFreeBytesAvailableToCaller: TULargeInteger;
	var lpTotalNumberOfBytes: TULargeInteger;
	var lpTotalNumberOfFreeBytes: TULargeInteger): BOOL;
begin
	if IsApi32FuncExists(kernel32, 'GetDiskFreeSpaceExA', @_GetDiskFreeSpaceEx) then
	result:= _GetDiskFreeSpaceEx(lpDirectoryName, lpFreeBytesAvailableToCaller,
		lpTotalNumberOfBytes, lpTotalNumberOfFreeBytes) else
	result:= False;
end;

function GetFileAttributesEx(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
	lpFileInformation: Pointer): BOOL;
begin
	if IsApi32FuncExists(kernel32, 'GetFileAttributesExA', @_GetFileAttributesEx) then
	result:= _GetFileAttributesEx(lpFileName, fInfoLevelId, lpFileInformation) else
	result:= False;
end;

function IsProcessorFeaturePresent(ProcessorFeature: DWORD): BOOL;
begin
	if IsApi32FuncExists(kernel32, 'IsProcessorFeaturePresent', @_IsProcessorFeaturePresent) then
	result:= _IsProcessorFeaturePresent(ProcessorFeature) else
	result:= False;
end;

function ShellExecuteEx(lpExecInfo: PShellExecuteInfo): BOOL;
begin
	if IsApi32FuncExists(Shell32, 'ShellExecuteEx', @_ShellExecuteEx) then
	result:= _ShellExecuteEx(lpExecInfo) else
	result:= False;
end;

function GetCurrentHwProfile(var lpHwProfileInfo: THWProfileInfo): BOOL;
begin
	if IsApi32FuncExists(AdvApi32, 'GetCurrentHwProfileA', @_GetCurrentHwProfile) then
	result:= _GetCurrentHwProfile(lpHwProfileInfo) else
	result:= False;
end;

// function IsRunning checks the existance of the currently running application
// and if it is does not let the run second copy of that application
// To see an example, check APITest.pas project file
function IsRunning: BOOL;
var
	fHandle: HWND;
begin
	// Use application's title to create unique ID, so it doesn't conflict with other
	// applications that uses APITool component and this function
	fHandle:= CreateMutex(
		nil,												// do not inherit the handle
		False,											// no owner
		PChar(Application.Title));	// unique ID
	result:= GetLastError = ERROR_ALREADY_EXISTS;	// is current application running?
	if result then
	begin
		// Make sure close the second object's handle
		if CloseHandle(fHandle) then
		// You can change this message with your own error message...
		mcError('Cannot start more than one copy of %s', [Application.Title]);
	end;
end;

function RemoveFont(const Path: string): BOOL;
begin
	result:= RemoveFontResource(PChar(Path));
	if result then
	SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) else
	result:= BOOL(GetLastError);
end;

function PrintSetupDlg(hWin: THandle): BOOL;
var
	PrntDlg: TPrintDlg;
begin
	with PrntDlg do
	begin
		lStructSize:= SizeOf(TPrintDlg);
		hwndOwner:= hWin;
		hDevMode:= 0;
		hDevNames:= 0;
		hDC:= 0;
		Flags:=
			//PD_ALLPAGES or
			//PD_COLLATE or
			//PD_PAGENUMS;// or
			PD_PRINTSETUP;// or
			//PD_USEDEVMODECOPIESANDCOLLATE;
		nFromPage:= 0;
		nToPage:= 0;
		nMinPage:= 0;
		nMaxPage:= 0;
		nCopies:= 0;
		//hInstance:= 0;
		lCustData:= 0;
		lpfnPrintHook:= nil;
		lpfnSetupHook:= nil;
		lpPrintTemplateName:= nil;
		lpSetupTemplateName:= nil;
		hPrintTemplate:= 0;
		hSetupTemplate:= 0;
	end;
	result:= PrintDlg(PrntDlg);
end;

procedure Register;
begin
	RegisterPropertyEditor(TypeInfo(TDBSpdBtnType),nil,'',TDBSBEnumProperty);
end;

end.


