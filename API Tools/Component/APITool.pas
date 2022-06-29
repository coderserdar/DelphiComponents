{
******************************************************************************
*                                                                            *
*															APITools V1.0                                  *
*                                                                            *
******************************************************************************
*	Copyright 2002, mcTech - Ural Gunaydin All Rights Reserved.       				 *
*                                                                            *
*		File:       APITool.pas                                                  *
*		Content:    TAPITools unit file                                          *
*                                                                            *
*		Created by Ural Gunaydin                                                 *
*                                                                            *
*		E-mail: al_gun@ncable.net.au                                             *
*                                                                            *
******************************************************************************
* DISCLAIMER:                                                                *
* I will not except any responsibility if APITools cause any damage to your  *
*	system. They are not necessarily will work on every system. I have    		 *
* tested with WIN98 and WIN NT/2000, and got no problem at all. Please take  *
*	a precaution and debug them first. Or run supplied exe file to test if     *
*	everything OK.                                                             *
*																																						 *
*	APITools is free to use, modify or delete! If you mention my name in your  *
*	programs I'll be very happy.                                               *
*	Thank you.                                                                 *
*																																						 *
*	Ural Gunaydin.                                                             *
*																																						 *
******************************************************************************}

unit APITool;

{$A+,Z+}

interface

uses
	Windows, SysUtils, Classes, ShellAPI, Forms, Dialogs, Printers, mcConst;

type
	TAPITools = class(TComponent)
	private
		tabd: TAppBarData;

		fCPUActiveMask,
		fCPUAllocGran,
		fCPUCount,
		fCPUType,
		fCPUOEMId,
		fCPUPageSize: DWORD;
		fCPURevision,
		fCPUArchitecture,
		fCPULevel: WORD;
		fCPUMaxAppAddress,
		fCPUMinAppAddress: Pointer;
		fKBfncKeys,
		fKBType: BYTE;
		fKBTypeStr: string;
		fVerBuild,
		fVerMajor,
		fVerMinor,
		fVerPlatformID: WORD;
		fVerInfoEx: string;
		fBuf: PChar;

{ The HW_PROFILE_INFO structure contains information about a hardware profile.
	The GetCurrentHwProfile function uses this structure to retrieve the current
	hardware profile for the local computer. }
		fHwProfileInfo: THwProfileInfo;

{ A set of bit flags that indicate the docking state of the computer.
	This member can be a combination of the following values.

	Value										Meaning
	DOCKINFO_DOCKED					The computer is docked. This flag is always set for desktop systems
													that cannot be undocked.
	DOCKINFO_UNDOCKED				The computer is undocked. This flag is always set for desktop systems
													that cannot be undocked.
	DOCKINFO_USER_SUPPLIED	If this flag is set, GetCurrentHwProfile retrieved the current
													docking state from information provided by the user in the
													Hardware Profiles page of the System control panel application.
													Currently, Windows NT and Windows 95 are not able to detect the
													docking state. Consequently, this flag is always set.
	DOCKINFO_USER_DOCKED		The computer is docked, according to information provided by
													the user. This value is a combination of the DOCKINFO_USER_SUPPLIED
													and DOCKINFO_DOCKED flags.
	DOCKINFO_USER_UNDOCKED	The computer is undocked, according to information provided by
													the user. This value is a combination of the DOCKINFO_USER_SUPPLIED
													and DOCKINFO_UNDOCKED flags.
}
		fHwProfDocking: DWORD;

// 	A null-terminated string that contains the globally unique identifier (GUID)
// 	string for the current hardware profile. The string returned by GetCurrentHwProfile
//	encloses the GUID in curly braces {} and includes a null-terminator; for example:
//	"{12340001-4980-1920-6788-123456789012}"
//	You can use this string as a registry subkey under your application's configuration
//	settings key in HKEY_CURRENT_USER. This enables you to store settings for each
//	hardware profile.
		fHwProfGuid: szHwProfGuid;

//	A null-terminated string that contains the display name for the current hardware profile.
		fHwProfName: szHwProfName;

		fMemoryStatus: TMemoryStatus;
		fOSVerInf: TOSVersionInfo;

		function _ExecuteFile(const Action,FileName,Params,DefaultDir: string;
			ShowCmd: Integer): THandle; virtual;
		function _ExecuteFileEx(const Action,FileName,Params,DefaultDir: string;
			ShowCmd: Integer): BOOL; virtual;
		function GetAllDrives: string;
		function GetCompName: string;
		function GetCPInf(Index: integer): string;
		function GetCurrentDrive: string;
		function GetCurrentPath: string;
		function GetDrives(Index: integer): string;
		function GetKBLayout: string;
		function GetErrorMsg: string;
		function GetMemoryStatus(Index: integer): DWORD;
		function GetPlatform: ShortString;
		function GetPlatformName: ShortString;
		function GetUsrName: string;
		function GetWindowsPaths(Index: integer): string;
		procedure GetKeyboardInfo;
		procedure GetSysInfo;
		procedure GetVersionInfo;

	protected
		procedure Notification(Component: TComponent; Operation: TOperation); override;
		function hApplication(HType: THandleType): THandle; virtual;

	public
		VolumeName,
		VolFileSystem: string;
		VolSystemFlags,
		VolSerialNo: DWORD;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AddFont(const Path: string): BOOL;
		function ExecuteFile(const Action, FileName, Params, DefaultDir: string;
			ShowCmd: Integer): BOOL;
		function FileAttribute(const FileName: string; Attributes: TFileAttributes): BOOL;
		function FileCopy(const Src,Dest: string; Overwrite: BOOL): BOOL;
		function FileMove(const Src,Dest: string; Flags: DWORD): BOOL;
		function GetCPUID: string;
		function GetDiskCapacity(Drv: Char; var dskSize, dskFree, dskUsed:
			{$ifndef VER90} Int64 {$else} Comp {$endif}): Boolean;
		function GetDevCaps(DeviceIndex: TDeviceIndex; Flag: integer): integer;
		function GetEnvironmentVar(const Variable: string): string;
		function GetFSize(const FileName: string): DWORD;
		function GetFileTimes(const FileName: string; FileTimeInfo: TFileTimeInfo): string;
		function GetVolumeInfo(const Drv: Char): boolean;
		function LocaleInfo(lcType: DWord): string;
		function LocaleStr(idLocale: integer): string;
		function ProcessorFeature(Feature: DWORD): BOOL;
		function RemoveFont(const Path: string): BOOL;
		function ShortPathName(stPath: string): string;
		function SizeStr(const Msg: string; Num: Double; SizeStrType: TSizeStrType): string;
		// Adds or removes application icon to/from taskbar area
		function TaskBarIcon(const HintStr: THintStr; IconAction: TIconAction): BOOL;
		function GetAppBarState: UINT;

		procedure LoadFonts(const StrLst: TStrings; ListType: TFontsListType;
			const FontFamily: PChar{$ifndef VER90}= nil{$endif});
		procedure FormShow(FormClass: TFormClass; fParent: TObject);
		// Shuts down windows according to ShutDownAction parameter
		// (Log off, Reboot, Power off or Shutdown).
		// if fForce True, ShutDownWindows forces all open applications to close,
		// otherwise signals an error message.
		//
		// **** !!!!! WARNING !!!!! ****
		// Use ShutDownWindows() carefully, with fForce is True, you may loose all
		// unsaved data... so, don't blame me. Save your work first and then use this
		// option...
		procedure ShutDownWindows(const ShutDownAction: TShutDownAction;
			const fForce: boolean);

{ CPU / System properties }
		property CPUActiveMask		: DWORD 	read fCPUActiveMask;		// dwActiveProcessorMask
		property CPUAllocGran			: DWORD 	read fCPUAllocGran;			// dwAllocationGranularity
		property CPUArchitecture	: WORD 		read fCPUArchitecture;	// wProcessorArchitecture
		property CPUCount					: DWORD 	read fCPUCount;					// dwNumberOfProcessors
		property CPULevel					: WORD 		read fCPULevel;					// wProcessorLevel
		property CPURevision			: WORD 		read fCPURevision;			// wProcessorRevision
		property CPUType					: DWORD 	read fCPUType;					// dwProcessorType
		property CPUOEMId					: DWORD 	read fCPUOEMId;					// dwOEMId
		property CPUMaxAppAddress	: Pointer read fCPUMaxAppAddress;	// lpMaximumApplicationAddress
		property CPUMinAppAddress	: Pointer read fCPUMinAppAddress;	// lpMinimumApplicationAddress
		property CPUPageSize			: DWORD 	read fCPUPageSize;			// dwPageSize

{ Disk drive properties }
		property DrivesAll			: string read 				GetAllDrives;
		property DrivesCdRom		:	string index 0 read GetDrives;
		property DrivesFixed		:	string index 1 read GetDrives;
		property DrivesRamDisk	:	string index 2 read GetDrives;
		property DrivesRemote		:	string index 3 read GetDrives;
		property DrivesRemovable:	string index 4 read GetDrives;

{ Environment properties }
		property CompName				: string 	read 					GetCompName;
		property CPAnsi					: string 	index 0 read 	GetCPInf;		// Ansi Code Page
		property CPOEM					: string 	index 1 read 	GetCPInf;		// OEM Code Page
		property CurrentDrive		: string	read 					GetCurrentDrive;
		property CurrentPath		: string	read 					GetCurrentPath;
		property KBfncKeys			: BYTE		read 					fKBfncKeys;
		property KBLayout				: string	read 					GetKBLayout;	// Get keyboard layout
		property KBType					: BYTE		read 					fKBType;
		property KBTypeStr			: string 	read 					fKBTypeStr;
		property UsrName				: string 	read 					GetUsrName;		//	Get system User name
		property WindowsPath		: string 	index 0 read 	GetWindowsPaths;
		property WindowsSysPath	: string 	index 1 read 	GetWindowsPaths;
		property WindowsTempPath: string 	index 2 read 	GetWindowsPaths;

{ Current hardware profiles }
		property HwProfDocking: DWORD read fHwProfDocking;
		property HwProfGuid: szHwProfGuid read fHwProfGuid;
		property HwProfName: szHwProfName read fHwProfName;

{ Memory and Page file properties }
		property MemPageFileAvail	: DWORD index 0 read GetMemoryStatus;	// Available Page file size
		property MemPageFileTotal	: DWORD index 1 read GetMemoryStatus;	// Total Page file size
		property MemPhysicalAvail	:	DWORD index 2 read GetMemoryStatus;	// Available Physical memory size
		property MemPhysicalTotal	:	DWORD index 3 read GetMemoryStatus;	// Total Physical memory size
		property MemVirtualAvail	:	DWORD index 4 read GetMemoryStatus;	// Available Virtual memory file size
		property MemVirtualTotal	:	DWORD index 5 read GetMemoryStatus;	// Total Virtual memory file size

{ Version properties }
		property Platform			: ShortString read GetPlatform;			// Generic platform string
		property PlatformName	: ShortString read GetPlatformName; // Specific platform name
		property VerBuild			:	WORD				read fVerBuild;				// Build number
		property VerMajor			: WORD				read fVerMajor;				// Major version number
		property VerMinor			: WORD				read fVerMinor;				// Minor version number
		property VerPlatformID:	WORD				read fVerPlatformID;	// Platform id
		property VerInfoEx		:	string			read fVerInfoEx;			// version extra information

{ Error properties }
		property ErrorMsg			: string			read GetErrorMsg;

	end;

procedure Register;

implementation

{$R *.RES}

function TAPITools.hApplication(HType: THandleType): THandle;
begin
	result:= 0;
	case HType of
		htHandle: result:= Application.Handle;
		htIcon	:	result:= Application.Icon.Handle;
		htDC		: result:= Application.MainForm.Canvas.Handle;
	end;
end;

function TAPITools.GetCPUID: string;
begin
	case CPUArchitecture of
		PROCESSOR_ARCHITECTURE_INTEL:
		case CPULevel of
			3: result:= 'Intel 80386';
			4: result:= 'Intel 80486';
			else
			result:= 'Intel Pentium';
			{5: result:= 'Intel Pentium';				// ??? check intel site to find out pentium cpu
			6: result:= 'Intel Pentium II';			// ??? level information....
			7: result:= 'Intel Pentium III';
			8: result:= 'Intel Pentium IV - P4';}
		end;
		PROCESSOR_ARCHITECTURE_MIPS:
		case CPULevel of
			0004: result:= 'MIPS R4000';
		end;
		PROCESSOR_ARCHITECTURE_ALPHA:
		case CPULevel of
			21064: result:= 'Alpha 21064';
			21066: result:= 'Alpha 21066';
			21164: result:= 'Alpha 21164';
		end;
		PROCESSOR_ARCHITECTURE_PPC:
		case CPULevel of
			1: result:= 'PPC 601';
			3: result:= 'PPC 603';
			4: result:= 'PPC 604';
			6: result:= 'PPC 603+';
			9: result:= 'PPC 604+';
			20: result:= 'PPC 620';
		end;
		PROCESSOR_ARCHITECTURE_UNKNOWN: result:= 'Unknown';
	end;
end;

// TaskBarIcon - adds or removes current application's icon to/from the taskbar status area.
// Returns TRUE if successful or FALSE otherwise.
// hwnd - handle of the window to receive callback messages
// uID - identifier of the icon
// hicon - handle of the icon to add
// lpszTip - ToolTip text
function TAPITools.TaskBarIcon(const HintStr: THintStr; IconAction: TIconAction): BOOL;
var
	NID: TNotifyIconData;
begin
	with NID do
	begin
		cbSize:= SizeOf(TNotifyIconData);
		Wnd:= hApplication(htHandle);
		uID:= hApplication(htIcon);

		if IconAction = iaAdd then	// rest of the structure members not need to be specified if
		begin						// the icon removed from taskbar...
			uFlags:= (NIF_ICON	or NIF_MESSAGE or NIF_TIP);
			uCallbackMessage:= 0;	// ??? Write a callback routine to show application
														// when icon clicked
														// if anyone excercise this routine, please let me know.
			hIcon:= hApplication(htIcon);
			StrLCopy(szTip, HintStr, SizeOf(HintStr)-1);
			result:= Shell_NotifyIcon(NIM_ADD, @NID);
			if hIcon > 0 then		// if Icon already showing on the taskbar
			DestroyIcon(hIcon);	// delete old Icon...
		end else
		result:= Shell_NotifyIcon(NIM_DELETE, @NID);
	end;
end;

// Plain callback function for LoadFonts function to list available
// system/windows fonts.
function FontCallBack(var LogFont: TLogFont; var Metrics: TNewTextMetric;
		FontType: integer; Data: LongInt): integer; export; {$ifdef WIN32} stdcall; {$endif}
var
	i: integer;
begin
	with TStrings(Data) do
	case FontsListType of
		fltFonts: Add(StrPas(LogFont.lfFaceName));
		fltSize:
		begin
			Clear;
			for i:= Metrics.tmAveCharWidth-1 to Metrics.tmMaxCharWidth do
			Add(IntToStr(i));
		end;
		fltStyle:;
	end;
	result:= 1;
end;

procedure TAPITools.LoadFonts(const StrLst: TStrings; ListType: TFontsListType;
	const FontFamily: PChar{$ifndef VER90}= nil{$endif});
begin
	FontsListType:= ListType;
	StrLst.BeginUpdate;
	try
	{$ifdef WIN32}
		// Win32-based applications should use EnumFontFamilies instead of EnumFonts
		EnumFontFamilies(hApplication(htDC), FontFamily, @FontCallBack, LongInt(StrLst));
	{$else}
		EnumFonts(hApplication(htDC), FontFamily, @FontCallBack, PChar(StrLst));
	{$endif}
	finally
		StrLst.EndUpdate;
	end;
end;

{******************************************************************************
 ****************** Private functions and procedures **************************
 ******************************************************************************}
function TAPITools._ExecuteFile(const Action,FileName,Params,DefaultDir: string;
		ShowCmd: Integer): THandle;
var
	ErrMsg: ShortString;
begin
	result:= ShellExecute(hApplication(htHandle), PChar(Action),PChar(FileName),
		PChar(Params),PChar(DefaultDir),ShowCmd);
	if result <= 32 then
	begin
		case result of
			0: ErrMsg:= 'System low on memory or resources';
{0002}ERROR_FILE_NOT_FOUND: ErrMsg:= 'File not found';
{0003}ERROR_PATH_NOT_FOUND:	ErrMsg:= 'Path not found';
{0005}SE_ERR_ACCESSDENIED: ErrMsg:= 'Access denied';
{0008}SE_ERR_OOM: ErrMsg:= 'Out of memory';
{0011}ERROR_BAD_FORMAT: ErrMsg:= 'Invalid .EXE format';
{0027}SE_ERR_ASSOCINCOMPLETE: ErrMsg:= 'Invalid filename association';
{0028}SE_ERR_DDETIMEOUT: ErrMsg:= 'DDE timed out';
{0029}SE_ERR_DDEFAIL: ErrMsg:= 'DDE transaction failed';
{0030}SE_ERR_DDEBUSY: ErrMsg:= 'DDE busy';
{0031}SE_ERR_NOASSOC: ErrMsg:= 'No application associated with the filename extension';
{0032}SE_ERR_DLLNOTFOUND: ErrMsg:= 'DLL (dynamic-link library) not found';
			else
			ErrMsg:= Format('Unknown file execution error [%d]',[result]);
		end;
		ShowMessage('ExecuteFile FAILED. '+ErrMsg);
	end;
end;

function TAPITools._ExecuteFileEx(const Action,FileName,Params,DefaultDir: string;
	ShowCmd: Integer): BOOL;
var
	ExecInfo: TShellExecuteInfo;
	ErrMsg: ShortString;
begin
	with ExecInfo do
	begin
		cbSize:= SizeOf(TShellExecuteInfo);
		hWin:= hApplication(htHandle);
		lpVerb:= PChar(Action);
		lpFile:= PChar(FileName);
		fMask:= SEE_MASK_FLAG_NO_UI;	// Do not display an error message box if an error occurs
		if Params <> '' then
		lpParameters:= PChar(Params) else
		lpParameters:= nil;
		if DefaultDir <> '' then
		lpDirectory:= PChar(DefaultDir)
		else
		lpDirectory:= nil;
		nShow:= ShowCmd;
	end;
	result:= ShellExecuteEx(@ExecInfo);
	if (not result) and (ExecInfo.hInstApp <= 32) then
	begin
		case ExecInfo.hInstApp of
			SE_ERR_FNF: ErrMsg:= 'File not found';
			SE_ERR_PNF: ErrMsg:= 'Path not found';
			SE_ERR_ACCESSDENIED: ErrMsg:= 'Access denied';
			SE_ERR_OOM: ErrMsg:= 'Out of memory';
			SE_ERR_DLLNOTFOUND: ErrMsg:= 'Dynamic-link library not found';
			SE_ERR_SHARE: ErrMsg:= 'Cannot share open file';
			SE_ERR_ASSOCINCOMPLETE: ErrMsg:= 'File association information not complete';
			SE_ERR_DDETIMEOUT: ErrMsg:= 'DDE operation timed out';
			SE_ERR_DDEFAIL: ErrMsg:= 'DDE operation failed';
			SE_ERR_DDEBUSY: ErrMsg:= 'DDE operation busy';
			SE_ERR_NOASSOC: ErrMsg:= 'File association not available';
			else
			ErrMsg:= Format('Unknown file execution error [%d]',[result]);
		end;
		ShowMessage('ExecuteFileEx FAILED. '+ErrMsg);
	end;
end;

function TAPITools.GetAllDrives: string;
begin
	result:= DrivesRemovable+DrivesFixed+DrivesRemote+DrivesCdRom+DrivesRamDisk;
end;

function TAPITools.GetCompName: string;
var
	nSize: DWORD;
begin
	result:= '';
	nSize:= MAX_COMPUTERNAME_LENGTH + 1;	// +1 for null string
	fBuf:= AllocMem(nSize);
	try
		if GetComputerName(fBuf,nSize) then
		result:= StrPas(fBuf) else
		result:= ErrorMsg;
	finally
		FreeMem(fBuf);
	end;
end;

function TAPITools.GetCPInf(Index: integer): string;
var
	CPInfo: TCPInfo;
begin
	result:= '';
	case Index of
		0: result:= LoadStr(GetACP);
		1: result:= LoadStr(GetOEMCP);
	end;
	if (not GetCPInfo(CP_ACP,CPInfo)) or (not GetCPInfo(CP_OEMCP,CPInfo)) then
	result:= ErrorMsg;
end;

function TAPITools.GetCurrentDrive: string;
begin
	result:= CurrentPath[1];
end;

function TAPITools.GetCurrentPath: string;
begin
	result:= ExtractFilePath(ParamStr(0));
end;

function TAPITools.GetDrives(Index: integer): string;
var
	Drive: char;
begin
	result:= '';
	for Drive:= 'A' to 'Z' do
	begin
		case Index of
			0:if GetDriveType(PChar(Drive+':\')) = DRIVE_CDROM then
				result:= result+Drive;
			1:if GetDriveType(PChar(Drive+':\')) = DRIVE_FIXED then
				result:= result+Drive;
			2:if GetDriveType(PChar(Drive+':\')) = DRIVE_RAMDISK then
				result:= result+Drive;
			3:if GetDriveType(PChar(Drive+':\')) = DRIVE_REMOTE then
				result:= result+Drive;
			4:if GetDriveType(PChar(Drive+':\')) = DRIVE_REMOVABLE then
				result:= result+Drive;
		end;
	end;
end;

function TAPITools.GetKBLayout: string;
const
	nSize= KL_NAMELENGTH+1;
begin
	result:= '';
	fBuf:= AllocMem(nSize);
	try
		if GetKeyboardLayoutName(fBuf) then
		result:= StrPas(fBuf) else
		result:= ErrorMsg;
	finally
		FreeMem(fBuf);
	end;
end;

function TAPITools.GetMemoryStatus(Index: integer): DWORD;
begin
	ZeroMemory(@fMemoryStatus,SizeOf(TMemoryStatus));
	fMemoryStatus.dwLength:= SizeOf(TMemoryStatus);
	result:= fMemoryStatus.dwLength;
	if result > 0 then
	begin
		GlobalMemoryStatus(TMemoryStatus(fMemoryStatus));
		case Index of
			0: result:= fMemoryStatus.dwAvailPageFile;
			1: result:= fMemoryStatus.dwTotalPageFile;
			2: result:= fMemoryStatus.dwAvailPhys;
			3: result:= fMemoryStatus.dwTotalPhys;
			4: result:= fMemoryStatus.dwAvailVirtual;
			5: result:= fMemoryStatus.dwTotalVirtual;
		end;
	end else
	mcError('GetMemoryStatus FAILED');
end;

function TAPITools.GetPlatform: ShortString;
begin
	case VerPlatformID of
		WIN32s: result:= 'Windows_32s';
		WIN32	: result:= 'Windows_32';
		WINNT	: result:= 'Windows_NT';
	end;
end;

function TAPITools.GetPlatformName: ShortString;
begin
	result:= '';
	case VerMajor of
		4:
		begin
			case VerMinor of
				0:
				case VerBuild of
					950:	result:= '95';
					1111: result:= '95 SR2';
					1381: result:= 'NT';
					else
					result:= '95 SR2.5';
				end;
				10:
				case VerBuild of
					1525: result:= '97 / 98 (Beta 1) Memphis';
					1998: result:= '98';
					2120: result:= '98 SP1 (Beta 1)';
					2222:	result:= '98 SE';
				end;
				90:
				case VerBuild of
					2380:	result:= 'ME (Beta 1)';
					2419: result:= 'ME (Beta 2)';
					2452: result:= 'ME (Beta 2 refresh)';
					2499: result:= 'ME (Beta 3)';
					2525:	result:= 'ME (RC0, RC1)';
					2535:	result:= 'ME (RC2)';
					3000:	result:= 'ME Gold';
				end;
			end;
		end;
		5:
		begin
			case VerMinor of
				0:
				case VerBuild of
					2031:	result:= '2000 (Beta 3)';
					2128: result:= '2000 (RC2)';
					2183: result:= '2000 (RC3)';
					2195: result:= '2000 Professional';
				end;
				1:
				case VerBuild of
					2200,
					2223,
					2250:	result:= 'Whistler';
					2600:	result:= 'XP';
				end;
			end;
		end;
	end;
	result:= 'Microsoft Windows '+result;
end;

function TAPITools.GetUsrName: string;
var
	nSize: DWORD;
begin
	result:= '';
	nSize:= MAX_PATH;
	fBuf:= AllocMem(nSize);
	try
		if GetUserName(fBuf,nSize) then
		result:= StrPas(fBuf) else
		result:= ErrorMsg;
	finally
		FreeMem(fBuf);
	end;
end;

function TAPITools.GetWindowsPaths(Index: integer): string;
var
	PathLen: WORD;
begin
	PathLen:= MAX_PATH;
	fBuf:= AllocMem(MAX_PATH);
	try
		case Index of
			0: PathLen:= GetWindowsDirectory(fBuf,MAX_PATH);
			1: PathLen:= GetSystemDirectory(fBuf,MAX_PATH);
			2: PathLen:= GetTempPath(MAX_PATH,fBuf);
		end;
		if PathLen > 0 then
		begin
			if fBuf[Length(fBuf)-1] <> '\' then
			StrPCopy(fBuf,fBuf+'\');
			result:= StrPas(fBuf);
		end else
		result:= ErrorMsg;
	finally
		FreeMem(fBuf);
	end;
end;

procedure TAPITools.GetKeyboardInfo;
begin
	fKBfncKeys:= GetKeyboardType(Ord(kbfFuncKeys));
	fKBType:= GetKeyboardType(Ord(kbfType));
	fKBTypeStr:= LoadStr(fKBType);
	{case fKBType of
		1: fKBTypeStr:= 'IBM PC/XT or compatible (83-key) keyboard';
		2: fKBTypeStr:= 'Olivetti "ICO" (102-key) keyboard';
		3: fKBTypeStr:= 'IBM PC/AT (84-key) or similar keyboard';
		4: fKBTypeStr:= 'IBM enhanced (101- or 102-key) keyboard';
		5: fKBTypeStr:= 'Nokia 1050 and similar keyboards';
		6: fKBTypeStr:= 'Nokia 9140 and similar keyboards';
		7: fKBTypeStr:= 'Japanese keyboard';
	end;}
end;

procedure TAPITools.GetSysInfo;
var
	SysInf: TSystemInfo;
begin
	ZeroMemory(@SysInf,Sizeof(TSystemInfo));
	GetSystemInfo(SysInf);
	with SysInf do
	begin
		fCPUOEMId:= dwOEMId;
		fCPUActiveMask:= dwActiveProcessorMask;
		fCPUAllocGran:= dwAllocationGranularity;
		fCPUArchitecture:= wProcessorArchitecture;
		fCPUCount:= dwNumberOfProcessors;
		fCPULevel:= wProcessorLevel;
		fCPURevision:= wProcessorRevision;
		fCPUType:= dwProcessorType;
		fCPUMaxAppAddress:= lpMaximumApplicationAddress;
		fCPUMinAppAddress:= lpMinimumApplicationAddress;
		fCPUPageSize:= dwPageSize;
	end;
end;

procedure TAPITools.GetVersionInfo;
begin
	{ This is necessary to tell GetVersionEx function to handle structure }
	fOSVerInf.dwOSVersionInfoSize:= SizeOf(TOSVersionInfo);
	{ Check if OSVerInfo structure size is big enough to hold version information }
	if not GetVersionEx(fOSVerInf) then
	mcError(ErrorMsg);
	fVerMajor:= fOSVerInf.dwMajorVersion;	// Get major version e.g. 4, 5, etc.
	fVerMinor:= fOSVerInf.dwMinorVersion;	// Get minor version e.g. 5
	fVerPlatformID:= fOSVerInf.dwPlatformID;
	case fVerPlatformID of
		WIN32s: fVerBuild:= 0;			// Windows 95 doesn't have build number
		WIN32: fVerBuild:= LoWord(fOSVerInf.dwBuildNumber); // Get build number for Windows 98
		WINNT: fVerBuild:= fOSVerInf.dwBuildNumber;	// Get build number for Windows NT
	end;
	fVerInfoEx:= fOSVerInf.szCSDVersion;	// Get extra information e.g. Service pack installed...
end;

{******************************************************************************
 ****************** Protected functions and procedures ************************
 ******************************************************************************}
procedure TAPITools.Notification(Component: TComponent; Operation: TOperation);
begin
	inherited Notification(Component,Operation);
	if (Component <> Self) and
		(Component is ClassType) then
	case Operation of
		opInsert:	mcError('Cannot create more than one instance of %s', [ClassName]);
		opRemove:;
	end;
end;

{******************************************************************************
 ****************** Published functions and procedures ************************
 ******************************************************************************}
constructor TAPITools.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	GetSysInfo;
	GetVersionInfo;
	GetKeyboardInfo;
	if GetCurrentHwProfile(fHwProfileInfo) then
	//raise TAPIException.Create('Cannot get hardarwe profile');
	with fHwProfileInfo do
	begin
		fHwProfDocking:= dwDockInfo;
		fHwProfGuid:= szHwProfileGuid;
		fHwProfName:= szHwProfileName;
	end;
end;

destructor TAPITools.Destroy;
begin
	//
	inherited Destroy;
end;

function TAPITools.AddFont(const Path: string): BOOL;
begin
	result:= AddFontResource(PChar(Path)) <> 0;
	if not result then
	result:= BOOL(GetLastError);
end;

function TAPITools.ExecuteFile(const Action, FileName, Params, DefaultDir: string;
 ShowCmd: Integer): BOOL;
begin
	result:= _ExecuteFileEx(Action,FileName,Params,DefaultDir,ShowCmd);
	if not result then
	result:= _ExecuteFile(Action,FileName,Params,DefaultDir,ShowCmd) <= 32;
end;

function TAPITools.FileAttribute(const FileName: string; Attributes: TFileAttributes): BOOL;
var
	fAttr: DWORD;
begin
	ZeroMemory(@Win32FileAttributeData, SizeOf(TWin32FileAttributeData));
	result:=
		GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @Win32FileAttributeData);
	if GetLastError <> NO_ERROR then
	mcError(ErrorMsg);
	if result then
	fAttr:= Win32FileAttributeData.dwFileAttributes	else
	begin
		fAttr:= GetFileAttributes(PChar(FileName));
		if fAttr = $FFFFFFFF then
		mcError(ErrorMsg);
	end;
	case Attributes of
		fatArchive: result:= (fAttr and FILE_ATTRIBUTE_ARCHIVE)= FILE_ATTRIBUTE_ARCHIVE;
		fatCompressed: result:= (fAttr and FILE_ATTRIBUTE_COMPRESSED)= FILE_ATTRIBUTE_COMPRESSED;
		fatDirectory: result:= (fAttr and FILE_ATTRIBUTE_DIRECTORY)= FILE_ATTRIBUTE_DIRECTORY;
		fatHidden: result:= (fAttr and FILE_ATTRIBUTE_HIDDEN)= FILE_ATTRIBUTE_HIDDEN;
		fatNormal: result:= (fAttr and FILE_ATTRIBUTE_NORMAL)= FILE_ATTRIBUTE_NORMAL;
		fatOffLine: result:= (fAttr and FILE_ATTRIBUTE_OFFLINE)= FILE_ATTRIBUTE_OFFLINE;
		fatReadOnly: result:= (fAttr and FILE_ATTRIBUTE_READONLY)= FILE_ATTRIBUTE_READONLY;
		fatSystem: result:= (fAttr and FILE_ATTRIBUTE_SYSTEM)= FILE_ATTRIBUTE_SYSTEM;
		fatTemporary: result:= (fAttr and FILE_ATTRIBUTE_TEMPORARY)= FILE_ATTRIBUTE_TEMPORARY;
	end;
end;

function TAPITools.FileCopy(const Src,Dest: string; Overwrite: BOOL): BOOL;
begin
	//if fVerPlatformID = WINNT then
	result:= CopyFileEx(PChar(Src), PChar(Dest), nil, nil, False, DWORD(Overwrite));
	if not result then
	// Windows 95/OSR 1.0 doesn't support CopyFileEx function
	result:= CopyFile(PChar(Src), PChar(Dest), Overwrite);
	if not result then
	ShowMessage(ErrorMsg);
end;

function TAPITools.FileMove(const Src,Dest: string; Flags: DWORD): BOOL;
begin
	if fVerPlatformID = WINNT then
	result:= MoveFileEx(PChar(Src),PChar(Dest),Flags)	else
	// Windows 95/OSR 1.0 doesn't support MoveFileEx function
	result:= MoveFile(PChar(Src),PChar(Dest));
	if not result then
	ShowMessage(ErrorMsg);
end;

function TAPITools.GetDiskCapacity(Drv: Char; var dskSize, dskFree, dskUsed:
	{$ifndef VER90} Int64 {$else} Comp {$endif}): Boolean;
var
	TmpDrv: string;
	ErrMode: DWORD;
	SectorsPerCluster,
	BytesPerSector,
	NumberOfFreeClusters,
	TotalNumberOfClusters: DWORD;
	SPC,						// Sectors Per Cluster
	BPS,						// Bytes Per Sector
	NFC,						// Number of Free Clusters
	TNC: 						// Total Number of Clusters
	{$ifndef VER90} Int64; {$else} Comp; {$endif}
	dSize,dFree,dUsed: TULargeInteger;
begin
	if Drv = #0 then
	mcError('SetDiskCapacity FAILED!');
	ErrMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
	TmpDrv:= Drv+':\';
	dskSize:= 0;
	dskFree:= 0;
	// Use GetDiskFreeSpaceEx first to see if the system supports WINNT >= 4
	result:= GetDiskFreeSpaceEx(PChar(TmpDrv), dUsed, dSize, dFree);
	if result then
	begin
		dskSize:= dSize.QuadPart;
		dskFree:= dFree.QuadPart;
		dskUsed:= dskSize-dskFree;
	end else
	// Otherwise use good old GetDiskFreeSpace() function and make necessary
	// adjustments to show correct disk capacity information for drives larger than
	// 4.00GB
	begin
		result:= GetDiskFreeSpace(PChar(TmpDrv),SectorsPerCluster,BytesPerSector,
			NumberOfFreeClusters,TotalNumberOfClusters);
		if result then
		begin
			SPC:= SectorsPerCluster;				// Convert LongInt to Double so it can
			NFC:= BytesPerSector;           // hold larger disk capacity otherwise
			TNC:= NumberOfFreeClusters;     // capacity calculation will produce an
			BPS:= TotalNumberOfClusters;    // error ->> Invalid Math Operation.

	{ Or if you do this you'll get Integer Overflow error. Try if you like! }
	// dskSize:= (SectorsPerCluster*BytesPerSector*TotalNumberOfClusters); ->> Integer Overflow
	// dskFree:= (SectorsPerCluster*NumberOfFreeClusters*BytesPerSector);  ->> Integer Overflow

			dskSize:= (SPC*NFC*BPS);
			dskFree:= (SPC*TNC*NFC);
			dskUsed:= dskSize-dskFree;
		end;
	end;
	SetErrorMode(ErrMode);
end;

function TAPITools.GetDevCaps(DeviceIndex: TDeviceIndex; Flag: integer): integer;
var
	hWin: HWND;
begin
	//result:= 0;
	hWin:= 0;
	case DeviceIndex of
		diPlotter:;
		diDisplay: hWin:= GetWindowDC(GetDesktopWindow);
		diPrinter: hWin:= Printer.Handle;
		diCamera:;
		diStream:;
		diMetaFile:;
		diFile:;
	end;
	result:= GetDeviceCaps(hWin, Flag);
	//ReleaseDC(Application.Handle, hDC);
end;

function TAPITools.GetEnvironmentVar(const Variable: string): string;
var
	EnvBuff: PChar;
begin
	result:= '';
	EnvBuff:= AllocMem(MAX_PATH);
	try
		if GetEnvironmentVariable(PChar(Variable),EnvBuff,MAX_PATH) > 0 then
		result:= StrPas(EnvBuff);
	finally
	FreeMem(EnvBuff);
	end;
end;

function TAPITools.GetFSize(const FileName: string): DWORD;
var
	fHnd: THandle;
	fSize,fHighSize: DWORD;
begin
	result:= 0;
	fHnd:= FileOpen(FileName,fmOpenRead or fmShareDenyWrite);
	if fHnd > 0 then
	try
		fSize:= GetFileSize(fHnd,@fHighSize);
		if (fSize = $FFFFFFFF) and (GetLastError <> NO_ERROR) then	// Size > 4 GB?
		result:= HiWord(fHighSize) else
		result:= fSize;
	finally
		FileClose(fHnd);
	end else
	mcError(ErrorMsg);
end;

function TAPITools.GetFileTimes(const FileName: string; FileTimeInfo: TFileTimeInfo): string;
var
	vFileHandle: THandle;
	vFileTime,
	vFileCreated,
	vFileAccessed,
	vFileWritten: TFileTime;
	vSystemTime: TSystemTime;
	vLongDate,vDateSeparator,
	vLongTime,vTimeSeparator: ShortString;
begin
	vFileHandle:= FileOpen(FileName, fmOpenRead or fmShareDenyWrite);
	if vFileHandle > 0 then
	try
		if GetFileTime(vFileHandle, @vFileCreated, @vFileAccessed, @vFileWritten) then
		begin
			case FileTimeInfo of
				ftiCreated	: vFileTime:= vFileCreated;
				ftiAccessed	:	vFileTime:= vFileAccessed;
				ftiModified	:	vFileTime:= vFileWritten;
			end;
			if FileTimeToSystemTime(vFileTime, vSystemTime) then
			with vSystemTime do
			begin
				Inc(wHour, 10);			// force 24 hours mode "0..11 = 12 hours"
				if (wHour > 23) then // Adjust time and day
				begin
					Dec(wHour, 24);
					Inc(wDay);
				end;
				if (wMonth > 12) then	// Adjust Month and Year
				begin
					wMonth:= 1;
					Inc(wYear);
				end;
				// Adjust day depending on the month.
				if (wDay > 28) and (wDay > 29) and (wDay > 30) and (wDay > 31) then
				begin
					wDay:= 1;
					Inc(wMonth);
				end;
				if (wYear < 1980) then	// dates before 1980 are invalid.
				result:= 'Older than 1980' else
				begin
					vLongDate:= 			LocaleInfo(LOCALE_SLONGDATE);
					vDateSeparator:= 	LocaleInfo(LOCALE_SDATE);
					vLongTime:= 			LocaleInfo(LOCALE_STIMEFORMAT);
					vTimeSeparator:= 	LocaleInfo(LOCALE_STIME);

					result:= FormatDateTime(vLongDate, StrToDate(IntToStr(wDay) +
						DateSeparator + IntToStr(wMonth) + DateSeparator + IntToStr(wYear)));

					if (wHour > 0) or (wMinute > 0) or (wSecond > 0) then
					result:= result + ', ' + FormatDateTime(vLongTime, StrToTime(IntToStr(wHour)+
						TimeSeparator + IntToStr(wMinute) + TimeSeparator + IntToStr(wSecond)));
				end;
			end;
		end;
		finally
			FileClose(vFileHandle);
	end;
end;

function TAPITools.GetErrorMsg: string;
begin
	if GetLastError <> NO_ERROR then
	case GetLastError of
{0001}ERROR_INVALID_FUNCTION: result:= 'Incorrect function';
{0002}ERROR_FILE_NOT_FOUND: result:= 'File not found';
{0003}ERROR_PATH_NOT_FOUND: result:= 'Path not found';
{0004}ERROR_TOO_MANY_OPEN_FILES: result:= 'Too many open files';
{0005}ERROR_ACCESS_DENIED: result:= 'Access denied';
{0006}ERROR_INVALID_HANDLE: result:= 'Invalid file handle';
{0007}ERROR_ARENA_TRASHED: result:= 'Control block destroyed';
{0008}ERROR_NOT_ENOUGH_MEMORY: result:= 'Not enough memory';
{0009}ERROR_INVALID_BLOCK: result:= 'Invalid control block address';
{0010}ERROR_BAD_ENVIRONMENT: result:= 'Environment is incorrect';
{0011}ERROR_BAD_FORMAT: result:= 'Invalid file format';
{0012}ERROR_INVALID_ACCESS: result:= 'Invalid file access';
{0013}ERROR_INVALID_DATA: result:= 'Invalid data';
{0014}ERROR_OUTOFMEMORY: result:= 'Out of memory';
{0015}ERROR_INVALID_DRIVE: result:= 'Invalid drive';
{0016}ERROR_CURRENT_DIRECTORY: result:= 'Cannot remove current directory';
{0017}ERROR_NOT_SAME_DEVICE: result:= 'Cannot move across disk drives';
{0018}ERROR_NO_MORE_FILES: result:= 'No more files';
{0019}ERROR_WRITE_PROTECT: result:= 'Media is write protected';
{0020}ERROR_BAD_UNIT: result:= 'Cannot find specified device';
{0021}ERROR_NOT_READY: result:= 'Drive not ready';
{0022}ERROR_BAD_COMMAND: result:= 'Invalid command';
{0023}ERROR_CRC: result:= 'CRC error';
{0024}ERROR_BAD_LENGTH: result:= 'Incorrect command length';
{0025}ERROR_SEEK: result:= 'Seek error';
{0026}ERROR_NOT_DOS_DISK: result:= 'Incorrect disk';
{0027}ERROR_SECTOR_NOT_FOUND: result:= 'Sector not found';
{0028}ERROR_OUT_OF_PAPER: result:= 'Out of paper';
{0029}ERROR_WRITE_FAULT: result:= 'Write fault';
{0030}ERROR_READ_FAULT: result:= 'Read fault';
{0031}ERROR_GEN_FAILURE: result:= 'Generic device failure';
{0032}ERROR_SHARING_VIOLATION: result:= 'Sharing violation';
{0033}ERROR_LOCK_VIOLATION: result:= 'File locked';
{0034}ERROR_WRONG_DISK: result:= 'Wrong disk';
{0036}ERROR_SHARING_BUFFER_EXCEEDED: result:= 'Too many files opened for sharing';
{0038}ERROR_HANDLE_EOF: result:= 'End of file';
{0039}ERROR_HANDLE_DISK_FULL: result:= 'File too big';
{0050}ERROR_NOT_SUPPORTED: result:= 'Network request not supported';
{0051}ERROR_REM_NOT_LIST: result:= 'Remote computer not available';
{0052}ERROR_DUP_NAME: result:= 'Duplicate network name';
{0053}ERROR_BAD_NETPATH: result:= 'Incorrect Network path';
{0054}ERROR_NETWORK_BUSY: result:= 'Network is busy';
{0055}ERROR_DEV_NOT_EXIST: result:= 'Network device not exist';
{0056}ERROR_TOO_MANY_CMDS: result:= 'Too many Network commands';
{0057}ERROR_ADAP_HDW_ERR: result:= 'Network adapter hardware error';
{0058}ERROR_BAD_NET_RESP: result:= 'Cannot perform requested operation';
{0059}ERROR_UNEXP_NET_ERR: result:= 'Unexpected network error';
{0060}ERROR_BAD_REM_ADAP: result:= 'Remote adapter is not compatible';
{0061}ERROR_PRINTQ_FULL: result:= 'Printer queue full';
{0062}ERROR_NO_SPOOL_SPACE: result:= 'Network printer spool full';
{0063}ERROR_PRINT_CANCELLED: result:= 'File deleted from printer queue';
{0064}ERROR_NETNAME_DELETED: result:= 'Network name removed';
{0065}ERROR_NETWORK_ACCESS_DENIED: result:= 'Network access is denied';
{0066}ERROR_BAD_DEV_TYPE: result:= 'Incorrect Network resource';
{0067}ERROR_BAD_NET_NAME: result:= 'Invalid Network name';
{0068}ERROR_TOO_MANY_NAMES: result:= 'Too many Network name';
{0069}ERROR_TOO_MANY_SESS: result:= 'Too many Network session';
{0070}ERROR_SHARING_PAUSED: result:= 'Remote server sharing paused';
{0071}ERROR_REQ_NOT_ACCEP: result:= 'Network access denied';
{0072}ERROR_REDIR_PAUSED: result:= 'Specified printer or disk device paused';
{0080}ERROR_FILE_EXISTS: result:= 'File already exists';
{0082}ERROR_CANNOT_MAKE: result:= 'Cannot create directory or file';
{0083}ERROR_FAIL_I24: result:= 'Fail on INT 24';
{0084}ERROR_OUT_OF_STRUCTURES: result:= 'Too many structures';
{0085}ERROR_ALREADY_ASSIGNED: result:= 'Device already in use';
{0086}ERROR_INVALID_PASSWORD: result:= 'Incorrect password';
{0087}ERROR_INVALID_PARAMETER: result:= 'Invalid parameter';
{0088}ERROR_NET_WRITE_FAULT: result:= 'Network write fault';
{0100}ERROR_TOO_MANY_SEMAPHORES: result:= 'Too many semaphores';
{0101}ERROR_EXCL_SEM_ALREADY_OWNED: result:= 'Exclusive semaphore already owned';
{0102}ERROR_SEM_IS_SET: result:= 'Semaphore already set';
{0103}ERROR_TOO_MANY_SEM_REQUESTS: result:= 'Too many Semaphore request';
{0104}ERROR_INVALID_AT_INTERRUPT_TIME: result:= 'Exclusive Semaphores access denied';
{0105}ERROR_SEM_OWNER_DIED: result:= 'Previous ownership of this semaphore ended';
{0106}ERROR_SEM_USER_LIMIT: result:= 'Semaphore user limit reached';
{0107}ERROR_DISK_CHANGE: result:= 'Incorrect disk inserted';
{0108}ERROR_DRIVE_LOCKED: result:= 'Disk in use or locked';
{0109}ERROR_BROKEN_PIPE: result:= 'Broken pipe connection';
{0110}ERROR_OPEN_FAILED: result:= 'Cannot open specified device or file';
{0111}ERROR_BUFFER_OVERFLOW: result:= 'File name is too long';
{0112}ERROR_DISK_FULL: result:= 'Disk full';
{0113}ERROR_NO_MORE_SEARCH_HANDLES: result:= 'Too many file search handles';
{0114}ERROR_INVALID_TARGET_HANDLE: result:= 'Incorrect target file handle';
{0117}ERROR_INVALID_CATEGORY: result:= 'Invalid IOCTL category';
{0118}ERROR_INVALID_VERIFY_SWITCH: result:= 'Incorrect Verify switch parameter';
{0119}ERROR_BAD_DRIVER_LEVEL: result:= 'Incorrect driver level command';
{0120}ERROR_CALL_NOT_IMPLEMENTED: result:= 'API calls not implemented';
{0121}ERROR_SEM_TIMEOUT: result:= 'Semaphore timeout';
{0122}ERROR_INSUFFICIENT_BUFFER: result:= 'Insufficent buffer';
{0123}ERROR_INVALID_NAME: result:= 'Incorrect file, directory name or volume label';
{0124}ERROR_INVALID_LEVEL: result:= 'Incorrect system call level';
{0125}ERROR_NO_VOLUME_LABEL: result:= 'No volume label';
{0126}ERROR_MOD_NOT_FOUND: result:= 'Cannot find specified module';
{0127}ERROR_PROC_NOT_FOUND: result:= 'Cannot find specified procedure';
{0128}ERROR_WAIT_NO_CHILDREN: result:= 'No child processes to wait for';
{0129}ERROR_CHILD_NOT_COMPLETE: result:= 'Cannot complete child process';
{0130}ERROR_DIRECT_ACCESS_HANDLE: result:= 'Invalid direct access handle';
{0131}ERROR_NEGATIVE_SEEK: result:= 'Invalid seek operation (BOF)';
{0132}ERROR_SEEK_ON_DEVICE: result:= 'Invalid seek operation on device or file';
{0133}ERROR_IS_JOIN_TARGET: result:= 'Cannot JOIN or SUBST for joined drives';
{0134}ERROR_IS_JOINED: result:= 'Cannot JOIN joined drive';
{0135}ERROR_IS_SUBSTED: result:= 'Cannot SUBST substituted drive';
{0136}ERROR_NOT_JOINED: result:= 'Cannot delete non-JOINed drive';
{0137}ERROR_NOT_SUBSTED: result:= 'Cannot delete non-SUBSTed drive';
{0138}ERROR_JOIN_TO_JOIN: result:= 'Cannot join on a joined directory or drive';
{0139}ERROR_SUBST_TO_SUBST: result:= 'Cannot substituted on a substituted directory or drive';
{0140}ERROR_JOIN_TO_SUBST: result:= 'Cannot join on a substituted directory or drive';
{0141}ERROR_SUBST_TO_JOIN: result:= 'Cannot substitute on a joined directory or drive';
{0142}ERROR_BUSY_DRIVE: result:= 'Cannot JOIN or SUBST';
{0143}ERROR_SAME_DRIVE: result:= 'Cannot join or substitute on the same drive';
{0144}ERROR_DIR_NOT_ROOT: result:= 'Directory is not a subdirectory of the root directory';
{0145}ERROR_DIR_NOT_EMPTY: result:= 'Directory is not empty';
{0146}ERROR_IS_SUBST_PATH: result:= 'Path already substituted';
{0147}ERROR_IS_JOIN_PATH: result:= 'Path already joined';
{0148}ERROR_PATH_BUSY: result:= 'Cannot remove or move path';
{0149}ERROR_IS_SUBST_TARGET: result:= 'Target already substituted';
{0150}ERROR_SYSTEM_TRACE: result:= 'System trace information not specified';
{0151}ERROR_INVALID_EVENT_COUNT: result:= 'Incorrect semaphore event count';
{0152}ERROR_TOO_MANY_MUXWAITERS: result:= 'Too many semaphores set';
{0153}ERROR_INVALID_LIST_FORMAT: result:= 'Incorrect list';
{0154}ERROR_LABEL_TOO_LONG: result:= 'Invalid Volume label';
{0155}ERROR_TOO_MANY_TCBS: result:= 'Too many threads';
{0156}ERROR_SIGNAL_REFUSED: result:= 'Recipient refused the signal';
{0157}ERROR_DISCARDED: result:= 'Cannot lock discarded Segment';
{0158}ERROR_NOT_LOCKED: result:= 'Segment already unlocked';
{0159}ERROR_BAD_THREADID_ADDR: result:= 'Incorrect thread ID address';
{0160}ERROR_BAD_ARGUMENTS: result:= 'Incorrect DosExecPgm arguments';
{0170}ERROR_BUSY: result:= 'Requested resource is in use';
{0173}ERROR_CANCEL_VIOLATION: result:= 'Lock request was not outstanding for the supplied cancel region';
{0174}ERROR_ATOMIC_LOCKS_NOT_SUPPORTED: result:= 'File system does not support atomic changing of the lock type';
{0180}ERROR_INVALID_SEGMENT_NUMBER: result:= 'System detected a segment number that is incorrect';
{0182}ERROR_INVALID_ORDINAL: result:= 'Operating system cannot run this application';
{0183}ERROR_ALREADY_EXISTS: result:= 'Attempt to create file that already exists';
{0186}ERROR_INVALID_FLAG_NUMBER: result:= 'Flag passed is incorrect';
{0187}ERROR_SEM_NOT_FOUND: result:= 'Specified system semaphore name not found';
{0188}ERROR_INVALID_STARTING_CODESEG: result:= 'Operating system cannot run this application';
{0189}ERROR_INVALID_STACKSEG: result:= 'Operating system cannot run this application';
{0190}ERROR_INVALID_MODULETYPE: result:= 'Operating system cannot run this application';
{0191}ERROR_INVALID_EXE_SIGNATURE: result:= 'Application cannot be run in Windows/NT mode';
{0192}ERROR_EXE_MARKED_INVALID: result:= 'Invalid .EXE header';
{0193}ERROR_BAD_EXE_FORMAT: result:= 'Program is not a valid Windows-based application';
{0194}ERROR_ITERATED_DATA_EXCEEDS_64k: result:= 'Operating system cannot run this application';
{0195}ERROR_INVALID_MINALLOCSIZE: result:= 'Operating system cannot run this application';
{0196}ERROR_DYNLINK_FROM_INVALID_RING: result:= 'Operating system cannot run this application program';
{0197}ERROR_IOPL_NOT_ENABLED: result:= 'Operating system is not presently configured to run this application';
{0198}ERROR_INVALID_SEGDPL: result:= 'Operating system cannot run this application';
{0199}ERROR_AUTODATASEG_EXCEEDS_64k: result:= 'Operating system cannot run this application program';
{0200}ERROR_RING2SEG_MUST_BE_MOVABLE: result:= 'Code segment cannot be greater than or equal to 64KB';
{0201}ERROR_RELOC_CHAIN_XEEDS_SEGLIM: result:= 'Operating system cannot run this application';
{0202}ERROR_INFLOOP_IN_RELOC_CHAIN: result:= 'Operating system cannot run this application';
{0203}ERROR_ENVVAR_NOT_FOUND: result:= 'System could not find the environment option entered';
{0205}ERROR_NO_SIGNAL_SENT: result:= 'No process in the command subtree has a signal handler';
{0206}ERROR_FILENAME_EXCED_RANGE: result:= 'File name or extension is too long';
{0207}ERROR_RING2_STACK_IN_USE: result:= 'Ring 2 stack is in use';
{0208}ERROR_META_EXPANSION_TOO_LONG: result:= 'Global filename characters * or ? are entered incorrectly';
{0209}ERROR_INVALID_SIGNAL_NUMBER: result:= 'Signal being posted is incorrect';
{0210}ERROR_THREAD_1_INACTIVE: result:= 'Signal handler cannot be set';
{0212}ERROR_LOCKED: result:= 'Segment is locked and cannot be reallocated';
{0214}ERROR_TOO_MANY_MODULES: result:= 'Too many dynamic link modules are attached';
{0215}ERROR_NESTING_NOT_ALLOWED: result:= 'Can''t nest calls to LoadModule';
{0230}ERROR_BAD_PIPE: result:= 'Pipe state is invalid';
{0231}ERROR_PIPE_BUSY: result:= 'All pipe instances busy';
{0232}ERROR_NO_DATA: result:= 'Pipe close in progress';
{0233}ERROR_PIPE_NOT_CONNECTED: result:= 'No process on other end of pipe';
{0234}ERROR_MORE_DATA: result:= 'More data is available';
{0240}ERROR_VC_DISCONNECTED: result:= 'Session was canceled';
{0254}ERROR_INVALID_EA_NAME: result:= 'Specified EA name is invalid';
{0255}ERROR_EA_LIST_INCONSISTENT: result:= 'EAs are inconsistent';
{0259}ERROR_NO_MORE_ITEMS: result:= 'No more data is available';
{0266}ERROR_CANNOT_COPY: result:= 'Copy API cannot be used';
{0267}ERROR_DIRECTORY: result:= 'Directory name is invalid';
{0275}ERROR_EAS_DIDNT_FIT: result:= 'EAs did not fit in the buffer';
{0276}ERROR_EA_FILE_CORRUPT: result:= 'EA file on the mounted file system is damaged';
{0277}ERROR_EA_TABLE_FULL: result:= 'EA table in the EA file on the mounted file system is full';
{0278}ERROR_INVALID_EA_HANDLE: result:= 'Specified EA handle is invalid';
{0282}ERROR_EAS_NOT_SUPPORTED: result:= 'Mounted file system does not support extended attributes';
{0288}ERROR_NOT_OWNER: result:= 'Attempt to release mutex not owned by caller';
{0298}ERROR_TOO_MANY_POSTS: result:= 'Too many posts made to a semaphore';
{0317}ERROR_MR_MID_NOT_FOUND: result:= 'System cannot find message for message number in message the file';
{0487}ERROR_INVALID_ADDRESS: result:= 'Attempt to access invalid address';
{0534}ERROR_ARITHMETIC_OVERFLOW: result:= 'Arithmetic result exceeded 32-bits';
{0535}ERROR_PIPE_CONNECTED: result:= 'There is a process on other end of the pipe';
{0536}ERROR_PIPE_LISTENING: result:= 'Waiting for a process to open the other end of the pipe';
{0994}ERROR_EA_ACCESS_DENIED: result:= 'Access to the EA is denied';
{0995}ERROR_OPERATION_ABORTED: result:= 'I/O operation aborted due to either thread exit or application request';
{0996}ERROR_IO_INCOMPLETE: result:= 'Overlapped IO event not in signaled state';
{0997}ERROR_IO_PENDING: result:= 'Overlapped IO operation in progress';
{0998}ERROR_NOACCESS: result:= 'Invalid access to memory location';
{0999}ERROR_SWAPERROR: result:= 'Error accessing paging file';
{1001}ERROR_STACK_OVERFLOW: result:= 'Stack overflow';
{1002}ERROR_INVALID_MESSAGE: result:= 'Window can''t handle sent message';
{1003}ERROR_CAN_NOT_COMPLETE: result:= 'Cannot complete function for some reason';
{1004}ERROR_INVALID_FLAGS: result:= 'Flags are invalid';
{1005}ERROR_UNRECOGNIZED_VOLUME: result:= 'Unrecognized volume label';
{1006}ERROR_FILE_INVALID: result:= 'Volume for a file was externally altered and the opened file is no longer valid';
{1007}ERROR_FULLSCREEN_MODE: result:= 'Requested operation cannot be performed in full-screen mode';
{1008}ERROR_NO_TOKEN: result:= 'Attempt to reference a token that does not exist';
{1009}ERROR_BADDB: result:= 'Configuration registry database is damaged';
{1010}ERROR_BADKEY: result:= 'Configuration registry key is invalid';
{1011}ERROR_CANTOPEN: result:= 'Configuration registry key cannot be opened';
{1012}ERROR_CANTREAD: result:= 'Configuration registry key cannot be read';
{1013}ERROR_CANTWRITE: result:= 'Configuration registry key cannot be written';
{1014}ERROR_REGISTRY_RECOVERED: result:= 'Error registry recovery';
{1015}ERROR_REGISTRY_CORRUPT: result:= 'Registry is damaged';
{1016}ERROR_REGISTRY_IO_FAILED: result:= 'Registry I/O failed';
{1017}ERROR_NOT_REGISTRY_FILE: result:= 'Specified file is not in the format of a registry file';
{1018}ERROR_KEY_DELETED: result:= 'Illegal operation attempted on a registry key that has been marked for deletion';
{1019}ERROR_NO_LOG_SPACE: result:= 'System could not allocate required space in a registry log';
{1020}ERROR_KEY_HAS_CHILDREN: result:= 'Attempt to create a symbolic link in a registry key that already has subkeys or values';
{1021}ERROR_CHILD_MUST_BE_VOLATILE: result:= 'Cannot create a stable subkey under a volatile parent key';
{1022}ERROR_NOTIFY_ENUM_DIR: result:= 'The caller needs to enumerate the files to find the changes';
{1051}ERROR_DEPENDENT_SERVICES_RUNNING: result:= 'Stop control has been sent to a service which other running services are dependent on';
{1052}ERROR_INVALID_SERVICE_CONTROL: result:= 'Requested control is not valid for this service';
{1053}ERROR_SERVICE_REQUEST_TIMEOUT: result:= 'Service request time-out';
{1054}ERROR_SERVICE_NO_THREAD: result:= 'Thread could not be created';
{1055}ERROR_SERVICE_DATABASE_LOCKED: result:= 'Service database locked';
{1056}ERROR_SERVICE_ALREADY_RUNNING: result:= 'Instance of the service is already running';
{1057}ERROR_INVALID_SERVICE_ACCOUNT: result:= 'Account name is invalid or does not exist';
{1058}ERROR_SERVICE_DISABLED: result:= 'Specified service cannot be started';
{1059}ERROR_CIRCULAR_DEPENDENCY: result:= 'Circular service dependency was specified';
{1060}ERROR_SERVICE_DOES_NOT_EXIST: result:= 'Specified service does not exist';
{1061}ERROR_SERVICE_CANNOT_ACCEPT_CTRL: result:= 'Service cannot accept control messages';
{1062}ERROR_SERVICE_NOT_ACTIVE: result:= 'Service has not been started';
{1064}ERROR_EXCEPTION_IN_SERVICE: result:= 'Exception occurred in the service when handling the control request';
{1065}ERROR_DATABASE_DOES_NOT_EXIST: result:= 'Database specified does not exist';
{1066}ERROR_SERVICE_SPECIFIC_ERROR: result:= 'Service returned to a service-specific error code';
{1067}ERROR_PROCESS_ABORTED: result:= 'Process terminated unexpectedly';
{1068}ERROR_SERVICE_DEPENDENCY_FAIL: result:= 'Dependency service failed to start';
{1069}ERROR_SERVICE_LOGON_FAILED: result:= 'Service not started due to a logon failure';
{1070}ERROR_SERVICE_START_HANG: result:= 'Service hung in a start-pending state';
{1071}ERROR_INVALID_SERVICE_LOCK: result:= 'Specified service database lock is invalid';
{1072}ERROR_SERVICE_MARKED_FOR_DELETE: result:= 'Specified service marked for deletion';
{1073}ERROR_SERVICE_EXISTS: result:= 'Specified service already exists';
{1074}ERROR_ALREADY_RUNNING_LKG: result:= 'System is currently running with the last-known-good configuration';
{1075}ERROR_SERVICE_DEPENDENCY_DELETED: result:= 'Dependency service does not exist';
{1076}ERROR_BOOT_ALREADY_ACCEPTED: result:= 'Current boot has already been accepted for use as the last-known-good control set';
{1077}ERROR_SERVICE_NEVER_STARTED: result:= 'No attempts to start the service';
{1078}ERROR_DUPLICATE_SERVICE_NAME: result:= 'Name is already in use as either a service name or a service display name';
{1100}ERROR_END_OF_MEDIA: result:= 'End of tape mark was reached during an operation';
{1101}ERROR_FILEMARK_DETECTED: result:= 'Tape access reached a filemark';
{1102}ERROR_BEGINNING_OF_MEDIA: result:= 'Beginning of the tape or partition was encountered';
{1103}ERROR_SETMARK_DETECTED: result:= 'Tape access reached a setmark';
{1104}ERROR_NO_DATA_DETECTED: result:= 'During a tape access, the end of the data marker was reached';
{1105}ERROR_PARTITION_FAILURE: result:= 'Tape could not be partitioned';
{1106}ERROR_INVALID_BLOCK_LENGTH: result:= 'Incorrect block length';
{1107}ERROR_DEVICE_NOT_PARTITIONED: result:= 'Tape partition information could not be found when loading a tape';
{1108}ERROR_UNABLE_TO_LOCK_MEDIA: result:= 'Attempt to lock the eject media mechanism failed';
{1109}ERROR_UNABLE_TO_UNLOAD_MEDIA: result:= 'Unload media failed';
{1110}ERROR_MEDIA_CHANGED: result:= 'Media in drive may have changed';
{1111}ERROR_BUS_RESET: result:= 'I/O bus was reset';
{1112}ERROR_NO_MEDIA_IN_DRIVE: result:= 'Tape query failed because of no media in drive';
{1113}ERROR_NO_UNICODE_TRANSLATION: result:= 'No mapping for the Unicode character exists in the target multi-byte code page';
{1114}ERROR_DLL_INIT_FAILED: result:= 'DLL initialization routine failed';
{1115}ERROR_SHUTDOWN_IN_PROGRESS: result:= 'System shutdown is in progress';
{1116}ERROR_NO_SHUTDOWN_IN_PROGRESS: result:= 'Attempt to abort the shutdown of the system failed';
{1117}ERROR_IO_DEVICE: result:= 'Request could not be performed because of an I/O device error';
{1118}ERROR_SERIAL_NO_DEVICE: result:= 'No serial device initialized';
{1119}ERROR_IRQ_BUSY: result:= 'Unable to open a device (IRQ busy)';
{1120}ERROR_MORE_WRITES: result:= 'Serial I/O operation was completed by another write to the serial port';
{1121}ERROR_COUNTER_TIMEOUT: result:= 'Serial I/O operation time-out period expired';
{1122}ERROR_FLOPPY_ID_MARK_NOT_FOUND: result:= 'No ID address mark was found on the floppy disk';
{1123}ERROR_FLOPPY_WRONG_CYLINDER: result:= 'Mismatch between the floppy disk sector ID field and the floppy disk controller track address';
{1124}ERROR_FLOPPY_UNKNOWN_ERROR: result:= 'Floppy disk controller reported an error that is not recognized by the floppy disk driver';
{1125}ERROR_FLOPPY_BAD_REGISTERS: result:= 'Floppy disk controller returned inconsistent results in its registers';
{1126}ERROR_DISK_RECALIBRATE_FAILED: result:= 'Hard disk recalibrate operation failed';
{1127}ERROR_DISK_OPERATION_FAILED: result:= 'Hard disk operation failed';
{1128}ERROR_DISK_RESET_FAILED: result:= 'Hard disk controller reset failed';
{1129}ERROR_EOM_OVERFLOW: result:= 'Physical end of tape encountered';
{1130}ERROR_NOT_ENOUGH_SERVER_MEMORY: result:= 'Not enough server storage is available to process this command';
{1131}ERROR_POSSIBLE_DEADLOCK: result:= 'Potential deadlock condition has been detected';
{1200}ERROR_BAD_DEVICE: result:= 'Specified device name is invalid';
{1201}ERROR_CONNECTION_UNAVAIL: result:= 'Device is not currently connected';
{1202}ERROR_DEVICE_ALREADY_REMEMBERED: result:= 'Attempt was made to remember a device that was previously remembered';
{1203}ERROR_NO_NET_OR_BAD_PATH: result:= 'No network provider accepted the given network path';
{1204}ERROR_BAD_PROVIDER: result:= 'Specified network provider name is invalid';
{1205}ERROR_CANNOT_OPEN_PROFILE: result:= 'Unable to open the network connection profile';
{1206}ERROR_BAD_PROFILE: result:= 'Network connection profile is damaged';
{1207}ERROR_NOT_CONTAINER: result:= 'Cannot enumerate a non-container';
{1208}ERROR_EXTENDED_ERROR: result:= 'Extended error has occurred';
{1209}ERROR_INVALID_GROUPNAME: result:= 'Format of the specified group name is invalid';
{1210}ERROR_INVALID_COMPUTERNAME: result:= 'Format of the specified computer name is invalid';
{1211}ERROR_INVALID_EVENTNAME: result:= 'Format of the specified event name is invalid';
{1212}ERROR_INVALID_DOMAINNAME: result:= 'Format of the specified domain name is invalid';
{1213}ERROR_INVALID_SERVICENAME: result:= 'Format of the specified service name is invalid';
{1214}ERROR_INVALID_NETNAME: result:= 'Format of the specified network name is invalid';
{1215}ERROR_INVALID_SHARENAME: result:= 'Format of the specified share name is invalid';
{1216}ERROR_INVALID_PASSWORDNAME: result:= 'Format of the specified password is invalid';
{1217}ERROR_INVALID_MESSAGENAME: result:= 'Format of the specified message name is invalid';
{1218}ERROR_INVALID_MESSAGEDEST: result:= 'Format of the specified message destination is invalid';
{1221}ERROR_DUP_DOMAINNAME: result:= 'Workgroup or domain name is already in use by another computer on the network';
{1222}ERROR_NO_NETWORK: result:= 'Network is not present or not started';
{1300}ERROR_NOT_ALL_ASSIGNED: result:= 'Not all privileges referenced are assigned to the caller';
{1301}ERROR_SOME_NOT_MAPPED: result:= 'Some of the information to be mapped has not been translated';
{1302}ERROR_NO_QUOTAS_FOR_ACCOUNT: result:= 'No system quota limits are specifically set for this account';
{1303}ERROR_LOCAL_USER_SESSION_KEY: result:= 'User session key was requested for a local RPC connection';
{1304}ERROR_NULL_LM_PASSWORD: result:= 'Windows NT password is too complex to be converted';
{1305}ERROR_UNKNOWN_REVISION: result:= 'Unknown service revision number';
{1306}ERROR_REVISION_MISMATCH: result:= 'Two revision levels are incompatible';
{1307}ERROR_INVALID_OWNER: result:= 'Particular Security ID cannot be assigned as the owner of an object';
{1308}ERROR_INVALID_PRIMARY_GROUP: result:= 'Particular Security ID cannot be assigned as the primary group of an object';
{1309}ERROR_NO_IMPERSONATION_TOKEN: result:= 'Attempt to operate on an impersonation token by a thread';
{1310}ERROR_CANT_DISABLE_MANDATORY: result:= 'Mandatory group cannot be disabled';
{1311}ERROR_NO_LOGON_SERVERS: result:= 'No logon servers available to service the logon request';
{1312}ERROR_NO_SUCH_LOGON_SESSION: result:= 'Specified logon session does not exist';
{1314}ERROR_PRIVILEGE_NOT_HELD: result:= 'Required privilege is not held by the client';
{1315}ERROR_INVALID_ACCOUNT_NAME: result:= 'Name provided is not a properly formed account name';
{1316}ERROR_USER_EXISTS: result:= 'Specified user already exists';
{1317}ERROR_NO_SUCH_USER: result:= 'Specified user does not exist';
{1318}ERROR_GROUP_EXISTS: result:= 'Specified group already exists';
{1319}ERROR_NO_SUCH_GROUP: result:= 'Specified group does not exist';
{1320}ERROR_MEMBER_IN_GROUP: result:= 'Specified user account is already in the specified group account';
{1321}ERROR_MEMBER_NOT_IN_GROUP: result:= 'Specified user account is not a member of the specified group account';
{1322}ERROR_LAST_ADMIN: result:= 'Requested operation would disable or delete the last remaining administration account';
{1323}ERROR_WRONG_PASSWORD: result:= 'Current password is incorrect';
{1324}ERROR_ILL_FORMED_PASSWORD: result:= 'Value provided for the new password contains values not allowed in passwords';
{1325}ERROR_PASSWORD_RESTRICTION: result:= 'Password update rule violated';
{1326}ERROR_LOGON_FAILURE: result:= 'Attempted logon is invalid due to either a bad user name or authentication information';
{1327}ERROR_ACCOUNT_RESTRICTION: result:=	'User account restriction has prevented successful authentication';
{1328}ERROR_INVALID_LOGON_HOURS: result:= 'User account has time restrictions and cannot be logged onto at this time';
{1329}ERROR_INVALID_WORKSTATION: result:= 'User account is restricted and cannot be used to log on from the source workstation';
{1330}ERROR_PASSWORD_EXPIRED: result:= 'User account''s password has expired';
{1331}ERROR_ACCOUNT_DISABLED: result:= 'Referenced account is currently disabled and cannot be logged on to';
{1332}ERROR_NONE_MAPPED: result:= 'None of the information to be mapped has been translated';
{1333}ERROR_TOO_MANY_LUIDS_REQUESTED: result:= 'Number of LUID requested cannot be allocated with a single allocation';
{1334}ERROR_LUIDS_EXHAUSTED: result:= 'No more LUID to allocate';
{1335}ERROR_INVALID_SUB_AUTHORITY: result:= 'Sub-authority value is invalid for the particular use';
{1336}ERROR_INVALID_ACL: result:= 'ACL structure is not valid';
{1337}ERROR_INVALID_SID: result:= 'SID structure is invalid';
{1338}ERROR_INVALID_SECURITY_DESCR: result:= 'SECURITY_DESCRIPTOR structure is invalid';
{1340}ERROR_BAD_INHERITANCE_ACL: result:= 'Attempt to build either an inherited ACL or ACE did not succeed';
{1341}ERROR_SERVER_DISABLED: result:= 'GUID allocation server is already disabled';
{1342}ERROR_SERVER_NOT_DISABLED: result:= 'GUID allocation server is already enabled';
{1343}ERROR_INVALID_ID_AUTHORITY: result:= 'Value provided is an invalid value for an identifier authority';
{1344}ERROR_ALLOTTED_SPACE_EXCEEDED: result:= 'Access control and primary group information exceed the amount of memory originally allotted';
{1345}ERROR_INVALID_GROUP_ATTRIBUTES: result:= 'Specified attributes are invalid';
{1346}ERROR_BAD_IMPERSONATION_LEVEL: result:= 'Required impersonation level was not provided';
{1347}ERROR_CANT_OPEN_ANONYMOUS: result:= 'Anonymous tokens cannot be opened';
{1348}ERROR_BAD_VALIDATION_CLASS: result:= 'Requested validation information class is invalid';
{1349}ERROR_BAD_TOKEN_TYPE: result:= 'Type of token object is inappropriate for its attempted use';
{1350}ERROR_NO_SECURITY_ON_OBJECT: result:= 'Attempt to operate on the security of an object that does not have security associated with it';
{1351}ERROR_CANT_ACCESS_DOMAIN_INFO: result:= 'Domain controller could not be contacted';
{1352}ERROR_INVALID_SERVER_STATE: result:= 'Sam Server was in the wrong state to perform the desired operation';
{1353}ERROR_INVALID_DOMAIN_STATE: result:= 'Domain is in the wrong state to perform the desired operation';
{1354}ERROR_INVALID_DOMAIN_ROLE: result:= 'Requested operation cannot be completed with the domain in its present role';
{1355}ERROR_NO_SUCH_DOMAIN: result:= 'Specified domain does not exist';
{1357}ERROR_DOMAIN_LIMIT_EXCEEDED: result:= 'Attempt to exceed the limit on the number of domains per server for this release';
{1358}ERROR_INTERNAL_DB_CORRUPTION: result:= 'Requested operation cannot be completed due to a catastrophic media failure or on-disk data structure corruption';
{1359}ERROR_INTERNAL_ERROR: result:= 'SAM server has encounterred an internal consistency	error in its database';
{1360}ERROR_GENERIC_NOT_MAPPED: result:= 'Generic access types should be mapped to non-generic access types';
{1361}ERROR_BAD_DESCRIPTOR_FORMAT: result:= 'Security descriptor is not in the required format (absolute or self-relative)';
{1362}ERROR_NOT_LOGON_PROCESS: result:= 'Requested action is restricted for use by logon processes only';
{1363}ERROR_LOGON_SESSION_EXISTS: result:= 'Attempt to start a new session manager or LSA logon session with an ID already in use';
{1364}ERROR_NO_SUCH_PACKAGE: result:= 'Specified authentication package is unknown';
{1364}ERROR_NO_SUCH_PRIVILEGE: result:= 'Specified privilege does not exist';
{1365}ERROR_BAD_LOGON_SESSION_STATE: result:= 'Logon session is not in a state consistent with the requested operation';
{1365}ERROR_DOMAIN_EXISTS: result:= 'Specified domain already exists';
{1366}ERROR_LOGON_SESSION_COLLISION: result:= 'Logon session ID is already in use';
{1367}ERROR_INVALID_LOGON_TYPE: result:= 'Invalid value has been provided for LogonType has been requested';
{1368}ERROR_CANNOT_IMPERSONATE: result:= 'Attempt was made to impersonate via a named pipe was not yet read from';
{1369}ERROR_RXACT_INVALID_STATE: result:= 'Transaction state of a registry sub-tree is incompatible with the requested operation';
{1370}ERROR_RXACT_COMMIT_FAILURE: result:= 'Error occurred during a registry transaction commit';
{1371}ERROR_SPECIAL_ACCOUNT: result:= 'Operation attempted on a built-in SAM account';
{1372}ERROR_SPECIAL_GROUP: result:= 'Requested operation cannot be performed on the built-in special group';
{1373}ERROR_SPECIAL_USER: result:= 'Requested operation cannot be performed on the built-in special user';
{1374}ERROR_MEMBERS_PRIMARY_GROUP: result:= 'Member cannot be removed from member''s primary group';
{1375}ERROR_TOKEN_ALREADY_IN_USE: result:= 'Attempt to establish a token is in use';
{1376}ERROR_NO_SUCH_ALIAS: result:= 'Specified alias does not exist';
{1377}ERROR_MEMBER_NOT_IN_ALIAS: result:= 'Specified account name is not a member of the alias';
{1378}ERROR_MEMBER_IN_ALIAS: result:= 'Specified account name is not a member of the alias';
{1379}ERROR_ALIAS_EXISTS: result:= 'Specified alias already exists';
{1380}ERROR_LOGON_NOT_GRANTED: result:= 'Requested type of logon, such as Interactive, Network, or Service, is not granted';
{1381}ERROR_TOO_MANY_SECRETS: result:= 'Maximum number of secrets exceeded';
{1382}ERROR_SECRET_TOO_LONG: result:= 'Length of a secret exceeds the maximum length';
{1383}ERROR_INTERNAL_DB_ERROR: result:= 'Local Security Authority (LSA) database contains in internal inconsistency';
{1384}ERROR_TOO_MANY_CONTEXT_IDS: result:= 'User''s security context accumulated too many security IDs';
{1385}ERROR_LOGON_TYPE_NOT_GRANTED: result:= 'User has requested a type of logon not granted';
{1387}ERROR_NO_SUCH_MEMBER: result:= 'New member cannot be added to the alias';
{1388}ERROR_INVALID_MEMBER: result:= 'New member could not be added to an alias because the member has the wrong account type';
{1389}ERROR_TOO_MANY_SIDS: result:= 'Too many SIDs specified';
{1391}ERROR_NO_INHERITANCE: result:= 'ACL contains no inheritable components';
{1392}ERROR_FILE_CORRUPT: result:= 'File or directory is damaged and nonreadable';
{1393}ERROR_DISK_CORRUPT: result:= 'Disk structure is damaged and nonreadable';
{1394}ERROR_NO_USER_SESSION_KEY: result:= 'No user session key for the specified logon session';
{1400}ERROR_INVALID_WINDOW_HANDLE: result:= 'Window handle invalid';
{1401}ERROR_INVALID_MENU_HANDLE: result:= 'Menu handle is invalid';
{1402}ERROR_INVALID_CURSOR_HANDLE: result:= 'Cursor handle is invalid';
{1403}ERROR_INVALID_ACCEL_HANDLE: result:= 'Invalid accelerator-table handle';
{1404}ERROR_INVALID_HOOK_HANDLE: result:= 'Hook handle is invalid';
{1405}ERROR_INVALID_DWP_HANDLE: result:= 'DeferWindowPos handle is invalid';
{1406}ERROR_TLW_WITH_WSCHILD: result:= 'Creating top-level window with WS_CHILD style failed';
{1407}ERROR_CANNOT_FIND_WND_CLASS: result:= 'Cannot find window class';
{1408}ERROR_WINDOW_OF_OTHER_THREAD: result:= 'Invalid window, belongs to other thread';
{1409}ERROR_HOTKEY_ALREADY_REGISTERED: result:= 'Hotkey is already registered';
{1410}ERROR_CLASS_ALREADY_EXISTS: result:= 'Class already exists';
{1411}ERROR_CLASS_DOES_NOT_EXIST: result:= 'Class does not exist';
{1412}ERROR_CLASS_HAS_WINDOWS: result:= 'Class still has open windows';
{1413}ERROR_INVALID_INDEX: result:= 'Index is invalid';
{1414}ERROR_INVALID_ICON_HANDLE: result:= 'Icon handle is invalid';
{1415}ERROR_PRIVATE_DIALOG_INDEX: result:= 'Using private DIALOG window words';
{1416}ERROR_LISTBOX_ID_NOT_FOUND: result:= 'List box ID not found';
{1417}ERROR_NO_WILDCARD_CHARACTERS: result:= 'No wildcard characters found';
{1418}ERROR_CLIPBOARD_NOT_OPEN: result:= 'Thread doesn''t have clipboard open';
{1419}ERROR_HOTKEY_NOT_REGISTERED: result:= 'Hotkey is not registered';
{1420}ERROR_WINDOW_NOT_DIALOG: result:= 'Window is not a valid dialog window';
{1421}ERROR_CONTROL_ID_NOT_FOUND: result:= 'Control ID not found';
{1422}ERROR_INVALID_COMBOBOX_MESSAGE: result:= 'Invalid Message, combo box doesn''t have an edit control';
{1423}ERROR_WINDOW_NOT_COMBOBOX: result:= 'Window is not a combo box';
{1424}ERROR_INVALID_EDIT_HEIGHT: result:= 'Height must be less than 256';
{1425}ERROR_DC_NOT_FOUND: result:= 'Invalid HDC passed to ReleaseDC';
{1426}ERROR_INVALID_HOOK_FILTER: result:= 'Hook filter type is invalid';
{1427}ERROR_INVALID_FILTER_PROC: result:= 'Filter proc is invalid';
{1428}ERROR_HOOK_NEEDS_HMOD: result:= 'Cannot set non-local hook without an module handle';
{1429}ERROR_GLOBAL_ONLY_HOOK: result:= 'This hook can only be set globally';
{1430}ERROR_JOURNAL_HOOK_SET: result:= 'Journal hook is already installed';
{1431}ERROR_HOOK_NOT_INSTALLED: result:= 'Hook is not installed';
{1432}ERROR_INVALID_LB_MESSAGE: result:= 'Message for single-selection list box is invalid';
{1433}ERROR_SETCOUNT_ON_BAD_LB: result:= 'LB_SETCOUNT sent to non-lazy list box';
{1434}ERROR_LB_WITHOUT_TABSTOPS: result:= 'List box doesn''t support tab stops';
{1436}ERROR_CHILD_WINDOW_MENU: result:= 'Child windows can''t have menus';
{1437}ERROR_NO_SYSTEM_MENU: result:= 'Window does not have system menu';
{1438}ERROR_INVALID_MSGBOX_STYLE: result:= 'Message box style is invalid';
{1439}ERROR_INVALID_SPI_VALUE: result:= 'SPI_* parameter is invalid';
{1440}ERROR_SCREEN_ALREADY_LOCKED: result:= 'Screen already locked';
{1442}ERROR_NOT_CHILD_WINDOW: result:= 'Window is not a child window';
{1443}ERROR_INVALID_GW_COMMAND: result:= 'GW_* command is invalid';
{1444}ERROR_INVALID_THREAD_ID: result:= 'Thread ID is invalid';
{1445}ERROR_NON_MDICHILD_WINDOW: result:= 'DefMDIChildProc called with a non-MDI child window';
{1446}ERROR_POPUP_ALREADY_ACTIVE: result:= 'Pop-up menu already active';
{1447}ERROR_NO_SCROLLBARS: result:= 'Window does not have scroll bars';
{1448}ERROR_INVALID_SCROLLBAR_RANGE: result:= 'Scrollbar range greater than 0x7FFF';
{1449}ERROR_INVALID_SHOWWIN_COMMAND: result:= 'ShowWindow command is invalid';
{1500}ERROR_EVENTLOG_FILE_CORRUPT: result:= 'One of the Eventlog logfiles is damaged';
{1501}ERROR_EVENTLOG_CANT_START: result:= 'No event log file could be opened';
{1502}ERROR_LOG_FILE_FULL: result:= 'Event log file is full';
{1503}ERROR_EVENTLOG_FILE_CHANGED: result:= 'Event log file has changed between reads';
{1751}EPT_S_INVALID_ENTRY: result:= 'Entry is invalid';
{1752}EPT_S_CANT_PERFORM_OP: result:= 'Operation cannot be performed';
{1753}EPT_S_NOT_REGISTERED: result:= 'There are no more endpoints available from the endpoint mapper';
{1784}ERROR_INVALID_USER_BUFFER: result:= 'Supplied user buffer is invalid for the requested operation';
{1785}ERROR_UNRECOGNIZED_MEDIA: result:= 'Unrecognized media type';
{1786}ERROR_NO_TRUST_LSA_SECRET: result:= 'Workstation does not have a trust secret';
{1787}ERROR_NO_TRUST_SAM_ACCOUNT: result:= 'Domain controller does not have an account for this workstation';
{1788}ERROR_TRUSTED_DOMAIN_FAILURE: result:= 'Trust relationship between the primary domain and the trusted domain failed';
{1790}ERROR_TRUST_FAILURE: result:= 'Network logon failed';
{1792}ERROR_NETLOGON_NOT_STARTED: result:= 'Attempt to logon, but the network logon service was not started';
{1793}ERROR_ACCOUNT_EXPIRED: result:= 'User''s account has expired';
{1796}ERROR_UNKNOWN_PORT: result:= 'Specified port is unknown';
{1797}ERROR_UNKNOWN_PRINTER_DRIVER: result:= 'Printer driver is unknown';
{1798}ERROR_UNKNOWN_PRINTPROCESSOR: result:= 'Print processor is unknown';
{1799}ERROR_INVALID_SEPARATOR_FILE: result:= 'Specified separator file is invalid';
{1800}ERROR_INVALID_PRIORITY: result:= 'Specified priority is invalid';
{1801}ERROR_INVALID_PRINTER_NAME: result:= 'Printer name is invalid';
{1802}ERROR_PRINTER_ALREADY_EXISTS: result:= 'Printer already exists';
{1803}ERROR_INVALID_PRINTER_COMMAND: result:= 'Printer command is invalid';
{1804}ERROR_INVALID_DATATYPE: result:= 'Specified datatype is invalid';
{1805}ERROR_INVALID_ENVIRONMENT: result:= 'Environment specified is invalid';
{1810}ERROR_DOMAIN_TRUST_INCONSISTENT: result:= 'Name or security ID (SID) of the domain specified is inconsistent';
{1811}ERROR_SERVER_HAS_OPEN_HANDLES: result:= 'Server cannot be unloaded';
{1812}ERROR_RESOURCE_DATA_NOT_FOUND: result:= 'Specified image file did not contain a resource section';
{1813}ERROR_RESOURCE_TYPE_NOT_FOUND: result:= 'Specified resource type can not be found in the image file';
{1814}ERROR_RESOURCE_NAME_NOT_FOUND: result:= 'Specified resource name can not be found in the image file';
{1815}ERROR_RESOURCE_LANG_NOT_FOUND: result:= 'Specified resource language ID cannot be found in the image file';
{1816}ERROR_NOT_ENOUGH_QUOTA: result:= 'Not enough quota is available to process this command';
{1899}EPT_S_CANT_CREATE: result:= 'Endpoint mapper database could not be created';
{2202}ERROR_BAD_USERNAME: result:= 'Specified user name is invalid';
{2250}ERROR_NOT_CONNECTED: result:= 'Network connection does not exist';
{2401}ERROR_OPEN_FILES: result:= 'There are open files or requests pending on this connection';
{2404}ERROR_DEVICE_IN_USE: result:= 'Device is in use by an active process and cannot be disconnected';
{6118}ERROR_NO_BROWSER_SERVERS_FOUND: result:= 'List of servers for this workgroup is not currently available';
		else
		result:= 'unknown error #'+IntToStr(GetLastError);// capture other errors...
	end;
end;

function TAPITools.GetVolumeInfo(const Drv: Char): boolean;
const
	dwVolumeNameSize: DWORD= MAX_PATH;
	dwFileSystemNameSize: DWORD= 8;
var
	TmpDrv: string;
	szVolumeNameBuffer,
	szFileSystemNameBuffer: PChar;
	dwFileSystemFlags,
	dwVolumeSerialNumber,
	dwMaximumComponentLength,
	dwErrMode: DWORD;
begin
	if Drv = #0 then
	mcError('GetVolumeInformation FAILED!');

	{ make sure windows critical error box doesn't appear }
	dwErrMode:= SetErrorMode(SEM_FAILCRITICALERRORS);
	TmpDrv:= Drv+':\';
	szVolumeNameBuffer:= AllocMem(dwVolumeNameSize);
	szFileSystemNameBuffer:= AllocMem(dwFileSystemNameSize);
	try
		result:= GetVolumeInformation(PChar(TmpDrv),szVolumeNameBuffer,dwVolumeNameSize,
			@dwVolumeSerialNumber,dwMaximumComponentLength,dwFileSystemFlags,
			szFileSystemNameBuffer,dwFileSystemNameSize);
		if result then
		begin
			VolumeName:= szVolumeNameBuffer;
			VolSerialNo:= dwVolumeSerialNumber;
			VolFileSystem:= szFileSystemNameBuffer;
			VolSystemFlags:= dwFileSystemFlags;
		end;
	finally
		FreeMem(szVolumeNameBuffer);
		FreeMem(szFileSystemNameBuffer);
	end;
	SetErrorMode(dwErrMode);
end;

function TAPITools.LocaleInfo(lcType: DWord): string;
var
	Locale: LCID;	// locale identifier
	LCData: PChar;	// address of buffer for information
const
	LCSize: Byte= 128; 	// size of buffer
begin
	Locale:= GetSystemDefaultLCID;
	LCData:= AllocMem(LCSize);
	try
		if GetLocaleInfo(Locale,lcType,LCData,LCSize) > 0 then
		result:= LCData else
		result:= ErrorMsg;
	finally
		FreeMem(LCData);
	end;
end;

function TAPITools.LocaleStr(idLocale: integer): string;
begin
	result:= '';
	case idLocale of
{0001}LOCALE_ILANGUAGE: result:= 'LOCALE_ILANGUAGE';
{0002}LOCALE_SLANGUAGE: result:= 'LOCALE_SLANGUAGE';
{0003}LOCALE_SABBREVLANGNAME: result:= 'LOCALE_SABBREVLANGNAME';
{0004}LOCALE_SNATIVELANGNAME: result:= 'LOCALE_SNATIVELANGNAME';
{0005}LOCALE_ICOUNTRY: result:= 'LOCALE_ICOUNTRY';
{0006}LOCALE_SCOUNTRY: result:= 'LOCALE_SCOUNTRY';
{0007}LOCALE_SABBREVCTRYNAME: result:= 'LOCALE_SABBREVCTRYNAME';
{0008}LOCALE_SNATIVECTRYNAME: result:= 'LOCALE_SNATIVECTRYNAME';
{0009}LOCALE_IDEFAULTLANGUAGE: result:= 'LOCALE_IDEFAULTLANGUAGE';
{0010}LOCALE_IDEFAULTCOUNTRY: result:= 'LOCALE_IDEFAULTCOUNTRY';
{0011}LOCALE_IDEFAULTCODEPAGE: result:= 'LOCALE_IDEFAULTCODEPAGE';
{0012}LOCALE_SLIST: result:= 'LOCALE_SLIST';
{0013}LOCALE_IMEASURE: result:= 'LOCALE_IMEASURE';
{0014}LOCALE_SDECIMAL: result:= 'LOCALE_SDECIMAL';
{0015}LOCALE_STHOUSAND: result:= 'LOCALE_STHOUSAND';
{0016}LOCALE_SGROUPING: result:= 'LOCALE_SGROUPING';
{0017}LOCALE_IDIGITS: result:= 'LOCALE_IDIGITS';
{0018}LOCALE_ILZERO: result:= 'LOCALE_ILZERO';
{0019}LOCALE_SNATIVEDIGITS: result:= 'LOCALE_SNATIVEDIGITS';
{0020}LOCALE_SCURRENCY: result:= 'LOCALE_SCURRENCY';
{0021}LOCALE_SINTLSYMBOL: result:= 'LOCALE_SINTLSYMBOL';
{0022}LOCALE_SMONDECIMALSEP: result:= 'LOCALE_SMONDECIMALSEP';
{0023}LOCALE_SMONTHOUSANDSEP: result:= 'LOCALE_SMONTHOUSANDSEP';
{0024}LOCALE_SMONGROUPING: result:= 'LOCALE_SMONGROUPING';
{0025}LOCALE_ICURRDIGITS: result:= 'LOCALE_ICURRDIGITS';
{0026}LOCALE_IINTLCURRDIGITS: result:= 'LOCALE_IINTLCURRDIGITS';
{0027}LOCALE_ICURRENCY: result:= 'LOCALE_ICURRENCY';
{0028}LOCALE_INEGCURR: result:= 'LOCALE_INEGCURR';
{0029}LOCALE_SDATE: result:= 'LOCALE_SDATE';
{0030}LOCALE_STIME: result:= 'LOCALE_STIME';
{0031}LOCALE_SSHORTDATE: result:= 'LOCALE_SSHORTDATE';
{0032}LOCALE_SLONGDATE: result:= 'LOCALE_SLONGDATE';
{0033}LOCALE_IDATE: result:= 'LOCALE_IDATE';
{0034}LOCALE_ILDATE: result:= 'LOCALE_ILDATE';
{0035}LOCALE_ITIME: result:= 'LOCALE_ITIME';
{0036}LOCALE_ICENTURY: result:= 'LOCALE_ICENTURY';
{0037}LOCALE_ITLZERO: result:= 'LOCALE_ITLZERO';
{0038}LOCALE_IDAYLZERO: result:= 'LOCALE_IDAYLZERO';
{0039}LOCALE_IMONLZERO: result:= 'LOCALE_IMONLZERO';
{0040}LOCALE_S1159: result:= 'LOCALE_S1159';
{0041}LOCALE_S2359: result:= 'LOCALE_S2359';
{0042}LOCALE_SDAYNAME1: result:= 'LOCALE_SDAYNAME1';
{0043}LOCALE_SDAYNAME2: result:= 'LOCALE_SDAYNAME2';
{0044}LOCALE_SDAYNAME3: result:= 'LOCALE_SDAYNAME3';
{0045}LOCALE_SDAYNAME4: result:= 'LOCALE_SDAYNAME4';
{0046}LOCALE_SDAYNAME5: result:= 'LOCALE_SDAYNAME5';
{0047}LOCALE_SDAYNAME6: result:= 'LOCALE_SDAYNAME6';
{0048}LOCALE_SDAYNAME7: result:= 'LOCALE_SDAYNAME7';
{0049}LOCALE_SABBREVDAYNAME1: result:= 'LOCALE_SABBREVDAYNAME1';
{0050}LOCALE_SABBREVDAYNAME2: result:= 'LOCALE_SABBREVDAYNAME2';
{0051}LOCALE_SABBREVDAYNAME3: result:= 'LOCALE_SABBREVDAYNAME3';
{0052}LOCALE_SABBREVDAYNAME4: result:= 'LOCALE_SABBREVDAYNAME4';
{0053}LOCALE_SABBREVDAYNAME5: result:= 'LOCALE_SABBREVDAYNAME5';
{0054}LOCALE_SABBREVDAYNAME6: result:= 'LOCALE_SABBREVDAYNAME6';
{0055}LOCALE_SABBREVDAYNAME7: result:= 'LOCALE_SABBREVDAYNAME7';
{0056}LOCALE_SMONTHNAME1: result:= 'LOCALE_SMONTHNAME1';
{0057}LOCALE_SMONTHNAME2: result:= 'LOCALE_SMONTHNAME2';
{0058}LOCALE_SMONTHNAME3: result:= 'LOCALE_SMONTHNAME3';
{0059}LOCALE_SMONTHNAME4: result:= 'LOCALE_SMONTHNAME4';
{0060}LOCALE_SMONTHNAME5: result:= 'LOCALE_SMONTHNAME5';
{0061}LOCALE_SMONTHNAME6: result:= 'LOCALE_SMONTHNAME6';
{0062}LOCALE_SMONTHNAME7: result:= 'LOCALE_SMONTHNAME7';
{0063}LOCALE_SMONTHNAME8: result:= 'LOCALE_SMONTHNAME8';
{0064}LOCALE_SMONTHNAME9: result:= 'LOCALE_SMONTHNAME9';
{0065}LOCALE_SMONTHNAME10: result:= 'LOCALE_SMONTHNAME10';
{0066}LOCALE_SMONTHNAME11: result:= 'LOCALE_SMONTHNAME11';
{0067}LOCALE_SMONTHNAME12: result:= 'LOCALE_SMONTHNAME12';
{0068}LOCALE_SABBREVMONTHNAME1: result:= 'LOCALE_SABBREVMONTHNAME1';
{0069}LOCALE_SABBREVMONTHNAME2: result:= 'LOCALE_SABBREVMONTHNAME2';
{0070}LOCALE_SABBREVMONTHNAME3: result:= 'LOCALE_SABBREVMONTHNAME3';
{0071}LOCALE_SABBREVMONTHNAME4: result:= 'LOCALE_SABBREVMONTHNAME4';
{0072}LOCALE_SABBREVMONTHNAME5: result:= 'LOCALE_SABBREVMONTHNAME5';
{0073}LOCALE_SABBREVMONTHNAME6: result:= 'LOCALE_SABBREVMONTHNAME6';
{0074}LOCALE_SABBREVMONTHNAME7: result:= 'LOCALE_SABBREVMONTHNAME7';
{0075}LOCALE_SABBREVMONTHNAME8: result:= 'LOCALE_SABBREVMONTHNAME8';
{0076}LOCALE_SABBREVMONTHNAME9: result:= 'LOCALE_SABBREVMONTHNAME9';
{0077}LOCALE_SABBREVMONTHNAME10: result:= 'LOCALE_SABBREVMONTHNAME10';
{0078}LOCALE_SABBREVMONTHNAME11: result:= 'LOCALE_SABBREVMONTHNAME11';
{0079}LOCALE_SABBREVMONTHNAME12: result:= 'LOCALE_SABBREVMONTHNAME12';
{0080}LOCALE_SPOSITIVESIGN: result:= 'LOCALE_SPOSITIVESIGN';
{0081}LOCALE_SNEGATIVESIGN: result:= 'LOCALE_SNEGATIVESIGN';
{0082}LOCALE_IPOSSIGNPOSN: result:= 'LOCALE_IPOSSIGNPOSN';
{0083}LOCALE_INEGSIGNPOSN: result:= 'LOCALE_INEGSIGNPOSN';
{0084}LOCALE_IPOSSYMPRECEDES: result:= 'LOCALE_IPOSSYMPRECEDES';
{0085}LOCALE_IPOSSEPBYSPACE: result:= 'LOCALE_IPOSSEPBYSPACE';
{0086}LOCALE_INEGSYMPRECEDES: result:= 'LOCALE_INEGSYMPRECEDES';
{0087}LOCALE_INEGSEPBYSPACE: result:= 'LOCALE_INEGSEPBYSPACE';
{0088}LOCALE_FONTSIGNATURE: result:= 'LOCALE_FONTSIGNATURE';
{0089}LOCALE_SISO639LANGNAME: result:= 'LOCALE_SISO639LANGNAME';
{0090}LOCALE_SISO3166CTRYNAME: result:= 'LOCALE_SISO3166CTRYNAME';
{4097}LOCALE_SENGLANGUAGE: result:= 'LOCALE_SENGLANGUAGE';
{4098}LOCALE_SENGCOUNTRY: result:= 'LOCALE_SENGCOUNTRY';
{4099}LOCALE_STIMEFORMAT: result:= 'LOCALE_STIMEFORMAT';
{ LOCALE_NOUSEROVERRIDE	This constant may be OR'ed with any other LCTYPE
	constant in a call to the GetLocaleInfo function. This always causes the
	function to bypass	any user overrides, and return the system default value
	for the	other LCTYPE specified in the function call, based on the given LCID.
	************ See APITest program for and example *******************         }
	end;
end;

function TAPITools.ProcessorFeature(Feature: DWORD): BOOL;
begin
	result:= IsProcessorFeaturePresent(Feature);
end;

function TAPITools.RemoveFont(const Path: string): BOOL;
begin
	result:= RemoveFontResource(PChar(Path));
	if not result then
	result:= BOOL(GetLastError);
end;

function TAPITools.ShortPathName(stPath: string): string;
var
	stShortPath: PChar;
begin
	result:= '';
	GetMem(stShortPath, MAX_PATH);
	try
		if GetShortPathName(PChar(stPath),stShortPath,MAX_PATH) > 0 then
		result:= stShortPath;
	finally
		FreeMem(stShortPath,MAX_PATH);
	end;
end;

function TAPITools.SizeStr(const Msg: string; Num: Double; SizeStrType: TSizeStrType): string;
var
	nNum: Double;
	szNum1,szNum2,	// formatted output for "Num" parameter
	szNot,					// notation string e.g. KB, MB, GB, etc...
	szFrac: string;	// Fractional part of "Num" parameter
begin
	nNum:= 0.0;
	case Length(FloatToStr(Num)) of
		0..3:		// "Num" parameter is less then 1 KB
		begin
			nNum:= Num;
			szNot:= ' bytes';
		end;
		4..6:		// "Num" parameter is greater then 1 KB and less then 1 MB
		begin
			nNum:= Num/DivKByte;
			szNot:= ' KB';
		end;
		7..9:		// "Num" parameter is greater then 1 MB and less then 1 GB
		begin
			nNum:= Num/DivMByte;
			szNot:= ' MB';
		end;
		10..12:	// "Num" parameter is greater then 1 MB and less then 1 GB
		begin
			nNum:= Num/DivGByte;
			szNot:= ' GB';
		end;
	end;
	if Frac(nNum) > 0.0 then
	szFrac:= '%0.2n' else
	szFrac:= '%0.0n';			// do not show .00
	szNum1:= Format(szFrac+szNot,[nNum]);
	szNum2:= Format(szFrac+szNot+' (%0.0n bytes)',[nNum,Num]);
	case SizeStrType of
		ssType1: result:= Msg+szNum1;		// e.g. 92 bytes, 91.69 KB, etc.
		ssType2: result:= Msg+szNum2;		// e.g. 92 bytes (92 bytes), 91.69 KB (93,890 bytes), etc.
	end;
end;

procedure TAPITools.FormShow(FormClass: TFormClass; fParent: TObject);
var
	fChild: TForm;
begin
	if (fParent is TForm) then
	begin
		fChild:= FormClass.Create(fParent as TForm);
		try
			{$ifdef WIN32}
			SetWindowLong(fChild.Handle, GWL_HWNDPARENT, (fParent as TForm).Handle);
			{$else}
			SetWindowWord(fChild.Handle, GWL_HWNDPARENT, (fParent as TForm).Handle);
			{$endif}
			fChild.ShowModal;
		finally
			fChild.Release;
		end;
	end;
end;

procedure TAPITools.ShutDownWindows(const ShutDownAction: TShutDownAction;
	const fForce: boolean);
const
	sdFlag: DWORD = 0;
var
	hToken: THandle;
	tkp: TTokenPrivileges;
	rlen: DWORD;
begin
	case ShutDownAction of
		saLogOff	: sdFlag:= EWX_LOGOFF;
		saPowerOff: sdFlag:= EWX_POWEROFF;
		saReboot	: sdFlag:= EWX_REBOOT;
		saShutDown: sdFlag:= EWX_SHUTDOWN;
	end;

	// Shut down the system and force all applications to close.
	if fForce then
	begin
		if MessageDlg('Force enabled! Are you sure to continue?',
			mtWarning, [mbYes, mbNo], 0) <> idYes then
		Abort;

		// Get a token for this process.
		if not OpenProcessToken(GetCurrentProcess,
			TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, {$ifdef VER90} @hToken {$else} hToken {$endif}) then
		raise Exception.Create('Cannot open access token for this process'#13 + ErrorMsg);
		// Get the LUID for the shutdown privilege.
		LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, tkp.Privileges[0].Luid);
		tkp.PrivilegeCount:= 1;  // one privilege to set
		tkp.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED;
		// Get the shutdown privilege for this process.
		AdjustTokenPrivileges(hToken, FALSE, tkp, SizeOf(tkp), tkp, rlen);
		// Cannot test the return value of AdjustTokenPrivileges.
		if GetLastError <> ERROR_SUCCESS then
		raise Exception.Create('Cannot enable or disable access token privileges'#13 + ErrorMsg);
		sdFlag:= sdFlag or EWX_FORCE;
	end;
	if not ExitWindowsEx(sdFlag, 0) then
	raise Exception.Create('Cannot log off, shut down, or restart'#13 + ErrorMsg);
end;

function TAPITools.GetAppBarState: UINT;
begin
	tabd.cbSize:= SizeOf(TAppBarData);
	tabd.hWnd:= 0;
	result:= SHAppBarMessage(ABM_GETSTATE, tabd);
end;

procedure Register;
begin
	RegisterComponents('mcTech', [TAPITools]);
end;

end.

