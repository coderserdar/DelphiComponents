//
// The original Delphi code is : NameSpaceUnit.pas released 12.12.2002
// Last version: 0.96 released 15.04.2004
// The initial developer is Cedomir Plavljanic (cedomirp@yahoo.com)
// Copyright (C) 2002-2004 Cedomir Plavljanic
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// many function in this unit based on
// Undocumented win95 by James Holderness (Copyright © 1998-1999)
// http://www.geocities.com/SiliconValley/4942
// Microsoft documented now
//
// This unit contains functions, constants and types for work with
// PIDL structure and register notify event from shell
//
// version: 0.96 released  15.04.2004
// change const rsWinZip to SystemFileAssociations\.zip\CLSID
// add var CLSID_ZIPFile	= {E88DCCE0-B7B3-11D1-A9F0-00AA0060FA31}
//
// version: 0.94 released  15.02.2004
// fix bug - donn't show size of winzip file
//     winzip file sometimes has attribute SFGAO_FOLDER ?!?!
//     sometimes occurr on XP
//
// version: 0.93 released  04.02.2004
// fix another bug in FolderExists
// fix another bug in FileExists
//
// version: 0.92 released  02.02.2004
// fix bug in FolderExists
// fix bug in FileExists
//
// version: 0.91 released  24.12.2003
// add constant variable PIDL_NetHood
//
// version: 0.8
//

unit NameSpaceLibrary;

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

interface

uses
	Windows, SysUtils, Classes, Messages, ActiveX, ShellAPI, ShlObj,
	Forms, ComObj;

//Register function for Change Notify
const
	WM_STARTMESSAGE	= WM_USER+100;
	WM_CHANGENOTIFY = WM_STARTMESSAGE+1;

	SHCNF_ACCEPT_INTERRUPTS 		=	$0001;
	SHCNF_ACCEPT_NON_INTERRUPTS =	$0002;
	SHCNF_NO_PROXY 							=	$8000;

	SCHNE_ALLEVENTSMASK					=	$7FFFF;

	EnumsAll					=	SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
	EnumsAllOneHidden	= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS;
	EnumsFolder				= SHCONTF_FOLDERS;
	EnumsNonFolder		=	SHCONTF_NONFOLDERS;
	EnumsHidden				=	SHCONTF_INCLUDEHIDDEN;

	IsExistChangeNotificationLock	:	Boolean	=	False;

type
	TTypeParsingName	=	(tpnNormal, tpnForParsing, tpnInFolder );
	TTypeDisplayMode	=	(tdmAsIs,tdmLowerCase,tdmUpperCase,tdmFirstUpper);

	TNotifyRegister	=	record
		PIDL					:	PItemIDList;
		WatchSubTree  : Boolean;
	end;
	PNotifyRegister	=	^TNotifyRegister;

	TArrayPIDL	=	array of PItemIDList;

function SHChangeNotifyRegister(hWnd: HWND; dwFlags: Integer;	wEventMask, UMsg: LongWord; cItems: Integer; Items: PNotifyRegister) : HWND;
function SHChangeNotifyDeregister(hNotify:HWND):Boolean;
function SHChangeNotification_Lock(hMemoryMap:HWND;swProcessId:DWORD;lppidls, lpwEventID:Pointer):HWND;
function SHChangeNotification_Unlock(hLock:HWND):Boolean;

//Register function for operation with PIDL
function 	SHAlloc(cb:DWORD):PItemIDList;
function	SHFree(Item:PItemIDList):Integer;
function	SHIDFree(Item:PItemIDList):Integer;

function	SHPIDLFromFileName(Folder:PWideChar):PItemIDList;overload;
function	SHPIDLFromFileName(Folder:PChar):PItemIDList;overload;
function	SHPIDLFromFileName(Folder:String):PItemIDList;overload;
function	SHPIDLFromFileName(SF:IShellFolder;Folder:PWideChar):PItemIDList;overload;
function	SHPIDLFromFileName(SF:IShellFolder;Folder:PChar):PItemIDList;overload;
function	SHPIDLFromFileName(SF:IShellFolder;Folder:String):PItemIDList;overload;

function	SHNextPIDL(Item:PItemIDList):PItemIDList;
function	SHLastPIDL(Item:PItemIDList):PItemIDList;
function	SHFindChildPIDL(Parent,Child:PItemIDList):PItemIDList;

function	SHClonePIDL(Item:PItemIDList):PItemIDList;
function	SHCloneFirstPIDL(Item:PItemIDList):PItemIDList;
function	SHCloneLastPIDL(Item:PItemIDList):PItemIDList;
function	SHCombinePIDL(First,Second:PItemIDList):PItemIDList;
function	SHRemoveLastPIDL(Item:PItemIDList):Boolean;
function	SHParentPIDL(Item:PItemIDList):PItemIDList;
function	SHIsEqualPIDL(First,Second:PItemIDList):Boolean;
function	SHIsParentPIDL(Parent,Child:PItemIDList;Immendiate:Boolean=False):Boolean;
function	SHSizePIDL(Item:PItemIDList):LongWord;

function	SHFileNameFromPIDLWideChar(Item:PItemIDList):PWideChar;
function	SHFileNameFromPIDL(Item:PItemIDList):String;

function	StringFromStrRet(Item:PItemIDList;StrRet:TStrRet):String;
function	GetDisplayName(SF:IShellFolder;Item:PItemIDList;tpn:TTypeParsingName=tpnForParsing):String;
function	GetDescription(SF:IShellFolder;Item:PItemIDList):TSHDescriptionID;overload;
function	GetDescription(Item:PItemIDList):TSHDescriptionID;overload;
function	GetWin32FindData(SF:IShellFolder;Item:PItemIDList):TWin32FindData;
function	FolderExists(AName:String):Boolean;
function	FileExists(const AName:String):Boolean;
function	DeleteItemFromNameSpace(Parent:HWND;const Item:PItemIDList):Boolean;overload;
function	DeleteItemFromNameSpace(Parent:HWND;const Item:String):Boolean;overload;
function	CopyItemFromTo(Parent:HWND;const AFrom,ATo:PItemIDList):Boolean;overload;
function	CopyItemFromTo(Parent:HWND;const AFrom,ATo:String):Boolean;overload;
function	CopyItemFromTo(Parent:HWND;const AFrom:TArrayPIDL;const ATo:PItemIDList):Boolean;overload;

function	PathMakeUniqueName(var Buffer;dwBufSize:LongWord;Long,Path:String):Boolean;
function	PathMatchSpec(APath,ASpec:String):Boolean;

procedure	LoadShell32Library;
procedure FreeShell32Library;

const
	PIDL_Desktop					:	PItemIDList	=	nil;
	PIDL_DesktopFolder		:	PItemIDList	=	nil;
	PIDL_MyComputer				:	PItemIDList	=	nil;
	PIDL_RecycleBin				:	PItemIDList	=	nil;
	PIDL_ControlPanel			:	PItemIDList	=	nil;
	PIDL_Network					:	PItemIDList	=	nil;
	PIDL_Printers					:	PItemIDList	=	nil;
	PIDL_DialUpNetworking	:	PItemIDList	=	nil;
	PIDL_SheduledTasks		:	PItemIDList	=	nil;
	PIDL_WebFolders				:	PItemIDList	=	nil;
	PIDL_Internet         :	PItemIDList	=	nil;
//add in v0.91
	PIDL_NetHood         :	PItemIDList	=	nil;

	PIDL_Tree							: PItemIDList	=	nil;
	PIDL_List             : PItemIDList	=	nil;

	SF_Desktop					:	IShellFolder	=	nil;
	SF_DesktopFolder		: IShellFolder	= nil;

type
	TFolderType		=	(ftNone, ftDesktop, ftMyComputer,
									 ftNetworkNeighborhood, ftRecycleBin);
	TFoderSetType	= ftMyComputer..ftRecycleBin;
	TFolderTypes		=	set of TFoderSetType;

resourceString
	rsSystemFolder			=	'System Folder';
	rsDRIVE_REMOVABLE   =	'Removable Drive';
	rsDRIVE_FIXED				=	'Fixed Drive';
	rsDRIVE_REMOTE			=	'Remote (Network) Drive';
	rsDRIVE_CDROM				=	'CD ROM Drive';
	rsDRIVE_RAMDISK			=	'RAM Disk Drive';

	rsCLSIDDialUpNetworking	=	'{992CFFA0-F557-101A-88EC-00DD010CCC48}';
	rsCLSIDSheduledTasks		=	'{D6277990-4C6A-11CF-8D87-00AA0060F5BF}';
	rsCLSIDWebFolders				=	'{BDEADF00-C265-11d0-BCED-00A0C90AB50F}';

	//add in v0.94 and change in v0.96
	rsWinZip								= 'CompressedFolder\CLSID';
														//'SystemFileAssociations\.zip\CLSID';

	rsDesktop	 =	'Desktop';

var
	//add in v0.96
	CLSID_ZIPFile	: String	=	''; 	//'{E88DCCE0-B7B3-11D1-A9F0-00AA0060FA31}';

implementation

uses
	Registry;

const
	Shell32DLL 				= 'shell32.dll';
	Shell32DllHandle	:	THandle	=	0;

	sfDialUpNetworking	:	String	=	'';
	sfSheduledTasks			:	String	=	'';
	sfWebFolders				:	String	=	'';

type
	TSHChangeNotifyRegisterProc = function(hWnd: HWND; dwFlags: Integer; wEventMask, UMsg: LongWord; cItems: Integer; Items: PNotifyRegister) : HWND; stdcall;
	TSHChangeNotifyDeregisterProc = function(hNotify:HWND):Boolean;stdcall;
	TSHChangeNotification_LockProc = function(hMemoryMap:HWND;swProcessId:DWORD;lppidls,lpwEventID:Pointer):HWND;stdcall;
	TSHChangeNotification_UnlockProc = function(hLock:HWND):Boolean;stdcall;

	TSHAllocProc	=	function(cb:DWORD):PItemIDList;stdcall;
	TSHFreeProc		=	function(Item:PItemIDList):Integer;stdcall;
	TSHIDFreeProc	=	function(Item:PItemIDList):Integer;stdcall;

	TSHNextPIDLProc				=	function(Item:PItemIDList):PItemIDList;stdcall;
	TSHLastPIDLProc				=	function(Item:PItemIDList):PItemIDList;stdcall;
	TSHFindChildPIDLProc	=	function(Parent,Child:PItemIDList):PItemIDList;stdcall;
	TSHClonePIDLProc			=	function(Item:PItemIDList):PItemIDList;stdcall;
	TSHCloneFirstPIDLProc	=	function(Item:PItemIDList):PItemIDList;stdcall;
	TSHCombinePIDLProc		=	function(First,Second:PItemIDList):PItemIDList;stdcall;
	TSHRemoveLastPIDLProc	=	function(Item:PItemIDList):BOOL;stdcall;
	TSHIsEqualPIDLProc		=	function(First,Second:PItemIDList):BOOL;stdcall;
	TSHIsParentPIDLProc		=	function(Parent,Child:PItemIDList;Immendiate:BOOL):BOOL;stdcall;
	TSHSizePIDLProc				=	function(Item:PItemIDList):LongWord;stdcall;

	TPathMakeUniqueNameProc =	function(var Buffer;dwBufSize:LongWord;Short,Long,Path:PChar):BOOL;stdcall;
	TPathMatchSpecProc 			=	function(APath,ASpec:PChar):BOOL;stdcall;

	TPathSHGetPathFromIDListWProc	=	function(pidl: PItemIDList; pszPath: PWideChar): BOOL; stdcall;

const
	SHChangeNotifyRegisterProc			: TSHChangeNotifyRegisterProc			=	nil;
	SHChangeNotifyDeregisterProc		:	TSHChangeNotifyDeregisterProc		= nil;
	SHChangeNotification_LockProc		:	TSHChangeNotification_LockProc	= nil;
	SHChangeNotification_UnlockProc	:	TSHChangeNotification_UnlockProc= nil;
	SHSHGetPathFromIDListWProc			:	TPathSHGetPathFromIDListWProc		=	nil;

	SHAllocProc			:	TSHAlLocProc	=	nil;
	SHFreeProc			:	TSHFreeProc		=	nil;
	SHIDFreeProc		:	TSHIDFreeProc	=	nil;

	SHNextPIDLProc			:	TSHNextPIDLProc				=	nil;
	SHLastPIDLProc			:	TSHNextPIDLProc				=	nil;
	SHFindChildPIDLProc	:	TSHFindChildPIDLProc	=	nil;

	SHClonePIDLProc				:	TSHClonePIDLProc 			=	nil;
	SHCloneFirstPIDLProc	:	TSHCloneFirstPIDLProc	=	nil;
	SHCombinePIDLProc			:	TSHCombinePIDLProc		=	nil;
	SHRemoveLastPIDLProc	: TSHRemoveLastPIDLProc	=	nil;
	SHIsEqualPIDLProc			: TSHIsEqualPIDLProc		=	nil;
	SHIsParentPIDLProc		:	TSHIsParentPIDLProc		=	nil;
	SHSizePIDLProc				:	TSHSizePIDLProc				=	nil;

	PathMakeUniqueNameProc	:	TPathMakeUniqueNameProc	=	nil;
	PathMatchSpecProc				:	TPathMatchSpecProc			=	nil;

	IndexRegisterProc		=	2;
	IndexDeregisterProc = 4;
	IndexLockProc				= 644;
	IndexUnlockProc			=	645;

	IndexSHAllocProc		=	196;
	IndexSHFreeProc			=	195;
	IndexSHIDFreeProc		=	155;

	IndexNextPIDLProc					=	153;
	IndexLastPIDLProc					=	16;
	IndexFindChildPIDLProc		=	24;

	IndexClonePIDLProc				=	18;
	IndexCloneFirstPIDLProc		=	19;
	IndexCombinePIDLProc			=	25;
	IndexRemoveLastPIDLProc		=	17;
	IndexIsEqualPIDLProc			=	21;
	IndexIsParentPIDLProc			=	23;
	IndexSizePIDLProc					=	152;

	IndexPathMakeUniqueNameProc	=	47;
	IndexPathMatchSpecProc			=	46;

//Register function for ChangeNotify
function SHChangeNotifyRegister(hWnd: HWND;dwFlags: Integer; wEventMask, UMsg: LongWord; cItems: Integer; Items: PNotifyRegister) : HWND;

begin
	if Assigned(SHChangeNotifyRegisterProc) then
		Result:=SHChangeNotifyRegisterProc(hWnd, dwFlags, wEventMask, UMsg, cItems, Items)
	else
		Result:=0;
end;

function SHChangeNotifyDeregister(hNotify:HWND):Boolean;

begin
	if Assigned(SHChangeNotifyDeregisterProc) then
		Result:=SHChangeNotifyDeregisterProc(hNotify)
	else
		Result:=False;
end;

function SHChangeNotification_Lock(hMemoryMap:HWND;swProcessId:DWORD;lppidls,lpwEventID:Pointer) : HWND;

begin
	if Assigned(SHChangeNotification_LockProc) then
		Result:=SHChangeNotification_LockProc(hMemoryMap,swProcessID,lppidls,lpwEventID)
	else
		Result:=0;
end;

function SHChangeNotification_Unlock(hLock:HWND):Boolean;

begin
	if Assigned(SHChangeNotification_UnlockProc) then
		Result:=SHChangeNotification_UnlockProc(hLock)
	else
		Result:=False;
end;

//Register function for operation with PIDL
function SHAlloc(cb:DWORD):PItemIDList;

begin
	Result:=SHAllocProc(cb);
end;

function SHFree(Item:PItemIDList):Integer;

begin
	Result:=0;
	SHFreeProc(Item);
end;

function	SHIDFree(Item:PItemIDList):Integer;

begin
	Result:=0;
	SHIDFreeProc(Item);
end;

function	SHPIDLFromFileName(Folder:PWideChar):PItemIDList;overload;

var
	Eaten:ULONG;
	Attr:LongWord;

begin
	if Folder=nil then
		Result:=nil
	else
		if SF_Desktop.ParseDisplayName(0,nil,Folder,Eaten,Result,Attr)<>NOERROR then Result:=nil;
end;

function	SHPIDLFromFileName(Folder:PChar):PItemIDList;overload;

begin
	Result:=SHPIDLFromFileName(StringToOLEStr(Folder));
end;

function	SHPIDLFromFileName(Folder:String):PItemIDList;overload;

begin
	Result:=SHPIDLFromFileName(StringToOLEStr(PChar(Folder)));
end;

function	SHPIDLFromFileName(SF:IShellFolder;Folder:PWideChar):PItemIDList;overload;

var
	Eaten:ULONG;
	Attr:LongWord;

begin
	if SF.ParseDisplayName(0,nil,Folder,Eaten,Result,Attr)<>NOERROR then Result:=nil;
end;

function	SHPIDLFromFileName(SF:IShellFolder;Folder:PChar):PItemIDList;overload;

begin
	Result:=SHPIDLFromFileName(SF,StringToOLEStr(Folder));
end;

function	SHPIDLFromFileName(SF:IShellFolder;Folder:String):PItemIDList;overload;

begin
	Result:=SHPIDLFromFileName(SF,StringToOLEStr(PChar(Folder)));
end;

function	SHNextPIDL(Item:PItemIDList):PItemIDList;

begin
	Result:=SHNextPIDLProc(Item);
end;

function	SHLastPIDL(Item:PItemIDList):PItemIDList;

begin
	Result:=SHLastPIDLProc(Item);
end;

function	SHFindChildPIDL(Parent,Child:PItemIDList):PItemIDList;

begin
	Result:=SHFindChildPIDLProc(Parent,Child);
end;

function	SHClonePIDL(Item:PItemIDList):PItemIDList;

begin
	Result:=SHClonePIDLProc(Item);
end;

function	SHCloneFirstPIDL(Item:PItemIDList):PItemIDList;

begin
	Result:=SHCloneFirstPIDLProc(Item);
end;

function	SHCloneLastPIDL(Item:PItemIDList):PItemIDList;

begin
	Result:=SHClonePIDLProc(SHLastPIDL(Item));
end;

function	SHCombinePIDL(First,Second:PItemIDList):PItemIDList;

begin
	Result:=SHCombinePIDLProc(First,Second);
end;

function	SHRemoveLastPIDL(Item:PItemIDList):Boolean;

begin
	Result:=SHRemoveLastPIDLProc(Item);
end;

function	SHParentPIDL(Item:PItemIDList):PItemIDList;

var
	Help:PItemIDList;

begin
	Help:=SHClonePIDL(Item);
	if SHRemoveLastPIDL(Help) then Result:=SHClonePIDL(Help) else Result:=nil;
	SHFree(Help);
end;

function	SHIsEqualPIDL(First,Second:PItemIDList):Boolean;

begin
	if (First=nil) or (Second=nil) then
		Result:=False
	else
		Result:=SHIsEqualPIDLProc(First,Second);
end;

function	SHIsParentPIDL(Parent,Child:PItemIDList;Immendiate:Boolean=False):Boolean;

begin
	Result:=SHIsParentPIDLProc(Parent,Child,Immendiate);
end;

function	SHSizePIDL(Item:PItemIDList):LongWord;

begin
	Result:=SHSizePIDLProc(Item);
end;

function	SHFileNameFromPIDLWideChar(Item:PItemIDList):PWideChar;

begin
	Result:=AllocMem(MAX_PATH*SizeOf(WideChar));
	if Assigned(SHSHGetPathFromIDListWProc) then SHSHGetPathFromIDListWProc(Item,Result)
end;

function	SHFileNameFromPIDL(Item:PItemIDList):String;

var WC:PWideChar;
		Help:PChar;

begin
	if Assigned(SHSHGetPathFromIDListWProc) then begin
		WC:=SHFileNameFromPIDLWideChar(Item);
		try
			Result:=WideCharToString(WC);
		finally
			FreeMem(WC);
		end;
	end else begin
		Help:=AllocMem(MAX_PATH*SizeOf(Char));
		try
			SHGetPathFromIDList(Item,Help);
			Result:=StrPas(Help);
		finally
			FreeMem(Help);
		end;
	end;
end;

procedure	ReadRegistryCLSIDDescription;

var
	Reg: TRegistry;

begin
	Reg:=TRegistry.Create;
	try
		Reg.RootKey:=HKEY_CLASSES_ROOT;
		if Reg.OpenKeyReadOnly('CLSID\'+rsCLSIDDialUpNetworking) then	sfDialUpNetworking:=Reg.ReadString('');
		Reg.CloseKey;
		if Reg.OpenKeyReadOnly('CLSID\'+rsCLSIDSheduledTasks) then	sfSheduledTasks:=Reg.ReadString('');
		Reg.CloseKey;
		if Reg.OpenKeyReadOnly('CLSID\'+rsCLSIDWebFolders) then	sfWebFolders:=Reg.ReadString('');
		Reg.CloseKey;
		//add in v0.94 and change in v0.96
		if Reg.OpenKeyReadOnly(rsWinZip) then	CLSID_ZIPFile:=AnsiUpperCase(Reg.ReadString(''));
		Reg.CloseKey;
		//end add v0.94
	finally
		Reg.Free;
	end;
end;

procedure	MakePIDL_SpecialFolder;

var
	ID: PItemIDList;
	EnumList: IEnumIDList;
	NumIDs: LongWord;
	SF:IShellFolder;
	Name:String;

begin
	ReadRegistryCLSIDDescription;
	SHGetDesktopFolder(SF_Desktop);
	SHGetSpecialFolderLocation(0,CSIDL_DESKTOP,PIDL_Desktop);
	SHGetSpecialFolderLocation(0,CSIDL_DESKTOPDIRECTORY,PIDL_DesktopFolder);
	SHGetSpecialFolderLocation(0,CSIDL_DRIVES,PIDL_MyComputer);
	SHGetSpecialFolderLocation(0,CSIDL_BITBUCKET,PIDL_RecycleBin);
	SHGetSpecialFolderLocation(0,CSIDL_CONTROLS,PIDL_ControlPanel);
	SHGetSpecialFolderLocation(0,CSIDL_NETWORK,PIDL_Network);
	SHGetSpecialFolderLocation(0,CSIDL_PRINTERS,PIDL_Printers);
	SHGetSpecialFolderLocation(0,CSIDL_INTERNET,PIDL_Internet);
//add in v0.91
	SHGetSpecialFolderLocation(0,CSIDL_NETHOOD,PIDL_NetHood);

	SHGetSpecialFolderLocation(0,CSIDL_DESKTOP,PIDL_Tree);
	SHGetSpecialFolderLocation(0,CSIDL_DESKTOPDIRECTORY,PIDL_List);

	SF_Desktop.BindToObject(PIDL_DesktopFolder,nil,IID_IShellFolder,Pointer(SF_DesktopFolder));
	SF_Desktop.BindToObject(PIDL_MyComputer,nil,IShellFolder,Pointer(SF));
	SF.EnumObjects(0,	EnumsAll, EnumList);
	while EnumList.Next(1, ID, NumIDs)=S_OK do begin
		Name:=GetDisplayName(SF,SHLastPIDL(ID),tpnForParsing);
		if Name=sfDialUpNetworking then
			PIDL_DialUpNetworking:=SHCombinePIDL(PIDL_MyComputer,ID)
		else
			if Name=sfSheduledTasks then
				PIDL_SheduledTasks:=SHCombinePIDL(PIDL_MyComputer,ID)
			else
				if Name=sfWebFolders then
					PIDL_WebFolders:=SHCombinePIDL(PIDL_MyComputer,ID);
		SHFree(ID);
	end;
	SF:=nil;
end;

procedure	ClearPIDL_SpecialFolder;

begin
	SHFree(PIDL_Desktop);
	SHFree(PIDL_DesktopFolder);
	SHFree(PIDL_MyComputer);
	SHFree(PIDL_RecycleBin);
	SHFree(PIDL_ControlPanel);
	SHFree(PIDL_Network);
	SHFree(PIDL_Printers);
	SHFree(PIDL_DialUpNetworking);
	SHFree(PIDL_SheduledTasks);
	SHFree(PIDL_WebFolders);
	SHFree(PIDL_Internet);
//add in v0.91	
	SHFree(PIDL_NetHood);
	SHFree(PIDL_Tree);
	SHFree(PIDL_List);
	SF_DesktopFolder:=nil;
	SF_Desktop:=nil;
end;

function	StringFromStrRet(Item:PItemIDList;StrRet:TStrRet):String;

var
	P:PChar;

begin
	case StrRet.uType of
		STRRET_CSTR: SetString(Result,StrRet.cStr,LStrLen(StrRet.cStr));
		STRRET_OFFSET: begin
			if Item=nil then Result:='' else begin
				P:=PChar(@Item.mkid.abID[StrRet.uOffset-SizeOf(Item.mkid.cb)]);
				if P<>nil then SetString(Result,P,StrLen(P)) else Result:='';
			end;
		end;
		STRRET_WSTR:	if Assigned(StrRet.pOleStr) then
			Result:=WideCharToString(StrRet.pOleStr)
		else
			Result:='';
	else
		Result:='';
	end;
end;

function GetDisplayName(SF:IShellFolder;Item:PItemIDList;tpn:TTypeParsingName=tpnForParsing):String;

var
	StrRet: TStrRet;
	Flags: Integer;

begin
	Result:='';
	case tpn of
		tpnNormal			: Flags:=SHGDN_NORMAL;
		tpnForParsing	: Flags:=SHGDN_FORPARSING;
		tpnInFolder		: Flags:=SHGDN_INFOLDER;
	else
		Flags:=SHGDN_FORPARSING;
	end;
	FillChar(StrRet,Sizeof(StrRet),0);
	StrRet.uType:=STRRET_OFFSET;
	SF.GetDisplayNameOf(Item, Flags, StrRet);
  Result:=StringFromStrRet(Item,StrRet);
end;

function	GetDescription(SF:IShellFolder;Item:PItemIDList):TSHDescriptionID;

var
	ID:PItemIDList;

begin
	FillChar(Result,Sizeof(Result),0);
	ID:=SHCloneLastPIDL(Item);
	try
		SHGetDataFromIDList(SF, ID, SHGDFIL_DESCRIPTIONID, @Result, SizeOf(Result));
	finally
		SHFree(ID);
	end;
end;

function	GetDescription(Item:PItemIDList):TSHDescriptionID;overload;


begin
	FillChar(Result,Sizeof(Result),0);
	SHGetDataFromIDList(SF_Desktop, Item, SHGDFIL_DESCRIPTIONID, @Result, SizeOf(Result));
end;

function	GetWin32FindData(SF:IShellFolder;Item:PItemIDList):TWin32FindData;

var
	ID:PItemIDList;

begin
	FillChar(Result,Sizeof(Result),0);
	ID:=SHCloneLastPIDL(Item);
	try
		if SHGetDataFromIDList(SF, ID, SHGDFIL_FINDDATA, @Result, SizeOf(Result))<>NOERROR then
			SHGetDataFromIDList(SF_DesktopFolder, ID, SHGDFIL_FINDDATA, @Result, SizeOf(Result));
	finally
		SHFree(ID);
	end;
end;

function FolderExists(AName:String):Boolean;

var
	Res:ULONG;
	Attr,Size:LongWord;
	ID:PItemIDList;

begin
	Size:=Length(AName);
	if (Size>3) and (AName[Size]='\') then
		System.Delete(AName,Size,1)
	else
		if (Size=2) then AName:=System.Concat(AName,'\');
	ID:=nil;
	try
		//add and change in 0.93
		Attr:=SFGAO_CONTENTSMASK or SFGAO_FOLDER;
		Result:=SF_Desktop.ParseDisplayName(0,nil,StringToOLEStr(AName),Res,ID,Attr)=NOERROR;
		if Result then Result:=ID<>nil;
		//add in 0.92
		if Result then Result:=AnsiCompareText(AName,SHFileNameFromPIDL(ID))=0;
		if Result then Result:=(Attr and SFGAO_FOLDER)=SFGAO_FOLDER;
	finally
		SHFree(ID);
	end;
end;

function FileExists(const AName:String):Boolean;

var
	Attr:LongWord;
	Res:ULONG;
	ID:PItemIDList;

begin
	ID:=nil;
	try
		//add and change in 0.93
		Attr:=SFGAO_CONTENTSMASK or SFGAO_FILESYSTEM;
		Result:=SF_Desktop.ParseDisplayName(0,nil,StringToOLEStr(AName),Res,ID,Attr)=NOERROR;
		if Result then Result:=ID<>nil;
		if Result then Result:=AnsiCompareText(AName,SHFileNameFromPIDL(ID))=0;
		if Result then Result:=(Attr and SFGAO_FILESYSTEM)=SFGAO_FILESYSTEM;
	finally
		SHFree(ID);
	end;
end;

function DeleteItemFromNameSpace(Parent:HWND;const Item:PItemIDList):Boolean;

var DInfo:TSHFILEOPSTRUCT;
		DInfoW:TSHFILEOPSTRUCTW;

begin
	if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then begin
		FillChar(DInfoW,Sizeof(DInfoW),0);
		DInfoW.pFrom:=AllocMem(MAX_PATH*Sizeof(WideChar));
		try
			DInfoW.Wnd:=Parent;
			DInfoW.wFunc:=FO_DELETE;
			SHGetPathFromIDListW(Item,DInfoW.pFrom);
			DInfoW.fFlags:=FOF_NOCONFIRMATION;
			Result:=SHFileOperationW(DInfoW)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end else begin
		FillChar(DInfo,Sizeof(DInfo),0);
		DInfo.pFrom:=AllocMem(MAX_PATH);
		try
			DInfo.Wnd:=Parent;
			DInfo.wFunc:=FO_DELETE;
			SHGetPathFromIDList(Item,DInfo.pFrom);
			DInfo.fFlags:=FOF_NOCONFIRMATION;
			Result:=SHFileOperation(DInfo)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end;
end;

function	DeleteItemFromNameSpace(Parent:HWND;const Item:String):Boolean;overload;

var
	PIDL:PItemIDList;

begin
	PIDL:=SHPIDLFromFileName(Item);
	try
		Result:=DeleteItemFromNameSpace(Parent,PIDL);
	finally
		SHFree(PIDL);
	end;
end;

function	CopyItemFromTo(Parent:HWND;const AFrom,ATo:PItemIDList):Boolean;overload;

var DInfo:TSHFILEOPSTRUCT;
		DInfoW:TSHFILEOPSTRUCTW;

begin
	if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then begin
		FillChar(DInfoW,Sizeof(DInfoW),0);
		DInfoW.pFrom:=AllocMem(MAX_PATH*Sizeof(WideChar));
		DInfoW.pTo:=AllocMem(MAX_PATH*Sizeof(WideChar));
		try
			DInfoW.Wnd:=Parent;
			DInfoW.wFunc:=FO_COPY;
			SHGetPathFromIDListW(AFrom,DInfoW.pFrom);
			SHGetPathFromIDListW(ATo,DInfoW.pTo);
			DInfoW.fFlags:=FOF_NOCONFIRMATION or FOF_RENAMEONCOLLISION;
			Result:=SHFileOperationW(DInfoW)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end else begin
		FillChar(DInfo,Sizeof(DInfo),0);
		DInfo.pFrom:=AllocMem(MAX_PATH);
		DInfo.pTo:=AllocMem(MAX_PATH);
		try
			DInfo.Wnd:=Parent;
			DInfo.wFunc:=FO_COPY;
			SHGetPathFromIDList(AFrom,DInfo.pFrom);
			SHGetPathFromIDList(ATo,DInfo.pTo);
			DInfo.fFlags:=FOF_NOCONFIRMATION or FOF_RENAMEONCOLLISION;
			Result:=SHFileOperation(DInfo)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end;
end;

function	CopyItemFromTo(Parent:HWND;const AFrom,ATo:String):Boolean;overload;

var
	PFrom,PTo:PItemIDList;

begin
	PFrom:=SHPIDLFromFileName(AFrom);
	PTo:=SHPIDLFromFileName(ATo);
	try
		Result:=CopyItemFromTo(Parent,PFrom,PTo);
	finally
		SHFree(PFrom);
		SHFree(PTo);
	end;
end;

function	CopyItemFromTo(Parent:HWND;const AFrom:TArrayPIDL;const ATo:PItemIDList):Boolean;overload;

var DInfo:TSHFILEOPSTRUCT;
		PW:PWideChar;
		PC:PChar;
		DInfoW:TSHFILEOPSTRUCTW;
		i:Integer;

begin
	if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then begin
		FillChar(DInfoW,Sizeof(DInfoW),0);
		DInfoW.pFrom:=AllocMem(MAX_PATH*Sizeof(WideChar)*Length(AFrom)+Sizeof(WideChar)*2);
		DInfoW.pTo:=AllocMem(MAX_PATH*Sizeof(WideChar));
		try
			DInfoW.Wnd:=Parent;
			DInfoW.wFunc:=FO_COPY;
			PW:=DInfoW.pFrom;
			for i:=0 to Pred(Length(AFrom)) do begin
				SHGetPathFromIDListW(AFrom[i],PW);
				Inc(PW,Length(SHFileNameFromPIDLWideChar(AFrom[i]))+Sizeof(WideChar));
			end;
			SHGetPathFromIDListW(ATo,DInfoW.pTo);
			DInfoW.fFlags:=FOF_NOCONFIRMATION or FOF_RENAMEONCOLLISION;
			Result:=SHFileOperationW(DInfoW)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end else begin
		FillChar(DInfo,Sizeof(DInfo),0);
		DInfo.pFrom:=AllocMem(MAX_PATH*Length(AFrom)+SIzeof(Char)*2);
		DInfo.pTo:=AllocMem(MAX_PATH);
		try
			DInfo.Wnd:=Parent;
			DInfo.wFunc:=FO_COPY;
			PC:=DInfo.pFrom;
			for i:=0 to Pred(Length(AFrom)) do begin
				SHGetPathFromIDList(AFrom[i],PC);
				Inc(PC,Length(SHFileNameFromPIDL(AFrom[i]))+Sizeof(Char));
			end;
			SHGetPathFromIDList(ATo,DInfo.pTo);
			DInfo.fFlags:=FOF_NOCONFIRMATION or FOF_RENAMEONCOLLISION;
			Result:=SHFileOperation(DInfo)=0;
		finally
			FreeMem(DInfo.pFrom);
		end;
	end;
end;

function	PathMakeUniqueName(var Buffer;dwBufSize:LongWord;Long,Path:String):Boolean;

begin
	if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then
		if Assigned(PathMakeUniqueNameProc) then Result:=PathMakeUniqueNameProc(Buffer,dwBufSize,nil,PChar(StringToOleStr(Long)),PChar(StringToOleStr(Path))) else Result:=False
	else
		if Assigned(PathMakeUniqueNameProc) then Result:=PathMakeUniqueNameProc(Buffer,dwBufSize,nil,PChar(Long),PChar(Path)) else Result:=False;
end;

function	PathMatchSpec(APath,ASpec:String):Boolean;

begin
	if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then
		if Assigned(PathMatchSpecProc) then Result:=PathMatchSpecProc(PChar(StringToOleStr(APath)),PChar(StringToOleStr(ASpec))) else Result:=False
	else
		if Assigned(PathMatchSpecProc) then Result:=PathMatchSpecProc(PChar(APath),PChar(ASpec)) else Result:=False;
end;

procedure	LoadShell32Library;

begin
	Shell32DllHandle:=LoadLibrary(PChar(Shell32Dll));
	if Shell32DllHandle<=HINSTANCE_ERROR then	raise Exception.CreateFmt('%s: %s',[SysErrorMessage(GetLastError), Shell32Dll]);
	@SHChangeNotifyRegisterProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexRegisterProc)));
	@SHChangeNotifyDeregisterProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexDeregisterProc)));
	@SHChangeNotification_LockProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexLockProc)));
	@SHChangeNotification_UnlockProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexUnlockProc)));
	IsExistChangeNotificationLock:=	Assigned(SHChangeNotification_LockProc) and
																	Assigned(SHChangeNotification_UnlockProc);

	@SHAllocProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexSHAllocProc)));
	@SHFreeProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexSHFreeProc)));
	@SHIDFreeProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexSHIDFreeProc)));

	@SHNextPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexNextPIDLProc)));
	@SHLastPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexLastPIDLProc)));
	@SHFindChildPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexFindChildPIDLProc)));
	@SHClonePIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexClonePIDLProc)));
	@SHCloneFirstPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexCloneFirstPIDLProc)));
	@SHCombinePIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexCombinePIDLProc)));
	@SHRemoveLastPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexRemoveLastPIDLProc)));
	@SHIsEqualPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexIsEqualPIDLProc)));
	@SHIsParentPIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexIsParentPIDLProc)));
	@SHSizePIDLProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexSizePIDLProc)));
	@PathMakeUniqueNameProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexPathMakeUniqueNameProc)));
	@PathMatchSpecProc:=GetProcAddress(Shell32DllHandle,PChar(Integer(IndexPathMatchSpecProc)));
	@SHSHGetPathFromIDListWProc:=GetProcAddress(Shell32DllHandle,PChar('SHGetPathFromIDListW'));
end;

procedure FreeShell32Library;

begin
	if Shell32DllHandle>HINSTANCE_ERROR then FreeLibrary(Shell32DllHandle);
end;

initialization
	LoadShell32Library;
	MakePIDL_SpecialFolder;
finalization
	ClearPIDL_SpecialFolder;
	FreeShell32Library;
end.

