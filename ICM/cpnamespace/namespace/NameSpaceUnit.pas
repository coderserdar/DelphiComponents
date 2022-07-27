//
// The original Delphi code is : NameSpaceUnit.pas released 12.12.2002
// Last version: 0.98 released 23.05.2004
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
//
// This unit contains classes: TContextPopUpMenu, TNameSpaceTarget, TNameSpaceSource
// This unit contains interfaces: INameSpaceTarget, INameSpaceSource
// This unit contains functions for manipulation with TNameSpaceItem
//
// version: 0.98 released  23.05.2004
// fix bug in PopulateIDList in NameSpaceUnit
//				-	now correct work on system without compressed folder
// change definition of SFGAO_ALL in NameSpaceUnit
//		 remove SFGAO_FILESYSTEM - produce on win98 bug in TNameSpaceList
//		 				- windows function IShellFolder.GetAttributesOf donn't work correct
//							(not correct set sfgaof for removeable and fixed disk)
//							with this parameter on My Computer special folder produce a error on win98
//							I donn't know why
// 		 but namespace work correct without this parameter
//
// version: 0.97 released  26.04.2004
// fix bug in GetAttributesAndType and GetAttributes
// add GetAttribute in all MakeNameSpaceItem and MakeDesktop
// add constant SFGAO_CANMONIKER
//
// version: 0.96 released  18.04.2004
// now PNameSpaceItem contains field IsZipFile
// change function GetAttributesAndType and GetAttributes
// 		for ZIP file use fix attribute  AttributeOfZipFile	= $40400177;
//		this attribute remove SFGAO_FOLDER from attribute of zip file
//		no longer getattribute from ZIP file (it is very slow on XP)
// fix bug in GetFileInfo
//
// version: 0.95 released  29.02.2004
// fix bug in TNameSpaceTarget (not free object)
//
// version: 0.94 released  09.02.2004
// change in GetFileInfo (Attr=SFGAO_ALL)
// change in GetAttributesAndType (Attr=SFGAO_ALL)
// add type TNameSpaceBeforeNotifyEvent = function(Sender:TObject;TypeChange:LongWord;var Item1, Item2: PItemIDList):Boolean of object
// add type TNameSpaceChangeNotifyEvent	=	TNotifyEvent;
//
// version: 0.93 released  04.02.2004
// change definition SFGAO_ALL
//
// version: 0.92 released  02.02.2004
// fix bug PopulateIDList
// IEnumIDList.Next return S_FALSE for NumIDs>1
//
// version: 0.91 released  04.01.2004
// fix bug in BackGroundConcatPopUpMenu in TContextPopUpMenu (correct position of menu item)
//
// version: 0.89 released  02.11.2003
// add popup menu for background of TNameSpaceList (like Explorer - add new menu item)
//
// version: 0.88 realeased 28.09.2003
// add flag ActiveDragAndDrop default True
// some old programs (like Paradox 7.0)
// donn't work properly with COM Drag and Drop and OLE
//
// version: 0.85 released 19.06.2003
// add field (CompareSpaceList	: TClass) in TNameSpaceItem
// for thread safe sort function in TNameSpaceList
// this change produce change in next 6 function :
// PopulateIDList, MakeDesktop, MakeNameSpaceItem, MakeNameSpaceItem
// MakeNameSpaceItem, MakeNameSpaceItemWithParent
//
// version: 0.81 released 27.12.2002
// avoid problem with winzip and internet exploret 6.0
// added constant WM_UPDATECURRENT		= WM_STARTMESSAGE + 7;
// added resourcestring ecNotRegisterNotify
//
// version: 0.8
//

unit NameSpaceUnit;

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

interface

uses
	Windows, SysUtils, Classes, Controls, Messages, ActiveX, ShellAPI, ShlObj,
	Menus, Forms, Graphics, Dialogs,
	NameSpaceLibrary,	NameSpaceDetails;

const
	//add v 0.88
	ActiveDragAndDrop		=	True;

	WM_CLEARDROPTARGET	=	WM_STARTMESSAGE + 2;
	WM_SETDROPTARGET		=	WM_STARTMESSAGE + 3;
	WM_GETITEMDATA			= WM_STARTMESSAGE + 4;
	WM_RENAMEITEM				=	WM_STARTMESSAGE + 5;
	WM_SELECTCUTITEM		= WM_STARTMESSAGE + 6;
	WM_UPDATECURRENT		= WM_STARTMESSAGE + 7;
	WM_ADDNEWITEM				= WM_STARTMESSAGE + 8;

	//add in v0.97
	SFGAO_CANMONIKER = $00400000;

	SFGAO_ALL			=	SFGAO_GHOSTED or SFGAO_SHARE or SFGAO_LINK or SFGAO_CONTENTSMASK or SFGAO_CAPABILITYMASK or
	//add in v0.93
									SFGAO_FILESYSANCESTOR or SFGAO_FOLDER or SFGAO_HASSUBFOLDER;
  // remove in v0.98
  // or SFGAO_FILESYSTEM;


	scDefault			=	'';
	scCopy				=	'copy';
	scCut					=	'cut';
	scDelete			=	'delete';
	scExplore			=	'explore';
	scFind				=	'find';
	scLink				=	'link';
	scOpen				=	'open';
	scPaste				=	'paste';
	scPrint				=	'print';
	scProperties	=	'properties';
	scRename			=	'rename';

type
	//add in v0.94
	TNameSpaceBeforeNotifyEvent	=	function(Sender:TObject;TypeChange:LongWord;var Item1, Item2: PItemIDList):Boolean of object;
	TNameSpaceChangeNotifyEvent	=	procedure(Sender:TObject;TypeChange:LongWord) of object;

	PNameSpaceItem	=	^TNameSpaceItem;

	PSendMsgRecord	=	^TSendMsgRecord;
	TSendMsgRecord	=	record
		NSItem:PNameSpaceItem;
	end;

	TTypeContextMenu		=	(tcmOnlyContextMenu,tcmOnlyUserMenu,tcmConcatMenu);

	INameSpaceTarget	=	interface(IUnknown)
		['{F53D2840-9BFF-11D6-B4A5-444553540000}']
		procedure	CreateDropTarget(ANameSpace:PNameSpaceItem);
		procedure	RegisterDragDrop;
		procedure	RevokeDragDrop;

		//add in v0.95
		procedure	UnLockLockObjectExternal;
	end;

	INameSpaceSource	=	interface(IUnknown)
		['{F898EC60-B106-11D6-B4A5-444553540000}']
		procedure	DoDragDrop(SF:IShellFolder;var ID:array of PItemIDList);
	end;

	TNameSpaceItem	=	record
		ID							:	PItemIDList;
		Parent					:	PNameSpaceItem;
		ItemIndex   		:	LongWord;
		SubNameSpaces		:	TList;
		ShellFolder			:	IShellFolder;
		TotalSize				:	Int64;
		FreeAvailable		:	Int64;
		ModDate					:	TDateTime;
		SubItemText			:	TStringList;
		IconLoad				:	Boolean;
		Attribute				:	LongWord;
		ImageIndex			:	Integer;
		OverlayIndex		:	Integer;
		IsCut						:	Boolean;
		NameSpaceText		:	String;
		ShellDetails		:	IShellDetails;
		ShellFolder2		:	IShellFolder2;
		IsSpecialFolder	:	Boolean;
		//add in v 0.85
		CompareSpaceList	:	TClass;
		//add in v 0.96
		IsZipFile				:	Boolean;
	end;

	TContextPopUpMenu	=	class
	private
		FBackGroundPopUpMenu: TPopUpMenu;
		function GetPopUpMenu: TPopUpMenu;
	protected
		FOwner	:	TWinControl;
		StartAddCommand,StartCommand,EndCommand : LONGWORD;
		ContextMenu		:	IContextMenu;
		ContextMenu2  :	IContextMenu2;
		ContextMenu3  :	IContextMenu3;
		ShellFolder		:	IShellFolder;

		CountBitmap	:	Integer;
		Bitmap	:	array of TBitmap;

		function		ExecuteCommand(ACommand:LongWord):Boolean;
		function		GetCommandText(ACommand:LongWord):String;

		procedure		ShowPopUp(MENU:HMENU);virtual;

		procedure		ConcatPopUpMenu(MENU:HMENU);
		//add in v 0.89
		procedure		BackGroundConcatPopUpMenu(MENU:HMENU);
		procedure		InsertMenuItemToMenu(MI:TMenuItem;Menu:HMenu;Position:LongWord);
		procedure		InsertSeparatorToMenu(Menu:HMenu;Position:LongWord);

	public
		constructor	Create(AOwner:TWinControl);
		procedure		CreatePopUpMenu(SF:IShellFolder;var ID:array of PItemIDList;AConcatPopUp:Boolean;ARootPopUpMenu:TPopUpMenu=nil);
		function		ShellExecuteDefault(SF:IShellFolder;var ID:array of PItemIDList):Boolean;
		function		ShellExecuteCommand(SF:IShellFolder;var ID:array of PItemIDList;const ACommand:String):Boolean;
		destructor	Destroy;override;
		procedure		HandleMenuMsg(var Msg:TMessage);

		property	CommandText[ACommand:LongWord]:String read GetCommandText;
		property	Owner:TWinControl read FOwner;
		property	PopUpMenu:TPopUpMenu read GetPopUpMenu;
		property	BackGroundPopUpMenu:TPopUpMenu read FBackGroundPopUpMenu;
	end;

	TTypeDrag	=	(tdEnter,tdOver,tdLeave,tdDrop);

	TNameSpaceTarget	=	class(TInterfacedObject,INameSpaceTarget,IDropTarget)
	protected
		SaveHandle:HWND;
		FOwner:TWinControl;
		FDropTarget,DTItem	:	IDropTarget;
		FSaveObject	:	IDataObject;

		//add in v0.95
		IsUnlockExternal:Boolean;

		function	GetDropTarget:IDropTarget;
		function	DragOverItem(DTCurrent:IDropTarget;grfKeyState: Longint; pt: TPoint; var dwEffect: Longint;var dwResult:HResult):Boolean;

		//members of INameSpaceTarget
		procedure	CreateDropTarget(ANameSpace:PNameSpaceItem);
		procedure	RegisterDragDrop;
		procedure	RevokeDragDrop;

		//add in v0.95
		procedure	UnLockLockObjectExternal;

		//members of IDropTarget
		function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
		function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
		function DragLeave: HResult; stdcall;
		function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;

	public
		constructor	Create(AOwner:TWinControl);
		destructor Destroy;override;
	end;

	TNameSpaceSource	=	class(TInterfacedObject,INameSpaceSource,IDropSource)
	protected
		FOwner	:	TWinControl;
		FDataObject	:	IDataObject;

		//members of INameSpaceSource
		procedure	DoDragDrop(SF:IShellFolder;var ID:array of PItemIDList);

		//members of INameSpaceSource
		function QueryContinueDrag(fEscapePressed: BOOL;grfKeyState: Longint): HResult; stdcall;
		function GiveFeedback(dwEffect: Longint): HResult; stdcall;

	public
		constructor	Create(AOwner:TWinControl);
		destructor Destroy;override;
	end;

//fix bug in v 0.98
//change in v 0.96
//change in v 0.92
//change in v 0.85
procedure	PopulateIDList(ParentItem:PNameSpaceItem;EnumType : DWORD = EnumsAll);

//all 5 function change in v 0.85
function	MakeDesktop(ACompareSpaceList	:	TClass = nil):PNameSpaceItem;
function  MakeNameSpaceItem(SF:IShellFolder;ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;overload;
function  MakeNameSpaceItem(ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;overload;
function  MakeNameSpaceItem(const AName:String;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;overload;
function	MakeNameSpaceItemWithParent(ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;

function	GetShellIcon(SF:IShellFolder;PIDL:PItemIDList;Open:Boolean):Integer;
function	GetShellImage(Item:PNameSpaceItem;Large,Open:Boolean):Integer;
function	GetFileInfo(Item:PNameSpaceItem;var FileInfo:TSHFileInfo;Attr:LongWord=0):Integer;
function	GetClassID(Item:PNameSpaceItem):String;
function	GetAttributes(Item:PNameSpaceItem;AAttr:LongWord=SFGAO_ALL):LongWord;
function  GetAttributesAndType(Item:PNameSpaceItem):TSHFileInfo;

procedure	ClearSubItems(const List:TList);
procedure	ReScanItemIndex(NameSpace:PNameSpaceItem);
procedure	DisposeNameSpaceItem(const Item:PNameSpaceItem);
procedure	DisposeNameSpaceItemAllParent(Item:PNameSpaceItem);
procedure	RenameFolderItem(Item:PNameSpaceItem;const ANewName:String);
procedure	ChangeNameSpaceItem(Item:PNameSpaceItem;ID:PItemIDList);

function	MakeNameSpace(const APath,AFile:String):String;

resourcestring
	ecNotRegisterNotify	=	'Could not register Shell Change Notify.';

implementation

uses
	CommCtrl;

resourcestring
	erOutOfMemory		=	'Out of memory.';
	erUnknown				=	'Unknown error. Somthing bad happened.'#13'Error code: %d';
	
const
	ListofExt		 				=	'.ANI.CPL.CUR.EXE.ICO.JOB.LNK.PIF';
	AttributeOfZipFile	= $40400177;

var
	ListExtType : TStringList;

procedure	ClearSubItems(const List:TList);

var
	i : Integer;

begin
	for i:=0 to Pred(List.Count) do begin
		DisposeNameSpaceItem(PNameSpaceItem(List[i]));
		List[i]:=nil;
	end;
	List.Clear;
end;

procedure	ReScanItemIndex(NameSpace:PNameSpaceItem);

var
	i : Integer;

begin
	with NameSpace^ do begin
		i:=0;
		while i<SubNameSpaces.Count do begin
			PNameSpaceItem(SubNameSpaces[i]).ItemIndex:=i;
			Inc(i);
		end;
	end;
end;

procedure	DisposeNameSpaceItem(const Item:PNameSpaceItem);

begin
	if Item = nil then Exit;
	if Item.SubNameSpaces<>nil then begin
		ClearSubItems(Item.SubNameSpaces);
		Item.SubNameSpaces.Free;
		Item.SubNameSpaces:=nil;
	end;
	Item.SubItemText.Free;
	Item.SubItemText:=nil;
	Item.ShellFolder:=nil;
	Item.ShellDetails:=nil;
	Item.ShellFolder2:=nil;
	SHFree(Item.ID);
	Finalize(Item.NameSpaceText);
	FreeMem(Item);
end;

procedure	DisposeNameSpaceItemAllParent(Item:PNameSpaceItem);

begin
	if Item=nil then Exit;
	while Item.Parent<>nil do Item:=Item.Parent;
	DisposeNameSpaceItem(Item);
end;

function	MakeDesktop(ACompareSpaceList	:	TClass = nil):PNameSpaceItem;

begin
	Result:=AllocMem(Sizeof(TNameSpaceItem));
	Result.ID:=SHClonePIDL(PIDL_Desktop);
	Result.ShellFolder:=SF_Desktop;
	Result.SubNameSpaces:=TList.Create;
	Result.SubItemText:=TStringList.Create;
	//add in v0.97
	Result.Attribute:=GetAttributes(Result);
	//add in v 0.85
	Result.CompareSpaceList:=ACompareSpaceList;
end;

function  MakeNameSpaceItem(SF:IShellFolder;ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;

begin
	if (ID=nil) or SHIsEqualPIDL(ID,PIDL_Desktop) then
		//change in v 0.85
		Result:=MakeDesktop(ACompareSpaceList)
	else begin
		Result:=AllocMem(Sizeof(TNameSpaceItem));
		Result.ID:=SHClonePIDL(ID);
		if SF=nil then SF:=SF_Desktop;
		SF.BindToObject(SHLastPIDL(Result.ID),nil,IID_IShellFolder,Pointer(Result.ShellFolder));
		Result.SubNameSpaces:=TList.Create;
		Result.SubItemText:=TStringList.Create;
		//add in v0.97
		Result.Attribute:=GetAttributes(Result);
		//add in v 0.85
		Result.CompareSpaceList:=ACompareSpaceList;
	end;
end;

function  MakeNameSpaceItem(ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;

begin
	if (ID=nil) or SHIsEqualPIDL(ID,PIDL_Desktop) then
		//change in v 0.85
		Result:=MakeDesktop(ACompareSpaceList)
	else begin
		Result:=AllocMem(Sizeof(TNameSpaceItem));
		Result.ID:=SHClonePIDL(ID);
		SF_Desktop.BindToObject(Result.ID,nil,IID_IShellFolder,Pointer(Result.ShellFolder));
		Result.SubNameSpaces:=TList.Create;
		Result.SubItemText:=TStringList.Create;
		//add in v0.97
		Result.Attribute:=GetAttributes(Result);
		//add in v 0.85
		Result.CompareSpaceList:=ACompareSpaceList;
	end;
end;

function  MakeNameSpaceItem(const AName:String;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;overload;

var
	ID:PItemIDList;
	Attr:LongWord;
	Res:ULONG;

begin
	if SF_Desktop.ParseDisplayName(0,nil,StringToOLEStr(AName),Res,ID,Attr)=NOERROR then begin
		//change in v 0.85
		Result:=MakeNameSpaceItem(ID,ACompareSpaceList);
		SHFree(ID);
	end else
		Result:=nil;
end;

function	MakeNameSpaceItemWithParent(ID:PItemIDList;ACompareSpaceList	:	TClass = nil):PNameSpaceItem;

var
	NID,CID:PItemIDList;
	Item:PNameSpaceItem;

begin
	//change in v 0.85
	Result:=MakeDesktop(ACompareSpaceList);
	if ID=nil then ID:=PIDL_Desktop;
	NID:=ID;
	try
	while not SHIsEqualPIDL(Result.ID,ID) do begin
		Item:=AllocMem(Sizeof(TNameSpaceItem));
		CID:=SHCloneFirstPIDL(NID);
		try
			Item.ID:=SHCombinePIDL(Result.ID,CID);
		finally
			SHFree(CID);
		end;
		Result.ShellFolder.BindToObject(SHLastPIDL(Item.ID),nil,IShellFolder,Pointer(Item.ShellFolder));
		Item.SubNameSpaces:=TList.Create;
		Item.SubItemText:=TStringList.Create;
		Item.Parent:=Result;
		if Result<>nil then Item.ItemIndex:=Result.SubNameSpaces.Add(Item);
		Result:=Item;
		//add in v0.97
		Result.Attribute:=GetAttributes(Result);
		NID:=SHNextPIDL(NID);
		//add in v 0.85
		Result.CompareSpaceList:=ACompareSpaceList;
	end;
	except
	end;
end;

procedure	PopulateIDList(ParentItem:PNameSpaceItem;EnumType : DWORD = EnumsAll);

var
	ID: array[0..100] of PItemIDList;
	EnumList: IEnumIDList;
	i,NumIDs: LongWord;
	Help: PNameSpaceItem;
	Result:HResult;

begin
	if ParentItem=nil then Exit;
	ClearSubItems(ParentItem.SubNameSpaces);
	if ParentItem.ShellFolder=nil then Exit;
	ParentItem.ShellFolder.EnumObjects(0,	EnumType, EnumList);
	if EnumList=nil then Exit;
	ParentItem.SubNameSpaces.Capacity:=128;
	//change in 0.92
	repeat
		FillChar(ID,Sizeof(ID),0);
		NumIDS:=0;
		Result:=EnumList.Next(Sizeof(ID) div Sizeof(PItemIDList), ID[0], NumIDs);
		if not (((Result=S_FALSE) and (NumIDs>0)) or (Result=S_OK)) then Break;
		for i:=0 to Pred(NumIDs) do begin
			Help:=AllocMem(Sizeof(TNameSpaceItem));
			Help.ID:=SHCombinePIDL(ParentItem.ID,ID[i]);
    	Help.Parent:=ParentItem;
			ParentItem.ShellFolder.BindToObject(ID[i],nil,IID_IShellFolder,Pointer(Help.ShellFolder));
      //change in v0.98
			//add in v0.96
			Help.IsZipFile:=(CLSID_ZIPFile<>'') and (GetClassID(Help)=CLSID_ZIPFile);
			//end add in v0.96
      //end change in v0.98
			Help.ItemIndex:=ParentItem.SubNameSpaces.Add(Help);
			Help.SubNameSpaces:=TList.Create;
			Help.SubItemText:=TStringList.Create;
			Help.Attribute:=GetAttributes(Help);
			// add in v 0.85
			Help.CompareSpaceList:=ParentItem.CompareSpaceList;
			SHFree(ID[i]);
		end;
	until False;
end;

procedure	RenameFolderItem(Item:PNameSpaceItem;const ANewName:String);

var
	Parent,New:PItemIDList;
	SF:IShellFolder;

begin
	if Item.Parent<>nil then SF:=Item.Parent.ShellFolder else SF:=SF_Desktop;
	if SF.SetNameOf(0,SHLastPIDL(Item.ID),StringToOLEStr(ANewName),EnumsFolder,New)=NOERROR then begin
		Parent:=SHParentPIDL(Item.ID);
		try
			SHFree(Item.ID);
			Item.ID:=SHCombinePIDL(Parent,New);
			Item.ShellFolder:=nil;
			SF.BindToObject(New,nil,IID_IShellFolder,Pointer(Item.ShellFolder));
		finally
			SHFree(Parent);
			SHFree(New);
		end;
	end;
end;

procedure	ChangeNameSpaceItem(Item:PNameSpaceItem;ID:PItemIDList);

begin
	SHFree(Item.ID);
	Item.ID:=SHPIDLFromFileName(SHFileNameFromPIDL(ID));
	Item.ShellFolder:=nil;
	SF_Desktop.BindToObject(Item.ID,nil,IID_IShellFolder,Pointer(Item.ShellFolder));
end;

function	MakeNameSpace(const APath,AFile:String):String;

begin
	if APath[Length(APath)]<>'\' then Result:='%s\%s' else Result:='%s%s';
	Result:=Format(Result,[APath,AFile]);
end;

function	GetShellIcon(SF:IShellFolder;PIDL:PItemIDList;Open:Boolean):Integer;

var
	SI:IShellIcon;

begin
	Result:=-1;
	if SF.QueryInterface(IShellIcon,SI)=S_OK then
		if Open then
			SI.GetIconOf(PIDL,GIL_FORSHELL or GIL_OPENICON,Result)
		else
			SI.GetIconOf(PIDL,GIL_FORSHELL,Result);
end;

function	GetShellImage(Item:PNameSpaceItem;Large,Open:Boolean):Integer;

var
	FileInfo: TSHFileInfo;
	Flags: Integer;
	SF:IShellFolder;

begin
	if Item.Parent<>nil then SF:=Item.Parent.ShellFolder else SF:=nil;
	if SF=nil then SF:=SF_Desktop;
	Result:=GetShellIcon(SF,SHLastPIDL(Item.ID),Open);
	if Result=-1 then begin
		Flags:=SHGFI_PIDL or SHGFI_SYSICONINDEX;
		if Open then Flags:=Flags or SHGFI_OPENICON;
		if Large then Flags:=Flags or SHGFI_LARGEICON	else Flags:=Flags or SHGFI_SMALLICON;
		FillChar(FileInfo,Sizeof(FileInfo),0);
		if SHGetFileInfo(Pointer(Item.ID),0,FileInfo,Sizeof(FileInfo),Flags)=0 then
			Result:=-1
		else
			Result:=FileInfo.iIcon;
	end;
end;

function	GetFileInfo(Item:PNameSpaceItem;var FileInfo:TSHFileInfo;Attr:LongWord=0):Integer;

var
	AttrFile:LongWord;
	SF:IShellFolder;
	PIDL:PItemIDList;
	PSH:PSHFileInfo;
	Ext:String;
	Pos:Integer;
	Flag:Boolean;

begin
	Result:=-1;
	FillChar(FileInfo,Sizeof(FileInfo),0);
	if Attr=0 then begin
		if Item.Parent<>nil then SF:=Item.Parent.ShellFolder else SF:=nil;
		if SF=nil then SF:=SF_Desktop;
		//change in v0.94
		Attr:=SFGAO_ALL;
		PIDL:=SHLastPIDL(Item.ID);
		SF.GetAttributesOf(1,PIDL,Attr);
		Item.Attribute:=Attr;
	end;
	//add in v0.96
	if (Attr and SFGAO_FOLDER)=SFGAO_FOLDER then begin
		Pos:=-1;
		Flag:=False;
	end else begin
	//end add in v0.96	
		Ext:=UpperCase(ExtractFileExt(SHFileNameFromPIDL(Item.ID)));
		System.Delete(Ext,1,1);
		Flag:=(Ext<>'') and (System.Pos(Ext,ListOfExt)=0);
		if Flag then Pos:=ListExtType.IndexOf(Ext) else Pos:=-1;
  end;
	if Pos=-1 then begin
		if BOOL(Attr and SFGAO_FILESYSTEM) then begin
			if BOOL(Attr and SFGAO_FOLDER) then
				AttrFile:=FILE_ATTRIBUTE_DIRECTORY
			else
				AttrFile:=FILE_ATTRIBUTE_NORMAL;
			if SHGetFileInfo(Pointer(Item.ID),AttrFile,FileInfo,SizeOf(FileInfo),SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX)<>0 then Result:=FileInfo.iIcon else FileInfo.iIcon:=Result;
		end else
			if SHGetFileInfo(Pointer(Item.ID),0,FileInfo,SizeOf(FileInfo),SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_SYSICONINDEX)<>0 then Result:=FileInfo.iIcon else FileInfo.iIcon:=Result;
		if Flag then begin
			PSH:=AllocMem(Sizeof(TSHFileInfo));
			PSH^:=FileInfo;
			ListExtType.AddObject(Ext,TObject(PSH));
		end;
	end else begin
		FileInfo:=PSHFileInfo(ListExtType.Objects[Pos])^;
		Result:=FileInfo.iIcon;
	end;
	FileInfo.dwAttributes:=Attr;
end;

function	GetClassID(Item:PNameSpaceItem):String;

var
	IP:IPersistFile;
	classID: TCLSID;
	P:POLEStr;

begin
	Result:='';
	if Item.ShellFolder<>nil then begin
		Item.ShellFolder.QueryInterface(IPersistFile,IP);
		if IP<>nil then begin
			IP.GetClassID(classID);
			StringFromCLSID(classID,P);
			Result:=AnsiUpperCase(P);
		end;
	end;
end;

function	GetAttributes(Item:PNameSpaceItem;AAttr:LongWord=SFGAO_ALL):LongWord;

var
	SF:IShellFolder;
	PIDL:PItemIDList;
	PAttr:LongWord;

begin
	//add in v0.96
	if Item.IsZipFile then	begin
		if Item.Parent<>nil then PAttr:=Item.Parent.Attribute else PAttr:=AttributeOfZipFile;
		PAttr:=$FFFFFFFF-(PAttr and (not SFGAO_CAPABILITYMASK));
		//change in v0.97
		Result:=AttributeOfZipFile and (PAttr or SFGAO_CANMONIKER or SFGAO_FILESYSTEM);
		Exit;
	end;
	//end add in v0.96
	if Item.Parent<>nil then SF:=Item.Parent.ShellFolder else SF:=SF_Desktop;
	Result:=AAttr;
	PIDL:=SHLastPIDL(Item.ID);
	SF.GetAttributesOf(1,PIDL,Result);
end;

function  GetAttributesAndType(Item:PNameSpaceItem):TSHFileInfo;

var
	PAttr,Attr:LongWord;
	SF:IShellFolder;
	PIDL:PItemIDList;

begin
	FillChar(Result,Sizeof(Result),0);
	if Item.Parent<>nil then SF:=Item.Parent.ShellFolder else SF:=SF_Desktop;
	//change in v0.96
	if Item.IsZipFile then begin
		if Item.Parent<>nil then PAttr:=Item.Parent.Attribute else PAttr:=AttributeOfZipFile;
		PAttr:=$FFFFFFFF-(PAttr and (not SFGAO_CAPABILITYMASK));
		//change in v0.97
		Attr:=AttributeOfZipFile and (PAttr or SFGAO_CANMONIKER or SFGAO_FILESYSTEM);
	end else begin
		//end change in v0.96
		//change in v0.94
		Attr:=SFGAO_ALL;
		PIDL:=SHLastPIDL(Item.ID);
		SF.GetAttributesOf(1,PIDL,Attr);
	end;
	SHGetFileInfo(Pointer(Item.ID),0,Result,SizeOf(Result),SHGFI_PIDL or SHGFI_TYPENAME);
	Result.dwAttributes:=Attr;
end;

{ TContextPopUpMenu }
const
	SizeBuffer	=	1024;

type
	TFoo	=	class(TWinControl);

constructor TContextPopUpMenu.Create(AOwner: TWinControl);

begin
	inherited Create;
	FOwner:=AOwner;
	StartCommand:=1;
	StartAddCommand:=$8000;
	EndCommand:=$FFFF;
end;

procedure TContextPopUpMenu.CreatePopUpMenu(SF:IShellFolder;var ID:array of PItemIDList;AConcatPopUp:Boolean;ARootPopUpMenu:TPopUpMenu=nil);

//change in v 0.89

var
	Menu:HMenu;
	QCFlag : UINT;

begin
	FBackGroundPopUpMenu:=ARootPopUpMenu;
	if SF=nil then SF:=SF_Desktop;
	ShellFolder:=SF;
	if ARootPopUpMenu<>nil then begin
		QCFlag:=CMF_EXPLORE;
		ShellFolder.CreateViewObject(0,IContextMenu,Pointer(ContextMenu));
	end else begin
		QCFlag:=CMF_EXPLORE or CMF_CANRENAME;
		ShellFolder.GetUIObjectOf(0,Length(ID),ID[0],IContextMenu,nil,Pointer(ContextMenu));
	end;
	if ContextMenu=nil then Exit;
	try
		ContextMenu.QueryInterface(IContextMenu2,ContextMenu2);
		ContextMenu.QueryInterface(IContextMenu3,ContextMenu3);
	except
	end;
	Menu:=Windows.CreatePopUpMenu;
	try
		if ContextMenu3<>nil then
			ContextMenu3.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,QCFlag)
		else
			if ContextMenu2<>nil then
				ContextMenu2.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,QCFlag)
			else
				ContextMenu.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,QCFlag);
			if AConcatPopUp then
				if ARootPopUpMenu<>nil then BackGroundConcatPopUpMenu(Menu) else ConcatPopUpMenu(Menu);
		ShowPopUp(Menu)
	finally
		DestroyMenu(Menu);
	end;
end;

destructor TContextPopUpMenu.Destroy;

var
	i:Integer;

begin
	ContextMenu:=nil;
	ContextMenu2:=nil;
	ContextMenu3:=nil;
	ShellFolder:=nil;
	for i:=0 to Pred(CountBitmap) do Bitmap[i].Free;
	Finalize(Bitmap);
	inherited Destroy;
end;

function TContextPopUpMenu.ExecuteCommand(ACommand: LongWord):Boolean;

//change in v 0.89

var
	INFO:CMINVOKECOMMANDINFO;
	Comm:LongWord;
	AResult:HResult;

begin
	Result:=False;
	if ContextMenu=nil then Exit;
	if ACommand<>DWORD(-1) then begin
		if ACommand>=StartAddCommand then begin
			if BackGroundPopUpMenu<>nil then
				BackGroundPopUpMenu.DispatchCommand(ACommand-StartAddCommand)
			else
				PopUpMenu.DispatchCommand(ACommand-StartAddCommand);
			Result:=True;
		end else begin
			if BackGroundPopUpMenu<>nil then SendMessage(Owner.Handle,WM_ADDNEWITEM,0,1);		
			Comm:=(ACommand-StartCommand) and $FF;
			if CommandText[Comm]=scRename then begin
				PostMessage(Owner.Handle,WM_RENAMEITEM,0,0);
				Result:=True;
			end else begin
				FillChar(INFO,SIzeof(INFO),0);
				INFO.cbSize:=Sizeof(Info);
				INFO.hwnd:=Application.Handle;
				INFO.lpVerb:=MAKEINTRESOURCE(Comm);
				INFO.nShow:=SW_SHOWMAXIMIZED;
				if ContextMenu3<>nil then
					AResult:=ContextMenu3.InvokeCommand(INFO)
				else
					if ContextMenu2<>nil then
						AResult:=ContextMenu2.InvokeCommand(INFO)
					else
						AResult:=ContextMenu.InvokeCommand(INFO);
				if (AResult=S_OK) and ((CommandText[Comm]=scCut) or (CommandText[Comm]=scCopy)) then PostMessage(Owner.Handle,WM_SELECTCUTITEM,0,Integer(CommandText[Comm]=scCut));
				Result:=AResult=S_OK;
				if BackGroundPopUpMenu<>nil then SendMessage(Owner.Handle,WM_ADDNEWITEM,0,0);
			end;
		end;
		Application.ProcessMessages;
	end;
end;

function TContextPopUpMenu.GetCommandText(ACommand: LongWord): String;

var
	Buffer:PChar;

begin
	if ContextMenu=nil then Exit;
	Buffer:=AllocMem(SizeBuffer);
	try
		if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then begin
			ContextMenu.GetCommandString(ACommand,GCS_VERBW,nil,Buffer,SizeBuffer);
			Result:=WideCharToString(PWideChar(Buffer));
		end else begin
			ContextMenu.GetCommandString(ACommand,GCS_VERB,nil,Buffer,SizeBuffer);
			Result:=StrPas(Buffer);
		end;
	finally
		FreeMem(Buffer);
	end;
end;

procedure TContextPopUpMenu.ShowPopUp(MENU: HMENU);

var
	Point:TPoint;

begin
	GetCursorPos(Point);
	ExecuteCommand(LongWord(Integer(TrackPopupMenuEx(MENU,TPM_LEFTALIGN or TPM_RETURNCMD or
										TPM_RIGHTBUTTON or TPM_LEFTBUTTON or TPM_RECURSE, Point.X, Point.Y,TWinControl(Owner).Handle , nil))));
end;

procedure TContextPopUpMenu.HandleMenuMsg(var Msg: TMessage);

begin
	if ContextMenu3<>nil then
		ContextMenu3.HandleMenuMsg2(Msg.Msg,Msg.WParam,Msg.LParam,Msg.Result)
	else
		if ContextMenu2<>nil then ContextMenu2.HandleMenuMsg(Msg.Msg,Msg.WParam,Msg.LParam)
end;

const
	RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;

const
	IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
	IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
	IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
	IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
	IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
	IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);

procedure TContextPopUpMenu.ConcatPopUpMenu(MENU: HMENU);

var
	i,Count:Integer;

begin
	if PopUpMenu=nil then Exit;
	if PopUpMenu.Items.Count>0 then begin
		Count:=GetMenuItemCount(Menu);
		if Count>0 then InsertSeparatorToMenu(Menu,LongWord(-1));
		Count:=GetMenuItemCount(Menu);
		for i:=0 to Pred(PopUpMenu.Items.Count) do InsertMenuItemToMenu(PopUpMenu.Items[i],Menu,Count+i);
	end;
end;

procedure TContextPopUpMenu.InsertMenuItemToMenu(MI: TMenuItem; Menu: HMenu; Position: LongWord);

var
	INFO: MENUITEMINFO;
	i:Integer;
	Caption: string;

begin
	Caption:=MI.Caption;
	if Caption='-' then
		InsertSeparatorToMenu(Menu,Position)
	else begin
		FillChar(Info,Sizeof(Info),0);
		Info.cbSize:=Sizeof(Info);
		Info.fMask:=MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
		if MI.Count>0 then begin
			Info.hSubMenu:=Windows.CreatePopUpMenu;
			for i:=0 to Pred(MI.Count) do InsertMenuItemToMenu(MI.Items[i],Info.hSubMenu,0);
		end else
			if MI.ShortCut<>scNone then	Caption:=Caption+#9+ShortCutToText(MI.ShortCut);
		Info.fType:=IRadios[MI.RadioItem] or IBreaks[MI.Break] or	IRTL[PopUpMenu.IsRightToLeft];
		Info.fState:=IChecks[(MI.Checked) or (MI.ImageIndex>-1)] or IEnables[MI.Enabled] or IDefaults[MI.Default];
		Info.wID:=StartAddCommand+MI.Command;
		if MI.ImageIndex>-1 then begin
			Inc(CountBitmap);
			SetLength(Bitmap,CountBitmap);
			Bitmap[Pred(CountBitmap)]:=TBitmap.Create;
			PopUpMenu.Images.GetBitmap(MI.ImageIndex,Bitmap[Pred(CountBitmap)]);
			Info.hbmpChecked:=Bitmap[Pred(CountBitmap)].Handle;
		end;
		Info.dwTypeData:=PChar(Caption);
		InsertMenuItem(Menu, Position, True, Info);
	end;
end;

procedure TContextPopUpMenu.InsertSeparatorToMenu(Menu: HMenu; Position: LongWord);

var
	INFO: MENUITEMINFO;
	Caption: string;

begin
	FillChar(Info,Sizeof(Info),0);
	Info.cbSize:=Sizeof(Info);
	Info.fMask:=MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
	Caption:='-';
	Info.fType:=MFT_SEPARATOR;
	Info.fState:=MFS_ENABLED;
	Info.wID:=0;
	Info.hbmpChecked:=0;
	Info.hbmpUnchecked:=0;
	Info.dwTypeData:=PChar(Caption);
	InsertMenuItem(Menu, Position, True, Info);
end;

function TContextPopUpMenu.ShellExecuteDefault(SF: IShellFolder;	var ID: array of PItemIDList):Boolean;

var
	Menu	:	HMenu;

begin
	Result:=False;
	if SF=nil then SF:=SF_Desktop;
	ShellFolder:=SF;
	ShellFolder.GetUIObjectOf(0,Length(ID),ID[0],IContextMenu,nil,Pointer(ContextMenu));
	if ContextMenu=nil then Exit;
	try
		ContextMenu.QueryInterface(IContextMenu2,ContextMenu2);
		ContextMenu.QueryInterface(IContextMenu3,ContextMenu3);
	except
	end;
	Menu:=Windows.CreatePopUpMenu;
	try
		if ContextMenu3<>nil then
			ContextMenu3.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,CMF_EXPLORE or CMF_CANRENAME)
		else
			if ContextMenu2<>nil then
				ContextMenu2.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,CMF_EXPLORE or CMF_CANRENAME)
			else
				ContextMenu.QueryContextMenu(Menu,DWORD(0),StartCommand,EndCommand,CMF_EXPLORE or CMF_CANRENAME);
		Result:=ExecuteCommand(GetMenuDefaultItem(Menu,0,GMDI_GOINTOPOPUPS));
	finally
		DestroyMenu(Menu);
	end;
end;

function TContextPopUpMenu.ShellExecuteCommand(SF: IShellFolder; var ID: array of PItemIDList; const ACommand: String):Boolean;

var
	INFO:CMINVOKECOMMANDINFO;
	AResult:HResult;

begin
	if ACommand=scRename then begin Result:=True;PostMessage(Owner.Handle,WM_RENAMEITEM,0,0);Exit end;
	if ACommand='' then	begin Result:=ShellExecuteDefault(SF,ID);Exit end;
	Result:=False;
	if SF=nil then SF:=SF_Desktop;
	ShellFolder:=SF;
	ShellFolder.GetUIObjectOf(0,Length(ID),ID[0],IContextMenu,nil,Pointer(ContextMenu));
	if ContextMenu=nil then Exit;
	try
		ContextMenu.QueryInterface(IContextMenu2,ContextMenu2);
		ContextMenu.QueryInterface(IContextMenu3,ContextMenu3);
	except
	end;
	FillChar(INFO,SIzeof(INFO),0);
	INFO.cbSize:=Sizeof(Info);
	INFO.hwnd:=Application.Handle;
	INFO.lpVerb:=PChar(ACommand);
	INFO.nShow:=SW_SHOWMAXIMIZED;
	if ContextMenu3<>nil then
		AResult:=ContextMenu3.InvokeCommand(INFO)
		else
			if ContextMenu2<>nil then
				AResult:=ContextMenu2.InvokeCommand(INFO)
			else
				AResult:=ContextMenu.InvokeCommand(INFO);
	if (AResult=S_OK) and ((ACommand=scCut) or (ACommand=scCopy)) then PostMessage(Owner.Handle,WM_SELECTCUTITEM,0,Integer(ACommand=scCut));
	Result:=AResult=S_OK;
end;

function TContextPopUpMenu.GetPopUpMenu: TPopUpMenu;

begin
	Result:=TFoo(Owner).PopUpMenu;
end;

//add in v 0.89
procedure TContextPopUpMenu.BackGroundConcatPopUpMenu(MENU: HMENU);

var
	i,Count:Integer;

begin
	if BackGroundPopUpMenu=nil then Exit;
	if BackGroundPopUpMenu.Items.Count>0 then begin
		Count:=GetMenuItemCount(Menu);
		if Count>0 then InsertSeparatorToMenu(Menu,LongWord(-1));
		Count:=GetMenuItemCount(Menu);
		//change in v0.91
		for i:=0 to Pred(BackGroundPopUpMenu.Items.Count) do InsertMenuItemToMenu(BackGroundPopUpMenu.Items[i],Menu,Count+i);
	end;
end;

{ TNameSpaceTarget }
constructor TNameSpaceTarget.Create(AOwner: TWinControl);

begin
	inherited Create;
	FOwner:=AOwner;
	ActiveX.CoLockObjectExternal(Self,True,False);
	RegisterDragDrop;
end;

procedure TNameSpaceTarget.CreateDropTarget(ANameSpace: PNameSpaceItem);

begin
	FDropTarget:=nil;
	DTItem:=nil;
	ANameSpace.ShellFolder.CreateViewObject(0,IDropTarget,Pointer(FDropTarget));
end;

procedure TNameSpaceTarget.UnLockLockObjectExternal;

begin
	IsUnlockExternal:=True;
	RevokeDragDrop;	
	ActiveX.CoLockObjectExternal(Self,False,True);
end;

destructor TNameSpaceTarget.Destroy;

begin
	//change in v0.95
	if not IsUnlockExternal then UnLockLockObjectExternal;
	inherited;
end;

function TNameSpaceTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;

var
	Effect:Integer;

begin
	Result:=E_NOTIMPL;
	FSaveObject:=dataObj;
	SendMessage(FOwner.Handle,WM_SETDROPTARGET,0,Integer(@pt));
	Effect:=dwEffect;
	if FDropTarget<>nil then
		Result:=FDropTarget.DragEnter(dataObj,grfKeyState,pt,Effect)
	else
		Effect:=DROPEFFECT_NONE;
	dwEffect:=Effect;
end;

function TNameSpaceTarget.DragLeave: HResult;

begin
	Result:=E_NOTIMPL;
	FSaveObject:=nil;
	if FDropTarget<>nil then Result:=FDropTarget.DragLeave;
	PostMessage(FOwner.Handle,WM_CLEARDROPTARGET,0,0);
end;

function TNameSpaceTarget.DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;

var
	Effect:Integer;

begin
	Result:=E_NOTIMPL;
	SendMessage(FOwner.Handle,WM_SETDROPTARGET,0,Integer(@Pt));
	Effect:=dwEffect;
	if not DragOverItem(GetDropTarget,grfKeyState,pt,Effect,Result) then
		if FDropTarget<>nil then
			Result:=FDropTarget.DragOver(grfKeyState,pt,Effect)
		else
			Effect:=DROPEFFECT_NONE;
	dwEffect:=Effect;
end;

function TNameSpaceTarget.DragOverItem(DTCurrent: IDropTarget; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;var dwResult:HResult): Boolean;

begin
	Result:=False;
	if DTItem=DTCurrent then begin
		if (DTItem=nil) then Exit;
		dwResult:=DTItem.DragOver(grfKeyState,pt,dwEffect);
		Result:=True;
	end else begin
		if (DTItem<>nil) then DTItem.DragLeave;
		DTItem:=nil;
		DTItem:=GetDropTarget;
		if DTItem<>nil then begin
			dwResult:=DTItem.DragEnter(FSaveObject,grfKeyState,pt,dwEffect);
			Result:=True;
		end;
	end;
end;

function TNameSpaceTarget.Drop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;

var
	Effect:Integer;

begin
	Result:=E_NOTIMPL;
	SendMessage(FOwner.Handle,WM_SETDROPTARGET,0,Integer(@Pt));
	Effect:=dwEffect;
	if DTItem<>nil then
		Result:=DTItem.Drop(dataObj,grfKeyState,pt,Effect)
	else
		if FDropTarget<>nil then
			Result:=FDropTarget.Drop(dataObj,grfKeyState,pt,Effect)
		else
			Effect:=DROPEFFECT_NONE;
	FSaveObject:=nil;
	PostMessage(FOwner.Handle,WM_CLEARDROPTARGET,0,0);
	dwEffect:=Effect;
end;

function TNameSpaceTarget.GetDropTarget: IDropTarget;

var
	Last:PItemIDList;
	Smr:TSendMsgRecord;

begin
	Result:=nil; FillChar(Smr,Sizeof(Smr),0);
	SendMessage(FOwner.Handle,WM_GETITEMDATA,0,Integer(@Smr));
	with Smr do begin
		if NSItem=nil then Exit;
		if NSItem.Parent=nil then Exit;
		if NSItem.Parent.ShellFolder=nil then Exit;
		Last:=SHLastPIDL(NSItem.ID);
		NSItem.Parent.ShellFolder.GetUIObjectOf(0,1,Last,IDropTarget,nil,Pointer(Result));
	end;
end;

procedure TNameSpaceTarget.RegisterDragDrop;

begin
	if FOwner.HandleAllocated then begin
		SaveHandle:=FOwner.Handle;
		ActiveX.RegisterDragDrop(SaveHandle,Self);
	end;
	DTItem:=nil;
end;

procedure TNameSpaceTarget.RevokeDragDrop;

begin
	if SaveHandle<>0 then	ActiveX.RevokeDragDrop(SaveHandle);
	FDropTarget:=nil;
	SaveHandle:=0;
	DTItem:=nil;
end;

{ TNameSpaceSource }

constructor TNameSpaceSource.Create(AOwner: TWinControl);

begin
	inherited Create;
	FOwner:=AOwner;
end;

destructor TNameSpaceSource.Destroy;

begin
	FDataObject:=nil;
	inherited Destroy;
end;

procedure TNameSpaceSource.DoDragDrop(SF:IShellFolder;var ID:array of PItemIDList);

var
	Result:HResult;
	dwEffect:LongInt;
	dwOk:DWORD;

begin
	if SF=nil then SF:=SF_Desktop;
	SF.GetUIObjectOf(0,Length(ID),ID[0],IDataObject,nil,Pointer(FDataObject));
	try
		if (FDataObject=nil) then Exit;
		dwEffect:=0;
		dwOk:=DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK or DROPEFFECT_SCROLL;
		Result:=ActiveX.DoDragDrop(FDataObject,Self,dwOk,dwEffect);
		case Result of
			DRAGDROP_S_DROP,DRAGDROP_S_CANCEL : ;
			E_OUTOFMEMORY : raise Exception.Create(erOutOfMemory);
		else
			raise Exception.Create(Format(erUnknown,[Result]))
		end;
	finally
		FDataObject:=nil;
	end;
end;

function TNameSpaceSource.GiveFeedback(dwEffect: Integer): HResult;

begin
	if BOOL(dwEffect and DROPEFFECT_MOVE) or
		 BOOL(dwEffect and DROPEFFECT_COPY) or
		 BOOL(dwEffect and DROPEFFECT_COPY) or
		 BOOL(dwEffect and DROPEFFECT_SCROLL) or (dwEffect=DROPEFFECT_NONE) then
		Result:=DRAGDROP_S_USEDEFAULTCURSORS
	else
		Result:=S_OK;
end;

function TNameSpaceSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Integer): HResult;

begin
	if fEscapePressed then
		Result:=DRAGDROP_S_CANCEL
	else
		if (grfKeyState and (MK_LBUTTON OR MK_RBUTTON))=0 then
			Result:=DRAGDROP_S_DROP
		else
			Result:=S_OK;
end;

var
	i:Integer;

initialization
	//change in v 0.88
	if ActiveDragAndDrop then OleInitialize(nil);

	ListExtType:=TStringList.Create;
	ListExtType.Sorted:=True;
	ListExtType.Capacity:=128;
finalization
	i:=0;
	while i<ListExtType.Count do begin
		FreeMem(Pointer(ListExtType.Objects[i]));
		Inc(i);
	end;
	ListExtType.Free;

	//change in v 0.88
	if ActiveDragAndDrop then OleUnInitialize;
end.

