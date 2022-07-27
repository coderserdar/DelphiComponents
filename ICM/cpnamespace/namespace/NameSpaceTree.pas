//
// The original Delphi code is : NameSpaceTree.pas released 12.12.2002
// Last version: 0.95 released 29.02.2004
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
// This unit contains class TNameSpaceTree (Tree View of Folders)
//
// version: 0.95 realeased 29.02.2004
// fix bug - not free FDropTargetRoot on destroy TNameSpaceTree
//
// version: 0.94 realeased 09.02.2004
// add two Event
//		OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent
//		OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent (propose joel joly)
// change HandleChangeNotify
//
// version: 0.92 realeased 02.02.2004
// change in MakeChildFolderFor (replace folderExists with fileExists)
//
// version: 0.91 realeased 04.01.2004
// fix bug in methods MakeChildFolderFor
//	- windows fuction PathMakeUniqueName return in Buffer all path not only name folder
// fix bug in methods AddFolderToTree
//	- now add map network drive
// synchronize TreeView, ComboEdit and ListView
//
// version: 0.88 realeased 28.09.2003
// add flag ActiveDragAndDrop
// some old programs (like Paradox 7.0)
// donn't work properly with COM Drag and Drop and OLE
//
// version: 0.83 released 16.03.2003
// change AnsiUpperCase with NSWideUpperCaseA
// change AnsiLowerCase with NSWideLowerCaseA
// for TypeDisplayMode=tdmFirstUpper call now Result:=NSWideFirstUpperA(Result);
// this is for better support DBCS
//
// version: 0.81 released 27.12.2002
// move error text in CreateNotifyEvent in resourcestring
//
// version: 0.8
//

unit NameSpaceTree;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ActiveX, ComCtrls, Commctrl, ShellAPI, ShlObj, Menus, ComObj, StdCtrls,
	extctrls,
	NameSpaceLibrary, NameSpaceUnit, NameSpaceList;

{$I SetTypeCompiler.inc}

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

type
	TNameSpaceTree = class(TCustomTreeView)
	private
		FHiddenFolder	: Boolean;
		FDefaultNewFolderName: String;
		FRespondOnChangeNotify: Boolean;
		FFolderEdit: TEdit;
		FFolderStaticText: TStaticText;
		FTypeDisplayMode: TTypeDisplayMode;
		FNameSpaceList: TNameSpaceList;
		FTypePopUpMenu: TTypeContextMenu;
		FLockRefresh	:	Integer;
		FOnBeforeNotifyEvent: TNameSpaceBeforeNotifyEvent;
		FOnChangeNotifyEvent: TNameSpaceChangeNotifyEvent;
		procedure SetHiddenFolder(const Value: Boolean);
		procedure SetRespondOnChangeNotify(const Value: Boolean);
		function GetCurrentFolder: String;
		procedure SetCurrentFolder(Value: String);
		procedure SetFolderEdit(const Value: TEdit);
		procedure SetFolderStaticText(const Value: TStaticText);
		procedure SetTypeDisplayMode(const Value: TTypeDisplayMode);
		procedure SetNameSpaceList(const Value: TNameSpaceList);
		procedure SetTypePopUpMenu(const Value: TTypeContextMenu);
		{ Private declarations }
	protected
		{ Protected declarations }
		FImageList		:	TImageList;
		NameSpace			:	PNameSpaceItem;
		FDesktopNode	:	TTreeNode;
		FlagControl		:	Boolean;
		ContextPopUp	:	TContextPopUpMenu;
		LastCut				:	TTreeNode;

		FNotifyHandle 	: HWND;
		FNotifyRegister :	TNotifyRegister;
		FDropTargetRoot	:	INameSpaceTarget;
		FDropSourceRoot	:	INameSpaceSource;

		SaveTimerNode		:	TTreeNode;
		Timer	:	TTimer;

		//add in v0.95
		procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

		procedure	WMINITMENUPOPUP(var Msg:TMessage); message WM_INITMENUPOPUP;
		procedure	WMDRAWITEM(var Msg: TMessage); message WM_DRAWITEM;
		procedure	WMMEASUREITEM(var Msg:TMessage); message WM_MEASUREITEM;

		procedure	WMGETITEMDATA(var Msg:TMessage); message WM_GETITEMDATA;
		procedure	WMSETDROPTARGET(var Msg:TMessage); message WM_SETDROPTARGET;
		procedure	WMCLEARDROPTARGET(var Msg:TMessage); message WM_CLEARDROPTARGET;
		procedure	WMRENAMEITEM(var Msg:TMessage); message WM_RENAMEITEM;
		procedure	WMSELECTCUTITEM(var Msg:TMessage); message WM_SELECTCUTITEM;

		procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
		procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
{$IFDEF DELPHI5_UP}
		procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
{$ELSE}
		procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
{$ENDIF}
		procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
		procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;

		procedure	CompareData(Sender: TObject; Node1, Node2: TTreeNode;	Data: Integer; var Compare: Integer);
		function 	CanExpand(Node: TTreeNode): Boolean; override;
		function  CanEdit(Node: TTreeNode): Boolean; override;
		procedure Edit(const Item: TTVItem); override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		procedure Change(Node: TTreeNode); override;

		function 	LocalFindNode(Root:TTreeNode;LID:PItemIDList;ScanSubTree:Boolean):TTreeNode;
		function  FindNode(Item:PItemIDList):TTreeNode;overload;
		function	FindNode(const Folder:String):TTreeNode;overload;

		procedure	CreateNameSpace;virtual;
		function	MakeTreeNode(Node:TTreeNode;Item:PNameSpaceItem):TTreeNode;virtual;
		procedure	UpdateTreeNode(const Node:TTreeNode);virtual;
		procedure DeleteFromTree(TreeNode:TTreeNode);virtual;
		procedure	DeleteNode(TreeNode:TTreeNode); virtual;
		procedure FreeNode(Node:TTreeNode);virtual;
		procedure DeleteNodeChildren(Node:TTreeNode);virtual;
		function	AddIDToTree(TreeNode:TTreeNode;ID:PItemIDList;SelBool:Boolean):TTreeNode;virtual;
		function	AddFolderToTree(TreeNode:TTreeNode;const AFolder:String;SelBool:Boolean;AddIfExpand:Boolean):TTreeNode;virtual;
		function	MakeChildFolderFor(TreeNode:TTreeNode):Integer; virtual;
		procedure	PopulateSubTree(Node:TTreeNode;ADeletePrior:Boolean=False);
		procedure	CreateWnd;override;
		procedure DestroyWnd;override;
		procedure WndProc(var Message: TMessage); override;

		procedure	ValidateMyComputer;

		function	HandleChangeNotify(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;virtual;
		function	CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;virtual;
		procedure	CallChangeNotifyEvent(TypeChange:LongWord);virtual;
		procedure	CreateNotifyEvent;
		procedure DestroyNotifyEvent;

		procedure	RePopulateDisplayName;
		procedure	StartDragDrop;
		procedure	OnTimer(Sender:TObject);

		procedure	LockRefresh;
		procedure	UnLockRefresh;
		procedure	RefreshItems;

	public
		{ Public declarations }
		constructor Create(Owner:TComponent);override;
		destructor Destroy;override;

		function	LocatePIDL(PIDL:PItemIDList;Populate:Boolean):TTreeNode;virtual;
		function	ExecuteCommand(Node:TTreeNode;ACommand:String):Boolean;
		procedure	DeleteSelect;
		function  MakeChildForSelect:Integer;

		function	DisplayName(Node:TTreeNode;TypeName:TTypeParsingName	=	tpnInFolder):String;
		procedure	MakeEditSelected;

		property	DesktopNode:TTreeNode read FDesktopNode;
		property	ImageList	: TImageList read FImageList;
	published
		{ Published declarations }
		property ShowHiddenFolders:Boolean read FHiddenFolder write SetHiddenFolder default False;
		property DefaultNewFolderName:String read FDefaultNewFolderName write FDefaultNewFolderName;
		property RespondOnChangeNotify: Boolean read FRespondOnChangeNotify write SetRespondOnChangeNotify default True;
		property CurrentFolder:String read GetCurrentFolder write SetCurrentFolder;
		property FolderEdit: TEdit read FFolderEdit write SetFolderEdit;
		property FolderStaticText: TStaticText read FFolderStaticText write SetFolderStaticText;
		property TypeDisplayMode:TTypeDisplayMode read FTypeDisplayMode write SetTypeDisplayMode default tdmAsIs;
		property NameSpaceList:TNameSpaceList read FNameSpaceList write SetNameSpaceList;
		property TypePopUpMenu:TTypeContextMenu read FTypePopUpMenu write SetTypePopUpMenu default tcmOnlyContextMenu;		

		property Align;
		property Anchors;
		property BiDiMode;
		property BorderStyle;
		property BorderWidth;
		property ChangeDelay;
		property Color;
		property Ctl3D;
		property Constraints;
		property Enabled;
		property Font;
		property HideSelection;
		property HotTrack;
		property Indent;
		property ParentBiDiMode;
		property ParentColor default False;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property RightClickSelect;
		property RowSelect;
		property ShowButtons;
		property ShowHint;
		property ShowLines;
		property ShowRoot;
		property TabOrder;
		property TabStop default True;
		property ToolTips;
		property Visible;

		property OnChange;
		property OnChanging;
		property OnClick;
		property OnDblClick;
		property OnDeletion;
		property OnEdited;
		property OnEditing;
		property OnEndDock;
		property OnEnter;
		property OnExit;
		property OnExpanding;
		property OnExpanded;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;

		//add in 0.94
		property OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent read FOnBeforeNotifyEvent write FOnBeforeNotifyEvent;
		property OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent read FOnChangeNotifyEvent write FOnChangeNotifyEvent;
	end;

procedure Register;

implementation

uses
	NameSpaceWideChar;

procedure Register;
begin
	RegisterComponents('View Library', [TNameSpaceTree]);
end;

{ TNameSpaceTree }
type
	TNSL	=	class(TNameSpaceList);

function TNameSpaceTree.AddFolderToTree(TreeNode: TTreeNode;const AFolder: String; SelBool: Boolean;AddIfExpand:Boolean): TTreeNode;

var
	NSI:PNameSpaceItem;
	IDT,ID,LID:PItemIDList;
	AName:String;

begin
	Result:=nil;
	if TreeNode=nil then Exit;
	if not TreeNode.IsVisible then Exit;
	if AddIfExpand and (not TreeNode.Expanded) then begin
		TreeNode.HasChildren:=True;
		Exit;
	end;
	NSI:=PNameSpaceItem(TreeNode.Data);
//change in v0.91	
	IDT:=SHPIDLFromFileName(AFolder);
	try
		if SHIsParentPIDL(PIDL_NetHood,IDT) then
			LID:=SHCloneLastPIDL(IDT)
		else begin
			AName:=ExtractFileName(AFolder);
			if AName='' then AName:=AFolder;
			LID:=SHPIDLFromFileName(NSI.ShellFolder,AName);
		end;
	finally
		SHFree(IDT);
	end;

	ID:=SHCombinePIDL(NSI.ID,LID);
	Result:=nil;
	try
		if ID<>nil then
			if FindNode(ID)=nil then Result:=AddIDToTree(TreeNode,ID,SelBool);
		if Result<>nil then TreeNode.HasChildren:=True;
	finally
		SHFree(ID);
		SHFree(LID);
	end;
end;

function TNameSpaceTree.AddIDToTree(TreeNode: TTreeNode; ID: PItemIDList;SelBool: Boolean): TTreeNode;

var Item,PItem :	PNameSpaceItem;
		IsSelected : Boolean;
		scr:TCursor;

begin
	Result:=nil;
	if TreeNode=nil then Exit;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		IsSelected:=(TreeNode=Selected) and SelBool;
		PItem:=PNameSpaceItem(TreeNode.Data);
		Item:=MakeNameSpaceItem(ID);
		if Item=nil then Exit;		
		Item.Parent:=PItem;
		Item.ItemIndex:=PItem.SubNameSpaces.Add(Item);
		Items.BeginUpdate;
		try
			Result:=MakeTreeNode(TreeNode,Item);
			TreeNode.AlphaSort;
			if (not SelBool) and TreeNode.Expanded then TreeNode.Expand(False);
		finally
			Items.EndUpdate;
			if IsSelected then begin Selected:=Result;Selected.EditText; end;
		end;
	finally
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceTree.CanExpand(Node: TTreeNode): Boolean;

begin
	Result:=inherited CanExpand(Node);
	if Result and (not (csDesigning in ComponentState)) then PopulateSubTree(Node,FlagControl);
end;

procedure TNameSpaceTree.CompareData(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);

var sh1,sh2:PNameSpaceItem;

begin
	sh1:=PNameSpaceItem(Node1.Data);
	sh2:=PNameSpaceItem(Node2.Data);
	Compare:=SmallInt(sh1.Parent.ShellFolder.CompareIDs(0, SHLastPIDL(sh1.ID), SHLastPIDL(sh2.ID)));
end;

constructor TNameSpaceTree.Create(Owner: TComponent);

begin
	inherited Create(Owner);
	FImageList:=TImageList.Create(Self);
	Width:=225;
	Height:=240;
	FDefaultNewFolderName:='New Folder';
	FRespondOnChangeNotify:=True;
	inherited OnCompare:=CompareData;
	SortType:=stData;
	ContextPopUp:=TContextPopUpMenu.Create(Self);

	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot:=TNameSpaceTarget.Create(Self);
	if ActiveDragAndDrop then FDropSourceRoot:=TNameSpaceSource.Create(Self);

	Timer:=TTimer.Create(Self);
	Timer.Enabled:=False;
	Timer.Interval:=2000;
	Timer.OnTimer:=OnTimer;
end;

procedure TNameSpaceTree.CreateNameSpace;

var
	FileInfo: TSHFileInfo;
	scr:TCursor;

begin
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	try
		DisposeNameSpaceItem(NameSpace);
		NameSpace:=MakeDesktop;
		FImageList.ShareImages:=True;
		FImageList.Handle:=SHGetFileInfo(Pointer(NameSpace.ID), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
		Images:=FImageList;
		FDesktopNode:=MakeTreeNode(nil,NameSpace);
		if (csDesigning in ComponentState) then PopulateSubTree(DesktopNode);
		DesktopNode.Expand(False);

		//change in v 0.88
		if ActiveDragAndDrop then FDropTargetRoot.CreateDropTarget(NameSpace);

	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceTree.CreateWnd;

begin
	inherited;
	CreateNotifyEvent;	
	if NameSpace=nil then CreateNameSpace;
	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot.RegisterDragDrop;
end;

procedure TNameSpaceTree.DeleteFromTree(TreeNode: TTreeNode);

var
	Node,Parent:TTreeNode;
	IsSelected:Boolean;
	NSI:PNameSpaceItem;
	scr:TCursor;

begin
	if TreeNode=nil then Exit;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	Node:=nil;
	IsSelected:=TreeNode=Selected;
	try
		NSI:=PNameSpaceItem(TreeNode.Data);
		if NSI.Parent<>nil then NSI.Parent.SubNameSpaces[NSI.ItemIndex]:=nil;
		Node:=TreeNode.GetNextSibling;
		if Node=nil then begin
			Node:=TreeNode.GetPrevSibling;
			if Node=nil then Node:=TreeNode.Parent;
		end;
		Parent:=TreeNode.Parent;
		TreeNode.Delete;
		DisposeNameSpaceItem(NSI);
		if (Parent<>nil) and (Parent.Count=0) then begin
			Parent.Collapse(True);
			Parent.HasChildren:=False;
		end;
	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
		if IsSelected then
			if Node=nil then Selected:=DesktopNode else	Selected:=Node;
	end;
end;

procedure TNameSpaceTree.DeleteNode(TreeNode: TTreeNode);

begin
	ExecuteCommand(TreeNode,scDelete);
end;

procedure TNameSpaceTree.DeleteSelect;

begin
	DeleteNode(Selected);
end;

destructor TNameSpaceTree.Destroy;

begin
	//change in v0.95
	if ActiveDragAndDrop then begin
		FDropTargetRoot.UnLockLockObjectExternal;
		FDropTargetRoot:=nil;
		FDropSourceRoot:=nil;
	end;
	//end change in v0.95

	if NameSpaceList<>nil then begin
		with TNSL(NameSpaceList) do
			if ListNotifyTree<>nil then ListNotifyTree.Remove(Self);
	end;
	DestroyNotifyEvent;
	DisposeNameSpaceItem(NameSpace);
	ContextPopUp.Free;
	inherited;
end;

function TNameSpaceTree.FindNode(Item: PItemIDList): TTreeNode;

begin
	Result:=LocatePIDL(Item,False);
end;

procedure TNameSpaceTree.Edit(const Item: TTVItem);

var Node:TTreeNode;
		SaveText:String;
		ID:PItemIDList;

begin
	with Item do if (State and TVIF_PARAM)<>0 then
		Node:=Pointer(LParam)
	else
		Node:=Items.GetNode(HItem);
	if Node<>nil then SaveText:=Node.Text else SaveText:='®°';
	inherited Edit(Item);
	if (Node<>nil) and (SaveText<>Node.Text) then begin
		ID:=SHClonePIDL(PNameSpaceItem(Node.Data).ID);
		RenameFolderItem(PNameSpaceItem(Node.Data),Node.Text);
		try
			if not RespondOnChangeNotify then
				UpdateTreeNode(Node)
			else
				SHChangeNotify(SHCNE_RENAMEFOLDER,SHCNF_IDLIST or SHCNF_FLUSH,ID,PNameSpaceItem(Node.Data).ID)
		finally
			SHFree(ID);
		end;
	end;
end;

function TNameSpaceTree.FindNode(const Folder: String): TTreeNode;

var
	ID:PItemIDList;

begin
	ID:=SHPIDLFromFileName(Folder);
	try
		if ID<>nil then Result:=FindNode(ID) else Result:=nil;
	finally
		SHFree(ID);
	end;
end;

function TNameSpaceTree.LocalFindNode(Root: TTreeNode; LID: PItemIDList;ScanSubTree: Boolean): TTreeNode;

var
	i : Integer;

begin
	Result:=nil;
	if Root=nil then Exit;
	i:=0;
	with Root do while (Result=nil) and (i<Count) do begin
		if SHIsEqualPIDL(PNameSpaceItem(Item[i].Data).ID,LID) then Result:=Item[i];
		if ScanSubTree and (Result=nil) and (Item[i].Count>0) then Result:=LocalFindNode(Item[i],LID,ScanSubTree);
		Inc(i);
	end;
end;

function TNameSpaceTree.LocatePIDL(PIDL: PItemIDList;Populate: Boolean): TTreeNode;

var LP,CLP:PItemIDList;
		scr:TCursor;

begin
	CLP:=nil;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Result:=DesktopNode;
	try
		if PIDL=nil then Exit;
		if PIDL.mkid.abID[0]=0 then begin Result:=nil;Exit end;
		while PIDL.mkid.cb<>0 do begin
			LP:=SHCloneFirstPIDL(PIDL);
			CLP:=SHCombinePIDL(CLP,LP);
			SHFree(LP);
			Result:=LocalFindNode(Result,CLP,False);
			if Result=nil then Exit;
			if Populate then PopulateSubTree(Result);
			PIDL:=SHNextPIDL(PIDL);
		end;
	finally
		SHFree(CLP);
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceTree.MakeChildFolderFor(TreeNode: TTreeNode): Integer;

var DefFolder,NewFolder:String;
		NSI:PNameSpaceItem;
		LID,ID:PItemIDList;
		Buffer : Pointer;
		scr:TCursor;
		FlagC:Boolean;
		Node:TTreeNode;
		SizeBuffer:Integer;

begin
	if TreeNode=nil then begin Result:=-1;Exit end;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		NSI:=PNameSpaceItem(TreeNode.Data);
		if SHIsEqualPIDL(NSI.ID,PIDL_Desktop) then
			NewFolder:=SHFileNameFromPIDL(PIDL_DesktopFolder)
		else
			NewFolder:=SHFileNameFromPIDL(NSI.ID);
		if NewFolder='' then begin Result:=2;Exit end;

		Buffer:=nil;
		try
			if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then SizeBuffer:=Sizeof(WideChar)*MAX_PATH else SizeBuffer:=Sizeof(Char)*MAX_PATH;
			Buffer:=AllocMem(SizeBuffer);
			DefFolder:=DefaultNewFolderName;
			//change in v0.92
			if FileExists(MakeNameSpace(NewFolder,DefFolder)) then
				if PathMakeUniqueName(Buffer^,SizeBuffer,DefFolder,NewFolder) then
				//change in v0.91
					if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then begin
						DefFolder:=ExtractFileName(WideCharToString(Buffer));
						if DefFolder='' then DefFolder:=WideCharToString(Buffer);
					end else begin
						DefFolder:=ExtractFileName(StrPas(Buffer));
						if DefFolder='' then DefFolder:=StrPas(Buffer);
					end;
			NewFolder:=MakeNameSpace(NewFolder,DefFolder);
		finally
			FreeMem(Buffer);
		end;

		if (not TreeNode.Expanded) then TreeNode.Expand(False);
		if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then
			FlagC:=CreateDirectoryW(StringToOleStr(NewFolder),nil)
		else
			FlagC:=CreateDirectory(PChar(NewFolder),nil);

		if FlagC then begin
			Result:=0;
			if RespondOnChangeNotify then begin
				LID:=nil;ID:=nil;
				try
					if SHIsEqualPIDL(NSI.ID,PIDL_Desktop) or SHIsEqualPIDL(NSI.ID,PIDL_DesktopFolder) then
						ID:=SHPIDLFromFileName(NewFolder)
					else begin
						LID:=SHPIDLFromFileName(NSI.ShellFolder,ExtractFileName(NewFolder));
						ID:=SHCombinePIDL(NSI.ID,LID);
					end;
					SHChangeNotify(SHCNE_MKDIR,SHCNF_IDLIST	or SHCNF_FLUSH,ID,nil);
					Application.ProcessMessages;
					if (not TreeNode.Expanded) then TreeNode.Expand(False);
					Node:=FindNode(ID);
					if Node<>nil then begin	Selected:=Node;	Selected.EditText; end;
				finally
					SHFree(LID);
					SHFree(ID);
				end;
			end else
				AddFolderToTree(TreeNode,NewFolder,True,False)
		end else
			Result:=1;
	finally
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceTree.MakeChildForSelect: Integer;

begin
	Result:=MakeChildFolderFor(Selected);
end;

function TNameSpaceTree.MakeTreeNode(Node:TTreeNode;Item: PNameSpaceItem): TTreeNode;

var
	Attr:LongWord;

begin
	Result:=Items.AddChildObject(Node,' ',Item);
	Result.Text:=DisplayName(Result);
	Result.ImageIndex:=GetShellImage(Item, False , False);
	Result.SelectedIndex:=GetShellImage(Item, False , True);
	Attr:=GetAttributes(Item);
	if BOOL(Attr and SFGAO_SHARE) then
		Result.OverlayIndex:=0
	else
	if BOOL(Attr and SFGAO_LINK) then Result.OverlayIndex:=1 else Result.OverlayIndex:=-1;
	Result.Cut:=BOOL(Attr and SFGAO_GHOSTED);
	Result.HasChildren:=BOOL(Attr and SFGAO_HASSUBFOLDER);
end;

procedure TNameSpaceTree.PopulateSubTree(Node: TTreeNode;ADeletePrior:Boolean=False);

var
	i:Integer;
	Item:PNameSpaceItem;
	Help:TTreeNode;
	scr:TCursor;

begin
	if (Node.Count>0) then
		if ADeletePrior then DeleteNodeChildren(Node) else Exit;
	Item:=PNameSpaceItem(Node.Data);
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	try
		if ShowHiddenFolders then
			PopulateIDList(Item, EnumsFolder or EnumsHidden)
		else
			PopulateIDList(Item,EnumsFolder);
		with Item^ do for i:=0 to Pred(SubNameSpaces.Count) do	begin
			Help:=MakeTreeNode(Node,PNameSpaceItem(SubNameSpaces[i]));
			if SHIsEqualPIDL(PNameSpaceItem(SubNameSpaces[i]).ID,PIDL_MyComputer) then begin
				PopulateSubTree(Help);
				Help.AlphaSort;
				Help.Expand(False);
			end;
		end;
		Node.HasChildren:=Node.Count>0;
		Node.AlphaSort;
	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceTree.SetHiddenFolder(const Value: Boolean);

begin
	FHiddenFolder:=Value;
end;

procedure TNameSpaceTree.UpdateTreeNode(const Node: TTreeNode);

var
	Item:PNameSpaceItem;
	scr:TCursor;
	Attr:LongWord;

begin
	if Node=nil then Exit;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	try
		Item:=PNameSpaceItem(Node.Data);
		Node.ImageIndex:=GetShellImage(Item, False , False);
		Node.SelectedIndex:=GetShellImage(Item, False , True);
		Attr:=GetAttributes(Item);
		if BOOL(Attr and SFGAO_SHARE) then
			Node.OverlayIndex:=0
		else
		if BOOL(Attr and SFGAO_LINK) then Node.OverlayIndex:=1 else Node.OverlayIndex:=-1;
		Node.HasChildren:=BOOL(Attr and SFGAO_HASSUBFOLDER);
		Node.Text:=DisplayName(Node);
		if Node.Parent<>nil then Node.Parent.AlphaSort;
		Change(Node);
	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceTree.CanEdit(Node: TTreeNode): Boolean;

begin
	Result:=inherited CanEdit(Node);
	Result:=Result and (Node<>nil) and (Node<>DesktopNode)
					and (BOOL(GetAttributes(Node.Data) and SFGAO_CANRENAME))
					and (not SHIsEqualPIDL(PNameSpaceItem(Node.Data).ID,PIDL_DesktopFolder));
end;

procedure TNameSpaceTree.CreateNotifyEvent;

var
	Flags:Integer;

begin
	if csDesigning in ComponentState then Exit;
	if RespondOnChangeNotify and (FNotifyHandle=0) then begin
		FNotifyRegister.PIDL:=nil;
		FNotifyRegister.WatchSubTree:=False;
		if IsExistChangeNotificationLock then
			Flags:=SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS or SHCNF_NO_PROXY
		else
			Flags:=SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS;
		FNotifyHandle:=SHChangeNotifyRegister(Self.Handle,
									 Flags,
									 SHCNE_NETSHARE or SHCNE_NETUNSHARE or SHCNE_RENAMEFOLDER or SHCNE_UPDATEDIR or
									 SHCNE_RMDIR or SHCNE_DRIVEREMOVED or SHCNE_MKDIR or SHCNE_DRIVEADD or
									 SHCNE_MEDIAREMOVED or SHCNE_MEDIAINSERTED,
									 WM_CHANGENOTIFY,
									 1,
									 PNotifyRegister(@FNotifyRegister));
		if FNotifyHandle=0 then begin
			FRespondOnChangeNotify:=False;
			DestroyNotifyEvent;
			raise Exception.Create(ecNotRegisterNotify);
		end;
	end;
end;

procedure TNameSpaceTree.DestroyNotifyEvent;

begin
	if csDesigning in ComponentState then Exit;
	try
		if FNotifyHandle<>0 then SHChangeNotifyDeregister(FNotifyHandle);
	finally
		FNotifyHandle:=0;
	end;
end;

procedure TNameSpaceTree.SetRespondOnChangeNotify(const Value: Boolean);

begin
	if FRespondOnChangeNotify<>Value then begin
		FRespondOnChangeNotify:=Value;
		if Value then CreateNotifyEvent else DestroyNotifyEvent;
	end;
end;

function	TNameSpaceTree.CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;

begin
	Result:=True;
	if Assigned(OnBeforeNotifyEvent) then Result:=OnBeforeNotifyEvent(Self,TypeChange,Item1,Item2);
end;

procedure	TNameSpaceTree.CallChangeNotifyEvent(TypeChange:LongWord);

begin
	if Assigned(OnChangeNotifyEvent) then OnChangeNotifyEvent(Self,TypeChange);
end;

function TNameSpaceTree.HandleChangeNotify(TypeChange: LongWord; var Item1, Item2: PItemIDList):Boolean;

var
	Node:TTreeNode;
	IT1,IT2,P1,P2,ParentPIDL:PItemIDList;
	SaveNSL:TNameSpaceList;
	SaveTC 	: LongWord;

begin
	SaveNSL:=NameSpaceList;
	IT1:=SHClonePIDL(Item1);
	IT2:=SHClonePIDL(Item2);
	SaveTC:=TypeChange;
	try
		Result:=False;
		NameSpaceList:=nil;
		if not CallBeforeNotifyEvent(TypeChange,IT1,IT2) then Exit;
		TypeChange:=TypeChange and (not SHCNE_INTERRUPT);
		case TypeChange of
			SHCNE_RENAMEFOLDER : begin
				P1:=SHParentPIDL(IT1);
				P2:=SHParentPIDL(IT2);
				try
					if SHIsEqualPIDL(P1,P2) then begin
						Node:=FindNode(IT1);
						if Node<>nil then begin
							if IT2<>nil then
								ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT2)
							else
								ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT1);
							UpdateTreeNode(Node);
							Result:=True;
						end;
					end else begin
						DeleteFromTree(FindNode(IT1));
						AddFolderToTree(FindNode(P2),SHFileNameFromPIDL(IT2),False,True);
						Result:=True;
					end;
				finally
					SHFree(P1);
					SHFree(P2);
				end;
			end;
			SHCNE_NETSHARE,SHCNE_NETUNSHARE,SHCNE_UPDATEDIR : begin
				Node:=FindNode(IT1);
				if Node<>nil then begin
					if IT2<>nil then
						ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT2)
					else
						ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT1);
					UpdateTreeNode(Node);
					Result:=True;
				end;
			end;
			SHCNE_RMDIR,SHCNE_DRIVEREMOVED : begin DeleteFromTree(FindNode(IT1));Result:=True end;
			SHCNE_MKDIR,SHCNE_DRIVEADD : begin
				ParentPIDL:=SHParentPIDL(IT1);
				try
					AddFolderToTree(FindNode(ParentPIDL),SHFileNameFromPIDL(IT1),False,True);
				finally
					SHFree(ParentPIDL);
					Result:=True;
				end;
			end;
			SHCNE_MEDIAREMOVED : begin
				ValidateMyComputer;
				Node:=FindNode(IT1);
				if Node<>nil then begin
					DeleteNodeChildren(Node);
					ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT1);
					UpdateTreeNode(Node);
					Result:=True;
				end;
			end;
			SHCNE_MEDIAINSERTED : begin
				ValidateMyComputer;
				Node:=FindNode(IT1);
				if Node<>nil then begin
					ChangeNameSpaceItem(PNameSpaceItem(Node.Data),IT1);
					UpdateTreeNode(Node);
					PopulateSubTree(Node);
					Result:=True;
				end;
			end;
		end;
		CallChangeNotifyEvent(SaveTC);
	finally
		SHFree(IT1);
		SHFree(IT2);
		NameSpaceList:=SaveNSL;
	end;
end;

procedure TNameSpaceTree.DeleteNodeChildren(Node: TTreeNode);

begin
	Items.BeginUpdate;
	try
		while Node.Count>0 do FreeNode(Node.Item[0]);
	finally
		Items.EndUpdate;
	end;
end;

procedure TNameSpaceTree.FreeNode(Node: TTreeNode);

var Item:PNameSpaceItem;
		scr:TCursor;

		procedure LocalFreeNode(Node:TTreeNode);

		begin
			if Node=nil then Exit;
			while Node.Count>0 do	LocalFreeNode(Node.Item[0]);
			Node.Delete;
		end;

begin
	if Node=nil then Exit;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	try
		Item:=PNameSpaceItem(Node.Data);
		LocalFreeNode(Node);
		if Item.Parent<>nil then Item.Parent.SubNameSpaces[Item.ItemIndex]:=nil;
		DisposeNameSpaceItem(Item);
	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceTree.WMLButtonDown(var Message: TWMLButtonDown);

begin
	with Message do FlagControl:=(Keys and MK_CONTROL)=MK_CONTROL;
	inherited;
end;

procedure TNameSpaceTree.WMLButtonUp(var Message: TWMLButtonUp);

begin
	inherited;
	FlagControl:=False;
end;

procedure TNameSpaceTree.WMLButtonDblClk(var Message: TWMLButtonDblClk);

begin
	with Message do FlagControl:=(Keys and MK_CONTROL)=MK_CONTROL;
	if (Selected<>nil) and (Selected<>DesktopNode) then
		if BOOL(GetAttributes(Selected.Data) and SFGAO_REMOVABLE) then PopulateSubTree(Selected,True);
	inherited;
end;

function TNameSpaceTree.GetCurrentFolder: String;

begin
	Result:=DisplayName(Selected,tpnForParsing);
end;

procedure TNameSpaceTree.SetCurrentFolder(Value: String);

var PIDL:PItemIDList;
		scr:TCursor;

		function RemoveBackSlesh(const Value:String):String;

		begin
			Result:=Value;
			if (Result<>'') and (Result[Length(Result)]='\') then System.Delete(Result,Length(Result),1);
		end;

begin
	PIDL:=nil;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		if DesktopNode=nil then Exit;
		while (PIDL=nil) and (Length(Value)>2) do begin
			PIDL:=SHPIDLFromFileName(Value);
			if PIDL=nil then Value:=ExtractFilePath(RemoveBackSlesh(Value));
		end;
		if PIDL=nil then PIDL:=SHClonePIDL(PIDL_Desktop);
		if PIDL=nil then Selected:=DesktopNode else	Selected:=LocatePIDL(PIDL,True);
		Selected.Expand(False);
	finally
		SHFree(PIDL);
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceTree.SetFolderEdit(const Value: TEdit);

begin
	FFolderEdit:=Value;
	if Value<>nil then begin
		FFolderEdit.FreeNotification(Self);
		FFolderEdit.Text:=CurrentFolder;
	end;
end;

procedure TNameSpaceTree.SetFolderStaticText(const Value: TStaticText);

begin
	FFolderStaticText:=Value;
	if Value<>nil then	begin
		FFolderStaticText.FreeNotification(Self);
		FFolderStaticText.Caption:=CurrentFolder;
	end;
end;

procedure TNameSpaceTree.Change(Node: TTreeNode);

begin
	inherited Change(Node);
	if FolderStaticText<>nil then FolderStaticText.Caption:=CurrentFolder;
	if FolderEdit<>nil then FolderEdit.Text:=CurrentFolder;
	//change in v0.91
	if (NameSpaceList<>nil) and (Node<>nil) and (Node.Data<>nil) then NameSpaceList.CurrentFolderFromID(PNameSpaceItem(Node.Data).ID,Self);
end;

procedure TNameSpaceTree.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
	if (Operation = opRemove) then begin
		if (AComponent = FolderStaticText) then FFolderStaticText:=nil;
		if (AComponent = FolderEdit) then FFolderEdit:=nil;
		if (AComponent = NameSpaceList) then FNameSpaceList:=nil;
	end;
end;

function TNameSpaceTree.DisplayName(Node:TTreeNode;TypeName: TTypeParsingName	=	tpnInFolder): String;

begin
	if Node=nil then begin Result:='';Exit end;
	if Node.Data=nil then begin Result:='';Exit end;
	with PNameSpaceItem(Node.Data)^ do
		if SHIsEqualPIDL(ID,PIDL_Desktop) then
			Result:=rsDesktop
		else
			if Parent<>nil then
				Result:=GetDisplayName(Parent.ShellFolder,SHLastPIDL(ID),TypeName)
			else
				Result:=GetDisplayName(SF_Desktop,SHLastPIDL(ID),TypeName);
	case TypeDisplayMode of
		tdmLowerCase	:	Result:=NSWideLowerCaseA(Result);
		tdmUpperCase	:	Result:=NSWideUpperCaseA(Result);
		tdmFirstUpper	:	Result:=NSWideFirstUpperA(Result);
	end;
end;

procedure TNameSpaceTree.WMKeyDown(var Message: TWMKeyDown);

	function IsControl(AControl:Boolean):Boolean;

  begin
  	Result:=AControl;
    if Result then case Message.CharCode of
      Ord('X'),Ord('x')	:	ExecuteCommand(Selected,scCut);
      Ord('C'),Ord('c')	:	ExecuteCommand(Selected,scCopy);
      Ord('V'),Ord('v')	:	ExecuteCommand(Selected,scPaste);
    else
    	Result:=False;
    end;
  end;

	function IsShift(AShift:Boolean):Boolean;

  begin
  	Result:=AShift;
    if Result then case Message.CharCode of
      VK_DELETE : DeleteSelect;
    else
    	Result:=False;
    end;
  end;

  function IsNoShiftState(AShiftState:Boolean):Boolean;

  begin
  	Result:=AShiftState;
  	if Result then case Message.CharCode of
			VK_F2			:	if Selected<>nil then Selected.EditText;
      VK_DELETE : DeleteSelect;
    else
    	Result:=False;
    end;
  end;

begin
	if not (IsNoShiftState(KeyDataToShiftState(Message.KeyData)=[]) or
  	IsControl(KeyDataToShiftState(Message.KeyData)=[ssCtrl]) or
    IsShift(KeyDataToShiftState(Message.KeyData)=[ssShift])) then	inherited;
end;

procedure TNameSpaceTree.SetTypeDisplayMode(const Value: TTypeDisplayMode);

begin
	if TypeDisplayMode<>Value then begin
		FTypeDisplayMode:=Value;
		RePopulateDisplayName;
	end;
end;

procedure TNameSpaceTree.RePopulateDisplayName;

var
	scr:TCursor;

	procedure	RepopulateChild(Node:TTreeNode);

	var
		i:Integer;

	begin
		i:=0;
		with Node do while (i<Count) do begin
			Item[i].Text:=DisplayName(Item[i]);
			if Item[i].Count>0 then RepopulateChild(Item[i]);
			Inc(i);
		end;
	end;

begin
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	Items.BeginUpdate;
	try
		DesktopNode.Text:=DisplayName(DesktopNode);
		RepopulateChild(DesktopNode);
	finally
		Items.EndUpdate;
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceTree.SetNameSpaceList(const Value: TNameSpaceList);

begin
	if NameSpaceList<>nil then begin
		with TNSL(NameSpaceList) do
			if ListNotifyTree<>nil then ListNotifyTree.Remove(Self);
	end;
	FNameSpaceList:=Value;
	if Value<>nil then begin
		FNameSpaceList.FreeNotification(Self);
		if (Selected<>nil) and (Selected.Data<>nil) then
			//change in v0.91
			FNameSpaceList.CurrentFolderFromID(PNameSpaceItem(Selected.Data).ID,Self);
		with TNSL(NameSpaceList) do
			if ListNotifyTree<>nil then ListNotifyTree.Add(Self);
	end;
end;

procedure TNameSpaceTree.MakeEditSelected;

begin
	if Selected<>nil then Selected.EditText; 
end;

procedure TNameSpaceTree.SetTypePopUpMenu(const Value: TTypeContextMenu);

begin
	FTypePopUpMenu := Value;
end;

procedure TNameSpaceTree.WMDRAWITEM(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceTree.WMINITMENUPOPUP(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceTree.WMMEASUREITEM(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceTree.ValidateMyComputer;

var
	dwAttr:LongWord;
	SF:IShellFolder;
	PNil:PItemIDList;

begin
	dwAttr:=SFGAO_VALIDATE;PNil:=nil;
	SF_Desktop.BindToObject(PIDL_MyComputer,nil,IID_IShellFolder,Pointer(SF));
	if SF<>nil then SF.GetAttributesOf(0,PNil,dwAttr);
	SF:=nil;
end;

procedure TNameSpaceTree.DestroyWnd;

begin
	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot.RevokeDragDrop;
	inherited;
end;

procedure TNameSpaceTree.WMSETDROPTARGET(var Msg: TMessage);

var
	Pos:TPoint;
	Item:TTreeNode;

begin
	Pos:=ScreenToClient(PPoint(Msg.LParam)^);
	Item:=GetNodeAt(Pos.X,Pos.Y);
	if Item<>nil then begin
		if DropTarget<>Item then begin
			DropTarget:=Item;
			Timer.Enabled:=False;
			SaveTimerNode:=Item;
			if SaveTimerNode=nil then Exit;
			Timer.Enabled:=not SaveTimerNode.Expanded;
		end;
	end else
		DropTarget:=nil;
end;

procedure TNameSpaceTree.WMGETITEMDATA(var Msg: TMessage);

begin
	if (DropTarget<>nil) then PSendMsgRecord(MSG.LParam).NSItem:=DropTarget.Data;
end;

procedure TNameSpaceTree.WMCLEARDROPTARGET(var Msg: TMessage);

begin
	if (not RespondOnChangeNotify) and (DropTarget<>nil) then begin
		Selected:=DropTarget;
		SaveTimerNode:=nil;
		Timer.Enabled:=True;
	end;
	DropTarget:=nil;	
end;

procedure TNameSpaceTree.WMRENAMEITEM(var Msg: TMessage);

begin
	MakeEditSelected;
end;

procedure TNameSpaceTree.WMSELECTCUTITEM(var Msg: TMessage);

begin
	if LastCut<>nil then LastCut.Cut:=False;
	Selected.Cut:=True;
	LastCut:=Selected;
end;

procedure TNameSpaceTree.CNNotify(var Message: TWMNotify);

begin
	case Message.NMHdr.Code of
		TVN_BEGINDRAG,
		TVN_BEGINRDRAG	:	StartDragDrop;
	else
		inherited;
	end;
end;

procedure TNameSpaceTree.StartDragDrop;

var
	PIDL:PItemIDList;

begin
	if (Selected<>nil) then with PNameSpaceItem(Selected.Data)^ do if Parent<>nil then begin
		PIDL:=SHCloneLastPIDL(ID);
		try
			//change in v 0.88
			if ActiveDragAndDrop then FDropSourceRoot.DoDragDrop(Parent.ShellFolder,PIDL);
		finally
			SHFree(PIDL);
		end;
	end;
end;

procedure TNameSpaceTree.OnTimer(Sender: TObject);

begin
	Timer.Enabled:=False;
	if SaveTimerNode=nil then begin
		if RespondOnChangeNotify then Exit;
		RefreshItems; 
	end else if (SaveTimerNode=DropTarget) then begin
		PopulateSubTree(SaveTimerNode);
		SaveTimerNode.Expand(False);
	end;
end;

function TNameSpaceTree.ExecuteCommand(Node: TTreeNode; ACommand: String):Boolean;

var
	PIDL:PItemIDList;
	Item:PNameSPaceItem;

begin
	Result:=False;
	if Node=nil then Node:=Selected;
	if Node<>nil then Item:=Node.Data else Exit;
	if Item=nil then Exit;
	with Item^ do if Parent<>nil then begin
		if not RespondOnChangeNotify then LockRefresh;
		PIDL:=SHCloneLastPIDL(ID);
		try
			Result:=ContextPopUp.ShellExecuteCommand(Parent.ShellFolder,PIDL,ACommand);
		finally
			SHFree(PIDL);
			if not RespondOnChangeNotify then UnLockRefresh;
		end;
	end;
end;

procedure TNameSpaceTree.LockRefresh;

begin
	Inc(FLockRefresh);
end;

procedure TNameSpaceTree.UnLockRefresh;

begin
	Dec(FLockRefresh);
	if FLockRefresh=0 then RefreshItems;
end;

procedure TNameSpaceTree.RefreshItems;

var
	PIDL:PItemIDList;
	DParent,Data:TTreeNode;

begin
	if Selected=DesktopNode then
		UpdateTreeNode(Selected)
	else begin
		if Selected=nil then Exit;
		LockWindowUpdate(Handle);
		Data:=Selected;
		DParent:=Data.Parent;
		PIDL:=SHClonePIDL(PNameSpaceItem(Data.Data).ID);
		PopulateSubTree(DParent,True);
		try
			Data:=LocalFindNode(DParent,PIDL,False);
			if Data<>nil then Selected:=Data;
		finally
			SHFree(PIDL);
			LockWindowUpdate(0);
		end;
	end;
end;

{$IFDEF DELPHI5_UP}
procedure TNameSpaceTree.WMContextMenu(var Message: TWMContextMenu);

var
	PIDL:PItemIDList;

begin
	if not RespondOnChangeNotify then LockRefresh;
	case TypePopUpMenu of
		tcmOnlyUserMenu : inherited;
		tcmOnlyContextMenu, tcmConcatMenu 	:	if (Selected<>nil) then with PNameSpaceItem(Selected.Data)^ do if Parent<>nil then begin
			PIDL:=SHCloneLastPIDL(ID);
			try
				ContextPopUp.CreatePopUpMenu(Parent.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (PopUpMenu<>nil));
			finally
				SHFree(PIDL);
			end;
		end;
	end;
	if not RespondOnChangeNotify then UnLockRefresh;
end;

{$ELSE}

procedure TNameSpaceTree.WMRButtonUp(var Message: TWMRButtonUp);

var
	PIDL:PItemIDList;

begin
	if not RespondOnChangeNotify then LockRefresh;
	case TypePopUpMenu of
		tcmOnlyUserMenu : inherited;
		tcmOnlyContextMenu, tcmConcatMenu 	:	if (Selected<>nil) then with PNameSpaceItem(Selected.Data)^ do if Parent<>nil then begin
			PIDL:=SHCloneLastPIDL(ID);
			try
				ContextPopUp.CreatePopUpMenu(Parent.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (PopUpMenu<>nil));
			finally
				SHFree(PIDL);
			end;
		end;
	end;
	if not RespondOnChangeNotify then UnLockRefresh;
end;
{$ENDIF}

procedure TNameSpaceTree.WndProc(var Message: TMessage);

type
	TIDArray	=	record
		Item1,Item2 : PItemIDList;
	end;
	PIDArray	=	^TIDArray;

var IDArray		:	PIDArray;
		wEventID  :	LongWord;
		hLock			:	HWND;
		Handled	:	Boolean;

begin
	Handled:=False;
	with Message do case Msg of
		WM_CHANGENOTIFY : begin
			if IsExistChangeNotificationLock then begin
				hLock:=SHChangeNotification_Lock(HWND(WParam),LParam,@IDArray,@wEventID);
				if hLock<>0 then with IDArray^ do begin
					Handled:=HandleChangeNotify(wEventID,Item1,Item2);
					SHChangeNotification_Unlock(hLock);
				end;
			end else
				with PIDArray(Message.WParam)^ do
					Handled:=HandleChangeNotify(Message.LParam,Item1,Item2);
			Result:=1;
		end;
	end;
	if not Handled then inherited;
end;

//add in v0.95
procedure TNameSpaceTree.WMDestroy(var Message: TWMDestroy);

begin
	if (ActiveDragAndDrop) and (FDropTargetRoot<>nil) then FDropTargetRoot.RevokeDragDrop;
	inherited;
end;

end.
