//
// The original Delphi code is : NameSpaceList.pas released 12.12.2002
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
// This unit contains class TNameSpaceList (List View of Folders and Files)
//
// version: 0.98 realeased 23.05.2004
// add property NumberOfDecimalInSize (propose by Dave Crutchley)
//
// version: 0.97 realeased 26.04.2004
// add property StandardDblClick (propose by Philip Hadar)
//	change DblClick event
// fix bug in UpdateCurrentListOnNotify
// 	if ID is same naw free old and add new
// fix bug in CNNOTIFY (for all file) and UpdateData (for zip file)
// 	now correct ghosted icon on change Hidden attribute
//
// version: 0.96 realeased 15.04.2004
// change in fix bug for winzip - donn't show size of winzip file (from v0.94)
// 		no longer test Type of File
//		now PNameSpaceItem contains field IsZipFile
// for Shortcut folder on network no longer call Details folder
//
// version: 0.95 realeased 29.02.2004
// fix bug - not free FDropTargetRoot on destroy TNameSpaceList
//
// version: 0.94 realeased 15.02.2004
// add two Event
//		OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent
//		OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent (propose joel joly)
// add proporty
//		Items
// change HandleChangeNotify
// fix bug - donn't show size of winzip file
//     winzip file sometimes has attribute SFGAO_FOLDER ?!?!
//     sometimes occurr on XP but not on win98
//
// version: 0.92 realeased 02.02.2004
// change in MakeFolder (replace folderExists with fileExists)
//
// version: 0.91 realeased 04.01.2004
// fix bug in methods MakeFolder
//   - windows fuction PathMakeUniqueName return in Buffer all path not only name folder
// change method CurrentFolderFromID(ID:PItemIDList;SenderTreeView	:	TCustomTreeView	=	nil);
//	 - second parameter is no longer Boolean type, it is now TCustomTreeView type
//
// version: 0.90 released  07.12.2003
// add procedure SetCurrentToParent (OneLevelUp) (propose Michal Glebowski)
//
// version: 0.89 released  02.11.2003
// add popup menu for background of TNameSpaceList (like Explorer - add new menu item)
// add function	CurrentName(AType:TTypeParsingName	=	tpnInFolder):String;
//
// version: 0.88 released 28.09.2003
// - fix bug in MediaInsertInList (now refresh list item if change change disk)
// - add flag ActiveDragAndDrop
//   some old programs (like Paradox 7.0)
//   donn't work properly with COM Drag and Drop and OLE
//
// version: 0.87 released 26.07.2003
// - add save haeder column data for each Folder
// - add header column data save to TStream and load from TStream
//
// version: 0.85 released 23.06.2003
// - listsortproc now is thread safe
// global const IS_PIDL_DialUpNetworking is now field in TNameSpaceList
// global const CompareSpaceList is now field in TNameSpaceItem
// (only for transfer to ListSortProc (is not a member of TNameSpaceItem))
// - change in UpdateItemInList	(propose and make DAVE CRUTCHLEY)
// - change in UpdateData				(propose and make DAVE CRUTCHLEY)
//	 add date and time test
//	 better conversion to SysTime
// - add new property DateTimeFormat
//			- SHORT WINDOWS and empty string -> Date and Time Format is Windows Short
//			- LONG WINDOWS -> Date and Time Format is Windows Long
//			- for other formats see FormatDateTime function
//
// version: 0.84 released 20.04.2003
// add support for Shortcut to Folder
// correct bug with open folder on Desktop  (bug find Roland Ruder)
//
// version: 0.83 released 19.03.2003
// change AnsiUpperCase with NSWideUpperCaseA
// change AnsiLowerCase with NSWideLowerCaseA
// for TypeDisplayMode=tdmFirstUpper call now Result:=NSWideFirstUpperA(Result);
// this is for better support DBCS
// fix bug in ListSortProc (List index out of bounds)
//
// version: 0.82	released 02.02.2003
// fix bug in Drag & Drop (item stays highlighted if press Escape)
// (detected by Anders Lee) - change in WMCLEARDROPTARGET message
// fix bug on select item (icon is not selected) (detected by Anders Lee)
// change in CNNOTIFY message
// this bugs produce TCustomListView in OwnerDraw mode
// I don't know who make this bugs
// I avoid this problems
//
// detected another bug in TCustomListView in message CNNotify
// when call OwnerDataStateChange - call with wrong order State
// (swap OldState and NewState)
// I don't call this event
//
// version: 0.81  released 27.12.2002
// avoid problem with winzip and respond on notify event (detected by Philip Hadar)
// added field FCountUpdate
// added respond on message WM_UPDATECURRENT
// added method UpdateCurrentListOnNotify
// fix bug (delete only from list) in DeleteItemFromList (now delete from memory)
// fix bug (delete only from list) in RepopulateItemID if not in MetchSpecification (now delete from memory)
// move error text in CreateNotifyEvent in resourcestring
// change	in DeleteItemFromList for support UpdateCurrentListOnNotify
// change	in AddItemIDToList for support UpdateCurrentListOnNotify
// change	in RenameItemInList for support UpdateCurrentListOnNotify
// change	in UpdateItemInList for support UpdateCurrentListOnNotify
// change	in CheckFreeSpace for support UpdateCurrentListOnNotify
//
// version: 0.8
//

unit NameSpaceList;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls,
	ActiveX, Forms, Dialogs, ComCtrls, CommCtrl, ShellAPI, ShlObj, ComObj,
	Menus, NameSpaceLibrary, NameSpaceUnit, NameSpaceDetails,
	NameSpaceSystemLists ;

{$I SetTypeCompiler.inc}

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

type
	TSortOrder  					=	(soAscending,soDescending);
	TTypeDisplay					=	(tdNothing,tdFile,tdMyComputer,tdSpecialFolder);
	TTypeEnumNames				=	(teAll,teFolder,teNonFolder);
	TTypeFormatSize 			=	(tfsAutomatic,tfsByte,tfsKiloByte,tfsMegaByte,tfsGigaByte,tfsTeraByte);
	TTypeThousand					=	(tt1024,tt1000);
	//add in 0.98
	TTypeNumberOfDecimal	=	(tndZero, tndOne, tndTwo, tndThree);

	PNameSpaceListHeader	=	^TNameSpaceListHeader;
	TNameSpaceListHeader	=	record
		Size 			:	LongWord;
		Width			:	LongWord;
		Alignment	:	TAlignment;
		Caption 	:	String[255];
	end;

	//add in v 0.89
	TUpdateBackGroundMenu	=	procedure(Sender:TObject;var BackGroundMenu:TPopUpMenu) of object;

	TNameSpaceList = class(TCustomListView)
	private
		{ Private declarations }
		FTypeFormatSize: TTypeFormatSize;
		FTypeEnumNames: TTypeEnumNames;
		FHiddenName: Boolean;
		FTypeDisplay: TTypeDisplay;
		FRespondOnChangeNotify: Boolean;
		FDefaultNewFolderName: String;
		FSortByColumn: Integer;
		FTypeThousand: TTypeThousand;
		FSortOrder: TSortOrder;
		FTypeDisplayMode: TTypeDisplayMode;
		FMatchSpec: String;
		FTypePopUpMenu: TTypeContextMenu;
		FLockRefresh	:	Integer;

		//add in v 0.85
		FDateTimeFormat: String;

		//add in v 0.87
		ListFoldersData 	: TListClass;
		CurrentFolderData	:	TListFoldersData;
		//add in v 0.89
		FBackGroundPopupMenu: TPopupMenu;
		FOnUpdateBackGroundMenu: TUpdateBackGroundMenu;
		FOnBeforeNotifyEvent: TNameSpaceBeforeNotifyEvent;
    FOnChangeNotifyEvent: TNameSpaceChangeNotifyEvent;
    FStandardDblClick: Boolean;
    FNumberOfDecimalInSize: TTypeNumberOfDecimal;

		procedure SetTypeFormatSize(const Value: TTypeFormatSize);
		procedure SetTypeEnumNames(const Value: TTypeEnumNames);
		procedure SetHiddenName(const Value: Boolean);
		procedure SetRespondOnChangeNotify(const Value: Boolean);
		function 	GetCurrentFolder: String;
		procedure SetCurrentFolder(Value: String);
		procedure SetSortByColumn(const Value: Integer);
		procedure SetTypeThousand(const Value: TTypeThousand);
		procedure SetSortOrder(const Value: TSortOrder);
		procedure SetTypeDisplayMode(const Value: TTypeDisplayMode);
		procedure SetMatchSpec(const Value: String);
		procedure SetTypePopUpMenu(const Value: TTypeContextMenu);
		function	GetViewStyle: TViewStyle;
		procedure NewSetViewStyle(const Value: TViewStyle);

		//add in v 0.85
		procedure SetDateTimeFormat(const Value: String);
		//add in v 0.89
		procedure SetBackGroundPopupMenu(const Value: TPopupMenu);
		procedure SetNumberOfDecimalInSize(const Value: TTypeNumberOfDecimal);

	protected
		{ Protected declarations }
		NameSpace	:	PNameSpaceItem;
		LargeList,
		SmallList	:	TImageList;

		FNotifyHandle 	: HWND;
		FNotifyRegister :	TNotifyRegister;
		ContextPopUp		:	TContextPopupMenu;
		FDropTargetRoot	:	INameSpaceTarget;
		FDropSourceRoot	:	INameSpaceSource;

		SaveDropTargetIndex	:	Integer;

		ListNotifyTree	:	TList;
		ListMatchSpec		:	TStringList;
		Animate					:	TAnimate;
		AscendingBitmap,
		DescendingBitmap:	TBitmap;

		ShellBrowser	:	IShellBrowserInfo;

		//add in v 0.81
		FCountUpdate	:	Integer;

		//add in v 0.85
		IS_PIDL_DialUpNetworking	:	Boolean;

		FAddNewSaveItem	:	PNameSpaceItem;

		function	FormatSize(Value:Int64):String;virtual;
		procedure	MakeFileListColumns;
		procedure	MakeDiskListColumns;
		procedure	MakeSpecialColumns;
		procedure	MakeSpecialColumnsWithTest;
		procedure	ClearListColumns;
		function	LocalFindListItem(Item:PItemIDList):TListItem;

		procedure	CreateNameSpace;
		procedure	AddListToItems;
		procedure	RepopulateItemID;
		procedure	RepopulateDisplayName;
		procedure	RefreshItems;

		procedure	UpdateData(SI:PNameSpaceItem);
		procedure	CreateTypeDisplay;
		function	CreatePIDLList(Item:PNameSpaceItem):TArrayPIDL;

		procedure	LockRefresh;
		procedure	UnLockRefresh;

		//add in v0.95
		procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

		procedure	WMINITMENUPOPUP(var Msg:TMessage); message WM_INITMENUPOPUP;
		procedure	WMDRAWITEM(var Msg: TMessage); message WM_DRAWITEM;
		procedure	WMMEASUREITEM(var Msg:TMessage); message WM_MEASUREITEM;

		procedure	WMSETDROPTARGET(var Msg:TMessage); message WM_SETDROPTARGET;
		procedure	WMGETITEMDATA(var Msg:TMessage); message WM_GETITEMDATA;
		procedure	WMCLEARDROPTARGET(var Msg:TMessage); message WM_CLEARDROPTARGET;
		procedure	WMRENAMEITEM(var Msg:TMessage); message WM_RENAMEITEM;
		procedure	WMSELECTCUTITEM(var Msg:TMessage); message WM_SELECTCUTITEM;
		procedure	WMADDNEWITEM(var Msg:TMessage); message WM_ADDNEWITEM;

		//ver 0.81
		procedure	WMUPDATECURRENT(var Msg:TMessage); message WM_UPDATECURRENT;

		procedure CNNOTIFY(var Msg:TWMNotify); message CN_NOTIFY;

		//add in v 0.87
		procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;

		//add in v 0.89
		procedure	UpdateBackgroundMenu;

{$IFDEF DELPHI5_UP}
		procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
{$ELSE}
		procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
{$ENDIF}
		procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;

		function  CanEdit(Item:TListItem): Boolean; override;
		procedure Edit(const Item: TLVItem); override;
		procedure ColClick(Column: TListColumn); override;
		procedure CreateWnd; override;
		procedure	DestroyWnd;override;
		procedure	Loaded; override;
		procedure	DblClick; override;
		procedure	ExecuteDefaultCommand;
		procedure	TestAndExecuteDefault;
		procedure	NewSelectAll;
		procedure WndProc(var Message: TMessage); override;

		function	HandleChangeNotify(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;virtual;
		function	CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;virtual;
		procedure	CallChangeNotifyEvent(TypeChange:LongWord);virtual;

		function	DeleteItemFromList(ItemID:PItemIDList):Boolean;virtual;
		function	AddItemIDToList(ItemID:PItemIDList;SelBool:Boolean):Boolean;virtual;
		function	RenameItemInList(ItemID1,ItemID2:PItemIDList):Boolean;virtual;
		function	UpdateItemInList(ItemID:PItemIDList):Boolean;virtual;
		function	MediaRemoveInList(ItemID:PItemIDList):Boolean;virtual;
		function	MediaInsertInList(ItemID:PItemIDList):Boolean;virtual;
		function	CheckFreeSpace(ItemID:PItemIDList):Boolean;virtual;

		procedure	ValidateMyComputer;

		procedure	CreateNotifyEvent;
		procedure DestroyNotifyEvent;
		procedure	StartDragDrop;
		procedure	MakeColumns;

		function	BreakApartList(const Value:String;Cond:String):TStringList;
		function	TestNotInMatchSpec(const Value:String):Boolean;

		procedure	UpdateItem(SI:PNameSpaceItem);
		procedure	CreateColumnBitmap;
		procedure	SetColumnBitmap;

		procedure RefreshFolder;
		procedure	ClearIconLoad;

		procedure	GetDetailsInterface;

		function	DisplayNameItem(Item:PNameSpaceItem;TypeName:TTypeParsingName	=	tpnInFolder):String;
		procedure NameSpaceSort;
		function	OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
		function	OwnerDataFind(Find: TItemFind; const FindString: string; const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean): Integer; override;

		//add ver 0.81
		procedure UpdateCurrentListOnNotify;

		//add ver 0.87
		procedure	InitFoldersData;

		//add in v 0.89
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;

	public
		{ Public declarations }
		constructor Create(Owner:TComponent);override;
		destructor Destroy;override;

		procedure	CurrentFolderFromID(ID:PItemIDList;SenderTreeView	:	TCustomTreeView	=	nil);virtual;
		function	SelectedNames(AType:TTypeParsingName	=	tpnInFolder):TStringList;
		function	CurrentName(AType:TTypeParsingName	=	tpnInFolder):String;
		function	ExecuteCommand(ACommand:String):Boolean;
		procedure	DeleteSelect;
		function  MakeFolder:Integer;

		procedure	MakeEditSelected;

		function	DisplayName(Item:TListItem=nil;TypeName:TTypeParsingName	=	tpnInFolder):String;

		//add v 0.87
		procedure	SaveListFoldersData(AStream:TStream);
		procedure LoadListFoldersData(AStream:TStream);

		//add v 0.90
		procedure	SetCurrentToParent;

		property TypeDisplay	:	TTypeDisplay read FTypeDisplay;
		property SortByColumn	:	Integer read FSortByColumn write SetSortByColumn;

		//add in v0.94  (propose )
		property Items;

	published
		{ Published declarations }
		property TypeFormatSize 			: TTypeFormatSize read FTypeFormatSize write SetTypeFormatSize default tfsAutomatic;
		property TypeEnumNames				: TTypeEnumNames read FTypeEnumNames write SetTypeEnumNames default teAll;
		property ShowHiddenName				:	Boolean read FHiddenName write SetHiddenName default False;
		property RespondOnChangeNotify: Boolean read FRespondOnChangeNotify write SetRespondOnChangeNotify default True;
		property CurrentFolder				:	String read GetCurrentFolder write SetCurrentFolder;
		property DefaultNewFolderName	:	String read FDefaultNewFolderName write FDefaultNewFolderName;
		property TypeThousand					: TTypeThousand read FTypeThousand write SetTypeThousand default tt1024;
		property SortOrder						:	TSortOrder read FSortOrder write SetSortOrder default soAscending;
		property TypeDisplayMode			:	TTypeDisplayMode read FTypeDisplayMode write SetTypeDisplayMode default tdmAsIs;
		property MatchSpec						:	String read FMatchSpec write SetMatchSpec;
		property TypePopUpMenu				: TTypeContextMenu read FTypePopUpMenu write SetTypePopUpMenu default tcmOnlyContextMenu;

		//add v 0.85
		property DateTimeFormat	:	String read FDateTimeFormat write SetDateTimeFormat;

		//add v0.98
		property NumberOfDecimalInSize: TTypeNumberOfDecimal read FNumberOfDecimalInSize write SetNumberOfDecimalInSize default tndThree;

		property Align;
		property AllocBy;
		property Anchors;
		property BiDiMode;
		property BorderStyle;
		property BorderWidth;
		property Color;
		property Constraints;
		property Ctl3D;
		property Enabled;
		property Font;
		property FlatScrollBars;
		property FullDrag;
		property HideSelection;
		property HotTrack;
		property HotTrackStyles;

		property IconOptions;
		property MultiSelect default True;
		property ReadOnly default False;
		property RowSelect;
		property ParentBiDiMode;
		property ParentColor default False;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;

		//add in v 0.89
		property BackGroundPopUpMenu:TPopupMenu read FBackGroundPopupMenu write SetBackGroundPopupMenu;

		property ShowColumnHeaders;
		property ShowHint;
		//add in v0.97
		property StandardDblClick:Boolean read FStandardDblClick write FStandardDblClick default False;

		property TabOrder;
		property TabStop default True;
		property ViewStyle:TViewStyle read GetViewStyle write NewSetViewStyle default vsReport;
		property Visible;

		//add in v 0.89
		property OnUpdateBackGroundMenu:TUpdateBackGroundMenu read FOnUpdateBackGroundMenu write FOnUpdateBackGroundMenu;

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
		property OnInsert;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnResize;
		property OnSelectItem;
		property OnStartDock;

		//add in 0.94
		property OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent read FOnBeforeNotifyEvent write FOnBeforeNotifyEvent;
		property OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent read FOnChangeNotifyEvent write FOnChangeNotifyEvent;
	end;

procedure Register;

implementation

uses
	NameSpaceWideChar, NameSpaceTree;

const
	cpSize	=	1024;

// add in v 0.85
resourceString
	dtfShortWindows	=	'SHORT WINDOWS';
	dtfLongWindows	=	'LONG WINDOWS';
	hcFolderColumns	=	'?FOLDERCOLUMNS?';
	hcMyCompColumns	=	'?MYCOMPUTERCOLUMNS?';

procedure Register;

begin
	RegisterComponents('View Library', [TNameSpaceList]);
end;

function ListSortFunc(Item1, Item2: Pointer): Integer;

var sh1,sh2:PNameSpaceItem;
		OwnSize1,OwnSize2:Boolean;
		A1,A2:LongWord;
		F1,F2:Boolean;
		Date1,Date2:TDateTime;
    //add in v 0.85
   	CompareSpaceList	:	TNameSpaceList;

begin
	sh1:=PNameSpaceItem(Item1);
	sh2:=PNameSpaceItem(Item2);
  //add in v 0.85
	CompareSpaceList:=TNameSpaceList(sh1.CompareSpaceList);
	with CompareSpaceList do begin
	if IS_PIDL_DialUpNetworking then begin
		if (NameSpace.SubNameSpaces.IndexOf(sh1)=-1) then sh1:=nil;
		if (NameSpace.SubNameSpaces.IndexOf(sh2)=-1) then sh2:=nil;
	end;
	UpdateData(sh1);UpdateData(sh2);
	if (sh1<>nil) and (sh2<>nil) then	case TypeDisplay of
		tdFile	:	case SortByColumn of
			1	:	begin
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				OwnSize1:=sh1.SubItemText[Pred(SortByColumn)]<>'';
				OwnSize2:=sh2.SubItemText[Pred(SortByColumn)]<>'';
				if OwnSize1 and OwnSize2 then
					if sh1.TotalSize=sh2.TotalSize then
						Result:=0
					else
						if sh1.TotalSize>sh2.TotalSize then Result:=1 else Result:=-1
				else
					if OwnSize1=OwnSize2 then
						Result:=0
					else
						if OwnSize1 then Result:=1 else Result:=-1;
			end;
			2,4	:	begin
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				A1:=sh1.Attribute;
				A2:=sh2.Attribute;
				F1:=BOOL(A1 and SFGAO_HASSUBFOLDER) OR BOOL(A1 and SFGAO_FOLDER);
				F2:=BOOL(A2 and SFGAO_HASSUBFOLDER) OR BOOL(A2 and SFGAO_FOLDER);
				if (F1 xor F2) then
					Result:=0
				else
					Result:=SmallInt(AnsiCompareText(sh1.SubItemText[Pred(SortByColumn)],sh2.SubItemText[Pred(SortByColumn)]));
			end;
			3	: begin
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				A1:=sh1.Attribute;
				A2:=sh2.Attribute;
				F1:=BOOL(A1 and SFGAO_HASSUBFOLDER) OR BOOL(A1 and SFGAO_FOLDER);
				F2:=BOOL(A2 and SFGAO_HASSUBFOLDER) OR BOOL(A2 and SFGAO_FOLDER);
				if (F1 xor F2) then
					Result:=0
				else
					if sh1.ModDate=sh2.ModDate then
						Result:=0
					else
						if sh1.ModDate>sh2.ModDate then Result:=1 else Result:=-1;
			end;
		else
			Result:=0;
		end;
		tdMyComputer : case SortByColumn of
			1	:	Result:=SmallInt(AnsiCompareText(sh1.SubItemText[Pred(SortByColumn)],sh2.SubItemText[Pred(SortByColumn)]));
			2	:	begin
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				OwnSize1:=sh1.SubItemText[Pred(SortByColumn)]<>'';
				OwnSize2:=sh2.SubItemText[Pred(SortByColumn)]<>'';
				if OwnSize1 and OwnSize2 then
					if sh1.TotalSize=sh2.TotalSize then
						Result:=0
					else
						if sh1.TotalSize>sh2.TotalSize then Result:=1 else Result:=-1
				else
					if OwnSize1=OwnSize2 then
						Result:=0
					else
						if OwnSize1 then Result:=1 else Result:=-1;
			end;
			3	:	begin
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				OwnSize1:=sh1.SubItemText[Pred(SortByColumn)]<>'';
				OwnSize2:=sh2.SubItemText[Pred(SortByColumn)]<>'';
				if OwnSize1 and OwnSize2 then
					if sh1.FreeAvailable=sh2.FreeAvailable then
						Result:=0
					else
						if sh1.FreeAvailable>sh2.FreeAvailable then Result:=1 else Result:=-1
				else
					if OwnSize1=OwnSize2 then
						Result:=0
					else
						if OwnSize1 then Result:=1 else Result:=-1;
			end;
		else
			Result:=0;
		end;
		tdSpecialFolder	: if SortByColumn=0 then Result:=0 else begin
			A1:=sh1.Attribute;
			A2:=sh2.Attribute;
			F1:=BOOL(A1 and SFGAO_HASSUBFOLDER) OR BOOL(A1 and SFGAO_FOLDER);
			F2:=BOOL(A2 and SFGAO_HASSUBFOLDER) OR BOOL(A2 and SFGAO_FOLDER);
			if (F1 xor F2) then
				Result:=0
			else begin
				Date1:=0;Date2:=0;
				F1:=False;F2:=False;
				if sh1.SubItemText.Count<SortByColumn then SortByColumn:=sh1.SubItemText.Count;
				if sh2.SubItemText.Count<SortByColumn then SortByColumn:=sh2.SubItemText.Count;
				if (sh1.SubItemText.Objects[Pred(SortByColumn)]<>nil) or (sh2.SubItemText.Objects[Pred(SortByColumn)]<>nil) then begin
					OwnSize1:=sh1.SubItemText[Pred(SortByColumn)]<>'';
					OwnSize2:=sh2.SubItemText[Pred(SortByColumn)]<>'';
					if OwnSize1 and OwnSize2 then
						if sh1.TotalSize=sh2.TotalSize then
							Result:=0
						else
							if sh1.TotalSize>sh2.TotalSize then Result:=1 else Result:=-1
					else
						if OwnSize1=OwnSize2 then
							Result:=0
						else
							if OwnSize1 then Result:=1 else Result:=-1;
				end else begin
					try
						if sh1.SubItemText[Pred(SortByColumn)]<>'' then begin
							Date1:=StrToDateTime(sh1.SubItemText[Pred(SortByColumn)]);
							F1:=True;
						end;
					except
					end;
					try
						if sh2.SubItemText[Pred(SortByColumn)]<>'' then begin
							Date2:=StrToDateTime(sh2.SubItemText[Pred(SortByColumn)]);
							F2:=True;
						end;
					except
					end;
					if F1 and F2 then
						if Date1=Date2 then
							Result:=0
						else
							if Date1>Date2 then Result:=1 else Result:=-1
					else
						Result:=SmallInt(AnsiCompareText(sh1.SubItemText[Pred(SortByColumn)],sh2.SubItemText[Pred(SortByColumn)]));
				end;
			end;
		end;
	else
		Result:=0;
	end else begin
		if sh1=sh2 then
			Result:=0
		else
			if sh1=nil then Result:=1 else Result:=-1;
	end;
	if (Result=0) and (sh1<>nil) and (sh2<>nil) then Result:=SmallInt(NameSpace.ShellFolder.CompareIDs(0, SHLastPIDL(sh1.ID), SHLastPIDL(sh2.ID)));
	if SortOrder=soDescending then Result:=-Result;
	end;
end;

{ TNameSpaceList }
function TNameSpaceList.AddItemIDToList(ItemID: PItemIDList;SelBool:Boolean): Boolean;

var
	ParentNS,ParentID,ParentPIDL,ID,IDComb:PItemIDList;
	SI:PNameSpaceItem;
	Item:TListItem;

begin
	Result:=False;
	if ItemID=nil then Exit;
	ID:=SHPIDLFromFileName(NameSpace.ShellFolder,ExtractFileName(SHFileNameFromPIDL(ItemID)));
	ParentPIDL:=SHParentPIDL(ItemID);
	ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
	ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
	try
		if not SHIsEqualPIDL(ParentNS,ParentID) then Exit;
		IDComb:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ID));
		Item:=LocalFindListItem(IDComb);
		if Item=nil then begin;
			//change in v 0.85
			SI:=MakeNameSpaceItem(NameSpace.ShellFolder,IDComb,TClass(Self));
			SI.Parent:=NameSpace;
			SI.ItemIndex:=NameSpace.SubNameSpaces.Add(SI);
			Items.Count:=NameSpace.SubNameSpaces.Count;
			FAddNewSaveItem:=SI;
		end else begin
			SI:=PNameSpaceItem(Item.Data);
			SHFree(SI.ID);
			SI.ID:=IDComb;
		end;
		if SelBool then begin
			Selected:=Item;
			if Selected<>nil then Selected.EditCaption;
		end;
		Result:=True;
	finally
		SHFree(ID);
		SHFree(ParentPIDL);
		SHFree(ParentID);
		SHFree(ParentNS);
	end;
	if Result then begin
		Inc(FCountUpdate);
		PostMessage(Self.Handle,WM_UPDATECURRENT,0,0);
	end;
end;

procedure TNameSpaceList.AddListToItems;

var
	scr:TCursor;
	SA:Boolean;

begin
	if NameSpace=nil then Exit;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	SA:=False;
	try
		Items.Count:=0;
		Repaint;
		if Animate=nil then begin
			Animate:=TAnimate.Create(Self);
			Animate.Transparent:=True;
			Animate.Parent:=Self;
			Animate.CommonAVI:=aviFindFolder;
			Animate.Center:=True;
			Animate.Left:=(Width-Animate.Width) div 2;
			Animate.Top:=(Height-Animate.Height) div 2;
			Animate.Active:=True;
			SA:=True;
		end;
		RepopulateItemID;
		ShellBrowser.CreateShellView(NameSpace);
		with NameSpace^ do
			if IsSpecialFolder and ((ShellFolder2=nil) and (ShellDetails=nil)) then IsSpecialFolder:=(ShellBrowser.ListHandle>0) and (ShellBrowser.HeaderHandle>0);
		CreateTypeDisplay;
		NameSpaceSort;
	finally
		if SA then begin Animate.Free;Animate:=nil end;
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceList.CanEdit(Item: TListItem): Boolean;

begin
	if Item=nil then Item:=Selected;
	Result:=inherited CanEdit(Item);
	Result:=Result and (Item<>nil)
					and (BOOL(GetAttributes(Item.Data) and SFGAO_CANRENAME))
					and (not SHIsEqualPIDL(PNameSpaceItem(Item.Data).ID,PIDL_DesktopFolder));
end;

procedure TNameSpaceList.CNNOTIFY(var Msg: TWMNotify);

var
	SI:PNameSpaceItem;

begin
	if (Msg.NMHdr.Code=LVN_GETDISPINFO) then with PLVDispInfo(Msg.NMHdr).Item do try
		if iItem>=Items.Count then Exit;
		SI:=PNameSpaceItem(NameSpace.SubNameSpaces[iItem]);
		if Assigned(SI) then with SI^ do begin
			UpdateData(SI);
			if iSubItem=0 then begin
				if Bool(Mask and LVIF_TEXT) then begin
					if NameSpaceText='' then NameSpaceText:=DisplayNameItem(SI);
					if NameSpaceText='' then
						pszText:=#0
					else
						StrPLCopy(pszText,NameSpaceText,cchTextMax-1);
				end;
				if BOOL(Mask and LVIF_IMAGE) then iImage:=ImageIndex;
				if BOOL(StateMask and LVIS_OVERLAYMASK) then begin
					State:=State or LongWord(IndexToOverlayMask(OverlayIndex+1));
					StateMask:=StateMask or LVIS_OVERLAYMASK;
					Mask:=Mask or LVIF_STATE;
				end;
				//change in v0.97
				if BOOL(StateMask and LVIS_STATEIMAGEMASK) then begin
					//change in v0.97
					if IsCut then begin
						ListView_SetItemState(Handle, iItem, LVIS_CUT, LVIS_CUT);
						State:=State or LVIS_CUT;
						StateMask:=StateMask or LVIS_CUT;
						Mask:=Mask or LVIF_STATE;
					//add in v0.97
					end else begin
						ListView_SetItemState(Handle, iItem, 0, LVIS_CUT);
						State:=State and UINT(not LVIS_CUT);
						StateMask:=StateMask or LVIS_CUT;
						Mask:=Mask or LVIF_STATE;
					end;
					//end add in v0.97
				end;
			end else
				if SubItemText<>nil then
					if SubItemText.Count>Pred(iSubItem) then
						if SubItemText[Pred(iSubItem)]='' then
							pszText:=#0
						else
							StrPLCopy(pszText,SubItemText[Pred(iSubItem)],cchTextMax-1);
		end;
	except
	end else begin
		case Msg.NMHdr.Code of
			LVN_BEGINDRAG,
			LVN_BEGINRDRAG	:	StartDragDrop;
			//ver 0.82
			//avoid problem with select item
			//this is bug in TCustomListView in OwnerDraw mode
			LVN_ITEMCHANGED : with PNMListView(Msg.NMHdr)^ do begin
				ListView_RedrawItems(Handle, iItem, iItem);
				inherited;
			end;
			LVN_ODSTATECHANGED : with PNMLVODStateChange(Msg.NMHdr)^ do begin
				ListView_RedrawItems(Handle, iFrom, iTo);
				inherited;
			end;
		else
			inherited;
		end;
		SetColumnBitmap;
	end;
end;

procedure TNameSpaceList.ColClick(Column: TListColumn);

begin
	if SortByColumn=Column.Index then
		if SortOrder=soAscending then SortOrder:=soDescending else SortOrder:=soAscending
	else begin
		SortByColumn:=Column.Index;
		FSortOrder:=soAscending;
		NameSpaceSort;
	end;
end;

constructor TNameSpaceList.Create(Owner: TComponent);

begin
	inherited Create(Owner);

	//add in v0.98
	NumberOfDecimalInSize:=tndThree;

	//add in v 0.87
	ListFoldersData:=TListClass.Create;
	InitFoldersData;

	ShellBrowser:=TShellBrowser.Create(Self);
	OwnerData:=True;
	ListNotifyTree:=TList.Create;
	LargeList:=TImageList.Create(Self);
	SmallList:=TImageList.Create(Self);
	MultiSelect:=True;
	ViewStyle:=vsReport;
	SortType:=stNone;
	FRespondOnChangeNotify:=True;
	DefaultNewFolderName:='New Folder';
	ContextPopUp:=TContextPopUpMenu.Create(Self);

	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot:=TNameSpaceTarget.Create(Self);
	if ActiveDragAndDrop then FDropSourceRoot:=TNameSpaceSource.Create(Self);

	ListMatchSpec:=TStringList.Create;
	SaveDropTargetIndex:=-1;
	CreateColumnBitmap;

end;

procedure TNameSpaceList.CreateNameSpace;

var
	FileInfo: TSHFileInfo;
	SaveNameSpace:PNameSpaceItem;

begin
  //change in v 0.85
	SaveNameSpace:=MakeDesktop(TClass(Self));
	try
		LargeList.ShareImages:=True;
		FillChar(FileInfo,Sizeof(FileInfo),0);
		LargeList.Handle:=SHGetFileInfo(Pointer(SaveNameSpace.ID), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
		LargeImages:=LargeList;
		SmallList.ShareImages:=True;
		FillChar(FileInfo,Sizeof(FileInfo),0);
		SmallList.Handle:=SHGetFileInfo(Pointer(SaveNameSpace.ID), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
		SmallImages:=SmallList;
	finally
		DisposeNameSpaceItemAllParent(SaveNameSpace);
	end;
end;

procedure TNameSpaceList.CreateNotifyEvent;

var
	Flags:Integer;

begin
	if csDesigning in ComponentState then Exit;
	if NameSpace=nil then Exit;
	if RespondOnChangeNotify and (FNotifyHandle=0) then begin
		FNotifyRegister.PIDL:=NameSpace.ID;
		FNotifyRegister.WatchSubTree:=False;
		if IsExistChangeNotificationLock then
			Flags:=SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS or SHCNF_NO_PROXY
		else
			Flags:=SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS;
		FNotifyHandle:=SHChangeNotifyRegister(Self.Handle,
									 Flags,
									 SCHNE_ALLEVENTSMASK,
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

procedure TNameSpaceList.CreateTypeDisplay;


begin
	try
		if SHIsEqualPIDL(NameSpace.ID, PIDL_MyComputer) then FTypeDisplay:=tdMyComputer else
			if NameSpace.IsSpecialFolder then
				FTypeDisplay:=tdSpecialFolder
			else
				FTypeDisplay:=tdFile
	finally
		MakeColumns;
	end;
end;

procedure TNameSpaceList.CreateWnd;

begin
	inherited;
	ListView_SetCallbackMask(Handle,LVIS_OVERLAYMASK or LVIS_STATEIMAGEMASK);
	if (csDesigning in ComponentState) and (NameSpace=nil) and (not (csLoading in ComponentState)) then	CurrentFolder:=rsDesktop;
	CreateNotifyEvent;

	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot.RegisterDragDrop;

	SetColumnBitmap;	
end;

procedure TNameSpaceList.CurrentFolderFromID(ID: PItemIDList;SenderTreeView	:	TCustomTreeView	=	nil);

var
	SaveNameSpace:PNameSpaceItem;
	scr:TCursor;
	i:Integer;

begin
	if (NameSpace<>nil) and SHIsEqualPIDL(NameSpace.ID,ID) then Exit;
	if (ID=nil) and (NameSpace<>nil) and SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) then Exit;
	DestroyNotifyEvent;
	SaveNameSpace:=NameSpace;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
    //change in v 0.85  
		NameSpace:=MakeNameSpaceItemWithParent(ID,TClass(Self));
		GetDetailsInterface;
		AddListToItems;

		//change in v 0.88
		if ActiveDragAndDrop then FDropTargetRoot.CreateDropTarget(NameSpace);
	finally
		DisposeNameSpaceItemAllParent(SaveNameSpace);
		Screen.Cursor:=scr;

		// change in v0.91
		for i:=0 to Pred(ListNotifyTree.Count) do
			if ListNotifyTree[i]<>SenderTreeView then
				with TNameSpaceTree(ListNotifyTree[i]) do
					Selected:=LocatePIDL(Self.NameSpace.ID,True);

		if SortByColumn>Columns.Count then SortByColumn:=0;
		CreateNotifyEvent;
		SetColumnBitmap;
	end;
end;

procedure TNameSpaceList.DblClick;

begin
	if StandardDblClick then inherited else TestAndExecuteDefault;
end;

function TNameSpaceList.DeleteItemFromList(ItemID: PItemIDList): Boolean;

var
	ParentNS,ParentID,ParentPIDL,ID:PItemIDList;
	SI:PNameSpaceItem;
	Item:TListItem;
	Name:String;

begin
	Result:=False;
	if ItemID=nil then Exit;
	Name:=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID));
	if NSWideUpperCaseA(SHFileNameFromPIDL(NameSpace.ID))<>Name then begin
		ParentPIDL:=SHParentPIDL(ItemID);
		ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
		ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
		ID:=nil;
		try
			try
				if not SHIsEqualPIDL(ParentNS,ParentID) then Exit;
				ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID));
				Item:=LocalFindListItem(ID);
				if Item<>nil then begin
					SI:=Item.Data;
					NameSpace.SubNameSpaces.Delete(NameSpace.SubNameSpaces.IndexOf(SI));
					DisposeNameSpaceItem(SI);
					Items.Count:=NameSpace.SubNameSpaces.Count;
				end;
			except
				CurrentFolder:=ExtractFilePath(Name);
			end;
			Result:=True;
		finally
			SHFree(ID);
			SHFree(ParentPIDL);
			SHFree(ParentID);
			SHFree(ParentNS);
			if Items.Count=0 then Invalidate;
		end;
	end else begin
		CurrentFolder:=ExtractFilePath(Name);
		Result:=True;
	end;
	if Result then begin
		Inc(FCountUpdate);
		PostMessage(Self.Handle,WM_UPDATECURRENT,0,0);
	end;
end;

procedure TNameSpaceList.DeleteSelect;

begin
	ExecuteCommand(scDelete);
end;

destructor TNameSpaceList.Destroy;

begin
	//add in v 0.87
	ListFoldersData.Free;

	ShellBrowser:=nil;

	//start change in v0.95
	if ActiveDragAndDrop then begin
		FDropTargetRoot.UnLockLockObjectExternal;
		FDropTargetRoot:=nil;
		FDropSourceRoot:=nil;
	end;
	//end change in v0.95

	ListNotifyTree.Free;
	ListNotifyTree:=nil;
	ContextPopUp.Free;
	DestroyNotifyEvent;
	DisposeNameSpaceItemAllParent(NameSpace);
	ListMatchSpec.Free;
	AscendingBitmap.Free;
	DescendingBitmap.Free;
	inherited;
end;

procedure TNameSpaceList.DestroyNotifyEvent;

begin
	if csDesigning in ComponentState then Exit;
	try
		if FNotifyHandle<>0 then SHChangeNotifyDeregister(FNotifyHandle);
	finally
		FNotifyHandle:=0;
	end;
end;

procedure TNameSpaceList.DestroyWnd;

begin
	DestroyNotifyEvent;
	
	//change in v 0.88
	if ActiveDragAndDrop then FDropTargetRoot.RevokeDragDrop;
	inherited;
end;

function TNameSpaceList.DisplayName(Item: TListItem=nil; TypeName: TTypeParsingName=tpnInFolder): String;

begin
	if Item=nil then
		if NameSpace<>nil then
			Result:=GetDisplayName(SF_Desktop,NameSpace.ID,TypeName)
		else
			Result:=''
	else
		with PNameSpaceItem(Item.Data)^ do
		if Parent<>nil then
			Result:=GetDisplayName(Parent.ShellFolder,SHLastPIDL(ID),TypeName)
		else
			Result:=GetDisplayName(SF_Desktop,ID,TypeName);
	case TypeDisplayMode of
		tdmLowerCase	:	Result:=NSWideLowerCaseA(Result);
		tdmUpperCase	:	Result:=NSWideUpperCaseA(Result);
		tdmFirstUpper	:	Result:=NSWideFirstUpperA(Result);
	end;
end;

procedure TNameSpaceList.MakeColumns;

begin
	LockWindowUpdate(Handle);
	try
		case TypeDisplay of
			tdFile						:	MakeFileListColumns;
			tdMyComputer  		:	MakeDiskListColumns;
			tdSpecialFolder		:	MakeSpecialColumnsWithTest;
		else
			ClearListColumns;
		end;
	finally
		LockWindowUpdate(0);	
	end;
end;

procedure TNameSpaceList.Edit(const Item: TLVItem);

var AItem:TListItem;
		ID:PItemIDList;
		SI:PNameSpaceItem;

begin
	AItem:=Selected;
	if AItem<>nil then SI:=AItem.Data else SI:=nil;
	inherited Edit(Item);
	if (AItem<>nil) and (SI.NameSpaceText<>AItem.Caption) then begin
		SI.NameSpaceText:=AItem.Caption;
		ID:=SHClonePIDL(PNameSpaceItem(AItem.Data).ID);
		RenameFolderItem(PNameSpaceItem(AItem.Data),AItem.Caption);
		try
			if not RespondOnChangeNotify then
				UpdateItem(AItem.Data)
			else
				if BOOL(PNameSpaceItem(AItem.Data).Attribute and SFGAO_FOLDER) then
					SHChangeNotify(SHCNE_RENAMEFOLDER,SHCNF_IDLIST or SHCNF_FLUSH,ID,PNameSpaceItem(AItem.Data).ID)
				else
					SHChangeNotify(SHCNE_RENAMEITEM,SHCNF_IDLIST or SHCNF_FLUSH,ID,PNameSpaceItem(AItem.Data).ID);
		finally
			SHFree(ID);
		end;
	end;
end;

const
	KiloByte	:	LongWord	=	1024;
	MegaByte	:	LongWord	=	1024*1024;
	GigaByte	:	LongWord	= 1024*1024*1024;
	TeraByte	:	Int64			= Int64(1024)*1024*1024*1024;

function TNameSpaceList.FormatSize(Value: Int64): String;

begin
	//change in v0.98
	case TypeFormatSize of
		tfsByte			 : Result:=Format('%s Byte',[FloatToStrF(Value,ffNumber,18,0)]);
		tfsKiloByte  : Result:=Format('%s KB',[FloatToStrF(Value/KiloByte,ffNumber,18,Integer(NumberOfDecimalInSize))]);
		tfsMegaByte  : Result:=Format('%s MB',[FloatToStrF(Value/MegaByte,ffNumber,18,Integer(NumberOfDecimalInSize))]);
		tfsGigaByte  : Result:=Format('%s GB',[FloatToStrF(Value/GigaByte,ffNumber,18,Integer(NumberOfDecimalInSize))]);
		tfsTeraByte  : Result:=Format('%s TB',[FloatToStrF(Value/TeraByte,ffNumber,18,Integer(NumberOfDecimalInSize))]);
	else
		if (Value div TeraByte)>0 then
			Result:=Format('%s TB',[FloatToStrF(Value/TeraByte,ffNumber,18,Integer(NumberOfDecimalInSize))])
		else
			if (Value div GigaByte)>0 then
				Result:=Format('%s GB',[FloatToStrF(Value/GigaByte,ffNumber,18,Integer(NumberOfDecimalInSize))])
			else
				if (Value div MegaByte)>0 then
					Result:=Format('%s MB',[FloatToStrF(Value/MegaByte,ffNumber,18,Integer(NumberOfDecimalInSize))])
				else
					if (Value div KiloByte)>0 then
						Result:=Format('%s KB',[FloatToStrF(Value/KiloByte,ffNumber,18,Integer(NumberOfDecimalInSize))])
					else
						Result:=Format('%s Byte',[FloatToStrF(Value,ffNumber,18,0)]);
	end;
	//end change in v0.98
end;

function TNameSpaceList.GetCurrentFolder: String;

begin
	if NameSpace=nil then Result:='' else begin
		if SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) then
			Result:=rsDesktop
		else
			Result:=DisplayName(nil,tpnForParsing);
	end;
end;

function	TNameSpaceList.CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;

begin
	Result:=True;
	if Assigned(OnBeforeNotifyEvent) then Result:=OnBeforeNotifyEvent(Self,TypeChange,Item1,Item2);
end;

procedure	TNameSpaceList.CallChangeNotifyEvent(TypeChange:LongWord);

begin
	if Assigned(OnChangeNotifyEvent) then OnChangeNotifyEvent(Self,TypeChange);
end;

function TNameSpaceList.HandleChangeNotify(TypeChange: LongWord; var Item1, Item2: PItemIDList): Boolean;

//change in v0.94

var
	IT1,IT2 : PItemIDList;
	SaveTC 	: LongWord;

begin
	Result:=False;
	IT1:=SHClonePIDL(Item1);
	IT2:=SHClonePIDL(Item2);
	SaveTC:=TypeChange;
	try
		if not CallBeforeNotifyEvent(TypeChange,IT1,IT2) then Exit;
		TypeChange:=TypeChange and (not SHCNE_INTERRUPT);
		if (TypeChange and (SHCNE_RMDIR or SHCNE_DELETE or SHCNE_DRIVEREMOVED))<>0 then
			Result:=DeleteItemFromList(IT1);
		if (TypeChange and (SHCNE_MKDIR or SHCNE_CREATE or SHCNE_DRIVEADD))<>0 then
			Result:=AddItemIDToList(IT1,False);
		if (TypeChange and (SHCNE_RENAMEFOLDER or SHCNE_RENAMEITEM))<>0 then
			Result:=RenameItemInList(IT1,IT2);
		if (TypeChange and
				(	SHCNE_NETSHARE or SHCNE_NETUNSHARE or SHCNE_UPDATEIMAGE or
					SHCNE_UPDATEDIR or SHCNE_UPDATEITEM ))<>0 then Result:=UpdateItemInList(IT1);
		if (TypeChange and SHCNE_MEDIAINSERTED)<>0 then Result:=MediaInsertInList(IT1);
		if (TypeChange and SHCNE_MEDIAREMOVED)<>0 then Result:=MediaRemoveInList(IT1);
		if (TypeChange and SHCNE_FREESPACE)<>0 then Result:=CheckFreeSpace(IT1);
		CallChangeNotifyEvent(SaveTC);
	finally
		SHFree(IT1);
		SHFree(IT2);
	end;
end;

procedure TNameSpaceList.Loaded;

begin
	inherited;
	FTypeDisplay:=tdNothing;
	CreateTypeDisplay;
	SetColumnBitmap;
end;

function TNameSpaceList.LocalFindListItem(Item: PItemIDList): TListItem;

var
	i:Integer;

begin
	Result:=nil;
	i:=0;
	while (Result=nil) and (i<Items.Count) do begin
		if SHIsEqualPIDL(PNameSpaceItem(Items[i].Data).ID,Item) then	Result:=Items[i];
		Inc(i);
	end;
end;

procedure TNameSpaceList.MakeDiskListColumns;

var
	NewColumn:TListColumn;
	Pos:Integer;

	function ActionFind(AItem:TListFoldersData):Boolean;

	begin
		Result:=AItem.Folder=hcMyCompColumns;
	end;

	procedure	Action(AItem:PNameSpaceListHeader);

	begin
		NewColumn:=Columns.Add;
		NewColumn.Caption:=AItem.Caption;
		NewColumn.AutoSize:=False;
		NewColumn.Alignment:=AItem.Alignment;
		NewColumn.Width:=AItem.Width;
	end;

begin
	Columns.BeginUpdate;
	try
		Columns.Clear;
		Pos:=ListFoldersData.FirstThat(@ActionFind,0);
		if Pos=-1 then
			CurrentFolderData:=TListFoldersData(ListFoldersData[1])
		else
			CurrentFolderData:=TListFoldersData(ListFoldersData[Pos]);
		CurrentFolderData.ForEach(@Action,0);
	finally
		Columns.EndUpdate;
	end;
end;

procedure TNameSpaceList.MakeEditSelected;

begin
	if Selected<>nil then Selected.EditCaption;
end;

procedure TNameSpaceList.MakeFileListColumns;

var
	NewColumn:TListColumn;
	Pos:Integer;

	function ActionFind(AItem:TListFoldersData):Boolean;

	begin
		Result:=AItem.Folder=hcFolderColumns;
	end;

	procedure	Action(AItem:PNameSpaceListHeader);

	begin
		NewColumn:=Columns.Add;
		NewColumn.Caption:=AItem.Caption;
		NewColumn.AutoSize:=False;
		NewColumn.Alignment:=AItem.Alignment;
		NewColumn.Width:=AItem.Width;
	end;

begin
	Columns.BeginUpdate;
	try
		Columns.Clear;
		Pos:=ListFoldersData.FirstThat(@ActionFind,0);
		if Pos=-1 then
			CurrentFolderData:=TListFoldersData(ListFoldersData[0])
		else
			CurrentFolderData:=TListFoldersData(ListFoldersData[Pos]);
		CurrentFolderData.ForEach(@Action,0);
	finally
		Columns.EndUpdate;
	end;
end;

function TNameSpaceList.MakeFolder: Integer;

var DefFolder,NewFolder:String;
		PIDL,LID:PItemIDList;
		Buffer : Pointer;
		scr:TCursor;
		FlagC:Boolean;
		Item:TListItem;
		SizeBuffer:Integer;

begin
	if NameSpace=nil then begin Result:=-1;Exit end;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		if SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) then
			NewFolder:=SHFileNameFromPIDL(PIDL_DesktopFolder)
		else
			NewFolder:=SHFileNameFromPIDL(NameSpace.ID);
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

		if Win32Platform > VER_PLATFORM_WIN32_WINDOWS then
			FlagC:=CreateDirectoryW(StringToOleStr(NewFolder),nil)
		else
			FlagC:=CreateDirectory(PChar(NewFolder),nil);

		Result:=0;
		if FlagC then begin
			LID:=nil;
			if SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) or SHIsEqualPIDL(NameSpace.ID,PIDL_DesktopFolder) then
				PIDL:=SHPIDLFromFileName(NewFolder)
			else begin
				LID:=SHPIDLFromFileName(NameSpace.ShellFolder,ExtractFileName(NewFolder));
				PIDL:=SHCombinePIDL(NameSpace.ID,LID);
			end;
			try
				if RespondOnChangeNotify then begin
					SHChangeNotify(SHCNE_MKDIR,SHCNF_IDLIST	or SHCNF_FLUSH,PIDL,PIDL);
					Application.ProcessMessages;
					Item:=LocalFindListItem(PIDL);
					if Item<>nil then begin Selected:=Item;Selected.EditCaption end;
				end else
					AddItemIDToList(PIDL,True);
			finally
				SHFree(PIDL);
				SHFree(LID);
			end;
		end else
			Result:=1;
	finally
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceList.MediaInsertInList(ItemID: PItemIDList): Boolean;

var
	ParentNS,ParentID,ParentPIDL,ID:PItemIDList;
	Item:TListItem;

begin
	Result:=False;
	if ItemID=nil then Exit;
	ParentPIDL:=SHParentPIDL(ItemID);
	ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
	ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
	try
		ValidateMyComputer;
		if (not SHIsEqualPIDL(ParentNS,ParentID)) then Exit;
		ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID));
		try
			Item:=LocalFindListItem(ID);
			if Item<>nil then begin
				UpdateItem(Item.Data);
				PNameSpaceItem(Item.Data).NameSpaceText:='';
			end;
		finally
			SHFree(ID);
		end;
		Result:=True;
	finally
		SHFree(ParentPIDL);
		SHFree(ParentID);
		SHFree(ParentNS);
		//add in v 0.88
		if NSWideUpperCaseA(SHFileNameFromPIDL(NameSpace.ID))=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID)) then RefreshFolder;		
	end;
end;

function TNameSpaceList.MediaRemoveInList(ItemID: PItemIDList): Boolean;

var
	ParentNS,ParentID,ParentPIDL,ID:PItemIDList;
	Item:TListItem;
	Name:String;

begin
	Result:=False;
	if ItemID=nil then Exit;
	Name:=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID));
	if Copy(NSWideUpperCaseA(SHFileNameFromPIDL(NameSpace.ID)),1,3)<>Copy(Name,1,3) then begin
		ParentPIDL:=SHParentPIDL(ItemID);
		ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
		ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
		ID:=nil;
		try
			ValidateMyComputer;
			if not SHIsEqualPIDL(ParentNS,ParentID) then Exit;
			ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID));
			try
				Item:=LocalFindListItem(ID);
				if Item<>nil then begin
					UpdateItem(Item.Data);
					PNameSpaceItem(Item.Data).NameSpaceText:='';
				end;
			finally
				SHFree(ID);
			end;
			Result:=True;
		finally
			SHFree(ID);
			SHFree(ParentPIDL);
			SHFree(ParentID);
			SHFree(ParentNS);
		end;
	end else begin
		if (Length(Name)=3) and (Name[3]='\') and (Name[2]=':') then
			CurrentFolderFromID(PIDL_MyComputer)
		else
			CurrentFolder:=ExtractFilePath(Name);
		Result:=True;
	end;
end;

function TNameSpaceList.RenameItemInList(ItemID1,	ItemID2: PItemIDList): Boolean;

var
	ID,ParentNS,ParentID,ParentPIDL:PItemIDList;
	SI:PNameSpaceItem;
	Item:TListItem;
	Name:String;

begin
	Result:=False;
	if (ItemID1=nil) or (ItemID2=nil) then Exit;
	Name:=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID1));
	if SHIsParentPIDL(NameSpace.ID,ItemID1,True) and SHIsParentPIDL(NameSpace.ID,ItemID2,True) then begin
		if NSWideUpperCaseA(SHFileNameFromPIDL(NameSpace.ID))<>Name then begin
			ParentPIDL:=SHParentPIDL(ItemID1);
			ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
			ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
			try
				if not SHIsEqualPIDL(ParentNS,ParentID) then Exit;
				ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID1));
				try
					Item:=LocalFindListItem(ID);
					if Item<>nil then begin
						SI:=PNameSpaceItem(Item.Data);
						SHFree(ID);
						SHFree(SI.ID);
						ID:=SHPIDLFromFileName(NameSpace.ShellFolder,ExtractFileName(SHFileNameFromPIDL(ItemID2)));
						SI.ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ID));
						UpdateItem(SI);
						SI.NameSpaceText:='';
					end;
				finally
					SHFree(ID);
				end;
				Result:=True;
			finally
				SHFree(ParentPIDL);
				SHFree(ParentID);
				SHFree(ParentNS);
			end;
		end else begin
			CurrentFolder:=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID2));
			Result:=True;
		end;
	end else begin
		if SHIsParentPIDL(NameSpace.ID,ItemID2,True) then
			Result:=AddItemIDToList(ItemID2,False)
		else
			if SHIsParentPIDL(NameSpace.ID,ItemID1,True) then	Result:=DeleteItemFromList(ItemID1);
	end;
	if Result then begin
		Inc(FCountUpdate);
		PostMessage(Self.Handle,WM_UPDATECURRENT,0,0);
	end;	
end;

procedure TNameSpaceList.RepopulateItemID;

var
	Enum:DWORD;
	i:Integer;
	scr:TCursor;
	SA:Boolean;
	SI:PNameSpaceItem;

begin
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	if Animate=nil then begin
		Animate:=TAnimate.Create(Self);
		Animate.Transparent:=True;
		Animate.Parent:=Self;
		Animate.CommonAVI:=aviFindFolder;
		Animate.Center:=True;
		Animate.Left:=(Width-Animate.Width) div 2;
		Animate.Top:=(Height-Animate.Height) div 2;
		Animate.Active:=True;
		SA:=True;
	end else
		SA:=False;
	try
		case TypeEnumNames of
			teFolder 		: Enum:=EnumsFolder;
			teNonFolder	: Enum:=EnumsNonFolder;
		else
			Enum:=EnumsAllOneHidden;
		end;
		if ShowHiddenName then Enum:=Enum or EnumsHidden;
		PopulateIDList(NameSpace,Enum);
		if MatchSpec<>'' then begin
			i:=0;
			with NameSpace^ do while i<SubNameSpaces.Count do begin
				if not BOOL(PNameSpaceItem(SubNameSpaces[i]).Attribute and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER)) then
					if TestNotInMatchSpec(SHFileNameFromPIDL(PNameSpaceItem(SubNameSpaces[i]).ID)) then begin
						SI:=SubNameSpaces[i];
						SubNameSpaces.Delete(i);
						DisposeNameSpaceItem(SI);
						Continue;
					end;
				Inc(i);
			end;
		end;
	finally
		if SA then begin Animate.Free;Animate:=nil end;
		Screen.Cursor:=scr;
	end;
end;

function TNameSpaceList.SelectedNames(AType: TTypeParsingName	=	tpnInFolder): TStringList;

var
	Item:TListItem;

begin
	Result:=TStringList.Create;
	Item:=Selected;
	while Item<>nil do begin
		Result.Add(DisplayName(Item,AType));
		Item:=GetNextItem(Item,sdAll,[isSelected]);
	end;
end;


procedure TNameSpaceList.SetCurrentFolder(Value: String);

var
	PIDL:PItemIDList;
	scr:TCursor;

		function RemoveBackSlesh(const Value:String):String;

		begin
			Result:=Value;
			if (Result<>'') and (Result[Length(Result)]='\') then System.Delete(Result,Length(Result),1);
		end;

begin
	if Value<>CurrentFolder then begin
		scr:=Screen.Cursor;
		Screen.Cursor:=crHourGlass;
		try
			if Value='' then begin
				DisposeNameSpaceItemAllParent(NameSpace);
				NameSpace:=nil;
				CurrentFolderFromID(nil);
			end else begin
				PIDL:=nil;
				if NameSpace=nil then CreateNameSpace;
				try
					while (PIDL=nil) and (Length(Value)>2) do begin
						PIDL:=SHPIDLFromFileName(Value);
						if PIDL=nil then Value:=ExtractFilePath(RemoveBackSlesh(Value));
					end;
					CurrentFolderFromID(PIDL);
				finally
					SHFree(PIDL);
				end;
			end;
		finally
			Screen.Cursor:=scr;
		end;
	end;
end;

procedure TNameSpaceList.SetHiddenName(const Value: Boolean);

begin
	if ShowHiddenName<>Value then begin
		FHiddenName:=Value;
		AddListToItems;
	end;
end;

procedure TNameSpaceList.SetMatchSpec(const Value: String);

begin
	if MatchSpec<>Value then begin
		FMatchSpec:=Value;
		ListMatchSpec.Free;
		ListMatchSpec:=BreakApartList(FMatchSpec,';');
		AddListToItems;
	end;
end;

procedure TNameSpaceList.SetRespondOnChangeNotify(const Value: Boolean);

begin
	if FRespondOnChangeNotify<>Value then begin
		FRespondOnChangeNotify:=Value;
		if Value then CreateNotifyEvent else DestroyNotifyEvent;
	end;
end;

procedure TNameSpaceList.SetSortByColumn(const Value: Integer);

begin
	if Value>Columns.Count then
		FSortByColumn:=0
	else
		FSortByColumn:=Value;
end;

procedure TNameSpaceList.SetSortOrder(const Value: TSortOrder);

begin
	if SortOrder<>Value then begin
		FSortOrder:=Value;
		NameSpaceSort;
	end;
end;

procedure TNameSpaceList.SetTypeDisplayMode(const Value: TTypeDisplayMode);

begin
	if TypeDisplayMode<>Value then begin
		FTypeDisplayMode:=Value;
		RepopulateDisplayName;
	end;
end;

procedure TNameSpaceList.SetTypeEnumNames(const Value: TTypeEnumNames);

begin
	if TypeEnumNames<>Value then begin
		FTypeEnumNames:=Value;
		AddListToItems;
	end;
end;

procedure TNameSpaceList.SetTypeFormatSize(const Value: TTypeFormatSize);

begin
	if TypeFormatSize<>Value then begin
		FTypeFormatSize:=Value;
		if NameSpace=nil then Exit;
		ClearIconLoad;
		if ViewStyle=vsReport then Invalidate;
	end;
end;

procedure TNameSpaceList.SetTypePopUpMenu(const Value: TTypeContextMenu);

begin
	FTypePopUpMenu:=Value;
end;

procedure TNameSpaceList.SetTypeThousand(const Value: TTypeThousand);

begin
	if TypeThousand<>Value then begin
		FTypeThousand:=Value;
		if TypeThousand=tt1024 then begin
			KiloByte:=1024;
			MegaByte:=1024*1024;
			GigaByte:=1024*1024*1024;
			TeraByte:=Int64(1024)*1024*1024*1024;
		end else begin
			KiloByte:=1000;
			MegaByte:=1000*1000;
			GigaByte:=1000*1000*1000;
			TeraByte:=Int64(1000)*1000*1000*1000;
		end;
		if NameSpace=nil then Exit;
		ClearIconLoad;
		if ViewStyle=vsReport then Invalidate;
	end;
end;

procedure TNameSpaceList.StartDragDrop;

var
	PIDL: TArrayPIDL;
	i:LongWord;
	Item:PNameSPaceItem;

	function CreatePIDLList:TArrayPIDL;

	var
		Data:TListItem;
		Count:LongWord;

	begin
		if Item=NameSpace then begin
			SetLength(Result,1);
			Result[0]:=SHCloneLastPIDL(Item.ID);
		end else begin
			Data:=Selected;Count:=0;
			while Data<>nil do begin
				Inc(Count);
				SetLength(Result,Count);
				Result[Pred(Count)]:=SHCloneLastPIDL(PNameSpaceItem(Data.Data).ID);
				Data:=GetNextItem(Data,sdAll,[isSelected]);
			end;
		end;
	end;

begin
	if Selected<>nil then Item:=Selected.Data else Item:=nil;
	if Item=nil then Item:=NameSpace;
	with Item^ do if Parent<>nil then begin
		PIDL:=CreatePIDLList;
		try
			//change in v 0.88
			if ActiveDragAndDrop then FDropSourceRoot.DoDragDrop(Item.Parent.ShellFolder,PIDL);
		finally
			for i:=0 to Pred(Length(PIDL)) do SHFree(PIDL[i]);
			Finalize(PIDL);
		end;
	end;
end;

function TNameSpaceList.UpdateItemInList(ItemID: PItemIDList): Boolean;

var
	ParentNS,ParentID,ParentPIDL,ID:PItemIDList;
	SI:PNameSpaceItem;
	Item:TListItem;
	Name:String;

begin
	Result:=False;
	if ItemID=nil then Exit;

	//add in v 0.85
	if SHFilenameFromPIDL(ItemID)='' then Exit;

	Name:=NSWideUpperCaseA(SHFileNameFromPIDL(ItemID));
	if NSWideUpperCaseA(SHFileNameFromPIDL(NameSpace.ID))<>Name then begin
		ValidateMyComputer;
		ParentPIDL:=SHParentPIDL(ItemID);
		if 	SHIsEqualPIDL(NameSpace.ID,PIDL_MyComputer) and SHIsEqualPIDL(ParentPIDL,PIDL_MyComputer) then begin
			ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID));
			try
				Item:=LocalFindListItem(ID);
				if Item<>nil then begin
					SI:=PNameSpaceItem(Item.Data);
					UpdateItem(SI);
					SI.NameSpaceText:='';
				end;
			finally
				SHFree(ID);
			end;
		end else begin
			ParentNS:=SHPIDLFromFileName(SHFileNameFromPIDL(NameSpace.ID));
			ParentID:=SHPIDLFromFileName(SHFileNameFromPIDL(ParentPIDL));
			try
				if not SHIsEqualPIDL(ParentNS,ParentID) then Exit;
				ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ItemID));
				try
					Item:=LocalFindListItem(ID);
					if Item<>nil then begin
						SI:=PNameSpaceItem(Item.Data);
						SHFree(ID);
						SHFree(SI.ID);
						ID:=SHPIDLFromFileName(NameSpace.ShellFolder,ExtractFileName(SHFileNameFromPIDL(ItemID)));
						SI.ID:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ID));
						UpdateItem(SI);
						SI.NameSpaceText:='';
					end;
				finally
					SHFree(ID);
				end;
			finally
				SHFree(ParentID);
				SHFree(ParentNS);
			end;
		end;
		Result:=True;
		SHFree(ParentPIDL);
	end else
		Result:=True;
	if Result then begin
		Inc(FCountUpdate);
		PostMessage(Self.Handle,WM_UPDATECURRENT,0,0);
	end;
end;

procedure TNameSpaceList.ValidateMyComputer;

var
	dwAttr:LongWord;
	PNil:PItemIDList;

begin
	if SHIsEqualPIDL(NameSpace.ID,PIDL_MyComputer) then begin
		dwAttr:=SFGAO_VALIDATE;PNil:=nil;
		if NameSpace.ShellFolder<>nil then NameSpace.ShellFolder.GetAttributesOf(0,PNil,dwAttr);
		RepopulateDisplayName;
	end;
end;

procedure TNameSpaceList.WMCLEARDROPTARGET(var Msg: TMessage);

var
	i:Integer;

begin
	DropTarget:=nil;
	SaveDropTargetIndex:=-1;
//ver 0.82
//avoid problem with highlighted item if press Escape
//this is bug in TCustomListView in OwnerDraw mode
	for i:=ListView_GetTopIndex(Handle) to Pred(Items.Count) do Items[i].DropTarget:=False;
end;

procedure TNameSpaceList.WMDRAWITEM(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceList.WMGETITEMDATA(var Msg: TMessage);

begin
	if (SaveDropTargetIndex>-1) then PSendMsgRecord(MSG.LParam).NSItem:=NameSpace.SubNameSpaces[SaveDropTargetIndex];
end;

procedure TNameSpaceList.WMINITMENUPOPUP(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceList.WMKeyDown(var Message: TWMKeyDown);

	function IsControl(AControl:Boolean):Boolean;

  begin
  	Result:=AControl;
    if Result then case Message.CharCode of
		{$IFDEF DELPHI7_UP}
			Ord('A'),Ord('a')	:	SelectAll;
    {$ELSE}
			Ord('A'),Ord('a')	:	NewSelectAll;
    {$ENDIF}
      Ord('X'),Ord('x')	:	ExecuteCommand(scCut);      
      Ord('C'),Ord('c')	:	ExecuteCommand(scCopy);
      Ord('V'),Ord('v')	:	ExecuteCommand(scPaste);
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
			VK_F2	:	if Selected<>nil then Selected.EditCaption;
			VK_F5 : RefreshFolder;
      VK_DELETE : DeleteSelect;
			VK_RETURN : TestAndExecuteDefault;
    else
    	Result:=False;
    end;
  end;

begin
	if not (IsNoShiftState(KeyDataToShiftState(Message.KeyData)=[]) or
  	IsControl(KeyDataToShiftState(Message.KeyData)=[ssCtrl]) or
    IsShift(KeyDataToShiftState(Message.KeyData)=[ssShift])) then	inherited;
end;

procedure TNameSpaceList.WMMEASUREITEM(var Msg: TMessage);

begin
	inherited;
	ContextPopUp.HandleMenuMsg(Msg);
end;

procedure TNameSpaceList.WMRENAMEITEM(var Msg: TMessage);

begin
	MakeEditSelected;
end;

procedure TNameSpaceList.WMSELECTCUTITEM(var Msg: TMessage);

var
	Data,i:Integer;

begin
	i:=0;
	with NameSpace^ do while i<SubNameSpaces.Count do begin
		with PNameSpaceItem(SubNameSpaces[i])^ do	begin
			IsCut:=BOOL(Attribute and SFGAO_GHOSTED);
			if IsCut then Data:=LVIS_CUT else Data:=0;
			ListView_SetItemState(Handle, i, Data, LVIS_CUT);
		end;
		Inc(i);
	end;
	i:=-1;
	repeat
		i:=ListView_GetNextItem(Handle,i,LVNI_ALL or LVNI_SELECTED);
		if (i<>-1) and (Msg.LParam<>0) then	PNameSpaceItem(NameSpace.SubNameSpaces[i]).IsCut:=True;
	until i=-1;
	RePaint;
end;

procedure TNameSpaceList.WMSETDROPTARGET(var Msg: TMessage);

var
	Pos:TPoint;
	DT:TListItem;

begin
	Pos:=ScreenToClient(PPoint(Msg.LParam)^);
	DT:=GetItemAt(Pos.X,Pos.Y);
	DropTarget:=nil;
	if (DT<>nil) and (DT.Index<>SaveDropTargetIndex) then SaveDropTargetIndex:=DT.Index;
	DropTarget:=DT;
end;

function TNameSpaceList.BreakApartList(const Value: String;	Cond: String): TStringList;

var i:Integer;
		AText,ValueUpper:String;

		procedure AddText;

		begin
			Result.Add(AText);
			AText:='';
		end;

begin
	Result:=TStringList.Create;
	AText:='';
	if Cond='' then Cond:=' ';
	Cond:=NSWideUpperCaseA(Cond);
	ValueUpper:=NSWideUpperCaseA(Value);
	for i:=1 to System.Length(Value) do begin
		if System.Pos(ValueUpper[i],Cond)=0 then
			AText:=AText+Value[i]
		else
			if AText<>'' then AddText;
	end;
	if AText<>'' then Result.Add(AText);
end;

function TNameSpaceList.TestNotInMatchSpec(const Value: String): Boolean;

var
	i:Integer;

begin
	i:=0;
	while (i<ListMatchSpec.Count) do begin
		if PathMatchSpec(Value,ListMatchSpec[i]) then Break;
		Inc(i);
	end;
	Result:=i=ListMatchSpec.Count;
end;

procedure TNameSpaceList.UpdateData(SI:PNameSpaceItem);

var
	FileData: TWin32FindData;
	FileInfo: TSHFileInfo;
	SysTime	: TSystemTime;

	//add in v 0.85
	LocalFileTime : TFileTime;
	DTStr		:	String;

	LUI			:	ULARGE_INTEGER;
	Attrs		:	String;
	DT,Count:	Integer;
	TSD			:	TSHELLDETAILS;
	LVItem	:	TLVItem;
	P				:	PChar;

	function	AppendString(const First,Second:String):String;

	begin
		if First='' then Result:=Second else Result:=Format('%s, %s',[First,Second]);
	end;

	procedure	RemovePointAndComma(var Value:String);

	var
		Pos:Integer;

	begin
		repeat
			Pos:=System.Pos('.',Value);
			if Pos>0 then System.Delete(Value,Pos,1);
		until Pos=0;
		repeat
			Pos:=System.Pos(',',Value);
			if Pos>0 then System.Delete(Value,Pos,1);
		until Pos=0;
	end;

	function IsSize(Value:String):Int64;

	var
		s:String;
		Pos:Integer;

	begin
		Result:=-1;
		Value:=Trim(Value);
		s:=System.Copy(Value,Length(Value)-1,2);
		Pos:=0;
		if (s='KB') or (s='MB') or (s='GB') or (s='TB') then begin
			System.Delete(Value,Length(Value)-1,2);
			RemovePointAndComma(Value);
			Value:=Trim(Value);
			Val(Value,Result,Pos);
			if Pos>0 then Result:=-1;
		end else begin
			if System.Copy(Value,Length(Value)-3,4)='BYTE' then begin
				System.Delete(Value,Length(Value)-3,4);
				RemovePointAndComma(Value);
				Value:=Trim(Value);
				Val(Value,Result,Pos);
				if Pos>0 then Result:=-1;
			end else begin
				if System.Copy(Value,Length(Value)-4,5)='BYTES' then begin
					System.Delete(Value,Length(Value)-4,5);
					RemovePointAndComma(Value);
					Value:=Trim(Value);
					Val(Value,Result,Pos);
					if Pos>0 then Result:=-1;
				end;
			end;
		end;
	end;

	//add in v 0.85
	function ValidFileTime(FileTime: TFileTime): Boolean;

	begin
		Result:=(FileTime.dwLowDateTime<>0) or (FileTime.dwHighDateTime<>0);
	end;

	//add in v0.94
	function IfItemHasNotSize(Attr:LongWord;Item:PItemIDList;AType:String):Boolean;

	begin
		Result:=BOOL(Attr and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER));
//remove in v0.96
//		if Result then Result:=not (AType=TextZipFile);
		if not Result then Result:=SHIsEqualPIDL(Item,PIDL_RecycleBin);
	end;
	//end add in v0.94

begin
	if SI=nil then Exit;
	if SI.IconLoad then Exit;
	SI.IconLoad:=True;
	with SI^ do begin
		ImageIndex:=GetFileInfo(SI,FileInfo,Attribute);
		SubItemText.Clear;
		if BOOL(Attribute and SFGAO_SHARE) then
			OverlayIndex:=0
		else
			if BOOL(Attribute and SFGAO_LINK) then OverlayIndex:=1 else OverlayIndex:=-1;
		IsCut:=BOOL(Attribute and SFGAO_GHOSTED);
	end;
	case TypeDisplay of
		tdFile	:	with SI^,FileInfo,FileData do begin
			//change in v0.94
			FileData:=GetWin32FindData(NameSpace.ShellFolder,ID);
			if IfItemHasNotSize(Attribute,ID,szTypeName) then
				SubItemText.Add('')
			else begin
				LUI.LowPart:=nFileSizeLow;
				LUI.HighPart:=nFileSizeHigh;
				TotalSize:=LUI.QuadPart;
				SubItemText.Add(FormatSize(TotalSize));
			end;
			if BOOL(dwAttributes and SFGAO_FILESYSTEM) then
				SubItemText.Add(szTypeName)
			else
				if BOOL(dwAttributes and SFGAO_HASSUBFOLDER) then
					SubItemText.Add(rsSystemFolder)
				else
					SubItemText.Add(szTypeName);
			//change in v0.85
			DTStr:='';
			if ValidFileTime(ftLastWriteTime)
				 and FileTimeToLocalFileTime(ftLastWriteTime, LocalFileTime)
				 and FileTimeToSystemTime(LocalFileTime, SysTime) then
      try
				 ModDate:=SystemTimeToDateTime(SysTime);
				 if (AnsiUpperCase(DateTimeFormat)=dtfShortWindows) or
					 (DateTimeFormat='') then
					 DTStr:=DateTimeToStr(ModDate)
				 else
					if (AnsiUpperCase(DateTimeFormat)=dtfLongWindows) then
						DTStr:=FormatDateTime(LongDateFormat+' '+LongTimeFormat,ModDate)
					else
						DTStr:=FormatDateTime(DateTimeFormat,ModDate);
			except
			end;
			SubItemText.Add(DTStr);
			//end change in v0.85
			//end change in v0.94
			Attrs:='';
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_READONLY) then Attrs:=Attrs+'R';
			//change in v0.97
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) then begin
				Attrs:=Attrs+'H';
				if IsZipFile then begin
					Attribute:=Attribute or SFGAO_GHOSTED;
					IsCut:=True;
				end;
			end else
				if IsZipFile then begin
					Attribute:=Attribute and LONGWORD(not SFGAO_GHOSTED);
					IsCut:=False;
				end;
			//end change in v0.97
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) then Attrs:=Attrs+'S';
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) then Attrs:=Attrs+'A';
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_TEMPORARY) then Attrs:=Attrs+'T';
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED) then Attrs:=Attrs+'C';
			if Bool(dwFileAttributes and FILE_ATTRIBUTE_OFFLINE) then Attrs:=Attrs+'L';
			SubItemText.Add(Attrs);
		end;
		tdMyComputer	:	with SI^,FileInfo do begin
			Attrs:=SHFileNameFromPIDL(ID);
			DT:=GetDriveType(PChar(Attrs));
			case DT of
				DRIVE_REMOVABLE	: SubItemText.Add(rsDRIVE_REMOVABLE);
				DRIVE_FIXED			: SubItemText.Add(rsDRIVE_FIXED);
				DRIVE_REMOTE		: SubItemText.Add(rsDRIVE_REMOTE);
				DRIVE_CDROM			: SubItemText.Add(rsDRIVE_CDROM);
				DRIVE_RAMDISK		: SubItemText.Add(rsDRIVE_RAMDISK);
			else
				SubItemText.Add(szTypeName);
			end;
			if (DT>DRIVE_REMOVABLE) and GetDiskFreeSpaceEx(PChar(Attrs),FreeAvailable,TotalSize,nil) then begin
				SubItemText.Add(FormatSize(TotalSize));
				SubItemText.Add(FormatSize(FreeAvailable));
			end else begin
				SubItemText.Add('');
				SubItemText.Add('');
			end;
		end;
		tdSpecialFolder : with SI^,FileInfo,FileData do begin
			FileData:=GetWin32FindData(NameSpace.ShellFolder,ID);
			if NameSpace.ShellFolder2<>nil then begin
				DT:=1;
				while DT<Columns.Count do begin
					FillChar(TSD,Sizeof(TSD),0);
					if NameSpace.ShellFolder2.GetDetailsOf(SHLastPIDL(ID),DT,TSD)=S_OK then begin
						Attrs:=StringFromStrRet(SHLastPIDL(ID),TSD.Str);
						SubItemText.Add(Attrs);
						if IsSize(UpperCase(Attrs))>-1 then begin
							LUI.LowPart:=nFileSizeLow;
							LUI.HighPart:=nFileSizeHigh;
							TotalSize:=LUI.QuadPart;
							if BOOL(dwAttributes and SFGAO_HASSUBFOLDER) or SHIsEqualPIDL(ID,PIDL_RecycleBin) then
								Attrs:=''
							else
								Attrs:=FormatSize(TotalSize);
							SubItemText.Objects[Pred(SubItemText.Count)]:=TObject(1);
							SubItemText[Pred(SubItemText.Count)]:=Attrs;
							Columns[DT].Alignment:=taRightJustify;
						end;
					end else
						SubItemText.Add('');
					Inc(DT);
				end;
			end else
				if NameSpace.ShellDetails<>nil then begin
					DT:=1;
					while DT<Columns.Count do begin
						FillChar(TSD,Sizeof(TSD),0);
						if NameSpace.ShellDetails.GetDetailsOf(SHLastPIDL(ID),DT,TSD)=S_OK then begin
							Attrs:=StringFromStrRet(SHLastPIDL(ID),TSD.Str);
							SubItemText.Add(Attrs);
							if IsSize(UpperCase(Attrs))>-1 then begin
								LUI.LowPart:=nFileSizeLow;
								LUI.HighPart:=nFileSizeHigh;
								TotalSize:=LUI.QuadPart;
								if BOOL(dwAttributes and SFGAO_HASSUBFOLDER) or SHIsEqualPIDL(ID,PIDL_RecycleBin) then
									Attrs:=''
								else
									Attrs:=FormatSize(TotalSize);
								SubItemText.Objects[Pred(SubItemText.Count)]:=TObject(1);
								SubItemText[Pred(SubItemText.Count)]:=Attrs;
								Columns[DT].Alignment:=taRightJustify;
							end;
						end else
							SubItemText.Add('');
						Inc(DT);
					end;
				end else begin
					Count:=ListView_GetItemCount(ShellBrowser.ListHandle);
					if Count=0 then Exit;
					if NameSpaceText='' then NameSpaceText:=DisplayNameItem(SI);
					P:=AllocMem(cpSize);
					try
						DT:=0;
						while DT<Count do begin
							FillChar(LVItem,Sizeof(LVItem),0);
							LVItem.Mask:=LVIF_TEXT;
							LVItem.pszText:=P;
							LVItem.cchTextMax:=cpSize;
							LVItem.iSubItem:=0;
							LVItem.iItem:=DT;
							ListView_GetItem(ShellBrowser.ListHandle,LVItem);
							if AnsiCompareText(NameSpaceText,StrPas(LVItem.pszText))=0 then Break;
							Inc(DT);
						end;
						if DT=Count then Exit;
						Count:=1;
						while Count<Columns.Count do begin
							FillChar(LVItem,Sizeof(LVItem),0);
							LVItem.Mask:=LVIF_TEXT;
							LVItem.pszText:=P;
							LVItem.cchTextMax:=cpSize;
							LVItem.iSubItem:=Count;
							LVItem.iItem:=DT;
							ListView_GetItem(ShellBrowser.ListHandle,LVItem);
							Attrs:=StrPas(LVItem.pszText);
							if Attrs='' then SubItemText.Add('') else begin
								SubItemText.Add(Attrs);
								if IsSize(UpperCase(StrPas(LVItem.pszText)))>-1 then begin
									LUI.LowPart:=nFileSizeLow;
									LUI.HighPart:=nFileSizeHigh;
									TotalSize:=LUI.QuadPart;
									if BOOL(dwAttributes and SFGAO_HASSUBFOLDER) or SHIsEqualPIDL(ID,PIDL_RecycleBin) then
										Attrs:=''
									else
										Attrs:=FormatSize(TotalSize);
									SubItemText.Objects[Pred(SubItemText.Count)]:=TObject(1);
									SubItemText[Pred(SubItemText.Count)]:=Attrs;
									Columns[Count].Alignment:=taRightJustify;
							end;
							end;
							Inc(Count);
						end;
					finally
						FreeMem(P);
					end;
				end;
		end;
	end;
end;

function TNameSpaceList.GetViewStyle: TViewStyle;

begin
	Result:=inherited ViewStyle;
end;

procedure TNameSpaceList.NewSetViewStyle(const Value: TViewStyle);

var
	Flag:Boolean;

begin
	Flag:=inherited ViewStyle<>vsReport;
	inherited ViewStyle:=Value;
	if HandleAllocated then begin
		if Flag then ClearIconLoad;
		Arrange(arAlignTop);
		SetColumnBitmap;
	end;
end;

procedure TNameSpaceList.UpdateItem(SI: PNameSpaceItem);

begin
	SI.Attribute:=GetAttributes(SI);
	SI.IconLoad:=False;
	UpdateData(SI);
	Invalidate;
end;

procedure TNameSpaceList.RepopulateDisplayName;

var
	i:Integer;
	scr:TCursor;

begin
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		i:=0;
		while i<NameSpace.SubNameSpaces.Count do begin
			PNameSpaceItem(NameSpace.SubNameSpaces[i]).NameSpaceText:=DisplayNameItem(NameSpace.SubNameSpaces[i]);
			Inc(i);
		end;
	finally
		Invalidate;
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceList.CreateColumnBitmap;

begin
	AscendingBitmap.Free;
	DescendingBitmap.Free;
	AscendingBitmap:=TBitmap.Create;
	with AscendingBitmap do begin
		Canvas.Brush.Color:=clSilver;
		Width:=16;Height:=16;
		Canvas.FillRect(Rect(0,0,15,15));
		Canvas.Pen.Color:=clWhite;
		Canvas.MoveTo(4,12);Canvas.LineTo(11,12);
		Canvas.Pixels[3,11]:=clBlack; Canvas.Pixels[11,11]:=clWhite;
		Canvas.Pixels[3,10]:=clBlack; Canvas.Pixels[11,10]:=clWhite;

		Canvas.Pixels[4,9]:=clBlack; Canvas.Pixels[10,9]:=clWhite;
		Canvas.Pixels[4,8]:=clBlack; Canvas.Pixels[10,8]:=clWhite;

		Canvas.Pixels[5,7]:=clBlack; Canvas.Pixels[9,7]:=clWhite;
		Canvas.Pixels[5,6]:=clBlack; Canvas.Pixels[9,6]:=clWhite;

		Canvas.Pixels[6,5]:=clBlack; Canvas.Pixels[8,5]:=clWhite;
		Canvas.Pixels[7,4]:=clWhite;

		Canvas.Brush.Color:=clLime;
		Canvas.FloodFill(4,11,Canvas.Pixels[4,11],fsSurFace);
		Canvas.Pixels[7,4]:=clGray;
		Canvas.Brush.Color:=clBtnFace;
		Canvas.FloodFill(0,0,Canvas.Pixels[0,0],fsSurFace);
		Transparent:=True;
		TransparentColor:=Canvas.Pixels[0,0];
	end;
	DescendingBitmap:=TBitmap.Create;
	with DescendingBitmap do begin

		Canvas.Brush.Color:=clSilver;
		Width:=16;Height:=16;
		Canvas.FillRect(Rect(0,0,15,15));
		Canvas.Pen.Color:=clBlack;
		Canvas.MoveTo(4,4);Canvas.LineTo(11,4);
		Canvas.Pixels[3,5]:=clBlack; Canvas.Pixels[11,5]:=clWhite;
		Canvas.Pixels[3,6]:=clBlack; Canvas.Pixels[11,6]:=clWhite;

		Canvas.Pixels[4,7]:=clBlack; Canvas.Pixels[10,7]:=clWhite;
		Canvas.Pixels[4,8]:=clBlack; Canvas.Pixels[10,8]:=clWhite;

		Canvas.Pixels[5,9]:=clBlack; Canvas.Pixels[9,9]:=clWhite;
		Canvas.Pixels[5,10]:=clBlack; Canvas.Pixels[9,10]:=clWhite;

		Canvas.Pixels[6,11]:=clBlack; Canvas.Pixels[8,11]:=clWhite;
		Canvas.Pixels[7,12]:=clWhite;

		Canvas.Brush.Color:=clLime;
		Canvas.FloodFill(4,5,Canvas.Pixels[4,5],fsSurFace);
		Canvas.Brush.Color:=clBtnFace;
		Canvas.FloodFill(0,0,Canvas.Pixels[0,0],fsSurFace);
		Transparent:=True;
		TransparentColor:=Canvas.Pixels[0,0];
	end;
end;

procedure TNameSpaceList.SetColumnBitmap;

var
	i : Integer;
	HdItem : THdItem;

begin
	if (AscendingBitmap=nil) or (DescendingBitmap=nil) then Exit;
	if HandleAllocated and ShowColumnHeaders then begin
		if SortOrder=soAscending then HdItem.hbm:=AscendingBitmap.Handle else HdItem.hbm:=DescendingBitmap.Handle;
		i:=0;
		while i<Columns.Count do begin
			HdItem.Mask:=HDI_FORMAT;
			Header_GetItem(GetDlgItem(Handle,0),i,HdItem);
			HdItem.Mask:=HDI_BITMAP or HDI_FORMAT;
			if (SortByColumn=i) then begin
				HdItem.fmt:=HdItem.fmt or HDF_BITMAP;
				if Columns[i].Alignment=taLeftJustify then HdItem.fmt:=HdItem.fmt or HDF_BITMAP_ON_RIGHT;
			end else
				HdItem.fmt:=HdItem.fmt and (not HDF_BITMAP);
			Header_SetItem(GetDlgItem(Handle,0),i,HDItem);
			Inc(i);
		end;
	end;
end;

function TNameSpaceList.CreatePIDLList(Item:PNameSpaceItem): TArrayPIDL;

var
	Data:TListItem;
	Count:LongWord;

begin
	if Item=NameSpace then begin
		SetLength(Result,1);
		Result[0]:=SHCloneLastPIDL(Item.ID);
	end else begin
		Data:=Selected;Count:=0;
		while Data<>nil do begin
			Inc(Count);
			SetLength(Result,Count);
			Result[Pred(Count)]:=SHCloneLastPIDL(PNameSpaceItem(Data.Data).ID);
			Data:=GetNextItem(Data,sdAll,[isSelected]);
		end;
	end;
end;


procedure TNameSpaceList.ExecuteDefaultCommand;

var
	PIDL: TArrayPIDL;
	i:LongWord;
	Item:PNameSPaceItem;

begin
	if Selected<>nil then Item:=Selected.Data else Item:=nil;
	if Item=nil then Item:=NameSpace;
	with Item^ do if Parent<>nil then begin
		if SHIsEqualPIDL(ID, PIDL_ControlPanel) or SHIsEqualPIDL(ID, PIDL_Printers) or
			 SHIsEqualPIDL(ID, PIDL_DialUpNetworking) or SHIsEqualPIDL(ID, PIDL_SheduledTasks) then
			CurrentFolderFromID(ID)
		else begin
			if not RespondOnChangeNotify then LockRefresh;
			PIDL:=CreatePIDLList(Item);
			try
				ContextPopUp.ShellExecuteDefault(Item.Parent.ShellFolder,PIDL);
			finally
				for i:=0 to Pred(Length(PIDL)) do SHFree(PIDL[i]);
				Finalize(PIDL);
				if not RespondOnChangeNotify then UnLockRefresh;
			end;
		end;
	end;
end;

function TNameSpaceList.ExecuteCommand(ACommand: String):Boolean;

var
	PIDL: TArrayPIDL;
	i:LongWord;
	Item:PNameSPaceItem;

begin
	Result:=False;
	if Selected<>nil then Item:=Selected.Data else Item:=nil;
	if Item=nil then Item:=NameSpace;
	with Item^ do if Parent<>nil then begin
		if not RespondOnChangeNotify then LockRefresh;
		PIDL:=CreatePIDLList(Item);
		try
			Result:=ContextPopUp.ShellExecuteCommand(Item.Parent.ShellFolder,PIDL,ACommand);
		finally
			for i:=0 to Pred(Length(PIDL)) do SHFree(PIDL[i]);
			Finalize(PIDL);
			if not RespondOnChangeNotify then UnLockRefresh;
		end;
	end;
end;

procedure TNameSpaceList.RefreshItems;

var
	PIDL:TArrayPIDL;
	Data:TListItem;
	Count:LongWord;

begin
	LockWindowUpdate(Handle);
	Data:=Selected;Count:=0;
	while Data<>nil do begin
		Inc(Count);
		SetLength(PIDL,Count);
		PIDL[Pred(Count)]:=SHClonePIDL(PNameSpaceItem(Data.Data).ID);
		Data:=GetNextItem(Data,sdAll,[isSelected]);
	end;
	AddListToItems;
	try
		while Count>0 do begin
			Dec(Count);
			Data:=LocalFindListItem(PIDL[Count]);
			if Data<>nil then Selected:=Data;
			SHFree(PIDL[Count]);
		end;
	finally
		Finalize(PIDL);
		LockWindowUpdate(0);
	end;
end;

procedure TNameSpaceList.LockRefresh;

begin
	Inc(FLockRefresh);
end;

procedure TNameSpaceList.UnLockRefresh;

begin
	Dec(FLockRefresh);
	if FLockRefresh=0 then RefreshItems;
end;

procedure TNameSpaceList.ClearListColumns;

begin
	Columns.BeginUpdate;
	try
		Columns.Clear;
	finally
		Columns.EndUpdate;
	end;
end;

procedure TNameSpaceList.RefreshFolder;

var
	scr:TCursor;

begin
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		AddListToItems;
	finally
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceList.ClearIconLoad;

var
	i:Integer;

begin
	i:=0;
	with NameSpace^ do while i<SubNameSpaces.Count do begin
		PNameSpaceItem(SubNameSpaces[i]).IconLoad:=False;
		Inc(i);
	end;
end;

{$IFDEF DELPHI5_UP}
procedure TNameSpaceList.WMContextMenu(var Message: TWMContextMenu);

//change in v 0.89

var
	PIDL: TArrayPIDL;
	i:LongWord;
	Item:PNameSPaceItem;
	SavePopUpMenu:TPopupMenu;

begin
	if not RespondOnChangeNotify then LockRefresh;
	UpdateBackgroundMenu;
	SavePopUpMenu:=PopUpMenu;
	if (Selected=nil) and (SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop)) then try
	PopUpMenu:=BackgroundPopUpMenu;
		if TypePopUpMenu in [tcmOnlyContextMenu, tcmConcatMenu] then
			ContextPopUp.CreatePopUpMenu(SF_DesktopFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (BackgroundPopUpMenu<>nil),BackgroundPopUpMenu)
		else
			inherited;
	finally
		PopUpMenu:=SavePopUpMenu;
	end else
		case TypePopUpMenu of
			tcmOnlyUserMenu : inherited;
			tcmOnlyContextMenu, tcmConcatMenu 	: begin
				if Selected<>nil then Item:=Selected.Data else Item:=nil;
				if Item=nil then Item:=NameSpace;
				with Item^ do if Parent<>nil then begin
					PIDL:=CreatePIDLList(Item);
					try
						if Item<>NameSpace then
							ContextPopUp.CreatePopUpMenu(Item.Parent.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (PopUpMenu<>nil))
						else
							ContextPopUp.CreatePopUpMenu(Item.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (BackgroundPopUpMenu<>nil),BackgroundPopUpMenu);
					finally
						for i:=0 to Pred(Length(PIDL)) do SHFree(PIDL[i]);
						Finalize(PIDL);
					end;
				end;
			end;
		end;
	if not RespondOnChangeNotify then UnLockRefresh;
end;

{$ELSE}

procedure TNameSpaceList.WMRButtonUp(var Message: TWMRButtonUp);

//change in v 0.89

var
	PIDL: TArrayPIDL;
	i:LongWord;
	Item:PNameSPaceItem;	
	SavePopUpMenu:TPopupMenu;

begin
	if not RespondOnChangeNotify then LockRefresh;
	UpdateBackgroundMenu;	
	SavePopUpMenu:=PopUpMenu;
	if (Selected=nil) and (SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop)) then try
		PopUpMenu:=BackgroundPopUpMenu;
		if TypePopUpMenu in [tcmOnlyContextMenu, tcmConcatMenu] then
			ContextPopUp.CreatePopUpMenu(SF_DesktopFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (BackgroundPopUpMenu<>nil),BackgroundPopUpMenu)
		else
			inherited;
	finally
		PopUpMenu:=SavePopUpMenu;
	end else
		case TypePopUpMenu of
			tcmOnlyUserMenu : inherited;
			tcmOnlyContextMenu, tcmConcatMenu 	: begin
				if Selected<>nil then Item:=Selected.Data else Item:=nil;
				if Item=nil then Item:=NameSpace;
				with Item^ do if Parent<>nil then begin
					PIDL:=CreatePIDLList(Item);
					try
						if Item<>NameSpace then
							ContextPopUp.CreatePopUpMenu(Item.Parent.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (PopUpMenu<>nil))
						else
							ContextPopUp.CreatePopUpMenu(Item.ShellFolder,PIDL,(TypePopUpMenu=tcmConcatMenu) and (BackgroundPopUpMenu<>nil),BackgroundPopUpMenu);
					finally
						for i:=0 to Pred(Length(PIDL)) do SHFree(PIDL[i]);
						Finalize(PIDL);
					end;
				end;
			end;
		end;
	if not RespondOnChangeNotify then UnLockRefresh;
end;
{$ENDIF}

procedure TNameSpaceList.TestAndExecuteDefault;

	function GetAttributesHelp(Item:PNameSpaceItem):LongWord;

	var
		ID,Parent:PItemIDList;

	begin
		Result:=GetAttributes(Item);
		if BOOL(Result and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER)) then Exit;
		Parent:=SHParentPIDL(Item.ID);
		try
			if SHIsEqualPIDL(Parent,PIDL_Desktop) then begin
				ID:=SHLastPIDL(Item.ID);
				Result:=SFGAO_ALL;
				SF_DesktopFolder.GetAttributesOf(1,ID,Result);
			end;
		finally
			SHFree(Parent);
		end;
	end;

	function TestShortCut(Item:PNameSpaceItem):Boolean;

	var	MyObject	: IUnknown;
			MySLink   : IShellLink;
			MyPFile   : IPersistFile;
			Name		  : WideString;
			ID				: PItemIDList;
			Help			: PNameSpaceItem;

	begin
		Result:=False;
		ID:=nil;
		if not BOOL(GetAttributesHelp(Item) and SFGAO_LINK) then Exit;
		MyObject:=CreateComObject(CLSID_ShellLink);
		MySLink:=MyObject as IShellLink;
		MyPFile:=MyObject as IPersistFile;
		try
			Name:=SHFileNameFromPIDLWideChar(Item.ID);
			MyPFile.Load(PWideChar(Name),STGM_READ);
			MySLink.GetIDList(ID);
			if ID=nil then Exit;
      //change in v 0.85      
			Help:=MakeNameSpaceItemWithParent(ID,TClass(Self));
			try
				if BOOL(GetAttributesHelp(Help) and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER)) or SHIsEqualPIDL(PIDL_RecycleBin,ID) then begin
					Result:=True;
					CurrentFolderFromID(Help.ID);
				end;
			finally
				SHFree(ID);
				DisposeNameSpaceItemAllParent(Help);
			end;
		finally
			MyPFile:=nil;
			MySLink:=nil;
			MyObject:=nil;
		end;
	end;

begin
	if Selected=nil then Exit;
	with PNameSpaceItem(Selected.Data)^ do
		if not TestShortCut(Selected.Data) then
			if BOOL(GetAttributesHelp(Selected.Data) and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER)) or SHIsEqualPIDL(PIDL_RecycleBin,ID) then
				CurrentFolderFromID(ID)
			else
				ExecuteDefaultCommand;
end;

procedure TNameSpaceList.NewSelectAll;

var
	i:Integer;
	scr:TCursor;

begin
	Selected:=nil;
	i:=0;
	scr:=Screen.Cursor;
	Screen.Cursor:=crHourglass;
	try
		while i<Items.Count do begin Items[i].Selected := True;Inc(i); end;
	finally
		Screen.Cursor:=scr;
	end;
end;

procedure TNameSpaceList.GetDetailsInterface;

const
	CLSID_MyDocuments			=	'{450D8FBA-AD25-11D0-98A8-0800361B1103}';
	CLSID_Favorites				=	'{1A9BA3A0-143A-11CF-8350-444553540000}';
	CLSID_NULL						=	'{00000000-0000-0000-0000-000000000000}';
  //add in v0.96
  CLSID_Folder_Shortcut	= '{0AFACED1-E828-11D1-9187-B532F1E9575D}';

var
	TSD:TSHELLDETAILS;
	Result:Boolean;

	function	TestMyDocuments:Boolean;

	var
		SHDID:TSHDescriptionID;
		P:PWideChar;

	begin
		Result:=False;
		if NameSpace.Parent=nil then Exit;
		SHDID:=GetDescription(NameSpace.Parent.ShellFolder,NameSpace.ID);
		StringFromCLSID(SHDID.Id,P);
		Result:=P=CLSID_MyDocuments;
	end;

	function	TestFavorites:Boolean;

	var
		SHDID:TSHDescriptionID;
		P:PWideChar;
		NS:PNameSpaceItem;

	begin
		Result:=False;
		NS:=NameSpace;
		while (not Result) and (NS<>nil) and (NS.Parent<>nil) do begin
			SHDID:=GetDescription(NS.Parent.ShellFolder,NS.ID);
			StringFromCLSID(SHDID.Id,P);
			Result:=(P=CLSID_Favorites);
			if not Result then NS:=NS.Parent;
		end;
	end;

	function	TestNoCLSID:Boolean;

	var
		SHDID:TSHDescriptionID;
		P:PWideChar;

	begin
  	//change in 0.96
		Result:=BOOL(GetAttributes(NameSpace) and SFGAO_FILESYSTEM);
		if Result and (NameSpace.Parent<>nil) then begin
			SHDID:=GetDescription(NameSpace.Parent.ShellFolder,NameSpace.ID);
			StringFromCLSID(SHDID.Id,P);
			Result:=(P=CLSID_NULL) or (P=CLSID_Folder_Shortcut);
		end;
	end;

begin
	Result:=SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) or
					SHIsEqualPIDL(NameSpace.ID,PIDL_MyComputer) or
					SHIsEqualPIDL(NameSpace.ID,PIDL_Internet) or
					TestMyDocuments or
					(
						SHIsParentPIDL(PIDL_MyComputer,NameSpace.ID,True) and
						(SHFileNameFromPIDL(NameSpace.ID)<>'')
					);
	if not Result then begin
		if TestNoCLSID then Result:=True;
		if Result and TestFavorites then Result:=False;
	end;
	if not Result then begin
		FillChar(TSD,Sizeof(TSD),0);
		NameSpace.ShellFolder.QueryInterface(IShellFolder2,NameSpace.ShellFolder2);
		if NameSpace.ShellFolder2<>nil then
			if NameSpace.ShellFolder2.GetDetailsOf(nil,0,TSD)<>S_OK then NameSpace.ShellFolder2:=nil;
		if NameSpace.ShellFolder2=nil then
			NameSpace.ShellFolder.CreateViewObject(Handle,IShellDetails,Pointer(NameSpace.ShellDetails));
		NameSpace.IsSpecialFolder:=True;
	end;
end;

procedure TNameSpaceList.MakeSpecialColumns;

var
	NewColumn:TListColumn;
	i,Count:Integer;
	Result:HResult;
	TM:TTEXTMETRIC;
	TSD:TSHELLDETAILS;
	HdItem:THdItem;
	P:PChar;

begin
	Columns.BeginUpdate;
	FillChar(TM,Sizeof(TM),0);
	GetTextMetrics(Canvas.Handle,TM);
	with NameSpace^ do try
		Columns.Clear;
		if ShellFolder2<>nil then begin
			Count:=0;
			repeat
				FillChar(TSD,Sizeof(TSD),0);
				Result:=ShellFolder2.GetDetailsOf(nil,Count,TSD);
				if Result=S_OK then begin
					NewColumn:=Columns.Add;
					NewColumn.Caption:=StringFromStrRet(nil,TSD.Str);
					NewColumn.AutoSize:=False;
					NewColumn.Width:=TSD.cxChar*TM.tmAveCharWidth;
				end;
				Inc(Count);
			until Result<>S_OK;
		end else
			if ShellDetails<>nil then begin
				Count:=0;
				repeat
					FillChar(TSD,Sizeof(TSD),0);
					Result:=ShellDetails.GetDetailsOf(nil,Count,TSD);
					if Result=S_OK then begin
						NewColumn:=Columns.Add;
						NewColumn.Caption:=StringFromStrRet(nil,TSD.Str);
						NewColumn.AutoSize:=False;
						NewColumn.Width:=TSD.cxChar*TM.tmAveCharWidth;
					end;
					Inc(Count);
				until Result<>S_OK;
			end else begin
				Count:=Header_GetItemCount(ShellBrowser.HeaderHandle);
				i:=0;
				P:=AllocMem(cpSize);
				try
					while i<Count do begin
						FillChar(HdItem,Sizeof(HdItem),0);
						HdItem.Mask:=HDI_FORMAT	or HDI_TEXT	or HDI_WIDTH;
						HdItem.pszText:=P;
						HdItem.cchTextMax:=cpSize;
						Header_GetItem(ShellBrowser.HeaderHandle,i,HdItem);
						NewColumn:=Columns.Add;
						NewColumn.Caption:=StrPas(HdItem.pszText);
						NewColumn.AutoSize:=False;
						NewColumn.Width:=HdItem.cxy;
						if BOOL(HdItem.fmt and HDF_RIGHT) then
							NewColumn.Alignment:=taRightJustify
						else
							NewColumn.Alignment:=taLeftJustify;
						Inc(i);
					end;
				finally
					FreeMem(P);
				end;
			end;
	finally
		Columns.EndUpdate;
	end;
end;

function TNameSpaceList.DisplayNameItem(Item: PNameSpaceItem;	TypeName: TTypeParsingName): String;

begin
	with Item^ do
		if Parent<>nil then
			Result:=GetDisplayName(Parent.ShellFolder,SHLastPIDL(ID),TypeName)
		else
			Result:=GetDisplayName(SF_Desktop,ID,TypeName);
	case TypeDisplayMode of
		tdmLowerCase	:	Result:=NSWideLowerCaseA(Result);
		tdmUpperCase	:	Result:=NSWideUpperCaseA(Result);
		tdmFirstUpper	:	Result:=NSWideFirstUpperA(Result);
	end;
end;

procedure TNameSpaceList.NameSpaceSort;

begin
	if NameSpace.SubNameSpaces.Count=0 then Exit;
	Items.Count:=0;
	try
		IS_PIDL_DialUpNetworking:=SHIsEqualPIDL(NameSpace.ID,PIDL_DialUpNetworking);
		NameSpace.SubNameSpaces.Sort(@ListSortFunc);
	finally
		IS_PIDL_DialUpNetworking:=False;
		Items.Count:=NameSpace.SubNameSpaces.Count;
		SetColumnBitmap;
	end;
end;

function TNameSpaceList.OwnerDataFetch(Item: TListItem;	Request: TItemRequest): Boolean;

begin
	Item.Data:=NameSpace.SubNameSpaces[Item.Index];
	Result:=True;
end;

function TNameSpaceList.OwnerDataFind(Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
	StartIndex: Integer; Direction: TSearchDirection;
	Wrap: Boolean): Integer;

var
	i:Integer;
	Found:Boolean;

begin
	Result:=-1;
	if (Find=ifExactString) or (Find=ifPartialString) then with NameSpace^ do begin
		i:=StartIndex;
		repeat
			if (i=SubNameSpaces.Count) then	if Wrap then i:=0 else Exit;
			with PNameSpaceItem(SubNameSpaces[i])^ do begin
				if NameSpaceText='' then NameSpaceText:=DisplayNameItem(NameSpace.SubNameSpaces[i]);
				Found:=Pos(UpperCase(FindString),UpperCase(NameSpaceText))=1;
			end;
			Inc(i);
		until Found or (i=StartIndex);
		if Found then Result:=Pred(i);
	end;
end;

function TNameSpaceList.CheckFreeSpace(ItemID: PItemIDList): Boolean;

type
	PDWORDITEMID	=	^TDWORDITEMID;
	TDWORDITEMID	=	packed record
		cbSize	:	Word;
		Item1,Item2:DWORD;
	end;

var
	PDWID:PDWORDITEMID;
	ID,Item:PItemIDList;
	i:Integer;

begin
	Result:=True;
	try
	if not SHIsEqualPIDL(NameSpace.ID,PIDL_MyComputer) then Exit;
	PDWID:=PDWORDITEMID(ItemID);
	i:=0;
	while i<32 do begin
		if BOOL((1 shl i) and PDWID.Item1) then begin
			ID:=SHPIDLFromFileName(Format('%s:\',[Char(Ord('A')+i)]));
			Item:=SHCombinePIDL(NameSpace.ID,SHLastPIDL(ID));
			try
				UpdateItemInList(Item);
			finally
				SHFree(Item);
				SHFree(ID);
			end;
		end;
		Inc(i);
	end;
	finally
		Inc(FCountUpdate);
		PostMessage(Self.Handle,WM_UPDATECURRENT,0,0);
	end;
end;

procedure TNameSpaceList.WndProc(var Message: TMessage);

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
			try
				if IsExistChangeNotificationLock then begin
					hLock:=SHChangeNotification_Lock(HWND(WParam),LParam,@IDArray,@wEventID);
					if hLock<>0 then with IDArray^ do begin
						Handled:=HandleChangeNotify(wEventID,Item1,Item2);
						SHChangeNotification_Unlock(hLock);
					end;
				end else
					with PIDArray(Message.WParam)^ do
						Handled:=HandleChangeNotify(Message.LParam,Item1,Item2);
			except
				Application.HandleException(Self);
			end;
		end;
	end;
	if not Handled then inherited;
end;


// ver 0.81
// solution for problem with winzip
procedure TNameSpaceList.UpdateCurrentListOnNotify;

var
	NSNotify:PNameSpaceItem;
	Enum:DWORD;
	SI:PNameSpaceItem;
	i,j:Integer;

begin
	if FCountUpdate>0 then Exit;
	Inc(FCountUpdate);
  //change in v 0.85  
	NSNotify:=MakeNameSpaceItemWithParent(NameSpace.ID,TClass(Self));
	try
		case TypeEnumNames of
			teFolder 		: Enum:=EnumsFolder;
			teNonFolder	: Enum:=EnumsNonFolder;
		else
			Enum:=EnumsAllOneHidden;
		end;
		if ShowHiddenName then Enum:=Enum or EnumsHidden;
		PopulateIDList(NSNotify,Enum);
		if MatchSpec<>'' then begin
			i:=0;
			with NSNotify^ do while i<SubNameSpaces.Count do begin
				if not BOOL(PNameSpaceItem(SubNameSpaces[i]).Attribute and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER)) then
					if TestNotInMatchSpec(SHFileNameFromPIDL(PNameSpaceItem(SubNameSpaces[i]).ID)) then begin
						SI:=SubNameSpaces[i];
						SubNameSpaces.Delete(i);
						DisposeNameSpaceItem(SI);						
						Continue;
					end;
				Inc(i);
			end;
		end;
		i:=0;
		//delete and update
		with NameSpace^ do while i<SubNameSpaces.Count do begin
			j:=0;
			while j<NSNotify.SubNameSpaces.Count do begin
				if SHIsEqualPIDL(PNameSpaceItem(NSNotify.SubNameSpaces[j]).ID,PNameSpaceItem(SubNameSpaces[i]).ID) then Break;
				Inc(j);
			end;
			if j=NSNotify.SubNameSpaces.Count then begin
				SI:=SubNameSpaces[i];
				SubNameSpaces.Delete(i);
				DisposeNameSpaceItem(SI);
			end else begin
				SI:=SubNameSpaces[i];
				//change in v0.97
				SHFree(SI.ID);
				SI.ID:=SHClonePIDL(PNameSpaceItem(NSNotify.SubNameSpaces[j]).ID);
				//end change in v0.97
				SI.Attribute:=GetAttributes(SI);
				SI.IconLoad:=False;
				UpdateData(SI);
				SI.NameSpaceText:='';
				Inc(i);
				SI:=NSNotify.SubNameSpaces[j];
				NSNotify.SubNameSpaces.Delete(j);
				DisposeNameSpaceItem(SI);
			end;
		end;
		i:=0;
		//add
		with NSNotify^ do while i<SubNameSpaces.Count do begin
			SI:=SubNameSpaces[i];
			SI.Parent:=NameSpace;
			SI.ItemIndex:=NameSpace.SubNameSpaces.Add(SI);
			SubNameSpaces.Delete(i);
		end;
		Items.Count:=NameSpace.SubNameSpaces.Count;
		Repaint;
	finally
		DisposeNameSpaceItemAllParent(NSNotify);
		Dec(FCountUpdate);
	end;
end;

procedure TNameSpaceList.WMUPDATECURRENT(var Msg: TMessage);

begin
	Dec(FCountUpdate);
	if FCountUpdate=0 then UpdateCurrentListOnNotify;
end;

// add in v 0.85
procedure TNameSpaceList.SetDateTimeFormat(const Value: String);

begin
	if Value<>DateTimeFormat then begin
		FDateTimeFormat:=Value;
		RefreshItems;
	end;
end;

//add in v 0.87
procedure TNameSpaceList.WMNotify(var Message: TWMNotify);

var
	SaveC:String;
	Pos:Integer;

	function ActionFind(AItem:PNameSpaceListHeader):Boolean;

	begin
		Result:=SaveC=AItem.Caption;
	end;

begin
	inherited;
	if ColumnsShowing then
		with Message.NMHdr^ do
			case code of
				HDN_ENDTRACK: with PHDNotify(Pointer(Message.NMHdr))^, PItem^ do begin
					if (Mask and HDI_WIDTH) <> 0 then begin
						SaveC:=Columns.Items[Item].Caption;
						Pos:=CurrentFolderData.FirstThat(@ActionFind,0);
						if Pos<>-1 then	PNameSpaceListHeader(CurrentFolderData[Pos]).Width:=cxy;
					end;
				end;
			end;
end;

//add in v 0.87
procedure TNameSpaceList.InitFoldersData;

var
	Header:PNameSpaceListHeader;
	Help:TListFoldersData;

begin
	//Data of Columns for Default Folders
	Help:=TListFoldersData.Create(hcFolderColumns);
	ListFoldersData.Add(Help);

	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=150;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Name';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=60;
	Header.Alignment:=taRightJustify;
	Header.Caption:='Size';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=120;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Type';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=105;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Modified';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=60;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Attributes';
	Help.Add(Header);

	//Data of Columns for My Computer
	Help:=TListFoldersData.Create(hcMyCompColumns);
	ListFoldersData.Add(Help);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=150;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Name';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=140;
	Header.Alignment:=taLeftJustify;
	Header.Caption:='Type';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=120;
	Header.Alignment:=taRightJustify;
	Header.Caption:='Total Size';
	Help.Add(Header);
	Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
	Header.Width:=120;
	Header.Alignment:=taRightJustify;
	Header.Caption:='Free Space';
	Help.Add(Header);
end;

//add in v 0.87
procedure TNameSpaceList.MakeSpecialColumnsWithTest;

var
	NewColumn:TListColumn;
	SaveC:String;
	Pos:Integer;
	Header:PNameSpaceListHeader;

	function ActionFind(AItem:TListFoldersData):Boolean;

	begin
		Result:=SaveC=AItem.Folder;
	end;

	procedure	Action(AItem:PNameSpaceListHeader);

	begin
		NewColumn:=Columns.Add;
		NewColumn.Caption:=AItem.Caption;
		NewColumn.AutoSize:=False;
		NewColumn.Alignment:=AItem.Alignment;
		NewColumn.Width:=AItem.Width;
	end;

begin
	SaveC:=CurrentFolder;
	Pos:=ListFoldersData.FirstThat(@ActionFind,0);
	if Pos=-1 then begin
		MakeSpecialColumns;
		Pos:=0;
		CurrentFolderData:=TListFoldersData.Create(CurrentFolder);
		ListFoldersData.Add(CurrentFolderData);
		while Pos<Columns.Count do begin
			Header:=RecordAlloc(Sizeof(TNameSpaceListHeader));
			Header.Width:=Columns[Pos].Width;
			Header.Alignment:=Columns[Pos].Alignment;
			Header.Caption:=Columns[Pos].Caption;
			CurrentFolderData.Add(Header);
			Inc(Pos);
		end;
	end else begin
		CurrentFolderData:=TListFoldersData(ListFoldersData[Pos]);
		Columns.BeginUpdate;
		try
			Columns.Clear;
			CurrentFolderData.ForEach(@Action,0);
		finally
			Columns.EndUpdate;
		end;
	end;
end;

{
Format of Stream Data
Name Field							Size

Numbers_Folders						4	(Integer)

:Start structure Folders (repeat numbers_of_Folders times)

	Name Field								Size
	Length_of_Name_Folders		4 (Integer)
	Text_of_Name_Folders			Length_of_Name_Folders
	Numbers_of_Columns				4 (Integer)

	:Start of structure Columns (repeat numbers_of_Columns times)
		Name Field						Size
		Size_of_Column				4 (LongWord)
		Data_of_Column				Size_of_Column
			Fields in record TNameSpaceListHeader except Size (Size is Size_of_Column)
	:End structure Columns

:End structure Folders
}

//add in v 0.87
procedure TNameSpaceList.LoadListFoldersData(AStream: TStream);

var
	Count,i,Size,j,CountColumn:Integer;
	Help:TListFoldersData;
	AName:PChar;
	AItem,ARead:PNameSpaceListHeader;

begin
	if AStream=nil then Exit;
	ListFoldersData.Clear;
	AStream.Read(Count,Sizeof(Count));
	if Count>0 then begin
		i:=0;
		while i<Count do begin
			AStream.Read(Size,Sizeof(Size));
			if Size>0 then begin
				AName:=AllocMem(Size);
				try
					AStream.Read(AName^,Size);
					Help:=TListFoldersData.Create(AName);
				finally
					FreeMem(AName);
				end;
			end else Help:=TListFoldersData.Create('');
			ListFoldersData.Add(Help);
			AStream.Read(CountColumn,Sizeof(CountColumn));
			j:=0;
			while j<CountColumn do begin
				AItem:=RecordAlloc(Sizeof(TNameSpaceListHeader));
				AStream.Read(Size,Sizeof(Size));
				ARead:=RecordAlloc(Size);
				try
					AStream.Read(IncPtr(ARead,Sizeof(Size))^,Size-Sizeof(Size));
					RecordMove(ARead,AItem);
				finally
					FreeMem(ARead);
				end;
				Help.Add(AItem);
				Inc(j);
			end;
			Inc(i);
		end;
	end else
		InitFoldersData;
	Application.ProcessMessages;
	MakeColumns;
end;

//add in v 0.87
procedure TNameSpaceList.SaveListFoldersData(AStream: TStream);

var
	AName:PChar;

	procedure	ActionSave(AItem:TListFoldersData);

	var
		i,Size:Integer;

	begin
		Size:=Succ(Length(AItem.Folder));
		AStream.Write(Size,SizeOf(Size));
		if Size>0 then begin
			AName:=AllocMem(Size);
			try
				AName:=StrPCopy(AName,AItem.Folder);
				AStream.Write(AName^,Size);
			finally
				FreeMem(AName);
			end;
		end;
		AStream.Write(AItem.Count,Sizeof(AItem.Count));
		i:=0;
		while i<AItem.Count do begin
			AStream.Write(AItem[i]^,Sizeof(TNameSpaceListHeader));
			Inc(i);
		end;
	end;

begin
	if AStream=nil then Exit;
	AStream.Write(ListFoldersData.Count,SizeOf(ListFoldersData.Count));
	ListFoldersData.ForEach(@ActionSave,0);
end;

function TNameSpaceList.CurrentName(AType: TTypeParsingName): String;

begin
	if Selected<>nil then Result:=DisplayName(Selected,AType) else Result:='';
end;

procedure TNameSpaceList.SetBackGroundPopupMenu(const Value: TPopupMenu);

begin
	FBackGroundPopUpMenu:=Value;
	if Value<>nil then begin
		Value.ParentBiDiModeChanged(Self);
		Value.FreeNotification(Self);
	end;
end;

//add in v 0.89
procedure TNameSpaceList.Notification(AComponent: TComponent; Operation: TOperation);

begin
	inherited Notification(AComponent, Operation);
	if Operation=opRemove then
		if AComponent=FBackGroundPopupMenu then BackGroundPopupMenu:=nil;
end;

procedure TNameSpaceList.WMADDNEWITEM(var Msg: TMessage);

var
	Item:TListItem;

begin
	if Msg.LParam=1 then FAddNewSaveItem:=nil;
	if FAddNewSaveItem<>nil then begin
		Item:=LocalFindListItem(FAddNewSaveItem.ID);
		if Item<>nil then begin Selected:=Item;Selected.EditCaption end;
	end;
	if Msg.LParam=0 then FAddNewSaveItem:=nil;
end;

//add in v 0.89
procedure TNameSpaceList.UpdateBackgroundMenu;

var
	Help:TPopUpMenu;

begin
	if Assigned(FOnUpdateBackGroundMenu) then begin
		Help:=FBackGroundPopupMenu;
		OnUpdateBackGroundMenu(Self,Help);
		BackGroundPopupMenu:=Help;
	end;
end;

procedure TNameSpaceList.SetCurrentToParent;

var
	Parent:PItemIDList;

begin
	if SHIsEqualPIDL(NameSpace.ID,PIDL_Desktop) then Exit;
	Parent:=SHParentPIDL(NameSpace.ID);
	try
		CurrentFolderFromID(Parent);
	finally
		SHFree(Parent);
	end;
end;

//add in v0.95
procedure TNameSpaceList.WMDestroy(var Message: TWMDestroy);

begin
	if (ActiveDragAndDrop) and (FDropTargetRoot<>nil) then FDropTargetRoot.RevokeDragDrop;
	inherited;
end;

procedure TNameSpaceList.SetNumberOfDecimalInSize(const Value: TTypeNumberOfDecimal);

begin
	if NumberOfDecimalInSize<>Value then begin
		FNumberOfDecimalInSize := Value;
		if NameSpace=nil then Exit;
		ClearIconLoad;
		if ViewStyle=vsReport then Invalidate;
	end;
end;

end.
