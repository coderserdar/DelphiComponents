//
// The original Delphi code is : NameSpaceDetails.pas released 12.12.2002
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
// This unit contains interfaces: IShellBrowser, IShellView, IEnumExtraSearch,
// IShellFolder2, IShellDetails, IShellBrowserInfo
// This unit contains class TShellBrowser
// This unit also correct errors in definition interfaces by Borland
//
// version: 0.8
//

unit NameSpaceDetails;

interface

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
	ActiveX, ShlObj, ComCtrls, Commctrl, ComObj ;

const
	SHCOLSTATE_TYPE_STR			= $00000001;
	SHCOLSTATE_TYPE_INT 		= $00000002;
	SHCOLSTATE_TYPE_DATE 		= $00000003;
	SHCOLSTATE_TYPEMASK 		= $0000000F;
	SHCOLSTATE_ONBYDEFAULT	= $00000010;
	SHCOLSTATE_SLOW					= $00000020;
	SHCOLSTATE_EXTENDED			= $00000040;
	SHCOLSTATE_SECONDARYUI	= $00000080;
	SHCOLSTATE_HIDDEN				= $00000100;

	WM_GETISHELLBROWSER =	WM_USER + 7;

	IID_IShellFolder2: TGUID = (
		D1:$93F2F68C; D2:$1D1B; D3:$11D3; D4:($A3,$0E,$00,$C0,$4F,$79,$AB,$D1));
	IID_IShellDetails: TGUID = (
		D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
	IID_IEnumExtraSearch: TGUID = (
		D1:$E700BE1; D2: $9DB6; D3:$11D1; D4:($A1,$CE,$00,$C0,$4F,$D7,$5D,$13));

	SID_IShellDetails      = '{000214EC-0000-0000-C000-000000000046}';
	SID_IShellFolder2      = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';
	SID_IEnumExtraSearch   = '{0E700BE1-9DB6-11D1-A1CE-00C04FD75D13}';
	SID_IShellViewInfo		 = '{538545A0-0883-11D7-B782-00C0DFC36813}';

type
	PResult	=	^LResult;

	IShellView	=	interface;

	IShellBrowser = interface(IOleWindow)
		[SID_IShellBrowser]
		function InsertMenusSB(hMenuShared: HMENU;out MenuWidths: TOleMenuGroupWidths): HResult; stdcall;
		function SetMenuSB(hMenuShared: HMENU;hOleMenuReserved: HOLEMENU; hwndActiveObject:HWND): HResult; stdcall;
		function RemoveMenusSB(hMenuShared: HMENU): HResult; stdcall;
		function SetStatusTextSB(StatusText: POleStr): HResult; stdcall;
		function EnableModelessSB(Enable: BOOL): HResult; stdcall;
		function TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult; stdcall;
		function BrowseObject(pidl: PItemIDList; flags: UINT): HResult; stdcall;
		function GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult; stdcall;
		function GetControlWindow(ID: UINT; out Wnd: HWND): HResult; stdcall;
		function SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM; Result: PResult): HResult; stdcall;
		function QueryActiveShellView(out ShellView: IShellView): HResult; stdcall;
		function OnViewWindowActive(const AShellView: IShellView): HResult; stdcall;
		function SetToolbarItems(TBButton: PTBButton; nButtons, uFlags: UINT): HResult; stdcall;
	end;

	IShellView = interface(IOleWindow)
		[SID_IShellView]
		function TranslateAccelerator(var Msg: TMsg): HResult; stdcall;
		function EnableModeless(Enable: Boolean): HResult; stdcall;
		function UIActivate(State: UINT): HResult; stdcall;
		function Refresh: HResult; stdcall;
		function CreateViewWindow(PrevView: IShellView;	var FolderSettings: TFolderSettings; ShellBrowser: IShellBrowser; var Rect: TRect; out Wnd: HWND): HResult; stdcall;
		function DestroyViewWindow: HResult; stdcall;
		function GetCurrentInfo(out FolderSettings: TFolderSettings): HResult; stdcall;
		function AddPropertySheetPages(Reseved: DWORD;lpfnAddPage: TFNAddPropSheetPage; lParam: LPARAM): HResult; stdcall;
		function SaveViewState: HResult; stdcall;
		function SelectItem(pidl: PItemIDList; flags: UINT): HResult; stdcall;
		function GetItemObject(Item: UINT; const iid: TIID; IPtr: Pointer): HResult; stdcall;
	end;

	PShellDetails = ^TShellDetails;
	_SHELLDETAILS = record
		fmt,
		cxChar: Integer;
		str: STRRET;
	end;
	TShellDetails = _SHELLDETAILS;
	SHELLDETAILS = _SHELLDETAILS;

	PSHCOLUMNID	=	^TSHCOLUMNID;
	TSHCOLUMNID	= record
		GUID	:	TGUID;
		PID		:	LongWord;
	end;

	PExtraSearch = ^TExtraSearch;
	tagExtraSearch = record
		guidSearch: TGUID;
		wszFriendlyName,
		wszMenuText: array[0..79] of WideChar;
		wszHelpText: array[0..MAX_PATH] of WideChar;
		wszUrl: array[0..2047] of WideChar;
		wszIcon,
		wszGreyIcon,
		wszClrIcon: array[0..MAX_PATH+10] of WideChar;
	end;
	TExtraSearch = tagExtraSearch;

	IEnumExtraSearch = interface(IUnknown)
		[SID_IEnumExtraSearch]
		function Next(celt: ULONG; out rgelt: PExtraSearch;	out pceltFetched: ULONG): HResult; stdcall;
		function Skip(celt: ULONG): HResult; stdcall;
		function Reset: HResult; stdcall;
		function Clone(out ppEnum: IEnumExtraSearch): HResult; stdcall;
	end;

	SHCOLSTATE = type Integer;

	IShellFolder2 = interface(IShellFolder)
		[SID_IShellFolder2]
		function GetDefaultSearchGUID(out pguid: TGUID): HResult; stdcall;
		function EnumSearches(out ppEnum: IEnumExtraSearch): HResult; stdcall;
		function GetDefaultColumn(dwRes: DWORD; var pSort: ULONG;	var pDisplay: ULONG): HResult; stdcall;
		function GetDefaultColumnState(iColumn: UINT; var pcsFlags: DWORD): HResult; stdcall;
		function GetDetailsEx(pidl: PItemIDList; const pscid: TSHCOLUMNID;	pv: POleVariant): HResult; stdcall;
		function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;	var psd: TShellDetails): HResult; stdcall;
		function MapNameToSCID(pwszName: LPCWSTR; var pscid: TShColumnID): HResult; stdcall;
	end;

	IShellDetails = interface(IUnknown)
		[SID_IShellDetails]
		function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;	var pDetails: TShellDetails): HResult; stdcall;
		function ColumnClick(iColumn: UINT): HResult; stdcall;
	end;

	IShellBrowserInfo	=	interface(IUnknown)
		[SID_IShellViewInfo]
		function	ShellView:IShellView;stdcall;
		function	ShellViewHandle:HWND;stdcall;
		function  ListHandle:HWND;stdcall;
		function  HeaderHandle:HWND;stdcall;
		procedure	CreateShellView(ANameSpace:Pointer);stdcall;
	end;

	TShellBrowser	=	class(TWinControl, IShellBrowser, IShellBrowserInfo)
	protected
		FRefCount				:	Integer;

		FShellView				:	IShellView;
		FShellViewHandle	:	HWND;
		FListHandle,
		FHeaderHandle		:	HWND;

		FSaveOwner			:	TWinControl;

		procedure	WMGETISHELLBROWSER(var Msg:TMessage); message WM_GETISHELLBROWSER;

		function IShellBrowser.QueryInterface=IShellBrowser_QueryInterface;
		function IShellBrowser._AddRef=IShellBrowser_AddRef;
		function IShellBrowser._Release=IShellBrowser_Release;
		function IShellBrowserInfo.QueryInterface=IShellBrowser_QueryInterface;
		function IShellBrowserInfo._AddRef=IShellBrowser_AddRef;
		function IShellBrowserInfo._Release=IShellBrowser_Release;
		function IShellBrowser_QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
		function IShellBrowser_AddRef: Integer; stdcall;
		function IShellBrowser_Release: Integer; stdcall;
		//IShellBrowser as IOleWindow
		function GetWindow(out wnd: HWnd): HResult; stdcall;
		function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
		//IShellBrowser
		function InsertMenusSB(hMenuShared: HMENU;out MenuWidths: TOleMenuGroupWidths): HResult; stdcall;
		function SetMenuSB(hMenuShared: HMENU;hOleMenuReserved: HOLEMENU;hwndActiveObject:HWND): HResult; stdcall;
		function RemoveMenusSB(hMenuShared: HMENU): HResult; stdcall;
		function SetStatusTextSB(StatusText: POleStr): HResult; stdcall;
		function EnableModelessSB(Enable: BOOL): HResult; stdcall;
		function TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult; stdcall;
		function BrowseObject(pidl: PItemIDList; flags: UINT): HResult; stdcall;
		function GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult; stdcall;
		function GetControlWindow(ID: UINT; out Wnd: HWND): HResult; stdcall;
		function SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM; AResult: PResult): HResult; stdcall;
		function QueryActiveShellView(out AShellView: IShellView): HResult; stdcall;
		function OnViewWindowActive(const AShellView: IShellView): HResult; stdcall;
		function SetToolbarItems(TBButton: PTBButton;	nButtons, uFlags: UINT): HResult; stdcall;

		function	ShellView:IShellView;stdcall;
		function	ShellViewHandle:HWND;stdcall;
		function  ListHandle:HWND;stdcall;
		function  HeaderHandle:HWND;stdcall;
		procedure	CreateShellView(ANameSpace:Pointer);stdcall;

	public
		constructor	Create(AOwner:TComponent);override;
		destructor	Destroy;override;

	end;

implementation

uses NameSpaceUnit;

{ TShellBrowser }
function TShellBrowser.BrowseObject(pidl: PItemIDList;	flags: UINT): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.ContextSensitiveHelp(fEnterMode: BOOL): HResult;

begin
	Result:=S_OK;
end;

constructor TShellBrowser.Create(AOwner:TComponent);

begin
	inherited Create(nil);
	FSaveOwner:=TWinControl(AOwner);
end;

procedure TShellBrowser.CreateShellView(ANameSpace: Pointer);

//this procedure work on win95, win98, winNT, w2k
//this procedure don't work on XP - HeaderHandle and ListHandle is 0

const
	cnSysListView	=	'SysListView32';
	cnSysHeader	=	'SysHeader32';
	cpSize	=	1024;

var
	FS:TFolderSettings;
	R:TRect;
	P:PChar;
	NS:PNameSpaceItem;

begin
	try
		NS:=ANameSpace;
		if Parent=nil then Parent:=FSaveOwner;
		if ShellView<>nil then begin
			ShellView.UIActivate(LongWord(SVUIA_DEACTIVATE));
			ShellView.DestroyViewWindow;
			FShellView:=nil;
		end;
		FListHandle:=0;
		FHeaderHandle:=0;
		FShellViewHandle:=0;
		NS.ShellFolder.CreateViewObject(Self.Handle,IShellView,Pointer(FShellView));
		Application.ProcessMessages;
		if ShellView=nil then Exit;
		FS.ViewMode:=FVM_DETAILS;
		FS.fFlags:=0;
		R:=Rect(0,0,0,0);
		ShellView.CreateViewWindow(nil,FS,Self,R,FShellViewHandle);
		Application.ProcessMessages;
		ShowWindow(ShellViewHandle,SW_HIDE);
		P:=AllocMem(cpSize);
		try
			FListHandle:=GetDlgItem(ShellViewHandle,1);
			if ListHandle=0 then Exit;
			GetClassName(ListHandle,P,cpSize);
			if P<>cnSysListView then begin FListHandle:=0;Exit; end;
			FHeaderHandle:=GetDlgItem(ListHandle,0);
			if HeaderHandle=0 then Exit;
			GetClassName(HeaderHandle,P,cpSize);
			if P<>cnSysHeader then begin FListHandle:=0;FHeaderHandle:=0;Exit; end;
		finally
			FreeMem(P);
		end;
	except
		FShellView:=nil;
		FShellViewHandle:=0;
	end;
end;

destructor TShellBrowser.Destroy;

begin
	if ShellView<>nil then begin
		ShellView.UIActivate(LongWord(SVUIA_DEACTIVATE));
		ShellView.DestroyViewWindow;
		FHeaderHandle:=0;
		FListHandle:=0;
		FShellViewHandle:=0;
		FShellView:=nil;
	end;
	inherited;
end;

function TShellBrowser.EnableModelessSB(Enable: BOOL): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.GetControlWindow(ID: UINT; out Wnd: HWND): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.GetWindow(out wnd: HWnd): HResult;

begin
	wnd:=Self.Handle;
	Result:=S_OK;
end;

function TShellBrowser.HeaderHandle: HWND;

begin
	Result:=FHeaderHandle;
end;

function TShellBrowser.InsertMenusSB(hMenuShared: HMENU;out MenuWidths: TOleMenuGroupWidths): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.IShellBrowser_AddRef: Integer;

begin
	Result:=-1;
end;

function TShellBrowser.IShellBrowser_QueryInterface(const IID: TGUID; out Obj): HResult;

const
	E_NOINTERFACE = HResult($80004002);

begin
	if GetInterface(IID, Obj) then Result:=S_OK else Result:=E_NOINTERFACE;
end;

function TShellBrowser.IShellBrowser_Release: Integer;

begin
	Result:=-1;
end;

function TShellBrowser.ListHandle: HWND;

begin
	Result:=FListHandle;
end;

function TShellBrowser.OnViewWindowActive(const AShellView: IShellView): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.QueryActiveShellView(out AShellView: IShellView): HResult;

begin
	AShellView:=ShellView;
	Result:=S_OK;
end;

function TShellBrowser.RemoveMenusSB(hMenuShared: HMENU): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM; AResult: PResult): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.SetMenuSB(hMenuShared: HMENU; hOleMenuReserved: HOLEMENU; hwndActiveObject: HWND): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.SetStatusTextSB(StatusText: POleStr): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.SetToolbarItems(TBButton: PTBButton; nButtons, uFlags: UINT): HResult;

begin
	Result:=E_NOTIMPL;
end;

function TShellBrowser.ShellView: IShellView;

begin
	Result:=FShellView;
end;

function TShellBrowser.ShellViewHandle: HWND;

begin
	Result:=FShellViewHandle;
end;

function TShellBrowser.TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult;

begin
	Result:=E_NOTIMPL;
end;

procedure TShellBrowser.WMGETISHELLBROWSER(var Msg: TMessage);

var
	SB:IShellBrowser;

begin
	SB:=Self;
	Msg.Result:=Integer(SB);
end;

end.
