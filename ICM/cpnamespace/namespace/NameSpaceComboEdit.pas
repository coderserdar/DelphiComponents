//
// The original Delphi code is : NameSpaceComboEdit.pas released 12.12.2002
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
// This unit contains class TNameSpaceComboEdit (Combo Edit of Folders)
//
// version: 0.95 realeased 29.02.2004
// fix bug - not free Items in TNameSpaceComboTree on destroy and FCanvas object in TNameSpaceComboEdit)
//
// version: 0.94 realeased 09.02.2004
// add two Event
//		OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent
//		OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent (propose joel joly)
// change HandleChangeNotify
//
// version: 0.91 released 04.01.2004
// synchronize TreeView, ComboEdit and ListView
//
// version: 0.84 released 28.03.2003
// fix bug in TNameSpaceComboTree.Change in implementation section
// (now set Caption property of FolderStaticText objects) (bug find Roland Ruder)
//
// version: 0.8
//

unit NameSpaceComboEdit;

interface

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, ShlObj, NameSpaceLibrary, NameSpaceUnit, NameSpaceTree,
	NameSpaceList;

type
	TNameSpaceComboEdit = class(TCustomEdit)
	private
		FWorkWithList: Boolean;
		FOnBeforeNotifyEvent: TNameSpaceBeforeNotifyEvent;
		FOnChangeNotifyEvent: TNameSpaceChangeNotifyEvent;
		function GetColor: TColor;
		function GetComboEnabled: Boolean;
		function GetComboFolderStaticText: TStaticText;
		function GetComboHiddenFolder: Boolean;
		function GetComboRespondOnChangeNotify: Boolean;
		procedure SetColor(const Value: TColor);
		procedure SetComboEnabled(const Value: Boolean);
		procedure SetComboFolderStaticText(const Value: TStaticText);
		procedure SetComboHiddenFolder(const Value: Boolean);
		procedure SetComboRespondOnChangeNotify(const Value: Boolean);
		function GetCurrentFolder: String;
		procedure SetCurrentFolder(const Value: String);
		procedure SetWorkWithList(const Value: Boolean);
		function GetNameSpaceList: TNameSpaceList;
		procedure SetNameSpaceList(const Value: TNameSpaceList);
		{ Private declarations }
	protected
		{ Protected declarations }
		FCanvas	: TControlCanvas;
		FButton : TSpeedButton;
		FTree		: TNameSpaceTree;

		procedure CreateWnd; override;
		procedure SetDrawEditRect;
		procedure	OnButtonClick(Sender:TObject);
		procedure CreateParams(var Params: TCreateParams); override;
		procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMLButtonUp(var Message: TWMLButtonUp); message	WM_LBUTTONUP;
		procedure WMChar(var Message: TWMChar); message WM_CHAR;
		procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
		procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
		procedure	Loaded;override;

		function	CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;virtual;
		procedure	CallChangeNotifyEvent(TypeChange:LongWord);virtual;

	public
		{ Public declarations }
		constructor	Create(AOwner:TComponent);override;
		destructor Destroy;override;
		procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

		property	FolderTree:TNameSpaceTree read FTree;
		property CurrentFolder:String read GetCurrentFolder write SetCurrentFolder;

	published
		{ Published declarations }
		property ShowHiddenFolders: Boolean read GetComboHiddenFolder write SetComboHiddenFolder default False;
		property FolderStaticText: TStaticText read GetComboFolderStaticText write SetComboFolderStaticText;
		property RespondOnChangeNotify: Boolean read GetComboRespondOnChangeNotify write SetComboRespondOnChangeNotify default True;
		property WorkWithList:Boolean read FWorkWithList write SetWorkWithList;
		property NameSpaceList:TNameSpaceList read GetNameSpaceList write SetNameSpaceList;

		property Anchors;
		property BiDiMode;
		property BorderStyle;
		property Color : TColor read GetColor write SetColor;
		property Constraints;
		property Ctl3D;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled:Boolean read GetComboEnabled write SetComboEnabled;
		property Font;
		property ParentBiDiMode;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;

		property OnChange;
		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;

		//add in 0.94
		property OnBeforeNotifyEvent:TNameSpaceBeforeNotifyEvent read FOnBeforeNotifyEvent write FOnBeforeNotifyEvent;
		property OnChangeNotifyEvent:TNameSpaceChangeNotifyEvent read FOnChangeNotifyEvent write FOnChangeNotifyEvent;
	end;

procedure Register;

implementation

uses ComCtrls,Commctrl;

procedure Register;
begin
	RegisterComponents('View Library', [TNameSpaceComboEdit]);
end;

function	Min(x,y : Integer):Integer;

asm
	cmp eax,edx
	jle @End
	mov eax,edx
@End:
end;

function	Max(x,y : Integer):Integer;

asm
	cmp eax,edx
	jge @End
	mov eax,edx
@End:
end;

type
	TNameSpaceComboTree	=	class(TNameSpaceTree)
	protected
		procedure CreateWnd; override;
		procedure CreateParams(var Params: TCreateParams); override;
		procedure WMLButtonUp(var Message: TWMLButtonUp); message	WM_LBUTTONUP;
		procedure Change(Node: TTreeNode); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;

		function	CallBeforeNotifyEvent(TypeChange:LongWord;var Item1,Item2:PItemIDList):Boolean;override;
		procedure	CallChangeNotifyEvent(TypeChange:LongWord);override;

	public
		constructor Create(Owner:TComponent);override;
		destructor Destroy;override;
	end;


{ TNameSpaceComboTree }

function TNameSpaceComboTree.CallBeforeNotifyEvent(TypeChange: LongWord;var Item1, Item2: PItemIDList): Boolean;

begin
	Result:=TNameSpaceComboEdit(Owner).CallBeforeNotifyEvent(TypeChange,Item1,Item2);
end;

procedure TNameSpaceComboTree.CallChangeNotifyEvent(TypeChange: LongWord);

begin
	TNameSpaceComboEdit(Owner).CallChangeNotifyEvent(TypeChange);
end;

procedure TNameSpaceComboTree.Change(Node: TTreeNode);

begin
	if Assigned(FolderEdit) then begin
		FolderEdit.Text:=DisplayName(Node,tpnInFolder);
		if TNameSpaceTree(Owner).Enabled then FolderEdit.SelectAll;
		// change in v0.91
		if (NameSpaceList<>nil) and (Node<>nil) and (Node.Data<>nil) then
			NameSpaceList.CurrentFolderFromID(PNameSpaceItem(Node.Data).ID,Self);
	end;
	if Assigned(FolderStaticText) then FolderStaticText.Caption:=CurrentFolder;
end;

constructor TNameSpaceComboTree.Create(Owner: TComponent);

begin
	inherited Create(Owner);
	Anchors:=[];
	Visible:=False;
	BorderWidth:=2;
end;

procedure TNameSpaceComboTree.CreateParams(var Params: TCreateParams);

begin
	inherited CreateParams(Params);
	with Params do ExStyle:=ExStyle or WS_EX_TOOLWINDOW or WS_EX_STATICEDGE;
end;

procedure TNameSpaceComboTree.CreateWnd;

begin
	inherited CreateWND;
	if not (csDesigning in ComponentState) then Windows.SetParent(Handle, 0);
end;

destructor TNameSpaceComboTree.Destroy;

begin
	if not (csDesigning in ComponentState) then begin
		DeleteNodeChildren(DesktopNode);
		DesktopNode.Delete;
	end;
	inherited;
end;

procedure TNameSpaceComboTree.KeyDown(var Key: Word; Shift: TShiftState);

begin
	inherited KeyDown(Key,Shift);
	if (Shift=[]) then case Key of
		VK_ESCAPE,VK_RETURN : begin
			Visible:=False;
			if Owner<>nil then TWinControl(Owner).Perform(WM_SETFOCUS,0,0);
		end;
	end;
end;

procedure TNameSpaceComboTree.WMLButtonUp(var Message: TWMLButtonUp);

begin
	inherited;
	if htOnItem in GetHitTestInfoAt(Message.XPos, Message.YPos) then begin
		Visible:=False;
		if Owner<>nil then TWinControl(Owner).Perform(WM_SETFOCUS,0,0);
	end;
end;

{ TNameSpaceComboEdit }

function TNameSpaceComboEdit.CallBeforeNotifyEvent(TypeChange: LongWord;var Item1, Item2: PItemIDList): Boolean;

begin
	Result:=True;
	if Assigned(OnBeforeNotifyEvent) then Result:=OnBeforeNotifyEvent(Self,TypeChange,Item1,Item2);
end;

procedure TNameSpaceComboEdit.CallChangeNotifyEvent(TypeChange: LongWord);

begin
	if Assigned(OnChangeNotifyEvent) then OnChangeNotifyEvent(Self,TypeChange);
end;

constructor TNameSpaceComboEdit.Create(AOwner: TComponent);

begin
	inherited Create(AOwner);
	ControlStyle:=ControlStyle+[csAcceptsControls];
	Cursor:=crArrow;
	ReadOnly:=True;
	FCanvas:=TControlCanvas.Create;
	FCanvas.Control:=Self;
	FButton:=TSpeedButton.Create(Self);
	FButton.Align:=alRight;
	FButton.Font.Name:='Marlett';
	FButton.Caption:='6';
	FButton.Transparent:=False;
	FButton.Cursor:=crArrow;
	FButton.Spacing:=-1;
	FButton.OnClick:=OnButtonClick;
	FTree:=TNameSpaceComboTree.Create(Self);
	FButton.Parent:=Self;
	FTree.FolderEdit:=TEdit(Self);
	FTree.Parent:=Self;
	Width:=221;
end;

procedure TNameSpaceComboEdit.CreateParams(var Params: TCreateParams);

begin
	inherited CreateParams(Params);
	Params.Style:=Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TNameSpaceComboEdit.CreateWnd;

begin
	inherited;
	SetDrawEditRect;
end;

destructor TNameSpaceComboEdit.Destroy;
begin
	FCanvas.Free;
	FCanvas:=nil;
	inherited;
end;

function TNameSpaceComboEdit.GetColor: TColor;

begin
	Result:=inherited Color;
end;

function TNameSpaceComboEdit.GetComboEnabled: Boolean;

begin
	Result:=inherited Enabled;
end;

function TNameSpaceComboEdit.GetComboFolderStaticText: TStaticText;

begin
	Result:=FolderTree.FolderStaticText;
end;

function TNameSpaceComboEdit.GetComboHiddenFolder: Boolean;

begin
	Result:=FolderTree.ShowHiddenFolders;
end;

function TNameSpaceComboEdit.GetComboRespondOnChangeNotify: Boolean;

begin
	Result:=FolderTree.RespondOnChangeNotify;
end;

function TNameSpaceComboEdit.GetCurrentFolder: String;

begin
	Result:=FolderTree.CurrentFolder;
end;

function TNameSpaceComboEdit.GetNameSpaceList: TNameSpaceList;

begin
	Result:=FolderTree.NameSpaceList;
end;

procedure TNameSpaceComboEdit.Loaded;

begin
	inherited Loaded;
	FolderTree.HandleNeeded;
	FolderTree.Selected:=FolderTree.DesktopNode;
	Perform(WM_SETFOCUS,0,0);
end;

procedure TNameSpaceComboEdit.OnButtonClick(Sender: TObject);

var
	ClientPos, ScreenPos: TPoint;

begin
	if Enabled then SelectAll;
	with FTree do begin
		Visible:=not Visible;
		if Visible then begin
			ClientPos.X:=Self.Left;
			ClientPos.Y:=Self.Top+Self.Height+2;
			ScreenPos:=Self.Parent.ClientToScreen(ClientPos);
			SetBounds(ScreenPos.X,ScreenPos.Y,Self.Width,Height);
			Ctl3D:=False;
			BringToFront;
			Perform(WM_SETFOCUS,0,0);
			Self.SetFocus;
		end else
			SendToBack;
	end;
end;

procedure TNameSpaceComboEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);

var
	ClientPos, ScreenPos: TPoint;

begin
	if FButton<>nil then with FButton do begin
		Width:=Min(GetSystemMetrics(SM_CXHTHUMB),Self.Width div 6);
		Font.Size:=(3*Width div 5);
	end;
	if (FTree<>nil) and (Parent<>nil) then begin
		ClientPos.X:=ALeft;
		ClientPos.Y:=ATop+AHeight+2;
		ScreenPos:=Parent.ClientToScreen(ClientPos);
		FTree.SetBounds(ScreenPos.X,ScreenPos.Y,AWidth,FTree.Height);
		FTree.Ctl3D:=False;
	end;
	inherited SetBounds(ALeft, ATop, AWidth, AHeight);
	if FButton<>nil then with FButton do Align:=alRight;
	if Parent<>nil then SetDrawEditRect;
end;

procedure TNameSpaceComboEdit.SetColor(const Value: TColor);

begin
	inherited Color:=Value;
	FTree.Color:=Value;
end;

procedure TNameSpaceComboEdit.SetComboEnabled(const Value: Boolean);

begin
	FButton.Enabled:=Value;
	FTree.Enabled:=Value;
	inherited Enabled:=Value;
end;

procedure TNameSpaceComboEdit.SetComboFolderStaticText(const Value: TStaticText);

begin
	FolderTree.FolderStaticText:=Value;
end;

procedure TNameSpaceComboEdit.SetComboHiddenFolder(const Value: Boolean);

begin
	FolderTree.ShowHiddenFolders:=Value;
end;

procedure TNameSpaceComboEdit.SetComboRespondOnChangeNotify(const Value: Boolean);

begin
	FolderTree.RespondOnChangeNotify:=Value;
end;

procedure TNameSpaceComboEdit.SetCurrentFolder(const Value: String);

begin
	FolderTree.CurrentFolder:=Value;
end;

procedure TNameSpaceComboEdit.SetDrawEditRect;

var
	Space: TRect;

begin
//	if Parent=nil then Exit;
	SetRect(Space, 23, Max(0,((ClientHeight+Font.Height) div 2)-2), ClientWidth-FButton.Width-2,ClientHeight);
	SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Space));
	FButton.Flat:=(not Ctl3D) or (inherited BorderStyle=bsNone);	
end;

procedure TNameSpaceComboEdit.SetNameSpaceList(const Value: TNameSpaceList);

begin
	FolderTree.NameSpaceList:=Value;
end;

procedure TNameSpaceComboEdit.SetWorkWithList(const Value: Boolean);

begin
	if WorkWithList<>Value then begin
		FWorkWithList:=Value;
		FTree.ShowButtons:=not Value;
		FTree.ShowLines:=not Value;
		if Value then FTree.BorderWidth:=3 else FTree.BorderWidth:=2; 
	end;
end;

procedure TNameSpaceComboEdit.WMChar(var Message: TWMChar);

begin
	if FTree.Visible then
		FTree.Perform(WM_CHAR,Message.CharCode,Message.KeyData)
	else
		inherited;
end;

procedure TNameSpaceComboEdit.WMKeyDown(var Message: TWMKeyDown);

begin
	if FTree.Visible then
		FTree.Perform(WM_KEYDOWN,Message.CharCode,Message.KeyData)
	else
		if (not FTree.Visible) and (KeyDataToShiftState(Message.KeyData)=[]) and (Message.CharCode=VK_DOWN) then
			OnButtonClick(Self)
		else
			inherited;
end;

procedure TNameSpaceComboEdit.WMKeyUp(var Message: TWMKeyUp);

begin
	if FTree.Visible then
		FTree.Perform(WM_KEYUP,Message.CharCode,Message.KeyData)
	else
		inherited;
end;

procedure TNameSpaceComboEdit.WMLButtonUp(var Message: TWMLButtonUp);

begin
	inherited;
	OnButtonClick(Self);
end;

procedure TNameSpaceComboEdit.WMPaint(var Message: TWMPaint);

begin
  inherited;
	with FTree, ImageList do if Selected<>nil then
		DrawOverlay(FCanvas,4,(Self.ClientHeight-ImageList.Height) div 2,Selected.ImageIndex,Selected.OverlayIndex)
	else
		if DesktopNode<>nil then
			DrawOverlay(FCanvas,4,(Self.ClientHeight-ImageList.Height) div 2,DesktopNode.ImageIndex,DesktopNode.OverlayIndex)
end;

procedure TNameSpaceComboEdit.WMSetFocus(var Message: TWMSetFocus);

begin
	inherited;
	if HandleAllocated then HideCaret(Handle);
end;

end.
