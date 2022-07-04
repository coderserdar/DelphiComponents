
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ShlCtrls;

interface

{$I STD.INC}

uses
  Windows, Messages, Forms, ActiveX, Classes, Controls, SysUtils, ComCtrls,
  CommCtrl, FileTools, ShellAPI, ShlObj, ShlIntf, ComObj, Graphics, BtnEdit, PopCtrls,
  ImgList, ScrollCtrls, GraphTools;

{ The GetSysImages funtion returns the handle of the system image list }

function GetSysImages(SmallImages: Boolean = True): HIMAGELIST;

{ The GetFileImageIndex function returns the system image list image index of
  the file or directory specified by the FileName parameter }

function GetFileImageIndex(const FileName: string;
  SmallImages: Boolean = True): Integer;

{ Shared shell ImageLists }

function ShellImageList(SmallImages: Boolean = True): TCustomImageList;

type
  TShellImageNode = class(TShellNode)
  private
    FSmallIndex: Integer;
    FLargeIndex: Integer;
  protected
    procedure Initialize; override;
  public
    class procedure LoadImage(Folder: TSpecialFolder); overload;
    class procedure LoadImage(const Path: string); overload;
    class procedure LoadImage(const pidl: PItemIDList); overload;
    property SmallIndex: Integer read FSmallIndex;
    property LargeIndex: Integer read FLargeIndex;
  end;

{ Images }

  TShellImageList = class(TCustomImageList)
  protected
    procedure Loaded; override;
    procedure Setup; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSmallShellImages = class(TShellImageList)
  protected
    procedure Setup; override;
  end;

  TLargeShellImages = class(TShellImageList)
  protected
    procedure Setup; override;
  end;

{ TShellControl class }

  TShellControl = class(TWinControl, IUnknown)
  private
    FRefCount: Integer;
    FBorderStyle: TBorderStyle;
  protected
    procedure SetBorderStyle(Value: TBorderStyle); virtual;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; reintroduce;
      stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateParams(var Params: TCreateParams); override;
  protected
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TShellTreeNode class }

  TShellTree = class;

  TShellTreeNode = class(TShellNode)
  private
    FData: Pointer;
    FFileSystem: Boolean;
    FHandle: Pointer;
    FImageIndex: Integer;
    FTree: TShellTree;
    FSelectedIndex: Integer;
    function GetParent: TShellTreeNode;
    function GetItem(Index: Integer): TShellTreeNode;
    function GetFilePath: string;
  protected
    procedure Initialize; override;
    property Tree: TShellTree read FTree write FTree;
    property Handle: Pointer read FHandle write FHandle;
  public
    procedure Expand;
    procedure Collapse;
    property Data: Pointer read FData write FData;
    property FilePath: string read GetFilePath;
    property FileSystem: Boolean read FFileSystem;
    property Item[Index: Integer]: TShellTreeNode read GetItem; default;
    property ImageIndex: Integer read FImageIndex;
    property Parent: TShellTreeNode read GetParent;
    property SelectedIndex: Integer read FSelectedIndex;
  end;

{ TShellTree class }

  TShellActionEvent = procedure(Sender: TObject; Node: TShellNode;
    var AllowAction: Boolean) of object;
  TShellNotifyEvent = procedure(Sender: TObject; Node: TShellNode) of
    object;

  TShellTree = class(TShellControl)
  private
    FContextMenu: IContextMenu;
    FMenu: HMENU;
    FRoot: TShellNode;
    FAutoExpand: Boolean;
    FHideSelection: Boolean;
    FHotTrack: Boolean;
    FRowSelect: Boolean;
    FShowButtons: Boolean;
    FShowLines: Boolean;
    FShowRoot: Boolean;
    FSpecialFolder: TSpecialFolder;
    FReadOnly: Boolean;
    FToolTips: Boolean;
    FOnChange: TShellNotifyEvent;
    FOnCollapse: TShellNotifyEvent;
    FOnCollapsing: TShellActionEvent;
    FOnExpand: TShellNotifyEvent;
    FOnExpanding: TShellActionEvent;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure SetAutoExpand(Value: Boolean);
    procedure SetShowButtons(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetShowLines(Value: Boolean);
    function GetIndent: Integer;
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetRoot(Value: TShellNode);
    procedure SetRowSelect(Value: Boolean);
    function GetSelectedNode: TShellNode;
    procedure SetSelectedNode(Value: TShellNode);
    procedure SetSpecialFolder(Value: TSpecialFolder);
    procedure SetToolTips(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure InsertNode(Node: TShellTreeNode);
    function GetNode(TreeItem: HTREEITEM): TShellTreeNode;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeAt(X, Y: Integer): TShellTreeNode;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    property SelectedNode: TShellNode read GetSelectedNode write SetSelectedNode;
    property Root: TShellNode read FRoot write SetRoot;
  published
    property Align;
    property Anchors;
    property AutoExpand: Boolean read FAutoExpand write SetAutoExpand;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection: Boolean read FHideSelection write SetHideSelection;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property Indent: Integer read GetIndent write SetIndent;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowSelect: Boolean read FRowSelect write SetRowSelect;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write
      SetSpecialFolder;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property ShowRoot: Boolean read FShowRoot write SetShowRoot;
    property TabOrder;
    property TabStop default True;
    property ToolTips: Boolean read FToolTips write SetToolTips;
    property Visible;
    property OnChange: TShellNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnCollapse: TShellNotifyEvent read FOnCollapse write
      FOnCollapse;
    property OnCollapsing: TShellActionEvent read FOnCollapsing write
      FOnCollapsing;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpand: TShellNotifyEvent read FOnExpand write
      FOnExpand;
    property OnExpanding: TShellActionEvent read FOnExpanding write
      FOnExpanding;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TShellTreePopup }

  TShellTreePopup = class(TShellTree)
  private
    procedure WMNCCalcSize(var Message: TMessage); message WM_NCCALCSIZE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

{ TPopupTreeForm }

  TShellTreePopupForm = class(TCustomPopupForm)
  private
    FShellTree: TShellTreePopup;
    FSelectChanged: Boolean;
    FHoverNode: TShellTreeNode;
    FOnNodeHover: TShellNotifyEvent;
		procedure ShellTreeChange(Sender: TObject; Node: TShellNode);
		procedure ShellTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure ShellTreeMouseUp(Sender: TObject; Button: TMouseButton;
  		Shift: TShiftState; X, Y: Integer);
  protected
    procedure DoNodeHover(Node: TShellTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
  published
    property Associate;
    property Sizeable;
    property StatusText;
    property ShellTree: TShellTreePopup read FShellTree;
    property OnNodeHover: TShellNotifyEvent read FOnNodeHover write FOnNodeHover;
    property OnCancel;
    property OnSelect;
  end;

{ TShellEdit }

  TShellEditOptions = set of (soDefaultAction, soNavigate, soShortName, soSuggest);

  TShellEdit = class(TPopupEdit)
  private
    FAutoComplete: IAutoComplete2;
  	FShellTreePopupForm: TShellTreePopupForm;
    FOptions: TShellEditOptions;
		FStrings: IUnknown;
    FOnNodeHover: TShellNotifyEvent;
		procedure SelectionChanged;
    procedure TreeNodeHover(Sender: TObject; Node: TShellNode);
    function GetSelectedNode: TShellNode;
    procedure SetSelectedNode(Value: TShellNode);
    function GetHotTrack: Boolean;
    procedure SetHotTrack(Value: Boolean);
    procedure SetOptions(Value: TShellEditOptions);
    function GetRoot: TShellNode;
    procedure SetRoot(Value: TShellNode);
		function GetStatusText: string;
    procedure SetStatusText(Value: string);
    function GetSpecialFolder: TSpecialFolder;
    procedure SetSpecialFolder(Value: TSpecialFolder);
  protected
    function CreatePopup: TCustomPopupForm; override;
		procedure CreateWnd; override;
		procedure DoPlaceEdit(var Rect: TRect); override;
    procedure DoPopup; override;
    procedure DoCancel(Sender: TObject); override;
    procedure DoSelect(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedNode: TShellNode read GetSelectedNode write SetSelectedNode;
    property Root: TShellNode read GetRoot write SetRoot;
    property ButtonDown;
    property Canvas;
    property NextControl;
    property SearchStrings;
    property TextChanged;
  published
  	property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property Flat;
    property Glyph;
    property HideSelection;
    property Images;
    property ImageIndex;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property Options: TShellEditOptions read FOptions write SetOptions;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property Sizeable;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Text;
    property Visible;
    property WantTabs;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
		property StatusText: string read GetStatusText write SetStatusText;
    property SpecialFolder: TSpecialFolder read GetSpecialFolder write SetSpecialFolder;
    property OnButtonPress;
    property OnButtonClick;
    property OnChange;
    property OnCancel;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
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
    property OnNodeHover: TShellNotifyEvent read FOnNodeHover write FOnNodeHover;
    property OnPlaceEdit;
    property OnPopup;
    property OnSearch;
    property OnStartDock;
    property OnStartDrag;
    property OnSelect;
    property OnTab;
  end;

{ TShellView }

	TShellViewMode = (vmIcon, vmSmallIcon, vmList, vmDetails, vmThumnail,
  	vmTile, vmThumbstrip);

  TShellView = class(TShellControl, IOleWindow, IShellBrowser, ICommDlgBrowser,
  	ICommDlgBrowser2)
  private
    FDefListViewProc: Pointer;
    FListViewInstance: Pointer;
    FListViewHandle: HWND;
    FDefShellViewProc: Pointer;
    FShellViewInstance: Pointer;
    FShellViewHandle: HWND;
    FRoot: TShellNode;
    FShellView: IShellView;
    FSpecialFolder: TSpecialFolder;
    FStream: IStream;
    FStatusBar: TStatusBar;
    FViewMode: TShellViewMode;
    FFocusedNode: TShellNode;
    FOnDefaultAction: TShellActionEvent;
    FOnIncludeItem: TShellActionEvent;
    FOnViewChanged: TNotifyEvent;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetIShellBrowser(var Message: TMessage); message WM_GETISHELLBROWSER;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ListViewWndProc(var Message: TMessage);
    procedure ShellViewWndProc(var Message: TMessage);
    procedure SetRoot(Value: TShellNode);
    procedure SetSpecialFolder(Value: TSpecialFolder);
		procedure SetViewMode(Value: TShellViewMode);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure DoViewChanged;
    procedure SetBorderStyle(Value: TBorderStyle); override;
    { IOleWindow }
    function GetWindow(out wnd: HWnd): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    { IShellBrowser }
    function InsertMenusSB(hMenuShared: HMENU;
      out MenuWidths: TOleMenuGroupWidths): HResult; stdcall;
    function SetMenuSB(hMenuShared: HMENU;
      hOleMenuReserved: HOLEMENU; hwndActiveObject: HWND): HResult; stdcall;
    function RemoveMenusSB(hMenuShared: HMENU): HResult; stdcall;
    function SetStatusTextSB(StatusText: POleStr): HResult; stdcall;
    function EnableModelessSB(Enable: BOOL): HResult; stdcall;
    function TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult; stdcall;
    function BrowseObject(pidl: PItemIDList; flags: UINT): HResult; stdcall;
    function GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult; stdcall;
    function GetControlWindow(ID: UINT; out Wnd: HWND): HResult; stdcall;
    function SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM;
      var Rslt: LResult): HResult; stdcall;
    function QueryActiveShellView(var ShellView: IShellView): HResult; stdcall;
    function OnViewWindowActive(var ShellView: IShellView): HResult; stdcall;
    function SetToolbarItems(TBButton: PTBButton;
      nButtons, uFlags: UINT): HResult; stdcall;
    { ICommDlgBrowser }
    function OnDefaultCommand(const ppshv: IShellView): HResult; stdcall;
    function OnStateChange(const ppshv: IShellView; Change: ULONG): HResult; stdcall;
    function IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult; stdcall;
    { ICommDlgBrowser2 }
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
    	cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFocusedNode: TShellNode;
    procedure Up;
    property Root: TShellNode read FRoot write SetRoot;
    property ShellView: IShellView read FShellView;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
		property BorderStyle;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write
      SetSpecialFolder;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property TabOrder;
    property TabStop default True;
    property Text;
		property ViewMode: TShellViewMode read FViewMode write SetViewMode;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDefaultAction: TShellActionEvent read FOnDefaultAction write
      FOnDefaultAction;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
  	property OnIncludeItem: TShellActionEvent read FOnIncludeItem write
    	FOnIncludeItem;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnViewChanged: TNotifyEvent read FOnViewChanged write FOnViewChanged;
  end;

{ TShellBubbleItem }

  TShellBubbleItem = class(TCollectionItem)
  private
    FNode: TShellImageNode;
    FPath: string;
    FSpecialFolder: TSpecialFolder;
    procedure SetPath(const Value: string);
    procedure SetSpecialFolder(Value: TSpecialFolder);
  public
    constructor Create(Collection: TCollection); override;
    constructor CreateFromFolder(Collection: TCollection; SpecialFolder: TSpecialFolder);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Node: TShellImageNode read FNode;
  published
    property Path: string read FPath write SetPath;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write SetSpecialFolder;
  end;

{ TShellBubbleItems }

  TShellBubbleItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TShellBubbleItem;
    procedure SetItem(Index: Integer; Value: TShellBubbleItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TShellBubbleItem;
    function AddFolder(SpecialFolder: TSpecialFolder): TShellBubbleItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TShellBubbleItem read GetItem write SetItem; default;
  end;

{ TShellBubbles }

  TShellBubbleStyle = (sbSmall, sbLarge);

  TShellBubbles = class(TCustomBubbleList)
  private
    FItems: TShellBubbleItems;
    FStyle: TShellBubbleStyle;
    FTextLabels: Boolean;
    procedure SetItems(Value: TShellBubbleItems);
    procedure SetStyle(Value: TShellBubbleStyle);
    procedure SetTextLabels(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure Update; override;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Items: TShellBubbleItems read FItems write SetItems;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Style: TShellBubbleStyle read FStyle write SetStyle;
    property TabOrder;
    property TextLabels: Boolean read FTextLabels write SetTextLabels;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

function FindTreeNode(Root: TShellTreeNode; Node: TShellNode): TShellTreeNode;

var RegisterShellUnits: procedure(AOwner: TComponent);

implementation

{$R SHLCTRLS.RES}

const
  ShellImages: array[Boolean] of DWORD = (0, SHGFI_SMALLICON);

var
  ShellImageLists: array[Boolean] of TObject = (nil, nil);

function GetSysImages(SmallImages: Boolean = True): HIMAGELIST;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  Result := SHGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or ShellImages[SmallImages]);
end;

function GetFileImageIndex(const FileName: string;
  SmallImages: Boolean = True): Integer;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(FileName), 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or ShellImages[SmallImages]);
  Result := SHFileInfo.iIcon;
end;

function ShellImageList(SmallImages: Boolean = True): TCustomImageList;
begin
  Result := TCustomImageList(ShellImageLists[SmallImages]);
  if Result = nil then
  begin
    if SmallImages then
    	Result := TSmallShellImages.Create(Application)
    else
    	Result := TLargeShellImages.Create(Application);
		ShellImageLists[SmallImages] := Result;
  end;    
end;

{ TShellImageNode }

procedure TShellImageNode.Initialize;
const
  Flags = SHGFI_SYSICONINDEX or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
begin
  inherited Initialize;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags or SHGFI_SMALLICON);
  FSmallIndex := SHFileInfo.iIcon;
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags or SHGFI_LARGEICON);
  FLargeIndex := SHFileInfo.iIcon;
end;

class procedure TShellImageNode.LoadImage(Folder: TSpecialFolder);
begin
  TShellImageNode.CreateFromFolder(Folder).Free;
end;

class procedure TShellImageNode.LoadImage(const Path: string);
begin
  TShellImageNode.CreateFromList(ILCreateFromPath(Path)).Free;
end;

class procedure TShellImageNode.LoadImage(const pidl: PItemIDList);
begin
  TShellImageNode.CreateFromList(ILClone(pidl)).Free;
end;

{ Images }

constructor TShellImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Setup;
end;

procedure TShellImageList.Setup;
begin
  ShareImages := True;
  DrawingStyle := dsTransparent;
end;

procedure TShellImageList.Loaded;
begin
  inherited Loaded;
  Setup;
end;

procedure TSmallShellImages.Setup;
begin
  Handle := GetSysImages(True);
  inherited Setup;
end;

procedure TLargeShellImages.Setup;
begin
  Handle := GetSysImages(False);
  inherited Setup;
end;

{ TShellControl }

constructor TShellControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  if @RegisterShellUnits <> nil then
    RegisterShellUnits(AOwner);
end;

procedure TShellControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TShellControl.SetBorderStyle(Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

{ TShellControl.IUnknown }

function TShellControl.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TShellControl._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TShellControl._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
end;

{ TShellTreeNode }

procedure TShellTreeNode.Expand;
var
  I: Integer;
begin
  if TreeView_GetChild(FTree.Handle, Handle) = nil then
    for I := 0 to Count - 1 do
      FTree.InsertNode(Item[I]);
   TreeView_Expand(FTree.Handle, Handle, TVE_EXPAND);
end;

procedure TShellTreeNode.Collapse;
begin
   TreeView_Expand(FTree.Handle, Handle, TVE_COLLAPSE);
end;

function TShellTreeNode.GetParent: TShellTreeNode;
begin
  Result := TShellTreeNode(inherited Parent);
end;

function TShellTreeNode.GetItem(Index: Integer): TShellTreeNode;
begin
  Result := TShellTreeNode(inherited Item[index]);
end;

function FindTreeNode(Root: TShellTreeNode; Node: TShellNode): TShellTreeNode;
var
	I: Integer;
begin
	Result := nil;
	if ILIsEqual(Root.AbsoluteList, Node.AbsoluteList) then
  	Result := Root
  else if ILIsChild(Root.AbsoluteList, Node.AbsoluteList) then
  begin
  	Root.Expand;
    for I := 0 to Root.Count - 1 do
    begin
			Result := FindTreeNode(Root[I], Node);
      if Result <> nil then Break;
    end;
  end;
end;

{ TShellTree }

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
    else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

constructor TShellTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse] + [csDisplayDragImage,
    csReflector];
  Width := 121;
  Height := 97;
  TabStop := True;
  ReadOnly := True;
  ParentColor := False;
  FBorderStyle := bsSingle;
  FShowButtons := True;
  FShowRoot := False;
  FShowLines := True;
  FHideSelection := True;
  FToolTips := True;
  FRoot := TShellTreeNode.CreateFromFolder(FSpecialFolder);
  { TODO: Add edit instance pointer }
end;

destructor TShellTree.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

procedure TShellTree.CreateParams(var Params: TCreateParams);
const
  LineStyles: array[Boolean] of DWORD = (0, TVS_HASLINES);
  RootStyles: array[Boolean] of DWORD = (0, TVS_LINESATROOT);
  ButtonStyles: array[Boolean] of DWORD = (0, TVS_HASBUTTONS);
  EditStyles: array[Boolean] of DWORD = (TVS_EDITLABELS, 0);
  HideSelections: array[Boolean] of DWORD = (TVS_SHOWSELALWAYS, 0);
  DragStyles: array[TDragMode] of DWORD = (TVS_DISABLEDRAGDROP, 0);
  RTLStyles: array[Boolean] of DWORD = (0, TVS_RTLREADING);
  ToolTipStyles: array[Boolean] of DWORD = (TVS_NOTOOLTIPS, 0);
  AutoExpandStyles: array[Boolean] of DWORD = (0, TVS_SINGLEEXPAND);
  HotTrackStyles: array[Boolean] of DWORD = (0, TVS_TRACKSELECT);
  RowSelectStyles: array[Boolean] of DWORD = (0, TVS_FULLROWSELECT);
begin
  InitCommonControl(ICC_TREEVIEW_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TREEVIEW);
  with Params do
  begin
    Style := Style or LineStyles[FShowLines] or RootStyles[FShowRoot] or
      ButtonStyles[FShowButtons] or EditStyles[FReadOnly] or
      HideSelections[FHideSelection] or DragStyles[DragMode] or
      RTLStyles[UseRightToLeftReading] or ToolTipStyles[FToolTips] or
      AutoExpandStyles[FAutoExpand] or HotTrackStyles[FHotTrack] or
      RowSelectStyles[FRowSelect];
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TShellTree.CreateWnd;
var
  Images: HIMAGELIST;
begin
  inherited CreateWnd;
  Images := GetSysImages;
  TreeView_SetImageList(Handle, Images, TVSIL_NORMAL);
  InsertNode(TShellTreeNode(FRoot));
  SetReadOnly(ReadOnly);
end;

procedure TShellTree.CNNotify(var Message: TWMNotify);
var
  Node: TShellTreeNode;
  AllowAction: Boolean;
  Folder: IShellFolder;
  ItemList: PItemIDList;
  I: Integer;
begin
  with Message, PNMTreeView(NMHdr)^ do
    case NMHdr^.code of
      TVN_ITEMEXPANDING:
        begin
          Node := TShellTreeNode(itemNew.lParam);
          AllowAction := True;
          case action of
            TVE_COLLAPSE:
              if Assigned(FOnCollapsing) then
                FOnCollapsing(Self, Node, AllowAction);
            TVE_EXPAND:
              if Assigned(FOnExpanding) then
                FOnExpanding(Self, Node, AllowAction);
          end;
          if AllowAction then
          begin
            if TreeView_GetChild(Handle, itemNew.hItem) = nil then
              for I := 0 to Node.Count - 1 do
                InsertNode(Node[I]);
            if (Node.Count = 0) and (itemNew.cChildren = 1) then
            begin
              itemNew.mask := TVIF_CHILDREN;
              itemNew.cChildren := 0;
              TreeView_SetItem(Handle, itemNew);
            end;
            Message.Result := 0;
          end
          else
            Message.Result := 1;
        end;
      TVN_ITEMEXPANDED:
        begin
          Node := TShellTreeNode(itemNew.lParam);
          case action of
            TVE_COLLAPSE:
              if Assigned(FOnCollapse) then
                FOnCollapse(Self, Node);
            TVE_EXPAND:
              if Assigned(FOnExpand) then
                FOnExpand(Self, Node);
          end;
        end;
      TVN_SELCHANGED:
        begin
          Node := TShellTreeNode(itemNew.lParam);
          if Assigned(FOnChange) then
            FOnChange(Self, Node);
        end;
      NM_RCLICK:
        begin
          Node := TShellTreeNode(SelectedNode);
          if (Node <> nil) and (Node.Parent <> nil) then
          begin
            FMenu := CreatePopupMenu;
            try
              ItemList := Node.RelativeList;
              Folder := Node.Parent.ShellFolder;
              try
                OleCheck(Folder.GetUIObjectOf(Handle, 1, ItemList,
                  IID_IContextMenu, nil, FContextMenu));
                OleCheck(FContextMenu.QueryContextMenu(FMenu, 0, 0, High(Word),
                  CMF_NORMAL));
                with Mouse.CursorPos do
                  TrackPopupMenu(FMenu, TPM_LEFTALIGN, x, y, 0, Handle, nil);
                Application.ProcessMessages;
              finally
                FContextMenu := nil;
              end;
            finally
              DestroyMenu(FMenu);
              FMenu := 0;
            end;
          end;
        end;
    end;
end;

procedure TShellTree.WMCommand(var Message: TWMCommand);
var
  CommandInfo: TCMInvokeCommandInfo;
begin
  if (Message.NotifyCode = 0) and (FContextMenu <> nil) then
  begin
    FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
    with CommandInfo do
    begin
      cbSize := SizeOf(TCMInvokeCommandInfo);
      hwnd := Handle;
      lpVerb := MakeIntResource(Message.ItemID);
      nShow := SW_SHOW;
    end;
    OleCheck(FContextMenu.InvokeCommand(CommandInfo));
  end;
  inherited;
end;

procedure TShellTree.InsertNode(Node: TShellTreeNode);
var
  InsertStruct: TTVInsertStruct;
begin
  FillChar(InsertStruct, SizeOf(TTVInsertStruct), #0);
  if Node.Parent <> nil then
    InsertStruct.hParent := Node.Parent.Handle;
  InsertStruct.hInsertAfter := TVI_LAST;
  with InsertStruct.item do
  begin
    mask := TVIF_CHILDREN or TVIF_IMAGE or TVIF_PARAM or TVIF_SELECTEDIMAGE or
      TVIF_TEXT;
    pszText := PChar(Node.Name);
    iImage := Node.ImageIndex;
    iSelectedImage := Node.SelectedIndex;
    if Node.HasChildren then
      cChildren := 1;
    lParam := LongInt(Node);
  end;
  Node.Tree := Self;
  Node.Handle := TreeView_InsertItem(Handle, InsertStruct);
  if Node = FRoot then
    Node.Expand;
end;

function TShellTree.GetNode(TreeItem: HTREEITEM): TShellTreeNode;
var
  Item: TTVItem;
begin
  if TreeItem <> nil then
  begin
    FillChar(Item, SizeOf(TTVItem), #0);
    with Item do
    begin
      mask := TVIF_HANDLE or TVIF_PARAM;
      hItem := TreeItem;
    end;
    TreeView_GetItem(Handle, Item);
    Result := TShellTreeNode(Item.lParam)
  end
  else
    Result := nil;
end;

function TShellTree.GetNodeAt(X, Y: Integer): TShellTreeNode;
var
  HitTest: TTVHitTestInfo;
begin
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    if TreeView_HitTest(Handle, HitTest) <> nil then
      Result := GetNode(HitTest.hItem)
    else
    	Result := nil;
  end;
end;

function TShellTree.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TTVHitTestInfo;
begin
  Result := [];
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    TreeView_HitTest(Handle, HitTest);
    if (flags and TVHT_ABOVE) <> 0 then Include(Result, htAbove);
    if (flags and TVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and TVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and TVHT_ONITEM) = TVHT_ONITEM then
      Include(Result, htOnItem)
    else
    begin
      if (flags and TVHT_ONITEM) <> 0 then Include(Result, htOnItem);
      if (flags and TVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
      if (flags and TVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
      if (flags and TVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    end;
    if (flags and TVHT_ONITEMBUTTON) <> 0 then Include(Result, htOnButton);
    if (flags and TVHT_ONITEMINDENT) <> 0 then Include(Result, htOnIndent);
    if (flags and TVHT_ONITEMRIGHT) <> 0 then Include(Result, htOnRight);
    if (flags and TVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and TVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;
end;

procedure TShellTree.SetAutoExpand(Value: Boolean);
begin
  if FAutoExpand <> Value then
  begin
    FAutoExpand := Value;
    SetComCtlStyle(Self, TVS_SINGLEEXPAND, Value);
  end;
end;

procedure TShellTree.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    SetComCtlStyle(Self, TVS_TRACKSELECT, Value);
  end;
end;

procedure TShellTree.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    SetComCtlStyle(Self, TVS_FULLROWSELECT, Value);
  end;
end;

procedure TShellTree.SetToolTips(Value: Boolean);
begin
  if FToolTips <> Value then
  begin
    FToolTips := Value;
    SetComCtlStyle(Self, TVS_NOTOOLTIPS, not Value);
  end;
end;

procedure TShellTree.SetDragMode(Value: TDragMode);
begin
  if Value <> DragMode then
    SetComCtlStyle(Self, TVS_DISABLEDRAGDROP, Value = dmManual);
  inherited;
end;

procedure TShellTree.SetShowButtons(Value: Boolean);
begin
  if ShowButtons <> Value then
  begin
    FShowButtons := Value;
    SetComCtlStyle(Self, TVS_HASBUTTONS, Value);
  end;
end;

procedure TShellTree.SetHideSelection(Value: Boolean);
begin
  if HideSelection <> Value then
  begin
    FHideSelection := Value;
    SetComCtlStyle(Self, TVS_SHOWSELALWAYS, not Value);
    Invalidate;
  end;
end;

procedure TShellTree.SetIndent(Value: Integer);
begin
  if Value <> Indent then TreeView_SetIndent(Handle, Value);
end;

function TShellTree.GetIndent: Integer;
begin
  Result := TreeView_GetIndent(Handle)
end;

procedure TShellTree.SetShowLines(Value: Boolean);
begin
  if ShowLines <> Value then
  begin
    FShowLines := Value;
    SetComCtlStyle(Self, TVS_HASLINES, Value);
  end;
end;

procedure TShellTree.SetShowRoot(Value: Boolean);
begin
  if ShowRoot <> Value then
  begin
    FShowRoot := Value;
    SetComCtlStyle(Self, TVS_LINESATROOT, Value);
  end;
end;

procedure TShellTree.SetRoot(Value: TShellNode);
var
  NewRoot: TShellTreeNode;
begin
  if Value = nil then Exit;
  if (FRoot = nil) or (not ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList)) then
  begin
    NewRoot := TShellTreeNode.CreateFromList(ILClone(Value.AbsoluteList));
    TreeView_DeleteAllItems(Handle);
    FRoot.Free;
    FRoot := NewRoot;
    InsertNode(TShellTreeNode(FRoot));
    TreeView_SelectItem(Handle, TShellTreeNode(FRoot).Handle);
  end;
end;

procedure TShellTree.SetReadOnly(Value: Boolean);
begin
	FReadOnly := Value;
	SetComCtlStyle(Self, TVS_EDITLABELS, not Value);
end;

function TShellTree.GetSelectedNode: TShellNode;
begin
  Result := GetNode(TreeView_GetSelection(Handle));
end;

procedure TShellTree.SetSelectedNode(Value: TShellNode);
var
	Node: TShellTreeNode;
begin
  if Value <> nil then
    Node := FindTreeNode(TShellTreeNode(FRoot), Value)
	else
		Node := nil;
  if Node <> nil then
    TreeView_SelectItem(Handle, Node.Handle)
  else
    TForm(Owner).Caption := 'NotFound ' + FormatDateTime('hh:mm:ss', Now);
end;

procedure TShellTree.SetSpecialFolder(Value: TSpecialFolder);
begin
  if Value <> FSpecialFolder then
  begin
    FSpecialFolder := Value;
    TreeView_DeleteAllItems(Handle);
    FRoot.Free;
    FRoot := TShellTreeNode.CreateFromFolder(FSpecialFolder);
    InsertNode(TShellTreeNode(FRoot));
    SelectedNode := FRoot;
  end;
end;

{ TShellTreeNode }

procedure TShellTreeNode.Initialize;
const
  Flags = SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
  ItemList: PItemIDList;
  FileFlag: Longword;
begin
  inherited Initialize;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags);
  FImageIndex := SHFileInfo.iIcon;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo),
    Flags or SHGFI_OPENICON);
  FSelectedIndex := SHFileInfo.iIcon;
  if Parent <> nil then
  begin
    ItemList := RelativeList;
    FileFlag := SFGAO_FILESYSTEM;
    Parent.ShellFolder.GetAttributesOf(1, ItemList, FileFlag);
    FFileSystem := FileFlag and SFGAO_FILESYSTEM = SFGAO_FILESYSTEM;
  end
end;

function TShellTreeNode.GetFilePath: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if FFileSystem and SHGetPathFromIDList(AbsoluteList, Buffer) then
    Result := Buffer
  else
    Result := '';
end;

{ TShellTreePopup }

procedure TShellTreePopup.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and (not WS_BORDER or WS_HSCROLL);
    ExStyle := ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

procedure TShellTreePopup.WMNCCalcSize(var Message: TMessage);
//var
//  Style: Integer;
begin
  // Style := GetWindowLong(Handle, GWL_STYLE);
  // SetWindowLong(Handle, GWL_STYLE, Style and not (WS_BORDER or WS_HSCROLL));
  inherited;
end;

{ TShellTreePopupForm }

constructor TShellTreePopupForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShellTree := TShellTreePopup.Create(Self);
  with FShellTree do
  begin
    Parent := Self;
    SelectedNode := Root;
    OnChange := ShellTreeChange;
    OnMouseMove := ShellTreeMouseMove;
    OnMouseUp := ShellTreeMouseUp;
  end;
  ForwardControl := FShellTree;
  AdjustControlSize;
end;

procedure TShellTreePopupForm.Popup;
begin
	FSelectChanged := False;
  FHoverNode := nil;
  inherited Popup;
end;

procedure TShellTreePopupForm.DoNodeHover(Node: TShellTreeNode);
begin
	if Node <> FHoverNode then
  begin
  	FHoverNode := Node;
    if Assigned(FOnNodeHover) then
    	FOnNodeHover(Self, Node);
  end;
end;

procedure TShellTreePopupForm.ShellTreeChange(Sender: TObject; Node: TShellNode);
begin
	FSelectChanged := True;
end;

procedure TShellTreePopupForm.ShellTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
var
	Node: TShellTreeNode;
begin
	Node := FShellTree.GetNodeAt(X, Y);
  if Node <> nil then
  	DoNodeHover(Node);
end;

procedure TShellTreePopupForm.ShellTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if FSelectChanged then
  	Select;
end;

{ TShellEdit }

procedure HideSuggest;
const
  AutoClass = 'Auto-Suggest Dropdown';
var
  Wnd: HWND;
begin
  Wnd := FindWindow(AutoClass, nil);
  if Wnd <> 0 then
  	ShowWindow(Wnd, SW_HIDE);
end;

constructor TShellEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  AutoHeight := False;
  Images := ShellImageList;
	FShellTreePopupForm := TShellTreePopupForm.Create(Self);
  FShellTreePopupForm.OnNodeHover := TreeNodeHover;
  if @RegisterShellUnits <> nil then
    RegisterShellUnits(AOwner);
end;

function TShellEdit.CreatePopup: TCustomPopupForm;
begin
	Result := FShellTreePopupForm;
end;

procedure TShellEdit.CreateWnd;
begin
	inherited CreateWnd;
  FShellTreePopupForm.Height := 125;
  SelectionChanged;
end;

procedure TShellEdit.TreeNodeHover(Sender: TObject; Node: TShellNode);
begin
	if Assigned(FOnNodeHover) then
  	FOnNodeHover(Self, Node);
end;

procedure TShellEdit.SelectionChanged;
var
	Node: TShellTreeNode;
  Folder: IPersistFolder;
begin
	Node := TShellTreeNode(FShellTreePopupForm.FShellTree.SelectedNode);
  if Node <> nil then
	begin
  	ImageIndex := Node.ImageIndex;
    if Node.FileSystem and ((FOptions * [soShortName] = []) or (ILGetCount(Node.AbsoluteList) < 3)) then
  		Text := Node.Path
  	else
  		Text := Node.Name;
    SelStart := 0;
    if FStrings <> nil then
	    if Supports(FStrings, IPersistFolder, Folder) then
      	Folder.Initialize(Node.AbsoluteList);
	end;
end;

procedure TShellEdit.DoPlaceEdit(var Rect: TRect);
begin
	inherited DoPlaceEdit(Rect);
  Inc(Rect.Top);
  Rect.Bottom := Rect.Bottom - 2;
end;

procedure TShellEdit.DoPopup;
begin
  if FAutoComplete <> nil then
  begin
    FAutoComplete.Enable(False);
    HideSuggest;
  end;
  inherited DoPopup;
end;

procedure TShellEdit.DoCancel(Sender: TObject);
begin
  if FAutoComplete <> nil then
    FAutoComplete.Enable(True);
  inherited DoCancel(Sender);
end;

procedure TShellEdit.DoSelect(Sender: TObject);
begin
	SelectionChanged;
  if FAutoComplete <> nil then
    FAutoComplete.Enable(True);
	inherited DoSelect(Sender);
end;

procedure TShellEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
	Item: PItemIDList;
  Node, PriorNode: TShellNode;
  S: string;
begin
  inherited KeyDown(Key, Shift);
	if Key = VK_RETURN then
  begin
    if SelectedNode = nil then
      S := ''
    else
      S := SlashedPath(SelectedNode.Path);
    Item := ILCreateFromPath(S + Trim(Text));
    if Item = nil then
  		Item := ILCreateFromPath(Text);
    if Item <> nil then
    begin
    	Node := TShellNode.CreateFromList(Item);
      if Node.ShellFolder <> nil then
      begin
        PriorNode := SelectedNode;
        SelectedNode := Node;
        Node.Free;
        Node := nil;
        if SelectedNode <> PriorNode then
          if Assigned(OnSelect) then
            OnSelect(Self);
      end;
      Node.Free;
    end;
  end
  else if Key = VK_ESCAPE then
    SelectionChanged;
end;

function TShellEdit.GetSelectedNode: TShellNode;
begin
	Result := FShellTreePopupForm.FShellTree.SelectedNode;
end;

procedure TShellEdit.SetSelectedNode(Value: TShellNode);
begin
	FShellTreePopupForm.FShellTree.SelectedNode := Value;
  SelectionChanged;
end;

function TShellEdit.GetRoot: TShellNode;
begin
	Result := FShellTreePopupForm.FShellTree.Root;
end;

procedure TShellEdit.SetRoot(Value: TShellNode);
begin
	FShellTreePopupForm.FShellTree.Root := Value;
	SelectionChanged;
end;

function TShellEdit.GetHotTrack: Boolean;
begin
	Result := FShellTreePopupForm.FShellTree.HotTrack;
end;

procedure TShellEdit.SetHotTrack(Value: Boolean);
begin
	FShellTreePopupForm.FShellTree.HotTrack := Value;
end;

procedure TShellEdit.SetOptions(Value: TShellEditOptions);

  function Includes(ParentSet, ChildSet: TShellEditOptions): Boolean;
  begin
    Result := ParentSet * ChildSet = ChildSet;
  end;

  function OptionChanged(ChildSet: TShellEditOptions): Boolean;
  begin
    Result := not ((Value * ChildSet = ChildSet) = (FOptions * ChildSet = ChildSet));
  end;

begin
  if Value <> FOptions then
  begin
    if OptionChanged([soSuggest]) then
      if Includes(Value, [soSuggest]) then
      begin
      	FAutoComplete := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
        FStrings := CreateComObject(CLSID_ACListISF);
        OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
        OleCheck(FAutoComplete.Init(EditHandle, FStrings, nil, nil));
      end
      else
      begin
        FAutoComplete.Enable(False);
        FAutoComplete := nil;
        FStrings := nil;
        HideSuggest;
      end;
    FOptions := Value;
    SelectionChanged;
  end;
end;

function TShellEdit.GetStatusText: string;
begin
	Result := FShellTreePopupForm.StatusText;
end;

procedure TShellEdit.SetStatusText(Value: string);
begin
	FShellTreePopupForm.StatusText := Value;
end;

function TShellEdit.GetSpecialFolder: TSpecialFolder;
begin
	Result := FShellTreePopupForm.FShellTree.SpecialFolder;
end;

procedure TShellEdit.SetSpecialFolder(Value: TSpecialFolder);
begin
	FShellTreePopupForm.FShellTree.SpecialFolder := Value;
  SelectionChanged;
end;

{ TShellView }

const
	ViewModes: array[TShellViewMode] of UINT = (
		FVM_ICON, FVM_SMALLICON, FVM_LIST, FVM_DETAILS, FVM_THUMBNAIL,
		FVM_TILE, FVM_THUMBSTRIP);

constructor TShellView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  TabStop := True;
  ParentColor := False;
  Color := clWindow;
  FBorderStyle := bsSingle;
  FListViewInstance := MakeObjectInstance(ListViewWndProc);
  FShellViewInstance := MakeObjectInstance(ShellViewWndProc);
  FRoot := TShellNode.CreateFromFolder(FSpecialFolder);
end;

destructor TShellView.Destroy;
begin
  if FShellView <> nil then
  begin
    SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FDefShellViewProc));
    SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FDefListViewProc));
    FShellView.DestroyViewWindow;
    FShellView := nil;
  end;
  FStream := nil;
  FRoot.Free;
  FreeObjectInstance(FListViewInstance);
  FreeObjectInstance(FShellViewInstance);
  inherited Destroy;
end;

procedure TShellView.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
end;

procedure TShellView.CreateWnd;
var
  Node: TShellNode;
begin
  inherited CreateWnd;
  Node := TShellNode.CreateFromFolder(FSpecialFolder);
  try
    Root := Node;
  finally
    Node.Free;
  end;
end;

procedure TShellView.Resize;
begin
  if FShellView = nil then
    Invalidate;
end;

procedure TShellView.DoViewChanged;
begin
	if Assigned(FOnViewChanged) then
  	FOnViewChanged(Self);
end;

procedure TShellView.ListViewWndProc(var Message: TMessage);
var
  Form: TCustomForm;
begin
  with Message do
  try
    case Msg of
      WM_SETFOCUS:
        begin
          Form := GetParentForm(Self);
          if (Form <> nil) and Enabled and (not Form.SetFocusedControl(Self))
            then Exit;
        end;
      WM_KILLFOCUS:
        if csFocusing in ControlState then Exit;
      WM_KEYDOWN, WM_SYSKEYDOWN:
        if DoKeyDown(TWMKey(Message)) then Exit;
      WM_CHAR:
        if DoKeyPress(TWMKey(Message)) then Exit;
      WM_KEYUP, WM_SYSKEYUP:
        if DoKeyUp(TWMKey(Message)) then Exit;
      WM_NCHITTEST:
        if csDesigning in ComponentState then
        begin
          Result := HTTRANSPARENT;
          Exit;
        end;
      WM_MOUSEMOVE:
        begin
          with TWMMouseMove(Message) do MouseMove(KeysToShiftState(Keys),
            XPos, YPos);
          Application.HintMouseMessage(Self, Message);
        end;
      CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN, CN_SYSCHAR:
        begin
          WndProc(Message);
          Exit;
        end;
    end;
    Result := CallWindowProc(FDefListViewProc, FListViewHandle, Msg, WParam, LParam);
    if (Msg = WM_LBUTTONDBLCLK) and (csDoubleClicks in ControlStyle) then
      DblClick;
  except
    Application.HandleException(Self);
  end;
end;

procedure TShellView.ShellViewWndProc(var Message: TMessage);
const
  DefaultMenu = $7900;
var
  MenuItemInfo: TMenuItemInfo;
begin
  with Message do
  try
    case Msg of
      WM_NCHITTEST:
        if csDesigning in ComponentState then
        begin
          Result := HTTRANSPARENT;
          Exit;
        end;
      WM_INITMENUPOPUP:
        with TWMInitMenuPopup(Message) do
          if GetMenuItemID(MenuPopup, 0) = DefaultMenu then
          begin
            RemoveMenu(MenuPopup, 0, MF_BYPOSITION);
            RemoveMenu(MenuPopup, 0, MF_BYPOSITION);
            FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), #0);
            with MenuItemInfo do
            begin
              cbSize := SizeOf(TMenuItemInfo);
              fMask := MIIM_STATE or MIIM_ID;
              fState := MFS_DEFAULT;
              wID := DefaultMenu;
            end;
            SetMenuItemInfo(MenuPopup, 0, True, MenuItemInfo);
          end;
    end;
    Result := CallWindowProc(FDefShellViewProc, FShellViewHandle, Msg, WParam,
      LParam);
  except
    Application.HandleException(Self);
  end;
end;

function TShellView.GetFocusedNode: TShellNode;
var
  F: IFolderView;
  L: PItemIDList;
  I: Integer;
begin
	Result := nil;
	if FShellView = nil then
    Exit;
	if Supports(FShellView, IFolderView, F) and (F.GetFocusedItem(I) = S_OK) and
		(F.Item(I, L) = S_OK) and (L <> nil) then
	begin
    if FFocusedNode = nil then
      FFocusedNode := TShellNode.Create(FRoot.Clone, L)
    else if ILIsEqual(FFocusedNode.RelativeList, L) then
	  	ILFree(L)
    else
    begin
    	FFocusedNode.Free;
  	  FFocusedNode := nil;
	  	FFocusedNode := TShellNode.Create(FRoot.Clone, L);
    end;
    Result := FFocusedNode;
  end;
end;

procedure TShellView.Up;
var
	Node: TShellNode;
	L: PItemIDList;
begin
	if ILIsRoot(FRoot.AbsoluteList) then Exit;
	L := ILClone(FRoot.AbsoluteList);
  if ILRemoveLastID(L) then
		Node := TShellNode.CreateFromList(L)
	else
		Node := TShellNode.CreateFromList(nil);
	Root := Node;
  Node.Free;
end;

procedure TShellView.SetBorderStyle(Value: TBorderStyle);
begin
	if Value <> FBorderStyle then
  begin
  	FBorderStyle := Value;
    if HandleAllocated then
    begin
    	SendMessage(Handle, WM_SIZE, Width, Height);
			Invalidate;
    end;
  end;
end;

procedure TShellView.SetRoot(Value: TShellNode);
var
	WasFocused: Boolean;
  FolderSettings: TFolderSettings;
  ViewRect: TRect;
  OldView: IShellView;
  Edge: Integer;
begin
	if Value = nil then Exit;
	if Value.ShellFolder = nil then Exit;
	if IsWindow(FShellViewHandle) and ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList) then Exit;
  if HandleAllocated and (Value <> FRoot) then
  begin
    FRoot.Assign(Value);
  	FFocusedNode.Free;
    FFocusedNode := nil;
    WasFocused := IsChild(Handle, Windows.GetFocus);
    OldView := FShellView;
    if OldView <> nil then
    begin
      SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FDefShellViewProc));
      SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FDefListViewProc));
    end;
    OleCheck(CreateStreamOnHGlobal(GlobalAlloc(GMEM_MOVEABLE or
      GMEM_DISCARDABLE, 0), True, FStream));
    OleCheck(FRoot.ShellFolder.CreateViewObject(Handle,
      IID_IShellView, FShellView));
  	with FolderSettings do
  	begin
    	ViewMode := ViewModes[FViewMode];
  	  fFlags := FWF_NOCLIENTEDGE;
	  end;
		if BorderStyle = bsSingle then
	    Edge := GetSystemMetrics(SM_CXEDGE)
		else
	    Edge := 0;
    ViewRect := Rect(Edge, Edge, ClientWidth - Edge, ClientHeight - Edge);
    OleCheck(FShellView.CreateViewWindow(OldView, FolderSettings,  Self as
      IShellBrowser, ViewRect, FShellViewHandle));
    FShellView.UIActivate(1);
    FListViewHandle := FindWindowEx(FShellViewHandle, 0, WC_LISTVIEW, nil);
    FDefListViewProc := Pointer(GetWindowLong(FListViewHandle, GWL_WNDPROC));
    SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FListViewInstance));
    FDefShellViewProc := Pointer(GetWindowLong(FShellViewHandle, GWL_WNDPROC));
    SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FShellViewInstance));
    if WasFocused then
	    Windows.SetFocus(FShellViewHandle);
		if OldView <> nil then
      OldView.DestroyViewWindow;
    {if csDesigning in ComponentState then
    begin
      ShowWindow(FShellViewHandle, SW_HIDE);
      ShowWindow(FListViewHandle, SW_HIDE);
    end;}
  end;
end;

procedure TShellView.SetSpecialFolder(Value: TSpecialFolder);
var
  Node: TShellNode;
begin
  if Value <> FSpecialFolder then
  begin
    FSpecialFolder := Value;
    Node := TShellNode.CreateFromFolder(FSpecialFolder);
    try
      Root := Node;
    finally
      Node.Free;
    end;
  end;
end;

procedure TShellView.SetViewMode(Value: TShellViewMode);
var
	Changed: Boolean;
	F: IFolderView;
begin
	Changed := Value <> FViewmode;
	FViewMode := Value;
  if FShellView <> nil then
  begin
  	if Supports(FShellView, IFolderView, F) then
    	F.SetCurrentViewMode(ViewModes[Value]);
  end;
  if Changed then
  	DoViewChanged;
end;

{ TShellView.IOleWindow }

function TShellView.GetWindow(out wnd: HWnd): HResult;
begin
  Wnd := Handle;
  Result := S_OK;
end;

function TShellView.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := S_OK;
end;

{ TShellView.IShellBrowser }

function TShellView.InsertMenusSB(hMenuShared: HMENU;
  out MenuWidths: TOleMenuGroupWidths): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetMenuSB(hMenuShared: HMENU;
  hOleMenuReserved: HOLEMENU; hwndActiveObject: HWND): HResult;
begin
  Result := S_OK;
end;

function TShellView.RemoveMenusSB(hMenuShared: HMENU): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetStatusTextSB(StatusText: POleStr): HResult;
begin
  Result := S_OK;
end;

function TShellView.EnableModelessSB(Enable: BOOL): HResult;
begin
  Result := S_OK;
end;

function TShellView.TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult;
begin
  Result := S_OK;
end;

function TShellView.BrowseObject(pidl: PItemIDList; flags: UINT): HResult;
begin
  Result := S_OK;
end;

function TShellView.GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult;
begin
  Stream := FStream;
  Result := S_OK;
end;

function TShellView.GetControlWindow(ID: UINT; out Wnd: HWND): HResult;
begin
  Wnd := 0;
  case ID of
    FCW_STATUS:
      if FStatusBar <> nil then
        Wnd := FStatusBar.Handle;
    { FCW_TREE:
      if FShellTree <> nil then
        Wnd := FShellTree.Handle; }
  end;
  Result := S_OK;
end;

function TShellView.SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM;
  var Rslt: LResult): HResult;
begin
  Result := S_OK;
end;

function TShellView.QueryActiveShellView(var ShellView: IShellView): HResult;
begin
  ShellView := FShellView;
  Result := S_OK;
end;

function TShellView.OnViewWindowActive(var ShellView: IShellView): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetToolbarItems(TBButton: PTBButton;
  nButtons, uFlags: UINT): HResult;
begin
  Result := S_OK;
end;

{ TShellView.ICommDlgBrowser }

function TShellView.OnDefaultCommand(const ppshv: IShellView): HResult;
var
	AllowAction: Boolean;
  Node: TShellNode;
begin
	AllowAction := True;
  if Assigned(FOnDefaultAction) then
  try
  	Node := GetFocusedNode;
    if Node <> nil then
    try
    	Node := Node.Clone;
			FOnDefaultAction(Self, Node, AllowAction);
    finally
    	Node.Free;
    end;
  except
    Application.HandleException(Self);
  end;
  if AllowAction then
	  Result := S_FALSE
	else
	  Result := S_OK;
end;

function TShellView.OnStateChange(const ppshv: IShellView; Change: ULONG): HResult;
begin
  Result := S_OK;
end;

function TShellView.IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult;
var
	AllowAction: Boolean;
  Node: TShellNode;
begin
	AllowAction := True;
	if Assigned(FOnIncludeItem) then
	try
    Node := TShellNode.Create(FRoot.Clone, ILClone(pidl));
    try
      FOnIncludeItem(Self, Node, AllowAction);
    finally
      Node.Free;
    end;
  except
    Application.HandleException(Self);
  end;
  if AllowAction then
	  Result := S_OK
	else
	  Result := S_FALSE;
end;

{ TShellView.ICommDlgBrowser2 }

function TShellView.Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult;
var
	View: TShellViewMode;
	F: IFolderView;
	M: UINT;
begin
	if dwNotifyType = CDB2N_CONTEXTMENU_DONE then
		if Supports(ppshv, IFolderView, F) and (F.GetCurrentViewMode(M) = S_OK) and
    	(M <> ViewModes[FViewMode]) then
		for View := Low(View) to High(View) do
    	if ViewModes[View] = M then
	    begin
  	    FViewMode := View;
        try
      		DoViewChanged;
        except
    			Application.HandleException(Self);
  			end;
				Break;
  		end;
	Result := S_OK;
end;

function TShellView.GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
	cchMax: Integer): HResult;
begin
	FillChar(pszText^, 2, #0);
	Result := S_OK;
end;

function TShellView.GetViewFlags(out pdwFlags: DWORD): HResult;
begin
	pdwFlags := CDB2GVF_SHOWALLFILES;
	Result := S_OK;
end;

procedure TShellView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TShellView.WMGetIShellBrowser(var Message: TMessage);
begin
  if TShellTreeNode(FRoot).FileSystem then
    Message.Result := Integer(Self as IShellBrowser)
  else
    Message.Result := 0;
end;

procedure TShellView.WMPaint(var Message: TWMPaint);
var
	PS: TPaintStruct;
  Rect: TRect;
begin
	if GetUpdateRect(Handle, Rect, False) then
  begin
  	BeginPaint(Handle, PS);
		Rect := Classes.Rect(0, 0, Width, Height);
		DrawThemeBorder(PS.hdc, clWindow, Rect, []);
    EndPaint(Handle, PS);
  end;
  Message.Result := 0;
end;

procedure TShellView.WMSize(var Message: TWMSize);
var
	Edge: Integer;
begin
	if FBorderStyle = bsSingle then
  	Edge := GetSystemMetrics(SM_CXEDGE)
	else
  	Edge := 0;
  if FShellViewHandle <> 0 then
		with Message do
			SetWindowPos(FShellViewHandle, 0, Edge, Edge, Width - Edge,
				Height - Edge, SWP_NOZORDER or SWP_NOACTIVATE);
  inherited;
end;

{ TShellBubbleItem }

constructor TShellBubbleItem.Create(Collection: TCollection);
begin
  CreateFromFolder(Collection, sfDesktop);
end;

constructor TShellBubbleItem.CreateFromFolder(Collection: TCollection;
  SpecialFolder: TSpecialFolder);
begin
  inherited Create(Collection);
  FNode := TShellImageNode.CreateFromFolder(SpecialFolder);
  FSpecialFolder := SpecialFolder;
end;

destructor TShellBubbleItem.Destroy;
begin
  FNode.Free;
  inherited Destroy;
end;

procedure TShellBubbleItem.Assign(Source: TPersistent);
var
  Item: TShellBubbleItem absolute Source;
  HasChanged: Boolean;
begin
  if Source is TShellBubbleItem then
  begin
    HasChanged := not FNode.IsEqual(Item.Node);
    if HasChanged then
    begin
      FNode.Free;
      FNode := TShellImageNode.CreateFromList(ILClone(Item.Node.AbsoluteList));
    end;
    FPath := Item.Path;
    FSpecialFolder := Item.SpecialFolder;
    if HasChanged then
      TShellBubbleItems(Collection).Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TShellBubbleItem.SetPath(const Value: string);
var
  pidl: PItemIDList;
begin
  if FPath <> Value then
  begin
    FPath := Value;
    FNode.Free;
    if FPath <> '' then
      pidl := ILCreateFromPath(FPath)
    else
      pidl := nil;
    if pidl = nil then
      FNode := TShellImageNode.CreateFromFolder(FSpecialFolder)
    else
      FNode := TShellImageNode.CreateFromList(pidl);
    TShellBubbleItems(Collection).Changed;
  end;
end;

procedure TShellBubbleItem.SetSpecialFolder(Value: TSpecialFolder);
var
  pidl: PItemIDList;
begin
  if FSpecialFolder <> Value then
  begin
    FSpecialFolder := Value;
    if FPath <> '' then
      pidl := ILCreateFromPath(FPath)
    else
      pidl := nil;
    if pidl = nil then
    begin
      FNode.Free;
      FNode := TShellImageNode.CreateFromFolder(FSpecialFolder);
      TShellBubbleItems(Collection).Changed;
    end
    else
      ILFree(pidl);
  end;
end;

{ TShellBubbleItems }

constructor TShellBubbleItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TShellBubbleItem);
end;

function TShellBubbleItems.Add: TShellBubbleItem;
begin
  Result := TShellBubbleItem.Create(Self);
end;

function TShellBubbleItems.AddFolder(SpecialFolder: TSpecialFolder): TShellBubbleItem;
begin
  Result := TShellBubbleItem.CreateFromFolder(Self, SpecialFolder);
end;

procedure TShellBubbleItems.Assign(Source: TPersistent);
var
  Items: TShellBubbleItems absolute Source;
  I: Integer;
begin
  if Source is TShellBubbleItems then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to Items.Count - 1 do
      Add.Assign(Items[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

procedure TShellBubbleItems.Update(Item: TCollectionItem);
begin
  if GetOwner is TControl then
    (GetOwner as TControl).Update;
end;

function TShellBubbleItems.GetItem(Index: Integer): TShellBubbleItem;
begin
  Result := TShellBubbleItem(inherited Items[Index]);
end;

procedure TShellBubbleItems.SetItem(Index: Integer; Value: TShellBubbleItem);
begin
  GetItem(Index).Assign(Value);
end;

{ TShellBubbles }

constructor TShellBubbles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TShellBubbleItems.Create(Self);
  Style := sbLarge;
  Width := 79;
  Height := 204;
end;

destructor TShellBubbles.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TShellBubbles.CreateWnd;
begin
  inherited CreateWnd;
  if (csDesigning in ComponentState) and (Items.Count = 0) then
  begin
    Items.AddFolder(sfRecent);
    Items.AddFolder(sfDesktop);
    Items.AddFolder(sfDrives);
    Items.AddFolder(sfHistory);
  end;
end;

procedure TShellBubbles.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Images: TCustomImageList;
  DrawRect: TRect;
  H, W: Integer;
  S: TSize;
begin
  inherited DrawItem(Index, Rect, State);
  Images := ShellImageList(FStyle = sbSmall);
  DrawRect := Rect;
  H := HeightOf(Rect);
  W := WidthOf(Rect);
    case FStyle of
      sbSmall:
        if FTextLabels then
        begin
          Images.Draw(Canvas, DrawRect.Left + (H - 16) div 2,
            DrawRect.Top + (H - 16) div 2, Items[Index].Node.SmallIndex);
          Inc(DrawRect.Left, H);
          Dec(DrawRect.Right, 5);
          DrawCaption(Canvas.Handle, Items[Index].Node.Name, DrawRect, drLeft, Enabled);
        end
        else
          Images.Draw(Canvas, DrawRect.Left + (W - 16) div 2,
            DrawRect.Top + (H - 16) div 2, Items[Index].Node.SmallIndex);
      sbLarge:
        if FTextLabels then
        begin
          S := CalculateCaptionSize(Canvas.Handle, ' ');
          Images.Draw(Canvas, DrawRect.Left + (W - 32) div 2,
            DrawRect.Top + (H - 32) div 2 - S.cY, Items[Index].Node.LargeIndex);
          DrawRect.Top := DrawRect.Top + (H - 32) div 2 + 22;
          DrawCaption(Canvas.Handle, Items[Index].Node.Name, DrawRect, drWrap, Enabled);
        end
        else
          Images.Draw(Canvas, DrawRect.Left + (W - 32) div 2,
            DrawRect.Top + (H - 32) div 2, Items[Index].Node.LargeIndex);
    end;

end;

procedure TShellBubbles.Update;
begin
  inherited Update;
  if not (csDestroying in ComponentState) then
  begin
    Count := FItems.Count;
    Invalidate;
  end;
end;

procedure TShellBubbles.SetItems(Value: TShellBubbleItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

const
  LargeHeights: array[Boolean] of Integer = (50, 75);

procedure TShellBubbles.SetStyle(Value: TShellBubbleStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    case FStyle of
      sbSmall: ItemHeight := 25;
      sbLarge: ItemHeight := LargeHeights[FTextLabels];
    end;
  end;
end;

procedure TShellBubbles.SetTextLabels(Value: Boolean);
begin
  if Value <> FTextLabels then
  begin
    FTextLabels := Value;
    if FStyle = sbLarge then
      ItemHeight := LargeHeights[FTextLabels];
    Invalidate;
  end;
end;

procedure InitializeImages;
const
  AppNames: array[0..25] of string = (
    'cmd.exe', 'ahui.exe', 'calc.exe', 'charmap.exe', 'cleanmgr.exe',
    'magnify.exe', 'migpwd.exe', 'mobsync.exe', 'narrator.exe', 'mstsc.exe',
    'osk.exe', 'osuninst.exe', 'rasphone.exe', 'rtcshare.exe', 'sndrec32.exe',
    'sndvol32.exe', 'tourstart.exe', 'utilman.exe', 'wiaacmgr.exe', 'wuauclt1.exe',
    'notepad.exe', 'winmine.exe', 'freecell.exe', 'mshearts.exe', 'sol.exe',
    'spider.exe');
var
  Folder: TSpecialFolder;
  S: string;
  I: Integer;
begin
  for Folder := Low(Folder) to High(Folder) do
    TShellImageNode.LoadImage(Folder);
  S := GetSystemPath;
  for I := Low(AppNAmes) to High(AppNames) do
    TShellImageNode.LoadImage(S + AppNames[I]);
end;

initialization
  InitializeImages;
end.
