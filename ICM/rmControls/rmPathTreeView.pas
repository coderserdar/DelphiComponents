{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmPathTreeView
Purpose  : Visual treeview that supports operations on nodes via path structures
Date     : 08-15-2000
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmPathTreeView;

interface

{$I CompilerDefines.INC}

uses
   Windows, Messages, controls, classes, commctrl, comctrls, forms, ImgList,
   extctrls, sysutils, Graphics, Contnrs;

type
   TrmCustomPathTreeView = class;
   TrmTreeNodes = class;
   TrmTreeNode = class;

   TrmNodeInfo = class(TComponent)
   private
      fImageIndex: Integer;
      fSelectedIndex: Integer;
      fStateIndex: Integer;
      fOverlayIndex: Integer;
      fData: Integer;
      fPath: string;
   published
      property ImageIndex: integer read fImageIndex write fImageIndex default -1;
      property SelectedIndex: integer read fSelectedIndex write fSelectedIndex default -1;
      property StateIndex: integer read fStateIndex write fStateIndex default -1;
      property OverlayIndex: integer read fOverlayIndex write fOverlayIndex default -1;
      property Data: Integer read fData write fData;
      property Path: String read fPath write fPath;
   end;

{ TrmCustomPathTreeView Events }

   TrmTVChangingEvent = procedure(Sender: TObject; Node: TrmTreeNode;
      var AllowChange: Boolean) of object;
   TrmTVChangedEvent = procedure(Sender: TObject; Node: TrmTreeNode) of object;
   TrmTVEditingEvent = procedure(Sender: TObject; Node: TrmTreeNode;
      var AllowEdit: Boolean) of object;
   TrmTVEditedEvent = procedure(Sender: TObject; Node: TrmTreeNode; var S: string) of object;
   TrmTVExpandingEvent = procedure(Sender: TObject; Node: TrmTreeNode;
      var AllowExpansion: Boolean) of object;
   TrmTVCollapsingEvent = procedure(Sender: TObject; Node: TrmTreeNode;
      var AllowCollapse: Boolean) of object;
   TrmTVExpandedEvent = procedure(Sender: TObject; Node: TrmTreeNode) of object;
   TrmTVCompareEvent = procedure(Sender: TObject; Node1, Node2: TrmTreeNode;
      Data: Integer; var Compare: Integer) of object;
   TrmTVCustomDrawEvent = procedure(Sender: TrmCustomPathTreeView; const ARect: TRect;
      var DefaultDraw: Boolean) of object;
   TrmTVCustomDrawItemEvent = procedure(Sender: TrmCustomPathTreeView; Node: TrmTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean) of object;
   TrmTVAdvancedCustomDrawEvent = procedure(Sender: TrmCustomPathTreeView; const ARect: TRect;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
   TrmTVAdvancedCustomDrawItemEvent = procedure(Sender: TrmCustomPathTreeView; Node: TrmTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean) of object;

{ TrmTreeNode }

   TrmTreeNode = class(TPersistent)
   private
      FOwner: TrmTreeNodes;
      FText: string;
      FData: Pointer;
      FItemId: HTreeItem;
      FImageIndex: TImageIndex;
      FSelectedIndex: Integer;
      FOverlayIndex: Integer;
      FStateIndex: Integer;
      FDeleting: Boolean;
      FInTree: Boolean;

      function CompareCount(CompareMe: Integer) : Boolean;
      function DoCanExpand(Expand: Boolean) : Boolean;
      procedure DoExpand(Expand: Boolean) ;
      procedure ExpandItem(Expand: Boolean; Recurse: Boolean) ;
      function GetAbsoluteIndex: Integer;
      function GetExpanded: Boolean;
      function GetLevel: Integer;
      function GetParent: TrmTreeNode;
      function GetChildren: Boolean;
      function GetCut: Boolean;
      function GetDropTarget: Boolean;
      function GetFocused: Boolean;
      function GetIndex: Integer;
      function GetItem(Index: Integer) : TrmTreeNode;
      function GetSelected: Boolean;
      function GetState(NodeState: TNodeState) : Boolean;
      function GetCount: Integer;
      function GetTreeView: TrmCustomPathTreeView;
      procedure InternalMove(ParentNode, Node: TrmTreeNode; HItem: HTreeItem;
         AddMode: TAddMode) ;
      function IsEqual(Node: TrmTreeNode) : Boolean;
      function IsNodeVisible: Boolean;
      procedure ReadData(Stream: TStream; Info: PNodeInfo) ;
      procedure SetChildren(Value: Boolean) ;
      procedure SetCut(Value: Boolean) ;
      procedure SetData(Value: Pointer) ;
      procedure SetDropTarget(Value: Boolean) ;
      procedure SetItem(Index: Integer; Value: TrmTreeNode) ;
      procedure SetExpanded(Value: Boolean) ;
      procedure SetFocused(Value: Boolean) ;
      procedure SetImageIndex(Value: TImageIndex) ;
      procedure SetOverlayIndex(Value: Integer) ;
      procedure SetSelectedIndex(Value: Integer) ;
      procedure SetSelected(Value: Boolean) ;
      procedure SetStateIndex(Value: Integer) ;
      procedure SetText(const S: string) ;
      procedure WriteData(Stream: TStream; Info: PNodeInfo) ;
      procedure RemoveHash;
      procedure RenewHash;
      procedure SetParent(const Value: TrmTreeNode);
   public
      constructor Create(AOwner: TrmTreeNodes) ;
      destructor Destroy; override;
      function AlphaSort: Boolean;
      procedure Assign(Source: TPersistent) ; override;
      procedure Collapse(Recurse: Boolean) ;
      function CustomSort(SortProc: TTVCompare; Data: Longint) : Boolean;
      procedure Delete;
      procedure DeleteChildren;
      function DisplayRect(TextOnly: Boolean) : TRect;
      function EditText: Boolean;
      procedure EndEdit(Cancel: Boolean) ;
      procedure Expand(Recurse: Boolean) ;
      function getFirstChild: TrmTreeNode; {GetFirstChild conflicts with C++ macro}
      function GetHandle: HWND;
      function GetLastChild: TrmTreeNode;
      function GetNext: TrmTreeNode;
      function GetNextChild(Value: TrmTreeNode) : TrmTreeNode;
      function getNextSibling: TrmTreeNode; {GetNextSibling conflicts with C++ macro}
      function GetNextVisible: TrmTreeNode;
      function GetPrev: TrmTreeNode;
      function GetPrevChild(Value: TrmTreeNode) : TrmTreeNode;
      function getPrevSibling: TrmTreeNode; {GetPrevSibling conflicts with a C++ macro}
      function GetPrevVisible: TrmTreeNode;
      function HasAsParent(Value: TrmTreeNode) : Boolean;
      function IndexOf(Value: TrmTreeNode) : Integer;
      procedure MakeVisible;
      procedure MoveTo(Destination: TrmTreeNode; Mode: TNodeAttachMode) ; virtual;
      property AbsoluteIndex: Integer read GetAbsoluteIndex;
      property Count: Integer read GetCount;
      property Cut: Boolean read GetCut write SetCut;
      property Data: Pointer read FData write SetData;
      property Deleting: Boolean read FDeleting;
      property Focused: Boolean read GetFocused write SetFocused;
      property DropTarget: Boolean read GetDropTarget write SetDropTarget;
      property Selected: Boolean read GetSelected write SetSelected;
      property Expanded: Boolean read GetExpanded write SetExpanded;
      property Handle: HWND read GetHandle;
      property HasChildren: Boolean read GetChildren write SetChildren;
      property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
      property Index: Integer read GetIndex;
      property IsVisible: Boolean read IsNodeVisible;
      property Item[Index: Integer]: TrmTreeNode read GetItem write SetItem; default;
      property ItemId: HTreeItem read FItemId;
      property Level: Integer read GetLevel;
      property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex;
      property Owner: TrmTreeNodes read FOwner;
      property Parent: TrmTreeNode read GetParent write SetParent;
      property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
      property StateIndex: Integer read FStateIndex write SetStateIndex;
      property Text: string read FText write SetText;
      property TreeView: TrmCustomPathTreeView read GetTreeView;
   end;

{ TrmTreeNodes }

   TrmHashData = class(TObject)
      Hash: longint;
      IDLength: Integer;
      Node: TrmTreeNode;
   end;

   PrmNodeCache = ^TrmNodeCache;
   TrmNodeCache = record
      CacheNode: TrmTreeNode;
      CacheIndex: Integer;
   end;

   TrmTreeNodes = class(TPersistent)
   private
      FOwner: TrmCustomPathTreeView;
      FUpdateCount: Integer;
      FNodeCache: TrmNodeCache;
      FHashList: TObjectList;
      procedure AddedNode(Value: TrmTreeNode) ;
      function GetHandle: HWND;
      function GetNodeFromIndex(Index: Integer) : TrmTreeNode;
      procedure ReadData(Stream: TStream) ;
      procedure Repaint(Node: TrmTreeNode) ;
      procedure WriteData(Stream: TStream) ;
      procedure ClearCache;
      procedure WriteExpandedState(Stream: TStream) ;
      procedure ReadExpandedState(Stream: TStream) ;
    //Path Index Hashing
      function HashValue(St: string) : LongInt;
      function LocateHashIndex(Path: string) : integer;
      procedure BinaryInsert(Path: string; Node: TrmTreeNode) ;
      procedure RemoveHash(Node: TrmTreeNode) ;
   protected
      function AddItem(Parent, Target: HTreeItem; const Item: TTVItem;
         AddMode: TAddMode) : HTreeItem;
      function InternalAddObject(Node: TrmTreeNode; const S: string;
         Ptr: Pointer; AddMode: TAddMode) : TrmTreeNode;
      procedure DefineProperties(Filer: TFiler) ; override;
      function CreateItem(Node: TrmTreeNode) : TTVItem;
      function GetCount: Integer;
      procedure SetItem(Index: Integer; Value: TrmTreeNode) ;
      procedure SetUpdateState(Updating: Boolean) ;
   public
      procedure DumpHash;
      constructor Create(AOwner: TrmCustomPathTreeView) ;
      destructor Destroy; override;
      function AddChildFirst(Node: TrmTreeNode; const S: string) : TrmTreeNode;
      function AddChild(Node: TrmTreeNode; const S: string) : TrmTreeNode;
      function AddChildObjectFirst(Node: TrmTreeNode; const S: string;
         Ptr: Pointer) : TrmTreeNode;
      function AddChildObject(Node: TrmTreeNode; const S: string;
         Ptr: Pointer) : TrmTreeNode;
      function AddFirst(Node: TrmTreeNode; const S: string) : TrmTreeNode;
      function Add(Node: TrmTreeNode; const S: string) : TrmTreeNode;
      function AddObjectFirst(Node: TrmTreeNode; const S: string;
         Ptr: Pointer) : TrmTreeNode;
      function AddObject(Node: TrmTreeNode; const S: string;
         Ptr: Pointer) : TrmTreeNode;
      procedure Assign(Source: TPersistent) ; override;
      procedure BeginUpdate;
      procedure Clear;
      procedure Delete(Node: TrmTreeNode) ;
      procedure EndUpdate;
      function GetFirstNode: TrmTreeNode;
      function GetNode(ItemId: HTreeItem) : TrmTreeNode;
      function Insert(Node: TrmTreeNode; const S: string) : TrmTreeNode;
      function InsertObject(Node: TrmTreeNode; const S: string;
         Ptr: Pointer) : TrmTreeNode;
      function LocateNode(Path: string) : TrmTreeNode;
      property Count: Integer read GetCount;
      property Handle: HWND read GetHandle;
      property Item[Index: Integer]: TrmTreeNode read GetNodeFromIndex; default;
      property Owner: TrmCustomPathTreeView read FOwner;
   end;

{ TrmCustomPathTreeView }
   TrmCustomPathTreeView = class(TWinControl)
   private
      FAutoExpand: Boolean;
      FBorderStyle: TBorderStyle;
      FCanvas: TCanvas;
      FCanvasChanged: Boolean;
      FDefEditProc: Pointer;
      FDragged: Boolean;
      FDragImage: TDragImageList;
      FDragNode: TrmTreeNode;
      FEditHandle: HWND;
      FEditInstance: Pointer;
      FHideSelection: Boolean;
      FHotTrack: Boolean;
      FImageChangeLink: TChangeLink;
      FImages: TCustomImageList;
      FLastDropTarget: TrmTreeNode;
      FMemStream: TMemoryStream;
      FRClickNode: TrmTreeNode;
      FRightClickSelect: Boolean;
      FManualNotify: Boolean;
      FReadOnly: Boolean;
      FRowSelect: Boolean;
      FSaveIndex: Integer;
      FSaveIndent: Integer;
      FSaveItems: TStringList;
      FSaveTopIndex: Integer;
      FShowButtons: Boolean;
      FShowLines: Boolean;
      FShowRoot: Boolean;
      FSortType: TSortType;
      FStateChanging: Boolean;
      FStateImages: TCustomImageList;
      FStateChangeLink: TChangeLink;
      FToolTips: Boolean;
      FTreeNodes: TrmTreeNodes;
      FWideText: WideString;
      FOnAdvancedCustomDraw: TrmTVAdvancedCustomDrawEvent;
      FOnAdvancedCustomDrawItem: TrmTVAdvancedCustomDrawItemEvent;
      FOnChange: TrmTVChangedEvent;
      FOnChanging: TrmTVChangingEvent;
      FOnCollapsed: TrmTVExpandedEvent;
      FOnCollapsing: TrmTVCollapsingEvent;
      FOnCompare: TrmTVCompareEvent;
      FOnCustomDraw: TrmTVCustomDrawEvent;
      FOnCustomDrawItem: TrmTVCustomDrawItemEvent;
      FOnDeletion: TrmTVExpandedEvent;
      FOnEditing: TrmTVEditingEvent;
      FOnEdited: TrmTVEditedEvent;
      FOnExpanded: TrmTVExpandedEvent;
      FOnExpanding: TrmTVExpandingEvent;
      FOnGetImageIndex: TrmTVExpandedEvent;
      FOnGetSelectedIndex: TrmTVExpandedEvent;

    //Path seperator character
      FSepChar: Char;

      procedure CanvasChanged(Sender: TObject) ;
      procedure CMColorChanged(var Message: TMessage) ; message CM_COLORCHANGED;
      procedure CMCtl3DChanged(var Message: TMessage) ; message CM_CTL3DCHANGED;
      procedure CMFontChanged(var Message: TMessage) ; message CM_FONTCHANGED;
      procedure CMDrag(var Message: TCMDrag) ; message CM_DRAG;
      procedure CNNotify(var Message: TWMNotify) ; message CN_NOTIFY;
      procedure EditWndProc(var Message: TMessage) ;
      procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean) ;
      function GetChangeDelay: Integer;
      function GetDropTarget: TrmTreeNode;
      function GetIndent: Integer;
      function GetNodeFromItem(const Item: TTVItem) : TrmTreeNode;
      function GetSelection: TrmTreeNode;
      function GetTopItem: TrmTreeNode;
      procedure ImageListChange(Sender: TObject) ;
      procedure SetAutoExpand(Value: Boolean) ;
      procedure SetBorderStyle(Value: TBorderStyle) ;
      procedure SetButtonStyle(Value: Boolean) ;
      procedure SetChangeDelay(Value: Integer) ;
      procedure SetDropTarget(Value: TrmTreeNode) ;
      procedure SetHideSelection(Value: Boolean) ;
      procedure SetHotTrack(Value: Boolean) ;
      procedure SetImageList(Value: HImageList; Flags: Integer) ;
      procedure SetIndent(Value: Integer) ;
      procedure SetImages(Value: TCustomImageList) ;
      procedure SetLineStyle(Value: Boolean) ;
      procedure SetReadOnly(Value: Boolean) ;
      procedure SetRootStyle(Value: Boolean) ;
      procedure SetRowSelect(Value: Boolean) ;
      procedure SetSelection(Value: TrmTreeNode) ;
      procedure SetSortType(Value: TSortType) ;
      procedure SetStateImages(Value: TCustomImageList) ;
      procedure SetToolTips(Value: Boolean) ;
      procedure SetrmTreeNodes(Value: TrmTreeNodes) ;
      procedure SetTopItem(Value: TrmTreeNode) ;
      procedure OnChangeTimer(Sender: TObject) ;
      procedure WMLButtonDown(var Message: TWMLButtonDown) ; message WM_LBUTTONDOWN;
      procedure WMNotify(var Message: TWMNotify) ; message WM_NOTIFY;
      procedure WMContextMenu(var Message: TWMContextMenu) ; message WM_CONTEXTMENU;
      procedure CMSysColorChange(var Message: TMessage) ; message CM_SYSCOLORCHANGE;

    //Pathing functions...
      function ChildName(s: string) : string;
      function ParentName(s: string) : string;

    //Fix for MS ComCtrl bug with treeviews and hintwindows...
      procedure SetNewHint(Node: TrmTreeNode) ;
      procedure CMCancelMode(var Message: TMessage) ; message CM_CancelMode;
      procedure CMMouseLeave(var Message: TMessage) ; message CM_MouseLeave;

    //Extranous functions from Pat's original code...
      function GetFocussedNode: TrmTreeNode;
      procedure SetFocussedNode(const Value: TrmTreeNode) ;
   protected
      FChangeTimer: TTimer;
      function CanEdit(Node: TrmTreeNode) : Boolean; dynamic;
      function CanChange(Node: TrmTreeNode) : Boolean; dynamic;
      function CanCollapse(Node: TrmTreeNode) : Boolean; dynamic;
      function CanExpand(Node: TrmTreeNode) : Boolean; dynamic;
      procedure Change(Node: TrmTreeNode) ; dynamic;
      procedure Collapse(Node: TrmTreeNode) ; dynamic;
      function CreateNode: TrmTreeNode; virtual;
      procedure CreateParams(var Params: TCreateParams) ; override;
      procedure CreateWnd; override;
      function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage) : Boolean; virtual;
      function CustomDrawItem(Node: TrmTreeNode; State: TCustomDrawState;
         Stage: TCustomDrawStage; var PaintImages: Boolean) : Boolean; virtual;
      procedure Delete(Node: TrmTreeNode) ; dynamic;
      procedure DestroyWnd; override;
      procedure DoEndDrag(Target: TObject; X, Y: Integer) ; override;
      procedure DoStartDrag(var DragObject: TDragObject) ; override;
      procedure Edit(const Item: TTVItem) ; dynamic;
      procedure Expand(Node: TrmTreeNode) ; dynamic;
      function GetDragImages: TDragImageList; override;
      procedure GetImageIndex(Node: TrmTreeNode) ; virtual;
      procedure GetSelectedIndex(Node: TrmTreeNode) ; virtual;
      function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage) : Boolean;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent;
         Operation: TOperation) ; override;
      procedure SetDragMode(Value: TDragMode) ; override;
      procedure WndProc(var Message: TMessage) ; override;

    //Fix for MS ComCtrl bug with treeviews and hintwindows...
      procedure MouseMove(Shift: TShiftState; X, Y: Integer) ; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) ; override;

      property AutoExpand: Boolean read FAutoExpand write SetAutoExpand default False;
      property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
      property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
      property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
      property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
      property Images: TCustomImageList read FImages write SetImages;
      property Indent: Integer read GetIndent write SetIndent;
      property Items: TrmTreeNodes read FTreeNodes write SeTrmTreeNodes;
      property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
      property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
      property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
      property ShowButtons: Boolean read FShowButtons write SetButtonStyle default True;
      property ShowLines: Boolean read FShowLines write SetLineStyle default True;
      property ShowRoot: Boolean read FShowRoot write SetRootStyle default True;
      property SortType: TSortType read FSortType write SetSortType default stNone;
      property StateImages: TCustomImageList read FStateImages write SetStateImages;
      property ToolTips: Boolean read FToolTips write SetToolTips default True;
      property OnAdvancedCustomDraw: TrmTVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
      property OnAdvancedCustomDrawItem: TrmTVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
      property OnChange: TrmTVChangedEvent read FOnChange write FOnChange;
      property OnChanging: TrmTVChangingEvent read FOnChanging write FOnChanging;
      property OnCollapsed: TrmTVExpandedEvent read FOnCollapsed write FOnCollapsed;
      property OnCollapsing: TrmTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
      property OnCompare: TrmTVCompareEvent read FOnCompare write FOnCompare;
      property OnCustomDraw: TrmTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
      property OnCustomDrawItem: TrmTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
      property OnDeletion: TrmTVExpandedEvent read FOnDeletion write FOnDeletion;
      property OnEditing: TrmTVEditingEvent read FOnEditing write FOnEditing;
      property OnEdited: TrmTVEditedEvent read FOnEdited write FOnEdited;
      property OnExpanding: TrmTVExpandingEvent read FOnExpanding write FOnExpanding;
      property OnExpanded: TrmTVExpandedEvent read FOnExpanded write FOnExpanded;
      property OnGetImageIndex: TrmTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
      property OnGetSelectedIndex: TrmTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
   public
      constructor Create(AOwner: TComponent) ; override;
      destructor Destroy; override;
      function AlphaSort: Boolean;
      function CustomSort(SortProc: TTVCompare; Data: Longint) : Boolean;
      procedure FullCollapse;
      procedure FullExpand;
      function GetHitTestInfoAt(X, Y: Integer) : THitTests;
      function GetNodeAt(X, Y: Integer) : TrmTreeNode;
      function IsEditing: Boolean;
      procedure LoadFromFile(const FileName: string) ;
      procedure LoadFromStream(Stream: TStream) ;
      procedure SaveToFile(const FileName: string) ;
      procedure SaveToStream(Stream: TStream) ;

    //Pathing functions...
      function AddPathNode(Node: TrmTreeNode; Path: string) : TrmTreeNode;
      function FindPathNode(Path: string) : TrmTreeNode;
      function NodePath(Node: TrmTreeNode) : string;

      property Canvas: TCanvas read FCanvas;
      property DropTarget: TrmTreeNode read GetDropTarget write SetDropTarget;
      property Selected: TrmTreeNode read GetSelection write SetSelection;
      property TopItem: TrmTreeNode read GetTopItem write SetTopItem;

    //Original PathTreeView property.  Not exactly sure why Pat had this in here but....
      property FocussedNode: TrmTreeNode read GetFocussedNode write SetFocussedNode;
   published
    //Path seperator character
      property SepChar: Char read FSepChar write FSepChar default #47;
   end;

   TrmPathTreeView = class(TrmCustomPathTreeView)
   published
      property Align;
      property Anchors;
      property AutoExpand;
      property BiDiMode;
      property BorderStyle;
      property BorderWidth;
      property ChangeDelay;
      property Color;
      property Ctl3D;
      property Constraints;
      property DragKind;
      property DragCursor;
      property DragMode;
      property Enabled;
      property Font;
      property HideSelection;
      property HotTrack;
      property Images;
      property Indent;
      property ParentBiDiMode;
      property ParentColor default False;
      property ParentCtl3D;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ReadOnly;
      property RightClickSelect;
      property RowSelect;
      property ShowButtons;
      property ShowHint;
      property ShowLines;
      property ShowRoot;
      property SortType;
      property StateImages;
      property TabOrder;
      property TabStop default True;
      property ToolTips;
      property Visible;
      property OnAdvancedCustomDraw;
      property OnAdvancedCustomDrawItem;
      property OnChange;
      property OnChanging;
      property OnClick;
      property OnCollapsed;
      property OnCollapsing;
      property OnCompare;
      property OnContextPopup;
      property OnCustomDraw;
      property OnCustomDrawItem;
      property OnDblClick;
      property OnDeletion;
      property OnDragDrop;
      property OnDragOver;
      property OnEdited;
      property OnEditing;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnExpanding;
      property OnExpanded;
      property OnGetImageIndex;
      property OnGetSelectedIndex;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDock;
      property OnStartDrag;
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
      property Items;
   end;

implementation

{$ifdef D6_or_higher}
uses rmHint, RTLConsts, comstrs, rmLibrary;
{$else}
uses rmHint, Consts, comstrs, rmLibrary;
{$endif}

type
   PFontHandles = ^TFontHandles;
   TFontHandles = record
      OurFont,
         StockFont: Integer;
   end;

var
   fHint: TrmHintWindow;

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean) ;
var
   Style: Integer;
begin
   if Ctl.HandleAllocated then
   begin
      Style := GetWindowLong(Ctl.Handle, GWL_STYLE) ;
      if not UseStyle then Style := Style and not Value
      else Style := Style or Value;
      SetWindowLong(Ctl.Handle, GWL_STYLE, Style) ;
   end;
end;

{ TrmTreeNode }

function DefaultTreeViewSort(Node1, Node2: TrmTreeNode; lParam: Integer) : Integer; stdcall;
begin
   with Node1 do
      if Assigned(TreeView.OnCompare) then
         TreeView.OnCompare(TreeView, Node1, Node2, lParam, Result)
      else Result := lstrcmp(PChar(Node1.Text) , PChar(Node2.Text) ) ;
end;

procedure TreeViewError(const Msg: string) ;
begin
   raise ETreeViewError.Create(Msg) ;
end;

procedure TreeViewErrorFmt(const Msg: string; Format: array of const) ;
begin
   raise ETreeViewError.CreateFmt(Msg, Format) ;
end;

constructor TrmTreeNode.Create(AOwner: TrmTreeNodes) ;
begin
   inherited Create;
   FOverlayIndex := -1;
   FStateIndex := -1;
   FOwner := AOwner;
end;

destructor TrmTreeNode.Destroy;
var
   Node: TrmTreeNode;
   CheckValue: Integer;
begin
   Owner.ClearCache;
   FDeleting := True;
   Owner.RemoveHash(self) ;
   if Owner.Owner.FLastDropTarget = Self then
      Owner.Owner.FLastDropTarget := nil;
   Node := Parent;
   if (Node <> nil) and (not Node.Deleting) then
   begin
      if Node.IndexOf(Self) <> -1 then CheckValue := 1
      else CheckValue := 0;
      if Node.CompareCount(CheckValue) then
      begin
         Expanded := False;
         Node.HasChildren := False;
      end;
   end;
   if ItemId <> nil then TreeView_DeleteItem(Handle, ItemId) ;
   Data := nil;
   inherited Destroy;
end;

function TrmTreeNode.GetHandle: HWND;
begin
   Result := TreeView.Handle;
end;

function TrmTreeNode.GetTreeView: TrmCustomPathTreeView;
begin
   Result := Owner.Owner;
end;

function TrmTreeNode.HasAsParent(Value: TrmTreeNode) : Boolean;
begin
   if Value <> Nil then
   begin
      if Parent = nil then Result := False
      else if Parent = Value then Result := True
      else Result := Parent.HasAsParent(Value) ;
   end
   else Result := True;
end;

procedure TrmTreeNode.SetText(const S: string) ;
var
   Item: TTVItem;
   fRemoved: boolean;
begin

   fRemoved := false;
   if not (FText = '') and (FText <> S) then
   begin
      Self.RemoveHash;
      fRemoved := true;
   end;

   FText := S;
   with Item do
   begin
      mask := TVIF_TEXT;
      hItem := ItemId;
      pszText := LPSTR_TEXTCALLBACK;
   end;
   TreeView_SetItem(Handle, Item) ;
   if (TreeView.SortType in [stText, stBoth]) and FInTree then
   begin
      if (Parent <> nil) then Parent.AlphaSort
      else TreeView.AlphaSort;
   end;

   if fremoved and not (fText = '') then
   begin
      Self.RenewHash;
   end;
end;

procedure TrmTreeNode.SetData(Value: Pointer) ;
begin
   FData := Value;
   if (TreeView.SortType in [stData, stBoth]) and Assigned(TreeView.OnCompare)
      and (not Deleting) and FInTree then
   begin
      if Parent <> nil then Parent.AlphaSort
      else TreeView.AlphaSort;
   end;
end;

function TrmTreeNode.GetState(NodeState: TNodeState) : Boolean;
var
   Item: TTVItem;
begin
   Result := False;
   with Item do
   begin
      mask := TVIF_STATE;
      hItem := ItemId;
      if TreeView_GetItem(Handle, Item) then
         case NodeState of
            nsCut: Result := (state and TVIS_CUT) <> 0;
            nsFocused: Result := (state and TVIS_FOCUSED) <> 0;
            nsSelected: Result := (state and TVIS_SELECTED) <> 0;
            nsExpanded: Result := (state and TVIS_EXPANDED) <> 0;
            nsDropHilited: Result := (state and TVIS_DROPHILITED) <> 0;
         end;
   end;
end;

procedure TrmTreeNode.SetImageIndex(Value: TImageIndex) ;
var
   Item: TTVItem;
begin
   FImageIndex := Value;
   with Item do
   begin
      mask := TVIF_IMAGE or TVIF_HANDLE;
      hItem := ItemId;
      if Assigned(TrmCustomPathTreeView(Owner.Owner) .OnGetImageIndex) then
         iImage := I_IMAGECALLBACK
      else
         iImage := FImageIndex;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

procedure TrmTreeNode.SetSelectedIndex(Value: Integer) ;
var
   Item: TTVItem;
begin
   FSelectedIndex := Value;
   with Item do
   begin
      mask := TVIF_SELECTEDIMAGE or TVIF_HANDLE;
      hItem := ItemId;
      if Assigned(TrmCustomPathTreeView(Owner.Owner) .OnGetSelectedIndex) then
         iSelectedImage := I_IMAGECALLBACK
      else
         iSelectedImage := FSelectedIndex;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

procedure TrmTreeNode.SetOverlayIndex(Value: Integer) ;
var
   Item: TTVItem;
begin
   FOverlayIndex := Value;
   with Item do
   begin
      mask := TVIF_STATE or TVIF_HANDLE;
      stateMask := TVIS_OVERLAYMASK;
      hItem := ItemId;
      state := IndexToOverlayMask(FOverlayIndex + 1) ;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

procedure TrmTreeNode.SetStateIndex(Value: Integer) ;
var
   Item: TTVItem;
begin
   FStateIndex := Value;
   if Value >= 0 then Dec(Value) ;
   with Item do
   begin
      mask := TVIF_STATE or TVIF_HANDLE;
      stateMask := TVIS_STATEIMAGEMASK;
      hItem := ItemId;
      state := IndexToStateImageMask(Value + 1) ;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

function TrmTreeNode.CompareCount(CompareMe: Integer) : Boolean;
var
   Count: integer;
   Node: TrmTreeNode;
Begin
   Count := 0;
   Result := False;
   Node := GetFirstChild;
   while Node <> nil do
   begin
      Inc(Count) ;
      Node := Node.GetNextChild(Node) ;
      if Count > CompareMe then Exit;
   end;
   if Count = CompareMe then Result := True;
end;

function TrmTreeNode.DoCanExpand(Expand: Boolean) : Boolean;
begin
   Result := False;
   if HasChildren then
   begin
      if Expand then Result := TreeView.CanExpand(Self)
      else Result := TreeView.CanCollapse(Self) ;
   end;
end;

procedure TrmTreeNode.DoExpand(Expand: Boolean) ;
begin
   if HasChildren then
   begin
      if Expand then TreeView.Expand(Self)
      else TreeView.Collapse(Self) ;
   end;
end;

procedure TrmTreeNode.ExpandItem(Expand: Boolean; Recurse: Boolean) ;
var
   Flag: Integer;
   Node: TrmTreeNode;
begin
   if Recurse then
   begin
      Node := Self;
      repeat
         Node.ExpandItem(Expand, False) ;
         Node := Node.GetNext;
      until (Node = nil) or (not Node.HasAsParent(Self) ) ;
   end
   else
   begin
      TreeView.FManualNotify := True;
      try
         Flag := 0;
         if Expand then
         begin
            if DoCanExpand(True) then
            begin
               Flag := TVE_EXPAND;
               DoExpand(True) ;
            end;
         end
         else
         begin
            if DoCanExpand(False) then
            begin
               Flag := TVE_COLLAPSE;
               DoExpand(False) ;
            end;
         end;
         if Flag <> 0 then TreeView_Expand(Handle, ItemId, Flag) ;
      finally
         TreeView.FManualNotify := False;
      end;
   end;
end;

procedure TrmTreeNode.Expand(Recurse: Boolean) ;
begin
   ExpandItem(True, Recurse) ;
end;

procedure TrmTreeNode.Collapse(Recurse: Boolean) ;
begin
   ExpandItem(False, Recurse) ;
end;

function TrmTreeNode.GetExpanded: Boolean;
begin
   Result := GetState(nsExpanded) ;
end;

procedure TrmTreeNode.SetExpanded(Value: Boolean) ;
begin
   if Value then Expand(False)
   else Collapse(False) ;
end;

function TrmTreeNode.GetSelected: Boolean;
begin
   Result := GetState(nsSelected) ;
end;

procedure TrmTreeNode.SetSelected(Value: Boolean) ;
begin
   if Value then TreeView_SelectItem(Handle, ItemId)
   else if Selected then TreeView_SelectItem(Handle, nil) ;
end;

function TrmTreeNode.GetCut: Boolean;
begin
   Result := GetState(nsCut) ;
end;

procedure TrmTreeNode.SetCut(Value: Boolean) ;
var
   Item: TTVItem;
   Template: DWORD;
begin
   if Value then Template := DWORD(-1)
   else Template := 0;
   with Item do
   begin
      mask := TVIF_STATE;
      hItem := ItemId;
      stateMask := TVIS_CUT;
      state := stateMask and Template;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

function TrmTreeNode.GetDropTarget: Boolean;
begin
   Result := GetState(nsDropHilited) ;
end;

procedure TrmTreeNode.SetDropTarget(Value: Boolean) ;
begin
   if Value then TreeView_SelectDropTarget(Handle, ItemId)
   else if DropTarget then TreeView_SelectDropTarget(Handle, nil) ;
end;

function TrmTreeNode.GetChildren: Boolean;
var
   Item: TTVItem;
begin
   Item.mask := TVIF_CHILDREN;
   Item.hItem := ItemId;
   if TreeView_GetItem(Handle, Item) then Result := Item.cChildren > 0
   else Result := False;
end;

procedure TrmTreeNode.SetFocused(Value: Boolean) ;
var
   Item: TTVItem;
   Template: DWORD;
begin
   if Value then Template := DWORD(-1)
   else Template := 0;
   with Item do
   begin
      mask := TVIF_STATE;
      hItem := ItemId;
      stateMask := TVIS_FOCUSED;
      state := stateMask and Template;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

function TrmTreeNode.GetFocused: Boolean;
begin
   Result := GetState(nsFocused) ;
end;

procedure TrmTreeNode.SetChildren(Value: Boolean) ;
var
   Item: TTVItem;
begin
   with Item do
   begin
      mask := TVIF_CHILDREN;
      hItem := ItemId;
      cChildren := Ord(Value) ;
   end;
   TreeView_SetItem(Handle, Item) ;
end;

function TrmTreeNode.GetParent: TrmTreeNode;
begin
   with FOwner do
      Result := GetNode(TreeView_GetParent(Handle, ItemId) ) ;
end;

function TrmTreeNode.GetNextSibling: TrmTreeNode;
begin
   with FOwner do
      Result := GetNode(TreeView_GetNextSibling(Handle, ItemId) ) ;
end;

function TrmTreeNode.GetPrevSibling: TrmTreeNode;
begin
   with FOwner do
      Result := GetNode(TreeView_GetPrevSibling(Handle, ItemId) ) ;
end;

function TrmTreeNode.GetNextVisible: TrmTreeNode;
begin
   if IsVisible then
      with FOwner do
         Result := GetNode(TreeView_GetNextVisible(Handle, ItemId) )
   else Result := nil;
end;

function TrmTreeNode.GetPrevVisible: TrmTreeNode;
begin
   with FOwner do
      Result := GetNode(TreeView_GetPrevVisible(Handle, ItemId) ) ;
end;

function TrmTreeNode.GetNextChild(Value: TrmTreeNode) : TrmTreeNode;
begin
   if Value <> nil then Result := Value.GetNextSibling
   else Result := nil;
end;

function TrmTreeNode.GetPrevChild(Value: TrmTreeNode) : TrmTreeNode;
begin
   if Value <> nil then Result := Value.GetPrevSibling
   else Result := nil;
end;

function TrmTreeNode.GetFirstChild: TrmTreeNode;
begin
   with FOwner do
      Result := GetNode(TreeView_GetChild(Handle, ItemId) ) ;
end;

function TrmTreeNode.GetLastChild: TrmTreeNode;
var
   Node: TrmTreeNode;
begin
   Result := GetFirstChild;
   if Result <> nil then
   begin
      Node := Result;
      repeat
         Result := Node;
         Node := Result.GetNextSibling;
      until Node = nil;
   end;
end;

function TrmTreeNode.GetNext: TrmTreeNode;
var
   NodeID, ParentID: HTreeItem;
   Handle: HWND;
begin
   Handle := FOwner.Handle;
   NodeID := TreeView_GetChild(Handle, ItemId) ;
   if NodeID = nil then NodeID := TreeView_GetNextSibling(Handle, ItemId) ;
   ParentID := ItemId;
   while (NodeID = nil) and (ParentID <> nil) do
   begin
      ParentID := TreeView_GetParent(Handle, ParentID) ;
      NodeID := TreeView_GetNextSibling(Handle, ParentID) ;
   end;
   Result := FOwner.GetNode(NodeID) ;
end;

function TrmTreeNode.GetPrev: TrmTreeNode;
var
   Node: TrmTreeNode;
begin
   Result := GetPrevSibling;
   if Result <> nil then
   begin
      Node := Result;
      repeat
         Result := Node;
         Node := Result.GetLastChild;
      until Node = nil;
   end else
      Result := Parent;
end;

function TrmTreeNode.GetAbsoluteIndex: Integer;
var
   Node: TrmTreeNode;
begin
   if Owner.FNodeCache.CacheNode = Self then
      Result := Owner.FNodeCache.CacheIndex
   else
   begin
      Result := -1;
      Node := Self;
      while Node <> nil do
      begin
         Inc(Result) ;
         Node := Node.GetPrev;
      end;
   end;
end;

function TrmTreeNode.GetIndex: Integer;
var
   Node: TrmTreeNode;
begin
   Result := -1;
   Node := Self;
   while Node <> nil do
   begin
      Inc(Result) ;
      Node := Node.GetPrevSibling;
   end;
end;

function TrmTreeNode.GetItem(Index: Integer) : TrmTreeNode;
begin
   Result := GetFirstChild;
   while (Result <> nil) and (Index > 0) do
   begin
      Result := GetNextChild(Result) ;
      Dec(Index) ;
   end;
   if Result = nil then TreeViewError(SListIndexError) ;
end;

procedure TrmTreeNode.SetItem(Index: Integer; Value: TrmTreeNode) ;
begin
   item[Index].Assign(Value) ;
end;

function TrmTreeNode.IndexOf(Value: TrmTreeNode) : Integer;
var
   Node: TrmTreeNode;
begin
   Result := -1;
   Node := GetFirstChild;
   while (Node <> nil) do
   begin
      Inc(Result) ;
      if Node = Value then Break;
      Node := GetNextChild(Node) ;
   end;
   if Node = nil then Result := -1;
end;

function TrmTreeNode.GetCount: Integer;
var
   Node: TrmTreeNode;
begin
   Result := 0;
   Node := GetFirstChild;
   while Node <> nil do
   begin
      Inc(Result) ;
      Node := Node.GetNextChild(Node) ;
   end;
end;

procedure TrmTreeNode.EndEdit(Cancel: Boolean) ;
begin
   TreeView_EndEditLabelNow(Handle, Cancel) ;
end;

procedure TrmTreeNode.InternalMove(ParentNode, Node: TrmTreeNode;
   HItem: HTreeItem; AddMode: TAddMode) ;
var
   I: Integer;
   NodeId: HTreeItem;
   TreeViewItem: TTVItem;
   Children: Boolean;
   IsSelected: Boolean;
begin
   Owner.ClearCache;
   if (AddMode = taInsert) and (Node <> nil) then
      NodeId := Node.ItemId else
      NodeId := nil;
   Children := HasChildren;
   IsSelected := Selected;
   if (Parent <> nil) and (Parent.CompareCount(1) ) then
   begin
      Parent.Expanded := False;
      Parent.HasChildren := False;
   end;
   with TreeViewItem do
   begin
      mask := TVIF_PARAM;
      hItem := ItemId;
      lParam := 0;
   end;
   TreeView_SetItem(Handle, TreeViewItem) ;
   with Owner do
      HItem := AddItem(HItem, NodeId, CreateItem(Self) , AddMode) ;
   if HItem = nil then
      raise EOutOfResources.Create(sInsertError) ;
   for I := Count - 1 downto 0 do
      Item[I].InternalMove(Self, nil, HItem, taAddFirst) ;
   TreeView_DeleteItem(Handle, ItemId) ;
   FItemId := HItem;
   Assign(Self) ;
   HasChildren := Children;
   Selected := IsSelected;
end;

procedure TrmTreeNode.MoveTo(Destination: TrmTreeNode; Mode: TNodeAttachMode) ;
var
   AddMode: TAddMode;
   Node: TrmTreeNode;
   HItem: HTreeItem;
   OldOnChanging: TrmTVChangingEvent;
   OldOnChange: TrmTVChangedEvent;
begin
   OldOnChanging := TreeView.OnChanging;
   OldOnChange := TreeView.OnChange;
   TreeView.OnChanging := nil;
   TreeView.OnChange := nil;
   try
      if (Destination = nil) or not Destination.HasAsParent(Self) then
      begin
         AddMode := taAdd;
         if (Destination <> nil) and not (Mode in [naAddChild, naAddChildFirst]) then
            Node := Destination.Parent else
            Node := Destination;
         case Mode of
            naAdd,
               naAddChild: AddMode := taAdd;
            naAddFirst,
               naAddChildFirst: AddMode := taAddFirst;
            naInsert:
               begin
                  Destination := Destination.GetPrevSibling;
                  if Destination = nil then AddMode := taAddFirst
                  else AddMode := taInsert;
               end;
         end;
         if Node <> nil then
            HItem := Node.ItemId else
            HItem := nil;
         if (Destination <> Self) then
         begin
            RemoveHash;
            InternalMove(Node, Destination, HItem, AddMode) ;
            RenewHash;
         end;
         Node := Parent;
         if Node <> nil then
         begin
            Node.HasChildren := True;
            Node.Expanded := True;
         end;
      end;
   finally
      TreeView.OnChanging := OldOnChanging;
      TreeView.OnChange := OldOnChange;
   end;
end;

procedure TrmTreeNode.MakeVisible;
begin
   TreeView_EnsureVisible(Handle, ItemId) ;
end;

function TrmTreeNode.GetLevel: Integer;
var
   Node: TrmTreeNode;
begin
   Result := 0;
   Node := Parent;
   while Node <> nil do
   begin
      Inc(Result) ;
      Node := Node.Parent;
   end;
end;

function TrmTreeNode.IsNodeVisible: Boolean;
var
   Rect: TRect;
begin
   Result := TreeView_GetItemRect(Handle, ItemId, Rect, True) ;
end;

function TrmTreeNode.EditText: Boolean;
begin
   Result := TreeView_EditLabel(Handle, ItemId) <> 0;
end;

function TrmTreeNode.DisplayRect(TextOnly: Boolean) : TRect;
begin
   FillChar(Result, SizeOf(Result) , 0) ;
   TreeView_GetItemRect(Handle, ItemId, Result, TextOnly) ;
end;

function TrmTreeNode.AlphaSort: Boolean;
begin
   Result := CustomSort(nil, 0) ;
end;

function TrmTreeNode.CustomSort(SortProc: TTVCompare; Data: Longint) : Boolean;
var
   SortCB: TTVSortCB;
begin
   Owner.ClearCache;
   with SortCB do
   begin
      if not Assigned(SortProc) then lpfnCompare := @DefaultTreeViewSort
      else lpfnCompare := SortProc;
      hParent := ItemId;
      lParam := Data;
   end;
   Result := TreeView_SortChildrenCB(Handle, SortCB, 0) ;
end;

procedure TrmTreeNode.Delete;
begin
   if not Deleting then
   begin
      Owner.RemoveHash(self) ;
      Free;
   end;
end;

procedure TrmTreeNode.DeleteChildren;

   procedure recurseChildren(Node: TrmTreeNode) ;
   begin
      if assigned(Node) then
      begin
         Node := Node.getFirstChild;
         while node <> nil do
         begin
            Node.removeHash;
            recurseChildren(node) ;
            node := node.getNextSibling;
         end;
      end;
   end;

begin
   recurseChildren(self) ;

   Owner.ClearCache;

   TreeView_Expand(TreeView.Handle, ItemID, TVE_COLLAPSE or TVE_COLLAPSERESET) ;
   HasChildren := False;
end;

procedure TrmTreeNode.Assign(Source: TPersistent) ;
var
   Node: TrmTreeNode;
begin
   Owner.ClearCache;
   if Source is TrmTreeNode then
   begin
      Node := TrmTreeNode(Source) ;
      Text := Node.Text;
      Data := Node.Data;
      ImageIndex := Node.ImageIndex;
      SelectedIndex := Node.SelectedIndex;
      StateIndex := Node.StateIndex;
      OverlayIndex := Node.OverlayIndex;
      Focused := Node.Focused;
      DropTarget := Node.DropTarget;
      Cut := Node.Cut;
      HasChildren := Node.HasChildren;
   end
   else inherited Assign(Source) ;
end;

function TrmTreeNode.IsEqual(Node: TrmTreeNode) : Boolean;
begin
   Result := (Text = Node.Text) and (Data = Node.Data) ;
end;

procedure TrmTreeNode.ReadData(Stream: TStream; Info: PNodeInfo) ;
var
   I, Size, ItemCount: Integer;
begin
   Owner.ClearCache;
   Stream.ReadBuffer(Size, SizeOf(Size) ) ;
   Stream.ReadBuffer(Info^, Size) ;
   Text := Info^.Text;
   ImageIndex := Info^.ImageIndex;
   SelectedIndex := Info^.SelectedIndex;
   StateIndex := Info^.StateIndex;
   OverlayIndex := Info^.OverlayIndex;
   Data := Info^.Data;
   ItemCount := Info^.Count;

   for I := 0 to ItemCount - 1 do
      Owner.AddChild(Self, '') .ReadData(Stream, Info) ;
end;

procedure TrmTreeNode.WriteData(Stream: TStream; Info: PNodeInfo) ;
var
   I, Size, L, ItemCount: Integer;
begin
   L := Length(Text) ;
   if L > 255 then L := 255;
   Size := SizeOf(TNodeInfo) + L - 255;
   Info^.Text := Text;
   Info^.ImageIndex := ImageIndex;
   Info^.SelectedIndex := SelectedIndex;
   Info^.OverlayIndex := OverlayIndex;
   Info^.StateIndex := StateIndex;
   Info^.Data := Data;
   ItemCount := Count;
   Info^.Count := ItemCount;
   Stream.WriteBuffer(Size, SizeOf(Size) ) ;
   Stream.WriteBuffer(Info^, Size) ;
   for I := 0 to ItemCount - 1 do Item[I].WriteData(Stream, Info) ;
end;

procedure TrmTreeNode.RemoveHash;
var
   wNode: TrmTreeNode;
begin
   FOwner.RemoveHash(self) ;
   wNode := getFirstChild;
   while wNode <> nil do
   begin
      wNode.RemoveHash;
      wNode := wNode.getNextSibling;
   end;
end;

procedure TrmTreeNode.RenewHash;
var
   wNode: TrmTreeNode;
begin
   FOwner.BinaryInsert(FOwner.Owner.NodePath(self) , self) ;
   wNode := getFirstChild;
   while wNode <> nil do
   begin
      wNode.RenewHash;
      wNode := wNode.getNextSibling;
   end;
end;

procedure TrmTreeNodes.DumpHash;
var
   fstr: TextFile;
   loop: integer;
   wdata: trmhashdata;
begin
   AssignFile(fstr, 'c:\tvhash.txt') ;
   rewrite(fstr) ;
   for loop := 0 to fhashlist.count - 1 do
   begin
      wData := Trmhashdata(fhashlist[loop]) ;
      writeln(fstr, owner.nodepath(wdata.node) ) ;
   end;
   closefile(fstr) ;
end;

procedure TrmTreeNode.SetParent(const Value: TrmTreeNode);
begin
//Do Nothing...
end;

{ TrmTreeNodes }

constructor TrmTreeNodes.Create(AOwner: TrmCustomPathTreeView) ;
begin
   inherited Create;
   FOwner := AOwner;
   FHashList := TObjectList.Create;
   FHashList.OwnsObjects := true;
end;

destructor TrmTreeNodes.Destroy;
begin
   Clear;
   FHashList.Free;
   inherited Destroy;
end;

function TrmTreeNodes.GetCount: Integer;
begin
   if Owner.HandleAllocated then Result := TreeView_GetCount(Handle)
   else Result := 0;
end;

function TrmTreeNodes.GetHandle: HWND;
begin
   Result := Owner.Handle;
end;

procedure TrmTreeNodes.Delete(Node: TrmTreeNode) ;
var
   wIndex: integer;
begin
  //Remove the path index reference...
   wIndex := LocateHashIndex(Owner.NodePath(Node) ) ;
   if wIndex > -1 then
      FHashList.delete(wIndex) ;

   if (Node.ItemId = nil) then
      Owner.Delete(Node) ;
   Node.Delete;
end;

procedure TrmTreeNodes.Clear;
begin
   ClearCache;
   if not (csDestroying in Owner.ComponentState) and Owner.HandleAllocated then
   begin
      FHashList.Clear;
      TreeView_DeleteAllItems(Owner.Handle) ;
   end;
end;

function TrmTreeNodes.AddChildFirst(Node: TrmTreeNode; const S: string) : TrmTreeNode;
begin
   Result := AddChildObjectFirst(Node, S, nil) ;
end;

function TrmTreeNodes.AddChildObjectFirst(Node: TrmTreeNode; const S: string;
   Ptr: Pointer) : TrmTreeNode;
begin
   Result := InternalAddObject(Node, S, Ptr, taAddFirst) ;
end;

function TrmTreeNodes.AddChild(Node: TrmTreeNode; const S: string) : TrmTreeNode;
begin
   Result := AddChildObject(Node, S, nil) ;
end;

function TrmTreeNodes.AddChildObject(Node: TrmTreeNode; const S: string;
   Ptr: Pointer) : TrmTreeNode;
begin
   Result := InternalAddObject(Node, S, Ptr, taAdd) ;
end;

function TrmTreeNodes.AddFirst(Node: TrmTreeNode; const S: string) : TrmTreeNode;
begin
   Result := AddObjectFirst(Node, S, nil) ;
end;

function TrmTreeNodes.AddObjectFirst(Node: TrmTreeNode; const S: string;
   Ptr: Pointer) : TrmTreeNode;
begin
   if Node <> nil then Node := Node.Parent;
   Result := InternalAddObject(Node, S, Ptr, taAddFirst) ;
end;

function TrmTreeNodes.Add(Node: TrmTreeNode; const S: string) : TrmTreeNode;
begin
   Result := AddObject(Node, S, nil) ;
end;

procedure TrmTreeNodes.Repaint(Node: TrmTreeNode) ;
var
   R: TRect;
begin
   if FUpdateCount < 1 then
   begin
      while (Node <> nil) and not Node.IsVisible do Node := Node.Parent;
      if Node <> nil then
      begin
         R := Node.DisplayRect(False) ;
         InvalidateRect(Owner.Handle, @R, True) ;
      end;
   end;
end;

function TrmTreeNodes.AddObject(Node: TrmTreeNode; const S: string;
   Ptr: Pointer) : TrmTreeNode;
begin
   if Node <> nil then Node := Node.Parent;
   Result := InternalAddObject(Node, S, Ptr, taAdd) ;
end;

function TrmTreeNodes.Insert(Node: TrmTreeNode; const S: string) : TrmTreeNode;
begin
   Result := InsertObject(Node, S, nil) ;
end;

procedure TrmTreeNodes.AddedNode(Value: TrmTreeNode) ;
begin
   if Value <> nil then
   begin
      Value.HasChildren := True;
      Repaint(Value) ;
   end;
end;

function TrmTreeNodes.InsertObject(Node: TrmTreeNode; const S: string;
   Ptr: Pointer) : TrmTreeNode;
var
   Item, ItemId: HTreeItem;
   Parent: TrmTreeNode;
   AddMode: TAddMode;
begin
   Result := Owner.CreateNode;
   try
      Item := nil;
      ItemId := nil;
      Parent := nil;
      AddMode := taInsert;
      if Node <> nil then
      begin
         Parent := Node.Parent;
         if Parent <> nil then Item := Parent.ItemId;
         Node := Node.GetPrevSibling;
         if Node <> nil then ItemId := Node.ItemId
         else AddMode := taAddFirst;
      end;
      Result.Data := Ptr;
      Result.Text := S;
      Item := AddItem(Item, ItemId, CreateItem(Result) , AddMode) ;
      if Item = nil then
         raise EOutOfResources.Create(sInsertError) ;
      Result.FItemId := Item;
      AddedNode(Parent) ;
   except
      Result.Free;
      raise;
   end;
end;

function TrmTreeNodes.InternalAddObject(Node: TrmTreeNode; const S: string;
   Ptr: Pointer; AddMode: TAddMode) : TrmTreeNode;
var
   Item: HTreeItem;
begin
   Result := Owner.CreateNode;
   try
      if Node <> nil then Item := Node.ItemId
      else Item := nil;
      Result.Data := Ptr;
      Result.Text := S;
      Item := AddItem(Item, nil, CreateItem(Result) , AddMode) ;
      if Item = nil then
         raise EOutOfResources.Create(sInsertError) ;
      Result.FItemId := Item;

    //Setting up the path index...
      if not (s = '') then
         BinaryInsert(Owner.NodePath(Result) , Result) ;

      if (FUpdateCount = 0) and (Result = GetFirstNode) then
         SendMessage(Handle, WM_SETREDRAW, 1, 0) ;
      AddedNode(Node) ;
   except
      Result.Free;
      raise;
   end;
end;

function TrmTreeNodes.CreateItem(Node: TrmTreeNode) : TTVItem;
begin
   Node.FInTree := True;
   with Result do
   begin
      mask := TVIF_TEXT or TVIF_PARAM or TVIF_IMAGE or TVIF_SELECTEDIMAGE;
      lParam := Longint(Node) ;
      pszText := LPSTR_TEXTCALLBACK;
      iImage := I_IMAGECALLBACK;
      iSelectedImage := I_IMAGECALLBACK;
   end;
end;

function TrmTreeNodes.AddItem(Parent, Target: HTreeItem;
   const Item: TTVItem; AddMode: TAddMode) : HTreeItem;
var
   InsertStruct: TTVInsertStruct;
begin
   ClearCache;
   with InsertStruct do
   begin
      hParent := Parent;
      case AddMode of
         taAddFirst:
            hInsertAfter := TVI_FIRST;
         taAdd:
            hInsertAfter := TVI_LAST;
         taInsert:
            hInsertAfter := Target;
      end;
   end;
   InsertStruct.item := Item;
   FOwner.FChangeTimer.Enabled := False;
   Result := TreeView_InsertItem(Handle, InsertStruct) ;
end;

function TrmTreeNodes.GetFirstNode: TrmTreeNode;
begin
   Result := GetNode(TreeView_GetRoot(Handle) ) ;
end;

function TrmTreeNodes.GetNodeFromIndex(Index: Integer) : TrmTreeNode;
var
   I: Integer;
begin
   if Index < 0 then TreeViewError(sInvalidIndex) ;
   if (FNodeCache.CacheNode <> nil) and (Abs(FNodeCache.CacheIndex - Index) <= 1) then
   begin
      with FNodeCache do
      begin
         if Index = CacheIndex then Result := CacheNode
         else if Index < CacheIndex then Result := CacheNode.GetPrev
         else Result := CacheNode.GetNext;
      end;
   end
   else
   begin
      Result := GetFirstNode;
      I := Index;
      while (I <> 0) and (Result <> nil) do
      begin
         Result := Result.GetNext;
         Dec(I) ;
      end;
   end;
   if Result = nil then TreeViewError(sInvalidIndex) ;
   FNodeCache.CacheNode := Result;
   FNodeCache.CacheIndex := Index;
end;

function TrmTreeNodes.GetNode(ItemId: HTreeItem) : TrmTreeNode;
var
   Item: TTVItem;
begin
   with Item do
   begin
      hItem := ItemId;
      mask := TVIF_PARAM;
   end;
   if TreeView_GetItem(Handle, Item) then Result := TrmTreeNode(Item.lParam)
   else Result := nil;
end;

procedure TrmTreeNodes.SetItem(Index: Integer; Value: TrmTreeNode) ;
begin
   GetNodeFromIndex(Index) .Assign(Value) ;
end;

procedure TrmTreeNodes.BeginUpdate;
begin
   if FUpdateCount = 0 then SetUpdateState(True) ;
   Inc(FUpdateCount) ;
end;

procedure TrmTreeNodes.SetUpdateState(Updating: Boolean) ;
begin
   SendMessage(Handle, WM_SETREDRAW, Ord(not Updating) , 0) ;
   if not Updating then Owner.Refresh;
end;

procedure TrmTreeNodes.EndUpdate;
begin
   Dec(FUpdateCount) ;
   if FUpdateCount = 0 then SetUpdateState(False) ;
end;

procedure TrmTreeNodes.Assign(Source: TPersistent) ;
var
   TreeNodes: TrmTreeNodes;
   MemStream: TMemoryStream;
   wNode: TrmTreeNode;
begin
   ClearCache;
   if Source is TrmTreeNodes then
   begin
      TreeNodes := TrmTreeNodes(Source) ;
      Clear;
      MemStream := TMemoryStream.Create;
      try
         TreeNodes.WriteData(MemStream) ;
         MemStream.Position := 0;
         ReadData(MemStream) ;
      finally
         MemStream.Free;
      end;

      //Now that we've assigned all the nodes
      //we need to redo that hashlist
      wNode := Self.GetFirstNode;
      while wNode <> nil do
      begin
         wNode.RenewHash;
         wNode := wNode.GetNextSibling;
      end;
   end
   else inherited Assign(Source) ;
end;

procedure TrmTreeNodes.DefineProperties(Filer: TFiler) ;

   function WriteNodes: Boolean;
   var
      I: Integer;
      Nodes: TrmTreeNodes;
   begin
      Nodes := TrmTreeNodes(Filer.Ancestor) ;
      if Nodes = nil then
         Result := Count > 0
      else if Nodes.Count <> Count then
         Result := True
      else
      begin
         Result := False;
         for I := 0 to Count - 1 do
         begin
            Result := not Item[I].IsEqual(Nodes[I]) ;
            if Result then Break;
         end
      end;
   end;

begin
   inherited DefineProperties(Filer) ;
   Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes) ;
end;

procedure TrmTreeNodes.ReadData(Stream: TStream) ;
var
   I, Count: Integer;
   NodeInfo: TNodeInfo;
begin
   if Owner.HandleAllocated then BeginUpdate;
   try
      Clear;
      Stream.ReadBuffer(Count, SizeOf(Count) ) ;
      for I := 0 to Count - 1 do
      begin
         Add(nil, '') .ReadData(Stream, @NodeInfo) ;
      end;
   finally
      if Owner.HandleAllocated then EndUpdate;
   end;
end;

procedure TrmTreeNodes.WriteData(Stream: TStream) ;
var
   I: Integer;
   NodeInfo: TNodeInfo;
   Node: TrmTreeNode;
begin
   I := 0;
   Node := GetFirstNode;
   while Node <> nil do
   begin
      Inc(I) ;
      Node := Node.GetNextSibling;
   end;
   Stream.WriteBuffer(I, SizeOf(I) ) ;
   Node := GetFirstNode;
   while Node <> nil do
   begin
      Node.WriteData(Stream, @NodeInfo) ;
      Node := Node.GetNextSibling;
   end;
end;

procedure TrmTreeNodes.ReadExpandedState(Stream: TStream) ;
var
   ItemCount,
      Index: Integer;
   Node: TrmTreeNode;
   NodeExpanded: Boolean;
begin
   if Stream.Position < Stream.Size then
      Stream.ReadBuffer(ItemCount, SizeOf(ItemCount) )
   else Exit;
   Index := 0;
   Node := GetFirstNode;
   while (Index < ItemCount) and (Node <> nil) do
   begin
      Stream.ReadBuffer(NodeExpanded, SizeOf(NodeExpanded) ) ;
      Node.Expanded := NodeExpanded;
      Inc(Index) ;
      Node := Node.GetNext;
   end;
end;

procedure TrmTreeNodes.WriteExpandedState(Stream: TStream) ;
var
   Size: Integer;
   Node: TrmTreeNode;
   NodeExpanded: Boolean;
begin
   Size := SizeOf(Boolean) * Count;
   Stream.WriteBuffer(Size, SizeOf(Size) ) ;
   Node := GetFirstNode;
   while (Node <> nil) do
   begin
      NodeExpanded := Node.Expanded;
      Stream.WriteBuffer(NodeExpanded, SizeOf(Boolean) ) ;
      Node := Node.GetNext;
   end;
end;

procedure TrmTreeNodes.ClearCache;
begin
   FNodeCache.CacheNode := nil;
end;

type
   TTreeStrings = class(TStrings)
   private
      FOwner: TrmTreeNodes;
   protected
      function Get(Index: Integer) : string; override;
      function GetBufStart(Buffer: PChar; var Level: Integer) : PChar;
      function GetCount: Integer; override;
      function GetObject(Index: Integer) : TObject; override;
      procedure PutObject(Index: Integer; AObject: TObject) ; override;
      procedure SetUpdateState(Updating: Boolean) ; override;
   public
      constructor Create(AOwner: TrmTreeNodes) ;
      function Add(const S: string) : Integer; override;
      procedure Clear; override;
      procedure Delete(Index: Integer) ; override;
      procedure Insert(Index: Integer; const S: string) ; override;
      procedure LoadTreeFromStream(Stream: TStream) ;
      procedure SaveTreeToStream(Stream: TStream) ;
      property Owner: TrmTreeNodes read FOwner;
   end;

constructor TTreeStrings.Create(AOwner: TrmTreeNodes) ;
begin
   inherited Create;
   FOwner := AOwner;
end;

function TTreeStrings.Get(Index: Integer) : string;
const
   TabChar = #9;
var
   Level, I: Integer;
   Node: TrmTreeNode;
begin
   Result := '';
   Node := Owner.GetNodeFromIndex(Index) ;
   Level := Node.Level;
   for I := 0 to Level - 1 do Result := Result + TabChar;
   Result := Result + Node.Text;
end;

function TTreeStrings.GetBufStart(Buffer: PChar; var Level: Integer) : PChar;
begin
   Level := 0;
   while Buffer^ in [' ', #9] do
   begin
      Inc(Buffer) ;
      Inc(Level) ;
   end;
   Result := Buffer;
end;

function TTreeStrings.GetObject(Index: Integer) : TObject;
begin
   Result := Owner.GetNodeFromIndex(Index) .Data;
end;

procedure TTreeStrings.PutObject(Index: Integer; AObject: TObject) ;
begin
   Owner.GetNodeFromIndex(Index) .Data := AObject;
end;

function TTreeStrings.GetCount: Integer;
begin
   Result := Owner.Count;
end;

procedure TTreeStrings.Clear;
begin
   Owner.Clear;
end;

procedure TTreeStrings.Delete(Index: Integer) ;
begin
   Owner.GetNodeFromIndex(Index) .Delete;
end;

procedure TTreeStrings.SetUpdateState(Updating: Boolean) ;
begin
   SendMessage(Owner.Handle, WM_SETREDRAW, Ord(not Updating) , 0) ;
   if not Updating then Owner.Owner.Refresh;
end;

function TTreeStrings.Add(const S: string) : Integer;
var
   Level, OldLevel, I: Integer;
   NewStr: string;
   Node: TrmTreeNode;
begin
   Result := GetCount;
   if (Length(S) = 1) and (S[1] = Chr($1A) ) then Exit;
   Node := nil;
   OldLevel := 0;
   NewStr := GetBufStart(PChar(S) , Level) ;
   if Result > 0 then
   begin
      Node := Owner.GetNodeFromIndex(Result - 1) ;
      OldLevel := Node.Level;
   end;
   if (Level > OldLevel) or (Node = nil) then
   begin
      if Level - OldLevel > 1 then TreeViewError(sInvalidLevel) ;
   end
   else
   begin
      for I := OldLevel downto Level do
      begin
         Node := Node.Parent;
         if (Node = nil) and (I - Level > 0) then
            TreeViewError(sInvalidLevel) ;
      end;
   end;
   Owner.AddChild(Node, NewStr) ;
end;

procedure TTreeStrings.Insert(Index: Integer; const S: string) ;
begin
   with Owner do
      Insert(GetNodeFromIndex(Index) , S) ;
end;

procedure TTreeStrings.LoadTreeFromStream(Stream: TStream) ;
var
   List: TStringList;
   ANode, NextNode: TrmTreeNode;
   ALevel, i: Integer;
   CurrStr: string;
begin
   List := TStringList.Create;
   Owner.BeginUpdate;
   try
      try
         Clear;
         List.LoadFromStream(Stream) ;
         ANode := nil;
         for i := 0 to List.Count - 1 do
         begin
            CurrStr := GetBufStart(PChar(List[i]) , ALevel) ;
            if ANode = nil then
               ANode := Owner.AddChild(nil, CurrStr)
            else if ANode.Level = ALevel then
               ANode := Owner.AddChild(ANode.Parent, CurrStr)
            else if ANode.Level = (ALevel - 1) then
               ANode := Owner.AddChild(ANode, CurrStr)
            else if ANode.Level > ALevel then
            begin
               NextNode := ANode.Parent;
               while NextNode.Level > ALevel do
                  NextNode := NextNode.Parent;
               ANode := Owner.AddChild(NextNode.Parent, CurrStr) ;
            end
            else TreeViewErrorFmt(sInvalidLevelEx, [ALevel, CurrStr]) ;
         end;
      finally
         Owner.EndUpdate;
         List.Free;
      end;
   except
      Owner.Owner.Invalidate; // force repaint on exception
      raise;
   end;
end;

procedure TTreeStrings.SaveTreeToStream(Stream: TStream) ;
const
   TabChar = #9;
   EndOfLine = #13#10;
var
   i: Integer;
   ANode: TrmTreeNode;
   NodeStr: string;
begin
   if Count > 0 then
   begin
      ANode := Owner[0];
      while ANode <> nil do
      begin
         NodeStr := '';
         for i := 0 to ANode.Level - 1 do NodeStr := NodeStr + TabChar;
         NodeStr := NodeStr + ANode.Text + EndOfLine;
         Stream.Write(Pointer(NodeStr) ^, Length(NodeStr) ) ;
         ANode := ANode.GetNext;
      end;
   end;
end;

procedure TrmTreeNodes.BinaryInsert(Path: string;
   Node: TrmTreeNode) ;
var
   wHash: longint;
   wLen: integer;
   wData: TrmHashData;
   First, Middle, Last: longint;
   wFound: boolean;
begin
   wHash := HashValue(Path) ;
   wLen := Length(Path) ;

   First := 0;
   Last := FHashList.count - 1;
   wFound := false;

   while (not wFound) and (first <= last) do
   begin
      middle := round((last + first) / 2) ;
      wData := TrmHashData(fHashlist[middle]) ;

      if wHash = wData.hash then
         wFound := true
      else
      begin
         if wHash < wData.hash then
            last := middle - 1
         else
            first := middle + 1;
      end;
   end;

   if wFound then
   begin
      middle := round((last + first) / 2) ;
      wFound := false;

      while (Middle > 0) and (Middle - 1 >= First) and (TrmHashData(FHashList[middle - 1]) .Hash = wHash) do
         dec(Middle) ;

      while (not wfound) and (Middle < FHashList.Count) and (Middle + 1 < Last) and (TrmHashData(FHashList[middle + 1]) .Hash = wHash) do
      begin
         wData := TrmHashData(FHashList[middle]) ;
         if (Owner.NodePath(wData.Node) = Path) then
            wFound := true
         else
            inc(Middle) ;
      end;
      if not wFound then
         first := middle;
   end;

   if not wfound then
   begin
      wData := TrmHashData.create;
      wData.Hash := wHash;
      wData.IDLength := wLen;
      wData.Node := Node;
      fHashList.Insert(first, wData) ;
   end;
end;

function TrmTreeNodes.HashValue(St: string) : LongInt;
begin
   result := GetStrCRC32(St) ;
end;

function TrmTreeNodes.LocateHashIndex(Path: string) : integer;
var
   wHash: longint;
   wData: TrmHashData;
   First, Middle, Last, Temp: longint;
   wFound: boolean;
begin
   wHash := HashValue(Path) ;

   result := -1;
   First := 0;
   Last := FHashList.count - 1;
   wFound := false;
   middle := round((last + first) / 2) ;

   while (not wFound) and (first <= last) do
   begin
      middle := round((last + first) / 2) ;
      wData := TrmHashData(fHashlist[middle]) ;

      if wHash = wData.hash then
         wFound := true
      else
      begin
         if wHash < wData.hash then
            last := middle - 1
         else
            first := middle + 1;
      end;
   end;

   if wFound then
   begin
      Temp := middle;

      while (Middle > 0) and (Middle - 1 >= First) and (TrmHashData(FHashList[middle - 1]) .Hash = wHash) do
         dec(Middle) ;

      while (result = -1) and (Middle < FHashList.Count) and (Middle + 1 < Last) and (TrmHashData(FHashList[middle + 1]) .Hash = wHash) do
      begin
         wData := TrmHashData(FHashList[middle]) ;
         if (Owner.NodePath(wData.Node) = Path) then
            result := middle
         else
            inc(Middle) ;
      end;

      if result = -1 then
         result := temp;
   end;
end;

procedure TrmTreeNodes.RemoveHash(Node: TrmTreeNode) ;
var
   wIndex: integer;
begin
   wIndex := LocateHashIndex(Owner.NodePath(Node) ) ;
   if wIndex > -1 then
      FHashList.delete(wIndex) ;
end;

function TrmTreeNodes.LocateNode(Path: string) : TrmTreeNode;
var
   wIndex: integer;
begin
   wIndex := LocateHashIndex(Path) ;
   if wIndex = -1 then
      result := nil
   else
      result := TrmHashData(FHashList[wIndex]) .Node;
end;

{ TrmCustomPathTreeView }

constructor TrmCustomPathTreeView.Create(AOwner: TComponent) ;
begin
   inherited Create(AOwner) ;
   ControlStyle := ControlStyle - [csCaptureMouse] + [csDisplayDragImage, csReflector];
   Width := 121;
   Height := 97;
   TabStop := True;
   ParentColor := False;
   FCanvas := TControlCanvas.Create;
   TControlCanvas(FCanvas) .Control := Self;
   FTreeNodes := TrmTreeNodes.Create(Self) ;
   FBorderStyle := bsSingle;
   FShowButtons := True;
   FShowRoot := True;
   FShowLines := True;
   FHideSelection := True;
   FDragImage := TDragImageList.CreateSize(32, 32) ;
   FSaveIndent := -1;
   FChangeTimer := TTimer.Create(Self) ;
   FChangeTimer.Enabled := False;
   FChangeTimer.Interval := 0;
   FChangeTimer.OnTimer := OnChangeTimer;
   FToolTips := True;
   {$ifdef D6_or_higher}
   FEditInstance := Classes.MakeObjectInstance(EditWndProc) ;
   {$else}
   FEditInstance := MakeObjectInstance(EditWndProc) ;
   {$endif}
   FImageChangeLink := TChangeLink.Create;
   FImageChangeLink.OnChange := ImageListChange;
   FStateChangeLink := TChangeLink.Create;
   FStateChangeLink.OnChange := ImageListChange;
  { Version 5.01: DesignInfo is used here to store information necessary for
    deleting font handles allocated in CustomDraw routines. Fields can't be
    added now since class signatures must not be modified in a minor version.
    This will be removed in the next major version }
   if not (csDesigning in ComponentState) then
      DesignInfo := Integer(New(PFontHandles) ) ;

   FSepChar := '/';
end;

destructor TrmCustomPathTreeView.Destroy;
begin
   Try
      SetNewHint(nil) ;
   except
     //Do Nothing...
   end;
   FreeAndNil(FTreeNodes) ;
   FChangeTimer.Free;
   FSaveItems.Free;
   FDragImage.Free;
   FMemStream.Free;
   {$ifdef D6_or_higher}
   Classes.FreeObjectInstance(FEditInstance) ;
   {$else}
   FreeObjectInstance(FEditInstance) ;
   {$endif}
   FImageChangeLink.Free;
   FStateChangeLink.Free;
   FCanvas.Free;
   if not (csDesigning in ComponentState) then
      Dispose(PFontHandles(DesignInfo) ) ;
   inherited Destroy;
end;

procedure TrmCustomPathTreeView.CreateParams(var Params: TCreateParams) ;
const
   BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER) ;
   LineStyles: array[Boolean] of DWORD = (0, TVS_HASLINES) ;
   RootStyles: array[Boolean] of DWORD = (0, TVS_LINESATROOT) ;
   ButtonStyles: array[Boolean] of DWORD = (0, TVS_HASBUTTONS) ;
   EditStyles: array[Boolean] of DWORD = (TVS_EDITLABELS, 0) ;
   HideSelections: array[Boolean] of DWORD = (TVS_SHOWSELALWAYS, 0) ;
   DragStyles: array[TDragMode] of DWORD = (TVS_DISABLEDRAGDROP, 0) ;
   RTLStyles: array[Boolean] of DWORD = (0, TVS_RTLREADING) ;
   ToolTipStyles: array[Boolean] of DWORD = (TVS_NOTOOLTIPS, 0) ;
   AutoExpandStyles: array[Boolean] of DWORD = (0, TVS_SINGLEEXPAND) ;
   HotTrackStyles: array[Boolean] of DWORD = (0, TVS_TRACKSELECT) ;
   RowSelectStyles: array[Boolean] of DWORD = (0, TVS_FULLROWSELECT) ;
begin
   InitCommonControl(ICC_TREEVIEW_CLASSES) ;
   inherited CreateParams(Params) ;
   CreateSubClass(Params, WC_TREEVIEW) ;
   with Params do
   begin
      Style := Style or LineStyles[FShowLines] or BorderStyles[FBorderStyle] or
         RootStyles[FShowRoot] or ButtonStyles[FShowButtons] or
         EditStyles[FReadOnly] or HideSelections[FHideSelection] or
         DragStyles[DragMode] or RTLStyles[UseRightToLeftReading] or
         AutoExpandStyles[FAutoExpand] or HotTrackStyles[FHotTrack] or
         RowSelectStyles[FRowSelect] or TVS_NOTOOLTIPS;
      if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
      begin
         Style := Style and not WS_BORDER;
         ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
      end;
      WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW) ;
   end;
end;

procedure TrmCustomPathTreeView.CreateWnd;
begin
   FStateChanging := False;
   inherited CreateWnd;
   TreeView_SetBkColor(Handle, ColorToRGB(Color) ) ;
   TreeView_SetTextColor(Handle, ColorToRGB(Font.Color) ) ;
   if FMemStream <> nil then
   begin
      Items.ReadData(FMemStream) ;
      Items.ReadExpandedState(FMemStream) ;
      FMemStream.Destroy;
      FMemStream := nil;
      SetTopItem(Items.GetNodeFromIndex(FSaveTopIndex) ) ;
      FSaveTopIndex := 0;
      SetSelection(Items.GetNodeFromIndex(FSaveIndex) ) ;
      FSaveIndex := 0;
   end;
   if FSaveIndent <> -1 then Indent := FSaveIndent;
   if (Images <> nil) and Images.HandleAllocated then
      SetImageList(Images.Handle, TVSIL_NORMAL) ;
   if (StateImages <> nil) and StateImages.HandleAllocated then
      SetImageList(StateImages.Handle, TVSIL_STATE) ;
end;

procedure TrmCustomPathTreeView.DestroyWnd;
var
   Node: TrmTreeNode;
begin
   FStateChanging := True;
   if Items.Count > 0 then
   begin
      FMemStream := TMemoryStream.Create;
      Items.WriteData(FMemStream) ;
      Items.WriteExpandedState(FMemStream) ;
      FMemStream.Position := 0;
      Node := GetTopItem;
      if Node <> nil then FSaveTopIndex := Node.AbsoluteIndex;
      Node := Selected;
      if Node <> nil then FSaveIndex := Node.AbsoluteIndex;
      Items.BeginUpdate;
      try
         Items.Clear;
      finally
         Items.EndUpdate;
      end;
   end;
   FSaveIndent := Indent;
   inherited DestroyWnd;
end;

procedure TrmCustomPathTreeView.EditWndProc(var Message: TMessage) ;
begin
   try
      with Message do
      begin
         case Msg of
            WM_KEYDOWN,
               WM_SYSKEYDOWN: if DoKeyDown(TWMKey(Message) ) then Exit;
            WM_CHAR: if DoKeyPress(TWMKey(Message) ) then Exit;
            WM_KEYUP,
               WM_SYSKEYUP: if DoKeyUp(TWMKey(Message) ) then Exit;
            CN_KEYDOWN,
               CN_CHAR, CN_SYSKEYDOWN,
               CN_SYSCHAR:
               begin
                  WndProc(Message) ;
                  Exit;
               end;
         end;
         Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam) ;
      end;
   except
      Application.HandleException(Self) ;
   end;
end;

procedure TrmCustomPathTreeView.CMColorChanged(var Message: TMessage) ;
begin
   inherited;
   RecreateWnd;
end;

procedure TrmCustomPathTreeView.CMCtl3DChanged(var Message: TMessage) ;
begin
   inherited;
   if FBorderStyle = bsSingle then RecreateWnd;
end;

procedure TrmCustomPathTreeView.CMFontChanged(var Message: TMessage) ;
begin
   inherited;
   TreeView_SetTextColor(Handle, ColorToRGB(Font.Color) ) ;
end;

procedure TrmCustomPathTreeView.CMSysColorChange(var Message: TMessage) ;
begin
   inherited;
   if not (csLoading in ComponentState) then
   begin
      Message.Msg := WM_SYSCOLORCHANGE;
      DefaultHandler(Message) ;
   end;
end;

function TrmCustomPathTreeView.AlphaSort: Boolean;
var
   Node: TrmTreeNode;
begin
   if HandleAllocated then
   begin
      Result := CustomSort(nil, 0) ;
      Node := FTreeNodes.GetFirstNode;
      while Node <> nil do
      begin
         if Node.HasChildren then Node.AlphaSort;
         Node := Node.GetNext;
      end;
   end
   else
      Result := False;
end;

function TrmCustomPathTreeView.CustomSort(SortProc: TTVCompare; Data: Longint) : Boolean;
var
   SortCB: TTVSortCB;
   Node: TrmTreeNode;
begin
   Result := False;
   if HandleAllocated then
   begin
      with SortCB do
      begin
         if not Assigned(SortProc) then lpfnCompare := @DefaultTreeViewSort
         else lpfnCompare := SortProc;
         hParent := TVI_ROOT;
         lParam := Data;
         Result := TreeView_SortChildrenCB(Handle, SortCB, 0) ;
      end;
      Node := FTreeNodes.GetFirstNode;
      while Node <> nil do
      begin
         if Node.HasChildren then Node.CustomSort(SortProc, Data) ;
         Node := Node.GetNext;
      end;
      Items.ClearCache;
   end;
end;

procedure TrmCustomPathTreeView.SetAutoExpand(Value: Boolean) ;
begin
   if FAutoExpand <> Value then
   begin
      FAutoExpand := Value;
      SetComCtlStyle(Self, TVS_SINGLEEXPAND, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetHotTrack(Value: Boolean) ;
begin
   if FHotTrack <> Value then
   begin
      FHotTrack := Value;
      SetComCtlStyle(Self, TVS_TRACKSELECT, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetRowSelect(Value: Boolean) ;
begin
   if FRowSelect <> Value then
   begin
      FRowSelect := Value;
      SetComCtlStyle(Self, TVS_FULLROWSELECT, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetToolTips(Value: Boolean) ;
begin
   if FToolTips <> Value then
      FToolTips := Value;

   Try
      SetNewHint(nil) ;
   except
     //Do Nothing...
   end;
end;

procedure TrmCustomPathTreeView.SetSortType(Value: TSortType) ;
begin
   if SortType <> Value then
   begin
      FSortType := Value;
      if ((SortType in [stData, stBoth]) and Assigned(OnCompare) ) or
         (SortType in [stText, stBoth]) then
         AlphaSort;
   end;
end;

procedure TrmCustomPathTreeView.SetBorderStyle(Value: TBorderStyle) ;
begin
   if BorderStyle <> Value then
   begin
      FBorderStyle := Value;
      RecreateWnd;
   end;
end;

procedure TrmCustomPathTreeView.SetDragMode(Value: TDragMode) ;
begin
   if Value <> DragMode then
      SetComCtlStyle(Self, TVS_DISABLEDRAGDROP, Value = dmManual) ;
   inherited;
end;

procedure TrmCustomPathTreeView.SetButtonStyle(Value: Boolean) ;
begin
   if ShowButtons <> Value then
   begin
      FShowButtons := Value;
      SetComCtlStyle(Self, TVS_HASBUTTONS, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetLineStyle(Value: Boolean) ;
begin
   if ShowLines <> Value then
   begin
      FShowLines := Value;
      SetComCtlStyle(Self, TVS_HASLINES, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetRootStyle(Value: Boolean) ;
begin
   if ShowRoot <> Value then
   begin
      FShowRoot := Value;
      SetComCtlStyle(Self, TVS_LINESATROOT, Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetReadOnly(Value: Boolean) ;
begin
   if ReadOnly <> Value then
   begin
      FReadOnly := Value;
      SetComCtlStyle(Self, TVS_EDITLABELS, not Value) ;
   end;
end;

procedure TrmCustomPathTreeView.SetHideSelection(Value: Boolean) ;
begin
   if HideSelection <> Value then
   begin
      FHideSelection := Value;
      SetComCtlStyle(Self, TVS_SHOWSELALWAYS, not Value) ;
      Invalidate;
   end;
end;

function TrmCustomPathTreeView.GetNodeAt(X, Y: Integer) : TrmTreeNode;
var
   HitTest: TTVHitTestInfo;
begin
   with HitTest do
   begin
      pt.X := X;
      pt.Y := Y;
      if TreeView_HitTest(Handle, HitTest) <> nil then
         Result := Items.GetNode(HitTest.hItem)
      else Result := nil;
   end;
end;

function TrmCustomPathTreeView.GetHitTestInfoAt(X, Y: Integer) : THitTests;
var
   HitTest: TTVHitTestInfo;
begin
   Result := [];
   with HitTest do
   begin
      pt.X := X;
      pt.Y := Y;
      TreeView_HitTest(Handle, HitTest) ;
      if (flags and TVHT_ABOVE) <> 0 then Include(Result, htAbove) ;
      if (flags and TVHT_BELOW) <> 0 then Include(Result, htBelow) ;
      if (flags and TVHT_NOWHERE) <> 0 then Include(Result, htNowhere) ;
      if (flags and TVHT_ONITEM) = TVHT_ONITEM then
         Include(Result, htOnItem)
      else
      begin
         if (flags and TVHT_ONITEM) <> 0 then Include(Result, htOnItem) ;
         if (flags and TVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon) ;
         if (flags and TVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel) ;
         if (flags and TVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon) ;
      end;
      if (flags and TVHT_ONITEMBUTTON) <> 0 then Include(Result, htOnButton) ;
      if (flags and TVHT_ONITEMINDENT) <> 0 then Include(Result, htOnIndent) ;
      if (flags and TVHT_ONITEMRIGHT) <> 0 then Include(Result, htOnRight) ;
      if (flags and TVHT_TOLEFT) <> 0 then Include(Result, htToLeft) ;
      if (flags and TVHT_TORIGHT) <> 0 then Include(Result, htToRight) ;
   end;
end;

procedure TrmCustomPathTreeView.SetrmTreeNodes(Value: TrmTreeNodes) ;
begin
   Items.Assign(Value) ;
end;

procedure TrmCustomPathTreeView.SetIndent(Value: Integer) ;
begin
   if Value <> Indent then TreeView_SetIndent(Handle, Value) ;
end;

function TrmCustomPathTreeView.GetIndent: Integer;
begin
   Result := TreeView_GetIndent(Handle)
end;

procedure TrmCustomPathTreeView.FullExpand;
var
   Node: TrmTreeNode;
begin
   Node := Items.GetFirstNode;
   while Node <> nil do
   begin
      Node.Expand(True) ;
      Node := Node.GetNextSibling;
   end;
end;

procedure TrmCustomPathTreeView.FullCollapse;
var
   Node: TrmTreeNode;
begin
   Node := Items.GetFirstNode;
   while Node <> nil do
   begin
      Node.Collapse(True) ;
      Node := Node.GetNextSibling;
   end;
end;

procedure TrmCustomPathTreeView.Loaded;
begin
   inherited Loaded;
   if csDesigning in ComponentState then FullExpand;
end;

function TrmCustomPathTreeView.GetTopItem: TrmTreeNode;
begin
   if HandleAllocated then
      Result := Items.GetNode(TreeView_GetFirstVisible(Handle) )
   else Result := nil;
end;

procedure TrmCustomPathTreeView.SetTopItem(Value: TrmTreeNode) ;
begin
   if HandleAllocated and (Value <> nil) then
      TreeView_SelectSetFirstVisible(Handle, Value.ItemId) ;
end;

procedure TrmCustomPathTreeView.OnChangeTimer(Sender: TObject) ;
begin
   FChangeTimer.Enabled := False;
   Change(TrmTreeNode(FChangeTimer.Tag) ) ;
end;

function TrmCustomPathTreeView.GetSelection: TrmTreeNode;
begin
   if HandleAllocated then
   begin
      if FRightClickSelect and Assigned(FRClickNode) then
         Result := FRClickNode
      else
         Result := Items.GetNode(TreeView_GetSelection(Handle) ) ;
   end
   else Result := nil;
end;

procedure TrmCustomPathTreeView.SetSelection(Value: TrmTreeNode) ;
begin
   if Value <> nil then Value.Selected := True
   else TreeView_SelectItem(Handle, nil) ;
end;

procedure TrmCustomPathTreeView.SetChangeDelay(Value: Integer) ;
begin
   FChangeTimer.Interval := Value;
end;

function TrmCustomPathTreeView.GetChangeDelay: Integer;
begin
   Result := FChangeTimer.Interval;
end;

function TrmCustomPathTreeView.GetDropTarget: TrmTreeNode;
begin
   if HandleAllocated then
   begin
      Result := Items.GetNode(TreeView_GetDropHilite(Handle) ) ;
      if Result = nil then Result := FLastDropTarget;
   end
   else Result := nil;
end;

procedure TrmCustomPathTreeView.SetDropTarget(Value: TrmTreeNode) ;
begin
   if HandleAllocated then
      if Value <> nil then Value.DropTarget := True
      else TreeView_SelectDropTarget(Handle, nil) ;
end;

function TrmCustomPathTreeView.GetNodeFromItem(const Item: TTVItem) : TrmTreeNode;
begin
   Result := nil;
   if Items <> nil then
      with Item do
         if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
         else Result := Items.GetNode(hItem) ;
end;

function TrmCustomPathTreeView.IsEditing: Boolean;
var
   ControlHand: HWnd;
begin
   ControlHand := TreeView_GetEditControl(Handle) ;
   Result := (ControlHand <> 0) and IsWindowVisible(ControlHand) ;
end;

procedure TrmCustomPathTreeView.CNNotify(var Message: TWMNotify) ;
var
   Node: TrmTreeNode;
   MousePos: TPoint;
   R: TRect;
   DefaultDraw, PaintImages: Boolean;
   TmpItem: TTVItem;
   LogFont: TLogFont;
begin
   with Message do
      case NMHdr^.code of
         NM_CUSTOMDRAW:
            with PNMCustomDraw(NMHdr) ^ do
            begin
               FCanvas.Lock;
               try
                  Result := CDRF_DODEFAULT;
                  if (dwDrawStage and CDDS_ITEM) = 0 then
                  begin
                     R := ClientRect;
                     case dwDrawStage of
                        CDDS_PREPAINT:
                           begin
                              if IsCustomDrawn(dtControl, cdPrePaint) then
                              begin
                                 try
                                    FCanvas.Handle := hdc;
                                    FCanvas.Font := Font;
                                    FCanvas.Brush := Brush;
                                    DefaultDraw := CustomDraw(R, cdPrePaint) ;
                                 finally
                                    FCanvas.Handle := 0;
                                 end;
                                 if not DefaultDraw then
                                 begin
                                    Result := CDRF_SKIPDEFAULT;
                                    Exit;
                                 end;
                              end;
                              if IsCustomDrawn(dtItem, cdPrePaint) or IsCustomDrawn(dtItem, cdPreErase) then
                                 Result := Result or CDRF_NOTIFYITEMDRAW;
                              if IsCustomDrawn(dtItem, cdPostPaint) then
                                 Result := Result or CDRF_NOTIFYPOSTPAINT;
                              if IsCustomDrawn(dtItem, cdPostErase) then
                                 Result := Result or CDRF_NOTIFYPOSTERASE;
                           end;
                        CDDS_POSTPAINT:
                           if IsCustomDrawn(dtControl, cdPostPaint) then
                              CustomDraw(R, cdPostPaint) ;
                        CDDS_PREERASE:
                           if IsCustomDrawn(dtControl, cdPreErase) then
                              CustomDraw(R, cdPreErase) ;
                        CDDS_POSTERASE:
                           if IsCustomDrawn(dtControl, cdPostErase) then
                              CustomDraw(R, cdPostErase) ;
                     end;
                  end else
                  begin
                     FillChar(TmpItem, SizeOf(TmpItem) , 0) ;
                     TmpItem.hItem := HTREEITEM(dwItemSpec) ;
                     Node := GetNodeFromItem(TmpItem) ;
                     if Node = nil then Exit;
                     case dwDrawStage of
                        CDDS_ITEMPREPAINT:
                           begin
                    //release the font we may have loaned during item drawing.
                              if (dwDrawStage and CDDS_ITEMPOSTPAINT <> 0)
                                 and (PFontHandles(DesignInfo) .OurFont + PFontHandles(DesignInfo) .StockFont <> 0) then
                              begin
                                 SelectObject(hdc, PFontHandles(DesignInfo) .StockFont) ;
                                 DeleteObject(PFontHandles(DesignInfo) .OurFont) ;
                                 PFontHandles(DesignInfo) .OurFont := 0;
                                 PFontHandles(DesignInfo) .StockFont := 0;
                              end;

                              try
                                 FCanvas.Handle := hdc;
                                 FCanvas.Font := Font;
                                 FCanvas.Brush := Brush;
                      { Unlike the list view, the tree view doesn't override the text
                        foreground and background colors of selected items. }
                                 if uItemState and CDIS_SELECTED <> 0 then
                                 begin
                                    FCanvas.Font.Color := clHighlightText;
                                    FCanvas.Brush.Color := clHighlight;
                                 end;
                                 FCanvas.Font.OnChange := CanvasChanged;
                                 FCanvas.Brush.OnChange := CanvasChanged;
                                 FCanvasChanged := False;
                                 DefaultDraw := CustomDrawItem(Node,
                                    TCustomDrawState(Word(uItemState) ) , cdPrePaint, PaintImages) ;
                                 if not PaintImages then
                                    Result := Result or TVCDRF_NOIMAGES;
                                 if not DefaultDraw then
                                    Result := Result or CDRF_SKIPDEFAULT
                                 else if FCanvasChanged then
                                 begin
                                    FCanvasChanged := False;
                                    FCanvas.Font.OnChange := nil;
                                    FCanvas.Brush.OnChange := nil;
                                    with PNMTVCustomDraw(NMHdr) ^ do
                                    begin
                                       clrText := ColorToRGB(FCanvas.Font.Color) ;
                                       clrTextBk := ColorToRGB(FCanvas.Brush.Color) ;
                                       if GetObject(FCanvas.Font.Handle, SizeOf(LogFont) , @LogFont) <> 0 then
                                       begin
                                          FCanvas.Handle := 0; // disconnect from hdc
                            // don't delete the stock font
                                          PFontHandles(DesignInfo) .OurFont := CreateFontIndirect(LogFont) ;
                                          PFontHandles(DesignInfo) .StockFont :=
                                             SelectObject(hdc, PFontHandles(DesignInfo) .OurFont) ;
                                          Result := Result or CDRF_NEWFONT;
                                       end;
                                    end;
                                 end;
                                 if IsCustomDrawn(dtItem, cdPostPaint) then
                                    Result := Result or CDRF_NOTIFYPOSTPAINT;
                              finally
                                 FCanvas.Handle := 0;
                              end;
                           end;
                        CDDS_ITEMPOSTPAINT:
                           if IsCustomDrawn(dtItem, cdPostPaint) then
                              CustomDrawItem(Node, TCustomDrawState(Word(uItemState) ) , cdPostPaint, PaintImages) ;
                        CDDS_ITEMPREERASE:
                           if IsCustomDrawn(dtItem, cdPreErase) then
                              CustomDrawItem(Node, TCustomDrawState(Word(uItemState) ) , cdPreErase, PaintImages) ;
                        CDDS_ITEMPOSTERASE:
                           if IsCustomDrawn(dtItem, cdPostErase) then
                              CustomDrawItem(Node, TCustomDrawState(Word(uItemState) ) , cdPostErase, PaintImages) ;
                     end;
                  end;
               finally
                  FCanvas.Unlock;
               end;
            end;
         TVN_BEGINDRAG:
            begin
               FDragged := True;
               with PNMTreeView(NMHdr) ^ do
                  FDragNode := GetNodeFromItem(ItemNew) ;
            end;
         TVN_BEGINLABELEDIT:
            begin
               Try
                  SetNewHint(nil) ;
               except
             //Do Nothing...
               end;
               with PTVDispInfo(NMHdr) ^ do
                  if Dragging or not CanEdit(GetNodeFromItem(item) ) then
                     Result := 1;
               if Result = 0 then
               begin
                  FEditHandle := TreeView_GetEditControl(Handle) ;
                  FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC) ) ;
                  SetWindowLong(FEditHandle, GWL_WNDPROC, LongInt(FEditInstance) ) ;
               end;
            end;
         TVN_ENDLABELEDIT: Edit(PTVDispInfo(NMHdr) ^.item) ;
         TVN_ITEMEXPANDING:
            if not FManualNotify then
            begin
               with PNMTreeView(NMHdr) ^ do
               begin
                  Node := GetNodeFromItem(ItemNew) ;
                  if (action = TVE_EXPAND) and not CanExpand(Node) then
                     Result := 1
                  else if (action = TVE_COLLAPSE) and
                     not CanCollapse(Node) then Result := 1;
               end;
            end;
         TVN_ITEMEXPANDED:
            if not FManualNotify then
            begin
               with PNMTreeView(NMHdr) ^ do
               begin
                  Node := GetNodeFromItem(itemNew) ;
                  if (action = TVE_EXPAND) then Expand(Node)
                  else if (action = TVE_COLLAPSE) then Collapse(Node) ;
               end;
            end;
         TVN_SELCHANGINGA, TVN_SELCHANGINGW:
            if not CanChange(GetNodeFromItem(PNMTreeView(NMHdr) ^.itemNew) ) then
               Result := 1;
         TVN_SELCHANGEDA, TVN_SELCHANGEDW:
            with PNMTreeView(NMHdr) ^ do
               if FChangeTimer.Interval > 0 then
                  with FChangeTimer do
                  begin
                     Enabled := False;
                     Tag := Integer(GetNodeFromItem(itemNew) ) ;
                     Enabled := True;
                  end
               else
                  Change(GetNodeFromItem(itemNew) ) ;
         TVN_DELETEITEM:
            begin
               Node := GetNodeFromItem(PNMTreeView(NMHdr) ^.itemOld) ;
               if Node <> nil then
               begin
                  Node.FItemId := nil;
                  FChangeTimer.Enabled := False;
                  if FStateChanging then Node.Delete
                  else Items.Delete(Node) ;
               end;
            end;
         TVN_SETDISPINFO:
            with PTVDispInfo(NMHdr) ^ do
            begin
               Node := GetNodeFromItem(item) ;
               if (Node <> nil) and ((item.mask and TVIF_TEXT) <> 0) then
                  Node.Text := item.pszText;
            end;
         TVN_GETDISPINFO:
            with PTVDispInfo(NMHdr) ^ do
            begin
               Node := GetNodeFromItem(item) ;
               if Node <> nil then
               begin
                  if (item.mask and TVIF_TEXT) <> 0 then
                     StrLCopy(item.pszText, PChar(Node.Text) , item.cchTextMax) ;
                  if (item.mask and TVIF_IMAGE) <> 0 then
                  begin
                     GetImageIndex(Node) ;
                     item.iImage := Node.ImageIndex;
                  end;
                  if (item.mask and TVIF_SELECTEDIMAGE) <> 0 then
                  begin
                     GetSelectedIndex(Node) ;
                     item.iSelectedImage := Node.SelectedIndex;
                  end;
               end;
            end;
         NM_RCLICK:
            begin
               FRClickNode := nil;
               GetCursorPos(MousePos) ;
               if RightClickSelect then
                  with PointToSmallPoint(ScreenToClient(MousePos) ) do
                  begin
                     FRClickNode := GetNodeAt(X, Y) ;
                     Perform(WM_CONTEXTMENU, Handle, Integer(PointToSmallPoint(MousePos) ) ) ;
                     FRClickNode := nil;
                  end
               else
            // Win95/98 eat WM_CONTEXTMENU when posted to the message queue
                  PostMessage(Handle, CN_BASE + WM_CONTEXTMENU, Handle, Integer(PointToSmallPoint(MousePos) ) ) ;
               Message.Result := 1; // tell treeview not to perform default response
            end;
      end;
end;

function TrmCustomPathTreeView.GetDragImages: TDragImageList;
begin
   if FDragImage.Count > 0 then
      Result := FDragImage else
      Result := nil;
end;

procedure TrmCustomPathTreeView.WndProc(var Message: TMessage) ;
begin
   if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
      (Message.Msg = WM_LBUTTONDBLCLK) ) and not Dragging and
      (DragMode = dmAutomatic) and (DragKind = dkDrag) then
   begin
      if not IsControlMouseMsg(TWMMouse(Message) ) then
      begin
         ControlState := ControlState + [csLButtonDown];
         Dispatch(Message) ;
      end;
   end
   else if Message.Msg = CN_BASE + WM_CONTEXTMENU then
      Message.Result := Perform(WM_CONTEXTMENU, Message.WParam, Message.LParam)
   else inherited WndProc(Message) ;
end;

procedure TrmCustomPathTreeView.DoStartDrag(var DragObject: TDragObject) ;
var
   ImageHandle: HImageList;
   DragNode: TrmTreeNode;
   P: TPoint;
begin
   inherited DoStartDrag(DragObject) ;
   DragNode := FDragNode;
   FLastDropTarget := nil;
   FDragNode := nil;
   if DragNode = nil then
   begin
      GetCursorPos(P) ;
      with ScreenToClient(P) do DragNode := GetNodeAt(X, Y) ;
   end;
   if DragNode <> nil then
   begin
      ImageHandle := TreeView_CreateDragImage(Handle, DragNode.ItemId) ;
      if ImageHandle <> 0 then
         with FDragImage do
         begin
            Handle := ImageHandle;
            SetDragImage(0, 2, 2) ;
         end;
   end;
end;

procedure TrmCustomPathTreeView.DoEndDrag(Target: TObject; X, Y: Integer) ;
begin
   inherited DoEndDrag(Target, X, Y) ;
   FLastDropTarget := nil;
end;

procedure TrmCustomPathTreeView.CMDrag(var Message: TCMDrag) ;
begin
   inherited;
   with Message, DragRec^ do
      case DragMessage of
         dmDragMove:
            with ScreenToClient(Pos) do
               DoDragOver(Source, X, Y, Message.Result <> 0) ;
         dmDragLeave:
            begin
               TDragObject(Source) .HideDragImage;
               FLastDropTarget := DropTarget;
               DropTarget := nil;
               TDragObject(Source) .ShowDragImage;
            end;
         dmDragDrop: FLastDropTarget := nil;
      end;
end;

procedure TrmCustomPathTreeView.DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean) ;
var
   Node: TrmTreeNode;
begin
   Node := GetNodeAt(X, Y) ;
   if (Node <> nil) and
      ((Node <> DropTarget) or (Node = FLastDropTarget) ) then
   begin
      FLastDropTarget := nil;
      TDragObject(Source) .HideDragImage;
      Node.DropTarget := True;
      TDragObject(Source) .ShowDragImage;
   end;
end;

procedure TrmCustomPathTreeView.GetImageIndex(Node: TrmTreeNode) ;
begin
   if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, Node) ;
end;

procedure TrmCustomPathTreeView.GetSelectedIndex(Node: TrmTreeNode) ;
begin
   if Assigned(FOnGetSelectedIndex) then FOnGetSelectedIndex(Self, Node) ;
end;

function TrmCustomPathTreeView.CanChange(Node: TrmTreeNode) : Boolean;
begin
   Result := True;
   if Assigned(FOnChanging) then FOnChanging(Self, Node, Result) ;
end;

procedure TrmCustomPathTreeView.Change(Node: TrmTreeNode) ;
begin
   if Assigned(FOnChange) then FOnChange(Self, Node) ;
end;

procedure TrmCustomPathTreeView.Delete(Node: TrmTreeNode) ;
begin
   if Assigned(FOnDeletion) then FOnDeletion(Self, Node) ;
end;

procedure TrmCustomPathTreeView.Expand(Node: TrmTreeNode) ;
begin
   if Assigned(FOnExpanded) then FOnExpanded(Self, Node) ;
end;

function TrmCustomPathTreeView.CanExpand(Node: TrmTreeNode) : Boolean;
begin
   Result := True;
   if Assigned(FOnExpanding) then FOnExpanding(Self, Node, Result) ;
end;

procedure TrmCustomPathTreeView.Collapse(Node: TrmTreeNode) ;
begin
   if Assigned(FOnCollapsed) then FOnCollapsed(Self, Node) ;
end;

function TrmCustomPathTreeView.CanCollapse(Node: TrmTreeNode) : Boolean;
begin
   Result := True;
   if Assigned(FOnCollapsing) then FOnCollapsing(Self, Node, Result) ;
end;

function TrmCustomPathTreeView.CanEdit(Node: TrmTreeNode) : Boolean;
begin
   Result := True;
   if Assigned(FOnEditing) then FOnEditing(Self, Node, Result) ;
end;

procedure TrmCustomPathTreeView.Edit(const Item: TTVItem) ;
var
   S: string;
   Node: TrmTreeNode;
begin
   with Item do
      if pszText <> nil then
      begin
         S := pszText;
         Node := GetNodeFromItem(Item) ;
         if Assigned(FOnEdited) then FOnEdited(Self, Node, S) ;
         if Node <> nil then Node.Text := S;
      end;
end;

function TrmCustomPathTreeView.CreateNode: TrmTreeNode;
begin
   Result := TrmTreeNode.Create(Items) ;
end;

procedure TrmCustomPathTreeView.SetImageList(Value: HImageList; Flags: Integer) ;
begin
   if HandleAllocated then TreeView_SetImageList(Handle, Value, Flags) ;
end;

procedure TrmCustomPathTreeView.ImageListChange(Sender: TObject) ;
var
   ImageHandle: HImageList;
begin
   if HandleAllocated then
   begin
      if TCustomImageList(Sender) .HandleAllocated then
         ImageHandle := TCustomImageList(Sender) .Handle
      else
         ImageHandle := 0;
      if Sender = Images then
         SetImageList(ImageHandle, TVSIL_NORMAL)
      else if Sender = StateImages then
         SetImageList(ImageHandle, TVSIL_STATE) ;
   end;
end;

procedure TrmCustomPathTreeView.Notification(AComponent: TComponent;
   Operation: TOperation) ;
begin
   inherited Notification(AComponent, Operation) ;
   if Operation = opRemove then
   begin
      if AComponent = Images then Images := nil;
      if AComponent = StateImages then StateImages := nil;
   end;
end;

procedure TrmCustomPathTreeView.SetImages(Value: TCustomImageList) ;
begin
   if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink) ;
   FImages := Value;
   if Images <> nil then
   begin
      Images.RegisterChanges(FImageChangeLink) ;
      Images.FreeNotification(Self) ;
      SetImageList(Images.Handle, TVSIL_NORMAL)
   end
   else SetImageList(0, TVSIL_NORMAL) ;
end;

procedure TrmCustomPathTreeView.SetStateImages(Value: TCustomImageList) ;
begin
   if StateImages <> nil then
      StateImages.UnRegisterChanges(FStateChangeLink) ;
   FStateImages := Value;
   if StateImages <> nil then
   begin
      StateImages.RegisterChanges(FStateChangeLink) ;
      StateImages.FreeNotification(Self) ;
      SetImageList(StateImages.Handle, TVSIL_STATE)
   end
   else SetImageList(0, TVSIL_STATE) ;
end;

procedure TrmCustomPathTreeView.LoadFromFile(const FileName: string) ;
var
   Stream: TStream;
begin
   Stream := TFileStream.Create(FileName, fmOpenRead) ;
   try
      LoadFromStream(Stream) ;
   finally
      Stream.Free;
   end;
end;

procedure TrmCustomPathTreeView.LoadFromStream(Stream: TStream) ;
begin
   with TTreeStrings.Create(Items) do
   try
      LoadTreeFromStream(Stream) ;
   finally
      Free;
   end;
end;

procedure TrmCustomPathTreeView.SaveToFile(const FileName: string) ;
var
   Stream: TStream;
begin
   Stream := TFileStream.Create(FileName, fmCreate) ;
   try
      SaveToStream(Stream) ;
   finally
      Stream.Free;
   end;
end;

procedure TrmCustomPathTreeView.SaveToStream(Stream: TStream) ;
begin
   with TTreeStrings.Create(Items) do
   try
      SaveTreeToStream(Stream) ;
   finally
      Free;
   end;
end;

procedure TrmCustomPathTreeView.WMContextMenu(var Message: TWMContextMenu) ;
var
   R: TRect;
begin
   if (Message.XPos < 0) and (Selected <> nil) then
   begin
      R := Selected.DisplayRect(True) ;
      Message.Pos := PointToSmallPoint(ClientToScreen(Point(R.Left, R.Bottom) ) ) ;
   end;
   inherited;
end;

procedure TrmCustomPathTreeView.WMLButtonDown(var Message: TWMLButtonDown) ;
var
   Node: TrmTreeNode;
   MousePos: TPoint;
begin
   FDragged := False;
   FDragNode := nil;
   try
      inherited;
      if (DragMode = dmAutomatic) and (DragKind = dkDrag) then
      begin
         SetFocus;
         if not FDragged then
         begin
            GetCursorPos(MousePos) ;
            with PointToSmallPoint(ScreenToClient(MousePos) ) do
               Perform(WM_LBUTTONUP, 0, MakeLong(X, Y) ) ;
         end
         else
         begin
            Node := GetNodeAt(Message.XPos, Message.YPos) ;
            if Node <> nil then
            begin
               Node.Focused := True;
               Node.Selected := True;
               BeginDrag(False) ;
            end;
         end;
      end;
   finally
      FDragNode := nil;
   end;
end;

procedure TrmCustomPathTreeView.WMNotify(var Message: TWMNotify) ;
var
   Node: TrmTreeNode;
   MaxTextLen: Integer;
   Pt: TPoint;
begin
   with Message do
      if NMHdr^.code = TTN_NEEDTEXTW then
      begin
      // Work around NT COMCTL32 problem with tool tips >= 80 characters
         GetCursorPos(Pt) ;
         Pt := ScreenToClient(Pt) ;
         Node := GetNodeAt(Pt.X, Pt.Y) ;
         if (Node = nil) or (Node.Text = '') or
            (PToolTipTextW(NMHdr) ^.uFlags and TTF_IDISHWND = 0) then Exit;
         if (GetComCtlVersion >= ComCtlVersionIE4) and (Length(Node.Text) < 80) then
         begin
            inherited;
            Exit;
         end;
         FWideText := Node.Text;
         MaxTextLen := SizeOf(PToolTipTextW(NMHdr) ^.szText) div SizeOf(WideChar) ;
         if Length(FWideText) >= MaxTextLen then
            SetLength(FWideText, MaxTextLen - 1) ;
         PToolTipTextW(NMHdr) ^.lpszText := PWideChar(FWideText) ;
         FillChar(PToolTipTextW(NMHdr) ^.szText, MaxTextLen, 0) ;
         Move(Pointer(FWideText) ^, PToolTipTextW(NMHdr) ^.szText, Length(FWideText) * SizeOf(WideChar) ) ;
         PToolTipTextW(NMHdr) ^.hInst := 0;
         SetWindowPos(NMHdr^.hwndFrom, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or
            SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER) ;
         Result := 1;
      end
      else inherited;
end;

{ CustomDraw support }

procedure TrmCustomPathTreeView.CanvasChanged;
begin
   FCanvasChanged := True;
end;

function TrmCustomPathTreeView.IsCustomDrawn(Target: TCustomDrawTarget;
   Stage: TCustomDrawStage) : Boolean;
begin
  { Tree view doesn't support erase notifications }
   if Stage = cdPrePaint then
   begin
      if Target = dtItem then
         Result := Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem)
      else if Target = dtControl then
         Result := Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) or
            Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem)
      else
         Result := False;
   end
   else
   begin
      if Target = dtItem then
         Result := Assigned(FOnAdvancedCustomDrawItem)
      else if Target = dtControl then
         Result := Assigned(FOnAdvancedCustomDraw) or Assigned(FOnAdvancedCustomDrawItem)
      else
         Result := False;
   end;
end;

function TrmCustomPathTreeView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage) : Boolean;
begin
   Result := True;
   if (Stage = cdPrePaint) and Assigned(FOnCustomDraw) then FOnCustomDraw(Self, ARect, Result) ;
   if Assigned(FOnAdvancedCustomDraw) then FOnAdvancedCustomDraw(Self, ARect, Stage, Result) ;
end;

function TrmCustomPathTreeView.CustomDrawItem(Node: TrmTreeNode; State: TCustomDrawState;
   Stage: TCustomDrawStage; var PaintImages: Boolean) : Boolean;
begin
   Result := True;
   PaintImages := True;
   if (Stage = cdPrePaint) and Assigned(FOnCustomDrawItem) then FOnCustomDrawItem(Self, Node, State, Result) ;
   if Assigned(FOnAdvancedCustomDrawItem) then FOnAdvancedCustomDrawItem(Self, Node, State, Stage, PaintImages, Result) ;
end;

function TrmCustomPathTreeView.ParentName(s: string) : string;
var
   wLen: integer;
begin
   wLen := length(s) ;
   if (wlen > 0) and (s[wLen] = SepChar) then
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   while (wlen > 0) and (s[wLen] <> sepchar) do
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   if (wlen > 0) and (s[wLen] = SepChar) then
      system.Delete(s, wLen, 1) ;
   result := s;
end;

function TrmCustomPathTreeView.ChildName(s: string) : string;
var
   wLen: integer;
begin
   wLen := length(s) ;
   if (wlen > 0) and (s[wLen] = SepChar) then
   begin
      system.Delete(s, wLen, 1) ;
      dec(wLen) ;
   end;
   while (wlen > 0) and (s[wLen] <> sepchar) do
      dec(wLen) ;
   system.delete(s, 1, wLen) ;
   result := s;
end;

function TrmCustomPathTreeView.AddPathNode(Node: TrmTreeNode;
   Path: string) : TrmTreeNode;
var
   wNode, wParent, wChild: TrmTreeNode;
   wPName, wCName: string;
begin
   result := nil;
   if path = '' then
      exit;

   if pos(sepchar, path) <> 1 then
      path := sepchar+path;
      
   wNode := Items.LocateNode(Path) ;
   if wNode = nil then
   begin
      wPName := ParentName(Path) ;
      wCName := ChildName(Path) ;
      wParent := Items.LocateNode(wPName) ;
      if wParent = nil then
         wParent := AddPathNode(nil, wPname) ;
      wChild := Items.AddChild(wParent, wCName) ;

      if assigned(OnGetImageIndex) then
         OnGetImageIndex(self, wChild);
         
      result := wChild;
   end
   else
      result := wNode;
end;

function TrmCustomPathTreeView.FindPathNode(Path: string) : TrmTreeNode;
begin
   result := Items.LocateNode(Path) ;
end;

function TrmCustomPathTreeView.NodePath(Node: TrmTreeNode) : string;
var
   Temp: string;
begin
   Temp := '';

   while Node <> nil do
   begin
      Temp := FSepChar + Node.Text + Temp;
      Node := Node.Parent;
   end;
   Result := Temp;
end;

procedure TrmCustomPathTreeView.SetNewHint(Node: TrmTreeNode) ;
var
   wRect: TRect;
begin
   if FToolTips and assigned(Node) then
   begin
      if assigned(fHint) and (Node.text = fHint.Caption) then
         exit
      else
      begin
         fHint.free;
         fHint := nil;
      end;

      wRect := Node.DisplayRect(true) ;
      if (wRect.Right > Self.Width) then
      begin
         wRect.TopLeft := Self.ClientToScreen(wRect.TopLeft) ;
         wRect.BottomRight := Self.ClientToScreen(wRect.BottomRight) ;

         if not assigned(fHint) then
            fHint := TrmHintWindow.Create(nil) ;
         fHint.Color := clInfoBk;
         fHint.Font.Assign(self.font) ;
         fHint.ActivateHint(wRect, Node.Text) ;
      end;
   end
   else
   begin
      fHint.free;
      fHint := nil;
   end;
end;

function TrmCustomPathTreeView.GetFocussedNode: TrmTreeNode;
begin
   if HandleAllocated then
      Result := Items.GetNode(TreeView_GetSelection(Handle) )
   else
      Result := nil;
end;

procedure TrmCustomPathTreeView.SetFocussedNode(const Value: TrmTreeNode) ;
begin
   Selected := Value; // ie: use SetSelection
end;

procedure TrmCustomPathTreeView.CMCancelMode(var Message: TMessage) ;
begin
   Try
      SetNewHint(nil) ;
   except
      //Do Nothing...
   end;
   inherited;
end;

procedure TrmCustomPathTreeView.CMMouseLeave(var Message: TMessage) ;
begin
   Try
      SetNewHint(nil) ;
   except
      //Do Nothing...
   end;
   inherited;
end;

procedure TrmCustomPathTreeView.MouseDown(Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer) ;
var
   N: TrmTreeNode;
begin
   if (Button = mbRight) and (RightClickSelect) then
   begin
      N := GetNodeAt(X, Y) ;
      if N <> nil then Selected := N;
   end;
   inherited;
end;

procedure TrmCustomPathTreeView.MouseMove(Shift: TShiftState; X,
   Y: Integer) ;
begin
   Try
      if (Application.Active) and (htOnItem in GetHitTestInfoAt(X, y) ) then
         SetNewHint(GetNodeAt(x, y) )
      else
         SetNewHint(nil) ;
   except
     //Do Nothing...
   end;
   inherited;
end;

initialization
   RegisterClass(TrmNodeInfo) ;
end.

