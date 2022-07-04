{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: non-visible components      }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit Explorer;

interface
uses Messages, Windows, Classes, vgSystem, vgTools, Graphics, Controls, Menus,
  Forms, vgCtrls {$IFDEF _D4_}, ImgList, ActnList{$ENDIF};

type
  TExplorerNodes = class;
  TExplorerSource = class;
  TExplorerNodesList = class;
  TExplorerDragObject = class;
{$IFDEF _D4_}
  TExplorerNodesActionLink = class;
  TExplorerNodesActionLinkClass = class of TExplorerNodesActionLink;
{$ENDIF}
  TExplorerClipboard = class;

  TExplorerNodesEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes) of object;
  TExplorerNodesCanActionEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes;
    var Allow: Boolean) of object;
  TExplorerNodesListEvent = procedure (Sender: TObject; List: TExplorerNodesList) of object;
  TExplorerNodesListCanActionEvent = procedure (Sender: TObject; List: TExplorerNodesList;
    var Allow: Boolean) of object;
  TExplorerNodesListCanDropEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes;
    List: TExplorerNodesList; var Allow: Boolean) of object;
  TExplorerNodesListCanPasteEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes;
    List: TExplorerNodesList; var Allow: Boolean) of object;
  TExplorerNodesEditEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes;
    var Text: string) of object;

  TExplorerCanActionEvent = procedure (Sender: TObject; var Allow: Boolean) of object;
  TExplorerCanPasteEvent = procedure (Sender: TObject; List: TExplorerNodesList;
    var Allow: Boolean) of object;
  TExplorerCopyEvent = procedure (Sender: TObject; var Remember: Boolean) of object;
  TExplorerDragDropEvent = procedure (Sender: TObject; Dest: TExplorerNodes;
    List: TExplorerNodesList) of object;
  TExplorerDragOverEvent = procedure (Sender: TObject; Dest: TExplorerNodes;
    List: TExplorerNodesList; var Accept: Boolean) of object;
  TExplorerEditEvent = procedure (Sender: TObject; var Text: string) of object;

  TExplorerNodesProc = procedure (ExplorerSource: TExplorerSource; Data: Pointer; Event: Integer);

  TExplorerClipboardState = (csCopy, csCut);
  TExplorerClipboardMode  = (cmNormal, cmParent);
  TExplorerNodeType       = (ntFolder, ntNode, ntSeparator);
  TExplorerNodeTypes      = set of TExplorerNodeType;
  TExplorerNodeState      = (nsChanged, nsChangedFolders, nsChangedNodes, nsChangedSeparators, nsChecked,
    nsEnabled, nsExpandLock, nsRadioItem, nsVisible, nsExpanded, nsSorted, nsParentDrawing);
  TExplorerNodeStates     = set of TExplorerNodeState;

  TExplorerImageListType  = (ilLarge, ilSmall, ilState);
  TExplorerImageListTypes = set of TExplorerImageListType;

{ TExplorerImageList }
{$IFDEF _D4_}
  TExplorerImageList = TCustomImageList;
{$ELSE}
  TExplorerImageList = TImageList;
{$ENDIF}

{ TExplorerNodes }
  TExplorerNodes = class(TItemList)
  private
{$IFDEF _D4_}
    FActionLink: TExplorerNodesActionLink;
{$ENDIF}
    FData: Pointer;
    FState: TExplorerNodeStates;
    FExpandCount: Integer;
    FImages: array [0..3] of Integer;
    FSubItems: TStrings;
    FExplorerSources: TList;
    FText: string;
    FPopupMenu: TPopupMenu;
    FReferences: TList;
    procedure ClearReferences;
{$IFDEF _D4_}
    procedure DoActionChange(Sender: TObject);
{$ENDIF}
    procedure ForEachExplorerSource(Sender: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetExpanded: Boolean;
    function GetExplorerSource(Index: Integer): TExplorerSource;
    function GetExplorerSourceCount: Integer;
    function GetItem(Index: Integer): TExplorerNodes;
    function GetParent: TExplorerNodes;
    function GetParentDrawing: Boolean;
    function GetRadioItem: Boolean;
    function GetSubItems: TStrings;
    function GetVisible: Boolean;
    procedure InsertExplorerSource(ExplorerSource: TExplorerSource);
    procedure RemoveExplorerSource(ExplorerSource: TExplorerSource);
    procedure SetChecked(Value: Boolean);
    procedure SetExpanded(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetImage(Index: Integer; Value: Integer);
    procedure SetParent(Value: TExplorerNodes);
    procedure SetParentDrawing(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetRadioItem(Value: Boolean);
    procedure SetSubItems(Value: TStrings);
    procedure SetText(Value: string);
    procedure SetVisible(Value: Boolean);
    function StoreChecked: Boolean;
    function StoreEnabled: Boolean;
    function StoreImage(Index: Integer): Boolean;
    function StoreSubItems: Boolean;
    function StoreText: Boolean;
    function StoreVisible: Boolean;
    procedure SubItemsChanged(Sender: TObject);
    procedure UpdateSiblings;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
  protected
{$IFDEF _D4_}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
{$ENDIF}
    procedure ClipboardNotification(Operation: TOperation); dynamic;
    procedure DoCheck; dynamic;
    procedure DoCopy; dynamic;
    procedure DoCopyList(List: TExplorerNodesList); dynamic;
    procedure DoCut; dynamic;
    procedure DoCutList(List: TExplorerNodesList); dynamic;
    procedure DoDblClick; dynamic;
    procedure DoDelete; dynamic;
    procedure DoDeleteList(List: TExplorerNodesList); dynamic;
    procedure DoEdit(var Text: string); dynamic;
    procedure DoPaste(List: TExplorerNodesList); dynamic;
{$IFDEF _D4_}
    function GetAction: TBasicAction;
    function GetActionLinkClass: TExplorerNodesActionLinkClass; dynamic;
{$ENDIF}
    procedure GetChildren(Proc: TGetChildProc{$IFDEF _D3_}; Root: TComponent{$ENDIF}); override;
    function GetItemName: string; override;
    function GetNodeType: TExplorerNodeType; virtual;
    function GetLargeImages: TExplorerImageList; dynamic;
    function GetSmallImages: TExplorerImageList; dynamic;
    function GetStateImages: TExplorerImageList; dynamic;
    procedure InternalChanged; virtual;
    procedure InternalChangedChildren; virtual;
    procedure InternalGetDrawingParams(ExplorerNodes: TExplorerNodes; AFont: TFont; var ABkgnd: TColor); virtual;
    procedure InternalExpand; virtual;
    procedure InternalSelect; virtual;
    procedure ItemListEvent(Item: TItem; Event: Integer); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChange;
    procedure NotifyChangeChildren(NodeTypes: TExplorerNodeTypes);
    procedure SetName(const Value: TComponentName); override;
    procedure SortedChanged; dynamic;
{$IFDEF _D4_}
    procedure SetAction(Value: TBasicAction);
    property ActionLink: TExplorerNodesActionLink read FActionLink write FActionLink;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AbsoluteIndex(ExplorerNodes: TExplorerNodes): Integer;
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; dynamic;
    function AcceptsNodesList(List: TExplorerNodesList): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure BeginExpand;
    function ClipboardMode: TExplorerClipboardMode; dynamic;
    function CanCopy: Boolean; dynamic;
    function CanCopyList(List: TExplorerNodesList): Boolean; dynamic;
    function CanCut: Boolean; dynamic;
    function CanCutList(List: TExplorerNodesList): Boolean; dynamic;
    function CanDelete: Boolean; dynamic;
    function CanDeleteList(List: TExplorerNodesList): Boolean; dynamic;
    function CanDrag: Boolean; dynamic;
    function CanDrop(List: TExplorerNodesList): Boolean; dynamic;
    function CanEdit: Boolean; dynamic;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; dynamic;
    function CanPaste(List: TExplorerNodesList): Boolean; dynamic;
    procedure Changed;
    procedure ChangedChildren(NodeType: TExplorerNodeType);
    procedure Clear; override;
    procedure Collapse;
    procedure Copy;
    procedure Cut;
    procedure DblClick;
    procedure Delete;
    procedure DeleteList(List: TExplorerNodesList);
    procedure DefaultSort; virtual;
    procedure DisableControls;
    procedure DragDrop(List: TExplorerNodesList); dynamic;
    procedure Paste;
    procedure EnableControls;
    procedure EndExpand;
    procedure EndUpdate; override;
    procedure Expand;
    function HasChildrenOfType(NodeType: TExplorerNodeType): Boolean;
    function HasRefs(Ignore: Boolean): Boolean;
    function IsFolder: Boolean;
    function IsNode: Boolean;
    function IsParentOf(Item: TExplorerNodes): Boolean;
    function IsRunTime: Boolean; virtual;
    function IsSeparator: Boolean;
    function IsStored: Boolean; virtual;
    procedure Refresh;
    procedure Select;
    procedure Sort(Compare: TListSortCompare); override;
    procedure AnsiSort(Descending: Boolean);
    procedure GetDrawingParams(ExplorerNodes: TExplorerNodes; AFont: TFont; var ABkgnd: TColor);
    procedure InvalidateControls(Children: Boolean);
{$IFDEF _D4_}
    property Action: TBasicAction read GetAction write SetAction;
{$ENDIF}
    property Checked: Boolean read GetChecked write SetChecked  stored StoreChecked default False;
    property Data: Pointer read FData write FData;
    property Enabled: Boolean read GetEnabled write SetEnabled stored StoreEnabled default True;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property ExplorerSourceCount: Integer read GetExplorerSourceCount;
    property ExplorerSources[Index: Integer]: TExplorerSource read GetExplorerSource;
    property ImageIndex: Integer index 0 read FImages[0] write SetImage stored StoreImage default -1;
    property Index;
    property Items[Index: Integer]: TExplorerNodes read GetItem; default;
    property NodeType: TExplorerNodeType read GetNodeType;
    property OverlayIndex: Integer index 1 read FImages[1] write SetImage default -1;
    property Parent: TExplorerNodes read GetParent write SetParent;
    property ParentDrawing: Boolean read GetParentDrawing write SetParentDrawing default False;
    property SelectedIndex: Integer index 2 read FImages[2] write SetImage stored StoreImage default -1;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property StateIndex: Integer index 3 read FImages[3] write SetImage default -1;
    property SubItems: TStrings read GetSubItems write SetSubItems stored StoreSubItems;
    property RadioItem: Boolean read GetRadioItem write SetRadioItem default False;
    property Text: string read FText write SetText stored StoreText;
    property Visible: Boolean read GetVisible write SetVisible stored StoreVisible default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  TExplorerNodesClass = class of TExplorerNodes;

{$IFDEF _D4_}
{ TExplorerNodesActionLink }
  TExplorerNodesActionLink = class(TActionLink)
  protected
    FClient: TExplorerNodes;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;
{$ENDIF}

{ TExplorerNode }
  TExplorerNode = class(TExplorerNodes)
  end;

{ TExplorerRootNode }
  TExplorerRootNode = class(TExplorerNodes)
  public
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
  published
    property Checked;
    property ImageIndex;
    property OverlayIndex;
    property PopupMenu;
    property SelectedIndex;
    property Sorted;
    property StateIndex;
    property SubItems;
    property Text;
  end;

{ TExplorerInterfaceNode }
  TExplorerInterfaceNode = class(TExplorerNode)
  private
    FEnableCopy, FEnableDelete, FEnableDrag, FEnableDrop, FEnableEdit, FEnablePaste: Boolean;
    FImageLists: array [0..2] of TExplorerImageList;
    FOnCanCopy, FOnCanCut, FOnCanDelete, FOnCanDrag, FOnEditing: TExplorerCanActionEvent;
    FOnCanPaste: TExplorerCanPasteEvent;
    FOnCanDrop: TExplorerDragOverEvent;
    FOnCheck, FOnCopy, FOnDelete, FOnPaste: TNotifyEvent;
    FOnDragDrop: TExplorerDragDropEvent;
    FOnEdit: TExplorerEditEvent;
    procedure SetImageList(Index: Integer; Value: TExplorerImageList);
  protected
    procedure DoCheck; override;
    procedure DoCopy; override;
    procedure DoDelete; override;
    procedure DoEdit(var Text: string); override;
    procedure DoPaste(List: TExplorerNodesList); override;
    function GetLargeImages: TExplorerImageList; override;
    function GetSmallImages: TExplorerImageList; override;
    function GetStateImages: TExplorerImageList; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    function CanCopy: Boolean; override;
    function CanCut: Boolean; override;
    function CanDelete: Boolean; override;
    function CanDrag: Boolean; override;
    function CanDrop(List: TExplorerNodesList): Boolean; override;
    function CanEdit: Boolean; override;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    function CanPaste(List: TExplorerNodesList): Boolean; override;
    procedure DragDrop(List: TExplorerNodesList); override;
  published
    property Checked;
    property EnableCopy: Boolean read FEnableCopy write FEnableCopy default False;
    property EnableDelete: Boolean read FEnableDelete write FEnableDelete default False;
    property EnableDrag: Boolean read FEnableDrag write FEnableDrag default False;
    property EnableDrop: Boolean read FEnableDrop write FEnableDrop default False;
    property EnableEdit: Boolean read FEnableEdit write FEnableEdit default False;
    property EnablePaste: Boolean read FEnablePaste write FEnablePaste default False;
    property ImageIndex;
    property Index;
    property LargeImages: TExplorerImageList index 0 read FImageLists[0] write SetImageList;
    property OverlayIndex;
    property ParentDrawing;
    property PopupMenu;
    property RadioItem;
    property SelectedIndex;
    property SmallImages: TExplorerImageList index 1 read FImageLists[1] write SetImageList;
    property Sorted;
    property StateImages: TExplorerImageList index 2 read FImageLists[2] write SetImageList;
    property StateIndex;
    property SubItems;
    property Text;
    property Visible;
    property OnCanCopy: TExplorerCanActionEvent read FOnCanCopy write FOnCanCopy;
    property OnCanCut: TExplorerCanActionEvent read FOnCanCut write FOnCanCut;
    property OnCanDelete: TExplorerCanActionEvent read FOnCanDelete write FOnCanDelete;
    property OnCanDrag: TExplorerCanActionEvent read FOnCanDrag write FOnCanDrag;
    property OnCanDrop: TExplorerDragOverEvent read FOnCanDrop write FOnCanDrop;
    property OnCanPaste: TExplorerCanPasteEvent read FOnCanPaste write FOnCanPaste;
    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
    property OnCopy: TNotifyEvent read FOnCopy write FOnCopy;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property OnEdit: TExplorerEditEvent read FOnEdit write FOnEdit;
    property OnEditing: TExplorerCanActionEvent read FOnEditing write FOnEditing;
    property OnPaste: TNotifyEvent read FOnPaste write FOnPaste;
  end;

{ TExplorerSeparatorNode }
  TExplorerSeparatorNode = class(TExplorerInterfaceNode)
  protected
    function GetNodeType: TExplorerNodeType; override;
  end;

{ TExplorerFolderNode }
  TExplorerFolderNode = class(TExplorerInterfaceNode)
  private
    FOnGetChildrenParams: TExplorerNodesEvent;
  protected
    procedure DoGetChildrenParams(ExplorerNodes: TExplorerNodes); dynamic;
    procedure InsertItem(Item: TItem); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnGetChildrenParams: TExplorerNodesEvent read FOnGetChildrenParams write FOnGetChildrenParams;
  end;

{ TExplorerActionNode }
  TExplorerActionNode = class(TExplorerInterfaceNode)
  private
    FOnClick: TNotifyEvent;
    function StoreOnClick: Boolean;
  protected
    procedure DoDblClick; override;
    function GetNodeType: TExplorerNodeType; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Click; dynamic;
  published
{$IFDEF _D4_}
    property Action;
{$ENDIF}
    property Enabled;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored StoreOnClick;
  end;

{ TExplorerStringsNode }
  TExplorerStringsNode = class(TExplorerFolderNode)
  private
    FEnableDragChildren, FEnableDropChildren, FEnableOrderChildren: Boolean;
    FLines: TStrings;
    procedure SetEnableDragChildren(Value: Boolean);
    procedure SetEnableOrderChildren(Value: Boolean);
    procedure SetLines(Value: TStrings);
  protected
    procedure SortedChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
  published
    property EnableDragChildren: Boolean read FEnableDragChildren write SetEnableDragChildren default True;
    property EnableDropChildren: Boolean read FEnableDropChildren write FEnableDropChildren default True;
    property EnableOrderChildren: Boolean read FEnableOrderChildren write SetEnableOrderChildren default False;
    property Lines: TStrings read FLines write SetLines;
  end;

{ TExplorerFormNode }
  TExplorerFormNode = class(TExplorerActionNode)
  private
    FFormClassName: TClassName;
    FForm: TCustomForm;
    FOnChange: TNotifyEvent;
    procedure SetForm(Value: TCustomForm);
  protected
    procedure DoChange; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function CreateForm(AOwner: TComponent; Assign: Boolean): TCustomForm;
    function FormNeeded(AOwner: TComponent): TCustomForm;
    property Form: TCustomForm read FForm write SetForm;
  published
    property FormClassName: TClassName read FFormClassName write FFormClassName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TExplorerLink = class;

{ TExplorerNodesList }
  TExplorerNodesList = class
  private
    FItems: TList;
    FExplorerLink: TExplorerLink;
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetItem(Index: Integer): TExplorerNodes;
  protected
    procedure Notification(ExplorerNodes: TExplorerNodes; Operation: TOperation); dynamic;
  public
    constructor Create(IgnoreRefs: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure AddItem(ExplorerNodes: TExplorerNodes);
    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Items[Index: Integer]: TExplorerNodes read GetItem; default;
  end;

{ TExplorerClipboard }
  TExplorerClipboard = class
  private
    FItems: TExplorerNodesList;
    FState: TExplorerClipboardState;
  protected
    procedure GetItems(List: TExplorerNodesList); dynamic;
    property Items: TExplorerNodesList read FItems;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; dynamic;
    procedure Close; dynamic;
    procedure Open; dynamic;
    procedure Copy(List: TExplorerNodesList); dynamic;
    procedure Cut(List: TExplorerNodesList); dynamic;
    procedure Paste(ExplorerNodes: TExplorerNodes); dynamic;
    property State: TExplorerClipboardState read FState;
  end;

  TExplorerClipboardClass = class of TExplorerClipboard;

{ TExplorerSource }
  TExplorerSource = class(TComponent)
  private
    FImageLists: array [0..5] of TExplorerImageList;
    FImages: TExplorerImageListTypes;
    FLinks: TList;
    FExplorerRoot: TExplorerNodes;
    FDisableCount: Integer;
    FOnActiveChange: TNotifyEvent;
    FOnEdit: TExplorerNodesEditEvent;
    FOnCanCopy, FOnCanCut, FOnCanDelete: TExplorerNodesListCanActionEvent;
    FOnCanDrop: TExplorerNodesListCanDropEvent;
    FOnCanPaste: TExplorerNodesListCanPasteEvent;
    FOnEditing: TExplorerNodesCanActionEvent;
    FOnCopy, FOnCut, FOnDelete: TExplorerNodesListEvent;
    FOnDblClick, FOnExpanding: TExplorerNodesEvent;
    FOnDragDrop: TExplorerDragDropEvent;
    function GetDisabled: Boolean;
    procedure NotifyExplorerLinks(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
    procedure InsertExplorerLink(ExplorerLink: TExplorerLink);
    procedure RemoveExplorerLink(ExplorerLink: TExplorerLink);
    procedure SetExplorerRoot(Value: TExplorerNodes);
    procedure SetImageList(Index: Integer; Value: TExplorerImageList);
    procedure SetImages(Value: TExplorerImageListTypes);
  protected
    procedure DoActiveChanged; virtual;
    procedure ExplorerEvent(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function CanCopy(List: TExplorerNodesList): Boolean; dynamic;
    function CanCut(List: TExplorerNodesList): Boolean; dynamic;
    function CanDelete(List: TExplorerNodesList): Boolean; dynamic;
    function CanDrop(Dest: TExplorerNodes; List: TExplorerNodesList): Boolean; dynamic;
    function CanPaste(ExplorerNodes: TExplorerNodes): Boolean; dynamic;
    procedure DoCopy(List: TExplorerNodesList); dynamic;
    procedure DoCut(List: TExplorerNodesList); dynamic;
    procedure DoDelete(List: TExplorerNodesList); dynamic;
    procedure DoDblClick(ExplorerNodes: TExplorerNodes); dynamic;
    procedure DoDragDrop(Dest: TExplorerNodes; List: TExplorerNodesList); dynamic;
    procedure DoEdit(ExplorerNodes: TExplorerNodes; var Text: string); dynamic;
    procedure DoEditing(ExplorerNodes: TExplorerNodes; var Allow: Boolean); dynamic;
    procedure DoExpanding(ExplorerNodes: TExplorerNodes); dynamic;
    procedure DisableControls;
    procedure EnableControls;
    function GetImageIndex(ExplorerNodes: TExplorerNodes; Large: Boolean): Integer;
    function GetLargeImages: TExplorerImageList;
    function GetOverlayIndex(ExplorerNodes: TExplorerNodes): Integer;
    function GetSelectedIndex(ExplorerNodes: TExplorerNodes): Integer;
    function GetSmallImages: TExplorerImageList;
    function GetStateIndex(ExplorerNodes: TExplorerNodes): Integer;
    function GetStateImages: TExplorerImageList;
    property Disabled: Boolean read GetDisabled;
  published
    property ExplorerRoot: TExplorerNodes read FExplorerRoot write SetExplorerRoot;
    property Images: TExplorerImageListTypes read FImages write SetImages default [];
    property LargeImages: TExplorerImageList index 0 read FImageLists[0] write SetImageList;
    property SmallImages: TExplorerImageList index 1 read FImageLists[1] write SetImageList;
    property StateImages: TExplorerImageList index 2 read FImageLists[2] write SetImageList;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
    property OnCanCopy: TExplorerNodesListCanActionEvent read FOnCanCopy write FOnCanCopy;
    property OnCanCut: TExplorerNodesListCanActionEvent read FOnCanCut write FOnCanCut;
    property OnCanDelete: TExplorerNodesListCanActionEvent read FOnCanDelete write FOnCanDelete;
    property OnCanDrop: TExplorerNodesListCanDropEvent read FOnCanDrop write FOnCanDrop;
    property OnCanPaste: TExplorerNodesListCanPasteEvent read FOnCanPaste write FOnCanPaste;
    property OnCopy: TExplorerNodesListEvent read FOnCopy write FOnCopy;
    property OnDelete: TExplorerNodesListEvent read FOnDelete write FOnDelete;
    property OnDblClick: TExplorerNodesEvent read FOnDblClick write FOnDblClick;
    property OnExpanding: TExplorerNodesEvent read FOnExpanding write FOnExpanding;
    property OnEdit: TExplorerNodesEditEvent read FOnEdit write FOnEdit;
    property OnEditing: TExplorerNodesCanActionEvent read FOnEditing write FOnEditing;
    property OnDragDrop: TExplorerDragDropEvent read FOnDragDrop write FOnDragDrop;
  end;

{ TExplorerLink }
  TExplorerAcceptNodeEvent = procedure (Sender: TObject; ExplorerNodes: TExplorerNodes;
    var Accept: Boolean) of object;

  TExplorerLink = class
  private
    FExplorerSource: TExplorerSource;
    FIgnoreRefs: Boolean;
    FNodeTypes: TExplorerNodeTypes;
    FDrawing: Boolean;
    FOnAcceptNode: TExplorerAcceptNodeEvent;
    function FindRefData(ExplorerNodes: TExplorerNodes): Pointer;
    procedure SetExplorerSource(Value: TExplorerSource);
    procedure SetNodeTypes(Value: TExplorerNodeTypes);
    procedure SetOnAcceptNode(Value: TExplorerAcceptNodeEvent);
    procedure SetDrawing(Value: Boolean);
  public
    constructor Create(IgnoreRefs: Boolean); virtual;
    destructor Destroy; override;
    function AcceptIndex(Sender: TObject; ExplorerNodes: TExplorerNodes): Integer;
    function AcceptsChildren(Sender: TObject; ExplorerNodes: TExplorerNodes): Boolean;
    function AcceptsNodes(Sender: TObject; ExplorerNodes: TExplorerNodes): Boolean;
    procedure ExplorerEvent(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes); virtual;
    procedure InsertReference(ExplorerNodes: TExplorerNodes; AObject: TObject);
    procedure RemoveReference(ExplorerNodes: TExplorerNodes);
    function FindReference(ExplorerNodes: TExplorerNodes): Pointer;
    property ExplorerSource: TExplorerSource read FExplorerSource write SetExplorerSource;
    property Drawing: Boolean read FDrawing write SetDrawing;
    property NodeTypes: TExplorerNodeTypes read FNodeTypes write SetNodeTypes;
    property OnAcceptNode: TExplorerAcceptNodeEvent read FOnAcceptNode write SetOnAcceptNode;
  end;

{ TExplorerDragObject }
  TExplorerDragObject = class(TDragObject)
  private
    FList: TExplorerNodesList;
    FControl: TControl;
  protected
{$IFDEF _D4_}
    function GetDragImages: TDragImageList; override;
{$ELSE}
    function GetDragImages: TCustomImageList; override;
{$ENDIF}
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure HideDragImage; override;
    procedure ShowDragImage; override;
    property Control: TControl read FControl;
    property Items: TExplorerNodesList read FList;
  end;

  TRegisterExplorerNodesProc = procedure(ExplorerNodesClass: TExplorerNodesClass);
  TExplorerNodesCompareProc = function (ExplorerNodes: TExplorerNodes; Data: Pointer): Boolean;

function ExplorerClipboard: TExplorerClipboard;
function ExplorerFormClassList: TClassList;

function FindExplorerNode(Root: TExplorerNodes; FromRoot: Boolean; Data: Pointer;
  Compare: TExplorerNodesCompareProc): TExplorerNodes;

function FindChildExplorerNode(Root: TExplorerNodes; Data: Pointer;
  Compare: TExplorerNodesCompareProc): TExplorerNodes;
  
procedure AnsiSortChildren(ExplorerNodes: TExplorerNodes);

procedure RegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
procedure RegisterExplorerNodesClasses(ExplorerNodesClasses: array of TExplorerNodesClass);

procedure UnRegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
procedure UnRegisterExplorerNodesClasses(ExplorerNodesClasses: array of TExplorerNodesClass);

var
  ExplorerClipboardClass: TExplorerClipboardClass = TExplorerClipboard;
  RegisterExplorerNodesProc: TRegisterExplorerNodesProc = nil;
  UnRegisterExplorerNodesProc: TRegisterExplorerNodesProc = nil;

const
  eeActiveChange          = ieItemListLast - $1;
  eeNodesChange           = ieItemListLast - $2;
  eeNodesChangeChildren   = ieItemListLast - $3;
  eeDisableControls       = ieItemListLast - $4;
  eeEnableControls        = ieItemListLast - $5;
  eeLargeImagesChange     = ieItemListLast - $6;
  eeSmallImagesChange     = ieItemListLast - $7;
  eeStateImagesChange     = ieItemListLast - $8;
  eeSetSelected           = ieItemListLast - $9;
  eeInvalidate            = ieItemListLast - $A;

  ntNodeTypesAll = [ntFolder, ntNode, ntSeparator];


implementation
uses Consts, SysUtils, ClipBrd, vgUtils, vgVCLUtl;

type
  TControlHack = class(TControl);
  
var
  FExplorerClipboard: TExplorerClipboard = nil;
  FExplorerFormClassList: TClassList = nil;

function ExplorerClipboard: TExplorerClipboard;
begin
  if not Assigned(FExplorerClipboard) then
    FExplorerClipboard := ExplorerClipboardClass.Create;
  Result := FExplorerClipboard;
end;

function ExplorerFormClassList: TClassList;
begin
  if not Assigned(FExplorerFormClassList) then
    FExplorerFormClassList := TClassList.Create;
  Result := FExplorerFormClassList;
end;

function FindExplorerNode(Root: TExplorerNodes; FromRoot: Boolean; Data: Pointer;
  Compare: TExplorerNodesCompareProc): TExplorerNodes;
var
  I: Integer;
begin
  if FromRoot and Compare(Root, Data) then
  begin
    Result := Root;
    Exit;
  end;
  for I := 0 to Root.Count - 1 do
  begin
    Result := FindExplorerNode(Root[I], True, Data, Compare);
    if Assigned(Result) then Exit;
  end;
  Result := nil;
end;

function FindChildExplorerNode(Root: TExplorerNodes; Data: Pointer;
  Compare: TExplorerNodesCompareProc): TExplorerNodes;
var
  I: Integer;
begin
  for I := 0 to Root.Count - 1 do
  begin
    Result := Root[I];
    if Compare(Result, Data) then Exit;
  end;
  Result := nil;
end;

procedure AnsiSortChildren(ExplorerNodes: TExplorerNodes);
begin
  ExplorerNodes.AnsiSort(False);
end;

procedure RegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
begin
  RegisterClass(ExplorerNodesClass);
  if Assigned(RegisterExplorerNodesProc) then RegisterExplorerNodesProc(ExplorerNodesClass);
end;

procedure RegisterExplorerNodesClasses(ExplorerNodesClasses: array of TExplorerNodesClass);
var
  I: Integer;
begin
  for I := Low(ExplorerNodesClasses) to High(ExplorerNodesClasses) do
    RegisterExplorerNodes(ExplorerNodesClasses[I]);
end;

procedure UnRegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
begin
  if Assigned(RegisterExplorerNodesProc) then RegisterExplorerNodesProc(ExplorerNodesClass);
end;

procedure UnRegisterExplorerNodesClasses(ExplorerNodesClasses: array of TExplorerNodesClass);
var
  I: Integer;
begin
  for I := Low(ExplorerNodesClasses) to High(ExplorerNodesClasses) do
    UnRegisterExplorerNodes(ExplorerNodesClasses[I]);
end;

type
  PRefData = ^TRefData;
  TRefData = record
    ExplorerLink: TExplorerLink;
    Data: TObject;
    Ignore: Boolean;
  end;

  TImageEntry = class(TComponent)
  private
    FImageLists: array[0..1] of TExplorerImageList;
    FDestIndex, FSourceIndex: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor CreateNew(Source, Dest: TExplorerImageList; ImageIndex: Integer);
    destructor Destroy; override;
  end;

var
  ImageEntries: TList = nil;

{ TImageEntry }
constructor TImageEntry.CreateNew(Source, Dest: TExplorerImageList; ImageIndex: Integer);
begin
  inherited Create(nil);
  FImageLists[0] := Source; FreeNotification(Source);
  FImageLists[1] := Dest; FreeNotification(Dest);
  FSourceIndex := ImageIndex;
  ListAdd(ImageEntries, Self);
end;

destructor TImageEntry.Destroy;
begin
  ListRemove(ImageEntries, Self);
  inherited;
end;

procedure TImageEntry.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageLists[0])
    or (AComponent = FImageLists[1]) then Free;
end;

function FindImageIndex(Source, Dest: TExplorerImageList; Index: Integer): Integer;
var
  I: Integer;
  Entry: TImageEntry;
begin
  for I := 0 to ListCount(ImageEntries) - 1 do
  begin
    Entry := ImageEntries[I];
    if (Entry.FImageLists[0] = Source) and (Entry.FImageLists[1] = Dest) and
      (Entry.FSourceIndex = Index) then
    begin
      Result := Entry.FDestIndex;
      Exit;
    end;
  end;
  Result := -1;
end;

function RegisterImage(Source, Dest: TExplorerImageList; Index: Integer): Integer;
var
  Bitmap: TBitmap;
begin
  Result := FindImageIndex(Source, Dest, Index);
  if Result = -1 then
  begin
    with TImageEntry.CreateNew(Source, Dest, Index) do
    try
      Bitmap := TBitmap.Create;
      try
        if Index < Source.Count then
        begin
          Source.GetBitmap(Index, Bitmap);
          FDestIndex := Dest.Add(Bitmap, nil);
          Result := FDestIndex;
        end else
          Result := -1;
      finally
        Bitmap.Free;
      end;
    except
      Free;
      raise;
    end;
  end;
end;

procedure ClearImages(ImageList: TExplorerImageList);
var
  I: Integer;
  Entry: TImageEntry;
begin
  if Assigned(ImageList) then
  begin
    ImageList.Clear;
    for I := ListCount(ImageEntries) - 1 downto 0 do
    begin
      Entry := ImageEntries[I];
      if (Entry.FImageLists[1] = ImageList) then Entry.Free;
    end;
  end;
end;

constructor TExplorerNodes.Create(AOwner: TComponent);
begin
  inherited;
  FState := [nsEnabled, nsVisible];
  FImages[0] := -1; FImages[1] := -1;
  FImages[2] := -1; FImages[3] := -1;
end;

procedure TExplorerNodes.ClearReferences;
var
  P: PRefData;
begin
  while Assigned(FReferences) do
  begin
    P := FReferences[0];
    ListRemove(FReferences, P);
    P^.Data.Free;
    Dispose(P);
  end;
end;

destructor TExplorerNodes.Destroy;
begin
  DestroyingChildren;
{$IFDEF _D4_}
  FreeObject(FActionLink);
{$ENDIF}
  while ExplorerSourceCount > 0 do
    ExplorerSources[0].ExplorerRoot := nil;
  ClearReferences;
  FSubItems.Free;
  inherited;
end;

{$IFDEF _D4_}
procedure TExplorerNodes.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  BeginUpdate;
  try
    if Action is TCustomAction then
      with TCustomAction(Sender) do
      begin
        if not CheckDefaults or (Self.Text = '') then
          Self.Text := Caption;
        if not CheckDefaults or (Self.Checked = False) then
          Self.Checked := Checked;
        if (Self is TExplorerActionNode) and (not CheckDefaults or ((Self as TExplorerActionNode).Enabled = True)) then
          (Self as TExplorerActionNode).Enabled := Enabled;
        if not CheckDefaults or ((Self.ImageIndex = -1) and (Self.SelectedIndex = -1)) then
        begin
          Self.ImageIndex := ImageIndex;
          Self.SelectedIndex := ImageIndex;
        end;
        if not CheckDefaults or (Self.Visible = True) then
          Self.Visible := Visible;
        if (Self is TExplorerActionNode) and (not CheckDefaults or not Assigned((Self as TExplorerActionNode).OnClick)) then
          (Self as TExplorerActionNode).OnClick := OnExecute;
      end;
  finally
    EndUpdate;
  end;
end;
{$ENDIF}

function TExplorerNodes.AbsoluteIndex(ExplorerNodes: TExplorerNodes): Integer;
var
  Counter: Integer;
  function ForEachChildren(Root: TExplorerNodes): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Root.Count - 1 do
    begin
      Inc(Counter);
      Result := (Root[I] = ExplorerNodes) or ForEachChildren(Root[I]);
      if Result then Exit;
    end;
  end;
begin
  Counter := -1;
  if ForEachChildren(Self) then
    Result := Counter else
    Result := -1;
end;

function TExplorerNodes.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := False;
end;

function TExplorerNodes.AcceptsNodesList(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := (List.Count > 0);
  if Result then
    for I := 0 to List.Count - 1 do
    begin
      Result := AcceptsNodes(List[I]);
      if not Result then Exit;
    end;
end;

procedure TExplorerNodes.Assign(Source: TPersistent);
begin
  Self.BeginUpdate;
  try
    if (Source is TExplorerNodes) then
    with (Source as TExplorerNodes) do
    begin
      Self.Checked := Checked;
      Self.Enabled := Enabled;
      Self.ImageIndex := ImageIndex;
      Self.OverlayIndex := OverlayIndex;
      Self.SelectedIndex := SelectedIndex;
      Self.StateIndex := StateIndex;
      Self.Visible := Visible;
      if Assigned(FSubItems) then Self.SubItems := FSubItems;
      Self.Text := Text;
      Self.PopupMenu := PopupMenu;
{$IFDEF _D4_}
      Self.Action := Action;
{$ENDIF}
    end else
      inherited;
  finally
    Self.EndUpdate;
  end;
end;

function TExplorerNodes.ClipboardMode: TExplorerClipboardMode;
begin
  Result := cmNormal;
end;

function TExplorerNodes.CanCopy: Boolean;
begin
  Result := False;
end;

function TExplorerNodes.CanCopyList(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := List[0].CanCopy;
  if Result then
    for I := 1 to List.Count - 1 do
    begin
      Result := List[I].CanCopy;
      if not Result then Break;
    end;
end;

function TExplorerNodes.CanCut: Boolean;
begin
  Result := CanCopy and CanDelete;
end;

function TExplorerNodes.CanCutList(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := List[0].CanCut;
  if Result then
    for I := 1 to List.Count - 1 do
    begin
      Result := List[I].CanCut;
      if not Result then Break;
    end;
end;

function TExplorerNodes.CanDelete: Boolean;
begin
  Result := False;
end;

function TExplorerNodes.CanDeleteList(List: TExplorerNodesList): Boolean; 
var
  I: Integer;
begin
  Result := List[0].CanDelete;
  if Result then
    for I := 1 to List.Count - 1 do
    begin
      Result := List[I].CanDelete;
      if not Result then Break;
    end;
end;

function TExplorerNodes.CanDrag: Boolean;
begin
  Result := (csDesigning in ComponentState) and not IsRunTime;
end;

function TExplorerNodes.CanDrop(List: TExplorerNodesList): Boolean;
begin
  Result := AcceptsNodesList(List);
end;

function TExplorerNodes.CanEdit: Boolean;
begin
  Result := False;
end;

function TExplorerNodes.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := IsFolder;
end;

function TExplorerNodes.CanPaste(List: TExplorerNodesList): Boolean;
begin
  Result := IsFolder and AcceptsNodesList(List);
end;

procedure TExplorerNodes.Changed;
begin
  BeginUpdate;
  try
    Include(FState, nsChanged);
  finally
    EndUpdate;
  end;
end;

procedure TExplorerNodes.ChangedChildren(NodeType: TExplorerNodeType);
begin
  BeginExpand;
  try
    case NodeType of
      ntFolder:
        Include(FState, nsChangedFolders);
      ntNode:
        Include(FState, nsChangedNodes);
      ntSeparator:
        Include(FState, nsChangedSeparators);
    end;
  finally
    EndExpand;
  end;
end;

procedure TExplorerNodes.Clear;
var
  IsChanged: Boolean;
begin
  BeginExpand;
  try
    inherited Clear;
    IsChanged := nsExpanded in FState;
    Exclude(FState, nsExpanded);
    if IsChanged then Changed;
  finally
    EndExpand;
  end;
end;

procedure TExplorerNodes.ClipboardNotification(Operation: TOperation);
begin
end;

procedure TExplorerNodes.Collapse;
  function HasChildRefs(Nodes: TExplorerNodes): Boolean;
  var
    I: Integer;
    Node: TExplorerNodes;
  begin
    Result := False;
    for I := 0 to Nodes.Count - 1 do
    begin
      Node := Nodes[I];
      Result := not Node.IsRunTime or Node.HasRefs(True) or HasChildRefs(Node);
      if Result then Exit;
    end;
  end;
var
  Nodes: TExplorerNodes;
begin
  if HasChildren and not HasChildRefs(Self) then Clear;
  if not HasChildren then
  begin
    Nodes := Self.Parent;
    while Assigned(Nodes) do
    begin
      if Nodes.HasRefs(True) then
      begin
        Nodes.Collapse;
        Nodes := nil;
      end else
        Nodes := Nodes.Parent;
    end;
  end;
end;

procedure TExplorerNodes.Copy;
var
  List: TExplorerNodesList;
begin
  List := TExplorerNodesList.Create(False);
  try
    List.AddItem(Self);
    ExplorerClipboard.Copy(List);
  finally
    List.Free;
  end;
end;

procedure TExplorerNodes.Cut;
var
  List: TExplorerNodesList;
begin
  List := TExplorerNodesList.Create(False);
  try
    List.AddItem(Self);
    ExplorerClipboard.Cut(List);
  finally
    List.Free;
  end;
end;

procedure TExplorerNodes.DblClick;
begin
  DoDblClick;
end;

procedure TExplorerNodes.Delete;
begin
  DoDelete;
end;

procedure TExplorerNodes.DeleteList(List: TExplorerNodesList);
begin
  DoDeleteList(List);
end;

procedure TExplorerNodes.DefaultSort;
begin
  AnsiSort(False);
end;

procedure TExplorerNodes.Paste;
begin
  ExplorerClipboard.Paste(Self);
end;

procedure TExplorerNodes.DisableControls;
begin
  ForEachExplorerSource(Self, eeDisableControls, []);
end;

{$IFDEF _D4_}
procedure TExplorerNodes.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;
{$ENDIF}

procedure TExplorerNodes.DoCheck;
begin
end;

procedure TExplorerNodes.DoCopy;
begin
  ExplorerClipboard.Items.AddItem(Self);
end;

procedure TExplorerNodes.DoCopyList(List: TExplorerNodesList);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].DoCopy;
end;

procedure TExplorerNodes.DoCut;
begin
  ExplorerClipboard.Items.AddItem(Self);
end;

procedure TExplorerNodes.DoCutList(List: TExplorerNodesList);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].DoCut;
end;

procedure TExplorerNodes.DoDblClick;
begin
end;

procedure TExplorerNodes.DoDelete;
begin
  Free;
end;

procedure TExplorerNodes.DoDeleteList(List: TExplorerNodesList);
var
  I: Integer;
begin
  for I := List.Count - 1 downto 0 do
    List[I].DoDelete;
end;

procedure TExplorerNodes.DragDrop(List: TExplorerNodesList);
var
  I: Integer;
  SaveParent: TExplorerNodes;
begin
  BeginExpand;
  try
    if List.Count > 0 then
      SaveParent := List[0].Parent else
      SaveParent := nil;
    if Assigned(SaveParent) then SaveParent.BeginExpand;
    try
      for I := 0 to List.Count - 1 do
        List[I].Parent := Self;
    finally
      if Assigned(SaveParent) then SaveParent.EndExpand;
    end;
  finally
    EndExpand;
  end;
end;

procedure TExplorerNodes.DoEdit(var Text: string);
begin
  Self.Text := Text;
end;

procedure TExplorerNodes.DoPaste(List: TExplorerNodesList);
begin
end;

procedure TExplorerNodes.EnableControls;
begin
  ForEachExplorerSource(Self, eeEnableControls, []);
end;

function TExplorerNodes.HasChildrenOfType(NodeType: TExplorerNodeType): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if NodeType = Items[I].NodeType then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TExplorerNodes.HasRefs(Ignore: Boolean): Boolean;
var
  I: Integer;
  P: PRefData;
begin
  if Ignore then
  begin
    Result := False;
    for I := 0 to ListCount(FReferences) - 1 do
    begin
      P := FReferences[I];
      Result := not P^.Ignore;
      if Result then Exit;
    end;
  end else
    Result := Assigned(FReferences);
end;

procedure TExplorerNodes.InternalChanged;
begin
end;

procedure TExplorerNodes.InternalChangedChildren;
begin
  if GetSorted then DefaultSort;
end;

procedure TExplorerNodes.InternalExpand;
begin
end;

procedure TExplorerNodes.InternalSelect;
begin
end;

function TExplorerNodes.IsFolder: Boolean;
begin
  Result := NodeType = ntFolder;
end;

function TExplorerNodes.IsNode: Boolean;
begin
  Result := NodeType = ntNode;
end;

function TExplorerNodes.IsRunTime: Boolean;
begin
  Result := False;
end;

function TExplorerNodes.IsSeparator: Boolean;
begin
  Result := NodeType = ntSeparator;
end;

function TExplorerNodes.IsStored: Boolean;
begin
  Result := not IsRunTime;
end;

procedure TExplorerNodes.ItemListEvent(Item: TItem; Event: Integer);
begin
  inherited;
  case Event of
    ieItemListChanged: ChangedChildren((Item as TExplorerNodes).NodeType);
  end;
end;

function TExplorerNodes.IsParentOf(Item: TExplorerNodes): Boolean;
begin
  Item := Item.Parent;
  Result := False;
  while Assigned(Item) do
  begin
    Result := Self = Item;
    if Result then Break;
    Item := Item.Parent;
  end;
end;

procedure TExplorerNodes.Loaded;
begin
  inherited;
  InternalChangedChildren;
{$IFDEF _D4_}
  if Action <> nil then ActionChange(Action, True);
{$ENDIF}
end;

procedure TExplorerNodes.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent is TExplorerSource) then
      RemoveExplorerSource(AComponent as TExplorerSource)
    else if (AComponent = FPopupMenu) then
      SetPopupMenu(nil);
{$IFDEF _D4_}
    if (AComponent = Action) then
      SetAction(nil);
{$ENDIF}
  end;
end;

procedure TExplorerNodes.NotifyChange;
begin
  ForEachExplorerSource(Self, eeNodesChange, [NodeType]);
end;

procedure TExplorerNodes.NotifyChangeChildren(NodeTypes: TExplorerNodeTypes);
begin
  ForEachExplorerSource(Self, eeNodesChangeChildren, NodeTypes);
end;

procedure TExplorerNodes.BeginExpand;
begin
  Inc(FExpandCount);
end;

procedure TExplorerNodes.EndExpand;
var
  Changed: TExplorerNodeTypes;
begin
  Dec(FExpandCount);
  if (FExpandCount = 0) and not (csDestroying in ComponentState) and
    ([nsChangedFolders, nsChangedNodes, nsChangedSeparators] * FState <> []) then
  begin
    Include(FState, nsExpandLock);
    try
      InternalChangedChildren;
      Changed := [];
      if (nsChangedFolders in FState) then Include(Changed, ntFolder);
      if nsChangedNodes in FState then Include(Changed, ntNode);
      if nsChangedSeparators in FState then Include(Changed, ntSeparator);
    finally
      FState := FState - [nsChangedNodes, nsChangedFolders, nsChangedSeparators, nsExpandLock];
    end;
    NotifyChangeChildren(Changed);
  end;
end;

procedure TExplorerNodes.EndUpdate;
begin
  inherited;
  if (UpdateCount = 0) and (nsChanged in FState) and not (csDestroying in ComponentState) then
  begin
    Exclude(FState, nsChanged);
    InternalChanged;
    NotifyChange;
  end;
end;

procedure TExplorerNodes.Expand;
begin
  if Expanded then Exit;
  BeginExpand;
  try
    InternalExpand;
    Include(FState, nsExpanded)
  finally
    EndExpand;
  end;
end;

procedure TExplorerNodes.ForEachExplorerSource(Sender: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
var
  I: Integer;
begin
  for I := 0 to ExplorerSourceCount - 1 do ExplorerSources[I].ExplorerEvent(Sender, Event, NodeTypes);
  if Assigned(Parent) then
    Parent.ForEachExplorerSource(Sender, Event, NodeTypes);
end;

{$IFDEF _D4_}
function TExplorerNodes.GetAction: TBasicAction;
begin
  if Assigned(FActionLink) then Result := FActionLink.Action else Result := nil;
end;

function TExplorerNodes.GetActionLinkClass: TExplorerNodesActionLinkClass;
begin
  Result := TExplorerNodesActionLink;
end;
{$ENDIF}

function TExplorerNodes.GetChecked: Boolean;
begin
  Result := nsChecked in FState;
end;

procedure TExplorerNodes.GetChildren(Proc: TGetChildProc{$IFDEF _D3_}; Root: TComponent{$ENDIF});
var
  I: Integer;
  Item: TExplorerNodes;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.IsStored then Proc(Item);
  end;
end;

function TExplorerNodes.GetEnabled: Boolean;
begin
  Result := nsEnabled in FState;
end;

function TExplorerNodes.GetExpanded: Boolean;
begin
  Result := nsExpanded in FState;
end;

function TExplorerNodes.GetExplorerSource(Index: Integer): TExplorerSource;
begin
  Result := ListItem(FExplorerSources, Index);
end;

function TExplorerNodes.GetExplorerSourceCount: Integer;
begin
  Result := ListCount(FExplorerSources);
end;

function TExplorerNodes.GetLargeImages: TExplorerImageList;
begin
  Result := nil;
end;

function TExplorerNodes.GetNodeType: TExplorerNodeType;
begin
  Result := ntFolder;
end;

function TExplorerNodes.GetItemName: string;
begin
  Result := Text;
end;

function TExplorerNodes.GetSmallImages: TExplorerImageList;
begin
  Result := nil;
end;

function TExplorerNodes.GetStateImages: TExplorerImageList;
begin
  Result := nil;
end;

function TExplorerNodes.GetItem(Index: Integer): TExplorerNodes;
begin
  Result := inherited Items[Index];
end;

function TExplorerNodes.GetParent: TExplorerNodes;
begin
  Result := TExplorerNodes(inherited ItemList);
end;

function TExplorerNodes.GetRadioItem: Boolean;
begin
  Result := nsRadioItem in FState;
end;


function TExplorerNodes.GetSorted: Boolean;
begin
  Result := nsSorted in FState;
end;

function TExplorerNodes.GetSubItems: TStrings;
begin
  if not Assigned(FSubItems) then
  begin
    FSubItems := TStringList.Create;
    TStringList(FSubItems).OnChange := SubItemsChanged;
  end;
  Result := FSubItems;
end;

function TExplorerNodes.GetVisible: Boolean;
begin
  Result := nsVisible in FState;
end;

procedure TExplorerNodes.InsertExplorerSource(ExplorerSource: TExplorerSource);
begin
  if not Assigned(FExplorerSources) or (FExplorerSources.IndexOf(ExplorerSource) < 0) then
  begin
    ListAdd(FExplorerSources, ExplorerSource);
    ExplorerSource.FExplorerRoot := Self;
    FreeNotification(ExplorerSource);
    ExplorerSource.ExplorerEvent(Self, eeActiveChange, ntNodeTypesAll);
  end;
end;

procedure TExplorerNodes.RemoveExplorerSource(ExplorerSource: TExplorerSource);
begin
  if Assigned(FExplorerSources) and (FExplorerSources.IndexOf(ExplorerSource) >= 0) then
  begin
    ListRemove(FExplorerSources, ExplorerSource);
    ExplorerSource.FExplorerRoot := nil;
    ExplorerSource.ExplorerEvent(nil, eeActiveChange, ntNodeTypesAll);
  end;
end;

procedure TExplorerNodes.Refresh;
var
  I: Integer;
  Nodes: TExplorerNodes;
begin
  BeginExpand;
  try
    for I := Count - 1 downto 0 do
    begin
      Nodes := Items[I];
      if Nodes.IsRunTime then Nodes.Free;
    end;
    InternalExpand;
  finally
    EndExpand;
  end;
end;

{$IFDEF _D4_}
procedure TExplorerNodes.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end else begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;
{$ENDIF}

procedure TExplorerNodes.SetChecked(Value: Boolean);
begin
  if (GetChecked <> Value) then
  begin
    BeginUpdate;
    try
      if Value then
        Include(FState, nsChecked) else
        Exclude(FState, nsChecked);
      if Checked or not RadioItem then DoCheck;
      Changed;
    finally
      EndUpdate;
    end;
    UpdateSiblings;
  end;
end;

procedure TExplorerNodes.SetEnabled(Value: Boolean);
begin
  if (GetEnabled <> Value) then
  begin
    if Value then
      Include(FState, nsEnabled) else
      Exclude(FState, nsEnabled);
    InternalChanged;
  end;
end;

procedure TExplorerNodes.SetExpanded(Value: Boolean);
begin
  if (GetExpanded <> Value) then
  begin
    if Value then Expand else Clear;
  end;
end;

procedure TExplorerNodes.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (Name = Text) and
    ((Parent = nil) or not (Parent is TExplorerNodes) or
    not (csLoading in Parent.ComponentState));
  inherited SetName(Value);
  if ChangeText then Text := Value;
end;

procedure TExplorerNodes.SetText(Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TExplorerNodes.SetParent(Value: TExplorerNodes);
begin
  inherited ItemList := Value;
end;

procedure TExplorerNodes.SetPopupMenu(Value: TPopupMenu);
begin
  if (FPopupMenu <> Value) then
  begin
    FPopupMenu := Value;
    if Assigned(FPopupMenu) then FreeNotification(FPopupMenu);
  end;
end;

procedure TExplorerNodes.SetRadioItem(Value: Boolean);
begin
  if (GetRadioItem <> Value) then
  begin
    if Value then
      Include(FState, nsRadioItem) else
      Exclude(FState, nsRadioItem);
    Changed;
    UpdateSiblings;
  end;
end;

procedure TExplorerNodes.SetSorted(Value: Boolean);
begin
  if GetSorted <> Value then
  begin
    if Value then
      Include(FState, nsSorted) else
      Exclude(FState, nsSorted);
    SortedChanged;
  end;
end;

procedure TExplorerNodes.SetSubItems(Value: TStrings);
begin
  GetSubItems.Assign(Value);
end;

procedure TExplorerNodes.SetImage(Index: Integer; Value: Integer);
begin
  if (FImages[Index] <> Value) then
  begin
    FImages[Index] := Value;
    Changed;
  end;
end;

procedure TExplorerNodes.SetVisible(Value: Boolean);
begin
  if (GetVisible <> Value) then
  begin
    if Value then
      Include(FState, nsVisible) else
      Exclude(FState, nsVisible);
    if Assigned(Parent) then Parent.ChangedChildren(NodeType) else InternalChanged;
  end;
end;

procedure TExplorerNodes.SortedChanged;
begin
  if Count > 0 then
  begin
    BeginExpand;
    try
      ChangedChildren(ntFolder);
      ChangedChildren(ntNode);
      ChangedChildren(ntSeparator);
    finally
      EndExpand;
    end;
  end;
end;

function TExplorerNodes.StoreChecked: Boolean;
begin
{$IFDEF _D4_}
  Result := (ActionLink = nil) or not FActionLink.IsCheckedLinked;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TExplorerNodes.StoreEnabled: Boolean;
begin
{$IFDEF _D4_}
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TExplorerNodes.StoreImage(Index: Integer): Boolean;
begin
{$IFDEF _D4_}
  Result := not (Index in [0, 2]) or ((ActionLink = nil) or not FActionLink.IsImageIndexLinked);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TExplorerNodes.StoreSubItems: Boolean;
begin
  Result := Assigned(FSubItems);
end;

function TExplorerNodes.StoreText: Boolean;
begin
{$IFDEF _D4_}
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TExplorerNodes.StoreVisible: Boolean;
begin
{$IFDEF _D4_}
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
{$ELSE}
  Result := True;
{$ENDIF}
end;

procedure TExplorerNodes.Select;
begin
  InternalSelect;
  ForEachExplorerSource(Self, eeSetSelected, []);
end;

procedure TExplorerNodes.Sort(Compare: TListSortCompare);
var
  I: Integer;
  Item: TExplorerNodes;
  OldOrder: TList;
begin
  if not (nsExpandLock in FState) then
  begin
    BeginExpand;
    try
      OldOrder := nil;
      try
        for I := 0 to Count - 1 do
          ListAdd(OldOrder, Items[I]);
        inherited Sort(Compare);
        for I := 0 to Count - 1 do
        begin
          Item := Items[I];
          if OldOrder[I] <> Item then
            ChangedChildren(Item.NodeType);
        end;
      finally
        FreeObject(OldOrder);
      end;
    finally
      EndExpand;
    end;
  end else
    inherited Sort(Compare);
end;

procedure TExplorerNodes.AnsiSort(Descending: Boolean);

  function CompareAsc(Item1, Item2: Pointer): Integer;
  begin
    if Item1 = Item2 then
      Result := 0
    else
      Result := AnsiCompareText(TExplorerNodes(Item1).Text, TExplorerNodes(Item2).Text);
  end;

  function CompareDesc(Item1, Item2: Pointer): Integer;
  begin
    if Item1 = Item2 then
      Result := 0
    else
      Result := - AnsiCompareText(TExplorerNodes(Item1).Text, TExplorerNodes(Item2).Text);
  end;

begin
  if Descending then Sort(@CompareDesc) else Sort(@CompareAsc);
end;

procedure TExplorerNodes.SubItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TExplorerNodes.UpdateSiblings;
var
  I: Integer;
  Nodes: TExplorerNodes;
begin
  if Checked and RadioItem and Assigned(Parent) then
  begin
    for I := 0 to Parent.Count - 1 do
    begin
      Nodes := Parent[I];
      if (Nodes <> Self) and (Nodes.RadioItem) then
        Nodes.Checked := False;
    end;
  end;
end;

procedure TExplorerNodes.InternalGetDrawingParams(ExplorerNodes: TExplorerNodes; AFont: TFont; var ABkgnd: TColor);
begin
end;

procedure TExplorerNodes.GetDrawingParams(ExplorerNodes: TExplorerNodes; AFont: TFont; var ABkgnd: TColor);
begin
  if ParentDrawing and Assigned(Parent) then
    Parent.GetDrawingParams(ExplorerNodes, AFont, ABkgnd)
  else
    InternalGetDrawingParams(ExplorerNodes, AFont, ABkgnd);
end;

function TExplorerNodes.GetParentDrawing: Boolean;
begin
  Result := nsParentDrawing in FState;
end;

procedure TExplorerNodes.SetParentDrawing(Value: Boolean);
begin
  if GetParentDrawing <> Value then
  begin
    if Value then
      Include(FState, nsParentDrawing) else
      Exclude(FState, nsParentDrawing);
    InvalidateControls(False);
  end;
end;

procedure TExplorerNodes.InvalidateControls(Children: Boolean);
var
  I: Integer;
begin
  if Children then
    for I := 0 to Count - 1 do
      Items[I].InvalidateControls(True);
  ForEachExplorerSource(Self, eeInvalidate, []);
end;

{$IFDEF _D4_}
{ TExplorerNodesActionLink }
procedure TExplorerNodesActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TExplorerNodes;
end;

function TExplorerNodesActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Text = (Action as TCustomAction).Caption);
end;

function TExplorerNodesActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TExplorerNodesActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TExplorerNodesActionLink.IsHelpContextLinked: Boolean;
begin
  Result := False;
end;

function TExplorerNodesActionLink.IsHintLinked: Boolean;
begin
  Result := False;
end;

function TExplorerNodesActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex) and
    (FClient.SelectedIndex = (Action as TCustomAction).ImageIndex);
end;

function TExplorerNodesActionLink.IsShortCutLinked: Boolean;
begin
  Result := False;
end;

function TExplorerNodesActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TExplorerNodesActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and (FClient is TExplorerActionNode) and
    (@(FClient as TExplorerActionNode).OnClick = @Action.OnExecute);
end;

procedure TExplorerNodesActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Text := Value;
end;

procedure TExplorerNodesActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Checked := Value;
end;

procedure TExplorerNodesActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then (FClient as TExplorerActionNode).Enabled := Value;
end;

procedure TExplorerNodesActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    FClient.ImageIndex := Value;
    FClient.SelectedIndex := Value;
  end;
end;

procedure TExplorerNodesActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TExplorerNodesActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then (FClient as TExplorerActionNode).OnClick := Value;
end;
{$ENDIF}

{ TExplorerRootNode }
function TExplorerRootNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := (csDesigning in ComponentState) and (ExplorerNodes <> Self) and (ExplorerNodes.Parent <> Self) and
    not ExplorerNodes.IsRunTime and not ExplorerNodes.IsParentOf(Self);
end;

function TExplorerRootNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].NodeType in NodeTypes then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

{ TExplorerInterfaceNode }
function TExplorerInterfaceNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := ((ExplorerNodes is TExplorerSeparatorNode) and (csDesigning in ComponentState)) or
    ((ExplorerNodes is TExplorerInterfaceNode) and (ExplorerNodes <> Self) and
      not ExplorerNodes.IsParentOf(Self) and (IsFolder and (ExplorerNodes.Parent <> Self) or
        not IsFolder and (ExplorerNodes.Parent = Self.Parent)));
end;

procedure TExplorerInterfaceNode.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if (Source is TExplorerInterfaceNode) then
    with Source as TExplorerInterfaceNode do
    begin
      Self.EnableCopy := FEnableCopy;
      Self.EnableDelete := FEnableDelete;
      Self.EnableDrag := FEnableDrag;
      Self.FEnableEdit := FEnableEdit;
      Self.FEnablePaste := FEnablePaste;
      Self.LargeImages := LargeImages;
      Self.SmallImages := SmallImages;
      Self.StateImages := StateImages;
      Self.FOnCanCopy := FOnCanCopy;
      Self.FOnCanCut := FOnCanCut;
      Self.FOnCanDelete := FOnCanDelete;
      Self.FOnCanDrag := FOnCanDrag;
      Self.FOnEditing := FOnEditing;
      Self.FOnCanPaste := FOnCanPaste;
      Self.FOnCanDrop := FOnCanDrop;
      Self.FOnCheck := FOnCheck;
      Self.FOnCopy := FOnCopy;
      Self.FOnDelete := FOnDelete;
      Self.FOnPaste := FOnPaste;
      Self.FOnDragDrop := FOnDragDrop;
      Self.FOnEdit := FOnEdit;
    end;
    inherited;
  finally
    EndUpdate;
  end
end;

function TExplorerInterfaceNode.CanCopy: Boolean;
begin
  Result := FEnableCopy;
  if Assigned(FOnCanCopy) then FOnCanCopy(Self, Result);
end;

function TExplorerInterfaceNode.CanCut: Boolean;
begin
  Result := FEnableDelete;
  if Assigned(FOnCanCut) then FOnCanCut(Self, Result);
end;

function TExplorerInterfaceNode.CanDelete: Boolean;
begin
  Result := FEnableDelete;
  if Assigned(FOnCanDelete) then FOnCanDelete(Self, Result);
end;

function TExplorerInterfaceNode.CanDrag: Boolean;
begin
  Result := inherited CanDrag or FEnableDrag;
  if Assigned(FOnCanDrag) then FOnCanDrag(Self, Result);
end;

function TExplorerInterfaceNode.CanDrop(List: TExplorerNodesList): Boolean;
begin
  Result := (FEnableDrop or (csDesigning in ComponentState)) and inherited CanDrop(List);
  if Assigned(FOnCanDrop) then FOnCanDrop(Self, Self, List, Result);
end;

function TExplorerInterfaceNode.CanEdit: Boolean;
begin
  Result := FEnableEdit;
  if Assigned(FOnEditing) then FOnEditing(Self, Result);
end;

function TExplorerInterfaceNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].NodeType in NodeTypes then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TExplorerInterfaceNode.CanPaste(List: TExplorerNodesList): Boolean;
begin
  Result := FEnablePaste and inherited CanPaste(List);
  if Assigned(FOnCanPaste) then FOnCanPaste(Self, List, Result);
end;

procedure TExplorerInterfaceNode.DoCheck;
begin
  if Assigned(FOnCheck) then FOnCheck(Self);
end;

procedure TExplorerInterfaceNode.DoCopy;
begin
  if Assigned(FOnCopy) then FOnCopy(Self) else inherited;
end;

procedure TExplorerInterfaceNode.DoDelete;
begin
  if Assigned(FOnDelete) then FOnDelete(Self) else inherited;
end;

procedure TExplorerInterfaceNode.DragDrop(List: TExplorerNodesList);
var
  I: Integer;
  Nodes: TExplorerNodes;
begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Self, List) else
  begin
    BeginExpand;
    try
      for I := 0 to List.Count - 1 do
      begin
        Nodes := List[I];
        if IsFolder then
          Nodes.Parent := Self
        else if Nodes.Parent = Self.Parent then
          Nodes.Index := Self.Index
        else
      end;
    finally
      EndExpand;
    end;
  end;
end;

procedure TExplorerInterfaceNode.DoEdit(var Text: string);
begin
  if Assigned(FOnEdit) then FOnEdit(Self, Text);
  inherited;
end;

procedure TExplorerInterfaceNode.DoPaste;
begin
  if Assigned(FOnPaste) then FOnPaste(Self);
end;

function TExplorerInterfaceNode.GetLargeImages: TExplorerImageList;
begin
  Result := FImageLists[0];
end;

function TExplorerInterfaceNode.GetSmallImages: TExplorerImageList;
begin
  Result := FImageLists[1];
end;

function TExplorerInterfaceNode.GetStateImages: TExplorerImageList;
begin
  Result := FImageLists[2];
end;

procedure TExplorerInterfaceNode.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    for I := 0 to 2 do if (FImageLists[I] = AComponent) then
      SetImageList(I, nil);
  end;
end;

procedure TExplorerInterfaceNode.SetImageList(Index: Integer; Value: TExplorerImageList);
begin
  if (FImageLists[Index] <> Value) then
  begin
    FImageLists[Index] := Value;
    if Assigned(Value) then FreeNotification(Value);
    Changed;
  end;
end;

{ TExplorerSeparatorNode }
function TExplorerSeparatorNode.GetNodeType: TExplorerNodeType;
begin
  Result := ntSeparator;
end;

{ TExplorerFolderNode }
procedure TExplorerFolderNode.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if (Source is TExplorerFolderNode) then
      Self.FOnGetChildrenParams := FOnGetChildrenParams;
    inherited;
  finally
    EndUpdate;
  end;
end;

procedure TExplorerFolderNode.DoGetChildrenParams(ExplorerNodes: TExplorerNodes);
begin
  if Assigned(FOnGetChildrenParams) then FOnGetChildrenParams(Self, ExplorerNodes);
end;

procedure TExplorerFolderNode.InsertItem(Item: TItem);
begin
  inherited;
  with Item as TExplorerNodes do
  begin
    BeginUpdate;
    try
      DoGetChildrenParams(Item as TExplorerNodes);
    finally
      EndUpdate;
    end;
  end;
end;

{ TExplorerActionNode }
procedure TExplorerActionNode.Assign(Source: TPersistent);
begin
  Self.BeginUpdate;
  try
    if (Source is TExplorerActionNode) then
    with (Source as TExplorerActionNode) do
    begin
      Self.FOnClick := FOnClick;
    end;
    inherited Assign(Source);
  finally
    Self.EndUpdate;
  end;
end;

procedure TExplorerActionNode.Click;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TExplorerActionNode.DoDblClick;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

function TExplorerActionNode.GetNodeType: TExplorerNodeType;
begin
  Result := ntNode;
end;

function TExplorerActionNode.StoreOnClick: Boolean;
begin
{$IFDEF _D4_}
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
{$ELSE}
  Result := True;
{$ENDIF}
end;

{ TExplorerStringNode }
type
  TExplorerStringNode = class(TExplorerNode)
  protected
    function GetNodeType: TExplorerNodeType; override;
  public
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    function CanDrag: Boolean; override;
    procedure DragDrop(List: TExplorerNodesList); override;
    function IsStored: Boolean; override;
  end;

function TExplorerStringNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := (ExplorerNodes <> Self) and Parent.AcceptsNodes(ExplorerNodes);
end;

function TExplorerStringNode.CanDrag: Boolean;
begin
  Result := (Parent as TExplorerStringsNode).FEnableDragChildren;
end;

procedure TExplorerStringNode.DragDrop(List: TExplorerNodesList);
var
  I: Integer;
  SaveParent: TExplorerNodes;
begin
  Parent.BeginExpand;
  try
    if List.Count > 0 then
      SaveParent := List[0].Parent else
      SaveParent := nil;
    if Assigned(SaveParent) then SaveParent.BeginExpand;
    try
      for I := 0 to List.Count - 1 do
      begin
        List[I].Parent := Self.Parent;
        if (Parent as TExplorerStringsNode).FEnableOrderChildren then
          List[I].Index := Self.Index;
      end;
    finally
      if Assigned(SaveParent) then SaveParent.EndExpand;
    end;
  finally
    Parent.EndExpand;
  end;
end;

function TExplorerStringNode.GetNodeType: TExplorerNodeType;
begin
  Result := ntNode;
end;

function TExplorerStringNode.IsStored: Boolean;
begin
  Result := False;
end;

{ TExplorerStrings }
type
  TExplorerStrings = class(TStrings)
  private
    FExplorerNodes: TExplorerStringsNode;
  protected
    procedure Put(Index: Integer; const S: string); override;
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

procedure TExplorerStrings.Clear;
begin
  FExplorerNodes.Clear;
end;

procedure TExplorerStrings.Delete(Index: Integer);
begin
  FExplorerNodes[Index].Free;
end;

function TExplorerStrings.GetCount: Integer;
begin
  Result := FExplorerNodes.Count;
end;

function TExplorerStrings.Get(Index: Integer): string;
begin
  Result := FExplorerNodes[Index].Text;
end;

function TExplorerStrings.GetObject(Index: Integer): TObject;
begin
  Result := FExplorerNodes[Index].Data;
end;

procedure TExplorerStrings.Put(Index: Integer; const S: string);
begin
  FExplorerNodes[Index].Text := S;
end;

procedure TExplorerStrings.PutObject(Index: Integer; AObject: TObject);
begin
  FExplorerNodes[Index].Data := AObject;
end;

procedure TExplorerStrings.Insert(Index: Integer; const S: string);
var
  Nodes: TExplorerStringNode;
begin
  Nodes := TExplorerStringNode.Create(nil);
  try
    Nodes.Text := S;
    BeginUpdate;
    try
      Nodes.Parent := FExplorerNodes;
      Nodes.Index := Index;
    finally
      EndUpdate;
    end;
  except
    Nodes.Free;
    raise;
  end;
end;

procedure TExplorerStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    FExplorerNodes.BeginExpand else
    FExplorerNodes.EndExpand;
end;

{ TExplorerStringsNode }
constructor TExplorerStringsNode.Create(AOwner: TComponent);
begin
  inherited;
  FEnableDragChildren := True;
  FEnableDropChildren := True;
  FLines := TExplorerStrings.Create;
  TExplorerStrings(FLines).FExplorerNodes := Self;
end;

destructor TExplorerStringsNode.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TExplorerStringsNode.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if (Source is TExplorerStringsNode) then
       Self.Lines := Lines;
    inherited;
  finally
    EndUpdate;
  end;
end;

function TExplorerStringsNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := ((csDesigning in ComponentState) or FEnableDropChildren) and
    (FEnableOrderChildren or (ExplorerNodes.Parent <> Self)) and
    (ExplorerNodes is TExplorerStringNode)
end;

procedure TExplorerStringsNode.SetEnableDragChildren(Value: Boolean);
begin
  if FEnableDragChildren <> Value then
  begin
    FEnableDragChildren := Value;
    if not FEnableDragChildren then EnableOrderChildren := False;
  end;
end;

procedure TExplorerStringsNode.SetEnableOrderChildren(Value: Boolean);
begin
  if (FEnableOrderChildren <> Value) then
  begin
    FEnableOrderChildren := Value;
    if FEnableOrderChildren then Sorted := False;
  end;
end;

procedure TExplorerStringsNode.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TExplorerStringsNode.SortedChanged;
begin
  if Sorted then
    FEnableOrderChildren := False;
  inherited;
end;

{ TExplorerFormNode }
procedure TExplorerFormNode.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TExplorerFormNode.CreateForm(AOwner: TComponent; Assign: Boolean): TCustomForm;
var
  FormClass: TFormClass;
begin
  FormClass := TFormClass(ExplorerFormClassList.ClassItemByName(FFormClassName).GetClassType);
  Result := FormClass.Create(AOwner);
  if Assign then SetForm(Result);
end;

function TExplorerFormNode.FormNeeded(AOwner: TComponent): TCustomForm;
begin
  if not Assigned(FForm) then CreateForm(AOwner, True);
  Result := FForm;
end;

procedure TExplorerFormNode.SetForm(Value: TCustomForm);
begin
  if (FForm <> Value) then
  begin
    FForm := Value;
    if Assigned(FForm) then FreeNotification(FForm);
    DoChange;
  end;
end;

procedure TExplorerFormNode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FForm) then SetForm(nil);
end;

{ TExplorerListReference }
type
  TExplorerListReference = class
  private
    FList: TExplorerNodesList;
    FExplorerNodes: TExplorerNodes;
  public
    constructor Create(AList: TExplorerNodesList; AExplorerNodes: TExplorerNodes);
    destructor Destroy; override;
    property ExplorerNodes: TExplorerNodes read FExplorerNodes;
  end;

constructor TExplorerListReference.Create(AList: TExplorerNodesList; AExplorerNodes: TExplorerNodes);
begin
  FList := AList;
  FExplorerNodes := AExplorerNodes;
  FList.FExplorerLink.InsertReference(ExplorerNodes, Self);
  FList.Notification(ExplorerNodes, opInsert);
  ListAdd(FList.FItems, Self);
end;

destructor TExplorerListReference.Destroy;
begin
  ListRemove(FList.FItems, Self);
  FList.Notification(ExplorerNodes, opRemove);
  FList.FExplorerLink.RemoveReference(FExplorerNodes);
  inherited;
end;

{ TExplorerNodesList }
constructor TExplorerNodesList.Create(IgnoreRefs: Boolean);
begin
  FExplorerLink := TExplorerLink.Create(IgnoreRefs);
end;

destructor TExplorerNodesList.Destroy;
begin
  Clear;
  FExplorerLink.Free;
  inherited;
end;

procedure TExplorerNodesList.AddItem(ExplorerNodes: TExplorerNodes);
begin
  TExplorerListReference.Create(Self, ExplorerNodes)
end;

procedure TExplorerNodesList.Clear;
begin
  ListDestroyAll(FItems);
end;

function TExplorerNodesList.GetCount: Integer;
begin
  Result := ListCount(FItems);
end;

function TExplorerNodesList.GetEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TExplorerNodesList.GetItem(Index: Integer): TExplorerNodes;
begin
  Result := TExplorerListReference(ListItem(FItems, Index)).ExplorerNodes;
end;

procedure TExplorerNodesList.Notification(ExplorerNodes: TExplorerNodes; Operation: TOperation);
begin
end;

type
{ TExplorerClipboardNodesList }
  TExplorerClipboardNodesList = class(TExplorerNodesList)
  protected
    procedure Notification(ExplorerNodes: TExplorerNodes; Operation: TOperation); override;
  end;

procedure TExplorerClipboardNodesList.Notification(ExplorerNodes: TExplorerNodes; Operation: TOperation);
begin
  ExplorerNodes.ClipboardNotification(Operation);
end;

{ TExplorerClipboard }
constructor TExplorerClipboard.Create;
begin
  FItems := TExplorerClipboardNodesList.Create(True);
end;

procedure TExplorerClipboard.Clear;
begin
  Open;
  try
    FItems.Clear;
    Clipboard.Clear;
  finally
    Close;
  end;
end;

destructor TExplorerClipboard.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TExplorerClipboard.Close;
begin
  Clipboard.Close;
end;

procedure TExplorerClipboard.Copy(List: TExplorerNodesList);
var
  I: Integer;
begin
  Open;
  try
    Clear;
    FState := csCopy;
    for I := 0 to List.Count - 1 do
      with List[I] do
        if (ClipboardMode = cmParent) and Assigned(Parent) then
        begin
          Parent.DoCopyList(List);
          Exit;
        end;
    for I := 0 to List.Count - 1 do
      List[I].DoCopy;
  finally
    Close;
  end;
end;

procedure TExplorerClipboard.Cut(List: TExplorerNodesList);
var
  I: Integer;
begin
  Open;
  try
    Clear;
    FState := csCut;
    for I := 0 to List.Count - 1 do
      with List[I] do
        if (ClipboardMode = cmParent) and Assigned(Parent) then
        begin
          Parent.DoCutList(List);
          Exit;
        end;
    for I := List.Count - 1 downto 0 do
      List[I].DoCut;
  finally
    Close;
  end;
end;

procedure TExplorerClipboard.GetItems(List: TExplorerNodesList);
var
  I: Integer;
begin
  Open;
  try
    for I := 0 to FItems.Count - 1 do List.AddItem(FItems[I]);
  finally
    Close;
  end;
end;

procedure TExplorerClipboard.Open;
begin
  Clipboard.Open;
end;

procedure TExplorerClipboard.Paste(ExplorerNodes: TExplorerNodes);
var
  List: TExplorerNodesList;
begin
  List := TExplorerNodesList.Create(False);
  try
    GetItems(List);
    if ExplorerNodes.CanPaste(List) then ExplorerNodes.DoPaste(List);
  finally
    List.Free;
  end;
end;

{ TExplorerSource }
destructor TExplorerSource.Destroy;
var
  I: Integer;
begin
  SetExplorerRoot(nil);
  for I := 0 to 2 do SetImageList(I, nil);
  while (ListCount(FLinks) > 0) do RemoveExplorerLink(FLinks[0]);
  SetImages([]);
  inherited;
end;

procedure TExplorerSource.DisableControls;
begin
  Inc(FDisableCount);
  if (FDisableCount = 1) then ExplorerEvent(nil, eeDisableControls, []);
end;

procedure TExplorerSource.DoActiveChanged;
begin
  if Assigned(FOnActiveChange) then FOnActiveChange(Self);
end;

function TExplorerSource.CanCopy(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := List.Count > 0;
  for I := 0 to List.Count - 1 do
  begin
    with List[I] do
      if (ClipboardMode = cmParent) and Assigned(Parent) then
      begin
        Result := Parent.CanCopyList(List);
        Break;
      end else
        Result := List[I].CanCopy;
    if not Result then Break;
  end;
  if Assigned(FOnCanCopy) then FOnCanCopy(Self, List, Result);
end;

function TExplorerSource.CanCut(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := List.Count > 0;
  for I := 0 to List.Count - 1 do
  begin
    with List[I] do
      if (ClipboardMode = cmParent) and Assigned(Parent) then
      begin
        Result := Parent.CanCutList(List);
        Break;
      end else
        Result := List[I].CanCut;
    if not Result then Break;
  end;
  if Assigned(FOnCanCut) then FOnCanCut(Self, List, Result);
end;

function TExplorerSource.CanDelete(List: TExplorerNodesList): Boolean;
var
  I: Integer;
begin
  Result := List.Count > 0;
  for I := 0 to List.Count - 1 do
  begin
    with List[I] do
      if (ClipboardMode = cmParent) and Assigned(Parent) then
      begin
        Result := Parent.CanDeleteList(List);
        Break;
      end else
        Result := List[I].CanDelete;
    if not Result then Break;
  end;
  if Assigned(FOnCanDelete) then FOnCanDelete(Self, List, Result);
end;

function TExplorerSource.CanDrop(Dest: TExplorerNodes; List: TExplorerNodesList): Boolean;
begin
  Result := Dest.CanDrop(List);
  if Assigned(FOnCanDrop) then FOnCanDrop(Self, Dest, List, Result);
end;

function TExplorerSource.CanPaste(ExplorerNodes: TExplorerNodes): Boolean;
var
  List: TExplorerNodesList;
begin
  List := TExplorerNodesList.Create(False);
  try
    ExplorerClipboard.GetItems(List);
    Result := ExplorerNodes.CanPaste(List);
    if Assigned(FOnCanPaste) then FOnCanPaste(Self, ExplorerNodes, List, Result);
  finally
    List.Free;
  end;
end;

procedure TExplorerSource.DoDblClick(ExplorerNodes: TExplorerNodes);
begin
  if ExplorerNodes.Enabled then
  begin
    if Assigned(FOnDblClick) then
      FOnDblClick(Self, ExplorerNodes)
    else
      ExplorerNodes.DoDblClick;
  end;
end;

procedure TExplorerSource.DoCopy(List: TExplorerNodesList);
begin
  if Assigned(FOnCopy) then
    FOnCopy(Self, List)
  else
    ExplorerClipboard.Copy(List);
end;

procedure TExplorerSource.DoCut(List: TExplorerNodesList);
begin
  if Assigned(FOnCut) then
    FOnCut(Self, List)
  else
    ExplorerClipboard.Cut(List);
end;

procedure TExplorerSource.DoDelete(List: TExplorerNodesList);
var
  I: Integer;
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, List)
  else begin
    for I := 0 to List.Count - 1 do
      with List[I] do
        if (ClipboardMode = cmParent) and Assigned(Parent) then
        begin
          Parent.DoDeleteList(List);
          Exit;
        end;
    for I := List.Count - 1 downto 0 do
      List[I].Delete;
  end;
end;

procedure TExplorerSource.DoDragDrop(Dest: TExplorerNodes; List: TExplorerNodesList);
begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Dest, List) else Dest.DragDrop(List);
end;

procedure TExplorerSource.DoEdit(ExplorerNodes: TExplorerNodes; var Text: string);
begin
  if Assigned(FOnEdit) then FOnEdit(Self, ExplorerNodes, Text);
  ExplorerNodes.DoEdit(Text);
end;

procedure TExplorerSource.DoEditing(ExplorerNodes: TExplorerNodes; var Allow: Boolean);
begin
  Allow := ExplorerNodes.CanEdit;
  if Assigned(FOnEditing) then FOnEditing(Self, ExplorerNodes, Allow);
end;

procedure TExplorerSource.DoExpanding(ExplorerNodes: TExplorerNodes);
begin
  if Assigned(FOnExpanding) then FOnExpanding(Self, ExplorerNodes);
  ExplorerNodes.Expand;
end;

procedure TExplorerSource.EnableControls;
begin
  Dec(FDisableCount);
  if (FDisableCount = 0) then ExplorerEvent(nil, eeEnableControls, []);
end;

procedure TExplorerSource.ExplorerEvent(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
begin
  NotifyExplorerLinks(ExplorerNodes, Event, NodeTypes);
end;

function TExplorerSource.GetDisabled: Boolean;
begin
  Result := FDisableCount > 0;
end;

function TExplorerSource.GetImageIndex(ExplorerNodes: TExplorerNodes; Large: Boolean): Integer;
var
  Images, SelfImages: TExplorerImageList;
begin
  if Large then
  begin
    Images := ExplorerNodes.GetLargeImages;
    SelfImages := GetLargeImages;
    if not Assigned(SelfImages) then
      Result := 0
    else if (not (csDesigning in ComponentState) or (SelfImages.Owner = Self))
      and Assigned(Images) and (Images <> SelfImages) then
    begin
      Result := RegisterImage(Images, SelfImages, ExplorerNodes.ImageIndex);
    end else
      Result := ExplorerNodes.ImageIndex;
  end else begin
    Images := ExplorerNodes.GetSmallImages;
    SelfImages := GetSmallImages;
    if not Assigned(SelfImages) then
      Result := -1
    else if (not (csDesigning in ComponentState) or (SelfImages.Owner = Self))
      and Assigned(Images) and (Images <> SelfImages) then
    begin
      Result := RegisterImage(Images, SelfImages, ExplorerNodes.ImageIndex);
    end else
      Result := ExplorerNodes.ImageIndex;
  end;
end;

function TExplorerSource.GetLargeImages: TExplorerImageList;
begin
  if (ilLarge in FImages) then
  begin
    Result := LargeImages;
    if not Assigned(Result) and Assigned(FExplorerRoot) then
      Result := FExplorerRoot.GetLargeImages;
    if not Assigned(Result) then Result := FImageLists[3];
  end else
    Result := nil;
end;

function TExplorerSource.GetOverlayIndex(ExplorerNodes: TExplorerNodes): Integer;
var
  Images, SelfImages: TExplorerImageList;
begin
  Images := ExplorerNodes.GetStateImages;
  SelfImages := GetStateImages;
  if not Assigned(SelfImages) or (ExplorerNodes.OverlayIndex < 0) then
    Result := -1
  else if (not (csDesigning in ComponentState) or (SelfImages.Owner = Self))
    and Assigned(Images) and (Images <> SelfImages) then
  begin
    Result := RegisterImage(Images, SelfImages, ExplorerNodes.OverlayIndex);
  end else
    Result := ExplorerNodes.OverlayIndex;
end;

function TExplorerSource.GetSelectedIndex(ExplorerNodes: TExplorerNodes): Integer;
var
  Images, SelfImages: TExplorerImageList;
begin
  Images := ExplorerNodes.GetSmallImages;
  SelfImages := GetSmallImages;
  if not Assigned(SelfImages) or (ExplorerNodes.SelectedIndex < 0) then
    Result := -1
  else if (not (csDesigning in ComponentState) or (SelfImages.Owner = Self))
    and Assigned(Images) and (Images <> SelfImages) then
  begin
    Result := RegisterImage(Images, SelfImages, ExplorerNodes.SelectedIndex);
  end else
    Result := ExplorerNodes.SelectedIndex;
end;

function TExplorerSource.GetSmallImages: TExplorerImageList;
begin
  if (ilSmall in FImages) then
  begin
    Result := SmallImages;
    if not Assigned(Result) and Assigned(FExplorerRoot) then
      Result := FExplorerRoot.GetSmallImages;
    if not Assigned(Result) then Result := FImageLists[4];
  end else
    Result := nil;
end;

function TExplorerSource.GetStateIndex(ExplorerNodes: TExplorerNodes): Integer;
var
  Images, SelfImages: TExplorerImageList;
begin
  Images := ExplorerNodes.GetStateImages;
  SelfImages := GetStateImages;
  if not Assigned(SelfImages) or (ExplorerNodes.StateIndex < 0) then
    Result := -1
  else if (not (csDesigning in ComponentState) or (SelfImages.Owner = Self))
    and Assigned(Images) and (Images <> SelfImages) then
  begin
    Result := RegisterImage(Images, SelfImages, ExplorerNodes.StateIndex);
  end else
    Result := ExplorerNodes.StateIndex;
end;

function TExplorerSource.GetStateImages: TExplorerImageList;
begin
  if (ilState in FImages) then
  begin
    Result := StateImages;
    if not Assigned(Result) and Assigned(FExplorerRoot) then
      Result := FExplorerRoot.GetStateImages;
    if not Assigned(Result) then Result := FImageLists[5];
  end else
    Result := nil;
end;

procedure TExplorerSource.NotifyExplorerLinks(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
var
  I: Integer;
begin
  for I := 0 to ListCount(FLinks) - 1 do
    TExplorerLink(FLinks[I]).ExplorerEvent(ExplorerNodes, Event, NodeTypes);
end;

procedure TExplorerSource.InsertExplorerLink(ExplorerLink: TExplorerLink);
begin
  if (ListIndexOf(FLinks, ExplorerLink) < 0) then
  begin
    ListAdd(FLinks, ExplorerLink);
    ExplorerLink.FExplorerSource := Self;
    ExplorerLink.ExplorerEvent(FExplorerRoot, eeActiveChange, []);
  end;
end;

procedure TExplorerSource.RemoveExplorerLink(ExplorerLink: TExplorerLink);
var
  I: Integer;
begin
  I := ListIndexOf(FLinks, ExplorerLink);
  if I >= 0 then
  begin
    ListRemove(FLinks, ExplorerLink);
    ExplorerLink.FExplorerSource := nil;
    ExplorerLink.ExplorerEvent(nil, eeActiveChange, []);
  end;
end;

procedure TExplorerSource.SetExplorerRoot(Value: TExplorerNodes);
begin
  if (FExplorerRoot <> Value) then
  begin
    DisableControls;
    try
      if Assigned(FExplorerRoot) then
      begin
        FExplorerRoot.RemoveExplorerSource(Self);
        ClearImages(FImageLists[3]);
        ClearImages(FImageLists[4]);
        ClearImages(FImageLists[5]);
      end;
      if Assigned(Value) then Value.InsertExplorerSource(Self);
    finally
      EnableControls;
    end;
    if not (csDestroying in ComponentState) then DoActiveChanged;
  end;
end;

procedure TExplorerSource.SetImageList(Index: Integer; Value: TExplorerImageList);
begin
  if (FImageLists[Index] <> Value) then
  begin
    FImageLists[Index] := Value;
    if Assigned(Value) then FreeNotification(Value);
    ExplorerEvent(nil, eeLargeImagesChange - Index, []);
  end;
end;

procedure TExplorerSource.SetImages(Value: TExplorerImageListTypes);
var
  I: TExplorerImageListType;
begin
  for I := Low(TExplorerImageListType) to High(TExplorerImageListType) do
  begin
    if (I in FImages) and not (I in Value) then
    begin
      Exclude(FImages, I);
      FImageLists[3 + Integer(I)].Free;
      FImageLists[3 + Integer(I)] := nil;
      ExplorerEvent(nil, eeLargeImagesChange - Integer(I), []);
    end else if not (I in FImages) and (I in Value) then
    begin
      Include(FImages, I);
      FImageLists[3 + Integer(I)] := TExplorerImageList.Create(Self);
      if I = ilLarge then
      with FImageLists[3 + Integer(I)] do
      begin
        Width := 32;
        Height := 32;
      end;
      ExplorerEvent(nil, eeLargeImagesChange - Integer(I), []);
    end;
  end;
end;

procedure TExplorerSource.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FExplorerRoot) then
      SetExplorerRoot(nil)
    else for I := 0 to 2 do if (FImageLists[I] = AComponent) then
      SetImageList(I, nil);
  end;
end;

{ TExplorerLink }
constructor TExplorerLink.Create(IgnoreRefs: Boolean);
begin
  FDrawing := True;
  FNodeTypes := ntNodeTypesAll;
  FIgnoreRefs := IgnoreRefs;
end;

destructor TExplorerLink.Destroy;
begin
  SetExplorerSource(nil);
  inherited;
end;

function TExplorerLink.AcceptIndex(Sender: TObject; ExplorerNodes: TExplorerNodes): Integer;
var
  I: Integer;
  Nodes: TExplorerNodes;
begin
  Result := -1;
  if AcceptsNodes(Sender, ExplorerNodes) then
  begin
    Result := 0;
    Nodes := ExplorerNodes.Parent;
    for I := 0 to ExplorerNodes.Index - 1 do
      if AcceptsNodes(Sender, Nodes[I]) then Inc(Result);
  end;
end;

function TExplorerLink.AcceptsChildren(Sender: TObject; ExplorerNodes: TExplorerNodes): Boolean;
var
  I: Integer;
begin
  for I := 0 to ExplorerNodes.Count - 1 do
  begin
    Result := AcceptsNodes(Sender, ExplorerNodes[I]);
    if Result then Exit;
  end;
  Result := False;
end;

function TExplorerLink.AcceptsNodes(Sender: TObject; ExplorerNodes: TExplorerNodes): Boolean;
begin
  with ExplorerNodes do
    Result := ((csDesigning in ExplorerNodes.ComponentState) or (ExplorerNodes.Visible)) and
      (ExplorerNodes.NodeType in NodeTypes);
  if Assigned(FOnAcceptNode) then FOnAcceptNode(Sender, ExplorerNodes, Result);
end;

procedure TExplorerLink.SetExplorerSource(Value: TExplorerSource);
begin
  if (FExplorerSource <> Value) then
  begin
    if Assigned(FExplorerSource) then FExplorerSource.RemoveExplorerLink(Self);
    if Assigned(Value) then Value.InsertExplorerLink(Self);
  end;
end;

procedure TExplorerLink.SetNodeTypes(Value: TExplorerNodeTypes);
begin
  if (FNodeTypes <> Value) then
  begin
    FNodeTypes := Value;
    if Assigned(ExplorerSource) and Assigned(ExplorerSource.ExplorerRoot) then
      ExplorerEvent(ExplorerSource.ExplorerRoot, eeNodesChangeChildren, ntNodeTypesAll);
  end;
end;

procedure TExplorerLink.SetOnAcceptNode(Value: TExplorerAcceptNodeEvent);
begin
  if (@FOnAcceptNode <> @Value) then
  begin
    FOnAcceptNode := Value;
    if Assigned(ExplorerSource) and Assigned(ExplorerSource.ExplorerRoot) then
      ExplorerEvent(ExplorerSource.ExplorerRoot, eeNodesChangeChildren, ntNodeTypesAll);
  end;
end;

procedure TExplorerLink.ExplorerEvent(ExplorerNodes: TExplorerNodes; Event: Integer; NodeTypes: TExplorerNodeTypes);
begin
end;

procedure TExplorerLink.InsertReference(ExplorerNodes: TExplorerNodes; AObject: TObject);
var
  P: PRefData;
begin
  New(P);
  try
    P^.ExplorerLink := Self;
    P^.Data := AObject;
    P^.Ignore := FIgnoreRefs;
    ListAdd(ExplorerNodes.FReferences, P);
  except
    Dispose(P);
    raise;
  end;
end;

procedure TExplorerLink.RemoveReference(ExplorerNodes: TExplorerNodes);
var
  P: PRefData;
begin
  P := FindRefData(ExplorerNodes);
  if Assigned(P) then with ExplorerNodes do
  begin
    ListRemove(FReferences, P);
    Dispose(P);
    if IsRunTime and not (csDestroying in ComponentState)
      and Assigned(Parent) and not HasRefs(True) then Parent.Collapse;
  end;
end;

function TExplorerLink.FindRefData(ExplorerNodes: TExplorerNodes): Pointer;
var
  I: Integer;
  P: PRefData;
begin
  for I := ListCount(ExplorerNodes.FReferences) - 1 downto 0 do
  begin
    P := ExplorerNodes.FReferences[I];
    if P^.ExplorerLink = Self then
    begin
      Result := P;
      Exit;
    end;
  end;
  Result := nil;
end;

function TExplorerLink.FindReference(ExplorerNodes: TExplorerNodes): Pointer;
begin
  Result := FindRefData(ExplorerNodes);
  if Assigned(Result) then Result := PRefData(Result)^.Data;
end;

procedure TExplorerLink.SetDrawing(Value: Boolean);
begin
  if FDrawing <> Value then
  begin
    FDrawing := Value;
    ExplorerEvent(nil, eeInvalidate, []);
  end;
end;

{ TExplorerDragObject }
constructor TExplorerDragObject.Create(AControl: TControl);
begin
  FControl := AControl;
  FList := TExplorerNodesList.Create(False);
end;

destructor TExplorerDragObject.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TExplorerDragObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  if not Accepted then
  begin
    TControlHack(Control).DragCanceled;
    Target := nil;
  end;
  TControlHack(Control).DoEndDrag(Target, X, Y);
  Free;
end;

{$IFDEF _D4_}
function TExplorerDragObject.GetDragImages: TDragImageList;
{$ELSE}
function TExplorerDragObject.GetDragImages: TCustomImageList;
{$ENDIF}
begin
  Result := TControlHack(Control).GetDragImages;
end;

function TExplorerDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := TControlHack(Control).DragCursor else
    Result := crNoDrop;
end;

procedure TExplorerDragObject.HideDragImage;
begin
  if GetDragImages <> nil then GetDragImages.HideDragImage;
end;

procedure TExplorerDragObject.ShowDragImage;
begin
  if GetDragImages <> nil then GetDragImages.ShowDragImage;
end;

initialization

finalization
  FExplorerClipboard.Free;
  FExplorerFormClassList.Free;

end.

