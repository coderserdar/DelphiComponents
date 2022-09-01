unit gmTreeView;

interface
{$I Version.inc}
uses
  Windows, Classes, Messages, Graphics,// TypInfo,
  Controls, ComCtrls, CommCtrl, Dialogs,
  SysUtils, Math, Variants,
  Menus, DB,
{$IFDEF Delphi7}
  UxTheme
{$ELSE}
  gmXPThemes
{$ENDIF}
  , AdvEditControls_Const;


type
  TAdvDBlcEditing = (eoAutoCheckKeyValue,
                     eoInsertEnabled, eoEditEnabled, eoDeleteEnabled,
                     eoKeyValueEditable, eoFieldDisplayLabel);

  TTreeBuildMethod = (tbmDatasetFilter, tbmParentFieldSort, tbmProcessRawDataset);
  TTVDragDropForeignObjectEvent = procedure(Source: TObject; Parent: TTreeNode;
                                            var NodeText: string; var NodeData: Pointer) of object;
  TTVDragDropForeignDBObjectEvent = procedure(Source: TObject; Parent: TTreeNode;
                                              var NodeText: string; var NodeKeyValue: Variant) of object;

  TAdvCustomTreeView = class(TCustomTreeView)
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    function CanDeleteNode: Boolean; virtual;
    function CanInsertNode: Boolean; virtual;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                       var Accept: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;

    procedure DoOnDragDropForeignObject(Source: TObject; TargetNode: TTreeNode;
                                        var NodeText: string; var NodeData: Pointer); virtual;
    procedure DoOnAttachCopyMoveNode(Node: TTreeNode); virtual;
    procedure DoOnDeleteNode(Node: TTreeNode); virtual;
    procedure DoOnFreeNode(Node: TTreeNode); virtual;
    property OnDragDropForeignObject: TTVDragDropForeignObjectEvent
             read FOnDragDropForeignObject write FOnDragDropForeignObject;
    property ColorWhenFocused: TColor read FColorWhenFocused write FColorWhenFocused
             default clInfoBk; // clWindow; // $CDE6FF;
    property FontColorWhenFocused: TColor read FFontColorWhenFocused write FFontColorWhenFocused
             default clHotLight; // clActiveCaption; // clWindowText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  published
    property DragDropEnabled: Boolean read FDragDropEnabled write FDragDropEnabled
             default True;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete
             default True;
    property OnDeletingNode: TTVChangingEvent read FOnDeletingNode write FOnDeletingNode;
    property OnNodeDeleted: TNotifyEvent read FOnNodeDeleted write FOnNodeDeleted;
    property Align;
    property Anchors;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection default False;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
{$IFDEF Delphi2009}
    property ParentDoubleBuffered;
{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property ReadOnly;
    property RightClickSelect default True;
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
{$IFDEF Delphi2009}
    property Touch;
{$ENDIF}
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
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
{$IFDEF Delphi2009}
    property OnGesture;
{$ENDIF}
    property OnGetImageIndex;
    property OnGetSelectedIndex;
{$IFDEF Delphi2009}
    property OnHint;
{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF Delphi2009}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  TAdvDBltvEditing = eoAutoCheckKeyValue .. eoDeleteEnabled;
  TAdvDBltvEditOpt = set of TAdvDBltvEditing;

  TAdvDBCustomTreeView = class;

  TTreeDataLink = class(TDataLink)
  protected
    procedure ActiveChanged; override;
  public
    constructor Create(AOwner: TAdvDBCustomTreeView);
  end;


  TDBTreeViewDataLinkParams = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ATreeView: TAdvDBCustomTreeView); virtual;
    property TreeView: TAdvDBCustomTreeView read FTreeView;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisplayField: string read FDisplayFieldName write SetDisplayFieldName;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property ParentField: string read FParentFieldName write SetParentFieldName;
    property RootAsNode: Boolean read FRootAsNode write SetRootAsNode;
    property RootValue: Variant read FRootValue write SetRootValue;
    property SelectNodeOnActive: Boolean read FSelectNodeOnActive
             write FSelectNodeOnActive default False;
    property TreeBuildMethod: TTreeBuildMethod read FTreeBuildMethod
             write FTreeBuildMethod default tbmProcessRawDataset;
    property EditingOptions: TAdvDBltvEditOpt read FEditingOptions write FEditingOptions;
  end;

  TAdvDBCustomTreeView = class(TAdvCustomTreeView)
  protected
    procedure Change(Node: TTreeNode); override;
    function CanDeleteNode: Boolean; override;
    function CanInsertNode: Boolean; override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    function NodeChecked(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;

    procedure DoOnDragDropForeignObject(Source: TObject; TargetNode: TTreeNode;
                                        var NodeText: string; var NodeData: Pointer); override;
    procedure DoOnAttachCopyMoveNode(Node: TTreeNode); override;
    procedure DoOnDeleteNode(Node: TTreeNode); override;
    procedure DoOnFreeNode(Node: TTreeNode); override;
    property OnPostInsertedNode: TTVChangedEvent
             read FOnPostInsertedNode write FOnPostInsertedNode;
    property OnDragDropForeignObject: TTVDragDropForeignDBObjectEvent
             read FOnDragDropForeignDBObject write FOnDragDropForeignDBObject;
    property Field: TField read FKeyField;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property ListActive: Boolean read FListActive;
    property MultiParentAllowed: Boolean read FMultiParentAllowed
             write SetMultiParentAllowed default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InsertNode(Parent: TTreeNode; s: String): TTreeNode;
    procedure RebuildNode(Node: TTreeNode);
    function GetNodeKeyValue(Node: TTreeNode): Variant;
    function GetNodeByKeyValue(KeyFieldValue: Variant): TTreeNode;
    property DataLinkParams: TDBTreeViewDataLinkParams read FDataLinkParams
             write FDataLinkParams;
  end;

  TPopupDBTreeView = class(TAdvDBCustomTreeView)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoCloseUp(Accept: Boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property SizingBorder: Boolean read FSizingBorder write SetSizingBorder
             default True;
    property SelectOnlyChildlessNodes: Boolean read FSelectOnlyChildlessNodes
             write FSelectOnlyChildlessNodes;
  end;


implementation


end.
