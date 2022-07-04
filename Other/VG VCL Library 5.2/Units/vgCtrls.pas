{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Visual components                             }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgCtrls;

interface
uses Windows, Messages, Classes, Graphics, Controls, Menus, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, Forms, vgShlObj {$IFDEF _D4_}, ImgList{$ENDIF};

type

{$IFDEF _D4_}
  TControlImageList = TCustomImageList;
{$ELSE}
  TControlImageList = TImageList;
{$ENDIF}

{ TvgSplitter }
  TvgSplitter = class(TGraphicControl)
  private
    FLineDC: HDC;
    FDownPos: TPoint;
    FSplit: Integer;
    FMinSize: Cardinal;
    FMaxSize: Integer;
    FControl: TControl;
    FNewSize: Integer;
    FActiveControl: TWinControl;
    FOldKeyDown: TKeyEvent;
    FBeveled: Boolean;
    FLineVisible: Boolean;
    FOnMoved: TNotifyEvent;
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure ReleaseLineDC;
    procedure UpdateSize(X, Y: Integer);
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetBeveled(Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure StopSizing;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default alLeft;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property Color;
{$IFDEF _D4_}
    property Constraints;
{$ENDIF}
    property MinSize: Cardinal read FMinSize write FMinSize default 30;
    property ParentColor;
    property Visible;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

const
  scDefButtonShortCut = scAlt + VK_DOWN;

type
  TGlyphKind = (gkCustom, gkDropDown);

{ TCustomClickEdit }
  TCustomClickEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FButton: TSpeedButton;
    FBtnControl: TWinControl;
    FButtonShortCut: TShortCut;
    FCanvas: TControlCanvas;
    FFocused: Boolean;
    FCaret: Boolean;
    FGlyphKind: TGlyphKind;
    FOnButtonClick: TNotifyEvent;
    procedure EditButtonClick(Sender: TObject);
    function GetButtonEnabled: Boolean;
    function GetButtonVisible: Boolean;
    function GetButtonWidth: Integer;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    function IsCustomGlyph: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonEnabled(Value: Boolean);
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonVisible(Value: Boolean);
    procedure SetCaret(Value: Boolean);
    procedure SetEditRect;
    procedure SetFocused(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
{$IFDEF _D3_}
    function GetFlat: Boolean;
    procedure SetFlat(Value: Boolean);
{$ENDIF}
  protected
    procedure ButtonClick; dynamic;
    procedure ButtonReleased; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetTextMargins: TPoint;
    function IsCombo: Boolean; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ButtonEnabled: Boolean read GetButtonEnabled write SetButtonEnabled default True;
    property ButtonShortCut: TShortCut read FButtonShortCut write FButtonShortCut default scDefButtonShortCut;
    property ButtonVisible: Boolean read GetButtonVisible write SetButtonVisible default True;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 15;
    property Caret: Boolean read FCaret write SetCaret default True;
{$IFDEF _D3_}
    property Flat: Boolean read GetFlat write SetFlat default False;
{$ENDIF}
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property GlyphKind: TGlyphKind read FGlyphKind write SetGlyphKind default gkCustom;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TClickEdit }
  TClickEdit = class(TCustomClickEdit)
  published
    property Alignment;
    property ButtonEnabled;
    property ButtonShortCut;
    property ButtonVisible;
    property ButtonWidth;
    property Caret;
    property Glyph;
    property GlyphKind;
    property NumGlyphs;
    property OnButtonClick;
{$IFDEF _D3_}
    property Flat;
{$ENDIF}
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TJustifyEdit }
  TJustifyEdit = class(TClickEdit)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ButtonVisible default False;
  end;

{ TCustomComboBoxEdit }
  TDropDownAlign = (daLeft, daRight);

  TCustomComboBoxEdit = class(TCustomClickEdit)
  private
    FDisplayEmpty: string;
    FDropDownAlign: TDropDownAlign;
    FDropDownHeight, FDropDownWidth: Integer;
    FDroppedDown: Boolean;
    FPopup, FPopupControl: TWinControl;
    procedure HidePopupControl;
    procedure ShowPopupControl;
    procedure SetDisplayEmpty(Value: string);
  protected
    procedure ButtonClick; override;
    procedure ButtonReleased; override;
    procedure CloseUp(Accept: Boolean);
    procedure CreatePopupControl(var Control: TWinControl); dynamic; abstract;
    procedure DoCloseUp(Accept: Boolean); dynamic;
    procedure DoShow; dynamic;
    procedure WndProc(var Message: TMessage); override;
    function IsCombo: Boolean; override;
    property DisplayEmpty: string read FDisplayEmpty write SetDisplayEmpty;
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownHeight: Integer read FDropDownHeight write FDropDownHeight default 100;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property DroppedDown: Boolean read FDroppedDown;
    property PopupControl: TWinControl read FPopupControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
  end;


{$IFNDEF _D4_}
  TCustomDrawTarget = (dtControl, dtItem, dtSubItem);

  TCustomDrawStage = (cdPrePaint, cdPostPaint, cdPreErase, cdPostErase);

  TCustomDrawState = set of (cdsSelected, cdsGrayed, cdsDisabled, cdsChecked,
    cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate);

  TTVCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect;
    var DefaultDraw: Boolean) of object;

  TTVCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
{$ENDIF}

  TTVGetItemParamsEvent = procedure (Sender: TObject; Node: TTreeNode; AFont: TFont;
    var Background: TColor; var State: TCustomDrawState) of object;

{ TvgCustomTreeViewCombo }
  TvgCustomTreeViewCombo = class(TCustomComboBoxEdit)
  private
    FCanvas: TCanvas;
    function GetTreeView: TCustomTreeView;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    { TreeView properties }
    function GetTVShowButtons: Boolean;
    procedure SetTVShowButtons(Value: Boolean);
    function GetTVShowLines: Boolean;
    procedure SetTVShowLines(Value: Boolean);
    function GetTVShowRoot: Boolean;
    procedure SetTVShowRoot(Value: Boolean);
    function GetTVReadOnly: Boolean;
    procedure SetTVReadOnly(Value: Boolean);
    function GetTVIndent: Integer;
    procedure SetTVIndent(Value: Integer);
{$IFDEF _D4_}
    function GetTVAutoExpand: Boolean;
    procedure SetTVAutoExpand(Value: Boolean);
    function GetTVHotTrack: Boolean;
    procedure SetTVHotTrack(Value: Boolean);
    function GetTVRowSelect: Boolean;
    procedure SetTVRowSelect(Value: Boolean);
    function GetTVToolTips: Boolean;
    procedure SetTVToolTips(Value: Boolean);
{$ENDIF}
    { TreeView events }
    function GetTVOnClick: TNotifyEvent;
    procedure SetTVOnClick(Value: TNotifyEvent);
    function GetTVOnDragDrop: TDragDropEvent;
    procedure SetTVOnDragDrop(Value: TDragDropEvent);
    function GetTVOnDragOver: TDragOverEvent;
    procedure SetTVOnDragOver(Value: TDragOverEvent);
    function GetTVOnStartDrag: TStartDragEvent;
    procedure SetTVOnStartDrag(Value: TStartDragEvent);
    function GetTVOnEndDrag: TEndDragEvent;
    procedure SetTVOnEndDrag(Value: TEndDragEvent);
    function GetTVOnMouseDown: TMouseEvent;
    procedure SetTVOnMouseDown(Value: TMouseEvent);
    function GetTVOnMouseMove: TMouseMoveEvent;
    procedure SetTVOnMouseMove(Value: TMouseMoveEvent);
    function GetTVOnMouseUp: TMouseEvent;
    procedure SetTVOnMouseUp(Value: TMouseEvent);
    function GetTVOnDblClick: TNotifyEvent;
    procedure SetTVOnDblClick(Value: TNotifyEvent);
    function GetTVOnKeyDown: TKeyEvent;
    procedure SetTVOnKeyDown(Value: TKeyEvent);
    function GetTVOnKeyPress: TKeyPressEvent;
    procedure SetTVOnKeyPress(Value: TKeyPressEvent);
    function GetTVOnKeyUp: TKeyEvent;
    procedure SetTVOnKeyUp(Value: TKeyEvent);
    function GetTVOnEditing: TTVEditingEvent;
    procedure SetTVOnEditing(Value: TTVEditingEvent);
    function GetTVOnEdited: TTVEditedEvent;
    procedure SetTVOnEdited(Value: TTVEditedEvent);
    function GetTVOnExpanding: TTVExpandingEvent;
    procedure SetTVOnExpanding(Value: TTVExpandingEvent);
    function GetTVOnExpanded: TTVExpandedEvent;
    procedure SetTVOnExpanded(Value: TTVExpandedEvent);
    function GetTVOnCollapsing: TTVCollapsingEvent;
    procedure SetTVOnCollapsing(Value: TTVCollapsingEvent);
    function GetTVOnCompare: TTVCompareEvent;
    procedure SetTVOnCompare(Value: TTVCompareEvent);
    function GetTVOnCollapsed: TTVExpandedEvent;
    procedure SetTVOnCollapsed(Value: TTVExpandedEvent);
    function GetTVOnChanging: TTVChangingEvent;
    procedure SetTVOnChanging(Value: TTVChangingEvent);
    function GetTVOnChange: TTVChangedEvent;
    procedure SetTVOnChange(Value: TTVChangedEvent);
    function GetTVOnDeletion: TTVExpandedEvent;
    procedure SetTVOnDeletion(Value: TTVExpandedEvent);
    function GetTVOnGetImageIndex: TTVExpandedEvent;
    procedure SetTVOnGetImageIndex(Value: TTVExpandedEvent);
    function GetTVOnGetSelectedIndex: TTVExpandedEvent;
    procedure SetTVOnGetSelectedIndex(Value: TTVExpandedEvent);
{$IFDEF _D4_}
    function GetTVOnCustomDraw: TTVCustomDrawEvent;
    procedure SetTVOnCustomDraw(Value: TTVCustomDrawEvent);
    function GetTVOnCustomDrawItem: TTVCustomDrawItemEvent;
    procedure SetTVOnCustomDrawItem(Value: TTVCustomDrawItemEvent);
{$ENDIF}
    function GetTVItems: TTreeNodes;
    procedure SetTVItems(Value: TTreeNodes);
    function GetTVSelected: TTreeNode;
    procedure SetTVSelected(Value: TTreeNode);
    function GetTVImages: TControlImageList;
    function GetTVStateImages: TControlImageList;
    procedure SetTVImages(Value: TControlImageList);
    procedure SetTVStateImages(Value: TControlImageList);
    function GetTVOnGetItemParams: TTVGetItemParamsEvent;
    procedure SetTVOnGetItemParams(Value: TTVGetItemParamsEvent);
  protected
    procedure CreatePopupControl(var Control: TWinControl); override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    property Canvas: TCanvas read FCanvas;
    property TreeView: TCustomTreeView read GetTreeView;
    { TreeView properties }
{$IFDEF _D4_}
    property TVAutoExpand: Boolean read GetTVAutoExpand write SetTVAutoExpand default False;
    property TVHotTrack: Boolean read GetTVHotTrack write SetTVHotTrack default False;
    property TVRowSelect: Boolean read GetTVRowSelect write SetTVRowSelect default False;
    property TVToolTips: Boolean read GetTVToolTips write SetTVToolTips default True;
{$ENDIF}
    property TVShowButtons: Boolean read GetTVShowButtons write SetTVShowButtons default True;
    property TVShowLines: Boolean read GetTVShowLines write SetTVShowLines default True;
    property TVShowRoot: Boolean read GetTVShowRoot write SetTVShowRoot default True;
    property TVReadOnly: Boolean read GetTVReadOnly write SetTVReadOnly default False;
    property TVIndent: Integer read GetTVIndent write SetTVIndent;
    property Images: TControlImageList read GetTVImages write SetTVImages;
    property StateImages: TControlImageList read GetTVStateImages write SetTVStateImages;
    property Items: TTreeNodes read GetTVItems write SetTVItems;
    property Selected: TTreeNode read GetTVSelected write SetTVSelected;
    { TreeView events }
    property TVOnClick: TNotifyEvent read GetTVOnClick write SetTVOnClick;
    property TVOnDragDrop: TDragDropEvent read GetTVOnDragDrop write SetTVOnDragDrop;
    property TVOnDragOver: TDragOverEvent read GetTVOnDragOver write SetTVOnDragOver;
    property TVOnStartDrag: TStartDragEvent read GetTVOnStartDrag write SetTVOnStartDrag;
    property TVOnEndDrag: TEndDragEvent read GetTVOnEndDrag write SetTVOnEndDrag;
    property TVOnMouseDown: TMouseEvent read GetTVOnMouseDown write SetTVOnMouseDown;
    property TVOnMouseMove: TMouseMoveEvent read GetTVOnMouseMove write SetTVOnMouseMove;
    property TVOnMouseUp: TMouseEvent read GetTVOnMouseUp write SetTVOnMouseUp;
    property TVOnDblClick: TNotifyEvent read GetTVOnDblClick write SetTVOnDblClick;
    property TVOnKeyDown: TKeyEvent read GetTVOnKeyDown write SetTVOnKeyDown;
    property TVOnKeyPress: TKeyPressEvent read GetTVOnKeyPress write SetTVOnKeyPress;
    property TVOnKeyUp: TKeyEvent read GetTVOnKeyUp write SetTVOnKeyUp;
    property TVOnEditing: TTVEditingEvent read GetTVOnEditing write SetTVOnEditing;
    property TVOnEdited: TTVEditedEvent read GetTVOnEdited write SetTVOnEdited;
    property TVOnExpanding: TTVExpandingEvent read GetTVOnExpanding write SetTVOnExpanding;
    property TVOnExpanded: TTVExpandedEvent read GetTVOnExpanded write SetTVOnExpanded;
    property TVOnCollapsing: TTVCollapsingEvent read GetTVOnCollapsing write SetTVOnCollapsing;
    property TVOnCompare: TTVCompareEvent read GetTVOnCompare write SetTVOnCompare;
    property TVOnCollapsed: TTVExpandedEvent read GetTVOnCollapsed write SetTVOnCollapsed;
    property TVOnChanging: TTVChangingEvent read GetTVOnChanging write SetTVOnChanging;
    property TVOnChange: TTVChangedEvent read GetTVOnChange write SetTVOnChange;
    property TVOnDeletion: TTVExpandedEvent read GetTVOnDeletion write SetTVOnDeletion;
    property TVOnGetImageIndex: TTVExpandedEvent read GetTVOnGetImageIndex write SetTVOnGetImageIndex;
    property TVOnGetSelectedIndex: TTVExpandedEvent read GetTVOnGetSelectedIndex write SetTVOnGetSelectedIndex;
    property TVOnGetItemParams: TTVGetItemParamsEvent read GetTVOnGetItemParams write SetTVOnGetItemParams;
{$IFDEF _D4_}
    property TVOnCustomDraw: TTVCustomDrawEvent read GetTVOnCustomDraw write SetTVOnCustomDraw;
    property TVOnCustomDrawItem: TTVCustomDrawItemEvent read GetTVOnCustomDrawItem write SetTVOnCustomDrawItem;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TvgTreeViewCombo }
  TvgTreeViewCombo = class(TvgCustomTreeViewCombo)
  public
    property Canvas;
    property TreeView;
    property Selected;
  published
    property Alignment;
    property ButtonEnabled;
    property ButtonShortCut;
    property ButtonVisible;
    property ButtonWidth;
    property DisplayEmpty;
    property DropDownAlign;
    property DropDownHeight;
    property DropDownWidth;
    property Glyph;
    property GlyphKind;
    property NumGlyphs;
    property OnButtonClick;
{$IFDEF _D3_}
    property Flat;
{$ENDIF}
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
    { TreeView properties }
{$IFDEF _D4_}
    property TVAutoExpand;
    property TVHotTrack;
    property TVRowSelect;
    property TVToolTips;
{$ENDIF}
    property TVShowButtons;
    property TVShowLines;
    property TVShowRoot;
    property TVReadOnly;
    property TVIndent;
    property Images;
    property StateImages;    
    property Items;
    { TreeView events }
    property TVOnClick;
    property TVOnDragDrop;
    property TVOnDragOver;
    property TVOnStartDrag;
    property TVOnEndDrag;
    property TVOnMouseDown;
    property TVOnMouseMove;
    property TVOnMouseUp;
    property TVOnDblClick;
    property TVOnKeyDown;
    property TVOnKeyPress;
    property TVOnKeyUp;
    property TVOnEditing;
    property TVOnEdited;
    property TVOnExpanding;
    property TVOnExpanded;
    property TVOnCollapsing;
    property TVOnCompare;
    property TVOnCollapsed;
    property TVOnChanging;
    property TVOnChange;
    property TVOnDeletion;
    property TVOnGetImageIndex;
    property TVOnGetSelectedIndex;
    property TVOnGetItemParams;
{$IFDEF _D4_}
    property TVOnCustomDraw;
    property TVOnCustomDrawItem;
{$ENDIF}
  end;

  TTitlerPaintEvent = procedure (Sender: TObject; Canvas: TCanvas) of object;

{ TTitler }
  TTitler = class(TCustomPanel)
  private
    FTitles: TStrings;
    FTimer: TTimer;
    FPosition: Integer;
    FStep: Integer;
    FOnPaintAfter, FOnPaintBefore: TTitlerPaintEvent;
    procedure DoTimer(Sender: TObject);
    function GetInterval: Integer;
    procedure SetInterval(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetTitles(Value: TStrings);
    procedure TitlesChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Reset;
    property Position: Integer read FPosition;
{$IFDEF _D4_}
    property DockManager;
{$ENDIF}
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Titles: TStrings read FTitles write SetTitles;
    property Interval: Integer read GetInterval write SetInterval default 50;
    property OnPaintAfter: TTitlerPaintEvent read FOnPaintAfter write FOnPaintAfter;
    property OnPaintBefore: TTitlerPaintEvent read FOnPaintBefore write FOnPaintBefore;
    property Step: Integer read FStep write SetStep default 1;
  published
    property Align;
    property Alignment;
{$IFDEF _D4_}
    property AutoSize;
{$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
{$IFDEF _D4_}
    property UseDockManager default True;
    property DockSite;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnStartDock;
    property OnUnDock;
    property OnGetSiteInfo;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{$IFDEF _D4_}
{ TvgTabSheet }
  TvgTabSheet = class(TTabSheet)
  end;

{ TvgPageControl }
  TvgPageControl = class(TPageControl)
  end;
{$ELSE}

{ TvgTabSheet }
  TvgTabSheet = class(TTabSheet)
  private
    FImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex default 0;
  end;

  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);
  TTabStyle = (tsTabs, tsButtons, tsFlatButtons);

  TDrawTabEvent = procedure(Control: TCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;
  TTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;

{ TvgPageControl }
  TvgPageControl = class(TPageControl)
  private
    { Private declarations }
    FCanvas: TCanvas;
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TControlImageList;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FTabPosition: TTabPosition;
    FStyle: TTabStyle;
    FOnDrawTab: TDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    procedure ImageListChange(Sender: TObject);
    procedure SetHotTrack(Value: Boolean);
    procedure SetImages(Value: TControlImageList);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetStyle(Value: TTabStyle);
    procedure TabsChanged;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); dynamic;
    function GetImageIndex(TabIndex: Integer): Integer; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateTabImages;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TControlImageList read FImages write SetImages;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
  end;
{$ENDIF}

{ TvgCustomTreeView }
  TvgCustomTreeView = class(TCustomTreeView)
  private
    FFontChanged: Boolean;
    FOnGetItemParams: TTVGetItemParamsEvent;
{$IFNDEF _D4_}
  {$IFNDEF _D3_}
    FRightClickSelect: Boolean;
    FRClickNode: TTreeNode;
  {$ENDIF}
    FStyles: array [0..3] of Boolean;
    FCanvas: TCanvas;
    FCanvasChanged: Boolean;
{$ENDIF}
{$IFNDEF _D4_}
    FOnCustomDraw: TTVCustomDrawEvent;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
{$ENDIF}
    procedure FontChanged(Sender: TObject);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
{$IFNDEF _D4_}
    procedure CanvasChanged(Sender: TObject);
    procedure SetStyle(Index: Integer; Value: Boolean);
{$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetItemParams(Node: TTreeNode; AFont: TFont; var Background: TColor; var State: TCustomDrawState); virtual;
    procedure WndProc(var Message: TMessage); override;
{$IFNDEF _D4_}
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; virtual;
  {$IFNDEF _D3_}
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
  {$ENDIF}
    property AutoExpand: Boolean index 0 read FStyles[0] write SetStyle default False;
    property HotTrack: Boolean index 1 read FStyles[1] write SetStyle default False;
    property RowSelect: Boolean index 2 read FStyles[2] write SetStyle default False;
    property ToolTips: Boolean index 3 read FStyles[3] write SetStyle default True;
{$ENDIF}
    property OnGetItemParams: TTVGetItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
{$IFNDEF _D4_}
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
{$ENDIF}
{$IFNDEF _D4_}
    property Canvas: TCanvas read FCanvas;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectNode(Node: TTreeNode);
  end;

{ TvgTreeView }
  TvgTreeView = class (TvgCustomTreeView)
  published
    property AutoExpand;
    property HotTrack;
    property RowSelect;
    property ToolTips;
    property RightClickSelect;
    property OnGetItemParams;
    property OnCustomDraw;
    property OnCustomDrawItem;
  published
    property Align;
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property ParentBiDiMode;
    property ChangeDelay;
    property Constraints;
    property DragKind;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Images;
    property Indent;
    property Items;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsing;
    property OnCollapsed;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
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
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TCustomComboTreeView }
  TCustomComboTreeView = class(TvgCustomTreeView)
  private
    FComboBox: TvgCustomTreeViewCombo;
  protected
    procedure Change(Node: TTreeNode); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure GetItemParams(Node: TTreeNode; AFont: TFont; var Background: TColor;
      var State: TCustomDrawState); override;
    procedure UpdateComboBox;
    property ComboBox: TvgCustomTreeViewCombo read FComboBox;
  end;

  TLVGetItemParamsEvent = procedure (Sender: TObject; Item: TListItem; AFont: TFont;
    var Background: TColor; var State: TCustomDrawState) of object;

{ TvgCustomListView }
  TvgCustomListView = class(TCustomListView)
  private
{$IFNDEF _D4_}
    FCanvas: TCanvas;
{$ENDIF}
    FFontChanged: Boolean;
    FOnGetItemParams: TLVGetItemParamsEvent;
    procedure FontChanged(Sender: TObject);
    procedure SetBkColor(Value: TColor);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateWnd; override;
    procedure GetItemParams(Item: TListItem; AFont: TFont; var Background: TColor; var State: TCustomDrawState); virtual;
    property OnGetItemParams: TLVGetItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
{$IFNDEF _D4_}
    property Canvas: TCanvas read FCanvas;
{$ENDIF}
  public
    destructor Destroy; override;
    procedure SelectItem(Item: TListItem);
  end;

{ TvgListView }
  TvgListView = class(TvgCustomListView)
  public
    property Canvas;
  published
    property OnGetItemParams;
    property Align;
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property ParentBiDiMode;
    property Constraints;
    property DragKind;
    property FlatScrollBars;
    property FullDrag;
    property HotTrackStyles;
    property OwnerData;
    property OwnerDraw;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property AllocBy;
    property BorderStyle;
{$IFDEF _D3_}
    property Checkboxes;
    property GridLines;
    property HotTrack;
    property RowSelect;
{$ENDIF}
    property Color;
    property Columns;
    property ColumnClick;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property ReadOnly default False;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
{$IFDEF _D4_}
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDrawItem;
    property OnSelectItem;
    property OnGetImageIndex;
    property OnResize;
  {$IFDEF _D5_}
    property HoverTime;
    property ShowWorkAreas;
  {$ENDIF}
{$ENDIF}
    property OnDblClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnGetSubItemImage;
    property OnInfoTip;
  {$ENDIF}
{$ENDIF}
  end;

{ TvgNotebook }
  TTabSetAlign = (taNone, taTop, taBottom, taLeft, taRight);

  TvgNotebook = class(TNotebook)
  private
    { Private declarations }
    FTabSetAlign: TTabSetAlign;
    FBorderWidth: TBorderWidth;
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetTabSetAlign(Value: TTabSetAlign);
  protected
    { Protected declarations }
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 2;
    property TabSetAlign: TTabSetAlign read FTabSetAlign write SetTabSetAlign default taNone;
  end;

{ TvgTabSet }
  TvgTabSet = class;

  TvgTabList = class(TStringList)
  private
    FTabs: TvgTabSet;
  protected
    procedure Changed; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
  end;

  TTabSetStyle = TTabStyle;
  TTabChangeEvent = procedure(Sender: TObject; NewTab: Integer;
    var AllowChange: Boolean) of object;
  TDrawTabSetEvent = procedure(Sender: TObject; TabCanvas: TCanvas; R: TRect;
    Index: Integer; Selected: Boolean) of object;

  TvgTabSetStyle = (tsNormal, tsOwnerDraw);

  TvgTabSet = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FAutoSizeX, FAutoSizeY: Boolean;
    FFirstVisible: Integer;
    FDefaultFocusFont: Boolean;
    FFocusFont: TFont;
    FMargins: array [0..4] of Integer;
    FMemBitmap: TBitmap;
    FOffsetX, FOffsetY: Integer;
    FTabStyle: TvgTabSetStyle;
    FFocusOffset: Integer;
    FRoundBorders: Boolean;
    FTabs: TStrings;
    FTabIndex, FStreamedTabIndex: Integer;
    FTabsAlign: TTabSetAlign;
    FTabsHeight, FTabsWidth: Integer;
    FUpDown: TUpDown;
    { Events }
    FOnDrawTab: TDrawTabSetEvent;
    FOnChange: TTabChangeEvent;
    procedure FontChanged(Sender: TObject);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSizeX(Value: Boolean);
    procedure SetAutoSizeY(Value: Boolean);
    procedure SetDefaultFocusFont(Value: Boolean);
    procedure SetFirstVisible(Value: Integer);
    procedure SetFocusFont(Value: TFont);
    procedure SetFocusOffset(Value: Integer);
    procedure SetMargins(Index: Integer; Value: Integer);
    procedure SetRoundBorders(Value: Boolean);
    procedure SetTabStyle(Value: TvgTabSetStyle);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabList(Value: TStrings);
    procedure SetTabsAlign(Value: TTabSetAlign);
    procedure SetTabsHeight(Value: Integer);
    procedure SetTabsWidth(Value: Integer);
    function StoreFocusFont: Boolean;
    procedure UpdateTabSizes;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
      Selected: Boolean); virtual;
    procedure DrawTabText(TabCanvas: TCanvas; R: TRect; Index: Integer; Selected: Boolean);
    procedure SelectNext(Direction: Boolean);
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanChange(NewIndex: Integer): Boolean;
    function ItemAtPos(Pos: TPoint): Integer;
    function ItemRect(Item: Integer): TRect;
    property Canvas;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AutoSizeX: Boolean read FAutoSizeX write SetAutoSizeX default True;
    property AutoSizeY: Boolean read FAutoSizeY write SetAutoSizeY default True;
    property DefaultFocusFont: Boolean read FDefaultFocusFont write SetDefaultFocusFont default True;
    property FocusFont: TFont read FFocusFont write SetFocusFont stored StoreFocusFont;
    property FocusOffset: Integer read FFocusOffset write SetFocusOffset default 2;
    property MarginLeft: Integer index 0 read FMargins[0] write SetMargins default 2;
    property MarginRight: Integer index 1 read FMargins[1] write SetMargins default 2;
    property MarginTop: Integer index 2 read FMargins[2] write SetMargins default 2;
    property MarginBottom: Integer index 3 read FMargins[3] write SetMargins default 2;
    property MarginStart: Integer index 4 read FMargins[4] write SetMargins default 0;
    property RoundBorders: Boolean read FRoundBorders write SetRoundBorders default True;
    property TabStyle: TvgTabSetStyle read FTabStyle write SetTabStyle default tsNormal;
    property TabsAlign: TTabSetAlign read FTabsAlign write SetTabsAlign default taBottom;
    property TabsHeight: Integer read FTabsHeight write SetTabsHeight default 20;
    property TabsWidth: Integer read FTabsWidth write SetTabsWidth default 30;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property Tabs: TStrings read FTabs write SetTabList;
    property OnChange: TTabChangeEvent read FOnChange write FOnChange;
    property OnDrawTab: TDrawTabSetEvent read FOnDrawTab write FOnDrawTab;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Caption;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TClipboardShortCuts }
const
  scCopy      = scCtrl + ord('C');
  scCut       = scCtrl + ord('X');
  scDelete    = VK_DELETE;
  scPaste     = scCtrl + ord('V');
  scSelectAll = scCtrl + ord('A');

  scCopy2      = scCtrl + VK_INSERT;
  scCut2       = scShift+ VK_DELETE;
  scDelete2    = scCtrl + VK_DELETE;
  scPaste2     = scShift+ VK_INSERT;
  scSelectAll2 = scCtrl + ord('A');

type
  TClipboardAction = (caCopy, caCut, caDelete, caPaste, caSelectAll);
  TClipboardActions = set of TClipboardAction;

  TClipboardShortCuts = class(TPersistent)
  private
    FActions: TClipboardActions;
    FShortCuts: array [0..4] of TShortCut;
    FShortCuts2: array [0..4] of TShortCut;
    function StoreActions: Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function IsAction(Key: Word; Shift: TShiftState; Action: TClipboardAction): Boolean;
    function IsShortCutAction(ShortCut: TShortCut; Action: TClipboardAction): Boolean;
  published
    property Actions: TClipboardActions read FActions write FActions stored StoreActions;
    property Copy1st: TShortCut read FShortCuts[0] write FShortCuts[0] default scCopy;
    property Copy2nd: TShortCut read FShortCuts2[0] write FShortCuts2[0] default scCopy2;
    property Cut1st: TShortCut read FShortCuts[1] write FShortCuts[1] default scCut;
    property Cut2nd: TShortCut read FShortCuts2[1] write FShortCuts2[1] default scCut2;
    property Delete1st: TShortCut read FShortCuts[2] write FShortCuts[2] default scDelete;
    property Delete2nd: TShortCut read FShortCuts2[2] write FShortCuts2[2] default scDelete2;
    property Paste1st: TShortCut read FShortCuts[3] write FShortCuts[3] default scPaste;
    property Paste2nd: TShortCut read FShortCuts2[3] write FShortCuts2[3] default scPaste2;
    property SelectAll1st: TShortCut read FShortCuts[4] write FShortCuts[4] default scSelectAll;
    property SelectAll2nd: TShortCut read FShortCuts2[4] write FShortCuts2[4] default scSelectAll2;
  end;

{$IFNDEF _D3_}
  TCustomForm = TForm;
{$ENDIF}

  TCustomFormClass = class of TCustomForm;

  TCustomSheetForm = class;

  TSheetFormEvent = procedure (Sender: TObject;
    SheetForm: TCustomSheetForm) of object;

  TSheetFormGetControlEvent = procedure (Sender: TObject;
    SheetForm: TCustomSheetForm; var Control: TWinControl) of object;

{ TFormLoader }
  TFormLoader = class(TComponent)
  private
    FActiveForm: TCustomSheetForm;
    FActiveSheet: TWinControl;
    FActiveSpeedBar: TWinControl;
    FActiveMenu: TMainMenu;
    FForms: TList;
    FMenu: TMainMenu;
    FSpeedBar: TWinControl;
    FWorkplace: TWinControl;
    FOnActivate, FOnDeactivate, FOnInsert, FOnRemove: TSheetFormEvent;
    FOnGetSpeedBar, FOnGetWorkplace: TSheetFormGetControlEvent;
    function GetForm(Index: Integer): TCustomSheetForm;
    function GetFormCount: Integer;
    procedure LoadForm(AForm: TCustomSheetForm);
    procedure SetActiveForm(Value: TCustomSheetForm);
    procedure SetActiveMenu(Value: TMainMenu);
    procedure SetActiveSheet(Value: TWinControl);
    procedure SetActiveSpeedBar(Value: TWinControl);
    procedure SetMenu(Value: TMainMenu);
    procedure SetWorkplace(Value: TWinControl);
    procedure SetSpeedBar(Value: TWinControl);
  protected
    procedure DoActivate(SheetForm: TCustomSheetForm); dynamic;
    procedure DoDeactivate(SheetForm: TCustomSheetForm); dynamic;
    procedure DoInsert(SheetForm: TCustomSheetForm); dynamic;
    procedure DoRemove(SheetForm: TCustomSheetForm); dynamic;
    function GetSpeedBar(SheetForm: TCustomSheetForm): TWinControl; dynamic;
    function GetWorkplace(SheetForm: TCustomSheetForm): TWinControl; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ActiveMenu: TMainMenu read FActiveMenu write SetActiveMenu;
    property ActiveSheet: TWinControl read FActiveSheet write SetActiveSheet;
    property ActiveSpeedBar: TWinControl read FActiveSpeedBar write SetActiveSpeedBar;
  public
    destructor Destroy; override;
    procedure AddForm(AForm: TCustomSheetForm; Activate: Boolean);
    function FindNextForm(AForm: TCustomSheetForm; GoForward: Boolean): TCustomSheetForm;
    function IndexOf(AForm: TCustomSheetForm): Integer;
    procedure InsertForm(Index: Integer; AForm: TCustomSheetForm; Activate: Boolean);
    procedure RemoveForm(AForm: TCustomSheetForm);
    property ActiveForm: TCustomSheetForm read FActiveForm write SetActiveForm;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TCustomSheetForm read GetForm;
  published
    property Menu: TMainMenu read FMenu write SetMenu;
    property SpeedBar: TWinControl read FSpeedBar write SetSpeedBar;
    property Workplace: TWinControl read FWorkplace write SetWorkplace;
    property OnActivate: TSheetFormEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TSheetFormEvent read FOnDeactivate write FOnDeactivate;
    property OnGetSpeedBar: TSheetFormGetControlEvent read FOnGetSpeedBar write FOnGetSpeedBar;
    property OnGetWorkplace: TSheetFormGetControlEvent read FOnGetWorkplace write FOnGetWorkplace;
    property OnInsert: TSheetFormEvent read FOnInsert write FOnInsert;
    property OnRemove: TSheetFormEvent read FOnRemove write FOnRemove;
  end;

{ TCustomSheetForm }
  TCustomSheetForm = class(TForm)
  private
    FFormLoader: TFormLoader;
    FSheet: TWinControl;
    FSpeedBar: TWinControl;
    function GetIndex: Integer;
    procedure SetFormLoader(Value: TFormLoader);
    procedure SetSheet(Value: TWinControl);
    procedure SetSpeedBar(Value: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    property Index: Integer read GetIndex;
    property FormLoader: TFormLoader read FFormLoader write SetFormLoader;
  published
    property Sheet: TWinControl read FSheet write SetSheet;
    property SpeedBar: TWinControl read FSpeedBar write SetSpeedBar;
  end;

{ TCustomHook }
  TCustomHook = class;
  TCustomHookClass = class of TCustomHook;

  TCustomHook = class(TComponent)
  private
    FHookedObject, FStreamedHookedObject: TComponent;
    FActive, FStreamedActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure InternalHook;
    procedure InternalUnHook;
  protected
    procedure HookObject; virtual;
    procedure UnHookObject; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function FindHook(Value: TComponent; HookClass: TCustomHookClass): TCustomHook;
    procedure SetHookedObject(Value: TComponent); virtual;
    class function GetHookList: TList;
    function IsObjectHooked(Value: TComponent): Boolean;
    property HookedObject: TComponent read FHookedObject write SetHookedObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hook;
    procedure UnHook;
    property Active: Boolean read FActive write SetActive;
  end;

{ TLabelEffects }
  TLabelStyle = (lsNormal, lsRaised, lsLowered);

  TLabelEffects = class(TPersistent)
  private
    FLabel: TCustomLabel;
    FAngle: Integer;
    FPen: TPen;
    FShadowAngle: Integer;
    FShadowDepth: Integer;
    FShadowColor: TColor;
    FStyle: TLabelStyle;
    FGlyph: TBitmap;
    FOutline: Boolean;
    procedure GraphicsChanged(Sender: TObject);
    procedure SetAngle(Value: Integer);
    procedure SetPen(Value: TPen);
    procedure SetShadowAngle(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowDepth(Value: Integer);
    procedure SetStyle(Value: TLabelStyle);
    procedure SetGlyph(Value: TBitmap);
    procedure SetOutline(Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(ALabel: TCustomLabel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Angle: Integer read FAngle write SetAngle default 0;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Outline: Boolean read FOutline write SetOutline default False;
    property Pen: TPen read FPen write SetPen;
    property ShadowAngle: Integer read FShadowAngle write SetShadowAngle default 315;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property ShadowDepth: Integer read FShadowDepth write SetShadowDepth default 0;
    property Style: TLabelStyle read FStyle write SetStyle default lsNormal;
  end;

{$IFNDEF _D3_}
  TTextLayout = (tlTop, tlCenter, tlBottom);
{$ENDIF}

{ TvgCustomLabel }
  TvgCustomLabel = class(TLabel)
  private
    FEffects: TLabelEffects;
    FExecParams: TShellExecParams;
{$IFNDEF _D3_}
    FLayout: TTextLayout;
{$ENDIF}
    procedure SetEffects(Value: TLabelEffects);
    procedure SetExecParams(Value: TShellExecParams);
{$IFNDEF _D3_}
    procedure SetLayout(Value: TTextLayout);
{$ENDIF}
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property Effects: TLabelEffects read FEffects write SetEffects;
    property ExecParams: TShellExecParams read FExecParams write SetExecParams;
{$IFNDEF _D3_}
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: HINST;
  end;

{ TvgLabel }
  TvgLabel = class(TvgCustomLabel)
  published
    property Effects;
    property ExecParams;
    property Layout;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
{$ENDIF}
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TvgPanel }
  TvgPanel = class(TPanel)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BevelOuter default bvNone;
  end;

  TGetItemParamsEvent = procedure (Sender: TObject; Index: Integer; State: TOwnerDrawState;
    AFont: TFont; var Background: TColor; var ImageIndex, StateIndex, OverlayIndex: Integer) of object;

  TGetItemIndentEvent = procedure (Sender: TObject; Index: Integer; var Indent: Integer) of object;

{ TvgCustomListBox }
  TvgCustomListBox = class(TCustomListBox)
  { Horizontal scrolling is copied from RX library's TTextListBox component }
  private
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FImageWidth, FImageHeight: Integer;
    FImages: array[0..1] of TControlImageList;
    FImageLinks: array[0..1] of TChangeLink;
    FRowSelect: Boolean;
    FOnGetItemIndent: TGetItemIndentEvent;
    FOnGetItemParams: TGetItemParamsEvent;
    function GetImageWidth: Integer;
    function GetItemHeight(Index: Integer): Integer;
    function GetItemWidth(Index: Integer): Integer;
    procedure ImagesChanged(Sender: TObject);
    procedure ResetItemWidth;
    procedure ResetItemHeight;
    procedure SetItemWidth;
    procedure SetItemHeight;
    procedure SetImages(Index: Integer; Value: TControlImageList);
    procedure SetRowSelect(const Value: Boolean);
    procedure DoDrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetItemIndent(Index: Integer): Integer; virtual;
    procedure GetItemParams(Index: Integer; State: TOwnerDrawState; AFont: TFont;
      var Background: TColor; var ImageIndex, StateIndex, OverlayIndex: Integer); virtual;
    procedure DrawImage(Index: Integer; Rect: TRect;
      ImageIndex, StateIndex, OverlayIndex: Integer; State: TOwnerDrawState);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    property Images: TControlImageList index 0 read FImages[0] write SetImages;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default True;
    property StateImages: TControlImageList index 1 read FImages[1] write SetImages;
    property OnGetItemIndent: TGetItemIndentEvent read FOnGetItemIndent write FOnGetItemIndent;
    property OnGetItemParams: TGetItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TvgListBox }
  TvgListBox = class(TvgCustomListBox)
  published
    property Images;
    property RowSelect;
    property StateImages;
    property OnGetItemIndent;
    property OnGetItemParams;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TvgCustomComboBox }
  TvgCustomComboBox = class(TCustomComboBox)
  private
    FDropDownWidth: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FImageWidth, FImageHeight: Integer;
    FImages: array[0..1] of TControlImageList;
    FImageLinks: array[0..1] of TChangeLink;
    FRowSelect: Boolean;
{$IFNDEF _D5_}
    FDrawingComboEdit: Boolean;
{$ENDIF}
    FOnGetItemIndent: TGetItemIndentEvent;
    FOnGetItemParams: TGetItemParamsEvent;
    function GetImageWidth: Integer;
    function GetItemHeight(Index: Integer): Integer;
    function GetItemWidth(Index: Integer): Integer;
    procedure ImagesChanged(Sender: TObject);
    procedure ResetItemWidth;
    procedure ResetItemHeight;
    procedure SetDropDownWidth(Value: Integer);
    procedure SetItemHeight;
    procedure SetImages(Index: Integer; Value: TControlImageList);
    procedure SetRowSelect(const Value: Boolean);
    procedure DoDrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure UpdateDroppedWidth;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetItemIndent(Index: Integer): Integer; virtual;
    procedure GetItemParams(Index: Integer; State: TOwnerDrawState; AFont: TFont;
      var Background: TColor; var ImageIndex, StateIndex, OverlayIndex: Integer); virtual;
    procedure DrawImage(Index: Integer; Rect: TRect;
      ImageIndex, StateIndex, OverlayIndex: Integer; State: TOwnerDrawState);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 0;
    property Images: TControlImageList index 0 read FImages[0] write SetImages;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default True;
    property StateImages: TControlImageList index 1 read FImages[1] write SetImages;
    property OnGetItemIndent: TGetItemIndentEvent read FOnGetItemIndent write FOnGetItemIndent;
    property OnGetItemParams: TGetItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;
  end;

  TvgComboBox = class(TvgCustomComboBox)
  published
    property DropDownWidth;
    property Images;
    property RowSelect;
    property StateImages;
    property OnGetItemIndent;
    property OnGetItemParams;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Style; {Must be published before Items}
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

function CharToShortCut(C: Char): TShortCut;

var
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

implementation
uses Consts, ComCtl98, SysUtils, vgUtils, vgVCLUtl, CommCtrl, RichEdit;

type
  TControlHack = class(TControl);

  PNMCustomDraw = PNMCustomDrawInfo;

const
  DefOffset        = 2;
  Delimeter        = '|';

  hzHeight         = 16;
  hzWidth          = 24;

  vtHeight         = 24;
  vtWidth          = 16;

{$IFNDEF _D4_}
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
{$ENDIF}

function CharToShortCut(C: Char): TShortCut;
var
  Key: Word;
  Shift: TShiftState;
  VK: SHORT;
begin
  VK := VkKeyScan(C);
  Key := Lo(VK);
  Shift := [];
  if Hi(VK) and 1 = 1 then Shift := Shift + [ssShift];
  if Hi(VK) and 2 = 2 then Shift := Shift + [ssCtrl];
  if Hi(VK) and 4 = 4 then Shift := Shift + [ssAlt];
  Result := ShortCut(Key, Shift);
end;

procedure DrawLine(Canvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
begin
  with Canvas do
  begin
    Pen.Color := C;
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

procedure DrawVert(Canvas: TCanvas; X1, Y1, Height: Integer;
  Left, FullTop, FullBottom: Boolean);
begin
  Canvas.Pen.Width := 1;
  if Left then
  begin
    DrawLine(Canvas, clBtnHighlight, X1, Y1, X1, Y1 + Height);
  end else begin
    DrawLine(Canvas, clBlack, X1, Y1, X1, Y1 + Height);
    DrawLine(Canvas, clBtnShadow, X1 - 1, Y1 + Integer(FullTop)* 1,
      X1 - 1, Y1 + Height - Integer(FullBottom) * 1);
  end;
end;

procedure DrawHorz(Canvas: TCanvas; X1, Y1, Width: Integer;
  Top, FullLeft, FullRight: Boolean);
begin
  Canvas.Pen.Width := 1;
  if Top then
  begin
    DrawLine(Canvas, clBtnHighlight, X1, Y1, X1 + Width, Y1);
  end else begin
    DrawLine(Canvas, clBlack, X1, Y1, X1 + Width, Y1);
    DrawLine(Canvas, clBtnShadow, X1 + Integer(FullLeft) * 1, Y1 - 1,
      X1 + Width - Integer(FullRight) * 1, Y1 - 1);
  end;
end;

{ TvgSplitter }

type
  THack = class(TWinControl)
  public
    property OnKeyDown;
  end;

{$IFNDEF _D3_}
function ValidParentForm(Control: TControl): TCustomForm;
begin
  Result := GetParentForm(Control);
  if not Assigned(Result) then Abort;
end;
{$ENDIF}

constructor TvgSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDisplayDragImage];
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FBeveled := True;
end;

procedure TvgSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
end;

procedure TvgSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TvgSplitter.ReleaseLineDC;
begin
  ReleaseDC(Parent.Handle, FLineDC);
end;

procedure TvgSplitter.Paint;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
end;

procedure TvgSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  function FindControl: TControl;
  var
    P: TPoint;
    I: Integer;
  begin
    Result := nil;
    P := Point(Left, Top);
    case Align of
      alLeft: Dec(P.X);
      alRight: Inc(P.X, Width);
      alTop: Dec(P.Y);
      alBottom: Inc(P.Y, Height);
    else
      Exit;
    end;
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Result := Parent.Controls[I];
      if Result.Visible and PtInRect(Result.BoundsRect, P) then Exit;
    end;
    Result := nil;
  end;

var
  I: Integer;
begin
  inherited;
  if (Button = mbLeft) then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - Integer(FMinSize);
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - Integer(FMinSize);
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := THack(FActiveControl).OnKeyDown;
          THack(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      DrawLine;
    end;
  end;
end;

procedure TvgSplitter.UpdateSize(X, Y: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    FSplit := X - FDownPos.X
  else
    FSplit := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + FSplit;
    alRight: S := FControl.Width - FSplit;
    alTop: S := FControl.Height + FSplit;
    alBottom: S := FControl.Height - FSplit;
  end;
  FNewSize := S;
  if S < Integer(FMinSize) then
    FNewSize := FMinSize
  else if S > FMaxSize then
    FNewSize := FMaxSize;
  if S <> FNewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - FNewSize else
      S := FNewSize - S;
    Inc(FSplit, S);
  end;
end;

procedure TvgSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    UpdateSize(X, Y);
    DrawLine;
  end;
end;

procedure TvgSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    StopSizing;
  end;
end;

procedure TvgSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TvgSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TvgSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      THack(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

type
  TEditSpeedButton = class(TSpeedButton)
  private
    FEdit: TCustomClickEdit;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

procedure TEditSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (csDesigning in FEdit.ComponentState) then Exit;
  inherited;
  if (FState = bsDown) and (FEdit.IsCombo) then
  begin
    Update;
    Click;
  end;
end;

procedure TEditSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FEdit.ButtonReleased;
end;

{ TCustomClickEdit }
constructor TCustomClickEdit.Create(AOwner: TComponent);
begin
  inherited;
  FBtnControl := TWinControl.Create(nil);
  FBtnControl.Align := alRight;
  FBtnControl.Cursor := crArrow;
  TControlHack(FBtnControl).Color := clBtnFace;
  FBtnControl.Parent := Self;

  FButton := TEditSpeedButton.Create(nil);
  TEditSpeedButton(FButton).FEdit := Self;
  FButton.OnClick := EditButtonClick;
  FButton.Align := alRight;
  FButton.Parent := FBtnControl;
  FButtonShortCut := scDefButtonShortCut;
  FCaret := True;
  SetButtonWidth(15);
end;

destructor TCustomClickEdit.Destroy;
begin
  FButton.Free;
  FBtnControl.Free;
  inherited;
end;

procedure TCustomClickEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
end;

procedure TCustomClickEdit.ButtonReleased;
begin
end;

procedure TCustomClickEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FButton.Enabled := FBtnControl.Enabled and Enabled;
end;

procedure TCustomClickEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TCustomClickEdit.CMExit(var Message: TCMExit);
begin
  SetFocused(False);
  DoExit;
end;

procedure TCustomClickEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCustomClickEdit.CreateWnd;
begin
  inherited;
  SetEditRect;
end;

procedure TCustomClickEdit.EditButtonClick(Sender: TObject);
begin
  ButtonClick;
end;

function TCustomClickEdit.GetButtonEnabled: Boolean;
begin
  Result := FBtnControl.Enabled;
end;

function TCustomClickEdit.GetButtonVisible: Boolean;
begin
  Result := FBtnControl.Visible;
end;

function TCustomClickEdit.GetButtonWidth: Integer;
begin
  Result := FBtnControl.Width - 1;
end;

function TCustomClickEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function TCustomClickEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

function TCustomClickEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

function TCustomClickEdit.IsCombo: Boolean;
begin
  Result := False;
end;

function TCustomClickEdit.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = gkCustom;
end;

procedure TCustomClickEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (ShortCut(Key, Shift) = FButtonShortCut) and ButtonVisible and ButtonEnabled then
  begin
    ButtonClick;
    Key := 0;
  end;
end;

procedure TCustomClickEdit.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
    GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
  inherited KeyPress(Key);
  if Key = Char(VK_RETURN) then Key := #0;
end;

procedure TCustomClickEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomClickEdit.SetButtonEnabled(Value: Boolean);
begin
  if csDesigning in ComponentState then
  begin
    FBtnControl.Enabled := Value;
    FButton.Enabled := Enabled and Value;
  end else
    FButton.Enabled := Value;
end;

procedure TCustomClickEdit.SetButtonWidth(Value: Integer);
begin
  FBtnControl.Width := Value + 1;
  FButton.SetBounds(1, 0, FBtnControl.Width - 1, Height);
  SetEditRect;
  Invalidate;
end;

procedure TCustomClickEdit.SetButtonVisible(Value: Boolean);
begin
  FBtnControl.Visible := Value;
  SetEditRect;
  Invalidate;
end;

procedure TCustomClickEdit.SetCaret(Value: Boolean);
begin
  if (FCaret <> Value) then
  begin
    FCaret := Value;
    if FFocused then
      if not Caret then HideCaret(Handle) else ShowCaret(Handle);
  end;
end;

procedure TCustomClickEdit.SetEditRect;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := Rect(0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  end;
end;

procedure TCustomClickEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TCustomClickEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FGlyphKind := gkCustom;
end;

procedure TCustomClickEdit.SetGlyphKind(Value: TGlyphKind);
begin
  if FGlyphKind <> Value then
  begin
    FGlyphKind := Value;
    case FGlyphKind of
      gkDropDown:
        begin
          FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
          NumGlyphs := 1;
          SetButtonWidth(GetSystemMetrics(SM_CXVSCROLL));
        end;
      end;
  end;
end;

procedure TCustomClickEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  case FGlyphKind of
    gkDropDown:
      FButton.NumGlyphs := 1
  else
    FButton.NumGlyphs := Value;
  end;
end;

procedure TCustomClickEdit.WMPaint(var Message: TWMPaint);
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  BtnWidth: Integer;
begin
  if ((FAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    inherited;
    Exit;
  end;
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FBtnControl.Visible then
    BtnWidth := FBtnControl.Width else
    BtnWidth := 0;
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      S := Text;
      if (csPaintCopy in ControlState) then
      begin
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case FAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - BtnWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - BtnWidth - TextWidth(S)) div 2;
      end;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TCustomClickEdit.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if not FCaret then HideCaret(Handle);
end;

procedure TCustomClickEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  SetEditRect;
end;

{$IFDEF _D3_}
function TCustomClickEdit.GetFlat: Boolean;
begin
  Result := FButton.Flat;
end;

procedure TCustomClickEdit.SetFlat(Value: Boolean);
begin
  FButton.Flat := Value;
end;
{$ENDIF}

{ TJustifyEdit }
constructor TJustifyEdit.Create(AOwner: TComponent);
begin
  inherited;
  ButtonVisible := False;
end;

{ TCustomPopupWindow }
type
  TCustomPopupWindow = class(TWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure TCustomPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_CHILD or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WndParent := GetDesktopWindow;
  end;
end;

{ TCustomComboBoxEdit }
constructor TCustomComboBoxEdit.Create(AOwner: TComponent);
begin
  inherited;
  ButtonWidth := GetSystemMetrics(SM_CXHTHUMB);
  GlyphKind := gkDropDown;
  FPopup := TCustomPopupWindow.Create(Self);
  FPopup.Parent := Self;
  CreatePopupControl(FPopupControl);
  FPopupControl.Parent := FPopup;
  FPopupControl.Align := alClient;
  FDropDownAlign := daLeft;
  FDropDownHeight := 100;
end;

destructor TCustomComboBoxEdit.Destroy;
begin
  CloseUp(False);
  FPopup.Free;
  inherited;
end;

procedure TCustomComboBoxEdit.ButtonClick;
begin
  DropDown;
end;

procedure TCustomComboBoxEdit.ButtonReleased;
begin
end;

procedure TCustomComboBoxEdit.CloseUp(Accept: Boolean);
begin
  if FDroppedDown then
  begin
    DoCloseUp(Accept);
    HidePopupControl;
    FDroppedDown := False;
    Invalidate;
  end;
end;

procedure TCustomComboBoxEdit.DoCloseUp(Accept: Boolean);
begin
end;

procedure TCustomComboBoxEdit.DoShow;
begin
end;

procedure TCustomComboBoxEdit.DropDown;
begin
  if not FDroppedDown then
  begin
    ShowPopupControl;
    FDroppedDown := True;
    DoShow;
  end
end;

function TCustomComboBoxEdit.IsCombo: Boolean;
begin
  Result := True;
end;

procedure TCustomComboBoxEdit.HidePopupControl;
begin
  SetWindowPos(FPopup.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  FPopup.Visible := False;
end;

procedure TCustomComboBoxEdit.ShowPopupControl;
var
  P: TPoint;
  H, W, Y: Integer;
begin
  H := FDropDownHeight;
  if FDropDownWidth = 0 then
    W := Width else
    W := FDropDownWidth;

  P := Parent.ClientToScreen(Point(Left, Top));
  Y := P.Y + Height;
  if Y + H > Screen.Height then
    Y := P.Y - H;
  case DropDownAlign of
    daLeft:
      begin
        if P.X + W > Screen.Width then
          Dec(P.X, W - Width);
      end;
    daRight:
      begin
        Dec(P.X, W - Width);
        if P.X < 0 then Inc(P.X, W - Width);
      end;
  end;
  if P.X < 0 then P.X := 0
  else if P.X + W > Screen.Width then
    P.X := Screen.Width - W;

  if CanFocus then SetFocus;

  FPopup.Visible := True;

  SetWindowPos(FPopup.Handle, HWND_TOPMOST, P.X, Y, W, H,
    SWP_SHOWWINDOW);
end;

procedure TCustomComboBoxEdit.SetDisplayEmpty(Value: string);
begin
  if (FDisplayEmpty <> Value) then
  begin
    FDisplayEmpty := Value;
    Invalidate;
  end;
end;

procedure TCustomComboBoxEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_CANCELMODE:
      with TCMCancelMode(Message) do
      begin
        if not (csDesigning in ComponentState) then
          if (Sender <> FPopupControl) then
            CloseUp(False);
        inherited;
      end;
    CM_COLORCHANGED:
    begin
      inherited;
      TControlHack(FPopup).Color := Self.Color;
    end;
    CM_FONTCHANGED:
    begin
      inherited;
      TControlHack(FPopup).Font := Self.Font;
    end;
    WM_CHAR:
      if DroppedDown then
      begin
        if Message.wParam = 13 then
          CloseUp(True)
        else if Message.wParam = 27 then
          CloseUp(False);
        inherited;
      end;
    WM_KEYDOWN, WM_KEYUP:
      begin
        with Message do
          Result := FPopupControl.Perform(Msg, WParam, LParam)
      end;
    WM_LBUTTONDOWN:
      begin
        if not (csDesigning in ComponentState) then
          if DroppedDown then CloseUp(False) else DropDown
        else inherited;
      end;
    WM_SETCURSOR:
      if not (csDesigning in ComponentState) then
      begin
        with TWMSetCursor(Message) do
        if not Caret then
        begin
          if Cursor = crDefault then
            Windows.SetCursor(Screen.Cursors[crArrow]) else
            Windows.SetCursor(Screen.Cursors[Cursor]);
          Result := 1;
        end;
      end else
        inherited;
      WM_KILLFOCUS:
      begin
        if not (csDesigning in ComponentState) then CloseUp(False);
        inherited;
      end;
  else
    inherited;
  end;
end;

type
  TTreeViewHack = class(TCustomTreeView);

{ TvgCustomTreeViewCombo }
constructor TvgCustomTreeViewCombo.Create(AOwner: TComponent);
begin
  inherited;
  Caret := False;
  ReadOnly := True;
  TTreeViewHack(TreeView).BorderStyle := bsNone;
end;

destructor TvgCustomTreeViewCombo.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TvgCustomTreeViewCombo.GetTreeView: TCustomTreeView;
begin
  Result := TCustomTreeView(PopupControl);
end;

procedure TvgCustomTreeViewCombo.CreatePopupControl(var Control: TWinControl);
begin
  Control := TCustomComboTreeView.Create(Self);
  TCustomComboTreeView(Control).FComboBox := Self;
end;

procedure TvgCustomTreeViewCombo.CreateWnd;
begin
  inherited;
  PopupControl.HandleNeeded;
end;

procedure TvgCustomTreeViewCombo.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      begin
        inherited;
        Invalidate;
        Update;
      end;
    EM_CANPASTE:
      begin
        Message.Result := 0;
      end;
  else
    inherited;
  end;
end;

procedure TvgCustomTreeViewCombo.WMPaint(var Message: TWMPaint);

const
  Flags: array[Boolean] of Integer = (0, ILD_FOCUS);

var
  Margins: TPoint;
  R, ImageRect: TRect;
  DC: HDC;
  PS: TPaintStruct;
  Node: TTreeNode;
  S: string;
begin
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Self.Font;
    with TControlCanvas(FCanvas) do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;

      ImageRect.Right := 0;

      Node := TTreeViewHack(TreeView).Selected;

      if Assigned(Node) then
        S := Node.Text else S := DisplayEmpty;

      if Focused then
      begin
        FCanvas.Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end else begin
        FCanvas.Brush.Color := Self.Color;
      end;

      if Assigned(TTreeViewHack(TreeView).Images) and Assigned(Node) then
      begin
        ImageRect := R;
        ImageRect.Left := ImageRect.Left + 2;
        ImageRect.Right := ImageRect.Left + TTreeViewHack(TreeView).Images.Width + 2;
        R.Left := ImageRect.Right;
        ImageList_DrawEx(TTreeViewHack(TreeView).Images.Handle, Node.SelectedIndex, Handle,
          ImageRect.Left, ImageRect.Top, 0, 0,
          CLR_NONE, GetRGBColor(clHighlight), Flags[Focused]);
      end;

      Margins := GetTextMargins;
      InflateRect(R, -1, -1);
      FillRect(R);
      R.Left := ImageRect.Right + Margins.X;
      R.Top := Margins.Y;
      DrawText(Handle, PChar(S), -1, R, DT_EXPANDTABS or Alignments[Alignment]);
      if Focused then DrawFocusRect(R);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

{ TreeView properties }
function TvgCustomTreeViewCombo.GetTVShowButtons: Boolean;
begin
  Result := TTreeViewHack(TreeView).ShowButtons;
end;

procedure TvgCustomTreeViewCombo.SetTVShowButtons(Value: Boolean);
begin
  TTreeViewHack(TreeView).ShowButtons := Value;
end;

function TvgCustomTreeViewCombo.GetTVShowLines: Boolean;
begin
  Result := TTreeViewHack(TreeView).ShowLines;
end;

procedure TvgCustomTreeViewCombo.SetTVShowLines(Value: Boolean);
begin
  TTreeViewHack(TreeView).ShowLines := Value;
end;

function TvgCustomTreeViewCombo.GetTVShowRoot: Boolean;
begin
  Result := TTreeViewHack(TreeView).ShowRoot;
end;

procedure TvgCustomTreeViewCombo.SetTVShowRoot(Value: Boolean);
begin
  TTreeViewHack(TreeView).ShowRoot := Value;
end;

function TvgCustomTreeViewCombo.GetTVReadOnly: Boolean;
begin
  Result := TTreeViewHack(TreeView).ReadOnly;
end;

procedure TvgCustomTreeViewCombo.SetTVReadOnly(Value: Boolean);
begin
  TTreeViewHack(TreeView).ReadOnly := Value;
end;

function TvgCustomTreeViewCombo.GetTVIndent: Integer;
begin
  Result := TTreeViewHack(TreeView).Indent;
end;

procedure TvgCustomTreeViewCombo.SetTVIndent(Value: Integer);
begin
  TTreeViewHack(TreeView).Indent := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnGetItemParams: TTVGetItemParamsEvent;
begin
  Result := (TreeView as TvgCustomTreeView).OnGetItemParams;
end;

procedure TvgCustomTreeViewCombo.SetTVOnGetItemParams(Value: TTVGetItemParamsEvent);
begin
  (TreeView as TvgCustomTreeView).OnGetItemParams := Value;
end;

{$IFDEF _D4_}
function TvgCustomTreeViewCombo.GetTVAutoExpand: Boolean;
begin
  Result := TTreeViewHack(TreeView).AutoExpand;
end;

procedure TvgCustomTreeViewCombo.SetTVAutoExpand(Value: Boolean);
begin
  TTreeViewHack(TreeView).AutoExpand := Value;
end;

function TvgCustomTreeViewCombo.GetTVHotTrack: Boolean;
begin
  Result := TTreeViewHack(TreeView).HotTrack;
end;

procedure TvgCustomTreeViewCombo.SetTVHotTrack(Value: Boolean);
begin
  TTreeViewHack(TreeView).HotTrack := Value;
end;

function TvgCustomTreeViewCombo.GetTVRowSelect: Boolean;
begin
  Result := TTreeViewHack(TreeView).RowSelect;
end;

procedure TvgCustomTreeViewCombo.SetTVRowSelect(Value: Boolean);
begin
  TTreeViewHack(TreeView).RowSelect := Value;
end;

function TvgCustomTreeViewCombo.GetTVToolTips: Boolean;
begin
  Result := TTreeViewHack(TreeView).ToolTips;
end;

procedure TvgCustomTreeViewCombo.SetTVToolTips(Value: Boolean);
begin
  TTreeViewHack(TreeView).ToolTips := Value;
end;
{$ENDIF}

function TvgCustomTreeViewCombo.GetTVOnClick: TNotifyEvent;
begin
  Result := TTreeViewHack(TreeView).OnClick;
end;

procedure TvgCustomTreeViewCombo.SetTVOnClick(Value: TNotifyEvent);
begin
  TTreeViewHack(TreeView).OnClick := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnDragDrop: TDragDropEvent;
begin
  Result := TTreeViewHack(TreeView).OnDragDrop;
end;

procedure TvgCustomTreeViewCombo.SetTVOnDragDrop(Value: TDragDropEvent);
begin
  TTreeViewHack(TreeView).OnDragDrop := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnDragOver: TDragOverEvent;
begin
  Result := TTreeViewHack(TreeView).OnDragOver;
end;

procedure TvgCustomTreeViewCombo.SetTVOnDragOver(Value: TDragOverEvent);
begin
  TTreeViewHack(TreeView).OnDragOver := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnStartDrag: TStartDragEvent;
begin
  Result := TTreeViewHack(TreeView).OnStartDrag;
end;

procedure TvgCustomTreeViewCombo.SetTVOnStartDrag(Value: TStartDragEvent);
begin
  TTreeViewHack(TreeView).OnStartDrag := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnEndDrag: TEndDragEvent;
begin
  Result := TTreeViewHack(TreeView).OnEndDrag;
end;

procedure TvgCustomTreeViewCombo.SetTVOnEndDrag(Value: TEndDragEvent);
begin
  TTreeViewHack(TreeView).OnEndDrag := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnMouseDown: TMouseEvent;
begin
  Result := TTreeViewHack(TreeView).OnMouseDown;
end;

procedure TvgCustomTreeViewCombo.SetTVOnMouseDown(Value: TMouseEvent);
begin
  TTreeViewHack(TreeView).OnMouseDown := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnMouseMove: TMouseMoveEvent;
begin
  Result := TTreeViewHack(TreeView).OnMouseMove;
end;

procedure TvgCustomTreeViewCombo.SetTVOnMouseMove(Value: TMouseMoveEvent);
begin
  TTreeViewHack(TreeView).OnMouseMove := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnMouseUp: TMouseEvent;
begin
  Result := TTreeViewHack(TreeView).OnMouseUp;
end;

procedure TvgCustomTreeViewCombo.SetTVOnMouseUp(Value: TMouseEvent);
begin
  TTreeViewHack(TreeView).OnMouseUp := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnDblClick: TNotifyEvent;
begin
  Result := TTreeViewHack(TreeView).OnDblClick;
end;

procedure TvgCustomTreeViewCombo.SetTVOnDblClick(Value: TNotifyEvent);
begin
  TTreeViewHack(TreeView).OnDblClick := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnKeyDown: TKeyEvent;
begin
  Result := TTreeViewHack(TreeView).OnKeyDown;
end;

procedure TvgCustomTreeViewCombo.SetTVOnKeyDown(Value: TKeyEvent);
begin
  TTreeViewHack(TreeView).OnKeyDown := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnKeyPress: TKeyPressEvent;
begin
  Result := TTreeViewHack(TreeView).OnKeyPress;
end;

procedure TvgCustomTreeViewCombo.SetTVOnKeyPress(Value: TKeyPressEvent);
begin
  TTreeViewHack(TreeView).OnKeyPress := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnKeyUp: TKeyEvent;
begin
  Result := TTreeViewHack(TreeView).OnKeyUp;
end;

procedure TvgCustomTreeViewCombo.SetTVOnKeyUp(Value: TKeyEvent);
begin
  TTreeViewHack(TreeView).OnKeyUp := Value;
end;

{ TreeView events }
function TvgCustomTreeViewCombo.GetTVOnEditing: TTVEditingEvent;
begin
  Result := TTreeViewHack(TreeView).OnEditing;
end;

procedure TvgCustomTreeViewCombo.SetTVOnEditing(Value: TTVEditingEvent);
begin
  TTreeViewHack(TreeView).OnEditing := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnEdited: TTVEditedEvent;
begin
  Result := TTreeViewHack(TreeView).OnEdited;
end;

procedure TvgCustomTreeViewCombo.SetTVOnEdited(Value: TTVEditedEvent);
begin
  TTreeViewHack(TreeView).OnEdited := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnExpanding: TTVExpandingEvent;
begin
  Result := TTreeViewHack(TreeView).OnExpanding;
end;

procedure TvgCustomTreeViewCombo.SetTVOnExpanding(Value: TTVExpandingEvent);
begin
  TTreeViewHack(TreeView).OnExpanding := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnExpanded: TTVExpandedEvent;
begin
  Result := TTreeViewHack(TreeView).OnExpanded;
end;

procedure TvgCustomTreeViewCombo.SetTVOnExpanded(Value: TTVExpandedEvent);
begin
  TTreeViewHack(TreeView).OnExpanded := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnCollapsing: TTVCollapsingEvent;
begin
  Result := TTreeViewHack(TreeView).OnCollapsing;
end;

procedure TvgCustomTreeViewCombo.SetTVOnCollapsing(Value: TTVCollapsingEvent);
begin
  TTreeViewHack(TreeView).OnCollapsing := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnCompare: TTVCompareEvent;
begin
  Result := TTreeViewHack(TreeView).OnCompare;
end;

procedure TvgCustomTreeViewCombo.SetTVOnCompare(Value: TTVCompareEvent);
begin
  TTreeViewHack(TreeView).OnCompare := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnCollapsed: TTVExpandedEvent;
begin
  Result := TTreeViewHack(TreeView).OnCollapsed;
end;

procedure TvgCustomTreeViewCombo.SetTVOnCollapsed(Value: TTVExpandedEvent);
begin
  TTreeViewHack(TreeView).OnCollapsed := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnChanging: TTVChangingEvent;
begin
  Result := TTreeViewHack(TreeView).OnChanging;
end;

procedure TvgCustomTreeViewCombo.SetTVOnChanging(Value: TTVChangingEvent);
begin
  TTreeViewHack(TreeView).OnChanging := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnChange: TTVChangedEvent;
begin
  Result := TTreeViewHack(TreeView).OnChange;
end;

procedure TvgCustomTreeViewCombo.SetTVOnChange(Value: TTVChangedEvent);
begin
  TTreeViewHack(TreeView).OnChange := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnDeletion: TTVExpandedEvent;
begin
  Result := TTreeViewHack(TreeView).OnDeletion;
end;

procedure TvgCustomTreeViewCombo.SetTVOnDeletion(Value: TTVExpandedEvent);
begin
  TTreeViewHack(TreeView).OnDeletion := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnGetImageIndex: TTVExpandedEvent;
begin
  Result := TTreeViewHack(TreeView).OnGetImageIndex;
end;

procedure TvgCustomTreeViewCombo.SetTVOnGetImageIndex(Value: TTVExpandedEvent);
begin
  TTreeViewHack(TreeView).OnGetImageIndex := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnGetSelectedIndex: TTVExpandedEvent;
begin
  Result := TTreeViewHack(TreeView).OnGetSelectedIndex;
end;

procedure TvgCustomTreeViewCombo.SetTVOnGetSelectedIndex(Value: TTVExpandedEvent);
begin
  TTreeViewHack(TreeView).OnGetSelectedIndex := Value;
end;

{$IFDEF _D4_}
function TvgCustomTreeViewCombo.GetTVOnCustomDraw: TTVCustomDrawEvent;
begin
  Result := TTreeViewHack(TreeView).OnCustomDraw;
end;

procedure TvgCustomTreeViewCombo.SetTVOnCustomDraw(Value: TTVCustomDrawEvent);
begin
  TTreeViewHack(TreeView).OnCustomDraw := Value;
end;

function TvgCustomTreeViewCombo.GetTVOnCustomDrawItem: TTVCustomDrawItemEvent;
begin
  Result := TTreeViewHack(TreeView).OnCustomDrawItem;
end;

procedure TvgCustomTreeViewCombo.SetTVOnCustomDrawItem(Value: TTVCustomDrawItemEvent);
begin
  TTreeViewHack(TreeView).OnCustomDrawItem := Value;
end;
{$ENDIF}

function TvgCustomTreeViewCombo.GetTVItems: TTreeNodes;
begin
  Result := TTreeViewHack(TreeView).Items;
end;

procedure TvgCustomTreeViewCombo.SetTVItems(Value: TTreeNodes);
begin
  TTreeViewHack(TreeView).Items := Value;
end;

function TvgCustomTreeViewCombo.GetTVSelected: TTreeNode;
begin
  Result := TreeView.Selected;
end;

procedure TvgCustomTreeViewCombo.SetTVSelected(Value: TTreeNode);
begin
  TreeView.Selected := Value;
end;

function TvgCustomTreeViewCombo.GetTVImages: TControlImageList;
begin
  Result := TTreeViewHack(TreeView).Images;
end;

procedure TvgCustomTreeViewCombo.SetTVImages(Value: TControlImageList);
begin
  TTreeViewHack(TreeView).Images := Value;
end;

function TvgCustomTreeViewCombo.GetTVStateImages: TControlImageList;
begin
  Result := TTreeViewHack(TreeView).StateImages;
end;

procedure TvgCustomTreeViewCombo.SetTVStateImages(Value: TControlImageList);
begin
  TTreeViewHack(TreeView).StateImages := Value;
end;

{ TCustomComboTreeView }
procedure TCustomComboTreeView.Change(Node: TTreeNode);
begin
  inherited;
  UpdateComboBox;
end;

procedure TCustomComboTreeView.CreateWnd;
begin
  inherited;
  if not Assigned(Selected) then
    Selected := Items.GetFirstNode;
end;

procedure TCustomComboTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  Node: TTreeNode;
begin
  inherited;
  if (Button = mbLeft) then
  begin
    Node := GetNodeAt(X, Y);
    if Assigned(Node) then
    begin
      R := Node.DisplayRect(True);
      if PtInRect(R, Point(X, Y)) then
        FComboBox.CloseUp(True);
    end;
  end;
end;

procedure TCustomComboTreeView.GetItemParams(Node: TTreeNode; AFont: TFont; var Background: TColor;
  var State: TCustomDrawState);
begin
  if FComboBox.Focused then
    Include(State, cdsFocused);
  if Selected = Node then
  begin
    AFont.Color := clHighlightText;
    Background := clHighlight;
    Include(State, cdsSelected);
  end;
  inherited GetItemParams(Node, AFont, Background, State);
end;

procedure TCustomComboTreeView.UpdateComboBox;
var
  Node: TTreeNode;
  S: string;
begin
  Node := Selected;
  if Assigned(Node) then
    S := Node.Text else S := '';
  if S <> FComboBox.Text then
  begin
    FComboBox.Text := S;
    FComboBox.Update;
  end;
end;


{ TTitler }
constructor TTitler.Create(AOwner: TComponent);
begin
  inherited;
  Alignment := taCenter;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  Color := clBlack;
  Height := 50;
  Width := 50;
  Font.Color := clLime;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := DoTimer;
  FTitles := TStringList.Create;
  TStringList(FTitles).OnChange := TitlesChanged;
  FStep := 1;
  Interval := 50;
end;

destructor TTitler.Destroy;
begin
  FTitles.Free;
  FTimer.Free;
  inherited;
end;

procedure TTitler.Invalidate;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := GetClientRect;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TTitler.Reset;
begin
  FPosition := 0;
  Invalidate;
end;

function TTitler.GetInterval: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TTitler.SetInterval(Value: Integer);
begin
  FTimer.Interval := Value;
end;

procedure TTitler.SetStep(Value: Integer);
begin
  if (FStep = Value) or (Value < 1) then Exit;
  FStep := Value;
  Invalidate;
end;

procedure TTitler.SetTitles(Value: TStrings);
begin
  FTitles.Assign(Value);
end;

procedure TTitler.TitlesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTitler.Paint;
var
  CR: TRect;
  R: TRect;
  OldBkMode: Integer;
  FBmp: TBitmap;
begin
  CR := ClientRect;
  FBmp := TBitmap.Create;
  try
    FBmp.Width := CR.Right + 1;
    FBmp.Height := CR.Bottom + 1;
    with FBmp.Canvas do
    begin
      Brush.Color := Self.Color;
      FillRect(CR);
      OldBkMode := GetBkMode(Handle);
      if Assigned(FOnPaintBefore) then
      begin
        FOnPaintBefore(Self, FBmp.Canvas);
        SetBkMode(Handle, TRANSPARENT);
      end else
        SetBkMode(Handle, OPAQUE);
      Font := Self.Font;
      if FPosition > TextHeight('0') * FTitles.Count + Height then
        FPosition := 0;
      R := Rect(2, -FPosition + Height, Width - 3, CR.Bottom - 1);
      DrawText(Handle, PChar(FTitles.Text), -1, R, DT_EXPANDTABS or Alignments[Alignment]);
      SetBkMode(Handle, OldBkMode)
    end;
    if Assigned(FOnPaintAfter) then
      FOnPaintBefore(Self, FBmp.Canvas);
    Canvas.CopyRect(ClientRect, FBmp.Canvas, ClientRect);
  finally
    FBmp.Free;
  end;
end;

procedure TTitler.DoTimer(Sender: TObject);
begin
  Inc(FPosition, FStep);
  Invalidate;
end;

{$IFNDEF _D4_ }
{ TvgTabSheet }
procedure TvgTabSheet.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if TabVisible then (PageControl as TvgPageControl).UpdateTabImages;
  end;
end;

procedure TvgTabSheet.SetParent(AParent: TWinControl);
begin
  inherited;
  if Parent is TvgPageControl then TvgPageControl(Parent).UpdateTabImages;
end;

{$ENDIF}
{$IFNDEF _D3_}
{ TvgPageControl }
const
  TCS_SCROLLOPPOSITE    = $0001;
  TCS_BOTTOM            = $0002;
  TCS_RIGHT             = $0002;
  TCS_FORCEICONLEFT     = $0010;
  TCS_FORCELABELLEFT    = $0020;
  TCS_HOTTRACK          = $0040;
  TCS_VERTICAL          = $0080;
  TCS_TABS              = $0000;
  TCS_BUTTONS           = $0100;
  TCS_SINGLELINE        = $0000;
  TCS_MULTILINE         = $0200;
  TCS_RIGHTJUSTIFY      = $0000;
  TCS_FIXEDWIDTH        = $0400;
  TCS_RAGGEDRIGHT       = $0800;
  TCS_FOCUSONBUTTONDOWN = $1000;
  TCS_OWNERDRAWFIXED    = $2000;
  TCS_TOOLTIPS          = $4000;
  TCS_FOCUSNEVER        = $8000;
{$ENDIF}

{$IFNDEF _D4_}
const
  TCS_FLATBUTTONS       = $0008;
{$ENDIF}

{$IFNDEF _D4_}
var
  TabsRegistered: Boolean = False;

constructor TvgPageControl.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  if not TabsRegistered then
  begin
    RegisterClass(TvgTabSheet);
    TabsRegistered := True;
  end;
end;

destructor TvgPageControl.Destroy;
begin
  FCanvas.Free;
  FImageChangeLink.Free;
  inherited;
end;

procedure TvgPageControl.CreateParams(var Params: TCreateParams);
const
  AlignStyles: array[Boolean, TTabPosition] of DWORD =
    ((0, TCS_BOTTOM, TCS_VERTICAL, TCS_VERTICAL or TCS_RIGHT),
     (0, TCS_BOTTOM, TCS_VERTICAL or TCS_RIGHT, TCS_VERTICAL));
  TabStyles: array[TTabStyle] of DWORD = (TCS_TABS, TCS_BUTTONS,
    TCS_BUTTONS or TCS_FLATBUTTONS);
  RRStyles: array[Boolean] of DWORD = (0, TCS_RAGGEDRIGHT);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or AlignStyles[False, FTabPosition] or
      TabStyles[FStyle] or RRStyles[FRaggedRight];
    if FHotTrack and (not (csDesigning in ComponentState)) then
      Style := Style or TCS_HOTTRACK;
    if FOwnerDraw then Style := Style or TCS_OWNERDRAWFIXED;
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW) or CS_DBLCLKS;
  end;
end;

procedure TvgPageControl.CreateWnd;
begin
  inherited;
  if (Images <> nil) and Images.HandleAllocated then
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
end;

procedure TvgPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabIndex, Rect, Active) else
    FCanvas.FillRect(Rect);
end;

function TvgPageControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  if Pages[TabIndex] is TvgTabSheet then
    Result := (Pages[TabIndex] as TvgTabSheet).ImageIndex else
    Result := TabIndex;
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, TabIndex, Result);
end;

procedure TvgPageControl.ImageListChange(Sender: TObject);
begin
  Perform(TCM_SETIMAGELIST, 0, TControlImageList(Sender).Handle);
end;

procedure TvgPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then SetImages(nil);
end;

procedure TvgPageControl.UpdateTabImages;
var
  I: Integer;
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_IMAGE;
  for I := 0 to PageCount - 1 do
  begin
    TCItem.iImage := GetImageIndex(I);
    SendMessage(Handle, TCM_SETITEM, I, Longint(@TCItem));
  end;
  TabsChanged;
end;

procedure TvgPageControl.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    RecreateWnd;
  end;
end;

procedure TvgPageControl.SetImages(Value: TControlImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
  end else
    Perform(TCM_SETIMAGELIST, 0, 0);
end;

procedure TvgPageControl.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TvgPageControl.SetRaggedRight(Value: Boolean);
begin
  if FRaggedRight <> Value then
  begin
    FRaggedRight := Value;
    SetComCtlStyle(Self, TCS_RAGGEDRIGHT, Value);
  end;
end;

procedure TvgPageControl.SetTabPosition(Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    RecreateWnd;
  end;
end;

procedure TvgPageControl.SetStyle(Value: TTabStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RecreateWnd;
    Invalidate;
  end;
end;

procedure TvgPageControl.TabsChanged;
begin
  if HandleAllocated then
    SendMessage(Handle, WM_SIZE, SIZE_RESTORED, Word(Width) or Word(Height) shl 16);
  Realign;
end;

procedure TvgPageControl.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    SaveIndex := SaveDC(hDC);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    DrawTab(itemID, rcItem, itemState and ODS_SELECTED <> 0);
    FCanvas.Handle := 0;
    RestoreDC(hDC, SaveIndex);
  end;
  Message.Result := 1;
end;

procedure TvgPageControl.WMDrawItem(var Message: TWMDrawItem);
begin
  DefaultHandler(Message);
end;
{$ENDIF}

{ TvgCustomTreeView }
constructor TvgCustomTreeView.Create(AOwner: TComponent);
begin
  inherited;
{$IFNDEF _D4_}
  FStyles[3] := True;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  TControlCanvas(FCanvas).OnChange := CanvasChanged;
{$ENDIF}
end;

destructor TvgCustomTreeView.Destroy;
begin
{$IFNDEF _D4_}
  FreeObject(FCanvas);
{$ENDIF}
  inherited Destroy;
end;

procedure TvgCustomTreeView.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
end;

procedure TvgCustomTreeView.CNNotify(var Message: TWMNotify);
var
  AFont: TFont;
  ABackgrnd: TColor;
  State: TCustomDrawState;
  Node: TTreeNode;
  LF: TLogFont;
{$IFNDEF _D4_}
  R: TRect;
  DefaultDraw: Boolean;
  TmpItem: TTVItem;
  {$IFNDEF _D3_}
  MousePos: TPoint;
  {$ENDIF}
{$ENDIF}
begin
{$IFNDEF _D4_}
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end;
{$ENDIF}
  with Message do
    case NMHdr^.code of
      NM_CUSTOMDRAW:
{$IFDEF _D4_}
        if Assigned(OnCustomDrawItem) then
          inherited
        else
{$ELSE}
        if not Assigned(OnCustomDrawItem) then
{$ENDIF}
        begin
          with PNMCustomDrawInfo(Message.NMHdr)^ do
          begin
            case dwDrawStage of
              CDDS_PREPAINT:
                Message.Result := CDRF_NOTIFYITEMDRAW;
              CDDS_ITEMPREPAINT:
                begin
                  if not Assigned(FOnGetItemParams) then Exit;
                  State := TCustomDrawState(Word(uItemState));
                  AFont := TFont.Create;
                  try
                    Canvas.Handle := hdc;
                    Node := Items.GetNode(HTreeItem(dwItemSpec));
                    AFont.Assign(Canvas.Font);
                    AFont.Color := GetTextColor(hdc);
                    ABackgrnd := Canvas.Brush.Color;
                    AFont.OnChange := FontChanged;
                    FFontChanged := False;
                    GetItemParams(Node, AFont, ABackgrnd, State);
                    uItemState := Word(State);

                    with PNMTVCustomDraw(Message.NMHdr)^ do
                    begin
                      clrText := ColorToRGB(AFont.Color);
                      clrTextBk := ColorToRGB(ABackgrnd);
                    end;

                    if FFontChanged then
                    begin
                      Canvas.Handle := 0;
                      Canvas.Font := AFont;
                      GetObject(Canvas.Font.Handle, SizeOf(LF), @LF);
                      SelectObject(hdc, CreateFontIndirect(LF));
                    end;
                  finally
                    AFont.Free;
                  end;
                if FFontChanged then
                  Message.Result := CDRF_DODEFAULT or CDRF_NEWFONT else
                  Message.Result := CDRF_DODEFAULT;
                end;
            else
              Message.Result := 0;
            end;
          end;
        end
{$IFDEF _D4_}
        ;
{$ELSE}
        else
        with PNMCustomDraw(NMHdr)^ do
        begin
          Result := CDRF_DODEFAULT;
          if dwDrawStage = CDDS_PREPAINT then
          begin
            if IsCustomDrawn(dtControl, cdPrePaint) then
            begin
              FCanvas.Handle := hdc;
              FCanvas.Font := Font;
              FCanvas.Brush := Brush;
              R := ClientRect;
              DefaultDraw := CustomDraw(R, cdPrePaint);
              FCanvas.Handle := 0;
              if not DefaultDraw then
              begin
                Result := CDRF_SKIPDEFAULT;
                Exit;
              end;
            end;
            if IsCustomDrawn(dtControl, cdPostPaint) then
              Result := CDRF_NOTIFYPOSTPAINT;
            if IsCustomDrawn(dtItem, cdPrePaint) then
              Result := Result or CDRF_NOTIFYITEMDRAW else
              Result := Result or CDRF_DODEFAULT;
          end
          else if dwDrawStage = CDDS_ITEMPREPAINT then
          begin     
            ZeroMem(@TmpItem, SizeOf(TmpItem));
            TmpItem.hItem := HTREEITEM(dwItemSpec);
            Node := Items.GetNode(TmpItem.hItem);
            if Node <> nil then
            begin
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
              DefaultDraw := CustomDrawItem(Node,
                TCustomDrawState(Word(uItemState)), cdPrePaint);
              if not DefaultDraw then
                Result := Result or CDRF_SKIPDEFAULT
              else if FCanvasChanged then
              begin
                FCanvasChanged := False;
                FCanvas.Font.OnChange := nil;
                FCanvas.Brush.OnChange := nil;
                with PNMTVCustomDraw(NMHdr)^ do
                begin
                  clrText := ColorToRGB(FCanvas.Font.Color);
                  clrTextBk := ColorToRGB(FCanvas.Brush.Color);
                  SelectObject(hdc, FCanvas.Font.Handle);
                  Result := Result or CDRF_NEWFONT;
                end;
              end;
              FCanvas.Handle := 0;
              if IsCustomDrawn(dtItem, cdPostPaint) then
                Result := Result or CDRF_NOTIFYPOSTPAINT;
            end;
          end;
        end;
{$ENDIF}
{$IFNDEF _D3_}
      NM_RCLICK:
        begin
          if RightClickSelect then
          begin
            GetCursorPos(MousePos);
            with PointToSmallPoint(ScreenToClient(MousePos)) do
            begin
              FRClickNode := GetNodeAt(X, Y);
              Perform(WM_RBUTTONUP, 0, MakeLong(X, Y));
            end;
          end
          else FRClickNode := Pointer(1);
        end;
{$ENDIF}
    else
      inherited;
    end;
end;

procedure TvgCustomTreeView.CreateParams(var Params: TCreateParams);
{$IFNDEF _D4_}
const
  ToolTipStyles: array[Boolean] of DWORD = (TVS_NOTOOLTIPS, 0);
  AutoExpandStyles: array[Boolean] of DWORD = (0, TVS_SINGLEEXPAND);
  HotTrackStyles: array[Boolean] of DWORD = (0, TCS_HOTTRACK);
  RowSelectStyles: array[Boolean] of DWORD = (0, TVS_FULLROWSELECT);
{$ENDIF}
begin
  inherited;
{$IFNDEF _D4_}
  with Params do
  begin
    Style := Style or
      ToolTipStyles[ToolTips] or AutoExpandStyles[AutoExpand] or
      HotTrackStyles[HotTrack] or RowSelectStyles[RowSelect];
  end;
{$ENDIF}
end;

{$IFNDEF _D4_}
function TvgCustomTreeView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  { Tree view doesn't support erase notifications }
  if Stage = cdPrePaint then
  begin
    if Target = dtItem then
      Result := Assigned(FOnCustomDrawItem)
    else if Target = dtControl then
      Result := Assigned(FOnCustomDraw)
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TvgCustomTreeView.CanvasChanged(Sender: TObject);
begin
  FCanvasChanged := True;
end;

function TvgCustomTreeView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDraw) then FOnCustomDraw(Self, ARect, Result);
end;

function TvgCustomTreeView.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDrawItem) then FOnCustomDrawItem(Self, Node, State, Result);
end;
{$ENDIF}

procedure TvgCustomTreeView.GetItemParams(Node: TTreeNode; AFont: TFont;
  var Background: TColor; var State: TCustomDrawState);
begin
  if Assigned(FOnGetItemParams) then FOnGetItemParams(Self, Node, AFont, Background, State);
end;

procedure TvgCustomTreeView.SelectNode(Node: TTreeNode);
begin
  if Selected <> Node then
  begin
    Selected := Node;
    Change(Node);
    if Assigned(Node) then
    begin
      Node.Focused := True;
      Node.MakeVisible;
    end;
  end;
end;

procedure TvgCustomTreeView.WndProc(var Message: TMessage);
var
  Node: TTreeNode;
begin
  case Message.Msg of
    WM_RBUTTONDOWN:
      begin
        Node := GetNodeAt(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
        if Assigned(Node) then SelectNode(Node);
      end;
    WM_KILLFOCUS:
      Invalidate;
  end;
  inherited;
end;

{$IFNDEF _D4_}
procedure TvgCustomTreeView.SetStyle(Index: Integer; Value: Boolean);
begin
  if (FStyles[Index] <> Value) then
  begin
    FStyles[Index] := Value;
    RecreateWnd;
  end;
end;
{$ENDIF}

{ TvgCustomListView }
destructor TvgCustomListView.Destroy;
begin
{$IFNDEF _D4_}
  FreeObject(FCanvas);
{$ENDIF}
  inherited;
end;

procedure TvgCustomListView.CreateWnd;
begin
  inherited;
  SetBkColor(Color);
end;

procedure TvgCustomListView.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetBkColor(Color);
end;

procedure TvgCustomListView.CNNotify(var Message: TWMNotify);
var
  Item: TListItem;
  AFont: TFont;
  LF: TLogFont;
  ABackgrnd: TColor;
  State: TCustomDrawState;
begin
{$IFDEF _D4_}
  if Assigned(OnCustomDrawItem) or Assigned(OnCustomDrawSubItem) then
  begin
    inherited;
    Exit;
  end;
{$ENDIF}
{$IFNDEF _D4_}
  if not Assigned(FCanvas) then
  begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end;
{$ENDIF}
  case Message.NMHdr^.code of
    NM_CUSTOMDRAW:
      with PNMCustomDrawInfo(Message.NMHdr)^ do
      begin
        case dwDrawStage of
          CDDS_PREPAINT:
            Message.Result := CDRF_NOTIFYITEMDRAW;
          CDDS_ITEMPREPAINT:
            begin
              //Exit;
              Canvas.Handle := hdc;
              try
                AFont := TFont.Create;
                try
                  Item := Items[dwItemSpec];
                  State := TCustomDrawState(Word(uItemState));
                  uItemState := Word(State);

                  AFont.Assign(Canvas.Font);
                  AFont.OnChange := FontChanged;
                  ABackgrnd := Self.Color;
                  FFontChanged := False;
                  GetItemParams(Item, AFont, ABackgrnd, State);

                  with PNMLVCustomDraw(Message.NMHdr)^ do
                  begin
                    clrText := ColorToRGB(AFont.Color);
                    clrTextBk := ColorToRGB(ABackgrnd);
                  end;

                  if FFontChanged then
                  begin
                    Canvas.Handle := 0;
                    Canvas.Font := AFont;
                    GetObject(Canvas.Font.Handle, SizeOf(LF), @LF);
                    SelectObject(hdc, CreateFontIndirect(LF));
                  end;
                finally
                  AFont.Free;
                end;
              finally
                Canvas.Handle := 0;
              end;
              if FFontChanged then
                Message.Result := CDRF_DODEFAULT or CDRF_NEWFONT else
                Message.Result := CDRF_DODEFAULT;
            end;
        else
          Message.Result := 0;
        end;
      end;
  else
    inherited;
  end;
end;

procedure TvgCustomListView.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
end;

procedure TvgCustomListView.SetBkColor(Value: TColor);
begin
  ListView_SetBkColor(Handle, ColorToRGB(Value));
end;

procedure TvgCustomListView.GetItemParams(Item: TListItem; AFont: TFont;
  var Background: TColor; var State: TCustomDrawState);
begin
  if Assigned(FOnGetItemParams) then FOnGetItemParams(Self, Item, AFont, Background, State);
end;

procedure TvgCustomListView.SelectItem(Item: TListItem);
begin
  Selected := Item;
  if Assigned(Item) then Item.Focused := True;
end;

{ TvgNotebook }
constructor TvgNotebook.Create(AOwner: TComponent);
begin
  inherited;
  FBorderWidth := 2;
  FTabSetAlign := taNone;
end;

procedure TvgNotebook.Paint;
var
  R: TRect;
begin
  with Canvas do
  begin
    R := ClientRect;
    Brush.Color := Self.Color;
    FillRect(R);
    if BorderWidth = 0 then Exit;
    if FTabSetAlign <> taTop then
      DrawHorz(Canvas, 0, 0, Width, True,
        FTabSetAlign <> taLeft, FTabSetAlign <> taRight);
    if FTabSetAlign <> taBottom then
      DrawHorz(Canvas, 0, Height - 1, Width, False,
        FTabSetAlign <> taLeft, FTabSetAlign <> taRight);
    if FTabSetAlign <> taLeft then
      DrawVert(Canvas, 0, 0, Height, True,
        FTabSetAlign <> taTop, FTabSetAlign <> taBottom);
    if FTabSetAlign <> taRight then
      DrawVert(Canvas, Width - 1, 0, Height, False,
        FTabSetAlign <> taTop, FTabSetAlign <> taBottom);
 end;
end;

procedure TvgNotebook.AlignControls(AControl: TControl; var Rect: TRect);
begin
  InflateRect(Rect, -BorderWidth, -BorderWidth);
  Dec(Rect.Right); Dec(Rect.Bottom);
  inherited AlignControls(AControl, Rect);
end;

procedure TvgNotebook.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then begin
    FBorderWidth := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TvgNotebook.SetTabSetAlign(Value: TTabSetAlign);
begin
  if FTabSetAlign <> Value then
  begin
    FTabSetAlign := Value;
    Invalidate;
  end;
end;

{ TvgTabList }
function TvgTabList.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

procedure TvgTabList.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, S);
  if Assigned(FTabs) and ((Index <= FTabs.FTabIndex) or (FTabs.FTabIndex < 0)) then
    Inc(FTabs.FTabIndex);
end;

procedure TvgTabList.Delete(Index: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := FTabs.Tabindex;
  inherited Delete(Index);
  if OldIndex < Count then
    FTabs.FTabIndex := OldIndex else
    FTabs.FTabIndex := Count - 1;
  if OldIndex = Index then FTabs.Click;  { deleted selected tab }
end;

procedure TvgTabList.Changed;
begin
  FTabs.UpdateTabSizes;
  FTabs.Invalidate;
end;

procedure TvgTabList.Clear;
begin
  inherited Clear;
  FTabs.FTabIndex := -1;
  FTabs.SetFirstVisible(0);
end;

procedure TvgTabList.SetUpdateState(Updating: Boolean);
begin
  if Assigned(FTabs) and FTabs.HandleAllocated then
    SendMessage(FTabs.Handle, WM_SETREDRAW, Integer(not Updating), 0);
end;

{ TvgTabSet }
constructor TvgTabSet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csDoubleClicks, csOpaque];
  Width := 200;
  Height := 21;

  FUpDown := TUpDown.Create(nil);
  FUpDown.OnClick := UpDownClick;
  FUpDown.Parent := Self;

  FTabs := TvgTabList.Create;
  TvgTabList(FTabs).FTabs := Self;
  
  FMemBitmap := TBitmap.Create;
  FFocusFont := TFont.Create;
  FFocusFont.OnChange := FontChanged;
  FocusOffset := 2;
  FAlignment := taCenter;
  FAutoSizeX := True; FAutoSizeY := True;
  FDefaultFocusFont := True;
  FMargins[0] := 2; FMargins[1] := 2;
  FMargins[2] := 2; FMargins[3] := 2;
  FMargins[4] := 0;
  FTabsAlign := taBottom;
  FOffsetX := DefOffset; FOffsetY := 0;
  FRoundBorders := True;
  FTabIndex := -1;
  FTabsHeight := 20;
  FTabsWidth := 30;
  FStreamedTabIndex := -1;
  FTabStyle := tsNormal;
end;

destructor TvgTabSet.Destroy;
begin
  FTabs.Free;
  FFocusFont.Free;
  FMemBitmap.Free;
  FUpDown.Free;
  inherited;
end;

function TvgTabSet.CanChange(NewIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnChange) then FOnChange(Self, NewIndex, Result);
end;

procedure TvgTabSet.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  if FDefaultFocusFont then SetDefaultFocusFont(True);
  Invalidate;
end;

procedure TvgTabSet.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

procedure TvgTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateTabSizes;
  Invalidate;
end;

procedure TvgTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
  begin
    if IsAccel(Message.CharCode, FTabs[I]) then
    begin
      Message.Result := 1;
      if FTabIndex <> I then
        SetTabIndex(I);
      Exit;
    end;
  end;
  inherited;
end;

procedure TvgTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    Style := Style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TvgTabSet.DrawTabText(TabCanvas: TCanvas; R: TRect; Index: Integer;
  Selected: Boolean);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  TmpRect: TRect;
  I, J, H: Integer;
  Text, DrawText: String;
begin
  with TabCanvas do
  begin
    if Selected then Font := Self.FFocusFont else Font := Self.Font;
    Text := FTabs[Index];
    DrawText := '';
    I := 1; J := 1;
    while I <= Length(Text) do
    begin
      if DrawText <> '' then
      begin
        DrawText := DrawText + #13#10;
        Inc(J);
      end;
      DrawText := DrawText + ExtractDelimeted(Text, Delimeter, I);
    end;

    H := TextHeight('Wg') * J;

    TmpRect := R;
    with TmpRect do
    begin
      Top := Max(Top, ((Bottom + Top) - H) div 2 - 1);
      Bottom := Min(Bottom, Top + H);
    end;
    Windows.DrawText(Handle, PChar(DrawText), -1, TmpRect, (DT_EXPANDTABS or
      DT_VCENTER) or Alignments[FAlignment]);
  end;

  if (FTabStyle = tsOwnerDraw) and Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabCanvas, R, Index, Selected);
end;

procedure TvgTabSet.SelectNext(Direction: Boolean);
var
  NewIndex: Integer;
begin
  if Tabs.Count > 1 then
  begin
    NewIndex := TabIndex;
    if Direction then
      Inc(NewIndex)
    else Dec(NewIndex);
    if NewIndex = Tabs.Count then
      NewIndex := 0
    else if NewIndex < 0 then
      NewIndex := Tabs.Count - 1;
    SetTabIndex(NewIndex);
  end;
end;

procedure TvgTabSet.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TvgTabSet.Loaded;
begin
  inherited;
  SetTabIndex(FStreamedTabIndex);
end;

procedure TvgTabSet.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetAutoSizeX(Value: Boolean);
begin
  if FAutoSizeX <> Value then
  begin
    FAutoSizeX := Value;
    UpdateTabSizes;
  end;
end;

procedure TvgTabSet.SetAutoSizeY(Value: Boolean);
begin
  if FAutoSizeY <> Value then
  begin
    FAutoSizeY := Value;
    UpdateTabSizes;
  end;
end;

procedure TvgTabSet.SetFirstVisible(Value: Integer);
begin
  if (FFirstVisible <> Value) then
  begin
    if FTabs.Count = 0 then Value := 0;

    if (Value <= 0) and ((Value < FTabs.Count) or (FTabs.Count = 0)) then
    begin
      FFirstVisible := Value;
      Invalidate;
    end;
  end;
end;

procedure TvgTabSet.SetDefaultFocusFont(Value: Boolean);
begin
  if Value then
  begin
    SetFocusFont(Self.Font);
  end;
  FDefaultFocusFont := Value;
end;

procedure TvgTabSet.SetFocusFont(Value: TFont);
begin
  FFocusFont.Assign(Value);
  FDefaultFocusFont := False;
end;

procedure TvgTabSet.SetFocusOffset(Value: Integer);
begin
  if (FFocusOffset <> Value) and (Value >= 0) then
  begin
    FFocusOffset := Value;
    UpdateTabSizes;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetMargins(Index: Integer; Value: Integer);
begin
  if Value <> FMargins[Index] then
  begin
    FMargins[Index] := Value;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetRoundBorders(Value: Boolean);
begin
  if (FRoundBorders <> Value)  then
  begin
    FRoundBorders := Value;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetTabStyle(Value: TvgTabSetStyle);
begin
  if FTabStyle <> Value then
  begin
    FTabStyle := Value;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetTabIndex(Value: Integer);
begin
  if csLoading in ComponentState then
  begin
    FStreamedTabIndex := Value;
    Exit;
  end;
  if Value <> FTabIndex then
  begin
    if (Value < -1) or (Value >= Tabs.Count) then
      raise Exception.{$IFDEF _D3_}Create{$ELSE}CreateRes{$ENDIF}(SInvalidTabIndex);
    if CanChange(Value) then
    begin
      FTabIndex := Value;
      Repaint;
      Click;
    end;
  end;
end;

procedure TvgTabSet.SetTabList(Value: TStrings);
begin
  FTabs.Assign(Value);
  FTabIndex := -1;
  if FTabs.Count > 0 then TabIndex := 0 else Invalidate;
end;

procedure TvgTabSet.SetTabsAlign(Value: TTabSetAlign);
begin
  if Value = taNone then Value := taTop;
  if FTabsAlign <> Value then
  begin
    FTabsAlign := Value;
    case FTabsAlign of
      taLeft, taRight:
        begin FOffsetX := 0; FOffsetY := DefOffset; end;
      taTop, taBottom:
        begin FOffsetX := DefOffset; FOffsetY := 0; end;
    end;
    UpdateTabSizes;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetTabsHeight(Value: Integer);
begin
  if (FTabsHeight <> Value) and (Value > 0) then
  begin
    FTabsHeight := Value;
    UpdateTabSizes;
    Invalidate;
  end;
end;

procedure TvgTabSet.SetTabsWidth(Value: Integer);
begin
  if (FTabsWidth <> Value) and (Value > 0) then
  begin
    FTabsWidth := Value;
    UpdateTabSizes;
    Invalidate;
  end;
end;

function TvgTabSet.StoreFocusFont: Boolean;
begin
  Result := not FDefaultFocusFont;
end;

procedure TvgTabSet.UpdateTabSizes;
begin
  case FTabsAlign of
    taLeft, taRight:
    begin
      if FAutoSizeX then SetTabsWidth(Width - FocusOffset);
      FUpDown.Orientation := udVertical;
      FUpDown.SetBounds((Width - vtWidth - MarginStart) div 2, Height - vtHeight, vtWidth, vtHeight);
      FUpDown.Visible := FTabs.Count * FTabsHeight > Height;
      FUpDown.Max := Max(0, FTabs.Count - (Height - MarginStart) div FTabsHeight);
      FUpDown.Position := FUpDown.Max;
    end;
    taTop, taBottom:
    begin
      if FAutoSizeY then SetTabsHeight(Height - FocusOffset);
      FUpDown.Orientation := udHorizontal;
      FUpDown.SetBounds(Width - hzWidth, (Height - hzHeight - MarginStart) div 2 , hzWidth, hzHeight);
      FUpDown.Visible := FTabs.Count * FTabsWidth > Width;
      FUpDown.Max := Max(0, FTabs.Count - (Width - MarginStart) div FTabsWidth);
      FUpDown.Position := 0
    end;
  end;
end;

procedure TvgTabSet.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if (csDesigning in ComponentState) then Exit;

  if FTabsAlign in [taTop, taBottom] then
    case Button of
      btNext: Button := btPrev;
      btPrev: Button := btNext;
    end;

  case Button of
    btNext:
      begin
        if FFirstVisible < 0 then
          SetFirstVisible(FFirstVisible + 1);
      end;
    btPrev:
      begin
        if (- FFirstVisible) < FTabs.Count then
          SetFirstVisible(FFirstVisible - 1);
      end;
  end;
end;

procedure TvgTabSet.Paint;
var
  I: Integer;
begin
  inherited;
  FMemBitmap.Width := ClientWidth;
  FMemBitmap.Height := ClientHeight;
  with FMemBitmap.Canvas do
  begin
    Brush.Color := Self.Color;
    FillRect(Rect(0, 0, FMemBitmap.Width, FMemBitmap.Height));
    for I := 0 to FTabs.Count - 1 do
      if I <> FTabIndex then
        DrawTab(FMemBitmap.Canvas, ItemRect(I), I, False);
    case FTabsAlign of
      taLeft:
        DrawVert(FMemBitmap.Canvas, 1, 0, FMemBitmap.Height,
          False, False, False);
      taRight:
        DrawVert(FMemBitmap.Canvas, FMemBitmap.Width - 1, 0, FMemBitmap.Height,
          True, False, False);
      taTop:
        DrawHorz(FMemBitmap.Canvas, 0, 1, FMemBitmap.Width,
          False, False, False);
      taBottom:
        DrawHorz(FMemBitmap.Canvas, 0, FMemBitmap.Height - 1, FMemBitmap.Width,
          True, False, False);
    end;
    if FTabIndex >= 0 then
      DrawTab(FMemBitmap.Canvas, ItemRect(FTabIndex), FTabIndex, True)
  end;
  Canvas.Draw(0, 0, FMemBitmap);
end;

function TvgTabSet.ItemAtPos(Pos: TPoint): Integer;
  function TestItem(I: Integer): Boolean;
  begin
    Result := PtInRect(ItemRect(I), Pos)
  end;
var
  I: Integer;
begin
  Result := -1;
  if (Pos.Y < 0) or (Pos.Y > ClientHeight) then Exit;

  if (FTabIndex >= 0) and TestItem(FTabIndex) then
  begin
    Result := FTabIndex;
    Exit;
  end;

  for I := 0 to FTabs.Count - 1 do
    if (I <> FTabIndex) and TestItem(I) then
    begin
      Result := I;
      Break;
    end;
end;

function TvgTabSet.ItemRect(Item: Integer): TRect;
var
  X, Y: Integer;
begin
  if (Item >= 0) and (Item < FTabs.Count) then
  begin
    X := FOffsetX; Y := FOffsetY;
    case FTabsAlign of
      taRight:
        begin X := ClientWidth - FTabsWidth; end;
      taBottom:
        begin Y := ClientHeight - FTabsHeight; end;
    end;
    case FTabsAlign of
      taLeft, taRight:
        Inc(Y, (Item + FFirstVisible) * FTabsHeight + MarginStart);
      taTop, taBottom:
        Inc(X, (Item + FFirstVisible) * FTabsWidth + MarginStart);
    end;
    Result := Bounds(X, Y, FTabsWidth, FTabsHeight);
    if Item = FTabIndex then
    begin
      case FTabsAlign of
        taLeft, taRight:
          InflateRect(Result, 0, DefOffset);
        taTop, taBottom:
          InflateRect(Result, DefOffset, 0);
      end;
      case FTabsAlign of
        taLeft:
          Inc(Result.Right, FFocusOffset);
        taRight:
          Dec(Result.Left, FFocusOffset);
        taTop:
          Inc(Result.Bottom, FFocusOffset);
        taBottom:
          Dec(Result.Top, FFocusOffset);
      end;
    end;
  end else
    Result := Rect(0, 0, 0, 0);
end;

procedure TvgTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then
  begin
    I := ItemAtPos(Point(X, Y));
    if I >= 0 then SetTabIndex(I);
  end;
end;

procedure TvgTabSet.DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
      Selected: Boolean);
const
  RoundOffs = 2;

  function ROffs(Left, Top: Boolean): Integer;
  var
    DoOffs: Boolean;
  begin
    Result := 0; DoOffs := False;
    if not FRoundBorders then Exit;
    case FTabsAlign of
      taTop:
        DoOffs := not Top;
      taBottom:
        DoOffs := Top;
      taLeft:
        DoOffs := not Left;
      taRight:
        DoOffs := Left;
    end;
    if DoOffs then
    begin
      Result := RoundOffs;
      if (not Left) and (FTabsAlign <> taRight)
        or (not Top) and (FTabsAlign <> taBottom) then Inc(Result, 1);
    end;
  end;
var
  X, Y: Integer;
  Xhtl, Yhtl, Xhtr, Yhtr,
  Xvtr, Yvtr, Xvbr, YVbr,
  Xhbr, Yhbr, Xhbl, Yhbl,
  Xvbl, YVbl, Xvtl, YVtl: Integer;
  procedure DrawLT;
  begin
    DrawLine(TabCanvas, clBtnHighlight, Xvtl, Yvtl, Xhtl, Yhtl);
  end;
  procedure DrawRT;
  begin
    DrawLine(TabCanvas, clBlack, Xhtr, Yhtr, Xvtr, Yvtr);
  end;
  procedure DrawLB;
  begin
    DrawLine(TabCanvas, clBtnShadow, Xvbl, Yvbl, Xhbl, Yhbl);
  end;
  procedure DrawRB;
  begin
    DrawLine(TabCanvas, clBlack, Xhbr, Yhbr,
      Xvbr + Integer(FTabsAlign <> taRight), Yvbr - Integer(FTabsAlign <> taRight));
  end;
begin
  TabCanvas.Brush.Color := Self.Color;
  X := R.Left;
  Y := R.Top;
  TabCanvas.FillRect(R);

  { Right line and Bottom line must be 1 point shorter }
  if FTabsAlign <> taTop then
  begin
    Xhtl := X + ROffs(True, True);  Yhtl := Y;
    Xhtr := R.Right - ROffs(False, True);
    Yhtr := Y;
    DrawHorz(TabCanvas, Xhtl, Yhtl, Xhtr - Xhtl, True, False, False);
  end;
 if FTabsAlign <> taBottom then
  begin
    Xhbl := X + ROffs(True, False); Yhbl := R.Bottom - 1;
    Xhbr := R.Right - ROffs(False, False); Yhbr := R.Bottom - 1;
    DrawHorz(TabCanvas, Xhbl, Yhbl, Xhbr - Xhbl, False, False, False);
  end;
  if FTabsAlign <> taLeft then
  begin
    Xvtl := X; Yvtl := Y + ROffs(True, True);
    Xvbl := X; Yvbl := R.Bottom - ROffs(True, False);
    DrawVert(TabCanvas, Xvtl, Yvtl, Yvbl - Yvtl, True, False, False);
  end;
  if FTabsAlign <> taRight then
  begin
    Xvtr := R.Right - 1; Yvtr := Y + ROffs(False, True);
    Xvbr := R.Right - 1; Yvbr := R.Bottom - ROffs(False, False);
    DrawVert(TabCanvas, Xvtr, Yvtr, Yvbr - Yvtr, False, False, False);
  end;

  case FTabsAlign of
    taLeft:
      begin DrawRT; DrawRB; end;
    taRight:
      begin DrawLT; DrawLB; end;
    taTop:
      begin DrawLB; DrawRB; end;
    taBottom:
      begin DrawLT; DrawRT; end;
  end;

  Inc(R.Top, MarginTop + 1); Inc(R.Left, MarginLeft + 1);
  Dec(R.Right, MarginRight + 2); Dec(R.Bottom, MarginBottom + 2);

  DrawTabText(TabCanvas, R, Index, Selected);
end;

{ TClipboardShortCuts }
constructor TClipboardShortCuts.Create;
begin
  inherited Create;
  FActions := [caCopy, caCut, caDelete, caPaste, caSelectAll];
  FShortCuts[0] := scCopy;     FShortCuts[1] := scCut;
  FShortCuts[2] := scDelete;   FShortCuts[3] := scPaste;
  FShortCuts[4] := scSelectAll;
  FShortCuts2[0] := scCopy2;   FShortCuts2[1] := scCut2;
  FShortCuts2[2] := scDelete2; FShortCuts2[3] := scPaste2;
  FShortCuts2[4] := scSelectAll2;
end;

procedure TClipboardShortCuts.Assign(Source: TPersistent);
begin
  if (Source is TClipboardShortCuts) then
    with Source as TClipboardShortCuts do
    begin
      Self.FActions := FActions;
      Self.FShortCuts := FShortCuts;
      Self.FShortCuts2 := FShortCuts2;
    end
  else
    inherited;
end;

function TClipboardShortCuts.IsAction(Key: Word; Shift: TShiftState; Action: TClipboardAction): Boolean;
begin
  Result := IsShortCutAction(ShortCut(Key, Shift), Action);
end;

function TClipboardShortCuts.IsShortCutAction(ShortCut: TShortCut; Action: TClipboardAction): Boolean;
begin
  Result := (Action in Actions) and ((ShortCut = FShortCuts[Integer(Action)])
    or (ShortCut = FShortCuts2[Integer(Action)]));
end;

function TClipboardShortCuts.StoreActions: Boolean;
begin
  Result := FActions <> [caCopy, caCut, caDelete, caPaste, caSelectAll];
end;

{ TFormLoader }
destructor TFormLoader.Destroy;
begin
  while Assigned(FForms) do Forms[0].FormLoader := nil;
  inherited;
end;

procedure TFormLoader.DoActivate(SheetForm: TCustomSheetForm);
begin
  if not (csDestroying in ComponentState) and Assigned(FOnActivate) then FOnActivate(Self, SheetForm);
end;

procedure TFormLoader.DoDeactivate(SheetForm: TCustomSheetForm);
begin
  if not (csDestroying in ComponentState) and Assigned(FOnDeactivate) then FOnDeactivate(Self, SheetForm);
end;

procedure TFormLoader.DoInsert(SheetForm: TCustomSheetForm);
begin
  if Assigned(FOnInsert) then FOnInsert(Self, SheetForm);
end;

procedure TFormLoader.DoRemove(SheetForm: TCustomSheetForm);
begin
  if not (csDestroying in ComponentState) and Assigned(FOnRemove) then FOnRemove(Self, SheetForm);
end;

function TFormLoader.FindNextForm(AForm: TCustomSheetForm; GoForward: Boolean): TCustomSheetForm;
var
  I, J: Integer;
begin
  I := ListIndexOf(FForms, AForm);
  if I >= 0 then
  begin
    J := I;
    repeat
      if GoForward then
        J := (J + 1) mod ListCount(FForms) else
        J := (J - 1) mod ListCount(FForms);
      if J < 0 then J := ListCount(FForms) - 1;
      Result := ListItem(FForms, J);
      if I <> J then Exit;
    until J = I;
  end;
  Result := nil;
end;

function TFormLoader.GetForm(Index: Integer): TCustomSheetForm;
begin
  Result := ListItem(FForms, Index);
end;

function TFormLoader.GetFormCount: Integer;
begin
  Result := ListCount(FForms);
end;

function TFormLoader.GetSpeedBar(SheetForm: TCustomSheetForm): TWinControl;
begin
  Result := FSpeedBar;
  if Assigned(FOnGetSpeedBar) then FOnGetSpeedBar(Self, SheetForm, Result);
end;

function TFormLoader.GetWorkplace(SheetForm: TCustomSheetForm): TWinControl;
begin
  Result := FWorkPlace;
  if Assigned(FOnGetWorkplace) then FOnGetWorkplace(Self, SheetForm, Result);
end;

function TFormLoader.IndexOf(AForm: TCustomSheetForm): Integer;
begin
  Result := ListIndexOf(FForms, AForm);
end;

procedure TFormLoader.AddForm(AForm: TCustomSheetForm; Activate: Boolean);
begin
  InsertForm(FormCount, AForm, Activate)
end;

procedure TFormLoader.InsertForm(Index: Integer; AForm: TCustomSheetForm; Activate: Boolean);
var
  I: Integer;
begin
  I := ListIndexOf(FForms, AForm);
  if not Assigned(AForm.FormLoader) and (I < 0) then
  begin
    ListInsert(FForms, Index, AForm);
    AForm.FFormLoader := Self;
    DoInsert(AForm);
    LoadForm(AForm);
  end;
  if Activate and (AForm.FormLoader = Self) then SetActiveForm(AForm);
end;

procedure TFormLoader.LoadForm(AForm: TCustomSheetForm);
begin
  if Assigned(AForm.Sheet) then
  begin
    AForm.Sheet.Visible := False;
    AForm.Sheet.Parent := GetWorkplace(AForm);
  end;
  if Assigned(AForm.SpeedBar) then
  begin
    AForm.SpeedBar.Visible := False;
    AForm.SpeedBar.Parent := GetSpeedBar(AForm);
  end;
  if Assigned(AForm.OnShow) then AForm.OnShow(AForm);
end;

procedure TFormLoader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FMenu) then
      SetMenu(nil)
    else if (AComponent = FActiveMenu) then
      SetActiveMenu(nil)
    else if (AComponent = FWorkplace) then
      SetWorkplace(nil)
    else if (AComponent = FSpeedBar) then
      SetSpeedBar(nil)
    else if (AComponent = FActiveSpeedBar) then
      SetActiveSpeedBar(nil);
  end;
end;

procedure TFormLoader.RemoveForm(AForm: TCustomSheetForm);
var
  I: Integer;
  IsActive: Boolean;
  NextForm: TCustomSheetForm;
  Action: TCloseAction;
begin
  if AForm.FormLoader <> Self then Exit;
  I := ListIndexOf(FForms, AForm);
  if I >= 0 then
  begin
    Action := caHide;
    if Action in [caNone, caMinimize] then Exit;
    IsActive := (AForm = ActiveForm);
    if IsActive and not (csDestroying in ComponentState) then
      NextForm := FindNextForm(AForm, I = 0) else
      NextForm := nil;
    if IsActive then
      if Assigned(NextForm) then
        SetActiveForm(NextForm) else
        SetActiveForm(nil);
    DoRemove(AForm);
    ListDelete(FForms, I);
    AForm.FFormLoader := nil;
  end;
end;

procedure TFormLoader.SetActiveForm(Value: TCustomSheetForm);
begin
  if (FActiveForm <> Value) then
  begin
    if Assigned(FActiveForm) then
    begin
      DoDeactivate(FActiveForm);
      if Assigned(FActiveForm.OnDeactivate) then
        FActiveForm.OnDeactivate(FActiveForm);
    end;
    FActiveForm := Value;
    if Assigned(FActiveForm) then
    begin
      SetActiveMenu(FActiveForm.Menu);
      SetActiveSpeedBar(FActiveForm.SpeedBar);
      SetActiveSheet(FActiveForm.Sheet);
      if Assigned(FActiveForm.OnActivate) then
        FActiveForm.OnActivate(FActiveForm);
      DoActivate(FActiveForm);
    end else begin
      SetActiveSpeedBar(nil);
      SetActiveSheet(nil);
      SetActiveMenu(nil);
    end;
  end;
end;

procedure TFormLoader.SetActiveMenu(Value: TMainMenu);
begin
  if (FActiveMenu <> Value) then
  begin
    if Assigned(FActiveMenu) and Assigned(FMenu) then
      FMenu.UnMerge(FActiveMenu);
    FActiveMenu := Value;
    if Assigned(FActiveMenu) then
    begin
      FreeNotification(FActiveMenu);
      if Assigned(FMenu) then
        FMenu.Merge(FActiveMenu);
    end;
  end;
end;

procedure TFormLoader.SetActiveSheet(Value: TWinControl);
begin
  if (FActiveSheet <> Value) then
  begin
    if Assigned(FActiveSheet) and not (csDestroying in FActiveSheet.ComponentState) then
      FActiveSheet.Visible := False;
    FActiveSheet := Value;
    if Assigned(FActiveSheet) then
    begin
      FreeNotification(FActiveSheet);
      FActiveSheet.Visible := True;
      FActiveSheet.Parent := GetWorkplace(FActiveForm);
    end;
  end;
end;

procedure TFormLoader.SetActiveSpeedBar(Value: TWinControl);
begin
  if (FActiveSpeedBar <> Value) then
  begin
    if Assigned(FActiveSpeedBar) and not (csDestroying in FActiveSpeedBar.ComponentState) then
      FActiveSpeedBar.Visible := False;
    FActiveSpeedBar := Value;
    if Assigned(FActiveSpeedBar) then
    begin
      FreeNotification(FActiveSpeedBar);
      FActiveSpeedBar.Visible := True;
      FActiveSpeedBar.Parent := GetSpeedBar(ActiveForm);
    end;
  end;
end;

procedure TFormLoader.SetMenu(Value: TMainMenu);
begin
  if (FMenu <> Value) then
  begin
    if Assigned(FMenu) and Assigned(FActiveMenu) then FMenu.UnMerge(FActiveMenu);
    FMenu := Value;
    if Assigned(FMenu) then
    begin
      FreeNotification(Value);
      if Assigned(FActiveMenu) then FMenu.Merge(FActiveMenu);
    end;
  end;
end;

procedure TFormLoader.SetSpeedBar(Value: TWinControl);
var
  I: Integer;
  Form: TCustomSheetForm;
begin
  if (FSpeedBar <> Value) then
  begin
    for I := 0 to FormCount - 1 do
    begin
      Form := Forms[I];
      if not (csDestroying in Form.ComponentState) and Assigned(Form.SpeedBar) then
      begin
        Form.SpeedBar.Visible := Form = FActiveForm;
        Form.SpeedBar.Parent := Value;
      end;
    end;
    if Assigned(Value) then FreeNotification(Value);
    FSpeedBar := Value;
  end;
end;

procedure TFormLoader.SetWorkplace(Value: TWinControl);
var
  I: Integer;
  Form: TCustomSheetForm;
begin
  if (FWorkplace <> Value) then
  begin
    FWorkplace := Value;
    for I := 0 to FormCount - 1 do
    begin
      Form := Forms[I];
      if not (csDestroying in Form.ComponentState) and Assigned(Form.Sheet) then
      begin
        Form.Sheet.Visible := Form = FActiveForm;
        Form.Sheet.Parent := GetWorkPlace(Form);
      end;
    end;
    if Assigned(Value) then FreeNotification(Value);
  end;
end;

{ TCustomSheetForm }
destructor TCustomSheetForm.Destroy;
begin
  Destroying;
  SetFormLoader(nil);
  inherited;
end;

function TCustomSheetForm.GetIndex: Integer;
begin
  if Assigned(FFormLoader) then
    Result := FFormLoader.IndexOf(Self) else
    Result := -1;
end;

procedure TCustomSheetForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FSpeedBar) then
      SetSpeedBar(nil);
  end;
end;

procedure TCustomSheetForm.SetFormLoader(Value: TFormLoader);
begin
  if (FFormLoader <> Value) then
  begin
    if Assigned(FFormLoader) then FFormLoader.RemoveForm(Self);
    if Assigned(Value) then Value.InsertForm(Value.FormCount - 1, Self, True);
  end;
end;

procedure TCustomSheetForm.SetSheet(Value: TWinControl);
begin
  if (FSheet <> Value) then
  begin
    FSheet := Value;
    if Assigned(FSheet) then
    begin
      FreeNotification(FSheet);
      if (FSheet = FSpeedBar) then SetSpeedBar(nil);
    end;
    if Assigned(FFormLoader) and (FFormLoader.ActiveForm = Self) then
      FFormLoader.ActiveSheet := Value;
  end;
end;

procedure TCustomSheetForm.SetSpeedBar(Value: TWinControl);
begin
  if (FSpeedBar <> Value) then
  begin
    FSpeedBar := Value;
    if Assigned(FSpeedBar) then
    begin
      FreeNotification(FSpeedBar);
      if (FSheet = FSpeedBar) then SetSheet(nil);
    end;
    if Assigned(FFormLoader) and (FFormLoader.ActiveForm = Self) then
      FFormLoader.ActiveSpeedBar := Value;
  end;
end;

var
  HookList: TList = nil;

{ TCustomHook }
constructor TCustomHook.Create(AOwner: TComponent);
begin
  inherited;
  ListAdd(HookList, Self);
end;

destructor TCustomHook.Destroy;
begin
  SetHookedObject(nil);
  ListRemove(HookList, Self);
  inherited;
end;

procedure TCustomHook.Hook;
begin
  SetActive(True);
end;

procedure TCustomHook.UnHook;
begin
  SetActive(False);
end;

function TCustomHook.FindHook(Value: TComponent; HookClass: TCustomHookClass): TCustomHook;
var
  I: Integer;
begin
  for I := 0 to ListCount(HookList) - 1 do
  begin
    Result := HookList[I];
    if (Result is HookClass) and (Result.HookedObject = Value) then Exit;
  end;
  Result := nil;
end;

class function TCustomHook.GetHookList: TList;
begin
  Result := vgCtrls.HookList;
end;

function TCustomHook.IsObjectHooked(Value: TComponent): Boolean;
var
  Hook: TCustomHook;
  FClass: TCustomHookClass;
begin
  Result := Assigned(Value);
  if Result then
  begin
    FClass := TCustomHookClass(Self.ClassType);
    Hook := FindHook(Value, FClass);
    Result := Assigned(Hook) and (Hook <> Self);
  end;
end;

procedure TCustomHook.SetHookedObject(Value: TComponent);
var
  SaveActive: Boolean;
begin
  if (csLoading in ComponentState) then
    FStreamedHookedObject := Value
  else if FHookedObject <> Value then
  begin
    SaveActive := FActive;
    InternalUnHook;
    if Assigned(Value) then FreeNotification(Value);
    FHookedObject := Value;
    if SaveActive then InternalHook;
  end;
end;

procedure TCustomHook.SetActive(Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FStreamedActive := Value
  else if FActive <> Value then
    if Value then InternalHook else InternalUnHook;
end;

procedure TCustomHook.Loaded;
begin
  inherited;
  try
    SetHookedObject(FStreamedHookedObject);
    SetActive(FStreamedActive);
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TCustomHook.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FHookedObject) and (Operation = opRemove) then SetHookedObject(nil);
  inherited;
end;

procedure TCustomHook.InternalHook;
begin
  if not FActive and (FHookedObject <> nil) then HookObject;
  FActive := True;
end;

procedure TCustomHook.InternalUnHook;
begin
  if FActive and (FHookedObject <> nil) then UnHookObject;
  FActive := False;
end;

procedure TCustomHook.HookObject;
begin
end;

procedure TCustomHook.UnHookObject;
begin
end;

{ TLabelEffects }
constructor TLabelEffects.Create(ALabel: TCustomLabel);
begin
  inherited Create;
  FLabel := ALabel;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GraphicsChanged;
  FPen := TPen.Create;
  FPen.OnChange := GraphicsChanged;
  FShadowAngle := 315;
  FShadowColor := clBtnShadow;
end;

destructor TLabelEffects.Destroy;
begin
  FGlyph.Free;
  FPen.Free;
  inherited;
end;

procedure TLabelEffects.Assign(Source: TPersistent);
begin
  if Source is TLabelEffects then
    with TLabelEffects(Source) do
    begin
      Self.FAngle := FAngle;
      Self.FPen.Assign(FPen);
      Self.FShadowAngle := FShadowAngle;
      Self.FShadowDepth := FShadowDepth;
      Self.FShadowColor := FShadowColor;
      Self.FStyle := FStyle;
      Changed;
    end
  else
    inherited;
end;

procedure TLabelEffects.GraphicsChanged(Sender: TObject);
begin
  Changed;
end;

type
  TLabelHack = class(TCustomLabel);

procedure TLabelEffects.Changed;
begin
  TLabelHack(FLabel).AutoSize := False;
  if not (csLoading in FLabel.ComponentState) then
    FLabel.Invalidate;
end;

procedure TLabelEffects.SetAngle(Value: Integer);
begin
  Value := Value mod 360;
  if Value < 0 then Inc(Value, 360);

  if (FAngle <> Value) then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TLabelEffects.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TLabelEffects.SetOutline(Value: Boolean);
begin
  if (FOutline <> Value) then
  begin
    FOutline := Value;
    Changed;
  end;
end;

procedure TLabelEffects.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TLabelEffects.SetShadowAngle(Value: Integer);
begin
  Value := Value mod 360;
  if Value < 0 then Inc(Value, 360);

  if (FShadowAngle <> Value) then
  begin
    FShadowAngle := Value;
    Changed;
  end;
end;

procedure TLabelEffects.SetShadowColor(Value: TColor);
begin
  if (FShadowColor <> Value) then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TLabelEffects.SetShadowDepth(Value: Integer);
begin
  if (FShadowDepth <> Value) then
  begin
    FShadowDepth := Value;
    Changed;
  end;
end;

procedure TLabelEffects.SetStyle(Value: TLabelStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TvgCustomLabel }
constructor TvgCustomLabel.Create(AOwner: TComponent);
begin
  inherited;
  FEffects := TLabelEffects.Create(Self);
  FExecParams := TShellExecParams.Create;
end;

destructor TvgCustomLabel.Destroy;
begin
  FExecParams.Destroy;
  FEffects.Free;
  inherited;
end;

procedure TvgCustomLabel.SetEffects(Value: TLabelEffects);
begin
  FEffects.Assign(Value);
end;

procedure TvgCustomLabel.SetExecParams(Value: TShellExecParams);
begin
  FExecParams.Assign(Value);
end;

{$IFNDEF _D3_}
procedure TvgCustomLabel.SetLayout(Value: TTextLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TvgCustomLabel.WMSize(var Msg : TWMSize);
begin
  inherited
  Invalidate;
end;

procedure TvgCustomLabel.Paint;

var
  Rect: TRect;
  Bmp: TBitmap;
  TextW, TextH, TextPH, TextPW: Integer;
  LineH, TextAngle: Integer;
  Text: TStrings;
  TextSin, TextCos: Double;
  TM: TTextMetric;

  function GetTextWidth: Integer;
  var
    I: Integer;
  begin
    with Canvas do
    begin
      Result := TextWidth(Text[0]);
      for I := 1 to Text.Count - 1 do
        Result := Max(TextWidth(Text[I]), Result);
    end;
  end;

  function FontColor: TColor;
  begin
    if Enabled then
      Result := Self.Font.Color else Result := clBtnShadow;
  end;

  procedure DoTextOut(SX, SY: Integer);
  var
    I, X, Y, LineW, LineIH: Integer;
  begin
    with Bmp.Canvas do
    begin
      for I := 0 to Text.Count - 1 do
      begin
        X := 0; Y := 0;
        LineW := TextWidth(Text[I]);
        LineIH := I * LineH;
        
        case Alignment of
          taCenter:
            begin
              X := (Width - TextPW) div 2 + Round(LineIH * TextSin) + Round((TextW - LineW) * TextCos / 2);
              case Layout of
                tlTop:
                  Y := Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin) div 2;
                tlBottom:
                  Y := Height + TextPH + Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin) div 2;
              else
                Y := (Height + TextPH) div 2 + Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin / 2);
              end;
            end;

          taLeftJustify:
            begin
              if TextCos < 0 then
                X := - Round(TextW * TextCos) + Round(LineIH * TextSin) else
                X := Round(LineIH * TextSin);
              if TextSin < 0 then
                X := X - Round(TextH * TextSin);
              case Layout of
                tlTop:
                  Y := Round(LineIH * TextCos);
                tlBottom:
                  Y := Height + TextPH + Round(LineIH * TextCos);
              else
                Y := (Height + TextPH) div 2 + Round(LineIH * TextCos);
              end;
            end;

          taRightJustify:
            begin
              X := Width - TextPW + Round(LineIH * TextSin) + Round((TextW - LineW) * TextCos);
              if TextCos < 0 then
                X := X + Round(TextW * TextCos);
              if TextSin < 0 then
                X := X + Round(TextH * TextSin);
              case Layout of
                tlTop:
                  Y := Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin);
                tlBottom:
                  Y := Height + TextPH + Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin);
              else
                Y := (Height + TextPH) div 2 + Round(LineIH * TextCos) - Round((TextW - LineW) * TextSin);
              end;
            end;
        end;

        case Layout of
          tlTop:
            begin
              if TextSin > 0 then
                Y := Y + Round(TextW * TextSin);
              if TextCos < 0 then
                Y := Y - Round(TextH * TextCos);
            end;
          tlBottom:
            begin
              if TextSin > 0 then
                Y := Y - Round(TextW * TextSin);
              if TextCos < 0 then
                Y := Y + Round(TextH * TextCos);
            end;
        end;
        TextOut(SX + X, SY + Y, Text[I]);
      end;
    end;
  end;

  procedure DoDrawTextPath(X, Y: Integer);
  begin
    with Bmp.Canvas do
    begin
      if BeginPath(Handle) then
      try
        DoTextOut(X, Y);
      finally
        EndPath(Handle);
      end;
    end;
  end;

  procedure DoDrawText(X, Y: Integer; Shadow: Boolean);
  begin
    with FEffects, Bmp.Canvas do
    begin
      if Outline then
      begin
        DoDrawTextPath(X, Y);
        Pen.Assign(FEffects.Pen);
        if Shadow then
          Pen.Color := ShadowColor;
        StrokePath(Handle);
      end else
        DoTextOut(X, Y);
    end;
  end;

  procedure DoDrawShadow(X, Y: Integer);
  var
    DX, DY: Integer;
  begin
    with FEffects, Bmp.Canvas do
    begin
      DX :=   Round(ShadowDepth * Cos(ShadowAngle * PI / 180));
      DY := - Round(ShadowDepth * Sin(ShadowAngle * PI / 180));

      if (DX <> 0) or (DY <> 0) then
      begin
        Font.Color := ShadowColor;
        DoDrawText(X + DX, Y + DY, True);
      end;
    end;
  end;

  procedure DoDrawBorder(X, Y: Integer);
  var
    DrawStyle: TLabelStyle;
  begin
    with FEffects, Bmp.Canvas do
    begin
      if Enabled then
        DrawStyle := Style else
        DrawStyle := lsLowered;

      case DrawStyle of
        lsRaised:
          begin
            Font.Color := clBtnHighlight;
            DoDrawText(X - 1, Y - 1, False);
          end;
        lsLowered:
          begin
            Font.Color := clBtnHighlight;
            DoDrawText(X + 1, Y + 1, False);
          end;
      end;
      Font.Color := FontColor;
    end;
  end;

  procedure DoDrawGlyph(X, Y: Integer);
  begin
    with FEffects, Bmp.Canvas do
    begin
      DoDrawTextPath(X, Y);
      if SelectClipPath(Handle, RGN_COPY) then
      try
        X := 0;
        while X < Width do
        begin
          Y := 0;
          while Y < Height do
          begin
            Draw(X, Y, Glyph);
            Inc(Y, Glyph.Height);
          end;
          Inc(X, Glyph.Width);
        end;
      finally
        SelectClipRgn(Handle, 0);
      end;
    end;
  end;

var
  TextAngleRad: Double;
  LF: TLogFont;
begin
  Text := TStringList.Create;
  try
    Text.Text := Caption;
    if Text.Count = 0 then Text.Add(' ');

    Rect := ClientRect;

    with Canvas do
    begin
      Font := Self.Font;
      LineH := TextHeight('Wg');
      TextH := LineH * Text.Count;
      TextW := GetTextWidth;
      GetTextMetrics(Handle, TM);
      if (TM.tmPitchAndFamily and TMPF_TRUETYPE) <> 0 then
      begin
        TextAngle := FEffects.Angle;
        TextAngleRad := TextAngle * PI / 180;
        TextSin := Sin(TextAngleRad);
        TextCos := Cos(TextAngleRad);
        TextPW := Round(TextW * TextCos + TextH * TextSin);
        TextPH := Round(TextW * TextSin - TextH * TextCos);
      end else begin
        TextAngle := 0;
        TextSin := 0;
        TextCos := 1;
        TextPW := TextW;
        TextPH := - TextH;
      end;
      GetObject(Font.Handle, SizeOf(TLogFont), @LF);
    end;

    Bmp := TBitmap.Create;
    with FEffects, Bmp, Canvas do
    try
      Width := Self.Width;
      Height := Self.Height;

      Brush.Color := Self.Color;
      FillRect(Rect);
      Brush.Style := bsClear;

      LF.lfEscapement := TextAngle * 10;
      Font.Handle := CreateFontIndirect(LF);

      DoDrawShadow(0, 0);
      DoDrawBorder(0, 0);

      if Glyph.Empty then
        DoDrawText(0, 0, False) else
        DoDrawGlyph(0, 0);

      if Self.Transparent then
      begin
        Self.Canvas.Brush.Style := bsClear;
        Self.Canvas.BrushCopy(Rect, Bmp, Rect, Self.Color);
      end else
        Self.Canvas.CopyRect(Rect, Canvas, Rect);
    finally
      Bmp.Free;
    end;
  finally
    Text.Free;
  end;
end;

procedure TvgCustomLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled and (FExecParams.FileName <> '') then Execute;
end;

function TvgCustomLabel.Execute: HINST;
begin
  AppSetCursor(crAppStart);
  try
    Result := FExecParams.Execute;
  finally
    AppRestoreCursor;
  end;
end;

{ TvgPanel }
constructor TvgPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
end;

procedure TvgPanel.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and
     (BevelInner = bvNone) and (BevelOuter = bvNone) then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

{ TvgCustomListBox }
constructor TvgCustomListBox.Create(AOwner: TComponent);
begin
  inherited;
  FRowSelect := True;
end;

destructor TvgCustomListBox.Destroy;
begin
  SetImages(0, nil);
  SetImages(1, nil);
  inherited;
end;

procedure TvgCustomListBox.CreateWnd;
begin
  inherited;
  ResetItemHeight;
  ResetItemWidth;
  SendMessage(Handle, CB_SETHORIZONTALEXTENT, 300, 0);
end;

procedure TvgCustomListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if Style and (CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or CBS_OWNERDRAWFIXED;
end;

function TvgCustomListBox.GetImageWidth: Integer;
begin
  if FImageWidth > 0 then
    Result := FImageWidth + 2 else Result := 0;
end;

procedure TvgCustomListBox.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

function TvgCustomListBox.GetItemIndent(Index: Integer): Integer;
begin
  Result := 0;
  if Assigned(FOnGetItemIndent) then
    FOnGetItemIndent(Self, Index, Result);
end;

procedure TvgCustomListBox.GetItemParams(Index: Integer; State: TOwnerDrawState;
  AFont: TFont; var Background: TColor; var ImageIndex, StateIndex, OverlayIndex: Integer);
begin
  ImageIndex := Index;
  StateIndex := Index;
  OverlayIndex := -1;
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Index, State, AFont, Background, ImageIndex, StateIndex, OverlayIndex);
end;

procedure TvgCustomListBox.DrawImage(Index: Integer; Rect: TRect;
  ImageIndex, StateIndex, OverlayIndex: Integer; State: TOwnerDrawState);

const
  Flags: array[Boolean] of Integer = (0, ILD_FOCUS);

var
  L, H: Integer;
begin
  L := Rect.Left + 1;

  if Assigned(StateImages) then
  begin
    if StateIndex >= 0 then
    begin
      H := Rect.Top + (Rect.Bottom - Rect.Top - StateImages.Height) div 2;
      ImageList_DrawEx(StateImages.Handle, StateIndex, Canvas.Handle, L, H, 0, 0,
        CLR_NONE, GetRGBColor(clHighlight), 0);
    end;
    Inc(L, StateImages.Width + 1);
  end;

  if Assigned(Images) then
  begin
    if (ImageIndex >= 0) or (OverlayIndex >=0) then
    begin
      H := Rect.Top + (Rect.Bottom - Rect.Top - Images.Height) div 2;
      ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, L, H, 0, 0,
        CLR_NONE, GetRGBColor(clHighlight),
          Flags[not FRowSelect and (odFocused in State)] or (ILD_OVERLAYMASK and IndexToOverlayMask(OverlayIndex + 1)));
    end;
  end;
end;

procedure TvgCustomListBox.DoDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  S: string;
  Flags: Longint;
  DT: TDrawTextParams;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else begin
    if Index < Items.Count then
    begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      S := Items[Index];
      if (TabWidth > 0) then
      begin
        ZeroMem(@DT, SizeOf(DT));
        DT.cbSize := SizeOf(DT);
        DT.iTabLength := TabWidth;
        DrawTextEx(Canvas.Handle, PChar(S), Length(S), Rect, Flags or DT_EXPANDTABS or DT_TABSTOP, @DT);
      end else
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect, Flags);
    end;
  end;
end;

procedure TvgCustomListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R, TextR: TRect;
  SaveEvent: TDrawItemEvent;
  AImageWidth: Integer;
  Enable: Boolean;
  Font: TFont;
  Bkgnd: TColor;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
begin
  AImageWidth := GetImageWidth;

  SaveState := SaveDC(Canvas.Handle);
  try
    Font := TFont.Create;
    try
      Font.Assign(Canvas.Font);
      Bkgnd := Canvas.Brush.Color;
      if odSelected in State then
        Bkgnd := clHighlight else
        Bkgnd := Self.Color;
      GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
      Canvas.Font := Font;
      Canvas.Brush.Color := Bkgnd;
      Canvas.FillRect(Rect);

      TextR := Rect;

      if FRowSelect and not UseRightToLeftReading then
        Inc(TextR.Left, AImageWidth + GetItemIndent(Index));

      R := TextR;
      if not UseRightToLeftAlignment then
      begin
        R.Right := TextR.Left;
        R.Left := R.Right - AImageWidth;
      end else begin
        R.Left := TextR.Right;
        R.Right := R.Left + AImageWidth;
      end;

      DrawImage(Index, R, ImageIndex, StateIndex, OverlayIndex, State);

      if not FRowSelect then
        OffsetRect(TextR, - GetScrollPos(Handle, SB_HORZ), 0);

      Enable := Self.Enabled;
      if not Enable then
        Canvas.Font.Color := clGrayText;

      if UseRightToLeftAlignment then
        Dec(TextR.Right, AImageWidth);

      if (Style = lbStandard) and Assigned(OnDrawItem) then
      begin
        { Force lbStandard list to ignore OnDrawItem event. }
        SaveEvent := OnDrawItem;
        OnDrawItem := nil;
        try
          DoDrawItem(Index, TextR, State);
        finally
          OnDrawItem := SaveEvent;
        end;
      end else
        DoDrawItem(Index, TextR, State);
    finally
      Font.Free;
    end;
  finally
    RestoreDC(Canvas.Handle, SaveState);
  end;
end;

procedure TvgCustomListBox.CNDrawItem(var Message: TWMDrawItem);
var
  Indent: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    Indent := 0;
    if not FRowSelect or UseRightToLeftAlignment then
    begin
      if (Integer(itemID) >= 0) then
        Indent := GetItemIndent(itemID) else
        Indent := 0;
      if not UseRightToLeftAlignment then
        Inc(rcItem.Left, GetImageWidth + Indent) else
        Dec(rcItem.Right, GetImageWidth + Indent);
    end;
    if not FRowSelect then
    begin
      if not UseRightToLeftAlignment then
        rcItem.Right := rcItem.Left + GetItemWidth(itemID) + 2 - GetImageWidth - Indent else
        rcItem.Left := rcItem.Right - GetItemWidth(itemID) - 2 + GetImageWidth + Indent;
    end;
  end;
  inherited;
end;

procedure TvgCustomListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Index >= 0 then
    Height := GetItemHeight(Index);
  inherited;
end;

function TvgCustomListBox.GetItemHeight(Index: Integer): Integer;
var
  Font: TFont;
  Bkgnd: TColor;
  State: TOwnerDrawState;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
begin
  SaveState := SaveDC(Canvas.Handle);
  try
    Font := TFont.Create;
    try
      Font.Assign(Canvas.Font);
      Bkgnd := Self.Color;
      if Index = ItemIndex then
        State := [odSelected] else
        State := [];
      if Index >= 0 then
      begin
        GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
        Canvas.Font := Font;
      end;
      Result := Max(FImageHeight, Canvas.TextHeight('Wg'));
    finally
      Font.Free;
    end;
  finally
    RestoreDC(Canvas.Handle, SaveState);
  end;
end;

function TvgCustomListBox.GetItemWidth(Index: Integer): Integer;
var
  S: string;
  Font: TFont;
  Bkgnd: TColor;
  State: TOwnerDrawState;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
  R: TRect;
  DT: TDrawTextParams;
begin
  if Index < 0 then
  begin
    Result := 0;
    Exit;
  end;
  SaveState := SaveDC(Canvas.Handle);
  try
    Font := TFont.Create;
    try
      Font.Assign(Canvas.Font);
      Bkgnd := Self.Color;
      if Index = ItemIndex then
        State := [odSelected] else
        State := [];
      GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
      Canvas.Font := Font;
      S := Items[Index];
      ZeroMem(@R, SizeOf(R));
      if (TabWidth > 0) then
      begin
        ZeroMem(@DT, SizeOf(DT));
        DT.cbSize := SizeOf(DT);
        DT.iTabLength := TabWidth;
        ZeroMem(@R, SizeOf(R));
        DrawTextEx(Canvas.Handle, PChar(S), Length(S), R,
          DT_SINGLELINE or DT_NOPREFIX or DT_EXPANDTABS or DT_TABSTOP or DT_CALCRECT, @DT);
        Result := GetImageWidth + R.Right - R.Left;
      end else begin
        DrawText(Canvas.Handle, PChar(S), Length(S), R,
          DT_SINGLELINE or DT_NOPREFIX or DT_CALCRECT);
        Result := GetImageWidth + R.Right - R.Left;
      end;
      Inc(Result, GetItemIndent(Index));
    finally
      Font.Free;
    end;
  finally
    RestoreDC(Canvas.Handle, SaveState);
  end;
end;

procedure TvgCustomListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = Images) then
      Images := nil
    else if (AComponent = StateImages) then
      StateImages := nil;
  end;
end;

procedure TvgCustomListBox.ResetItemWidth;
var
  I: Integer;
begin
  if HandleAllocated then
  begin
    FMaxWidth := 0;
    for I := 0 to Items.Count - 1 do
      FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
    SetItemWidth;
  end;
end;

procedure TvgCustomListBox.ResetItemHeight;
var
  I: Integer;
begin
  if HandleAllocated and (Style <> lbOwnerDrawVariable) then
  begin
    FMaxHeight := Max(ItemHeight, FImageHeight);
    for I := 0 to Items.Count - 1 do
      FMaxHeight := Max(FMaxHeight, GetItemHeight(I));
    SetItemHeight;
  end;
end;

procedure TvgCustomListBox.SetItemHeight;
begin
  if (Style <> lbOwnerDrawVariable) then
    Perform(LB_SETITEMHEIGHT, 0, FMaxHeight);
end;

procedure TvgCustomListBox.SetImages(Index: Integer; Value: TControlImageList);
var
  I: Integer;
begin
  if (FImages[Index] <> Value) then
  begin
    FreeObject(FImageLinks[Index]);
    FImages[Index] := Value;
    if Assigned(FImages[Index]) then
    begin
      FreeNotification(FImages[Index]);
      FImageLinks[Index] := TChangeLink.Create;
      FImageLinks[Index].OnChange := ImagesChanged;
      FImages[Index].RegisterChanges(FImageLinks[Index]);
    end;

    if Assigned(FImages[0]) then
    begin
      FImageWidth := FImages[0].Width;
      FImageHeight := FImages[0].Height;
    end else begin
      FImageWidth := 0;
      FImageHeight := 0;
    end;
    if Assigned(FImages[1]) then
    begin
      FImageWidth := FImageWidth + FImages[1].Width + 1;
      FImageHeight := Max(FImageHeight, FImages[1].Height);
    end;
    ResetItemHeight;
    if HandleAllocated and (Style = lbOwnerDrawVariable) then
      for I := 0 to Items.Count - 1 do
        Perform(LB_SETITEMHEIGHT, I, GetItemHeight(I));
    ResetItemWidth;
    if HandleAllocated then
     Invalidate;
  end;
end;

procedure TvgCustomListBox.SetItemWidth;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth + GetImageWidth, 0);
end;

procedure TvgCustomListBox.WndProc(var Message: TMessage);
var
  ResetWidthNeeded, ResetHeightNeeded: Boolean;
begin
  case Message.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
        SetItemWidth;
        FMaxHeight := Max(FMaxHeight, GetItemHeight(Message.Result));
        SetItemHeight;
      end;
    LB_DELETESTRING:
      begin
        ResetHeightNeeded := GetItemHeight(Message.wParam) >= FMaxHeight;
        ResetWidthNeeded := GetItemWidth(Message.wParam) >= FMaxWidth;
        if ResetWidthNeeded then
          Perform(WM_HSCROLL, SB_TOP, 0);
        inherited;
        if ResetWidthNeeded then
          ResetItemWidth;
        if ResetHeightNeeded then
          ResetItemHeight;
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetItemWidth;
        Perform(WM_HSCROLL, SB_TOP, 0);
        FMaxHeight := FImageHeight;
        SetItemHeight;
        inherited;
      end;
    WM_SETFONT:
      begin
        inherited;
        ResetItemWidth;
        ResetItemHeight;
        Exit;
      end;
    WM_HSCROLL:
      begin
        inherited;
      end;
    else
      inherited WndProc(Message);
  end;
end;

procedure TvgCustomListBox.SetRowSelect(const Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if HandleAllocated then Invalidate;
  end;
end;

{ TvgCustomComboBox }
constructor TvgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FRowSelect := True;
end;

destructor TvgCustomComboBox.Destroy;
begin
  SetImages(0, nil);
  SetImages(1, nil);
  inherited;
end;

procedure TvgCustomComboBox.DropDown;
begin
  inherited DropDown;
end;

procedure TvgCustomComboBox.CreateWnd;
begin
  inherited;
  ResetItemHeight;
  ResetItemWidth;
  UpdateDroppedWidth;
end;

procedure TvgCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

function TvgCustomComboBox.GetImageWidth: Integer;
begin
  if FImageWidth > 0 then
    Result := FImageWidth + 2 else Result := 0;
end;

procedure TvgCustomComboBox.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

function TvgCustomComboBox.GetItemIndent(Index: Integer): Integer;
begin
  Result := 0;
  if Assigned(FOnGetItemIndent) then
    FOnGetItemIndent(Self, Index, Result);
end;

procedure TvgCustomComboBox.GetItemParams(Index: Integer; State: TOwnerDrawState;
  AFont: TFont; var Background: TColor; var ImageIndex, StateIndex, OverlayIndex: Integer);
begin
  ImageIndex := Index;
  StateIndex := Index;
  OverlayIndex := -1;
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Index, State, AFont, Background, ImageIndex, StateIndex, OverlayIndex);
end;

procedure TvgCustomComboBox.DrawImage(Index: Integer; Rect: TRect;
  ImageIndex, StateIndex, OverlayIndex: Integer; State: TOwnerDrawState);

const
  Flags: array[Boolean] of Integer = (0, ILD_FOCUS);

var
  L, H: Integer;
begin
  L := Rect.Left + 1;

  if Assigned(StateImages) then
  begin
    if StateIndex >= 0 then
    begin
      H := Rect.Top + (Rect.Bottom - Rect.Top - StateImages.Height) div 2;
      ImageList_DrawEx(StateImages.Handle, StateIndex, Canvas.Handle, L, H, 0, 0,
        CLR_NONE, GetRGBColor(clHighlight), 0);
    end;
    Inc(L, StateImages.Width + 1);
  end;

  if Assigned(Images) then
  begin
    if (ImageIndex >= 0) or (OverlayIndex >=0) then
    begin
      H := Rect.Top + (Rect.Bottom - Rect.Top - Images.Height) div 2;
      ImageList_DrawEx(Images.Handle, ImageIndex, Canvas.Handle, L, H, 0, 0,
        CLR_NONE, GetRGBColor(clHighlight),
          Flags[not FRowSelect and (odFocused in State)] or (ILD_OVERLAYMASK and IndexToOverlayMask(OverlayIndex + 1)));
    end;
  end;
end;

procedure TvgCustomComboBox.DoDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  S: string;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else begin
    if Index < Items.Count then
    begin
{$IFDEF _D4_}
      TControlCanvas(Canvas).UpdateTextFlags;
{$ENDIF}
      S := Items[Index];
      Canvas.TextOut(Rect.Left, Rect.Top, S);
    end;
  end;
end;

procedure TvgCustomComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R, TextR: TRect;
  SaveEvent: TDrawItemEvent;
  AImageWidth: Integer;
  Enable: Boolean;
  Font: TFont;
  Bkgnd: TColor;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
begin
  AImageWidth := GetImageWidth;

  SaveState := SaveDC(Canvas.Handle);
  try
    Font := TFont.Create;
    try
      Font.Assign(Canvas.Font);
      Bkgnd := Canvas.Brush.Color;
      if odSelected in State then
      begin
        Bkgnd := clHighlight;
        if Focused then Include(State, odFocused);
      end else
        Bkgnd := Self.Color;
      GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
      Canvas.Font := Font;
      Canvas.Brush.Color := Bkgnd;
      Canvas.FillRect(Rect);

      TextR := Rect;

      if FRowSelect and not UseRightToLeftReading then
      begin
        Inc(TextR.Left, AImageWidth);
{$IFDEF _D5_}
        if not (odComboBoxEdit in State) then
{$ELSE}
        if not FDrawingComboEdit then 
{$ENDIF}
          Inc(TextR.Left, GetItemIndent(Index));
      end;

      R := TextR;
      if not UseRightToLeftAlignment then
      begin
        R.Right := TextR.Left;
        R.Left := R.Right - AImageWidth;
      end else begin
        R.Left := TextR.Right;
        R.Right := R.Left + AImageWidth;
      end;

      DrawImage(Index, R, ImageIndex, StateIndex, OverlayIndex, State);

      Enable := Self.Enabled;
      if not Enable then
        Canvas.Font.Color := clGrayText;

      if UseRightToLeftAlignment then
        Dec(TextR.Right, AImageWidth);

      if not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) and Assigned(OnDrawItem) then
      begin
        { Force csStandard list to ignore OnDrawItem event. }
        SaveEvent := OnDrawItem;
        OnDrawItem := nil;
        try
          DoDrawItem(Index, TextR, State);
        finally
          OnDrawItem := SaveEvent;
        end;
      end else
        DoDrawItem(Index, TextR, State);
    finally
      Font.Free;
    end;
  finally
    RestoreDC(Canvas.Handle, SaveState);
  end;
end;

procedure TvgCustomComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  Indent, DrawIndent: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    Indent := 0;
{$IFNDEF _D5_}
    FDrawingComboEdit := (itemState and ODS_COMBOBOXEDIT = ODS_COMBOBOXEDIT);
{$ENDIF}
    if not FRowSelect or UseRightToLeftAlignment then
    begin
      if (Integer(itemID) >= 0) then
        Indent := GetItemIndent(itemID) else
        Indent := 0;

{$IFDEF _D5_}
      if (itemState and ODS_COMBOBOXEDIT = ODS_COMBOBOXEDIT) then
{$ELSE}
      if FDrawingComboEdit then
{$ENDIF}
        DrawIndent := 0 else
        DrawIndent := Indent;

      if not UseRightToLeftAlignment then
        Inc(rcItem.Left, GetImageWidth + DrawIndent) else
        Dec(rcItem.Right, GetImageWidth + DrawIndent);
    end;
    if not FRowSelect then
    begin
      if not UseRightToLeftAlignment then
        rcItem.Right := rcItem.Left + GetItemWidth(itemID) + 2 - GetImageWidth - Indent else
        rcItem.Left := rcItem.Right - GetItemWidth(itemID) - 2 + GetImageWidth + Indent;
    end;
  end;
  inherited;
end;

procedure TvgCustomComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Index >= 0 then
    Height := GetItemHeight(Index);
  inherited;
end;

function TvgCustomComboBox.GetItemHeight(Index: Integer): Integer;
var
  Font: TFont;
  Bkgnd: TColor;
  State: TOwnerDrawState;
  Canvas: TControlCanvas;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := Self;
    SaveState := SaveDC(Canvas.Handle);
    try
      Font := TFont.Create;
      try
        Font.Assign(Canvas.Font);
        Bkgnd := Self.Color;
        if Index = ItemIndex then
          State := [odSelected] else
          State := [];
        GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
        Result := Max(FImageHeight, Canvas.TextHeight('Wg'));
      finally
        Font.Free;
      end;
    finally
      RestoreDC(Canvas.Handle, SaveState);
    end;
  finally
    Canvas.Free;
  end;
end;

function TvgCustomComboBox.GetItemWidth(Index: Integer): Integer;
var
  S: string;
  Font: TFont;
  Bkgnd: TColor;
  State: TOwnerDrawState;
  Canvas: TControlCanvas;
  ImageIndex, StateIndex, OverlayIndex: Integer;
  SaveState: Integer;
  R: TRect;
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := Self;
    SaveState := SaveDC(Canvas.Handle);
    try
      Font := TFont.Create;
      try
        Font.Assign(Canvas.Font);
        Bkgnd := Self.Color;
        if Index = ItemIndex then
          State := [odSelected] else
          State := [];
        GetItemParams(Index, State, Font, Bkgnd, ImageIndex, StateIndex, OverlayIndex);
        Canvas.Font := Font;
        S := Items[Index];
        ZeroMem(@R, SizeOf(R));
        DrawText(Canvas.Handle, PChar(S), Length(S), R,
          DT_SINGLELINE or DT_NOPREFIX or DT_CALCRECT);
        Result := GetImageWidth + R.Right - R.Left;
        Inc(Result, GetItemIndent(Index));
      finally
        Font.Free;
      end;
    finally
      RestoreDC(Canvas.Handle, SaveState);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TvgCustomComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = Images) then
      Images := nil
    else if (AComponent = StateImages) then
      StateImages := nil;
  end;
end;

procedure TvgCustomComboBox.ResetItemWidth;
var
  I: Integer;
begin
  if HandleAllocated then
  begin
    FMaxWidth := 0;
    for I := 0 to Items.Count - 1 do
      FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  end;
end;

procedure TvgCustomComboBox.ResetItemHeight;
var
  I: Integer;
begin
  if HandleAllocated and (Style <> csOwnerDrawVariable) then
  begin
    FMaxHeight := Max(ItemHeight, FImageHeight);
    for I := 0 to Items.Count - 1 do
      FMaxHeight := Max(FMaxHeight, GetItemHeight(I));
    SetItemHeight;
  end;
end;

procedure TvgCustomComboBox.SetItemHeight;
begin
  if (Style <> csOwnerDrawVariable) then
    Perform(CB_SETITEMHEIGHT, 0, FMaxHeight);
end;

procedure TvgCustomComboBox.SetImages(Index: Integer; Value: TControlImageList);
var
  I: Integer;
begin
  if (FImages[Index] <> Value) then
  begin
    FreeObject(FImageLinks[Index]);
    FImages[Index] := Value;
    if Assigned(FImages[Index]) then
    begin
      FreeNotification(FImages[Index]);
      FImageLinks[Index] := TChangeLink.Create;
      FImageLinks[Index].OnChange := ImagesChanged;
      FImages[Index].RegisterChanges(FImageLinks[Index]);
    end;

    if Assigned(FImages[0]) then
    begin
      FImageWidth := FImages[0].Width;
      FImageHeight := FImages[0].Height;
    end else begin
      FImageWidth := 0;
      FImageHeight := 0;
    end;
    if Assigned(FImages[1]) then
    begin
      FImageWidth := FImageWidth + FImages[1].Width + 1;
      FImageHeight := Max(FImageHeight, FImages[1].Height);
    end;
    ResetItemHeight;
    if HandleAllocated and (Style = csOwnerDrawVariable) then
      for I := 0 to Items.Count - 1 do
        Perform(CB_SETITEMHEIGHT, I, GetItemHeight(I));
    ResetItemWidth;
    if HandleAllocated then
     Invalidate;
  end;
end;

procedure TvgCustomComboBox.WndProc(var Message: TMessage);
var
  ResetWidthNeeded, ResetHeightNeeded: Boolean;
begin
  case Message.Msg of
    CB_ADDSTRING, CB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        Exit;
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
        FMaxHeight := Max(FMaxHeight, GetItemHeight(Message.Result));
        SetItemHeight;
      end;
    CB_DELETESTRING:
      begin
        ResetHeightNeeded := GetItemHeight(Message.wParam) >= FMaxHeight;
        ResetWidthNeeded := GetItemWidth(Message.wParam) >= FMaxWidth;
        if ResetWidthNeeded then
          Perform(WM_HSCROLL, SB_TOP, 0);
        inherited;
        if ResetWidthNeeded then
          ResetItemWidth;
        if ResetHeightNeeded then
          ResetItemHeight;
      end;
    CB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        Perform(WM_HSCROLL, SB_TOP, 0);
        FMaxHeight := FImageHeight;
        SetItemHeight;
        inherited;
      end;
    WM_SETFONT:
      begin
        inherited;
        ResetItemWidth;
        ResetItemHeight;
      end;
    WM_HSCROLL:
      begin
        if UseRightToLeftReading then Invalidate;
        inherited;
      end;
    else
      inherited WndProc(Message);
  end;
end;

procedure TvgCustomComboBox.SetRowSelect(const Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if HandleAllocated then Invalidate;
  end;
end;

procedure TvgCustomComboBox.SetDropDownWidth(Value: Integer);
begin
  if (FDropDownWidth <> Value) and (Value >= 0) then
  begin
    FDropDownWidth := Value;
    UpdateDroppedWidth;
  end;
end;

procedure TvgCustomComboBox.UpdateDroppedWidth;
begin
  if HandleAllocated then
    SendMessage(Handle, CB_SETDROPPEDWIDTH, FDropDownWidth, 0);
end;

end.


