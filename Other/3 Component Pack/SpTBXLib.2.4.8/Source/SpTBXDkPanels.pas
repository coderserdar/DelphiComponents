unit SpTBXDkPanels;

{==============================================================================
Version 2.4.8

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - All the DockablePanels rules are marked with '[DockablePanel-Rule]'.
  - To handle the size constraints use GetMinMaxSize when the DP is floating,
    and ConstrainedResize when is Docked (explicitly check if it's docked).

Limitations:
  - DockablePanels can be docked only on MultiDocks.
  - MultiDocks doesn't have lateral splitters, you can solve this by
    adding a TSpTBXSplitter on the Form.

History:
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - Improved Toolbar Load/Save Position helpers to fix the
    locked toolbars restoring TB2K issue.

12 March 2010 - version 2.4.5
  - Fixed incorrect TSpTBXDockablePanel split resizing when
    the mouse was moved too quickly, thanks to Mattias
    Andersson for reporting this.
  - Fixed incorrect TSpTBXSplitter sizing when AlignWithMargin
    was set to true, thanks to Dany Marmur for reporting this.
  - Fixed TSpTBXDockablePanel bug, the adjacent splitter is
    misaligned when the DP is hidden at designtime, thanks to
    Irina for reporting this.

2 December 2009 - version 2.4.4
  - No changes.

13 September 2009 - version 2.4.3
  - Added OnCanResize event to TSpTBXDockablePanel.
  - Changed TSpTBXDockablePanel docking behavior, when the
    panel is docked and it's DockPos isn't specified it
    will be appended to the bottom of the MultiDock.
  - Fixed incorrect TSpTBXDockablePanel split resizing when
    FixedDockedSize was true, thanks to Gilles Arcas for
    reporting this.

8 May 2009 - version 2.4.2
  - Fixed incorrect TSpTBXDockablePanel floating border
    painting on Vista with Aero enabled, thanks to Mattias
    Andersson for reporting this.

15 March 2009 - version 2.4.1
  - Fixed TSpTBXSplitter bug, it wasn't correctly minimized
    when MinSize was 1, 2 or 3, thanks to Sertac Akyuz for
    fixing this.
  - Fixed TSpTBXSplitter bug, it wasn't correctly restored
    when SpTBIniLoadPositions was called.
  - Fixed incorrect TSpTBXDockablePanel behavior, when DockMode
    is dmCannotFloat the DP should be able to be re-docked,
    thanks to Ivan Petrovic for reporting this.
  - Fixed incorrect TSpTBXDockablePanel behavior, it wasn't
    correctly resized when using the embedded splitter, thanks
    to Gilles Arcas for reporting this.
  - Fixed incorrect TSpTBXDockablePanel painting,
    OnDrawCaptionPanel wasn't called when painting the NC
    area borders, thanks to Mikael Stalvik for reporting this.

17 January 2009 - version 2.4
  - Added FloatingClientWidth and FloatingClientHeight public
    properties to TSpTBXDockablePanel.
  - Added TaskPaneStyleResize property to TSpTBXDockablePanel,
    when this property is set to True the Minimize/Restore
    behavior will be the same as the Windows Task Pane (the
    DockablePanel is minimized from bottom to top).
  - Changed TSpTBXDockablePanel undocking behavior the panel
    will remember the previous floating size when it is
    undocked.
  - Changed TSpTBXDockablePanel docking behavior, when the
    panel is docked on an empty MultiDock it will use the
    DefaultDockedSize property to set its size.
    If DefaultDockedSize is 0 it will use the floating size.

26 September 2008 - version 2.3
  - Added DefaultDockedSize property to TSpTBXDockablePanel,
    this property is used to set the DockablePanel size when
    it's docked on an empty MultiDock.
  - Added ShowVerticalCaption property to TSpTBXDockablePanel,
    this property is used to rotate the caption panel vertically.
    When the DockablePanel is floating or docked on a vertical
    MultiDock the caption will be horizontal regardless of the
    value of ShowVerticalCaption.
  - Fixed TSpTBXDockablePanel bug, anchored children were not
    correctly resized when the Form was loaded, thanks to
    Alex Neznanov for reporting this.
  - Fixed TSpTBXDockablePanel bug, hiding/restoring a DP
    misaligned the adjacent splitter, thanks to Alexander for
    reporting this (related to Delphi's zero size align bug).
  - Fixed TSpTBXDockablePanel bug, dragging a floating DP
    with CTRL key pressed should not dock the DP.

29 July 2008 - version 2.2
  - Fixed TSpTBXDockablePanel bug, an AV was raised when the
    DockablePanel was undocked when it was minimized by an
    adjacent splitter, thanks to Minoru Yoshida for reporting this.

26 June 2008 - version 2.1
  - Added AutoSplitterVisibility property to TSpTBXMultiDock,
    use this property to automatically hide the adjacent
    splitter when the MultiDock is empty.
  - Added OnWindowStateChanged event to TSpTBXDockablePanel,
    this event is fired when the DockablePanel gets minimized,
    maximized or restored.
  - Fixed TSpTBXDockablePanel bug, the floating panel was able to be
    dragged offscreen leaving no way to move it back, thanks to
    Minoru Yoshida for reporting this.
  - Fixed TSpTBXSplitter bug, incorrect alignment when the split
    control was minimized and the Form was resized, thanks to
    Den and Minoru Yoshida for reporting this.

3 May 2008 - version 2.0
  - Decoupled from TBX.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList, IniFiles,
  TB2Item, TB2Dock, TB2Toolbar,
  SpTBXSkins, SpTBXItem, SpTBXControls;

type
  TSpTBXCustomDockablePanel = class;
  TSpTBXCustomSplitter = class;

  TSpTBXDockStateRec = record
    DockedState: TWindowState;
    RestoreSize: Integer;
  end;

  TSpTBXDockPosition = (
    dpxLeft,     // dpLeft
    dpxTop,      // dpTop
    dpxRight,    // dpRight
    dpxBottom,   // dpBottom
    dpxClient    // dpRight
  );

  TSpTBXDPResizeType = (
    dprtManualResize,
    dprtMinimizeOrRestore,
    dprtMinimizeOrRestoreTaskPaneStyle,
    dprtSplitResize,
    dprtAppendResize
  );

  TSpTBXWindowStateChangedEvent = procedure(Sender: TObject; AWindowState: TWindowState) of object;

  { TSpTBXMultiDock }

  TSpTBXCustomMultiDock = class(TTBDock)
  private
    FAutoSplitterVisibility: Boolean;
    FLimitToOneRow: Boolean;
    FLastSplitter: TSpTBXCustomSplitter;
    FPosition: TSpTBXDockPosition;
    FReadingPositionData: Boolean;
    FUpdatingLateralSize: Boolean;
    FOnInsertRemoveBar: TTBInsertRemoveEvent;
    FOnRequestDock: TTBRequestDockEvent;
    procedure UpdateDPLateralSize(AWidth, AHeight: Integer);
    procedure SetPosition(const Value: TSpTBXDockPosition);
    procedure SetLimitToOneRow(const Value: Boolean);
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DoInsertRemoveBar(Sender: TObject; Inserting: Boolean; Bar: TTBCustomDockableWindow); virtual; // OnInsertRemoveBar is republished
    procedure DoRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean); virtual; // OnRequestDock is republished
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ValidateInsert(AComponent: TComponent); override;

    procedure InsertingOnEmptyDock(Splitter: TControl);
    function GetAdjacentSplitter(SpacingDelta: Integer = 1): TSpTBXCustomSplitter;
    property ReadingPositionData: Boolean read FReadingPositionData;
  public
    constructor Create(AOwner: TComponent); override;
    function IsVertical: Boolean;
    procedure GetDockablePanelList(DPList: TList);
    procedure GetDockablePanelDockIndex(DPList: TList; DP: TSpTBXCustomDockablePanel; out DPDockIndex: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateDockablePanelsDockPos;
    property UpdatingLateralSize: Boolean read FUpdatingLateralSize;
  published
    // Republish LimitToOneRow, the inherited LimitToOneRow should always be True
    property LimitToOneRow: Boolean read FLimitToOneRow write SetLimitToOneRow default True;
    property Position: TSpTBXDockPosition read FPosition write SetPosition default dpxLeft;
    property AutoSplitterVisibility: Boolean read FAutoSplitterVisibility write FAutoSplitterVisibility default True;
    // Republish OnInsertRemoveBar, use the inherited OnRequestDock to show/hide the Splitter
    property OnInsertRemoveBar: TTBInsertRemoveEvent read FOnInsertRemoveBar write FOnInsertRemoveBar;
    // Republish OnRequestDock, use the inherited OnRequestDock to deny non DPs
    property OnRequestDock: TTBRequestDockEvent read FOnRequestDock write FOnRequestDock;
  end;

  TSpTBXMultiDock = class(TSpTBXCustomMultiDock);

  { TSpTBXDockablePanelButtonOptions }

  TSpTBXDockablePanelButtonOptions = class(TSpTBXButtonOptions)
  private
    FTaskPaneStyleResize: Boolean;
  protected
    FDockablePanel: TSpTBXCustomDockablePanel;
    procedure ButtonsClick(Sender: TObject); override;
    procedure CreateButtons; override;
    function Restoring(B: TSpTBXCustomItem): Boolean; override;
    procedure SetupButton(B: TSpTBXCustomItem); override;
  public
    constructor Create(AParent: TWinControl); override;
  published
    property Maximize default False;
    property Minimize default False;
    property TaskPaneStyleResize: Boolean read FTaskPaneStyleResize write FTaskPaneStyleResize default False;
    property TitleBarMaxSize default 19;
  end;

  { TSpTBXDockablePanelToolbar }

  TSpTBXDockablePanelToolbar = class(TSpTBXToolbar)
  protected
    function CanItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer): Boolean; override;
    function GetItemsTextColor(State: TSpTBXSkinStatesType): TColor; override;
    function GetRightAlignMargin: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetParentDockablePanel: TSpTBXCustomDockablePanel;
  end;

  { TSpTBXDockablePanel }

  TSpTBXCustomDockablePanel = class(TSpTBXCustomToolWindow, ITBItems)
  private
    FDefaultDockedSize: Integer;
    FFixedDockedSize: Boolean;
    FFloatingClientHeight: Integer;
    FFloatingClientWidth: Integer;
    FIsDockedMoving: Boolean;
    FIsManualSizing: Boolean;
    FLoadedBarSize: TSize;
    FLoadedDockPos: Integer;
    FLoadedState: TWindowState;
    FOptions: TSpTBXDockablePanelButtonOptions;
    FShowCaption: Boolean;
    FShowCaptionWhenDocked: Boolean;
    FShowVerticalCaption: Boolean;
    FOnDrawCaptionPanel: TSpTBXDrawEvent;
    FOnWindowStateChanged: TSpTBXWindowStateChangedEvent;
    function CanSplitResize(EdgePosition: TTBDockPosition): Boolean;
    procedure DockRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean);
    procedure DockResize(Sender: TObject);
    function InternalMaximize(Restore: Boolean): Boolean;
    procedure UpdateTitleBarRotation;
    function GetCaptionPanelSize: TPoint;
    function GetEffectiveHeight: Integer;
    function GetEffectiveWidth: Integer;
    function GetFloatingClientHeight: Integer;
    function GetFloatingClientWidth: Integer;
    function GetImages: TCustomImageList;
    function GetItems: TTBCustomItem;  // For ITBItems interface
    function GetRootItems: TTBRootItem;
    function GetToolbar: TSpTBXToolbar;
    function GetView: TTBToolbarView;
    procedure SetDefaultDockedSize(Value: Integer);
    procedure SetEffectiveHeight(const Value: Integer);
    procedure SetEffectiveWidth(const Value: Integer);
    procedure SetFloatingClientHeight(const Value: Integer);
    procedure SetFloatingClientWidth(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetShowCaptionWhenDocked(const Value: Boolean);
    procedure SetShowVerticalCaption(const Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    FPanel: TPanel;
    FToolbarDock: TSpTBXDock;
    FToolbar: TSpTBXDockablePanelToolbar;
    FState: TSpTBXDockStateRec;
    FDockForms: TList;

    // Component
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure ValidateContainer(AComponent: TComponent); override;

    // Sizing
    procedure BeginDockedMoving;
    procedure BeginSplitResizing(HitTest: Integer);
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    procedure DoWindowStateChanged(AWindowState: TWindowState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;

    // Painting
    procedure DoDrawCaptionPanel(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DockDrawBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure ToolbarDrawBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True); override;

    property DefaultDockedSize: Integer read FDefaultDockedSize write SetDefaultDockedSize default 0;
    property FixedDockedSize: Boolean read FFixedDockedSize write FFixedDockedSize default False;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTBRootItem read GetRootItems;
    property Options: TSpTBXDockablePanelButtonOptions read FOptions write FOptions;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowCaptionWhenDocked: Boolean read FShowCaptionWhenDocked write SetShowCaptionWhenDocked default True;
    property ShowVerticalCaption: Boolean read FShowVerticalCaption write SetShowVerticalCaption default False;
    property OnDrawCaptionPanel: TSpTBXDrawEvent read FOnDrawCaptionPanel write FOnDrawCaptionPanel;
    property OnWindowStateChanged: TSpTBXWindowStateChangedEvent read FOnWindowStateChanged write FOnWindowStateChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddDockForm(const Form: TTBCustomForm);
    procedure RemoveDockForm(const Form: TTBCustomForm);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // For ITBItems interface
    procedure DoneReadingPositionData(const Data: TTBReadPositionData); override;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); override;
    function IsResizable: Boolean;
    function IsVerticalTitleBar: Boolean;
    function Maximize: Boolean; virtual;
    function Maximized: Boolean;
    function Minimize: Boolean; virtual;
    function Minimized: Boolean;
    function Restore: Boolean; virtual;
    function SizeToggle(ToMaximize: Boolean): Boolean;

    property CaptionPanelSize: TPoint read GetCaptionPanelSize;
    property EffectiveWidth: Integer read GetEffectiveWidth write SetEffectiveWidth;
    property EffectiveHeight: Integer read GetEffectiveHeight write SetEffectiveHeight;
    property FloatingClientHeight: Integer read GetFloatingClientHeight write SetFloatingClientHeight;
    property FloatingClientWidth: Integer read GetFloatingClientWidth write SetFloatingClientWidth;
    property Toolbar: TSpTBXToolbar read GetToolbar;
    property View: TTBToolbarView read GetView;
  end;

  TSpTBXDockablePanel = class(TSpTBXCustomDockablePanel)
  published
    property ActivateParent;
    property Align;
    property Anchors;
    property CurrentDock;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property FloatingMode;
    property Font;
    property HideWhenInactive;
    property LastDock;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable;
    property ShowHint;
    property TabOrder;
    property UseLastDock;
    property Visible;
    // TTBCustomDockableWindow doesn't store the Width and Height, make
    // sure it is stored and do not store TSpTBXCustomToolWindow
    // ClientWidth/ClientHeight
    property Height stored True;
    property Width stored True;
    property OnCanResize;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnResize;
    property OnVisibleChanged;
    // TSpTBXCustomDockablePanel properties
    property DefaultDockedSize;
    property FixedDockedSize;
    property Images;
    property Items;
    property Options;
    property ShowCaption;
    property ShowCaptionWhenDocked;
    property ShowVerticalCaption;
    property OnDrawCaptionPanel;
    property OnWindowStateChanged;
  end;

  { TSpTBXSplitter }

  TSpTBXCustomSplitter = class(TCustomControl)
  private
    FAutoCalcMaxSize: Boolean;
    FGripSize: Integer;
    FGripHotTrack: Boolean;
    FMouseSplitControl: TControl;
    FMouseActiveControl: TWinControl;
    FMouseBrush: TBrush;
    FMouseDownPos: TPoint;
    FMousePrevSplitControlSize: Integer;
    FMouseDownOnGrip: Boolean;
    FMouseOverGrip: Boolean;
    FMouseLineDC: HDC;
    FMouseLineVisible: Boolean;
    FMousePrevBrush: HBrush;
    FMoving: Boolean;
    FMinSize: Integer;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FOldKeyDown: TKeyEvent;
    FResizeStyle: TResizeStyle;
    FSplitLinePaintingPos: Integer;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnMoving: TSpTBXCanResizeEvent;
    FOnMoved: TNotifyEvent;
    function GetGripRect: TRect;
    function GetMinimized: Boolean;
    procedure SetGripSize(const Value: Integer);
    procedure SetMinSize(const Value: integer);
    procedure UpdateControlSize(SplitControl: TControl);
    procedure MouseCalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure MouseAllocateLineDC;
    procedure MouseReleaseLineDC;
    procedure MouseDrawLine;
    procedure MouseFocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function ValidateSplitControl: TControl;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    FRestorePos: Integer;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoMoved; virtual;
    function DoMoving(var NewSize: Integer): Boolean; virtual;
    function IsVertical: Boolean;
    procedure MouseStopSizing; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    property AutoCalcMaxSize: Boolean read FAutoCalcMaxSize write FAutoCalcMaxSize default True;
    property GripSize: Integer read FGripSize write SetGripSize default 50;
    property GripHotTrack: Boolean read FGripHotTrack write FGripHotTrack default True;
    property MinSize: Integer read FMinSize write SetMinSize default 0;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsUpdate;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnMoving: TSpTBXCanResizeEvent read FOnMoving write FOnMoving;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeSplitControlSize(NewControlSize: Integer);
    procedure InvalidateGrip;
    procedure Minimize;
    procedure Restore;
    procedure Toggle;
    property GripRect: TRect read GetGripRect;
    property Minimized: Boolean read GetMinimized;
    property MouseOverGrip: Boolean read FMouseOverGrip;
    property Moving: Boolean read FMoving;
  published
    property Align default alLeft;
    property Width default 5;
  end;

  TSpTBXSplitter = class(TSpTBXCustomSplitter)
  published
    property Align;
    property Color;
    property Constraints;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // TSpTBXCustomSplitter properties
    property AutoCalcMaxSize;
    property GripSize;
    property MinSize;
    property ResizeStyle;
    property OnDrawBackground;
    property OnMoving;
    property OnMoved;
  end;

{ Painting helpers }
procedure SpDrawXPDockablePanelTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive, Vertical: Boolean);
procedure SpDrawXPDockablePanelBody(ACanvas: TCanvas; ARect: TRect; IsActive, IsFloating: Boolean);

{ Toolbar Load/Save Position helpers }
procedure SpTBRegLoadPositions(const OwnerComponent: TComponent; const RootKey: DWORD; const BaseRegistryKey: string);
procedure SpTBRegSavePositions(const OwnerComponent: TComponent; const RootKey: DWORD; const BaseRegistryKey: string);
procedure SpTBIniLoadPositions(const OwnerComponent: TComponent; const Filename, SectionNamePrefix: string); overload;
procedure SpTBIniLoadPositions(const OwnerComponent: TComponent; const IniFile: TCustomIniFile; const SectionNamePrefix: string); overload;
procedure SpTBIniSavePositions(const OwnerComponent: TComponent; const Filename, SectionNamePrefix: string); overload;
procedure SpTBIniSavePositions(const OwnerComponent: TComponent; const IniFile: TCustomIniFile; const SectionNamePrefix: string); overload;

implementation

uses
  Types, Themes, ComCtrls, Registry, TB2Consts, TB2Common;

const
  DockedBorderSize = 2;
  HT_TB2k_Border = 2000;
  HT_DP_SPLITRESIZELEFT = 86;
  HT_DP_SPLITRESIZERIGHT = 87;
  HT_DP_SPLITRESIZETOP = 88;
  HT_DP_SPLITRESIZEBOTTOM = 89;
  // Constants for ini/registry values. Do not localize!
  rvMultiDockWidth = 'MultiDockWidth';
  rvMultiDockHeight = 'MultiDockHeight';
  rvFloatingClientWidth = 'FloatingClientWidth';
  rvFloatingClientHeight = 'FloatingClientHeight';
  rvRestoreSize = 'RestoreSize';
  rvState = 'State';
  rvSplitterRestorePos = 'SplitterRestorePos';

type
  TTBCustomItemAccess = class(TTBCustomItem);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);
  TTBDockAccess = class(TTBDock);
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);


//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpFixDelphiAlignBug(W: TControl; NewSize: Integer; Splitter: TControl);
// [Bugfix]
{ Try to fix a Delphi align bug when a control is right or bottom aligned and
  its size is changed.
  To reproduce this:
  - On a new form drop 3 TPanels, align them to the bottom, don't change the size.
  - Drop a button, on its OnClick event change the middle panel size to 200.
  - Run the app, when you click the button the middle panel will be moved to the bottom of the form.

  Another Delphi bug: the controls aligning is based on the Controls array when they
  have the same position.
  For example:
  Form1.DisableAlign;
  try
    Control1.Left:= 10;  Control1.Top:= 10;
    Control1.Align:= alLeft;
    Control2.Left:= 10;  Control2.Top:= 10;
    Control2.Align:= alLeft;
    // Control1 has the aligning priority because it has lower index in the Controls array
  finally
    Form1.EnableAlign;
  end;

  This will affect the splitter when the splitter is Minimized or Maximazed to 0.
  Thats why the standard Borland TSplitter must minimize to a value higher than 0.

  Solution: we must move the splitter before or after the other control
  based on the Align property, for that we use SendToBack and BringToFront methods.
  SendToBack and BringToFront besides of changing the "z-order" changes the
  order of the control in the Controls array, SendToBack moves the control
  to the first position while BringToFront moves it the last position.

  When Align is alLeft or alTop we should make the splitter to have the
  aligning priority using BringToFront.
  And when Align is alRight or alBottom we will use SendToBack. }
var
  I, Delta: Integer;
  PrevBounds: TRect;
  C: TControl;
begin
  case W.Align of
    alLeft:
      begin
        if Assigned(Splitter) then Splitter.BringToFront;
        W.SendToBack;
        W.Width := NewSize;
      end;
    alTop:
      begin
        if Assigned(Splitter) then Splitter.BringToFront;
        W.SendToBack;
        W.Height := NewSize;
      end;
    alRight:
      begin
        W.Parent.DisableAlign;
        if W is TSpTBXCustomMultiDock then
          TSpTBXCustomMultiDock(W).BeginUpdate;
        try
          if Assigned(Splitter) then Splitter.SendToBack;
          W.BringToFront;
          PrevBounds := W.BoundsRect;
          W.Width := NewSize;
          Delta := (PrevBounds.Right - PrevBounds.Left) - W.Width;

          // Move all children
          for I := 0 to W.Parent.ControlCount - 1 do begin
            C := W.Parent.Controls[I];
            if C.Align = W.Align then
              if (C = Splitter) and (PrevBounds.Right - PrevBounds.Left = 0) then
                C.Left := C.Left + Delta - 1
              else
                if (C.Left < PrevBounds.Right) or ((C = W) and (C.Left = PrevBounds.Right)) then
                  C.Left := C.Left + Delta;
          end;
        finally
          if W is TSpTBXCustomMultiDock then
            TSpTBXCustomMultiDock(W).EndUpdate;
          W.Parent.EnableAlign;
        end;
      end;
    alBottom:
      begin
        W.Parent.DisableAlign;
        if W is TSpTBXCustomMultiDock then
          TSpTBXCustomMultiDock(W).BeginUpdate;
        try
          if Assigned(Splitter) then Splitter.SendToBack;
          W.BringToFront;
          PrevBounds := W.BoundsRect;
          W.Height := NewSize;
          Delta := (PrevBounds.Bottom - PrevBounds.Top) - W.Height;

          // Move all children
          for I := 0 to W.Parent.ControlCount - 1 do begin
            C := W.Parent.Controls[I];
            if C.Align = W.Align then
              if (C = Splitter) and (PrevBounds.Bottom - PrevBounds.Top = 0) then
                C.Top := C.Top + Delta - 1
              else
                if (C.Top < PrevBounds.Bottom) or ((C = W) and (C.Top = PrevBounds.Bottom)) then
                  C.Top := C.Top + Delta;
          end;
        finally
          if W is TSpTBXCustomMultiDock then
            TSpTBXCustomMultiDock(W).EndUpdate;
          W.Parent.EnableAlign;
        end;
      end;
  end;
end;

function SpAdjacentSplitter(Dock: TTBDock; Space: Integer = 1): TSpTBXCustomSplitter;
var
  I: Integer;
  P: TPoint;
  R: TRect;
  C: TControl;
begin
  Result := nil;

  P := Point(Dock.Left, Dock.Top);
  case Dock.Position of
    dpLeft: Inc(P.X, Dock.Width + Space);
    dpRight: Dec(P.X, Space);
    dpTop: Inc(P.Y, Dock.Height + Space);
    dpBottom: Dec(P.Y, Space);
  end;

  for I := 0 to Dock.Parent.ControlCount - 1 do begin
    C := Dock.Parent.Controls[I];
    R := C.BoundsRect;
    if (C.Align = Dock.Align) and (C is TSpTBXCustomSplitter) then begin
      // The splitter should be adjacent to the Dock
      // Or the splitter is not visible and a DP is being docked on an
      // empty Dock.
      if PtInRect(R, P) or
        (not C.Visible and (Space = -1)) then
      begin
        Result := TSpTBXCustomSplitter(C);
        Break;
      end;
    end;
  end;
end;

procedure SpDPGetDockableMultiDockList(DP: TSpTBXCustomDockablePanel; var L: TList);
// Returns a valid list of MultiDocks where the DP can dock to.

  procedure Recurse(const ParentCtl: TWinControl);
  var
    M: TTBDock;
    I: Integer;
  begin
    if DP.ContainsControl(ParentCtl) or not ParentCtl.Showing then Exit;

    for I := 0 to ParentCtl.ControlCount-1 do
      if ParentCtl.Controls[I] is TSpTBXCustomMultiDock then begin
        M := TSpTBXCustomMultiDock(ParentCtl.Controls[I]);
        if (L.IndexOf(M) = -1) and M.Visible and M.AllowDrag and (M.Position in DP.DockableTo) then
          if DP.CurrentDock = M then
            L.Insert(0, M)  // Add CurrentDock to the DockList first so that it gets priority
          else
            L.Add(M);
      end
      else
        if (ParentCtl.Controls[I] is TWinControl) and not (ParentCtl.Controls[I] is TTBDock) then
          Recurse(TWinControl(ParentCtl.Controls[I]));
  end;

var
  ParentForm: TTBCustomForm;
  DockFormsList: TList;
  I, J: Integer;
begin
  L.Clear;
  ParentForm := TBGetToolWindowParentForm(DP);
  DockFormsList := TList.Create;
  try
    if Assigned(DP.FDockForms) then begin
      for I := 0 to Screen.CustomFormCount - 1 do begin
        J := DP.FDockForms.IndexOf(Screen.CustomForms[I]);
        if (J > -1) and (DP.FDockForms[J] <> ParentForm) then
          DockFormsList.Add(DP.FDockForms[J]);
      end;
    end;
    
    if Assigned(ParentForm) then
      DockFormsList.Insert(0, ParentForm);

    for I := 0 to DockFormsList.Count - 1 do
      Recurse(DockFormsList[I]);
  finally
    DockFormsList.Free;
  end;
end;

function SpDPInmediateResizableSibling(DP: TSpTBXCustomDockablePanel;
  ResizeType: TSpTBXDPResizeType; out IsAdjacent: Boolean): TSpTBXCustomDockablePanel;
// Returns the inmediate resizable DP sibling

  function FindPrevSibling(L: TList; StartIndex: Integer): TSpTBXCustomDockablePanel;
  var
    J: Integer;
    LItem: TSpTBXCustomDockablePanel;
  begin
    Result := nil;
    if StartIndex > 0 then
      for J := StartIndex - 1 downto 0 do begin
        LItem := TSpTBXCustomDockablePanel(L[J]);
        if (LItem <> DP) and LItem.IsResizable then begin
          Result := LItem;
          IsAdjacent := J = StartIndex - 1;
          Break;
        end;
      end;
  end;

  function FindNextSibling(L: TList; StartIndex: Integer): TSpTBXCustomDockablePanel;
  var
    J: Integer;
    LItem: TSpTBXCustomDockablePanel;
  begin
    Result := nil;
    if StartIndex + 1 < L.Count then
      for J := StartIndex + 1 to L.Count - 1 do begin
        LItem := TSpTBXCustomDockablePanel(L[J]);
        if (LItem <> DP) and LItem.IsResizable then begin
          Result := LItem;
          IsAdjacent := J = StartIndex + 1;
          Break;
        end;
      end;
  end;

var
  I: Integer;
  MultiDock: TSpTBXCustomMultiDock;
  L: TList;
  DkPanel: TSpTBXCustomDockablePanel;
begin
  Result := nil;
  DkPanel := nil;
  IsAdjacent := False;
  if not (DP.CurrentDock is TSpTBXCustomMultiDock) then Exit;

  if DP.Docked then begin
    L := TList.Create;
    try
      MultiDock := TSpTBXCustomMultiDock(DP.CurrentDock);
      MultiDock.GetDockablePanelList(L);
      if L.Count <= 1 then Exit;

      // Resize only the inmediate dockable panel sibling
      // Find DP on the DockList
      for I := 0 to L.Count - 1 do
        if L[I] = DP then begin
          DkPanel := L[I];
          Break;
        end;
      if not Assigned(DkPanel) then Exit;

      // Find the inmediate resizable sibling
      case ResizeType of
        dprtMinimizeOrRestore:
          begin
            // Search the prev sibling, if not found search the next sibling
            Result := FindPrevSibling(L, I);
            if not Assigned(Result) then
              Result := FindNextSibling(L, I);
          end;
        dprtAppendResize:
          begin
            // Search from the first item to the last
            I := -1;
            Result := FindNextSibling(L, I);
          end;
      else
        // Search the next sibling, if not found search the prev sibling
        Result := FindNextSibling(L, I);
        if not Assigned(Result) then
          Result := FindPrevSibling(L, I);
      end;

    finally
      L.Free;
    end;
  end;
end;

procedure SpDPUpdateDockPos(DPList: TList; IsVertical: Boolean); overload;
// Updates the DP.DockPos on all the DPs on the list
var
  I, TotalDockPos: Integer;
  DP: TSpTBXCustomDockablePanel;
begin
  TotalDockPos := 0;
  for I := 0 to DPList.Count - 1 do begin
    DP := DPList[I];
    if IsVertical then begin
      DP.DockPos := TotalDockPos;
      Inc(TotalDockPos, DP.Height);
    end
    else begin
      DP.DockPos := TotalDockPos;
      Inc(TotalDockPos, DP.Width);
    end;
  end;
end;

procedure SpDPSwapPos(MultiDock: TSpTBXCustomMultiDock; DP1, DP2: TSpTBXCustomDockablePanel);
// Swaps the positions of two DPs
var
  L: TList;
  I, DP1Index, DP2Index: Integer;
  Temp: TSpTBXCustomDockablePanel;
begin
  L := TList.Create;
  MultiDock.BeginUpdate;
  try
    MultiDock.GetDockablePanelList(L);

    // Find the DPs indexes
    DP1Index := -1;
    DP2Index := -1;
    for I := 0 to L.Count - 1 do begin
      Temp := L[I];
      if Temp = DP1 then DP1Index := I;
      if Temp = DP2 then DP2Index := I;
      if (DP1Index > -1) and (DP2Index > -1) then Break;
    end;

    if (DP1Index > -1) and (DP2Index > -1) then begin
      // Swap the DPs
      Temp := L[DP1Index];
      L[DP1Index] := L[DP2Index];
      L[DP2Index] := Temp;
      // Adjust the Dock Pos
      SpDPUpdateDockPos(L, MultiDock.IsVertical);
    end;
  finally
    Multidock.EndUpdate;
    L.Free;
  end;
end;

function SpDPResize(DP: TSpTBXCustomDockablePanel; NewSize: Integer; ResizeType: TSpTBXDPResizeType = dprtManualResize): Boolean;
var
  PrevSize, Delta, MinSize: Integer;
  MultiDock: TSpTBXCustomMultiDock;
  DPSibling: TSpTBXCustomDockablePanel;
  IsDPSiblingAdjacent: Boolean;
begin
  Result := False;

  if DP.Docked then begin
    if not (DP.CurrentDock is TSpTBXCustomMultiDock) then Exit;

    MultiDock := TSpTBXCustomMultiDock(DP.CurrentDock);
    if MultiDock.ToolbarCount < 2 then Exit;

    MultiDock.BeginUpdate;
    try
      // Resize only the inmediate dockable panel sibling
      DPSibling := SpDPInmediateResizableSibling(DP, ResizeType, IsDPSiblingAdjacent);
      if Assigned(DPSibling) then begin
        case ResizeType of
          dprtAppendResize:
            begin
              // Resize the 1st DP
              if MultiDock.IsVertical then
                DPSibling.Height := DPSibling.Height - NewSize
              else
                DPSibling.Width := DPSibling.Width - NewSize;
              MultiDock.UpdateDockablePanelsDockPos;
            end;
          dprtManualResize, dprtMinimizeOrRestore, dprtMinimizeOrRestoreTaskPaneStyle:
            begin
              if MultiDock.IsVertical then begin
                PrevSize := DP.Height;
                DP.Height := NewSize;
                Delta := DP.Height - PrevSize;
                DPSibling.Height := DPSibling.Height - Delta;
              end
              else begin
                PrevSize := DP.Width;
                DP.Width := NewSize;
                Delta := DP.Width - PrevSize;
                DPSibling.Width := DPSibling.Width - Delta;
              end;
              MultiDock.UpdateDockablePanelsDockPos;
            end;
          dprtSplitResize:
            begin
              if DPSibling.DockPos < DP.DockPos then
                Exit;

              if MultiDock.IsVertical then
                MinSize := DPSibling.MinClientHeight + (DockedBorderSize * 2)
              else
                MinSize := DPSibling.MinClientWidth + (DockedBorderSize * 2);

              // If DP can't be resized find another sibling
              if not (csDesigning in MultiDock.ComponentState) and DP.FixedDockedSize then begin
                if MultiDock.IsVertical then
                  Delta := NewSize - DP.Height
                else
                  Delta := NewSize - DP.Width;
                DP := DPSibling;
                // Use dprtMinimizeOrRestore as the ResizeType, we need to find the previous DP in the list
                DPSibling := SpDPInmediateResizableSibling(DP, dprtMinimizeOrRestore, IsDPSiblingAdjacent);
                // Make sure DPSibling index < DP index
                if not Assigned(DPSibling) or (DPSibling.DockPos > DP.DockPos) then
                  Exit;

                if MultiDock.IsVertical then begin
                  NewSize := DP.Height - Delta;
                  if (DPSibling.Height + Delta < DPSibling.MinClientHeight + (DockedBorderSize * 2)) or
                     (NewSize < DP.MinClientHeight + (DockedBorderSize * 2)) then
                  begin
                    Exit;
                  end;
                end
                else begin
                  NewSize := DP.Width - Delta;
                  if (DPSibling.Width + Delta < DPSibling.MinClientWidth + (DockedBorderSize * 2)) or
                     (NewSize < DP.MinClientWidth + (DockedBorderSize * 2)) then
                  begin
                    Exit;
                  end;
                end;
              end;

              // Resize the DP and DPSibling
              if MultiDock.IsVertical then begin
                if NewSize < MinSize then
                  NewSize := MinSize;
                PrevSize := DP.Height;
                Delta := NewSize - PrevSize;
                if DPSibling.Height - Delta < MinSize then begin
                  Delta := DPSibling.Height - MinSize;
                  if Delta <= 0  then Exit;
                  DP.Height := PrevSize + Delta;
                end
                else
                  DP.Height := NewSize;
                DPSibling.Height := DPSibling.Height - Delta;
              end
              else begin
                if NewSize < MinSize then
                  NewSize := MinSize;
                PrevSize := DP.Width;
                Delta := NewSize - PrevSize;
                if DPSibling.Width - Delta < MinSize then begin
                  Delta := DPSibling.Width - MinSize;
                  if Delta <= 0 then Exit;
                  DP.Width := PrevSize + Delta;
                end
                else
                  DP.Width := NewSize;
                DPSibling.Width := DPSibling.Width - Delta;
              end;
            end;
        end;

        Result := True;
      end;
    finally
      // Update DockPos of all the DPs, including the non-visible DPs, even when DPSibling = nil
      if ResizeType = dprtSplitResize then
        MultiDock.UpdateDockablePanelsDockPos;

      MultiDock.EndUpdate;

      // Resize the DP after the it was appended
      if ResizeType = dprtAppendResize then
        SpDPResize(DP, NewSize, dprtManualResize);
    end;
  end
  else begin
    // Not docked nor floating
    DP.Width := NewSize
  end;
end;

function SpPtInMultiDock(P: TPoint; MultiDockList: TList): TSpTBXCustomMultiDock;
// Returns the Dock that is under the point, on screen coordinates
var
  I: Integer;
  MultiDock: TSpTBXCustomMultiDock;
  R: TRect;
const
  SnapBuffer = 24;
  MinDockSize = 4;
begin
  Result := nil;
  for I := 0 to MultiDockList.Count - 1 do
    if TControl(MultiDockList[I]) is TSpTBXCustomMultiDock then begin
      MultiDock := TSpTBXCustomMultiDock(MultiDockList[I]);
      GetWindowRect(MultiDock.Handle, R);

      // Ensure there is a minimum size for mouse sensibility
      case MultiDock.Position of
        dpxTop:
          if (R.Bottom - R.Top) < MinDockSize then
            Inc(R.Bottom, SnapBuffer);
        dpxBottom:
          if (R.Bottom - R.Top) < MinDockSize then
            Dec(R.Top, SnapBuffer);
        dpxLeft:
          if (R.Right - R.Left) < MinDockSize then
            Inc(R.Right, SnapBuffer);
        dpxRight, dpxClient:
          if (R.Right - R.Left) < MinDockSize then
            Dec(R.Left, SnapBuffer);
      end;

      if PtInRect(R, P) then begin
        Result := MultiDock;
        Break;
      end;
    end;
end;

function SpPtInDP(P: TPoint; MultiDock: TSpTBXCustomMultiDock; OnlyOnTitleBar: Boolean): TSpTBXCustomDockablePanel;
// Returns a DP that is under the point, on screen coordinates
// If OnlyOnTitleBar is true it returns a DP if the point is under the DP's TitleBar.
var
  I: Integer;
  DP: TSpTBXCustomDockablePanel;
  R: TRect;
begin
  Result := nil;
  for I := 0 to MultiDock.ToolbarCount - 1 do
    if MultiDock.Toolbars[I] is TSpTBXCustomDockablePanel then begin
      DP := TSpTBXCustomDockablePanel(MultiDock.Toolbars[I]);
      if OnlyOnTitleBar then begin
        if MultiDock.IsVertical then
          GetWindowRect(DP.FToolbar.Handle, R)
        else begin
          // When the DP is horizontal track 20 pixels from the left
          R.TopLeft := DP.ClientToScreen(Point(0, 0));
          R.BottomRight := DP.ClientToScreen(Point(20, DP.ClientHeight));
        end;
      end
      else
        GetWindowRect(DP.Handle, R);

      if PtInRect(R, P) then begin
        Result := DP;
        Break;
      end;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

procedure SpDrawXPDockablePanelTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive, Vertical: Boolean);
var
  Details: TThemedElementDetails;
begin
  case SkinManager.GetSkinType of
    sknNone, sknWindows:
      begin
        // [Theme-Change]
        // When the XP theme is used just paint a gradient
        SpGradientFill(ACanvas, ARect, SpLighten(clBtnFace, 12), SpLighten(clBtnFace, -12), not Vertical);
        Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
      end;
    sknDelphiStyle:
      if CurrentSkin.GetThemedElementDetails(skncDockablePanelTitleBar, sknsNormal, Details) then
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
    sknSkin:
      CurrentSkin.PaintBackground(ACanvas, ARect, skncDockablePanelTitleBar, sknsNormal, True, True, Vertical);
  end;
end;

procedure SpDrawXPDockablePanelBody(ACanvas: TCanvas; ARect: TRect; IsActive, IsFloating: Boolean);
var
  C: TColor;
  {$IF CompilerVersion >= 23} // for Delphi XE2 and up
  Details: TThemedElementDetails;
  {$IFEND}
begin
  case SkinManager.GetSkinType of
    sknNone, sknWindows:
      begin
        C := ACanvas.Brush.Color;
        ACanvas.Brush.Color := SpMixColors(clBtnFace, clWindow, 80);
        ACanvas.FillRect(ARect);
        if not IsFloating then begin
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.FrameRect(ARect);
          InflateRect(ARect, -1, -1);
          ACanvas.Brush.Color := clWhite;
          ACanvas.FrameRect(ARect);
        end;
        ACanvas.Brush.Color := C;
      end;
    sknDelphiStyle:
      begin
        {$IF CompilerVersion >= 23} // for Delphi XE2 and up
        if CurrentSkin.GetThemedElementDetails(skncDockablePanel, sknsNormal, Details) then begin
          if SpTBXThemeServices.GetElementColor(Details, ecFillColor, C) and (C <> clNone) then
            ACanvas.Brush.Color := C
          else
            ACanvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace);
          ACanvas.FillRect(ARect);
        end;
        {$IFEND}
      end;
    sknSkin:
      CurrentSkin.PaintBackground(ACanvas, ARect, skncDockablePanel, sknsNormal, True, not IsFloating);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Toolbar Load/Save Position helpers }

procedure SpTBUpdateMultiDocksAfterLoad(const M: TSpTBXMultiDock);
// TBCustomLoadPositions doesn't correctly position the DPs after they are loaded.
// To reproduce:
// 1) Drop a top aligned MultiDock with 3 DPs (DP1, DP2, DP3), and drop a left
//    aligned Multidock with a single DP (DP4)
// 2) Use TBIniLoadPositions in Form.OnShow and TBIniSavePositions in Form.OnClose
// 3) Run the app, position the top DPs in this order: DP4, DP3, DP1, and
//    dock DP2 on the left aligned Multidock
// 4) Close and run the app, the top DPs are not correctly positioned/sized
// This happens because TBCustomLoadPositions updates the DockPos and Size
// after the toolbar Parent is set, toolbar by toolbar.
// DockPos and Size should be changed after all the toolbars parent is changed.
// http://news.jrsoftware.org/read/article.php?id=15131&group=jrsoftware.toolbar2000.thirdparty#15131
var
  J: Integer;
  DP: TSpTBXCustomDockablePanel;
  L: TList;
  Sz: TSize;
  R: TRect;
begin
  L := TList.Create;
  M.BeginUpdate;
  try
    M.GetDockablePanelList(L);
    for J := 0 to L.Count - 1 do begin
      DP := TSpTBXCustomDockablePanel(L[J]);
      if DP.FLoadedDockPos > -1 then
        DP.DockPos := DP.FLoadedDockPos;
      Windows.GetClientRect(DP.Handle, R);
      Sz := DP.FLoadedBarSize;
      if M.IsVertical then begin
        if Sz.cy > 0 then
          DP.Height := DP.Height - R.Bottom + Sz.cy;
      end
      else begin
        if Sz.cx > 0 then
          DP.Width := DP.Width - R.Right + Sz.cx;
      end;
    end;
  finally
    M.EndUpdate;
    L.Free;
  end;
end;

procedure SpTBUpdateBeforeLoadIni(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string; LockedDocks: TList);
var
  I: Integer;
  Dock: TTBDock;
  MultiDock: TSpTBXMultiDock;
begin
  for I := 0 to OwnerComponent.ComponentCount - 1 do begin
    if OwnerComponent.Components[I] is TSpTBXMultiDock then begin
      MultiDock := TSpTBXMultiDock(OwnerComponent.Components[I]);
      MultiDock.FReadingPositionData := True; // Set the flag to allow resizing of FixedDockedSize DPs
    end;
    // Set AllowDrag to true on all the Docks, otherwise TB2Dock.TBCustomLoadPositions
    // won't load the locked toolbars
    if OwnerComponent.Components[I] is TTBDock then begin
      Dock := TTBDock(OwnerComponent.Components[I]);
      if not Dock.AllowDrag then begin
        Dock.AllowDrag := True;
        LockedDocks.Add(Dock);
      end;
    end;
  end;
end;

procedure SpTBUpdateAfterLoadIni(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string; LockedDocks: TList);
var
  I, W, H: Integer;
  Dock: TTBDock;
  MultiDock: TSpTBXMultiDock;
  Splitter: TSpTBXCustomSplitter;
begin
  for I := 0 to OwnerComponent.ComponentCount - 1 do begin
    // Load the MultiDock size
    if OwnerComponent.Components[I] is TSpTBXMultiDock then begin
      MultiDock := TSpTBXMultiDock(OwnerComponent.Components[I]);
      MultiDock.FReadingPositionData := False; // Reset the flag
      if MultiDock.ToolbarCount > 0 then begin
        W := IniFile.ReadInteger(SectionNamePrefix + MultiDock.Name, rvMultiDockWidth, -1);
        H := IniFile.ReadInteger(SectionNamePrefix + MultiDock.Name, rvMultiDockHeight, -1);
        Splitter := MultiDock.GetAdjacentSplitter;
        if Assigned(Splitter) then begin
          case MultiDock.Align of
            alLeft, alRight:
              if (W > -1) and (W <> MultiDock.Width) then
                SpFixDelphiAlignBug(MultiDock, W, Splitter);
            alTop, alBottom:
              if (H > -1) and (H <> MultiDock.Height) then
                SpFixDelphiAlignBug(MultiDock, H, Splitter);
          end;
          MultiDock.InsertingOnEmptyDock(Splitter); // Re-align adjacent splitter
        end;

        // Update the size and position of the DPs
        SpTBUpdateMultiDocksAfterLoad(MultiDock);
      end;
    end;

    // Load Splitter.RestorePos
    if OwnerComponent.Components[I] is TSpTBXCustomSplitter then begin
      Splitter := TSpTBXCustomSplitter(OwnerComponent.Components[I]);
      Splitter.FRestorePos := IniFile.ReadInteger(SectionNamePrefix + Splitter.Name, rvSplitterRestorePos, 60);
    end;
  end;

  // Restore AllowDrag on LockedDocks
  for I := 0 to LockedDocks.Count - 1 do begin
    Dock := TTBDock(LockedDocks[I]);
    Dock.AllowDrag := False;      
  end;
end;

procedure SpTBUpdateBeforeSaveIni(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string; LockedDocks: TList);
var
  I: Integer;
  Dock: TTBDock;
begin
  for I := 0 to OwnerComponent.ComponentCount - 1 do begin
    // Set AllowDrag to true on all the Docks, otherwise TB2Dock.TBCustomLoadPositions
    // won't load the locked toolbars
    if OwnerComponent.Components[I] is TTBDock then begin
      Dock := TTBDock(OwnerComponent.Components[I]);
      if not Dock.AllowDrag then begin
        Dock.AllowDrag := True;
        LockedDocks.Add(Dock);
      end;
    end;
  end;
end;

procedure SpTBUpdateAfterSaveIni(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string; LockedDocks: TList);
var
  I: Integer;
  Dock: TTBDock;
  MultiDock: TSpTBXMultiDock;
  Splitter: TSpTBXCustomSplitter;
begin
  for I := 0 to OwnerComponent.ComponentCount - 1 do begin
    // Save the MultiDock size
    if OwnerComponent.Components[I] is TSpTBXMultiDock then begin
      MultiDock := TSpTBXMultiDock(OwnerComponent.Components[I]);
      if MultiDock.ToolbarCount > 0 then begin
        IniFile.WriteInteger(SectionNamePrefix + MultiDock.Name, rvMultiDockWidth, MultiDock.Width);
        IniFile.WriteInteger(SectionNamePrefix + MultiDock.Name, rvMultiDockHeight, MultiDock.Height);
      end;
    end;

    // Save the Splitter.RestorePos
    if OwnerComponent.Components[I] is TSpTBXCustomSplitter then begin
      Splitter := TSpTBXCustomSplitter(OwnerComponent.Components[I]);
      IniFile.WriteInteger(SectionNamePrefix + Splitter.Name, rvSplitterRestorePos, Splitter.FRestorePos);
    end;
  end;

  // Restore AllowDrag on LockedDocks
  for I := 0 to LockedDocks.Count - 1 do begin
    Dock := TTBDock(LockedDocks[I]);
    Dock.AllowDrag := False;      
  end;
end;

procedure SpTBRegLoadPositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: string);
var
  Reg: TRegistryIniFile;
  LockedDocks: TList;  
begin
  // Use TRegistryIniFile to call SpTBUpdateAfterLoadIni
  Reg := TRegistryIniFile.Create('', KEY_QUERY_VALUE);
  LockedDocks := TList.Create;  
  try
    Reg.RegIniFile.RootKey := RootKey;
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, False) then begin
      SpTBUpdateBeforeLoadIni(OwnerComponent, Reg, '', LockedDocks);
      TBRegLoadPositions(OwnerComponent, RootKey, BaseRegistryKey);
      SpTBUpdateAfterLoadIni(OwnerComponent, Reg, '', LockedDocks);
    end;
  finally
    Reg.Free;
    LockedDocks.Free;          
  end;
end;

procedure SpTBRegSavePositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: string);
var
  Reg: TRegistryIniFile;
  LockedDocks: TList;
begin
  // Use TRegistryIniFile to call SpTBUpdateAfterSaveIni
  Reg := TRegistryIniFile.Create('');
  LockedDocks := TList.Create;
  try
    Reg.RegIniFile.RootKey := RootKey;
    Reg.RegIniFile.CreateKey(BaseRegistryKey);
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, True) then begin
      SpTBUpdateBeforeSaveIni(OwnerComponent, Reg, '', LockedDocks);
      TBRegSavePositions(OwnerComponent, RootKey, BaseRegistryKey);
      SpTBUpdateAfterSaveIni(OwnerComponent, Reg, '', LockedDocks);
    end;
  finally
    Reg.Free;
    LockedDocks.Free;      
  end;
end;

procedure SpTBIniLoadPositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string);
var
  LockedDocks: TList;
begin
  LockedDocks := TList.Create;
  try
    SpTBUpdateBeforeLoadIni(OwnerComponent, IniFile, SectionNamePrefix, LockedDocks);
    TBIniLoadPositions(OwnerComponent, IniFile, SectionNamePrefix);
    SpTBUpdateAfterLoadIni(OwnerComponent, IniFile, SectionNamePrefix, LockedDocks);  
  finally
    LockedDocks.Free;
  end;
end;

procedure SpTBIniSavePositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: string);
var
  LockedDocks: TList;
begin
  LockedDocks := TList.Create;
  try
    SpTBUpdateBeforeSaveIni(OwnerComponent, IniFile, SectionNamePrefix, LockedDocks);
    TBIniSavePositions(OwnerComponent, IniFile, SectionNamePrefix);
    SpTBUpdateAfterSaveIni(OwnerComponent, IniFile, SectionNamePrefix, LockedDocks);
  finally
    LockedDocks.Free;
  end;
end;

procedure SpTBIniLoadPositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: string);
// Use TMemIniFile instead of TIniFile for better readability and to solve
// the #7363 bug report from QC: http://qc.borland.com/wc/qcmain.aspx?d=7363
var
  MemIniFile: TMemIniFile;
begin
  MemIniFile := TMemIniFile.Create(Filename);
  try
    SpTBIniLoadPositions(OwnerComponent, MemIniFile, SectionNamePrefix);
  finally
    MemIniFile.Free;
  end;
end;

procedure SpTBIniSavePositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: string);
// Use TMemIniFile instead of TIniFile for better readability and to solve
// the #7363 bug report from QC: http://qc.borland.com/wc/qcmain.aspx?d=7363
var
  MemIniFile: TMemIniFile;
begin
  MemIniFile := TMemIniFile.Create(Filename);
  try
    SpTBIniSavePositions(OwnerComponent, MemIniFile, SectionNamePrefix);
    MemIniFile.UpdateFile;
  finally
    MemIniFile.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomMultiDock }

constructor TSpTBXCustomMultiDock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited LimitToOneRow := True;
  FAutoSplitterVisibility := True;
  FLimitToOneRow := True;
  FPosition := dpxTop;
  SetPosition(dpxLeft);
  inherited OnInsertRemoveBar := DoInsertRemoveBar;
  inherited OnRequestDock := DoRequestDock;
end;

procedure TSpTBXCustomMultiDock.DoInsertRemoveBar(Sender: TObject;
  Inserting: Boolean; Bar: TTBCustomDockableWindow);
var
  SpacingDelta: Integer;
  Splitter: TSpTBXCustomSplitter;
begin
  // Automatically Show or Hide the adjacent splitter when
  // the MultiDock is empty
  if FAutoSplitterVisibility then begin
    if Inserting and (ToolbarCount = 1) then
      SpacingDelta := -1 // Inserting on an empty Dock
    else
      SpacingDelta := 1;

    Splitter := GetAdjacentSplitter(SpacingDelta);

    if Assigned(Splitter) then begin
      if Inserting and (ToolbarCount = 1) then begin
        // Inserting on an empty Dock, re-align adjacent splitter
        // When Width/Height = 0 the realign must be done by SetParent
        Splitter.Visible := True;
        if (IsVertical and (Width > 0)) or (not IsVertical and (Height > 0)) then
          InsertingOnEmptyDock(Splitter);
      end
      else begin
        if ToolbarCount = 0 then
          Splitter.Visible := False;
      end;
    end;
  end;

  if Assigned(FOnInsertRemoveBar) then FOnInsertRemoveBar(Sender, Inserting, Bar);
end;

procedure TSpTBXCustomMultiDock.DoRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
begin
  Accept := Assigned(Bar) and (Bar is TSpTBXCustomDockablePanel);
  if Accept then
    if Assigned(FOnRequestDock) then FOnRequestDock(Sender, Bar, Accept);
end;

procedure TSpTBXCustomMultiDock.AlignControls(AControl: TControl;
  var Rect: TRect);
begin
  inherited;
  if FPosition = dpxClient then
    UpdateDPLateralSize(Width, Height);
end;

function CompareEffectiveDockPos(Item1, Item2: Pointer): Integer;
begin
  Result := TSpTBXCustomDockablePanel(Item1).EffectiveDockPos - TSpTBXCustomDockablePanel(Item2).EffectiveDockPos;
end;

procedure TSpTBXCustomMultiDock.GetDockablePanelList(DPList: TList);
var
  I: Integer;
  T: TTBCustomDockableWindow;
begin
  DPList.Clear;
  for I := 0 to ToolbarCount - 1 do begin
    T := Toolbars[I];
    if T is TSpTBXCustomDockablePanel then
      DPList.Add(T);
  end;
  // Sort the list based on the dock pos
  DPList.Sort(CompareEffectiveDockPos);
end;

function TSpTBXCustomMultiDock.GetAdjacentSplitter(SpacingDelta: Integer = 1): TSpTBXCustomSplitter;
begin
  if Assigned(FLastSplitter) then
    Result := FLastSplitter
  else begin
    Result := SpAdjacentSplitter(Self, SpacingDelta);
    if Result <> FLastSplitter then begin
      if Assigned(FLastSplitter) then FLastSplitter.RemoveFreeNotification(Self);
      FLastSplitter := Result;
      if Assigned(FLastSplitter) then
        FLastSplitter.FreeNotification(Self);
    end;
  end;
end;

procedure TSpTBXCustomMultiDock.GetDockablePanelDockIndex(DPList: TList;
  DP: TSpTBXCustomDockablePanel; out DPDockIndex: Integer);
var
  I: Integer;
begin
  DPDockIndex := -1;
  GetDockablePanelList(DPList);
  for I := 0 to DPList.Count - 1 do
    if DPList[I] = DP then begin
      DPDockIndex := I;
      Break;
    end;
end;

procedure TSpTBXCustomMultiDock.InsertingOnEmptyDock(Splitter: TControl);
begin
  // When a DP is docked on an empty right/bottom aligned MultiDock and there's
  // an adjacent Splitter, the Splitter is moved to the right/bottom side
  // of the MultiDock:
  // http://news.jrsoftware.org/read/article.php?id=14410&group=jrsoftware.toolbar2000.thirdparty#14410
  // To fix this, re align the Splitter after the MultiDock is resized.
  if Assigned(Splitter) then begin
    Parent.DisableAlign;
    try
      case Splitter.Align of
        alTop:    Splitter.Top := Top + Height + 1;
        alBottom: Splitter.Top := Top - 1;
        alLeft:   Splitter.Left := Left + Width + 1;
        alRight:  Splitter.Left := Left - 1;
      end;
    finally
      Parent.EnableAlign;
    end;
  end;
end;

function TSpTBXCustomMultiDock.IsVertical: Boolean;
begin
  Result := not (Position in [dpxTop, dpxBottom]);
end;

procedure TSpTBXCustomMultiDock.Loaded;
var
  Splitter: TSpTBXCustomSplitter;
begin
  inherited;

  // Automatically Hide the adjacent splitter when
  // the MultiDock is empty
  if FAutoSplitterVisibility and (ToolbarCount = 0) then begin
    Splitter := GetAdjacentSplitter;
    if Assigned(Splitter) then
      Splitter.Visible := False;
  end;

  ArrangeToolbars;  // Needed to rearrange fixed sized DPs when the form is loaded
end;

procedure TSpTBXCustomMultiDock.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLastSplitter) then
    FLastSplitter := nil;
end;

procedure TSpTBXCustomMultiDock.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateDPLateralSize(AWidth, AHeight);
end;

procedure TSpTBXCustomMultiDock.SetLimitToOneRow(const Value: Boolean);
begin
  FLimitToOneRow := True;
end;

procedure TSpTBXCustomMultiDock.SetPosition(const Value: TSpTBXDockPosition);
begin
  if FPosition <> Value then begin
    if (ControlCount <> 0) then
      raise EInvalidOperation.Create(STBDockCannotChangePosition);
    FPosition := Value;
    case Value of
      dpxLeft:   inherited Position := dpLeft;
      dpxTop:    inherited Position := dpTop;
      dpxRight:  inherited Position := dpRight;
      dpxBottom: inherited Position := dpBottom;
      dpxClient:
        begin
          inherited Position := dpRight;
          Align := alClient;
        end;
    end;
    ArrangeToolbars;
  end;
end;

procedure TSpTBXCustomMultiDock.UpdateDockablePanelsDockPos;
// Updates the DP.DockPos on all the DPs on the MultiDock
var
  L: TList;
begin
  L := TList.Create;
  BeginUpdate;
  try
    GetDockablePanelList(L);
    SpDPUpdateDockPos(L, IsVertical);
  finally
    EndUpdate;
    L.Free;
  end;
end;

procedure TSpTBXCustomMultiDock.UpdateDPLateralSize(AWidth, AHeight: Integer);
// Update the lateral size of all the DPs relative to the MultiDock
// This causes flicker!
var
  L: TList;
  I, Size: Integer;
  DP: TSpTBXCustomDockablePanel;
  IsDPSiblingAdjacent: Boolean;
begin
  L := TList.Create;
  try
    GetDockablePanelList(L);
    if L.Count = 0 then Exit;
    FUpdatingLateralSize := True;
    BeginUpdate;
    try
      Size := 0;
      if IsVertical then begin
        for I := 0 to L.Count - 1 do begin
          DP := TSpTBXCustomDockablePanel(L[I]);
          DP.Width := AWidth;    // Update the lateral size
          Inc(Size, DP.Height);  // Calculate the total size of all the DPs
        end;
        // If the last DP is not resizable make sure we fill the empty space
        if (Size < Height) or (Size > Height) then begin
          DP := L[L.Count - 1];
          if not DP.IsResizable then begin
            // Use dprtMinimizeOrRestore as the ResizeType, we need to find the previous DP in the list
            DP := SpDPInmediateResizableSibling(DP, dprtMinimizeOrRestore, IsDPSiblingAdjacent);
            if Assigned(DP) then
              DP.Height := DP.Height + (Height - Size);
          end;
          SpDPUpdateDockPos(L, IsVertical);
        end;
      end
      else begin
        for I := 0 to L.Count - 1 do begin
          DP := TSpTBXCustomDockablePanel(L[I]);
          DP.Height := AHeight;  // Update the lateral size
          Inc(Size, DP.Width);   // Calculate the total size of all the DPs
        end;
        // If the last DP is not resizable make sure we fill the empty space
        if (Size < Width) or (Size > Width) then begin
          DP := L[L.Count - 1];
          if not DP.IsResizable then begin
            // Use dprtMinimizeOrRestore as the ResizeType, we need to find the previous DP in the list
            DP := SpDPInmediateResizableSibling(DP, dprtMinimizeOrRestore, IsDPSiblingAdjacent);
            if Assigned(DP) then
              DP.Width := DP.Width + (Width - Size);
          end;
          SpDPUpdateDockPos(L, IsVertical);
        end;
      end;
    finally
      EndUpdate;
      FUpdatingLateralSize := False;
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomMultiDock.ValidateInsert(AComponent: TComponent);
begin
  inherited;
  if not (AComponent is TSpTBXCustomDockablePanel) then
    raise EInvalidOperation.CreateFmt('Cannot insert %s into MultiDock', [AComponent.ClassName]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanelButtonOptions }

constructor TSpTBXDockablePanelButtonOptions.Create(AParent: TWinControl);
begin
  FDockablePanel := AParent as TSpTBXCustomDockablePanel;
  inherited;
  Maximize := False;
  Minimize := False;
  TitleBarMaxSize := 19;
end;

procedure TSpTBXDockablePanelButtonOptions.CreateButtons;
begin
  FToolbar := FDockablePanel.FToolbar;
  inherited;
end;

procedure TSpTBXDockablePanelButtonOptions.ButtonsClick(Sender: TObject);
begin
  if Sender = MinimizeButton then FDockablePanel.SizeToggle(False)
  else if Sender = MaximizeButton then FDockablePanel.SizeToggle(True)
  else if Sender = CloseButton then FDockablePanel.Close;
end;

function TSpTBXDockablePanelButtonOptions.Restoring(B: TSpTBXCustomItem): Boolean;
begin
  Result := False;
  if Assigned(FDockablePanel) then
    if B = MinimizeButton then
      Result := FDockablePanel.Minimized
    else
      if B = MaximizeButton then
        Result := FDockablePanel.Maximized;
end;

procedure TSpTBXDockablePanelButtonOptions.SetupButton(B: TSpTBXCustomItem);
begin
  inherited;
  TSpTBXCustomItemAccess(B).CustomWidth := 15;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanelToolbar }

constructor TSpTBXDockablePanelToolbar.Create(AOwner: TComponent);
begin
  inherited;
  CompoundToolbar := True;
end;

function TSpTBXDockablePanelToolbar.GetItemsTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := CurrentSkin.GetTextColor(skncDockablePanelTitleBar, State);
end;

function TSpTBXDockablePanelToolbar.GetParentDockablePanel: TSpTBXCustomDockablePanel;
var
  P: TWinControl;
begin
  Result := nil;
  P := Parent;
  while Assigned(P) do
    if P is TSpTBXCustomDockablePanel then begin
      Result := P as TSpTBXCustomDockablePanel;
      Break;
    end
    else
      P := P.Parent;
end;

function TSpTBXDockablePanelToolbar.GetRightAlignMargin: Integer;
begin
  Result := 4;
end;

function TSpTBXDockablePanelToolbar.CanItemClick(Item: TTBCustomItem;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  TransparentClick: Boolean;
  DP: TSpTBXCustomDockablePanel;
begin
  Result := True;

  // Move the DockablePanel if the toolbar client area or an item with
  // tbisClicksTransparent itemstyle is clicked (like a LabelItem)
  if Button = mbLeft then begin
    DP := GetParentDockablePanel;
    if Assigned(DP) and DP.IsMovable then begin
      if Assigned(Item) then
        TransparentClick := tbisClicksTransparent in TTBCustomItemAccess(Item).ItemStyle
      else
        TransparentClick := True;
      if TransparentClick then
        if ssDouble in Shift then
          DP.DoubleClick
        else begin
          Result := False;
          SendMessage(DP.Handle, WM_NCLBUTTONDOWN, HT_TB2k_Border, 0);
        end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomDockablePanel }

constructor TSpTBXCustomDockablePanel.Create(AOwner: TComponent);
begin
  inherited;

  FLoadedDockPos := -1;
  FLoadedBarSize.cx := -1;
  FLoadedBarSize.cy := -1;

  Stretch := True;
  DragHandleStyle := dhNone;

  SetBounds(Left, Top, 160, 128);

  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alTop;
  FPanel.BevelOuter := bvNone;

  FToolbarDock := TSpTBXDock.Create(Self);
  FToolbarDock.Parent := FPanel;
  FToolbarDock.OnRequestDock := DockRequestDock;
  FToolbarDock.OnDrawBackground := DockDrawBackground;
  FToolbarDock.OnResize := DockResize;

  FToolbar := TSpTBXDockablePanelToolbar.Create(Self);
  FToolbar.Parent := FToolbarDock;
  FToolbar.CurrentDock := FToolbarDock;
  FToolbar.Name := Name + 'Toolbar';
  FToolbar.Customizable := False;
  FToolbar.BorderStyle := bsNone;
  FToolbar.DockMode := dmCannotFloatOrChangeDocks;
  FToolbar.DragHandleStyle := dhNone;
  FToolbar.Options := FToolbar.Options + [tboNoAutoHint];
  FToolbar.Stretch := True;
  FToolbar.ShrinkMode := tbsmNone;
  FToolbar.ShowCaption := False;
  FToolbar.OnDrawBackground := ToolbarDrawBackground;

  FOptions := TSpTBXDockablePanelButtonOptions.Create(Self);
  FOptions.CaptionLabel := Caption;

  inherited ShowCaption := False; // Re-publish it, should always be False
  FShowCaption := True;
  FShowCaptionWhenDocked := True;

  DockResize(FToolbarDock); // Adjust ToolbarDock resizing
end;

procedure TSpTBXCustomDockablePanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if not (csDesigning in ComponentState) then
    with Params do
      Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

destructor TSpTBXCustomDockablePanel.Destroy;
begin
  FOptions.Free;
  FToolbar.Free;
  FToolbarDock.Free;
  FPanel.Free;

  inherited;

  FreeAndNil(FDockForms);  // After inherited, Notification accesses FDockForms
end;

procedure TSpTBXCustomDockablePanel.Loaded;
var
  I: Integer;
  C: TControl;
  DesignerRootItem: TTBCustomItem;
begin
  inherited;

  // The parent of TTBControlItem.Control should be the toolbar, not Self
  // (as setted in GetChildren for dfm streaming).
  DesignerRootItem := GetItems;
  for I := 0 to DesignerRootItem.Count - 1 do
    if DesignerRootItem[I] is TTBControlItem then begin
      C := TTBControlItem(DesignerRootItem[I]).Control;
      if Assigned(C) and (C.Parent <> FToolbar) then
        C.Parent := FToolbar;
    end;
end;

procedure TSpTBXCustomDockablePanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    RemoveFromList(FDockForms, AComponent);
end;

function TSpTBXCustomDockablePanel.Maximize: Boolean;
var
  PrevState: TWindowState;
begin
  Result := False;
  if not Maximized then begin
    // [DockablePanel-Rule]
    // Do not maximize if it's the only DP on the dock
    if Docked and (CurrentDock.ToolbarCount > 1) then begin
      PrevState := FState.DockedState;
      FIsManualSizing := True;
      try
        if InternalMaximize(False) then begin
          Result := True;
          FState.DockedState := wsMaximized;
          FOptions.SetupButtonIcon(FOptions.MinimizeButton);
          FOptions.SetupButtonIcon(FOptions.MaximizeButton);
        end
        else
          FState.DockedState := PrevState;
      finally
        FIsManualSizing := False;
      end;
    end;
  end;

  if Result then
    DoWindowStateChanged(wsMaximized);
end;

function TSpTBXCustomDockablePanel.Maximized: Boolean;
begin
  if Floating then
    Result := False
  else
    Result := FState.DockedState = wsMaximized;
end;

function TSpTBXCustomDockablePanel.Minimize: Boolean;
var
  CanMinimize: Boolean;
  I, MinimizedCount: Integer;
  L: TList;
  MultiDock: TSpTBXCustomMultiDock;
  RS: TSpTBXDPResizeType;
begin
  Result := False;
  if Floating then begin
    if FState.DockedState <> wsMinimized then begin
      FState.DockedState := wsMinimized;
      FState.RestoreSize := Parent.Height;
      Parent.ClientHeight := MinClientHeight;
      FOptions.SetupButtonIcon(FOptions.MinimizeButton);
      FOptions.SetupButtonIcon(FOptions.MaximizeButton);
      Result := True;
    end;
  end
  else
    if Docked and (FState.DockedState <> wsMinimized) and (CurrentDock is TSpTBXCustomMultiDock) then begin
      MultiDock := TSpTBXCustomMultiDock(CurrentDock);
      L := TList.Create;
      try
        MultiDock.GetDockablePanelList(L);
        // [DockablePanel-Rule]
        // Only minimize if it's horizontal and is the only DP on the dock
        // Or if it's vertical and it's not the only DP on the dock and the rest of the siblings are not minimized
        MinimizedCount := 0;
        if not MultiDock.IsVertical then
          CanMinimize := L.Count = 1
        else begin
          for I := 0 to L.Count - 1 do
            if TSpTBXCustomDockablePanel(L[I]).FState.DockedState = wsMinimized then
              Inc(MinimizedCount);
          CanMinimize := (L.Count > 1) and (L.Count - 1 > MinimizedCount);
        end;

        if CanMinimize then begin
          FIsManualSizing := True;
          try
            if Height > MinClientHeight then
              FState.RestoreSize := Height;
            if Options.TaskPaneStyleResize then
              RS := dprtMinimizeOrRestoreTaskPaneStyle
            else
              RS := dprtMinimizeOrRestore;
            if SpDPResize(Self, MinClientHeight, RS) then begin
              FState.DockedState := wsMinimized;
              FOptions.SetupButtonIcon(FOptions.MinimizeButton);
              FOptions.SetupButtonIcon(FOptions.MaximizeButton);
              Result := True;
            end;
          finally
            FIsManualSizing := False;
          end;
        end;

      finally
        L.Free;
      end;
    end;

  if Result then
    DoWindowStateChanged(wsMinimized);
end;

function TSpTBXCustomDockablePanel.Minimized: Boolean;
begin
  Result := FState.DockedState = wsMinimized;
end;

procedure TSpTBXCustomDockablePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Don't let the DP be dragged by the client area
  // Override TTBCustomDockableWindow.MouseDown
  if (Button <> mbLeft) or not IsMovable then
    inherited
  else
    if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSpTBXCustomDockablePanel.Resize;
var
  TotalBorderSize: Integer;
begin
  inherited;

  if Floating and Assigned(Parent) then begin
    // When Floating the caption panel is always horizontal
    // Make sure to calculate the floating form constraints taking into
    // account the borders and the close button.
    TotalBorderSize := GetFloatingBorderSize.Y * 2;
    Parent.Constraints.MinWidth := 20 + TotalBorderSize;
    Parent.Constraints.MinHeight := MinClientHeight + TotalBorderSize;
    if (FState.DockedState = wsMinimized) and (Parent.ClientHeight > MinClientHeight) then begin
      FState.DockedState := wsNormal;
      FOptions.SetupButtonIcon(FOptions.MinimizeButton);
    end;
  end
  else begin
    if (FState.DockedState = wsMinimized) and (ClientHeight > MinClientHeight) then begin
      FState.DockedState := wsNormal;
      FOptions.SetupButtonIcon(FOptions.MinimizeButton);
    end;
  end;
end;

function TSpTBXCustomDockablePanel.Restore: Boolean;
var
  I: Integer;
  DkPanel: TSpTBXCustomDockablePanel;
  RS: TSpTBXDPResizeType;
begin
  Result := False;
  FIsManualSizing := True;
  try
    if Floating then begin
      if FState.DockedState = wsMinimized then begin
        FState.DockedState := wsNormal;
        Parent.Height := FState.RestoreSize;
        Result := True;
      end;
    end
    else
      if Docked then begin
        case FState.DockedState of
          wsNormal: ;
          wsMinimized:
            begin
              if Options.TaskPaneStyleResize then
                RS := dprtMinimizeOrRestoreTaskPaneStyle
              else
                RS := dprtMinimizeOrRestore;
              Result := SpDPResize(Self, FState.RestoreSize, RS);
              if Result then begin
                FState.DockedState := wsNormal;

                // If a sibling was Maximized restore it
                if Assigned(CurrentDock) then begin
                  for I := 0 to CurrentDock.ToolbarCount - 1 do
                    if (CurrentDock.Toolbars[I] <> Self) and (CurrentDock.Toolbars[I] is TSpTBXCustomDockablePanel) then begin
                      DkPanel := TSpTBXCustomDockablePanel(CurrentDock.Toolbars[I]);
                      if DkPanel.Maximized then begin
                        DkPanel.FState.DockedState := wsNormal;
                        DkPanel.Options.SetupButtonIcon(DkPanel.Options.MaximizeButton);
                        Break;
                      end;
                    end;
                end;
              end;
            end;
          wsMaximized:
            begin
              Result := InternalMaximize(True);
              if Result then
                FState.DockedState := wsNormal;
            end;
        end;
      end;

    FOptions.SetupButtonIcon(FOptions.MinimizeButton);
    FOptions.SetupButtonIcon(FOptions.MaximizeButton);
  finally
    FIsManualSizing := False;
  end;

  if Result then
    DoWindowStateChanged(wsNormal);
end;

procedure TSpTBXCustomDockablePanel.AddDockForm(const Form: TTBCustomForm);
begin
  if Assigned(Form) and AddToList(FDockForms, Form) then
    Form.FreeNotification(Self);
end;

procedure TSpTBXCustomDockablePanel.RemoveDockForm(const Form: TTBCustomForm);
begin
  RemoveFromList(FDockForms, Form);
end;

procedure TSpTBXCustomDockablePanel.BeginDockedMoving;

  function DockDPOnMultiDock(DockList: TList; CursorPos: TPoint): Boolean;
  var
    MultiDock: TSpTBXCustomMultiDock;
    DP: TSpTBXCustomDockablePanel;
  begin
    Result := False;
    MultiDock := SpPtInMultiDock(CursorPos, DockList);
    if Assigned(MultiDock) then begin
      Result := True;
      MultiDock.DoRequestDock(MultiDock, Self, Result);
    end;

    if Result then begin
      // Dock the DP if the cursor is over a valid dock
      DP := SpPtInDP(CursorPos, MultiDock, False);
      if Assigned(DP) then begin
        if MultiDock.IsVertical then
          DockPos := DP.EffectiveDockPos + (DP.Height) div 2
        else
          DockPos := DP.EffectiveDockPos + (DP.Width) div 2;
      end;
      CurrentDock := MultiDock;
      MultiDock.ArrangeToolbars; // Needed to rearrange fixed sized DPs
    end
  end;

  procedure MouseMoved(DockList: TList; ClientClickPos: TPoint;
    OldCursor: HCURSOR; PreventDocking: Boolean; var OldCursorPos: TPoint);
  var
    CursorPos, Delta: TPoint;
    R, TitleBarR, FloatR1, FloatR2: TRect;
    MultiDock: TSpTBXCustomMultiDock;
    DP: TSpTBXCustomDockablePanel;
    FloatingW: Integer;
  begin
    GetCursorPos(CursorPos);
    if (CursorPos.X = OldCursorPos.X) and (CursorPos.Y = OldCursorPos.Y) then Exit;

    SetCursor(OldCursor);

    if Docked and (CurrentDock is TSpTBXCustomMultiDock) then begin
      GetWindowRect(CurrentDock.Handle, R);
      if PtInRect(R, CursorPos) then begin
        MultiDock := TSpTBXCustomMultiDock(CurrentDock);
        DP := SpPtInDP(CursorPos, MultiDock, True);
        if Assigned(DP) then begin
          // The cursor is over another dockable window, swap the pos
          SpDPSwapPos(MultiDock, Self, DP);
        end;
      end
      else begin
        // Change the cursor if it can't float
        if DockMode = dmCanFloat then begin
          // The cursor is outside the Dock, make the DP float
          // Position the DP at the center of the clicked point
          if FFloatingClientWidth > 0 then
            FloatingW := FFloatingClientWidth
          else
            FloatingW := ClientAreaWidth;
          FloatingPosition := Point(CursorPos.X - (FloatingW div 2), CursorPos.Y - 10);
          Floating := True;
          MoveOnScreen(True);
        end
        else begin
          SetCursor(LoadCursor(0, IDC_NO));
          if DockMode = dmCannotFloat then
            // The DP can't float but can be re-docked on a different MultiDock
            DockDPOnMultiDock(DockList, CursorPos);
        end;
      end;
      OldCursorPos := CursorPos;
    end
    else
      if Floating then
        if DockMode <> dmCanFloat then
          SetCursor(LoadCursor(0, IDC_NO))
        else begin
          // Clip the point so it doesn't get dragged under the taskbar
          R := GetRectOfMonitorContainingPoint(CursorPos, True);
          if CursorPos.X < R.Left then CursorPos.X := R.Left;
          if CursorPos.X > R.Right then CursorPos.X := R.Right;
          if CursorPos.Y < R.Top then CursorPos.Y := R.Top;
          if CursorPos.Y > R.Bottom then CursorPos.Y := R.Bottom;

          // Try to dock it on a MultiDock
          if not PreventDocking and DockDPOnMultiDock(DockList, CursorPos) then
            OldCursorPos := CursorPos
          else begin
            Delta := Point(CursorPos.X - OldCursorPos.X, CursorPos.Y - OldCursorPos.Y);

            // Make sure the TitleBar is still accessible if it's dragged almost
            // completely off the screen so it can be dragged back.
            GetWindowRect(FToolbar.Handle, TitleBarR);
            OffsetRect(TitleBarR, Delta.X, Delta.Y);
            with GetFloatingBorderSize do
              InflateRect(TitleBarR, -X, -Y);
            if TitleBarR.Right < R.Left then Delta.X := 0;
            if TitleBarR.Left > R.Right then Delta.X := 0;
            if TitleBarR.Bottom < R.Top then Delta.Y := 0;
            if TitleBarR.Top > R.Bottom then Delta.Y := 0;

            // Move the floating DP if it's still floating
            GetWindowRect(Parent.Handle, FloatR1);
            FloatingPosition := Point(Parent.Left + Delta.X, Parent.Top + Delta.Y);
            GetWindowRect(Parent.Handle, FloatR2);

            // Don't change OldCursorPos if the floating DP wasn't moved
            if not EqualRect(FloatR1, FloatR2) then begin
              if FloatR1.Left <> FloatR2.Left then
                OldCursorPos.X := CursorPos.X;
              if FloatR1.Top <> FloatR2.Top then
                OldCursorPos.Y := CursorPos.Y;
            end;
          end;
        end;
  end;

var
  L: TList;
  ClientClickPos, OldCursorPos: TPoint;
  OldCursor: HCURSOR;
  PreventDocking: Boolean;
  Msg: TMsg;
begin
  L := TList.Create;
  FIsDockedMoving := True;
  try
    OldCursor := GetCursor; // Save the original mouse cursor

    SpDPGetDockableMultiDockList(Self, L);

    SetCapture(Handle);
    GetCursorPos(OldCursorPos);
    ClientClickPos := ScreenToClient(OldCursorPos);
    PreventDocking := GetKeyState(VK_CONTROL) < 0;

    while GetCapture = Handle do begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break; // if GetMessage failed
        0: begin
             // Repost WM_QUIT messages
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP:
          if (Msg.wParam = VK_CONTROL) and (PreventDocking <> (Msg.Message = WM_KEYDOWN)) then begin
            PreventDocking := Msg.Message = WM_KEYDOWN;
            MouseMoved(L, ClientClickPos, OldCursor, PreventDocking, OldCursorPos);
          end
          else
            if Msg.wParam = VK_ESCAPE then Break;
        WM_MOUSEMOVE:
          MouseMoved(L, ClientClickPos, OldCursor, PreventDocking, OldCursorPos);
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
          Break;
        WM_LBUTTONUP:
          Break;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK: ;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;

  finally
    if GetCapture = Handle then
      ReleaseCapture;
    FIsDockedMoving := False;
    L.Free;
  end;
end;

procedure TSpTBXCustomDockablePanel.BeginSplitResizing(HitTest: Integer);

  procedure MouseMoved(DP: TSpTBXCustomDockablePanel; VerticalSplitting: Boolean;
    OldCursorPos: TPoint; OldSize: Integer);
  var
    CursorPos: TPoint;
    Delta: Integer;
  begin
    GetCursorPos(CursorPos);
    if VerticalSplitting then
      Delta := CursorPos.Y - OldCursorPos.Y
    else
      Delta := CursorPos.X - OldCursorPos.X;
    SpDPResize(DP, OldSize + Delta, dprtSplitResize);
  end;

var
  M: TSpTBXCustomMultiDock;
  L: TList;
  VerticalSplitting: Boolean;
  OldSize, DockIndex: Integer;
  OldCursorPos: TPoint;
  Msg: TMsg;
  EffectivePanel: TSpTBXCustomDockablePanel;
  Form: TCustomForm;
begin
  if not (CurrentDock is TSpTBXCustomMultiDock) then Exit;
  M := TSpTBXCustomMultiDock(CurrentDock);

  // Get the EffectivePanel
  EffectivePanel := Self;
  VerticalSplitting := False;
  case HitTest of
    HT_DP_SPLITRESIZELEFT, HT_DP_SPLITRESIZETOP:
      begin
        // If we are grabbing the left or top side of the DP the
        // EffectivePanel should be the previous sibling
        L := TList.Create;
        try
          M.GetDockablePanelDockIndex(L, Self, DockIndex);
          if DockIndex > 0 then
            EffectivePanel := TSpTBXCustomDockablePanel(L[DockIndex - 1]);
          if HitTest = HT_DP_SPLITRESIZETOP then
            VerticalSplitting := True;
        finally
          L.Free;
        end;
      end;
    HT_DP_SPLITRESIZEBOTTOM: VerticalSplitting := True;
    HT_DP_SPLITRESIZERIGHT: VerticalSplitting := False;
  end;

  try
    SetCapture(Handle);
    GetCursorPos(OldCursorPos);
    if VerticalSplitting then
      OldSize := EffectivePanel.Height
    else
      OldSize := EffectivePanel.Width;

    while GetCapture = Handle do begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break; { if GetMessage failed }
        0: begin
             { Repost WM_QUIT messages }
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP:
          if Msg.WParam = VK_ESCAPE then Break;
        WM_MOUSEMOVE:
          MouseMoved(EffectivePanel, VerticalSplitting, OldCursorPos, OldSize);
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
          Break;
        WM_LBUTTONUP:
          Break;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK: ;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;

  finally
    if GetCapture = Handle then
      ReleaseCapture;
    if csDesigning in ComponentState then begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.Designer <> nil) then
        Form.Designer.Modified;
    end;
  end;
end;

function TSpTBXCustomDockablePanel.CanSplitResize(EdgePosition: TTBDockPosition): Boolean;
var
  M: TSpTBXCustomMultiDock;
  L: TList;
begin
  Result := Docked and (CurrentDock is TSpTBXCustomMultiDock) and HandleAllocated;

  if not Result then Exit;

  M := TSpTBXCustomMultiDock(CurrentDock);

  L := TList.Create;
  try
    M.GetDockablePanelList(L);

    if M.IsVertical then begin
      case EdgePosition of
        dpTop: Result := EffectiveDockPos > 0;
        dpBottom: Result := L.Last <> Self;
      else
        Result := False;
      end;
    end
    else begin
      case EdgePosition of
        dpLeft: Result := EffectiveDockPos > 0;
        dpRight: Result := L.Last <> Self;
      else
        Result := False;
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomDockablePanel.ConstrainedResize(var MinWidth, MinHeight,
  MaxWidth, MaxHeight: Integer);
var
  Sz: TPoint;
  M: TSpTBXCustomMultiDock;
begin
  Sz := CalcNCSizes;

  if MinClientWidth > 0 then MinWidth := MinClientWidth + Sz.X;
  if MinClientHeight > 0 then MinHeight := MinClientHeight + Sz.Y;
  if MaxClientWidth > 0 then MaxWidth := MaxClientWidth + Sz.X;
  if MaxClientHeight > 0 then MaxHeight := MaxClientHeight + Sz.Y;

  // Disallow lateral Width change when the DP is docked
  if Docked and (CurrentDock is TSpTBXCustomMultiDock) then begin
    M := TSpTBXCustomMultiDock(CurrentDock);
    if M.IsVertical then begin
      if not M.UpdatingLateralSize then
        MinWidth := Width;
      if FFixedDockedSize and not FIsManualSizing and not (csDesigning in ComponentState) and not M.ReadingPositionData then begin
        MinHeight := Height;
        MaxHeight := Height;
      end;
    end
    else begin
      if not M.UpdatingLateralSize then
        MinHeight := Height;
      if FFixedDockedSize and not FIsManualSizing and not (csDesigning in ComponentState) and not M.ReadingPositionData then begin
        MinWidth := Width;
        MaxWidth := Width;
      end;
    end;
  end;
end;

procedure TSpTBXCustomDockablePanel.DockDrawBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  DefaultPainting: Boolean;
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault := False;

    if Docked and not Floating then begin
      // When the DP is not floating draws the CaptionBar borders on the NC area
      // of the DockablePanel, see InternalDrawBackground
      // Just draw 1 pixel from the left and right, and 2 pixels from the top
      // ARect.Bottom should remain the same
      if IsVerticalTitleBar then begin
        InflateRect(ARect, 0, 1);
        ARect.Left := ARect.Left - DockedBorderSize;
      end
      else begin
        InflateRect(ARect, 1, 0);
        ARect.Top := ARect.Top - DockedBorderSize;
      end;
    end;

    DefaultPainting := True;
    DoDrawCaptionPanel(ACanvas, ARect, pstPrePaint, DefaultPainting);
    if DefaultPainting then
      SpDrawXPDockablePanelTitleBar(ACanvas, ARect, True, IsVerticalTitleBar);
    DefaultPainting := True;
    DoDrawCaptionPanel(ACanvas, ARect, pstPostPaint, DefaultPainting);
  end;
end;

procedure TSpTBXCustomDockablePanel.ToolbarDrawBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  // Let the background be painted by the Dock
  if PaintStage = pstPrePaint then
    PaintDefault := False;
end;

procedure TSpTBXCustomDockablePanel.DockRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(FToolbar) then
    Accept := Bar = FToolbar;
end;

procedure TSpTBXCustomDockablePanel.DockResize(Sender: TObject);
begin
  if IsVerticalTitleBar then begin
    // If the Panel is left/right aligned
    if FPanel.Width <> FToolbarDock.Width then begin
      FPanel.Width := FToolbarDock.Width;
      if Floating and Assigned(Parent) then begin
        Parent.Constraints.MinWidth := FPanel.Width + GetFloatingBorderSize.X * 2;
        Parent.Constraints.MinHeight := 0;
      end;
    end;
    MinClientHeight := 0;
    if not Floating and not ShowCaptionWhenDocked then
      MinClientWidth := 1
    else
      MinClientWidth := FPanel.Width;
  end
  else begin
    if FPanel.Height <> FToolbarDock.Height then begin
      FPanel.Height := FToolbarDock.Height;
      if Floating and Assigned(Parent) then begin
        Parent.Constraints.MinWidth := 0;
        Parent.Constraints.MinHeight := FPanel.Height + GetFloatingBorderSize.Y * 2;
      end;
    end;
    MinClientWidth := 0;
    if not Floating and not ShowCaptionWhenDocked then
      MinClientHeight := 1
    else
      MinClientHeight := FPanel.Height;
  end;
end;

procedure TSpTBXCustomDockablePanel.DoDrawCaptionPanel(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawCaptionPanel) then FOnDrawCaptionPanel(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomDockablePanel.DoWindowStateChanged(AWindowState: TWindowState);
begin
  if Assigned(FOnWindowStateChanged) then FOnWindowStateChanged(Self, AWindowState);
end;

function TSpTBXCustomDockablePanel.GetCaptionPanelSize: TPoint;
begin
  Result := Point(FPanel.Width, FPanel.Height);
end;

procedure TSpTBXCustomDockablePanel.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  C: TControl;
  DesignerRootItem: TTBCustomItem;
begin
  // Needed to fake the DFM streaming system because the owner of the items
  // is the Form and not the Toolbar nor Self.
  // But the parent must be the Toolbar.
  // GetChildren is used to pass the children components of Self to the DFM
  // streaming system.
  // We also need to do the same with the controls of TTBControlItems.
  // More info on the Delphi help or Classes.TWriter.WriteData
  // Same as TSpTBXCompoundItemsControl and TSpTBXCustomDockablePanel

  DesignerRootItem := GetItems;
  TTBCustomItemAccess(DesignerRootItem).GetChildren(Proc, Root);
  for I := 0 to DesignerRootItem.Count - 1 do
    if (DesignerRootItem[I] is TTBControlItem) then begin
      C := TTBControlItem(DesignerRootItem[I]).Control;
      if Assigned(C) then
        if SpFindControl(Self, C) = -1 then Proc(C);
    end;
  inherited;
end;

function TSpTBXCustomDockablePanel.GetImages: TCustomImageList;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Images
  else
    Result := nil;
end;

function TSpTBXCustomDockablePanel.GetItems: TTBCustomItem;
begin
  // The ToolbarEditor designer will open the editable items and
  // not the Toolbar.Items
  Result := Options.EditableItems;
end;

function TSpTBXCustomDockablePanel.GetRootItems: TTBRootItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXCustomDockablePanel.GetToolbar: TSpTBXToolbar;
begin
  Result := FToolbar;
end;

function TSpTBXCustomDockablePanel.GetView: TTBToolbarView;
begin
  Result := FToolbar.View;
end;

function TSpTBXCustomDockablePanel.GetEffectiveHeight: Integer;
begin
  Result := Height;
end;

function TSpTBXCustomDockablePanel.GetEffectiveWidth: Integer;
begin
  Result := Width;
end;

function TSpTBXCustomDockablePanel.GetFloatingClientHeight: Integer;
begin
  if Docked then
    Result := FFloatingClientHeight
  else
    Result := ClientAreaHeight;
end;

function TSpTBXCustomDockablePanel.GetFloatingClientWidth: Integer;
begin
  if Docked then
    Result := FFloatingClientWidth
  else
    Result := ClientAreaWidth;
end;

procedure TSpTBXCustomDockablePanel.InternalDrawBackground(ACanvas: TCanvas;
  ARect: TRect; PaintOnNCArea, PaintBorders: Boolean);
var
  DefaultPainting: Boolean;
begin
  if Color = clNone then
    SpDrawXPDockablePanelBody(ACanvas, ARect, True, Floating)
  else begin
    ACanvas.Brush.Color := Color;
    ACanvas.FillRect(ARect);
  end;

  if PaintOnNCArea and FPanel.Visible then begin
    // Draw the CaptionBar borders on the NC Area of the embedded Dock
    // See DockDrawBackground.
    // Just draw 1 pixel from the left and right, and 2 pixels from the top
    // ARect.Top should remain the same
    if IsVerticalTitleBar then begin
      ARect.Right := ARect.Left + CaptionPanelSize.X + 2;
      InflateRect(ARect, 0, -(DockedBorderSize - 1));
    end
    else begin
      ARect.Bottom := ARect.Top + CaptionPanelSize.Y + 2;
      InflateRect(ARect, -(DockedBorderSize - 1), 0);
    end;

    DefaultPainting := True;
    DoDrawCaptionPanel(ACanvas, ARect, pstPrePaint, DefaultPainting);
    if DefaultPainting then
      SpDrawXPDockablePanelTitleBar(ACanvas, ARect, True, IsVerticalTitleBar);
    DefaultPainting := True;
    DoDrawCaptionPanel(ACanvas, ARect, pstPostPaint, DefaultPainting);
  end;
end;

function TSpTBXCustomDockablePanel.InternalMaximize(Restore: Boolean): Boolean;
// Resize the dockable panel to the maximum size, and minimize the rest of
// the panels
// Horizontal resizing is not supported.
var
  I: Integer;
  L, PrevRestoreSize: TList;
  DP: TSpTBXCustomDockablePanel;
  MultiDock: TSpTBXCustomMultiDock;
begin
  Result := False;
  if not Docked or not (CurrentDock is TSpTBXCustomMultiDock) or (CurrentDock.Position in [dpTop, dpBottom]) then Exit;
  MultiDock := TSpTBXCustomMultiDock(CurrentDock);

  L := TList.Create;
  PrevRestoreSize := TList.Create;
  try
    MultiDock.GetDockablePanelList(L);
    if L.Count < 2 then Exit;

    if Restore then begin
      // Restore the minimized DPs, from down-to-top
      for I := L.Count - 1 downto 0 do
        if L[I] <> Self then begin
          DP := TSpTBXCustomDockablePanel(L[I]);
          if DP.Minimized then begin
            DP.FIsManualSizing := True;
            if SpDPResize(DP, DP.FState.RestoreSize, dprtMinimizeOrRestore) then
              PrevRestoreSize.Add(DP);
            DP.FIsManualSizing := False;
          end;
        end;
      // Now set the DockedState
      for I := 0 to PrevRestoreSize.Count - 1 do begin
        DP := TSpTBXCustomDockablePanel(PrevRestoreSize[I]);
        DP.FState.DockedState := wsNormal;
        DP.Options.SetupButtonIcon(DP.Options.MinimizeButton);
        DP.Options.SetupButtonIcon(DP.Options.MaximizeButton);
      end;
    end
    else begin
      // Remember the previous Height of the DPs
      for I := 0 to L.Count - 1 do begin
        DP := TSpTBXCustomDockablePanel(L[I]);
        PrevRestoreSize.Add(Pointer(DP.Height));
      end;
      // Minimize the DPs
      for I := 0 to L.Count - 1 do
        if L[I] <> Self then begin
          DP := TSpTBXCustomDockablePanel(L[I]);
          if not DP.Minimized then begin
            if not DP.Maximized then begin
              DP.FState.RestoreSize := Integer(PrevRestoreSize[I]);
            end;
            DP.FIsManualSizing := True;
            SpDPResize(DP, DP.CaptionPanelSize.Y, dprtMinimizeOrRestore);
            DP.FIsManualSizing := False;
            DP.FState.DockedState := wsMinimized;
            DP.Options.SetupButtonIcon(DP.Options.MinimizeButton);
            DP.Options.SetupButtonIcon(DP.Options.MaximizeButton);
          end;
        end;
    end;

    Result := True;
  finally
    L.Free;
    PrevRestoreSize.Free;
  end;
end;

procedure TSpTBXCustomDockablePanel.InvalidateBackground(InvalidateChildren: Boolean);
begin
  SpInvalidateSpTBXControl(Self, True, True);
end;

function TSpTBXCustomDockablePanel.IsResizable: Boolean;
var
  R: TRect;
begin
  Result := False;
  if FState.DockedState <> wsMinimized then begin
    R := Rect(1, 1, 0, 0);
    ConstrainedResize(R.Left, R.Top, R.Right, R.Bottom);
    Result := (R.Top <> R.Bottom) and (R.Left <> R.Right);
  end;
end;

function TSpTBXCustomDockablePanel.IsVerticalTitleBar: Boolean;
begin
  Result := FPanel.Align in [alLeft, alRight];
end;

procedure TSpTBXCustomDockablePanel.DoneReadingPositionData(const Data: TTBReadPositionData);
begin
  inherited;

  // Special case when it's floating and minimized force the state
  if Floating and (FLoadedState = wsMinimized) then begin
    FState.DockedState := FLoadedState;
    Parent.ClientHeight := FPanel.Height;
  end;

  // Update buttons state
  FOptions.SetupButtonIcon(FOptions.MinimizeButton);
  FOptions.SetupButtonIcon(FOptions.MaximizeButton);
end;

procedure TSpTBXCustomDockablePanel.ReadPositionData(const Data: TTBReadPositionData);
begin
  inherited;

  // Load FLoadedBarSize and FLoadedDockPos
  FLoadedBarSize.cx := ClientAreaWidth;
  FLoadedBarSize.cy := ClientAreaHeight;
  FLoadedDockPos := DockPos;

  // Load FloatingClientWidth/FloatingClientHeight, RestoreSize, State
  with Data do begin
    FFloatingClientWidth := ReadIntProc(Name, rvFloatingClientWidth, 0, ExtraData);
    FFloatingClientHeight := ReadIntProc(Name, rvFloatingClientHeight, 0, ExtraData);
    FState.RestoreSize := ReadIntProc(Name, rvRestoreSize, 0, ExtraData);
    FState.DockedState := TWindowState(ReadIntProc(Name, rvState, 0, ExtraData));
    FLoadedState := FState.DockedState;
  end;
end;

procedure TSpTBXCustomDockablePanel.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;

  // Save FloatingClientWidth/FloatingClientHeight, RestoreSize, State
  with Data do begin
    WriteIntProc(Name, rvFloatingClientWidth, FFloatingClientWidth, ExtraData);
    WriteIntProc(Name, rvFloatingClientHeight, FFloatingClientHeight, ExtraData);
    WriteIntProc(Name, rvRestoreSize, FState.RestoreSize, ExtraData);
    WriteIntProc(Name, rvState, Integer(FState.DockedState), ExtraData);
  end;
end;

procedure TSpTBXCustomDockablePanel.SetDefaultDockedSize(Value: Integer);
begin
  if FDefaultDockedSize <> Value then
    FDefaultDockedSize := Value;
end;

procedure TSpTBXCustomDockablePanel.SetEffectiveHeight(const Value: Integer);
begin
  if Docked and IsVertical and (CurrentDock is TSpTBXCustomMultiDock) then
    SpDPResize(Self, Value)
  else
    Height := Value;
end;

procedure TSpTBXCustomDockablePanel.SetEffectiveWidth(const Value: Integer);
begin
  if Docked and not IsVertical and (CurrentDock is TSpTBXCustomMultiDock) then
    SpDPResize(Self, Value)
  else
    Width := Value;
end;

procedure TSpTBXCustomDockablePanel.SetFloatingClientHeight(const Value: Integer);
begin
  if Docked then
    FFloatingClientHeight := Value
  else
    ClientAreaHeight := Value;
end;

procedure TSpTBXCustomDockablePanel.SetFloatingClientWidth(const Value: Integer);
begin
  if Docked then
    FFloatingClientWidth := Value
  else
    ClientAreaWidth := Value;
end;

procedure TSpTBXCustomDockablePanel.SetImages(const Value: TCustomImageList);
begin
  if Assigned(FToolbar) then
    FToolbar.Images := Value;
end;

procedure TSpTBXCustomDockablePanel.SetParent(AParent: TWinControl);
var
  ToDock, ToFloating, ToEmptyMultiDock, WasMinimized, DockingByCode: Boolean;
  PrevSize: TSize;
  D: TTBDock;
  Splitter: TSpTBXCustomSplitter;
begin
  if not (csDestroying in ComponentState) and Assigned(Parent) and Assigned(AParent) and
    not ((csLoading in ComponentState) and (AParent = Parent)) then
  begin
    ToDock := AParent is TTBDock;
    ToFloating := AParent is TTBFloatingWindowParent;
    ToEmptyMultiDock := False;
    WasMinimized := Minimized;
    DockingByCode := DockPos < 0;

    if ToDock then FPanel.Visible := FShowCaptionWhenDocked
    else FPanel.Visible := FShowCaption;

    PrevSize.cx := ClientAreaWidth;
    PrevSize.cy := ClientAreaHeight;

    if Floating then begin
      FFloatingClientWidth := PrevSize.cx;
      FFloatingClientHeight := PrevSize.cy;
    end;

    if ToDock then begin
      // [DockablePanel-Rule]
      // When a floating DP is re-docked the DP width should be the same
      // as the rest of the DPs that are present on the MultiDock.
      // If the MultiDock is empty then the size should be DefaultDockedSize,
      // and if DefaultDockedSize is 0 use the previous size.
      D := TTBDock(AParent);
      if D is TSpTBXCustomMultiDock then begin
        ToEmptyMultiDock := D.ToolbarCount = 0;
        if D.Position in [dpLeft, dpRight] then begin
          if ToEmptyMultiDock then begin
            if FDefaultDockedSize > 0 then EffectiveWidth := FDefaultDockedSize;
          end
          else begin
            if not Docked then // If it's not docked compute the borders
              EffectiveWidth := D.ClientWidth - (DockedBorderSize * 2)
            else
              EffectiveWidth := D.ClientWidth;
            // Append the DP to the bottom if it's being docked by code
            if DockingByCode then begin
              DockPos := D.ClientHeight;
              TSpTBXCustomMultiDock(D).UpdateDockablePanelsDockPos;
            end;
          end;
        end
        else begin
          if ToEmptyMultiDock then begin
            if FDefaultDockedSize > 0 then EffectiveHeight := FDefaultDockedSize;
          end
          else begin
            if not Docked then // If it's not docked compute the borders
              EffectiveHeight := D.ClientHeight - (DockedBorderSize * 2)
            else
              EffectiveHeight := D.ClientHeight;
            // Append the DP to the bottom if it's being docked by code
            if DockingByCode then begin
              DockPos := D.ClientWidth;
              TSpTBXCustomMultiDock(D).UpdateDockablePanelsDockPos;
            end;
          end;
        end;
      end;
    end;

    inherited;

    if ToEmptyMultiDock then begin
      Splitter := TSpTBXMultiDock(AParent).GetAdjacentSplitter;
      if Assigned(Splitter) then
        TSpTBXMultiDock(AParent).InsertingOnEmptyDock(Splitter); // Re-align adjacent splitter
    end
    else
      if ToDock and DockingByCode then begin
        D := TTBDock(AParent);
        if D is TSpTBXCustomMultiDock then
          if D.Position in [dpLeft, dpRight] then
            SpDPResize(Self, PrevSize.cy, dprtAppendResize)
          else
            SpDPResize(Self, PrevSize.cx, dprtAppendResize);
      end
      else
        if ToFloating then begin
          // [DockablePanel-Rule]
          // Remember the previous floating size
          if FFloatingClientWidth > 0 then
            ClientAreaWidth := FFloatingClientWidth
          else
            ClientAreaWidth := PrevSize.cx;
          if FFloatingClientHeight > 0 then
            ClientAreaHeight := FFloatingClientHeight
          else
            // [DockablePanel-Rule]
            // If the previous floating size is not valid and the DP was minimized
            // when it was undocked then use the Restore size.
            if WasMinimized then
              ClientAreaHeight := FState.RestoreSize
            else
              ClientAreaHeight := PrevSize.cy;

          if Assigned(Parent) and (Parent is TSpTBXFloatingWindowParent) then begin
            Parent.Constraints.MinHeight := FPanel.Height + GetFloatingBorderSize.Y * 2;
            TSpTBXFloatingWindowParent(Parent).CloseOnAltF4 := True;
          end;
        end;

    UpdateTitleBarRotation; // Update the rotation of the titlebar
  end
  else
    inherited;
end;

procedure TSpTBXCustomDockablePanel.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    if not Docked then begin
      FPanel.Visible := Value;
      RedrawWindow(Parent.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
  end;
end;

procedure TSpTBXCustomDockablePanel.SetShowCaptionWhenDocked(const Value: Boolean);
begin
  if FShowCaptionWhenDocked <> Value then begin
    FShowCaptionWhenDocked := Value;
    if not Floating then begin
      FPanel.Visible := Value;
      DockResize(nil);  // Resize and update MinClientWidth/Height
      // The panel can't be hidden at designtime, move it outside the client area
      if (csDesigning in ComponentState) then
        if Value then
          FPanel.Align := alTop
        else begin
          FPanel.Align := alNone;
          FPanel.Top := FPanel.Top - FPanel.Height - 60;
        end;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
  end;
end;

procedure TSpTBXCustomDockablePanel.SetShowVerticalCaption(const Value: Boolean);
begin
  if FShowVerticalCaption <> Value then begin
    FShowVerticalCaption := Value;
    UpdateTitleBarRotation;
  end;
end;

function TSpTBXCustomDockablePanel.SizeToggle(ToMaximize: Boolean): Boolean;
begin
  if (Minimized and not ToMaximize) or (Maximized and ToMaximize) then
    Result := Restore
  else
    if ToMaximize then Result := Maximize
    else Result := Minimize;
end;

procedure TSpTBXCustomDockablePanel.UpdateTitleBarRotation;
begin
  if not HandleAllocated then Exit;
  
  if FShowVerticalCaption and not (Floating or IsVertical) then begin
    if not IsVerticalTitleBar then begin
      // TTBDock doesn't allow us to change the position when there are
      // docked toolbars, we have to undock the toolbar
      FToolbar.Visible := False;
      FToolbar.Floating := True;
      FToolbarDock.Position := dpLeft;
      FPanel.Align := alLeft;
      FToolbar.CurrentDock := FToolbarDock;
      FToolbar.Visible := True;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
  end
  else begin
    if IsVerticalTitleBar then begin
      // TTBDock doesn't allow us to change the position when there are
      // docked toolbars, we have to undock the toolbar
      FToolbar.Visible := False;
      FToolbar.Floating := True;
      FToolbarDock.Position := dpTop;
      FPanel.Align := alTop;
      FToolbar.CurrentDock := FToolbarDock;
      FToolbar.Visible := True;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
  end;

  // Update buttons glyphs
  FOptions.SetupButtonIcon(FOptions.MinimizeButton);
  FOptions.SetupButtonIcon(FOptions.MaximizeButton);
end;

procedure TSpTBXCustomDockablePanel.ValidateContainer(AComponent: TComponent);
begin
  inherited;
  if (AComponent is TTBDock) and not (AComponent is TSpTBXCustomMultiDock) then
    raise EInvalidOperation.CreateFmt('Cannot insert %s into %s. Place it on a MultiDock instead', [Self.ClassName, AComponent.ClassName]);
end;

procedure TSpTBXCustomDockablePanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FOptions) then
    FOptions.CaptionLabel := Caption;
end;

procedure TSpTBXCustomDockablePanel.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
  if Docked then
    with Message.CalcSize_Params^ do
      InflateRect(rgrc[0], -DockedBorderSize, -DockedBorderSize);
end;

procedure TSpTBXCustomDockablePanel.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;

  if Docked then begin
    P := SmallPointToPoint(Message.Pos);
    GetWindowRect(Handle, R);

    if IsVertical then begin
      if (P.Y >= R.Bottom - DockedBorderSize) and CanSplitResize(dpBottom) then
        Message.Result := HT_DP_SPLITRESIZEBOTTOM
      else if (P.Y <= R.Top + DockedBorderSize) and CanSplitResize(dpTop) then
        Message.Result := HT_DP_SPLITRESIZETOP;
    end
    else begin
      if (P.X >= R.Right - DockedBorderSize) and CanSplitResize(dpRight) then Message.Result := HT_DP_SPLITRESIZERIGHT
      else if (P.X <= R.Left + DockedBorderSize) and CanSplitResize(dpLeft) then Message.Result := HT_DP_SPLITRESIZELEFT;
    end;
  end;
end;

procedure TSpTBXCustomDockablePanel.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  OldCursor: HCURSOR;
begin
  if Message.HitTest in [HT_DP_SPLITRESIZELEFT..HT_DP_SPLITRESIZEBOTTOM] then
    BeginSplitResizing(Message.HitTest)
  else
    if (Message.HitTest = HT_TB2k_Border) and IsMovable then begin
      FIsDockedMoving := True;
      OldCursor := SetCursor(LoadCursor(0, IDC_SIZEALL));
      try
        // To prevent resizing when clicking on the CaptionPanel:
        // Instead of calling inherited call BeginDockedMoving
        // that mimics TTBCustomDockableWindow.BeginMoving.
        // The new method should only change position and change dock/floating
        // it won't resize the DP.
        BeginDockedMoving;
      finally
        SetCursor(OldCursor);
        FIsDockedMoving := False;
      end;
    end
    else
      inherited;
end;

procedure TSpTBXCustomDockablePanel.WMSetCursor(var Message: TWMSetCursor);
begin
  if Docked and CurrentDock.AllowDrag and (Message.CursorWnd = WindowHandle) then begin
    case Message.HitTest of
      HT_DP_SPLITRESIZELEFT, HT_DP_SPLITRESIZERIGHT:
        begin
          SetCursor(LoadCursor(0, IDC_SIZEWE));
          Message.Result := 1;
          Exit;
        end;
      HT_DP_SPLITRESIZETOP, HT_DP_SPLITRESIZEBOTTOM:
        begin
          SetCursor(LoadCursor(0, IDC_SIZENS));
          Message.Result := 1;
          Exit;
        end;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSplitter }

constructor TSpTBXCustomSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  Width := 5;
  Height := 100;
  Align := alLeft;
  Cursor := crSizeWE;
  FResizeStyle := rsUpdate;
  FOldSize := -1;
  FMinSize := 0;
  FGripSize := 50;
  FGripHotTrack := True;
  FAutoCalcMaxSize := True;
  FRestorePos := 60;

  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXCustomSplitter.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  FreeAndNil(FMouseBrush);
  inherited Destroy;
end;

procedure TSpTBXCustomSplitter.RequestAlign;
begin
  inherited;
  if Align in [alTop, alBottom] then Cursor:= crSizeNS
  else Cursor:= crSizeWE;
end;

function TSpTBXCustomSplitter.IsVertical: Boolean;
begin
  Result := not (Align in [alTop, alBottom]);
end;

function TSpTBXCustomSplitter.ValidateSplitControl: TControl;
// Find the control that the Splitter must resize
var
  P: TPoint;
  I: Integer;
  R, RMargins: TRect;
  C, ZeroSized: TControl;
  bAlignWithMargins: Boolean;
begin
  Result := nil;
  ZeroSized := nil;

  P := Point(Left, Top);
  RMargins := Rect(0, 0, 0, 0);
  {$IF CompilerVersion > 15}
  bAlignWithMargins := AlignWithMargins;
  if bAlignWithMargins then
    RMargins := Rect(Margins.Left, Margins.Top, Margins.Right, Margins.Bottom);
  {$ELSE}
  bAlignWithMargins := False;
  {$IFEND}

  // Fix from QC#68209, when the Splitter has bAlignWithMargins set to true
  case Align of
    alLeft:
      if not bAlignWithMargins then
        Dec(P.X)
      else
        Dec(P.X, RMargins.Left + 1);
    alRight:
      if not bAlignWithMargins then
        Inc(P.X, Width)
      else
        Inc(P.X, Width + RMargins.Right + 1);
    alTop:
      if not bAlignWithMargins then
        Dec(P.Y)
      else
        Dec(P.Y, RMargins.Top + 1);
    alBottom:
      if not bAlignWithMargins then
        Inc(P.Y, Height)
      else
        Inc(P.Y, Height + RMargins.Bottom + 1);
  else
    Exit;
  end;

  // Try to find the Split Control
  for I := 0 to Parent.ControlCount - 1 do begin
    C := Parent.Controls[I];
    // Can't be a StatusBar, a Splitter or a regular Toolbar Dock
    if C.Visible and (C.Align = Align) and
      not ((C is TSpTBXCustomStatusBar) or (C is TCustomStatusBar)) and
      not ((C is TSpTBXCustomSplitter) or (C is TSplitter)) and
      not ((C is TTBDock) and not (C is TSpTBXCustomMultiDock)) then
    begin
      R := C.BoundsRect;
      // Fix from QC#68209, when the control has bAlignWithMargins set to true
      {$IF CompilerVersion > 15}
      if C.AlignWithMargins then
      begin
        R.Left   := R.Left - C.Margins.Left;
        R.Right  := R.Right + C.Margins.Right;
        R.Top    := R.Top - C.Margins.Top;
        R.Bottom := R.Bottom + C.Margins.Bottom;
      end;
      {$IFEND}

      case Align of
        alLeft, alRight:
          if (R.Right - R.Left) = 0 then begin
            if (R.Left = Left - RMargins.Left) or (R.Left = Left + Width + RMargins.Right) then
              ZeroSized := C;
          end;
        alTop, alBottom:
          if (R.Bottom - R.Top) = 0 then begin
            if (R.Top = Top - RMargins.Top) or (R.Top = Top + Height + RMargins.Bottom) then
              ZeroSized := C;
          end;
      end;

      if (Result = nil) and PtInRect(R, P) then
        Result := C;
    end;
  end;

  // Zero sized control has the priority
  if Assigned(ZeroSized) then
    Result := ZeroSized;

  // Don't try to resize an empty MultiDock
  if Assigned(Result) and (Result is TSpTBXCustomMultiDock) then
    if TSpTBXCustomMultiDock(Result).ToolbarCount = 0 then begin
      Result := nil;
      Exit;
    end;
end;

procedure TSpTBXCustomSplitter.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSpTBXCustomSplitter.WMSpSkinChange(var Message: TMessage);
begin
  Invalidate;
end;

procedure TSpTBXCustomSplitter.MouseAllocateLineDC;
begin
  FMouseLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then begin
    if FMouseBrush = nil then begin
      FMouseBrush := TBrush.Create;
      FMouseBrush.Bitmap:= AllocPatternBitmap(clBlack,clWhite);
    end;
    FMousePrevBrush := SelectObject(FMouseLineDC, FMouseBrush.Handle);
  end;
end;

procedure TSpTBXCustomSplitter.MouseReleaseLineDC;
begin
  if FMousePrevBrush <> 0 then SelectObject(FMouseLineDC, FMousePrevBrush);
  ReleaseDC(Parent.Handle, FMouseLineDC);
  if FMouseBrush <> nil then
    FreeAndNil(FMouseBrush);
end;

procedure TSpTBXCustomSplitter.MouseDrawLine;
var
  P: TPoint;
begin
  FMouseLineVisible := not FMouseLineVisible;
  P := Point(Left, Top);
  if IsVertical then
    P.X := Left + FSplitLinePaintingPos
  else
    P.Y := Top + FSplitLinePaintingPos;
  PatBlt(FMouseLineDC, P.X, P.Y, Width, Height, PATINVERT);
end;

procedure TSpTBXCustomSplitter.MouseCalcSplitSize(X, Y: Integer; var NewSize,
  Split: Integer);
var
  I: Integer;
begin
  if Assigned(FMouseSplitControl) then begin
    if IsVertical then
      Split := X - FMouseDownPos.X
    else
      Split := Y - FMouseDownPos.Y;

    I := 0;
    case Align of
      alLeft:   I := FMouseSplitControl.Width + Split;
      alRight:  I := FMouseSplitControl.Width - Split;
      alTop:    I := FMouseSplitControl.Height + Split;
      alBottom: I := FMouseSplitControl.Height - Split;
    end;

    NewSize := I;
    if I < FMinSize then
      NewSize := FMinSize
    else
      if AutoCalcMaxSize and (I > FMaxSize) then
        NewSize := FMaxSize;  // Use the Maximum Size

    if I <> NewSize then begin
      if Align in [alRight, alBottom] then
        I := I - NewSize
      else
        I := NewSize - I;
      Inc(Split, I);
    end;
  end;
end;

procedure TSpTBXCustomSplitter.MouseStopSizing;
begin
  if Assigned(FMouseSplitControl) then begin
    if FMouseLineVisible then MouseDrawLine;
    FMouseSplitControl := nil;
    MouseReleaseLineDC;
    if Assigned(FMouseActiveControl) then begin
      TWinControlAccess(FMouseActiveControl).OnKeyDown := FOldKeyDown;
      FMouseActiveControl := nil;
    end;
  end;
  DoMoved;
  FMoving:= False;
end;

procedure TSpTBXCustomSplitter.MouseFocusKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then MouseStopSizing
  else if Assigned(FOldKeyDown) then FOldKeyDown(Sender,Key,Shift);
end;

procedure TSpTBXCustomSplitter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  C: TControl;
  F: TCustomForm;
  P: TPoint;
  I: Integer;
begin
  inherited;

  if (Button = mbLeft) and not (ssDouble in Shift) then begin
    C := ValidateSplitControl;
    if C = nil then Exit;

    P := Point(X,Y);
    if (FGripSize > 0) and PtInRect(GripRect, P) then
      FMouseDownOnGrip := True;

    FMouseSplitControl := C;
    FMouseDownPos := P;

    if AutoCalcMaxSize and Assigned(FMouseSplitControl) then begin
      if Align in [alLeft, alRight] then begin
        FMaxSize := Parent.ClientWidth - FMinSize - Width;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FMouseSplitControl.Width);
      end
      else begin
        FMaxSize := Parent.ClientHeight - FMinSize - Height;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alTop, alBottom]) then Dec(FMaxSize, Height);
        Inc(FMaxSize, FMouseSplitControl.Height);
      end;
    end;

    MouseCalcSplitSize(X, Y, FNewSize, FSplitLinePaintingPos);
    MouseAllocateLineDC;
    FMousePrevSplitControlSize := FNewSize;

    // When the ESC key is pressed we must abort the moving with StopSizing,
    // for that we must intercept the key event from the Active control.
    F := ValidParentForm(Self);
    if Assigned(F) then
      if F.ActiveControl <> nil then begin
        FMouseActiveControl := F.ActiveControl;
        FOldKeyDown := TWinControlAccess(FMouseActiveControl).OnKeyDown;
        TWinControlAccess(FMouseActiveControl).OnKeyDown := MouseFocusKeyDown;
      end;

    if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
  end;
end;

procedure TSpTBXCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I, Split: Integer;
  GripR: TRect;
  MouseInGrip: Boolean;
begin
  inherited;

  if (ssLeft in Shift) and not (ssDouble in Shift) and Assigned(FMouseSplitControl) then begin
    MouseCalcSplitSize(X, Y, I, Split);
    if DoMoving(I) then begin
      FMoving := True;
      if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
      FNewSize := I;
      FSplitLinePaintingPos := Split;
      case ResizeStyle of
        rsUpdate: UpdateControlSize(FMouseSplitControl);
        rsLine, rsPattern: MouseDrawLine;
      end;
    end;
  end;

  // Track the mouse to invalidate the Grip when the mouse enters or leaves the grip zone
  if (FGripSize > 0) and not FMoving then begin
    GripR := GripRect;
    MouseInGrip := PtInRect(GripR, Point(X, Y));
    if (MouseInGrip <> FMouseOverGrip) then begin
      FMouseOverGrip := MouseInGrip;
      InvalidateGrip;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  if (Button = mbLeft) and not (ssDouble in Shift) and Assigned(FMouseSplitControl) then begin
    P := Point(X, Y);
    if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
    UpdateControlSize(FMouseSplitControl);
    // If the splitter was minimized then RestorePos should be the MouseDown point
    if not FMouseDownOnGrip and FMoving and (FNewSize = 0) and (FMousePrevSplitControlSize <> 0) then
      FRestorePos := FMousePrevSplitControlSize;

    if (FGripSize > 0) and FMouseDownOnGrip then begin
      if not FMoving and PtInRect(GripRect, P) then
        Toggle;
      FMouseDownOnGrip := False;
      FMouseOverGrip := False;
      InvalidateGrip;
    end;

    MouseStopSizing;
  end;
end;

procedure TSpTBXCustomSplitter.Paint;
var
  ClientR, R, DragHandleR: TRect;
  C1, C2: TColor;
  PaintDefault: Boolean;
begin
  ClientR := ClientRect;
  PaintDefault := True;
  DoDrawBackground(Canvas, ClientR, pstPrePaint, PaintDefault);
  if PaintDefault then begin
    // Paint background
    if SkinManager.GetSkinType = sknSkin then
      CurrentSkin.PaintBackground(Canvas, ClientR, skncSplitter, sknsNormal, True, False, IsVertical)
    else begin
      if Color = clNone then
        Canvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace)
      else
        Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientR);
    end;

    // Paint grip
    R := GripRect;
    DragHandleR := R;
    if IsVertical then
      InflateRect(DragHandleR, -1, -10)
    else
      InflateRect(DragHandleR, -10, -1);

    if SkinManager.GetSkinType = sknSkin then begin
      if FMouseOverGrip then
        CurrentSkin.PaintBackground(Canvas, R, skncButton, sknsNormal, True, True, False, [akLeft, akTop, akRight, akBottom]);
      C1 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color1;
      C2 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color2;
      SpDrawXPGrip(Canvas, DragHandleR, C1, C2);
    end
    else begin
      C1 := CurrentSkin.GetThemedSystemColor(clBtnShadow);
      C2 := CurrentSkin.GetThemedSystemColor(clWindow);
      SpDrawXPGrip(Canvas, DragHandleR, C1, C2);
    end;
  end;

  PaintDefault := True;
  DoDrawBackground(Canvas, ClientR, pstPostPaint, PaintDefault);
end;

procedure TSpTBXCustomSplitter.CMMouseleave(var Message: TMessage);
begin
  inherited;
  if FMouseOverGrip and not FMoving then begin
    FMouseOverGrip := False;
    InvalidateGrip;
  end;
end;

procedure TSpTBXCustomSplitter.ChangeSplitControlSize(NewControlSize: Integer);
var
  C: TControl;
begin
  if not (csDesigning in ComponentState) then begin
    C := ValidateSplitControl;
    if C = nil then Exit;

    if NewControlSize < FMinSize then
      NewControlSize := FMinSize;
    if DoMoving(NewControlSize) then begin
      // If minimizing save restore position
      if NewControlSize = FMinSize then begin
        if IsVertical then FRestorePos := C.Width
        else FRestorePos := C.Height;
      end;

      FMoving := True;
      FNewSize := NewControlSize;
      UpdateControlSize(C);
    end;
    FMoving := False;
  end;
end;

function TSpTBXCustomSplitter.GetMinimized: Boolean;
var
  I, MinW, MinH: Integer;
  C: TControl;
begin
  Result := False;
  C := ValidateSplitControl;
  if Assigned(C) then begin
    MinW := C.Constraints.MinWidth;
    MinH := C.Constraints.MinHeight;

    if FMinSize > MinW then
      MinW := FMinSize;
    if FMinSize > MinH then
      MinH := FMinSize;

    if IsVertical then begin
      I := C.Width;
      Result := I <= MinW;
    end
    else begin
      I := C.Height;
      Result := I <= MinH;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.Minimize;
begin
  ChangeSplitControlSize(0);
end;

procedure TSpTBXCustomSplitter.Restore;
begin
  if Minimized then
    ChangeSplitControlSize(FRestorePos);
end;

procedure TSpTBXCustomSplitter.Toggle;
begin
  if Minimized then
    Restore
  else
    Minimize;
end;

procedure TSpTBXCustomSplitter.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomSplitter.DoMoved;
begin
  if Assigned(FOnMoved) then FOnMoved(Self);
end;

function TSpTBXCustomSplitter.DoMoving(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnMoving) then FOnMoving(Self, NewSize, Result);
  if Result and (NewSize < FMinSize) then NewSize := 0;
end;

procedure TSpTBXCustomSplitter.SetMinSize(const Value: integer);
begin
  if (Value <> FMinSize) and (Value >= 0) then FMinSize := Value;
end;

procedure TSpTBXCustomSplitter.UpdateControlSize(SplitControl: TControl);
begin
  if (FOldSize <> FNewSize) and Assigned(SplitControl) then begin
    case Align of
      alLeft:
        begin
          SpFixDelphiAlignBug(SplitControl, FNewSize, Self);
          FOldSize := SplitControl.Width;
        end;
      alTop:
        begin
          SpFixDelphiAlignBug(SplitControl, FNewSize, Self);
          FOldSize := SplitControl.Height;
        end;
      alRight:
        begin
          SpFixDelphiAlignBug(SplitControl, FNewSize, Self);
          FOldSize := SplitControl.Width;
        end;
      alBottom:
        begin
          SpFixDelphiAlignBug(SplitControl, FNewSize, Self);
          FOldSize := SplitControl.Height;
        end;
    end;
    Update;
    DoMoved;
  end;
end;

function TSpTBXCustomSplitter.GetGripRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FGripSize > 0 then
    if IsVertical then
      Result := Bounds(0, (Height - FGripSize) div 2, Width, FGripSize)
    else
      Result := Bounds((Width - FGripSize) div 2, 0, FGripSize, Height);
end;

procedure TSpTBXCustomSplitter.SetGripSize(const Value: Integer);
begin
  if FGripSize <> Value then begin
    if Value < 0 then FGripSize := 0
    else FGripSize := Value;
    InvalidateGrip;
  end;
end;

procedure TSpTBXCustomSplitter.InvalidateGrip;
begin
  if FGripHotTrack then Invalidate;
end;

end.
