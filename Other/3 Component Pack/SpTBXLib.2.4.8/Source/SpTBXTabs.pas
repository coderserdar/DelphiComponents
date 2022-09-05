unit SpTBXTabs;

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

Development Notes:
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - When an item is hidden the ItemViewer.BoundsRect property is invalid.
  - tbicDeleting item notification is fired after the ItemViewer of the
    Item is destroyed by TTBView, but the Items array still has the Item.

TODO:
  - Tabbed docking

History:
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - Changed tab close button behavior, the tab will close
    when the mouse button is released, thanks to Simon H. for
    reporting this.
  - Added TabCloseMiddleClick property to SpTBXTabSet and
    TSpTBXTabControl, when set to true a middle mouse button
    on a tab item will close it.

12 March 2010 - version 2.4.5
  - Fixed incorrect TabControl behavior, when changing the
    active tab the focused control was not correctly saved,
    thanks to Dmitry Belkevich for reporting this.

2 December 2009 - version 2.4.4
  - Fixed TabControl flicker when changing the caption of a tab
    item, thanks to Simon H. for reporting this.

13 September 2009 - version 2.4.3
  - Fixed TabControl flicker when closing/selecting/scrolling
    tab items.
  - Fixed incorrect TabControl painting when transparent
    child controls get invalidated, thanks for Alfred Vink
    for reporting this.
  - Fixed incorrect TabControl scrolling, thanks to Henner
    Drewes for reporting this.

8 May 2009 - version 2.4.2
  - Fixed incorrect TabControl behavior, when deleting
    the tabs the ActiveTabIndex is incorrectly set if
    there are regular Items on the control, thanks to
    Jonah for reporting this.

15 March 2009 - version 2.4.1
  - No changes.

17 January 2009 - version 2.4
  - No changes.

26 September 2008 - version 2.3
  - No changes.

29 July 2008 - version 2.2
  - No changes.

26 June 2008 - version 2.1
  - No changes.

3 May 2008 - version 2.0
  - Fixed incorrect close button painting on tab items,
    thanks to Miha for reporting this.

2 April 2008 - version 1.9.5
  - Fixed incorrect TabControl behavior, when deleting the
    only visible Tab all the auto-hidden tabs will not
    be showed on resize, thanks to Jim for reporting this.

3 February 2008 - version 1.9.4
  - Fixed AV on TabControl when scrolling tabs, thanks
    to Beta Xiong and Yucel Yavuz for reporting this.
  - Fixed incorrect TabControl.ScrollState behavior
    thanks to Michele for reporting this.

19 January 2008 - version 1.9.3
  - Fixed recursion on TSpTBXTabToolbar.RightAlignItems,
    thanks to Jim for reporting this.

26 December 2007 - version 1.9.2
  - Minor bug fixes.

1 December 2007 - version 1.9.1
  - Added OnTabClosing event to TSpTBXTabItem.
  - Fixed incorrect tab items painting when TabPosition was
    ttpBottom, thanks to Marko Savolainen for reporting this.

20 November 2007 - version 1.9
  - Added TabBackgroundBorders property to TSpTBXTabSet and
    TSpTBXTabControl, when set to true the tabs area is
    painted with borders.
  - Added TabCloseButton and TabCloseButtonImageIndex properties
    to SpTBXTabSet and TSpTBXTabControl to control the close
    button visibility on the tab items.
  - Added TabMaxSize property to SpTBXTabSet and TSpTBXTabControl
    to control the maximum size of the tab items.
  - Improved tab scrolling, thanks to Kevin Lu for
    reporting this.

8 February 2007 - version 1.8.3
  - Added accel char handling to TSpTBXTabSet and TSpTBXTabControl

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - Fixed incorrect Tab painting when the Default theme was used
    and the ThemeType was tttFlat.

27 August 2006 - version 1.8
  - Fixed incorrect OnActiveTabChanging handling when
    ActiveTabIndex is changed on this event, thanks to
    Serg Chechenin for reporting this.

15 June 2006 - version 1.7
  - Fixed incorrect Tab painting when the default theme was used,
    the captions were painted in a pushed state, thanks to
    Mikalai Arapau for reporting this.
  - Fixed incorrect Tab aligning when Autofit was used and the
    tab control was parented by a Frame, thanks to
    Henk van Kampen for reporting this.

12 April 2006 - version 1.5
  - Fixed incorrect Tab painting when TabAutofit was true.

27 February 2006 - version 1.4
  - Fixed flicker when reordering TSpTBXTabSet and TSpTBXTabControl
    tabs, thanks to Alexey Naumov for reporting this.
  - Fixed incorrect context menu handling in TSpTBXTabSet and
    TSpTBXTabControl, thanks to Boris Yankov for reporting this.
  - Added OnActiveTabReorder event to TSpTBXTabSet and TSpTBXTabControl.

10 February 2006 - version 1.3
  - Added TabDragReorder property to TSpTBXTabSet and TSpTBXTabControl,
    when this property is true it allows tabs reordering with
    drag and drop.
  - Added TabAutofit and TabAutofitMaxSize properties to TSpTBXTabSet
    and TSpTBXTabControl. When TabAutofit is true the tabs are resized
    to fit the tabset.

28 December 2005 - version 1.2
  - Fixed incorrect TSpTBXTabControl background painting on some themes.
  - Fixed incorrect OnActiveTabChange call when the component is being
    loaded, thanks to Leroy Casterline for reporting this.
  - Fixed incorrect tab scrolling when an item is deleted, thanks to
    Daniel Rikowski for reporting this.

18 October 2005 - version 1.1
  - Fixed incorrect TSpTBXTabItem painting on some themes.
  - Added Margins property to TSpTBXPageControl.

18 August 2005 - version 1.0
  - Added TabVisible property to TSpTBXTabSet and TSpTBXPageControl.
  - Added OnActiveTabChanging event to TSpTBXTabSet and TSpTBXPageControl.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.
  - Fixed AV in TSpTBXTabSet and TSpTBXPageControl when used in a Frame
    with TabPosition setted to dpBottom, thanks to Cyril Velter for the fix.

20 May 2005 - version 0.8
  - Fixed tab scrolling of TSpTBXTabSet and TSpTBXPageControl, the tabs
    were not allowed to scroll when one single tab was visible, thanks
    to Anders Olsson for the fix.
  - Added MakeVisible method to the TSpTBXTabSet and TSpTBXPageControl,
    it scrolls the tabset, if necessary, to ensure a Tab is in view.

16 February 2005 - version 0.7
  - No changes.

23 December 2004 - version 0.6
  - Fixed TSpTBXTabControl reordering bug.
  - Changed the order of the TSpTBXTabThemeType enumerated type.
  - Added ActivePage property to the TSpTBXTabControl.
  - Added Caption, ImageIndex, TabVisible and PopupMenu properties
    to TSpTBXTabControl.

30 August 2004 - version 0.5
  - No changes.

21 July 2004 - version 0.4
  - Fixed TSpTBXTabControl design time bug, it was allowing to drop
    components when ActiveTabIndex = -1
  - Fixed TSpTBXTabSet and TSpTBXTabControl design time bug, the
    hidden items were not streamed to the DFM.
  - Changed TSpTBXTabControl.OnTabClick event for OnActiveTabChange.
  - Added GetPage method to TSpTBXTabControl to get the TSpTBXTabSheet
    linked to a TSpTBXTabItem.

12 July 2004 - version 0.3.1
  - Fixed nasty AV when setting TBXSwitcher.EnableXPStyles to false,
    thanks to Alfred for reporting this.
    Note: TBXThemeManager unloads the theme library and the theme
    parts when some conditions are met, we must handle extra theme
    parts outside TBXThemeManager space.
  - Fixed incorrect TSpTBXTabSet.ActiveTabIndex property update at
    design time.
  - Fixed incorrect TSpTBXTabSet painting on some TBX themes, thanks
    to Tim for reporting this.

9 July 2004 - version 0.3
  - Fixed design time AVs when moving or deleting TabSheets.
  - Published ThemeType and TabPosition properties for TSpTBXTabItem.
  - New component added, TSpTBXTabSet, a fully customizable TabSet
    with unicode and toolbar items support.

28 June 2004 - version 0.2
  - No changes.

22 June 2004 - version 0.1
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Dialogs, ExtCtrls, TB2Item, TB2Dock, TB2Toolbar,
  SpTBXSkins, SpTBXItem, SpTBXControls;

const
  C_SpTBXTabGroupIndex = 7777;
  WM_INVALIDATETABBACKGROUND = WM_USER + 7777;

type
  TSpTBXTabEdge = (
    tedNone,         // No edge needed
    tedLeft,         // Left edge of the tab
    tedRight         // Right edge of the tab
  );

  TSpTBXTabPosition = (
    ttpTop,          // Top aligned tabset
    ttpBottom        // Bottom aligned tabset
  );

  TSpTBXTabCloseButton = (
    tcbNone,         // No close button on tabs
    tcbActive,       // Close button only on active tab
    tcbAll           // Close button on all the tabs
  );

  TSpTBXTabChangeEvent = procedure(Sender: TObject; TabIndex: Integer) of object;
  TSpTBXTabChangingEvent = procedure(Sender: TObject; TabIndex, NewTabIndex: Integer; var Allow: Boolean) of object;
  TSpTBXTabClosingEvent = procedure(Sender: TObject; var Allow, CloseAndFree: Boolean) of object;

  TSpTBXTabToolbar = class;
  TSpTBXCustomTabSet = class;
  TSpTBXCustomTabControl = class;
  TSpTBXTabSheet = class;

  TSpTBXTabItemDragObject = class(TSpTBXCustomDragObject)
  public
    constructor Create(ASourceControl: TControl; AItem: TTBCustomItem); override;
  end;

  { TSpTBXTabItem }

  TSpTBXTabItem = class(TSpTBXCustomItem)
  private
    FOnDrawTabCloseButton: TSpTBXDrawImageEvent;
    FOnTabClose: TNotifyEvent;
    FOnTabClosing: TSpTBXTabClosingEvent;
    function GetTabColor: TColor;
  protected
    function DialogChar(CharCode: Word): Boolean; override;
    procedure DoDrawTabCloseButton(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean); virtual;
    procedure DoTabClose; virtual;
    procedure DoTabClosing(var Allow, CloseAndFree: Boolean); virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetTabToolbar(out TabToolbar: TSpTBXTabToolbar): Boolean;
    procedure ToggleControl; override;
    property Control;  // TabSheet
    property TabColor: TColor read GetTabColor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure TabClose;
    function GetNextTab(GoForward: Boolean; SearchType: TSpTBXSearchItemViewerType): TSpTBXTabItem;
    function IsFirstVisible: Boolean;
    function IsFirstVisibleTab: Boolean;
    function IsLastVisibleTab: Boolean;
  published
    property Action;
    property Checked;
    // Hide DisplayMode
    // property DisplayMode default nbdmImageAndText;
    property Enabled;
    // Hide GroupIndex, all the TabItems must have the same GroupIndex
    // property GroupIndex;
    property HelpContext;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property ShortCut;
    property Visible;
    property OnClick;
    property OnSelect;
    // TSpTBXCustomItem properties
    property Alignment;
    property CustomWidth;
    property CustomHeight;
    property Margins default 4;
    property MinHeight;
    property MinWidth;
    property FontSettings;
    property Wrapping default twEndEllipsis;
    property OnDrawImage;
    property OnDrawItem;
    property OnDrawHint;
    property OnDrawCaption;
    // TSpTBXTabItem properties
    property OnDrawTabCloseButton: TSpTBXDrawImageEvent read FOnDrawTabCloseButton write FOnDrawTabCloseButton;
    property OnTabClose: TNotifyEvent read FOnTabClose write FOnTabClose;
    property OnTabClosing: TSpTBXTabClosingEvent read FOnTabClosing write FOnTabClosing;
  end;

  TSpTBXTabItemViewer = class(TSpTBXItemViewer)
  private
    FTabCloseButtonState: TSpTBXSkinStatesType;
    function CorrectTabRect(ARect: TRect): TRect;
    function GetItem: TSpTBXTabItem;
    procedure GetTabCloseButtonImgList(var AImageList: TCustomImageList; var AImageIndex: Integer);
    function IsTabCloseButtonVisible: Boolean;
    function GetTabPosition: TSpTBXTabPosition;
    procedure UpdateTabCloseButtonState(MousePosition: TPoint);
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
      var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
      IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect;
      var PaintDefault: Boolean); override;
    procedure DoDrawTabCloseButton(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean); virtual;
    procedure DrawBottomBorder(ACanvas: TCanvas; ARect: TRect);
    procedure DrawTab(ACanvas: TCanvas; ARect: TRect;
      AEnabled, AChecked, AHoverItem: Boolean; Position: TSpTBXTabPosition;
      ASeparator: Boolean = False; AEdge: TSpTBXTabEdge = tedNone); virtual;
    procedure DrawItemRightImage(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo); override;
    function GetRightImageSize: TSize; override;
    function GetRightImageRect: TRect;
    function GetTextColor(State: TSpTBXSkinStatesType): TColor; override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure Leaving; override;
  public
    function IsOnTabToolbar: Boolean;
    property Item: TSpTBXTabItem read GetItem; // Hides the inherited TB2K Item property
    property TabCloseButtonState: TSpTBXSkinStatesType read FTabCloseButtonState;
    property TabPosition: TSpTBXTabPosition read GetTabPosition;
  end;

  { TSpTBXTabToolbar }

  TSpTBXTabToolbarView = class(TSpTBXToolbarView)
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TSpTBXTabToolbar = class(TSpTBXToolbar)
  private
    FActiveTabRect: TRect;
    FTabAutofit: Boolean;
    FTabAutofitMaxSize: Integer;
    FTabCloseButtonImageIndex: Integer;
    FTabCloseButton: TSpTBXTabCloseButton;
    FTabCloseMiddleClick: Boolean;
    FTabDragReorder: Boolean;
    FTabBackgroundBorders: Boolean;
    FTabColor: TColor;
    FTabMaxSize: Integer;
    FTabPosition: TSpTBXTabPosition;
    procedure Scroll(ToRight: Boolean);
    function GetActiveTab: TSpTBXTabItem;
    procedure SetActiveTabIndex(Value: Integer);
    procedure SetTabCloseButton(const Value: TSpTBXTabCloseButton);
    procedure SetTabCloseButtonImageIndex(const Value: Integer);
    procedure SetTabAutofit(const Value: Boolean);
    procedure SetTabAutofitMaxSize(const Value: Integer);
    procedure SetTabBackgroundBorders(const Value: Boolean);
    procedure SetTabColor(const Value: TColor);
    procedure SetTabMaxSize(const Value: Integer);
    procedure SetTabPosition(const Value: TSpTBXTabPosition);
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    FHiddenTabs: TSpTBXItemCacheCollection;
    FActiveTabIndex: Integer;
    FOwnerTabControl: TSpTBXCustomTabSet;
    procedure Autofit;
    function GetItemsTextColor(State: TSpTBXSkinStatesType): TColor; override;
    function GetViewClass: TTBToolbarViewClass; override;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True); override;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); override;
    procedure RightAlignItems; override;

    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabsCount(VisibleOnly: Boolean): Integer;
    procedure InvalidateActiveTab;
    procedure InvalidateNC;
    procedure MakeVisible(ATab: TSpTBXTabItem);
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure ScrollState(out CanScrollToLeft, CanScrollToRight: Boolean);
    procedure TabClose(ATab: TSpTBXTabItem);
  published
    property ActiveTab: TSpTBXTabItem read GetActiveTab;
    property ActiveTabIndex: Integer read FActiveTabIndex write SetActiveTabIndex;
    property TabCloseButton: TSpTBXTabCloseButton read FTabCloseButton write SetTabCloseButton default tcbNone;
    property TabCloseButtonImageIndex: Integer read FTabCloseButtonImageIndex write SetTabCloseButtonImageIndex default -1;
    property TabCloseMiddleClick: Boolean read FTabCloseMiddleClick write FTabCloseMiddleClick default False;
    property TabBackgroundBorders: Boolean read FTabBackgroundBorders write SetTabBackgroundBorders;
    property TabAutofit: Boolean read FTabAutofit write SetTabAutofit default False;
    property TabAutofitMaxSize: Integer read FTabAutofitMaxSize write SetTabAutofitMaxSize default 200;
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
    property TabMaxSize: Integer read FTabMaxSize write SetTabMaxSize default -1;
    property TabPosition: TSpTBXTabPosition read FTabPosition write SetTabPosition default ttpTop;
    property TabDragReorder: Boolean read FTabDragReorder write FTabDragReorder default False;
  end;

  { TSpTBXTabSheet }

  TSpTBXTabSheet = class(TCustomControl)
  private
    FTabControl: TSpTBXCustomTabControl;
    FItem: TSpTBXTabItem;
    FItemName: String;
    FPrevFocused: TWincontrol;
    procedure ReadItemName(Reader: TReader);
    procedure WriteItemName(Writer: TWriter);
    function GetCaption: WideString;
    function GetTabVisible: Boolean;
    procedure SetCaption(const Value: WideString);
    procedure SetTabVisible(const Value: Boolean);
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure VisibleChanging; override;
    property Align default alClient;
    property PrevFocused: TWincontrol read FPrevFocused;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Item: TSpTBXTabItem read FItem write FItem;
    property TabControl: TSpTBXCustomTabControl read FTabControl write FTabControl;
  published
    {$IF CompilerVersion > 17}  // For Delphi 2006 and up
    property Padding;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFEND}
    property PopupMenu;
    property Caption: WideString read GetCaption write SetCaption;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
  end;

  { TSpTBXTabControl }

  TSpTBXCustomTabSet = class(TSpTBXCompoundItemsControl)
  private
    FItemMoveCount: Integer;
    FItemMoved: TSpTBXTabItem;
    FTabVisible: Boolean;
    FLoadingActiveIndex: Integer;
    FUpdatingIndex: Boolean;
    FResizing: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnActiveTabChange: TSpTBXTabChangeEvent;
    FOnActiveTabChanging: TSpTBXTabChangingEvent;
    FOnActiveTabReorder: TSpTBXTabChangeEvent;
    FOnActiveTabReordering: TSpTBXTabChangingEvent;
    procedure ReadHiddenItems(Reader: TReader);
    procedure WriteHiddenItems(Writer: TWriter);
    function GetActiveTab: TSpTBXTabItem;
    function GetActiveTabIndex: Integer;
    procedure SetActiveTabIndex(Value: Integer);
    function GetTabAutofit: Boolean;
    procedure SetTabAutofit(const Value: Boolean);
    function GetTabAutofitMaxSize: Integer;
    procedure SetTabAutofitMaxSize(const Value: Integer);
    function GetTabBackgroundBorders: Boolean;
    procedure SetTabBackgroundBorders(const Value: Boolean);
    function GetTabBackgroundColor: TColor;
    procedure SetTabBackgroundColor(const Value: TColor);
    function GetTabCloseButton: TSpTBXTabCloseButton;
    procedure SetTabCloseButton(const Value: TSpTBXTabCloseButton);
    function GetTabCloseButtonImageIndex: Integer;
    procedure SetTabCloseButtonImageIndex(const Value: Integer);
    function GetTabCloseMiddleClick: Boolean;
    procedure SetTabCloseMiddleClick(const Value: Boolean);
    function GetTabDragReorder: Boolean;
    procedure SetTabDragReorder(const Value: Boolean);
    function GetTabMaxSize: Integer;
    procedure SetTabMaxSize(const Value: Integer);
    function GetTabPosition: TSpTBXTabPosition;
    procedure SetTabPosition(const Value: TSpTBXTabPosition);
    procedure SetTabVisible(const Value: Boolean);
    function GetTabToolbar: TSpTBXTabToolbar;
    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMSpTBXControlsInvalidate(var Message: TMessage); message CM_SPTBXCONTROLSINVALIDATE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMInvalidateTabBackground(var Message: TMessage); message WM_INVALIDATETABBACKGROUND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FBackground: TBitmap;
    // Painting
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    function GetFullRepaint: Boolean; virtual;

    // Tabs
    function CanActiveTabChange(const TabIndex, NewTabIndex: Integer): Boolean; virtual;
    procedure DoActiveTabChange(const TabIndex: Integer); virtual;
    function CanActiveTabReorder(const TabIndex, NewTabIndex: Integer): Boolean; virtual;
    procedure DoActiveTabReorder(const TabIndex: Integer); virtual;
    procedure ItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); virtual;  // Items change notification
    procedure TabInserted(Item: TSpTBXTabItem); virtual;
    procedure TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean = True); virtual;

    // Component
    procedure DefineProperties(Filer: TFiler); override;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    procedure Loaded; override;

    property Color default clBtnFace;
    property ParentColor default False;
    property ActiveTabIndex: Integer read GetActiveTabIndex write SetActiveTabIndex;
    property TabAutofit: Boolean read GetTabAutofit write SetTabAutofit default False;
    property TabAutofitMaxSize: Integer read GetTabAutofitMaxSize write SetTabAutofitMaxSize default 200;
    property TabBackgroundColor: TColor read GetTabBackgroundColor write SetTabBackgroundColor default clNone;
    property TabBackgroundBorders: Boolean read GetTabBackgroundBorders write SetTabBackgroundBorders default False;
    property TabCloseButton: TSpTBXTabCloseButton read GetTabCloseButton write SetTabCloseButton default tcbNone;
    property TabCloseButtonImageIndex: Integer read GetTabCloseButtonImageIndex write SetTabCloseButtonImageIndex default -1;
    property TabCloseMiddleClick: Boolean read GetTabCloseMiddleClick write SetTabCloseMiddleClick default False;
    property TabDragReorder: Boolean read GetTabDragReorder write SetTabDragReorder default False;
    property TabMaxSize: Integer read GetTabMaxSize write SetTabMaxSize default -1;
    property TabPosition: TSpTBXTabPosition read GetTabPosition write SetTabPosition default ttpTop;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property OnActiveTabChange: TSpTBXTabChangeEvent read FOnActiveTabChange write FOnActiveTabChange;
    property OnActiveTabChanging: TSpTBXTabChangingEvent read FOnActiveTabChanging write FOnActiveTabChanging;
    property OnActiveTabReorder: TSpTBXTabChangeEvent read FOnActiveTabReorder write FOnActiveTabReorder;
    property OnActiveTabReordering: TSpTBXTabChangingEvent read FOnActiveTabReordering write FOnActiveTabReordering;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ACaption: WideString): TSpTBXTabItem;
    function Insert(NewIndex: Integer; ACaption: WideString): TSpTBXTabItem;
    function DrawBackground(DC: HDC; ARect: TRect): Boolean;
    function GetTabSetHeight: Integer;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); override;
    procedure MakeVisible(ATab: TSpTBXTabItem);
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure ScrollState(out Left, Right: Boolean);
    procedure TabClick(ATab: TSpTBXTabItem); virtual;
    property ActiveTab: TSpTBXTabItem read GetActiveTab;
    property Canvas;
    property Toolbar: TSpTBXTabToolbar read GetTabToolbar;
  end;

  TSpTBXTabSet = class(TSpTBXCustomTabSet)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IF CompilerVersion > 17}  // For Delphi 2006 and up
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFEND}
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomTabSet properties
    property ActiveTabIndex;
    property Images;
    property TabAutofit;
    property TabAutofitMaxSize;
    property TabBackgroundColor;
    property TabBackgroundBorders;
    property TabCloseButton;
    property TabCloseButtonImageIndex;
    property TabCloseMiddleClick;
    property TabDragReorder;
    property TabMaxSize;
    property TabPosition;
    property TabVisible;
    property OnActiveTabChange;
    property OnActiveTabChanging;
    property OnActiveTabReorder;
    property OnActiveTabReordering;
    property OnDrawBackground;
  end;

  { TSpTBXTabControl }

  TSpTBXCustomTabControl = class(TSpTBXCustomTabSet)
  private
    FEmptyTabSheet: TSpTBXTabSheet;
    procedure RealignTabSheets;
    function GetActivePage: TSpTBXTabSheet;
    function GetPages(Index: Integer): TSpTBXTabSheet;
    function GetPagesCount: Integer;
    procedure SetActivePage(const Value: TSpTBXTabSheet);
    procedure CMSpTBXControlsInvalidate(var Message: TMessage); message CM_SPTBXCONTROLSINVALIDATE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    FPages: TList;
    procedure DoActiveTabChange(const ItemIndex: Integer); override;
    function GetFullRepaint: Boolean; override;
    procedure TabInserted(Item: TSpTBXTabItem); override;
    procedure TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean = True); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPage(Item: TSpTBXTabItem): TSpTBXTabSheet;
    property ActivePage: TSpTBXTabSheet read GetActivePage write SetActivePage;
    property Pages[Index: Integer]: TSpTBXTabSheet read GetPages;
    property PagesCount: Integer read GetPagesCount;
  end;

  TSpTBXTabControl = class(TSpTBXCustomTabControl)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomTabControl properties
    property ActiveTabIndex;
    property Images;
    property TabAutofit;
    property TabAutofitMaxSize;
    property TabBackgroundColor;
    property TabBackgroundBorders;
    property TabCloseButton;
    property TabCloseButtonImageIndex;
    property TabCloseMiddleClick;
    property TabDragReorder;
    property TabMaxSize;
    property TabPosition;
    property TabVisible;
    property OnActiveTabChange;
    property OnActiveTabChanging;
    property OnActiveTabReorder;
    property OnActiveTabReordering;
    property OnDrawBackground;
  end;

function SpGetNextTabItemViewer(View: TTBView; IV: TTBItemViewer; GoForward: Boolean; SearchType: TSpTBXSearchItemViewerType): TTBItemViewer;
procedure SpDrawXPTab(ACanvas: TCanvas; ARect: TRect; Enabled, Checked, HotTrack, Focused: Boolean; Position: TSpTBXTabPosition; Edge: TSpTBXTabEdge = tedNone);
procedure SpDrawXPTabControlBackground(ACanvas: TCanvas; ARect: TRect; AColor: TColor; BottomTabs: Boolean);

implementation

uses
  Themes, UxTheme, Types;

type
  TTBItemViewerAccess = class(TTBItemViewer);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);
  TSpTBXDockAccess = class(TSpTBXDock);
  TSpTBXToolbarAccess = class(TSpTBXToolbar);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

function SpGetNextTabItemViewer(View: TTBView; IV: TTBItemViewer; GoForward: Boolean;
  SearchType: TSpTBXSearchItemViewerType): TTBItemViewer;
// Returns the left or right Tab item depending on GoForward
// SearchType can be:
//  sivtNormal: Normal search
//  sivtInmediate: Search for the inmediate ItemViewer
//  sivtInmediateSkipNonVisible: Search for the next inmediate ItemViewer, skipping non visible ones
begin
  Result := nil;
  while Result = nil do begin
    IV := SpGetNextItemSameEdge(View, IV, GoForward, SearchType);
    if not Assigned(IV) then
      Break  // Not found, exit
    else
      if IV.Item is TSpTBXTabItem then begin
        Result := IV;  // Found Tab
        Break;
      end
      else begin
        case SearchType of
          sivtInmediate:
            Break;  // Inmediate not found, exit
          sivtInmediateSkipNonVisible:
            if IV.Item.Visible then Break;  // Inmediate not found and visible, exit
        end;
      end;
  end;
end;

procedure SpDrawXPTab(ACanvas: TCanvas; ARect: TRect;
  Enabled, Checked, HotTrack, Focused: Boolean; Position: TSpTBXTabPosition;
  Edge: TSpTBXTabEdge = tedNone);
var
  B: TBitmap;
  R, FlippedR: TRect;
  State: TSpTBXSkinStatesType;
  DrawState: TThemedTab;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  SkinType := SkinManager.GetSkinType;
  if (SkinType = sknNone) and not Checked then
    Exit;

  B := TBitmap.Create;
  try
    B.Width := ARect.Right - ARect.Left;
    B.Height := ARect.Bottom - ARect.Top;
    R := Rect(0, 0, B.Width, B.Height);
    if SkinType = sknDelphiStyle then
      B.Canvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace)
    else begin
      B.Canvas.Brush.Color := clFuchsia;
      B.TransparentColor := clFuchsia;
      B.Transparent := True;
    end;
    B.Canvas.FillRect(R);

    case SkinType of
      sknNone:
        if Checked then begin
          Position := ttpTop;  // Don't need to flip
          B.Canvas.Brush.Color := ACanvas.Brush.Color;
          B.Canvas.FillRect(R);
          ExtCtrls.Frame3D(B.Canvas, R, clWindow, clWindowFrame, 1);
          ExtCtrls.Frame3D(B.Canvas, R, B.Canvas.Brush.Color, clBtnShadow, 1);
          R := Rect(0, 0, B.Width, B.Height);  // Frame3D changed R
        end;
      sknWindows, sknDelphiStyle:
        begin
          case Edge of
            tedLeft:  DrawState := ttTabItemLeftEdgeNormal;
            tedRight: DrawState := ttTabItemRightEdgeNormal;
          else
            DrawState := ttTabItemNormal;
          end;

          if not Enabled then DrawState := TThemedTab(Ord(DrawState) + 3)
          else
            if Checked then DrawState := TThemedTab(Ord(DrawState) + 2)
            else
              if HotTrack then DrawState := TThemedTab(Ord(DrawState) + 1);

          Details := SpTBXThemeServices.GetElementDetails(DrawState);
          CurrentSkin.PaintThemedElementBackground(B.Canvas, R, Details);
        end;
      sknSkin:
        begin
          State := CurrentSkin.GetState(Enabled, False, HotTrack, Checked);
          CurrentSkin.PaintBackground(B.Canvas, R, skncTab, State, True, True);
        end;
    end;

    // Flip top to bottom
    if Position = ttpBottom then begin
      // Unclear why extra "-1" is needed here.
      FlippedR := R;
      FlippedR.Top := R.Bottom - 1;
      FlippedR.Bottom := R.Top - 1;
      B.Canvas.CopyRect(R, B.Canvas, FlippedR);
    end;

    // Draw focus
    if Checked and Focused then begin
      InflateRect(R, -3, -3);
      SpDrawFocusRect(B.Canvas, R);
    end;

    ACanvas.Draw(ARect.Left, ARect.Top, B);
  finally
    B.Free;
  end;
end;

procedure SpDrawXPTabControlBackground(ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  BottomTabs: Boolean);
var
  B: TBitmap;
  R: TRect;
begin
  B := TBitmap.Create;
  try
    B.Width := ARect.Right - ARect.Left;
    B.Height := ARect.Bottom - ARect.Top;
    R := Rect(0, 0, B.Width, B.Height);

    // Draw the top/bottom border
    case SkinManager.GetSkinType of
      sknNone:
        begin
          BottomTabs := False; // Don't flip
          B.Canvas.Brush.Color := AColor;
          B.Canvas.FillRect(R);
          ExtCtrls.Frame3D(B.Canvas, R, clWindow, clWindowFrame, 1);
          ExtCtrls.Frame3D(B.Canvas, R, AColor, clBtnShadow, 1);
          R := Rect(0, 0, B.Width, B.Height);  // Frame3D changed R
        end;
      sknWindows, sknDelphiStyle:
        SpTBXThemeServices.DrawElement(B.Canvas.Handle, SpTBXThemeServices.GetElementDetails(ttPane), R);
      sknSkin:
        begin
          B.Canvas.Brush.Color := clWhite;
          B.Canvas.FillRect(R);
          CurrentSkin.PaintBackground(B.Canvas, R, skncTabBackground, sknsNormal, True, True);
        end;
    end;

    // Flip top to bottom
    if BottomTabs then begin
      // Unclear why extra "-1" is needed here.
      R.Top := B.Height - 1;
      R.Bottom := -1
    end;

    ACanvas.CopyRect(ARect, B.Canvas, R);
  finally
    B.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabItemDragObject }

constructor TSpTBXTabItemDragObject.Create(ASourceControl: TControl;
  AItem: TTBCustomItem);
begin
  inherited Create(ASourceControl, AItem);
  DragCursorAccept := crDefault;
  DragCursorCancel := crDefault;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabItem }

constructor TSpTBXTabItem.Create(AOwner: TComponent);
begin
  inherited;
  DisplayMode := nbdmImageAndText;
  GroupIndex := C_SpTBXTabGroupIndex;
  Wrapping := twEndEllipsis;
  Margins := 4;
end;

procedure TSpTBXTabItem.Click;
var
  T: TSpTBXTabToolbar;
  I: Integer;
begin
  // Set the Checked property to True, Autocheck is False
  // Checked will call Item.Invalidate, the TabSet.ItemNotification will
  // handle the invalidation and set the ActiveTabIndex.
  if not Checked and Enabled and Visible then begin
    if GetTabToolbar(T) then begin
      I := T.Items.IndexOf(Self);
      if T.FOwnerTabControl.CanActiveTabChange(T.ActiveTabIndex, I) then
        Checked := True;
    end;
    inherited;
  end;
end;

procedure TSpTBXTabItem.TabClose;
var
  NextTab: TSpTBXTabItem;
  T: TSpTBXTabToolbar;
  CloseAndFree, CanTabClose: Boolean;
begin
  if Visible then begin
    GetTabToolbar(T);
    if not Assigned(T) then Exit;

    CanTabClose := True;
    CloseAndFree := False;
    DoTabClosing(CanTabClose, CloseAndFree);
    if CanTabClose then begin
      // Check the next visible tab
      NextTab := nil;
      if Checked then begin
        NextTab := GetNextTab(True, sivtInmediateSkipNonVisible);
        if not Assigned(NextTab) then
          NextTab := GetNextTab(False, sivtInmediateSkipNonVisible);
      end;

      T.BeginUpdate;
      try
        Visible := False;
        DoTabClose;
        if CloseAndFree then
          Free;  // Removes the item from the parent, sends tbicDeleting notification and frees the item
        if Assigned(NextTab) then
          NextTab.Click;  // Sends tbicInvalidate notification, which is handled by TSpTBXCustomTabSet.ItemNotification
      finally
        T.EndUpdate;
      end;
    end;
  end;
end;

function TSpTBXTabItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := inherited DialogChar(CharCode);
  if Enabled and Visible and IsAccel(CharCode, Caption) then begin
    Click;
    Result := True;
  end;
end;

procedure TSpTBXTabItem.DoDrawTabCloseButton(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawTabCloseButton) then FOnDrawTabCloseButton(Self, ACanvas, State, PaintStage,
    AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXTabItem.DoTabClose;
begin
  if Assigned(FOnTabClose) then FOnTabClose(Self);
end;

procedure TSpTBXTabItem.DoTabClosing(var Allow, CloseAndFree: Boolean);
begin
  if Assigned(FOnTabClosing) then FOnTabClosing(Self, Allow, CloseAndFree);
end;

function TSpTBXTabItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXTabItemViewer;
end;

function TSpTBXTabItem.GetNextTab(GoForward: Boolean; SearchType: TSpTBXSearchItemViewerType): TSpTBXTabItem;
// Returns the left or right Tab item depending on GoForward, skipping all the non-visible ones.
// If Inmediate is true it will only search for the next inmediate tab
var
  T: TSpTBXTabToolbar;
  IV: TTBItemViewer;
begin
  Result := nil;

  if GetTabToolbar(T) then begin
    IV := SpFindItemViewer(T.View, Self);
    if Assigned(IV) then begin
      IV := SpGetNextTabItemViewer(T.View, IV, GoForward, SearchType);
      if Assigned(IV) then
        Result := IV.Item as TSpTBXTabItem
    end;
  end;
end;

function TSpTBXTabItem.GetTabColor: TColor;
var
  T: TSpTBXTabToolbar;
begin
  Result := clBtnFace;
  if GetTabToolbar(T) then
    Result := T.TabColor;
end;

function TSpTBXTabItem.IsFirstVisible: Boolean;
var
  T: TSpTBXTabToolbar;
  IV: TTBItemViewer;
begin
  Result := False;
  if GetTabToolbar(T) then begin
    IV := SpFindItemViewer(T.View, Self);
    if Assigned(IV) then
      Result := T.View.NextSelectable(nil, True) = IV;
  end;
end;

function TSpTBXTabItem.IsFirstVisibleTab: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  if GetTabToolbar(T) then
    Result := not Assigned(GetNextTab(False, sivtNormal))
  else
    Result := False;
end;

function TSpTBXTabItem.IsLastVisibleTab: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  if GetTabToolbar(T) then
    Result := not Assigned(GetNextTab(True, sivtNormal))
  else
    Result := False;
end;

function TSpTBXTabItem.GetTabToolbar(out TabToolbar: TSpTBXTabToolbar): Boolean;
var
  C: TComponent;
begin
  C := GetParentComponent;
  if Assigned(C) and (C is TSpTBXTabToolbar) then
    TabToolbar := C as TSpTBXTabToolbar
  else
    TabToolbar := nil;
  Result := Assigned(TabToolbar);
end;

procedure TSpTBXTabItem.ToggleControl;
begin
  // Do nothing, the Control property is the Tabsheet, and its visibility
  // is setted by TabSet.ActiveTabIndex
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabItemViewer }

procedure TSpTBXTabItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
var
  TabMaxSize: Integer;
begin
  inherited CalcSize(Canvas, AWidth, AHeight);

  if IsOnTabToolbar and not Item.Anchored then begin
    TabMaxSize := TSpTBXTabToolbar(View.Window).TabMaxSize;
    if TabMaxSize > 0 then
      if IsRotated then begin
        if AHeight > TabMaxSize then
          AHeight := TabMaxSize;
      end
      else begin
        if AWidth > TabMaxSize then
          AWidth := TabMaxSize;
      end;
  end;
end;

function TSpTBXTabItemViewer.CorrectTabRect(ARect: TRect): TRect;
// Offsets the rect to give a pushed effect on the tabs
begin
  Result := ARect;
  if not Item.Checked then
    case TabPosition of
      ttpTop:    OffsetRect(Result, 0, 2);
      ttpBottom: OffsetRect(Result, 0, -2);
    end;
end;

procedure TSpTBXTabItemViewer.DoDrawButton(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  LeftT, RightT: TTBItemViewer;
  IsHoverItem: Boolean;
  R: TRect;
  Position: TSpTBXTabPosition;
begin
  inherited;

  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;
    IsHoverItem := (ItemInfo.State = sknsHotTrack) or (ItemInfo.State = sknsCheckedAndHotTrack);
    Position := TabPosition;

    // Match the bottom of the Tab with the bottom of the TabSet
    case Position of
      ttpTop:    ARect.Bottom := ARect.Bottom + 1;
      ttpBottom: ARect.Top := ARect.Top - 1;
    end;
    R := ARect;

    case SkinManager.GetSkinType of
      sknNone, sknSkin:
        if Item.Checked or (IsHoverItem and (SkinManager.GetSkinType <> sknNone)) or
          not CurrentSkin.Options(skncTab, sknsNormal).Body.IsEmpty or
          not CurrentSkin.Options(skncTab, sknsNormal).Borders.IsEmpty then
        begin
          case Position of
            ttpTop:    Inc(R.Bottom, 5);
            ttpBottom: Dec(R.Top, 5);
          end;
          DrawTab(ACanvas, R, Item.Enabled, Item.Checked, IsHoverItem, Position);
        end
        else begin
          // Draw the separators
          RightT := SpGetNextTabItemViewer(View, Self, True, sivtInmediateSkipNonVisible);
          if Assigned(RightT) and not RightT.Item.Checked then
            DrawTab(ACanvas, R, Item.Enabled, Item.Checked, IsHoverItem, Position, True);
        end;
      sknWindows, sknDelphiStyle:
        begin
          if IsOnTabToolbar then begin
            // Find the inmediate left and right tabs
            LeftT := SpGetNextTabItemViewer(View, Self, False, sivtInmediateSkipNonVisible);
            RightT := SpGetNextTabItemViewer(View, Self, True, sivtInmediateSkipNonVisible);
          end
          else begin
            LeftT := nil;
            RightT := nil;
          end;

          if Item.Checked then begin
            // The left border of the Tab will be painted by the Left tab if
            // its the first tab
            if Assigned(LeftT) or (Item.IsFirstVisible) then
              R.Left := R.Left - 2;
            // The right border of the Tab will be painted by the Right tab
            if Assigned(RightT) then
              R.Right := R.Right + 2;
          end
          else begin
            // Non checked tabs should be smaller
            case Position of
              ttpTop:    Inc(R.Top, 2);
              ttpBottom: Dec(R.Bottom, 2);
            end;
          end;

          // Draw the Tab
          DrawTab(ACanvas, R, Item.Enabled, Item.Checked, IsHoverItem, Position);

          // If the Tab is not checked then it should paint the active tab borders
          if not Item.Checked then begin
            R := ARect;
            // Draw the left border
            if Assigned(LeftT) and LeftT.Item.Checked then begin
              R.Right := R.Left + 2;
              R.Left := R.Right - 10;
              DrawTab(ACanvas, R, LeftT.Item.Enabled, True, IsHoverItem, Position);
            end
            else
              // Draw the right border
              if Assigned(RightT) and RightT.Item.Checked then begin
                R.Left := R.Right - 2;
                R.Right := R.Left + 10;
                DrawTab(ACanvas, R, RightT.Item.Enabled, True, IsHoverItem, Position);
              end;
          end;
        end;
    end;
  end;
end;

procedure TSpTBXTabItemViewer.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect;
  State: TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect;
  var CaptionFormat: Cardinal; IsTextRotated: Boolean;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  CaptionRect := CorrectTabRect(CaptionRect);  // Offset the rect to give a pushed effect on the tabs
  inherited DoDrawCaption(ACanvas, ClientAreaRect, State, ACaption, CaptionRect,
    CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
end;

procedure TSpTBXTabItemViewer.DoDrawImage(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
begin
  ARect := CorrectTabRect(ARect);  // Offset the rect to give a pushed effect on the tabs
  inherited DoDrawImage(ACanvas, State, PaintStage, AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXTabItemViewer.DoDrawTabCloseButton(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
begin
  TSpTBXTabItem(Item).DoDrawTabCloseButton(ACanvas, State, PaintStage, AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXTabItemViewer.DrawBottomBorder(ACanvas: TCanvas; ARect: TRect);
var
  CR, R: TRect;
  Edge: TSpTBXTabEdge;
  LeftT, RightT: Boolean;
  Position: TSpTBXTabPosition;
  B: TBitmap;
begin
  if not IsOnTabToolbar then Exit;

  Position := TabPosition;
  Edge := tedNone;
  CR := ARect;

  case Position of
    ttpTop:
      Inc(CR.Bottom, 2);
    ttpBottom:
      begin
        // When sknNone the bottom border size is 2
        if SkinManager.GetSkinType = sknNone then
          Dec(CR.Top, 2)
        else
          Dec(CR.Top, 2);
      end;
  end;

  if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then begin
    LeftT := Assigned(SpGetNextTabItemViewer(View, Self, False, sivtInmediateSkipNonVisible));
    RightT := Assigned(SpGetNextTabItemViewer(View, Self, True, sivtInmediateSkipNonVisible));

    if Item.IsFirstVisible then  // Is first IV?
      Edge := tedLeft;

    if Edge = tedLeft then begin
      CR.Left := CR.Left - 2;
      if RightT then
        CR.Right := CR.Right + 2;
    end
    else begin
      if LeftT then
        CR.Left := CR.Left - 2;
      if RightT then
        CR.Right := CR.Right + 2;
    end;
  end;

  B := TBitmap.Create;
  try
    B.Width := CR.Right - CR.Left;
    B.Height := CR.Bottom - CR.Top + 4; // Larger than CR
    R := Rect(0, 0, B.Width, B.Height);
    DrawTab(B.Canvas, R, True, True, False, Position, False, Edge);

    case Position of
      ttpTop:
        R  := Bounds(0, 0, CR.Right - CR.Left, CR.Bottom - CR.Top); // Copy from Y = 0
      ttpBottom:
        R  := Bounds(0, 2, CR.Right - CR.Left, CR.Bottom - CR.Top + 2); // Copy from Y = 2
    end;

    ACanvas.CopyRect(CR, B.Canvas, R);
  finally
    B.Free;
  end;
end;

procedure TSpTBXTabItemViewer.DrawItemRightImage(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo);
var
  PaintDefault: Boolean;
  ImgList: TCustomImageList;
  ImgIndex: Integer;
  PatternColor: TColor;
begin
  if IsOnTabToolbar then begin
    if not IsTabCloseButtonVisible then
      Exit;

    GetTabCloseButtonImgList(ImgList, ImgIndex);
    ARect := CorrectTabRect(ARect);  // Offset the rect to give a pushed effect on the tabs

    ItemInfo.Pushed := False;
    ItemInfo.Checked := False;
    if ItemInfo.Enabled then
      if FTabCloseButtonState = sknsHotTrack then
        ItemInfo.State := sknsHotTrack
      else begin
        ItemInfo.Enabled := False;
        ItemInfo.State := sknsDisabled;
      end;

    PaintDefault := True;
    DoDrawTabCloseButton(ACanvas, ItemInfo.State, pstPrePaint, ImgList, ImgIndex, ARect, PaintDefault);
    if PaintDefault and Assigned(ImgList) and (FTabCloseButtonState = sknsHotTrack) then
      if (ImgList = MDIButtonsImgList) or ((ImgIndex >= 0) and (ImgIndex < ImgList.Count)) then
        SpDrawXPMenuItem(ACanvas, ARect, ItemInfo);

    PaintDefault := True;
    if ImgList = MDIButtonsImgList then begin
      PatternColor := GetTextColor(ItemInfo.State);
      SpDrawGlyphPattern(ACanvas, ARect, ImgIndex, PatternColor);
    end
    else
      DoDrawTabCloseButton(ACanvas, ItemInfo.State, pstPostPaint, ImgList, ImgIndex, ARect, PaintDefault);
    if PaintDefault and Assigned(ImgList) and (ImgIndex >= 0) and (ImgIndex < ImgList.Count) then
      SpDrawXPMenuItemImage(ACanvas, ARect, ItemInfo, ImgList, ImgIndex);
  end;
end;

procedure TSpTBXTabItemViewer.DrawTab(ACanvas: TCanvas; ARect: TRect; AEnabled,
  AChecked, AHoverItem: Boolean; Position: TSpTBXTabPosition;
  ASeparator: Boolean; AEdge: TSpTBXTabEdge);
begin
  if ASeparator then begin
    ARect.Left := ARect.Right - 2;
    SpDrawXPMenuSeparator(ACanvas, ARect, False, True)
  end
  else begin
    ACanvas.Brush.Color := Item.TabColor;
    SpDrawXPTab(ACanvas, ARect, AEnabled, AChecked, AHoverItem, False, Position, AEdge);
  end;
end;

function TSpTBXTabItemViewer.GetItem: TSpTBXTabItem;
begin
  Result := TSpTBXTabItem(inherited Item);
end;

function TSpTBXTabItemViewer.GetRightImageRect: TRect;
var
  RightGlyphSize: TSize;
  R: TRect;
begin
  RightGlyphSize := GetRightImageSize;
  R := BoundsRect;
  InflateRect(R, -4, -4);  // Apply borders

  Result.Left := R.Right - RightGlyphSize.cx;
  Result.Right := Result.Left + RightGlyphSize.cx;
  Result.Top := R.Top + (R.Bottom - R.Top - RightGlyphSize.cy) div 2;
  Result.Bottom := Result.Top + RightGlyphSize.cy;

  Result := CorrectTabRect(Result);  // Offset the rect to give a pushed effect on the tabs
end;

function TSpTBXTabItemViewer.GetRightImageSize: TSize;
var
  ImgList: TCustomImageList;
  ImgIndex: Integer;
begin
  Result.cx := 0;
  Result.cy := 0;
  GetTabCloseButtonImgList(ImgList, ImgIndex);
  if Assigned(ImgList) then
    if ImgList = MDIButtonsImgList then begin
      Result.cx := 15;
      Result.cy := 15;
    end
    else if (ImgIndex >= 0) and (ImgIndex < ImgList.Count) then begin
      Result.cx := ImgList.Width;
      Result.cy := ImgList.Height;
    end;
end;

procedure TSpTBXTabItemViewer.GetTabCloseButtonImgList(var AImageList: TCustomImageList;
  var AImageIndex: Integer);
var
  T: TSpTBXTabToolbar;
begin
  AImageList := nil;
  AimageIndex := -1;
  if IsOnTabToolbar then begin
    T := TSpTBXTabToolbar(View.Window);
    if T.TabCloseButton <> tcbNone then begin
      AImageList := GetImageList;
      AImageIndex := T.TabCloseButtonImageIndex;
      if not Assigned(AImageList) or (AImageIndex < 0) or (AImageIndex >= AImageList.Count) then begin
        AImageList := MDIButtonsImgList;
        AImageIndex := 0;
      end;
    end;
  end;
end;

function TSpTBXTabItemViewer.GetTabPosition: TSpTBXTabPosition;
begin
  if IsOnTabToolbar then
    Result := TSpTBXTabToolbar(View.Window).TabPosition
  else
    Result := ttpTop;
end;

function TSpTBXTabItemViewer.GetTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := Item.FontSettings.Color;
  if Result = clNone then begin
    if View.Window is TSpTBXTabToolbar then
      Result := TSpTBXToolbarAccess(View.Window).GetItemsTextColor(State);
    if Result = clNone then
      Result := CurrentSkin.GetTextColor(skncTab, State)
  end;
end;

procedure TSpTBXTabItemViewer.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateTabCloseButtonState(Point(X, Y));
end;

function TSpTBXTabItemViewer.IsOnTabToolbar: Boolean;
begin
  Result := Assigned(View.Window) and (View.Window is TSpTBXTabToolbar);
end;

function TSpTBXTabItemViewer.IsTabCloseButtonVisible: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  Result := False;
  if IsOnTabToolbar then begin
    T := TSpTBXTabToolbar(View.Window);
    case T.TabCloseButton of
      tcbNone:
        Exit;
      tcbActive:
        if not Item.Checked then Exit;
    end;
    Result := True;
  end;
end;

procedure TSpTBXTabItemViewer.Leaving;
var
  R: TRect;
begin
  inherited;
  if FTabCloseButtonState = sknsHotTrack then begin
    FTabCloseButtonState := sknsNormal;
    R := GetRightImageRect;
    InvalidateRect(View.Window.Handle, @R, True)
  end;
end;

procedure TSpTBXTabItemViewer.MouseMove(X, Y: Integer);
var
  P: TPoint;
begin
  // TTBItemViewer.MouseMove is only called when the modal handler is active,
  // that is when the mouse button is being clicked.
  // We need to update the tab close button state.
  inherited;

  // Correct the params, TTBItemViewer.MouseMove is called by
  // TTBModalHandler.Loop.MouseMoved with incorrect param values
  P := Point(X, Y);
  Inc(P.X, BoundsRect.Left);
  Inc(P.Y, BoundsRect.Top);
  UpdateTabCloseButtonState(P);
end;

procedure TSpTBXTabItemViewer.MouseUp(X, Y: Integer;
  MouseWasDownOnMenu: Boolean);
begin
  inherited;
  if Assigned(Item) and Item.Enabled and Item.Visible then begin
    // Close the tab if the close button is pressed
    if (TabCloseButtonState = sknsHotTrack) and IsTabCloseButtonVisible then
      Item.TabClose;
  end;
end;

procedure TSpTBXTabItemViewer.UpdateTabCloseButtonState(MousePosition: TPoint);
var
  R: TRect;
  NewState: TSpTBXSkinStatesType;
begin
  if FTabCloseButtonState in [sknsNormal, sknsHotTrack] then begin
    if not Item.Enabled then begin
      FTabCloseButtonState := sknsDisabled;
      Exit;
    end;
    R := GetRightImageRect;
    if PtInRect(R, MousePosition) then
      NewState := sknsHotTrack
    else
      NewState := sknsNormal;
    if NewState <> FTabCloseButtonState then begin
      FTabCloseButtonState := NewState;
      InvalidateRect(View.Window.Handle, @R, True)
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabToolbarView }

procedure TSpTBXTabToolbarView.BeginUpdate;
var
  T: TSpTBXTabToolbar;
begin
  if (FUpdating = 0) and (Owner is TSpTBXTabToolbar) then begin
    T := TSpTBXTabToolbar(Owner);
    if Assigned(T.FOwnerTabControl) then
      SendMessage(T.FOwnerTabControl.Handle, WM_SETREDRAW, 0, 0);
  end;
  inherited;
end;

procedure TSpTBXTabToolbarView.EndUpdate;
var
  T: TSpTBXTabToolbar;
begin
  inherited;
  if (FUpdating = 0) and (Owner is TSpTBXTabToolbar) then begin
    T := TSpTBXTabToolbar(Owner);
    if Assigned(T.FOwnerTabControl) then begin
      SendMessage(T.FOwnerTabControl.Handle, WM_SETREDRAW, 1, 0);
      SpInvalidateSpTBXControl(T.FOwnerTabControl, True, False);
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabToolbar }

constructor TSpTBXTabToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FHiddenTabs := TSpTBXItemCacheCollection.Create(TSpTBXItemCache);
  if Owner is TSpTBXCustomTabSet then
    FOwnerTabControl := Owner as TSpTBXCustomTabSet
  else
    FOwnerTabControl := nil;
  FActiveTabIndex := -1;
  FTabBackgroundBorders := False;
  FTabAutofitMaxSize := 200;
  FTabCloseButtonImageIndex := -1;  
  FTabColor := clBtnFace;
  FTabMaxSize := -1;
  FTabPosition := ttpTop;
end;

destructor TSpTBXTabToolbar.Destroy;
begin
  FHiddenTabs.Free;
  inherited;
end;

procedure TSpTBXTabToolbar.DoItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
begin
  inherited;
  if Action = tbicInvalidateAndResize then begin
    // Invalidate the NC area, draw the bottom border of the active tab
    // Instead of invalidating every time, save the Rect that needs to be drawn
    // (bottom border of the active tab) and see if it needs to be repainted
    if not IsItemMoving then begin
      Tab := ActiveTab;
      if Assigned(Tab) and Tab.Visible then begin
        IV := SpFindItemViewer(View, Tab);
        if Assigned(IV) then
          if not EqualRect(FActiveTabRect, IV.BoundsRect) then
            InvalidateNC;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.InternalDrawBackground(ACanvas: TCanvas; ARect: TRect;
  PaintOnNCArea: Boolean; PaintBorders: Boolean = True);
var
  B: TBitmap;
  R, BitmapR, DestR: TRect;
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
  PrevDelta, NextDelta: Integer;
begin
  if PaintOnNCArea and Assigned(FOwnerTabControl) then begin
    B := TBitmap.Create;
    B.Canvas.Lock;
    try
      R := ARect;
      B.Width := R.Right - R.Left;
      B.Height := R.Bottom - R.Top;

      SpDrawXPToolbar(Self, B.Canvas, R, PaintOnNCArea, FTabBackgroundBorders and (SkinManager.GetSkinType <> sknNone), skncTabToolbar);

      // Draw the bottom border of the active tab
      Tab := ActiveTab;
      if Assigned(Tab) and Tab.Visible then begin
        IV := SpFindItemViewer(View, Tab);
        if Assigned(IV) then begin
          FActiveTabRect := IV.BoundsRect;
          DestR := IV.BoundsRect;
          OffsetRect(DestR, 2, 2);  // Add the toolbar margins
          TSpTBXTabItemViewer(IV).DrawBottomBorder(B.Canvas, DestR);
        end;
        if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then begin
          if Tab.IsFirstVisible or Assigned(Tab.GetNextTab(False, sivtInmediateSkipNonVisible)) then
            PrevDelta := 1
          else
            PrevDelta := -1;
          if Assigned(Tab.GetNextTab(True, sivtInmediateSkipNonVisible)) then
            NextDelta := 1
          else
            NextDelta := -1;
          if FTabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Bottom - 2, DestR.Right + NextDelta, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Top + 2, DestR.Right + NextDelta, R.Top - 4);
        end
        else
          if FTabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Bottom - 2, DestR.Right - 1, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Top + 2, DestR.Right -1 , R.Top - 4);
      end;

      // Draw the bottom border of the tabs pane
      BitmapR := Rect(0, 0, FOwnerTabControl.FBackground.Width, FOwnerTabControl.FBackground.Height);
      case FTabPosition of
        ttpTop:
          begin
            DestR := Rect(R.Left, R.Bottom - 2, R.Right, R.Bottom);
            BitmapR.Bottom := BitmapR.Top + 2;
          end;
        ttpBottom:
          begin
            DestR := Rect(R.Left, R.Top, R.Right, R.Top + 2);
            BitmapR.Top := BitmapR.Bottom - 2;
          end;
      end;

      B.Canvas.CopyRect(DestR, FOwnerTabControl.FBackground.Canvas, BitmapR);
      ACanvas.Draw(0, 0, B);
    finally
      B.Canvas.UnLock;
      B.Free;
    end;
  end
  else
    SpDrawXPToolbar(Self, ACanvas, ARect, PaintOnNCArea, FTabBackgroundBorders and (SkinManager.GetSkinType <> sknNone), skncTabToolbar);
end;

procedure TSpTBXTabToolbar.InvalidateActiveTab;
var
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
begin
  Tab := ActiveTab;
  if Assigned(Tab) then begin
    IV := SpFindItemViewer(View, Tab);
    if Assigned(IV) then
      View.Invalidate(IV);
  end;
end;

procedure TSpTBXTabToolbar.InvalidateNC;
begin
  if not IsUpdating and not (tstResizing in FState) and HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
end;

function TSpTBXTabToolbar.GetActiveTab: TSpTBXTabItem;
var
  Item: TTBCustomItem;
begin
  Result := nil;
  if Assigned(Items) and (FActiveTabIndex > -1) and (Items.Count > 0) and
    (FActiveTabIndex < Items.Count) then
  begin
    Item := Items[FActiveTabIndex];
    if not (csDestroying in Item.ComponentState) and (Item is TSpTBXTabItem) and Assigned(Item.Parent) then
      Result := Items[FActiveTabIndex] as TSpTBXTabItem;
  end;
end;

function TSpTBXTabToolbar.GetItemsTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := CurrentSkin.GetTextColor(skncTabToolbar, State);
  // Don't call inherited GetItemsTextColor, let the TabItem decide the color.
end;

function TSpTBXTabToolbar.GetTabsCount(VisibleOnly: Boolean): Integer;
var
  I: Integer;
  IV: TTBItemViewer;
begin
  Result := 0;
  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV.Item is TSpTBXTabItem then
      if VisibleOnly then begin
        if IV.Item.Visible then Inc(Result);
      end
      else
        Inc(Result);
  end;
end;

function TSpTBXTabToolbar.GetViewClass: TTBToolbarViewClass;
begin
  Result := TSpTBXTabToolbarView;
end;

procedure TSpTBXTabToolbar.Autofit;
var
  I, TabsCount, TabsWidth, TabsArea, NonTabsArea, RightAlignWidth: Integer;
  IV: TTBItemViewer;
  R: TRect;
begin
  if not FTabAutofit or IsUpdating or (Items.Count = 0) then Exit;

  View.ValidatePositions;
  View.BeginUpdate;
  try
    // Make all the clipped items visible
    for I := 0 to FHiddenTabs.Count - 1 do
      FHiddenTabs.Items[I].Item.Visible := True;
    FHiddenTabs.Clear;

    TabsCount := 0;
    TabsWidth := 0;
    NonTabsArea := 0;
    RightAlignWidth := 0;

    // Get TabsCount and NonTabsArea
    for I := 0 to View.ViewerCount - 1 do begin
      IV := View.Viewers[I];
      if IV.Item.Visible then begin
        if IV.Item is TSpTBXTabItem then
          Inc(TabsCount)
        else
          if IV.Item is TSpTBXRightAlignSpacerItem then
            Inc(RightAlignWidth, 20)
          else begin
            R := SpGetBoundsRect(IV, Items);
            Inc(NonTabsArea, R.Right - R.Left);
          end;
      end;
    end;

    // Get TabsArea
    if TabsCount > 0 then begin
      TabsArea := CurrentDock.ClientWidth - 4 - NonTabsArea - RightAlignWidth;
      TabsWidth := TabsArea div TabsCount;
      if TabsWidth > FTabAutofitMaxSize then
        TabsWidth := FTabAutofitMaxSize;
    end;

    // Get RightAlignWidth
    Inc(RightAlignWidth, CurrentDock.Width - ((TabsWidth * TabsCount) + NonTabsArea + RightAlignWidth));

    // Set TabsWidth and RightAlignWidth to the Items
    for I := 0 to View.ViewerCount - 1 do begin
      IV := View.Viewers[I];
      if IV.Item.Visible then begin
        if IV.Item is TSpTBXTabItem then
          TSpTBXTabItem(IV.Item).CustomWidth := TabsWidth
        else
          if IV.Item is TSpTBXRightAlignSpacerItem then
            TSpTBXRightAlignSpacerItem(IV.Item).CustomWidth := RightAlignWidth - GetRightAlignMargin;
      end;
    end;
  finally
    View.EndUpdate;
  end;
end;

procedure TSpTBXTabToolbar.RightAlignItems;
// Hide the Tab Items on resizing (only tab items!!!)
var
  I, J, W, H, VisibleTabsCount, iStart, iEnd: Integer;
  VisibleWidth, RightAlignedWidth, SpacerW, RightAlignedBorder: Integer;
  IV: TTBItemViewer;
  Spacer: TSpTBXItemViewer;
  RightAlignedList: TList;
  IsRotated, IsFirstPartiallyVisible, CanHide: Boolean;
begin
  if (csDestroying in ComponentState) or (tstRightAligning in FState) or
    not Assigned(CurrentDock) or (Items.Count <= 0) or
    not Stretch or (ShrinkMode <> tbsmNone) or
    (CurrentDock.Width <= 0) or (CurrentDock.Height <= 0) or IsUpdating then
      Exit;

  if FTabAutofit then begin
    Autofit;
    Exit;
  end;

  FState := FState + [tstRightAligning];
  View.ValidatePositions;
  View.BeginUpdate;
  RightAlignedList := TList.Create;
  try
    IsRotated := CurrentDock.Position in [dpLeft, dpRight];
    // Find the spacer and the right aligned items
    Spacer := SpGetRightAlignedItems(View, RightAlignedList, IsRotated, VisibleWidth, RightAlignedWidth);
    if Assigned(Spacer) then begin
      SpacerW := Spacer.BoundsRect.Right - Spacer.BoundsRect.Left;
      RightAlignedBorder := CurrentDock.Width - 2 - RightAlignedWidth + SpacerW;
      VisibleWidth := VisibleWidth - SpacerW;
      SpacerW := CurrentDock.Width - VisibleWidth - 4;
    end
    else begin
      SpacerW := 0;
      RightAlignedBorder := CurrentDock.Width - 2;
    end;

    // Show items
    VisibleTabsCount := GetTabsCount(True);
    IsFirstPartiallyVisible := False;
    if VisibleTabsCount = 1 then begin
      if VisibleWidth > CurrentDock.Width - 2 then
        IsFirstPartiallyVisible := True;
    end;

    if not IsFirstPartiallyVisible then begin
      IV := View.NextSelectable(nil, True);
      if Assigned(IV) then begin
        iStart := IV.Index;
        iEnd := IV.Index;
      end
      else begin
        iStart := 0;
        iEnd := 0;
      end;
      // Show items from left side of the first visible tab
      for I := iStart downto 0 do begin
        IV := View.Viewers[I];
        if not IV.Item.Visible and (RightAlignedList.IndexOf(IV) = -1) then begin
          // If the item was hidden and can be showed remove it from the HiddenList
          J := FHiddenTabs.IndexOf(IV.Item);
          if J > -1 then begin
            W := 0;
            H := 0;
            TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
            VisibleWidth := VisibleWidth + W;
            if (VisibleTabsCount = 0) or (VisibleWidth < CurrentDock.Width - 2) then begin
              SpacerW := SpacerW - W;
              FHiddenTabs.Delete(J);
              IV.Item.Visible := True;
              Inc(VisibleTabsCount);
            end
            else
              Break;
          end;
        end;
      end;
      // Show items from right side of the first visible tab
      for I := iEnd to View.ViewerCount - 1 do begin
        IV := View.Viewers[I];
        if not IV.Item.Visible and (RightAlignedList.IndexOf(IV) = -1) then begin
          // If the item was hidden and can be showed remove it from the HiddenList
          J := FHiddenTabs.IndexOf(IV.Item);
          if J > -1 then begin
            W := 0;
            H := 0;
            TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
            VisibleWidth := VisibleWidth + W;
            if (VisibleTabsCount = 0) or (VisibleWidth < CurrentDock.Width - 2) then begin
              SpacerW := SpacerW - W;
              FHiddenTabs.Delete(J);
              IV.Item.Visible := True;
              Inc(VisibleTabsCount);
            end
            else
              Break;
          end;
        end;
      end;

      // Hide tabitems
      if VisibleTabsCount > 1 then begin
        CanHide := False;
        for I := View.ViewerCount - 1 downto 0 do begin
          IV := View.Viewers[I];
          if (VisibleTabsCount > 1) and IV.Item.Visible and (RightAlignedList.IndexOf(IV) = -1) then begin
            if IV.BoundsRect.Right > RightAlignedBorder then
              CanHide := True;
            if CanHide and (IV is TSpTBXTabItemViewer) then begin
              W := IV.BoundsRect.Right - IV.BoundsRect.Left;
              // If the tabitem can't be showed add it to the HiddenList
              SpacerW := SpacerW + (IV.BoundsRect.Right - IV.BoundsRect.Left);
              FHiddenTabs.Add(IV.Item);
              IV.Item.Visible := False;
              Dec(VisibleTabsCount);
              CanHide := False;
            end;
          end;
        end;
      end;
    end;

    // Resize the spacer
    if Assigned(Spacer) then
      TSpTBXCustomItemAccess(Spacer.Item).CustomWidth := SpacerW;

    View.UpdatePositions;
  finally
    RightAlignedList.Free;
    View.EndUpdate;
    FState := FState - [tstRightAligning];
  end;
end;

procedure TSpTBXTabToolbar.TabClose(ATab: TSpTBXTabItem);
begin
  ATab.TabClose;
end;

procedure TSpTBXTabToolbar.MakeVisible(ATab: TSpTBXTabItem);
var
  TabIV, FirstIV, LastIV: TTBItemViewer;
  I: Integer;
  Spacer: TSpTBXItemViewer;
begin
  if (Items.Count > 1) and Assigned(ATab) and (ATab.Visible = False) then begin
    TabIV := View.Find(ATab);
    FirstIV := View.NextSelectable(nil, True);

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then
      LastIV := View.NextSelectable(Spacer, False)
    else
      LastIV := View.NextSelectable(nil, False);

    if Assigned(FirstIV) and Assigned(LastIV) then begin
      for I := 0 to View.ViewerCount - 1 do begin
        if TabIV.Index >= FirstIV.Index then
          ScrollRight
        else
          ScrollLeft;
        if TabIV.Item.Visible then
          Break;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.Scroll(ToRight: Boolean);
var
  FirstIV, LastIV: TTBItemViewer;
  I, VisibleWidth: Integer;
  Spacer: TSpTBXItemViewer;

  function ProcessScroll(IV: TTBItemViewer): Boolean;
  var
    IVIndex, ClippedIndex, VisibleTabsCount, W, H: Integer;
  begin
    Result := False;

    ClippedIndex := FHiddenTabs.IndexOf(IV.Item);
    if Assigned(IV) and (ClippedIndex > -1) then begin
      Result := True; // a clipped tab was found
      BeginUpdate;
      try
        VisibleTabsCount := GetTabsCount(True);
        // Try to hide all the necessary tabs
        W := 0;
        H := 0;
        TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
        if ToRight then begin
          while Assigned(FirstIV) and (VisibleWidth + W >= CurrentDock.ClientWidth - 2) do begin
            VisibleWidth := VisibleWidth - (FirstIV.BoundsRect.Right - FirstIV.BoundsRect.Left);
            FHiddenTabs.Add(FirstIV.Item);
            FirstIV.Item.Visible := False;
            FirstIV := SpGetNextTabItemViewer(View, FirstIV, True, sivtNormal);
            Dec(VisibleTabsCount);
          end;
        end
        else begin
          while Assigned(LastIV) and (VisibleWidth + W >= CurrentDock.ClientWidth - 2) do begin
            VisibleWidth := VisibleWidth - (LastIV.BoundsRect.Right - LastIV.BoundsRect.Left);
            FHiddenTabs.Add(LastIV.Item);
            LastIV.Item.Visible := False;
            LastIV := SpGetNextTabItemViewer(View, LastIV, False, sivtNormal);
            Dec(VisibleTabsCount);
          end;
        end;

        // Try to show all the necessary clipped tabs
        IVIndex := IV.Index;
        while Assigned(IV) and (ClippedIndex > -1) and ((VisibleTabsCount = 0) or (VisibleWidth + W <= CurrentDock.ClientWidth - 2)) do begin
          VisibleWidth := VisibleWidth + W;
          IV.Item.Visible := True;
          FHiddenTabs.Delete(ClippedIndex);
          Inc(VisibleTabsCount);

          if ToRight then
            Inc(IVIndex)
          else
            Dec(IVIndex);

          if (IVIndex > -1) and (IVIndex < View.ViewerCount) then begin
            IV := View.Viewers[IVIndex];
            ClippedIndex := FHiddenTabs.IndexOf(IV.Item);
            W := 0;
            H := 0;
            TTBItemViewerAccess(IV).CalcSize(Canvas, W, H);
          end
          else
            Break;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;

begin
  if (Items.Count > 1) and not FTabAutofit then begin
    // Find the first Tab
    FirstIV := SpGetNextTabItemViewer(View, nil, True, sivtNormal);

    // Get the VisibleWidth
    LastIV := View.NextSelectable(nil, False);
    if not Assigned(LastIV) then Exit;
    VisibleWidth := LastIV.BoundsRect.Right;

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then begin
      VisibleWidth := VisibleWidth - (Spacer.BoundsRect.Right - Spacer.BoundsRect.Left);
      LastIV := View.NextSelectable(Spacer, False);
    end;
    if Assigned(LastIV) and not (LastIV.Item is TSpTBXTabItem) then
      LastIV := SpGetNextTabItemViewer(View, LastIV, False, sivtNormal);

    if Assigned(FirstIV) and Assigned(LastIV) then begin
      if ToRight then begin
        // Find the first clipped tab from the right side of the tabset
        for I := LastIV.Index + 1 to View.ViewerCount - 1 do
          if ProcessScroll(View.Viewers[I]) then
            Break;
      end
      else begin
        // Find the first clipped tab from the left side of the tabset
        for I := FirstIV.Index - 1 downto 0 do
          if ProcessScroll(View.Viewers[I]) then
            Break;
      end;
    end;

  end;
end;

procedure TSpTBXTabToolbar.ScrollLeft;
begin
  Scroll(False);
end;

procedure TSpTBXTabToolbar.ScrollRight;
begin
  Scroll(True);
end;

procedure TSpTBXTabToolbar.ScrollState(out CanScrollToLeft, CanScrollToRight: Boolean);
var
  FirstIV, LastIV: TTBItemViewer;
  I, ClippedIndex: Integer;
  Spacer: TSpTBXItemViewer;
begin
  CanScrollToLeft := False;
  CanScrollToRight := False;
  if (FHiddenTabs.Count > 0) and not FTabAutofit then begin
    // Find the first Tab
    FirstIV := SpGetNextTabItemViewer(View, nil, True, sivtNormal);

    if Assigned(FirstIV) then begin
      // Find the first clipped tab from the left side of the tabset
      for I := FirstIV.Index - 1 downto 0 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then begin
          CanScrollToLeft := True;
          Break;
        end;
      end;
    end;

    // Find the last visible Tab minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    LastIV := SpGetNextTabItemViewer(View, Spacer, False, sivtNormal);

    // Find the first clipped tab from the right side of the tabset
    if Assigned(LastIV) then begin
      for I := LastIV.Index + 1 to View.ViewerCount - 1 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then begin
          CanScrollToRight := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetActiveTabIndex(Value: Integer);
var
  ATab, APrevTab: TSpTBXTabItem;
  I: Integer;
begin
  if not Assigned(FOwnerTabControl) then Exit;

  if (Value > -1) and (Value < Items.Count) and not (csDestroying in Items[Value].ComponentState) then begin
    if not (Items[Value] is TSpTBXTabItem) then
      Value := FActiveTabIndex;
  end
  else
    Value := -1;

  if (Value <> FActiveTabIndex) and FOwnerTabControl.CanActiveTabChange(FActiveTabIndex, Value) then
  begin
    I := FActiveTabIndex;
    FActiveTabIndex := Value;

    // Hide the previous TabSheet
    if (I > -1)  and (I < Items.Count) and not (csDestroying in Items[I].ComponentState) and
      (Items[I] is TSpTBXTabItem) then
    begin
      APrevTab := Items[I] as TSpTBXTabItem;
      APrevTab.Checked := False;
      if Assigned(APrevTab.Control) then
        APrevTab.Control.Visible := False;
    end;

    // Check the item and invalidate NC
    if FActiveTabIndex > -1 then begin
      // Show the TabSheet
      ATab := Items[FActiveTabIndex] as TSpTBXTabItem;
      ATab.Checked := True;
      if Assigned(ATab.Control) then begin
        ATab.Control.Visible := True;
        ATab.Control.BringToFront;
      end;
      MakeVisible(ATab);
    end;
    FOwnerTabControl.DoActiveTabChange(FActiveTabIndex);
    // To avoid flicker don't call InvalidateNC, use the following instead
    if not IsUpdating then begin
      View.InvalidatePositions;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOSIZE or
        SWP_NOMOVE or SWP_DRAWFRAME or SWP_SHOWWINDOW);
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetTabCloseButton(const Value: TSpTBXTabCloseButton);
begin
  if FTabCloseButton <> Value then begin
    FTabCloseButton := Value;
    View.InvalidatePositions;
    RightAlignItems;
    InvalidateNC;
  end;
end;

procedure TSpTBXTabToolbar.SetTabCloseButtonImageIndex(const Value: Integer);
begin
  if FTabCloseButtonImageIndex <> Value then begin
    FTabCloseButtonImageIndex := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTabToolbar.SetTabAutofit(const Value: Boolean);
begin
  if FTabAutofit <> Value then begin
    FTabAutofit := Value;
    if FTabAutofit then begin
      Autofit;
      InvalidateNC;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetTabAutofitMaxSize(const Value: Integer);
begin
  if FTabAutofitMaxSize <> Value then begin
    FTabAutofitMaxSize := Value;
    if FTabAutofit then Autofit;
  end;
end;

procedure TSpTBXTabToolbar.SetTabBackgroundBorders(const Value: Boolean);
begin
  if FTabBackgroundBorders <> Value then begin
    FTabBackgroundBorders := Value;
    InvalidateNC;
  end;
end;

procedure TSpTBXTabToolbar.SetTabColor(const Value: TColor);
begin
  if (FTabColor <> Value) then begin
    FTabColor := Value;
    if SkinManager.GetSkinType <> sknSkin then begin
      Invalidate;
      InvalidateNC;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetTabMaxSize(const Value: Integer);
begin
  if FTabMaxSize <> Value then begin
    FTabMaxSize := Value;
    View.InvalidatePositions;
    RightAlignItems;
    InvalidateNC;
  end;
end;

procedure TSpTBXTabToolbar.SetTabPosition(const Value: TSpTBXTabPosition);
begin
  if FTabPosition <> Value then
    FTabPosition := Value;
end;

procedure TSpTBXTabToolbar.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  P: TPoint;
  IV: TTBItemViewer;
  Shift: TShiftState;
begin
  // Allow left-clicks on TabItems at design time
  Shift := KeysToShiftState(Message.Keys);
  if (csDesigning in ComponentState) and (ssLeft in Shift) and Assigned(View) then begin
    P := SmallPointToPoint(Message.Pos);
    IV := View.ViewerFromPoint(P);
    if Assigned(IV) and Assigned(IV.Item) and (IV.Item is TSpTBXTabItem) then
      IV.Item.Click;
  end;

  inherited;
end;

function TSpTBXTabToolbar.CanDragCustomize(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
  TabIV: TSpTBXTabItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and (Button = mbLeft) then begin
    IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
    if Assigned(IV) and (IV is TSpTBXTabItemViewer) and Assigned(IV.Item) and IV.Item.Enabled and IV.Item.Visible then begin
      // Close the tab if the close button is pressed
      TabIV := TSpTBXTabItemViewer(IV);
      if (TabIV.TabCloseButtonState = sknsHotTrack) and TabIV.IsTabCloseButtonVisible then begin
        // Do nothing, let TSpTBXTabItemViewer.MouseUp handle the tab close
      end
      else begin
        // Click the item on mouse down
        if not IV.Item.Checked then begin
          Result := True; // Bypass the inherited mouse down
          IV.Item.Click;
          if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
        end;
        // Drag reorder
        if FTabDragReorder and not IsCustomizing and IV.Item.Checked then begin
          Result := True; // Bypass the inherited mouse down
          FBeginDragIV := IV;
          BeginDrag(False, 2);
        end;
      end;
    end
    else
      Result := inherited CanDragCustomize(Button, Shift, X, Y);
  end;
end;

procedure TSpTBXTabToolbar.DoStartDrag(var DragObject: TDragObject);
begin
  if FTabDragReorder and Assigned(FBeginDragIV) and Assigned(FBeginDragIV.Item) and (FBeginDragIV is TSpTBXTabItemViewer) then begin
    DragObject := TSpTBXTabItemDragObject.Create(Self, FBeginDragIV.Item);
    inherited DoStartDrag(DragObject);
  end
  else
    inherited DoStartDrag(DragObject);
end;

procedure TSpTBXTabToolbar.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  D: TSpTBXTabItemDragObject;
  DestIV, RightAlignIV: TTBItemViewer;
  OrigItem: TTBCustomItem;
  OrigPos, DestPos, RightAlignPos: Integer;
begin
  inherited DragOver(Source, X, Y, State, Accept);

  if FTabDragReorder and Assigned(Source) and (Source is TSpTBXTabItemDragObject) then begin
    D := Source as TSpTBXTabItemDragObject;
    OrigItem := D.SouceItem;
    OrigPos := OrigItem.Parent.IndexOf(OrigItem);

    // Move the dragging item in the toolbar
    if OrigItem.Parent = Items then begin
      Accept := True;
      SpGetDropPosItemViewer(Items, View, Point(X, Y), OrigPos, DestIV, DestPos);
      RightAlignIV := SpGetFirstRightAlignSpacer(View);
      if Assigned(RightAlignIV) then
        RightAlignPos := Items.IndexOf(RightAlignIV.Item)
      else
        RightAlignPos := -1;
      if (OrigPos <> DestPos) and (DestPos > -1) and (DestPos < Items.Count) and (OrigItem <> DestIV.Item) and
        not ((RightAlignPos > -1) and (DestPos >= RightAlignPos)) then
      begin
        if FOwnerTabControl.CanActiveTabReorder(OrigPos, DestPos) then begin
          BeginItemMove;
          View.BeginUpdate;
          try
            // The item is the active tab, we need to update the ActiveTabIndex
            // Just set the internal value because the page didn't change
            FActiveTabIndex := DestPos;
            Items.Move(OrigPos, DestPos);
            FOwnerTabControl.DoActiveTabReorder(DestPos);
          finally
            View.EndUpdate;
            EndItemMove;
            InvalidateNC;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  IV: TTBItemViewer;
begin
  if FTabCloseMiddleClick and (Button = mbMiddle) then begin
    IV := View.ViewerFromPoint(Point(X, Y));
    if Assigned(IV) and (IV is TSpTBXTabItemViewer) then begin
      TSpTBXTabItem(IV.Item).TabClose;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabSheet }

procedure TSpTBXTabSheet.AdjustClientRect(var Rect: TRect);
var
  Margin, XPMargin: Integer;
begin
  inherited AdjustClientRect(Rect);

  if Assigned(FTabControl) then begin
    Margin := 2;
    XPMargin := 2;
    // [Theme-Change]
    // WinXP theme needs to have 4 pixel margin
    if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then
      XPMargin := Margin + 2;

    Inc(Rect.Left, Margin);
    Dec(Rect.Right, XPMargin);
    case FTabControl.TabPosition of
      ttpTop:    dec(Rect.Bottom, XPMargin);
      ttpBottom: inc(Rect.Top, XPMargin);
    end;
  end;
end;

constructor TSpTBXTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csSetCaption];
  Align := alClient;
  Visible := False;
end;

procedure TSpTBXTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TSpTBXTabSheet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TabItem', ReadItemName, WriteItemName, True);
end;

destructor TSpTBXTabSheet.Destroy;
begin
  // If the Item <> nil it means the tabsheet was removed from the form at
  // designtime or freed at runtime.
  // If that happens TabDeleting was not called, we should call it before
  // the tabsheet is destroyed to free the Item and delete it from the
  // FPages list.
  if Assigned(FItem) then
    if Assigned(FTabControl) and not (csDestroying in FTabControl.ComponentState) then
      FTabControl.TabDeleting(FItem, False);

  FTabControl := nil;
  FItem := nil;
  inherited;
end;

procedure TSpTBXTabSheet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FPrevFocused then FPrevFocused := nil;
end;

procedure TSpTBXTabSheet.VisibleChanging;
begin
  if not (csDesigning in ComponentState) then
    if Visible then begin
      // TabSheet will be hidden, save the focused control
      if Assigned(FPrevFocused) then FPrevFocused.RemoveFreeNotification(Self);
      SpIsFocused(Self, FPrevFocused);
      if Assigned(FPrevFocused) then begin
        // If FPrevFocused is a RadioButton on a RadioGroup save the RadioGroup instead
        if Assigned(FPrevFocused.Parent) then
          if (FPrevFocused.Parent is TCustomRadioGroup) or (FPrevFocused.Parent is TSpTBXCustomRadioGroup) then
            FPrevFocused := FPrevFocused.Parent;
        FPrevFocused.FreeNotification(Self);
      end;
    end;

  inherited;
end;

procedure TSpTBXTabSheet.CMVisiblechanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    if Visible then begin
      // TabSheet was showed.
      // Focus the previous focused control, or focus the first child
      if Assigned(FPrevFocused) then begin
        FPrevFocused.RemoveFreeNotification(Self);
        if SpCanFocus(FPrevFocused) then
          if (FPrevFocused is TCustomRadioGroup) or (FPrevFocused is TSpTBXCustomRadioGroup) then
            SpFocusFirstChild(FPrevFocused)
          else
            FPrevFocused.SetFocus;
        FPrevFocused := nil;
      end
      else
        SpFocusFirstChild(Self);
    end;

  inherited;
end;

function TSpTBXTabSheet.GetCaption: WideString;
begin
  if Assigned(FItem) then Result := FItem.Caption
  else Result := '';
end;

function TSpTBXTabSheet.GetImageIndex: Integer;
begin
  if Assigned(FItem) then Result := FItem.ImageIndex
  else Result := -1;
end;

function TSpTBXTabSheet.GetTabVisible: Boolean;
begin
  if Assigned(FItem) then Result := FItem.Visible
  else Result := False;
end;

procedure TSpTBXTabSheet.SetCaption(const Value: WideString);
begin
  if Assigned(FItem) then FItem.Caption := Value;
end;

procedure TSpTBXTabSheet.SetImageIndex(const Value: Integer);
begin
  if Assigned(FItem) then FItem.ImageIndex := Value;
end;

procedure TSpTBXTabSheet.SetTabVisible(const Value: Boolean);
begin
  if Assigned(FItem) then FItem.Visible := Value;
end;

procedure TSpTBXTabSheet.ReadItemName(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
     FItemName := Reader.ReadString;
  else
    FItemName := Reader.ReadWideString;
  end;
end;

procedure TSpTBXTabSheet.WriteItemName(Writer: TWriter);
begin
  if Assigned(Item) then
    FItemName := Item.Name;
  Writer.WriteWideString(FItemName);
end;

procedure TSpTBXTabSheet.ReadState(Reader: TReader);
var
  C: TComponent;
  TC: TSpTBXCustomTabControl;
begin
  // The TabSheet is being created from the DFM stream
  // We must set the initial values of TabControl, Item and add itself to
  // the Pages list of the parent TabControl.

  inherited ReadState(Reader);
  if Reader.Parent is TSpTBXCustomTabControl then begin
    // Set TabControl
    TC := TSpTBXCustomTabControl(Reader.Parent);
    TabControl := TC;
    // Set Item and add Self to TabControl.Pages
    if not Assigned(FItem) and (FItemName <> '') then begin
      C := Owner.FindComponent(FItemName);
      if Assigned(C) and (C is TSpTBXTabItem) then begin
        FItem := C as TSpTBXTabItem;
        FItem.Control := Self;
        if TC.FPages.IndexOf(Self) = -1 then
          TC.FPages.Add(Self);
      end;
    end;
  end;
end;

procedure TSpTBXTabSheet.WMEraseBkgnd(var Message: TMessage);
var
  R: TRect;
begin
  Message.Result := 1;

  if Assigned(FTabControl) and Visible then begin
    if not DoubleBuffered or (Message.wParam = WPARAM(Message.lParam)) then begin
      R := ClientRect;
      if FTabControl.TabVisible then begin
        case FTabControl.TabPosition of
          ttpTop:    dec(R.Top, 4);
          ttpBottom: inc(R.Bottom, 4);
        end;
      end;
      FTabControl.DrawBackground(TWMEraseBkgnd(Message).DC, R);
    end;
  end;
end;

procedure TSpTBXTabSheet.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  SpInvalidateSpTBXControl(Self, True, True);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTabSet }

constructor TSpTBXCustomTabSet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  FTabVisible := True;
  FBackground := TBitmap.Create;

  Width := 289;
  Height := FDock.Height + 2;
  ParentColor := False;
  Color := clBtnFace;

  FToolbar.Items.RegisterNotification(ItemNotification);
end;

destructor TSpTBXCustomTabSet.Destroy;
begin
  FToolbar.Items.UnRegisterNotification(ItemNotification);
  FreeAndNil(FBackground);
  inherited;
end;

procedure TSpTBXCustomTabSet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('HiddenItems', ReadHiddenItems, WriteHiddenItems, True);
end;

procedure TSpTBXCustomTabSet.ReadHiddenItems(Reader: TReader);
begin
  if Reader.ReadValue = vaCollection then
    Reader.ReadCollection(Toolbar.FHiddenTabs);
end;

procedure TSpTBXCustomTabSet.WriteHiddenItems(Writer: TWriter);
begin
  Writer.WriteCollection(Toolbar.FHiddenTabs);
end;

procedure TSpTBXCustomTabSet.Loaded;
var
  I: Integer;
  CacheCollection: TSpTBXItemCacheCollection;
  Cache: TSpTBXItemCache;
  C: TComponent;
begin
  ActiveTabIndex := FLoadingActiveIndex;

  inherited;
  // Read the HiddenTabs collection, and fill the Item property of the
  // collection items reading the Name from the DFM
  CacheCollection := Toolbar.FHiddenTabs;
  if Assigned(CacheCollection) then
    for I := CacheCollection.Count - 1 downto 0 do begin
      Cache := CacheCollection[I];
      if not Assigned(Cache.Item) then begin
        if Cache.Name = '' then
          CacheCollection.Delete(I)
        else begin
          C := Owner.FindComponent(Cache.Name);
          if Assigned(C) and (C is TTBCustomItem) then begin
            Cache.Item := C as TTBCustomItem;
            // If the Item is visible then the entry is not valid, delete it
            if Cache.Item.Visible then
              CacheCollection.Delete(I);
          end;
        end;
      end;
    end;

  if TabAutofit then
    Toolbar.Autofit;
end;

function TSpTBXCustomTabSet.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXTabToolbar;
end;

function TSpTBXCustomTabSet.GetFullRepaint: Boolean;
begin
  Result := True;
end;

function TSpTBXCustomTabSet.Add(ACaption: WideString): TSpTBXTabItem;
var
  I: Integer;
  SpacerIV: TSpTBXItemViewer;
begin
  Result := TSpTBXTabItem.Create(Self);
  try
    Result.Caption := ACaption;
    SpacerIV := SpGetFirstRightAlignSpacer(View);

    if Assigned(SpacerIV) then begin
      I := Items.IndexOf(SpacerIV.Item);
      if I > -1 then
        Items.Insert(I, Result);
    end
    else
      Items.Add(Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

function TSpTBXCustomTabSet.Insert(NewIndex: Integer; ACaption: WideString): TSpTBXTabItem;
begin
  Result := TSpTBXTabItem.Create(Self);
  try
    Result.Caption := ACaption;
    Items.Insert(NewIndex, Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TSpTBXCustomTabSet.TabClick(ATab: TSpTBXTabItem);
begin
  ATab.Click; // calls TabToolbar.DoTabClick and Self.DoTabClick
end;

function TSpTBXCustomTabSet.CanActiveTabChange(const TabIndex, NewTabIndex: Integer): Boolean;
begin
  Result := True;
  if not (csLoading in ComponentState) then
    if (NewTabIndex > -1) and not Items[NewTabIndex].Checked then
      if Assigned(FOnActiveTabChanging) then FOnActiveTabChanging(Self, TabIndex, NewTabIndex, Result);
end;

procedure TSpTBXCustomTabSet.DoActiveTabChange(const TabIndex: Integer);
begin
  if not (csLoading in ComponentState) then
    if Assigned(FOnActiveTabChange) then FOnActiveTabChange(Self, TabIndex);
end;

function TSpTBXCustomTabSet.CanActiveTabReorder(const TabIndex, NewTabIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnActiveTabReordering) then FOnActiveTabReordering(Self, TabIndex, NewTabIndex, Result);
end;

procedure TSpTBXCustomTabSet.DoActiveTabReorder(const TabIndex: Integer);
begin
  if Assigned(FOnActiveTabReorder) then FOnActiveTabReorder(Self, TabIndex);
end;

procedure TSpTBXCustomTabSet.MakeVisible(ATab: TSpTBXTabItem);
begin
  if Assigned(FToolbar) then Toolbar.MakeVisible(ATab);
end;

procedure TSpTBXCustomTabSet.ScrollLeft;
begin
  if Assigned(FToolbar) then Toolbar.ScrollLeft;
end;

procedure TSpTBXCustomTabSet.ScrollRight;
begin
  if Assigned(FToolbar) then Toolbar.ScrollRight;
end;

procedure TSpTBXCustomTabSet.ScrollState(out Left, Right: Boolean);
begin
  if Assigned(FToolbar) then Toolbar.ScrollState(Left, Right);
end;

function TSpTBXCustomTabSet.GetActiveTab: TSpTBXTabItem;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.ActiveTab
  else
    Result := nil;
end;

function TSpTBXCustomTabSet.GetActiveTabIndex: Integer;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.ActiveTabIndex
  else
    Result := -1;
end;

procedure TSpTBXCustomTabSet.SetActiveTabIndex(Value: Integer);
begin
  // When the component is reading from the DFM the Items are not created.
  // We must save the value setted at design time and use it when the
  // form is finally loaded.
  if csReading in ComponentState then
    FLoadingActiveIndex := Value
  else
    if Assigned(FToolbar) then
      Toolbar.ActiveTabIndex := Value;
end;

function TSpTBXCustomTabSet.GetTabCloseButton: TSpTBXTabCloseButton;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabCloseButton
  else
    Result := tcbNone;
end;

procedure TSpTBXCustomTabSet.SetTabCloseButton(const Value: TSpTBXTabCloseButton);
begin
  if Assigned(FToolbar) then
    Toolbar.TabCloseButton := Value;
end;

function TSpTBXCustomTabSet.GetTabCloseButtonImageIndex: Integer;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabCloseButtonImageIndex
  else
    Result := -1;
end;

procedure TSpTBXCustomTabSet.SetTabCloseButtonImageIndex(const Value: Integer);
begin
  if Assigned(FToolbar) then
    Toolbar.TabCloseButtonImageIndex := Value;
end;

function TSpTBXCustomTabSet.GetTabCloseMiddleClick: Boolean;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabCloseMiddleClick
  else
    Result := False;
end;

procedure TSpTBXCustomTabSet.SetTabCloseMiddleClick(const Value: Boolean);
begin
  if Assigned(FToolbar) then
    Toolbar.TabCloseMiddleClick := Value;
end;

function TSpTBXCustomTabSet.GetTabAutofit: Boolean;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabAutofit
  else
    Result := False;
end;

procedure TSpTBXCustomTabSet.SetTabAutofit(const Value: Boolean);
begin
  if Assigned(FToolbar) then
    Toolbar.TabAutofit := Value;
end;

function TSpTBXCustomTabSet.GetTabAutofitMaxSize: Integer;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabAutofitMaxSize
  else
    Result := -1;
end;

procedure TSpTBXCustomTabSet.SetTabAutofitMaxSize(const Value: Integer);
begin
  if Assigned(FToolbar) then
    Toolbar.TabAutofitMaxSize := Value;
end;

function TSpTBXCustomTabSet.GetTabBackgroundBorders: Boolean;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabBackgroundBorders
  else
    Result := True;
end;

procedure TSpTBXCustomTabSet.SetTabBackgroundBorders(const Value: Boolean);
begin
  if Assigned(FToolbar) then Toolbar.TabBackgroundBorders := Value
end;

function TSpTBXCustomTabSet.GetTabBackgroundColor: TColor;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Color
  else
    Result := clNone;
end;

procedure TSpTBXCustomTabSet.SetTabBackgroundColor(const Value: TColor);
begin
  if Assigned(FToolbar) then FToolbar.Color := Value
end;

function TSpTBXCustomTabSet.GetTabDragReorder: Boolean;
begin
  Result := False;
  if Assigned(FToolbar) then
    Result := Toolbar.TabDragReorder;
end;

procedure TSpTBXCustomTabSet.SetTabDragReorder(const Value: Boolean);
begin
  if Assigned(FToolbar) then
    Toolbar.TabDragReorder := Value;
end;

function TSpTBXCustomTabSet.GetTabMaxSize: Integer;
begin
  Result := -1;
  if Assigned(FToolbar) then
    Result := Toolbar.TabMaxSize;
end;

procedure TSpTBXCustomTabSet.SetTabMaxSize(const Value: Integer);
begin
  if Assigned(FToolbar) then
    Toolbar.TabMaxSize := Value;
end;

function TSpTBXCustomTabSet.GetTabToolbar: TSpTBXTabToolbar;
begin
  Result := FToolbar as TSpTBXTabToolbar;
end;

function TSpTBXCustomTabSet.GetTabPosition: TSpTBXTabPosition;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabPosition
  else
    Result := ttpTop;
end;

procedure TSpTBXCustomTabSet.SetTabPosition(const Value: TSpTBXTabPosition);
var
  T: TSpTBXTabToolbar;
begin
  if Assigned(FToolbar) and Assigned(FDock) then begin
    T := Toolbar;
    if T.TabPosition <> Value then begin
      T.Visible := False;
      T.Parent := nil;
      T.TabPosition := Value;
      case Value of
        ttpTop:    FDock.Position := dpTop;
        ttpBottom: FDock.Position := dpBottom;
      end;
      T.CurrentDock := FDock;
      T.Visible := True;

      InvalidateBackground;
    end;
  end;
end;

procedure TSpTBXCustomTabSet.SetTabVisible(const Value: Boolean);
begin
  if FTabVisible <> Value then begin
    FTabVisible := Value;
    if Assigned(FDock) then
      FDock.Visible := Value;
  end;
end;

function TSpTBXCustomTabSet.GetTabSetHeight: Integer;
begin
  if Assigned(FDock) then
    Result := FDock.Height
  else
    Result := 0;
end;

procedure TSpTBXCustomTabSet.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXCustomTabSet.DrawBackground(DC: HDC; ARect: TRect): Boolean;
var
  ACanvas: TCanvas;
  PaintDefault: Boolean;
  R: TRect;
begin
  Result := False;
  if (csDestroying in ComponentState) or not Assigned(FDock) or
    not Assigned(FBackground) or IsRectEmpty(ARect) then Exit;

  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := DC;
    ACanvas.Lock;
    R := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

    if (FBackground.Width = R.Right) and (FBackground.Height = R.Bottom) and not Assigned(FOnDrawBackground) then
      ACanvas.Draw(ARect.Left, ARect.Top, FBackground)
    else begin
      FBackground.Width := R.Right;
      FBackground.Height := R.Bottom;
      FBackground.Canvas.Brush.Color := clWhite;
      FBackground.Canvas.FillRect(R);

      PaintDefault := True;
      DoDrawBackground(FBackground.Canvas, R, pstPrePaint, PaintDefault);
      if PaintDefault then
        SpDrawXPTabControlBackground(FBackground.Canvas, R, Color, TabPosition = ttpBottom);
      PaintDefault := True;
      DoDrawBackground(FBackground.Canvas, R, pstPostPaint, PaintDefault);

      ACanvas.Draw(ARect.Left, ARect.Top, FBackground);
    end;

    Result := True;
  finally
    ACanvas.Unlock;
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXCustomTabSet.InvalidateBackground(InvalidateChildren: Boolean);
begin
  // Force background repaint
  if not (csDestroying in ComponentState) and Assigned(FToolbar) then begin
    if Assigned(FBackground) then
      FBackground.Width := 1;
    SpInvalidateSpTBXControl(Self, InvalidateChildren, FResizing);
  end;
end;

procedure TSpTBXCustomTabSet.ItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  I: Integer;
  Tab: TSpTBXTabItem;
begin
  inherited;

  if Assigned(FToolbar) and not Relayed and not FToolbar.IsItemMoving then
    case Action of
      tbicSubitemsBeginUpdate:
        begin
          // When a Tab item is moved (TTBCustomItem.Move)
          // tbicDeleting and tbicInserted change actions are fired
          // but we don't want the associated TabSheet to be recreated
          // because the children will be destroyed.
          // When a TTBCustomItem is moved it is not recreated, it simply
          // deletes and reinserts its reference in the items array.
          // We need to find out if the item is being moved and stop the
          // TabSheet recreation.
          // The action sequence for a move operation is the following:
          // tbicSubitemsBeginUpdate    (FItemMoveCount = 1)
          //   tbicDeleting             (FItemMoveCount = 2)
          //   tbicSubitemsBeginUpdate  (FItemMoveCount = 1)
          //     tbicInserted           (FItemMoveCount = 0)
          //   tbicSubitemsEndUpdate    (FItemMoveCount = 0)
          // tbicSubitemsEndUpdate      (FItemMoveCount = 0)
          FItemMoveCount := 1;
          FItemMoved := nil;
        end;
      tbicSubitemsEndUpdate:
        begin
          // Destroy the TabSheet if the sequence was:
          // tbicSubitemsBeginUpdate - tbicDeleting - tbicSubitemsEndUpdate
          if FItemMoveCount = 2 then
            TabDeleting(FItemMoved);
          FItemMoveCount := 0;
          FItemMoved := nil;
        end;
      tbicInserted:
        if Assigned(Item) then begin
          // Update the index if a new item is inserted before the ActiveTabIndex
          I := Items.IndexOf(Item);
          if (I > -1) and (I <= ActiveTabIndex) then begin
            FUpdatingIndex := True;
            try
              // Don't change the ActiveTabIndex, just set the internal value
              // because the page didn't change
              Toolbar.FActiveTabIndex := Toolbar.FActiveTabIndex + 1;
            finally
              FUpdatingIndex := False;
            end;
          end;
          if (Item is TSpTBXTabItem) then
            TabInserted(Item as TSpTBXTabItem);
          InvalidateBackground;
          FItemMoveCount := 0;
          FItemMoved := nil;
        end;
      tbicDeleting:
        // The ItemViewer of the Item is not valid, it was destroyed by TTBView
        // The Items array still has the Item.
        if not (csDestroying in ComponentState) and Assigned(Item) then begin
          FUpdatingIndex := True;
          try
            Tab := nil;
            I := Items.IndexOf(Item);
            if I > -1 then begin
              if I < ActiveTabIndex then
                // Don't change the ActiveTabIndex, just set the internal value
                // because the page didn't change
                Toolbar.FActiveTabIndex := Toolbar.FActiveTabIndex - 1
              else
                if I = ActiveTabIndex then
                  if I = 0 then begin
                    if (Items.Count > 1) and (Items[1] is TSpTBXTabItem) then begin
                      // The first tab was deleted, change the internal value of
                      // Update the checked tab on WM_INVALIDATETABBACKGROUND
                      Tab := Items[1] as TSpTBXTabItem;
                      Tab.Click;
                    end
                    else
                      Toolbar.FActiveTabIndex := -1;
                  end
                  else begin
                    Dec(I); // Prev tab
                    if (I > -1) and (I < Items.Count) and (Items[I] is TSpTBXTabItem) then
                      SetActiveTabIndex(I)
                    else
                      Toolbar.FActiveTabIndex := -1;
                  end;
            end;

            if (Item is TSpTBXTabItem) then
              if FItemMoveCount = 1 then begin
                FItemMoveCount := 2;
                FItemMoved := Item as TSpTBXTabItem;
              end
              else begin
                FItemMoveCount := 0;
                TabDeleting(Item as TSpTBXTabItem);
              end;

            if (csDesigning in ComponentState) or Assigned(Tab) then
              PostMessage(Handle, WM_INVALIDATETABBACKGROUND, 0, 0)
            else
              InvalidateBackground;
          finally
            FUpdatingIndex := False;
          end;
        end;
      tbicInvalidate:
        // When the Item.Checked property changes we must reset the ActiveTabIndex
        if not FUpdatingIndex and Assigned(Item) and (Item is TSpTBXTabItem) and
          Item.Checked and Item.Enabled then
        begin
          I := Items.IndexOf(Item);
          if I <> ActiveTabIndex then begin
            FUpdatingIndex := True;
            try
              SetActiveTabIndex(I);
            finally
              FUpdatingIndex := False;
            end;
          end;
        end;
    end;
end;

procedure TSpTBXCustomTabSet.TabDeleting(Item: TSpTBXTabItem;
  FreeTabSheet: Boolean);
begin
  if not (csDestroying in ComponentState) then
    ScrollLeft;
end;

procedure TSpTBXCustomTabSet.TabInserted(Item: TSpTBXTabItem);
var
  I: Integer;
  IV: TTBItemViewer;
begin
  if not (csLoading in ComponentState) and Assigned(Item) and (Item is TSpTBXTabItem) then
    if Items.Count = 1 then
      Item.Click // Select the first Tab
    else begin
      // If the item is inserted after a hidden tab, we should also hide it
      I := Items.IndexOf(Item) - 1;
      if I > -1 then
        if Toolbar.FHiddenTabs.IndexOf(Items[I]) > -1 then begin
          IV := View.Find(Item);
          if Assigned(IV) then begin
            Toolbar.BeginUpdate;
            try
              Toolbar.FHiddenTabs.Add(Item);
              Item.Visible := False;
            finally
              Toolbar.EndUpdate;
            end;
          end;
        end;
    end;
end;

procedure TSpTBXCustomTabSet.CMColorchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FToolbar) then begin
    Toolbar.TabColor := Color;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomTabSet.CMSpTBXControlsInvalidate(var Message: TMessage);
begin
  InvalidateBackground;
  Message.Result := 1;
end;

procedure TSpTBXCustomTabSet.WMEraseBkgnd(var Message: TMessage);
var
  R: TRect;
begin
  if not DoubleBuffered or (Message.wParam = WPARAM(Message.lParam)) then begin
    if not (csDestroying in ComponentState) and GetFullRepaint then begin
      R := ClientRect;
      if FTabVisible then begin
        case TabPosition of
          ttpTop:    Inc(R.Top, GetTabSetHeight - 2);
          ttpBottom: Dec(R.Bottom, GetTabSetHeight - 2);
        end;
      end;
      DrawBackground(TWMEraseBkgnd(Message).DC, R);
    end;
  end;
  Message.Result := 1;
end;

procedure TSpTBXCustomTabSet.WMInvalidateTabBackground(var Message: TMessage);
var
  Tab: TSpTBXTabItem;
  I: Integer;
begin
  if Assigned(FToolbar) then begin
    Tab := Toolbar.ActiveTab;
    if Assigned(Tab) then begin
      I := Items.IndexOf(Tab);
      FUpdatingIndex := True;
      try
        Toolbar.FActiveTabIndex := -1;
        SetActiveTabIndex(I);
      finally
        FUpdatingIndex := False;
      end;
    end;
    if HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TSpTBXCustomTabSet.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  FResizing := True;
  inherited;
  if GetFullRepaint then
    InvalidateBackground;
  FResizing := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTabControl }

constructor TSpTBXCustomTabControl.Create(AOwner: TComponent);
begin
  inherited;
//  ControlStyle := ControlStyle - [csAcceptsControls];
  FPages := TList.Create;

  // FEmptyTabSheet is used to hide the rest of the TabSheets
  // when ActiveTabIndex = -1 at design time.
  FEmptyTabSheet := TSpTBXTabSheet.Create(Self);
  FEmptyTabSheet.Parent := Self;
  FEmptyTabSheet.TabControl := Self;
  FEmptyTabSheet.Item := nil;
  FEmptyTabSheet.Visible := True;
  FEmptyTabSheet.BringToFront;
  FEmptyTabSheet.ControlStyle := FEmptyTabSheet.ControlStyle - [csAcceptsControls];

  Width := 289;
  Height := 193;
end;

destructor TSpTBXCustomTabControl.Destroy;
begin
  FPages.Free;
  inherited;
end;

procedure TSpTBXCustomTabControl.DoActiveTabChange(const ItemIndex: Integer);
begin
  if ItemIndex = -1 then begin
    FEmptyTabSheet.Visible := True;
    FEmptyTabSheet.BringToFront;
  end
  else
    FEmptyTabSheet.Visible := False;
  inherited;
end;

procedure TSpTBXCustomTabControl.RealignTabSheets;
var
  I, C: Integer;
begin
  if HandleAllocated then begin
    C := PagesCount;
    for I := 0 to C - 1 do
      Pages[I].Realign;
  end;
end;

function TSpTBXCustomTabControl.GetFullRepaint: Boolean;
begin
  if not (csDestroying in ComponentState) then
    Result := not Assigned(FPages) or (FPages.Count = 0) or not Assigned(FToolbar) or
      not Assigned(Toolbar.ActiveTab) or not Toolbar.ActiveTab.Checked
  else
    Result := False;
end;

function TSpTBXCustomTabControl.GetPage(Item: TSpTBXTabItem): TSpTBXTabSheet;
var
  I: Integer;
begin
  Result := nil;
  I := Items.IndexOf(Item);
  if (I > - 1) and Assigned(Item.Control) and (Item.Control is TSpTBXTabSheet) then
    Result := Item.Control as TSpTBXTabSheet;
end;

function TSpTBXCustomTabControl.GetActivePage: TSpTBXTabSheet;
begin
  if ActiveTabIndex > -1 then
    Result := GetPage(Items[ActiveTabIndex] as TSpTBXTabItem)
  else
    Result := nil;
end;

procedure TSpTBXCustomTabControl.SetActivePage(const Value: TSpTBXTabSheet);
var
  I: Integer;
begin
  if Assigned(Value) and (FPages.IndexOf(Value) > -1) and Assigned(FToolbar) then begin
    I := FToolbar.Items.IndexOf(Value.Item);
    if I > -1 then ActiveTabIndex := I;
  end;
end;

function TSpTBXCustomTabControl.GetPages(Index: Integer): TSpTBXTabSheet;
begin
  Result := TSpTBXTabSheet(FPages[Index]);
end;

function TSpTBXCustomTabControl.GetPagesCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TSpTBXCustomTabControl.TabInserted(Item: TSpTBXTabItem);
var
  T: TSpTBXTabSheet;
  I: Integer;
begin
  // Create a TabSheet and Link it to the TabItem, only if the Item is created
  // at DesignTime.
  // If the Item is created from the DFM stream then the TabSheet will be
  // automatically created, because it will also be streamed, but it won't be
  // linked to the Item, this is done in TabSheet.ReadState

  if (csLoading in ComponentState) or not Assigned(Item) then Exit;

  for I := 0 to FPages.Count - 1 do begin
    T := TSpTBXTabSheet(FPages[I]);
    if T.Item = Item then begin
      Exit;
      raise Exception.Create('TabSheet Already Exists');
    end;
  end;

  // Find unique name
  I := 1;
  while Owner.FindComponent('SpTBXTabSheet' + IntToStr(I)) <> nil do
    inc(I);

  // The Form will be the owner, it will stream the tabsheet to the DFM
  T := TSpTBXTabSheet.Create(Owner);
  T.Name := 'SpTBXTabSheet' + IntToStr(I);
  T.Parent := Self;
  T.TabControl := Self;
  T.Item := Item;
  Item.Control := T;
  T.SendToBack;
  FPages.Add(T);

  inherited;
end;

procedure TSpTBXCustomTabControl.TabDeleting(Item: TSpTBXTabItem;
  FreeTabSheet: Boolean);
var
  I: Integer;
  T: TSpTBXTabSheet;
begin
  inherited;
  // The Toolbar will free the Items, and the Form will free the TabSheets
  if (csDestroying in ComponentState) or not Assigned(Item) then Exit;

  for I := 0 to FPages.Count - 1 do begin
    T := TSpTBXTabSheet(FPages[I]);
    if Assigned(T) and Assigned(T.Item) and (T.Item = Item) then begin
      FPages[I] := nil;
      FPages.Delete(I);
      if FreeTabSheet then begin
        T.Item := nil;
        T.Free;
      end
      else begin
        // TabSheet deleted at design time, free the linked Item
        T.Item.Free;
        T.Item := nil;
      end;
      Break;
    end;
  end;
end;

procedure TSpTBXCustomTabControl.CMSpTBXControlsInvalidate(var Message: TMessage);
var
  I, C: Integer;
begin
  // Force TabControl and TabSheets background repaint
  inherited;
  if not (csDestroying in ComponentState) and Assigned(FToolbar) then begin
    C := PagesCount;
    for I := 0 to C - 1 do
      SpInvalidateSpTBXControl(Pages[I], True, True);
  end;
  Message.Result := 1;
end;

procedure TSpTBXCustomTabControl.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  RealignTabSheets;
end;

end.
