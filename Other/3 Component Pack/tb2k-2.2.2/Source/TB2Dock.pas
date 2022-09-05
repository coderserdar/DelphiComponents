unit TB2Dock;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Dock.pas,v 1.127 2008/09/17 20:12:25 jr Exp $
}

interface

{x$DEFINE TB2Dock_DisableLock}
{ Remove the 'x' to enable the define. It will disable calls to
  LockWindowUpdate, which it calls to disable screen updates while dragging.
  You may want to temporarily enable the define while debugging so you are able
  to see your code window while stepping through the dragging routines. }

{$I TB2Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, IniFiles;

type
  TTBCustomForm = {$IFDEF JR_D3} TCustomForm {$ELSE} TForm {$ENDIF};

  { TTBDock }

  TTBDockBoundLinesValues = (blTop, blBottom, blLeft, blRight);
  TTBDockBoundLines = set of TTBDockBoundLinesValues;
  TTBDockPosition = (dpTop, dpBottom, dpLeft, dpRight);
  TTBDockType = (dtNotDocked, dtFloating, dtTopBottom, dtLeftRight);
  TTBDockableTo = set of TTBDockPosition;

  TTBCustomDockableWindow = class;
  TTBBasicBackground = class;

  TTBInsertRemoveEvent = procedure(Sender: TObject; Inserting: Boolean;
    Bar: TTBCustomDockableWindow) of object;
  TTBRequestDockEvent = procedure(Sender: TObject; Bar: TTBCustomDockableWindow;
    var Accept: Boolean) of object;

  TTBDock = class(TCustomControl)
  private
    { Property values }
    FPosition: TTBDockPosition;
    FAllowDrag: Boolean;
    FBoundLines: TTBDockBoundLines;
    FBackground: TTBBasicBackground;
    FBkgOnToolbars: Boolean;
    FFixAlign: Boolean;
    FCommitNewPositions: Boolean;
    FLimitToOneRow: Boolean;
    FOnInsertRemoveBar: TTBInsertRemoveEvent;
    FOnRequestDock: TTBRequestDockEvent;
    {$IFNDEF JR_D4}
    FOnResize: TNotifyEvent;
    {$ENDIF}

    { Internal }
    FDisableArrangeToolbars: Integer;  { Increment to disable ArrangeToolbars }
    FArrangeToolbarsNeeded: Boolean;
    FNonClientWidth, FNonClientHeight: Integer;
    DockList: TList;  { List of the toolbars docked, and those floating and have LastDock
                        pointing to the dock. Items are casted in TTBCustomDockableWindow's. }
    DockVisibleList: TList;  { Similar to DockList, but lists only docked and visible toolbars }

    { Property access methods }
    //function GetVersion: TToolbar97Version;
    procedure SetAllowDrag(Value: Boolean);
    procedure SetBackground(Value: TTBBasicBackground);
    procedure SetBackgroundOnToolbars(Value: Boolean);
    procedure SetBoundLines(Value: TTBDockBoundLines);
    procedure SetFixAlign(Value: Boolean);
    procedure SetPosition(Value: TTBDockPosition);
    //procedure SetVersion(const Value: TToolbar97Version);

    function GetToolbarCount: Integer;
    function GetToolbars(Index: Integer): TTBCustomDockableWindow;

    { Internal }
    procedure BackgroundChanged(Sender: TObject);
    procedure ChangeDockList(const Insert: Boolean; const Bar: TTBCustomDockableWindow);
    procedure ChangeWidthHeight(const NewWidth, NewHeight: Integer);
    procedure CommitPositions;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
      const Clip: HRGN);
    function GetDesignModeRowOf(const XY: Integer): Integer;
    function HasVisibleToolbars: Boolean;
    procedure RelayMsgToFloatingBars({$IFNDEF CLR}var{$ELSE}const{$ENDIF} Message: TMessage);
    function ToolbarVisibleOnDock(const AToolbar: TTBCustomDockableWindow): Boolean;
    procedure ToolbarVisibilityChanged(const Bar: TTBCustomDockableWindow;
      const ForceRemove: Boolean);

    { Messages }
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    {$IFNDEF JR_D4}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ENDIF}
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPrintClient(var Message: {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF}); message WM_PRINTCLIENT;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBackground(DC: HDC; const DrawRect: TRect); virtual;
    function GetPalette: HPALETTE; override;
    procedure InvalidateBackgrounds;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    function UsingBackground: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ArrangeToolbars;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetCurrentRowSize(const Row: Integer; var AFullSize: Boolean): Integer;
    function GetHighestRow(const HighestEffective: Boolean): Integer;
    function GetMinRowSize(const Row: Integer;
      const ExcludeControl: TTBCustomDockableWindow): Integer;

    property CommitNewPositions: Boolean read FCommitNewPositions write FCommitNewPositions;
    property NonClientWidth: Integer read FNonClientWidth;
    property NonClientHeight: Integer read FNonClientHeight;
    property ToolbarCount: Integer read GetToolbarCount;
    property Toolbars[Index: Integer]: TTBCustomDockableWindow read GetToolbars;
  published
    property AllowDrag: Boolean read FAllowDrag write SetAllowDrag default True;
    property Background: TTBBasicBackground read FBackground write SetBackground;
    property BackgroundOnToolbars: Boolean read FBkgOnToolbars write SetBackgroundOnToolbars default True;
    property BoundLines: TTBDockBoundLines read FBoundLines write SetBoundLines default [];
    property Color default clBtnFace;
    property FixAlign: Boolean read FFixAlign write SetFixAlign default False;
    property LimitToOneRow: Boolean read FLimitToOneRow write FLimitToOneRow default False;
    property PopupMenu;
    property Position: TTBDockPosition read FPosition write SetPosition default dpTop;
    //property Version: TToolbar97Version read GetVersion write SetVersion stored False;
    property Visible;

    {$IFDEF JR_D5}
    property OnContextPopup;
    {$ENDIF}
    property OnInsertRemoveBar: TTBInsertRemoveEvent read FOnInsertRemoveBar write FOnInsertRemoveBar;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnRequestDock: TTBRequestDockEvent read FOnRequestDock write FOnRequestDock;
    {$IFDEF JR_D4}
    property OnResize;
    {$ELSE}
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    {$ENDIF}
  end;

  { TTBFloatingWindowParent - internal }

  TTBToolWindowNCRedrawWhatElement = (twrdBorder, twrdCaption, twrdCloseButton);
  TTBToolWindowNCRedrawWhat = set of TTBToolWindowNCRedrawWhatElement;

  TTBFloatingWindowParentClass = class of TTBFloatingWindowParent;
  TTBFloatingWindowParent = class(TCustomForm)
  private
    FCloseButtonDown: Boolean; { True if Close button is currently depressed }
    FDockableWindow: TTBCustomDockableWindow;
    FParentForm: TTBCustomForm;
    FShouldShow: Boolean;

    procedure CallRecreateWnd;
    function GetCaptionRect(const AdjustForBorder, MinusCloseButton: Boolean): TRect;
    function GetCloseButtonRect(const AdjustForBorder: Boolean): TRect;
    procedure SetCloseButtonState(Pushed: Boolean);
    procedure RedrawNCArea(const RedrawWhat: TTBToolWindowNCRedrawWhat);

    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDblClk(var Message: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMNCRButtonUp(var Message: TWMNCRButtonUp); message WM_NCRBUTTONUP;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPrintClient(var Message: {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF}); message WM_PRINTCLIENT;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
      const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DockableWindow: TTBCustomDockableWindow read FDockableWindow;
    property CloseButtonDown: Boolean read FCloseButtonDown;
  public
    property ParentForm: TTBCustomForm read FParentForm;
  end;

  { TTBCustomDockableWindow }

  TTBDockChangingEvent = procedure(Sender: TObject; Floating: Boolean;
    DockingTo: TTBDock) of object;
  TTBDragHandleStyle = (dhDouble, dhNone, dhSingle);
  TTBDockMode = (dmCanFloat, dmCannotFloat, dmCannotFloatOrChangeDocks);
  TTBFloatingMode = (fmOnTopOfParentForm, fmOnTopOfAllForms);
  TTBSizeHandle = (twshLeft, twshRight, twshTop, twshTopLeft,
    twshTopRight, twshBottom, twshBottomLeft, twshBottomRight);
    { ^ must be in same order as HTLEFT..HTBOTTOMRIGHT }
  TTBPositionExtraData = {$IFNDEF CLR} Pointer {$ELSE} TObject {$ENDIF};
  TTBPositionReadIntProc = function(const ToolbarName, Value: String; const Default: Longint;
    const ExtraData: TTBPositionExtraData): Longint;
  TTBPositionReadStringProc = function(const ToolbarName, Value, Default: String;
    const ExtraData: TTBPositionExtraData): String;
  TTBPositionWriteIntProc = procedure(const ToolbarName, Value: String; const Data: Longint;
    const ExtraData: TTBPositionExtraData);
  TTBPositionWriteStringProc = procedure(const ToolbarName, Value, Data: String;
    const ExtraData: TTBPositionExtraData);
  TTBReadPositionData = record
    ReadIntProc: TTBPositionReadIntProc;
    ReadStringProc: TTBPositionReadStringProc;
    ExtraData: TTBPositionExtraData;
  end;
  TTBWritePositionData = record
    WriteIntProc: TTBPositionWriteIntProc;
    WriteStringProc: TTBPositionWriteStringProc;
    ExtraData: TTBPositionExtraData;
  end;
  TTBDockableWindowStyles = set of (tbdsResizeEightCorner, tbdsResizeClipCursor);
  TTBShrinkMode = (tbsmNone, tbsmWrap, tbsmChevron);

  TTBCustomDockableWindow = class(TCustomControl)
  private
    { Property variables }
    FAutoResize: Boolean;
    FDockPos, FDockRow, FEffectiveDockPos, FEffectiveDockRow: Integer;
    FDocked: Boolean;
    FCurrentDock, FDefaultDock, FLastDock: TTBDock;
    FCurrentSize: Integer;
    FFloating: Boolean;
    FOnClose, FOnDockChanged, FOnMove, FOnRecreated,
      FOnRecreating, {$IFNDEF JR_D4} FOnResize, {$ENDIF}
      FOnVisibleChanged: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnDockChanging, FOnDockChangingHidden: TTBDockChangingEvent;
    FActivateParent, FHideWhenInactive, FCloseButton, FCloseButtonWhenDocked,
      FFullSize, FResizable, FShowCaption, FStretch, FUseLastDock: Boolean;
    FBorderStyle: TBorderStyle;
    FDockMode: TTBDockMode;
    FDragHandleStyle: TTBDragHandleStyle;
    FDockableTo: TTBDockableTo;
    FFloatingMode: TTBFloatingMode;
    FSmoothDrag: Boolean;
    FDockableWindowStyles: TTBDockableWindowStyles;
    FLastRowSize: Integer;
    FInsertRowBefore: Boolean;

    { Misc. }
    FUpdatingBounds,           { Incremented while internally changing the bounds. This allows
                                 it to move the toolbar freely in design mode and prevents the
                                 SizeChanging protected method from begin called }
    FDisableArrange,           { Incremented to disable Arrange }
    FDisableOnMove,            { Incremented to prevent WM_MOVE handler from calling the OnMoved handler }
    FHidden: Integer;          { Incremented while the toolbar is temporarily hidden }
    FArrangeNeeded, FMoved: Boolean;
    FInactiveCaption: Boolean; { True when the caption of the toolbar is currently the inactive color }
    FFloatingPosition: TPoint;
    FDockForms: TList;
    FSavedAtRunTime: Boolean;
    //FNonClientWidth, FNonClientHeight: Integer;
    FDragMode, FDragSplitting, FDragCanSplit: Boolean;
    FSmoothDragging: Boolean;

    { When floating. These are not used in design mode }
    FCloseButtonDown: Boolean; { True if Close button is currently depressed }
    FCloseButtonHover: Boolean;
    FFloatParent: TTBFloatingWindowParent; { Run-time only: The actual Parent of the toolbar when it is floating }

    { Property access methods }
    //function GetVersion: TToolbar97Version;
    function GetNonClientWidth: Integer;
    function GetNonClientHeight: Integer;
    function IsLastDockStored: Boolean;
    function IsWidthAndHeightStored: Boolean;
    procedure SetAutoResize(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCloseButton(Value: Boolean);
    procedure SetCloseButtonWhenDocked(Value: Boolean);
    procedure SetCurrentDock(Value: TTBDock);
    procedure SetDefaultDock(Value: TTBDock);
    procedure SetDockPos(Value: Integer);
    procedure SetDockRow(Value: Integer);
    procedure SetDragHandleStyle(Value: TTBDragHandleStyle);
    procedure SetFloating(Value: Boolean);
    procedure SetFloatingMode(Value: TTBFloatingMode);
    procedure SetFloatingPosition(Value: TPoint);
    procedure SetFullSize(Value: Boolean);
    procedure SetLastDock(Value: TTBDock);
    procedure SetResizable(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure SetUseLastDock(Value: Boolean);
    //procedure SetVersion(const Value: TToolbar97Version);

    { Internal }
    procedure CancelNCHover;
    procedure DrawDraggingOutline(const DC: HDC; const NewRect, OldRect: TRect;
      const NewDocking, OldDocking: Boolean);
    procedure RedrawNCArea;
    procedure SetCloseButtonState(Pushed: Boolean);
    procedure ShowNCContextMenu(const PosX, PosY: Smallint);
    procedure Moved;
    function GetShowingState: Boolean;
    procedure UpdateCaptionState;
    procedure UpdateTopmostFlag;
    procedure UpdateVisibility;
    procedure ReadSavedAtRunTime(Reader: TReader);
    procedure WriteSavedAtRunTime(Writer: TWriter);

    { Messages }
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    {$IFDEF JR_D5}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$ENDIF}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCMouseLeave(var Message: TMessage); message $2A2 {WM_NCMOUSELEAVE};
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDblClk(var Message: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMNCRButtonUp(var Message: TWMNCRButtonUp); message WM_NCRBUTTONUP;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPrintClient(var Message: {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF}); message WM_PRINTCLIENT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    {$IFNDEF JR_D4}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ENDIF}
  protected
    property ActivateParent: Boolean read FActivateParent write FActivateParent default True;
    property AutoResize: Boolean read FAutoResize write SetAutoResize default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clBtnFace;
    property CloseButton: Boolean read FCloseButton write SetCloseButton default True;
    property CloseButtonDown: Boolean read FCloseButtonDown;
    property CloseButtonHover: Boolean read FCloseButtonHover;
    property CloseButtonWhenDocked: Boolean read FCloseButtonWhenDocked write SetCloseButtonWhenDocked default False;
    property DefaultDock: TTBDock read FDefaultDock write SetDefaultDock;
    property DockableTo: TTBDockableTo read FDockableTo write FDockableTo default [dpTop, dpBottom, dpLeft, dpRight];
    property DockableWindowStyles: TTBDockableWindowStyles read FDockableWindowStyles write FDockableWindowStyles;
    property DockMode: TTBDockMode read FDockMode write FDockMode default dmCanFloat;
    property DragHandleStyle: TTBDragHandleStyle read FDragHandleStyle write SetDragHandleStyle default dhSingle;
    property FloatingMode: TTBFloatingMode read FFloatingMode write SetFloatingMode default fmOnTopOfParentForm;
    property FullSize: Boolean read FFullSize write SetFullSize default False;
    property InactiveCaption: Boolean read FInactiveCaption;
    property HideWhenInactive: Boolean read FHideWhenInactive write FHideWhenInactive default True;
    property Resizable: Boolean read FResizable write SetResizable default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property SmoothDrag: Boolean read FSmoothDrag write FSmoothDrag default True;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property UseLastDock: Boolean read FUseLastDock write SetUseLastDock default True;
    //property Version: TToolbar97Version read GetVersion write SetVersion stored False;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnDockChanged: TNotifyEvent read FOnDockChanged write FOnDockChanged;
    property OnDockChanging: TTBDockChangingEvent read FOnDockChanging write FOnDockChanging;
    property OnDockChangingHidden: TTBDockChangingEvent read FOnDockChangingHidden write FOnDockChangingHidden;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
    property OnRecreated: TNotifyEvent read FOnRecreated write FOnRecreated;
    property OnRecreating: TNotifyEvent read FOnRecreating write FOnRecreating;
    {$IFNDEF JR_D4}
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    {$ENDIF}
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;

    { Overridden methods }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    procedure SetParent(AParent: TWinControl); override;

    { Methods accessible to descendants }
    procedure Arrange;
    function CalcNCSizes: TPoint; virtual;
    procedure ChangeSize(AWidth, AHeight: Integer);
    function ChildControlTransparent(Ctl: TControl): Boolean; dynamic;
    procedure Close;
    procedure ControlExistsAtPos(const P: TPoint; var ControlExists: Boolean); virtual;
    function DoArrange(CanMoveControls: Boolean; PreviousDockType: TTBDockType;
      NewFloating: Boolean; NewDock: TTBDock): TPoint; virtual; abstract;
    procedure DoDockChangingHidden(NewFloating: Boolean; DockingTo: TTBDock); dynamic;
    procedure DoubleClick;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
      const Clip: HRGN); virtual;
    procedure GetBaseSize(var ASize: TPoint); virtual; abstract;
    function GetDockedCloseButtonRect(LeftRight: Boolean): TRect; virtual;
    function GetFloatingWindowParentClass: TTBFloatingWindowParentClass; dynamic;
    procedure GetMinShrinkSize(var AMinimumSize: Integer); virtual;
    procedure GetMinMaxSize(var AMinClientWidth, AMinClientHeight,
      AMaxClientWidth, AMaxClientHeight: Integer); virtual;
    function GetShrinkMode: TTBShrinkMode; virtual;
    procedure InitializeOrdering; dynamic;
    function IsAutoResized: Boolean;
    procedure ResizeBegin(SizeHandle: TTBSizeHandle); dynamic;
    procedure ResizeEnd; dynamic;
    procedure ResizeTrack(var Rect: TRect; const OrigRect: TRect); dynamic;
    procedure ResizeTrackAccept; dynamic;
    procedure SizeChanging(const AWidth, AHeight: Integer); virtual;
  public
    property Docked: Boolean read FDocked;
    property Canvas;
    property CurrentDock: TTBDock read FCurrentDock write SetCurrentDock stored False;
    property CurrentSize: Integer read FCurrentSize write FCurrentSize;
    property DockPos: Integer read FDockPos write SetDockPos default -1;
    property DockRow: Integer read FDockRow write SetDockRow default 0;
    property DragMode: Boolean read FDragMode;
    property DragSplitting: Boolean read FDragSplitting;
    property EffectiveDockPos: Integer read FEffectiveDockPos;
    property EffectiveDockRow: Integer read FEffectiveDockRow;
    property Floating: Boolean read FFloating write SetFloating default False;
    property FloatingPosition: TPoint read FFloatingPosition write SetFloatingPosition;
    property LastDock: TTBDock read FLastDock write SetLastDock stored IsLastDockStored;
    property NonClientWidth: Integer read GetNonClientWidth;
    property NonClientHeight: Integer read GetNonClientHeight;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure AddDockForm(const Form: TTBCustomForm);
    procedure AddDockedNCAreaToSize(var S: TPoint; const LeftRight: Boolean);
    procedure AddFloatingNCAreaToSize(var S: TPoint);
    procedure BeginMoving(const InitX, InitY: Integer);
    procedure BeginSizing(const ASizeHandle: TTBSizeHandle);
    procedure BeginUpdate;
    procedure DoneReadingPositionData(const Data: TTBReadPositionData); dynamic;
    procedure EndUpdate;
    procedure GetDockedNCArea(var TopLeft, BottomRight: TPoint;
      const LeftRight: Boolean);
    function GetFloatingBorderSize: TPoint; virtual;
    procedure GetFloatingNCArea(var TopLeft, BottomRight: TPoint);
    function IsMovable: Boolean;
    procedure MoveOnScreen(const OnlyIfFullyOffscreen: Boolean);
    procedure ReadPositionData(const Data: TTBReadPositionData); dynamic;
    procedure RemoveDockForm(const Form: TTBCustomForm);
    procedure WritePositionData(const Data: TTBWritePositionData); dynamic;
  published
    property Height stored IsWidthAndHeightStored;
    property Width stored IsWidthAndHeightStored;
  end;

  TTBBasicBackground = class(TComponent)
  protected
    procedure Draw(DC: HDC; const DrawRect: TRect); virtual; abstract;
    function GetPalette: HPALETTE; virtual; abstract;
    procedure RegisterChanges(Proc: TNotifyEvent); virtual; abstract;
    procedure SysColorChanged; virtual; abstract;
    procedure UnregisterChanges(Proc: TNotifyEvent); virtual; abstract;
    function UsingBackground: Boolean; virtual; abstract;
  end;

  TTBBackground = class(TTBBasicBackground)
  private
    FBitmap, FBitmapCache: TBitmap;
    FBkColor: TColor;
    FNotifyList: TList;
    FTransparent: Boolean;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBitmap(Value: TBitmap);
    procedure SetBkColor(Value: TColor);
    procedure SetTransparent(Value: Boolean);
  protected
    procedure Draw(DC: HDC; const DrawRect: TRect); override;
    function GetPalette: HPALETTE; override;
    procedure RegisterChanges(Proc: TNotifyEvent); override;
    procedure SysColorChanged; override;
    procedure UnregisterChanges(Proc: TNotifyEvent); override;
    function UsingBackground: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BkColor: TColor read FBkColor write SetBkColor default clBtnFace;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

procedure TBRegLoadPositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: String);
procedure TBRegSavePositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: String);
procedure TBIniLoadPositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: String); overload;
procedure TBIniLoadPositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: String); overload;
procedure TBIniSavePositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: String); overload;
procedure TBIniSavePositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: String); overload;

procedure TBCustomLoadPositions(const OwnerComponent: TComponent;
  const ReadIntProc: TTBPositionReadIntProc;
  const ReadStringProc: TTBPositionReadStringProc;
  const ExtraData: TTBPositionExtraData);
procedure TBCustomSavePositions(const OwnerComponent: TComponent;
  const WriteIntProc: TTBPositionWriteIntProc;
  const WriteStringProc: TTBPositionWriteStringProc;
  const ExtraData: TTBPositionExtraData);

function TBGetDockTypeOf(const Control: TTBDock; const Floating: Boolean): TTBDockType;
function TBGetToolWindowParentForm(const ToolWindow: TTBCustomDockableWindow):
  TTBCustomForm;
function TBValidToolWindowParentForm(const ToolWindow: TTBCustomDockableWindow):
  TTBCustomForm;

implementation

uses
  {$IFDEF CLR} Types, System.Runtime.InteropServices, {$ENDIF}
  Registry, Consts, Menus,
  TB2Common, TB2Hook, TB2Consts;

type
  TControlAccess = class(TControl);

const
  DockedBorderSize = 2;
  DockedBorderSize2 = DockedBorderSize*2;
  DragHandleSizes: array[Boolean, TTBDragHandleStyle] of Integer =
    ((9, 0, 6), (14, 14, 14));
  DragHandleXOffsets: array[Boolean, TTBDragHandleStyle] of Integer =
    ((2, 0, 1), (3, 0, 5));
  HT_TB2k_Border = 2000;
  HT_TB2k_Close = 2001;
  HT_TB2k_Caption = 2002;

  DefaultBarWidthHeight = 8;

  ForceDockAtTopRow = 0;
  ForceDockAtLeftPos = -8;

  PositionLeftOrRight = [dpLeft, dpRight];

  twrdAll = [Low(TTBToolWindowNCRedrawWhatElement)..High(TTBToolWindowNCRedrawWhatElement)];

  { Constants for TTBCustomDockableWindow registry values/data.
    Don't localize any of these names! }
  rvRev = 'Rev';
  rdCurrentRev = 2000;
  rvVisible = 'Visible';
  rvDockedTo = 'DockedTo';
  rdDockedToFloating = '+';
  rvLastDock = 'LastDock';
  rvDockRow = 'DockRow';
  rvDockPos = 'DockPos';
  rvFloatLeft = 'FloatLeft';
  rvFloatTop = 'FloatTop';

threadvar
  FloatingToolWindows: TList;


{ Misc. functions }

function GetSmallCaptionHeight: Integer;
{ Returns height of the caption of a small window }
begin
  Result := GetSystemMetrics(SM_CYSMCAPTION);
end;

function GetMDIParent(const Form: TTBCustomForm): TTBCustomForm;
{ Returns the parent of the specified MDI child form. But, if Form isn't a
  MDI child, it simply returns Form. }
var
  I, J: Integer;
begin
  Result := Form;
  if Form = nil then Exit;
  if {$IFDEF JR_D3} (Form is TForm) and {$ENDIF}
     (TForm(Form).FormStyle = fsMDIChild) then
    for I := 0 to Screen.FormCount-1 do
      with Screen.Forms[I] do begin
        if FormStyle <> fsMDIForm then Continue;
        for J := 0 to MDIChildCount-1 do
          if MDIChildren[J] = Form then begin
            Result := Screen.Forms[I];
            Exit;
          end;
      end;
end;

function TBGetDockTypeOf(const Control: TTBDock; const Floating: Boolean): TTBDockType;
begin
  if Floating then
    Result := dtFloating
  else
  if Control = nil then
    Result := dtNotDocked
  else begin
    if not(Control.Position in PositionLeftOrRight) then
      Result := dtTopBottom
    else
      Result := dtLeftRight;
  end;
end;

function TBGetToolWindowParentForm(const ToolWindow: TTBCustomDockableWindow): TTBCustomForm;
var
  Ctl: TWinControl;
begin
  Result := nil;
  Ctl := ToolWindow;
  while Assigned(Ctl.Parent) do begin
    if Ctl.Parent is TTBCustomForm then
      Result := TTBCustomForm(Ctl.Parent);
    Ctl := Ctl.Parent;
  end;
  { ^ for compatibility with ActiveX controls, that code is used instead of
    GetParentForm because it returns nil unless the form is the *topmost*
    parent }
  if Result is TTBFloatingWindowParent then
    Result := TTBFloatingWindowParent(Result).ParentForm;
end;

function TBValidToolWindowParentForm(const ToolWindow: TTBCustomDockableWindow): TTBCustomForm;
begin
  Result := TBGetToolWindowParentForm(ToolWindow);
  if Result = nil then
    raise EInvalidOperation.{$IFDEF JR_D3}CreateFmt{$ELSE}CreateResFmt{$ENDIF}
      (SParentRequired, [ToolWindow.Name]);
end;

procedure SetWindowOwner(const Wnd, NewOwnerWnd: HWND);
begin
  SetWindowLong(Wnd, GWL_HWNDPARENT,
    {$IFDEF JR_D11} LONG_PTR {$ELSE} Longint {$ENDIF} (NewOwnerWnd));
end;

procedure ToolbarHookProc(Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);
var
  I: Integer;
  ToolWindow: TTBCustomDockableWindow;
  WindowPos: {$IFNDEF CLR} PWindowPos {$ELSE} TWindowPos {$ENDIF};
  Form: TTBCustomForm;
begin
  case Code of
    hpSendActivate,
    hpSendActivateApp: begin
        if Assigned(FloatingToolWindows) then
          for I := 0 to FloatingToolWindows.Count-1 do
            { Hide or restore toolbars when a form or the application is
              deactivated or activated, and/or update their caption state
              (active/inactive) }
            TTBCustomDockableWindow(FloatingToolWindows[I]).UpdateVisibility;
      end;
    hpSendWindowPosChanged: begin
        if Assigned(FloatingToolWindows) then begin
          {$IFNDEF CLR}
          WindowPos := PWindowPos(LParam);
          {$ELSE}
          WindowPos := TWindowPos(Marshal.PtrToStructure(IntPtr(LParam), TypeOf(TWindowPos)));
          {$ENDIF}
          for I := 0 to FloatingToolWindows.Count-1 do begin
            ToolWindow := TTBCustomDockableWindow(FloatingToolWindows[I]);
            if (ToolWindow.FFloatingMode = fmOnTopOfParentForm) and ToolWindow.HandleAllocated then begin
              { Call UpdateVisibility if parent form's visibility has
                changed, or if it has been minimized or restored }
              if ((WindowPos.flags and (SWP_SHOWWINDOW or SWP_HIDEWINDOW) <> 0) or
                  (WindowPos.flags and SWP_FRAMECHANGED <> 0)) then begin
                Form := TBGetToolWindowParentForm(ToolWindow);
                if Assigned(Form) and Form.HandleAllocated and ((Wnd = Form.Handle) or IsChild(Wnd, Form.Handle)) then
                  ToolWindow.UpdateVisibility;
              end;
            end;
          end;
        end;
      end;
    hpPreDestroy: begin
        if Assigned(FloatingToolWindows) then
          for I := 0 to FloatingToolWindows.Count-1 do begin
            with TTBCustomDockableWindow(FloatingToolWindows[I]) do
              { It must remove the form window's ownership of the tool window
                *before* the form gets destroyed, otherwise Windows will destroy
                the tool window's handle. }
              if Assigned(Parent) and Parent.HandleAllocated and
                 (HWND(GetWindowLong(Parent.Handle, GWL_HWNDPARENT)) = Wnd) then
                SetWindowOwner(Parent.Handle, Application.Handle);
                { ^ Restore GWL_HWNDPARENT back to Application.Handle }
          end;
      end;
  end;
end;

type
  {$IFNDEF CLR}
  PFindWindowData = ^TFindWindowData;
  TFindWindowData = record
  {$ELSE}
  TFindWindowData = class
  private
  {$ENDIF}
    TaskActiveWindow, TaskFirstWindow, TaskFirstTopMost: HWND;
    {$IFDEF CLR}
    function DoFindWindow(Wnd: HWND; Param: LPARAM): BOOL;
    {$ENDIF}
  end;

{$IFNDEF CLR}
function DoFindWindow(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
{$ELSE}
function TFindWindowData.DoFindWindow(Wnd: HWND; Param: LPARAM): BOOL;
{$ENDIF}
begin
  {$IFNDEF CLR}
  with PFindWindowData(Param)^ do
  {$ENDIF}
    if (Wnd <> TaskActiveWindow) and (Wnd <> Application.Handle) and
       IsWindowVisible(Wnd) and IsWindowEnabled(Wnd) then begin
      if GetWindowLong(Wnd, GWL_EXSTYLE) and WS_EX_TOPMOST = 0 then begin
        if TaskFirstWindow = 0 then TaskFirstWindow := Wnd;
      end
      else begin
        if TaskFirstTopMost = 0 then TaskFirstTopMost := Wnd;
      end;
    end;
  Result := True;
end;

function FindTopLevelWindow(ActiveWindow: HWND): HWND;
var
  FindData: TFindWindowData;
begin
  {$IFDEF CLR}
  FindData := TFindWindowData.Create;
  {$ENDIF}
  with FindData do begin
    TaskActiveWindow := ActiveWindow;
    TaskFirstWindow := 0;
    TaskFirstTopMost := 0;
    {$IFNDEF CLR}
    EnumThreadWindows(GetCurrentThreadID, @DoFindWindow, LPARAM(@FindData));
    {$ELSE}
    EnumThreadWindows(GetCurrentThreadID, DoFindWindow, 0);
    {$ENDIF}
    if TaskFirstWindow <> 0 then
      Result := TaskFirstWindow
    else
      Result := TaskFirstTopMost;
  end;
end;

function IsAncestorOfWindow(const ParentWnd: HWND; Wnd: HWND): Boolean;
{ Returns True if Wnd is a child of, is owned by, or is the same window as
  ParentWnd }
begin
  while Wnd <> 0 do begin
    if Wnd = ParentWnd then begin
      Result := True;
      Exit;
    end;
    Wnd := GetParent(Wnd);
  end;
  Result := False;
end;

procedure RecalcNCArea(const Ctl: TWinControl);
begin
  if Ctl.HandleAllocated then
    SetWindowPos(Ctl.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;

procedure InvalidateAll(const Ctl: TWinControl);
{ Invalidate both non-client and client area, and erase. }
begin
  if Ctl.HandleAllocated then
    RedrawWindow(Ctl.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
      RDW_ERASE or RDW_NOCHILDREN);
end;

type
  TSetCloseButtonStateProc = procedure(Pushed: Boolean) of object;

function CloseButtonLoop(const Wnd: HWND; const ButtonRect: TRect;
  const SetCloseButtonStateProc: TSetCloseButtonStateProc): Boolean;
  function MouseInButton: Boolean;
  var
    P: TPoint;
  begin
    GetCursorPos(P);
    Result := PtInRect(ButtonRect, P);
  end;
var
  Msg: TMsg;
begin
  Result := False;

  SetCloseButtonStateProc(MouseInButton);

  SetCapture(Wnd);

  try
    while GetCapture = Wnd do begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break; { if GetMessage failed }
        0: begin
             { Repost WM_QUIT messages }
             PostQuitMessage(ClipToLongint(Msg.wParam));
             Break;
           end;
      end;

      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP:
          { Ignore all keystrokes while in a close button loop }
          ;
        WM_MOUSEMOVE: begin
            { Note to self: WM_MOUSEMOVE messages should never be dispatched
              here to ensure no hints get shown }
            SetCloseButtonStateProc(MouseInButton);
          end;
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
          { Make sure it doesn't begin another loop }
          Break;
        WM_LBUTTONUP: begin
            if MouseInButton then
              Result := True;
            Break;
          end;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:
          { Ignore all other mouse up/down messages }
          ;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = Wnd then
      ReleaseCapture;
    SetCloseButtonStateProc(False);
  end;
end;


{ TTBDock - internal }

constructor TTBDock.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csAcceptsControls, csMenuEvents] -
    [csClickEvents, csCaptureMouse, csOpaque];
  FAllowDrag := True;
  FBkgOnToolbars := True;
  DockList := TList.Create;
  DockVisibleList := TList.Create;
  Color := clBtnFace;
  Position := dpTop;
end;

procedure TTBDock.CreateParams(var Params: TCreateParams);
begin
  inherited;
  { Disable complete redraws when size changes. CS_H/VREDRAW cause flicker
    and are not necessary for this control at run time }
  if not(csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not(CS_HREDRAW or CS_VREDRAW);
end;

destructor TTBDock.Destroy;
begin
  if Assigned(FBackground) then
    FBackground.UnregisterChanges(BackgroundChanged);
  inherited;
  DockVisibleList.Free;
  DockList.Free;
end;

procedure TTBDock.SetParent(AParent: TWinControl);
begin
  if (AParent is TTBCustomDockableWindow) or (AParent is TTBDock) then
    raise EInvalidOperation.Create(STBDockParentNotAllowed);

  inherited;
end;

procedure TTBDock.BeginUpdate;
begin
  Inc(FDisableArrangeToolbars);
end;

procedure TTBDock.EndUpdate;
begin
  Dec(FDisableArrangeToolbars);
  if FArrangeToolbarsNeeded and (FDisableArrangeToolbars = 0) then
    ArrangeToolbars;
end;

function TTBDock.HasVisibleToolbars: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to DockList.Count-1 do
    if ToolbarVisibleOnDock(TTBCustomDockableWindow(DockList[I])) then begin
      Result := True;
      Break;
    end;
end;

function TTBDock.ToolbarVisibleOnDock(const AToolbar: TTBCustomDockableWindow): Boolean;
begin
  Result := (AToolbar.Parent = Self) and
    (AToolbar.Visible or (csDesigning in AToolbar.ComponentState));
end;

function TTBDock.GetCurrentRowSize(const Row: Integer;
  var AFullSize: Boolean): Integer;
var
  I, J: Integer;
  T: TTBCustomDockableWindow;
begin
  Result := 0;
  AFullSize := False;
  if Row < 0 then Exit;
  for I := 0 to DockList.Count-1 do begin
    T := TTBCustomDockableWindow(DockList[I]);
    if (T.FEffectiveDockRow = Row) and ToolbarVisibleOnDock(T) then begin
      AFullSize := T.FullSize;
      if not(Position in PositionLeftOrRight) then
        J := T.Height
      else
        J := T.Width;
      if J > Result then
        Result := J;
    end;
  end;
end;

function TTBDock.GetMinRowSize(const Row: Integer;
  const ExcludeControl: TTBCustomDockableWindow): Integer;
var
  I, J: Integer;
  T: TTBCustomDockableWindow;
begin
  Result := 0;
  if Row < 0 then Exit;
  for I := 0 to DockList.Count-1 do begin
    T := TTBCustomDockableWindow(DockList[I]);
    if (T <> ExcludeControl) and (T.FEffectiveDockRow = Row) and
       ToolbarVisibleOnDock(T) then begin
      J := T.FLastRowSize;
      if J > Result then
        Result := J;
    end;
  end;
end;

function TTBDock.GetDesignModeRowOf(const XY: Integer): Integer;
{ Similar to GetRowOf, but is a little different to accomidate design mode
  better }
var
  HighestRowPlus1, R, CurY, CurRowSize: Integer;
  FullSize: Boolean;
begin
  Result := 0;
  HighestRowPlus1 := GetHighestRow(True)+1;
  CurY := 0;
  for R := 0 to HighestRowPlus1 do begin
    Result := R;
    if R = HighestRowPlus1 then Break;
    CurRowSize := GetCurrentRowSize(R, FullSize);
    if CurRowSize = 0 then Continue;
    Inc(CurY, CurRowSize);
    if XY < CurY then
      Break;
  end;
end;

function TTBDock.GetHighestRow(const HighestEffective: Boolean): Integer;
{ Returns highest used row number, or -1 if no rows are used }
var
  I, J: Integer;
begin
  Result := -1;
  for I := 0 to DockList.Count-1 do
    with TTBCustomDockableWindow(DockList[I]) do begin
      if HighestEffective then
        J := FEffectiveDockRow
      else
        J := FDockRow;
      if J > Result then
        Result := J;
    end;
end;

procedure TTBDock.ChangeWidthHeight(const NewWidth, NewHeight: Integer);
{ Same as setting Width/Height directly, but does not lose Align position.
  Specifically, it ensures that a bottom-aligned dock stays above a
  bottom-aligned TStatusBar when the only toolbar on the dock is undocked
  and then redocked. }
begin
  case Align of
    alNone, alTop, alLeft:
      SetBounds(Left, Top, NewWidth, NewHeight);
    alBottom:
      SetBounds(Left, Top-NewHeight+Height, NewWidth, NewHeight);
    alRight:
      SetBounds(Left-NewWidth+Width, Top, NewWidth, NewHeight);
  end;
end;

procedure TTBDock.AlignControls(AControl: TControl; var Rect: TRect);
begin
  ArrangeToolbars;
end;

function CompareDockRowPos(Item1, Item2: TListItemType): Integer;
begin
  Result := TTBCustomDockableWindow(Item1).FDockRow - TTBCustomDockableWindow(Item2).FDockRow;
  if Result = 0 then
    Result := TTBCustomDockableWindow(Item1).FDockPos - TTBCustomDockableWindow(Item2).FDockPos;
end;

procedure TTBDock.ArrangeToolbars;
{ The main procedure to arrange all the toolbars docked to it }
type
  TPosDataRec = record
    Row, ActualRow, PrecSpace, FullSize, MinimumSize, Size, Overlap, Pos: Integer;
    ShrinkMode: TTBShrinkMode;
    NeedArrange: Boolean;
  end;
var
  NewDockList: TList;
  PosData: array of TPosDataRec;

  function IndexOfDraggingToolbar(const List: TList): Integer;
  { Returns index of toolbar in List that's currently being dragged, or -1 }
  var
    I: Integer;
  begin
    for I := 0 to List.Count-1 do
      if TTBCustomDockableWindow(List[I]).FDragMode then begin
        Result := I;
        Exit;
      end;
    Result := -1;
  end;

  function ShiftLeft(const Row, StartIndex, MaxSize: Integer): Integer;
  { Removes PrecSpace pixels from toolbars at or before StartIndex until the
    right edge of the toolbar at StartIndex is <= MaxSize.
    Returns the total number of PrecSpace pixels removed from toolbars. }
  var
    PixelsOffEdge, I, J: Integer;
  begin
    Result := 0;
    PixelsOffEdge := -MaxSize;
    for I := 0 to StartIndex do begin
      if PosData[I].Row = Row then begin
        Inc(PixelsOffEdge, PosData[I].PrecSpace);
        Inc(PixelsOffEdge, PosData[I].Size);
      end;
    end;
    if PixelsOffEdge > 0 then
      for I := StartIndex downto 0 do begin
        if PosData[I].Row = Row then begin
          J := PixelsOffEdge;
          if PosData[I].PrecSpace < J then
            J := PosData[I].PrecSpace;
          Dec(PosData[I].PrecSpace, J);
          Dec(PixelsOffEdge, J);
          Inc(Result, J);
          if PixelsOffEdge = 0 then
            Break;
        end;
      end;
  end;

  function GetNextToolbar(const GoForward: Boolean; const Row: Integer;
    const StartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    I := StartIndex;
    while True do begin
      if GoForward then begin
        Inc(I);
        if I >= NewDockList.Count then
          Break;
      end
      else begin
        Dec(I);
        if I < 0 then
          Break;
      end;
      if PosData[I].Row = Row then begin
        Result := I;
        Break;
      end;
    end;
  end;

var
  LeftRight: Boolean;
  EmptySize, HighestRow, R, CurPos, CurRowPixel, I, J, K, L, ClientW,
    ClientH, MaxSize, TotalSize, PixelsPastMaxSize, Offset, CurRealPos, DragIndex,
    MinRealPos, DragIndexPos, ToolbarsOnRow, CurRowSize: Integer;
  T: TTBCustomDockableWindow;
  S: TPoint;
  RowIsEmpty: Boolean;
label FoundNextToolbar;
begin
  if (FDisableArrangeToolbars > 0) or (csLoading in ComponentState) then begin
    FArrangeToolbarsNeeded := True;
    Exit;
  end;

  NewDockList := nil;
  Inc(FDisableArrangeToolbars);
  try
    { Work around VCL alignment bug when docking toolbars taller or wider than
      the client height or width of the form. }
    {if not(csDesigning in ComponentState) and HandleAllocated then
      SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0,
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);}

    LeftRight := Position in PositionLeftOrRight;

    if not HasVisibleToolbars then begin
      EmptySize := Ord(FFixAlign);
      if csDesigning in ComponentState then
        EmptySize := 9;
      if not LeftRight then
        ChangeWidthHeight(Width, EmptySize)
      else
        ChangeWidthHeight(EmptySize, Height);
      Exit;
    end;

    { It can't read the ClientWidth and ClientHeight properties because they
      attempt to create a handle, which requires Parent to be set. "ClientW"
      and "ClientH" are calculated instead. }
    ClientW := Width - FNonClientWidth;
    if ClientW < 0 then ClientW := 0;
    ClientH := Height - FNonClientHeight;
    if ClientH < 0 then ClientH := 0;

    { Remove toolbars from DockList & DockVisibleList that are destroying, so
      that no methods on these toolbars will be called.
      This is needed because in certain rare cases ArrangeToolbars can be
      indirectly called while a docked toolbar is being destroyed. }
    for I := DockList.Count-1 downto 0 do begin
      T := TTBCustomDockableWindow(DockList[I]);
      if csDestroying in T.ComponentState then begin
        DockList.Delete(I);
        DockVisibleList.Remove(T);
      end;
    end;

    { If LimitToOneRow is True, only use the first row }
    if FLimitToOneRow then
      for I := 0 to DockList.Count-1 do
        with TTBCustomDockableWindow(DockList[I]) do
          FDockRow := 0;

    { Copy DockList to NewDockList, and ensure it is in correct ordering
      according to DockRow/DockPos }
    NewDockList := TList.Create;
    NewDockList.Count := DockList.Count;
    for I := 0 to NewDockList.Count-1 do
      NewDockList[I] := DockList[I];
    I := IndexOfDraggingToolbar(NewDockList);
    NewDockList.Sort(CompareDockRowPos);
    DragIndex := IndexOfDraggingToolbar(NewDockList);
    if (I <> -1) and TTBCustomDockableWindow(NewDockList[DragIndex]).FDragSplitting then begin
      { When splitting, don't allow the toolbar being dragged to change
        positions in the dock list }
      NewDockList.Move(DragIndex, I);
      DragIndex := I;
    end;
    DockVisibleList.Sort(CompareDockRowPos);
    { Find highest row number }
    HighestRow := GetHighestRow(False);

    { Create a temporary array that holds new position data for the toolbars }
    SetLength(PosData, NewDockList.Count);
    for I := 0 to NewDockList.Count-1 do begin
      T := TTBCustomDockableWindow(NewDockList[I]);
      PosData[I].ActualRow := T.FDockRow;
      if ToolbarVisibleOnDock(T) then
        PosData[I].Row := T.FDockRow
      else
        PosData[I].Row := -1;
      PosData[I].Pos := T.FDockPos;
    end;

    { Find FInsertRowBefore=True and FullSize=True toolbars and make sure there
      aren't any other toolbars on the same row. If there are, shift them down
      a row. }
    for L := 0 to 1 do begin
      R := 0;
      while R <= HighestRow do begin
        for I := 0 to NewDockList.Count-1 do begin
          T := TTBCustomDockableWindow(NewDockList[I]);
          if (PosData[I].ActualRow = R) and
             (((L = 0) and T.FInsertRowBefore and not LimitToOneRow) or
              ((L = 1) and T.FullSize)) then
            for J := 0 to NewDockList.Count-1 do
              if (J <> I) and (PosData[J].ActualRow = R) then begin
                for K := 0 to NewDockList.Count-1 do begin
                  if K <> I then begin
                    if PosData[K].ActualRow >= R then
                      Inc(PosData[K].ActualRow);
                    if PosData[K].Row >= R then
                      Inc(PosData[K].Row);
                  end;
                end;
                Inc(HighestRow);
                Break;
              end;
        end;
        Inc(R);
      end;
    end;

    { Remove blank rows.
      Note that rows that contain only invisible or currently floating toolbars
      are intentionally not removed, so that when the toolbars are shown again,
      they stay on their own row. }
    R := 0;
    while R <= HighestRow do begin
      RowIsEmpty := True;
      for I := 0 to NewDockList.Count-1 do
        if PosData[I].ActualRow = R then begin
          RowIsEmpty := False;
          Break;
        end;
      if RowIsEmpty then begin
        { Shift all ones higher than R back one }
        for I := 0 to NewDockList.Count-1 do begin
          if PosData[I].ActualRow > R then
            Dec(PosData[I].ActualRow);
          if PosData[I].Row > R then
            Dec(PosData[I].Row);
        end;
        Dec(HighestRow);
      end
      else
        Inc(R);
    end;

    { Calculate positions and sizes of each row }
    R := 0;
    while R <= HighestRow do begin
      if not LeftRight then
        MaxSize := ClientW
      else
        MaxSize := ClientH;

      { Set initial sizes }
      TotalSize := 0;
      ToolbarsOnRow := 0;
      MinRealPos := 0;
      for I := 0 to NewDockList.Count-1 do begin
        if PosData[I].Row = R then begin
          T := TTBCustomDockableWindow(NewDockList[I]);
          T.GetBaseSize(S);
          if not LeftRight then
            J := S.X + T.NonClientWidth
          else
            J := S.Y + T.NonClientHeight;
          PosData[I].FullSize := J;
          PosData[I].Size := J;
          PosData[I].ShrinkMode := T.GetShrinkMode;
          PosData[I].MinimumSize := 0;
          T.GetMinShrinkSize(PosData[I].MinimumSize);
          if PosData[I].MinimumSize > PosData[I].FullSize then
            { don't allow minimum shrink size to be less than full size } 
            PosData[I].MinimumSize := PosData[I].FullSize;
          if PosData[I].ShrinkMode = tbsmChevron then
            Inc(MinRealPos, PosData[I].MinimumSize)
          else
            Inc(MinRealPos, PosData[I].FullSize);
          { If the toolbar isn't the first toolbar on the row, and the toolbar
            would go off the edge even after it's shrunk, then move it onto a
            row of its own }
          if (ToolbarsOnRow > 0) and (MinRealPos > MaxSize) and
             not LimitToOneRow then begin
            for K := I to NewDockList.Count-1 do begin
              if PosData[K].ActualRow >= R then
                Inc(PosData[K].ActualRow);
              if PosData[K].Row >= R then
                Inc(PosData[K].Row);
            end;
            Inc(HighestRow);
            Break;
          end;
          Inc(TotalSize, J);
          Inc(ToolbarsOnRow);
        end;
      end;
      PixelsPastMaxSize := TotalSize - MaxSize;

      { Set initial arrangement; don't shrink toolbars yet }
      DragIndexPos := 0;
      CurPos := 0;
      CurRealPos := 0;
      MinRealPos := 0;
      for I := 0 to NewDockList.Count-1 do begin
        T := TTBCustomDockableWindow(NewDockList[I]);
        if PosData[I].Row = R then begin
          if (CurPos = 0) and (T.FullSize or T.Stretch) then
            { Force to left }
            J := 0
          else
            J := T.FDockPos;
          if I = DragIndex then
            DragIndexPos := J;
          { Don't let this toolbar overlap preceding toolbars by more than
            the sum of their minimum sizes }
          if J < MinRealPos then
            J := MinRealPos;
          if J > CurPos then begin
            { There's a gap between the left edge or previous toolbar and
              this toolbar }
            if PixelsPastMaxSize <= 0 then begin
              PosData[I].PrecSpace := J - CurPos;
              CurPos := J;
            end
            else
              { Don't allow a gap if exceeding MaxSize }
              J := CurPos;
          end
          else begin
            if J < CurRealPos then
              PosData[I].Overlap := CurRealPos - J;
          end;

          Inc(CurPos, PosData[I].Size);
          CurRealPos := J + PosData[I].Size;
          Inc(MinRealPos, PosData[I].MinimumSize);
        end;
      end;

      { If we aren't exceeding MaxSize, allow the toolbar being dragged
        to push other toolbars to the left }
      if (PixelsPastMaxSize < 0) and (DragIndex <> -1) and
         (PosData[DragIndex].Row = R) then begin
        I := GetNextToolbar(False, R, DragIndex);
        if I <> -1 then begin
          J := ShiftLeft(R, I, DragIndexPos);
          if J > 0 then begin
            { Ensure that toolbars that follow the toolbar being dragged stay
              at the same place by increasing PrecSpace on the next toolbar }
            I := GetNextToolbar(True, R, DragIndex);
            if I <> -1 then
              Inc(PosData[I].PrecSpace, J);
          end;
        end;
      end;

      { If any toolbars are going off the edge of the dock, try to make them
        at least partially visible by shifting preceding toolbars left }
      I := GetNextToolbar(False, R, NewDockList.Count);
      if I <> -1 then
        ShiftLeft(R, I, MaxSize);

      { Shrink toolbars that overlap other toolbars (Overlaps[x] > 0) }
      if PixelsPastMaxSize > 0 then begin
        Offset := 0;
        for I := 0 to NewDockList.Count-1 do begin
          if PosData[I].Row <> R then
            Continue;
          T := TTBCustomDockableWindow(NewDockList[I]);
          if (ToolbarsOnRow > 1) and T.FDragMode then
            T.FDragCanSplit := True;
          Inc(Offset, PosData[I].Overlap);
          if Offset > PixelsPastMaxSize then
            Offset := PixelsPastMaxSize;
          if Offset > 0 then
            for J := I-1 downto 0 do begin
              if PosData[J].Row <> R then
                Continue;
              { How much can we shrink this toolbar J to get toolbar I to
                its preferred position? }
              if PosData[J].ShrinkMode = tbsmChevron then
                L := Offset
              else
                L := 0;
              K := -(PosData[J].Size - L - PosData[J].MinimumSize);  { the number of pixels that exceed the minimum size }
              if K > 0 then
                { Don't shrink a toolbar below its minimum allowed size }
                Dec(L, K);
              Dec(PosData[J].Size, L);
              Dec(PixelsPastMaxSize, L);
              Dec(Offset, L);
              if (Offset = 0) or
                 { This is needed so toolbars can push other toolbars to the
                   right when splitting: }
                 (J = DragIndex) then
                Break;
            end;
        end;
      end;

      { Still exceeding MaxSize? Make sure the rightmost toolbar(s) are
        at least partially visible with a width of MinimumSize }
      if PixelsPastMaxSize > 0 then begin
        for I := NewDockList.Count-1 downto 0 do begin
          if (PosData[I].Row <> R) or (PosData[I].ShrinkMode = tbsmNone) or
             ((PosData[I].ShrinkMode = tbsmWrap) and (ToolbarsOnRow > 1)) then
            Continue;
          J := PosData[I].Size - PosData[I].MinimumSize;
          if J > 0 then begin  { can we shrink this toolbar any? }
            if J > PixelsPastMaxSize then
              J := PixelsPastMaxSize;
            Dec(PosData[I].Size, J);
            Dec(PixelsPastMaxSize, J);
          end;
          if PixelsPastMaxSize = 0 then
            Break;
        end;
      end;

      { Set Poses, and adjust size of FullSize & Stretch toolbars }
      CurPos := 0;
      for I := 0 to NewDockList.Count-1 do begin
        T := TTBCustomDockableWindow(NewDockList[I]);
        if PosData[I].Row = R then begin
          if T.FullSize or T.Stretch then begin
            { Remove any preceding space from this toolbar }
            Inc(PosData[I].Size, PosData[I].PrecSpace);
            PosData[I].PrecSpace := 0;
          end;
          Inc(CurPos, PosData[I].PrecSpace);
          if T.FullSize then begin
            { Claim all space }
            if PosData[I].Size < MaxSize then
              PosData[I].Size := MaxSize;
          end
          else if T.Stretch then begin
            { Steal any preceding space from the next toolbar }
            for J := I+1 to NewDockList.Count-1 do
              if PosData[J].Row = R then begin
                Inc(PosData[I].Size, PosData[J].PrecSpace);
                PosData[J].PrecSpace := 0;
                goto FoundNextToolbar;
              end;
            { or claim any remaining space }
            if PosData[I].Size < MaxSize - CurPos then
              PosData[I].Size := MaxSize - CurPos;
            FoundNextToolbar:
          end;
          PosData[I].Pos := CurPos;
          Inc(CurPos, PosData[I].Size);
        end;
      end;

      Inc(R);
    end;

    for I := 0 to NewDockList.Count-1 do begin
      T := TTBCustomDockableWindow(NewDockList[I]);
      T.FEffectiveDockRow := PosData[I].ActualRow;
      T.FEffectiveDockPos := PosData[I].Pos;
      { If FCommitNewPositions is True, update all the toolbars' DockPos and
        DockRow properties to match the actual positions.
        Also update the ordering of DockList to match NewDockList }
      if FCommitNewPositions then begin
        T.FDockRow := T.FEffectiveDockRow;
        T.FDockPos := T.FEffectiveDockPos;
        DockList[I] := NewDockList[I];
      end;
    end;

    { Now actually move the toolbars }
    CurRowPixel := 0;
    for R := 0 to HighestRow do begin
      CurRowSize := -1;
      for I := 0 to NewDockList.Count-1 do begin
        T := TTBCustomDockableWindow(NewDockList[I]);
        if PosData[I].Row = R then begin
          K := T.FCurrentSize;
          T.FCurrentSize := PosData[I].Size;
          if PosData[I].Size >= PosData[I].FullSize then begin
            T.FCurrentSize := 0;
            { Reason: so that if new items are added to a non-shrunk toolbar
              at run-time (causing its width to increase), the toolbar won't
              shrink unnecessarily }
          end;
          if (PosData[I].ShrinkMode <> tbsmNone) and (T.FCurrentSize <> K) then begin
            { If Size is changing and we are to display a chevron or wrap,
              call DoArrange to get an accurate row size }
            S := T.DoArrange(False, TBGetDockTypeOf(Self, False), False, Self);
            { Force a rearrange in case the actual size isn't changing but the
              chevron visibility might have changed (which can happen if
              items are added to a FullSize=True toolbar at run-time) }
            PosData[I].NeedArrange := True;
          end
          else begin
            if (PosData[I].ShrinkMode = tbsmWrap) and (PosData[I].Size < PosData[I].FullSize) then begin
              { Preserve existing height (or width) on a wrapped toolbar
                whose size isn't changing now }
              S.X := T.Width - T.NonClientWidth;
              S.Y := T.Height - T.NonClientHeight;
            end
            else
              T.GetBaseSize(S);
          end;
          if not LeftRight then
            K := S.Y
          else
            K := S.X;
          T.FLastRowSize := K;
          if K > CurRowSize then
            CurRowSize := K;
        end;
      end;
      if CurRowSize <> -1 then
        Inc(CurRowSize, DockedBorderSize2)
      else
        CurRowSize := 0;
      for I := 0 to NewDockList.Count-1 do begin
        T := TTBCustomDockableWindow(NewDockList[I]);
        if PosData[I].Row = R then begin
          Inc(T.FUpdatingBounds);
          try
            K := T.FCurrentSize;
            if PosData[I].NeedArrange then
              T.FArrangeNeeded := True;
            if not LeftRight then
              T.SetBounds(PosData[I].Pos, CurRowPixel, PosData[I].Size, CurRowSize)
            else
              T.SetBounds(CurRowPixel, PosData[I].Pos, CurRowSize, PosData[I].Size);
            if T.FArrangeNeeded then
              { ^ don't arrange again if SetBounds call already caused one }
              T.Arrange;
            { Restore FCurrentSize since TTBToolbarView.DoUpdatePositions
              clears it }
            T.FCurrentSize := K;
          finally
            Dec(T.FUpdatingBounds);
          end;
        end;
      end;
      Inc(CurRowPixel, CurRowSize);
    end;

    { Set the size of the dock }
    if not LeftRight then
      ChangeWidthHeight(Width, CurRowPixel + FNonClientHeight)
    else
      ChangeWidthHeight(CurRowPixel + FNonClientWidth, Height);
  finally
    Dec(FDisableArrangeToolbars);
    FArrangeToolbarsNeeded := False;
    FCommitNewPositions := False;
    NewDockList.Free;
  end;
end;

procedure TTBDock.CommitPositions;
{ Copies docked toolbars' EffectiveDockRow and EffectiveDockPos properties
  into DockRow and DockPos respectively.
  Note that this does not reorder DockList like ArrangeToolbars does when
  FCommitNewPositions=True. }
var
  I: Integer;
  T: TTBCustomDockableWindow;
begin
  for I := 0 to DockVisibleList.Count-1 do begin
    T := TTBCustomDockableWindow(DockVisibleList[I]);
    T.FDockRow := T.FEffectiveDockRow;
    T.FDockPos := T.FEffectiveDockPos;
  end;
end;

procedure TTBDock.ChangeDockList(const Insert: Boolean;
  const Bar: TTBCustomDockableWindow);
{ Inserts or removes Bar from DockList }
var
  I: Integer;
begin
  I := DockList.IndexOf(Bar);
  if Insert then begin
    if I = -1 then begin
      Bar.FreeNotification(Self);
      DockList.Add(Bar);
    end;
  end
  else begin
    if I <> -1 then
      DockList.Delete(I);
  end;
  ToolbarVisibilityChanged(Bar, False);
end;

procedure TTBDock.ToolbarVisibilityChanged(const Bar: TTBCustomDockableWindow;
  const ForceRemove: Boolean);
var
  Modified, VisibleOnDock: Boolean;
  I: Integer;
begin
  Modified := False;
  I := DockVisibleList.IndexOf(Bar);
  VisibleOnDock := not ForceRemove and ToolbarVisibleOnDock(Bar);
  if VisibleOnDock then begin
    if I = -1 then begin
      DockVisibleList.Add(Bar);
      Modified := True;
    end;
  end
  else begin
    if I <> -1 then begin
      DockVisibleList.Remove(Bar);
      Modified := True;
    end;
  end;

  if Modified then begin
    ArrangeToolbars;

    if Assigned(FOnInsertRemoveBar) then
      FOnInsertRemoveBar(Self, VisibleOnDock, Bar);
  end;
end;

procedure TTBDock.Loaded;
begin
  inherited;
  { Rearranging is disabled while the component is loading, so now that it's
    loaded, rearrange it. }
  ArrangeToolbars;
end;

procedure TTBDock.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FBackground then
      Background := nil
    else if AComponent is TTBCustomDockableWindow then begin
      DockList.Remove(AComponent);
      DockVisibleList.Remove(AComponent);
    end;
  end;
end;

function TTBDock.GetPalette: HPALETTE;
begin
  if UsingBackground and Assigned(FBackground) then
    { ^ by default UsingBackground returns False if FBackground isn't assigned,
      but UsingBackground may be overridden and return True when it isn't }
    Result := FBackground.GetPalette
  else
    Result := 0;
end;

procedure TTBDock.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R, R2: TRect;
  P1, P2: TPoint;
  SaveIndex: Integer;
begin
  { Draw the Background if there is one, otherwise use default erasing
    behavior }
  if UsingBackground then begin
    R := ClientRect;
    R2 := R;
    { Make up for nonclient area }
    P1 := ClientToScreen(Point(0, 0));
    P2 := Parent.ClientToScreen(BoundsRect.TopLeft);
    Dec(R2.Left, Left + (P1.X-P2.X));
    Dec(R2.Top, Top + (P1.Y-P2.Y));
    SaveIndex := SaveDC(Message.DC);
    IntersectClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);
    DrawBackground(Message.DC, R2);
    RestoreDC(Message.DC, SaveIndex);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TTBDock.Paint;
var
  R: TRect;
begin
  inherited;
  { Draw dotted border in design mode }
  if csDesigning in ComponentState then begin
    R := ClientRect;
    with Canvas do begin
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Brush.Style := bsClear;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      Pen.Style := psSolid;
    end;
  end;
end;

procedure TTBDock.WMMove(var Message: TWMMove);
begin
  inherited;
  if UsingBackground then
    InvalidateBackgrounds;
end;

{$IFNDEF JR_D4}
procedure TTBDock.WMSize(var Message: TWMSize);
begin
  inherited;
  if not(csLoading in ComponentState) and Assigned(FOnResize) then
    FOnResize(Self);
end;
{$ENDIF}

procedure TTBDock.WMNCCalcSize(var Message: TWMNCCalcSize);

  procedure ApplyToRect(var R: TRect);
  begin
    if blTop in BoundLines then Inc(R.Top);
    if blBottom in BoundLines then Dec(R.Bottom);
    if blLeft in BoundLines then Inc(R.Left);
    if blRight in BoundLines then Dec(R.Right);
  end;

{$IFDEF CLR}
var
  Params: TNCCalcSizeParams;
{$ENDIF}
begin
  inherited;
  { note to self: non-client size is stored in FNonClientWidth &
    FNonClientHeight }
  {$IFNDEF CLR}
  ApplyToRect(Message.CalcSize_Params.rgrc[0]);
  {$ELSE}
  Params := Message.CalcSize_Params;
  ApplyToRect(Params.rgrc0);
  Message.CalcSize_Params := Params;
  {$ENDIF}
end;

procedure TTBDock.DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
  const Clip: HRGN);

  procedure DrawLine(const DC: HDC; const X1, Y1, X2, Y2: Integer);
  begin
    MoveToEx(DC, X1, Y1, nil);  LineTo(DC, X2, Y2);
  end;
var
  RW, R, R2, RC: TRect;
  DC: HDC;
  HighlightPen, ShadowPen, SavePen: HPEN;
  FillBrush: HBRUSH;
label SkipFillRect;
begin
  { This works around WM_NCPAINT problem described at top of source code }
  {no!  R := Rect(0, 0, Width, Height);}
  GetWindowRect(Handle, RW);
  R := RW;
  OffsetRect(R, -R.Left, -R.Top);

  if not DrawToDC then
    DC := GetWindowDC(Handle)
  else
    DC := ADC;
  try
    { Use update region }
    if not DrawToDC then
      SelectNCUpdateRgn(Handle, DC, Clip);

    { Draw BoundLines }
    R2 := R;
    if (BoundLines <> []) and
       ((csDesigning in ComponentState) or HasVisibleToolbars) then begin
      HighlightPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNHIGHLIGHT));
      ShadowPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
      SavePen := SelectObject(DC, ShadowPen);
      if blTop in BoundLines then begin
        DrawLine(DC, R.Left, R.Top, R.Right, R.Top);
        Inc(R2.Top);
      end;
      if blLeft in BoundLines then begin
        DrawLine(DC, R.Left, R.Top, R.Left, R.Bottom);
        Inc(R2.Left);
      end;
      SelectObject(DC, HighlightPen);
      if blBottom in BoundLines then begin
        DrawLine(DC, R.Left, R.Bottom-1, R.Right, R.Bottom-1);
        Dec(R2.Bottom);
      end;
      if blRight in BoundLines then begin
        DrawLine(DC, R.Right-1, R.Top, R.Right-1, R.Bottom);
        Dec(R2.Right);
      end;
      SelectObject(DC, SavePen);
      DeleteObject(ShadowPen);
      DeleteObject(HighlightPen);
    end;
    Windows.GetClientRect(Handle, RC);
    if not IsRectEmpty(RC) then begin
      { ^ ExcludeClipRect can't be passed rectangles that have (Bottom < Top) or
        (Right < Left) since it doesn't treat them as empty }
      MapWindowPoints(Handle, 0, RC, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      if EqualRect(RC, R2) then
        { Skip FillRect because there would be nothing left after ExcludeClipRect }
        goto SkipFillRect;
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    end;
    FillBrush := CreateSolidBrush(ColorToRGB(Color));
    FillRect(DC, R2, FillBrush);
    DeleteObject(FillBrush);
  SkipFillRect:
  finally
    if not DrawToDC then
      ReleaseDC(Handle, DC);
  end;
end;

procedure TTBDock.WMNCPaint(var Message: TMessage);
begin
  DrawNCArea(False, 0, HRGN(Message.WParam));
end;

procedure DockNCPaintProc(Wnd: HWND; DC: HDC; AppData: TObject);
begin
  TTBDock(AppData).DrawNCArea(True, DC, 0);
end;

procedure TTBDock.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Handle, Message, DockNCPaintProc, Self);
end;

procedure TTBDock.WMPrintClient(var Message:
  {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF});
begin
  HandleWMPrintClient(PaintHandler, Message);
end;

procedure TTBDock.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if Assigned(FBackground) then
    FBackground.SysColorChanged;
end;

procedure TTBDock.RelayMsgToFloatingBars({$IFNDEF CLR}var{$ELSE}const{$ENDIF} Message: TMessage);
var
  I: Integer;
  T: TTBCustomDockableWindow;
begin
  for I := 0 to DockList.Count-1 do begin
    T := TTBCustomDockableWindow(DockList[I]);
    { Note: We must be careful about relaying WM_SYSCOMMAND. We can't send it
      to classes that don't have special handling for it (as indicated by the
      csMenuEvents style, which TTBToolWindow lacks) because the VCL's
      default handling would send it back to the main form, resulting in
      infinite recursion. }
    if ((Message.Msg <> WM_SYSCOMMAND) or (csMenuEvents in T.ControlStyle)) and
       T.Floating and T.CanFocus then begin
      Message.Result := T.Perform(Message.Msg, Message.WParam, Message.LParam);
      if Message.Result <> 0 then
        Exit;
    end;
  end;
end;

procedure TTBDock.WMSysCommand(var Message: TWMSysCommand);
begin
  { Relay WM_SYSCOMMAND messages to floating toolbars which were formerly
    docked. That way, items on floating menu bars can be accessed with Alt. }
  if Message.CmdType and $FFF0 = SC_KEYMENU then
    RelayMsgToFloatingBars({$IFNDEF CLR} TMessage(Message) {$ELSE} Message.OriginalMessage {$ENDIF});
end;

procedure TTBDock.CMDialogKey(var Message: TCMDialogKey);
begin
  RelayMsgToFloatingBars({$IFNDEF CLR} TMessage(Message) {$ELSE} Message.OriginalMessage {$ENDIF});
  if Message.Result = 0 then
    inherited;
end;

procedure TTBDock.CMDialogChar(var Message: TCMDialogChar);
begin
  RelayMsgToFloatingBars({$IFNDEF CLR} TMessage(Message) {$ELSE} Message.OriginalMessage {$ENDIF});
  if Message.Result = 0 then
    inherited;
end;

{ TTBDock - property access methods }

procedure TTBDock.SetAllowDrag(Value: Boolean);
var
  I: Integer;
begin
  if FAllowDrag <> Value then begin
    FAllowDrag := Value;
    for I := 0 to ControlCount-1 do
      if Controls[I] is TTBCustomDockableWindow then
        RecalcNCArea(TTBCustomDockableWindow(Controls[I]));
  end;
end;

function TTBDock.UsingBackground: Boolean;
begin
  Result := Assigned(FBackground) and FBackground.UsingBackground;
end;

procedure TTBDock.DrawBackground(DC: HDC; const DrawRect: TRect);
begin
  FBackground.Draw(DC, DrawRect);
end;

procedure TTBDock.InvalidateBackgrounds;
{ Called after background is changed }
var
  I: Integer;
  T: TTBCustomDockableWindow;
begin
  Invalidate;
  { Synchronize child toolbars also }
  for I := 0 to DockList.Count-1 do begin
    T := TTBCustomDockableWindow(DockList[I]);
    if ToolbarVisibleOnDock(T) then
      { Invalidate both non-client and client area }
      InvalidateAll(T);
  end;
end;

procedure TTBDock.SetBackground(Value: TTBBasicBackground);
begin
  if FBackground <> Value then begin
    if Assigned(FBackground) then
      FBackground.UnregisterChanges(BackgroundChanged);
    FBackground := Value;
    if Assigned(Value) then begin
      Value.FreeNotification(Self);
      Value.RegisterChanges(BackgroundChanged);
    end;
    InvalidateBackgrounds;
  end;
end;

procedure TTBDock.BackgroundChanged(Sender: TObject);
begin
  InvalidateBackgrounds;
end;

procedure TTBDock.SetBackgroundOnToolbars(Value: Boolean);
begin
  if FBkgOnToolbars <> Value then begin
    FBkgOnToolbars := Value;
    InvalidateBackgrounds;
  end;
end;

procedure TTBDock.SetBoundLines(Value: TTBDockBoundLines);
var
  X, Y: Integer;
  B: TTBDockBoundLines;
begin
  if FBoundLines <> Value then begin
    FBoundLines := Value;
    X := 0;
    Y := 0;
    B := BoundLines;  { optimization }
    if blTop in B then Inc(Y);
    if blBottom in B then Inc(Y);
    if blLeft in B then Inc(X);
    if blRight in B then Inc(X);
    FNonClientWidth := X;
    FNonClientHeight := Y;
    RecalcNCArea(Self);
  end;
end;

procedure TTBDock.SetFixAlign(Value: Boolean);
begin
  if FFixAlign <> Value then begin
    FFixAlign := Value;
    ArrangeToolbars;
  end;
end;

procedure TTBDock.SetPosition(Value: TTBDockPosition);
begin
  if (FPosition <> Value) and (ControlCount <> 0) then
    raise EInvalidOperation.Create(STBDockCannotChangePosition);
  FPosition := Value;
  case Position of
    dpTop: Align := alTop;
    dpBottom: Align := alBottom;
    dpLeft: Align := alLeft;
    dpRight: Align := alRight;
  end;
end;

function TTBDock.GetToolbarCount: Integer;
begin
  Result := DockVisibleList.Count;
end;

function TTBDock.GetToolbars(Index: Integer): TTBCustomDockableWindow;
begin
  Result := TTBCustomDockableWindow(DockVisibleList[Index]);
end;

(*function TTBDock.GetVersion: TToolbar97Version;
begin
  Result := Toolbar97VersionPropText;
end;

procedure TTBDock.SetVersion(const Value: TToolbar97Version);
begin
  { write method required for the property to show up in Object Inspector }
end;*)


{ TTBFloatingWindowParent - Internal }

procedure TTBFloatingWindowParent.CreateParams(var Params: TCreateParams);
const
  ThickFrames: array[Boolean] of DWORD = (0, WS_THICKFRAME);
begin
  inherited;

  { Disable complete redraws when size changes. CS_H/VREDRAW cause flicker
    and are not necessary for this control at run time }
  if not(csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not(CS_HREDRAW or CS_VREDRAW);

  with Params do begin
    { Note: WS_THICKFRAME and WS_BORDER styles are included to ensure that
      sizing grips are displayed on child controls with scrollbars. The
      thick frame or border is not drawn by Windows; TCustomToolWindow97
      handles all border drawing by itself. }
    if not(csDesigning in ComponentState) then
      Style := WS_POPUP or WS_BORDER or ThickFrames[FDockableWindow.FResizable]
    else
      Style := Style or WS_BORDER or ThickFrames[FDockableWindow.FResizable];
    { The WS_EX_TOOLWINDOW style is needed so there isn't a taskbar button
      for the toolbar when FloatingMode = fmOnTopOfAllForms. }
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;

procedure TTBFloatingWindowParent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParentForm) then
    FParentForm := nil;
end;

procedure TTBFloatingWindowParent.AlignControls(AControl: TControl; var Rect: TRect);
begin
  { ignore Align setting of the child toolbar }
end;

procedure TTBFloatingWindowParent.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
{$IFDEF CLR}
var
  Temp: TMinMaxInfo;
{$ENDIF}
begin
  inherited;
  { Because the window uses the WS_THICKFRAME style (but not for the usual
    purpose), it must process the WM_GETMINMAXINFO message to remove the
    minimum and maximum size limits it imposes by default. }
  {$IFNDEF CLR}
  with Message.MinMaxInfo^ do begin
  {$ELSE}
  Temp := Message.MinMaxInfo;
  with Temp do begin
  {$ENDIF}
    with ptMinTrackSize do begin
      X := 1;
      Y := 1;
      { Note to self: Don't put GetMinimumSize code here, since
        ClientWidth/Height values are sometimes invalid during a RecreateWnd }
    end;
    with ptMaxTrackSize do begin
      { Because of the 16-bit (signed) size limitations of Windows 95,
        Smallints must be used instead of Integers or Longints }
      X := High(Smallint);
      Y := High(Smallint);
    end;
  end;
  {$IFDEF CLR}
  Message.MinMaxInfo := Temp;
  {$ENDIF}
end;

procedure TTBFloatingWindowParent.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
begin
  { Must override TCustomForm/TForm's CM_SHOWINGCHANGED handler so that the
    form doesn't get activated when Visible is set to True. }
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing and FShouldShow]);
end;

procedure TTBFloatingWindowParent.CMDialogKey(var Message: TCMDialogKey);
begin
  { If Escape if pressed on a floating toolbar, return focus to the form }
  if (Message.CharCode = VK_ESCAPE) and
     (KeyDataToShiftState(ClipToLongint(Message.KeyData)) = []) and
     Assigned(ParentForm) then begin
    ParentForm.SetFocus;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TTBFloatingWindowParent.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RedrawNCArea([twrdCaption]);
end;

function TTBFloatingWindowParent.GetCaptionRect(const AdjustForBorder,
  MinusCloseButton: Boolean): TRect;
var
  P: TPoint;
begin
  Result := Rect(0, 0, ClientWidth, GetSmallCaptionHeight-1);
  if MinusCloseButton then
    Dec(Result.Right, Result.Bottom);
  if AdjustForBorder then begin
    P := FDockableWindow.GetFloatingBorderSize;
    OffsetRect(Result, P.X, P.Y);
  end;
end;

function TTBFloatingWindowParent.GetCloseButtonRect(const AdjustForBorder: Boolean): TRect;
begin
  Result := GetCaptionRect(AdjustForBorder, False);
  Result.Left := Result.Right - (GetSmallCaptionHeight-1);
end;

procedure TTBFloatingWindowParent.WMNCCalcSize(var Message: TWMNCCalcSize);

  procedure ApplyToRect(var R: TRect);
  var
    TL, BR: TPoint;
  begin
    FDockableWindow.GetFloatingNCArea(TL, BR);
    Inc(R.Left, TL.X);
    Inc(R.Top, TL.Y);
    Dec(R.Right, BR.X);
    Dec(R.Bottom, BR.Y);
  end;

{$IFDEF CLR}
var
  Params: TNCCalcSizeParams;
{$ENDIF}
begin
  { Doesn't call inherited since it overrides the normal NC sizes }
  Message.Result := 0;
  {$IFNDEF CLR}
  ApplyToRect(Message.CalcSize_Params.rgrc[0]);
  {$ELSE}
  Params := Message.CalcSize_Params;
  ApplyToRect(Params.rgrc0);
  Message.CalcSize_Params := Params;
  {$ENDIF}
end;

procedure TTBFloatingWindowParent.WMNCPaint(var Message: TMessage);
begin
  { Don't call inherited because it overrides the default NC painting }
  DrawNCArea(False, 0, HRGN(Message.WParam), twrdAll);
end;

procedure FloatingWindowParentNCPaintProc(Wnd: HWND; DC: HDC; AppData: TObject);
begin
  with TTBFloatingWindowParent(AppData) do
    DrawNCArea(True, DC, 0, twrdAll);
end;

procedure TTBFloatingWindowParent.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Handle, Message, FloatingWindowParentNCPaintProc, Self);
end;

procedure TTBFloatingWindowParent.WMPrintClient(var Message:
  {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF});
begin
  HandleWMPrintClient(PaintHandler, Message);
end;

procedure TTBFloatingWindowParent.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
  BorderSize: TPoint;
  C: Integer;
begin
  inherited;
  with Message do begin
    P := SmallPointToPoint(Pos);
    GetWindowRect(Handle, R);
    Dec(P.X, R.Left);  Dec(P.Y, R.Top);
    if Result <> HTCLIENT then begin
      Result := HTNOWHERE;
      if FDockableWindow.ShowCaption and PtInRect(GetCaptionRect(True, False), P) then begin
        if FDockableWindow.FCloseButton and PtInRect(GetCloseButtonRect(True), P) then
          Result := HT_TB2k_Close
        else
          Result := HT_TB2k_Caption;
      end
      else
      if FDockableWindow.Resizable then begin
        BorderSize := FDockableWindow.GetFloatingBorderSize;
        if not(tbdsResizeEightCorner in FDockableWindow.FDockableWindowStyles) then begin
          if (P.Y >= 0) and (P.Y < BorderSize.Y) then Result := HTTOP else
          if (P.Y < Height) and (P.Y >= Height-BorderSize.Y-1) then Result := HTBOTTOM else
          if (P.X >= 0) and (P.X < BorderSize.X) then Result := HTLEFT else
          if (P.X < Width) and (P.X >= Width-BorderSize.X-1) then Result := HTRIGHT;
        end
        else begin
          C := BorderSize.X + (GetSmallCaptionHeight-1);
          if (P.X >= 0) and (P.X < BorderSize.X) then begin
            Result := HTLEFT;
            if (P.Y < C) then Result := HTTOPLEFT else
            if (P.Y >= Height-C) then Result := HTBOTTOMLEFT;
          end
          else
          if (P.X < Width) and (P.X >= Width-BorderSize.X-1) then begin
            Result := HTRIGHT;
            if (P.Y < C) then Result := HTTOPRIGHT else
            if (P.Y >= Height-C) then Result := HTBOTTOMRIGHT;
          end
          else
          if (P.Y >= 0) and (P.Y < BorderSize.Y) then begin
            Result := HTTOP;
            if (P.X < C) then Result := HTTOPLEFT else
            if (P.X >= Width-C) then Result := HTTOPRIGHT;
          end
          else
          if (P.Y < Height) and (P.Y >= Height-BorderSize.Y-1) then begin
            Result := HTBOTTOM;
            if (P.X < C) then Result := HTBOTTOMLEFT else
            if (P.X >= Width-C) then Result := HTBOTTOMRIGHT;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTBFloatingWindowParent.SetCloseButtonState(Pushed: Boolean);
begin
  if FCloseButtonDown <> Pushed then begin
    FCloseButtonDown := Pushed;
    RedrawNCArea([twrdCloseButton]);
  end;
end;

procedure TTBFloatingWindowParent.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  P: TPoint;
  R, BR: TRect;
begin
  case ClipToLongint(Message.HitTest) of
    HT_TB2k_Caption: begin
        P := FDockableWindow.ScreenToClient(Point(Message.XCursor, Message.YCursor));
        FDockableWindow.BeginMoving(P.X, P.Y);
      end;
    HTLEFT..HTBOTTOMRIGHT:
      if FDockableWindow.Resizable then
        FDockableWindow.BeginSizing(TTBSizeHandle(ClipToLongint(Message.HitTest) - HTLEFT));
    HT_TB2k_Close: begin
        GetWindowRect(Handle, R);
        BR := GetCloseButtonRect(True);
        OffsetRect(BR, R.Left, R.Top);
        if CloseButtonLoop(Handle, BR, SetCloseButtonState) then
          FDockableWindow.Close;
      end;
  else
    inherited;
  end;
end;

procedure TTBFloatingWindowParent.WMNCLButtonDblClk(var Message: TWMNCLButtonDblClk);
begin
  if ClipToLongint(Message.HitTest) = HT_TB2k_Caption then
    FDockableWindow.DoubleClick;
end;

procedure TTBFloatingWindowParent.WMNCRButtonUp(var Message: TWMNCRButtonUp);
begin
  FDockableWindow.ShowNCContextMenu(Message.XCursor, Message.YCursor);
end;

procedure TTBFloatingWindowParent.WMClose(var Message: TWMClose);
var
  MDIParentForm: TTBCustomForm;
begin
  { A floating toolbar does not use WM_CLOSE messages when its close button
    is clicked, but Windows still sends a WM_CLOSE message if the user
    presses Alt+F4 while one of the toolbar's controls is focused. Inherited
    is not called since we do not want Windows' default processing - which
    destroys the window. Instead, relay the message to the parent form. }
  MDIParentForm := GetMDIParent(TBGetToolWindowParentForm(FDockableWindow));
  if Assigned(MDIParentForm) and MDIParentForm.HandleAllocated then
    SendMessage(MDIParentForm.Handle, WM_CLOSE, 0, 0);
  { Note to self: MDIParentForm is used instead of OwnerForm since MDI
    childs don't process Alt+F4 as Close }
end;

procedure TTBFloatingWindowParent.WMActivate(var Message: TWMActivate);
var
  ParentForm: TTBCustomForm;
begin
  if csDesigning in ComponentState then begin
    inherited;
    Exit;
  end;

  ParentForm := GetMDIParent(TBGetToolWindowParentForm(FDockableWindow));

  if Assigned(ParentForm) and ParentForm.HandleAllocated then
    SendMessage(ParentForm.Handle, WM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);

  if Message.Active <> WA_INACTIVE then begin
    { This works around a "gotcha" in TCustomForm.CMShowingChanged. When a form
      is hidden, it uses the internal VCL function FindTopMostWindow to
      find a new active window. The problem is that handles of floating
      toolbars on the form being hidden can be returned by
      FindTopMostWindow, so the following code is used to prevent floating
      toolbars on the hidden form from being left active. }
    if not IsWindowVisible(Handle) then
      { ^ Calling IsWindowVisible with a floating toolbar handle will
         always return False if its parent form is hidden since the
         WH_CALLWNDPROC hook automatically updates the toolbars'
         visibility. }
      { Find and activate a window besides this toolbar }
      SetActiveWindow(FindTopLevelWindow(Handle))
    else
      { If the toolbar is being activated and the previous active window wasn't
        its parent form, the form is activated instead. This is done so that if
        the application is deactivated while a floating toolbar was active and
        the application is reactivated again, it returns focus to the form. }
      if Assigned(ParentForm) and ParentForm.HandleAllocated and
         (Message.ActiveWindow <> ParentForm.Handle) then
        SetActiveWindow(ParentForm.Handle);
  end;
end;

procedure TTBFloatingWindowParent.WMMouseActivate(var Message: TWMMouseActivate);
var
  ParentForm, MDIParentForm: TTBCustomForm;
begin
  if csDesigning in ComponentState then begin
    inherited;
    Exit;
  end;

  { When floating, prevent the toolbar from activating when clicked.
    This is so it doesn't take the focus away from the window that had it }
  Message.Result := MA_NOACTIVATE;

  { Similar to calling BringWindowToTop, but doesn't activate it }
  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);

  { Since it is returning MA_NOACTIVATE, activate the form instead. }
  ParentForm := TBGetToolWindowParentForm(FDockableWindow);
  MDIParentForm := GetMDIParent(ParentForm);
  if (FDockableWindow.FFloatingMode = fmOnTopOfParentForm) and
     FDockableWindow.FActivateParent and
     Assigned(MDIParentForm) and (GetActiveWindow <> Handle) then begin
    { ^ Note to self: The GetActiveWindow check must be in there so that
        double-clicks work properly on controls like Edits }
    if MDIParentForm.HandleAllocated then
      SetActiveWindow(MDIParentForm.Handle);
    if (MDIParentForm <> ParentForm) and  { if it's an MDI child form }
       ParentForm.HandleAllocated then
      BringWindowToTop(ParentForm.Handle);
  end;
end;

procedure TTBFloatingWindowParent.WMMove(var Message: TWMMove);
begin
  inherited;
  FDockableWindow.Moved;
end;

procedure TTBFloatingWindowParent.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat);
{ Redraws all the non-client area (the border, title bar, and close button) of
  the toolbar when it is floating. }
const
  BorderColors: array[Boolean] of Integer =
    (COLOR_ACTIVEBORDER, COLOR_INACTIVEBORDER);
  CaptionBkColors: array[Boolean, Boolean] of Integer =
    ((COLOR_ACTIVECAPTION, COLOR_INACTIVECAPTION),
     (COLOR_GRADIENTACTIVECAPTION, COLOR_GRADIENTINACTIVECAPTION));
  CloseButtonState: array[Boolean] of UINT = (0, DFCS_PUSHED);
var
  DC: HDC;
  R, R2: TRect;
  Gradient: Boolean;
  SavePen: HPEN;
  SaveIndex: Integer;
  S: TPoint;
begin
  if not HandleAllocated then Exit;

  if not DrawToDC then
    DC := GetWindowDC(Handle)
  else
    DC := ADC;
  try
    { Use update region }
    if not DrawToDC then
      SelectNCUpdateRgn(Handle, DC, Clip);

    { Work around an apparent NT 4.0 & 2000 bug. If the width of the DC is
      greater than the width of the screen, then any call to ExcludeClipRect
      inexplicably shrinks the clipping rectangle to the screen width. I've
      found that calling IntersectClipRect as done below magically fixes the
      problem (but I'm not sure why). }
    GetWindowRect(Handle, R);  OffsetRect(R, -R.Left, -R.Top);
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);

    Gradient := GetSystemParametersInfoBool(SPI_GETGRADIENTCAPTIONS, False);

    { Border }
    if twrdBorder in RedrawWhat then begin
      { This works around WM_NCPAINT problem described at top of source code }
      {no!  R := Rect(0, 0, Width, Height);}
      GetWindowRect(Handle, R);  OffsetRect(R, -R.Left, -R.Top);
      R2 := R;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
      S := FDockableWindow.GetFloatingBorderSize;
      InflateRect(R2, -(S.X - 1), -(S.Y - 1));
      FrameRect(DC, R2, GetSysColorBrush(COLOR_BTNFACE));
      SaveIndex := SaveDC(DC);
      ExcludeClipRect(DC, R2.Left, R2.Top, R2.Right, R2.Bottom);
      FillRect(DC, R, GetSysColorBrush(BorderColors[FDockableWindow.FInactiveCaption]));
      RestoreDC(DC, SaveIndex);
    end;

    if FDockableWindow.ShowCaption then begin
      if (twrdCaption in RedrawWhat) and FDockableWindow.FCloseButton and
         (twrdCloseButton in RedrawWhat) then
        SaveIndex := SaveDC(DC)
      else
        SaveIndex := 0;
      try
        if SaveIndex <> 0 then
          with GetCloseButtonRect(True) do
            { Reduces flicker }
            ExcludeClipRect(DC, Left, Top, Right, Bottom);

        { Caption }
        if twrdCaption in RedrawWhat then begin
          R := GetCaptionRect(True, FDockableWindow.FCloseButton);
          DrawSmallWindowCaption(Handle, DC, R, Caption,
            not FDockableWindow.FInactiveCaption);

          { Line below caption }
          R := GetCaptionRect(True, False);
          SavePen := SelectObject(DC, CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNFACE)));
          MoveToEx(DC, R.Left, R.Bottom, nil);
          LineTo(DC, R.Right, R.Bottom);
          DeleteObject(SelectObject(DC, SavePen));
        end;
      finally
        if SaveIndex <> 0 then
          RestoreDC(DC, SaveIndex);
      end;

      { Close button }
      if FDockableWindow.FCloseButton then begin
        R := GetCloseButtonRect(True);
        R2 := R;
        InflateRect(R2, 0, -2);
        Dec(R2.Right, 2);
        if twrdCaption in RedrawWhat then begin
          SaveIndex := SaveDC(DC);
          ExcludeClipRect(DC, R2.Left, R2.Top, R2.Right, R2.Bottom);
          FillRect(DC, R, GetSysColorBrush(CaptionBkColors[Gradient,
            FDockableWindow.FInactiveCaption]));
          RestoreDC(DC, SaveIndex);
        end;
        if twrdCloseButton in RedrawWhat then
          DrawFrameControl(DC, R2, DFC_CAPTION, DFCS_CAPTIONCLOSE or
            CloseButtonState[FCloseButtonDown]);
      end;
    end;
  finally
    if not DrawToDC then
      ReleaseDC(Handle, DC);
  end;
end;

procedure TTBFloatingWindowParent.RedrawNCArea(const RedrawWhat: TTBToolWindowNCRedrawWhat);
begin
  { Note: IsWindowVisible is called as an optimization. There's no need to
    draw on invisible windows. }
  if HandleAllocated and IsWindowVisible(Handle) then
    DrawNCArea(False, 0, 0, RedrawWhat);
end;

procedure TTBFloatingWindowParent.CallRecreateWnd;
{ This method exists for Delphi.NET: If we try to call RecreateWnd directly
  in TTBCustomDockableWindow.SetResizable, we get this compiler error:
  "Only methods of descendant types may access protected member
  [Borland.Vcl]TWinControl.RecreateWnd across assembly boundaries" }
begin
  RecreateWnd;
end;


{ TTBCustomDockableWindow }

constructor TTBCustomDockableWindow.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle +
    [csAcceptsControls, csClickEvents, csDoubleClicks, csSetCaption] -
    [csCaptureMouse{capturing is done manually}, csOpaque];
  FAutoResize := True;
  FActivateParent := True;
  FBorderStyle := bsSingle;
  FCloseButton := True;
  FDockableTo := [dpTop, dpBottom, dpLeft, dpRight];
  FDockableWindowStyles := [tbdsResizeEightCorner, tbdsResizeClipCursor];
  FDockPos := -1;
  FDragHandleStyle := dhSingle;
  FEffectiveDockRow := -1;
  FHideWhenInactive := True;
  FResizable := True;
  FShowCaption := True;
  FSmoothDrag := True;
  FUseLastDock := True;

  Color := clBtnFace;

  if not(csDesigning in ComponentState) then
    InstallHookProc(Self, ToolbarHookProc, [hpSendActivate, hpSendActivateApp,
      hpSendWindowPosChanged, hpPreDestroy]);
  InitTrackMouseEvent;
end;

destructor TTBCustomDockableWindow.Destroy;
begin
  inherited;
  FreeAndNil(FDockForms);  { must be done after 'inherited' because Notification accesses FDockForms }
  FreeAndNil(FFloatParent);
  UninstallHookProc(Self, ToolbarHookProc);
end;

function TTBCustomDockableWindow.HasParent: Boolean;
begin
  if Parent is TTBFloatingWindowParent then
    Result := False
  else
    Result := inherited HasParent;
end;

function TTBCustomDockableWindow.GetParentComponent: TComponent;
begin
  if Parent is TTBFloatingWindowParent then
    Result := nil
  else
    Result := inherited GetParentComponent;
end;

procedure TTBCustomDockableWindow.Moved;
begin
  if not(csLoading in ComponentState) and Assigned(FOnMove) and (FDisableOnMove <= 0) then
    FOnMove(Self);
end;

procedure TTBCustomDockableWindow.WMMove(var Message: TWMMove);

  procedure Redraw;
  { Redraws the control using an off-screen bitmap to avoid flicker }
  var
    CR, R: TRect;
    W: HWND;
    DC, BmpDC: HDC;
    Bmp: HBITMAP;
  begin
    if not HandleAllocated then Exit;
    CR := ClientRect;
    W := Handle;
    if GetUpdateRect(W, R, False) and EqualRect(R, CR) then begin
      { The client area is already completely invalid, so don't bother using
        an off-screen bitmap }
      InvalidateAll(Self);
      Exit;
    end;
    ValidateRect(W, nil);
    BmpDC := 0;
    Bmp := 0;
    DC := GetDC(W);
    try
      BmpDC := CreateCompatibleDC(DC);
      Bmp := CreateCompatibleBitmap(DC, CR.Right, CR.Bottom);
      SelectObject(BmpDC, Bmp);
      SendMessage(W, WM_NCPAINT, 0, 0);
      SendMessage(W, WM_ERASEBKGND, WPARAM(BmpDC), 0);
      SendMessage(W, WM_PAINT, WPARAM(BmpDC), 0);
      BitBlt(DC, 0, 0, CR.Right, CR.Bottom, BmpDC, 0, 0, SRCCOPY);
    finally
      if BmpDC <> 0 then DeleteDC(BmpDC);
      if Bmp <> 0 then DeleteObject(Bmp);
      ReleaseDC(W, DC);
    end;
  end;

begin
  inherited;
  FMoved := True;
  if Docked and CurrentDock.UsingBackground then begin
    { Needs to redraw so that the background is lined up with the dock at the
      new position. }
    Redraw;
  end;
  Moved;
end;

{$IFNDEF JR_D4}
procedure TTBCustomDockableWindow.WMSize(var Message: TWMSize);
begin
  inherited;
  if not(csLoading in ComponentState) and Assigned(FOnResize) then
    FOnResize(Self);
end;
{$ENDIF}

procedure TTBCustomDockableWindow.UpdateCaptionState;
{ Updates the caption active/inactive state of a floating tool window.
  Called when the tool window is visible or is about to be shown. }

  function IsPopupWindowActive: Boolean;
  const
    IID_ITBPopupWindow: TGUID = '{E45CBE74-1ECF-44CB-B064-6D45B1924708}';
  var
    Ctl: TWinControl;
    {$IFDEF CLR}
    Intfs: array of System.Type;
    I: Integer;
    {$ENDIF}
  begin
    Ctl := FindControl(GetActiveWindow);
    { Instead of using "is TTBPopupWindow", which would require linking to the
      TB2Item unit, check if the control implements the ITBPopupWindow
      interface. This will tell us if it's a TTBPopupWindow or descendant. }
    {$IFNDEF CLR}
    Result := Assigned(Ctl) and Assigned(Ctl.GetInterfaceEntry(IID_ITBPopupWindow));
    {$ELSE}
    Result := False;
    if Assigned(Ctl) then begin
      Intfs := TypeOf(Ctl).GetInterfaces;
      for I := Low(Intfs) to High(Intfs) do begin
        if Intfs[I].GUID = IID_ITBPopupWindow then begin
          Result := True;
          Break;
        end;
      end;
    end;
    {$ENDIF}
  end;

  function GetActiveFormWindow: HWND;
  var
    Ctl: TWinControl;
  begin
    Result := GetActiveWindow;
    { If the active window is a TTBFloatingWindowParent (i.e. a control on a
      floating toolbar is focused), return the parent form handle instead }
    Ctl := FindControl(Result);
    if Assigned(Ctl) and (Ctl is TTBFloatingWindowParent) then begin
      Ctl := TTBFloatingWindowParent(Ctl).ParentForm;
      if Assigned(Ctl) and Ctl.HandleAllocated then
        Result := Ctl.Handle;
    end;
  end;

var
  Inactive: Boolean;
  ActiveWnd: HWND;
begin
  { Update caption state if floating, but not if a control on a popup window
    (e.g. a TTBEditItem) is currently focused; we don't want the captions on
    all floating toolbars to turn gray in that case. (The caption state will
    get updated when we're called the next time the active window changes,
    i.e. when the user dismisses the popup window.) }
  if (Parent is TTBFloatingWindowParent) and Parent.HandleAllocated and
     not IsPopupWindowActive then begin
    Inactive := False;
    if not ApplicationIsActive then
      Inactive := True
    else if (FFloatingMode = fmOnTopOfParentForm) and
       (HWND(GetWindowLong(Parent.Handle, GWL_HWNDPARENT)) <> Application.Handle) then begin
      { Use inactive caption if the active window doesn't own the float parent
        (directly or indirectly). Note: For compatibility with browser-embedded
        TActiveForms, we use IsAncestorOfWindow instead of checking
        TBGetToolWindowParentForm. }
      ActiveWnd := GetActiveFormWindow;
      if (ActiveWnd = 0) or not IsAncestorOfWindow(ActiveWnd, Parent.Handle) then
        Inactive := True;
    end;
    if FInactiveCaption <> Inactive then begin
      FInactiveCaption := Inactive;
      TTBFloatingWindowParent(Parent).RedrawNCArea(twrdAll);
    end;
  end;
end;

function TTBCustomDockableWindow.GetShowingState: Boolean;

  function IsWindowVisibleAndNotMinimized(Wnd: HWND): Boolean;
  begin
    Result := IsWindowVisible(Wnd);
    if Result then begin
      { Wnd may not be a top-level window (e.g. in the case of an MDI child
        form, or an ActiveForm embedded in a web page), so go up the chain of
        parent windows and see if any of them are minimized }
      repeat
        if IsIconic(Wnd) then begin
          Result := False;
          Break;
        end;
        { Stop if we're at a top-level window (no need to check owner windows) }
        if GetWindowLong(Wnd, GWL_STYLE) and WS_CHILD = 0 then
          Break;
        Wnd := GetParent(Wnd);
      until Wnd = 0;
    end;
  end;

var
  HideFloatingToolbars: Boolean;
  ParentForm: TTBCustomForm;
begin
  Result := Showing and (FHidden = 0);
  if Floating and not(csDesigning in ComponentState) then begin
    HideFloatingToolbars := FFloatingMode = fmOnTopOfParentForm;
    if HideFloatingToolbars then begin
      ParentForm := TBGetToolWindowParentForm(Self);
      if Assigned(ParentForm) and ParentForm.HandleAllocated and
         IsWindowVisibleAndNotMinimized(ParentForm.Handle) then
        HideFloatingToolbars := False;
    end;
    Result := Result and not (HideFloatingToolbars or (FHideWhenInactive and not ApplicationIsActive));
  end;
end;

procedure TTBCustomDockableWindow.UpdateVisibility;
{ Updates the visibility of the tool window, and additionally the caption
  state if floating and showing }
var
  IsVisible: Boolean;
begin
  if HandleAllocated then begin
    IsVisible := IsWindowVisible(Handle);
    if IsVisible <> GetShowingState then begin
      Perform(CM_SHOWINGCHANGED, 0, 0);
      { Note: CMShowingChanged will call UpdateCaptionState automatically
        when floating and showing }
    end
    else if IsVisible and Floating then begin
      { If we're floating and we didn't send the CM_SHOWINGCHANGED message
        then we have to call UpdateCaptionState manually }
      UpdateCaptionState;
    end;
  end;
end;

function IsTopmost(const Wnd: HWND): Boolean;
begin
  Result := GetWindowLong(Wnd, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0;
end;

procedure TTBCustomDockableWindow.UpdateTopmostFlag;
const
  Wnds: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
var
  ShouldBeTopmost: Boolean;
begin
  if HandleAllocated then begin
    if FFloatingMode = fmOnTopOfAllForms then
      ShouldBeTopmost := True
    else
      ShouldBeTopmost := IsTopmost(HWND(GetWindowLong(Parent.Handle, GWL_HWNDPARENT)));
    if ShouldBeTopmost <> IsTopmost(Parent.Handle) then
      { ^ it must check if it already was topmost or non-topmost or else
        it causes problems on Win95/98 for some reason }
      SetWindowPos(Parent.Handle, Wnds[ShouldBeTopmost], 0, 0, 0, 0,
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TTBCustomDockableWindow.CMShowingChanged(var Message: TMessage);

  function GetPrevWnd(W: HWND): HWND;
  var
    WasTopmost, Done: Boolean;
    ParentWnd: HWND;
  begin
    WasTopmost := IsTopmost(Parent.Handle);
    Result := W;
    repeat
      Done := True;
      Result := GetWindow(Result, GW_HWNDPREV);
      ParentWnd := Result;
      while ParentWnd <> 0 do begin
        if WasTopmost and not IsTopmost(ParentWnd) then begin
          Done := False;
          Break;
        end;
        ParentWnd := HWND(GetWindowLong(ParentWnd, GWL_HWNDPARENT));
        if ParentWnd = W then begin
          Done := False;
          Break;
        end;
      end;
    until Done;
  end;

const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
var
  Show: Boolean;
  Form: TTBCustomForm;
begin
  { inherited isn't called since TTBCustomDockableWindow handles CM_SHOWINGCHANGED
    itself. For reference, the original TWinControl implementation is:
    const
      ShowFlags: array[Boolean] of Word = (
        SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
        SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
    begin
      SetWindowPos(FHandle, 0, 0, 0, 0, 0, ShowFlags[FShowing]);
    end;
  }
  if HandleAllocated then begin
    Show := GetShowingState;
    if Parent is TTBFloatingWindowParent then begin
      if Show then begin
        { If the toolbar is floating, set its "owner window" to the parent form
          so that the toolbar window always stays on top of the form }
        if FFloatingMode = fmOnTopOfParentForm then begin
          Form := GetMDIParent(TBGetToolWindowParentForm(Self));
          if Assigned(Form) and Form.HandleAllocated and
             (HWND(GetWindowLong(Parent.Handle, GWL_HWNDPARENT)) <> Form.Handle) then begin
            SetWindowOwner(Parent.Handle, Form.Handle);
            { Following is necessarily to make it immediately realize the
              GWL_HWNDPARENT change }
            SetWindowPos(Parent.Handle, GetPrevWnd(Form.Handle), 0, 0, 0, 0, SWP_NOACTIVATE or
              SWP_NOMOVE or SWP_NOSIZE);
          end;
        end
        else begin
          SetWindowOwner(Parent.Handle, Application.Handle);
        end;
        { Initialize caption state after setting owner but before showing }
        UpdateCaptionState;
      end;
      UpdateTopmostFlag;
      { Show/hide the TTBFloatingWindowParent. The following lines had to be
        added to fix a problem that was in 1.65d/e. In 1.65d/e, it always
        kept TTBFloatingWindowParent visible (this change was made to improve
        compatibility with D4's Actions), but this for some odd reason would
        cause a Stack Overflow error if the program's main form was closed
        while a floating toolwindow was focused. (This problem did not occur
        on NT.) }
      TTBFloatingWindowParent(Parent).FShouldShow := Show;
      Parent.Perform(CM_SHOWINGCHANGED, 0, 0);
    end;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, ShowFlags[Show]);
    if not Show and (GetActiveWindow = Handle) then
      { If the window is hidden but is still active, find and activate a
        different window }
      SetActiveWindow(FindTopLevelWindow(Handle));
  end;
end;

procedure TTBCustomDockableWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;

  { Disable complete redraws when size changes. CS_H/VREDRAW cause flicker
    and are not necessary for this control at run time }
  if not(csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not(CS_HREDRAW or CS_VREDRAW);
end;

procedure TTBCustomDockableWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FDefaultDock then
      FDefaultDock := nil
    else
    if AComponent = FLastDock then
      FLastDock := nil
    else
      RemoveFromList(FDockForms, AComponent);
  end;
end;

procedure TTBCustomDockableWindow.MoveOnScreen(const OnlyIfFullyOffscreen: Boolean);
{ Moves the (floating) toolbar so that it is fully (or at least mostly) in
  view on the screen }
var
  R, S, Test: TRect;
begin
  if Floating then begin
    R := Parent.BoundsRect;
    S := GetRectOfMonitorContainingRect(R, True);

    if OnlyIfFullyOffscreen and IntersectRect(Test, R, S) then
      Exit;

    if R.Right > S.Right then
      OffsetRect(R, S.Right - R.Right, 0);
    if R.Bottom > S.Bottom then
      OffsetRect(R, 0, S.Bottom - R.Bottom);
    if R.Left < S.Left then
      OffsetRect(R, S.Left - R.Left, 0);
    if R.Top < S.Top then
      OffsetRect(R, 0, S.Top - R.Top);
    Parent.BoundsRect := R;
  end;
end;

procedure TTBCustomDockableWindow.ReadPositionData(const Data: TTBReadPositionData);
begin
end;

procedure TTBCustomDockableWindow.DoneReadingPositionData(const Data: TTBReadPositionData);
begin
end;

procedure TTBCustomDockableWindow.WritePositionData(const Data: TTBWritePositionData);
begin
end;

procedure TTBCustomDockableWindow.InitializeOrdering;
begin
end;

procedure TTBCustomDockableWindow.SizeChanging(const AWidth, AHeight: Integer);
begin
end;

procedure TTBCustomDockableWindow.ReadSavedAtRunTime(Reader: TReader);
begin
  FSavedAtRunTime := Reader.ReadBoolean;
end;

procedure TTBCustomDockableWindow.WriteSavedAtRunTime(Writer: TWriter);
begin
  { WriteSavedAtRunTime only called when not(csDesigning in ComponentState) }
  Writer.WriteBoolean(True);
end;

procedure TTBCustomDockableWindow.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('SavedAtRunTime', ReadSavedAtRunTime,
    WriteSavedAtRunTime, not(csDesigning in ComponentState));
end;

procedure TTBCustomDockableWindow.Loaded;
var
  R: TRect;
begin
  inherited;
  { Adjust coordinates if it was initially floating }
  if not FSavedAtRunTime and not(csDesigning in ComponentState) and
     (Parent is TTBFloatingWindowParent) then begin
    R := BoundsRect;
    MapWindowPoints(TBValidToolWindowParentForm(Self).Handle, 0, R, 2);
    BoundsRect := R;
    MoveOnScreen(False);
  end;
  InitializeOrdering;
  { Arranging is disabled while component was loading, so arrange now }
  Arrange;
end;

procedure TTBCustomDockableWindow.BeginUpdate;
begin
  Inc(FDisableArrange);
end;

procedure TTBCustomDockableWindow.EndUpdate;
begin
  Dec(FDisableArrange);
  if FArrangeNeeded and (FDisableArrange = 0) then
    Arrange;
end;

procedure TTBCustomDockableWindow.AddDockForm(const Form: TTBCustomForm);
begin
  if Form = nil then Exit;
  if AddToList(FDockForms, Form) then
    Form.FreeNotification(Self);
end;

procedure TTBCustomDockableWindow.RemoveDockForm(const Form: TTBCustomForm);
begin
  RemoveFromList(FDockForms, Form);
end;

function TTBCustomDockableWindow.IsAutoResized: Boolean;
begin
  Result := AutoResize or Assigned(CurrentDock) or Floating;
end;

procedure TTBCustomDockableWindow.ChangeSize(AWidth, AHeight: Integer);
var
  S: TPoint;
begin
  if Docked then
    CurrentDock.ArrangeToolbars
  else begin
    S := CalcNCSizes;
    Inc(AWidth, S.X);
    Inc(AHeight, S.Y);
    { Leave the width and/or height alone if the control is Anchored
      (or Aligned) }
    if not Floating then begin
      if (akLeft in Anchors) and (akRight in Anchors) then
        AWidth := Width;
      if (akTop in Anchors) and (akBottom in Anchors) then
        AHeight := Height;
    end;
    Inc(FUpdatingBounds);
    try
      SetBounds(Left, Top, AWidth, AHeight);
    finally
      Dec(FUpdatingBounds);
    end;
  end;
end;

procedure TTBCustomDockableWindow.Arrange;
var
  Size: TPoint;
begin
  if (FDisableArrange > 0) or
     { Prevent flicker while loading }
     (csLoading in ComponentState) or
     { Don't call DoArrangeControls when Parent is nil. The VCL sets Parent to
       'nil' during destruction of a component; we can't have an OrderControls
       call after a descendant control has freed its data. }
     (Parent = nil) then begin
    FArrangeNeeded := True;
    Exit;
  end;

  FArrangeNeeded := False;

  Size := DoArrange(True, TBGetDockTypeOf(CurrentDock, Floating), Floating,
    CurrentDock);
  if IsAutoResized then
    ChangeSize(Size.X, Size.Y);
end;

procedure TTBCustomDockableWindow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not(csDesigning in ComponentState) and Floating then begin
    { Force Top & Left to 0 if floating }
    ALeft := 0;
    ATop := 0;
    if Parent is TTBFloatingWindowParent then
      with Parent do
        SetBounds(Left, Top, (Width-ClientWidth) + AWidth,
          (Height-ClientHeight) + AHeight);
  end;
  if (FUpdatingBounds = 0) and ((AWidth <> Width) or (AHeight <> Height)) then
    SizeChanging(AWidth, AHeight);
  { This allows you to drag the toolbar around the dock at design time }
  if (csDesigning in ComponentState) and not(csLoading in ComponentState) and
     Docked and (FUpdatingBounds = 0) and ((ALeft <> Left) or (ATop <> Top)) then begin
    if not(CurrentDock.Position in PositionLeftOrRight) then begin
      FDockRow := CurrentDock.GetDesignModeRowOf(ATop+(Height div 2));
      FDockPos := ALeft;
    end
    else begin
      FDockRow := CurrentDock.GetDesignModeRowOf(ALeft+(Width div 2));
      FDockPos := ATop;
    end;
    inherited SetBounds(Left, Top, AWidth, AHeight);  { only pass any size changes }
    CurrentDock.ArrangeToolbars;  { let ArrangeToolbars take care of position changes }
  end
  else begin
    inherited;
    {if not(csLoading in ComponentState) and Floating and (FUpdatingBounds = 0) then
      FFloatingPosition := BoundsRect.TopLeft;}
  end;
end;

procedure TTBCustomDockableWindow.SetParent(AParent: TWinControl);
  procedure UpdateFloatingToolWindows;
  begin
    if Parent is TTBFloatingWindowParent then begin
      AddToList(FloatingToolWindows, Self);
      Parent.SetBounds(FFloatingPosition.X, FFloatingPosition.Y,
        Parent.Width, Parent.Height);
    end
    else
      RemoveFromList(FloatingToolWindows, Self);
  end;
  function ParentToCurrentDock(const Ctl: TWinControl): TTBDock;
  begin
    if Ctl is TTBDock then
      Result := TTBDock(Ctl)
    else
      Result := nil;
  end;
var
  OldCurrentDock, NewCurrentDock: TTBDock;
  NewFloating: Boolean;
  SaveHandle: HWND;
begin
  OldCurrentDock := ParentToCurrentDock(Parent);
  NewCurrentDock := ParentToCurrentDock(AParent);
  NewFloating := AParent is TTBFloatingWindowParent;

  if AParent = Parent then begin
    { Even though AParent is the same as the current Parent, this code is
      necessary because when the VCL destroys the parent of the tool window,
      it calls TWinControl.Remove to set FParent instead of using SetParent.
      However TControl.Destroy does call SetParent(nil), so it is
      eventually notified of the change before it is destroyed. }
    FCurrentDock := NewCurrentDock;
    FFloating := NewFloating;
    FDocked := Assigned(FCurrentDock);
    UpdateFloatingToolWindows;
  end
  else begin
    if not(csDestroying in ComponentState) and Assigned(AParent) then begin
      if Assigned(FOnDockChanging) then
        FOnDockChanging(Self, NewFloating, NewCurrentDock);
      if Assigned(FOnRecreating) then
        FOnRecreating(Self);
    end;

    { Before changing between docked and floating state (and vice-versa)
      or between docks, increment FHidden and call UpdateVisibility to hide the
      toolbar. This prevents any flashing while it's being moved }
    Inc(FHidden);
    Inc(FDisableOnMove);
    try
      UpdateVisibility;
      if Assigned(OldCurrentDock) then
        OldCurrentDock.BeginUpdate;
      if Assigned(NewCurrentDock) then
        NewCurrentDock.BeginUpdate;
      Inc(FUpdatingBounds);
      try
        if Assigned(AParent) then
          DoDockChangingHidden(NewFloating, NewCurrentDock);
        BeginUpdate;
        try
          { FCurrentSize probably won't be valid after changing Parents, so
            reset it to zero }
          FCurrentSize := 0;

          if Parent is TTBDock then begin
            if not FUseLastDock or (FLastDock <> Parent) then
              TTBDock(Parent).ChangeDockList(False, Self);
            TTBDock(Parent).ToolbarVisibilityChanged(Self, True);
          end;

          { By default, the VCL destroys a control's window handle when it
            changes parents. Prevent that from happening by capturing the
            current handle, detaching the control from its current parent,
            then restoring the handle back. }
          SaveHandle := 0;
          if Assigned(AParent) then begin
            SaveHandle := WindowHandle;
            WindowHandle := 0;
          end;
          inherited SetParent(nil);
          FCurrentDock := NewCurrentDock;
          FFloating := NewFloating;
          FDocked := Assigned(FCurrentDock);
          try
            if SaveHandle <> 0 then begin
              WindowHandle := SaveHandle;
              Windows.SetParent(SaveHandle, AParent.Handle);
              SetWindowPos(SaveHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
                SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
            end;
            inherited;
          except
            { Failure is rare, but just in case, restore these back. }
            FCurrentDock := ParentToCurrentDock(Parent);
            FFloating := Parent is TTBFloatingWindowParent;
            FDocked := Assigned(FCurrentDock);
            raise;
          end;

          { FEffectiveDockRow probably won't be valid on the new Parent, so
            reset it to -1 so that GetMinRowSize will temporarily ignore this
            toolbar }
          FEffectiveDockRow := -1;

          { To conserve resources, free FFloatParent if it's no longer the
            Parent. But don't do this while FSmoothDragging=True, because
            destroying the window the user initially clicked down on causes
            Windows to stop delivering mouse-move messages when the cursor is
            moved over other applications' windows, even if we still have the
            mouse capture. }
          if not FSmoothDragging and
             Assigned(FFloatParent) and (Parent <> FFloatParent) then
            FreeAndNil(FFloatParent);

          if Parent is TTBDock then begin
            if FUseLastDock and not FSmoothDragging then begin
              LastDock := TTBDock(Parent);  { calls ChangeDockList if LastDock changes }
              TTBDock(Parent).ToolbarVisibilityChanged(Self, False);
            end
            else
              TTBDock(Parent).ChangeDockList(True, Self);
          end;

          UpdateFloatingToolWindows;

          { Schedule an arrange }
          Arrange;
        finally
          EndUpdate;
        end;
      finally
        Dec(FUpdatingBounds);
        if Assigned(NewCurrentDock) then
          NewCurrentDock.EndUpdate;
        if Assigned(OldCurrentDock) then
          OldCurrentDock.EndUpdate;
      end;
    finally
      Dec(FDisableOnMove);
      Dec(FHidden);
      UpdateVisibility;
      { ^ The above UpdateVisibility call not only updates the tool window's
        visibility after decrementing FHidden, it also sets the
        active/inactive state of the caption. }
    end;
    if Assigned(Parent) then
      Moved;

    if not(csDestroying in ComponentState) and Assigned(AParent) then begin
      if Assigned(FOnRecreated) then
        FOnRecreated(Self);
      if Assigned(FOnDockChanged) then
        FOnDockChanged(Self);
    end;
  end;
end;

procedure TTBCustomDockableWindow.AddDockedNCAreaToSize(var S: TPoint;
  const LeftRight: Boolean);
var
  TopLeft, BottomRight: TPoint;
begin
  GetDockedNCArea(TopLeft, BottomRight, LeftRight);
  Inc(S.X, TopLeft.X + BottomRight.X);
  Inc(S.Y, TopLeft.Y + BottomRight.Y);
end;

procedure TTBCustomDockableWindow.AddFloatingNCAreaToSize(var S: TPoint);
var
  TopLeft, BottomRight: TPoint;
begin
  GetFloatingNCArea(TopLeft, BottomRight);
  Inc(S.X, TopLeft.X + BottomRight.X);
  Inc(S.Y, TopLeft.Y + BottomRight.Y);
end;

procedure TTBCustomDockableWindow.GetDockedNCArea(var TopLeft, BottomRight: TPoint;
  const LeftRight: Boolean);
var
  Z: Integer;
begin
  Z := DockedBorderSize;  { code optimization... }
  TopLeft.X := Z;
  TopLeft.Y := Z;
  BottomRight.X := Z;
  BottomRight.Y := Z;
  if not LeftRight then begin
    Inc(TopLeft.X, DragHandleSizes[CloseButtonWhenDocked, DragHandleStyle]);
    //if FShowChevron then
    //  Inc(BottomRight.X, tbChevronSize);
  end
  else begin
    Inc(TopLeft.Y, DragHandleSizes[CloseButtonWhenDocked, DragHandleStyle]);
    //if FShowChevron then
    //  Inc(BottomRight.Y, tbChevronSize);
  end;
end;

function TTBCustomDockableWindow.GetFloatingBorderSize: TPoint;
{ Returns size of a thick border. Note that, depending on the Windows version,
  this may not be the same as the actual window metrics since it draws its
  own border }
const
  XMetrics: array[Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array[Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
begin
  Result.X := GetSystemMetrics(XMetrics[Resizable]);
  Result.Y := GetSystemMetrics(YMetrics[Resizable]);
end;

procedure TTBCustomDockableWindow.GetFloatingNCArea(var TopLeft, BottomRight: TPoint);
begin
  with GetFloatingBorderSize do begin
    TopLeft.X := X;
    TopLeft.Y := Y;
    if ShowCaption then
      Inc(TopLeft.Y, GetSmallCaptionHeight);
    BottomRight.X := X;
    BottomRight.Y := Y;
  end;
end;

function TTBCustomDockableWindow.GetDockedCloseButtonRect(LeftRight: Boolean): TRect;
var
  X, Y, Z: Integer;
begin
  Z := DragHandleSizes[CloseButtonWhenDocked, FDragHandleStyle] - 3;
  if not LeftRight then begin
    X := DockedBorderSize+1;
    Y := DockedBorderSize;
  end
  else begin
    X := (ClientWidth + DockedBorderSize) - Z;
    Y := DockedBorderSize+1;
  end;
  Result := Bounds(X, Y, Z, Z);
end;

function TTBCustomDockableWindow.CalcNCSizes: TPoint;
var
  Z: Integer;
begin
  if not Docked then begin
    Result.X := 0;
    Result.Y := 0;
  end
  else begin
    Result.X := DockedBorderSize2;
    Result.Y := DockedBorderSize2;
    if CurrentDock.FAllowDrag then begin
      Z := DragHandleSizes[FCloseButtonWhenDocked, FDragHandleStyle];
      if not(CurrentDock.Position in PositionLeftOrRight) then
        Inc(Result.X, Z)
      else
        Inc(Result.Y, Z);
    end;
  end;
end;

procedure TTBCustomDockableWindow.WMNCCalcSize(var Message: TWMNCCalcSize);

  procedure ApplyToRect(var R: TRect);
  var
    Z: Integer;
  begin
    InflateRect(R, -DockedBorderSize, -DockedBorderSize);
    if CurrentDock.FAllowDrag then begin
      Z := DragHandleSizes[FCloseButtonWhenDocked, FDragHandleStyle];
      if not(CurrentDock.Position in PositionLeftOrRight) then
        Inc(R.Left, Z)
      else
        Inc(R.Top, Z);
    end;
  end;

{$IFDEF CLR}
var
  Params: TNCCalcSizeParams;
{$ENDIF}
begin
  { Doesn't call inherited since it overrides the normal NC sizes }
  Message.Result := 0;
  if Docked then begin
    {$IFNDEF CLR}
    ApplyToRect(Message.CalcSize_Params.rgrc[0]);
    {$ELSE}
    Params := Message.CalcSize_Params;
    ApplyToRect(Params.rgrc0);
    Message.CalcSize_Params := Params;
    {$ENDIF}
  end;
end;

procedure TTBCustomDockableWindow.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  R: TRect;
  I: Integer;
begin
  if Docked and CurrentDock.FAllowDrag and
     (Message.CursorWnd = WindowHandle) and
     (Smallint(Message.HitTest) = HT_TB2k_Border) and
     (DragHandleStyle <> dhNone) then begin
    GetCursorPos(P);
    GetWindowRect(Handle, R);
    if not(CurrentDock.Position in PositionLeftOrRight) then
      I := P.X - R.Left
    else
      I := P.Y - R.Top;
    if I < DockedBorderSize + DragHandleSizes[CloseButtonWhenDocked, DragHandleStyle] then begin
      SetCursor(LoadCursor(0, IDC_SIZEALL));
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TTBCustomDockableWindow.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
{ Redraws all the non-client area of the toolbar when it is docked. }
var
  DC: HDC;
  R: TRect;
  VerticalDock: Boolean;
  X, Y, Y2, Y3, YO, S, SaveIndex: Integer;
  R2, R3, R4: TRect;
  P1, P2: TPoint;
  Brush: HBRUSH;
  Clr: TColorRef;
  UsingBackground, B: Boolean;

  procedure DrawRaisedEdge(R: TRect; const FillInterior: Boolean);
  const
    FillMiddle: array[Boolean] of UINT = (0, BF_MIDDLE);
  begin
    DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT or FillMiddle[FillInterior]);
  end;

  function CreateCloseButtonBitmap: HBITMAP;
  const
    Pattern: array[0..15] of Byte =
      (0, 0, $CC, 0, $78, 0, $30, 0, $78, 0, $CC, 0, 0, 0, 0, 0);
  begin
    Result := CreateMonoBitmap(8, 8, Pattern);
  end;

  procedure DrawButtonBitmap(const Bmp: HBITMAP);
  var
    TempBmp: TBitmap;
  begin
    TempBmp := TBitmap.Create;
    try
      TempBmp.Handle := Bmp;
      SetTextColor(DC, clBlack);
      SetBkColor(DC, clWhite);
      SelectObject(DC, GetSysColorBrush(COLOR_BTNTEXT));
      BitBlt(DC, R2.Left, R2.Top, R2.Right - R2.Left, R2.Bottom - R2.Top,
        TempBmp.Canvas.Handle, 0, 0, $00E20746 {ROP_DSPDxax});
    finally
      TempBmp.Free;
    end;
  end;

const
  CloseButtonState: array[Boolean] of UINT = (0, DFCS_PUSHED);
begin
  if not Docked or not HandleAllocated then Exit;

  if not DrawToDC then
    DC := GetWindowDC(Handle)
  else
    DC := ADC;
  try
    { Use update region }
    if not DrawToDC then
      SelectNCUpdateRgn(Handle, DC, Clip);

    { This works around WM_NCPAINT problem described at top of source code }
    {no!  R := Rect(0, 0, Width, Height);}
    GetWindowRect(Handle, R);  OffsetRect(R, -R.Left, -R.Top);

    VerticalDock := CurrentDock.Position in PositionLeftOrRight;

    Brush := CreateSolidBrush(ColorToRGB(Color));

    UsingBackground := CurrentDock.UsingBackground and CurrentDock.FBkgOnToolbars;

    { Border }
    if BorderStyle = bsSingle then
      DrawRaisedEdge(R, False)
    else
      FrameRect(DC, R, Brush);
    R2 := R;
    InflateRect(R2, -1, -1);
    if not UsingBackground then
      FrameRect(DC, R2, Brush);

    { Draw the Background }
    if UsingBackground then begin
      R2 := R;
      P1 := CurrentDock.ClientToScreen(Point(0, 0));
      P2 := CurrentDock.Parent.ClientToScreen(CurrentDock.BoundsRect.TopLeft);
      Dec(R2.Left, Left + CurrentDock.Left + (P1.X-P2.X));
      Dec(R2.Top, Top + CurrentDock.Top + (P1.Y-P2.Y));
      InflateRect(R, -1, -1);
      GetWindowRect(Handle, R4);
      R3 := ClientRect;
      with ClientToScreen(Point(0, 0)) do
        OffsetRect(R3, X-R4.Left, Y-R4.Top);
      SaveIndex := SaveDC(DC);
      IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      ExcludeClipRect(DC, R3.Left, R3.Top, R3.Right, R3.Bottom);
      CurrentDock.DrawBackground(DC, R2);
      RestoreDC(DC, SaveIndex);
    end;

    { The drag handle at the left, or top }
    if CurrentDock.FAllowDrag then begin
      SaveIndex := SaveDC(DC);
      if not VerticalDock then
        Y2 := ClientHeight
      else
        Y2 := ClientWidth;
      Inc(Y2, DockedBorderSize);
      S := DragHandleSizes[FCloseButtonWhenDocked, FDragHandleStyle];
      if FDragHandleStyle <> dhNone then begin
        Y3 := Y2;
        X := DockedBorderSize + DragHandleXOffsets[FCloseButtonWhenDocked, FDragHandleStyle];
        Y := DockedBorderSize;
        YO := Ord(FDragHandleStyle = dhSingle);
        if FCloseButtonWhenDocked then begin
          if not VerticalDock then
            Inc(Y, S - 2)
          else
            Dec(Y3, S - 2);
        end;
        Clr := GetSysColor(COLOR_BTNHIGHLIGHT);
        for B := False to (FDragHandleStyle = dhDouble) do begin
          if not VerticalDock then
            R2 := Rect(X, Y+YO, X+3, Y2-YO)
          else
            R2 := Rect(Y+YO, X, Y3-YO, X+3);
          DrawRaisedEdge(R2, True);
          if not VerticalDock then
            SetPixelV(DC, X, Y2-1-YO, Clr)
          else
            SetPixelV(DC, Y3-1-YO, X, Clr);
          ExcludeClipRect(DC, R2.Left, R2.Top, R2.Right, R2.Bottom);
          Inc(X, 3);
        end;
      end;
      if not UsingBackground then begin
        if not VerticalDock then
          R2 := Rect(DockedBorderSize, DockedBorderSize,
            DockedBorderSize+S, Y2)
        else
          R2 := Rect(DockedBorderSize, DockedBorderSize,
            Y2, DockedBorderSize+S);
        FillRect(DC, R2, Brush);
      end;
      RestoreDC(DC, SaveIndex);
      { Close button }
      if FCloseButtonWhenDocked then begin
        R2 := GetDockedCloseButtonRect(VerticalDock);
        if FCloseButtonDown then
          DrawEdge(DC, R2, BDR_SUNKENOUTER, BF_RECT)
        else if FCloseButtonHover then
          DrawRaisedEdge(R2, False);
        InflateRect(R2, -2, -2);
        if FCloseButtonDown then
          OffsetRect(R2, 1, 1);
        DrawButtonBitmap(CreateCloseButtonBitmap);
      end;
    end;

    DeleteObject(Brush);
  finally
    if not DrawToDC then
      ReleaseDC(Handle, DC);
  end;
end;

procedure TTBCustomDockableWindow.RedrawNCArea;
begin
  { Note: IsWindowVisible is called as an optimization. There's no need to
    draw on invisible windows. }
  if HandleAllocated and IsWindowVisible(Handle) then
    DrawNCArea(False, 0, 0);
end;

procedure TTBCustomDockableWindow.WMNCPaint(var Message: TMessage);
begin
  { Don't call inherited because it overrides the default NC painting }
  DrawNCArea(False, 0, HRGN(Message.WParam));
end;

procedure DockableWindowNCPaintProc(Wnd: HWND; DC: HDC; AppData: TObject);
begin
  with TTBCustomDockableWindow(AppData) do
    DrawNCArea(True, DC, 0)
end;

procedure TTBCustomDockableWindow.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Handle, Message, DockableWindowNCPaintProc, Self);
end;

procedure TTBCustomDockableWindow.WMPrintClient(var Message:
  {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF});
begin
  HandleWMPrintClient(PaintHandler, Message);
end;

procedure TTBCustomDockableWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R, R2, R3: TRect;
  P1, P2: TPoint;
  SaveIndex: Integer;
begin
  if Docked and CurrentDock.UsingBackground and CurrentDock.FBkgOnToolbars then begin
    R := ClientRect;
    R2 := R;
    P1 := CurrentDock.ClientToScreen(Point(0, 0));
    P2 := CurrentDock.Parent.ClientToScreen(CurrentDock.BoundsRect.TopLeft);
    Dec(R2.Left, Left + CurrentDock.Left + (P1.X-P2.X));
    Dec(R2.Top, Top + CurrentDock.Top + (P1.Y-P2.Y));
    GetWindowRect(Handle, R3);
    with ClientToScreen(Point(0, 0)) do begin
      Inc(R2.Left, R3.Left-X);
      Inc(R2.Top, R3.Top-Y);
    end;
    SaveIndex := SaveDC(Message.DC);
    IntersectClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);
    CurrentDock.DrawBackground(Message.DC, R2);
    RestoreDC(Message.DC, SaveIndex);
    Message.Result := 1;
  end
  else
    inherited;
end;

function TTBCustomDockableWindow.GetPalette: HPALETTE;
begin
  if Docked then
    Result := CurrentDock.GetPalette
  else
    Result := 0;
end;

function TTBCustomDockableWindow.PaletteChanged(Foreground: Boolean): Boolean;
begin
  Result := inherited PaletteChanged(Foreground);
  if Result and not Foreground then begin
    { There seems to be a bug in Delphi's palette handling. When the form is
      inactive and another window realizes a palette, docked TToolbar97s
      weren't getting redrawn. So this workaround code was added. }
    InvalidateAll(Self);
  end;
end;

procedure TTBCustomDockableWindow.DrawDraggingOutline(const DC: HDC;
  const NewRect, OldRect: TRect; const NewDocking, OldDocking: Boolean);
var
  NewSize, OldSize: TSize;
begin
  with GetFloatingBorderSize do begin
    if NewDocking then NewSize.cx := 1 else NewSize.cx := X;
    NewSize.cy := NewSize.cx;
    if OldDocking then OldSize.cx := 1 else OldSize.cx := X;
    OldSize.cy := OldSize.cx;
  end;
  DrawHalftoneInvertRect(DC, NewRect, OldRect, NewSize, OldSize);
end;

procedure TTBCustomDockableWindow.CMColorChanged(var Message: TMessage);
begin
  { Make sure non-client area is redrawn }
  InvalidateAll(Self);
  inherited;  { the inherited handler calls Invalidate }
end;

procedure TTBCustomDockableWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Parent is TTBFloatingWindowParent then
    TTBFloatingWindowParent(Parent).Caption := Caption;
end;

procedure TTBCustomDockableWindow.CMVisibleChanged(var Message: TMessage);
begin
  if not(csDesigning in ComponentState) and Docked then
    CurrentDock.ToolbarVisibilityChanged(Self, False);
  inherited;
  if Assigned(FOnVisibleChanged) then
    FOnVisibleChanged(Self);
end;

type
  TRowSize = record
    Size: Integer;
    FullSizeRow: Boolean;
  end;
  TDockedSize = class
    Dock: TTBDock;
    BoundsRect: TRect;
    Size: TPoint;
    RowSizes: array of TRowSize;
  end;

procedure TTBCustomDockableWindow.BeginMoving(const InitX, InitY: Integer);
const
  SplitCursors: array[Boolean] of {$IFNDEF CLR} PChar {$ELSE} Integer {$ENDIF} =
    (IDC_SIZEWE, IDC_SIZENS);
var
  UseSmoothDrag: Boolean;
  DockList: TList;
  NewDockedSizes: TList;
  OriginalDock, MouseOverDock: TTBDock;
  MoveRect: TRect;
  StartDocking, PreventDocking, PreventFloating, WatchForSplit, SplitVertical: Boolean;
  ScreenDC: HDC;
  OldCursor: HCURSOR;
  NPoint, DPoint: TPoint;
  OriginalDockRow, OriginalDockPos: Integer;
  FirstPos, LastPos, CurPos: TPoint;

  function FindDockedSize(const ADock: TTBDock): TDockedSize;
  var
    I: Integer;
  begin
    for I := 0 to NewDockedSizes.Count-1 do begin
      Result := TDockedSize(NewDockedSizes[I]);
      if Result.Dock = ADock then
        Exit;
    end;
    Result := nil;
  end;

  function GetRowOf(const RowSizes: array of TRowSize; const XY: Integer;
    var Before: Boolean): Integer;
  { Returns row number of the specified coordinate. Before is set to True if it
    was in the top (or left) quarter of the row. }
  var
    HighestRow, R, CurY, NextY, CurRowSize, EdgeSize: Integer;
    FullSizeRow: Boolean;
  begin
    Before := False;
    HighestRow := High(RowSizes);
    CurY := 0;
    for R := 0 to HighestRow do begin
      CurRowSize := RowSizes[R].Size;
      FullSizeRow := FullSize or RowSizes[R].FullSizeRow;
      if CurRowSize = 0 then
        Continue;
      NextY := CurY + CurRowSize;
      if not FullSizeRow then
        EdgeSize := CurRowSize div 4
      else
        EdgeSize := CurRowSize div 2;
      if XY < CurY + EdgeSize then begin
        Result := R;
        Before := True;
        Exit;
      end;
      if not FullSizeRow and (XY < NextY - EdgeSize) then begin
        Result := R;
        Exit;
      end;
      CurY := NextY;
    end;
    Result := HighestRow+1;
  end;

  procedure Dropped;
  var
    NewDockRow: Integer;
    Before: Boolean;
    MoveRectClient: TRect;
    C: Integer;
    DockedSize: TDockedSize;
  begin
    if MouseOverDock <> nil then begin
      DockedSize := FindDockedSize(MouseOverDock);
      MoveRectClient := MoveRect;
      OffsetRect(MoveRectClient, -DockedSize.BoundsRect.Left,
        -DockedSize.BoundsRect.Top);
      if not FDragSplitting then begin
        if not(MouseOverDock.Position in PositionLeftOrRight) then
          C := (MoveRectClient.Top+MoveRectClient.Bottom) div 2
        else
          C := (MoveRectClient.Left+MoveRectClient.Right) div 2;
        NewDockRow := GetRowOf(DockedSize.RowSizes, C, Before);
        if Before then
          WatchForSplit := False;
      end
      else begin
        NewDockRow := FDockRow;
        Before := False;
      end;
      if WatchForSplit then begin
        if (MouseOverDock <> OriginalDock) or (NewDockRow <> OriginalDockRow) then
          WatchForSplit := False
        else begin
          if not SplitVertical then
            C := FirstPos.X - LastPos.X
          else
            C := FirstPos.Y - LastPos.Y;
          if Abs(C) >= 10 then begin
            WatchForSplit := False;
            FDragSplitting := True;
            SetCursor(LoadCursor(0, SplitCursors[SplitVertical]));
          end;
        end;
      end;
      FDockRow := NewDockRow;
      if not(MouseOverDock.Position in PositionLeftOrRight) then
        FDockPos := MoveRectClient.Left
      else
        FDockPos := MoveRectClient.Top;
      Parent := MouseOverDock;
      if not FSmoothDragging then
        CurrentDock.CommitNewPositions := True;
      FInsertRowBefore := Before;
      try
        CurrentDock.ArrangeToolbars;
      finally
        FInsertRowBefore := False;
      end;
    end
    else begin
      WatchForSplit := False;
      FloatingPosition := MoveRect.TopLeft;
      Floating := True;
      { Make sure it doesn't go completely off the screen }
      MoveOnScreen(True);
    end;

    { Make sure it's repainted immediately (looks better on really slow
      computers when smooth dragging is enabled) }
    Update;
  end;

  procedure MouseMoved;
  var
    OldMouseOverDock: TTBDock;
    OldMoveRect: TRect;
    Pos: TPoint;

    function GetDockRect(Control: TTBDock): TRect;
    var
      I: Integer;
    begin
      for I := 0 to NewDockedSizes.Count-1 do
        with TDockedSize(NewDockedSizes[I]) do begin
          if Dock <> Control then Continue;
          Result := Bounds(Pos.X-MulDiv(Size.X-1, NPoint.X, DPoint.X),
            Pos.Y-MulDiv(Size.Y-1, NPoint.Y, DPoint.Y),
            Size.X, Size.Y);
          Exit;
        end;
      SetRectEmpty(Result);
    end;

    function CheckIfCanDockTo(Control: TTBDock; R: TRect): Boolean;
    const
      DockSensX = 25;
      DockSensY = 25;
    var
      S, Temp: TRect;
      Sens: Integer;
    begin
      with Control do begin
        Result := False;

        InflateRect(R, 3, 3);
        S := GetDockRect(Control);

        { Like Office, distribute ~25 pixels of extra dock detection area
          to the left side if the toolbar was grabbed at the left, both sides
          if the toolbar was grabbed at the middle, or the right side if
          toolbar was grabbed at the right. If outside, don't try to dock. }
        Sens := MulDiv(DockSensX, NPoint.X, DPoint.X);
        if (Pos.X < R.Left-(DockSensX-Sens)) or (Pos.X >= R.Right+Sens) then
          Exit;

        { Don't try to dock to the left or right if pointer is above or below
          the boundaries of the dock }
        if (Control.Position in PositionLeftOrRight) and
           ((Pos.Y < R.Top) or (Pos.Y >= R.Bottom)) then
          Exit;

        { And also distribute ~25 pixels of extra dock detection area to
          the top or bottom side }
        Sens := MulDiv(DockSensY, NPoint.Y, DPoint.Y);
        if (Pos.Y < R.Top-(DockSensY-Sens)) or (Pos.Y >= R.Bottom+Sens) then
          Exit;

        Result := IntersectRect(Temp, R, S);
      end;
    end;

  var
    R, R2: TRect;
    I: Integer;
    Dock: TTBDock;
    Accept: Boolean;
    TL, BR: TPoint;
  begin
    OldMouseOverDock := MouseOverDock;
    OldMoveRect := MoveRect;

    GetCursorPos(Pos);

    if FDragSplitting then
      MouseOverDock := CurrentDock
    else begin
      { Check if it can dock }
      MouseOverDock := nil;
      if StartDocking and not PreventDocking then
        for I := 0 to DockList.Count-1 do begin
          Dock := TTBDock(DockList[I]);
          if CheckIfCanDockTo(Dock, FindDockedSize(Dock).BoundsRect) then begin
            MouseOverDock := Dock;
            Accept := True;
            if Assigned(MouseOverDock.FOnRequestDock) then
              MouseOverDock.FOnRequestDock(MouseOverDock, Self, Accept);
            if Accept then
              Break
            else
              MouseOverDock := nil;
          end;
        end;
    end;

    { If not docking, clip the point so it doesn't get dragged under the
      taskbar }
    if MouseOverDock = nil then begin
      R := GetRectOfMonitorContainingPoint(Pos, True);
      if Pos.X < R.Left then Pos.X := R.Left;
      if Pos.X > R.Right then Pos.X := R.Right;
      if Pos.Y < R.Top then Pos.Y := R.Top;
      if Pos.Y > R.Bottom then Pos.Y := R.Bottom;
    end;

    MoveRect := GetDockRect(MouseOverDock);

    { Make sure title bar (or at least part of the toolbar) is still accessible
      if it's dragged almost completely off the screen. This prevents the
      problem seen in Office 97 where you drag it offscreen so that only the
      border is visible, sometimes leaving you no way to move it back short of
      resetting the toolbar. }
    if MouseOverDock = nil then begin
      R2 := GetRectOfMonitorContainingPoint(Pos, True);
      R := R2;
      with GetFloatingBorderSize do
        InflateRect(R, -(X+4), -(Y+4));
      if MoveRect.Bottom < R.Top then
        OffsetRect(MoveRect, 0, R.Top-MoveRect.Bottom);
      if MoveRect.Top > R.Bottom then
        OffsetRect(MoveRect, 0, R.Bottom-MoveRect.Top);
      if MoveRect.Right < R.Left then
        OffsetRect(MoveRect, R.Left-MoveRect.Right, 0);
      if MoveRect.Left > R.Right then
        OffsetRect(MoveRect, R.Right-MoveRect.Left, 0);

      GetFloatingNCArea(TL, BR);
      I := R2.Top + 4 - TL.Y;
      if MoveRect.Top < I then
        OffsetRect(MoveRect, 0, I-MoveRect.Top);
    end;

    { Empty MoveRect if it's wanting to float but it's not allowed to, and
      set the mouse cursor accordingly. }
    if PreventFloating and not Assigned(MouseOverDock) then begin
      SetRectEmpty(MoveRect);
      SetCursor(LoadCursor(0, IDC_NO));
    end
    else begin
      if FDragSplitting then
        SetCursor(LoadCursor(0, SplitCursors[SplitVertical]))
      else
        SetCursor(OldCursor);
    end;

    { Update the dragging outline }
    if not UseSmoothDrag then
      DrawDraggingOutline(ScreenDC, MoveRect, OldMoveRect, MouseOverDock <> nil,
        OldMouseOverDock <> nil)
    else
      if not IsRectEmpty(MoveRect) then
        Dropped;
  end;

  procedure BuildDockList;

    function AcceptableDock(const D: TTBDock): Boolean;
    begin
      Result := D.FAllowDrag and (D.Position in DockableTo);
    end;

    procedure Recurse(const ParentCtl: TWinControl);
    var
      D: TTBDockPosition;
      I: Integer;
    begin
      if ContainsControl(ParentCtl) or not ParentCtl.HandleAllocated or
         not IsWindowVisible(ParentCtl.Handle) then
        Exit;
      with ParentCtl do begin
        for D := Low(D) to High(D) do
          for I := 0 to ParentCtl.ControlCount-1 do
            if (Controls[I] is TTBDock) and (TTBDock(Controls[I]).Position = D) then
              Recurse(TWinControl(Controls[I]));
        for I := 0 to ParentCtl.ControlCount-1 do
          if (Controls[I] is TWinControl) and not(Controls[I] is TTBDock) then
            Recurse(TWinControl(Controls[I]));
      end;
      if (ParentCtl is TTBDock) and AcceptableDock(TTBDock(ParentCtl)) and
         (DockList.IndexOf(ParentCtl) = -1) then
        DockList.Add(ParentCtl);
    end;

  var
    ParentForm: TTBCustomForm;
    DockFormsList: TList;
    I, J: Integer;
  begin
    { Manually add CurrentDock to the DockList first so that it gets priority
      over other docks }
    if Assigned(CurrentDock) and AcceptableDock(CurrentDock) then
      DockList.Add(CurrentDock);
    ParentForm := TBGetToolWindowParentForm(Self);
    DockFormsList := TList.Create;
    try
      if Assigned(FDockForms) then begin
        for I := 0 to Screen.{$IFDEF JR_D3}CustomFormCount{$ELSE}FormCount{$ENDIF}-1 do begin
          J := FDockForms.IndexOf(Screen.{$IFDEF JR_D3}CustomForms{$ELSE}Forms{$ENDIF}[I]);
          if (J <> -1) and (FDockForms[J] <> ParentForm) then
            DockFormsList.Add(FDockForms[J]);
        end;
      end;
      if Assigned(ParentForm) then
        DockFormsList.Insert(0, ParentForm);
      for I := 0 to DockFormsList.Count-1 do
        Recurse(TWinControl(DockFormsList[I]));
    finally
      DockFormsList.Free;
    end;
  end;

var
  Accept: Boolean;
  R: TRect;
  Msg: TMsg;
  NewDockedSize: TDockedSize;
  I, J: Integer;
begin
  Accept := False;
  SplitVertical := False;
  WatchForSplit := False;
  OriginalDock := CurrentDock;
  OriginalDockRow := FDockRow;
  OriginalDockPos := FDockPos;
  try
    FDragMode := True;
    FDragSplitting := False;
    if Docked then begin
      FDragCanSplit := False;
      CurrentDock.CommitNewPositions := True;
      CurrentDock.ArrangeToolbars;  { needed for WatchForSplit assignment below }
      SplitVertical := CurrentDock.Position in PositionLeftOrRight;
      WatchForSplit := FDragCanSplit;
    end;
    DockList := nil;
    NewDockedSizes := nil;
    try
      UseSmoothDrag := FSmoothDrag;
      FSmoothDragging := UseSmoothDrag;

      NPoint := Point(InitX, InitY);
      { Adjust for non-client area }
      if not(Parent is TTBFloatingWindowParent) then begin
        GetWindowRect(Handle, R);
        R.BottomRight := ClientToScreen(Point(0, 0));
        DPoint := Point(Width-1, Height-1);
      end
      else begin
        GetWindowRect(Parent.Handle, R);
        R.BottomRight := Parent.ClientToScreen(Point(0, 0));
        DPoint := Point(Parent.Width-1, Parent.Height-1);
      end;
      Dec(NPoint.X, R.Left-R.Right);
      Dec(NPoint.Y, R.Top-R.Bottom);

      PreventDocking := GetKeyState(VK_CONTROL) < 0;
      PreventFloating := DockMode <> dmCanFloat;

      { Build list of all TTBDock's on the form }
      DockList := TList.Create;
      if DockMode <> dmCannotFloatOrChangeDocks then
        BuildDockList
      else
        if Docked then
          DockList.Add(CurrentDock);

      { Ensure positions of each possible dock are committed }
      for I := 0 to DockList.Count-1 do
        TTBDock(DockList[I]).CommitPositions;

      { Set up potential sizes for each dock type }
      NewDockedSizes := TList.Create;
      for I := -1 to DockList.Count-1 do begin
        NewDockedSizes.Expand;
        NewDockedSize := TDockedSize.Create;
        try
          with NewDockedSize do begin
            if I = -1 then begin
              { -1 adds the floating size }
              Dock := nil;
              SetRectEmpty(BoundsRect);
              Size := DoArrange(False, TBGetDockTypeOf(CurrentDock, Floating), True, nil);
              AddFloatingNCAreaToSize(Size);
            end
            else begin
              Dock := TTBDock(DockList[I]);
              BoundsRect := Dock.ClientRect;
              MapWindowPoints(Dock.Handle, 0, BoundsRect, 2);
              if Dock <> CurrentDock then begin
                Size := DoArrange(False, TBGetDockTypeOf(CurrentDock, Floating), False, Dock);
                AddDockedNCAreaToSize(Size, Dock.Position in PositionLeftOrRight);
              end
              else
                Size := Point(Width, Height);
            end;
          end;
          if Assigned(NewDockedSize.Dock) then begin
            SetLength(NewDockedSize.RowSizes, NewDockedSize.Dock.GetHighestRow(True) + 1);
            for J := 0 to High(NewDockedSize.RowSizes) do begin
              NewDockedSize.RowSizes[J].Size := NewDockedSize.Dock.GetCurrentRowSize(J,
                NewDockedSize.RowSizes[J].FullSizeRow);
            end;
          end;
        except
          NewDockedSize.Free;
          raise;
        end;
        NewDockedSizes.Add(NewDockedSize);
      end;

      { Before locking, make sure all pending paint messages are processed }
      ProcessPaintMessages;

      { Save the original mouse cursor }
      OldCursor := GetCursor;

      SetRectEmpty(MoveRect);
      if not UseSmoothDrag then begin
        { This uses LockWindowUpdate to suppress all window updating so the
          dragging outlines doesn't sometimes get garbled. (This is safe, and in
          fact, is the main purpose of the LockWindowUpdate function)
          IMPORTANT! While debugging you might want to enable the 'TB2Dock_DisableLock'
          conditional define (see top of the source code). }
        {$IFNDEF TB2Dock_DisableLock}
        LockWindowUpdate(GetDesktopWindow);
        {$ENDIF}
        { Get a DC of the entire screen. Works around the window update lock
          by specifying DCX_LOCKWINDOWUPDATE. }
        ScreenDC := GetDCEx(GetDesktopWindow, 0,
          DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW);
      end
      else
        ScreenDC := 0;
      try
        SetCapture(Handle);

        { Initialize }
        StartDocking := Docked;
        MouseOverDock := nil;
        GetCursorPos(FirstPos);
        LastPos := FirstPos;
        MouseMoved;
        StartDocking := True;

        { Stay in message loop until capture is lost. Capture is removed either
          by this procedure manually doing it, or by an outside influence (like
          a message box or menu popping up) }
        while GetCapture = Handle do begin
          case Integer(GetMessage(Msg, 0, 0, 0)) of
            -1: Break; { if GetMessage failed }
            0: begin
                 { Repost WM_QUIT messages }
                 PostQuitMessage(ClipToLongint(Msg.wParam));
                 Break;
               end;
          end;

          case Msg.Message of
            WM_KEYDOWN, WM_KEYUP:
              { Ignore all keystrokes while dragging. But process Ctrl and Escape }
              case Word(Msg.wParam) of
                VK_CONTROL:
                  if PreventDocking <> (Msg.Message = WM_KEYDOWN) then begin
                    PreventDocking := Msg.Message = WM_KEYDOWN;
                    MouseMoved;
                  end;
                VK_ESCAPE:
                  Break;
              end;
            WM_MOUSEMOVE: begin
                { Note to self: WM_MOUSEMOVE messages should never be dispatched
                  here to ensure no hints get shown during the drag process }
                CurPos := GetMessagePosAsPoint;
                if (LastPos.X <> CurPos.X) or (LastPos.Y <> CurPos.Y) then begin
                  MouseMoved;
                  LastPos := CurPos;
                end;
              end;
            WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
              { Make sure it doesn't begin another loop }
              Break;
            WM_LBUTTONUP: begin
                Accept := True;
                Break;
              end;
            WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:
              { Ignore all other mouse up/down messages }
              ;
          else
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        end;
      finally
        { Since it sometimes breaks out of the loop without capture being
          released }
        if GetCapture = Handle then
          ReleaseCapture;

        if not UseSmoothDrag then begin
          { Hide dragging outline. Since NT will release a window update lock if
            another thread comes to the foreground, it has to release the DC
            and get a new one for erasing the dragging outline. Otherwise,
            the DrawDraggingOutline appears to have no effect when this happens. }
          ReleaseDC(GetDesktopWindow, ScreenDC);
          ScreenDC := GetDCEx(GetDesktopWindow, 0,
            DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW);
          SetRectEmpty(R);
          DrawDraggingOutline(ScreenDC, R, MoveRect, True, MouseOverDock <> nil);
          ReleaseDC(GetDesktopWindow, ScreenDC);

          { Release window update lock }
          {$IFNDEF TB2Dock_DisableLock}
          LockWindowUpdate(0);
          {$ENDIF}
        end;
      end;

      { Move to new position only if MoveRect isn't empty }
      FSmoothDragging := False;
      if Accept and not IsRectEmpty(MoveRect) then
        { Note: Dropped must be called again after FSmoothDragging is reset to
          False so that TTBDock.ArrangeToolbars makes the DockPos changes
          permanent }
        Dropped;

      { LastDock isn't automatically updated while FSmoothDragging=True, so
        update it now that it's back to False }
      if FUseLastDock and Assigned(CurrentDock) then
        LastDock := CurrentDock;

      { To conserve resources, free FFloatParent if it's no longer the Parent.
        (SetParent doesn't do this automatically when FSmoothDragging=True.) }
      if Assigned(FFloatParent) and (Parent <> FFloatParent) then
        FreeAndNil(FFloatParent);
    finally
      FSmoothDragging := False;
      if not Docked then begin
        { If we didn't end up docking, restore the original DockRow & DockPos
          values }
        FDockRow := OriginalDockRow;
        FDockPos := OriginalDockPos;
      end;
      if Assigned(NewDockedSizes) then begin
        for I := NewDockedSizes.Count-1 downto 0 do
          TDockedSize(NewDockedSizes[I]).Free;
        NewDockedSizes.Free;
      end;
      DockList.Free;
    end;
  finally
    FDragMode := False;
    FDragSplitting := False;
  end;
end;

function TTBCustomDockableWindow.ChildControlTransparent(Ctl: TControl): Boolean;
begin
  Result := False;
end;

procedure TTBCustomDockableWindow.ControlExistsAtPos(const P: TPoint;
  var ControlExists: Boolean);
var
  I: Integer;
begin
  for I := 0 to ControlCount-1 do
    if not ChildControlTransparent(Controls[I]) and Controls[I].Visible and
       PtInRect(Controls[I].BoundsRect, P) then begin
      ControlExists := True;
      Break;
    end;
end;

procedure TTBCustomDockableWindow.DoubleClick;
begin
  if Docked then begin
    if DockMode = dmCanFloat then begin
      Floating := True;
      MoveOnScreen(True);
    end;
  end
  else
  if Assigned(LastDock) then
    Parent := LastDock
  else
  if Assigned(DefaultDock) then begin
    FDockRow := ForceDockAtTopRow;
    FDockPos := ForceDockAtLeftPos;
    Parent := DefaultDock;
  end;
end;

function TTBCustomDockableWindow.IsMovable: Boolean;
begin
  Result := (Docked and CurrentDock.FAllowDrag) or Floating;
end;

procedure TTBCustomDockableWindow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  CtlExists: Boolean;
begin
  inherited;
  if (Button <> mbLeft) or not IsMovable then
    Exit;
  { Ignore message if user clicked on a child control }
  P := Point(X, Y);
  if PtInRect(ClientRect, P) then begin
    CtlExists := False;
    ControlExistsAtPos(P, CtlExists);
    if CtlExists then
      Exit;
  end;

  if not(ssDouble in Shift) then begin
    BeginMoving(X, Y);
    MouseUp(mbLeft, [], -1, -1);
  end
  else
    { Handle double click }
    DoubleClick;
end;

procedure TTBCustomDockableWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;
  if Docked then
    with Message do begin
      P := SmallPointToPoint(Pos);
      GetWindowRect(Handle, R);
      Dec(P.X, R.Left);  Dec(P.Y, R.Top);
      if Result <> HTCLIENT then begin
        Result := HTNOWHERE;
        if FCloseButtonWhenDocked and CurrentDock.FAllowDrag and
           PtInRect(GetDockedCloseButtonRect(
             TBGetDockTypeOf(CurrentDock, Floating) = dtLeftRight), P) then
          Result := HT_TB2k_Close
        else
          Result := HT_TB2k_Border;
      end;
    end;
end;

procedure TTBCustomDockableWindow.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  InArea: Boolean;
begin
  inherited;
  { Note: TME_NONCLIENT was introduced in Windows 98 and 2000 }
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});
  InArea := (ClipToLongint(Message.HitTest) = HT_TB2k_Close);
  if FCloseButtonHover <> InArea then begin
    FCloseButtonHover := InArea;
    RedrawNCArea;
  end;
end;

procedure TTBCustomDockableWindow.WMNCMouseLeave(var Message: TMessage);
begin
  if not MouseCapture then
    CancelNCHover;
  inherited;
end;

procedure TTBCustomDockableWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  { On Windows versions that can't send a WM_NCMOUSELEAVE message, trap
    CM_MOUSELEAVE to detect when the mouse moves from the non-client area to
    another control. }
  CancelNCHover;
end;

procedure TTBCustomDockableWindow.WMMouseMove(var Message: TWMMouseMove);
begin
  { On Windows versions that can't send a WM_NCMOUSELEAVE message, trap
    WM_MOUSEMOVE to detect when the mouse moves from the non-client area to
    the client area.
    Note: We are overriding WM_MOUSEMOVE instead of MouseMove so that our
    processing always gets done first. }
  CancelNCHover;
  inherited;
end;

procedure TTBCustomDockableWindow.CancelNCHover;
begin
  if FCloseButtonHover then begin
    FCloseButtonHover := False;
    RedrawNCArea;
  end;
end;

procedure TTBCustomDockableWindow.Close;
var
  Accept: Boolean;
begin
  Accept := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, Accept);
  { Did the CloseQuery event return True? }
  if Accept then begin
    Hide;
    if Assigned(FOnClose) then
      FOnClose(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetCloseButtonState(Pushed: Boolean);
begin
  if FCloseButtonDown <> Pushed then begin
    FCloseButtonDown := Pushed;
    RedrawNCArea;
  end;
end;

procedure TTBCustomDockableWindow.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  R, BR: TRect;
  P: TPoint;
begin
  case ClipToLongint(Message.HitTest) of
    HT_TB2k_Close: begin
        GetWindowRect(Handle, R);
        BR := GetDockedCloseButtonRect(
          TBGetDockTypeOf(CurrentDock, Floating) = dtLeftRight);
        OffsetRect(BR, R.Left, R.Top);
        if CloseButtonLoop(Handle, BR, SetCloseButtonState) then
          Close;
      end;
    HT_TB2k_Border: begin
        P := ScreenToClient(GetMessagePosAsPoint);
        if IsMovable then
          BeginMoving(P.X, P.Y);
      end;
  else
    inherited;
  end;
end;

procedure TTBCustomDockableWindow.WMNCLButtonDblClk(var Message: TWMNCLButtonDblClk);
begin
  if ClipToLongint(Message.HitTest) = HT_TB2k_Border then begin
    if IsMovable then
      DoubleClick;
  end
  else
    inherited;
end;

procedure TTBCustomDockableWindow.ShowNCContextMenu(const PosX, PosY: Smallint);

  {$IFNDEF JR_D5}
  { Note: this is identical to TControl.CheckMenuPopup (from Delphi 4),
    except where noted.
    TControl.CheckMenuPopup is unfortunately 'private', so it can't be called
    outside of the Controls unit. }
  procedure CheckMenuPopup;
  var
    Control: TControl;
    PopupMenu: TPopupMenu;
  begin
    if csDesigning in ComponentState then Exit;
    Control := Self;
    while Control <> nil do
    begin
      { Added TControlAccess cast because GetPopupMenu is 'protected' }
      PopupMenu := TControlAccess(Control).GetPopupMenu;
      if (PopupMenu <> nil) then
      begin
        if not PopupMenu.AutoPopup then Exit;
        SendCancelMode(nil);
        PopupMenu.PopupComponent := Control;
        { Changed the following. LPARAM of WM_NCRBUTTONUP is in screen
          coordinates, not client coordinates }
        {with ClientToScreen(SmallPointToPoint(Pos)) do
          PopupMenu.Popup(X, Y);}
        PopupMenu.Popup(PosX, PosY);
        Exit;
      end;
      Control := Control.Parent;
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF JR_D5}
  { Delphi 5 and later use the WM_CONTEXTMENU message for popup menus }
  SendMessage(Handle, WM_CONTEXTMENU, WPARAM(Handle), MAKELPARAM(Word(PosX), Word(PosY)));
  {$ELSE}
  CheckMenuPopup;
  {$ENDIF}
end;

procedure TTBCustomDockableWindow.WMNCRButtonUp(var Message: TWMNCRButtonUp);
begin
  ShowNCContextMenu(Message.XCursor, Message.YCursor);
end;

{$IFDEF JR_D5}
procedure TTBCustomDockableWindow.WMContextMenu(var Message: TWMContextMenu);
{ Unfortunately TControl.WMContextMenu ignores clicks in the non-client area.
  On docked toolbars, we need right clicks on the border, part of the
  non-client area, to display the popup menu. The only way I see to have it do
  that is to create a new version of WMContextMenu specifically for the
  non-client area, and that is what this method is.
  Note: This is identical to Delphi 2006's TControl.WMContextMenu, except where
  noted. }
var
  Pt, Temp: TPoint;
  Handled: Boolean;
  PopupMenu: TPopupMenu;
begin
  { Added 'inherited;' here }
  inherited;
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  Pt := SmallPointToPoint(Message.Pos);
  if InvalidPoint(Pt) then
    Temp := Pt
  else
  begin
    Temp := ScreenToClient(Pt);
    { Changed the following. We're only interested in the non-client area }
    {if not PtInRect(ClientRect, Temp) then}
    if PtInRect(ClientRect, Temp) then
    begin
      {inherited;}
      Exit;
    end;
  end;

  Handled := False;
  DoContextPopup(Temp, Handled);
  Message.Result := Ord(Handled);
  if Handled then Exit;

  PopupMenu := GetPopupMenu;
  if (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    SendCancelMode(Self);
    PopupMenu.PopupComponent := Self;
    if InvalidPoint(Pt) then
      Pt := ClientToScreen(Point(0, 0));
    PopupMenu.Popup(Pt.X, Pt.Y);
    Message.Result := 1;
  end;

  if Message.Result = 0 then
    inherited;
end;
{$ENDIF}

procedure TTBCustomDockableWindow.GetMinShrinkSize(var AMinimumSize: Integer);
begin
end;

function TTBCustomDockableWindow.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TTBFloatingWindowParent;
end;

procedure TTBCustomDockableWindow.GetMinMaxSize(var AMinClientWidth,
  AMinClientHeight, AMaxClientWidth, AMaxClientHeight: Integer);
begin
end;

function TTBCustomDockableWindow.GetShrinkMode: TTBShrinkMode;
begin
  Result := tbsmNone;
end;

procedure TTBCustomDockableWindow.ResizeBegin;
begin
end;

procedure TTBCustomDockableWindow.ResizeTrack(var Rect: TRect; const OrigRect: TRect);
begin
end;

procedure TTBCustomDockableWindow.ResizeTrackAccept;
begin
end;

procedure TTBCustomDockableWindow.ResizeEnd;
begin
end;

procedure TTBCustomDockableWindow.BeginSizing(const ASizeHandle: TTBSizeHandle);
var
  UseSmoothDrag, DragX, DragY, ReverseX, ReverseY: Boolean;
  MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
  DragRect, OrigDragRect: TRect;
  ScreenDC: HDC;
  OrigPos, OldPos: TPoint;

  procedure DoResize;
  begin
    BeginUpdate;
    try
      ResizeTrackAccept;
      Parent.BoundsRect := DragRect;
      SetBounds(Left, Top, Parent.ClientWidth, Parent.ClientHeight);
    finally
      EndUpdate;
    end;

    { Make sure it doesn't go completely off the screen }
    MoveOnScreen(True);
  end;

  procedure MouseMoved;
  var
    Pos: TPoint;
    OldDragRect: TRect;
  begin
    GetCursorPos(Pos);
    { It needs to check if the cursor actually moved since last time. This is
      because a call to LockWindowUpdate (apparently) generates a mouse move
      message even when mouse hasn't moved. }
    if (Pos.X = OldPos.X) and (Pos.Y = OldPos.Y) then Exit;
    OldPos := Pos;

    OldDragRect := DragRect;
    DragRect := OrigDragRect;
    if DragX then begin
      if not ReverseX then Inc(DragRect.Right, Pos.X-OrigPos.X)
      else Inc(DragRect.Left, Pos.X-OrigPos.X);
    end;
    if DragY then begin
      if not ReverseY then Inc(DragRect.Bottom, Pos.Y-OrigPos.Y)
      else Inc(DragRect.Top, Pos.Y-OrigPos.Y);
    end;
    if DragRect.Right-DragRect.Left < MinWidth then begin
      if not ReverseX then DragRect.Right := DragRect.Left + MinWidth
      else DragRect.Left := DragRect.Right - MinWidth;
    end;
    if (MaxWidth > 0) and (DragRect.Right-DragRect.Left > MaxWidth) then begin
      if not ReverseX then DragRect.Right := DragRect.Left + MaxWidth
      else DragRect.Left := DragRect.Right - MaxWidth;
    end;
    if DragRect.Bottom-DragRect.Top < MinHeight then begin
      if not ReverseY then DragRect.Bottom := DragRect.Top + MinHeight
      else DragRect.Top := DragRect.Bottom - MinHeight;
    end;
    if (MaxHeight > 0) and (DragRect.Bottom-DragRect.Top > MaxHeight) then begin
      if not ReverseY then DragRect.Bottom := DragRect.Top + MaxHeight
      else DragRect.Top := DragRect.Bottom - MaxHeight;
    end;

    ResizeTrack(DragRect, OrigDragRect);
    if not UseSmoothDrag then
      DrawDraggingOutline(ScreenDC, DragRect, OldDragRect, False, False)
    else
      DoResize;
  end;
var
  Accept: Boolean;
  Msg: TMsg;
  R: TRect;
begin
  if not Floating then Exit;

  Accept := False;

  UseSmoothDrag := FSmoothDrag;

  MinWidth := 0;
  MinHeight := 0;
  MaxWidth := 0;
  MaxHeight := 0;
  GetMinMaxSize(MinWidth, MinHeight, MaxWidth, MaxHeight);
  Inc(MinWidth, Parent.Width-Width);
  Inc(MinHeight, Parent.Height-Height);
  if MaxWidth > 0 then
    Inc(MaxWidth, Parent.Width-Width);
  if MaxHeight > 0 then
    Inc(MaxHeight, Parent.Height-Height);

  DragX := ASizeHandle in [twshLeft, twshRight, twshTopLeft, twshTopRight,
    twshBottomLeft, twshBottomRight];
  ReverseX := ASizeHandle in [twshLeft, twshTopLeft, twshBottomLeft];
  DragY := ASizeHandle in [twshTop, twshTopLeft, twshTopRight, twshBottom,
    twshBottomLeft, twshBottomRight];
  ReverseY := ASizeHandle in [twshTop, twshTopLeft, twshTopRight];

  ResizeBegin(ASizeHandle);
  try
    { Before locking, make sure all pending paint messages are processed }
    ProcessPaintMessages;

    if not UseSmoothDrag then begin
      { This uses LockWindowUpdate to suppress all window updating so the
        dragging outlines doesn't sometimes get garbled. (This is safe, and in
        fact, is the main purpose of the LockWindowUpdate function)
        IMPORTANT! While debugging you might want to enable the 'TB2Dock_DisableLock'
        conditional define (see top of the source code). }
      {$IFNDEF TB2Dock_DisableLock}
      LockWindowUpdate(GetDesktopWindow);
      {$ENDIF}
      { Get a DC of the entire screen. Works around the window update lock
        by specifying DCX_LOCKWINDOWUPDATE. }
      ScreenDC := GetDCEx(GetDesktopWindow, 0,
        DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW);
    end
    else
      ScreenDC := 0;
    try
      SetCapture(Handle);
      if (tbdsResizeClipCursor in FDockableWindowStyles) and
         not UsingMultipleMonitors then begin
        R := GetRectOfPrimaryMonitor(False);
        ClipCursor({$IFNDEF CLR}@{$ENDIF} R);
      end;

      { Initialize }
      OrigDragRect := Parent.BoundsRect;
      DragRect := OrigDragRect;
      if not UseSmoothDrag then begin
        SetRectEmpty(R);
        DrawDraggingOutline(ScreenDC, DragRect, R, False, False);
      end;
      GetCursorPos(OrigPos);
      OldPos := OrigPos;

      { Stay in message loop until capture is lost. Capture is removed either
        by this procedure manually doing it, or by an outside influence (like
        a message box or menu popping up) }
      while GetCapture = Handle do begin
        case Integer(GetMessage(Msg, 0, 0, 0)) of
          -1: Break; { if GetMessage failed }
          0: begin
               { Repost WM_QUIT messages }
               PostQuitMessage(ClipToLongint(Msg.wParam));
               Break;
             end;
        end;

        case Msg.Message of
          WM_KEYDOWN, WM_KEYUP:
            { Ignore all keystrokes while sizing except for Escape }
            if Word(Msg.wParam) = VK_ESCAPE then
              Break;
          WM_MOUSEMOVE:
            { Note to self: WM_MOUSEMOVE messages should never be dispatched
              here to ensure no hints get shown during the drag process }
            MouseMoved;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
            { Make sure it doesn't begin another loop }
            Break;
          WM_LBUTTONUP: begin
              Accept := True;
              Break;
            end;
          WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:
            { Ignore all other mouse up/down messages }
            ;
        else
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Since it sometimes breaks out of the loop without capture being
        released }
      if GetCapture = Handle then
        ReleaseCapture;
      ClipCursor(nil);

      if not UseSmoothDrag then begin
        { Hide dragging outline. Since NT will release a window update lock if
          another thread comes to the foreground, it has to release the DC
          and get a new one for erasing the dragging outline. Otherwise,
          the DrawDraggingOutline appears to have no effect when this happens. }
        ReleaseDC(GetDesktopWindow, ScreenDC);
        ScreenDC := GetDCEx(GetDesktopWindow, 0,
          DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW);
        SetRectEmpty(R);
        DrawDraggingOutline(ScreenDC, R, DragRect, False, False);
        ReleaseDC(GetDesktopWindow, ScreenDC);

        { Release window update lock }
        {$IFNDEF TB2Dock_DisableLock}
        LockWindowUpdate(0);
        {$ENDIF}
      end;
    end;

    if not UseSmoothDrag and Accept then
      DoResize;
  finally
    ResizeEnd;
  end;
end;

procedure TTBCustomDockableWindow.DoDockChangingHidden(NewFloating: Boolean;
  DockingTo: TTBDock);
begin
  if not(csDestroying in ComponentState) and Assigned(FOnDockChangingHidden) then
    FOnDockChangingHidden(Self, NewFloating, DockingTo);
end;

{ TTBCustomDockableWindow - property access methods }

function TTBCustomDockableWindow.GetNonClientWidth: Integer;
begin
  Result := CalcNCSizes.X;
end;

function TTBCustomDockableWindow.GetNonClientHeight: Integer;
begin
  Result := CalcNCSizes.Y;
end;

function TTBCustomDockableWindow.IsLastDockStored: Boolean;
begin
  Result := FCurrentDock = nil;   {}{should this be changed to 'Floating'?}
end;

function TTBCustomDockableWindow.IsWidthAndHeightStored: Boolean;
begin
  Result := (CurrentDock = nil) and not Floating;
end;

procedure TTBCustomDockableWindow.SetCloseButton(Value: Boolean);
begin
  if FCloseButton <> Value then begin
    FCloseButton := Value;

    { Update the close button's visibility }
    if Parent is TTBFloatingWindowParent then
      TTBFloatingWindowParent(Parent).RedrawNCArea([twrdCaption, twrdCloseButton]);
  end;
end;

procedure TTBCustomDockableWindow.SetCloseButtonWhenDocked(Value: Boolean);
begin
  if FCloseButtonWhenDocked <> Value then begin
    FCloseButtonWhenDocked := Value;
    if Docked then
      RecalcNCArea(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetDefaultDock(Value: TTBDock);
begin
  if FDefaultDock <> Value then begin
    FDefaultDock := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetCurrentDock(Value: TTBDock);
begin
  if not(csLoading in ComponentState) then begin
    if Assigned(Value) then
      Parent := Value
    else
      Parent := TBValidToolWindowParentForm(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetDockPos(Value: Integer);
begin
  FDockPos := Value;
  if Docked then
    CurrentDock.ArrangeToolbars;
end;

procedure TTBCustomDockableWindow.SetDockRow(Value: Integer);
begin
  FDockRow := Value;
  if Docked then
    CurrentDock.ArrangeToolbars;
end;

procedure TTBCustomDockableWindow.SetAutoResize(Value: Boolean);
begin
  if FAutoResize <> Value then begin
    FAutoResize := Value;
    if Value then
      Arrange;
  end;
end;

procedure TTBCustomDockableWindow.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    if Docked then
      RecalcNCArea(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetDragHandleStyle(Value: TTBDragHandleStyle);
begin
  if FDragHandleStyle <> Value then begin
    FDragHandleStyle := Value;
    if Docked then
      RecalcNCArea(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetFloating(Value: Boolean);
var
  ParentFrm: TTBCustomForm;
  NewFloatParent: TTBFloatingWindowParent;
begin
  if FFloating <> Value then begin
    if Value and not(csDesigning in ComponentState) then begin
      ParentFrm := TBValidToolWindowParentForm(Self);
      if FFloatParent = nil then begin
        NewFloatParent := GetFloatingWindowParentClass.CreateNew(nil);
        try
          with NewFloatParent do begin
            FDockableWindow := Self;
            BorderStyle := bsToolWindow;
            ShowHint := True;
            Visible := True;
            { Note: The above line doesn't actually make it visible at this
              point since FShouldShow is still False. }
          end;
        except
          NewFloatParent.Free;
          raise;
        end;
        FFloatParent := NewFloatParent;
      end;
      ParentFrm.FreeNotification(FFloatParent);
      FFloatParent.FParentForm := ParentFrm;
      FFloatParent.Caption := Caption;
      Parent := FFloatParent;
      SetBounds(0, 0, Width, Height);
    end
    else
      Parent := TBValidToolWindowParentForm(Self);
  end;
end;

procedure TTBCustomDockableWindow.SetFloatingMode(Value: TTBFloatingMode);
begin
  if FFloatingMode <> Value then begin
    FFloatingMode := Value;
    if HandleAllocated then
      Perform(CM_SHOWINGCHANGED, 0, 0);
  end;
end;

procedure TTBCustomDockableWindow.SetFloatingPosition(Value: TPoint);
begin
  FFloatingPosition := Value;
  if Floating and Assigned(Parent) then
    Parent.SetBounds(Value.X, Value.Y, Parent.Width, Parent.Height);
end;

procedure TTBCustomDockableWindow.SetFullSize(Value: Boolean);
begin
  if FFullSize <> Value then begin
    FFullSize := Value;
    if Docked then
      CurrentDock.ArrangeToolbars;
  end;
end;

procedure TTBCustomDockableWindow.SetLastDock(Value: TTBDock);
begin
  if FUseLastDock and Assigned(FCurrentDock) then
    { When docked, LastDock must be equal to DockedTo }
    Value := FCurrentDock;
  if FLastDock <> Value then begin
    if Assigned(FLastDock) and (FLastDock <> Parent) then
      FLastDock.ChangeDockList(False, Self);
    FLastDock := Value;
    if Assigned(Value) then begin
      FUseLastDock := True;
      Value.FreeNotification(Self);
      Value.ChangeDockList(True, Self);
    end;
  end;
end;

procedure TTBCustomDockableWindow.SetResizable(Value: Boolean);
begin
  if FResizable <> Value then begin
    FResizable := Value;
    if Floating and (Parent is TTBFloatingWindowParent) then begin
      { Recreate the window handle because Resizable affects whether the
        tool window is created with a WS_THICKFRAME style }
      TTBFloatingWindowParent(Parent).CallRecreateWnd;
    end;
  end;
end;

procedure TTBCustomDockableWindow.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    if Floating then begin
      { Recalculate FloatingWindowParent's NC area, and resize the toolbar
        accordingly }
      RecalcNCArea(Parent);
      Arrange;
    end;
  end;
end;

procedure TTBCustomDockableWindow.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then begin
    FStretch := Value;
    if Docked then
      CurrentDock.ArrangeToolbars;
  end;
end;

procedure TTBCustomDockableWindow.SetUseLastDock(Value: Boolean);
begin
  if FUseLastDock <> Value then begin
    FUseLastDock := Value;
    if not Value then
      LastDock := nil
    else
      LastDock := FCurrentDock;
  end;
end;

(*function TTBCustomDockableWindow.GetVersion: TToolbar97Version;
begin
  Result := Toolbar97VersionPropText;
end;

procedure TTBCustomDockableWindow.SetVersion(const Value: TToolbar97Version);
begin
  { write method required for the property to show up in Object Inspector }
end;*)


{ TTBBackground }

{$IFNDEF CLR}
type
  PNotifyEvent = ^TNotifyEvent;
{$ENDIF}

constructor TTBBackground.Create(AOwner: TComponent);
begin
  inherited;
  FBkColor := clBtnFace;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
end;

destructor TTBBackground.Destroy;
{$IFNDEF CLR}
var
  I: Integer;
{$ENDIF}
begin
  inherited;
  FBitmapCache.Free;
  FBitmap.Free;
  if Assigned(FNotifyList) then begin
    {$IFNDEF CLR}
    for I := FNotifyList.Count-1 downto 0 do
      Dispose(PNotifyEvent(FNotifyList[I]));
    {$ENDIF}
    FNotifyList.Free;
  end;
end;

procedure TTBBackground.BitmapChanged(Sender: TObject);
var
  I: Integer;
begin
  { Erase the cache and notify }
  FreeAndNil(FBitmapCache);
  if Assigned(FNotifyList) then
    for I := 0 to FNotifyList.Count-1 do
      {$IFNDEF CLR}
      PNotifyEvent(FNotifyList[I])^(Self);
      {$ELSE}
      TNotifyEvent(FNotifyList[I])(Self);
      {$ENDIF}
end;

procedure TTBBackground.Draw(DC: HDC; const DrawRect: TRect);
var
  UseBmp: TBitmap;
  R2: TRect;
  SaveIndex: Integer;
  DC2: HDC;
  Brush: HBRUSH;
  P: TPoint;
begin
  if FBitmapCache = nil then begin
    FBitmapCache := TBitmap.Create;
    FBitmapCache.Palette := CopyPalette(FBitmap.Palette);
    FBitmapCache.Width := FBitmap.Width;
    FBitmapCache.Height := FBitmap.Height;
    if not FTransparent then begin
      { Copy from a possible DIB to our DDB }
      BitBlt(FBitmapCache.Canvas.Handle, 0, 0, FBitmapCache.Width,
        FBitmapCache.Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else begin
      with FBitmapCache do begin
        Canvas.Brush.Color := FBkColor;
        R2 := Rect(0, 0, Width, Height);
        Canvas.BrushCopy(R2, FBitmap, R2,
          FBitmap.Canvas.Pixels[0, Height-1] or $02000000);
      end;
    end;
    FBitmap.Dormant;
  end;
  UseBmp := FBitmapCache;

  DC2 := 0;
  SaveIndex := SaveDC(DC);
  try
    if UseBmp.Palette <> 0 then begin
      SelectPalette(DC, UseBmp.Palette, True);
      RealizePalette(DC);
    end;
    { Note: versions of Toolbar97 prior to 1.68 used 'UseBmp.Canvas.Handle'
      instead of DC2 in the BitBlt call. This was changed because there
      seems to be a bug in D2/BCB1's Graphics.pas: if you called
      <dockname>.Background.LoadFromFile(<filename>) twice the background
      would not be shown. }
    if (UseBmp.Width = 8) and (UseBmp.Height = 8) then begin
      { Use pattern brushes to draw 8x8 bitmaps.
        Note: Win9x can't use bitmaps <8x8 in size for pattern brushes }
      Brush := CreatePatternBrush(UseBmp.Handle);
      GetWindowOrgEx(DC, P);
      SetBrushOrgEx(DC, DrawRect.Left - P.X, DrawRect.Top - P.Y, nil);
      FillRect(DC, DrawRect, Brush);
      DeleteObject(Brush);
    end
    else begin
      { BitBlt is faster than pattern brushes on large bitmaps }
      DC2 := CreateCompatibleDC(DC);
      SelectObject(DC2, UseBmp.Handle);
      R2 := DrawRect;
      while R2.Left < R2.Right do begin
        while R2.Top < R2.Bottom do begin
          BitBlt(DC, R2.Left, R2.Top, UseBmp.Width, UseBmp.Height,
            DC2, 0, 0, SRCCOPY);
          Inc(R2.Top, UseBmp.Height);
        end;
        R2.Top := DrawRect.Top;
        Inc(R2.Left, UseBmp.Width);
      end;
    end;
  finally
    if DC2 <> 0 then
      DeleteDC(DC2);
    { Restore the palette and brush origin back }
    RestoreDC(DC, SaveIndex);
  end;
end;

function TTBBackground.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;

procedure TTBBackground.SysColorChanged;
begin
  if FTransparent and (FBkColor < 0) then
    BitmapChanged(nil);
end;

function TTBBackground.UsingBackground: Boolean;
begin
  Result := (FBitmap.Width <> 0) and (FBitmap.Height <> 0);
end;

procedure TTBBackground.RegisterChanges(Proc: TNotifyEvent);
var
  I: Integer;
  {$IFNDEF CLR}
  P: PNotifyEvent;
  {$ENDIF}
begin
  if FNotifyList = nil then
    FNotifyList := TList.Create;
  for I := 0 to FNotifyList.Count-1 do begin
    {$IFNDEF CLR}
    P := FNotifyList[I];
    if MethodsEqual(TMethod(P^), TMethod(Proc)) then
    {$ELSE}
    if @TNotifyEvent(FNotifyList[I]) = @Proc then
    {$ENDIF}
      Exit;
  end;
  {$IFNDEF CLR}
  FNotifyList.Expand;
  New(P);
  P^ := Proc;
  FNotifyList.Add(P);
  {$ELSE}
  FNotifyList.Add(@Proc);
  {$ENDIF}
end;

procedure TTBBackground.UnregisterChanges(Proc: TNotifyEvent);
var
  I: Integer;
  {$IFNDEF CLR}
  P: PNotifyEvent;
  {$ENDIF}
begin
  if FNotifyList = nil then
    Exit;
  for I := 0 to FNotifyList.Count-1 do begin
    {$IFNDEF CLR}
    P := FNotifyList[I];
    if MethodsEqual(TMethod(P^), TMethod(Proc)) then begin
    {$ELSE}
    if @TNotifyEvent(FNotifyList[I]) = @Proc then begin
    {$ENDIF}
      FNotifyList.Delete(I);
      {$IFNDEF CLR}
      Dispose(P);
      {$ENDIF}
      Break;
    end;
  end;
end;

procedure TTBBackground.SetBkColor(Value: TColor);
begin
  if FBkColor <> Value then begin
    FBkColor := Value;
    if FTransparent then
      BitmapChanged(nil);
  end;
end;

procedure TTBBackground.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TTBBackground.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then begin
    FTransparent := Value;
    BitmapChanged(nil);
  end;
end;


{ Global procedures }

procedure TBCustomLoadPositions(const OwnerComponent: TComponent;
  const ReadIntProc: TTBPositionReadIntProc;
  const ReadStringProc: TTBPositionReadStringProc;
  const ExtraData: TTBPositionExtraData);
var
  Rev: Integer;

  function FindDock(AName: String): TTBDock;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to OwnerComponent.ComponentCount-1 do
      if (OwnerComponent.Components[I] is TTBDock) and
         {$IFNDEF CLR}
         (CompareText(OwnerComponent.Components[I].Name, AName) = 0) then begin
         {$ELSE}
         SameText(OwnerComponent.Components[I].Name, AName, loInvariantLocale) then begin
         {$ENDIF}
        Result := TTBDock(OwnerComponent.Components[I]);
        Break;
      end;
  end;

  procedure ReadValues(const Toolbar: TTBCustomDockableWindow; const NewDock: TTBDock);
  var
    Pos: TPoint;
    Data: TTBReadPositionData;
    LastDockName: String;
    ADock: TTBDock;
  begin
    with Toolbar do begin
      DockRow := ReadIntProc(Name, rvDockRow, DockRow, ExtraData);
      DockPos := ReadIntProc(Name, rvDockPos, DockPos, ExtraData);
      Pos.X := ReadIntProc(Name, rvFloatLeft, 0, ExtraData);
      Pos.Y := ReadIntProc(Name, rvFloatTop, 0, ExtraData);
      Data.ReadIntProc := ReadIntProc;
      Data.ReadStringProc := ReadStringProc;
      Data.ExtraData := ExtraData;
      ReadPositionData(Data);
      FloatingPosition := Pos;
      if Assigned(NewDock) then
        Parent := NewDock
      else begin
        //Parent := Form;
        Floating := True;
        MoveOnScreen(True);
        if (Rev >= 3) and FUseLastDock then begin
          LastDockName := ReadStringProc(Name, rvLastDock, '', ExtraData);
          if LastDockName <> '' then begin
            ADock := FindDock(LastDockName);
            if Assigned(ADock) then
              LastDock := ADock;
          end;
        end;
      end;
      Arrange;
      DoneReadingPositionData(Data);
    end;
  end;

var
  DocksDisabled: TList;
  I: Integer;
  ToolWindow: TComponent;
  ADock: TTBDock;
  DockedToName: String;
begin
  DocksDisabled := TList.Create;
  try
    with OwnerComponent do
      for I := 0 to ComponentCount-1 do
        if Components[I] is TTBDock then begin
          TTBDock(Components[I]).BeginUpdate;
          DocksDisabled.Add(Components[I]);
        end;

    for I := 0 to OwnerComponent.ComponentCount-1 do begin
      ToolWindow := OwnerComponent.Components[I];
      if ToolWindow is TTBCustomDockableWindow then
        with TTBCustomDockableWindow(ToolWindow) do begin
          {}{should skip over toolbars that are neither Docked nor Floating }
          if Name = '' then
            Continue;
          Rev := ReadIntProc(Name, rvRev, 0, ExtraData);
          if Rev = 2000 then begin
            Visible := ReadIntProc(Name, rvVisible, Ord(Visible), ExtraData) <> 0;
            DockedToName := ReadStringProc(Name, rvDockedTo, '', ExtraData);
            if DockedToName <> '' then begin
              if DockedToName <> rdDockedToFloating then begin
                ADock := FindDock(DockedToName);
                if (ADock <> nil) and (ADock.FAllowDrag) then
                  ReadValues(TTBCustomDockableWindow(ToolWindow), ADock);
              end
              else
                ReadValues(TTBCustomDockableWindow(ToolWindow), nil);
            end;
          end;
        end;
    end;
  finally
    for I := DocksDisabled.Count-1 downto 0 do
      TTBDock(DocksDisabled[I]).EndUpdate;
    DocksDisabled.Free;
  end;
end;

procedure TBCustomSavePositions(const OwnerComponent: TComponent;
  const WriteIntProc: TTBPositionWriteIntProc;
  const WriteStringProc: TTBPositionWriteStringProc;
  const ExtraData: TTBPositionExtraData);
var
  I: Integer;
  N, L: String;
  Data: TTBWritePositionData;
begin
  for I := 0 to OwnerComponent.ComponentCount-1 do
    if OwnerComponent.Components[I] is TTBCustomDockableWindow then
      with TTBCustomDockableWindow(OwnerComponent.Components[I]) do begin
        if Name = '' then
          Continue;
        if Floating then
          N := rdDockedToFloating
        else if Docked then begin
          if CurrentDock.FAllowDrag then begin
            N := CurrentDock.Name;
            if N = '' then
              raise Exception.Create(STBToolwinDockedToNameNotSet);
          end
          else
            N := '';
        end
        else
          Continue;  { skip if it's neither floating nor docked }
        L := '';
        if Assigned(FLastDock) then
          L := FLastDock.Name;
        WriteIntProc(Name, rvRev, rdCurrentRev, ExtraData);
        WriteIntProc(Name, rvVisible, Ord(Visible), ExtraData);
        WriteStringProc(Name, rvDockedTo, N, ExtraData);
        WriteStringProc(Name, rvLastDock, L, ExtraData);
        WriteIntProc(Name, rvDockRow, FDockRow, ExtraData);
        WriteIntProc(Name, rvDockPos, FDockPos, ExtraData);
        WriteIntProc(Name, rvFloatLeft, FFloatingPosition.X, ExtraData);
        WriteIntProc(Name, rvFloatTop, FFloatingPosition.Y, ExtraData);
        Data.WriteIntProc := WriteIntProc;
        Data.WriteStringProc := WriteStringProc;
        Data.ExtraData := ExtraData;
        WritePositionData(Data);
      end;
end;

type
  TIniReadWriteData = class
  private
    IniFile: TCustomIniFile;
    SectionNamePrefix: String;
  end;

function IniReadInt(const ToolbarName, Value: String; const Default: Longint;
  const ExtraData: TTBPositionExtraData): Longint;
begin
  Result := TIniReadWriteData(ExtraData).IniFile.ReadInteger(
    TIniReadWriteData(ExtraData).SectionNamePrefix + ToolbarName, Value, Default);
end;
function IniReadString(const ToolbarName, Value, Default: String;
  const ExtraData: TTBPositionExtraData): String;
begin
  Result := TIniReadWriteData(ExtraData).IniFile.ReadString(
    TIniReadWriteData(ExtraData).SectionNamePrefix + ToolbarName, Value, Default);
end;
procedure IniWriteInt(const ToolbarName, Value: String; const Data: Longint;
  const ExtraData: TTBPositionExtraData);
begin
  TIniReadWriteData(ExtraData).IniFile.WriteInteger(
    TIniReadWriteData(ExtraData).SectionNamePrefix + ToolbarName, Value, Data);
end;
procedure IniWriteString(const ToolbarName, Value, Data: String;
  const ExtraData: TTBPositionExtraData);
begin
  TIniReadWriteData(ExtraData).IniFile.WriteString(
    TIniReadWriteData(ExtraData).SectionNamePrefix + ToolbarName, Value, Data);
end;

procedure TBIniLoadPositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: String);
var
  Data: TIniReadWriteData;
begin
  Data := TIniReadWriteData.Create;
  try
    Data.IniFile := IniFile;
    Data.SectionNamePrefix := SectionNamePrefix;
    TBCustomLoadPositions(OwnerComponent, IniReadInt, IniReadString, Data);
  finally
    Data.Free;
  end;
end;

procedure TBIniLoadPositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: String);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Filename);
  try
    TBIniLoadPositions(OwnerComponent, IniFile, SectionNamePrefix);
  finally
    IniFile.Free;
  end;
end;

procedure TBIniSavePositions(const OwnerComponent: TComponent;
  const IniFile: TCustomIniFile; const SectionNamePrefix: String);
var
  Data: TIniReadWriteData;
begin
  Data := TIniReadWriteData.Create;
  try
    Data.IniFile := IniFile;
    Data.SectionNamePrefix := SectionNamePrefix;
    TBCustomSavePositions(OwnerComponent, IniWriteInt, IniWriteString, Data);
  finally
    Data.Free;
  end;
end;

procedure TBIniSavePositions(const OwnerComponent: TComponent;
  const Filename, SectionNamePrefix: String);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Filename);
  try
    TBIniSavePositions(OwnerComponent, IniFile, SectionNamePrefix);
  finally
    IniFile.Free;
  end;
end;

function RegReadInt(const ToolbarName, Value: String; const Default: Longint;
  const ExtraData: TTBPositionExtraData): Longint;
begin
  Result := TRegIniFile(ExtraData).ReadInteger(ToolbarName, Value, Default);
end;
function RegReadString(const ToolbarName, Value, Default: String;
  const ExtraData: TTBPositionExtraData): String;
begin
  Result := TRegIniFile(ExtraData).ReadString(ToolbarName, Value, Default);
end;
procedure RegWriteInt(const ToolbarName, Value: String; const Data: Longint;
  const ExtraData: TTBPositionExtraData);
begin
  TRegIniFile(ExtraData).WriteInteger(ToolbarName, Value, Data);
end;
procedure RegWriteString(const ToolbarName, Value, Data: String;
  const ExtraData: TTBPositionExtraData);
begin
  TRegIniFile(ExtraData).WriteString(ToolbarName, Value, Data);
end;

procedure TBRegLoadPositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: String);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  try
    {$IFDEF JR_D5}
    Reg.Access := KEY_QUERY_VALUE;
    {$ENDIF}
    Reg.RootKey := RootKey;
    if Reg.OpenKey(BaseRegistryKey, False) then
      TBCustomLoadPositions(OwnerComponent, RegReadInt, RegReadString, Reg);
  finally
    Reg.Free;
  end;
end;

procedure TBRegSavePositions(const OwnerComponent: TComponent;
  const RootKey: DWORD; const BaseRegistryKey: String);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := RootKey;
    Reg.CreateKey(BaseRegistryKey);
    if Reg.OpenKey(BaseRegistryKey, True) then
      TBCustomSavePositions(OwnerComponent, RegWriteInt, RegWriteString, Reg);
  finally
    Reg.Free;
  end;
end;

end.
