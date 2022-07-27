{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit      : rmCCTabs
Purpose   : This is some of the best features from the depricated ComCtrls95
            component set, that I also authored.  Floating Tabsheets and Tabhints.
Date      : 02-01-2000
Author    : Ryan J. Mills
Version   : 1.90
================================================================================}

unit rmCCTabs;

interface

{$I CompilerDefines.INC}

uses Messages, Windows, SysUtils, CommCtrl, Classes, Controls, Graphics,
     ImgList, Forms;

const                                 
     CM_rmCCTabsBase            = CM_BASE+333;
     CM_rmCCTabSheetDraggedON   = CM_rmCCTabsBase+1;
     CM_rmCCTabSheetDraggedOFF  = CM_rmCCTabsBase+2;

type
  ECCTabError = Exception;

  TrmCustomCCTabControl = class;

  TTabChangingEvent = procedure(Sender: TObject; var AllowChange: Boolean) of object;

  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);
  TTabStyle = (tsTabs, tsButtons, tsFlatButtons);
  TGripAlign = (gaLeft, gaRight, gaTop, gaBottom);
  TFloatState = (fsFloating, fsDocked);
  TTabFloatEvent = procedure(Sender: TObject; FloatState: TFloatState) of object;
  TTabTrackEvent = procedure(sender:TObject; TabIndex: integer) of object;

  TDrawTabEvent = procedure(Control: TrmCustomCCTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;
  TTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;

  TrmCustomCCTabControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FMouseDragTab: integer;
    FMouseOverTab: integer;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FSaveTabIndex: Integer;
    FSaveTabs: TStringList;
    FScrollOpposite: Boolean;
    FStyle: TTabStyle;
    FTabPosition: TTabPosition;
    FTabs: TStrings;
    FTabShifting:boolean;
    FTabSize: TSmallPoint;
    FUpdating: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    FOnTabTrack: TTabTrackEvent;
    FOnTabShift: TNotifyEvent;
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    procedure ImageListChange(Sender: TObject);
    function InternalSetMultiLine(Value: Boolean): Boolean;
    procedure SetHotTrack(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Smallint);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTabs(Value: TStrings);
    procedure SetTabWidth(Value: Smallint);
    procedure TabsChanged;
    procedure UpdateTabSize;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure CMMouseLeave(var message:tmessage); message CM_MOUSELEAVE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMNCHitTest(var message:TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanChange: Boolean; dynamic;
    function CanShowTab(TabIndex: Integer): Boolean; virtual;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DisplayTabHint(TabIndex:integer); virtual; abstract;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure UpdateTabImages;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property AllowTabShifting:boolean read fTabShifting write fTabShifting default false;
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property ScrollOpposite: Boolean read FScrollOpposite
      write SetScrollOpposite default False;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnTabTrack:TTabTrackEvent read fontabtrack write fontabtrack;
    property OnTabShift:TNotifyEvent read FOnTabShift write FOnTabShift;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabAt(x,y:integer):integer;
    property Canvas: TCanvas read FCanvas;
    property TabStop default True;
  end;

  TrmCCTabControl = class(TrmCustomCCTabControl)
  private
    FTabHints:TStrings;
    procedure SetTabHints(value:TStrings);
  protected
    procedure DisplayTabHint(TabIndex:integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    property DisplayRect;
  published
    property AllowTabShifting;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property MultiSelect;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabHints:tstrings read ftabHints write SetTabHints;
    property TabOrder;
    property TabPosition;
    property Tabs;
    property TabIndex;  // must be after Tabs
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTabTrack;
    property OnTabShift;
    property OnUnDock;
  end;

  TrmCCPageControl = class;
  TrmCCTabSheet = class;

  TrmCCTabsFloatingForm = class(TForm)
  private
    { Private declarations }
    FSheet : TrmCCTabSheet;
    fMoveSize: TNotifyEvent;
    procedure DoCloseWindow(Sender: TObject; var Action: TCloseAction);
    procedure DoDestroyWindow(Sender: TObject);
    procedure wmExitSizeMove(var msg: TMessage); message WM_ExitSizeMove;
  public
    { Public declarations }
    constructor CreateNew(AOwner: TComponent); reintroduce;
    property TabSheet: TrmCCTabSheet read FSheet write FSheet;
    property OnMoveSize: TNotifyEvent read fMoveSize write fMoveSize;
  end;

  TrmCCTabSheet = class(TWinControl)
  private
    FImageIndex: Integer;
    FPageControl: TrmCCPageControl;
    FTabHint : string;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;

    //Floating Vars...
    fOldMousePos: TPoint;    { previous mouse position                      }
    fMouseOffset: TPoint;    { Mouse coordinates in relation to client rect }
    fDragStart: boolean;
    fDragging: boolean;   { true when dragging                           }
    fDragRect: TRect;     { position of rectangle while dragging         }
    fDragable: boolean;   { true to make this dragable                   }
    fWidth,
    fHeight: integer;   { width and height of client area at dragstart }
    fOldPageControl: TrmCCPageControl; { saves the page control so that it can be reset }
    ffloating: boolean;
    FOnFloatChange: TTabFloatEvent;

    { fields for the form }
    fFloatingForm                : TrmCCTabsFloatingForm; { form to use when floating           }
    fFloatOnTop                  : Boolean;
    fcanvas : tcanvas;
    fGripAlign : TGripAlign;
    FPageIndex : integer;
    fMoveSize: TNotifyEvent;

    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    procedure SetPageControl(APageControl: TrmCCPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure UpdateTabShowing;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

    //Floating Tabsheet Procedures and Functions
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Msg : TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMPaint(var msg:TWMPaint); message WM_Paint;

    procedure DrawDraggingRect(MousePos : TPoint); { Draws the focus rectangle at appropriate place}
    procedure setDragOption(value:boolean);
    procedure setGripAlign(value:TGripAlign);
    function GetGripRect:TRect;
    function GetGripperRect:TRect;
    procedure SetFloatOnTop(const Value: boolean);
    procedure DoMoveSize(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TrmCCPageControl read FPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;

    //Floating Tabsheet Procedures and Functions
    procedure FloatTabSheet;
    procedure FloatTabSheetBounds(aLeft, aTop, aWidth, aHeight: integer);
    procedure DockTabSheet;
    function GetClientRect:TRect; override;
    property FloatingForm: TrmCCTabsFloatingForm read fFloatingForm;
    property GripRect: TRect read GetGripperRect;
    property Floating: Boolean read fFloating;
  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Dragable : boolean read fDragable write SetDragOption default false; //Floating
    property Enabled;
    property FloatOnTop : boolean read FFloatOnTop write SetFloatOnTop default false;
    property Font;
    property GripAlign : TGripAlign read FGripAlign write SetGripAlign;
    property Height stored False;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default 0;
    property Left stored False;
    property Constraints;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StaticPageIndex : integer read fpageindex write fpageindex;
    property TabHint:string read FTabHint write FTabHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFloatChange: TTabFloatEvent read FOnFloatChange write FOnFloatChange;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;
    property OnFloatFormMoveSize: TNotifyEvent read fMoveSize write fMoveSize;
  end;

  TrmCCPageControl = class(TrmCustomCCTabControl)
  private
    FPages: TList;
    FActivePage: TrmCCTabSheet;
    FNewDockSheet: TrmCCTabSheet;
    FUndockingPage: TrmCCTabSheet;

    FFloatingPages: TList;
    FOnFloatChange: TTabFloatEvent;

    procedure ChangeActivePage(Page: TrmCCTabSheet);
    procedure DeleteTab(Page: TrmCCTabSheet; Index: Integer);
    function GetDockClientFromMousePos(MousePos: TPoint): TControl;

    function GetPage(Index: Integer): TrmCCTabSheet;
    function GetPageCount: Integer;

    function GetFloatingPage(Index: Integer): TrmCCTabSheet;
    function GetFloatingPageCount: Integer;

    procedure InsertPage(Page: TrmCCTabSheet);
    procedure InsertTab(Page: TrmCCTabSheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TrmCCTabSheet);
    procedure SetActivePage(Page: TrmCCTabSheet);
    procedure UpdateTab(Page: TrmCCTabSheet);
    procedure UpdateActivePage;

    procedure AddToFloatList(Page: TrmCCTabSheet);
    procedure RemoveFromFloatList(Page: TrmCCTabSheet);

    procedure CMTabDraggedOff(var Message:TMessage); message CM_rmCCTabSheetDraggedOFF;
    procedure CMTabDraggedOn(var Message:TMessage); message CM_rmCCTabSheetDraggedON;

    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    function GetActivePageIndex: Integer;
    procedure SetActivePageIndex(const Value: Integer);
  protected
    function CanShowTab(TabIndex: Integer): Boolean; override;
    procedure Change; override;
    procedure DisplayTabHint(TabIndex:integer); override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetImageIndex(TabIndex: Integer): Integer; override;
    function GetPageFromDockClient(Client: TControl): TrmCCTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TrmCCTabSheet;
      GoForward, CheckTabVisible: Boolean): TrmCCTabSheet;
    procedure SelectNextPage(GoForward: Boolean);

    procedure HideFloatingPages;
    procedure ShowFloatingPages;

    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TrmCCTabSheet read GetPage;

    property FloatingPageCount: Integer read GetFloatingPageCount;
    property FloatingPages[Index: Integer]: TrmCCTabSheet read GetFloatingPage;

    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;

  published
    property ActivePage: TrmCCTabSheet read FActivePage write SetActivePage;
    property AllowTabShifting;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property TabIndex;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFloatChange: TTabFloatEvent read FOnFloatChange write FOnFloatChange;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTabTrack;
    property OnTabShift;
    property OnUnDock;
  end;

function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);

const
  ComCtlVersionIE3 = $00040046;
  ComCtlVersionIE4 = $00040047;
  ComCtlVersionIE401 = $00040048;

function GetComCtlVersion: Integer;

implementation

uses Consts, ComStrs;

const
  ComCtlDllName = 'comctl32.dll';
  GripSize = 7;

var
  ComCtlVersion: Integer;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create(SInvalidComCtl32);
end;

function GetComCtlVersion: Integer;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ComCtlVersion = 0 then
  begin
    FileName := ComCtlDllName;
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            ComCtlVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := ComCtlVersion;
end;

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

{ TTabStrings }

type
  TTabStrings = class(TStrings)
  private
    FTabControl: TrmCustomCCTabControl;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

procedure TabControlError(const S: string);
begin
  raise EListError.Create(S);
end;

procedure TTabStrings.Clear;
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEALLITEMS, 0, 0) = 0 then
    TabControlError(sTabFailClear);
  FTabControl.TabsChanged;
end;

procedure TTabStrings.Delete(Index: Integer);
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEITEM, Index, 0) = 0 then
    TabControlError(Format(sTabFailDelete, [Index]));
  FTabControl.TabsChanged;
end;

function TTabStrings.Get(Index: Integer): string;
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING); 
var
  TCItem: TTCItem;
  Buffer: array[0..4095] of Char;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading];
  TCItem.pszText := Buffer;
  TCItem.cchTextMax := SizeOf(Buffer);
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailRetrieve, [Index]));
  Result := Buffer;
end;

function TTabStrings.GetCount: Integer;
begin
  Result := SendMessage(FTabControl.Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TTabStrings.GetObject(Index: Integer): TObject;
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailGetObject, [Index]));
  Result := TObject(TCItem.lParam);
end;

procedure TTabStrings.Put(Index: Integer; const S: string);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TTabStrings.PutObject(Index: Integer; AObject: TObject);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  TCItem.lParam := Longint(AObject);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSetObject, [Index]));
end;

procedure TTabStrings.Insert(Index: Integer; const S: string);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_INSERTITEM, Index,
    Longint(@TCItem)) < 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TTabStrings.SetUpdateState(Updating: Boolean);
begin
  FTabControl.FUpdating := Updating;
  SendMessage(FTabControl.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
  begin
    FTabControl.Invalidate;
    FTabControl.TabsChanged;
  end;
end;

{ TrmCustomCCTabControl }

constructor TrmCustomCCTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 289;
  Height := 193;
  TabStop := True;
  ControlStyle := [csAcceptsControls, csDoubleClicks];
  FTabs := TTabStrings.Create;
  TTabStrings(FTabs).FTabControl := Self;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  fmouseovertab := -1;
  fTabShifting := false;
end;

destructor TrmCustomCCTabControl.Destroy;
begin
  FCanvas.Free;
  FTabs.Free;
  FSaveTabs.Free;
  FImageChangeLink.Free;
  inherited Destroy;
end;

function TrmCustomCCTabControl.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, Result);
end;

function TrmCustomCCTabControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TrmCustomCCTabControl.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TrmCustomCCTabControl.CreateParams(var Params: TCreateParams);
const
  AlignStyles: array[Boolean, TTabPosition] of DWORD =
    ((0, TCS_BOTTOM, TCS_VERTICAL, TCS_VERTICAL or TCS_RIGHT),
     (0, TCS_BOTTOM, TCS_VERTICAL or TCS_RIGHT, TCS_VERTICAL));
  TabStyles: array[TTabStyle] of DWORD = (TCS_TABS, TCS_BUTTONS,
    TCS_BUTTONS or TCS_FLATBUTTONS);
   RRStyles: array[Boolean] of DWORD = (0, TCS_RAGGEDRIGHT);
begin
  InitCommonControl(ICC_TAB_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TABCONTROL);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or
      AlignStyles[UseRightToLeftAlignment, FTabPosition] or
      TabStyles[FStyle] or RRStyles[FRaggedRight];
    if not TabStop then Style := Style or TCS_FOCUSNEVER;
    if FMultiLine then Style := Style or TCS_MULTILINE;
    if FMultiSelect then Style := Style or TCS_MULTISELECT;
    if FOwnerDraw then Style := Style or TCS_OWNERDRAWFIXED;
    if FTabSize.X <> 0 then Style := Style or TCS_FIXEDWIDTH;
    if FHotTrack and (not (csDesigning in ComponentState)) then
      Style := Style or TCS_HOTTRACK;
    if FScrollOpposite then Style := Style or TCS_SCROLLOPPOSITE;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW) or
      CS_DBLCLKS;
  end;
end;

procedure TrmCustomCCTabControl.CreateWnd;
begin
  inherited CreateWnd;
  if (Images <> nil) and Images.HandleAllocated then
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
  if Integer(FTabSize) <> 0 then UpdateTabSize;
  if FSaveTabs <> nil then
  begin
    FTabs.Assign(FSaveTabs);
    SetTabIndex(FSaveTabIndex);
    FSaveTabs.Free;
    FSaveTabs := nil;
  end;
end;

procedure TrmCustomCCTabControl.DestroyWnd;
begin
  if FTabs.Count > 0 then
  begin
    FSaveTabs := TStringList.Create;
    FSaveTabs.Assign(FTabs);
    FSaveTabIndex := GetTabIndex;
  end;
  inherited DestroyWnd;
end;

procedure TrmCustomCCTabControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabIndex, Rect, Active) else
    FCanvas.FillRect(Rect);
end;

function TrmCustomCCTabControl.GetDisplayRect: TRect;
begin
  Result := ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, Integer(@Result));
  Inc(Result.Top, 2);
end;

function TrmCustomCCTabControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result := TabIndex;
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, TabIndex, Result);
end;

function TrmCustomCCTabControl.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

procedure TrmCustomCCTabControl.Loaded;
begin
  inherited Loaded;
  if Images <> nil then UpdateTabImages;
end;

procedure TrmCustomCCTabControl.SetHotTrack(Value: Boolean);
begin                                 
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TrmCustomCCTabControl.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
  end
  else Perform(TCM_SETIMAGELIST, 0, 0);
end;

procedure TrmCustomCCTabControl.ImageListChange(Sender: TObject);
begin
  Perform(TCM_SETIMAGELIST, 0, TCustomImageList(Sender).Handle);
end;

function TrmCustomCCTabControl.InternalSetMultiLine(Value: Boolean): Boolean;
begin
  Result := FMultiLine <> Value;
  if Result then
  begin
    if not Value and ((TabPosition = tpLeft) or (TabPosition = tpRight)) then
      TabControlError(sTabMustBeMultiLine);
    FMultiLine := Value;
    if not Value then FScrollOpposite := False;
  end;
end;

procedure TrmCustomCCTabControl.SetMultiLine(Value: Boolean);
begin
  if InternalSetMultiLine(Value) then RecreateWnd;
end;

procedure TrmCustomCCTabControl.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.SetRaggedRight(Value: Boolean);
begin
  if FRaggedRight <> Value then
  begin
    FRaggedRight := Value;
    SetComCtlStyle(Self, TCS_RAGGEDRIGHT, Value);
  end;
end;

procedure TrmCustomCCTabControl.SetScrollOpposite(Value: Boolean);
begin
  if FScrollOpposite <> Value then
  begin
    FScrollOpposite := Value;
    if Value then FMultiLine := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.SetStyle(Value: TTabStyle);
begin
  if FStyle <> Value then
  begin
    if (Value <> tsTabs) and (TabPosition <> tpTop) then
      raise EInvalidOperation.Create(SInvalidTabStyle);
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.SetTabHeight(Value: Smallint);
begin
  if FTabSize.Y <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    FTabSize.Y := Value;
    UpdateTabSize;
  end;
end;

procedure TrmCustomCCTabControl.SetTabIndex(Value: Integer);
begin
  SendMessage(Handle, TCM_SETCURSEL, Value, 0);
end;

procedure TrmCustomCCTabControl.SetTabPosition(Value: TTabPosition);
const
  AlignStyles: array[TTabPosition] of Integer =
    (0, TCS_BOTTOM, TCS_VERTICAL, TCS_VERTICAL or TCS_RIGHT);
begin
  if FTabPosition <> Value then
  begin
    if (Value <> tpTop) and (Style <> tsTabs) then
      raise EInvalidOperation.Create(SInvalidTabPosition);
    FTabPosition := Value;
    if not MultiLine and ((Value = tpLeft) or (Value = tpRight)) then
      InternalSetMultiLine(True);
    RecreateWnd;
  end;
end;

procedure TrmCustomCCTabControl.SetTabs(Value: TStrings);
begin
  FTabs.Assign(Value);
end;

procedure TrmCustomCCTabControl.SetTabWidth(Value: Smallint);
var
  OldValue: Smallint;
begin
  if FTabSize.X <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    OldValue := FTabSize.X;
    FTabSize.X := Value;
    if (OldValue = 0) or (Value = 0) then RecreateWnd
    else UpdateTabSize;
  end;
end;

procedure TrmCustomCCTabControl.TabsChanged;
begin
  if not FUpdating then
  begin
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, SIZE_RESTORED,
        Word(Width) or Word(Height) shl 16);
    Realign;
  end;
end;

procedure TrmCustomCCTabControl.UpdateTabSize;
begin
  SendMessage(Handle, TCM_SETITEMSIZE, 0, Integer(FTabSize));
  TabsChanged;
end;

procedure TrmCustomCCTabControl.UpdateTabImages;
var
  I: Integer;
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_IMAGE;
  for I := 0 to FTabs.Count - 1 do
  begin
    TCItem.iImage := GetImageIndex(I);
    if SendMessage(Handle, TCM_SETITEM, I,
      Longint(@TCItem)) = 0 then
      TabControlError(Format(sTabFailSet, [FTabs[I], I]));
  end;
  TabsChanged;
end;

procedure TrmCustomCCTabControl.CNDrawItem(var Message: TWMDrawItem);
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

procedure TrmCustomCCTabControl.WMDestroy(var Message: TWMDestroy);
var
  FocusHandle: HWnd;
begin
  FocusHandle := GetFocus;
  if (FocusHandle <> 0) and ((FocusHandle = Handle) or
    IsChild(Handle, FocusHandle)) then
    Windows.SetFocus(0);
  inherited;
end;

procedure TrmCustomCCTabControl.WMNotifyFormat(var Message: TMessage);
begin
  with Message do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TrmCustomCCTabControl.WMSize(var Message: TMessage);
begin
  inherited;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
end;

procedure TrmCustomCCTabControl.CMFontChanged(var Message);
begin
  inherited;
  if HandleAllocated then Perform(WM_SIZE, 0, 0);
end;

procedure TrmCustomCCTabControl.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Message.Msg := WM_SYSCOLORCHANGE;
    DefaultHandler(Message);
  end;
end;

procedure TrmCustomCCTabControl.CMTabStopChanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then RecreateWnd;
end;

procedure TrmCustomCCTabControl.CNNotify(var Message: TWMNotify);
begin
  with Message do
    case NMHdr^.code of
      TCN_SELCHANGE:
        Change;
      TCN_SELCHANGING:
        begin
          Result := 1;
          if CanChange then Result := 0;
        end;
    end;
end;

procedure TrmCustomCCTabControl.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
    if IsAccel(Message.CharCode, FTabs[I]) and CanShowTab(I) and CanFocus then
    begin
      TabIndex := I;
      Message.Result := 1;
      Change;
      Exit;
    end;
  inherited;
end;

procedure TrmCustomCCTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := DisplayRect;
  inherited AdjustClientRect(Rect);
end;

procedure TrmCustomCCTabControl.CMMouseLeave(var message: tmessage);
var
   oldtab:trect;
begin
     inherited;
     if (hottrack) and not (csdesigning in componentstate) then
     begin
          sendmessage(handle,TCM_GetItemRect, fmouseovertab, longint(@OldTab));
          InvalidateRect(handle,@OldTab,false);
          fmouseovertab := -1;
     end;
end;

procedure TrmCustomCCTabControl.WMNCHitTest(var message: TWMNCHitTest);
var
   HitTest : TTCHitTestInfo;
   result : integer;
   OldTab, NewTab : trect;
begin
     inherited;
     if not (csdesigning in componentstate) then
     begin
          HitTest.pt := screentoclient(point(message.XPos,message.ypos));
          HitTest.flags := TCHT_ONITEM;
          result :=  sendmessage(handle,TCM_HITTEST,0,longint(@HitTest));
          if (result <> fmouseovertab) then
          begin
               if assigned(fontabtrack) then
                  fontabtrack(self,result);
               DisplayTabHint(result);
               if (hottrack) then
               begin
                    sendmessage(handle,TCM_GetItemRect, fmouseovertab, longint(@OldTab));
                    sendmessage(handle,TCM_GetItemRect, result, longint(@NewTab));
                    InvalidateRect(handle,@OldTab,false);
                    InvalidateRect(handle,@NewTab,false);
               end;
               fmouseovertab := result;
          end;
     end
     else
     fmouseovertab := -1;
end;

function TrmCustomCCTabControl.GetTabAt(x,y:integer):integer;
var
   HitTest : TTCHitTestInfo;
begin
     HitTest.pt := point(x,y);
     HitTest.flags := TCHT_ONITEM;
     result :=  sendmessage(handle,TCM_HITTEST,0,longint(@HitTest));
end;

{ TrmCCTabControl }

procedure TrmCCTabControl.DisplayTabHint(TabIndex: integer);
begin
     application.CancelHint;
     if (tabindex > -1) and (tabindex < tabhints.Count)  then
        hint := trim(tabhints.strings[tabindex]);
end;

procedure TrmCCTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseOverTab = TabIndex) then
        fMouseDragTab := fMouseOverTab;
     Inherited;
end;

procedure TrmCCTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) then
     begin
          if (ssLeft in Shift) then
          begin
               if (fMouseOverTab = -1) then
                  Cursor := crNo
               else
               if (fMouseDragTab <> fMouseOverTab) then
                  Cursor := crDrag
               else
                  Cursor := crDefault;
          end
          else
          Cursor := crDefault;
     end;
     inherited;
end;

procedure TrmCCTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseDragTab <> fMouseOverTab) and (fMouseOverTab <> -1) then
     begin
          Tabs.Move(fMouseDragTab,fMouseOverTab);
          Cursor := crDefault;
          if assigned(fOnTabShift) then fOnTabShift(self);
     end;
     inherited;
end;

procedure TrmCCTabControl.SetTabHints(value: TStrings);
begin
     ftabhints.assign(value);
end;

{ TrmCCTabsFloatingForm }

constructor TrmCCTabsFloatingForm.CreateNew(AOwner: TComponent);
begin
   inherited CreateNew(AOwner);
   OnClose := DoCloseWindow;
   OnDestroy := DoDestroyWindow;     
end;

procedure TrmCCTabsFloatingForm.DoCloseWindow(Sender: TObject; var Action: TCloseAction);
begin
     if assigned(fsheet) then fsheet.docktabsheet;
     action := cafree;
end;

procedure TrmCCTabsFloatingForm.DoDestroyWindow(Sender: TObject);
begin
     fsheet.ffloatingform := nil;
end;

{ TTabSheet }

constructor TrmCCTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  FTabVisible := True;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  fpageindex := -1;
  fdragstart := false;
  fDragging := false;
  GripAlign := gaLeft;
end;

destructor TrmCCTabSheet.Destroy;
begin
  fcanvas.free;
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TrmCCTabSheet.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TrmCCTabSheet.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

function TrmCCTabSheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf(Self) else
    Result := -1;
end;

function TrmCCTabSheet.GetTabIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not FTabShowing then Dec(Result) else
    for I := 0 to PageIndex - 1 do
      if TrmCCTabSheet(FPageControl.FPages[I]).FTabShowing then
        Inc(Result);
end;

procedure TrmCCTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TrmCCTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TrmCCPageControl then
    PageControl := TrmCCPageControl(Reader.Parent);
end;

procedure TrmCCTabSheet.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FTabShowing then FPageControl.UpdateTab(Self);
  end;
end;

procedure TrmCCTabSheet.SetPageControl(APageControl: TrmCCPageControl);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then FPageControl.RemovePage(Self);
    Parent := APageControl;
    if APageControl <> nil then APageControl.InsertPage(Self);
  end;
end;

procedure TrmCCTabSheet.SetPageIndex(Value: Integer);
var
  I, MaxPageIndex: Integer;
begin
  if FPageControl <> nil then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt(SPageIndexError, [Value, MaxPageIndex]);
    I := TabIndex;
    FPageControl.FPages.Move(PageIndex, Value);
    if I >= 0 then FPageControl.MoveTab(I, TabIndex);
  end;
end;

procedure TrmCCTabSheet.SetTabShowing(Value: Boolean);
var
  Index: Integer;
begin
  if FTabShowing <> Value then
    if Value then
    begin
      FTabShowing := True;
      FPageControl.InsertTab(Self);
    end else
    begin
      Index := TabIndex;
      FTabShowing := False;
      FPageControl.DeleteTab(Self, Index);
    end;
end;

procedure TrmCCTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;
  end;
end;

procedure TrmCCTabSheet.UpdateTabShowing;
begin
  SetTabShowing((FPageControl <> nil) and
    (FTabVisible or (csDesigning in ComponentState)));
end;

procedure TrmCCTabSheet.CMTextChanged(var Message: TMessage);
begin
  if FTabShowing then FPageControl.UpdateTab(Self);
end;

procedure TrmCCTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end else if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TrmCCTabSheet.DrawDraggingRect(MousePos : TPoint);
var
  DC             : hDC;      { device context for the window       }
  Canvas         : TCanvas;  { canvas to draw dragging rect        }
  AdjustedRect   : TRect;    { fDragRect adjusted for MousePos     }
  ScreenPos      : TPoint;   { screen-relative version of MousePos }

begin
  DC := GetWindowDc(GetDesktopWindow);
  if DC <> 0 then begin
    ScreenPos := ClientToScreen(MousePos);
    with AdjustedRect do begin
      Left := ScreenPos.X-fMouseOffset.X;
      Top := ScreenPos.Y-fMouseOffset.Y;
      Right := Left+fWidth;
      Bottom := Top+fHeight;
    end; { with AdjustedRect do }
    fDragRect := AdjustedRect;
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    Canvas.DrawFocusRect(AdjustedRect);
    Canvas.Free;
  end else
    Raise ECCTabError.Create('Error retreiving DC(0)');
end;

procedure TrmCCTabSheet.WMLBUTTONDOWN(var Msg : TWMLButtonDown);
begin
     inherited;

     if ffloating then exit;

     if (fDragable) and (ptinrect(GripRect, point(msg.pos.x,msg.pos.y))) then
     begin
          fDragStart := TRUE;

          fWidth := width;
          fHeight := height;

          fOldMousePos := Point(Msg.Pos.x,Msg.Pos.y);
          fMouseOffset := fOldMousePos;
     end;
end;

procedure TrmCCTabSheet.WMMOUSEMOVE(var Msg : TWMMouseMove);
begin
     inherited;
     if (fDragStart) and ((abs(foldmousepos.x - msg.pos.x) > 3) or (abs(foldmousepos.y - msg.pos.y) > 3)) then
     begin
          fdragging := true;
          fDragStart := false;
          DrawDraggingRect(fOldMousePos);
     end;
     if fDragging then
     begin
          DrawDraggingRect(fOldMousePos);
          fOldMousePos := Point(Msg.Pos.x,Msg.Pos.y);
          DrawDraggingRect(fOldMousePos);
     end;
end;

procedure TrmCCTabSheet.WMLBUTTONUP(var Msg : TWMLButtonDown);
begin
     inherited;
     if fDragging then
     begin
          DrawDraggingRect(fOldMousePos);
          FloatTabSheet;
          fDragging := FALSE;
     end; { if dragging }
     if fDragStart then
     begin
          fDragStart := false;
          invalidate;
     end;
end;

procedure TrmCCTabSheet.FloatTabSheet;
var
   ScreenPos: TPoint;
begin
     if (not FFloating) then
     begin
        if not fDragging then
        begin
           ScreenPos := ClientToScreen(Point(Left, Top));
           with fDragRect do begin
              Left := ScreenPos.X;
              Top := ScreenPos.Y;
              fWidth := ClientRect.Right;
              fHeight := ClientRect.Bottom;
              Right := Left + fWidth;
              Bottom := Top + fHeight;
           end;
        end;

        fOldPageControl := PageControl;
        PageControl := nil;

        FFloating := true;

        fOldPageControl.SelectNextPage(True);

        fOldPageControl.AddToFloatList(self);

        fOldPageControl.Perform(CM_rmCCTabSheetDraggedOFF,0,0);

        if not assigned(fFloatingForm) then
        begin
           fFloatingForm := TrmCCTabsFloatingForm.CreateNew(self.owner);
           if (fFloatOnTop) and (Application.mainform <> nil) and (Application.mainform is tform) then
              setwindowlong(ffloatingform.handle,gwl_hwndparent,Application.mainform.handle);
           if assigned(fOldPageControl.images) and (imageindex <> -1) then
              fOldPageControl.Images.GetIcon(imageindex,ffloatingform.Icon);

           fFloatingForm.OnMoveSize := DoMoveSize;
           
           fFloatingForm.Caption := Caption;
           fFloatingForm.ClientWidth := fWidth;
           fFloatingForm.ClientHeight := fHeight;
           fFloatingForm.TabSheet := self;
        end;

        fFloatingForm.Left := fDragRect.Left;
        fFloatingForm.Top := fDragRect.Top;

        if assigned(FOnFloatChange) then
           FOnFloatChange(Self, fsFloating);

        Parent := fFloatingForm;
        fFloatingForm.Show;
        Show;
   end;
end;

procedure TrmCCTabSheet.DockTabSheet;
begin
   if FFloating then
   begin
      fFloatingForm.Hide;
      FFloating := false;

      PageControl:= fOldPageControl;

      PageControl.RemoveFromFloatList(self);

      PageControl.Perform(CM_rmCCTabSheetDraggedON,0,Integer(@Self));
      PageControl.ActivePage := Self;
      foldPageControl := nil;

      if assigned(FOnFloatChange) then
         FOnFloatChange(Self, fsDocked);
   end;
end;

function TrmCCTabSheet.GetClientRect:TRect;
var
   clientrect : trect;
begin
     clientrect := inherited GetClientRect;
     if (not dragable) or (ffloating) then
        result := clientrect
     else
     begin
          case gripalign of
               gaLeft: clientrect.Left := clientrect.Left + GripSize;
               gaRight: clientrect.right := clientrect.right - GripSize;
               gaTop: clientrect.Top := clientrect.top + GripSize;
               gaBottom: clientrect.bottom := clientrect.bottom - GripSize;
          end;
          result := clientrect;
     end;
end;

procedure TrmCCTabSheet.setDragOption(value:boolean);
begin
     if value <> fdragable then
     begin
          fDragable := value;
          realign;
          invalidate;
     end;
end;

procedure TrmCCTabSheet.setGripAlign(value:TGripAlign);
begin
     fGripAlign := value;
     realign;
     invalidate;
end;

function TrmCCTabSheet.GetGripRect:TRect;
begin
     result := Rect(0,0,0,0);
     if (dragable) and (not fFloating) then
     case GripAlign of
          gaLeft: result := rect(0,0,GripSize,height);
          gaRight: result := rect(width-GripSize,0,width,height);
          gaTop: result := rect(0,0,width,GripSize);
          gaBottom: result := rect(0,height-gripsize,width,height);
     end;
end;

function TrmCCTabSheet.GetGripperRect:TRect;
begin
     result := Rect(0,0,0,0);
     if (dragable) and (not fFloating) then
     case GripAlign of
          gaLeft: result := rect(0,0,GripSize,20);
          gaRight: result := rect(width-GripSize,height-20,width,height);
          gaTop: result := rect(width-20,0,width,GripSize);
          gaBottom: result := rect(0,height-gripsize,20,height);
     end;
end;

procedure TrmCCTabSheet.WMPaint(var msg:TWMPaint);
const
     xpos = 4;
var
   loop : integer;
   workcolor : tcolor;
   position : integer;
   ypos : integer;
begin
     inherited;
     if (dragable) and not (FFloating) then
     with fcanvas do
     begin
          brush.color := clbtnface;
          brush.style := bsSolid;
          fillrect(GetGripRect);
          if enabled then
            workcolor := clactivecaption
          else
            workcolor := clinactivecaption;

          ypos := 0;
          if gripalign = gabottom then ypos := (height-gripsize)+2;
          if gripalign = garight then ypos := (width-gripsize)+2;
          if (gripalign in [gabottom, gaTop]) then
          for loop := 0 to 4 do
          begin
               if gripalign = gaTop then position := (width - (xpos * loop))-4
               else
               position := xpos * loop;

               pixels[position,ypos] := clbtnhighlight;
               pixels[1+position,ypos] := workcolor;
               pixels[position,ypos+1] := workcolor;
               pixels[1+position,ypos+1] := workcolor;

               pixels[2+position,ypos+3] := clbtnhighlight;
               pixels[3+position,ypos+3] := workcolor;
               pixels[2+position,ypos+4] := workcolor;
               pixels[3+position,ypos+4] := workcolor;
          end;

          if (gripalign = gaLeft) or (gripalign = gaRight)  then
          for loop := 0 to 4 do
          begin
               if gripalign = gaRight then position := (height - (xpos * loop))-4
               else
               position := xpos*loop;

               pixels[ypos,position] := clbtnhighlight;
               pixels[ypos,1+position] := workcolor;
               pixels[ypos+1,position] := workcolor;
               pixels[ypos+1,1+position] := workcolor;

               pixels[ypos+3,2+position] := clbtnhighlight;
               pixels[ypos+3,3+position] := workcolor;
               pixels[ypos+4,2+position] := workcolor;
               pixels[ypos+4,3+position] := workcolor;
          end;
     end;
end;

procedure TrmCCTabSheet.SetFloatOnTop(const Value: boolean);
begin
     if FFloatOnTop <> Value then
     begin
          FFloatOnTop := Value;
          if Floating then
          begin
               if Value then
               if (Application.MainForm <> nil) and (Application.MainForm is TForm) then
                  setwindowlong(FFloatingForm.Handle,gwl_hwndparent,Application.MainForm.Handle)
               else
                  setwindowlong(FFloatingForm.Handle,gwl_hwndparent,0);
          end
     end;
end;

procedure TrmCCTabsFloatingForm.wmExitSizeMove(var msg: TMessage);
begin
   inherited;  
   if assigned(fMoveSize) then
      fMoveSize(self);
end;

{ TrmCCPageControl }

constructor TrmCCPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDoubleClicks, csOpaque];
  FPages := TList.Create;
  FFloatingPages := TList.create;
end;

destructor TrmCCPageControl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do TrmCCTabSheet(FPages[I]).FPageControl := nil;
  FPages.Free;

  for I := 0 to FFloatingPages.Count - 1 do TrmCCTabSheet(FFloatingPages[I]).FPageControl := nil;
  FFloatingPages.free;
  inherited Destroy;
end;

function TrmCCPageControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := TrmCCTabSheet(FPages[TabIndex]).Enabled;
end;

procedure TrmCCPageControl.Change;
var
  Form: TCustomForm;
begin
  UpdateActivePage;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
  end;
  inherited Change;
end;

procedure TrmCCPageControl.ChangeActivePage(Page: TrmCCTabSheet);
var
  ParentForm: TCustomForm;
begin
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        Exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page else
          ParentForm.ActiveControl := Self;
    end;
    if FActivePage <> nil then FActivePage.Visible := False;
    FActivePage := Page;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
  end;
end;

procedure TrmCCPageControl.DeleteTab(Page: TrmCCTabSheet; Index: Integer);
var
  UpdateIndex: Boolean;
begin
  UpdateIndex := Page = ActivePage;
  Tabs.Delete(Index);
  if UpdateIndex then
  begin
    if Index >= Tabs.Count then
      Index := Tabs.Count - 1;
    TabIndex := Index;
  end;
  UpdateActivePage;
end;

procedure TrmCCPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then Client.Parent := FNewDockSheet;
end;

procedure TrmCCPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TrmCCPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TrmCCPageControl.FindNextPage(CurPage: TrmCCTabSheet;
  GoForward, CheckTabVisible: Boolean): TrmCCTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then StartIndex := FPages.Count - 1 else StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then I := 0;
      end else
      begin
        if I = 0 then I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      if not CheckTabVisible or Result.TabVisible then Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TrmCCPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do Proc(TComponent(FPages[I]));
end;

function TrmCCPageControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  if Assigned(FOnGetImageIndex) then
    Result := inherited GetImageIndex(TabIndex) else
    Result := GetPage(TabIndex).ImageIndex;
end;

function TrmCCPageControl.GetPageFromDockClient(Client: TControl): TrmCCTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TrmCCPageControl.GetPage(Index: Integer): TrmCCTabSheet;
begin
  Result := FPages[Index];
end;

function TrmCCPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TrmCCPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TrmCCPageControl.InsertPage(Page: TrmCCTabSheet);
begin
  FPages.Add(Page);
  Page.FPageControl := Self;
  Page.UpdateTabShowing;
end;

procedure TrmCCPageControl.InsertTab(Page: TrmCCTabSheet);
begin
  Tabs.InsertObject(Page.TabIndex, Page.Caption, Page);
  UpdateActivePage;
end;

procedure TrmCCPageControl.MoveTab(CurIndex, NewIndex: Integer);
begin
  Tabs.Move(CurIndex, NewIndex);
end;

procedure TrmCCPageControl.RemovePage(Page: TrmCCTabSheet);
begin
  Page.SetTabShowing(False);
  Page.FPageControl := nil;
  FPages.Remove(Page);
end;

procedure TrmCCPageControl.SelectNextPage(GoForward: Boolean);
var
  Page: TrmCCTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, True);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    TabIndex := Page.TabIndex;
    Change;
  end;
end;

procedure TrmCCPageControl.SetActivePage(Page: TrmCCTabSheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then Exit;
  ChangeActivePage(Page);
  if Page = nil then
    TabIndex := -1
  else if Page = FActivePage then
    TabIndex := Page.TabIndex;
end;

procedure TrmCCPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TrmCCTabSheet(Child).PageIndex := Order;
end;

procedure TrmCCPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TrmCCTabSheet) and (TrmCCTabSheet(AControl).PageControl = Self) then
    SetActivePage(TrmCCTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TrmCCPageControl.UpdateTab(Page: TrmCCTabSheet);
begin
  Tabs[Page.TabIndex] := Page.Caption;
end;

procedure TrmCCPageControl.UpdateActivePage;
begin
  if TabIndex >= 0 then
    SetActivePage(TrmCCTabSheet(Tabs.Objects[TabIndex]))
  else
    SetActivePage(nil);
end;

procedure TrmCCPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
begin
  HitTestInfo.pt := SmallPointToPoint(Message.Pos);
  HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
  if (HitIndex >= 0) and (HitIndex <> TabIndex) then Message.Result := 1;
end;

procedure TrmCCPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited;
end;

procedure TrmCCPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
begin
  Message.Result := 0;
  FNewDockSheet := TrmCCTabSheet.Create(Self);
  try
    try
      DockCtl := Message.DockSource.Control;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      FNewDockSheet.PageControl := Self;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewDockSheet.TabVisible := IsVisible;
    if IsVisible then ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;

procedure TrmCCPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  Page: TrmCCTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        Page.Caption := PChar(Message.NotifyRec.MsgLParam);
      CM_VISIBLECHANGED:
        with Page do
        begin
          Visible := Boolean(Message.NotifyRec.MsgWParam);
          TabVisible := Boolean(Message.NotifyRec.MsgWParam);;
        end;
    end;
  inherited;
end;

procedure TrmCCPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TrmCCTabSheet;
begin
  Message.Result := 0;
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Message.Client.Align := alNone;
  end;
end;

function TrmCCPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
  Page: TrmCCTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    HitTestInfo.pt := MousePos;
    HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
    if HitIndex >= 0 then
    begin
      Page := Pages[HitIndex];
      if not Page.TabVisible then Page := FindNextPage(Page, True, True);
      if (Page <> nil) and (Page.ControlCount > 0) then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then Result := nil;
      end;
    end;
  end;
end;

procedure TrmCCPageControl.WMLButtonDown(var Message: TWMLButtonDown);
var
  DockCtl: TControl;
begin
  inherited;
  DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
  if DockCtl <> nil then DockCtl.BeginDrag(False);
end;

procedure TrmCCPageControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  DockCtl: TControl;
begin
  inherited;
  DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
  if DockCtl <> nil then DockCtl.ManualDock(nil, nil, alNone);
end;

procedure TrmCCPageControl.CMTabDraggedOff(var Message: TMessage);
begin
     if assigned(FOnFloatChange) then
        FOnFloatChange(Self, fsFloating);
end;

procedure TrmCCPageControl.CMTabDraggedOn(var Message: TMessage);
var
   loop, loop1 : integer;
   worksheet : TrmCCTabSheet;
begin
     for loop :=  0 to fpages.count-1 do
     begin
          worksheet := TrmCCTabSheet(fpages[loop]);
          loop1 := 0;
          while (loop1 < fpages.count - 1) and (worksheet.StaticPageIndex > TrmCCTabSheet(fpages[loop1]).staticpageindex) do
                inc(loop1);
          worksheet.pageindex := loop1;
          if worksheet.TabVisible then
             worksheet.imageindex := worksheet.imageindex; 
     end;
     if assigned(FOnFloatChange) then
        FOnFloatChange(Self, fsDocked);
end;

procedure TrmCCPageControl.DisplayTabHint(TabIndex: integer);
begin
     application.CancelHint;
     if tabindex <> -1 then
        hint := trim(TrmCCTabSheet(fpages.items[tabindex]).tabhint)
     else
     begin
        if assigned(activepage) then
           hint := trim(TrmCCTabSheet(activepage).hint);
     end;
end;

procedure TrmCCPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseOverTab = ActivePage.PageIndex) then
        fMouseDragTab := fMouseOverTab;
     Inherited;
end;

procedure TrmCCPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) then
     begin
          if (ssLeft in Shift) then
          begin
               if (fMouseOverTab = -1) then
                  Cursor := crNo
               else
               if (fMouseDragTab <> fMouseOverTab) then
                  Cursor := crDrag
               else
                  Cursor := crDefault;
          end
          else
          Cursor := crDefault;
     end;
     inherited;
end;

procedure TrmCCPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
   tabsheet1 : TrmCCTabSheet;
begin
     if (ftabshifting) and (button = mbleft) and (fMouseDragTab <> fMouseOverTab) and (fMouseOverTab <> -1) then
     begin
          TabSheet1 := Pages[fMouseDragTab];
          TabSheet1.PageIndex := fMouseOverTab;
          Cursor := crDefault;
          if assigned(fOnTabShift) then fOnTabShift(self);
     end;
     inherited;
end;

function TrmCCPageControl.GetFloatingPage(Index: Integer): TrmCCTabSheet;
begin
   Result := FFloatingPages[Index];
end;

function TrmCCPageControl.GetFloatingPageCount: Integer;
begin
   result := FFloatingPages.Count;
end;

procedure TrmCCPageControl.AddToFloatList(Page: TrmCCTabSheet);
begin
   if fFloatingPages.IndexOf(page) = -1 then
      fFloatingPages.add(Page);
end;

procedure TrmCCPageControl.RemoveFromFloatList(Page: TrmCCTabSheet);
var
   index : integer;
begin
   index := fFloatingPages.IndexOf(page);
   if index <> -1 then
      fFloatingPages.delete(index);
end;

procedure TrmCCTabSheet.FloatTabSheetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
     if (not FFloating) then
     begin
        fOldPageControl := PageControl;
        PageControl := nil;

        FFloating := true;

        fOldPageControl.SelectNextPage(True);

        fOldPageControl.AddToFloatList(self);

        fOldPageControl.Perform(CM_rmCCTabSheetDraggedOFF,0,0);

        if not assigned(fFloatingForm) then
        begin
           fFloatingForm := TrmCCTabsFloatingForm.CreateNew(self.owner);
           if (fFloatOnTop) and (Application.mainform <> nil) and (Application.mainform is tform) then
              setwindowlong(ffloatingform.handle,gwl_hwndparent,Application.mainform.handle);
           if assigned(fOldPageControl.images) and (imageindex <> -1) then
              fOldPageControl.Images.GetIcon(imageindex,ffloatingform.Icon);
           fFloatingForm.OnMoveSize := DoMoveSize;
           fFloatingForm.Caption := Caption;
           fFloatingForm.SetBounds(aleft, atop, awidth, aheight);
           fFloatingForm.TabSheet := self;
        end;

        if assigned(FOnFloatChange) then
           FOnFloatChange(Self, fsFloating);

        Parent := fFloatingForm;
        fFloatingForm.Show;
        Show;
   end;
end;

procedure TrmCCTabSheet.DoMoveSize(Sender: TObject);
begin
   if assigned(fMoveSize) then
      fMoveSize(self);
end;

procedure TrmCCPageControl.HideFloatingPages;
var
   loop : integer;
begin
   loop := GetFloatingPageCount;
   while loop > 0 do
   begin
      dec(loop);
      GetFloatingPage(loop).FloatingForm.Hide;
   end;
end;

procedure TrmCCPageControl.ShowFloatingPages;
var
   loop : integer;
begin
   loop := GetFloatingPageCount;
   while loop > 0 do
   begin
      dec(loop);
      GetFloatingPage(loop).FloatingForm.show;
   end;
end;

function TrmCCPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage <> nil then
    Result := ActivePage.GetPageIndex
  else
    Result := -1;
end;

procedure TrmCCPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value > -1) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

end.
