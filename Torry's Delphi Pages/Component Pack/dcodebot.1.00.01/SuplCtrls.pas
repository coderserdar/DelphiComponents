
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit SuplCtrls;

interface

{$I STD.INC}

uses
  Classes, Controls, Windows, Menus, Forms, Messages, SysUtils,
  StdCtrls, Graphics, GraphTools, StrTools, ComCtrls, TypInfo, Buttons,
  ExtCtrls, DB, Dialogs, ImgList, MathTools, FormTools, BtnCtrls;

{ TCustomCaptionBox }

const
  WM_CHILDFOCUS       = WM_USER + $0AC1A;
  WM_CHILDKILLFOCUS   = WM_CHILDFOCUS + 1;

type
  TCaptionStyle = (csClose, csStick);

  TCustomCaptionBox = class(TCustomControl)
  private
    FActive: Boolean;
    FBorderStyle: TBorderStyle;
    FButton: TThemeGlyphButton;
    FCaptionHeight: Integer;
    FPanel: TPanel;
    Flabel: TLabel;
    FStyle: TCaptionStyle;
    FOnClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnStick: TNotifyEvent;
    procedure MouseActivate;
    procedure ButtonClick(Sender: TObject);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaptionHeight(Value: Integer);
    function GetCloseButton: Boolean;
    procedure SetCloseButton(Value: Boolean);
    procedure SetStyle(Value: TCaptionStyle);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMChildFocus(var Message: TMessage); message WM_CHILDFOCUS;
    procedure WMChildKillFocus(var Message: TMessage); message WM_CHILDKILLFOCUS;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    procedure CloseQuery;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure UpdateControls;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight;
    property CloseButton: Boolean read GetCloseButton write SetCloseButton;
    property Style: TCaptionStyle read FStyle write SetStyle;
    property Active: Boolean read FACtive write SetActive;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write
      FOnCloseQuery;
    property OnStick: TNotifyEvent read FOnStick write FOnStick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close;
    procedure Stick;
  end;

{ TCaptionBox }

  TCaptionBox = class(TCustomCaptionBox)
  published
    property Align;
    property BorderStyle;
    property Caption;
    property CloseButton;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Style;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClose;
    property OnCloseQuery;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStick;
  end;

{ TExpandableBox }

  TExpandableBox = class(TCustomControl)
  private
    FAllowCollapse: Boolean;
    FBoxClick: Boolean;
    FCaptionRect: TRect;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    FNodeDown: Boolean;
    FNodePressed: Boolean;
    FVisibleData: TVisibleDataArray;
    FOnCollapse: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    function GetCaptionRect: TRect;
    function GetNodeRect: TRect;
    procedure SetAllowCollapse(Value: Boolean);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
		procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    function GetClientRect: TRect; override;
    procedure Resize; override;
    property CaptionRect: TRect read GetCaptionRect;
    property NodeRect: TRect read GetNodeRect;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property AllowCollapse: Boolean read FAllowCollapse write SetAllowCollapse;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
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
    property OnClick;
    property OnContextPopup;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TLineGrid }

  TIntegerArray = array of Integer;

  TLineGrid = class(TCustomControl)
  private
    FAmplitude: Double;
    FBorderStyle: TBorderStyle;
    FGridSize: Integer;
    FGridColor: TColor;
    FLog: TIntegerArray;
    FPen: TPen;
    FVelocity: Double;
    procedure StyleChanged(Sender: TObject);
    procedure SetAmplitude(const Value: Double);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetGridColor(Value: TColor);
    procedure SetGridSize(Value: Integer);
    procedure SetPen(Value: TPen);
    procedure SetVelocity(const Value: Double);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(Value: Integer);
  published
    property Align;
    property Amplitude: Double read FAmplitude write SetAmplitude;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GridColor: TColor read FGridColor write SetGridColor default clGreen;
    property GridSize: Integer read FGridSize write SetGridSize default 12;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Pen: TPen read FPen write SetPen;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Velocity: Double read FVelocity write SetVelocity;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TIntegerEdit }

  TIntegerEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FDisplayZero: Boolean;
    FIntValue: Integer;
    FMax: Integer;
    FMin: Integer;
    FOnCalculateRect: TCalculateRectEvent;
    procedure SetIntValue(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  published
    property Anchors;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DisplayZero: Boolean read FDisplayZero write FDisplayZero;
    property Enabled;
    property Font;
    property HideSelection;
    property IntValue: Integer read FIntValue write SetIntValue;
    property MaxLength;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCalculateRect: TCalculateRectEvent read FOnCalculateRect
      write FOnCalculateRect;
    property OnChange;
    property OnClick;
    property OnContextPopup;
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
  end;

{ TMoneyEdit }

  TMoneyEditOption = (moDollarSign, moForceAmount);
  TMoneyEditOptions = set of TMoneyEditOption;

  TMoneyEdit = class(TCustomEdit)
  private
    FAmount: Double;
    FDecimals: Integer;
    FOptions: TMoneyEditOptions;
    FRevertText: string;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetAmountText: string;
    procedure SetAmount(const Value: Double);
    procedure SetDecimals(Value: Integer);
    procedure SetOptions(Value: TMoneyEditOptions);
  protected
    property AmountText: string read GetAmountText;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Amount: Double read FAmount write SetAmount;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property Decimals: Integer read FDecimals write SetDecimals default 2;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property Options: TMoneyEditOptions read FOptions write SetOptions;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
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
  end;

  TCustomDBDrawList = class(TCustomControl)
  private
    FActive: Boolean;
    FBorderStyle: TBorderStyle;
    FDataLink: TDataLink;
    FDisplayWidth: Integer;
    FColumnCount: Integer;
    FInternalFocused: Boolean;
    FItemHeight: Integer;
    FRecordCount: Integer;
    FRecordIndex: Integer;
    FTracking: Boolean;
    FScrollPos: Integer;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure DataLinkChanged;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDisplayWidth(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DrawItem(const Rect: TRect; Index: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure Paint; override;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ColumnCount: Integer read FColumnCount;
    property DisplayWidth: Integer read FDisplayWidth write SetDisplayWidth;
    property RecordIndex: Integer read FRecordIndex;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property InternalFocused: Boolean read FInternalFocused;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 18;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TDrawItemEvent read FOnDrawitem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TDBDrawList }

  TDBDrawList = class(TCustomDBDrawList)
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataSource;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDrawBackground;
    property OnDrawItem;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TReportViewColumn }

  TCustomDBReportView = class;

  TReportViewColumn = class(TCollectionItem)
  private
    FSection: THeaderSection;
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetText: string;
    procedure SetText(Value: string);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Text: string read GetText write SetText;
    property Width: Integer read GetWidth write SetWidth;
  end;

{ TReportViewColumns }

  TReportViewColumns = class(TCollection)
  private
    FReportView: TCustomDBReportView;
    function GetItem(Index: Integer): TReportViewColumn;
    procedure SetItem(Index: Integer; Value: TReportViewColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ReportView: TCustomDBReportView);
    function Add: TReportViewColumn;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    property Items[Index: Integer]: TReportViewColumn read GetItem write SetItem; default;
  end;

{ TCustomDBReportView }

  TReportViewStyle = (rsColumn, rsGrid);

  TColumnClickEvent = procedure (Sender: TObject; Column: TReportViewColumn)
    of object;
  TCalculateImageIndexEvent = procedure (Sender: TObject;
    var ImageIndex: Integer)  of object;
  TCalculateTextEvent = procedure (Sender: TObject; Index: Integer;
    var Text: string) of object;

  TCustomDBReportView = class(TWinControl)
  private
    FBorderStyle: TBorderStyle;
    FColumns: TReportViewColumns;
    FDefaultDraw: Boolean;
    FDrawList: TDBDrawList;
    FForwarding: Boolean;
    FHeaderControl: THeaderControl;
    FImages: TCustomImageList;
    FDefDrawListProc: TWndMethod;
    FOnCalculateImageIndex: TCalculateImageIndexEvent;
    FOnCalculateText: TCalculateTextEvent;
    FOnColumnClick: TColumnClickEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetDefaultDraw(Value: Boolean);
    procedure SetColumns(Value: TReportViewColumns);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetImages(Value: TCustomImageList);
    function GetFlat: Boolean;
    procedure SetFlat(Value: Boolean);
    function GetStyle: TReportViewStyle;
    procedure SetStyle(Value: TReportViewStyle);
    procedure DrawListProc(var Message: TMessage);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure DoSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure DoSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure WndProc(var Message: TMessage); override;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultDraw: Boolean read FDefaultDraw write SetDefaultDraw default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property Columns: TReportViewColumns read FColumns write SetColumns;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Style: TReportViewStyle read GetStyle write SetStyle default rsColumn;
    property OnCalculateImageIndex: TCalculateImageIndexEvent read
      FOnCalculateImageIndex write FOnCalculateImageIndex;
    property OnCalculateText: TCalculateTextEvent read FOnCalculateText write
      FOnCalculateText;
    property OnColumnClick: TColumnClickEvent read FOnColumnClick write
      FOnColumnClick;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TDBReportView }

  TDBReportView = class(TCustomDBReportView)
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property DataSource;
    property DefaultDraw;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Images;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCalculateImageIndex;
    property OnCalculateText;
    property OnColumnClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDrawItem;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TSlideBar }

  TSlideBarKind = (sbVertical, sbHorizontal);

  TCustomSlideBar = class(TGraphicControl)
  private
  	FKind: TSlideBarKind;
    FMin: Double;
    FMax: Double;
    FStep: Double;
    FPosition: Double;
    FAssociate: TControl;
    FDrawState: TDrawState;
    FOnDraw: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetPosition(Value: Double);
    procedure SetKind(Value: TSlideBarKind);
    procedure SetDrawState(Value: TDrawState);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    function GetGripRect: TRect;
    procedure Change; dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property DrawState: TDrawState read FDrawState write SetDrawState;
    property Associate: TControl read FAssociate write FAssociate;
    property Kind: TSlideBarKind read FKind write SetKind;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Step: Double read FStep write FStep;
    property Position: Double read FPosition write SetPosition;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSlideBar = class(TCustomSlideBar)
  public
    property Canvas;
  published
    property Align;
    property Associate;
    property Anchors;
    property Enabled;
    property Kind;
    property Min;
    property Max;
    property Step;
    property Position;
    property OnChange;
    property OnDraw;
  end;

  TColorSlideBar = class(TCustomSlideBar)
  protected
    procedure Paint; override;
  end;

  { TBackground }

  TBackground = class(TGraphicControl)
  private
  	FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
  	constructor Create(AOwner: TComponent); override;
    procedure Draw;
    property Canvas;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  DefaultCurrencyFormat: string = '%.2m';
  DefaultDateFormat: string = 'mm/dd/yyyy';
  DefaultTimeFormat: string = 'hh:mm:ss am/pm';
  DefaultDateTimeFormat: string = 'mm/dd/yyyy hh:mm:ss am/pm';

implementation

type
  PComponentState = ^TComponentState;

{ TCustomCaptionBox }

constructor TCustomCaptionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FCaptionHeight := GetSystemMetrics(SM_CYVSCROLL) + 3;
  Color := clBtnFace;
  Height := 200;
  Width := 100;
  FBorderStyle := bsSingle;
  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Parent := Self;
    Align := alTop;
    BevelOuter := bvNone;
    Color := clInactiveCaption;
    Height := GetSystemMetrics(SM_CYVSCROLL) + 4;
    OnMouseDown := ControlMouseDown;
  end;
  FLabel := TLabel.Create(Self);
  with FLabel do
  begin
    Parent := FPanel;
    Transparent := True;
    ParentFont := True;
    Align := alLeft;
    Layout := tlCenter;
    Font.Color := clInactiveCaptionText;
    OnMouseDown := ControlMouseDown;
  end;
  FButton := TThemeGlyphButton.Create(Self);
  with FButton do
  begin
    Parent := FPanel;
    Kind := tgClose;
    Style := bsBeveled;
    Align := alRight;
    Color := clInactiveCaptionText;
    OnClick := ButtonClick;
    OnMouseDown := ControlMouseDown;
  end;
end;

procedure TCustomCaptionBox.Close;
begin
  Style := csClose;
  CloseQuery;
end;

procedure TCustomCaptionBox.CloseQuery;
var
  CanClose: Boolean;
begin
  CanClose := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, CanClose);
  if CanClose then
  begin
    if Assigned(FOnClose) then
      FOnClose(Self);
    if Style = csClose then
      Hide;
  end;
end;

procedure TCustomCaptionBox.Stick;
begin
  Style := csStick;
  if Assigned(FOnStick) then
    FOnStick(Self);
end;

procedure TCustomCaptionBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.style := CS_DBLCLKS;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TCustomCaptionBox.CreateWnd;
begin
  inherited CreateWnd;
  FActive := not FActive;
  Active := not Active;
  UpdateControls;
end;

procedure TCustomCaptionBox.Loaded;
begin
  inherited Loaded;
  if Parent <> nil then HandleNeeded;
  UpdateControls;
  FActive := not FActive;
  Active := not Active;
end;

procedure TCustomCaptionBox.MouseActivate;
begin
  Active := True;
  if IsChild(Handle, GetFocus) then Exit;
  SelectNext(nil, True, True);
  if not IsChild(Handle, GetFocus) then SetFocus;
end;

procedure TCustomCaptionBox.UpdateControls;
begin
  if HandleAllocated then
  begin
    FPanel.Font := Font;
    FLabel.Font := Font;
    FLabel.Font.Color := clInactiveCaptionText;
    Canvas.Font := Font;
    if FCaptionHeight < Canvas.TextHeight(' ') then
      FCaptionHeight := Canvas.TextHeight(' ');
    if FCaptionHeight < GetSystemMetrics(SM_CYVSCROLL) + 4 then
      FCaptionHeight := GetSystemMetrics(SM_CYVSCROLL) + 4;
    FPanel.Height := FCaptionHeight;
  end;
end;

procedure TCustomCaptionBox.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      FPanel.Color := clActiveCaption;
      FLabel.Font.Color := clCaptionText;
      FButton.Color := clCaptionText;
    end
    else
    begin
      FPanel.Color := clInactiveCaption;
      FLabel.Font.Color := clInactiveCaptionText;
      FButton.Color := clInactiveCaptionText;
      FActive := False;
    end;
  end;
end;

procedure TCustomCaptionBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomCaptionBox.SetCaptionHeight(Value: Integer);
begin
  if Value <> FCaptionHeight then
  begin
    FCaptionHeight := Value;
    UpdateControls;
  end;
end;

function TCustomCaptionBox.GetCloseButton: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TCustomCaptionBox.SetCloseButton(Value: Boolean);
begin
  FButton.Visible := Value;
end;

procedure TCustomCaptionBox.SetStyle(Value: TCaptionStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if FStyle = csClose then
      FButton.Kind := tgClose
    else
      FButton.Kind := tgPin;
  end;
end;

procedure TCustomCaptionBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateControls;
end;

procedure TCustomCaptionBox.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  Active := not Active;
  Active := not Active;
end;

procedure TCustomCaptionBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  FLabel.Caption := ' ' + Text;
end;

procedure TCustomCaptionBox.WMChildFocus(var Message: TMessage);
begin
  inherited;
  Active := True;
end;

procedure TCustomCaptionBox.WMChildKillFocus(var Message: TMessage);
begin
  inherited;
  Active := False;
end;

procedure TCustomCaptionBox.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  Rect: TRect;
begin
  if FBorderStyle = bsSingle then
  begin
    DC := GetWindowDC(Handle);
    Rect := Classes.Rect(0, 0, Width, Height);
    InflateRect(Rect, -2, -2);
    ExcludeClipRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    InflateRect(Rect, 2, 2);
    DrawThemeBorder(DC, Color, Rect, []);
    ReleaseDC(Handle, DC);
    Message.Result := 0;
  end
  else
    inherited;
end;
    
procedure TCustomCaptionBox.ButtonClick(Sender: TObject);
begin
  if Style = csClose then
    Close
  else
    Stick;
end;

procedure TCustomCaptionBox.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseActivate;
end;

{ TExpandableBox }

constructor TExpandableBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowCollapse := true;
  FExpandedHeight := 150;
  FExpanded := True;
  Color := clBtnFace;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csReplicatable];
  Height := FExpandedHeight;
  Width := 325;
  Canvas.Brush.Color := Color;
end;

procedure TExpandableBox.DblClick;
begin
  if not FBoxClick then
		Expanded := not Expanded;
end;

procedure TExpandableBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = VK_SPACE) or (Key = VK_RETURN) then
		Expanded := not Expanded;
end;

procedure TExpandableBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  FBoxClick := False;
  Rect := NodeRect;
  if (Button = mbLeft) and FAllowCollapse then
  begin
    if PtInRect(Rect, Point(X, Y)) then
    begin
      FBoxClick := True;
      FNodePressed := True;
      FNodeDown := True;
      InvalidateRect(Handle, @Rect, True);
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TExpandableBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := NodeRect;
  if FNodePressed and (PtInRect(Rect, Point(X, Y)) <> FNodeDown) and FAllowCollapse then
  begin
    FNodeDown := not FNodeDown;
    InvalidateRect(Handle, @Rect, True);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TExpandableBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := NodeRect;
  if (Button = mbLeft) and FAllowCollapse then
  begin
    if FNodeDown then
      Expanded := not Expanded;
    FNodePressed := False;
    FNodeDown := False;
    InvalidateRect(Handle, @Rect, True);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TExpandableBox.Paint;
var
  Rect: TRect;
  DC: HDC;
  State: TDrawState;
begin
  Rect := inherited GetClientRect;
  Canvas.FillRect(Rect);
  DC := Canvas.Handle;
  State := [];
  if not Enabled then State := State + [dsDisabled];
  if FNodeDown then State := State + [dsPressed];
  if FExpanded then State := State + [dsExpanded];
  if Focused then State := State + [dsFocused];
  if FAllowCollapse then
    DrawThemeExpandableBox(DC, Caption, Rect, State)
  else
    DrawThemeGroupBox(DC, Caption, Rect, State);
end;

function TExpandableBox.GetClientRect: TRect;
var
  I: Integer;
begin
  Result := inherited GetClientRect;
  with Result do
  begin
    I := HeightOf(CaptionRect);
    if FAllowCollapse and (I < NodeSize) then
      I := NodeSize;
    Top := I + 1;
    if FAllowCollapse then
      Left := Left + NodeSize div 2 + 2
    else
      Left := Left + 2;
    Right := Right - 2;
    Bottom := Bottom - 2;
  end;
end;

procedure TExpandableBox.Resize;
begin
  inherited Resize;
  if FExpanded then
    FExpandedHeight := Height;
end;

function TExpandableBox.GetCaptionRect: TRect;
var
  DC: HDC;
  F: HFONT;
  Size: TSize;
begin
  if IsRectEmpty(FCaptionRect) then
  begin
    DC := GetDC(0);
    F := SelectObject(DC, Font.Handle);
    Size := CalculateCaptionSize(DC, ' ');
    if Size.cy < NodeSize then
      Size.cy := NodeSize;
    FCaptionRect := Classes.Rect(0, 0, Width, Size.cy);
    SelectObject(DC, F);
    ReleaseDC(0, DC);
  end;
  Result :=  FCaptionRect;
end;

function TExpandableBox.GetNodeRect: TRect;
var
  I: Integer;
begin
  I := HeightOf(CaptionRect);
  if I < NodeSize then
    I := (I - NodeSize) div 2
  else
    I := 0;
  Result := Rect(0, I, NodeSize, I + NodeSize);
end;

procedure TExpandableBox.SetAllowCollapse(Value: Boolean);
begin
  if Value <> FAllowCollapse then
  begin
    if not Value then
      Expanded := True;
    FAllowCollapse := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, 0, 0);
    Invalidate;
  end;
end;

procedure TExpandableBox.SetExpanded(Value: Boolean);
var
  Event: TNotifyEvent;
begin
  Value := Value or (not FAllowCollapse);
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    if Value then
    begin
      Height := FExpandedHeight;
      Event := FOnExpand;
      if not (csDesigning in ComponentState) then
      begin
  	    RestoreVisible(FVisibleData);
	      FVisibleData := nil;
      end;
    end
    else
    begin
      if HeightOf(CaptionRect) > NodeSize then
        Height := CaptionRect.Bottom
      else
        Height := NodeSize;
      Event := FOnCollapse;
			if IsChild(Handle, GetFocus) then
	    	SetFocus;
	    if not (csDesigning in ComponentState) then
		    FVisibleData := SaveVisible(Self);
    end;
    if Assigned(Event) then
      Event(Self);
    Invalidate;
  end;
end;

procedure TExpandableBox.SetExpandedHeight(const Value: Integer);
begin
  if Value <> FExpandedHeight then
  begin
    FExpandedHeight := Value;
    if Expanded then
      Height := FExpandedHeight;
  end;
end;

procedure TExpandableBox.CMColorChanged(var Message: TMessage);
begin
  Canvas.Brush.Color := Color;
  Invalidate;
  inherited;
end;

procedure TExpandableBox.CMFontChanged(var Message: TMessage);
begin
  FillChar(FCaptionRect, SizeOf(FCaptionRect), #0);
  Canvas.Font := Font;
  Invalidate;
  inherited;
end;

procedure TExpandableBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TExpandableBox.WMSetFocus(var Message: TWMSetFocus);
begin
	Invalidate;
  inherited;
end;

procedure TExpandableBox.WMKillFocus(var Message: TWMSetFocus);
begin
	Invalidate;
  inherited;
end;

{ TLineGrid }

constructor TLineGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clBlack;
  Canvas.Brush.Color := Color;
  Height := 100;
  Width := 300;
  FAmplitude := 3;
  FBorderStyle := bsSingle;
  FGridSize := 12;
  FGridColor := clGreen;
  FPen := TPen.Create;
  FPen.Color := clLime;
  FPen.OnChange := StyleChanged;
  FVelocity := 6;
end;

destructor TLineGrid.Destroy;
begin
  FPen.Free;
  inherited Destroy;
end;

procedure TLineGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := CS_DBLCLKS or CS_VREDRAW or CS_HREDRAW;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TLineGrid.Log(Value: Integer);
var
  Delta: Integer;
  I: Integer;
begin
  I := Length(FLog);
  SetLength(FLog, I + 1);
  FLog[I] := Value;
  if FVelocity <> 0 then
  begin
    Delta := Round(I * FVelocity - ((I - 1) * FVelocity)) * -1;
    if Delta <> 0 then
      ScrollBy(Delta, 0);
  end;
end;

procedure TLineGrid.Paint;
var
  Rect: TRect;
  Delta: Integer;
  I: Integer;

  procedure DrawLines;
  var
    DC: HDC;
    PriorPen: HPEN;
    Size: Integer;
    FirstIndex: Integer;
    X, Y: Integer;
    I: Integer;
  begin
    DC := Canvas.Handle;
    PriorPen := SelectObject(DC, FPen.Handle);
    Size := Length(FLog) - 1;
    if FVelocity <> 0 then
      FirstIndex := Round(Length(FLog) - WidthOf(Rect) / FVelocity) - 1
    else
      FirstIndex := 0;
    if FirstIndex < 0 then
      FirstIndex := 0;
    X := 0;
    Y := 0;
    for I := FirstIndex to Length(FLog) - 1 do
    begin
      MoveToEx(DC, X, Y, nil);
      X := Round(Rect.Right - (Size - I) * FVelocity);
      Y := Round(Rect.Bottom - FLog[I] * FAmplitude);
      if I > FirstIndex then
        LineTo(DC, X, Y);
    end;
    SelectObject(DC, PriorPen);
  end;

begin
  with Canvas do
  begin
    Rect := ClientRect;
    FillRect(Rect);
    if FGridSize > 3 then
    begin
      Pen.Color := FGridColor;
      if FVelocity = 0 then
        Delta := 0
      else
        Delta := Round(Remainder(FVelocity * Length(FLog), FGridSize));
      for I := 0 to Rect.Bottom div FGridSize do
      begin
        MoveTo(0, Rect.Bottom - 1 - I * FGridSize);
        LineTo(Rect.Right, Rect.Bottom - 1 - I * FGridSize);
      end;
      for I := 0 to Rect.Right div FGridSize do
      begin
        MoveTo(Rect.Right - 1 - I * FGridSize - Delta, 0);
        LineTo(Rect.Right - 1 - I * FGridSize - Delta, Rect.Bottom);
      end;
    end;
    DrawLines;
  end;
end;

procedure TLineGrid.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TLineGrid.SetAmplitude(const Value: Double);
begin
  if Value <> FAmplitude then
  begin
    FAmplitude := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TLineGrid.SetGridSize(Value: Integer);
begin
  if FGridSize <> Value then
  begin
    FGridSize := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetGridColor(Value: TColor);
begin
  if Value <> FGridColor then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TLineGrid.SetVelocity(const Value: Double);
begin
  if Value <> FVelocity then
  begin
    FVelocity := Value;
    Invalidate;
  end;
end;

{ TIntegerEdit }

procedure TIntegerEdit.SetIntValue(Value: Integer);
begin
  if Value <> FIntValue then
  begin
    FIntValue := Value;
    if FIntValue > FMax then FIntValue := FMax;
    if FIntValue < FMin then FIntValue := FMin;
    if (FIntValue = 0) and (not FDisplayZero) then
      Text := ''
    else
      Text := IntToStr(FIntValue);
    Invalidate;
  end;
end;

procedure TIntegerEdit.SetMax(Value: Integer);
begin
  FMax := Value;
  if FMax < FMin then FMax := FMin;
  IntValue := FIntValue;
end;

procedure TIntegerEdit.SetMin(Value: Integer);
begin
  FMin := Value;
  if FMin > FMax then FMin := FMax;
  IntValue := FIntValue;
end;

procedure TIntegerEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TIntegerEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    if (FIntValue = 0) and (not FDisplayZero) then
      Text := ''
    else
      Text := IntToStr(FIntValue);
end;

procedure TIntegerEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  try
    if Text = '' then
      FIntValue := 0
    else
      FIntValue := StrToInt(Trim(Text));
  except
    // ignore error
  end;
  if (FIntValue = 0) and (not FDisplayZero) then
    Text := ''
  else
    Text := IntToStr(FIntValue);
  Invalidate;
end;

procedure TIntegerEdit.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  Rect: TRect;
  Brush: HBRUSH;
  PriorFont: HFONT;
  PriorColor: TColorRef;
  PriorMode: Cardinal;
begin
  if not Focused then
  begin
    BeginPaint(Handle, PS);
    with PS do
    try
      Windows.GetClientRect(Handle, Rect);
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(hdc, Rect, Brush);
      DeleteObject(Brush);
      if FIntValue < FMin then
        FIntValue := FMin;
      if (FIntValue <> 0) or FDisplayZero then
      begin
        InflateRect(Rect, -1, 0);
        OffsetRect(Rect, 0, 1);
        if Assigned(FOnCalculateRect) then
          FOnCalculateRect(Self, Rect);
        PriorFont := SelectObject(hdc, Font.Handle);
        PriorColor := 0;
        if not Enabled then
          PriorColor := SetTextColor(hdc, GetSysColor(COLOR_BTNSHADOW));
        PriorMode := SetBkMode(hdc, TRANSPARENT);
        DrawText(hdc, PChar(IntToStr(FIntValue)), -1, Rect, DT_TOP or DT_LEFT or DT_SINGLELINE);
        SetBkMode(hdc, PriorMode);
        if PriorColor <> 0 then
          SetTextColor(hdc, PriorColor);
        SelectObject(hdc, PriorFont);
      end;
    finally
      EndPaint(Handle, PS);
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

{ TMoneyEdit }

function Round(const Value: Extended; Precision: Integer): Extended;
begin
  if Precision > 0 then
    Result := StrToFloat(Format('%.' + IntToStr(Precision) + 'f', [Value]))
  else
    Result := Trunc(Value)
end;

constructor TMoneyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDecimals := 2;
  FOptions := [moDollarSign, moForceAmount];
end;

procedure TMoneyEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    Text := FRevertText;
end;

procedure TMoneyEdit.WMKillFocus(var Message: TWMKillFocus);
var
  Value: Extended;
begin
  inherited;
  if IsFloat(Text, Value) then
  begin
    FAmount := Round(Value, FDecimals);
    if moForceAmount in FOptions then
      FRevertText := FloatToStr(FAmount)
    else
      FRevertText := Text;
  end;
  Text := FRevertText;
  InvalidateRect(Handle, nil, True);
end;

procedure TMoneyEdit.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  Rect: TRect;
  Brush: HBRUSH;
  PriorMode: Cardinal;
  PriorColor: Cardinal;
  PriorFont: HFONT;
begin
  if not Focused then
  begin
    BeginPaint(Handle, PS);
    with PS do
    try
      Windows.GetClientRect(Handle, Rect);
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(hdc, Rect, Brush);
      DeleteObject(Brush);
      InflateRect(Rect, -2, 0);
      PriorFont := SelectObject(hdc, Font.Handle);
      PriorMode := SetBkMode(hdc, TRANSPARENT);
      if Enabled then
        DrawText(hdc, PChar(GetAmountText), -1, Rect, DT_RIGHT or DT_VCENTER or
          DT_SINGLELINE)
      else
      begin
        PriorColor := SetTextColor(hdc, GetSysColor(COLOR_GRAYTEXT));
        DrawText(hdc, PChar(GetAmountText), -1, Rect, DT_RIGHT or DT_VCENTER or
          DT_SINGLELINE);
         SetTextColor(hdc, PriorColor);
      end;
      SetBkMode(hdc, PriorMode);
      SelectObject(hdc, PriorFont);
    finally
      EndPaint(Handle, PS);
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

function TMoneyEdit.GetAmountText: string;
var
  S: string;
begin
  S := IntToStr(Abs(FDecimals));
  if moDollarSign in FOptions then
    S := S + 'm'
  else
    S := S + 'f';
  if FDecimals > 0 then
    Result := Format('%.' + S, [FAmount])
  else
    Result := Format('%-.' + S, [FAmount]);
end;

procedure TMoneyEdit.SetAmount(const Value: Double);
begin
  if Value <> FAmount then
  begin
    FAmount := Round(Value, FDecimals);
    FRevertText := FloatToStr(FAmount);
    Text := FRevertText;
    InvalidateRect(Handle, nil, True);
  end;
end;

procedure TMoneyEdit.SetDecimals(Value: Integer);
begin
  if Value <> FDecimals then
  begin
    FDecimals := Value;
    Invalidate;
  end;
end;

procedure TMoneyEdit.SetOptions(Value: TMoneyEditOptions);
begin
  FOptions := Value;
  Invalidate;
end;

{ TDrawListLink }

type
  TDrawListLink = class(TDataLink)
  private
    FControl: TCustomDBDrawList;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(Control: TCustomDBDrawList);
  end;

constructor TDrawListLink.Create(Control: TCustomDBDrawList);
begin
  inherited Create;
  FControl := Control;
  ReadOnly := True;
end;

procedure TDrawListLink.ActiveChanged;
begin
  inherited ActiveChanged;
  if FControl <> nil then
    FControl.DataLinkChanged;
end;

procedure TDrawListLink.DataSetChanged;
begin
  inherited DataSetChanged;
  if FControl <> nil then
    FControl.DataLinkChanged;
end;

{ TCustomDBDrawList }

constructor TCustomDBDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FDataLink := TDrawListLink.Create(Self);
  FItemHeight := 18;
  Color := clWindow;
  DoubleBuffered := True;
  ParentColor := False;
  TabStop := True;
  Width := 150;
  Height := 100;
end;

destructor TCustomDBDrawList.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TCustomDBDrawList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_VSCROLL;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TCustomDBDrawList.CreateWnd;
var
  ScrollInfo: TScrollInfo;
begin
  inherited CreateWnd;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), #0);
  with ScrollInfo do
  begin
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := 100;
    nPage := 10;
    nPos := FScrollPos;
  end;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

function TCustomDBDrawList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  with FDataLink do
    if (not Result) and (DataSet <> nil) then
      Result := DataSet.MoveBy(1) <> 0;
end;

function TCustomDBDrawList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  with FDataLink do
    if (not Result) and (DataSet <> nil) then
      Result := DataSet.MoveBy(-1) <> 0;
end;

procedure TCustomDBDrawList.DrawItem(const Rect: TRect; Index: Integer);
var
  State: TOwnerDrawState;
begin
  if Assigned(FOnDrawitem) then
  begin
    if Index = RecordIndex then
    begin
      State := [odSelected];
      if FInternalFocused then
        Include(State, odFocused);
    end
    else
      State := [];
    FOnDrawItem(Self, Index, Rect, State);
  end;
end;

procedure TCustomDBDrawList.DataLinkChanged;
var
  Active: Boolean;
begin
  Active := FDataLink.Active;
  if Active then
  begin
    if not FActive then
    begin
      FActive := Active;
      FDataLink.BufferCount := ClientHeight div FItemHeight;;
    end;
    FColumnCount := FDataLink.DataSet.FieldCount;
    FRecordCount := FDataLink.RecordCount;
    FRecordIndex := FDataLink.ActiveRecord;
    if FDataLink.BOF then
      FScrollPos := 0
    else if FDataLink.EOF then
      FScrollPos := 100
    else
      FScrollPos := 45;
  end
  else
  begin
    FActive := Active;
    FColumnCount := -1;
    FRecordCount := -1;
    FRecordIndex := -1;
    FScrollPos := 0;
  end;
  if HandleAllocated then
    SetScrollPos(Handle, SB_VERT, FScrollPos, True);
  Invalidate;
end;

procedure TCustomDBDrawList.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if not FActive then
    Exit;
  Delta := 0;
  case Key of
    VK_UP: Delta := -1;
    VK_DOWN: Delta := 1;
    VK_PRIOR: Delta := -FRecordCount;
    VK_NEXT: Delta := FRecordCount;
    VK_HOME: Delta := -MaxInt;
    VK_END: Delta := MaxInt;
  end;
  if Delta <> 0 then
  begin
    if Delta = -MaxInt then
      FDataLink.DataSet.First
    else if Delta = MaxInt then
      FDataLink.DataSet.Last
    else
      FDataLink.DataSet.MoveBy(Delta);
  end;
end;

procedure TCustomDBDrawList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TCustomDBDrawList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if CanFocus then
      SetFocus
    else
      PostMessage(Handle, WM_SETFOCUS, 0, 0);
    if ssDouble in Shift then
      DblClick
    else
    begin
      MouseCapture := True;
      FTracking := True;
      SelectItemAt(X, Y);
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomDBDrawList.Paint;
var
  DrawRect: TRect;
  PriorRecord: Integer;
  I: Integer;
begin
  DrawRect := ClientRect;
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
    Canvas.FillRect(DrawRect);
  if FActive then
  begin
    PriorRecord := FDataLink.ActiveRecord;
    FDataLink.ActiveRecord := 0;
    with DrawRect do
    begin
      Bottom := Top;
      for I := 0 to FRecordCount - 1 do
      begin
        FDataLink.ActiveRecord := I;
        Inc(Bottom, FItemHeight);
        DrawItem(DrawRect, I);
        Inc(Top, FItemHeight);
      end;
    end;
    FDataLink.ActiveRecord := PriorRecord;
  end;
end;

procedure TCustomDBDrawList.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if FActive then
  begin
    if Y < 0 then
      Y := 0;
    if Y >= ClientHeight then
      Y := ClientHeight - 1;
    Delta := Y div FItemHeight - FRecordIndex;
      FDataLink.DataSet.MoveBy(Delta);
  end;
end;

procedure TCustomDBDrawList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FDataLink.BufferCount := AHeight div FItemHeight;
end;

procedure TCustomDBDrawList.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
    begin
      RecreateWnd;
      if not (csReading in ComponentState) then
        FDataLink.BufferCount := ClientHeight div FItemHeight;
    end;
  end;
end;

function TCustomDBDrawList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBDrawList.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCustomDBDrawList.SetDisplayWidth(Value: Integer);
begin
  if Value <> FDisplayWidth then
  begin
    FDisplayWidth := Value;
    //
  end;
end;

procedure TCustomDBDrawList.SetItemHeight(Value: Integer);
begin
  if Value < 1 then
    Exit;
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    if FItemHeight < 1 then
      FItemHeight := 1;
    if HandleAllocated then
      FDataLink.BufferCount := ClientHeight div FItemHeight;
  end;
end;

procedure TCustomDBDrawList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Invalidate;
end;

procedure TCustomDBDrawList.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TCustomDBDrawList.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FInternalFocused := False;
  Invalidate;
end;

procedure TCustomDBDrawList.WMSetFocus(var message: TWMSetFocus);
begin
  inherited;
  FInternalFocused := True;
  Invalidate;
end;

procedure TCustomDBDrawList.WMVScroll(var Message: TWMVScroll);
begin
  if FActive then
    with Message, FDataLink.DataSet do
      case ScrollCode of
        SB_LINEUP: MoveBy(-1);
        SB_LINEDOWN: MoveBy(1);
        SB_PAGEUP: MoveBy(-FRecordCount);
        SB_PAGEDOWN: MoveBy(FRecordCount);
        SB_THUMBPOSITION:
          begin
            //
          end;
        SB_BOTTOM: Last;
        SB_TOP: First;
      end;
end;

{ TReportViewColumn }

constructor TReportViewColumn.Create(Collection: TCollection);
var
  Columns: TReportViewColumns absolute Collection;
begin
  inherited Create(Collection);
  FSection := Columns.FReportView.FHeaderControl.Sections.Add;
end;

destructor TReportViewColumn.Destroy;
begin
  FSection.Free;
  inherited Destroy;
end;

function TReportViewColumn.GetAlignment: TAlignment;
begin
  Result := FSection.Alignment;
end;

procedure TReportViewColumn.SetAlignment(Value: TAlignment);
begin
  FSection.Alignment := Value;
end;

function TReportViewColumn.GetText: string;
begin
  Result := FSection.Text;
end;

procedure TReportViewColumn.SetText(Value: string);
begin
  FSection.Text := Value;
end;

function TReportViewColumn.GetWidth: Integer;
begin
  Result := FSection.Width;
end;

procedure TReportViewColumn.SetWidth(Value: Integer);
begin
  FSection.Width := Value;
  Changed(True);
end;

{ TReportViewColumns }

constructor TReportViewColumns.Create(ReportView: TCustomDBReportView);
begin
  inherited Create(TReportViewColumn);
  FReportView := ReportView;
end;

function TReportViewColumns.Add: TReportViewColumn;
begin
  Result := TReportViewColumn(inherited Add);
end;

procedure TReportViewColumns.BeginUpdate;
begin
  inherited BeginUpdate;
  FReportView.FHeaderControl.Sections.BeginUpdate;
end;
procedure TReportViewColumns.EndUpdate;
begin
  inherited EndUpdate;
  FReportView.FHeaderControl.Sections.EndUpdate;
end;

procedure TReportViewColumns.Update(Item: TCollectionItem);
begin
  FReportView.FDrawList.Invalidate;
end;

function TReportViewColumns.GetItem(Index: Integer): TReportViewColumn;
begin
  Result := TReportViewColumn(inherited GetItem(Index));
end;

function TReportViewColumns.GetOwner: TPersistent;
begin
  Result := FReportView;
end;

procedure TReportViewColumns.SetItem(Index: Integer; Value: TReportViewColumn);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomDBReportView }

constructor TCustomDBReportView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FDefaultDraw := True;
  FHeaderControl := THeaderControl.Create(Self);
  with FHeaderControl do
  begin
    Parent := Self;
    FullDrag := False;
    OnSectionClick := DoSectionClick;
    OnSectionResize := DoSectionResize;
  end;
  FColumns := TReportViewColumns.Create(Self);
  FDrawList := TDBDrawList.Create(Self);
  with FDrawList do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    FDefDrawListProc := WindowProc;
    WindowProc := DrawListProc;
    OnDrawItem := DoDrawItem;
  end;
  Height := 200;
  Width := 200;
end;

destructor TCustomDBReportView.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

procedure TCustomDBReportView.WndProc(var Message: TMessage);
begin
  if not FForwarding then
    case Message.Msg of
      WM_KILLFOCUS, WM_SETFOCUS, WM_MOUSEFIRST..WM_MOUSELAST,
      WM_KEYFIRST..WM_KEYLAST:
        begin
          FForwarding := True;
          try
            FDefDrawListProc(Message);
          finally
            FForwarding := False;
          end;
        end;
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBReportView.DrawListProc(var Message: TMessage);
begin
  if not FForwarding then
    case Message.Msg of
      WM_KILLFOCUS, WM_SETFOCUS, WM_MOUSEFIRST..WM_MOUSELAST,
      WM_KEYFIRST..WM_KEYLAST:
        begin
          FForwarding := True;
          try
            WindowProc(Message);
          finally
            FForwarding := False;
          end;
        end;
    end;
  FDefDrawListProc(Message);
end;

procedure TCustomDBReportView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TCustomDBReportView.DoDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  I: Integer;

  procedure DrawImage;
  var
    ImageIndex: Integer;
    PriorBlendColor: TColor;
    PriorDrawStyle: TDrawingStyle;
  begin
    ImageIndex := 0;
    if Assigned(FOnCalculateImageIndex) then
      FOnCalculateImageIndex(Self, ImageIndex);
    if FImages <> nil then
    begin
      PriorBlendColor := FImages.BlendColor;
      FImages.BlendColor := clHighlight;
      PriorDrawStyle := FImages.DrawingStyle;
      if odFocused in State then
        FImages.DrawingStyle := dsSelected
      else
        FImages.DrawingStyle := dsNormal;
      FImages.Draw(Canvas, Rect.Left, Rect.Top, ImageIndex);
      FImages.DrawingStyle := PriorDrawStyle;
      FImages.BlendColor := PriorBlendColor;
      Inc(Rect.Left, FImages.Width + 4);
    end;
  end;

  procedure DrawColumn(Index: Integer);
  var
    Text: string;
    Section: THeaderSection;

    procedure DrawCaptionColumn;
    var
      PriorBrushColor: TColor;
      PriorFontColor: TColor;
      FocusRect: TRect;
    begin
      Rect.Right := Section.Width - 2;
      if odFocused in State then
      begin
        PriorBrushColor := Canvas.Brush.Color;
        Canvas.Brush.Color := clHighlight;
        PriorFontColor := Canvas.Font.Color;
        Canvas.Font.Color := clWindow;
        FocusRect := CalculateCaptionRect(Canvas.Handle, Text, Rect,
          AlignmentToDirection(Section.Alignment));
        InflateRect(FocusRect, 1, 1);
        Canvas.FillRect(FocusRect);
        InflateRect(FocusRect, 1, 0);
        Canvas.DrawFocusRect(FocusRect);
        DrawCaption(Canvas.Handle, Text, Rect, AlignmentToDirection(Section.Alignment));
        Canvas.Font.Color := PriorFontColor;
        Canvas.Brush.Color := PriorBrushColor;
      end
      else
        DrawCaption(Canvas.Handle, Text, Rect, AlignmentToDirection(Section.Alignment));
      Rect.Left := Rect.Right + 2;
    end;

    procedure DrawTextColumn;
    begin
      Rect.Right := Rect.Left + Section.Width - 2;
      DrawText(Canvas.Handle, PChar(Text), -1, Rect, DR_FORMAT or DT_VCENTER or
        DT_LEFT or DT_NOPREFIX);
      Rect.Left := Rect.Right + 2;
    end;

  var
    Field: TField;
  begin
    Text := '';
    if I < FDrawList.ColumnCount then
    begin
      Field := DataSource.DataSet.Fields[Columns[I].FSection.Index];
      case Field.DataType of
        ftString, ftSmallint, ftInteger, ftWord, ftFloat, ftAutoInc,
          ftWideString, ftLargeint:
          Text := Field.AsString;
        ftBoolean:
          Text := BooleanIdents[Field.AsBoolean];
        ftCurrency:
          Text := Format(DefaultCurrencyFormat, [Field.AsFloat]);
        ftDate:
          Text := FormatDateTime(DefaultDateFormat, Field.AsDateTime);
        ftTime:
          Text := FormatDateTime(DefaultTimeFormat, Field.AsDateTime);
        ftDateTime:
          Text := FormatDateTime(DefaultDateTimeFormat, Field.AsDateTime);
      end;
    end;
    if Assigned(FOnCalculateText) then
      FOnCalculateText(Self, I, Text);
    Section := FHeaderControl.Sections[I];
    Inc(Rect.Left, 2);
    if I = 0 then
      DrawCaptionColumn
    else
      DrawTextColumn;
    Dec(Rect.Left, 2);
  end;

begin
  if FHeaderControl.Sections.Count = 0 then
    Exit;
  if DefaultDraw then
  begin
    Inc(Rect.Left, 2);
    Canvas := TDBDrawList(Control).Canvas;
    DrawImage;
    for I := 0 to FHeaderControl.Sections.Count - 1 do
      DrawColumn(I);
  end
  else if Assigned(FOnDrawItem) then
    FOnDrawItem(Control, Index, Rect, State);
end;

procedure TCustomDBReportView.DoSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, FColumns[Section.Index]);
end;

procedure TCustomDBReportView.DoSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
var
  MinWidth: Integer;
begin
  if Section.Index = 0 then
  begin
    MinWidth := 10;
    if FImages <> nil then
      Inc(MinWidth, FImages.Width);
    if Section.Width < MinWidth then
      Section.Width := MinWidth;
  end;
  FDrawList.Invalidate;
end;

procedure TCustomDBReportView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FDrawList.DataLinkChanged;
end;

procedure TCustomDBReportView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomDBReportView.SetColumns(Value: TReportViewColumns);
begin
  FColumns.Assign(Value);
  Invalidate;
end;

function TCustomDBReportView.GetDataSource: TDataSource;
begin
  Result := FDrawList.DataSource;
end;

procedure TCustomDBReportView.SetDataSource(Value: TDataSource);
begin
  FDrawList.DataSource := Value;
end;

procedure TCustomDBReportView.SetDefaultDraw(Value: Boolean);
begin
  if Value <> FDefaultDraw then
  begin
    FDefaultDraw := Value;
    Invalidate;
  end;
end;

function TCustomDBReportView.GetFlat: Boolean;
begin
  Result := FHeaderControl.Style = hsFlat;
end;

procedure TCustomDBReportView.SetFlat(Value: Boolean);
const
  Styles: array[Boolean] of THeaderStyle = (hsButtons, hsFlat);
begin
  FHeaderControl.Style := Styles[Value];
end;

procedure TCustomDBReportView.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      FDrawList.ItemHeight := FImages.Height + 1
    else
      FDrawList.ItemHeight := 18;
    Invalidate;
  end;
end;

function TCustomDBReportView.GetStyle: TReportViewStyle;
const
  Styles: array[Boolean] of TReportViewStyle = (rsGrid, rsColumn);
begin
  Result := Styles[FHeaderControl.Visible];
end;

procedure TCustomDBReportView.SetStyle(Value: TReportViewStyle);
begin
  FHeaderControl.Visible := Value = rsColumn;
end;

procedure TCustomDBReportView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 0;
end;

{ TCustomSlideBar }

constructor TCustomSlideBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 100;
  Width := 50;
  FMax := 100;
  FMin := 0;
  FStep := 1;
  if not Enabled then
	  FDrawState := [dsDisabled];
end;

function TCustomSlideBar.GetGripRect: TRect;
var
  X, Y: Integer;
begin
	if FKind = sbVertical then
  begin
    X := Width div 2;
    Y := Height - (System.Round(Abs(Position - Min) * ((Height - 10) / (FMax - FMin))) + 5);
    Result := ClientRect;
    Result.Left := X - 5;
    Result.Right := X + 5;
    Result.Top := Y - 5;
    Result.Bottom := Y + 5;
    InflateRect(Result, 8, 4);
	end
  else
  begin
    X := (System.Round(Abs(Position - Min) * ((Width - 10) / (FMax - FMin))) + 5);
    Y := Height div 2;
    Result := ClientRect;
    Result.Left := X - 5;
    Result.Right := X + 5;
    Result.Top := Y - 5;
    Result.Bottom := Y + 5;
    InflateRect(Result, 4, 8);
  end;
end;

procedure TCustomSlideBar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	Range, Delta: Single;
begin
  inherited MouseMove(Shift, X, Y);
  if PtInRect(GetGripRect, Point(X, Y)) then
		DrawState := DrawState + [dsHot]
  else
		DrawState := DrawState - [dsHot];
  if FKind = sbVertical then
  begin
  	Range := Height;
    Delta := Y;
  end
  else
  begin
  	Range := Width;
    Delta := X;
  end;
  if Range = 0 then
  	Range := 0.000001;
  if (dsFocused in DrawState) and (FMax > FMin) then
	  if FKind = sbVertical then
  	  Position := (Range - Delta) / (Range / (FMax - FMin)) + FMin
    else
  	  Position := (Delta / Range) * (FMax - FMin) + FMin;
end;

procedure TCustomSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
	  DrawState := DrawState + [dsFocused];
end;

procedure TCustomSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
	  DrawState := DrawState - [dsFocused];
end;

procedure TCustomSlideBar.Paint;
var
  DC: HDC;
  Rect: TRect;
  X, Y: Integer;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self)
  else
  begin
    DC := Canvas.Handle;
    Rect := ClientRect;
    if FKind = sbVertical then
    begin
      X := Width div 2;
      Y := Height - (System.Round(Abs(Position - Min) * ((Height - 10) / (FMax - FMin))) + 5);
      Rect.Left := X - 1;
      Rect.Right := X + 1;
      DrawThemeHorzSplit(DC, Rect, FDrawState);
      Rect.Top := Y - 1;
      Rect.Bottom := Y + 1;
      InflateRect(Rect, 8, 4);
      FillRect(DC, Rect, COLOR_BTNFACE + 1);
      DrawThemeVertThumb(DC, Rect, FDrawState);
    end
    else
    begin
      X := (System.Round(Abs(Position - Min) * ((Width - 10) / (FMax - FMin))) + 5);
      Y := Height div 2;
      Rect.Top := Y - 1;
      Rect.Bottom := Y + 1;
      DrawThemeVertSplit(DC, Rect, FDrawState);
      Rect.Left := X - 1;
      Rect.Right := X + 1;
      InflateRect(Rect, 4, 8);
      FillRect(DC, Rect, COLOR_BTNFACE + 1);
      DrawThemeHorzThumb(DC, Rect, FDrawState);
    end;
  end;
end;

procedure TCustomSlideBar.SetDrawState(Value: TDrawState);
begin
	if Value <> FDrawState then
  begin
		FDrawState := Value;
    Invalidate;
  end;
end;

procedure TCustomSlideBar.SetKind(Value: TSlideBarKind);
var
	I: Integer;
begin
	if Value <> FKind then
  begin
  	FKind := Value;
    if csLoading in ComponentState then Exit;
    I := Width;
    Width := Height;
    Height := I;
    Invalidate;
  end;
end;

procedure TCustomSlideBar.SetMax(const Value: Double);
begin
  if Value <> FMax then
  begin
    if Value < FMin then
      FMax := FMin
    else
      FMax := Value;
    Invalidate;
  end;
end;

procedure TCustomSlideBar.SetMin(const Value: Double);
begin
  if Value <> FMin then
  begin
    if FMax < Value then
      FMin := FMax
    else
      FMin := Value;
    Invalidate;
  end;
end;

procedure InvalidateControlRect(Control: TControl; Rect: TRect);
var
	WinControl: TWinControl absolute Control;
begin
	if csDesigning in Control.ComponentState then
  	Control.Invalidate
	else if not Control.Visible then
  	Exit
	else if (Control is TGraphicControl) and
  	(Control.Parent <> nil) and	Control.Parent.HandleAllocated then
  begin
  	with Control.BoundsRect do
			OffsetRect(Rect, Left, Top);
    InvalidateRect(Control.Parent.Handle, @Rect, True)
  end
  else if (Control is TWinControl) and (WinControl.HandleAllocated) then
    InvalidateRect(WinControl.Handle, @Rect, False)
end;

procedure TCustomSlideBar.SetPosition(Value: Double);
begin
  if Value < FMin then
    Value := FMin
  else if Value > FMax then
    Value := FMax;
  if Value <> FPosition then
  begin
  	InvalidateControlRect(Self, GetGripRect);
    if FStep > 0 then
      FPosition := Divide(Value, FStep)
    else
      FPosition := Value;
    Change;
    if FAssociate <> nil then
      TCustomSlideBar(FAssociate).Text := FloatToStr(FPosition);
  	InvalidateControlRect(Self, GetGripRect);
  end;
end;

procedure TCustomSlideBar.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
  if Enabled then
	  DrawState := DrawState - [dsDisabled]
  else
	  DrawState := DrawState + [dsDisabled];
end;

procedure TCustomSlideBar.CMMouseLeave(var Message: TMessage);
begin
	inherited;
  DrawState := DrawState - [dsHot];
end;

{ TColorClideBar }

procedure TColorSlideBar.Paint;
begin
end;

{ TBackground }

constructor TBackground.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Width := 150;
  Height := 100;
end;

procedure TBackground.Draw;
begin
	Paint;
end;

procedure TBackground.Paint;
var
  Rect: TRect;
begin
  Rect := Classes.Rect(0, 0, Width, Height);
	DrawThemeBorder(Canvas.Handle, clAppWorkSpace, Rect, []);
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

end.
