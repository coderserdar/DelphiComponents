unit TBXControls;

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, StdCtrls, Forms, Graphics, ImgList,
  Menus, ExtCtrls, TBX, TBXThemes, TBXUtils;

const
  { New hit test constants for page scrollers }
  HTSCROLLPREV = 30;
  HTSCROLLNEXT = 31;

type
  { TTBXControlMargins }
  TTBXControlMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetBottom(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetTop(Value: Integer);
  public
    procedure Assign(Src: TPersistent); override;
    procedure Modified; dynamic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Integer read FLeft write SetLeft default 0;
    property Top: Integer read FTop write SetTop default 0;
    property Right: Integer read FRight write SetRight default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;
  end;

  { TTBXControl }
  TTBXControl = class(TCustomControl)
  private
    FDisableScroll: Boolean;
    FFullRedraw: Boolean;
    FInSpaceClick: Boolean;
    FMouseInControl: Boolean;
{$IFNDEF JR_D7}
    FParentBackground: Boolean;
{$ENDIF}
    FPushed: Boolean;
    FSmartFocus: Boolean;
    FSpaceAsClick: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure MouseTimerHandler(Sender: TObject);
    procedure RemoveMouseTimer;
    procedure SetFullRedraw(Value: Boolean);
    procedure TBMThemeChange(var Message); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
  protected
    ControlStateEx: set of (csShowAccels, csShowFocus);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function  GetMinHeight: Integer; virtual;
    function  GetMinWidth: Integer; virtual;
    procedure SetAccelVisibility(DoShow: Boolean);
{$IFNDEF JR_D7}
    procedure SetParentBackground(Value: Boolean); virtual;
{$ENDIF}
    property FullRedraw: Boolean read FFullRedraw write SetFullRedraw;
    property MouseInControl: Boolean read FMouseInControl;
    property InSpaceClick: Boolean read FInSpaceClick;
{$IFDEF JR_D7}
    property ParentBackground default True;
{$ELSE}
    property ParentBackground: Boolean read FParentBackground write SetParentBackground default True;
{$ENDIF}
    property Pushed: Boolean read FPushed;
    property SpaceAsClick: Boolean read FSpaceAsClick write FSpaceAsClick default False;
    property SmartFocus: Boolean read FSmartFocus write FSmartFocus default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeVisible;
    procedure MouseEntered;
    procedure MouseLeft;
  end;

  { TTBXAlignmentPanel }
  TTBXAlignmentPanel = class(TTBXControl)
  private
    FMargins: TTBXControlMargins;
    procedure MarginsChangeHandler(Sender: TObject);
    procedure SetMargins(Value: TTBXControlMargins);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    function  GetMinHeight: Integer; override;
    function  GetMinWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ParentColor;
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins: TTBXControlMargins read FMargins write SetMargins;
    property ParentBackground;
    property ParentFont;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXTextObject }
  TTBXTextObject = class(TTBXControl)
  private
    FAlignment: TLeftRight;
    FCaption: WideString;
    FHint: WideString;
    FMargins: TTBXControlMargins;
    FWrapping: TTextWrapping;
    FShowAccelChar: Boolean;
    FUpdatingCaption: Boolean;
    FUpdatingSize: Boolean;
    procedure SetHint(const Value: WideString);
    procedure SetCaption(const Value: WideString);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure MarginsChangeHandler(Sender: TObject);
    procedure ReadCaptionProperty(Reader: TReader);
    procedure ReadHintProperty(Reader: TReader);
    procedure WriteCaptionProperty(Writer: TWriter);
    procedure WriteHintProperty(Writer: TWriter);
    procedure SetAlignment(Value: TLeftRight);
    procedure SetMargins(Value: TTBXControlMargins);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetWrapping(Value: TTextWrapping);
  protected
    procedure AdjustFont(AFont: TFont); virtual;
    procedure AdjustHeight;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); virtual;
    function  DoDrawText(ACanvas: TCanvas; var Rect: TRect; Flags: Longint): Integer; virtual;
    procedure DoMarginsChanged; virtual;
    function  GetFocusRect(const R: TRect): TRect; virtual;
    function  GetLabelText: WideString; virtual;
    function  GetTextAlignment: TAlignment; virtual;
    function  GetTextMargins: TRect; virtual;
    function  GetTextFlags: Cardinal; virtual;
    function  HandleAccelChar: Boolean; virtual;
    function  HandleDialogKey(CharCode: Word; ShiftState: TShiftState): Boolean; virtual;
    procedure Loaded; override;
    procedure Paint; override;
    property Alignment: TLeftRight read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize default True;
    property DoubleBuffered default True;
    property ParentBackground default True;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Margins: TTBXControlMargins read FMargins write SetMargins;
    property Wrapping: TTextWrapping read FWrapping write SetWrapping default twNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetControlsAlignment: TAlignment; override;
  published
    property Caption: WideString read FCaption write SetCaption stored False;
    property Hint: WideString read FHint write SetHint stored False;
  end;

  { TTBXCustomLabel }
  TTBXCustomLabel = class(TTBXTextObject)
  private
    FFocusControl: TWinControl;
    FUnderline: Boolean;
    FUnderlineColor: TColor;
    procedure SetUnderline(Value: Boolean);
    procedure SetUnderlineColor(Value: TColor);
    procedure SetFocusControl(Value: TWinControl);
  protected
    function  GetTextMargins: TRect; override;
    function  HandleAccelChar: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Underline: Boolean read FUnderline write SetUnderline default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBtnShadow;
    property Wrapping default twWrap;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TTBXLabel }
  TTBXLabel = class(TTBXCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Margins;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Underline;
    property UnderlineColor;
    property Visible;
    property Wrapping;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXCustomLink }

  TTBXCustomLink = class(TTBXTextObject)
  private
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure AdjustFont(AFont: TFont); override;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    function  GetFocusRect(const R: TRect): TRect; override;
    function  GetTextAlignment: TAlignment; override;
    function  GetTextMargins: TRect; override;
    function  HandleAccelChar: Boolean; override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Cursor default crHandPoint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property SmartFocus default True;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetControlsAlignment: TAlignment; override;
  end;

  { TTBXLink }

  TTBXLink = class(TTBXCustomLink)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImageIndex;
    property Images;
    property Margins;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property SmartFocus;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXCustomButton }
  TTBXCustomButton = class;
  TButtonLayout = (blGlyphLeft, blGlyphTop, blGlyphRight, blGlyphBottom);
  TButtonStyle = (bsNormal, bsFlat);
  TDropDownEvent = procedure(Sender: TTBXCustomButton; var AllowDropDown: Boolean) of object;

  TTBXCustomButton = class(TTBXTextObject)
  private
    FAlignment: TAlignment;
    FAllowAllUnchecked: Boolean;
    FBorderSize: Integer;
    FChecked: Boolean;
    FDropdownCombo: Boolean;
    FDropdownMenu: TPopupMenu;
    FButtonStyle: TButtonStyle;
    FGlyphSpacing: Integer;
    FGroupIndex: Integer;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FInClick: Boolean;
    FLayout: TButtonLayout;
    FMenuVisible: Boolean;
    FModalResult: TModalResult;
    FRepeating: Boolean;
    FRepeatDelay: Integer;
    FRepeatInterval: Integer;
    FRepeatTimer: TTimer;
    FOnDropDown: TDropDownEvent;
    procedure ImageListChange(Sender: TObject);
    procedure RepeatTimerHandler(Sender: TObject);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAllowAllUnchecked(Value: Boolean);
    procedure SetBorderSize(Value: Integer);
    procedure SetButtonStyle(Value: TButtonStyle);
    procedure SetChecked(Value: Boolean);
    procedure SetDropdownCombo(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetGlyphSpacing(Value: Integer);
    procedure SetGroupIndex(Value: Integer);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLayout(Value: TButtonLayout);
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function  ArrowVisible: Boolean;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); override;
    function  DoDrawText(ACanvas: TCanvas; var Rect: TRect; Flags: Longint): Integer; override;
    function  DoDropDown: Boolean; virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    function  GetFocusRect(const R: TRect): TRect; override;
    procedure GetItemInfo(out ItemInfo: TTBXItemInfo); virtual;
    function  GetTextAlignment: TAlignment; override;
    function  GetTextMargins: TRect; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function  PtInButtonPart(const Pt: TPoint): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateCheckedState;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AllowAllUnchecked: Boolean read FAllowAllUnchecked write SetAllowAllUnchecked default False;
    property BorderSize: Integer read FBorderSize write SetBorderSize default 4;
    property ButtonStyle: TButtonStyle read FButtonStyle write SetButtonStyle default bsNormal;
    property Checked: Boolean read FChecked write SetChecked default False;
    property DropdownCombo: Boolean read FDropdownCombo write SetDropdownCombo default False;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property GlyphSpacing: Integer read FGlyphSpacing write SetGlyphSpacing default 4;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Repeating: Boolean read FRepeating write FRepeating default False;
    property RepeatDelay: Integer read FRepeatDelay write FRepeatDelay default 400;
    property RepeatInterval: Integer read FRepeatInterval write FRepeatInterval default 100;
    property SmartFocus default True;
    property TabStop default True;
    property OnDropDown: TDropDownEvent read FOnDropDown write FOnDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function  GetControlsAlignment: TAlignment; override;
  end;

  { TTBXButton }
  TTBXButton = class(TTBXCustomButton)
  published
    property Align;
    property Alignment;
    property GroupIndex;
    property AllowAllUnchecked;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSize;
    property ButtonStyle;
    property Caption;
    property Checked;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCombo;
    property DropDownMenu;
    property Enabled;
    property Font;
    property GlyphSpacing;
    property ImageIndex;
    property Images;
    property Layout;
    property Margins;
    property ModalResult;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Repeating;
    property RepeatDelay;
    property RepeatInterval;
    property ShowAccelChar;
    property ShowHint;
    property SmartFocus;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXCustomCheckBox }

  TTBXCustomCheckBox = class(TTBXTextObject)
  private
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FOnChange: TNotifyEvent;
    function GetChecked: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetChecked(Value: Boolean);
    procedure SetState(Value: TCheckBoxState);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure Click; override;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); override;
    procedure DoChange; virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    function  DoSetState(var NewState: TCheckBoxState): Boolean; virtual;
    function  GetGlyphSize: Integer;
    function  GetFocusRect(const R: TRect): TRect; override;
    function  GetTextAlignment: TAlignment; override;
    function  GetTextMargins: TRect; override;
    procedure Paint; override;
    procedure Toggle; virtual;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Checked: Boolean read GetChecked write SetChecked stored False;
    property SmartFocus default True;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTBXCheckBox = class(TTBXCustomCheckBox)
  published
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property SmartFocus;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnChange;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXCustomRadioButton }

  TTBXCustomRadioButton = class(TTBXTextObject)
  private
    FChecked: Boolean;
    FGroupIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetChecked(Value: Boolean);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetGroupIndex(Value: Integer);
    procedure TurnSiblingsOff;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure Click; override;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); override;
    procedure DoChange; virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    function  DoSetChecked(var Value: Boolean): Boolean; virtual;
    function  GetGlyphSize: Integer;
    function  GetFocusRect(const R: TRect): TRect; override;
    function  GetTextAlignment: TAlignment; override;
    function  GetTextMargins: TRect; override;
    procedure Paint; override;
    property Checked: Boolean read FChecked write SetChecked default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property SmartFocus default True;
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTBXRadioButton = class(TTBXCustomRadioButton)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property Margins;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property SmartFocus;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnChange;
    property OnClick;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TTBXPageScroller }

  TTBXPageScrollerOrientation = (tpsoVertical, tpsoHorizontal);
  TTBXPageScrollerButtons = set of (tpsbPrev, tpsbNext);

  TTBXCustomPageScroller = class(TWinControl)
  private
    FAutoRangeCount: Integer;
    FAutoRange: Boolean;
    FAutoScroll: Boolean;
    FButtonSize: Integer;
    FMargin: Integer;
    FOrientation: TTBXPageScrollerOrientation;
{$IFNDEF JR_D7}
    FParentBackground: Boolean;
{$ENDIF}
    FPosition: Integer;
    FPosRange: Integer;
    FRange: Integer;
    FScrollDirection: Integer;
    FScrollCounter: Integer;
    FScrollPending: Boolean;
    FScrollTimer: TTimer;
    FUpdatingButtons: Boolean;
    FVisibleButtons: TTBXPageScrollerButtons;
    procedure CalcAutoRange;
    function  IsRangeStored: Boolean;
    procedure ScrollTimerTimer(Sender: TObject);
    procedure SetButtonSize(Value: Integer);
    procedure SetAutoRange(Value: Boolean);
    procedure SetOrientation(Value: TTBXPageScrollerOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure StopScrolling;
    procedure ValidatePosition(var NewPos: Integer);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseLeave(var Message: TMessage); message $2A2 {WM_NCMOUSELEAVE};
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function  AutoScrollEnabled: Boolean; virtual;
    procedure BeginScrolling(HitTest: Integer);
    function  CalcClientArea: TRect;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoSetRange(Value: Integer); virtual;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); virtual;
    procedure HandleScrollTimer; virtual;
    procedure Loaded; override;
    procedure RecalcNCArea;
    procedure Resizing; virtual;
{$IFNDEF JR_D7}
    procedure SetParentBackground(Value: Boolean); virtual;
{$ENDIF}
    procedure UpdateButtons;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 10;
    property Orientation: TTBXPageScrollerOrientation read FOrientation write SetOrientation default tpsoVertical;
{$IFDEF JR_D7}
    property ParentBackground default False;
{$ELSE}
    property ParentBackground: Boolean read FParentBackground write SetParentBackground default False;
{$ENDIF}
    property Position: Integer read FPosition write SetPosition default 0;
    property Margin: Integer read FMargin write FMargin default 0;
    property Range: Integer read FRange write SetRange stored IsRangeStored;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure ScrollToCenter(ARect: TRect); overload;
    procedure ScrollToCenter(AControl: TControl); overload;
    property AutoRange: Boolean read FAutoRange write SetAutoRange default False;
  end;

  TTBXPageScroller = class(TTBXCustomPageScroller)
  public
    property Position;
  published
    property Align;
    property Anchors;
    property AutoRange;
    property AutoScroll;
    property ButtonSize;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DoubleBuffered;
    property Enabled;
    property Ctl3D;
    property Font;
    property Margin;
    property Orientation;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Range;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses TB2Dock, TB2Item, TB2Common;

const
  ScrollDelay = 300;
  ScrollInterval = 75;

var
  MouseTimer: TTimer = nil;
  MouseInObject: TTBXControl = nil;
  ObjectCount: Integer = 0;

function IsActivated(AWinControl: TWinControl): Boolean;
var
  C: TWinControl;
begin
  { Returns true if AWinControl contains a focused control }
  C := Screen.ActiveControl;
  Result := True;
  while C <> nil do
    if C = AWinControl then Exit
    else C := C.Parent;
  Result := False;
end;

function GetMinControlHeight(Control: TControl): Integer;
begin
  if Control.Align = alClient then
  begin
    if Control is TTBXControl then Result := TTBXControl(Control).GetMinHeight
    else Result := Control.Constraints.MinHeight;
  end
  else Result := Control.Height;
end;

function GetMinControlWidth(Control: TControl): Integer;
begin
  if Control.Align = alClient then
  begin
    if Control is TTBXControl then Result := TTBXControl(Control).GetMinWidth
    else Result := Control.Constraints.MinWidth;
  end
  else Result := Control.Width;
end;

procedure ApplyMargins(var R: TRect; const Margins: TTBXControlMargins); overload;
begin
  with Margins do
  begin
    Inc(R.Left, Left); Inc(R.Top, Top);
    Dec(R.Right, Right); Dec(R.Bottom, Bottom);
  end;
end;

procedure ApplyMargins(var R: TRect; const Margins: TRect); overload;
begin
  with Margins do
  begin
    Inc(R.Left, Left); Inc(R.Top, Top);
    Dec(R.Right, Right); Dec(R.Bottom, Bottom);
  end;
end;

function GetRealAlignment(Control: TControl): TAlignment;
const
  ReverseAlignment: array [TAlignment] of TAlignment = (taRightJustify, taLeftJustify, taCenter);
begin
  Result := Control.GetControlsAlignment;
  if Control.UseRightToLeftAlignment then Result := ReverseAlignment[Result];
end;

//----------------------------------------------------------------------------//

{ TTBXControlMargins }

procedure TTBXControlMargins.Assign(Src: TPersistent);
begin
  inherited;
  Modified;
end;

procedure TTBXControlMargins.Modified;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTBXControlMargins.SetBottom(Value: Integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Modified;
  end;
end;

procedure TTBXControlMargins.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Modified;
  end;
end;

procedure TTBXControlMargins.SetRight(Value: Integer);
begin
  if FRight  <> Value then
  begin
    FRight := Value;
    Modified;
  end;
end;

procedure TTBXControlMargins.SetTop(Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Modified;
  end;
end;

//----------------------------------------------------------------------------//

{ TTBXControl }

procedure TTBXControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    RemoveMouseTimer;
    DoMouseLeave;
    Invalidate;
    Perform(WM_CANCELMODE, 0, 0);
  end;
end;

procedure TTBXControl.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
//  Invalidate;
end;

constructor TTBXControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents, csDoubleClicks] - [csOpaque];
  if MouseTimer = nil then
  begin
    MouseTimer := TTimer.Create(nil);
    MouseTimer.Enabled := False;
    MouseTimer.Interval := 125;
  end;
  Inc(ObjectCount);
  FFullRedraw := True;
  ParentBackground := True;
  AddThemeNotification(Self);
end;

procedure TTBXControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) and not FullRedraw then
    Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TTBXControl.Destroy;
begin
  RemoveThemeNotification(Self);
  RemoveMouseTimer;
  Dec(ObjectCount);
  if ObjectCount = 0 then
  begin
    MouseTimer.Free;
    MouseTimer := nil;
  end;
  inherited;
end;

procedure TTBXControl.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TTBXControl.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

function TTBXControl.GetMinHeight: Integer;
begin
  Result := Height;
end;

function TTBXControl.GetMinWidth: Integer;
begin
  Result := Width;
end;

procedure TTBXControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FInSpaceClick := False;
  if SpaceAsClick and (Key = VK_SPACE) then
  begin
    FPushed := True;
    FInSpaceClick := True;
    Invalidate;
  end;
end;

procedure TTBXControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if SpaceAsClick and Pushed and (Key = VK_SPACE) then
  begin
    FPushed := False;
    Click;
    Invalidate;
  end;
  FInSpaceClick := False;
  inherited;
end;

procedure TTBXControl.MakeVisible;

  procedure HandleScroll(SW: TControl);
  begin
    if SW is TScrollingWinControl then TScrollingWinControl(SW).ScrollInView(Self)
    else if SW is TTBXCustomPageScroller then TTBXCustomPageScroller(SW).ScrollToCenter(Self)
    else if (Parent <> nil) and (Parent <> SW) then HandleScroll(Parent);
  end;

begin
  HandleScroll(Parent);
end;

procedure TTBXControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FInSpaceClick := False;
  if (Button = mbLeft) and not FPushed then
  begin
    FPushed := True;
    Invalidate;
  end;
  if Enabled then MouseEntered;
  if not SmartFocus and CanFocus then SetFocus
  else if SmartFocus and CanFocus and Assigned(Parent) and IsActivated(Parent) then
  begin
    FDisableScroll := True;
    SetFocus;
    FDisableScroll := False;
  end;
  inherited;
end;

procedure TTBXControl.MouseEntered;
begin
  if Enabled and not FMouseInControl then
  begin
    FMouseInControl := True;
    DoMouseEnter;
  end;
end;

procedure TTBXControl.MouseLeft;
begin
  if Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    RemoveMouseTimer;
    DoMouseLeave;
    Invalidate;
  end;
end;

procedure TTBXControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  DragTarget: TControl;
begin
  P := ClientToScreen(Point(X, Y));
  DragTarget := FindDragTarget(P, True);
  if (MouseInObject <> Self) and (DragTarget = Self) then
  begin
    if Assigned(MouseInObject) then MouseInObject.MouseLeft;
    MouseInObject := Self;
    MouseTimer.OnTimer := MouseTimerHandler;
    MouseTimer.Enabled := True;
    MouseEntered;
  end
  else if (DragTarget <> Self) and (Mouse.Capture = Handle) and FMouseInControl then
  begin
    MouseLeft;
  end;
  inherited;
end;

procedure TTBXControl.MouseTimerHandler(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if FindDragTarget(P, True) <> Self then MouseLeft;
end;

procedure TTBXControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPushed := False;
  Invalidate;
  inherited;
end;

procedure TTBXControl.RemoveMouseTimer;
begin
  if MouseInObject = Self then
  begin
    MouseTimer.Enabled := False;
    MouseInObject := nil;
  end;
end;

procedure TTBXControl.SetAccelVisibility(DoShow: Boolean);
begin
  DoShow := DoShow or AreKeyboardCuesEnabled;
  if DoShow <> (csShowAccels in ControlStateEx) then
  begin
    if DoShow then Include(ControlStateEx, csShowAccels)
    else Exclude(ControlStateEx, csShowAccels);
    if HandleAllocated and IsWindowVisible(Handle) then Invalidate;
  end;
end;

procedure TTBXControl.SetFullRedraw(Value: Boolean);
begin
  if FFullRedraw <> Value then
  begin
    FFullRedraw := Value;
    if HandleAllocated then RecreateWnd;
  end;
end;

{$IFNDEF JR_D7}
procedure TTBXControl.SetParentBackground(Value: Boolean);
begin
  if FParentBackground <> Value then
  begin
    FParentBackground := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TTBXControl.TBMThemeChange(var Message);
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := ClientRect;
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TTBXControl.WMEraseBkgnd(var Message: TMessage);
begin
  if not DoubleBuffered or (Message.wParam = Message.lParam) then
  begin
    if ParentBackground then
      DrawParentBackground(Self, TWMEraseBkgnd(Message).DC, ClientRect)
    else
      Windows.FillRect(TWMEraseBkgnd(Message).DC, ClientRect, Brush.Handle);
  end;
  Message.Result := 1;
end;

procedure TTBXControl.WMKillFocus(var Message: TMessage);
begin
  FPushed := False;
  FInSpaceClick := False;
  Invalidate;
  inherited;
end;

procedure TTBXControl.WMSetFocus(var Message: TMessage);
begin
  inherited;
  FInSpaceClick := False;
  if not FDisableScroll then MakeVisible;
  Invalidate;
end;

//----------------------------------------------------------------------------//

{ TTBXTextObject }

procedure TTBXTextObject.AdjustFont(AFont: TFont);
begin
end;

procedure TTBXTextObject.AdjustHeight;
var
  NewHeight: Integer;
begin
  if HandleAllocated and not FUpdatingSize and ([csReading, csLoading] * ComponentState = []) and AutoSize then
  begin
    FUpdatingSize := True;
    try
      NewHeight := 0;
      DoAdjustHeight(StockCompatibleBitmap.Canvas, NewHeight);
      SetBounds(Left, Top, Width, NewHeight);
    finally
      FUpdatingSize := False;
    end;
  end;
end;

function TTBXTextObject.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if not FUpdatingSize and ([csReading, csLoading] * ComponentState = []) and AutoSize then
  begin
    FUpdatingSize := True;
    try
      NewHeight := 0;
      DoAdjustHeight(StockCompatibleBitmap.Canvas, NewHeight);
      Result := True;
    finally
      FUpdatingSize := False;
    end;
  end
  else Result := False;
end;

procedure TTBXTextObject.CMDialogChar(var Message: TCMDialogChar);

  function IsAccelW: Boolean;
  var
    W: WideChar;
  begin
    W := FindAccelCharW(Caption);
    Result := (W <> #0) and (W = LocaleCharToUnicode(Message.CharCode));
  end;

begin
  if Enabled and Visible and ShowAccelChar and IsAccelW then
    if HandleAccelChar then
    begin
      Message.Result := 1;
    end;
  inherited;
end;

procedure TTBXTextObject.CMDialogKey(var Message: TCMDialogKey);
begin
  if HandleDialogKey(Message.CharCode, KeyDataToShiftState(Message.KeyData)) then
    Message.Result := 1
  else
    inherited;
end;

procedure TTBXTextObject.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TTBXTextObject.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXTextObject.CMHintShow(var Message: TCMHintShow);
begin
  if Length(Hint) > 0 then
    with Message.HintInfo^ do
    begin
      HintStr := UTF8Encode(GetShortHintW(Hint));
      HintWindowClass := TTBHintWindow;
    end;
end;

procedure TTBXTextObject.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not FUpdatingCaption then Caption := inherited Caption;
end;

constructor TTBXTextObject.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks];
  FMargins := TTBXControlMargins.Create;
  FMargins.OnChange := MarginsChangeHandler;
  FShowAccelChar := True;
  DoubleBuffered := True;
  ParentBackground := True;
  AutoSize := True;
  Width := 100;
end;

procedure TTBXTextObject.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    with Params.WindowClass do style := style or CS_HREDRAW;
end;

procedure TTBXTextObject.CreateWnd;
const
  WM_CHANGEUISTATE = $0127;
  WM_QUERYUISTATE  = $0129;
  UIS_INITIALIZE = 3;
  UISF_HIDEACCEL = $2;
var
  B: Boolean;
begin
  inherited;
  SendMessage(Handle, WM_CHANGEUISTATE, UIS_INITIALIZE, 0);
  B := (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL = 0);
  SetAccelVisibility(B);
end;

procedure TTBXTextObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Caption', ReadCaptionProperty, WriteCaptionProperty, Length(Caption) > 0);
  Filer.DefineProperty('Hint', ReadHintProperty, nil, False);
  Filer.DefineProperty('Hint_W', ReadHintProperty, WriteHintProperty, Length(Hint) > 0);
end;

destructor TTBXTextObject.Destroy;
begin
  FMargins.Free;
  inherited;
end;

procedure TTBXTextObject.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
const
  WordWraps: array [TTextWrapping] of Word = (0, DT_END_ELLIPSIS, DT_PATH_ELLIPSIS, DT_WORDBREAK);
var
  R: TRect;
  EffectiveMargins: TRect;
begin
  R := ClientRect;
  EffectiveMargins := GetTextMargins;
  with Margins do
  begin
    Inc(EffectiveMargins.Left, Left);  Inc(EffectiveMargins.Right, Right);
    Inc(EffectiveMargins.Top, Top);    Inc(EffectiveMargins.Bottom, Bottom);
  end;
  ApplyMargins(R, EffectiveMargins);
  NewHeight := DoDrawText(ACanvas, R, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[Wrapping]);
  with EffectiveMargins do Inc(NewHeight, Top + Bottom);
end;

function TTBXTextObject.DoDrawText(ACanvas: TCanvas; var Rect: TRect; Flags: Integer): Integer;
var
  Text: WideString;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  Flags := DrawTextBiDiModeFlags(Flags);

  ACanvas.Font := Font;
  AdjustFont(ACanvas.Font);

  if Flags and DT_CALCRECT = DT_CALCRECT then
  begin
    Flags := Flags and not DT_VCENTER;
    Result := _DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  end
  else if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    ACanvas.Font.Color := clBtnHighlight;
    _DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    ACanvas.Font.Color := clBtnShadow;
    Result := _DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  end
  else
  begin
    Result := _DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  end;
end;

procedure TTBXTextObject.DoMarginsChanged;
begin
  Invalidate;
  AdjustHeight;
end;

function TTBXTextObject.GetControlsAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TTBXTextObject.GetFocusRect(const R: TRect): TRect;
begin
  { R is the client rectangle without the margins }
  Result := Rect(0, 0, 0, 0);
end;

function TTBXTextObject.GetLabelText: WideString;
begin
  Result := Caption;
end;

function TTBXTextObject.GetTextAlignment: TAlignment;
begin
  Result := Alignment;
end;

function TTBXTextObject.GetTextFlags: Cardinal;
const
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
begin
  Result := DT_EXPANDTABS or WordWraps[Wrapping] or Alignments[GetRealAlignment(Self)];

  if ShowAccelChar then
  begin
    if not AreKeyboardCuesEnabled and not (csShowAccels in ControlStateEx) then
      Result := Result or DT_HIDEPREFIX;
  end
  else Result := Result or DT_NOPREFIX;
end;

function TTBXTextObject.GetTextMargins: TRect;
const
  ZeroRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
begin
  Result := ZeroRect;
end;

function TTBXTextObject.HandleAccelChar: Boolean;
begin
  Result := True;
  Click;
end;

function TTBXTextObject.HandleDialogKey(CharCode: Word; ShiftState: TShiftState): Boolean;
begin
  Result := (CharCode = VK_RETURN) and Focused and (ShiftState = []);
  if Result then Click;
end;

procedure TTBXTextObject.Loaded;
begin
  inherited;
  AdjustHeight;
end;

procedure TTBXTextObject.MarginsChangeHandler(Sender: TObject);
begin
  DoMarginsChanged;
end;

procedure TTBXTextObject.Paint;
var
  R, R2: TRect;
  DrawStyle: Longint;
  CaptionHeight: Integer;
begin
  with Canvas do
  begin
    R := ClientRect;
    ApplyMargins(R, Margins);
    if Focused then DrawFocusRect2(Canvas.Handle, GetFocusRect(R));
    DrawStyle := GetTextFlags;
    Brush.Style := bsClear;
    ApplyMargins(R, GetTextMargins);
    R2 := R;
    CaptionHeight := DoDrawText(Canvas, R2, DrawStyle or DT_CALCRECT);
    R.Top := (R.Top + R.Bottom - CaptionHeight) div 2;
    R.Bottom := R.Top + CaptionHeight;
    DoDrawText(Canvas, R, DrawStyle);
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXTextObject.ReadCaptionProperty(Reader: TReader);
begin
  Caption := FilerReadWideString(Reader);
end;

procedure TTBXTextObject.ReadHintProperty(Reader: TReader);
begin
  Hint := FilerReadWideString(Reader);
end;

procedure TTBXTextObject.SetAlignment(Value: TLeftRight);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TTBXTextObject.SetCaption(const Value: WideString);
begin
  if not FUpdatingCaption and (FCaption <> Value) then
  begin
    FUpdatingCaption := True;
    try
      FCaption := Value;
      AdjustHeight;
      Invalidate;
    finally
      FUpdatingCaption := False;
    end;
  end
end;

procedure TTBXTextObject.SetHint(const Value: WideString);
begin
  if FHint <> Value then
  begin
    FHint := Value;
  end;
end;

procedure TTBXTextObject.SetMargins(Value: TTBXControlMargins);
begin
  FMargins.Assign(Value);
end;

procedure TTBXTextObject.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    AdjustHeight;
    Invalidate;
  end;
end;

procedure TTBXTextObject.SetWrapping(Value: TTextWrapping);
begin
  FWrapping := Value;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXTextObject.WriteCaptionProperty(Writer: TWriter);
begin
  FilerWriteWideString(Writer, Caption);
end;

procedure TTBXTextObject.WriteHintProperty(Writer: TWriter);
begin
  FilerWriteWideString(Writer, Hint);
end;

//----------------------------------------------------------------------------//

{ TTBXCustomLabel }

constructor TTBXCustomLabel.Create(AOwner: TComponent);
begin
  inherited;
  Wrapping := twWrap;
  FUnderlineColor := clBtnShadow;
  TabStop := False;
end;

function TTBXCustomLabel.GetTextMargins: TRect;
const
  BottomMargin: array [Boolean] of Integer = (0, 1);
begin
  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Result.Bottom := BottomMargin[Underline];
  end;
end;

function TTBXCustomLabel.HandleAccelChar: Boolean;
begin
  Result := False;
  if FFocusControl <> nil then
    with FFocusControl do if CanFocus then
    begin
      SetFocus;
      Result := True;
    end;
end;

procedure TTBXCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then FFocusControl := nil;
end;

procedure TTBXCustomLabel.Paint;
var
  Rect: TRect;
begin
  inherited;
  if Underline then
  begin
    Rect := ClientRect;
    ApplyMargins(Rect, Margins);
    ApplyMargins(Rect, GetTextMargins);
    DrawLineEx(Canvas.Handle, Rect.Left, Rect.Bottom, Rect.Right, Rect.Bottom, UnderlineColor);
  end;
end;

procedure TTBXCustomLabel.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl <> Value then
  begin
    if FFocusControl <> nil then FFocusControl.RemoveFreeNotification(Self);
    FFocusControl := Value;
    if FFocusControl <> nil then FFocusControl.FreeNotification(Self);
  end;
end;

procedure TTBXCustomLabel.SetUnderline(Value: Boolean);
begin
  if Value <> FUnderline then
  begin
    FUnderline := Value;
    Invalidate;
    AdjustHeight;
  end;
end;

procedure TTBXCustomLabel.SetUnderlineColor(Value: TColor);
begin
  FUnderlineColor := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------//

{ TTBXCustomLink }

procedure TTBXCustomLink.AdjustFont(AFont: TFont);
begin
  if MouseInControl then AFont.Style := AFont.Style + [fsUnderline];
end;

constructor TTBXCustomLink.Create(AOwner: TComponent);
begin
  inherited;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  SmartFocus := True;
  SpaceAsClick := True;
  TabStop := True;
  Cursor := crHandPoint;
end;

destructor TTBXCustomLink.Destroy;
begin
  FImageChangeLink.Free;
  inherited;
end;

procedure TTBXCustomLink.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
begin
  inherited DoAdjustHeight(ACanvas, NewHeight);
  if Images <> nil then
    if NewHeight < Images.Height + 4 then NewHeight := Images.Height + 4;
end;

procedure TTBXCustomLink.DoMouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TTBXCustomLink.DoMouseLeave;
begin
  inherited;
  Invalidate;
end;

function TTBXCustomLink.GetControlsAlignment: TAlignment;
begin
  Result := GetTextAlignment;
end;

function TTBXCustomLink.GetFocusRect(const R: TRect): TRect;
const
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE or DT_VCENTER,
    DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS, DT_WORDBREAK);
var
  TR: TRect;
  ShowImage: Boolean;
begin
  Result := R;
  ShowImage := Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count);

  { Text Rectangle }
  TR := R;
  ApplyMargins(TR, GetTextMargins);
  DoDrawText(Canvas, TR, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[Wrapping] or DT_LEFT);

  if ShowImage then
  begin
    if GetRealAlignment(Self) = taLeftJustify then
    begin
      Result.Left := R.Left;
      Result.Right := TR.Right;
    end
    else
    begin
      Result.Left := TR.Left;
      Result.Right := R.Right;
    end;
  end
  else
  begin
    Result.Right := TR.Right;
    Result.Left := TR.Left;
  end;
  Dec(Result.Left, 2);
  Inc(Result.Right, 2);
end;

function TTBXCustomLink.GetTextAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TTBXCustomLink.GetTextMargins: TRect;
begin
  Result := Rect(2, 1, 2, 1);
  if Assigned(Images) then with Result do
  begin
    if GetRealAlignment(Self) = taLeftJustify then Inc(Left, Images.Width + 5)
    else Inc(Right, Images.Width + 5);
  end;
end;

function TTBXCustomLink.HandleAccelChar: Boolean;
begin
  Click;
  Result := True;
end;

procedure TTBXCustomLink.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
  begin
    Invalidate;
    AdjustHeight;
  end;
end;

procedure TTBXCustomLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = Images) and (Operation = opRemove) then Images := nil;
end;

procedure TTBXCustomLink.Paint;
var
  Rect, R: TRect;
begin
  inherited;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
    with Canvas do
    begin
      Rect := ClientRect;
      ApplyMargins(Rect, Margins);

      if GetRealAlignment(Self) = taLeftJustify then R.Left := Rect.Left + 2
      else R.Left := Rect.Right - 2 - Images.Width;
      
      R.Top := (Rect.Top + Rect.Bottom - Images.Height) div 2;
      R.Right := R.Left + Images.Width;
      R.Bottom := R.Top + Images.Height;

      if Enabled then Images.Draw(Canvas, R.Left, R.Top, ImageIndex)
      else DrawTBXImage(Canvas.Handle, R, Images, ImageIndex, ISF_DISABLED);
    end;
end;

procedure TTBXCustomLink.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if Assigned(Images) then Invalidate;
  end;
end;

procedure TTBXCustomLink.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXCustomLink.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    P := ScreenToClient(SmallPointToPoint(Message.Pos));
    R := ClientRect;
    ApplyMargins(R, Margins);
    R := GetFocusRect(R);
    if not PtInRect(R, P) then Message.Result := HTTRANSPARENT;
  end;
end;

//----------------------------------------------------------------------------//

{ TTBXCustomButton }

function TTBXCustomButton.ArrowVisible: Boolean;
begin
  Result := DropDownMenu <> nil;
end;

procedure TTBXCustomButton.Click;
var
  Form: TCustomForm;
  Pt: TPoint;
  R: TRect;
  SaveAlignment: TPopupAlignment;

  procedure RemoveClicks;
  var
    RepostList: TList;
    Repost: Boolean;
    I: Integer;
    Msg: TMsg;
    P: TPoint;
  begin
    RepostList := TList.Create;
    try
      while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK, PM_REMOVE) do
        with Msg do
        begin
          Repost := True;
          case Message of
            WM_QUIT: begin
                { Throw back any WM_QUIT messages }
                PostQuitMessage(wParam);
                Break;
              end;
            WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
            WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
            WM_MBUTTONDOWN, WM_MBUTTONDBLCLK: begin
                P := SmallPointToPoint(TSmallPoint(lParam));
                Windows.ClientToScreen(hwnd, P);
                if FindDragTarget(P, True) = Self then Repost := False;
              end;
          end;
          if Repost then
          begin
            RepostList.Add(AllocMem(SizeOf(TMsg)));
            PMsg(RepostList.Last)^ := Msg;
          end;
        end;
    finally
      for I := 0 to RepostList.Count-1 do
      begin
        with PMsg(RepostList[I])^ do PostMessage(hwnd, message, wParam, lParam);
        FreeMem(RepostList[I]);
      end;
      RepostList.Free;
    end;
  end;

begin
  if FRepeating and not FMenuVisible then inherited
  else
  try
    FInClick := True;
    if (GroupIndex <> 0) and not FMenuVisible then SetChecked(not Checked);
    MouseLeft;
    if (DropDownMenu = nil) or (DropDownCombo and not FMenuVisible) then
    begin
      if ModalResult <> 0 then
      begin
        Form := GetParentForm(Self);
        if Form <> nil then Form.ModalResult := ModalResult;
      end;
      inherited;
    end
    else
    begin
      MouseCapture := False;
      SaveAlignment := paLeft; // to avoid compiler warnings
      if DoDropDown then
      try
        Pt := Point(0, Height);
        Pt := ClientToScreen(Pt);
        SaveAlignment := DropDownMenu.Alignment;
        DropDownMenu.PopupComponent := Self;
        FMenuVisible := True;
        Invalidate;
        Update;

        if DropDownMenu is TTBXPopupMenu then
        begin
          R := ClientRect;
          ApplyMargins(R, Margins);
          R.TopLeft := ClientToScreen(R.TopLeft);
          R.BottomRight := ClientToScreen(R.BottomRight);
          TTBXPopupMenu(DropDownMenu).PopupEx(R);
        end
        else DropDownMenu.Popup(Pt.X, Pt.Y);
      finally
        FMenuVisible := False;
        DropDownMenu.Alignment := SaveAlignment;
        if Pushed then FPushed := False;
        Invalidate;
        RemoveClicks;
      end
      else inherited;
    end;
  finally
    FInClick := False;
  end;
end;

constructor TTBXCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taCenter;
  FBorderSize := 4;
  FGlyphSpacing := 4;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FRepeatDelay := 400;
  FRepeatInterval := 100;
  SmartFocus := True;
  SpaceAsClick := True;
  TabStop := True;
end;

destructor TTBXCustomButton.Destroy;
begin
  FImageChangeLink.Free;
  inherited;
end;

procedure TTBXCustomButton.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
var
  Sz: Integer;
begin
  if Length(GetLabelText) = 0 then
  begin
    if Images <> nil then NewHeight := Images.Height + BorderSize * 2
    else if BorderSize * 2 >= 16 then NewHeight := BorderSize * 2
    else NewHeight := 16;
  end
  else
  begin
    inherited DoAdjustHeight(ACanvas, NewHeight);
    if Images <> nil then
      if Layout in [blGlyphLeft, blGlyphRight] then
      begin
        Sz := Images.Height + BorderSize * 2;
        if NewHeight < Sz then NewHeight := Sz;
      end;
  end;
end;

function TTBXCustomButton.DoDrawText(ACanvas: TCanvas; var Rect: TRect; Flags: Integer): Integer;
var
  ItemInfo: TTBXItemInfo;
  S: WideString;
begin
  S := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((S = '') or
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  Flags := DrawTextBiDiModeFlags(Flags);
  ACanvas.Font := Font;
  AdjustFont(ACanvas.Font);

  if Flags and DT_CALCRECT = DT_CALCRECT then
  begin
    Flags := Flags and not DT_VCENTER;
    Result := _DrawTextW(ACanvas.Handle, PWideChar(S), Length(S), Rect, Flags);
  end
  else
  begin
    GetItemInfo(ItemInfo);
    CurrentTheme.PaintCaption(ACanvas.Handle, Rect, ItemInfo, S, Flags, clDefault);
    Flags := Flags or DT_CALCRECT;
    Result := _DrawTextW(ACanvas.Handle, PWideChar(S), Length(S), Rect, Flags);
  end;
end;

function TTBXCustomButton.DoDropDown: Boolean;
begin
  Result := FDropDownMenu <> nil;
  if Result and Assigned(FOnDropDown) then FOnDropDown(Self, Result);
end;

procedure TTBXCustomButton.DoMouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TTBXCustomButton.DoMouseLeave;
begin
  inherited;
  Invalidate;
end;

function TTBXCustomButton.GetControlsAlignment: TAlignment;
begin
  Result := GetTextAlignment;
end;

function TTBXCustomButton.GetFocusRect(const R: TRect): TRect;
begin
  Result := R;
  InflateRect(Result, -2, -2);
end;

procedure TTBXCustomButton.GetItemInfo(out ItemInfo: TTBXItemInfo);
const
  ViewTypes: array [TButtonStyle] of Integer = (VT_BARS or VT_EMBEDDED, VT_BARS);
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.ViewType := ViewTypes[ButtonStyle];
  ItemInfo.Enabled := Enabled;
  ItemInfo.ItemOptions := IO_TOOLBARSTYLE or IO_APPACTIVE;
  if csDesigning in ComponentState then ItemInfo.ItemOptions := ItemInfo.ItemOptions or IO_DESIGNING;
  ItemInfo.Pushed := Pushed and (MouseInControl or FMenuVisible or InSpaceClick);
  if FMenuVisible then
  begin
    ItemInfo.IsPopupParent := True;
    ItemInfo.Pushed := not DropDownCombo;
  end;
  ItemInfo.Selected := Checked;
  ItemInfo.IsVertical := False;
  if ArrowVisible and DropDownCombo then ItemInfo.ComboPart := cpCombo;
  if MouseInControl or FMenuVisible then ItemInfo.HoverKind := hkMouseHover;
end;

function TTBXCustomButton.GetTextAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TTBXCustomButton.GetTextMargins: TRect;
var
  L, Sz: Integer;
  IsSpecialDropDown: Boolean;
begin
  Result := Rect(BorderSize, BorderSize, BorderSize, BorderSize);
  L := Length(GetLabelText);
  if (Images <> nil) and (L > 0) then Sz := GlyphSpacing
  else Sz := 0;
  if Assigned(Images) then with Result do
  case Layout of
    blGlyphLeft: Inc(Left, Images.Width + Sz);
    blGlyphTop: Inc(Top, Images.Height + Sz);
    blGlyphRight: Inc(Right, Images.Width + Sz);
    blGlyphBottom: Inc(Bottom, Images.Height + Sz);
  end;
  if ArrowVisible then
  begin
    if DropDownCombo then Inc(Result.Right, CurrentTheme.SplitBtnArrowWidth)
    else
    begin
      IsSpecialDropDown := (L > 0) and (Images <> nil) and (Layout in [blGlyphTop, blGlyphBottom]);
      if not IsSpecialDropDown then Inc(Result.Right, CurrentTheme.DropDownArrowWidth);
    end;
  end;
end;

procedure TTBXCustomButton.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
  begin
    Invalidate;
    AdjustHeight;
  end;
end;

procedure TTBXCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F4) and DropDownCombo and Assigned(DropDownMenu) then
  begin
    FMenuVisible := True;
    try
      Click;
    finally
      FMenuVisible := False;
    end;
    Exit;
  end;
  inherited;
end;

procedure TTBXCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if Enabled and (Button = mbLeft) then
  begin
    R := ClientRect;
    ApplyMargins(R, Margins);
    FMenuVisible := not FInClick and Assigned(DropDownMenu) and
      (not DropDownCombo or (X >= R.Right - CurrentTheme.SplitBtnArrowWidth));
    try
      if FMenuVisible then
      begin
        ControlState := ControlState - [csClicked];
        if not FInClick then
        begin
          Click;
        end;
      end
      else if Repeating then
      begin
        Click;
        ControlState := ControlState - [csClicked];
        if not Assigned(FRepeatTimer) then FRepeatTimer := TTimer.Create(Self);
        FRepeatTimer.Interval := RepeatDelay;
        FRepeatTimer.OnTimer := RepeatTimerHandler;
        FRepeatTimer.Enabled := True;
      end;
    finally
      FMenuVisible := False;
    end;
  end;
end;

procedure TTBXCustomButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FRepeatTimer) and PtInButtonPart(Point(X, Y)) then FRepeatTimer.Enabled := True;
end;

procedure TTBXCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TTBXCustomButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then Images := nil
    else if AComponent = DropdownMenu then DropdownMenu := nil;
  end;
end;

procedure TTBXCustomButton.Paint;
const
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
  ShowAccelChars: array [Boolean] of Integer = (DT_NOPREFIX, 0);
var
  CR, IR, TR: TRect;
  W, X: Integer;
  Text: WideString;
  ItemInfo: TTBXItemInfo;
  RealAlignment: TAlignment;
  CaptionHeight: Integer;
  DrawStyle: Cardinal;
  ShowArrow: Boolean;
begin
  CR := ClientRect;
  ApplyMargins(CR, Margins);

  ShowArrow := ArrowVisible;
  GetItemInfo(ItemInfo);
  if ShowArrow and DropDownCombo then
  begin
    TR := CR;
    TR.Left := TR.Right - CurrentTheme.SplitBtnArrowWidth;
    CR.Right := TR.Left;

    ItemInfo.ComboPart := cpSplitRight;
    ItemInfo.Pushed := FMenuVisible;
    CurrentTheme.PaintButton(Canvas.Handle, TR, ItemInfo);

    ItemInfo.ComboPart := cpSplitLeft;
    ItemInfo.Pushed := Pushed and not FMenuVisible;
    CurrentTheme.PaintButton(Canvas.Handle, CR, ItemInfo);
  end
  else CurrentTheme.PaintButton(Canvas.Handle, CR, ItemInfo);
  if Focused then DrawFocusRect2(Canvas.Handle, GetFocusRect(CR));
  InflateRect(CR, -BorderSize, -BorderSize);

  if ShowArrow and not DropDownCombo then
  begin
    TR := CR;
    TR.Left := TR.Right - CurrentTheme.DropdownArrowWidth;
    CurrentTheme.PaintDropDownArrow(Canvas.Handle, TR, ItemInfo);
    CR.Right := TR.Left - CurrentTheme.DropdownArrowMargin;
  end;

  Text := GetLabelText;
  DrawStyle := 0;

  if (Length(Text) > 0) or (Images <> nil) then
  begin
    RealAlignment := GetRealAlignment(Self);

    if Length(Text) = 0 then
    begin
      IR.Top := (CR.Top + CR.Bottom - Images.Height) div 2;
      IR.Bottom := IR.Top + Images.Height;

      case RealAlignment of
        taLeftJustify: IR.Left := CR.Left;
        taRightJustify: IR.Left := CR.Right - Images.Width;
      else
        IR.Left := (CR.Left + CR.Right - Images.Width) div 2;
      end;
      IR.Right := IR.Left + Images.Width;
    end
    else
    begin
      TR := CR;
      DrawStyle := DT_EXPANDTABS or WordWraps[Wrapping] or
        Alignments[RealAlignment] or ShowAccelChars[ShowAccelChar];
      if (Images = nil) or (Layout in [blGlyphTop, blGlyphBottom]) then
      begin
        CaptionHeight := DoDrawText(Canvas, TR, DrawStyle or DT_CALCRECT);
        TR := CR;
        if Images = nil then
        begin
          TR.Top := (TR.Top + TR.Bottom - CaptionHeight) div 2;
        end
        else
        begin
          TR.Top := (CR.Top + CR.Bottom - Images.Height - GlyphSpacing - CaptionHeight) div 2;
          IR.Top := TR.Top;
          if Layout = blGlyphTop then Inc(TR.Top, Images.Height + GlyphSpacing)
          else Inc(IR.Top, CaptionHeight + GlyphSpacing);
          TR.Bottom := TR.Top + CaptionHeight;
          IR.Bottom := IR.Top + Images.Height;
          case RealAlignment of
            taLeftJustify: IR.Left := CR.Left;
            taRightJustify: IR.Left := CR.Right - Images.Width;
          else
            IR.Left := (CR.Left + CR.Right - Images.Width) div 2;
          end;
          IR.Right := IR.Left + Images.Width;
        end;
      end
      else
      begin
        IR.Left := CR.Left;
        if Layout = blGlyphLeft then Inc(TR.Left, Images.Width + GlyphSpacing)
        else Dec(TR.Right, Images.Width + GlyphSpacing);
        IR.Right := IR.Left + Images.Width;
        IR.Top := (CR.Top + CR.Bottom - Images.Height) div 2;
        IR.Bottom := IR.Top + Images.Height;
        CaptionHeight := DoDrawText(Canvas, TR, DrawStyle or DT_CALCRECT);
        TR.Top := (CR.Top + CR.Bottom - CaptionHeight) div 2;
        TR.Bottom := TR.Top + CaptionHeight;
        W := Images.Width + GlyphSpacing + TR.Right - TR.Left;
        case RealAlignment of
          taLeftJustify: X := CR.Left;
          taRightJustify: X := CR.Right - W;
        else
          X := (CR.Left + CR.Right - W) div 2;
        end;
        case Layout of
          blGlyphLeft:
            begin
              if X < CR.Left then X := CR.Left;
              IR.Left := X;
              IR.Right := X + Images.Width;
              OffsetRect(TR, IR.Right + GlyphSpacing - TR.Left, 0);
              if TR.Right > CR.Right then TR.Right := CR.Right;
              DrawStyle := DrawStyle and not DT_RIGHT and not DT_CENTER or DT_LEFT;
            end;
          blGlyphRight:
            begin
              OffsetRect(TR, X - TR.Left, 0);
              IR.Left := TR.Right + GlyphSpacing;
              IR.Right := IR.Left + Images.Width;
              DrawStyle := DrawStyle and not DT_CENTER and not DT_LEFT or DT_RIGHT;
            end;
        end;
      end;
    end;

    if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
      CurrentTheme.PaintImage(Canvas.Handle, IR, ItemInfo, Images, ImageIndex);    

    if Length(Text) > 0 then
    begin
//      Canvas.Brush.Style := bsClear;

      DoDrawText(Canvas, TR, DrawStyle);
//      Canvas.Brush.Style := bsSolid;
    end;
  end;
end;

function TTBXCustomButton.PtInButtonPart(const Pt: TPoint): Boolean;
var
  R: TRect;
begin
  R := ClientRect;
  ApplyMargins(R, Margins);
  Result := PtInRect(R, Pt);
end;

procedure TTBXCustomButton.RepeatTimerHandler(Sender: TObject);
var
  P: TPoint;
begin
  FRepeatTimer.Interval := RepeatInterval;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if not MouseCapture then
  begin
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end
  else if Repeating and Pushed and PtInButtonPart(P) then Click
  else FRepeatTimer.Enabled := False;
end;

procedure TTBXCustomButton.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TTBXCustomButton.SetAllowAllUnchecked(Value: Boolean);
begin
  if FAllowAllUnchecked <> Value then
  begin
    FAllowAllUnchecked := Value;
    UpdateCheckedState;
  end;
end;

procedure TTBXCustomButton.SetBorderSize(Value: Integer);
begin
  FBorderSize := Value;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXCustomButton.SetButtonStyle(Value: TButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

procedure TTBXCustomButton.SetChecked(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if FChecked <> Value then
  begin
    if FChecked and not AllowAllUnchecked then Exit;
    FChecked := Value;
    Invalidate;
    if Value then UpdateCheckedState;
  end;
end;

procedure TTBXCustomButton.SetDropdownCombo(Value: Boolean);
begin
  FDropdownCombo := Value;
  Invalidate;
end;

procedure TTBXCustomButton.SetDropdownMenu(Value: TPopupMenu);
begin
  if FDropdownMenu <> Value then
  begin
    if FDropDownMenu <> nil then RemoveFreeNotification(FDropDownMenu);
    FDropDownMenu := Value;
    if FDropDownMenu <> nil then FreeNotification(FDropDownMenu);
    Invalidate;
  end;
end;

procedure TTBXCustomButton.SetGlyphSpacing(Value: Integer);
begin
  FGlyphSpacing := Value;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXCustomButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateCheckedState;
  end;
end;

procedure TTBXCustomButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if Assigned(Images) then Invalidate;
  end;
end;

procedure TTBXCustomButton.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXCustomButton.SetLayout(Value: TButtonLayout);
begin
  FLayout := Value;
  Invalidate;
  AdjustHeight;
end;

procedure TTBXCustomButton.UpdateCheckedState;
var
  I: Integer;
  C: TControl;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then with Parent do
    for I := 0 to ControlCount - 1 do
    begin
      C := Controls[I];
      if (C <> Self) and (C is TTBXCustomButton) then
        with TTBXCustomButton(C) do
          if FGroupIndex = Self.FGroupIndex then
          begin
            if Self.Checked and FChecked then
            begin
              FChecked := False;
              Invalidate;
            end;
            FAllowAllUnchecked := Self.AllowAllUnchecked;
          end;
    end;
end;

procedure TTBXCustomButton.WMCancelMode(var Message: TWMCancelMode);
begin
  FRepeatTimer.Free;
  FRepeatTimer := nil;
  MouseLeft;
end;

procedure TTBXCustomButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
  DblClick; 
end;

//----------------------------------------------------------------------------//

procedure TTBXCustomButton.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    P := ScreenToClient(SmallPointToPoint(Message.Pos));
    R := ClientRect;
    ApplyMargins(R, Margins);
    if not PtInRect(R, P) then Message.Result := HTTRANSPARENT;
  end;
end;

{ TTBXAlignmentPanel }

procedure TTBXAlignmentPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  with Margins do
  begin
    Inc(Rect.Left, Left);
    Inc(Rect.Top, Top);
    Dec(Rect.Right, Right);
    Dec(Rect.Bottom, Bottom);
  end;
end;

constructor TTBXAlignmentPanel.Create(AOwner: TComponent);
begin
  inherited;
  ParentBackground := True;
  FMargins := TTBXControlMargins.Create;
  FMargins.OnChange := MarginsChangeHandler;
end;

destructor TTBXAlignmentPanel.Destroy;
begin
  FMargins.Free;
  inherited;
end;

function TTBXAlignmentPanel.GetMinHeight: Integer;
var
  I: Integer;
  Control: TControl;
begin
  Result := 0;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Visible then
      if Control.Align in [alTop, alBottom] then Inc(Result, Control.Height)
      else if Control.Align = alClient then Inc(Result, GetMinControlHeight(Control));
  end;
  Inc(Result, Margins.Top + Margins.Bottom);
end;

function TTBXAlignmentPanel.GetMinWidth: Integer;
var
  I: Integer;
  Control: TControl;
begin
  Result := 0;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Visible then
      if Control.Align in [alLeft, alRight] then Inc(Result, Control.Width)
      else if Control.Align = alClient then Inc(Result, GetMinControlWidth(Control));
  end;
  Inc(Result, Margins.Left + Margins.Right);
end;

procedure TTBXAlignmentPanel.MarginsChangeHandler(Sender: TObject);
begin
  Realign;
  Invalidate;
end;

procedure TTBXAlignmentPanel.Paint;
var
  R: TRect;
  DC: HDC;
begin
  if csDesigning in ComponentState then
  begin
    DC := Canvas.Handle;
    R := ClientRect;
    SaveDC(DC);
    InflateRect(R, -1, -1);
    with R do ExcludeClipRect(DC, Left, Top, Right, Bottom);
    InflateRect(R, 1, 1);
    DitherRect(DC, R, clBtnFace, clBtnShadow);
    RestoreDC(DC, -1);
  end;
end;

procedure TTBXAlignmentPanel.SetMargins(Value: TTBXControlMargins);
begin
  FMargins.Assign(Value);
end;

//----------------------------------------------------------------------------//

{ TTBXCustomPageScroller }

procedure TTBXCustomPageScroller.AdjustClientRect(var Rect: TRect);
begin
  if Orientation = tpsoVertical then
  begin
    if tpsbPrev in FVisibleButtons then Dec(Rect.Top, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Rect.Bottom, ButtonSize);
    OffsetRect(Rect, 0, -Position);
    if Range > Rect.Bottom - Rect.Top then Rect.Bottom := Rect.Top + Range;
  end
  else
  begin
    if tpsbPrev in FVisibleButtons then Dec(Rect.Left, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Rect.Right, ButtonSize);
    OffsetRect(Rect, -Position, 0);
    if Range > Rect.Right - Rect.Left then Rect.Right := Rect.Left + Range;
  end;
end;

procedure TTBXCustomPageScroller.AlignControls(AControl: TControl; var ARect: TRect);
begin
  CalcAutoRange;
  UpdateButtons;
  ARect := ClientRect;
  inherited AlignControls(AControl, ARect);
end;

function TTBXCustomPageScroller.AutoScrollEnabled: Boolean;
begin
  Result := not AutoSize and not (DockSite and UseDockManager);
end;

procedure TTBXCustomPageScroller.BeginScrolling(HitTest: Integer);
var
  Msg: TMsg;
begin
  if HitTest = HTSCROLLPREV then FScrollDirection := -1 else FScrollDirection := 1;
  try
    SetCapture(Handle);
    FScrollCounter := FScrollDirection * 8;
    FScrollPending := True;
    FScrollTimer.Enabled := True;
    DrawNCArea(False, 0, 0);
    HandleScrollTimer;
    FScrollPending := True;
    FScrollTimer.Interval := ScrollDelay;

    while GetCapture = Handle do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP: if Msg.WParam = VK_ESCAPE then Break;
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
            Break;
          end;
        WM_LBUTTONUP:
          begin
            Break;
          end;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:;
        WM_TIMER:
          begin
            HandleScrollTimer;
          end;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    StopScrolling;
    if GetCapture = Handle then ReleaseCapture;
  end;
end;

procedure TTBXCustomPageScroller.CalcAutoRange;
var
  I: Integer;
  Bias: Integer;
  NewRange, AlignMargin: Integer;
  CW, CH: Integer;
  Control: TControl;
begin
  if (FAutoRangeCount <= 0) and AutoRange then
  begin
    if AutoScrollEnabled then
    begin
      NewRange := 0;
      AlignMargin := 0;
      if Position > 0 then Bias := ButtonSize
      else Bias := 0;
      CW := ClientWidth;
      CH := ClientHeight;
      DisableAlign;
      for I := 0 to ControlCount - 1 do
      begin
        Control := Controls[I];
        if Control.Visible or (csDesigning in Control.ComponentState) and
          not (csNoDesignVisible in Control.ControlStyle) then
        begin
          if Orientation = tpsoVertical then
          begin
            if Control.Align in [alTop, alBottom, alClient] then
              Control.Width := CW;
            case Control.Align of
              alTop, alNone:
                if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
                  NewRange := Max(NewRange, Position + Control.Top + Control.Height + Bias);
              alBottom: Inc(AlignMargin, Control.Height);
              alClient: Inc(AlignMargin, GetMinControlHeight(Control));
            end
          end
          else
          begin
            if Control.Align in [alLeft, alRight, alClient] then
              Control.Height := CH;
            case Control.Align of
              alLeft, alNone:
                if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
                  NewRange := Max(NewRange, Position + Control.Left + Control.Width + Bias);
              alRight: Inc(AlignMargin, Control.Width);
              alClient: Inc(AlignMargin, GetMinControlWidth(Control));
            end;
          end;
        end;
      end;
      EnableAlign;
      DoSetRange(NewRange + AlignMargin + Margin);
    end
    else DoSetRange(0);
  end;
end;

function TTBXCustomPageScroller.CalcClientArea: TRect;
begin
  Result := ClientRect;
  if Orientation = tpsoVertical then
  begin
    if tpsbPrev in FVisibleButtons then Dec(Result.Top, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Result.Bottom, ButtonSize);
  end
  else
  begin
    if tpsbPrev in FVisibleButtons then Dec(Result.Left, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Result.Right, ButtonSize);
  end;
end;

function TTBXCustomPageScroller.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := NewHeight > FButtonSize * 3;
end;

procedure TTBXCustomPageScroller.CMParentColorChanged(var Message: TMessage);
begin
  if (Message.WParam = 0) then
  begin
    Message.WParam := 1;
    Message.LParam := GetEffectiveColor(Parent);
  end;
  inherited;
end;

procedure TTBXCustomPageScroller.ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  // do not call inherited here
end;

constructor TTBXCustomPageScroller.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents, csDoubleClicks];
  FAutoScroll := True;
  FButtonSize := 10;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 60;
  FScrollTimer.OnTimer := ScrollTimerTimer;
  Width := 64;
  Height := 64;
end;

procedure TTBXCustomPageScroller.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TTBXCustomPageScroller.DisableAutoRange;
begin
  Inc(FAutoRangeCount);
end;

procedure TTBXCustomPageScroller.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  UpdateButtons;
end;

procedure TTBXCustomPageScroller.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
const
  CBtns: array [TTBXPageScrollerOrientation, Boolean] of Integer =
    ((PSBT_UP, PSBT_DOWN), (PSBT_LEFT, PSBT_RIGHT));
var
  DC: HDC;
  R, CR, BR: TRect;
  PrevBtnSize, NextBtnSize: Integer;
begin
  if FVisibleButtons = [] then Exit;
  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      CR := R;
      PrevBtnSize := 0;
      NextBtnSize := 0;
      if tpsbPrev in FVisibleButtons then PrevBtnSize := ButtonSize;
      if tpsbNext in FVisibleButtons then NextBtnSize := ButtonSize;
      if Orientation = tpsoVertical then
      begin
        Inc(CR.Top, PrevBtnSize);
        Dec(CR.Bottom, NextBtnSize);
      end
      else
      begin
        Inc(CR.Left, PrevBtnSize);
        Dec(CR.Right, NextBtnSize);
      end;
      with CR do ExcludeClipRect(DC, Left, Top, Right, Bottom);
    end;


    FillRectEx(DC, R, Color);
    if tpsbPrev in FVisibleButtons then
    begin
      BR := R;
      if Orientation = tpsoVertical then BR.Bottom := BR.Top + ButtonSize
      else BR.Right := BR.Left + ButtonSize;
      CurrentTheme.PaintPageScrollButton(DC, BR, CBtns[Orientation, False], FScrollDirection < 0);
    end;
    if tpsbNext in FVisibleButtons then
    begin
      BR := R;
      if Orientation = tpsoVertical then BR.Top := BR.Bottom - ButtonSize
      else BR.Left := BR.Right - ButtonSize;
      CurrentTheme.PaintPageScrollButton(DC, BR, CBtns[Orientation, True], FScrollDirection > 0);
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TTBXCustomPageScroller.EnableAutoRange;
begin
  if FAutoRangeCount > 0 then
  begin
    Dec(FAutoRangeCount);
    if FAutoRangeCount = 0 then CalcAutoRange;
  end;
end;

procedure TTBXCustomPageScroller.HandleScrollTimer;
var
  Pt: TPoint;
  R: TRect;
  OldPosition: Integer;
  OldDirection: Integer;
begin
  GetCursorPos(Pt);
  GetWindowRect(Handle, R);
  if not PtInRect(R, Pt) then
  begin
    StopScrolling;
  end
  else if FScrollDirection = 0 then
  begin
    FScrollTimer.Enabled := False;
    FScrollCounter := 0;
  end
  else
  begin
    OldPosition := Position;
    OldDirection := FScrollDirection;
    if ((FScrollDirection > 0) and (FScrollCounter < 0)) or
      ((FScrollDirection < 0) and (FScrollCounter > 0)) then FScrollCounter := 0;
    if FScrollDirection > 0 then Inc(FScrollCounter)
    else Dec(FScrollCounter);
    Position := Position + FScrollCounter;
    if Position = OldPosition then
    begin
      ReleaseCapture;
      FScrollTimer.Enabled := False;
      DrawNCArea(False, 0, 0);
    end
    else
    begin
      if FScrollPending or (FScrollDirection * OldDirection <= 0) or
        (FScrollDirection * OldDirection <= 0) then
        DrawNCArea(False, 0, 0);
    end;
  end;
  if FScrollPending then FScrollTimer.Interval := ScrollInterval;
  FScrollPending := False;
end;

function TTBXCustomPageScroller.IsRangeStored: Boolean;
begin
  Result := not AutoRange;
end;

procedure TTBXCustomPageScroller.Loaded;
begin
  inherited;
  UpdateButtons;
end;

procedure TTBXCustomPageScroller.RecalcNCArea;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TTBXCustomPageScroller.Resizing;
begin
  // do nothing by default
end;

procedure TTBXCustomPageScroller.ScrollTimerTimer(Sender: TObject);
begin
  HandleScrollTimer;
end;

procedure TTBXCustomPageScroller.ScrollToCenter(ARect: TRect);
var
  X, Y: Integer;
begin
  if Orientation = tpsoVertical then
  begin
    if ARect.Bottom - ARect.Top < Range then Y := (ARect.Top + ARect.Bottom) div 2
    else Y := ARect.Top;
    Position := Position + Y - Height div 2;
  end
  else
  begin
    if ARect.Right - ARect.Left < Range then X := (ARect.Left + ARect.Right) div 2
    else X := ARect.Left;
    Position := Position + X - Width div 2;
  end;
end;

procedure TTBXCustomPageScroller.ScrollToCenter(AControl: TControl);
var
  R: TRect;
begin
  R := AControl.ClientRect;
  R.TopLeft := ScreenToClient(AControl.ClientToScreen(R.TopLeft));
  R.BottomRight := ScreenToClient(AControl.ClientToScreen(R.BottomRight));
  ScrollToCenter(R);
end;

procedure TTBXCustomPageScroller.SetAutoRange(Value: Boolean);
begin
  if FAutoRange <> Value then
  begin
    FAutoRange := Value;
    if Value then CalcAutoRange else Range := 0;
  end;
end;

procedure TTBXCustomPageScroller.SetButtonSize(Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    UpdateButtons;
  end;
end;

procedure TTBXCustomPageScroller.SetOrientation(Value: TTBXPageScrollerOrientation);
begin
  if Orientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

{$IFNDEF JR_D7}
procedure TTBXCustomPageScroller.SetParentBackground(Value: Boolean);
begin
  if FParentBackground <> Value then
  begin
    FParentBackground := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TTBXCustomPageScroller.SetPosition(Value: Integer);
var
  OldPos: Integer;
begin
  if csReading in ComponentState then FPosition := Value
  else
  begin
    ValidatePosition(Value);
    if FPosition <> Value then
    begin
      OldPos := FPosition;
      FPosition := Value;

      if OldPos > 0 then Inc(OldPos, ButtonSize);
      if Value > 0 then Inc(Value, ButtonSize);

      if Orientation = tpsoHorizontal then ScrollBy(OldPos - Value, 0)
      else ScrollBy(0, OldPos - Value);
      UpdateButtons;
    end;
  end;
end;

procedure TTBXCustomPageScroller.SetRange(Value: Integer);
begin
  FAutoRange := False;
  DoSetRange(Value);
end;

procedure TTBXCustomPageScroller.StopScrolling;
begin
  if (FScrollDirection <> 0) or (FScrollCounter <> 0) or (FScrollTimer.Enabled) then
  begin
    FScrollDirection := 0;
    FScrollCounter := 0;
    FScrollTimer.Enabled := False;
    if HandleAllocated and IsWindowVisible(Handle) then DrawNCArea(False, 0, 0);
  end;
end;

procedure TTBXCustomPageScroller.UpdateButtons;
var
  Sz: Integer;
  OldVisibleButtons: TTBXPageScrollerButtons;
  RealignNeeded: Boolean;
begin
  RealignNeeded := False;
  if not FUpdatingButtons and HandleAllocated then
  try
    FUpdatingButtons := True;
    if Orientation = tpsoHorizontal then Sz := Width
    else Sz := Height;
    OldVisibleButtons := FVisibleButtons;
    FVisibleButtons := [];

    FPosRange := Range - Sz;
    if FPosRange < 0 then FPosRange := 0;
    if FPosition > FPosRange - 1 then
    begin
      FPosition := FPosRange;
      RealignNeeded := True;
    end;

    if Sz > ButtonSize * 3 then
    begin
      if Position > 0 then Include(FVisibleButtons, tpsbPrev);
      if Range - Position > Sz then Include(FVisibleButtons, tpsbNext);
    end;
    if FVisibleButtons <> OldVisibleButtons then
    begin
      RecalcNCArea;
      RealignNeeded := True;
    end;
  finally
    FUpdatingButtons := False;
    if RealignNeeded then Realign;
  end;
end;

procedure TTBXCustomPageScroller.ValidatePosition(var NewPos: Integer);
begin
  if NewPos < 0 then NewPos := 0;
  if NewPos > FPosRange then NewPos := FPosRange;
end;

procedure TTBXCustomPageScroller.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if ParentBackground then
  begin
    DrawParentBackground(Self, Message.DC, ClientRect);
    Message.Result := 1;
  end
  else inherited;
end;

procedure TTBXCustomPageScroller.WMMouseMove(var Message: TWMMouseMove);
begin
  if AutoScroll then StopScrolling;
  inherited;
end;

procedure TTBXCustomPageScroller.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with Message.CalcSize_Params^ do
  begin
    if Orientation = tpsoVertical then
    begin
      if tpsbPrev in FVisibleButtons then Inc(rgrc[0].Top, ButtonSize);
      if tpsbNext in FVisibleButtons then Dec(rgrc[0].Bottom, ButtonSize);
    end
    else
    begin
      if tpsbPrev in FVisibleButtons then Inc(rgrc[0].Left, ButtonSize);
      if tpsbNext in FVisibleButtons then Dec(rgrc[0].Right, ButtonSize);
    end;
    Message.Result := 0;
  end;
end;

procedure TTBXCustomPageScroller.WMNCHitTest(var Message: TWMNCHitTest);
var
  Pt: TPoint;
  R: TRect;
begin
  DefaultHandler(Message);
  with Message do if Result <> HTCLIENT then
  begin
    Pt := SmallPointToPoint(Pos);
    GetWindowRect(Handle, R);
    if PtInRect(R, Pt) then
    begin
      if (tpsbPrev in FVisibleButtons) then
      begin
        if Orientation = tpsoVertical then
        begin
          if Pt.Y < R.Top + ButtonSize then Result := HTSCROLLPREV
        end
        else
        begin
          if Pt.X < R.Left + ButtonSize then Result := HTSCROLLPREV
        end;
      end;
      if (tpsbNext in FVisibleButtons) then
      begin
        if Orientation = tpsoVertical then
        begin
          if Pt.Y >= R.Bottom - ButtonSize then Result := HTSCROLLNEXT;
        end
        else
        begin
          if Pt.X >= R.Right - ButtonSize then Result := HTSCROLLNEXT;
        end;
      end;
    end;
  end;
end;

procedure TTBXCustomPageScroller.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});

  if not AutoScroll and (Message.HitTest in [HTSCROLLPREV, HTSCROLLNEXT]) then
    BeginScrolling(Message.HitTest)
  else
    inherited;
end;

procedure TTBXCustomPageScroller.WMNCMouseLeave(var Message: TMessage);
begin
  if AutoScroll then StopScrolling;
  inherited;
end;

procedure TTBXCustomPageScroller.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  OldScrollDirection: Integer;
begin
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});

  if AutoScroll then
  begin
    OldScrollDirection := FScrollDirection;
    case Message.HitTest of
      HTSCROLLPREV: FScrollDirection := -1;
      HTSCROLLNEXT: FScrollDirection := 1;
    else
      StopScrolling;
      inherited;
      Exit;
    end;
    if OldScrollDirection <> FScrollDirection then
    begin
      FScrollCounter := 0;
      FScrollPending := True;
      FScrollTimer.Interval := ScrollDelay;
      FScrollTimer.Enabled := True;
      DrawNCArea(False, 0, 0);
    end;
  end;
end;

procedure TTBXCustomPageScroller.WMNCPaint(var Message: TMessage);
begin
  DrawNCArea(False, 0, HRGN(Message.WParam));
end;

procedure TTBXCustomPageScroller.WMSize(var Message: TWMSize);
begin
  FUpdatingButtons := True;
  try
    CalcAutoRange;
  finally
    FUpdatingButtons := False;
  end;
  Inc(FAutoRangeCount);
  inherited;
  Resizing;
  Dec(FAutoRangeCount);
end;

{ TTBXCustomCheckBox }

procedure TTBXCustomCheckBox.Click;
begin
  Toggle;
  Invalidate;
  inherited;
end;

procedure TTBXCustomCheckBox.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Toggle;
end;

constructor TTBXCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  SmartFocus := True;
  SpaceAsClick := True;
  TabStop := True;
end;

procedure TTBXCustomCheckBox.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
begin
  inherited DoAdjustHeight(ACanvas, NewHeight);
  if NewHeight < GetGlyphSize + 4 then NewHeight := GetGlyphSize + 4;
end;

procedure TTBXCustomCheckBox.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTBXCustomCheckBox.DoMouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TTBXCustomCheckBox.DoMouseLeave;
begin
  inherited;
  Invalidate;
end;

function TTBXCustomCheckBox.DoSetState(var NewState: TCheckBoxState): Boolean;
begin
  Result := True;
end;

function TTBXCustomCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

function TTBXCustomCheckBox.GetFocusRect(const R: TRect): TRect;
const
  Alignments: array [TLeftRight] of Word = (DT_LEFT, DT_RIGHT);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE or DT_VCENTER,
    DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS, DT_WORDBREAK);
var
  TR: TRect;
begin
  TR := R;
  ApplyMargins(TR, GetTextMargins);
  DoDrawText(Canvas, TR, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[Wrapping] or Alignments[Alignment]);
  Result := R;
  Result.Right := TR.Right + 2;
  Result.Left := TR.Left - 2;
end;

function TTBXCustomCheckBox.GetGlyphSize: Integer;
begin
  Result := 13;
end;

function TTBXCustomCheckBox.GetTextAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TTBXCustomCheckBox.GetTextMargins: TRect;
begin
  Result := Rect(2, 2, 2, 2);
  with Result do
    if GetRealAlignment(Self) = taLeftJustify then Inc(Left, GetGlyphSize + 6)
    else Inc(Right, GetGlyphSize + 6);
end;

procedure TTBXCustomCheckBox.Paint;
const
  EnabledState: array [Boolean] of Integer = (PFS_DISABLED, 0);
  StateFlags: array [TCheckBoxState] of Integer = (0, PFS_CHECKED, PFS_MIXED);
  HotState: array [Boolean] of Integer = (0, PFS_HOT);
  PushedState: array [Boolean] of Integer = (0, PFS_PUSHED);
  FocusedState: array [Boolean] of Integer = (0, PFS_FOCUSED);
var
  Rect: TRect;
  Sz, Flags: Integer;
begin
  inherited;
  with Canvas do
  begin
    Rect := ClientRect;
    ApplyMargins(Rect, Margins);
    Sz := GetGlyphSize;
    if Alignment = taLeftJustify then Rect.Right := Rect.Left + GetGlyphSize
    else Rect.Left := Rect.Right - GetGlyphSize;
    Rect.Top := (Rect.Top + Rect.Bottom + 1 - Sz) div 2;
    Rect.Bottom := Rect.Top + Sz;
    Brush.Color := clBtnShadow;
    Flags := EnabledState[Enabled];
    if Enabled then Flags :=
      Flags or StateFlags[State] or
      HotState[MouseInControl or FInSpaceClick] or
      PushedState[Pushed and (MouseInControl or InSpaceClick)] or
      FocusedState[Focused];
    CurrentTheme.PaintFrameControl(Canvas.Handle, Rect, PFC_CHECKBOX, Flags, nil);
  end;
end;

procedure TTBXCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TTBXCustomCheckBox.SetState(Value: TCheckBoxState);
begin
  if (FState <> Value) and DoSetState(Value) then
  begin
    FState := Value;
    Invalidate;
    DoChange;
  end;
end;

procedure TTBXCustomCheckBox.Toggle;
begin
  case State of
    cbUnchecked: if AllowGrayed then State := cbGrayed else State := cbChecked;
    cbChecked: State := cbUnchecked;
    cbGrayed: State := cbChecked;
  end;
end;

procedure TTBXCustomCheckBox.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
  SL, SR: Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    P := ScreenToClient(SmallPointToPoint(Message.Pos));
    R := ClientRect;
    ApplyMargins(R, Margins);
    SL := R.Left; SR := R.Right;
    R := GetFocusRect(R);
    if GetRealAlignment(Self) = taLeftJustify then R.Left := SL
    else R.Right := SR;
    if not PtInRect(R, P) then Message.Result := HTTRANSPARENT;
  end;
end;

{ TTBXCustomRadioButton }

procedure TTBXCustomRadioButton.Click;
begin
  if not Checked then Checked := True;
  Invalidate;
  inherited;
end;

procedure TTBXCustomRadioButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Checked := not Checked;
end;

constructor TTBXCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  SmartFocus := True;
  SpaceAsClick := True;
  TabStop := True;
end;

procedure TTBXCustomRadioButton.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
begin
  inherited DoAdjustHeight(ACanvas, NewHeight);
  if NewHeight < GetGlyphSize + 4 then NewHeight := GetGlyphSize + 4;
end;

procedure TTBXCustomRadioButton.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTBXCustomRadioButton.DoMouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TTBXCustomRadioButton.DoMouseLeave;
begin
  inherited;
  Invalidate;
end;

function TTBXCustomRadioButton.DoSetChecked(var Value: Boolean): Boolean;
begin
  Result := True;
end;

function TTBXCustomRadioButton.GetFocusRect(const R: TRect): TRect;
const
  Alignments: array [TLeftRight] of Word = (DT_LEFT, DT_RIGHT);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE or DT_VCENTER,
    DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_VCENTER or DT_PATH_ELLIPSIS, DT_WORDBREAK);
var
  TR: TRect;
begin
  TR := R;
  ApplyMargins(TR, GetTextMargins);
  DoDrawText(Canvas, TR, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[Wrapping] or Alignments[Alignment]);
  Result := R;
  Result.Right := TR.Right + 2;
  Result.Left := TR.Left - 2;
end;

function TTBXCustomRadioButton.GetGlyphSize: Integer;
begin
  Result := 13;
end;

function TTBXCustomRadioButton.GetTextAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TTBXCustomRadioButton.GetTextMargins: TRect;
begin
  Result := Rect(2, 2, 2, 2);
  with Result do
    if GetRealAlignment(Self) = taLeftJustify then Inc(Left, GetGlyphSize + 6)
    else Inc(Right, GetGlyphSize + 6);
end;

procedure TTBXCustomRadioButton.Paint;
const
  EnabledState: array [Boolean] of Integer = (PFS_DISABLED, 0);
  CheckedState: array [Boolean] of Integer = (0, PFS_CHECKED);
  HotState: array [Boolean] of Integer = (0, PFS_HOT);
  PushedState: array [Boolean] of Integer = (0, PFS_PUSHED);
  FocusedState: array [Boolean] of Integer = (0, PFS_FOCUSED);
var
  Rect: TRect;
  Sz, Flags: Integer;
begin
  inherited;
  with Canvas do
  begin
    Rect := ClientRect;
    with Margins do
    begin
      Inc(Rect.Left, Left);
      Inc(Rect.Top, Top);
      Dec(Rect.Right, Right);
      Dec(Rect.Bottom, Bottom);
    end;
    Sz := GetGlyphSize;
    if Alignment = taLeftJustify then Rect.Right := Rect.Left + GetGlyphSize
    else Rect.Left := Rect.Right - GetGlyphSize;
    Rect.Top := (Rect.Top + Rect.Bottom + 1 - Sz) div 2;
    Rect.Bottom := Rect.Top + Sz;
    Brush.Color := clBtnShadow;
    Flags := EnabledState[Enabled];
    if Enabled then Flags := Flags or CheckedState[Checked] or
      HotState[MouseInControl or InSpaceClick] or PushedState[Pushed and (MouseInControl or InSpaceClick)] or FocusedState[Focused];
    CurrentTheme.PaintFrameControl(Canvas.Handle, Rect, PFC_RADIOBUTTON, Flags, nil);
  end;
end;

procedure TTBXCustomRadioButton.SetChecked(Value: Boolean);
begin
  if (Value <> FChecked) and DoSetChecked(Value) then
  begin
    FChecked := Value;
    TabStop := Value;
    if Value then TurnSiblingsOff;
    Invalidate;
    DoChange;
  end;
end;

procedure TTBXCustomRadioButton.SetGroupIndex(Value: Integer);
begin
  FGroupIndex := Value;
  if Checked then TurnSiblingsOff;
end;

procedure TTBXCustomRadioButton.TurnSiblingsOff;
var
  I: Integer;
  Sibling: TControl;
begin
  if Parent <> nil then
    with Parent do
      for I := 0 to ControlCount - 1 do
      begin
        Sibling := Controls[I];
        if (Sibling <> Self) and (Sibling is TTBXCustomRadioButton) then
          with TTBXCustomRadioButton(Sibling) do
          begin
            if GroupIndex = Self.GroupIndex then SetChecked(False);
          end;
      end;
end;

procedure TTBXCustomRadioButton.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
  SL, SR: Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    P := ScreenToClient(SmallPointToPoint(Message.Pos));
    R := ClientRect;
    ApplyMargins(R, Margins);
    SL := R.Left; SR := R.Right;
    R := GetFocusRect(R);
    if GetRealAlignment(Self) = taLeftJustify then R.Left := SL
    else R.Right := SR;
    if not PtInRect(R, P) then Message.Result := HTTRANSPARENT;
  end;
end;

end.
