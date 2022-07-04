unit SpTBXControls;

{==============================================================================
Version 2.4

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
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - All the compatibility changes are marked with '[Backward-Compatibility]'.

History:
17 January 2009 - version 2.4
  - No changes.

28 September 2008 - version 2.3.1
  - Fixed incorrect TSpTBXGroupBox painting, the control was not
    repainted when the font was changed, thanks to Yury Plashenkov
    for reporting this.

26 September 2008 - version 2.3
  - Removed LinkFont property from TSpTBXTextObject, having 2 font
    properties to control the text state was a bad idea.

29 July 2008 - version 2.2
  - Fixed incorrect ProgressBar painting on Windows Vista,
    thanks to Arvid for reporting this.

22 June 2008 - version 2.1
  - No changes.

3 May 2008 - version 2.0
  - No changes.

2 April 2008 - version 1.9.5
  - Improved the background painting of TSpTBXPanel.

3 February 2008 - version 1.9.4
  - No changes.

19 January 2008 - version 1.9.3
  - Fixed incorrect Autosizing of TSpTBXTextControl, thanks
    to Alexey Naumov for reporting this.

26 December 2007 - version 1.9.2
  - Added State parameter to TSpTBXTextControl.OnDrawCaption
  - Fixed incorrect Default property handling of TSpTBXButton,
    thanks to Karpushin Matvey and Beta Xiong for reporting this.

1 December 2007 - version 1.9.1
  - Added various painting enhancements made by Jim.
  - Fixed incorrect caption color on the controls when
    the Font is changed, thanks to Arvid and Zunyite for
    reporting this.
  - Fixed incorrect nested panel painting (canvas was not locked),
    thanks to Jim for reporting this.

20 November 2007 - version 1.9
  - Removed TBX dependency.

8 February 2007 - version 1.8.3
  - Added GripHotTrack property to TSpTBXSplitter.

17 December 2006 - version 1.8.2
  - Added AutoSize property to TSpTBXPanel.
  - Fixed incorrect resizing behavior on TSpTBXSplitter when a
    DockablePanel was adjacent.

24 November 2006 - version 1.8.1
  - Improved TSpTBXPanel painting, thanks to Jim Kueneman for
    his code donation.
  - Fixed incorrect focus behavior on TSpTBXRadioButton when used
    on a groupbox, thanks to Andrew for reporting this.

27 August 2006 - version 1.8
  - Added DropDownArrow property to TSpTBXButton and TSpTBXSpeedButton.
  - Fixed incorrect TSpTBXGroupBox painting when changing the
    Enabled property, thanks to Tomaz Kunaver for reporting this.

15 June 2006 - version 1.7
  - Fixed incorrect TSpTBXButton painting when using a bitmap
    skin and the DropDownMenu is shown, thanks to Boris Yankov
    for reporting this.

4 May 2006 - version 1.6
  - New component added, TSpTBXRadioGroup.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - Added GroupIndex property to TSpTBXButton and TSpTBXSpeedButton.

10 February 2006 - version 1.3
  - New component added, TSpTBXSpeedButton.
  - New component added, TSpTBXSplitter.
  - Fixed incorrect TSpTBXButton behavior when trying to close the
    DropDownMenu clicking the button, thanks to Alexey Naumov for
    reporting this.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Forms, Controls, Graphics, ImgList,
  Menus, StdCtrls, ExtCtrls, ComCtrls, ActnList,
  {$IFNDEF UNICODE}
  TntClasses, TntControls,
  {$ENDIF}
  TB2Dock, TB2Toolbar, TB2Item, SpTBXItem, SpTBXSkins;

{$IFDEF UNICODE}
type
  TTntStrings = TStrings;
{$ENDIF}

const
  ConstStatesCount = 4;        // Buttons have 4 states (normal, hottrack, pushed, disabled)
  ConstInitRepeatPause = 400;  // Delay of the first repeated click (ms)
  ConstRepeatPause     = 100;  // Interval of the repeated clicks (ms)
  CM_SPGROUPINDEXUPDATE = CM_BASE + 2222;  // Message sent to the controls to update its state based on the GroupIndex
  CM_SPTBXCONTROLSINVALIDATE = CM_BASE + 3333;  // Message sent to SpTBX controls to invalidate the background

type
  TSpTBXTextObject = class;

  TSpTBXPanelBorder = (
    pbrRaised,
    pbrDoubleRaised,
    pbrSunken,
    pbrDoubleSunken,
    pbrBumped,
    pbrEtched,
    pbrFramed
  );

  TSpTBXProgressCaption = (
    pctNone,
    pctDefault,
    pctPercentage,
    pctProgress
  );

  TSpTBXTickMark = (
    tmxBottomRight,
    tmxTopLeft,
    tmxBoth,
    tmxCenter
  );

  TSpTBXCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer; var Accept: Boolean) of object;

  { TSpTBXPanel }

  TSpTBXCustomPanel = class(TSpTBXCustomControl)
  private
    FBorders: Boolean;
    FBorderType: TSpTBXPanelBorder;
    FTBXStyleBackground: Boolean;
    FSkinType: TSpTBXSkinType;
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure SetBorders(const Value: Boolean);
    procedure SetBorderType(const Value: TSpTBXPanelBorder);
    procedure SetTBXStyleBackground(const Value: Boolean);
    procedure SetSkinType(const Value: TSpTBXSkinType);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSpTBXControlsInvalidate(var Message: TMessage); message CM_SPTBXCONTROLSINVALIDATE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FBackground: TBitmap;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    property Borders: Boolean read FBorders write SetBorders default True;
    property BorderType: TSpTBXPanelBorder read FBorderType write SetBorderType default pbrEtched;
    property ParentColor default False;
    property TBXStyleBackground: Boolean read FTBXStyleBackground write SetTBXStyleBackground default False;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); virtual;
  published
    property Caption;
    property Hint;
    property Color default clNone;
    property SkinType: TSpTBXSkinType read FSkinType write SetSkinType default sknSkin;
  end;

  TSpTBXPanel = class(TSpTBXCustomPanel)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FChildFocused: Boolean;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetHotTracking(const Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); override;
  public
    property HotTracking: Boolean read FHotTracking;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
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
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property TBXStyleBackground;
    property OnDrawBackground;
  end;

  { TSpTBXGroupBox }

  TSpTBXCustomGroupBox = class(TSpTBXCustomPanel)
  private
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXGroupBox = class(TSpTBXCustomGroupBox)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
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
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property TBXStyleBackground;
    property OnDrawBackground;
  end;

  { TSpTBXTextObjectActionLink }

  {$IFNDEF UNICODE}
  TSpTBXTextObjectActionLink = class(TControlActionLink)
  protected
    FUnicodeClient: TSpTBXTextObject;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetCaption(const Value: String); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: Integer); override;
  end;
  {$ELSE}
  TSpTBXTextObjectActionLink = class(TControlActionLink);
  {$ENDIF}

  { TSpTBXTextObject }

  TSpTBXTextObject = class(TSpTBXCustomControl)
  private
    FAlignment: TAlignment;
    FCaptionGlow: TSpGlowDirection;
    FCaptionGlowColor: TColor;
    FCaptionRoatationAngle: TSpTextRotationAngle;
    FChecked: Boolean;
    FDisabledIconCorrection: Boolean;
    FDrawPushedCaption: Boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FLinkText: WideString;
    FLinkTextParams: WideString;
    FMouseInControl: Boolean;    
    FPushed: Boolean;
    FSkinType: TSpTBXSkinType;
    FSpaceAsClick: Boolean;
    FShowAccelChar: Boolean;
    FUpdating: Boolean;
    FWrapping: TTextWrapping;
    FOnDraw: TSpTBXDrawEvent;
    FOnDrawCaption: TSpTBXDrawTextEvent;
    FOnDrawHint: TSpTBXDrawHintEvent;
    FOnGetImageIndex: TSpTBXGetImageIndexEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure ReadLinkFont(Reader: TReader);   // [Backward-Compatibility]
    procedure ImageListChange(Sender: TObject);
    procedure UpdateTracking(ForceMouseLeave: Boolean = False);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaptionGlow(const Value: TSpGlowDirection);
    procedure SetCaptionGlowColor(const Value: TColor);
    procedure SetCaptionRoatationAngle(const Value: TSpTextRotationAngle);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetSkinType(const Value: TSpTBXSkinType);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetWrapping(Value: TTextWrapping);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    // Painting
    procedure AdjustFont(AFont: TFont); virtual;
    procedure AdjustBounds;
    procedure DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean); virtual;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    function DoDrawText(ACanvas: TCanvas; var ARect: TRect; Flags: Longint): Integer; virtual;
    procedure DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer); virtual;
    procedure DoInternalGlyphDraw(ACanvas: TCanvas; AGlyphRect: TRect); virtual;
    function GetFocusRect(R, TextR, GlyphR: TRect): TRect; virtual;
    function GetTextMargins: TRect; virtual;
    function IsImageIndexValid: Boolean;
    procedure Paint; override;

    // Sizing
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoAdjustBounds(var NewWidth, NewHeight: Integer); virtual;

    // Mouse
    function GetFocused: Boolean; virtual;
    function GetPushed: Boolean; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // Component
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ExecuteLink; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize default True;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CaptionGlow: TSpGlowDirection read FCaptionGlow write SetCaptionGlow default gldNone;
    property CaptionGlowColor: TColor read FCaptionGlowColor write SetCaptionGlowColor default clYellow;
    property CaptionRoatationAngle: TSpTextRotationAngle read FCaptionRoatationAngle write SetCaptionRoatationAngle default tra0;
    property DrawPushedCaption: Boolean read FDrawPushedCaption write FDrawPushedCaption default False;
    property DisabledIconCorrection: Boolean read FDisabledIconCorrection write FDisabledIconCorrection default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property LinkText: WideString read FLinkText write FLinkText;
    property LinkTextParams: WideString read FLinkTextParams write FLinkTextParams;
    property SkinType: TSpTBXSkinType read FSkinType write SetSkinType default sknSkin;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property SpaceAsClick: Boolean read FSpaceAsClick write FSpaceAsClick default False;
    property Wrapping: TTextWrapping read FWrapping write SetWrapping default twNone;
    property OnDraw: TSpTBXDrawEvent read FOnDraw write FOnDraw;
    property OnDrawCaption: TSpTBXDrawTextEvent read FOnDrawCaption write FOnDrawCaption;
    property OnDrawHint: TSpTBXDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnGetImageIndex: TSpTBXGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property TabStop default True;
    property ParentColor default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure Click; override;
    function GetControlsAlignment: TAlignment; override;
    procedure GetSize(out TotalR, TextR, GlyphR: TRect); virtual;
    function GetTextFlags: Cardinal;
    function GetGlyphSize: TSize; virtual;
    property MouseInControl: Boolean read FMouseInControl;
    property Pushed: Boolean read GetPushed;
  published
    property Caption;
    property Hint;
    property Color default clNone;
  end;

  { TSpTBXLabel }

  TSpTBXCustomLabel = class(TSpTBXTextObject)
  private
    FFocusControl: TWinControl;
    FUnderline: Boolean;
    FUnderlineColor: TColor;
    procedure SetFocusControl(const Value: TWinControl);
    procedure SetUnderline(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TColor);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure AdjustFont(AFont: TFont); override;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Underline: Boolean read FUnderline write SetUnderline default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBtnShadow;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetSize(out TotalR: TRect; out TextR: TRect; out GlyphR: TRect); override;
  end;

  TSpTBXLabel = class(TSpTBXCustomLabel)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
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
    // TSpTBXCustomLabel properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property FocusControl;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    property SkinType;
    property Underline;
    property UnderlineColor;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXButtonControl }

  TSpTBXButtonControl = class(TSpTBXTextObject)
  private
    FGroupIndex: Integer;
    FStateChanged: Boolean;
    procedure UpdateExclusive;
    procedure SetGroupIndex(const Value: Integer);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMSPGroupIndexUpdate(var Message: TMessage); message CM_SPGROUPINDEXUPDATE;
  protected
    function CanUpdateExclusive: Boolean; virtual;
    procedure SetChecked(Value: Boolean); override;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property StateChanged: Boolean read FStateChanged write FStateChanged;
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
  end;

  { TSpTBXCheckBox }

  TSpTBXCustomCheckButton = class(TSpTBXButtonControl)
  protected
    procedure Toggle; virtual;
  public
    function GetGlyphSize: TSize; override;
    procedure GetSize(out TotalR, TextR, GlyphR: TRect); override;
  end;

  TSpTBXCustomCheckBox = class(TSpTBXCustomCheckButton)
  private
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    procedure SetState(const Value: TCheckBoxState);
  protected
    procedure AdjustFont(AFont: TFont); override;
    procedure DoInternalGlyphDraw(ACanvas: TCanvas; AGlyphRect: TRect); override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; override;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TSpTBXCheckBox = class(TSpTBXCustomCheckBox)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
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
    // TSpTBXCustomCheckBox properties
    property Alignment;
    property AllowGrayed;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property State;
    property SkinType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXRadioButton }

  TSpTBXCustomRadioButton = class(TSpTBXCustomCheckButton)
  private
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure AdjustFont(AFont: TFont); override;
    function CanUpdateExclusive: Boolean; override;    
    procedure DoInternalGlyphDraw(ACanvas: TCanvas; AGlyphRect: TRect); override;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; override;
    property TabStop default False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TSpTBXRadioButton = class(TSpTBXCustomRadioButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
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
    // TSpTBXCustomRadioButton properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property GroupIndex;
    property SkinType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXRadioGroup }

  TSpTBXCustomRadioGroup = class(TSpTBXCustomGroupBox)
  private
    FButtons: TList;
    FItems: TTntStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    function GetButtons(Index: Integer): TSpTBXRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TTntStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TTntStrings read FItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); override;
    procedure SetFocus; override;
    property Buttons[Index: Integer]: TSpTBXRadioButton read GetButtons;
  end;

  TSpTBXRadioGroup = class(TSpTBXCustomRadioGroup)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
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
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property TBXStyleBackground;
    property OnDrawBackground;
    // TSpTBXCustomRadioGroup properties
    property Columns;
    property ItemIndex;
    property Items;
  end;

  { TSpTBXButton }

  TSpTBXCustomButton = class(TSpTBXButtonControl)
  private
    FBitmap: TBitmap;
    FBitmapTransparent: Boolean;
    FActive: Boolean;
    FCancel: Boolean;
    FDefault: Boolean;
    FDropDownArrow: Boolean;
    FDropDownMenu: TPopupMenu;
    FDropDownMenuVisible: Boolean;
    FModalResult: TModalResult;
    FRepeating: Boolean;
    FRepeatTimer: TTimer;
    procedure BitmapChanged(Sender: TObject);
    procedure RepeatTimerHandler(Sender: TObject);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetDefault(const Value: Boolean);
    procedure SetDropDownArrow(const Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMSPPopupClose(var Message: TMessage); message CM_SPPOPUPCLOSE;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
  protected
    FPopupControl: TControl;
    procedure CreateWnd; override;
    procedure AdjustFont(AFont: TFont); override;
    function BitmapValid: boolean;
    function DoDrawDropDownArrow(ACanvas: TCanvas; ARect: TRect): Boolean; virtual;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    function GetFocused: Boolean; override;
    function GetFocusRect(R, TextR, GlyphR: TRect): TRect; override;
    function GetInternalDropDownMenu: TPopupMenu; virtual;
    function GetPushed: Boolean; override;
    function GetTextMargins: TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Alignment default taCenter;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapTransparent: Boolean read FBitmapTransparent write FBitmapTransparent default True;
    property DrawPushedCaption default True;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property DropDownArrow: Boolean read FDropDownArrow write SetDropDownArrow default True;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Repeating: Boolean read FRepeating write FRepeating default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function GetSkinStateRect: TRect;
    function IsDroppedDown: Boolean;
    procedure StopRepeat; virtual;
  end;

  TSpTBXButton = class(TSpTBXCustomButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    // property OnDblClick; Buttons don't have OnDblClick events
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
    // TSpTBXCustomButton properties
    property Alignment;
    property Bitmap;
    property BitmapTransparent;
    property Cancel;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property Default;
    property DrawPushedCaption;
    property DropDownArrow;
    property DropDownMenu;
    property GroupIndex;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    property ModalResult;
    property SkinType;
    property Repeating;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXSpeedButton }

  TSpTBXCustomSpeedButton = class(TSpTBXCustomButton)
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
    procedure Click; override;
  end;

  TSpTBXSpeedButton = class(TSpTBXCustomSpeedButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    // property TabOrder; SpeedButtons don't have TabStops
    // property TabStop; SpeedButtons don't have TabStops
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    // property OnDblClick; SpeedButtons don't have OnDblClick events
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
    // TSpTBXCustomButton properties
    property Alignment;
    property Bitmap;
    property BitmapTransparent;
    property Cancel;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property Default;
    property DrawPushedCaption;
    property DropDownArrow;
    property DropDownMenu;
    property GroupIndex;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    // property ModalResult;
    property SkinType;
    property Repeating;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXProgressBar }

  TSpTBXProgressBarChangeEvent = procedure(Sender: TObject; NewPosition: Integer) of object;

  TSpTBXCustomProgressBar = class(TSpTBXTextObject)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FProgressVisible: Boolean;
    FSmooth: Boolean;
    FVertical: Boolean;
    FCaptionType: TSpTBXProgressCaption;
    FOnProgressChange: TSpTBXProgressBarChangeEvent;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(Value: integer);
    procedure SetSmooth(const Value: Boolean);
    procedure SetVertical(const Value: Boolean);
    procedure SetCaptionType(const Value: TSpTBXProgressCaption);
    procedure SetProgressVisible(const Value: Boolean);
  protected
    procedure AdjustFont(AFont: TFont); override;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure DoProgressChange; virtual;
    function GetTextMargins: TRect; override;
    property Alignment default taCenter;
    property CaptionGlow default gldAll;
    property CaptionType: TSpTBXProgressCaption read FCaptionType write SetCaptionType default pctPercentage;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property ProgressVisible: Boolean read FProgressVisible write SetProgressVisible default True;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Vertical: Boolean read FVertical write SetVertical default False;
    property OnProgressChange: TSpTBXProgressBarChangeEvent read FOnProgressChange write FOnProgressChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt(Delta: Integer = 1);
  end;

  TSpTBXProgressBar = class(TSpTBXCustomProgressBar)
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    // TSpTBXCustomProgressBar properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property CaptionType;
    property Max;
    property Min;
    property Position;
    property Smooth;
    property Vertical;
    property SkinType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnProgressChange;
  end;

  { TSpTBXTrackBar }

  TSpTBXTrackBar = class(TTrackBar)
  private
    FSkinType: TSpTBXSkinType;
    FTickMarks: TSpTBXTickMark;
    FOnDrawChannel: TSpTBXDrawEvent;
    FOnDrawChannelTicks: TSpTBXDrawPosEvent;
    FOnDrawThumb: TSpTBXDrawEvent;
    FCanDrawChannelSelection: Boolean;
    procedure SetSkinType(const Value: TSpTBXSkinType);
    procedure SetTickMarks(const Value: TSpTBXTickMark);
    procedure CMSpTBXControlsInvalidate(var Message: TMessage); message CM_SPTBXCONTROLSINVALIDATE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function DoDrawChannel(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    function DoDrawChannelTicks(ACanvas: TCanvas; X, Y: Integer): Boolean; virtual;
    function DoDrawThumb(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    procedure DrawTicks(ACanvas: TCanvas); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ChannelRect: TRect;
    function MouseInThumb: Boolean;
    procedure InvalidateBackground;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property SkinType: TSpTBXSkinType read FSkinType write SetSkinType default sknSkin;
    property TickMarks: TSpTBXTickMark read FTickMarks write SetTickMarks default tmxBottomRight;
    property OnDrawChannel: TSpTBXDrawEvent read FOnDrawChannel write FOnDrawChannel;
    property OnDrawChannelTicks: TSpTBXDrawPosEvent read FOnDrawChannelTicks write FOnDrawChannelTicks;
    property OnDrawThumb: TSpTBXDrawEvent read FOnDrawThumb write FOnDrawThumb;
  end;

{ Painting helpers }
procedure SpDrawXPPanel(ACanvas: TCanvas; ARect: TRect; Enabled, TBXStyleBackground: Boolean; SkinType: TSpTBXSkinType; Border: TSpTBXPanelBorder);
procedure SpDrawXPPanelBorder(ACanvas: TCanvas; ARect: TRect; Border: TSpTBXPanelBorder);
procedure SpDrawXPGroupBox(ACanvas: TCanvas; ARect: TRect; ACaption: WideString; TextFlags: Cardinal; Enabled, TBXStyleBackground: Boolean; SkinType: TSpTBXSkinType);
procedure SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect; Min, Max, Position: Integer; Back, Fore: TBitmap); overload;
function SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect; Vertical, Smooth, DrawProgress: Boolean; Min, Max, Position: Integer; SkinType: TSpTBXSkinType): Integer; overload;
procedure SpDrawXPTrackBar(ACanvas: TCanvas; ARect: TRect; Part: Cardinal; Vertical, Pushed, ChannelSelection: Boolean; TickMark: TSpTBXTickMark; Min, Max, SelStart, SelEnd: Integer; SkinType: TSpTBXSkinType);
procedure SpInvalidateSpTBXControl(AControl: TWinControl; InvalidateChildren, OnlySpTBXControls: Boolean);

implementation

uses
  Themes, UxTheme,
  {$IFNDEF UNICODE} TntActnList, {$ENDIF}
  CommCtrl, ShellAPI;
type
  TWinControlAccess = class(TWinControl);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpDrawXPPanel(ACanvas: TCanvas; ARect: TRect; Enabled, TBXStyleBackground: Boolean;
  SkinType: TSpTBXSkinType; Border: TSpTBXPanelBorder);
var
  Flags: Integer;
begin
  case SpTBXSkinType(SkinType) of
    sknWindows:
      begin
        if Enabled then Flags := GBS_NORMAL
        else Flags := GBS_DISABLED;
        DrawThemeBackground(ThemeServices.Theme[teButton], ACanvas.Handle, BP_GROUPBOX, Flags, ARect, nil);
      end;
    sknNone:
      SpDrawXPPanelBorder(ACanvas, ARect, Border);
    sknSkin:
      CurrentSkin.PaintBackground(ACanvas, ARect, skncPanel, sknsNormal, TBXStyleBackground, True);
  end;
end;

procedure SpDrawXPPanelBorder(ACanvas: TCanvas; ARect: TRect; Border: TSpTBXPanelBorder);
const
  Edge: array [TSpTBXPanelBorder] of Cardinal = (BDR_RAISEDINNER, EDGE_RAISED,
    BDR_SUNKENOUTER, EDGE_SUNKEN, EDGE_BUMP, EDGE_ETCHED, 0);
begin
  if Border = pbrFramed then begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FrameRect(ARect);
  end
  else
    DrawEdge(ACanvas.Handle, ARect, Edge[Border], BF_RECT);
end;

procedure SpDrawXPGroupBox(ACanvas: TCanvas; ARect: TRect; ACaption: WideString;
  TextFlags: Cardinal; Enabled, TBXStyleBackground: Boolean; SkinType: TSpTBXSkinType);
var
  Width, Flags, SaveIndex: Integer;
  R: TRect;
  CaptionRect: TRect;
  XPThemes: Boolean;
begin
  Flags := 0;
  XPThemes := SpTBXSkinType(SkinType) = sknWindows;
  Width := ARect.Right - ARect.Left;

  if ACaption <> '' then begin
    CaptionRect := Rect(0, 0, 1, 1);

    if Enabled then Flags := GBS_NORMAL
    else Flags := GBS_DISABLED;
    if XPThemes then
      GetThemeTextExtent(ThemeServices.Theme[teButton], ACanvas.Handle, BP_GROUPBOX, Flags,
        PWideChar(ACaption), Length(ACaption), DT_LEFT, nil, CaptionRect)
    else
      SpDrawXPText(ACanvas, ACaption, CaptionRect, TextFlags or DT_CALCRECT);

    if (TextFlags and DT_RTLREADING) = 0 then
      OffsetRect(CaptionRect, 8, 0)
    else
      OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
  end
  else
    CaptionRect := Rect(0, 0, 0, 0);

  R := ARect;
  R.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
  SaveIndex := SaveDC(ACanvas.Handle);
  with CaptionRect do
    ExcludeClipRect(ACanvas.Handle, Left, Top, Right, Bottom);
  try
    SpDrawXPPanel(ACanvas, R, Enabled, TBXStyleBackground, SkinType, pbrEtched);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  if ACaption <> '' then
    if XPThemes then
      DrawThemeText(ThemeServices.Theme[teButton], ACanvas.Handle, BP_GROUPBOX, Flags, PWideChar(ACaption), -1, TextFlags, 0, CaptionRect)
    else begin
      if CurrentSkin.Options(skncPanel, sknsNormal).TextColor <> clNone then
        ACanvas.Font.Color := CurrentSkin.Options(skncPanel, sknsNormal).TextColor;
      SpDrawXPText(ACanvas, ACaption, CaptionRect, TextFlags);
    end;
end;

procedure SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect;
  Min, Max, Position: Integer; Back, Fore: TBitmap);
var
  Percent, Delta: Integer;
  DeltaR, R: TRect;
begin
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;

  // Get the delta
  if (Max > Min) and (Position > Min) then begin
    Percent := (Position * 100) div (Max - Min);
    DeltaR := ARect;
    R := Rect(0, 0, Back.Width, Back.Height);
    if Back.Height > Back.Width then begin
      Delta := (Back.Height * Percent) div 100;
      DeltaR.Bottom := DeltaR.Top + Delta;
      R.Bottom := R.Top + Delta;
    end
    else begin
      Delta := (Back.Width * Percent) div 100;
      DeltaR.Right := DeltaR.Left + Delta;
      R.Right := R.Left + Delta;
    end;
  end
  else
    Delta := 0;

  ACanvas.Draw(ARect.Left, ARect.Top, Back);
  if Delta > 0 then
    ACanvas.CopyRect(DeltaR, Fore.Canvas, R);
end;

function SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect;
  Vertical, Smooth, DrawProgress: Boolean; Min, Max, Position: Integer;
  SkinType: TSpTBXSkinType): Integer;
const
  PartID: array [Boolean] of Integer = (PP_BAR, PP_BARVERT);
  VistaPartID: array [Boolean] of Integer = (5, 6); // PP_FILL, PP_FILLVERT
  ChunkID: array [Boolean] of Integer = (PP_CHUNK, PP_CHUNKVERT);
var
  ChunkPaint: Boolean;
  I: Integer;
  DeltaR, R: TRect;
  B: TBitmap;
  Percentage: Double;
begin
  Result := 0;
  ChunkPaint := False;
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;
  SkinType := SpTBXSkinType(SkinType);

  // Get the delta
  if (Max > Min) and (Position > Min) then begin
    DeltaR := ARect;
    case SkinType of
      sknWindows:
        if not SpIsWinVista then
          if Vertical then InflateRect(DeltaR, -3, -4)
          else InflateRect(DeltaR, -4, -3);
      sknNone: InflateRect(DeltaR, -2, -2);
    end;
    // Cast Position to a Double real type, otherwise Percentage * 100
    // returns a negative value, e.g. 30000000 * 100
    Percentage := Position;
    Percentage := (Percentage * 100) / (Max - Min);
    Result := Round(Percentage);
    if Vertical then
      DeltaR.Top := DeltaR.Bottom - (((DeltaR.Bottom - DeltaR.Top) * Result) div 100)
    else
      DeltaR.Right := DeltaR.Left + (((DeltaR.Right - DeltaR.Left) * Result) div 100);
  end
  else
    DeltaR := Rect(0, 0, 0, 0);

  B := TBitmap.Create;
  try
    case SkinType of
      sknWindows:
        begin
          DrawThemeBackground(ThemeServices.Theme[teProgress], ACanvas.Handle, PartID[Vertical], 0, ARect, nil);
          if DrawProgress and not IsRectEmpty(DeltaR) then begin
            if SpIsWinVista then
              DrawThemeBackground(ThemeServices.Theme[teProgress], ACanvas.Handle, VistaPartID[Vertical], 1, DeltaR, nil)
            else begin
              // [Theme-Change]
              // Another Windows API bug, Windows XP progress bar chunks are 8 x 11,
              // but DrawThemeBackground draws 10 x 11 chunks. We must draw the chunks manually.
              if Vertical then begin
                B.Width := DeltaR.Right - DeltaR.Left;
                B.Height := 8;
                R := Rect(0, 2, B.Width, B.Height);
              end
              else begin
                B.Width := 8;
                B.Height := DeltaR.Bottom - DeltaR.Top;
                R := Rect(0, 0, B.Width - 2, B.Height);
              end;
              DrawThemeBackground(ThemeServices.Theme[teProgress], B.Canvas.Handle, ChunkID[Vertical], 0, R, nil);
              ChunkPaint := True;
            end;
          end;
        end;
      sknNone:
        begin
          DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
          if DrawProgress and not IsRectEmpty(DeltaR) then
            if Smooth then begin
              ACanvas.Brush.Color := clHighlight;
              ACanvas.FillRect(DeltaR);
            end
            else begin
              // Chunks are 10 x 13
              if Vertical then begin
                B.Width := DeltaR.Right - DeltaR.Left;
                B.Height := 10;
                R := Rect(0, 2, B.Width, B.Height);
              end
              else begin
                B.Width := 10;
                B.Height := DeltaR.Bottom - DeltaR.Top;
                R := Rect(0, 0, B.Width - 2, B.Height);
              end;
              B.Canvas.Brush.Color := clBtnFace;
              B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
              B.Canvas.Brush.Color := clHighlight;
              B.Canvas.FillRect(R);
              ChunkPaint := True;
            end;
        end;
      sknSkin:
        begin
          CurrentSkin.PaintBackground(ACanvas, ARect, skncProgressBar, sknsNormal, True, True);
          if DrawProgress and not IsRectEmpty(DeltaR) then begin
            B.Width := ARect.Right - ARect.Left;
            B.Height := ARect.Bottom - ARect.Top;
            R := Rect(0, 0, B.Width, B.Height);
            B.Canvas.CopyRect(R, ACanvas, ARect); // B is transparent

            CurrentSkin.PaintBackground(B.Canvas, R, skncProgressBar, sknsHotTrack, True, True);

            if Vertical then
              R.Top := R.Bottom - (DeltaR.Bottom - DeltaR.Top)
            else
              R.Right := DeltaR.Right - DeltaR.Left;
            ACanvas.CopyRect(DeltaR, B.Canvas, R);
          end;
        end;
    end;

    if ChunkPaint then begin
      if Vertical then begin
        ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Top + 2);
        I := DeltaR.Bottom - B.Height;
        while I > DeltaR.Top - B.Height do begin
          ACanvas.Draw(DeltaR.Left, I, B);
          Dec(I, B.Height);
        end;
      end
      else begin
        ExcludeClipRect(ACanvas.Handle, ARect.Right - 2, ARect.Top, ARect.Right, ARect.Bottom);
        I := DeltaR.Left;
        while I < DeltaR.Right do begin
          ACanvas.Draw(I, DeltaR.Top, B);
          Inc(I, B.Width);
        end;
      end;
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    B.Free;
  end;
end;

procedure SpDrawXPTrackBar(ACanvas: TCanvas; ARect: TRect; Part: Cardinal;
  Vertical, Pushed, ChannelSelection: Boolean; TickMark: TSpTBXTickMark;
  Min, Max, SelStart, SelEnd: Integer; SkinType: TSpTBXSkinType);
var
  Flags: Integer;

  procedure DrawChannelSelection(ChannelR: TRect);
  var
    I: Integer;
    Step : Single;
  begin
    if not ChannelSelection then Exit;
    I := Max - Min;
    if (I > 0) and (SelEnd > SelStart) then begin
      if SkinType = sknSkin then
        InflateRect(ChannelR, -2, -2)
      else
        InflateRect(ChannelR, -1, -1);
      Step := (ChannelR.Right - ChannelR.Left) / I;
      ChannelR.Right := ChannelR.Left + Round(SelEnd * Step);
      ChannelR.Left := ChannelR.Left  + Round(SelStart * Step);

      if SkinType = sknSkin then
        CurrentSkin.PaintBackground(ACanvas, ChannelR, skncTrackBar, sknsHotTrack, True, True)
      else begin
        ACanvas.Brush.Color := clHighlight;
        ACanvas.FillRect(ChannelR);
      end;
    end;
  end;

begin
  SkinType := SpTBXSkinType(SkinType);
  case SkinType of
    sknWindows:
      if Part = TBCD_THUMB then begin
        if Pushed then Flags := TUS_HOT
        else Flags := TUS_NORMAL;
        Case TickMark of
          tmxBottomRight:
            if Vertical then Part := TKP_THUMBRIGHT
            else Part := TKP_THUMBBOTTOM;
          tmxTopLeft:
            if Vertical then Part := TKP_THUMBLEFT
            else Part := TKP_THUMBTOP;
          tmxBoth, tmxCenter:
            if Vertical then Part := TKP_THUMBVERT
            else Part := TKP_THUMB;
        end;
        DrawThemeBackground(ThemeServices.Theme[teTrackBar], ACanvas.Handle, Part, Flags, ARect, nil);
      end
      else if Part = TBCD_CHANNEL then begin
        if Vertical then Part := TKP_TRACKVERT
        else Part := TKP_TRACK;
        DrawThemeBackground(ThemeServices.Theme[teTrackBar], ACanvas.Handle, Part, TKS_NORMAL, ARect, nil);
        DrawChannelSelection(ARect);
      end;
    sknNone:
      if Part = TBCD_THUMB then begin
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ARect);
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
      end
      else if Part = TBCD_CHANNEL then begin
        ACanvas.Brush.Color := clWindow;
        ACanvas.FillRect(ARect);
        ExtCtrls.Frame3D(ACanvas, ARect, clBtnShadow, clBtnHighlight, 1);
        ExtCtrls.Frame3D(ACanvas, ARect, cl3DDkShadow, clBtnFace, 1);
        DrawChannelSelection(ARect);
      end;
    sknSkin:
      if Part = TBCD_THUMB then begin
        if Pushed then
          CurrentSkin.PaintBackground(ACanvas, ARect, skncTrackBarButton, sknsPushed, True, True)
        else
          CurrentSkin.PaintBackground(ACanvas, ARect, skncTrackBarButton, sknsNormal, True, True);
      end
      else if Part = TBCD_CHANNEL then begin
        CurrentSkin.PaintBackground(ACanvas, ARect, skncTrackBar, sknsNormal, True, True);
        DrawChannelSelection(ARect);
      end;
  end;
end;

procedure SpInvalidateSpTBXControl(AControl: TWinControl; InvalidateChildren, OnlySpTBXControls: Boolean);
var
  I: Integer;
  ChildW: TWinControl;
begin
  // Invalidate will not fire WM_ERASEBKGND, because csOpaque is setted
  if Assigned(AControl) and not (csDestroying in AControl.ComponentState) and AControl.HandleAllocated then
  begin
    if InvalidateChildren then begin
      if OnlySpTBXControls then begin
        RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
        // Only invalidate SpTBXControls
        for I := 0 to AControl.ControlCount - 1 do
          if Assigned(AControl.Controls[I]) and (AControl.Controls[I] is TWinControl) then begin
            ChildW := AControl.Controls[I] as TWinControl;
            if ChildW is TSpTBXTextObject then
              RedrawWindow(ChildW.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE)
            else
              PostMessage(ChildW.Handle, CM_SPTBXCONTROLSINVALIDATE, ChildW.Width, ChildW.Height);
          end;
      end
      else
        RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
    end
    else
      RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
  end;
end;

procedure ApplyMargins(var R: TRect; const Margins: TRect); overload;
begin
  with Margins do begin
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

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomPanel }

constructor TSpTBXCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FBackground := TBitmap.Create;

  FBorders := True;
  FBorderType := pbrEtched;
  FSkinType := sknSkin;
  Color := clNone;
  ParentColor := False;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXCustomPanel.Destroy;
begin
  FreeAndNil(FBackground);
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TSpTBXCustomPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -2, -2);
end;

procedure TSpTBXCustomPanel.InvalidateBackground(InvalidateChildren: Boolean);
begin
  // Force background repaint
  if Assigned(FBackground) then
    FBackground.Width := 1;
  SpInvalidateSpTBXControl(Self, InvalidateChildren, True);
end;

procedure TSpTBXCustomPanel.SetBorders(const Value: Boolean);
begin
  if FBorders <> Value then begin
    FBorders := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomPanel.SetBorderType(const Value: TSpTBXPanelBorder);
begin
  if FBorderType <> Value then begin
    FBorderType := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomPanel.SetSkinType(const Value: TSpTBXSkinType);
begin
  if Value <> FSkinType then begin
    FSkinType := Value;
    InvalidateBackground(False);
  end;
end;

procedure TSpTBXCustomPanel.SetTBXStyleBackground(const Value: Boolean);
begin
  if FTBXStyleBackground <> Value then begin
    FTBXStyleBackground := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomPanel.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXCustomPanel.DrawBackground(ACanvas: TCanvas; ARect: TRect);
begin
  SpDrawXPPanel(ACanvas, ARect, True, FTBXStyleBackground, FSkinType, FBorderType);
end;

procedure TSpTBXCustomPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  InvalidateBackground(False);
end;

procedure TSpTBXCustomPanel.CMSpTBXControlsInvalidate(var Message: TMessage);
begin
  InvalidateBackground;
  Message.Result := 1;
end;

procedure TSpTBXCustomPanel.WMEraseBkgnd(var Message: TMessage);
var
  R, R2: TRect;
  PaintDefault: Boolean;
  ACanvas: TCanvas;
begin
  Message.Result := 1;

  if (not DoubleBuffered or (Message.wParam = Message.lParam)) and
    not (csDestroying in ComponentState) and Assigned(FBackground) then
  begin
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := TWMEraseBkgnd(Message).DC;
      R := ClientRect;

      if (FBackground.Width = R.Right) and (FBackground.Height = R.Bottom) and not Assigned(FOnDrawBackground) then
        ACanvas.Draw(R.Left, R.Top, FBackground)
      else begin
        FBackground.Width := R.Right;
        FBackground.Height := R.Bottom;

        if (Color = clNone) and Assigned(Parent) then begin
          // The Panel is a special component, it has the ability
          // to paint the parent background on its children controls.
          // For that it receives WM_ERASEBKGND messages from its children
          // via SpDrawParentBackground.
          SpDrawParentBackground(Self, FBackground.Canvas.Handle, R);
          // PerformEraseBackground(Self, FBackground.Canvas.Handle);
        end
        else
          Windows.FillRect(FBackground.Canvas.Handle, ClientRect, Brush.Handle);

        // Set the Font after SpDrawParentBackground, DrawThemeParentBackground,
        // or PerformEraseBackground.
        // The API messes the font, it seems it destroys it.
        // For more info see:
        // - TCustomActionControl.DrawBackground for more info.
        // - Theme Explorer Main.pas TMainForm.ControlMessage
        //   (http://www.soft-gems.net:8080/browse/Demos)
        FBackground.Canvas.Font.Handle := 0;  // Reset the font, it gets destroyed
        FBackground.Canvas.Font.Assign(Self.Font);

        PaintDefault := True;
        DoDrawBackground(FBackground.Canvas, R, pstPrePaint, PaintDefault);
        if PaintDefault then begin
          if not FBorders then begin
            R2 := R;
            InflateRect(R2, 3, 3);
            DrawBackground(FBackground.Canvas, R2);
          end
          else
            DrawBackground(FBackground.Canvas, R);
        end;
        PaintDefault := True;
        DoDrawBackground(FBackground.Canvas, R, pstPostPaint, PaintDefault);

        ACanvas.Draw(R.Left, R.Top, FBackground);
      end;
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  end;
end;

procedure TSpTBXCustomPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  InvalidateBackground;
end;

procedure TSpTBXCustomPanel.WMSpSkinChange(var Message: TMessage);
begin
  InvalidateBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPanel }

procedure TSpTBXPanel.DrawBackground(ACanvas: TCanvas; ARect: TRect);
begin
  if not TBXStyleBackground and FHotTrack then begin
    if SpTBXSkinType(SkinType) = sknNone then
      SpDrawXPPanelBorder(ACanvas, ARect, pbrDoubleSunken)
    else
      SpDrawXPEditFrame(ACanvas, ARect, Enabled, FHotTracking, SkinType, True, True);
  end
  else
    inherited;
end;

procedure TSpTBXPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := SpFindControl(Self, Message.Sender) > -1;
    if FChildFocused <> FHotTracking then
      SetHotTracking(FChildFocused);
  end;
end;

procedure TSpTBXPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then
    SetHotTracking(True);
end;

procedure TSpTBXPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then
    SetHotTracking(False);
end;

procedure TSpTBXPanel.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBackground(False);
  end;
end;

procedure TSpTBXPanel.SetHotTracking(const Value: Boolean);
begin
  if SpTBXSkinType(SkinType) = sknSkin then begin
    FHotTracking := Value;
    InvalidateBackground(False);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomGroupBox }

constructor TSpTBXCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];
  Width := 185;
  Height := 105;
end;

procedure TSpTBXCustomGroupBox.AdjustClientRect(var Rect: TRect);
var
  R: TRect;
  H: Integer;
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  R := Rect;
  H := SpDrawXPText(Canvas, '0', R, DT_SINGLELINE or DT_CALCRECT);
  Inc(Rect.Top, H);
end;

procedure TSpTBXCustomGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and SpCanFocus(Self) then begin
      SelectFirst;
      Result := 1;
    end
    else
      inherited;
end;

procedure TSpTBXCustomGroupBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  InvalidateBackground(False);
  Realign;
end;

procedure TSpTBXCustomGroupBox.DrawBackground(ACanvas: TCanvas; ARect: TRect);
var
  Flags: Cardinal;
begin
  Flags := DT_SINGLELINE;
  if UseRightToLeftAlignment then
    Flags := Flags or DT_RTLREADING;
  SpDrawXPGroupBox(ACanvas, ARect, Caption, Flags, True, TBXStyleBackground, SkinType);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTextObjectActionLink }

{$IFNDEF UNICODE}
procedure TSpTBXTextObjectActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FUnicodeClient := AClient as TSpTBXTextObject;
end;

function TSpTBXTextObjectActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FUnicodeClient.Checked = (Action as TCustomAction).Checked);
end;

procedure TSpTBXTextObjectActionLink.SetCaption(const Value: String);
begin
  if IsCaptionLinked then
    if Action is TTntAction then
      FUnicodeClient.Caption := TntActnList.TntAction_GetNewCaption(Action as TCustomAction, Value)
    else
      FUnicodeClient.Caption := Value;
end;

procedure TSpTBXTextObjectActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FUnicodeClient.Checked := Value;
end;

procedure TSpTBXTextObjectActionLink.SetHint(const Value: String);
begin
  if IsHintLinked then
    if Action is TTntAction then
      FUnicodeClient.Hint := TntActnList.TntAction_GetNewHint(Action as TCustomAction, Value)
    else
      FUnicodeClient.Hint := Value;
end;

procedure TSpTBXTextObjectActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FUnicodeClient.ImageIndex := Value;
end;
{$ENDIF}

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTextObject }

constructor TSpTBXTextObject.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csSetCaption] - [csAcceptsControls, csOpaque];

  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FAlignment := taLeftJustify;
  FCaptionGlowColor := clYellow;
  FDisabledIconCorrection := True;
  FDrawPushedCaption := False;
  FShowAccelChar := True;
  FSkinType := sknSkin;

  Autosize := True;
  Color := clNone;
  DoubleBuffered := True;
  ParentColor := False;
  TabStop := True;
  Width := 100;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXTextObject.Destroy;
begin
  FImageChangeLink.Free;
  SkinManager.RemoveSkinNotification(Self);

  inherited;
end;

procedure TSpTBXTextObject.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {
  if not (csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
      }
end;

procedure TSpTBXTextObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then SetImages(nil);
end;

procedure TSpTBXTextObject.DefineProperties(Filer: TFiler);
begin
  inherited;

  // [Backward-Compatibility]: Don't read/save LinkFont, it's not used anymore
  Filer.DefineProperty('LinkFont.Charset', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Color', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Height', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Name', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Orientation', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Pitch', ReadLinkFont, nil, False);
  Filer.DefineProperty('LinkFont.Style', ReadLinkFont, nil, False);

  Filer.DefineProperty('EditButton.LinkFont.Charset', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Color', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Height', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Name', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Orientation', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Pitch', ReadLinkFont, nil, False);
  Filer.DefineProperty('EditButton.LinkFont.Style', ReadLinkFont, nil, False);

  Filer.DefineProperty('SpinButton.LinkFont.Charset', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Color', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Height', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Name', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Orientation', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Pitch', ReadLinkFont, nil, False);
  Filer.DefineProperty('SpinButton.LinkFont.Style', ReadLinkFont, nil, False);
end;

function TSpTBXTextObject.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpTBXTextObjectActionLink;
end;

procedure TSpTBXTextObject.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;

  {$IFNDEF UNICODE}
  if Action is TTntAction then
    with TTntAction(Sender) do begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
    end;
  {$ENDIF}

  if Sender is TCustomAction then
    with TCustomAction(Sender) do begin
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

procedure TSpTBXTextObject.AdjustFont(AFont: TFont);
begin
  if (FLinkText <> '') and MouseInControl then begin
    AFont.Color := clBlue;
    AFont.Style := AFont.Style + [fsUnderline];
  end;
end;

procedure TSpTBXTextObject.AdjustBounds;
var
  NewWidth, NewHeight: Integer;
begin
  if HandleAllocated and not FUpdating and ([csReading, csLoading] * ComponentState = []) and AutoSize then
  begin
    FUpdating := True;
    try
      NewWidth := Width;
      NewHeight := 0;
      DoAdjustBounds(NewWidth, NewHeight);
      SetBounds(Left, Top, NewWidth, NewHeight);
    finally
      FUpdating := False;
    end;
  end;
end;

function TSpTBXTextObject.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TSpTBXTextObject.Click;
begin
  if not (csLoading in ComponentState) then begin
    Invalidate;
    inherited;
    ExecuteLink;
  end;
end;

function TSpTBXTextObject.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if not FUpdating and ([csReading, csLoading] * ComponentState = []) and AutoSize then begin
    FUpdating := True;
    try
      NewHeight := 0;
      DoAdjustBounds(NewWidth, NewHeight);
      Result := True;
    finally
      FUpdating := False;
    end;
  end
  else
    Result := False;
end;

procedure TSpTBXTextObject.DoAdjustBounds(var NewWidth, NewHeight: Integer);
var
  R, R1, R2: TRect;
begin
  GetSize(R, R1, R2);
  NewHeight := R.Bottom - R.Top;
  if Wrapping = twNone then
    NewWidth := R.Right - R.Left;
end;

procedure TSpTBXTextObject.DoDrawHint(AHintBitmap: TBitmap;
  var AHint: Widestring; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawHint) then FOnDrawHint(Self, AHintBitmap, AHint, PaintDefault);
end;

function TSpTBXTextObject.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDraw) then FOnDraw(Self, ACanvas, ARect, PaintStage, Result);
end;

function TSpTBXTextObject.DoDrawText(ACanvas: TCanvas; var ARect: TRect;
  Flags: Longint): Integer;
var
  PaintDefault: Boolean;
  GlyphSize, DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
  R, R1, R2: TRect;
  WS: WideString;
  TextFlags: Cardinal;
  State: TSpTBXSkinStatesType;
begin
  Result := 0;
  WS := Caption;
  TextFlags := Flags;
  if TextFlags and DT_CALCRECT = 0 then begin
    ACanvas.Brush.Style := bsClear;
    State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);

    PaintDefault := True;
    if Assigned(FOnDrawCaption) then
      FOnDrawCaption(Self, ACanvas, ClientRect, State, WS, ARect, TextFlags, False, pstPrePaint, PaintDefault);

    if PaintDefault then begin
      // Calc the rects
      GlyphSize := GetGlyphSize;
      DummyRightGlyphSize.cx := 0;
      DummyRightGlyphSize.cy := 0;
      DummyRightGlyphRect := Rect(0, 0, 0, 0);
      SpCalcXPText(ACanvas, ARect, WS, GetRealAlignment(Self), TextFlags, GlyphSize, DummyRightGlyphSize, ghlGlyphLeft, DrawPushedCaption and Pushed, R1, R2, DummyRightGlyphRect, FCaptionRoatationAngle);

      // Paint the text
      if not Enabled then
        if SpTBXSkinType(FSkinType) = sknNone then begin
          OffsetRect(R1, 1, 1);
          ACanvas.Font.Color := clBtnHighlight;
          SpDrawXPText(ACanvas, WS, R1, TextFlags, FCaptionGlow, FCaptionGlowColor, FCaptionRoatationAngle);
          OffsetRect(R1, -1, -1);
          ACanvas.Font.Color := clGrayText;
        end;
      SpDrawXPText(ACanvas, WS, R1, TextFlags, FCaptionGlow, FCaptionGlowColor, FCaptionRoatationAngle);

      // Paint the glyph
      DoInternalGlyphDraw(ACanvas, R2);
    end;

    PaintDefault := True;
    if Assigned(FOnDrawCaption) then
      FOnDrawCaption(Self, ACanvas, ClientRect, State, WS, ARect, TextFlags, False, pstPostPaint, PaintDefault);
    if PaintDefault then
      if GetFocused then begin
        R := ClientRect;
        SpDrawFocusRect(ACanvas, GetFocusRect(R, R1, R2));
      end;
  end
  else
    Result := SpDrawXPText(ACanvas, WS, ARect, TextFlags);
end;

procedure TSpTBXTextObject.DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer);
begin
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, AImageList, AImageIndex);
end;

procedure TSpTBXTextObject.DoInternalGlyphDraw(ACanvas: TCanvas;
  AGlyphRect: TRect);
var
  I: Integer;
  IL: TCustomImageList;
begin
  IL := FImages;
  I := FImageIndex;
  DoGetImageIndex(IL, I);
  if Assigned(IL) and (I > -1) and (I < IL.Count) then
    SpDrawImageList(ACanvas, AGlyphRect, IL, I, Enabled, FDisabledIconCorrection)
end;

procedure TSpTBXTextObject.DoMouseEnter;
begin
  Invalidate;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);  
end;

procedure TSpTBXTextObject.DoMouseLeave;
begin
  Invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TSpTBXTextObject.ExecuteLink;
begin
  if FLinkText <> '' then
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      ShellExecuteA(Application.Handle, 'open', PAnsiChar(AnsiString(FLinkText)), PAnsiChar(AnsiString(FLinkTextParams)), '', SW_SHOWNORMAL)
    else
      ShellExecuteW(Application.Handle, 'open', PWideChar(FLinkText), PWideChar(FLinkTextParams), '', SW_SHOWNORMAL);
end;

function TSpTBXTextObject.GetControlsAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TSpTBXTextObject.GetTextFlags: Cardinal;
const
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
  ShowAccelChars: array [Boolean] of Integer = (DT_NOPREFIX, 0);
  SystemAccelChars: array [Boolean] of Integer = (DT_HIDEPREFIX, 0);
begin
  // Note on SystemAccelChars: custom controls need to update the accel painting
  // in response to WM_UPDATEUISTATE, call WM_QUERYUISTATE to get the accel
  // painting state:
  // http://blogs.msdn.com/oldnewthing/archive/2005/05/03/414317.aspx
  //
  //  Another way of doing it is updating a flag in WM_UPDATEUISTATE, without
  //  calling WM_QUERYUISTATE everytime the control needs to be painted:
  //  begin
  //    if LoWord(Message.WParam) and UISF_HIDEACCEL = UISF_HIDEACCEL then begin
  //      if HiWord(Message.WParam) and UIS_CLEAR = UIS_CLEAR then
  //        FSystemShowAccelChar := True;
  //      if HiWord(Message.WParam) and UIS_SET = UIS_SET then
  //        FSystemShowAccelChar := False;
  //    end;
  //    inherited;
  //  end;
  //
  // To test this use the mouse to run the app on the IDE, don't use F9
  // otherwise the accel will always be visible

  Result := DT_EXPANDTABS or WordWraps[Wrapping] or
    Alignments[GetRealAlignment(Self)] or
    ShowAccelChars[ShowAccelChar] or SystemAccelChars[SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL = 0];
  Result := DrawTextBiDiModeFlags(Result);
end;

function TSpTBXTextObject.GetFocusRect(R, TextR, GlyphR: TRect): TRect;
begin
  if Caption = '' then
    Result := Rect(0, 0, 0, 0)
  else begin
    InflateRect(TextR, 1, 1);
    Result := TextR;
  end;
end;

function TSpTBXTextObject.GetGlyphSize: TSize;
var
  I: Integer;
  IL: TCustomImageList;
begin
  IL := FImages;
  I := FImageIndex;
  DoGetImageIndex(IL, I);
  if Assigned(IL) and (I > -1) and (I < IL.Count) then begin
    Result.cx := IL.Width;
    Result.cy := IL.Height;
  end
  else begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

function TSpTBXTextObject.GetFocused: Boolean;
begin
  Result := Focused;
end;

function TSpTBXTextObject.GetPushed: Boolean;
begin
  Result := FPushed and MouseInControl;
end;

procedure TSpTBXTextObject.GetSize(out TotalR, TextR, GlyphR: TRect);
// Size of Text + Glyph + TextMargin + Margins
var
  GlyphSize, DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
  R: TRect;
begin
  GlyphSize := GetGlyphSize;
  DummyRightGlyphSize.cx := 0;
  DummyRightGlyphSize.cy := 0;
  DummyRightGlyphRect := Rect(0, 0, 0, 0);
  R := ClientRect;
  ApplyMargins(R, GetTextMargins);

  Canvas.Font.Assign(Font);
  AdjustFont(Canvas.Font);
  SpCalcXPText(Canvas, R, Caption, GetRealAlignment(Self), GetTextFlags, GlyphSize, DummyRightGlyphSize,
    ghlGlyphLeft, DrawPushedCaption and Pushed, TextR, GlyphR, DummyRightGlyphRect);

  UnionRect(TotalR, TextR, GlyphR);

  {$IF CompilerVersion > 17}
  if Autosize then
    with Margins do begin
      Inc(TotalR.Right, Left + Right);
      Inc(TotalR.Bottom, Top + Bottom);
    end;
  {$IFEND}
end;

function TSpTBXTextObject.GetTextMargins: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

procedure TSpTBXTextObject.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled and not FPushed then begin
    FPushed := True;
    if not Focused and CanFocus then
      SetFocus // Invalidates the canvas
    else
      Invalidate;
  end;
  inherited;
end;

procedure TSpTBXTextObject.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FPushed then
    UpdateTracking;
  inherited;
end;

procedure TSpTBXTextObject.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FPushed := False;
  Invalidate;
  inherited;
end;

procedure TSpTBXTextObject.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) and FSpaceAsClick then begin
    FPushed := True;
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and FSpaceAsClick and FPushed then begin
    FPushed := False;
    FMouseInControl := False;
    Click;
    Invalidate;
  end;
  inherited;
end;

procedure TSpTBXTextObject.Paint;
var
  R, TextR: TRect;
begin
  R := ClientRect;
  Canvas.Font.Assign(Font);
  AdjustFont(Canvas.Font);
  // Draw the background
  DoDrawItem(Canvas, R, pstPrePaint);
  // Draw the text
  TextR := R;
  ApplyMargins(TextR, GetTextMargins);
  DoDrawText(Canvas, TextR, GetTextFlags);
  // Draw the Focus, Icon and Text
  DoDrawItem(Canvas, R, pstPostPaint);
end;

procedure TSpTBXTextObject.ReadLinkFont(Reader: TReader);
begin
  // [Backward-Compatibility]
  Reader.SkipValue;
end;

procedure TSpTBXTextObject.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.Loaded;
begin
  inherited;
  AdjustBounds;
end;

procedure TSpTBXTextObject.SetCaptionGlow(const Value: TSpGlowDirection);
begin
  if FCaptionGlow <> Value then begin
    FCaptionGlow := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.SetCaptionGlowColor(const Value: TColor);
begin
  if FCaptionGlowColor <> Value then begin
    FCaptionGlowColor := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.SetCaptionRoatationAngle(const Value: TSpTextRotationAngle);
begin
  if FCaptionRoatationAngle <> Value then begin
    FCaptionRoatationAngle := Value;
    Invalidate;
  end;
end;

function TSpTBXTextObject.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TSpTBXTextObject.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then begin
    FChecked := Value;
    Invalidate;
  end;
end;

function TSpTBXTextObject.IsImageIndexValid: Boolean;
var
  I: Integer;
  IL: TCustomImageList;
begin
  IL := FImages;
  I := FImageIndex;
  DoGetImageIndex(IL, I);
  Result := Assigned(IL) and (I > -1) and (I < IL.Count);
end;

procedure TSpTBXTextObject.ImageListChange(Sender: TObject);
begin
  if Sender = Images then begin
    Invalidate;
    AdjustBounds;
  end;
end;

procedure TSpTBXTextObject.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if Assigned(Images) then Invalidate;
    AdjustBounds;
  end;
end;

procedure TSpTBXTextObject.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
  AdjustBounds;
end;

procedure TSpTBXTextObject.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.SetSkinType(const Value: TSpTBXSkinType);
begin
  if FSkinType <> Value then begin
    FSkinType := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.SetWrapping(Value: TTextWrapping);
begin
  if FWrapping <> Value then begin
    FWrapping := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.UpdateTracking(ForceMouseLeave: Boolean = False);
var
  P: TPoint;
  IsInControl: Boolean;
begin
  if ForceMouseLeave then begin
    FMouseInControl := True;
    Perform(CM_MOUSELEAVE, 0, 0)
  end
  else
    if Enabled then begin
      GetCursorPos(P);
      IsInControl := FindDragTarget(P, True) = Self;

      if FMouseInControl <> IsInControl then begin
        FMouseInControl := not IsInControl;
        if FMouseInControl then
          Perform(CM_MOUSELEAVE, 0, 0)
        else
          Perform(CM_MOUSEENTER, 0, 0);
      end;
    end;
end;

procedure TSpTBXTextObject.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled and FMouseInControl then begin
    FMouseInControl := False;
    DoMouseLeave;
    Perform(WM_CANCELMODE, 0, 0);
  end
  else
    Invalidate;
end;

procedure TSpTBXTextObject.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

procedure TSpTBXTextObject.CMHintShow(var Message: TCMHintShow);
// Handle the CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
var
  HintInfo: PHintInfo;
  WideHint: Widestring;
  R, TextR: TRect;
  PaintDefault: Boolean;
begin
  WideHint := Hint;

  // Prepare the HintBitmap
  SpStockHintBitmap.Canvas.Font.Assign(Screen.HintFont);
  SpStockHintBitmap.Canvas.Font.Color := clInfoText;
  SpStockHintBitmap.Canvas.Pen.Color := clBlack;
  SpStockHintBitmap.Canvas.Brush.Color := clInfoBk;
  TextR := Rect(0, 0, 1, 1);
  SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
  SpStockHintBitmap.Width := TextR.Right + 8;
  SpStockHintBitmap.Height := TextR.Bottom + 4;

  // Draw the hint in the HintBitmap
  PaintDefault := True;
  DoDrawHint(SpStockHintBitmap, WideHint, PaintDefault);
  if PaintDefault then begin
    // Prepare the HintInfo
    HintInfo := Message.HintInfo;
    HintInfo.HintStr := WideHint;
    HintInfo.CursorRect := ClientRect;
    HintInfo.HintWindowClass := SpTBXHintWindowClass;   // Custom HintWindow class
    HintInfo.HintData := SpStockHintBitmap;  // TApplication.ActivateHint will pass the data to the HintWindow
    HintInfo.HideTimeout := 60000; // 1 minute

    // Recalculate TextR
    TextR := Rect(0, 0, 1, 1);
    SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
    SpStockHintBitmap.Width := TextR.Right + 8;
    SpStockHintBitmap.Height := TextR.Bottom + 4;
    // Draw the hint
    R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);
    SpStockHintBitmap.Canvas.FillRect(R);
    OffsetRect(TextR, ((R.Right - TextR.Right) div 2) - 2, (R.Bottom - TextR.Bottom) div 2);
    SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX);
  end;
end;

procedure TSpTBXTextObject.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl then begin
    FMouseInControl := True;
    DoMouseEnter;
  end;
end;

procedure TSpTBXTextObject.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl then begin
    FMouseInControl := False;
    DoMouseLeave;
  end;
end;

procedure TSpTBXTextObject.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

procedure TSpTBXTextObject.WMEraseBkgnd(var Message: TMessage);
begin
  if not DoubleBuffered or (Message.wParam = Message.lParam) then
  begin
    if (Color = clNone) and Assigned(Parent) then
      SpDrawParentBackground(Self, TWMEraseBkgnd(Message).DC, ClientRect)
    else
      Windows.FillRect(TWMEraseBkgnd(Message).DC, ClientRect, Brush.Handle);
  end;
  Message.Result := 1;
end;

procedure TSpTBXTextObject.WMKillFocus(var Message: TMessage);
begin
  inherited;
  FPushed := False;
  Invalidate;
end;

procedure TSpTBXTextObject.WMSetFocus(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSpTBXTextObject.WMSetCursor(var Message: TWMSetCursor);
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (FLinkText <> '') and MouseInControl and (Screen.Cursor = crDefault) then
  begin
    // Replace the Delphi hand cursor for the one used by Windows only if
    // there is no other cursor assigned.
    Windows.SetCursor(Screen.Cursors[crSpTBXNewHandPoint]);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TSpTBXTextObject.WMSpSkinChange(var Message: TMessage);
var
  R: TRect;
begin
  if HandleAllocated then begin
    R := ClientRect;
    InvalidateRect(Handle, @R, True);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomLabel }

constructor TSpTBXCustomLabel.Create(AOwner: TComponent);
begin
  inherited;
  FUnderlineColor := clBtnShadow;
  TabStop := False;
end;

procedure TSpTBXCustomLabel.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else
    if (SkinType = sknSkin) and ((AFont.Color = clWindowText) or (AFont.Color = clNone)) then begin
      State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);
      AFont.Color := CurrentSkin.GetTextColor(skncLabel, State);
    end;
end;

function TSpTBXCustomLabel.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  C: TColor;
begin
  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) and FUnderline then begin
    C := ACanvas.Pen.Color;
    try
      ACanvas.Pen.Color := UnderlineColor;
      ACanvas.MoveTo(ARect.Left, ARect.Bottom - 1);
      ACanvas.LineTo(ARect.Right, ARect.Bottom - 1);
    finally
      ACanvas.Pen.Color := C;
    end;
  end;
end;

procedure TSpTBXCustomLabel.GetSize(out TotalR, TextR, GlyphR: TRect);
begin
  inherited GetSize(TotalR, TextR, GlyphR);
  if FUnderline then
    Inc(TotalR.Bottom);
end;

procedure TSpTBXCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FocusControl then SetFocusControl(nil);
end;

procedure TSpTBXCustomLabel.SetFocusControl(const Value: TWinControl);
begin
  if FFocusControl <> Value then
  begin
    FFocusControl := Value;
    if FFocusControl <> nil then FFocusControl.FreeNotification(Self);
  end;
end;

procedure TSpTBXCustomLabel.SetUnderline(const Value: Boolean);
begin
  if Value <> FUnderline then begin
    FUnderline := Value;
    Invalidate;
    AdjustBounds;
  end;
end;

procedure TSpTBXCustomLabel.SetUnderlineColor(const Value: TColor);
begin
  FUnderlineColor := Value;
  Invalidate;
end;

procedure TSpTBXCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if Assigned(FFocusControl) and IsAccel(Message.CharCode, Caption) and SpCanFocus(FFocusControl) then begin
    FFocusControl.SetFocus;
    Message.Result := 1;
  end
  else
    inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXButtonControl }

constructor TSpTBXButtonControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDoubleClicks];  
  FGroupIndex := 0;
end;

function TSpTBXButtonControl.CanFocus: Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled then begin
    Control := Self;
    while Control <> Form do
    begin
      if not (Control.Visible and Control.Enabled) then Exit;
      Control := Control.Parent;
    end;
    Result := True;
  end;
end;

function TSpTBXButtonControl.CanUpdateExclusive: Boolean;
begin
  Result := FGroupIndex <> 0;
end;

procedure TSpTBXButtonControl.UpdateExclusive;
var
  Msg: TMessage;
begin
  if Assigned(Parent) and CanUpdateExclusive then begin
    Msg.Msg := CM_SPGROUPINDEXUPDATE;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TSpTBXButtonControl.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TSpTBXButtonControl.SetChecked(Value: Boolean);
begin
  inherited;
  if Value then UpdateExclusive;
end;

procedure TSpTBXButtonControl.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and ShowAccelChar and IsAccel(Message.CharCode, Caption) and
    CanFocus and Visible then
  begin
    SetFocus;
    Click;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TSpTBXButtonControl.CMSPGroupIndexUpdate(var Message: TMessage);
var
  Sender: TComponent;
  SenderButton: TSpTBXButtonControl;
begin
  if Message.WParam = FGroupIndex then begin
    Sender := TComponent(Message.LParam);
    if (Sender <> Self) and (Sender is TSpTBXButtonControl) and (Sender.ClassType = Self.ClassType) then begin
      SenderButton := Sender as TSpTBXButtonControl;
      if SenderButton.Checked and Checked then
        Checked := False;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomCheckButton }

function TSpTBXCustomCheckButton.GetGlyphSize: TSize;
begin
  Result := inherited GetGlyphSize;
  if (Result.cx = 0) or (Result.cy = 0) then begin
    Result.cx := 13;
    Result.cy := 13;
  end;
end;

procedure TSpTBXCustomCheckButton.GetSize(out TotalR, TextR, GlyphR: TRect);
begin
  inherited GetSize(TotalR, TextR, GlyphR);
  // Inc TotalR for the FocusRect
  if Autosize then begin
    Inc(TotalR.Right);
    Inc(TotalR.Bottom, 2);
  end;
end;

procedure TSpTBXCustomCheckButton.Toggle;
begin
  // Toggle the check state
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomCheckBox }

constructor TSpTBXCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FAllowGrayed := False;
  FState := cbUnchecked;
  FGroupIndex := -1;
  SpaceAsClick := True;
end;

procedure TSpTBXCustomCheckBox.DoInternalGlyphDraw(ACanvas: TCanvas;
  AGlyphRect: TRect);
begin
  if IsImageIndexValid then
    inherited
  else
    SpDrawXPCheckBoxGlyph(ACanvas, AGlyphRect, Enabled, State, MouseInControl, Pushed, SkinType);
end;

procedure TSpTBXCustomCheckBox.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else
    if (SkinType = sknSkin) and ((AFont.Color = clWindowText) or (AFont.Color = clNone)) then begin
      State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);
      AFont.Color := CurrentSkin.GetTextColor(skncCheckBox, State);
    end;
end;

procedure TSpTBXCustomCheckBox.Click;
begin
  if StateChanged then
    inherited
  else
    Toggle; // Toggle calls OnClick
end;

function TSpTBXCustomCheckBox.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

procedure TSpTBXCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Checked <> Value then begin
    inherited;
    if Value then SetState(cbChecked)
    else SetState(cbUnchecked);
  end;
end;

procedure TSpTBXCustomCheckBox.SetState(const Value: TCheckBoxState);
begin
  if (FState <> Value) then begin
    FState := Value;

    // When State is changed OnClick must be fired
    StateChanged := True;
    try
      Click;
    finally
      StateChanged := False;
    end;
  end;
end;

procedure TSpTBXCustomCheckBox.Toggle;
begin
  case State of
    cbUnchecked: if AllowGrayed then SetState(cbGrayed) else SetState(cbChecked);
    cbChecked: SetState(cbUnchecked);
    cbGrayed: SetState(cbChecked);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomRadioButton }

constructor TSpTBXCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := False;
end;

procedure TSpTBXCustomRadioButton.DoInternalGlyphDraw(ACanvas: TCanvas;
  AGlyphRect: TRect);
begin
  if IsImageIndexValid then
    inherited
  else
    SpDrawXPRadioButtonGlyph(ACanvas, AGlyphRect, Enabled, Checked, MouseInControl, Pushed, SkinType);
end;

procedure TSpTBXCustomRadioButton.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else
    if (SkinType = sknSkin) and ((AFont.Color = clWindowText) or (AFont.Color = clNone)) then begin
      State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);
      AFont.Color := CurrentSkin.GetTextColor(skncRadioButton, State);
    end;
end;

function TSpTBXCustomRadioButton.CanUpdateExclusive: Boolean;
begin
  // Special case on RadioButtons, UpdateExclusive on all
  // the radiobuttons regardless of the GroupIndex
  Result := True;
end;

procedure TSpTBXCustomRadioButton.Click;
begin
  if StateChanged then
    inherited
  else
    if not Checked then Toggle; // Toggle calls OnClick
end;

procedure TSpTBXCustomRadioButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if Focused then
    Toggle;
end;

procedure TSpTBXCustomRadioButton.SetChecked(Value: Boolean);
var
  WasChecked: Boolean;
begin
  WasChecked := Checked;
  inherited;
  TabStop := Value;
  // When Checked is true OnClick must be fired
  if not WasChecked and Value then begin
    StateChanged := True;
    try
      Click;
    finally
      StateChanged := False;
    end;
  end;
end;

procedure TSpTBXCustomRadioButton.Toggle;
begin
  if not Checked then Checked := True;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXRadioGroupButton }

type
  TSpTBXRadioGroupButton = class(TSpTBXRadioButton)
  public
    constructor InternalCreate(RadioGroup: TSpTBXCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TSpTBXRadioGroupButton.InternalCreate(RadioGroup: TSpTBXCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Parent := RadioGroup;
  AutoSize := False;
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  TabStop := False;
  OnClick := RadioGroup.ButtonClick;
end;

destructor TSpTBXRadioGroupButton.Destroy;
begin
  TSpTBXCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomRadioGroup }

constructor TSpTBXCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FButtons := TList.Create;
  FItems := TTntStringList.Create;
  TTntStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
end;

destructor TSpTBXCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TTntStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TSpTBXCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TSpTBXCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TSpTBXRadioGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TSpTBXCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TSpTBXCustomRadioGroup.InvalidateBackground(InvalidateChildren: Boolean);
var
  I: Integer;
  T: TSpTBXSkinType;
begin
  inherited;

  if not InvalidateChildren and not (csDestroying in ComponentState) then
    if HandleAllocated then begin
      T := SkinType;
      for I := 0 to FButtons.Count - 1 do
        Buttons[I].SkinType := T;
    end;
end;

procedure TSpTBXCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TSpTBXCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  try
    inherited ReadState(Reader);
  finally
    FReading := False;
  end;
  UpdateButtons;
end;

procedure TSpTBXCustomRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TSpTBXRadioGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do
    TSpTBXRadioGroupButton(FButtons.Last).Free;
end;

procedure TSpTBXCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TSpTBXCustomRadioGroup.SetFocus;
begin
  inherited;
  if Enabled and (FItemIndex > -1) then
    GetButtons(FItemIndex).SetFocus;
end;

procedure TSpTBXCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then
    FItemIndex := Value
  else begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        GetButtons(FItemIndex).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        GetButtons(FItemIndex).Checked := True;
    end;
  end;
end;

procedure TSpTBXCustomRadioGroup.SetItems(Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

procedure TSpTBXCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    Buttons[I].Caption := FItems[I];
  if FItemIndex >= 0 then begin
    FUpdating := True;
    try
      GetButtons(FItemIndex).Checked := True;
    finally
      FUpdating := False;
    end;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TSpTBXCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    GetButtons(I).Enabled := Enabled;
end;

procedure TSpTBXCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Do nothing
end;

function TSpTBXCustomRadioGroup.GetButtons(Index: Integer): TSpTBXRadioButton;
begin
  Result := TSpTBXRadioButton(FButtons[Index]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomButton }

constructor TSpTBXCustomButton.Create(AOwner: TComponent);
begin
  inherited;

  FBitmapTransparent := True;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;

  FPopupControl := Self;
  FDropDownArrow := True;
  Alignment := taCenter;
  Autosize := False;
  DrawPushedCaption := True;
  SpaceAsClick := True;
  Width := 75;
  Height := 25;
end;

destructor TSpTBXCustomButton.Destroy;
begin
  StopRepeat;
  FBitmap.Free;
  inherited;
end;

procedure TSpTBXCustomButton.CreateWnd;
begin
  inherited;
  FActive := FDefault;
end;

procedure TSpTBXCustomButton.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else
    if (SkinType = sknSkin) and ((AFont.Color = clWindowText) or (AFont.Color = clNone)) then begin
      State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);
      AFont.Color := CurrentSkin.GetTextColor(skncButton, State);
    end;
end;

procedure TSpTBXCustomButton.BitmapChanged(Sender: TObject);
begin
  Invalidate;
end;

function TSpTBXCustomButton.BitmapValid: boolean;
begin
  Result := (Bitmap <> nil) and (not Bitmap.Empty) and (Bitmap.Height mod ConstStatesCount = 0);
end;

procedure TSpTBXCustomButton.Click;
var
  P: TPoint;
  Form: TCustomForm;
  M: TPopupMenu;
  SpTBXPopup: ISpTBXPopupMenu;

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

      for I := 0 to RepostList.Count-1 do
      begin
        with PMsg(RepostList[I])^ do PostMessage(hwnd, message, wParam, lParam);
        FreeMem(RepostList[I]);
      end;
    finally
      RepostList.Free;
    end;
  end;

begin
  if not FRepeating then begin
    M := GetInternalDropDownMenu;
    if Assigned(M) then begin
      FDropDownMenuVisible := True;
      try
        UpdateTracking(True);
        MouseCapture := False;
        M.PopupComponent := Self;

        if M.GetInterface(ISpTBXPopupMenu, SpTBXPopup) then begin
          if not SpTBXPopup.InternalPopup(0, 0, False, FPopupControl) then
            FDropDownMenuVisible := False;
        end
        else begin
          P := ClientToScreen(Point(0, Height));
          M.Popup(P.X, P.Y);
          FDropDownMenuVisible := False;
        end;
      finally
        Invalidate;
        RemoveClicks;
      end;
      Exit; // don't call the Click handler if the DropDownMenu is shown
    end;

    Form := GetParentForm(Self);
    if Assigned(Form) then Form.ModalResult := FModalResult;
  end;

  inherited;
end;

function TSpTBXCustomButton.DoDrawDropDownArrow(ACanvas: TCanvas;
  ARect: TRect): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  Result := True;
  if FDropDownArrow and Assigned(FDropDownMenu) then begin
    R := ARect;
    R.Left := R.Right - GetTextMargins.Right;

    P.X := (R.Left + R.Right) div 2 - 1;
    P.Y := (R.Top + R.Bottom) div 2 - 1;
    SpDrawArrow(ACanvas, P.X, P.Y, ACanvas.Font.Color, True, False, 2);
  end;
end;

function TSpTBXCustomButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  B: TBitmap;
  T: TSpTBXSkinType;
begin
  T := SpTBXSkinType(SkinType);

  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) then begin
    if BitmapValid then begin
      B := TBitmap.Create;
      try
        B.Width := ARect.Right - ARect.Left;
        B.Height := ARect.Bottom - ARect.Top;
        SetStretchBltMode(B.Canvas.Handle, COLORONCOLOR);
        B.Canvas.CopyRect(ARect, Bitmap.Canvas, GetSkinStateRect);
        if FBitmapTransparent then
          B.Transparent := True;
        ACanvas.Draw(0, 0, B);
      finally
        B.Free;
      end;
    end
    else begin
      case T of
        sknNone, sknWindows:
          SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, False, FActive, T);
        sknSkin:
          SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, False, False, T);
      end;
    end;
  end;

  // Draw the button arrow
  if Result and (PaintStage = pstPostPaint) then
    DoDrawDropDownArrow(ACanvas, ARect);
end;

function TSpTBXCustomButton.GetFocused: Boolean;
begin
  Result := Focused and (IsDroppedDown or (inherited GetFocused));
end;

function TSpTBXCustomButton.GetFocusRect(R, TextR, GlyphR: TRect): TRect;
begin
  Result := R;
  if SpTBXSkinType(SkinType) = sknNone then
    InflateRect(Result, -4, -4)
  else
    InflateRect(Result, -3, -3);
end;

function TSpTBXCustomButton.GetInternalDropDownMenu: TPopupMenu;
begin
  Result := FDropDownMenu;
end;

function TSpTBXCustomButton.GetPushed: Boolean;
begin
  Result := IsDroppedDown or (inherited GetPushed);
end;

function TSpTBXCustomButton.GetTextMargins: TRect;
const
  ArrowWidth = 5;
begin
  Result := Rect(8, 2, 8, 2);
  if FDropDownArrow and Assigned(FDropdownMenu) then
    Inc(Result.Right, ArrowWidth + 4);
end;

function TSpTBXCustomButton.IsDroppedDown: Boolean;
begin
  Result := FDropDownMenuVisible;
end;

function TSpTBXCustomButton.GetSkinStateRect: TRect;
var
  W, H: integer;
begin
  // Finds the skin rect based on the button state
  Result := Rect(0, 0, 0, 0);

  if BitmapValid then begin
    W := Bitmap.Width;
    H := (Bitmap.Height div ConstStatesCount); // 4 states
    if not Enabled then
      Result := Bounds(0, H * 3, W, H)  // 4th state (disabled)
    else begin
      if Checked or Pushed then
        Result := Bounds(0, H * 2, W, H)  // 3rd state (down)
      else
        if MouseInControl then
          Result := Bounds(0, H * 1, W, H)  // 2nd state (hottrack)
        else
          Result := Bounds(0, H * 0, W, H); // 1st state (up)
    end;
  end;
end;

procedure TSpTBXCustomButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Enabled and (Button = mbLeft) then begin
    if Repeating then begin
      Click;
      ControlState := ControlState - [csClicked];
      if not Assigned(FRepeatTimer) then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.Interval := ConstInitRepeatPause;
      FRepeatTimer.OnTimer := RepeatTimerHandler;
      FRepeatTimer.Enabled := True;
    end;
  end;
end;

procedure TSpTBXCustomButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then StopRepeat;
end;

procedure TSpTBXCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = DropdownMenu then DropdownMenu := nil;
end;

procedure TSpTBXCustomButton.RepeatTimerHandler(Sender: TObject);
begin
  FRepeatTimer.Interval := ConstRepeatPause;
  if Repeating then begin
    if Pushed then
      Click;
  end
  else
    StopRepeat;
end;

procedure TSpTBXCustomButton.StopRepeat;
begin
  if Assigned(FRepeatTimer) then begin
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TSpTBXCustomButton.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  Invalidate;
end;

procedure TSpTBXCustomButton.SetDefault(const Value: Boolean);
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if HandleAllocated then begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
end;

procedure TSpTBXCustomButton.SetDropDownArrow(const Value: Boolean);
begin
  if FDropDownArrow <> Value then begin
    FDropDownArrow := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomButton.SetDropDownMenu(Value: TPopupMenu);
begin
  if FDropDownMenu <> Value then begin
    if Assigned(FDropDownMenu) then RemoveFreeNotification(FDropDownMenu);
    FDropDownMenu := Value;
    if Assigned(FDropDownMenu) then FreeNotification(FDropDownMenu);
    Invalidate;
  end;
end;

procedure TSpTBXCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if (((CharCode = VK_RETURN) and FActive) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TSpTBXCustomButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TSpTBXCustomButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
  inherited;
end;

procedure TSpTBXCustomButton.CMSPPopupClose(var Message: TMessage);
begin
  FDropDownMenuVisible := False;
  Invalidate;
  inherited;
end;

procedure TSpTBXCustomButton.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  StopRepeat;
  UpdateTracking(True);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomSpeedButton }

constructor TSpTBXCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  SetBounds(0, 0, 23, 22);
end;

function TSpTBXCustomSpeedButton.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TSpTBXCustomSpeedButton.Click;
begin
  if FGroupIndex <> 0 then
    Checked := not Checked;
  inherited
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomProgressBar }

constructor TSpTBXCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 100;
  FMin := 0;
  FPosition := 0;
  FProgressVisible := True;
  FCaptionGlow := gldAll;
  FCaptionType := pctPercentage;

  Alignment := taCenter;
  Autosize := False;
  Width := 150;
  Height := 17;
  Font.Style := Font.Style + [fsBold];
  TabStop := False;
end;

procedure TSpTBXCustomProgressBar.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else
    if (SkinType = sknSkin) and ((AFont.Color = clWindowText) or (AFont.Color = clNone)) then begin
      State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl, Checked);
      AFont.Color := CurrentSkin.GetTextColor(skncProgressBar, State);
    end;
end;

function TSpTBXCustomProgressBar.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  I: Integer;
  T: TSpTBXSkinType;
begin
  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) then begin
    T := SpTBXSkinType(SkinType);
    I := SpDrawXPProgressBar(ACanvas, ARect, FVertical, FSmooth, FProgressVisible, FMin, FMax, FPosition, T);
    case FCaptionType of
      pctNone: Caption := '';
      pctPercentage: Caption := IntToStr(I) + '%';
      pctProgress: Caption := IntToStr(FPosition);
    end;
  end;
end;

procedure TSpTBXCustomProgressBar.DoProgressChange;
begin
  if Assigned(FOnProgressChange) then FOnProgressChange(Self, Position);
end;

function TSpTBXCustomProgressBar.GetTextMargins: TRect;
begin
  Result := Rect(8, 2, 8, 2);
end;

procedure TSpTBXCustomProgressBar.SetCaptionType(const Value: TSpTBXProgressCaption);
begin
  if FCaptionType <> Value then begin
    FCaptionType := Value;
    if Value <> pctDefault then Caption := '';
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetMax(const Value: integer);
begin
  if FMax <> Value then begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetMin(const Value: integer);
begin
  if FMin <> Value then begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetPosition(Value: integer);
begin
  if Value > FMax then Value := FMax
  else if Value < FMin then Value := FMin;
  if FPosition <> Value then begin
    FPosition := Value;
    Invalidate;
    DoProgressChange;
  end;
end;

procedure TSpTBXCustomProgressBar.SetProgressVisible(const Value: Boolean);
begin
  if FProgressVisible <> Value then begin
    FProgressVisible := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetSmooth(const Value: Boolean);
begin
  if FSmooth <> Value then begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetVertical(const Value: Boolean);
begin
  if FVertical <> Value then begin
    FVertical := Value;
    if FVertical then
      FCaptionRoatationAngle := tra90
    else
      FCaptionRoatationAngle := tra0;
    if Width > Height then
      SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.StepIt(Delta: Integer = 1);
begin
  SetPosition(FPosition + Delta);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTrackBar }

constructor TSpTBXTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FSkinType := sknSkin;
  FTickMarks := tmxBottomRight;
  SkinManager.AddSkinNotification(Self);
end;

procedure TSpTBXTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FCanDrawChannelSelection := (Params.Style and TBS_ENABLESELRANGE) <> 0;
end;

destructor TSpTBXTrackBar.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

function TSpTBXTrackBar.ChannelRect: TRect;
var
  R: TRect;
begin
  // TBM_GETCHANNELRECT allways returns the horizontal channel rect, even
  // when the Orientation is vertical.
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, Integer(@Result));
  if Orientation = trVertical then begin
    R := Result;
    Result := Rect(R.Top, R.Left, R.Bottom, R.Right);
  end;
end;

function TSpTBXTrackBar.DoDrawChannel(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawChannel) then FOnDrawChannel(Self, ACanvas, ARect, PaintStage, Result);
end;

function TSpTBXTrackBar.DoDrawChannelTicks(ACanvas: TCanvas; X, Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawChannelTicks) then FOnDrawChannelTicks(Self, ACanvas, X, Y, Result);
end;

function TSpTBXTrackBar.DoDrawThumb(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawThumb) then FOnDrawThumb(Self, ACanvas, ARect, PaintStage, Result);
end;

procedure TSpTBXTrackBar.DrawTicks(ACanvas: TCanvas);
var
  PosArray: array of Integer;
  I, Count, Y, iStart, iEnd: Integer;
  ChannelR, ThumbR: TRect;
  FirstTickSize, TickSize, TickDelta: Integer;
  LastPenColor: TColor;
begin
  // Returns the position of the ticks on the client area
  // Check if Max - Min > 2 to see if the ticks array is valid.
  Count := Max - Min;
  if Count < 2 then
    Count := 2
  else
    Count := Count + 1;
  SetLength(PosArray, Count);

  // Fill the array, the first and last ticks are not included in the ticks array:
  // http://msdn.microsoft.com/library/en-us/shellcc/platform/commctls/trackbar/messages/tbm_getticpos.asp?frame=true
  // First we need to get the middle ticks
  //  0 1 2 3 4 5 6 7 8 9    // Tick positions seen on the trackbar.
  //    1 2 3 4 5 6 7 8      // Tick positions whose position can be identified.
  //    0 1 2 3 4 5 6 7      // Index numbers for the identifiable positions.
  if Count >= 2 then begin
    iStart := 1;
    iEnd := Count - 1 - 1;
    for I := iStart to iEnd do
      PosArray[I] := SendMessage(Self.Handle, TBM_GETTICPOS, I - 1, 0);
  end;

  LastPenColor := ACanvas.Pen.Color;

  case SpTBXSkinType(FSkinType) of
    sknNone:
      ACanvas.Pen.Color := clBlack;
    sknWindows:
      ACanvas.Pen.Color := clBtnShadow;
    sknSkin:
      if CurrentSkin.Options(skncTrackBar, sknsNormal).TextColor <> clNone then
        ACanvas.Pen.Color := CurrentSkin.Options(skncTrackBar, sknsNormal).TextColor
      else
        ACanvas.Pen.Color := clBtnShadow;
  end;

  SendMessage(Self.Handle, TBM_GETTHUMBRECT, 0, Integer(@ThumbR));
  ChannelR := ChannelRect;
  FirstTickSize := 4;
  TickSize := 3;
  Y := 0;

  if Orientation = trHorizontal then begin
    I := (ThumbR.Right - ThumbR.Left) div 2;
    PosArray[0] := ChannelR.Left + I;
    PosArray[Count - 1] := ChannelR.Right - I - 1;
    case TickMarks of
      tmxBottomRight:
        begin
          Y := ThumbR.Bottom + 1;
          FirstTickSize := 4;
          TickSize := 3;
        end;
      tmxTopLeft:
        begin
          Y := ThumbR.Top - 2;
          FirstTickSize := -4;
          TickSize := -3;
        end;
      tmxBoth:
        begin
          Y := ThumbR.Top - 2;
          FirstTickSize := -4;
          TickSize := -3;
        end;
      tmxCenter:
        begin
          Y := ChannelR.Top + (ChannelR.Bottom - ChannelR.Top) div 2;
          FirstTickSize := 1;
          TickSize := 1;
        end;
    end;
    for I := 0 to Count - 1 do
      if DoDrawChannelTicks(ACanvas, PosArray[I], Y) then begin
        if (I = 0) or (I = Count - 1) then TickDelta := FirstTickSize
        else TickDelta := TickSize;
        ACanvas.MoveTo(PosArray[I], Y);
        ACanvas.LineTo(PosArray[I], Y + TickDelta);
        if TickMarks = tmxBoth then begin
          ACanvas.MoveTo(PosArray[I], ThumbR.Bottom + 1);
          ACanvas.LineTo(PosArray[I], ThumbR.Bottom + 1 - TickDelta);
        end;
      end;
  end
  else begin
    I := (ThumbR.Bottom - ThumbR.Top) div 2;
    PosArray[0] := ChannelR.Top + I;
    PosArray[Count - 1] := ChannelR.Bottom - I - 1;
    case TickMarks of
      tmxBottomRight:
        begin
          Y := ThumbR.Right + 1;
          FirstTickSize := 4;
          TickSize := 3;
        end;
      tmxTopLeft:
        begin
          Y := ThumbR.Left - 2;
          FirstTickSize := -4;
          TickSize := -3;
        end;
      tmxBoth:
        begin
          Y := ThumbR.Left - 2;
          FirstTickSize := -4;
          TickSize := -3;
        end;
      tmxCenter:
        begin
          Y := ChannelR.Left + (ChannelR.Right - ChannelR.Left) div 2;
          FirstTickSize := 1;
          TickSize := 1;
        end;
    end;
    for I := 0 to Count - 1 do
      if DoDrawChannelTicks(ACanvas, Y, PosArray[I]) then begin
        if (I = 0) or (I = Count - 1) then TickDelta := FirstTickSize
        else TickDelta := TickSize;
        ACanvas.MoveTo(Y, PosArray[I]);
        ACanvas.LineTo(Y + TickDelta, PosArray[I]);
        if TickMarks = tmxBoth then begin
          ACanvas.MoveTo(ThumbR.Right + 1, PosArray[I]);
          ACanvas.LineTo(ThumbR.Right + 1 - TickDelta, PosArray[I]);
        end;
      end;
  end;

  ACanvas.Pen.Color := LastPenColor;
end;

function TSpTBXTrackBar.MouseInThumb: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then
    Result := False
  else begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
    GetCursorPos(P);
    P := ScreenToClient(P);
    Result := PtInRect(R, P)
  end;

  if SpTBXSkinType(SkinType) = sknWindows then begin
    if Focused then Result := not (GetCaptureControl = Self);
  end
  else
    Result := GetCaptureControl = Self;
end;

procedure TSpTBXTrackBar.InvalidateBackground;
begin
  // Invalidate, Repaint, Update, SetWindowPos and RedrawWindow doesn't work
  // on Trackbars (CN_NOTIFY messages are not sent), we have to send a
  // WM_SIZE message in order to invalidate the control.
  if HandleAllocated then
    SendMessage(Handle, WM_SIZE, SIZE_RESTORED, MakeLParam(Width, Height));
end;

procedure TSpTBXTrackBar.SetSkinType(const Value: TSpTBXSkinType);
begin
  if Value <> FSkinType then begin
    FSkinType := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXTrackBar.SetTickMarks(const Value: TSpTBXTickMark);
const
  A: array [TSpTBXTickMark] of TTickMark = (tmBottomRight, tmTopLeft, tmBoth, tmBoth);
begin
  if Value <> FTickMarks then begin
    if A[FTickMarks] = A[Value] then begin
      FTickMarks := Value;
      inherited TickMarks := A[Value];
      RecreateWnd;
    end
    else begin
      FTickMarks := Value;
      inherited TickMarks := A[Value];
    end;
  end;
end;

procedure TSpTBXTrackBar.CMSpTBXControlsInvalidate(var Message: TMessage);
begin
  InvalidateBackground;
  Message.Result := 1;
end;

procedure TSpTBXTrackBar.CNNotify(var Message: TWMNotify);
var
  Info: PNMCustomDraw;
  ACanvas: TCanvas;
  R: TRect;
  Rgn: HRGN;
  Offset: Integer;
begin
  if Message.NMHdr.code = NM_CUSTOMDRAW then begin
    Message.Result := CDRF_DODEFAULT;
    Info := Pointer(Message.NMHdr);
    case Info.dwDrawStage of
      CDDS_PREPAINT:
        Message.Result := CDRF_NOTIFYITEMDRAW;
      CDDS_ITEMPREPAINT:
        begin
          ACanvas := TCanvas.Create;
          ACanvas.Lock;
          try
            ACanvas.Handle := Info.hdc;
            case Info.dwItemSpec of
              TBCD_TICS:
                begin
                  R := ClientRect;
                  SpDrawParentBackground(Self, ACanvas.Handle, R);
                  if Focused then
                    SpDrawFocusRect(ACanvas, R);
                  if FTickMarks <> tmxCenter then
                    DrawTicks(ACanvas);
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
              TBCD_THUMB:
                begin
                  if SliderVisible then begin
                    SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
                    if DoDrawThumb(ACanvas, R, pstPrePaint) then
                      SpDrawXPTrackBar(ACanvas, R, TBCD_THUMB, Orientation = trVertical, MouseInThumb, False, FTickMarks, Min, Max, SelStart, SelEnd, FSkinType);
                    DoDrawThumb(ACanvas, R, pstPostPaint);
                    Message.Result := CDRF_SKIPDEFAULT;
                  end;
                end;
              TBCD_CHANNEL:
                begin
                  SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
                  Offset := 0;
                  if Focused then
                    Inc(Offset);
                  if Orientation = trHorizontal then begin
                    R.Left := ClientRect.Left + Offset;
                    R.Right := ClientRect.Right - Offset;
                  end
                  else begin
                    R.Top := ClientRect.Top + Offset;
                    R.Bottom := ClientRect.Bottom - Offset;
                  end;
                  with R do
                    Rgn := CreateRectRgn(Left, Top, Right, Bottom);
                  SelectClipRgn(ACanvas.Handle, Rgn);
                  try
                    SpDrawParentBackground(Self, ACanvas.Handle, ClientRect);
                    R := ChannelRect;

                    if DoDrawChannel(ACanvas, R, pstPrePaint) then
                      SpDrawXPTrackBar(ACanvas, R, TBCD_CHANNEL, Orientation = trVertical, False, FCanDrawChannelSelection, FTickMarks, Min, Max, SelStart, SelEnd, FSkinType);
                    DoDrawChannel(ACanvas, R, pstPostPaint);

                    // Draw channel tics
                    if FTickMarks = tmxCenter then
                      DrawTicks(ACanvas);
                  finally
                    DeleteObject(Rgn);
                    SelectClipRgn(ACanvas.Handle, 0);
                  end;
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
            end;
          finally
            ACanvas.Unlock;
            ACanvas.Handle := 0;
            ACanvas.Free;
          end;
        end;
    end;
  end;
end;

procedure TSpTBXTrackBar.WMEraseBkGnd(var Message: TMessage);
begin
  if SpTBXSkinType(SkinType) <> sknNone then
    Message.Result := 1
  else
    inherited;
end;

procedure TSpTBXTrackBar.WMSpSkinChange(var Message: TMessage);
begin
  InvalidateBackground;
end;

end.
