{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDEControl;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdActns, ActnList, Menus, SCDECommon, SCDEConsts;

type
  TSCDePictureList = class;
  TSCDePictureNotifier = class;
  
  TSCDePictureListItem = class(TCollectionItem)
  private
    FPicture: TPicture;
    FCaption: String;
    FDescription: TStrings;
    FName: String;
    FData: TObject;
    FUpdateCount: Integer;
    procedure SetPicture(Value: TPicture);
    procedure SetCaption(const Value: String);
    procedure SetDescription(Value: TStrings);
    procedure SetName(const Value: String);

    procedure DescriptionChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
  protected
    function  GetDisplayName: string; override;
    procedure DoChanged;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Data: TObject read FData write FData;
  published
    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate: Boolean;

    property Picture: TPicture read FPicture write SetPicture;
    property Caption: String read FCaption write SetCaption;
    property Description: TStrings read FDescription write SetDescription;
    property Name: String read FName write SetName;
  end;

  TSCDePictureListItems = class(TCollection)
  private
    FOwner: TSCDePictureList;
    function  GetItem(Index: Integer): TSCDePictureListItem;
    procedure SetItem(Index: Integer; Value: TSCDePictureListItem);
  protected
    function  GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TSCDePictureList); virtual;
    function Add: TSCDePictureListItem;
    {$IFDEF SCDE_DELPHI5_AND_EARLY}
    property Owner: TSCDePictureList read FOwner;
    {$ENDIF}
    property Items[Index: Integer]: TSCDePictureListItem read GetItem write SetItem; default;
  end;

  TSCDePictureChangeAction = (scpcaChanged, scpcaDestroyed);

  TSCDePictureChangedEvent = procedure(Sender: TSCDePictureList; Action: TSCDePictureChangeAction) of object;

  TSCDePictureNotifier = class(TObject)
  private
    FOnChange: TSCDePictureChangedEvent;
  protected
    procedure DoChange(Sender: TSCDePictureList; Action: TSCDePictureChangeAction); dynamic;
  public
    property OnChange: TSCDePictureChangedEvent read FOnChange write FOnChange;
  end;

  TSCDePictureList = class(TComponent)
  private
    FNotifyList: TList;
    FPictures: TSCDePictureListItems;
    FUpdateCount: Integer;
    procedure SetPictures(Value: TSCDePictureListItems);
    procedure ItemsChanged(Action: TSCDePictureChangeAction);
    function  GetCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure RegisterNotifier(Sender: TSCDePictureNotifier);
    procedure UnregisterNotifier(Sender: TSCDePictureNotifier);

    procedure NotifyAll;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate: Boolean;

    function  GetPicture(Index: Integer): TPicture; overload;
    function  GetPicture(AName: String): TPicture; overload;

    property Count: Integer read GetCount;
  published
    property Pictures: TSCDePictureListItems read FPictures write SetPictures;
  end;

  TSCDeScrollbarDrawKind = (scsdkHorizontal, scsdkVertical, scsdkAll);
  TSCDeOrientation = (scoHorizontal, scoVertical);

  TSCDeCustomControl = class;
  TSCDeCustomScrollControl = class;
  TSCDeCustomControlScrollbar = class;

  TSCDeCollectionItem = class(TCollectionItem);
  TSCDeCollectionActionLink = class(TActionLink);

  TSCDeCollectionActionLinkClass = class of TSCDeCollectionActionLink;

  TSCDeControlActionLink = class(TControlActionLink)
  protected
    FClient: TSCDeCustomControl;
    procedure AssignClient(AClient: TObject); override;
    function  IsCaptionLinked: Boolean; override;
    function  IsEnabledLinked: Boolean; override;
    function  IsHintLinked: Boolean; override;
    function  IsImageIndexLinked: Boolean; override;
    function  IsShortCutLinked: Boolean; override;
    function  IsVisibleLinked: Boolean; override;
    function  IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

{ TSCDeControlActionLink }

  TSCDeControlActionLinkClass = class of TSCDeControlActionLink;

  TSCDeImageLayout = (scilBottom, scilLeft, scilRight, scilTop);

{ TSCDeCustomControl }

  TSCDeControlBorderProps = class(TPersistent)
  private
    FOwner: TSCDeCustomControl;
    function  GetBorder: TSCDeControlBorder;
    procedure SetBorder(Value: TSCDeControlBorder);
    function  GetBorderColor: TColor;
    procedure SetBorderColor(Value: TColor);
    function  GetBorderEx: Boolean;
    procedure SetBorderEx(Value: Boolean);
    function  GetBorderInner: TSCDeControlBorder;
    procedure SetBorderInner(Value: TSCDeControlBorder);
    function  GetBorderWidth: TBorderWidth;
    procedure SetBorderWidth(Value: TBorderWidth);
    function  GetFlatColor: TColor;
    procedure SetFlatColor(Value: TColor);
    function  GetFlatInnerColor: TColor;
    procedure SetFlatInnerColor(Value: TColor);
  protected
    function GetOwner: TPersistent; override;
    property FlatInnerColor: TColor read GetFlatInnerColor write SetFlatInnerColor default clBtnShadow;
    property InnerBorder: TSCDeControlBorder read GetBorderInner write SetBorderInner default scdcbNone;
  public
    constructor Create(AOwner: TSCDeCustomControl); virtual;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeCustomControl read FOwner;
    property Border: TSCDeControlBorder read GetBorder write SetBorder default scdcbNone;
    property Color: TColor read GetBorderColor write SetBorderColor default clBtnFace;
    property ExDraw: Boolean read GetBorderEx write SetBorderEx default False;
    property FlatColor: TColor read GetFlatColor write SetFlatColor default clBtnShadow;
    property Width: TBorderWidth read GetBorderWidth write SetBorderWidth default 0;
  end;

  TSCDeControlBorderPropsClass = class of TSCDeControlBorderProps;

  TSCDeBorderProps = class(TSCDeControlBorderProps)
  published
    property Border;
    property Color;
    property ExDraw;
    property FlatColor;
    property FlatInnerColor;
    property InnerBorder;
    property Width;
  end;

  TSCDeCustomStyleProps = class;
  TSCDeStyleBorderProps = class;

  TSCDeCustomStyleController = class(TComponent)
  private
    FControls: TList;
    FBorderProps: TSCDeStyleBorderProps;
    procedure SetBorderProps(Value: TSCDeStyleBorderProps);
  protected
    procedure Changed(Props: TSCDeCustomStyleProps);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterChanges(Control: TSCDeCustomControl);
    procedure UnregisterChanges(Control: TSCDeCustomControl);
  published
    property BorderProps: TSCDeStyleBorderProps read FBorderProps write SetBorderProps;
  end;

  TSCDeCustomStyleProps = class(TPersistent)
  private
    FOwner: TSCDeCustomStyleController;
    FUpdateCount: Integer;
  protected
    function  GetOwner: TPersistent; override;
    procedure Changed;
  public
    constructor Create(AOwner: TSCDeCustomStyleController); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    property  UpdateCount: Integer read FUpdateCount;

    property Owner: TSCDeCustomStyleController read FOwner;
  end;

  TSCDeStyleBorderProps = class(TSCDeCustomStyleProps)
  private
    FBorder: TSCDeControlBorder;
    FBorderColor: TColor;
    FBorderEx: Boolean;
    FBorderInner: TSCDeControlBorder;
    FBorderWidth: TBorderWidth;
    FFlatColor: TColor;
    FFlatInnerColor: TColor;
    procedure SetBorder(Value: TSCDeControlBorder);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderEx(Value: Boolean);
    procedure SetBorderInner(Value: TSCDeControlBorder);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetFlatColor(Value: TColor);
    procedure SetFlatInnerColor(Value: TColor);
  public
    constructor Create(AOwner: TSCDeCustomStyleController); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Border: TSCDeControlBorder read FBorder write SetBorder default scdcbNone;
    property Color: TColor read FBorderColor write SetBorderColor default clBtnFace;
    property ExDraw: Boolean read FBorderEx write SetBorderEx default False;
    property FlatColor: TColor read FFlatColor write SetFlatColor default clBtnShadow;
    property FlatInnerColor: TColor read FFlatInnerColor write SetFlatInnerColor default clBtnShadow;
    property InnerBorder: TSCDeControlBorder read FBorderInner write SetBorderInner default scdcbNone;
    property Width: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
  end;

{$IFDEF SCDE_DELPHI4_AND_EARLY}
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint; var Handled: Boolean) of object;
{$ENDIF}

  TSCDeNotificationEvent = procedure (Sender: TObject; AComponent: TComponent;
    Operation: TOperation) of object;

  TSCDeNotifier = class(TObject)
  private
    FOnNotification: TSCDeNotificationEvent;
  protected
    procedure Notification(Sender: TObject; AComponent: TComponent;
      Operation: TOperation);
  published
    property OnNotification: TSCDeNotificationEvent read FOnNotification write FOnNotification;
  end;

  TSCDeCustomPictureProps = class(TPersistent)
  private
    FOwner: TSCDeCustomControl;
    function  GetIndent: Integer;
    procedure SetIndent(Value: Integer);
    function  GetOrient: TSCDePictureOrient;
    procedure SetOrient(Value: TSCDePictureOrient);
    function  GetPictureIndex: Integer;
    procedure SetPictureIndex(Value: Integer);
    function  GetPictureList: TSCDePictureList;
    procedure SetPictureList(Value: TSCDePictureList);
    function  GetTopIndent: Integer;
    procedure SetTopIndent(Value: Integer);
    function  GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    function  GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
  protected
    function GetOwner: TPersistent; override;

    property Indent: Integer read GetIndent write SetIndent default 0;
    property Orient: TSCDePictureOrient read GetOrient write SetOrient default scdpoTiled;
    property PictureIndex: Integer read GetPictureIndex write SetPictureIndex default -1;
    property PictureList: TSCDePictureList read GetPictureList write SetPictureList;
    property TopIndent: Integer read GetTopIndent write SetTopIndent default 0;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Visible: Boolean read GetVisible write SetVisible default True;
  public
    constructor Create(AOwner: TSCDeCustomControl); virtual;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeCustomControl read FOwner;
  end;

  TSCDeCustomPicturePropsClass = class of TSCDeCustomPictureProps;

  TSCDePictureProps = class(TSCDeCustomPictureProps)
  published
    property Indent;
    property Orient;
    property PictureIndex;
    property PictureList;
    property TopIndent;
    property Transparent;
    property Visible;
  end;

  TSCDeCustomControl = class(TCustomControl)
  private
    FActive: Boolean;
    FAutoSize: Boolean;
    FBlendColor: Boolean;
    FBorder: TSCDeControlBorder;
    FBorderColor: TColor;
    FBorderEx: Boolean;
    FBorderInner: TSCDeControlBorder;
    FBorderProps: TSCDeControlBorderProps;
    FClickFocus: Boolean;
    FFlatColor: TColor;
    FFlatInnerColor: TColor;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FImageLayout: TSCDeImageLayout;
    FImageChangeLink: TChangeLink;
    FIndent: Integer;
    FPicture: TPicture;
    FPictureProps: TSCDeCustomPictureProps;
    FPictureOrient: TSCDePictureOrient;
    FPictureIndent: Integer;
    FPictureTopIndent: Integer;
    FPictureTransparent: Boolean;
    FPictureIndex: Integer;
    FPictureList: TSCDePictureList;
    FPictureNotifier: TSCDePictureNotifier;
    FSpacing: Integer;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    FMouseInControl: Boolean;
    FMouseIsDown: Boolean;
    FMouseDownPoint: TPoint;
    FCreatingControl: Boolean;
    FShowPicture: Boolean;
    FDrawingPicture: Boolean;
    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPictureChange: TNotifyEvent;
    FHotNCArea: LongInt;
    FNCIsDown: Boolean;
    FHasFocus: Boolean;
    FInChange: Integer;
    FNotificationList: TList;
    FStyleController: TSCDeCustomStyleController;
    {$IFDEF SCDE_DELPHI4_AND_EARLY}
    FOnContextPopup: TContextPopupEvent;
    {$ENDIF}
    FOnFocusChange: TNotifyEvent;
    function  GetAbout: TSCDeAboutString;
    procedure SetAbout(Value: TSCDeAboutString);
    procedure SetBorder(Value: TSCDeControlBorder);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderEx(Value: Boolean);
    procedure SetBorderInner(Value: TSCDeControlBorder);
    function  GetBorderProps: TSCDeControlBorderProps;
    procedure SetBorderProps(Value: TSCDeControlBorderProps);
    procedure SetBlendColor(Value: Boolean);
    procedure SetFlatColor(Value: TColor);
    procedure SetFlatInnerColor(Value: TColor);
    procedure SetImageLayout(Value: TSCDeImageLayout);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure SetPicture(Value: TPicture);
    procedure SetPictureProps(Value: TSCDeCustomPictureProps);
    procedure SetPictureOrient(Value: TSCDePictureOrient);
    procedure SetPictureIndent(Value: Integer);
    procedure SetPictureTopIndent(Value: Integer);
    procedure SetPictureTransparent(Value: Boolean);
    procedure SetPictureIndex(Value: Integer);
    procedure SetPictureList(Value: TSCDePictureList);
    procedure SetShowPicture(Value: Boolean);
    procedure SetStyleController(Value: TSCDeCustomStyleController);
    procedure SetTransparent(Value: Boolean);
    
    procedure TranslateMouseDown(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure TranslateMouseMove(var Message: TWMMouse);
    procedure TranslateMouseUp(var Message: TWMMouse; Button: TMouseButton);

    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMJumpToNext(var Message: TMessage); message CM_SCDEJUMPTONEXT;
    procedure CMIsVisibleChild(var Message: TMessage); message CM_SCDEISVISIBLECHILD;
    procedure CNChar(var Message: TWMChar); message CN_CHAR;

    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSettingChange(var Message: TWMSettingChange); message WM_SETTINGCHANGE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TMessage); message WM_WINDOWPOSCHANGED;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCRButtonDown(var Message: TWMNCHitMessage); message WM_NCRBUTTONDOWN;
    procedure WMNCMButtonDown(var Message: TWMNCHitMessage); message WM_NCMBUTTONDOWN;
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMNCRButtonDblClk(var Message: TWMNCHitMessage); message WM_NCRBUTTONDBLCLK;
    procedure WMNCMButtonDblClk(var Message: TWMNCHitMessage); message WM_NCMBUTTONDBLCLK;
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage); message WM_NCLBUTTONUP;
    procedure WMNCRButtonUp(var Message: TWMNCHitMessage); message WM_NCRBUTTONUP;
    procedure WMNCMButtonUp(var Message: TWMNCHitMessage); message WM_NCMBUTTONUP;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    procedure DoBorderChanged;
    procedure DoFocusChanged;

    procedure DoFakeMouseDown(Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure DoFakeMouseMove(Message: TWMMouse);
    procedure DoFakeMouseUp(Message: TWMMouse; Button: TMouseButton);

    procedure DoNCMouseDown(var Message: TWMNCHitMessage; Button: TMouseButton; Shift: TShiftState);
    procedure DoNCMouseUp(var Message: TWMNCHitMessage; Button: TMouseButton);

    procedure PictureListChanged(Sender: TSCDePictureList; Action: TSCDePictureChangeAction);
    procedure PictureChanged(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyAll(AComponent: TComponent; Operation: TOperation);

    function  GetClientRect: TRect; override;

    function  IsVisibleChild(C: TControl): Boolean; dynamic;
    function  CanDoNextControl(CharCode: Word; Shift: TShiftState): Boolean;
    function  DoNextControl(CharCode: Word; Shift: TShiftState): Boolean;
    function  CanMenuPopup(const Pos: TSmallPoint): Boolean;

    procedure BeforeEnter; virtual;
    procedure AfterEnter; virtual;
    procedure BeforeExit; virtual;
    procedure AfterExit; virtual;

    function  GetImages: TCustomImageList; virtual;
    function  GetImageIndex: TImageIndex; virtual;

    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    function  CanSetBorder(Value: TSCDeControlBorder): Boolean; dynamic;
    function  CanSetInnerBorder(Value: TSCDeControlBorder): Boolean; dynamic;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

    function  CanUpdateStyle(Props: TSCDeCustomStyleProps): Boolean; dynamic;
    procedure AssignProps(Props: TSCDeCustomStyleProps); dynamic;

    procedure NotifyStyleChange; overload;
    procedure NotifyStyleChange(Props: TSCDeCustomStyleProps); overload;

    procedure DoPictureListChanged; dynamic;
    procedure DoPictureChanged; dynamic;

    function  HasPicture: Boolean; overload; 
    function  HasPicture(P: TPicture): Boolean; overload; virtual;
    function  GetPicture: TPicture; virtual;
    function  GetPictureRect: TRect; overload;
    function  GetPictureRect(P: TPicture): TRect; overload; virtual;
    function  CanDrawPicture: Boolean; overload;
    function  CanDrawPicture(P: TPicture): Boolean; overload; virtual;
    procedure DrawPicture(C: TCanvas); virtual;

    function  GetBorderPropsClass: TSCDeControlBorderPropsClass; dynamic;
    function  GetPicturePropsClass: TSCDeCustomPicturePropsClass; dynamic;

    procedure Change; dynamic;
    procedure DoChange; dynamic;
    procedure FocusChanged; dynamic;
    procedure TransparentChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure SystemColorsChanged; dynamic;

    procedure ScrollerDestroyed(Sender: TSCDeCustomControlScrollbar); dynamic;
    procedure ScrollerChanged(Sender: TSCDeCustomControlScrollbar); dynamic;
    procedure ScrollerPositionChanged(Sender: TSCDeCustomControlScrollbar); overload; dynamic;
    procedure ScrollerPositionChanged(Sender: TSCDeCustomControlScrollbar; OldPos, NewPos: Integer); overload; dynamic;
    procedure ScrollerPositionChanging(Sender: TSCDeCustomControlScrollbar;
      var ScrollPos: Integer; var CanScroll: Boolean); dynamic;
    procedure ScrollerRangeChanged(Sender: TSCDeCustomControlScrollbar); dynamic;
    procedure ScrollerSizeChanged(Sender: TSCDeCustomControlScrollbar); dynamic;
    procedure ScrollerVisibleChanged(Sender: TSCDeCustomControlScrollbar); dynamic;

    function  GetHorzScrollbarRect: TRect; dynamic;
    function  GetVertScrollbarRect: TRect; dynamic;

    function  GetNCAreaAtPos(P: TPoint): LongInt;
    function  GetInheritedNCArea(P: TPoint): LongInt; dynamic;
    function  CanCaptureMouseOnNC(P: TPoint): Boolean; dynamic;

    procedure NCMouseLeave; dynamic;
    procedure NCMouseEnter(var HitTest: LongInt; X, Y: Integer); dynamic;
    procedure NCMouseDown(Button: TMouseButton; HitTest: LongInt; DblClk: Boolean; X, Y: Integer); dynamic;
    procedure NCMouseMove(HitTest: LongInt; X, Y: Integer); dynamic;
    procedure NCMouseUp(Button: TMouseButton; HitTest: LongInt; X, Y: Integer); dynamic;

    function  CanGetFocus: Boolean; virtual;

    function  GetBorderSize: Integer; virtual;
    function  GetInnerBorderSize: Integer; virtual;
    procedure CalculateBorder(var R: TRect); virtual;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure UpdateTracking;

    procedure DoFillScrollBarsCorner(OnDC: HDC = 0); virtual;
    procedure RedrawBorder(const Clip: HRGN; OnDC: HDC = 0); virtual;
    procedure RedrawScrollbars(Kind: TSCDeScrollbarDrawKind = scsdkAll; OnDC: HDC = 0); virtual;

    procedure BorderChanged; virtual;
    function  GetBorderExColor: TColor; dynamic;
    procedure PaintParentOn(C: TCanvas); virtual;

    procedure AutoSizeChanged; dynamic;
    procedure DoAutoSize(Value: Boolean);
    procedure SetAutoSize(Value: Boolean); {$IFDEF SCDE_DELPHI6_UP} override; {$ELSE} virtual; {$ENDIF}
    procedure AdjustBounds; dynamic;
    function  GetAdjustedRect(NewWidth, NewHeight: Integer): TRect; virtual;

    procedure SetIndent(Value: Integer); virtual;
    procedure IndentChanged; dynamic;

    procedure SetSpacing(Value: Integer); virtual;
    procedure SpacingChanged; dynamic;

    procedure CaptureChanged(Captured: Boolean); virtual;
    procedure StopTracking; virtual;
    procedure MouseInControlChanged; virtual;
    function  IsInClientRect(X, Y: Integer): Boolean; dynamic;

    function  IsActive: Boolean; virtual;
    procedure ImageListChange(Sender: TObject); dynamic;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function  GetActionLinkClass: TControlActionLinkClass; override;

    function  GetFaceColor: TColor; virtual;
    function  GetIndent: Integer; virtual;
    function  GetImageRect: TRect; virtual;
    function  GetTextRect: TRect; virtual;
    function  GetTextCalculateFont: TFont; virtual;
    function  GetTextHeight: Integer; virtual;
    function  CalculateImageRect: TRect; virtual;
    function  CalculateTextRect: TRect; virtual;
    function  GetBlendValue: Word; dynamic;
    function  CurrentBlendedColor(AColor: TColor): TColor; dynamic;
    function  DefaultBlendedColor(AColor: TColor): TColor; dynamic;
    procedure UpdateLocked; dynamic;
    procedure UpdateUnlocked; dynamic;

    function  IsVertScrollBarVisible: Boolean; dynamic;
    function  IsHorzScrollBarVisible: Boolean; dynamic;

    property HotNCArea: LongInt read FHotNCArea;
    property NCIsDown: Boolean read FNCIsDown;

    property Active: Boolean read FActive write FActive default False;
    {$IFDEF SCDE_DELPHI4_UP}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$ELSE}
    property AutoSize default False;
    {$ENDIF}
    property BlendColor: Boolean read FBlendColor write SetBlendColor default False;
    property Border: TSCDeControlBorder read FBorder write SetBorder default scdcbNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnFace;
    property BorderEx: Boolean read FBorderEx write SetBorderEx default False;
    property BorderInner: TSCDeControlBorder read FBorderInner write SetBorderInner default scdcbNone;
    property BorderProps: TSCDeControlBorderProps read GetBorderProps write SetBorderProps;
    property Caption;
    property Color nodefault;
    property FlatColor: TColor read FFlatColor write SetFlatColor default clBtnShadow;
    property FlatInnerColor: TColor read FFlatInnerColor write SetFlatInnerColor default clBtnShadow;
    property Font;
    property MouseInControl: Boolean read FMouseInControl write FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown write FMouseIsDown;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageLayout: TSCDeImageLayout read FImageLayout write SetImageLayout default scilLeft;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 0;
    property MouseDownPoint: TPoint read FMouseDownPoint write FMouseDownPoint;
    property Picture: TPicture read FPicture write SetPicture;
    property PictureProps: TSCDeCustomPictureProps read FPictureProps write SetPictureProps;
    property PictureOrient: TSCDePictureOrient read FPictureOrient write SetPictureOrient default scdpoTiled;
    property PictureIndent: Integer read FPictureIndent write SetPictureIndent default 0;
    property PictureTopIndent: Integer read FPictureTopIndent write SetPictureTopIndent default 0;
    property PictureTransparent: Boolean read FPictureTransparent write SetPictureTransparent default False;
    property PictureIndex: Integer read FPictureIndex write SetPictureIndex default -1;
    property PictureList: TSCDePictureList read FPictureList write SetPictureList;
    property ShowPicture: Boolean read FShowPicture write SetShowPicture default True;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property StyleController: TSCDeCustomStyleController read FStyleController write SetStyleController;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Height;
    property Width;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$IFDEF SCDE_DELPHI4_AND_EARLY}
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    {$ENDIF}
    property OnFocusChange: TNotifyEvent read FOnFocusChange write FOnFocusChange;
    property OnPictureChange: TNotifyEvent read FOnPictureChange write FOnPictureChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function  CanGetClientRect: Boolean;
    function  ScreenToNC(P: TPoint): TPoint; virtual;
    function  IsValidImage(Indx: Integer): Boolean; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate: Boolean;

    procedure RegisterNotifier(ANotifier: TSCDeNotifier);
    procedure UnregisterNotifier(ANotifier: TSCDeNotifier);

    procedure SaveToFile(const FileName: String); dynamic;
    procedure LoadFromFile(const FileName: String); dynamic;

    property ClickFocus: Boolean read FClickFocus write FClickFocus default True;
    property HasFocus: Boolean read FHasFocus;
  published
    property About: TSCDeAboutString read GetAbout write SetAbout stored False;
  end;

  TSCDeCustomSizableControl = class(TSCDeCustomControl)
  private
    FShowSizeGrip: Boolean;
    FShowStatusbar: Boolean;
    FStatusbarColor: TColor;
    FStatusbarText: TCaption;
    FStatusbarAlignment: TAlignment;
    procedure SetShowSizeGrip(Value: Boolean);
    procedure SetShowStatusbar(Value: Boolean);
    procedure SetStatusbarAlignment(Value: TAlignment);
    procedure SetStatusbarColor(Value: TColor);
    procedure SetStatusbarText(const Value: TCaption);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    function  GetStatusbarHeight: Integer;
    procedure CalculateBorder(var R: TRect); override;
    procedure RedrawBorder(const Clip: HRGN; OnDC: HDC = 0); override;

    procedure StatusbarChanged; dynamic;

    function  GetStatusbarPartAtPos(P: TPoint): LongInt;
    function  GetInheritedNCArea(P: TPoint): LongInt; override;

    property ShowSizeGrip: Boolean read FShowSizeGrip write SetShowSizeGrip default True;
    property ShowStatusbar: Boolean read FShowStatusbar write SetShowStatusbar default False;
    property StatusbarAlignment: TAlignment read FStatusbarAlignment write SetStatusbarAlignment default taLeftJustify;
    property StatusbarColor: TColor read FStatusbarColor write SetStatusbarColor default clBtnFace;
    property StatusbarText: TCaption read FStatusbarText write SetStatusbarText;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  TSCDeScrollbarPart = class(TPersistent)
  private
    FBlend: Boolean;
    FColor: TColor;
    FDisabledColor: TColor;
    FDownColor: TColor;
    FEnabled: Boolean;
    FHotColor: TColor;
    FVisible: Boolean;
    FOwner: TSCDeCustomControlScrollbar;
    procedure SetBlend(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetDisabledColor(Value: TColor);
    procedure SetDownColor(Value: TColor);
    procedure SetEnabled(Value: Boolean);
    procedure SetHotColor(Value: TColor);
    procedure SetVisible(Value: Boolean);

    procedure DoChanged;
  protected
    function GetOwner: TPersistent; override;

    property Blend: Boolean read FBlend write SetBlend default False;
    property Color: TColor read FColor write SetColor default clScrollBar;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clScrollBar;
    property DownColor: TColor read FDownColor write SetDownColor default clScrollBar;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property HotColor: TColor read FHotColor write SetHotColor default clScrollBar;
    property Visible: Boolean read FVisible write SetVisible default True;
  public
    constructor Create(AOwner: TSCDeCustomControlScrollbar); virtual;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeCustomControlScrollbar read FOwner;
  end;

  TSCDeScrollbarBkground = class(TSCDeScrollbarPart)
  public
    constructor Create(AOwner: TSCDeCustomControlScrollbar); override;
  published
    property Blend default True;
    property Color;
    property DisabledColor;
    property HotColor;
    property DownColor default cl3DDkShadow;
  end;

  TSCDeScrollbarButtonColors = class(TSCDeScrollbarPart)
  published
    property Color;
    property DisabledColor;
    property DownColor;
    property HotColor;
  end;

  TSCDeScrollbarButton = class(TSCDeScrollbarPart)
  published
    property Enabled;
    property Visible;
  end;

  TSCDeScrollbarIcons = class(TSCDeScrollbarPart)
  public
    constructor Create(AOwner: TSCDeCustomControlScrollbar); override;
  published
    property Color default clWindowText;
    property DisabledColor default clGrayText;
    property DownColor default clWindowText;
    property HotColor default clWindowText;
  end;

  TSCDeScrollbarThumb = class(TSCDeScrollbarPart)
  public
    property Visible;
  published
    property Color;
    property DisabledColor;
    property DownColor;
    property HotColor;
  end;

  TSCDeScrollbarPartClass = class of TSCDeScrollbarPart;

  TSCDeScrollerPart = (scdsbpBkground, scdsbpIcon, scdsbpButtons, scdsbpExtraButton,
    scdsbpLeftButton, scdsbpRightButton, scdsbpThumb);

  TSCDeCustomControlScrollbar = class(TPersistent)
  private
    FBackground: TSCDeScrollbarPart;
    FBorderColor: TColor;
    FButtonColors: TSCDeScrollbarPart;
    FButtonLayout: TSCDeScrollButtonLayout;
    FButtonExtra: TSCDeScrollbarPart;
    FButtonLeft: TSCDeScrollbarPart;
    FButtonRight: TSCDeScrollbarPart;
    FButtonSize: Integer;
    FEnabled: Boolean;
    FIcons: TSCDeScrollbarPart;
    FKind: TSCDeScrollbarKind;
    FLargeChange: TSCDeScrollbarInc;
    FMax: Integer;
    FMin: Integer;
    FPageSize: Integer;
    FPosition: Integer;
    FSensitivity: Integer;
    FSlideLine: Boolean;
    FSmallChange: TSCDeScrollbarInc;
    FStyle: TSCDeScrollbarStyle;
    FThumb: TSCDeScrollbarPart;
    FThumbLines: TSCDeScrollThumbline;
    FThumbSize: Integer;
    FTrack: Boolean;
    FTrimPageSize: Boolean;
    FVisible: Boolean;
    FOwner: TSCDeCustomControl;
    FUpdateCount: Integer;
    FPosChangeLock: Integer;
    FWasVisible: Boolean;
    FDefH_ButtonSize: Integer;
    FDefV_ButtonSize: Integer;
    FUpdatePos: Integer;
    FLineDiv: Integer;
    FPageDiv: Integer;
    FDelay: Integer;
    FSmooth: Boolean;
    procedure SetBackground(Value: TSCDeScrollbarPart);
    procedure SetBorderColor(Value: TColor);
    procedure SetButtonColors(Value: TSCDeScrollbarPart);
    procedure SetButtonExtra(Value: TSCDeScrollbarPart);
    procedure SetButtonLeft(Value: TSCDeScrollbarPart);
    procedure SetButtonRight(Value: TSCDeScrollbarPart);
    procedure SetButtonSize(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    function  GetExtraButton: Boolean;
    procedure SetExtraButton(Value: Boolean);
    procedure SetIcons(Value: TSCDeScrollbarPart);
    procedure SetLargeChange(Value: TSCDeScrollbarInc);
    procedure SetPageSize(Value: Integer);
    function  GetRange: Integer;
    procedure SetSensitivity(Value: Integer);
    procedure SetSlideLine(Value: Boolean);
    procedure SetSmallChange(Value: TSCDeScrollbarInc);
    procedure SetStyle(Value: TSCDeScrollbarStyle);
    procedure SetThumb(Value: TSCDeScrollbarPart);
    procedure SetThumbLines(Value: TSCDeScrollThumbline);
    procedure SetThumbSize(Value: Integer);
    procedure SetTrack(Value: Boolean);
    procedure SetTrimPageSize(Value: Boolean);
    procedure SetVisible(Value: Boolean);

    function  GetHovered: Boolean;
    function  GetFocused: Boolean;
    function  GetDownPoint: TPoint;
    function  GetHotPart: TSCDeScrollerHitPart;
    function  GetPressedPart: TSCDeScrollerHitPart;
    function  GetThumbMoving: Boolean;

    procedure RearrangeDefaults;

    procedure BufferedPaint(DC: HDC; R: TRect);
    procedure PaintOn(DC: HDC; R: TRect);
    procedure PartChanged(Sender: TSCDeScrollbarPart);
  protected
    function  GetOwner: TPersistent; override;
    procedure Paint(Canvas: TCanvas; R: TRect); virtual;

    procedure SetMax(Value: Integer); virtual;
    procedure SetMin(Value: Integer); virtual;
    procedure SetRange(Value: Integer); virtual;
    procedure SetPosition(Value: Integer); virtual;

    procedure ScrollMessage(var Msg: TWMScroll); virtual;
    function  GetCalcRange: Integer; dynamic;
    function  GetSensitivity: Integer; virtual;

    procedure DoChange;
    procedure DoPositionChanged; overload;
    procedure DoPositionChanged(OldPos, NewPos: Integer); overload;
    procedure DoPositionChanging(var ScrollPos: Integer; var CanScroll: Boolean);
    procedure DoRangeChanged;
    procedure DoSizeChanged;
    procedure DoVisibleChanged;

    function  CanScrollToPos(var NewValue: Integer): Boolean; dynamic;
    function  GetScrollbarPartClass(Part: TSCDeScrollerPart): TSCDeScrollbarPartClass; dynamic;

    function  GetPageUpDownSize: Integer; virtual;

    function  GetBackColor: TColor; virtual;
    function  GetExtraBtnColor: TColor; virtual;
    function  GetExtraBtnIconColor: TColor; virtual;
    function  GetLeftBtnColor: TColor; virtual;
    function  GetLeftBtnIconColor: TColor; virtual;
    function  GetRightBtnColor: TColor; virtual;
    function  GetRightBtnIconColor: TColor; virtual;
    function  GetThumbColor: TColor; virtual;

    function  GetButtonSize(R: TRect): Integer;
    function  GetThumbOffset(R: TRect): Integer;
    function  GetThumbSize(R: TRect): Integer;
    function  GetDefaultThumbSize(R: TRect; Pg: Integer): Integer;
    function  GetPositionPos(R: TRect; P: Integer): Integer;

    function  GetBackRect(R: TRect): TRect;
    function  GetExtraButtonRect(R: TRect): TRect; dynamic;
    function  GetLeftButtonRect(R: TRect): TRect; dynamic;
    function  GetRightButtonRect(R: TRect): TRect; dynamic;
    function  GetThumbRect(R: TRect): TRect; dynamic;

    property Focused: Boolean read GetFocused;
    property Hovered: Boolean read GetHovered;
    property DownPoint: TPoint read GetDownPoint;
    property HotPart: TSCDeScrollerHitPart read GetHotPart;
    property PressedPart: TSCDeScrollerHitPart read GetPressedPart;

    property Background: TSCDeScrollbarPart read FBackground write SetBackground;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowFrame;
    property ButtonColors: TSCDeScrollbarPart read FButtonColors write SetButtonColors;
    property ButtonLayout: TSCDeScrollButtonLayout read FButtonLayout;
    property ButtonExtra: TSCDeScrollbarPart read FButtonExtra write SetButtonExtra;
    property ButtonLeft: TSCDeScrollbarPart read FButtonLeft write SetButtonLeft;
    property ButtonRight: TSCDeScrollbarPart read FButtonRight write SetButtonRight;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ExtraButton: Boolean read GetExtraButton write SetExtraButton default False;
    property Icons: TSCDeScrollbarPart read FIcons write SetIcons;
    property Kind: TSCDeScrollbarKind read FKind;
    property LargeChange: TSCDeScrollbarInc read FLargeChange write SetLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property PageSize: Integer read FPageSize write SetPageSize default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer read GetRange write SetRange stored False default 100; 
    property Sensitivity: Integer read FSensitivity write SetSensitivity default -1;
    property SlideLine: Boolean read FSlideLine write SetSlideLine default False;
    property SmallChange: TSCDeScrollbarInc read FSmallChange write SetSmallChange default 1;
    property Smooth: Boolean read FSmooth write FSmooth default False;
    property Style: TSCDeScrollbarStyle read FStyle write SetStyle default scssDefault;
    property Thumb: TSCDeScrollbarPart read FThumb write SetThumb;
    property ThumbLines: TSCDeScrollThumbline read FThumbLines write SetThumbLines default sctlNone;
    property ThumbSize: Integer read FThumbSize write SetThumbSize default -1;
    property Track: Boolean read FTrack write SetTrack default True;
    property TrimPageSize: Boolean read FTrimPageSize write SetTrimPageSize default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  public
    constructor Create(AOwner: TSCDeCustomControl; AKind: TSCDeScrollbarKind); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Invalidate;
    function  GetClientRect: TRect;
    function  GetMaximumValue: Integer; virtual;

    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate: Boolean;

    procedure LockPosChange;
    procedure UnlockPosChange;
    function  PosChangeLocked: Boolean;

    property Owner: TSCDeCustomControl read FOwner;
  end;

  TSCDeCustomControlScrollbarClass = class of TSCDeCustomControlScrollbar;

  TSCDeControlScrollbar = class(TSCDeCustomControlScrollbar)
  protected
    procedure Paint(Canvas: TCanvas; R: TRect); override;

    procedure DrawBack(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawScrollBack(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawExtraButton(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawLeftButton(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawRightButton(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawThumb(C: TCanvas; InRect: TRect); dynamic;
    procedure DrawThumbLines(C: TCanvas; R: TRect; Cl: TColor); dynamic;
    procedure DrawBorder(C: TCanvas; R: TRect); dynamic;
    procedure DrawXPFace(C: TCanvas; R: TRect; Cl, BkCl: TColor; IsThumb,
      IsDown, IsHot: Boolean); dynamic;
    procedure DrawOffice12Border(C: TCanvas; R: TRect); dynamic;

    property Visible default False;
    property TrimPageSize;
  public
    constructor Create(AOwner: TSCDeCustomControl; AKind: TSCDeScrollbarKind); override;
  end;

  TSCDeScrollInfo = record
    Min: Integer;
    Max: Integer;
    Page: Integer;
    Pos: Integer;
    Range: Integer;
    ButtonSize: Integer;
    ThumbSize: Integer;
    LargeChange: Integer;
    SmallChange: Integer;
    Visible: Boolean;
    Enabled: Boolean;
    Smooth: Boolean;
    LeftButtonEnabled: Boolean;
    LeftButtonVisible: Boolean;
    RightButtonEnabled: Boolean;
    RightButtonVisible: Boolean;
    Tracking: Boolean;
    ThumbEnabled: Boolean;
    ThumbVisible: Boolean;
    TrimPageSize: Boolean;
  end;
  PSCDeScrollInfo = ^TSCDeScrollInfo;

  TSCDeScrollbarHittest = record
    Kind: TSCDeScrollbarHitKind;
    HitPart: TSCDeScrollerHitPart;
    Enabled: Boolean;
    X: Integer;
    Y: Integer;
  end;
  PSCDeScrollbarHittest = ^TSCDeScrollbarHittest;

  TSCDeControlCustomScrollbars = class(TPersistent)
  private
    FSmooth: Boolean;
    FOwner: TSCDeCustomScrollControl;
    function  GetExtraButton: Boolean;
    procedure SetExtraButton(Value: Boolean);
    function  GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function  GetHorizontal: TSCDeCustomControlScrollbar;
    procedure SetHorizontal(Value: TSCDeCustomControlScrollbar);
    function  GetLayout: TSCDeScrollButtonLayout;
    procedure SetLayout(Value: TSCDeScrollButtonLayout);
    procedure SetSmooth(Value: Boolean);
    function  GetStyle: TSCDeScrollbarStyle;
    procedure SetStyle(Value: TSCDeScrollbarStyle);
    function  GetThumbLines: TSCDeScrollThumbline;
    procedure SetThumbLines(Value: TSCDeScrollThumbline);
    function  GetVertical: TSCDeCustomControlScrollbar;
    procedure SetVertical(Value: TSCDeCustomControlScrollbar);
  protected
    function GetOwner: TPersistent; override;

    property ExtraButton: Boolean read GetExtraButton write SetExtraButton default False;
    property Height: Integer read GetHeight write SetHeight default -1;
    property Horizontal: TSCDeCustomControlScrollbar read GetHorizontal write SetHorizontal;
    property Layout: TSCDeScrollButtonLayout read GetLayout write SetLayout default scdsbDefault;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Style: TSCDeScrollbarStyle read GetStyle write SetStyle default scssDefault;
    property ThumbLines: TSCDeScrollThumbline read GetThumbLines write SetThumbLines default sctlNone;
    property Vertical: TSCDeCustomControlScrollbar read GetVertical write SetVertical;
  public
    constructor Create(AOwner: TSCDeCustomScrollControl); virtual;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeCustomScrollControl read FOwner;
  end;

  TSCDeControlCustomScrollbarsClass = class of TSCDeControlCustomScrollbars;

  TSCDeControlScrollbars = class(TSCDeControlCustomScrollbars)
  published
    property ExtraButton;
    property Height;
    property Horizontal;
    property Layout;
    property Style;
    property ThumbLines;
    property Vertical;
  end;

  TSCDeScrollChangeEvent = procedure (Sender: TSCDeCustomScrollControl;
    Kind: TSCDeScrollbarKind) of object;

  TSCDeCustomScrollControl = class(TSCDeCustomSizableControl)
  private
    FScrollbars: TSCDeControlCustomScrollbars;
    FScrollbarHeight: Integer;
    FScrollbarExtraButton: Boolean;
    FScrollbarHorz: TSCDeCustomControlScrollbar;
    FScrollbarVert: TSCDeCustomControlScrollbar;
    FScrollbarStyle: TSCDeScrollbarStyle;
    FScrollButtonsLayout: TSCDeScrollButtonLayout;
    FScrollbarThumbLines: TSCDeScrollThumbline;
    FHotTest: TSCDeScrollbarHittest;
    FDownTest: TSCDeScrollbarHittest;
    FDownPoint: TPoint;
    FMovePoint: TPoint;
    FDownPosition: Integer;
    FMovingThumb: Boolean;
    FScrolling: Boolean;
    FScrollTimer: Integer;
    FKeyboardSpeed: Integer;
    FOnScroll: TSCDeScrollEvent;
    FOnScrollChangeEvent: TSCDeScrollChangeEvent;
    procedure SetScrollbars(Value: TSCDeControlCustomScrollbars);
    procedure SetScrollbarExtraButton(Value: Boolean);
    procedure SetScrollbarHeight(Value: Integer);
    procedure SetScrollbarHorz(Value: TSCDeCustomControlScrollbar);
    procedure SetScrollbarVert(Value: TSCDeCustomControlScrollbar);
    procedure SetScrollbarStyle(Value: TSCDeScrollbarStyle);
    procedure SetScrollbarThumbLines(Value: TSCDeScrollThumbline);
    procedure SetScrollButtonsLayout(Value: TSCDeScrollButtonLayout);

    procedure FillScrollbarBack(DC: HDC; R: TRect);
    procedure FillScrollbarHittest(P: TPoint; Ht: PSCDeScrollbarHittest);
    procedure AssignHittest(FromHt, ToHt: PSCDeScrollbarHittest);

    function  IsScrollingReady: Boolean;
    function  CanScrollBars(AsLeft: Boolean): Boolean;
    function  ScrollingBars: Boolean;
    function  ScrollingPaused: Boolean;
    procedure ResumeScrollingBars;
    procedure StartScrollingBars;
    procedure PauseScrollingBars;
    procedure StopScrollingBars;
    procedure ScrollControl;
    function  IsScrollPart(HitPart: TSCDeScrollerHitPart): Boolean;
    procedure ResetScrollbarInfo(Si: PSCDeScrollInfo);

    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSettingChange(var Message: TMessage);message WM_SETTINGCHANGE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CalculateBorder(var R: TRect); override;
    procedure StopTracking; override;
    procedure EnabledChanged; override;

    procedure ScrollerDestroyed(Sender: TSCDeCustomControlScrollbar); override;
    procedure ScrollerChanged(Sender: TSCDeCustomControlScrollbar); override;
    procedure ScrollerVisibleChanged(Sender: TSCDeCustomControlScrollbar); override;
    procedure ScrollerPositionChanged(Sender: TSCDeCustomControlScrollbar); overload; override;
    procedure ScrollerPositionChanged(Sender: TSCDeCustomControlScrollbar; OldPos, NewPos: Integer); overload; override;
    procedure ScrollerPositionChanging(Sender: TSCDeCustomControlScrollbar;
      var ScrollPos: Integer; var CanScroll: Boolean); override;

    function  IsVertScrollBarVisible: Boolean; override;
    function  IsHorzScrollBarVisible: Boolean; override;

    procedure RedrawScrollbars(Kind: TSCDeScrollbarDrawKind = scsdkAll; OnDC: HDC = 0); override;
    function  CanScrollToPos(Kind: TSCDeScrollbarKind; var NewValue: Integer): Boolean; dynamic;
    procedure DoScrollerPositionChanged(Kind: TSCDeScrollbarKind); overload; dynamic;
    procedure DoScrollerPositionChanged(Kind: TSCDeScrollbarKind; OldPos, NewPos: Integer); overload; dynamic;
    procedure DoScrollerPositionChanging(Kind: TSCDeScrollbarKind; CurPos: Integer;
      var ScrollPos: Integer; var CanScroll: Boolean); dynamic;

    function  GetHorzScrollbarRect: TRect; override;
    function  GetVertScrollbarRect: TRect; override;

    procedure ResetHittest(Ht: PSCDeScrollbarHittest);
    function  SameHittests(Ht1, Ht2: PSCDeScrollbarHittest; CheckPos: Boolean = False): Boolean;
    function  GetScrollbarHittest(P: TPoint): TSCDeScrollbarHittest; dynamic;
    function  GetInheritedNCArea(P: TPoint): LongInt; override;
    function  CanCaptureMouseOnNC(P: TPoint): Boolean; override;

    function  GetDistanceIncrement(Kind: TSCDeScrollbarKind; Dist: Integer): Integer;
    function  GetPositionAtPos(Kind: TSCDeScrollbarKind; X, Y: Integer; IncParts: Boolean): Integer;

    function  GetHorzScrollbarHeight: Integer; dynamic;
    function  GetVertScrollbarWidth: Integer; dynamic;

    function  GetBackgroundColor: TColor; dynamic;
    function  GetControlScrollbarsClass: TSCDeControlCustomScrollbarsClass; dynamic;
    function  GetScrollbarClass: TSCDeCustomControlScrollbarClass; dynamic;

    function  GetScrollbarInfo(Kind: TSCDeScrollbarKind): TSCDeScrollInfo;
    procedure SetScrollbarInfo(Kind: TSCDeScrollbarKind; Info: TSCDeScrollInfo);

    procedure NCMouseLeave; override;
    procedure NCMouseEnter(var HitTest: LongInt; X, Y: Integer); override;
    procedure NCMouseDown(Button: TMouseButton; HitTest: LongInt; DblClk: Boolean; X, Y: Integer); override;
    procedure NCMouseMove(HitTest: LongInt; X, Y: Integer); override;
    procedure NCMouseUp(Button: TMouseButton; HitTest: LongInt; X, Y: Integer); override;

    property HotTest: TSCDeScrollbarHittest read FHotTest;
    property DownTest: TSCDeScrollbarHittest read FDownTest;
    property Scrollbars: TSCDeControlCustomScrollbars read FScrollbars write SetScrollbars;
    property ScrollbarExtraButton: Boolean read FScrollbarExtraButton write SetScrollbarExtraButton default False;
    property ScrollbarHeight: Integer read FScrollbarHeight write SetScrollbarHeight default -1;
    property ScrollbarHorz: TSCDeCustomControlScrollbar read FScrollbarHorz write SetScrollbarHorz;
    property ScrollbarVert: TSCDeCustomControlScrollbar read FScrollbarVert write SetScrollbarVert;
    property ScrollbarStyle: TSCDeScrollbarStyle read FScrollbarStyle write SetScrollbarStyle default scssDefault;
    property ScrollbarThumbLines: TSCDeScrollThumbline read FScrollbarThumbLines write SetScrollbarThumbLines default sctlNone;
    property ScrollButtonsLayout: TSCDeScrollButtonLayout read FScrollButtonsLayout write SetScrollButtonsLayout default scdsbDefault;
    property OnScroll: TSCDeScrollEvent read FOnScroll write FOnScroll;
    property OnScrollChangeEvent: TSCDeScrollChangeEvent read FOnScrollChangeEvent write FOnScrollChangeEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TSCDeGraphicControl = class;

  TSCDeGraphicBorderProps = class(TPersistent)
  private
    FOwner: TSCDeGraphicControl;
    FBorder: TSCDeControlBorder;
    FColor: TColor;
    FExDraw: Boolean;
    FInnerBorder: TSCDeControlBorder;
    FWidth: TBorderWidth;
    FFlatColor: TColor;
    FFlatInnerColor: TColor;
    procedure SetBorder(Value: TSCDeControlBorder);
    procedure SetColor(Value: TColor);
    procedure SetExDraw(Value: Boolean);
    procedure SetInnerBorder(Value: TSCDeControlBorder);
    procedure SetWidth(Value: TBorderWidth);
    procedure SetFlatColor(Value: TColor);
    procedure SetFlatInnerColor(Value: TColor);
    procedure DoChange;
  protected
    function GetOwner: TPersistent; override;

    property Border: TSCDeControlBorder read FBorder write SetBorder default scdcbNone;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property ExDraw: Boolean read FExDraw write SetExDraw default False;
    property FlatColor: TColor read FFlatColor write SetFlatColor default clBtnShadow;
    property FlatInnerColor: TColor read FFlatInnerColor write SetFlatInnerColor default clBtnShadow;
    property InnerBorder: TSCDeControlBorder read FInnerBorder write SetInnerBorder default scdcbNone;
    property Width: TBorderWidth read FWidth write SetWidth default 0;
  public
    constructor Create(AOwner: TSCDeGraphicControl); virtual;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeGraphicControl read FOwner;
  end;

  TSCDeGraphicBorderPropsClass = class of TSCDeGraphicBorderProps;

  TSCDeGraphicControlActionLink = class(TControlActionLink)
  protected
    FClient: TSCDeGraphicControl;
    procedure AssignClient(AClient: TObject); override;
    function  IsCaptionLinked: Boolean; override;
    function  IsEnabledLinked: Boolean; override;
    function  IsHintLinked: Boolean; override;
    function  IsImageIndexLinked: Boolean; override;
    function  IsShortCutLinked: Boolean; override;
    function  IsVisibleLinked: Boolean; override;
    function  IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TSCDeGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    FBorderProps: TSCDeGraphicBorderProps;
    FDoubleBuffered: Boolean;
    FTransparent: Boolean;
    FInChange: Integer;
    FAutoSize: Boolean;
    FIndent: Integer;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FImageLayout: TSCDeImageLayout;
    FImageChangeLink: TChangeLink;
    FPictureIndex: Integer;
    FPictureList: TSCDePictureList;
    FPictureNotifier: TSCDePictureNotifier;
    FSpacing: Integer;
    FUpdateCount: Integer;
    FMouseInControl: Boolean;
    FMouseIsDown: Boolean;
    FMouseDownPoint: TPoint;
    FCreatingControl: Boolean;
    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    function  GetAbout: TSCDeAboutString;
    procedure SetAbout(const Value: TSCDeAboutString);
    procedure SetBorderProps(Value: TSCDeGraphicBorderProps);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageLayout(const Value: TSCDeImageLayout);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPictureIndex(Value: Integer);
    procedure SetPictureList(const Value: TSCDePictureList);

    procedure DrawBorder;
    procedure DoBorderChanged;
    procedure ProcessPaint(DC: HDC);
    procedure PictureListChanged(Sender: TSCDePictureList; Action: TSCDePictureChangeAction);

    procedure TranslateMouseDown(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure TranslateMouseMove(var Message: TWMMouse);
    procedure TranslateMouseUp(var Message: TWMMouse; Button: TMouseButton);

    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMSettingChange(var Message: TWMSettingChange); message WM_SETTINGCHANGE;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
  protected
    procedure Loaded; override;
    procedure Change;

    function  GetClientRect: TRect; override;

    procedure BeforePaint; virtual;
    procedure Paint; virtual;
    procedure AfterPaint; virtual;

    function  IsTransparent: Boolean; virtual;
    procedure PaintParentOn(DC: HDC);

    function  GetImages: TCustomImageList; virtual;
    function  GetImageIndex: TImageIndex; virtual;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure AutoSizeChanged; dynamic;
    procedure DoAutoSize(Value: Boolean);
    procedure SetAutoSize(Value: Boolean); {$IFDEF SCDE_DELPHI6_UP} override; {$ELSE} virtual; {$ENDIF}
    procedure AdjustBounds; dynamic;
    function  GetAdjustedRect(NewWidth, NewHeight: Integer): TRect; virtual;
    procedure AdjustClientRect(var Rect: TRect); virtual;

    procedure DoChange; dynamic;
    procedure EnabledChanged; dynamic;
    procedure BorderChanged; virtual;
    procedure StopTracking; virtual;
    procedure MouseInControlChanged; virtual;
    procedure SystemColorsChanged; dynamic;
    procedure DoPictureListChanged; dynamic;

    procedure UpdateTracking;

    function  CanGetClientRect: Boolean; virtual;
    function  GetBorderPropsClass: TSCDeGraphicBorderPropsClass; dynamic;

    function  GetBorderSize: Integer; virtual;
    function  GetInnerBorderSize: Integer; virtual;
    function  GetBorderExColor: TColor; dynamic;
    procedure CalculateBorder(var R: TRect); virtual;

    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    function  CanSetBorder(Value: TSCDeControlBorder): Boolean; dynamic;
    function  CanSetInnerBorder(Value: TSCDeControlBorder): Boolean; dynamic;

    function  GetTransparent: Boolean; virtual;
    procedure SetTransparent(Value: Boolean); virtual;

    procedure SetIndent(Value: Integer); virtual;
    procedure IndentChanged; dynamic;

    procedure SetSpacing(Value: Integer); virtual;
    procedure SpacingChanged; dynamic;

    procedure ImageListChange(Sender: TObject); dynamic;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function  GetActionLinkClass: TControlActionLinkClass; override;

    function  GetIndent: Integer; virtual;
    function  GetImageRect: TRect; virtual;
    function  GetTextRect: TRect; virtual;
    function  GetTextCalculateFont: TFont; virtual;
    function  GetTextHeight: Integer; virtual;
    function  CalculateImageRect: TRect; virtual;
    function  CalculateTextRect: TRect; virtual;
    procedure UpdateLocked; dynamic;
    procedure UpdateUnlocked; dynamic;

    function IsActive: Boolean; virtual;
    function IsInClientRect(X, Y: Integer): Boolean; virtual;

    property Canvas: TCanvas read FCanvas;
    {$IFDEF SCDE_DELPHI6_UP}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$ELSE}
    property AutoSize default False;
    {$ENDIF}
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property BorderProps: TSCDeGraphicBorderProps read FBorderProps write SetBorderProps;
    property Caption;
    property Color nodefault;
    property Font;
    property MouseInControl: Boolean read FMouseInControl write FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown write FMouseIsDown;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageLayout: TSCDeImageLayout read FImageLayout write SetImageLayout default scilLeft;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 0;
    property MouseDownPoint: TPoint read FMouseDownPoint write FMouseDownPoint;
    property PictureIndex: Integer read FPictureIndex write SetPictureIndex default -1;
    property PictureList: TSCDePictureList read FPictureList write SetPictureList;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    procedure  AfterConstruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function  IsValidImage(Indx: Integer): Boolean; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  InUpdate: Boolean;
  published
    property About: TSCDeAboutString read GetAbout write SetAbout stored False;
  end;

var
  DbCnt: Integer = 0;

implementation

type
  TSCDeFakeControl = class(TControl);
  TSCDeParentControl = class(TWinControl);

const
  SCDE_SCRLBAR_SCROLLTIMERID = 13210;

{ TSCDeControlActionLink }

procedure TSCDeControlActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TSCDeCustomControl;
end;

function TSCDeControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    AnsiSameText(FClient.Caption, (Action as TCustomAction).Caption);
end;

function TSCDeControlActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TSCDeControlActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TSCDeControlActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TSCDeControlActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnClick = @Action.OnExecute);
end;

function TSCDeControlActionLink.IsShortCutLinked: Boolean;
begin
  Result := False;
end;

function TSCDeControlActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TSCDeControlActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TSCDeControlActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TSCDeControlActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TSCDeControlActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TSCDeControlActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;

procedure TSCDeControlActionLink.SetShortCut(Value: TShortCut);
begin
  inherited;
end;

procedure TSCDeControlActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

{ TSCDeNotifier }

procedure TSCDeNotifier.Notification(Sender: TObject; AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FOnNotification) then
    FOnNotification(Sender, AComponent, Operation);
end;

{ TSCDeCustomPictureProps }

procedure TSCDeCustomPictureProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeCustomPictureProps then
  begin
    with TSCDeCustomPictureProps(Source) do
    begin
      Self.Visible := Visible;
      Self.Indent  := Indent;
      Self.PictureList := PictureList;
      Self.PictureIndex := PictureIndex;
      Self.Orient  := Orient;
      Self.TopIndent := TopIndent;
      Self.Transparent := Transparent;
    end;
  end else
    inherited Assign(Source);
end;

constructor TSCDeCustomPictureProps.Create(AOwner: TSCDeCustomControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TSCDeCustomPictureProps.GetIndent: Integer;
begin
  Result := 0;
  if FOwner <> nil then
    Result := FOwner.PictureIndent;
end;

function TSCDeCustomPictureProps.GetOrient: TSCDePictureOrient;
begin
  Result := scdpoTiled;
  if FOwner <> nil then
    Result := FOwner.PictureOrient;
end;

function TSCDeCustomPictureProps.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeCustomPictureProps.GetPictureIndex: Integer;
begin
  Result := -1;
  if FOwner <> nil then
    Result := FOwner.PictureIndex;
end;

function TSCDeCustomPictureProps.GetPictureList: TSCDePictureList;
begin
  Result := nil;
  if FOwner <> nil then
    Result := FOwner.PictureList;
end;

function TSCDeCustomPictureProps.GetTopIndent: Integer;
begin
  Result := 0;
  if FOwner <> nil then
    Result := FOwner.PictureTopIndent;
end;

function TSCDeCustomPictureProps.GetTransparent: Boolean;
begin
  Result := False;
  if FOwner <> nil then
    Result := FOwner.PictureTransparent;
end;

function TSCDeCustomPictureProps.GetVisible: Boolean;
begin
  Result := True;
  if FOwner <> nil then
    Result := FOwner.ShowPicture;
end;

procedure TSCDeCustomPictureProps.SetIndent(Value: Integer);
begin
  if FOwner <> nil then
    FOwner.PictureIndent := Value;
end;

procedure TSCDeCustomPictureProps.SetOrient(Value: TSCDePictureOrient);
begin
  if FOwner <> nil then
    FOwner.PictureOrient := Value;
end;

procedure TSCDeCustomPictureProps.SetPictureIndex(Value: Integer);
begin
  if FOwner <> nil then
    FOwner.PictureIndex := Value;
end;

procedure TSCDeCustomPictureProps.SetPictureList(Value: TSCDePictureList);
begin
  if FOwner <> nil then
    FOwner.PictureList := Value;
end;

procedure TSCDeCustomPictureProps.SetTopIndent(Value: Integer);
begin
  if FOwner <> nil then
    FOwner.PictureTopIndent := Value;
end;

procedure TSCDeCustomPictureProps.SetTransparent(Value: Boolean);
begin
  if FOwner <> nil then
    FOwner.PictureTransparent := Value;
end;

procedure TSCDeCustomPictureProps.SetVisible(Value: Boolean);
begin
  if FOwner <> nil then
    FOwner.ShowPicture := Value;
end;

{ TSCDeCustomControl }

procedure TSCDeCustomControl.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
end;

procedure TSCDeCustomControl.AdjustBounds;
begin
  //
end;

procedure TSCDeCustomControl.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then UpdateLocked;
end;

procedure TSCDeCustomControl.CalculateBorder(var R: TRect);
var
  B: Integer;
begin
  B := GetBorderSize + GetInnerBorderSize;
  InflateRect(R, -B, -B);
end;

function TSCDeCustomControl.CalculateImageRect: TRect;
var
  ARect: TRect;
begin
  Result := GetImageRect;
  if FImages = nil then
    Exit;

  OffsetRect(Result, (ClientWidth - Images.Width) div 2,
    (ClientHeight - Images.Height) div 2);

  ARect := GetTextRect;
  if (ARect.Right <= ARect.Left) or
    (ARect.Bottom <= ARect.Top) then Exit;

  case ImageLayout of
    scilBottom:
      OffsetRect(Result, 0, (((ARect.Bottom - ARect.Top) + 2) div 2));
    scilLeft:
      OffsetRect(Result, -(((ARect.Right - ARect.Left) + 2) div 2) - GetIndent, 0);
    scilRight:
      OffsetRect(Result, (((ARect.Right - ARect.Left) + 2) div 2) + GetIndent, 0);
    scilTop:
      OffsetRect(Result, 0, -(((ARect.Bottom - ARect.Top) + 2) div 2));
  end;
end;

function TSCDeCustomControl.CalculateTextRect: TRect;
var
  ARect: TRect;
  TW, TH: Integer;
begin
  Result := GetTextRect;

  TW := Result.Right - Result.Left;
  TH := Result.Bottom - Result.Top;
  OffsetRect(Result, (ClientWidth - TW) div 2,
    (ClientHeight - TH) div 2);

  if (Images = nil) or (ImageIndex = -1) or
    ((Images <> nil) and (ImageIndex > Images.Count-1)) then Exit;

  ARect := GetImageRect;
  if (ARect.Right <= ARect.Left) or
    (ARect.Bottom <= ARect.Top) then Exit;

  case ImageLayout of
    scilBottom:
      OffsetRect(Result, 0, -(((ARect.Bottom - ARect.Top) + 2) div 2));
    scilLeft:
      OffsetRect(Result, (((ARect.Right - ARect.Left) + 2) div 2) + GetIndent, 0);
    scilRight:
      OffsetRect(Result, -(((ARect.Right - ARect.Left) + 2) div 2) - GetIndent, 0);
    scilTop:
      OffsetRect(Result, 0, (((ARect.Bottom - ARect.Top) + 2) div 2));
  end;
end;

function TSCDeCustomControl.CanGetClientRect: Boolean;
begin
  Result := (Self <> nil) and (HandleAllocated or (Parent <> nil));
end;

function TSCDeCustomControl.CanGetFocus: Boolean;
begin
  Result := True;
end;

procedure TSCDeCustomControl.CaptureChanged(Captured: Boolean);
begin
  //
end;

procedure TSCDeCustomControl.Change;
begin
  Inc(FInChange);
  try
    Changed;
  finally
    Dec(FInChange);
  end;

  DoChange;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then
    FOnChange(Self);
end;

procedure TSCDeCustomControl.CMChanged(var Message: TMessage);
begin
  inherited;
  if FInChange = 0 then
    Change;
end;

procedure TSCDeCustomControl.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeCustomControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  EnabledChanged;

  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);
  FMouseInControl := False;

  if Enabled and not (csDesigning in ComponentState) then
    UpdateTracking;

  MouseInControlChanged;
end;

procedure TSCDeCustomControl.CMFocusChanged(var Message: TCMFocusChanged);
begin
  FActive := Message.Sender = Self;
  StopTracking;
  inherited;
end;

procedure TSCDeCustomControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TSCDeCustomControl.CMMouseEnter(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    inherited;
    MouseInControlChanged;

    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
  end else
    inherited;
end;

procedure TSCDeCustomControl.CMMouseLeave(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := False;
    inherited;
    MouseInControlChanged;

    NCMouseLeave;

    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
  end else
    inherited;  
end;

procedure TSCDeCustomControl.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeCustomControl.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  SystemColorsChanged;
  Invalidate;
end;

procedure TSCDeCustomControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

constructor TSCDeCustomControl.Create(AOwner: TComponent);
begin
  FNotificationList := nil;
  FHotNCArea := SCDE_HTNONE;

  FCreatingControl := True;
  FImageIndex := -1;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks,
    csOpaque, csReplicatable];

  FAutoSize   := False;
  {$IFDEF SCDE_DELPHI6_UP}
  AutoSize    := False;
  {$ENDIF}

  FBorder      := scdcbNone;
  FBorderColor := clBtnFace;
  FBorderInner := scdcbNone;
  FBlendColor  := False;
  FClickFocus  := True;
  FFlatColor   := clBtnShadow;
  FFlatInnerColor := clBtnShadow;
  FMouseDownPoint := Point(-1, -1);
  FImageLayout := scilLeft;
  FSpacing     := 4;

  FBorderProps := GetBorderPropsClass.Create(Self);

  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FPictureIndex := -1;
  FPictureNotifier := TSCDePictureNotifier.Create;
  FPictureNotifier.OnChange := PictureListChanged;

  FShowPicture := True;
  FPictureOrient := scdpoTiled;

  FPictureProps := GetPicturePropsClass.Create(Self);

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  AdjustBounds;
end;

function TSCDeCustomControl.CurrentBlendedColor(AColor: TColor): TColor;
begin
  Result := AColor;
  if FBlendColor then
    Result := DefaultBlendedColor(AColor);
end;

function TSCDeCustomControl.DefaultBlendedColor(AColor: TColor): TColor;
begin
  Result := scdBlendedColor(AColor, GetBlendValue, GetBlendValue,
    GetBlendValue, True);
end;

destructor TSCDeCustomControl.Destroy;
begin
  FPicture.OnChange := nil;
  FreeAndNil(FPicture);

  FPictureNotifier.OnChange := nil;
  if FPictureList <> nil then
    FPictureList.UnregisterNotifier(FPictureNotifier);

  FreeAndNil(FPictureNotifier);
  FreeAndNil(FPictureProps);

  NotifyAll(Self, opRemove);
  FreeAndNil(FNotificationList);

  FreeAndNil(FImageChangeLink);
  FreeAndNil(FBorderProps);
  inherited Destroy;
end;

procedure TSCDeCustomControl.DoAutoSize(Value: Boolean);
begin
  if (FAutoSize <> Value) or (inherited AutoSize <> Value) then
  begin
    FAutoSize := Value;

    AutoSizeChanged;

    {$IFDEF SCDE_DELPHI6_UP}
    inherited SetAutoSize(Value);
    {$ELSE}
    inherited AutoSize := Value;
    {$ENDIF}
    AdjustBounds;
  end;
end;

procedure TSCDeCustomControl.DoBorderChanged;
begin
  // RecreateWnd;
  Perform(CM_BORDERCHANGED, 0, 0);
  {if HandleAllocated then SetWindowPos(Handle, 0, 0,0,0,0, SWP_NOACTIVATE or
    SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);}

  if HandleAllocated then
  begin
    RedrawScrollbars;
    RedrawBorder(0);
  end;

  BorderChanged;
end;

procedure TSCDeCustomControl.DoChange;
begin
  //
end;

procedure TSCDeCustomControl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FClickFocus and CanGetFocus and CanFocus then
  begin
    SetFocus;
    if not Focused then
      Exit;
  end;

  if (Button = mbLeft) and IsActive and IsInClientRect(X, Y) and
    ((ssLeft in Shift) or (ssDouble in Shift)) then
  begin
    SetCapture(Handle);

    FMouseIsDown := True;
    FMouseDownPoint := Point(X, Y);
  end;
end;

procedure TSCDeCustomControl.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (FMouseIsDown or FMouseInControl) then
    UpdateTracking;
end;

procedure TSCDeCustomControl.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);
end;

procedure TSCDeCustomControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then UpdateLocked;
  end;
end;

function TSCDeCustomControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSCDeControlActionLink;
end;

function TSCDeCustomControl.GetBlendValue: Word;
begin
  Result := 0;
  if FBlendColor then Result := 35;
end;

function TSCDeCustomControl.GetBorderExColor: TColor;
begin
  Result := Self.Color;
end;

function TSCDeCustomControl.GetBorderSize: Integer;
begin
  Result := 0;
  if FBorder in [scdcbRaised, scdcbLowered, scdcbFlat, scdcbColor] then
    Result := 1
  else if FBorder in [scdcbFlatBold, scdcbFlatRounded, scdcbFlatBoldRounded,
    scdcb3DRaised, scdcb3DLowered, scdcbBumped, scdcbEtched, scdcbMacLowered,
    scdcbMacRaised, scdcbMetal, scdcbSoftLowered, scdcbSoftRaised] then
    Result := 2;
end;

function TSCDeCustomControl.GetFaceColor: TColor;
var
  B: Integer;
begin
  Result := Self.Color;
  if Result = clNone then Result := clBtnFace;

  if FBlendColor then
  begin
    B := GetBlendValue;
    Result := scdBlendedColor(Self.Color, B, B, B, True);
  end;
end;

function TSCDeCustomControl.GetImageRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FImages <> nil then
    Result := Rect(0, 0, Images.Width, Images.Height);
end;

function TSCDeCustomControl.GetIndent: Integer;
begin
  Result := 0;
  if (ImageLayout in [scilLeft, scilRight]) and 
    IsValidImage(FImageIndex) and (Caption <> '') then
    Result := FIndent;
end;

function TSCDeCustomControl.GetInnerBorderSize: Integer;
begin
  Result := 0;
  if FBorderInner in [scdcbRaised, scdcbLowered, scdcbFlat, scdcbColor] then
    Result := 1
  else if FBorderInner in [scdcbFlatBold, scdcbFlatRounded, scdcbFlatBoldRounded,
    scdcb3DRaised, scdcb3DLowered, scdcbBumped, scdcbEtched, scdcbMacLowered,
    scdcbMacRaised, scdcbMetal, scdcbSoftLowered, scdcbSoftRaised] then
    Result := 2;
end;

function TSCDeCustomControl.GetTextRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if Caption <> '' then
  begin
    Canvas.Font.Assign(Self.Font);
    Result := Rect(0, 0, ClientWidth, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
      Result, DT_CALCRECT);
  end;
end;

procedure TSCDeCustomControl.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TSCDeCustomControl.IndentChanged;
begin
  Invalidate;
end;

function TSCDeCustomControl.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TSCDeCustomControl.IsActive: Boolean;
begin
  Result := True;
end;

function TSCDeCustomControl.IsInClientRect(X, Y: Integer): Boolean;
begin
  Result := True;
end;

function TSCDeCustomControl.IsValidImage(Indx: Integer): Boolean;
begin
  Result := (Indx > -1) and (Images <> nil) and
    (Images.Count > 0) and (Indx < Images.Count);
end;

procedure TSCDeCustomControl.Loaded;
begin
  inherited Loaded;
  if FStyleController <> nil then
    NotifyStyleChange;

  AdjustBounds;
end;

procedure TSCDeCustomControl.MouseInControlChanged;
begin
  //
end;

procedure TSCDeCustomControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TSCDeCustomControl.DoFillScrollBarsCorner(OnDC: HDC);
var
  DC: HDC;
  R, RW, RC: TRect;
begin
  if HandleAllocated and ((GetWindowLong(Handle, GWL_STYLE) and
    (WS_HSCROLL or WS_VSCROLL)) = (WS_HSCROLL or WS_VSCROLL)) then
  begin
    DC := OnDC;
    if OnDc = 0 then
      DC := GetWindowDC(Handle);

    try
      GetWindowRect(Handle, RW);
      R := RW;

      MapWindowPoints(0, Handle, RW, 2);

      OffsetRect(R, -R.Left, -R.Top);
      InflateRect(R, RW.Left, RW.Top);

      Windows.GetClientRect(Handle, RC);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

      R.Top := RC.Bottom;
      R.Bottom := R.Top + scdHorzScrollbarHeight;
      
      if UseRightToLeftScrollBar then  // vertical scrollbar is on left
      begin
        R.Right := RC.Left;
        R.Left  := R.Right - scdVertScrollbarWidth
      end else
      begin
        R.Left  := RC.Right;
        R.Right := R.Left + scdVertScrollbarWidth;
      end;

      if not IsRectEmpty(R) then
        FillRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
    finally
      if OnDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TSCDeCustomControl.RedrawBorder(const Clip: HRGN; OnDC: HDC);
var
  DC: HDC;
  R, BRect: TRect;
  OldDCPen: HPen;
  FramePen: TPen;
  C1, Cl: TColor;
  Bs, Offset: Integer;
  Points: array[0..4] of TPoint;
begin
  if not HandleAllocated or IsIconic(Handle) or (csDestroying in ComponentState) then
    Exit;

  DC := OnDC;
  if OnDC = 0 then
    DC := GetWindowDC(Handle);

  Cl := BorderColor;
  if Cl = clNone then
  begin
    Cl := Self.Color;
    if Cl = clNone then Cl := clWindow;
  end;  

  if BlendColor then
    Cl := DefaultBlendedColor(Cl);

  FramePen := TPen.Create;
  with FramePen do
  begin
    Style := psInsideFrame;
    Color := Cl;
  end;

  OldDCPen := 0;
  try
    Offset := BorderWidth div 2;

    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    BRect := R;
    InflateRect(R, -Offset, -Offset);

    if Odd(BorderWidth) then
    begin
      Dec(R.Right);
      if R.Right < R.Left then
        R.Right := R.Left;

      Dec(R.Bottom);
      if R.Bottom < R.Top then
        R.Bottom := R.Top;
    end;

    if BorderWidth > 0 then
      FramePen.Width := BorderWidth;

    OldDCPen := SelectObject(DC, FramePen.Handle);
    SetROP2(DC, scdPenModes[FramePen.Mode]);

    Bs := GetBorderSize;

    if BorderWidth > 0 then
    begin
      InflateRect(R, -Bs, -Bs);

      Points[0] := Point(R.Left, R.Top);
      Points[1] := Point(R.Right, R.Top);
      Points[2] := Point(R.Right, R.Bottom);
      Points[3] := Point(R.Left, R.Bottom);
      Points[4] := Point(R.Left, R.Top);

      Windows.MoveToEx(DC, Points[0].X - Offset, Points[0].Y, nil);
      Windows.LineTo(DC, Points[1].X + Offset, Points[1].Y);

      Windows.MoveToEx(DC, Points[1].X, Points[1].Y - Offset, nil);
      Windows.LineTo(DC, Points[2].X, Points[2].Y + Offset);

      Windows.MoveToEx(DC, Points[2].X + Offset, Points[2].Y, nil);
      Windows.LineTo(DC, Points[3].X - Offset, Points[3].Y);

      Windows.MoveToEx(DC, Points[3].X, Points[3].Y + Offset, nil);
      Windows.LineTo(DC, Points[4].X, Points[4].Y - Offset);
    end;

    R := BRect;
    if not IsRectEmpty(R) and (FBorder <> scdcbNone) then
    begin
      if FBorderEx then
      begin
        C1 := GetBorderExColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        if FBorder in [scdcbFlat, scdcbFlatBold] then
          C1 := scdGetBtnShadowOf(C1);

        scdDrawBevelEx(DC, R, C1, TSCDeFakeControl(Parent).Color, True, FBorder);
      end else
      begin
        C1 := FFlatColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        scdDrawEdgeEx(DC, R, C1, TSCDeFakeControl(Parent).Color, True, FBorder);
      end;

      InflateRect(BRect, -Bs, -Bs);
    end;

    R := BRect;
    InflateRect(R, -BorderWidth, -BorderWidth);

    if not IsRectEmpty(BRect) and (FBorderInner <> scdcbNone) then
    begin
      if FBorderEx then
      begin
        C1 := GetBorderExColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        if FBorder in [scdcbFlat, scdcbFlatBold] then
          C1 := scdGetBtnShadowOf(C1);

        scdDrawBevelEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, FBorderInner);
      end else
      begin
        C1 := FFlatInnerColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        scdDrawEdgeEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, FBorderInner);
      end;
    end;
  finally
    if OldDCPen <> 0 then
      SelectObject(DC, OldDCPen);

    if (DC <> 0) and (OnDC = 0) then
      ReleaseDC(Handle, DC);

    FramePen.Free;
  end;
end;

function TSCDeCustomControl.ScreenToNC(P: TPoint): TPoint;
var
  R: TRect;
begin
  Result := P;

  {if HandleAllocated and GetWindowRect(Self.Handle, R) then
  begin
    OffsetRect(R, -R.Left, -R.Top);
    CalculateBorder(R);

    InflateRect(R, -BorderWidth, -BorderWidth);

    MapWindowPoints(0, Self.Handle, Result, 1);

    Inc(Result.x, R.Left);
    Inc(Result.y, R.Top);
  end;}

  if HandleAllocated and GetWindowRect(Self.Handle, R) then
  begin
    MapWindowPoints(0, Self.Handle, R, 2);
    MapWindowPoints(0, Self.Handle, Result, 1);

    Dec(Result.x, R.Left);
    Dec(Result.y, R.Top);
  end;
end;

procedure TSCDeCustomControl.SetAutoSize(Value: Boolean);
begin
  DoAutoSize(Value);
end;

procedure TSCDeCustomControl.SetBlendColor(Value: Boolean);
begin
  if FBlendColor <> Value then
  begin
    FBlendColor := Value;
    Invalidate;
  end;
end;

procedure TSCDeCustomControl.SetBorder(Value: TSCDeControlBorder);
begin
  if CanSetBorder(Value) and (FBorder <> Value) then
  begin
    FBorder := Value;
    DoBorderChanged;
  end;
end;

procedure TSCDeCustomControl.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    RedrawBorder(0);
  end;
end;

procedure TSCDeCustomControl.SetBorderEx(Value: Boolean);
begin
  if FBorderEx <> Value then
  begin
    FBorderEx := Value;
    if FBorder <> scdcbNone then
      DoBorderChanged;
  end;
end;

procedure TSCDeCustomControl.SetBorderInner(Value: TSCDeControlBorder);
begin
  if CanSetInnerBorder(Value) and (FBorderInner <> Value) then
  begin
    FBorderInner := Value;
    DoBorderChanged;
  end;
end;

procedure TSCDeCustomControl.SetFlatColor(Value: TColor);
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    if FBorder <> scdcbNone then
      RedrawBorder(0);
  end;
end;

procedure TSCDeCustomControl.SetFlatInnerColor(Value: TColor);
begin
  if FFlatInnerColor <> Value then
  begin
    FFlatInnerColor := Value;
    if FBorderInner <> scdcbNone then
      RedrawBorder(0);
  end;
end;

procedure TSCDeCustomControl.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FImages <> nil then
      Invalidate;
  end;
end;

procedure TSCDeCustomControl.SetImageLayout(Value: TSCDeImageLayout);
begin
  if FImageLayout <> Value then
  begin
    FImageLayout := Value;
    if FImages <> nil then
      Invalidate;
  end;
end;

procedure TSCDeCustomControl.SetImages(Value: TCustomImageList);
var
  OldImages: TCustomImageList;
begin
  OldImages := FImages;
  
  if FImages <> nil then
  begin
  {$IFDEF SCDE_DELPHI5_UP}
    FImages.RemoveFreeNotification(Self);
  {$ENDIF}
    FImages.UnRegisterChanges(FImageChangeLink);
  end;

  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;

  if OldImages <> FImages then
  begin
    Invalidate;
    ImageListChange(FImages);
  end;
end;

procedure TSCDeCustomControl.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;

    AdjustBounds;
    IndentChanged;
  end;
end;

procedure TSCDeCustomControl.SetSpacing(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FSpacing <> Value then
  begin
    FSpacing := Value;

    AdjustBounds;
    SpacingChanged;
  end;
end;

procedure TSCDeCustomControl.SpacingChanged;
begin
  Invalidate;
end;

procedure TSCDeCustomControl.StopTracking;
var
  WasDown, WasInside: Boolean;
  AForm: TCustomForm;
  P: TPoint;
  R: TRect;
begin
  if not HandleAllocated then
    Exit;

  if GetCapture = Handle then
    ReleaseCapture;

  WasDown := FMouseIsDown;
  WasInside := FMouseInControl;

  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);

  AForm := GetParentForm(Self);
  GetCursorPos(P);
  P := ScreenToNC(P);
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);

  FMouseInControl := (Screen.ActiveCustomForm = AForm) and PtInRect(R, P);

  if (WasDown <> FMouseIsDown) or (WasInside <> FMouseInControl) then
    Invalidate;
end;

procedure TSCDeCustomControl.TranslateMouseDown(var Message: TWMMouse;
  Button: TMouseButton; Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
end;

procedure TSCDeCustomControl.TranslateMouseMove(var Message: TWMMouse);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseMove(KeysToShiftState(Keys), XPos, YPos);
end;

procedure TSCDeCustomControl.TranslateMouseUp(var Message: TWMMouse;
  Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TSCDeCustomControl.UpdateLocked;
begin
  //
end;

procedure TSCDeCustomControl.UpdateTracking;
var
  P: TPoint;
begin
  FMouseInControl := False;
  if Enabled and not (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

procedure TSCDeCustomControl.UpdateUnlocked;
begin
  //
end;

procedure TSCDeCustomControl.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TSCDeCustomControl.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    CaptureChanged(HandleAllocated and (HWND(Message.LParam) = Self.Handle));
end;

procedure TSCDeCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  SaveIndex: Integer;
begin
  Message.Result := 0;
  if HandleAllocated and (Message.DC <> 0) and (csPaintCopy in ControlState) then
  begin
    SaveIndex := SaveDC(Message.DC);
    try
      DoFillScrollBarsCorner(Message.DC);
      RedrawScrollbars(scsdkAll, Message.DC);
      RedrawBorder(0, Message.DC);
    finally
      RestoreDC(Message.DC, SaveIndex);
    end;
  end;
end;

procedure TSCDeCustomControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  // Message.Result := DLGC_WANTARROWS;
end;

procedure TSCDeCustomControl.WMKillFocus(var Message: TWMKillFocus);
var
  P: TPoint;
  InCtrl: Boolean;
begin
  FHasFocus := False;
  StopTracking;

  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);

  GetCursorPos(P);
  P := Self.ScreenToClient(P);

  InCtrl := FMouseInControl;
  FMouseInControl := Enabled and not (csDesigning in ComponentState) and
    (FindDragTarget(P, True) = Self);

  if Enabled and not (csDesigning in ComponentState) and
    (InCtrl <> FMouseInControl) then
  begin
    if not FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else Perform(CM_MOUSEENTER, 0, 0);
  end;

  Invalidate;
  inherited;
  
  DoFocusChanged;
end;

procedure TSCDeCustomControl.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if not (csDesigning in ComponentState) then
  begin
    SendCancelMode(Self);
    TranslateMouseDown(Message, mbLeft, []);
  end;
  inherited;
end;

procedure TSCDeCustomControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseUp(Message, mbLeft);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseUp(Message, mbLeft);
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMMButtonDown(var Message: TWMMButtonDown);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbMiddle, []);
  inherited;
end;

procedure TSCDeCustomControl.WMMButtonUp(var Message: TWMMButtonUp);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseUp(Message, mbMiddle);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseUp(Message, mbMiddle);
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMMouseMove(var Message: TWMMouseMove);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      DoFakeMouseMove(Message);
      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseMove(Message);
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  with Message.CalcSize_Params^ do
    CalculateBorder(Rgrc[0]);
end;

procedure TSCDeCustomControl.WMPrint(var Message: TMessage);
var
  DC: HDC;
  SaveIndex: Integer;
begin
  inherited;

  if HandleAllocated then
  begin
    if ((Message.LParam and PRF_CHECKVISIBLE) <> PRF_CHECKVISIBLE) and
      not IsWindowVisible(Handle) then
      Exit;

    DC := HDC(Message.WParam);
    SaveIndex := SaveDC(DC);
    try
      RedrawScrollbars(scsdkAll, DC);
      DoFillScrollBarsCorner(DC);
      RedrawBorder(0, DC);
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;
end;

procedure TSCDeCustomControl.WMNCPaint(var Message: TMessage);
begin
  DefaultHandler(Message);
  DoFillScrollBarsCorner;
  RedrawScrollbars;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TSCDeCustomControl.WMRButtonDown(var Message: TWMRButtonDown);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbRight, []);
  inherited;
end;

procedure TSCDeCustomControl.WMRButtonUp(var Message: TWMRButtonUp);
{$IFDEF SCDE_DELPHI4_AND_EARLY}
var
  Handled: Boolean;
{$ENDIF}
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseUp(Message, mbRight);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseUp(Message, mbRight);

    {$IFDEF SCDE_DELPHI4_AND_EARLY}
    if (Message.Result = 0) and CanMenuPopup(Message.Pos) and
      Assigned(FOnContextPopup) then
    begin
      Handled := False;
      FOnContextPopup(Self, SmallPointToPoint(Message.Pos), Handled);
    end;
    {$ENDIF}
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMSetFocus(var Message: TWMSetFocus);
var
  CapC: HWND;
begin
  FHasFocus := True;
  inherited;

  if HandleAllocated and not Focused then
  begin
    CapC := GetCapture;
    if (CapC <> 0) and (CapC <> Self.Handle) then
      SetCapture(Self.Handle);
  end;

  Invalidate;
  DoFocusChanged;
end;

procedure TSCDeCustomControl.WMSettingChange(var Message: TWMSettingChange);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeCustomControl.WMWindowPosChanged(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TSCDeCustomControl.PaintParentOn(C: TCanvas);
var
  DC: Cardinal;
  InDesign: Boolean;
  Ctrl: TControl;
  R, R2, CR,
  SelfR, CtlR, ParentR: TRect;
  I, X, Y, Cnt, SaveIndex: Integer;
begin
  if (Parent = nil) or (C = nil) or (C.Handle = 0) then
    Exit;

  ParentR := Parent.ClientRect;
  if IsRectEmpty(ParentR) then
    Exit;

  SelfR := GetClientRect;
  with SelfR do
  begin
    TopLeft := Self.ClientToScreen(TopLeft);
    BottomRight := Self.ClientToScreen(BottomRight);

    TopLeft := Parent.ScreenToClient(TopLeft);
    BottomRight := Parent.ScreenToClient(BottomRight);
  end;

  X := -SelfR.Left;
  Y := -SelfR.Top;

  IntersectRect(SelfR, SelfR, ParentR);
  if IsRectEmpty(SelfR) then
    Exit;

  DC := C.Handle;

  // Copy parent control image
  SaveIndex := SaveDC(DC);
  try
    SetViewportOrgEx(DC, X, Y, nil);
    IntersectClipRect(DC, 0, 0, ParentR.Right - ParentR.Left, ParentR.Bottom - ParentR.Top);
    Self.Parent.Perform(WM_ERASEBKGND, WParam(DC), 0);
    TSCDeParentControl(Self.Parent).PaintWindow(DC);
  finally
    RestoreDC(DC, SaveIndex);
  end;  

  //Copy images of parent controls
  InDesign := csDesigning in ComponentState;

  Cnt := Parent.ControlCount;
  for I := 0 to Cnt - 1 do begin
    if Parent.Controls[I] <> nil then
    begin
      Ctrl := Parent.Controls[I];
      if Ctrl = Self then
        Break;

      with Ctrl do
      begin
        CtlR := Bounds(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height);

        if Bool(IntersectRect(R, SelfR, CtlR)) and (Visible or InDesign) then
        begin
          CR := ClientRect;
          R2 := CR;

          with R2 do
          begin
            TopLeft := ClientToScreen(TopLeft);
            BottomRight := ClientToScreen(BottomRight);

            TopLeft := Parent.ScreenToClient(TopLeft);
            BottomRight := Parent.ScreenToClient(BottomRight);
          end;

          SaveIndex := SaveDC(DC);
          try
            SetViewportOrgEx(DC, Left + X, Top + Y, nil);
            IntersectClipRect(DC, 0, 0, Width, Height);

            Perform(WM_PRINT, DC, PRF_NONCLIENT or PRF_CLIENT or
              PRF_ERASEBKGND or PRF_CHILDREN);

            SetViewportOrgEx(DC, R2.Left + X, R2.Top + Y, nil);
            IntersectClipRect(DC, CR.Left, CR.Top, CR.Right, CR.Bottom);
            Perform(WM_PAINT, DC, 0);
          finally
            RestoreDC(DC, SaveIndex);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomControl.SetTransparent(Value: Boolean);
var
  CS: TControlStyle;
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;

    CS := ControlStyle - [csOpaque];
    if not Value then
      CS := CS + [csOpaque];

    ControlStyle := CS;  
    TransparentChanged;
  end;
end;

procedure TSCDeCustomControl.TransparentChanged;
begin
  Invalidate;
end;

function TSCDeCustomControl.IsHorzScrollBarVisible: Boolean;
begin
  Result := HandleAllocated and
    (GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0);
end;

function TSCDeCustomControl.IsVertScrollBarVisible: Boolean;
begin
  Result := HandleAllocated and
    (GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0);
end;

procedure TSCDeCustomControl.AfterConstruction;
begin
  FCreatingControl := False;
  inherited AfterConstruction;
end;

procedure TSCDeCustomControl.Assign(Source: TPersistent);
begin
  if Source is TControl then
  begin
    with TControl(Source) do
    begin
      Self.Align := Align;
      Self.Anchors := Anchors;
      Self.BiDiMode := BiDiMode;
      Self.Caption := Caption;
      Self.Color := Color;
      Self.Constraints := Constraints;
      Self.Cursor := Cursor;
      Self.DockSite := DockSite;
      Self.DragCursor := DragCursor;
      Self.DragKind := DragKind;
      Self.DragMode := DragMode;
      Self.Enabled := Enabled;
      Self.Font := Font;
      Self.Hint := Hint;
      Self.ImeMode := ImeMode;
      Self.ImeName := ImeName;
      Self.ParentBiDiMode := ParentBiDiMode;
      Self.ParentColor := ParentColor;
      Self.ParentFont := ParentFont;
      Self.ShowHint := ShowHint;
      Self.UseDockManager := UseDockManager;
      Self.Visible := Visible;
      Self.TabStop := TabStop;
      Self.Width := Width;
      Self.Height := Height;
    end;

    if Self is TSCDeCustomControl then
    begin
      with TSCDeCustomControl(Source) do
      begin
        Self.BorderProps := BorderProps;
        Self.ClickFocus := ClickFocus;
        Self.ImageIndex := ImageIndex;
        Self.ImageLayout := ImageLayout;
        Self.Indent := Indent;
        Self.Picture := Picture;
        Self.PictureOrient := PictureOrient;
        Self.PictureIndent := PictureIndent;
        Self.PictureTopIndent := PictureTopIndent;
        Self.PictureTransparent := PictureTransparent;
        Self.PictureIndex := PictureIndex;
        Self.ShowPicture := ShowPicture;
        Self.Spacing := Spacing;
        Self.Transparent := Transparent;
      end;
    end;
  end else
    inherited Assign(Source);
end;

function TSCDeCustomControl.GetClientRect: TRect;
var
  B: Integer;
begin
  if not HandleAllocated then
  begin
    Result := Rect(0, 0, Width, Height);

    B := GetBorderSize + BorderWidth + GetInnerBorderSize;
    if B < 0 then B := 0;

    InflateRect(Result, -B, -B);

    if Result.Right < Result.Left then
    begin
      Result.Left := Result.Left - ((Result.Left - Result.Right) div 2);
      Result.Right := Result.Left;
    end;

    if Result.Bottom < Result.Top then
    begin
      Result.Top := Result.Top - ((Result.Top - Result.Bottom) div 2);
      Result.Bottom := Result.Top;
    end;
  end else
    Result := inherited GetClientRect;
end;

procedure TSCDeCustomControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
  begin
    SendCancelMode(Self);
    if csCaptureMouse in ControlStyle then MouseCapture := True;

    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseDown(Message, mbLeft, [ssDouble]);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseDown(Message, mbLeft, [ssDouble]);
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseDown(Message, mbMiddle, [ssDouble]);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseDown(Message, mbMiddle, [ssDouble]);
  end;

  inherited;
end;

procedure TSCDeCustomControl.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
  begin
    if FNCIsDown then
    begin
      FNCIsDown := False;
      DoFakeMouseDown(Message, mbRight, [ssDouble]);

      DefaultHandler(Message);
      Exit;
    end;

    TranslateMouseDown(Message, mbRight, [ssDouble]);
  end;

  inherited;
end;

procedure TSCDeCustomControl.AutoSizeChanged;
begin
  //
end;

procedure TSCDeCustomControl.EnabledChanged;
begin
  //
end;

function TSCDeCustomControl.GetAbout: TSCDeAboutString;
begin
  Result := SCDE_VersionStr;
end;

procedure TSCDeCustomControl.SetAbout(Value: TSCDeAboutString);
begin
  //
end;

procedure TSCDeCustomControl.RedrawScrollbars(Kind: TSCDeScrollbarDrawKind;
  OnDC: HDC);
begin
  //
end;

procedure TSCDeCustomControl.DoNCMouseDown(var Message: TWMNCHitMessage;
  Button: TMouseButton; Shift: TShiftState);
var
  P: TPoint;
begin
  with Message do
  begin
    P := ScreenToNC(Point(XCursor, YCursor));
    NCMouseDown(Button, HitTest, ssDouble in Shift, P.x, P.y);
  end;
end;

procedure TSCDeCustomControl.DoNCMouseUp(var Message: TWMNCHitMessage;
  Button: TMouseButton);
var
  P: TPoint;
begin
  with Message do
  begin
    P := ScreenToNC(Point(XCursor, YCursor));
    NCMouseUp(Button, HitTest, P.x, P.y);
  end;
end;

procedure TSCDeCustomControl.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
begin
  if csDesigning in ComponentState then
    inherited
  else begin
    SendCancelMode(Self);
    inherited;
    FNCIsDown := GetCapture = Self.Handle;
    DoNCMouseDown(Message, mbLeft, [ssDouble]);
  end;
end;

procedure TSCDeCustomControl.WMNCLButtonDown(var Message: TWMNCHitMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if csDesigning in ComponentState then
      inherited
    else begin
      SendCancelMode(Self);

      if CanCaptureMouseOnNC(Point(Message.XCursor, Message.YCursor)) then
        SetCapture(Handle);

      inherited;
      FNCIsDown := GetCapture = Self.Handle;
      DoNCMouseDown(Message, mbLeft, []);
    end;
  end else
    inherited;
end;

procedure TSCDeCustomControl.WMNCLButtonUp(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DoNCMouseUp(Message, mbLeft);
end;

procedure TSCDeCustomControl.WMNCMButtonDblClk(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FNCIsDown := GetCapture = Self.Handle;
    DoNCMouseDown(Message, mbMiddle, [ssDouble]);
  end;  
end;

procedure TSCDeCustomControl.WMNCMButtonDown(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FNCIsDown := GetCapture = Self.Handle;
    DoNCMouseDown(Message, mbMiddle, []);
  end;
end;

procedure TSCDeCustomControl.WMNCMButtonUp(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DoNCMouseUp(Message, mbRight);
end;

procedure TSCDeCustomControl.WMNCMouseMove(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    with Message do
    begin
      P := ScreenToNC(Point(XCursor, YCursor));
      NCMouseMove(Message.HitTest, P.x, P.y);
    end;
end;

procedure TSCDeCustomControl.WMNCRButtonDblClk(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FNCIsDown := GetCapture = Self.Handle;
    DoNCMouseDown(Message, mbRight, [ssDouble]);
  end;  
end;

procedure TSCDeCustomControl.WMNCRButtonDown(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FNCIsDown := GetCapture = Self.Handle;
    DoNCMouseDown(Message, mbRight, []);
  end;  
end;

procedure TSCDeCustomControl.WMNCRButtonUp(var Message: TWMNCHitMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    DoNCMouseUp(Message, mbRight);
end;

procedure TSCDeCustomControl.NCMouseDown(Button: TMouseButton;
  HitTest: Integer; DblClk: Boolean; X, Y: Integer);
begin
  //
end;

procedure TSCDeCustomControl.NCMouseMove(HitTest, X, Y: Integer);
begin
  //
end;

procedure TSCDeCustomControl.NCMouseUp(Button: TMouseButton; HitTest, X,
  Y: Integer);
begin
  //
end;

procedure TSCDeCustomControl.NCMouseEnter(var HitTest: Integer; X, Y: Integer);
begin
  //
end;

procedure TSCDeCustomControl.NCMouseLeave;
begin
  //
end;

procedure TSCDeCustomControl.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  OldNC: LongInt;
  CapHnd: HWND;
begin
  CapHnd := GetCapture;
  if (csDesigning in ComponentState) or ((CapHnd <> 0) and
    not (HandleAllocated and (CapHnd = Self.Handle))) then
    inherited
  else begin
    OldNC := FHotNCArea;
    inherited;

    P := SmallPointToPoint(Message.Pos);
    FHotNCArea := GetNCAreaAtPos(P);

    if OldNC <> FHotNCArea then
    begin
      P := ScreenToNC(P);

      Message.Result := HTBORDER;
      NCMouseEnter(Message.Result, P.x, P.y);
    end;
  end;  
end;

function TSCDeCustomControl.GetNCAreaAtPos(P: TPoint): LongInt;
var
  B: Integer;
  R, CR: TRect;
begin
  Result := SCDE_HTNONE;
  if HandleAllocated and Windows.GetWindowRect(Handle, R) and
    Windows.GetClientRect(Handle, CR) then
  begin
    MapWindowPoints(Handle, 0, CR, 2);

    if PtInRect(R, P) and (IsRectEmpty(CR) or not PtInRect(CR, P)) then
    begin
      B := GetBorderSize;
      if B > 0 then
      begin
        InflateRect(R, -B, -B);

        if IsRectEmpty(R) or not PtInRect(R, P) then
        begin
          Result := SCDE_HTOUTTERBORDER;
          Exit;
        end;
      end;

      B := BorderWidth;
      if B > 0 then
      begin
        InflateRect(R, -B, -B);

        if IsRectEmpty(R) or not PtInRect(R, P) then
        begin
          Result := SCDE_HTBORDER;
          Exit;
        end;
      end;

      B := GetInnerBorderSize;
      if B > 0 then
      begin
        InflateRect(R, -B, -B);

        if IsRectEmpty(R) or not PtInRect(R, P) then
        begin
          Result := SCDE_HTOUTTERBORDER;
          Exit;
        end;
      end;

      Result := GetInheritedNCArea(P);
    end;
  end;
end;

function TSCDeCustomControl.GetInheritedNCArea(P: TPoint): LongInt;
begin
  Result := SCDE_HTNONE;
end;

procedure TSCDeCustomControl.DoFakeMouseUp(Message: TWMMouse;
  Button: TMouseButton);
var
  P: TPoint;
  Msg: TWMNCHitMessage;
begin
  P := Self.ClientToScreen(Point(Message.XPos, Message.YPos));

  Msg.Msg     := Message.Msg;
  Msg.Result  := HTCLIENT;
  Msg.HitTest := HTCLIENT;
  Msg.XCursor := P.x;
  Msg.YCursor := P.y;

  if csCaptureMouse in ControlStyle then MouseCapture := False;
  DoNCMouseUp(Msg, Button);
end;

procedure TSCDeCustomControl.DoFakeMouseDown(Message: TWMMouse;
  Button: TMouseButton; Shift: TShiftState);
var
  P: TPoint;
  Msg: TWMNCHitMessage;
begin
  P := Self.ClientToScreen(Point(Message.XPos, Message.YPos));

  Msg.Msg     := Message.Msg;
  Msg.Result  := HTCLIENT;
  Msg.HitTest := HTCLIENT;
  Msg.XCursor := P.x;
  Msg.YCursor := P.y;

  // if csCaptureMouse in ControlStyle then MouseCapture := True;
  SetCapture(Self.Handle);
  DoNCMouseDown(Msg, Button, Shift);
end;

procedure TSCDeCustomControl.DoFakeMouseMove(Message: TWMMouse);
var
  P: TPoint;
begin
  P := Self.ClientToScreen(Point(Message.XPos, Message.YPos));
  P := ScreenToNC(P);

  NCMouseMove(HTCLIENT, P.x, P.y);
end;

function TSCDeCustomControl.CanCaptureMouseOnNC(P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TSCDeCustomControl.ScrollerChanged(Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerDestroyed(
  Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerPositionChanged(
  Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerPositionChanging(
  Sender: TSCDeCustomControlScrollbar; var ScrollPos: Integer;
  var CanScroll: Boolean);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerRangeChanged(
  Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerSizeChanged(
  Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

procedure TSCDeCustomControl.ScrollerVisibleChanged(
  Sender: TSCDeCustomControlScrollbar);
begin
  //
end;

function TSCDeCustomControl.GetHorzScrollbarRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TSCDeCustomControl.GetVertScrollbarRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TSCDeCustomControl.GetBorderPropsClass: TSCDeControlBorderPropsClass;
begin
  Result := TSCDeBorderProps;
end;

procedure TSCDeCustomControl.SetBorderProps(Value: TSCDeControlBorderProps);
begin
  FBorderProps.Assign(Value);
end;

procedure TSCDeCustomControl.DoPictureListChanged;
begin
  //
end;

procedure TSCDeCustomControl.SetPictureList(Value: TSCDePictureList);
begin
  if FPictureList <> Value then
  begin
    if FPictureList <> nil then
      FPictureList.UnregisterNotifier(FPictureNotifier);

    FPictureList := Value;
    if FPictureList <> nil then
      FPictureList.RegisterNotifier(FPictureNotifier);

    PictureListChanged(FPictureList, scpcaChanged);
  end;
end;

procedure TSCDeCustomControl.PictureListChanged(Sender: TSCDePictureList;
  Action: TSCDePictureChangeAction);
begin
  if (Action = scpcaDestroyed) and (Sender <> nil) and (Sender = FPictureList) then
  begin
    FPictureList.UnregisterNotifier(FPictureNotifier);
    FPictureList := nil;
  end;

  if FShowPicture and not FDrawingPicture then
    Invalidate;

  DoPictureListChanged;
end;

procedure TSCDeCustomControl.SetPictureIndex(Value: Integer);
begin
  if Value < -1 then Value := -1;

  if FPictureIndex <> Value then
  begin
    FPictureIndex := Value;
    if FPictureList <> nil then
      PictureListChanged(nil, scpcaChanged);
  end;
end;

procedure TSCDeCustomControl.FocusChanged;
begin
  //
end;

procedure TSCDeCustomControl.SystemColorsChanged;
begin
  Invalidate;
end;

procedure TSCDeCustomControl.Paint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;
    FillRect(ClientRect);
  end;  
end;

procedure TSCDeCustomControl.NotifyStyleChange(Props: TSCDeCustomStyleProps);
begin
  if Props <> nil then
    AssignProps(Props);
end;

procedure TSCDeCustomControl.SetStyleController(Value: TSCDeCustomStyleController);
begin
  if FStyleController <> Value then
  begin
    if FStyleController <> nil then
    begin
      {$IFDEF SCDE_DELPHI5_UP}
      FStyleController.RemoveFreeNotification(Self);
      {$ENDIF}
      FStyleController.UnregisterChanges(Self);
    end;

    FStyleController := Value;

    if FStyleController <> nil then
    begin
      FStyleController.FreeNotification(Self);
      FStyleController.RegisterChanges(Self);
    end;
  end;
end;

procedure TSCDeCustomControl.NotifyStyleChange;
begin
  if FStyleController <> nil then
    AssignProps(FStyleController.BorderProps);
end;

function TSCDeCustomControl.CanUpdateStyle(Props: TSCDeCustomStyleProps): Boolean;
begin
  Result := Props <> nil;
end;

procedure TSCDeCustomControl.AssignProps(Props: TSCDeCustomStyleProps);
begin
  if Props is TSCDeStyleBorderProps then
    Self.BorderProps.Assign(Props);
end;

function TSCDeCustomControl.CanSetBorder(Value: TSCDeControlBorder): Boolean;
begin
  Result := True;
end;

function TSCDeCustomControl.CanSetInnerBorder(Value: TSCDeControlBorder): Boolean;
begin
  Result := True;
end;

function TSCDeCustomControl.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, GetTextCalculateFont.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;

  Result := Metrics.tmHeight;
end;

function TSCDeCustomControl.GetTextCalculateFont: TFont;
begin
  Result := Self.Font;
end;

function TSCDeCustomControl.GetBorderProps: TSCDeControlBorderProps;
begin
  Result := FBorderProps;
end;

function TSCDeCustomControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  R: TRect;
begin
  Result := AutoSize and inherited CanAutoSize(NewWidth, NewHeight);
  if Result then
  begin
    R := GetAdjustedRect(NewWidth, NewHeight);

    NewWidth := R.Right - R.Left;
    if NewWidth < 0 then NewWidth := 0;

    NewHeight := R.Bottom - R.Top;
    if NewHeight < 0 then NewHeight := 0;
  end;
end;

function TSCDeCustomControl.GetAdjustedRect(NewWidth, NewHeight: Integer): TRect;
var
  B: Integer;
begin
  Result := Rect(Left, Top, Left + NewWidth, Top + NewHeight);

  B := 2*(GetBorderSize + BorderWidth + GetInnerBorderSize);
  
  Inc(Result.Right, B);
  Inc(Result.Bottom, B);
end;

procedure TSCDeCustomControl.CMEnter(var Message: TCMEnter);
begin
  FHasFocus := True;

  BeforeEnter;
  inherited;
  AfterEnter;
end;

procedure TSCDeCustomControl.CMExit(var Message: TCMExit);
begin
  FHasFocus := False;

  BeforeExit;
  inherited;
  AfterExit;
end;

procedure TSCDeCustomControl.AfterEnter;
begin
  //
end;

procedure TSCDeCustomControl.AfterExit;
begin
  //
end;

procedure TSCDeCustomControl.BeforeEnter;
begin
  //
end;

procedure TSCDeCustomControl.BeforeExit;
begin
  //
end;

function TSCDeCustomControl.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
end;

function TSCDeCustomControl.GetImages: TCustomImageList;
begin
  Result := FImages;
end;

function TSCDeCustomControl.CanMenuPopup(const Pos: TSmallPoint): Boolean;
var
  Control: TControl;
  PopupMenu: TPopupMenu;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;

  Control := Self;
  while Control <> nil do
  begin
    PopupMenu := TSCDeFakeControl(Control).GetPopupMenu;
    if (PopupMenu <> nil) then
    begin
      if not PopupMenu.AutoPopup then Exit;
      Result := True;
      Exit;
    end;
    Control := Control.Parent;
  end;
end;

procedure TSCDeCustomControl.CMJumpToNext(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TSCDeCustomControl.WMKeyDown(var Message: TWMKeyDown);
begin
  if DoNextControl(Message.CharCode, scdKeyDataToShiftState(Message.KeyData)) then
  begin
    Message.Result := 0;
    Message.CharCode := VK_TAB;
  end else
    inherited;
end;

procedure TSCDeCustomControl.WMSysKeyDown(var Message: TWMKeyDown);
begin
  if not DoNextControl(Message.CharCode, scdKeyDataToShiftState(Message.KeyData)) then
    inherited;
end;

function TSCDeCustomControl.DoNextControl(CharCode: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  if Self.HandleAllocated then
  begin
    Result := CanDoNextControl(CharCode, Shift);
    if Result then PostMessage(Self.Handle, WM_KEYDOWN, VK_TAB, 0);
  end;  
end;

procedure TSCDeCustomControl.CMIsVisibleChild(var Message: TMessage);
var
  C: TControl;
  I: Integer;
begin
  Message.Result := 0;
  C := TControl(Message.WParam);

  if C <> nil then
    for I := 0 to Self.ControlCount-1 do
      if Self.Controls[I] = C then
      begin
        if not IsVisibleChild(C) then
          Message.Result := 1;

        Break;
      end;
end;

function TSCDeCustomControl.IsVisibleChild(C: TControl): Boolean;
begin
  Result := True;
end;

procedure TSCDeCustomControl.LoadFromFile(const FileName: String);
var
  C: TComponent;
  Sl: TStringList;
begin
  if FileExists(FileName) then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromFile(FileName);

      C := scdStringToComponent(Sl.Text);
      if C is TSCDeCustomControl then
        Self.Assign(C);
    finally
      Sl.Free;
    end;
  end;
end;

procedure TSCDeCustomControl.SaveToFile(const FileName: String);
var
  Sl: TStringList;
begin
  Sl := TStringList.Create;
  try
    Sl.Text := scdComponentToString(Self);
    Sl.SaveToFile(FileName);
  finally
    Sl.Free;
  end;
end;

procedure TSCDeCustomControl.DoFocusChanged;
begin
  FocusChanged;
  if Assigned(FOnFocusChange) then
    FOnFocusChange(Self);
end;

procedure TSCDeCustomControl.NotifyAll(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if FNotificationList <> nil then
    for I := 0 to FNotificationList.Count-1 do
      TSCDeNotifier(FNotificationList[I]).Notification(Self, AComponent, Operation);
end;

procedure TSCDeCustomControl.RegisterNotifier(ANotifier: TSCDeNotifier);
begin
  if (ANotifier <> nil) and ((FNotificationList = nil) or
    (FNotificationList.IndexOf(ANotifier) = -1)) then
  begin
    FNotificationList := TList.Create;
    FNotificationList.Add(ANotifier);
  end;
end;

procedure TSCDeCustomControl.UnregisterNotifier(ANotifier: TSCDeNotifier);
var
  Index: Integer;
begin
  if (ANotifier <> nil) and (FNotificationList <> nil) then
  begin
    Index := FNotificationList.IndexOf(ANotifier);
    if Index > -1 then
    begin
      FNotificationList.Delete(Index);
      if FNotificationList.Count = 0 then
        FreeAndNil(FNotificationList);
    end;
  end;
end;

procedure TSCDeCustomControl.BorderChanged;
begin
  //
end;

procedure TSCDeCustomControl.ScrollerPositionChanged(
  Sender: TSCDeCustomControlScrollbar; OldPos, NewPos: Integer);
begin
  //
end;

procedure TSCDeCustomControl.WMPaint(var Message: TWMPaint);
var
  CR, R: TRect;
begin
  if HandleAllocated and (Message.DC <> 0) and (csPaintCopy in ControlState) then
  begin
    Windows.GetWindowRect(Handle, R);
    Windows.GetClientRect(Handle, CR);

    CR.TopLeft := ClientToScreen(CR.TopLeft);
    CR.BottomRight := ClientToScreen(CR.BottomRight);

    OffsetRect(CR, -R.Left, -R.Top);
    MoveWindowOrg(Message.DC, CR.Left, CR.Top);

    OffsetRect(CR, -CR.Left, -CR.Top);
    IntersectClipRect(Message.DC, 0, 0, CR.Right, CR.Bottom);

    inherited;

    MoveWindowOrg(Message.DC, -CR.Left, -CR.Top);
  end else
    inherited;
end;

function TSCDeCustomControl.GetPicturePropsClass: TSCDeCustomPicturePropsClass;
begin
  Result := TSCDePictureProps;
end;

procedure TSCDeCustomControl.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TSCDeCustomControl.SetPictureIndent(Value: Integer);
begin
  if FPictureIndent <> Value then
  begin
    FPictureIndent := Value;
    if HasPicture then
      PictureChanged(nil);
  end;
end;

procedure TSCDeCustomControl.SetPictureOrient(Value: TSCDePictureOrient);
begin
  if FPictureOrient <> Value then
  begin
    FPictureOrient := Value;
    if HasPicture then
      PictureChanged(nil);
  end;
end;

procedure TSCDeCustomControl.SetPictureProps(Value: TSCDeCustomPictureProps);
begin

end;

procedure TSCDeCustomControl.SetPictureTopIndent(Value: Integer);
begin
  if FPictureTopIndent <> Value then
  begin
    FPictureTopIndent := Value;
    if HasPicture then
      PictureChanged(nil);
  end;
end;

procedure TSCDeCustomControl.SetPictureTransparent(Value: Boolean);
begin
  if FPictureTransparent <> Value then
  begin
    FPictureTransparent := Value;
    if HasPicture then
      PictureChanged(nil);
  end;
end;

procedure TSCDeCustomControl.SetShowPicture(Value: Boolean);
begin
  if FShowPicture <> Value then
  begin
    FShowPicture := Value;
    PictureChanged(nil);
  end;
end;

procedure TSCDeCustomControl.PictureChanged(Sender: TObject);
begin
  if not FDrawingPicture then Invalidate;
  DoPictureChanged;
  if Assigned(FOnPictureChange) then
    FOnPictureChange(Self);
end;

procedure TSCDeCustomControl.DoPictureChanged;
begin
  //
end;

function TSCDeCustomControl.GetPicture: TPicture;
begin
  Result := FPicture;
  if PictureList <> nil then
    Result := PictureList.GetPicture(PictureIndex);
end;

function TSCDeCustomControl.HasPicture(P: TPicture): Boolean;
begin
  Result := FShowPicture and (P <> nil) and (P.Graphic <> nil) and
    not P.Graphic.Empty and (P.Height > 0) and (P.Width > 0);
end;

function TSCDeCustomControl.GetPictureRect: TRect;
begin
  Result := GetPictureRect(GetPicture);
end;

procedure TSCDeCustomControl.DrawPicture(C: TCanvas);
var
  R: TRect;
  P: TPicture;
  DrawPict: TPicture;
  SR, I, J, X, Y, L, T: Integer;
begin
  P := GetPicture;
  if (P = nil) or FDrawingPicture or (C = nil) or
    not CanDrawPicture(P) then
    Exit;

  FDrawingPicture := True;
  try
    R := GetPictureRect;
    if IsRectEmpty(R) then
      Exit;

    SR := IntersectClipRect(C.Handle, R.Left, R.Top, R.Right, R.Bottom);
    try
      if SR <> NULLREGION then
      begin
        DrawPict := TPicture.Create;
        try
          DrawPict.Assign(P);

          P := DrawPict;
          P.Graphic.Transparent := FPictureTransparent;

          case FPictureOrient of
            scdpoTopLeft:
              C.Draw(R.Left, R.Top, P.Graphic);
            scdpoTopRight:
            begin
              L := R.Right - P.Width;
              C.Draw(L, R.Top, P.Graphic);
            end;
            scdpoBottomLeft:
            begin
              T := R.Bottom - P.Height;
              C.Draw(R.Left, T, P.Graphic);
            end;
            scdpoBottomRight:
            begin
              L := R.Right - P.Width;
              T := R.Bottom - P.Height;

              C.Draw(L, T, P.Graphic);
            end;
            scdpoCenter:
            begin
              L := R.Left + ((R.Right - R.Left - P.Width) div 2);
              T := R.Top + ((R.Bottom - R.Top - P.Height) div 2);

              C.Draw(L, T, P.Graphic);
            end;
            scdpoTiled:
            begin
              X := (R.Right - R.Left) div P.Width;
              if (R.Right - R.Left) mod P.Width <> 0 then
                Inc(X);

              Y := (R.Bottom - R.Top) div P.Height;
              if (R.Bottom - R.Top) mod P.Height <> 0 then
                Inc(Y);

              L := R.Left;
              for I := 1 to X do
              begin
                T := R.Top;

                for J := 1 to Y do
                begin
                  C.Draw(L, T, P.Graphic);
                  Inc(T, P.Height);
                end;

                Inc(L, P.Width);
              end;
            end;
          end;
        finally
          DrawPict.Free;
        end;
      end;
    finally
      SelectClipRgn(C.Handle, 0);
    end;
  finally
    FDrawingPicture := False;
  end;
end;

function TSCDeCustomControl.CanDrawPicture(P: TPicture): Boolean;
begin
  Result := FShowPicture and not FDrawingPicture and HasPicture(P);
end;

function TSCDeCustomControl.HasPicture: Boolean;
begin
  Result := HasPicture(GetPicture);
end;

function TSCDeCustomControl.CanDrawPicture: Boolean;
begin
  Result := CanDrawPicture(GetPicture);
end;

function TSCDeCustomControl.GetPictureRect(P: TPicture): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FShowPicture then
  begin
    Result := GetClientRect;
    InflateRect(Result, -FPictureIndent, -FPictureTopIndent);
  end;
end;

procedure TSCDeCustomControl.WMChar(var Message: TWMChar);
var
  Shift: TShiftState;
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Shift := KeyboardStateToShiftState(KeyState);

  if CanDoNextControl(Message.CharCode, Shift) then
  begin
    Message.Result := 0;
    Message.CharCode := VK_TAB;
  end else
    inherited;
end;

function TSCDeCustomControl.CanDoNextControl(CharCode: Word; Shift: TShiftState): Boolean;
begin
  Result := SendMessage(Self.Handle, CM_SCDEJUMPTONEXT,
    CharCode, scdShiftStateToKeys(Shift)) = 1;
end;

procedure TSCDeCustomControl.CNChar(var Message: TWMChar);
var
  Shift: TShiftState;
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Shift := KeyboardStateToShiftState(KeyState);

  if CanDoNextControl(Message.CharCode, Shift) then
  begin
    Message.Result := 0;
    Message.CharCode := VK_TAB;
  end else
    inherited;
end;

procedure TSCDeCustomControl.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if AControl = nil then
    Invalidate;
end;

{ TSCDeCustomSizableControl }

procedure TSCDeCustomSizableControl.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TSCDeCustomSizableControl then
  begin
    with TSCDeCustomSizableControl(Source) do
    begin
      Self.ShowSizeGrip := ShowSizeGrip;
      Self.ShowStatusbar := ShowStatusbar;
      Self.StatusbarAlignment := StatusbarAlignment;
      Self.StatusbarColor := StatusbarColor;
      Self.StatusbarText := StatusbarText;
    end;
  end;
end;

procedure TSCDeCustomSizableControl.CalculateBorder(var R: TRect);
var
  SH: Integer;
begin
  inherited CalculateBorder(R);

  if FShowStatusbar then
  begin
    SH := GetStatusbarHeight;
    if SH < 0 then SH := 0;

    Dec(R.Bottom, SH);
  end;
end;

constructor TSCDeCustomSizableControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowSizeGrip := True;
  FStatusbarAlignment := taLeftJustify;
  FStatusbarColor := clBtnFace;
end;

function TSCDeCustomSizableControl.GetInheritedNCArea(P: TPoint): LongInt;
begin
  Result := inherited GetInheritedNCArea(P);

  if FShowStatusbar and (Result = SCDE_HTNONE) then
    Result := GetStatusbarPartAtPos(P);
end;

function TSCDeCustomSizableControl.GetStatusbarHeight: Integer;
begin
  Result := 16;
end;

function TSCDeCustomSizableControl.GetStatusbarPartAtPos(P: TPoint): LongInt;
var
  R, CR: TRect;
  B, SH: Integer;
begin
  Result := SCDE_HTNONE;

  if FShowStatusbar and (Result = SCDE_HTNONE) and
    HandleAllocated and GetWindowRect(Handle, R) then
  begin
    SH := GetStatusbarHeight;

    if SH > 0 then
    begin
      B := GetBorderSize + GetInnerBorderSize + BorderWidth;

      CR := R;
      if B > 0 then
      begin
        InflateRect(CR, -B, -B);
        if IsRectEmpty(CR) then
          Exit;
      end;

      CR.Top := CR.Bottom - SH;
      IntersectRect(CR, CR, R);

      if not IsRectEmpty(CR) and PtInRect(CR, P) then
      begin
        Result := SCDE_HTSTATUSBAR;

        if FShowSizeGrip then
        begin
          CR.Left := CR.Right - SH;
          IntersectRect(CR, CR, R);

          if not IsRectEmpty(CR) and PtInRect(CR, P) then
            Result := SCDE_HTSIZEGRIP;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomSizableControl.RedrawBorder(const Clip: HRGN; OnDC: HDC);
var
  DC: HDC;
  R, TxR: TRect;
  I, SH, TxtFormat: Integer;
  FrameCanvas: TCanvas;
begin
  inherited RedrawBorder(Clip, OnDC);

  if not (FShowStatusbar and HandleAllocated) or
    IsIconic(Handle) or (csDestroying in ComponentState) then
    Exit;

  DC := OnDC;
  if OnDC = 0 then
    DC := GetWindowDC(Handle);

  try
    SH := GetStatusbarHeight;
    if SH < 0 then SH := 0;

    if SH > 0 then
    begin
      R := Rect(0, 0, Width, Height);
      inherited CalculateBorder(R);

      InflateRect(R, -BorderWidth, -BorderWidth);

      if not IsRectEmpty(R) then
      begin
        if R.Bottom - R.Top < SH then
          SH := R.Bottom - R.Top;

        R.Top := R.Bottom - SH;
        if IsRectEmpty(R) then
          Exit;

        IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
        try
          FrameCanvas := TCanvas.Create;
          try
            with FrameCanvas do
            begin
              Handle := DC;
              Brush.Color := FStatusbarColor;

              FillRect(R);

              TxR := R;
              InflateRect(TxR, -2, -1);

              if FShowSizeGrip then
                Dec(TxR.Right, GetStatusbarHeight);

              if not IsRectEmpty(TxR) then
              begin
                TxtFormat := DT_SINGLELINE or DT_EXPANDTABS or DT_LEFT or
                  DT_VCENTER or DT_NOPREFIX or DT_END_ELLIPSIS;

                if FStatusbarAlignment = taRightJustify then
                  TxtFormat := TxtFormat and not DT_LEFT or DT_RIGHT
                else
                if FStatusbarAlignment = taCenter then
                  TxtFormat := TxtFormat and not DT_LEFT or DT_CENTER;

                DrawText(Handle, PChar(FStatusbarText), Length(FStatusbarText),
                  TxR, TxtFormat);
              end;

              InflateRect(R, -2, -1);

              if FShowSizeGrip then
              begin
                I := 10;

                while I > 0 do
                begin
                  Pen.Color := FStatusbarColor;
                  if Odd(I div 2) then
                    Pen.Color := scdGetBtnShadowOf(FStatusbarColor);

                  MoveTo(R.Right, R.Bottom - I);
                  LineTo(R.Right - I, R.Bottom);
                  
                  MoveTo(R.Right, R.Bottom - (I - 1));
                  LineTo(R.Right - (I - 1), R.Bottom);

                  Dec(I, 2);
                end;
              end;

              (*
              Pen.Color := scdGetBtnShadowOf(FStatusbarColor);

              InflateRect(R, 2, 1);
              MoveTo(R.Left, R.Top);
              LineTo(R.Right, R.Top);
              *)
            end;
          finally
            FrameCanvas.Free;
          end;
        finally
          SelectClipRgn(DC, 0);
        end;
      end;
    end;
  finally
    if (DC <> 0) and (OnDC = 0) then
      ReleaseDC(Handle, DC);
  end;
end;

procedure TSCDeCustomSizableControl.SetShowSizeGrip(Value: Boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;

    if FShowStatusbar then
    begin
      RedrawScrollbars;
      RedrawBorder(0);
    end;
  end;
end;

procedure TSCDeCustomSizableControl.SetShowStatusbar(Value: Boolean);
begin
  if FShowStatusbar <> Value then
  begin
    FShowStatusbar := Value;
    if HandleAllocated then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOZORDER or
        SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);

     StatusbarChanged;   
  end;
end;

procedure TSCDeCustomSizableControl.SetStatusbarAlignment(Value: TAlignment);
begin
  if FStatusbarAlignment <> Value then
  begin
    FStatusbarAlignment := Value;
    if FShowStatusbar then
      RedrawBorder(0);
  end;
end;

procedure TSCDeCustomSizableControl.SetStatusbarColor(Value: TColor);
begin
  if FStatusbarColor <> Value then
  begin
    FStatusbarColor := Value;
    if FShowStatusbar then
      RedrawBorder(0);
  end;
end;

procedure TSCDeCustomSizableControl.SetStatusbarText(const Value: TCaption);
begin
  if FStatusbarText <> Value then
  begin
    FStatusbarText := Value;
    if FShowStatusbar then
      RedrawBorder(0);
  end;
end;

procedure TSCDeCustomSizableControl.StatusbarChanged;
begin
  //
end;

procedure TSCDeCustomSizableControl.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  R1, R2: TRect;
  SH, W, H: Integer;
begin
  inherited;

  if FShowStatusbar then
  begin
    R1 := Rect(0, 0, Width, Height);

    R2 := R1;
    inherited CalculateBorder(R2);

    InflateRect(R2, -BorderWidth, -BorderWidth);

    OffsetRect(R2, -R2.Left, -R2.Top);

    SH := GetStatusbarHeight;
    if SH < 0 then SH := 0;

    W := R1.Right - R2.Right;

    if W < 0 then W := 0;
    Inc(W, SH);

    H := R1.Bottom - R2.Bottom;

    if H < 0 then H := 0;
    Inc(H, SH);

    with Message.MinMaxInfo^ do
    begin
      // min sizing
      if Constraints.MinWidth > W then
        W := Constraints.MinWidth;

      if Constraints.MinHeight > H then
        H := Constraints.MinHeight;

      if ptMinTrackSize.x > W then
        W := ptMinTrackSize.x;
        
      if ptMinTrackSize.y > H then
        H := ptMinTrackSize.y;

      ptMinTrackSize := Point(W, H);

      // max sizing
      H := Constraints.MaxHeight;
      if ptMaxTrackSize.y > H then
        H := ptMaxTrackSize.y;

      W := Constraints.MaxWidth;
      if ptMaxTrackSize.x > W then
        W := ptMaxTrackSize.x;

      ptMaxTrackSize := Point(W, H);
    end;
  end;
end;

procedure TSCDeCustomSizableControl.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;

  if not (csDesigning in ComponentState) and
    FShowStatusbar and FShowSizeGrip then
  begin
    if Message.Result = HTBOTTOMRIGHT then
      Message.Result := HTCLIENT;

    P := SmallPointToPoint(Message.Pos);
    if GetStatusbarPartAtPos(P) = SCDE_HTSIZEGRIP then
       Message.Result := HTBOTTOMRIGHT;
  end;
end;

procedure TSCDeCustomSizableControl.WMSize(var Message: TWMSize);
begin
  inherited;
  if FShowStatusbar then
  begin
    RedrawScrollbars;
    RedrawBorder(0);
  end;
end;

{ TSCDeCustomControlScrollbar }

procedure TSCDeCustomControlScrollbar.Assign(Source: TPersistent);
begin
  if Source is TSCDeCustomControlScrollbar then
  begin
    BeginUpdate;
    try
      with TSCDeCustomControlScrollbar(Source) do
      begin
        Self.Visible := Visible;

        Self.Background := Background;
        Self.ButtonColors := ButtonColors;
        Self.ButtonExtra := ButtonExtra;
        Self.ButtonLeft := ButtonLeft;
        Self.ButtonRight := ButtonRight;
        Self.ButtonSize := ButtonSize;
        Self.Enabled := Enabled;
        Self.Icons := Icons;
        Self.LargeChange := LargeChange;
        Self.Max := Max;
        Self.Min := Min;
        Self.PageSize := PageSize;
        Self.Position := Position;
        Self.SmallChange := SmallChange;
        Self.Style := Style;
        Self.Thumb := Thumb;
        Self.ThumbSize := ThumbSize;
        Self.Track := Track;
      end;
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TSCDeCustomControlScrollbar.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FWasVisible := FVisible;
    FUpdatePos := FPosition;
  end;

  Inc(FUpdateCount);
end;

procedure TSCDeCustomControlScrollbar.BufferedPaint(DC: HDC; R: TRect);
var
  CR: TRect;
  MemDC: HDC;
  W, H: Integer;
  MemBitmap, OldBitmap: HBITMAP;
begin
  if (DC = 0) or IsRectEmpty(R) then
    Exit;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  MemDC := CreateCompatibleDC(DC);
  try
    MemBitmap := CreateCompatibleBitmap(DC, W, H);
    try
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        CR := R;
        OffsetRect(CR, -CR.Left, -CR.Top);

        PaintOn(MemDC, CR);
        BitBlt(DC, R.Left, R.Top, W, H, MemDC, 0, 0, SRCCOPY);
      finally
        SelectObject(MemDC, OldBitmap);
      end;
    finally
      DeleteObject(MemBitmap);
    end;
  finally
    DeleteDC(MemDC);
  end;
end;

constructor TSCDeCustomControlScrollbar.Create(AOwner: TSCDeCustomControl;
  AKind: TSCDeScrollbarKind);
begin
  inherited Create;
  FOwner := AOwner;
  FKind  := AKind;

  FBorderColor := clWindowFrame;
  FButtonSize := -1;
  FButtonLayout := scdsbDefault;
  FEnabled := True;
  FLineDiv := 4;
  FPageDiv := 12;
  FDelay := 10;
  FLargeChange := 1;
  FMax := 100;
  FMin := 0;
  FPageSize := 0;
  FPosition := 0;
  FSensitivity := -1;
  FSmallChange := 1;
  FStyle := scssDefault;
  FThumbLines := sctlNone;
  FThumbSize := -1;
  FTrack := True;
  FTrimPageSize := True;
  FVisible := True;

  FBackground   := GetScrollbarPartClass(scdsbpBkground).Create(Self);
  FButtonColors := GetScrollbarPartClass(scdsbpButtons).Create(Self);
  FButtonExtra  := GetScrollbarPartClass(scdsbpExtraButton).Create(Self);
  FButtonLeft   := GetScrollbarPartClass(scdsbpLeftButton).Create(Self);
  FButtonRight  := GetScrollbarPartClass(scdsbpRightButton).Create(Self);
  FIcons := GetScrollbarPartClass(scdsbpIcon).Create(Self);
  FThumb := GetScrollbarPartClass(scdsbpThumb).Create(Self);

  FButtonExtra.Visible := False;
  RearrangeDefaults;
end;

destructor TSCDeCustomControlScrollbar.Destroy;
begin
  if FOwner <> nil then
    FOwner.ScrollerDestroyed(Self);

  FBackground.Free;
  FButtonColors.Free;
  FButtonLeft.Free;
  FButtonExtra.Free;
  FButtonRight.Free;
  FIcons.Free;
  FThumb.Free;

  inherited Destroy;
end;

procedure TSCDeCustomControlScrollbar.DoChange;
begin
  if not InUpdate and (FOwner <> nil) then
    FOwner.ScrollerChanged(Self);
end;

procedure TSCDeCustomControlScrollbar.DoPositionChanged;
begin
  if not (InUpdate or PosChangeLocked) and (FOwner <> nil) then
    FOwner.ScrollerPositionChanged(Self);
end;

procedure TSCDeCustomControlScrollbar.DoPositionChanging(
  var ScrollPos: Integer; var CanScroll: Boolean);
var
  Mx: Integer;
begin
  if (FPosition <> ScrollPos) and (FOwner <> nil) and
    not (InUpdate or PosChangeLocked) then
  begin
    FOwner.ScrollerPositionChanging(Self, ScrollPos, CanScroll);
    
    if CanScroll then
    begin
      Mx := GetMaximumValue;

      if ScrollPos > Mx then ScrollPos := Mx;
      if ScrollPos < FMin then ScrollPos := FMin;
    end;
  end;
end;

procedure TSCDeCustomControlScrollbar.DoVisibleChanged;
begin
  if not InUpdate and (FOwner <> nil) then
    FOwner.ScrollerVisibleChanged(Self);
end;

procedure TSCDeCustomControlScrollbar.EndUpdate;
var
  P: Integer;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);

    if FUpdateCount = 0 then
    begin
      if FWasVisible <> FVisible then
        DoVisibleChanged;

      FWasVisible := FVisible;

      DoChange;
      DoRangeChanged;

      P := FUpdatePos;
      FUpdatePos := FPosition;

      DoPositionChanged;
      DoPositionChanged(P, FPosition);
    end;
  end;
end;

function TSCDeCustomControlScrollbar.GetBackColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    if PressedPart in [scspLeftSpare, scspRightSpare] then
    begin
      if HotPart <> scspNone then
        Result := scdGetOfficeXPSelColor
      else
        Result := scdGetOfficeXPDownedSelColor;
    end else
    if (PressedPart = scspNone) and
      (HotPart in [scspLeftSpare, scspRightSpare]) then
      Result := scdGetOfficeXPSelColor
    else
      Result := scdGetOfficeXPBtnColor;

    if FBackground.FBlend then
      Result := scdBlendedColor(Result, 24, 24, 24, True);
  end else
  begin
    Result := FBackground.FColor;
    if not Enabled then
      Result := FBackground.FDisabledColor
    else
    if (PressedPart = scspNone) and
      (HotPart in [scspLeftSpare, scspRightSpare]) then
      Result := FBackground.FHotColor;

    if Result = clNone then
      Result := clScrollBar;

    if FBackground.FBlend then
      Result := scdBlendedColor(scdGetOfficeXPBtnColorOf(Result), 24, 24, 24, True);  
  end;
end;

function TSCDeCustomControlScrollbar.GetBackRect(R: TRect): TRect;
var
  Rr, Rl, Re: TRect;
  REmpty, LEmpty, EEmpty: Boolean;
begin
  Result := Rect(0, 0, 0, 0);
  if IsRectEmpty(R) then
    Exit;

  Result := R;
  if (FButtonSize <> 0) and (FButtonLeft.Visible or
    FButtonRight.Visible or FButtonExtra.Visible) then
  begin
    Rl := GetLeftButtonRect(R);
    Rr := GetRightButtonRect(R);
    Re := GetExtraButtonRect(R);

    LEmpty := IsRectEmpty(Rl);
    REmpty := IsRectEmpty(Rr);
    EEmpty := IsRectEmpty(Re);

    if FKind = scdskHorizontal then
    begin
      case FButtonLayout of
        scdsbDefault:
        begin
          if not LEmpty and (Rl.Right > Result.Left) then
            Result.Left := Rl.Right;

          if not REmpty and (Rr.Left < Result.Right) then
            Result.Right := Rr.Left;

          if not EEmpty and (Re.Left < Result.Right) then
            Result.Right := Re.Left;
        end;
        scdsbLeftTop:
        begin
          if not LEmpty and (Rl.Right > Result.Left) then
            Result.Left := Rl.Right;

          if not REmpty and (Rr.Right > Result.Left) then
            Result.Left := Rr.Right;

          if not EEmpty and (Re.Left < Result.Right) then
            Result.Right := Re.Left;
        end;
        scdsbRightBottom:
        begin
          if not FButtonExtra.Visible then
          begin
            if not LEmpty and (Rl.Left < Result.Right) then
              Result.Right := Rl.Left;
          end else
          begin
            if not LEmpty and (Rl.Right > Result.Left) then
              Result.Left := Rl.Right;

            if not EEmpty and (Re.Left < Result.Right) then
              Result.Right := Re.Left;
          end;

          if not REmpty and (Rr.Left < Result.Right) then
            Result.Right := Rr.Left;
        end;
      end;
    end else
    begin
      case FButtonLayout of
        scdsbDefault:
        begin
          if not LEmpty and (Rl.Bottom > Result.Top) then
            Result.Top := Rl.Bottom;

          if not REmpty and (Rr.Top < Result.Bottom) then
            Result.Bottom := Rr.Top;

          if not EEmpty and (Re.Top < Result.Bottom) then
            Result.Bottom := Re.Top;
        end;
        scdsbLeftTop:
        begin
          if not LEmpty and (Rl.Bottom > Result.Top) then
            Result.Top := Rl.Bottom;

          if not REmpty and (Rr.Bottom > Result.Top) then
            Result.Top := Rr.Bottom;

          if not EEmpty and (Re.Top < Result.Bottom) then
            Result.Bottom := Re.Top;
        end;
        scdsbRightBottom:
        begin
          if not FButtonExtra.Visible then
          begin
            if not LEmpty and (Rl.Top < Result.Bottom) then
              Result.Bottom := Rl.Top;
          end else
          begin
            if not LEmpty and (Rl.Bottom > Result.Top) then
              Result.Top := Rl.Bottom;

            if not EEmpty and (Re.Top < Result.Bottom) then
              Result.Bottom := Re.Top;
          end;

          if not REmpty and (Rr.Top < Result.Bottom) then
            Result.Bottom := Rr.Top;
        end;
      end;
    end;

    if IsRectEmpty(Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TSCDeCustomControlScrollbar.GetButtonSize(R: TRect): Integer;
var
  W, H, M: Integer;
begin
  Result := 0;
  if IsRectEmpty(R) then
    Exit;

  Result := FButtonSize;

  M := 0;
  if ButtonLeft.Visible then Inc(M);
  if ButtonRight.Visible then Inc(M);
  if ButtonExtra.Visible then Inc(M);

  if M = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if Result = -1 then
  begin
    if FKind = scdskHorizontal then
      Result := scdHorzScrollButtonWidth
    else
      Result := scdVertScrollButtonHeight;
  end;

  if Result < 0 then Result := 0;

  if FKind = scdskHorizontal then
  begin
    W := (R.Right - R.Left) div M;
    if W < Result then Result := W;
  end else
  begin
    H := (R.Bottom - R.Top) div M;
    if H < Result then Result := H;
  end;

  if Result < 3 then Result := 0;
end;

function TSCDeCustomControlScrollbar.GetClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  
  if FOwner <> nil then
  begin
    if FKind = scdskHorizontal then
      Result := FOwner.GetHorzScrollbarRect
    else
      Result := FOwner.GetVertScrollbarRect;
  end;
end;

function TSCDeCustomControlScrollbar.GetDefaultThumbSize(R: TRect;
  Pg: Integer): Integer;
var
  Bh, Bs, M: Integer;
begin
  Result := 0;
  if IsRectEmpty(R) then
    Exit;

  if Pg <= 0 then
  begin
    if FKind = scdskHorizontal then
      Result := FDefV_ButtonSize
    else
      Result := FDefH_ButtonSize;
  end else
  begin
    Bh := 0;
    M  := 0;

    if ButtonLeft.Visible then Inc(M);
    if ButtonRight.Visible then Inc(M);
    if ButtonExtra.Visible then Inc(M);

    if M > 0 then
    begin
      Bh := GetButtonSize(R);
      if Bh < 0 then Bh := 0;

      Bh := M*Bh;
    end;

    if FKind = scdskHorizontal then
      Bs := R.Right - R.Left
    else
      Bs := R.Bottom - R.Top;

    Dec(Bs, Bh);   

    if Bs <= 0 then
    begin
      Result := 0;
      Exit;
    end;

    M := Max - Min + 1;
    if M = 0 then Result := 0
    else Result := MulDiv(Pg, Bs, M);

    if FKind = scdskHorizontal then
      Bs := Muldiv(2, FDefV_ButtonSize, 3)
    else
      Bs := Muldiv(2, FDefH_ButtonSize, 3);

    if Result < Bs then
      Result := Bs;
  end;

  if Result < 3 then Result := 3;
end;

function TSCDeCustomControlScrollbar.GetDownPoint: TPoint;
begin
  Result := Point(0, 0);
  if FOwner is TSCDeCustomScrollControl then
    Result := TSCDeCustomScrollControl(FOwner).FDownPoint;
end;

function TSCDeCustomControlScrollbar.GetHotPart: TSCDeScrollerHitPart;
var
  P1, P2: PSCDeScrollbarHittest;
  AOwner: TSCDeCustomScrollControl;
begin
  Result := scspNone;
  if FOwner is TSCDeCustomScrollControl then
  begin
    AOwner := TSCDeCustomScrollControl(FOwner);

    P1 := @(AOwner.FHotTest);
    P2 := @(AOwner.FDownTest);

    if ((P2^.Kind in [scshkNone, scshkVertical]) and (Kind = scdskVertical) and
      (P1^.Kind = scshkVertical)) or ((P2^.Kind in [scshkNone, scshkHorizontal]) and
      (Kind = scdskHorizontal) and (P1^.Kind = scshkHorizontal)) then
      Result := P1^.HitPart;
  end;
end;

function TSCDeCustomControlScrollbar.GetLeftBtnColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    if PressedPart = scspLeftButton then
    begin
      if HotPart = scspLeftButton then
        Result := scdGetOfficeXPDownedSelColor
      else
        Result := scdGetOfficeXPSelColor;
    end else
    if (PressedPart = scspNone) and (HotPart = scspLeftButton) then
      Result := scdGetOfficeXPSelColor
    else
      Result := scdGetOfficeXPBtnColor;
  end else
  begin
    Result := FButtonColors.FColor;
    if not Enabled then
      Result := FButtonColors.FDisabledColor
    else
    if PressedPart = scspLeftButton then
      Result := FButtonColors.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspLeftButton) then
      Result := FButtonColors.FHotColor;

    if Result = clNone then
      Result := clScrollBar;
  end;

  if FButtonColors.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetLeftBtnIconColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    Result := clBtnText;
    if (PressedPart = scspLeftButton) and (HotPart = PressedPart) then
      Result := clHighlightText;
  end else
  begin
    Result := FIcons.FColor;
    if not Enabled then
      Result := FIcons.FDisabledColor
    else
    if PressedPart = scspLeftButton then
      Result := FIcons.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspLeftButton) then
      Result := FIcons.FHotColor;

    if Result = clNone then
      Result := clWindowText;
  end;    

  if FIcons.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetLeftButtonRect(R: TRect): TRect;
var
  Bs: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FButtonSize = 0) or not FButtonLeft.Visible or IsRectEmpty(R) then
    Exit;

  Result := R;
  Bs := GetButtonSize(R);

  if FKind = scdskHorizontal then
  begin
    if (FButtonLayout in [scdsbDefault, scdsbLeftTop]) or FButtonExtra.Visible then
      Result.Right := Result.Left + Bs
    else begin
      Dec(Result.Right, Bs);
      Result.Left := Result.Right - Bs;
    end;
  end else
  begin
    if (FButtonLayout in [scdsbDefault, scdsbLeftTop]) or FButtonExtra.Visible then
      Result.Bottom := Result.Top + Bs
    else begin
      Dec(Result.Bottom, Bs);
      Result.Top := Result.Bottom - Bs;
    end;
  end;
end;

function TSCDeCustomControlScrollbar.GetPositionPos(R: TRect; P: Integer): Integer;
var
  W, ThS, Mx: Integer;
begin
  Result := 0;
  if IsRectEmpty(R) then
    Exit;

  Mx := GetMaximumValue;

  if FKind = scdskHorizontal then
  begin
    if P <= FMin then
      Result := R.Left
    else begin
      ThS := GetThumbSize(R);
      if ThS < 0 then ThS := 0;

      W := R.Right - R.Left - ThS;

      if P >= FMin then
      begin
        Result := R.Left + W;
        Exit;
      end;

      P := Muldiv(W, P, Mx - FMin);
      Result := R.Left + P;
    end;
  end else
  begin
    if P <= FMin then
      Result := R.Top
    else begin
      ThS := GetThumbSize(R);
      if ThS < 0 then ThS := 0;

      W := R.Bottom - R.Top - ThS;

      if P >= FMin then
      begin
        Result := R.Top + W;
        Exit;
      end;

      P := Muldiv(W, P, Mx - FMin);
      Result := R.Top + P;
    end;
  end;
end;

function TSCDeCustomControlScrollbar.GetPressedPart: TSCDeScrollerHitPart;
var
  P: PSCDeScrollbarHittest;
  AOwner: TSCDeCustomScrollControl;
begin
  Result := scspNone;
  if FOwner is TSCDeCustomScrollControl then
  begin
    AOwner := TSCDeCustomScrollControl(FOwner);

    P := @(AOwner.FDownTest);

    if ((Kind = scdskVertical) and (P^.Kind = scshkVertical)) or
      ((Kind = scdskHorizontal) and (P^.Kind = scshkHorizontal)) then
      Result := P^.HitPart;
  end;
end;

function TSCDeCustomControlScrollbar.GetRightBtnColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    if PressedPart = scspRightButton then
    begin
      if HotPart = scspRightButton then
        Result := scdGetOfficeXPDownedSelColor
      else
        Result := scdGetOfficeXPSelColor;
    end else
    if (PressedPart = scspNone) and (HotPart = scspRightButton) then
      Result := scdGetOfficeXPSelColor
    else
      Result := scdGetOfficeXPBtnColor;
  end else
  begin
    Result := FButtonColors.FColor;
    if not Enabled then
      Result := FButtonColors.FDisabledColor
    else
    if PressedPart = scspRightButton then
      Result := FButtonColors.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspRightButton) then
      Result := FButtonColors.FHotColor;

    if Result = clNone then
      Result := clScrollBar;
  end;

  if FButtonColors.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetRightBtnIconColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    Result := clBtnText;
    if (PressedPart = scspRightButton) and (HotPart = PressedPart) then
      Result := clHighlightText;
  end else
  begin
    Result := FIcons.FColor;
    if not Enabled then
      Result := FIcons.FDisabledColor
    else
    if PressedPart = scspRightButton then
      Result := FIcons.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspRightButton) then
      Result := FIcons.FHotColor;

    if Result = clNone then
      Result := clWindowText;
  end;    

  if FIcons.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetRightButtonRect(R: TRect): TRect;
var
  Bs: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FButtonSize = 0) or not FButtonLeft.Visible or IsRectEmpty(R) then
    Exit;

  Result := R;
  Bs := GetButtonSize(R);

  if FKind = scdskHorizontal then
  begin
    if FButtonLayout in [scdsbDefault, scdsbRightBottom] then
      Result.Left := Result.Right - Bs
    else begin
      Inc(Result.Left, Bs);
      Result.Right := Result.Left + Bs;
    end;
  end else
  begin
    if FButtonLayout in [scdsbDefault, scdsbRightBottom] then
      Result.Top := Result.Bottom - Bs
    else begin
      Inc(Result.Top, Bs);
      Result.Bottom := Result.Top + Bs;
    end;
  end;
end;

function TSCDeCustomControlScrollbar.GetScrollbarPartClass(
  Part: TSCDeScrollerPart): TSCDeScrollbarPartClass;
begin
  Result := nil;
  case Part of
    scdsbpBkground:
      Result := TSCDeScrollbarBkground;
    scdsbpIcon:
      Result := TSCDeScrollbarIcons;
    scdsbpButtons:
      Result := TSCDeScrollbarButtonColors;
    scdsbpExtraButton,
    scdsbpLeftButton,
    scdsbpRightButton:
      Result := TSCDeScrollbarButton;
    scdsbpThumb:
      Result := TSCDeScrollbarThumb;
  end;
end;

function TSCDeCustomControlScrollbar.GetThumbColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    if PressedPart = scspThumb then
    begin
      if HotPart = scspThumb then
        Result := scdGetOfficeXPDownedSelColor
      else
        Result := scdGetOfficeXPSelColor;
    end else
    if (PressedPart = scspNone) and (HotPart = scspThumb) then
      Result := scdGetOfficeXPSelColor
    else
      Result := scdGetOfficeXPBtnColor;
  end else
  begin
    Result := FThumb.FColor;
    if not Enabled then
      Result := FThumb.FDisabledColor
    else
    if PressedPart = scspThumb then
      Result := FThumb.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspThumb) then
      Result := FThumb.FHotColor;

    if Result = clNone then
      Result := clScrollBar;
  end;    

  if FThumb.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetThumbMoving: Boolean;
var
  AOwner: TSCDeCustomScrollControl;
begin
  Result := False;
  if FOwner is TSCDeCustomScrollControl then
  begin
    AOwner := TSCDeCustomScrollControl(FOwner);

    Result := FEnabled and (FMin < GetMaximumValue) and (FThumbSize <> 0) and
      (AOwner.FDownPoint.x > -1) and (AOwner.FDownPoint.y > -1) and
      AOwner.Enabled and AOwner.FMovingThumb and (GetPressedPart = scspThumb);
  end;
end;

function TSCDeCustomControlScrollbar.GetThumbOffset(R: TRect): Integer;
var
  ThS: Integer;
begin
  Result := 0;
  if not IsRectEmpty(R) and ((FThumbSize > 0) or (PageSize > 0)) then
  begin
    ThS := GetDefaultThumbSize(R, PageSize);
    if ThS < 0 then ThS := 0;

    if FThumbSize > 0 then
      Result := FThumbSize - ThS
    else
      Result := ThS - GetDefaultThumbSize(R, 0);

    Result := Round(Result / 2);  
    if Result < 0 then Result := 0;
  end;
end;

function TSCDeCustomControlScrollbar.GetThumbRect(R: TRect): TRect;
var
  R1, R2: TRect;
  W, P, S, Ts, Mx: Integer;
  ThumbMoving: Boolean;
  AOwner: TSCDeCustomScrollControl;
begin
  Result := Rect(0, 0, 0, 0);
  if IsRectEmpty(R) or not (Enabled and FThumb.Visible) then
    Exit;

  R1 := GetBackRect(R);

  Mx := GetMaximumValue;

  S := Mx - FMin;
  if S <= 0 then Exit;

  Ts := GetThumbSize(R);
  P  := FPosition - FMin;

  ThumbMoving := GetThumbMoving;

  AOwner := nil;
  if FOwner is TSCDeCustomScrollControl then
    AOwner := TSCDeCustomScrollControl(FOwner);

  if ThumbMoving and (AOwner <> nil) then
    P := AOwner.FDownPosition - FMin;

  R2 := R1;
  if FKind = scdskHorizontal then
  begin
    OffsetRect(R2, -R2.Left, 0);

    if Ts < R2.Right then
    begin
      W := R2.Right - Ts;
      R2.Right := Ts;

      P := Muldiv(W, P, S);

      if ThumbMoving and (AOwner <> nil) then
        Inc(P, AOwner.FMovePoint.x - AOwner.FDownPoint.x);

      OffsetRect(R2, P, 0);
    end;

    OffsetRect(R2, R1.Left, 0);
    if R2.Right > R1.Right then
      OffsetRect(R2, R1.Right - R2.Right, 0);

    if R2.Left < R1.Left then
      OffsetRect(R2, R1.Left - R2.Left, 0);
  end else
  begin
    OffsetRect(R2, 0, -R2.Top);

    if Ts < R2.Bottom then
    begin
      W := R2.Bottom - Ts;
      R2.Bottom := Ts;

      P := Muldiv(W, P, S);

      if ThumbMoving and (AOwner <> nil) then
        Inc(P, AOwner.FMovePoint.y - AOwner.FDownPoint.y);

      OffsetRect(R2, 0, P);
    end;

    OffsetRect(R2, 0, R1.Top);
    if R2.Bottom > R1.Bottom then
      OffsetRect(R2, 0, R1.Bottom - R2.Bottom);

    if R2.Top < R1.Top then
      OffsetRect(R2, 0, R1.Top - R2.Top);
  end;

  Result := R2;
  IntersectRect(Result, Result, R1);
end;

function TSCDeCustomControlScrollbar.GetThumbSize(R: TRect): Integer;
begin
  Result := 0;
  if not (Enabled and FThumb.Visible) or IsRectEmpty(R) then
    Exit;

  Result := FThumbSize;
  if Result = -1 then
    Result := GetDefaultThumbSize(R, PageSize);

  if Result < 3 then
    Result := 0;
end;

function TSCDeCustomControlScrollbar.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDeCustomControlScrollbar.Invalidate;
begin
  DoChange;
end;

procedure TSCDeCustomControlScrollbar.LockPosChange;
begin
  Inc(FPosChangeLock);
end;

function TSCDeCustomControlScrollbar.PosChangeLocked: Boolean;
begin
  Result := FPosChangeLock > 0;
end;

procedure TSCDeCustomControlScrollbar.Paint(Canvas: TCanvas; R: TRect);
begin
  //
end;

procedure TSCDeCustomControlScrollbar.PaintOn(DC: HDC; R: TRect);
var
  C: TCanvas;
begin
  if (DC = 0) or IsRectEmpty(R) then
    Exit;

  C := TCanvas.Create;
  try
    C.Lock;
    try
      C.Handle := DC;
      try
        Paint(C, R);
      finally
        C.Handle := 0;
      end;
    finally
      C.Unlock;
    end;
  finally
    C.Free;
  end;
end;

procedure TSCDeCustomControlScrollbar.PartChanged(Sender: TSCDeScrollbarPart);
begin
  DoChange;
end;

procedure TSCDeCustomControlScrollbar.RearrangeDefaults;
begin
  FDefH_ButtonSize := GetSystemMetrics(SM_CYVTHUMB);
  FDefV_ButtonSize := GetSystemMetrics(SM_CXHTHUMB);
end;

procedure TSCDeCustomControlScrollbar.SetBackground(Value: TSCDeScrollbarPart);
begin
  FBackground.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetButtonColors(
  Value: TSCDeScrollbarPart);
begin
  FButtonColors.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetButtonLeft(Value: TSCDeScrollbarPart);
begin
  FButtonLeft.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetButtonRight(Value: TSCDeScrollbarPart);
begin
  FButtonRight.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetButtonSize(Value: Integer);
begin
  if Value < 0 then
    Value := -1
  else
  if (Value > 0) and (Value < 12) then
    Value := 12;

  if FButtonSize <> Value then
  begin
    FButtonSize := Value;

    DoSizeChanged;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetIcons(Value: TSCDeScrollbarPart);
begin
  FIcons.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetLargeChange(Value: TSCDeScrollbarInc);
begin
  if FLargeChange <> Value then
  begin
    FLargeChange := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetMax(Value: Integer);
var
  P1, P2, Mx: Integer;
begin
  if Value < FMin then
    Value := FMin;

  if FMax <> Value then
  begin
    FMax := Value;

    P1 := FPosition;
    Mx := GetMaximumValue;
    if FPosition > Mx then
      FPosition := Mx;

    P2 := FPosition;

    DoRangeChanged;
    DoPositionChanged;
    if P1 <> P2 then DoPositionChanged(P1, P2);
  end;
end;

procedure TSCDeCustomControlScrollbar.SetMin(Value: Integer);
var
  P1, P2: Integer;
begin
  if Value > FMax then
    Value := FMax;

  if FMin <> Value then
  begin
    FMin := Value;

    P1 := FPosition;
    if FPosition < FMin then
      FPosition := FMin;

    P2 := FPosition;

    DoRangeChanged;
    DoPositionChanged;
    if P1 <> P2 then DoPositionChanged(P1, P2);
  end;
end;

procedure TSCDeCustomControlScrollbar.SetPageSize(Value: Integer);
begin
  if FPageSize <> Value then
  begin
    FPageSize := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetPosition(Value: Integer);
var
  P1, P2, Mx: Integer;
  CanScroll: Boolean;
begin
  Mx := GetMaximumValue;
  if Value > Mx then Value := Mx;
  if Value < FMin then Value := FMin;

  if CanScrollToPos(Value) then
  begin
    CanScroll := True;
    DoPositionChanging(Value, CanScroll);

    if CanScroll and CanScrollToPos(Value) and (FPosition <> Value) then
    begin
      P1 := FPosition;
      FPosition := Value;

      P2 := FPosition;

      DoPositionChanged;
      if P1 <> P2 then DoPositionChanged(P1, P2);
    end;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetSensitivity(Value: Integer);
begin
  if Value < -1 then Value := -1;
  FSensitivity := Value;
end;

procedure TSCDeCustomControlScrollbar.SetSmallChange(Value: TSCDeScrollbarInc);
begin
  if FSmallChange <> Value then
  begin
    FSmallChange := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetStyle(
  Value: TSCDeScrollbarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetThumb(Value: TSCDeScrollbarPart);
begin
  FThumb.Assign(Value);
end;

procedure TSCDeCustomControlScrollbar.SetThumbSize(Value: Integer);
begin
  if Value < -1 then
    Value := -1;

  if FThumbSize <> Value then
  begin
    FThumbSize := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetTrack(Value: Boolean);
begin
  if FTrack <> Value then
  begin
    FTrack := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoVisibleChanged;
  end;
end;

procedure TSCDeCustomControlScrollbar.UnlockPosChange;
begin
  if FPosChangeLock > 0 then
    Dec(FPosChangeLock);
end;

procedure TSCDeCustomControlScrollbar.SetTrimPageSize(Value: Boolean);
var
  Mx: Integer;
begin
  if FTrimPageSize <> Value then
  begin
    FTrimPageSize := Value;

    Mx := GetMaximumValue;
    if FTrimPageSize and (FPosition > Mx) then
      SetPosition(Mx);
  end;
end;

function TSCDeCustomControlScrollbar.GetMaximumValue: Integer;
begin
  Result := FMax;
  if FTrimPageSize and (FPageSize > 0) then
  begin
    Result := FMax - FPageSize;
    if Result < FMin then
      Result := FMin;
  end;
end;

procedure TSCDeCustomControlScrollbar.SetSlideLine(Value: Boolean);
begin
  if FSlideLine <> Value then
  begin
    FSlideLine := Value;
    if FVisible then DoChange;
  end;
end;

function TSCDeCustomControlScrollbar.CanScrollToPos(var NewValue: Integer): Boolean;
begin
  Result := True;
  if FOwner is TSCDeCustomScrollControl then
    Result := TSCDeCustomScrollControl(FOwner).CanScrollToPos(FKind, NewValue);
end;

function TSCDeCustomControlScrollbar.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeCustomControlScrollbar.SetThumbLines(Value: TSCDeScrollThumbline);
begin
  if FThumbLines <> Value then
  begin
    FThumbLines := Value;
    if FVisible then DoChange;
  end;
end;

procedure TSCDeCustomControlScrollbar.DoRangeChanged;
begin
  if not (InUpdate or PosChangeLocked) and (FOwner <> nil) then
    FOwner.ScrollerRangeChanged(Self);
end;

procedure TSCDeCustomControlScrollbar.DoSizeChanged;
begin
  if not InUpdate and (FOwner <> nil) then
    FOwner.ScrollerSizeChanged(Self);
end;

procedure TSCDeCustomControlScrollbar.DoPositionChanged(OldPos,
  NewPos: Integer);
begin
  if not (InUpdate or PosChangeLocked) and (FOwner <> nil) then
    FOwner.ScrollerPositionChanged(Self, OldPos, NewPos);
end;

function TSCDeCustomControlScrollbar.GetFocused: Boolean;
begin
  Result := False;
  if FOwner is TSCDeCustomScrollControl then
    Result := TSCDeCustomScrollControl(FOwner).HasFocus;
end;

function TSCDeCustomControlScrollbar.GetHovered: Boolean;
begin
  Result := False;
  if FOwner is TSCDeCustomScrollControl then
    Result := TSCDeCustomScrollControl(FOwner).MouseInControl;
end;

procedure TSCDeCustomControlScrollbar.SetButtonExtra(Value: TSCDeScrollbarPart);
begin
  FButtonExtra.Assign(Value);
end;

function TSCDeCustomControlScrollbar.GetExtraBtnColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    if PressedPart = scspExtraButton then
    begin
      if HotPart = scspExtraButton then
        Result := scdGetOfficeXPDownedSelColor
      else
        Result := scdGetOfficeXPSelColor;
    end else
    if (PressedPart = scspNone) and (HotPart = scspExtraButton) then
      Result := scdGetOfficeXPSelColor
    else
      Result := scdGetOfficeXPBtnColor;
  end else
  begin
    Result := FButtonColors.FColor;
    if not Enabled then
      Result := FButtonColors.FDisabledColor
    else
    if PressedPart = scspExtraButton then
      Result := FButtonColors.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspExtraButton) then
      Result := FButtonColors.FHotColor;

    if Result = clNone then
      Result := clScrollBar;
  end;

  if FButtonColors.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetExtraBtnIconColor: TColor;
begin
  if FStyle = scssOffice2k then
  begin
    Result := clBtnText;
    if (PressedPart = scspExtraButton) and (HotPart = PressedPart) then
      Result := clHighlightText;
  end else
  begin
    Result := FIcons.FColor;
    if not Enabled then
      Result := FIcons.FDisabledColor
    else
    if PressedPart = scspExtraButton then
      Result := FIcons.FDownColor
    else
    if (PressedPart = scspNone) and (HotPart = scspExtraButton) then
      Result := FIcons.FHotColor;

    if Result = clNone then
      Result := clWindowText;
  end;    

  if FIcons.FBlend then
    Result := scdBlendedColor(Result, 24, 24, 24, True);
end;

function TSCDeCustomControlScrollbar.GetExtraButtonRect(R: TRect): TRect;
var
  Bs: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FButtonSize = 0) or not FButtonExtra.Visible or IsRectEmpty(R) then
    Exit;

  Result := R;
  Bs := GetButtonSize(R);

  if FKind = scdskHorizontal then
  begin
    if FButtonLayout in [scdsbDefault, scdsbRightBottom] then
      Dec(Result.Right, Bs);

    Result.Left := Result.Right - Bs;
  end else
  begin
    if FButtonLayout in [scdsbDefault, scdsbRightBottom] then
      Dec(Result.Bottom, Bs);

    Result.Top := Result.Bottom - Bs;
  end;
end;

function TSCDeCustomControlScrollbar.GetExtraButton: Boolean;
begin
  Result := FButtonExtra.Visible;
end;

procedure TSCDeCustomControlScrollbar.SetExtraButton(Value: Boolean);
begin
  FButtonExtra.Visible := Value;
end;

procedure TSCDeCustomControlScrollbar.ScrollMessage(var Msg: TWMScroll);
var
  Incr, FinalIncr, Count, CalcRange: Integer;
  CurrentTime, StartTime, ElapsedTime: Longint;
begin
  with Msg do
  begin
    if FSmooth and (ScrollCode in [SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN]) then
    begin
      case ScrollCode of
        SB_LINEUP, SB_LINEDOWN:
        begin
          Incr := FSmallChange div FLineDiv;
          FinalIncr := FSmallChange mod FLineDiv;
          Count := FLineDiv;
        end;
        SB_PAGEUP, SB_PAGEDOWN:
        begin
          Incr := GetPageUpDownSize;
          
          FinalIncr := Incr mod FPageDiv;
          Incr := Incr div FPageDiv;
          Count := FPageDiv;
        end;
        else begin
          Count := 0;
          Incr := 0;
          FinalIncr := 0;
        end;
      end;

      CurrentTime := 0;
      while Count > 0 do
      begin
        StartTime := GetCurrentTime;
        ElapsedTime := StartTime - CurrentTime;

        if ElapsedTime < FDelay then
          Sleep(FDelay - ElapsedTime);

        CurrentTime := StartTime;
        case ScrollCode of
          SB_LINEUP:
            SetPosition(FPosition - Incr);
          SB_LINEDOWN:
            SetPosition(FPosition + Incr);
          SB_PAGEUP:
            SetPosition(FPosition - Incr);
          SB_PAGEDOWN:
            SetPosition(FPosition + Incr);
        end;

        if FOwner <> nil then
          FOwner.Update;
          
        Dec(Count);
      end;
      
      if FinalIncr > 0 then
      begin
        case ScrollCode of
          SB_LINEUP:
            SetPosition(FPosition - FinalIncr);
          SB_LINEDOWN:
            SetPosition(FPosition + FinalIncr);
          SB_PAGEUP:
            SetPosition(FPosition - FinalIncr);
          SB_PAGEDOWN:
            SetPosition(FPosition + FinalIncr);
        end;
      end;
    end else
    begin
      CalcRange := GetCalcRange;
      if CalcRange < 0 then CalcRange := 0;

      case ScrollCode of
        SB_LINEUP:
          SetPosition(FPosition - FSmallChange);
        SB_LINEDOWN:
          SetPosition(FPosition + FSmallChange);
        SB_PAGEUP:
          SetPosition(FPosition - GetPageUpDownSize);
        SB_PAGEDOWN:
          SetPosition(FPosition + GetPageUpDownSize);
        SB_TOP: SetPosition(0);
        SB_BOTTOM:
          SetPosition(CalcRange);
      end;
    end;
  end;
end;

function TSCDeCustomControlScrollbar.GetPageUpDownSize: Integer;
begin
  Result := FLargeChange;
end;

function TSCDeCustomControlScrollbar.GetRange: Integer;
begin
  Result := GetMaximumValue - FMin;
  if Result < 0 then Result := 0;
end;

procedure TSCDeCustomControlScrollbar.SetRange(Value: Integer);
var
  P1, P2, Mx, Rg: Integer;
begin
  if Value < 0 then Value := 0;

  Rg := GetRange;
  if Rg <> Value then
  begin
    FMin := 0;
    FMax := Value;

    P1 := FPosition;
    Mx := GetMaximumValue;
    if FPosition > Mx then
      FPosition := Mx;

    P2 := FPosition;

    DoRangeChanged;
    DoPositionChanged;
    if P1 <> P2 then DoPositionChanged(P1, P2);
  end;
end;

function TSCDeCustomControlScrollbar.GetCalcRange: Integer;
begin
  Result := GetMaximumValue - FMin;
  if Result < 0 then Result := 0;
end;

function TSCDeCustomControlScrollbar.GetSensitivity: Integer;
begin
  Result := FSensitivity;
end;

{ TSCDeCustomScrollControl }

procedure TSCDeCustomScrollControl.AssignHittest(FromHt,
  ToHt: PSCDeScrollbarHittest);
begin
  if (FromHt = nil) or (ToHt = nil) then
    Exit;

  with ToHt^ do
  begin
    Kind := FromHt^.Kind;
    HitPart := FromHt^.HitPart;
    Enabled := FromHt^.Enabled;
    X := FromHt^.X;
    Y := FromHt^.Y;
  end;
end;

procedure TSCDeCustomScrollControl.CalculateBorder(var R: TRect);
var
  H, W: Integer;
begin
  inherited CalculateBorder(R);

  H := 0;
  if (FScrollbarHorz <> nil) and FScrollbarHorz.Visible then
  begin
    H := GetHorzScrollbarHeight;
    if H < 0 then H := 0;
  end;

  W := 0;
  if (FScrollbarVert <> nil) and FScrollbarVert.Visible then
  begin
    W := GetVertScrollbarWidth;
    if W < 0 then W := 0;
  end;

  Dec(R.Right, W);
  Dec(R.Bottom, H);
end;

function TSCDeCustomScrollControl.CanCaptureMouseOnNC(P: TPoint): Boolean;
var
  Ret: LongInt;
begin
  Ret := GetNCAreaAtPos(P);
  Result := (Ret >= SCDE_HTHSCROLLBAR) and (Ret <= SCDE_HTVSB_RIGHTSPARE);
end;

function TSCDeCustomScrollControl.CanScrollBars(AsLeft: Boolean): Boolean;
var
  Mx: Integer;
  Sb: TSCDeCustomControlScrollbar;
begin
  Result := False;

  if IsScrollingReady then
  begin
    Sb := FScrollbarHorz;
    if FDownTest.Kind = scshkVertical then
      Sb := FScrollbarVert;

    Mx := Sb.GetMaximumValue;

    with Sb do
      if FMin < Mx then
      begin
        if AsLeft then
          Result := (FPosition > FMin) and
            (FDownTest.HitPart in [scspExtraButton, scspLeftSpare, scspLeftButton])
        else
          Result := (FPosition < Mx) and
            (FDownTest.HitPart in [scspRightSpare, scspRightButton]);
      end;
  end;  
end;

constructor TSCDeCustomScrollControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollbarHeight := -1;
  FScrollButtonsLayout := scdsbDefault;
  FScrollbarStyle := scssDefault;
  FScrollbarThumbLines := sctlNone;
  FScrollbarHorz := GetScrollbarClass.Create(Self, scdskHorizontal);
  FScrollbarVert := GetScrollbarClass.Create(Self, scdskVertical);

  ResetHittest(@FHotTest);
  ResetHittest(@FDownTest);

  FScrollbars := GetControlScrollbarsClass.Create(Self);
end;

destructor TSCDeCustomScrollControl.Destroy;
begin
  FreeAndNil(FScrollbarHorz);
  FreeAndNil(FScrollbarVert);
  FreeAndNil(FScrollbars);
  inherited Destroy;
end;

procedure TSCDeCustomScrollControl.DoScrollerPositionChanged(
  Kind: TSCDeScrollbarKind);
begin
  //
end;

procedure TSCDeCustomScrollControl.DoScrollerPositionChanging(
  Kind: TSCDeScrollbarKind; CurPos: Integer; var ScrollPos: Integer;
  var CanScroll: Boolean); 
begin
  //
end;

procedure TSCDeCustomScrollControl.FillScrollbarBack(DC: HDC; R: TRect);
var
  CR: TRect;
  MemDC: HDC;
  W, H: Integer;
  MemBitmap, OldBitmap: HBITMAP;
  NewBrush, LastBrush: HBRUSH;
begin
  if (DC = 0) or IsRectEmpty(R) then
    Exit;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  MemDC := CreateCompatibleDC(DC);
  try
    MemBitmap := CreateCompatibleBitmap(DC, W, H);
    try
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        CR := R;
        OffsetRect(CR, -CR.Left, -CR.Top);

        NewBrush := CreateSolidBrush(ColorToRGB(GetBackgroundColor));
        LastBrush := SelectObject(MemDC, NewBrush);
        try
          FillRect(MemDC, CR, NewBrush);
        finally
          SelectObject(MemDC, LastBrush);
          DeleteObject(NewBrush);
        end;

        BitBlt(DC, R.Left, R.Top, W, H, MemDC, 0, 0, SRCCOPY);
      finally
        SelectObject(MemDC, OldBitmap);
      end;
    finally
      DeleteObject(MemBitmap);
    end;
  finally
    DeleteDC(MemDC);
  end;
end;

procedure TSCDeCustomScrollControl.FillScrollbarHittest(P: TPoint;
  Ht: PSCDeScrollbarHittest);
var
  R, Br, Pr, Tr: TRect;
begin
  if Ht = nil then
    Exit;

  ResetHittest(Ht);

  if (FScrollbarHorz <> nil) and FScrollbarHorz.Visible then
  begin
    R := GetHorzScrollbarRect;
    
    if not IsRectEmpty(R) and PtInRect(R, P) then
    begin
      Ht^.Kind := scshkHorizontal;
      Ht^.Enabled := Self.Enabled and FScrollbarHorz.Enabled;

      Ht^.X := P.x;
      Ht^.Y := P.y;

      Pr := FScrollbarHorz.GetLeftButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspLeftButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarHorz.FButtonLeft.Enabled;

        Exit;
      end;

      Pr := FScrollbarHorz.GetRightButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspRightButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarHorz.FButtonRight.Enabled;

        Exit;
      end;

      Pr := FScrollbarHorz.GetExtraButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspExtraButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarHorz.FButtonExtra.Enabled;

        Exit;
      end;

      Br := FScrollbarHorz.GetBackRect(R);
      if not IsRectEmpty(Br) and PtInRect(Br, P) then
      begin
        Tr := FScrollbarHorz.GetThumbRect(R);
        if IsRectEmpty(Tr) then
          Exit;

        if PtInRect(Tr, P) then
        begin
          Ht^.HitPart := scspThumb;
          Exit;
        end;

        Pr := Br;
        Pr.Right := Tr.Left;

        if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
        begin
          Ht^.HitPart := scspLeftSpare;
          Exit;
        end;

        Pr := Br;
        Pr.Left := Tr.Right;

        if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
        begin
          Ht^.HitPart := scspRightSpare;
          Exit;
        end;
      end;

      Exit;
    end;
  end;

  if (FScrollbarVert <> nil) and FScrollbarVert.Visible then
  begin
    R := GetVertScrollbarRect;

    if not IsRectEmpty(R) and PtInRect(R, P) then
    begin
      Ht^.Enabled := Self.Enabled and FScrollbarVert.Enabled;
      Ht^.Kind := scshkVertical;

      Ht^.X := P.x;
      Ht^.Y := P.y;

      Pr := FScrollbarVert.GetLeftButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspLeftButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarVert.FButtonLeft.Enabled;

        Exit;
      end;

      Pr := FScrollbarVert.GetRightButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspRightButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarVert.FButtonRight.Enabled;

        Exit;
      end;

      Pr := FScrollbarVert.GetExtraButtonRect(R);
      if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
      begin
        Ht^.HitPart := scspExtraButton;
        Ht^.Enabled := Ht^.Enabled and FScrollbarVert.FButtonExtra.Enabled;

        Exit;
      end;

      Br := FScrollbarVert.GetBackRect(R);
      if not IsRectEmpty(Br) and PtInRect(Br, P) then
      begin
        Tr := FScrollbarVert.GetThumbRect(R);
        if IsRectEmpty(Tr) then
          Exit;

        if PtInRect(Tr, P) then
        begin
          Ht^.HitPart := scspThumb;
          Exit;
        end;

        Pr := Br;
        Pr.Bottom := Tr.Top;

        if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
        begin
          Ht^.HitPart := scspLeftSpare;
          Exit;
        end;

        Pr := Br;
        Pr.Top := Tr.Bottom;

        if not IsRectEmpty(Pr) and PtInRect(Pr, P) then
        begin
          Ht^.HitPart := scspRightSpare;
          Exit;
        end;
      end;

      Exit;
    end;
  end;
end;

function TSCDeCustomScrollControl.GetBackgroundColor: TColor;
begin
  Result := clScrollBar;
end;

function TSCDeCustomScrollControl.GetDistanceIncrement(
  Kind: TSCDeScrollbarKind; Dist: Integer): Integer;
var
  W, P, Mx: Integer;
  R, R1, R2: TRect;
  Sb: TSCDeCustomControlScrollbar;
begin
  Result := 0;

  Sb := FScrollbarHorz;
  if Kind = scdskVertical then
    Sb := FScrollbarVert;

  Mx := Sb.GetMaximumValue;

  if (Dist = 0) or (Sb.FMin >= Mx) then
    Exit;

  if Kind = scdskHorizontal then
    R := GetHorzScrollbarRect
  else
    R := GetVertScrollbarRect;

  if IsRectEmpty(R) then Exit;

  R1 := Sb.GetBackRect(R);
  if IsRectEmpty(R1) then Exit;

  R2 := Sb.GetThumbRect(R);
  OffsetRect(R2, R1.Left - R2.Left, R1.Top - R2.Top);

  if EqualRect(R1, R2) then Exit;

  if Kind = scdskHorizontal then
  begin
    R1.Left := R2.Right;
    W := R1.Right - R1.Left;
  end else
  begin
    R1.Top := R2.Bottom;
    W := R1.Bottom - R1.Top;
  end;

  if W <= 0 then Exit;

  P := Abs(Dist);
  if P > W then
  begin
    Result := Mx - Sb.FMin;
    if Dist < 0 then
      Result := -Result;

    Exit;
  end;

  Result := Muldiv(Mx - Sb.FMin, P, W);
  if Dist < 0 then
    Result := -Result;
end;

function TSCDeCustomScrollControl.GetHorzScrollbarHeight: Integer;
begin
  Result := FScrollbarHeight;
  if Result < 0 then
    Result := scdHorzScrollbarHeight;
end;

function TSCDeCustomScrollControl.GetHorzScrollbarRect: TRect;
var
  B, H: Integer;
  WR, CR: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not (HandleAllocated and (FScrollbarHorz <> nil) and FScrollbarHorz.Visible and
    GetWindowRect(Handle, WR) and not IsRectEmpty(WR)) then
    Exit;

  OffsetRect(WR, -WR.Left, -WR.Top);

  CR := WR;
  CalculateBorder(CR);

  B := BorderWidth;
  if B > 0 then InflateRect(CR, -B, -B);

  H := GetHorzScrollbarHeight;
  if H > 0 then
  begin
    Result := CR;
    Result.Top := Result.Bottom;
    Inc(Result.Bottom, H);

    IntersectRect(Result, Result, WR);
  end;  
end;

function TSCDeCustomScrollControl.GetInheritedNCArea(P: TPoint): LongInt;
var
  Ht: TSCDeScrollbarHittest;
begin
  Result := inherited GetInheritedNCArea(P);

  if (Result = SCDE_HTNONE) and
    (((FScrollbarHorz <> nil) and FScrollbarHorz.Visible) or
     ((FScrollbarVert <> nil) and FScrollbarVert.Visible)) then
  begin
    P := ScreenToNC(P);
    Ht := GetScrollbarHittest(P);

    if Ht.Kind <> scshkNone then
      case Ht.HitPart of
        scspLeftButton:
        begin
          Result := SCDE_HTHSB_LEFTBTN;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_LEFTBTN;
        end;
        scspRightButton:
        begin
          Result := SCDE_HTHSB_RIGHTBTN;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_RIGHTBTN;
        end;
        scspExtraButton:
        begin
          Result := SCDE_HTHSB_EXTRABTN;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_EXTRABTN;
        end;
        scspThumb:
        begin
          Result := SCDE_HTHSB_THUMB;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_THUMB;
        end;
        scspLeftSpare:
        begin
          Result := SCDE_HTHSB_LEFTSPARE;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_LEFTSPARE;
        end;
        scspRightSpare:
        begin
          Result := SCDE_HTHSB_RIGHTSPARE;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSB_RIGHTSPARE;
        end;
        else
        begin
          Result := SCDE_HTHSCROLLBAR;
          if Ht.Kind = scshkVertical then
            Result := SCDE_HTVSCROLLBAR;
        end;
      end;
  end;
end;

function TSCDeCustomScrollControl.GetPositionAtPos(Kind: TSCDeScrollbarKind; X,
  Y: Integer; IncParts: Boolean): Integer;
var
  P: TPoint;
  R, CR, R2: TRect;
  I, M, W, Mx: Integer;
  Sb: TSCDeCustomControlScrollbar;
begin
  Sb := FScrollbarHorz;
  if Kind = scdskVertical then
    Sb := FScrollbarVert;

  Mx := Sb.GetMaximumValue;  

  Result := Sb.FMin;
  if Sb.FMin >= Mx then Exit;

  Result := Sb.FPosition;

  if Kind = scdskHorizontal then
    CR := GetHorzScrollbarRect
  else
    CR := GetVertScrollbarRect;

  if IsRectEmpty(CR) then Exit;

  R := Sb.GetBackRect(CR);
  if IsRectEmpty(R) then Exit;

  R2 := R;
  if Kind = scdskHorizontal then
    R2.Right := R2.Left
  else
    R2.Bottom := R2.Top;

  P := Point(X, Y);

  if not IncParts then
  begin
    if not PtInRect(R, P) then
      Exit;

    if Sb.FThumb.Visible and (Sb.FThumbSize <> 0) then
    begin
      R2 := Sb.GetThumbRect(CR);
      if not IsRectEmpty(R2) and PtInRect(R2, P) then
        Exit;
    end;
  end;

  if Kind = scdskHorizontal then
  begin
    if X <= R.Left then
    begin
      if not IncParts and (X < R.Left) then
        Exit;

      Result := Sb.FMin;
    end else
    if X >= R.Right then
    begin
      if not IncParts then
        Exit;

      Result := Mx;
    end else
    begin
      W := R.Right - R.Left;
      M := Mx - Sb.FMin;

      if M = 1 then
      begin
        R.Right := W div 2;

        if PtInRect(R, P) then
          Result := Sb.FMin
        else
          Result := Mx;

        Exit;
      end;

      Dec(X, R.Left);
      OffsetRect(R, -R.Left, 0);

      for I := 0 to M-1 do
      begin
        R.Left  := Muldiv(I, W, M);
        R.Right := Muldiv(I + 1, W, M);

        if (X >= R.Left) and (X < R.Right) then
        begin
          Result := Sb.FMin + I;
          Exit;
        end;
      end;
    end;
  end else
  begin
    if Y <= R.Top then
    begin
      if not IncParts and (Y < R.Top) then
        Exit;

      Result := Sb.FMin;
    end else
    if Y >= R.Bottom then
    begin
      if not IncParts then
        Exit;

      Result := Mx;
    end else
    begin
      W := R.Bottom - R.Top;
      M := Mx - Sb.FMin;

      if M = 1 then
      begin
        R.Bottom := W div 2;

        if PtInRect(R, P) then
          Result := Sb.FMin
        else
          Result := Mx;

        Exit;
      end;

      Dec(Y, R.Top);
      OffsetRect(R, 0, -R.Top);

      for I := 0 to M-1 do
      begin
        R.Left  := Muldiv(I, W, M);
        R.Right := Muldiv(I + 1, W, M);

        if (Y >= R.Top) and (Y < R.Bottom) then
        begin
          Result := Sb.FMin + I;
          Exit;
        end;
      end;
    end;
  end;
end;

function TSCDeCustomScrollControl.GetScrollbarClass: TSCDeCustomControlScrollbarClass;
begin
  Result := TSCDeControlScrollbar;
end;

function TSCDeCustomScrollControl.GetScrollbarHittest(P: TPoint): TSCDeScrollbarHittest;
begin
  FillScrollbarHittest(P, @Result);
end;

function TSCDeCustomScrollControl.GetScrollbarInfo(Kind: TSCDeScrollbarKind): TSCDeScrollInfo;
var
  Sb: TSCDeCustomControlScrollbar;
begin
  Sb := FScrollbarHorz;
  if Kind = scdskVertical then
    Sb := FScrollbarVert;

  ResetScrollbarInfo(@Result);

  with Result do
  begin
    Min  := Sb.Min;
    Max  := Sb.Max;
    Page := Sb.PageSize;
    Pos  := Sb.Position;
    Range := Sb.Max - Sb.Min;
    ButtonSize  := Sb.ButtonSize;
    ThumbSize   := Sb.ThumbSize;
    LargeChange := Sb.LargeChange;
    SmallChange := Sb.SmallChange;
    Visible     := Sb.Visible;
    Enabled     := Sb.Enabled;
    Smooth      := Sb.Smooth;
    LeftButtonEnabled  := Sb.ButtonLeft.Enabled;
    LeftButtonVisible  := Sb.ButtonLeft.Visible;
    RightButtonEnabled := Sb.ButtonRight.Enabled;
    RightButtonVisible := Sb.ButtonRight.Visible;
    Tracking           := Sb.Track;
    ThumbEnabled       := Sb.Thumb.Enabled;
    ThumbVisible       := Sb.Thumb.Visible;
    TrimPageSize       := Sb.TrimPageSize;
  end;
end;

function TSCDeCustomScrollControl.GetVertScrollbarRect: TRect;
var
  B, W: Integer;
  WR, CR: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not (HandleAllocated and (FScrollbarVert <> nil) and FScrollbarVert.Visible and
    GetWindowRect(Handle, WR) and not IsRectEmpty(WR)) then
    Exit;

  OffsetRect(WR, -WR.Left, -WR.Top);

  CR := WR;
  CalculateBorder(CR);

  B := BorderWidth;
  if B > 0 then InflateRect(CR, -B, -B);

  W := GetVertScrollbarWidth;
  if W > 0 then
  begin
    Result := CR;
    Result.Left := Result.Right;
    Inc(Result.Right, W);

    IntersectRect(Result, Result, WR);
  end;
end;

function TSCDeCustomScrollControl.GetVertScrollbarWidth: Integer;
begin
  Result := FScrollbarHeight;
  if Result < 0 then
    Result := scdVertScrollbarWidth;
end;

function TSCDeCustomScrollControl.IsScrollingReady: Boolean;
begin
  Result := Enabled and FDownTest.Enabled and
    (FDownTest.Kind <> scshkNone) and (FDownTest.Kind = FHotTest.Kind) and
    (FDownTest.HitPart <> scspNone) and (FDownTest.HitPart = FHotTest.HitPart)
end;

function TSCDeCustomScrollControl.IsScrollPart(HitPart: TSCDeScrollerHitPart): Boolean;
begin
  Result := HitPart in [scspExtraButton, scspLeftButton, scspLeftSpare,
    scspRightButton, scspRightSpare];
end;

procedure TSCDeCustomScrollControl.NCMouseDown(Button: TMouseButton;
  HitTest: LongInt; DblClk: Boolean; X, Y: Integer);
var
  OldHot, OldDown: TSCDeScrollbarHittest;
  Sb: TSCDeCustomControlScrollbar;
begin
  FMovingThumb  := True;
  FDownPoint    := Point(X, Y);
  FMovePoint    := Point(X, Y);
  FDownPosition := 0;

  OldHot  := FHotTest;
  OldDown := FDownTest;

  ResetHittest(@FDownTest);
  ResetHittest(@FHotTest);

  if HitTest = HTBORDER then
    FillScrollbarHittest(Point(X, Y), @FHotTest);

  StopScrollingBars;

  if (HitTest = HTBORDER) and (Button = mbLeft) then
  begin
    AssignHittest(@FHotTest, @FDownTest);

    Sb := FScrollbarHorz;
    if FDownTest.Kind = scshkVertical then
      Sb := FScrollbarVert;

    if FDownTest.Kind <> scshkNone then
      FDownPosition := Sb.FPosition;

    if FHotTest.Enabled and IsScrollPart(FHotTest.HitPart) and
      CanScrollBars(FHotTest.HitPart in [scspExtraButton, scspLeftButton, scspLeftSpare]) then
      StartScrollingBars;
  end;

  if not (OldHot.Enabled and (OldHot.Enabled = FHotTest.Enabled) and
    OldDown.Enabled and (OldDown.Enabled = FDownTest.Enabled) and
    SameHittests(@OldHot, @FHotTest) and SameHittests(@OldDown, @FDownTest)) then
    RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.NCMouseEnter(var HitTest: Integer; X, Y: Integer);
var
  OldTest: TSCDeScrollbarHittest;
begin
  OldTest := FHotTest;
  FillScrollbarHittest(Point(X, Y), @FHotTest);

  if (OldTest.Kind <> FHotTest.Kind) or
    (FHotTest.Kind <> scshkNone) and (OldTest.HitPart <> FHotTest.HitPart) then
  begin
    if OldTest.Kind <> FHotTest.Kind then
    begin
      if OldTest.Kind = scshkHorizontal then
        RedrawScrollbars(scsdkHorizontal)
      else
      if OldTest.Kind = scshkVertical then
        RedrawScrollbars(scsdkVertical);
    end;

    if FHotTest.Kind = scshkHorizontal then
      RedrawScrollbars(scsdkHorizontal)
    else
    if FHotTest.Kind = scshkVertical then
      RedrawScrollbars(scsdkVertical);
  end;    

  if FHotTest.Kind in [scshkHorizontal, scshkVertical] then
    ResumeScrollingBars;
end;

procedure TSCDeCustomScrollControl.NCMouseLeave;
var
  OldTest: TSCDeScrollbarHittest;
begin
  OldTest := FHotTest;

  ResetHittest(@FHotTest);
  PauseScrollingBars;

  if OldTest.Kind = scshkHorizontal then
    RedrawScrollbars(scsdkHorizontal)
  else
  if OldTest.Kind = scshkVertical then
    RedrawScrollbars(scsdkVertical);
end;

procedure TSCDeCustomScrollControl.NCMouseMove(HitTest: LongInt; X, Y: Integer);
var
  R, BkR, ThR: TRect;
  OldPoint: TPoint;
  P, OldP, Sense, Mx: Integer;
  OldHot: TSCDeScrollbarHittest;
  Sb: TSCDeCustomControlScrollbar;
begin
  FMovingThumb := True;

  OldHot := FHotTest;
  FillScrollbarHittest(Point(X, Y), @FHotTest);

  if (FDownTest.Kind <> scshkNone) and (FDownTest.HitPart <> scspNone) then
  begin
    OldPoint := FMovePoint;
    FMovePoint := Point(X, Y);

    Sb := FScrollbarHorz;
    if FDownTest.Kind = scshkVertical then
      Sb := FScrollbarVert;

    Mx := Sb.GetMaximumValue;
      
    if Sb.FMin >= Mx then Exit;

    if FDownTest.Kind = scshkVertical then
      R := GetVertScrollbarRect
    else
      R := GetHorzScrollbarRect;

    BkR := Sb.GetBackRect(R);

    ThR := Rect(0, 0, 0, 0);
    if not IsRectEmpty(BkR) then
      ThR := Sb.GetThumbRect(R);

    if IsRectEmpty(BkR) or EqualRect(BkR, ThR) then
      Exit;

    Sense := Sb.GetSensitivity;

    case FDownTest.HitPart of
      scspThumb:
      begin
        OldP := Sb.FPosition;

        if FDownTest.Kind = scshkHorizontal then
        begin
          P := FDownPosition + GetDistanceIncrement(scdskHorizontal,
            FMovePoint.x - FDownPoint.x);

          if (Sense > -1) and ((FMovePoint.y < R.Top - Sense) or
            (FMovePoint.y > R.Bottom + Sense)) then
          begin
            FMovePoint := FDownPoint;
            
            FMovingThumb := False;
            if Sb.FTrack then
              Sb.SetPosition(FDownPosition);

            if not Sb.FTrack or ((OldP <> Sb.FPosition) and
              (OldPoint.x <> FDownPoint.x) and (OldPoint.y <> FDownPoint.y)) then
              RedrawScrollbars(scsdkHorizontal);

            Exit;
          end;
        end else
        begin
          P := FDownPosition + GetDistanceIncrement(scdskVertical,
            FMovePoint.y - FDownPoint.y);

          if (Sense > -1) and ((FMovePoint.x < R.Left - Sense) or
            (FMovePoint.x > R.Right + Sense)) then
          begin
            FMovePoint := FDownPoint;

            FMovingThumb := False;
            if Sb.FTrack then
              Sb.SetPosition(FDownPosition);

            if not Sb.FTrack or ((OldP <> Sb.FPosition) and
              (OldPoint.x <> FDownPoint.x) and (OldPoint.y <> FDownPoint.y)) then
              RedrawScrollbars(scsdkVertical);

            Exit;
          end;
        end;

        if P < Sb.FMin then P := Sb.FMin;
        if P > Mx then P := Mx;

        if Sb.FTrack then
          Sb.SetPosition(P);

        if (Sb.FPosition = OldP) and
          (((FDownTest.Kind = scshkHorizontal) and (FMovePoint.x <> OldPoint.x)) or
           ((FDownTest.Kind = scshkVertical) and (FMovePoint.y <> OldPoint.y))) then
          RedrawScrollbars;
      end;
      scspExtraButton, scspLeftButton,
      scspRightButton, scspLeftSpare, scspRightSpare:
      begin
        if OldHot.HitPart <> FHotTest.HitPart then
          RedrawScrollbars;
      end;
    end;
  end else
  begin
    FMovePoint := Point(X, Y);

    if OldHot.Kind <> FHotTest.Kind then
    begin
      if OldHot.Kind = scshkHorizontal then
        RedrawScrollbars(scsdkHorizontal)
      else
      if OldHot.Kind = scshkVertical then
        RedrawScrollbars(scsdkVertical);

      if FHotTest.Kind = scshkHorizontal then
        RedrawScrollbars(scsdkHorizontal)
      else
      if FHotTest.Kind = scshkVertical then
        RedrawScrollbars(scsdkVertical);
    end else
    if (OldHot.Kind <> scshkNone) and (OldHot.HitPart <> FHotTest.HitPart) then
    begin
      if FHotTest.Kind = scshkHorizontal then
        RedrawScrollbars(scsdkHorizontal)
      else
      if FHotTest.Kind = scshkVertical then
        RedrawScrollbars(scsdkVertical);
    end;
  end;  
end;

procedure TSCDeCustomScrollControl.NCMouseUp(Button: TMouseButton;
  HitTest: LongInt; X, Y: Integer);
var
  P: Integer;
  OldHot, OldDown: TSCDeScrollbarHittest;
begin
  FMovingThumb := False;

  P := 0;
  if FDownTest.Kind = scshkHorizontal then
    P := FDownPosition + GetDistanceIncrement(scdskHorizontal, FMovePoint.x - FDownPoint.x)
  else
  if FDownTest.Kind = scshkVertical then
    P := FDownPosition + GetDistanceIncrement(scdskVertical, FMovePoint.y - FDownPoint.y);

  FDownPoint := Point(-1, -1);
  FMovePoint := Point(-1, -1);

  FDownPosition := 0;

  OldHot  := FHotTest;
  OldDown := FDownTest;

  FHotTest  := GetScrollbarHittest(Point(X, Y));
  ResetHittest(@FDownTest);

  StopScrollingBars;
  if not ScrollingBars and (OldDown.HitPart = scspThumb) then
  begin
    if OldDown.Kind = scshkHorizontal then
      FScrollbarHorz.SetPosition(P)
    else
    if OldDown.Kind = scshkVertical then
      FScrollbarVert.SetPosition(P);
  end;

  if not SameHittests(@OldHot, @FHotTest) or
    not SameHittests(@FDownTest, @OldDown) then
    RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.PauseScrollingBars;
begin
  FScrolling := False;
end;

procedure TSCDeCustomScrollControl.RedrawScrollbars(Kind: TSCDeScrollbarDrawKind; OnDC: HDC);
var
  DC: HDC;
  WR, CR, R: TRect;
  B, H, W: Integer;
begin
  if not (HandleAllocated and GetWindowRect(Handle, WR) and
    not IsRectEmpty(WR)) then
    Exit;

  OffsetRect(WR, -WR.Left, -WR.Top);

  CR := WR;
  CalculateBorder(CR);

  B := BorderWidth;
  if B > 0 then InflateRect(CR, -B, -B);

  DC := OnDC;
  if OnDC = 0 then
    DC := GetWindowDC(Handle);

  try
    H := 0;
    if (FScrollbarHorz <> nil) and FScrollbarHorz.Visible then
    begin
      H := GetHorzScrollbarHeight;
      if H < 0 then H := 0;
    end;

    W := 0;
    if (FScrollbarVert <> nil) and FScrollbarVert.Visible then
    begin
      W := GetVertScrollbarWidth;
      if W < 0 then W := 0;
    end;

    if (W > 0) and (H > 0) then
    begin
      R := CR;

      R.Top := R.Bottom;
      Inc(R.Bottom, H);

      R.Left := R.Right;
      Inc(R.Right, W);

      FillScrollbarBack(DC, R);
    end;

    if (H > 0) and (Kind in [scsdkHorizontal, scsdkAll]) then
    begin
      R := CR;
      R.Top := R.Bottom;
      Inc(R.Bottom, H);

      IntersectRect(R, R, WR);
      FScrollbarHorz.BufferedPaint(DC, R);
    end;

    if (W > 0) and (Kind in [scsdkVertical, scsdkAll]) then
    begin
      R := CR;
      R.Left := R.Right;
      Inc(R.Right, W);

      IntersectRect(R, R, WR);
      FScrollbarVert.BufferedPaint(DC, R);
    end;
  finally
    if (DC <> 0) and (OnDC = 0) then
      ReleaseDC(Handle, DC);
  end;
end;

procedure TSCDeCustomScrollControl.ResetHittest(Ht: PSCDeScrollbarHittest);
begin
  if Ht <> nil then
    with Ht^ do
    begin
      Kind := scshkNone;
      HitPart := scspNone;
      Enabled := True;
      X := 0;
      Y := 0;
    end;
end;

procedure TSCDeCustomScrollControl.ResetScrollbarInfo(Si: PSCDeScrollInfo);
begin
  if Si <> nil then
    with Si^ do
    begin
      Min  := 0;
      Max  := 0;
      Page := 0;
      Pos  := 0;
      ButtonSize  := 0;
      ThumbSize   := 0;
      LargeChange := 0;
      SmallChange := 0;
      Visible     := False;
      Enabled     := False;
      LeftButtonEnabled  := True;
      LeftButtonVisible  := True;
      RightButtonEnabled := True;
      RightButtonVisible := True;
      Tracking           := True;
      ThumbEnabled       := True;
      ThumbVisible       := True;
      TrimPageSize       := True;
    end;
end;

procedure TSCDeCustomScrollControl.ResumeScrollingBars;
begin
  FScrolling := (FScrollTimer <> -1) and
    (CanScrollBars(True) or CanScrollBars(False));
end;

function TSCDeCustomScrollControl.SameHittests(Ht1, Ht2: PSCDeScrollbarHittest;
  CheckPos: Boolean): Boolean;
begin
  Result := False;
  if (Ht1 = nil) or (Ht2 = nil) then
    Exit;

  Result := (Ht1^.Kind = Ht2^.Kind) and (Ht1^.HitPart = Ht2^.HitPart) and
    (Ht1^.Enabled = Ht2^.Enabled);

  if Result and CheckPos then
    Result := (Ht1^.X = Ht2^.X) and (Ht1^.Y = Ht2^.Y);
end;

procedure TSCDeCustomScrollControl.ScrollControl;
var
  DrawKind: TSCDeScrollbarDrawKind;
  SbTest: TSCDeScrollbarHittest;
  Sb: TSCDeCustomControlScrollbar;
begin
  if not ScrollingBars or ScrollingPaused then
    Exit;

  Sb := FScrollbarHorz;
  DrawKind := scsdkHorizontal;

  if FDownTest.Kind = scshkVertical then
  begin
    Sb := FScrollbarVert;
    DrawKind := scsdkVertical;
  end;

  with Sb do
  begin
    if CanScrollBars(True) then
    begin
      if FDownTest.HitPart in [scspExtraButton, scspLeftButton] then
        SetPosition(Position - FSmallChange)
      else
        SetPosition(Position - FLargeChange);

      if FPosition <= FMin then
      begin
        SbTest := FDownTest;
        StopScrollingBars;

        FDownTest := SbTest;
        RedrawScrollbars(DrawKind);
      end;
    end else
    if CanScrollBars(False) then
    begin
      if PressedPart = scspRightButton then
        SetPosition(Position + FSmallChange)
      else
        SetPosition(Position + FLargeChange);

      if FPosition >= Sb.GetMaximumValue then
      begin
        SbTest := FDownTest;
        StopScrollingBars;

        FDownTest := SbTest;
        RedrawScrollbars(DrawKind);
      end;
    end;
  end;  
end;

procedure TSCDeCustomScrollControl.ScrollerChanged(Sender: TSCDeCustomControlScrollbar);
begin
  RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.ScrollerDestroyed(Sender: TSCDeCustomControlScrollbar);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Sender = FScrollbarHorz then
      FScrollbarHorz := nil;
    if Sender = FScrollbarVert then
      FScrollBarVert := nil;

    DoBorderChanged;
  end;
end;

procedure TSCDeCustomScrollControl.ScrollerPositionChanged(
  Sender: TSCDeCustomControlScrollbar);
begin
  if Sender <> nil then
  begin
    RedrawScrollbars;
    DoScrollerPositionChanged(Sender.Kind);

    if Assigned(FOnScrollChangeEvent) then
      FOnScrollChangeEvent(Self, Sender.Kind);
  end;
end;

procedure TSCDeCustomScrollControl.ScrollerPositionChanging(
  Sender: TSCDeCustomControlScrollbar; var ScrollPos: Integer;
  var CanScroll: Boolean);
begin
  if Sender <> nil then
  begin
    DoScrollerPositionChanging(Sender.Kind, Sender.Position,
      ScrollPos, CanScroll);

    if Assigned(FOnScroll) then
      FOnScroll(Self, Sender.Kind, Sender.Position, ScrollPos, CanScroll);
  end;
end;

procedure TSCDeCustomScrollControl.ScrollerVisibleChanged(Sender: TSCDeCustomControlScrollbar);
begin
  Perform(CM_BORDERCHANGED, 0, 0);
end;

function TSCDeCustomScrollControl.ScrollingBars: Boolean;
begin
  Result := FScrolling and (FScrollTimer <> -1) and
    (CanScrollBars(True) or CanScrollBars(False));
end;

function TSCDeCustomScrollControl.ScrollingPaused: Boolean;
begin
  Result := not FScrolling and (FScrollTimer <> -1) and
    (CanScrollBars(True) or CanScrollBars(False));
end;

procedure TSCDeCustomScrollControl.SetScrollbarHeight(Value: Integer);
begin
  if Value < 0 then Value := -1
  else
  if Value < 12 then Value := 12
  else
  if Value > 36 then Value := 36;

  if FScrollbarHeight <> Value then
  begin
    if FScrollbarHorz.FButtonSize = Value then
       FScrollbarHorz.FButtonSize := Value;

    if FScrollbarVert.FButtonSize = Value then
       FScrollbarVert.FButtonSize := Value;

    FScrollbarHeight := Value;

    if FScrollbarHorz.Visible or FScrollbarVert.Visible then
      DoBorderChanged;
  end;
end;

procedure TSCDeCustomScrollControl.SetScrollbarHorz(Value: TSCDeCustomControlScrollbar);
begin
  FScrollbarHorz.Assign(Value);
end;

procedure TSCDeCustomScrollControl.SetScrollbarStyle(Value: TSCDeScrollbarStyle);
begin
  FScrollbarHorz.FStyle := Value;
  FScrollbarVert.FStyle := Value;

  if FScrollbarStyle <> Value then
  begin
    FScrollbarStyle := Value;
    RedrawScrollbars;
  end;
end;

procedure TSCDeCustomScrollControl.SetScrollbarThumbLines(Value: TSCDeScrollThumbline);
begin
  FScrollbarHorz.FThumbLines := Value;
  FScrollbarVert.FThumbLines := Value;

  if FScrollbarThumbLines <> Value then
  begin
    FScrollbarThumbLines := Value;
    RedrawScrollbars;
  end;
end;

procedure TSCDeCustomScrollControl.SetScrollbarVert(
  Value: TSCDeCustomControlScrollbar);
begin
  FScrollbarVert.Assign(Value);
end;

procedure TSCDeCustomScrollControl.SetScrollButtonsLayout(
  Value: TSCDeScrollButtonLayout);
begin
  FScrollbarHorz.FButtonLayout := Value;
  FScrollbarVert.FButtonLayout := Value;

  if FScrollButtonsLayout <> Value then
  begin
    FScrollButtonsLayout := Value;
    RedrawScrollbars;
  end;
end;

procedure TSCDeCustomScrollControl.SetScrollbarInfo(Kind: TSCDeScrollbarKind;
  Info: TSCDeScrollInfo);
var
  Si: TSCDeScrollInfo;
  Sb: TSCDeCustomControlScrollbar;
begin
  Si := GetScrollbarInfo(Kind);
  if (Info.Min <> Si.Min) or (Info.Max <> Si.Max) or (Info.Page <> Si.Page) or
    (Info.Pos <> Si.Pos) or (Info.Visible <> Si.Visible) or (Info.Tracking <> Si.Tracking) or
    (Info.Enabled <> Si.Enabled) or (Info.ButtonSize <> Si.ButtonSize) or
    (Info.ThumbSize <> Si.ThumbSize) or (Info.LargeChange <> Si.LargeChange) or
    (Info.SmallChange <> Si.SmallChange) or (Info.LeftButtonEnabled <> Si.LeftButtonEnabled) or
    (Info.LeftButtonVisible <> Si.LeftButtonVisible) or (Info.RightButtonEnabled <> Si.RightButtonEnabled) or
    (Info.RightButtonVisible <> Si.RightButtonEnabled) or (Info.ThumbEnabled <> Si.ThumbEnabled) or
    (Info.ThumbVisible <> Si.ThumbVisible) and (Info.TrimPageSize <> Si.TrimPageSize) then
  begin
    Sb := FScrollbarHorz;
    if Kind = scdskVertical then
      Sb := FScrollbarVert;

    if Info.LargeChange < Low(TSCDeScrollbarInc) then
      Info.LargeChange := Low(TSCDeScrollbarInc);

    if Info.LargeChange > High(TSCDeScrollbarInc) then
      Info.LargeChange := High(TSCDeScrollbarInc);

    if Info.SmallChange < Low(TSCDeScrollbarInc) then
      Info.SmallChange := Low(TSCDeScrollbarInc);

    if Info.SmallChange > High(TSCDeScrollbarInc) then
      Info.SmallChange := High(TSCDeScrollbarInc);

    Sb.BeginUpdate;
    try
      with Sb do
      begin
        Min  := Info.Min;
        Max  := Info.Max;
        PageSize := Info.Page;
        Position := Info.Pos;
        ButtonSize  := Info.ButtonSize;
        ThumbSize   := Info.ThumbSize;
        LargeChange := Info.LargeChange;
        SmallChange := Info.SmallChange;
        Visible     := Info.Visible;
        Enabled     := Info.Enabled;
        Smooth      := Info.Smooth;
        ButtonLeft.Enabled  := Info.LeftButtonEnabled;
        ButtonLeft.Visible  := Info.LeftButtonVisible;
        ButtonRight.Enabled := Info.RightButtonEnabled;
        ButtonRight.Visible := Info.RightButtonVisible;
        Track         := Info.Tracking;
        Thumb.Enabled := Info.ThumbEnabled;
        Thumb.Visible := Info.ThumbVisible;
        TrimPageSize  := Info.TrimPageSize;
      end;
    finally
      Sb.EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomScrollControl.StartScrollingBars;
var
  P: TPoint;
  SbTest: TSCDeScrollbarHittest;
begin
  if HandleAllocated and (CanScrollBars(False) or CanScrollBars(True)) then
  begin
    FKeyboardSpeed := scdKeyBoardSpeed;

    if not Enabled then
    begin
      StopScrollingBars;
      Exit;
    end;

    if ScrollingPaused then
    begin
      ResumeScrollingBars;
      Exit;
    end;

    if not ScrollingBars then
    begin
      SbTest := FDownTest;

      StopScrollingBars;
      FDownTest := SbTest;

      FScrollTimer := SetTimer(Handle, SCDE_SCRLBAR_SCROLLTIMERID, scdKeyBoardDelay, nil);
      FScrolling   := FScrollTimer <> -1;

      ScrollControl;
      if not GetCursorPos(P) then
        StopScrollingBars
      else begin
        P := Self.ScreenToNC(P);
        FHotTest := GetScrollbarHittest(P);
      end;
    end;
  end;
end;

procedure TSCDeCustomScrollControl.StopScrollingBars;
var
  HitPart: TSCDeScrollerHitPart;
  DrawKind: TSCDeScrollbarDrawKind;
begin
  FScrolling := False;

  DrawKind := scsdkAll;
  if FDownTest.Kind = scshkHorizontal then
    DrawKind := scsdkHorizontal
  else
  if FDownTest.Kind = scshkVertical then
    DrawKind := scsdkVertical;

  HitPart := scspNone;
  if FDownTest.Kind = scshkNone then
    HitPart := FDownTest.HitPart;
    
  ResetHittest(@FDownTest);

  if FScrollTimer <> -1 then
  begin
    FKeyboardSpeed := 0;

    if HandleAllocated then
      KillTimer(Handle, FScrollTimer);

    FScrollTimer := -1;
    if HitPart <> scspNone then
      RedrawScrollbars(DrawKind);
  end;
end;

procedure TSCDeCustomScrollControl.StopTracking;
begin
  StopScrollingBars;
  inherited StopTracking;
end;

procedure TSCDeCustomScrollControl.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
  P: TPoint;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    P := ScreenToNC(SmallPointToPoint(Message.Pos));

    if (Message.Result = HTNOWHERE) and HandleAllocated then
    begin
      if FDownTest.Kind <> scshkNone then
      begin
        Message.Result := HTBORDER;
        Exit;
      end;

      if (FScrollbarHorz <> nil) and FScrollbarHorz.Visible then
      begin
        R := GetHorzScrollbarRect;

        if not IsRectEmpty(R) and PtInRect(R, P) then
        begin
          Message.Result := HTBORDER;
          Exit;
        end;
      end;

      if (FScrollbarVert <> nil) and FScrollbarVert.Visible then
      begin
        R := GetVertScrollbarRect;

        if not IsRectEmpty(R) and PtInRect(R, P) then
        begin
          Message.Result := HTBORDER;
          Exit;
        end;
      end;
    end;
  end;  
end;

procedure TSCDeCustomScrollControl.WMSettingChange(var Message: TMessage);
begin
  inherited;

  FScrollbarHorz.RearrangeDefaults;
  FScrollbarVert.RearrangeDefaults;

  RecreateWnd;
end;

procedure TSCDeCustomScrollControl.WMTimer(var Message: TWMTimer);
var
  P: TPoint;
  Ps, Mx: Integer;
  DrawKind: TSCDeScrollbarDrawKind;
  Sb: TSCDeCustomControlScrollbar;
begin
  inherited;

  if HandleAllocated and ScrollingBars and
    (Message.TimerID = SCDE_SCRLBAR_SCROLLTIMERID) then
  begin
    if FKeyboardSpeed = 0 then
      FKeyboardSpeed := scdKeyBoardSpeed;

    if FKeyboardSpeed = 0 then
      FKeyboardSpeed := 30;

    if not (Enabled and GetCursorPos(P)) then
    begin
      StopScrollingBars;
      Exit;
    end;

    if IsScrollingReady then
    begin
      Sb := FScrollbarHorz;
      DrawKind := scsdkHorizontal;

      if FDownTest.Kind = scshkVertical then
      begin
        Sb := FScrollbarVert;
        DrawKind := scsdkVertical;
      end;

      Ps := Sb.Position;

      P := Self.ScreenToNC(P);
      FHotTest := GetScrollbarHittest(P);

      ScrollControl;

      if FScrollTimer <> -1 then
        KillTimer(Handle, FScrollTimer);

      Mx := Sb.GetMaximumValue;

      if (Ps = Sb.Position) and (Sb.FMin < Mx) and (Ps > Sb.FMin) and
        (Ps < Mx) and not SameHittests(@FDownTest, @FHotTest) then
        RedrawScrollbars(DrawKind, 0);

      FScrollTimer := SetTimer(Handle, SCDE_SCRLBAR_SCROLLTIMERID, FKeyboardSpeed, nil);
      FScrolling := FScrollTimer <> -1;
    end;  
  end;
end;

function TSCDeCustomScrollControl.IsHorzScrollBarVisible: Boolean;
begin
  Result := HandleAllocated and FScrollbarHorz.Visible;
end;

function TSCDeCustomScrollControl.IsVertScrollBarVisible: Boolean;
begin
  Result := HandleAllocated and FScrollbarVert.Visible;
end;

function TSCDeCustomScrollControl.CanScrollToPos(Kind: TSCDeScrollbarKind;
  var NewValue: Integer): Boolean;
begin
  Result := True;
end;

function TSCDeCustomScrollControl.GetControlScrollbarsClass: TSCDeControlCustomScrollbarsClass;
begin
  Result := TSCDeControlScrollbars;
end;

procedure TSCDeCustomScrollControl.SetScrollbars(Value: TSCDeControlCustomScrollbars);
begin
  FScrollbars.Assign(Value);
end;

procedure TSCDeCustomScrollControl.EnabledChanged;
begin
  StopScrollingBars;
  inherited EnabledChanged;
end;

procedure TSCDeCustomScrollControl.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TSCDeCustomScrollControl then
  begin
    with TSCDeCustomScrollControl(Source) do
    begin
      Self.Scrollbars := Scrollbars;
      Self.ScrollbarHeight := ScrollbarHeight;
      Self.ScrollbarHorz := ScrollbarHorz;
      Self.ScrollbarVert := ScrollbarVert;
      Self.ScrollbarStyle := ScrollbarStyle;
      Self.ScrollbarThumbLines := ScrollbarThumbLines;
      Self.ScrollButtonsLayout := ScrollButtonsLayout;
    end;
  end;
end;

procedure TSCDeCustomScrollControl.ScrollerPositionChanged(
  Sender: TSCDeCustomControlScrollbar; OldPos, NewPos: Integer);
begin
  if Sender <> nil then
    DoScrollerPositionChanged(Sender.Kind, OldPos, NewPos);
end;

procedure TSCDeCustomScrollControl.DoScrollerPositionChanged(
  Kind: TSCDeScrollbarKind; OldPos, NewPos: Integer);
begin
  //
end;

procedure TSCDeCustomScrollControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if HandleAllocated and ((FScrollbarHorz.Style = scssFlatHover) or
    (FScrollbarVert.Style = scssFlatHover)) then
    RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if HandleAllocated and ((FScrollbarHorz.Style = scssFlatHover) or
    (FScrollbarVert.Style = scssFlatHover)) then
    RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  inherited;
  if HandleAllocated and ((FScrollbarHorz.Style = scssFlatHover) or
    (FScrollbarVert.Style = scssFlatHover)) then
    RedrawScrollbars;
end;

procedure TSCDeCustomScrollControl.SetScrollbarExtraButton(Value: Boolean);
begin
  if FScrollbarExtraButton <> Value then
  begin
    FScrollbarExtraButton := Value;

    FScrollbarHorz.FButtonExtra.Visible := Value;
    FScrollbarVert.FButtonExtra.Visible := Value;
  end;
end;

procedure TSCDeCustomScrollControl.WMHScroll(var Message: TWMHScroll);
begin
  if (Message.ScrollBar = 0) and FScrollbarHorz.Visible then
    FScrollbarHorz.ScrollMessage(Message) else
    inherited;
end;

procedure TSCDeCustomScrollControl.WMVScroll(var Message: TWMVScroll);
begin
  if (Message.ScrollBar = 0) and FScrollBarVert.Visible then
    FScrollBarVert.ScrollMessage(Message) else
    inherited;
end;

{ TSCDeScrollbarPart }

procedure TSCDeScrollbarPart.Assign(Source: TPersistent);
begin
  if Source is TSCDeScrollbarPart then
  begin
    with TSCDeScrollbarPart(Source) do
    begin
      Self.FBlend := Blend;
      Self.FColor := Color;
      Self.FDisabledColor := DisabledColor;
      Self.FDownColor := DownColor;
      Self.FEnabled := Enabled;
      Self.FHotColor := HotColor;
      Self.FVisible := Visible;
    end;
    DoChanged;
  end else
    inherited Assign(Source);
end;

constructor TSCDeScrollbarPart.Create(AOwner: TSCDeCustomControlScrollbar);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clScrollBar;
  FDisabledColor := clScrollBar;
  FDownColor := clScrollBar;
  FEnabled := True;
  FHotColor := clScrollBar;
  FVisible := True;
end;

procedure TSCDeScrollbarPart.DoChanged;
begin
  if FOwner is TSCDeCustomControlScrollbar then
    TSCDeCustomControlScrollbar(FOwner).PartChanged(Self);
end;

function TSCDeScrollbarPart.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeScrollbarPart.SetBlend(Value: Boolean);
begin
  if FBlend <> Value then
  begin
    FBlend := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetDownColor(Value: TColor);
begin
  if FDownColor <> Value then
  begin
    FDownColor := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    DoChanged;
  end;
end;

procedure TSCDeScrollbarPart.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChanged;
  end;
end;

{ TSCDeScrollbarIcons }

constructor TSCDeScrollbarIcons.Create(AOwner: TSCDeCustomControlScrollbar);
begin
  inherited Create(AOwner);
  FColor := clWindowText;
  FDisabledColor := clGrayText;
  FDownColor := clWindowText;
  FHotColor := clWindowText;
end;

{ TSCDeControlScrollbar }

constructor TSCDeControlScrollbar.Create(AOwner: TSCDeCustomControl;
  AKind: TSCDeScrollbarKind);
begin
  inherited Create(AOwner, AKind);
  FVisible := False;
end;

procedure TSCDeControlScrollbar.DrawBack(C: TCanvas; InRect: TRect);
var
  Cl, C1: TColor;
  BR, R, R1: TRect;
begin
  if (C = nil) or IsRectEmpty(InRect) then
    Exit;

  R  := InRect;
  Cl := GetBackColor;
  if FStyle = scssOffice12 then
  begin
    Cl := Background.Color;
    if Cl = clNone then
      Cl := clScrollBar;

    Cl := SCDECommon.scdBlendColor(Cl, 12);
  end;

  with C do
  begin
    Brush.Color := Cl;
    FillRect(R);
  end;

  BR := GetBackRect(R);

  if FStyle = scssOffice12 then
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 36);

    if FKind = scdskHorizontal then
    begin
      R1 := InRect;
      R1.Bottom := R1.Top + ((R1.Bottom - R1.Top) div 5);

      if not IsRectEmpty(R1) then
        scdDrawGradient(C, R1, scdgTopToBottom, C1, Cl);
    end else
    begin
      R1 := InRect;
      R1.Right := R1.Left + ((R1.Right - R1.Left) div 5);

      if not IsRectEmpty(R1) then
        scdDrawGradient(C, R1, scdgLeftToRight, C1, Cl);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawBorder(C: TCanvas; R: TRect);
var
  Cl: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then Exit;

  if FStyle = scssOffice12 then
    DrawOffice12Border(C, R)
  else
  if FStyle = scssMac then
  begin
    Cl := scdGet3DDkShadowOf(GetBackColor);
    scdFrame3D(C, R, Cl, Cl, 1, 0);
  end else
  if FStyle = scssMetal then
  begin
    Cl := scdGetBtnShadowOf(GetBackColor);
    scdFrame3D(C, R, Cl, Cl, 1, 0);
  end;
end;

procedure TSCDeControlScrollbar.DrawExtraButton(C: TCanvas; InRect: TRect);
var
  R: TRect;
  W, H, I: Integer;
  OffP, P: TPoint;
  C1, C2, C3, C4: TColor;

  procedure DrawButtonIcon;
  begin
    OffP := Point(0, 0);
    if (PressedPart = scspExtraButton) and (HotPart = PressedPart) and
      not (FStyle in [scssOffice2k, scssXP, scssXP2, scssFlatEx])  then
    begin
      OffP := Point(1, 1);

      if FStyle = scssMac then
      begin
        if FKind = scdskHorizontal then
          OffP := Point(-1, 0)
        else
          OffP := Point(0, -1);
      end;
    end;

    if C2 <> C1 then
    begin
      W := 6; H := 3;

      I := R.Right - R.Left;
      if I > R.Bottom - R.Top then
        I := R.Bottom - R.Top;

      if I < 16 then
      begin
        W := 4; H := 2;
        if I < 12 then
        begin
          W := 2; H := 1;
        end;
      end;

      if FKind = scdskHorizontal then
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) - 2 + OffP.x;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) + OffP.y;

        scdDrawRightSlider(C, P, W, H, sctsMac, False, C2, C2);
      end else
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) + OffP.x;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) - 2 + OffP.y;

        scdDrawDownSlider(C, P, W, H, sctsMac, False, C2, C2);
      end;
    end;
  end;

begin
  if (C = nil) or IsRectEmpty(InRect) or
    not FButtonLeft.Visible then
    Exit;

  R := GetExtraButtonRect(InRect);
  if IsRectEmpty(R) then
    Exit;

  C1 := GetExtraBtnColor;
  C2 := GetExtraBtnIconColor;

  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := C1;

    FillRect(R);
  end;

  DrawButtonIcon;

  case FStyle of
    scssDefault, scssDefaultEx:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspExtraButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlat:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspExtraButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlatEx:
    begin
      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssFlatHover:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspExtraButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      if Focused or Hovered then
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssMetal:
    begin
      InflateRect(R, -1, -1);

      if (HotPart = PressedPart) and
        (PressedPart = scspExtraButton) then
      begin
        C3 := C1;
        // C4 := scdGetBtnHighlightOf(C1);
        C4 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C3 := SCDECommon.scdBlendColor(C1, 48);
        // C3 := scdGetBtnHighlightOf(C1);
        C4 := C1;
      end;

      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C3, C4, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice2k:
    begin
      C3 := clHighlight;
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice12:
    begin
      DrawBack(C, R);
      DrawOffice12Border(C, R);

      if HotPart <> scspNone then
        scdDrawOffice12Face(Kind, C, R, C1, GetBackColor, True,
          PressedPart = scspExtraButton, HotPart = scspExtraButton,
          True, False);

      DrawButtonIcon;
    end;
    scss3D, scss3DX:
    begin
      scdDraw3DFace(Kind, C, R, C1, GetBackColor, FStyle = scss3DX,
        PressedPart = scspExtraButton, HotPart = scspExtraButton);

      DrawButtonIcon;
    end;
    scssNew, scssNewX:
    begin
      scdDrawNewFace(Kind, C, R, C1, GetBackColor, FStyle = scssNewX,
        PressedPart = scspExtraButton, HotPart = scspExtraButton);

      DrawButtonIcon;
    end;
    scssXP, scssXP2:
    begin
      DrawXPFace(C, R, C1, GetBackColor, False, PressedPart = scspExtraButton,
        HotPart = scspExtraButton);

      with C do
      begin
        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C2;

        W := 4;

        I := R.Right - R.Left;
        if I > R.Bottom - R.Top then
          I := R.Bottom - R.Top;

        if I < 14 then
        begin
          W := 3;
          if I < 8 then
            W := 2;
        end;

        if FKind = scdskHorizontal then
        begin
          Dec(P.x);
          for I := 0 to W - 2 do
          begin
            MoveTo(P.x + (W - 1), P.y - (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x + W, P.y + W);

            Inc(P.x);
          end;
        end else
        begin
          Dec(P.y);
          for I := 0 to W - 2 do
          begin
            MoveTo(P.x - (W - 1), P.y + (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x + W, P.y + W);

            Inc(P.y);
          end;
        end;
      end;
    end;
    scssMac:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspExtraButton) then
      begin
        C2 := SCDECommon.scdBlendColor(C1, -48);
        C3 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C2 := SCDECommon.scdBlendColor(C1, 48);
        C3 := SCDECommon.scdBlendColor(C1, -48);
      end;

      InflateRect(R, -1, -1);
      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C2, C3, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGet3DDkShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawLeftButton(C: TCanvas; InRect: TRect);
var
  R: TRect;
  W, H, I: Integer;
  OffP, P: TPoint;
  C1, C2, C3, C4: TColor;

  procedure DrawButtonIcon;
  begin
    OffP := Point(0, 0);
    if (PressedPart = scspLeftButton) and (HotPart = PressedPart) and
      not (FStyle in [scssOffice2k, scssXP, scssXP2, scssFlatEx])  then
    begin
      OffP := Point(1, 1);

      if FStyle = scssMac then
      begin
        if FKind = scdskHorizontal then
          OffP := Point(-1, 0)
        else
          OffP := Point(0, -1);
      end;
    end;

    if C2 <> C1 then
    begin
      W := 6; H := 3;

      I := R.Right - R.Left;
      if I > R.Bottom - R.Top then
        I := R.Bottom - R.Top;

      if I < 16 then
      begin
        W := 4; H := 2;
        if I < 12 then
        begin
          W := 2; H := 1;
        end;
      end;

      if FKind = scdskHorizontal then
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) - 2 + OffP.x;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) + OffP.y;

        scdDrawRightSlider(C, P, W, H, sctsMac, False, C2, C2);
      end else
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) + OffP.x;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) - 2 + OffP.y;

        scdDrawDownSlider(C, P, W, H, sctsMac, False, C2, C2);
      end;
    end;
  end;

begin
  if (C = nil) or IsRectEmpty(InRect) or
    not FButtonLeft.Visible then
    Exit;

  R := GetLeftButtonRect(InRect);
  if IsRectEmpty(R) then
    Exit;

  C1 := GetLeftBtnColor;
  C2 := GetLeftBtnIconColor;

  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := C1;

    FillRect(R);
  end;

  DrawButtonIcon;

  case FStyle of
    scssDefault, scssDefaultEx:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspLeftButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlat:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspLeftButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlatEx:
    begin
      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssFlatHover:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspLeftButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      if Focused or Hovered then
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssMetal:
    begin
      InflateRect(R, -1, -1);

      if (HotPart = PressedPart) and
        (PressedPart = scspLeftButton) then
      begin
        C3 := C1;
        // C4 := scdGetBtnHighlightOf(C1);
        C4 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C3 := SCDECommon.scdBlendColor(C1, 48);
        // C3 := scdGetBtnHighlightOf(C1);
        C4 := C1;
      end;

      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C3, C4, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice2k:
    begin
      C3 := clHighlight;
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice12:
    begin
      DrawBack(C, R);
      DrawOffice12Border(C, R);

      if HotPart <> scspNone then
        scdDrawOffice12Face(Kind, C, R, C1, GetBackColor, True,
          PressedPart = scspLeftButton, HotPart = scspLeftButton,
          True, False);

      DrawButtonIcon;
    end;
    scss3D, scss3DX:
    begin
      scdDraw3DFace(Kind, C, R, C1, GetBackColor, FStyle = scss3DX,
        PressedPart = scspLeftButton, HotPart = scspLeftButton);

      DrawButtonIcon;
    end;
    scssNew, scssNewX:
    begin
      scdDrawNewFace(Kind, C, R, C1, GetBackColor, FStyle = scssNewX,
        PressedPart = scspLeftButton, HotPart = scspLeftButton);

      DrawButtonIcon;
    end;
    scssXP, scssXP2:
    begin
      DrawXPFace(C, R, C1, GetBackColor, False, PressedPart = scspLeftButton,
        HotPart = scspLeftButton);

      with C do
      begin
        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C2;

        W := 4;

        I := R.Right - R.Left;
        if I > R.Bottom - R.Top then
          I := R.Bottom - R.Top;

        if I < 14 then
        begin
          W := 3;
          if I < 8 then
            W := 2;
        end;

        if FKind = scdskHorizontal then
        begin
          Dec(P.x);
          for I := 0 to W - 2 do
          begin
            MoveTo(P.x + (W - 1), P.y - (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x + W, P.y + W);

            Inc(P.x);
          end;
        end else
        begin
          Dec(P.y);
          for I := 0 to W - 2 do
          begin
            MoveTo(P.x - (W - 1), P.y + (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x + W, P.y + W);

            Inc(P.y);
          end;
        end;
      end;
    end;
    scssMac:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspLeftButton) then
      begin
        C2 := SCDECommon.scdBlendColor(C1, -48);
        C3 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C2 := SCDECommon.scdBlendColor(C1, 48);
        C3 := SCDECommon.scdBlendColor(C1, -48);
      end;

      InflateRect(R, -1, -1);
      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C2, C3, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGet3DDkShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawOffice12Border(C: TCanvas; R: TRect);
var
  Cl: TColor;
begin
  if (FStyle = scssOffice12) and (C <> nil) and not IsRectEmpty(R) then
  begin
    Cl := FBackground.FColor;
    if not Enabled then
      Cl := FBackground.FDisabledColor;

    if Cl = clNone then
      Cl := clScrollBar;

    Cl := SCDECommon.scdBlendColor(Cl, 12);

    with C do
    begin
      Pen.Style := psSolid;
      Pen.Mode  := pmCopy;
      Pen.Color := Cl;
      Pen.Width := 1;
    end;

    if FKind = scdskHorizontal then
    begin
      C.MoveTo(R.Left, R.Top);
      C.LineTo(R.Right, R.Top);

      // C.MoveTo(R.Left, R.Bottom - 1);
      // C.LineTo(R.Right, R.Bottom - 1);
    end else
    begin
      C.MoveTo(R.Left, R.Top);
      C.LineTo(R.Left, R.Bottom);

      // C.MoveTo(R.Right - 1, R.Top);
      // C.LineTo(R.Right - 1, R.Bottom);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawRightButton(C: TCanvas; InRect: TRect);
var
  R: TRect;
  W, H, I: Integer;
  OffP, P: TPoint;
  C1, C2, C3, C4: TColor;

  procedure DrawButtonIcon;
  begin
    OffP := Point(0, 0);
    if (PressedPart = scspRightButton) and (HotPart = PressedPart) and
      not (FStyle in [scssOffice2k, scssXP, scssXP2, scssFlatEx]) then
    begin
      OffP := Point(1, 1);

      if FStyle = scssMac then
      begin
        if FKind = scdskHorizontal then
          OffP := Point(1, 0)
        else
          OffP := Point(0, 1);
      end;
    end;

    if C2 <> C1 then
    begin
      W := 6; H := 3;

      I := R.Right - R.Left;
      if I > R.Bottom - R.Top then
        I := R.Bottom - R.Top;

      if I < 16 then
      begin
        W := 4; H := 2;
        if I < 12 then
        begin
          W := 2; H := 1;
        end;
      end;

      if FKind = scdskHorizontal then
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) + 1 + OffP.X;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) + OffP.y;

        scdDrawLeftSlider(C, P, W, H, sctsMac, False, C2, C2);
      end else
      begin
        P.x := R.Left + ((R.Right - R.Left) div 2) + OffP.x;
        P.y := R.Top +  ((R.Bottom - R.Top) div 2) + 1 + OffP.y;

        scdDrawUpSlider(C, P, W, H, sctsMac, False, C2, C2);
      end;
    end;
  end;

begin
  if (C = nil) or IsRectEmpty(InRect) or
    not FButtonRight.Visible then
    Exit;

  R := GetRightButtonRect(InRect);
  if IsRectEmpty(R) then
    Exit;

  C1 := GetRightBtnColor;
  C2 := GetRightBtnIconColor;

  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := C1;

    FillRect(R);
  end;

  DrawButtonIcon;

  case FStyle of
    scssDefault, scssDefaultEx:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspRightButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlat:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspRightButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssFlatEx:
    begin
      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssFlatHover:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspRightButton) then
      begin
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C3, 1, 0);
      end else
      if Focused or Hovered then
      begin
        C3 := C1;
        C4 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);

        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end else
      begin
        C3 := scdGetBtnHighlightOf(C1);
        C4 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C3, C4, 1, 0);
      end;
    end;
    scssMetal:
    begin
      InflateRect(R, -1, -1);

      if (HotPart = PressedPart) and
        (PressedPart = scspRightButton) then
      begin
        C3 := C1;
        // C4 := scdGetBtnHighlightOf(C1);
        C4 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C3 := SCDECommon.scdBlendColor(C1, 48);
        // C3 := scdGetBtnHighlightOf(C1);
        C4 := C1;
      end;

      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C3, C4, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice2k:
    begin
      C3 := clHighlight;
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssOffice12:
    begin
      DrawBack(C, R);
      DrawOffice12Border(C, R);

      if HotPart <> scspNone then
        scdDrawOffice12Face(Kind, C, R, C1, GetBackColor, True,
          PressedPart = scspRightButton, HotPart = scspRightButton,
          True, False);

      DrawButtonIcon;
    end;
    scss3D, scss3DX:
    begin
      scdDraw3DFace(Kind, C, R, C1, GetBackColor, FStyle = scss3DX,
        PressedPart = scspRightButton, HotPart = scspRightButton);

      DrawButtonIcon;
    end;
    scssNew, scssNewX:
    begin
      scdDrawNewFace(Kind, C, R, C1, GetBackColor, FStyle = scssNewX,
        PressedPart = scspRightButton, HotPart = scspRightButton);

      DrawButtonIcon;
    end;
    scssXP, scssXP2:
    begin
      DrawXPFace(C, R, C1, GetBackColor, False, PressedPart = scspRightButton,
        HotPart = scspRightButton);

      with C do
      begin
        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C2;

        W := 4;

        I := R.Right - R.Left;
        if I > R.Bottom - R.Top then
          I := R.Bottom - R.Top;

        if I < 14 then
        begin
          W := 3;
          if I < 8 then
            W := 2;
        end;

        if FKind = scdskHorizontal then
        begin
          Dec(P.x);

          for I := 0 to W - 2 do
          begin
            MoveTo(P.x - (W - 1), P.y - (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x - W, P.y + W);

            Inc(P.x);
          end;
        end else
        begin
          Dec(P.y);
          for I := 0 to W - 2 do
          begin
            MoveTo(P.x - (W - 1), P.y - (W - 1));
            LineTo(P.x, P.y);
            LineTo(P.x + W, P.y - W);

            Inc(P.y);
          end;
        end;
      end;
    end;
    scssMac:
    begin
      if (HotPart = PressedPart) and (PressedPart = scspRightButton) then
      begin
        C2 := SCDECommon.scdBlendColor(C1, -48);
        C3 := SCDECommon.scdBlendColor(C1, 48);
      end else
      begin
        C2 := SCDECommon.scdBlendColor(C1, 48);
        C3 := SCDECommon.scdBlendColor(C1, -48);
      end;

      InflateRect(R, -1, -1);
      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C2, C3, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGet3DDkShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawScrollBack(C: TCanvas; InRect: TRect);
var
  Cl, C1: TColor;
  BR, R, R1, R2: TRect;
begin
  if (C = nil) or IsRectEmpty(InRect) then
    Exit;

  R  := InRect;
  Cl := GetBackColor;

  DrawBack(C, R);

  BR := GetBackRect(R);

  if FStyle = scssSports then
  begin
    R1 := BR;

    if not IsRectEmpty(R1) then
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := scdGetBtnShadowOf(Cl);

        FillRect(R1);

        scdFrame3D(C, R1, scdGet3DDkShadowOf(Cl), scdGetBtnHighlightOf(Cl), 1, 0);
      end;
  end else
  if (HotPart = PressedPart) and (PressedPart in [scspLeftSpare, scspRightSpare]) then
  begin
    R2 := GetThumbRect(R);
    R1 := R;

    if not IsRectEmpty(R2) then
    begin
      if FKind = scdskHorizontal then
      begin
        if PressedPart = scspLeftSpare then
        begin
          if R2.Left >= R.Left then
            R1.Right := R2.Left;
        end else
        if R2.Right <= R.Right then
          R1.Left := R2.Right;
      end else
      begin
        if PressedPart = scspLeftSpare then
        begin
          if R2.Top >= R.Top then
            R1.Bottom := R2.Top;
        end else
        if R2.Bottom <= R.Bottom then
          R1.Top := R2.Bottom;
      end;
    end;

    if FStyle <> scssMetal then
    begin
      Cl := FBackground.FDownColor;
      if FStyle = scssOffice2k then
        Cl := scdGetOfficeXPDownedSelColor
      else
      if FBackground.FBlend then
        Cl := scdBlendedColor(scdGetOfficeXPBtnColorOf(FBackground.FDownColor), 24, 24, 24, True);

      with C do
      begin
        Brush.Color := Cl;
        FillRect(R1);
      end;

      if FStyle = scssOffice12 then
      begin
        C1 := SCDECommon.scdBlendColor(Cl, 36);

        if FKind = scdskHorizontal then
        begin
          R2 := R1;
          R2.Bottom := R2.Top + ((R2.Bottom - R2.Top) div 5);

          if not IsRectEmpty(R2) then
            scdDrawGradient(C, R2, scdgTopToBottom, C1, Cl);

          R2 := R1;
          R2.Top := R2.Bottom - ((R2.Bottom - R2.Top) div 5);

          if not IsRectEmpty(R2) then
            scdDrawGradient(C, R2, scdgTopToBottom, Cl, C1);
        end else
        begin
          R2 := R1;
          R2.Right := R2.Left + ((R2.Right - R2.Left) div 5);

          if not IsRectEmpty(R2) then
            scdDrawGradient(C, R2, scdgLeftToRight, C1, Cl);

          R2 := R1;
          R2.Left := R2.Right - ((R2.Right - R2.Left) div 5);

          if not IsRectEmpty(R2) then
            scdDrawGradient(C, R2, scdgLeftToRight, Cl, C1);
        end;
      end;
    end;
  end;

  if FSlideLine then
  begin
    R1 := BR;

    if FKind = scdskHorizontal then
    begin
      InflateRect(R1, -4, 0);

      if R1.Bottom - R1.Top > 3 then
      begin
        R1.Top := R1.Top + ((R1.Bottom - R1.Top) div 2);
        R1.Bottom := R1.Top + 1;

        if not IsRectEmpty(R1) then
          with C do
          begin
            Pen.Style := psSolid;

            Pen.Color := scdGetBtnShadowOf(Cl);
            MoveTo(R1.Left, R1.Top);
            LineTo(R1.Right, R1.Top);

            Pen.Color := scdGetBtnHighlightOf(Cl);
            MoveTo(R1.Left, R1.Bottom);
            LineTo(R1.Right, R1.Bottom);
          end;
      end;
    end else
    begin
      InflateRect(R1, 0, -4);

      if R1.Right - R1.Left > 3 then
      begin
        R1.Left := R1.Left + ((R1.Right - R1.Left) div 2);
        R1.Right := R1.Left + 1;

        if not IsRectEmpty(R1) then
          with C do
          begin
            Pen.Style := psSolid;

            Pen.Color := scdGetBtnShadowOf(Cl);
            MoveTo(R1.Left, R1.Top);
            LineTo(R1.Left, R1.Bottom);

            Pen.Color := scdGetBtnHighlightOf(Cl);
            MoveTo(R1.Right, R1.Top);
            LineTo(R1.Right, R1.Bottom);
          end;
      end;
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawThumb(C: TCanvas; InRect: TRect);
var
  R1, R: TRect;
  Rgn, PrevRgn: HRgn;
  HasPrevRgn: Boolean;
  C1, C2, C3, Cl: TColor;
  I, J, K, L, X, Y, SR: Integer;
begin
  if (C = nil) or IsRectEmpty(InRect) or
    not (Enabled and FThumb.Visible) then
    Exit;

  R := GetThumbRect(InRect);
  if IsRectEmpty(R) then
    Exit;

  C1 := GetThumbColor;

  with C do
  begin
    R1 := R;
    if FStyle = scssSports then
      InflateRect(R1, -1, -1);

    Brush.Color := C1;
    FillRect(R1);
  end;

  if FThumbLines <> sctlNone then
    DrawThumbLines(C, R, C1);

  case FStyle of
    scssDefault:
    begin
      C2 := C1;
      C3 := scdGet3DDkShadowOf(C1);
      scdFrame3D(C, R, C2, C3, 1, 0);

      C2 := scdGetBtnHighlightOf(C1);
      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C2, C3, 1, 0);
    end;
    scssDefaultEx:
    begin
      if PressedPart = scspThumb then
      begin
        C2 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C2, 1, 0);
      end else
      begin
        C2 := C1;
        C3 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);

        C2 := scdGetBtnHighlightOf(C1);
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);
      end;
    end;
    scssFlat:
    begin
      if PressedPart = scspThumb then
      begin
        C2 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C2, 1, 0);
      end else
      begin
        C2 := scdGetBtnHighlightOf(C1);
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);
      end;
    end;
    scssFlatEx:
    begin
      C2 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C2, C2, 1, 0);
    end;
    scssFlatHover:
    begin
      if PressedPart = scspThumb then
      begin
        C2 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C2, 1, 0);
      end else
      if Focused or Hovered then
      begin
        C2 := C1;
        C3 := scdGet3DDkShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);

        C2 := scdGetBtnHighlightOf(C1);
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);
      end else
      begin
        C2 := scdGetBtnHighlightOf(C1);
        C3 := scdGetBtnShadowOf(C1);
        scdFrame3D(C, R, C2, C3, 1, 0);
      end;
    end;
    scssOffice2k:
    begin
      C2 := clHighlight;
      scdFrame3D(C, R, C2, C2, 1, 0);
    end;
    scssOffice12:
    begin
      scdDrawOffice12Face(Kind, C, R, C1, GetBackColor, False,
        PressedPart = scspThumb, (HotPart = scspThumb) or (PressedPart = scspThumb),
        True, True);

      DrawThumbLines(C, R, C1);
    end;
    scss3D, scss3DX:
    begin
      scdDraw3DFace(Kind, C, R, C1, GetBackColor, FStyle = scss3DX,
        PressedPart = scspThumb, PressedPart = scspThumb);

      if (FThumbLines <> sctlNone) or (FStyle = scssMac) then
        DrawThumbLines(C, R, C1);
    end;
    scssSports:
    begin
      R1 := R;
      InflateRect(R1, -1, -1);

      if not IsRectEmpty(R1) then
        scdFrame3D(C, R1, scdGetBtnHighlightOf(C1), scdGet3DDkShadowOf(C1), 1, 0);
    end;
    scssNew, scssNewX:
    begin
      scdDrawNewFace(Kind, C, R, C1, GetBackColor, FStyle = scssNewX,
        PressedPart = scspThumb, PressedPart = scspThumb);

      if (FThumbLines <> sctlNone) or (FStyle = scssMac) then
        DrawThumbLines(C, R, C1);
    end;
    scssXP, scssXP2:
    begin
      DrawXPFace(C, R, C1, GetBackColor, True, PressedPart = scspThumb,
        (HotPart = scspThumb) and (PressedPart = scspNone));

      InflateRect(R, -2, -2);
      if FKind = scdskHorizontal then
      begin
        InflateRect(R, -2, 0);
        if PressedPart = scspThumb then
          OffsetRect(R, 0, 1);
      end else
      begin
        InflateRect(R, 0, -2);
        if PressedPart = scspThumb then
          OffsetRect(R, 1, 0);
      end;

      DrawThumbLines(C, R, C1);
    end;
    scssMac:
    begin
      C2 := SCDECommon.scdBlendColor(C1, 48);
      C3 := SCDECommon.scdBlendColor(C1, -48);

      InflateRect(R, -1, -1);
      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C2, C3, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGet3DDkShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
    scssMetal:
    begin
      InflateRect(R, -3, -3);
      if not IsRectEmpty(R) then
      begin
        PrevRgn := CreateRectRgn(0, 0, 0, 0);
        try
          SR := GetClipRgn(C.Handle, PrevRgn);
          HasPrevRgn := SR > 0;

          if SR > -1 then
          begin
            if not HasPrevRgn then
            begin
              SR := IntersectClipRect(C.Handle, R.Left, R.Top, R.Right, R.Bottom);
              
              DeleteObject(PrevRgn);
              PrevRgn := 0;
            end else
            begin
              Rgn := CreateRectRgnIndirect(R);
              try
                SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_AND);
              finally
                DeleteObject(Rgn);
              end;
            end;
            
            try
              if SR <> NULLREGION then
              begin
                // C2 := scdGetBtnHighlightOf(C1);
                C2 := SCDECommon.scdBlendColor(C1, 48);
                C3 := scdGetBtnShadowOf(C1);

                C.Pen.Width := 1;
                C.Pen.Style := psSolid;

                if FKind = scdskHorizontal then
                begin
                  K := R.Right - R.Left;
                  L := (R.Bottom - R.Top) div 4;

                  for I := 0 to K do
                  begin
                    if (I = K) and not Odd(I) then
                      Break;

                    for J := 0 to L do
                    begin
                      X := I;
                      Y := 4*J;

                      Cl := C2;
                      if Odd(X) then
                      begin
                        Cl := C3;
                        Inc(Y);

                        if Odd((X - 1) div 2) then
                          Inc(Y, 2);
                      end else
                      if Odd(X div 2) then
                        Inc(Y, 2);

                      C.Pen.Color := Cl;

                      Inc(X, R.Left);
                      Inc(Y, R.Top);

                      C.MoveTo(X, Y);
                      C.LineTo(X + 1, Y);
                    end;
                  end;  
                end else
                begin
                  K := R.Bottom - R.Top;
                  L := (R.Right - R.Left) div 4;

                  for I := 0 to K do
                  begin
                    if (I = K) and not Odd(I) then
                      Break;

                    for J := 0 to L do
                    begin
                      X := 4*J;
                      Y := I;

                      Cl := C2;
                      if Odd(Y) then
                      begin
                        Cl := C3;
                        Inc(X);

                        if Odd((Y - 1) div 2) then
                          Inc(X, 2);
                      end else
                      if Odd(Y div 2) then
                        Inc(X, 2);

                      C.Pen.Color := Cl;

                      Inc(X, R.Left);
                      Inc(Y, R.Top);

                      C.MoveTo(X, Y);
                      C.LineTo(X + 1, Y);
                    end;
                  end;
                end;
              end;
            finally
              if HasPrevRgn then
                SelectClipRgn(C.Handle, PrevRgn)
              else
                SelectClipRgn(C.Handle, 0);
            end;
          end;
        finally
          if PrevRgn > 0 then
            DeleteObject(PrevRgn);
        end;
      end;

      InflateRect(R, 3, 3);

      // C2 := scdGetBtnHighlightOf(C1);
      C2 := SCDECommon.scdBlendColor(C1, 48);
      C3 := C1;

      InflateRect(R, -1, -1);
      if not IsRectEmpty(R) then
      begin
        scdFrame3D(C, R, C2, C3, 1, 0);
        InflateRect(R, 1, 1);
      end;

      InflateRect(R, 1, 1);

      C3 := scdGetBtnShadowOf(C1);
      scdFrame3D(C, R, C3, C3, 1, 0);
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawThumbLines(C: TCanvas; R: TRect; Cl: TColor);
var
  R1, R2: TRect;
  C1, C2, C3: TColor;
  Tl: TSCDeScrollThumbline;
  I, J, Cnt, L, T, X, Y, H: Integer;
begin
  if (C = nil) or IsRectEmpty(R) or
    (FThumbLines = sctlNone) or (FStyle = scssMetal) then
    Exit;

  R1 := R;
  InflateRect(R1, -3, -3);
  if IsRectEmpty(R1) then Exit;

  C1 := scdGetBtnHighlightOf(Cl);
  C2 := scdGetBtnShadowOf(Cl);

  Tl := FThumbLines;
  if (Tl = sctlNone) and (FStyle = scssOffice12) then
    Tl := sctlDash;

  if Tl in [sctlLowered, sctlDots] then
  begin
    C3 := C1;
    C1 := C2;
    C2 := C3;
  end;

  if FKind = scdskHorizontal then
  begin
    if Tl = sctlDash then
    begin
      if FStyle = scssOffice12 then
        C1 := C2;
        
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C1;
      end;

      X := (R1.Right - R1.Left) div 2;
      if X > 4 then X := 4;

      H := R1.Bottom - R1.Top - 2;
      if H > 16 then H := 16;

      L := R1.Left + ((R1.Right - R1.Left - (2*X - 1)) div 2);
      T := R1.Top + ((R1.Bottom - R1.Top - H) div 2);

      for I := 0 to X - 1 do
      begin
        R2 := Rect(L, T, L + 1, T + H);
        C.FillRect(R2);
        Inc(L, 2);
      end;
      
      Exit;
    end;

    if Tl = sctlWideDash then
    begin
      if FStyle = scssOffice12 then
        C1 := C2;
        
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C1;
      end;

      X := (R1.Right - R1.Left) div 4;
      if X > 4 then X := 4;

      H := R1.Bottom - R1.Top - 2;
      if H > 16 then H := 16;

      L := R1.Left + ((R1.Right - R1.Left - (4*X - 2)) div 2);
      T := R1.Top + ((R1.Bottom - R1.Top - H) div 2);

      for I := 0 to X - 1 do
      begin
        R2 := Rect(L, T, L + 2, T + H);
        C.FillRect(R2);
        Inc(L, 4);
      end;
      
      Exit;
    end;
    
    if Tl = sctlDots then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C2;
      end;

      X := (R1.Right - R1.Left) div 4;
      if X > 5 then X := 5;

      Y := (R1.Bottom - R1.Top) div 4;
      if Y > 3 then Y := 3;

      L := R1.Left + ((R1.Right - R1.Left) div 2) + (1 - 2*X);
      for I := 0 to X-1 do
      begin
        T := R1.Top + ((R1.Bottom - R1.Top) div 2) + (1 - 2*Y);

        for J := 0 to Y-1 do
        begin
          R2 := Rect(L, T, L + 2, T + 2);
          with C do
          begin
            Brush.Color := C1;
            FillRect(R2);

            Inc(R2.Left);
            Inc(R2.Top);

            Brush.Color := C2;
            FillRect(R2);
          end;

          Inc(T, 4);
        end;

        Inc(L, 4);
      end;

      Exit;
    end;

    Cnt := (R1.Right - R1.Left) div 2;
    if Cnt > 4 then Cnt := 4
    else
    if Cnt <= 0 then Exit;

    R1.Left  := R1.Left + ((R1.Right - R1.Left) div 2);
    R1.Right := R1.Left;

    Dec(R1.Left, Cnt);
    Inc(R1.Right, Cnt);

    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := C1;
      for I := 0 to Cnt-1 do
      begin
        MoveTo(R1.Left + 2*I, R1.Top);
        LineTo(R1.Left + 2*I, R1.Bottom - 1);
      end;

      Pen.Color := C2;
      for I := 0 to Cnt-1 do
      begin
        MoveTo(R1.Left + 1 + 2*I, R1.Top + 1);
        LineTo(R1.Left + 1 + 2*I, R1.Bottom);
      end;
    end;
  end else
  begin
    if Tl = sctlDash then
    begin
      if FStyle = scssOffice12 then
        C1 := C2;

      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C1;
      end;

      Y := (R1.Bottom - R1.Top) div 2;
      if Y > 4 then Y := 4;

      H := R1.Right - R1.Left - 2;
      if H > 16 then H := 16;

      L := R1.Left + ((R1.Right - R1.Left - H) div 2);
      T := R1.Top + ((R1.Bottom - R1.Top - (2*Y - 1)) div 2);

      for I := 0 to Y - 1 do
      begin
        R2 := Rect(L, T, L + H, T + 1);
        C.FillRect(R2);
        Inc(T, 2);
      end;

      Exit;
    end;

    if Tl = sctlWideDash then
    begin
      if FStyle = scssOffice12 then
        C1 := C2;

      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C1;
      end;

      Y := (R1.Bottom - R1.Top) div 4;
      if Y > 4 then Y := 4;

      H := R1.Right - R1.Left - 2;
      if H > 16 then H := 16;

      L := R1.Left + ((R1.Right - R1.Left - H) div 2);
      T := R1.Top + ((R1.Bottom - R1.Top - (4*Y - 2)) div 2);

      for I := 0 to Y - 1 do
      begin
        R2 := Rect(L, T, L + H, T + 2);
        C.FillRect(R2);
        Inc(T, 4);
      end;

      Exit;
    end;

    if Tl = sctlDots then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C1;

        Pen.Style := psSolid;
        Pen.Mode  := pmCopy;
        Pen.Width := 1;
        Pen.Color := C2;
      end;

      X := (R1.Right - R1.Left) div 4;
      if X > 3 then X := 3;

      Y := (R1.Bottom - R1.Top) div 4;
      if Y > 5 then Y := 5;

      L := R1.Left + ((R1.Right - R1.Left) div 2) + (1 - 2*X);
      for I := 0 to X-1 do
      begin
        T := R1.Top + ((R1.Bottom - R1.Top) div 2) + (1 - 2*Y);

        for J := 0 to Y-1 do
        begin
          R2 := Rect(L, T, L + 2, T + 2);
          with C do
          begin
            Brush.Color := C1;
            FillRect(R2);

            Inc(R2.Left);
            Inc(R2.Top);

            Brush.Color := C2;
            FillRect(R2);
          end;

          Inc(T, 4);
        end;

        Inc(L, 4);
      end;

      Exit;
    end;

    Cnt := (R1.Bottom - R1.Top) div 2;
    if Cnt > 4 then Cnt := 4
    else
    if Cnt <= 0 then Exit;

    R1.Top  := R1.Top + ((R1.Bottom - R1.Top) div 2);
    R1.Bottom := R1.Top;

    Dec(R1.Top, Cnt);
    Inc(R1.Bottom, Cnt);

    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := C1;
      for I := 0 to Cnt-1 do
      begin
        MoveTo(R1.Left,      R1.Top + 2*I);
        LineTo(R1.Right - 1, R1.Top + 2*I);
      end;

      Pen.Color := C2;
      for I := 0 to Cnt-1 do
      begin
        MoveTo(R1.Left + 1, R1.Top + 1 + 2*I);
        LineTo(R1.Right,    R1.Top + 1 + 2*I);
      end;
    end;
  end;
end;

procedure TSCDeControlScrollbar.DrawXPFace(C: TCanvas; R: TRect; Cl,
  BkCl: TColor; IsThumb, IsDown, IsHot: Boolean);
begin
  if FStyle = scssXP then
    scdDrawXPFace(FKind, C, R, Cl, BkCl, IsDown, IsHot)
  else
    scdDrawXPFace2(FKind, C, R, Cl, BkCl, IsThumb, IsDown, IsHot);
end;

procedure TSCDeControlScrollbar.Paint(Canvas: TCanvas; R: TRect);
begin
  if (Canvas <> nil) and not IsRectEmpty(R) then
  begin
    DrawScrollBack(Canvas, R);
    DrawBorder(Canvas, R);
    DrawLeftButton(Canvas, R);
    DrawRightButton(Canvas, R);
    DrawExtraButton(Canvas, R);
    DrawThumb(Canvas, R);            
  end;
end;

{ TSCDeScrollbarBkground }

constructor TSCDeScrollbarBkground.Create(AOwner: TSCDeCustomControlScrollbar);
begin
  inherited Create(AOwner);
  FBlend := True;
  FDownColor := cl3DDkShadow;
end;

{ TSCDeControlBorderProps }

procedure TSCDeControlBorderProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeControlBorderProps then
  begin
    with TSCDeControlBorderProps(Source) do
    begin
      Self.Border := Border;
      Self.Color := Color;
      Self.ExDraw := ExDraw;
      Self.FlatColor := FlatColor;
      Self.FlatInnerColor := FlatInnerColor;
      Self.InnerBorder := InnerBorder;
      Self.Width := Width;
    end;
  end else
  if Source is TSCDeStyleBorderProps then
  begin
    with TSCDeStyleBorderProps(Source) do
    begin
      Self.Border := Border;
      Self.Color := Color;
      Self.ExDraw := ExDraw;
      Self.FlatColor := FlatColor;
      Self.FlatInnerColor := FlatInnerColor;
      Self.InnerBorder := InnerBorder;
      Self.Width := Width;
    end;
  end else
    inherited Assign(Source);
end;

constructor TSCDeControlBorderProps.Create(AOwner: TSCDeCustomControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TSCDeControlBorderProps.GetBorder: TSCDeControlBorder;
begin
  Result := scdcbNone;
  if FOwner <> nil then Result := FOwner.Border;
end;

function TSCDeControlBorderProps.GetBorderColor: TColor;
begin
  Result := clBtnFace;
  if FOwner <> nil then Result := FOwner.BorderColor;
end;

function TSCDeControlBorderProps.GetBorderEx: Boolean;
begin
  Result := False;
  if FOwner <> nil then Result := FOwner.BorderEx;
end;

function TSCDeControlBorderProps.GetBorderInner: TSCDeControlBorder;
begin
  Result := scdcbNone;
  if FOwner <> nil then Result := FOwner.BorderInner;
end;

function TSCDeControlBorderProps.GetBorderWidth: TBorderWidth;
begin
  Result := 0;
  if FOwner <> nil then Result := FOwner.BorderWidth;
end;

function TSCDeControlBorderProps.GetFlatColor: TColor;
begin
  Result := clBtnShadow;
  if FOwner <> nil then Result := FOwner.FlatColor;
end;

function TSCDeControlBorderProps.GetFlatInnerColor: TColor;
begin
  Result := clBtnShadow;
  if FOwner <> nil then Result := FOwner.FlatInnerColor;
end;

function TSCDeControlBorderProps.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeControlBorderProps.SetBorder(Value: TSCDeControlBorder);
begin
  if FOwner <> nil then FOwner.Border := Value;
end;

procedure TSCDeControlBorderProps.SetBorderColor(Value: TColor);
begin
  if FOwner <> nil then FOwner.BorderColor := Value;
end;

procedure TSCDeControlBorderProps.SetBorderEx(Value: Boolean);
begin
  if FOwner <> nil then FOwner.BorderEx := Value;
end;

procedure TSCDeControlBorderProps.SetBorderInner(Value: TSCDeControlBorder);
begin
  if FOwner <> nil then FOwner.BorderInner := Value;
end;

procedure TSCDeControlBorderProps.SetBorderWidth(Value: TBorderWidth);
begin
  if FOwner <> nil then FOwner.BorderWidth := Value;
end;

procedure TSCDeControlBorderProps.SetFlatColor(Value: TColor);
begin
  if FOwner <> nil then FOwner.FlatColor := Value;
end;

procedure TSCDeControlBorderProps.SetFlatInnerColor(Value: TColor);
begin
  if FOwner <> nil then FOwner.FlatInnerColor := Value;
end;

{ TSCDeControlCustomScrollbars }

procedure TSCDeControlCustomScrollbars.Assign(Source: TPersistent);
begin
  if Source is TSCDeControlCustomScrollbars then
  begin
    with TSCDeControlCustomScrollbars(Source) do
    begin
      Self.ExtraButton := ExtraButton;
      Self.Height := Height;
      Self.Horizontal := Horizontal;
      Self.Layout := Layout;
      Self.Style := Style;
      Self.ThumbLines := ThumbLines;
      Self.Vertical := Vertical;
    end;
  end else
    inherited Assign(Source);
end;

constructor TSCDeControlCustomScrollbars.Create(AOwner: TSCDeCustomScrollControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TSCDeControlCustomScrollbars.GetExtraButton: Boolean;
begin
  Result := False;
  if FOwner <> nil then
    Result := FOwner.ScrollbarExtraButton;
end;

function TSCDeControlCustomScrollbars.GetHeight: Integer;
begin
  Result := -1;
  if FOwner <> nil then
    Result := FOwner.ScrollbarHeight;
end;

function TSCDeControlCustomScrollbars.GetHorizontal: TSCDeCustomControlScrollbar;
begin
  Result := nil;
  if FOwner <> nil then
    Result := FOwner.ScrollbarHorz;
end;

function TSCDeControlCustomScrollbars.GetLayout: TSCDeScrollButtonLayout;
begin
  Result := scdsbDefault;
  if FOwner <> nil then
    Result := FOwner.ScrollButtonsLayout;
end;

function TSCDeControlCustomScrollbars.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeControlCustomScrollbars.GetStyle: TSCDeScrollbarStyle;
begin
  Result := scssDefault;
  if FOwner <> nil then
    Result := FOwner.ScrollbarStyle;
end;

function TSCDeControlCustomScrollbars.GetThumbLines: TSCDeScrollThumbline;
begin
  Result := sctlNone;
  if FOwner <> nil then
    Result := FOwner.ScrollbarThumbLines;
end;

function TSCDeControlCustomScrollbars.GetVertical: TSCDeCustomControlScrollbar;
begin
  Result := nil;
  if FOwner <> nil then
    Result := FOwner.ScrollbarVert;
end;

procedure TSCDeControlCustomScrollbars.SetExtraButton(Value: Boolean);
begin
  if FOwner <> nil then
    FOwner.ScrollbarExtraButton := Value;
end;

procedure TSCDeControlCustomScrollbars.SetHeight(Value: Integer);
begin
  if FOwner <> nil then
    FOwner.ScrollbarHeight := Value;
end;

procedure TSCDeControlCustomScrollbars.SetHorizontal(Value: TSCDeCustomControlScrollbar);
begin
  if FOwner <> nil then
    FOwner.ScrollbarHorz := Value;
end;

procedure TSCDeControlCustomScrollbars.SetLayout(Value: TSCDeScrollButtonLayout);
begin
  if FOwner <> nil then
    FOwner.ScrollButtonsLayout := Value;
end;

procedure TSCDeControlCustomScrollbars.SetSmooth(Value: Boolean);
begin
  Horizontal.Smooth := Value;
  Vertical.Smooth := Value;
end;

procedure TSCDeControlCustomScrollbars.SetStyle(Value: TSCDeScrollbarStyle);
begin
  if FOwner <> nil then
    FOwner.ScrollbarStyle := Value;
end;

procedure TSCDeControlCustomScrollbars.SetThumbLines(Value: TSCDeScrollThumbline);
begin
  if FOwner <> nil then
    FOwner.ScrollbarThumbLines := Value;
end;

procedure TSCDeControlCustomScrollbars.SetVertical(Value: TSCDeCustomControlScrollbar);
begin
  if FOwner <> nil then
    FOwner.ScrollbarVert := Value;
end;

{ TSCDePictureList }

procedure TSCDePictureList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TSCDePictureList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPictures := TSCDePictureListItems.Create(Self);
end;

destructor TSCDePictureList.Destroy;
begin
  Destroying;
  if FNotifyList <> nil then
  begin
    ItemsChanged(scpcaDestroyed);
    FreeAndNil(FNotifyList);
  end;

  FPictures.FOwner := nil;
  FreeAndNil(FPictures);
  inherited Destroy;
end;

function TSCDePictureList.GetPicture(Index: Integer): TPicture;
begin
  Result := nil;
  if (Index > -1) and (Index < FPictures.Count) then
    Result := FPictures[Index].Picture;
end;

procedure TSCDePictureList.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

function TSCDePictureList.GetPicture(AName: String): TPicture;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FPictures.Count-1 do
    if AnsiSameText(FPictures[I].Name, AName) then
    begin
      Result := FPictures[I].Picture;
      Exit;
    end;
end;

function TSCDePictureList.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDePictureList.ItemsChanged(Action: TSCDePictureChangeAction);
var
  I: Integer;
  N: TSCDePictureNotifier;
begin
  if (FNotifyList <> nil) and ((Action = scpcaDestroyed) or not InUpdate) then
    for I := 0 to FNotifyList.Count-1 do
    begin
      try
        N := FNotifyList.Items[I];
        N.DoChange(Self, Action);
      except
      end;
    end;
end;

procedure TSCDePictureList.RegisterNotifier(Sender: TSCDePictureNotifier);
begin
  if Sender <> nil then
  begin
    if FNotifyList = nil then
      FNotifyList := TList.Create;

    if FNotifyList.IndexOf(Sender) = -1 then
      FNotifyList.Add(Sender);
  end;
end;

procedure TSCDePictureList.SetPictures(Value: TSCDePictureListItems);
begin
  FPictures.Assign(Value)
end;

procedure TSCDePictureList.UnregisterNotifier(Sender: TSCDePictureNotifier);
var
  Index: Integer;
begin
  if Sender <> nil then
  begin
    Index := -1;
    if FNotifyList <> nil then
      Index := FNotifyList.IndexOf(Sender);

    if Index > -1 then
    begin
      FNotifyList.Delete(Index);
      if FNotifyList.Count = 0 then
        FreeAndNil(FNotifyList);
    end;
  end;
end;

procedure TSCDePictureList.NotifyAll;
begin
  ItemsChanged(scpcaChanged);
end;

procedure TSCDePictureList.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TSCDePictureList then
    with TSCDePictureList(Source) do
      Self.Pictures := Pictures;
end;

function TSCDePictureList.GetCount: Integer;
begin
  Result := 0;
  if FPictures <> nil then Result := FPictures.Count;
end;

{ TSCDePictureNotifier }

procedure TSCDePictureNotifier.DoChange(Sender: TSCDePictureList; Action: TSCDePictureChangeAction);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender, Action);
end;

{ TSCDePictureListItems }

function TSCDePictureListItems.Add: TSCDePictureListItem;
begin
  Result := TSCDePictureListItem(inherited Add);
end;

constructor TSCDePictureListItems.Create(AOwner: TSCDePictureList);
begin
  inherited Create(TSCDePictureListItem);
  FOwner := AOwner;
end;

function TSCDePictureListItems.GetItem(Index: Integer): TSCDePictureListItem;
begin
  Result := TSCDePictureListItem(inherited GetItem(Index));
end;

function TSCDePictureListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDePictureListItems.SetItem(Index: Integer;
  Value: TSCDePictureListItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSCDePictureListItems.Update(Item: TCollectionItem);
begin
  if FOwner <> nil then
    FOwner.ItemsChanged(scpcaChanged);
end;

{ TSCDePictureListItem }

procedure TSCDePictureListItem.Assign(Source: TPersistent);
begin
  if Source is TSCDePictureListItem then
  begin
    BeginUpdate;
    try
      with TSCDePictureListItem(Source) do
      begin
        Self.Picture := Picture;
        Self.Caption := Caption;
        Self.Name := Name;
        Self.Description := Description;
      end;
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TSCDePictureListItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TSCDePictureListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FDescription := TStringList.Create;
  TStringList(FDescription).OnChange := DescriptionChanged;
end;

procedure TSCDePictureListItem.DescriptionChanged(Sender: TObject);
begin
  DoChanged;
end;

destructor TSCDePictureListItem.Destroy;
begin
  TStringList(FDescription).OnChange := nil;
  FreeAndNil(FDescription);
  FPicture.OnChange := nil;
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TSCDePictureListItem.DoChanged;
begin
  if not InUpdate then
    Changed(True);
end;

procedure TSCDePictureListItem.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      DoChanged;
  end;
end;

function TSCDePictureListItem.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then Result := FCaption;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TSCDePictureListItem.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDePictureListItem.PictureChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TSCDePictureListItem.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    DoChanged;
  end;
end;

procedure TSCDePictureListItem.SetDescription(Value: TStrings);
begin
  FDescription.Assign(Value);
end;

procedure TSCDePictureListItem.SetName(const Value: String);
begin
  if FName <> Value then
  begin
    FName := Value;
    DoChanged;
  end;
end;

procedure TSCDePictureListItem.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TSCDeCustomStyleController }

procedure TSCDeCustomStyleController.Changed(Props: TSCDeCustomStyleProps);
var
  I: Integer;
begin
  for I := 0 to FControls.Count-1 do
    TSCDeCustomControl(FControls[I]).NotifyStyleChange(Props);
end;

constructor TSCDeCustomStyleController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TList.Create;
  FBorderProps := TSCDeStyleBorderProps.Create(Self);
end;

destructor TSCDeCustomStyleController.Destroy;
begin
  FControls.Free;
  FBorderProps.Free;
  inherited Destroy;
end;

procedure TSCDeCustomStyleController.RegisterChanges(Control: TSCDeCustomControl);
begin
  if (Control <> nil) and (FControls.IndexOf(Control) = -1) then
  begin
    FControls.Add(Control);
    Control.NotifyStyleChange;
  end;
end;

procedure TSCDeCustomStyleController.SetBorderProps(Value: TSCDeStyleBorderProps);
begin
  FBorderProps.Assign(Value);
end;

procedure TSCDeCustomStyleController.UnregisterChanges(Control: TSCDeCustomControl);
var
  Index: Integer;
begin
  if Control <> nil then
  begin
    Index :=  FControls.IndexOf(Control);
    if Index > -1 then
    begin
      FControls.Remove(Control);
      Control.NotifyStyleChange;
    end;
  end;
end;

{ TSCDeCustomStyleProps }

procedure TSCDeCustomStyleProps.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeCustomStyleProps.Changed;
begin
  if (FOwner <> nil) and (FUpdateCount = 0) then
    FOwner.Changed(Self);
end;

constructor TSCDeCustomStyleProps.Create(AOwner: TSCDeCustomStyleController);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TSCDeCustomStyleProps.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Changed;
  end;
end;

function TSCDeCustomStyleProps.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TSCDeStyleBorderProps }

procedure TSCDeStyleBorderProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeStyleBorderProps then
  begin
    BeginUpdate;
    try
      with TSCDeStyleBorderProps(Source) do
      begin
        Self.FBorder := FBorder;
        Self.FBorderColor := FBorderColor;
        Self.FBorderEx := FBorderEx;
        Self.FFlatColor := FFlatColor;
        Self.FFlatInnerColor := FFlatInnerColor;
        Self.FBorderInner := FBorderInner;
        Self.FBorderWidth := FBorderWidth;
      end;
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

constructor TSCDeStyleBorderProps.Create(AOwner: TSCDeCustomStyleController);
begin
  inherited Create(AOwner);
  FBorder := scdcbNone;
  FBorderColor := clBtnFace;
  FBorderEx := False;
  FFlatColor := clBtnShadow;
  FFlatInnerColor := clBtnShadow;
  FBorderInner := scdcbNone;
  FBorderWidth := 0;
end;

procedure TSCDeStyleBorderProps.SetBorder(Value: TSCDeControlBorder);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetBorderEx(Value: Boolean);
begin
  if FBorderEx <> Value then
  begin
    FBorderEx := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetBorderInner(Value: TSCDeControlBorder);
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetFlatColor(Value: TColor);
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Changed;
  end;
end;

procedure TSCDeStyleBorderProps.SetFlatInnerColor(Value: TColor);
begin
  if FFlatInnerColor <> Value then
  begin
    FFlatInnerColor := Value;
    Changed;
  end;
end;

{ TSCDeGraphicBorderProps }

procedure TSCDeGraphicBorderProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeGraphicBorderProps then
  begin
    with TSCDeGraphicBorderProps(Source) do
    begin
      Self.FBorder := Border;
      Self.FColor := Color;
      Self.FExDraw := ExDraw;
      Self.FFlatColor := FlatColor;
      Self.FFlatInnerColor := FlatInnerColor;
      Self.FInnerBorder := InnerBorder;
      Self.FWidth := Width;
    end;
    
    DoChange;
  end else
    inherited Assign(Source);
end;

constructor TSCDeGraphicBorderProps.Create(AOwner: TSCDeGraphicControl);
begin
  inherited Create;
  FOwner := AOwner;
  FBorder := scdcbNone;
  FColor := clBtnFace;
  FExDraw := False;
  FFlatColor := clBtnShadow;
  FFlatInnerColor := clBtnShadow;
  FInnerBorder := scdcbNone;
  FWidth := 0;
end;

procedure TSCDeGraphicBorderProps.DoChange;
begin
  if FOwner <> nil then
    FOwner.DoBorderChanged;
end;

function TSCDeGraphicBorderProps.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeGraphicBorderProps.SetBorder(Value: TSCDeControlBorder);
begin
  if (FBorder <> Value) and ((FOwner = nil) or
    FOwner.CanSetBorder(Value)) then
  begin
    FBorder := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetExDraw(Value: Boolean);
begin
  if FExDraw <> Value then
  begin
    FExDraw := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetInnerBorder(Value: TSCDeControlBorder);
begin
  if (FInnerBorder <> Value) and ((FOwner = nil) or
    FOwner.CanSetInnerBorder(Value)) then
  begin
    FInnerBorder := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetWidth(Value: TBorderWidth);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetFlatColor(Value: TColor);
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    DoChange;
  end;
end;

procedure TSCDeGraphicBorderProps.SetFlatInnerColor(Value: TColor);
begin
  if FFlatInnerColor <> Value then
  begin
    FFlatInnerColor := Value;
    DoChange;
  end;
end;

{ TSCDeGraphicControlActionLink }

procedure TSCDeGraphicControlActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TSCDeGraphicControl;
end;

function TSCDeGraphicControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    AnsiSameText(FClient.Caption, (Action as TCustomAction).Caption);
end;

function TSCDeGraphicControlActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TSCDeGraphicControlActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TSCDeGraphicControlActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TSCDeGraphicControlActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnClick = @Action.OnExecute);
end;

function TSCDeGraphicControlActionLink.IsShortCutLinked: Boolean;
begin
  Result := False;
end;

function TSCDeGraphicControlActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TSCDeGraphicControlActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TSCDeGraphicControlActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TSCDeGraphicControlActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TSCDeGraphicControlActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TSCDeGraphicControlActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;

procedure TSCDeGraphicControlActionLink.SetShortCut(Value: TShortCut);
begin
  inherited;
end;

procedure TSCDeGraphicControlActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

{ TSCDeGraphicControl }

constructor TSCDeGraphicControl.Create(AOwner: TComponent);
begin
  FCreatingControl := True;
  FImageIndex := -1;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];

  FBorderProps := GetBorderPropsClass.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FAutoSize   := False;
  {$IFDEF SCDE_DELPHI6_UP}
  AutoSize    := False;
  {$ENDIF}

  FPictureIndex := -1;
  FPictureNotifier := TSCDePictureNotifier.Create;
  FPictureNotifier.OnChange := PictureListChanged;

  FMouseDownPoint := Point(-1, -1);
  FImageLayout := scilLeft;
  FSpacing     := 4;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  AdjustBounds;

  FTransparent := True;
end;

destructor TSCDeGraphicControl.Destroy;
begin
  if GetCaptureControl = Self then
    SetCaptureControl(nil);

  FreeAndNil(FImageChangeLink);
  FreeAndNil(FBorderProps);
  FreeAndNil(FCanvas);

  FPictureNotifier.OnChange := nil;
  if FPictureList <> nil then
    FPictureList.UnregisterNotifier(FPictureNotifier);

  FreeAndNil(FPictureNotifier);
  inherited Destroy;
end;

procedure TSCDeGraphicControl.WMPaint(var Message: TWMPaint);
var
  R: TRect;
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
begin
  if Message.DC = 0 then
    Exit;

  if (Parent = nil) or not FDoubleBuffered then
    ProcessPaint(Message.DC)
  else begin
    R := Rect(0, 0, Width, Height);

    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, R.Right, R.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);

    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      if IsTransparent then PaintParentOn(MemDC);

      Perform(WM_ERASEBKGND, MemDC, MemDC);
      ProcessPaint(MemDC);
      BitBlt(Message.DC, 0, 0, R.Right, R.Bottom, MemDC, 0, 0, SRCCOPY);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure TSCDeGraphicControl.Paint;
begin
  //
end;

procedure TSCDeGraphicControl.AfterPaint;
begin
  //
end;

procedure TSCDeGraphicControl.BeforePaint;
begin
  //
end;

function TSCDeGraphicControl.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  CalculateBorder(Result);
end;

procedure TSCDeGraphicControl.ProcessPaint(DC: HDC);
var
  R: TRect;
  C: TColor;
begin
  if DC <> 0 then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        if not IsTransparent then
        begin
          R := ClientRect;
          if not IsRectEmpty(R) then
          begin
            C := Self.Color;
            if C = clNone then
            begin
              if Parent <> nil then
                C := TSCDeParentControl(Parent).Color;
                
              if C = clNone then C := clWindow;
            end;

            with Canvas do
            begin
              try
                Brush.Style := bsSolid;
                Brush.Color := C;

                FillRect(R);
              finally
                Brush.Style := bsClear;
              end;
            end;
          end;
        end;

        BeforePaint;
        Paint;
        AfterPaint;
        DrawBorder;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TSCDeGraphicControl.DrawBorder;
var
  DC: HDC;
  R, BRect: TRect;
  OldDCPen: HPen;
  FramePen: TPen;
  Points: array[0..4] of TPoint;
  C1, AColor: TColor;
  Bs, Offset: Integer;
begin
  if (csDestroying in ComponentState) or (Canvas.Handle = 0) then
    Exit;

  DC := Canvas.Handle;

  AColor := Self.Color;
  if BorderProps.Color <> clNone then
    AColor := BorderProps.Color;

  if AColor = clNone then AColor := clBtnFace;

  FramePen := TPen.Create;
  with FramePen do
  begin
    Style := psInsideFrame;
    Color := AColor;
  end;

  OldDCPen := 0;
  try
    Offset := BorderProps.Width div 2;

    R := BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);

    BRect := R;
    InflateRect(R, -Offset, -Offset);

    if Odd(BorderProps.Width) then
    begin
      Dec(R.Right);
      if R.Right < R.Left then
        R.Right := R.Left;

      Dec(R.Bottom);
      if R.Bottom < R.Top then
        R.Bottom := R.Top;
    end;

    if BorderProps.Width > 0 then
      FramePen.Width := BorderProps.Width;

    OldDCPen := SelectObject(DC, FramePen.Handle);
    SetROP2(DC, scdPenModes[FramePen.Mode]);

    Bs := GetBorderSize;

    if (BorderProps.Width > 0) and (BorderProps.Color <> clNone) then
    begin
      InflateRect(R, -Bs, -Bs);

      Points[0] := Point(R.Left, R.Top);
      Points[1] := Point(R.Right, R.Top);
      Points[2] := Point(R.Right, R.Bottom);
      Points[3] := Point(R.Left, R.Bottom);
      Points[4] := Point(R.Left, R.Top);

      Windows.MoveToEx(DC, Points[0].X - Offset, Points[0].Y, nil);
      Windows.LineTo(DC, Points[1].X + Offset, Points[1].Y);

      Windows.MoveToEx(DC, Points[1].X, Points[1].Y - Offset, nil);
      Windows.LineTo(DC, Points[2].X, Points[2].Y + Offset);

      Windows.MoveToEx(DC, Points[2].X + Offset, Points[2].Y, nil);
      Windows.LineTo(DC, Points[3].X - Offset, Points[3].Y);

      Windows.MoveToEx(DC, Points[3].X, Points[3].Y + Offset, nil);
      Windows.LineTo(DC, Points[4].X, Points[4].Y - Offset);
    end;

    R := BRect;
    if not IsRectEmpty(R) and (BorderProps.Border <> scdcbNone) then
    begin
      if BorderProps.ExDraw then
      begin
        C1 := GetBorderExColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        if BorderProps.Border in [scdcbFlat, scdcbFlatBold] then
          C1 := scdGetBtnShadowOf(C1);

        scdDrawBevelEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, BorderProps.Border);
      end else
      begin
        C1 := BorderProps.FlatColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        scdDrawEdgeEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, BorderProps.Border);
      end;

      InflateRect(BRect, -Bs, -Bs);
    end;

    R := BRect;
    InflateRect(R, -BorderProps.Width, -BorderProps.Width);

    if not IsRectEmpty(BRect) then
    begin
      if BorderProps.ExDraw then
      begin
        C1 := GetBorderExColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        if BorderProps.InnerBorder in [scdcbFlat, scdcbFlatBold] then
          C1 := scdGetBtnShadowOf(C1);

        scdDrawBevelEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, BorderProps.InnerBorder);
      end else
      begin
        C1 := BorderProps.FlatInnerColor;
        if C1 = clNone then C1 := Self.Color;
        if C1 = clNone then C1 := clWindow;

        scdDrawEdgeEx(DC, R, C1, TSCDeFakeControl(Parent).Color, False, BorderProps.InnerBorder);
      end;
    end;
  finally
    if OldDCPen <> 0 then
      SelectObject(DC, OldDCPen);

    FramePen.Free;
  end;
end;

procedure TSCDeGraphicControl.BorderChanged;
begin
  //
end;

procedure TSCDeGraphicControl.CalculateBorder(var R: TRect);
var
  B: Integer;
begin
  B := GetBorderSize + FBorderProps.Width + GetInnerBorderSize;
  InflateRect(R, -B, -B);
end;

function TSCDeGraphicControl.GetBorderSize: Integer;
begin
  Result := 0;
  if FBorderProps.Border in [scdcbRaised, scdcbLowered, scdcbFlat,
    scdcbColor] then
    Result := 1
  else
  if FBorderProps.Border in [scdcbFlatBold, scdcbFlatRounded, scdcbFlatBoldRounded,
    scdcb3DRaised, scdcb3DLowered, scdcbBumped, scdcbEtched, scdcbMacLowered,
    scdcbMacRaised, scdcbMetal, scdcbSoftLowered, scdcbSoftRaised] then
    Result := 2;
end;

function TSCDeGraphicControl.GetInnerBorderSize: Integer;
begin
  Result := 0;
  if FBorderProps.InnerBorder in [scdcbRaised, scdcbLowered, scdcbFlat,
    scdcbColor] then
    Result := 1
  else
  if FBorderProps.Border in [scdcbFlatBold, scdcbFlatRounded, scdcbFlatBoldRounded,
    scdcb3DRaised, scdcb3DLowered, scdcbBumped, scdcbEtched, scdcbMacLowered,
    scdcbMacRaised, scdcbMetal, scdcbSoftLowered, scdcbSoftRaised] then
    Result := 2;
end;

procedure TSCDeGraphicControl.SetBorderProps(Value: TSCDeGraphicBorderProps);
begin
  FBorderProps.Assign(Value);
end;

procedure TSCDeGraphicControl.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

function TSCDeGraphicControl.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

function TSCDeGraphicControl.GetBorderExColor: TColor;
begin
  Result := Self.Color;
end;

procedure TSCDeGraphicControl.DoBorderChanged;
begin
  Invalidate;
  BorderChanged;
end;

procedure TSCDeGraphicControl.StopTracking;
begin
  //
end;

procedure TSCDeGraphicControl.CMChanged(var Message: TMessage);
begin
  inherited;
  if FInChange = 0 then
    Change;
end;

procedure TSCDeGraphicControl.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeGraphicControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  EnabledChanged;

  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);
  FMouseInControl := False;
  if Enabled then UpdateTracking;
  MouseInControlChanged;
end;

procedure TSCDeGraphicControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TSCDeGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    inherited;
    MouseInControlChanged;

    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
  end else
    inherited;
end;

procedure TSCDeGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  if Enabled and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := False;
    inherited;
    MouseInControlChanged;

    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
  end else
    inherited;
end;

procedure TSCDeGraphicControl.CMParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeGraphicControl.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  SystemColorsChanged;
  Invalidate;
end;

procedure TSCDeGraphicControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TSCDeGraphicControl.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TSCDeGraphicControl.WMPrint(var Message: TMessage);
begin
  if (Message.WParam = 0) or
    ((Message.LParam and PRF_CHECKVISIBLE) <> PRF_CHECKVISIBLE) then
    Exit;

  ProcessPaint(HDC(Message.WParam));
end;

procedure TSCDeGraphicControl.WMSettingChange(var Message: TWMSettingChange);
begin
  inherited;
  Invalidate;
end;

procedure TSCDeGraphicControl.Change;
begin
  Inc(FInChange);
  try
    Changed;
  finally
    Dec(FInChange);
  end;

  DoChange;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then
    FOnChange(Self);
end;

procedure TSCDeGraphicControl.DoChange;
begin
  //
end;

procedure TSCDeGraphicControl.AdjustBounds;
begin
  //
end;

procedure TSCDeGraphicControl.AutoSizeChanged;
begin
  //
end;

procedure TSCDeGraphicControl.DoAutoSize(Value: Boolean);
begin
  if (FAutoSize = Value) or (inherited AutoSize <> Value) then
  begin
    FAutoSize := Value;

    AutoSizeChanged;

    {$IFDEF SCDE_DELPHI6_UP}
    inherited SetAutoSize(Value);
    {$ELSE}
    inherited AutoSize := Value;
    {$ENDIF}
    AdjustBounds;
  end;
end;

procedure TSCDeGraphicControl.EnabledChanged;
begin
  //
end;

function TSCDeGraphicControl.GetAdjustedRect(NewWidth, NewHeight: Integer): TRect;
var
  B: Integer;
begin
  Result := Rect(Left, Top, Left + NewWidth, Top + NewHeight);

  B := 2*(GetBorderSize + FBorderProps.Width + GetInnerBorderSize);

  Inc(Result.Right, B);
  Inc(Result.Bottom, B);
end;

procedure TSCDeGraphicControl.SetAutoSize(Value: Boolean);
begin
  DoAutoSize(Value);
end;

procedure TSCDeGraphicControl.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TSCDeGraphicControl.MouseInControlChanged;
begin
  //
end;

procedure TSCDeGraphicControl.SystemColorsChanged;
begin
  //
end;

function TSCDeGraphicControl.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  R: TRect;
begin
  Result := AutoSize and inherited CanAutoSize(NewWidth, NewHeight);
  if Result then
  begin
    R := GetAdjustedRect(NewWidth, NewHeight);

    NewWidth := R.Right - R.Left;
    if NewWidth < 0 then NewWidth := 0;

    NewHeight := R.Bottom - R.Top;
    if NewHeight < 0 then NewHeight := 0;
  end;
end;

function TSCDeGraphicControl.CanSetBorder(Value: TSCDeControlBorder): Boolean;
begin
  Result := True;
end;

function TSCDeGraphicControl.CanSetInnerBorder(
  Value: TSCDeControlBorder): Boolean;
begin
  Result := True;
end;

procedure TSCDeGraphicControl.UpdateTracking;
var
  P: TPoint;
begin
  FMouseInControl := False;
  if Enabled and not (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);

    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

procedure TSCDeGraphicControl.TranslateMouseMove(var Message: TWMMouse);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseMove(KeysToShiftState(Keys), XPos, YPos);
end;

procedure TSCDeGraphicControl.WMMouseMove(var Message: TWMMouseMove);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseMove(Message);
  inherited;
end;

procedure TSCDeGraphicControl.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
end;

function TSCDeGraphicControl.CalculateImageRect: TRect;
var
  ARect: TRect;
begin
  Result := GetImageRect;
  if FImages = nil then
    Exit;

  OffsetRect(Result, (ClientWidth - Images.Width) div 2,
    (ClientHeight - Images.Height) div 2);

  ARect := GetTextRect;
  if (ARect.Right <= ARect.Left) or
    (ARect.Bottom <= ARect.Top) then Exit;

  case ImageLayout of
    scilBottom:
      OffsetRect(Result, 0, (((ARect.Bottom - ARect.Top) + 2) div 2));
    scilLeft:
      OffsetRect(Result, -(((ARect.Right - ARect.Left) + 2) div 2) - GetIndent, 0);
    scilRight:
      OffsetRect(Result, (((ARect.Right - ARect.Left) + 2) div 2) + GetIndent, 0);
    scilTop:
      OffsetRect(Result, 0, -(((ARect.Bottom - ARect.Top) + 2) div 2));
  end;
end;

function TSCDeGraphicControl.CalculateTextRect: TRect;
var
  ARect: TRect;
  TW, TH: Integer;
begin
  Result := GetTextRect;

  TW := Result.Right - Result.Left;
  TH := Result.Bottom - Result.Top;
  OffsetRect(Result, (ClientWidth - TW) div 2,
    (ClientHeight - TH) div 2);

  if (Images = nil) or (ImageIndex = -1) or
    ((Images <> nil) and (ImageIndex > Images.Count-1)) then Exit;

  ARect := GetImageRect;
  if (ARect.Right <= ARect.Left) or
    (ARect.Bottom <= ARect.Top) then Exit;

  case ImageLayout of
    scilBottom:
      OffsetRect(Result, 0, -(((ARect.Bottom - ARect.Top) + 2) div 2));
    scilLeft:
      OffsetRect(Result, (((ARect.Right - ARect.Left) + 2) div 2) + GetIndent, 0);
    scilRight:
      OffsetRect(Result, -(((ARect.Right - ARect.Left) + 2) div 2) - GetIndent, 0);
    scilTop:
      OffsetRect(Result, 0, (((ARect.Bottom - ARect.Top) + 2) div 2));
  end;
end;

function TSCDeGraphicControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSCDeGraphicControlActionLink;
end;

function TSCDeGraphicControl.GetImageRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FImages <> nil then
    Result := Rect(0, 0, Images.Width, Images.Height);
end;

function TSCDeGraphicControl.GetIndent: Integer;
begin
  Result := -1;
  if (ImageLayout in [scilLeft, scilRight]) and 
    IsValidImage(FImageIndex) and (Caption <> '') then
    Result := FIndent;
end;

function TSCDeGraphicControl.GetTextCalculateFont: TFont;
begin
  Result := Self.Font;
end;

function TSCDeGraphicControl.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, GetTextCalculateFont.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;

  Result := Metrics.tmHeight;
end;

function TSCDeGraphicControl.GetTextRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if Caption <> '' then
  begin
    Canvas.Font.Assign(Self.Font);
    Result := Rect(0, 0, ClientWidth, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
      Result, DT_CALCRECT);
  end;
end;

procedure TSCDeGraphicControl.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TSCDeGraphicControl.IndentChanged;
begin
  Invalidate;
end;

procedure TSCDeGraphicControl.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;

    AdjustBounds;
    IndentChanged;
  end;
end;

procedure TSCDeGraphicControl.SetSpacing(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FSpacing <> Value then
  begin
    FSpacing := Value;

    AdjustBounds;
    SpacingChanged;
  end;
end;

procedure TSCDeGraphicControl.SpacingChanged;
begin
  Invalidate;
end;

procedure TSCDeGraphicControl.UpdateLocked;
begin
  //
end;

procedure TSCDeGraphicControl.UpdateUnlocked;
begin
  //
end;

procedure TSCDeGraphicControl.AfterConstruction;
begin
  FCreatingControl := False;
  inherited AfterConstruction;
end;

procedure TSCDeGraphicControl.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FImages <> nil then
      Invalidate;
  end;
end;

procedure TSCDeGraphicControl.SetImageLayout(const Value: TSCDeImageLayout);
begin
  FImageLayout := Value;
end;

procedure TSCDeGraphicControl.SetImages(const Value: TCustomImageList);
var
  FOldImages: TCustomImageList;
begin
  FOldImages := FImages;
  if FImages <> nil then
  begin
  {$IFDEF SCDE_DELPHI5_UP}
    FImages.RemoveFreeNotification(Self);
  {$ENDIF}
    FImages.UnRegisterChanges(FImageChangeLink);
  end;

  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;

  if FOldImages <> FImages then
  begin
    Invalidate;
    ImageListChange(FImages);
  end;
end;

procedure TSCDeGraphicControl.SetPictureIndex(Value: Integer);
begin
  if Value < -1 then Value := -1;

  if FPictureIndex <> Value then
  begin
    FPictureIndex := Value;
    if FPictureList <> nil then
      PictureListChanged(nil, scpcaChanged);
  end;
end;

procedure TSCDeGraphicControl.SetPictureList(const Value: TSCDePictureList);
begin
  if FPictureList <> Value then
  begin
    if FPictureList <> nil then
      FPictureList.UnregisterNotifier(FPictureNotifier);

    FPictureList := Value;
    if FPictureList <> nil then
      FPictureList.RegisterNotifier(FPictureNotifier);

    PictureListChanged(FPictureList, scpcaChanged);
  end;
end;

procedure TSCDeGraphicControl.DoPictureListChanged;
begin
  //
end;

procedure TSCDeGraphicControl.PictureListChanged(Sender: TSCDePictureList;
  Action: TSCDePictureChangeAction);
begin
  if (Action = scpcaDestroyed) and (Sender <> nil) and (Sender = FPictureList) then
  begin
    FPictureList.UnregisterNotifier(FPictureNotifier);
    FPictureList := nil;
  end;

  DoPictureListChanged;
end;

procedure TSCDeGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then UpdateLocked;
end;

procedure TSCDeGraphicControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then UpdateLocked;
  end;
end;

function TSCDeGraphicControl.GetAbout: TSCDeAboutString;
begin
  Result := SCDE_VersionStr;
end;

function TSCDeGraphicControl.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TSCDeGraphicControl.IsValidImage(Indx: Integer): Boolean;
var
  Img: TCustomImageList;
begin
  Img := GetImages;
  Result := (Indx > -1) and (Img <> nil) and
    (Img.Count > 0) and (Indx < Img.Count);
end;

procedure TSCDeGraphicControl.SetAbout(const Value: TSCDeAboutString);
begin
  //
end;

procedure TSCDeGraphicControl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and IsActive and IsInClientRect(X, Y) and
    ((ssLeft in Shift) or (ssDouble in Shift)) then
  begin
    FMouseIsDown := True;
    FMouseDownPoint := Point(X, Y);
  end;
end;

procedure TSCDeGraphicControl.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (FMouseIsDown or FMouseInControl) then
    UpdateTracking;
end;

procedure TSCDeGraphicControl.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := False;
  FMouseDownPoint := Point(-1, -1);
end;

function TSCDeGraphicControl.IsActive: Boolean;
begin
  Result := True;
end;

function TSCDeGraphicControl.IsInClientRect(X, Y: Integer): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  P := Point(X, Y);
  R := GetClientRect;

  Result := PtInRect(R, P);
end;

procedure TSCDeGraphicControl.TranslateMouseDown(var Message: TWMMouse;
  Button: TMouseButton; Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
end;

procedure TSCDeGraphicControl.TranslateMouseUp(var Message: TWMMouse;
  Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      DoMouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TSCDeGraphicControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
  begin
    SendCancelMode(Self);
    if csCaptureMouse in ControlStyle then MouseCapture := True;
    TranslateMouseDown(Message, mbLeft, [ssDouble]);
  end;
  inherited;
end;

procedure TSCDeGraphicControl.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if not (csDesigning in ComponentState) then
  begin
    SendCancelMode(Self);
    TranslateMouseDown(Message, mbLeft, []);
  end;
  inherited;
end;

procedure TSCDeGraphicControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseUp(Message, mbLeft);
  inherited;
end;

procedure TSCDeGraphicControl.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbMiddle, [ssDouble]);
  inherited;
end;

procedure TSCDeGraphicControl.WMMButtonDown(var Message: TWMMButtonDown);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbMiddle, []);
  inherited;
end;

procedure TSCDeGraphicControl.WMMButtonUp(var Message: TWMMButtonUp);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseUp(Message, mbMiddle);
  inherited;
end;

procedure TSCDeGraphicControl.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbRight, [ssDouble]);
  inherited;
end;

procedure TSCDeGraphicControl.WMRButtonDown(var Message: TWMRButtonDown);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseDown(Message, mbRight, []);
  inherited;
end;

procedure TSCDeGraphicControl.WMRButtonUp(var Message: TWMRButtonUp);
begin
  if not (csDesigning in ComponentState) then
    TranslateMouseUp(Message, mbRight);
  inherited;
end;

function TSCDeGraphicControl.CanGetClientRect: Boolean;
begin
  Result := True;
end;

function TSCDeGraphicControl.GetBorderPropsClass: TSCDeGraphicBorderPropsClass;
begin
  Result := TSCDeGraphicBorderProps;
end;

procedure TSCDeGraphicControl.PaintParentOn(DC: HDC);
var
  InDesign: Boolean;
  Ctrl: TControl;
  R, R2, CR, CtlR,
  SelfR, ParentR: TRect;
  I, X, Y, Cnt, SaveIndex: Integer;
begin
  if (Parent = nil) or (DC = 0) then
    Exit;

  ParentR := Parent.ClientRect;
  if IsRectEmpty(ParentR) then
    Exit;

  SelfR := GetClientRect;
  with SelfR do
  begin
    TopLeft := Self.ClientToScreen(TopLeft);
    BottomRight := Self.ClientToScreen(BottomRight);

    TopLeft := Parent.ScreenToClient(TopLeft);
    BottomRight := Parent.ScreenToClient(BottomRight);
  end;

  X := -SelfR.Left;
  Y := -SelfR.Top;

  IntersectRect(SelfR, SelfR, ParentR);
  if IsRectEmpty(SelfR) then
    Exit;

  // Copy parent control image
  SaveIndex := SaveDC(DC);
  try
    SetViewportOrgEx(DC, X, Y, nil);
    IntersectClipRect(DC, 0, 0, ParentR.Right - ParentR.Left, ParentR.Bottom - ParentR.Top);
    Self.Parent.Perform(WM_ERASEBKGND, WParam(DC), 0);
    TSCDeParentControl(Self.Parent).PaintWindow(DC);
  finally
    RestoreDC(DC, SaveIndex);
  end;  

  //Copy images of parent controls
  InDesign := csDesigning in ComponentState;

  Cnt := Parent.ControlCount;
  for I := 0 to Cnt - 1 do begin
    if Parent.Controls[I] <> nil then
    begin
      Ctrl := Parent.Controls[I];
      if Ctrl = Self then
        Break;

      if Ctrl is TWinControl then
        Continue;
        
      with Ctrl do
      begin
        CtlR := Bounds(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height);

        if Bool(IntersectRect(R, SelfR, CtlR)) and (Visible or InDesign) then
        begin
          CR := ClientRect;
          R2 := CR;

          with R2 do
          begin
            TopLeft := ClientToScreen(TopLeft);
            BottomRight := ClientToScreen(BottomRight);

            TopLeft := Parent.ScreenToClient(TopLeft);
            BottomRight := Parent.ScreenToClient(BottomRight);
          end;

          SaveIndex := SaveDC(DC);
          try
            SetViewportOrgEx(DC, Left + X, Top + Y, nil);
            IntersectClipRect(DC, 0, 0, Width, Height);

            Perform(WM_PRINT, DC, PRF_NONCLIENT or PRF_CLIENT or
              PRF_ERASEBKGND or PRF_CHILDREN);

            SetViewportOrgEx(DC, R2.Left + X, R2.Top + Y, nil);
            IntersectClipRect(DC, CR.Left, CR.Top, CR.Right, CR.Bottom);
            Perform(WM_PAINT, DC, 0);
          finally
            RestoreDC(DC, SaveIndex);
          end;
        end;
      end;
    end;
  end;
end;

function TSCDeGraphicControl.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
end;

function TSCDeGraphicControl.GetImages: TCustomImageList;
begin
  Result := FImages;
end;

function TSCDeGraphicControl.IsTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TSCDeGraphicControl.AdjustClientRect(var Rect: TRect);
var
  B: Integer;
begin
  B := GetBorderSize + FBorderProps.Width + GetInnerBorderSize;
  InflateRect(Rect, -B, -B);
end;

procedure TSCDeGraphicControl.Assign(Source: TPersistent);
begin
  if Source is TControl then
  begin
    with TControl(Source) do
    begin
      Self.Align := Align;
      Self.Anchors := Anchors;
      Self.BiDiMode := BiDiMode;
      Self.Caption := Caption;
      Self.Color := Color;
      Self.Constraints := Constraints;
      Self.Cursor := Cursor;
      Self.DragCursor := DragCursor;
      Self.DragKind := DragKind;
      Self.DragMode := DragMode;
      Self.Enabled := Enabled;
      Self.Font := Font;
      Self.Hint := Hint;
      Self.ParentBiDiMode := ParentBiDiMode;
      Self.ParentColor := ParentColor;
      Self.ParentFont := ParentFont;
      Self.ShowHint := ShowHint;
      Self.Visible := Visible;
      Self.Width := Width;
      Self.Height := Height;
    end;

    if Source is TSCDeGraphicControl then
    begin
      with TSCDeGraphicControl(Source) do
      begin
        Self.DoubleBuffered := DoubleBuffered;
        Self.BorderProps := BorderProps;
        Self.MouseInControl := MouseInControl;
        Self.MouseIsDown := MouseIsDown;
        Self.ImageIndex := ImageIndex;
        Self.ImageLayout := ImageLayout;
        Self.Images := Images;
        Self.Indent := Indent;
        Self.MouseDownPoint := MouseDownPoint;
        Self.PictureIndex := PictureIndex;
        Self.PictureList := PictureList;
        Self.Spacing := Spacing;
        Self.Transparent := Transparent;
      end;
    end;
  end else
    inherited Assign(Source);
end;

{$I SCDEVerRec.inc}

end.
