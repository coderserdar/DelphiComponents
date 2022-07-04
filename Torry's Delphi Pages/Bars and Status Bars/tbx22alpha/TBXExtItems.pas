unit TBXExtItems;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXExtItems.pas 128 2005-10-12 21:47:40Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls,
  TBX, TBXThemes, TB2Item, TB2Toolbar, TB2ExtItems, TBXLists, TBXStrUtils, TB2MRU;

type
  TTBXEditItemViewer = class;
  TTBXEditTyping = procedure(Sender: TObject; const Text: WideString) of object;

  { TTBXEditItem }
  { Extends standard TTBEditItem, providing additional features and some
    combo box functionality, which is used in descendants }

  TTBXEditItem = class(TTBEditItem)
  private
    FAlignment: TAlignment;
    FEditorFontSettings: TFontSettings;
    FFontSettings: TFontSettings;
    FReadOnly: Boolean;
    FShowImage: Boolean;
    FOnChange: TNotifyEvent;
    FOnTyping: TTBXEditTyping;
    FPassword: Boolean;
    procedure FontSettingsChanged(Sender: TObject);
    procedure ReadPasswordCharProperty(Reader: TReader);
    procedure SetPassword(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetShowImage(Value: Boolean);
    procedure SetFontSettings(Value: TFontSettings);
    procedure SetEditorFontSettings(Value: TFontSettings);
  protected
    FWriting: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    function  DoAutoComplete(var AText: WideString): Boolean; virtual;
    procedure DoTextChanged; override;
    procedure DoTyping(const S, CompleteText: WideString); virtual;
    function  GetImageIndex: Integer; virtual;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure GetPopupPosition(ParentView: TTBView; PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec); override;
    function  GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure WriteText(const AText: WideString);
  public
    function StartEditing(AView: TTBView): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property EditorFontSettings: TFontSettings read FEditorFontSettings write SetEditorFontSettings;
    property ExtendedAccept;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property ImageIndex;
    property Images;
    property Password: Boolean read FPassword write SetPassword default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowImage: Boolean read FShowImage write SetShowImage default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTyping: TTBXEditTyping read FOnTyping write FOnTyping;
    property OnSelect;
  end;

  TTBXEditItemViewer = class(TTBEditItemViewer)
  private
    FAutoCompleteCounter: Integer;
    function  MeasureEditCaption: TPoint;
    function  MeasureTextHeight: Integer;
  protected
    procedure AdjustEditControlFont(Font: TFont); override;
    procedure AdjustEditControlStyle(var Style, ExStyle: Cardinal; var Color: TColor); override;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function  HandleEditMessage(var Message: TMessage): Boolean; override;
    procedure HandleTyping; virtual;
    function  GetAccRole: Integer; override;
    function  GetEditMargins: TRect; override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); virtual;
    procedure GetItemInfo(out ItemInfo: TTBXItemInfo; IsHoverItem, IsPushed, UseMenuColor: Boolean); virtual;
    function  GetIndentBefore: Integer; virtual;
    function  GetIndentAfter: Integer; virtual;
    procedure GetEditRect(var R: TRect); override;
    function  GetPasswordChar: WideChar;
    function  IsToolbarSize: Boolean; override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    procedure SetupEditControl(EditControlHandle: HWND); override;
    function  ShowImage: Boolean; virtual;
  public
    function  IsToolbarStyle: Boolean; override;
  end;

  { TTBXSpinEditItem }
  TTBXCustomSpinEditItem = class;

  TSEValueType = (evtInteger, evtFloat, evtHex);
  TDecimal = 0..10;
  TSEConvertEvent = procedure(Sender: TTBXCustomSpinEditItem; const APrefix, APostfix: WideString; var AValue: Extended; var CanConvert: Boolean) of object;
  TSEStepEvent = procedure(Sender: TTBXCustomSpinEditItem; Step: Integer; const OldValue: Extended; var NewValue: Extended) of object;
  TSETextToValueEvent = procedure(Sender: TTBXCustomSpinEditItem; const AText: WideString; out AValue: Extended; var CanConvert: Boolean) of object;
  TSEValueToTextEvent = procedure(Sender: TTBXCustomSpinEditItem; const AValue: Extended; var Text: WideString) of object;

  TTBXCustomSpinEditItem = class(TTBXEditItem)
  private
    FDecimal: TDecimal;
    FLastGoodValue: Extended;
    FMaxValue: Extended;
    FMinValue: Extended;
    FIncrement: Extended;
    FSpaceBeforePostfix: Boolean;
    FSpaceAfterPrefix: Boolean;
    FPostfix: WideString;
    FPrefix: WideString;
    FSnap: Boolean;
    FValueType: TSEValueType;
    FOnConvert: TSEConvertEvent;
    FOnTextToValue: TSETextToValueEvent;
    FOnValueToText: TSEValueToTextEvent;
    FOnStep: TSEStepEvent;
    function  IsIncrementStored: Boolean;
    function  IsMinValueStored: Boolean;
    function  IsMaxValueStored: Boolean;
    function  IsValueStored: Boolean;
    function  GetValue: Extended;
    procedure SetValue(NewValue: Extended);
    procedure SetValueType(NewType: TSEValueType);
    procedure SetDecimal(NewDecimal: TDecimal);
    procedure SetIncrement(const NewIncrement: Extended);
    procedure SetPostfix(const NewPostfix: WideString);
    procedure SetPrefix(const NewPrefix: WideString);
    procedure SetSpaceAfterPrefix(UseSpace: Boolean);
    procedure SetSpaceBeforePostfix(UseSpace: Boolean);
    function  ValidateUnits(const S: WideString): Boolean;
    function  GetAsInteger: Integer;
    procedure SetAsInteger(AValue: Integer);
  protected
    function  CheckValue(const V: Extended): Extended;
    procedure ClickUp;
    procedure ClickDown;
    function  DoAcceptText(var NewText: WideString): Boolean; override;
    function  DoConvert(const APrefix, APostfix: WideString; var AValue: Extended): Boolean; virtual;
    procedure DoStep(Step: Integer; const OldValue: Extended; var NewValue: Extended); virtual;
    procedure DoTextChanged; override;
    function  DoTextToValue(const AText: WideString; out AValue: Extended): Boolean; virtual;
    procedure DoValueToText(const NewValue: Extended; var NewText: WideString); virtual;
    function  GetAsText(AValue: Extended): WideString;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function  ParseValue(const S: WideString; out V: Extended): Boolean;
    procedure WriteValue(NewValue: Extended);
    property Alignment default taRightJustify;
    property OnConvert: TSEConvertEvent read FOnConvert write FOnConvert;
    property OnStep: TSEStepEvent read FOnStep write FOnStep;
    property OnTextToValue: TSETextToValueEvent read FOnTextToValue write FOnTextToValue;
    property OnValueToText: TSEValueToTextEvent read FOnValueToText write FOnValueToText;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueType: TSEValueType read FValueType write SetValueType default evtInteger;
    property AsInteger: Integer read GetAsInteger write SetAsInteger stored False;
    property Decimal: TDecimal read FDecimal write SetDecimal default 2;
    property Increment: Extended read FIncrement write SetIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write FMaxValue stored IsMaxValueStored;
    property MinValue: Extended read FMinValue write FMinValue stored IsMinValueStored;
    property Postfix: WideString read FPostfix write SetPostfix;
    property Prefix: WideString read FPrefix write SetPrefix;
    property Snap: Boolean read FSnap write FSnap default True;
    property SpaceAfterPrefix: Boolean read FSpaceAfterPrefix write SetSpaceAfterPrefix default False;
    property SpaceBeforePostfix: Boolean read FSpaceBeforePostfix write SetSpaceBeforePostfix default False;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
  published
    property Text stored False;
  end;

  TTBXSpinEditItem = class(TTBXCustomSpinEditItem)
  published
    property ValueType;
    property Alignment;
    property Decimal;
    property Increment;
    property MaxValue;
    property MinValue;
    property Postfix;
    property Prefix;
    property Snap;
    property SpaceAfterPrefix;
    property SpaceBeforePostfix;
    property Value;
    property OnConvert;
    property OnStep;
    property OnTextToValue;
    property OnValueToText;
  end;

  TSEBtnState = (ebsNone, ebsUp, ebsDown);

  TTBXSpinEditViewer = class(TTBXEditItemViewer)
  private
    FBtnState: TSEBtnState;
    FBtnTimer: TTimer;
    procedure TimerHandler(Sender: TObject);
  protected
    function  GetIndentAfter: Integer; override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); override;
    function  HandleEditMessage(var Message: TMessage): Boolean; override;
    procedure InvalidateButtons;
    function  IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure LosingCapture; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
  public
    destructor Destroy; override;
  end;

  { TTBXCustomDropDownItem }
  { An extended edit item tb2k with a button. The dropdown list support is
    implemented in descendants, such as TTBXComboBoxItem }

  TTBXCustomDropDownItem = class(TTBXEditItem)
  private
    FAlwaysSelectFirst: Boolean;
    FDropDownList: Boolean;
  protected
    function CreatePopup(const ParentView: TTBView; const ParentViewer: TTBItemViewer;
      const PositionAsSubmenu, SelectFirstItem, Customizing: Boolean;
      const APopupPoint: TPoint; const Alignment: TTBPopupAlignment): TTBPopupWindow; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AlwaysSelectFirst: Boolean read FAlwaysSelectFirst write FAlwaysSelectFirst default True;
    property DropDownList: Boolean read FDropDownList write FDropDownList default False;
  end;

  TTBXDropDownItem = class(TTBXCustomDropDownItem)
  published
    property AlwaysSelectFirst;
    property DropDownList;
    property LinkSubitems;
    property SubMenuImages;
  end;

  TTBXDropDownItemViewer = class(TTBXEditItemViewer)
  protected
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); override;
    function  GetIndentAfter: Integer; override;
    function  HandleEditMessage(var Message: TMessage): Boolean; override;
    function  IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TTBXComboBoxItem }
  { A combination of dropdown combo with a stringlist subitem }
  TTBXComboBoxItem = class;
  TTBXCAdjustImageIndex = procedure(Sender: TTBXComboBoxItem; const AText: WideString;
    AIndex: Integer; var ImageIndex: Integer) of object;

  TTBXComboBoxItem = class(TTBXCustomDropDownItem)
  private
    FAutoComplete: Boolean;
    FList: TTBXStringList;
    FOnItemClick: TNotifyEvent;
    FOnAdjustImageIndex: TTBXCAdjustImageIndex;
    function GetItemIndex: Integer;
    function GetMaxVisibleItems: Integer;
    function GetMaxWidth: Integer;
    function GetMinWidth: Integer;
    function GetLines: TStringListW;
    procedure HandleAdjustImageIndex(Sender: TTBXCustomList; AItemIndex: Integer; var ImageIndex: Integer);
    function GetShowListImages: Boolean;
    function GetOnClearItem: TTBXLPaintEvent;
    function GetOnDrawItem: TTBXLPaintEvent;
    function GetOnMeasureHeight: TTBXLMeasureHeight;
    function GetOnMeasureWidth: TTBXLMeasureWidth;
    procedure ListChangeHandler(Sender: TObject);
    procedure ListClickHandler(Sender: TObject);
    procedure ReadStringsLegacyProperty(Reader: TReader);
    procedure SetItemIndex(Value: Integer);
    procedure SetMaxVisibleItems(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetLines(Value: TStringListW);
    procedure SetOnClearItem(Value: TTBXLPaintEvent);
    procedure SetOnDrawItem(Value: TTBXLPaintEvent);
    procedure SetOnMeasureHeight(Value: TTBXLMeasureHeight);
    procedure SetOnMeasureWidth(Value: TTBXLMeasureWidth);
    procedure SetShowListImages(Value: Boolean);
  protected
    CachedImageIndex: Integer;
    CacheValid: Boolean;
    IsChanging: Boolean;
    procedure AdjustImageIndex(const AText: WideString; AIndex: Integer; var ImageIndex: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function  DoAutoComplete(var AText: WideString): Boolean; override;
    procedure DoListChange; virtual;
    procedure DoListClick; virtual;
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
    function  GetImageIndex: Integer; override;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function  GetStringListClass: TTBXStringListClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
  published
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property DropDownList;
    property MaxListWidth: Integer read GetMaxWidth write SetMaxWidth default 0;
    property MaxVisibleItems: Integer read GetMaxVisibleItems write SetMaxVisibleItems default 8;
    property MinListWidth: Integer read GetMinWidth write SetMinWidth default 64;
    property ShowListImages: Boolean read GetShowListImages write SetShowListImages default False;
    property Lines: TStringListW read GetLines write SetLines;
    property SubMenuImages;
    property OnTyping;
    property OnAdjustImageIndex: TTBXCAdjustImageIndex read FOnAdjustImageIndex write FOnAdjustImageIndex;
    property OnClearItem: TTBXLPaintEvent read GetOnClearItem write SetOnClearItem;
    property OnDrawItem: TTBXLPaintEvent read GetOnDrawItem write SetOnDrawItem;
    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnMeasureHeight: TTBXLMeasureHeight read GetOnMeasureHeight write SetOnMeasureHeight;
    property OnMeasureWidth: TTBXLMeasureWidth read GetOnMeasureWidth write SetOnMeasureWidth;
    property OnPopup;
  end;

  TTBXComboBoxItemViewer = class(TTBXDropDownItemViewer)
  protected
    function HandleEditMessage(var Message: TMessage): Boolean; override;
  end;

  { TTBXLabelItem }

  TTBXLabelOrientation = (tbxoAuto, tbxoHorizontal, tbxoVertical);
  TNonNegativeInt = 0..MaxInt;

  TTBXLabelItem = class(TTBCustomItem)
  private
    FFontSettings: TFontSettings;
    FMargin: Integer;
    FShowAccelChar: Boolean;
    FOrientation: TTBXLabelOrientation;
    FOnAdjustFont: TAdjustFontEvent;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetMargin(Value: Integer);
    procedure SetOrientation(Value: TTBXLabelOrientation);
    procedure SetFontSettings(Value: TFontSettings);
    procedure SetShowAccelChar(Value: Boolean);
  protected
    function GetItemViewerClass (AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCaption(const Value: TCaption);
  published
    property Caption;
    property Enabled;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property Margin: Integer read FMargin write SetMargin default 0;
    property Orientation: TTBXLabelOrientation read FOrientation write SetOrientation default tbxoAuto;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Visible;
    property OnAdjustFont: TAdjustFontEvent read FOnAdjustFont write FOnAdjustFont;
  end;

  TTBXLabelItemViewer = class(TTBItemViewer)
  protected
    function  GetIsHoriz: Boolean; virtual;
    procedure DoAdjustFont(AFont: TFont; StateFlags: Integer); virtual;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function  IsToolbarSize: Boolean; override;
  public
    function  IsToolbarStyle: Boolean; override;
  end;

  { TTBXColorItem }

  TTBXColorItem = class(TTBXCustomItem)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    function GetItemViewerClass (AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property AutoCheck;
    property Caption;
    property Checked;
    property Color: TColor read FColor write SetColor default clWhite;
    property DisplayMode;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    property Hint;
    property InheritOptions;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property ShortCut;
    property Visible;
    property OnAdjustFont;
    property OnClick;
  end;

  TTBXColorItemViewer = class(TTBXItemViewer)
  protected
    procedure DoPaintCaption(Canvas: TCanvas; const ClientAreaRect: TRect;
      var CaptionRect: TRect; IsTextRotated: Boolean; var PaintDefault: Boolean); override;
    function GetImageShown: Boolean; override;
    function GetImageSize: TSize; override;
    procedure DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo); override;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); override;
  end;

  { TTBXMRUList }

  TTBXMRUList = class(TTBMRUList)
  private
    FKeyShift: Integer;
    procedure SetKeyShift(Value: Integer);
  protected
    function GetFirstKey: Integer; override;
    function GetItemClass: TTBCustomItemClass; override;
    procedure SetItemCaptions; override;
  published
    property KeyShift: Integer read FKeyShift write SetKeyShift default 0;
  end;

  { TTBXMRUListItem }

  TTBXMRUListItem = class(TTBXCustomItem)
  private
    FMRUList: TTBMRUList;
    procedure SetMRUList(Value: TTBMRUList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MRUList: TTBMRUList read FMRUList write SetMRUList;
  end;

implementation

uses TB2Common, TB2Consts, TBXUtils, TypInfo, Math, TBXUxThemes, ImgList {$IFNDEF JR_D5}, DsgnIntf{$ENDIF};

const
  { Repeat intervals for spin edit items }
  SE_FIRSTINTERVAL = 400;
  SE_INTERVAL = 100;

type
  TTBViewAccess = class(TTBView);
  TTBItemAccess = class(TTBCustomItem);
  TTBMRUListAccess = class(TTBMRUList);
  TCustomEditAccess = class(TCustomEdit);
  TFontSettingsAccess = class(TFontSettings);
  TReaderAccess = class(TReader);

{ Misc. functions }

function IsPrefix(const Prefix, S: string): Boolean;
var                          
  L1, L2: Integer;
begin
  L1 := Length(Prefix);
  L2 := Length(S);
  if L1 > L2 then Result := False
  else Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    PChar(Prefix), L1, PChar(S), L2) = 2;
end;

function IsPrefixW(const Prefix, S: WideString): Boolean;
var
  L1, L2: Integer;
begin
  L1 := Length(Prefix);
  L2 := Length(S);
  if L1 > L2 then Result := False
  else
  begin
    SetLastError(0);
    Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PWideChar(Prefix), L1, PWideChar(S), L2) = 2;
    case GetLastError of
      0: ;
      ERROR_CALL_NOT_IMPLEMENTED: Result := IsPrefix(Prefix, S);
    else
      RaiseLastOSError;
    end;
  end;
end;

procedure SetWideString(var S: WideString; Buffer: PWideChar; Len: Integer);
begin
  SetLength(S, Len);
  if Len > 0 then Move(Buffer^, PWideChar(S)^, Len * 2);
end;

var
  ComCtlVer: Cardinal = Cardinal(-1);

function GetComCtlVersion: Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ComCtlVer <> Cardinal(-1) then Result := ComCtlVer
  else
  begin
    Result := Cardinal(-1);
    FileName := comctl32;
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            Result:= FI.dwProductVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
end;

//============================================================================//

{ TTBXEdit }

type
  TTBXEdit = class(TEdit)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

procedure TTBXEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

procedure TTBXEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;


//============================================================================//

{ TTBXEditItem }

constructor TTBXEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditorFontSettings := TFontSettings.Create;
  FFontSettings := TFontSettings.Create;
  TFontSettingsAccess(FEditorFontSettings).OnChange := FontSettingsChanged;
  TFontSettingsAccess(FFontSettings).OnChange := FontSettingsChanged;
end;

procedure TTBXEditItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  { in older versions, PasswordChar was used instead of the Password property
    ReadPasswordCharProperty is used to convert convert/upgrade PasswordChar to
    the Password property }
  Filer.DefineProperty('PasswordChar', ReadPasswordCharProperty, nil, False);
end;

destructor TTBXEditItem.Destroy;
begin
  FFontSettings.Free;
  FEditorFontSettings.Free;
  inherited;
end;

function TTBXEditItem.DoAutoComplete(var AText: WideString): Boolean;
begin
  Result := False;
end;

procedure TTBXEditItem.DoTextChanged;
begin
  if not FWriting then
  begin
    inherited;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TTBXEditItem.DoTyping(const S, CompleteText: WideString);
begin
  if Assigned(FOnTyping) then FOnTyping(Self, S);
end;

procedure TTBXEditItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TTBXEditItem.GetImageIndex: Integer;
begin
  Result := ImageIndex;
end;

function TTBXEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXEditItemViewer;
end;

procedure TTBXEditItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  VT: Integer;
begin
  inherited;
  VT := GetWinViewType(PopupWindow);
  PopupPositionRec.PlaySound := VT and VT_TYPEMASK <> VT_LISTBOX;
end;

function TTBXEditItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXPopupWindow;
end;

procedure TTBXEditItem.ReadPasswordCharProperty(Reader: TReader);
var
  C: Char;
begin
  C := Reader.ReadChar;
  if C <> #0 then Password := True;
end;

procedure TTBXEditItem.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Change(True);
  end;
end;

procedure TTBXEditItem.SetEditorFontSettings(Value: TFontSettings);
begin
  FEditorFontSettings.Assign(Value);
end;

procedure TTBXEditItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings.Assign(Value);
end;

procedure TTBXEditItem.SetPassword(Value: Boolean);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    Change(True);
  end;
end;

procedure TTBXEditItem.SetShowImage(Value: Boolean);
begin
  FShowImage := Value;
  Change(True);
end;

function TTBXEditItem.StartEditing(AView: TTBView): Boolean;
var
  V: TTBItemViewer;
  SaveText: WideString;
begin
  Result := False;
  if Assigned(AView) then
  begin
    V := AView.Find(Self);
    if V is TTBXEditItemViewer then
    begin
      SaveText := Text;
      TTBXEditItemViewer(V).DoExecute;
      Result := Text <> SaveText;
    end;
  end;
end;

procedure TTBXEditItem.WriteText(const AText: WideString);
begin
  if not FWriting then
  try
    FWriting := True;
    Text := AText;
  finally
    FWriting := False;
  end;
end;


//============================================================================//

{ TTBXEditItemViewer }

procedure TTBXEditItemViewer.AdjustEditControlFont(Font: TFont);
begin
  TTBXEditItem(Item).EditorFontSettings.Apply(Font);
end;

procedure TTBXEditItemViewer.AdjustEditControlStyle(var Style, ExStyle: Cardinal; var Color: TColor);
const
  Alignments: array [TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
  Passwords: array [Boolean] of DWORD = (0, ES_PASSWORD);
var
  EffectiveAlignment: TAlignment;
begin
  inherited;
  if Item is TTBXEditItem then
  begin
    EffectiveAlignment := TTBXEditItem(Item).Alignment;
    { todo: handle bidimode here }
    Style := Style or Alignments[EffectiveAlignment];
    Style := Style or Passwords[TTBXEditItem(Item).Password];
  end;
end;

procedure TTBXEditItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  W, B: Integer;
  EditBoxHeight: Integer;
  EditCaptionSize: TPoint;
begin
  if Self.Item is TTBXEditItem then with CurrentTheme do
  begin
    B := CurrentTheme.GetIntegerMetrics(TMI_EDIT_FRAMEWIDTH);

    AWidth := TTBXEditItem(Item).EditWidth;
    if not IsToolbarStyle then
    begin
      EditCaptionSize := MeasureEditCaption;
      W := EditCaptionSize.X;
      if W > 0 then Inc(W, MenuLeftCaptionMargin + MenuRightCaptionMargin + MenuImageTextSpace);
      Inc(AWidth, GetPopupMargin(Self) + MenuImageTextSpace + W + EditMenuRightIndent);
    end
    else
    begin
      EditCaptionSize.X := 0;
      EditCaptionSize.Y := 0;
    end;

    EditBoxHeight := MeasureTextHeight + 1;
    Inc(EditBoxHeight, EditTextMarginVert * 2 + B * 2);
    AHeight := Max(EditBoxHeight, EditCaptionSize.Y);
    if not IsToolbarStyle then AHeight := AHeight;
    if GetBooleanMetrics(TMB_EDITHEIGHTEVEN) then AHeight := (AHeight + 1) and not $01
    else AHeight := AHeight or $01;
  end
  else inherited;
end;

function TTBXEditItemViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_SPINBUTTON = $34;
  ROLE_SYSTEM_COMBOBOX = $2E;
begin
  Result := inherited GetAccRole;
  if Self is TTBXSpinEditViewer then Result := ROLE_SYSTEM_SPINBUTTON
  else if Self is TTBXDropDownItemViewer then Result := ROLE_SYSTEM_COMBOBOX;
end;

procedure TTBXEditItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
begin
  FillChar(EditInfo, SizeOf(EditInfo), 0);
  EditInfo.LeftBtnWidth := GetIndentBefore;
  EditInfo.RightBtnWidth := GetIndentAfter;
end;

function TTBXEditItemViewer.GetEditMargins: TRect;
var
  MarginHorz, MarginVert, Frame: Integer;
begin
  MarginHorz := CurrentTheme.GetIntegerMetrics(TMI_EDIT_TEXTMARGINHORZ);
  MarginVert := CurrentTheme.GetIntegerMetrics(TMI_EDIT_TEXTMARGINVERT);
  Frame := CurrentTheme.GetIntegerMetrics(TMI_EDIT_FRAMEWIDTH);
  Result.Left := MarginHorz + Frame;
  Result.Top := MarginVert + Frame;
  Result.Right := MarginHorz + Frame;
  Result.Bottom := MarginVert + Frame;
end;

procedure TTBXEditItemViewer.GetEditRect(var R: TRect);
const
  TB2K_EDIT_BORDER = 3;
var
  W: Integer;
begin
  if Item is TTBXEditItem then with CurrentTheme do
  begin
    R := BoundsRect;
    if not IsToolbarStyle then
    begin
      W := MeasureEditCaption.X;
      if W > 0 then Inc(W, MenuLeftCaptionMargin + MenuRightCaptionMargin + MenuImageTextSpace);
      Inc(R.Left, GetPopupMargin(Self) + MenuImageTextSpace + W);
      Dec(R.Right, EditMenuRightIndent);
    end;
    Inc(R.Left, GetIndentBefore);
    Dec(R.Right, GetIndentAfter);
  end
  else inherited;
end;

function TTBXEditItemViewer.GetIndentAfter: Integer;
begin
  Result := 0;
end;

function TTBXEditItemViewer.GetIndentBefore: Integer;
var
  ImgList: TCustomImageList;
begin
  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then Result := ImgList.Width + 2
    else Result := 0;
  end
  else Result := 0;
end;

procedure TTBXEditItemViewer.GetItemInfo(out ItemInfo: TTBXItemInfo; IsHoverItem, IsPushed, UseMenuColor: Boolean);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
var
  Item: TTBXEditItem;
begin
  Item := TTBXEditItem(Self.Item);

  FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[IsToolbarStyle]
    or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := Item.Checked;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not View.MouseOverSelected then
      ItemInfo.HoverKind := hkKeyboardHover
    else
      if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  if not IsToolbarStyle then ItemInfo.PopupMargin := GetPopupMargin(Self);
end;

function TTBXEditItemViewer.GetPasswordChar: WideChar;
begin
  { note: this does not repeat the exact behavior of edit control }
  if GetComCtlVersion >= $60000 then Result := #$25CF
  else Result := '*';
end;

function TTBXEditItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
const
  CharKeys = [VK_SPACE, $30..$5A, VK_NUMPAD0..VK_DIVIDE, $BA..$F5];
begin
  case Message.Msg of
    WM_KEYDOWN:
      begin
        if Message.WParam in CharKeys then Inc(FAutoCompleteCounter)
      end;
    WM_KEYUP:
      begin
        if Message.WParam in CharKeys then Dec(FAutoCompleteCounter);
      end;
    WM_COMMAND:
      begin
        if (TWMCommand(Message).NotifyCode = EN_CHANGE) and not FUpdating then HandleTyping;
      end;
  end;
  Result := inherited HandleEditMessage(Message);
end;

procedure TTBXEditItemViewer.HandleTyping;
var
  S, S2: WideString;
begin
  S := GetHandleTextW(EditControlHandle);
  S2 := S;
  if (Length(S) > 0) and (FAutoCompleteCounter > 0) and
    TTBXEditItem(Item).DoAutoComplete(S2) then
  begin
    SetEditControlText(S2);
    SendMessage(EditControlHandle, EM_SETSEL, Length(S) - 1, Length(S2) - 1);
    S := S2;
  end;
  TTBXEditItem(Item).DoTyping(S, S2);
end;

function TTBXEditItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and VT_TYPEMASK) = VT_TOOLBOX);
end;

function TTBXEditItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and VT_TYPEMASK) = VT_TOOLBOX);
end;

function TTBXEditItemViewer.MeasureEditCaption: TPoint;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  TextMetric: TTextMetric;
  S: WideString;
begin
  Result.X := 0;
  Result.Y := 0;
  if Item is TTBXEditItem then
  begin
    S := StripAccelCharsW(TTBXEditItem(Item).EditCaption);
    if Length(S) > 0 then
    begin
      DummyColor := clWhite;
      DC := GetDC(0);
      Fnt := TTBXEditItem(Item).FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
      OldFnt := SelectObject(DC, Fnt);
      GetTextExtentPoint32W(DC, PWideChar(S), Length(S), TSize(Result));
      GetTextMetrics(DC, TextMetric);
      Inc(Result.Y, TextMetric.tmExternalLeading);
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
      ReleaseDC(0, DC);
    end;
  end;
end;

function TTBXEditItemViewer.MeasureTextHeight: Integer;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  TextMetric: TTextMetric;
begin
  Result := 0;
  if Item is TTBXEditItem then
  begin
    DummyColor := clWhite;
    DC := GetDC(0);
    Fnt := TTBXEditItem(Item).EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
    OldFnt := SelectObject(DC, Fnt);
    Result := GetTextHeight(DC);
    GetTextMetrics(DC, TextMetric);
    Inc(Result, TextMetric.tmExternalLeading);
    SelectObject(DC, OldFnt);
    DeleteObject(Fnt);
    ReleaseDC(0, DC);
  end;
end;

procedure TTBXEditItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  FillColors: array [Boolean] of Integer = (COLOR_BTNFACE, COLOR_WINDOW);
  TextColors: array [Boolean] of Integer = (COLOR_GRAYTEXT, COLOR_WINDOWTEXT);
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  DC: HDC;
  Item: TTBXEditItem;
  S: WideString;
  R, R2: TRect;
  M, W: Integer;
  ItemInfo: TTBXItemInfo;
  EditInfo: TTBXEditInfo;
  ImgList: TCustomImageList;
  ImgIndex: Integer;
  Fnt, OldFnt: HFont;
  C: TColor;
begin
  DC := Canvas.Handle;
  Item := TTBXEditItem(Self.Item);
  GetItemInfo(ItemInfo, IsHoverItem, IsPushed, UseDisabledShadow);
  GetEditInfo(EditInfo, ItemInfo);
  R := ClientAreaRect;

  if not IsToolbarStyle then with CurrentTheme do
  begin
    S := Item.EditCaption;

    if Length(S) > 0 then
    begin
      { measure EditCaption }
      Fnt := TTBXEditItem(Item).FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
      OldFnt := SelectObject(DC, Fnt);
      W := GetTextWidthW(DC, S, True) + MenuImageTextSpace + MenuLeftCaptionMargin + MenuRightCaptionMargin;
      SelectObject(DC, OldFnt);
    end
    else
    begin
      Fnt := 0; // to suppress compiler warning
      W := 0;
    end;

    M := GetPopupMargin(Self);
    if not GetBooleanMetrics(TMB_EDITMENUFULLSELECT) then R.Right := M + W
    else Dec(R.Right, EditMenuRightIndent);
    PaintMenuItemFrame(Canvas.Handle, R, ItemInfo);
    Inc(R.Left, M + MenuImageTextSpace);
    R.Right := ClientAreaRect.Right - EditMenuRightIndent;

    if Length(S) > 0 then
    begin
      Inc(R.Left, MenuLeftCaptionMargin);
      OldFnt := SelectObject(DC, Fnt);
      PaintCaption(DC, R, ItemInfo, S, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      W := GetTextWidthW(DC, S, True);
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
      Inc(R.Left, W + MenuRightCaptionMargin + MenuImageTextSpace);
    end;
  end;

  CurrentTheme.PaintEditFrame(DC, R, ItemInfo, EditInfo);
  W := CurrentTheme.EditFrameWidth;
  InflateRect(R, -W - CurrentTheme.EditTextMarginHorz, -W - CurrentTheme.EditTextMarginVert);

  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then
    begin
      R2.Left := R.Left;
      R2.Right := R.Left + ImgList.Width;
      R2.Top := (R.Top + R.Bottom + 1 - ImgList.Height) div 2;
      R2.Bottom := R2.Top + ImgList.Height;
      ImgIndex := TTBXEditItem(Item).GetImageIndex;
      if Item.Enabled then ImgList.Draw(Canvas, R.Left, R2.Top, ImgIndex)
      else DrawTBXImage(Canvas.Handle, R2, ImgList, ImgIndex, ISF_DISABLED);
    end;
  end;
  Inc(R.Left, EditInfo.LeftBtnWidth);
  Dec(R.Right, EditInfo.RightBtnWidth + 1);

  if Item.Text <> '' then
  begin
    Fnt := Item.EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
    OldFnt := SelectObject(DC, Fnt);
    if not Item.Enabled then C := GetSysColor(COLOR_GRAYTEXT)
    else if Item.EditorFontSettings.Color <> clNone then C := ColorToRGB(Item.EditorFontSettings.Color)
    else C := GetSysColor(COLOR_WINDOWTEXT);

    S := Item.Text;
    if TTBXEditItem(Item).Password then S := StringOfCharW(GetPasswordChar, Length(S));
    DrawTextRW(DC, S, R, DT_SINGLELINE or DT_NOPREFIX or Alignments[Item.Alignment], C);
    SelectObject(DC, OldFnt);
    DeleteObject(Fnt);
  end;
end;

procedure TTBXEditItemViewer.SetupEditControl(EditControlHandle: HWND);
var
  C: WideChar;
  S: AnsiString;
begin
  inherited;
  FAutoCompleteCounter := 0;
  if (Item is TTBXEditItem) and TTBXEditItem(Item).ReadOnly then
    SendMessage(EditControlHandle, EM_SETREADONLY, 1, 0);
  if (Item is TTBXEditItem) and TTBXEditItem(Item).Password then
  begin
    C := GetPasswordChar;
    if IsWindowUnicode(EditControlHandle) then
      SendMessageW(EditControlHandle, EM_SETPASSWORDCHAR, Integer(C), 0)
    else
    begin
      S := C;
      if Length(S) > 0 then
        SendMessageA(EditControlHandle, EM_SETPASSWORDCHAR, Integer(S[1]), 0);
    end;
  end;    
end;

function TTBXEditItemViewer.ShowImage: Boolean;
begin
  Result := TTBXEditItem(Item).ShowImage;
end;


//============================================================================//

{ TTBXCustomDropDownItem }

constructor TTBXCustomDropDownItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisCombo, tbisSubmenu, tbisSubitemsEditable] - [tbisDontSelectFirst];
  FAlwaysSelectFirst := True;
end;

function TTBXCustomDropDownItem.CreatePopup(const ParentView: TTBView;
  const ParentViewer: TTBItemViewer; const PositionAsSubmenu,
  SelectFirstItem, Customizing: Boolean; const APopupPoint: TPoint;
  const Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  SelectFirst: Boolean;
begin
  if AlwaysSelectFirst then SelectFirst := True
  else SelectFirst := SelectFirstItem;
  Result := inherited CreatePopup(ParentView, ParentViewer, PositionAsSubmenu,
    SelectFirst, Customizing, APopupPoint, Alignment);
end;

function TTBXCustomDropDownItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXDropDownItemViewer;
end;


//----------------------------------------------------------------------------//

{ TTBXDropDownItemViewer }

procedure TTBXDropDownItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
begin
  if not TTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

procedure TTBXDropDownItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
const
  CDisabled: array [Boolean] of Integer = (EBDS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBDS_HOT);
  CPressed: array [Boolean] of Integer = (0, EBDS_PRESSED);
begin
  inherited GetEditInfo(EditInfo, ItemInfo);
  EditInfo.RightBtnInfo.ButtonType := EBT_DROPDOWN;
  EditInfo.RightBtnInfo.ButtonState := CDisabled[ItemInfo.Enabled] or
    CHot[ItemInfo.HoverKind = hkMouseHover] or CPressed[ItemInfo.Pushed];
end;

function TTBXDropDownItemViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then Result := CurrentTheme.EditBtnWidth
  else Result := GetSystemMetrics(SM_CXMENUCHECK) + 2;
end;

function TTBXDropDownItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if TWMKeyDown(Message).CharCode = VK_F4 then
    begin
      if View.OpenViewer = Self then View.CloseChildPopups
      else View.OpenChildPopup(True);
      Result := True;
      Exit;
    end;
  end;
  Result := inherited HandleEditMessage(Message);
end;

function TTBXDropDownItemViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := not (tbisSubmenu in TTBXCustomDropDownItem(Item).ItemStyle);
  if TTBXCustomDropDownItem(Item).DropDownList then Result := False
  else if (tbisCombo in TTBXCustomDropDownItem(Item).ItemStyle) then
    Result := X < (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TTBXDropDownItemViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
  begin
    TTBViewAccess(View).OpenChildPopup(True);
    Key := 0;
    Exit;
  end;
  if not TTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

//============================================================================//

{ TTBXComboBoxItem }

procedure TTBXComboBoxItem.AdjustImageIndex(const AText: WideString;
  AIndex: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnAdjustImageIndex) then FOnAdjustImageIndex(Self, AText, AIndex, ImageIndex);
end;

constructor TTBXComboBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSubItemsEditable];
  FAutoComplete := True;
  FList := GetStringListClass.Create(Self);
  FList.OnChange := ListChangeHandler;
  FList.OnClick := ListClickHandler;
  FList.OnAdjustImageIndex := HandleAdjustImageIndex;
  MinListWidth := 64;
end;

procedure TTBXComboBoxItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Strings', ReadStringsLegacyProperty, nil, False);
end;

function TTBXComboBoxItem.DoAutoComplete(var AText: WideString): Boolean;
var
  I: Integer;
  S, R: WideString;
  TemplateL, MinL, L: Integer;
begin
  Result := False;
  if Length(AText) > 0 then
  begin
    { choose the shortest matching string }
    TemplateL := Length(AText);
    MinL := MaxInt;
    SetLength(R, 0);
    for I := 0 to FList.Lines.Count - 1 do
    begin
      S := FList.Lines[I];
      L := Length(S);
      if (L >= TemplateL) and (L < MinL) and IsPrefixW(AText, S) then
      begin
        R := S;
        MinL := L;
        if MinL = TemplateL then Break;
      end;
    end;
    Result := Length(R) > 0;
    if Result then AText := R;
  end;
end;

procedure TTBXComboBoxItem.DoListChange;
begin
  { Update text in edit item. This will call OnChange automatically }
  if (FList.ItemIndex >= 0) and (FList.ItemIndex < FList.Lines.Count) then
  begin
    IsChanging := True;
    try
      if Text <> FList.Lines[Flist.ItemIndex] then
      begin
        Text := FList.Lines[FList.ItemIndex];
      end;
    finally
      IsChanging := False;
    end;
  end;
end;

procedure TTBXComboBoxItem.DoListClick;
begin
  if Assigned(FOnItemClick) then FOnItemClick(Self);
end;

procedure TTBXComboBoxItem.DoPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  inherited;
  FList.ItemIndex := FList.Lines.IndexOf(Text);
end;

function TTBXComboBoxItem.GetImageIndex: Integer;
begin
  if not CacheValid then
  begin
    CachedImageIndex := ImageIndex;
    if ItemIndex >= 0 then CachedImageIndex := ItemIndex;
    AdjustImageIndex(Text, -1, CachedImageIndex);
    CacheValid := True;
  end;
  Result := CachedImageIndex;
end;

function TTBXComboBoxItem.GetItemIndex: Integer;
begin
  Result := FList.ItemIndex;
end;

function TTBXComboBoxItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXComboBoxItemViewer;
end;

function TTBXComboBoxItem.GetLines: TStringListW;
begin
  Result := FList.Lines;
end;

function TTBXComboBoxItem.GetMaxVisibleItems: Integer;
begin
  Result := FList.MaxVisibleItems;
end;

function TTBXComboBoxItem.GetMaxWidth: Integer;
begin
  Result := FList.MaxWidth;
end;

function TTBXComboBoxItem.GetMinWidth: Integer;
begin
  Result := FList.MinWidth;
end;

function TTBXComboBoxItem.GetOnClearItem: TTBXLPaintEvent;
begin
  Result := FList.OnClearItem;
end;

function TTBXComboBoxItem.GetOnDrawItem: TTBXLPaintEvent;
begin
  Result := FList.OnDrawItem;
end;

function TTBXComboBoxItem.GetOnMeasureHeight: TTBXLMeasureHeight;
begin
  Result := FList.OnMeasureHeight;
end;

function TTBXComboBoxItem.GetOnMeasureWidth: TTBXLMeasureWidth;
begin
  Result := FList.OnMeasureWidth;
end;

function TTBXComboBoxItem.GetShowListImages: Boolean;
begin
  Result := FList.ShowImages;
end;

function TTBXComboBoxItem.GetStringListClass: TTBXStringListClass;
begin
  Result := TTBXStringList;
end;

procedure TTBXComboBoxItem.HandleAdjustImageIndex(Sender: TTBXCustomList;
  AItemIndex: Integer; var ImageIndex: Integer);
begin
  AdjustImageIndex(FList.Lines[AItemIndex], AItemIndex, ImageIndex);
end;

procedure TTBXComboBoxItem.ListChangeHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListChange;
end;

procedure TTBXComboBoxItem.ListClickHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListClick;
end;

procedure TTBXComboBoxItem.Loaded;
begin
  inherited;
  if FList.Lines.IndexOf(Text) >= 0 then
  begin
    IsChanging := True;
    try
      FList.ItemIndex := FList.Lines.IndexOf(Text);
    finally
      IsChanging := False;
    end;
  end;
  if not (csDesigning in ComponentState) then Add(FList);
end;

procedure TTBXComboBoxItem.ReadStringsLegacyProperty(Reader: TReader);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(TStringListW, 'Lines');
  TReaderAccess(Reader).ReadPropValue(Lines, PropInfo);
end;

procedure TTBXComboBoxItem.SetItemIndex(Value: Integer);
begin
  FList.ItemIndex := Value;
end;

procedure TTBXComboBoxItem.SetLines(Value: TStringListW);
begin
  FList.Lines := Value;
end;

procedure TTBXComboBoxItem.SetMaxVisibleItems(Value: Integer);
begin
  FList.MaxVisibleItems := Value;
end;

procedure TTBXComboBoxItem.SetMaxWidth(Value: Integer);
begin
  FList.MaxWidth := Value;
end;

procedure TTBXComboBoxItem.SetMinWidth(Value: Integer);
begin
  FList.MinWidth := Value;
end;

procedure TTBXComboBoxItem.SetOnClearItem(Value: TTBXLPaintEvent);
begin
  FList.OnClearItem := Value;
end;

procedure TTBXComboBoxItem.SetOnDrawItem(Value: TTBXLPaintEvent);
begin
  FList.OnDrawItem := Value;
end;

procedure TTBXComboBoxItem.SetOnMeasureHeight(Value: TTBXLMeasureHeight);
begin
  FList.OnMeasureHeight := Value;
end;

procedure TTBXComboBoxItem.SetOnMeasureWidth(Value: TTBXLMeasureWidth);
begin
  FList.OnMeasureWidth := Value;
end;

procedure TTBXComboBoxItem.SetShowListImages(Value: Boolean);
begin
  FList.ShowImages := Value;
end;


//============================================================================//

{ TTBXComboBoxItemViewer }

function TTBXComboBoxItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if (Message.Msg = WM_KEYDOWN) then with TTBXComboBoxItem(Item) do
  begin
    case Message.wParam of
      VK_UP:
        begin
          ItemIndex := Max(0, ItemIndex - 1);
          SendMessageW(EditControlHandle, EM_SETSEL, 0, -1);
          Result := True;
        end;

      VK_DOWN:
        begin
          ItemIndex := Min(ItemIndex + 1, Lines.Count - 1);
          SendMessageW(EditControlHandle, EM_SETSEL, 0, -1);
          Result := True;
        end;
    else
      Result := inherited HandleEditMessage(Message);
    end
  end
  else Result := inherited HandleEditMessage(Message);
end;


//============================================================================//

{ TTBXLabelItem }

constructor TTBXLabelItem.Create(AOwner: TComponent);
begin
  inherited;
  FFontSettings := TFontSettings.Create;
  TFontSettingsAccess(FFontSettings).OnChange := FontSettingsChanged;
  FShowAccelChar := True;
  ItemStyle := ItemStyle - [tbisSelectable] + [tbisClicksTransparent, tbisStretch];
end;

destructor TTBXLabelItem.Destroy;
begin
  FFontSettings.Free;
  inherited;
end;

procedure TTBXLabelItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TTBXLabelItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXLabelItemViewer;
end;

procedure TTBXLabelItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings := Value;
end;

procedure TTBXLabelItem.SetMargin(Value: Integer);
begin
  FMargin := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetOrientation(Value: TTBXLabelOrientation);
begin
  FOrientation := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetShowAccelChar(Value: Boolean);
begin
  FShowAccelChar := Value;
  Change(True);
end;

procedure TTBXLabelItem.UpdateCaption(const Value: TCaption);
begin
  TTBItemAccess(Self).WriteCaption(Value);
  Change(False);
end;


//============================================================================//

{ TTBXLabelItemViewer }

procedure TTBXLabelItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  DC: HDC;
  S: WideString;
  RotatedFont, SaveFont: HFont;
begin
  Canvas.Font := TTBViewAccess(View).GetFont;
  DoAdjustFont(Canvas.Font, 0);
  S := GetCaptionText;
  if Length(S) = 0 then S := '0';
  DC := Canvas.Handle;
  if IsToolbarStyle then
  begin
    AWidth := TTBXLabelItem(Item).Margin;
    AHeight := AWidth;
    if Length(S) > 0 then
    begin
      if GetIsHoriz then
      begin
        Inc(AHeight, GetTextHeight(DC));
        Inc(AWidth, GetTextWidthW(DC, S, TTBXLabelItem(Item).ShowAccelChar));
      end
      else
      begin
        RotatedFont := CreateRotatedFont(DC);
        SaveFont := SelectObject(DC, RotatedFont);
        Inc(AWidth, GetTextHeight(DC));
        Inc(AHeight, GetTextWidthW(DC, S, TTBXLabelItem(Item).ShowAccelChar));
        SelectObject(DC, SaveFont);
        DeleteObject(RotatedFont);
      end;
    end;
  end
  else if Length(S) > 0 then
  begin
    AHeight := GetTextHeight(DC);
    AWidth := GetTextWidth(DC, S, TTBXLabelItem(Item).ShowAccelChar);
  end;

  if AWidth < 6 then AWidth := 6;
  if AHeight < 6 then AHeight := 6;
  with TTBXLabelItem(Item) do
  begin
    Inc(AWidth, Margin shl 1 + 1);
    Inc(AHeight, Margin shl 1 + 1);
  end;
end;

procedure TTBXLabelItemViewer.DoAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  if Item is TTBXLabelItem then
    with TTBXLabelItem(Item) do
    begin
      FontSettings.Apply(AFont);
      if Assigned(FOnAdjustFont) then FOnAdjustFont(Item, Self, AFont, StateFlags);
    end;
end;

function TTBXLabelItemViewer.GetIsHoriz: Boolean;
begin
  with TTBXLabelItem(Item) do
   case Orientation of
     tbxoHorizontal: Result := True;
     tbxoVertical: Result := False;
   else // tbxoAuto
     Result := View.Orientation <> tbvoVertical;
   end;
end;

function TTBXLabelItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and VT_TYPEMASK) = VT_TOOLBOX);
end;

function TTBXLabelItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and VT_TYPEMASK) = VT_TOOLBOX);
end;

procedure TTBXLabelItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  CEnabledStates: array [Boolean] of Integer = (ISF_DISABLED, 0);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
  CPrefixes: array [Boolean] of Integer = (DT_NOPREFIX, 0);
  CRotation: array [Boolean] of Integer = (DTR_0, DTR_270);
var
  Fmt: Cardinal;
  ItemInfo: TTBXItemInfo;
  R: TRect;
  Font: TFont;
  DC: HDC;
  OldFont: HFONT;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := IO_TOOLBARSTYLE or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := False;
  ItemInfo.Selected := False;
  ItemInfo.ImageShown := False;
  ItemInfo.ImageWidth := 0;
  ItemInfo.ImageHeight := 0;
  ItemInfo.HoverKind := hkNone;
  ItemInfo.IsPopupParent := False;
  ItemInfo.IsVertical := not GetIsHoriz;

  Font := TFont.Create;
  try
    Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);
    DoAdjustFont(Font, CEnabledStates[ItemInfo.Enabled]);
    Fmt := DT_SINGLELINE or DT_CENTER or DT_VCENTER or
      CPrefixes[TTBXLabelItem(Item).ShowAccelChar] or
      CRotation[ItemInfo.IsVertical];
    R := ClientAreaRect;
    DC := Canvas.Handle;
    OldFont := SelectObject(DC, Font.Handle);
    CurrentTheme.PaintCaption(DC, R, ItemInfo, GetCaptionText, Fmt);
    SelectObject(DC, OldFont);
  finally
    Font.Free;
  end;
end;


//============================================================================//

{ TTBXColorItem }

constructor TTBXColorItem.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clWhite;
end;

function TTBXColorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXColorItemViewer;
end;

procedure TTBXColorItem.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change(False);
  end;
end;


//============================================================================//

{ TTBXColorItemViewer }

procedure TTBXColorItemViewer.DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo);
begin
  with ItemInfo, Canvas do if Enabled then
  begin
    if ((ItemOptions and IO_TOOLBARSTYLE) = 0) then InflateRect(ARect, -2, -2);
    if TTBXColorItem(Item).Color <> clNone then
    begin
      Brush.Color := clBtnShadow;
      FrameRect(ARect);
      InflateRect(ARect, -1, -1);
      Brush.Color := TTBXColorItem(Item).Color;
      FillRect(ARect);
    end;
  end
  else
  begin
    Inc(ARect.Right);
    Inc(ARect.Bottom);
    DrawEdge(Handle, ARect, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_RECT);
  end;
end;

procedure TTBXColorItemViewer.DoPaintCaption(Canvas: TCanvas;
  const ClientAreaRect: TRect; var CaptionRect: TRect;
  IsTextRotated: Boolean; var PaintDefault: Boolean);
begin
  if (GetViewType(View) and VT_TYPEMASK) = VT_TOOLBOX then
  begin
    { Center Caption }
    OffsetRect(CaptionRect, -CaptionRect.Left, 0);
    OffsetRect(CaptionRect, (ClientAreaRect.Right - CaptionRect.Right) div 2, 0);
  end;
end;

function TTBXColorItemViewer.GetImageSize: TSize;
begin
  if IsToolbarStyle then
  begin
    Result.CX := 12;
    Result.CY := 12;
  end
  else
  begin
    Result.CX := 16;
    Result.CY := 16;
  end;
end;

function TTBXColorItemViewer.GetImageShown: Boolean;
begin
  Result := ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));
end;

constructor TTBXColorItemViewer.Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
begin
  inherited;
  Wide := False;
end;

//============================================================================//

{ TTBXMRUList }

function TTBXMRUList.GetFirstKey:Integer;
begin
  Result := FKeyShift;
end;

function TTBXMRUList.GetItemClass: TTBCustomItemClass;
begin
  Result := TTBXCustomItem;
end;

procedure TTBXMRUList.SetItemCaptions;
var
  I: Integer;
begin
  inherited;
  if Container is TTBXCustomItem then
    for I := 0 to Items.Count - 1 do
      TTBXCustomItem(Items[I]).FontSettings := TTBXCustomItem(Container).FontSettings;
end;

procedure TTBXMRUList.SetKeyShift(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FKeyShift := Value;
  SetItemCaptions;
end;


//============================================================================//

{ TTBXMRUListItem }

constructor TTBXMRUListItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := STBMRUListItemDefCaption[1] + 'TBX ' +
    Copy(STBMRUListItemDefCaption, 2, Length(STBMRUListItemDefCaption) - 1);
end;

procedure TTBXMRUListItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMRUList) and (Operation = opRemove) then MRUList := nil;
end;

procedure TTBXMRUListItem.SetMRUList(Value: TTBMRUList);
begin
  if FMRUList <> Value then
  begin
    FMRUList := Value;
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
      LinkSubitems := TTBMRUListAccess(Value).Container;
    end
    else LinkSubitems := nil;
  end;
end;

{ TTBXCustomSpinEditItem }

function TTBXCustomSpinEditItem.CheckValue(const V: Extended): Extended;
begin
  Result := V;
  if FMaxValue <> FMinValue then
  begin
    if V < FMinValue then Result := FMinValue
    else if V > FMaxValue then Result := FMaxValue;
  end;
end;

procedure TTBXCustomSpinEditItem.ClickDown;
var
  OldValue, NewValue: Extended;
begin
  OldValue := GetValue;
  if Snap then
    NewValue := Ceil(OldValue / Increment - 1 - Increment * 0.00001) * Increment
  else
    NewValue := OldValue - FIncrement;
  DoStep(-1, OldValue, NewValue);
  SetValue(NewValue);
end;

procedure TTBXCustomSpinEditItem.ClickUp;
var
  OldValue, NewValue: Extended;
begin
  OldValue := GetValue;
  if Snap then
    NewValue := Floor(OldValue / Increment + 1 + Increment * 0.00001) * Increment
  else
    NewValue := OldValue + FIncrement;
  DoStep(+1, OldValue, NewValue);
  SetValue(NewValue);
end;

constructor TTBXCustomSpinEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taRightJustify;
  FDecimal := 2;
  FIncrement := 1;
  FSnap := True;
  Text := '0';
end;

function TTBXCustomSpinEditItem.DoAcceptText(var NewText: WideString): Boolean;
var
  V: Extended;
begin
  if ParseValue(NewText, V) then
  begin
    NewText := GetAsText(CheckValue(V));
    Result := True;
  end
  else Result := False;
end;

function TTBXCustomSpinEditItem.DoConvert(const APrefix, APostfix: WideString; var AValue: Extended): Boolean;
begin
  Result := True;
  if Assigned(FOnConvert) then FOnConvert(Self, APrefix, APostfix, AValue, Result);
end;

procedure TTBXCustomSpinEditItem.DoStep(Step: Integer; const OldValue: Extended; var NewValue: Extended);
begin
  if Assigned(FOnStep) then FOnStep(Self, Step, OldValue, NewValue);
end;

procedure TTBXCustomSpinEditItem.DoTextChanged;
begin
  if not FWriting then SetValue(GetValue);
end;

function TTBXCustomSpinEditItem.DoTextToValue(const AText: WideString; out AValue: Extended): Boolean;
begin
  Result := False;
  if Assigned(FOnTextToValue) then FOnTextToValue(Self, AText, AValue, Result);
end;

procedure TTBXCustomSpinEditItem.DoValueToText(const NewValue: Extended; var NewText: WideString);
begin
  if Assigned(FOnValueToText) then FOnValueToText(Self, NewValue, NewText);
end;

function TTBXCustomSpinEditItem.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TTBXCustomSpinEditItem.GetAsText(AValue: Extended): WideString;
begin
  AValue := CheckValue(AValue);
  if ValueType = evtFloat then Result := FloatToStrF(AValue, ffFixed, 15, FDecimal)
  else if ValueType = evtHex then Result := IntToHex(Round(AValue), 1)
  else Result := IntToStr(Round(AValue));

  if Length(Prefix) > 0 then
  begin
    if SpaceAfterPrefix then Result := ' ' + Result;
    Result := Prefix + Result;
  end;
  if Length(Postfix) > 0 then
  begin
    if SpaceBeforePostfix then Result := Result + ' ';
    Result := Result + Postfix;
  end;
  DoValueToText(AValue, Result);
end;

function TTBXCustomSpinEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXSpinEditViewer;
end;

function TTBXCustomSpinEditItem.GetValue: Extended;
begin
  if not ParseValue(Text, Result) then
    Result := FLastGoodValue;
end;

function TTBXCustomSpinEditItem.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1;
end;

function TTBXCustomSpinEditItem.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TTBXCustomSpinEditItem.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

function TTBXCustomSpinEditItem.IsValueStored: Boolean;
begin
  Result := GetValue <> 0;
end;

function TTBXCustomSpinEditItem.ParseValue(const S: WideString; out V: Extended): Boolean;
const
  CWhiteSpace = [' ', #9];
  CDigits = ['0'..'9'];
  CHexDigits = CDigits + ['A'..'F'];
  CInvalidUnitChars = [#0..#31, ' ', '*', '+', ',', '-', '.', '/', '0'..'9', '^'];
  CInvalidHexUnitChars = CInvalidUnitChars + ['A'..'F'];
var
  P: PWideChar;
  Sign1: Integer;
  Value1: Extended;
  Value2: Extended;
  Op: Char;
  PrefixString, PostfixString: WideString;

  procedure SkipWhiteSpace;
  begin
    while (P^ = ' ') or (P^ = #9) do Inc(P);
  end;

  function IsDigit: Boolean;
  begin
    Result := (P^ >= '0') and (P^ <= '9');
  end;

  function IsHexDigit: Boolean;
  begin
    Result := (P^ < #255) and (Char(P^) in ['0'..'9', 'a'..'f', 'A'..'F']);
  end;

  function GetInt: Integer;
  begin
    Result := 0;
    while (P^ >= '0') and (P^ <= '9') do
    begin
      Result := Result * 10 + (Integer(P^) - Integer('0'));
      Inc(P);
    end;
  end;

  function GetInt2: Extended;
  begin
    Result := 0;
    while (P^ >= '0') and (P^ <= '9') do
    begin
      Result := Result * 10 + (Integer(P^) - Integer('0'));
      Inc(P);
    end;
  end;

  function GetNumber(out PrefixString, PostfixString: WideString; out R: Extended): Boolean;
  var
    PStart: PWideChar;
    Tmp: Integer;
    ExponentSign, IR: Integer;
    Count1, Count2: Integer;
    E: Extended;
  begin
    R := 0;
    Result := False;

    { Read prefix }
    PStart := P;
    if ValueType <> evtHex then
      while (P^ <= #255) and not (Char(P^) in CInvalidUnitChars) do Inc(P)
    else
      while (P^ <= #255) and not (Char(P^) in CInvalidHexUnitChars) do Inc(P);
    SetString(PrefixString, PStart, P - PStart);
    SkipWhiteSpace;

    { Read value }
    if ValueType in [evtFloat, evtInteger] then
    begin
      if (ValueType = evtInteger) and not IsDigit then Exit;

      { get the integer part }
      PStart := P;
      R := GetInt2;
      Count1 := P - PStart;

      if (ValueType = evtFloat) and (P^ = WideChar(DecimalSeparator)) then
      begin
        Inc(P);
        PStart := P;
        E := GetInt2;
        R := R + E / IntPower(10, P - PStart);
        Count2 := P - PStart;
      end
      else Count2 := 0;

      if (Count1 = 0) and (Count2 = 0) then Exit; // '.' (or ',') is not a number

      if (ValueType = evtFloat) and ((P^ = 'e') or (P^ = 'E')) and
        (PWideChar(P + 1)^ <= #255) and (Char(PWideChar(P + 1)^) in ['+', '-', '0'..'9']) then
      begin
        Inc(P);
        ExponentSign := 1;
        if P^ = '-' then
        begin
          ExponentSign := -1;
          Inc(P);
        end
        else if P^ = '+' then Inc(P);
        if not IsDigit then Exit;
        Tmp := GetInt;
        if Tmp >= 5000 then Exit;
        R := R * IntPower(10, Tmp * ExponentSign);
      end;
    end
    else { evtHex }
    begin
      IR := 0;
      if not IsHexDigit then Exit;
      while IsHexDigit do
      begin
        IR := IR shl 4;
        if (P^ >= '0') and (P^ <= '9') then Inc(IR, Integer(P^) - Integer('0'))
        else if (P^ >= 'a') and (P^ <= 'f') then Inc(IR, Integer(P^) - Integer('a') + 10)
        else Inc(IR, Integer(P^) - Integer('A') + 10);
        Inc(P);
      end;
      R := IR;
    end;
    SkipWhiteSpace;

    { Read postfix }
    PStart := P;
    if ValueType <> evtHex then
      while (P^ <= #255) and not (Char(P^) in CInvalidUnitChars) do Inc(P)
    else
      while (P^ <= #255) and not (Char(P^) in CInvalidHexUnitChars) do Inc(P);
    SetString(PostfixString, PStart, P - PStart);
    SkipWhiteSpace;

    Result := True;
  end;

begin
  V := 0;

  { Try text-to-value conversion for predefined "constants" }
  Result := DoTextToValue(S, V);
  if Result then Exit;

  { Parse the string for values and expressions }
  if Length(S) = 0 then Exit;
  P := PWideChar(S);
  SkipWhiteSpace;

  { Read the sign }
  Sign1 := 1;
  if P^ = '-' then
  begin
    Sign1 := -1;
    Inc(P);
    SkipWhiteSpace;
  end
  else if P^ = '+' then
  begin
    Inc(P);
    SkipWhiteSpace;
  end;

  { Read value }
  if not GetNumber(PrefixString, PostfixString, Value1) then Exit;
  if not DoConvert(PrefixString, PostfixString, Value1) then Exit;
  Value1 := Value1 * Sign1;
  V := Value1;

  { Read operator }
  if (P^ < #255) and (Char(P^) in ['*', '+', '-', '/']) then
  begin
    Op := Char(P^);
    Inc(P);
    SkipWhiteSpace;
    if not GetNumber(PrefixString, PostfixString, Value2) then Exit;
    if not DoConvert(PrefixString, PostfixString, Value2) then Exit;
    case Op of
      '*': V := V * Value2;
      '+': V := V + Value2;
      '-': V := V - Value2;
      '/': if Value2 <> 0 then V := V / Value2 else Exit;
    end;
  end;

  if P^ = #0 then Result := True;
end;

procedure TTBXCustomSpinEditItem.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TTBXCustomSpinEditItem.SetDecimal(NewDecimal: TDecimal);
begin
  if NewDecimal <> FDecimal then
  begin
    FDecimal := NewDecimal;
    SetValue(GetValue);
  end;
end;

procedure TTBXCustomSpinEditItem.SetIncrement(const NewIncrement: Extended);
begin
  if NewIncrement <= 0 then
    raise EPropertyError.Create('Increment must be a positive value');
  FIncrement := NewIncrement;
end;

procedure TTBXCustomSpinEditItem.SetPostfix(const NewPostfix: WideString);
begin
  if not ValidateUnits(NewPostfix) then
    raise EPropertyError.Create('Invalid postfix');
  FPostfix := NewPostfix;
  WriteValue(GetValue);
end;

procedure TTBXCustomSpinEditItem.SetPrefix(const NewPrefix: WideString);
begin
  if not ValidateUnits(NewPrefix) then
    raise EPropertyError.Create('Invalid prefix');
  FPrefix := NewPrefix;
  WriteValue(GetValue);
end;

procedure TTBXCustomSpinEditItem.SetSpaceAfterPrefix(UseSpace: Boolean);
begin
  FSpaceAfterPrefix := UseSpace;
  WriteValue(GetValue);
end;

procedure TTBXCustomSpinEditItem.SetSpaceBeforePostfix(UseSpace: Boolean);
begin
  FSpaceBeforePostfix := UseSpace;
  WriteValue(GetValue);
end;

procedure TTBXCustomSpinEditItem.SetValue(NewValue: Extended);
begin
  Text := GetAsText(NewValue);
  FLastGoodValue := NewValue;
end;

procedure TTBXCustomSpinEditItem.SetValueType(NewType: TSEValueType);
var
  V: Extended;
begin
  if NewType <> FValueType then
  begin
    V := GetValue;
    FValueType := NewType;
    SetValue(V);
    if NewType in [evtInteger, evtHex] then FIncrement := Max(Round(FIncrement), 1);
  end;
end;

function TTBXCustomSpinEditItem.ValidateUnits(const S: WideString): Boolean;
const
  InvalidChars = [#0..#31, ' ', '*', '+', ',', '-', '.', '/', '0'..'9', '^'];
var
  I: Integer;
begin
  Result := False;
  if Length(S) > 0 then
    for I := 1 to Length(S) do
     if (S[I] <= #255) and (Char(S[I]) in InvalidChars) then Exit;
  Result := True;
end;

procedure TTBXCustomSpinEditItem.WriteValue(NewValue: Extended);
begin
  WriteText(GetAsText(NewValue));
  FLastGoodValue := NewValue;
end;

{ TTBXSpinEditViewer }

destructor TTBXSpinEditViewer.Destroy;
begin
  FBtnTimer.Free;
  inherited;
end;

procedure TTBXSpinEditViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
const
  CDisabled: array [Boolean] of Integer = (EBSS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBSS_HOT);
  CUpDnState: array [TSEBtnState] of Integer = (0, EBSS_UP, EBSS_DOWN);
begin
  inherited GetEditInfo(EditInfo, ItemInfo);
  EditInfo.RightBtnInfo.ButtonType := EBT_SPIN;
  EditInfo.RightBtnInfo.ButtonState := CDisabled[ItemInfo.Enabled] or
    CHot[ItemInfo.HoverKind = hkMouseHover] or CUpDnState[FBtnState];
end;

function TTBXSpinEditViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then Result := CurrentTheme.EditBtnWidth  + 2
  else Result := GetSystemMetrics(SM_CXMENUCHECK) + 2;
end;

function TTBXSpinEditViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_KEYDOWN then
    case TWMKeyDown(Message).CharCode of
      VK_UP:
        begin
          TTBXCustomSpinEditItem(Item).ClickUp;
          SendMessageW(EditControlHandle, EM_SETSEL, 0, -1);
          Result := True;
          Exit;
        end;
      VK_DOWN:
        begin
          TTBXCustomSpinEditItem(Item).ClickDown;
          SendMessageW(EditControlHandle, EM_SETSEL, 0, -1);
          Result := True;
          Exit;
        end;
    end;

  Result := inherited HandleEditMessage(Message);
end;

procedure TTBXSpinEditViewer.InvalidateButtons;
var
  R: TRect;
begin
  with TTBXSpinEditItem(Item) do
    if Show and not IsRectEmpty(BoundsRect) then
      begin
        R := BoundsRect;
        R.Left := R.Right - GetIndentAfter;
        InvalidateRect(View.Window.Handle, @R, False);
        Include(State, tbisInvalidated);
      end;
end;

function TTBXSpinEditViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := X <= (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TTBXSpinEditViewer.LosingCapture;
begin
  FBtnTimer.Free;
  FBtnTimer := nil;
  inherited;
end;

procedure TTBXSpinEditViewer.MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean);
begin
  if not Item.Enabled then Exit;
  FBtnState := ebsNone;
  if X >= BoundsRect.Right - BoundsRect.Left - GetIndentAfter then
  begin
    if Y < (BoundsRect.Bottom - BoundsRect.Top) div 2 then
    begin
      FBtnState := ebsUp;
      TTBXCustomSpinEditItem(Item).ClickUp;
    end
    else
    begin
      FBtnState := ebsDown;
      TTBXCustomSpinEditItem(Item).ClickDown;
    end;

    if FBtnTimer = nil then
    begin
      FBtnTimer := TTimer.Create(nil);
      FBtnTimer.OnTimer := TimerHandler;
    end;
    FBtnTimer.Interval := SE_FIRSTINTERVAL;
    FBtnTimer.Enabled := True;
  end;

  if FBtnState <> ebsNone then
  begin
    InvalidateButtons;
    inherited;
    View.SetCapture;
  end
  else inherited;
end;

procedure TTBXSpinEditViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  if FBtnState <> ebsNone then
  begin
    FBtnState := ebsNone;
    FBtnTimer.Free;
    FBtnTimer := nil;
    InvalidateButtons;
  end;
  inherited;
end;

procedure TTBXSpinEditViewer.TimerHandler(Sender: TObject);
begin
  FBtnTimer.Interval := SE_INTERVAL;
  if FBtnState = ebsUp then TTBXSpinEditItem(Item).ClickUp
  else if FBtnState = ebsDown then TTBXSpinEditItem(Item).ClickDown
  else
  begin
    FBtnTimer.Free;
    FBtnTimer := nil;
  end;
end;

end.
