
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BtnEdit;

interface

{$I STD.INC}

uses
  Windows, Messages, Controls, StdCtrls, Classes, Graphics, Forms, ImgList,
  GraphTools, SysUtils, PopCtrls;

{ TCustomButtonEdit class }

type
  TButtonEditStyle = (beStandard, beClose, beEllipse, beQuestion, beOwnerDrawn);

  TOwnerDrawEvent = procedure (Control: TWinControl; Rect: TRect;
    State: TOwnerDrawState; var DefaultDraw: Boolean) of object;
  TSearchEvent = procedure (Sender: TObject; Index: Integer) of object;

  TCustomButtonEdit = class(TCustomControl)
  private
  	FAutoHeight: Boolean;
    FBorderStyle: TBorderStyle;
    FButtonDown: Boolean;
    FButtonHot: Boolean;
    FButtonVisible: Boolean;
    FTextChanged: Boolean;
    FChangeLink: TChangeLink;
    FDefEditProc: TWndMethod;
    FEdit: TEdit;
    FEditRect: TRect;
    FFlat: Boolean;
    FFocused: Boolean;
    FGlyph: TBitmap;
    FGlyphAssigned: Boolean;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FNextControl: TWinControl;
    FTitle: string;
    FSearchStrings: TStrings;
    FStyle: TButtonEditStyle;
    FOnButtonClick: TNotifyEvent;
    FOnButtonPress: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnCustomDraw: TOwnerDrawEvent;
    FOnPlaceEdit: TCalculateRectEvent;
    FOnSearch: TSearchEvent;
    FOnTab: TNotifyEvent;
    FWantTabs: Boolean;
    procedure AdjustEdit;
    procedure AdjustHeight;
    procedure EditChange(Sender: TObject);
    procedure EditProc(var Message: TMessage);
    procedure ImagesChange(Sender: TObject);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButtonVisible(Value: Boolean);
    function GetEditHandle: HWND;
    procedure SetFlat(Value: Boolean);
		procedure AssignGlyph;
    function  GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    procedure SetImageIndex(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetStyle(Value: TButtonEditStyle);
    { TCustomButtonEdit.TEdit  }
    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(Value: Boolean);
    function GetCanUndo: Boolean;
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(Value: TEditCharCase);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(Value: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(Value: Integer);
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    function GetOEMConvert: Boolean;
    procedure SetOEMConvert(Value: Boolean);
    function GetPasswordChar: Char;
    procedure SetPasswordChar(Value: Char);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetSelLength: Integer;
    procedure SetSelLength(Value: Integer);
    function GetSelStart: Integer;
    procedure SetSelStart(Value: Integer);
    function GetSelText: string;
    procedure SetSelText(Value: string);
    function GetTabStop: Boolean;
    procedure SetTabStop(Value: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure AdjustSize; override;
    procedure DoButtonPress; dynamic;
    procedure DoButtonClick; dynamic;
		procedure DoPlaceEdit(var Rect: TRect); dynamic;
    procedure DrawButtonGlyph(Rect: TRect); virtual;
    procedure ParseInput; virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    function GetButtonRect: TRect; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ButtonDown: Boolean read FButtonDown;
    property ButtonHot: Boolean read FButtonHot;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored FGlyphAssigned;
    property TextChanged: Boolean read FTextChanged;
    property EditHandle: HWND read GetEditHandle;
    property Flat: Boolean read FFlat write SetFlat;
    property NextControl: TWinControl read FNextControl;
    property WantTabs: Boolean read FWantTabs write SetWantTabs;
    property SearchStrings: TStrings read FSearchStrings write FSearchStrings;
    property Style: TButtonEditStyle read FStyle write SetStyle;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property Title: string read FTitle write FTitle;
    property OnCustomDraw: TOwnerDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnButtonPress: TNotifyEvent read FOnButtonPress write FOnButtonPress;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnPlaceEdit: TCalculateRectEvent read FOnPlaceEdit write FOnPlaceEdit;
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnTab: TNotifyEvent read FOnTab write FOnTab;
    { TCustomButtonEdit.TEdit }
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property CanUndo: Boolean read GetCanUndo;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default True;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property Modified: Boolean read GetModified write SetModified;
    property OEMConvert: Boolean read GetOEMConvert write SetOEMConvert default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateNextControl;
    { TCustomButtonEdit.TEdit }
    procedure Clear;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure ClearUndo;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    procedure SelectAll;
    procedure SetSelTextBuf(Buffer: PChar);
  end;

{ TButtonEdit }

  TButtonEdit = class(TCustomButtonEdit)
  public
    property ButtonDown;
    property Canvas;
    property NextControl;
    property SearchStrings;
    property TextChanged;
  published
  	property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property Flat;
    property Glyph;
    property HideSelection;
    property Images;
    property ImageIndex;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Text;
    property Visible;
    property WantTabs;
    property OnButtonPress;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnStartDock;
    property OnStartDrag;
    property OnTab;
  end;

{ TPopupEdit }

  TPopupEdit = class(TCustomButtonEdit)
  private
    FPopupForm: TCustomPopupForm;
    FOnPopup: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    function GetSizeable: Boolean;
    procedure SetSizeable(const Value: Boolean);
  protected
    procedure DoButtonClick; override;
    procedure RequestPopup;
    function CreatePopup: TCustomPopupForm; virtual; abstract;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoPopup; virtual;
    procedure DoCancel(Sender: TObject); virtual;
    procedure DoSelect(Sender: TObject); virtual;
    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  public
    procedure Popup;
    procedure Select;
    procedure Cancel;
  end;

{ TDatePopupEdit }

  TDatePopupEdit = class(TPopupEdit)
  private
  	FPopupDateForm: TPopupDateForm;
  	function GetDate: TDate;
    procedure SetDate(const Value: TDate);
  protected
    function CreatePopup: TCustomPopupForm; override;
		procedure DoSelect(Sender: TObject); override;
		procedure ParseInput; override;
  public
    constructor Create(AOwner: TComponent); override;
  	property Date: TDate read GetDate write SetDate;
  end;

{ TListEdit }

  TListEdit = class(TPopupEdit)
  private
    FPopupListBox: TPopupListForm;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoSelect(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SearchStrings;
  published
  	property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Ctl3D;
    property Font;
    property HideSelection;
    property Images;
    property ImageIndex;
    property Items: TStrings read GetItems write SetItems;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property Sizeable;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonPress;
    property OnButtonClick;
    property OnCancel;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TImageListEdit }

  TImageListEdit = class(TPopupEdit)
  private
  	FDisplayCount: Integer;
  	FPopupImageList: TPopupImageListForm;
    procedure ImageClick(Sender: TObject);
    procedure ImageSelectItem(Sender: TObject);
  	function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
  	function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
	protected
    procedure CreateWnd; override;
		function CreatePopup: TCustomPopupForm; override;
    procedure DoPopup; override;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  public
  	constructor Create(AOwner: TComponent); override;
  published
  	property Enabled;
  	property DisplayCount: Integer read FDisplayCount write FDisplayCount;
  	property ImageIndex: Integer read GetImageIndex write SetImageIndex;
  	property Images: TCustomImageList read GetImages write SetImages;
    property TabOrder;
  	property TabStop;
    property Visible;
    property OnChange;
    property OnEnter;
    property OnExit;
  end;

{ TCheckListEdit }

  TCheckListEdit = class(TPopupEdit)
  private
    FPopupCheckList: TPopupCheckListForm;
    FOnClickCheck: TNotifyEvent;
    FOnClickItem: TNotifyEvent;
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; Value: Boolean);
    function GetCheckText: string;
    function GetFlat: Boolean;
    procedure SetFlat(Value: Boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    function GetSizeable: Boolean;
    procedure SetSizeable(Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; Value: TCheckBoxState);
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoClickCheck(Sender: TObject);
    procedure DoClickItem(Sender: TObject);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CheckText: string read GetCheckText;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SearchStrings;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
  	property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Ctl3D;
    property Flat: Boolean read GetFlat write SetFlat;
    property Font;
    property HideSelection;
    property Images;
    property ImageIndex;
    property Items: TStrings read GetItems write SetItems;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property StatusText: string read GetStatusText write SetStatusText;
    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonPress;
    property OnButtonClick;
    property OnCancel;
    property OnChange;
    property OnClick;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnClickItem: TNotifyEvent read FOnClickItem write FOnClickItem;
    property OnContextPopup;
    property OnCustomDraw;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TImageEditItem }

  TImageEditItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FIndent: Integer;
    FSelectedIndex: Integer;
    FText: string;
    procedure SetImageIndex(const Value: Integer);
    procedure SetIndent(const Value: Integer);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetText(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Indent: Integer read FIndent write SetIndent;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Text: string read FText write SetText;
  end;

{ TImageEditItems }

  TImageEditItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TImageEditItem;
    procedure Put(Index: Integer; const Value: TImageEditItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TImageEditItem;
    function FindItemID(ID: Integer): TImageEditItem;
    function Insert(Index: Integer): TImageEditItem;
    property Items[Index: Integer]: TImageEditItem read Get write Put;
  end;

{ TImageEdit }

  TImageEdit = class(TCustomButtonEdit)
  private
    FImageList: TImageList;
    FItems: TImageEditItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

type
  TPopupHack = class(TCustomPopupForm);

{ TCustomButtonEdit }

constructor TCustomButtonEdit.Create(AOwner: TComponent);
var
	DC: HDC;
  Ratio: Single;
begin
  inherited Create(AOwner);
	FAutoHeight := True;
  FBorderStyle := bsSingle;
  FButtonVisible := True;
  FImageIndex := -1;
  Color := clWindow;
  Canvas.Brush.Color := Color;
  ParentColor := False;
  inherited TabStop := False;
	DC := GetDC(GetDesktopWindow);
  Ratio := GetDeviceCaps(DC, LOGPIXELSX) / 96;
  Height := Round(24 * Ratio);
  Width := Round(121 * Ratio);
  ReleaseDC(GetDesktopWindow, DC);
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Parent := Self;
    AutoSelect := False;
    Left := 1;
    Top := 1;
    Anchors := [akLeft, akTop, akRight];
    BorderStyle := bsNone;
    ParentColor := True;
    ParentFont := True;
    FDefEditProc := WindowProc;
    WindowProc := EditProc;
    OnChange := EditChange;
    OnKeyDown := EditKeyDown;
  end;
  AdjustHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
end;

destructor TCustomButtonEdit.Destroy;
begin
  FEdit.WindowProc := FDefEditProc;
  Images := nil;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TCustomButtonEdit.AdjustEdit;
var
	I: Integer;
begin
	if not HandleAllocated then Exit;
  FEditRect := BoundsRect;
  if BorderStyle = bsSingle then
    InflateRect(FEditRect, -GetSystemMetrics(SM_CXEDGE) div 2,
      -GetSystemMetrics(SM_CXEDGE) div 2);
  OffsetRect(FEditRect, -FEditRect.Left, -FEditRect.Top);
  with FEditRect do
  begin
	  {if FBorderStyle = bsSingle then
    begin
  	  Inc(Top, 3);
	    Inc(Left, 3);
	    Dec(Bottom, 3);
    end
    else}
    begin
      Canvas.Font := Font;
      I := Canvas.TextHeight('Wg');
      Top := (ClientHeight - I) div 2;
      Bottom := Top + I;
    end;
    if FBorderStyle = bsSingle then
    begin
	    Inc(Left, 3);
  	  Dec(Top);
	    Dec(Bottom);
    end;
    Dec(Right, 2);
    if FImages <> nil then
      Inc(Left, FImages.Width + 4);
    if FButtonVisible then
      Dec(Right, GetSystemMetrics(SM_CXVSCROLL) - 1);
    DoPlaceEdit(FEditRect);
    FEdit.SetBounds(FEditRect.Left, FEditRect.Top, FEditRect.Right - Left, FEditRect.Bottom);
  end;
end;

procedure TCustomButtonEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
	if FAutoHeight then
	begin
    DC := GetDC(0);
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    if (FImages <> nil) and (FImages.Height - 3 > Metrics.tmHeight) then
      Metrics.tmHeight := FImages.Height;
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 1;
    Height := Metrics.tmHeight + I;
  end;
  AdjustEdit;
end;

procedure TCustomButtonEdit.AdjustSize;
begin
	inherited AdjustSize;
  AdjustHeight;
end;

procedure TCustomButtonEdit.DoButtonPress;
begin
  if Assigned(FOnButtonPress) then
    FOnButtonPress(Self);
end;

procedure TCustomButtonEdit.DoButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

procedure TCustomButtonEdit.DoPlaceEdit(var Rect: TRect);
begin
	if Assigned(FOnPlaceEdit) then
		FOnPlaceEdit(Self, Rect);
end;

procedure TCustomButtonEdit.DrawButtonGlyph(Rect: TRect);
var
  DefaultDraw: Boolean;
  OwnerDrawState: TOwnerDrawState;
  State: TDrawState;
  X, Y: Integer;
begin
  DefaultDraw := True;
  if csDesigning in ComponentState then
    State := []
  else if not Enabled then
    State := [dsDisabled]
  else if FButtonDown then
    State := [dsPressed]
  else if FButtonHot then
    State := [dsHot]
  else
    State := [];
  if FStyle <> beStandard then
    DrawThemeScroll(Canvas.Handle, drCenter, Rect, State);
  if Assigned(FOnCustomDraw)then
  begin
    OwnerDrawState := [];
    if FFocused or Focused then
      Include(OwnerDrawState, odFocused);
    FOnCustomDraw(Self, Rect, OwnerDrawState, DefaultDraw);
    Include(OwnerDrawState, odComboBoxEdit);
    FOnCustomDraw(Self, FEditRect, OwnerDrawState, DefaultDraw);
  end;
  if DefaultDraw then
  	if FGlyph <> nil then
    begin
	    DrawThemeScroll(Canvas.Handle, drCenter, Rect, State);
      AssignGlyph;
      X := Rect.Left + (WidthOf(Rect) - FGlyph.Width) div 2 + 1;
      Y := Rect.Top + (HeightOf(Rect) - FGlyph.Height) div 2;
      if (not ThemePainter.Enabled) and (dsPressed in State) then
      begin
      	Inc(X);
        Inc(Y);
      end;
      Canvas.Draw(X, Y, FGlyph);
    end
    else if FStyle = beStandard then
      DrawThemeScroll(Canvas.Handle, drDown, Rect, State)
    else
    begin
      if (not ThemePainter.Enabled) and FButtonDown then
        OffsetRect(Rect, 1, 1);
      case FStyle of
        beClose: GlyphDraw(Canvas.Handle, Rect, gkClose, clWindowFrame);
        beEllipse: GlyphDraw(Canvas.Handle, Rect, gkEllipse, clWindowFrame);
        beQuestion: GlyphDraw(Canvas.Handle, Rect, gkQuestion, clWindowFrame);
      end;
    end;
end;

procedure TCustomButtonEdit.ParseInput;
begin
end;

procedure TCustomButtonEdit.EditChange(Sender: TObject);
begin
  FTextChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomButtonEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (ssAlt in Shift) then
    DoButtonClick;
end;

procedure TCustomButtonEdit.EditProc(var Message: TMessage);

   procedure SelectNextControl;
   var
     Shift: Boolean;
     Control: TWinControl;
   begin
     Shift := GetKeyState(VK_SHIFT) > -1;
     Control := FEdit;
     while Control.Parent <> nil do
       Control := Control.Parent;
     TCustomButtonEdit(Control).SelectNext(FEdit, Shift, True);
   end;

var
  Point: TPoint;
  PriorLParam: Longint;
begin
  with Message do
    case Msg of
      WM_CANCELMODE, CM_CANCELMODE, WM_KILLFOCUS, WM_SETFOCUS,
      WM_KEYFIRST..WM_KEYLAST:
				begin
        	case Msg of
          	WM_KILLFOCUS, WM_SETFOCUS:
            	begin
								FFocused := Msg = WM_SETFOCUS;
                if FFocused then
                begin
									FTextChanged := False;
                  AdjustHeight;
                end
								else
                	ParseInput;
								InvalidateRect(Handle, nil, False);
							end;
						WM_CHAR:
                if Message.WParam = 9 then
                begin
                  if FWantTabs then
                  begin
                    UpdateNextControl;
                    if Assigned(FOnTab) then
                      FOnTab(Self);
                  end
                  else
                    SelectNextControl;
                  Exit;
                end
                else if (FSearchStrings <> nil) and (Message.WParam > 31) then
                  SetTimer(Handle, 0, 10, nil);
              WM_KEYDOWN:
                if Message.WParam = VK_TAB then
                  Exit;
           end;
           WindowProc(Message);
         end;
      WM_GETDLGCODE:
        begin
          FDefEditProc(Message);
          Result := Result or DLGC_WANTTAB;
          Exit;
        end;
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          Point := SmallPointToPoint(TSmallPoint(LParam));
          MapWindowPoints(FEdit.Handle, Handle, Point, 1);
          PriorLParam := LParam;
          TSmallPoint(LParam) := PointToSmallPoint(Point);
          WindowProc(Message);
          LParam := PriorLParam;
        end;
    end;
  FDefEditProc(Message);
end;

procedure TCustomButtonEdit.ImagesChange(Sender: TObject);
begin
  InvalidateRect(Handle, nil, False);
  AdjustHeight;
  AdjustEdit;
end;

function TCustomButtonEdit.GetButtonRect: TRect;
var
	I: Integer;
begin
	Result := Rect(0, 0, Width, Height);
  if FBorderStyle = bsSingle then
		InflateRect(Result, -GetBorder, -GetBorder);
	I := Result.Left;
  Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
  if ThemePainter.Enabled then Dec(Result.Left);
  if Result.Left < I then
  	Result.Left := I;
end;

procedure TCustomButtonEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ButtonRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then Exit;
  if Button in [mbLeft, mbRight] then
  begin
    if CanFocus then
      if FEdit.Visible then
        FEdit.SetFocus
      else
        SetFocus;
    if FButtonVisible and (Button = mbLeft) then
    begin
      ButtonRect := GetButtonRect;
      FButtonDown := PtInRect(ButtonRect, Point(X, Y));
      if FButtonDown then
      begin
        InvalidateRect(Handle, @ButtonRect, False);
        UpdateWindow(Handle);
        DoButtonPress;
      end;
    end;
  end;
end;

procedure TCustomButtonEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonRect: TRect;
  Hot: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  ButtonRect := GetButtonRect;
  Hot := PtInRect(ButtonRect, Point(X, Y));
  if Hot <> FButtonHot then
  begin
  	FButtonHot := Hot;
    InvalidateRect(Handle, @ButtonRect, False);
  end;
end;

procedure TCustomButtonEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ButtonRect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FButtonDown and (Button = mbLeft) then
  begin
    FButtonDown := False;
    ButtonRect := GetButtonRect;
    InvalidateRect(Handle, @ButtonRect, False);
    UpdateWindow(Handle);
    if PtInRect(ButtonRect, Point(X, Y)) then
      DoButtonClick;
  end;
end;

procedure TCustomButtonEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    SetImages(nil);
end;

procedure TCustomButtonEdit.Paint;
var
	DC: HDC;
  Brush: HBRUSH;
begin
	DC := Canvas.Handle;
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect(0, 0, Width, Height), Brush);
  DeleteObject(Brush);
  if FBorderStyle = bsSingle then
    if Flat then
    	DrawThemeBorder(DC, Color, Rect(0, 0, Width, Height), [dsThin])
    else
    	DrawThemeBorder(DC, Color, Rect(0, 0, Width, Height), []);
  if FImages <> nil then
  	FImages.Draw(Canvas, 3, Round((Height - FImages.Height) / 2), FImageIndex, Enabled);
	if FButtonVisible then
		DrawButtonGlyph(GetButtonRect);
end;

procedure TCustomButtonEdit.WndProc(var Message: TMessage);
begin
  if HandleAllocated then
    with Message do
      case Msg of
        WM_CHAR:
          if Message.WParam = 9 then
          begin
            Message.Result := 0;
            Exit;
          end;
        WM_GETTEXT, WM_GETTEXTLENGTH, WM_SETTEXT, EM_GETSEL..EM_GETIMESTATUS:
          Result := SendMessage(FEdit.Handle, Msg, wParam, LParam);
        WM_SETFOCUS:
          begin
            inherited WndProc(Message);
            if FEdit.Visible then
              FEdit.SetFocus;
          end;
      else
        inherited WndProc(Message);
      end
  else
    inherited WndProc(Message);
end;

procedure TCustomButtonEdit.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
  	AdjustHeight;
  	AdjustEdit;
  end;
end;

function TCustomButtonEdit.GetEditHandle: HWND;
begin
	Result := FEdit.Handle;
end;

procedure TCustomButtonEdit.SetButtonVisible(Value: Boolean);
begin
  if Value <> FButtonVisible then
  begin
    FButtonVisible := Value;
    AdjustEdit;
  end;
end;

procedure TCustomButtonEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
    FFlat := Value;
end;

procedure TCustomButtonEdit.AssignGlyph;
begin
	if (FGlyph = nil) or FGlyphAssigned then Exit;
	FGlyph.Transparent := True;
  FGlyph.TransparentColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
	FGlyphAssigned := True;
end;


function TCustomButtonEdit.GetGlyph: TBitmap;
begin
  if (FGlyph = nil) and ([csDesigning] * ComponentState = []) then
    FGlyph := TBitmap.Create;
	Result := FGlyph;
end;

procedure TCustomButtonEdit.SetGlyph(Value: TBitmap);
begin
	if Value = nil then
  begin
  	FGlyph.Free;
    FGlyph := nil;
    FGlyphAssigned := False;
  end
  else
  begin
  	if FGlyph = nil then FGlyph := TBitmap.Create;
  	FGlyph.Assign(Value);
  end;
  Invalidate;
end;

procedure TCustomButtonEdit.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if FImages <> nil then
      InvalidateRect(Handle, nil, False);
  end;
end;

procedure TCustomButtonEdit.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FChangeLink);
    FImages.FreeNotification(Self);
  end;
  AdjustHeight;
  AdjustEdit;
  Invalidate;
end;

procedure TCustomButtonEdit.SetStyle(Value: TButtonEditStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if FStyle = beOwnerDrawn then
    begin
      FEdit.Visible := False;
      inherited TabStop := TabStop;
      if FFocused then
        SetFocus;
    end
    else
    begin
      FEdit.Visible := True;
      inherited TabStop := False;
      if Focused then
        FEdit.SetFocus;
    end;
    Invalidate;
  end;
end;

procedure TCustomButtonEdit.UpdateNextControl;
begin
  FNextControl := TCustomButtonEdit(Parent).FindNextControl(Self,
    GetKeyState(VK_SHIFT) > -1, False, True);
  if not (FNextControl is TCustomButtonEdit) then
    FNextControl := TCustomButtonEdit(Parent).FindNextControl(Self,
      GetKeyState(VK_SHIFT) > -1, True, True);
end;

{ TCustomButtonEdit.TEdit  }

procedure TCustomButtonEdit.Clear;
begin
  FEdit.Clear;
end;

procedure TCustomButtonEdit.ClearSelection;
begin
  FEdit.ClearSelection;
end;

procedure TCustomButtonEdit.CopyToClipboard;
begin
  FEdit.CopyToClipboard;
end;

procedure TCustomButtonEdit.CutToClipboard;
begin
  FEdit.CutToClipboard;
end;

procedure TCustomButtonEdit.PasteFromClipboard;
begin
  FEdit.PasteFromClipboard;
end;

procedure TCustomButtonEdit.Undo;
begin
  FEdit.Undo;
end;

procedure TCustomButtonEdit.ClearUndo;
begin
  FEdit.ClearUndo;
end;

function TCustomButtonEdit.GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := FEdit.GetSelTextBuf(Buffer, BufSize)
end;

procedure TCustomButtonEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TCustomButtonEdit.SetSelTextBuf(Buffer: PChar);
begin
  FEdit.SetSelTextBuf(Buffer);
end;

function TCustomButtonEdit.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

procedure TCustomButtonEdit.SetAutoSelect(Value: Boolean);
begin
  FEdit.AutoSelect := Value;
end;

function TCustomButtonEdit.GetCanUndo: Boolean;
begin
  Result := FEdit.CanUndo;
end;

function TCustomButtonEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

procedure TCustomButtonEdit.SetCharCase(Value: TEditCharCase);
begin
  FEdit.CharCase := Value;
end;

function TCustomButtonEdit.GetHideSelection: Boolean;
begin
  Result := FEdit.HideSelection;
end;

procedure TCustomButtonEdit.SetHideSelection(Value: Boolean);
begin
  FEdit.HideSelection := Value;
end;

function TCustomButtonEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

procedure TCustomButtonEdit.SetMaxLength(Value: Integer);
begin
  FEdit.MaxLength := Value;
end;

function TCustomButtonEdit.GetModified: Boolean;
begin
  Result := FEdit.Modified;
end;

procedure TCustomButtonEdit.SetModified(Value: Boolean);
begin
  FEdit.Modified := Value;
end;

function TCustomButtonEdit.GetOEMConvert: Boolean;
begin
  Result := FEdit.OEMConvert;
end;

procedure TCustomButtonEdit.SetOEMConvert(Value: Boolean);
begin
  FEdit.OEMConvert := Value;
end;

function TCustomButtonEdit.GetPasswordChar: Char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TCustomButtonEdit.SetPasswordChar(Value: Char);
begin
  FEdit.PasswordChar := Value;
end;

function TCustomButtonEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TCustomButtonEdit.SetReadOnly(Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

function TCustomButtonEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

procedure TCustomButtonEdit.SetSelLength(Value: Integer);
begin
  FEdit.SelLength := Value;
end;

function TCustomButtonEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

procedure TCustomButtonEdit.SetSelStart(Value: Integer);
begin
  FEdit.SelStart := Value;
end;

function TCustomButtonEdit.GetSelText: string;
begin
  Result := FEdit.SelText;
end;

procedure TCustomButtonEdit.SetSelText(Value: string);
begin
  FEdit.SelText := Value;
end;

function TCustomButtonEdit.GetTabStop: Boolean;
begin
  Result := FEdit.TabStop;
end;

procedure TCustomButtonEdit.SetWantTabs(Value: Boolean);
begin
  if Value <> FWantTabs then
    FWantTabs := Value;
end;

procedure TCustomButtonEdit.SetTabStop(Value: Boolean);
begin
  FEdit.TabStop := Value;
end;

procedure TCustomButtonEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FEdit.Enabled := Enabled;
  InvalidateRect(Handle, nil, False);
end;

procedure TCustomButtonEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustHeight;
end;

procedure TCustomButtonEdit.CMMouseLeave(var Message: TMessage);
var
	ButtonRect: TRect;
begin
	inherited;
  if FButtonHot then
  begin
  	FButtonHot := False;
    ButtonRect := GetButtonRect;
    InvalidateRect(Handle, @ButtonRect, False);
  end;
end;

procedure TCustomButtonEdit.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TCustomButtonEdit.WMParentNotify(var Message: TWMParentNotify);
begin
  if Message.Event = WM_CREATE then
  	PostMessage(Handle, CM_FONTCHANGED, 0, 0);
  Message.Result := 0;
end;

procedure TCustomButtonEdit.WMKillFocus(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomButtonEdit.WMSetFocus(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomButtonEdit.WMTimer(var Message: TWMTimer);
var
  Index: Integer;
  S: string;
  I: Integer;
begin
  KillTimer(Handle, Message.TimerID);
  if FSearchStrings <> nil then
  begin
    Index := -1;
    S := UpperCase(Copy(Text, 0, SelStart));
    if S <> '' then
      for I := 0 to FSearchStrings.Count - 1 do
        if S = UpperCase(Copy(FSearchStrings[I], 0, Length(S))) then
        begin
          Index := I;
          Text := FSearchStrings[I];
          SelStart := Length(S);
          SelLength := Length(FSearchStrings[I]) - Length(S);
          Break;
        end;
    if Assigned(FOnSearch) then
      FOnSearch(Self, Index);
  end;
  Message.Result := 0;
end;

procedure TCustomButtonEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

{ TPopupEdit }

procedure TPopupEdit.DoButtonClick;
begin
  inherited DoButtonClick;
	ParseInput;
  Popup;
end;

procedure TPopupEdit.RequestPopup;
begin
	if FPopupForm = nil then
  begin
    FPopupForm := CreatePopup;
    with FPopupForm do
    begin
      Associate := Self;
      Height := 75;
      SendKeys := True;
      OnCancel := DoCancel;
      OnSelect := DoSelect;
    end;
  end;
end;

procedure TPopupEdit.CreateWnd;
begin
  inherited CreateWnd;
  RequestPopup;
end;

procedure TPopupEdit.DestroyWnd;
begin
  FPopupForm.Associate := nil;
  inherited DestroyWnd;
end;

procedure TPopupEdit.DoPopup;
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TPopupEdit.DoCancel(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TPopupEdit.DoSelect(Sender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TPopupEdit.Popup;
begin
	DoPopup;
  FPopupForm.Popup;
end;

procedure TPopupEdit.Select;
begin
  TPopupHack(FPopupForm).Select;
end;

procedure TPopupEdit.Cancel;
begin
  TPopupHack(FPopupForm).Cancel;
end;

function TPopupEdit.GetSizeable: Boolean;
begin
	RequestPopup;
  Result := FPopupForm.Sizeable;
end;

procedure TPopupEdit.SetSizeable(const Value: Boolean);
begin
	RequestPopup;
  FPopupForm.Sizeable := Value;
end;

{ TDatePopupEdit }

constructor TDatePopupEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FPopupDateForm := TPopupDateForm.Create(Self);
end;

function TDatePopupEdit.CreatePopup: TCustomPopupForm;
begin
	Result := FPopupDateForm;
  Text := DateToStr(FPopupDateForm.Date);
end;

procedure TDatePopupEdit.DoSelect(Sender: TObject);
begin
  with FPopupDateForm do
		begin
			Text := DateToStr(Date);
			SelStart := 0;
			SelLength := High(Word);
			if Assigned(FOnSelect) then
				FOnSelect(Self);
		end;
end;

procedure TDatePopupEdit.ParseInput;
var
	D: TDate;
begin
	try
    D := StrToDate(Text);
  except
  	D := Date;
  end;
  Date := D;
end;

function TDatePopupEdit.GetDate: TDate;
begin
	Result := FPopupDateForm.Date;
end;

procedure TDatePopupEdit.SetDate(const Value: TDate);
begin
	FPopupDateForm.Date := Value;
  Text := DateToStr(Value);
end;

{ TListEdit }

constructor TListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupListBox := TPopupListForm.Create(Self);
end;

function TListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupListBox;
end;

procedure TListEdit.DoSelect(Sender: TObject);
begin
  with FPopupListBox do
		if ItemIndex > -1 then
		begin
			Text := Items[ItemIndex];
			SelStart := 0;
			SelLength := High(Word);
			if Assigned(FOnSelect) then
				FOnSelect(Self);
		end;
end;

function TListEdit.GetItemIndex: Integer;
begin
  Result := FPopupListBox.ItemIndex;
end;

procedure TListEdit.SetItemIndex(Value: Integer);
begin
  FPopupListBox.ItemIndex := Value;
end;

function TListEdit.GetItems: TStrings;
begin
  Result := FPopupListBox.Items;
end;

procedure TListEdit.SetItems(Value: TStrings);
begin
  FPopupListBox.Items := Value;
end;

{ TImageListEdit }

constructor TImageListEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  ReadOnly := True;
  Width := 75;
	FPopupImageList := TPopupImageListForm.Create(Self);
  FPopupImageList.ImageDrawList.OnClick := ImageClick;
  FPopupImageList.ImageDrawList.OnSelectItem := ImageSelectItem;
  FPopupImageList.Top := -9000;
  FPopupImageList.Show;
  FPopupImageList.ImageDrawList.ImageIndex := -1;
	FPopupImageList.Hide;
  FDisplayCount := 5;
end;

procedure TImageListEdit.CreateWnd;
begin
	inherited CreateWnd;
  Text := IntToStr(ImageIndex);
end;

function TImageListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupImageList;
end;

procedure TImageListEdit.DoPopup;
var
	I: Integer;
begin
	if FDisplayCount < 0 then
  	FDisplayCount := 1;
	I := 1;
	if Images <> nil then
  	I := I + Images.Count;
  if I > FDisplayCount then
  	I := FDisplayCount;
	FPopupImageList.Height := FPopupImageList.ImageDrawList.LineHeight * I + 2;
  inherited DoPopup;
end;

procedure TImageListEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	inherited EditKeyDown(Sender, Key, Shift);
  if (not FPopupImageList.Visible) and (Shift = []) then
  	case Key of
    	VK_DOWN:
      	begin
        	ImageIndex := ImageIndex + 1;
          Text := IntToStr(ImageIndex);
        end;
      VK_UP:
      	begin
        	ImageIndex := ImageIndex - 1;
          Text := IntToStr(ImageIndex);
        end;
    end;
end;

procedure TImageListEdit.ImageClick(Sender: TObject);
begin
	Select;
end;

procedure TImageListEdit.ImageSelectItem(Sender: TObject);
begin
	Text := IntToStr(ImageIndex);
end;

function TImageListEdit.GetImageIndex: Integer;
begin
	Result := FPopupImageList.ImageDrawList.ImageIndex;
end;

procedure TImageListEdit.SetImageIndex(Value: Integer);
begin
	FPopupImageList.ImageDrawList.ImageIndex := Value;
end;

function TImageListEdit.GetImages: TCustomImageList;
begin
	Result := FPopupImageList.ImageDrawList.Images;
end;

procedure TImageListEdit.SetImages(Value: TCustomImageList);
begin
	FPopupImageList.ImageDrawList.Images := Value;
end;

{ TCheckListEdit }

constructor TCheckListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupCheckList := TPopupCheckListForm.Create(Self);
  with FPopupCheckList.CheckList do
  begin
    OnClick := DoClickItem;
    OnClickCheck := DoClickCheck;
  end;
end;

function TCheckListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupCheckList;
end;

procedure TCheckListEdit.DoClickCheck(Sender: TObject);
begin
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Self);
end;

procedure TCheckListEdit.DoClickItem(Sender: TObject);
begin
  if Assigned(FOnClickItem) then
    FOnClickItem(Self);
end;

procedure TCheckListEdit.Resize;
begin
  inherited Resize;
  FPopupCheckList.Width := Width;
end;

function TCheckListEdit.GetChecked(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.Checked[Index];
end;

procedure TCheckListEdit.SetChecked(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.Checked[Index] := Value;
end;

function TCheckListEdit.GetCheckText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Items.Count - 1 do
    if Checked[I] then
      Result := Result + Items[I] + ', ';
  if Result <> '' then
    SetLength(Result, Length(Result) - 2);
end;

function TCheckListEdit.GetFlat: Boolean;
begin
  Result := FPopupCheckList.CheckList.Flat;
end;

procedure TCheckListEdit.SetFlat(Value: Boolean);
begin
  FPopupCheckList.CheckList.Flat := Value;
end;

function TCheckListEdit.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.ItemEnabled[Index];
end;

procedure TCheckListEdit.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.ItemEnabled[Index] := Value;
end;

function TCheckListEdit.GetItemIndex: Integer;
begin
  Result := FPopupCheckList.CheckList.ItemIndex;
end;

procedure TCheckListEdit.SetItemIndex(Value: Integer);
begin
  FPopupCheckList.CheckList.ItemIndex := Value;
end;

function TCheckListEdit.GetItems: TStrings;
begin
  Result := FPopupCheckList.CheckList.Items;
end;

procedure TCheckListEdit.SetItems(Value: TStrings);
begin
  FPopupCheckList.CheckList.Items := Value;
end;

function TCheckListEdit.GetSizeable: Boolean;
begin
  Result := FPopupCheckList.Sizeable;
end;

procedure TCheckListEdit.SetSizeable(Value: Boolean);
begin
  FPopupCheckList.Sizeable := Value;
end;

function TCheckListEdit.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FPopupCheckList.CheckList.State[Index];
end;

procedure TCheckListEdit.SetState(Index: Integer; Value: TCheckBoxState);
begin
  FPopupCheckList.CheckList.State[Index] := Value;
end;

function TCheckListEdit.GetStatusText: string;
begin
  Result := FPopupCheckList.StatusText;
end;

procedure TCheckListEdit.SetStatusText(Value: string);
begin
  FPopupCheckList.StatusText := Value;
end;

{ TImageEditItem }

procedure TImageEditItem.Assign(Source: TPersistent);
var
  EditItem: TImageEditItem absolute Source;
begin
  if Source is TImageEditItem then
  begin
    { TODO: Resolve changes here }
    FText := EditItem.Text;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TImageEditItem.SetImageIndex(const Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetIndent(const Value: Integer);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetSelectedIndex(const Value: Integer);
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TImageEditItems }

constructor TImageEditItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TImageEditItem);
end;

function TImageEditItems.Add: TImageEditItem;
begin
  Result := TImageEditItem(inherited Add);
end;

function TImageEditItems.FindItemID(ID: Integer): TImageEditItem;
begin
  Result := TImageEditItem(inherited FindItemID(ID));
end;

function TImageEditItems.Insert(Index: Integer): TImageEditItem;
begin
  Result := TImageEditItem(GetItem(Index));
end;

procedure TImageEditItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TControl then (GetOwner as TControl).Invalidate;
end;

function TImageEditItems.Get(Index: Integer): TImageEditItem;
begin
  Result := TImageEditItem(GetItem(Index));
end;

procedure TImageEditItems.Put(Index: Integer; const Value: TImageEditItem);
begin
  SetItem(Index, Value);
end;

{ TImageEdit }

constructor TImageEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageList := TImageList.Create(Self);
  FItems := TImageEditItems.Create(Self);
end;

destructor TImageEdit.Destroy;
begin
  FImageList.Free;
  FItems.Free;
  inherited Destroy;
end;

end.


