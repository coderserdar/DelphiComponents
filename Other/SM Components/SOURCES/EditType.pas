{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  This component allows to define a min/max values for end-user's entering,
  to check a type of value (date, integer, number etc).
  Also you can change the alignment and use the right-aligned button.
  For button you can define the width, hint and glyph style
  (bsNone, bsDropDown, bsElipsis, bsCustom)

  To use:
   - drop component on form
   - set the EnabledLimit property in True for activation of limitation on MIN/MAX
   - set the EnabledTypedLimit property in True for activation of check on value type
}

unit EditType;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses
  Windows, Messages, Classes, Graphics, Controls,
  Mask, Buttons, Menus, SMCnst
  {$IFDEF SMForDelphi6} , Variants {$ENDIF}
  ;

type
  TSMCompletition = (mcAutoSuggest, mcAutoAppend, mcFileSystem, mcUrlHistory, mcUrlMRU);
  TSMCompletitions = set of TSMCompletition;

  TTypeForEdit = (teString, teStringUpper, teStringLower,
                  teNumber, teInteger,
                  teDateTime, teShortDateTime, teDateShortTime, teShortDateShortTime,
                  teDate, teShortDate,
                  teTime, teShortTime,
                  tePhone, tePhoneExt, teSocial, teZIPCode);

  TSMButtonStyle = (bsNone, bsCustom, bsDropDown, bsEllipsis,
                    bsCalculator, bsCalendar, bsFile, bsDirectory,
                    bsOk, bsCancel, bsFind, bsSearch, bsSum);

  TCustomEditButton = class(TCustomMaskEdit)
  private
    { Private declarations }
    MouseInControl: Boolean;
    FFlat: Boolean;

    FTransparent: Boolean;
    FPainting: Boolean;

    FButton: TSpeedButton;
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FButtonStyle: TSMButtonStyle;
    FButtonKey: TShortCut;

    FAlignment: TAlignment;
    FAutoComplete: TSMCompletitions;

    {for flat support}
    procedure SetFlat(Value: Boolean);
    procedure RedrawBorder(const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;

    procedure SetAlignment(Value: TAlignment);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    procedure SetButtonStyle(Value: TSMButtonStyle); virtual;
    procedure EditButtonClick(Sender: TObject);
    procedure RecreateGlyph;
    procedure SetEditRect;
    procedure UpdateBtnBounds;
    function GetMinHeight: Integer;
    function GetTextHeight: Integer;

    function GetButtonFlat: Boolean;
    procedure SetButtonFlat(Value: Boolean);

    function GetPasswordChar: Char;
    procedure SetPasswordChar(Value: Char);

    procedure SetTransparent(Value: Boolean);
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure ButtonClick; dynamic;

    procedure RepaintWindow;
    procedure Change; override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure PaintParent(ACanvas: TCanvas);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Flat: Boolean read FFlat write SetFlat;

    property Button: TSpeedButton read FButton;
    property Alignment: TAlignment read FAlignment write SetAlignment;

    property AutoComplete: TSMCompletitions read FAutoComplete write FAutoComplete default [];

    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ButtonStyle: TSMButtonStyle read FButtonStyle write SetButtonStyle default bsCustom;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonKey: TShortCut read FButtonKey write FButtonKey;
    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TPopupAlign = (epaRight, epaLeft);
  TCustomPopupEdit = class(TCustomEditButton)
  private
    { Private declarations }
    FPopupVisible: Boolean;
    FFocused: Boolean;

    FPopupColor: TColor;
    FPopupAlign: TPopupAlign;
    FDirectInput: Boolean;

    function GetPopupColor: TColor;
    procedure SetPopupColor(Value: TColor);

    function GetDirectInput: Boolean;
    procedure SetDirectInput(Value: Boolean);

    function GetPopupVisible: Boolean;
    procedure SetShowCaret;

    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;

    procedure SetButtonStyle(Value: TSMButtonStyle); override;
    procedure CreateCalendarPopup;
    procedure DestroyCalendarPopup;
  protected
    { Protected declarations }
    FPopup: TCustomControl;

    procedure ButtonClick; override;

    procedure PopupCloseUp(Sender: TObject; Accept: Boolean);
    procedure ShowPopup(Origin: TPoint); virtual;
    procedure HidePopup; virtual;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure SetPopupValue(const Value: Variant); virtual;
    function GetPopupValue: Variant; virtual;

    function AcceptPopup(var Value: Variant): Boolean; virtual;
    procedure AcceptValue(const Value: Variant); virtual;
    procedure UpdatePopupVisible;

    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupColor: TColor read GetPopupColor write SetPopupColor default clBtnFace;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;

    property PopupVisible: Boolean read GetPopupVisible;
  published
    { Published declarations }
  end;

  TEditTyped = class(TCustomPopupEdit)
  private
    { Private declarations }
    FOldText: string;

    FMinValue, FMaxValue: String;
    FTypeValue: TTypeForEdit;
    FEnabledLimit, FEnabledTypedLimit: Boolean;

    procedure SetMinValue(Value: string);
    procedure SetMaxValue(Value: string);
    procedure SetTypeValue(Value: TTypeForEdit);
    procedure SetEnabledLimit(Value: Boolean);
    procedure SetEnabledTypedLimit(Value: Boolean);

    procedure SetMaskForEdit;
    function GetTypeName: string;
    function SetValue(var Dest: string; Source: string): Boolean;
    function GetValue: Variant;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function IsChanged: Boolean;
  published
    { Published declarations }
    property Alignment;
    property AutoComplete;
    property MinValue: String read FMinValue write SetMinValue;
    property MaxValue: String read FMaxValue write SetMaxValue;
    property TypeValue: TTypeForEdit read FTypeValue write SetTypeValue;
    property Value: Variant read GetValue;
    property EnabledLimit: Boolean read FEnabledLimit write SetEnabledLimit;
    property EnabledTypedLimit: Boolean read FEnabledTypedLimit write SetEnabledTypedLimit;
//    property MaskState;
    function ConvertValue(Source: string): Variant;

    property Flat;

    property Glyph;
    property ButtonStyle default bsCustom;
    property ButtonWidth;
    property NumGlyphs;
    property ButtonHint;
    property ButtonKey;
    property ButtonFlat;
    property OnButtonClick;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
{$R *.RES}
uses Forms, SysUtils, StdCtrls, ExtCtrls, Grids, Calendar;

procedure Register;
begin
  RegisterComponents('SMComponents', [TEditTyped]);
end;

const
  DefEditBtnWidth = 21;

type
  TShAutoCompleteFunc = function(hwndEdit: HWND; dwFlags: dWord): LongInt; stdcall;

var
  dllShlwApi: THandle;
  SHAutoComplete: TShAutoCompleteFunc;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B
end;

function Max(A, B: Integer): Integer;
begin
  if A < B then
    Result := B
  else
    Result := A
end;

{ TPopupEditWindow }
type
  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;

  TPopupEditWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    procedure InvalidateEditor;
    procedure PopupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint);
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

{ TPopupEditWindow }
constructor TPopupEditWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditor := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable,
    csAcceptsControls];
  Ctl3D := False;
  ParentCtl3D := False;
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TPopupEditWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TPopupEditWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

function TPopupEditWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TPopupEditWindow.InvalidateEditor;
var
  R: TRect;
begin
  if (FEditor is TCustomPopupEdit) then
  begin
    with TCustomPopupEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  end
  else
    R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TPopupEditWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TPopupEditWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

procedure TPopupEditWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TPopupEditWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

{ TLocCalendar }
type
  TLocCalendar = class(TCalendar)
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;

constructor TLocCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  Ctl3D := False;
  Enabled := False;
  BorderStyle := forms.bsNone;
  ParentColor := True;
  CalendarDate := Trunc(Now);
  UseCurrentDate := False;
  FixedColor := Self.Color;
  Options := [goFixedHorzLine];
  TabStop := False;
end;

procedure TLocCalendar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ParentColor then FixedColor := Self.Color;
end;

procedure TLocCalendar.CMEnabledChanged(var Message: TMessage);
begin
  if HandleAllocated and not (csDesigning in ComponentState) then
    EnableWindow(Handle, True);
end;

procedure TLocCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and not (WS_BORDER or WS_TABSTOP or WS_DISABLED);
end;

procedure TLocCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TLocCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  D, M, Y: Word;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
  DecodeDate(CalendarDate, Y, M, D);
  D := StrToIntDef(CellText[ACol, ARow], 0);
  if (D > 0) and (D <= DaysPerMonth(Y, M)) then
  begin
    if (EncodeDate(Y, M, D) = SysUtils.Date) then
      Frame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1);
  end;
end;

{ TPopupEditCalendar }
type
  TPopupEditCalendar = class(TPopupEditWindow)
  private
    FCalendar: TCalendar;
    FTitleLabel: TLabel;
    FFourDigitYear: Boolean;
    FBtns: array[0..3] of TSpeedButton;
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TPopupEditCalendar.Create(AOwner: TComponent);
const
  BtnSide = 14;
  PopupCalendarSize: TPoint = (X: 187; Y: 124);
  SBtnGlyphs: array[0..3] of PChar = ('PREV2', 'PREV1', 'NEXT1', 'NEXT2');
var
  Control, BackPanel: TWinControl;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);

  FFourDigitYear := True;//FourDigitYear;
  Height := Max(PopupCalendarSize.Y, 120);
  Width := Max(PopupCalendarSize.X, 180);
  Color := clBtnFace;

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
  begin
    Font.Color := clWindowText;
    Font.Name := 'MS Sans Serif';
    Font.Size := 8;
    Font.Style := [];
  end;

  if AOwner is TControl then
    ShowHint := TControl(AOwner).ShowHint
  else
    ShowHint := True;
  if (csDesigning in ComponentState) then Exit;

  BackPanel := TPanel.Create(Self);
  with BackPanel as TPanel do
  begin
    Parent := Self;
    Align := alClient;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := BackPanel;
    Align := alTop;
    Width := Self.Width - 4;
    Height := 18;
    BevelOuter := bvNone;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  FCalendar := TLocCalendar.Create(Self);
  with TLocCalendar(FCalendar) do
  begin
    Parent := BackPanel;
    Align := alClient;
    OnChange := CalendarChange;
    OnMouseUp := CalendarMouseUp;
  end;

  FBtns[0] := TSpeedButton.Create(Self);
  with FBtns[0] do
  begin
    Parent := Control;
    SetBounds(-1, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[0]);
    OnClick := PrevYearBtnClick;
//    Hint := SPrevYear;
  end;

  FBtns[1] := TSpeedButton.Create(Self);
  with FBtns[1] do
  begin
    Parent := Control;
    SetBounds(BtnSide - 2, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[1]);
    OnClick := PrevMonthBtnClick;
//    Hint := SPrevMonth;
  end;

  FTitleLabel := TLabel.Create(Self);
  with FTitleLabel do
  begin
    Parent := Control;
    AutoSize := False;
    Alignment := taCenter;
    SetBounds(BtnSide * 2 + 1, 1, Control.Width - 4 * BtnSide - 2, 14);
    Transparent := True;
    OnDblClick := TopPanelDblClick;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  FBtns[2] := TSpeedButton.Create(Self);
  with FBtns[2] do
  begin
    Parent := Control;
    SetBounds(Control.Width - 2 * BtnSide + 2, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[2]);
    OnClick := NextMonthBtnClick;
//    Hint := SNextMonth;
  end;

  FBtns[3] := TSpeedButton.Create(Self);
  with FBtns[3] do
  begin
    Parent := Control;
    SetBounds(Control.Width - BtnSide + 1, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[3]);
    OnClick := NextYearBtnClick;
//    Hint := SNextYear;
  end;
end;

procedure TPopupEditCalendar.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Longint;
begin
  if (Button = mbLeft) and (Shift = []) then
  begin
    TLocCalendar(FCalendar).MouseToCell(X, Y, Col, Row);
    if (Row > 0) and (FCalendar.CellText[Col, Row] <> '') then
      CloseUp(True);
  end;
end;

procedure TPopupEditCalendar.TopPanelDblClick(Sender: TObject);
begin
  FCalendar.CalendarDate := Trunc(Now);
end;

procedure TPopupEditCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FCalendar <> nil then
    case Key of
      VK_NEXT:
        begin
          if ssCtrl in Shift then
            FCalendar.NextYear
          else
            FCalendar.NextMonth;
        end;
      VK_PRIOR:
        begin
          if ssCtrl in Shift then
            FCalendar.PrevYear
          else
            FCalendar.PrevMonth;
        end;
      else
        TLocCalendar(FCalendar).KeyDown(Key, Shift);
    end;
end;

procedure TPopupEditCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

//  if (FCalendar <> nil) and (Key <> #0) then
//    FCalendar.KeyPress(Key);
end;

function TPopupEditCalendar.GetValue: Variant;
begin
  if (csDesigning in ComponentState) then
    Result := VarFromDateTime(SysUtils.Date)
  else
    Result := VarFromDateTime(FCalendar.CalendarDate);
end;

procedure TPopupEditCalendar.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then
  begin
    try
{      if (Trim(ReplaceStr(VarToStr(Value), DateSeparator, '')) = '') or
        VarIsNull(Value) or VarIsEmpty(Value) then
        FCalendar.CalendarDate := VarToDateTime(SysUtils.Date)
      else
        FCalendar.CalendarDate := VarToDateTime(Value);
}      CalendarChange(nil);
    except
      FCalendar.CalendarDate := VarToDateTime(SysUtils.Date);
    end;
  end;
end;

procedure TPopupEditCalendar.PrevYearBtnClick(Sender: TObject);
begin
  FCalendar.PrevYear;
end;

procedure TPopupEditCalendar.NextYearBtnClick(Sender: TObject);
begin
  FCalendar.NextYear;
end;

procedure TPopupEditCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  FCalendar.PrevMonth;
end;

procedure TPopupEditCalendar.NextMonthBtnClick(Sender: TObject);
begin
  FCalendar.NextMonth;
end;

procedure TPopupEditCalendar.CalendarChange(Sender: TObject);
begin
  FTitleLabel.Caption := FormatDateTime('MMMM, YYYY', FCalendar.CalendarDate);
end;

{ TCustomEditButton }
constructor TCustomEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTransparent := False;
  FPainting := False;

  FAutoComplete := [];
  FAlignment := taLeftJustify;
  Flat := False;

  FBtnControl := TWinControl.Create(Self);
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  FBtnControl.Width := 0; //DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;

  FButton := TSpeedButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  FButton.OnClick := EditButtonClick;
  Height := 21;
  FButtonStyle := bsNone;
  FButtonKey := scAlt + vk_Down;
end;

destructor TCustomEditButton.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

procedure TCustomEditButton.CreateParams(var Params: TCreateParams);
const Alignments: array[TAlignment] of dWord =
      (ES_LEFT, ES_RIGHT, ES_CENTER);
      Multilines: array[Boolean] of dWord = (0, ES_MULTILINE);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or
                  Multilines[PasswordChar = #0] or
                  Alignments[FAlignment];
end;

procedure TCustomEditButton.CreateWnd;

const
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;
  SHACF_DEFAULT = $0;
  SHACF_FILESYSTEM = $1;
  SHACF_URLHISTORY = $2;
  SHACF_URLMRU = $4;
  SHACF_URLALL = SHACF_URLHISTORY or SHACF_URLMRU;

var
  Options: dWord;
begin
  inherited CreateWnd;

  SetEditRect;

  if (dllShlwApi <> 0) and
     (FAutoComplete <> []) and
     (@ShAutoComplete <> nil) then
  begin
    Options := 0;
    if (mcAutoSuggest in FAutoComplete) then
      Options := Options or SHACF_AUTOSUGGEST_FORCE_ON;
    if (mcAutoAppend in FAutoComplete) then
      Options := Options or SHACF_AUTOAPPEND_FORCE_ON;
    if (mcFileSystem in FAutoComplete) then
      Options := Options or SHACF_FILESYSTEM;
    if (mcUrlHistory in FAutoComplete) then
      Options := Options or SHACF_URLHISTORY;
    if (mcUrlMRU in FAutoComplete) then
      Options := Options or SHACF_URLMRU;

    SHAutoComplete(Handle, Options);
  end;
end;

procedure TCustomEditButton.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TCustomEditButton.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if Value then
    begin
      BorderStyle := TBorderStyle(bsNone);
      ControlStyle := ControlStyle - [csFramed]; {fixes a VCL bug with Win 3.x}
    end
    else
    begin
      BorderStyle := bsSingle;
//      ControlStyle := ControlStyle + [csFramed];
    end;
    Ctl3D := not Value;
  end;
end;

procedure TCustomEditButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
  canvas: TCanvas;
begin
  canvas := TCanvas.create;
  try
    canvas.Handle := message.dc;
    if FTransparent and
       not (csDesigning in ComponentState) then
      PaintParent(Canvas)
    else
    begin
      canvas.Brush.Color := Color;
      canvas.Brush.Style := bsSolid;
      canvas.FillRect(ClientRect);
    end
  finally
    canvas.free;
  end;
end;

procedure TCustomEditButton.WMPaint(var Message: TWMPaint);
begin
  inherited;

  if FTransparent then
    if not FPainting then
      RepaintWindow;
end;

procedure TCustomEditButton.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
  inherited;

  if FTransparent then
    SetBkMode(Message.ChildDC, 1);
end;

procedure TCustomEditButton.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  inherited;

  if FTransparent then
    SetBkMode(Message.ChildDC, 1);
end;

procedure TCustomEditButton.CMParentColorChanged(var Message: TMessage);
begin
  inherited;

  if FTransparent then
    Invalidate;
end;

procedure TCustomEditButton.WMMove(var Message: TWMMove);
var
  r: TRect;
begin
  inherited;

  Invalidate;
  r := ClientRect;
  InvalidateRect(Handle, @r, False);
end;

procedure TCustomEditButton.RepaintWindow;
const 
  BorderRec: array[TBorderStyle] of Integer = (1, -1); 
var
  DC: hDC;
  TmpBitmap, Bitmap: hBitmap;
begin
  if FTransparent then
  begin
    FPainting := True;
    HideCaret(Handle);
    DC := CreateCompatibleDC(GetDC(Handle));
    TmpBitmap := CreateCompatibleBitmap(GetDC(Handle), Succ(ClientWidth), Succ(ClientHeight));
    Bitmap := SelectObject(DC, TmpBitmap);
    PaintTo(DC, 0, 0);
    {$IFDEF SMForDelphi5}
    BitBlt(GetDC(Handle), BorderRec[BorderStyle] + BorderWidth, BorderRec[BorderStyle] + BorderWidth, ClientWidth, ClientHeight, DC, 1, 1, SRCCOPY);
    {$ELSE}
    BitBlt(GetDC(Handle), BorderRec[BorderStyle], BorderRec[BorderStyle], ClientWidth, ClientHeight, DC, 1, 1, SRCCOPY);
    {$ENDIF}
    SelectObject(DC, Bitmap);
    DeleteDC(DC);
    ReleaseDC(Handle, GetDC(Handle));
    DeleteObject(TmpBitmap);
    ShowCaret(Handle);
    FPainting := False;
  end;
end;

procedure TCustomEditButton.Change;
begin
  RepaintWindow;

  inherited Change;
end;

type 
  THackWinControl = class(TWinControl);

procedure TCustomEditButton.PaintParent(ACanvas: TCanvas);
var
  i, Count, X, Y, SaveIndex: Integer;
  DC: Cardinal;
  R, SelfR, CtlR: TRect;
  Control: TControl;
begin
  Control := Self;
  if Control.Parent = nil then Exit;

  Count := Control.Parent.ControlCount;
  DC := ACanvas.Handle;

  SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
  X := -Control.Left; Y := -Control.Top;
  // Copy parent control image
  SaveIndex := SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, nil);
  IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
  with THackWinControl(Control.Parent) do
  begin
    Perform(WM_ERASEBKGND, DC, 0);
    PaintWindow(DC);
  end;
  RestoreDC(DC, SaveIndex);

  //Copy images of graphic controls
  for i := 0 to Count - 1 do
  begin
    if (Control.Parent.Controls[i] <> nil) then
    begin
      if Control.Parent.Controls[i] = Control then break;

      with Control.Parent.Controls[i] do
      begin
        CtlR := Bounds(Left, Top, Width, Height);
        if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
        begin
          SaveIndex := SaveDC(DC);
          SetViewportOrgEx(DC, Left + X, Top + Y, nil);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(WM_ERASEBKGND,DC,0);
          Perform(WM_PAINT, integer(DC), 0);
          RestoreDC(DC, SaveIndex);
        end;
      end;
    end;
  end;
end;

procedure TCustomEditButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  if Flat then
  begin
    MouseInControl := True;
    RedrawBorder(0);
  end;
end;

procedure TCustomEditButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if Flat then
  begin
    MouseInControl := False;
    RedrawBorder(0);
  end;
end;

procedure TCustomEditButton.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  Height := Metrics.tmHeight + 6;
end;

procedure TCustomEditButton.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) and Flat then
    NewAdjustHeight;
end;

procedure TCustomEditButton.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;

  if Flat then
    Color := EnableColors[Enabled];
end;

procedure TCustomEditButton.CMFontChanged(var Message: TMessage);
begin
  inherited;

  if not ((csDesigning in ComponentState) and (csLoading in ComponentState)) and Flat then
    NewAdjustHeight;
end;

procedure TCustomEditButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  if not (csDesigning in ComponentState) and Flat then
    RedrawBorder(0);
end;

procedure TCustomEditButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  if not(csDesigning in ComponentState) and Flat then
    RedrawBorder(0);
end;

procedure TCustomEditButton.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;

  if Flat then
    InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TCustomEditButton.WMNCPaint(var Message: TMessage);
begin
  inherited;

  if Flat then
    RedrawBorder(Message.WParam);
end;

procedure TCustomEditButton.RedrawBorder(const Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  NewClipRgn: HRGN;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  DC := GetWindowDC(Handle);
  try
    { Use update region }
    if Clip <> 0 then
    begin
      GetWindowRect(Handle, R);
      { An invalid region is generally passed when the window is first created }
      if SelectClipRgn(DC, Clip) = ERROR then {ERROR = 0 in Windows.Pas}
      begin
        NewClipRgn := CreateRectRgnIndirect(R);
        SelectClipRgn(DC, NewClipRgn);
        DeleteObject(NewClipRgn);
      end;
      OffsetClipRgn(DC, -R.Left, -R.Top);
    end;

    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));
    if ((csDesigning in ComponentState) and Enabled) or
       (not(csDesigning in ComponentState) and
        (Focused or (MouseInControl and not (Screen.ActiveControl is TCustomEditButton)))) then
    begin
      DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
    end
    else
    begin
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, BtnFaceBrush);
      InflateRect(R, -1, -1);
      FrameRect(DC, R, WindowBrush);
    end;
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

function TCustomEditButton.GetPasswordChar: Char;
begin
  Result := inherited PasswordChar;
end;

procedure TCustomEditButton.SetPasswordChar(Value: Char);
begin
  if PasswordChar <> Value then
  begin
    inherited PasswordChar := Value;

    RecreateWnd;
  end;
end;

procedure TCustomEditButton.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TCustomEditButton.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TCustomEditButton.SetButtonWidth(Value: Integer);
begin
  if (csCreating in ControlState) then
  begin
    FBtnControl.Width := Value;
    FButton.Width := Value;
    RecreateGlyph;
  end
  else
    if (Value <> ButtonWidth) and (Value < ClientWidth) then
    begin
      FButton.Width := Value;
      if HandleAllocated then RecreateWnd;
      RecreateGlyph;
    end;
end;

function TCustomEditButton.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TCustomEditButton.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TCustomEditButton.GetButtonFlat: Boolean;
begin
  Result := FButton.Flat;
end;

procedure TCustomEditButton.SetButtonFlat(Value: Boolean);
begin
  FButton.Flat := Value;
end;

function TCustomEditButton.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TCustomEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
//  FButtonStyle := bsCustom;
end;

function TCustomEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TCustomEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FButtonStyle in [bsDropDown, bsEllipsis, bsCalculator, bsCalendar, bsFile, bsDirectory] then
    FButton.NumGlyphs := 1
  else
    FButton.NumGlyphs := Value;
end;

procedure TCustomEditButton.EditButtonClick(Sender: TObject);
begin
  ButtonClick;
end;

procedure TCustomEditButton.ButtonClick;
begin
  if (ButtonWidth > 0) and Assigned(FOnButtonClick) then
    FOnButtonClick(Self)
end;

procedure TCustomEditButton.SetButtonStyle(Value: TSMButtonStyle);
begin
  if (FButtonStyle <> Value) then
  begin
    FButtonStyle := Value;
    if (FButtonStyle = bsNone) or
       ((FButtonStyle = bsCustom) and (csReading in ComponentState)) then
      Glyph := nil;

    RecreateGlyph;
    case FButtonStyle of
      bsNone: FButton.Width := 0;
      bsDropDown: ButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
      bsCalculator, bsCalendar: ButtonWidth := 17;
    else
      ButtonWidth := DefEditBtnWidth;
    end;
  end;
end;


procedure TCustomEditButton.RecreateGlyph;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, G, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      G := (Result.Width - 3 * W) div 2;
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      I := (Width - 3 * W - 2 * G) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

var
  NewGlyph: TBitmap;
begin
  case FButtonStyle of
    bsDropDown: FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
    bsEllipsis: begin
                  NewGlyph := CreateEllipsisGlyph;
                  try
                    FButton.Glyph := NewGlyph;
                  finally
                    NewGlyph.Destroy;
                  end;
                end;
    bsCalculator: FButton.Glyph.Handle := LoadBitmap(hInstance, 'CALCULATORBMP');
    bsCalendar: FButton.Glyph.Handle := LoadBitmap(hInstance, 'CALENDARBMP');
    bsFile: FButton.Glyph.Handle := LoadBitmap(hInstance, 'FILEBMP');
    bsDirectory: FButton.Glyph.Handle := LoadBitmap(hInstance, 'DIRECTORYBMP');
    bsOk: FButton.Glyph.Handle := LoadBitmap(hInstance, 'OKBMP');
    bsCancel: FButton.Glyph.Handle := LoadBitmap(hInstance, 'CANCELBMP');
    bsFind: FButton.Glyph.Handle := LoadBitmap(hInstance, 'FINDBMP');
    bsSearch: FButton.Glyph.Handle := LoadBitmap(hInstance, 'SEARCHBMP');
    bsSum: FButton.Glyph.Handle := LoadBitmap(hInstance, 'SUMBMP');
  end;
  if (FButtonStyle <> bsCustom) then
    NumGlyphs := 1;
end;

procedure TCustomEditButton.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, 1{0}, ClientWidth - FBtnControl.Width {- 2}, ClientHeight {+ 1});
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TCustomEditButton.UpdateBtnBounds;
begin
  if NewStyleControls then
  begin
    if Ctl3D or Flat then
//      FBtnControl.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 4)
      FBtnControl.SetBounds(ClientWidth - FButton.Width, 0, FButton.Width, ClientHeight)
    else
//      FBtnControl.SetBounds(Width - FButton.Width - 2, 2, FButton.Width, Height - 4);
      FBtnControl.SetBounds(ClientWidth - FButton.Width - 2, 1, FButton.Width, ClientHeight-2);
  end
  else
    FBtnControl.SetBounds(Width - FButton.Width - 2, 1, FButton.Width, ClientHeight - 2);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

procedure TCustomEditButton.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;

  UpdateBtnBounds;
end;

procedure TCustomEditButton.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  r: TRect;
begin
  inherited;

  r := ClientRect;
  InvalidateRect(Handle, @r, False);

  if not (csLoading in ComponentState) then
  begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then
    begin
      Height := MinHeight;
      Exit;
    end;
  end;
  UpdateBtnBounds;
end;

function TCustomEditButton.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Min(SysMetrics.tmHeight, Metrics.tmHeight);
end;

function TCustomEditButton.GetMinHeight: Integer;
var i: Integer;
begin
  i := GetTextHeight;
  Result := i + GetSystemMetrics(SM_CYBORDER) * 4 + 1 + (i div 4);
end;

procedure TCustomEditButton.CMEnter(var Message: TCMEnter);
begin
  inherited;

  if AutoSelect then
    SendMessage(Handle, EM_SETSEL, 0, -1);
end;

procedure TCustomEditButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (FButtonKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

{ TCustomPopupEdit }
constructor TCustomPopupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPopupColor := clBtnFace;
  FPopupAlign := epaRight;

  FPopup := nil;
end;

destructor TCustomPopupEdit.Destroy;
begin
  DestroyCalendarPopup;

  inherited Destroy;
end;

procedure TCustomPopupEdit.CreateCalendarPopup;
begin
  FPopup := nil;
  if (csDesigning in ComponentState) then exit;
  
  FPopup := TPopupEditWindow(TPopupEditCalendar.Create(Self));
  with TPopupEditWindow(FPopup) do
  begin
    OnCloseUp := PopupCloseUp;
    Color := FPopupColor;
  end;
end;

procedure TCustomPopupEdit.DestroyCalendarPopup;
begin
  if FPopup <> nil then
  begin
    TPopupEditWindow(FPopup).OnCloseUp := nil;
    FPopup.Free;
    FPopup := nil;
  end
end;

function TCustomPopupEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then
    Result := TPopupEditWindow(FPopup).Color
  else
    Result := FPopupColor;
end;

procedure TCustomPopupEdit.SetPopupColor(Value: TColor);
begin
  if Value <> PopupColor then
  begin
    if FPopup <> nil then
      TPopupEditWindow(FPopup).Color := Value;
    FPopupColor := Value;
  end;
end;

function TCustomPopupEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

procedure TCustomPopupEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or ReadOnly;
  FDirectInput := Value;
end;

procedure TCustomPopupEdit.HidePopup;
begin
  TPopupEditWindow(FPopup).Hide;
end;

procedure TCustomPopupEdit.ShowPopup(Origin: TPoint);
begin
  TPopupEditWindow(FPopup).Show(Origin);
end;

procedure TCustomPopupEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;
begin
  if (FPopup <> nil) and not (ReadOnly or FPopupVisible) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then
      P.X := 0
    else
      if P.X + FPopup.Width > Screen.Width then
        P.X := Screen.Width - FPopup.Width;
    if Text <> '' then
      SetPopupValue(Text)
    else
      SetPopupValue(Null);

    if CanFocus then
      SetFocus;
    ShowPopup(Point(P.X, Y));
    FPopupVisible := True;
    if DisableEdit then
    begin
//      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TCustomPopupEdit.SetShowCaret;
const
  CaretWidth: array[Boolean] of Byte = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], ClientHeight-5{GetTextHeight});
  ShowCaret(Handle);
end;

procedure TCustomPopupEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  AValue: Variant;
begin
  if (FPopup <> nil) and FPopupVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
        begin
          SetFocus;
          if GetFocus = Handle then
            SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
//      SetDirectInput(DirectInput);
      Invalidate;
      if Accept and AcceptPopup(AValue) and EditCanModify then
      begin
        AcceptValue(AValue);
        if FFocused then
          inherited SelectAll;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TCustomPopupEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

function TCustomPopupEdit.AcceptPopup(var Value: Variant): Boolean;
begin
  Result := True;
end;

function TCustomPopupEdit.GetPopupValue: Variant;
begin
  if FPopup <> nil then
    Result := TPopupEditWindow(FPopup).GetValue
  else
    Result := '';
end;

procedure TCustomPopupEdit.SetPopupValue(const Value: Variant);
begin
  if FPopup <> nil then
    TPopupEditWindow(FPopup).SetValue(Value);
end;

procedure TCustomPopupEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then
  begin
    Text := Value;
    Modified := True;
    UpdatePopupVisible;

    inherited Change;
  end;
end;

function TCustomPopupEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

procedure TCustomPopupEdit.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
  begin
    if PopupVisible then
    begin
      PopupCloseUp(FPopup, Key = Char(VK_RETURN));
      Key := #0;
    end
    else
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TCustomPopupEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then
  begin
    if CanFocus then
      SetFocus;
    if not FFocused then Exit;
    if FPopupVisible then
      PopupCloseUp(FPopup, False);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomPopupEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and
     (Message.Sender <> FPopup) and
     (Message.Sender <> FButton) and
     ((FPopup <> nil) and
       not FPopup.ContainsControl(Message.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TCustomPopupEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  FFocused := False;
  PopupCloseUp(FPopup, False);
end;

procedure TCustomPopupEdit.WMSetFocus(var Message: TMessage);
begin
  inherited;
  FFocused := True;
  SetShowCaret;
end;

procedure TCustomPopupEdit.SetButtonStyle(Value: TSMButtonStyle);
begin
  if (Value <> FButtonStyle) then
    DestroyCalendarPopup;

  inherited SetButtonStyle(Value);

  if (Value = bsCalendar) then
    CreateCalendarPopup;
end;

procedure TCustomPopupEdit.ButtonClick;
begin
  inherited ButtonClick;

  if FPopup <> nil then
  begin
    if FPopupVisible then
      PopupCloseUp(FPopup, True)
    else
      PopupDropDown(True);
  end;
end;


{ TEditTyped }
constructor TEditTyped.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetMaskForEdit;
  EnabledTypedLimit := True;

  FOldText := '';
end;

procedure TEditTyped.SetMaskForEdit;
begin
  EditMask := '';

  case FTypeValue of
    teString: CharCase := ecNormal;
    teStringUpper: CharCase := ecUpperCase;
    teStringLower: CharCase := ecLowerCase;
//    teNumber: EditMask := '### ### ### ##0.00;1;_';
    teDateTime: EditMask := '!99/99/9999 99:99:99;1;_';
    teShortDateTime: EditMask := '!99/99/99 99:99:99;1;_';
    teDateShortTime: EditMask := '!99/99/9999 99:99;1;_';
    teShortDateShortTime: EditMask := '!99/99/99 99:99;1;_';
    teDate: EditMask := '!99/99/9999;1;_';
    teShortDate: EditMask := '!99/99/99;1;_';
    teTime: EditMask := '!90:99:99;1;_';
    teShortTime: EditMask := '!99:99;1;_';
    tePhone: EditMask := '!\(999\) 999\-9999;1;_';
    tePhoneExt: EditMask := '!\(999\) 999\-9999 \x9999;1;_';
    teSocial: EditMask := '!999\-99\-9999;1;_';
    teZIPCode: EditMask := '!99999\-9999;1;_';
  end;
{  if FTypeValue in [teInteger, teNumber] then
    SetAlignment(taRightJustify)
  else
    SetAlignment(taLeftJustify);
}end;

function TEditTyped.GetTypeName: string;
begin
  case FTypeValue of
    teNumber: Result := etValidNumber;
    teInteger: Result := etValidInteger;
    teDateTime,
    teShortDateTime,
    teDateShortTime,
    teShortDateShortTime: Result := etValidDateTime;
    teDate,
    teShortDate: Result := etValidDate;
    teTime,
    teShortTime: Result := etValidTime;
  else
    Result := etValid;
  end;
end;

procedure TEditTyped.SetMinValue(Value: String);
begin
  if FMinValue <> Value then
    SetValue(FMinValue, Value);
end;

procedure TEditTyped.SetMaxValue(Value: String);
begin
  if FMaxValue <> Value then
    SetValue(FMaxValue, Value);
end;

procedure TEditTyped.SetTypeValue(Value: TTypeForEdit);
begin
  if FTypeValue <> Value then
  begin
    FTypeValue := Value;
    SetMaskForEdit;
  end;
end;

procedure TEditTyped.SetEnabledLimit(Value: Boolean);
begin
  if FEnabledLimit <> Value then
    FEnabledLimit := Value;
end;

procedure TEditTyped.SetEnabledTypedLimit(Value: Boolean);
begin
  if FEnabledTypedLimit <> Value then
    FEnabledTypedLimit := Value;
end;

function TEditTyped.ConvertValue(Source: string): Variant;

  function Stuff(strStr: string; intIndex, intCount: Integer; strSub: string): string;
  begin
    Result := strStr;
    Delete(Result, intIndex, intCount);
    Insert(strSub, Result, intIndex);
  end;

var
  varForConvert: Variant;
  str, strOldMaskEdit, strEditText: string;
begin
  Result := Null;

  try
    {save current editing text}
    strEditText := EditText;
    Text := Source;

    {change a mask for check of the null string}
    str := Stuff(EditMask, Length(EditMask)-2, 1, '0');
    strOldMaskEdit := EditMask;
    ReformatText(str);
    str := Text;
    ReformatText(strOldMaskEdit);

    {restore a saved editing text}
    EditText := strEditText;

    if (Source = '') or (Trim(str) = '') then
    begin
      varForConvert := NULL;
{      case FTypeValue of
        teNumber,
        teInteger: varForConvert := 0;
        teDateTime,
        teShortDateTime,
        teDateShortTime,
        teShortDateShortTime: varForConvert := 0;
        teDate,
        teShortDate: varForConvert := 0;
        teTime,
        teShortTime: varForConvert := 0;
      else
        varForConvert := ''
      end;
}    end
    else
    begin
      case FTypeValue of
        teNumber: varForConvert := StrToFloat(Source);
//        {$IFDEF VER130}
//        teInteger: varForConvert := StrToInt64(Source);
//        {$ELSE}
        teInteger: varForConvert := StrToInt(Source);
//        {$ENDIF}
        teDateTime,
        teShortDateTime,
        teDateShortTime,
        teShortDateShortTime: varForConvert := StrToDateTime(Source);
        teDate,
        teShortDate: varForConvert := StrToDate(Source);
        teTime,
        teShortTime: varForConvert := StrToTime(Source);
      else
        varForConvert := Source
      end;
    end;
    Result := varForConvert
  except
    raise EConvertError.CreateFmt('"%s" %s %s', [Source, etIsNot, GetTypeName()])
  end;
end;

function TEditTyped.SetValue(var Dest: String; Source: String): Boolean;
var
  varForConvert: Variant;
begin
  varForConvert := ConvertValue(Source);
  Result := not varIsNull(varForConvert);
  if Result then Dest := Source;
end;

procedure TEditTyped.CMEnter(var Message: TCMEnter);
begin
  FOldText := Text;

  inherited;
end;

procedure TEditTyped.CMExit(var Message: TCMExit);
var
  varValue, varMin, varMax: Variant;
  strValue: string;
begin
  try
    if (FEnabledTypedLimit = True) then
      SetValue(strValue, Text);

    if (FEnabledLimit = True) then
    begin
      varValue := ConvertValue(Text);
      varMin   := ConvertValue(FMinValue);
      varMax   := ConvertValue(FMaxValue);

      if ((varValue < varMin) or (varValue > varMax)) then
      begin
        if not (csDesigning in ComponentState) then
        begin
          raise ERangeError.CreateFmt(etOutOfRange, [Text, FMinValue, FMaxValue]);
        end;
      end;
    end;

    inherited;
  except
    SetFocus;
    raise;
  end;
end;

procedure TEditTyped.KeyPress(var Key: Char);
const
  Alpha = [#1..#255]; //'A'..'Z', 'a'..'z', '_'];
  NumericOnly = ['0'..'9'];
  SignOnly = ['-', '+'];
  Numeric = SignOnly + NumericOnly;
var
  boolCallInherited: Boolean;
  frmParent: TCustomForm;
begin
  boolCallInherited := True;
  if FEnabledTypedLimit and
     not {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#0, #8, #13]) then
  begin
    case FTypeValue of
      teNumber: begin
                  if {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}['.', ',']) then
                    Key := DecimalSeparator;

                  boolCallInherited := (((Key = DecimalSeparator) or
                                         {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}SignOnly)) and
                                        (Pos(Key, Text) = 0)) or
                                         {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}Numeric + ['E', 'e']);
                end;

      teInteger: boolCallInherited := ({$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}SignOnly) and
                                       (Pos(Key, Text) = 0)) or
                                      {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}Numeric);

      teDateTime,
      teShortDateTime,
      teDateShortTime,
      teShortDateShortTime,
      teDate,
      teShortDate,
      teTime,
      teShortTime: boolCallInherited := {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}Numeric);

    end;
  end;

  if boolCallInherited then
  begin
    if (Key = Char(VK_RETURN)) then
    begin
      frmParent := GetParentForm(Self);
      SendMessage(frmParent.Handle, WM_NEXTDLGCTL, 0, 0);
      Key := #0;
    end
  end
  else
  begin
    MessageBeep(0);
    Key := #0;
  end;

  inherited KeyPress(Key);
end;

function TEditTyped.GetValue: Variant;
begin
  Result := ConvertValue(Text);
end;

function TEditTyped.IsChanged: Boolean;
begin
  Result := (Text <> FOldText);
end;

initialization 
  dllShlwApi := LoadLibrary('shlwapi.dll');
  if dllShlwApi <> 0 then
    @ShAutoComplete := GetProcAddress(dllShlwApi, 'SHAutoComplete');

finalization
  if dllShlwApi <> 0 then
    FreeLibrary(dllShlwApi);

end.
