
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsCalc;
  {-calculator component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, ClipBrd, Controls, ExtCtrls, Forms, Graphics,
  Menus, Messages, StdCtrls, SysUtils,
  EsBase, EsConst, EsData, EsUtil;

const
  {$IFDEF Win32}
  calcDefBorderStyle       = bsNone;
  {$ELSE}
  calcDefBorderStyle       = bsSingle;
  {$ENDIF Win32}
  calcDefColor             = clBtnFace;                                {!!.01}
  {$IFDEF Win32}
  calcDefHeight            = 140;
  {$ELSE}
  calcDefHeight            = 160;
  {$ENDIF Win32}
  calcDefShowMemoryButtons = True;
  calcDefTabStop           = True;
  calcDefWidth             = 200;

type
  TEsCalculatorButton = (
    ccNone, ccBack, ccClearEntry, ccClear, ccAdd, ccSub, ccMul, ccDiv,
    cc0, cc1, cc2, cc3, cc4, cc5,  cc6, cc7, cc8, cc9,
    ccDecimal, ccEqual, ccInvert, ccChangeSign, ccPercent, ccSqrt,
    ccMemClear, ccMemRecall, ccMemStore, ccMemAdd, ccMemSub);

  TEsButtonInfo = packed record
    Position : TRect;      {position and size}
    Caption  : string[10]; {button text}
    Visible  : Boolean;    {true to display button}
  end;

  TEsButtonArray = array[ccBack..ccMemSub] of TEsButtonInfo;
  TEsCalcState = (csValid, csLocked, csClear);                         {!!.03}

type
  TEsCalcColorArray = array[0..7] of TColor;
  TEsCalcColorScheme = (csCustom, csWindows, csDark, csOcean, csPlain);
  TEsCalcSchemeArray = array[TEsCalcColorScheme] of TEsCalcColorArray;

const
  {DisabledMemoryButtons, Display, DisplayText, EditButtons,
   FunctionButtons, MemoryButtons, NumberButtons, OperatorButtons}
  CalcScheme : TEsCalcSchemeArray =
    ((0, 0, 0, 0, 0, 0, 0, 0),
     (clGray, clWindow, clWindowText, clMaroon, clNavy, clRed,  clBlue,   clRed),
     (clGray, clBlack,  clAqua,       clBlack,  clTeal, clNavy, clMaroon, clBlue),
     (clGray, clAqua,   clBlack,      clPurple, clNavy, clNavy, clAqua,   clBlue),
     (clGray, clWhite,  clNavy,       clBlack,  clNavy, clNavy, clBlue,   clBlue)
    );

type
  TEsCalcColors = class(TPersistent)
  private
    {.Z+}
    {property variables}
    FUpdating     : Boolean;
    FOnChange     : TNotifyEvent;

    {internal variables}
    SettingScheme : Boolean;

    {internal methods}
    procedure DoOnChange;

    {property methods}
    function GetColor(Index : Integer) : TColor;
    procedure SetColor(Index : Integer; Value : TColor);
    procedure SetColorScheme(Value : TEsCalcColorScheme);
    procedure SetDisplayText(Value : TColor);
    {.Z-}
  public
    {.Z+}
    {property variables}
    FCalcColors   : TEsCalcColorArray;
    FColorScheme  : TEsCalcColorScheme;

    procedure Assign(Source : TPersistent);
      override;
    procedure BeginUpdate;
    procedure EndUpdate;

    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;
    {.Z-}

  published
    property ColorScheme : TEsCalcColorScheme
      read FColorScheme
      write SetColorScheme;

    property DisabledMemoryButtons : TColor index 0
      read GetColor
      write SetColor;

    property Display : TColor index 1
      read GetColor
      write SetColor;

    property DisplayText : TColor
      read FCalcColors[2]
      write SetDisplayText
      nodefault;

    property EditButtons : TColor index 3
      read GetColor
      write SetColor;

    property FunctionButtons : TColor index 4
      read GetColor
      write SetColor;

    property MemoryButtons : TColor index 5
      read GetColor
      write SetColor;

    property NumberButtons : TColor index 6
      read GetColor
      write SetColor;

    property OperatorButtons : TColor index 7
      read GetColor
      write SetColor;
  end;

type
  {.Z+}
  TEsCalcPanel = class(TPanel)
  protected
    procedure Click;
      override;
  public
  end;
  {.Z-}

type
  TButtonPressedEvent = procedure(Sender : TObject; Button : TEsCalculatorButton)
    of object;

  TEsCustomCalculator = class(TEsBase)
  protected {private}
    {.Z+}
    {property variables}
    FBorderStyle        : TBorderStyle;
    FColors             : TEsCalcColors;
    FShowMemoryButtons  : Boolean;

    {event variables}
    FOnButtonPressed    : TButtonPressedEvent;

    {internal variables}
    cButtons            : TEsButtonArray;
    cDisplay            : Extended;     {the calculated value}
    cDisplayStr         : string;       {the string that is displayed}
    cDownButton         : TEsCalculatorButton;
    cLastButton         : TEsCalculatorButton;
    cMargin             : Integer;
    cMemory             : Extended;     {value stored in memory register}
    cOperand            : Extended;     {the operand}
    cOperation          : TEsCalculatorButton;
    cPanel              : TEsCalcPanel;
    cState              : set of TEsCalcState;                         {!!.03}
    cPopup              : Boolean;      {true if being created as a popup}

    {internal methods}
    procedure cAdjustHeight;
    procedure cCalculateLook;
    procedure cClearAll;
    procedure cColorChange(Sender : TObject);
    procedure cDisplayError;
    procedure cDisplayValue(const Value : Extended);
    procedure cDrawCalcButton(Button : TEsButtonInfo; Pressed : Boolean);
    procedure cDrawFocusState;
    procedure cEvaluate;
    procedure cInvalidateIndicator;

    {property methods}
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure SetShowMemoryButtons(Value : Boolean);

    {VCL control methods}
    procedure CMCtl3DChanged(var Msg : TMessage);
      message CM_CTL3DCHANGED;
    procedure CMEnter(var Msg : TMessage);
      message CM_ENTER;
    procedure CMExit(var Msg : TMessage);
      message CM_EXIT;
    procedure CMFontChanged(var Msg : TMessage);
      message CM_FONTCHANGED;

    {windows message handlers}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd);
      message WM_ERASEBKGND;
    procedure WMGetText(var Msg : TWMGetText);
      message WM_GETTEXT;
    procedure WMGetTextLength(var Msg : TWMGetTextLength);
      message WM_GETTEXTLENGTH;
    procedure WMKeyDown(var Msg : TWMKeyDown);
      message WM_KEYDOWN;
    procedure WMSetText(var Msg : TWMSetText);
      message WM_SETTEXT;
    {.Z-}

  protected
    {.Z+}
    procedure CreateParams(var Params : TCreateParams);
      override;
    procedure CreateWnd;
      override;
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure Paint;
      override;
    {.Z-}

    {protected properties}
    property BorderStyle : TBorderStyle
      read FBorderStyle
      write SetBorderStyle
      default calcDefBorderStyle;

    property ShowMemoryButtons : Boolean
      read FShowMemoryButtons
      write SetShowMemoryButtons
      default calcDefShowMemoryButtons;

    {protected events}
    property OnButtonPressed : TButtonPressedEvent
      read FOnButtonPressed
      write FOnButtonPressed;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    constructor CreateEx(AOwner : TComponent; AsPopup : Boolean);
      virtual;
    destructor Destroy;
      override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
      override;
    {.Z-}

    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure PressButton(Button : TEsCalculatorButton);

    {public properties}
    property Colors : TEsCalcColors
      read FColors
      write FColors;

    property Memory : Extended {run-time}
      read cMemory
      write cMemory;

    property Text; {run-time}

    property Value : Extended  {run-time}
      read cDisplay;
  end;

  TEsCalculator = class(TEsCustomCalculator)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
   property Font;  {must be prior to "Colors"}
    property Align;
    property BorderStyle;
    property Ctl3D;
    property Colors;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EsLabelInfo;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowMemoryButtons;
    property TabOrder;
    property TabStop default calcDefTabStop;
    property Version;
    property Visible;

    {events}
    property OnButtonPressed;
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
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF Win32}
  end;


implementation


{*** TEsCalcColors ***}

procedure TEsCalcColors.Assign(Source : TPersistent);
begin
  if Source is TEsCalcColors then begin
    FCalcColors := TEsCalcColors(Source).FCalcColors;
    FColorScheme := TEsCalcColors(Source).FColorScheme;
  end else
    inherited Assign(Source);
end;

procedure TEsCalcColors.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TEsCalcColors.EndUpdate;
begin
  FUpdating := False;
  DoOnChange;
end;

procedure TEsCalcColors.DoOnChange;
begin
  if not FUpdating and Assigned(FOnChange) then
    FOnChange(Self);

  if not SettingScheme then
    FColorScheme := csCustom;
end;

function TEsCalcColors.GetColor(Index : Integer) : TColor;
begin
  Result := FCalcColors[Index];
end;

procedure TEsCalcColors.SetColor(Index : Integer; Value : TColor);
begin
  if Value <> FCalcColors[Index] then begin
    FCalcColors[Index] := Value;
    DoOnChange;
  end;
end;

procedure TEsCalcColors.SetColorScheme(Value : TEsCalcColorScheme);
begin
  if Value <> FColorScheme then begin
    SettingScheme := True;
    try
      FColorScheme := Value;
      if Value <> csCustom then begin
        FCalcColors := CalcScheme[Value];
        DoOnChange;
      end;
    finally
      SettingScheme := False;
    end;
  end;
end;

procedure TEsCalcColors.SetDisplayText(Value : TColor);
begin
  if Value <> FCalcColors[2] then begin
    FCalcColors[2] := Value;
    DoOnChange;
  end;
end;


{*** TEsCalcPanel ***}

procedure TEsCalcPanel.Click;
begin
  (Owner as TEsCustomCalculator).SetFocus;
end;


{*** TEsCustomCalculator ***}

procedure TEsCustomCalculator.cAdjustHeight;
var
  DC         : hDC;
  SaveFont   : hFont;
  I          : Integer;
  SysMetrics : TTextMetric;
  Metrics    : TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  cPanel.Height := Metrics.tmHeight + I;
end;

procedure TEsCustomCalculator.cCalculateLook;
var
  CW  : Integer;  {client width}
  BW  : Integer;  {button width}
  BH  : Integer;  {button height}
  LBW : Integer;  {large button width}
  M1  : Integer;  {margin between buttons}
  M2  : Integer;  {left and right edge margins}
  M3  : Integer;  {margin between panel and frst row of buttons}
  M4  : Integer;  {margin between memory buttons and other buttons}
  TM  : Integer;  {area where the panel is placed}
  X   : Integer;
  Y   : Integer;
  PW  : Integer;  {panel width}
  B   : TEsCalculatorButton;
begin
  if not HandleAllocated then
    Exit;

  {set panel height based on font}
  cAdjustHeight;

  for B := Low(cButtons) to High(cButtons) do
    cButtons[B].Visible := True;

  CW := ClientWidth;

  if Width <= 200 then begin
    M1 := 2;
    M2 := 4;
  end else begin
    M1 := 4;
    M2 := 6;
  end;
  {save left/right/top/bottom margin value}
  cMargin := M2;

  M4 := M2;
  if FShowMemoryButtons then begin
    BW := (CW - 3*M2 - 4*M1) div 6;
    M4 := CW - 2*M2 - 6*BW - 4*M1;
  end else begin
    BW := (CW - 2*M2 - 4*M1) div 5;
    if (CW - 2*M2 - 4*M1) div 6 >= 4 then
      Inc(M2, 2)
    else if (CW - 2*M2 - 4*M1) div 6 >= 2 then
      Inc(M2, 1);
  end;

  {button height, using an estimate for TM}
  TM := M2 + M2 + cPanel.Height;
  BH := (ClientHeight - TM - M2 - 4*M1) div 5;

  {calculate actual area below panel}
  M3 := ClientHeight - M2 - cPanel.Height - 5*BH - 4*M1 - M2;

  {calculate actual height of area above buttons}
  TM := M2 + M3 + cPanel.Height;

  {large button width}
  LBW := (4*BW + 3*M1 - 2*M1) div 3;

  {calculate the width of the edit window}
  cMargin := M2;
  if FShowMemoryButtons then
    PW := 6*BW + M4 + 4*M1
  else
    PW := 5*BW + 4*M1;

  {position edit control}
  cPanel.SetBounds(cMargin, cMargin, PW, cPanel.Height);

  {memory column}
  if FShowMemoryButtons then begin
    X := M2;
    Y := TM;
    cButtons[ccMemClear].Position := Rect(X, Y, X+BW, Y+BH);
    cButtons[ccMemClear].Caption := 'MC';

    Y := TM + BH + M1;
    cButtons[ccMemRecall].Position := Rect(X, Y, X+BW, Y+BH);
    cButtons[ccMemRecall].Caption := 'MR';

    Y := TM + 2*BH + 2*M1;
    cButtons[ccMemStore].Position := Rect(X, Y, X+BW, Y+BH);
    cButtons[ccMemStore].Caption := 'MS';

    Y := TM + 3*BH + 3*M1;
    cButtons[ccMemAdd].Position := Rect(X, Y, X+BW, Y+BH);
    cButtons[ccMemAdd].Caption := 'M+';

    Y := TM + 4*BH + 4*M1;
    cButtons[ccMemSub].Position := Rect(X, Y, X+BW, Y+BH);
    cButtons[ccMemSub].Caption := 'M-';
  end else
    for B := ccMemClear to ccMemSub do
      cButtons[B].Visible := False;

  {row 1 - large buttons}
  Y := TM;
  if FShowMemoryButtons then
    X := 2*BW + M4 + M2 + M1
  else
    X := BW + M2 + M1;
  cButtons[ccBack].Position := Rect(X, Y, X+LBW, Y+BH);
  cButtons[ccBack].Caption := 'Back';

  Inc(X, LBW+M1);
  cButtons[ccClearEntry].Position := Rect(X, Y, X+LBW, Y+BH);
  cButtons[ccClearEntry].Caption := 'CE';

  Inc(X, LBW+M1);
  cButtons[ccClear].Position := Rect(X, Y, X+LBW, Y+BH);
  cButtons[ccClear].Caption := 'C';

  {row 2}
  Y := TM + BH + M1;
  if FShowMemoryButtons then
    X := M2 + BW + M4
  else
    X := M2;
  cButtons[cc7].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc7].Caption := '7';

  Inc(X, BW+M1);
  cButtons[cc8].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc8].Caption := '8';

  Inc(X, BW+M1);
  cButtons[cc9].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc9].Caption := '9';

  Inc(X, BW+M1);
  cButtons[ccDiv].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccDiv].Caption := '/';

  Inc(X, BW+M1);
  cButtons[ccSqrt].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccSqrt].Caption := 'Sqrt';

  {row 3}
  Y := TM + 2*BH + 2*M1;
  if FShowMemoryButtons then
    X := M2 + BW + M4
  else
    X := M2;
  cButtons[cc4].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc4].Caption := '4';

  Inc(X, BW+M1);
  cButtons[cc5].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc5].Caption := '5';

  Inc(X, BW+M1);
  cButtons[cc6].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc6].Caption := '6';

  Inc(X, BW+M1);
  cButtons[ccMul].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccMul].Caption := '*';

  Inc(X, BW+M1);
  cButtons[ccPercent].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccPercent].Caption := '%';

  {row 4}
  Y := TM + 3*BH + 3*M1;
  if FShowMemoryButtons then
    X := M2 + BW + M4
  else
    X := M2;
  cButtons[cc1].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc1].Caption := '1';

  Inc(X, BW+M1);
  cButtons[cc2].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc2].Caption := '2';

  Inc(X, BW+M1);
  cButtons[cc3].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc3].Caption := '3';

  Inc(X, BW+M1);
  cButtons[ccSub].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccSub].Caption := '-';

  Inc(X, BW+M1);
  cButtons[ccInvert].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccInvert].Caption := '1/x';

  {row 5}
  Y := TM + 4*BH + 4*M1;
  if FShowMemoryButtons then
    X := M2 + BW + M4
  else
    X := M2;
  cButtons[cc0].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[cc0].Caption := '0';

  Inc(X, BW+M1);
  cButtons[ccChangeSign].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccChangeSign].Caption := '+/-';

  Inc(X, BW+M1);
  cButtons[ccDecimal].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccDecimal].Caption := DecimalSeparator;

  Inc(X, BW+M1);
  cButtons[ccAdd].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccAdd].Caption := '+';

  Inc(X, BW+M1);
  cButtons[ccEqual].Position := Rect(X, Y, X+BW, Y+BH);
  cButtons[ccEqual].Caption := '=';
end;

procedure TEsCustomCalculator.cColorChange(Sender : TObject);
begin
  {update panel background color}
  if Assigned(cPanel) then begin
    cPanel.Color := FColors.Display;
    cPanel.Font.Color := FColors.DisplayText;
    {update the main font color}
    if not (csLoading in ComponentState) and (Font <> nil) then
      Font.Color := FColors.DisplayText;
  end;

  Invalidate;
end;

procedure TEsCustomCalculator.cDisplayError;
begin
  cPanel.Caption := '****** ';
  cState := [csLocked]; {user will have to clear this}
  MessageBeep(0);
end;

procedure TEsCustomCalculator.cDisplayValue(const Value : Extended);
begin
  try
    if cPanel.HandleAllocated then
      cPanel.Caption := Format('%g',[Value]) + ' ';
  except
    cDisplayError;
  end;
end;

procedure TEsCustomCalculator.cClearAll;
begin
  cDisplay := 0;
  cOperand := 0;
  cOperation := ccEqual;
  cDisplayStr := '0';
  cDisplayValue(cDisplay);
  cState := [csValid, csClear];
end;

procedure TEsCustomCalculator.cDrawCalcButton(Button : TEsButtonInfo; Pressed : Boolean);
var
  TR  : TRect;
  Buf : array[0..255] of Char;
begin
  if Button.Visible then begin
    TR := DrawButtonFace(Canvas, Button.Position, 1, bsNew, False, Pressed, False);
    StrPLCopy(Buf, Button.Caption, 255);
    DrawText(Canvas.Handle, Buf, Length(Button.Caption), TR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

    if Focused and (Button.Caption = '=') then
      cDrawFocusState;
  end;
end;

procedure TEsCustomCalculator.cDrawFocusState;
var
  R : TRect;
begin
  R := cButtons[ccEqual].Position;
  InflateRect(R, -3, -3);
  Canvas.DrawFocusRect(R);
end;

procedure TEsCustomCalculator.cEvaluate;
begin
  if csValid in cState then begin
    try
      {evaluate the expression}
      case cOperation of
        ccAdd : cDisplay := cDisplay + cOperand;
        ccSub : cDisplay := cDisplay - cOperand;
        ccMul : cDisplay := cDisplay * cOperand;
        ccDiv : cDisplay := cDisplay / cOperand;
        ccEqual : cDisplay := cOperand;
      end;
      cDisplayValue(cDisplay);
      cDisplayStr := '0';
      cOperand := 0;
    except
      cDisplayError;
    end;
  end;
end;

procedure TEsCustomCalculator.cInvalidateIndicator;
begin
  InvalidateRect(Handle, @cButtons[ccMemRecall].Position, False);
  InvalidateRect(Handle, @cButtons[ccMemClear].Position, False);
end;

procedure TEsCustomCalculator.CMCtl3DChanged(var Msg : TMessage);
begin
  inherited;

  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  {$IFDEF Win32}
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  {$ENDIF}

  Invalidate;
end;

procedure TEsCustomCalculator.CMEnter(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  {invalidate the "=" button to ensure that the focus rect is painted}
  R := cButtons[ccEqual].Position;
  InvalidateRect(Handle, @R, False);
end;

procedure TEsCustomCalculator.CMExit(var Msg : TMessage);
var
  R : TRect;
begin
  inherited;

  {invalidate the "=" button to ensure that the focus rect is painted}
  R := cButtons[ccEqual].Position;
  InvalidateRect(Handle, @R, False);
end;

procedure TEsCustomCalculator.CMFontChanged(var Msg : TMessage);
begin
  inherited;

  if not (csLoading in ComponentState) and Assigned(cPanel) then begin
    cPanel.Color := FColors.Display;
    cPanel.Font.Color := FColors.DisplayText;
    FColors.FCalcColors[2] := Font.Color;
  end;

  cCalculateLook;
  Invalidate;
end;

procedure TEsCustomCalculator.CopyToClipboard;
begin
  Clipboard.AsText := Text;
end;

constructor TEsCustomCalculator.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  if cPopup then
    ControlStyle := ControlStyle + [csClickEvents, csFramed] - [csCaptureMouse]
  else
    ControlStyle := ControlStyle + [csClickEvents, csFramed, csCaptureMouse];

  Color      := calcDefColor;
  Height     := calcDefHeight;
  TabStop    := calcDefTabStop;
  Width      := calcDefWidth;

  {create edit control}
  cPanel := TEsCalcPanel.Create(Self);
  cPanel.Parent := Self;
  cPanel.ParentFont := True;
  cPanel.ParentCtl3D := True;
  cPanel.Alignment := taRightJustify;
  cPanel.BevelOuter := bvLowered;
  cPanel.BorderStyle := bsNone;
  cPanel.Color := clWindow;
  cPanel.BevelWidth := 2;
  cPanel.Caption := '0 ';

  {set property defaults}
  FBorderStyle       := calcDefBorderStyle;
  FShowMemoryButtons := calcDefShowMemoryButtons;

  FColors := TEsCalcColors.Create;
  FColors.OnChange := cColorChange;

  {assign default color scheme}
  FColors.FCalcColors := CalcScheme[csWindows];

  cLastButton := ccEqual;

end;

constructor TEsCustomCalculator.CreateEx(AOwner : TComponent; AsPopup : Boolean);
begin
  cPopup := AsPopup;
  Create(AOwner);
end;

procedure TEsCustomCalculator.CreateParams(var Params : TCreateParams);
const
  BorderStyles : array[TBorderStyle] of DWord = (0, WS_BORDER);        {!!.05}
begin
  inherited CreateParams(Params);

  with Params do begin
    Style := Style or BorderStyles[FBorderStyle];
    {!!.02} {block revised}
    if cPopup then begin
      Style := WS_POPUP or WS_BORDER;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
      {$IFDEF Win32}
      Ctl3D := False;
      if NewStyleControls then
        ExStyle := WS_EX_TOOLWINDOW or WS_EX_CLIENTEDGE;
      {$ENDIF Win32}
    end;
  end;

  {$IFDEF Win32}
  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  {$ENDIF}
end;

procedure TEsCustomCalculator.CreateWnd;
begin
  inherited CreateWnd;

  cCalculateLook;
  cClearAll;

  cPanel.Color := FColors.Display;
end;

destructor TEsCustomCalculator.Destroy;
begin
  cPanel.Free;
  cPanel := nil;

  FColors.Free;
  FColors := nil;

  inherited Destroy;
end;

procedure TEsCustomCalculator.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case Key of
    VK_DELETE : if Shift = [] then
                  PressButton(ccClearEntry);
    VK_F9     : if Shift = [] then
                  PressButton(ccChangeSign);
  end;
end;

procedure TEsCustomCalculator.KeyPress(var Key : Char);
begin
  inherited KeyPress(Key);

  case Key of
    '0' : PressButton(cc0);
    '1' : PressButton(cc1);
    '2' : PressButton(cc2);
    '3' : PressButton(cc3);
    '4' : PressButton(cc4);
    '5' : PressButton(cc5);
    '6' : PressButton(cc6);
    '7' : PressButton(cc7);
    '8' : PressButton(cc8);
    '9' : PressButton(cc9);

    '+' : PressButton(ccAdd);
    '-' : PressButton(ccSub);
    '*' : PressButton(ccMul);
    '/' : PressButton(ccDiv);

    '.' : PressButton(ccDecimal);
    '=' : PressButton(ccEqual);
    'r' : PressButton(ccInvert);
    '%' : PressButton(ccPercent);
    '@' : PressButton(ccSqrt);

    ^L  : PressButton(ccMemClear);  {^L}
    ^R  : PressButton(ccMemRecall); {^R}
    ^P  : PressButton(ccMemAdd);    {^P}
    ^S  : PressButton(ccMemSub);    {^S}

    ^C  : CopyToClipboard;          {^C}{copy}
    ^V  : PasteFromClipboard;       {^V}{paste}

    #8  : PressButton(ccBack);      {backspace}
    #27 : PressButton(ccClear);     {esc}
  else
    if Key = DecimalSeparator then
      PressButton(ccDecimal);
  end;
end;

procedure TEsCustomCalculator.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  B : TEsCalculatorButton;
begin
  SetFocus;

  if Button = mbLeft then begin
    cDownButton := ccNone;
    for B := Low(cButtons) to High(cButtons) do
      if PtInRect(cButtons[B].Position, Point(X,Y)) then begin
        if (B in [ccMemClear, ccMemRecall]) and (cMemory = 0) then
          Exit;
        cDownButton := B;
        InvalidateRect(Handle, @cButtons[cDownButton].Position, False);
        Break;
      end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TEsCustomCalculator.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if cDownButton = ccNone then
    Exit;

  InvalidateRect(Handle, @cButtons[cDownButton].Position, False);

  {if still over the button...}
  if PtInRect(cButtons[cDownButton].Position, Point(X,Y)) then
    PressButton(cDownButton);

  cDownButton := ccNone;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TEsCustomCalculator.PasteFromClipboard;
var
  I : Integer;
  C : AnsiChar;
  S : string;
begin
  S := Clipboard.AsText;
  if S > '' then begin
    cClearAll;
    for I := 1 to Length(S) do begin
      C := S[I];
      if (C in ['0'..'9', DecimalSeparator, '.', '+', '-', '*', '/', '=', '%']) then
        KeyPress(C);
    end;
  end;
end;

{!!.03} {revised}
procedure TEsCustomCalculator.PressButton(Button : TEsCalculatorButton);
var
  Ch   : AnsiChar;
  Sign : Integer;
  D    : Extended;
begin
  if not HandleAllocated then
    Exit;

  {simulate a button down if needed}
  if cDownButton = ccNone then begin
    cDownButton := Button;
    InvalidateRect(Handle, @cButtons[cDownButton].Position, False);
    Update;
  end;

  try
    case Button of
      ccClear :
        begin
          cClearAll;
          cDisplayValue(cDisplay);
        end;
    end;

    if (csLocked in cState) then begin
      MessageBeep(0);
      Exit;
    end;

    case Button of
      cc0..cc9 :
        begin
          if cLastButton = ccEqual then begin
            {clear pending operations if last command was =}
            cClearAll;
          end;

          if csClear in cState then
            cDisplayStr := '';

          cLastButton := Button;

          Ch := cButtons[Button].Caption[1];
          if cOperand < 0 then
            Sign := -1
          else
            Sign := 1;
          cDisplayStr := cDisplayStr + Ch;

          try
            D := StrToFloat(cDisplayStr) * Sign;
            cOperand := D;
            if (D <> 0) or
               (Pos(DecimalSeparator, cDisplayStr) > 0) then begin
              cPanel.Caption := cDisplayStr + ' ';
              cState := [csValid];
            end else begin
              cDisplayStr := '0';
              cDisplayValue(D);
              cState := [csValid, csClear];
            end;
          except
            cDisplayError;
          end;
        end;
      ccDecimal :
        {check if there is already a decimal separator in the string}
        if Pos(DecimalSeparator, cDisplayStr) = 0 then begin
          try
            cDisplayStr := cDisplayStr + DecimalSeparator;
            D := StrToFloat(cDisplayStr);
            cPanel.Caption := cDisplayStr + ' ';
            cOperand := D;
            cState := [csValid];
          except
            cDisplayError;
          end;
        end;
      ccAdd,
      ccSub,
      ccMul,
      ccDiv :
        begin
          cEvaluate;
          cState := [csValid, csClear];
          cOperation := Button;
        end;
      ccEqual :
        begin
          cEvaluate;
          cOperand := cDisplay;
          cState := [csValid];
          cOperation := ccEqual;
        end;
      ccBack :
        try
          if Length(cDisplayStr) > 1 then begin
            cDisplayStr := Copy(cDisplayStr, 1, Length(cDisplayStr)-1);
            cOperand := StrToFloat(cDisplayStr);
            cPanel.Caption := cDisplayStr + ' ';
          end else begin
            cOperand := 0;
            cDisplayValue(cOperand);
            cState := [csValid, csClear];
          end;
        except
          cDisplayError;
        end;
      ccClearEntry :
        begin
          cDisplayStr := '';
          cOperand := 0;
          cDisplayValue(cOperand);
        end;
      ccMemStore :
        begin
          if (cMemory = 0) and (cOperand <> 0) then
            cInvalidateIndicator;
          cMemory := cOperand;
          Include(cState, csClear);
        end;
      ccMemRecall :
        begin
          cOperand := cMemory;
          cDisplayValue(cOperand);
          cState := [csValid, csClear];
        end;
      ccMemClear :
        begin
          if cMemory <> 0 then
            cInvalidateIndicator;
          cMemory := 0;
        end;
      ccMemAdd,
      ccMemSub :
        begin
          D := cMemory;
          try
            if Button = ccMemAdd then
              cMemory := cMemory + cOperand
            else
              cMemory := cMemory - cOperand;
          except
            cDisplayError;
            cMemory := 0;
          end;
          if ((D = 0) and (cMemory <> 0)) or
             ((D <> 0) and (cMemory = 0)) then
          cInvalidateIndicator;
          Include(cState, csClear);
        end;
      ccChangeSign :
        try
          cOperand := -cOperand;
          cDisplayValue(cOperand);
        except
          cDisplayError;
        end;
      ccInvert :
        try
          cDisplayStr := '';
          cOperand := 1 / cOperand;
          cDisplayValue(cOperand);
        except
          cDisplayError;
        end;
      ccPercent :
        try
          if cOperation in [ccAdd, ccSub] then
            cOperand := (cOperand / 100) * cDisplay  {do markup/down}
          else
            cOperand := cOperand / 100;              {as a percentage}
          cEvaluate;
          cOperand := cDisplay;
          cState := [csValid, csClear];
          cOperation := ccEqual;
        except
          cDisplayError;
        end;
      ccSqrt :
        try
          cOperand := Sqrt(cOperand);
          cDisplayValue(cOperand);
        except
          cDisplayError;
        end;
    end;
  finally
    {record button press}
    cLastButton := Button;

    {simulate a button up, if the mouse button is up or we aren't focused}
    if not Focused or (GetAsyncKeyState(GetLeftButton) and $8000 = 0) then begin
      InvalidateRect(Handle, @cButtons[cDownButton].Position, False);
      cDownButton := ccNone;
      Update;
    end;
  end;

  if Assigned(FOnButtonPressed) then
    FOnButtonPressed(Self, Button);
end;

procedure TEsCustomCalculator.Paint;
var
  B  : TEsCalculatorButton;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := calcDefColor;
  Canvas.FillRect(ClientRect);

  if Ctl3D then begin
    cPanel.BevelOuter := bvLowered;
    cPanel.BorderStyle := bsNone;
  end else begin
    cPanel.BevelOuter := bvNone;
  cPanel.BorderStyle := bsSingle;
  end;

  {draw buttons}
  for B := Low(cButtons) to High(cButtons) do begin
    if (B in [ccMemClear, ccMemRecall, ccMemStore, ccMemAdd, ccMemSub]) then begin
      if (B in [ccMemClear, ccMemRecall]) and (cMemory = 0) then
        Canvas.Font.Color := FColors.DisabledMemoryButtons
      else
        Canvas.Font.Color := FColors.MemoryButtons;
    end else if (B in [ccBack, ccClearEntry, ccClear]) then
      Canvas.Font.Color := FColors.EditButtons
    else if (B in [ccAdd, ccSub, ccMul, ccDiv, ccEqual]) then
      Canvas.Font.Color := FColors.OperatorButtons
    else if (B in [cc0..cc9, ccDecimal]) then
      Canvas.Font.Color := FColors.NumberButtons
    else if (B in [ccInvert, ccChangeSign, ccPercent, ccSqrt]) then
      Canvas.Font.Color := FColors.FunctionButtons;

    cDrawCalcButton(cButtons[B], (B = cDownButton));
  end;
end;

procedure TEsCustomCalculator.SetBorderStyle(Value : TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TEsCustomCalculator.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited Setbounds(ALeft, ATop, AWidth, AHeight);

  cCalculateLook;
end;

procedure TEsCustomCalculator.SetShowMemoryButtons(Value : Boolean);
begin
  if Value <> FShowMemoryButtons then begin
    FShowMemoryButtons := Value;
    cCalculateLook;
    Invalidate;
  end;
end;

procedure TEsCustomCalculator.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;   {don't erase background, just say we did}
end;

procedure TEsCustomCalculator.WMGetText(var Msg : TWMGetText);
begin
  if not cPanel.HandleAllocated then
    Exit;

  Msg.Result := SendMessage(cPanel.Handle, WM_GETTEXT,
    TMessage(Msg).wParam, TMessage(Msg).lParam);
end;

procedure TEsCustomCalculator.WMGetTextLength(var Msg : TWMGetTextLength);
begin
  if not cPanel.HandleAllocated then
    Exit;

  Msg.Result := SendMessage(cPanel.Handle, WM_GETTEXTLENGTH,
    TMessage(Msg).wParam, TMessage(Msg).lParam);
end;

procedure TEsCustomCalculator.WMKeyDown(var Msg : TWMKeyDown);
begin
  if Msg.CharCode = Ord('M') then begin
    if (GetAsyncKeyState(VK_CONTROL) and $8000) <> 0 then begin
      PressButton(ccMemStore);
    end;
  end else if Msg.CharCode = VK_RETURN then
    PressButton(ccEqual);

  inherited;
end;

procedure TEsCustomCalculator.WMSetText(var Msg : TWMSetText);
var
  I : Integer;
  C : AnsiChar;
begin
  cClearAll;
  for I := 0 to Pred(StrLen(Msg.Text)) do begin
    C := Msg.Text[I];
    KeyPress(C);
  end;
  Msg.Result := 1{true};
end;

end.
