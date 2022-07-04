
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

unit EsEdCalc;
  {-numeric edit field with popup calculator}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, Forms, Graphics, Menus, Messages,
  StdCtrls, SysUtils,
  EsBase, EsCalc, EsConst, EsEdPop;

type
  TEsCustomNumberEdit = class(TEsEdPopup)
  protected {private}
    {.Z+}
    FAllowIncDec     : Boolean;
    FPopupCalcColors : TEsCalcColors;
    FPopupCalcFont   : TFont;
    FPopupCalcHeight : Integer;
    FPopupCalcWidth  : Integer;

    {internal variables}
    Calculator       : TEsCalculator;
    HoldCursor       : TCursor;                                        {!!.04}
    WasAutoScroll    : Boolean;

    {property methods}
    function GetAsFloat : Double;
    function GetAsInteger : LongInt;
    function GetAsString : string;
    procedure SetAsFloat(Value : Double);
    procedure SetAsInteger(Value : LongInt);
    procedure SetAsString(const Value : string);

    {property methods}
    function GetReadOnly : Boolean;
    procedure SetPopupCalcFont(Value : TFont);
    procedure SetReadOnly(Value : Boolean);

    {internal methods}
    procedure PopupButtonPressed(Sender : TObject; Button : TEsCalculatorButton);
    procedure PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure PopupKeyPress(Sender : TObject; var Key : Char);
    procedure PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    {.Z-}

  protected
    {.Z+}
    procedure DoExit;
      override;
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure PopupClose(Sender : TObject);
      override;
    {.Z-}

    property AllowIncDec : Boolean
      read FAllowIncDec
      write FAllowIncDec
      default False;

    property PopupCalcColors : TEsCalcColors
      read FPopupCalcColors
      write FPopupCalcColors;

    property PopupCalcFont : TFont
      read FPopupCalcFont
      write SetPopupCalcFont;

    property PopupCalcHeight : Integer
      read FPopupCalcHeight
      write FPopupCalcHeight
      default calcDefHeight;

    property PopupCalcWidth : Integer
      read FPopupCalcWidth
      write FPopupCalcWidth
      default calcDefWidth;

    property ReadOnly : Boolean
      read GetReadOnly
      write SetReadOnly;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PopupOpen;                                               {!!.05}
      override;
    {.Z-}

    property AsInteger : LongInt
      read GetAsInteger
      write SetAsInteger;

    property AsFloat : Double
      read GetAsFloat
      write SetAsFloat;

    property AsString : string
      read GetAsString
      write SetAsString;
  end;

  TEsNumberEdit = class(TEsCustomNumberEdit)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property AllowIncDec;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EsLabelInfo;
    property Font;
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupCalcColors;
    property PopupCalcFont;
    property PopupCalcHeight;
    property PopupCalcWidth;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property Version;
    property Visible;

    {events}
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
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF Win32}
  end;


implementation


{*** TEsCustomNumberEdit ***}

constructor TEsCustomNumberEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FAllowIncDec := False;
  FPopupCalcHeight := calcDefHeight;
  FPopupCalcWidth := calcDefWidth;
  FPopupCalcFont := TFont.Create;
  FPopupCalcFont.Assign(Font);

  {load button glyph}
  FButton.Glyph.Handle := LoadBitmap(HInstance, 'ESSMALLDOWNARROW');

  {create color class}
  FPopupCalcColors := TEsCalcColors.Create;
  {assign default color scheme}
  FPopupCalcColors.FCalcColors := CalcScheme[csWindows];
  FPopupCalcColors.FColorScheme := csWindows;

end;

destructor TEsCustomNumberEdit.Destroy;
begin
  FPopupCalcColors.Free;
  FPopupCalcColors := nil;

  FPopupCalcFont.Free;
  FPopupCalcFont := nil;

  inherited Destroy;
end;

procedure TEsCustomNumberEdit.DoExit;
begin
  if not PopupActive then
    inherited DoExit;
end;

{!!.04} {revised}
function TEsCustomNumberEdit.GetAsFloat : Double;
var
  I : Integer;
  S : string;
begin
  S := Text;
  for I := Length(S) downto 1 do
    if not (S[I] in ['0'..'9', '+', '-', DecimalSeparator]) then
      Delete(S, I, 1);
  { catch any mal-formed text }                                       {!!.10}
  try                                                                 {!!.10}
    Result := StrToFloat(S);                                          {!!.10}
  except                                                              {!!.10}
    Result := 0.0;                                                    {!!.10}
  end;                                                                {!!.10}
end;

function TEsCustomNumberEdit.GetAsInteger : LongInt;
begin
  Result := Trunc(GetAsFloat);
end;

function TEsCustomNumberEdit.GetAsString : string;
begin
  Result := Text;
end;

function TEsCustomNumberEdit.GetReadOnly : Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TEsCustomNumberEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_DOWN) and (ssAlt in Shift) then
    PopupOpen;
end;

procedure TEsCustomNumberEdit.KeyPress(var Key : Char);
var
  D : Double;
  X : Integer;
  L : Integer;
begin
  inherited KeyPress(Key);

  if not (Key in [#27, '0'..'9', '.', DecimalSeparator, #8, '+', '-']) then begin
    Key := #0;
    MessageBeep(0);
  end;

  if FAllowIncDec  and (Key in ['+', '-']) then begin
    if Text = '' then
      Text := '0';
    D := StrToFloat(Text);
    X := SelStart;
    L := SelLength;

    if Key = '+' then
      Text := FloatToStr(D+1)
    else {'-'}
      Text := FloatToStr(D-1);

    SelStart := X;
    SelLength := L;

    Key := #0; {clear key}
  end;

end;

procedure TEsCustomNumberEdit.PopupButtonPressed(Sender : TObject;
          Button : TEsCalculatorButton);
begin
  case Button of
    ccEqual :
      begin
        {get the current value}
        Text := FloatToStr(Calculator.Value);
        Modified := True;                                              {!!.04}

        {hide the calculator}
        PopupClose(Sender);
        SetFocus;
        SelStart := Length(Text);
        SelLength := 0;
      end;
  end;
end;

procedure TEsCustomNumberEdit.PopupClose(Sender : TObject);
begin
  inherited PopupClose(Sender);

  if GetCapture = Calculator.Handle then
    ReleaseCapture;

  SetFocus;                                                            {!!.05}
  Calculator.Hide;  {hide the calculator}

  if (Calculator.Parent <> nil) then                                   {!!.06}
    if (Calculator.Parent is TForm) then                               {!!.05}
      TForm(Calculator.Parent).AutoScroll := WasAutoScroll
    else if (Calculator.Parent is TScrollBox) then                     {!!.06}
      TScrollBox(Calculator.Parent).AutoScroll := WasAutoScroll;       {!!.06}

  Cursor := HoldCursor;                                                {!!.04}

  {change parentage so that we control the window handle destruction}  {!!.04}
  Calculator.Parent := Self;                                           {!!.04}
end;

procedure TEsCustomNumberEdit.PopupKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
var
  X : Integer;
begin
  case Key of
    VK_UP : if Shift = [ssAlt] then begin
              PopupClose(Sender);
              X := SelStart;
              SetFocus;
              SelStart := X;
              SelLength := 0;
            end;
  end;
end;

procedure TEsCustomNumberEdit.PopupKeyPress(Sender : TObject; var Key : Char);
var
  X : Integer;
begin
  case Key of
    #27 :
      begin
        PopupClose(Sender);
        X := SelStart;
        SetFocus;
        SelStart := X;
        SelLength := 0;
      end;
  end;
end;

procedure TEsCustomNumberEdit.PopupMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  P : TPoint;
  I : Integer;
begin
  P := Point(X,Y);
  if not PtInRect(Calculator.ClientRect, P) then
    PopUpClose(Sender);

  {convert to our coordinate system}
  P := ScreenToClient(Calculator.ClientToScreen(P));

  if PtInRect(ClientRect, P) then begin
    I := SelStart;
    SetFocus;
    SelStart := I;
    SelLength := 0;
  end;
end;

procedure TEsCustomNumberEdit.PopupOpen;
var
  P : TPoint;
  {$IFDEF Win32}
  R : TRect;                                                           {!!.04}
  {$ENDIF}
begin
  inherited PopupOpen;

  if not Assigned(Calculator) then begin
    Calculator := TEsCalculator.CreateEx(Self, True);
    Calculator.OnButtonPressed := PopupButtonPressed;
    Calculator.OnExit := PopupClose;
    Calculator.OnKeyDown := PopupKeyDown;
    Calculator.OnKeyPress := PopupKeyPress;
    Calculator.OnMouseDown := PopupMouseDown;
    Calculator.Visible := False; {to avoid flash at 0,0}
    Calculator.ShowMemoryButtons := False;
    Calculator.BorderStyle := bsSingle;
    Calculator.Height := FPopupCalcHeight;
    Calculator.Width := FPopupCalcWidth;
    Calculator.ParentCtl3D := False;                                   {!!.02}
    Calculator.Ctl3D := Ctl3D;                                         {!!.02}
    Calculator.Font.Assign(FPopupCalcFont);
  end;
  {!!.05}
  if (Parent.Parent <> nil) then                                     {!!.08}
    Calculator.Parent := Parent.Parent                               {!!.08}
  else if Parent <> nil then                                         {!!.08}
    Calculator.Parent := Parent
  else
    Calculator.Parent := GetParentForm(Self);

  if (Calculator.Parent <> nil) then                                   {!!.06}
    if (Calculator.Parent is TForm) then begin                         {!!.05}
      WasAutoScroll := TForm(Calculator.Parent).AutoScroll;
      TForm(Calculator.Parent).AutoScroll := False;
    end else if (Calculator.Parent is TScrollBox) then begin           {!!.06}
      WasAutoScroll := TScrollBox(Calculator.Parent).AutoScroll;       {!!.06}
      TScrollBox(Calculator.Parent).AutoScroll := False;               {!!.06}
    end;                                                               {!!.06}

  {set colors}
  Calculator.Colors.Assign(FPopupCalcColors);

  {determine the proper position}
  {$IFDEF Win32}
  P := ClientToScreen(Point(-2, Height-2));
  {$ELSE}
  P := ClientToScreen(Point(0, Height));
  {$ENDIF}

  {!!.04}
  {$IFDEF Win32}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  if P.Y + Calculator.Height >= R.Bottom then
    P.Y := P.Y - Calculator.Height - Height - 2;
  if P.X + Calculator.Width >= R.Right then
    P.X := R.Right - Calculator.Width - 1;
  {$ELSE}
  if P.Y + Calculator.Height >= Screen.Height then
    P.Y := P.Y - Calculator.Height - Height - 2;
  if P.X + Calculator.Width >= Screen.Width then
    P.X := Screen.Width - Calculator.Width - 1;
  {$ENDIF}

  MoveWindow(Calculator.Handle, P.X, P.Y, Calculator.Width, Calculator.Height, False);

  HoldCursor := Cursor;                                                {!!.04}
  Cursor := crArrow;                                                   {!!.04}
  Calculator.Show;
  Calculator.SetFocus;

  SetCapture(Calculator.Handle);
end;

procedure TEsCustomNumberEdit.SetAsFloat(Value : Double);
begin
  Text := FloatToStr(Value);
end;

procedure TEsCustomNumberEdit.SetAsInteger(Value : LongInt);
begin
  Text := IntToStr(Value);
end;

procedure TEsCustomNumberEdit.SetAsString(const Value : string);
begin
  Text := Value;
end;

procedure TEsCustomNumberEdit.SetPopupCalcFont(Value : TFont);
begin
  if Assigned(Value) then
    FPopupCalcFont.Assign(Value);
end;

procedure TEsCustomNumberEdit.SetReadOnly(Value : Boolean);
begin
  inherited ReadOnly := Value;
  FButton.Enabled := not ReadOnly;
end;

end.
