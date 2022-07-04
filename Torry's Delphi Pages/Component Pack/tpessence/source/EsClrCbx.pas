
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

{$IFDEF WIN32}                                                         {!!.02}
  {$J+} {Writable constants}                                           {!!.02}
{$ENDIF}                                                               {!!.02}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsClrCbx;
  {-color combobox selector}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, Forms, Graphics, Menus, Messages, StdCtrls,
  EsBase, EsConst, EsData, EsUtil;

type
  TEsCustomColorComboBox = class(TCustomComboBox)
  protected {private}
    {.Z+}
    {property Variables}
    FEsLabel        : TEsLabelInfo;
    FShowColorNames : Boolean;

    {internal variables}
    BoxWidth        : Integer;

    {property methods}
    function GetAttachedLabel : TEsAttachedLabel;
    function GetSelectedColor : TColor;
    function GetVersion : string;
    procedure SetSelectedColor(Value : TColor);
    procedure SetShowColorNames(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure CalculateBoxWidth;
    procedure LabelChange(Sender : TObject);
    procedure LabelAttach(Sender : TObject; Value : Boolean);
    procedure PositionLabel;

    {message methods}
    procedure CMFontChanged(var Message : TMessage);
      message CM_FONTCHANGED;

    procedure ESAssignLabel(var Msg : TMessage);
      message ES_ASSIGNLABEL;
    procedure ESPositionLabel(var Msg : TMessage);
      message ES_POSITIONLABEL;
    procedure ESRecordLabelPosition(var Msg : TMessage);
      message ES_RECORDLABELPOSITION;
    {.Z-}

  protected
    {.Z+}
    {descendants can set the value of this variable after calling inherited }
    {create to set the default location and point-of-reference (POR) for the}
    {attached label. if dlpTopLeft, the default location and POR will be at }
    {the top left of the control. if dlpBottomLeft, the default location and}
    {POR will be at the bottom left}
    DefaultLabelPosition : TEsLabelPosition;

    procedure CreateParams(var Params : TCreateParams);
      override;
    procedure CreateWnd;
      override;
    procedure Notification(AComponent : TComponent; Operation: TOperation);
      override;
    {.Z-}

    property EsLabelInfo : TEsLabelInfo
      read FEsLabel
      write FEsLabel;

    property SelectedColor : TColor
      read GetSelectedColor
      write SetSelectedColor
      stored False;

    property ShowColorNames : Boolean
      read FShowColorNames
      write SetShowColorNames
      default True;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure DrawItem(Index : Integer; Rect : TRect; State : TOwnerDrawState);
      override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer);       {!!.05}
      override;
    {.Z-}

    property AtachedLabel : TEsAttachedLabel
      read GetAttachedLabel;
  end;

  TEsColorComboBox = class(TEsCustomColorComboBox)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property EsLabelInfo;
    property Font;
    property Height;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property ShowColorNames;
    property ShowHint;
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
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF Win32}
  end;


implementation


procedure TEsCustomColorComboBox.CalculateBoxWidth;
var
  I : Integer;
  X : Integer;
  T : Integer;
begin
  if not HandleAllocated or (BoxWidth > 0) then
    Exit;

  if not FShowColorNames then begin
    BoxWidth := ClientWidth - 1;                                       {!!.02}
    Exit;
  end;

  Canvas.Font := Font;
  BoxWidth := 0;
  T := 0;

  {calculate width of the color box}
  for I := 0 to 15 do begin
    X := Canvas.TextWidth(Items[I]+'X');
    if X > T then
      T := X;
  end;

  BoxWidth := ClientWidth - T;
  if BoxWidth < 25 then
    BoxWidth := 25;
end;

procedure TEsCustomColorComboBox.CMFontChanged(var Message : TMessage);
begin
  inherited;

  BoxWidth := 0;
  Invalidate;
end;

constructor TEsCustomColorComboBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Style := csDropDownList;

  FShowColorNames := True;

  {set default position and reference point}
  DefaultLabelPosition := dlpTopLeft;

  FEsLabel := TEsLabelInfo.Create;
  FEsLabel.OnChange := LabelChange;
  FEsLabel.OnAttach := LabelAttach;
end;

procedure TEsCustomColorComboBox.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or CBS_OWNERDRAWFIXED;
end;

procedure TEsCustomColorComboBox.CreateWnd;
begin
  inherited CreateWnd;

  Text := '';
  Items.Clear;
  Items.AddObject(StrRes[SCEsColorBlack], TObject(clBlack));
  Items.AddObject(StrRes[SCEsColorMaroon], TObject(clMaroon));
  Items.AddObject(StrRes[SCEsColorGreen], TObject(clGreen));
  Items.AddObject(StrRes[SCEsColorOlive], TObject(clOlive));
  Items.AddObject(StrRes[SCEsColorNavy], TObject(clNavy));
  Items.AddObject(StrRes[SCEsColorPurple], TObject(clPurple));
  Items.AddObject(StrRes[SCEsColorTeal], TObject(clTeal));
  Items.AddObject(StrRes[SCEsColorGray], TObject(clGray));
  Items.AddObject(StrRes[SCEsColorSilver], TObject(clSilver));
  Items.AddObject(StrRes[SCEsColorRed], TObject(clRed));
  Items.AddObject(StrRes[SCEsColorLime], TObject(clLime));
  Items.AddObject(StrRes[SCEsColorYellow], TObject(clYellow));
  Items.AddObject(StrRes[SCEsColorBlue], TObject(clBlue));
  Items.AddObject(StrRes[SCEsColorFuchsia], TObject(clFuchsia));
  Items.AddObject(StrRes[SCEsColorAqua], TObject(clAqua));
  Items.AddObject(StrRes[SCEsColorWhite], TObject(clWhite));

  ItemIndex := 0;

end;

destructor TEsCustomColorComboBox.Destroy;
begin
  {detatch and destroy label, if any}
  FEsLabel.Visible := False;

  {destroy label info}
  FEsLabel.Free;
  FEsLabel := nil;

  inherited Destroy;
end;

procedure TEsCustomColorComboBox.DrawItem(Index : Integer; Rect : TRect;
                                 State : TOwnerDrawState);
var
  BC : TColor;
  S  : string;
begin
  {get selected color and text to display}
  if Index > -1 then begin
    S := Items[Index];
    BC := TColor(Items.Objects[Index])
  end else begin
    S := StrRes[SCEsColorBlack];
    BC := clBlack;
  end;

  CalculateBoxWidth;

  if (State * [odSelected, odFocused] <> []) then begin
    Canvas.Font.Color := clHighLightText;
    Canvas.Brush.Color := clHighLight;
  end else begin
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
  end;

  if FShowColorNames then begin
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    {Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, S);}                  {!!.03}
    Inc(Rect.Left);                                                    {!!.03}
    DrawText(Canvas.Handle, @S[1], Length(S), Rect,                    {!!.03}
      DT_LEFT or DT_VCENTER or DT_SINGLELINE);                         {!!.03}
  end;

  Canvas.Pen.Color := Font.Color;
  Canvas.Brush.Color := BC;
  Canvas.Rectangle(ClientWidth - BoxWidth, Rect.Top + 1, Rect.Right -1, Rect.Bottom - 1);
end;

procedure TEsCustomColorComboBox.ESAssignLabel(var Msg : TMessage);
begin
  FEsLabel.ALabel := TEsAttachedLabel(Msg.lParam);
end;

procedure TEsCustomColorComboBox.ESPositionLabel(var Msg : TMessage);
const
  DX : Integer = 0;
  DY : Integer = 0;
begin
  if FEsLabel.Visible and Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) and
     not (csLoading in ComponentState) then begin
    if DefaultLabelPosition = dlpTopLeft then begin
      DX := FEsLabel.ALabel.Left - Left;
      DY := FEsLabel.ALabel.Top + FEsLabel.ALabel.Height - Top;
    end else begin
      DX := FEsLabel.ALabel.Left - Left;
      DY := FEsLabel.ALabel.Top - Top - Height;
    end;
    if (DX <> FEsLabel.OffsetX) or (DY <> FEsLabel.OffsetY) then
      PositionLabel;
  end;
end;

procedure TEsCustomColorComboBox.ESRecordLabelPosition(var Msg : TMessage);
begin
  if Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) then begin
    {if the label was cut and then pasted, this will complete the reattachment}
    FEsLabel.FVisible := True;

    if DefaultLabelPosition = dlpTopLeft then
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
                          FEsLabel.ALabel.Top + FEsLabel.ALabel.Height - Top)
    else
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
                          FEsLabel.ALabel.Top - Top - Height);
  end;
end;

function TEsCustomColorComboBox.GetAttachedLabel : TEsAttachedLabel;
begin
  if not FEsLabel.Visible then
    raise EEssentialsError.Create(StrRes[SCEsLabelNotAttached]);

  Result := FEsLabel.ALabel;
end;

function TEsCustomColorComboBox.GetSelectedColor : TColor;
begin
  if ItemIndex > -1 then
    Result := TColor(Items.Objects[ItemIndex])
  else
    Result := clBlack;
end;

function TEsCustomColorComboBox.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsCustomColorComboBox.LabelAttach(Sender : TObject; Value : Boolean);
var
  PF : TForm;
begin
  if csLoading in ComponentState then
    Exit;

  PF := TForm(GetParentForm(Self));
  if Value then begin
    if Assigned(PF) then begin
      FEsLabel.ALabel.Free;
      FEsLabel.ALabel := TEsAttachedLabel.CreateEx(PF, Self);
      FEsLabel.ALabel.Parent := Parent;
      FEsLabel.ALabel.Caption := Name+'Label';
      FEsLabel.SetOffsets(0, 0);
      PositionLabel;
      FEsLabel.ALabel.BringToFront;
      {turn off auto size}
      TLabel(FEsLabel.ALabel).AutoSize := False;
    end;
  end else begin
    if Assigned(PF) then begin
      FEsLabel.ALabel.Free;
      FEsLabel.ALabel := nil;
    end;
  end;
end;

procedure TEsCustomColorComboBox.LabelChange(Sender : TObject);
begin
  if not (csLoading in ComponentState) then
    PositionLabel;
end;

procedure TEsCustomColorComboBox.Notification(AComponent : TComponent; Operation: TOperation);
var
  PF : TForm;
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if Assigned(FEsLabel) and (AComponent = FEsLabel.ALabel) then begin
      PF := TForm(GetParentForm(Self));
      if Assigned(PF) and not (csDestroying in PF.ComponentState) then begin
        FEsLabel.FVisible := False;
        FEsLabel.ALabel := nil;
      end
    end;
end;

procedure TEsCustomColorComboBox.PositionLabel;
begin
  if FEsLabel.Visible and Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) and
     not (csLoading in ComponentState) then begin

    if DefaultLabelPosition = dlpTopLeft then begin
      FEsLabel.ALabel.SetBounds(Left + FEsLabel.OffsetX,
                         FEsLabel.OffsetY - FEsLabel.ALabel.Height + Top,
                         FEsLabel.ALabel.Width, FEsLabel.ALabel.Height);
    end else begin
      FEsLabel.ALabel.SetBounds(Left + FEsLabel.OffsetX,
                         FEsLabel.OffsetY + Top + Height,
                         FEsLabel.ALabel.Width, FEsLabel.ALabel.Height);
    end;
  end;
end;

procedure TEsCustomColorComboBox.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  BoxWidth := 0;
  Invalidate;

  if HandleAllocated then
    PostMessage(Handle, ES_POSITIONLABEL, 0, 0);
end;

procedure TEsCustomColorComboBox.SetSelectedColor(Value : TColor);
var
  I : Integer;
begin
  for I := 0 to Pred(Items.Count) do begin
    if Value = TColor(Items.Objects[I]) then begin
      ItemIndex := I;
      Change;
      Break;
    end;
  end;
end;

procedure TEsCustomColorComboBox.SetShowColorNames(Value : Boolean);
begin
  if Value <> FShowColorNames then begin
    FShowColorNames := Value;
    BoxWidth := 0;
    Invalidate;
  end;
end;

procedure TEsCustomColorComboBox.SetVersion(const Value : string);
begin
end;

end.
