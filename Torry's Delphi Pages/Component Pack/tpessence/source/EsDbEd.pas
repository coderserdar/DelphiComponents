
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

unit EsDbEd;
  {-data-aware calculator and calendar popup edit controls}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, DB, DbCtrls,
  {$IFNDEF VER130}
  DBTables,
  {$ENDIF}
  Forms, Graphics, Menus, Messages, StdCtrls,
  EsBase, EsCal, EsCalc, EsEdCal, EsEdCalc, EsEdPop;

const
  {field type supported by the number edit field}
  NumFieldTypes : set of  TFieldType =
    [ftSmallInt, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD];
  {field type supported by the date edit field}
  DateFieldTypes : set of  TFieldType = [ftDate, ftDateTime];

type
  TEsCustomDbNumberEdit = class(TEsCustomNumberEdit)
  protected {private}
    {.Z+}
    FAlignment : TAlignment;
    FCanvas    : TControlCanvas;
    FDataLink  : TFieldDataLink;
    FFocused   : Boolean;

    {property methods}
    function GetDataField : string;
    function GetDataSource : TDataSource;
    function GetField : TField;
    function GetReadOnly : Boolean;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);
    procedure SetFocused(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);

    {internal methods}
    procedure DataChange(Sender : TObject);
    procedure EditingChange(Sender : TObject);
    function GetTextMargins : TPoint;
    procedure UpdateData(Sender : TObject);

    {message methods}
    procedure WMCut(var Message : TMessage);
      message WM_CUT;
    procedure WMPaste(var Message : TMessage);
      message WM_PASTE;
    procedure WMPaint(var Message : TWMPaint);
      message WM_PAINT;
    procedure CMEnter(var Message : TCMEnter);
      message CM_ENTER;
    procedure CMExit(var Message : TCMExit);
      message CM_EXIT;
    {$IFDEF Win32}
    procedure CMGetDataLink(var Message : TMessage);
      message CM_GETDATALINK;
    {$ENDIF Win32}
    {.Z-}

  protected
    {.Z+}
    procedure Change;
      override;
    function GetButtonEnabled : Boolean;                               {!!.03}
      override;
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure Notification(AComponent : TComponent; Operation : TOperation);
      override;
    procedure PopupClose(Sender : TObject);
      override;
    {.Z-}

    {protected properties}
    property DataField : string
      read GetDataField
      write SetDataField;

    property DataSource : TDataSource
      read GetDataSource
      write SetDataSource;

    property ReadOnly : Boolean {hides ancestor's ReadOnly property}
      read GetReadOnly
      write SetReadOnly
      default False;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PopupOpen;                                               {!!.05}
      override;
    {.Z-}

    {public properties}
    property Field : TField
      read GetField;
  end;

  TEsDbNumberEdit = class(TEsCustomDbNumberEdit)
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
    property DataField;
    property DataSource;
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
    property PopupCalcHeight;
    property PopupCalcFont;                                            {!!.02}
    property PopupCalcWidth;
    property PopupMenu;
    property ReadOnly;
    property ShowButton;
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

  TEsCustomDbDateEdit = class(TEsCustomDateEdit)
  protected {private}
    {.Z+}
    FAlignment : TAlignment;
    FCanvas    : TControlCanvas;
    FDataLink  : TFieldDataLink;
    FFocused   : Boolean;

    {property methods}
    function GetDataField : string;
    function GetDataSource : TDataSource;
    function GetField : TField;
    function GetReadOnly : Boolean;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);
    procedure SetFocused(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);

    {internal methods}
    procedure DataChange(Sender : TObject);
    procedure EditingChange(Sender : TObject);
    function GetTextMargins : TPoint;
    procedure UpdateData(Sender : TObject);

    {message methods}
    procedure WMCut(var Message : TMessage);
      message WM_CUT;
    procedure WMPaste(var Message : TMessage);
      message WM_PASTE;
    procedure WMPaint(var Message : TWMPaint);
      message WM_PAINT;
    procedure CMEnter(var Message : TCMEnter);
      message CM_ENTER;
    procedure CMExit(var Message : TCMExit);
      message CM_EXIT;
    {$IFDEF Win32}
    procedure CMGetDataLink(var Message : TMessage);
      message CM_GETDATALINK;
    {$ENDIF Win32}
    {.Z-}

  protected
    {.Z+}
    procedure Change;
      override;
    function GetButtonEnabled : Boolean;                               {!!.03}
      override;
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure KeyPress(var Key : Char);
      override;
    procedure Notification(AComponent : TComponent; Operation : TOperation);
      override;
    procedure PopupClose(Sender : TObject);
      override;
    {.Z-}

    {protected properties}
    property DataField : string
      read GetDataField
      write SetDataField;

    property DataSource : TDataSource
      read GetDataSource
      write SetDataSource;

    property ReadOnly : Boolean {hides ancestor's ReadOnly property}
      read GetReadOnly
      write SetReadOnly
      default False;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PopupOpen;                                               {!!.05}
      override;
    {.Z-}

    {public properties}
    property Field : TField
      read GetField;
  end;

  TEsDbDateEdit = class(TEsCustomDbDateEdit)
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
    property CharCase;
    property Color;
    property Ctl3D;
    property Cursor;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Epoch;                                                    {!!.04}
    property EsLabelInfo;
    property Font;
    property ForceCentury;                                             {!!.04}
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PopupCalColors;
    property PopupCalHeight;
    property PopupCalFont;                                             {!!.02}
    property PopupCalWidth;
    property ReadOnly;
    property RequiredFields;
    property ShowButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TodayString;
    property Version;
    property Visible;
    property WeekStarts;                                               {!!.02}

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


{*** TEsCustomDbNumberEdit ***}

procedure TEsCustomDbNumberEdit.Change;
begin
  FDataLink.Modified;

  inherited Change;
end;

procedure TEsCustomDbNumberEdit.CMEnter(var Message : TCMEnter);
begin
  SetFocused(True);

  inherited;
end;

procedure TEsCustomDbNumberEdit.CMExit(var Message : TCMExit);
begin
  if PopupActive then
    Exit;

  try
    if Modified then                                                   {!!.04}
      FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

{$IFDEF Win32}
procedure TEsCustomDbNumberEdit.CMGetDataLink(var Message : TMessage);
begin
  Message.Result := Integer(FDataLink);
end;
{$ENDIF Win32}

constructor TEsCustomDbNumberEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  inherited ReadOnly := True;

  {$IFDEF Win32}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF Win32}

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;

end;

procedure TEsCustomDbNumberEdit.DataChange(Sender : TObject);
var
  P : Integer;
  S : string[80];
begin
  if FDataLink.Field <> nil then begin
    if FAlignment <> FDataLink.Field.Alignment then begin
      FAlignment := FDataLink.Field.Alignment;
      Text := '';
    end;
    if FDataLink.Field.DataType in NumFieldTypes then begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
    end else begin
      S := FDataLink.Field.ClassName;
      S[1] := '(';
      P := Pos('Field', S);
      if P > 0 then begin
        S[P] := ')';
        S[0] := Char(P);
      end else
        S := Concat(S, ')');
      Text := S;
    end;
  end else begin
    FAlignment := taLeftJustify;
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

destructor TEsCustomDbNumberEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  FCanvas.Free;
  FCanvas := nil;

  inherited Destroy;
end;

procedure TEsCustomDbNumberEdit.EditingChange(Sender : TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  FButton.Enabled := GetButtonEnabled;                                 {!!.03}
end;

{!!.03}
function TEsCustomDbNumberEdit.GetButtonEnabled : Boolean;
begin
  Result := (FDataLink <> nil) and (FDataLink.DataSource <> nil) and
    (FDataLink.Editing or FDataLink.DataSource.AutoEdit) or
    (csDesigning in ComponentState);
end;

function TEsCustomDbNumberEdit.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TEsCustomDbNumberEdit.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TEsCustomDbNumberEdit.GetField : TField;
begin
  Result := FDataLink.Field;
end;

function TEsCustomDbNumberEdit.GetReadOnly : Boolean;
begin
  Result := FDataLink.ReadOnly;
  if FDataLink.Field <> nil then
    if not (FDataLink.Field.DataType in NumFieldTypes) then
      Result := True;
end;

function TEsCustomDbNumberEdit.GetTextMargins : TPoint;
var
  DC         : HDC;
  SaveFont   : HFont;
  I          : Integer;
  SysMetrics : TTextMetric;
  Metrics    : TTextMetric;
begin
  if NewStyleControls then begin
    if BorderStyle = bsNone then
      I := 0
    else if Ctl3D then
      I := 1
    else
      I := 2;
    {$IFDEF Win32}
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    {$ELSE}
    Result.X := 2;
    {$ENDIF Win32}
    Result.Y := I;
  end else begin
    if BorderStyle = bsNone then
      I := 0
    else begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TEsCustomDbNumberEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  {start edit mdoe if cutting or pasting}
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TEsCustomDbNumberEdit.KeyPress(var Key : Char);
begin
  if AllowIncDec and (Key in ['+', '-']) then
    FDataLink.Edit;

  inherited KeyPress(Key);

  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
     not FDataLink.Field.IsValidChar(Key) then begin
    MessageBeep(0);
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..#255 :
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TEsCustomDbNumberEdit.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TEsCustomDbNumberEdit.PopupClose(Sender : TObject);
begin
  inherited PopupClose(Sender);

  {allow control to see focus change that was blocked when popup became active}
  if not Focused then
    Perform(CM_EXIT, 0, 0);
end;

{!!.03}
procedure TEsCustomDbNumberEdit.PopupOpen;
begin
  if FDataLink.Edit then  {enter edit mode}
    inherited PopupOpen
  else begin
    MessageBeep(0);
    SetFocus;
  end;
end;

procedure TEsCustomDbNumberEdit.SetDataField(const Value : string);
begin
  try
    FDataLink.FieldName := Value;
  except
    FDataLink.FieldName := '';
    raise;
  end;
end;

procedure TEsCustomDbNumberEdit.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
  {$IFDEF Win32}
  if Value <> nil then
    Value.FreeNotification(Self);
  {$ENDIF Win32}
end;

procedure TEsCustomDbNumberEdit.SetFocused(Value : Boolean);
begin
  if FFocused <> Value then begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then
      Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TEsCustomDbNumberEdit.SetReadOnly(Value : Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TEsCustomDbNumberEdit.UpdateData(Sender : TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TEsCustomDbNumberEdit.WMCut(var Message : TMessage);
begin
  FDataLink.Edit;

  inherited;
end;

procedure TEsCustomDbNumberEdit.WMPaint(var Message : TWMPaint);
var
  Left    : Integer;
  Margins : TPoint;
  R       : TRect;
  DC      : HDC;
  PS      : TPaintStruct;
  S       : string;
begin
  {$IFDEF Win32}
  if ((FAlignment = taLeftJustify) or FFocused) and not (csPaintCopy in ControlState) then begin
  {$ELSE}
  if ((FAlignment = taLeftJustify) or FFocused) then begin
  {$ENDIF Win32}
    inherited;
    Exit;
  end;

  {draw right and center justify manually unless the edit has the focus}
  if FCanvas = nil then begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      {$IFDEF Win32}
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then begin
        S := FDataLink.Field.DisplayText;
      end else
      {$ENDIF Win32}
        S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case FAlignment of
        taLeftJustify  : Left := Margins.X;
        taRightJustify : Left := ClientWidth - TextWidth(S) - Margins.X - 2 - GetButtonWidth;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

procedure TEsCustomDbNumberEdit.WMPaste(var Message : TMessage);
begin
  FDataLink.Edit;

  inherited;
end;


{*** TEsCustomDbDateEdit ***}

procedure TEsCustomDbDateEdit.Change;
begin
  FDataLink.Modified;

  inherited Change;
end;

procedure TEsCustomDbDateEdit.CMEnter(var Message : TCMEnter);
begin
  SetFocused(True);

  inherited;
end;

procedure TEsCustomDbDateEdit.CMExit(var Message : TCMExit);
var                                                                    {!!.05}
  WasModified : Boolean;                                               {!!.05}
begin
  if PopupActive then
    Exit;

  WasModified := Modified;                                             {!!.05}
  DoExit;    {force update of date}
  try
    if WasModified then                                                {!!.05}
      FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
end;

{$IFDEF Win32}
procedure TEsCustomDbDateEdit.CMGetDataLink(var Message : TMessage);
begin
  Message.Result := Integer(FDataLink);
end;
{$ENDIF Win32}

constructor TEsCustomDbDateEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  inherited ReadOnly := True;

  {$IFDEF Win32}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF Win32}

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;

end;

procedure TEsCustomDbDateEdit.DataChange(Sender : TObject);
var
  P  : Integer;
  DT : TDateTime;
  S  : string[80];
begin
  if FDataLink.Field <> nil then begin
    if FAlignment <> FDataLink.Field.Alignment then begin
      FAlignment := FDataLink.Field.Alignment;
      Text := '';
    end;
    if FDataLink.Field.DataType in DateFieldTypes then begin
      DT := FDataLink.Field.AsDateTime;                                {!!.04}
      FDate := Trunc(DT);                                              {!!.04}
      if FFocused and FDataLink.CanModify then                         {!!.04}
        SetDate(Trunc(DT))                                             {!!.04}
      else
        Text := FDataLink.Field.DisplayText;
    end else begin
      S := FDataLink.Field.ClassName;
      S[1] := '(';
      P := Pos('Field', S);
      if P > 0 then begin
        S[P] := ')';
        S[0] := Char(P);
      end else
        S := Concat(S, ')');
      Text := S;
    end;
  end else begin
    FAlignment := taLeftJustify;
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

destructor TEsCustomDbDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  FCanvas.Free;
  FCanvas := nil;

  inherited Destroy;
end;

procedure TEsCustomDbDateEdit.EditingChange(Sender : TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  FButton.Enabled := GetButtonEnabled;                                 {!!.03}
end;

{!!.03}
function TEsCustomDbDateEdit.GetButtonEnabled : Boolean;
begin
  Result := (FDataLink <> nil) and (FDataLink.DataSource <> nil) and
    (FDataLink.Editing or FDataLink.DataSource.AutoEdit) or
    (csDesigning in ComponentState);
end;

function TEsCustomDbDateEdit.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TEsCustomDbDateEdit.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TEsCustomDbDateEdit.GetField : TField;
begin
  Result := FDataLink.Field;
end;

function TEsCustomDbDateEdit.GetReadOnly : Boolean;
begin
  Result := FDataLink.ReadOnly;
  if FDataLink.Field <> nil then
    if not (FDataLink.Field.DataType in DateFieldTypes) then
      Result := True;
end;

function TEsCustomDbDateEdit.GetTextMargins : TPoint;
var
  DC         : HDC;
  SaveFont   : HFont;
  I          : Integer;
  SysMetrics : TTextMetric;
  Metrics    : TTextMetric;
begin
  if NewStyleControls then begin
    if BorderStyle = bsNone then
      I := 0
    else if Ctl3D then
      I := 1
    else
      I := 2;
    {$IFDEF Win32}
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    {$ELSE}
    Result.X := 2;
    {$ENDIF Win32}
    Result.Y := I;
  end else begin
    if BorderStyle = bsNone then
      I := 0
    else begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TEsCustomDbDateEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  inherited KeyDown(Key, Shift);

  {start edit mdoe if cutting or pasting}
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TEsCustomDbDateEdit.KeyPress(var Key : Char);
begin
  if AllowIncDec and (Key in ['+', '-']) then
    FDataLink.Edit;

  inherited KeyPress(Key);

  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
     not FDataLink.Field.IsValidChar(Key) then begin
    MessageBeep(0);
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..#255 :
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TEsCustomDbDateEdit.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TEsCustomDbDateEdit.PopupClose(Sender : TObject);
begin
  inherited PopupClose(Sender);

  {allow control to see focus change that was blocked when popup became active}
  if not Focused then
    Perform(CM_EXIT, 0, 0);
end;

{!!.03}
procedure TEsCustomDbDateEdit.PopupOpen;
begin
  if FDataLink.Edit then  {enter edit mode}
    inherited PopupOpen
  else begin
    MessageBeep(0);
    SetFocus;
  end;
end;

procedure TEsCustomDbDateEdit.SetDataField(const Value : string);
begin
  try
    FDataLink.FieldName := Value;
  except
    FDataLink.FieldName := '';
    raise;
  end;
end;

procedure TEsCustomDbDateEdit.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
  {$IFDEF Win32}
  if Value <> nil then
    Value.FreeNotification(Self);
  {$ENDIF Win32}
end;

procedure TEsCustomDbDateEdit.SetFocused(Value : Boolean);
begin
  if FFocused <> Value then begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then
      Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TEsCustomDbDateEdit.SetReadOnly(Value : Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{!!.04}
procedure TEsCustomDbDateEdit.UpdateData(Sender : TObject);
var
  DT : TDateTime;
begin
  if FDataLink.Field.DataType in DateFieldTypes then begin
    DT := FDataLink.Field.AsDateTime;
    if Text = '' then begin {save just the time portion}
      if (FDataLink.Field.DataType = ftDateTime) and (Frac(DT) <> 0) then
        FDataLink.Field.AsDateTime := Frac(DT)
      else
        FDataLink.Field.Clear;
    end else begin
      DoExit;  {validate field and translate date}
      FDataLink.Field.AsDateTime := Date + Frac(DT);
    end;
  end else
    FDataLink.Field.Text := Text;
end;

procedure TEsCustomDbDateEdit.WMCut(var Message : TMessage);
begin
  FDataLink.Edit;

  inherited;
end;

procedure TEsCustomDbDateEdit.WMPaint(var Message : TWMPaint);
var
  Left    : Integer;
  Margins : TPoint;
  R       : TRect;
  DC      : HDC;
  PS      : TPaintStruct;
  S       : string;
begin
  {$IFDEF Win32}
  if ((FAlignment = taLeftJustify) or FFocused) and not (csPaintCopy in ControlState) then begin
  {$ELSE}
  if ((FAlignment = taLeftJustify) or FFocused) then begin
  {$ENDIF Win32}
    inherited;
    Exit;
  end;

  {draw right and center justify manually unless the edit has the focus}
  if FCanvas = nil then begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      {$IFDEF Win32}
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then begin
        S := FDataLink.Field.DisplayText;
      end else
      {$ENDIF Win32}
        S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case FAlignment of
        taLeftJustify  : Left := Margins.X;
        taRightJustify : Left := ClientWidth - TextWidth(S) - Margins.X - 2 - GetButtonWidth;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

procedure TEsCustomDbDateEdit.WMPaste(var Message : TMessage);
begin
  FDataLink.Edit;

  inherited;
end;

end.