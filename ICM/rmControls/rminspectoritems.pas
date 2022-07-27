{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmInspectorItems
Purpose  : This unit contains the actual implementations of the rmInspectorItem
           items.
Date     : 01-18-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmInspectorItems;

interface

{$I CompilerDefines.INC}

uses rmInspector, Controls, stdctrls, classes;

{
TrmStringInspectorItem
TrmComplexInspectorItem
TrmComboInspectorItem
TrmIntegerInspectorItem
TrmDateInspectorItem
TrmCheckBoxInspectorItem
}


type
  TrmStringInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: string;
    fPassChar: char;
    fMaxLen: integer;
    procedure SetValue(const Value: string);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    constructor create(AOwner:TComponent); override;
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property MaxLength : integer read fMaxLen write fMaxLen;
    property PasswordChar : char read fPassChar write fPassChar;
    property Value: string read fValue write SetValue;
  end;

  TrmComplexInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: String;
    procedure SetValue(const Value: String);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property Value: String read fValue write SetValue;
  end;

  TrmComboInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: String;
    fItems: TStrings;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetValue(const Value: String);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property Value: String read fValue write SetValue;
    property Items: TStrings read GetItems write SetItems;
  end;

  TrmIntegerInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: Integer;
    fUseRange: boolean;
    fMaxValue: integer;
    fMinValue: integer;
    procedure SetMaxValue(const Value: integer);
    procedure SetMinValue(const Value: integer);
    procedure SetUseRange(const Value: boolean);
    procedure SetValue(const Value: integer);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    constructor create(AOwner:TComponent); override;
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property MinValue:integer read fMinValue write SetMinValue default 0;
    property MaxValue:integer read fMaxValue write SetMaxValue default 1;
    property UseRange:boolean read fUseRange write SetUseRange default false;
    property Value: integer read fValue write SetValue;
  end;

  TrmDateInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: TDate;
    fDateFormat: string;
    procedure SetValue(const Value: TDate);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property Value: TDate read fValue write SetValue;
    property DateFormat:string read fDateFormat write fDateFormat;
  end;

  TrmCheckBoxInspectorItem = class(TrmCustomInspectorItem)
  private
    fValue: Boolean;
    procedure SetValue(const Value: Boolean);
  protected
    function GetStringValue: string; override;
    procedure SetStringValue(const Value: string); override;
  public
    function EditorClass: TWinControlClass; override;
    procedure GetValueFromEditor(Editor: TWinControl); override;
    procedure SetValueIntoEditor(Editor: TWinControl); override;
    procedure SetupEditor(Inspector:TrmInspector; Editor:TWinControl); override;
  published
    property Value: Boolean read fValue write SetValue;
  end;


implementation
                                                     
uses windows, SysUtils, Forms, rmBaseEdit, rmBtnEdit, rmBtnCombo, rmCalendar, rmLibrary, rmCheckBox;

type
  TrmInspectorInvasion = class(TrmInspector)
  end;

  TrmInspectorBtnCombo = class(TrmBtnCombo)
  protected
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TrmInspectorCheckBox = class(TrmCheckBox)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure keypress(var Key: Char); override;
  end;

  TrmInspectorEdit = class(TrmCustomEdit)
  protected
    procedure keypress(var Key: Char); override;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
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
    property OnContextPopup;
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
    property OnStartDock;
    property OnStartDrag;
  end;

{ TrmStringInspectorItem }

constructor TrmStringInspectorItem.create(AOwner: TComponent);
begin
  inherited;
  fMaxLen := 0;
  fPassChar := #0;
end;

function TrmStringInspectorItem.EditorClass: TWinControlClass;
begin
  result := TrmInspectorEdit;
end;

function TrmStringInspectorItem.GetStringValue: string;
begin
  result := fValue;
end;

procedure TrmStringInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
  if Editor is TrmInspectorEdit then
    Value := TrmInspectorEdit(Editor).Text;
end;

procedure TrmStringInspectorItem.SetStringValue(const Value: string);
begin
  fValue := value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmStringInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmInspectorEdit then
   begin
      with TrmInspectorEdit(Editor) do
      begin
         BorderStyle := bsNone;
         Font := Inspector.Font;
         WantTabs := true;
         MaxLength := fMaxLen;
         PasswordChar := fPassChar;
      end;
   end;
end;

procedure TrmStringInspectorItem.SetValue(const Value: string);
begin
  fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmStringInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
  if Editor is TrmInspectorEdit then
    TrmInspectorEdit(Editor).Text := Value;
end;

{ TrmComplexInspectorItem }

function TrmComplexInspectorItem.EditorClass: TWinControlClass;
begin
   Result := TrmBtnEdit;
end;

function TrmComplexInspectorItem.GetStringValue: string;
begin
   result := fValue;
end;

procedure TrmComplexInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
   //Do Nothing...
end;

procedure TrmComplexInspectorItem.SetStringValue(const Value: string);
begin
   fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmComplexInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmBtnEdit then
   begin
      with TrmBtnEdit(Editor) do
      begin
         borderstyle := bsNone;
         Readonly := true;
         font := Inspector.Font;
         WantTabs := true;
         OnBtn1Click := Inspector.DoComplexEdit;
      end;
   end;
end;

procedure TrmComplexInspectorItem.SetValue(const Value: String);
begin
  fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmComplexInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
   if Editor is TrmBtnEdit then
      TrmBtnEdit(Editor).text := fValue;
end;

{ TrmComboInspectorItem }

constructor TrmComboInspectorItem.create(AOwner: TComponent);
begin
  inherited;
  fItems := TStringList.Create;
end;

destructor TrmComboInspectorItem.destroy;
begin
  fItems.free;
  inherited;
end;

function TrmComboInspectorItem.EditorClass: TWinControlClass;
begin
   result := TrmInspectorBtnCombo;
end;

function TrmComboInspectorItem.GetItems: TStrings;
begin
   result := fItems;
end;

function TrmComboInspectorItem.GetStringValue: string;
begin
   result := fValue;
end;

procedure TrmComboInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
   if editor is TrmInspectorBtnCombo then
      fValue := TrmInspectorBtnCombo(Editor).Text;
end;

procedure TrmComboInspectorItem.SetItems(const Value: TStrings);
begin
   fItems.assign(Value);  
end;

procedure TrmComboInspectorItem.SetStringValue(const Value: string);
begin
   fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmComboInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmInspectorBtnCombo then
   begin
      with TrmInspectorBtnCombo(Editor) do
      begin
         borderstyle := bsNone;
         readonly := true;
         font := Inspector.font;
         EllipsisBtnVisible := false;
         WantTabs := true;
         Items.Assign(Self.Items);
      end;
   end;
end;

procedure TrmComboInspectorItem.SetValue(const Value: String);
begin
  fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmComboInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
   if editor is TrmInspectorBtnCombo then
      TrmInspectorBtnCombo(Editor).Text := fvalue;
end;

{ TrmIntegerInspectorItem }

constructor TrmIntegerInspectorItem.create(AOwner: TComponent);
begin
  inherited;
  fMaxValue := 1;
  fMinValue := 0;
  fUseRange := false;
end;

function TrmIntegerInspectorItem.EditorClass: TWinControlClass;
begin
   Result := TrmInspectorEdit;
end;

function TrmIntegerInspectorItem.GetStringValue: string;
begin
   Result := inttostr(fValue);
end;

procedure TrmIntegerInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
   if Editor is TrmInspectorEdit then
      AsString := TrmInspectorEdit(Editor).Text;
end;

procedure TrmIntegerInspectorItem.SetMaxValue(const Value: integer);
begin
  if Value > fMinValue then
     fMaxValue := Value;

  if fUseRange and (fValue > fMaxValue) then
     fValue := fMaxValue;
end;

procedure TrmIntegerInspectorItem.SetMinValue(const Value: integer);
begin
  if Value < fMaxValue then
     fMinValue := Value;

  if fUseRange and (fValue < fMinValue) then
     fValue := fMinValue;
end;

procedure TrmIntegerInspectorItem.SetStringValue(const Value: string);
var
   wTemp : integer;
begin
   wTemp := strtoint(Value);
   if fUseRange then
   begin
      if (wTemp <= fMaxValue) and (wTemp >= fMinValue) then
         fValue := wTemp
      else
         raise exception.create('Value must be between '+inttostr(fMinValue)+' and '+inttostr(fMaxValue));
   end
   else
      fValue := wTemp;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmIntegerInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmInspectorEdit then
   begin
      with TrmInspectorEdit(Editor) do
      begin
         Font := Inspector.Font;
         BorderStyle := bsNone;
         WantTabs := true;
      end;
   end;
end;

procedure TrmIntegerInspectorItem.SetUseRange(const Value: boolean);
begin
  fUseRange := Value;
end;

procedure TrmIntegerInspectorItem.SetValue(const Value: integer);
begin
   if fUseRange then
   begin
      if (Value <= fMaxValue) and (Value >= MinValue) then
         fValue := Value
      else
         raise exception.create('Value must be between '+inttostr(fMinValue)+' and '+inttostr(fMaxValue));
   end
   else
      fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmIntegerInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
   if Editor is TrmInspectorEdit then
      TrmInspectorEdit(Editor).Text := AsString;
end;

{ TrmDateInspectorItem }

function TrmDateInspectorItem.EditorClass: TWinControlClass;
begin
  result := TrmComboCalendar;
end;

function TrmDateInspectorItem.GetStringValue: string;
begin
   result := datetostr(fValue);
end;

procedure TrmDateInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
   if Editor is TrmComboCalendar then
      AsString := TrmComboCalendar(Editor).Text;
end;

procedure TrmDateInspectorItem.SetStringValue(const Value: string);
begin
   fValue := strtoDate(Value);
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmDateInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmComboCalendar then
   begin
      with TrmComboCalendar(Editor) do
      begin
         Font := Inspector.Font;
         BorderStyle := bsNone;
         WantTabs := true;
         DateFormat := fDateFormat;
      end;
   end;
end;

procedure TrmDateInspectorItem.SetValue(const Value: TDate);
begin
  fValue := Value;
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmDateInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
   if Editor is TrmComboCalendar then
      TrmComboCalendar(Editor).SelectedDate := Value;
end;

{ TrmInspectorEdit }

procedure TrmInspectorEdit.keypress(var Key: Char);
begin
  inherited;
  if key = #9 then
     key := #0;
end;

{ TrmInspectorBtnCombo }

procedure TrmInspectorBtnCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (not DroppedDown and ((key = vk_Down) or (key = vk_up)) and (shift = [])) then
  begin
     if Owner is TrmInspector then
        TrmInspectorInvasion(Owner).EditorKeyDown(Self, key, shift);
  end
  else
  inherited;
end;

{ TrmCheckBoxInspectorItem }

function TrmCheckBoxInspectorItem.EditorClass: TWinControlClass;
begin
   result := TrmInspectorCheckBox;
end;

function TrmCheckBoxInspectorItem.GetStringValue: string;
begin
   result := booltostr(fValue);
end;

procedure TrmCheckBoxInspectorItem.GetValueFromEditor(Editor: TWinControl);
begin
   if Editor is TrmInspectorCheckBox then
      AsString := BoolTostr(TrmInspectorCheckBox(Editor).checked);
end;

procedure TrmCheckBoxInspectorItem.SetStringValue(const Value: string);
begin
   fValue := strtoBool(Value);
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmCheckBoxInspectorItem.SetupEditor(Inspector: TrmInspector;
  Editor: TWinControl);
begin
   if Editor is TrmInspectorCheckBox then
   begin
      with TrmInspectorCheckBox(Editor) do
      begin
         Font := Inspector.Font;
         Caption := 'Ryan';
         CBXAlignment := cbxCentered;
         WantTabs := true;
         WantArrows := true;
         Flat := true;
         ShowFocusRect := false;
      end;
   end;
end;

procedure TrmCheckBoxInspectorItem.SetValue(const Value: Boolean);
begin
   fValue := value;  
   if assigned(InspectorControl) then
      InspectorControl.Invalidate;
end;

procedure TrmCheckBoxInspectorItem.SetValueIntoEditor(Editor: TWinControl);
begin
   if Editor is TrmInspectorCheckBox then
      TrmInspectorCheckBox(Editor).checked := strtobool(AsString);
end;

{ TrmInspectorCheckBox }

{ TrmInspectorCheckBox }

procedure TrmInspectorCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((key = vk_Down) or (key = vk_up)) and (shift = []) then
  begin
     if Owner is TrmInspector then
        TrmInspectorInvasion(Owner).EditorKeyDown(Self, key, shift);
  end
  else
  inherited;
end;

procedure TrmInspectorCheckBox.keypress(var Key: Char);
begin
  inherited;
  if key = #9 then
     key := #0;
end;

initialization
   RegisterClasses([ TrmStringInspectorItem,
                     TrmComplexInspectorItem,
                     TrmComboInspectorItem,
                     TrmIntegerInspectorItem,
                     TrmDateInspectorItem,
                     TrmCheckboxInspectorItem ]);
end.
