{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Data controls                                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgDBCtrl;

interface
uses Windows, Messages, Classes, Controls, StdCtrls, DB, {$IFNDEF _D3_} DBTables, {$ENDIF}
  Menus, DBCtrls, DBGrids, vgCtrls;

resourcestring
  SDataSourceFixed = 'Operation not allowed in a DBCtrlGrid';
  SNotReplicatable = 'Control cannot be used in a DBCtrlGrid';

type
{ TvgDBMenu }
  TMenuAction = (maInsert, maEdit, maDelete, maFirst, maLast, maRefresh);
  TMenuActions = set of TMenuAction;

  TMenuActionEvent = procedure (Sender: TObject; Action: TMenuAction) of object;

  TvgDBMenu = class(TCustomHook)
  private
    FDataLink: TDataLink;
    FPopupMenu: TPopupMenu;
    FMenuActions: TMenuActions;
    FMenuItems: array [TMenuAction] of TMenuItem;
    FOnMenuActions: TMenuActionEvent;
    FOnPopup: TNotifyEvent;
    FOnUpdateMenuItems: TNotifyEvent;
    function GetCaption(Index: Integer): string;
    procedure SetCaption(Index: Integer; Value: string);
    function IsCaptionStored(Index: Integer): Boolean;
    function GetEnabled(Action: TMenuAction): Boolean;
    procedure SetEnabled(Action: TMenuAction; Value: Boolean);
    function GetShortCut(Index: Integer): TShortCut;
    procedure SetShortCut(Index: Integer; Value: TShortCut);
    function IsShortCutStored(Index: Integer): Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetMenuActions(Value: TMenuActions);
    function GetMenuItem(Index: TMenuAction): TMenuItem;
    procedure MenuItemClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    function GetControl: TControl;
    procedure SetControl(Value: TControl);
  protected
    procedure DataSetChanged; virtual;
    procedure DoClick(Action: TMenuAction); virtual;
    procedure DoPopup; virtual;
    procedure DoUpdateMenuItems; virtual;
    procedure HookObject; override;
    procedure UnHookObject; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click(Action: TMenuAction);
    procedure UpdateMenuItems;
    property MenuItems[Index: TMenuAction]: TMenuItem read GetMenuItem;
    property PopupMenu: TPopupMenu read FPopupMenu;
    property Enabled[Action: TMenuAction]: Boolean read GetEnabled write SetEnabled;
  published
    property CaptionInsert: string index 0 read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionEdit: string index 1 read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionDelete: string index 2 read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionFirst: string index 3 read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionLast: string index 4 read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionRefresh: string index 5 read GetCaption write SetCaption stored IsCaptionStored;
    property Control: TControl read GetControl write SetControl;
    property MenuActions: TMenuActions read FMenuActions write SetMenuActions
      default [maInsert, maEdit, maDelete, maFirst, maLast, maRefresh];
    property ShortCutInsert: TShortCut index 0 read GetShortCut write SetShortCut stored IsShortCutStored;
    property ShortCutEdit: TShortCut index 1 read GetShortCut write SetShortCut stored IsShortCutStored;
    property ShortCutDelete: TShortCut index 2 read GetShortCut write SetShortCut stored IsShortCutStored;
    property ShortCutFirst: TShortCut index 3 read GetShortCut write SetShortCut stored IsShortCutStored;
    property ShortCutLast: TShortCut index 4 read GetShortCut write SetShortCut stored IsShortCutStored;
    property ShortCutRefresh: TShortCut index 5 read GetShortCut write SetShortCut stored IsShortCutStored;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property OnMenuItemClick: TMenuActionEvent read FOnMenuActions write FOnMenuActions;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnUpdateMenuItems: TNotifyEvent read FOnUpdateMenuItems write FOnUpdateMenuItems;
  end;

{ TDBRadioButton }
  TDBRadioButton = class(TRadioButton)
  private
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    FValue: string;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetValue(Value: string);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure Click; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Value: string read FValue write SetValue;
  end;

{ TvgQuickSearch }
  TvgQuickSearch = class(TCustomHook)
  private
    FEnableCopy: Boolean;
    FSearchText: string;
    FOldKeyDown: TKeyEvent;
    FOldKeyPress: TKeyPressEvent;
    FOldMouseDown: TMouseEvent;
    function Locate(NewText: string): Boolean;
    procedure OldKeyDown(var Key: Word; Shift: TShiftState);
    procedure OldKeyPress(var Key: Char);
    procedure OldMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetGrid: TCustomDBGrid;
    procedure SetGrid(Value: TCustomDBGrid);
  protected
    function GetDataSet: TDataSet;
    function InBrowseMode: Boolean;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(Sender: TObject; var Key: Char); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function LocateDataSet(DataSet: TDataSet; const KeyFields: string;
      const KeyValues: Variant; Options: TLocateOptions): Boolean; virtual;
    procedure HookObject; override;
    procedure UnHookObject; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
  published
    property EnableCopy: Boolean read FEnableCopy write FEnableCopy default True;
    property Grid: TCustomDBGrid read GetGrid write SetGrid;
  end;

{ TvgCustomDBText }
  TvgCustomDBText = class(TvgCustomLabel)
  private
    FDataLink: TFieldDataLink;
    FFormat: string;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetFormat(const Value: string);
  protected
    function GetLabelText: string; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
    property Field: TField read GetField;
    property Format: string read FFormat write SetFormat;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF _D4_}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
  end;

{ TvgDBText }
  TvgDBText = class(TvgCustomDBText)
  public
    property Field;
  published
    property DataField;
    property DataSource;
    property Effects;
    property ExecParams;
    property Format;
    property Layout;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
  {$ENDIF}
{$ENDIF}
    property Align;
    property Alignment;
    property AutoSize default False;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Transparent;
    property ShowHint;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TvgCustomDBListBox }
  TvgCustomDBListBox = class(TvgCustomListBox)
  private
    FDataLink: TFieldDataLink;
    FEnableValues: Boolean;
    FValues: TStrings;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetItems(Value: TStrings);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property Field: TField read GetField;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EnableValues: Boolean read FEnableValues write SetEnableValues default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF _D4_}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
  end;

{ TvgDBListBox }
  TvgDBListBox = class(TvgCustomDBListBox)
  public
    property Field;
  published
    property Images;
    property RowSelect;
    property StateImages;
    property OnGetItemIndent;
    property OnGetItemParams;
  published
    property DataField;
    property DataSource;
    property EnableValues;
    property ReadOnly;
    property Values;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property ParentBiDiMode;
    property Constraints;
    property DragKind;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D default True;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items write SetItems;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

{ TvgCustomDBComboBox }
  TvgCustomDBComboBox = class(TvgCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    FPaintControl: TPaintControl;
    FEnableValues: Boolean;
    FValues: TStrings;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetComboText: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetComboText(const Value: string);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetItems(Value: TStrings);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetStyle(Value: TComboboxStyle); override;
    procedure WndProc(var Message: TMessage); override;
    property Field: TField read GetField;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EnableValues: Boolean read FEnableValues write SetEnableValues default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; override;
{$IFDEF _D4_}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
  end;

{ TvgDBComboBox }
  TvgDBComboBox = class(TvgCustomDBComboBox)
  public
    property Field;
    property Text;
  published
    property DropDownWidth;
    property EnableValues;
    property Images;
    property RowSelect;
    property StateImages;
    property OnGetItemIndent;
    property OnGetItemParams;
    property DataField;
    property DataSource;
    property ReadOnly;
    property Values;
  published
{$IFDEF _D3_}
  {$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property ParentBiDiMode;
    property Constraints;
    property DragKind;
  {$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property Style; {Must be published before Items}
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items write SetItems;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
  {$IFDEF _D5_}
    property OnContextPopup;
  {$ENDIF}
{$ENDIF}
  end;

implementation
uses SysUtils, TypInfo, vgDBRes, Forms, DBConsts, vgUtils, vgDBUtl;

{ TvgDBMenu }
var
  DefCaptions: array [TMenuAction] of string;

  DefShortCuts: array [TMenuAction] of TShortCut =
    (VK_INSERT, VK_RETURN, VK_DELETE, VK_HOME, VK_END, VK_F5);

type
  TDBMenuDataLink = class(TDataLink)
  private
    FDBMenu: TvgDBMenu;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  end;

  TControlHack = class(TControl);

procedure TDBMenuDataLink.ActiveChanged;
begin
  if Assigned(FDBMenu) then FDBMenu.DataSetChanged;
end;

procedure TDBMenuDataLink.DataSetChanged;
begin
  if Assigned(FDBMenu) then FDBMenu.DataSetChanged;
end;

{ TvgDBMenu }

var
  CaptionsLoaded: Boolean = False;

constructor TvgDBMenu.Create(AOwner: TComponent);
var
  I: TMenuAction;
begin
  inherited;

  if not CaptionsLoaded then
  begin
    for I := Low(DefCaptions) to High(DefCaptions) do
      DefCaptions[I] := LoadStr(SDBMenuInsert - Integer(I));
    CaptionsLoaded := True;
  end;

  Active := True;
  FDataLink := TDBMenuDataLink.Create;
  TDBMenuDataLink(FDataLink).FDBMenu := Self;
  FPopupMenu := TPopupMenu.Create(nil);
  for I := Low(TMenuAction) to High(TMenuAction) do
  begin
    FMenuItems[I] := NewItem(DefCaptions[I], DefShortCuts[I],  False, False, MenuItemClick, 0, '');
    FMenuItems[I].Tag := Integer(I);
    FPopupMenu.Items.Add(FMenuItems[I]);
  end;
  FPopupMenu.Items.Insert(FMenuItems[maFirst].MenuIndex, NewLine);
  FPopupMenu.Items.Insert(FMenuItems[maRefresh].MenuIndex, NewLine);
  FPopupMenu.OnPopup := MenuPopup;
  FMenuActions := [maInsert, maEdit, maDelete, maFirst, maLast, maRefresh];
end;

destructor TvgDBMenu.Destroy;
begin
  FDataLink.Free;
  while FPopupMenu.Items.Count > 0 do FPopupMenu.Items[0].Free;
  FPopupMenu.Free;
  inherited;
end;

procedure TvgDBMenu.DoClick(Action: TMenuAction);
begin
  case Action of
    maFirst:
      DataSource.DataSet.First;
    maLast:
      DataSource.DataSet.Last;
  else if Assigned(FOnMenuActions) then
    FOnMenuActions(Self, Action);
  end;
end;

procedure TvgDBMenu.Click(Action: TMenuAction);
begin
  MenuItems[Action].Click;
end;

procedure TvgDBMenu.DataSetChanged;
begin
  DoUpdateMenuItems;
end;

procedure TvgDBMenu.DoPopup;
begin
  if Assigned(FOnPopup) then FOnPopup(Self);
end;

procedure TvgDBMenu.DoUpdateMenuItems;
  function NotEmpty: Boolean;
  begin
    Result := not (DataSource.DataSet.BOF and DataSource.DataSet.EOF);
  end;
begin
  FMenuItems[maInsert].Enabled := FDataLink.Active;
  FMenuItems[maEdit].Enabled := FDataLink.Active and NotEmpty;
  FMenuItems[maDelete].Enabled := FMenuItems[maEdit].Enabled;
  FMenuItems[maFirst].Enabled  := FDataLink.Active and not DataSource.DataSet.BOF;
  FMenuItems[maLast].Enabled  := FDataLink.Active and not DataSource.DataSet.EOF;
  FMenuItems[maRefresh].Enabled := FDataLink.Active;
  if not (csDestroying in ComponentState) and Assigned(FOnUpdateMenuItems) then
    FOnUpdateMenuItems(Self);
end;

function TvgDBMenu.GetCaption(Index: Integer): string;
begin
  Result := FMenuItems[TMenuAction(Index)].Caption;
end;

procedure TvgDBMenu.SetCaption(Index: Integer; Value: string);
begin
  FMenuItems[TMenuAction(Index)].Caption := Value;
end;

function TvgDBMenu.IsCaptionStored(Index: Integer): Boolean;
begin
  Result := FMenuItems[TMenuAction(Index)].Caption <> DefCaptions[TMenuAction(Index)];
end;

function TvgDBMenu.GetEnabled(Action: TMenuAction): Boolean;
begin
  Result := FMenuItems[Action].Enabled;
end;

procedure TvgDBMenu.SetEnabled(Action: TMenuAction; Value: Boolean);
begin
  FMenuItems[Action].Enabled := Value;
end;

function TvgDBMenu.GetShortCut(Index: Integer): TShortCut;
begin
  Result := FMenuItems[TMenuAction(Index)].ShortCut;
end;

procedure TvgDBMenu.SetShortCut(Index: Integer; Value: TShortCut);
begin
  FMenuItems[TMenuAction(Index)].ShortCut := Value;
end;

function TvgDBMenu.IsShortCutStored(Index: Integer): Boolean;
begin
  Result := FMenuItems[TMenuAction(Index)].ShortCut <> DefShortCuts[TMenuAction(Index)];
end;

function TvgDBMenu.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgDBMenu.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Assigned(Value) then FreeNotification(Value);
end;

procedure TvgDBMenu.SetMenuActions(Value: TMenuActions);
  function IsSeparator(Index: Integer): Boolean;
  var
    I: Integer;
    Item: TMenuItem;
  begin
    Result := True;
    for I := Index to FPopupMenu.Items.Count - 1 do
    begin
      Item := FPopupMenu.Items[I];
      if (Item.Caption <> '-') and Item.Visible then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
  procedure UpdateVisible;
  var
    I, Count: Integer;
    Item: TMenuItem;
  begin
    Count := 0;
    for I := 0 to FPopupMenu.Items.Count - 1 do
    begin
      Item := FPopupMenu.Items[I];
      if (Item.Caption <> '-') then
      begin
        if Item.Visible then Inc(Count);
      end else begin
        Item.Visible := (Count > 0) and not IsSeparator(I + 1);
        Count := 0;
      end;
    end;
  end;
var
  I: TMenuAction;
begin
  if FMenuActions <> Value then
  begin
    for I := Low(TMenuAction) to High(TMenuAction) do
      FMenuItems[I].Visible := I in Value;
    FMenuActions := Value;
    UpdateVisible;
  end;
end;

function TvgDBMenu.GetMenuItem(Index: TMenuAction): TMenuItem;
begin
  Result := FMenuItems[Index];
end;

procedure TvgDBMenu.MenuPopup(Sender: TObject);
begin
  DoPopup;
end;

procedure TvgDBMenu.MenuItemClick(Sender: TObject);
begin
  DoClick(TMenuAction((Sender as TMenuItem).Tag));
end;

procedure TvgDBMenu.Loaded;
begin
  Active := True;
  inherited;
  DoUpdateMenuItems;
end;

procedure TvgDBMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = GetDataSource) then SetDataSource(nil);
end;

function TvgDBMenu.GetControl: TControl;
begin
  Result := TControl(HookedObject);
end;

procedure TvgDBMenu.SetControl(Value: TControl);
begin
  HookedObject := Value;
end;

procedure TvgDBMenu.HookObject;
begin
  if (csDesigning in ComponentState) then Exit;
  TControlHack(GetControl).PopupMenu := FPopupMenu;
end;

procedure TvgDBMenu.UnHookObject;
begin
  if (csDesigning in ComponentState) then Exit;
  TControlHack(GetControl).PopupMenu := nil;
end;

procedure TvgDBMenu.UpdateMenuItems;
begin
  DoUpdateMenuItems;
end;

{ TDBRadioButton }
constructor TDBRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
end;

destructor TDBRadioButton.Destroy;
begin
  FreeObject(FDataLink);
  inherited;
end;

procedure TDBRadioButton.Click;
begin
  inherited;
  if FDataLink.Editing then FDataLink.Field.Text := FValue;
end;

procedure TDBRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = GetDataSource) then DataSource := nil;
end;

procedure TDBRadioButton.DataChange(Sender: TObject);
begin
  Checked := (FDataLink.Field <> nil) and (FDataLink.Field.Text = FValue);
end;

procedure TDBRadioButton.EditingChange(Sender: TObject);
begin
end;

function TDBRadioButton.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBRadioButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBRadioButton.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBRadioButton.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBRadioButton.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBRadioButton.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Assigned(Value) then FreeNotification(Value);
end;

procedure TDBRadioButton.SetFocused(Value: Boolean);
begin
  FFocused := Value;
end;

procedure TDBRadioButton.SetReadOnly(AValue: Boolean);

  procedure TurnSiblingsReadOnly;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do
        begin
          Sibling := Controls[I];
          if (Sibling <> Self) and (Sibling is TDBRadioButton) then
            with (Sibling as TDBRadioButton) do ReadOnly := AValue;
        end;
  end;

begin
  if (ReadOnly <> AValue) then
  begin
    FDataLink.ReadOnly := AValue;
    TurnSiblingsReadOnly;
  end;
end;

procedure TDBRadioButton.SetValue(Value: string);
begin
  if (FValue <> Value) then
  begin
    FValue := Value;
    DataChange(nil);
  end;
end;

procedure TDBRadioButton.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TDBRadioButton.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TDBRadioButton.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

procedure TDBRadioButton.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = BN_CLICKED) then
  begin
    FDataLink.Edit;
    if not FDataLink.Editing then Exit;
  end;
  inherited;
end;

{ TvgQuickSearch }
type
  TDBGridHack = class(TCustomDBGrid);

constructor TvgQuickSearch.Create(AOwner: TComponent);
begin
  inherited;
  Active := True;
  FEnableCopy := True;
end;

destructor TvgQuickSearch.Destroy;
begin
  inherited;
end;

procedure TvgQuickSearch.Reset;
begin
  FSearchText := '';
end;

function TvgQuickSearch.GetDataSet: TDataSet;
begin
  if InBrowseMode then
    Result := TDBGridHack(GetGrid).DataSource.DataSet else
    Result := nil;
end;

function TvgQuickSearch.InBrowseMode: Boolean;
begin
  Result := (GetGrid <> nil) and Assigned(TDBGridHack(GetGrid).DataSource) and
    (TDBGridHack(GetGrid).DataSource.State = dsBrowse);
end;

procedure TvgQuickSearch.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  OldKeyDown(Key, Shift);
  if (Key in [0, VK_PRIOR..VK_DOWN]) then Reset;

  if FEnableCopy and (Key in [VK_INSERT, 67 {'C'}]) and (ssCtrl in Shift) and InBrowseMode then
  begin
    GridToClipboard(Grid);
    Key := 0;
  end;
end;

procedure TvgQuickSearch.KeyPress(Sender: TObject; var Key: Char);
var
  NewText: string;
begin
  OldKeyPress(Key);
  NewText := FSearchText;
  case Key of
    #8:
      if Length(NewText) > 0 then Delete(NewText, Length(NewText), 1);
    #32..#255:
      begin
        NewText := NewText + Key;
      end;
  end;
  if (NewText <> '') and Locate(NewText) then Key := #0;
end;

procedure TvgQuickSearch.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldMouseDown(Button, Shift, X, Y);
  Reset;
end;

function TvgQuickSearch.Locate(NewText: string): Boolean;
var
  DataSet: TDataSet;
  Col: Integer;
  Field: TField;
begin
  Result := False;
  if InBrowseMode then
  begin
    DataSet := TDBGridHack(GetGrid).DataSource.DataSet;
    Col := TDBGridHack(GetGrid).Col - Ord(dgIndicator in TDBGridHack(GetGrid).Options);
    if Col < TDBGridHack(GetGrid).Columns.Count then
    begin
      Field := TDBGridHack(GetGrid).Columns[Col].Field;
      if Assigned(Field) and not Field.Calculated and not Field.Lookup then
      begin
        DataSet.DisableControls;
        try
          try
            Result := LocateDataSet(DataSet, Field.FieldName, NewText, [loCaseInsensitive, loPartialKey]);
            if Result then FSearchText := NewText;
          except end;
        finally
          DataSet.EnableControls;
        end;
      end;
    end;
  end;
end;

function TvgQuickSearch.LocateDataSet(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := DataSet.Locate(KeyFields, KeyValues, Options);
end;

procedure TvgQuickSearch.OldKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOldKeyDown) then FOldKeyDown(GetGrid, Key, Shift);
end;

procedure TvgQuickSearch.OldKeyPress(var Key: Char);
begin
  if Assigned(FOldKeyPress) then FOldKeyPress(GetGrid, Key);
end;

procedure TvgQuickSearch.OldMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldMouseDown) then FOldMouseDown(GetGrid, Button, Shift, X, Y);
end;

function TvgQuickSearch.GetGrid: TCustomDBGrid;
begin
  Result := TCustomDBGrid(HookedObject);
end;

procedure TvgQuickSearch.SetGrid(Value: TCustomDBGrid);
begin
  if HookedObject <> Value then
  begin
    if IsObjectHooked(Value) then Exit;
    HookedObject := Value;
  end;
end;

procedure TvgQuickSearch.Loaded;
begin
  Active := True;
  inherited;
end;

procedure TvgQuickSearch.HookObject;
var
  FKeyPress: TKeyPressEvent;
begin
  if (csDesigning in ComponentState) then Exit;
  with TDBGridHack(GetGrid) do
  begin
    FOldKeyDown := OnKeyDown;
    TMethod(FOldKeyPress) := GetMethodProp(GetGrid, GetPropInfo(Grid.ClassInfo, 'OnKeyPress'));
    FOldMouseDown := OnMouseDown;
    OnKeyDown := Self.KeyDown;
    FKeyPress := Self.KeyPress;
    { TrxDBGrid overrides default event and kills the old one }
    SetMethodProp(GetGrid, GetPropInfo(Grid.ClassInfo, 'OnKeyPress'), TMethod(FKeyPress));
    OnMouseDown := Self.MouseDown;
  end;
end;

procedure TvgQuickSearch.UnHookObject;
begin
  if (csDesigning in ComponentState) then Exit;
  with TDBGridHack(GetGrid) do
  begin
    OnKeyDown := FOldKeyDown;
    SetMethodProp(GetGrid, GetPropInfo(Grid.ClassInfo, 'OnKeyPress'), TMethod(FOldKeyPress));
    OnMouseDown := FOldMouseDown;
  end;
end;

{ TvgCustomDBText }
constructor TvgCustomDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  AutoSize := False;
  ShowAccelChar := False;
  FDataLink := TFieldDataLink.Create;
{$IFDEF _D3_}
  FDataLink.Control := Self;
{$ENDIF}
  FDataLink.OnDataChange := DataChange;
end;

destructor TvgCustomDBText.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TvgCustomDBText.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TvgCustomDBText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{$IFDEF _D4_}
function TvgCustomDBText.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$ENDIF}

procedure TvgCustomDBText.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(ResStr(SDataSourceFixed));
    inherited SetAutoSize(Value);
  end;
end;

function TvgCustomDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgCustomDBText.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TvgCustomDBText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgCustomDBText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgCustomDBText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TvgCustomDBText.GetFieldText: string;
var
  S: string;
begin
  if FDataLink.Field <> nil then
  begin
    S := FDataLink.Field.DisplayText;
    if FFormat <> '' then
      Result := SysUtils.Format(FFormat, [S]);
  end else if csDesigning in ComponentState then
    Result := Name else Result := '';
end;

procedure TvgCustomDBText.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

function TvgCustomDBText.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText else
    Result := Caption;
end;

procedure TvgCustomDBText.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

{$IFDEF _D4_}
function TvgCustomDBText.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TvgCustomDBText.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

procedure TvgCustomDBText.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    DataChange(Self);
  end;
end;

{ TvgCustomDBListBox }
constructor TvgCustomDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
end;

destructor TvgCustomDBListBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;
  FreeObject(FDataLink);
  inherited;
end;

procedure TvgCustomDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{$IFDEF _D4_}
function TvgCustomDBListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$ENDIF}

procedure TvgCustomDBListBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FEnableValues then
      ItemIndex := FValues.IndexOf(FDataLink.Field.Text) else
      ItemIndex := Items.IndexOf(FDataLink.Field.Text);
  end else
    ItemIndex := -1;
end;

procedure TvgCustomDBListBox.UpdateData(Sender: TObject);
var
  I: Integer;
begin
  I := ItemIndex;
  if I >= 0 then
  begin
    if FEnableValues then
      if (I < FValues.Count) then
        FDataLink.Field.Text := FValues[I] else
        FDataLink.Field.Text := ''
    else
      FDataLink.Field.Text := Items[I]
  end else
    FDataLink.Field.Text := '';
end;

procedure TvgCustomDBListBox.Click;
begin
  if FDataLink.Edit then
  begin
    inherited Click;
    FDataLink.Modified;
  end;
end;

function TvgCustomDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgCustomDBListBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Assigned(Value) then Value.FreeNotification(Self);
end;

function TvgCustomDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgCustomDBListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgCustomDBListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TvgCustomDBListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TvgCustomDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TvgCustomDBListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN] then
    if not FDataLink.Edit then Key := 0;
end;

procedure TvgCustomDBListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TvgCustomDBListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if FDataLink.Edit then inherited
  else
  begin
    SetFocus;
    with Message do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TvgCustomDBListBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TvgCustomDBListBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

{$IFDEF _D4_}
function TvgCustomDBListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TvgCustomDBListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

procedure TvgCustomDBListBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TvgCustomDBListBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TvgCustomDBListBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

{ TvgCustomDBComboBox }
constructor TvgCustomDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');
  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
end;

destructor TvgCustomDBComboBox.Destroy;
begin
  FPaintControl.Free;
  TStringList(FValues).OnChange := nil;
  FValues.Free;
  FreeObject(FDataLink);
  inherited;
end;

procedure TvgCustomDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TvgCustomDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TvgCustomDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TvgCustomDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style = csSimple) and DroppedDown then Exit;
  if FDataLink.Field <> nil then
    SetComboText(FDataLink.Field.Text)
  else if csDesigning in ComponentState then
    SetComboText(Name) else
    SetComboText('');
end;

procedure TvgCustomDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TvgCustomDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          I := -1
        else if FEnableValues then
          I := FValues.IndexOf(Value) else
          I := Items.IndexOf(Value);

        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

function TvgCustomDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I >= 0 then
      if FEnableValues then
        if (I < FValues.Count) then
          Result := FValues[I] else
          Result := ''
      else
        Result := Items[I]
    else
      Result := '';
  end;
end;

procedure TvgCustomDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TvgCustomDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

procedure TvgCustomDBComboBox.DropDown;
begin
  if not FDataLink.CanModify then
  begin
    PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
    Exit;
  end else
    inherited;
end;

function TvgCustomDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TvgCustomDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TvgCustomDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TvgCustomDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TvgCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TvgCustomDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TvgCustomDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TvgCustomDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TvgCustomDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TvgCustomDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TvgCustomDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TvgCustomDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then
          FDataLink.Edit
        else
          if not FDataLink.Editing then
            DataChange(Self); {Restore text}
      WM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED:
        FPaintControl.DestroyHandle;
    end;
  inherited WndProc(Message);
end;

procedure TvgCustomDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TvgCustomDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
{$IFDEF _D3_}
  if SysLocale.FarEast and FDataLink.CanModify then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(False), 0);
{$ENDIF}
end;

procedure TvgCustomDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TvgCustomDBComboBox.WMPaint(var Message: TWMPaint);
var
  S: string;
  R: TRect;
  P: TPoint;
  Child: HWND;
begin
  if csPaintCopy in ControlState then
  begin
    if FDataLink.Field <> nil then S := FDataLink.Field.Text else S := '';
    if Style = csDropDown then
    begin
      SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Longint(PChar(S)));
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      Child := GetWindow(FPaintControl.Handle, GW_CHILD);
      if Child <> 0 then
      begin
        Windows.GetClientRect(Child, R);
        Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
        GetWindowOrgEx(Message.DC, P);
        SetWindowOrgEx(Message.DC, P.X - R.Left, P.Y - R.Top, nil);
        IntersectClipRect(Message.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
        SendMessage(Child, WM_PAINT, Message.DC, 0);
      end;
    end else begin
      SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
      if Items.IndexOf(S) <> -1 then
      begin
        SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
        SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
      end;
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
    end;
  end else
    inherited;
end;

procedure TvgCustomDBComboBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TvgCustomDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if FEnableValues and (Value in [csSimple, csDropDown]) then
    Value := csDropDownList;
  if (Value = csSimple) and Assigned(FDataLink) and FDatalink.DatasourceFixed then
    DatabaseError(ResStr(SNotReplicatable));
  inherited SetStyle(Value);
end;

{$IFDEF _D4_}
function TvgCustomDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$ENDIF}

procedure TvgCustomDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

{$IFDEF _D4_}
function TvgCustomDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TvgCustomDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

procedure TvgCustomDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TvgCustomDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    if Value and (Style in [csDropDown, csSimple]) then
      Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TvgCustomDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

end.
