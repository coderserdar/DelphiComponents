
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit InspectCtrls;

interface

{$I STD.INC}

uses
  Windows, ScrollCtrls, Controls, Classes, BtnEdit, Messages, Graphics,
  SysUtils, Forms, StdCtrls, WinTools, GraphTools;

{ The TInspectorEditor class is the base class for inspector editors. Class
  hierarchy of some standard editors are as follows:

  TPersistent
  |
  +-+ TInspectorEditor
    |
    +-+ TStringInspectorEditor
    |
    +-+ TIntegerInspectorEditor
    |
    +-+ TImageInspectorEditor
    |
    +-+ TFloatInspectorEditor
    | |
    | +-+ TMoneyInspectorEditor
    |
    +-+ TPopupInspectorEditor
      |
      +-+ TColorGridInspectorEditor
      |
      +-+ TDateInspectorEditor
      |
      +-+ TFolderInspectorEditor
      |
      +-+ TFontInspectorEditor
      |
      +-+ TStringsInspectorEditor }

type
  TCustomInspector = class;
  TInspectorEditors = class;

  TEditorAttribute = (eaButton, eaEllipseButton, eaDrawButton, eaDrawInline,
    eaKeyPress, eaReadOnly);
  TEditorAttributes = set of TEditorAttribute;
  TEditorKind = type Integer;

  TInspectorEditor = class(TPersistent)
  private
    FAttributes: TEditorAttributes;
    FActive: Boolean;
    FData: Pointer;
    FName: string;
    FKind: TEditorKind;
    FReadOnly: Boolean;
    FOwner: TInspectorEditors;
    function IsEqual(Editor: TInspectorEditor): Boolean;
    procedure InplaceButtonClick(Sender: TObject);
    procedure InplaceCustomDraw(Control: TWinControl; Rect: TRect;
      State: TOwnerDrawState; var DefaultDraw: Boolean);
    procedure InplaceKeyPress(Sender: TObject; var Key: Char);
    procedure SetName(const Value: string);
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    function GetInplaceEdit: TButtonEdit;
    procedure SetKind(Value: TEditorKind);
    procedure SetOwner(Value: TInspectorEditors);
    procedure SetReadOnly(Value: Boolean);
  protected
    procedure Change;
    procedure Click; dynamic;
    procedure DoSubmit;
    procedure DrawButton(Canvas: TCanvas; Rect: TRect); virtual;
    procedure DrawInline(Canvas: TCanvas; Rect: TRect); virtual;
    procedure Initialize; dynamic;
    procedure KeyPress(Editor: TButtonEdit; var Key: Char); dynamic;
    procedure SetActive(Value: Boolean); virtual;
    function GetAttributes: TEditorAttributes; virtual;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    property Active: Boolean read FActive write SetActive;
    property Attributes: TEditorAttributes read GetAttributes;
    property InplaceEdit: TButtonEdit read GetInplaceEdit;
    property Owner: TInspectorEditors read FOwner write SetOwner;
  public
    constructor Create(AOwner: TInspectorEditors); virtual;
    destructor Destroy; override;
    property Data: Pointer read FData write FData;
    property Index: Integer read GetIndex write SetIndex;
  published
    property Name: string read FName write SetName;
    property Kind: TEditorKind read FKind write SetKind;
    property Text: string read GetText write SetText;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  TInspectorEditorClass = class of TInspectorEditor;

{ TInspectorEditors }

  TInspectorEditors = class(TPersistent)
  private
    FInspector: TCustomInspector;
    FList: TList;
    FUpdateCount: Integer;
  protected
    procedure Change(Item: TInspectorEditor);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    function GetCount: Integer;
    function GetEditor(Index: Integer): TInspectorEditor;
    procedure SetEditor(Index: Integer; Value: TInspectorEditor);
    property Inspector: TCustomInspector read FInspector;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AOwner: TCustomInspector);
    destructor Destroy; override;
    function Add(Kind: TEditorKind): TInspectorEditor;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    procedure Insert(Item: TInspectorEditor; Index: Integer);
    procedure Remove(Item: TInspectorEditor);
    property Count: Integer read GetCount;
    property Editor[Index: Integer]: TInspectorEditor read GetEditor write
      SetEditor; default;
  end;

{ TInspectObject }

  TInspectObject = class(TPersistent)
  private
    FChanged: Boolean;
    FInspector: TCustomInspector;
    FOnChange: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnEdit: TNotifyEvent;
    procedure EditorValidated(Sender: TObject; Editor: TInspectorEditor);
  protected
    procedure BuildEditors(Editors: TInspectorEditors); virtual;
    procedure Change;
    procedure ConnectInspector; dynamic;
    procedure DisconnectInspector; dynamic;
    function EditorByName(const EditorName: string): TInspectorEditor;
    procedure UpdateField(Editor: TInspectorEditor); virtual;
    function GetCaption: string; virtual;
    procedure SetEditorText(const EditorName: string; const EditorText: string);
    property Changed: Boolean read FChanged write FChanged;
    property Inspector: TCustomInspector read FInspector;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Edit; dynamic;
    property Caption: string read GetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
  end;

  TInspectClass = class of TInspectObject;

{ TInspectContainer }

  TInspectContainer = class(TInspectObject)
  private
    FClearing: Boolean;
    FList: TList;
    function GetCount: Integer;
  protected
    procedure RemoveItem(Instance: TObject);
    procedure InsertItem(Instance: TObject);
    function Get(Index: Integer): TObject;
    function GetIndex(Instance: TObject): Integer;
    procedure SetIndex(Instance: TObject; Index: Integer);
    function GetOwnsObjects: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(Instance: TObject): Integer;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects;
  end;

{ TCustomInspector control }

  TInspectorEditorEvent = procedure(Sender: TObject; Editor: TInspectorEditor) of object;

  TInspectorStyle = (isClassic, isFlat);

  TCustomInspector = class(TScrollList)
  private
    FAutoPopup: Boolean;
    FDefEditProc: TWndMethod;
    FEditors: TInspectorEditors;
    FIncrementalSearch: Boolean;
    FInplaceEdit: TButtonEdit;
    FInspectObject: TInspectObject;
    FNewIndex: Integer;
    FReadOnly: Boolean;
    FShowExceptions: Boolean;
    FSplitPos: Integer;
    FStyle: TInspectorStyle;
    FWantEnter: Boolean;
    FOnEditorValidated: TInspectorEditorEvent;
    FOnInspect: TNotifyEvent;
    procedure EditProc(var Message: TMessage);
    procedure UpdateControls(Index: Integer);
    procedure SetEditors(Value: TInspectorEditors);
    procedure SetInspectObject(Value: TInspectObject);
    procedure SetReadOnly(Value: Boolean);
    function GetSelected: TInspectorEditor;
    procedure SetSelected(Value: TInspectorEditor);
    procedure SetSplitPos(Value: Integer);
    procedure SetStyle(Value: TInspectorStyle);
    procedure InplaceEditClick(Sender: TObject);
    procedure InplaceExit(Sender: TObject);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMGetTextLength(var Message: TMessage); message WM_GETTEXTLENGTH;
    procedure WMGetText(var Message: TWMGetText); message WM_GETTEXT;
  protected
    procedure CaptureMove(X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure HandleException(E: Exception); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure Revert(Editor: TInspectorEditor);
    procedure SelectItem(PriorIndex: Integer; NewIndex: Integer;
      var CanSelect: Boolean); override;
    procedure EditorChange(Item: TInspectorEditor);
    procedure EditorValidated(Editor: TInspectorEditor);
    property AutoPopup: Boolean read FAutoPopup write FAutoPopup default True;
    property Editors: TInspectorEditors read FEditors write SetEditors;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property IncrementalSearch: Boolean read FIncrementalSearch write FIncrementalSearch;
    property InplaceEdit: TButtonEdit read FInplaceEdit;
    property InspectObject: TInspectObject read FInspectObject write
      SetInspectObject;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property SplitPos: Integer read FSplitPos write SetSplitPos;
    property Style: TInspectorStyle read FStyle write SetStyle;
    property WantEnter: Boolean read FWantEnter write FWantEnter;
    property Selected: TInspectorEditor read GetSelected write SetSelected;
    property ShowExceptions: Boolean read FShowExceptions write FShowExceptions;
    property OnEditorValidated: TInspectorEditorEvent read FOnEditorValidated
      write FOnEditorValidated;
    property OnInspect: TNotifyEvent read FOnInspect write FOnInspect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Submit(Editor: TInspectorEditor; const Value: string): Boolean;
  end;

{ TInspector control }

  TInspector = class(TCustomInspector)
  public
    property InspectObject;
    property Selected;
    property Text;
  published
    property Align;
    property Anchors;
    property AutoPopup;
    property BorderStyle;
    property CharCase;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Editors;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property IncrementalSearch;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentCtl3D;
    property PopupMenu;
    property ReadOnly;
    property ShowExceptions;
    property SplitPos;
    property Style;
    property TabStop;
    property TabOrder;
    property UseDockManager;
    property WantEnter;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEditorValidated;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnInspect;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TEditorKind registration and enumeration routines}

type
  TEditorKindEnumProc = procedure(Kind: TEditorKind; Data: Pointer);

procedure RegisterEditorKind(Kind: TEditorKind; EditorClass: TInspectorEditorClass; const Name: string);
procedure EnumEditorKinds(EnumProc: TEditorKindEnumProc; Data: Pointer);
function EditorKindToClass(Kind: TEditorKind): TInspectorEditorClass;
function EditorKindToString(Kind: TEditorKind): string;
procedure GetEditorKindStrings(Strings: TStrings);

var RegisterInspectorUnits: procedure(AOwner: TComponent);

implementation

{ TInspectorEditor class }

constructor TInspectorEditor.Create(AOwner: TInspectorEditors);
begin
  inherited Create;
  FOwner := AOwner;
  FAttributes := Attributes;
  ReadOnly := False;
  Initialize;
  if FOwner <> nil then
  begin
    FOwner.FList.Add(Self);
    FOwner.Change(nil);
  end;
end;

destructor TInspectorEditor.Destroy;
begin
  Active := False;
  SetOwner(nil);
  inherited Destroy;
end;

procedure TInspectorEditor.Change;
begin
  if FOwner <> nil then
    FOwner.Change(Self);
end;

procedure TInspectorEditor.Click;
begin
end;

procedure TInspectorEditor.DoSubmit;
begin
  FOwner.FInspector.Submit(Self, Text);
end;

procedure TInspectorEditor.DrawButton(Canvas: TCanvas; Rect: TRect);
begin
end;

procedure TInspectorEditor.DrawInline(Canvas: TCanvas; Rect: TRect);
begin
end;

procedure TInspectorEditor.Initialize;
begin
end;

function TInspectorEditor.IsEqual(Editor: TInspectorEditor): Boolean;
begin
  Result := (Editor <> nil) and (Editor.Name = Name) and
    (Editor.Text = Text) and (Editor.Kind = Kind);
end;

procedure TInspectorEditor.KeyPress(Editor: TButtonEdit; var Key: Char);
begin
  if (FReadOnly) or FOwner.FInspector.ReadOnly then Exit;
  if Key = #13 then
  begin
    with FOwner.FInspector do
      if Submit(Self, Editor.Text) and (not WantEnter) then
        if ItemIndex < Count - 1 then
          ItemIndex := ItemIndex + 1
        else
          SendMessage(GetParentForm(FOwner.FInspector).Handle, WM_NEXTDLGCTL, 0 ,0);
    Key := #0;
  end;
end;

procedure TInspectorEditor.InplaceKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Sender as TButtonEdit, Key);
end;

procedure TInspectorEditor.InplaceButtonClick(Sender: TObject);
begin
  Click;
end;

procedure TInspectorEditor.InplaceCustomDraw(Control: TWinControl; Rect: TRect;
  State: TOwnerDrawState; var DefaultDraw: Boolean);
var
  ButtonEdit: TButtonEdit;
begin
  DefaultDraw := True;
  if (odComboBoxEdit in State) and (eaDrawButton in Attributes) then
  begin
    DefaultDraw := False;
    ButtonEdit := Control as TButtonEdit;
    DrawButton(ButtonEdit.Canvas, Rect);
  end;
end;

procedure TInspectorEditor.SetActive(Value: Boolean);
var
  Edit: TButtonEdit;
begin
  if FOwner <> nil then
  begin
    FActive := Value;
    Edit := FOwner.FInspector.InplaceEdit;
    if FActive then
    begin
      Edit.ReadOnly := (FReadOnly) or FOwner.FInspector.FReadOnly;
      if eaButton in FAttributes then
      begin
        if eaDrawButton in FAttributes then
          Edit.Style := beOwnerDrawn
        else if eaEllipseButton in FAttributes then
          Edit.Style := beEllipse
        else
          Edit.Style := beStandard;
        Edit.ButtonVisible := True;
      end
      else
      begin
        Edit.Style := beStandard;
        Edit.ButtonVisible := False;
      end;
      Edit.OnButtonClick := InplaceButtonClick;
      if eaDrawButton in FAttributes then
        Edit.OnCustomDraw := InplaceCustomDraw
      else
         Edit.OnCustomDraw := nil;
      if eaKeyPress in FAttributes then
         Edit.OnKeyPress := InplaceKeyPress
      else
         Edit.OnKeyPress := nil;
    end
    else
    begin
      Edit.OnButtonClick := nil;
      Edit.OnCustomDraw := nil;
      Edit.OnKeyPress := nil;
    end;
  end;
end;

function TInspectorEditor.GetIndex: Integer;
begin
  if FOwner <> nil then
    Result := FOwner.FList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TInspectorEditor.SetIndex(Value: Integer);
var
  I: Integer;
begin
  I := GetIndex;
  if (I > -1) and (I <> Value) then
  begin
    Active := False;
    FOwner.FList.Move(I, Value);
    FOwner.Change(nil);
  end;
end;

function TInspectorEditor.GetInplaceEdit: TButtonEdit;
begin
  Result := FOwner.FInspector.FInplaceEdit;
end;

function TInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := [eaKeyPress];
end;

procedure TInspectorEditor.SetKind(Value: TEditorKind);
var
  WasActive: Boolean;
  OwnerRef: TInspectorEditors;
  Editor: TInspectorEditor;
begin
  if Value <> FKind then
  begin
    OwnerRef := FOwner;
    WasActive := Active;
    Active := False;
    OwnerRef.BeginUpdate;
    try
      Editor := FOwner.Add(Value);
      Editor.Index := Index;
      Editor.Name := Name;
      Destroy;
    finally
      OwnerRef.EndUpdate;
    end;
    Editor.Active := WasActive;
  end;
end;

procedure TInspectorEditor.SetName(const Value: string);
begin
  FName := Value;
  Change;
end;

procedure TInspectorEditor.SetOwner(Value: TInspectorEditors);
begin
  if Value <> FOwner then
  begin
    Active := False;
    if FOwner <> nil then
      FOwner.Remove(Self);
    FOwner := Value;
    if FOwner <> nil then
      FOwner.FList.Add(Self);
    Change;
  end;
end;

function TInspectorEditor.GetText: string;
begin
  Result := '';
end;

procedure TInspectorEditor.SetText(const Value: string);
begin
end;

procedure TInspectorEditor.SetReadOnly(Value: Boolean);
begin
  if eaReadOnly in FAttributes then
    Value := True;
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    if FActive then SetActive(FActive);
  end;
end;

{ TInspectorEditors }

constructor TInspectorEditors.Create(AOwner: TCustomInspector);
begin
  inherited Create;
  FInspector := AOwner;
  FList := TList.Create;
end;

destructor TInspectorEditors.Destroy;
begin
  FUpdateCount := 1;
  Clear;
  inherited Destroy;
end;

function TInspectorEditors.Add(Kind: TEditorKind): TInspectorEditor;
var
  EditorClass: TInspectorEditorClass;
begin
   Result := nil;
   EditorClass := EditorKindToClass(Kind);
   if EditorClass <> nil then
   begin
     Result := EditorClass.Create(Self);
     Result.FKind := Kind;
   end;
end;

procedure TInspectorEditors.Assign(Source: TPersistent);
var
  InspectorEditors: TInspectorEditors absolute Source;
  I: Integer;
begin
  if Source is TInspectorEditors then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to InspectorEditors.Count - 1 do
        with Add(InspectorEditors[I].Kind) do
        begin
          Name := InspectorEditors[I].Name;
          Text := InspectorEditors[I].Text;
        end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TInspectorEditors.Insert(Item: TInspectorEditor; Index: Integer);
begin
  if Item.Owner <> Self then
  begin
    Item.Owner := nil;
    Item.FOwner := Self;
    FList.Insert(Index, Item);
    Change(nil);
  end;
end;

procedure TInspectorEditors.Remove(Item: TInspectorEditor);
begin
  FList.Remove(Item);
  Change(Item);
end;

procedure TInspectorEditors.Change(Item: TInspectorEditor);
begin
  if FUpdateCount < 1 then
    FInspector.EditorChange(Item);
end;

procedure TInspectorEditors.DefineProperties(Filer: TFiler);

  function WriteEditors: Boolean;
  var
    Editors: TInspectorEditors;
    I: Integer;
  begin
    Editors := TInspectorEditors(Filer.Ancestor);
    if Editors = nil then
      Result := Count > 0
    else if Editors.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Editor[I].IsEqual(Editors[I]);
        if Result then
          Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteEditors);
end;

procedure TInspectorEditors.ReadData(Stream: TStream);
var
  EditorCount: Integer;
  TextLength: Integer;
  EditorName: string;
  EditorText: string;
  EditorKind: TEditorKind;
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    Stream.ReadBuffer(EditorCount, SizeOf(Integer));
    for I := 0 to EditorCount - 1 do
    begin
      EditorName := '';
      Stream.ReadBuffer(TextLength, SizeOf(Integer));
      if TextLength > 0 then
      begin
        SetLength(EditorName, TextLength);
        Stream.ReadBuffer(Pointer(EditorName)^, TextLength);
      end;
      EditorText := '';
      Stream.ReadBuffer(TextLength, SizeOf(Integer));
      if TextLength > 0 then
      begin
        SetLength(EditorText, TextLength);
        Stream.ReadBuffer(Pointer(EditorText)^, TextLength);
      end;
      Stream.ReadBuffer(EditorKind, SizeOf(TEditorKind));
      with Add(EditorKind) do
      begin
        Name := EditorName;
        Text := EditorText;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TInspectorEditors.WriteData(Stream: TStream);
var
  EditorCount: Integer;
  TextLength: Integer;
  Text: string;
  EditorKind: TEditorKind;
  I: Integer;
begin
  EditorCount := Count;
  Stream.WriteBuffer(EditorCount, SizeOf(Integer));
  for I := 0 to EditorCount - 1 do
  begin
    Text := Editor[I].Name;
    TextLength := Length(Text);
    Stream.WriteBuffer(TextLength, SizeOf(Integer));
    if TextLength > 0 then
      Stream.WriteBuffer(Pointer(Text)^, TextLength);
    Text := Editor[I].Text;
    TextLength := Length(Text);
    Stream.WriteBuffer(TextLength, SizeOf(Integer));
    if TextLength > 0 then
      Stream.WriteBuffer(Pointer(Text)^, TextLength);
    EditorKind := Editor[I].Kind;
    Stream.WriteBuffer(EditorKind, SizeOf(TEditorKind));
  end;
end;

procedure TInspectorEditors.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TInspectorEditors.EndUpdate;
begin
  Dec(FUpdateCount);
  Change(nil);
end;

procedure TInspectorEditors.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := FList.Count - 1 downto 0 do
      TObject(FList[I]).Free;
  finally
    EndUpdate;
  end;
end;

function TInspectorEditors.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TInspectorEditors.GetEditor(Index: Integer): TInspectorEditor;
begin
  Result := TInspectorEditor(FList[Index]);
end;

procedure TInspectorEditors.SetEditor(Index: Integer; Value: TInspectorEditor);
begin
  TPersistent(FList[Index]).Assign(Value);
end;

{ TInspectObject }

constructor TInspectObject.Create;
begin
  inherited Create;
end;

destructor TInspectObject.Destroy;
begin
  if (FInspector <> nil) then
    FInspector.InspectObject := nil;
  inherited Destroy;
end;

procedure TInspectObject.Change;
begin
  FChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TInspectObject.Edit;
begin
  if Assigned(FOnEdit) then
    FOnEdit(Self);
end;

procedure TInspectObject.EditorValidated(Sender: TObject; Editor: TInspectorEditor);
begin
  UpdateField(Editor);
end;

procedure TInspectObject.BuildEditors(Editors: TInspectorEditors);
begin
end;

procedure TInspectObject.ConnectInspector;
begin
  if FInspector <> nil then
    BuildEditors(FInspector.FEditors);
end;

procedure TInspectObject.DisconnectInspector;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

function TInspectObject.EditorByName(const EditorName: string): TInspectorEditor;
var
  S: string;
  I: Integer;
begin
  Result := nil;
  S := UpperCase(EditorName);
  if FInspector <> nil then
    for I := 0 to FInspector.FEditors.Count - 1 do
      if UpperCase(FInspector.FEditors[I].Name) = S then
      begin
        Result := FInspector.FEditors[I];
        Break
      end;
end;

procedure TInspectObject.UpdateField(Editor: TInspectorEditor);
begin
end;

function TInspectObject.GetCaption: string;
begin
  Result := '';
end;

procedure TInspectObject.SetEditorText(const EditorName: string;
  const EditorText: string);
var
  Editor: TInspectorEditor;
begin
  if (Inspector <> nil) and (Inspector.InspectObject = Self) then
  begin
    Editor := EditorByName(EditorName);
    if Editor <> nil then
      Editor.Text := EditorText;
  end;
end;

{ TInspectContainer }

constructor TInspectContainer.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TInspectContainer.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TInspectContainer.Clear;
var
  I: Integer;
begin
  FClearing := True;
  try
    if OwnsObjects then
      for I := 0 to FList.Count - 1 do
        TObject(FList[I]).Free;
    FList.Clear;
  finally
    FClearing := False;
  end;
end;

function TInspectContainer.IndexOf(Instance: TObject): Integer;
begin
  Result := FList.IndexOf(Instance);
end;

function TInspectContainer.Get(Index: Integer): TObject;
begin
  Result := TObject(FList[Index]);
end;

function TInspectContainer.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TInspectContainer.RemoveItem(Instance: TObject);
begin
  if not FClearing then
    FList.Remove(Instance);
end;

procedure TInspectContainer.InsertItem(Instance: TObject);
begin
  FList.Add(Instance);
end;

function TInspectContainer.GetIndex(Instance: TObject): Integer;
begin
  Result := FList.IndexOf(Instance);
end;

procedure TInspectContainer.SetIndex(Instance: TObject; Index: Integer);
begin
  with FList do
    Move(IndexOf(Instance), Index);
end;

function TInspectContainer.GetOwnsObjects: Boolean;
begin
  Result := True;
end;

{ TCustomInspector }

var
  DottedLine: TBitmap;

constructor TCustomInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Font := Font;
  Count := 0;
  FAutoPopup := True;
  FEditors := TInspectorEditors.Create(Self);
  FInplaceEdit := TButtonEdit.Create(Self);
  with FInplaceEdit do
  begin
    Parent := Self;
    AutoHeight := False;
    BorderStyle:= bsNone;
    Ctl3D := False;
    Height := ItemHeight;
    Left := 0;
    Top := -Height;
    FDefEditProc := WindowProc;
    WindowProc := EditProc;
    Visible := False;
    OnButtonClick := InplaceEditClick;
    OnExit := InplaceExit;
  end;
  FSplitPos := Width div 2;
  if @RegisterInspectorUnits <> nil then RegisterInspectorUnits(AOwner);
end;

destructor TCustomInspector.Destroy;
begin
  InspectObject := nil;
  FEditors.Free;
  FInplaceEdit.WindowProc := FDefEditProc;
  FInplaceEdit.OnExit := nil;
  inherited Destroy;
end;

procedure TCustomInspector.CaptureMove(X, Y: Integer);
begin
  if Cursor = crHSplit then
    if X < 50 then
      SplitPos := 50
    else if X > ClientWidth - 50 then
      SplitPos := ClientWidth - 50
    else
      SplitPos := X
  else
    inherited CaptureMove(X, Y);
end;

procedure TCustomInspector.CreateWnd;
begin
  inherited CreateWnd;
  ItemHeight := Canvas.TextHeight('Wg') + 4;

end;

procedure TCustomInspector.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);

  procedure DrawClassicGrid;
  begin
    with Canvas, Rect do
    begin
      Brush.Color := Color;
      if dsHot in State then
      begin
        FillRect(Classes.Rect(Left, Top, FSplitPos, Bottom));
        if not FReadOnly then
          Brush.Color := clWindow;
        FillRect(Classes.Rect(FSplitPos + 1, Top, ClientWidth, Bottom));
      end
      else
        FillRect(Rect);
      Pen.Color := clBtnShadow;
      MoveTo(FSplitPos, Top);
      LineTo(FSplitPos, Bottom);
      Pen.Color := clBtnHighlight;
      MoveTo(FSplitPos + 1, Top);
      LineTo(FSplitPos + 1, Bottom);
      if Index = FNewIndex then
      begin
        if ThemePainter.Enabled then
          Pen.Color := clBtnShadow
        else
          Pen.Color := cl3DDkShadow;
        MoveTo(Left, Top);
        LineTo(Left, Bottom - 1);
        if ThemePainter.Enabled then
          Pen.Color := Color
        else
          Pen.Color := clBtnShadow;
        MoveTo(Left + 1, Top);
        LineTo(Left + 1, Bottom - 3);
        if ThemePainter.Enabled then
          Pen.Color := clBtnShadow
        else
          Pen.Color := clBtnHighlight;
        MoveTo(Left, Bottom - 1);
        LineTo(ClientWidth, Bottom - 1);
      end
      else if Index = FNewIndex - 1 then
      begin
        if ThemePainter.Enabled then
          Pen.Color := clBtnShadow
        else
          Pen.Color := cl3DDkShadow;
        MoveTo(Left, Bottom - 1);
        LineTo(Left, Bottom - 2);
        LineTo(ClientWidth, Bottom - 2);
        if ThemePainter.Enabled then
          Pen.Color := Color
        else
          Pen.Color := clBtnShadow;
        MoveTo(Left + 1, Bottom - 1);
        LineTo(ClientWidth, Bottom - 1);
      end
      else
      begin
        Brush.Bitmap := DottedLine;
        FillRect(Classes.Rect(Left, Bottom - 1, ClientWidth, Bottom));
        Brush.Bitmap := nil;
      end;
    end;
  end;

  procedure DrawFlatGrid;
  begin
    with Canvas, Rect do
    begin
      Brush.Color := Color;
      FillRect(Classes.Rect(Left, Top, FSplitPos, Bottom));
      Brush.Color := clWindow;
      FillRect(Classes.Rect(FSplitPos + 1, Top, ClientWidth, Bottom));
      if Index = FNewIndex then
        Font.Style := [fsBold]
      else
        Font.Style := [];
      Pen.Color := clBtnShadow;
      Pen.Width := 1;
      MoveTo(FSplitPos, Top);
      LineTo(FSplitPos, Bottom);
      Pen.Color := clBtnHighlight;
      MoveTo(FSplitPos + 1, Top);
      LineTo(FSplitPos + 1, Bottom);
      Brush.Bitmap := DottedLine;
      FillRect(Classes.Rect(Left, Bottom - 1, ClientWidth, Bottom));
      Brush.Bitmap := nil;
    end;
  end;

const
  DrawLeft = DT_LEFT or DT_TOP or DT_SINGLELINE;
var
  Editor: TInspectorEditor;
  DrawRect: TRect;
  PriorMode: Integer;
begin
  if FStyle = isClassic then
    DrawClassicGrid
  else
    DrawFlatGrid;
  DrawRect := Rect;
  with Canvas, DrawRect do
  begin
    Editor := FEditors[Index];
    Brush.Color := Color;
    Font.Color := clWindowText;
    Left := 5;
    Right := FSplitPos - 1;
    PriorMode := SetBkMode(Handle, TRANSPARENT);
    { TODO: Alter colors when read only }
    Dec(DrawRect.Bottom);
    DrawText(Handle, PChar(Editor.Name), -1, DrawRect, DrawLeft or DT_VCENTER);
    Left := FSplitPos + 4;
    Right := ClientWidth;
    if FStyle = isClassic then
      Font.Color := clNavy;
    if dsHot in State then
      Brush.Color := clWindow;
    { TODO: Alter colors when read only }
    if eaDrawInline in Editor.FAttributes then
      Editor.DrawInline(Canvas, DrawRect)
    else
      DrawText(Handle, PChar(Editor.Text), -1, DrawRect,
        DrawLeft or DT_VCENTER);
    SetBkMode(Handle, PriorMode);
  end;
end;

procedure TCustomInspector.HandleException(E: Exception);
begin
  { TODO: show exceptions here }
end;

procedure TCustomInspector.EditProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KEYDOWN, WM_KEYUP:
      begin
        Dispatch(Message);
        case Message.wParam of
          VK_UP, VK_DOWN: Message.wParam := 0;
          VK_ESCAPE:
            if ItemIndex > -1 then
              Revert(Editors[ItemIndex]);
        end;
      end;
    WM_CHAR:
      Dispatch(Message);
  end;
  FDefEditProc(Message);
end;

procedure TCustomInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;

  procedure DisplayPopupHint(const Hint: string; Point: TPoint; Show: Boolean);
  begin
    if Show then
    begin
      HintWindow.Canvas.Font := Font;
      HintWindow.Caption := Hint;
      HintWindow.Point := Point;
      HintWindow.Active := True;
    end
    else
      HintWindow.Active := False;
  end;

  procedure CalcPropHint(const Hint: string);
  begin
    DisplayPopupHint(Hint, Point(6, (I - TopIndex) * ItemHeight + 1),
      Canvas.TextWidth(Hint) + 10 > FSplitPos);
  end;

  procedure CalcTypeHint(const Hint: string);
  begin
    DisplayPopupHint(Hint, Point(FSplitPos + 5, (I - TopIndex) * ItemHeight + 1),
      Canvas.TextWidth(Hint) > ClientWidth - (FSplitPos + 4));
  end;

begin
  inherited;
  if Scrolling or MouseCapture then
    Exit;
  I := ItemAtPos(Point(X, Y), True);
  if I > -1 then
    if X < FSplitPos then
      CalcPropHint(FEditors[I].Name)
    else
      CalcTypeHint(FEditors[I].GetText)
  else
    HintWindow.Active := False;
end;

procedure TCustomInspector.Resize;
begin
  UpdateControls(ItemIndex);
end;

procedure TCustomInspector.Revert(Editor: TInspectorEditor);
begin
  if Editor <> nil then
    with FInplaceEdit do
    begin
      Text := Editor.Text;
      SelStart := 0;
      if FReadOnly then
        SelLength := 0
      else
        SelLength := High(Word);
      SetFocus;
    end;
end;

procedure TCustomInspector.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  if PriorIndex > -1 then
    InvalidateItem(PriorIndex);
  if PriorIndex > 0 then
    InvalidateItem(PriorIndex - 1);
  if NewIndex > 0 then
    InvalidateItem(NewIndex - 1);
  if PriorIndex > -1 then
  begin
    Submit(FEditors[PriorIndex], FInplaceEdit.Text);
    FEditors[PriorIndex].Active := False;
  end;
  if NewIndex > -1 then
  begin
    FEditors[NewIndex].Active := True;
    FInplaceEdit.Text := FEditors[NewIndex].Text;
    InplaceEdit.Visible := True;
    InplaceEdit.Invalidate;
  end;
  UpdateControls(NewIndex);
  FNewIndex := NewIndex;
  UpdateWindow(Handle);
  inherited SelectItem(PriorIndex, NewIndex, CanSelect);
end;

function TCustomInspector.Submit(Editor: TInspectorEditor; const Value: string): Boolean;
begin
  Result := False;
  try
    Editor.Text := Value;
    EditorValidated(Editor);
    Result := True;
  except
    on E: Exception do
    begin
      HandleException(E);
      Revert(Editor);
    end
  end;
end;

procedure TCustomInspector.EditorValidated(Editor: TInspectorEditor);
begin
  if Assigned(FOnEditorValidated) then
    FOnEditorValidated(Self, Editor);
end;

procedure TCustomInspector.CMDesignHitTest(var Message: TCMDesignHitTest);
const
  HitTests: array[Boolean] of Integer = (0, 1);
begin
  inherited;
  with Message do
    Result := HitTests[(XPos > FSplitPos - 3) and (XPos < FSplitPos - 3)];
end;

procedure TCustomInspector.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight('Wg') + 4;
  if ItemIndex > -1 then
    UpdateControls(ItemIndex);
  Invalidate;
end;

procedure TCustomInspector.WMSetCursor(var Message: TWMSetCursor);
var
  Pt: TPoint;
begin
  GetCursorPos(Pt);
  Windows.ScreenToClient(Handle, Pt);
  if Count > 0 then
    if (Pt.x > FSplitPos - 3) and (Pt.x < FSplitPos + 3) then
      Cursor := crHSplit
    else
      Cursor := crArrow;
  inherited;
end;

procedure TCustomInspector.WMSetFocus(var Message: TWMSetFocus);
begin
  UpdateControls(ItemIndex);
end;

procedure TCustomInspector.WMGetTextLength(var Message: TMessage);
begin
  Message.Result := Length(FInplaceEdit.Text);
end;

procedure TCustomInspector.WMGetText(var Message: TWMGetText);
begin
  with Message do
  begin
    Result := Length(FInplaceEdit.Text);
    if Result > TextMax then
      Result := TextMax;
    StrPLCopy(Text, FInplaceEdit.Text, Result)
  end;
end;

procedure TCustomInspector.UpdateControls(Index: Integer);
var
  Wnd: HWND;
begin
  with FInplaceEdit do
    if Index > -1 then
    begin
      with ItemRect(Index) do
        SetBounds(SplitPos + 4, Top, Right - SplitPos - 4, ItemHeight - 1);
      Visible := True;
      Wnd := GetFocus;
      if (Wnd = Self.Handle) or IsChild(Self.Handle, Wnd) then
      begin
        SetFocus;
        if FReadOnly then
          SelLength := 0;
      end;
      Invalidate;
    end
    else
      Visible := False;
end;

procedure TCustomInspector.EditorChange(Item: TInspectorEditor);
var
  S: string;
begin
  Count := FEditors.Count;
  if Item <> nil then
  begin
    if (Item.Index = ItemIndex) and (Item.Text <> FInplaceEdit.Text) then
      FInplaceEdit.Text := Item.Text;
    InvalidateItem(Item.Index);
  end
  else
  begin
    if Count > 0 then
      S := FEditors[0].Text
    else
      FInplaceEdit.Top := - ItemHeight * 2;   
    ItemIndex := 0;
    UpdateControls(ItemIndex);
    if Count > 0 then
    begin
      Editors[ItemIndex].Active := True;
      FInplaceEdit.Text := '';
      FInplaceEdit.SelText := S;
    end;
    Invalidate;
  end;
end;

function TCustomInspector.GetCharCase: TEditCharCase;
begin
  Result := FInplaceEdit.CharCase;
end;

procedure TCustomInspector.SetCharCase(const Value: TEditCharCase);
begin
  FInplaceEdit.CharCase := Value;
end;

procedure TCustomInspector.SetEditors(Value: TInspectorEditors);
var
	Form: TCustomForm;
begin
  FEditors.Assign(Value);
  if Owner is TCustomForm then
  begin
  	Form := Owner as TCustomForm;
    if Form.Designer <> nil then
			Form.Designer.Modified;
  end;
end;

procedure TCustomInspector.SetInspectObject(Value: TInspectObject);
begin
  if Value <> FInspectObject then
  begin
    OnEditorValidated := nil;
    FEditors.BeginUpdate;
    try
      FEditors.Clear;
      if FInspectObject <> nil then
      begin
        FInspectObject.DisconnectInspector;
        FInspectObject.FInspector := nil;
      end;
      FInspectObject := Value;
      if FInspectObject <> nil then
      begin
        FInspectObject.FInspector := Self;
        FInspectObject.ConnectInspector;
        OnEditorValidated := FInspectObject.EditorValidated;
      end;
    finally
      FEditors.EndUpdate;
    end;
    if Assigned(FOnInspect) then
      FOnInspect(Self);
  end;
end;

procedure TCustomInspector.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    FInplaceEdit.ReadOnly := Value;
    TCustomInspector(FInplaceEdit).Color := EnabledColors[not FReadOnly];
    Invalidate;
  end;
end;

function TCustomInspector.GetSelected: TInspectorEditor;
begin
  if ItemIndex > -1 then
    Result := FEditors[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomInspector.SetSelected(Value: TInspectorEditor);
var
  I: Integer;
begin
  I := FEditors.FList.IndexOf(Value);
  if I > -1 then
    ItemIndex := I;
end;

procedure TCustomInspector.SetSplitPos(Value: Integer);
var
  Delta: Integer;
  ClipRect: TRect;
  I: Integer;
begin
  if Value <> FSplitPos then
  begin
    Delta := Value - FSplitPos;
    FSplitPos := Value;
    ClipRect := Rect(FSplitPos, 0, ClientWidth + 200, ClientHeight);
    ScrollWindowEx(Handle, Delta, 0, @ClipRect, @ClipRect, 0, nil, SW_INVALIDATE);
    if Delta > 0 then
      ClipRect := Rect(FSplitPos - Delta - 1, 0, FSplitPos + Delta, ClientHeight)
    else
      ClipRect := Rect(ClientWidth + Delta, 0, ClientWidth + 1, ClientHeight);
    InvalidateRect(Handle, @ClipRect, False);
    ClipRect := Rect(FSplitPos, ItemHeight - 1, ClientWidth + 1, ItemHeight);
    for I := TopIndex to Count do
    begin
      InvalidateRect(Handle, @ClipRect, False);
      OffsetRect(ClipRect, 0, ItemHeight);
    end;
    UpdateControls(ItemIndex);
    UpdateWindow(Handle);
  end
end;

procedure TCustomInspector.SetStyle(Value: TInspectorStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TCustomInspector.InplaceEditClick(Sender: TObject);
begin
  if ItemIndex > -1 then
    FEditors[ItemIndex].Click;
end;

procedure TCustomInspector.InplaceExit(Sender: TObject);
var
  Wnd: HWND;
begin
  // Scrolling := False;
  // MouseCapture := False;
  Wnd := GetFocus;
  if not ((Wnd = Handle) or IsChild(Handle, Wnd)) then
   if ItemIndex > -1 then
     Submit(Editors[ItemIndex], InplaceEdit.Text)
end;

procedure BuildBitmap(Bitmap: TBitmap);
const
  PixelColors: array[Boolean] of TColor = (clBtnFace, clBtnShadow);
var
  Col: Integer;
  Row: Integer;
begin
  with Bitmap do
  begin
    Height := 8;
    Width := 8;
    for Col := 0 to Width - 1 do
      for Row := 0 to Height - 1 do
        Canvas.Pixels[Col, Row] := PixelColors[Odd(Col + Row)];
  end;
end;

{ EditorKind routines }

type
  PEditorKindLink = ^TEditorKindLink;
  TEditorKindLink = record
    Kind: TEditorKind;
    EditorClass: TInspectorEditorClass;
    Name: string;
    Link: PEditorKindLink;
  end;

var
  EditorLinks: PEditorKindLink;

procedure RegisterEditorKind(Kind: TEditorKind;
  EditorClass: TInspectorEditorClass; const Name: string);
var
  S: string;

  procedure AddLink(var Link: PEditorKindLink);
  begin
    if Link = nil then
    begin
      New(Link);
      Link.Kind := Kind;
      Link.EditorClass := EditorClass;
      Link.Name := Name;
      Link.Link := nil;
    end
    else if (Link.Kind <> Kind)  and (UpperCase(Link.Name) <> S) then
      AddLink(Link.Link);
  end;

begin
  S := UpperCase(Name);
  AddLink(EditorLinks);
end;

procedure EnumEditorKinds(EnumProc: TEditorKindEnumProc; Data: Pointer);
var
  Link: PEditorKindLink;
begin
  Link := EditorLinks;
  while Link <> nil do
  begin
    EnumProc(Link.Kind, Data);
    Link := Link.Link;
  end;
end;

function EditorKindToClass(Kind: TEditorKind): TInspectorEditorClass;
var
  Link: PEditorKindLink;
begin
  Result := nil;
  Link := EditorLinks;
  while Link <> nil do
  begin
    if Link.Kind = Kind then
    begin
      Result := Link.EditorClass;
      Break;
    end;
    Link := Link.Link;
  end;
end;

function EditorKindToString(Kind: TEditorKind): string;
var
  Link: PEditorKindLink;
begin
  Result := '';
  Link := EditorLinks;
  while Link <> nil do
  begin
    if Link.Kind = Kind then
    begin
      Result := Link.Name;
      Break;
    end;
    Link := Link.Link;
  end;
end;

procedure GetEditorKindStrings(Strings: TStrings);
var
  Link: PEditorKindLink;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Link := EditorLinks;
    while Link <> nil do
    begin
      Strings.AddObject(Link.Name, TObject(Link.Kind));
      Link := Link.Link;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure DisposeLinks;

  procedure DisposeLink(Link: PEditorKindLink);
  begin
    if Link <> nil then
    begin
      DisposeLink(Link.Link);
      Dispose(Link);
    end;
  end;

begin
  DisposeLink(EditorLinks);
end;

initialization
  EditorLinks := nil;
  DottedLine := TBitmap.Create;
  BuildBitmap(DottedLine);
finalization
  DottedLine.Free;
  DisposeLinks;
end.
