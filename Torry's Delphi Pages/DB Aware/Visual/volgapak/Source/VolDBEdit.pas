//---------------------------------------------------------------------------
//  TVolgaDBEdit - inherited from TCustomMaskEdit
//  Supports styles: vdsEdit,vdsCustomDlg,vdsCombo,vdsLookup,vdsCalendar,vdsTree,
//  vdsCalculator
//  May be used as DB-aware and without setting DataField and DataSource
//  Supports ENTER and cursor buttons for navigation to next and previous TVolgaDBEdit
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
unit VolDBEdit;

interface

uses
  Forms, SysUtils, Windows, Graphics, Messages, Classes, VolDBConst,
{$IFDEF VER140} Variants, {$ENDIF}
{$IFDEF VER150} Variants, {$ENDIF}
  Controls, Buttons, Mask, dbctrls, db, stdctrls, VolCalend, ComCtrls,
  VolCalc;

type

  TVolgaComboBoxStyle = (vcsDropDown, vcsDropDownList);
  TVolgaDialogStyle = (vdsEdit, vdsCustomDlg, vdsCombo, vdsLookup, vdsCalendar,
    vdsTree, vdsCalculator);
  TVolgaKindSearch = (vksFindFirst, vksFilter);

  //текст контроля для отображения
  TGetTextEvent = function(Sender: TObject; FieldValue: string): string of object;
  //текст контроля для сохранения изменений
  TSetTextEvent = procedure(Sender: TObject; FieldValue: string) of object;
  TCloseUpEvent = procedure(Sender: TObject; Selected: Boolean) of object;

{ TVolgaPopupData}

  TVolgaPopupData = class(TPopupDataList)
  private
    FSearchText: string;
    FKindSearch: TVolgaKindSearch;
    FOldFiltered: Boolean;
    FOldFilterRecord: TFilterRecordEvent;
    FViewField: string;
    FFilterSet: Boolean;
    procedure AFilterRecord(DataSet: TDataSet; var Accept: Boolean);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure SetFilter;
    procedure ResetFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property KindSearch: TVolgaKindSearch read FKindSearch write FKindSearch;
  end;

{ TVolgaListbox }

  TVolgaListbox = class(TCustomListBox)
  private
    FSearchText: string;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TVolgaDlgCalendar }

  TVolgaDlgCalendar = class(TVolgaCalendar)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TVolgaPopupTree }

  TVolgaPopupTree = class(TCustomTreeView)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetFullCaption(UniqueID: string; Delimiter: char): string;
    function GetFullNodeCaption(Node: TTreeNode; Delimiter: char): string;
    function FindNodeByUniqueID(UniqueID: string): TTreeNode;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVolgaDBEdit = class;

  TVolgaComboProperties = class(TPersistent)
  private
    FOwner: TVolgaDBEdit;
    FItems: TStrings;
    FValues: TStrings;
    function GetComboItems: TStrings;
    function GetComboValues: TStrings;
    procedure SetComboItems(const Value: TStrings);
    procedure SetComboValues(const Value: TStrings);
    function GetHasValues: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AssignList(const AList: TStrings);
    property HasValues: Boolean read GetHasValues;
  published
    property ComboItems: TStrings read GetComboItems write SetComboItems;
    property ComboValues: TStrings read GetComboValues write SetComboValues;
  end;

  TVolgaLookupProperties = class(TPersistent)
  private
    FOwner: TVolgaDBEdit;
    FListFieldIndex: Integer;
    FLookupKeyField: string;
    FListFieldNames: string;
    FLFields: TList;
    FSourceKeyField: string;
    FKindSearch: TVolgaKindSearch;
    procedure SetLookupKeyField(const Value: string);
    procedure SetListFieldNames(const Value: string);
    function GetViewField: string;
    procedure PrepareLookup;
    procedure SetSourceKeyField(const Value: string);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property Owner: TVolgaDBEdit read FOwner;
    property ViewField: string read GetViewField;  //видимое поле, которое видно после закрытия Lookup'a
  published
    property KindSearch: TVolgaKindSearch read FKindSearch write FKindSearch default
    vksFindFirst;
    property SourceKeyField: string read FSourceKeyField write SetSourceKeyField;
    property LookupKeyField: string read FLookupKeyField write SetLookupKeyField;
    property ListFieldNames: string read FListFieldNames write SetListFieldNames;
    property ViewFieldIndex: Integer read FListFieldIndex write FListFieldIndex default
    0;
  end;

  TVolgaTreeProperties = class(TPersistent)
  private
    FOwner: TVolgaDBEdit;
    FUniqueField: string;
    FLevelField: string;
    FSeparator: char;
    FTextField: string;
  public
    constructor Create(AOwner: TComponent);
    procedure PrepareTree;
    property Owner: TVolgaDBEdit read FOwner;
  published
    property LevelField: string read FLevelField write FLevelField;
    property PathSeparator: char read FSeparator write FSeparator default '/';
    property TextField: string read FTextField write FTextField;
    property UniqueField: string read FUniqueField write FUniqueField;
  end;

  TVolgaDBEdit = class(TCustomMaskEdit)  //TCustomMaskEdit
  private
    FButtonWidth: Integer;
    FDroppedDown: Boolean;
    FCanvas: TControlCanvas;
    FTracking: Boolean;
    FPressed: Boolean;
    FLookupList: TVolgaPopupData;
    FComboList: TVolgaListbox;
    FTreeList: TVolgaPopupTree;
    FCalendar: TVolgaDlgCalendar;
    FCalculator: TVolgaCalculator;
    FActiveList: TWinControl;
    FOnCustomDlg: TNotifyEvent;
    FStyle: TVolgaComboBoxStyle;
    FDataLink: TFieldDataLink;
    FReadOnly: Boolean;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FOnDisplayText: TGetTextEvent;
    FOnUpdateData: TSetTextEvent;
    FDialogStyle: TVolgaDialogStyle;
    FDropDownWidth: Cardinal;
    FDropDownRows: Cardinal;
    FComboProps: TVolgaComboProperties;
    FLookupProps: TVolgaLookupProperties;
    FTreeProps: TVolgaTreeProperties;
    FAutoDrop: Boolean;
    FNextControl: integer;
    FOnCloseUp: TCloseUpEvent;
    FValue: Variant;
    FClearValue: string;
    FPrepared: Boolean;
    FLookupSource: TDatasource;
    FLookupDataSet: TDataSet;
    FOnDropDown: TNotifyEvent;
    FAsTab: Boolean;
    BmpCalc: TBitmap;
    BmpCaln: TBitmap;
    function ButtonRect: TRect;
    function GetSelectedValue: Variant;
    procedure SetViewText(AValue: Variant; AText: string);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetDialogStyle(const Value: TVolgaDialogStyle);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetComboProps: TVolgaComboProperties;
    procedure SetComboProps(const Value: TVolgaComboProperties);
    procedure UpdateData(Sender: TObject);
    procedure SetEditRect;
    procedure CalSelectDate(Sender: TObject);
    procedure CalcExit(Sender: TObject; Selected: Boolean);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function OverButton(const P: TPoint): Boolean;
    procedure CheckDB;
    function DBLinked: Boolean;
    function GetViewTextNonDB: string;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SetFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    function GetLookupProps: TVolgaLookupProperties;
    procedure SetLookupProps(const Value: TVolgaLookupProperties);
    function GetTreeProps: TVolgaTreeProperties;
    procedure SetTreeProps(const Value: TVolgaTreeProperties);
    procedure SetValue(const Value: Variant);
    procedure SetLookupDataSet(const Value: TDataSet);
    function GetItemIndex: integer;
    procedure SetItemIndex(const AValue: integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonWidth(const Value: integer);
  protected
    procedure Change; override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    function Editable: boolean;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Reset; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure DropDown; virtual;
    procedure CreateDropDownList;       //создать-обновить выпад.лист после изменений
    function IsLinkActive: Boolean;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Value: Variant read FValue write SetValue;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Alignment: TAlignment read FAlignment write SetAlignment default
    taLeftJustify;
    property AutoDropDown: Boolean read FAutoDrop write FAutoDrop default false;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 15;
    property BorderStyle;
    property CharCase;
    property ClearValue: string read FClearValue write FClearValue;
    property Color;
    property Constraints;
    property ComboProps: TVolgaComboProperties read GetComboProps write SetComboProps;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
//  property DragCursor;
//  property DragMode;
    property DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 10;
    property DropDownWidth: Cardinal read FDropDownWidth write FDropDownWidth default 0;
    property DialogStyle: TVolgaDialogStyle read FDialogStyle write SetDialogStyle default
    vdsEdit;
    property EditMask;
    property Enabled;
    property EnterAsTab: Boolean read FAsTab write FAsTab default true;
    property Font;
    property LookupDataSet: TDataSet read FLookupDataSet write SetLookupDataSet; //Volga
    property LookupProps: TVolgaLookupProperties read GetLookupProps write SetLookupProps;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Style: TVolgaComboBoxStyle read FStyle write FStyle default vcsDropDown;
    property TabOrder;
    property TabStop;
    property TreeProps: TVolgaTreeProperties read GetTreeProps write SetTreeProps;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TCloseUpEvent read FOnCloseUp write FOnCloseUp;
    property OnCustomDlg: TNotifyEvent read FOnCustomDlg write FOnCustomDlg;
    property OnDisplayText: TGetTextEvent read FOnDisplayText write FOnDisplayText;
    property OnUpdateData: TSetTextEvent read FOnUpdateData write FOnUpdateData;
//  property OnDragDrop;
//  property OnDragOver;
//  property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{$R VOLDBEDIT.RES}

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

{ TVolgaListbox }

constructor TVolgaListbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
end;

procedure TVolgaListbox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TVolgaListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TVolgaListbox.KeyPress(var Key: Char);
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

{ TVolgaPopupData }

constructor TVolgaPopupData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldFiltered := false;
  FOldFilterRecord := nil;
  FKindSearch := vksFindFirst;
  FViewField := '';
  FFilterSet := false;
end;

destructor TVolgaPopupData.Destroy;
begin
  ResetFilter;
  inherited Destroy;
end;

procedure TVolgaPopupData.ResetFilter;
begin
  with ListLink do
    if Active and (FKindSearch = vksFilter) and FFilterSet then
    begin
      Dataset.OnFilterRecord := FOldFilterRecord;
      Dataset.Filtered := FOldFiltered;
    end;
  FFilterSet := false;
end;

procedure TVolgaPopupData.SetFilter;
var Func1, Func2: TFilterRecordEvent;
  FL: TList;
begin
  if (FKindSearch = vksFindFirst) or (ListLink.DataSet = nil) then Exit;
  FSearchText := '';
  FL := TList.Create;
  ListLink.DataSet.GetFieldList(FL, ListField);
  FViewField := TField(FL[ListFieldIndex]).FieldName;
  FL.Free;
  Func1 := ListLink.DataSet.OnFilterRecord;
  Func2 := AFilterRecord;
  if ListLink.Active and (@Func1 <> @Func2) then
  begin
    FFilterSet := true;
    FOldFilterRecord := ListLink.DataSet.OnFilterRecord;
    FOldFiltered := ListLink.DataSet.Filtered;
    ListLink.DataSet.OnFilterRecord := AFilterRecord;
  end;
end;

procedure TVolgaPopupData.KeyPress(var Key: Char);
begin
  if KindSearch = vksFindFirst then
    inherited KeyPress(Key)
    //ProcessSearchKey(Key)
  else
  begin
    //if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
    case Key of
      #8: FSearchText := Copy(FSearchText, 1, Length(FSearchText) - 1); //BackSpace
      #27: FSearchText := '';           //ESC
      #32..#255:
        begin
          if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
          Key := #0;
        end;
    end;
    ListLink.DataSet.Filtered := FSearchText > '';
  end;
end;

procedure TVolgaPopupData.AFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := true;
  if FOldFiltered and Assigned(FOldFilterRecord) then
    FOldFilterRecord(DataSet, Accept);
  if not Accept then Exit;
  Accept := Pos(AnsiUpperCase(FSearchText),
    AnsiUpperCase(DataSet.FieldByName(FViewField).AsString)) > 0
end;

{ TVolgaDlgCalendar }

constructor TVolgaDlgCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
end;

procedure TVolgaDlgCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TVolgaDlgCalendar.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

{ TVolgaPopupTree }

constructor TVolgaPopupTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  ShowLines := true;
end;

procedure TVolgaPopupTree.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TVolgaPopupTree.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

function TVolgaPopupTree.FindNodeByUniqueID(UniqueID: string): TTreeNode;
var i: integer;
begin
  {найти Node,соотв.уник.коду записи}
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if PShortString(Items[i].Data)^ = UniqueID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TVolgaPopupTree.GetFullCaption(UniqueID: string; Delimiter: char): string;
{полное наименование Node по его уник.коду,начиная с первого родителя}
var FindNode: TTreeNode;
begin
  FindNode := FindNodeByUniqueID(UniqueID);
  Result := GetFullNodeCaption(FindNode, Delimiter);
end;

function TVolgaPopupTree.GetFullNodeCaption(Node: TTreeNode; Delimiter: char): string;
{полное наименование Node,начиная с первого родителя}
var ParentNode: TTreeNode;
begin
  if Node <> nil then
  begin
    Result := Node.Text;
    if Node.Level = 0 then Exit;        //дошли до самого верхнего уровня
    ParentNode := Node.Parent;
    while (ParentNode <> nil) and (ParentNode.Level >= 0) do
    begin
      Result := ParentNode.Text + Delimiter + Result;
      try ParentNode := ParentNode.Parent;
      except;
      end;
    end;
  end
  else
    Result := '';
end;

{ TVolgaDBEdit }

constructor TVolgaDBEdit.Create(AOwner: tcomponent);
begin
  inherited Create(AOwner);
  //inherited ReadOnly := true;
  ControlStyle := ControlStyle + [csReplicatable] - [csDoubleClicks];
  DropDownRows := 10;
  FDialogStyle := vdsEdit;
  FAutoDrop := false;
  FStyle := vcsDropDown;
  FNextControl := 0;
  FActiveList := nil;
  FAlignment := taLeftJustify;
  FPrepared := false;
  FAsTab := true;
  FValue := '';
  FClearValue := '';
  FValue := ClearValue;                 //пустая строка
  FButtonWidth := 15;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnActiveChange := ActiveChange;
  FDataLink.OnEditingChange := EditingChange; //вошли или вышли из режима редактирования
  FDataLink.OnUpdateData := UpdateData;
  BmpCalc := TBitmap.Create;
  BmpCalc.LoadFromResourceName(HInstance, 'TVCALCULAT');
  BmpCaln := TBitmap.Create;
  BmpCaln.LoadFromResourceName(HInstance, 'TVCALENDAR');
  Invalidate;
end;

destructor TVolgaDBEdit.Destroy;
begin
  FDataLink.Control := nil;
  FDataLink.Free;
  FDataLink := nil;
  if Assigned(FLookupList) then         //отцепили lookup-датасет
    FLookupList.ListSource := nil;
  FLookupSource.Dataset := nil;
  FLookupSource.Free;
  FLookupSource := nil;
  if Assigned(FComboProps) then FComboProps.Free;
  FComboProps := nil;
  if Assigned(FLookupProps) then FLookupProps.Free;
  FLookupProps := nil;
  if Assigned(FTreeProps) then FTreeProps.Free;
  FTreeProps := nil;
  FCanvas.Free;
  BmpCalc.Free;
  BmpCaln.Free;
  //выпадающие контроли дестроятся сами, т.к. у них есть owner
  inherited Destroy;
end;

procedure TVolgaDBEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
    begin
      DataSource := nil;
      FPrepared := false;
    end;
    if (LookupDataSet = AComponent) then
    begin
      LookupDataSet := nil;
      FPrepared := false;
    end;
  end;
end;

procedure TVolgaDBEdit.CreateWnd;
begin
  inherited CreateWnd;
  if DialogStyle <> vdsEdit then
    SetEditRect;
end;

procedure TVolgaDBEdit.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, 0, ClientWidth - FButtonWidth - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  if not DBLinked and not (csDesigning in ComponentState) then
    CreateDropDownList;
end;

procedure TVolgaDBEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not (ES_AUTOVSCROLL or ES_WANTRETURN) or
    WS_CLIPCHILDREN or ES_MULTILINE or Alignments[FAlignment];
  //ES_MULTILINE - чтобы сработал ES_RIGHT, ES_CENTER
end;

function TVolgaDBEdit.ButtonRect: TRect;
var R: TRect;
begin
  R := ClientRect;
  Result := Rect(R.Right - FButtonWidth, R.Top, R.Right, R.Bottom);
end;

procedure TVolgaDBEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
begin
   //рисуем кнопку
  if FDialogStyle <> vdsEdit then
  begin
    R := ButtonRect;                    //регион, занимаемый кнопкой
    Flags := 0;
    if FDialogStyle in [vdsCombo, vdsLookup, vdsTree] then
    begin
      if FActiveList = nil then         //выпадающий контроль не создан
        Flags := DFCS_INACTIVE
      else if FPressed then             //кнопка рисуется нажатой
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else                                { vdsCustomDlg }
    begin
      if FPressed then Flags := BF_FLAT; //кнопка рисуется нажатой
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      case FDialogStyle of
        vdsCustomDlg:
          begin
            X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);
            Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(FPressed);
            W := FButtonWidth shr 3;
            if W = 0 then W := 1;
            PatBlt(DC, X, Y, W, W, BLACKNESS); //рисуются три точки
            PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
            PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
          end;
          vdsCalendar:
          begin
            X := R.Left + ((R.Right - R.Left - BmpCaln.Width) shr 1) + Ord(FPressed);
            Y := R.Top + ((R.Bottom - R.Top - BmpCaln.Height) shr 1) + Ord(FPressed);
            BitBlt(DC, X, Y, BmpCaln.Width, BmpCaln.Height, BmpCaln.Canvas.Handle, 0, 0,
              SRCCOPY);
          end;
        vdsCalculator:
          begin
            X := R.Left + ((R.Right - R.Left - BmpCalc.Width) shr 1) + Ord(FPressed);
            Y := R.Top + ((R.Bottom - R.Top - BmpCalc.Height) shr 1) + Ord(FPressed);
            BitBlt(DC, X, Y, BmpCalc.Width, BmpCalc.Height, BmpCalc.Canvas.Handle, 0, 0,
              SRCCOPY);
          end;
      end;
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TVolgaDBEdit.CMCancelMode(var Message: TCMCancelMode);
begin //закрыть выпадающий контроль, если message послан не нашему компоненту
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    if (Message.Sender <> nil) and (Message.Sender.Parent <> FActiveList)
      or (Message.Sender = nil) then
      CloseUp(False);
end;

procedure TVolgaDBEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;                         //рисуем кнопку отжатой
  inherited;
end;

procedure TVolgaDBEdit.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if FDroppedDown then
    CloseUp(False);
end;

procedure TVolgaDBEdit.SetViewText(AValue: Variant; AText: string);
var i: integer;
  strValue: string;
begin //установить отображаемый текст по значению из поля БД
  if (csDestroying in ComponentState) then Exit;
  if not FPrepared then CreateDropDownList;
  if not VarIsArray(AValue) and not VarIsNull(AValue) then
    strValue := AText
  else
    strValue := '';
  if not FPrepared then
    Text := strValue
  else
  begin
    if Assigned(FOnDisplayText) then    //показываем текст, calculated по полю!!!
      Text := FOnDisplayText(self, strValue)
    else if Assigned(FActiveList) then
    begin
      if FActiveList = FComboList then
      begin
        if ComboProps.HasValues then
        begin                           //текст с подстановкой из выпадающего списка
          i := ComboProps.ComboValues.IndexOf(strValue);
          if i >= 0 then
            Text := ComboProps.ComboItems[i]
          else
            Text := '';
        end
        else if Style = vcsDropDownList then
        begin
          i := ComboProps.ComboItems.IndexOf(strValue);
          if i >= 0 then
            Text := strValue
          else
            Text := '';
        end
        else
          Text := strValue;
      end
      else if FActiveList = FLookupList then
      begin
        //по коду ищем текст
        if not VarIsNull(AValue) and IsLinkActive and
          FLookupDataSet.Locate(LookupProps.LookupKeyField, AValue, []) then  //здесь Value м.б. сразу 2 поля!!
          Text := FLookupDataSet.FieldByName(LookupProps.ViewField).Text
        else if Style = vcsDropDownList then
          Text := ''
        else
          Text := strValue;
      end
      else if FActiveList = FTreeList then
      begin
        //по коду ищем текст - полное наименование группы
        if not VarIsNull(AValue) and (FTreeList.Items.Count > 0) then
          Text := FTreeList.GetFullCaption(strValue, TreeProps.PathSeparator)
        else
          Text := '';
      end
      else if (Pos('.00.', strValue) > 0) or (Pos('/00/', strValue) > 0) then
        Text := ''
      else
        Text := strValue;               //для Calendar
    end
    else
      Text := strValue;
  end;
  EditText := Text;
  case CharCase of
    ecUpperCase: EditText := AnsiUpperCase(EditText);
    ecLowerCase: EditText := AnsiLowerCase(EditText);
  end;
end;

function TVolgaDBEdit.GetViewTextNonDB: string;
var ind: integer;
begin //установить отображаемый текст по значению,выбранному из выпад.списка, если нет БД
  if (csDestroying in ComponentState) then Exit;
  if not FPrepared then CreateDropDownList;
  if not FPrepared then
    Result := ''
  else
  begin
    if Assigned(FActiveList) then
    begin
      if FActiveList = FComboList then
      begin
        ind := FComboList.ItemIndex;
        if ind > -1 then
          Result := ComboProps.ComboItems[ind];
      end
      else if (FActiveList = FLookupList) and (FLookupList.SelectedItem > '') then
        Result := FLookupList.SelectedItem
      else if (FActiveList = FTreeList) and (FTreeList.Items.Count > 0) then
        Result := FTreeList.GetFullNodeCaption(FTreeList.Selected,
          TreeProps.PathSeparator)
      else if (FActiveList = FCalendar) then
        Result := DateToStr(FCalendar.Date)
      else if (FActiveList = FCalculator) then
        Result := FCalculator.StrValue
      else
        Result := '';
    end;
  end;
end;

function TVolgaDBEdit.GetSelectedValue: Variant;
//присвоить полю значение по выбранному тексту на данный момент
var ind: integer;
begin
  if not FPrepared then CreateDropDownList;
  if not FPrepared then
    Result := Text
  else
  begin
    if Assigned(FActiveList) then
    begin
      if (FActiveList = FLookupList) and (FLookupList.SelectedItem > '') then
        Result := FLookupList.KeyValue  //код для выбранного текста в lookup'e
      else if (FActiveList = FTreeList) and (FTreeList.Selected <> nil) then
        Result := PShortString(FTreeList.Selected.Data)^  //код для выбранного текста в Tree
      else if FActiveList = FComboList then
      begin
        ind := FComboList.ItemIndex;
        if ind > -1 then
          if ComboProps.HasValues then
            Result := ComboProps.ComboValues[ind]
          else
            Result := ComboProps.ComboItems[ind]
        else if (Style = vcsDropDownList) and DBLinked then
          Result := FDataLink.Field.Text //не меняем на неправильный!
        else
          Result := Text;
      end
      else if FActiveList = FCalendar then
        Result := FCalendar.Date
      else if FActiveList = FCalculator then
        Result := FCalculator.Value;
    end
    else                                //Tree???????
      Result := Text;
  end;
end;

procedure TVolgaDBEdit.CloseUp(Accept: boolean);
begin
  if Accept then Modified := True;
  if FDroppedDown then
  begin
    //Сообщение посылается окну, имеющему фокус при отображении модальных форм
    //- диалогов и сообщений об ошибках. Дает возможность окну закрыться и освобождает мышь.
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    //спрятать выпадающий контроль
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    if FActiveList = FLookupList then   //отцепить фильтр
      FLookupList.ResetFilter;
    FDroppedDown := False;
    if Accept and EditCanModify then    //значение записываем в поле датасета
    try
      FValue := GetSelectedValue; //даже не для DB-ориентир!, м.б. >1 поля, если это lookup по двум полям!!
      if DBLinked then
        FDataLink.Modified
      else
      begin                             //не DB-oriented
        Text := GetViewTextNonDB;
        EditText := Text;
        case CharCase of
          ecUpperCase: EditText := AnsiUpperCase(EditText);
          ecLowerCase: EditText := AnsiLowerCase(EditText);
        end;
      end;
      UpdateData(Self);
    except
      SelectAll;
      SetFocus;
      raise;
    end
    else
      Reset;
    Invalidate;
    if Assigned(FOnCloseUp) then FOnCloseUp(self, Accept);
    Windows.SetFocus(Handle);           //фокус остался на edit-контроле
    ShowCaret(Handle);
    SelectAll;
  end;
end;

function TVolgaDBEdit.EditCanModify: Boolean;
begin
  Result := (FDataLink.Field = nil) or (FDataLink.Dataset.CanModify and FDataLink.Edit);
end;

procedure TVolgaDBEdit.DropDown;
var I, J, X, Y: integer;
  P: TPoint;
begin
  if not FDroppedDown and Assigned(FActiveList) then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    FDroppedDown := True;
    if DropDownWidth > 0 then
      FActiveList.Width := DropDownWidth //ширина указана явно
    else
      FActiveList.Width := Width;
    if FActiveList = FLookupList then
    begin                               //пытаемся искать в списке нужное значение
      FLookupList.KeyValue := FValue;
      LookupDataset.Locate(LookupProps.LookupKeyField, FValue, []);
      if LookupProps.KindSearch = vksFilter then FLookupList.SetFilter;
    end
    else if FActiveList = FTreeList then
    begin
      FTreeList.FullCollapse;
      //пытаемся искать в списке нужное значение
      if VarToStr(FValue) > '' then
        FTreeList.Selected := FTreeList.FindNodeByUniqueID(VarToStr(FValue));
      FTreeList.Height := 200;
    end
    else if FActiveList = FComboList then
    begin
      FComboList.FSearchText := '';     //очищаем текст при каждом выпадении!!
      //рассчитываем высоту выпадающего списка
      if FComboList.Items.Count >= Integer(DropDownRows) then
        FComboList.Height := Integer(DropDownRows) * FComboList.ItemHeight + 4
      else
        FComboList.Height := FComboList.Items.Count * FComboList.ItemHeight + 4;
      if VarToStr(FValue) = '' then
        FComboList.ItemIndex := -1
      else if ComboProps.HasValues then
        FComboList.ItemIndex := ComboProps.ComboValues.IndexOf(VarToStr(FValue))
      else
        FComboList.ItemIndex := ComboProps.ComboItems.IndexOf(VarToStr(FValue));
      //рассчитываем ширину максимально длинного элемента
      J := FComboList.ClientWidth;
      for I := 0 to FComboList.Items.Count - 1 do
      begin
        Y := FComboList.Canvas.TextWidth(FComboList.Items[I]);
        if Y > J then J := Y;
      end;
      FComboList.ClientWidth := J;
    end
    else if FActiveList = FCalendar then
    begin                               //календарь
      if VarIsNull(FValue) or (VarToStr(FValue) = '') then
        FCalendar.Date := Date
      else
      try
        FCalendar.Date := VarAsType(FValue, varDate);
      except FCalendar.Date := Date;
      end;
    end
    else if FActiveList = FCalculator then
    begin
      HideCaret(Handle);
      if DBLinked and FDataLink.Dataset.CanModify and not ReadOnly then
        FDataLink.Edit;
      if VarIsNull(FValue) or (VarToStr(FValue) = '') then
        FCalculator.Value := 0
      else
      try
        FCalculator.Value := StrToFloat(VarToStr(FValue));
        FCalculator.OldValue := FCalculator.Value;
      except FCalculator.Value := 0;
      end;
      SelectAll;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    X := P.X;
    Y := P.Y + Height;
    if X + FActiveList.Width > Screen.Width then X := Screen.Width - FActiveList.Width;
    //он будет сверху или снизу парент-контроля?
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    //показать выпадающий контроль неактивным сверху
    SetWindowPos(FActiveList.Handle, HWND_TOP, X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FDroppedDown := True;
    Invalidate;
    Windows.SetFocus(Handle);           //фокус остался на edit-контроле
  end
  else if Assigned(FOnCustomDlg) then
  begin
    FDroppedDown := True;
    Invalidate;
    try                                 { If exception then clean-up }
      if DBLinked and FDataLink.Dataset.CanModify and not ReadOnly then
        FDataLink.Edit;
      FOnCustomDlg(self);
    finally
      if (not editable) then            //спрятать курсор
        HideCaret(Handle);              { Support csDropDownList style }
      Invalidate;
      FDroppedDown := False;            {CustomDialog закрылся и dropdown закончился}
    end;
  end;
end;

function TVolgaDBEdit.Editable: boolean;
begin
  Result := (FStyle <> vcsDropDownList) or FDroppedDown;
end;

procedure TVolgaDBEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  FNextControl := 0;
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FDroppedDown then
          CloseUp(True)
        else
          DropDown;
        Key := 0;
      end
      else if not FDroppedDown then
      begin
        if Key = VK_Down then
          FNextControl := 1
        else
          FNextControl := -1;
      end;
    VK_RETURN, VK_ESCAPE:
      begin
        if FDroppedDown and not (ssAlt in Shift) {and (FDialogStyle <> vdsCalculator)} then
        begin                           //закрываем выпад.контроль по ENTER и ESC
          CloseUp(Key = VK_RETURN);
          Key := 0;
          Abort;
        end;
      end;
  end;
end;

procedure TVolgaDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (FDialogStyle = vdsCustomDlg) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    if Assigned(FOnCustomDlg) then FOnCustomDlg(self);  //нажата кнопка произв.редактирования
    KillMessage(Handle, WM_CHAR);
  end
  else
  begin
    if FDroppedDown and (FDialogStyle = vdsCalculator) then begin key := 0; Exit; end;
    if GetKeyState(VK_MENU) < 0 then
      Include(Shift, ssAlt);
    DoDropDownKeys(Key, Shift);
    if FNextControl = 0 then
    begin
      inherited KeyDown(Key, Shift);
      if ((Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in
        Shift))) and DBLinked and FDataLink.Dataset.CanModify and not ReadOnly then
      begin
        FDataLink.Edit;
        if ((Key = VK_DELETE) or (Key = VK_BACK)) and not FDroppedDown and (Style = vcsDropDownList) then
        begin
          Text := '';
          FValue := ClearValue;
        end;
      end;
    end
    else if FNextControl > 0 then
      if Parent is TCustomForm then     //т.к. для TabSheet не работает!!!!!!!!
        PostMessage(Parent.Handle, WM_NEXTDLGCTL, 0, 0)
      else if Owner is TCustomForm then
        PostMessage(TWinControl(Owner).Handle, WM_NEXTDLGCTL, 0, 0)
      else
    else
      if Parent is TCustomForm then
        PostMessage(Parent.Handle, WM_NEXTDLGCTL, 1, 0)
      else if Owner is TCustomForm then
        PostMessage(TWinControl(Owner).Handle, WM_NEXTDLGCTL, 1, 0)
      else
  end;
end;

procedure TVolgaDBEdit.KeyPress(var Key: Char);
begin
  { Disregard tab key since inherited maskedit event will beep }
//if isMasked and (Key = #9) then exit;
  if FDroppedDown and (FDialogStyle = vdsCalculator) then begin key := #0; Exit; end;
  inherited KeyPress(Key);
  //добавить еще сюда если комбо без values или дата
  if (Style <> vcsDropDownList) and (Key in [#32..#255]) and DBLinked
    and not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      begin
        if (Key = #32) and not FDroppedDown and (Style = vcsDropDownList) then
        begin
          Key := #0;
          DropDown;                     //по пробелу выпадаем
        end
        else
        begin
          if DBLinked and FDataLink.Dataset.CanModify and not FDataLink.Editing and not ReadOnly then
            FDataLink.Edit;
          if SelLength > 0 then ClearSelection;
        end;
      end;
    #13:                                //переходим на следующий контроль!
      if not FDroppedDown then
        if EnterAsTab then
        begin
          Key := #0;
          if Parent is TCustomForm then //т.к. для TabSheet не работает!!!!!!!!
            PostMessage(Parent.Handle, WM_NEXTDLGCTL, 0, 0)
          else if Owner is TCustomForm then
            PostMessage(TWinControl(Owner).Handle, WM_NEXTDLGCTL, 0, 0);
        end
        else
          GetParentForm(Self).Perform(CM_DIALOGKEY, 13, 0); //вызов Default-кнопки
    #27:
      begin
        Reset;
        Key := #0;
      end;
  end;
end;

procedure TVolgaDBEdit.CalSelectDate(Sender: TObject);
begin
  CloseUp(true);
end;

procedure TVolgaDBEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var TreeButton: Boolean;
begin                                   //обработчик OnMouseUp для выпадающих контролей
  TreeButton := Assigned(FActiveList) and (FActiveList = FTreeList);
  if TreeButton then
    TreeButton := (htOnButton in FTreeList.GetHitTestInfoAt(X, Y));
  if (Button = mbLeft) and not TreeButton then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TVolgaDBEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FDialogStyle <> vdsEdit) and
    (OverButton(Point(X, Y)) or (Style = vcsDropDownList)) then //клик над кнопкой
  begin
    if FDroppedDown then
      CloseUp(False)                    //закрыть выпад.контроль
    else
    begin
      MouseCapture := True;
      FTracking := True;                //мышь нажата
      TrackButton(X, Y);                //перерисовать кнопку нажатой
      if Assigned(FActiveList) then
        DropDown;                       //открыть выпад.контроль
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVolgaDBEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then                     //мышь была нажата?
  begin
    TrackButton(X, Y); //перерисовать кнопку нажатой/отжатой в зависимости от координаты точки
    if FDroppedDown then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin                             //если нажали в области выпадающего контроля
        StopTracking;                   //риссуем кнопку отжатой
        MousePos := PointToSmallPoint(ListPos);
        //посылаем мессагу о нажатии на область выпадающего контроля
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TVolgaDBEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;               //кнопка была нажата/нет
  StopTracking;                         //отжать кнопку
  if (Button = mbLeft) and (FDialogStyle = vdsCustomDlg) and WasPressed then
    if Assigned(FOnCustomDlg) then FOnCustomDlg(self);  //нажата кнопка произв.редактирования
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TVolgaDBEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);                //перерисовать кнопку отжатой
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TVolgaDBEdit.TrackButton(X, Y: Integer);
var //проверка, надо ли перерисовать кнопку нажатой/отжатой
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;                      //здесь наша кнопка
  NewState := PtInRect(R, Point(X, Y)); //эта точка на кнопке?
  if FPressed <> NewState then //если нажали на кнопке, а кнопка была отжата или наоборот
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);  //перерисовать кнопку в новом состоянии
  end;
end;

function TVolgaDBEdit.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TVolgaDBEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
    if (FDialogStyle <> vdsEdit) and OverButton(Point(XPos, YPos)) then
      Exit;
  inherited;
end;

procedure TVolgaDBEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin                                   //форма курсора над кнопкой - палец
  GetCursorPos(P);
  P := ScreenToClient(P);
  if not (csDesigning in ComponentState) and (FDialogStyle <> vdsEdit)
    and (OverButton(P) or (Style = vcsDropDownList)) then
    Windows.SetCursor(Screen.Cursors[crHandPoint])
  else
    inherited;
end;

procedure TVolgaDBEdit.WndProc(var Message: TMessage);
var DC: HDC;
  PS: TPaintStruct;
begin
  case Message.Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
      if DialogStyle in [vdsCombo, vdsLookup, vdsCalendar, vdsTree, vdsCalculator] then
        with TWMKey(Message) do
        begin
          DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (Message.Msg = wm_Char) and (CharCode > 32) and FAutoDrop and not
            FDroppedDown and (DialogStyle in [vdsCombo, vdsLookup]) then
            DropDown; //выпадаем, если установлено AutoDropDown и нажата буква
          if (CharCode <> 0) and FDroppedDown then
          begin
            with TMessage(Message) do //перенаправляем символ выпад.контролю для проведения поиска
              SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          end;
        end;
    WM_SETTEXT:
      ;
    WM_PAINT:
      if DialogStyle <> vdsEdit then
      begin
        DC := TWMPaint(Message).DC;
        if DC = 0 then DC := BeginPaint(Handle, PS);
        try
          PaintWindow(DC);
        finally
          if TWMPaint(Message).DC = 0 then EndPaint(Handle, PS);
        end;
      end;
  end;
  inherited;
end;

procedure TVolgaDBEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
  Invalidate;
end;

procedure TVolgaDBEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TVolgaDBEdit.Reset;
begin
  if DBLinked then
    FDataLink.Reset;                    //игнорируем все предыдущие изменения в поле
  SelectAll;
end;

procedure TVolgaDBEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    if DBLinked then FDataLink.Reset;
  end;
end;

procedure TVolgaDBEdit.Change;
begin
  if DBLinked and FDataLink.DataSet.CanModify then FDataLink.Modified;
  if Modified and not FDroppedDown then
    if Text > '' then
      FValue := Text
    else
      FValue := ClearValue;
  inherited Change;
end;

function TVolgaDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TVolgaDBEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  CheckDB;
end;

function TVolgaDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TVolgaDBEdit.SetDataField(const Value: string);
begin
  if Value > '' then
    FDataLink.FieldName := Value;
  CheckDB;
end;

procedure TVolgaDBEdit.CheckDB;
begin
  if (FDataLink.Field <> nil) and (FDataLink.DataSource <> nil) then
  begin
    ReadOnly := FDataLink.Field.ReadOnly;
    Alignment := FDataLink.Field.Alignment;
    //для lookup-поля проставляем поля
    if FDataLink.Field.FieldKind = fkLookup then
    begin
      DialogStyle := vdsLookup;
      LookupDataSet := FDataLink.Field.LookupDataSet;
      LookupProps.SourceKeyField := FDataLink.Field.KeyFields;
      LookupProps.LookupKeyField := FDataLink.Field.LookupKeyFields;
      LookupProps.ListFieldNames := FDataLink.Field.LookupResultField;
    end;
  end;
end;

procedure TVolgaDBEdit.ActiveChange(Sender: TObject);
begin
  CheckDB;
end;

procedure TVolgaDBEdit.DataChange(Sender: TObject);
begin
  VarClear(FValue);
  if DBLinked and ((FDataLink.DataSet.RecordCount > 0) or (FDataLink.DataSet.State = dsInsert)) then
  begin
    //для Lookup это поля линка!!!
    if (DialogStyle = vdsLookup) and (LookupProps.SourceKeyField>'') then
      FValue := FDatalink.DataSet.FieldByName(LookupProps.SourceKeyField).Value
    else FValue := FDataLink.Field.Value;
    EditMask := FDataLink.Field.EditMask;
    if FFocused and FDataLink.CanModify then
      SetViewText(FValue, FDataLink.Field.Text)    //устанавливается свойство Text!
    else
    begin
      SetViewText(FValue, FDataLink.Field.DisplayText);    //устанавливается свойство Text!
      if FDataLink.Dataset.CanModify and FDataLink.Editing {and FDataLink.FModified} then
        Modified := True;
    end;
  end
  else if csDesigning in ComponentState then
    SetViewText(Name, Name)
  else
    SetViewText(NULL, '');
end;

procedure TVolgaDBEdit.EditingChange(Sender: TObject);
begin
  if DBLinked then
    inherited ReadOnly := not FDataLink.Editing;
end;

function TVolgaDBEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
  if DBLinked then
    Result := FReadOnly;
end;

procedure TVolgaDBEdit.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  if DBLinked then
    FDataLink.ReadOnly := Value;
  inherited ReadOnly := FReadOnly;
end;

procedure TVolgaDBEdit.UpdateData(Sender: TObject);
var val: string;
begin                                   //установить по выбранному тексту поле в датасете
  ValidateEdit;                         //Validates the EditText against the current mask
  if ((DialogStyle = vdsEdit) or (DialogStyle = vdsCustomDlg)) and not Assigned(FOnDisplayText) then
    val := Text
  else if VarIsNull(FValue) or VarIsEmpty(FValue) then
    val := ''
  else
    val := string(FValue);
  if Assigned(FOnUpdateData) then       //изменено calculated поле!!!!!!!
    FOnUpdateData(self, Text)
  else if DBLinked then
    if (DialogStyle = vdsLookup) and (LookupProps.SourceKeyField > '') then
    begin                               //поля линка
      if VarIsNull(FValue) or VarIsEmpty(FValue) then
        FDataLink.DataSet.FieldByName(LookupProps.SourceKeyField).Clear
      else //используем свойство Text, чтобы сработал OnSetText для поля
        FDataLink.DataSet.FieldByName(LookupProps.SourceKeyField).Text := val;
      if (LookupProps.SourceKeyField <> DataField) and
        (FDataLink.Field.FieldKind = fkData) then
        FDataLink.Field.Text := Text;   //видимое поле
    end
    else if not FDataLink.Field.Calculated then
      if VarIsNull(FValue) or VarIsEmpty(FValue) then
        FDataLink.Field.Clear
      else
        FDataLink.Field.Text := val;
end;

procedure TVolgaDBEdit.WMPaste(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  if DBLinked and not ReadOnly then FDataLink.Edit;
  inherited;
end;

procedure TVolgaDBEdit.WMCut(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  if DBLinked and not ReadOnly then FDataLink.Edit;
  inherited;
end;

procedure TVolgaDBEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TVolgaDBEdit.CMExit(var Message: TCMExit);
begin
  try
    if DBLinked then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;                          //встаем на конец редактируемого поля?
  DoExit;
end;

procedure TVolgaDBEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TVolgaDBEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TVolgaDBEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TVolgaDBEdit.SetDialogStyle(const Value: TVolgaDialogStyle);
begin
  if FDialogStyle = Value then Exit;
  FDialogStyle := Value;
  FPrepared := false;
  //Alignment := FDataLink.Field.Alignment;
  if FDialogStyle = vdsCalculator then
    FAlignment := taRightJustify;
  //else if FDialogStyle <> vdsEdit then
  //  FAlignment := taLeftJustify;
  if (csDesigning in ComponentState) then DataChange(Self);
//CreateDropDownList;
  Invalidate;
end;

procedure TVolgaDBEdit.CreateDropDownList;
begin
  if (csDestroying in ComponentState) then Exit;
  if (csLoading in ComponentState) then Exit;
  FPrepared := true;
  case FDialogStyle of
    vdsCombo:
      begin
        if FComboList = nil then
        begin
          FComboList := TVolgaListbox.Create(Self);
          FComboList.Visible := False;
          FComboList.Parent := Self;
          FComboList.OnMouseUp := ListMouseUp;
          FComboList.IntegralHeight := True;
          FComboList.ItemHeight := 11;
        end;
        FComboList.Color := Color;
        FComboList.Font := Font;
        FComboList.Items.Assign(ComboProps.ComboItems);
        if ComboProps.HasValues then
        begin
          Style := vcsDropDownList;
          FAutoDrop := true;
        end
        else
          Style := vcsDropDown;
        FActiveList := FComboList;
      end;
    vdsLookup:
      begin
        if FLookupList = nil then
        begin
          FLookupList := TVolgaPopupData.Create(Self);
          FLookupList.Visible := False;
          FLookupList.Parent := Self;
          FLookupList.OnMouseUp := ListMouseUp;
        end;
        FLookupList.KindSearch := LookupProps.KindSearch;
        FLookupList.Color := Color;
        FLookupList.Font := Font;
        FLookupList.RowCount := DropDownRows;
        FLookupList.KeyField := LookupProps.LookupKeyField;
        FLookupList.ListField := LookupProps.ListFieldNames;
        FLookupList.ListFieldIndex := LookupProps.ViewFieldIndex;
        FLookupSource.DataSet := LookupDataSet;
        FLookupList.ListSource := FLookupSource;
        LookupProps.PrepareLookup;
        if not IsLinkActive then FPrepared := false; //не удалось подготовить!!!
        FActiveList := FLookupList;
      end;
    vdsCalendar:
      begin
        if FCalendar = nil then
        begin
          FCalendar := TVolgaDlgCalendar.Create(Self);
          FCalendar.Visible := False;
          FCalendar.Parent := Self;
          FCalendar.OnMouseUp := ListMouseUp;
          FCalendar.OnSelectDate := CalSelectDate;
        end;
        FActiveList := FCalendar;
      end;
    vdsTree:
      begin
        if FTreeList = nil then
        begin
          FTreeList := TVolgaPopupTree.Create(Self);
          FTreeList.Visible := False;
          FTreeList.Parent := Self;
          FTreeList.OnMouseUp := ListMouseUp;
        end;
        FTreeList.Color := Color;
        FTreeList.Font := Font;
        TreeProps.PrepareTree;
        if not IsLinkActive then FPrepared := false; //не удалось подготовить!!!
        Style := vcsDropDownList;
        FActiveList := FTreeList;
      end;
    vdsCalculator:
      begin
        if FCalculator = nil then
        begin
          FCalculator := TVolgaCalculator.Create(Self);
          FCalculator.Visible := False;
          FCalculator.Parent := Self;
          FCalculator.OnExitClick := CalcExit;
        end;
        FActiveList := FCalculator;
      end;
    else
      FActiveList := nil;
  end;
  if FPrepared and DBLinked then
    if (DialogStyle = vdsLookup) and (LookupProps.SourceKeyField > '') then
      SetViewText(FDataLink.DataSet[LookupProps.SourceKeyField],
        FDataLink.Field.DisplayText)
    else
      SetViewText(FDataLink.Field.Value, FDataLink.Field.DisplayText);  //устанавливается свойство Text!
  Invalidate;
end;

procedure TVolgaDBEdit.CalcExit(Sender: TObject; Selected: Boolean);
begin
  CloseUp(Selected);
end;

function TVolgaDBEdit.GetComboProps: TVolgaComboProperties;
begin
  if FComboProps = nil then
    FComboProps := TVolgaComboProperties.Create(self);
  Result := FComboProps;
end;

procedure TVolgaDBEdit.SetComboProps(const Value: TVolgaComboProperties);
begin
  if Value = nil then
  begin
    FComboProps.Free;
    FComboProps := nil;
    Exit;
  end;
  FComboProps.ComboItems.Assign(Value.ComboItems);
  FComboProps.ComboValues.Assign(Value.ComboValues);
  FPrepared := false;
end;

function TVolgaDBEdit.GetLookupProps: TVolgaLookupProperties;
begin
  if FLookupProps = nil then
    FLookupProps := TVolgaLookupProperties.Create(self);
  Result := FLookupProps;
end;

procedure TVolgaDBEdit.SetLookupProps(const Value: TVolgaLookupProperties);
begin
  if Value = nil then
  begin
    FLookupProps.Free;
    FLookupProps := nil;
    Exit;
  end;
  FLookupProps.ViewFieldIndex := Value.ViewFieldIndex;
  FLookupProps.SourceKeyField := Value.SourceKeyField;
  FLookupProps.LookupKeyField := Value.LookupKeyField;
  FLookupProps.ListFieldNames := Value.ListFieldNames;
  FPrepared := false;
end;

function TVolgaDBEdit.GetTreeProps: TVolgaTreeProperties;
begin
  if FTreeProps = nil then
    FTreeProps := TVolgaTreeProperties.Create(self);
  Result := FTreeProps;
end;

procedure TVolgaDBEdit.SetTreeProps(const Value: TVolgaTreeProperties);
begin
  if Value = nil then
  begin
    FTreeProps.Free;
    FTreeProps := nil;
    Exit;
  end;
  FTreeProps.UniqueField := Value.UniqueField;
  FTreeProps.TextField := Value.TextField;
  FTreeProps.LevelField := Value.LevelField;
  FPrepared := false;
end;

procedure TVolgaDBEdit.SetValue(const Value: Variant);
begin
  if DBLinked then
    Exit //для Dataset-ориентированных контролей нельзя установить Value!!!!!!
  else
  begin
    FValue := Value;
    SetViewText(FValue, VarToStr(Value)); //установить текст по выбранному значению
  end;
end;

function TVolgaDBEdit.DBLinked: Boolean;
begin
  Result := (FDataLink.Field <> nil) and FDataLink.Active;
end;

procedure TVolgaDBEdit.SetLookupDataSet(const Value: TDataSet);
begin
//  CheckInactive;
  if (Value <> nil) and (FDataLink.Field <> nil) and (Value = FDataLink.DataSet) then
    DatabaseError(V_LOOKUPSOURCEERROR, Self);
  FLookupDataSet := Value;
end;

function TVolgaDBEdit.IsLinkActive: Boolean;
begin
  try
    Result := (LookupDataSet <> nil) and LookupDataSet.Active;
    if Result then
      if FDialogStyle = vdsLookup then
        Result := (FLookupProps.LookupKeyField > '') and (FLookupProps.ListFieldNames >
          '')
      else if FDialogStyle = vdsTree then
        Result := (FTreeProps.TextField > '') and (FTreeProps.UniqueField > '')
          and (FTreeProps.LevelField > '');
  except Result := false;
  end;
end;

function TVolgaDBEdit.GetItemIndex: integer;
begin
  Result := -1;
  if Assigned(FActiveList) then
  begin
    if (FActiveList = FLookupList) and (FLookupList.SelectedItem > '')
      and FLookupDataSet.IsSequenced then
      Result := FLookupDataSet.RecNo - 1
    else if (FActiveList = FTreeList) and (FTreeList.Selected <> nil) then
      Result := FTreeList.Selected.AbsoluteIndex
    else if FActiveList = FComboList then
      Result := FComboList.ItemIndex;
  end;
end;

procedure TVolgaDBEdit.SetItemIndex(const AValue: integer);
var tekvalue: Variant;
begin
  if DBLinked then
    Exit //для Dataset-ориентированных контролей нельзя установить ItemIndex
  else
  begin
    if Assigned(FActiveList) then
    begin
      tekvalue := NULL;
      if AValue >= 0 then
      try
        if FActiveList = FComboList then
        begin
          if ComboProps.HasValues then
            tekvalue := ComboProps.ComboValues[AValue]
          else
            tekvalue := ComboProps.ComboItems[AValue];
          FComboList.ItemIndex := AValue;
        end
        else if FActiveList = FLookupList then
          if FLookupDataSet.IsSequenced then
          begin
            FLookupDataSet.RecNo := AValue + 1;
            tekvalue := FLookupDataSet.FieldValues[LookupProps.LookupKeyField];
          end
          else
        else if FActiveList = FTreeList then
        begin
          tekvalue := PShortString(FTreeList.Items[AValue].Data)^;
          FTreeList.Items[AValue].Selected := true;
        end;
      except;
      end;
      if tekvalue <> NULL then
      begin
        FValue := tekvalue;
        SetViewText(FValue, VarToStr(FValue));
      end;
    end;
  end;
end;

procedure TVolgaDBEdit.SetAlignment(const Value: TAlignment);
begin
  if FDialogStyle = vdsCalculator then
    FAlignment := taRightJustify
  else
    FAlignment := Value;
  Invalidate;
end;

procedure TVolgaDBEdit.SetButtonWidth(const Value: integer);
begin
  if (Value > 11) and (Value < Round(Width / 2)) then
    FButtonWidth := Value;
  Invalidate;
end;

procedure TVolgaDBEdit.CMDialogKey(var Message: TCMDialogKey);
begin
  Broadcast(Message);
end;

{ TVolgaComboProperties }

constructor TVolgaComboProperties.Create(AOwner: TComponent);
begin
  inherited Create;
  if AOwner is TVolgaDBEdit then
    FOwner := TVolgaDBEdit(AOwner)
  else
    FOwner := nil;
end;

destructor TVolgaComboProperties.Destroy;
begin
  if FItems <> nil then FItems.Free;
  FItems := nil;
  if FValues <> nil then FValues.Free;
  FValues := nil;
  inherited Destroy;
end;

procedure TVolgaComboProperties.AssignList(const AList: TStrings);
var i: integer;
begin //присвоить сразу Items и Values из списка типа Name=Value
  if FItems = nil then
    FItems := TStringList.Create
  else
    FItems.Clear;
  if FValues = nil then
    FValues := TStringList.Create
  else
    FValues.Clear;
  for i := 0 to AList.Count - 1 do
  begin
    FItems.Add(AList.Names[i]);
    FValues.Add(AList.Values[AList.Names[i]]);
  end;
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

function TVolgaComboProperties.GetComboItems: TStrings;
begin
  if FItems = nil then
    FItems := TStringList.Create;
  Result := FItems;
end;

function TVolgaComboProperties.GetComboValues: TStrings;
begin
  if FValues = nil then
    FValues := TStringList.Create;
  Result := FValues;
end;

function TVolgaComboProperties.GetHasValues: Boolean;
begin
  Result := (FValues <> nil) and (FValues.Count = FItems.Count);
end;

procedure TVolgaComboProperties.SetComboItems(const Value: TStrings);
begin
  if Value = nil then
  begin
    FItems.Free;
    FItems := nil;
    Exit;
  end;
  FItems.Assign(Value);
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

procedure TVolgaComboProperties.SetComboValues(const Value: TStrings);
begin
  if Value = nil then
  begin
    FValues.Free;
    FValues := nil;
    Exit;
  end;
  FValues.Assign(Value);
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

{ TVolgaLookupProperties }

constructor TVolgaLookupProperties.Create(AOwner: TComponent);
begin
  inherited Create;
  if AOwner is TVolgaDBEdit then
    FOwner := TVolgaDBEdit(AOwner)
  else
    FOwner := nil;
  FListFieldIndex := 0;
  FLFields := TList.Create;
  FKindSearch := vksFindFirst;
end;

destructor TVolgaLookupProperties.Destroy;
begin
  FLFields.Free;
  inherited Destroy;
end;

function TVolgaLookupProperties.GetViewField: string;
begin
  if FLFields.Count = 0 then PrepareLookup;
  Result := TField(FLFields[ViewFieldIndex]).FieldName;
end;

procedure TVolgaLookupProperties.PrepareLookup;
begin
  if (FListFieldNames > '') and (FOwner.LookupDataSet <> nil) then
  try
    FLFields.Clear;
    if FOwner.DBLinked or not (csDesigning in FOwner.ComponentState) then
      if not FOwner.LookupDataSet.Active then FOwner.LookupDataSet.Open;
    FOwner.LookupDataSet.GetFieldList(FLFields, FListFieldNames);
  except;
  end;
end;

procedure TVolgaLookupProperties.SetLookupKeyField(const Value: string);
begin
  FLookupKeyField := Value;
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

procedure TVolgaLookupProperties.SetListFieldNames(const Value: string);
begin
  FListFieldNames := Value;
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

procedure TVolgaLookupProperties.SetSourceKeyField(const Value: string);
begin
  FSourceKeyField := Value;
  if Assigned(FOwner) then FOwner.FPrepared := false;
end;

{ TVolgaTreeProperties }

constructor TVolgaTreeProperties.Create(AOwner: TComponent);
begin
  inherited Create;
  if AOwner is TVolgaDBEdit then
    FOwner := TVolgaDBEdit(AOwner)
  else
    FOwner := nil;
  FSeparator := '/';
end;

procedure TVolgaTreeProperties.PrepareTree;
var ActNode1, ActNode2, ActNode3, ActNode4, ActNode5, ActNode6: TTreeNode;
  ptrUID: PShortString;
begin
  if (FTextField = '') or (FUniqueField = '') or (FLevelField = '')
    or (FOwner = nil) or not Assigned(FOwner.LookupDataset) then Exit;
  if not FOwner.LookupDataSet.Active then
  try FOwner.LookupDataSet.Open;
  except Exit;
  end;
  if FOwner.FTreeList = nil then Exit;
  with FOwner.FTreeList, FOwner.LookupDataSet do
  begin                                 {создать дерево из таблицы}
    ActNode1 := nil;
    ActNode2 := nil;
    ActNode3 := nil;
    ActNode4 := nil;
    ActNode5 := nil;
    ActNode6 := nil;
    Items.Clear;
    //Не рисовать TreeView пока не внесены все изменения
    Items.BeginUpdate;
    DisableControls;
    First; //считаем, что таблица упорядочена по полю IndexField!!!!!!!!!!
    while not Eof do
    begin
      New(ptrUID);
      ptrUID^ := FieldByName(UniqueField).AsString;
      if FieldByName(LevelField).AsInteger = 1 then {не дочерняя ни к чему}
        ActNode1 := Items.AddObject(nil, FieldByName(TextField).AsString, ptrUID)
      else if FieldByName(LevelField).AsInteger = 2 then
        ActNode2 := Items.AddChildObject(ActNode1, FieldByName(TextField).AsString,
          ptrUID)
      else if FieldByName(LevelField).AsInteger = 3 then
        ActNode3 := Items.AddChildObject(ActNode2, FieldByName(TextField).AsString,
          ptrUID)
      else if FieldByName(LevelField).AsInteger = 4 then
        ActNode4 := Items.AddChildObject(ActNode3, FieldByName(TextField).AsString,
          ptrUID)
      else if FieldByName(LevelField).AsInteger = 5 then
        ActNode5 := Items.AddChildObject(ActNode4, FieldByName(TextField).AsString,
          ptrUID)
      else if FieldByName(LevelField).AsInteger = 6 then
        ActNode6 := Items.AddChildObject(ActNode5, FieldByName(TextField).AsString,
          ptrUID)
      else
        Items.AddChildObject(ActNode6, FieldByName(TextField).AsString, ptrUID);
      Next;
    end;
    EnableControls;
    // теперь нарисовать TTreeView после загрузки всех данных
    Items.EndUpdate;
  end;
end;

end.

