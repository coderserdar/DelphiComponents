
{*******************************************************}
{                                                       }
{           TDBLookupComboPlus Component                }
{                                                       }
{  Copyright (c) 1995,96,97 Out & About Production      }
{   Portions Copyright (c) 1995,96,97,98 Inprise Inc.   }
{                                                       }
{                  Version 6.0                          }
{                                                       }
{                                                       }
{*******************************************************}
{ DEFINE ISCSVER}

{$IFDEF VER100}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D3OR4OR5}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D4OR5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE D6OR7}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D6OR7}
{$ENDIF}

unit DBLup2;

interface

uses
  WinTypes,
  {$IFDEF D3OR4OR5}
    DBCtrls,
    BDEConst,
      {$IFDEF ISCSVER}
      DBClient,
      {$ENDIF}
  {$ENDIF}
  {$IFDEF D6OR7}
    Variants,
  {$ENDIF}
  {$IFDEF Win32}
    BDE,
    DBCGrids,
  {$ELSE}
    DbiProcs, DbiTypes,
  {$ENDIF}
  Classes, StdCtrls, DB, Controls, Messages, SysUtils,
  Forms, Graphics, Menus, Buttons, DBGrids, DBTables, Grids, Dialogs;

type
 TPopupGrid = class;
 TDBLookupComboPlusStyle = (csDropDown, csDropDownList, csIncSearch, csIncSrchEdit);
 TDBLUPlusListOption = (loColLines, loRowLines, loTitles);
 TDBLUPlusListOptions = set of TDBLUPlusListOption;
 TNewLookUpRecEvent = procedure(Sender: TObject; var Cancelled: Boolean) of object;
 TTranslateEvent = procedure(Sender: TObject; var RecFound: Boolean) of object;
 TLookupRecChangedEvent = procedure(Sender: TObject; byIncSearch: Boolean) of object;
 TLeftRight = (Left, Right);
 TBelowAbove = (Below, Above);

 TDBLookupComboPlus = class(TCustomEdit)
 private
    FCanvas: TControlCanvas;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TLeftRight;
    FDropDownTop: TBelowAbove;
    FAutoDropDown : Boolean;
    FTopTextMargin: Integer;
    FLeftTextMargin: Integer;
    FFieldLink: TFieldDataLink;
    FGrid: TPopupGrid;
    FButton: TSpeedButton;
    FBtnControl: TWinControl;
    FShowSpeedButton : Boolean;
    FStyle: TDBLookupComboPlusStyle;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FAutoFill : Boolean;
    FSorted : Boolean;
    FAutoRefresh : Boolean;
    FLookupIndex: string;
    FSaveIndex: string;
    FSearchValue : String; {Holds the current search value}
    FLastChar : char;
    FLastKey : Word;
    FMastFields : String;
    FMastSource : TDataSource;
    FOnLookupRecChanged: TLookupRecChangedEvent;
    FOnNewLookupRec: TNewLookUpRecEvent;
    FOnPrepareList: TNotifyEvent;
    FOnBeforeSearch: TNotifyEvent;
    FOnAfterSearch: TNotifyEvent;
    FOnSortList : TNotifyEvent;
    FOnUnSortList : TNotifyEvent;
    FOnTranslate : TTranslateEvent;
    FOnSearchKeyPress : TTranslateEvent;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetIgnorLUIndexErr : Boolean;
    function GetDisplayFldPosInIndex : Integer;
    function GetLookupSource: TDataSource;
    function GetLookupDisplay: string;
    function GetLookupField: string;
    function GetListVisible : Boolean;
    function GetReadOnly: Boolean;
    function GetValue: string;
    function GetDisplayValue: string;
    function GetListColor : TColor;
    function GetListFont : TFont;
    function GetListCursor : TCursor;
    function GetButtonCursor : TCursor;
    function GetMinHeight: Integer;
    function GetOptions: TDBLUPlusListOptions;
    function CanEdit: Boolean;
    function Editable: Boolean;
    procedure SetListColor(value : TColor);
    procedure SetListFont(value : TFont);
    procedure SetListCursor(value : TCursor);
    procedure SetButtonCursor(value : TCursor);
    procedure SetValue(const NewValue: string);
    procedure SetDisplayValue(const NewValue: string);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetIgnorLUIndexErr(Value: Boolean);
    procedure SetDisplayFldPosInIndex(Value: Integer);
    procedure SetLookupSource(Value: TDataSource);
    procedure SetLookupIndex(Value: string);
    procedure SetLookupDisplay(const Value: string);
    procedure SetLookupField(const Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetOptions(Value: TDBLUPlusListOptions);
    procedure SetStyle(Value: TDBLookupComboPlusStyle);
    Procedure SetShowSpeedButton(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure FieldLinkActive(Sender: TObject);
    procedure NonEditMouseDown(var Message: TWMLButtonDown);
    procedure ResetMaxLength;
    procedure DoSelectAll;
    Procedure SelectAmt(Const NumChars : Integer);
    procedure DoSelectSome;
    procedure SetEditRect;
    procedure WMPaste (var Message: TMessage); message WM_PASTE;
    procedure WMCut (var Message: TMessage); message WM_CUT;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
{$IFDEF Win32}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK; {CtlGrid}
{$ENDIF}
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoSearch;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd;  override;
    procedure GridClick (Sender: TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SortList; dynamic;
    procedure DropDown; dynamic;
    procedure EnterLookupRec; dynamic;
    procedure CloseUp; dynamic;
    property Value: string read GetValue write SetValue;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property SearchValue: string read fSearchValue write fSearchValue;
    property ListVisible: Boolean read GetListVisible;
    property AutoRefresh: boolean read FAutoRefresh write FAutoRefresh;
    property IgnorLUIndexErr: boolean read GetIgnorLUIndexErr write SetIgnorLUIndexErr;
             {this needs to be set to true in the rare case when two TDBLookUpComboPlus
              controls are accessing the same field data and a lookup index is assigned.
              In this case the LUIndex error message is not reported. See the DoLookUp
              Method}
    property DisplayFldPosInIndex: Integer read getDisplayFldPosInIndex write SetDisplayFldPosInIndex;
             { This needs to be set whenever the display field is not the first field in the lookup index}
    property LeftTextMargin : Integer read FLeftTextMargin;
    property TopTextMargin : Integer read FTopTextMargin;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property LookupDisplay: string read GetLookupDisplay write SetLookupDisplay;
    property LookupField: string read GetLookupField write SetLookupField;
    property LookupIndex: string read fLookupIndex write SetLookupIndex;
    property Options: TDBLUPlusListOptions read GetOptions write SetOptions default [];
    property Style: TDBLookupComboPlusStyle read FStyle write SetStyle default csDropDown;
    property BorderStyle;
    property ShowSpeedButton : Boolean read FShowSpeedButton Write SetShowSpeedButton default True;
    property AutoSelect;
    property Color;
    property Ctl3D;
    property DragCursor;
{$IFDEF D4OR5} //VER120
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property DragMode;
    property DropDownAlign: TLeftRight read FDropDownAlign write FDropDownAlign default Right;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 8;
    property DropDownTop: TBelowAbove read FDropDownTop write FDropDownTop default Below;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property AutoDropDown : Boolean read FAutoDropDown Write FAutoDropDown default True;
    property Enabled;
    property Font;
    property ListColor : TColor read GetListColor write SetListColor;
    property ListFont: TFont read GetListFont write SetListFont;
    property ListCursor : TCursor read GetListCursor write SetListCursor;
    property ButtonCursor : TCursor read GetButtonCursor write SetButtonCursor;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write fOnCloseUp;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnLookupRecChanged: TLookupRecChangedEvent read FOnLookupRecChanged write FOnLookupRecChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewLookupRec: TNewLookUpRecEvent read FOnNewLookupRec write FOnNewLookupRec;
    property OnPrepareList: TNotifyEvent read FOnPrepareList write FOnPrepareList;
    property OnAfterSearch: TNotifyEvent read FOnAfterSearch write FOnAfterSearch;
    property OnBeforeSearch: TNotifyEvent read FOnBeforeSearch write FOnBeforeSearch;
    property OnSortList: TNotifyEvent read FOnSortList write FOnSortList;
    property OnUnSortList: TNotifyEvent read FOnUnSortList write FOnUnSortList;
    property OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property OnSearchKeyPress: TTranslateEvent read FOnSearchKeyPress write FOnSearchKeyPress;
  end;

{ TDBLUPlusList }
  TDBLUPlusList = class(TCustomDBGrid)
  private
    FFieldLink: TFieldDataLink; {NEVER USED BY TDBLOOKUPCOMBO PLUS it should be removed}
    FLookupDisplay: String;  {This holds the name(s) of the field(s) to display}
    FLookupField: String; {This holds the name of the field to use as the lookup}
    FValue: String;      {The is the value used to lookup the display value, this is what true data source returns}
    FDisplayValue: String;
    FDisplayFld: TField;  {TField who's name is stored in FLookupDisplay Field (the primary display field,}
                          {Note that multiple fields can show in the grid}
    FValueFld: TField;    {TField whos name is stored in FLookupField (the lookup field)}
    FHiliteRow: Integer;
    FOptions: TDBLUPlusListOptions;
    FTitleOffset: Integer;
    FFoundValue: Boolean;  {Flag that indicates if DoLookUp was successful}
    FInCellSelect: Boolean;
    FOnListClick: TNotifyEvent;
    FOnTranslate : TTranslateEvent;
    FOnSearchKeyPress : TTranslateEvent;
    FIgnorLUIndexErr : Boolean;
    FDisplayFldPosInIndex : Integer; {keeps track of where the display field is in the lookup index}
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetLookupSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure FieldLinkActive(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetLookupSource(Value: TDataSource);
    procedure SetLookupDisplay(const Value: string);
    procedure SetLookupField(const Value: string);
    procedure SetValue(const Value: string);
    procedure SetDisplayValue(const Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetOptions(Value: TDBLUPlusListOptions);
    procedure UpdateData(Sender: TObject);
    procedure NewLayout;
    procedure DoLookup;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    procedure DefineFieldMap; override;
    procedure SetColumnAttributes; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function CanEdit: Boolean; virtual;
    procedure InitFields(ShowError: Boolean);
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoSearch(const Value: string; FindExact, FireOnSearchKeyPress: Boolean) : Boolean;
    procedure LinkActive(Value: Boolean); override;
    procedure Paint; override;
    procedure Scroll(Distance: Integer); override;
    procedure ListClick; dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: string read fValue write SetValue;
    property DisplayValue: string read fDisplayValue write SetDisplayValue;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property LookupDisplay: string read fLookupDisplay write SetLookupDisplay;
    property LookupField: string read fLookupField write SetLookupField;
    property Options: TDBLUPlusListOptions read FOptions write SetOptions default [];
    property OnClick: TNotifyEvent read FOnListClick write FOnListClick;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

{ TPopupGrid }
  TPopupGrid = class(TDBLUPlusList)
  private
    FCombo: TDBLookupComboPlus;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function CanEdit: Boolean; override;
    procedure LinkActive(Value: Boolean); override;
  public
    property RowCount;
    constructor Create(AOwner: TComponent); override;
  end;

{ TComboButton }
  TComboButton = class(TSpeedButton)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

IMPLEMENTATION
uses
{$IFDEF Win32}
  Windows, DBConsts;
{$ELSE}
  WinProcs, DBConsts;
{$ENDIF}

{$IFDEF D3OR4OR5}
{$IFDEF ISCSVER}
type
  TMyClientDataSet = class(TClientDataSet) {just declare it so we can get to protected stuff}
  public
    property DSCursor;
  end;
{$ENDIF}
{$ENDIF}

var
  IsWin31 : Boolean;

{ TDBLookupComboPlus }

{$IFDEF Demo}
  Function DelphiLoaded(theState : TComponentState) : Boolean;
{$IFNDEF Win32}
  var
    aHandle : THandle;
    buff : ARRAY[0..60] OF Char;
    WndClass : TWndClass;
{$ENDIF}
  begin
    Result := False;
{$IFDEF Win32}
    if (FindWindow('TAppBuilder',nil) <> 0) then  Result := True
{$ELSE}
    aHandle := GetModuleHandle('delphi.exe');
    if aHandle = 0 then exit;
    strCopy(buff, 'TAppBuilder');
    If not(GetClassInfo(aHandle, buff, WndClass)) then exit;
    Result := True;
{$ENDIF}
  end;


Procedure KillApp(Comp : String);
begin
  ShowMessage(
  Format('The %S component you used in this application is a demo version ',  [Comp]) + #13#10 +
         'and requires that Delphi be running. To fix this problem you need to ' +  #13#10 +
         'contact O && A Productions at ' +  #13#10 +
         '          Fax (619)839-3834 or ' + #13#10 +
         '          E-mail sales@o2a.com or /n/r' +  #13#10 +
         'Visit our web site at www.o2a.com');
  Halt;
end;
{$ENDIF} {Demo}


constructor TDBLookupComboPlus.Create(AOwner: TComponent);
begin
{$IFDEF Demo}
  if Not DelphiLoaded(ComponentState) then KillApp('TDBLookupComboPlus');
{$ENDIF} {Demo}
  inherited Create(AOwner);
{$IFDEF Win32}
  ControlStyle := ControlStyle + [csReplicatable]; {CtlGrid}
{$ENDIF}
  AutoSize := False;
  FFieldLink := TFieldDataLink.Create;
  FFieldLink.Control := Self;
  FFieldLink.OnDataChange := DataChange;
  FFieldLink.OnEditingChange := EditingChange;
  FFieldLink.OnUpdateData := UpdateData;
  FFieldLink.OnActiveChange := FieldLinkActive;
  FShowSpeedButton := True;
  FAutoRefresh := True;
  FBtnControl := TWinControl.Create (Self);
{$IFDEF Win32}
  FBtnControl.ControlStyle := FBtnControl.ControlStyle + [csReplicatable]; {CtlGrid}
{$ENDIF}
  FBtnControl.Width := GetSystemMetrics(SM_CXVSCROLL);{17};
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TComboButton.Create (Self);
{$IFDEF Win32}
  FButton.ControlStyle := FButton.ControlStyle + [csReplicatable]; {CtlGrid}
{$ENDIF}
  FButton.SetBounds (0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  FGrid := TPopupGrid.Create(Self);
{$IFDEF Win32}
  FGrid.ControlStyle := FGrid.ControlStyle + [csNoDesignVisible, csReplicatable]; {CtlGrid}
{$ENDIF}
  FGrid.FCombo := Self;
  FGrid.Parent := Self;
  FGrid.Visible := False;
  FGrid.OnClick := GridClick;
  FGrid.FIgnorLUIndexErr := False;
  FGrid.FDisplayFldPosInIndex := 1;
  FSearchValue := '';
  Height := 24;
  FDropDownCount := 8;
  FDropDownAlign := Right;
  FDropDownTop := Below;
  FAutoDropDown := True;
  FSorted := False;
  FCanvas := Nil;
end;

destructor TDBLookupComboPlus.Destroy;
begin
  FFieldLink.OnDataChange := nil;
  FFieldLink.Free;
  FFieldLink := nil;
  FCanvas.Free;
  FCanvas := Nil;
  inherited Destroy;
end;

procedure TDBLookupComboPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FFieldLink <> nil) then
  begin
    if (AComponent = DataSource) then
      DataSource := nil
    else if (AComponent = LookupSource) then
      LookupSource := nil;
  end;
end;

procedure TDBLookupComboPlus.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and (DataSource<>nil) and (DataSource.DataSet<>nil) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType = ftString) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TDBLookupComboPlus.Loaded;
begin
  inherited Loaded;
  { get rid of speedbutton if not requried }
  ResetMaxLength;
  If not FShowSpeedButton then
  begin
    FButton.Free;
    FButton := Nil;
    FBtnControl.Free;
    FBtnControl := Nil;
  end;
  FGrid.FOnTranslate := FOnTranslate;
  FGrid.FOnSearchKeyPress := FOnSearchKeyPress;
  DataChange(Self);
end;

function TDBLookupComboPlus.Editable: Boolean;
begin
  Result := (FFieldLink.DataSource = nil) or    {editable if there is no connection a DB}
     (((FGrid.FValueFld = FGrid.FDisplayFld) or
        Assigned(FOnNewLookupRec)) and
     ((FStyle = csIncSrchEdit) or (FStyle = csDropDown)));
  If (FFieldLink.DataSource = nil) and
    (FStyle in [csDropDownList, csIncSearch]) then
    result := False;
end;

function TDBLookupComboPlus.CanEdit: Boolean;
begin
  Result := (FFieldLink.DataSource = nil) or
    (FFieldLink.Editing and Editable);
  If (FFieldLink.DataSource = nil) and
    (FStyle in [csDropDownList, csIncSearch]) then
    result := False;
end;

procedure TDBLookupComboPlus.DoSearch;
{Locates a display value in the lookup list}
begin
  if assigned(FOnSearchKeyPress) then
  begin
    FOnSearchKeyPress(Self, FGrid.FFoundValue);
    exit;
  end;
  if LookupSource.DataSet = nil then exit;
  if Assigned(FOnBeforeSearch) then FOnBeforeSearch(Self);
  FGrid.DoSearch(SearchValue, False, True);
  if Assigned(FOnAfterSearch) then FOnAfterSearch(Self);
end;

procedure TDBLookupComboPlus.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  FLastKey := key;
  FLastChar := #0;
  if Key in [VK_BACK, VK_DELETE, VK_INSERT] then
  begin
    if Editable then
      FFieldLink.Edit;
    if not CanEdit then
      Key := 0;
  end
  else if not Editable and (Key in [{VK_HOME, VK_END,} VK_LEFT, VK_RIGHT]) then
    Key := 0;
  if (Editable and (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR])) or
     (not Editable and (Key in [VK_HOME, VK_END, VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR])) then
  begin
    if not FGrid.Visible then
      DropDown
    else
    begin
      if (FStyle in [csIncSearch]) then searchvalue := '';
      FFieldLink.Edit;
      if (FFieldLink.DataSource = nil) or FFieldLink.Editing then
        FGrid.KeyDown (Key, Shift);
    end;
    Key := 0;
  end;
end;

procedure TDBLookupComboPlus.KeyPress(var Key: Char);
var
  SchResult,Str : String;
  X, I : Word;
begin
  inherited KeyPress(Key);
  case Key of
    ^H, ^V, ^X, #32..#255:     {BS, Insert, Down}
      begin
        if Editable or (FStyle in [csIncSearch]) then
          FFieldLink.Edit;
        if (FStyle in [csIncSearch, csIncSrchEdit]) then
        begin
          if ((FFieldLink.DataSource <> nil) and
            not (FFieldLink.DataSource.DataSet.CanModify)) then
          begin            {this new section 9/26 protects the}
            Key := #0;     {Displayed data from changing when the}
                           {DataSource is read only}
          end;
          if not FSorted then SortList;
          if not FGrid.Visible and AutoDropDown then DropDown;
          FLastChar := Key;
          If (FStyle = csIncSearch) {and (FFieldLink.Editing)} then   {<<< A mystery}
          begin
            Str := SearchValue;
            if (FLastChar = chr(8)) then   {BackSpace}
              delete(Str,length(SearchValue) ,1)
            else if FLastChar <> #0 then
              Str := Str + FLastChar;
            Searchvalue := Str;
            If (Searchvalue > '') then
            begin
              While True do
              begin
                DoSearch;
                SchResult := FGrid.FDisplayFld.asString;
                If SchResult = '' then exit;      { New 3/26/98 }
                I := 0;
                For X := 1 to Length(Str) do
                begin
                  I := X;
                  If UpCase(Str[I]) <> UpCase(SchResult[I]) then break;
                end;
                If (I < Length(Str)) or
                  ((SchResult[I] = ' ') and     {where there are spaces between words - Del Mar Heights}
                  (UpCase(Str[I]) <> UpCase(SchResult[I]))) then  {case of last char different}
                begin
                  Delete(str, Length(str), 1);
                  Searchvalue := Str;
                  If Length(Str) < 1 then break;
                end
                else BREAK;
              end;
            end
            else {if they backspaced into null, Ok}
              if FLastChar = chr(8) then
              begin
                DoSearch;
                SchResult := '';   {needed to suport numeric entries via before/aftersearch event}
              end;
            DisplayValue := SchResult{or FGrid.FDisplayFld.asString};
            DoSelectSome;
          end;
        end; {if FStyle in ....}
        if not CanEdit then
          Key := #0;
      end;
    Char (VK_RETURN):
    begin
      if (FStyle in [csIncSearch, csIncSrchEdit]) and FGrid.Visible then
      begin
        if FStyle in [csIncSrchEdit] then
        begin
          Str := Text;
          If AnsiCompareText(Str, Copy(FGrid.FDisplayFld.asString, 1, Length(Str))) = 0 then
            Text := FGrid.FDisplayFld.asString;
        end;
        CloseUp;
      end;
      Key := #0;  { must catch and remove this, since is actually multi-line }
    end;
    Char (VK_ESCAPE):
      begin
        if not FGrid.Visible and not FSorted then
          FFieldLink.Reset
        else
          CloseUp;
        DoSelectAll;
        if (FStyle in [csIncSearch, csIncSrchEdit]) then SelectAmt(0);
        Key := #0;
      end;
  end;
end;

procedure TDBLookupComboPlus.Change;
var
  SchResult,Str : String;
  I,X : Word;
  OldStart : Integer;
  StrNul : Array[0..255] of char;

  function IsSearchStrInResult(SString, SResult : String) : Word;
  {returns 0 if SSTring is the first characters of SResult otherwise
   returns the index of the character in SResult where they are different}
  Var
    I,X : Word;
    OK : Boolean;
  begin
    Ok := True;
    Result := 0;
    X := 0;
    if SResult = '' then exit;
    For I := 1 To Length(SString) do
    begin
      X := I;
      If UpCase(SString[I]) <> UpCase(SResult[I]) then
      begin
        OK := False;
        break;
      end;
    end;
    If Not OK then Result := X;
  end;

begin
  if FFieldLink.Editing then
    FFieldLink.Modified;
  inherited Change;
  If (FStyle = csIncSrchEdit) and FSorted and
    not(FLastChar in [#0]) then
  begin
    Str := Text;
    If Length(Str) < Length(SearchValue) then
      SearchValue := Str;
    if FLastKey = VK_BACK then
      delete(Str,length(SearchValue) ,1)
    else If (Str > '') then
    begin
      if (UpperCase(Str)<>UpperCase(SearchValue)) Then
      begin
        SearchValue := Str;
        DoSearch;
        SchResult := FGrid.FDisplayFld.asString;
        If (IsSearchStrInResult(Str, SchResult) = 0) and
           (SchResult <> '') then                                        {New 3/12/96}
        begin {Search String is in search result so auto fill}
          FAutoFill := True;
          Perform(WM_SETTEXT, 0, Longint(StrPCopy(StrNul, SchResult)));
          X := 0;
          For I := 1 to Length(Str) do
          begin
            X := I;
            If UpCase(Str[I]) <> UpCase(SchResult[I]) then break;
          end;
          SelectAmt(X);
          if Assigned(FOnLookupRecChanged) then   { New 3/12/96 }
            FOnLookupRecChanged(Self, True);
        end
        else {Search String not in Search Result so stop autofill}
        begin
          OldStart := SelStart;
          If FAutoFill then
          begin
            Perform(WM_SETTEXT, 0, Longint(StrPCopy(StrNul, Str)));
            SelStart := OldStart;
            FAutoFill := False;
          end;
        end;
      end;
    end
    else {if they backspaced into null, Ok}
      if FLastChar = chr(8) then
      begin
      end;
    if FGrid.DataLink.Active then
      FGrid.DisplayValue := FGrid.FDisplayFld.asString;
  end;
end;

function TDBLookupComboPlus.GetDataSource: TDataSource;
begin
  Result := FFieldLink.DataSource;
end;

procedure TDBLookupComboPlus.SetDataSource(Value: TDataSource);
begin
  if ((Value <> nil) and (Value = LookupSource)) OR
     ((Value <> nil) and (LookupSource <> nil) and
      (Value.DataSet <> nil) and
      (Value.DataSet = LookupSource.DataSet)) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}

{$IFDEF Win32}
  if not (FFieldLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
    FFieldLink.DataSource := Value;
{$IFDEF Win32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

function TDBLookupComboPlus.GetLookupSource: TDataSource;
begin
  Result := FGrid.LookupSource;
end;

procedure TDBLookupComboPlus.SetLookupSource(Value: TDataSource);
begin
  if (Value <> nil) and ((Value = DataSource) or
    ((Value.DataSet <> nil) and (Value.DataSet = FFieldLink.DataSet))) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}
  FGrid.LookupSource := Value;
  DataChange (Self);
{$IFDEF Win32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TDBLookupComboPlus.SetLookupIndex(Value: string);
begin
  if Assigned(FGrid) and (FGrid.LookupSource<>nil) and
     (FGrid.LookupSource.DataSet<>nil) and
     FGrid.LookupSource.DataSet.InheritsFrom(TQuery) then
    raise EInvalidOperation.Create('LookupIndex not used for TQuery LookupSources');
  fLookupIndex := Value;
end;


procedure TDBLookupComboPlus.SetLookupDisplay(const Value: string);
begin
  FGrid.LookupDisplay := Value; {call owned object property}
  FGrid.InitFields(True);
  SetValue ('');   {force a data update}
  DataChange (Self);
end;

function TDBLookupComboPlus.GetLookupDisplay: string;
begin
  Result := FGrid.LookupDisplay;
end;

procedure TDBLookupComboPlus.SetLookupField(const Value: string);
begin
  FGrid.LookupField := Value;
  FGrid.InitFields(True);
  DataChange (Self);
end;

function TDBLookupComboPlus.GetLookupField: string;
begin
  Result := FGrid.LookupField;
end;

function TDBLookupComboPlus.GetDataField: string;
begin
  Result := FFieldLink.FieldName;
end;

procedure TDBLookupComboPlus.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FFieldLink.FieldName := Value;
end;

procedure TDBLookupComboPlus.DataChange(Sender: TObject);
begin
  if (FFieldLink.Field <> nil) and not (csLoading in ComponentState) then
    Value := FFieldLink.Field.AsString
  else
    Text := EmptyStr;
end;

function TDBLookupComboPlus.GetValue: String;
begin
  if Editable then
    Result := Text
  else
    Result := FGrid.Value;
end;

function TDBLookupComboPlus.GetDisplayValue: String;
begin
  Result := Text;
end;

function TDBLookupComboPlus.GetListVisible : boolean;
begin
  result := FGrid.Visible;
end;

procedure TDBLookupComboPlus.SetDisplayValue(const NewValue: String);
begin
  if FGrid.DisplayValue <> NewValue then
  begin
    if FGrid.DataLink.Active then
    begin
      FGrid.DisplayValue := NewValue;
      Text := FGrid.DisplayValue;
      if Assigned(FOnLookupRecChanged) then
        FOnLookupRecChanged(Self, True); {2/17/96}
    end;
  end;
end;

procedure TDBLookupComboPlus.SetValue(const NewValue: String);
begin
  if FGrid.DataLink.Active and FFieldLink.Active and
      ((DataSource = LookupSource) or
      (DataSource.DataSet = LookupSource.DataSet)) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}
  if (FGrid.Value <> NewValue) or (Text <> NewValue) then
  begin
    if FGrid.DataLink.Active then
    begin
      FGrid.Value := NewValue;      {this moves to the right place in grid}
      Text := FGrid.DisplayValue;   {this returns the correct text}
    end;
  end;
end;

function TDBLookupComboPlus.GetIgnorLUIndexErr : Boolean;
begin
  result := FGrid.FIgnorLUIndexErr;
end;

procedure TDBLookupComboPlus.SetIgnorLUIndexErr(Value: Boolean);
begin
  FGrid.FIgnorLUIndexErr := Value;
end;

function TDBLookupComboPlus.GetDisplayFldPosInIndex : Integer;
begin
  result := FGrid.FDisplayFldPosInIndex;
end;

procedure TDBLookupComboPlus.SetDisplayFldPosInIndex(Value : Integer);
begin
  FGrid.FDisplayFldPosInIndex := Value;
end;

function TDBLookupComboPlus.GetReadOnly: Boolean;
begin
  Result := FFieldLink.ReadOnly;
end;

procedure TDBLookupComboPlus.SetReadOnly(Value: Boolean);
begin
  FFieldLink.ReadOnly := Value;
  inherited ReadOnly := not CanEdit;
end;

procedure TDBLookupComboPlus.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not CanEdit;
end;

procedure TDBLookupComboPlus.UpdateData(Sender: TObject);
var
  str : String;
begin
  if FFieldLink.Field <> nil then
  begin
    if Editable then
    begin
      if FStyle in [csIncSrchEdit] then
      begin  {bypass the new record if it's really OK data. See also the vk_enter in KeyPress}
        Str := Text;
        If AnsiCompareText(Str, Copy(FGrid.FDisplayFld.asString, 1, Length(FGrid.FDisplayFld.asString))) = 0 then
          Text := FGrid.FDisplayFld.asString;
      end;
      if not Assigned(FOnNewLookupRec) or (DisplayValue = '') then
        FFieldLink.Field.AsString := Text
      else
      begin
        if (FGrid.FDisplayFld.asString <> DisplayValue) then
          EnterLookupRec;
        if ((FGrid.FValueFld = FGrid.FDisplayFld) and
           ((FStyle = csIncSrchEdit) or (FStyle = csDropDown))) then
          {Edit box is editable}
          FFieldLink.Field.AsString := Text
        else
          FFieldLink.Field.AsString := FGrid.Value;
      end
    end
    else
      FFieldLink.Field.AsString := FGrid.Value;
  end;
end;

procedure TDBLookupComboPlus.FieldLinkActive(Sender: TObject);
begin
  if FFieldLink.Active and FGrid.DataLink.Active then
  begin
    FGrid.SetValue ('');   {force a data update}
    DataChange (Self)
  end;
end;

procedure TDBLookupComboPlus.WMPaste (var Message: TMessage);
begin
  if Editable then FFieldLink.Edit;
  if CanEdit then inherited;
end;

procedure TDBLookupComboPlus.WMCut (var Message: TMessage);
begin
  if Editable then FFieldLink.Edit;
  if CanEdit then inherited;
end;

procedure TDBLookupComboPlus.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TDBLookupComboPlus.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  FGrid.HandleNeeded;
  DataChange (Self);  {update to current value}
end;

procedure TDBLookupComboPlus.DestroyWnd;
begin
  Closeup;
  inherited DestroyWnd;
end;

procedure TDBLookupComboPlus.SetEditRect;
var
  Loc: TRect;
begin
  Loc.Bottom := ClientHeight {+ 1};  {+1 is workaround for windows paint bug}
  If showSpeedButton then
    Loc.Right := FBtnControl.Left + 1
  else
    Loc.Right := ClientWidth;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TDBLookupComboPlus.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if (csDesigning in ComponentState) then
    FGrid.SetBounds (0, Height + 1, 10, 10);
  MinHeight := GetMinHeight;   {Set text margin}
  { text edit bug: if size to less than minheight, then edit ctrl does not display the text }
  { The above statement was in the original code. I'm not sure I buy it.}
  if Height < MinHeight then Height := MinHeight
  else
  begin
    if ShowSpeedButton then
    begin
      if not NewStyleControls then
        FBtnControl.SetBounds (Width - FButton.Width, 0, FButton.Width, Height)
      else
{$IFDEF Win32}
        FBtnControl.SetBounds (ClientWidth - FButton.Width {- 2}, 0, FButton.Width, ClientHeight);
{$ELSE}
        FBtnControl.SetBounds (Width - FButton.Width - 2, 2, FButton.Width, ClientHeight - 4);
{$ENDIF}
      FButton.Height := FBtnControl.Height;
    end;
    SetEditRect;
  end;
end;

function TDBLookupComboPlus.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  ARect : TRect;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if BorderStyle = bsNone then
  begin
    FTopTextMargin := 0;
    FLeftTextMargin := 0;
  end
  else
  begin
    if handleAllocated then
      SendMessage(Handle, EM_GETRECT, 0, LongInt(@ARect))
    else
    begin
      ARect.Top := 0;
      ARect.Left := 0;
    end;
    FTopTextMargin := ARect.Top;
    FLeftTextMargin := ARect.Left;
  end;
  if IsWin31 and (BorderStyle = bsSingle) then
    Result := Metrics.tmHeight + (Metrics.tmHeight div 4) + GetSystemMetrics(SM_CYBORDER) * 4 + 1
  else
    Result := 0 {Metrics.tmHeight};
end;

procedure TDBLookupComboPlus.WMPaint(var Message: TWMPaint);
var
{$IFNDEF Win32}
  S: array[0..255] of Char;
{$ELSE}
  S : Variant; //String;
{$ENDIF}
  PS: TPaintStruct;
  ARect: TRect;
  TextLeft, TextTop: Integer;
  Focused: Boolean;
  DC: HDC;
  Metrics : TTextMetric;                 {new 1/23/96}
begin
  if (Editable or (fstyle = csIncSearch)) and (Text <> '')
{$IFDEF Win32}
  and not (csPaintCopy in ControlState)
{$ENDIF}
     then
  begin
    inherited;
    Exit;
  end;
  { if not editable with focus, need to do drawing to show proper focus }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    Focused := (GetFocus = Handle) {$IFDEF Win32}and not(csPaintCopy in ControlState){$ENDIF};
    FCanvas.Font := Font;
    with FCanvas do
    begin
      ARect := ClientRect;
      if (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
 {$IFNDEF Win32}
        FrameRect(ARect);   { draw the border }
 {$ENDIF}
        InflateRect(ARect, -1, -1);
      end;
      if not (Editable or (fstyle = csIncSearch)) or
             ((Text = '') and (fstyle = csIncSearch))
{$IFDEF Win32}
             or (csPaintCopy in ControlState)
{$ENDIF}
             then
      begin
        Brush.Style := bsSolid;   {fill the recttangle inside the boarder with color}
        Brush.Color := Color;
        FillRect (ARect);
        if (BorderStyle = bsSingle) then
          TextTop := FTopTextMargin
        else {bsNone}
          TextTop := FTopTextMargin;
        if ShowSpeedButton then
          ARect.Right := FBtnControl.Left - 1
        else
          ARect.Right := ClientWidth - 2; {new 1/21/96 Deven report added '- 2'}
{$IFNDEF Win32}
        StrPCopy (S, Text);
{$ENDIF}
        TextLeft := FLeftTextMargin;
        if Focused then  {draw the focus rectangle}
        begin
          Brush.Color := clHighlight;
          Font.Color := clHighlightText;
          ARect.Top := ARect.Top + FTopTextMargin;
{}        GetTextMetrics(FCanvas.Handle, Metrics); {new 1/23/96}
          ARect.Bottom := ARect.Top + Metrics.tmHeight;
        end;
{$IFDEF Win32}
{CtlGrid}      if (csPaintCopy in ControlState) and (FFieldLink.Field <> nil) and
{CtlGrid}          (LookupSource <> nil) then
{CtlGrid}      begin
{CtlGrid}        If (LookupField = LookupDisplay) or (FFieldLink.Field.DisplayText = '') then {don't do the lookup if not necessary}
{CtlGrid}          S := FFieldLink.Field.DisplayText
{CtlGrid}        else
{CtlGrid}          S := LookupSource.DataSet.Lookup(LookupField, FFieldLink.Field.DisplayText,
{CtlGrid}            LookupDisplay);
{CtlGrid}        If VarIsNull(S) then
{CtlGrid}          S := ''
{CtlGrid}        else if not VarIsEmpty(S) then S := '';
{CtlGrid}        ExtTextOut(FCanvas.Handle, TextLeft, TextTop, ETO_OPAQUE or ETO_CLIPPED, @ARect,
{CtlGrid}        PChar(String(S)), Length(String(S)), nil);
{CtlGrid}      end
{CtlGrid}      else
        ExtTextOut(FCanvas.Handle, TextLeft, TextTop, ETO_OPAQUE or ETO_CLIPPED, @ARect,
          PChar(Text), Length(Text), nil);
{$ELSE}
        ExtTextOut(FCanvas.Handle, TextLeft, TextTop, ETO_OPAQUE or ETO_CLIPPED, @ARect,
          S, StrLen(S), nil);
{$ENDIF}
        If assigned(FBtnControl) then FBtnControl.refresh;
        if Focused then
          DrawFocusRect (ARect);
      end;
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TDBLookupComboPlus.CMFontChanged(var Message: TMessage);
begin
  inherited;
  GetMinHeight;  { set FTextMargin }
end;

procedure TDBLookupComboPlus.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if assigned(FButton) then
    FButton.Enabled := Enabled;
end;

{$IFDEF Win32}
procedure TDBLookupComboPlus.CMGetDataLink(var Message: TMessage); {CtlGrid}
begin
  Message.Result := Integer(FFieldLink);
end;
{$ENDIF}

procedure TDBLookupComboPlus.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp; 
end;

procedure TDBLookupComboPlus.CMCancelMode(var Message: TCMCancelMode);
begin
  with Message do
    if (Sender <> Self) and (Sender <> FBtnControl) and
      (Sender <> FButton) and (Sender <> FGrid) then CloseUp;
end;

procedure TDBLookupComboPlus.EnterLookupRec;
var
  cancelled : Boolean;
begin
  Cancelled := True;
  if Assigned(FOnNewLookupRec) then FOnNewLookupRec(Self, Cancelled);
  If Cancelled then FFieldLink.Reset;
end;

procedure TDBLookupComboPlus.SortList;
{ Note that if this is not a sortable data set (not(TTable or TClientDataSet))
  then it just falls through and we say its sorted }
var
  I, pos : Integer;
{  OldKeyExclusive : Boolean; }
  HoldRec : PChar;
begin
  searchvalue := '';
  if not FSorted then
  begin
    if Assigned(FOnPrepareList) then FOnPrepareList(Self);
    if (LookupSource<>nil) and         { Save the original IndexName }
       (LookupSource.DataSet<>nil) then
    begin
      if Assigned(FOnSortList) then
      begin
        FOnSortList(Self);
      end
      else if (LookupSource.DataSet.InheritsFrom(TTable)) AND
        ((LookupSource.DataSet as TTable).MasterSource <> nil) then
      begin
        With (LookupSource.DataSet as TTable) do
        begin
          {Create a temp record buffer to hold the current record so we can go back to it in the new range}
          HoldRec := AllocMem(RecordSize);
          {Fill the temp record buffer with the current record}
          if  Active then
            move(ActiveBuffer^, HoldRec^, RecordSize);
          {Save current table state data}
          FSaveIndex := IndexFieldNames;
          FMastFields := MasterFields;
          FMastSource := MasterSource;
          {Change Index}
          IndexName  := FLookupIndex;
          {Create then new lookup detail range}
          SetRangeStart;
          Pos := 1;
          I := 0;
          while Pos <= Length(MasterFields) do
          begin
            IndexFields[I].AsString :=
            MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
            Inc(I);
          end;
          If not MasterSource.DataSet.EOF then
          begin
            SetRangeEnd;
            KeyExclusive := False;
            Pos := 1;
            I := 0;
            while Pos <= Length(MasterFields) do
            begin
              IndexFields[I].AsString :=
              MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
              Inc(I);
            end;
            {make sure to get everything through the z's}
            IndexFields[I].AsString := 'zzzzzzzzzzz';
          end;
          ApplyRange;
          {Move to the original record but in the new sort order}
          {Goto the same record that's in the temp record buffer}
          If RecordCount > 0 then
          begin
            CursorPosChanged;
            check(DbiGetRecordForKey(Handle, false, 0, 0, HoldRec, nil));
            Resync([rmExact, rmCenter]);
          end;
          {kill the temp record buffer}
          FreeMem(HoldRec, RecordSize);
          {restore the master tables original state }
        end;
      end
{$IFDEF D3OR4OR5}
    {$IFDEF ISCSVER}
      else if (LookupSource.DataSet.InheritsFrom(TClientDataSet)) AND
        ((LookupSource.DataSet as TClientDataSet).MasterSource <> nil) then
      begin
        With (LookupSource.DataSet as TMyClientDataSet) do
        begin
          {Create a temp record buffer to hold the current record so we can go back to it in the new range}
          HoldRec := AllocMem(RecordSize);
          {Fill the temp record buffer with the current record}
          if  Active then
            move(ActiveBuffer^, HoldRec^, RecordSize);
          {Save current table state data}
          FSaveIndex := IndexFieldNames;
          FMastFields := MasterFields;
          FMastSource := MasterSource;
          {Change Index}
          IndexName  := FLookupIndex;
          {Create then new lookup detail range}
          SetRangeStart;
          Pos := 1;
          I := 0;
          while Pos <= Length(MasterFields) do
          begin
            IndexFields[I].AsString :=
            MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
            Inc(I);
          end;
          If not MasterSource.DataSet.EOF then
          begin
            SetRangeEnd;
            KeyExclusive := False;
            Pos := 1;
            I := 0;
            while Pos <= Length(MasterFields) do
            begin
              IndexFields[I].AsString :=
              MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
              Inc(I);
            end;
            {make sure to get everything through the z's}
            IndexFields[I].AsString := 'zzzzzzzzzzz';
          end;
          ApplyRange;
          {Move to the original record but in the new sort order}
          {Goto the same record that's in the temp record buffer}
          If RecordCount > 0 then
          begin
            CursorPosChanged;
 {            check(DbiGetRecordForKey(Handle, false, 0, 0, HoldRec, nil)); }
            check(DSCursor.GetRecordForKey(0, 0, HoldRec, nil));
            Resync([rmExact, rmCenter]);
          end;
          {kill the temp record buffer}
          FreeMem(HoldRec, RecordSize);
          {restore the master tables original state }
        end;
      end
    {$ENDIF}
{$ENDIF}
      else
      begin
        {if its not a dataset type that can be sorted then just say its sorted}
        If LookupSource.DataSet.InheritsFrom(TTable) then
        begin
          FSaveIndex := (LookupSource.DataSet as TTable).IndexName;
          (LookupSource.DataSet as TTable).IndexName := FLookupIndex;
        end;
{$IFDEF D3OR4OR5}
    {$IFDEF ISCSVER}
        If LookupSource.DataSet.InheritsFrom(TClientDataSet) then
        begin
          FSaveIndex := (LookupSource.DataSet as TClientDataSet).IndexFieldNames;
          (LookupSource.DataSet as TClientDataSet).IndexName := FLookupIndex;
        end;
    {$ENDIF}
{$ENDIF}
      end;
      FSorted := True;
      if (Text = '') and (DataSource = Nil) then
        LookupSource.DataSet.First;
    end;
  end;
end;

procedure TDBLookupComboPlus.DropDown;
var
  ItemCount: Integer;
  P: TPoint;
  Y:  Integer;
  GridWidth, GridHeight, BorderWidth: Integer;
  ActAlign : TLeftRight;
begin
 {Fix 10/7 Technically this should only happen for the csIncSearch
  and csIncSrchEdit Styles.}
  if (FStyle in [csIncSearch, csIncSrchEdit]) then
    SortList;
  if not FGrid.Visible and (Width > 20) then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    ItemCount := DropDownCount;
    if DropDownCount = 0 then ItemCount := 1;
    P := ClientOrigin;            {top left corner of TEdit, TPoint}
    BorderWidth := 0;
    if loRowLines in Options then BorderWidth := 1;
    GridHeight := (FGrid.DefaultRowHeight + BorderWidth) *
      (ItemCount + FGrid.FTitleOffset) + 2;
    FGrid.Height := GridHeight;
    if ItemCount > FGrid.RowCount then
    begin
      ItemCount := FGrid.RowCount;
      GridHeight := (FGrid.DefaultRowHeight + BorderWidth) *
        (ItemCount + FGrid.FTitleOffset) + 4;
    end;
    if FDropDownTop = Below then
    begin
      Y := P.Y + Height - 1;  {calc Top of grid }
      if (Y + GridHeight) > Screen.height then Y := P.Y - GridHeight + 1;
      if Y < 0 then Y := P.Y + Height - 1;
    end
    else {FDropDownTop = Above}
    begin
      Y := P.Y - GridHeight + 1;
      if Y < 0 then Y := P.Y + Height - 1;
    end;
    GridWidth := DropDownWidth;
    if GridWidth = 0 then GridWidth := Width - 4;
    ActAlign := DropDownAlign;
    if ActAlign <> Right then
    begin
      if P.X + GridWidth > Screen.Width then
        ActAlign := Right;
    end;
    if ActAlign = Right then
    begin
      if P.X + Width - GridWidth < 0 then
      begin
        ActAlign := TLeftRight(0);  {same as left but doesn't conflict with TEdit.left}
        if P.X + GridWidth > Screen.Width then
            ActAlign := Right;
      end;
    end;
    If ActAlign = Right then
      SetWindowPos (FGrid.Handle, 0,
         P.X + Width - GridWidth,   {Left Side of Grid window}
         Y,                         {Top of Grid window}
         GridWidth,                 {Width of grid window}
         GridHeight,                {Height of Grid window}
         SWP_NOACTIVATE)
    else        {Left Side of Grid window}
      SetWindowPos (FGrid.Handle, 0,
         P.X,
         Y,                         {Top of Grid window}
         GridWidth,                 {Width of grid window}
         GridHeight,                {Height of Grid window}
         SWP_NOACTIVATE);
    if (Owner is TForm) and (TForm(Owner).FormStyle = fsStayOnTop) then
      SetWindowPos(Fgrid.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
           SWP_NOSIZE or SWP_NOACTIVATE);
    if Length (LookupField) = 0 then
      FGrid.DisplayValue := Text;
    FGrid.Visible := True;
{$IFDEF Win32}
    Windows.SetFocus(Handle);
{$ELSE}
    WinProcs.SetFocus(Handle);
{$ENDIF}
  end;
end;


procedure TDBLookupComboPlus.CloseUp;
var
  HoldRec : PChar;
begin
  if FSorted and
    (LookupSource<>nil) and         { Restore original IndexName }
    (LookupSource.DataSet<>nil) then
  begin
    if assigned(FOnUnSortList) then
    begin
      FOnUnSortList(Self);
    end
    else if (LookupSource.DataSet.InheritsFrom(TTable)) AND
        ((LookupSource.DataSet as TTable).MasterSource <> nil) then
    begin
      With (LookupSource.DataSet as TTable) do
      begin
        {Change back to the natural index}
        CancelRange;
        IndexFieldNames := FSaveIndex;
        {Create a temp record buffer}
        HoldRec := AllocMem(RecordSize);
        {Fill the temp record buffer with the current record}
        move(ActiveBuffer^, HoldRec^, RecordSize);
        {Reactivate the Master detail relationship}
        MasterSource := FMastSource;
        MasterFields := FMastFields;
        {Goto the same record that's in the temp record buffer}
        If RecordCount > 0 then
        begin
          CursorPosChanged;
          check(DbiGetRecordForKey(Handle, false, 0, 0, HoldRec, nil));
          Resync([rmExact, rmCenter]);
        end;
        {kill the temp record buffer}
        FreeMem(HoldRec, RecordSize);
      end;
    end
{$IFDEF D3OR4OR5}
   {$IFDEF ISCSVER}
    else if (LookupSource.DataSet.InheritsFrom(TClientDataSet)) AND
        ((LookupSource.DataSet as TClientDataSet).MasterSource <> nil) then
    begin
      With (LookupSource.DataSet as TMyClientDataSet) do
      begin
        {Change back to the natural index}
        CancelRange;
        IndexFieldNames := FSaveIndex;
        {Create a temp record buffer}
        HoldRec := AllocMem(RecordSize);
        {Fill the temp record buffer with the current record}
        move(ActiveBuffer^, HoldRec^, RecordSize);
        {Reactivate the Master detail relationship}
        MasterSource := FMastSource;
        MasterFields := FMastFields;
        {Goto the same record that's in the temp record buffer}
        If RecordCount > 0 then
        begin
          CursorPosChanged;
          check(DSCursor.GetRecordForKey(0, 0, HoldRec, nil));
          Resync([rmExact, rmCenter]);
        end;
        {kill the temp record buffer}
        FreeMem(HoldRec, RecordSize);
      end;
    end
   {$ENDIF}
{$ENDIF}
    else
    {if its not a dataset type that can be sorted then just say its not sorted}
    begin
      If LookupSource.DataSet.InheritsFrom(TTable) then
        (LookupSource.DataSet as TTable).IndexName := FSaveIndex;
{$IFDEF D3OR4OR5}
    {$IFDEF ISCSVER}
      If LookupSource.DataSet.InheritsFrom(TClientDataSet) then
        (LookupSource.DataSet as TClientDataSet).IndexFieldNames := FSaveIndex;
    {$ENDIF}
{$ENDIF}
    end;
    FSorted := False;
  end;
  if FGrid.Visible then
    FGrid.Visible := False;

  If (LookupSource <> nil) and (LookupSource.DataSet <> nil) and {new 1/96}
     LookupSource.DataSet.Active and AutoRefresh then
    LookupSource.DataSet.Refresh;

  if (FStyle in [csIncSearch, csIncSrchEdit]) then
  begin
    searchvalue := '';
    SelectAmt(0);
  end;
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;

procedure TDBLookupComboPlus.GridClick (Sender: TObject);
begin
  FFieldLink.Edit;
  if (FFieldLink.DataSource = nil) or FFieldLink.Editing then
  begin
    FFieldLink.Modified;
    Text := FGrid.DisplayValue;
    if Assigned(FOnLookupRecChanged) then
      FOnLookupRecChanged(Self, False);
  end;
end;

procedure TDBLookupComboPlus.SetStyle(Value: TDBLookupComboPlusStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
  end;
end;

Procedure TDBLookupComboPlus.SetShowSpeedButton(Value: Boolean);
begin
  if FShowSpeedButton <> Value then
  begin
   FShowSpeedButton := Value;
   If not FShowSpeedButton then
   begin
     FButton.Free;
     FButton := Nil;
     FBtnControl.Free;
     FBtnControl := Nil;
   end
   else
   begin
     FBtnControl := TWinControl.Create (Self);
     FBtnControl.Width := GetSystemMetrics(SM_CXVSCROLL); {17; }
     FBtnControl.Height := 0;
     FBtnControl.Visible := True;
     FBtnControl.Parent := Self;
     FButton := TComboButton.Create (Self);
     FButton.SetBounds (0, 0, FBtnControl.Width, FBtnControl.Height);
     FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));  {tacky!!}
     FButton.Visible := True;
     FButton.Parent := FBtnControl;
     if not NewStyleControls then
       FBtnControl.SetBounds (Width - FButton.Width, 0, FButton.Width, Height)
     else
       FBtnControl.SetBounds (Width - FButton.Width - 2, 2, FButton.Width, ClientHeight - 4);
     FButton.Height := FBtnControl.Height;
     SetEditRect;
   end;
   if HandleAllocated then
   begin
     SetEditRect;
     DataChange(Self);
   end;
  end;
end;

function TDBLookupComboPlus.GetListColor : TColor;
begin
  result := FGrid.Color;
end;

procedure TDBLookupComboPlus.SetListColor(value : TColor);
begin
  FGrid.Color := value;
end;

function TDBLookupComboPlus.GetListFont : TFont;
begin
  result := FGrid.Font;
end;

procedure TDBLookupComboPlus.SetListFont(value : TFont);
begin
  FGrid.Font := value;
end;

function TDBLookupComboPlus.GetListCursor : TCursor;
begin
  result := FGrid.Cursor;
end;

procedure TDBLookupComboPlus.SetListCursor(value : TCursor);
begin
  FGrid.Cursor := value;
end;


function TDBLookupComboPlus.GetButtonCursor : TCursor;
begin
  if assigned(FButton) then
    result := FButton.Cursor
  else
    result := 0;
end;

procedure TDBLookupComboPlus.SetButtonCursor(value : TCursor);
begin
  if assigned(FButton) then
    FButton.Cursor := value;
end;

procedure TDBLookupComboPlus.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if Editable then
    inherited
  else
    NonEditMouseDown (Message);
end;

procedure TDBLookupComboPlus.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if not Editable then MouseCapture := False;
  inherited;
end;

procedure TDBLookupComboPlus.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if Editable then
    inherited
  else
  begin
    NonEditMouseDown (Message);
    if csClickEvents in ControlStyle then DblClick;
  end;
end;

procedure TDBLookupComboPlus.NonEditMouseDown(var Message: TWMLButtonDown);
var
  CtrlState: TControlState;
begin
  SetFocus;
  HideCaret (Handle);
  if FGrid.Visible then
    CloseUp
  else
    DropDown;
  MouseCapture := True;
  if csClickEvents in ControlStyle then
  begin
    CtrlState := ControlState;
    Include(CtrlState, csClicked);
    ControlState := CtrlState;
  end;
  with Message do
    MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure MouseDragToGrid (Ctrl: TControl; Grid: TPopupGrid; X, Y: Integer);
var
  pt, clientPt: TPoint;
begin
  if (Grid.Visible) then
  begin
    pt.X := X;
    pt.Y := Y;
    pt := Ctrl.ClientToScreen (pt);
    clientPt := Grid.ClientOrigin;
    if (pt.X >= clientPt.X) and (pt.Y >= clientPt.Y) and
       (pt.X <= clientPt.X + Grid.ClientWidth) and
       (pt.Y <= clientPt.Y + Grid.ClientHeight) then
    begin
      Ctrl.Perform(WM_LBUTTONUP, 0, MakeLong (abs(X), abs(Y)));
      pt := Grid.ScreenToClient(pt);
      Grid.Perform(WM_LBUTTONDOWN, 0, MakeLong (abs(pt.x), abs(pt.y)));
    end;
  end;
end;

procedure TDBLookupComboPlus.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);
  if (ssLeft in Shift) and not Editable and (GetCapture = Handle) then
    MouseDragToGrid (Self, FGrid, X, Y);
end;

procedure TDBLookupComboPlus.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not Editable then
    HideCaret (Handle);
  if (FStyle in [csIncSearch, csIncSrchEdit]) then
    SelectAmt(0);
end;

procedure TDBLookupComboPlus.CMExit(var Message: TCMExit);
begin
  try
    FFieldLink.UpdateRecord;
  except
    DoSelectAll;
    SetFocus;
    raise;
  end;
  inherited;
  if not Editable then Invalidate;
end;

procedure TDBLookupComboPlus.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then DoSelectAll;
  inherited;
  if not Editable then Invalidate;
end;

procedure TDBLookupComboPlus.DoSelectAll;
begin
  if Editable then SelectAll;
end;

Procedure TDBLookupComboPlus.SelectAmt(Const NumChars : Integer);
{selects from NumChars to Max Int or Max short int}
{$IFNDEF Win32}
  type
    TSelection = record
      StartPos, EndPos: Integer;
    end;
  var
    Selection: TSelection;
{$ENDIF}
begin
{$IFDEF Win32}
  SendMessage(Handle, EM_SETSEL, MaxShort, NumChars);
{$ELSE}
  Selection.StartPos := MaxInt{NumChars};
  Selection.EndPos := NumChars{MaxInt};
  Perform(EM_SETSEL, 0, LongInt(Selection));
{$ENDIF}
end;

procedure TDBLookupComboPlus.DoSelectSome;
var
  Str : String;
  I,X : Word;
begin
  Str := SearchValue;
  X := 0;
  For I := 1 to Length(Str) do
  begin
    X := I;
    If UpCase(Str[I]) <> UpCase(Text[I]) then break;
  end;
  SelectAmt(X);
end;

procedure TDBLookupComboPlus.SetOptions(Value: TDBLUPlusListOptions);
begin
  FGrid.Options := Value;
end;

function TDBLookupComboPlus.GetOptions: TDBLUPlusListOptions;
begin
  Result := FGrid.Options;
end;

{ TLookupList }

constructor TDBLUPlusList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldLink := TFieldDataLink.Create;
  FFieldLink.Control := Self;
  FFieldLink.OnDataChange := DataChange;
  FFieldLink.OnEditingChange := EditingChange;
  FFieldLink.OnUpdateData := UpdateData;
  FFieldLink.OnActiveChange := FieldLinkActive;
  FTitleOffset := 0;
  FUpdateFields := False;
  FHiliteRow := -1;
  inherited Options := [dgRowSelect];
  FixedCols := 0;
  FixedRows := 0;
  Width := 121;
  Height := 97;
end;

destructor TDBLUPlusList.Destroy;
begin
  FFieldLink.OnDataChange := nil;
  FFieldLink.Free;
  FFieldLink := nil;
  inherited Destroy;
end;

procedure TDBLUPlusList.CreateWnd;
begin
  inherited CreateWnd;
  DataChange(Self);  {update to current value}
end;

procedure TDBLUPlusList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FFieldLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

function TDBLUPlusList.GetDataSource: TDataSource;
begin
  Result := FFieldLink.DataSource;
end;

procedure TDBLUPlusList.SetDataSource(Value: TDataSource);
begin
  if (Value <> nil) and ((Value = LookupSource) or ((Value.DataSet <> nil)
    and (Value.DataSet = DataLink.DataSet))) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}
  FFieldLink.DataSource := Value;
{$IFDEF Win32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

function TDBLUPlusList.GetLookupSource: TDataSource;
begin
  Result := inherited DataSource;
end;

procedure TDBLUPlusList.NewLayout;
begin
  InitFields(True);
  LayoutChanged;
  FValue := '';
  DataChange(Self);
end;

procedure TDBLUPlusList.SetLookupSource(Value: TDataSource);
begin
  if (Value <> nil) and ((Value = DataSource) or
    ((Value.DataSet <> nil) and (Value.DataSet = FFieldLink.DataSet))) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}
(*
//{ IFNDEF D3OR4OR5} {not Delphi 3 or 4}
//  if (Value <> nil) and (Value.DataSet <> nil) and
//      not (Value.DataSet.InheritsFrom(TTable)) then
//    raise EInvalidOperation.Create(LoadStr(SLookupTableError));
//{ ENDIF}
*)
  inherited DataSource := Value;
  NewLayout;
end;

procedure TDBLUPlusList.SetLookupDisplay(const Value: string);
begin
  if Value <> LookupDisplay then
  begin
    FLookupDisplay := Value;
    NewLayout;
  end;
end;

procedure TDBLUPlusList.SetLookupField(const Value: string);
begin
  if Value <> LookupField then
  begin
    FLookupField := Value;
    NewLayout;
  end;
end;

procedure TDBLUPlusList.SetValue(const Value: string);
{This method is a driving force. This not only sets the
current value of the grid it also moves to that part of the grid}
begin
  if DataLink.Active and FFieldLink.Active and
    ((DataSource = LookupSource) or
    (DataSource.DataSet = LookupSource.DataSet)) then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    raise EInvalidOperation.Create (LoadStr (SLookupSourceError));
{$ELSE}  {Delphi 3 or 4}
    raise EInvalidOperation.Create(SLookupSourceError);
{$ENDIF}
  {The FTitleOffset=0 if no grid title row or 1 if there is a grid title row}
  if (FValue <> Value) or (Row = FTitleOffset) then
  begin
    if DataLink.Active and (FValueFld <> nil) then
    begin
      FValue := Value;
      FHiliteRow := -1;     { to be reset in .HighlightCell }
      DoLookup;             { this is a really key step }
      if FFoundValue and (FValueFld <> FDisplayFld) then
        FDisplayValue := FDisplayFld.AsString
      else if (FValueFld = FDisplayFld) then FDisplayValue := FValue
      else FDisplayValue := '';
    end;
  end;
end;

function TDBLUPlusList.DoSearch(const Value: string;
               FindExact, FireOnSearchKeyPress: Boolean): Boolean;
{returns true if search was successful}
var
  Pos,
  I : Integer;
//  DEBUGSTR : String;
begin
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
  result := false;
{$ENDIF}
  if inherited DataSource.DataSet.inheritsFrom(TTable) then {Inherited DataSource is LookupDataSource}
  begin
    with TTable(inherited DataSource.DataSet) do
    begin
      if FindExact then
        setKey
      else
        editKey;
      If (MasterSource <> nil) and (MasterFields <> '') then
      begin
        Pos := 1;
        I := 0;
        while Pos <= Length(MasterFields) do
        begin
          IndexFields[I].AsString :=
            MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
          Inc(I);
        end;
        FDisplayFldPosInIndex := I + 1;
        KeyFieldCount := FDisplayFldPosInIndex; {must do this to get an exact match}
      end;
      FDisplayFld.AsString := Value;
      if FindExact then
        result := GotoKey
      else
      begin
        GotoNearest;
        result := True;
      end;
    end; {with}
  end
  {$IFDEF D3OR4OR5} {Delphi 3 or 4}
  {$IFDEF ISCSVER}
  else if inherited DataSource.DataSet.inheritsFrom(TClientDataSet) AND {Inherited DataSource is LookupDataSource}
   (TClientDataSet(inherited DataSource.DataSet).MasterSource <> nil) AND
   (TClientDataSet(inherited DataSource.DataSet).MasterFields <> '') then
  begin
    with TClientDataSet(inherited DataSource.DataSet) do
    begin
      if FindExact then
        setKey
      else
        editKey;
      Pos := 1;
      I := 0;
      while Pos <= Length(MasterFields) do
      begin
        IndexFields[I].AsString :=
        MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
        Inc(I);
      end;
      FDisplayFldPosInIndex := I + 1;
      KeyFieldCount := FDisplayFldPosInIndex; {must do this to get an exact match}
      FDisplayFld.AsString := Value;
      if FindExact then
        result := GotoKey
      else
      begin
        GotoNearest;
        result := True;
      end;
    end; {with}
  end
  {$ENDIF}
  {$ENDIF}
  else {any other type of dataset }
  begin
  {$IFDEF D3OR4OR5} {Delphi 3 4 5}
    with inherited DataSource.DataSet do
      if FindExact then
        result := Locate(FDisplayFld.FieldName, Value, [loCaseInsensitive])
      else
      begin
        result := False;
        if value = '' then exit; // this is for backspace to nothing.
        result := Locate(FDisplayFld.FieldName, Value, [loPartialKey,loCaseInsensitive]);
      end;
  {$ELSE}
    if FireOnSearchKeyPress and not(assigned(FOnSearchKeyPress)) then
      raise EInvalidOperation.Create('OnSearchKeyPress must be defined when LookupSource is not a TTable');
  {$ENDIF}
  end;
end;

procedure TDBLUPlusList.SetDisplayValue(const Value: string);
{Setting the DisplayValue has the side effect of moving to the correct
 record in the lookuptable}
begin
  if (FDisplayValue <> Value) or (Row = FTitleOffset) then
  begin
    If Assigned(FOnSearchKeyPress) then
    begin
      FDisplayValue := Value;
      if (FValueFld = FDisplayFld) then FValue := FDisplayValue
      else if not FFoundValue then
      begin
        FDisplayValue := '';
        FValue := '';
      end
      else { if (FValueFld <> FDisplayFld) then }
        FValue := FValueFld.AsString;
      exit;
    end;
    FFoundValue := False;
    if DataLink.Active and (FDisplayFld <> nil) then
    begin
      FHiliteRow := -1;     { to be reset in .HighlightCell }
      FFoundValue := DoSearch(Value, True, False);
      FDisplayValue := Value;
      if FValueFld = FDisplayFld then FValue := FDisplayValue
      else if not FFoundValue then
      begin
        FDisplayValue := '';
        FValue := '';
      end
      else FValue := FValueFld.AsString;     { if (FValueFld <> FDisplayFld) then }
    end;
  end;
end;

procedure TDBLUPlusList.DoLookup;
var
  I, Pos : Integer;
begin
  FFoundValue := False;
  if not HandleAllocated then Exit;
  if (Value = '') or visible then Exit;
  If assigned(FOnTranslate) then
  begin
    FOnTranslate(Self, FFoundValue);
    exit;
  end;

  if inherited DataSource.DataSet.InheritsFrom(TTable) then
  begin
    with TTable(inherited DataSource.DataSet) do {inherited dataSource is same as lookupdataSource}
    begin
      if (IndexFieldCount > 0) then
      begin  {make sure the specified index will work}
        if (AnsiCompareText(IndexFields[0].FieldName, LookupField) <> 0) and
           (MasterSource = nil) then
        begin
          if not FIgnorLUIndexErr then
            raise EInvalidOperation.Create
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
              (FmtLoadStr (SLookupIndexError, [LookupField]))
{$ELSE}  {Delphi 3 or 4}
              ('LookupIndexError ' + LookupField)
{$ENDIF}
          else
            exit;
        end;
      end;
      if State = dsSetKey then Exit;
      SetKey;
      If (MasterSource <> nil) and (MasterFields <> '') then
      begin
        Pos := 1;
        I := 0;
        while Pos <= Length(MasterFields) do
        begin
          IndexFields[I].AsString :=
          MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
          Inc(I);
        end;
        FDisplayFldPosInIndex := I + 1;
      end;
      FValueFld.AsString := Value;{set the index value to the current data in the edit}
      KeyFieldCount := FDisplayFldPosInIndex; {must do this to get an exact match}
      FFoundValue := GotoKey;
      if not FFoundValue then First;
    end;{with}
  end
{$IFDEF D3OR4OR5} {Delphi 3 or 4}
  {$IFDEF ISCSVER}
  else if inherited DataSource.DataSet.InheritsFrom(TClientDataSet) then
  begin
    with TClientDataSet(inherited DataSource.DataSet) do {inherited dataSource is same as lookupdataSource}
    begin
      if (IndexFieldCount > 0) then
      begin  {make sure the specified index will work}
       { tmp := IndexFields[0].FieldName; }
        if (AnsiCompareText(IndexFields[0].FieldName, LookupField) <> 0) and
           (MasterSource = nil) then
        begin
          if not FIgnorLUIndexErr then
            raise EInvalidOperation.Create
              ('LookupIndexError ' + LookupField)
          else
            exit;
        end;
      end;
      if State = dsSetKey then Exit;
      SetKey;
      If (MasterSource <> nil) and (MasterFields <> '') then
      begin
        Pos := 1;
        I := 0;
        while Pos <= Length(MasterFields) do
        begin
          IndexFields[I].AsString :=
          MasterSource.DataSet.FieldByName(ExtractFieldName(MasterFields, Pos)).AsString;
          Inc(I);
        end;
        FDisplayFldPosInIndex := I + 1;
      end;
      FValueFld.AsString := Value;{set the index value to the current data in the edit}
      KeyFieldCount := FDisplayFldPosInIndex; {must do this to get an exact match}
      FFoundValue := GotoKey;
      if not FFoundValue then First;
    end;{with}
  end
{$ENDIF}
{$ENDIF}
  else
  begin
{$IFDEF D3OR4OR5} {Delphi 3 or 4}
    with inherited DataSource.DataSet do {inherited dataSource is same as lookupdataSource}
    begin
      FFoundValue:= Locate(FValueFld.FieldName, Value, [loCaseInsensitive]);
      if not FFoundValue then First;
    end;{with}
{$ELSE}  {Not Delphi 3 or 4}
    if  not(assigned(FOnTranslate)) and not(csDesigning in ComponentState) then
      raise EInvalidOperation.Create('OnTranslate must be defined when LookupSource is not a TTable');
{$ENDIF}
  end
end;

function TDBLUPlusList.GetDataField: string;
begin
  Result := FFieldLink.FieldName;
end;

procedure TDBLUPlusList.SetDataField(const Value: string);
begin
  FFieldLink.FieldName := Value;
end;

function TDBLUPlusList.GetReadOnly: Boolean;
begin
  Result := FFieldLink.ReadOnly;
end;

function TDBLUPlusList.CanEdit: Boolean;
begin
  Result := (FFieldLink.DataSource = nil) or FFieldLink.Editing;
end;

procedure TDBLUPlusList.SetReadOnly(Value: Boolean);
begin
  FFieldLink.ReadOnly := Value;
end;

procedure TDBLUPlusList.DataChange(Sender: TObject);
begin
  if (FFieldLink.Field <> nil) and not (csLoading in ComponentState) then
    Value := FFieldLink.Field.AsString
  else
    Value := EmptyStr;
end;

procedure TDBLUPlusList.EditingChange(Sender: TObject);
begin
end;

procedure TDBLUPlusList.UpdateData(Sender: TObject);
begin
  if FFieldLink.Field <> nil then
    FFieldLink.Field.AsString := Value;
end;

procedure TDBLUPlusList.InitFields(ShowError: Boolean);
var
  Pos: Integer;
begin
  FDisplayFld := nil;      {set the two important TField Obj to Nil..}
  FValueFld := nil;         {..}
  if not DataLink.Active or (Length(LookupField) = 0) then Exit;
  with Datalink.DataSet do
  begin
    FValueFld := FindField(LookupField);  {Set FValueFld the safe way}
    if (FValueFld = nil) and ShowError then
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
      raise EInvalidOperation.Create(FmtLoadStr(SFieldNotFound, [LookupField]))
{$ELSE}  {Delphi 3 or 4}
      raise EInvalidOperation.Create('FieldNotFound ' + LookupField)
{$ENDIF}
    else if FValueFld <> nil then
    begin
      if Length (LookupDisplay) > 0 then {if LookupDisplay is filled in...}
      begin
        Pos := 1;    {Set the FDiaplayFld Value, use extract name cause multiple fields can be selected}
        FDisplayFld := FindField(ExtractFieldName(LookupDisplay, Pos));
        if (FDisplayFld = nil) and ShowError then
        begin
          Pos := 1;
{$IFNDEF D3OR4OR5} {not Delphi 3 or 4}
          raise EInvalidOperation.Create(FmtLoadStr(SFieldNotFound,
            [ExtractFieldName(LookupDisplay, Pos)]));
{$ELSE}  {Delphi 3 or 4}
          raise EInvalidOperation.Create('FieldNotFound ' + ExtractFieldName(LookupDisplay, Pos));
{$ENDIF}
        end;
      end; {if no FDisplayFld Speced then make it the same as the lookup field}
      if FDisplayFld = nil then FDisplayFld := FValueFld;
    end;
  end;
end;

procedure TDBLUPlusList.DefineFieldMap;
var
  Pos: Integer;
begin
  InitFields(False);
  if FValueFld <> nil then
  begin
    if Length(LookupDisplay) = 0 then
      Datalink.AddMapping (FValueFld.FieldName)
    else
    begin
      Pos := 1;
      while Pos <= Length(LookupDisplay) do
        Datalink.AddMapping (ExtractFieldName(LookupDisplay, Pos));
    end;
  end;
end;

procedure TDBLUPlusList.SetColumnAttributes;
var
  I: Integer;
  TotalWidth, BorderWidth: Integer;
begin
  inherited SetColumnAttributes;
  if FieldCount > 0 then
  begin
    BorderWidth := 0;
    if loColLines in FOptions then BorderWidth := 1;
    TotalWidth := 0;
    for I := 0 to ColCount -2 do
      TotalWidth := TotalWidth + ColWidths[I] + BorderWidth;
    if (ColCount = 1) or (TotalWidth < (ClientWidth - 15)) then
      ColWidths[ColCount-1] := ClientWidth - TotalWidth;
  end;
end;

procedure TDBLUPlusList.WMSize(var Message: TWMSize);
begin
  inherited;
  SetColumnAttributes;
end;

function TDBLUPlusList.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
var  {this function is probably not needed unless incsearch is implmented in a TDBLUPlusListPlus}
  MyOnKeyDown: TKeyEvent;
begin
  Result := True;
  if Key = VK_INSERT then Result := False
  else if Key in [VK_UP, VK_DOWN, VK_NEXT, VK_RIGHT, VK_LEFT, VK_PRIOR,
    VK_HOME, VK_END] then
  begin
    FFieldLink.Edit;
    if (Key in [VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT]) and not CanEdit then
      Result := False
    else if (inherited DataSource <> nil) and
        (inherited DataSource.State <> dsInactive) then
    begin {inherited DataSource is the same as LookupdataSource}
      if (FHiliteRow >= 0) and (FHiliteRow <> DataLink.ActiveRecord) then
      begin
        Row := FHiliteRow;
        Datalink.ActiveRecord := FHiliteRow;
      end
      else if (FHiliteRow < 0) then
      begin
        if FFoundValue then
          DoLookup 
        else
        begin
          DataLink.DataSource.DataSet.First;    {Does first on lookup Table}
          Row := FTitleOffset;
          Key := 0;
          MyOnKeyDown := OnKeyDown;
          if Assigned(MyOnKeyDown) then MyOnKeyDown(Self, Key, Shift);
          InvalidateRow (FTitleOffset);
          ListClick;
          Result := False;
        end;
      end;
    end;
  end;
end;

procedure TDBLUPlusList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  try
    FInCellSelect := True;
    inherited KeyDown (Key, Shift);
  finally
    FInCellSelect := False;
  end;
  if (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR, VK_HOME, VK_END]) and
      CanEdit then
    ListClick;
end;

procedure TDBLUPlusList.KeyPress(var Key: Char);
begin
  inherited KeyPress (Key);
  case Key of
    #32..#255:
      DataLink.Edit;
    Char (VK_ESCAPE):
      begin
        FFieldLink.Reset;
        Key := #0;
      end;
  end;
end;

procedure TDBLUPlusList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CellHit: TGridCoord;
  MyOnMouseDown: TMouseEvent;
begin
  if not (csDesigning in ComponentState) and CanFocus and TabStop then
  begin
    SetFocus;
    if ValidParentForm(Self).ActiveControl <> Self then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if ssDouble in Shift then
  begin
    DblClick;
    Exit;
  end;
  if (Button = mbLeft) and (DataLink.DataSource <> nil) and
    (FDisplayFld <> nil)  then
  begin
    CellHit := MouseCoord (X, Y);
    if (CellHit.Y >= FTitleOffset) then
    begin
      FFieldLink.Edit;
      FGridState := gsSelecting;
      SetTimer(Handle, 1, 60, nil);
      if (CellHit.Y <> (FHiliteRow + FTitleOffset)) then
      begin
        InvalidateRow (FHiliteRow + FTitleOffset);
        InvalidateRow (CellHit.Y);
      end;
      Row := CellHit.Y;
      Datalink.ActiveRecord := Row - FTitleOffset;
    end;
  end;
  MyOnMouseDown := OnMouseDown;
  if Assigned(MyOnMouseDown) then MyOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TDBLUPlusList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);
  if FGridState = gsSelecting then
    if (Row >= FTitleOffset) then
      Datalink.ActiveRecord := Row - FTitleOffset;
end;

procedure TDBLUPlusList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldState: TGridState;
begin
  OldState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);
  if OldState = gsSelecting then
  begin
    if (Row >= FTitleOffset) then
      Datalink.ActiveRecord := Row - FTitleOffset;
    ListClick;
  end;
end;

procedure TDBLUPlusList.ListClick;
begin
  if CanEdit and (FDisplayFld <> nil) then
  begin
    if FFieldLink.Editing then FFieldLink.Modified;
    FDisplayValue := FDisplayFld.AsString;
    if (FValueFld <> FDisplayFld) then
      FValue := FValueFld.AsString
    else FValue := FDisplayValue;
  end;
  if Assigned(FOnListClick) then
    FOnListClick(Self);
end;

function TDBLUPlusList.HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean;
var
  OldActive: Integer;
begin
  Result := False;
  if not DataLink.Active or (FValueFld = nil) then Exit;
  if (CanEdit) and ((FGridState = gsSelecting) or FInCellSelect) then
  begin
    if Row = (DataRow + FTitleOffset) then
    begin
      Result := True;
      FHiliteRow := DataRow;
    end;
  end
  else
  begin
    OldActive := DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord := DataRow;
      if (FValue = FValueFld.AsString) and
      ((FHiliteRow = -1) or (FHiliteRow = DataRow)) then {new 9/13 Fixes multiple higlhight problem}
      begin
        Result := True;
        FHiliteRow := DataRow;
      end;
    finally
      DataLink.ActiveRecord := OldActive;
    end;
  end;
end;

procedure TDBLUPlusList.Paint;
begin
  FHiliteRow := -1;
  inherited Paint;
  if Focused and (FHiliteRow <> -1) then
    Canvas.DrawFocusRect(BoxRect(0, FHiliteRow, MaxInt, FHiliteRow));
end;

procedure TDBLUPlusList.Scroll(Distance: Integer);
begin
  if FHiliteRow >= 0 then
  begin
    FHiliteRow := FHiliteRow - Distance;
    if FHiliteRow >= VisibleRowCount then
      FHiliteRow := -1;
  end;
  inherited Scroll(Distance);
end;

procedure TDBLUPlusList.LinkActive(Value: Boolean);
begin
  inherited LinkActive (Value);
  if DataLink.Active then
  begin
(* { IFNDEF D3OR4OR5} {not Delphi 3 or 4}
    if not (LookupSource.DataSet.InheritsFrom(TTable)) then
      raise EInvalidOperation.Create (LoadStr (SLookupTableError));
  { ENDIF} *)
    SetValue ('');   {force a data update}
    DataChange(Self);
  end;
end;

procedure TDBLUPlusList.FieldLinkActive(Sender: TObject);
begin
  if FFieldLink.Active and DataLink.Active then
    DataChange(Self);
end;

procedure TDBLUPlusList.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if FHiliteRow <> -1 then InvalidateRow(FHiliteRow);
end;

procedure TDBLUPlusList.CMExit(var Message: TCMExit);
begin
  try
    FFieldLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
  if FHiliteRow <> -1 then InvalidateRow(FHiliteRow);
end;

procedure TDBLUPlusList.SetOptions(Value: TDBLUPlusListOptions);
var
  NewGridOptions: TDBGridOptions;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    FTitleOffset := 0;
    NewGridOptions := [dgRowSelect];
    if loColLines in Value then
      NewGridOptions := NewGridOptions + [dgColLines];
    if loRowLines in Value then
      NewGridOptions := NewGridOptions + [dgRowLines];
    if loTitles in Value then
    begin
      FTitleOffset := 1;
      NewGridOptions := NewGridOptions + [dgTitles];
    end;
    inherited Options := NewGridOptions;
  end;
end;

procedure TDBLUPlusList.Loaded;
begin
  inherited Loaded;
  DataChange(Self);
end;

{ TPopupGrid }

constructor TPopupGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcquireFocus := False;
  TabStop := False;
end;

procedure TPopupGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{$IFDEF Win32}
  with Params do
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
{$ENDIF}
  Params.WindowClass.Style := CS_SAVEBITS;
end;

procedure TPopupGrid.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
{$IFDEF Win32}
    Windows.SetParent(Handle, 0);
{$ELSE}
    WinProcs.SetParent(Handle, 0);
{$ENDIF}
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  FCombo.DataChange(Self);  {update to current value}
end;

procedure TPopupGrid.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  with Message do
    FCombo.CloseUp;
end;

function TPopupGrid.CanEdit: Boolean;
begin
  Result := (FCombo.FFieldLink.DataSource = nil) or FCombo.FFieldLink.Editing;
end;

procedure TPopupGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FCombo.FFieldLink.Edit;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPopupGrid.LinkActive(Value: Boolean);
begin
  if Parent = nil then Exit;   {grid being destroyed}
  inherited LinkActive (Value);
  if DataLink.Active then
  begin
    if FValueFld = nil then InitFields(True);
    SetValue ('');   {force a data update}
    FCombo.DataChange (Self);
  end;
end;

{ TComboButton }

procedure TComboButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  with TDBLookupComboPlus (Parent.Parent) do
  begin
    if not FGrid.Visible then
    begin
      if (Handle <> GetFocus) and CanFocus then
      begin
        SetFocus;
        if GetFocus <> Handle then Exit; 
      end;
    end;
  end;
  inherited MouseDown (Button, Shift, X, Y);
  with TDBLookupComboPlus (Parent.Parent) do
  begin
    if FGrid.Visible then
      CloseUp
    else
    begin
      DropDown;
    end;
  end;
end;

procedure TComboButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);
  if (ssLeft in Shift) and (GetCapture = Parent.Handle) then
    MouseDragToGrid (Self, TDBLookupComboPlus (Parent.Parent).FGrid, X, Y);
end;

function CheckForWin31 : boolean;
var	 L : LONGINT;
	 A : ARRAY[0..3] OF BYTE ABSOLUTE L;
begin
 L := GetVersion;
 CheckForWin31 := (A[0] = 3) AND (A[1] = 10)
end;

BEGIN
 IF CheckForWin31 then IsWin31 := TRUE;
END.

