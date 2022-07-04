{ The unit SqlitePassVisualTools implements the generic TSqlitePassDBAction class
  and preset actions like TSqlitePassDatasetFilter, TSqlitePassDatasetFilterOnOff,
  TSqlitePassDatasetFilterOnSelection, TSqlitePassDatasetLocate, etc...

  
  ---------------------------------------------------------------------------
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    Last update : 2009-08-20

  --------------------------------------------------------------------------- }

unit SqlitePassVisualTools;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
  LCLType,
  LCLProc,
 {$ELSE}
  Windows,
  Math,
 {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Extctrls, Buttons, TypInfo,
  stdctrls, Grids, Db, DbGrids, ActnList, DBActns, DBCtrls, Menus, SqlitePassDbo,
  SqlitePassVisualToolsLang;

const
EmptyImageIndex = -1;
PlusImageIndex = 0;
MinusImageIndex = 1;
SelectedImageIndex = 2;
DatabaseImageIndex = 3;
TableImageIndex = 4;
QueryImageIndex = 5;
WiewImageIndex = 6;
TriggerImageIndex = 7;
SQLImageIndex = 8;
SystemTableImageIndex = 9;
TableItemImageIndex = 10;
SQLItemImageIndex = 11;
QueryItemImageIndex = 12;
AttachedItemImageIndex = 13;

type
  TSPVTVAlignment = (vaTop, vaCenter, vaBottom);

  TSPVTCtrlGrid = class;

  TSPVTCtrlGridState = (cgsBrowse, cgsEdit, cgsInsert, cgsAppend, cgsOrder);
  TSPVTCtrlGridNotifyEvent = procedure(CtrlGrid: TSPVTCtrlGrid) of object;

  TSPVTCtrlGrid = class(TScrollBox)
   private
    FBmpBrowse: TBitmap;
    FBmpEdit: TBitmap;
    FBmpInsert: TBitmap;
    FBof: Boolean;
    FControlIndex: Integer;
    FColorUnSelected: TColor;
    FColorSelected: TColor;
    FEof: Boolean;
    FImageEditState: TImage;
    FPanel: TPanel;
    FTabControlNames: TStringList;
    FShowEditState: Boolean;
    FShowDragGrip: Boolean;
    FCanDelete: Boolean;
    FCanInsert: Boolean;
    FActiveRowIndex: Integer;
    FActiveRow: TPanel;
    FRows: TList;
    FOnNewRecord: TSPVTCtrlGridNotifyEvent;
    FState: TSPVTCtrlGridState;
    FAfterScroll: TSPVTCtrlGridNotifyEvent;
    FBeforeScroll: TSPVTCtrlGridNotifyEvent;
    procedure SetFPanel(const Value: TPanel);
    function CloneControl(Control: TControl; Parent: TWinControl): TControl;
    Procedure CloneChildControls(Source, Dest: TPanel);
    procedure SetFActiveRowIndex(const Value: Integer);
    procedure SetFActiveRow(Const Value: TPanel);
    function CreateNewRow(PriorRow: Integer = -1): TPanel;
    procedure _OnRowClick(Sender: TObject);
    procedure _OnClick(Sender: TObject);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure DispachReturnKey;
    procedure SetFState(const Value: TSPVTCtrlGridState);
    function GetFRowCount: Integer;
    function GetFIsEmpty: Boolean;
   protected
   public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function Append: TPanel;
   procedure Delete;
   function Insert: TPanel;
   procedure Clear;
   procedure First;
   procedure Prior;
   procedure Next;
   procedure Last;
   procedure MoveUp;
   procedure MoveDown;
   Procedure Post;
   procedure Exchange(Index1, Index2: Integer);
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   function ControlByName(ControlName: String): TControl;
   procedure FocusActiveRow(const Value: TObject);
   procedure FocusFirstControl;
   Property BOF: Boolean Read FBof;
   Property CanInsert: Boolean Read FCanInsert Write FCanInsert;
   Property CanDelete: Boolean Read FCanDelete Write FCanDelete;
   Property ColorSelected: TColor Read FColorSelected Write FColorSelected;
   Property EOF: Boolean Read FEof;
   Property IsEmpty: Boolean Read GetFIsEmpty;
   Property Panel: TPanel Read FPanel Write SetFPanel;
   Property ActiveRow: TPanel read FActiveRow write SetFActiveRow;
   Property ActiveRowIndex: Integer Read FActiveRowIndex Write SetFActiveRowIndex;
   Property RowCount: Integer Read GetFRowCount;
   Property ShowEditState: Boolean Read FShowEditState Write FShowEditState;
   Property ShowDragGrip: Boolean Read FShowDragGrip Write FShowDragGrip;
   Property State: TSPVTCtrlGridState Read FState Write SetFState;
   property OnNewRecord: TSPVTCtrlGridNotifyEvent read FOnNewRecord write FOnNewRecord;
   property BeforeScroll: TSPVTCtrlGridNotifyEvent read FBeforeScroll write FBeforeScroll;
   property AfterScroll: TSPVTCtrlGridNotifyEvent read FAfterScroll write FAfterScroll;
   end;

  TSPVTNavButtons = (NavFirst, NavPrior, NavNext, NavLast, NavInsert, NavAppend, NavDelete, NavMoveUp, NavMoveDown);

  TSPVTCtrlGridNavigator = class(TCustomPanel)
  private
    FActiveButton: Integer;
    FButtons: array[TSPVTNavButtons] of TSpeedButton;
    FButtonCount: Integer;
    FCtrlGrid: TSPVTCtrlGrid;
    FTimer: TTimer;
    procedure _OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure _OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure _OnButtonClick(Btn: Integer);
    Procedure _RefreshButtonState;
    procedure _SetSize;//(var Width, Height: Integer);
    procedure TimerEvent(Sender:TObject);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    Property CtrlGrid: TSPVTCtrlGrid read FCtrlGrid write FCtrlGrid;
    property Align;
    Property BevelInner;
    Property BevelOuter;
    Property BevelWidth;
    Property BorderWidth;
    Property BorderStyle;
    Property Color;
//    Property Ctl3D;
    Property ShowHint;
    Property Tag;
    Property Visible;
  end;

  TSqlitePassActiveControlWatcher = class(TPersistent)
  private
    FEnabled: Boolean;
    FActiveControl: TWinControl;
    FActiveDataField: TField;
    FActiveDataset: TSqlitePassDataset;
    FActiveDataSource: TDataSource;
    FConnected: Boolean;
    FDataAware: Boolean;
    FLocked: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure GetInfos;
  public
    property Connected: Boolean read FConnected;
    property Control: TWinControl read FActiveControl;
    property DataAware: Boolean read FDataAware;
    property Dataset: TSqlitePassDataset Read FActiveDataset;
    property DataSource: TDataSource Read FActiveDataSource;
    property DataField: TField Read FActiveDataField;
    property Locked: Boolean Read FLocked Write FLocked;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TSPVTDBItemList = class;
  TSPVTDBItemType = (itDatabase, itTable, itQuery, itView, itTrigger, itTableSystem,
                     itSql, itSqlSelect, itSqlCreate, itSqlCreateTable, itSqlUpdate,
                     itSqlInsert, itSqlDelete, itSqlOthers);
  TSPVTDBItemTypes = set of TSPVTDBItemType;

  TSPVTDBItem = class
    Private
    FNestedLevel: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndexSelected: Integer;
    FImageIndex: Integer;
    FCaption: String;
    FColor: TColor;
    FFont: TFont;
    FParent: TSPVTDBItem;
    FChildren: TSPVTDBItemList;
    FItemType: TSPVTDBItemType;
    FExpanded: Boolean;
    FAttached: Boolean;
    function GetFHasCHildren: Boolean;
    Public
    Property Caption: String read FCaption write FCaption;
    Property Font: TFont read FFont write FFont;
    Property Color: TColor read FColor write FColor;
    Property ImageIndexSelected: Integer read FImageIndexSelected write FImageIndexSelected;
    Property ImageIndex: Integer read FImageIndex write FImageIndex;
    Property Enabled: Boolean read FEnabled write FEnabled;
    Property Expanded: Boolean read FExpanded write FExpanded;
    Property HasChildren: Boolean read GetFHasCHildren;
    Property Attached: Boolean read FAttached;
    Property ItemType: TSPVTDBItemType read FItemType write FItemType;
    Property Visible: Boolean read FVisible write FVisible;
    Property Children: TSPVTDBItemList read FChildren;
    constructor Create(_Caption: String;
                       _ItemType:TSPVTDBItemType;
                       _Font: TFont;
                       _Color: TColor;
                       _ImageIndexSelected: Integer;
                       _ImageIndex: Integer;
                       _Attached: Boolean;
                       _Enabled: Boolean;
                       _Visible: Boolean);
    destructor Destroy; override;

    function AddChildItem(_Caption: String;
                   _ItemType: TSPVTDBItemType;
                   _Font: TFont;
                   _Color: TColor;
                   _ImageIndexSelected: Integer;
                   _ImageIndex: Integer;
                   _Attached: Boolean;
                   _Enabled: Boolean;
                   _Visible: Boolean): TSPVTDBItem;
    Property Parent: TSPVTDBItem read FParent;
    end;

  TSPVTDBItemList = class(TList)
  private
  Protected
  function GetItem(Index: Integer): TSPVTDBItem;
  procedure SetItem(Index: Integer; const Value: TSPVTDBItem);
  public
  constructor Create;
  destructor Destroy; override;
  procedure ClearAndFreeItems;
  function AddItem(_Caption: String;
                   _ItemType: TSPVTDBItemType;
                   _Font: TFont;
                   _Color: TColor;
                   _ImageIndexSelected: Integer;
                   _ImageIndex: Integer;
                   _Attached: Boolean;
                   _Enabled: Boolean;
                   _Visible: Boolean): TSPVTDBItem;

  function ItemByName(const Value: String): TSPVTDBItem;
  property Items[Index: Integer]: TSPVTDBItem read GetItem write SetItem; default;
  end;

  TSPVTDBTreeView = class(TDrawGrid)
  private
    FDatabase: TSqlitePassDatabase;
    FDBItems: TSPVTDBItemList;
    FVisibleDBItems: TSPVTDBItemList;
    FImageList: TImageList;
    FLeftMargin: Integer;
    FSelectedIndex: Integer;
    FTempImage: TImage;
    FVisibleItems: TSPVTDBItemTypes;
    procedure RefreshVisibleItems(FullExpand: Boolean = False);
    function GetFSelectedItem: TSPVTDBItem;
    procedure SetFImageList(const Value: TImageList);
  protected
    procedure DrawDatabaseItems(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
              aState:TGridDrawState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshItems;
    procedure SelectByName(ItemName: String; ItemType: TSPVTDBItemTypes);

    Property SelectedItem: TSPVTDBItem read GetFSelectedItem;
    Property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex;
    Property VisibleItems: TSPVTDBItemTypes read FVisibleItems write FVisibleItems;
  published
    property Database: TSqlitePassDatabase read FDatabase write FDatabase;
    property ImageList: TImageList Read FImageList Write SetFImageList;
  end;

  {TSqlitePassDBActionList is derived from TActionList to provide additional features related to
   TSqlitePassDatabase or TSqlitePassDataset.
   @br
   @br
   As an example, the demo program SqliteToolbox uses the TSqlitePassDBActionList component
   @br
   @br
   @image(..\..\Documentation\Images\DbToolbar.png)}
  TSqlitePassDBActionList = class(TActionList)
  private
   FActiveControl: TSqlitePassActiveControlWatcher;
   FDataSource: TDataSource;
   FDataSourceAuto: Boolean;
   procedure SetDataSource(Value: TDataSource);
  protected
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function UpdateAction(Action: TBasicAction): Boolean; override;
   {The ActiveControl property keeps track of the current active control and gives information on
    his associated datasource, datafield...
    @seealso(TSqlitePassActiveControlWatcher)}
   property ActiveControl: TSqlitePassActiveControlWatcher read FActiveControl write FActiveControl;
  published
   {The Datasource }
   property DataSource: TDataSource read FDataSource write SetDataSource;
   { When set to TRUE, the datasource is automatically updated to reflect the active control datasource. When set to FALSE,
    the Datasource keeps her original setting}
   property DataSourceAuto: Boolean read FDataSourceAuto write FDataSourceAuto;
  end;


  TSqlitePassDBAction = class(TDatasetAction)
  public
   {Returns TRUE if the Active control is able to process the requested action}
   Function CanProcessAction: Boolean;
   {}
   procedure UpdateTarget(Target: TObject); override;
 end;

{ --------------------------------------------------------------
  Preset DBAware Actions
  -------------------------------------------------------------- }

  { Sort }


  {TSqlitePassDatasetSortAsc sorts the dataset on active control ascending order}
  TSqlitePassDatasetSortAsc = class(TSqlitePassDBAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {TSqlitePassDatasetSortAsc sorts the dataset on active control descending order}
  TSqlitePassDatasetSortDesc = class(TSqlitePassDBAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {TSqlitePassDatasetSortAsc shows a custom dialog to choose the dataset sort parameters
  @br
  @br
  @image(..\..\Documentation\Images\SortByDialog.png)}
  TSqlitePassDatasetSort = class(TSqlitePassDBAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { Filter }
  TSqlitePassDatasetFilterOnSelection = class;
  TSqlitePassDatasetFilter = class;
  TSqlitePassDatasetFilterOnOff = class;

  {TSqlitePassDatasetFilterOnSelection filters the dataset on the active control datafield}
  TSqlitePassDatasetFilterOnSelection = class(TSqlitePassDBAction)
  private
    FFilterOnOff: TSqlitePassDatasetFilterOnOff;
    FFilterText: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    Property FilterText: String Read FFilterText;
  published
    Property FilterOnOff: TSqlitePassDatasetFilterOnOff Read FFilterOnOff Write FFilterOnOff;
  end;

  {TSqlitePassDatasetFilter shows a custom dialog to choose the dataset Filter parameters
  @br
  @br
  @image(..\..\Documentation\Images\FilterDialog.png)}
  TSqlitePassDatasetFilter = class(TSqlitePassDBAction)
  private
    FFilterOnOff: TSqlitePassDatasetFilterOnOff;
    FFilterText: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    Property FilterText: String Read FFilterText;
    published
    Property FilterOnOff: TSqlitePassDatasetFilterOnOff Read FFilterOnOff Write FFilterOnOff;
  end;

  {TSqlitePassDatasetFilterOnOff activates or desactivates the current filter designed with TSqlitePassDatasetFilter.}
  TSqlitePassDatasetFilterOnOff = class(TSqlitePassDBAction)
  private
    FFilterEditor: TSqlitePassDatasetFilter;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    Property FilterEditor: TSqlitePassDatasetFilter Read FFilterEditor Write FFilterEditor;
  end;


  { Locate }
  {TSqlitePassDatasetLocate shows a custom dialog to define locate parameters.
  @br
  @br
  @image(..\..\Documentation\Images\LocateDialog_1.png)}
  TSqlitePassDatasetLocate = class(TSqlitePassDBAction)
  public
    FStatusLabel: TLabel;
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property StatusLabel: TLabel Read FStatusLabel Write FStatusLabel;
  end;

  {TSqlitePassDBLocateMoveAction is an internal generic TSqlitePassDBAction designed
   to navigate through located records.}
  TSqlitePassDBLocateMoveAction = Class(TSqlitePassDBAction)
  public
  procedure UpdateTarget(Target: TObject); override;
  end;

  {TSqlitePassDatasetLocateFirst moves to the first located record}
  TSqlitePassDatasetLocateFirst = class(TSqlitePassDBLocateMoveAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {TSqlitePassDatasetLocateNext moves to the next located record. When the last located record
   is  reached, it starts over and move to the first located record}
  TSqlitePassDatasetLocateNext = class(TSqlitePassDBLocateMoveAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {TSqlitePassDatasetLocatePrior moves to the prior located record. When the first located record
   is  reached, it starts over and move to the last located record}
  TSqlitePassDatasetLocatePrior = class(TSqlitePassDBLocateMoveAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

 {TSqlitePassDatasetLocateLast moves to the last located record}
  TSqlitePassDatasetLocateLast = class(TSqlitePassDBLocateMoveAction)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

Const
  SPVTNavButtonName: array[TSPVTNavButtons] of PChar = ('FIRST', 'PRIOR', 'NEXT',
    'LAST', 'INSERT', 'APPEND', 'DELETE', 'UP', 'DOWN');

{ DbGrid  utility Functions}
procedure ResetColumnTitles(DbGrid: TDBGrid);
procedure SortDbGrid(DbGrid: TdbGrid; Column: TColumn);

{ DrawGrid  utility Functions}
procedure DisplayText(Grid: TDrawGrid;
                      Col, Row: Integer;
                      Rect: TRect;
                      Text: String;
                      Font: TFont;
                      HAlignment: TAlignment; VAlignment: TSPVTVAlignment;
                      Margins: TRect;
                      Autosize: Boolean = True);

procedure DisplayImage(Grid: TDrawGrid;
                       Col, Row: Integer;
                       Rect: TRect;
                       ImageList: TImageList;
                       ImageIndex: Integer;
                       HAlignment: TAlignment; VAlignment: TSPVTVAlignment;
                       Margins: TRect);

implementation

Uses SqlitePassSortByDialog, SqlitePassFiltersDialog, SqlitePassLocateDialog;

{$IFNDEF FPC}
  {$R *.RES}
{$ENDIF}


{ TSPVTCtrlGrid }

constructor TSPVTCtrlGrid.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Parent := TWinControl(AOwner);
FRows := TList.Create;
FBmpBrowse := TBitmap.Create;
FBmpEdit := TBitmap.Create;
FBmpInsert := TBitmap.Create;
{$IFDEF FPC}
FBmpBrowse.LoadFromLazarusResource('CG_BROWSE');
FBmpEdit.LoadFromLazarusResource('CG_EDIT');
FBmpInsert.LoadFromLazarusResource('CG_NEW');
{$ELSE}
FBmpBrowse.LoadFromResourceName(HInstance, 'CG_BROWSE');
FBmpEdit.LoadFromResourceName(HInstance, 'CG_EDIT');
FBmpInsert.LoadFromResourceName(HInstance, 'CG_NEW');
{$ENDIF}
FImageEditState := TImage.Create(Self);
FCanInsert := True;
FCanDelete := True;
FTabControlNames := TStringList.Create;
FBof := True;
FEof := True;
With FImageEditState do
     begin
     Transparent := True;
     AutoSize := True;
     Left := 2;
     State := cgsBrowse;
     end;
FActiveRowIndex := -1;
FActiveRow := nil;
Color := $00E2E2E2;
FColorSelected := clBtnShadow;
FColorUnSelected := cl3DLight;
OnClick := _OnClick;
end;

destructor TSPVTCtrlGrid.Destroy;
begin
FBmpBrowse.Free;
FBmpEdit.Free;
FBmpInsert.Free;
FRows.Free;
FTabControlNames.Free;
Inherited Destroy;
end;

function TSPVTCtrlGrid.CloneControl(Control: TControl; Parent: TWinControl): TControl;
var
MemoryStream: TMemoryStream;
WasNamed: String;
ControlMethod: TMethod;
PData   : PTypeData;
PListe  : PPropList;
//PropertyName: String;
i, PropCount: integer;
begin
  Result := nil;

  { Copies the properties }
  MemoryStream := TMemoryStream.Create;
  try
  WasNamed := Control.Name;
  Control.Name := '';
  MemoryStream.WriteComponent(Control);
  MemoryStream.Position := 0;
  Result := TControlClass(Control.ClassType).Create(Self);
  Result.Parent := Parent;
  MemoryStream.ReadComponent(Result);
  Result.Name := WasNamed + '_' + IntToStr(Random(MaxInt));

  { Assigns the control event methods to the clone ones }
  PData := GetTypeData(PTypeInfo(Control.ClassInfo));
  PropCount := PData^.PropCount;
  New(PListe);
  GetPropInfos(PTypeInfo(Control.ClassInfo), PListe);
  for i := 0 to Pred(PropCount) do
     if PListe^[I]^.PropType^.Kind = tkMethod then
        begin
          ControlMethod := GetMethodProp(Control, PListe^[I]);
          if Assigned(ControlMethod.Code) then
             begin
//             PropertyName := PListe^[I]^.PropType^.Name;
//             if PropertyName := 'TKeyEvent'
//                then SetMethodProp(Result, PListe^[I], ControlMethod);*)
             SetMethodProp(Result, PListe^[I], ControlMethod);
             end;
        end;
  Dispose(PListe);

  finally
  Control.Name := WasNamed;
  MemoryStream.Free;
  end;
end;

Procedure TSPVTCtrlGrid.CloneChildControls(Source, Dest: TPanel);
var
i: integer;
NewControl: TControl;
begin
for i:= 0 to Pred(Source.ControlCount) do
    begin
    NewControl := CloneControl(Source.Controls[i], Dest);
    NewControl.Top := Source.Controls[i].top;
    NewControl.Left := Source.Controls[i].Left;
    end;
end;

function TSPVTCtrlGrid.CreateNewRow(PriorRow: Integer = -1): TPanel;
begin
Result := TPanel(CloneControl(FPanel, Self));
With Result do
     begin
     Caption := '';
     Visible := True;
     OnClick := _OnRowClick;
     end;

CloneChildControls(FPanel, Result);

if (PriorRow < 0) or (PriorRow > ControlCount)
   { Append }
   then begin
        Result.SetBounds(1, ControlCount * FPanel.Height + 100, Result.Width, Result.Height);
        FRows.Add(Result);
        end
   { Insert }
   else begin
        Result.SetBounds(1, (PriorRow + 1) * FPanel.Height, Result.Width, Result.Height);
        FRows.Insert(PriorRow, Result);
        end;
end;

function TSPVTCtrlGrid.Append: TPanel;
begin
  if Not FCanInsert then Exit;
  State := cgsAppend;
  Result := CreateNewRow;
  ActiveRowIndex := Pred(RowCount);
  FocusFirstControl;
  if Assigned(FOnNewRecord) then FOnNewRecord(Self);
end;

function TSPVTCtrlGrid.Insert: TPanel;
begin
 if Not FCanInsert then Exit;
 State := cgsInsert;
 Result := CreateNewRow(FActiveRowIndex);
 ActiveRowIndex := FActiveRowIndex;
 if Assigned(FOnNewRecord) then FOnNewRecord(Self);
 FocusFirstControl;
end;

procedure TSPVTCtrlGrid.Delete;
begin
 if FCanDelete and Assigned(FActiveRow) then
    begin
    FImageEditState.Parent := nil;
    FRows.Remove(FActiveRow);
    FActiveRow.Destroy;
    FActiveRow := nil;
    If FRows.Count > 0
       then begin
            FState := cgsOrder;
            if FActiveRowIndex < FRows.Count
               then Inc(FActiveRowIndex);
            Prior;
            end
       else begin
            FActiveRowIndex := -1;
            FBof := True;
            FEof := True;
            end;
    end;
end;

procedure TSPVTCtrlGrid.Exchange(Index1, Index2: Integer);
var
Panel1, Panel2, TempPanel: TPanel;
begin
 TempPanel := TPanel.Create(Self);
 Panel1 := TPanel(FRows[Index1]);
 Panel2 := TPanel(FRows[Index2]);
 While Panel1.ControlCount > 0
       do Panel1.Controls[0].Parent := TempPanel;
 While Panel2.ControlCount > 0
       do Panel2.Controls[0].Parent := Panel1;
 While TempPanel.ControlCount > 0
       do TempPanel.Controls[0].Parent := Panel2;
 TempPanel.Free;
 State := cgsOrder;
end;

{ Navigating }

procedure TSPVTCtrlGrid.First;
begin
 ActiveRowIndex := 0;
 FocusFirstControl;
 State := cgsBrowse;
end;

procedure TSPVTCtrlGrid.Prior;
begin
 ActiveRowIndex := FActiveRowIndex - 1;
 FocusFirstControl;
 State := cgsBrowse;
end;

procedure TSPVTCtrlGrid.Next;
begin
 ActiveRowIndex := FActiveRowIndex + 1;
 FocusFirstControl;
 State := cgsBrowse;
end;

procedure TSPVTCtrlGrid.Last;
begin
 ActiveRowIndex := Pred(RowCount);
 FocusFirstControl;
 State := cgsBrowse;
end;

procedure TSPVTCtrlGrid.MoveUp;
begin
 if Not BOF then
    begin
    Post;
    Exchange(FActiveRowIndex, Pred(FActiveRowIndex));
    ActiveRowIndex := Pred(FActiveRowIndex);
    FocusFirstControl;
    State := cgsBrowse;
    end;
end;

procedure TSPVTCtrlGrid.MoveDown;
begin
 if Not EOF then
    begin
    Post;
    Exchange(FActiveRowIndex, Succ(FActiveRowIndex));
    ActiveRowIndex := Succ(FActiveRowIndex);
    FocusFirstControl;
    State := cgsBrowse;
    end;
end;

procedure TSPVTCtrlGrid.SetFActiveRowIndex(const Value: Integer);
begin
if (Value < 0) or (Value >= RowCount) then Exit;
if (FActiveRowIndex <> Value) or (FState = cgsInsert) or (FState = cgsOrder) then
   begin
   FBof := Value = 0;
   FEof := Value = Pred(RowCount);
   if Assigned(FActiveRow)
     then FActiveRow.Color := FColorUnSelected;
   if Assigned(FBeforeScroll) and (FState <> cgsOrder)
     then BeforeScroll(Self);
   FActiveRowIndex := Value;
   FActiveRow := TPanel(FRows[FActiveRowIndex]);
   FActiveRow.Color := FColorSelected;
   FImageEditState.Parent := FActiveRow;
   {$IFNDEF FPC}
   Self.ScrollInView(FActiveRow);
   {$ENDIF}
   if Assigned(FAfterScroll)
     then AfterScroll(Self);
   end;
end;

procedure TSPVTCtrlGrid._OnRowClick(Sender: TObject);
begin
 ActiveRowIndex := FRows.IndexOf(TPanel(Sender));
end;

procedure TSPVTCtrlGrid.SetFPanel(const Value: TPanel);
begin
  FPanel := Value;
  if FPanel.Parent = Self
     then FPanel.Parent := Self.Parent;

  FPanel.Visible := False;
  FPanel.Align := alTop;
  FColorUnSelected := FPanel.Color;
  FImageEditState.Top := (FPanel.Height - FImageEditState.Height) div 2;
  Clear;
  Append;
end;

procedure TSPVTCtrlGrid.Clear;
begin
While FRows.Count > 0
      do Delete;
end;

function TSPVTCtrlGrid.ControlByName(ControlName: String): TControl;
var
i: integer;
begin
  FControlIndex := -1;
  Result := nil;
  ControlName := Uppercase(ControlName);
  if Not Assigned(ActiveRow) then Exit;
  for i := 0 to Pred(ActiveRow.ControlCount) do
    if AnsiPos(ControlName, Uppercase(ActiveRow.Controls[i].Name)) <> 0 then
       begin
       Result := ActiveRow.Controls[i];
       FControlIndex := i;
       Exit;
       end;
end;

procedure TSPVTCtrlGrid._OnClick(Sender: TObject);
begin
 SetFocus;
end;

procedure TSPVTCtrlGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  {$IFDEF WIN32}
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
  {$ENDIF}
end;

procedure TSPVTCtrlGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
case Key of
    VK_HOME : First;
    VK_UP   : if ((State = cgsInsert) or (State = cgsAppend)) and (FActiveRowIndex > 0)
                 then begin
                      Delete;
                      Key := 0;
                      end
                 else Prior;
    VK_PRIOR: Prior;
    VK_DOWN : Next;
    VK_NEXT : Next;
    VK_END  : Last;
    VK_RETURN : begin
                DispachReturnKey;
                Key := 0;
                end;
    else State := cgsEdit;
    end;
end;

procedure TSPVTCtrlGrid.DispachReturnKey;
var
F: TCustomForm;
NextActiveControl: TWinControl;
ActiveControlName: String;
begin
F := GetParentForm(Self);
ActiveControlName := Uppercase(F.ActiveControl.Name);
if AnsiPos(Uppercase(FPanel.Controls[Pred(FPanel.ControlCount)].Name), ActiveControlName) <> 0
   then begin
        if FActiveRowIndex = Pred(FRows.Count)
           then Append
           else Next
        end
   else begin
        ControlByName(ActiveControlName);
        If FControlIndex > -1 then
           begin
           NextActiveControl := TWinControl(ActiveRow.Controls[FControlIndex + 1]);
           if NextActiveControl.CanFocus
              then NextActiveControl.SetFocus;
           end;
        end;
end;

procedure TSPVTCtrlGrid.FocusFirstControl;
var
FirstControl: TWinControl;
begin
FirstControl := TWinControl(ControlByName(FPanel.Controls[0].Name));
if Not Assigned(FirstControl) then Exit;
{$IFNDEF FPC}
// Bug - Abstract error with D2010
//if FirstControl.Showing and FirstControl.CanFocus
//   then FirstControl.SetFocus;
{$ENDIF}
end;

procedure TSPVTCtrlGrid.SetFState(const Value: TSPVTCtrlGridState);
begin
  FState := Value;
  Case FState of
       cgsBrowse: FImageEditState.Picture.Bitmap := FBmpBrowse;
       cgsInsert..cgsAppend: FImageEditState.Picture.Bitmap := FBmpInsert;
       cgsEdit: FImageEditState.Picture.Bitmap := FBmpEdit;
       end;
end;

function TSPVTCtrlGrid.GetFRowCount: Integer;
begin
 Result := FRows.Count;
end;

function TSPVTCtrlGrid.GetFIsEmpty: Boolean;
begin
 Result := (FRows.Count = 0);
end;

procedure TSPVTCtrlGrid.SetFActiveRow(const Value: TPanel);
var
i: Integer;
begin
 i := FRows.IndexOf(Value);
 if i > -1
    then ActiveRowIndex := i;
end;

procedure TSPVTCtrlGrid.FocusActiveRow(const Value: TObject);
begin
 if TControl(Value).Parent is TPanel
    then ActiveRow := TPanel(TControl(Value).Parent);
end;

procedure TSPVTCtrlGrid.Post;
begin
if Assigned(BeforeScroll)
   then BeforeScroll(Self);
State := cgsBrowse;
end;

{ TSPVTCtrlGridNavigator }

constructor TSPVTCtrlGridNavigator.Create(AOwner:TComponent);
var
i: TSPVTNavButtons;
ResName: String;
begin
inherited Create(AOwner);
Parent := TWinControl(AOwner);
ResName := '';
FCtrlGrid := nil;
FTimer := TTimer.Create(Self);
FTimer.Enabled := True;
FTimer.OnTimer := TimerEvent;
FTimer.Interval := 200;
Caption := '';
FActiveButton := 0;
ShowHint := True;
Height := 22;
FButtonCount := Succ(Ord(High(FButtons)));
for i := Low(FButtons) to High(FButtons) do
    begin
    FButtons[i]:=TSpeedButton.Create(Self);
    With FButtons[i] do
         begin
         Parent:=Self;
         Flat := True;
         NumGlyphs := 1;
         Tag:= Succ(Ord(i));
         Caption := '';
         Hint := SPVTNavButtonName[i];
         NumGlyphs := 2;
         FmtStr(ResName, 'BTN_%s', [SPVTNavButtonName[i]]);
         {$IFDEF FPC}
         Glyph.LoadFromLazarusResource(ResName);
         {$ELSE}
         Glyph.LoadFromResourceName(HInstance, ResName);
         {$ENDIF}
         OnMouseDown := _OnMouseDown;
         OnMouseUp := _OnMouseUp;
         end;
end;
Width := FButtonCount * 22;
{$IFDEF FPC}
_SetSize;//(Width, Height);
{$ENDIF}
end;

destructor TSPVTCtrlGridNavigator.Destroy;
var
  i: TSPVTNavButtons;
begin
  FTimer.Free;
  For i:=Low(FButtons) to High(FButtons) do FButtons[i].Free;
  inherited Destroy;
end;

procedure TSPVTCtrlGridNavigator._OnButtonClick(Btn: Integer);
begin
With FCtrlGrid do
   begin
   Case Btn of
   1: First;
   2: Prior;
   3: Next;
   4: Last;
   5: Insert;
   6: Append;
   7: Delete;
   8: MoveUp;
   9: MoveDown;
   end;
   end;
_RefreshButtonState;
end;

procedure TSPVTCtrlGridNavigator.TimerEvent;
begin
  { Only for scrolling }
  (*Case FActiveButton of
   2..3: DoButtonClick(FActiveButton);
   end;*)
   _RefreshButtonState;
end;


Procedure TSPVTCtrlGridNavigator._RefreshButtonState;
begin
  if csLoading in ComponentState then Exit;
  FButtons[NavFirst].Enabled    := Not CtrlGrid.BOF;
  FButtons[NavPrior].Enabled    := Not CtrlGrid.BOF;
  FButtons[NavMoveUp].Enabled   := Not CtrlGrid.BOF;
  FButtons[NavNext].Enabled     := Not CtrlGrid.EOF;
  FButtons[NavLast].Enabled     := Not CtrlGrid.EOF;
  FButtons[NavMoveDown].Enabled := Not CtrlGrid.EOF;
  FButtons[NavDelete].Enabled   := Not CtrlGrid.IsEmpty;
  FButtons[NavInsert].Enabled   := Not CtrlGrid.IsEmpty;
end;

procedure TSPVTCtrlGridNavigator.WMSize(var Msg: TWMSize);
begin
  inherited;
  { check for minimum size }
  _SetSize;
  Inherited Resize;
  Invalidate;
  Msg.Result := 0;
end;

procedure TSPVTCtrlGridNavigator._SetSize;
var
 i: TSPVTNavButtons;
 ButtonHeight, ButtonWidth, BorderWidth: integer;
begin
 if (csLoading in ComponentState)
    then Exit;

 ButtonWidth := Width div FButtonCount;

 { Ajust panel width }
 Width := (ButtonWidth * FButtonCount) + 1;

 BorderWidth := 1;
 If BevelInner <> bvNone
    then Inc(BorderWidth, BevelWidth);
 ButtonHeight := Height - BorderWidth;

 { Left, Top, Width, Height }
 for i := Low(FButtons) to High(FButtons)
     do FButtons[i].SetBounds((Ord(i)* ButtonWidth) + BorderWidth, 1, ButtonWidth, ButtonHeight);
end;

procedure TSPVTCtrlGridNavigator._OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  FActiveButton := (Sender as TSpeedButton).Tag;
  _OnButtonClick(FActiveButton);
end;

procedure TSPVTCtrlGridNavigator._OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X,Y: Integer);
begin
  FActiveButton := 0;
end;

procedure TSPVTCtrlGridNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FCtrlGrid <> nil) and
    (AComponent = FCtrlGrid) then FCtrlGrid := nil;
end;

{ TSPVTActiveControlWatcher }

procedure TSqlitePassActiveControlWatcher.SetEnabled(const Value: Boolean);
begin
 FEnabled := Value;
end;


procedure TSqlitePassActiveControlWatcher.GetInfos;

  function GetDatasource(Control: TControl):TDatasource;
  begin
  Result := nil;

  if FActiveControl.InheritsFrom(TDBEdit)
     then begin
          Result := TDBEdit(Control).DataSource;
          FActiveDataField := TDBEdit(FActiveControl).Field;
          end
          else
  if FActiveControl.InheritsFrom(TDBGrid)
     then begin
          Result := TDBGrid(Control).DataSource;
          FActiveDataField := TDBGrid(FActiveControl).SelectedField;
          end;
  end;

  function GetDataset(Datasource: TDatasource):TSqlitePassDataset;
  begin
  if FActiveDataSource.Dataset is TSqlitePassDataset
     then Result := TSqlitePassDataset(Datasource.Dataset)
     else Result := nil;
  end;


begin
{ Keep the last selected control }
If Locked then exit;

FActiveDataSource := nil;
FActiveDataset := nil;
FActiveDataField := nil;
FConnected := False;

If Not FEnabled then exit;

FActiveControl := Screen.ActiveControl;
if Assigned(FActiveControl)
   then FDataAware := GetPropInfo(FActiveControl.ClassInfo,'DataSource') <> nil;

if DataAware then
   begin
   FActiveDatasource := GetDatasource(FActiveControl);
   if Assigned(FActiveDataSource)
      then FActiveDataset := GetDataset(FActiveDatasource);
   if Assigned(FActiveDataset)
      then FConnected := FActiveDataset.Active = True
   end;
end;


{ TSqlitePassDBActionList }

constructor TSqlitePassDBActionList.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FActiveControl := TSqlitePassActiveControlWatcher.Create;
FActiveControl.Enabled := True;
FActiveControl.Locked := False;
FDataSourceAuto := False;
end;

destructor TSqlitePassDBActionList.Destroy;
begin
FActiveControl.Free;
Inherited Destroy;
end;

procedure TSqlitePassDBActionList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then DataSource := nil;
end;

procedure TSqlitePassDBActionList.SetDataSource(Value: TDataSource);
var
i: Integer;
begin
  if Value <> FDataSource then
  begin
    FDataSource := Value;
    if Value <> nil then
    begin
    Value.FreeNotification(Self);
    For i := 0 to ActionCount -1 do
      begin
      if Actions[i].inheritsFrom(TSqlitePassDBAction)
         then TSqlitePassDBAction(Actions[i]).DataSource := FDataSource;
      end;
    end;
  end;
end;

function TSqlitePassDBActionList.UpdateAction(Action: TBasicAction): Boolean;
begin
 ActiveControl.GetInfos;
 if FDataSourceAuto and ActiveControl.Connected
    then DataSource := ActiveControl.DataSource;
 Result := inherited UpdateAction(Action);
end;


{ TSqlitePassDBAction }

function TSqlitePassDBAction.CanProcessAction: Boolean;
var
C: TSqlitePassActiveControlWatcher;

begin
Result := False;

if ActionList.InheritsFrom(TSqlitePassDBActionList) then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   Result := C.Connected;
   if Result and Not TSqlitePassDBActionList(ActionList).DataSourceAuto
      then Result := C.DataSource = TSqlitePassDBActionList(ActionList).DataSource;
   end;
end;

procedure TSqlitePassDBAction.UpdateTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
if ActionList.InheritsFrom(TSqlitePassDBActionList)
   then begin
        C := TSqlitePassDBActionList(ActionList).ActiveControl;
        Enabled := C.Connected and ((C.Dataset.RecordCount > 0) Or C.Dataset.Filtered);
        end
   else Enabled := TDataset(GetDataset(Target)).Active;
Inherited UpdateTarget(Target);
end;


{ TSqlitePassDatasetSortAsc }

constructor TSqlitePassDatasetSortAsc.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Caption := SqlitePassRes_DBActionsSortAscCaption;
Hint := SqlitePassRes_DBActionsSortAscHint;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsSortAscShortcut);
end;

procedure TSqlitePassDatasetSortAsc.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
If CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   With C.Dataset do
        begin
        if C.Control is TDBGrid
           then ResetColumnTitles(TDBGrid(C.Control));
        SortedBy := '['+C.DataField.FieldName+'] ASC';
        Sort;
        end;
   end;
end;

{ TSqlitePassDatasetSortDesc }

constructor TSqlitePassDatasetSortDesc.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Caption := SqlitePassRes_DBActionsSortDescCaption;
Hint := SqlitePassRes_DBActionsSortDescHint;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsSortDescShortcut);
end;

procedure TSqlitePassDatasetSortDesc.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
If CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   With C.Dataset do
        begin
        if C.Control is TDBGrid
           then ResetColumnTitles(TDBGrid(C.Control));
        SortedBy := '['+C.DataField.FieldName+'] DESC';
        Sort;
        end;
   end;
end;

{ TSqlitePassDatasetSort }

constructor TSqlitePassDatasetSort.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);
 Caption := SqlitePassRes_DBActionsSortCaption;
 Hint := SqlitePassRes_DBActionsSortHint;
 ShortCut := TextToShortCut(SqlitePassRes_DBActionsSortShortcut);
end;

procedure TSqlitePassDatasetSort.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;
F: TSqlitePassSortedByDialog;
begin
if Not CanProcessAction then Exit;
C := TSqlitePassDBActionList(ActionList).ActiveControl;
F := TSqlitePassSortedByDialog.Create(Self, C.Dataset.SortedBy, C.Dataset.Fields);

Try
  F.ShowModal;
  if F.ModalResult = mrOk then
     begin
     { We have a TSqlitePassDBGrid }
     if C.Control is TDBGrid
        then ResetColumnTitles(TDBGrid(C.Control));
     C.Dataset.Sorted := True;
     C.Dataset.SortedBy := F.SQL;
     C.Dataset.Sort;
     end;
finally
  F.Free;
end;
end;

{ TSqlitePassDBToolButtonFilterOnSelection }

constructor TSqlitePassDatasetFilterOnSelection.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
FFilterText := '';
Caption := SqlitePassRes_DBActionsFilterOnSelectionCaption;
Hint := SqlitePassRes_DBActionsFilterOnSelectionHintOff;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsFilterOnSelectionShortcut);
end;

procedure TSqlitePassDatasetFilterOnSelection.ExecuteTarget(Target: TObject);
var
CanFilter: Boolean;
QuoteStr, FilterValue: String;
C: TSqlitePassActiveControlWatcher;
begin

FFilterText := '';

If Not CanProcessAction then Exit;
C := TSqlitePassDBActionList(ActionList).ActiveControl;

if C.Dataset.Filtered
   then begin
        { Turns off the current filter }
        C.Dataset.Filtered := False;
        CanFilter := Assigned(C.DataField) and Not Checked
        end
   else CanFilter := Assigned(C.DataField) and Not (C.DataField.IsNull or C.Dataset.IsEmpty);

if CanFilter and not Checked
   then begin
        Checked := True;
        Case C.DataField.DataType of
             ftDateTime: FilterValue := C.Dataset.Database.Translator.DateTimeToStr(C.DataField.Value);
             ftDate    : FilterValue := C.Dataset.Database.Translator.DateToStr(C.DataField.Value);
             ftTime    : FilterValue := C.Dataset.Database.Translator.TimeToStr(C.DataField.Value);
             else        FilterValue := C.DataField.AsString;
             End;

        Case C.DataField.DataType of
             ftDateTime, ftDate,
             ftTime
             : QuoteStr := '#';

             ftString, ftWideString,
             ftMemo, ftFmtMemo, ftFloat, ftCurrency, ftBCD
             : QuoteStr := '"';

             else QuoteStr := ''
             end;

        FFilterText :=  '[' + C.DataField.FieldName + '] = ' + QuoteStr + FilterValue + QuoteStr;
        
        if C.Dataset.Filtermode = fmSQLDirect
           then C.Dataset.Filter := ';' + FFilterText
           else C.Dataset.Filter := FFilterText;
        C.Dataset.Filters.FilterByField(C.DataField).Filtered := True;
        C.Dataset.Filtered := True;
        Hint := SqlitePassRes_DBActionsFilterOnSelectionHintOn + FFilterText;
        end
   else begin
        Checked := False;
        C.Dataset.Filtered := False;
        Hint := SqlitePassRes_DBActionsFilterOnSelectionHintOff;
        end;

If Assigned(FFilterOnOff)
   then FFilterOnOff.Checked := Not C.Dataset.filtered;
end;

procedure TSqlitePassDatasetFilterOnSelection.UpdateTarget(
  Target: TObject);
var
IsEnabled: Boolean;
C: TSqlitePassActiveControlWatcher;

begin
IsEnabled := False;
if ActionList.InheritsFrom(TSqlitePassDBActionList)
   then begin
        if Assigned(FFilterOnOff) then
           begin
           C := TSqlitePassDBActionList(ActionList).ActiveControl;
           IsEnabled := C.Connected and ((C.Dataset.RecordCount > 0) Or C.Dataset.Filtered)
                        and ((Not FFilterOnOff.Enabled) or (FFilterOnOff.Enabled and Not FFilterOnOff.Checked));
           end;
        end
   else IsEnabled := TDataset(GetDataset(Target)).Active;

{ Avoid flickering }
if IsEnabled <> Enabled
   then Enabled := IsEnabled;

Checked := Enabled and Checked;
end;


{ TSqlitePassDatasetFilter }

constructor TSqlitePassDatasetFilter.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
FFilterText := '';
Caption := SqlitePassRes_DBActionsAdvancedFilterCaption;
Hint := SqlitePassRes_DBActionsAdvancedFilterHintOff;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsAdvancedFilterShortcut);
end;

procedure TSqlitePassDatasetFilter.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;
begin
FFilterText := '';
Hint := SqlitePassRes_DBActionsAdvancedFilterHintOff;
If CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   SqlitePassFilterDialog := TSqlitePassFilterDialog.Create(Self, C.Dataset);
   if SqlitePassFilterDialog.ShowModal = mrOk then
      begin
      FFilterText := SqlitePassFilterDialog.FilterText;
      Hint := SqlitePassRes_DBActionsAdvancedFilterHintOn + FFilterText;
      If Assigned(FFilterOnOff) then
         begin
         FilterOnOff.Checked := False;
         //FilterOnOff.Execute;
         end;
      end;
   SqlitePassFilterDialog.Free;
   end;
end;


{ TSqlitePassDatasetFilterOnOff }

constructor TSqlitePassDatasetFilterOnOff.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Caption := SqlitePassRes_DBActionsAdvancedFilterOnOffCaption;
Hint := SqlitePassRes_DBActionsAdvancedFilterOnOffHintOff;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsAdvancedFilterOnOffShortcut);
end;

procedure TSqlitePassDatasetFilterOnOff.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
Checked := Not Checked;
Hint := SqlitePassRes_DBActionsAdvancedFilterOnOffHintOff + FFilterEditor.FFilterText;

If CanProcessAction
   then begin
        C := TSqlitePassDBActionList(ActionList).ActiveControl;
        if Checked and Assigned(FFilterEditor) then
           begin
           C.Dataset.Filter := FFilterEditor.FFilterText;
           Hint := SqlitePassRes_DBActionsAdvancedFilterOnOffHintOn + FFilterEditor.FFilterText;
           end;
        C.Dataset.Filtered := Checked
        end
   else Checked := False;

end;

procedure TSqlitePassDatasetFilterOnOff.UpdateTarget(Target: TObject);
var
IsEnabled: Boolean;
C: TSqlitePassActiveControlWatcher;

begin
IsEnabled := False;

if ActionList.InheritsFrom(TSqlitePassDBActionList)
   then begin
        if Assigned(FFilterEditor) then
           begin
           C := TSqlitePassDBActionList(ActionList).ActiveControl;
           IsEnabled := C.Connected and ((C.Dataset.RecordCount > 0) Or C.Dataset.Filtered)
                        and (FFilterEditor.FFilterText <> '');
           end;
        end
   else IsEnabled := TDataset(GetDataset(Target)).Active;

{ Avoid flickering }
if IsEnabled <> Enabled
   then Enabled := IsEnabled;
end;

{ TSqlitePassDatasetLocate }

constructor TSqlitePassDatasetLocate.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Caption := SqlitePassRes_DBActionsLocateRecordCaption;
Hint := SqlitePassRes_DBActionsLocateRecordHint;
ShortCut := TextToShortCut(SqlitePassRes_DBActionsLocateRecordShortcut);
end;

procedure TSqlitePassDatasetLocate.ExecuteTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;
F: TSqlitePassLocateDlg;
begin
if CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   FStatusLabel.Caption := 'No record located';
   F := TSqlitePassLocateDlg.Create(Self, C.Dataset);
   F.ShowModal;
   F.Free;

   If Assigned(C.Dataset) and (C.Dataset.LocateFilters.Results.Count > 0)
      and Assigned(FStatusLabel)
      then FStatusLabel.Caption := IntToStr(C.Dataset.LocateFilters.Results.Count) + ' record(s) located';
   end;
end;


{ TSqlitePassDatasetLocateFirstRecord }

constructor TSqlitePassDatasetLocateFirst.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);

end;

procedure TSqlitePassDatasetLocateFirst.ExecuteTarget(
  Target: TObject);
begin
if CanProcessAction
   then TSqlitePassDBActionList(ActionList).ActiveControl.Dataset.LocateFirst;
end;

{ TSqlitePassDatasetLocatePriorRecord }

constructor TSqlitePassDatasetLocatePrior.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
end;

procedure TSqlitePassDatasetLocatePrior.ExecuteTarget(
  Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
if CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   if C.Dataset.LocateMoveState = grBOF
      then C.Dataset.LocateLast
      else C.Dataset.LocatePrior;
   end;
end;

{ TSqlitePassDatasetLocateNextRecord }

constructor TSqlitePassDatasetLocateNext.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
end;

procedure TSqlitePassDatasetLocateNext.ExecuteTarget(
  Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
if CanProcessAction then
   begin
   C := TSqlitePassDBActionList(ActionList).ActiveControl;
   if C.Dataset.LocateMoveState = grEOF
      then C.Dataset.LocateFirst
      else C.Dataset.LocateNext;
   end;
end;


{ TSqlitePassDatasetLocateLastRecord }

constructor TSqlitePassDatasetLocateLast.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
end;

procedure TSqlitePassDatasetLocateLast.ExecuteTarget(
  Target: TObject);
begin
if CanProcessAction
   then TSqlitePassDBActionList(ActionList).ActiveControl.Dataset.LocateLast;
end;


{ TSqlitePassDbGrid }

procedure SortDbGrid(DbGrid: TdbGrid; Column: TColumn);
var
SortPos: Integer;
SortDirection: String;
Dataset: TSqlitePassDataset;
begin
  if Not (DbGrid.Datasource.Dataset is TSqlitePassDataset)
     then Exit;

  Try
  Dataset := TSqlitePassDataset(DbGrid.Datasource.Dataset);
  Dataset.DisableControls;
  SortPos := AnsiPos(' [>', Column.Title.Caption);
  if SortPos > 0
     then SortDirection := ' DESC'
     else SortDirection := ' ASC';

  ResetColumnTitles(DbGrid);

  if SortDirection = ' ASC'
     then Column.Title.Caption := Column.Title.Caption + ' [>'
     else Column.Title.Caption := Column.Title.Caption + ' <]';

  Column.Title.Font.Color := clRed;
  Dataset.SortedBy := '"' + Column.FieldName + '" ' + SortDirection;
  Dataset.Sort;
  finally
  Dataset.EnableControls;
  end;
end;

procedure ResetColumnTitles(DbGrid: TdbGrid);
var
i: Integer;
begin
  For i := 0 to Pred(DbGrid.Columns.Count) do
      With DbGrid.Columns[i].Title do
           begin
           Font.Color := clBlack;
           Caption := StringReplace(Caption, ' [>', '', []);
           Caption := StringReplace(Caption, ' <]', '', []);
           end;
end;

{ TSqlitePassDBLocateMoveAction }

procedure TSqlitePassDBLocateMoveAction.UpdateTarget(Target: TObject);
var
C: TSqlitePassActiveControlWatcher;

begin
 if ActionList.InheritsFrom(TSqlitePassDBActionList)
   then begin
        C := TSqlitePassDBActionList(ActionList).ActiveControl;
        Enabled := C.Connected and (C.Dataset.RecordCount > 0) and (C.Dataset.LocateFilters.Results.Count > 0);
        end
   else Enabled := TDataset(GetDataset(Target)).Active;
end;


{ TSPVTDBItem }

function TSPVTDBItem.AddChildItem(_Caption: String;
  _ItemType: TSPVTDBItemType; _Font: TFont; _Color: TColor;
  _ImageIndexSelected, _ImageIndex: Integer; _Attached: Boolean;
  _Enabled, _Visible: Boolean): TSPVTDBItem;
begin
Result := Children.AddItem(_Caption,
                 _ItemType,
                 _Font,
                 _Color,
                 _ImageIndexSelected,
                 _ImageIndex,
                 _Attached,
                 _Enabled,
                 _Visible);
Result.FParent := Self;
end;

constructor TSPVTDBItem.Create(_Caption: String;
                       _ItemType:TSPVTDBItemType;
                       _Font: TFont;
                       _Color: TColor;
                       _ImageIndexSelected: Integer;
                       _ImageIndex: Integer;
                       _Attached: Boolean;
                       _Enabled: Boolean;
                       _Visible: Boolean);
begin
Inherited Create;
FChildren := TSPVTDBItemList.Create;
FFont := TFont.Create;
FCaption            := _Caption;
FItemType           := _ItemType;
FColor              := _Color;
FImageIndexSelected := _ImageIndexSelected;
FImageIndex         := _ImageIndex;
FAttached           := _Attached;
FEnabled            := _Enabled;
FVisible            := _Visible;
FFont.Assign(_Font);
end;

destructor TSPVTDBItem.Destroy;
begin
FFont.Free;
FFont := nil;
FChildren.Free;
FChildren := nil;
Inherited Destroy;
end;


function TSPVTDBItem.GetFHasCHildren: Boolean;
begin
 if Assigned(FChildren)
    then Result := FChildren.Count > 0
    else Result := False;
end;

{ TSPVTDBItemList }

constructor TSPVTDBItemList.Create;
begin
inherited Create;
end;

destructor TSPVTDBItemList.Destroy;
begin
ClearAndFreeItems;
inherited Destroy;
end;

procedure TSPVTDBItemList.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

function TSPVTDBItemList.ItemByName(const Value: String): TSPVTDBItem;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSPVTDBItem(inherited Items[I]);
    if AnsiCompareText(Result.Caption, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TSPVTDBItemList.GetItem(Index: Integer): TSPVTDBItem;
begin
Result:=TSPVTDBItem(Inherited Items[Index]);
end;

procedure TSPVTDBItemList.SetItem(Index: Integer;
  const Value: TSPVTDBItem);
begin
Put(Index, Value);
end;

function TSPVTDBItemList.AddItem(_Caption: String;
                       _ItemType:TSPVTDBItemType;
                       _Font: TFont;
                       _Color: TColor;
                       _ImageIndexSelected: Integer;
                       _ImageIndex: Integer;
                       _Attached: Boolean;
                       _Enabled: Boolean;
                       _Visible: Boolean): TSPVTDBItem;
 begin
 Result := TSPVTDBItem.Create(_Caption,
                               _ItemType,
                               _Font,
                               _Color,
                               _ImageIndexSelected,
                               _ImageIndex,
                               _Attached,
                               _Enabled,
                               _Visible);
 Inherited Add(Result);
end;


{ TSPVTDBTreeView }

constructor TSPVTDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLeftMargin:= 2;
  FTempImage := TImage.Create(Self);
  FTempImage.Transparent := True;
  FTempImage.AutoSize := False;
  FDbItems := TSPVTDBItemList.Create;
  FVisibleItems := [itTable, itQuery, itView, itTrigger, itTableSystem,
                   itSql, itSqlSelect, itSqlCreate, itSqlCreateTable, itSqlUpdate,
                   itSqlInsert, itSqlDelete, itSqlOthers];
  FVisibleDBItems := TSPVTDBItemList.Create;
  DefaultRowHeight := 18;
  {$IFDEF FPC}
  ColCount := 2;
  RowCount := 2;
  Options := [goRowSelect,goAlwaysShowEditor,goSmoothScroll];
  { Lazarus fcl bug - Cannot set FixedRows := 0 -> Freeze }
  FixedCols := 1;
  FixedRows := 1;
  {$ELSE}
  ColCount := 2;
  RowCount := 2;
  Options := Options + [goRowSelect];
  Options := Options - [goVertLine, goHorzLine];
  FixedCols := 0;
  FixedRows := 1; // DU to fcl bug we add a title...in order to be consistent
  {$ENDIF}

  if Assigned(Owner) and (Owner is TPanel) then
     begin
     ColWidths[0] := 0;
     ColWidths[1] := TPanel(Owner).Width;
     Align := AlClient;
     end;
  OnDrawCell := Self.DrawDatabaseItems;
end;

destructor TSPVTDBTreeView.Destroy;
begin
  FDatabase := nil;
  FImageList := nil;
  FTempImage.Free;
  FDbItems.Free;
  { Clear to avoid items to be fred twice }
  FVisibleDBItems.Clear;
  FVisibleDBItems.Free;
  inherited Destroy;
end;


procedure TSPVTDBTreeView.DrawDatabaseItems(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
              aState:TGridDrawState);

  procedure DisplayDbItem(DbItem: TSPVTDBItem);
  var
  L, T, TextWidth, TextHeight: Integer;
  begin
  TextWidth := Canvas.TextWidth(DbItem.Caption);
  TextHeight := Canvas.TextHeight(DbItem.Caption);
  L := (FLeftMargin * (DbItem.FNestedLevel));
  T := ARect.Top + ((RowHeights[ARow] - TextHeight) Div 2);
  With Canvas do
       begin

       { Draw Background }
       if ARow = FSelectedIndex
          then Brush.Color := $00E2E2E2
          else Brush.Color := DbItem.Color;
       FillRect(ARect);

       { Draw Caption }
       Font := DbItem.Font;
       TextRect(ARect, L + (FTempImage.Width + FLeftMargin) * (3 + DbItem.FNestedLevel) - FLeftMargin, T, DbItem.Caption);

       Dec(T, 2);
       { Draw Selection Image }
       if (ARow = FSelectedIndex) and Assigned(FIMageList) then
          begin
          FTempImage.Picture := nil;
          FImageList.GetBitmap(DbItem.ImageIndexSelected,FTempImage.Picture.Bitmap);
          Draw(FLeftMargin, T+1, FTempImage.Picture.Bitmap);
          end;

       Inc(L, (FTempImage.Width * DbItem.FNestedLevel) + FLeftMargin);
       { Draw + or - sign }
       if DbItem.HasChildren then
          begin
          FTempImage.Picture := nil;
          if DbItem.Expanded
             then FImageList.GetBitmap(MinusImageIndex,FTempImage.Picture.Bitmap)
             else FImageList.GetBitmap(PlusImageIndex,FTempImage.Picture.Bitmap);
          Draw(L, T, FTempImage.Picture.Bitmap);
          end;

       { Draw Item Image }
       FTempImage.Picture := nil;
       FImageList.GetBitmap(DbItem.ImageIndex,FTempImage.Picture.Bitmap);
       Inc(L, FTempImage.Width + FLeftMargin);
       Draw(L, T, FTempImage.Picture.Bitmap);

       { Draw Attached Image }
       if DbItem.Attached then
          begin
          FTempImage.Picture := nil;
          FImageList.GetBitmap(AttachedItemImageIndex,FTempImage.Picture.Bitmap);
          Inc(L, FTempImage.Width);
          Draw(L, T, FTempImage.Picture.Bitmap);
          end;

       end;
 end;

begin
if ACol <> 1 then Exit;
if (ARow > 0) and (ARow <= FVisibleDBItems.Count)
   then DisplayDbItem(FVisibleDBItems[ARow-1]);
end;


function TSPVTDBTreeView.GetFSelectedItem: TSPVTDBItem;
begin
 If (FSelectedIndex > 0) and (FSelectedIndex <= FVisibleDBItems.Count)
    then Result := FVisibleDBItems[FSelectedIndex-1]
    else Result := nil;
end;


procedure TSPVTDBTreeView.RefreshVisibleItems(FullExpand: Boolean = False);
var
Level: Integer;

  procedure RecurseRefreshVisibleItems(Items: TSPVTDBItemList);
  var
  i: Integer;
  begin
  for i:=0 to Pred(Items.Count) do
    begin
    Items[i].FNestedLevel := Level;
    FVisibleDBItems.Add(Pointer(Items[i]));
    If FullExpand
       then Items[i].FExpanded := True;
    if Items[i].HasChildren and Items[i].Expanded
       then begin
            Inc(Level);
            RecurseRefreshVisibleItems(Items[i].Children);
            Dec(Level);
            end;
    end;
  end;

begin
Level := 1;
FVisibleDBItems.Clear;
RecurseRefreshVisibleItems(FDBItems);
RowCount := FVisibleDBItems.Count+1;
end;

procedure TSPVTDBTreeView.RefreshItems;
var
i: Integer;
DatabaseItem, NewItem,
SQLItem, SQLSelectItem, SQLCreateItem, SQLCreateTableItem,
SQLInsertItem, SQLUpdateItem, SQLDeleteItem,
SQLOthersItem : TSPVTDBItem;

  function GetItemImageIndex(Attached: Boolean): Integer;
  begin
  If Attached
     then Result := AttachedItemImageIndex
     else Result := TableItemImageIndex;
  end;

  function GetSQLItem(var Item: TSPVTDBItem; Caption: String; ItemType: TSPVTDBItemType): TSPVTDBItem;
  begin
  if Not Assigned(Item)
     then Result := NewItem.AddChildItem(Caption,ItemType,Font,cl3DLight,SelectedImageIndex,QueryImageIndex, False, True,True)
     else Result := Item;
  end;


begin
SQLItem := nil;
SQLSelectItem := nil;
SQLCreateItem := nil;
SQLCreateTableItem := nil;
SQLInsertItem := nil;
SQLUpdateItem := nil;
SQLDeleteItem := nil;
SQLOthersItem := nil;

Try
FDbItems.ClearAndFreeItems;

if Not FDatabase.Connected then
   begin
   Visible := False;
   Exit;
   end
   else Visible := True;


{ Database }
DatabaseItem := FDBItems.AddItem('Database',itDatabase,Font,clBtnface,SelectedImageIndex,DatabaseImageIndex,False, True,True);
DatabaseItem.Font.Color := clBlack;
DatabaseItem.Font.Style := [fsBold];
DatabaseItem.Color := clBtnface;

{ Tables }
if (itTable in FVisibleItems) then
   begin
   NewItem := DatabaseItem.AddChildItem('Tables',itTable,Font,cl3DLight,SelectedImageIndex,TableImageIndex,False, True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.TableDefs.Count)
       do if Not FDatabase.IsSystemTable(FDatabase.TableDefs[i].TableName)
             then NewItem.AddChildItem(FDatabase.TableDefs[i].TableFullName,itTable,Font,clWhite,SelectedImageIndex,TableItemImageIndex,FDatabase.TableDefs[i].Attached,True,True);
   end;

{ Queries }
if (itQuery in FVisibleItems) then
   begin
   NewItem := DatabaseItem.AddChildItem('Queries',itQuery,Font,cl3DLight,SelectedImageIndex,QueryImageIndex,False,True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.QueryDefs.Count)
       do NewItem.AddChildItem(FDatabase.QueryDefs[i].QueryFullName,itQuery,Font,clWhite,SelectedImageIndex,QueryItemImageIndex,FDatabase.QueryDefs[i].Attached,True,True);
   end;

{ Views }
if (itView in FVisibleItems) then
   begin
   NewItem := DatabaseItem.AddChildItem('Views',itView,Font,cl3DLight,SelectedImageIndex,WiewImageIndex,False, True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.Views.Count)
      do NewItem.AddChildItem(FDatabase.Views[i].ViewFullName,itView,Font,clWhite,SelectedImageIndex,QueryItemImageIndex,FDatabase.Views[i].Attached, True,True);
   end;

{ Triggers }
if (itTrigger in FVisibleItems) then
   begin
   NewItem := DatabaseItem.AddChildItem('Triggers',itTrigger,Font,cl3DLight,SelectedImageIndex,TriggerImageIndex,False,True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.Triggers.Count)
      do NewItem.AddChildItem(FDatabase.Triggers[i].TriggerFullName,itTrigger,Font,clWhite,SelectedImageIndex,TableItemImageIndex,FDatabase.Triggers[i].Attached,True,True);
   end;

{ System Tables }
if (itTableSystem in FVisibleItems) then
   begin
   NewItem := DatabaseItem.AddChildItem('Tables (System)',itTableSystem,Font,cl3DLight,SelectedImageIndex,SystemTableImageIndex,False, True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.TableDefs.Count)
       do if FDatabase.IsSystemTable(FDatabase.TableDefs[i].TableName)
             then NewItem.AddChildItem(FDatabase.TableDefs[i].TableFullName,itTableSystem,Font,clWhite,SelectedImageIndex,TableItemImageIndex,FDatabase.TableDefs[i].Attached, True,True);
   end;

{ SQL Statements }
if FDatabase.SQLStmtDefs.Count > 0 then
   begin
   NewItem := DatabaseItem.AddChildItem('SQL Statements)',itSQL,Font,cl3DLight,SelectedImageIndex,SQLImageIndex,False,True,True);
   NewItem.Font.Color := clNavy;
   NewItem.Font.Style := [fsBold];
   NewItem.Color := clBtnface;

   for i:= 0 to Pred(FDatabase.SQLStmtDefs.Count) do
       begin
       case FDatabase.SQLStmtDefs[i].SQLType of
            stSelect     : if (itSqlSelect in FVisibleItems) then
                           begin
                           SQLSelectItem := GetSQLItem(SQLSelectItem, '(Select)', itSQLSelect);
                           SQLSelectItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLSelectItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached, True,True);
                           end;
            stCreate     : if (itSqlCreate in FVisibleItems) then
                           begin
                           SQLCreateItem := GetSQLItem(SQLCreateItem, '(Create)', itSQLCreate);
                           SQLCreateItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLCreateItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached, True,True);
                           end;
            stCreateTable: if (itSqlCreateTable in FVisibleItems) then
                           begin
                           SQLCreateTableItem := GetSQLItem(SQLCreateTableItem, '(Create Table)', itSQLCreateTable);
                           SQLCreateTableItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLCreateTableItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached,True,True);
                           end;
            stInsert     : if (itSqlInsert in FVisibleItems) then
                           begin
                           SQLInsertItem := GetSQLItem(SQLInsertItem, '(Insert', itSQLInsert);
                           SQLInsertItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLInsertItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached,True,True);
                           end;
            stUpdate     : if (itSqlUpdate in FVisibleItems) then
                           begin
                           SQLUpdateItem := GetSQLItem(SQLUpdateItem, '(Update)', itSQLUpdate);
                           SQLUpdateItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLUpdateItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached,True,True);
                           end;
            stDelete     : if (itSqlDelete in FVisibleItems) then
                           begin
                           SQLDeleteItem := GetSQLItem(SQLDeleteItem, '(Delete)', itSQLDelete);
                           SQLDeleteItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLDeleteItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached,True,True);
                           end;
            else           if (itSqlOthers in FVisibleItems) then
                           begin
                           SQLOthersItem := GetSQLItem(SQLOthersItem, '(Others)', itSQLOthers);
                           SQLOthersItem.AddChildItem(FDatabase.SQLStmtDefs[i].SQLStmtName,SQLOthersItem.ItemType,Font,clWhite,SelectedImageIndex,SQLItemImageIndex,FDatabase.SQLStmtDefs[i].Attached,True,True);
                           end;
            end; { Case }
       end; { For SQL Stmts }
     end;

finally
RefreshVisibleItems(True); { True = Full Expand }
ColWidths[1] := Parent.Width;
//if CanFocus then SetFocus;
end;
end;

procedure TSPVTDBTreeView.SelectByName(ItemName: String; ItemType: TSPVTDBItemTypes);
var
i: Integer;
begin
if ItemName = '' then Exit;
For i := 0 to Pred(FVisibleDBItems.Count) do
   if (FVisibleDBItems[i].FCaption = ItemName)
      and (FVisibleDBItems[i].FItemType in ItemType) then
      begin
      FSelectedIndex := i+1;
      Break;
      end;
end;

function TSPVTDBTreeView.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if Assigned(FVisibleDBItems) and (ARow <= FVisibleDBItems.Count)
     then begin
          FSelectedIndex := ARow;
          Result := Inherited SelectCell(ACol, ARow);
          end
     else begin
          FSelectedIndex := -1;
          Result := False;
          end;
end;

procedure TSPVTDBTreeView.SetFImageList(const Value: TImageList);
begin
  FImageList := Value;
  if Assigned(FImageList) then
     begin
     FTempImage.Width := FImageList.Width;
     FTempImage.Height := FImageList.Height;
     end;
end;

procedure TSPVTDBTreeView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
inherited MouseDown(Button, Shift, X, Y);
if (FSelectedIndex > 0) and (FSelectedIndex <= FVisibleDBItems.Count)
    then FVisibleDBItems[FSelectedIndex-1].Expanded := not FVisibleDBItems[FSelectedIndex-1].Expanded;
if Button = mbLeft
   then RefreshVisibleItems;
end;

procedure DisplayText(Grid: TDrawGrid;
                      Col, Row: Integer;
                      Rect: TRect;
                      Text: String;
                      Font: TFont;
                      HAlignment: TAlignment; VAlignment: TSPVTVAlignment;
                      Margins: TRect;
                      Autosize: Boolean = True);
var
Left, Top, ColWidth, TextWidth, TextHeight: Integer;
begin
ColWidth := Grid.ColWidths[Col];
TextWidth := Grid.Canvas.TextWidth(Text);
TextHeight := Grid.Canvas.TextHeight(Text);
if Autosize and (Grid.ColWidths[Col] < TextWidth)
   then Grid.ColWidths[Col] := TextWidth + (Margins.Left * 4);

Case HAlignment of
     taLeftJustify : Left := Rect.Left + Margins.Left;
     taRightJustify: Left := Rect.Left + ColWidth-TextWidth-Margins.Left;
     taCenter      : Left := Rect.Left + ((ColWidth-TextWidth) Div 2);
     end;

Case VAlignment of
     vaTop   : Top := 0;
     vaCenter: Top := Rect.Top + ((Grid.RowHeights[Row] - TextHeight) Div 2);
     vaBottom: Top := Rect.Bottom - TextHeight;
     end;

Grid.Canvas.Font := Font;
Grid.Canvas.TextRect(Rect, Left, Top, Text);
end;

procedure DisplayImage(Grid: TDrawGrid;
                       Col, Row: Integer;
                       Rect: TRect;
                       ImageList: TImageList;
                       ImageIndex: Integer;
                       HAlignment: TAlignment; VAlignment: TSPVTVAlignment;
                       Margins: TRect);
var
Left, Top, ColWidth, ImageWidth, ImageHeight: Integer;
begin
ColWidth := Grid.ColWidths[Col];
ImageWidth := ImageList.Width;
ImageHeight := ImageList.Height;

Case HAlignment of
     taLeftJustify : Left := Rect.Left + Margins.Left;
     taRightJustify: Left := Rect.Left + ColWidth-ImageWidth-Margins.Left;
     taCenter      : Left := Rect.Left + ((ColWidth-ImageWidth) Div 2);
     end;

Case VAlignment of
     vaTop   : Top := 0;
     vaCenter: Top := Rect.Top + ((Grid.RowHeights[Row] - ImageHeight) Div 2)+1;
     vaBottom: Top := Rect.Bottom - ImageHeight;
     end;

 With Grid.Canvas do
      begin
      Brush.Color := $00E2E2E2;
      FillRect(Rect);
      end;
 ImageList.Draw(Grid.Canvas, Left, Top, ImageIndex);
end;


initialization
 {$IFDEF FPC}
  {$I SqlitePassVisualTools.lrs}
 {$ENDIF}

end.
 
