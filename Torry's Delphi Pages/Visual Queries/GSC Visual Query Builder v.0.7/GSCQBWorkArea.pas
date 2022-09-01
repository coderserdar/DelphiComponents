{*******************************************************************************
* GSC Query Builder v0.7 - Copyright(C) 2002 by GSC.                           *
*------------------------------------------------------------------------------*
* With this component you're able to add a visual query builder to your        *
* application.                                                                 *
* Full database platfrom independent, table names and fields are retrived via  *
* TableList property and OnGetTableFields event.                               *
* Supports only SQL Select command                                             *
* Free for any (even commercial) project, but if you use my component, please  *
* put a small text into the about box of the program with the URL to my        *
* website. Thanks!                                                             * 
*------------------------------------------------------------------------------*
* The component is based on the Open Query Builder 4.0a from Sergey Orlik      *
* Unfortunatelly I can't get in contact with the original author so I couldn't *
* ask him about the status of his component but as I can see the website of OQB*
* was not updated since 1999.                                                  *
* Also he wrote in the readme.1st file of his package:                         *
* "I can't provide the technical support for these tools, but you have all     *
*  source code and you can modify it without restriction for any non-commercial*
*  projects."                                                                  *
* If you are using this component, please agree with his wish too.             *
*------------------------------------------------------------------------------*
* I rewrote his component's to support Delphi 5 and cleaned up the code,       *
* but there are still code to clean so it is possible that eg. property names  *
* may change in future releases.                                               *
*------------------------------------------------------------------------------*
* Comming up:                                                                  *
*  v0.8: Fixes and cleans in TGSCQBGrid                                        *
*  v0.9: Fixes and cleans in TGSCQBWorkArea                                    *
*  v1.0: Fixes and modifications to get a stable component                     *
*  v2.0: If I or somebody will be able to write a complex SQL parser, the      *
*        component will be able to parse an SQL SELECT command and build it    *
*        visually.                                                             *
*------------------------------------------------------------------------------*
* History:                                                                     *
*  v0.7: This release mainly fixes and cleans TGSCQBTable                      *
*        * TableName, TableAlias and Selected are now public properties        *
*        - Removed SetupForm method                                            *
*        * The caption of the TGSCQBTable window is now painted gradient with  *
*          the Windows GDI function GradientFill                               *
*        + Added separate OnChange event to TGSCQBGrid                         *
*        * Redesigned TGSCQBGrid: instead of popup menus, you can select       *
*          sort options, sort orders, etc. from a drop down box like in TDBGrid*
*        * Fixed error witch caused an access violation while linking two      *
*          fields.                                                             *
*        * Fixed error witch caused an access violation while calling OnChange *
*          after you closed the link properties window.                        *
*        * Some minor bugfixes                                                 *
*  v0.6: This release mainly fixes and cleans TGSCQBLink                       *
*        * Replaced join bitmaps with better ones.                             *
*        - Removed InternalClick, ControlAtPos and WndProc from TGSCQBLink     *
*        + Added MouseDown override to TGSCQBLink                              *
*        + Added Selected property to TGSCQBLink                               *
*        + Added SetupLink to TGSCQBLink witch shows a dialog to setup the link*
*        + Added Unlink method to TGSCQBLink to free the link                  *
*        + Added OnLinkMenuPopup and OnTableMenuPopup to be able to provide    *
*          a custom menu for these objects                                     *
*        * Replaced TGSCQBTable with a new component witch is a TForm desceant.*
*          This enables you to resize a table's form.                          *
*        * Rewritten SQL command generation code witch is more effective if you*
*          place other controls on TGSCQBWorkArea.                             *
*        * Renamed: TGSCQBWorkArea.FindTable      -> TableByName               *
*                   TGSCQBWorkArea.FindTableAlias -> TableByAlias              *
*        * Fixed bug in FindLink witch allowed to insert an other link between *
*          two already linked fields                                           *
*        - Removed FindOtherLink in TGSCQBWorkArea                             *
*        + Added two new overload method for FindLink in TGSCQBWorkArea and    *
*          changed their parameter lists.                                      *
*        + Added new Tables, TableCount, Links and LinkCount properties to     *
*          TGSCQBWorkArea                                                      *
*        + Added new SelectedItem read-only property to TGSCQBWorkArea         *  
*        * Changed a bit the parameter names in event handlers                 *
*  v0.5: First release.                                                        *
*------------------------------------------------------------------------------*
* Please remember that this component is not able to parse SQL commands.       *
* If you have a good open source SQL parser or you know an URL where it can be *
* located please let me know!                                                  *
* Feel you free to modify the code but please send me back any modifications   *
* to info@gsc.hu and mark modified blocks with                                 *
*        "//Your (nick)name"                                                   *
*------------------------------------------------------------------------------*
* Contact:                                                                     *
*   Web: http://www.gsc.hu                                                     *
*   E-mail: info@gsc.hu (Subject: GSCQBuilder)                                 *
********************************************************************************}
unit GSCQBWorkArea;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CheckLst, Buttons, StdCtrls, ExtCtrls, Menus, Grids, GSCQBConst;

const
  // Link option constants
  loEqual      = 0;
  loSmaller    = 1;
  loLarger     = 2;
  loSmallerEq  = 3;
  loLargerEq   = 4;
  loNotEqual   = 5;

  // Link type constants
  ltInner      = 0;
  ltLeftOuter  = 1;
  ltRightOuter = 2;
  ltFullOuter  = 3;

  EqualStr : array[loEqual..loNotEqual] of string[2] =
             ('=','<','>','<=','>=','<>');

type
  EGSCQBException = class(Exception);

  TGetTableFieldsEvent = procedure(Sender : TObject; const TableName : string; Fields : TStrings) of object;
  TAddTableEvent = procedure(Sender : TObject; const TableName : string) of object;
  TAddLinkEvent = procedure(Sender : TObject; const Table1, Table2, Field1, Field2 : string) of object;

  TGSCQBWorkArea = class;
  TGSCQBGrid = class;

  TGSCQBTable = class(TForm)
    FieldList: TCheckListBox;
    UnLinkBtn: TSpeedButton;
    CloseBtn: TSpeedButton;
    TableMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    miSelectNone: TMenuItem;
    N1: TMenuItem;
    miSetTableAlias: TMenuItem;
    N2: TMenuItem;
    miUnLink: TMenuItem;
    miClose: TMenuItem;
    procedure CaptionPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action : TCloseAction);
    procedure CloseBtnClick(Sender: TObject);
    procedure UnLinkBtnClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure UnSelectAllClick(Sender: TObject);
    procedure SetTableAliasClick(Sender : TObject);
    procedure FieldListClickCheck(Sender: TObject);
    procedure FieldListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FieldListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FieldListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TableMenuPopup(Sender: TObject);
  private
    { Private declarations }
    FTableName : string;
    FTableAlias : string;
    FSelected : boolean;
    procedure SetSelected(const Value: boolean);
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure SetTableAlias(const Value: string);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    { Public declarations }
    property TableAlias : string read FTableAlias write SetTableAlias;
    property TableName : string read FTableName;
    property Selected : boolean read FSelected write SetSelected;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint;override;
    procedure CheckField(const AFieldName : string; Check : boolean = true);
    function  GetRowY(FieldNo: integer) : integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TGSCQBLink = class(TShape)
  private
    FTable1,
    FTable2 : TGSCQBTable;
    FFieldNo1,
    FFieldNo2 : integer;
    FFieldName1,
    FFieldName2 : string;
    FLinkOpt,
    FLinkType : integer;
    FLinkX,
    FLinkY : byte;
    FRegion  : THandle;
    FSelected: boolean;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure LinkOptionsClick(Sender : TObject);
    procedure UnlinkClick(Sender : TObject);
    procedure AllFromTable1Click(Sender : TObject);
    procedure AllFromTable2Click(Sender : TObject);
    procedure SetFieldName1(const Value : string);
    procedure SetFieldName2(const Value : string);
    procedure UpdateHint;
    procedure SetLinkType(const Value: integer);
    procedure SetFieldNo1(const Value: integer);
    procedure SetFieldNo2(const Value: integer);
    procedure SetSelected(const Value: boolean);
    procedure SetLinkOptions(const Value: integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    property Table1 : TGSCQBTable read FTable1;
    property Table2 : TGSCQBTable read FTable2;
    property FieldNo1 : integer read FFieldNo1 write SetFieldNo1;
    property FieldNo2 : integer read FFieldNo2 write SetFieldNo2;
    property FieldName1 : string read FFieldName1 write SetFieldName1;
    property FieldName2 : string read FFieldName2 write SetFieldName2;
    property LinkOptions : integer read FLinkOpt write SetLinkOptions;
    property LinkType : integer read FLinkType write SetLinkType;
    property Selected : boolean read FSelected write SetSelected;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Rebound;
    procedure SetupLink;
    procedure Unlink;
  end;

  TGSCQBWorkArea = class(TScrollBox)
  private
    { Private declarations }
    FTableList : TListBox;
    FFieldGrid : TGSCQBGrid;
    FOnGetTableFields : TGetTableFieldsEvent;
    FOnAddTable : TAddTableEvent;
    FOnRemoveTable : TAddTableEvent;
    FOnAddLink : TAddLinkEvent;
    FOnRemoveLink : TAddLinkEvent;
    FTables : TList;
    FLinks : TList;
    FOnLinkPopupMenu: TContextPopupEvent;
    FOnTablePopupMenu: TContextPopupEvent;
    FOptionsClause: string;
    FClosingClause: string;
    FOnChange: TNotifyEvent;
    FSelectedItem : TControl;
    FNoChangeEvent : boolean;
    procedure SetFieldGrid(Value : TGSCQBGrid);
    function GetSQL: TStrings;
    function GetTable(Index: integer): TGSCQBTable;
    function GetLink(Index: integer): TGSCQBLink;
    function GetTableCount: integer;
    function GetLinkCount: integer;
    procedure SetClosingClause(const Value: string);
    procedure SetOptionsClause(const Value: string);
  protected
    { Protected declarations }
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddTableForm(ATable : TGSCQBTable);
    procedure RemoveTableForm(ATable : TGSCQBTable);
    procedure AddLinkObject(ALink : TGSCQBLink);
    procedure RemoveLinkObject(ALink : TGSCQBLink);
    procedure DoChange; dynamic;
  public
    { Public declarations }
    property SQL : TStrings read GetSQL;
    property Tables[Index : integer] : TGSCQBTable read GetTable;
    property TableCount : integer read GetTableCount;
    property Links[Index : integer] : TGSCQBLink read GetLink;
    property LinkCount : integer read GetLinkCount;
    property SelectedItem : TControl read FSelectedItem;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    procedure DragDrop(Source: TObject; X, Y: Integer);override;
    function InsertTable(X, Y: Integer; const ATableName : string = '') : TGSCQBTable;
    function InsertLink(ATable1, ATable2: TGSCQBTable; AFieldNo1, AFieldNo2: Integer) : TGSCQBLink;
    function TableByName(const TableName : string) : TGSCQBTable;
    function TableByAlias(const Alias : string) : TGSCQBTable;
    function FindLink(const ATable1, ATable2 : TGSCQBTable; StartWith : integer = 0) : TGSCQBLink; overload;
    function FindLink(const ATable1, ATable2 : TGSCQBTable; AFieldNo1, AFieldNo2 : integer; StartWith : integer = 0) : TGSCQBLink; overload;
    procedure ReboundLinksForTable(ATable : TGSCQBTable);
    procedure UnlinkTable(ATable : TGSCQBTable);
    procedure ClearSelected;
  published
    { Published declarations }
    property ClosingClause : string read FClosingClause write SetClosingClause;
    property FieldGrid : TGSCQBGrid read FFieldGrid write SetFieldGrid;
    property OptionsClause : string read FOptionsClause write SetOptionsClause;
    property TableList : TListBox read FTableList write FTableList;
    property OnGetTableFields : TGetTableFieldsEvent read FOnGetTableFields write FOnGetTableFields;
    property OnAddTable : TAddTableEvent read FOnAddTable write FOnAddTable;
    property OnRemoveTable : TAddTableEvent read FOnRemoveTable write FOnRemoveTable;
    property OnAddLink : TAddLinkEvent read FOnAddLink write FOnAddLink;
    property OnRemoveLink : TAddLinkEvent read FOnRemoveLink write FOnRemoveLink;
    property OnLinkPopupMenu : TContextPopupEvent read FOnLinkPopupMenu write FOnLinkPopupMenu;
    property OnTablePopupMenu : TContextPopupEvent read FOnTablePopupMenu write FOnTablePopupMenu;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TGSCQBGrid = class(TStringGrid)
  private
    FWorkArea : TGSCQBWorkArea;
    FCheckedBmp : TBitmap;
    FUnCheckedBmp : TBitmap;
    FUseGroupBy : boolean;
    FOnChange: TNotifyEvent;
    procedure SetWorkArea(Value : TGSCQBWorkArea);
    function GetIsEmpty: boolean;
    procedure SetUseGroupBy(const Value: boolean);
    function GetDataCol(Index : Integer): TStrings;
    function GetAliases(Index: integer): string;
    function GetColNames(Index: integer): string;
    function GetCriterias(Index: integer): string;
    function GetGroupFuncs(Index: integer): string;
    function GetOutputStates(Index: integer): string;
    function GetSortTypes(Index: integer): string;
    function GetSortOrders(Index: integer): string;
    function GetTableNames(Index: integer): string;
    procedure SetAliases(Index: integer; const Value: string);
    procedure SetColNames(Index: integer; const Value: string);
    procedure SetCriterias(Index: integer; const Value: string);
    procedure SetGroupFuncs(Index: integer; const Value: string);
    procedure SetOutputStates(Index: integer; const Value: string);
    procedure SetSortTypes(Index: integer; const Value: string);
    procedure SetSortOrders(Index: integer; const Value: string);
    procedure SetTableNames(Index: integer; const Value: string);
  protected
    property InternalColNames : TStrings index CColumn read GetDataCol;
    property InternalAliases : TStrings index CAlias read GetDataCol;
    property InternalTableNames : TStrings index CTable read GetDataCol;
    property InternalOutputStates : TStrings index CShow read GetDataCol;
    property InternalSortTypes : TStrings index CSortType read GetDataCol;
    property InternalSortOrders : TStrings index CSortOrder read GetDataCol;
    property InternalGroupFuncs : TStrings index CGroupBy read GetDataCol;
    property InternalCriterias : TStrings index CCriteria read GetDataCol;
    procedure DragOver(Source : TObject; X, Y:integer; State : TDragState; var Accept : boolean);override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key : Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetMaxOrderID : integer; virtual;
    function GetRowWithOrderID(OrderID : integer) : integer; virtual;
    procedure DecOrderIDsByOne(FromID : integer);
    procedure IncOrderIDsByOne(FromID : integer);
    procedure DoChange;
    function CreateEditor: TInplaceEdit; override;
  public
    property IsEmpty : boolean read GetIsEmpty;
    property ColNames[Index : integer] : string read GetColNames write SetColNames;
    property Aliases[Index : integer] : string read GetAliases write SetAliases;
    property TableNames[Index : integer] : string read GetTableNames write SetTableNames;
    property OutputStates[Index : integer] : string read GetOutputStates write SetOutputStates;
    property SortTypes[Index : integer] : string read GetSortTypes write SetSortTypes;
    property SortOrders[Index : integer] : string read GetSortOrders write SetSortOrders;
    property GroupFuncs[Index : integer] : string read GetGroupFuncs write SetGroupFuncs;
    property Criterias[Index : integer] : string read GetCriterias write SetCriterias;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    function FindColumn(const AColumn, ATableName : string) : integer; virtual;
    procedure Insert(const AColumn, ATableName : string); virtual;
    procedure RemoveColumn(const AColumn, ATableName : string); virtual;
    procedure RemoveColumnsForTable(const ATableName : string); virtual;
    procedure DragDrop(Source : TObject; X, Y : integer);override;
  published
    property WorkArea : TGSCQBWorkArea read FWorkArea write SetWorkArea;
    property UseGroupBy : boolean read FUseGroupby write SetUseGroupBy;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

uses GSCQBLinkProp;

{$R *.dcr}
{$R buttons.res}
{$R GSCQBTableForm.dfm}

type
// The _TRIVERTEX structure is declared wrong in Windows.pas because the COLOR16 type
// is declared as ShortInt instead of Word

  GSCCOLOR16 = WORD;
  GSCTRIVERTEX = packed record
    x: Longint;
    y: Longint;
    Red: GSCCOLOR16;
    Green: GSCCOLOR16;
    Blue: GSCCOLOR16;
    Alpha: GSCCOLOR16;
  end;

// We have to redefine the import function to apply the new parameter type
function GradientFill(DC: HDC; var p2: GSCTriVertex; p3: ULONG; p4: Pointer; p5, p6: ULONG): BOOL; stdcall; external msimg32 name 'GradientFill';

const
  clGradientActiveCaption = TColor(COLOR_GRADIENTACTIVECAPTION or $80000000);
  clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or $80000000);

type
  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TEditStyle = (esSimple, esPickList);

  TGSCQBInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
//    FDataList: TDBLookupListBox;
    FPickList: TPopupListbox;
    FActiveList: TWinControl;
//    FLookupSource: TDatasource;
    FEditStyle: TEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FDropDownRows: Cardinal;
    FPickItems : string;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TEditStyle);
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    function OverButton(const P: TPoint): Boolean;
    function ButtonRect: TRect;
  protected
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property  EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property  ActiveList: TWinControl read FActiveList write FActiveList;
//    property  DataList: TDBLookupListBox read FDataList;
    property  PickList: TPopupListbox read FPickList;
    property  DropDownRows : Cardinal read FDropDownRows write FDropDownRows;
  public
    constructor Create(Owner: TComponent); override;
  end;

function Min(A, B : Integer) : integer;
begin
  if A < B then Result := A
           else Result := B;
end;

procedure GradFill(Canvas : TCanvas; BeginColor, EndColor: TColor; ARect: TRect);

  function ByteToWord(b : Byte) : Word;
  begin
    Result := b * 255;
  end;
  
var
  vert : array[0..1] of GSCTRIVERTEX;
  gRect : GRADIENT_RECT;
  C1, C2 : TColor;
begin
  // Get the correct RGB values from system colors
  C1 := ColorToRGB(BeginColor);
  C2 := ColorToRGB(EndColor);

  // Define top left coordinate and its color
  vert[0].x      := ARect.Left;
  vert[0].y      := ARect.Top;
  vert[0].Red    := ByteToWord(GetRValue(C1));
  vert[0].Green  := ByteToWord(GetGValue(C1));
  vert[0].Blue   := ByteToWord(GetBValue(C1));
  vert[0].Alpha  := ByteToWord(0);

  // Define bottom right coordinate and its color
  vert[1].x      := ARect.Right;
  vert[1].y      := ARect.Bottom;
  vert[1].Red    := ByteToWord(GetRValue(C2));
  vert[1].Green  := ByteToWord(GetGValue(C2));
  vert[1].Blue   := ByteToWord(GetBValue(C2));
  vert[1].Alpha  := ByteToWord(0);

  gRect.UpperLeft  := 0;
  gRect.LowerRight := 1;

  // Do the fill
  GradientFill(Canvas.Handle,vert[0],2,@gRect,1,GRADIENT_FILL_RECT_H);
end;

function MininizeString(const Caption : string; Canvas : TCanvas; MaxLength : Integer) : string;
var
  S : string;
  i : integer;
begin
  for i := 1 to Length(Caption) do
    begin
      S := S + Copy(Caption,i,1);
      if Canvas.TextWidth(S) > MaxLength then
        begin
          Delete(S,i,1);
          break;
        end;
    end;
    
  Result := S;
end;

procedure Register;
begin
  RegisterComponents('GSC QBuilder', [TGSCQBWorkArea]);
  RegisterComponents('GSC QBuilder', [TGSCQBGrid]);
end;

{ TGSCQBTable }

destructor TGSCQBTable.Destroy;
begin
  Selected := true;

  if Assigned(Parent) then
    begin
      if Assigned(TGSCQBWorkArea(Parent).FOnRemoveTable) then
        TGSCQBWorkArea(Parent).FOnRemoveTable(TGSCQBWorkArea(Parent),FTableName);

      TGSCQBWorkArea(Parent).UnlinkTable(Self);
      TGSCQBWorkArea(Parent).RemoveTableForm(Self);
      if Assigned(TGSCQBWorkArea(Parent).FieldGrid) then
        TGSCQBWorkArea(Parent).FieldGrid.RemoveColumnsForTable(FTableName);

      Parent.RemoveControl(Self);
    end;

  inherited;
end;

procedure TGSCQBTable.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style + WS_THICKFRAME;
end;

procedure TGSCQBTable.CaptionPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Selected := true;
  ReleaseCapture;
  Perform(WM_SYSCOMMAND,SC_MOVE+2,0);
end;

procedure TGSCQBTable.FormResize(Sender: TObject);
begin
  CloseBtn.Left := Width - CloseBtn.Width - 8;
  UnLinkBtn.Left := Width - CloseBtn.Width  - UnLinkBtn.Width - 8;
  FieldList.Height := Height - 24;
  Repaint;

  if Assigned(Parent) then
    TGSCQBWorkArea(Parent).ReboundLinksForTable(Self);
end;

procedure TGSCQBTable.FormClose(Sender: TObject; var Action : TCloseAction);
begin
  Action := caFree;
end;

procedure TGSCQBTable.Paint;
var
  Color1, Color2 : TColor;
  OldBkMode : integer;
begin
  inherited;
  if FSelected then
    begin
      Color1 := clActiveCaption;
      Color2 := clGradientActiveCaption;
      Canvas.Font.Color := clCaptionText;
    end
    else
    begin
      Color1 := clInactiveCaption;
      Color2 := clGradientInactiveCaption;
      Canvas.Font.Color := clInactiveCaptionText;
    end;

  GradFill(Canvas, Color1, Color2, Rect(0,0,Width,18));

  OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);  { paint text transparently - so gradient can show through }
  try
    Canvas.TextOut(2,2,MininizeString(FTableName + ' : ' + FTableAlias,Canvas,UnLinkBtn.Left - 5));
  finally
    SetBkMode(Canvas.Handle, OldBkMode);
  end;
end;

procedure TGSCQBTable.SetSelected(const Value: boolean);
begin
  if Value and Assigned(Parent) then
    begin
      TGSCQBWorkArea(Parent).ClearSelected;
      TGSCQBWorkArea(Parent).FSelectedItem := Self;
    end;

  FSelected := Value;
  Invalidate;
end;

function TGSCQBTable.GetRowY(FieldNo: integer): integer;
var
  p : TPoint;
begin
  p.X := FieldList.Left;
  p.Y := Min(Height - 10,FieldList.Top + FieldNo * FieldList.ItemHeight + FieldList.ItemHeight div 2 + 1);
  p := Parent.ScreenToClient(ClientToScreen(p));
  Result := p.Y;
end;

procedure TGSCQBTable.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and (not (AParent is TGSCQBWorkArea)) then
    raise EGSCQBException.Create(SNotValidParent);

  if Assigned(Parent) then
    TGSCQBWorkArea(Parent).RemoveTableForm(Self);

  inherited SetParent(AParent);

  if Assigned(AParent) then
    TGSCQBWorkArea(AParent).AddTableForm(Self);
end;

procedure TGSCQBTable.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TGSCQBTable.UnLinkBtnClick(Sender: TObject);
begin
  Selected := true;
  TGSCQBWorkArea(Parent).UnlinkTable(Self);
end;

procedure TGSCQBTable.SelectAllClick(Sender: TObject);
var
  i : integer;
begin
  if FieldList.Items.Count = 1 then
    exit;

  for i := 0 to FieldList.Items.Count - 1 do
    begin
      FieldList.Checked[i] := true;

      if Assigned(TGSCQBWorkArea(Parent).FieldGrid) then
        TGSCQBWorkArea(Parent).FieldGrid.Insert(FieldList.Items[i], FTableName);
    end;
end;

procedure TGSCQBTable.SetTableAliasClick(Sender: TObject);
var
  NewAlias : string;
begin
  NewAlias := FTableAlias;
  if InputQuery(SAliasChangeCaption,SAliasChangePrompt,NewAlias) then
    begin
      if NewAlias = FTableAlias then
        exit;

      if Assigned(TGSCQBWorkArea(Parent).TableByAlias(NewAlias)) then
          raise EGSCQBException.Create(SAliasExists)
        else
        begin
          FTableAlias := NewAlias;
          Invalidate;
          TGSCQBWorkArea(Parent).DoChange;
        end;
    end;
end;

procedure TGSCQBTable.UnSelectAllClick(Sender: TObject);
var
  i : integer;
begin
  if FieldList.Items.Count = 1 then
    exit;

  for i := 0 to FieldList.Items.Count - 1 do
    begin
      FieldList.Checked[i] := false;

      if Assigned(TGSCQBWorkArea(Parent).FieldGrid) then
        TGSCQBWorkArea(Parent).FieldGrid.RemoveColumnsForTable(FTableName);
    end;
end;

procedure TGSCQBTable.FieldListClickCheck(Sender: TObject);
begin
  if Sender <> FieldList then
    exit;

  if FieldList.Checked[FieldList.ItemIndex] then
    begin
      if Assigned(TGSCQBWorkArea(Parent).FieldGrid) then
        TGSCQBWorkArea(Parent).FieldGrid.Insert(FieldList.Items[FieldList.ItemIndex],
                                                FTableName);
    end
    else
    begin
      if Assigned(TGSCQBWorkArea(Parent).FieldGrid) then
        TGSCQBWorkArea(Parent).FieldGrid.RemoveColumn(FieldList.Items[FieldList.ItemIndex],
                                                      FTableName);
    end;

  TGSCQBWorkArea(Parent).DoChange;
end;

procedure TGSCQBTable.FieldListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Selected := true;
  if Button = mbLeft then
    FieldList.BeginDrag(false)
end;

procedure TGSCQBTable.FieldListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := ((Source is TCheckListBox) and (TCheckListBox(Source).Parent is TGSCQBTable)) or
            ((Source is TListBox) and (Source = TGSCQBWorkArea(Parent).TableList));

  if (Source is TCheckListBox) and (Source <> FieldList) then
    FieldList.ItemIndex := FieldList.ItemAtPos(Point(X,Y),true);
end;

procedure TGSCQBTable.FieldListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  RowNo,
  RowHeight : integer;
begin
  if (Source is TCheckListBox) then
    begin
      if (TWinControl(Source).Parent is TGSCQBTable) then
        begin
          RowHeight := FieldList.ItemHeight;
          if RowHeight <> 0 then RowNo := Y div RowHeight
                            else RowNo := 0;

          if RowNo > FieldList.Items.Count - 1 then
            RowNo := FieldList.Items.Count - 1;

          // handler for target's '*' row
          if RowNo = 0 then
            exit;

          // handler for source's '*' row
          if TGSCQBTable(TWinControl(Source).Parent).FieldList.ItemIndex = 0 then
            exit;
            
          if Source <> FieldList then
              TGSCQBWorkArea(Parent).InsertLink(
                TGSCQBTable(TWinControl(Source).Parent), Self,
                TGSCQBTable(TWinControl(Source).Parent).FieldList.ItemIndex, RowNo)
            else
            begin
              if RowNo <> FieldList.ItemIndex then
                TGSCQBWorkArea(Parent).InsertLink(Self, Self, FieldList.ItemIndex, RowNo);
            end;
        end
    end
    else
    begin
      // If user will add a table from TableList and not link two columns
      if (Sender = FieldList) and (Source = TGSCQBWorkArea(Parent).TableList) then
        begin
          X := X + Left + TWinControl(Sender).Left;
          Y := Y + Top + TWinControl(Sender).Top;
        end
        else
        begin
          X := X + Left;
          Y := Y + Top;
        end;
      TGSCQBWorkArea(Parent).InsertTable(X,Y);
    end;
end;

procedure TGSCQBTable.CheckField(const AFieldName: string; Check: boolean);
var
  ix : integer;
begin
  ix := FieldList.Items.IndexOf(AFieldName);
  if ix > -1 then
    begin
      FieldList.Checked[ix] := Check;
      FieldList.ItemIndex := ix;
      FieldListClickCheck(FieldList);
    end;
end;

procedure TGSCQBTable.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Assigned(Parent) then
    TGSCQBWorkArea(Parent).ReboundLinksForTable(Self);
  inherited;
end;

procedure TGSCQBTable.WMMove(var Message: TWMMove);
begin
  inherited;
  if Assigned(Parent) then
    TGSCQBWorkArea(Parent).ReboundLinksForTable(Self);
end;

procedure TGSCQBTable.TableMenuPopup(Sender: TObject);
var
  MenuHandled : boolean;
begin
  MenuHandled := false;

  if Assigned(TGSCQBWorkArea(Parent).FOnTablePopupMenu) then
    TGSCQBWorkArea(Parent).FOnTablePopupMenu(Self,ScreenToClient(Mouse.CursorPos),MenuHandled);

  if MenuHandled then
    Abort;
end;

procedure TGSCQBTable.SetTableAlias(const Value: string);
begin
  if Assigned(TGSCQBWorkArea(Parent).TableByAlias(Value)) then
    raise EGSCQBException.CreateFmt(STableAliasExists,[Value]);

  FTableAlias := Value;
  Invalidate;
  TGSCQBWorkArea(Parent).DoChange;
end;

{ TGSCQBLink }

constructor TGSCQBLink.Create(AOwner: TComponent);
var
  MenuArray : array [0..7] of TMenuItem;
begin
  if not (AOwner is TGSCQBWorkArea) then
    raise EGSCQBException.Create(SNotValidOwner);
    
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable];

  Width := 105;
  Height := 105;

  FRegion := CreateRectRgn(0,0,Hand,Hand);

  MenuArray[0] := NewItem('',0,false,false,nil,0,'miLinkName');
  MenuArray[1] := NewLine;
  MenuArray[2] := NewItem(SRemoveLink,0,false,true,UnlinkClick,0,'miUnlink');
  MenuArray[3] := NewLine;
  MenuArray[4] := NewItem(SAllRowsForm,0,false,true,AllFromTable1Click,0,'miAllFromTable1');
  MenuArray[5] := NewItem(SAllRowsForm,0,false,true,AllFromTable2Click,0,'miAllFromTable2');
  MenuArray[6] := NewLine;
  MenuArray[7] := NewItem(SProperties,0,false,true,LinkOptionsClick,0,'miLinkOptions');
  PopupMenu := NewPopupMenu(Self,'Menu',paLeft,false,MenuArray);
  PopupMenu.PopupComponent := Self;

  ShowHint := true;
  Hint := '';

  TGSCQBWorkArea(Owner).AddLinkObject(Self);
end;

destructor TGSCQBLink.Destroy;
begin
  DeleteObject(FRegion);
  TGSCQBWorkArea(Owner).RemoveLinkObject(Self);
  inherited Destroy;
end;

procedure TGSCQBLink.Paint;
var
  RegionArray,
  PointArray : array [1..4] of TPoint;
  CenterPoint : TPoint;
  JoinBitmap : TBitmap;
  BmpName : string;

  procedure FillPointArray(X1, Y1, X2, Y2, X3, Y3, X4, Y4 : LongInt);
  begin
    PointArray[1].X := X1;
    PointArray[1].Y := Y1;
    PointArray[2].X := X2;
    PointArray[2].Y := Y2;
    PointArray[3].X := X3;
    PointArray[3].Y := Y3;
    PointArray[4].X := X4;
    PointArray[4].Y := Y4;

    RegionArray[1].X := PointArray[2].X + 5;
    RegionArray[1].Y := PointArray[2].Y - 5;
    RegionArray[2].X := PointArray[2].X - 5;
    RegionArray[2].Y := PointArray[2].Y + 5;
    RegionArray[3].X := PointArray[3].X - 5;
    RegionArray[3].Y := PointArray[3].Y + 5;
    RegionArray[4].X := PointArray[3].X + 5;
    RegionArray[4].Y := PointArray[3].Y - 5;
  end;
  
begin
  // FLinkX: 1 - Table1 left to Table2 and enought space
  //         2 - Table2 left to Table1 and Table2 wider
  //         3 - Table1 left to Table2 and Table1 wider
  //         4 - Table2 left to Table1 and enought space

  // FLinkY: 1 - Field1 higher than Field2
  //         2 - Field2 higher than Field1
  
  if FTable1 <> FTable2 then
    begin
      if ((FLinkX = 1) and (FLinkY = 1)) or ((FLinkX = 4) and (FLinkY = 2)) then
        // |----|
        // | F  |¬
        // |----| | |----|
        //        - | F  |
        //          |----|
        FillPointArray(0,Hand div 2, Hand, Hand div 2, Width - Hand, Height - Hand div 2, Width, Height - Hand div 2);

      if Width > (Hand + Hand2) then
        begin
          if ((FLinkX = 2) and (FLinkY = 1)) or ((FLinkX = 3) and (FLinkY = 2)) then
            FillPointArray(0,Hand div 2, Hand, Hand div 2, Width - 5, Height - Hand div 2, Width - Hand, Height - Hand div 2);

          if ((FLinkX = 3) and (FLinkY = 1)) or ((FLinkX = 2) and (FLinkY = 2)) then
            FillPointArray(Width - Hand, Hand div 2, Width - 5, Hand div 2, Hand, Height - Hand div 2, 0, Height - Hand div 2);
        end
        else
        begin
          if ((FLinkX =2) and (FLinkY =1)) or ((FLinkX =3) and (FLinkY =2)) or
             ((FLinkX =3) and (FLinkY =1)) or ((FLinkX =2) and (FLinkY =2)) then
            FillPointArray(0,Hand div 2, Width - Hand2, Hand div 2, Width - Hand2, Height - Hand div 2, 0, Height - Hand div 2);
        end;
          if ((FLinkX = 4) and (FLinkY = 1)) or  ((FLinkX = 1) and (FLinkY = 2)) then
            FillPointArray(Width, Hand div 2, Width - Hand, hand div 2, Hand, Height - Hand div 2, 0, Height - Hand div 2);
    end
    else
      FillPointArray(0, Hand div 2, Hand - 5, Hand div 2, Hand - 5, Height - Hand div 2, 0, Height - Hand div 2);

  if FSelected then Canvas.Pen.Width := 2
               else Canvas.Pen.Width := 1;
  Canvas.PolyLine(PointArray);
  Canvas.Brush := Parent.Brush;
  DeleteObject(FRegion);

  FRegion := CreatePolygonRgn(RegionArray,4,ALTERNATE);

  CenterPoint.X := Min(PointArray[2].X,PointArray[3].X) + Abs(PointArray[2].X-PointArray[3].X) div 2;
  CenterPoint.Y := Min(PointArray[2].Y,PointArray[3].Y) + Abs(PointArray[2].Y-PointArray[3].Y) div 2;

  BmpName := 'QBINNERJOIN';
  case FLinkType of
    ltInner      : begin
                     BmpName := 'QBINNERJOIN';
                   end;
    ltLeftOuter  : begin
                     if FLinkX in [1,2] then BmpName := 'QBLEFTOUTER'
                                        else BmpName := 'QBRIGHTOUTER';
                   end;
    ltRightOuter : begin
                     if FLinkX in [1,2] then BmpName := 'QBRIGHTOUTER'
                                        else BmpName := 'QBLEFTOUTER';
                   end;
    ltFullOuter  : begin
                     BmpName := 'QBFULLOUTER';
                   end;
  end;

  JoinBitmap := TBitmap.Create;
  try
    JoinBitmap.LoadFromResourceName(HINSTANCE,BmpName);
    JoinBitmap.Transparent := true;
    Canvas.Draw(CenterPoint.X - 8, CenterPoint.Y - 8,JoinBitmap);
  finally
    JoinBitmap.Free;
  end;
end;

procedure TGSCQBLink.CMHitTest(var Message: TCMHitTest);
// Method to allow user to activate control near its line
begin
  if PtInRegion(FRegion,Message.XPos,Message.YPos) then
    Message.Result := 1;
end;

procedure TGSCQBLink.LinkOptionsClick(Sender: TObject);
// Event handler for popupmenu's "Properties" command
begin
  SetupLink;
end;

procedure TGSCQBLink.UnlinkClick(Sender: TObject);
// Event handler for captions "Unlink" button
begin
  Unlink;
end;

procedure TGSCQBLink.AllFromTable1Click(Sender : TObject);
// Event handler for popupmenu's "All columns from ..." command
begin
  if not TMenuItem(Sender).Checked then
    begin
      if FLinkType = ltRightOuter then FLinkType := ltFullOuter
                                  else FLinkType := ltLeftOuter;
    end
    else
    begin
      if FLinkType = ltFullOuter then FLinkType := ltRightOuter
                                 else FLinkType := ltInner;
    end;
  Invalidate;
  ReleaseCapture;
  TGSCQBWorkArea(Parent).DoChange;
end;

procedure TGSCQBLink.AllFromTable2Click(Sender : TObject);
// Event handler for popupmenu's "All columns from ..." command
begin
  if not TMenuItem(Sender).Checked then
    begin
      if FLinkType = ltLeftOuter then FLinkType := ltFullOuter
                                 else FLinkType := ltRightOuter;
    end
    else
    begin
      if FLinkType = ltFullOuter then FLinkType := ltLeftOuter
                                 else FLinkType := ltInner;
    end;
  Invalidate;
  ReleaseCapture;
  TGSCQBWorkArea(Parent).DoChange;
end;

procedure TGSCQBLink.Rebound;
var
  X1,X2,
  Y1,Y2 : integer;
begin
  if FTable1 = FTable2 then
    begin
      // Link between the same table
      X1 := FTable1.Left + FTable1.Width; // Bound starts at Table1's right...
      X2 := FTable1.Left + FTable1.Width + Hand;  // ... and ends at Table1's right + a small
    end
    else
    begin
      if FTable1.Left < FTable2.Left then
        begin
          //Table1 is left to Table2...
          if (FTable1.Left + FTable1.Width + Hand) < FTable2.Left then
            begin
              // ...and there is enought space between them
              //
              //  |----|
              //  | T1 |    |----|
              //  |----|    | T2 |
              //            |----|
              X1 := FTable1.Left + FTable1.Width; // Bound starts at Table1's right...
              X2 := FTable2.Left; // ... and ends at Table2's left
              FLinkX := 1;
            end
            else
            begin
              if (FTable1.Left + FTable1.Width) > (FTable2.Left + FTable2.Width) then
                begin
                  // ...and Table1 is wider than Table2
                  //
                  //  |----|
                  //  | T1 |
                  //  |----|
                  //
                  //   |--|
                  //   |T2|
                  //   |--|
                  X1 := FTable2.Left + FTable2.Width; // Bound starts at Table2's right...
                  X2 := FTable1.Left + FTable1.Width + Hand; // ... and ends at Table1's left
                  FLinkX := 3;
                end
                else
                begin
                  // ...and Table2 is wider than Table1
                  //
                  //  |----|
                  //  | T2 |
                  //  |----|
                  //
                  //   |--|
                  //   |T1|
                  //   |--|
                  X1 := FTable1.Left + FTable1.Width;  // Bound starts at Table1's right...  
                  X2 := FTable2.Left + FTable2.Width + Hand;  // ... and ends at Table2's left
                  FLinkX := 2;
                end;
            end;
        end
        else
        begin
          // Table2 is left to Table1...
          if (FTable2.Left + FTable2.Width + Hand) > FTable1.Left then
            begin
              if (FTable2.Left + FTable2.Width) > (FTable1.Left + FTable1.Width) then
                begin
                  // ...and Table1 is wider than Table2
                  //
                  //  |----|
                  //  | T2 |
                  //  |----|
                  //
                  //   |--|
                  //   |T1|
                  //   |--|
                  X1 := FTable1.Left + FTable1.Width;
                  X2 := FTable2.Left + FTable2.Width + Hand;
                  FLinkX := 2;
                end
                else
                begin
                  // ...and Table1 is wider than Table2
                  //
                  //  |----|
                  //  | T1 |
                  //  |----|
                  //
                  //   |--|
                  //   |T2|
                  //   |--|
                  X1 := FTable2.Left + FTable2.Width;
                  X2 := FTable1.Left + FTable1.Width + Hand;
                  FLinkX:=3;
                end;
            end
            else
            begin
              // ...and there is enought space between them
              //
              //  |----|
              //  | T2 |    |----|
              //  |----|    | T1 |
              //            |----|
              X1 := FTable2.Left + FTable2.Width;
              X2 := FTable1.Left;
              FLinkX := 4;
            end;
        end;
    end;

  Y1 := FTable1.GetRowY(FieldNo1);
  Y2 := FTable2.GetRowY(FieldNo2);
  if Y1 < Y2 then
    begin
      // Field1 is higher than Field2
      Y1 := FTable1.GetRowY(FieldNo1) - Hand div 2; // Y1 is the upper coord
      Y2 := FTable2.GetRowY(FieldNo2) + Hand div 2; // Y2 is the lower coord
      FLinkY := 1;
    end
    else
    begin
      // Field1 is lower than Field2
      Y2 := FTable1.GetRowY(FieldNo1) + Hand div 2;  // Y2 is the upper coord
      Y1 := FTable2.GetRowY(FieldNo2) - Hand div 2;  // Y1 is the lower coord
      FLinkY:=2;
    end;

  SetBounds(X1,Y1,X2-X1,Y2-Y1);
end;

procedure TGSCQBLink.SetFieldName1(const Value : string);
// Set the internal FFieldName1 field and FFieldNo1 field
var
  FieldIX : Integer;
begin
  if FFieldName1 <> Value then
    begin
      FieldIX := FTable1.FieldList.Items.IndexOf(Value);
      if (FieldIX <> -1) and (Value <> SAllColumns) then
        begin
          FFieldName1 := Value;
          FFieldNo1 := FieldIX;
          UpdateHint;
          Rebound;
          TGSCQBWorkArea(Parent).DoChange;
        end
        else
          raise EGSCQBException.CreateFmt(SFieldNotInTable,[Value,FTable1.TableName]);
    end;
end;

procedure TGSCQBLink.SetFieldName2(const Value : string);
// Set the internal FFieldName2 field and FFieldNo2 field
var
  FieldIX : Integer;
begin
  if FFieldName2 <> Value then
    begin
      FieldIX := FTable2.FieldList.Items.IndexOf(Value);
      if (FieldIX <> -1) and (Value <> SAllColumns)  then
        begin
          FFieldName2 := Value;
          FFieldNo2 := FieldIX;
          UpdateHint;
          Rebound;
          TGSCQBWorkArea(Parent).DoChange;
        end
        else
          raise EGSCQBException.CreateFmt(SFieldNotInTable,[Value,FTable2.TableName]);
    end;
end;

procedure TGSCQBLink.UpdateHint;
// Updates the hint text for the link
begin
  if Assigned(FTable1)   and Assigned(FTable2) and
     (FFieldName1 <> '') and (FFieldName2 <> '') then Hint := QuotedStr(FTable1.TableName) + '.' + QuotedStr(FFieldName1) + ' ' + EqualStr[FLinkOpt] + ' ' + QuotedStr(FTable2.TableName) + '.' + QuotedStr(FFieldName2)
                                                 else Hint := '';
end;

procedure TGSCQBLink.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and (not (AParent is TGSCQBWorkArea)) then
    raise EGSCQBException.Create(SNotValidParent);

  if Assigned(Parent) then
    begin
      TGSCQBWorkArea(Parent).RemoveLinkObject(Self);
      TGSCQBWorkArea(Parent).DoChange;
    end;

  inherited SetParent(AParent);

  if Assigned(AParent) then
    begin
      TGSCQBWorkArea(AParent).AddLinkObject(Self);
      TGSCQBWorkArea(AParent).DoChange;
    end;
end;

procedure TGSCQBLink.SetLinkType(const Value: integer);
// Set the internal FLinkType field
begin
  FLinkType := Value;
  Invalidate;
  TGSCQBWorkArea(Parent).DoChange;
end;

procedure TGSCQBLink.SetFieldNo1(const Value: integer);
// Set the internal FFieldName1 field and FFieldNo1 field
begin
  if FFieldNo1 <> Value then
    begin
      if (Value < (FTable1.FieldList.Items.Count - 1)) and (Value > 0) then
        begin
          FFieldNo1 := Value;
          FFieldName1 := FTable1.FieldList.Items[Value];
          UpdateHint;
          Rebound;
          TGSCQBWorkArea(Parent).DoChange;
        end
        else
          raise EGSCQBException.CreateFmt(SInvalidFieldIndex,[Value,FTable1.TableName]);
    end;
end;

procedure TGSCQBLink.SetFieldNo2(const Value: integer);
// Set the internal FFieldName2 field and FFieldNo2 field
begin
  if FFieldNo2 <> Value then
    begin
      if (Value < (FTable2.FieldList.Items.Count - 1)) and (Value > 0) then
        begin
          FFieldNo2 := Value;
          FFieldName2 := FTable2.FieldList.Items[Value];
          UpdateHint;
          Rebound;
          TGSCQBWorkArea(Parent).DoChange;
        end
        else
          raise EGSCQBException.CreateFmt(SInvalidFieldIndex,[Value,FTable2.TableName]);
    end;
end;

procedure TGSCQBLink.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  p : TPoint;
  i : integer;
  Link : TGSCQBLink;
  MenuHandled : boolean;
begin
  if not Parent.Focused then
    Parent.SetFocus;

  Selected := true;
  
  p.X := X;
  p.Y := Y;
  p := ClientToScreen(p);

  if PtInRegion(FRegion,X,Y) then
    begin
      if Assigned(FTable1) and Assigned(FTable2) then
        begin
          PopupMenu.Items[0].Caption := FTable1.TableName + ' :: ' + FTable2.TableName;
          PopupMenu.Items[4].Caption := Format(PopupMenu.Items[4].Caption,[FTable1.TableName]);
          PopupMenu.Items[4].Checked := FLinkType in [ltLeftOuter,ltFullOuter];
          PopupMenu.Items[5].Caption := Format(PopupMenu.Items[5].Caption,[FTable2.TableName]);
          PopupMenu.Items[5].Checked := FLinkType in [ltRightOuter,ltFullOuter];
        end;

      MenuHandled := false;
      if Assigned(TGSCQBWorkArea(Parent).FOnLinkPopupMenu) then
        TGSCQBWorkArea(Parent).FOnLinkPopupMenu(Self,Point(X,Y),MenuHandled);

      if not MenuHandled then
        PopupMenu.Popup(p.X,p.Y);
    end
    else
    begin
      for i := TGSCQBWorkArea(Parent).FLinks.Count - 1 downto 0 do
        begin
          Link := TGSCQBWorkArea(Parent).FLinks[i];
          if (Link <> Self) then
            with Link do
              begin
                p := ScreenToClient(p);
                if Perform(CM_HITTEST,0,Integer(PointToSmallPoint(p))) <> 0 then
                  exit;
              end;
        end;
    end;

  inherited;
end;

procedure TGSCQBLink.SetupLink;
begin
  with TGSCQBLinkPropForm.Create(Self) do
    begin
      try
        Table1.Text := FTable1.TableName;
        Table2.Text := FTable2.TableName;
        Field1.Text := FFieldName1;
        Field2.Text := FFieldName2;
        JoinOp.ItemIndex := FLinkOpt;
        CBAllFrom1.Checked := (FLinkType = ltLeftOuter) or (FLinkType = ltFullOuter);
        CBAllFrom2.Checked := (FLinkType = ltRightOuter) or (FLinkType = ltFullOuter);
        CBAllFrom1.Caption := Format(CBAllFrom1.Caption,[FTable1.TableName]);
        CBAllFrom2.Caption := Format(CBAllFrom2.Caption,[FTable2.TableName]);

        if ShowModal = mrOK then
          begin
            FLinkOpt := JoinOp.ItemIndex;
            FLinkType := ltInner;
            if CBAllFrom1.Checked and CBAllFrom2.Checked then
                FLinkType := ltFullOuter
              else
                if CBAllFrom1.Checked then
                    FLinkType := ltLeftOuter
                  else
                    if CBAllFrom2.Checked then
                      FLinkType := ltRightOuter;

            UpdateHint;
            TGSCQBWorkArea(Self.Parent).DoChange;
          end;
      finally
        Release;
      end;
    end;

  Invalidate;
  ReleaseCapture;
end;

procedure TGSCQBLink.Unlink;
begin
  if Assigned(TGSCQBWorkArea(Parent).FOnRemoveLink) then
    TGSCQBWorkArea(Parent).FOnRemoveLink(TGSCQBWorkArea(Parent),FTable1.TableName,FTable2.TableName,FFieldName1,FFieldName2);

  Parent := nil;
  Free;

  ReleaseCapture;
end;

procedure TGSCQBLink.SetSelected(const Value: boolean);
// Set the internal FSelected field
begin
  if Value then
    begin
      TGSCQBWorkArea(Parent).ClearSelected;
      TGSCQBWorkArea(Parent).FSelectedItem := Self;
    end;

  FSelected := Value;
  Invalidate;
end;

procedure TGSCQBLink.SetLinkOptions(const Value: integer);
// Set the internal FLinkOpt field
begin
  FLinkOpt := Value;
  UpdateHint;
  TGSCQBWorkArea(Parent).DoChange;
end;

{ TGSCQBWorkArea }

constructor TGSCQBWorkArea.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FTableList := nil;
  FTables := TList.Create;
  FLinks := TList.Create;
  FOptionsClause := '';
  FClosingClause := '';
  FSelectedItem := nil;
  FNoChangeEvent := false;
end;

destructor TGSCQBWorkArea.Destroy;
begin
  FTables.Free;
  FLinks.Free;
  inherited;
end;

function TGSCQBWorkArea.InsertTable(X, Y: Integer; const ATableName : string = '') : TGSCQBTable;
var
  i, j : integer;
  NewTable: TGSCQBTable;

  procedure AddTable(const ATable : string);
  var
    i : integer;
    Fields : TStringList;
  begin
    NewTable := TableByName(ATable);

    if Assigned(NewTable) then
        NewTable.Selected := true
      else
      begin
        if Assigned(FOnAddTable) then
          FOnAddTable(Self,ATable);

        NewTable := TGSCQBTable.Create(Self);
        with NewTable do
          begin
            Parent := Self;
            try
              Selected := true;
              Left := X + j * 20;
              Top := Y + j * 20;

              FTableName := ATable;
              Hint := FTableName;

              if Assigned(TableByAlias(FTableName[1])) then
                begin
                  i := 1;

                  while (i < 256) and Assigned(TableByAlias(FTableName[1] + IntToStr(i))) do
                    Inc(i);

                  if i = 256 then FTableAlias := '#ERROR'
                             else FTableAlias := FTableName[1] + IntToStr(i);
                end
                else
                  FTableAlias := FTableName[1];
    
              Fields := TStringList.Create;
              try
                Fields.Add(SAllColumns);
                if Assigned(OnGetTableFields) then
                  OnGetTableFields(Self,FTableName,Fields);

                i := Fields.IndexOf('');
                while i > -1 do
                  begin
                    Fields.Delete(i);
                    i := Fields.IndexOf('');
                  end;

                if Fields.Count = 1 then
                  raise EGSCQBException.CreateFmt(SNoFieldsInTable,[FTableName]);

                FieldList.Items.AddStrings(Fields);
              finally
                Fields.Free;
              end;

              Height := Min(FieldList.Items.Count * FieldList.ItemHeight + 28,Height);
              if Canvas.TextWidth(FTableName) > (Width - 34) then
                Width := Canvas.TextWidth(FTableName) + 34;

              Visible := true;
            except
              on E : Exception do
                begin
                  MessageDlg(E.Message,mtError,[mbOK],0);
                  Free;
                end;
            end;
          end;
      end;
  end;

begin
  j := 0;
  if ATableName = '' then
    begin
      if not Assigned(FTableList) then
        raise EGSCQBException.Create(STableListPropNil);
        
      for i := 0 to FTableList.Items.Count - 1 do
        begin
          if FTableList.Selected[i] then
            begin
              AddTable(FTableList.Items[i]);
              Inc(j);
            end;
        end;
    end
    else
    begin
      AddTable(ATableName);
    end;

  DoChange;
  
  Result := NewTable;
end;

function TGSCQBWorkArea.InsertLink(ATable1, ATable2: TGSCQBTable; AFieldNo1, AFieldNo2: Integer) : TGSCQBLink;
begin
  if Assigned(FindLink(ATable1, ATable2, AFieldNo1, AFieldNo2)) then
    raise EGSCQBException.Create(STablesLinked);

  if Assigned(FOnAddLink) then
    FOnAddLink(Self,ATable1.TableName,ATable2.TableName,ATable1.FieldList.Items[AFieldNo1],ATable2.FieldList.Items[AFieldNo2]);

  Result := TGSCQBLink.Create(Self);

  with Result do
    begin
      FNoChangeEvent := true;
      try
        Parent := Self;
      finally
        FNoChangeEvent := false;
      end;
      FTable1 := ATable1;
      FTable2 := ATable2;
      FieldName1 := ATable1.FieldList.Items[AFieldNo1];
      FieldName2 := ATable2.FieldList.Items[AFieldNo2];
      Selected := true;
    end;

  DoChange;
end;

function TGSCQBWorkArea.TableByName(const TableName : string) : TGSCQBTable;
var
  i : integer;
begin
  Result := nil;

  for i := FTables.Count - 1 downto 0 do
    if (TGSCQBTable(FTables[i]).TableName = TableName) then
      begin
        Result := FTables[i];
        break;
      end;

{  for i := ControlCount - 1 downto 0 do
    if Controls[i] is TGSCQBTable then
      begin
        TempTable := TGSCQBTable(Controls[i]);
        if (TempTable.TableName = TableName) then
          begin
            Result := TempTable;
            break;
          end;
      end;
}
end;

function TGSCQBWorkArea.TableByAlias(const Alias : string) : TGSCQBTable;
var
  i : integer;
begin
  Result := nil;

  for i := FTables.Count - 1 downto 0 do
    if (TGSCQBTable(FTables[i]).TableAlias = Alias) then
      begin
        Result := FTables[i];
        break;
      end;

{  for i := ControlCount - 1 downto 0 do
    if Controls[i] is TGSCQBTable then
      begin
        TempTable := TGSCQBTable(Controls[i]);
        if (TempTable.TableAlias = Alias) then
          begin
            Result := TempTable;
            break;
          end;
      end;
}
end;

function TGSCQBWorkArea.FindLink(const ATable1, ATable2 : TGSCQBTable; StartWith : integer = 0) : TGSCQBLink;
var
  i : integer;
  Link : TGSCQBLink;
begin
  Result := nil;

  for i := StartWith to FLinks.Count - 1 do
    begin
      Link := TGSCQBLink(FLinks[i]);
      if ((Link.Table1 = ATable1) and (Link.Table2 = ATable2)) or
         ((Link.Table1 = ATable2) and (Link.Table2 = ATable1)) then
         begin
           Result := Link;
           exit;
         end;
    end;
end;

function TGSCQBWorkArea.FindLink(const ATable1, ATable2: TGSCQBTable;
  AFieldNo1, AFieldNo2, StartWith: integer): TGSCQBLink;
var
  i : integer;
  Link : TGSCQBLink;
begin
  Result := nil;

  for i := StartWith to FLinks.Count - 1 do
    begin
      Link := TGSCQBLink(FLinks[i]);
      if (((Link.Table1 = ATable1) and (Link.FieldNo1 = AFieldNo1)) and
          ((Link.Table2 = ATable2) and (Link.FieldNo2 = AFieldNo2))) or
         (((Link.Table1 = ATable2) and (Link.FieldNo1 = AFieldNo2)) and
          ((Link.Table2 = ATable1) and (Link.FieldNo2 = AFieldNo1))) then
         begin
           Result := Link;
           exit;
         end;
    end;
end;

procedure TGSCQBWorkArea.ReboundLinksForTable(ATable : TGSCQBTable);
var
  i : integer;
  Link : TGSCQBLink;
begin
  for i := 0 to FLinks.Count - 1 do
    begin
      Link := TGSCQBLink(FLinks[i]);
      if (Link.Table1 = ATable) or (Link.Table2 = ATable) then
        Link.Rebound;
    end;
end;

procedure TGSCQBWorkArea.UnlinkTable(ATable : TGSCQBTable);
var
  i : integer;
  TempLink : TGSCQBLink;
begin
  for i := 0 to FLinks.Count - 1 do
    begin
      TempLink := TGSCQBLink(FLinks[i]);
      if (TempLink.Table1 = ATable) or (TempLink.Table2 = ATable) then
        begin
          if Assigned(FOnRemoveLink) then
            FOnRemoveLink(Self,TempLink.Table1.TableName,TempLink.Table2.TableName,TempLink.FieldName1,TempLink.FieldName2);

          RemoveControl(TempLink);
          TempLink.Free;
          DoChange;
        end;
    end;
end;

procedure TGSCQBWorkArea.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Accept := Source = FTableList;
end;

procedure TGSCQBWorkArea.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
{  X := X + Left;
  Y := Y + Top;}

  if Source = FTableList then
    InsertTable(X,Y);
end;

procedure TGSCQBWorkArea.ClearSelected;
var
  i : integer;
begin
  if not (csDesigning in ComponentState) then
    begin
      for i := 0 to FTables.Count - 1 do
        TGSCQBTable(FTables[i]).Selected := false;

      for i := 0 to FLinks.Count - 1 do
        TGSCQBLink(FLinks[i]).Selected := false;

      FSelectedItem := nil;
    end;
end;

procedure TGSCQBWorkArea.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ClearSelected;
  SetFocus;
end;

procedure TGSCQBWorkArea.SetFieldGrid(Value: TGSCQBGrid);
begin
  if Value <> FFieldGrid then
    begin
      FFieldGrid := Value;

      if Assigned(FFieldGrid) then
        if FFieldGrid.WorkArea <> Self then
          FFieldGrid.WorkArea := Self;
    end;
end;

procedure TGSCQBWorkArea.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  ix : integer;
begin
  if (Operation = opRemove) then
    begin
      if AComponent = FFieldGrid then
        FFieldGrid := nil;

      if AComponent = FTableList then
        FTableList := nil;

      if (AComponent is TGSCQBTable) and (TGSCQBTable(AComponent).Parent = Self) then
        begin
          ix := FTables.IndexOf(AComponent);
          if ix > -1 then
            FTables.Delete(ix);
        end;

      if (AComponent is TGSCQBLink) and (TGSCQBLink(AComponent).Parent = Self) then
        begin
          ix := FLinks.IndexOf(AComponent);
          if ix > -1 then
            FLinks.Delete(ix);
        end;
    end;

  inherited;
end;

function TGSCQBWorkArea.GetSQL: TStrings;
const
  SOuterJoins : array [1..3] of string =
    (' LEFT OUTER JOIN ',
     ' RIGHT OUTER JOIN ',
     ' FULL OUTER JOIN ');
var
  i, j : integer;
  FieldList, JoinList, WhereList, GroupList,
  TempFieldName, TempGroupBy, TempT1Alias, TempT2Alias : string;
  TempOrderByList, JoinedTables, OuterJoins : TStringList;
  TempTable : TGSCQBTable;
  TempLink : TGSCQBLink;
  TempRow : Integer;
begin
  Result := TStringList.Create;

  FieldList := FOptionsClause;
  WhereList := '';
  GroupList := '';
  TempOrderByList := TStringList.Create;
  TempOrderByList.Sorted := true;
  JoinedTables := TStringList.Create;
  OuterJoins := TStringList.Create;
  try
    for i := 0 to FLinks.Count - 1 do
      begin
        TempLink := TGSCQBLink(FLinks[i]);
        TempT1Alias := TempLink.Table1.TableAlias;
        TempT2Alias := TempLink.Table2.TableAlias;

        if TempLink.FLinkType > 0 then
          begin
            if JoinedTables.IndexOf(TempT1Alias) = -1 then
              JoinedTables.Add(TempT1Alias);
            if JoinedTables.IndexOf(TempT2Alias)=-1 then
              JoinedTables.Add(TempT2Alias);
             OuterJoins.Add(TempLink.Table1.TableName + ' AS ' + TempT1Alias
               + SOuterJoins[TempLink.LinkType]
               + TempLink.Table2.TableName + ' AS ' + TempT2Alias + ' ON '
               + TempT1Alias + '.' + TempLink.FieldName1
               + EqualStr[TempLink.LinkOptions]
               + TempT2Alias + '.' + TempLink.FieldName2
              );
          end
          else
            if WhereList = '' then WhereList := TempT1Alias + '.' + TempLink.FieldName1
                                              + EqualStr[TempLink.LinkOptions]
                                              + TempT2Alias + '.' + TempLink.FieldName2
                              else WhereList := WhereList + ' AND ' +  TempT1Alias + '.' + TempLink.FieldName1
                                              + EqualStr[TempLink.LinkOptions]
                                              + TempT2Alias + '.' + TempLink.FieldName2;
      end;

    for i := 0 to FTables.Count - 1 do
      begin
        TempTable := TGSCQBTable(FTables[i]);
        for j := 0 to TempTable.FieldList.Items.Count - 1 do
          if TempTable.FieldList.Checked[j] then
            begin
{              if TempTable.FieldList.Items[j] = SAllColumns then TempFieldName := TempTable.FieldList.Items[j]
                                                            else }TempFieldName := TempTable.TableAlias + '.' + TempTable.FieldList.Items[j];

              if Assigned(FFieldGrid) then
                begin
                  TempRow := FFieldGrid.FindColumn(TempTable.FieldList.Items[j],TempTable.TableName);
                  if TempRow = -1 then
                    raise EGSCQBException.CreateFmt(SCantFindColumn,[TempTable.FieldList.Items[j],TempTable.TableName]);

                  // Build field and grouping list
                  if FFieldGrid.OutputStates[TempRow] = SShow then
                    begin
                      if FFieldGrid.UseGroupBy and (FFieldGrid.GroupFuncs[TempRow] <> SGroupby) then
                        begin
                          if TempTable.FieldList.Items[j] = SAllColumns then TempGroupBy := FFieldGrid.GroupFuncs[TempRow] + '(' + TempTable.FieldList.Items[j] + ')'
                                                                        else TempGroupBy := FFieldGrid.GroupFuncs[TempRow] + '(' + TempFieldName + ')';
                        end
                        else
                        begin
                          TempGroupBy := TempFieldName;
                          if FFieldGrid.UseGroupBy then
                            if GroupList = '' then GroupList := TempFieldName
                                              else GroupList := GroupList + ', ' + TempFieldName;
                        end;

                      if FieldList = FOptionsClause then FieldList := FieldList + TempGroupBy
                                                    else FieldList := FieldList + ', ' + TempGroupBy;

                      if (FFieldGrid.Aliases[TempRow] <> '') and (FFieldGrid.UseGroupBy or (FFieldGrid.ColNames[TempRow] <> SAllColumns)) then
                        FieldList := FieldList + ' AS ' + FFieldGrid.Aliases[TempRow];
                    end;

                  // Define field alias
                  if (FFieldGrid.Aliases[TempRow] <> '') and (FFieldGrid.UseGroupBy or (FFieldGrid.ColNames[TempRow] <> SAllColumns)) then
                    TempFieldName := FFieldGrid.Aliases[TempRow];

                  // Build order by list
                  if FFieldGrid.SortTypes[TempRow] <> SUnsorted then
                    if FFieldGrid.SortTypes[TempRow] = SAsc then
                        TempOrderByList.Add(FFieldGrid.SortOrders[TempRow] + '=' + TempFieldName)
                      else
                        TempOrderByList.Add(FFieldGrid.SortOrders[TempRow] + '=' + TempFieldName + ' desc');

                  // Build where list
                  if FFieldGrid.Criterias[TempRow] <> '' then
                    if WhereList = '' then WhereList := TempFieldName + ' ' + FFieldGrid.Criterias[TempRow]
                                      else WhereList := WhereList + ' AND ' + TempFieldName + ' ' + FFieldGrid.Criterias[TempRow];
                end
                else
                  // Build field list if no TGSCQBGrid is connected with this control
                  if FieldList = FOptionsClause then FieldList := FieldList + TempTable.TableAlias + '.' + TempTable.FieldList.Items[j]
                                                else FieldList := FieldList + ', ' + TempTable.TableAlias + '.' + TempTable.FieldList.Items[j];
            end;

        // Add tables witch are not written in the join list
        TempT1Alias := TempTable.TableAlias;
        if JoinedTables.IndexOf(TempT1Alias) = -1 then
          if JoinList = '' then JoinList := TempTable.TableName + ' AS ' + TempT1Alias
                           else JoinList := JoinList + ', ' + TempTable.TableName + ' AS ' + TempT1Alias;

        Application.ProcessMessages;
      end;

    Result.Add('SELECT ' + FieldList);
    Result.Add('FROM ' + JoinList);
    Result.AddStrings(OuterJoins);
    if WhereList <> '' then
      Result.Add('WHERE ' + WhereList);
    if GroupList <> '' then
      Result.Add('GROUP BY ' + GroupList);
    if TempOrderByList.Count > 0 then
      begin
        FieldList := '';
        for i := 1 to TempOrderByList.Count do
          if FieldList = '' then FieldList := TempOrderByList.Values[IntToStr(i)]
                            else FieldList := FieldList + ', ' + TempOrderByList.Values[IntToStr(i)];
        if FieldList <> '' then
          Result.Add('ORDER BY ' + FieldList);
      end;
    if FClosingClause <> '' then
      Result.Add(FClosingClause);
  finally
    TempOrderByList.Free;
    JoinedTables.Free;
    OuterJoins.Free;
  end;
end;

procedure TGSCQBWorkArea.AddTableForm(ATable: TGSCQBTable);
begin
  if FTables.IndexOf(ATable) = -1 then
    FTables.Add(ATable);
end;

procedure TGSCQBWorkArea.RemoveTableForm(ATable: TGSCQBTable);
var
  ix : integer;
begin
  ix := FTables.IndexOf(ATable);
  if ix > -1 then
    FTables.Delete(ix);
end;

procedure TGSCQBWorkArea.AddLinkObject(ALink: TGSCQBLink);
begin
  if FLinks.IndexOf(ALink) = -1 then
    FLinks.Add(ALink);
end;

procedure TGSCQBWorkArea.RemoveLinkObject(ALink: TGSCQBLink);
var
  ix : integer;
begin
  ix := FLinks.IndexOf(ALink);
  if ix > -1 then
    FLinks.Delete(ix);
end;

function TGSCQBWorkArea.GetTable(Index: integer): TGSCQBTable;
begin
  Result := TGSCQBTable(FTables[Index]);
end;

function TGSCQBWorkArea.GetLink(Index: integer): TGSCQBLink;
begin
  Result := TGSCQBLink(FLinks[Index]);
end;

function TGSCQBWorkArea.GetTableCount: integer;
begin
  Result := FTables.Count;
end;

function TGSCQBWorkArea.GetLinkCount: integer;
begin
  Result := FLinks.Count;
end;

procedure TGSCQBWorkArea.DoChange;
begin
  if Assigned(FOnChange) and not FNoChangeEvent then
    FOnChange(Self);
end;

procedure TGSCQBWorkArea.SetClosingClause(const Value: string);
begin
  FClosingClause := Value;
  DoChange;
end;

procedure TGSCQBWorkArea.SetOptionsClause(const Value: string);
begin
  FOptionsClause := Value;
  DoChange;
end;

{ TGSCQBGrid }

constructor TGSCQBGrid.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goDrawFocusSelected,goColSizing,goEditing];
  ColCount := 9;
  RowCount := 2;
  DefaultRowHeight := 16;

  InternalColNames[0]     := SCaptionColumn;
  InternalAliases[0]      := SCaptionAlias;
  InternalTableNames[0]   := SCaptionTable;
  InternalOutputStates[0] := SCaptionOutput;
  InternalSortTypes[0]    := SCaptionSortType;
  InternalSortOrders[0]   := SCaptionSortOrder;
  InternalGroupFuncs[0]   := SCaptionGroupBy;
  InternalCriterias[0]    := SCaptionCriterias;

  ColWidths[0] := 11;
  ColWidths[CColumn] := 100;
  ColWidths[CTable] := 100;
  ColWidths[CShow] := 40;
  ColWidths[CSortType] := 80;
  ColWidths[CSortOrder] := 80;
  ColWidths[CCriteria] := 80;

  FWorkArea := nil;

  FCheckedBmp := TBitmap.Create;
  FCheckedBmp.LoadFromResourceName(HINSTANCE,'QBCHECKED');
  FUnCheckedBmp := TBitmap.Create;
  FUnCheckedBmp.LoadFromResourceName(HINSTANCE,'QBUNCHECKED');

{  FSortMenu := NewPopupMenu(Self,'SortMenu',paLeft,false,[
                             NewItem(SAsc,0,false,true,SetSortType,0,'miSortAsc'),
                             NewItem(SDesc,0,false,true,SetSortType,0,'miSortAsc'),
                             NewLine,
                             NewItem(SUnsorted,0,false,true,SetSortType,0,'miSortAsc')
                             ]);
  FSortMenu.Tag := CSortType;
  FSortMenu.OnPopup := SortMenuPopup;}

{  FSortOrderMenu := NewPopupMenu(Self,'SortOrderMenu',paLeft,false,[
                                  NewLine,
                                  NewItem(SUnsorted,0,false,true,SetSortOrder,0,'miUnsorted')
                                  ]);
  FSortOrderMenu.OnPopup := SortOrderPopup;}

  FUseGroupBy := false;
{  FGroupByMenu := NewPopupMenu(Self,'GroupByMenu',paLeft,false,[
                                NewItem(SGroupBy,0,false,true,SetGroupBy,0,'miGroupBy'),
                                NewItem(SCount,0,false,true,SetGroupBy,0,'miCount'),
                                NewItem('Sum',0,false,true,SetGroupBy,0,'miSum'),
                                NewItem('Avg',0,false,true,SetGroupBy,0,'miAvg'),
                                NewItem('Min',0,false,true,SetGroupBy,0,'miMin'),
                                NewItem('Max',0,false,true,SetGroupBy,0,'miMax'),
                                NewLine,
                                NewItem('Custom...',0,false,true,SetCustomGroupBy,0,'miCustom')
                                ]);
  FGroupByMenu.Tag := CGroupBy;
  FGroupByMenu.OnPopup := GroupByMenuPopup;}
end;

destructor TGSCQBGrid.Destroy;
begin
  FCheckedBmp.Free;
  FUnCheckedBmp.Free;
  inherited;
end;

procedure TGSCQBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  mCol,
  mRow : integer;
begin
  inherited;

  MouseToCell(X,Y,mCol,mRow);

  if (not IsEmpty) and (mRow > 0) and (mCol = CShow) then
    begin
      if InternalOutputStates[mRow] = SShow then InternalOutputStates[mRow] := ''
                                            else InternalOutputStates[mRow] := SShow;

      // I don't know why but painting the checkbox fails sometimes
      // Moving to the next column and then back solves the problem
      // Repaint, Update, Invalidate, InvalidateCol, ... doesn't!!!
      DoChange;

      Col := CShow + 1;
      Col := CShow;
    end;
end;

procedure TGSCQBGrid.Insert(const AColumn, ATableName : string);
var
  ARow : integer;
begin
  ARow := FindColumn(AColumn,ATableName);
  if ARow = -1 then
    begin
      if IsEmpty then ARow := 1
                 else
                 begin
                   inherited RowCount := RowCount + 1;
                   ARow := RowCount - 1;
                 end;

      InternalColNames[ARow] := AColumn;
      InternalTableNames[ARow] := ATableName;
      InternalOutputStates[ARow] := SShow;
      InternalSortTypes[ARow] := SUnsorted;
      InternalSortOrders[ARow] := SUnsorted;
      if UseGroupBy then
        if AColumn = SAllColumns then InternalGroupFuncs[ARow] := SCount
                                 else InternalGroupFuncs[ARow] := SGroupBy;
    end;

  DoChange;
  Row := ARow;
end;

function TGSCQBGrid.FindColumn(const AColumn, ATableName : string) : integer;
var
  i : integer;
begin
  Result := -1;

  for i := 1 to RowCount - 1 do
    if (InternalColNames[i] = AColumn) and (InternalTableNames[i] = ATableName) then
      begin
        Result := i;
        exit;
      end;
end;

procedure TGSCQBGrid.RemoveColumn(const AColumn, ATableName : string);
var
  ARow, i  : integer;
begin
  ARow := FindColumn(AColumn,ATableName);

  if ARow > -1 then
    if (RowCount > 2) then
      begin
        DeleteRow(ARow);
        DoChange;
      end
      else
      begin
        for i := 0 to ColCount - 1 do
          Cells[i,1] := '';
      end;
end;

procedure TGSCQBGrid.RemoveColumnsForTable(const ATableName : string);
var
  i  : integer;
begin
  for i := RowCount - 1 downto 1 do
    if InternalTableNames[i] = ATableName then
      RemoveColumn(InternalColNames[i],ATableName);
end;

procedure TGSCQBGrid.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                              var Accept: Boolean);
begin
  Accept := (Source is TCheckListBox) and (TCheckListBox(Source).Parent is TGSCQBTable) and (TCheckListBox(Source).Parent.Parent = FWorkArea);
end;

procedure TGSCQBGrid.DragDrop(Source: TObject; X, Y: Integer);
var
  dCol,
  dRow    : integer;
begin
  if (Source is TCheckListBox) and (TCheckListBox(Source).Parent is TGSCQBTable) then
    begin
      TCheckListBox(Source).Checked[TCheckListBox(Source).ItemIndex] := True;//*** check

      MouseToCell(X,Y,dCol,dRow);

      if dRow = 0 then
        exit;

      Insert(TCheckListBox(Source).Items[TCheckListBox(Source).ItemIndex],
             TGSCQBTable(TCheckListBox(Source).Parent).TableName);
    end;
end;

procedure TGSCQBGrid.SetWorkArea(Value: TGSCQBWorkArea);
begin
  if Value <> FWorkArea then
    begin
      FWorkArea := Value;

      if Assigned(FWorkArea) then
        if FWorkArea.FieldGrid <> Self then
          FWorkArea.FieldGrid := Self;
    end;
end;

procedure TGSCQBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FWorkArea) then
    FWorkArea := nil;
    
  inherited;
end;

function TGSCQBGrid.GetIsEmpty: boolean;
begin
  Result := InternalColNames[1] = '';
end;

procedure TGSCQBGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
const
  CheckedState : array[boolean] of integer = (0,DFCS_CHECKED);
begin
  if (ACol = CGroupBy) and (not FUseGroupBy) and not (gdSelected in AState) then
    Canvas.Brush.Color := clBtnFace;

  inherited DrawCell(ACol, ARow, ARect, AState);

  if (ACol = CShow) and (ARow > 0) then
    begin
      Canvas.FillRect(ARect);
      if InternalOutputStates[ARow] = SShow then Canvas.Draw(ARect.Left + (((ARect.Right - ARect.Left) - FCheckedBmp.Width) div 2),ARect.Top + (((ARect.Bottom - ARect.Top) - FCheckedBmp.Height) div 2),FCheckedBmp)
                                            else Canvas.Draw(ARect.Left + (((ARect.Right - ARect.Left) - FUnCheckedBmp.Width) div 2),ARect.Top + (((ARect.Bottom - ARect.Top) - FUnCheckedBmp.Height) div 2),FUnCheckedBmp);
    end;
end;

procedure TGSCQBGrid.SetUseGroupBy(const Value: boolean);
var
  i : integer;
begin
  FUseGroupby := Value;

  if not IsEmpty then
    for i := 1 to RowCount - 1 do
      if Value then
               begin
                 if InternalColNames[i] = SAllColumns then
                                                      begin
                                                        InternalGroupFuncs[i] := SCount;
                                                        InternalAliases[i] := '';
                                                      end
                                                      else
                                                        InternalGroupFuncs[i] := SGroupBy
               end
               else
                 InternalGroupFuncs[i] := '';

  InvalidateCol(CGroupBy);

  DoChange;
end;

procedure TGSCQBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);

  if IsEmpty then
    exit;
    
  if (Key = VK_SPACE) and (Col = CShow) then
    begin
      Key := 0;
      MouseDown(mbRight,[],CellRect(Col,Row).Left + 10,CellRect(Col,Row).Top + 10);
    end
    else
      if (Key = VK_APPS) and (Col <> CShow) then
        begin
          Key := 0;
          MouseDown(mbRight,[],CellRect(Col,Row).Left + 10,CellRect(Col,Row).Top + 10);
        end
        else
          if EditorMode then
            DoChange;
end;

function TGSCQBGrid.GetMaxOrderID: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 1 to RowCount - 1 do
    if InternalSortTypes[i] <> SUnsorted then
      Inc(Result);
end;

function TGSCQBGrid.GetRowWithOrderID(OrderID: integer): integer;
var
  i : integer;
begin
  Result := -1;

  for i := 1 to RowCount - 1 do
    if InternalSortOrders[i] = IntToStr(OrderID) then
      begin
        Result := i;
        break;
      end;
end;

procedure TGSCQBGrid.DecOrderIDsByOne(FromID: integer);
var
  i : integer;
begin
  for i := 1 to RowCount - 1 do
    if InternalSortOrders[i] <> SUnsorted then
      if StrToInt(InternalSortOrders[i]) > FromID then
        InternalSortOrders[i] := IntToStr(StrToInt(SortOrders[i]) - 1)
end;

procedure TGSCQBGrid.IncOrderIDsByOne(FromID: integer);
var
  i : integer;
begin
  for i := 1 to RowCount - 1 do
    if InternalSortOrders[i] <> SUnsorted then
      if StrToInt(InternalSortOrders[i]) >= FromID then
        InternalSortOrders[i] := IntToStr(StrToInt(InternalSortOrders[i]) + 1)
end;

function TGSCQBGrid.GetDataCol;
begin
  Result := Cols[Index];
end;

function TGSCQBGrid.GetSortTypes(Index: integer): string;
begin
  Result := InternalSortTypes[Index];
end;

procedure TGSCQBGrid.SetSortTypes(Index: integer; const Value: string);
var
  nextid : integer;
begin
  InternalSortTypes[Index] := Value;
  if InternalSortTypes[Index] = SUnsorted then
    begin
      DecOrderIDsByOne(StrToIntDef(InternalSortOrders[Index],MaxInt));
      InternalSortOrders[Index] := SUnsorted;
    end
    else
      if InternalSortOrders[Index] = SUnsorted then
        begin
          nextid := GetMaxOrderID;

          if nextid = 0 then
            nextid := 1;
            
          InternalSortOrders[Index] := IntToStr(nextid);
        end;

  Col := CSortType + 1;
  Col := CSortType;
end;

procedure TGSCQBGrid.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TGSCQBGrid.CreateEditor: TInplaceEdit;
begin
  Result := TGSCQBInplaceEdit.Create(Self);
end;

function TGSCQBGrid.GetAliases(Index: integer): string;
begin
  Result := InternalAliases[Index];
end;

function TGSCQBGrid.GetColNames(Index: integer): string;
begin
  Result := InternalColNames[Index];
end;

function TGSCQBGrid.GetCriterias(Index: integer): string;
begin
  Result := InternalCriterias[Index];
end;

function TGSCQBGrid.GetGroupFuncs(Index: integer): string;
begin
  Result := InternalGroupFuncs[Index];
end;

function TGSCQBGrid.GetOutputStates(Index: integer): string;
begin
  Result := InternalOutputStates[Index];
end;

function TGSCQBGrid.GetSortOrders(Index: integer): string;
begin
  Result := InternalSortOrders[Index];
end;

function TGSCQBGrid.GetTableNames(Index: integer): string;
begin
  Result := InternalTableNames[Index];
end;

procedure TGSCQBGrid.SetAliases(Index: integer; const Value: string);
begin
  if not IsEmpty then
    begin
      InternalAliases[Index] := Value;
      DoChange;
    end;
end;

procedure TGSCQBGrid.SetColNames(Index: integer; const Value: string);
begin
  if not IsEmpty then
    begin
      InternalColNames[Index] := Value;
      DoChange;
    end;
end;

procedure TGSCQBGrid.SetCriterias(Index: integer; const Value: string);
begin
  if not IsEmpty then
    begin
      InternalCriterias[Index] := Value;
      DoChange;
    end;
end;

procedure TGSCQBGrid.SetGroupFuncs(Index: integer; const Value: string);
begin
  if not IsEmpty and FUseGroupBy then
    begin
      InternalGroupFuncs[Row] := Value;

      Col := CGroupBy + 1;
      Col := CGroupBy;

      DoChange;
    end;
end;

procedure TGSCQBGrid.SetOutputStates(Index: integer; const Value: string);
begin
  if not IsEmpty then
    begin
      InternalOutputStates[Index] := Value;
      DoChange;
    end;
end;

procedure TGSCQBGrid.SetSortOrders(Index: integer; const Value: string);
var
  SameRow : integer;
begin
  if IsEmpty then
    exit;
    
  if Value <> SUnsorted then
    begin
      SameRow := GetRowWithOrderID(StrToInt(Value));
      if SameRow > -1 then
        if InternalSortOrders[Row] <> SUnsorted then InternalSortOrders[SameRow] := InternalSortOrders[Row]
                                                else IncOrderIDsByOne(StrToInt(Value));
    end
    else
      DecOrderIDsByOne(StrToIntDef(InternalSortOrders[Row],MaxInt));
    
  InternalSortOrders[Row] := Value;
  if InternalSortOrders[Row] = SUnsorted then
      InternalSortTypes[Row] := SUnsorted
    else
      if InternalSortTypes[Row] = SUnsorted then
        InternalSortTypes[Row] := SAsc;

  Col := CSortOrder + 1;
  Col := CSortOrder;

  DoChange;
end;

procedure TGSCQBGrid.SetTableNames(Index: integer; const Value: string);
begin
  if not IsEmpty then
    begin
      InternalTableNames[Index] := Value;
      DoChange;
    end;
end;

{ TGSCQBInplaceEdit }

procedure TGSCQBInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> esSimple then
    if not TGSCQBGrid(Owner).UseRightToLeftAlignment then
      Dec(R.Right, FButtonWidth)
    else
      Inc(R.Left, FButtonWidth - 2);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

function TGSCQBInplaceEdit.ButtonRect: TRect;
begin
  if not TGSCQBGrid(Owner).UseRightToLeftAlignment then
    Result := Rect(Width - FButtonWidth, 0, Width, Height)
  else
    Result := Rect(0, 0, FButtonWidth, Height);
end;

procedure TGSCQBInplaceEdit.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FPickList.ItemIndex <> -1 then
      ListValue := FPickList.Items[FPicklist.ItemIndex];
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;

    Invalidate;
    if Accept then
      if EditCanModify then
        with TGSCQBGrid(Grid) do
          case Col of
            CColumn    : ColNames[Row] := ListValue;
            CAlias     : Aliases[Row] := ListValue;
            CTable     : TableNames[Row] := ListValue;
            CShow      : OutputStates[Row] := ListValue;
            CSortType  : SortTypes[Row] := ListValue;
            CSortOrder : SortOrders[Row] := ListValue;
            CGroupBy   : GroupFuncs[Row] := ListValue;
            CCriteria  : Criterias[Row] := ListValue;
          end;
  end;
end;

procedure TGSCQBInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

constructor TGSCQBInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
  FDropDownRows := 7;
end;

procedure TGSCQBInplaceEdit.DoDropDownKeys(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TGSCQBInplaceEdit.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FActiveList.Width := Width;

    FPickList.Color := Color;
    FPickList.Font := Font;
    FPickList.Items.CommaText := FPickItems;
    if FPickList.Items.Count >= Integer(FDropDownRows) then
      FPickList.Height := Integer(FDropDownRows) * FPickList.ItemHeight + 4
    else
      FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;
    FPickList.ItemIndex := FPickList.Items.IndexOf(Text);
    J := FPickList.ClientWidth;
    for I := 0 to FPickList.Items.Count - 1 do
    begin
      Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
      if Y > J then J := Y;
    end;
    FPickList.ClientWidth := J;
  end;
  P := Parent.ClientToScreen(Point(Left, Top));
  Y := P.Y + Height;
  if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
  SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FListVisible := True;
  Invalidate;
  Windows.SetFocus(Handle);
end;

procedure TGSCQBInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TGSCQBInplaceEdit.ListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TGSCQBInplaceEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TGSCQBInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TGSCQBInplaceEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TGSCQBInplaceEdit.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TGSCQBInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
begin
  if FEditStyle <> esSimple then
  begin
    R := ButtonRect;
    Flags := 0;
    if FEditStyle in [esPickList] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TGSCQBInplaceEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    esPickList:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
  else  { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  Repaint;
end;

procedure TGSCQBInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TGSCQBInplaceEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TGSCQBInplaceEdit.UpdateContents;
var
  NewStyle : TEditStyle;
  NewReadOnly,
  NewEditorMode : boolean;
  i, max : integer;
begin
  NewStyle := esSimple;
  NewReadOnly := false;
  NewEditorMode := true;

  FPickItems := '';
  with TGSCQBGrid(Grid) do
    begin
      case Col of
        CColumn   : NewReadOnly := true;
        CAlias    : ;
        CTable    : NewReadOnly := true;
        CShow     : begin
                      NewReadOnly := true;
                      NewEditorMode := false;
                    end;  
        CSortType : begin
                      NewStyle := esPickList;
                      NewReadOnly := true;
                      if InternalColNames[Row] = SAllColumns then FPickItems := '"' + SUnsorted + '"'
                                                             else FPickItems := SSortMenu;
                    end;
        CSortOrder: begin
                      NewStyle := esPickList;
                      NewReadOnly := true;
                      max := GetMaxOrderID;

                      if (max < RowCount - 1) and (InternalSortOrders[Row] = SUnsorted) then
                        Inc(max);

                      if InternalColNames[Row] <> SAllColumns then
                        for i := max downto 1 do
                          if FPickItems = '' then FPickItems := '"' + IntToStr(i) + '"'
                                             else FPickItems := FPickItems + ',"' + IntToStr(i) + '"';

                      if FPickItems = '' then FPickItems := '"' + SUnsorted + '"'
                                         else FPickItems := FPickItems + ',"' + SUnsorted + '"';
                    end;
        CGroupBy  : begin
                      NewStyle := esPickList;
                      if InternalColNames[Row] = SAllColumns then FPickItems := '"' + SCount + '"'
                                                             else FPickItems := SGroupMenu;
                    end;
        CCriteria : ;
      end;
    end;

  EditStyle := NewStyle;
  inherited UpdateContents;
  ReadOnly := NewReadOnly;
  TGSCQBGrid(Grid).EditorMode := NewEditorMode;
end;

procedure TGSCQBInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TGSCQBInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.WParam) <> TGSCQBGrid(Grid).Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

procedure TGSCQBInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (FEditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TGSCQBInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TGSCQBInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (FEditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TGSCQBInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle in [esPickList] then
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and FListVisible then
        begin
          with TMessage(Message) do
            SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

{ TPopupListbox }

procedure TPopupListbox.CreateParams(var Params: TCreateParams);
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

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TGSCQBInplaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;

end.
