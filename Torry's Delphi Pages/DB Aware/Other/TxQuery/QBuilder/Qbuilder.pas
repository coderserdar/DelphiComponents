{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Dialog component                  }
{                                                       }
{       Copyright (c) 1996-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik and Alfonso Moreno                 }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

{$HINTS OFF}

{$R QBButton.res}
unit QBuilder;

{$I xq_flag.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, ComCtrls, ToolWin, Menus, CheckLst, Grids,
  DB, DBGrids, SyntaxHi, xqmiscel;

type
  TOQBbutton = (bOpenDialog, bSaveDialog, bRunQuery);
  TOQBbuttons = set of TOQBbutton;

  TOQBEngine = class;

  TOQBuilderDialog = class(TComponent)
  private
    FOQBForm : TForm;
    FSQL : TStrings;
    FOQBEngine: TOQBEngine;
    FShowButtons: TOQBbuttons;
    procedure SetOQBEngine(const Value: TOQBEngine);
    procedure SetShowButtons(const Value: TOQBbuttons);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; virtual;
    property SQL: TStrings read FSQL;
  published
    property OQBEngine: TOQBEngine read FOQBEngine write SetOQBEngine;
    property ShowButtons: TOQBbuttons read FShowButtons write SetShowButtons
      default [bOpenDialog, bSaveDialog, bRunQuery];
    property OQBForm : TForm read FOQBForm;
  end;

  TOQBEngine = class(TComponent)
  private
    FDatabaseName: string;
    FUserName: string;
    FPassword: string;
    FTableList : TStringList;
    FAliasList : TStringList;
    FFieldList : TStringList;
    FSQL: TStringList;
    FSQLcolumns: TStringList;
    FSQLcolumns_table: TStringList;
    FSQLcolumns_func: TStringList;
    FSQLfrom: TStringList;
    FSQLwhere: TStringList;
    FSQLgroupby: TStringList;
    FSQLorderby: TStringList;
    FUseTableAliases: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDatabaseName(const Value: string); virtual;
    procedure SetUserName(const Value: string); virtual;
    procedure SetPassword(const Value: string); virtual;
    procedure SetQuerySQL(Value: string); virtual; abstract;
    procedure GenerateAliases;
    procedure ReadTableList; virtual; abstract;
    procedure ReadFieldList(ATableName: string); virtual; abstract;
  public
    FOQBDialog : TOQBuilderDialog;
    TableName : string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  SelectDatabase: boolean; virtual; abstract;
    function  GenerateSQL : string; virtual; // is not abstract !!!
    procedure ClearQuerySQL; virtual; abstract;
    function  ResultQuery : TDataSet; virtual; abstract;
    procedure OpenResultQuery; virtual; abstract;
    procedure CloseResultQuery; virtual; abstract;
    procedure SaveResultQueryData; virtual; abstract;
    property  TableList : TStringList read FTableList;
    property  AliasList : TStringList read FAliasList;
    property  FieldList : TStringList read FFieldList;
    property  SQL : TStringList read FSQL;
    property  SQLcolumns : TStringList read FSQLcolumns;
    property  SQLcolumns_table : TStringList read FSQLcolumns_table;
    property  SQLcolumns_func : TStringList read FSQLcolumns_func;
    property  SQLfrom : TStringList read FSQLfrom;
    property  SQLwhere : TStringList read FSQLwhere;
    property  SQLgroupby : TStringList read FSQLgroupby;
    property  SQLorderby : TStringList read FSQLorderby;

    property UseTableAliases : boolean read FUseTableAliases write FUseTableAliases default true;
  end;

type
  TArr = array [0..0] of integer;
  PArr = ^TArr;

  TOQBLbx = class(TCheckListBox)
  private
    FArrBold : PArr;
    FLoading : boolean;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DrawItem;
    procedure WMLButtonDown(var Message: TWMLButtonDblClk); message WM_LButtonDown;
    procedure WMRButtonDown(var Message: TWMRButtonDblClk); message WM_RButtonDown;
    function  GetCheckW: Integer;
    procedure AllocArrBold;
    procedure SelectItemBold(Item:integer);
    procedure UnSelectItemBold(Item:integer);
    function  GetItemY(Item:integer):integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClickCheck; override;
  end;

  TOQBTable = class(TPanel)
  private
    ScreenDC : HDC;
    OldX,
    OldY,
    OldLeft,
    OldTop   : Integer;
    ClipRgn  : HRGN;
    ClipRect,
    MoveRect : TRect;
    Moving   : Boolean;
    FCloseBtn,
    FUnlinkBtn : TSpeedButton;
    FLbx : TOQBLbx;
    FTableName : string;
    FTableAlias : string;
    PopMenu : TPopupMenu;
    procedure WMRButtonDown(var Message: TWMRButtonDblClk); message WM_RButtonDown;
    function  Activate(const ATableName:string; X,Y:Integer):boolean;
    function  GetRowY(FldN:integer):integer;
    procedure _CloseBtn(Sender: TObject);
    procedure _UnlinkBtn(Sender: TObject);
    procedure _SelectAll(Sender: TObject);
    procedure _UnSelectAll(Sender: TObject);
    procedure _DragOver(Sender, Source: TObject; X, Y: Integer;
                            State: TDragState; var Accept: Boolean);
    procedure _DragDrop(Sender, Source: TObject; X, Y: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    property Align;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  TOQBLink = class(TShape)
  private
    Tbl1,
    Tbl2 : TOQBTable;
    FldN1,
    FldN2 : integer;
    FldNam1,
    FldNam2 : string;
    FLinkOpt,
    FLinkType : integer;
    LnkX,
    LnkY : byte;
    Rgn  : HRgn;
    PopMenu : TPopupMenu;
    procedure _Click(X,Y:integer);
    procedure CMHitTest(var Message: TCMHitTest); message CM_HitTest;
    function  ControlAtPos(const Pos: TPoint): TControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Paint; override;
  end;

  TOQBArea = class(TScrollBox)
  public
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetOptions(Sender: TObject);
    procedure InsertTable(X,Y: Integer);
    function  InsertLink(_tbl1,_tbl2: TOQBTable; _fldN1,_fldN2: Integer):TOQBLink;
    function  FindTable(TableName:string):TOQBTable;
    function  FindLink(Link:TOQBLink):boolean;
    function  FindOtherLink(Link:TOQBLink;Tbl:TOQBTable;FldN:integer):boolean;
    procedure ReboundLink(Link:TOQBLink);
    procedure ReboundLinks4Table(ATable:TOQBTable);
    procedure Unlink(Sender: TObject);
    procedure UnlinkTable(ATable:TOQBTable);
    procedure _DragOver(Sender, Source: TObject; X, Y: Integer;
                                State: TDragState; var Accept: Boolean);
    procedure _DragDrop(Sender, Source: TObject; X, Y: Integer);
  end;

  { definitions for the field in every column }
  TOQBSortType = (stNone,
                  stAsc,
                  stDesc);

  TOQBFilterAction = ( faIsEqualTo,
                       faIsBetween,
                       faIsGreaterThan,
                       faIsGreaterEqualTo,
                       faIsLessThan,
                       faIsLessEqualTo,
                       faIsLike,
                       faIsNotEqualTo,
                       faIsNotBetween,
                       faIsNotLike );

  TOQBShowAction = ( saShow,
                     saGroup,
                     saHide,
                     saSum,
                     saCount,
                     saAverage,
                     saMinimum,
                     saMaximum );

  TOQBFilterList = class;

  TOQBFilter = class
  private
     FFilterList   : TOQBFilterList;
     FFilterAction : TOQBFilterAction;
     FCustomExpres : String;       { custom expression }
     FData         : array[1..5] of String;
     function GetData(Index: Integer): String;
     procedure SetData(Index: Integer; const Value: String);
  public
     constructor Create(FilterList : TOQBFilterList);
     function FilterVerb: String;
     function IsFilterEmpty(Index: Integer): Boolean;

     property FilterAction: TOQBFilterAction read FFilterAction write FFilterAction;
     property CustomExpres: String read FCustomExpres write FCustomExpres;
     property Data[Index: Integer]: String read GetData write SetData;
  end;

  TOQBFilterList = class
     FItems: TList;
     function GetCount: Integer;
     function GetItem(Index: Integer): TOQBFilter;
  public
     constructor Create;
     destructor Destroy; override;
     function Add: TOQBFilter;
     procedure Clear;
     procedure Delete(Index: Integer);
     property Count: Integer read GetCount;
     property Items[Index: Integer]: TOQBFilter read GetItem; default;
  end;

  TOQBFieldList = class;

  TOQBField = class
  private
     FFieldList  : TOQBFieldList;
     FTable      : String;         { the Table }
     FTableAlias : string;
     FFieldName  : String;         { The FieldName }
     FAlias      : String;         { the Alias }
     FSortType   : TOQBSortType;   { the sort for the column }
     FShowAction : TOQBShowAction; { show actioin for the column }
     FFilters    : TOQBFilterList; { the filters }
  public
     constructor Create(FieldList : TOQBFieldList);
     destructor Destroy; override;
     procedure Assign(Source: TOQBField);
     procedure SaveToStream(Stream: TStream);
     procedure LoadFromStream(Stream: TStream);

     property Table : String read FTable write FTable;
     property TableAlias : String read FTableAlias write FTableAlias;
     property FieldName: String read FFieldName write FFieldName;
     property Alias : String read FAlias write FAlias;
     property SortType : TOQBSortType read FSortType write FSortType;
     property ShowAction : TOQBShowAction read FShowAction write FShowAction;
     property Filters : TOQBFilterList read FFilters write FFilters;
  end;

  TOQBFieldList = class
     FItems: TList;
     function GetCount: Integer;
     function GetItem(Index: Integer): TOQBField;
  public
     constructor Create;
     destructor Destroy; override;
     function Add: TOQBField;
     function Insert(Index: Integer): TOQBField;
     procedure ColumnMoved(FromIndex,ToIndex: Integer);
     procedure Exchange(FromIndex,ToIndex: Integer);
     procedure Clear;
     procedure Delete(Index: Integer);
     procedure SaveToStream(Stream: TStream);
     procedure LoadFromStream(Stream: TStream);

     property Count: Integer read GetCount;
     property Items[Index: Integer]: TOQBField read GetItem; default;
  end;

  TOQBGrid = class(TDrawGrid)
  private
    FFieldList : TOQBFieldList;
    procedure _ColumnMoved(Sender: TObject; FromIndex,ToIndex: Integer);
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    CurrCol    : integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function IsEmpty: Boolean;
    procedure WndProc(var Message: TMessage); override;
    function  MaxSW(const s1,s2:string):integer;
    procedure Insert(aCol:integer;const aField,aTable,aTableAlias:string);
    function  FindColumn(const sCol:string):integer;
    function  FindSameColumn(aCol:integer):boolean;
    procedure RemoveColumn(aCol:integer);
    procedure RemoveColumn4Tbl(const Tbl:string);
    procedure ClickCell(X,Y:integer);
    procedure DblClickCell(X,Y:integer);
    procedure ResetWidths;
    //function  SelectCell(ACol, ARow: integer):boolean; override;
    procedure _DragOver(Sender,Source:TObject;X,Y:integer;State:TDragState;var Accept:boolean);
    procedure _DragDrop(Sender,Source:TObject;X,Y:integer);

    property FieldList: TOQBFieldList read FFieldList;
  end;

  TOQBForm = class(TForm)
    mnuTbl: TPopupMenu;
    Remove1: TMenuItem;
    ResDataSource: TDataSource;
    DlgSave: TSaveDialog;
    DlgOpen: TOpenDialog;
    Panel2: TPanel;
    btnNew: TSpeedButton;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    Bevel1: TBevel;
    btnSQL: TSpeedButton;
    btnResults: TSpeedButton;
    Pages: TPageControl;
    TabColumns: TTabSheet;
    TabSQL: TTabSheet;
    MemoSQL: TRichEdit;
    TabResults: TTabSheet;
    ResDBGrid: TDBGrid;
    Panel3: TPanel;
    QBTables: TListBox;
    HSplitter: TSplitter;
    VSplitter: TSplitter;
    Bevel3: TBevel;
    btnColorSet: TSpeedButton;
    Panel1: TPanel;
    Bevel2: TBevel;
    btnExport: TSpeedButton;
    SaveDialog1: TSaveDialog;
    SyntaxHighlighter1: TSyntaxHighlighter;
    Button1: TButton;
    Button2: TButton;
    procedure mnuRemoveClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSQLClick(Sender: TObject);
    procedure btnResultsClick(Sender: TObject);
    procedure btnColorSetClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    QBDialog : TOQBuilderDialog;
    QBArea   : TOQBArea;
    QBGrid   : TOQBGrid;

    procedure ClearAll;
    procedure OpenDatabase;
  public
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

uses
  QBLnkFrm, xqconsts, QBEdFrm;

{$R *.DFM}

resourcestring
  sMainCaption = 'Query Builder';
  sNotValidTableParent = 'Parent must be TScrollBox or its descendant.';
  sLinkHint = 'Click here for options';

const

  sRows : array[0..8] of string =
     ( 'Field',
       'Table',
       'Show',
       'Sort',
       'Filter 1',
       'Filter 2',
       'Filter 3',
       'Filter 4',
       'Filter 5' );

  sShow : array[TOQBShowAction] of string =
     ( 'Show',
       'Group',
       'Hide',
       'Sum',
       'Count',
       'Average',
       'Minimum',
       'Maximum');

  sAggregate : array[TOQBShowAction] of string =
     ( '',
       '',
       '',
       'SUM',
       'COUNT',
       'AVG',
       'MIN',
       'MAX');

  sFilterAction : array[TOQBFilterAction] of string =
     ( '=',
       'BETWEEN',
       '>',
       '>=',
       '<',
       '<=',
       'LIKE',
       '<>',
       'BETWEEN',
       'LIKE'
       );

  sFilterActionVerb : array[TOQBFilterAction] of string =
     ( ' Is Equal To ',
       ' Is Between ',
       ' Is Greater Than ',
       ' Is Greater Than Or Equal To ',
       ' Is Less Than ',
       ' Is Less Than Or Equal To ',
       ' Is Like ',
       ' Is Not Equal To ',
       ' Is Not Between ',
       ' is Not Like '
       );

  sSort  : array [TOQBSortType] of string =
    ( '',
      'Asc',
      'Desc' );

  sLinkOpt : array [0..5] of string =
    ('=',
     '>',
     '>=',
     '<',
     '<=',
     '<>');

  sInnerOuterJoin : array [0..3] of string =
    ( ' INNER JOIN ',
      ' LEFT  JOIN ',
      ' RIGHT JOIN ',
      ' OUTER JOIN ');

  Hand = 15;
  Hand2 = 12;

  QBSignature = '# QBuilder';

{ TOQBFilter }

constructor TOQBFilter.Create(FilterList : TOQBFilterList);
begin
   inherited Create;
   FFilterList := FilterList;
end;

function TOQBFilter.GetData(Index: Integer): String;
begin
   Result := FData[Index];
end;

procedure TOQBFilter.SetData(Index: Integer; const Value: String);
begin
   FData[Index] := Value;
end;

function TOQBFilter.IsFilterEmpty(Index: Integer): Boolean;
begin
   Result := (Length(FData[Index])=0);
end;

function TOQBFilter.FilterVerb: String;
var
   i : integer;
begin
   Result:='';
   case FFilterAction of
      faIsEqualTo, faIsLike, faIsNotEqualTo, faIsNotLike:
         for i:= 1 to 5 do
            if Length(Trim(FData[i]))>0 then
               if Length(Result)=0 then
                  Result:=sFilterActionVerb[FFilterAction] + FData[i]
               else
                  Result:=Result+' Or ' + FData[i];
      faIsBetween, faIsNotBetween:
         Result:= sFilterActionVerb[FFilterAction]+FData[1]+' And '+FData[2];
      faIsGreaterThan, faIsGreaterEqualTo, faIsLessThan, faIsLessEqualTo:
         Result:= sFilterActionVerb[FFilterAction]+FData[1];
   end;
end;

{ TOQBFilterList }
constructor TOQBFilterList.Create;
begin
   inherited Create;
   FItems:= TList.Create;
end;

destructor TOQBFilterList.Destroy;
begin
   Clear;
   FItems.Free;
   inherited Destroy;
end;

function TOQBFilterList.GetCount;
begin
   Result := FItems.Count;
end;

function TOQBFilterList.GetItem(Index: Integer): TOQBFilter;
begin
   Result := FItems[Index];
end;

function TOQBFilterList.Add: TOQBFilter;
begin
   Result := TOQBFilter.Create(Self);
   FItems.Add(Result);
end;

procedure TOQBFilterList.Clear;
var
   I: Integer;
begin
   for I:= 0 to FItems.Count - 1 do
      TOQBFilter(FItems[I]).Free;
   FItems.Clear;
end;

procedure TOQBFilterList.Delete(Index: Integer);
begin
   TOQBFilter(FItems[Index]).Free;
   FItems.Delete(Index);
end;

{ TOQBField }
constructor TOQBField.Create(FieldList : TOQBFieldList);
var
   i: Integer;
begin
   inherited Create;
   FFieldList := FieldList;               { belongs to }
   FFilters   := TOQBFilterList.Create;
   for i := 1 to 5 do    { 5 filters for every column }
       FFilters.Add;
end;

destructor TOQBField.Destroy;
begin
   FFilters.Free;
   inherited Destroy;
end;

procedure TOQBField.Assign(Source: TOQBField);
var
   i, j : integer;
begin
   FTable := Source.FTable;
   FTableAlias := Source.FTableAlias;
   FFieldName := Source.FFieldName;
   FAlias := SOurce.FAlias;
   FSortType := SOurce.FSortType;
   FShowAction := Source.FShowAction;
   for i := 0 to FFilters.Count - 1 do
   begin
      FFilters[i].FCustomExpres := Source.Filters[i].FCustomExpres;
      FFilters[i].FilterAction := Source.Filters[i].FilterAction;
      for j := 1 to 5 do
         FFilters[i].Data[j] := Source.Filters[i].Data[j];
   end;
end;

procedure TOQBField.SaveToStream(Stream:TStream);
var
   i,
   n,
   j : integer;
   s : string;
   OQBFilter : TOQBFilter ;

   procedure WriteString(const s: string);
   var
      n: integer;
   begin
      n := Length(s);
      Stream.Write(n,sizeof(n));
      if n > 0 then
         Stream.Write(s[1],n);
   end;

begin
   WriteString(FTable);
   WriteString(FTableAlias);
   WriteString(FFieldName);
   WriteString(FAlias);
   Stream.Write(FSortType,sizeof(FSortType));
   Stream.Write(FShowAction,sizeof(FShowAction));
   for i:= 0 to 4 do
   begin
      OQBFilter := FFilters[i];
      Stream.Write(OQBFilter.FFilterAction,SizeOf(OQBFilter.FFilterAction));
      WriteString(OQBFilter.FCustomExpres);
      for j := 1 to 5 do
         WriteString(OQBFilter.Data[j]);
   end;
end;

procedure TOQBField.LoadFromStream(Stream:TStream);
var
   i,
   j,
   n : integer;
   s : string;
   OQBFilter : TOQBFilter ;

   function ReadString: String;
   var
      n: integer;
   begin
      Stream.Read(n,sizeof(n));
      Result:='';
      if n > 0 then
      begin
         SetLength(Result,n);
         Stream.Read(Result[1],n);
      end;
   end;

begin
   FTable := ReadString;
   FTableAlias := ReadString;
   FFieldName := ReadString;
   FAlias:= ReadString;
   Stream.Read(FSortType,sizeof(FSortType));
   Stream.Read(FShowAction,sizeof(FShowAction));
   for i:= 0 to 4 do
   begin
      OQBFilter := FFilters[i];
      Stream.Read(OQBFilter.FFilterAction,SizeOf(OQBFilter.FFilterAction));
      OQBFilter.FCustomExpres := ReadString;
      for j:= 1 to 5 do
         OQBFilter.Data[j] := ReadString;
   end;
end;

{ TOQBFieldList }
constructor TOQBFieldList.Create;
begin
   inherited Create;
   FItems := TList.Create;
end;

destructor TOQBFieldList.Destroy;
begin
   Clear;
   FItems.Free;
   inherited Destroy;
end;

function TOQBFieldList.GetCount;
begin
   Result := FItems.Count;
end;

function TOQBFieldList.GetItem(Index: Integer): TOQBField;
begin
   Result := FItems[Index];
end;

function TOQBFieldList.Add: TOQBField;
begin
   Result := TOQBField.Create(Self);
   FItems.Add(Result);
end;

function TOQBFieldList.Insert(Index: Integer): TOQBField;
begin
   Result := TOQBField.Create(Self);
   FItems.Insert(Index,Result);
end;

procedure TOQBFieldList.Clear;
var
   I: Integer;
begin
   for I:= 0 to FItems.Count - 1 do
      TOQBField(FItems[I]).Free;
   FItems.Clear;
end;

procedure TOQBFieldList.Delete(Index: Integer);
begin
   TOQBField(FItems[Index]).Free;
   FItems.Delete(Index);
end;

procedure TOQBFieldList.ColumnMoved(FromIndex,ToIndex: Integer);
var
   P : Pointer;
begin
   P := FItems[FromIndex];
   FItems.Delete(FromIndex);
   FItems.Insert(ToIndex,P);
end;

procedure TOQBFieldList.Exchange(FromIndex,ToIndex: Integer);
begin
   FItems.Exchange(FromIndex,ToIndex);
end;

procedure TOQBFieldList.LoadFromStream(Stream: TStream);
var
   n,
   i : integer;
begin
   Clear;
   Stream.Read(n,sizeof(n));
   for i:= 0 to n - 1 do
      with Add do
         LoadFromStream(Stream);
end;

procedure TOQBFieldList.SaveToStream(Stream: TStream);
var
   i,
   n : integer;
begin
   n := Count;
   Stream.Write(n,sizeof(n));
   for i := 0 to n - 1 do
      Items[i].SaveToStream(Stream);
end;

{ TQueryBuilderDialog}

constructor TOQBuilderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowButtons:=[bOpenDialog, bSaveDialog, bRunQuery];
  FSQL:=TStringList.Create;
end;

destructor TOQBuilderDialog.Destroy;
begin
  if FSQL<>nil then FSQL.Free;
  FOQBEngine:=nil;
  inherited Destroy;
end;

function TOQBuilderDialog.Execute: Boolean;
begin
  Result:=false;
  if (not Assigned(FOQBForm)) and Assigned((FOQBEngine)) then
  begin
    TOQBForm(FOQBForm):=TOQBForm.Create(Application);
    TOQBForm(FOQBForm).QBDialog:=Self;
    TOQBForm(FOQBForm).btnOpen.Visible:=bOpenDialog in FShowButtons;
    TOQBForm(FOQBForm).btnSave.Visible:=bSaveDialog in FShowButtons;
    TOQBForm(FOQBForm).btnResults.Visible:=bRunQuery in FShowButtons;
    { open the database }
    TOQBForm(FOQBForm).OpenDatabase;

    if TOQBForm(FOQBForm).ShowModal=mrOk then
    begin
      FSQL.Assign(TOQBForm(FOQBForm).MemoSQL.Lines);
      Result:=true;
    end;
    OQBEngine.CloseResultQuery;
    FOQBForm.Free;
    FOQBForm:=nil;
  end;
end;

procedure TOQBuilderDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent=FOQBEngine) and (Operation=opRemove) then
    FOQBEngine:=nil;
end;

procedure TOQBuilderDialog.SetOQBEngine(const Value: TOQBEngine);
begin
  if FOQBEngine<>nil then
    FOQBEngine.FOQBDialog:=nil;
  FOQBEngine := Value;
  if FOQBEngine<>nil then
  begin
    FOQBEngine.FOQBDialog:=Self;
    FOQBEngine.FreeNotification(Self);
  end;
end;

procedure TOQBuilderDialog.SetShowButtons(const Value: TOQBbuttons);
begin
  if Value <> FShowButtons then
  begin
    FShowButtons := Value;
  end;
end;

{ TOQBEngine }

constructor TOQBEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableList:=TStringList.Create;
  FAliasList:=TStringList.Create;
  FFieldList:=TStringList.Create;
  FSQL:=TStringList.Create;
  FSQLcolumns:=TStringList.Create;
  FSQLcolumns_table:=TStringList.Create;
  FSQLcolumns_func:=TStringList.Create;
  FSQLfrom:=TStringList.Create;
  FSQLwhere:=TStringList.Create;
  FSQLgroupby:=TStringList.Create;
  FSQLorderby:=TStringList.Create;
  FUseTableAliases:=true;

end;

destructor TOQBEngine.Destroy;
begin
  FSQL.Free;
  FSQLcolumns.Free;
  FSQLcolumns_table.Free;
  FSQLcolumns_func.Free;
  FSQLfrom.Free;
  FSQLwhere.Free;
  FSQLgroupby.Free;
  FSQLorderby.Free;
  FFieldList.Free;
  FAliasList.Free;
  FTableList.Free;
  FreeNotification(Self);

  inherited;
end;

procedure TOQBEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent=FOQBDialog) and (Operation=opRemove) then
    FOQBDialog:=nil;
end;

procedure TOQBEngine.SetDatabaseName(const Value: string);
begin
  TableList.Clear;
  FDatabaseName := Value;
  if ResultQuery.Active then
    ResultQuery.Close;
end;

procedure TOQBEngine.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

procedure TOQBEngine.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TOQBEngine.GenerateAliases;
var
  i,j  : integer;
  s,s1 : string;
begin
  FAliasList.Clear;
  for i:=0 to FTableList.Count-1 do
  begin
    s:=' ';
    s[1]:=FTableList[i][1]; // get the first character [1] of the table name [i]
    if FAliasList.IndexOf(s)=-1 then
      FAliasList.Add(s)
    else
      begin
        j:=1;
        repeat
          Inc(j);
          s1:=s+IntToStr(j);
        until FAliasList.IndexOf(s1)=-1;
        FAliasList.Add(s1);
      end;
  end;
end;

function TOQBEngine.GenerateSQL : string;
var
  s : string;
  i : integer;
  tbl : string;
begin
  SQL.Clear;

  s:='  SELECT ';
  for i:=0 to SQLcolumns.Count-1 do
  begin
    if SQLcolumns_func[i]=EmptyStr then
      s:=s+SQLcolumns[i]                              { a field or expression }
    else
      s:=s+SQLcolumns_func[i]+'('+SQLcolumns[i]+')';  { an aggregate function}
    if (i<SQLcolumns.Count-1) then
      s:=s+', ';
  end;
  SQL.Add(s);

  s:='    FROM ';
  for i:=0 to SQLfrom.Count-1 do
  begin
    s:=s+SQLfrom[i];
    if (i<SQLfrom.Count-1) then
      s:=s+', ';
    if (i=SQLfrom.Count-1) then
    begin
      SQL.Add(s);
      s:='  ';
    end;
  end;

  s:='   WHERE ';
  for i:=0 to SQLwhere.Count-1 do
  begin
    if (i<SQLwhere.Count-1) then
      s:=s+Format('(%s) OR ', [SQLwhere[i]])
    else
      s:=s+Format('(%s) ', [SQLwhere[i]]);
    if (i=SQLwhere.Count-1) then
    begin
      SQL.Add(s);
      s:='  ';
    end;
  end;

  s:='GROUP BY ';
  for i:=0 to SQLgroupby.Count-1 do
  begin
    if (i<SQLgroupby.Count-1) then
      s:=s+SQLgroupby[i]+', '
    else
      s:=s+SQLgroupby[i];
    if (i=SQLgroupby.Count-1) then
    begin
      SQL.Add(s);
      s:='  ';
    end;
  end;

  s:='ORDER BY ';
  for i:=0 to SQLorderby.Count-1 do
  begin
    if (i<SQLorderby.Count-1) then
      s:=s+SQLorderby[i]+', '
    else
      s:=s+SQLorderby[i];
    if (i=SQLorderby.Count-1) then
    begin
      SQL.Add(s);
      s:='  ';
    end;
  end;

  Result:=SQL.Text;
end;

{ TOQBLbx }

constructor TOQBLbx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style:=lbOwnerDrawFixed;
  ParentFont:=false;
  Font.Style:=[];
  Font.Size:=8;
  Color:= clGray;
  FArrBold:=nil;
  FLoading:=false;
end;

destructor TOQBLbx.Destroy;
begin
  if FArrBold<>nil then FreeMem(FArrBold);
  inherited;
end;

function TOQBLbx.GetCheckW: Integer;
begin
  Result:=GetCheckWidth;
end;

procedure TOQBLbx.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  Idx: integer;
begin
  with Message.DrawItemStruct^ do
  begin
    rcItem.Left := rcItem.Left + GetCheckWidth; //*** check
    {$IFDEF LEVEL5}
    State := TOwnerDrawState(LongRec(itemState).Lo);
    {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ENDIF}
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    Idx:=Integer(itemID);
    if (Idx>=0) and (Idx<=Items.Count-1) then
      begin
        {$R-}
        if (FArrBold<>nil) then
          if FArrBold^[Idx]=1 then
            Canvas.Font.Style:=[fsBold];
        Canvas.Font.Color:=clBlack;
        DrawItem(itemID, rcItem, State);
        if (FArrBold<>nil) then
          if FArrBold^[Idx]=1 then
            Canvas.Font.Style:=[];
        {$R+}
      end
    else
      Canvas.FillRect(rcItem);
  end;
end;

procedure TOQBLbx.WMLButtonDown(var Message: TWMLButtonDblClk);
begin
  inherited;
  BeginDrag(false);
end;

procedure TOQBLbx.WMRButtonDown(var Message: TWMLButtonDblClk);
var
  pnt : TPoint;
begin
  inherited;
  pnt.X:=Message.XPos;
  pnt.Y:=Message.YPos;
  pnt:=ClientToScreen(pnt);
  PopupMenu.Popup(pnt.X,pnt.Y);
end;

procedure TOQBLbx.ClickCheck;
var
  iCol : integer;
begin
  inherited;
  if FLoading then
    Exit;
  if Checked[ItemIndex] then
    begin
      TOQBForm(GetParentForm(Self)).QBGrid.Insert(
        TOQBForm(GetParentForm(Self)).QBGrid.ColCount,
        Items[ItemIndex],TOQBTable(Parent).FTableName,TOQBTable(Parent).FTableAlias);
    end
  else
    begin
      iCol:=TOQBForm(GetParentForm(Self)).QBGrid.FindColumn(Items[ItemIndex]);
      while iCol<>-1 do
      begin
        TOQBForm(GetParentForm(Self)).QBGrid.RemoveColumn(iCol);
        iCol:=TOQBForm(GetParentForm(Self)).QBGrid.FindColumn(Items[ItemIndex]);
      end;
    end;
  TOQBForm(GetParentForm(Self)).QBGrid.Refresh; // StringGrid bug
end;

procedure TOQBLbx.AllocArrBold;
begin
  FArrBold:=AllocMem(Items.Count*SizeOf(integer));
end;

procedure TOQBLbx.SelectItemBold(Item:integer);
begin
  if FArrBold<>nil then
  begin
    {$R-}
    if FArrBold[Item]=0 then
      FArrBold^[Item]:=1;
    {$R+}
  end;
end;

procedure TOQBLbx.UnSelectItemBold(Item:integer);
begin
  if FArrBold<>nil then
  begin
    {$R-}
    if FArrBold[Item]=1 then
      FArrBold^[Item]:=0;
    {$R+}
  end;
end;

function TOQBLbx.GetItemY(Item:integer):integer;
begin
  Result:=Item*ItemHeight+ItemHeight div 2 +1;
end;

{ TOQBTable }

constructor TOQBTable.Create(AOwner: TComponent);
var
  mnuArr   : array [1..5] of TMenuItem;
begin
  inherited Create(AOwner);
  Visible:=false;
  ShowHint:=True;
  BevelInner:=bvRaised;
  BevelOuter:=bvRaised;
  BorderWidth:=1;
  FCloseBtn:=TSpeedButton.Create(Self);
  FCloseBtn.Parent:=Self;
  FCloseBtn.Hint:='Close';
  FUnlinkBtn:=TSpeedButton.Create(Self);
  FUnlinkBtn.Parent:=Self;
  FUnlinkBtn.Hint:='Unlink';

  FLbx:=TOQBLbx.Create(Self);
  FLbx.Parent:=Self;
  FLbx.Style:=lbOwnerDrawFixed;
  FLbx.Align:=alBottom;
  FLbx.TabStop:=false;
  FLbx.Visible:=false;

  mnuArr[1]:=NewItem('Select All',0,false,true,_SelectAll,0,'mnuSelectAll');
  mnuArr[2]:=NewItem('Unselect All',0,false,true,_UnSelectAll,0,'mnuUnSelectAll');
  mnuArr[3]:=NewLine;
  mnuArr[4]:=NewItem('Unlink',0,false,true,_UnlinkBtn,0,'mnuUnLink');
  mnuArr[5]:=NewItem('Close',0,false,true,_CloseBtn,0,'mnuClose');
  PopMenu:=NewPopupMenu(Self,'mnu',paLeft,false,mnuArr);
  PopMenu.PopupComponent:=Self;

  FLbx.PopupMenu:=PopMenu;
end;

destructor TOQBTable.Destroy;
begin
  inherited Destroy;
end;

procedure TOQBTable.WMRButtonDown(var Message: TWMLButtonDblClk);
var
  pnt : TPoint;
begin
  inherited;
  pnt.X:=Message.XPos;
  pnt.Y:=Message.YPos;
  pnt:=ClientToScreen(pnt);
  PopMenu.Popup(pnt.X,pnt.Y);
end;

procedure TOQBTable.Paint;
begin
  inherited Paint;
  if TOQBForm(GetParentForm(Self)).QBDialog.OQBEngine.UseTableAliases then
    Canvas.TextOut(4,4,FTableName+' : '+FTableAlias)
  else
    Canvas.TextOut(4,4,FTableName);
end;

function  TOQBTable.GetRowY(FldN:integer):integer;
var
  pnt  : TPoint;
begin
  pnt.X:=FLbx.Left;
  pnt.Y:=FLbx.Top+FLbx.GetItemY(FldN);
  pnt:=Parent.ScreenToClient(ClientToScreen(pnt));
  Result:=pnt.Y;
end;

function TOQBTable.Activate(const ATableName:string; X,Y:Integer):boolean;
var
  i      : integer;
  W,W1   : integer;
  OQBEngine : TOQBEngine;
begin
  Result:=false;
  Top:=Y;
  Left:=X;
  Font.Style:=[fsBold];
  Font.Size:=8;
  Canvas.Font:=Font;
  Hint:=ATableName;

  FTableName:=ATableName;
  FTableAlias:=TOQBForm(GetParentForm(Self)).QBDialog.FOQBEngine.AliasList[
                TOQBForm(GetParentForm(Self)).QBDialog.FOQBEngine.TableList.IndexOf(ATableName)];
  OQBEngine:=TOQBForm(GetParentForm(Self)).QBDialog.FOQBEngine;
  try
    OQBEngine.ReadFieldList(ATableName);
    FLbx.Items.Assign(OQBEngine.FieldList);
  except
    on E:EDatabaseError do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
  end;

  FLbx.AllocArrBold;
  FLbx.ParentFont:=false;
  FLbx.TabStop:=false;
  FLbx.Height:=FLbx.ItemHeight*FLbx.Items.Count+4;

  Height:=FLbx.Height+22;
  W:=110;
  for i:=0 to FLbx.Items.Count-1 do
  begin
    W1:=Canvas.TextWidth(FLbx.Items[i]);
    if W<W1 then
      W:=W1;
  end;
  Width:=W+20+FLbx.GetCheckW; //*** check

  if TOQBForm(GetParentForm(Self)).QBDialog.OQBEngine.UseTableAliases then
    begin
      if Canvas.TextWidth(FTableName+' : '+FTableAlias)>Width-34 then
        Width:=Canvas.TextWidth(FTableName+' : '+FTableAlias)+34
    end
  else
    if Canvas.TextWidth(FTableName)>Width-34 then
      Width:=Canvas.TextWidth(FTableName)+34;

  FLbx.Visible:=true;
  FLbx.OnDragOver:=_DragOver;
  FLbx.OnDragDrop:=_DragDrop;
  FCloseBtn.Top:=4;
  FCloseBtn.Left:=Self.ClientWidth-16;
  FCloseBtn.Width:=12;                        
  FCloseBtn.Height:=12;
  FCloseBtn.Glyph.LoadFromResourceName(HInstance,'CLOSEBMP');;
  FCloseBtn.Margin:=-1;
  FCloseBtn.Spacing:=0;
  FCloseBtn.OnClick:=_CloseBtn;
  FCloseBtn.Visible:=true;
  FCloseBtn.Hint:='Close';
  FCloseBtn.ShowHint:=True;
  FUnlinkBtn.Top:=4;
  FUnlinkBtn.Left:=Self.ClientWidth-16-FCloseBtn.Width;
  FUnlinkBtn.Width:=12;
  FUnlinkBtn.Height:=12;
  FUnlinkBtn.Glyph.LoadFromResourceName(HInstance,'UNLINKBMP');;
  FUnlinkBtn.Margin:=-1;
  FUnlinkBtn.Spacing:=0;
  FUnlinkBtn.OnClick:=_UnlinkBtn;
  FUnlinkBtn.Visible:=true;
  FUnlinkBtn.Hint:= 'Unlink';
  FUnlinkBtn.ShowHint:=True;
  Visible:=true;
  Result:=True;
end;

procedure TOQBTable._CloseBtn(Sender: TObject);
begin
  TOQBArea(Parent).UnlinkTable(Self);
  TOQBForm(GetParentForm(Self)).QBGrid.RemoveColumn4Tbl(FTableName);
  Parent.RemoveControl(Self);
  Free;
end;

procedure TOQBTable._UnlinkBtn(Sender: TObject);
begin
  TOQBArea(Parent).UnlinkTable(Self);
end;

procedure TOQBTable._SelectAll(Sender: TObject);
var
  i : integer;
begin
  if FLbx.Items.Count=1 then
    Exit;
  for i:=1 to (FLbx.Items.Count-1) do
  begin
    FLbx.Checked[i]:=True;
    TOQBForm(GetParentForm(Self)).QBGrid.Insert(
      TOQBForm(GetParentForm(Self)).QBGrid.ColCount,
      FLbx.Items[i],FTableName,FTableAlias);
  end;
end;

procedure TOQBTable._UnSelectAll(Sender: TObject);
var
  i : integer;
begin
  if FLbx.Items.Count=1 then
    Exit;
  for i:=1 to (FLbx.Items.Count-1) do
  begin
    FLbx.Checked[i]:=False;
    TOQBForm(GetParentForm(Self)).QBGrid.RemoveColumn4Tbl(FTableName);
  end;
end;

procedure TOQBTable._DragOver(Sender, Source: TObject; X, Y: Integer;
                            State: TDragState; var Accept: Boolean);
begin
  if (Source is TCustomListBox)
     and
     (TWinControl(Source).Parent is TOQBTable)
  then
    Accept:=true;
end;

procedure TOQBTable._DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  nRow : integer;
  hRow : integer;
begin
  if (Source is TCustomListBox) then
  begin
    if (TWinControl(Source).Parent is TOQBTable) then
      begin
        hRow:=FLbx.ItemHeight;
        if hRow<>0 then
          nRow:=Y div hRow
        else
          nRow:=0;
        if nRow>FLbx.Items.Count-1 then
          nRow:=FLbx.Items.Count-1;
        // handler for target's '*' row
        if nRow=0 then
          Exit;
        // handler for source's '*' row
        if TOQBTable(TWinControl(Source).Parent).FLbx.ItemIndex=0 then
          Exit;
        if Source<>FLbx then
          TOQBArea(Parent).InsertLink(
            TOQBTable(TWinControl(Source).Parent), Self,
            TOQBTable(TWinControl(Source).Parent).FLbx.ItemIndex, nRow)
        else
          begin
            if nRow<>FLbx.ItemIndex then
              TOQBArea(Parent).InsertLink(Self, Self, FLbx.ItemIndex, nRow);
          end;
      end
    else
      if Source=TOQBForm(GetParentForm(Self)).QBTables then
      begin
        X:=X+Left+TWinControl(Sender).Left;
        Y:=Y+Top+TWinControl(Sender).Top;
        TOQBArea(Parent).InsertTable(X,Y);
      end;
  end
end;

procedure TOQBTable.SetParent(AParent: TWinControl);
begin
  if (AParent<>nil) and (not (AParent is TScrollBox)) then
    raise Exception.Create(sNotValidTableParent);
  inherited SetParent(AParent);
end;

procedure TOQBTable.MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  BringToFront;
  if (Button=mbLeft) then
  begin
    SetCapture(Self.Handle);
    ScreenDC:=GetDC(0);

    ClipRect:=Bounds(Parent.Left,Parent.Top,Parent.Width,Parent.Height);
    ClipRect.TopLeft:=Parent.Parent.ClientToScreen(ClipRect.TopLeft);
    ClipRect.BottomRight:=Parent.Parent.ClientToScreen(ClipRect.BottomRight);
    ClipRgn:=CreateRectRgn(ClipRect.Left,ClipRect.Top,ClipRect.Right,ClipRect.Bottom);
    SelectClipRgn(ScreenDC,ClipRgn);
    ClipCursor(@ClipRect);
    OldX:=X;
    OldY:=Y;
    OldLeft:=X;
    OldTop:=Y;
    MoveRect:=Rect(Self.Left,Self.Top,Self.Left+Self.Width,Self.Top+Self.Height);
    MoveRect.TopLeft:=Parent.ClientToScreen(MoveRect.TopLeft);
    MoveRect.BottomRight:=Parent.ClientToScreen(MoveRect.BottomRight);
    DrawFocusRect(ScreenDC,MoveRect);
    Moving:=True;
  end;
end;

procedure TOQBTable.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
  if Moving then
  begin
    DrawFocusRect(ScreenDC,MoveRect);
    OldX:=X;
    OldY:=Y;
    MoveRect:=Rect(Self.Left+OldX-OldLeft,Self.Top+OldY-OldTop,
                   Self.Left+Self.Width+OldX-OldLeft,Self.Top+Self.Height+OldY-OldTop);
    MoveRect.TopLeft:=Parent.ClientToScreen(MoveRect.TopLeft);
    MoveRect.BottomRight:=Parent.ClientToScreen(MoveRect.BottomRight);
    DrawFocusRect(ScreenDC,MoveRect);
  end;
end;

procedure TOQBTable.MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  if Button=mbLeft then
  begin
    ReleaseCapture;
    DrawFocusRect(ScreenDC,MoveRect);
    begin
      if (Self.Left<>Self.Left+X+OldLeft)
          or
         (Self.Top<>Self.Top+Y-OldTop)
      then
      begin
        Self.Visible:=False;
        Self.Left:=Self.Left+X-OldLeft;
        Self.Top:=Self.Top+Y-OldTop;
        Self.Visible:=True;
      end
    end;
    ClipRect:=Rect(0,0,Screen.Width,Screen.Height);
    ClipCursor(@ClipRect);
    DeleteObject(ClipRgn);
    ReleaseDC(0,ScreenDC);
    Moving:=False;
  end;
  TOQBArea(Parent).ReboundLinks4Table(Self);  
end;

{ TOQBLink }

constructor TOQBLink.Create(AOwner: TComponent);
var
  mnuArr   : array [1..4] of TMenuItem;
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  Width:=105;
  Height:=105;
  Rgn:=CreateRectRgn(0,0,Hand,Hand);
  mnuArr[1]:=NewItem('',0,false,false,nil,0,'mnuLinkName');
  mnuArr[2]:=NewLine;
  mnuArr[3]:=NewItem('Link options',0,false,true,TOQBArea(AOwner).SetOptions,0,'mnuOptions');
  mnuArr[4]:=NewItem('Unlink',0,false,true,TOQBArea(AOwner).Unlink,0,'mnuUnlink');
  PopMenu:=NewPopupMenu(Self,'mnu',paLeft,false,mnuArr);
  PopMenu.PopupComponent:=Self;
  Hint := sLinkHint;
  ShowHint := True;
end;

destructor TOQBLink.Destroy;
begin
  DeleteObject(Rgn);
  inherited Destroy;
end;

procedure TOQBLink.Paint;
var
  ArrRgn,
  pntArray : array [1..4] of TPoint;
  ArrCnt   : integer;
begin
  if tbl1<>tbl2 then
    begin
      if ((LnkX=1) and (LnkY=1))
         or
         ((LnkX=4) and (LnkY=2))
      then
      begin
        pntArray[1].X:=0;
        pntArray[1].Y:=Hand div 2;
        pntArray[2].X:=Hand;
        pntArray[2].Y:=Hand div 2;
        pntArray[3].X:=Width-Hand;
        pntArray[3].Y:=Height-Hand div 2;
        pntArray[4].X:=Width;
        pntArray[4].Y:=Height-Hand div 2;
        ArrRgn[1].X:=pntArray[2].X+5;
        ArrRgn[1].Y:=pntArray[2].Y-5;
        ArrRgn[2].X:=pntArray[2].X-5;
        ArrRgn[2].Y:=pntArray[2].Y+5;
        ArrRgn[3].X:=pntArray[3].X-5;
        ArrRgn[3].Y:=pntArray[3].Y+5;
        ArrRgn[4].X:=pntArray[3].X+5;
        ArrRgn[4].Y:=pntArray[3].Y-5;
      end;
      if Width>Hand+Hand2 then
      begin
        if ((LnkX=2) and (LnkY=1))
           or
           ((LnkX=3) and (LnkY=2))
        then
        begin
          pntArray[1].X:=0;
          pntArray[1].Y:=Hand div 2;
          pntArray[2].X:=Hand;
          pntArray[2].Y:=Hand div 2;
          pntArray[3].X:=Width-5;
          pntArray[3].Y:=Height-Hand div 2;
          pntArray[4].X:=Width-Hand;
          pntArray[4].Y:=Height-Hand div 2;
          ArrRgn[1].X:=pntArray[2].X+5;
          ArrRgn[1].Y:=pntArray[2].Y-5;
          ArrRgn[2].X:=pntArray[2].X-5;
          ArrRgn[2].Y:=pntArray[2].Y+5;
          ArrRgn[3].X:=pntArray[3].X-5;
          ArrRgn[3].Y:=pntArray[3].Y+5;
          ArrRgn[4].X:=pntArray[3].X+5;
          ArrRgn[4].Y:=pntArray[3].Y-5;
        end;
        if ((LnkX=3) and (LnkY=1))
           or
           ((LnkX=2) and (LnkY=2))
        then
        begin
          pntArray[1].X:=Width-Hand;
          pntArray[1].Y:=Hand div 2;
          pntArray[2].X:=Width-5;
          pntArray[2].Y:=Hand div 2;
          pntArray[3].X:=Hand;
          pntArray[3].Y:=Height-Hand div 2;
          pntArray[4].X:=0;
          pntArray[4].Y:=Height-Hand div 2;
          ArrRgn[1].X:=pntArray[2].X-5;
          ArrRgn[1].Y:=pntArray[2].Y-5;
          ArrRgn[2].X:=pntArray[2].X+5;
          ArrRgn[2].Y:=pntArray[2].Y+5;
          ArrRgn[3].X:=pntArray[3].X+5;
          ArrRgn[3].Y:=pntArray[3].Y+5;
          ArrRgn[4].X:=pntArray[3].X-5;
          ArrRgn[4].Y:=pntArray[3].Y-5;
        end;
      end
      else
      begin
        if ((LnkX=2) and (LnkY=1))
           or
           ((LnkX=3) and (LnkY=2))
           or
           ((LnkX=3) and (LnkY=1))
           or
           ((LnkX=2) and (LnkY=2))
        then
        begin
          pntArray[1].X:=0;
          pntArray[1].Y:=Hand div 2;
          pntArray[2].X:=Width-Hand2;
          pntArray[2].Y:=Hand div 2;
          pntArray[3].X:=Width-Hand2;
          pntArray[3].Y:=Height-Hand div 2;
          pntArray[4].X:=0;
          pntArray[4].Y:=Height-Hand div 2;
          ArrRgn[1].X:=pntArray[2].X-5;
          ArrRgn[1].Y:=pntArray[2].Y-5;
          ArrRgn[2].X:=pntArray[2].X+5;
          ArrRgn[2].Y:=pntArray[2].Y+5;
          ArrRgn[3].X:=pntArray[3].X+5;
          ArrRgn[3].Y:=pntArray[3].Y+5;
          ArrRgn[4].X:=pntArray[3].X-5;
          ArrRgn[4].Y:=pntArray[3].Y-5;
        end;
      end;
      if ((LnkX=4) and (LnkY=1))
         or
         ((LnkX=1) and (LnkY=2))
      then
      begin
        pntArray[1].X:=Width;
        pntArray[1].Y:=Hand div 2;
        pntArray[2].X:=Width-Hand;
        pntArray[2].Y:=Hand div 2;
        pntArray[3].X:=Hand;
        pntArray[3].Y:=Height-Hand div 2;
        pntArray[4].X:=0;
        pntArray[4].Y:=Height-Hand div 2;
        ArrRgn[1].X:=pntArray[2].X-5;
        ArrRgn[1].Y:=pntArray[2].Y-5;
        ArrRgn[2].X:=pntArray[2].X+5;
        ArrRgn[2].Y:=pntArray[2].Y+5;
        ArrRgn[3].X:=pntArray[3].X+5;
        ArrRgn[3].Y:=pntArray[3].Y+5;
        ArrRgn[4].X:=pntArray[3].X-5;
        ArrRgn[4].Y:=pntArray[3].Y-5;
      end;
    end
  else
    begin
      pntArray[1].X:=0;
      pntArray[1].Y:=Hand div 2;
      pntArray[2].X:=Hand-5;
      pntArray[2].Y:=Hand div 2;
      pntArray[3].X:=Hand-5;
      pntArray[3].Y:=Height-Hand div 2;
      pntArray[4].X:=0;
      pntArray[4].Y:=Height-Hand div 2;
      ArrRgn[1].X:=pntArray[2].X+5;
      ArrRgn[1].Y:=pntArray[2].Y-5;
      ArrRgn[2].X:=pntArray[2].X-5;
      ArrRgn[2].Y:=pntArray[2].Y+5;
      ArrRgn[3].X:=pntArray[3].X-5;
      ArrRgn[3].Y:=pntArray[3].Y+5;
      ArrRgn[4].X:=pntArray[3].X+5;
      ArrRgn[4].Y:=pntArray[3].Y-5;
    end;
  if FLinkOpt > 0 then  { is a geographic link?}
  begin
     Canvas.Pen.Color:=clBlue;
     Canvas.Pen.Width:=2;
  end else
  begin
     Canvas.Pen.Color:=clBlack;
     Canvas.Pen.Width:=1;
  end;
  Canvas.PolyLine(pntArray);
  Canvas.Brush:=Parent.Brush;
  DeleteObject(Rgn);
  ArrCnt:=4;
  Rgn:=CreatePolygonRgn(ArrRgn,ArrCnt,ALTERNATE);
end;

procedure TOQBLink._Click(X,Y:integer);
var
  pnt : TPoint;
begin
  pnt.X:=X;
  pnt.Y:=Y;
  pnt:=ClientToScreen(pnt);
  PopMenu.Popup(pnt.X,pnt.Y);
end;

procedure TOQBLink.CMHitTest(var Message: TCMHitTest);
begin
  if PtInRegion(Rgn,Message.XPos,Message.YPos) then
    Message.Result:=1;
end;

function TOQBLink.ControlAtPos(const Pos: TPoint): TControl;
var
  I      : integer;
  scrnP,
  P      : TPoint;
begin
  scrnP:=ClientToScreen(Pos);
  for I:=Parent.ControlCount-1 downto 0 do
  begin
    Result:=Parent.Controls[I];
    if (Result is TOQBLink) and (Result<>Self) then
      with Result do
      begin
        P := Result.ScreenToClient(scrnP);
        if Perform(CM_HITTEST,0,integer(PointToSmallPoint(P)))<>0 then
          Exit;
      end;
    end;
  Result := nil;
end;

procedure TOQBLink.WndProc(var Message: TMessage);
begin
  if (Message.Msg=WM_RBUTTONDOWN) or (Message.Msg=WM_LBUTTONDOWN)  then
    if not PtInRegion(Rgn,TWMMouse(Message).XPos,TWMMouse(Message).YPos) then
      ControlAtPos(SmallPointToPoint(TWMMouse(Message).Pos))
    else
      _Click(TWMMouse(Message).XPos,TWMMouse(Message).YPos);  
  inherited WndProc(Message);
end;

{ TOQBArea }

procedure TOQBArea.CreateParams(var Params: TCreateParams);
begin
  inherited;
  OnDragOver:=_DragOver;
  OnDragDrop:=_DragDrop;
end;

procedure TOQBArea.SetOptions(Sender: TObject);
var
  AForm : TOQBLinkForm;
  ALink : TOQBLink;
begin
  if TPopupMenu(Sender).Owner is TOQBLink then
  begin
    ALink:=TOQBLink(TPopupMenu(Sender).Owner);
    AForm:=TOQBLinkForm.Create(Application);
    AForm.txtTable1.Caption:=ALink.tbl1.FTableName;
    AForm.txtCol1.Caption:=ALink.fldNam1;
    AForm.txtTable2.Caption:=ALink.tbl2.FTableName;
    AForm.txtCol2.Caption:=ALink.fldNam2;
    AForm.CboOpt.ItemIndex:=ALink.FLinkOpt;
    case ALink.FLinkType of
       0 : AForm.Label2.OnClick(nil);
       1 : AForm.Label3.OnClick(nil);
    end;
    if AForm.ShowModal=mrOk then
    begin
      ALink.FLinkOpt:=AForm.CboOpt.ItemIndex;
      ALink.FLinkType:=AForm.JoinType;
    end;
    AForm.Free;
  end;
end;

procedure TOQBArea.InsertTable(X,Y: Integer);
var
  NewTable: TOQBTable;
begin
  if FindTable(TOQBForm(GetParentForm(Self)).QBTables.Items[TOQBForm(GetParentForm(Self)).QBTables.ItemIndex])<>nil then
  begin
    ShowMessage('This table is already inserted.');
    Exit;
  end;
  NewTable:=TOQBTable.Create(Self);
  NewTable.Parent:=Self;
  try
    NewTable.Activate(TOQBForm(GetParentForm(Self)).QBTables.Items[TOQBForm(GetParentForm(Self)).QBTables.ItemIndex],
                      X,Y);
  except
    NewTable.Free;
  end;
end;

function TOQBArea.InsertLink(_tbl1,_tbl2: TOQBTable; _fldN1,_fldN2: Integer):TOQBLink;
begin
  Result:=TOQBLink.Create(Self);
  with Result do
  begin
    Parent:=Self;
    tbl1:=_tbl1;
    tbl2:=_tbl2;
    fldN1:=_fldN1;
    fldN2:=_fldN2;
    fldNam1:=tbl1.FLbx.Items[fldN1];
    fldNam2:=tbl2.FLbx.Items[fldN2];
  end;
  if FindLink(Result) then
  begin
    ShowMessage('These tables are already linked.');
    Result.Free;
    Result:=nil;
    Exit;
  end;
  with Result do
  begin
    tbl1.FLbx.SelectItemBold(fldN1);
    tbl1.FLbx.Refresh;
    tbl2.FLbx.SelectItemBold(fldN2);
    tbl2.FLbx.Refresh;
    OnDragOver:=_DragOver;
    OnDragDrop:=_DragDrop;
  end;
  ReboundLink(Result);
  Result.Visible:=True;
end;

function TOQBArea.FindTable(TableName:string):TOQBTable;
var
  i : integer;
  TempTable : TOQBTable;
begin
  Result:=nil;
  for i:=ControlCount-1 downto 0 do
    if Controls[i] is TOQBTable then
    begin
      TempTable:=TOQBTable(Controls[i]);
      if (TempTable.FTableName=TableName) then
      begin
        Result:=TempTable;
        Exit;
      end;
    end;
end;

function TOQBArea.FindLink(Link:TOQBLink):boolean;
var
  i : integer;
  TempLink : TOQBLink;
begin
  Result:=false;
  for i:=ControlCount-1 downto 0 do
    if Controls[i] is TOQBLink then
    begin
      TempLink:=TOQBLink(Controls[i]);
      if (TempLink<>Link) then
        if (((TempLink.tbl1=Link.tbl1) and (TempLink.fldN1=Link.fldN1))
            and
           ((TempLink.tbl2=Link.tbl2) and (TempLink.fldN2=Link.fldN2)))
           or
           (((TempLink.tbl1=Link.tbl2) and (TempLink.fldN1=Link.fldN2))
            and
           ((TempLink.tbl2=Link.tbl1) and (TempLink.fldN2=Link.fldN1)))
        then
        begin
          Result:=true;
          Exit;
        end;
    end;
end;

function  TOQBArea.FindOtherLink(Link:TOQBLink;Tbl:TOQBTable;FldN:integer):boolean;
var
  i         : integer;
  OtherLink : TOQBLink;
begin
  Result:=false;
  for i:=ControlCount-1 downto 0 do
    if Controls[i] is TOQBLink then
    begin
      OtherLink:=TOQBLink(Controls[i]);
      if (OtherLink<>Link) then
        if ((OtherLink.tbl1=Tbl) and (OtherLink.fldN1=FldN))
           or
           ((OtherLink.tbl2=Tbl) and (OtherLink.fldN2=FldN))
        then
        begin
          Result:=true;
          Exit;
        end;
    end;
end;

procedure TOQBArea.ReboundLink(Link:TOQBLink);
var
  X1,X2,
  Y1,Y2  : integer;
begin
  Link.PopMenu.Items[0].Caption:=Link.tbl1.FTableName+' :: '+Link.tbl2.FTableName;
  with Link do
  begin
    if Tbl1=Tbl2 then
      begin
        X1:=Tbl1.Left+Tbl1.Width;
        X2:=Tbl1.Left+Tbl1.Width+Hand;
      end
    else
      begin
        if Tbl1.Left<Tbl2.Left then
          begin
            if Tbl1.Left+Tbl1.Width+Hand<Tbl2.Left then
              begin    //A
                X1:=Tbl1.Left+Tbl1.Width;
                X2:=Tbl2.Left;
                LnkX:=1;
              end
            else
              begin    //B
                if Tbl1.Left+Tbl1.Width>Tbl2.Left+Tbl2.Width then
                  begin
                    X1:=Tbl2.Left+Tbl2.Width;
                    X2:=Tbl1.Left+Tbl1.Width+Hand;
                    LnkX:=3;
                  end
                else
                  begin
                    X1:=Tbl1.Left+Tbl1.Width;
                    X2:=Tbl2.Left+Tbl2.Width+Hand;
                    LnkX:=2;
                  end;
              end;
          end
        else
          begin
            if Tbl2.Left+Tbl2.Width+Hand>Tbl1.Left then
              begin    //C
                if Tbl2.Left+Tbl2.Width>Tbl1.Left+Tbl1.Width then
                  begin
                    X1:=Tbl1.Left+Tbl1.Width;
                    X2:=Tbl2.Left+Tbl2.Width+Hand;
                    LnkX:=2;
                  end
                else
                  begin
                    X1:=Tbl2.Left+Tbl2.Width;
                    X2:=Tbl1.Left+Tbl1.Width+Hand;
                    LnkX:=3;
                  end;
              end
            else
              begin    //D
                X1:=Tbl2.Left+Tbl2.Width;
                X2:=Tbl1.Left;
                LnkX:=4;
              end;
          end;
      end;

    Y1:=Tbl1.GetRowY(FldN1);
    Y2:=Tbl2.GetRowY(FldN2);
    if Y1<Y2 then
      begin        //M
        Y1:=Tbl1.GetRowY(FldN1)-Hand div 2;
        Y2:=Tbl2.GetRowY(FldN2)+Hand div 2;
        LnkY:=1;
      end
    else
      begin         //N
        Y2:=Tbl1.GetRowY(FldN1)+Hand div 2;
        Y1:=Tbl2.GetRowY(FldN2)-Hand div 2;
        LnkY:=2;
      end;
    SetBounds(X1,Y1,X2-X1,Y2-Y1);
  end;
end;

procedure TOQBArea.ReboundLinks4Table(ATable:TOQBTable);
var
  i    : integer;
  Link : TOQBLink;
begin
  for i:=0 to ControlCount-1 do
  begin
    if Controls[i] is TOQBLink then
    begin
      Link:=TOQBLink(Controls[i]);
      if (Link.Tbl1=ATable) or (Link.Tbl2=ATable) then
        ReboundLink(Link);
    end;
  end;
end;

procedure TOQBArea.Unlink(Sender: TObject);
var
  Link  : TOQBLink;
begin
  if TPopupMenu(Sender).Owner is TOQBLink then
  begin
    Link:=TOQBLink(TPopupMenu(Sender).Owner);
    RemoveControl(Link);
    if not FindOtherLink(Link,Link.tbl1,Link.fldN1) then
    begin
      Link.tbl1.FLbx.UnSelectItemBold(Link.fldN1);
      Link.tbl1.FLbx.Refresh;
    end;
    if not FindOtherLink(Link,Link.tbl2,Link.fldN2) then
    begin
      Link.tbl2.FLbx.UnSelectItemBold(Link.fldN2);
      Link.tbl2.FLbx.Refresh;
    end;
    Link.Free;
  end;
end;

procedure TOQBArea.UnlinkTable(ATable:TOQBTable);
var
  i    : integer;
  TempLink : TOQBLink;
begin
  for i:=ControlCount-1 downto 0 do
  begin
    if Controls[i] is TOQBLink then
    begin
      TempLink:=TOQBLink(Controls[i]);
      if (TempLink.Tbl1=ATable) or (TempLink.Tbl2=ATable) then
      begin
        RemoveControl(TempLink);
        if not FindOtherLink(TempLink,TempLink.tbl1,TempLink.fldN1) then
        begin
          TempLink.tbl1.FLbx.UnSelectItemBold(TempLink.fldN1);
          TempLink.tbl1.FLbx.Refresh;
        end;
        if not FindOtherLink(TempLink,TempLink.tbl2,TempLink.fldN2) then
        begin
          TempLink.tbl2.FLbx.UnSelectItemBold(TempLink.fldN2);
          TempLink.tbl2.FLbx.Refresh;
        end;
        TempLink.Free;
      end;
    end;
  end;
end;

procedure TOQBArea._DragOver(Sender, Source: TObject; X, Y: Integer;
                            State: TDragState; var Accept: Boolean);
begin
  if (Source=TOQBForm(GetParentForm(Self)).QBTables) then
    Accept:=true;
end;

procedure TOQBArea._DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if not (Sender is TOQBArea) then
  begin
    X:=X+TControl(Sender).Left;
    Y:=Y+TControl(Sender).Top;
  end;
  if Source=TOQBForm(GetParentForm(Self)).QBTables then
    InsertTable(X,Y);
end;

{ TOQBGrid }

constructor TOQBGrid.Create(AOwner: TComponent);
begin
   inherited Create(Aowner);
   FFieldList := TOQBFieldList.Create;
   Options:=[goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goColSizing,goColMoving];
   OnColumnMoved:=_ColumnMoved;
   Hint := 'Double click to edit Field or'+#10#10+'Right click for popup menu';
   ShowHint:=True;
end;

destructor TOQBGrid.Destroy;
begin
   FFieldList.Free;
   inherited Destroy;
end;

procedure TOQBGrid.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   ColCount:=2;
   RowCount:=9;
   Height:=Parent.ClientHeight div 3;
   DefaultRowHeight:=Height div (6+1) - GridLineWidth;
   OnDragOver:=_DragOver;
   OnDragDrop:=_DragDrop;
end;

function TOQBGrid.IsEmpty: Boolean;
begin
   Result:=(FFieldList.Count=0);
end;

procedure TOQBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
   Text: string;
   Justif: Word;
begin
   inherited DrawCell(ACol,ARow,ARect,AState);
   { pendiente aqui }
   with Canvas do
   begin
      if ACol = 0 then
      begin
         Text:= sRows[ARow];
      end else if (FieldList.Count>0) and (ARow <9) then
      begin
         case ARow of
            0 : Text := FieldList[ACol - 1].FieldName;
            1 : Text := FieldList[ACol - 1].Table;
            2 : Text := sShow[FieldList[ACol - 1].ShowAction];
            3 : Text := sSort[FieldList[ACol - 1].SortType];
            4..8 :
               begin
               if Length(FieldList[ACol - 1].Filters[ARow-4].FCustomExpres)>0 then
                  Text := FieldList[ACol - 1].Filters[ARow-4].FCustomExpres
               else
                  Text := FieldList[ACol - 1].Filters[ARow-4].FilterVerb;
               end;
         end;
      end;
      Font.Style:= [];
      Justif:= DT_LEFT;
      DrawText(Handle, PChar(Text), -1, ARect, Justif or DT_SINGLELINE or DT_VCENTER);
   end;
end;

procedure TOQBGrid.WndProc(var Message: TMessage);
begin
  if (Message.Msg=WM_RBUTTONDOWN) then
    ClickCell(TWMMouse(Message).XPos,TWMMouse(Message).YPos);
  inherited WndProc(Message);
end;

procedure TOQBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited MouseDown(Button,Shift,X,Y);
   if (Button=mbLeft) and (ssDouble in Shift) then
      DblClickCell(X,Y);
end;

function TOQBGrid.MaxSW(const s1,s2:string):integer;
begin
  Result:=Canvas.TextWidth(s1);
  if Result<Canvas.TextWidth(s2) then
    Result:=Canvas.TextWidth(s2);
end;

procedure TOQBGrid.ResetWidths;
var
   i: Integer;
begin
   if FieldList.Count > 0 then
   begin
      ColCount:= FieldList.Count+1;
      for i:= 0 to FieldList.Count - 1 do
      ColWidths[i+1]:=MaxSW(FieldList[i].FieldName,FieldList[i].Table)+8;
   end else
   begin
      ColCount:=2;
      ColWidths[1]:= Canvas.TextWidth('WWWWWWWW');
   end;
end;

procedure TOQBGrid.Insert(aCol:integer;const aField,aTable,aTableAlias:string);
var
  i : integer;
  OQBField : TOQBField;
begin
  if FieldList.Count=0 then
    begin
      with FieldList.Add do
      begin
          Table     := aTable;
          TableAlias:= aTableAlias;
          FieldName := aField;
      end;
      aCol:=FieldList.Count;
      Invalidate;
    end
  else
    begin
      if aCol=-1 then
        begin
          with FieldList.Add do
          begin
              Table     := aTable;
              TableAlias:= aTableAlias;
              FieldName := aField;
          end;
          aCol:=FieldList.Count;
        end
      else
        begin
          FieldList.Insert(aCol-1);
          {FieldList.Add;
          for i:=FieldList.Count-1 downto aCol+1 do
            FieldList.Exchange(i-1,i); }
          FieldList[aCol-1].FieldName:=aField;
          FieldList[aCol-1].Table:=aTable;
          FieldList[aCol-1].TableAlias:=aTableAlias;
        end;
      //* Fix StringGrid Bug *
        {if aCol>1 then
          ColWidths[aCol-1]:=MaxSW(FieldList[aCol-2].FieldName,FieldList[aCol-2].Table)+8;
        if aCol<ColCount-1 then
          ColWidths[aCol+1]:=MaxSW(FieldList[aCol].FieldName,FieldList[aCol].Table)+8;
        ColWidths[ColCount-1]:=MaxSW(FieldList[ColCount-2].FieldName,FieldList[ColCount-2].Table)+8; }
      end;
  if FieldList.Count=0 then
     ColCount := 2
  else
     ColCount := FieldList.Count + 1;
  ColWidths[aCol]:=MaxSW(aTable,aField)+8;
  Invalidate;
end;

procedure TOQBGrid._ColumnMoved(Sender: TObject; FromIndex,ToIndex: Integer);
begin
   FieldList.ColumnMoved(FromIndex-1,ToIndex-1);
   Invalidate;
end;

function TOQBGrid.FindColumn(const sCol:string):integer;
var
  i : integer;
begin
  Result:=-1;
  for i:=0 to FieldList.Count-1 do
     if FieldList[i].Fieldname=sCol then
     begin
        Result:=i+1;
        Exit;
     end;
end;

function TOQBGrid.FindSameColumn(aCol:integer):boolean;
var
  i : integer;
begin
  Result:=false;
  for i:=0 to FieldList.Count-1 do
  begin
    if i=aCol-1 then
      Continue
    else
      if FieldList[i].FieldName=FieldList[aCol-1].FieldName then
      begin
        Result:=True;
        Exit;
      end;
  end;
end;

procedure TOQBGrid.RemoveColumn(aCol:integer);
var
  i  : integer;
begin
  if (FieldList.Count>0) then
  begin
      FieldList.Delete(aCol-1);
      if FieldList.Count=0 then
         ColCount:=2
      else
         ColCount:=FieldList.Count+1;
  end;
end;

procedure TOQBGrid.RemoveColumn4Tbl(const Tbl:string);
var
  i     : integer;
  found : boolean;
begin
  repeat
    found:= false;
    for i:=0 to FieldList.Count - 1 do
       if FieldList[i].Table=Tbl then
       begin
          RemoveColumn(i+1);
          found:= true;
          Break;
       end;
  until not found;
end;

procedure TOQBGrid.ClickCell(X,Y:integer);
var
  P     : TPoint;
  mCol,
  mRow  : integer;
begin
  MouseToCell(X,Y,mCol,mRow);
  CurrCol:=mCol;
  P.X:=X;
  P.Y:=Y;
  P:=ClientToScreen(P);
  if (mCol>0) and (mCol<=FieldList.Count) and (not IsEmpty) then
  begin
    if (FieldList[mCol-1].FieldName='*') and (mRow<>0) then
      Exit;
    TOQBForm(GetParentForm(Self)).mnuTbl.Popup(P.X,P.Y);
  end;
end;

procedure TOQBGrid.DblClickCell(X,Y:integer);
var
  P     : TPoint;
  mCol,
  mRow  : integer;
begin
  MouseToCell(X,Y,mCol,mRow);
  CurrCol:=mCol;
  if (mCol>0) and (mCol<=FieldList.Count) and (not IsEmpty) then
  begin
    if (FieldList[mCol-1].FieldName='*') and (mRow<>0) then
      Exit;
    with TfrmEdQBField.Create(Application) do
    begin
       try
          if Enter((GetParentForm(Self) as TOQBForm).QBDialog.OQBEngine.ResultQuery,
                  FieldList[mCol-1],
            IMax(mRow-4,0)) = mrOk then
             Self.Invalidate;
       finally
          Free;
       end;
    end;
  end;
end;

procedure TOQBGrid._DragOver(Sender, Source: TObject; X, Y: Integer;
                            State: TDragState; var Accept: Boolean);
begin
  if (Source<>TOQBForm(GetParentForm(Self)).QBTables) then
    Accept:=true;
end;

procedure TOQBGrid._DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dCol,
  dRow    : integer;
begin
  if ((Source is TOQBLbx) and
      (Source<>TOQBForm(GetParentForm(Self)).QBTables))
  then
  begin
    TOQBTable(TWinControl(Source).Parent).FLbx.Checked[TOQBTable(TWinControl(Source).Parent).FLbx.ItemIndex]:=True;//*** check
    MouseToCell(X,Y,dCol,dRow);
    if dCol=0 then
      Exit;
    Insert(dCol, TOQBTable(TWinControl(Source).Parent).FLbx.Items[TOQBTable(TWinControl(Source).Parent).FLbx.ItemIndex],
                 TOQBTable(TWinControl(Source).Parent).FTableName,
                 TOQBTable(TWinControl(Source).Parent).FTableAlias);
  end;
end;


{ TOQBForm }

procedure TOQBForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  QBArea:=TOQBArea.Create(Self);
  QBArea.Parent:=TabColumns;
  QBArea.Align:=alClient;
  QBArea.Color:=clSilver;//clAqua;
  {QBGrid:=TOQBGrid.Create(Self);
  QBGrid.Parent:=TabColumns;
  VSplitter.Tag:=VSplitter.Left;
  HSplitter.Tag:=HSplitter.Top;
  HSplitter.Align:=alBottom;
  HSplitter.Top:=0;}        { just a trick }
  //QBGrid.Align:=alBottom;  //alClient;
end;

procedure TOQBForm.mnuRemoveClick(Sender: TObject);
var
  TempTable : TOQBTable;
begin
  TempTable:=QBArea.FindTable(QBGrid.FieldList[QBGrid.CurrCol-1].Table);
  if not QBGrid.FindSameColumn(QBGrid.CurrCol) then
    TempTable.FLbx.Checked[TempTable.FLbx.Items.IndexOf(QBGrid.FieldList[QBGrid.CurrCol-1].FieldName)]:=false;
  QBGrid.RemoveColumn(QBGrid.CurrCol);
  QBGrid.Refresh; // fix for StringGrid bug
end;

procedure TOQBForm.ClearAll;
var
  i : integer;
  TempTable : TOQBTable;
begin
  for i:=QBArea.ControlCount-1 downto 0 do
    if QBArea.Controls[i] is TOQBTable then
      begin
        TempTable:=TOQBTable(QBArea.Controls[i]);
        QBGrid.RemoveColumn4Tbl(TempTable.FTableName);
        TempTable.Free;
      end
    else
      QBArea.Controls[i].Free; // QBLink
  QBGrid.ResetWidths;
  MemoSQL.Lines.Clear;
  QBDialog.OQBEngine.ResultQuery.Close;
  QBDialog.OQBEngine.ClearQuerySQL;
  Pages.ActivePage:=TabColumns;
end;

procedure TOQBForm.btnNewClick(Sender: TObject);
begin
  ClearAll;
end;

procedure TOQBForm.btnOpenClick(Sender: TObject);
var
  i,ii,j,k,n : integer;
  s,ss : string;
  NewTable : TOQBTable;
  TableName : string;
  X,Y : integer;
  NewLink : TOQBLink;
  Table1,Table2 : TOQBTable;
  FieldN1,FieldN2 : integer;
  ColField, ColTable : string;
  StrList : TStringList;
  Stream: TStream;
  found: boolean;

  function GetNextVal(var s:string):string;
  var
    p : integer;
    s1,s2:string;
  begin
    Result:=EmptyStr;
    p:=Pos(',',s);
    if p=0 then
    begin
      p:=Pos(';',s);
      if p=0 then
         Exit;
    end;
    Result:=System.Copy(s,1,p-1);
    System.Delete(s,1,p);
    s1:=result;
    s2:=s;
  end;

begin
  j:=-1;
  if not DlgOpen.Execute then Exit;
  Stream:= TFileStream.Create(DlgOpen.FileName, fmOpenRead or fmShareDenyNone);
  StrList:=TStringList.Create;
  Stream.Read(n,sizeof(n));
  for i:=1 to n do
  begin
     Stream.Read(k,sizeof(k));
     s:='';
     if k >0 then
     begin
        SetLength(s,k);
        Stream.Read(s[1],k);
     end;
     StrList.Add(s);
  end;
  if StrList[0]<>QBSignature then
  begin
    ShowMessage('File '+DlgOpen.FileName+' is not a Query Builder''s file.');
    StrList.Free;
    Exit;
  end;
  ClearAll;
  try
    s:=StrList[2];  // read options
    if s='+' then
      WindowState:=wsMaximized
    else
      begin
        WindowState:=wsNormal;
        Top:=StrToInt(GetNextVal(s));
        Left:=StrToInt(GetNextVal(s));
        Height:=StrToInt(GetNextVal(s));
        Width:=StrToInt(GetNextVal(s));
      end;
    s:=StrList[3];
    QBTables.Width:=StrToInt(GetNextVal(s));
    Pages.Height:=StrToInt(GetNextVal(s));

    for i:=5 to StrList.Count-1 do  // read tables
    begin
      if StrList[i]='[Links]' then
      begin
        j:=i+1;
        Break;
      end;
      s:=StrList[i];
      TableName:=GetNextVal(s);
      Y:=StrToInt(GetNextVal(s));
      X:=StrToInt(GetNextVal(s));
      NewTable:=TOQBTable.Create(Self);
      NewTable.Parent:=QBArea;
      try
        NewTable.Activate(TableName,X,Y);
        NewTable.FLbx.FLoading:=true;
        for ii:=0 to NewTable.FLbx.Items.Count-1 do
        begin
          ss:=GetNextVal(s);
          if ss<>EmptyStr then
            NewTable.FLbx.Checked[ii]:=boolean(StrToInt(ss));
        end;
        NewTable.FLbx.FLoading:=false;
      except
        NewTable.Free;
      end;
    end;

    if j<>-1 then
      for i:=j to StrList.Count-1 do  // read links
      begin
        if StrList[i]='[Columns]' then
        begin
          j:=i+1;
          Break;
        end;
        s:=StrList[i];
        ss:=GetNextVal(s);
        Table1:=QBArea.FindTable(ss);
        ss:=GetNextVal(s);
        FieldN1:=StrToInt(ss);
        ss:=GetNextVal(s);
        Table2:=QBArea.FindTable(ss);
        ss:=GetNextVal(s);
        FieldN2:=StrToInt(ss);
        NewLink:=QBArea.InsertLink(Table1,Table2,FieldN1,FieldN2);
        ss:=GetNextVal(s);
        NewLink.FLinkOpt:=StrToInt(ss);
        ss:=GetNextVal(s);
        NewLink.FLinkType:=StrToInt(ss);
      end;
    for i:= 0 to StrList.Count - 1 do
       if StrList[i]='[Columns]' then
       begin
         s:=StrList[i+1];
         ss:=GetNextVal(s);
         n:=StrToInt(ss);
         QBGrid.ColCount:= n;
         for k:= 0 to n-1 do
         begin
            ss:=GetNextVal(s);
            QBGrid.ColWidths[k]:=StrToInt(ss);
         end;
         break;
       end;

    if j<>-1 then
    begin
      QBGrid.FieldList.LoadFromStream(Stream);
      QBGrid.ResetWidths;
    end;
  finally
    StrList.Free;
    Stream.Free;
  end;
end;

procedure TOQBForm.btnSaveClick(Sender: TObject);
var
  i,j,k,n : integer;
  s : string;
  TempTable : TOQBTable;
  TempLink : TOQBLink;
  StrList : TStringList;
  Stream: TStream;

  function CreateIfNotExists(Const FileName: String): TStream;
  begin
     if FileExists(FileName) then
        Result := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
     else
        Result := TFileStream.Create(FileName, fmCreate);
  end;

begin
  if not DlgSave.Execute then Exit;
  Stream := CreateIfNotExists(DlgSave.FileName);
  StrList:=TStringList.Create;
  StrList.Add(QBSignature);
  StrList.Add('[Options]');
  if WindowState=wsMaximized then
    s:='+'
  else
    s:=IntToStr(Top)+','+IntToStr(Left)+','+IntToStr(Height)+','+IntToStr(Width)+';';
  StrList.Add(s);
  s:=IntToStr(QBTables.Width)+','+IntToStr(Pages.Height)+';';
  StrList.Add(s);

  StrList.Add('[Tables]');  // save tables
  for i:=0 to QBArea.ControlCount-1 do
    if QBArea.Controls[i] is TOQBTable then
    begin
      TempTable:=TOQBTable(QBArea.Controls[i]);
      s:=TempTable.FTableName+','
         +IntToStr(TempTable.Top+QBArea.VertScrollBar.ScrollPos)+','
         +IntToStr(TempTable.Left+QBArea.HorzScrollBar.ScrollPos);
      for j:=0 to TempTable.FLbx.Items.Count-1 do
        if TempTable.FLbx.Checked[j] then
          s:=s+',1'
        else
          s:=s+',0';
      s:=s+';';
      StrList.Add(s);
    end;

  StrList.Add('[Links]');   // save links
  for i:=0 to QBArea.ControlCount-1 do
    if QBArea.Controls[i] is TOQBLink then
    begin
      TempLink:=TOQBLink(QBArea.Controls[i]);
      s:=TempLink.Tbl1.FTableName+','+IntToStr(TempLink.FldN1)+','+
         TempLink.Tbl2.FTableName+','+IntToStr(TempLink.FldN2)+','+
         IntToStr(TempLink.FLinkOpt)+','+IntToStr(TempLink.FLinkType);
      s:=s+';';
      StrList.Add(s);
    end;
  StrList.Add('[Columns]');
  s:= IntToStr(QBGrid.ColCount)+',';
  for i:= 0 to QBGrid.ColCount - 1 do
  begin
     s:=s+IntToStr(QBGrid.ColWidths[i]);
     if i<QBGrid.ColCount - 1 then
       s:=s+',';
  end;
  s:=s+';';
  StrList.Add(s);

  n:=StrList.Count;
  Stream.Write(n,sizeof(n));
  for i:=0 to n-1 do begin
    s:= StrList[i];
    k:=Length(s);
    Stream.Write(k,sizeof(k));
    if k>0 then
      Stream.Write(s[1],k);
  end;

  { save the columns }
  QBGrid.FieldList.SaveToStream(Stream);

  StrList.Free;
  Stream.Free;
end;

procedure TOQBForm.OpenDatabase;
begin
  try
    QBDialog.OQBEngine.ReadTableList;
    QBDialog.OQBEngine.GenerateAliases;
    QBTables.Items.Assign(QBDialog.OQBEngine.TableList);
    ResDataSource.DataSet:=QBDialog.OQBEngine.ResultQuery;
    Caption:=sMainCaption+' ['+
      ExtractFileName(ChangeFileExt(QBDialog.OQBEngine.ResultQuery.Name,''))+']';
  except
  end;
end;

procedure TOQBForm.btnSQLClick(Sender: TObject);
var
  Lst, Lst1 : TStringList;   // temporary string lists
  i,j,k : integer;
  numjoins : integer;
  found1, found2 : boolean;
  s,ss,ss1 : string;
  joins : string;
  tbl1, tbl2 : string;
  Link : TOQBLink;

  function ExtractName(s:string):string;
  var
    p : integer;
  begin
    Result:=s;
    p:=Pos('.',s);
    if p=0 then
      Exit;
    Result:=System.Copy(s,1,p-1);
  end;

begin
  if QBGrid.IsEmpty then
  begin
    ShowMessage('Columns are not selected.');
    Exit;
  end;
  Lst:=TStringList.Create;

  QBDialog.OQBEngine.SQLcolumns.Clear;
  QBDialog.OQBEngine.SQLcolumns_func.Clear;
  QBDialog.OQBEngine.SQLcolumns_table.Clear;
  QBDialog.OQBEngine.SQLfrom.Clear;
  QBDialog.OQBEngine.SQLwhere.Clear;
  QBDialog.OQBEngine.SQLgroupby.Clear;
  QBDialog.OQBEngine.SQLorderby.Clear;

//  SELECT clause
  with QBGrid do
  begin
    for i:=0 to FieldList.Count-1 do
      if FieldList[i].ShowAction <> saHide then
      begin
        if QBDialog.OQBEngine.UseTableAliases then
          tbl1:=QBDialog.OQBEngine.AliasList[QBDialog.OQBEngine.TableList.IndexOf(FieldList[i].Table)]
        else
          tbl1:=FieldList[i].Table;
        s:=tbl1+'.'+FieldList[i].FieldName;
        if Length(FieldList[i].Alias)>0 then
        begin
          if Pos(#32,FieldList[i].Alias)>0 then
            s := s + ' As [' + FieldList[i].Alias+']'
          else
            s := s + ' As ' + FieldList[i].Alias;
        end;
        Lst.Add(LowerCase(s));
        if FieldList[i].ShowAction in [saSum,saCount,saAverage,saMinimum,saMaximum] then
          s:=sAggregate[FieldList[i].ShowAction]
        else
          s:=EmptyStr;
        if QBDialog.OQBEngine.UseTableAliases then
          QBDialog.OQBEngine.SQLcolumns_table.Add(LowerCase(
            QBDialog.OQBEngine.AliasList[QBDialog.OQBEngine.TableList.IndexOf(FieldList[i].Table)]
          ))
        else
          QBDialog.OQBEngine.SQLcolumns_table.Add(LowerCase(FieldList[i].Table));
        QBDialog.OQBEngine.SQLcolumns_func.Add(s);
      end;
    if Lst.Count=0 then
    begin
      ShowMessage('Columns are not selected.');
      Lst.Free;
      Exit;
    end;
    QBDialog.OQBEngine.SQLcolumns.Assign(Lst);
    Lst.Clear;
  end;

//  FROM clause
  with QBArea do
  begin
    Lst1:=TSTringList.Create;  // tables in joins
    joins := '';  // outer joins
    numjoins := 0;
    for i:=0 to ControlCount-1 do  // search tables for joins
      if Controls[i] is TOQBLink then
      begin
        Link:=TOQBLink(Controls[i]);
        if Link.FLinkType >= 0 then
        begin
          if QBDialog.OQBEngine.UseTableAliases then
            begin
              tbl1:=LowerCase(Link.Tbl1.FTableAlias);
              tbl2:=LowerCase(Link.Tbl2.FTableAlias);
            end
          else
            begin
              tbl1:=LowerCase(Link.Tbl1.FTableName);
              tbl2:=LowerCase(Link.Tbl2.FTableName);
            end;
          found1 := false;
          found2 := false;
          if Lst1.IndexOf(tbl1)=-1 then
            Lst1.Add(tbl1)
          else
            found1 := true;
          if Lst1.IndexOf(tbl2)=-1 then
            Lst1.Add(tbl2)
          else
            found2 := true;
          if QBDialog.OQBEngine.UseTableAliases then
          begin
            if numjoins = 0 then
               joins := joins + LowerCase(Link.Tbl1.FTableName)+' '+tbl1
                        +sInnerOuterJoin[Link.FLinkType]
                        +LowerCase(Link.Tbl2.FTableName)+' '+tbl2+' ON '
                        +'('+tbl1+'.'+LowerCase(Link.FldNam1)
                        +#32 + sLinkOpt[Link.FLinkOpt] + #32
                        +tbl2+'.'+LowerCase(Link.FldNam2)+')'
            else begin
               if found1 and found2 then begin
                  joins := joins + ' AND '
                           +'('+tbl1+'.'+LowerCase(Link.FldNam1)
                           +#32+sLinkOpt[Link.FLinkOpt]+#32
                           +tbl2+'.'+LowerCase(Link.FldNam2)+')' ;
               end else begin
                  joins := joins + sInnerOuterJoin[Link.FLinkType]
                           +LowerCase(Link.Tbl2.FTableName)+' '+tbl2+' ON '
                           +'('+tbl1+'.'+LowerCase(Link.FldNam1)
                           +#32+sLinkOpt[Link.FLinkOpt]+#32
                           +tbl2+'.'+LowerCase(Link.FldNam2)+')' ;
               end;
            end;
            Inc(numjoins);
          end
          else
          begin
            if numjoins = 0 then
               joins := joins +tbl1+sInnerOuterJoin[Link.FLinkType]+tbl2+' ON '
                        +'('+tbl1+'.'+LowerCase(Link.FldNam1)
                        +#32+sLinkOpt[Link.FLinkOpt]+#32
                        +tbl2+'.'+LowerCase(Link.FldNam2)+')'
            else
               joins := joins + sInnerOuterJoin[Link.FLinkType]+tbl2+' ON '
                        +'('+tbl1+'.'+LowerCase(Link.FldNam1)
                        +#32+sLinkOpt[Link.FLinkOpt]+#32
                        +tbl2+'.'+LowerCase(Link.FldNam2)+')' ;
            Inc(numjoins);
          end;
        end;
      end;

    for i:=0 to ControlCount-1 do
      if Controls[i] is TOQBTable then
      begin
        if QBDialog.OQBEngine.UseTableAliases then
          tbl1:=LowerCase(TOQBTable(Controls[i]).FTableAlias)
        else
          tbl1:=LowerCase(TOQBTable(Controls[i]).FTableName);
        if (Lst.IndexOf(tbl1)=-1)
          and
           (Lst1.IndexOf(tbl1)=-1)
        then
          if QBDialog.OQBEngine.UseTableAliases then
            Lst.Add(LowerCase(TOQBTable(Controls[i]).FTableName)+' '+tbl1)
          else
            Lst.Add(tbl1);
      end;

    Lst1.Free;

    QBDialog.OQBEngine.SQLfrom.Text := joins;
    QBDialog.OQBEngine.SQLfrom.AddStrings(Lst);
    Lst.Clear;
  end;

// WHERE clause (the filters)
  with QBGrid do
  begin
     for i := 0 to FieldList.Count - 1 do
     begin
        if QBDialog.OQBEngine.UseTableAliases then
            tbl1:=LowerCase(FieldList[i].FTableAlias)
        else
            tbl1:=LowerCase(FieldList[i].FTable);
        for j:= 0 to 4 do  { the filters }
        begin
           s:='';
           if Length(FieldList[i].Filters[j].FCustomExpres)>0 then
              s := FieldList[i].Filters[j].FCustomExpres
           else
              case FieldList[i].Filters[j].FilterAction of
                faIsEqualTo, faIsLike, faIsNotEqualTo, faIsNotLike :
                  begin
                  s := '';
                  for k:= 1 to 5 do { the data for every filter }
                     if Length(FieldList[i].Filters[j].Data[k])>0 then
                     begin
                        ss := Format( '(%s.%s %s %s)',
                                 [tbl1,LowerCase(FieldList[i].FieldName),
                                  sFilterAction[FieldList[i].Filters[j].FilterAction],
                                  FieldList[i].Filters[j].Data[k]] );
                        if FieldList[i].Filters[j].FilterAction=faIsNotLike then
                           ss:='NOT ' +ss;
                        if Length(s) = 0 then
                           s := ss
                        else
                           s := s + ' OR ' + ss;
                     end;
                  end;
                faIsBetween, faIsNotBetween :
                  begin
                  ss  := FieldList[i].Filters[j].Data[1];
                  ss1 := FieldList[i].Filters[j].Data[2];
                  if (Length(ss)>0) and (Length(ss1)>0) then
                  s := Format('(%s.%s %s %s AND %s)', [tbl1,LowerCase(FieldList[i].FieldName),
                           sFilterAction[FieldList[i].Filters[j].FilterAction],
                           ss, ss1]);
                  if FieldList[i].Filters[j].FilterAction=faIsNotBetween then
                     s:= 'NOT '+s;
                  end;
                faIsGreaterThan, faIsGreaterEqualTo, faIsLessThan, faIsLessEqualTo:
                  begin
                  ss:=FieldList[i].Filters[j].Data[1];
                  if Length(ss)>0 then
                     s := Format('(%s.%s %s %s)', [tbl1,LowerCase(FieldList[i].FieldName),
                              sFilterAction[FieldList[i].Filters[j].FilterAction],
                              ss]);
                  end;
              end;
           if Length(s)>0 then
             Lst.Add(s);
        end;
     end;
     QBDialog.OQBEngine.SQLwhere.Assign(Lst);
     Lst.Clear;
  end;

// GROUP BY clause
  with QBGrid do
  begin
    for i:=0 to FieldList.Count-1 do
    begin
      if FieldList[i].ShowAction in [saGroup] then
      begin
        if QBDialog.OQBEngine.UseTableAliases then
          tbl1:=QBDialog.OQBEngine.AliasList[QBDialog.OQBEngine.TableList.IndexOf(FieldList[i].Table)]
        else
          tbl1:=FieldList[i].Table;
        s:=tbl1+'.'+FieldList[i].FieldName;
        Lst.Add(LowerCase(s));
      end;
    end;
    QBDialog.OQBEngine.SQLgroupby.Assign(Lst);
    Lst.Clear;
  end;

// ORDER BY clause
  with QBGrid do
  begin
    for i:=0 to FieldList.Count-1 do
    begin
      if (FieldList[i].SortType in [stAsc,stDesc]) and
         not (FieldList[i].ShowAction in [saHide]) then
      begin
        if QBDialog.OQBEngine.UseTableAliases then
          tbl1:=QBDialog.OQBEngine.AliasList[QBDialog.OQBEngine.TableList.IndexOf(FieldList[i].Table)]
        else
          tbl1:=FieldList[i].Table;
        // --- to order result set by the result of an aggregate function
        if FieldList[i].ShowAction in [saSum,saCount,saAverage,saMinimum,saMaximum] then
          s:=LowerCase(tbl1+'.'+FieldList[i].FieldName)
        else
          s:=IntToStr(i+1);
        // ---

        if FieldList[i].SortType=stDesc then
          s:=s+' DESC';
        Lst.Add(s);
      end;
    end;
    QBDialog.OQBEngine.SQLorderby.Assign(Lst);
    Lst.Clear;
  end;

  MemoSQL.Lines.Text:=QBDialog.OQBEngine.GenerateSQL;
  Pages.ActivePage:=TabSQL;
  Lst.Free;
end;

procedure TOQBForm.btnResultsClick(Sender: TObject);
begin
  QBDialog.OQBEngine.CloseResultQuery; // OQB 4.0a
  QBDialog.OQBEngine.SetQuerySQL(MemoSQL.Lines.Text);
  QBDialog.OQBEngine.OpenResultQuery;
  Pages.ActivePage:=TabResults;
  If Assigned(Pages.OnChange) Then
    Pages.OnChange(nil);
  ResdataSource.DataSet := QBDialog.OQBEngine.ResultQuery;
end;

procedure TOQBForm.btnColorSetClick(Sender: TObject);
begin
  SyntaxHighlighter1.EditColorSet;
end;

procedure TOQBForm.btnExportClick(Sender: TObject);
begin
   ShowMessage('Method not implemented');
end;

procedure TOQBForm.FormShow(Sender: TObject);
begin
  QBArea := TOQBArea.Create( Self );
  QBArea.Parent := TabColumns;
  QBArea.Align := alClient;
  QBArea.Color := clSilver; //clAqua;
  QBGrid := TOQBGrid.Create( Self );
  QBGrid.Parent := TabColumns;
  VSplitter.Tag := VSplitter.Left;
  HSplitter.Tag := HSplitter.Top;
  HSplitter.Align := alBottom;
  HSplitter.Top := 0; { just a trick }
  QBGrid.Align := alBottom; //alClient;
//{$ENDIF}
  //Pages.OnChange( Sender );
end;

end.
