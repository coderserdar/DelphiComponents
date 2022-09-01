   //=======================================================//
  //  SQL Builder, Written By Viktor Ismagilov (Immortal)  //
 //=======================================================//
unit VQBuilder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, CheckLst, ComCtrls,
  VQBField, contnrs, ExtCtrls, Menus, Math, ToolWin,
  Grids, DBGrids, VQBLink, ImgList, VQBConst, ActnList, VQBAbout,
  ErrorDlg, VQBLocalize, DBCtrls, Buttons;

type
   //Load and save events
   TSaveEvent = Procedure(Sender: TObject; Content: String) of object;
   TLoadEvent = Procedure(Sender: TObject; var Content: String) of object;

   //forward declarations
   TSQLForm = class;
   TDBEngine = class;

  //TSQLDialog
  TSQLDialog = class(TComponent)
  Private
    FDBEngine: TDBEngine;
    FSQLForm: TForm;
    FResultSQL: String;
    FSaveEvent: TSaveEvent;
    FLoadEvent: TLoadEvent;
    procedure SetDBEngine(const Value: TDBEngine);
    procedure SetResultSQL(const Value: String);
  Protected

  Public
     Constructor Create(AOwner: TComponent); override;
     Destructor Destroy; override;
     Function Execute: Boolean;
     Property ResultSQL: String read FResultSQL write SetResultSQL;
  Published
     Property DBEngine: TDBEngine read FDBEngine write SetDBEngine;
     Property OnSaveModel: TSaveEvent read FSaveEvent write FSaveEvent;
     Property OnLoadModel: TLoadEvent read FLoadEvent write FLoadEvent;
  end;

  //TDBEngine;
  TDBEngine = class(TComponent)
  Private
     FTableList: TStringList;
     FFieldList: TStringList;
     FSystemList: TStringList;
     FSQLDialog: TSQLDialog;

     FUDFList: TStringList;

     procedure SetFieldList(const Value: TStringList);
     procedure SetTableList(const Value: TStringList);
     procedure SetSystemList(const Value: TStringList);
    procedure SetUDFList(const Value: TStringList);
  Protected
     Property TableList: TStringList read FTableList write SetTableList;
     Property FieldList: TStringList read FFieldList write SetFieldList;
     Property SystemList: TStringList read FSystemList write SetSystemList;
     Property UDFList: TStringList read FUDFList write SetUDFList;
     Procedure ReadTableList; virtual; abstract;
     procedure SetQuerySQL(const Value: string); virtual; abstract;
     Procedure ReadFieldList(const ATableName: string); virtual; abstract;
     function ResultQuery: TDataSet; virtual; abstract;
     procedure OpenResultQuery; virtual; abstract;
     procedure CloseResultQuery; virtual; abstract;
     procedure ClearQuerySQL; virtual; abstract;
     Function ConvertTableName(const AName: string): string; virtual; abstract;
     Function ConvertFieldName(const AName: string): string; virtual; abstract;
     Function FindFullName(ListBox: TCheckListBoxEx): String; virtual; abstract;
  Public
     Constructor Create(AOwner: TComponent); override;
     Destructor Destroy; override;
  end;

  TSQLGrid = class(TStringGrid)
  Private
     FEmpty: Boolean;
  Protected
     Procedure MouseMessage(var Message: TWMMouse); message WM_RBUTTONDOWN;
     Procedure CreateWnd; override;
  Public
     AColumn: Integer;
     procedure CreateParams(var Params: TCreateParams); override;
     Procedure AddColumn(TableName, FieldName: String); overload;
     Procedure AddColumn; overload;
     Function FindMaxDefWidth: Integer;
     Function FindMaxWidth(S1, S2: String):Integer;
     Function FindColumn(TableName, FieldName: String): Integer;
     Procedure RemoveColumn(Column: Integer);
     Procedure ClickCell(X, Y: Integer);
     Procedure ClearGrid;
     Procedure Realign;
     Function CanGenerateSQL: Boolean;
  end;

  TExpEdit = class(TInplaceEditList);

  TGridEvent = Procedure (Sender: TObject; ACol, ARow: Integer) of object;

  TCriteriaGrid = class(TStringGrid)
  Private
     FGridEvent: TGridEvent;
  Protected
     procedure CreateParams(var Params: TCreateParams); override;
     Procedure CreateWnd; override;
     Procedure WMSize(var Message: TMessage); message WM_SIZE;
     function CreateEditor: TInplaceEdit; override;
     function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
     Procedure _OnClick(Sender: TObject);
  Public
     Constructor Create(AOwner: TComponent); override;
     Destructor Destroy; override;
     Procedure ClearGrid;
     Property OnButtonClick: TGridEvent read FGridEvent write FGridEvent;
  end;

  TScrollBoxEx = class(TScrollBox)
  private
    FCanvas: TCanvas;
    FPaint: TNotifyEvent;
  Protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    property OnPaint: TNotifyEvent read FPaint write FPaint;
  end;

  TSQLForm = class(TForm)
    PopupMenu2: TPopupMenu;
    BreakConnect1: TMenuItem;
    JoinParams1: TMenuItem;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ImageList1: TImageList;
    ListBox1: TListBox;
    N1: TMenuItem;
    mnuShow: TPopupMenu;
    Show: TMenuItem;
    mnuFunc: TPopupMenu;
    NoFunction: TMenuItem;
    N3: TMenuItem;
    Average: TMenuItem;
    Count: TMenuItem;
    Maximum: TMenuItem;
    Minimum: TMenuItem;
    Sum: TMenuItem;
    mnuGroup: TPopupMenu;
    Group: TMenuItem;
    DataSource1: TDataSource;
    SaveDlg: TSaveDialog;
    OpenDlg: TOpenDialog;
    tbMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ActionList: TActionList;
    acNew: TAction;
    acFileOpen: TAction;
    OpenQuery1: TMenuItem;
    acFileSave: TAction;
    SaveQuery1: TMenuItem;
    acGenerateSQL: TAction;
    acRunSQL: TAction;
    Query1: TMenuItem;
    GenerateSQL1: TMenuItem;
    RunSQL1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton12: TToolButton;
    acTableView: TAction;
    acAccept: TAction;
    ToolButton13: TToolButton;
    Panel2: TPanel;
    CheckBox1: TCheckBox;
    TabSheet4: TTabSheet;
    Memo1: TMemo;
    Panel4: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    TabSheet5: TTabSheet;
    Panel3: TPanel;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Panel6: TPanel;
    ListBox2: TListBox;
    Panel5: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    acSortAdd: TAction;
    acSortRemove: TAction;
    acSortAZ: TAction;
    acSortZA: TAction;
    acSortUp: TAction;
    acSortDown: TAction;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure BreakConnect1Click(Sender: TObject);
    procedure JoinParams1Click(Sender: TObject);
    procedure ShowClick(Sender: TObject);
    procedure NoFunctionClick(Sender: TObject);
    procedure GroupClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acNewUpdate(Sender: TObject);
    procedure acFileSaveUpdate(Sender: TObject);
    procedure acGenerateSQLExecute(Sender: TObject);
    procedure acGenerateSQLUpdate(Sender: TObject);
    procedure acRunSQLExecute(Sender: TObject);
    procedure acRunSQLUpdate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure acTableViewExecute(Sender: TObject);
    procedure acAcceptExecute(Sender: TObject);
    procedure acAcceptUpdate(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure TabSheet3Resize(Sender: TObject);
    procedure acSortAddExecute(Sender: TObject);
    procedure acSortAddUpdate(Sender: TObject);
    procedure acSortRemoveExecute(Sender: TObject);
    procedure acSortRemoveUpdate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure acSortAZExecute(Sender: TObject);
    procedure acSortAZUpdate(Sender: TObject);
    procedure acSortZAExecute(Sender: TObject);
    procedure acSortZAUpdate(Sender: TObject);
    procedure acSortUpExecute(Sender: TObject);
    procedure acSortUpUpdate(Sender: TObject);
    procedure acSortDownExecute(Sender: TObject);
    procedure acSortDownUpdate(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
  private
    { Private declarations }
    SQLGrid: TSQLGrid;
    WorkArea: TScrollBoxEx;
    ExpGrid: TCriteriaGrid;
  Protected
  public
    { Public declarations }
    SQLDialog: TSQLDialog;
    SelectedLine: Integer;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure OnChangeState(Sender: TObject);
    Procedure UpdateLine(Sender: TObject);
    Procedure DoRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
    Procedure GridButtonClick(Sender: TObject; ACol, ARow: Integer);
    Procedure UpdateExpGrid;
    Procedure DoCheck(Sender: TObject; const Item: Integer; Checked: Boolean);
    Procedure DoShowHint(var HintStr: String;
      var CanShow: Boolean; var HintInfo: THintInfo);
    Procedure DoFormDestroy(Sender: TObject);
    Procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Function GetLineAtCursor: Integer;
    Function GetItemAtCursor(Sender: TCheckListBoxEx): Integer;
    Function GetSelItem(Source: TListBox): Integer;
    Function GetCheckItem(Source: TCheckListBox): Integer;
    Procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Function FindNewAlias(Obj: TCheckListBoxEx): String;
    Function GenerateSQL: String;
    Function LinkExist(Source, Sender: TObject): Boolean;
    Function AnotherLinkExist(Source: TObject; Index: Integer): Boolean;
  Published
    Procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    Procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    Procedure FillForm(var obj: TCheckListBoxEx; const TableName: string);
  end;

 { лас, содержащий номера items'ов ListBox'ов между которыми нат€гиваетс€
 нитка, а также ссылки на ќбъекты этих классов и их названи€}
  TLink = class
  Public
    SourceIndex,
    DestIndex: Integer;
    SourceName, DestName: string;
    SourceCoords,
    DestCoords: TPoint;
    FLinkType: Integer;
    FJoinType: Integer;

    SourceList,
    DestList: TCheckListBoxEx;
    Constructor Create;
  end;

  //Wrapper TObjectList for TLink
  TLinkList = class(TObjectList)
  Private
    Procedure Put(Index: Integer; Item: TLink);
    Function Get(Index: Integer): TLink;
  Public
    Property Items[Index: Integer]: TLink read Get write Put; default;
    Function Add(Obj: TLink): Integer;
    Procedure Insert(Index: Integer; Obj: TLink);
  end;

  //Wrapper TObjectList for TFieldForm
  TFormList = class(TObjectList)
  Private
    Procedure Put(Index: Integer; Item: TCheckListBoxEx);
    Function Get(Index: Integer): TCheckListBoxEx;
  Public
    Property Items[Index: Integer]: TCheckListBoxEx read Get write Put; default;
    Function Add(Obj: TCheckListBoxEx): Integer;
    Procedure Insert(Index: Integer; Obj: TCheckListBoxEx);
  end;


const
   //Link Types
   LinkType: array[0..5] of string = ('=', '>', '<', '>=', '<=', '<>');
   JoinType: array[0..3] of string = ('INNER JOIN', 'LEFT OUTER JOIN',
                                      'RIGHT OUTER JOIN', 'FULL OUTER JOIN');

   SaveID = '[#SQLBuilder#]';  //save file identifier
   SaveIDMAXVER = '1';         //save file current revision

   ExpParams: Array[0..3] of String = (' AND ', ' OR ', ' OR ', ' AND ');

var
  SQLForm: TSQLForm;
  FormList: TFormList;
  LinkList: TLinkList;
  
implementation
uses ExpressBuilder;

{$R *.dfm}

{$R GR_RESOURCE.res}

{ TSQLDialog }

constructor TSQLDialog.Create(AOwner: TComponent);
begin
  inherited;
  FSQLForm:= TSQLForm.Create(nil);
end;

destructor TSQLDialog.Destroy;
begin
  FSQLForm.Free;
  FSQLForm:= nil;
  inherited;
end;

function TSQLDialog.Execute: Boolean;
begin
  TSQLForm(FSQLForm):= TSQLForm.Create(Application);
  TSQLForm(FSQLForm).SQLDialog:= Self;
  Result:= TSQLForm(FSQLForm).ShowModal = mrOK;
  FSQLForm.Free;
  FSQLForm:= nil;
end;

procedure TSQLDialog.SetDBEngine(const Value: TDBEngine);
begin
   if FDBEngine <> nil then FDBEngine.FSQLDialog:= nil;
   FDBEngine:= Value;
   if FDBEngine <> nil then FDBEngine.FSQLDialog:= Self;
end;

procedure TSQLDialog.SetResultSQL(const Value: String);
begin
  FResultSQL := Value;
end;

{ TDBEngine }

constructor TDBEngine.Create(AOwner: TComponent);
begin
  inherited;
  FTableList:= TStringList.Create;
  FFieldList:= TStringList.Create;
  FSystemList:= TStringList.Create;

  FUDFList:= TStringList.Create;
end;

destructor TDBEngine.Destroy;
begin
  FTableList.Free;
  FFieldList.Free;
  FSystemList.Free;

  FUDFList.Free;
  inherited;
end;

procedure TDBEngine.SetFieldList(const Value: TStringList);
begin
  FFieldList := Value;
end;

procedure TDBEngine.SetSystemList(const Value: TStringList);
begin
  FSystemList := Value;
end;

procedure TDBEngine.SetTableList(const Value: TStringList);
begin
  FTableList := Value;
end;

procedure TDBEngine.SetUDFList(const Value: TStringList);
begin
  FUDFList := Value;
end;

{ TSQLGrid }

procedure TSQLGrid.AddColumn(TableName, FieldName: String);
var
  Column: Integer;
begin
   if FEmpty then
   begin
      FEmpty:= False;
      Column:= 1;
      Cells[Column, ResTableID]:= TableName;
      Cells[Column, ResFieldID]:= FieldName;
      Cells[Column, ResShowID]:= resGridVisible;
      ColWidths[Column]:= FindmaxWidth(TableName, FieldName);
   end
     else
   begin
      ColCount:= ColCount + 1;
      Column:= ColCount - 1;
      Cells[Column, ResTableID]:= TableName;
      Cells[Column, ResFieldID]:= FieldName;
      Cells[Column, ResShowID]:= resGridVisible;
      ColWidths[Column]:= FindmaxWidth(TableName, FieldName);
   end;

end;

procedure TSQLGrid.AddColumn;
begin
   FEmpty:= False;
   ColCount:= ColCount + 1;
end;

function TSQLGrid.CanGenerateSQL: Boolean;
var
  i: integer;
begin
   Result:= False;
   for i:= 1 to ColCount - 1 do
      if Cells[i, 2] <> EmptyStr then
      begin
         Result:= True;
         exit;
      end;
end;

procedure TSQLGrid.ClearGrid;
var
  i: integer;
begin
   for i:= ColCount - 1 downto 1 do RemoveColumn(i);
   Realign;
   FEmpty:= True;
end;

procedure TSQLGrid.ClickCell(X, Y: Integer);
var
  P: TPoint;
  ACol, ARow: Integer;
begin
   MouseToCell(X, Y, ACol, ARow);
   AColumn:= Acol;
   P.X:= X;
   P.Y:= Y;
   P:= ClientToScreen(P);
   if (ACol > 0) and (ACol < ColCount) and (not FEmpty) then
   begin
      case ARow of
         ResShowID:
           begin
              TSQLForm(GetParentForm(Self)).mnuShow.Items[0].Checked:= Cells[ACol, ARow] = resGridVisible;
              TSQLForm(GetParentForm(Self)).mnuShow.Popup(P.X, P.Y);
           end;
         ResFunctionID:
           begin
              if Cells[ACol, ResFunctionID] = EmptyStr then
                 TSQLForm(GetParentForm(Self)).mnuFunc.Items[0].Checked:= True
              else if Cells[ACol, ResFunctionID] = ResFuncOrder[2] then
                 TSQLForm(GetParentForm(Self)).mnuFunc.Items[2].Checked:= True
              else if Cells[ACol, ResFunctionID] = ResFuncOrder[3] then
                 TSQLForm(GetParentForm(Self)).mnuFunc.Items[3].Checked:= True
              else if Cells[ACol, ResFunctionID] = ResFuncOrder[4] then
                 TSQLForm(GetParentForm(Self)).mnuFunc.Items[4].Checked:= True
              else if Cells[ACol, ResFunctionID] = ResFuncOrder[5] then
                 TSQLForm(GetParentForm(Self)).mnuFunc.Items[5].Checked:= True
              else  TSQLForm(GetParentForm(Self)).mnuFunc.Items[6].Checked:= True;
              TSQLForm(GetParentForm(Self)).mnuFunc.Popup(P.X, P.Y);
           end;
         ResGroupID:
           begin
              TSQLForm(GetParentForm(Self)).mnuGroup.Items[0].Checked:= Cells[ACol, ARow] <> EmptyStr;
              TSQLForm(GetParentForm(Self)).mnuGroup.Popup(P.X, P.Y);
           end;
      end;
   end;
end;

procedure TSQLGrid.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goColSizing, goColMoving];
  ColCount := 2;
  RowCount := 5;
  Height := Parent.ClientHeight;
  Cells[0, ResTableID] := resGridTable;
  Cells[0, ResFieldID] := resGridField;
  Cells[0, ResShowID] := resGridShow;
  Cells[0, ResFunctionID] := resGridFunction;
  Cells[0, ResGroupID] := resGridGroup;
  FEmpty := True;
  Align:= alClient;
end;

procedure TSQLGrid.CreateWnd;
begin
  inherited;
  DefaultRowHeight:= Canvas.TextHeight('Cg') + 5;
  ColWidths[0]:= FindMaxDefWidth + 5;
end;

function TSQLGrid.FindColumn(TableName, FieldName: String): Integer;
var
  i: integer;
begin
   Result:= - 1;
   for i:= 1 to ColCount - 1 do
   begin
      if (Cells[i, 1] = FieldName) and (cells[i, 0] = TableName) then
      begin
         Result:= i;
         exit;
      end;
   end;
end;

function TSQLGrid.FindMaxWidth(S1, S2: String): Integer;
begin
   with Canvas do
     if TextWidth(S1) > TextWidth(S2) then Result:= TextWidth(S1) + 5
     else Result:= TextWidth(S2) + 5;
end;

function TSQLGrid.FindMaxDefWidth: Integer;
var
  i, DefW: integer;
begin
   DefW:= 0;
   for i:= 0 to RowCount - 1 do
     if DefW <  Canvas.TextWidth(Cells[0, i]) then DefW:= Canvas.TextWidth(Cells[0, i]);
   Result:= DefW;
end;

procedure TSQLGrid.MouseMessage(var Message: TWMMouse);
begin
   inherited;
   ClickCell(Message.XPos, Message.YPos);
end;

procedure TSQLGrid.Realign;
var
  i: integer;
begin
   for i:= 1 to ColCount - 1 do
   begin
      if Cells[i, 0] <> EmptyStr then
         ColWidths[i]:= FindMaxWidth(Cells[i,0], Cells[i, 1])
      else
         ColWidths[i]:= DefaultColWidth;
   end;
end;

procedure TSQLGrid.RemoveColumn(Column: Integer);
var
  i: integer;
begin
   for i:= 0 to RowCount - 1 do Cells[Column, i]:= '';
   if ColCount > 2 then DeleteColumn(Column) else FEmpty:= True;
end;

{ TScrollBoxEx }

constructor TScrollBoxEx.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  Color:= clBtnShadow;
  FCanvas.Brush.Color:= clBtnShadow;
end;

destructor TScrollBoxEx.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TScrollBoxEx.WMPaint(var Message: TWMPaint);
begin
   inherited;
   if Assigned(OnPaint) then FPaint(Self);
end;


{ TLink }

constructor TLink.Create;
begin
   FLinkType:= 0; //Paranoic check
   FJoinType:= 0;
end;

{ TLinkList }

function TLinkList.Add(Obj: TLink): Integer;
begin
   Result := inherited Add(Obj);
end;

function TLinkList.Get(Index: Integer): TLink;
begin
  if Count-1 >= Index then          // Check Index
    Result := TLink(inherited Items[Index])
  else
    Result := nil;
end;

procedure TLinkList.Insert(Index: Integer; Obj: TLink);
begin
   inherited Insert(Index,Obj);
end;

procedure TLinkList.Put(Index: Integer; Item: TLink);
begin
   inherited Items[Index] := Item;
end;

{ TFormList }

function TFormList.Add(Obj: TCheckListBoxEx): Integer;
begin
   Result := inherited Add(Obj);
end;

function TFormList.Get(Index: Integer): TCheckListBoxEx;
begin
  if Count-1 >= Index then          // Check Index
    Result := TCheckListBoxEx(inherited Items[Index])
  else
    Result := nil;
end;

procedure TFormList.Insert(Index: Integer; Obj: TCheckListBoxEx);
begin
   inherited Insert(Index,Obj);
end;

procedure TFormList.Put(Index: Integer; Item: TCheckListBoxEx);
begin
   inherited Items[Index] := Item;
end;

{ TSQLForm }

constructor TSQLForm.Create(AOwner: TComponent);
begin
  inherited;
  //Localization
  Caption:= resMainTitle;
  acNew.Caption:= resMainNew;
  acFileOpen.Caption:= resMainOpen;
  acFileSave.Caption:= resMainSave;
  File1.Caption:= resMainFile;
  Query1.Caption:= resMainQuery;
  acGenerateSQL.Caption:= resMainGenSQL;
  acRunSQL.Caption:= resMainRunSQL;
  Help1.Caption:= resMainHelp;
  About1.Caption:= resMainAbout;
  TabSheet1.Caption:= resMainTab1;
  TabSheet2.Caption:= resMainTab2;
  TabSheet3.Caption:= resMainTab3;
  TabSheet4.Caption:= resMainTab4;
  TabSheet5.Caption:= resMainTab5;
  acSortAdd.Caption:= resMainSortAdd;
  acSortRemove.Caption:= resMainSortRemove;
  acSortAZ.Caption:= resMainSortAZ;
  acSortZA.Caption:= resMainSortZA;
  Label3.Caption:= resMainOutputFields;
  Label2.Caption:= resMainCriteriaMet;
  Label1.Caption:= resMainRemoveDuplicates;
  NoFunction.Caption:= resPopNoFunc;
  Average.Caption:= resPopAvg;
  Count.Caption:= resPopCount;
  Maximum.Caption:= resPopMax;
  Minimum.Caption:= resPopMin;
  Sum.Caption:= resPopSum;
  Group.Caption:= resPopGroup;
  Show.Caption:= resPopShow;
  BreakConnect1.Caption:= resPopBreakConnect;
  JoinParams1.Caption:= resPopJoinParams;
  ComboBox1.Items.Add(resMainCmb1);
  ComboBox1.Items.Add(resMainCmb2);
  ComboBox1.Items.Add(resMainCmb3);
  ComboBox1.Items.Add(resMainCmb4);
  ComboBox1.ItemIndex:= 0;
  ListView1.Columns.Items[0].Caption:= resMainList1;
  ListView1.Columns.Items[1].Caption:= resMainList2;
  //ToolBar hint localization
  acNew.Hint:= resTblNew;
  acFileOpen.Hint:= resTblOpen;
  acFileSave.Hint:= resTblSave;
  acGenerateSQL.Hint:= resTblGen;
  acRunSQL.Hint:= resTblRun;
  acTableView.Hint:= resTblTable;
  acAccept.Hint:= resTblAccep;
  ToolButton9.Hint:= resTblDecline;

   //Enable dragging of full windows if possible
   //(on Win95 W/O PLUS! not possible)
   SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, 1, nil, 0);

   //Load Drag cursors;
   Screen.Cursors[crCUR_LINK]:= LoadCursor(hInstance, 'CUR_LINK');
   Screen.Cursors[crCUR_TABLE]:= LoadCursor(hInstance, 'CUR_TABLE');
   ListBox1.DragCursor:= crCUR_TABLE;
end;

destructor TSQLForm.Destroy;
begin

  inherited;
end;

procedure TSQLForm.FormShow(Sender: TObject);
begin
   FormList:= TFormList.Create(True);
   LinkList:= TLinkList.Create(True);
   Try
     SQLDialog.DBEngine.ReadTableList;
   except
     on E: Exception do ShowSQLError(resMainDBError, E.Message);
   end;
   ListBox1.Items.Assign(SQLDialog.DBEngine.TableList);
   SelectedLine:= -1;
   Application.OnShowHint:= DoShowHint;
   WorkArea:= TSCrollBoxEx.Create(Panel1);
   WorkArea.Parent:= Panel1;
   WorkArea.Align:= AlClient;
   WorkArea.OnMouseDown:= DoMouseDown;
   WorkArea.OnDragOver:= DoDragOver;
   WorkArea.OnDragDrop:= DoDragDrop;
   WorkArea.OnPaint:= UpdateLine;
   WorkArea.AutoScroll:= True;
   WorkArea.VertScrollBar.Visible:= True;
   WorkArea.VertScrollBar.Style:= ssHotTrack;
   WorkArea.VertScrollBar.Tracking:= True;
   WorkArea.VertScrollBar.Margin:= 0;
   WorkArea.HorzScrollBar.Visible:= True;
   WorkArea.HorzScrollBar.Style:= ssHotTrack;
   WorkArea.HorzScrollBar.Tracking:= True;
   WorkArea.HorzScrollBar.Margin:= 0;

   //StringGrid
   SQLGrid:= TSQLGrid.Create(TabSheet1);
   SQLGrid.Parent:= TabSheet1;
   //ExpGrid
   ExpGrid:= TCriteriaGrid.Create(TabSheet2);
   ExpGrid.Parent:= TabSheet2;
   ExpGrid.OnButtonClick:= GridButtonClick;
   ExpGrid.OnRowMoved:= DoRowMoved;

   DataSource1.DataSet:= SQLDialog.DBEngine.ResultQuery;
end;

procedure TSQLForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   LinkList.Free;   //Release in that order only!
   FormList.Free;
   WorkArea.Free;

   //Grids
   SQLGrid.Free;
   ExpGrid.Free;
end;

procedure TSQLForm.UpdateLine(Sender: TObject);  {Draw Lines between Forms}
const
   sf = 15; //Bold line Outlet
var
  SourcePoint, DestPoint,
  SrcTop, SrcBottom,
  DestTop, DestBottom: TPoint;
  SourceRect, DestRect: TRect;
  i: integer;
  PArrX: Array[1..4] of Integer;

begin
   WorkArea.Canvas.FillRect(WorkArea.Canvas.ClipRect); //Clip area
   if LinkList.Count = 0 then exit;
   for i:= 0 to LinkList.Count - 1 do
   begin

      SourceRect:= LinkList[i].SourceList.ItemRect(LinkList[i].SourceIndex);
      DestRect:= LinkList[i].DestList.ItemRect(LinkList[i].DestIndex);

      SourcePoint.X:= SourceRect.Left;// - WorkArea.HorzScrollBar.Position; ???
      SourcePoint.Y:= SourceRect.Top;

      DestPoint.X:= DestRect.Left;
      DestPoint.Y:= DestRect.Top;

      //Transform TCheckListBoxEx coords into WorkArea coords
      SourcePoint:= WorkArea.ScreenToClient(LinkList[i].SourceList.ClientToScreen(SourcePoint));
      DestPoint:= WorkArea.ScreenToClient(LinkList[i].DestList.ClientToScreen(DestPoint));

       //Shift Y down to Half of CheckListBoxEx item
      SourcePoint.Y:= SourcePoint.Y + (SourceRect.Bottom - SourceRect.Top) div 2;
      DestPoint.Y:= DestPoint.Y + (DestRect.Bottom - DestRect.Top) div 2;

      SrcTop:= LinkList[i].SourceList.ClientRect.TopLeft;
      SrcBottom:= LinkList[i].SourceList.ClientRect.BottomRight;
      DestTop:= LinkList[i].DestList.ClientRect.TopLeft;
      DestBottom:= LinkList[i].DestList.ClientRect.BottomRight;

      //transform coordinates
      SrcTop:= WorkArea.ScreenToClient(LinkList[i].SourceList.ClientToScreen(SrcTop));
      SrcBottom:= WorkArea.ScreenToClient(LinkList[i].SourceList.ClientToScreen(SrcBottom));
      DestTop:= WorkArea.ScreenToClient(LinkList[i].DestList.ClientToScreen(DestTop));
      DestBottom:= WorkArea.ScreenToClient(LinkList[i].DestList.ClientToScreen(DestBottom));

      //Check coordinates
      if SourcePoint.Y <  SrcTop.Y then SourcePoint.Y:= SrcTop.Y - LinkList[i].SourceList.TitleHeight div 2;
      if SourcePoint.Y > SrcBottom.Y then SourcePoint.Y:= SrcBottom.Y;
      if DestPoint.Y < DestTop.Y then DestPoint.Y:= DestTop.Y - LinkList[i].DestList.TitleHeight div 2;
      if DestPoint.Y > DestBottom.Y then DestPoint.Y:= DestBottom.Y;

      {смотрим какой край ближе к другому и отрисовываем нитку с нужной
       стороны}
      if SourcePoint.X < DestPoint.X then
      begin
         PArrX[1]:= LinkList[i].SourceList.BoundsRect.Right;
         PArrX[2]:= LinkList[i].SourceList.BoundsRect.Right + sf;
         PArrX[3]:= LinkList[i].DestList.Left - sf;
         PArrX[4]:= LinkList[i].DestList.Left;
      end
   else
      begin
         PArrX[1]:= LinkList[i].SourceList.Left;
         PArrX[2]:= LinkList[i].SourceList.Left - sf;
         PArrX[3]:= LinkList[i].DestList.BoundsRect.Right + sf;
         PArrX[4]:= LinkList[i].DestList.BoundsRect.Right;
      end;

      //Draw lines
      WorkArea.Canvas.Pen.Width:= 3;
      WorkArea.Canvas.MoveTo(PArrX[1], SourcePoint.Y);
      WorkArea.Canvas.LineTo(PArrX[2], SourcePoint.Y);
      LinkList[i].SourceCoords.X:= PArrX[2];
      LinkList[i].SourceCoords.Y:= SourcePoint.Y;
      if SelectedLine = i then WorkArea.Canvas.Pen.Width:= 3
      else WorkArea.Canvas.Pen.Width:= 1;
      WorkArea.Canvas.LineTo(PArrX[3], DestPoint.Y);
      LinkList[i].DestCoords.X:= PArrX[3];
      LinkList[i].DestCoords.Y:= DestPoint.Y;
      WorkArea.Canvas.Pen.Width:= 3;
      WorkArea.Canvas.LineTo(PArrX[4], DestPoint.Y);
   end;
end;

{Drag'n'Drop in Progress}
procedure TSQLForm.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Item: Integer;
  MyPoint: TPoint;
  SourceType,
  DestType: String;
begin
   if (Source is TListBox) and (Sender is TScrollBoxEx) then
   begin
      Accept:= True;
      exit;
   end;

   if (not (Sender is TCheckListBoxEx)) or
      (not (Source is TCheckListBoxEx)) then
   begin
      Accept:= False;
      Exit;
   end;

   MyPoint.X:= X;
   MyPoint.Y:= Y;

   {≈сли это нужный компонент и источник не равен приемнику, то подсвечиваем
    тот Item приемника, над которым находитс€ курсор}
   if (TCheckListBoxEx(Sender).TableAlias <>
       TCheckListBoxEx(Source).TableAlias) then
   begin
      Item:= TCheckListBoxEx(Sender).ItemAtPos(MyPoint, False);
      if (Item >= 0) and (item <= TCheckListBoxEx(Sender).Count - 1) then
      begin
         TCheckListBoxEx(Sender).Selected[Item]:= True;

         DestType:= TCheckListBoxEx(Sender).TypeField.Strings[GetSelItem(TListBox(Sender))];
         SourceType:= TCheckListBoxEx(Source).TypeField.Strings[GetSelItem(TListBox(Source))];

         {≈сли тип пол€ источника равен типу приемника, то принимаем}
         if (SourceType = DestType) and not LinkExist(Source, Sender) then Accept:= True else Accept:= False;
      end else Accept:= False;
   end else Accept:= False;
end;

{Drag'n'Drop Complete}
procedure TSQLForm.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   Item, i: Integer;
   SourceName: string;
   MyPoint: TPoint;
   Link: TLink;
   FieldForm: TCheckListBoxEx;
begin

   {≈сли перетаскиваетс€ с TListBox, то рождаем TFieldForm}
   if (Source is TListBox) and (Sender is TScrollBoxEx) then
   begin
      SourceName:= ListBox1.Items.Strings[GetSelItem(ListBox1)];
      FieldForm:= TCheckListBoxEx.Create(WorkArea);
      FieldForm.Parent:= WorkArea;
      FieldForm.Left:= X - FieldForm.Width div 2;
      FieldForm.Top:= Y - (FieldForm.Height - FieldForm.ClientHeight) div 2;

      FieldForm.TableName:= SourceName;
      FieldForm.TableAlias:= FindNewAlias(FieldForm);

      FillForm(FieldForm, SourceName);

      FieldForm.OnChangeState:= OnChangeState;
      FieldForm.OnClose:= DoFormDestroy;
      FieldForm.OnDragOver:= DoDragOver;
      FieldForm.OnDragDrop:= DoDragDrop;
      //FieldForm.PopupMenu:= PopupMenu1;
      FieldForm.OnMouseMove:= DoMouseMove;
      FieldForm.OnCheck:= DoCheck;

      FieldForm.Show;
      FormList.Add(FieldForm);
      UpdateLine(WorkArea);
      exit;
   end;

   //Ќи то ни сЄ, выходим
   if (Not (Sender is TCheckListBoxEx)) or (Not (Source is TCheckListBoxEx)) then Exit;

   MyPoint.X:= X;
   MyPoint.Y:= Y;
   Item:= TCheckListBoxEx(Sender).ItemAtPos(MyPoint, True); //”знаем номер Item'а под курсором
   if Item = - 1 then exit; // не попали мышкой, выходим

   Link:= TLink.Create; //–ождаем Link

   //Ќаходим Item, с которого было перетаскивание
   Link.SourceIndex:= GetSelItem(TListBox(Source));

   Link.DestIndex:= Item; //Item, на который перетаскиваем
   TCheckListBoxEx(Source).BoldItem[Link.SourceIndex]:= True;
   TCheckListBoxEx(Sender).BoldItem[Link.DestIndex]:= True;

   //«аполн€ем остальные пол€ класса
   Link.SourceList:= Source as TCheckListBoxEx;
   Link.DestList:= Sender as TCheckListBoxEx;
   Link.SourceName:= TCheckListBoxEx(Source).TableAlias;
   Link.DestName:= TCheckListBoxEx(Sender).TableAlias;
   for i:= 0 to LinkList.Count - 1 do
   begin
      if (LinkList[i].SourceName = Link.SourceName) and
         (LinkList[i].DestName = Link.DestName) then
      begin
         Link.FJoinType:= LinkList[i].FJoinType;
         break;
      end;
   end;
   LinkList.Add(Link); // Add Link to list
   UpdateLine(Self);
end;

{Line number under Cursor}
function TSQLForm.GetLineAtCursor: Integer;
  procedure SwapInt(var X, Y: Integer);
  var
    T: Integer;
  begin
    T := X;
    X := Y;
    Y := T;
  end;
const
    sf = 6; //Scale factor
var
  i,TX1, TX2, TY1,TY2,X1,Y1,
  X2,Y2,Lx, Ly, C: integer;
  MousePos: TPoint;
  Delta: Real;
begin
   Result:= - 1;
   for i:= 0 to LinkList.Count - 1 do
   begin
      MousePos:= Mouse.CursorPos;
      MousePos:= WorkArea.ScreenToClient(MousePos);
      X1:= LinkList[i].SourceCoords.X;
      X2:= LinkList[i].DestCoords.X;
      Y1:= LinkList[i].SourceCoords.Y;
      Y2:= LinkList[i].DestCoords.Y;
      TX1:= X1;
      TX2:= X2;
      TY1:= Y1;
      TY2:= Y2;
      if TX1> TX2 then SwapInt(TX1, TX2);
      if TY1> TY2 then SwapInt(TY1, TY2);
      Lx:= X2-X1;
      Ly:= Y2-Y1;
      C:= -Ly*X1 + Lx*Y1;
      Delta:= Sqrt(Power((X1-X2), 2) + Power((Y1-Y2), 2)) * sf;
      if (Abs(-Ly*MousePos.X + Lx*MousePos.Y - C)<= Delta) and
         InRange(MousePos.X, TX1 - sf, TX2 + sf) and
         InRange(MousePos.Y, TY1 - sf, TY2 + sf) then
      begin
         Result:= i;
         break;
      end;
   end;
end;

procedure TSQLForm.PopupMenu2Popup(Sender: TObject);
begin
   BreakConnect1.Enabled:= GetLineAtCursor <> - 1;
   JoinParams1.Enabled:= BreakConnect1.Enabled;
end;

//Connection break and line delete
procedure TSQLForm.BreakConnect1Click(Sender: TObject);
var
  SIndex, DIndex: Integer;
  SBox, DBox: TCheckListBoxEx;
begin
   SIndex:= LinkList[SelectedLine].SourceIndex;
   SBox:= LinkList[SelectedLine].SourceList;
   DIndex:= LinkList[SelectedLine].DestIndex;
   DBox:= LinkList[SelectedLine].DestList;

   LinkList.Delete(SelectedLine);
   SBox.BoldItem[SIndex]:= AnotherLinkExist(SBox, SIndex);
   DBox.BoldItem[DIndex]:= AnotherLinkExist(DBox, DIndex);

   SelectedLine:= -1;
   UpdateLine(Self);
end;

//Hint support (will be modified)
procedure TSQLForm.DoShowHint(var HintStr: String; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  item: integer;
begin
   if (HintInfo.HintControl = WorkArea) then
   begin
      Item:= GetLineAtCursor;
      if Item <> - 1 then
      begin
         HintInfo.HideTimeout:= 5000;
         HintStr:= LinkList[Item].SourceName + '.' +
         LinkList[Item].SourceList.Items.Strings[LinkList[Item].SourceIndex] +
         #32 + LinkType[LinkList[Item].FLinkType] + #32 +
         LinkList[Item].DestName + '.' +
         LinkList[Item].DestList.Items.Strings[LinkList[Item].DestIndex];
      end;
   end;
end;

function TSQLForm.GetSelItem(Source: TListBox): Integer;
var
  i: integer;
begin
   Result:= - 1;
   for i:= 0 to Source.Count - 1 do
   begin
      if Source.Selected[i] then
      begin
         Result:= i;
         break;
      end;
   end;
end;

function TSQLForm.GetCheckItem(Source: TCheckListBox): Integer;
var
  i: integer;
begin
   Result:= - 1;
   for i:= 0 to Source.Count - 1 do
   begin
      if Source.Checked[i] then
      begin
         Result:= i;
         break;
      end;
   end;
end;

procedure TSQLForm.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  P: TPoint;
begin
   Index:= GetLineAtCursor;
   if Index <> - 1 then SelectedLine:= Index else SelectedLine:= -1;
   UpdateLine(WorkArea);
   if (Button = mbRight) then
   begin
      P.X:= X;
      P.Y:= Y;
      P:= WorkArea.ClientToScreen(P);
      if Index <> - 1 then PopupMenu2.Popup(P.X, P.Y);
   end;
end;

{Join Params Dialog}
procedure TSQLForm.JoinParams1Click(Sender: TObject);
Var
  LinkDlg: TLinkDlg;
  item, i: integer;
begin
    item:= SelectedLine;
    LinkDlg:= TLinkDlg.Create(Self);
    LinkDlg.StaticText1.Caption:= LinkList[Item].SourceName;
    LinkDlg.StaticText2.Caption:= LinkList[Item].SourceList.Items.Strings[LinkList[Item].SourceIndex];
    LinkDlg.StaticText3.Caption:= LinkList[Item].DestName;
    LinkDlg.StaticText4.Caption:= LinkList[Item].DestList.Items.Strings[LinkList[Item].DestIndex];
    LinkDlg.RadioGroup1.ItemIndex:= LinkList[Item].FLinkType;
    LinkDlg.RadioGroup2.ItemIndex:= LinkList[Item].FJoinType;
    LinkDlg.ShowModal;
    if LinkDlg.ModalResult = mrOK then
    begin
       LinkList[Item].FLinkType:= LinkDlg.RadioGroup1.ItemIndex;
       LinkList[Item].FJoinType:= LinkDlg.RadioGroup2.ItemIndex;

       //Make similar links JoinType same as selected
       for i:= 0 to LinkList.Count - 1 do
       begin
          if (LinkList[i].SourceName = LinkList[item].SourceName) and
             (LinkList[i].DestName = LinkList[item].DestName) and
             (i <> item) then LinkList[i].FJoinType:= LinkList[item].FJoinType;
       end;
    end;
    LinkDlg.Free;
    //SelectedLine:= -1 ????
    UpdateLine(Self);
end;

{Generate SQL (Revision 1.2)}
function TSQLForm.GenerateSQL: String;

 Function FindRealName(Table, Field: String): String;
 var
   i, j: Integer;
 begin
    for i:= 0 to FormList.Count - 1 do
    begin
       if FormList[i].TableAlias = Table then
       begin
          for j:= 0 to FormList[i].Items.Count - 1 do
          begin
             if FormList[i].Items.Strings[j] = Field then
             begin
                Result:= FormList[i].NameField.Strings[j];
                exit;
             end;
          end;
       end;
    end;
 end;

 Function HaveLink(TableName: String): Boolean;
 var
   i: Integer;
 begin
    Result:= False;
    for i:= 0 to LinkList.Count - 1 do
    begin
       if (LinkList[i].SourceName = TableName) or (LinkList[i].DestName = TableName) then
       begin
          Result:= True;
          exit;
       end;
    end;
 end;

 function LinkMatch(Obj: TLinkList; Idx: Integer): boolean;
 var
   i: integer;
 begin
    Result:= False;
    for i:= 0 to Idx - 1 do
    begin
       if (Obj[i].SourceName = Obj[idx].SourceName) or
          (Obj[i].DestName = Obj[idx].DestName) or
          (Obj[i].DestName = Obj[idx].SourceName) or
          (Obj[i].SourceName = Obj[idx].DestName) then
       begin
          Result:= True;
          exit;
       end;
    end;
 end;
 
const
    Select = 'SELECT ';
    From = #13#10'FROM ';
    Where = #13#10'    WHERE ';
    GroupBy = #13#10'    GROUP BY ';
    OrderBy = #13#10'    ORDER BY ';
    ANDS = ' AND ';
    Distinct = 'DISTINCT ';

var
  i, j: integer;
  S, Ts: string;
  SelectList: TStringList;
  TableList: TStringList;
  AliasList: TSTringList;
  FromAddList: TStringList;
  ExpList: TStringList;
  OrderList: TStringList;
  GroupList: TStringList;
  FSortList: TLinkList;
begin
   if SQLGrid.Cells[1, ResTableID] = EmptyStr then exit;

   SelectList:= TStringList.Create;
   TableList:= TStringList.Create;
   AliasList:= TStringList.Create;
   FromAddList:= TStringList.Create;
   ExpList:= TStringList.Create;
   OrderList:= TStringList.Create;
   GroupList:= TStringList.Create;

   FSortList:= TLinkList.Create(False);

   //Build 'SELECT' list
   for i:= 1 to SQLGrid.ColCount - 1 do
   begin
      if SQLGrid.Cells[i, ResShowID] <> EmptyStr then
      begin
         Ts:= SQLDialog.DBEngine.ConvertTableName(SQLGrid.Cells[i, ResTableID]) + '.' +
         SQLDialog.DBEngine.ConvertFieldName(FindRealName(SQLGrid.Cells[i, ResTableID], SQLGrid.Cells[i, ResFieldID]));

         if (SQLGrid.Cells[i, ResFunctionID] <> EmptyStr) then
            SelectList.Add(SQLGrid.Cells[i, ResFunctionID] + ' (' + Ts + ')')
         else SelectList.Add(Ts);
      end;
   end;

   //makes links sorted by grouping similar links
   //(it will gives ability to combinate links in 'JOIN' predicate, using 'AND')
   for i:= 0 to LinkList.Count - 1 do
   begin
      for j:= 0 to LinkList.Count - 1 do
      begin
         if (LinkList[i].SourceName = LinkList[j].SourceName) and
            (LinkList[i].DestName = LinkList[j].DestName) and
            (FSortList.IndexOf(LinkList[j]) = - 1) then
            FSortList.Add(LinkList[j]);
      end;
   end;

   //build table list for 'FROM' clause
   for i:= 0 to FormList.Count - 1 do
   begin
      TableList.Add(FormList[i].TableName);
      AliasList.Add(FormList[i].TableAlias);
      if (not HaveLink(FormList[i].TableAlias)) then
      begin
         TS:= SQLDialog.DBEngine.ConvertTableName(FormList[i].TableName);
         if FormList[i].TableName <> FormList[i].TableAlias then
            TS:= TS + ' ' + SQLDialog.DBEngine.ConvertTableName(FormList[i].TableAlias);
         FromAddList.Add(TS);
      end;
   end;

   //build expression list
   for i:= 1 to ExpGrid.RowCount - 1 do
      if ExpGrid.Cells[1, i] <> EmptyStr then ExpList.Add(ExpGrid.Cells[1, i]);

   //build order list
   for i:= 0 to ListView1.Items.Count - 1 do
   begin
      for j:= 1 to SQLGrid.ColCount - 1 do
      begin
         if SQLGrid.Cells[j, ResShowID] <> EmptyStr then
         begin
            Ts:= SQLGrid.Cells[j, ResTableID] + '.' + SQLGrid.Cells[j, ResFieldID];
            if ListView1.Items.Item[i].Caption = Ts then
            begin
               Ts:= SQLDialog.DBEngine.ConvertTableName(SQLGrid.Cells[j, ResTableID]) + '.' +
                    SQLDialog.DBEngine.ConvertFieldName(FindRealName(SQLGrid.Cells[j, ResTableID], SQLGrid.Cells[j, ResFieldID]));
               OrderList.Add(TS + ' ' + ListView1.Items.Item[i].SubItems.Strings[0]);
               break;
            end;
         end;
      end;
   end;

   //build group list
   for i:= 1 to SQLGrid.ColCount - 1 do
   begin
      if (SQLGrid.Cells[i, ResGroupID] <> EmptyStr) then
      begin
         Ts:= SQLDialog.DBEngine.ConvertTableName(SQLGrid.Cells[i, ResTableID]) + '.' +
         SQLDialog.DBEngine.ConvertFieldName(FindRealName(SQLGrid.Cells[i, ResTableID], SQLGrid.Cells[i, ResFieldID]));
         GroupList.Add(Ts);
      end;
   end;

   //build 'SELECT' clause
   S:= Select;
   if CheckBox1.Checked then S:= S + Distinct;
   for i:= 0 to SelectList.Count - 1 do
   begin
      S:= S + SelectList.Strings[i];
      if i < SelectList.Count - 1 then S:= S + ', ';
   end;

   //build Joins list
   if LinkList.Count = 0 then
   begin
      S:= S + From;
      for i:= 0 to TableList.Count - 1 do
      begin
         S:= S + SQLDialog.DBEngine.ConvertTableName(TableList.Strings[i]);
         if TableList.Strings[i] <> AliasList.Strings[i] then
            S:= S + ' ' + SQLDialog.DBEngine.ConvertTableName(AliasList.Strings[i]);
         if i < TableList.Count - 1 then S:= S + ', ';
      end; //for
   end
     else
   begin
      S:= S + FROM;
      i:= 0;
      While (i <= FSortList.Count - 1) do
      begin
         if Not LinkMatch(FSortList, i) then
         begin
            if i > 0 then S:= S + ', '+ #13#10;
            S:= S + SQLDialog.DBEngine.FindFullName(FSortList[i].SourceList);
         end;

         S:= S + #13#10 + '    ' + JoinType[FSortList[i].FJoinType]+ ' ';
         S:= S +  SQLDialog.DBEngine.FindFullName(FSortList[i].DestList) + 'ON ';
         S:= S + '( ';
         S:= S + SQLDialog.DBEngine.ConvertTableName(FSortList[i].SourceName) + '.' +
         SQLDialog.DBEngine.ConvertFieldName(FSortList[i].SourceList.NameField.Strings[FSortList[i].SourceIndex]) +
         #32 + LinkType[FSortList[i].FLinkType] + #32 +
         SQLDialog.DBEngine.ConvertTableName(FSortList[i].DestName) + '.' +
         SQLDialog.DBEngine.ConvertFieldName(FSortList[i].DestList.NameField.Strings[FSortList[i].DestIndex]) + ' )';
         if i < FSortList.Count - 1 then
         begin
            while (i < FSortList.Count - 1) and
                  (FSortList[Succ(i)].SourceName = FSortList[i].SourceName) and
                  (FSortList[Succ(i)].DestName = FSortList[i].DestName) do
            begin
               S:= S + ANDS;
               S:= S + '( ';
               S:= S + SQLDialog.DBEngine.ConvertTableName(FSortList[Succ(i)].SourceName) + '.' +
               SQLDialog.DBEngine.ConvertFieldName(FSortList[Succ(i)].SourceList.NameField.Strings[FSortList[Succ(i)].SourceIndex]) +
               #32 + LinkType[FSortList[Succ(i)].FLinkType] + #32 +
               SQLDialog.DBEngine.ConvertTableName(FSortList[Succ(i)].DestName) + '.' +
               SQLDialog.DBEngine.ConvertFieldName(FSortList[Succ(i)].DestList.NameField.Strings[FSortList[Succ(i)].DestIndex]) + ' )';
               inc(i);
            end; //while
         end; //if
         inc(i);
      end; //while

      //generate additional table names which not included in Join clause
      if FromAddList.Count > 0 then
      begin
         S:= S + ', ';
         for i:= 0 to FromAddList.Count - 1 do
         begin
            S:= S + FromAddList.Strings[i];
            if i < FromAddList.Count - 1 then S:= S + ', ';
         end;
      end;
   end; //else

   //Build expressions
   if ExpList.Count > 0 then
   begin
      S:= S + Where;
      if (ComboBox1.ItemIndex = 2) or (ComboBox1.ItemIndex = 3) then S:= S + 'NOT ( ';
      for i:= 0 to ExpList.Count - 1 do
      begin
         s:= s + '( ' + ExpList.Strings[i] + ' )';
         if i < ExpList.Count - 1 then S:= S + ExpParams[ComboBox1.itemIndex];
      end;
      if (ComboBox1.ItemIndex = 2) or (ComboBox1.ItemIndex = 3) then S:= S + ' )';
   end;

   //build group
   if GroupList.Count > 0 then
   begin
      S:= S + GroupBy;
      for i:= 0 to GroupList.Count - 1 do
      begin
         S:= S + GroupList.Strings[i];
         if i < GroupList.Count - 1 then S:= S + ', ';
      end;
   end;

   //build order
   if OrderList.Count > 0 then
   begin
      S:= S + OrderBy;
      for i:= 0 to OrderList.Count - 1 do
      begin
         S:= S + OrderList.Strings[i];
         if i < OrderList.Count - 1 then S:= S + ', ';
      end;
   end;

   Result:= S;

   //Free Lists
   SelectList.Free;
   TableList.Free;
   AliasList.Free;
   FromAddList.Free;
   ExpList.Free;
   OrderList.Free;
   GroupList.Free;

   FSortList.Free;
end;

{Free objects before TFieldForm destruction}
procedure TSQLForm.DoFormDestroy(Sender: TObject);
var
  i, j, k, SInd, DInd, idx: integer;
  SBox, DBox: TCheckListBoxEx;
  ts: string;
begin
   for j:= 0 to FormList.Count - 1 do
   begin
      if FormList[j].TableAlias = TCheckListBoxEx(Sender).TableAlias then
      begin
         for k:= LinkList.Count - 1 downto 0 do
         begin
            if (FormList[j].TableAlias = LinkList[k].SourceName)
               or (FormList[j].TableAlias = LinkList[k].DestName) then
               begin
                  SInd:= LinkList[k].SourceIndex;
                  SBox:= LinkList[k].SourceList;
                  DInd:= LinkList[k].DestIndex;
                  DBox:= LinkList[k].DestList;
                  LinkList.Delete(k);
                  SBox.BoldItem[SInd]:= AnotherLinkExist(SBox, SInd);
                  DBox.BoldItem[DInd]:= AnotherLinkExist(DBox, DInd);
               end;
         end;

         //remove Cells from SQLGrid and fields from Sorting order;
         for k:= 0 to TCheckListBoxEx(Sender).Items.Count - 1 do
         begin
            if TCheckListBoxEx(Sender).Checked[k] then
               SQLGrid.RemoveColumn(SQLGrid.FindColumn(TCheckListBoxEx(Sender).TableAlias, TCheckListBoxEx(Sender).Items.Strings[k]));

            ts:= TCheckListBoxEx(Sender).TableAlias +'.'+ TCheckListBoxEx(Sender).Items.Strings[k];
            idx:= ListBox2.Items.IndexOf(ts);
            if idx > - 1 then
            begin
               ListBox2.Items.Delete(idx);
               Continue;
            end;

            for i:= 0 to ListView1.Items.Count - 1 do
            begin
               if (ListView1.Items.Item[i].Caption = ts) then
               begin
                  ListView1.Items.Delete(i);
                  break;
               end;
            end;
         end;

         FormList.Delete(j);
         UpdateLine(Self);
         SQLGrid.Realign;
         exit;
      end;
   end;
end;

procedure TSQLForm.DoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  Index: integer;
begin
   Index:= GetItemAtCursor(TCheckListBoxEx(Sender));
   if Index <> - 1 then
      TCheckListBoxEx(Sender).Hint:= TCheckListBoxEx(Sender).Items[Index]
   else TCheckListBoxEx(Sender).Hint:= '';
end;

function TSQLForm.GetItemAtCursor(Sender: TCheckListBoxEx): Integer;
var
  MousePoint: TPoint;
begin
   MousePoint:= Mouse.CursorPos;
   MousePoint:= Sender.ScreenToClient(MousePoint);
   Result:= Sender.ItemAtPos(MousePoint, True);
end;

procedure TSQLForm.OnChangeState(Sender: TObject);
begin
   SelectedLine:= - 1;
   UpdateLine(Self);
end;

function TSQLForm.LinkExist(Source, Sender: TObject): Boolean;
var
  i: integer;
begin
   Result:= False;
   for i:= 0 to LinkList.Count - 1 do
   begin
      if (LinkList[i].SourceList.TableAlias = TCheckListBoxEx(Source).TableAlias) and
         (LinkList[i].SourceIndex = GetSelItem(TListBox(Source))) and
         (LinkList[i].DestList.TableAlias = TCheckListBoxEx(Sender).TableAlias) and
         (LinkList[i].DestIndex = GetSelItem(TListBox(Sender))) then
         begin
            Result:= True;
            exit;
         end;

      if (LinkList[i].SourceList.TableAlias = TCheckListBoxEx(Sender).TableAlias) and
         (LinkList[i].SourceIndex = GetSelItem(TListBox(Sender))) and
         (LinkList[i].DestList.TableAlias = TCheckListBoxEx(Source).TableAlias) and
         (LinkList[i].DestIndex = GetSelItem(TListBox(Source))) then
         begin
            Result:= True;
            exit;
         end;
   end;
end;

function TSQLForm.AnotherLinkExist(Source: TObject; Index: Integer): Boolean;
var
  i: integer;
begin
   Result:= False;
   for i:= 0 to LinkList.Count - 1 do
   begin
      if ((LinkList[i].SourceList.TableAlias = TCheckListBoxEx(Source).TableAlias) and
         (LinkList[i].SourceIndex = Index)) or
         ((LinkList[i].DestList.TableAlias = TCheckListBoxEx(Source).TableAlias) and
         (LinkList[i].DestIndex = index)) then
         begin
            Result:= True;
            exit;
         end;
   end;
end;

// Find new alias name
function TSQLForm.FindNewAlias(Obj: TCheckListBoxEx): String;

   function ExtractInt(const Source: string): integer;
   var
     Count: integer;
     tempStr: string;
   begin
      Count:= Length(Source);
      tempStr:= EmptyStr;
      Result:= 0;
      While (Source[Count] in ['0'..'9'])  do
      begin
         TempStr:= Source[Count] + TempStr;
         dec(Count);
      end;
      if (TempStr <> EmptyStr) then Result:= StrToInt(TempStr);
   end;

   Function ExtractShortName(const Source: String): String;
   begin
      Result:= Source;
      if Pos('.', Source) > 0 then Result:= Copy(Source, 1, Pos('.', Source) - 1);
   end;

var
  i, MaxCount: integer;
  Alias, TableName, TableAlias: string;
begin
   MaxCount:= 0;
   TableName:= Obj.TableName;
   TableAlias:= ExtractShortName(Obj.TableAlias);
   for i:= 0 to FormList.Count - 1 do
   begin
      if (FormList[i].TableName = TableName) then
      begin
         Alias:= FormList[i].TableAlias;
         if (Alias <> TableAlias) then
         begin
            if MaxCount < ExtractInt(Alias) then MaxCount:= ExtractInt(Alias);
         end;
      end;
   end;
   inc(MaxCount);
   Result:= TableAlias;  //Result:= TableName;
   if (Alias <> EmptyStr) then Result:= Result + '_' + IntToStr(MaxCount);
end;

procedure TSQLForm.DoCheck(Sender: TObject; const Item: Integer;
  Checked: Boolean);
var
  i, idx: integer;
begin
   //beep; //debug!
   PageControl1.ActivePageIndex:= 0;
   if Checked then
   begin
      SQLGrid.AddColumn(TCheckListBoxEx(Sender).TableAlias, TCheckListBoxEx(Sender).Items.Strings[Item]);
      ListBox2.Items.Add(TCheckListBoxEx(Sender).TableAlias +'.'+ TCheckListBoxEx(Sender).Items.Strings[Item])
   end
     else
   begin
      SQLGrid.RemoveColumn(SQLGrid.FindColumn(TCheckListBoxEx(Sender).TableAlias, TCheckListBoxEx(Sender).Items.Strings[Item]));
      for i:= 0 to ListBox2.Items.Count - 1 do
      begin
         idx:= ListBox2.Items.IndexOf(TCheckListBoxEx(Sender).TableAlias +'.'+ TCheckListBoxEx(Sender).Items.Strings[Item]);
         if idx > - 1 then
         begin
            ListBox2.Items.Delete(idx);
            exit;
         end;
      end;
      for i:= 0 to ListView1.Items.Count - 1 do
      begin
         if (ListView1.Items.Item[i].Caption =
           TCheckListBoxEx(Sender).TableAlias +'.'+ TCheckListBoxEx(Sender).Items.Strings[Item]) then
         begin
            ListView1.Items.Delete(i);
            exit;
         end;
      end;
   end;
end;

procedure TSQLForm.FillForm(var obj: TCheckListBoxEx; const TableName: string);
var
  i: integer;
begin
   SQLDialog.DBEngine.ReadFieldList(TableName);
   for i:= 0 to SQLDialog.DBEngine.FieldList.Count - 1 do
   begin
      obj.AddStr(SQLDialog.DBEngine.FieldList.Strings[i] + ' (' + SQLDialog.DBEngine.SystemList.Strings[i] + ')');
      obj.NameField.Add(SQLDialog.DBEngine.FieldList.Strings[i]);
      obj.TypeField.Add(SQLDialog.DBEngine.SystemList.Strings[i]);
   end;
end;

procedure TSQLForm.ShowClick(Sender: TObject);
var
  i, idx: integer;
begin
   if mnuShow.Items[0].Checked then
   begin
      SQLGrid.Cells[SQLGrid.AColumn, ResShowID]:= EmptyStr;
      mnuShow.Items[0].Checked:= False;

      idx:= ListBox2.Items.IndexOf(SQLGrid.Cells[SQLGrid.AColumn, 0] +'.'+ SQLGrid.Cells[SQLGrid.AColumn, 1]);
      if idx > - 1 then
      begin
         ListBox2.Items.Delete(idx);
         exit;
      end;

      for i:= 0 to ListView1.Items.Count - 1 do
      begin
         if (ListView1.Items.Item[i].Caption =
           SQLGrid.Cells[SQLGrid.AColumn, 0] +'.'+ SQLGrid.Cells[SQLGrid.AColumn, 1]) then
         begin
            ListView1.Items.Delete(i);
            exit;
         end;
      end;

   end
     else
   begin
      SQLGrid.Cells[SQLGrid.AColumn, ResShowID]:= resGridVisible;
      mnuShow.Items[0].Checked:= True;
      ListBox2.Items.Add(SQLGrid.Cells[SQLGrid.AColumn, 0] +'.'+ SQLGrid.Cells[SQLGrid.AColumn, 1])
   end;
end;

procedure TSQLForm.NoFunctionClick(Sender: TObject);
var
  item: TMenuItem;
begin
   if Sender is TMenuItem then
   begin
      item:= TMenuItem(Sender);
      if not item.Checked then
      begin
         item.Checked:= True;
         SQLGrid.Cells[SQLGrid.AColumn, ResFunctionID]:= ResFuncOrder[Item.tag];
      end;
   end;
end;

procedure TSQLForm.GroupClick(Sender: TObject);
begin
   if mnuGroup.Items[0].Checked then
   begin
      SQLGrid.Cells[SQLGrid.AColumn, ResGroupID]:= EmptyStr;
      mnuGroup.Items[0].Checked:= False;
   end
     else
   begin
      SQLGrid.Cells[SQLGrid.AColumn, ResGroupID]:= resGridVisible;
      mnuGroup.Items[0].Checked:= True;
   end;
end;

procedure TSQLForm.ToolButton2Click(Sender: TObject);
begin
end;

{Clear work area for new Query}
procedure TSQLForm.acNewExecute(Sender: TObject);
begin
   LinkList.Clear;
   FormList.Clear;
   SQLGrid.ClearGrid;
   Memo1.Lines.Clear;
   ExpGrid.ClearGrid;
   ListBox2.Items.Clear;
   ListView1.Items.Clear;
   CheckBox1.Checked:= False;
   ComboBox1.ItemIndex:= 0;
   SQLDialog.DBEngine.CloseResultQuery;
   UpdateLine(Self);
end;

procedure TSQLForm.acFileOpenExecute(Sender: TObject);

  Function FindForm(const TableName: string): TCheckListBoxEx;
  var
    i: integer;
  begin
     Result:= nil;
     for i:= 0 to FormList.Count - 1 do
     begin
        if FormList[i].TableAlias = TableName then
        begin
           Result:= FormList[i];
           exit;
        end;
     end;
  end;

  Function FindItem(Obj: TCheckListBoxEx; const Field: string): Integer;
  var
    i: integer;
  begin
     Result:= - 1;
     for i:= 0 to Obj.Count - 1 do
     begin
        if (Obj.Items.Strings[i] = Field) then
        begin
           Result:= i;
           exit;
        end;
     end;
  end;

var
  LoadList: TStringList;
  ListBox, TempBox: TCheckListBoxEx;
  ALink: TLink;
  i, j, idx, Count: integer;
  Content: String;
  LItem: TListItem;
begin
   LoadList:= TStringList.Create;

   if Assigned(SQLDialog.OnLoadModel) then
   begin
      SQLDialog.FLoadEvent(SQLDialog, Content);
      LoadList.Text:= Content;
   end
   else
   if OpenDlg.Execute then LoadList.LoadFromFile(OpenDlg.FileName)
   else
   begin
      LoadList.Free;
      exit;
   end;


   if LoadList.IndexOf(SaveID) = - 1 then
   begin
      ShowSQLError(resMainUnknownFile1, resMainUnknownFile2);
      LoadList.Free;
      exit;
   end;

   acNewExecute(Self); //clear

   //there is no Revision check in this release
   //it will be added in future versions for backward compatability

   idx:= LoadList.IndexOf('[FORMS]');
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   for i:= 0 to Count - 1 do
   begin
      ListBox:= TCheckListBoxEx.Create(WorkArea);
      ListBox.Parent:= WorkArea;
      ListBox.TableName:= LoadList.Strings[idx + 1];
      ListBox.TableAlias:= LoadList.Strings[idx + 2];

      ListBox.Left:= StrToInt(LoadList.Strings[idx + 3]);
      ListBox.Top:= StrToInt(LoadList.Strings[idx + 4]);
      ListBox.Width:= StrToInt(LoadList.Strings[idx + 5]);
      ListBox.Height:= StrToInt(LoadList.Strings[idx + 6]);

      FillForm(ListBox, ListBox.TableName);

      ListBox.OnChangeState:= OnChangeState;
      ListBox.OnClose:= DoFormDestroy;
      ListBox.OnDragOver:= DoDragOver;
      ListBox.OnDragDrop:= DoDragDrop;
      ListBox.OnMouseMove:= DoMouseMove;
      ListBox.OnCheck:= DoCheck;

      ListBox.Show;
      FormList.Add(ListBox);
      idx:= idx + 6;
   end;


   idx:= LoadList.IndexOf('[LINKS]');
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   for i:= 0 to Count - 1 do
   begin
      ALink:= TLink.Create;
      ALink.SourceIndex:= StrToInt(LoadList.Strings[idx + 1]);
      ALink.DestIndex:= StrToInt(LoadList.Strings[idx + 2]);

      ALink.SourceName:= LoadList.Strings[idx + 3];
      ALink.DestName:= LoadList.Strings[idx + 4];
      ALink.FLinkType:= StrToInt(LoadList.Strings[idx + 5]);
      ALink.FJoinType:= StrToInt(LoadList.Strings[idx + 6]);

      ALink.SourceList:= FindForm(ALink.SourceName);
      ALink.DestList:= FindForm(ALink.DestName);

      ALink.SourceList.BoldItem[ALink.SourceIndex]:= True;
      ALink.DestList.BoldItem[ALink.DestIndex]:= True;

      LinkList.Add(ALink);
      UpdateLine(Self);

      idx:= idx + 6;
   end;

   //load sqlgrid
   idx:= LoadList.IndexOf('[GRIDS]');
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   for i:= 1 to Count - 1 do
   begin
      for j:= 0 to SQLGrid.RowCount - 1 do
      begin
         inc(idx);
         SQLGrid.Cells[i, j]:= LoadList.Strings[idx];
      end;
      if (i < Count - 1) then SQLGrid.AddColumn;
      if (SQLGrid.Cells[i, 0] <> EmptyStr) then
      begin
         TempBox:= FindForm(SQLGrid.Cells[i, 0]);
         TempBox.Checked[FindItem(TempBox, SQLGrid.Cells[i, 1])]:= True;
      end;
   end;
   SQLGrid.Realign;

   //load expressions
   idx:= LoadList.IndexOf('[EXPRESSIONS]');
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   for i:= 1 to Count - 1 do
   begin
      inc(idx);
      ExpGrid.Cells[1, i]:= LoadList.Strings[idx];
      if i < Count - 1 then ExpGrid.RowCount:= ExpGrid.RowCount + 1;
   end;

   //load Order list tab
   idx:= LoadList.IndexOf('[SORTING]');
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   for i:= 0 to Count - 1 do
   begin
      inc(idx);
      ListBox2.Items.Add(LoadList.Strings[idx]);
   end;
   inc(idx);
   Count:= StrToInt(LoadList.Strings[idx]);
   inc(idx);
   for i:= 0 to Count - 1 do
   begin
      LItem:= ListView1.Items.Add;
      LItem.Caption:= LoadList.Strings[idx];
      LItem.SubItems.add(LoadList.Strings[idx + 1]);
      inc(idx, 2);
   end;

   //Load options
   idx:= LoadList.IndexOf('[OPTIONS]');
   inc(idx);
   CheckBox1.Checked:= StrToBool(LoadList.Strings[idx]);
   inc(idx);
   ComboBox1.ItemIndex:= StrToInt(LoadList.Strings[idx]);
   UpdateExpGrid;

   LoadList.Free;
end;

procedure TSQLForm.acFileSaveExecute(Sender: TObject);
var
  SaveList: TStringList;
  i, j: integer;
const
   Delimiter = '--------------';
begin
   //if not SaveDlg.Execute then exit;

   SaveList:= TStringList.Create;

   SaveList.Add(SaveID);
   SaveList.Add(Delimiter);
   SaveList.Add('[Revision]');
   SaveList.Add(SaveIDMAXVER);
   SaveList.Add(Delimiter);

   //Save Forms
   SaveList.Add('[FORMS]');
   SaveList.Add(IntToStr(FormList.Count)); // Forms count
   for i:= 0 to FormList.Count - 1 do
   begin
      SaveList.Add(FormList[i].TableName);
      SaveList.Add(FormList[i].TableAlias);

      SaveList.Add(IntToStr(FormList[i].Left));
      SaveList.Add(IntToStr(FormList[i].Top));
      SaveList.Add(IntToStr(FormList[i].Width));
      SaveList.Add(IntToStr(FormList[i].Height));
   end;
   SaveList.Add(Delimiter);

   //SaveLink
   SaveList.Add('[LINKS]');
   SaveList.Add(IntToStr(LinkList.Count)); //LinkList count
   for i:= 0 to LinkList.Count - 1 do
   begin
      SaveList.Add(IntToStr(LinkList[i].SourceIndex)); //source index
      SaveList.Add(IntToStr(LinkList[i].DestIndex));

      SaveList.Add(LinkList[i].SourceList.TableAlias); // Source table alias
      SaveList.Add(LinkList[i].DestList.TableAlias); //Dest table alias
      SaveList.Add(IntToStr(LinkList[i].FLinkType)); //Link type
      SaveList.Add(IntToStr(LinkList[i].FJoinType)); //Join type
   end;
   SaveList.Add(Delimiter);

   //Save SQLGrid
   SaveList.Add('[GRIDS]');
   SaveList.Add(IntToStr(SQLGrid.ColCount)); // ColCount
   for i:= 1 to SQLGrid.ColCount - 1 do
   begin
      for j:= 0 to SQLGrid.RowCount - 1 do
      begin
         SaveList.Add(SQLGrid.Cells[i, j]);
      end
   end;
   SaveList.Add(Delimiter);

   SaveList.Add('[EXPRESSIONS]');
   SaveList.Add(IntToStr(ExpGrid.RowCount)); //rowcount
   for i:= 1 to ExpGrid.RowCount - 1 do
   begin
      SaveList.Add(ExpGrid.Cells[1, i]);
   end;
   SaveList.Add(Delimiter);

   //Save Sorting order tab
   SaveList.Add('[SORTING]');
   SaveList.Add(IntToStr(ListBox2.Items.Count)); //count
   for i:= 0 to ListBox2.Items.Count - 1 do
       SaveList.Add(ListBox2.Items.Strings[i]);
   SaveList.Add(IntToStr(ListView1.Items.Count)); //count
   for i:= 0 to ListView1.Items.Count - 1 do
   begin
      SaveList.Add(ListView1.Items.Item[i].Caption);
      SaveList.Add(ListView1.Items.Item[i].SubItems.Strings[0]);
   end;
   SaveList.Add(Delimiter);

   //Save options
   SaveList.Add('[OPTIONS]');
   SaveList.Add(BoolToStr(CheckBox1.Checked));
   SaveList.Add(IntToStr(ComboBox1.ItemIndex));
   SaveList.Add(Delimiter);

   SaveList.Add('[END OF FILE]');

   if Assigned(SQLDialog.OnSaveModel) then
      SQLDialog.FSaveEvent(SQLDialog, SaveList.Text)
   else
   if SaveDlg.Execute then SaveList.SaveToFile(SaveDlg.FileName);
   SaveList.Free;
end;

procedure TSQLForm.acNewUpdate(Sender: TObject);
begin
   acNew.Enabled:= FormList.Count > 0;
   PageControl1.Visible:= acNew.Enabled;
   Splitter1.Visible:= acNew.Enabled;
end;

procedure TSQLForm.acFileSaveUpdate(Sender: TObject);
begin
   acFileSave.Enabled:= FormList.Count > 0;
end;

procedure TSQLForm.acGenerateSQLExecute(Sender: TObject);
begin
   Memo1.Lines.Text:= GenerateSQL;
   PageControl1.ActivePageIndex:= 3;
end;

procedure TSQLForm.acGenerateSQLUpdate(Sender: TObject);
begin
   acGenerateSQL.Enabled:= SQLGrid.CanGenerateSQL;
end;

procedure TSQLForm.acRunSQLExecute(Sender: TObject);
begin
   SQLDialog.DBEngine.ClearQuerySQL;
   SQLDialog.DBEngine.CloseResultQuery;
   SQLDialog.DBEngine.SetQuerySQL(Memo1.Text);
   Try
     Screen.Cursor:= crSQLWait;
     try
       SQLDialog.DBEngine.OpenResultQuery;
       PageControl1.ActivePageIndex:= 4;
     except
       on E: Exception do ShowSQLError(resMainWrongSQL, E.Message);
     end;
   Finally
     Screen.Cursor:= crDefault;
   end;
end;

procedure TSQLForm.acRunSQLUpdate(Sender: TObject);
begin
   acRunSQL.Enabled:= Memo1.Lines.Text <> EmptyStr;
end;

procedure TSQLForm.About1Click(Sender: TObject);
Var
  About: TSQLAboutBox;
begin
   ABout:= TSQLAboutBox.Create(Application);
   About.ShowModal;
   About.Free;
end;

procedure TSQLForm.acTableViewExecute(Sender: TObject);
begin
   ListBox1.Visible:= acTableView.Checked;
   Splitter2.Visible:= acTableView.Checked;
end;

procedure TSQLForm.acAcceptExecute(Sender: TObject);
begin
   SQLDialog.ResultSQL:= Memo1.Text;
   ModalResult:= mrOK;
end;

procedure TSQLForm.acAcceptUpdate(Sender: TObject);
begin
   acAccept.Enabled:= Memo1.Text <> EmptyStr;
end;

procedure TSQLForm.ToolButton9Click(Sender: TObject);
begin
   ModalResult:= mrCancel;
end;

procedure TSQLForm.GridButtonClick(Sender: TObject; ACol, ARow: Integer);
var
  Builder: TExpBuilder;
begin
   Builder:= TExpBuilder.Create(Self);
   Builder.FormList:= FormList;
   Builder.DBEngine:= SQLDialog.DBEngine;
   Builder.ExpResult:= ExpGrid.Cells[ACol, ARow];
   if Builder.ShowModal = mrOK then ExpGrid.Cells[ACol, ARow]:= Builder.ExpResult;
   Builder.Free;
end;

procedure TSQLForm.UpdateExpGrid;
var
  i: integer;
begin
   for i:= 1 to ExpGrid.RowCount - 1 do
   begin
      if (i = 1) then
      begin
        if (ComboBox1.ItemIndex = 2) or (ComboBox1.ItemIndex = 3) then ExpGrid.Cells[0, i]:= 'NOT'
        else ExpGrid.Cells[0, i]:= EmptyStr;
      end
      else
        ExpGrid.Cells[0, i]:= Trim(ExpParams[ComboBox1.ItemIndex]);
   end;
end;

procedure TSQLForm.DoRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
begin
   UpdateExpGrid;
end;

{ TCriteriaGrid }

procedure TCriteriaGrid.ClearGrid;
var
  i: integer;
begin
   for i:= RowCount - 1 downto 1 do
   begin
      Cells[1, i]:= EmptyStr;
      if i > 1 then RowCount:= RowCount - 1;
   end;
end;

constructor TCriteriaGrid.Create(AOwner: TComponent);
begin
  inherited;
  ColCount := 2;
  RowCount := 2;
  Align:= alClient;
end;

function TCriteriaGrid.CreateEditor: TInplaceEdit;
begin
   Result:= TExpEdit.Create(Self);
   TExpEdit(Result).OnEditButtonClick:= _OnClick;
end;

procedure TCriteriaGrid.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goRowMoving,
  goDrawFocusSelected];
  Height := Parent.ClientHeight;
end;

procedure TCriteriaGrid.CreateWnd;
begin
  inherited;
  DefaultRowHeight:= Canvas.TextHeight('Cg') + 5;
  ColWidths[0]:= 30;
end;

destructor TCriteriaGrid.Destroy;
begin
  Hide;
  inherited;
end;

function TCriteriaGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
   Result:= esEllipsis;
end;

procedure TCriteriaGrid.WMSize(var Message: TMessage);
begin
   inherited;
  ColWidths[1]:= (ClientWidth - ColWidths[0] - GridLineWidth * 4);
end;

procedure TCriteriaGrid._OnClick(Sender: TObject);
begin
   if Assigned(OnButtonClick) then FGridEvent(Self, Col, Row);
end;

procedure TSQLForm.ComboBox1CloseUp(Sender: TObject);
begin
   UpdateExpGrid;
end;

procedure TSQLForm.TabSheet3Resize(Sender: TObject);
begin
   Panel5.Left:= (TabSheet3.ClientWidth div 2) - Panel5.Width div 2;
   ListBox2.Width:= Panel5.Left;
   ListView1.Left:= Panel5.Left + Panel5.Width;
   ListView1.Width:= TabSheet3.ClientWidth - ListView1.Left;
   SpeedButton1.Left:= ListView1.Left;
   SpeedButton2.Left:= SpeedButton1.Left + SpeedButton1.Width;
end;

procedure TSQLForm.acSortAddExecute(Sender: TObject);
var
  MyItem: TListItem;
  i: integer;
begin
   for i:= 0 to ListBox2.Items.Count - 1 do
   begin
      if ListBox2.Selected[i] then
      begin
         MyItem:= ListView1.Items.Add;
         MyItem.Caption:= ListBox2.Items.Strings[i];
         MyItem.SubItems.Add('ASC');
      end;
   end;
   for i:= ListBox2.Items.Count - 1 downto 0 do
       if ListBox2.Selected[i] then ListBox2.Items.Delete(i);
end;

procedure TSQLForm.acSortAddUpdate(Sender: TObject);
begin
   acSortAdd.Enabled:= (ListBox2.Items.Count > 0) and (ListBox2.SelCount > 0);
end;

procedure TSQLForm.acSortRemoveExecute(Sender: TObject);
begin
   ListBox2.Items.Add(ListView1.Selected.Caption);
   ListView1.DeleteSelected;
end;

procedure TSQLForm.acSortRemoveUpdate(Sender: TObject);
begin
   acSortRemove.Enabled:= (ListView1.Items.Count > 0) and (ListView1.ItemIndex <> - 1);
end;

procedure TSQLForm.Button4Click(Sender: TObject);
begin
   ListView1.Selected.SubItems.Strings[0]:= 'DESC';
end;

procedure TSQLForm.acSortAZExecute(Sender: TObject);
begin
   ListView1.Selected.SubItems.Strings[0]:= 'ASC';
end;

procedure TSQLForm.acSortAZUpdate(Sender: TObject);
begin
   acSortAZ.Enabled:= ListView1.ItemIndex <> - 1;
end;

procedure TSQLForm.acSortZAExecute(Sender: TObject);
begin
   ListView1.Selected.SubItems.Strings[0]:= 'DESC';
end;

procedure TSQLForm.acSortZAUpdate(Sender: TObject);
begin
   acSortZA.Enabled:= ListView1.ItemIndex <> - 1;
end;

procedure TSQLForm.acSortUpExecute(Sender: TObject);
var
  Cap1, Cap2, Ord1, Ord2: String;
begin
   Cap1:= ListView1.Selected.Caption;
   Ord1:= ListView1.Selected.SubItems.Strings[0];
   Cap2:= ListView1.Items.Item[ListView1.Selected.index - 1].Caption;
   Ord2:= ListView1.Items.Item[ListView1.Selected.index - 1].SubItems.Strings[0];

   ListView1.Selected.Caption:= Cap2;
   ListView1.Selected.SubItems.Strings[0]:= Ord2;
   ListView1.Items.Item[ListView1.Selected.index - 1].Caption:= Cap1;
   ListView1.Items.Item[ListView1.Selected.index - 1].SubItems.Strings[0]:= Ord1;
   ListView1.Items.Item[ListView1.Selected.Index - 1].Selected:= True;
end;

procedure TSQLForm.acSortUpUpdate(Sender: TObject);
begin
   acSortUp.Enabled:= (ListView1.ItemIndex <> - 1) and (ListView1.Selected.Index > 0);
end;

procedure TSQLForm.acSortDownExecute(Sender: TObject);
var
  Cap1, Cap2, Ord1, Ord2: String;
begin
   Cap1:= ListView1.Selected.Caption;
   Ord1:= ListView1.Selected.SubItems.Strings[0];
   Cap2:= ListView1.Items.Item[ListView1.Selected.index + 1].Caption;
   Ord2:= ListView1.Items.Item[ListView1.Selected.index + 1].SubItems.Strings[0];

   ListView1.Selected.Caption:= Cap2;
   ListView1.Selected.SubItems.Strings[0]:= Ord2;
   ListView1.Items.Item[ListView1.Selected.index + 1].Caption:= Cap1;
   ListView1.Items.Item[ListView1.Selected.index + 1].SubItems.Strings[0]:= Ord1;
   ListView1.Items.Item[ListView1.Selected.Index + 1].Selected:= True;
end;

procedure TSQLForm.acSortDownUpdate(Sender: TObject);
begin
   acSortDown.Enabled:= (ListView1.ItemIndex <> - 1) and (ListView1.Selected.Index < ListView1.Items.Count - 1);
end;

procedure TSQLForm.SpeedButton5Click(Sender: TObject);
begin
   ExpGrid.RowCount:= ExpGrid.RowCount + 1;
   UpdateExpGrid;
end;

procedure TSQLForm.SpeedButton6Click(Sender: TObject);
begin
   if ExpGrid.RowCount > 2 then
   begin
      ExpGrid.Cells[1, ExpGrid.RowCount - 1]:= EmptyStr;
      ExpGrid.RowCount:= ExpGrid.RowCount - 1
   end
   else ExpGrid.Cells[1, 1]:= '';
   UpdateExpGrid;
end;

procedure TSQLForm.SpeedButton7Click(Sender: TObject);
begin
   ExpGrid.Cells[1, ExpGrid.Row]:= EmptyStr;
   if ExpGrid.Row > 1 then ExpGrid.DeleteRow(ExpGrid.Row);
   UpdateExpGrid;
end;

end.
