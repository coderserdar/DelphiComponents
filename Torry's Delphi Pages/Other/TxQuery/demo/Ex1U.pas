Unit Ex1U;

{$I XQ_FLAG.INC}
Interface

{.$DEFINE USE_DBF_ENGINE}
{.$DEFINE QUERYBUILDER}   { use query builder in the demo. Comment if not used }
Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, XQuery, Db, DBTables, StdCtrls, ComCtrls, Menus, ExtCtrls,
  DBCtrls, Buttons, QBaseExpr, SyntaxHi, XQMiscel, xqbase
{$IFDEF USE_DBF_ENGINE}
  , halcn6DB
  , gs6_shel
{$ENDIF}
{$IFDEF QUERYBUILDER}
  , QBuilder
  , OQBExQry
{$ENDIF}
  ;
  

Type
  TfrmTest = Class(TForm)
    About1: TMenuItem;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    ComboBox1: TComboBox;
    Contents1: TMenuItem;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    DBGrid5: TDBGrid;
    DBGrid6: TDBGrid;
    DBImage1: TDbImage;
    DBLabel1: TDbText;
    DBMemo1: TDbMemo;
    DBNavigator2: TDbNavigator;
    DBNavigator3: TDbNavigator;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DataSource4: TDataSource;
    DataSource5: TDataSource;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Help1: TMenuItem;
    Howtobuy1: TMenuItem;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    N5: TMenuItem;
    N6: TMenuItem;
    PageControl1: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PopupMenu2: TPopupMenu;
    RichEdit2: TRichEdit;
    RichEdit3: TRichEdit;
    RichEdit4: TRichEdit;
    SaveDialog1: TSaveDialog;
    Saveresultsetastext1: TMenuItem;
    StatusBar1: TStatusBar;
    SyntaxHighlighter1: TSyntaxHighlighter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Table1: TTable;
    Table2: TTable;
    Table3: TTable;
    Table4: TTable;
    Table5: TTable;
    XQuery1: TXQuery;
    XQuery2: TXQuery;
    xQuery3: TXQuery;
    xQuery4: TXQuery;
    Table2OrderNo: TFloatField;
    Table2CustNo: TFloatField;
    Table2SaleDate: TDateTimeField;
    Table2ShipDate: TDateTimeField;
    Table2EmpNo: TIntegerField;
    Table2ShipToContact: TStringField;
    Table2ShipToAddr1: TStringField;
    Table2ShipToAddr2: TStringField;
    Table2ShipToCity: TStringField;
    Table2ShipToState: TStringField;
    Table2ShipToZip: TStringField;
    Table2ShipToCountry: TStringField;
    Table2ShipToPhone: TStringField;
    Table2ShipVIA: TStringField;
    Table2PO: TStringField;
    Table2Terms: TStringField;
    Table2PaymentMethod: TStringField;
    Table2ItemsTotal: TCurrencyField;
    Table2TaxRate: TFloatField;
    Table2Freight: TCurrencyField;
    Table2AmountPaid: TCurrencyField;
    Table3OrderNo: TFloatField;
    Table3ItemNo: TFloatField;
    Table3PartNo: TFloatField;
    Table3Qty: TIntegerField;
    Table3Discount: TFloatField;
    Table4PartNo: TFloatField;
    Table4VendorNo: TFloatField;
    Table4Description: TStringField;
    Table4OnHand: TFloatField;
    Table4OnOrder: TFloatField;
    Table4Cost: TCurrencyField;
    Table4ListPrice: TCurrencyField;
    Table5SpeciesNo: TFloatField;
    Table5Category: TStringField;
    Table5Common_Name: TStringField;
    Table5SpeciesName: TStringField;
    Table5Lengthcm: TFloatField;
    Table5Length_In: TFloatField;
    Table5Notes: TMemoField;
    Table5Graphic: TGraphicField;
    Panel5: TPanel;
    Label2: TLabel;
    DBLabel2: TDbText;
    PageControlSQLExamples: TPageControl;
    TabSheetSQLString: TTabSheet;
    Panel8: TPanel;
    PanelSideButtons: TPanel;
    Button2: TButton;
    Button5: TButton;
    BtnQBuilder: TButton;
    ButtonRunSQL: TBitBtn;
    TabSheetResultDataSet: TTabSheet;
    Panel9: TPanel;
    DBGrid1: TDBGrid;
    Panel10: TPanel;
    DBNavigator1: TDbNavigator;
    Panel1: TPanel;
    Bar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    SaveDialog2: TSaveDialog;
    TreeView1: TTreeView;
    Panel11: TPanel;
    RichEdit1: TRichEdit;
    Splitter1: TSplitter;
    ChkParse: TCheckBox;
    Panel12: TPanel;
    MemoParse: TMemo;
    TreeView2: TTreeView;
    Button1: TButton;
    Table1CustNo: TFloatField;
    Table1Company: TStringField;
    Table1Addr1: TStringField;
    Table1Addr2: TStringField;
    Table1City: TStringField;
    Table1State: TStringField;
    Table1Zip: TStringField;
    Table1Country: TStringField;
    Table1Phone: TStringField;
    Table1FAX: TStringField;
    Table1TaxRate: TFloatField;
    Table1Contact: TStringField;
    Table1LastInvoiceDate: TDateTimeField;
    Table1_Campo: TIntegerField;
    Procedure ButtonRunSQLClick(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure XQuery1Progress(Sender: TObject; Status: TXProgressStatus; Min, Max, Position: Integer);
    Procedure Contents1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure About1Click(Sender: TObject);
    Procedure Howtobuy1Click(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Exit1Click(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure SyntaxHighlighter1PosChange(Sender: TObject; Row, Col: Integer);
    Procedure Button2Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure XQuery1CreateIndex(Sender: TObject; Unique, Descending: Boolean; Const TableName, IndexName: String; ColumnExprList: TStringList);
    Procedure XQuery1DropTable(Sender: TObject; Const TableName: String);
    Procedure XQuery1DropIndex(Sender: TObject; Const TableName, IndexName: String);
    Procedure XQuery1CreateTable(Sender: TObject; CreateTable: TCreateTableItem);
    Procedure XQuery1SyntaxError(Sender: TObject; Const ErrorMsg, OffendingText: String; LineNum, ColNum, TextLen: Integer);
    Procedure Button12Click(Sender: TObject);
    Procedure BtnQBuilderClick(Sender: TObject);
    Procedure Saveresultsetastext1Click(Sender: TObject);
    Procedure PageControlSQLExamplesChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure XQuery1AlterTable(Sender: TObject;
      CreateTable: TCreateTableItem);
    procedure XQuery1ResolveDataset(Sender: TObject;
      const Filename: String; var ATableName: String;
      var Dataset: TDataSet);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure ChkParseClick(Sender: TObject);
    procedure TreeView2Click(Sender: TObject);
    procedure XQuery1SetRange(Sender: TObject;
      RelOperator: TRelationalOperator; DataSet: TDataSet;
      const FieldNames, StartValues, EndValues: String;
      IsJoining: Boolean);
    procedure XQuery1CancelRange(Sender: TObject; DataSet: TDataSet;
      IsJoining: Boolean);
    procedure XQuery1IndexNeededFor(Sender: TObject; DataSet: TDataSet;
      const FieldNames: String; ActivateIndex, IsJoining: Boolean;
      var Accept: Boolean);
    procedure XQuery1CancelFilter(Sender: TObject; DataSet: TDataSet;
      IsJoining: Boolean);
    procedure XQuery1SetFilter(Sender: TObject; DataSet: TDataSet;
      const Filter: String; IsJoining: Boolean; var Handled: Boolean);
    procedure xQuery4BeforeInsert(DataSet: TDataSet);
    procedure XQuery1UDFCheck(Sender: TObject; const Identifier: String;
      Params: TParameterList; var DataType: TExprType; var MaxLen: Integer;
      var Accept: Boolean);
    procedure XQuery1UDFSolve(Sender: TObject; const Identifier: String;
      Params: TParameterList; var Value: Variant);
    procedure Button1Click(Sender: TObject);
    procedure XQuery1CancelQuery(Sender: TObject; var Cancel: Boolean);
  Private
    fAnalizer: TSqlAnalizer;
    {$IFDEF QUERYBUILDER}
    fOQBDialog: TOQBuilderDialog;
    fOQBxQuery: TOQBEnginexQry;
    {$ENDIF}
    {$IFNDEF LEVEL3}
    Procedure PopupDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    Procedure PopupMeasureItem(Sender: TObject; ACanvas: TCanvas; Var Width, Height: Integer);
    {$ENDIF}
    procedure SelectionChange;
    procedure ParseSQL;
    procedure AnalizerChange;
  Public
  End;

Var
  frmTest: TfrmTest;

Implementation

{$R *.DFM}

Uses
  DemoAb, DemoReg, BDE, XQYacc, HTMLQry, ShellApi;

Var
  Start: Boolean = True;

Const
  CRLF = #13#10;
  (* Examples of SQL statements supported (some of them not useful at all but will show you the syntax supported)  *)

Procedure TfrmTest.ButtonRunSQLClick(Sender: TObject);
Begin
  PageControlSQLExamples.ActivePage := TabSheetResultDataSet;
  XQuery1.Close;
  RichEdit1.WordWrap := False; //There is a silly bug in RichEdit. It returns wrapped lines as new lines.
  XQuery1.Sql.Assign(RichEdit1.Lines);
  RichEdit1.WordWrap := True; //We're havin wordwrapping again
  XQuery1.Filtered := False;
  If ButtonRunSQL.Caption = 'Run SQL' Then
    XQuery1.Open //Run SQL
  Else
    XQuery1.ExecSql; //Exec SQL
  ButtonRunSQL.Enabled := False;

  { User wants to parse the SQL statement }
  if ChkParse.Checked then
  begin
    MemoParse.Lines.Clear;
    ParseSQL;
  end;
End;

Procedure TfrmTest.Button3Click(Sender: TObject);
Begin
  XQuery1.Filter   := ComboBox1.Text;
  XQuery1.Filtered := True;
End;

Procedure TfrmTest.Button4Click(Sender: TObject);
Begin
  XQuery2.Close;
  XQuery2.Sql.SetText(PChar(RichEdit2.Text));
  XQuery2.Open;
End;

Procedure TfrmTest.XQuery1Progress(Sender: TObject; Status: TXProgressStatus; Min, Max, Position: Integer);
Begin

  Case Status Of
    PsXStart: Begin
        If (Min = 0) Or (Max = 0) Then Exit;
        Bar1.Visible := True;
        Bar1.Min := Min;
        Bar1.Max := Max;
        Bar1.Position := Position;
      End;
    PsXProgress: Begin
        If Position = 0 Then Exit;
        Bar1.Position := Position;
      End;
    PsXEnd: Begin
        Bar1.Visible := False;
      End;
  End;
End;

Procedure TfrmTest.Contents1Click(Sender: TObject);
Begin
  Application.HelpCommand(HELP_CONTENTS, 0);
End;

Procedure TfrmTest.FormCreate(Sender: TObject);
var
  TreeNode: TTreeNode;
Begin

  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'txquery.hlp';
  ComboBox1.ItemIndex := 0;
  {$IFNDEF LEVEL3}
  PopupMenu2.OwnerDraw := True;
  {$ELSE}
  Button5.Visible := False;
  {$ENDIF}

  with TreeView1.Items do
  begin
     AddObject(nil, 'BETWEEN',Pointer(1));
     TreeNode:= Add(nil, 'DISTINCT');
        AddChildObject(TreeNode, 'Agregate',Pointer(2));
        AddChildObject(TreeNode, 'Record',Pointer(3));
     TreeNode:= Add(nil, 'GROUP BY');
        AddChildObject(TreeNode, 'Sample 1',Pointer(4));
        AddChildObject(TreeNode, 'Sample 2',Pointer(5));
        AddChildObject(TreeNode, 'Sample 2',Pointer(6));
     AddObject(nil, 'IN',Pointer(7));
     TreeNode:= Add(nil, 'JOIN');
        AddChildObject(TreeNode, 'Sample',Pointer(8));
        AddChildObject(TreeNode, 'SELECT',Pointer(9));
        AddChildObject(TreeNode, 'WHERE',Pointer(10));
        AddChildObject(TreeNode, 'Multiple Fields',Pointer(11));
        AddChildObject(TreeNode, 'Table Alias',Pointer(12));
     TreeNode:= Add(nil, 'LIKE');
        AddChildObject(TreeNode, 'Sample 1',Pointer(13));
        AddChildObject(TreeNode, 'Sample 2',Pointer(14));
        AddChildObject(TreeNode, 'Sample 3',Pointer(15));
     AddObject(nil, 'UNION',Pointer(16));
     TreeNode:= Add(nil, 'ORDER BY');
        AddChildObject(TreeNode, 'Sample 1',Pointer(17));
        AddChildObject(TreeNode, 'Sample 2',Pointer(18));
        AddChildObject(TreeNode, 'Sample 3',Pointer(19));
     AddObject(nil, 'SELECT',Pointer(20));
     TreeNode:= Add(nil, 'FUNCTIONS');
        AddChildObject(TreeNode, 'UDFs',Pointer(21));
        AddChildObject(TreeNode, 'Solved in event',Pointer(22));
     TreeNode:= Add(nil, 'SUBQUERIES');
        AddChildObject(TreeNode, 'Sample 1',Pointer(23));
        AddChildObject(TreeNode, 'Sample 2',Pointer(24));
        AddChildObject(TreeNode, 'In SELECT clause',Pointer(25));
        AddChildObject(TreeNode, 'In FROM clause',Pointer(36));
     TreeNode:= Add(nil, 'TRANSFORM...PIVOT');
        AddChildObject(TreeNode, 'Sample 1',Pointer(26));
        AddChildObject(TreeNode, 'Sample 2',Pointer(27));
        AddChildObject(TreeNode, 'Sample 3',Pointer(28));
     AddObject(nil, 'CREATE TABLE',Pointer(29));
     AddObject(nil, 'ALTER TABLE',Pointer(30));
     AddObject(nil, 'CREATE INDEX',Pointer(31));
     AddObject(nil, 'DROP TABLE',Pointer(32));
     AddObject(nil, 'DROP INDEX',Pointer(33));
     AddObject(nil, 'CASE...WHEN...ELSE...END', Pointer(34));
     AddObject(nil, 'OnResolveDataset event',Pointer(35));
     //AddObject(nil, 'CASE',Pointer(35));
  end;
  TreeView1.FullExpand;

  Table1.Open;
  Table2.Open;
  Table3.Open;
  Table4.Open;
  Table5.Open;

End;

Procedure TfrmTest.About1Click(Sender: TObject);
Begin
  With TFrmAbout.Create(Application) Do Begin
    Try
      ShowModal;
    Finally
      Free;
    End;
  End;
End;

Procedure TfrmTest.Howtobuy1Click(Sender: TObject);
Begin
  With TfrmRegister.Create(Application) Do Begin
    Try
      ShowModal;
    Finally
      Free;
    End;
  End;
End;

Procedure TfrmTest.FormDestroy(Sender: TObject);
Begin
  if Assigned(fAnalizer) then fAnalizer.Free;
  Application.HelpCommand(HELP_QUIT, 0);
End;

Procedure TfrmTest.Exit1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmTest.PageControl1Change(Sender: TObject);
Begin
  If PageControl1.ActivePage = TabSheet2 Then Begin
  End Else If PageControl1.ActivePage = TabSheet1 Then SyntaxHighlighter1.Editor := RichEdit1
  Else If PageControl1.ActivePage = TabSheet3 Then SyntaxHighlighter1.Editor := RichEdit2
  Else If PageControl1.ActivePage = TabSheet4 Then SyntaxHighlighter1.Editor := RichEdit3
  Else If PageControl1.ActivePage = TabSheet5 Then SyntaxHighlighter1.Editor := RichEdit4;
  If Not (PageControl1.ActivePage = TabSheet5) Then XQuery4.Close;
End;

Procedure TfrmTest.SyntaxHighlighter1PosChange(Sender: TObject;
  Row, Col: Integer);
Begin
  StatusBar1.SimpleText := Format('Row: %d Col: %d', [Row, Col]);
End;

Procedure TfrmTest.Button2Click(Sender: TObject);
Begin
  SyntaxHighlighter1.EditColorSet;
End;

Procedure TfrmTest.Button5Click(Sender: TObject);
Var
  TmpPt: TPoint;
  Item: TMenuItem;
  ColorElement: PColorElement;
  I: Integer;
  g: TElementGroup;
Begin
  TmpPt := PanelSideButtons.ClientToScreen(Point(Button5.Left, Button5.Top + Button5.Height));
  For I := 0 To PopupMenu2.Items.Count - 1 Do PopupMenu2.Items.Delete(0);

  For g := Low(TElementGroup) To High(TElementGroup) Do Begin
    For I := 0 To SyntaxHighlighter1.ColorConfig.ColorSettings.Count - 1 Do Begin
      ColorElement := PColorElement(SyntaxHighlighter1.ColorConfig.ColorSettings[I]);
      If ColorElement^.Group = g Then
      Begin
        Item := TMenuItem.Create(Self);
        {$IFNDEF LEVEL3}
        Item.OnDrawItem := PopupDrawItem; // Delphi 3 cannot owner draw in menus
        Item.OnMeasureItem := PopupMeasureItem;
        {$ENDIF}
        Item.Tag := I;
        Case ColorElement^.Group Of
          IdWhiteSpace: Item.Caption := 'WhiteSpace';
          IdComment: Item.Caption := 'Comment';
          IdReservedWord: Item.Caption := 'ReservedWord';
          IdIdentifier: Item.Caption := 'Identifier';
          IdTable: Item.Caption := 'Dataset';
          IdField: Item.Caption := 'Field';
          IdString: Item.Caption := 'String';
          IdNumber: Item.Caption := 'Number';
          IdComma: Item.Caption := 'Comma';
          IdParenthesis: Item.Caption := 'Parenthesis';
          IdOperator: Item.Caption := 'Operator';
          IdSemicolon: Item.Caption := 'Semicolon';
          IdPeriod: Item.Caption := 'Period';
        End;
        PopupMenu2.Items.Add(Item);
      End;
    End;
  End;
  PopupMenu2.Popup(TmpPt.X, TmpPt.Y);
End;

{$IFNDEF LEVEL3}

Procedure TfrmTest.PopupDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
Var
  S: String;
  TmpRect: TRect;
  ColorElement: PColorElement;
Begin
  With ACanvas Do Begin
    FillRect(ARect);
    S := (Sender As TMenuItem).Caption;
    ReplaceString(S, '&', '');
    TmpRect := ARect;
    TmpRect.Left := TextWidth('X') * 2;
    //TextOut(TmpRect.Left, TmpRect.Top, (Sender as TMenuItem).Caption);
    DrawText(Handle, PChar(S), -1, TmpRect, DT_SINGLELINE Or DT_LEFT Or DT_VCENTER);
    ColorElement := PColorElement(SyntaxHighlighter1.ColorConfig.ColorSettings[(Sender As TMenuItem).Tag]);
    TmpRect := ARect;
    TmpRect.Right := TextWidth('X') * 2 - 1;
    InflateRect(TmpRect, -2, -2);
    Brush.Color := ColorElement^.ForeColor;
    FillRect(TmpRect);
  End;
End;

Procedure TfrmTest.PopupMeasureItem(Sender: TObject;
  ACanvas: TCanvas; Var Width, Height: Integer);
Begin
  With ACanvas Do Begin
    Height := TextHeight('0') + 2;
    Width := TextWidth('X') * 13 + 2;
  End;
End;
{$ENDIF}

Procedure TfrmTest.Button6Click(Sender: TObject);
Begin
  XQuery1.Find(Edit1.Text);
End;

Procedure TfrmTest.Button7Click(Sender: TObject);
Begin
  If XQuery3.ParamCount = 0 Then
  Begin
    XQuery3.Params.CreateParam(FtInteger, 'LOWRANGE', PtUnknown);
    XQuery3.Params.CreateParam(FtInteger, 'HIGHRANGE', PtUnknown);
  End;
  XQuery3.Close;
  XQuery3.ParamByName('LOWRANGE').AsInteger := StrToInt(Edit2.Text);
  XQuery3.ParamByName('HIGHRANGE').AsInteger := StrToInt(Edit3.Text);
  XQuery3.Open;
End;

Procedure TfrmTest.Button8Click(Sender: TObject);
Begin
  If xQuery4.ParamCount = 0 Then
    xQuery4.Params.CreateParam(FtFloat, 'CustNo', PtUnknown);
  xQuery4.Close;
  xQuery4.Open;
End;

Procedure TfrmTest.XQuery1CreateIndex(Sender: TObject; Unique, Descending: Boolean; Const TableName, IndexName: String; ColumnExprList: TStringList);
Var
  S, TempS: String;
  I: Integer;
Begin
  S := 'Requested to create an index on table : ' + TableName + CRLF + 'Index name on this table : ' + IndexName + CRLF;
  If Unique Then TempS := 'Index Unique ' Else TempS := 'Duplicates allowed ';

  S := S + TempS + CRLF;
  If Descending Then TempS := 'Sort descending ' Else TempS := 'Sort ascending';

  S := S + TempS + CRLF + 'Columns expressions to index on :' + CRLF;

  For I := 0 To ColumnExprList.Count - 1 Do S := S + ColumnExprList[I] + CRLF;
  ShowMessage(S);
End;

Procedure TfrmTest.XQuery1DropTable(Sender: TObject; Const TableName: String);
Begin
  ShowMessage('Requested to drop table ' + TableName);
End;

Procedure TfrmTest.XQuery1DropIndex(Sender: TObject; Const TableName, IndexName: String);
Begin
  ShowMessage('Requested to drop index ' + IndexName + ' on table ' + TableName);
End;

Procedure TfrmTest.XQuery1CreateTable(Sender: TObject;
  CreateTable: TCreateTableItem);
Var
  S, BlobType: String;
  I: Integer;
  {$IFDEF USE_DBF_ENGINE}
  FieldList: TStringList;
  FileName, FieldName, IndexFileName: String;
  FieldType: Char;
  FieldSize, FieldDec: Integer;
  Halc: THalcyonDataSet;
  {$ENDIF}
Begin
  S := 'SQL statement issued:' + CRLF + XQuery1.Sql.Text + CRLF;
  ShowMessage(S);

  S := 'Analisis of CREATE TABLE statement:' + CRLF + Format('CREATE TABLE requested on table "%s"', [CreateTable.TableName]) + CRLF + Format('Number of fields to create : %d', [CreateTable.FieldCount]) + CRLF;

  For I := 0 To CreateTable.FieldCount - 1 Do Begin
    S := S + Format('"%s" ', [CreateTable.Fields[I].FieldName]);
    Case CreateTable.Fields[I].FieldType Of // list of possible types accepted in TxQuery parser
      RW_CHAR: S := S + Format('type CHAR of Length %d', [CreateTable.Fields[I].Size]) + CRLF; // use Size property here
      RW_INTEGER: S := S + 'type INTEGER' + CRLF;
      RW_SMALLINT: S := S + 'type SMALLINT' + CRLF;
      RW_BOOLEAN: S := S + 'type BOOLEAN' + CRLF;
      RW_DATE: S := S + 'type DATE' + CRLF;
      RW_TIME: S := S + 'type TIME' + CRLF;
      RW_DATETIME: S := S + 'type DATETIME' + CRLF;
      RW_MONEY: S := S + 'type MONEY' + CRLF;
      RW_FLOAT: S := S + Format('type FLOAT Scale %d Precision %d', [CreateTable.Fields[I].Scale, CreateTable.Fields[I].Precision]) + CRLF; // use Scale and Precision properties here
      RW_AUTOINC: S := S + 'type AUTOINC' + CRLF;
      RW_BLOB: Begin // use BlobType property here
          Case CreateTable.Fields[I].BlobType Of
            1: BlobType := 'Memo';
            2: BlobType := 'Binary';
            3: BlobType := 'Formatted Memo';
            4: BlobType := 'OLE';
            5: BlobType := 'Graphic/Binary';
          End;
          S := S + Format('is a BLOB of type %s', [BlobType]) + CRLF;
        End;
    End;
  End;

  S := S + CRLF + 'SORT ORDER:' + CRLF;

  If CreateTable.PrimaryKey.Count = 0 Then S := S + 'NONE'
  Else Begin
    For I := 0 To CreateTable.PrimaryKey.Count - 1 Do S := S + CreateTable.PrimaryKey[I] + CRLF;
  End;

  ShowMessage(S);

  {$IFDEF USE_DBF_ENGINE} // A working example with Halcyon
  FieldList := TStringList.Create;
  Try
    For I := 0 To CreateTable.FieldCount - 1 Do Begin
      FieldName := CreateTable.Fields[I].FieldName;
      Case CreateTable.Fields[I].FieldType Of // list of possible types accepted in TxQuery parser
        RW_CHAR: Begin
            FieldType := 'C';
            FieldSize := CreateTable.Fields[I].Size;
            FieldDec := 0;
          End;
        RW_INTEGER, RW_AUTOINC: Begin
            FieldType := 'N';
            FieldSize := 11;
            FieldDec := 0;
          End;
        RW_SMALLINT: Begin
            FieldType := 'N';
            FieldSize := 6;
            FieldDec := 0;
          End;
        RW_BOOLEAN: Begin
            FieldType := 'L';
            FieldSize := 1;
            FieldDec := 0;
          End;
        RW_DATE, RW_TIME, RW_DATETIME: Begin
            FieldType := 'D';
            FieldSize := 10;
            FieldDec := 0;
          End;
        RW_MONEY, RW_FLOAT: Begin
            FieldType := 'N';
            If CreateTable.Fields[I].Scale = 0 Then Begin
              FieldSize := 20;
              FieldDec := 4;
            End Else Begin
              FieldSize := CreateTable.Fields[I].Scale;
              FieldDec := CreateTable.Fields[I].Precision;
            End;
          End;
        RW_BLOB: Begin // use BlobType property here
            Case CreateTable.Fields[I].BlobType Of
              1, 3: FieldType := 'M'; // Memo, Formatted Memo
              2, 4: FieldType := 'B'; // Binary, OLE
              5: FieldType := 'G'; // Graphic/Binary
            End;
            FieldSize := 8;
            FieldDec := 0;
          End;
      End;
      FieldList.Add(Format('%s;%s;%d;%d', [FieldName, FieldType, FieldSize, FieldDec]));
    End;
    FileName := CreateTable.TableName;
    gs6_shel.CreateDBF(FileName, '', FoxPro2, FieldList); // change FoxPro2 to your choice
    If CreateTable.PrimaryKey.Count > 0 Then Begin
      S := CreateTable.PrimaryKey[0];
      For I := 1 To CreateTable.PrimaryKey.Count - 1 Do S := S + '+' + CreateTable.PrimaryKey[I];
      Halc := THalcyonDataSet.Create(Nil);
      Try
        Halc.DataBaseName := ExtractFilePath(FileName);
        Halc.TableName := ExtractFileName(FileName);
        Halc.Open;
        IndexFileName := ChangeFileExt(FileName, '.cdx');
        Halc.IndexOn(IndexFileName, 'PRIMARY', S, '.NOT.DELETED()', Halcn6DB.Unique, Halcn6DB.Ascending); // optionl
      Finally
        Halc.Free;
      End;
    End;
    ShowMessage(Format('Table %s was successful created', [FileName]));
  Finally
    FieldList.Free;
  End;
  {$ENDIF}
End;

Procedure TfrmTest.XQuery1SyntaxError(Sender: TObject; Const ErrorMsg, OffendingText: String; LineNum, ColNum, TextLen: Integer);
Var
  I, NumChars: Integer;
Begin
  ShowMessage(ErrorMsg + ' at or before ' + OffendingText + Format(' Line %d, Column %d', [LineNum, ColNum]));
  { Will not show or use ErrorMsg parameter }
  NumChars := 0;
  I := 0;
  While I < LineNum - 1 Do Begin
    Inc(NumChars, Length(RichEdit1.Lines[I]) + 2);
    Inc(I);
  End;
  PageControlSQLExamples.ActivePage := TabSheetSQLString;
  RichEdit1.SelStart := NumChars + ColNum;
  RichEdit1.SelLength := TextLen;
  RichEdit1.SetFocus;
End;

Procedure TfrmTest.Button12Click(Sender: TObject);
Begin
  If Not XQuery1.Active Then Exit;
End;

Procedure TfrmTest.BtnQBuilderClick(Sender: TObject);
Begin
{$IFDEF QUERYBUILDER}
  If Not Assigned(FOQBDialog) Then Begin
    FOQBDialog := TOQBuilderDialog.Create(Self);
    FOQBxQuery := TOQBEnginexQry.Create(Self);
    FOQBxQuery.XQuery := XQuery1;
    FOQBxQuery.UseTableAliases := True;
    FOQBDialog.OQBEngine := FOQBxQuery;
  End;
  If FOQBDialog.Execute Then RichEdit1.Lines.Text := FOQBDialog.Sql.Text;
{$ELSE}
  ShowMessage('If you want to use the query builder option,' + CRLF +
    'You must download the software from this URL : ' + CRLF +
    'http://www.geocities.com/SiliconValley/Way/9006/index.html' + CRLF +
    'After downloading see help file for details searching for:' + CRLF +
    '"Query builder"' + CRLF +
    'After that, enable optional compilation switch QUERYBUILDER on' + CRLF +
    'top of this form');
{$ENDIF}
End;

Procedure TfrmTest.Saveresultsetastext1Click(Sender: TObject);
Var
  FieldNames: TStringList;
Begin
  If Not XQuery1.Active Or Not SaveDialog1.Execute Then Exit;
  FieldNames := TStringList.Create;
  Try
    XQuery1.WriteToTextFile(SaveDialog1.FileName, // save to this file
      '"', // field delim character
      ',', // text separator
      False, // true = CSV format, false = text only
      FieldNames); // empty = all fields
  Finally
    FieldNames.Free;
  End;
End;

Procedure TfrmTest.PageControlSQLExamplesChange(Sender: TObject);
Begin
  ButtonRunSQL.Enabled := (PageControlSQLExamples.ActivePage = TabSheetSQLString) And (Trim(RichEdit1.Text) <> '');
End;

procedure TfrmTest.SpeedButton1Click(Sender: TObject);
var
  ViewerHandle: HWND;
begin
  if not savedialog2.execute then exit;
  with THTMLExport.Create(nil) do begin
    try
      // add your header here
      Header.Add('First test exporting a TxQuery dataset');
      Header.Add('Date : '+FormatDateTime('dd/mmm/yyyy',Now));
      // add your footer here
      Footer.Add('Author: Alfonso Moreno');
      Footer.Add('<A HREF="http://www.sigmap.com/txquery.htm">http://www.sigmap.com/txquery.htm</A>');
      Footer.Add('mailto: <A HREF="mailto:amoreno@sigmap.com">amoreno@sigmap.com</A>');
      Title.Add('Test exporting a TxQuery dataset');
      DataSet:= XQuery1;
      SaveToFile(SaveDialog2.FileName);
    finally
      free;
    end;
  end;
  ViewerHandle := ShellExecute(0, 'open', PChar(SaveDialog2.FileName), nil, nil, SW_SHOWNORMAL);
  SetForegroundWindow(ViewerHandle);
end;

procedure TfrmTest.XQuery1AlterTable(Sender: TObject;
  CreateTable: TCreateTableItem);
Var
  S, BlobType: String;
  I: Integer;
Begin
  S := 'SQL statement issued:' + CRLF + XQuery1.Sql.Text + CRLF;
  ShowMessage(S);

  S := 'Analisis of ALTER TABLE statement:' + CRLF +
      Format('ALTER TABLE requested on table "%s"', [CreateTable.TableName]) + CRLF +
         Format('Number of fields to alter/drop : %d', [CreateTable.FieldCount]) + CRLF;

  For I := 0 To CreateTable.FieldCount - 1 Do Begin
    if CreateTable.Fields[I].MustDrop then
       S := S + Format('DROP FIELD "%s" ', [CreateTable.Fields[I].FieldName]) +CRLF
    else
       S := S + Format('ADD FIELD "%s" ', [CreateTable.Fields[I].FieldName]) +CRLF;
    Case CreateTable.Fields[I].FieldType Of // list of possible types accepted in TxQuery parser
      RW_CHAR: S := S + Format('type CHAR of Length %d', [CreateTable.Fields[I].Size]) + CRLF; // use Size property here
      RW_INTEGER: S := S + 'type INTEGER' + CRLF;
      RW_SMALLINT: S := S + 'type SMALLINT' + CRLF;
      RW_BOOLEAN: S := S + 'type BOOLEAN' + CRLF;
      RW_DATE: S := S + 'type DATE' + CRLF;
      RW_TIME: S := S + 'type TIME' + CRLF;
      RW_DATETIME: S := S + 'type DATETIME' + CRLF;
      RW_MONEY: S := S + 'type MONEY' + CRLF;
      RW_FLOAT: S := S + Format('type FLOAT Scale %d Precision %d', [CreateTable.Fields[I].Scale, CreateTable.Fields[I].Precision]) + CRLF; // use Scale and Precision properties here
      RW_AUTOINC: S := S + 'type AUTOINC' + CRLF;
      RW_BLOB: Begin // use BlobType property here
          Case CreateTable.Fields[I].BlobType Of
            1: BlobType := 'Memo';
            2: BlobType := 'Binary';
            3: BlobType := 'Formatted Memo';
            4: BlobType := 'OLE';
            5: BlobType := 'Graphic/Binary';
          End;
          S := S + Format('is a BLOB of type %s', [BlobType]) + CRLF;
        End;
    End;
  End;

  ShowMessage(S);

  // the implementation in specific database is cumbersome :-)

end;

procedure TfrmTest.XQuery1ResolveDataset(Sender: TObject;
  const Filename: String; var ATableName: String; var Dataset: TDataSet);
begin
   dataset := TTable.Create(nil);
   with TTable(dataset) do
   begin
      DatabaseName := ExtractFilePath(Filename);
      TableName := ExtractFileName(Filename);
      // this will be the table name in the result set (return value)
      if Length(ATableName)=0 then
         ATableName := ChangeFileExt(ExtractFileName(Filename),'');
      Open;
   end;
end;

procedure TfrmTest.TreeView1Click(Sender: TObject);
begin
   SelectionChange;
end;

procedure TfrmTest.SelectionChange;
var
  Selected: TTreeNode;
begin
  Selected := TreeView1.Selected;
  if (Selected=nil) or (Longint(Selected.Data)=0) then begin
     RichEdit1.Lines.Clear;
     exit;
  end;
  RichEdit1.Lines.Clear;
  ButtonRunSQL.Caption := 'Run SQL';
  SyntaxHighlighter1.Editor := Nil;
  //Table1.First;
  Case Longint(Selected.Data)-1 Of
    00: Begin //BETWEEN
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT customer.* FROM customer '
                          + 'WHERE LastInvoiceDate '
                          + 'BETWEEN #01/01/1990# '
                          + 'AND #12/31/1994# ORDER BY '
                          + 'Customer.LastInvoiceDate');
        RichEdit1.Lines.EndUpdate;
      End;
    01: Begin //DISTINCT Aggregate
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('Select Count(DISTINCT Country) '
                          + 'FROM Customer;');
        RichEdit1.Lines.EndUpdate;
      End;
    02: Begin //DISTINCT Record
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('Select DISTINCT Country '
                          + 'FROM Customer ORDER BY Country;');
        RichEdit1.Lines.EndUpdate;
      End;
    03: Begin //GROUP BY and Aggregates: Sample 1
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT c.CustNo, CAST(Sum(AmountPaid) '
                          + 'AS MONEY) As SumAmount, '
                          + 'CAST(Avg(AmountPaid) AS MONEY), '
                          + 'CAST(Min(AmountPaid) AS MONEY), '
                          + 'Count(*) FROM Customer c, Orders o '
                          + 'WHERE (c.CustNo = o.CustNo) '
                          + 'GROUP BY c.CustNo HAVING Sum(AmountPaid) > 80000;');
        RichEdit1.Lines.EndUpdate;
      End;
    04: Begin //GROUP BY and Aggregates: Sample 2
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT CUSTNO, SUM(AMOUNTPAID) / SUM(ITEMSTOTAL) '
                          + 'As Rate '
                          + 'FROM ORDERS GROUP BY CUSTNO;');
        RichEdit1.Lines.EndUpdate;
      End;
    05: Begin //GROUP BY and Aggregates: Sample 3
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT CAST(SUM(ItemsTotal) / COUNT(CustNo) AS MONEY) '
                          + 'FROM ORDERS;');
        RichEdit1.Lines.EndUpdate;
      End;
    06: Begin //IN
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM customer WHERE City IN ('
                          + '"Bogota", "Sarasota", "Freeport", "Tampa", '
                          + '"Somerset", "Honolulu");');
        RichEdit1.Lines.EndUpdate;
      End;
    07: Begin //JOIN
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM Customer c, Orders o, Items i, ');
        RichEdit1.Lines.Add('Parts p WHERE (c.CustNo = o.CustNo) And ');
        RichEdit1.Lines.Add('(o.OrderNo = i.OrderNo) And (i.PartNo = p.PartNo) ');
        RichEdit1.Lines.Add('And c.CustNo > 1300 AND c.CustNo < 2000;');
        RichEdit1.Lines.EndUpdate;
      End;
    08: Begin //JOIN In SELECT
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM Customer INNER JOIN Orders ON ');
        RichEdit1.Lines.Add('(Customer.CustNo = Orders.CustNo)');
        RichEdit1.Lines.Add(' INNER JOIN Items ON (Orders.OrderNo = Items.OrderNo)');
        RichEdit1.Lines.Add(' INNER JOIN Parts ON (Items.PartNo = Parts.PartNo);');
        RichEdit1.Lines.EndUpdate;
      End;
    09: Begin //JOIN In WHERE
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM Customer c, Orders o, Items i, ');
        RichEdit1.Lines.Add('Parts p ');
        RichEdit1.Lines.Add('WHERE c.CustNo = o.CustNo And o.OrderNo = i.OrderNo ');
        RichEdit1.Lines.Add('And i.PartNo = p.PartNo;');
        RichEdit1.Lines.EndUpdate;
      End;
    10: Begin //JOIN With Multiple Fields
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('/* This is only to illustrate multiple fields joining */ ');
        RichEdit1.Lines.Add('SELECT * FROM Customer c INNER JOIN Orders o ON ');
        RichEdit1.Lines.Add('c.CustNo = o.CustNo ');
        RichEdit1.Lines.Add('INNER JOIN Items i ');
        RichEdit1.Lines.Add('ON o.OrderNo = i.OrderNo;');
        RichEdit1.Lines.EndUpdate;
      End;
    11: Begin //JOIN With Table Alias
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM Customer c ');
        RichEdit1.Lines.Add('INNER JOIN Orders o ON (c.CustNo = o.CustNo)');
        RichEdit1.Lines.Add(' INNER JOIN Items i ON (o.OrderNo = i.OrderNo)');
        RichEdit1.Lines.Add(' INNER JOIN Parts p ON (i.PartNo = p.PartNo);');
        RichEdit1.Lines.EndUpdate;
      End;
    12: Begin //LIKE: Sample 1
        RichEdit1.Text := 'SELECT * FROM Customer WHERE Company LIKE "%Under%";';
      End;
    13: Begin //LIKE: Sample 2
        RichEdit1.Text := 'SELECT * FROM Customer WHERE Company LIKE "%C_ub";';
      End;
    14: Begin //LIKE: Sample 3
        RichEdit1.Text := 'SELECT * FROM Customer WHERE company LIKE ''A%Under%'';';
      End;
    15: Begin //UNION
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('/* Syntax is: ');
        RichEdit1.Lines.Add('select_statement_1 UNION select_statement_2 ');
        RichEdit1.Lines.Add('You can also combine different tables ');
        RichEdit1.Lines.Add('but they must have compatible column types */');
        RichEdit1.Lines.Add('SELECT * FROM customer WHERE custno ');
        RichEdit1.Lines.Add('BETWEEN 1000 AND 2500; ');
        RichEdit1.Lines.Add('UNION SELECT * FROM customer WHERE custno ');
        RichEdit1.Lines.Add('BETWEEN 2000 AND 3000;');
        RichEdit1.Lines.EndUpdate;
      End;
    16: Begin //ORDER BY: Sample 1
        RichEdit1.Text := 'SELECT CustNo, Company, Addr1, Addr2 FROM customer ORDER BY custno DESC;';
      End;
    17: Begin //ORDER BY: Sample 2
        RichEdit1.Text := 'SELECT CustNo, Company, Addr1, Addr2 FROM customer ORDER BY 2 DESC;';
      End;
    18: Begin //ORDER BY: Sample 3
        RichEdit1.Text := 'SELECT CustNo, Company, City FROM Customer ORDER BY City, 2 DESC;';
      End;
    19: Begin //SELECT
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('/* TXQuery dataset demo');
        RichEdit1.Lines.Add('Version 1.80 */');
        RichEdit1.Lines.Add('SELECT * FROM Customer;');
        RichEdit1.Lines.EndUpdate;
      End;
    20: Begin //Some Functions
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT LOWER(TRIM(TRAILING "a" FROM company)) As TrimmedAndLowered, ');
        RichEdit1.Lines.Add(' EXTRACT(YEAR FROM LastInvoiceDate) As TheYear, ');
        RichEdit1.Lines.Add(' EXTRACT(MONTH FROM LastInvoiceDate) As TheMonth, ');
        RichEdit1.Lines.Add(' CAST(MINOF(LastInvoiceDate, NOW) AS DATETIME) As MinDate, ');
        RichEdit1.Lines.Add(' CAST(FORMATDATETIME("dd/mm/yy",MAXOF(LastInvoiceDate, NOW)) AS CHAR(20)) ');
        RichEdit1.Lines.Add(' As MaxDate, ');
        RichEdit1.Lines.Add(' SUBSTRING(country FROM 2 FOR 6) As SubCountry, ');
        //RichEdit1.Lines.Add(' /* same as */');
        //RichEdit1.Lines.Add(' COPY(country, 2, 6) As UsingCopy,');
        RichEdit1.Lines.Add(' addr2,');
        RichEdit1.Lines.Add(' LENGTH(addr2) > 0 As LenAddr2 ');
        RichEdit1.Lines.Add('FROM customer ');
        RichEdit1.Lines.Add('ORDER BY 2 DESC;');
        RichEdit1.Lines.EndUpdate;
      End;
    21: Begin //Functions Solved In Events
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT itemstotal, amountpaid, freight, ');
        RichEdit1.Lines.Add(' /* AVGOF() is a function solved in events');
        RichEdit1.Lines.Add(' OnUDFCheck and OnUDFSolve */');
        RichEdit1.Lines.Add(' AVGOF(itemstotal, amountpaid, freight) As Average ');
        RichEdit1.Lines.Add('FROM orders; ');
        RichEdit1.Lines.EndUpdate;
      End;
    22: Begin //Subqueries: Sample 1
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * ');
        RichEdit1.Lines.Add('FROM Customer ');
        RichEdit1.Lines.Add('WHERE CustNo > (SELECT AVG(CustNo) FROM Customer);');
        RichEdit1.Lines.EndUpdate;
      End;
    23: Begin //Subqueries: Sample 2
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT * FROM Customer WHERE custno >= ');
        RichEdit1.Lines.Add('ALL (SELECT CustNo FROM customer WHERE ');
        RichEdit1.Lines.Add('/* This is a comment inside SQL statement*/ City ');
        RichEdit1.Lines.Add('IN ("Freeport", "Christiansted", "Kailua-Kona", ');
        RichEdit1.Lines.Add('"Giribaldi", "Kitchener", "Negril"));');
        RichEdit1.Lines.EndUpdate;
      End;
    24: Begin //Subqueries: Sample 3
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('SELECT (SELECT SUM(amountpaid) FROM Orders WHERE ');
        RichEdit1.Lines.Add('OrderNo Between 1100 and 1200) / ');
        RichEdit1.Lines.Add('(SELECT Count(*) FROM orders) As Average_From_1100_To_1200 ');
        RichEdit1.Lines.Add('FROM Orders GROUP BY 1; ');
        RichEdit1.Lines.EndUpdate;
      End;
    25: Begin //TRANSFORM...PIVOT: Sample 1
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('/* This is a TRANSFORM...PIVOT demo ala Microsoft Access */');
        RichEdit1.Lines.Add('TRANSFORM CAST(SUM(AmountPaid) AS MONEY) SELECT CUSTNO FROM ');
        RichEdit1.Lines.Add('ORDERS GROUP BY CUSTNO ');
        RichEdit1.Lines.Add('PIVOT FormatDateTime("yyyy", SALEDATE);');
        RichEdit1.Lines.EndUpdate;
      End;
    26: Begin //TRANSFORM...PIVOT: Sample 2
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('/* Multiple aggregate in the result set */ ');
        RichEdit1.Lines.Add('TRANSFORM SUM(AmountPaid), COUNT(*), AVG(AMOUNTPAID) SELECT CUSTNO ');
        RichEdit1.Lines.Add('FROM ORDERS GROUP BY CUSTNO ');
        RichEdit1.Lines.Add('PIVOT FormatDateTime("yyyy", SALEDATE) IN ');
        RichEdit1.Lines.Add('(SELECT DISTINCT EXTRACT(YEAR FROM LASTINVOICEDATE) FROM');
        RichEdit1.Lines.Add('CUSTOMER ORDER BY 1);');
        RichEdit1.Lines.EndUpdate;
      End;
    27: Begin //TRANSFORM...PIVOT: Sample 3
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('TRANSFORM CAST(SUM(AmountPaid) AS MONEY) ');
        RichEdit1.Lines.Add('SELECT CUSTNO, ROUNDDEC(SUM(AmountPaid),2) FROM ORDERS ');
        RichEdit1.Lines.Add('GROUP BY CUSTNO ');
        RichEdit1.Lines.Add('PIVOT FormatDateTime("mmm", SALEDATE) IN ');
        RichEdit1.Lines.Add('("Jan", "Feb", "Mar", "Apr", "May", "Jun", ');
        RichEdit1.Lines.Add('"Jul", "Aug", "Sep", "Oct", "Nov", "Dec");');
        RichEdit1.Lines.EndUpdate;
      End;
    28: Begin //CREATE TABLE
        ButtonRunSQL.Caption := 'Exec SQL';
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('CREATE TABLE "MyDatabase.Dbf"');
        RichEdit1.Lines.Add('(');
        RichEdit1.Lines.Add(' last_name  CHAR(30),');
        RichEdit1.Lines.Add(' first_name CHAR(40),');
        RichEdit1.Lines.Add(' salary     FLOAT(20,2),');
        RichEdit1.Lines.Add(' zip_code   CHAR(15),');
        RichEdit1.Lines.Add(' work_phone CHAR(30),');
        RichEdit1.Lines.Add(' home_phone CHAR(30),');
        RichEdit1.Lines.Add(' f1         FLOAT,');
        RichEdit1.Lines.Add(' f2         FLOAT(15),');
        RichEdit1.Lines.Add(' i1         SMALLINT,');
        RichEdit1.Lines.Add(' i2         INTEGER,');
        RichEdit1.Lines.Add(' B1         BOOLEAN,');
        RichEdit1.Lines.Add(' D1         DATE,');
        RichEdit1.Lines.Add(' D2         TIME,');
        RichEdit1.Lines.Add(' D3         DATETIME,');
        RichEdit1.Lines.Add(' CUSTNO     AUTOINC,');
        RichEdit1.Lines.Add(' M          MONEY,');
        RichEdit1.Lines.Add(' photo      BLOB(5)');
        RichEdit1.Lines.Add(' PRIMARY KEY (last_name, first_name)');
        RichEdit1.Lines.Add(')');
        RichEdit1.Lines.Add('CREATE TABLE "Table2.Dbf"');
        RichEdit1.Lines.Add('(');
        RichEdit1.Lines.Add(' last_name   CHAR(30),');
        RichEdit1.Lines.Add(' first_name  CHAR(40),');
        RichEdit1.Lines.Add(' salary      FLOAT(20,2)');
        RichEdit1.Lines.Add(');');
        RichEdit1.Lines.EndUpdate;
      End;
    29: Begin //ALTER TABLE
        ButtonRunSQL.Caption := 'Exec SQL';
        RichEdit1.Lines.BeginUpdate;
        RichEdit1.Lines.Add('ALTER TABLE "MyDatabase.Dbf"');
        RichEdit1.Lines.Add(' DROP COLUMN zip_code,');
        RichEdit1.Lines.Add(' DROP salary,');
        RichEdit1.Lines.Add(' ADD COLUMN home_phone CHAR(30),');
        RichEdit1.Lines.Add(' ADD photo BLOB(5);');
        RichEdit1.Lines.EndUpdate;
      End;
    30: Begin //CREATE INDEX
        ButtonRunSQL.Caption := 'Exec SQL';
        RichEdit1.Text := 'CREATE UNIQUE DESC INDEX custdate ON "Table1.DBF" (first_name, last_name); ';
      End;
    31: Begin //DROP TABLE
        ButtonRunSQL.Caption := 'Exec SQL';
        RichEdit1.Text := 'DROP TABLE "table1.dbf" ;';
      End;
    32: Begin //DROP INDEX
        ButtonRunSQL.Caption := 'Exec SQL';
        RichEdit1.Text := 'DROP INDEX "table1.dbf" primaryindex ;';
      End;
    33: Begin //CASE IN SELECT CLAUSE
        ButtonRunSQL.Caption := 'Run SQL';
        RichEdit1.Lines.Add('/* a specially complex one :-) */');
        RichEdit1.Lines.Add('SELECT c.CustNo, City, c.state,');
        RichEdit1.Lines.Add('   CASE');
        RichEdit1.Lines.Add('     WHEN c.state = "FL" THEN "Florida"');
        RichEdit1.Lines.Add('     WHEN c.state = "CA" THEN "California"');
        RichEdit1.Lines.Add('     WHEN c.state = "AL" THEN "Alabama"');
        RichEdit1.Lines.Add('     WHEN c.state = "OR" THEN "Oregon"');
        RichEdit1.Lines.Add('     ELSE "Unknown state"');
        RichEdit1.Lines.Add('   END');
        RichEdit1.Lines.Add('   As StateName, c.ZIP');
        RichEdit1.Lines.Add('FROM (SELECT * FROM customer WHERE Custno BETWEEN 1000 AND 3000) c;');
      End;
    34: Begin //DATASETS DEFINED
        ButtonRunSQL.Caption := 'Run SQL';
        RichEdit1.Text := 'SELECT * FROM "c:\d3\demos\data\biolife.db" bio';
      End;
    35: Begin
        ButtonRunSQL.Caption := 'Run SQL';
        RichEdit1.Text := 'SELECT c.CustNo, Company, c.Addr1, c.City FROM (SELECT * FROM Customer WHERE CustNo BETWEEN 2000 AND 3000) c;';
      End;
  End;
  {if not RadioGroupSQLExamples.ItemIndex in [33] then
     SyntaxHighlighter1.Editor := RichEdit1; }
  if Longint(Selected.Data)-1<>34 then   // "\" CHAR IS CAUSING PROBLEM TO THE TRichEdit
    SyntaxHighlighter1.Editor := RichEdit1;
  ButtonRunSQL.Enabled := True;

end;

procedure TfrmTest.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  SelectionChange;
end;

procedure TfrmTest.ChkParseClick(Sender: TObject);
begin
  Panel12.Visible:= ChkParse.Checked;
  Splitter1.Visible:= ChkParse.Checked;
end;

procedure TfrmTest.ParseSQL;
var
  ErrLine, ErrCol: Integer;
  ErrMsg, Errtxt: string;
  Node: TTreeNode;

  procedure RecursePopulate(A: TSqlAnalizer; ParentNode: TTreeNode);
  var
    I: Integer;
    TempA: TSqlAnalizer;
    Node: TTreeNode;
  begin
    for I:= 0 to A.SubqueryList.Count-1 do
    begin
      TempA:= TSqlAnalizer(A.SubQueryList[0]);
      Node:= TreeView2.Items.AddChildObject(ParentNode, 'Subquery ' + IntToStr(I),  TempA);
      RecursePopulate(TempA, Node);
    end;
  end;

begin
  if Assigned(FAnalizer) then
  begin
    FAnalizer.Free;
    FAnalizer:= nil;
  end;
  FAnalizer:= TSqlAnalizer.Create(nil, xQuery1);
  if FAnalizer.parser.yyparse = 1 then
  begin
    ErrLine:= fAnalizer.lexer.yylineno;
    ErrCol:= fAnalizer.lexer.yycolno - fAnalizer.lexer.yyTextLen - 1;
    ErrMsg:= fAnalizer.parser.yyerrormsg;
    fAnalizer.lexer.GetyyText(ErrTxt);
    if Assigned(xQuery1.OnSyntaxError) then
    begin
      xQuery1.OnSyntaxError(Self, ErrMsg, ErrTxt, ErrLine, ErrCol, Length(ErrTxt));
      Exit;
    end
    else
      { if not raised an error, will raise here }
      raise Exception.CreateFmt(' %s at line : %d, Column: %d, token: %s',
        [ErrMsg, ErrLine, ErrCol, ErrTxt]);
  end;
  with TreeView2.Items do
  begin
    Clear;
    Node:= AddObject(nil, 'Top SQL', fAnalizer);
    RecursePopulate(fAnalizer, Node);
  end;
end;

procedure TfrmTest.TreeView2Click(Sender: TObject);
begin
  AnalizerChange;
end;

procedure TfrmTest.AnalizerChange;
var
  A: TSqlAnalizer;
  Selected: TTreeNode;
  s: string;
  I, J: Integer;
begin
  Selected := TreeView2.Selected;
  if (Selected=nil) or (Selected.Data=nil) then exit;
  A:= TSqlAnalizer(Selected.Data);
  { show all parameters for A }
  with MemoParse.Lines do
  begin
    clear;
    case A.Statement of
      ssSelect: s:= 'SELECT';
      ssUpdate: s:= 'UPDATE';
      ssDelete: s:= 'DELETE';
      ssInsert: s:= 'INSERT';
      ssUnion:  s:= 'UNION';
      ssCreateTable: s:= 'CREATETABLE';
      ssAlterTable:  s:= 'ALTERTABLE';
      ssCreateIndex: s:= 'CREATEINDEX';
      ssDropTable:   s:= 'DROPTABLE';
      ssDropIndex:   s:= 'DROPINDEX';
      ssPackTable:   s:= 'PACKTABLE';
      ssZapTable:    s:= 'ZAPTABLE';
      ssReindexTable: s:= 'REINDEXTABLE';
    end;
    Add(Format('Statement is : %s', [s]));

    if A.IsDistinct then
      Add('SELECT DISTINCT issued');
    if A.DoSelectAll then
      Add('SELECT * issued');
    if A.TableAllFields.Count> 0 then
    begin
      Add('The following tables include all fields');
      for I:= 0 to A.TableAllFields.Count-1 do
        Add(A.TableAllFields[I]);
    end;
    if A.ColumnList.Count>0 then
      Add('Columns follows :');
    for I:= 0 to A.ColumnList.Count-1 do
      with A.ColumnList[I] do
      begin
        Add(format('Column %d expression :%s', [I, ColumnExpr]));
        if Length(AsAlias) > 0 then
          Add(format('Alias %d: %s',[i, AsAlias]));
      end;
    if A.TableList.Count>0 then
      Add('Tables follows :');
    for I:= 0 to A.TableList.Count-1 do
      with A.TableList[I] do
      begin
        Add(format('Table %d Name :%s', [i,TableName]));
        if Length(Alias) > 0 then
          Add(format('Alias %d: %s', [i,Alias]));
      end;
    if A.JoinList.Count > 0 then
      Add('JOIN information follows:');
    for I:= 0 to A.JoinList.Count-1 do
      with A.JoinList[I] do
      begin
        for J:= 0 to Count-1 do
          Add('JOIN ON Expression :'+JoinExpression);
      end;

    if Length(A.WhereStr) > 0 then
      Add('WHERE expression: ' + A.WhereStr);

    if A.OrderByList.Count > 0 then
      for I:= 0 to A.OrderByList.Count-1 do
        with A.OrderByList[I] do
        begin
          if ColIndex > 0 then
            Add(format('Column %d index to order: %d',[ I, ColIndex]))
          else
            Add(format('ORDER BY %d expression: %s', [I, Alias]));
        end;

    if A.GroupByList.Count > 0 then
      for I:= 0 to A.GroupByList.Count-1 do
        with A.GroupByList[I] do
        begin
          if ColIndex > 0 then
            Add(format('Column %d index to group: %d', [I,ColIndex]))
          else
            Add(format('GROUP BY %d expression: %s',[I, Alias]));
        end;

    { other statements are not showed here: UPDATE, DELETE, INSERT, etc.
      but it is the same:

      A.UpdateColumnList for UPDATE
      A.InsertList for INSERT
      A.CreateTableList for CREATE TABLE
      A.AlterTableList for ALTER TABLE
      if Length(A.PivotStr) > 0 then   TRANSFORM...PIVOT issued
        A.PivotInList contains the pivots
        A.TransformColumnList contains the columns to pivot
      if Length(A.IntoTable)>0 then
        sintax: SELECT * FROM CUSTOMER INTO TABLE x
      if A.FromWithSubquery then
        sintax: SELECT * FROM (SELECT * FROM customer);
    }
  end;
end;

procedure TfrmTest.XQuery1SetRange(Sender: TObject;
  RelOperator: TRelationalOperator; DataSet: TDataSet; const FieldNames,
  StartValues, EndValues: String; IsJoining: Boolean);
Var
  F: TField;
Begin
  If RelOperator = ropBETWEEN Then Begin // Warning: this will fail with multiple fields "OrderNo;ItemNo"
    With DataSet As TTable Do Begin
      SetRangeStart;
      FieldByName(FieldNames).AsString := StartValues;
      SetRangeEnd;
      FieldByName(FieldNames).AsString := EndValues;
      ApplyRange;
    End;
  End Else If RelOperator In [ropGT, ropGE, ropLT, ropLE, ropNEQ] Then Begin // instead, will use a filter
    With DataSet As TTable Do Begin
      F := FindField(FieldNames);
      Case F.DataType Of
        FtString{$IFDEF LEVEL4}, FtFixedChar, FtWideString{$ENDIF}
           {$IFDEF LEVEL5}, FtGUID{$ENDIF}: Begin
            Case RelOperator Of
              ropGT: Filter := Format('%s > ''%s''', [FieldNames, StartValues]);
              ropGE: Filter := Format('%s >= ''%s''', [FieldNames, StartValues]);
              ropLT: Filter := Format('%s < ''%s''', [FieldNames, StartValues]);
              ropLE: Filter := Format('%s <= ''%s''', [FieldNames, StartValues]);
              ropNEQ: Filter := Format('%s <> ''%s''', [FieldNames, StartValues]);
            End;
            Filtered := True;
          End;
        FtFloat, FtCurrency, FtBCD, FtDate, FtTime, FtDateTime,
          FtAutoInc, FtSmallint, FtInteger, FtWord
          {$IFNDEF LEVEL3}, FtLargeInt{$ENDIF}, FtBoolean: Begin
            Case RelOperator Of
              ropGT: Filter := Format('%s > %s', [FieldNames, StartValues]);
              ropGE: Filter := Format('%s >= %s', [FieldNames, StartValues]);
              ropLT: Filter := Format('%s < %s', [FieldNames, StartValues]);
              ropLE: Filter := Format('%s <= %s', [FieldNames, StartValues]);
              ropNEQ: Filter := Format('%s <> %s', [FieldNames, StartValues]);
            End;
            Filtered := True;
          End;
      End;
    End;
  End;
end;

procedure TfrmTest.XQuery1CancelRange(Sender: TObject; DataSet: TDataSet;
  IsJoining: Boolean);
begin
  (DataSet As TTable).CancelRange; // if a range was set
  (DataSet As TTable).Filtered := False; // if was filtered
end;

procedure TfrmTest.XQuery1IndexNeededFor(Sender: TObject;
  DataSet: TDataSet; const FieldNames: String; ActivateIndex,
  IsJoining: Boolean; var Accept: Boolean);
begin
  if IsJoining then Exit;
  if not (Dataset is TTable) then
  Begin
    Accept:= False;
    Exit;
  End;
  If DataSet = Table1 Then Accept := (AnsiCompareText(FieldNames, 'CustNo') = 0)
  Else If DataSet = Table2 Then Accept := (AnsiCompareText(FieldNames, 'CustNo') = 0) Or (AnsiCompareText(FieldNames, 'OrderNo') = 0)
  Else If DataSet = Table3 Then Accept := (AnsiCompareText(FieldNames, 'OrderNo') = 0)
    Or (AnsiCompareText(FieldNames, 'OrderNo;ItemNo') = 0)
      Or (AnsiCompareText(FieldNames, 'PartNo') = 0)
  Else If DataSet = Table4 Then Accept := (AnsiCompareText(FieldNames, 'PartNo') = 0)
    Or (AnsiCompareText(FieldNames, 'Description') = 0)
      Or (AnsiCompareText(FieldNames, 'VendorNo') = 0);

  If Accept And ActivateIndex Then (DataSet As TTable).IndexFieldNames := FieldNames;
end;

procedure TfrmTest.XQuery1CancelFilter(Sender: TObject; DataSet: TDataSet;
  IsJoining: Boolean);
begin
  //ShowMessage('Filter canceled on dataset ' + Dataset.name);
  if not IsJoining then Exit;
  (DataSet as TTable).Filtered := False;
   (DataSet as TTable).Filter := '';
end;

procedure TfrmTest.XQuery1SetFilter(Sender: TObject; DataSet: TDataSet;
  const Filter: String; IsJoining: Boolean; var Handled: Boolean);
begin
  //ShowMessage(Format('Dataset name %s; Filter : %s', [Dataset.Name, Filter]));
  { Note: in this case, filters now needed, they are set in OnSetRange this is only to illustrate how to }
  //ShowMessage(Filter);
  if not IsJoining then Exit;
  { can this filter be set ?}
  try
     (DataSet as TTable).Filtered := False;
     (DataSet as TTable).Filter   := Filter;
     (DataSet as TTable).Filtered := True;
     Handled := True;
  except
     Handled := False;
     (DataSet as TTable).Filtered := False;
     raise;
  end;
end;

procedure TfrmTest.xQuery4BeforeInsert(DataSet: TDataSet);
begin
  SysUtils.Abort;
end;

procedure TfrmTest.XQuery1UDFCheck(Sender: TObject;
  const Identifier: String; Params: TParameterList;
  var DataType: TExprType; var MaxLen: Integer; var Accept: Boolean);
var
  I: Integer;  
begin
  If AnsiCompareText(Identifier, 'DTOS') = 0 Then Begin
    Accept := True;
    Datatype:= ttString;
    MaxLen:= 50;
    Exit;
  End Else If AnsiCompareText(Identifier, 'AVGOF') = 0 Then Begin
    {this function will take a list of parameters and will calculate the
      average of the integer or float parameters }
    If Not (Assigned(Params) And (Params.Count > 1)) Then Begin
      Accept := False;
      Exit;
    End;
    { check that the function have only integers and float parameters }
    For I := 0 To Params.Count - 1 Do Begin
      If Not (Params.ExprType[I] In [TtFloat, TtInteger]) Then Begin
        Accept := False;
        Exit;
      End;
    End;
    Accept := True;
    Datatype:= ttFloat;
    Exit;
  End Else If AnsiCompareText(Identifier, 'TRIMDC') = 0 Then Begin
    {this function will trim all "$" and "," from a string
     and will return a float
     example of use SELECT TRIMDC("$10,000.45") FROM MyTable }
    If Not (Assigned(Params) And (Params.Count = 1)) Then Begin
      Accept := False;
      Exit;
    End;
    {check that the function have only string parameters}
    If Not (Params.ExprType[0] = TtString) Then Begin
      Accept := False;
      Exit;
    End;
    Accept:= True;
    Datatype:= ttFloat;
    Exit;
  End;
end;

procedure TfrmTest.XQuery1UDFSolve(Sender: TObject;
  const Identifier: String; Params: TParameterList; var Value: Variant);
Var
  I: Integer;
  Temp: Double;
  TempS, S: String;
Begin
  If AnsiCompareText(Identifier, 'DTOS') = 0 Then
  Begin
    Value := DateToStr( Value );
  End
  Else If AnsiCompareText(Identifier, 'AVGOF') = 0 Then
  Begin
    Temp := Params.AsFloat[0];
    For I := 1 To Params.Count - 1 Do // start from second param of function
      Temp := Temp + Params.AsFloat[I];
    Value := Temp / Params.Count;
  End
  Else If AnsiCompareText(Identifier, 'TRIMDC') = 0 Then
  Begin
    S := Params.AsString[0];
    TempS := '';
    For I := 1 To Length(S) Do Begin
      If Not (S[I] In ['$', ',']) Then TempS := TempS + S[I]; // discard "$" and ","
    End;
    Value := StrToFloat(TempS);
  End;

end;

procedure TfrmTest.Button1Click(Sender: TObject);
begin
  xquery1.delete;
end;

procedure TfrmTest.XQuery1CancelQuery(Sender: TObject;
  var Cancel: Boolean);
begin
  If (GetAsyncKeyState( VK_ESCAPE ) shr 1 ) <> 0 then
  begin
    ShowMessage('user canceled');
    cancel:=true;
  end;
end;

End.

