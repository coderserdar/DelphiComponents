unit ExpressBuilder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, VQBuilder, VQBLocalize, ErrorDlg;

type
  THackDB = class(TDBEngine);

  TExpBuilder = class(TForm)
    Memo1: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    TreeView1: TTreeView;
    ListBox1: TListBox;
    ImageList1: TImageList;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Button5: TButton;
    Button16: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1DblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FConstants: TStringList;
    FArithmetics: TStringList;
    FEquations: TStringList;
    FLogical: TStringList;
    FFunctions: TStringList;

    FStringFunc: TStringList;
    FAggFunc: TStringList;
    FDataFunc: TStringList;
    FSQLFunc: TStringList;
    FUDFFunc: TStringList;

    FFormList: TFormList;
    FExpResult: string;
    FEngine: TDBEngine;

    Procedure FillBaseList;

    procedure SetFormList(const Value: TFormList);
    procedure SetExpResult(const Value: string);
    Function GetRealName(TableName, FieldName: String): String;
    procedure SetDBEngine(const Value: TDBEngine);
    Function ExtractName(S: String): String;
    Function ExtractRef(S: String): String;
    Function ExtractText(S: String): String;
  public
    { Public declarations }
    Property FormList: TFormList read FFormList write SetFormList;
    Property ExpResult: string read FExpResult write SetExpResult;
    Property DBEngine: TDBEngine read FEngine write SetDBEngine;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure UpdateMemo;
  end;

const
    Operators: array [0..15] of string = ('+ ',
                                          '- ',
                                          '/ ',
                                          '* ',
                                          '= ',
                                          '> ',
                                          '< ',
                                          '<> ',
                                          'AND ',
                                          'OR ',
                                          'NOT ',
                                          'LIKE ',
                                          '( ',
                                          ') ',
                                          '%',
                                          '_');

    StringFunc: array [0..4] of string = (
                '|| @String1 || String2@Concatenation',
                'LOWER @Lower(String)@Convert String to lower case',
                'UPPER @Upper(String)@Convert String to upper case',
                'SUBSTRING @SUBSTRING(column_reference FROM start_index [FOR length])@Extract substring from a string',
                'TRIM @TRIM([LEADING|TRAILING|BOTH] [trimmed_char] FROM column_reference)@Removes the trailing or leading character, or both, from a string'
                );
    AggregateFunc: array [0..4] of string = (
                'AVG @AVG([ALL] column_reference | DISTINCT column_reference)@Returns the average of the values in a specified column or an expression',
                'COUNT @COUNT(* | [ALL] column_reference | DISTINCT column_reference)@Returns the number of rows that satisfy a query’s search condition',
                'MAX @MAX([ALL] column_reference | DISTINCT column_reference)@Returns the largest value in the specified column',
                'MIN @MIN([ALL] column_reference | DISTINCT column_reference)@Returns the smallest value in the specified column',
                'SUM @SUM([ALL] column_reference | DISTINCT column_reference)@Calculates the sum of values for a column'
                );
    DataFunc: array [0..1] of string = (
                'CAST @CAST(column_reference AS data_type)@Converts specified value to the specified data type',
                'EXTRACT @EXTRACT(extract_field FROM column_reference)@Returns one field from a date value'
                );
    SQLFunc: array [0..19] of string = (
                'SELECT @SELECT [DISTINCT] * | column_list FROM table_reference [WHERE predicates][GROUP BY group_list][HAVING having_condition][ORDER BY order_list]@Retrieves data from tables',
                'FROM @FROM table_reference [, table_reference...]@Specifies the tables from which a SELECT statement retrieves data',
                'WHERE @WHERE predicates@Specifies filtering conditions for a SELECT or UPDATE statement',
                'ORDER BY @ORDER BY column_reference [, column_reference...] [ASC|DESC]@Sorts the rows retrieved by a SELECT statement',
                'GROUP BY @GROUP BY column_reference [, column reference...]@Combines rows with column values in common into single rows',
                'HAVING @HAVING predicates@Specifies filtering conditions for a SELECT statement',
                'BETWEEN @value1 [NOT] BETWEEN value2 AND value3@Determines whether a value falls inside a range',
                'EXISTS @EXISTS subquery@Indicates whether values exist in a subquery',
                'IN @value [NOT] IN (value_set)@Indicates whether a value exists in a set of values',
                'LIKE @value [NOT] LIKE [substitution_char] comparison_value [substitution_char] [ESCAPE escape_char]@Indicates the similarity of one value as compared to another',
                'IS NULL @column_reference IS [NOT] NULL@Indicates whether a column contains a NULL value',
                'SOME @column_reference comparison_predicate SOME(subquery)@Compares a column value to a column value in multiple rows in a subquery',
                'ANY @column_reference comparison_predicate ANY(subquery)@Compares a column value to a column value in multiple rows in a subquery',
                'ALL @column_reference comparison_predicate ALL(subquery)@Compares a column value to a column value in multiple rows in a subquery',
                'DISTINCT @Distinct(column_reference)@ignores duplicate values when averaging values in the specified column',
                'INNER JOIN @SELECT column_list FROM table_reference INNER JOIN table_reference ON predicate INNER JOIN table_reference ON predicate...@Joins two tables based on column values common between the two, excluding non-matches',
                'LEFT OUTER JOIN @SELECT column_list FROM table_reference LEFT OUTER JOIN table_reference ON predicate LEFT OUTER JOIN table_reference ON predicate...@Joins two tables based on column values common between the two, including non-matches',
                'RIGHT OUTER JOIN @SELECT column_list FROM table_reference RIGHT OUTER JOIN table_reference ON predicate RIGHT OUTER JOIN table_reference ON predicate...@Joins two tables based on column values common between the two, including non-matches',
                'FULL OUTER JOIN @SELECT column_list FROM table_reference FULL OUTER JOIN table_reference ON predicate FULL OUTER JOIN table_reference ON predicate...@Joins two tables based on column values common between the two, including non-matches',
                'UNION @SELECT col_1 [, col_2, ... col_n] FROM table_reference UNION [ALL] SELECT col_1 [, col_2, ... col_n] FROM table_reference@Concatenates the rows of one table to the end of another table'
                );

   ExpConst: array [0..5] of string = ('''', '''''', 'NULL ', 'IS ', '%', '_');
   ExpArithmetics: Array [0..3] of string = ('- ', '* ', '/ ', '+ ');
   ExpEquals: array [0..5] of string = ('< ', '<= ', '<> ', '= ', '> ', '>= ');
   ExpLogical: array[0..2] of string = ('AND ', 'NOT ', 'OR ');


var
  ExpBuilder: TExpBuilder;

implementation


{$R *.dfm}

procedure TExpBuilder.Button1Click(Sender: TObject);
begin
   Memo1.Text:= Memo1.Text + Operators[TButton(Sender).tag];
   UpdateMemo;
end;

constructor TExpBuilder.Create(AOwner: TComponent);
begin
  inherited;
  // localiazation
  Caption:= resExprTitle;
  btnOk.Caption:= resExpOkBtn;
  btnCancel.Caption:= resExpCancelBtn;

  FConstants:= TStringList.Create;
  FArithmetics:= TStringList.Create;
  FEquations:= TStringList.Create;
  FLogical:= TStringList.Create;
  FFunctions:= TStringList.Create;

  FStringFunc:= TStringList.Create;
  FAggFunc:= TStringList.Create;
  FDataFunc:= TStringList.Create;
  FSQLFunc:= TStringList.Create;
  FUDFFunc:= TStringList.Create;

  FillBaseList;
end;

destructor TExpBuilder.Destroy;
begin
  FConstants.Free;
  FArithmetics.Free;
  FEquations.Free;
  FLogical.Free;
  FFunctions.Free;

  FStringFunc.Free;
  FAggFunc.Free;
  FDataFunc.Free;
  FSQLFunc.Free;
  FUDFFunc.Free;

  inherited;
end;

procedure TExpBuilder.FillBaseList;
var
  i: integer;
begin
   for i:= 0 to High(ExpConst) do FConstants.Add(ExpConst[i]);
   for i:= 0 to High(ExpArithmetics) do FArithmetics.Add(ExpArithmetics[i]);
   for i:= 0 to High(ExpEquals) do FEquations.Add(ExpEquals[i]);
   for i:= 0 to High(ExpLogical) do FLogical.Add(ExpLogical[i]);

   for i:= 0 to High(StringFunc) do FStringFunc.Add(ExtractName(StringFunc[i]));
   for i:= 0 to High(AggregateFunc) do FAggFunc.Add(ExtractName(AggregateFunc[i]));
   for i:= 0 to High(DataFunc) do FDataFunc.Add(ExtractName(DataFunc[i]));
   for i:= 0 to High(SQLFunc) do FSQLFunc.Add(ExtractName(SQLFunc[i]));
end;

procedure TExpBuilder.FormShow(Sender: TObject);
var
  i: Integer;
  TableNode, ChildNode: TTreeNode;
begin
   TableNode:= TreeView1.Items.Item[11];
   for i:= 0 to FormList.Count -1 do
   begin
      ChildNode:= TreeView1.Items.AddChild(TableNode, FormList[i].TableAlias);
      ChildNode.ImageIndex:= 2;
      ChildNode.SelectedIndex:= 2;
   end;
   Memo1.Text:= ExpResult;
   UpdateMemo;

   for i:= 0 to THackDB(DBEngine).UDFList.Count - 1 do FUDFFunc.Add(ExtractName(THackDB(DBEngine).UDFList.Strings[i]));
end;

procedure TExpBuilder.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
   UpdateMemo;
   Case TreeView1.Selected.AbsoluteIndex of
      0, 7, 11:
      begin
         ListBox1.Items.Clear;
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      1:
      begin
         ListBox1.Items.Assign(FStringFunc);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      2:
      begin
         ListBox1.Items.Assign(FAggFunc);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      3:
      begin
         ListBox1.Items.Assign(FDataFunc);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      4:
      begin
         ListBox1.Items.Assign(FSQLFunc);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      5:
      begin
         ListBox1.Items.Assign(FUDFFunc);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      6:
      begin
         ListBox1.Items.Assign(FConstants);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      8:
      begin
         ListBox1.Items.Assign(FArithmetics);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      9:
      begin
         ListBox1.Items.Assign(FEquations);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
      10:
      begin
         ListBox1.Items.Assign(FLogical);
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
   else
       for i:= 0 to FormList.Count - 1 do
       begin
          if TreeView1.Selected.Text = FormList[i].TableAlias then
          begin
             ListBox1.Items.Assign(FormList[i].Items);
             exit;
          end;
       end;
   end; //case
end;

procedure TExpBuilder.SetFormList(const Value: TFormList);
begin
  FFormList := Value;
end;

procedure TExpBuilder.ListBox1DblClick(Sender: TObject);
var
  i, Index: integer;
begin
   if ListBox1.Items.Count = 0 then exit;
   Index:= TreeView1.Selected.AbsoluteIndex;
   for i:= 0 to ListBox1.Items.Count - 1 do
   begin
      if ListBox1.Selected[i] then
      begin
         if Index > 11 then
            Memo1.Text:= Memo1.Text + THackDB(DBEngine).ConvertTableName(TreeView1.Items.Item[Index].Text) + '.' +
            THackDB(DBEngine).ConvertFieldName(GetRealName(TreeView1.Items.Item[Index].Text, ListBox1.Items.Strings[i])) + ' '
         else
            Memo1.Text:= Memo1.Text + ListBox1.Items.Strings[i];
         UpdateMemo;
         exit;
      end;
   end;
end;

procedure TExpBuilder.btnOKClick(Sender: TObject);
var
  i, ALeft, ARight: integer;
begin
   ALeft:= 0;
   ARight:= 0;
   for i:= 1 to Length(Memo1.Text) do
   begin
      if Memo1.Text[i] = '(' then inc(ALeft);
      if Memo1.Text[i] = ')' then inc(ARight);
   end;
   if ALeft > ARight then
   begin
      ShowSQLError(resExpRight1, resExpRight2);
      exit;
   end;
   if ARight > ALeft then
   begin
      ShowSQLError(resExpLeft1, resExpLeft2);
      exit;
   end;

   ExpResult:= Memo1.Text;
   ModalResult:= mrOK;
end;

procedure TExpBuilder.SetExpResult(const Value: string);
begin
  FExpResult := Value;
end;

procedure TExpBuilder.TreeView1DblClick(Sender: TObject);
var
  i: integer;
  TableName: String;
begin
   if TreeView1.Selected.AbsoluteIndex > 11 then
   begin
      TableName:= TreeView1.Selected.Text;
      for i:= 0 to FormList.Count - 1 do
      begin
         if (TableName = FormList[i].TableAlias) then
         begin
            if TableName = FormList[i].TableName then
               Memo1.Text:= Memo1.Text + THackDB(DBEngine).ConvertTableName(TableName) + ' '
            else Memo1.Text:= Memo1.Text + THackDB(DBEngine).ConvertTableName(FormList[i].TableName) + ' ' +
                 THackDB(DBEngine).ConvertTableName(FormList[i].TableAlias) + ' ';
            UpdateMemo;
            exit;
         end;
      end;
   end;
end;

function TExpBuilder.GetRealName(TableName, FieldName: String): String;
var
  i, j: Integer;
begin
   for i:= 0 to FormList.Count - 1 do
   begin
      if (FormList[i].TableAlias = TableName) then
      begin
         for j:= 0 to FormList[i].NameField.Count - 1 do
         begin
            if FormList[i].Items.Strings[j] = FieldName then
            begin
               Result:= FormList[i].NameField.Strings[j];
               exit;
            end;
         end;
      end;
   end;
end;

procedure TExpBuilder.SetDBEngine(const Value: TDBEngine);
begin
  FEngine := Value;
end;

procedure TExpBuilder.UpdateMemo;
begin
   Memo1.SetFocus;
   Memo1.SelStart:= Length(Memo1.Text);
end;

procedure TExpBuilder.ListBox1Click(Sender: TObject);
begin
   case TreeView1.Selected.AbsoluteIndex of
      1:
      begin
         Label1.Caption:= ExtractRef(StringFunc[ListBox1.itemindex]);
         Label2.Caption:= ExtractText(StringFunc[ListBox1.itemindex]);
      end;
      2:
      begin
         Label1.Caption:= ExtractRef(AggregateFunc[ListBox1.itemindex]);
         Label2.Caption:= ExtractText(AggregateFunc[ListBox1.itemindex]);
      end;
      3:
      begin
         Label1.Caption:= ExtractRef(DataFunc[ListBox1.itemindex]);
         Label2.Caption:= ExtractText(DataFunc[ListBox1.itemindex]);
      end;
      4:
      begin
         Label1.Caption:= ExtractRef(SQLFunc[ListBox1.itemindex]);
         Label2.Caption:= ExtractText(SQLFunc[ListBox1.itemindex]);
      end;
      5:
      begin
         Label1.Caption:= ExtractRef(THackDB(DBEngine).UDFList.Strings[ListBox1.itemindex]);
         Label2.Caption:= ExtractText(THackDB(DBEngine).UDFList.Strings[ListBox1.itemindex]);
      end;
      else
      begin
         Label1.Caption:= EmptyStr;
         Label2.Caption:= EmptyStr;
      end;
   end;
   UpdateMemo;
end;

function TExpBuilder.ExtractName(S: String): String;
begin
   Result:= Copy(S, 1, Pos('@', S) - 1);
end;

function TExpBuilder.ExtractRef(S: String): String;
var
  i: integer;
begin
   Result:= EmptyStr;
   i:= Pos('@', S) + 1;
   while S[i] <> '@' do
   begin
      Result:= Result + S[i];
      inc(i);
   end;
end;

function TExpBuilder.ExtractText(S: String): String;
var
  i, j: integer;
begin
   Result:= EmptyStr;
   j:= 0;
   for i:= 1 to Length(S) do
   begin
      if S[i] = '@' then inc(j);
      if j = 2 then break;
   end;
   Result:= Copy(S, i + 1, Length(S) - i);
end;

end.
