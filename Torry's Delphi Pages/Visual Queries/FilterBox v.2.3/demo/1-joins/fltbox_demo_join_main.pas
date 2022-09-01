unit fltbox_demo_join_main;

interface
{$I psc_defines.inc}

uses
  {$IFDEF D6}
  {$ENDIF}
  gridcolors_frm_setup,
  myla_interfaces,
  myla_system,

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, psc_procs, psc_listbox, psc_fltbox, ADODB, Grids, DBGrids,
  StdCtrls, CheckLst, ExtCtrls, Buttons, psc_expreval;

type
  TMainForm = class(TForm)
    DBGrid1: TDBGrid;
    ADOConnection1: TADOConnection;
    PSCFltBld1: TPSCFltBld;
    PSCFltBox1: TPSCFltBox;
    DataSource1: TDataSource;
    ADOQuery1: TADOQuery;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    SQLMemo: TMemo;
    Label3: TLabel;
    ADOTableCountry: TADOTable;
    ADOTableCustomer: TADOTable;
    ADOTableCountryIDCountry: TAutoIncField;
    ADOTableCountryName: TWideStringField;
    ADOTableCustomerCustNo: TFloatField;
    ADOTableCustomerCompany: TWideStringField;
    SpeedButton1: TButton;
    CheckBox_ReOpen: TCheckBox;
    Button1: TButton;
    PSCGridColors1: TPSCGridColors;
    procedure FormCreate(Sender: TObject);
    procedure PSCFltBld1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox2ClickCheck(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PSCFltBld1QuoteStr(Sender: TObject; AField: TPSCField;
      var AQuotedStr: String);
    procedure ADOQuery1AfterOpen(DataSet: TDataSet);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoApply(Sender: TObject);
    procedure RefreshQuery;
  end;

var
  MainForm: TMainForm;

implementation

uses fltbox_demo_join_datastruct;

var
  TableFields: TStringList;

{$R *.dfm}

function StringsToStr(S:TStrings):String;
var
  i: Integer;
begin
  Result:='';
  for i:= 0 to S.Count - 1 do
    Result:= Result + S[i] + ' ';
end;


function GetTableName(AFullName: String): String;
var
  MyPointPos: Integer;
begin
  MyPointPos:= Pos('.', AFullName);
  Result:= Copy(AFullName, 0, MyPointPos-1);
end;

function GetFieldName(AFullName: String): String;
var
  MyPointPos: Integer;
begin
  MyPointPos:= Pos('.', AFullName);
  Result:= Copy(AFullName, MyPointPos+1, Length(AFullName)- MyPointPos);
end;

function GetFieldIndex(AFieldName: String): Integer;
var
  i: Integer;
  MyCompStr: String;
begin
  Result:= TableFields.IndexOf(AFieldName);
  if(Result>0)then
    Exit;
  with MainForm do
    for i:= 0 to TableFields.Count - 1 do
    begin
      MyCompStr:= GetFieldName(TableFields[i]);
      if MyCompStr = AFieldName then
      begin
        Result:= i;
        Exit;
      end;
    end;
end;

procedure AdjustFltBld;
var
  MyFieldName: String;
  MyTableName: String;
  MyIndex: Integer;
  MyFieldIndex: Integer;
begin
  with MainForm.ADOQuery1, MainForm do
  begin
    for MyIndex:= 0 to Fields.Count-1 do
    begin
      MyFieldIndex:= GetFieldIndex(ADOQuery1.Fields[MyIndex].FieldName);
      MyTableName:= GetTableName(TableFields[MyFieldIndex]);
      MyFieldName:= GetFieldName(TableFields[MyFieldIndex]);
      with TPSCFltBoxField(PSCFltBld1.Fields.Add) do
      begin
        DataType:= PSCDBFieldTypeToPSC(Fields[MyIndex].DataType);
        DataField:= MyFieldName;
        DisplayLabel:= Fields[MyIndex].FieldName;
        SQLFieldName:= MyFieldName;
        TableName:= MyTableName;
        if UpperCase(MyFieldName) = 'IDCOUNTRY' then
        begin
          LookupDataSet:= ADOTableCountry;
          LookupKeyField:= ADOTableCountryIDCountry.FieldName;
          LookupDisplayField:= ADOTableCountryName.FieldName;
        end else
        if UpperCase(MyFieldName) = 'CUSTNO' then
        begin
          LookupDataSet:= ADOTableCustomer;
          LookupKeyField:= ADOTableCustomerCustNo.FieldName;
          LookupDisplayField:= ADOTableCustomerCompany.FieldName;
          LookupGridFields:= ADOTableCustomerCustNo.FieldName
            +';'+ADOTableCustomerCompany.FieldName;
        end;
      end;
    end;
    PSCFltBld1.Items.Clear;
  end;
end;

function AdjustQuery: Boolean;
var
  i: Integer;
  MyTableName: String;
  MyFieldName: String;
  MyStrings: TStringList;
begin
  Result:= False;
  with MainForm.ADOQuery1, MainForm do
  begin
    SQL.Clear;
    SQL.Add('SELECT');
    PSCFltBld1.Fields.Clear;
    MyStrings:= TStringList.Create;
    try
      for i:= 0 to TableFields.Count-1 do
      begin
        MyTableName:= GetTableName(TableFields.Strings[i]);
        MyFieldName:= GetFieldName(TableFields.Strings[i]);
        if MyStrings.IndexOf(MyFieldName) >= 0 then
          Continue;
        MyStrings.Add(MyFieldName);
        if CheckListBox1.Checked[CheckListBox1.Items.IndexOf(MyTableName)] then
        begin
          SQL.Add(TableFields.Strings[i]);
          SQL.Add(',');
        end;
     end;
    finally
      MyStrings.Free;
    end;
    SQL.Delete(SQL.Count-1);
    if(SQL.Count = 0)then
      Exit;
    SQL.Add('FROM');
    for i:= 0 to CheckListBox1.Items.Count-1 do
    begin
      if CheckListBox1.Checked[i] then
      begin
        SQL.Add(CheckListBox1.Items[i]);
        SQL.Add(',');
      end;
    end;
    SQL.Delete(SQL.Count-1);
    SQL.Add('WHERE');
    with CheckListBox1 do
      if Checked[Items.IndexOf('CUSTOMER')] then
      begin
        if Checked[Items.IndexOf('ORDERS')] then
        begin
          SQL.Add('(ORDERS.CUSTNO = CUSTOMER.CUSTNO)');
          SQL.Add('AND');
        end;
        if Checked[Items.IndexOf('COUNTRY')] then
        begin
          SQL.Add('(COUNTRY.IDCOUNTRY = CUSTOMER.IDCOUNTRY)');
          SQL.Add('AND');
        end;
      end;
    SQL.Delete(SQL.Count-1);
    PSCFltBld1.SQLHead.Assign(SQL);
  end;
  Result:= True;
end;

procedure TMainForm.RefreshQuery;
begin
  PSCGridColors1.RecordsInfo.Clear;
  with ADOQuery1 do
  begin

    DisableControls;
    Close;
    if AdjustQuery then
    begin
      Open;
      AdjustFltBld;
    end;
    EnableControls;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ADOConnection1.ConnectionString:=
    'Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;'+
    'Data Source='+ PSCGetAppFolder+'join_demo.mdb;'+
    'Mode=Share Deny None;Persist Security Info=False;Jet OLEDB:System database="";Jet OLEDB:Registry Path="";'+
    'Jet OLEDB:Database Password="";'+
    'Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode=1;Jet OLEDB:Global Partial Bulk Ops=2;'+
    'Jet OLEDB:Global Bulk Transactions=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create System Database=False;'+
    'Jet OLEDB:Encrypt Database=False;Jet OLEDB:Don''t Copy Locale on Compact=False;'+
    'Jet OLEDB:Compact Without Replica Repair=False;Jet OLEDB:SFP=False';

  DBGrid1.OnDrawColumnCell:=PSCGridColors1.DoGridDrawColumnCell;

  ADOConnection1.Open;
  ADOTableCountry.Open;
  ADOTableCustomer.Open;
  ADOConnection1.GetTableNames(CheckListBox1.Items);
  TableFields:= TStringList.Create;
  RefreshQuery;
end;

procedure TMainForm.PSCFltBld1Change(Sender: TObject);
begin
  if (PSCFltBld1.SQL.Count = 0) or (csDestroying in ADOQuery1.ComponentState) then
    Exit;

  If CheckBox_ReOpen.Checked then
  begin
    ADOQuery1.DisableControls;
    try
      ADOQuery1.Close;
      ADOQuery1.SQL.Assign(PSCFltBld1.SQL);
      ADOQuery1.Open;
    finally
      ADOQuery1.EnableControls;
    end;  
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TableFields.Destroy;
  ADOQuery1.Close;
  ADOTableCountry.Close;
  ADOTableCustomer.Close;
  ADOConnection1.Close;
end;

procedure TMainForm.CheckListBox1ClickCheck(Sender: TObject);
begin
  RefreshQuery;
end;

procedure TMainForm.DoApply(Sender: TObject);
begin
  if psc_frm_setup <> nil then
  begin
    psc_frm_setup.SaveGridProperties(PSCGridColors1);
    DBGrid1.Invalidate;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  psc_frm_setup := Tpsc_frm_setup.Create(nil);
  try
    psc_frm_setup.LoadGridProperties(PSCGridColors1, DataSource1.DataSet);
    psc_frm_setup.MyGridColorsUpdate := DoApply;
    psc_frm_setup.ShowModal;
    if psc_frm_setup.ModalResult = mrOk then
    begin
      psc_frm_setup.SaveGridProperties(PSCGridColors1);
      DBGrid1.Invalidate;
    end;
  finally
    psc_frm_setup.Free;
    psc_frm_setup:=nil;
  end;
end;

procedure TMainForm.CheckListBox1Click(Sender: TObject);
var
  i: Integer;
begin
  ADOConnection1.GetFieldNames(CheckListBox1.Items[CheckListBox1.ItemIndex], CheckListBox2.Items);
  for i:= 0 to CheckListBox2.Items.Count-1 do
  begin
    if TableFields.IndexOf(CheckListBox1.Items[CheckListBox1.ItemIndex] + '.' + CheckListBox2.Items[i]) >= 0  then
      CheckListBox2.Checked[i]:= True
    else
      CheckListBox2.Checked[i]:= False;
  end;
  CheckListBox2.Enabled:= CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TMainForm.CheckListBox2ClickCheck(Sender: TObject);
var
  MyCheckIndex: Integer;
  MyIndex: Integer;
  MyStr: String;
begin
  MyCheckIndex:= CheckListBox2.ItemIndex;
  MyStr:=
    CheckListBox1.Items[CheckListBox1.ItemIndex] + '.' +
    CheckListBox2.Items[MyCheckIndex];
  MyIndex:= TableFields.IndexOf(MyStr);
  if(CheckListBox2.Checked[MyCheckIndex])then
  begin
    if MyIndex < 0 then
      TableFields.Add(MyStr);
  end else
  begin
    if MyIndex >= 0 then
      TableFields.Delete(MyIndex);
  end;
  RefreshQuery;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  DataStructureForm.ShowModal;
end;

procedure TMainForm.PSCFltBld1QuoteStr(Sender: TObject; AField: TPSCField;
  var AQuotedStr: String);
begin
  if (AField.DataType in [FT_DATE,FT_TIME,FT_DATETIME])
  then
    begin
      PSCReplaceAllOccur(AQuotedStr,#39,'#');
    end;
end;

procedure TMainForm.ADOQuery1AfterOpen(DataSet: TDataSet);
begin
  SQLMemo.Text:=StringsToStr(ADOQuery1.SQL);
end;

end.
