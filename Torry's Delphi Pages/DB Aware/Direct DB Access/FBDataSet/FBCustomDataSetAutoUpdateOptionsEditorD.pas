unit FBCustomDataSetAutoUpdateOptionsEditorD;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jvuib, FBCustomDataSet, ExtCtrls, DB;

type
  TFBCustomDataSetAutoUpdateOptionsEditorForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    ComboBox2: TComboBox;
    Button1: TButton;
    Button2: TButton;
    JvUIBQuery1: TJvUIBQuery;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function ShowEditor(DS: TFBDataSet):boolean;
  end;

var
  FBCustomDataSetAutoUpdateOptionsEditorForm: TFBCustomDataSetAutoUpdateOptionsEditorForm;

implementation
uses FBMisc;
{$R *.dfm}

procedure TFBCustomDataSetAutoUpdateOptionsEditorForm.FormCreate(
  Sender: TObject);
begin
  Button1.Caption:=slcOk;
  Button2.Caption:=slcCancel;
  Label1.Caption:=slcUpdatedField;
  Label2.Caption:=slcGeneratorName;
  Label3.Caption:=slcIncrementBy;
end;

function TFBCustomDataSetAutoUpdateOptionsEditorForm.ShowEditor(
  DS: TFBDataSet):boolean;
var
  i:integer;
  OldDataBaseConnected:boolean;
begin
  Result:=false;
  JvUIBQuery1.DataBase:=DS.DataBase;
  JvUIBQuery1.Transaction:=DS.Transaction;
  if not Assigned(JvUIBQuery1.DataBase) then FBError(fbeDatabaseNotAssigned, [DS.Name]);
  if not Assigned(JvUIBQuery1.Transaction) then FBError(fbeTransactionNotAssigned, [DS.Name]);
  OldDataBaseConnected:=JvUIBQuery1.DataBase.Connected;

  //Fill generator list
  JvUIBQuery1.Execute;
  ComboBox2.Items.Clear;
  try
    while not JvUIBQuery1.Eof do
    begin
      ComboBox2.Items.Add(Trim(JvUIBQuery1.Fields.AsString[0]));
      JvUIBQuery1.Next
    end;
  finally
    JvUIBQuery1.Close;
    JvUIBQuery1.DataBase.Connected:=OldDataBaseConnected;
  end;
  if ComboBox2.Items.IndexOf(DS.AutoUpdateOptions.GeneratorName)<>-1 then
    ComboBox2.ItemIndex:=ComboBox2.Items.IndexOf(DS.AutoUpdateOptions.GeneratorName);

  //Fill field list
  ComboBox1.Items.Clear;
  DS.FieldDefs.Update;
  for i:=0 to DS.FieldDefs.Count-1 do
    if DS.FieldDefs[i].DataType in [ftSmallint, ftInteger, ftWord, ftFloat,
      ftCurrency, ftBCD, ftAutoInc, ftLargeint] then
      ComboBox1.Items.Add(DS.FieldDefs[i].Name);
  ComboBox1.Text:=DS.AutoUpdateOptions.KeyField;
  Edit1.Text:=IntToStr(DS.AutoUpdateOptions.IncrementBy);
  RadioGroup1.ItemIndex:=ord(DS.AutoUpdateOptions.WhenGetGenID);

  if ShowModal = mrOK then
  begin
    DS.AutoUpdateOptions.IncrementBy:=StrToIntDef(Edit1.Text, 0);
    DS.AutoUpdateOptions.WhenGetGenID:=TWhenGetGenID(RadioGroup1.ItemIndex);
    DS.AutoUpdateOptions.KeyField:=ComboBox1.Text;
    DS.AutoUpdateOptions.GeneratorName:=ComboBox2.Items[ComboBox2.ItemIndex];
    Result:=true;
  end;
end;

end.
