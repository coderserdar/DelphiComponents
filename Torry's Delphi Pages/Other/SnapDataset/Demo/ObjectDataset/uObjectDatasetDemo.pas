unit uObjectDatasetDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Db, Controls,
  ObjectDatasetClass, DBCtrls, StdCtrls, Mask, ExtCtrls, Grids,
  DBGrids, Contnrs, SnapObjectDataset, SnapBaseDataset;


type
  TFrmObjectDatasetDemo = class(TForm)
    DBGrid1: TDBGrid;
    dsMain: TDataSource;
    Panel2: TPanel;
    Label4: TLabel;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    CheckBox1: TCheckBox;
    DBCheckBox1: TDBCheckBox;
    Label21: TLabel;
    DBImage1: TDBImage;
    LabelInfo: TLabel;
    dsSubItems: TDataSource;
    GroupBox1: TGroupBox;
    DBGrid2: TDBGrid;
    Label22: TLabel;
    Label23: TLabel;
    DBEdit11: TDBEdit;
    DBEdit12: TDBEdit;
    DBNavigator2: TDBNavigator;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    DBEditByte: TDBEdit;
    DBEditWord: TDBEdit;
    DBEditShort: TDBEdit;
    DBEditInteger: TDBEdit;
    DBEditSmall: TDBEdit;
    DBEditInt64: TDBEdit;
    DBEditCardinal: TDBEdit;
    DBEditLongint: TDBEdit;
    DBEditLongword: TDBEdit;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label17: TLabel;
    Label24: TLabel;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit7: TDBEdit;
    DBEdit10: TDBEdit;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    DBEdit1: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit8: TDBEdit;
    DBEdit9: TDBEdit;
    EditMoney: TDBEdit;
    rgObjectClass: TRadioGroup;
    SnapObjectDataset1: TSnapObjectDataset;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tbDatasetCalcFields(DataSet: TDataSet);
    procedure tbDatasetFilterRecord(DataSet: TDataSet;var Accept: Boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure tbDatasetObjectCreate(Sender: TObject; var NewInstance: TObject);
    procedure tbSubItemsObjectCreate(Sender: TObject; var NewInstance: TObject);
    procedure dsMainDataChange(Sender: TObject; Field: TField);
    procedure InsertRandomValue(O: TTestItem);
    procedure rgObjectClassClick(Sender: TObject);
  private
    TheObjectList: TObjectList;
    TheCollection: TCollection;
    tbDataSet: TSnapObjectDataset;
    tbSubItems: TSnapObjectDataset;
  end;

var
  FrmObjectDatasetDemo: TFrmObjectDatasetDemo;

implementation

{$R *.DFM}


procedure TFrmObjectDatasetDemo.FormCreate(Sender: TObject);
var
  o: TTestItem;
begin
  tbDataSet := TSnapObjectDataset.Create(Self);
  dsMain.DataSet := tbDataSet;
  tbDataSet.OnCalcFields := tbDatasetCalcFields;
  tbDataSet.OnFilterRecord := tbDatasetFilterRecord;

  tbSubItems := TSnapObjectDataset.Create(Self);
  dsSubItems.DataSet := tbSubItems;

  TheCollection := TCollection.Create(TTestItem);
  TheObjectList := TObjectList.Create(True);

  // Add into the collection
  o := TheCollection.Add as TTestItem;;
  InsertRandomValue(o);
  // create and add into the objectlist
  o := TTestItem.Create(nil);
  InsertRandomValue(o);
  TheObjectList.Add(o);

  //tbDataset.ObjectClass := TTestItem;
  //tbDataset.ObjectInstance := ObjectList;
  tbDataset.ObjectInstance := TheCollection;

  tbSubItems.MasterSource := dsMain;
  tbSubItems.MasterAttribute := 'SubItems';
  //tbSubItems.DataSetField := tbDatasetSubItems;
  //tbSubItems.ObjectClass := TTestSubItem;
  //Wow, we've got a single record.
  if tbDataset.Active then
    tbDataset.Refresh
  else
    tbDataset.Open;
  tbSubItems.Open;
end;


procedure TFrmObjectDatasetDemo.FormDestroy(Sender: TObject);
begin
  TheObjectList.Free;
  TheCollection.free;
  tbSubItems.Free;
  tbDataSet.Free;
end;

procedure TFrmObjectDatasetDemo.Button1Click(Sender: TObject);
//Some BS data to show the capabilities of the TObjectDataset
var
  i: integer;
  n: integer;
  o: TTestItem;
begin
  Randomize;
  n := Random(10);

  for i := 0 to n do
  begin
    // Add into the collection
    o := TheCollection.Add as TTestItem;
    InsertRandomValue(o);
    // create and add into the objectlist
    o := TTestItem.Create(nil);
    InsertRandomValue(o);
    TheObjectList.Add(o);
  end;
  tbDataset.Refresh;
end;


procedure TFrmObjectDatasetDemo.tbDatasetCalcFields(Dataset: TDataSet);
//It even supports calculated fields
begin
  tbDataset.FieldByname('calc').asinteger := tbdataset.Recno;
  tbDataset.FieldByname('calc2').asstring := 'Hello world ' + inttostr(tbdataset.Recno);
end;


procedure TFrmObjectDatasetDemo.tbDatasetFilterRecord(Dataset: TDataSet;var Accept: Boolean);
//Ok, filtering is possible, but it ain't the nicest approach ;-)
begin
  Accept := tbDataset.FieldByname('Available').asboolean;
end;


procedure TFrmObjectDatasetDemo.CheckBox1Click(Sender: TObject);
begin
  tbDataset.Filtered := CheckBox1.Checked;
end;


procedure TFrmObjectDatasetDemo.tbDatasetObjectCreate(Sender: TObject; var NewInstance: TObject);
//Since the overruled constructor ain't called (TPersistent hasn't got a
//virtual constructor) we have to create the object ourselves.. This has to
//be done this way to make sure you're object constructor(s) are called!!
var
  o: TTestItem;
  oSubItem : TTestSubItem;
begin
(*
  //o := TTestItem.Create;
  o:=TheCollection.add as TTestItem;
  with o do begin
    //And btw the TObjectDataset does take default values
    AnsiName := 'Record';
    ShortName := 'Added';
    WideName := 'Record added';
    Available := true;
  end;

  if (nId and 3)=0 then
  begin
    //Some random Subitems
    oSubItem := o.SubItems.Add as TTestSubItem;//TTestSubItem.Create(nil);
    oSubItem.Id := nId;
    oSubItem.Name := 'New record';
    //o.SubItems.Add(oSubItem);
    inc(nId);
  end;

  NewInstance := o;
*)
end;


procedure TFrmObjectDatasetDemo.tbSubItemsObjectCreate(Sender: TObject; var NewInstance: TObject);
//This is a subitem which goes into to the TTestItem.SubItems list.
//We can show its data via the TObjectdataset..
begin
  //NewInstance := TTestSubItem.Create;
end;


procedure TFrmObjectDatasetDemo.dsMainDataChange(Sender: TObject; Field: TField);
var
  n: integer;
begin
  n := 0;
  if TheCollection<>nil then
    n := TheCollection.Count;
  LabelInfo.Caption := Format('List.Count=%d, RecordCount=%d, Recno=%d, DetailRecordCount=%d',[n, tbDataset.RecordCount, tbDataset.RecNo, tbSubItems.RecordCount]);
end;

procedure TFrmObjectDatasetDemo.InsertRandomValue(O: TTestItem);
const
  Names: array[0..9] of string = ('Mimmo', 'Giulio', 'Elena', 'Clara', 'Patrizia', 'Fabrizio', 'Roberto', 'Stefano', 'Andrea', 'Giovanna');
  Images: array[0..3] of string = ('image1.bmp', 'image2.bmp', 'image3.bmp', 'image4.bmp');
  Memos: array[0..1] of string = ('Thanks to a good idea of Paul Johnson and of an excellent article written by GExperts'#13#10'http://www.gexperts.com by Gerald Nunn', 'SnapObjects');
var
  nId, j, n, m: integer;
  so: TTestSubItem;
begin
  nId := o.Index;

  o.Available := nId and 1 > 0;
  o.Boolean := nId and 1 = 0;
  o.ByteBool := nId and 1 > 0;
  o.WordBool := nId and 1 = 0;
  o.LongBool := nId and 1 > 0;

  n := Random(10);
  o.Character := Names[n][1];
  o.AnsiName := Names[n] + ' ' + inttostr(nId);
  o.WideName := Names[n] + ' ' + inttostr(nId + 1000);
  o.ShortName := Names[n] + ' ' + inttostr(nId + 2000);

  o.Shortint := (nId and 127);
  o.Byte := (nId + 1) and 255;
  o.Smallint := nId + 2;
  o.Word := nId + 3;
  o.Integer := nId;
  o.LongInt := nId + 5;
  o.Cardinal := nId + 6;
  o.LongWord := nId + 7;
  o.Int64 := nId + 8;

  o.Real := (nId + 9) * 10 / 1000;
  o.Double := (nId + 10) * 10 / 1000;
  o.Extended := (nId + 11) * 10 / 1000;
  o.Money := (nId + 12) * 10 / 1000;

  o.Date := Now;
  o.Time := Now;
  o.DateTime := Now;

  n := Random(4);
  o.Picture.LoadFromFile(Images[n]);
  n := Random(2);
  o.Memo.Text := Memos[n];

  m := Random(10);
  for j:=0 to m do
  begin
    so := o.SubItems.Add as TTestSubItem;
    so.Id := j;
    so.Name := format('%d. %d %s', [nId, j, 'sub item']);
  end;
end;

procedure TFrmObjectDatasetDemo.rgObjectClassClick(Sender: TObject);
begin
  tbDataset.Close;
  case rgObjectClass.ItemIndex of
    0: begin { TCollection }
         tbDataset.ObjectInstance := TheCollection;
       end;
    1: begin { TObjectList }
         tbDataset.ObjectClassName := TTestItem.ClassName;
         tbDataset.ObjectInstance := TheObjectList;
       end;
  end;
  tbDataset.Open;
end;

end.
