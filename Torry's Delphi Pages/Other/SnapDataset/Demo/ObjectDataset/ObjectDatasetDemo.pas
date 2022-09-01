unit ObjectDatasetDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, ExtCtrls, DBCtrls, StdCtrls, ObjectDataset,
  ObjectDatasetClass, Mask;

type
  TFrmObjectDatasetDemo = class(TForm)
    DBGrid1: TDBGrid;
    dsMain: TDataSource;
    Panel2: TPanel;
    Dataset: TObjectDataset;
    DatasetAvailable: TBooleanField;
    DatasetFirstName: TStringField;
    DatasetLastName: TStringField;
    DatasetDescription: TMemoField;
    DBCheckBox2: TDBCheckBox;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    DatasetDate: TDateField;
    DatasetTime: TTimeField;
    DatasetDatetime: TDateTimeField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label5: TLabel;
    DBEdit4: TDBEdit;
    Label6: TLabel;
    DBEdit5: TDBEdit;
    DatasetByte: TIntegerField;
    DatasetWord: TIntegerField;
    DatasetShortint: TIntegerField;
    DatasetInteger: TIntegerField;
    DatasetInt64: TIntegerField;
    Label7: TLabel;
    Label8: TLabel;
    DBEdit6: TDBEdit;
    Label9: TLabel;
    DBEdit7: TDBEdit;
    Label10: TLabel;
    DBEdit8: TDBEdit;
    Label11: TLabel;
    DBEdit9: TDBEdit;
    Label12: TLabel;
    DBEdit10: TDBEdit;
    Datasetcalc: TIntegerField;
    Label13: TLabel;
    DBEdit11: TDBEdit;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DatasetCalcFields(DataSet: TDataSet);
    procedure DatasetFilterRecord(DataSet: TDataSet;var Accept: Boolean);
    procedure CheckBox1Click(Sender: TObject);
  end;

var
  FrmObjectDatasetDemo: TFrmObjectDatasetDemo;

implementation

{$R *.DFM}


procedure TFrmObjectDatasetDemo.FormCreate(Sender: TObject);
var o: TTestItem;
begin
  with DataSet do begin
    o := TTestItem.Create;
    with o do begin
      LastName := 'Ellen';
      FirstName := 'Marius';
      Description.Add('Comments to finalist@home.nl'+#13#10+'Thanks to an excellent model of GExpert!');
    end;
    ObjectList.Add(o);

    o := TTestItem.Create;
    with o do begin
      LastName := 'Steward';
      FirstName := 'Rod';
      Available := true;
    end;
    ObjectList.Add(o);

    o := TTestItem.Create;
    with o do begin
      LastName := 'Chaplin';
      FirstName := 'Charlie';
    end;
    ObjectList.Add(o);

    o := TTestItem.Create;
    with o do begin
      LastName := 'Wonder';
      FirstName := 'Stevie';
      Available := true;
    end;
    ObjectList.Add(o);
  end;
  Dataset.Open;
end;


procedure TFrmObjectDatasetDemo.Button1Click(Sender: TObject);
var i: integer;
begin
  for i := 0 to 4 do begin
    Dataset.Insert;
    DatasetFirstName.Asstring := '12345';
    DatasetLastName.Asstring := '12345';
    Dataset.Post;
  end;
end;


procedure TFrmObjectDatasetDemo.DatasetCalcFields(DataSet: TDataSet);
begin
  Dataset.FieldByname('calc').asinteger := 1234567;
end;


procedure TFrmObjectDatasetDemo.DatasetFilterRecord(DataSet: TDataSet;var Accept: Boolean);
begin
  Accept := Dataset.FieldByname('Available').asboolean;
end;


procedure TFrmObjectDatasetDemo.CheckBox1Click(Sender: TObject);
begin
  Dataset.Filtered := CheckBox1.Checked;
end;

end.
