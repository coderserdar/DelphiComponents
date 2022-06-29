unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, DBTables, DBSumLst, StdCtrls, ExtCtrls, DBCtrls;

type
  TForm1 = class(TForm)
    Query1: TQuery;
    Query1VNo: TFloatField;
    Query1VName: TStringField;
    Query1PNo: TFloatField;
    Query1PDescription: TStringField;
    Query1PCost: TCurrencyField;
    Query1IQty: TIntegerField;
    DBGrid1: TDBGrid;
    DBSumList1: TDBSumList;
    DataSource1: TDataSource;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    bFiltr: TButton;
    DBNavigator1: TDBNavigator;
    bOpenClose: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    procedure Query1UpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure DBSumList1SumListChanged(Sender: TObject);
    procedure bFiltrClick(Sender: TObject);
    procedure bOpenCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Query1UpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
 // Allow to change records
end;

procedure TForm1.DBSumList1SumListChanged(Sender: TObject);
begin
  Edit1.Text := FloatToStr(DBSumList1.SumCollection.Items[0].SumValue);
  Edit2.Text := FloatToStr(DBSumList1.SumCollection.Items[1].SumValue);
  Edit3.Text := FloatToStr(DBSumList1.SumCollection.Items[2].SumValue);
end;

procedure TForm1.bFiltrClick(Sender: TObject);
begin
  if (Query1.Filtered = True) then begin
    Query1.Filtered := False;
    bFiltr.Caption := 'Filtr';
  end else begin
    Query1.Filtered := True;
    bFiltr.Caption := 'UnFiltr';
  end;
end;

procedure TForm1.bOpenCloseClick(Sender: TObject);
begin
  if (Query1.Active = False) then begin
    Query1.Active := True;
    bOpenClose.Caption := 'Close';
  end else begin
    Query1.Active := False;
    bOpenClose.Caption := 'Open';
  end;
end;

end.
