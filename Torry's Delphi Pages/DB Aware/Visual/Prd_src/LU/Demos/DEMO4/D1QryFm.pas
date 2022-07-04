unit D1qryfm;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Dblup2, Grids, DBGrids, DB, DBTables;

type
  TForm1 = class(TForm)
    TableOrds: TTable;
    DataSourceOrds: TDataSource;
    DBLookupComboPlus1: TDBLookupComboPlus;
    QueryEMP: TQuery;
    DataSourceEmpQry: TDataSource;
    Memo1: TMemo;
    procedure DBLookupComboPlus1Translate(Sender: TObject;
      var RecFound: Boolean);
    procedure DBLookupComboPlus1SearchKeyPress(Sender: TObject;
      var RecFound: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DBLookupComboPlus1Translate(Sender: TObject;
  var RecFound: Boolean);

var
  LUValue, DataValue : String;
begin
  with DBLookupComboPlus1.LookupSource.DataSet do
  begin
    First;
    DataValue :=
      DBLookupComboPlus1.DataSource.DataSet.FieldByName(DBLookupComboPlus1.DataField).AsString;
    While not EOF do
    begin
      LUValue := FieldByName(DBLookupComboPlus1.LookupField).AsString;
      if LUValue = DataValue  then
      begin
        RecFound := True;
        break;
      end;
      next;
    end;
    If not RecFound then
      showmessage('OnTranslate failed to match the value');
  end;
end;

procedure TForm1.DBLookupComboPlus1SearchKeyPress(Sender: TObject;
  var RecFound: Boolean);
var
 I : Integer;
 LUValue, DataValue : String;
begin
  with DBLookupComboPlus1.LookupSource.DataSet do
  begin
    RecFound := False;
    First;
    LUValue := DBLookupComboPlus1.SearchValue;
    While not EOF do
    begin
      DataValue := FieldByName(DBLookupComboPlus1.LookupDisplay).AsString;
      I := AnsiCompareText(LUValue,DataValue);
      If I <= 0 then
      begin
        if (1 = Pos(LUValue, DataValue)) then
        begin
          RecFound := True;
          break;
        end
        else
          prior;
          RecFound := True;
          break;
      end;
      next;
    end;
  end;
end;

end.
