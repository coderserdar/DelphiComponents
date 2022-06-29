unit f_selectRecords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Grids, DBGrids,
  cyBaseDBGrid, cyDBAdvGrid, DB, DBClient, cyStrUtils, Buttons, ImgList;

type
  TFrmSelectRecords = class(TForm)
    DBGrid1: TcyDBAdvGrid;
    Panel1: TPanel;
    Button1: TButton;
    DataSource1: TDataSource;
    SBUncheckAll: TSpeedButton;
    SBCheckAll: TSpeedButton;
    ImgList: TImageList;
    BtnEditFind: TButtonedEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCheckAllClick(Sender: TObject);
    procedure BtnEditFindChange(Sender: TObject);
  private
    { Private declarations }
    EditFilterField: String;
    SelectFromDataSet: TDataSet;
  public
    { Public declarations }
  end;

var
  FrmSelectRecords: TFrmSelectRecords;

  function CreateSelectRecordsForm(const Title: string; const Width, Height: Integer; const DataSet: TDataSet; const ListField, ClientColumnField: string; const CheckAll: Boolean = false): TFrmSelectRecords;

implementation

{$R *.dfm}

function CreateSelectRecordsForm(const Title: string; const Width, Height: Integer; const DataSet: TDataSet; const ListField, ClientColumnField: string; const CheckAll: Boolean = false): TFrmSelectRecords;
var
  f: Integer;
  Column: TColumn;
begin
  Application.CreateForm(TFrmSelectRecords, Result);
  Result.Caption := Title;
  Result.Width := Width;
  Result.Height := Height;
  Result.SelectFromDataSet := Dataset;
  Result.DataSource1.Dataset := Dataset;

  if ClientColumnField <> '' then
  begin
    Result.DBGrid1.ClientColumn.FieldName := ClientColumnField;
    Result.DBGrid1.ClientColumn.Enabled := true;
  end;

  Result.DBGrid1.Columns.Clear;

  for f := 1 to Substring_count(ListField, ';') do
  begin
    Column := Result.DBGrid1.Columns.Add;
    Column.FieldName := SubString_Get(ListField, ';', f);
  end;

  if Result.DBGrid1.Columns.Count <> 0
  then Result.EditFilterField := Result.DBGrid1.Columns[0].FieldName
  else Result.EditFilterField := '';

  if CheckAll then
    Result.DBGrid1.CheckAllRecords;
end;

procedure TFrmSelectRecords.BtnEditFindChange(Sender: TObject);
begin
  if EditFilterField = '' then
    Exit;

  if SelectFromDataSet is TCustomClientDataSet then
  begin
    SelectFromDataSet.DisableControls;

    try
      SelectFromDataSet.Filtered := false;

      if BtnEditFind.Text <> '' then
      begin
        SelectFromDataSet.Filter := EditFilterField + ' LIKE ' + QuotedStr('%' + BtnEditFind.Text + '%');
        SelectFromDataSet.Filtered := true;
      end;
    finally
      SelectFromDataSet.EnableControls;
    end;
  end
  else begin
    if BtnEditFind.Text <> '' then
      SelectFromDataSet.Locate(EditFilterField, BtnEditFind.Text, [loPartialKey, loCaseInsensitive]);
  end;
end;

procedure TFrmSelectRecords.FormCreate(Sender: TObject);
begin
  //
end;

procedure TFrmSelectRecords.SBCheckAllClick(Sender: TObject);
begin
  if Sender = SBCheckAll
  then DBGrid1.CheckAllRecords
  else DBGrid1.CheckedList.Clear;
end;

end.
