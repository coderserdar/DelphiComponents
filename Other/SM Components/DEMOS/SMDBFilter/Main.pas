unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SMDBFltr, Grids, DBGrids, ComCtrls, Db, DBTables;

type
  TfrmDemo = class(TForm)
    tblDemo: TTable;
    dSrcTblDemo: TDataSource;
    pcDemo: TPageControl;
    tsTable: TTabSheet;
    DBGridTable: TDBGrid;
    SMDBFilterDialogForTable: TSMDBFilterDialog;
    SMDBFilterDialogForQuery2: TSMDBFilterDialog;
    btnTableFilter: TButton;
    tsQueryWhere: TTabSheet;
    btnQueryWhere: TButton;
    DBGridQuery2: TDBGrid;
    qryDemo: TQuery;
    dSrcQryDemo: TDataSource;
    tsQuery: TTabSheet;
    btnQueryFilter: TButton;
    DBGridQuery1: TDBGrid;
    SMDBFilterDialogForQuery1: TSMDBFilterDialog;
    btnTableFields: TButton;
    procedure btnTableFilterClick(Sender: TObject);
    procedure btnQueryWhereClick(Sender: TObject);
    procedure btnQueryFilterClick(Sender: TObject);
    procedure btnTableFieldsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.DFM}
uses AvailFld;

procedure TfrmDemo.btnTableFilterClick(Sender: TObject);
begin
  SMDBFilterDialogForTable.Execute
end;


procedure TfrmDemo.btnQueryWhereClick(Sender: TObject);
var
  s: string;
begin
  if SMDBFilterDialogForQuery2.Execute then
  begin
    s := SMDBFilterDialogForQuery2.Expression;
    if s <> '' then
      s := 'WHERE ' + s;
    qryDemo.SQL.Text := 'SELECT * FROM ORDERS ' + s;
  end;
end;


procedure TfrmDemo.btnQueryFilterClick(Sender: TObject);
begin
  SMDBFilterDialogForQuery1.Execute
end;


procedure TfrmDemo.btnTableFieldsClick(Sender: TObject);
var
  frmAvailFields: TfrmAvailFields;
  i: Integer;
begin
  frmAvailFields := TfrmAvailFields.Create(Application);
  try
    frmAvailFields.CheckListBox.Items.Clear;

    {fill the field list}
    for i := 0 to SMDBFilterDialogForTable.Dataset.FieldCount-1 do
      frmAvailFields.CheckListBox.Items.Add(SMDBFilterDialogForTable.Dataset.Fields[i].FieldName);

    {set the checkbox for available field}
    for i := 0 to frmAvailFields.CheckListBox.Items.Count-1 do
      if (SMDBFilterDialogForTable.AllowedFields.Count = 0) or
         (SMDBFilterDialogForTable.AllowedFields.IndexOf(frmAvailFields.CheckListBox.Items[i]) > -1) then
        frmAvailFields.CheckListBox.Checked[i] := True;

    if (frmAvailFields.ShowModal = mrOk) then
    begin
      SMDBFilterDialogForTable.AllowedFields.Clear;
      for i := 0 to frmAvailFields.CheckListBox.Items.Count-1 do
        if frmAvailFields.CheckListBox.Checked[i] then
          SMDBFilterDialogForTable.AllowedFields.Add(frmAvailFields.CheckListBox.Items[i]);
    end;
  finally
    frmAvailFields.Free
  end;
end;

end.
