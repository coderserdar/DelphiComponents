unit ce_simple_data;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, sql3_defs, ComCtrls;

type
  Tfce_simple_data = class(TForm)
    grid: TStringGrid;
    Status: TStatusBar;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure set_table(table: TSivak3SimpleTable);
  end;

implementation

{$R *.dfm}

{ Tfce_simple_data }

procedure Tfce_simple_data.set_table(table: TSivak3SimpleTable);
var
  c, r: Integer;
begin
  if Assigned(table) then
  try
    Status.Panels[0].Text := '  ' + table.TableName;
    table.Open;
    try
      if table.IsEmpty then
      Status.Panels[1].Text := '  Record count = 0, table is empty.'
      else Status.Panels[1].Text := '  Record count = ' + IntToStr(table.RowCount);

      if table.RowCount > 256 then
      grid.RowCount := 258
      else grid.RowCount := table.RowCount + 1;
      
      grid.ColCount := table.ColCount + 1;
      for c := 0 to table.ColCount - 1 do
      begin
        grid.ColWidths[c + 1] := 128;
        grid.Cells[c + 1, 0] := String(table.ColumnName[c]);
      end;

      //ShowMessage(IntToStr(table.RowCount));
      for r := 0 to table.RowCount - 1 do
      begin
        if r >= 256 then
        begin
          grid.Cells[0, r + 1] := 'Etc...';
          for c := 0 to table.ColCount - 1 do
          grid.Cells[c + 1, r + 1] := '...';
          Break;
        end;
        grid.Cells[0, r + 1] := IntToStr(r + 1);
        for c := 0 to table.ColCount - 1 do
        grid.Cells[c + 1, r + 1] := String(table.ColumnValue[c, r]);
      end;
    finally
      table.Close;
    end;
  except
    on e: Exception do
    Status.Panels[1].Text := '  ' + e.Message;
  end;
end;

end.
