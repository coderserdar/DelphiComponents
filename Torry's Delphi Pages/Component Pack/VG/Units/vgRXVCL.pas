unit vgRXVCL;

{$D-,L-}
interface
uses SysUtils, Placemnt, ComCtrls;

procedure SaveListViewColumns(Placement: TFormPlacement; List: TCustomListView);
procedure RestoreListViewColumns(Placement: TFormPlacement; List: TCustomListView);

implementation

type
  TListViewHack = class(TCustomListView);

procedure RestoreListViewColumns(Placement: TFormPlacement; List: TCustomListView);
  procedure RestoreColumn(Column: TListColumn);
  begin
    with Placement, Placement.RegIniFile do
    begin
      Column.Width := ReadInteger(IniSection, Format('Column%d', [Column.Index]), Column.Width);
    end;
  end;
var
  I: Integer;
begin
  for I := 0 to TListViewHack(List).Columns.Count - 1 do
    RestoreColumn(TListViewHack(List).Columns[I]);
end;

procedure SaveListViewColumns(Placement: TFormPlacement; List: TCustomListView);
  procedure SaveColumn(Column: TListColumn);
  begin
    with Placement, Placement.RegIniFile do
    begin
      WriteInteger(IniSection, Format('Column%d', [Column.Index]), Column.Width);
    end;
  end;
var
  I: Integer;
begin
  for I := 0 to TListViewHack(List).Columns.Count - 1 do
    SaveColumn(TListViewHack(List).Columns[I]);
end;

end.
