unit pe_simpletable_collist;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, ComCtrls, Menus, Buttons, sql3_defs;

type
  Tfpe_st_collist = class(TForm)
    pan: TPanel;
    Button1: TButton;
    Cancel: TButton;
    CheckCols: TCheckListBox;
    Popup: TPopupMenu;
    Selectall1: TMenuItem;
    Unselectall1: TMenuItem;
    upb: TSpeedButton;
    dnb: TSpeedButton;
    procedure UpDownClick(Sender: TObject);
    procedure SelectClick(Sender: TObject);
    procedure CheckColsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CheckColsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private
    { Private declarations }
    procedure move_up;
    procedure move_down;
    procedure set_order(list: TStringList; fields: String);
  public
    { Public declarations }
    function show_editor(table: TSivak3SimpleTable; var ColList: String): Boolean;   
  end;

implementation

{$R *.dfm}

procedure Tfpe_st_collist.move_down;
var
  i: Integer;
begin
  i := CheckCols.ItemIndex;
  if (i >= 0) and (i < CheckCols.Items.Count - 1) then
  begin
    CheckCols.Items.Exchange(i, i + 1);
    CheckCols.ItemIndex := i + 1;
  end;
end;

procedure Tfpe_st_collist.move_up;
var
  i: Integer;
begin
  i := CheckCols.ItemIndex;
  if (CheckCols.ItemIndex > 0) then
  begin
    CheckCols.Items.Exchange(i, i - 1);
    CheckCols.ItemIndex := i - 1;
  end;
end;

var
  _fields: String;

function _sort(l: TStringList; i1, i2: Integer): Integer;
var
  x, y: Integer;
begin
  x := pos(AnsiUpperCase(l.Strings[i1]), _fields);
  if x < 1 then
  x := 8000 + i1;
  y := pos(AnsiUpperCase(l.Strings[i2]), _fields);
  if y < 1 then
  y := 8000 + i2;
  Result := x - y;
end;

procedure Tfpe_st_collist.set_order(list: TStringList; fields: String);
begin
  _fields := fields;
  list.CustomSort(_sort);
  CheckCols.Items.Assign(list);
end;

procedure Tfpe_st_collist.UpDownClick(Sender: TObject);
begin
  if CheckCols.Items.Count > 2 then
  if TComponent(Sender).Tag = 1 then
  move_down
  else move_up;
end;

procedure Tfpe_st_collist.SelectClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to CheckCols.Items.Count - 1 do
  CheckCols.Checked[i] := TMenuItem(Sender).Tag = 1;
end;

procedure Tfpe_st_collist.CheckColsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  p: TPoint;
  i, j: Integer;
begin
  p.X := X;
  p.Y := Y;
  if (Sender is TCheckListBox) and (Source is TCheckListBox) then
  begin
    i := TCheckListBox(Source).ItemIndex;
    j := TCheckListBox(Sender).ItemAtPos(p, true);
    if (i >= 0) and (i < TCheckListBox(Source).Items.Count) and (j >= 0) and (j < TCheckListBox(Sender).Items.Count) then
    begin
      TCheckListBox(Sender).Items.Exchange(i, j);
      TCheckListBox(Sender).ItemIndex := j;
    end;
  end;
end;

procedure Tfpe_st_collist.CheckColsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender is TCheckListBox) and (Source is TCheckListBox);
end;

function Tfpe_st_collist.show_editor(table: TSivak3SimpleTable; var ColList: String): Boolean;
var
  l: TStringList;
  i: Integer;
begin
  ColList := AnsiUpperCase(Table.ColumnList) + ',';
  while pos(' ,', ColList) > 0 do
  Delete(ColList, pos(' ,', ColList), 1);

  l := TStringList.Create;
  try
    Table.Database.GetFieldNames(l, Table.TableName);
    set_order(l, ColList);
    for i := 0 to CheckCols.Items.Count - 1 do
    CheckCols.Checked[i] := pos(AnsiUpperCase(CheckCols.Items.Strings[i]) + ',', ColList) > 0;

    Result := ShowModal = mrOk;
    if Result then
    begin
      ColList := '';
      for i := 0 to CheckCols.Items.Count - 1 do
      if CheckCols.Checked[i] then
      ColList := ColList + CheckCols.Items.Strings[i] + ',';
      if length(ColList) > 0 then
      Delete(ColList, length(ColList), 1);
    end;
  finally
    l.Free;
  end;
end;

end.
