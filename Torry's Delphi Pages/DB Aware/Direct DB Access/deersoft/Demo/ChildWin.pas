unit Childwin;

interface

uses Windows, Classes, Dialogs, Graphics, Forms, Controls, StdCtrls, Grids, DBGrids,
  Menus, SysUtils, Db, DDB, DTables, ExtCtrls, DMaster, DBCtrls, ToolWin, ComCtrls,
  Mask, DUtils;

type
  TGridColumn = procedure (Source: TDataSource; Field: TField) of object;
  TMDIChild = class(TForm)
    Grid: TDBGrid;
    GridMenu: TPopupMenu;
    Filter1: TMenuItem;
    miFilterAdd: TMenuItem;
    miFilterOne: TMenuItem;
    miFilterAll: TMenuItem;
    miExport: TMenuItem;
    Table: TDTable;
    DataSource: TDataSource;
    ToolBar: TToolBar;
    cbxTable: TComboBox;
    Navigator: TDBNavigator;
    panCounter: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    edSearch: TMaskEdit;
    miColor: TMenuItem;
    chkIncremental: TCheckBox;
    miColorFlt: TMenuItem;
    procedure cbxTableChange(Sender: TObject);
    procedure TableScroll(DataSet: TDataSet);
    procedure GridTitleClick(Column: TColumn);
    procedure GridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TableFillStart(DataSet: TDataSet);
    procedure GridColEnter(Sender: TObject);
  private
    { Private declarations }
    bShift     : Boolean;
    evOnOpen   : TNotifyEvent;
    evOnColumn : TGridColumn;

  public
    { Public declarations }

    property OnOpen   : TNotifyEvent read evOnOpen   write evOnOpen;
    property OnColumn : TGridColumn  read evOnColumn write evOnColumn;

  end;

implementation

uses Main;

{$R *.DFM}

procedure TMDIChild.cbxTableChange(Sender: TObject);
begin
     if cbxTable.Text <> Table.TableName then
     begin
          Table.Close();
          Table.TableName := cbxTable.Text;
          Table.Open();
          Grid.SetFocus();
          if Assigned(evOnOpen) then OnOpen(Self);
     end;
end;


procedure TMDIChild.TableScroll(DataSet: TDataSet);
begin
     panCounter.Caption := Table.Position;
end;


procedure TMDIChild.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     bShift := (ssShift in Shift);
end;

procedure TMDIChild.GridTitleClick(Column: TColumn);
begin
     Table.SortAdd(Column.Field, bShift);
     bShift := False;
end;


procedure TMDIChild.GridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
   x          : Integer;
   y          : Integer;
   i          : Integer;
   clPaper    : TColor;
   clInk      : TColor;
   clSavePaper: TColor;
   clSaveInk  : TColor;
   xSaveStyle : TFontStyles;
begin
     if Assigned(Column.Field) then
     begin
          // Boolean type
          if (Column.Field.DataType = ftBoolean) then
          begin
               x := Rect.Right-17;
               y := Rect.Top;
               if Column.Field.AsBoolean then i := 6 else i := 7;
               MainForm.ImageList1.Draw(Grid.Canvas, x, y, i);
          end;
          if State = [] then
          begin
               clPaper     := Grid.Canvas.Brush.Color;
               clInk       := Grid.Font.Color;
               clSavePaper := Grid.Canvas.Brush.Color;
               clSaveInk   := Grid.Font.Color;
               Table.FieldColors(Column.FieldName, clPaper, clInk);
               if (clPaper <> clSavePaper) or (clInk <> clSaveInk) then
               begin
                    // Save colors and fontstate
                    xSaveStyle := Canvas.Font.Style;
                    // Set the new values
                    Grid.Canvas.Font.Style  := Font.Style + [fsBold];
                    Grid.Canvas.Brush.Color := clPaper;
                    Grid.Canvas.Font.Color  := clInk;
                    Grid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
                    // Load colors and fontstate
                    Grid.Canvas.Brush.Color := clSavePaper;
                    Grid.Canvas.Font.Color  := clSaveInk;
                    Grid.Canvas.Font.Style  := xSaveStyle;
               end;
          end;

          // Draw Sort arrows
          x := Rect.Right-18;
          y := 0;
          if Rect.Top < 20 then
          begin
               case Column.Field.Tag of
                    cniSortASC  : MainForm.ImageList1.Draw(Grid.Canvas, x, y, 4);
                    cniSortDESC : MainForm.ImageList1.Draw(Grid.Canvas, x, y, 5);
               end;
          end;
     end;
end;


procedure TMDIChild.SearchChange(Sender: TObject);
begin
     if Assigned(Grid.SelectedField) then
     begin
          if not Table.IsSorted(Grid.SelectedField) then Table.SortAdd(Grid.SelectedField);
          if edSearch.Text = ''
          then
              Table.First()
          else
              Table.Search(Grid.SelectedField, edSearch.Text, chkIncremental.Checked);
     end;
end;


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree;
end;

procedure TMDIChild.TableFillStart(DataSet: TDataSet);
begin
     Grid.Refresh();
end;

procedure TMDIChild.GridColEnter(Sender: TObject);
begin
     if Assigned(evOnColumn) and Assigned(Grid.SelectedField) then
     begin
          OnColumn(DataSource, Grid.SelectedField);
     end;
end;

end.
