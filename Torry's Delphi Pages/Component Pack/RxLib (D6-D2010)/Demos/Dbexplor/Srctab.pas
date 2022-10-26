unit SrcTab;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Grids, StdCtrls, Mask, ToolEdit, Placemnt, DB,
  DBLists, DBTables, CurrEdit;

type
  TSrcTableDlg = class(TForm)
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    RecordCountBox: TGroupBox;
    Label2: TLabel;
    FirstRecsBtn: TRadioButton;
    AllRecsBtn: TRadioButton;
    MapGrid: TStringGrid;
    Label3: TLabel;
    MapBtn: TButton;
    Collapsed: TBevel;
    Expanded: TBevel;
    FormStorage: TFormStorage;
    TableFields: TTableItems;
    Label4: TLabel;
    ModeCombo: TComboBox;
    SrcNameEdit: TFilenameEdit;
    RecordCntEdit: TCurrencyEdit;
    procedure FormCreate(Sender: TObject);
    procedure MapBtnClick(Sender: TObject);
    procedure SrcNameEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure AllRecsBtnClick(Sender: TObject);
    procedure MapGridSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
  private
    { Private declarations }
    FExpanded: Boolean;
    FDstTable: TTable;
    procedure UpdateFormView;
    procedure UpdateMapGrid;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

function GetImportParams(const DstTable: TTable; var TabName: string;
  var RecordCount: Longint; Mappings: TStrings; var Mode: TBatchMode): Boolean;

implementation

{$R *.DFM}

function GetImportParams(const DstTable: TTable; var TabName: string;
  var RecordCount: Longint; Mappings: TStrings; var Mode: TBatchMode): Boolean;
var
  I: Integer;
begin
  with TSrcTableDlg.Create(Application) do begin
    try
      Caption := Format(Caption, [DstTable.TableName]);
      FDstTable := DstTable;
      Result := ShowModal = mrOk;
      if Result then begin
        TabName := SrcNameEdit.Text;
        RecordCount := 0;
        if FirstRecsBtn.Checked then
          RecordCount := RecordCntEdit.AsInteger;
        if Mappings <> nil then begin
          Mappings.Clear;
          for I := 1 to MapGrid.RowCount - 1 do begin
            if (MapGrid.Cells[0, I] <> EmptyStr) and
              (MapGrid.Cells[1, I] <> EmptyStr) then
            begin
              Mappings.Add(Format('%s=%s', [MapGrid.Cells[1, I],
                MapGrid.Cells[0, I]]));
            end;
          end;
        end;
        Mode := TBatchMode(ModeCombo.ItemIndex);
      end;
    finally
      Free;
    end;
  end;
end;

const
  SCollapsed = '&Mappings >>';
  SExpanded = '<< &Mappings';
  SSrcFieldName = 'Source';
  SDstFieldName = 'Destination';

procedure TSrcTableDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

procedure TSrcTableDlg.FormCreate(Sender: TObject);
begin
  ModeCombo.ItemIndex := 0;
  UpdateFormView;
  MapGrid.Cells[0, 0] := SSrcFieldName;
  MapGrid.Cells[1, 0] := SDstFieldName;
end;

procedure TSrcTableDlg.UpdateFormView;
begin
  DisableAlign;
  try
    if FExpanded then begin
      ClientHeight := Expanded.Height;
      MapBtn.Caption := SExpanded;
    end
    else begin
      ClientHeight := Collapsed.Height;
      MapBtn.Caption := SCollapsed;
      MapGrid.RowCount := 1;
    end;
    MapGrid.Enabled := FExpanded;
    MapGrid.Visible := FExpanded;
    Repaint;
  finally
    EnableAlign;
  end;
end;

procedure TSrcTableDlg.MapBtnClick(Sender: TObject);
begin
  if FExpanded then begin
  end
  else begin
    UpdateMapGrid;
  end;
  FExpanded := not FExpanded;
  UpdateFormView;
end;

procedure TSrcTableDlg.SrcNameEditChange(Sender: TObject);
begin
  OkBtn.Enabled := SrcNameEdit.Text <> EmptyStr;
  MapBtn.Enabled := FExpanded or (SrcNameEdit.Text <> EmptyStr);
end;

procedure TSrcTableDlg.OkBtnClick(Sender: TObject);
begin
  if not FileExists(SrcNameEdit.FileName) then begin
    raise Exception.Create(Format('File %s does not exist',
      [SrcNameEdit.FileName]));
  end;
  ModalResult := mrOk;
end;

procedure TSrcTableDlg.AllRecsBtnClick(Sender: TObject);
begin
  RecordCntEdit.Enabled := FirstRecsBtn.Checked;
  if RecordCntEdit.Enabled then begin
    RecordCntEdit.Color := clWindow;
    RecordCntEdit.ParentFont := True;
    if SrcNameEdit.Text <> '' then ActiveControl := RecordCntEdit
    else ActiveControl := SrcNameEdit;
  end
  else begin
    RecordCntEdit.ParentColor := True;
    RecordCntEdit.Font.Color := RecordCntEdit.Color;
  end;
end;

procedure TSrcTableDlg.UpdateMapGrid;
var
  I: Integer;
begin
  TableFields.Close;
  TableFields.TableName := SrcNameEdit.FileName;
  TableFields.Open;
  MapGrid.RowCount := 2;
  MapGrid.FixedRows := 1;
  MapGrid.Cells[0, 1] := EmptyStr;
  MapGrid.Cells[1, 1] := EmptyStr;
  I := 1;
  while not TableFields.Eof do begin
    MapGrid.Cells[0, I] := TableFields.FieldByName('NAME').AsString;
    if FDstTable.FindField(MapGrid.Cells[0, I]) <> nil then
      MapGrid.Cells[1, I] := MapGrid.Cells[0, I]
    else
      MapGrid.Cells[1, I] := EmptyStr;
    MapGrid.RowCount := I + 1;
    TableFields.Next;
    Inc(I);
  end;
  if MapGrid.RowCount > 1 then begin
    MapGrid.Row := 1;
  end;
  MapGrid.Col := 1;
end;

procedure TSrcTableDlg.MapGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  if Col = 0 then
    MapGrid.Options := MapGrid.Options - [goEditing]
  else
    MapGrid.Options := MapGrid.Options + [goEditing];
end;

end.
