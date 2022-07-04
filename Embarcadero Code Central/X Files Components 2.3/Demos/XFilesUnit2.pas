unit XFilesUnit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, XDBGrids, StdCtrls, ExtCtrls, ComCtrls, Mask,
  XQRGrids;

type
  TXFilesForm2 = class(TForm)
    SaveDialog: TSaveDialog;
    OrdersPanel: TPanel;
      XDBGrid1: TXDBGrid;
        DataSource1: TDataSource;
          Table1: TTable;
      XQRGrid1: TXQRGrid;
      XDBGrid2: TXDBGrid;
        DataSource2: TDataSource;
          Query2: TQuery;
    OperatePanel: TPanel;
      CommentsPanel: TPanel;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Bevel1: TBevel;
        Bevel2: TBevel;
        Bevel3: TBevel;
        Bevel4: TBevel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        CheckBox1: TCheckBox;
        CheckBox2: TCheckBox;
        CheckBox3: TCheckBox;
        CheckBox4: TCheckBox;
      ReportPanel: TPanel;
        ReportLabel: TLabel;
        PreviewButton: TButton;
        ShowReportButton: TButton;
        SaveReportButton: TButton;
        PrintButton: TButton;
        PrintAllButton: TButton;
        AlignLabel: TLabel;
        AlignComboBox: TComboBox;
        PartLabel: TLabel;
        PartMaskEdit: TMaskEdit;
        PartUpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure Table1AfterChange(DataSet: TDataSet);
    procedure XDBGrid1PaintColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TXColumn; Highlight: Boolean; Selections: TSelections;
      var Color: TColor; Font: TFont; var Image: TPersistent);
    procedure XDBGrid1ColumnMoving(Sender: TObject; FromIndex, ToIndex: Integer; var Accept: Boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    {Print & Preview}
    procedure PrintButtonClick(Sender: TObject);
    procedure PrintAllButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
    procedure ShowReportButtonClick(Sender: TObject);
    procedure SaveReportButtonClick(Sender: TObject);
    procedure ReportChange(Sender: TObject);
  end;

var
  XFilesForm2: TXFilesForm2;

implementation

{$R *.DFM}

{This demo ilustrate using MasterGrid property to obtain XDBGrid summarization.}
{Additional, many new properties are defined into XDBGrid1 columns. Check it.  }

procedure TXFilesForm2.FormCreate(Sender: TObject);
begin
  ClientWidth := ClientWidth + (GetSystemMetrics(SM_CXVSCROLL) - 16);
  ClientHeight := ClientHeight + (GetSystemMetrics(SM_CYHSCROLL) - 16);
  AlignComboBox.ItemIndex := Ord(XQRGrid1.ReportAlign);
  PartUpDown.Position := XQRGrid1.ReportPart;
end;

procedure TXFilesForm2.Table1AfterChange(DataSet: TDataSet);
begin
  Query2.DisableControls;
  try
    Query2.Close;
    Query2.Open;
  finally
    Query2.EnableControls;
  end;
end;

procedure TXFilesForm2.XDBGrid1PaintColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TXColumn; Highlight: Boolean;
  Selections: TSelections; var Color: TColor; Font: TFont; var Image: TPersistent);
begin
  if (slCellSelected in Selections) or (slCellFixed in Selections) then Exit;
  if slRowSelected in Selections then
    if Table1.State in dsEditModes then Color := clRed else Color := $00D8D8D8 else
  if slMultiSelected in Selections then Color := clInactiveCaption;
end;

procedure TXFilesForm2.XDBGrid1ColumnMoving(Sender: TObject; FromIndex,
  ToIndex: Integer; var Accept: Boolean);
begin
  Accept := (FromIndex <> 12) and (ToIndex <> 12); {Don't move last column}
end;

procedure TXFilesForm2.CheckBox1Click(Sender: TObject);
begin
{$IFDEF VER100}{Delphi 3}
  XDBGrid1.ColumnByName('TaxRate').WidthBase := Ord(CheckBox1.Checked) * 48;
{$ELSE}
{$IFDEF VER110}{C++Builder 3}
  XDBGrid1.ColumnByName('TaxRate').WidthBase := Ord(CheckBox1.Checked) * 48;
{$ELSE}
  XDBGrid1.ColumnByName('TaxRate').Visible := CheckBox1.Checked;
{$ENDIF VER110}
{$ENDIF VER100}
  XDBGrid1.SetFocus;
end;

procedure TXFilesForm2.CheckBox2Click(Sender: TObject);
begin
  with XDBGrid2 do
    if MasterGrid <> nil then MasterGrid := nil else MasterGrid := XDBGrid1;
  XDBGrid1.SetFocus;
end;

procedure TXFilesForm2.CheckBox3Click(Sender: TObject);
begin
  with XDBGrid1 do StretchMode := not StretchMode;
  XDBGrid1.SetFocus;
end;

procedure TXFilesForm2.CheckBox4Click(Sender: TObject);
begin
  with XDBGrid1 do
    if not (dgRowSelect in Options) then Options := Options + [dgRowSelect]
    else Options := Options - [dgRowSelect] + [dgEditing];
  XDBGrid1.SetFocus;
end;

{Print & Preview}

procedure TXFilesForm2.PrintButtonClick(Sender: TObject);
begin
  XQRGrid1.Print;
end;

procedure TXFilesForm2.PrintAllButtonClick(Sender: TObject);
begin
  XQRGrid1.PrintAll;
end;

procedure TXFilesForm2.PreviewButtonClick(Sender: TObject);
begin
  XQRGrid1.Preview;
end;

procedure TXFilesForm2.ShowReportButtonClick(Sender: TObject);
begin
  XQRGrid1.ShowReport;
end;

procedure TXFilesForm2.SaveReportButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then XQRGrid1.SaveReport(SaveDialog.FileName);
end;

procedure TXFilesForm2.ReportChange(Sender: TObject);
begin
  XQRGrid1.ReportAlign := TXReportAlign(AlignComboBox.ItemIndex);
  XQRGrid1.ReportPart := PartUpDown.Position;
end;

end.
