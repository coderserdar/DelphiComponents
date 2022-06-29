unit XFilesUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, XDBGrids, StdCtrls, ExtCtrls, ComCtrls, CheckLst, Mask,
  Menus, XQRGrids;

type
  TXFilesForm1 = class(TForm)
    ColorDialog: TColorDialog;
    FontDialog: TFontDialog;
    CustomerDataSource: TDataSource;
      Customer: TQuery;
    CustomerPanel: TPanel;
      CustomerDBGrid: TXDBGrid;
    CommentsPanel: TPanel;
      HeadersPanel: TPanel;
        DBGridOptionsLabel: TLabel;
        XDBGridOptionsLabel: TLabel;
        HintOptionsLabel: TLabel;
        TitlePropertyLabel: TLabel;
        ColumnPropertyLabel: TLabel;
        ColorPropertyLabel: TLabel;
        FontPropertyLabel: TLabel;
      OptionsPanel: TPanel;
        OptionsCheckListBox: TCheckListBox;
        HintOptionsCheckListBox: TCheckListBox;
        ShowHintBevel: TBevel;
        ShowHintCheckBox: TCheckBox;
        StretchModeCheckBox: TCheckBox;
        TitleCaptionEdit: TEdit;
        TitleAlignmentComboBox: TComboBox;
        TitleVAlignmentComboBox: TComboBox;
        TitleButtonCheckBox: TCheckBox;
        TitleEllipsisCheckBox: TCheckBox;
        TitleWordWrapCheckBox: TCheckBox;
        TitleAutoToggleCheckBox: TCheckBox;
        ColumnTextEdit: TEdit;
        ColumnAlignmentComboBox: TComboBox;
        ColumnVAlignmentComboBox: TComboBox;
        ColumnShowEditCheckBox: TCheckBox;
        ColumnEllipsisCheckBox: TCheckBox;
        ColumnWordWrapCheckBox: TCheckBox;
        ColumnToolTipsWidthCheckBox: TCheckBox;
        ActionInfoPanel: TPanel;
        GridColorButton: TButton;
        HeaderColorButton: TButton;
        TitleColorButton: TButton;
        ColumnColorButton: TButton;
        GridFontButton: TButton;
        HeaderFontButton: TButton;
        TitleFontButton: TButton;
        ColumnFontButton: TButton;
        LinesCountLabel: TLabel;
        StretchWidthLabel: TLabel;
        LinesStretchBevel: TBevel;
        HeaderLabel: TLabel;
        HeaderMaskEdit: TMaskEdit;
        HeaderUpDown: TUpDown;
        TitleLabel: TLabel;
        TitleMaskEdit: TMaskEdit;
        TitleUpDown: TUpDown;
        RowLabel: TLabel;
        RowMaskEdit: TMaskEdit;
        RowUpDown: TUpDown;
        MinLabel: TLabel;
        MinMaskEdit: TMaskEdit;
        MinUpDown: TUpDown;
        MaxLabel: TLabel;
        MaxMaskEdit: TMaskEdit;
        MaxUpDown: TUpDown;
    IndicatorImageList: TImageList;
    CheckboxImageList: TImageList;
    TitleImageList: TImageList;
    FillerPopupMenu: TPopupMenu;
      ChangeFixedStyle: TMenuItem;
      ChangeColorStyle: TMenuItem;
      ChangeCheckbox: TMenuItem;
      N1: TMenuItem;
      RestoreFillerMenu: TMenuItem;
    PrintPopupMenu: TPopupMenu;
      Print: TMenuItem;
      PrintAll: TMenuItem;
      Preview: TMenuItem;
      ShowReport: TMenuItem;
      SaveReport: TMenuItem;
      N2: TMenuItem;
      ClearSelection: TMenuItem;
    CustomerQRGrid: TXQRGrid;
    SaveDialog: TSaveDialog;
    ReportPanel: TPanel;
      ReportLabel: TLabel;
      PrintButton: TButton;
      PrintAllButton: TButton;
      PreviewButton: TButton;
      ShowReportButton: TButton;
      SaveReportButton: TButton;
      PrintCurrentRowCheckBox: TCheckBox;
      PrintSelectedRowsCheckBox: TCheckBox;
      AlignLabel: TLabel;
      AlignComboBox: TComboBox;
      PartLabel: TLabel;
      PartMaskEdit: TMaskEdit;
      PartUpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OptionsChange(Sender: TObject);
    procedure ControlsChange(Sender: TObject);
    procedure CustomerDBGridColEnter(Sender: TObject);
    procedure CustomerAfterScroll(DataSet: TDataSet);
    { Additional XDBGrid events }
    procedure CustomerDBGridColumnScroll(Sender: TObject; LeftIndex: Integer);
    procedure CustomerDBGridColumnResize(Sender: TObject);
    procedure CustomerDBGridRowResize(Sender: TObject);
    procedure CustomerDBGridHeaderClick(Column: TXColumn; Index: Integer);
    procedure CustomerDBGridIndicatorClick(Sender: TObject);
    { OrderFields & ToggleMarker }
    procedure CustomerDBGridOrderChanged(Sender: TObject);
    procedure CustomerDBGridTitleClick(Column: TXColumn);
    { FillerPopupMenu & IndicatorImages }
    procedure CustomerDBGridFillerClick(Sender: TObject);
    procedure RestoreFillerMenuClick(Sender: TObject);
    procedure ChangeFixedStyleClick(Sender: TObject);
    procedure ChangeColorStyleClick(Sender: TObject);
    procedure ChangeCheckboxClick(Sender: TObject);
    { CellHints & ToolTips }
    procedure CustomerDBGridCellHint(Sender: TObject; Column: TXColumn;
      Index: Integer; HintCell: THintCell; var HintStr: String);
    { Checkboxes & Indicators }
    procedure UpdateCheckBoxColumn;
    procedure CustomerDBGridCalcImageIndex(Sender: TObject; Column: TXColumn;
      var Index: Integer);
    { SelectedRows & PaintColumnCell }
    procedure CustomerDBGridPaintColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TXColumn; Highlight: Boolean; Selections: TSelections;
      var Color: TColor; Font: TFont; var Image: TPersistent);
    procedure ClearSelectionClick(Sender: TObject);
    { Setup Color property}
    procedure GridColorButtonClick(Sender: TObject);
    procedure HeaderColorButtonClick(Sender: TObject);
    procedure TitleColorButtonClick(Sender: TObject);
    procedure ColumnColorButtonClick(Sender: TObject);
    { Setup Font property}
    procedure GridFontButtonClick(Sender: TObject);
    procedure HeaderFontButtonClick(Sender: TObject);
    procedure TitleFontButtonClick(Sender: TObject);
    procedure ColumnFontButtonClick(Sender: TObject);
    { Print & Preview }
    procedure PrintButtonClick(Sender: TObject);
    procedure PrintAllButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
    procedure ShowReportButtonClick(Sender: TObject);
    procedure SaveReportButtonClick(Sender: TObject);
  private
    FRestore: Boolean;
    FHorizontalLines: Boolean;
    procedure RestoreOptions;
    procedure SetupOptions;
    procedure RestoreProperty;
    procedure SetupProperty;
    procedure ShowInfo(const Info: string);
    procedure ShowHeightInfo;
    procedure PopupPrintMenu;
  end;

var
  XFilesForm1: TXFilesForm1;

implementation

{$R *.DFM}

{ private }

procedure TXFilesForm1.RestoreOptions;
var
  Option: TXDBGridOption;
  HintOption: TXHintOption;
begin
  with CustomerDBGrid do
  begin
    for Option := Low(TXDBGridOption) to High(TXDBGridOption) do
      OptionsCheckListBox.Checked[Ord(Option)] := Option in Options;
    for HintOption := Low(TXHintOption) to High(TXHintOption) do
      HintOptionsCheckListBox.Checked[Ord(HintOption)] := HintOption in HintOptions;
    UpdateCheckBoxColumn;
    ShowHeightInfo;
  end;
end;

procedure TXFilesForm1.SetupOptions;
var
  Option: TXDBGridOption;
  HintOption: TXHintOption;
begin
  with CustomerDBGrid do
  begin
    for Option := Low(TXDBGridOption) to High(TXDBGridOption) do
      if OptionsCheckListBox.Checked[Ord(Option)] then Options := Options + [Option]
      else Options := Options - [Option];
    for HintOption := Low(TXHintOption) to High(TXHintOption) do
      if HintOptionsCheckListBox.Checked[Ord(HintOption)] then HintOptions := HintOptions + [HintOption]
      else HintOptions := HintOptions - [HintOption];
  end;
end;

procedure TXFilesForm1.RestoreProperty;
var
  Column: TXColumn;
begin
  if FRestore then Exit;
  FRestore := True;
  with CustomerDBGrid do
  begin
    Column := Columns[SelectedIndex];
    TitleCaptionEdit.Text := Column.Title.Caption;
    TitleAlignmentComboBox.ItemIndex := Ord(Column.Title.Alignment);
    TitleVAlignmentComboBox.ItemIndex := Ord(Column.Title.VAlignment);
    TitleButtonCheckBox.Checked := Column.Title.Button;
    TitleEllipsisCheckBox.Checked := Column.Title.Ellipsis;
    TitleWordWrapCheckBox.Checked := Column.Title.WordWrap;
    TitleAutoToggleCheckBox.Checked := Column.Title.AutoToggle;
    if Column.Field <> nil then ColumnTextEdit.Text := Column.Field.Text;
    ColumnAlignmentComboBox.ItemIndex := Ord(Column.Alignment);
    ColumnVAlignmentComboBox.ItemIndex := Ord(Column.VAlignment);
    ColumnShowEditCheckBox.Checked := Column.ShowEdit;
    ColumnEllipsisCheckBox.Checked := Column.Ellipsis;
    ColumnWordWrapCheckBox.Checked := Column.WordWrap;
    ColumnToolTipsWidthCheckBox.Checked := Boolean(Column.ToolTipsWidth);
    HeaderUpDown.Position := HeaderLinesCount;
    TitleUpDown.Position := TitleLinesCount;
    RowUpDown.Position := LinesCount;
    MinUpDown.Position := StretchWidthMin;
    MaxUpDown.Position := StretchWidthMax;
    StretchModeCheckBox.Checked := StretchMode;
    ShowHintCheckBox.Checked := ShowHint;
    ShowHeightInfo;
  end;
  with CustomerQRGrid do
  begin
    PrintCurrentRowCheckBox.Checked := PrintCurrentRow;
    PrintSelectedRowsCheckBox.Checked := PrintSelectedRows;
    AlignComboBox.ItemIndex := Ord(ReportAlign);
    PartUpDown.Position := ReportPart;
  end;
  FRestore := False;
end;

procedure TXFilesForm1.SetupProperty;
var
  Column: TXColumn;
begin
  if FRestore then Exit;
  with CustomerDBGrid do
  begin
    Column := Columns[SelectedIndex];
    Column.Title.Caption := TitleCaptionEdit.Text;
    Column.Title.Alignment := TAlignment(TitleAlignmentComboBox.ItemIndex);
    Column.Title.VAlignment := TVAlignment(TitleVAlignmentComboBox.ItemIndex);
    Column.Title.Button := TitleButtonCheckBox.Checked;
    Column.Title.Ellipsis := TitleEllipsisCheckBox.Checked;
    Column.Title.WordWrap := TitleWordWrapCheckBox.Checked;
    Column.Title.AutoToggle := TitleAutoToggleCheckBox.Checked;
    Column.Alignment := TAlignment(ColumnAlignmentComboBox.ItemIndex);
    Column.VAlignment := TVAlignment(ColumnVAlignmentComboBox.ItemIndex);
    Column.ShowEdit := ColumnShowEditCheckBox.Checked;
    Column.Ellipsis := ColumnEllipsisCheckBox.Checked;
    Column.WordWrap := ColumnWordWrapCheckBox.Checked;
    Column.ToolTipsWidth := TToolTipsWidth(ColumnToolTipsWidthCheckBox.Checked);
    HeaderLinesCount := HeaderUpDown.Position;
    TitleLinesCount := TitleUpDown.Position;
    LinesCount := RowUpDown.Position;
    StretchWidthMin := MinUpDown.Position;
    StretchWidthMax := MaxUpDown.Position;
    StretchMode := StretchModeCheckBox.Checked;
    ShowHint := ShowHintCheckBox.Checked;
  end;
  with CustomerQRGrid do
  begin
    PrintCurrentRow := PrintCurrentRowCheckBox.Checked;
    PrintSelectedRows := PrintSelectedRowsCheckBox.Checked;
    ReportAlign := TXReportAlign(AlignComboBox.ItemIndex);
    ReportPart := PartUpDown.Position;
  end;
end;

procedure TXFilesForm1.ShowInfo(const Info: string);
begin
  ActionInfoPanel.Caption := Info;
end;

procedure TXFilesForm1.ShowHeightInfo;
var
  RowCount: Integer;
begin
  with CustomerDBGrid do
  begin
    RowCount := (ClientHeight-TitleHeight+RowLineWidth) div RowHeight;
    if RowCount < 1 then RowCount := Ord(ClientHeight > TitleHeight);
    ShowInfo(Format('TitleHeight=%d RowHeight=%d Count=%d', [TitleHeight, RowHeight, RowCount]));
  end;
end;

{ published }

procedure TXFilesForm1.FormCreate(Sender: TObject);
begin
  if Screen.Width >= 830 then Width := 830 else Width := 636;
  ClientWidth := ClientWidth + (GetSystemMetrics(SM_CXVSCROLL) - 16);
  ClientHeight := ClientHeight + (GetSystemMetrics(SM_CYHSCROLL) - 16);
  RestoreOptions;
  RestoreProperty;
{$IFNDEF VER100}{Delphi 3}
{$IFNDEF VER110}{C++Builder 3}
  FillerPopupMenu.Images := IndicatorImageList;
  RestoreFillerMenu.ImageIndex := 5;
  ChangeFixedStyle.ImageIndex := 6;
  ChangeColorStyle.ImageIndex := 7;
  ChangeCheckbox.ImageIndex := 8;
{$ENDIF VER110}
{$ENDIF VER100}
end;

procedure TXFilesForm1.FormResize(Sender: TObject);
begin
  ShowHeightInfo;
end;

procedure TXFilesForm1.OptionsChange(Sender: TObject);
begin
  SetupOptions;
  RestoreOptions;
end;

procedure TXFilesForm1.ControlsChange(Sender: TObject);
begin
  SetupProperty;
  RestoreProperty;
end;

procedure TXFilesForm1.CustomerDBGridColEnter(Sender: TObject);
begin
  RestoreProperty;
end;

procedure TXFilesForm1.CustomerAfterScroll(DataSet: TDataSet);
begin
  RestoreProperty;
end;

{ Additional XDBGrid events }

procedure TXFilesForm1.CustomerDBGridColumnScroll(Sender: TObject; LeftIndex: Integer);
begin
  ShowInfo(Format('ColumnScroll: %s', [TXDBGrid(Sender).Columns[LeftIndex].Title.Caption]));
end;

procedure TXFilesForm1.CustomerDBGridColumnResize(Sender: TObject);
begin
  ShowInfo('ColumnResize');
end;

procedure TXFilesForm1.CustomerDBGridRowResize(Sender: TObject);
begin
  RestoreProperty;
  ShowInfo(Format('RowResize: RowHeight=%d', [TXDBGrid(Sender).RowHeight]));
end;

procedure TXFilesForm1.CustomerDBGridHeaderClick(Column: TXColumn; Index: Integer);
begin
  ShowInfo(Format('HeaderClick: %s->%s', [Column.Title.HeaderRows[Index], Column.Title.Caption]));
end;

procedure TXFilesForm1.CustomerDBGridIndicatorClick(Sender: TObject);
begin
  ShowInfo('IndicatorClick');
end;

{ OrderFields & ToggleMarker }

procedure TXFilesForm1.CustomerDBGridOrderChanged(Sender: TObject);
var
  CurrentID: string;
begin
  {Here you should change order setting into DataSet connected to XDBGrid.     }

  Customer.DisableControls;
  try
    CurrentID := Customer.FieldByName('CustNo').AsString;
    Customer.Close;
    TXDBGrid(Sender).SelectedRows.Clear; { Clear selection - controls disabled }
    ModifyOrderFields(Customer.SQL, TXDBGrid(Sender).OrderFields);
    Customer.Open;
    Customer.Locate('CustNo', CurrentID, []);
  finally
    Customer.EnableControls;
  end;

  {Important notice for developers that using csDefault style of XDBGrid !!!   }
  {When Columns.State is csDefault you must restore XDBGrid columns layout     }
  {after DataSet is open. Layout should be restored when controls are enabled. }
  {You should restore all additional properties for columns and OrderFields.   }
  {Better way: change Columns.State to csCustomized after first DataSet opening}

  if TXDBGrid(Sender).Columns.State = csDefault then
    TXDBGrid(Sender).SetupOrderFields(ExtractOrderFields(Customer.SQL));

  {ModifyOrderFields and ExtractOrderFields are simple SQL parsers.            }
  {They're working correct, when 'ORDER BY' clause is contained in single line }
  {or 'ORDER BY' is last clause in a line.                                     }
  {Use SetupOrderFields for changing order column without OnOrderChanged event.}

  {Important notice for developers that using ADODataSet !!!                   }
  {You may use code showing below to easy change order setting:                }
  {  ADODataSet.Sort := TXDBGrid(Sender).OrderFields;                          }

  {Important notice for developers that using ClientDataSet or ADODataSet !!!  }
  {Include option dgMarkerAscendOnly to eliminate descending column order.     }
  {Than you may use code showing below to easy change order setting:           }
  {  IndexFieldNames := CommaToSemicolon(TXDBGrid(Sender).OrderFields);        }

  {Important notice for developers that using TTable, etc. (single field index)}
  {Setup property Button := False for all columns contained nonindexed fields. }
  {Include option dgMarkerAscendOnly to eliminate descending column order.     }
  {Include option dgMarkerAutoSwitch to prevent marker setting for few fields. }
  {Than you may use code showing below to easy change order setting:           }
  {  IndexFieldNames := TXDBGrid(Sender).OrderFields; (*for single field only*)}

  {For other ways, you should exclude option dgMarkerAutoToggle and handle     }
  {marker setting into OnTitleClick event. In this event you may switch then   }
  {IndexName property due to OrderFields.                                      }

  ShowInfo(Format('OrderChanged: %s', [TXDBGrid(Sender).OrderFields]));
end;

procedure TXFilesForm1.CustomerDBGridTitleClick(Column: TXColumn);
begin
  {OrderFields info is visible before or after TitleClick info depending on    }
  {dgMarkerAutoToggle. Uncomment below statement to see difference.            }
  (*ShowInfo(Format('TitleClick: %s', [Column.Title.Caption]));*)

  {Important notice:                                                           }
  {When option dgMarkerAutoToggle is included OnOrderChanged event is fired by }
  {XDBGrid for Column.Title.AutoToggle=True. You may exclude dgMarkerAutoToggle}
  {option, if you would self handle ToggleMarker in this event.                }
  {When option dgTitleButtons is included this event (and OnOrderChanged too)  }
  {is fired only for: Column.Title.Button=True (Button = True is now default)  }

  if Column.FieldName = '' then PopupPrintMenu {Column of Checkboxes} else
  with TXDBGrid(Column.Grid) do
  if not (dgMarkerAutoToggle in Options) then
    if Column.Title.AutoToggle then Column.Title.ToggleMarker(not (ssCtrl in LastShiftState))
    else {Read: Column.Field can't be included to OrderFields}
  else {Above statements was performed by XDBGrid};
  {To include another column into OrderFields you may use Ctrl+Click (default).}

  {You may exclude dgAutoMarkerToggle and setup markers manualy by example:    }
  {  with TXDBGrid(Column.Grid) do                                             }
  {  begin                                                                     }
  {    BeginOrderUpdate;                                                       }
  {    try                                                                     }
  {      {* your code goes here *)                                             }
  {    finally                                                                 }
  {      EndOrderUpdate; (* OnOrderChanged event will be fired once only *)    }
  {    end;                                                                    }
  {  end;                                                                      }
  {Now, you may handle custom marker settings into OnOrderChanged event.       }
end;

{ FillerPopupMenu & IndicatorImages }

procedure TXFilesForm1.CustomerDBGridFillerClick(Sender: TObject);
var
  P: TPoint;
begin
  with CustomerDBGrid do {FillerIndex >= 0 indicate images from TitleImages    }
  case FillerIndex of    {FillerIndex < -1 indicate images from Indicators     }
    -7: begin            {Calculation: IndicatorImageIndex = -FillerIndex - 2  }
          P := ClientToScreen(Point(0, TitleHeight));
          FillerPopupMenu.Popup(P.X, P.Y);
        end;
    -8: ChangeFixedStyleClick(nil);
    -9: ChangeColorStyleClick(nil);
   -10: ChangeCheckboxClick(nil);
  end;
end;

procedure TXFilesForm1.RestoreFillerMenuClick(Sender: TObject);
begin
  CustomerDBGrid.FillerHint := 'Popup FillerMenu';
  CustomerDBGrid.FillerIndex := -7;
end;

procedure TXFilesForm1.ChangeFixedStyleClick(Sender: TObject);
const
  StyleName: array[TFixedStyle] of string = ('fsDefault', 'fsSoft', 'fsNice', 'fsFlat', 'fsFine', 'fsMild');
begin
  with CustomerDBGrid do
  begin
    FixedStyle := TFixedStyle((Ord(FixedStyle)+1) mod (Ord(High(TFixedStyle))+1));
    ShowInfo(Format('FixedStyle=%s', [StyleName[FixedStyle]]));
    FillerHint := 'Change FixedStyle';
    FillerIndex := -8;
  end;
end;

procedure TXFilesForm1.ChangeColorStyleClick(Sender: TObject);
begin
  FHorizontalLines := not FHorizontalLines;{ See CustomerDBGridPaintColumnCell }
  CustomerDBGrid.Invalidate;
  CustomerDBGrid.FillerHint := 'Change ColorLayout';
  CustomerDBGrid.FillerIndex := -9;
end;

procedure TXFilesForm1.ChangeCheckboxClick(Sender: TObject);
begin
  with CustomerDBGrid.Columns[0] do
  if Images <> nil then Images := nil else Images := CheckBoxImageList;
  CustomerDBGrid.FillerHint := 'Change Checkbox';
  CustomerDBGrid.FillerIndex := -10;
end;

{ CellHints & ToolTips }

procedure TXFilesForm1.CustomerDBGridCellHint(Sender: TObject;
  Column: TXColumn; Index: Integer; HintCell: THintCell; var HintStr: String);
begin
  {CellHints & ToolTips are fired only if XDBGrid.ShowHint is True.            }
  {You may catch any CellHint in this event and modify it by setting HintStr.  }
  {You may cancel CellHint by setting HintStr := ''.                           }
  {CellHints are properly handled by Application.OnHint (showing on StatusBar).}
  {EditorHints are fired only when EditorMode is True (click on selected cell).}
  {You may setup common EditorHint: 'Enter value for field '+Column.FieldName. }
  {ToolTips are not treat as CellHints. You may only show ToolTips on screen.  }

  case HintCell of
    hcTitle:  HintStr := 'TitleHint: ' + HintStr;
    hcHeader: HintStr := 'HeaderHint: Row ' + IntToStr(Index) + ': ' + HintStr;
    hcFiller: HintStr := 'FillerHint: ' + HintStr;
    hcEditor: HintStr := 'EditorHint: ' + HintStr;
  end;

  ShowInfo(GetLongHint(HintStr));
end;

{ Checkboxes & Indicators }

procedure TXFilesForm1.UpdateCheckboxColumn;
begin
  {When dgMultiSelect option is not included you may handle selected rows by   }
  {additional column. Add additional column in Column Editor, set CheckBoxes   }
  {property as True and leave empty FieldName. Then you may select row by      }
  {simple click. When dgMultiSelect is included you may use this column to     }
  {selected rows visualization.                                                }

  with CustomerDBGrid do Columns[0].Transparent := (dgMultiSelect in Options);
end;

procedure TXFilesForm1.CustomerDBGridCalcImageIndex(Sender: TObject;
  Column: TXColumn; var Index: Integer);
begin
  { In this example selected rows are marked in another column by checkboxes.  }
  { To eliminate some markers from indicator you may use code located below.   }
  { To expand set of indicator markers you may connect custom IndicatorImages  }
  { list and select custom markers in this event.                              }

  if Column = nil then { Indicator }
  case Index of
    iiMultiDot: Index := iiClear;
    iiMultiArrow: Index := iiArrow;
  end;
end;

{ SelectedRows & PaintColumnCell }

procedure TXFilesForm1.CustomerDBGridPaintColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TXColumn;
  Highlight: Boolean; Selections: TSelections; var Color: TColor;
  Font: TFont; var Image: TPersistent);
begin
  {OnPaintColumnCell is fired for any data cell when DefaultDrawing is True    }
  {Color, Font & Image are default values that will be used, if no changes     }
  {will be made in this events.                                                }

  {Use default values for selected cell and any fixed cell.                    }
  if (slCellSelected in Selections) or (slCellFixed in Selections) then Exit;

  {Use special color values for other cell in current row and all selected rows}
  if slRowSelected in Selections then
    if slMultiSelected in Selections then Color := $00FF8000 else Color := $00FFFF80 else
  if slMultiSelected in Selections then Color := $00C08000
  {Use separete colors for even and odd rows                                   }
  else if FHorizontalLines then
    if Odd(Customer.RecNo) then Color := $0080FF00 else Color := $0080FF80;
end;

procedure TXFilesForm1.ClearSelectionClick(Sender: TObject);
begin
  CustomerDBGrid.SelectedRows.Clear;
end;

{ Setup Color property}

procedure TXFilesForm1.GridColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := CustomerDBGrid.Color;
  if ColorDialog.Execute then CustomerDBGrid.Color := ColorDialog.Color;
end;

procedure TXFilesForm1.HeaderColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := CustomerDBGrid.HeaderColor;
  if ColorDialog.Execute then CustomerDBGrid.HeaderColor := ColorDialog.Color;
end;

procedure TXFilesForm1.TitleColorButtonClick(Sender: TObject);
var
  Column: TXColumn;
begin
  with CustomerDBGrid do Column := Columns[SelectedIndex];
  ColorDialog.Color := Column.Title.Color;
  if ColorDialog.Execute then Column.Title.Color := ColorDialog.Color;
end;

procedure TXFilesForm1.ColumnColorButtonClick(Sender: TObject);
var
  Column: TXColumn;
begin
  with CustomerDBGrid do Column := Columns[SelectedIndex];
  ColorDialog.Color := Column.Color;
  if ColorDialog.Execute then Column.Color := ColorDialog.Color;
end;

{ Setup Font property}

procedure TXFilesForm1.GridFontButtonClick(Sender: TObject);
begin
  FontDialog.Font := TPanel(CustomerDBGrid.Parent).Font;
  if FontDialog.Execute then
  begin
    TPanel(CustomerDBGrid.Parent).Font := FontDialog.Font;
    CustomerDBGrid.ParentFont := True;
  end;
end;

procedure TXFilesForm1.HeaderFontButtonClick(Sender: TObject);
begin
  FontDialog.Font := CustomerDBGrid.HeaderFont;
  if FontDialog.Execute then CustomerDBGrid.HeaderFont := FontDialog.Font;
end;

procedure TXFilesForm1.TitleFontButtonClick(Sender: TObject);
var
  Column: TXColumn;
begin
  with CustomerDBGrid do Column := Columns[SelectedIndex];
  FontDialog.Font := Column.Title.Font;
  if FontDialog.Execute then Column.Title.Font := FontDialog.Font;
end;

procedure TXFilesForm1.ColumnFontButtonClick(Sender: TObject);
var
  Column: TXColumn;
begin
  with CustomerDBGrid do Column := Columns[SelectedIndex];
  FontDialog.Font := Column.Font;
  if FontDialog.Execute then Column.Font := FontDialog.Font;
end;

{ Print & Preview }

procedure TXFilesForm1.PopupPrintMenu;
var
  P: TPoint;
begin
  with CustomerDBGrid do
  P := ClientToScreen(Point(IndicatorWidth + ColLineWidth, TitleHeight));
  PrintPopupMenu.Popup(P.X, P.Y);
end;

procedure TXFilesForm1.PrintButtonClick(Sender: TObject);
begin
  CustomerQRGrid.Print;
end;

procedure TXFilesForm1.PrintAllButtonClick(Sender: TObject);
begin
  CustomerQRGrid.PrintAll;
end;

procedure TXFilesForm1.PreviewButtonClick(Sender: TObject);
begin
  CustomerQRGrid.Preview;
end;

procedure TXFilesForm1.ShowReportButtonClick(Sender: TObject);
begin
  CustomerQRGrid.ShowReport;
end;

procedure TXFilesForm1.SaveReportButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then CustomerQRGrid.SaveReport(SaveDialog.FileName);
end;

end.
