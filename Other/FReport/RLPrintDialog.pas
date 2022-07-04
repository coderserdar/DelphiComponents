unit RLPrintDialog;

interface

uses
  Classes, SysUtils, 
{$ifndef LINUX}
  Windows, 
{$else}
{$endif}
{$ifdef VCL}
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, 
{$else}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, 
{$endif}
  RLFilters, RLConsts, RLPrinters, RLTypes;

type
  TRLPrintDialogOption = (rpoPrintToFile, rpoPageNums, rpoSelection, rpoWarning, rpoHelp, rpoDisablePrintToFile);
  TRLPrintDialogOptions = set of TRLPrintDialogOption;
  
  TRLPrintRange = (rprAllPages, rprSelection, rprPageNums);

  TRLPrintDialog = class(TForm)
    GroupBoxPrinter: TGroupBox;
    ComboBoxPrinterNames: TComboBox;
    LabelPrinterName: TLabel;
    GroupBoxPages: TGroupBox;
    GroupBoxCopies: TGroupBox;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    EditFromPage: TEdit;
    LabelToPage: TLabel;
    EditToPage: TEdit;
    LabelCopies: TLabel;
    EditCopies: TEdit;
    CheckBoxPrintToFile: TCheckBox;
    ComboBoxFilters: TComboBox;
    LabelFilterName: TLabel;
    LabelOptions: TLabel;
    ComboBoxOptions: TComboBox;
    ButtonPrinterSetup: TButton;
    EditPageSelection: TEdit;
    LabelPageSelectionHint: TLabel;
    LabelFromPage: TLabel;
    LabelOddPages: TLabel;
    ComboBoxOddPages: TComboBox;
    procedure EditFromPageChange(Sender: TObject);
    procedure ComboBoxFiltersChange(Sender: TObject);
    procedure ButtonPrinterSetupClick(Sender: TObject);
    procedure ComboBoxPrinterNamesChange(Sender: TObject);
    procedure EditPageSelectionChange(Sender: TObject);
  private
    { Private declarations }
    FMaxPage: Integer;
    FToPage: Integer;
    FMinPage: Integer;
    FFromPage: Integer;
    FOptions: TRLPrintDialogOptions;
    FPrintRange: TRLPrintRange;
    FPageRanges: string;
    FCopies: Integer;
    FPrintToFile: Boolean;
    FFileName: string;
    FOrientation: TRLPageOrientation;
    //
    procedure SetMaxPage(const Value: Integer);
    //
    procedure LoadEditors;
    procedure SaveEditors;
    procedure LoadPrinterList;
    procedure LoadFilterList;
    procedure Init;
  protected
    { Protected declarations }
    procedure DoCreate; override;
  public
    { Public declarations }
    function Execute: Boolean;
    //
    property Options: TRLPrintDialogOptions read FOptions write FOptions;
    property MaxPage: Integer read FMaxPage write SetMaxPage;
    property MinPage: Integer read FMinPage write FMinPage;
    property FromPage: Integer read FFromPage write FFromPage;
    property ToPage: Integer read FToPage write FToPage;
    property PrintRange: TRLPrintRange read FPrintRange write FPrintRange;
    property PageRanges: string read FPageRanges write FPageRanges;
    property Copies: Integer read FCopies write FCopies;
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile;
    property FileName: string read FFileName write FFileName;
    property Orientation: TRLPageOrientation read FOrientation write FOrientation;
  end;

implementation

//{$R *.dfm}

// UTILS

function IntToEmptyStr(AInt: Integer): string;
begin
  if AInt = 0 then
    Result := ''
  else
    Result := IntToStr(AInt);
end;

function EmptyStrToInt(const AStr: string): Integer;
begin
  Result := StrToIntDef(AStr, 0);
end;

{ TRLPrintDialog }

// OVERRIDE

procedure TRLPrintDialog.DoCreate;
begin
  FMinPage := 1;
  FMaxPage := 9999;
  FFromPage := FMinPage;
  FToPage := FMaxPage;
  FOptions := [rpoPrintToFile, rpoPageNums, rpoSelection, rpoWarning, rpoDisablePrintToFile];
  FPrintRange := rprAllPages;
  FPageRanges := '';
  FCopies := RLPrinter.Copies;
  FPrintToFile := False;
  FFileName := '';
  //
  inherited;
  //
  Init;
end;

// PUBLIC

function TRLPrintDialog.Execute: Boolean;
begin
  LoadPrinterList;
  LoadFilterList;
  LoadEditors;
  Result := (ShowModal = mrOk);
  if Result then
    SaveEditors;
end;

// PRIVATE

procedure TRLPrintDialog.Init;
begin
  AutoScroll := False;
  BorderWidth := 8;
  Caption := 'Imprimir';
  ClientHeight := 291;
  ClientWidth := 561;
  Position := poScreenCenter;
  Scaled := False;
{$ifdef VCL}
  BorderStyle := bsDialog;
{$else}
  BorderStyle := fbsDialog;
{$endif};
  GroupBoxPrinter := TGroupBox.Create(Self);
  with GroupBoxPrinter do
  begin
    Name := 'GroupBoxPrinter';
    Parent := Self;
    Left := 0;
    Top := 0;
    Width := 561;
    Height := 105;
    Caption := 'Impressora';
    TabOrder := 0;
    LabelPrinterName := TLabel.Create(Self);
    with LabelPrinterName do
    begin
      Name := 'LabelPrinterName';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 24;
      Width := 31;
      Height := 13;
      Caption := '&Nome:';
    end;
    LabelFilterName := TLabel.Create(Self);
    with LabelFilterName do
    begin
      Name := 'LabelFilterName';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 48;
      Width := 47;
      Height := 13;
      Caption := 'Usar &filtro:';
    end;
    LabelOptions := TLabel.Create(Self);
    with LabelOptions do
    begin
      Name := 'LabelOptions';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 72;
      Width := 77;
      Height := 13;
      Alignment := taRightJustify;
      Caption := 'Opções do filtro:';
    end;
    ComboBoxPrinterNames := TComboBox.Create(Self);
    with ComboBoxPrinterNames do
    begin
      Name := 'ComboBoxPrinterNames';
      Parent := GroupBoxPrinter;
      Left := 68;
      Top := 20;
      Width := 365;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 0;
      OnChange := ComboBoxPrinterNamesChange;
    end;
    CheckBoxPrintToFile := TCheckBox.Create(Self);
    with CheckBoxPrintToFile do
    begin
      Name := 'CheckBoxPrintToFile';
      Parent := GroupBoxPrinter;
      Left := 440;
      Top := 48;
      Width := 113;
      Height := 17;
      TabStop := False;
      Caption := 'Imprimir em arquivo';
      TabOrder := 4;
    end;
    ComboBoxFilters := TComboBox.Create(Self);
    with ComboBoxFilters do
    begin
      Name := 'ComboBoxFilters';
      Parent := GroupBoxPrinter;
      Left := 68;
      Top := 44;
      Width := 365;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 1;
      OnChange := ComboBoxFiltersChange;
    end;
    ComboBoxOptions := TComboBox.Create(Self);
    with ComboBoxOptions do
    begin
      Name := 'ComboBoxOptions';
      Parent := GroupBoxPrinter;
      Left := 96;
      Top := 68;
      Width := 161;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 2;
    end;
    ButtonPrinterSetup := TButton.Create(Self);
    with ButtonPrinterSetup do
    begin
      Name := 'ButtonPrinterSetup';
      Parent := GroupBoxPrinter;
      Left := 440;
      Top := 20;
      Width := 109;
      Height := 21;
      Caption := 'Propriedades';
      TabOrder := 3;
      TabStop := False;
      OnClick := ButtonPrinterSetupClick;
    end;
  end;
  GroupBoxPages := TGroupBox.Create(Self);
  with GroupBoxPages do
  begin
    Name := 'GroupBoxPages';
    Parent := Self;
    Left := 0;
    Top := 108;
    Width := 261;
    Height := 145;
    Caption := 'Intervalo de páginas';
    TabOrder := 1;
    LabelToPage := TLabel.Create(Self);
    with LabelToPage do
    begin
      Name := 'LabelToPage';
      Parent := GroupBoxPages;
      Left := 172;
      Top := 48;
      Width := 18;
      Height := 13;
      Caption := '&até:';
    end;
    LabelPageSelectionHint := TLabel.Create(Self);
    with LabelPageSelectionHint do
    begin
      Name := 'LabelPageSelectionHint';
      Parent := GroupBoxPages;
      Left := 12;
      Top := 96;
      Width := 237;
      Height := 41;
      AutoSize := False;
      Caption := '';
      WordWrap := True;
    end;
    LabelFromPage := TLabel.Create(Self);
    with LabelFromPage do
    begin
      Name := 'LabelFromPage';
      Parent := GroupBoxPages;
      Left := 96;
      Top := 49;
      Width := 15;
      Height := 13;
      Caption := '&de:';
    end;
    RadioButtonPagesAll := TRadioButton.Create(Self);
    with RadioButtonPagesAll do
    begin
      Name := 'RadioButtonPagesAll';
      Parent := GroupBoxPages;
      Left := 12;
      Top := 24;
      Width := 113;
      Height := 17;
      Caption := '&Todas';
      Checked := True;
      TabOrder := 0;
      TabStop := True;
    end;
    RadioButtonPagesInterval := TRadioButton.Create(Self);
    with RadioButtonPagesInterval do
    begin
      Name := 'RadioButtonPagesInterval';
      Parent := GroupBoxPages;
      Left := 12;
      Top := 48;
      Width := 81;
      Height := 17;
      Caption := 'Intervalo';
      TabOrder := 1;
    end;
    RadioButtonPagesSelect := TRadioButton.Create(Self);
    with RadioButtonPagesSelect do
    begin
      Name := 'RadioButtonPagesSelect';
      Parent := GroupBoxPages;
      Left := 12;
      Top := 72;
      Width := 65;
      Height := 17;
      Caption := 'Seleção';
      TabOrder := 4;
    end;
    EditFromPage := TEdit.Create(Self);
    with EditFromPage do
    begin
      Name := 'EditFromPage';
      Parent := GroupBoxPages;
      Left := 116;
      Top := 44;
      Width := 49;
      Height := 21;
      TabStop := False;
      TabOrder := 2;
      Text := '1';
      OnChange := EditFromPageChange;
    end;
    EditToPage := TEdit.Create(Self);
    with EditToPage do
    begin
      Name := 'EditToPage';
      Parent := GroupBoxPages;
      Left := 196;
      Top := 44;
      Width := 49;
      Height := 21;
      TabStop := False;
      TabOrder := 3;
      Text := IntToStr(MaxPageNo);
      OnChange := EditFromPageChange;
    end;
    EditPageSelection := TEdit.Create(Self);
    with EditPageSelection do
    begin
      Name := 'EditPageSelection';
      Parent := GroupBoxPages;
      Left := 96;
      Top := 68;
      Text := '';
      Width := 149;
      Height := 21;
      TabOrder := 5;
      OnChange := EditPageSelectionChange;
    end;
  end;
  GroupBoxCopies := TGroupBox.Create(Self);
  with GroupBoxCopies do
  begin
    Name := 'GroupBoxCopies';
    Parent := Self;
    Left := 268;
    Top := 108;
    Width := 293;
    Height := 145;
    Caption := 'Cópias';
    TabOrder := 2;
    LabelCopies := TLabel.Create(Self);
    with LabelCopies do
    begin
      Name := 'LabelCopies';
      Parent := GroupBoxCopies;
      Left := 12;
      Top := 24;
      Width := 89;
      Height := 13;
      Caption := 'Número de &cópias:';
    end;
    EditCopies := TEdit.Create(Self);
    with EditCopies do
    begin
      Name := 'EditCopies';
      Parent := GroupBoxCopies;
      Left := 108;
      Top := 20;
      Width := 49;
      Height := 21;
      TabOrder := 0;
      Text := '1';
    end;
    LabelOddPages := TLabel.Create(Self);
    with LabelOddPages do
    begin
      Name := 'LabelOddPages';
      Parent := GroupBoxCopies;
      Left := 28;
      Top := 48;
      Width := 73;
      Height := 13;
      Alignment := taRightJustify;
      Caption := 'Pares/'#237'mpares:';
      FocusControl := ComboBoxOddPages;
    end;
    ComboBoxOddPages := TComboBox.Create(Self);
    with ComboBoxOddPages do
    begin
      Name := 'ComboBoxOddPages';
      Parent := GroupBoxCopies;
      Left := 108;
      Top := 44;
      Width := 161;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 1;
      Items.Text := 'Pares'#13#10#205'mpares'#13#10'Todas';
    end;
  end;
  ButtonOk := TButton.Create(Self);
  with ButtonOk do
  begin
    Name := 'ButtonOk';
    Parent := Self;
    Left := 402;
    Top := 266;
    Width := 75;
    Height := 25;
    Caption := 'OK';
    Default := True;
    ModalResult := 1;
    TabOrder := 3;
  end;
  ButtonCancel := TButton.Create(Self);
  with ButtonCancel do
  begin
    Name := 'ButtonCancel';
    Parent := Self;
    Left := 486;
    Top := 266;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := 'Cancelar';
    ModalResult := 2;
    TabOrder := 4;
  end;
  //
  ActiveControl := ComboBoxPrinterNames;
  LabelPrinterName.FocusControl := ComboBoxPrinterNames;
  LabelFilterName.FocusControl := ComboBoxFilters;
  LabelOptions.FocusControl := ComboBoxOptions;
  LabelToPage.FocusControl := EditToPage;
  LabelFromPage.FocusControl := EditFromPage;
  //
  LabelPageSelectionHint.Caption := LocaleStrings.LS_PageSelectionHint;
  Caption := LocaleStrings.LS_PrintStr;
  GroupBoxPrinter.Caption := ' ' + LocaleStrings.LS_PrinterStr + ' ';
  LabelPrinterName.Caption := LocaleStrings.LS_NameStr + ':';
  LabelFilterName.Caption := LocaleStrings.LS_UseFilterStr + ':';
  CheckBoxPrintToFile.Caption := LocaleStrings.LS_PrintToFileStr;
  GroupBoxPages.Caption := ' ' + LocaleStrings.LS_PageRangeStr + ' ';
  LabelFromPage.Caption := LocaleStrings.LS_RangeFromStr + ':';
  LabelToPage.Caption := LocaleStrings.LS_RangeToStr + ':';
  RadioButtonPagesAll.Caption := LocaleStrings.LS_AllStr;
  RadioButtonPagesInterval.Caption := LocaleStrings.LS_PagesStr;
  RadioButtonPagesSelect.Caption := LocaleStrings.LS_SelectionStr;
  GroupBoxCopies.Caption := ' ' + LocaleStrings.LS_CopiesStr + ' ';
  EditCopies.Text := IntToStr(RLPrinter.Copies);
  LabelCopies.Caption := LocaleStrings.LS_NumberOfCopiesStr + ':';
  ButtonOk.Caption := LocaleStrings.LS_OkStr;
  ButtonCancel.Caption := LocaleStrings.LS_CancelStr;
  LabelOddPages.Caption := LocaleStrings.LS_OddPages + '/' + LocaleStrings.LS_EvenPages + ':';
  ComboBoxOddPages.Items.Text :=
    LocaleStrings.LS_OddPagesOnly + #13#10 +
    LocaleStrings.LS_EvenPagesOnly + #13#10 +
    LocaleStrings.LS_AllOddAndEven;
  case RLPrinter.OddEven of
    odOddPagesOnly: ComboBoxOddPages.ItemIndex := 0;
    odEvenPagesOnly: ComboBoxOddPages.ItemIndex := 1;
  else
    ComboBoxOddPages.ItemIndex := 2;
  end;
  LabelOptions.Visible := False;
  ComboBoxOptions.Visible := False;
end;

procedure TRLPrintDialog.LoadPrinterList;
var
  I, J: Integer;
begin
  ComboBoxPrinterNames.Items.Clear;
  J := 0;
  RLPrinter.Refresh;
  for I := 0 to RLPrinter.Printers.Count - 1 do
  begin
    if RLPrinter.PrinterNames[I] = RLPrinter.PrinterName then
      J := I;
    ComboBoxPrinterNames.Items.Add(RLPrinter.PrinterDisplays[I]);
  end;
  ComboBoxPrinterNames.ItemIndex := J;
  ButtonPrinterSetup.Enabled := ComboBoxPrinterNames.ItemIndex <> -1;
end;

procedure TRLPrintDialog.LoadFilterList;
var
  I, J, P: Integer;
  N: string;
  F: TRLCustomPrintFilter;
begin
  ComboBoxFilters.Items.Clear;
  ComboBoxFilters.Items.AddObject(LocaleStrings.LS_DefaultStr, nil);
  //
  J := 0;
  for I := 0 to ActiveFilters.Count - 1 do
    if TObject(ActiveFilters[I]) is TRLCustomPrintFilter then
    begin
      F := TRLCustomPrintFilter(ActiveFilters[I]);
      N := F.GetDisplayLabel;
      if N <> '' then
      begin
        P := ComboBoxFilters.Items.AddObject(N, F);
        if Assigned(SelectedFilter) and (SelectedFilter = F) then
          J := P;
      end;
    end;
  //
  ComboBoxFilters.ItemIndex := J;
  if ComboBoxFilters.Items.Count <= 1 then
  begin
    ComboBoxFilters.Enabled := False;
    ComboBoxFilters.Color := Self.Color;
  end;
  ComboBoxFiltersChange(ComboBoxFilters);
end;

procedure TRLPrintDialog.LoadEditors;
const
  StateColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  case FPrintRange of
    rprAllPages: RadioButtonPagesAll.Checked := True;
    rprSelection: RadioButtonPagesSelect.Checked := True;
    rprPageNums: RadioButtonPagesInterval.Checked := True;
  end;
  EditFromPage.Text := IntToEmptyStr(FFromPage);
  EditToPage.Text := IntToEmptyStr(FToPage);
  EditPageSelection.Text := FPageRanges;
  EditCopies.Text := IntToEmptyStr(FCopies);
  case RLPrinter.OddEven of
    odOddPagesOnly: ComboBoxOddPages.ItemIndex := 0;
    odEvenPagesOnly: ComboBoxOddPages.ItemIndex := 1;
  else
    ComboBoxOddPages.ItemIndex := 2;
  end;
  CheckBoxPrintToFile.Visible := (rpoPrintToFile in FOptions);
  CheckBoxPrintToFile.Enabled := not (rpoDisablePrintToFile in FOptions);
  CheckBoxPrintToFile.Checked := FPrintToFile;
  RadioButtonPagesInterval.Enabled := (rpoPageNums in FOptions);
  EditFromPage.Enabled := (rpoPageNums in FOptions);
  EditToPage.Enabled := (rpoPageNums in FOptions);
  EditPageSelection.Enabled := (rpoSelection in FOptions);
  EditFromPage.Color := StateColors[EditFromPage.Enabled];
  EditToPage.Color := StateColors[EditToPage.Enabled];
  EditPageSelection.Color := StateColors[EditPageSelection.Enabled];
  RadioButtonPagesSelect.Enabled := (rpoSelection in FOptions);
  if rpoHelp in FOptions then
    BorderIcons := BorderIcons + [biHelp]
  else
    BorderIcons := BorderIcons - [biHelp];
end;

procedure TRLPrintDialog.SaveEditors;
begin
  if RadioButtonPagesSelect.Checked then
    FPrintRange := rprSelection
  else if RadioButtonPagesInterval.Checked then
    FPrintRange := rprPageNums
  else
    FPrintRange := rprAllPages;
  case FPrintRange of
    rprAllPages:
    begin
      FFromPage := FMinPage;
      FToPage := FMaxPage;
    end;
    rprSelection:
    begin
      FFromPage := FMinPage;
      FToPage := FMaxPage;
      FPageRanges := EditPageSelection.Text;
    end;
    rprPageNums:
    begin
      FFromPage := EmptyStrToInt(EditFromPage.Text);
      FToPage := EmptyStrToInt(EditToPage.Text);
    end;
  end;
  FCopies := EmptyStrToInt(EditCopies.Text);
  FPrintToFile := CheckBoxPrintToFile.Checked;
  //
  if ComboBoxPrinterNames.ItemIndex <> -1 then
    RLPrinter.PrinterIndex := ComboBoxPrinterNames.ItemIndex;
  if ComboBoxFilters.ItemIndex <> -1 then
  begin
    SelectedFilter := TRLCustomFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
    if (SelectedFilter <> nil) and (SelectedFilter is TRLCustomPrintFilter) then
      TRLCustomPrintFilter(SelectedFilter).OptionIndex := ComboBoxOptions.ItemIndex; 
  end; 
  RLPrinter.Copies := FCopies;
  case ComboBoxOddPages.ItemIndex of
    0: RLPrinter.OddEven := odOddPagesOnly;
    1: RLPrinter.OddEven := odEvenPagesOnly;
  else
    RLPrinter.OddEven := odAllPages;
  end;
end;

// EVENTS

procedure TRLPrintDialog.EditFromPageChange(Sender: TObject);
begin
  if not RadioButtonPagesInterval.Checked then
    RadioButtonPagesInterval.Checked := True;
end;

procedure TRLPrintDialog.SetMaxPage(const Value: Integer);
begin
  if FToPage = FMaxPage then
    FToPage := Value;
  FMaxPage := Value;
end;

procedure TRLPrintDialog.ComboBoxFiltersChange(Sender: TObject);
var
  P: TRLCustomPrintFilter;
begin
  if ComboBoxFilters.ItemIndex = -1 then
    P := nil
  else
    P := TRLCustomPrintFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
  if (P <> nil) and (P.Options <> nil) then
  begin
    P.SetOrientation(Orientation);
    LabelOptions.Caption := P.OptionsLabel + ':';
    ComboBoxOptions.Items := P.Options;
    ComboBoxOptions.ItemIndex := P.OptionIndex;
    LabelOptions.Show;
    ComboBoxOptions.Show;
  end
  else
  begin
    LabelOptions.Hide;
    ComboBoxOptions.Hide;
  end;
end;

procedure TRLPrintDialog.ButtonPrinterSetupClick(Sender: TObject);
begin
  ButtonPrinterSetup.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    if not RLPrinter.ExecuteSetup then
      ShowMessage(LocaleStrings.LS_PrintDialogError);
  finally
    Screen.Cursor := crDefault;
    ButtonPrinterSetup.Enabled := True;
  end;
end;

procedure TRLPrintDialog.ComboBoxPrinterNamesChange(Sender: TObject);
begin
  RLPrinter.PrinterIndex := ComboBoxPrinterNames.ItemIndex;
  ButtonPrinterSetup.Enabled := RLPrinter.PrinterIndex <> -1;
end;

procedure TRLPrintDialog.EditPageSelectionChange(Sender: TObject);
begin
  RadioButtonPagesSelect.Checked := True;
end;

end.

