unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, DateUtils,
  StdCtrls, ComCtrls, Controls, ExtCtrls, Spin, Forms, Dialogs,
  DB, DBmem, xDataset, memxDataset,
  Grids, DBGrids, gmDBGridTreeView,
  gmAdvEditControls, AdvEditCtrls_RegDeclar,
  XPMan;

type
  TDBGridTreeViewDemo_MainForm = class(TForm)
    dsPopulation: TDataSource;
    XPManifest1: TXPManifest;
    mdsCountries: TMemoryDataset;
    mdsCountriesID: TSmallintField;
    mdsCountriesName: TStringField;
    mdsFirstNames: TMemoryDataset;
    mdsLastNames: TMemoryDataset;
    mdsFirstNamesID: TSmallintField;
    mdsFirstNamesName: TStringField;
    mdsFirstNamesSex: TStringField;
    mdsLastNamesID: TIntegerField;
    mdsLastNamesName: TStringField;
    mdsPopulation: TMemoryDataset;
    AutoIncField1: TAutoIncField;
    SmallintField2: TSmallintField;
    IntegerField1: TIntegerField;
    IntegerField3: TIntegerField;
    IntegerField5: TIntegerField;
    StringField3: TStringField;
    DateField2: TDateField;
    IntegerField6: TIntegerField;
    DateField3: TDateField;
    mdsPopulationResidencePlace_ID: TIntegerField;
    dsTowns: TDataSource;
    dsFrstNames: TDataSource;
    dsLastNames: TDataSource;
    tdsPopulation: TTreeDataset;
    AutoIncField3: TAutoIncField;
    StringField1: TStringField;
    SmallintField3: TSmallintField;
    IntegerField7: TIntegerField;
    DateField4: TDateField;
    IntegerField8: TIntegerField;
    IntegerField9: TIntegerField;
    IntegerField10: TIntegerField;
    IntegerField11: TIntegerField;
    DateField5: TDateField;
    StringField2: TStringField;
    StringField4: TStringField;
    StringField5: TStringField;
    StringField6: TStringField;
    StringField7: TStringField;
    dbgtvPopulation: TDBGridTreeView;
    mdsTowns: TMemoryDataset;
    IntegerField12: TIntegerField;
    StringField9: TStringField;
    SmallintField4: TSmallintField;
    StringField10: TStringField;
    mdsPopulationLast_and_first_name: TStringField;
    tdsPopulationFather: TStringField;
    Panel1: TPanel;
    Bevel2: TBevel;
    Bevel4: TBevel;
    Label35: TLabel;
    Label36: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Bevel5: TBevel;
    Label41: TLabel;
    Label42: TLabel;
    Label45: TLabel;
    Label48: TLabel;
    Label44: TLabel;
    Label49: TLabel;
    LBirthDateRangeSpanSign: TLabel;
    Label52: TLabel;
    LDeathDateRangeSpanSign: TLabel;
    LFilteredRecordHandling: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    acbGridColor: TAdvColorBox;
    acbButtonStyle: TAdvComboBox;
    acbLineStyle: TAdvComboBox;
    acbTreeColumnAlign: TAdvComboBox;
    rbParent_as_Mother_View: TRadioButton;
    rbParent_as_Father_View: TRadioButton;
    acbSex: TAdvComboBox;
    adblcbFirstName: TAdvDBLookupComboBox;
    adblcbLastName: TAdvDBLookupComboBox;
    adteBirthDateTo: TAdvDateTimeEdit;
    adteBirthDateFrom: TAdvDateTimeEdit;
    adblcbBirthPlace: TAdvDBLookupComboBox;
    adblcbResidencePlace: TAdvDBLookupComboBox;
    adteDeathDateTo: TAdvDateTimeEdit;
    adteDeathDateFrom: TAdvDateTimeEdit;
    chbDeathDateRange: TCheckBox;
    acbFilteredRecordHandling: TAdvComboBox;
    chbBirthDateRange: TCheckBox;
    bApplyFilter: TButton;
    bCancelFilter: TButton;
    bLocateNode: TButton;    
    chbSex: TCheckBox;
    chbFirstName: TCheckBox;
    chbLastName: TCheckBox;
    chbBirthPlace: TCheckBox;
    chbResidencePlace: TCheckBox;
    bFullExpand: TButton;
    bFullCollapse: TButton;
    Label1: TLabel;
    cb_dgAlwaysShowEditor: TCheckBox;
    cb_dgIndicator: TCheckBox;
    cb_dgTitles: TCheckBox;
    cb_dgAlwaysShowSelection: TCheckBox;
    dg_dgColumnResize: TCheckBox;
    cb_dgColLines: TCheckBox;
    cb_dgRowLines: TCheckBox;
    cb_dgRowSelect: TCheckBox;
    cb_dgMultiSelect: TCheckBox;
    Label2: TLabel;
    acbFixedColor: TAdvColorBox;
    acbFilter_or_Locate: TAdvComboBox;
    LBirthDateRange: TLabel;
    LDeathDateRange: TLabel;
    chbID: TCheckBox;
    lID: TLabel;
    aneID: TAdvNumericEdit;
    procedure FormCreate(Sender: TObject);
    procedure cb_dgIndicatorClick(Sender: TObject);
    procedure cb_dgTitlesClick(Sender: TObject);
    procedure cb_dgAlwaysShowEditorClick(Sender: TObject);
    procedure cb_dgAlwaysShowSelectionClick(Sender: TObject);
    procedure dg_dgColumnResizeClick(Sender: TObject);
    procedure cb_dgColLinesClick(Sender: TObject);
    procedure cb_dgRowLinesClick(Sender: TObject);
    procedure cb_dgRowSelectClick(Sender: TObject);
    procedure cb_dgMultiSelectClick(Sender: TObject);
    procedure acbFixedColorChange(Sender: TObject);
    procedure dbgtvPopulationIndicatorShowingChanged(Sender: TObject);
    procedure acbGridColorChange(Sender: TObject);
    procedure mdsTownsCalcFields(DataSet: TDataSet);
    procedure dbgtvPopulationColExit(Sender: TObject);
    procedure rbParent_as_Mother_ViewClick(Sender: TObject);
    procedure rbParent_as_Father_ViewClick(Sender: TObject);
    procedure mdsPopulationCalcFields(DataSet: TDataSet);
    procedure tdsPopulationBeforeDelete(DataSet: TDataSet);
    procedure tdsPopulationBeforePost(DataSet: TDataSet);
    procedure acbButtonStyleItemIndexChanged(Sender: TObject);
    procedure acbLineStyleItemIndexChanged(Sender: TObject);
    procedure acbTreeColumnAlignItemIndexChanged(Sender: TObject);
    procedure acbFilteredRecordHandlingItemIndexChanged(Sender: TObject);
    procedure dbgtvPopulationPopupListDropDown(Column: TColumn; var Accept: Boolean);
    procedure dbgtvPopulationPopupListCloseUp(Column: TColumn; var Accept: Boolean);
    procedure bFullExpandClick(Sender: TObject);
    procedure bFullCollapseClick(Sender: TObject);
    procedure bApplyFilterClick(Sender: TObject);
    procedure bCancelFilterClick(Sender: TObject);
    procedure bLocateNodeClick(Sender: TObject);
    procedure acbFilter_or_LocateItemIndexChanged(Sender: TObject);
    procedure chbIDClick(Sender: TObject);
    procedure chbOtherFieldsClick(Sender: TObject);
  private
    { Private declarations }
    PopulationCount: Integer;
    PopulationID: Integer;
    procedure Change_dgOption(StdGridOption: TDBGridOption);
    procedure SetParentView(FatherView: Boolean);
    procedure Prepare_mdsPopulation_ForParentLookup(srcPopulation: TDataset;
                                                    ForMother: Boolean);
  public
    { Public declarations }
  end;

var
  DBGridTreeViewDemo_MainForm: TDBGridTreeViewDemo_MainForm;

implementation

{$R *.dfm}

type
//  THackDSet = class(TDataset);
//  THack_xDSet = class(TxDataset);
//  THackFLst = class(TFieldList);
  THackAGrid = class(TCustomDBGrid);

procedure TDBGridTreeViewDemo_MainForm.FormCreate(Sender: TObject);

  procedure GetInitialData;
  var
    ib,jb: Byte;
    tf: TextFile;
    s, nc: string;
    Country: string;
    CountriesCount: Byte;
    TownsCount: Word;
    FirstNamesCount: Byte;
    LastNamesCount: Word;

    function Digits: string;
    begin
      Result := '';
      jb := 0;
      while jb <= ib do
      begin
        Result := Result + IntToStr(jb);
        Inc(jb);
      end;
    end;

  begin
    s := 'Countries_Towns.txt';
    if FileExists(s) then
    begin
      AssignFile(tf, s);
      Reset(tf);
      try
        mdsCountries.Open;
        mdsTowns.Open;
        mdsTowns.DisableCalcFields;
        CountriesCount := 0;
        TownsCount := 0;
        Country := '';
        while not Eof(tf) do
        begin
          ReadLn(tf, s);
          ib := Pos(#9, s);
          nc := Copy(s, 1, ib - 1);
          if nc <> Country then
          begin
            Inc(CountriesCount);
            with mdsCountries do
            begin
              Append;
              Fields[0].AsInteger := CountriesCount;
              Fields[1].AsString := nc;
              Post;
            end;
            Country := nc;
          end;
          Inc(ib);
          jb := ib;
          while (ib <= Length(s))
                 and (s[ib] <> #9)
          do Inc(ib);
          Inc(TownsCount);
          with mdsTowns do
          begin
            Append;
            Fields[0].AsInteger := TownsCount;
            Fields[1].AsString := Copy(s, jb, ib - jb);
            Fields[2].AsInteger := CountriesCount;
            Post;
          end;

        end;
      finally
        CloseFile(tf);
        mdsTowns.EnableCalcFields;
      end;
//      mdsCountries.SaveToTextFile('Countries.txt', defInsStmtWithoutFieldList);
//      mdsTowns.SaveToTextFile('Townes.txt', defInsStmtWithoutFieldList);

      mdsTowns.IndexFieldNames := 'Name';
    end;

    s := 'GivenNames.txt';
    if FileExists(s) then
    begin
      AssignFile(tf, s);
      Reset(tf);
      try
        mdsFirstNames.Open;
        FirstNamesCount := 0;
        while not Eof(tf) do
        begin
          Inc(FirstNamesCount);
          ReadLn(tf, s);
          ib := Pos(' ', s);
          with mdsFirstNames do
          begin
            Append;
            Fields[0].AsInteger := FirstNamesCount;
            Fields[1].AsString := Copy(s, 1, ib - 1);
            Fields[2].AsString := s[ib+1];
            Post;
          end;
        end;
      finally
        CloseFile(tf);
      end;
//      mdsFirstNames.SaveToTextFile('FirstNames.txt', defInsStmtWithoutFieldList);
      FirstNamesCount := FirstNamesCount div 2;
      mdsFirstNames.IndexFieldNames := 'Name';
    end;

    s := 'Surnames.txt';
    if FileExists(s) then
    begin
      AssignFile(tf, s);
      Reset(tf);
      try
        mdsLastNames.Open;
        LastNamesCount := 0;
        while not Eof(tf) do
        begin
          ReadLn(tf, s);
          with mdsLastNames do
          for ib:=0 to 9 do
          begin
            Inc(LastNamesCount);          
            Append;
            Fields[0].AsInteger := LastNamesCount;
            Fields[1].AsString := s + Digits;
            Post;
          end;
        end;
      finally
        CloseFile(tf);
      end;
//      mdsLastNames.SaveToTextFile('LastNames.txt', defInsStmtWithoutFieldList);
      mdsLastNames.IndexFieldNames := 'Name';
    end;

  end;

begin

  with dbgtvPopulation do
  begin
    Top := Panel1.Top + Panel1.Height + 1;

//    AdvDBGridDemo_MainForm.Height := dbgtvPopulation.BoundsRect.Bottom + 60;
//    AdvDBGridDemo_MainForm.Width := PageControl1.Width + 10;
    DBGridTreeViewDemo_MainForm.ClientHeight := Panel1.Height + dbgtvPopulation.Height + 10;
    DBGridTreeViewDemo_MainForm.ClientWidth := Panel1.Width + 5;

    Align := alBottom;
    Anchors := [akTop, akLeft, akRight, akBottom];

// standard TDBGridOptions
    cb_dgIndicator.Checked := dgIndicator in Options;
    cb_dgTitles.Checked := dgTitles in Options;
    cb_dgAlwaysShowEditor.Checked := dgAlwaysShowEditor in Options;
    cb_dgAlwaysShowSelection.Checked := dgAlwaysShowSelection in Options;
    dg_dgColumnResize.Checked := dgColumnResize in Options;
    cb_dgColLines.Checked := dgColLines in Options;
    cb_dgRowLines.Checked := dgRowLines in Options;
    cb_dgRowSelect.Checked := dgRowSelect in Options;
    cb_dgMultiSelect.Checked := dgMultiSelect in Options;
//  TDBGridOptionsEX

    acbFixedColor.SelectedColor := FixedColor;
    acbGridColor.SelectedColor := Color;

    with TreeViewParams do
    begin
      acbButtonStyle.ItemIndex := Ord(ButtonStyle);
      acbLineStyle.ItemIndex := Ord(LineStyle);
      acbTreeColumnAlign.ItemIndex := Ord(TreeColumnAlign);
    end;
  end;
  acbFilteredRecordHandling.ItemIndex := Ord(tdsPopulation.TreeParams.FilteredRecordHandling);

  PopulationCount := 10000;
  PopulationID := 10000;

  rbParent_as_Mother_View.Checked := True;

  GetInitialData;
  with mdsPopulation do
  begin
    LoadFromTextFile('GeneratedPopulation.txt');
    DisableCalcFields;
    First;
  end;
  tdsPopulation.Open;
  mdsPopulation.EnableCalcFields;
  mdsPopulation.Open;

end;

procedure TDBGridTreeViewDemo_MainForm.Change_dgOption(StdGridOption: TDBGridOption);
begin
  with dbgtvPopulation do
    if Showing then
      if StdGridOption in Options then
        Options := Options - [StdGridOption]
      else
        Options := Options + [StdGridOption];
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgIndicatorClick(Sender: TObject);
begin
//  Change_dgOption(dgIndicator);
  with dbgtvPopulation do
    if Showing then
      if cb_dgIndicator.Checked then
        Options := Options + [dgIndicator]
      else
        Options := Options - [dgIndicator];
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgTitlesClick(Sender: TObject);
begin
  Change_dgOption(dgTitles);
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgAlwaysShowEditorClick(Sender: TObject);
begin
  Change_dgOption(dgAlwaysShowEditor);
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgAlwaysShowSelectionClick(Sender: TObject);
begin
  Change_dgOption(dgAlwaysShowSelection);
end;

procedure TDBGridTreeViewDemo_MainForm.dg_dgColumnResizeClick(Sender: TObject);
begin
  Change_dgOption(dgColumnResize);
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgColLinesClick(Sender: TObject);
begin
  Change_dgOption(dgColLines);
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgRowLinesClick(Sender: TObject);
begin
  Change_dgOption(dgRowLines);
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgRowSelectClick(Sender: TObject);
begin
  Change_dgOption(dgRowSelect);
  with dbgtvPopulation do
    if Showing then
      if not(dgRowSelect in Options) then
        Options := Options + [dgEditing];
end;

procedure TDBGridTreeViewDemo_MainForm.cb_dgMultiSelectClick(Sender: TObject);
begin
  Change_dgOption(dgMultiSelect);
end;

procedure TDBGridTreeViewDemo_MainForm.acbFixedColorChange(Sender: TObject);
begin
  dbgtvPopulation.FixedColor := acbFixedColor.SelectedColor;
end;

procedure TDBGridTreeViewDemo_MainForm.dbgtvPopulationIndicatorShowingChanged(Sender: TObject);
begin
  cb_dgIndicator.Checked := (dgIndicator in dbgtvPopulation.Options);
end;

procedure TDBGridTreeViewDemo_MainForm.acbGridColorChange(Sender: TObject);
begin
  if Assigned(dbgtvPopulation) then
    dbgtvPopulation.Color := acbGridColor.SelectedColor;
end;

procedure TDBGridTreeViewDemo_MainForm.mdsTownsCalcFields(DataSet: TDataSet);
begin
  with Dataset do
    Fields[3].AsString := Fields[1].AsString + ' ' +
                          mdsCountries.Lookup('ID', Fields[2].AsInteger, 'Name');
end;

procedure TDBGridTreeViewDemo_MainForm.dbgtvPopulationColExit(Sender: TObject);
begin
  with THackAGrid(Sender), Columns[SelectedIndex].Field do
    if (Datalink.Dataset.State in [dsInsert,dsEdit]) and
       (FieldName = 'FirstName') and not IsNull then
      Datalink.Dataset.FieldByName('Sex').AsString := mdsFirstNames.Fields[2].AsString;
end;

procedure TDBGridTreeViewDemo_MainForm.SetParentView(FatherView: Boolean);
  function ColumnByFieldname(FieldName: string): TColumn;
  var
    si: SmallInt;
  begin
    with dbgtvPopulation do
      for si:=0 to Columns.Count-1 do
        if CompareText(Columns[si].FieldName, FieldName) = 0 then
        begin
          Result := Columns[si];
          Exit;
        end;
    Result := nil;
  end;

begin
  ColumnByFieldName('Father').Visible := not FatherView;
  ColumnByFieldName('Mother').Visible := FatherView;;
  if FatherView then
    tdsPopulation.TreeParams.ParentField := 'Father_ID'
  else
    tdsPopulation.TreeParams.ParentField := 'Mother_ID';
  mdsPopulation.IndexFieldNames := tdsPopulation.TreeParams.ParentField;
end;

procedure TDBGridTreeViewDemo_MainForm.rbParent_as_Mother_ViewClick(Sender: TObject);
begin
  SetParentView(False);
end;

procedure TDBGridTreeViewDemo_MainForm.rbParent_as_Father_ViewClick(Sender: TObject);
begin
  SetParentView(True);
end;

procedure TDBGridTreeViewDemo_MainForm.mdsPopulationCalcFields(DataSet: TDataSet);
begin
  with Dataset do
    Fields[10].AsString := mdsLastNames.Lookup('ID', Fields[3].AsInteger, 'Name') + ' '
                           + mdsFirstNames.Lookup('ID', Fields[2].AsInteger, 'Name');
end;

procedure TDBGridTreeViewDemo_MainForm.tdsPopulationBeforeDelete(DataSet: TDataSet);
begin
  try
   if mdsPopulation.Locate('ID', Dataset.Fields[0].AsInteger, []) then
   begin
     mdsPopulation.Delete;
     Dec(PopulationCount);
   end;
  except
// do nothing
  end;
end;

procedure TDBGridTreeViewDemo_MainForm.tdsPopulationBeforePost(DataSet: TDataSet);
var
  ib: Byte;
begin
  mdsPopulation.DisableCalcFields;
  if DataSet.State = dsInsert then
  begin
    Inc(PopulationCount);
    Inc(PopulationID);
    Dataset.Fields[0].AsInteger := PopulationID;
    mdsPopulation.AppendRecord(Dataset.Fields);
  end else
    with mdsPopulation do
      if Locate('ID', Dataset.Fields[0].Value, []) then
      begin
        Edit;
        for ib:=1 to Fields.Count - 3 do
          Fields[ib].Value := Dataset.Fields[ib].Value;
        Post;
      end;
  mdsPopulation.EnableCalcFields;
end;


procedure TDBGridTreeViewDemo_MainForm.acbButtonStyleItemIndexChanged(Sender: TObject);
begin
  if Assigned(dbgtvPopulation) then
  dbgtvPopulation.TreeViewParams.ButtonStyle := TDBGTVButtonStyle(acbButtonStyle.ItemIndex);
end;

procedure TDBGridTreeViewDemo_MainForm.acbLineStyleItemIndexChanged(Sender: TObject);
begin
  if Assigned(dbgtvPopulation) then
  dbgtvPopulation.TreeViewParams.LineStyle := TDBGTVLineStyle(acbLineStyle.ItemIndex);
end;

procedure TDBGridTreeViewDemo_MainForm.acbTreeColumnAlignItemIndexChanged(Sender: TObject);
begin
  if Assigned(dbgtvPopulation) then
  dbgtvPopulation.TreeViewParams.TreeColumnAlign := TBGTVTreeColumnAlign(acbTreeColumnAlign.ItemIndex);
end;

procedure TDBGridTreeViewDemo_MainForm.acbFilteredRecordHandlingItemIndexChanged(Sender: TObject);
begin
  if Assigned(tdsPopulation) then
  tdsPopulation.TreeParams.FilteredRecordHandling := TFilteredRecordHandling(acbFilteredRecordHandling.ItemIndex);
end;

procedure TDBGridTreeViewDemo_MainForm.Prepare_mdsPopulation_ForParentLookup(srcPopulation: TDataset;
                                                                        ForMother: Boolean);
var
  ib: Byte;
  FilterStr: string;
  IndexStr: string;
  vnull: Variant;
  rsValues: array of TVarRec;
  reValues: array of TVarRec;
//  ResPlaceRangeFixed: Boolean;
  FilterForFatherLastName: Boolean;
begin
  FilterForFatherLastName := False;
//  FilterForFatherLastName := not(ForMother or srcPopulation.Fields[3].IsNull);
  SetLength(rsValues, 3 + Byte(FilterForFatherLastName));
  SetLength(reValues, 3 + Byte(FilterForFatherLastName));

  ib := 0;
  FilterStr := '';
  IndexStr := 'Sex';
  if ForMother then
  begin
    AnsiString(rsValues[0].VAnsiString) := 'F';
    AnsiString(reValues[0].VAnsiString) := 'F';
  end else
  begin
    AnsiString(rsValues[0].VAnsiString) := 'M';
    AnsiString(reValues[0].VAnsiString) := 'M';
    if FilterForFatherLastName then
    begin
      Inc(ib);
      with srcPopulation.Fields[3] do
      begin
        IndexStr := IndexStr + ';' + FieldName;
        rsValues[1].VInteger := AsInteger;
        reValues[1].VInteger := AsInteger;
      end;
      rsValues[1].VType := vtInteger;
      reValues[1].VType := vtInteger;
    end;
  end;
  rsValues[0].VType := vtAnsiString;
  reValues[0].VType := vtAnsiString;

  with srcPopulation do
  begin
     // BirthPlace_ID
//    ResPlaceRangeFixed := not Fields[5].IsNull;
//    if ResPlaceRangeFixed then
    if not Fields[5].IsNull then
    begin      // ResidencePlace_ID
      Inc(ib);
      IndexStr := IndexStr + ';' + Fields[8].FieldName;
      rsValues[ib].VInteger := Fields[5].AsInteger;
      reValues[ib].VInteger := Fields[5].AsInteger;
      rsValues[ib].VType := vtInteger;
      reValues[ib].VType := vtInteger;
    end;
         // BirthDate
    if not Fields[4].IsNull then
    begin
      Inc(ib);
      vnull := null;
      IndexStr := IndexStr + ';' + Fields[4].FieldName;
//        rsValues[Byte(ResPlaceRangeFixed) + 1].VInteger := -DateDelta;

      rsValues[ib].VVariant := @vnull;
      if ForMother then
        reValues[ib].VInteger := Trunc(Fields[4].AsDateTime - 19)
//                                 - ApproxDaysPerYear*aneProcreationAgeMinWomen.IntValue)
      else
        reValues[ib].VInteger := Trunc(Fields[4].AsDateTime - 19);
//                                 - ApproxDaysPerYear*aneProcreationAgeMinMen.IntValue);
      rsValues[ib].VType := vtVariant;
      reValues[ib].VType := vtInteger;

                                         // DeathDate
      FilterStr := Fields[9].FieldName + ' is null or ' +
                   Fields[9].FieldName + '>=' + DateToStr(Fields[4].AsDateTime);
    end;
  end;

  with mdsPopulation do
  begin
    IndexFieldNames := IndexStr;
    SetRange(rsValues, reValues);
    if RecordCount > 0 then
    begin
      Filter := FilterStr;
      Filtered := True;
    end;
  end;

  AnsiString(rsValues[0].VAnsiString) := '';
  AnsiString(reValues[0].VAnsiString) := '';
  SetLength(rsValues, 0);
  SetLength(reValues, 0);
end; // tdsPopulation


procedure TDBGridTreeViewDemo_MainForm.dbgtvPopulationPopupListDropDown(Column: TColumn;
                                                                    var Accept: Boolean);
{
  procedure Filter_mdsPopulation(ForMother: Boolean);
  const
    str_and_ = ' and ';
  var
    FilterStr: string;
  begin
    with tdsPopulation do
    begin
      FilterStr := Fields[1].FieldName + '='; // Sex
      if ForMother then
        FilterStr := FilterStr + '''F'''
      else
        FilterStr := FilterStr + '''M''';

           // BirthPlace_ID
      if not Fields[5].IsNull then        // ResidencePlace_ID
        FilterStr := FilterStr + str_and_ + Fields[8].FieldName + '=' + Fields[5].AsString;

      if not Fields[4].IsNull then // BirthDate
      begin
        FilterStr := FilterStr + str_and_ + Fields[4].FieldName + '<=';
        if ForMother then
          FilterStr := FilterStr + DateToStr(Fields[4].AsDateTime - ApproxDaysPerYear*aneProcreationAgeMinWomen.IntValue)
        else
          FilterStr := FilterStr + DateToStr(Fields[4].AsDateTime - ApproxDaysPerYear*aneProcreationAgeMinMen.IntValue);
                                           // DeathDate
        FilterStr := FilterStr + str_and_ + '(' +
                     Fields[9].FieldName + ' is null or ' +
                     Fields[9].FieldName + '>=' + DateToStr(Fields[4].AsDateTime) + ')';
      end;
    end;

    mdsPopulation.Filter := FilterStr;
    mdsPopulation.Filtered := True;
  end;
}
begin
{
  if Column.Field.FieldName = 'Mother' then
    Filter_mdsPopulation(True)
  else if Column.Field.FieldName = 'Father' then
    Filter_mdsPopulation(False)
  else
    begin
    end;
}
  if Column.Field.FieldName = 'Mother' then
    Prepare_mdsPopulation_ForParentLookup(tdsPopulation, True)  // Filter_mdsPopulation(True)
  else if Column.Field.FieldName = 'Father' then
    Prepare_mdsPopulation_ForParentLookup(tdsPopulation, False) // Filter_mdsPopulation(False)
  else
    begin
    end;
end;

procedure TDBGridTreeViewDemo_MainForm.dbgtvPopulationPopupListCloseUp(Column: TColumn;
                                                                       var Accept: Boolean);
begin
  if (Column.Field.FieldName = 'Mother') or
     (Column.Field.FieldName = 'Father') then
    with mdsPopulation do
    begin
      Filtered := False;
      CancelRange;
    end
  else
    begin
    end;
end;

procedure TDBGridTreeViewDemo_MainForm.bFullExpandClick(Sender: TObject);
begin
  tdsPopulation.FullExpand;
end;

procedure TDBGridTreeViewDemo_MainForm.bFullCollapseClick(Sender: TObject);
begin
  tdsPopulation.FullCollapse;
end;

procedure TDBGridTreeViewDemo_MainForm.bApplyFilterClick(Sender: TObject);
const
  str_AND_ = ' and ';
var
  FilterStr: string;
begin
  if chbSex.Checked and (acbSex.ItemIndex >= 0) then
    FilterStr := tdsPopulation.Fields[1].FieldName + '=''' + Copy(acbSex.Text, 1, 1) + ''''
  else
    FilterStr := '';

  if chbFirstName.Checked and (adblcbFirstName.KeyValue <> null) then
  begin
    if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
    FilterStr := FilterStr + tdsPopulation.Fields[2].FieldName +
                 '=' + VarToStr(adblcbFirstName.KeyValue);
  end;

  if chbLastName.Checked and (adblcbLastName.KeyValue <> null) then
  begin
    if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
    FilterStr := FilterStr + tdsPopulation.Fields[3].FieldName +
                 '=' + VarToStr(adblcbLastName.KeyValue);
  end;

  if chbBirthDateRange.Checked then
  begin
    if adteBirthDateFrom.DateTime <> 0 then
    begin
      if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
      FilterStr := FilterStr + tdsPopulation.Fields[4].FieldName +
                    '>=' + DateToStr(adteBirthDateFrom.DateTime);
    end;
    if adteBirthDateTo.DateTime <> 0 then
    begin
      if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
      FilterStr := FilterStr + tdsPopulation.Fields[4].FieldName +
                    '<=' + DateToStr(adteBirthDateTo.DateTime);
    end;
  end;

  if chbDeathDateRange.Checked then
  begin
    if adteDeathDateFrom.DateTime <> 0 then
    begin
      if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
      FilterStr := FilterStr + tdsPopulation.Fields[9].FieldName +
                    '>=' + DateToStr(adteDeathDateFrom.DateTime);
    end;
    if adteDeathDateTo.DateTime <> 0 then
    begin
      if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
      FilterStr := FilterStr + tdsPopulation.Fields[9].FieldName +
                    '<=' + DateToStr(adteDeathDateTo.DateTime);
    end;
  end;

  if chbBirthPlace.Checked and (adblcbBirthPlace.KeyValue <> null) then
  begin
    if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
    FilterStr := FilterStr + tdsPopulation.Fields[5].FieldName +
                 '=' + VarToStr(adblcbBirthPlace.KeyValue);
  end;

  if chbResidencePlace.Checked and (adblcbResidencePlace.KeyValue <> null) then
  begin
    if FilterStr <> '' then FilterStr := FilterStr + str_AND_;
    FilterStr := FilterStr + tdsPopulation.Fields[8].FieldName +
                 '=' + VarToStr(adblcbResidencePlace.KeyValue);
  end;

  tdsPopulation.Filter := FilterStr;
  tdsPopulation.Filtered := True;
  
  if tdsPopulation.Filter <> '' then
    bApplyFilter.Font.Style := [fsBold];  
end;

procedure TDBGridTreeViewDemo_MainForm.bCancelFilterClick(Sender: TObject);
begin
  tdsPopulation.Filtered := False;
end;

procedure TDBGridTreeViewDemo_MainForm.bLocateNodeClick(Sender: TObject);
var
  KeyFieldsCount: Byte;
  KeyFields: string;
  KeyValues: Variant;
begin
  KeyFieldsCount := 0;
  KeyValues := VarArrayCreate([0,6], varVariant);

  if chbID.Checked then
  begin
    KeyFields := tdsPopulation.Fields[0].FieldName + ';';
    KeyValues[KeyFieldsCount] := aneID.IntValue;
    Inc(KeyFieldsCount);
  end;

  if chbSex.Checked then
  begin
    KeyFields := tdsPopulation.Fields[1].FieldName + ';';
    KeyValues[KeyFieldsCount] := Copy(acbSex.Text,1,1);
    Inc(KeyFieldsCount);
  end else
    if not chbID.Checked
    then KeyFields := '';

  if chbFirstName.Checked and (adblcbFirstName.KeyValue <> null) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[2].FieldName + ';';
    KeyValues[KeyFieldsCount] := adblcbFirstName.KeyValue;
    Inc(KeyFieldsCount);
  end;

  if chbLastName.Checked and (adblcbLastName.KeyValue <> null) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[3].FieldName + ';';
    KeyValues[KeyFieldsCount] := adblcbLastName.KeyValue;
    Inc(KeyFieldsCount);
  end;

  if chbBirthDateRange.Checked and
    (adteBirthDateFrom.DateTime <> 0) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[4].FieldName + ';';
    KeyValues[KeyFieldsCount] := adteBirthDateFrom.DateTime;
    Inc(KeyFieldsCount);
  end;

  if chbDeathDateRange.Checked and
    (adteDeathDateFrom.DateTime <> 0) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[9].FieldName + ';';
    KeyValues[KeyFieldsCount] := adteDeathDateFrom.DateTime;
    Inc(KeyFieldsCount);
  end;

  if chbBirthPlace.Checked and (adblcbBirthPlace.KeyValue <> null) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[5].FieldName + ';';
    KeyValues[KeyFieldsCount] := adblcbBirthPlace.KeyValue;
    Inc(KeyFieldsCount);
  end;

  if chbResidencePlace.Checked and (adblcbResidencePlace.KeyValue <> null) then
  begin
    KeyFields := KeyFields + tdsPopulation.Fields[8].FieldName + ';';
    KeyValues[KeyFieldsCount] := adblcbResidencePlace.KeyValue;
    Inc(KeyFieldsCount);
  end;

  if KeyFields <> '' then
  begin
    Delete(KeyFields, Length(KeyFields), 1);
    VarArrayRedim(KeyValues, KeyFieldsCount-1);
    tdsPopulation.Locate(KeyFields, KeyValues, []);
    VarClear(KeyValues);
  end;
end;

procedure TDBGridTreeViewDemo_MainForm.acbFilter_or_LocateItemIndexChanged(Sender: TObject);
begin
  LFilteredRecordHandling.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  acbFilteredRecordHandling.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  LBirthDateRange.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  LBirthDateRangeSpanSign.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  adteBirthDateTo.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  LDeathDateRange.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  LDeathDateRangeSpanSign.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  adteDeathDateTo.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  bApplyFilter.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  bCancelFilter.Visible := (acbFilter_or_Locate.ItemIndex = 0);
  bLocateNode.Visible := (acbFilter_or_Locate.ItemIndex = 1);
  chbID.Visible := (acbFilter_or_Locate.ItemIndex = 1);
  aneID.Visible := (acbFilter_or_Locate.ItemIndex = 1);
  lID.Visible := (acbFilter_or_Locate.ItemIndex = 1);
end;

procedure TDBGridTreeViewDemo_MainForm.chbIDClick(Sender: TObject);
begin
  if chbID.Checked then
  begin
    chbSex.Checked := False;
    chbFirstName.Checked := False;
    chbLastName.Checked := False;
    chbBirthDateRange.Checked := False;
    chbDeathDateRange.Checked := False;
    chbBirthPlace.Checked := False;
    chbResidencePlace.Checked := False;
  end;
end;

procedure TDBGridTreeViewDemo_MainForm.chbOtherFieldsClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked
     and chbID.Checked
  then chbID.Checked := False;
end;


end.

