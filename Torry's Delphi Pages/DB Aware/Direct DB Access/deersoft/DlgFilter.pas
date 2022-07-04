unit DlgFilter;

interface
                   
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, DB, Buttons, DUtils, Spin, DFilters;

type
  TFilterDialog = class(TForm)
    pcFilter: TPageControl;
    panFilter: TPanel;
    cbxFields: TComboBox;
    Label1: TLabel;
    tsString: TTabSheet;
    tsNumeric: TTabSheet;
    tsDate: TTabSheet;
    tsLogical: TTabSheet;
    tsMarker: TTabSheet;
    tsCustom: TTabSheet;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnTrue: TSpeedButton;
    btnFalse: TSpeedButton;
    btnMarked: TSpeedButton;
    btnUnMarked: TSpeedButton;
    ScrollBox1: TScrollBox;
    btnClear1: TSpeedButton;
    cbxField1: TComboBox;
    cbxRel1: TComboBox;
    edValue1: TEdit;
    cbxLog1: TComboBox;
    btnClear2: TSpeedButton;
    cbxField2: TComboBox;
    cbxRel2: TComboBox;
    edValue2: TEdit;
    cbxLog2: TComboBox;
    btnClear3: TSpeedButton;
    cbxField3: TComboBox;
    cbxRel3: TComboBox;
    edValue3: TEdit;
    cbxLog3: TComboBox;
    btnClear4: TSpeedButton;
    cbxField4: TComboBox;
    cbxRel4: TComboBox;
    edValue4: TEdit;
    cbxLog4: TComboBox;
    btnClear5: TSpeedButton;
    cbxField5: TComboBox;
    cbxRel5: TComboBox;
    edValue5: TEdit;
    cbxLog5: TComboBox;
    btnClear6: TSpeedButton;
    cbxField6: TComboBox;
    cbxRel6: TComboBox;
    edValue6: TEdit;
    cbxLog6: TComboBox;
    btnClear7: TSpeedButton;
    cbxField7: TComboBox;
    cbxRel7: TComboBox;
    edValue7: TEdit;
    cbxLog7: TComboBox;
    btnClear8: TSpeedButton;
    cbxField8: TComboBox;
    cbxRel8: TComboBox;
    edValue8: TEdit;
    cbxLog8: TComboBox;
    btnClear9: TSpeedButton;
    cbxField9: TComboBox;
    cbxRel9: TComboBox;
    edValue9: TEdit;
    cbxLog9: TComboBox;
    Panel1: TPanel;
    Label2: TLabel;
    edFrom: TEdit;
    edTill: TEdit;
    chkNegationStr: TCheckBox;
    Panel2: TPanel;
    Label4: TLabel;
    chkNegationNum: TCheckBox;
    Panel3: TPanel;
    Label6: TLabel;
    chkNegationDat: TCheckBox;
    dpFrom: TDateTimePicker;
    dpTill: TDateTimePicker;
    seFrom: TSpinEdit;
    seTill: TSpinEdit;
    chkTillStr: TCheckBox;
    chkTillNum: TCheckBox;
    chkTillDat: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure FieldsChange(Sender: TObject);
    procedure TillStrClick(Sender: TObject);
    procedure TillNumClick(Sender: TObject);
    procedure TillDatClick(Sender: TObject);
    procedure FieldChange(Sender: TObject);
    procedure RelChange(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure LogChange(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure ValueExit(Sender: TObject);
  private
    { Private declarations }
    FField      : TField;
    FDataSet    : TDataSet;
    FFilter     : TDSubFilters;
    FFields     : TStrings;
    FSelected   : String;
    FMarker     : String;
    FUpper      : Boolean;

    function  FindField(const ItemName: String): TField;
    procedure SetFields(Value: TStrings);
    procedure SetFilters(Value: TDSubFilters);
    // Handle Custom Filter
    procedure ClearAllItem;
    procedure SetAllItems;

  public
    { Public declarations }

    function PageToFilterType(Page: TTabSheet): TFilterType;
    function FilterTypeToPage(FilterType: TFilterType): TTabSheet;

    property Field    : TField        read FField    write FField;
    property DataSet  : TDataSet      read FDataSet  write FDataSet;
    property Fields   : TStrings      read FFields   write SetFields;
    property Filters  : TDSubFilters  read FFilter   write SetFilters;
    property Selected : String        read FSelected write FSelected;
    property Marker   : String        read FMarker   write FMarker;
    property UpperCase: Boolean       read FUpper    write FUpper;

  end;

var
  FilterDialog: TFilterDialog;

implementation

{$R *.DFM}


procedure TFilterDialog.FormCreate(Sender: TObject);
begin
     FFields := TStringList.Create;
     FFilter := TDSubFilters.Create(Self);
end;


procedure TFilterDialog.FormDestroy(Sender: TObject);
begin
     FFields.Free;
     FFields := nil;
     FFilter.Free;
end;


procedure TFilterDialog.FormShow(Sender: TObject);
begin
     if Assigned(Field) then
     begin
          if Field.FieldName = FMarker
          then
              pcFilter.ActivePage := FilterTypeToPage(ffMarker)
          else
              pcFilter.ActivePage := FilterTypeToPage(FieldToFilterType(Field));
          cbxFields.ItemIndex := cbxFields.Items.IndexOf(Trim(Field.DisplayLabel));
     end
     else
     begin
          pcFilter.ActivePage := FilterTypeToPage(ffCustom);
          cbxFields.SetFocus;
     end;
     cbxField1.Items.AddStrings(cbxFields.Items);
     cbxField2.Items.AddStrings(cbxFields.Items);
     cbxField3.Items.AddStrings(cbxFields.Items);
     cbxField4.Items.AddStrings(cbxFields.Items);
     cbxField5.Items.AddStrings(cbxFields.Items);
     cbxField6.Items.AddStrings(cbxFields.Items);
     cbxField7.Items.AddStrings(cbxFields.Items);
     cbxField8.Items.AddStrings(cbxFields.Items);
     cbxField9.Items.AddStrings(cbxFields.Items);
     LogicalOpStrings(cbxLog1.Items);
     cbxLog2.Items.AddStrings(cbxLog1.Items);
     cbxLog3.Items.AddStrings(cbxLog1.Items);
     cbxLog4.Items.AddStrings(cbxLog1.Items);
     cbxLog5.Items.AddStrings(cbxLog1.Items);
     cbxLog6.Items.AddStrings(cbxLog1.Items);
     cbxLog7.Items.AddStrings(cbxLog1.Items);
     cbxLog8.Items.AddStrings(cbxLog1.Items);
     cbxLog9.Items.AddStrings(cbxLog1.Items);
end;


procedure TFilterDialog.FilterChange(Sender: TObject);
var
   i    : Integer;
   oFld : TField;
begin
     if pcFilter.ActivePage <> tsCustom then
     begin
          oFld := FindField(cbxFields.Text);
          if Assigned(oFld) then
          begin
               if FieldToFilterType(oFld) <> PageToFilterType(pcFilter.ActivePage) then
               begin
                    pcFilter.ActivePage := FilterTypeToPage(FieldToFilterType(oFld));
                    Exit;
               end;
          end;
     end;
     case PageToFilterType(pcFilter.ActivePage) of
          ffString :
          begin
               if FUpper then
               begin
                    edFrom.CharCase := ecUpperCase;
                    edTill.CharCase := ecUpperCase;
               end;
               edFrom.SetFocus;
          end;
          ffNumeric : seFrom.SetFocus;
          ffDate :
          begin
               dpFrom.Date := Now;
               dpTill.Date := Now;
               dpFrom.SetFocus;
          end;
          ffCustom :
          begin
               if FUpper then
               begin
                    for i := 1 to 9 do
                    begin
                         (FindComponent('edValue' + IntToStr(i)) as TEdit).CharCase := ecUpperCase;
                         with (FindComponent('cbxField' + IntToStr(i)) as TComboBox) do
                         begin
                              Items.Clear;
                              Items.AddStrings(cbxFields.Items);
                         end;
                    end;
               end;
               cbxField1.SetFocus;
          end;
     end;
end;


function TFilterDialog.FindField(const ItemName: String): TField;
var
   iPos : Integer;
begin
     Result := nil;
     iPos := cbxFields.Items.IndexOf(ItemName);
     if (iPos > -1) and (iPos < FFields.Count) then Result := DataSet.FindField(FFields.Strings[iPos]);
end;


procedure TFilterDialog.SetFilters(Value: TDSubFilters);
begin
     FFilter.Assign(Value);
end;


procedure TFilterDialog.TillStrClick(Sender: TObject);
begin
     edTill.Enabled := chkTillStr.Checked;
     edTill.Color   := IIF(edTill.Enabled, clWindow, clInfoBk);
end;


procedure TFilterDialog.TillNumClick(Sender: TObject);
begin
     seTill.Enabled := chkTillNum.Checked;
     seTill.Color   := IIF(seTill.Enabled, clWindow, clInfoBk);
end;


procedure TFilterDialog.TillDatClick(Sender: TObject);
begin
     dpTill.Enabled := chkTillDat.Checked;
     dpTill.Color   := IIF(dpTill.Enabled, clWindow, clInfoBk);
end;


procedure TFilterDialog.SetFields(Value: TStrings);
begin
     FFields.Assign(Value);
end;


procedure TFilterDialog.FieldsChange(Sender: TObject);
begin
     FFilter.Clear;
     FField := FindField(cbxFields.Text);
     if Assigned(FField) then pcFilter.ActivePage := FilterTypeToPage(FieldToFilterType(FField));
end;


function TFilterDialog.PageToFilterType(Page: TTabSheet): TFilterType;
begin
     Result := ffNone;
     if Page = tsString  then Result := ffString;
     if Page = tsNumeric then Result := ffNumeric;
     if Page = tsDate    then Result := ffDate;
     if Page = tsLogical then Result := ffLogical;
     if Page = tsMarker  then Result := ffMarker;
     if Page = tsCustom  then Result := ffCustom;
end;


function TFilterDialog.FilterTypeToPage(FilterType: TFilterType): TTabSheet;
begin
     Result := tsString;
     case FilterType of
          ffString  : Result := tsString;
          ffNumeric : Result := tsNumeric;
          ffDate    : Result := tsDate;
          ffLogical : Result := tsLogical;
          ffMarker  : Result := tsMarker;
          ffCustom  : Result := tsCustom;
     end;
end;


procedure TFilterDialog.OkClick(Sender: TObject);
var
   oItem  : TDSubFilter;
   sLower : String;
   sUpper : String;
   sFld   : String;
   sRel   : String;
   sVal   : String;
   sLog   : String;
   iPos   : Integer;
   i      : Integer;
begin
     case PageToFilterType(pcFilter.ActivePage) of
          ffString  :
          begin
               sLower := edFrom.Text;
               sLower := StrTran(sLower, '*', '%');
               sLower := StrTran(sLower, '?', '_');
               if chkTillStr.Checked then
               begin
                    sUpper := edTill.Text;
                    sUpper := StrTran(sUpper, '*', '%');
                    sUpper := StrTran(sUpper, '?', '_');
               end
               else sUpper := '';
               oItem := FFilter.Add;
               oItem.FieldName  := FField.FieldName;
               oItem.Negation   := chkNegationStr.Checked;
               oItem.Relation   := AnalyseRelation(sLower, sUpper);
               oItem.LowerValue := sLower;
               oItem.UpperValue := sUpper;
               oItem.FilterType := ffString;
               oItem.LogicalOp  := flEnd;
          end;
          ffNumeric :
          begin
               oItem := FFilter.Add;
               oItem.FieldName  := FField.FieldName;
               oItem.Negation   := chkNegationNum.Checked;
               oItem.Relation   := frEqual;
               oItem.LowerValue := IntToStr(seFrom.Value);
               oItem.UpperValue := IntToStr(IIF(chkTillNum.Checked, seTill.Value, seFrom.Value));
               oItem.FilterType := ffNumeric;
               oItem.LogicalOp  := flEnd;
          end;
          ffDate    :
          begin
               oItem := FFilter.Add;
               oItem.FieldName  := FField.FieldName;
               oItem.Negation   := chkNegationDat.Checked;
               oItem.Relation   := frEqual;
               oItem.LowerValue := DateToStd(dpFrom.DateTime, '-');
               oItem.UpperValue := IIF(chkTillDat.Checked, DateToStd(dpTill.DateTime, '-'), '');
               oItem.FilterType := ffDate;
               oItem.LogicalOp  := flEnd;
          end;
          ffLogical :
          begin
               oItem := FFilter.Add;
               oItem.FieldName  := FField.FieldName;
               oItem.Negation   := False;
               oItem.Relation   := frEqual;
               oItem.LowerValue := LogToStr(btnTrue.Down);
               oItem.UpperValue := '';
               oItem.FilterType := ffLogical;
               oItem.LogicalOp  := flEnd;
          end;
          ffMarker  :
          begin
               oItem := FFilter.Add;
               oItem.FieldName  := FField.FieldName;
               oItem.Negation   := False;
               oItem.Relation   := frEqual;
               oItem.LowerValue := LogToStr(btnMarked.Down);
               oItem.UpperValue := '';
               oItem.FilterType := ffMarker;
               oItem.LogicalOp  := flEnd;
          end;
          ffCustom  :
          begin
               FFilter.Clear;
               for i := 1 to 9 do
               begin
                    iPos := (FindComponent('cbxField' + IntToStr(i)) as TComboBox).ItemIndex;
                    if iPos > -1 then
                    begin
                         sFld := FFields.Strings[iPos];
                         sRel := (FindComponent('cbxRel'   + IntToStr(i)) as TComboBox).Text;
                         sVal := (FindComponent('edValue'  + IntToStr(i)) as TEdit).Text;
                         sLog := (FindComponent('cbxLog'   + IntToStr(i)) as TComboBox).Text;

                         if (sFld <> '') and (sRel <> '') and (sVal <> '') and (sLog <> '') then
                         begin
                              oItem := FFilter.Add;
                              oItem.FieldName  := sFld;
                              oItem.Relation   := StringToRelation(sRel);
                              oItem.FilterType := FieldToFilterType(FDataSet.FieldByName(sFld));
                              oItem.LowerValue := sVal;
                              oItem.UpperValue := '';
                              oItem.LogicalOp  := StringToLogical(sLog);
                              oItem.Negation   := False;
                         end;
                    end;
               end;
          end;
     end;
end;


{******************************************************************************}


procedure TFilterDialog.ClearAllItem;
var
   i : Integer;
begin
     for i := 1 to 9 do
     begin
          with (FindComponent('cbxField' + IntToStr(i)) as TComboBox) do
          begin
               ItemIndex := -1;
               Enabled   := False;
               Color     := clInfoBk;
          end;

          with (FindComponent('cbxRel' + IntToStr(i)) as TComboBox) do
          begin
               ItemIndex := -1;
               Enabled   := False;
               Color     := clInfoBk;
          end;

          with (FindComponent('edValue' + IntToStr(i)) as TEdit) do
          begin
               Text    := '';
               Enabled := False;
               Color   := clInfoBk;
          end;

          with (FindComponent('cbxLog' + IntToStr(i)) as TComboBox) do
          begin
               ItemIndex := -1;
               Enabled   := False;
               Color     := clInfoBk;
          end;
     end;
     cbxField1.Enabled := True;
     cbxField1.Color   := clWindow;
end;


// Set the Filter levels
procedure TFilterDialog.SetAllItems;
var
   i     : Integer;
   cbxTmp: TComboBox;
   edTmp : TEdit;
begin
     ClearAllItem;
     for i := 0 to FFilter.Count-1 do
     begin
          if FFilter.Items[i].LogicalOp = flNone then Break;
          // Field
          cbxTmp := (FindComponent('cbxField' + IntToStr(i+1)) as TComboBox);
          cbxTmp.OnChange := nil;
          cbxTmp.ItemIndex := FFields.IndexOf(FFilter.Items[i].FieldName);
          if cbxTmp.ItemIndex > -1 then
          begin
               cbxTmp.Enabled := True;
               cbxTmp.Color   := clWindow;
          end;
          cbxTmp.Update;
          cbxTmp.OnChange := FieldChange;

          // Relation
          cbxTmp := (FindComponent('cbxRel' + IntToStr(i+1)) as TComboBox);
          cbxTmp.OnChange := nil;
          cbxTmp.ItemIndex := cbxTmp.Items.IndexOf(RelationToString(FFilter.Items[i].Relation));
          if cbxTmp.ItemIndex > -1 then
          begin
               cbxTmp.Enabled := True;
               cbxTmp.Color   := clWindow;
          end;
          cbxTmp.OnChange := RelChange;
          cbxTmp.Update;

          // Edit
          edTmp := (FindComponent('edValue' + IntToStr(i+1)) as TEdit);
          edTmp.OnChange := nil;
          edTmp.Text := FFilter.Items[i].LowerValue;
          if edTmp.Text <> '' then
          begin
               edTmp.Enabled := True;
               edTmp.Color   := clWindow;
          end;
          edTmp.OnChange := ValueChange;

          // Logical Op.
          cbxTmp := (FindComponent('cbxLog' + IntToStr(i+1)) as TComboBox);
          cbxTmp.OnChange := nil;
          cbxTmp.ItemIndex := cbxTmp.Items.IndexOf(LogicalToString(FFilter.Items[i].LogicalOp));
          if cbxTmp.ItemIndex > -1 then
          begin
               cbxTmp.Enabled := True;
               cbxTmp.Color   := clWindow;
          end;
          cbxTmp.OnChange := LogChange;
          cbxTmp.Update;
     end;
     btnOk.SetFocus;
end;


procedure TFilterDialog.FieldChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr(TComboBox(Sender).Name, 1));
     if TComboBox(Sender).Text <> '' then
     begin
          with (FindComponent('cbxRel'  + IntToStr(iNum)) as TComboBox) do
          begin
               Enabled := True;
               Color   := clWindow;
               RelationStrings(Items, DataSet.FieldByName(FFields.Strings[TComboBox(Sender).ItemIndex]));
               SetFocus;
          end;
     end;
end;


procedure TFilterDialog.RelChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr(TComboBox(Sender).Name, 1));
     if TComboBox(Sender).Text <> '' then
     begin
          with (FindComponent('edValue'  + IntToStr(iNum)) as TEdit) do
          begin
               Enabled := True;
               Color   := clWindow;
               SetFocus;
          end;
     end;
end;


procedure TFilterDialog.ValueChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr(TEdit(Sender).Name, 1));
     if TEdit(Sender).Text <> '' then
     begin
          with (FindComponent('cbxLog'  + IntToStr(iNum)) as TComboBox) do
          begin
               Enabled := True;
               Color   := clWindow;
          end;
     end;
end;


procedure TFilterDialog.ValueExit(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr(TEdit(Sender).Name, 1));
     if TEdit(Sender).Text <> '' then
     begin
          (FindComponent('cbxLog'  + IntToStr(iNum)) as TComboBox).SetFocus;
     end;
end;


procedure TFilterDialog.LogChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TComboBox).Name, 1)) + 1;
     if (TComboBox(Sender).Text <> cnsLogEnd) and
        (TComboBox(Sender).Text <> '') then
     begin
          if (iNum < 10) then
          begin
               (FindComponent('btnClear' + IntToStr(iNum-1)) as TSpeedButton).Enabled := (FindComponent('cbxLog' + IntToStr(iNum-1)) as TComboBox).Enabled;
               with (FindComponent('cbxField' + IntToStr(iNum)) as TComboBox) do
               begin
                    Enabled := True;
                    Color   := clWindow;
                    SetFocus;
               end;
          end;
     end;
     if TComboBox(Sender).Text = cnsLogEnd then
     begin
          (FindComponent('btnClear' + IntToStr(iNum-1)) as TSpeedButton).Enabled := (FindComponent('cbxLog' + IntToStr(iNum-1)) as TComboBox).Enabled;
          btnOk.SetFocus;
     end;
end;


procedure TFilterDialog.ClearClick(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TSpeedButton).Name, 1));
     FFilter.Delete(iNum);
     SetAllItems;
end;


end.
