unit DlgColor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, checklst, Buttons, Mask,
  ComCtrls, DB, DUtils, ColorGrd, DlgSelFld, DFilters, DDB;

type
  TColorFltDialog = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edName: TEdit;
    lbxColorSet: TListBox;
    btnClear1: TSpeedButton;
    btnClear2: TSpeedButton;
    btnClear3: TSpeedButton;
    btnClear4: TSpeedButton;
    btnClear5: TSpeedButton;
    btnClear6: TSpeedButton;
    btnClear7: TSpeedButton;
    btnClear8: TSpeedButton;
    btnClear9: TSpeedButton;
    cgOffer: TColorGrid;
    chkSelField: TCheckBox;
    cbxField1: TComboBox;
    cbxField2: TComboBox;
    cbxField3: TComboBox;
    cbxField4: TComboBox;
    cbxField5: TComboBox;
    cbxField6: TComboBox;
    cbxField7: TComboBox;
    cbxField8: TComboBox;
    cbxField9: TComboBox;
    cbxRel9: TComboBox;
    cbxRel8: TComboBox;
    cbxRel7: TComboBox;
    cbxRel6: TComboBox;
    cbxRel5: TComboBox;
    cbxRel4: TComboBox;
    cbxRel3: TComboBox;
    cbxRel2: TComboBox;
    cbxRel1: TComboBox;
    edValue1: TEdit;
    edValue2: TEdit;
    edValue3: TEdit;
    edValue4: TEdit;
    edValue5: TEdit;
    edValue6: TEdit;
    edValue7: TEdit;
    edValue8: TEdit;
    edValue9: TEdit;
    cbxLog1: TComboBox;
    cbxLog2: TComboBox;
    cbxLog3: TComboBox;
    cbxLog4: TComboBox;
    cbxLog5: TComboBox;
    cbxLog6: TComboBox;
    cbxLog7: TComboBox;
    cbxLog8: TComboBox;
    cbxLog9: TComboBox;
    Bevel1: TBevel;
    panTest: TPanel;
    btnInsert: TBitBtn;
    btnDelete: TBitBtn;
    btnSelect: TBitBtn;
    procedure OfferChange(Sender: TObject);
    procedure OfferClick(Sender: TObject);
    procedure LogChange(Sender: TObject);
    procedure FieldChange(Sender: TObject);
    procedure RelChange(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure InsColorClick(Sender: TObject);
    procedure DelColorClick(Sender: TObject);
    procedure ColorSetClick(Sender: TObject);
    procedure SelFieldClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure SelFieldCheck(Sender: TObject);
    procedure NameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
    FDataSet : TDataSet;
    bNew     : Boolean;
    sSelFld  : String;
    FFields  : TStrings;
    FColors  : TDFilters;
    FMarker  : String;

    // Clear all color item
    procedure ClearAllItem;
    // Save the data
    procedure SaveCustomColor;
    procedure SetFields(Value: TStrings);

  public
    { Public declarations }
    property DataSet     : TDataSet  read FDataSet write FDataSet;
    property Colors      : TDFilters read FColors  write FColors;
    property Fields      : TStrings  read FFields  write SetFields;
    property MarkerField : String    read FMarker  write FMarker;

  end;

var
  ColorFltDialog: TColorFltDialog;

implementation

{$R *.DFM}


procedure TColorFltDialog.SetFields(Value: TStrings);
begin
     FFields.Assign(Value);
end;


procedure TColorFltDialog.FormShow(Sender: TObject);
var
   i : Integer;
begin
     FFields.Clear;
     cbxField1.Items.Clear;
     for i := 0 to DataSet.Fields.Count-1 do
     begin
          FFields.Add(DataSet.Fields[i].FieldName);
          cbxField1.Items.Add(Trim(DataSet.Fields[i].DisplayLabel));
     end;
     cbxField2.Items.AddStrings(cbxField1.Items);
     cbxField3.Items.AddStrings(cbxField1.Items);
     cbxField4.Items.AddStrings(cbxField1.Items);
     cbxField5.Items.AddStrings(cbxField1.Items);
     cbxField6.Items.AddStrings(cbxField1.Items);
     cbxField7.Items.AddStrings(cbxField1.Items);
     cbxField8.Items.AddStrings(cbxField1.Items);
     cbxField9.Items.AddStrings(cbxField1.Items);

     Colors.SetMenuItems(lbxColorSet.Items);
     RelationStrings(cbxRel1.Items, ffString);
     LogicalOpStrings(cbxLog1.Items);

     cbxRel2.Items.AddStrings(cbxRel1.Items);
     cbxRel3.Items.AddStrings(cbxRel1.Items);
     cbxRel4.Items.AddStrings(cbxRel1.Items);
     cbxRel5.Items.AddStrings(cbxRel1.Items);
     cbxRel6.Items.AddStrings(cbxRel1.Items);
     cbxRel7.Items.AddStrings(cbxRel1.Items);
     cbxRel8.Items.AddStrings(cbxRel1.Items);
     cbxRel9.Items.AddStrings(cbxRel1.Items);

     if foCaseInsensitive in DataSet.FilterOptions then
     begin
          edValue1.CharCase := ecUpperCase;
          edValue2.CharCase := ecUpperCase;
          edValue3.CharCase := ecUpperCase;
          edValue4.CharCase := ecUpperCase;
          edValue5.CharCase := ecUpperCase;
          edValue6.CharCase := ecUpperCase;
          edValue7.CharCase := ecUpperCase;
          edValue8.CharCase := ecUpperCase;
          edValue9.CharCase := ecUpperCase;
     end;

     cbxLog2.Items.AddStrings(cbxLog1.Items);
     cbxLog3.Items.AddStrings(cbxLog1.Items);
     cbxLog4.Items.AddStrings(cbxLog1.Items);
     cbxLog5.Items.AddStrings(cbxLog1.Items);
     cbxLog6.Items.AddStrings(cbxLog1.Items);
     cbxLog7.Items.AddStrings(cbxLog1.Items);
     cbxLog8.Items.AddStrings(cbxLog1.Items);
     cbxLog9.Items.AddStrings(cbxLog1.Items);
     panTest.Color      := cgOffer.BackgroundColor;
     panTest.Font.Color := cgOffer.ForegroundColor;
     btnDelete.Enabled  := (lbxColorSet.Items.Count > 0);

     if lbxColorSet.Items.Count > 0 then
     begin
          lbxColorSet.ItemIndex := 0; // Go to first item
          ColorSetClick(lbxColorSet);
     end;
     bNew := False;
end;


// Changed the colorset name
procedure TColorFltDialog.NameChange(Sender: TObject);
var
   oColor : TDFilter;
begin
     if (lbxColorSet.ItemIndex > -1) and not bNew then
     begin
          oColor := Colors.FindItem(lbxColorSet.Items.Strings[lbxColorSet.ItemIndex]);
          if not Assigned(oColor) then oColor := Colors.Add;
          oColor.Caption := edName.Text;
          Colors.SetMenuItems(lbxColorSet.Items);
     end;
end;


// Change Color settings
procedure TColorFltDialog.OfferChange(Sender: TObject);
begin
     panTest.Color      := cgOffer.BackgroundColor;
     panTest.Font.Color := cgOffer.ForegroundColor;
end;


// Click on the color
procedure TColorFltDialog.OfferClick(Sender: TObject);
begin
     if edName.Text <> '' then SaveCustomColor;
end;


// FieldBox changed
procedure TColorFltDialog.FieldChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TComboBox).Name, 1));
     if ((Sender as TComboBox).Text <> '') then
     begin
          with (FindComponent('cbxRel'  + IntToStr(iNum)) as TComboBox) do
          begin
               Enabled := True;
               Color   := clWindow;
               RelationStrings(Items, DataSet.FieldByName(FFields.Strings[TComboBox(Sender).ItemIndex]));
          end;
     end;
end;


// RelationBox changed
procedure TColorFltDialog.RelChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TComboBox).Name, 1));
     if ((Sender as TComboBox).Text <> '') then
     begin
          with (FindComponent('edValue'  + IntToStr(iNum)) as TEdit) do
          begin
               Enabled := True;
               Color   := clWindow;
          end;
     end;
end;


// Value Edit changed
procedure TColorFltDialog.ValueChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TEdit).Name, 1));
     if ((Sender as TEdit).Text <> '') then
     begin
          with (FindComponent('cbxLog'  + IntToStr(iNum)) as TComboBox) do
          begin
               Enabled := True;
               Color   := clWindow;
          end;
     end;
end;


// Log box changed
procedure TColorFltDialog.LogChange(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TComboBox).Name, 1)) + 1;
     if ((Sender as TComboBox).Text <> cnsLogEnd) and
        ((Sender as TComboBox).Text <> '') then
     begin
          if (iNum < 10) then
          begin
               (FindComponent('btnClear' + IntToStr(iNum-1)) as TSpeedButton).Enabled := (FindComponent('cbxLog' + IntToStr(iNum-1)) as TComboBox).Enabled;
               (FindComponent('cbxField' + IntToStr(iNum)) as TComboBox).Enabled := True;
          end;
     end;
     if ((Sender as TComboBox).Text = cnsLogEnd) then
     begin
          (FindComponent('btnClear' + IntToStr(iNum-1)) as TSpeedButton).Enabled := (FindComponent('cbxLog' + IntToStr(iNum-1)) as TComboBox).Enabled;
          SaveCustomColor;
          lbxColorSet.ItemIndex := lbxColorSet.Items.Count-1;
          lbxColorSet.SetFocus;
     end;
end;


// Ne color generate
procedure TColorFltDialog.InsColorClick(Sender: TObject);
begin
     ClearAllItem;
     cbxField1.Enabled := True;
     cbxField1.Color   := clWindow;
     edName.Enabled    := True;
     edName.Color      := clWindow;
     edName.Text       := 'Color ' + IntToStr(lbxColorSet.Items.Count);
     edName.SetFocus;
     bNew := True;
end;


// Clear all color item
procedure TColorFltDialog.ClearAllItem;
var
   i : Integer;
begin
     edName.Text := '';
     cgOffer.ForegroundIndex := cgOffer.ColorToIndex(clWindowText);
     cgOffer.BackgroundIndex := cgOffer.ColorToIndex(clWindow);
     chkSelField.Checked := False;

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


// Clear the selected level
procedure TColorFltDialog.ClearClick(Sender: TObject);
var
   iNum : Integer;
begin
     iNum := StrToInt(RightStr((Sender as TSpeedButton).Name, 1));
     Colors.DeleteLevel(edName.Text, iNum-1);
     lbxColorSet.SetFocus;
     ColorSetClick(Sender);
end;


// Delete selected item
procedure TColorFltDialog.DelColorClick(Sender: TObject);
begin
     if lbxColorSet.ItemIndex >= 0 then
     begin
          Colors.Delete(lbxColorSet.Items.Strings[lbxColorSet.ItemIndex]);
          lbxColorSet.Items.Delete(lbxColorSet.ItemIndex);
          lbxColorSet.SetFocus;
          ColorSetClick(Sender);
          btnDelete.Enabled := (lbxColorSet.Items.Count > 0);
     end;
end;


// Save the new item
procedure TColorFltDialog.SaveCustomColor;
var
   oSub   : TDSubFilter;
   sFld   : String;
   sRel   : String;
   sVal   : String;
   sLog   : String;
   i      : Integer;
   iPos   : Integer;
   oColor : TDFilter;
begin
     oColor := Colors.FindItem(edName.Text);
     if not Assigned(oColor) then
     begin
          oColor := Colors.Add;
          oColor.Caption := edName.Text;
     end;
     oColor.Levels.Clear;
     for i := 1 to 9 do
     begin
          iPos := (FindComponent('cbxField' + IntToStr(i)) as TComboBox).ItemIndex;
          if iPos > -1 then
          begin
               sFld := FFields.Strings[iPos];
               sRel := (FindComponent('cbxRel'   + IntToStr(i)) as TComboBox).Text;
               sVal := (FindComponent('edValue'  + IntToStr(i)) as TEdit).Text;
               sLog := (FindComponent('cbxLog'   + IntToStr(i)) as TComboBox).Text;

               if (sFld <> '') and (sRel <> '') and (sVal <> '') then
               begin
                    oSub := oColor.Levels.Add;
                    oSub.FieldName    := sFld;
                    oSub.Negation     := False;
                    oSub.Relation     := StringToRelation(sRel);
                    oSub.FilterType   := FieldToFilterType(DataSet.FieldByName(sFld));
                    oSub.LowerValue   := sVal;
                    oSub.UpperValue   := '';
                    oSub.LogicalOp    := StringToLogical(sLog);
                    oColor.BackGround := cgOffer.BackgroundColor;
                    oColor.ForeGround := cgOffer.ForegroundColor;
                    oColor.Selected   := IIF(chkSelField.Checked, sSelFld, cnsAllItem);
               end;
          end;
     end;
     Colors.SetMenuItems(lbxColorSet.Items);
     lbxColorSet.ItemIndex := lbxColorSet.Items.Count-1;
     bNew := False;
     btnDelete.Enabled := (lbxColorSet.Items.Count > 0);
end;


// Select one item in Color Set
procedure TColorFltDialog.ColorSetClick(Sender: TObject);
var
   i     : Integer;
   oItem : TDFilter;
   cbxTmp: TComboBox;
   edTmp : TEdit;
begin
     ClearAllItem;
     if (lbxColorSet.ItemIndex >= 0) then
     begin
          edName.Enabled  := True;
          edName.Color    := clWindow;
          edName.Text     := lbxColorSet.Items.Strings[lbxColorSet.ItemIndex];
          oItem := Colors.FindItem(edName.Text);
          if Assigned(oItem) then
          begin
               sSelFld := oItem.Selected;
               chkSelField.Enabled := True;
               chkSelField.OnClick := nil;
               chkSelField.Checked := not (sSelFld = cnsAllItem);
               chkSelField.OnClick := SelFieldCheck;
               cgOffer.BackgroundIndex := cgOffer.ColorToIndex(oItem.BackGround);
               cgOffer.ForegroundIndex := cgOffer.ColorToIndex(oItem.ForeGround);

               for i := 1 to 9 do
               begin
                    if oItem.Levels[i].LogicalOp = flNone then Break;

                    // Field
                    cbxTmp := (FindComponent('cbxField' + IntToStr(i)) as TComboBox);
                    cbxTmp.OnChange := nil;
                    cbxTmp.ItemIndex := FFields.IndexOf(oItem.Levels[i].FieldName);
                    if cbxTmp.ItemIndex > -1 then
                    begin
                         cbxTmp.Enabled := True;
                         cbxTmp.Color   := clWindow;
                    end;
                    cbxTmp.Update;
                    cbxTmp.OnChange := FieldChange;

                    // Relation
                    cbxTmp := (FindComponent('cbxRel' + IntToStr(i)) as TComboBox);
                    cbxTmp.OnChange := nil;
                    cbxTmp.ItemIndex := cbxTmp.Items.IndexOf(RelationToString(oItem.Levels[i].Relation));
                    if cbxTmp.ItemIndex > -1 then
                    begin
                         cbxTmp.Enabled := True;
                         cbxTmp.Color   := clWindow;
                    end;
                    cbxTmp.OnChange := RelChange;
                    cbxTmp.Update;

                    // Edit
                    edTmp := (FindComponent('edValue' + IntToStr(i)) as TEdit);
                    edTmp.OnChange := nil;
                    edTmp.Text := oItem.Levels[i].LowerValue;
                    if edTmp.Text <> '' then
                    begin
                         edTmp.Enabled := True;
                         edTmp.Color   := clWindow;
                    end;
                    edTmp.OnChange := ValueChange;

                    // Logical Op.
                    cbxTmp := (FindComponent('cbxLog' + IntToStr(i)) as TComboBox);
                    cbxTmp.OnChange := nil;
                    cbxTmp.ItemIndex := cbxTmp.Items.IndexOf(LogicalToString(oItem.Levels[i].LogicalOp));
                    if cbxTmp.ItemIndex > -1 then
                    begin
                         cbxTmp.Enabled := True;
                         cbxTmp.Color   := clWindow;
                    end;
                    cbxTmp.OnChange := LogChange;
                    cbxTmp.Update;
               end;
          end;
          lbxColorSet.SetFocus;
          if lbxColorSet.ItemIndex < 0 then lbxColorSet.ItemIndex := 0;
     end;
end;


// Click on the field selection
procedure TColorFltDialog.SelFieldClick(Sender: TObject);
var
   DlgSelFld : TSelFldDialog;
   i         : Integer;
   bAll      : Boolean;
   bNone     : Boolean;
begin
     DlgSelFld := TSelFldDialog.Create(Application);
     try
     begin
          DlgSelFld.clbSelField.Clear;
          DlgSelFld.Fields.Clear;
          for i := 0 to cbxField1.Items.Count-1 do
          begin
               if FFields.Strings[i] <> MarkerField then
               begin
                    DlgSelFld.clbSelField.Items.Add(cbxField1.Items.Strings[i]);
                    DlgSelFld.Fields.Add(FFields.Strings[i]);
               end;
          end;

          // Check it the items if all or selected earlier
          if sSelFld = '' then sSelFld := cnsAllItem;
          if sSelFld = cnsAllItem then DlgSelFld.MarkONClick(nil) else
          begin
               for i := 0 to DlgSelFld.clbSelField.Items.Count-1 do
               begin
                    DlgSelFld.clbSelField.Checked[i] := IsSub(DlgSelFld.Fields.Strings[i], sSelFld);
               end;
          end;

          if (DlgSelFld.ShowModal = mrOk) then
          begin
               bAll  := True;
               bNone := True;
               sSelFld := '';
               for i := 0 to DlgSelFld.clbSelField.Items.Count-1 do
               begin
                    if DlgSelFld.clbSelField.Checked[i] then
                    begin
                         sSelFld := sSelFld + ',' + DlgSelFld.Fields.Strings[i];
                         bNone   := False;
                    end
                    else bAll := False;
               end;
               if bAll then sSelFld := cnsAllItem else
               begin
                    if bNone then sSelFld := cnsAllItem else
                    begin
                         sSelFld := Copy(sSelFld, 2, Length(sSelFld));
                    end;
               end;
          end;
     end;
     finally
          DlgSelFld.Free;
     end;
     SaveCustomColor;
end;


// Check the Selection
procedure TColorFltDialog.SelFieldCheck(Sender: TObject);
begin
     if chkSelField.Checked then SelFieldClick(Sender) else sSelFld := cnsAllItem;
end;


procedure TColorFltDialog.FormCreate(Sender: TObject);
begin
     FFields := TStringList.Create;
end;


procedure TColorFltDialog.FormDestroy(Sender: TObject);
begin
     FFields.Free;
     FFields := nil;
end;


end.
 