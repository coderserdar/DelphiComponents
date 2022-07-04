const
  sqlSelectFields =
      'select '+
      '  RDB$RELATION_FIELDS.RDB$FIELD_NAME, '+
      '  RDB$RELATION_FIELDS.rdb$description '+
      'from '+
      '  RDB$RELATION_FIELDS '+
      'where '+
      '  RDB$RELATION_FIELDS.rdb$relation_name = ''%s'' '+
      'order by rdb$relation_fields.rdb$field_position ';
type
  TFieldInfo = class
    Description:string;
    constructor Create(ADescription:string);
  end;

  { TFieldInfo }
constructor TFieldInfo.Create(ADescription:string);
begin
  inherited Create;
  Description:=ADescription;
end;


  { TFBCustomDataSetSQLEditor }
procedure TFBCustomDataSetSQLEditor.ListBoxRelationsDblClick(
   Sender: TObject);
var
  i: integer;
  Als, FieldsStr, Tmp: string;
  b:boolean;
begin
  if ActiveEditor <> nil then
  begin
    if (ListBoxRelations.Items.Count>0) and (ListBoxRelations.ItemIndex>-1) and
       (ListBoxRelations.ItemIndex<ListBoxRelations.Items.Count) then
    with ActiveEditor do
    begin
      if CheckBox1.Checked then Lines.Clear;
      case Pagecontrol1.ActivePageIndex of
        0:begin
            if ListBoxFields.SelCount = 0 then
            begin
              Lines.Add('select');
              Lines.Add('  *');
              Lines.Add('from');
              Lines.Add('  ' + ListBoxRelations.Items[ListBoxRelations.ItemIndex] + ' ' + Edit1.Text);
            end
            else
            begin
              Lines.Add('select');
              if Edit1.Text <> '' then
                Als := Edit1.Text
              else
                Als := ListBoxRelations.Items[ListBoxRelations.ItemIndex];
              FieldsStr := '';
              for i := 0 to ListBoxFields.Items.Count - 1 do
              begin
                if ListBoxFields.Selected[i] then
                begin
                  if FieldsStr <> '' then Lines.Add(FieldsStr + ',');
                  FieldsStr := '  ' + Als + '.' + ListBoxFields.Items[i];
                end;
              end;
              if FieldsStr <> '' then Lines.Add(FieldsStr);
              Lines.Add('from');
              Lines.Add('  ' + ListBoxRelations.Items[ListBoxRelations.ItemIndex]);
            end;
          end;
        1:begin
            Lines.clear;
            Lines.Add('insert into ' + ListBoxRelations.Items[ListBoxRelations.ItemIndex]);
            Tmp := ListBoxFields.Items[0];
            if ListBoxFields.Items.Count - 1 > 0 then
            begin
              for i := 1 to ListBoxFields.Items.Count - 1 do
                tmp := tmp + ', ' + ListBoxFields.Items[i];
            end;
            tmp := '(' + tmp + ')';
            Lines.Add(tmp);
            Tmp := ':' + ListBoxFields.Items[0];
            if ListBoxFields.Items.Count - 1 > 0 then
            begin
              for i := 1 to ListBoxFields.Items.Count - 1 do
                tmp := tmp + ', ' + ':' + ListBoxFields.Items[i];
            end;
            tmp := 'values (' + tmp + ')';
            Lines.Add(tmp);
          end;
        2:begin
            Lines.clear;
            Lines.Add('update');
            Lines.Add('  '+ListBoxRelations.Items[ListBoxRelations.ItemIndex]);
            Lines.Add('set ');
            for i := 1 to ListBoxFields.Items.Count - 1 do
            begin
              FieldsStr :='  '+ ListBoxFields.Items[i] + ' = :' + ListBoxFields.Items[i];
              if i<ListBoxFields.Items.Count - 1 then
                FieldsStr:=FieldsStr + ',';
              Lines.Add(FieldsStr);
            end;
            b:=true;
            FieldsStr := '';
            for i := 0 to ListBoxFields.Items.Count - 1 do
            begin
              if ListBoxFields.Selected[i] then
              begin
                if FieldsStr <> '' then
                begin
                  if b then
                  begin
                    Lines.Add('where ');
                    b:=false;
                  end;
                  Lines.Add(FieldsStr + ',');
                end;
                FieldsStr := '  ' + ListBoxFields.Items[i] + ' = :' +ListBoxFields.Items[i]
              end;
            end;
            if FieldsStr <> '' then
            begin
              if b then
                Lines.Add('where ');
              Lines.Add(FieldsStr);
            end;
          end;
        3:begin
            Lines.clear;
            Lines.Add('delete');
            Lines.Add('from');
            Lines.Add('  '+ListBoxRelations.Items[ListBoxRelations.ItemIndex]);
            Lines.Add('where ');
            for i := 0 to ListBoxFields.Items.Count - 1 do
            begin
              if ListBoxFields.Selected[i] then
                tmp := ListBoxFields.Items[i] + ' = :' +ListBoxFields.Items[i];
            end;
            Lines.Add(tmp);
            end;
        4:begin
            Lines.clear;
            Lines.Add('select');
            Lines.Add('  *');
            Lines.Add('from');
            Lines.Add('  ' +ListBoxRelations.Items[ListBoxRelations.ItemIndex]);
            Lines.Add('where');
            for i := 0 to ListBoxFields.Items.Count - 1 do
            begin
              if ListBoxFields.Selected[i] then
                tmp := ListBoxFields.Items[i] + ' = :' +ListBoxFields.Items[i];
            end;
            Lines.Add('  '+ tmp);
          end;
      end;
    end;
  end;
end;


procedure TFBCustomDataSetSQLEditor.btnTestClick(Sender: TObject);
begin
  if Assigned(FDS.DataBase) and Assigned(FDS.Transaction) then
  begin
    FBCustomDataSetSQLEditorTestForm:=TFBCustomDataSetSQLEditorTestForm.Create(Application);
    try
      FBCustomDataSetSQLEditorTestForm.FBDataSetTest.DataBase:=FDS.DataBase;
      FBCustomDataSetSQLEditorTestForm.FBDataSetTest.Transaction:=FDS.Transaction;
      FBCustomDataSetSQLEditorTestForm.FBDataSetTest.SQLSelect.Text:=ActiveEditor.Lines.Text;
      FBCustomDataSetSQLEditorTestForm.FBDataSetTest.Open;
      FBCustomDataSetSQLEditorTestForm.SynEdit1.Text:=FBCustomDataSetSQLEditorTestForm.FBDataSetTest.QuerySelect.Plan;
      FBCustomDataSetSQLEditorTestForm.SynEdit1.Highlighter:=SynSQLSyn1;
      FBCustomDataSetSQLEditorTestForm.Caption:='Test of '+FBCustomDataSetSQLEditorTestForm.FBDataSetTest.Name+' - '+PageControl1.ActivePage.Caption;
      FBCustomDataSetSQLEditorTestForm.ShowModal
    except
      on E:Exception do
        ShowMessage(E.Message);
    end;
    FBCustomDataSetSQLEditorTestForm.Free
  end;
end;


