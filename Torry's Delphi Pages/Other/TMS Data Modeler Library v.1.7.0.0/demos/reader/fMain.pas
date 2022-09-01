unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, ComCtrls,
  Dialogs, Menus, StdCtrls,
  uAppMetaData, uGDAO, dgConsts;         

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    tsTable: TTabSheet;
    ListBoxTables: TListBox;
    PageControl2: TPageControl;
    tsFields: TTabSheet;
    ListBoxFields: TListBox;
    Splitter1: TSplitter;
    tsTableDescription: TTabSheet;
    Panel1: TPanel;
    lbFieldProperties: TListBox;
    mmFieldDescription: TMemo;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    mmTableDescription: TMemo;
    tsProcedures: TTabSheet;
    tsViews: TTabSheet;
    tsIndexes: TTabSheet;
    ListBoxIndexes: TListBox;
    lbProcedures: TListBox;
    Splitter4: TSplitter;
    mmProcedure: TMemo;
    lbViews: TListBox;
    Splitter5: TSplitter;
    mmView: TMemo;
    Panel2: TPanel;
    btOpen: TButton;
    tsRelationship: TTabSheet;
    lbRelationships: TListBox;
    Splitter6: TSplitter;
    mmRelationship: TMemo;
    btAbout: TButton;
    procedure Close1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxTablesClick(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure lbProceduresClick(Sender: TObject);
    procedure lbViewsClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure lbRelationshipsClick(Sender: TObject);
    procedure btAboutClick(Sender: TObject);
  private
    FAMD: TAppMetaData;
    procedure LoadAMD(AFileName: string);
    procedure LoadTable(ATableName: string);
    procedure LoadProcedure(AObject: TGDAOObject);
    procedure LoadView(AObject: TGDAOObject);
    procedure LoadRelationship(ARel: TGDAORelationship);
    procedure LoadField(AField: TGDAOField);
  end;

var
  Form1: TForm1;

implementation
uses
  fAbout;

{$R *.dfm}

procedure TForm1.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.LoadAMD(AFileName: string);
var
  i: integer;
  Cat: TGDAOCategory;
begin
  if Assigned(FAMD) then
    FAMD.Free;
  FAMD := TAppMetaData.LoadFromFile(AFileName);

  Caption := Format('Database Model Explorer (%s)', [AFileName]);

  {Load tables}
  ListBoxTables.Items.BeginUpdate;
  try
    ListBoxTables.Items.Clear;
    for i:=0 to FAMD.DataDictionary.Tables.Count-1 do
      ListBoxTables.Items.Add(FAMD.DataDictionary.Tables[i].TableName);
  finally
    ListBoxTables.Items.EndUpdate;
  end;

  {Load Relationships}
  lbRelationships.Items.BeginUpdate;
  try
    lbRelationships.Items.Clear;
    for i := 0 to FAMD.DataDictionary.Relationships.Count - 1 do
      lbRelationships.Items.AddObject(
        FAMD.DataDictionary.Relationships[i].RelationshipName, FAMD.DataDictionary.Relationships[i]);
  finally
    lbRelationships.Items.EndUpdate;
  end;

  {Load procedures}
  Cat := FAMD.DataDictionary.Categories.FindByType(ctProcedure);
  if Cat <> nil then
  begin
    lbProcedures.Items.BeginUpdate;
    try
      lbProcedures.Items.Clear;
      for i := 0 to Cat.Objects.Count - 1 do
        lbProcedures.Items.AddObject(
          Cat.Objects[i].ObjectName, Cat.Objects[i]);
    finally
      lbProcedures.Items.EndUpdate;
    end;
  end;

  {Load views}
  Cat := FAMD.DataDictionary.Categories.FindByType(ctView);
  if Cat <> nil then
  begin
    lbViews.Items.BeginUpdate;
    try
      lbViews.Items.Clear;
      for i := 0 to Cat.Objects.Count - 1 do
        lbViews.Items.AddObject(
          Cat.Objects[i].ObjectName, Cat.Objects[i]);
    finally
      lbViews.Items.EndUpdate;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FAMD) then
    FAMD.Free;
end;

procedure TForm1.ListBoxTablesClick(Sender: TObject);
begin
  if Assigned(FAMD) and (ListBoxTables.ItemIndex >= 0) then
    LoadTable(ListBoxTables.Items[ListBoxTables.ItemIndex]);
end;

procedure TForm1.LoadTable(ATableName: string);
var
  table: TGDAOTable;
  i: integer;
  j: integer;
  indexFields: string;
begin
  if Assigned(FAMD) then
  begin
    table := FAMD.DataDictionary.TableByName(ATableName);
    if Assigned(table) then
    begin
      mmTableDescription.Lines.Text := table.Description;

      {Fill Fields list box}
      ListBoxFields.Items.BeginUpdate;
      try
        ListBoxFields.Items.Clear;
        for i := 0 to table.Fields.Count - 1 do
        begin
          if table.Fields[i].InPrimaryKey then
            ListBoxFields.Items.AddObject('(PK) ' + table.Fields[i].FieldName, table.Fields[i])
          else
          if table.Fields[i].IsForeignKey then
            ListBoxFields.Items.AddObject('(FK) ' + table.Fields[i].FieldName, table.Fields[i])
          else
            ListBoxFields.Items.AddObject(table.Fields[i].FieldName, table.Fields[i]);
        end;
      finally
        ListBoxFields.Items.EndUpdate;
      end;

      {Fill Indexes list box}
      ListBoxIndexes.Items.BeginUpdate;
      try
        ListBoxIndexes.Items.Clear;
        for i := 0 to table.Indexes.Count - 1 do
        begin
          indexFields := '';
          for j := 0 to table.Indexes[i].IFields.Count - 1 do
            indexFields := indexFields + table.Indexes[i].IFields[j].FieldName + ', ';
          indexFields := Copy(indexFields, 1, Length(indexFields) - 2);

          ListBoxIndexes.Items.Add(Format('%s (%s)', [table.Indexes[i].IndexName, indexFields]));
        end;
      finally
        ListBoxIndexes.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TForm1.ListBoxFieldsClick(Sender: TObject);
begin
  if Assigned(FAMD) and (ListBoxTables.ItemIndex >= 0) and (ListBoxFields.ItemIndex >= 0) then
    LoadField(TGDAOField(ListBoxFields.Items.Objects[ListBoxFields.ItemIndex]));
end;

procedure TForm1.LoadField(AField: TGDAOField);
begin
  if Assigned(AField) then
  begin
    with lbFieldProperties.Items do
    begin
      Clear;
      Add(Format('Data type: %s', [AField.DataTypeName]));
      Add(Format('Size: %d', [AField.Size]));
      if AField.Required then
        Add('Required: Yes')
      else
        Add('Required: No');
    end;
    mmFieldDescription.Lines.Text := AField.Description;
  end;
end;

procedure TForm1.lbProceduresClick(Sender: TObject);
begin
  if Assigned(FAMD) and (lbProcedures.ItemIndex >= 0) then
    LoadProcedure(TGDAOObject(lbProcedures.Items.Objects[lbProcedures.ItemIndex]));
end;

procedure TForm1.LoadProcedure(AObject: TGDAOObject);
begin
  mmProcedure.Lines.Text := AObject.CreateImplementation;
end;

procedure TForm1.lbViewsClick(Sender: TObject);
begin
  if Assigned(FAMD) and (lbViews.ItemIndex >= 0) then
    LoadView(TGDAOObject(lbViews.Items.Objects[lbViews.ItemIndex]));
end;

procedure TForm1.LoadView(AObject: TGDAOObject);
begin
  mmView.Lines.Text := AObject.CreateImplementation;
end;

procedure TForm1.btOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadAMD(OpenDialog1.FileName);
end;

procedure TForm1.lbRelationshipsClick(Sender: TObject);
begin
  if Assigned(FAMD) and (lbRelationships.ItemIndex >= 0) then
    LoadRelationship(TGDAORelationship(lbRelationships.Items.Objects[lbRelationships.ItemIndex]));
end;

procedure TForm1.LoadRelationship(ARel: TGDAORelationship);
var
  c: integer;
begin
  with mmRelationship.Lines do
  begin
    Clear;
    Add(Format('Parent table: %s', [ARel.ParentTable.TableName]));
    Add(Format('Child table: %s', [ARel.ChildTable.TableName]));
    Add('Field keys:');
    for c := 0 to ARel.FieldLinks.Count - 1 do
      Add(Format('%s => %s', [ARel.FieldLinks[c].ParentFieldName, ARel.FieldLinks[c].ChildFieldName]));

  end;
end;

procedure TForm1.btAboutClick(Sender: TObject);
begin
  fmAbout.ShowModal;
end;

end.

