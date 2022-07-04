unit MakeSelectD;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Buttons, Grids, DBGrids, Db, DBCtrls,
  SelectPanel, DBClient, SelectPanelConsts;

type
  TMakeSelect = class(TForm)
    GroupBox2: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    DBText1: TDBText;
    DBLookupListBox1: TDBLookupListBox;
    DBLookupListBox2: TDBLookupListBox;
    SelectedRows: TClientDataSet;
    DSelectedRows: TDataSource;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure KBKDBGrid3KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBox2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DBLookupListBox1DblClick(Sender: TObject);
    procedure DBLookupListBox2DblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FKey: Word;
  protected
    FieldKey, FieldList, FieldNote: TField;
    FSelectPanel: TSelectPanel;
  public
  end;

var
  MakeSelect: TMakeSelect;

function MakeSelectExecute(ASelectPanel: TSelectPanel): Boolean;

implementation

{$R *.DFM}

uses Variants;

function MakeSelectExecute(ASelectPanel: TSelectPanel): Boolean;
begin
  result:=False;
  if ASelectPanel=nil then exit;
  if MakeSelect=nil then Application.CreateForm(TMakeSelect, MakeSelect);
  with MakeSelect do begin
    FSelectPanel:=ASelectPanel;
    Caption:=FSelectPanel.Caption;
    FieldKey:=FSelectPanel.KeyF;
    FieldList:=FSelectPanel.ListF;
    FieldNote:=FSelectPanel.NoteF;
    if FieldNote<>nil then begin
      DBText1.DataSource:=FSelectPanel.ListSource;
      DBText1.DataField:=FieldNote.FieldName;
      DBLookupListBox1.Height:=DBLookupListBox2.Height-13;
    end
    else begin
      DBText1.DataSource:=nil;
      DBText1.DataField:='';
      DBLookupListBox1.Height:=DBLookupListBox2.Height;
    end;
    DBLookupListBox1.KeyField:='';
    DBLookupListBox1.ListField:='';
    DBLookupListBox1.ListSource:=FSelectPanel.ListSource;
    DBLookupListBox1.KeyField:=FSelectPanel.KeyField;
    DBLookupListBox1.ListField:=FSelectPanel.ListField;
    if FSelectPanel.OldKeyValue=Null then begin
      DBLookupListBox1.ListSource.DataSet.First;
      DBLookupListBox1.KeyValue:=FSelectPanel.KeyF.Value;
    end
    else DBLookupListBox1.KeyValue:=FSelectPanel.OldKeyValue;

    with SelectedRows do begin
      Active:=False;
      with FieldDefs do begin
        Clear;
        with AddFieldDef do begin
          Name := FSelectPanel.DS.Fields[0].FieldName;
          DataType := FSelectPanel.DS.Fields[0].DataType;
          Required := FSelectPanel.DS.Fields[0].Required;
        end;
        with AddFieldDef do begin
          Name := FSelectPanel.DS.Fields[1].FieldName;
          DataType := FSelectPanel.DS.Fields[1].DataType;
          Size := FSelectPanel.DS.Fields[1].Size;
        end;
      end;
      with IndexDefs do begin
        Clear;
        IndexName:='';
        CreateDataSet;
        try
          DisableControls;
          FSelectPanel.DS.First;
          while not FSelectPanel.DS.EOF do begin
            InsertRecord([FSelectPanel.DS.Fields[0].Value, FSelectPanel.DS.Fields[1].Value]);
            FSelectPanel.DS.Next;
          end;
        finally
          EnableControls;
        end;
        if not IsEmpty then DBLookupListBox2.KeyValue:=Fields[0].Value;

        with AddIndexDef do begin
          Name := 'IDIndex';
          Fields := FSelectPanel.DS.Fields[0].FieldName;
          Options := [ixPrimary];
        end;
        with AddIndexDef do begin
          Name := 'TEXTIndex';
          Fields := FSelectPanel.DS.Fields[1].FieldName;
          Options := [ixCaseInsensitive];
        end;
      end;
      IndexName:=IndexDefs.Items[1].Name;
    end;
    DBLookupListBox2.KeyField:=SelectedRows.Fields[0].FieldName;
    DBLookupListBox2.ListField:=SelectedRows.Fields[1].FieldName;

    result:=Showmodal=mrOk;
    if result then begin
      FSelectPanel.OldKeyValue:=DBLookupListBox1.KeyValue;
      try
        FSelectPanel.DS.DisableControls;
//        FSelectPanel.DS.Active:=false;
//        FSelectPanel.DS.CreateDataSet;
        FSelectPanel.DS.EmptyDataSet;
        SelectedRows.First;
        while not SelectedRows.EOF do begin
          FSelectPanel.DS.InsertRecord([SelectedRows.Fields[0].Value, SelectedRows.Fields[1].Value]);
          SelectedRows.Next;
        end;
      finally
        FSelectPanel.DS.EnableControls;
      end;
    end;
  end;
end;

procedure TMakeSelect.Button3Click(Sender: TObject);
begin
  if not SelectedRows.Locate(SelectedRows.Fields[0].FieldName, DBLookupListBox1.KeyValue, [])
  then SelectedRows.InsertRecord([DBLookupListBox1.KeyValue, DBLookupListBox1.SelectedItem]);
  DBLookupListBox2.KeyValue:=DBLookupListBox1.KeyValue;
end;

procedure TMakeSelect.Button4Click(Sender: TObject);
var
  DV, OV: Variant;
begin
  if SelectedRows.IsEmpty then exit;
  DV:=DBLookupListBox2.KeyValue;
  OV:=Null;
  try
    SelectedRows.DisableControls;
    SelectedRows.Next;
    if SelectedRows.Fields[0].Value<>DV then OV:=SelectedRows.Fields[0].Value
    else begin
      SelectedRows.Prior;
      if SelectedRows.Fields[0].Value<>DV then OV:=SelectedRows.Fields[0].Value
    end;
  finally
    SelectedRows.EnableControls;
    if SelectedRows.Locate(SelectedRows.Fields[0].FieldName, DV, []) then SelectedRows.Delete;
  end;
  DBLookupListBox2.KeyValue:=OV;
end;

procedure TMakeSelect.Button6Click(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  SelectedRows.DisableControls;
  try
    SelectedRows.Active:=false;
    SelectedRows.CreateDataSet;
  finally
    Screen.Cursor:=crDefault;
    SelectedRows.EnableControls;
  end;
end;

procedure TMakeSelect.KBKDBGrid3KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_Insert) then Button3Click(nil);
end;

procedure TMakeSelect.ListBox2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_delete) then Button4Click(nil);
end;

procedure TMakeSelect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DBText1.DataSource:=nil;
  DBtext1.DataField:='';
end;

procedure TMakeSelect.DBLookupListBox1DblClick(Sender: TObject);
begin
  FKey:=VK_Insert;
  KBKDBGrid3KeyDown(nil,  FKey, []);
end;

procedure TMakeSelect.DBLookupListBox2DblClick(Sender: TObject);
begin
  FKey:=VK_Delete;
  ListBox2KeyDown(nil,  FKey, []);
end;

procedure TMakeSelect.FormActivate(Sender: TObject);
begin
  ActiveControl:=DBLookupListBox1;
end;

procedure TMakeSelect.FormCreate(Sender: TObject);
begin
  GroupBox2.Caption:=SFullListCaption;
  GroupBox1.Caption:=SSelectedItemsCaption;
  Button3.Hint:=SAddButtonHint;
  Button4.Hint:=SDeleteButtonHint;
  Button6.Hint:=SDeleteAllButtonHint;
  BitBtn2.Caption:=SCancelButtonCaption;
end;

end.
