unit FieldPropFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ImgList, Db, TinyDB, ExtCtrls, BaseFrm;

type
  TFieldPropFormData = record
    TableName: string;
    CurFieldName: string;
  end;

  TFieldPropForm = class(TBaseForm)
    ImageList: TImageList;
    TinyTable: TTinyTable;
    BottomPanel: TPanel;
    OkButton: TButton;
    Panel2: TPanel;
    FieldListView: TListView;
    procedure BottomPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TFieldPropFormData);
  end;

var
  FieldPropForm: TFieldPropForm;

procedure ShowFieldPropForm(Value: TFieldPropFormData);

implementation

uses MainFrm, LangMgr;

{$R *.DFM}

procedure ShowFieldPropForm(Value: TFieldPropFormData);
var
  Frm: TFieldPropForm;
begin
  Frm := TFieldPropForm.Create(Application);
  Frm.SetData(Value);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TFieldPropForm.TransLanguage;
begin
  inherited;
  FieldListView.Columns[0].Caption := AppLangMgr.Trans('NO.');
  FieldListView.Columns[1].Caption := AppLangMgr.Trans('Field Name');
  FieldListView.Columns[2].Caption := AppLangMgr.Trans('Field Type');
  FieldListView.Columns[3].Caption := AppLangMgr.Trans('Field Size');
  FieldListView.Columns[4].Caption := AppLangMgr.Trans('Data Processing');
  FieldListView.Columns[5].Caption := AppLangMgr.Trans('Note');
end;

procedure TFieldPropForm.SetData(Value: TFieldPropFormData);
var
  DPModeStr: array[TFieldDataProcessMode] of string;
  ListItem: TListItem;
  I: Integer;
begin
  DPModeStr[fdDefault] := AppLangMgr.Trans('fdDefault');
  DPModeStr[fdOriginal] := AppLangMgr.Trans('fdOriginal');

  TinyTable.DatabaseName := MainForm.TinyDatabase.DatabaseName;
  TinyTable.TableName := Value.TableName;
  TinyTable.Open;
  for I := 0 to TinyTable.Fields.Count - 1 do
  begin
    ListItem := FieldListView.Items.Add;
    ListItem.Caption := IntToStr(I);
    ListItem.SubItems.Add(TinyTable.TableIO.FieldDefs[I].Name);
    ListItem.SubItems.Add(MainForm.GetFieldStrByType(TinyTable.TableIO.FieldDefs[I].FieldType));
    ListItem.SubItems.Add(IntToStr(TinyTable.TableIO.FieldDefs[I].FieldSize));
    ListItem.SubItems.Add(DPModeStr[TinyTable.TableIO.FieldDefs[I].DPMode]);
    ListItem.SubItems.Add(MainForm.GetFieldCommentsByType(TinyTable.TableIO.FieldDefs[I].FieldType));
    ListItem.ImageIndex := 0;
    if CompareText(Value.CurFieldName, TinyTable.TableIO.FieldDefs[I].Name) = 0 then
    begin
      ListItem.Selected := True;
      ListItem.MakeVisible(True);
    end;
  end;
  TinyTable.Close;
end;

procedure TFieldPropForm.BottomPanelResize(Sender: TObject);
begin
  OkButton.Left := BottomPanel.Width - OkButton.Width - 8;
end;

end.
