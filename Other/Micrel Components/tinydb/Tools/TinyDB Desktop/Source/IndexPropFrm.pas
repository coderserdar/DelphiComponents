unit IndexPropFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Db, TinyDB, ImgList, BaseFrm;

type
  TIndexPropFormData = record
    TableName: string;
    CurIndexName: string;
  end;

  TIndexPropForm = class(TBaseForm)
    Panel1: TPanel;
    IndexListView: TListView;
    BottomPanel: TPanel;
    OkButton: TButton;
    ImageList: TImageList;
    TinyTable: TTinyTable;
    procedure BottomPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TIndexPropFormData);
  end;

var
  IndexPropForm: TIndexPropForm;

procedure ShowIndexPropForm(Value: TIndexPropFormData);

implementation

uses MainFrm, Misc, LangMgr;

{$R *.DFM}

procedure ShowIndexPropForm(Value: TIndexPropFormData);
var
  Frm: TIndexPropForm;
begin
  Frm := TIndexPropForm.Create(Application);
  Frm.SetData(Value);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TIndexPropForm.TransLanguage;
begin
  inherited;
  IndexListView.Columns[0].Caption := AppLangMgr.Trans('NO.');
  IndexListView.Columns[1].Caption := AppLangMgr.Trans('Index Name');
  IndexListView.Columns[2].Caption := AppLangMgr.Trans('Index Field');
  IndexListView.Columns[3].Caption := AppLangMgr.Trans('Primary');
  IndexListView.Columns[4].Caption := AppLangMgr.Trans('Unique');
  IndexListView.Columns[5].Caption := AppLangMgr.Trans('Case Sensitive');
  IndexListView.Columns[6].Caption := AppLangMgr.Trans('Sort Mode');
end;

procedure TIndexPropForm.SetData(Value: TIndexPropFormData);
var
  ListItem: TListItem;
  I, J, FieldIdx: Integer;
  IndexFields: string;
  YesStr, NoStr: string;
  AscStr, DescStr: string;
begin
  YesStr := AppLangMgr.Trans('Yes');
  NoStr := AppLangMgr.Trans('No');
  AscStr := AppLangMgr.Trans('Ascending');
  DescStr := AppLangMgr.Trans('Descending');

  TinyTable.DatabaseName := MainForm.TinyDatabase.DatabaseName;
  TinyTable.TableName := Value.TableName;
  TinyTable.Open;
  for I := 0 to TinyTable.TableIO.IndexDefs.Count - 1 do
  begin
    ListItem := IndexListView.Items.Add;
    ListItem.Caption := IntToStr(I);
    ListItem.SubItems.Add(TinyTable.TableIO.IndexDefs[I].Name);
    IndexFields := '';
    for J := 0 to High(TinyTable.TableIO.IndexDefs[I].FieldIdxes) do
    begin
      if J <> 0 then IndexFields := IndexFields + ',';
      FieldIdx := TinyTable.TableIO.IndexDefs[I].FieldIdxes[J];
      IndexFields := IndexFields + TinyTable.TableIO.FieldDefs[FieldIdx].Name;
    end;
    ListItem.SubItems.Add(IndexFields);
    ListItem.SubItems.Add(Iif(tiPrimary in TinyTable.TableIO.IndexDefs[I].Options, YesStr, NoStr));
    ListItem.SubItems.Add(Iif(tiUnique in TinyTable.TableIO.IndexDefs[I].Options, YesStr, NoStr));
    ListItem.SubItems.Add(Iif(tiCaseInsensitive in TinyTable.TableIO.IndexDefs[I].Options, NoStr, YesStr));
    ListItem.SubItems.Add(Iif(tiDescending in TinyTable.TableIO.IndexDefs[I].Options, DescStr, AscStr));
    ListItem.ImageIndex := 0;
    if CompareText(Value.CurIndexName, TinyTable.TableIO.IndexDefs[I].Name) = 0 then
    begin
      ListItem.Selected := True;
      ListItem.MakeVisible(True);
    end;
  end;
  TinyTable.Close;
end;

procedure TIndexPropForm.BottomPanelResize(Sender: TObject);
begin
  OkButton.Left := BottomPanel.Width - OkButton.Width - 8;
end;

end.
