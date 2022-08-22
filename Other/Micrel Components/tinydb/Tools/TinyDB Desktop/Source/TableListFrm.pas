unit TableListFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  TinyDB, BaseFrm;

type
  TTableListFormData = record
    TinyDatabase: TTinyDatabase;
    TableName: string;
  end;

  TTableListForm = class(TBaseForm)
    TableNameEdit: TEdit;
    ListView: TListView;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TableNameEditChange(Sender: TObject);
    procedure TableNameEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FData: TTableListFormData;
    FTextChanging: Boolean;

    procedure FillListView;
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TTableListFormData);
    procedure GetData(var Value: TTableListFormData);
  end;

var
  TableListForm: TTableListForm;

function ShowTableListForm(var Value: TTableListFormData): Boolean;

implementation

uses DBInfoFrm, LangMgr;

{$R *.DFM}

function ShowTableListForm(var Value: TTableListFormData): Boolean;
var
  Frm: TTableListForm;
begin
  Frm := TTableListForm.Create(Application);
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TTableListForm.TransLanguage;
begin
  inherited;
  ListView.Columns[0].Caption := AppLangMgr.Trans('NO.');
  ListView.Columns[1].Caption := AppLangMgr.Trans('Table Name');
end;

procedure TTableListForm.SetData(Value: TTableListFormData);
begin
  FData := Value;
  FillListView;
end;

procedure TTableListForm.GetData(var Value: TTableListFormData);
begin
  Value.TableName := TableNameEdit.Text;
end;

procedure TTableListForm.FillListView;
var
  I: Integer;
  ListItem: TListItem;
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  for I := 0 to FData.TinyDatabase.TableDefs.Count - 1 do
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := IntToStr(I);
    ListItem.SubItems.Add(FData.TinyDatabase.TableDefs[I].Name);
    ListItem.ImageIndex := 0;
  end;
  ListView.Items.EndUpdate;
end;

function TTableListForm.CheckValid: Boolean;
var
  I: Integer;
  Exists: Boolean;
begin
  Result := True;
  Exists := False;
  for I := 0 to ListView.Items.Count - 1 do
  begin
    if CompareText(TableNameEdit.Text, ListView.Items[I].SubItems[0]) = 0 then
    begin
      Exists := True;
      Break;
    end;
  end;
  if not Exists then
  begin
    if TableNameEdit.Text <> '' then
      Application.MessageBox(PChar(AppLangMgr.Trans('Table name [%s] does not exist.', [TableNameEdit.Text])), PChar(Application.Title), 48);
    TableNameEdit.SetFocus;
    Result := False;
    Exit;
  end;
end;

procedure TTableListForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
    ModalResult := mrOk;
end;

procedure TTableListForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTableListForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if FTextChanging then Exit;
  if ListView.Selected = nil then Exit;
  TableNameEdit.Text := ListView.Selected.SubItems[0];
end;

procedure TTableListForm.TableNameEditChange(Sender: TObject);
var
  I: Integer;
begin
  FTextChanging := True;
  for I := 0 to ListView.Items.Count - 1 do
  begin
    if Pos(UpperCase(TableNameEdit.Text), UpperCase(ListView.Items[I].SubItems[0])) = 1 then
    begin
      ListView.Items[I].Selected := True;
      Break;
    end;
  end;
  FTextChanging := False;
end;

procedure TTableListForm.TableNameEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if ListView.Items.Count = 0 then Exit;
  if Key = VK_RETURN then
  begin
    if ListView.Selected <> nil then
    begin
      if Pos(UpperCase(TableNameEdit.Text), UpperCase(ListView.Selected.SubItems[0])) = 1 then
      begin
        TableNameEdit.Text := ListView.Selected.SubItems[0];
        OkButtonClick(nil);
      end;
    end
    else
    begin
      TableNameEdit.SetFocus;
    end;
  end
  else if Key = VK_UP then
  begin
    if ListView.Selected = nil then
      ListView.Items[0].Selected := True;
    if ListView.Selected.Index > 0 then
      ListView.Items[ListView.Selected.Index - 1].Selected := True;
    TableNameEdit.SelectAll;
    Key := 0;
  end
  else if Key = VK_DOWN then
  begin
    if ListView.Selected = nil then
      ListView.Items[0].Selected := True;
    if ListView.Selected.Index < ListView.Items.Count - 1 then
      ListView.Items[ListView.Selected.Index + 1].Selected := True;
    TableNameEdit.SelectAll;
    Key := 0;
  end;
end;

end.
