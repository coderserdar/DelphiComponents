unit SearchFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, TinyDB, Db, BaseFrm, Variants;

type
  TSearchFormData = record
    TinyTable: TTinyTable;
  end;

  TSearchForm = class(TBaseForm)
    PageControl: TPageControl;
    LocateTabSheet: TTabSheet;
    OkButton: TButton;
    CancelButton: TButton;
    FindTabSheet: TTabSheet;
    GotoKeyTabSheet: TTabSheet;
    LocateFieldComboBox: TComboBox;
    Label1: TLabel;
    LocateValueEdit: TEdit;
    LocateAddButton: TButton;
    LocateConListView: TListView;
    LocateCaseInsCheckBox: TCheckBox;
    LocatePartCheckBox: TCheckBox;
    LocateClearButton: TButton;
    LocateDeleteButton: TButton;
    LocateReplaceButton: TButton;
    FindFieldComboBox: TComboBox;
    FindValueEdit: TEdit;
    FindAddButton: TButton;
    FindOprComboBox: TComboBox;
    FindConMemo: TMemo;
    FindModeRadioGroup: TRadioGroup;
    GotoKeyScrollBox: TScrollBox;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure LocateAddButtonClick(Sender: TObject);
    procedure LocateReplaceButtonClick(Sender: TObject);
    procedure LocateDeleteButtonClick(Sender: TObject);
    procedure LocateClearButtonClick(Sender: TObject);
    procedure FindAddButtonClick(Sender: TObject);
  private
    { Private declarations }
    FData: TSearchFormData;

    function GetTinyTable: TTinyTable;
    procedure Init;
    procedure InitLocate;
    procedure InitFind;
    procedure InitGotoKey;
    procedure FillFieldComboBox(ComboBox: TComboBox);
    function DoLocate: Boolean;
    function DoFind: Boolean;
    function DoGotoKey: Boolean;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TSearchFormData);
    property TinyTable: TTinyTable read GetTinyTable;
  end;

var
  SearchForm: TSearchForm;

function ShowSearchForm(Value: TSearchFormData): Boolean;

implementation

uses LangMgr;

{$R *.DFM}

function ShowSearchForm(Value: TSearchFormData): Boolean;
begin
  SearchForm.SetData(Value);
  Result := SearchForm.ShowModal = mrOk;
end;

procedure TSearchForm.TransLanguage;
begin
  inherited;
  FindModeRadioGroup.Items[0] := AppLangMgr.Trans('Find first');
  FindModeRadioGroup.Items[1] := AppLangMgr.Trans('Find next');
  FindModeRadioGroup.Items[2] := AppLangMgr.Trans('Find last');
  FindModeRadioGroup.Items[3] := AppLangMgr.Trans('Find prior');
end;

procedure TSearchForm.SetData(Value: TSearchFormData);
begin
  FData := Value;
  Init;
end;

function TSearchForm.GetTinyTable: TTinyTable;
begin
  Result := FData.TinyTable;
end;

procedure TSearchForm.Init;
begin
  InitLocate;
  InitFind;
  InitGotoKey;
end;

procedure TSearchForm.InitLocate;
begin
  FillFieldComboBox(LocateFieldComboBox);
end;

procedure TSearchForm.InitFind;
begin
  FillFieldComboBox(FindFieldComboBox);
  FindOprComboBox.ItemIndex := 0;
end;

procedure TSearchForm.InitGotoKey;
var
  I, Idx, FieldIdx: Integer;
begin
  for I := 0 to GotoKeyScrollBox.ControlCount - 1 do
    GotoKeyScrollBox.Controls[0].Free;

  if TinyTable.IndexIdx = -1 then
  begin
    with TLabel.Create(Self) do
    begin
      Parent := GotoKeyScrollBox;
      Left := 10;
      Top := 10;
      Caption := AppLangMgr.Trans('No index, can not search.');
    end;
    Exit;
  end;

  for Idx := 0 to High(TinyTable.TableIO.IndexDefs[TinyTable.IndexIdx].FieldIdxes) do
  begin
    FieldIdx := TinyTable.TableIO.IndexDefs[TinyTable.IndexIdx].FieldIdxes[Idx];
    with TLabel.Create(Self) do
    begin
      Parent := GotoKeyScrollBox;
      Left := 10;
      Top := Idx * 25 + 15;
      Caption := TinyTable.Fields[FieldIdx].FieldName;
    end;
    with TLabel.Create(Self) do
    begin
      Parent := GotoKeyScrollBox;
      Left := 80;
      Top := Idx * 25 + 15;
      Caption := '=';
    end;
    with TEdit.Create(Self) do
    begin
      Parent := GotoKeyScrollBox;
      Left := 110;
      Top := Idx * 25 + 10;
      Height := 20;
      Width := 200;
      Name := 'GotoKeyValue' + IntToStr(Idx);
      Text := '';
    end;
  end;
end;

procedure TSearchForm.FillFieldComboBox(ComboBox: TComboBox);
var
  I: Integer;
begin
  ComboBox.Items.Clear;
  for I := 0 to TinyTable.Fields.Count - 1 do
    ComboBox.Items.Add(TinyTable.Fields[I].FieldName);
  if ComboBox.Items.Count > 0 then
    ComboBox.ItemIndex := 0;
end;

function TSearchForm.DoLocate: Boolean;
var
  KeyFields: string;
  KeyValues: Variant;
  Options: TLocateOptions;
  I, Count: Integer;
begin
  Result := False;
  Count := LocateConListView.Items.Count;
  if Count = 0 then Exit;
  KeyValues := VarArrayCreate([0, Count - 1], varVariant);
  for I := 0 to Count - 1 do
  begin
    if I > 0 then KeyFields := KeyFields + ';';
    KeyFields := KeyFields + LocateConListView.Items[I].Caption;
    KeyValues[I] := LocateConListView.Items[I].SubItems[0];
  end;
  Options := [];
  if LocateCaseInsCheckBox.Checked then Options := Options + [loCaseInsensitive];
  if LocatePartCheckBox.Checked then Options := Options + [loPartialKey];

  Result := TinyTable.Locate(KeyFields, KeyValues, Options);
end;

function TSearchForm.DoFind: Boolean;
begin
  TinyTable.Filter := FindConMemo.Lines.Text;
  case FindModeRadioGroup.ItemIndex of
    0: TinyTable.FindFirst;
    1: TinyTable.FindNext;
    2: TinyTable.FindLast;
    3: TinyTable.FindPrior;
  end;
  Result := TinyTable.Found;
end;

function TSearchForm.DoGotoKey: Boolean;
var
  Idx, FieldIdx, Count: Integer;
begin
  Count := Length(TinyTable.TableIO.IndexDefs[TinyTable.IndexIdx].FieldIdxes);

  TinyTable.SetKey;
  for Idx := 0 to Count - 1 do
  begin
    FieldIdx := TinyTable.TableIO.IndexDefs[TinyTable.IndexIdx].FieldIdxes[Idx];
    TinyTable.Fields[FieldIdx].AsString := (FindComponent('GotoKeyValue' + IntToStr(Idx)) as TEdit).Text;
  end;
  Result := TinyTable.GotoKey;
end;

procedure TSearchForm.OkButtonClick(Sender: TObject);
var
  Found: Boolean;
begin
  Found := False;
  Screen.Cursor := crHourGlass;
  if PageControl.ActivePage = LocateTabSheet then
    Found := DoLocate
  else if PageControl.ActivePage = FindTabSheet then
    Found := DoFind
  else if PageControl.ActivePage = GotoKeyTabSheet then
    Found := DoGotoKey;
  Screen.Cursor := crDefault;

  if not Found then
    Application.MessageBox(PChar(AppLangMgr.Trans('Record not found.')), PChar(Application.Title), 48)
  else
    ModalResult := mrOk;
end;

procedure TSearchForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSearchForm.LocateAddButtonClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  if LocateFieldComboBox.ItemIndex = -1 then Exit;
  ListItem := LocateConListView.Items.Add;
  ListItem.Caption := LocateFieldComboBox.Text;
  ListItem.SubItems.Add(LocateValueEdit.Text);
  ListItem.Selected := True;
  LocateValueEdit.Text := '';
  LocateValueEdit.SetFocus;
end;

procedure TSearchForm.LocateReplaceButtonClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  if LocateFieldComboBox.ItemIndex = -1 then Exit;
  if LocateConListView.Selected = nil then Exit;
  ListItem := LocateConListView.Selected;
  ListItem.Caption := LocateFieldComboBox.Text;
  ListItem.SubItems[0] := LocateValueEdit.Text;
  ListItem.Selected := True;
  LocateValueEdit.Text := '';
  LocateValueEdit.SetFocus;
end;

procedure TSearchForm.LocateDeleteButtonClick(Sender: TObject);
begin
  if LocateConListView.Selected = nil then Exit;
  LocateConListView.Selected.Delete;
end;

procedure TSearchForm.LocateClearButtonClick(Sender: TObject);
begin
  LocateConListView.Items.Clear;
end;

procedure TSearchForm.FindAddButtonClick(Sender: TObject);
var
  S: string;
  FieldType: TFieldType;
begin
  if FindFieldComboBox.ItemIndex = -1 then Exit;
  if FindOprComboBox.ItemIndex = -1 then Exit;

  FieldType := TinyTable.Fields[FindFieldComboBox.ItemIndex].DataType;
  S := FindFieldComboBox.Text + FindOprComboBox.Text;
  if FieldType in [ftString, ftFixedChar, ftWideString, ftMemo, ftGraphic, ftBlob, ftFmtMemo, ftDate, ftTime, ftDateTime] then
    S := S + '''' + FindValueEdit.Text + ''''
  else
    S := S + FindValueEdit.Text;

  if FindConMemo.Lines.Text = '' then
    FindConMemo.Lines.Add(S)
  else
    FindConMemo.Lines.Add('AND ' + S);

  FindValueEdit.Text := '';
  FindValueEdit.SetFocus;
end;

end.
