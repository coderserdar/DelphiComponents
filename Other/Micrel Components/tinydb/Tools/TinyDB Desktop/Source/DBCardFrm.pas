unit DBCardFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, DBTableFrm, Db, TinyDB,
  DBCtrls, ComCtrls, StdCtrls, ExtCtrls, ToolWin,
  BlobDataFrm;

type
  TDBCardForm = class(TDBTableForm)
    ScrollBox: TScrollBox;
    BottomPanel: TPanel;
    RecNoScrollBar: TScrollBar;
    RecNoPanel: TPanel;
    procedure RecNoScrollBarChange(Sender: TObject);
    procedure TinyTableAfterScroll(DataSet: TDataSet);
    procedure TinyTableAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure InitControls;
    procedure BlobButtonClick(Sender: TObject);
  public
    { Public declarations }
    procedure SetData(Value: TDBTableFormData); override;
  end;

var
  DBCardForm: TDBCardForm;

procedure ShowDBCardForm(Value: TDBTableFormData);

implementation

uses MainFrm, LangMgr;

{$R *.DFM}

procedure ShowDBCardForm(Value: TDBTableFormData);
var
  Frm: TDBTableForm;
begin
  Frm := TDBCardForm.Create(Application);
  Frm.SetData(Value);
  Frm.Show;
end;

procedure TDBCardForm.SetData(Value: TDBTableFormData);
begin
  inherited;
  InitControls;
  if TinyTable.RecordCount > 0 then
  begin
    RecNoScrollBar.Max := TinyTable.RecordCount;
    RecNoScrollBar.Min := 1;
    RecNoScrollBar.Enabled := True;
  end
  else
  begin
    RecNoScrollBar.Min := 0;
    RecNoScrollBar.Max := 0;
    RecNoScrollBar.Enabled := False;
  end;
  RecNoPanel.Caption := IntToStr(TinyTable.RecNo) + '/' + IntToStr(TinyTable.RecordCount);
end;

procedure TDBCardForm.InitControls;

  function GetFieldNameMaxWidth: Integer;
  var
    I, Count: Integer;
  begin
    Count := 0;
    for I := 0 to TinyTable.FieldCount - 1 do
    begin
      if Count < Length(TinyTable.Fields[I].FieldName) then
        Count := Length(TinyTable.Fields[I].FieldName);
    end;
    Result := Canvas.TextWidth('A') * Count;
  end;

  function GetMemoCount: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to TinyTable.FieldCount - 1 do
      if TinyTable.Fields[I].DataType = ftMemo then
        Result := Result + 1;
  end;

var
  FieldType: TFieldType;
  FieldIdx: Integer;
  FldNameWidth: Integer;
  TempFldCount: Integer;
  MemoCount: Integer;
begin
  MemoCount := 0;
  TempFldCount := TinyTable.FieldCount;
  if TempFldCount = 0 then TempFldCount := 1;

  //ClientHeight := TempFldCount * 25 + 10 + ToolBar.Height + BottomPanel.Height;
  ClientHeight := (TempFldCount + GetMemoCount) * 25 + 10 + ToolBar.Height + BottomPanel.Height;
  if ClientHeight > MainForm.ClientHeight - 30 then
    ClientHeight := MainForm.ClientHeight - 30;

  FldNameWidth := GetFieldNameMaxWidth + 25;
  for FieldIdx := 0 to TinyTable.FieldCount - 1 do
  begin
    // Print the name of the fields
    with TLabel.Create(Self) do
    begin
      Parent := ScrollBox;
      Left := 10;
      Top := FieldIdx * 25 + 10;
      Height := 20;
      Caption := TinyTable.Fields[FieldIdx].FieldName;
    end;
    // Print the type name of the fields
    with TLabel.Create(Self) do
    begin
      Parent := ScrollBox;
      Left := ScrollBox.ClientWidth - 70;
//    Top := FieldIdx * 25 + 10;
      Top := (FieldIdx + MemoCount) * 25 + 10;
      Height := 20;
      Caption := '(' + MainForm.GetFieldStrByType(TinyTable.Fields[FieldIdx].DataType) + ')';
    end;
    FieldType := TinyTable.Fields[FieldIdx].DataType;
    // Draw the controls that homo-logous the fields
    case FieldType of
      ftBoolean:
        begin
          with TDBCheckBox.Create(Self) do
          begin
            Parent := ScrollBox;
            Left := FldNameWidth;
//          Top := FieldIdx * 25 + 6;
            Top := (FieldIdx + MemoCount) * 25 + 6;
            Height := 20;
            DataSource := Self.DataSource;
            DataField := TinyTable.Fields[FieldIdx].FieldName;
          end;
        end;
      ftMemo:
        begin
          with TDBMemo.Create(Self) do
          begin
            Parent := ScrollBox;
            Left := FldNameWidth;
            Top := (FieldIdx + MemoCount) * 25 + 6;
            Height := 45;
            Width := ScrollBox.ClientWidth - Left - 80;
            ScrollBars := ssVertical;
            DataSource := Self.DataSource;
            DataField := TinyTable.Fields[FieldIdx].FieldName;
            MemoCount := MemoCount + 1;
          end;
        end;
      ftGraphic, ftBlob:
        begin
          with TButton.Create(Self) do
          begin
            Parent := ScrollBox;
            Left := FldNameWidth;
            Top := (FieldIdx + MemoCount) * 25 + 6;
            Height := 20;
            Width := 80;
            Caption := AppLangMgr.Trans('Details...');
            Tag := FieldIdx;
            OnClick := BlobButtonClick;
          end;
        end;
    else
      begin
        with TDBEdit.Create(Self) do
        begin
          Parent := ScrollBox;
          Left := FldNameWidth;
          Top := (FieldIdx + MemoCount) * 25 + 6;
          Height := 20;
          Width := ScrollBox.ClientWidth - Left - 80;
          DataSource := Self.DataSource;
          DataField := TinyTable.Fields[FieldIdx].FieldName;
        end;
      end;
    end;
  end;
end;

procedure TDBCardForm.BlobButtonClick(Sender: TObject);
var
  FieldIdx: Integer;
  FBlobDataForm: TBlobDataForm;
  Value: TBlobDataFormData;
begin
  FieldIdx := (Sender as TButton).Tag;
  FBlobDataForm := TBlobDataForm.Create(nil);
  try
    Value.TinyTable := TinyTable;
    Value.DataSource := DataSource;
    FBlobDataForm.SetData(Value);
    FBlobDataForm.SetCurFieldIdx(FieldIdx);
    FBlobDataForm.ShowModal;
  finally
    FBlobDataForm.Free;
  end;
end;

procedure TDBCardForm.RecNoScrollBarChange(Sender: TObject);
begin
  TinyTable.RecNo := RecNoScrollBar.Position;
end;

procedure TDBCardForm.TinyTableAfterScroll(DataSet: TDataSet);
begin
  inherited;
  RecNoPanel.Caption := IntToStr(TinyTable.RecNo) + '/' + IntToStr(TinyTable.RecordCount);
  RecNoScrollBar.Position := TinyTable.RecNo;
end;

procedure TDBCardForm.TinyTableAfterPost(DataSet: TDataSet);
begin
  inherited;
  RecNoPanel.Caption := IntToStr(TinyTable.RecNo) + '/' + IntToStr(TinyTable.RecordCount);

  if TinyTable.RecordCount > 0 then
  begin
    RecNoScrollBar.Max := TinyTable.RecordCount;
    RecNoScrollBar.Min := 1;
    RecNoScrollBar.Enabled := True;
  end
  else
  begin
    RecNoScrollBar.Min := 0;
    RecNoScrollBar.Max := 0;
    RecNoScrollBar.Enabled := False;
  end;
  RecNoScrollBar.Position := TinyTable.RecNo;
end;

end.
