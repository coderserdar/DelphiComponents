unit BlobDataFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, DBCtrls,
  ExtCtrls, Db, TinyDB, BaseFrm;

type
  TBlobDataFormData = record
    TinyTable: TTinyTable;
    DataSource: TDataSource;
  end;

  TBlobDataForm = class(TBaseForm)
    BlobPanel: TPanel;
    BottomPanel: TPanel;
    CloseButton: TButton;
    procedure BottomPanelResize(Sender: TObject);
  private
    { Private declarations }
    FData: TBlobDataFormData;
  public
    { Public declarations }
    function IsBinary(var ABlob: AnsiString): Boolean;

    procedure SetData(Value: TBlobDataFormData);
    procedure SetCurFieldIdx(Value: Integer);
  end;

var
  BlobDataForm: TBlobDataForm;

implementation

uses LangMgr;

{$R *.DFM}

procedure TBlobDataForm.SetData(Value: TBlobDataFormData);
var
  I, J: Integer;
  S, P: AnsiString;
begin
  FData := Value;
  for I := 0 to Value.TinyTable.Fields.Count - 1 do
  begin
    if Value.TinyTable.Fields[I].IsBlob then
    begin
      case Value.TinyTable.Fields[I].DataType of
        ftGraphic:
          with TDBImage.Create(Self) do
          begin
            Parent := BlobPanel;
            Align := alClient;
            DataSource := Value.DataSource;
            DataField := Value.TinyTable.Fields[I].FieldName;
            Name := 'Blob' + IntToStr(I);
          end;
      else
        S := Value.TinyTable.Fields[I].{$IFDEF UNICODE}AsAnsiString{$ELSE}AsString{$ENDIF};
        if IsBinary(S) then
          with TMemo.Create(Self) do
          begin
            Parent := BlobPanel;
            Align := alClient;
            Name := 'Blob' + IntToStr(I);
            Font.Name := 'Courier New';
            Lines.Clear;

            while S <> '' do
            begin
              P := System.Copy(S, 1, 32);
              System.Delete(S, 1, 32);
              for J := 1 to Length(P) do
                if P[J] < ' ' then P[J] := '.';
              Lines.Add(string(P));
            end;
            ReadOnly := True;

            ScrollBars := ssVertical;
          end
        else
          with TDBMemo.Create(Self) do
          begin
            Parent := BlobPanel;
            Align := alClient;
            Name := 'Blob' + IntToStr(I);
            Lines.Clear;
            DataSource := Value.DataSource;
            DataField := Value.TinyTable.Fields[I].FieldName;
            ScrollBars := ssVertical;
          end;
      end;
    end;
  end;
end;

function TBlobDataForm.IsBinary(var ABlob: AnsiString): Boolean;
var
  J: Integer;
begin
  Result := False;
  for J := 1 to Length(ABlob) do
    if (ABlob[J] < ' ') and not (ABlob[J] in [#13, #10, #8, #26]) then
    begin
      Result := True; //is binary
      Exit
    end;
end;

procedure TBlobDataForm.SetCurFieldIdx(Value: Integer);
begin
  (FindComponent('Blob' + IntToStr(Value)) as TControl).BringToFront;
  Caption := FData.TinyTable.Fields[Value].FieldName;
end;

procedure TBlobDataForm.BottomPanelResize(Sender: TObject);
begin
  CloseButton.Left := BottomPanel.ClientWidth - CloseButton.Width - 6;
end;

end.
