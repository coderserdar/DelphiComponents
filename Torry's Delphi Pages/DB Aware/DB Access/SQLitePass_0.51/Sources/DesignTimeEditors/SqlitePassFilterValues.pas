unit SqlitePassFilterValues;
{$i ..\..\Sources\SqlitePassDbo.inc}
interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, Buttons, SqlitePassDbo, Db;

type
  TSqlitePassFilterValuesDlg = class(TForm)
    PanelIndexApplyChanges: TPanel;
    SbCancel: TSpeedButton;
    SbOk: TSpeedButton;
    PanelCaptionAvailableIFields: TPanel;
    Image5: TImage;
    CheckListBoxValues: TCheckListBox;
    SbClearAll: TSpeedButton;
    SbSelectAll: TSpeedButton;
    procedure SbCancelClick(Sender: TObject);
    procedure SbOkClick(Sender: TObject);
    procedure CheckListBoxValuesDblClick(Sender: TObject);
    procedure SbClearAllClick(Sender: TObject);
    procedure SbSelectAllClick(Sender: TObject);
  private
    FLocateFilterText: String;
    FFilterText: String;
    MyDataset: TSqlitePassDataset;
    SelectedField: TField;
    procedure ShowFieldValues;
    procedure CheckItems(CheckedState: Boolean);
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset; Field: TField; LocateFilter: String); reintroduce;
    property FilterText: String read FFilterText;
  end;

var
  SqlitePassFilterValuesDlg: TSqlitePassFilterValuesDlg;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}


constructor TSqlitePassFilterValuesDlg.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset; Field: TField; LocateFilter: String);
var
//SelectStmt,
FromStmt, NewSQL: String;

begin
  Inherited Create(AOwner);
  FFilterText := '';
  FLocateFilterText := LocateFilter;
  MyDataset := TSqlitePassDataset.Create(Self);
  MyDataset.Database := Dataset.Database;
  if Dataset.DatasetName <> ''
     then MyDataset.DatasetName := Dataset.DatasetName
     else MyDataset.SQL.Text := Dataset.SQL.Text;
  MyDataset.ReadOnly := True; { Necessary to accept 'Distinct' }
  { Change query to get only distinct values from table }
  MyDataset.SQLSelectStmt.Tokenizer.Text := MyDataset.SQL.Text;
  if MyDataset.SQLSelectStmt.Tokenizer.Locate('from') then
        begin
        //SelectStmt := MyDataset.SQLSelectStmt.Tokenizer.GetTextBefore(ttUnknown);
        FromStmt := MyDataset.SQLSelectStmt.Tokenizer.GetTextAfter(ttUnknown);
        NewSQL := 'select distinct "'+Field.FieldName+'" from ' + FromStmt;
        MyDataset.SQL.Text := NewSQL;
        end;
  Try
  MyDataset.SortedBy := '"' + Field.FieldName + '" ASC;';
  MyDataset.Open;
  Except
   try
   MyDataset.SQL.Text := Dataset.SQL.Text;
   MyDataset.Open;
   Except
   MyDataset.Close;
   end;
  end;
  SelectedField := MyDataset.FieldByName(Field.FieldName);
  ShowFieldValues;
end;

procedure TSqlitePassFilterValuesDlg.SbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSqlitePassFilterValuesDlg.SbOkClick(Sender: TObject);
var
i: Integer;
QuoteChar: String;
begin
FFilterText := '';
  for i := 0 to Pred(CheckListBoxValues.Items.Count) do
      begin
      if CheckListBoxValues.State[i] = cbChecked then
         begin
         QuoteChar := '';
         Case SelectedField.Datatype of
              ftString, ftFixedChar, ftWideString: QuoteChar := '"';
              ftDateTime, ftDate, ftTime: QuoteChar := '#';
              end;
         FFilterText := FFilterText + QuoteChar + CheckListBoxValues.Items[i] + QuoteChar + ',';
         end;
      end;
  if FFilterText <> ''
     then Delete(FFilterText, Length(FFilterText), 1);
  ModalResult := mrOk;
end;

procedure TSqlitePassFilterValuesDlg.ShowFieldValues;
var
i: Integer;
StrValue: String;
begin
  Try
  CheckListBoxValues.Clear;
  MyDataset.DisableControls;
  MyDataset.Filters.InitActiveFilters;
  MyDataset.First;
  while Not MyDataset.Eof do
        begin
        StrValue := '';
        if Not SelectedField.IsNull then
           begin
           Case SelectedField.Datatype of
             ftDateTime: StrValue := MyDataset.Database.Translator.DateTimeToStr(SelectedField.Value);
             ftDate    : StrValue := MyDataset.Database.Translator.DateToStr(SelectedField.Value);
             ftTime    : StrValue := MyDataset.Database.Translator.TimeToStr(SelectedField.Value);
             else StrValue := SelectedField.AsString;
             end;
            CheckListBoxValues.Items.Add(StrValue);
           end;
        MyDataset.Next;
        end;
  if MyDataset.Locate(FLocateFilterText, [loCaseInsensitive]) then
     begin
     For i := 0 to Pred(MyDataset.LocateFilters.Results.Count)
       do CheckListBoxValues.Checked[MyDataset.LocateFilters.Results[i]] := True;
     end;   
  finally
  MyDataset.EnableControls;
  end;
end;

procedure TSqlitePassFilterValuesDlg.CheckListBoxValuesDblClick(
  Sender: TObject);
begin
 CheckListBoxValues.Checked[CheckListBoxValues.ItemIndex] := Not CheckListBoxValues.Checked[CheckListBoxValues.ItemIndex];
end;

procedure TSqlitePassFilterValuesDlg.SbClearAllClick(Sender: TObject);
begin
CheckItems(False);
end;

procedure TSqlitePassFilterValuesDlg.SbSelectAllClick(Sender: TObject);
begin
CheckItems(True);
end;

procedure TSqlitePassFilterValuesDlg.CheckItems(CheckedState: Boolean);
var
i: Integer;
begin
For i := 0 to Pred(CheckListBoxValues.Items.Count)
    do CheckListBoxValues.checked[i] := CheckedState;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassFilterValues.lrs}
 {$ENDIF}
end.
