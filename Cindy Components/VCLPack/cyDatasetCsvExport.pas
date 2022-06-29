unit cyDatasetCsvExport;

{   Component(s):
    TcyDatasetCsvExport

    Description:
    Use a TcyDatasetCsvExport to export any dataset into a *.csv file



    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, sysUtils, Db, cyStrUtils;

type
  TTitlesMode = (tmDisplayLabel, tmFieldName, tmDontExport);

  TBeforeExportRecord = procedure (Sender: TObject; var ExportRecord: Boolean) of object;
  TOnDetermineFieldExport = procedure (Sender: TObject; const Field: TField; var ExportField: Boolean) of object;
  TOnCustomFieldExportRecord = procedure (Sender: TObject; const Field: TField; var FieldValue: String) of object;
  TOnCustomRecordExport = procedure (Sender: TObject; var MoreData: Boolean) of object;

  TcyDatasetCsvExport = class(TComponent)
  private
    FDataset: TDataset;
    FTTitlesMode: TTitlesMode;
    FBeforeExportRecord: TBeforeExportRecord;
    FAfterExportRecord: TNotifyEvent;
    FFilename: String;
    FUseGetFieldText: Boolean;
    FOnCustomFieldExportRecord: TOnCustomFieldExportRecord;
    FOnDetermineFieldExport: TOnDetermineFieldExport;
    FOnCustomRecordExport: TOnCustomRecordExport;
    fSeparation: Char;
    FAppendToExistingFile: Boolean;
    procedure SetDataset(const Value: TDataset);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoExportRecord: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(Destination: TStrings; AppendMode: Boolean; const fromCursor: Boolean); overload;
    procedure Execute(fromCursor: Boolean); overload;
    {$IFDEF UNICODE}
    procedure Execute(fromCursor: Boolean; aEncoding: TEncoding); overload;
    {$ENDIF}
  published
    property Dataset: TDataset read FDataset write SetDataset;
    property Filename: String read FFilename write FFilename;
    property Titles: TTitlesMode read FTTitlesMode write FTTitlesMode default tmDisplayLabel;
    property AppendToExistingFile: Boolean read FAppendToExistingFile write FAppendToExistingFile default false;
    property UseGetFieldText: Boolean read FUseGetFieldText write FUseGetFieldText default false;
    property Separation: Char read fSeparation write fSeparation default ';';
    property OnCustomRecordExport: TOnCustomRecordExport read FOnCustomRecordExport write FOnCustomRecordExport;
    property BeforeExportRecord: TBeforeExportRecord read FBeforeExportRecord write FBeforeExportRecord;
    property AfterExportRecord: TNotifyEvent read FAfterExportRecord write FAfterExportRecord;
    property OnDetermineFieldExport: TOnDetermineFieldExport read FOnDetermineFieldExport write FOnDetermineFieldExport;
    property OnCustomFieldExportRecord: TOnCustomFieldExportRecord read FOnCustomFieldExportRecord write FOnCustomFieldExportRecord;
  end;

implementation

{ TcyDatasetCsvExport }
constructor TcyDatasetCsvExport.Create(AOwner: TComponent);
begin
  inherited;
  FTTitlesMode := tmDisplayLabel;
  FAppendToExistingFile := false;
  FUseGetFieldText := false;
  fSeparation := ';';
end;

destructor TcyDatasetCsvExport.Destroy;
begin

  inherited;
end;

procedure TcyDatasetCsvExport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if FDataset <> nil then
      if AComponent = FDataset then
        FDataset := nil;
  end;
end;

procedure TcyDatasetCsvExport.SetDataset(const Value: TDataset);
begin
  FDataset := Value;

  if Value <> nil then
    FDataset.FreeNotification(Self);  // Inform TcyDatasetCsvExport if component removed ...
end;

procedure TcyDatasetCsvExport.Execute(Destination: TStrings; AppendMode: Boolean; const fromCursor: Boolean);
var
  f: Integer;
  ExportField, FirstField, ExportRecord: Boolean;
  array_exportfield: Array of Boolean;
  StrLine, CsvFieldValue: String;

        procedure CsvLineAdd(const aStr: String; const FirsCol: Boolean);
        begin
          if not FirsCol then
            StrLine := StrLine + fSeparation;

          StrLine := StrLine + cyStrUtils.StringToCsvCell(aStr, fSeparation);
        end;
begin
  if not Assigned(FDataset) then
    raise Exception.Create('TcyDatasetCsvExport.Dataset property not specified !');

  if not FDataset.Active then
    raise Exception.Create('TcyDatasetCsvExport.Dataset not Active !');

  try
    if AppendMode then
      if Destination.Count = 0 then
        AppendMode := false;

    Destination.BeginUpdate;

    if not AppendMode then
      Destination.Clear;

    if not Assigned(FOnCustomRecordExport) then
      if not fromCursor then
        FDataset.First;

    StrLine := '';
    SetLength(array_exportfield, FDataset.FieldCount);
    FirstField := true;

    for f := 0 to FDataset.FieldCount-1 do
    begin
      ExportField := true;
      if Assigned(FOnDetermineFieldExport) then
        FOnDetermineFieldExport(Self, FDataset.Fields[f], ExportField);

      array_exportfield[f] := ExportField;

      // * Export field title * //
      if array_exportfield[f] then
      begin
        case FTTitlesMode of
          tmDisplayLabel: CsvLineAdd(FDataset.Fields[f].DisplayLabel, FirstField);
          tmFieldName :   CsvLineAdd(FDataset.Fields[f].FieldName, FirstField);
        end;

        FirstField := false;
      end;
    end;

    if (StrLine <> '') and (not AppendMode) then
      Destination.Add(StrLine);


    // * Export records * //
    while DoExportRecord do
    begin
      ExportRecord := true;

      if Assigned(FBeforeExportRecord) then
        FBeforeExportRecord(Self, ExportRecord);

      if ExportRecord then
      begin
        StrLine := '';
        FirstField := true;

        for f := 0 to FDataset.FieldCount-1 do
          if array_exportfield[f] then
          begin
            if FDataset.Fields[f].DataType in [ftMemo, ftFmtMemo {$IFDEF UNICODE}, ftWideMemo{$ENDIF}] then
              CsvFieldValue := FDataset.Fields[f].AsString
            else
              if FUseGetFieldText
              then CsvFieldValue := FDataset.Fields[f].DisplayText
              else CsvFieldValue := FDataset.Fields[f].AsString;

            if Assigned(FOnCustomFieldExportRecord) then
              FOnCustomFieldExportRecord(Self, FDataset.Fields[f], CsvFieldValue);

            CsvLineAdd(CsvFieldValue, FirstField);
            FirstField := false;
          end;

        if StrLine <> '' then
          Destination.Add(StrLine);

        if Assigned(FAfterExportRecord) then
          FAfterExportRecord(Self);
      end;

      if not Assigned(FOnCustomRecordExport) then
        FDataset.Next;
    end;
  finally
    Destination.EndUpdate;
  end;
end;

function TcyDatasetCsvExport.DoExportRecord: Boolean;
var
  MoreData: Boolean;
begin
  if Assigned(FOnCustomRecordExport) then
  begin
    MoreData := true;
    FOnCustomRecordExport(Self, MoreData);
    Result := MoreData;
  end
  else
    Result := not FDataset.Eof;
end;

procedure TcyDatasetCsvExport.Execute(fromCursor: Boolean);
var
  aStrings: TStrings;
begin
  aStrings := TStringList.Create;

  try
    if FileExists(FFilename) and FAppendToExistingFile then
      aStrings.LoadFromFile(FFilename);

    Execute(aStrings, FAppendToExistingFile, fromCursor);
    aStrings.SaveToFile(FFilename);
  finally
    aStrings.Free;
  end;
end;

{$IFDEF UNICODE}
procedure TcyDatasetCsvExport.Execute(fromCursor: Boolean; aEncoding: TEncoding);
var
  aStrings: TStrings;
begin
  aStrings := TStringList.Create;

  try
    if FileExists(FFilename) and FAppendToExistingFile then
      aStrings.LoadFromFile(FFilename, aEncoding);

    Execute(aStrings, FAppendToExistingFile, fromCursor);
    aStrings.SaveToFile(FFilename, aEncoding);
  finally
    aStrings.Free;
  end;
end;
{$ENDIF}

end.
