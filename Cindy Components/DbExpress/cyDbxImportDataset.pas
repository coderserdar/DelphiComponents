unit cyDbxImportDataset;

{   Component(s):
    TcyDbxImportDataset

    Description:
    TcyDbxImportDataset allows
      - Create/ alter SQL table based on attached TDataset or directly filling Fields property.
      - Import records.



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

uses Classes, SysUtils, Db, DBClient, Provider, SqlExpr, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} cyDBX, cyDbxBaseImport, cyDbxBaseTable;

type
  TImportFieldsOption = (ifAggregate, ifCalculated, ifData, ifInternalCalc, ifLookup);
  TImportFieldsOptions = set of TImportFieldsOption;

  TcyDbxImportDataset = class(TcyDbxBaseImport)
  private
    FDataset: TDataset;
    FFieldsOptions: TImportFieldsOptions;
    procedure SetDataset(const Value: TDataset);
    procedure SetFieldsOptions(const Value: TImportFieldsOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateFields(ClearCurrentFields: Boolean); override;
    function IncludeField(aField: TField): Boolean;
    procedure ImportRecords(const MaxErrors: Integer = -1); override;
  published
    property Dataset: TDataset read FDataset write SetDataset;
    property FieldsOptions: TImportFieldsOptions read FFieldsOptions write SetFieldsOptions default [ifData];
  end;

implementation

{ TcyDbxImportDataset }
constructor TcyDbxImportDataset.Create(AOwner: TComponent);
begin
  inherited;
  FFieldsOptions := [ifData];
end;

destructor TcyDbxImportDataset.Destroy;
begin

  inherited;
end;

procedure TcyDbxImportDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if FDataset <> nil then
      if AComponent = FDataset then
        FDataset := nil;
  end;
end;

procedure TcyDbxImportDataset.SetDataset(const Value: TDataset);
begin
  FDataset := Value;

  if Value <> nil then
  begin
    FDataset.FreeNotification(Self);  // Inform TcyDbxBaseImport if component removed ...

    if not (csLoading in ComponentState) then
      GenerateFields(True);
  end;
end;

procedure TcyDbxImportDataset.SetFieldsOptions(const Value: TImportFieldsOptions);
begin
  FFieldsOptions := Value;
  GenerateFields(false);
end;

function TcyDbxImportDataset.IncludeField(aField: TField): Boolean;
begin
  Result := true;
  case aField.FieldKind of
    fkData:         Result := ifData in FFieldsOptions;
    fkCalculated:   Result := ifCalculated in FFieldsOptions;
    fkLookup:       Result := ifLookup in FFieldsOptions;
    fkInternalCalc: Result := ifInternalCalc in FFieldsOptions;
    fkAggregate:    Result := ifAggregate in FFieldsOptions;
  end;
end;

procedure TcyDbxImportDataset.GenerateFields(ClearCurrentFields: Boolean);
var
  f: Integer;
  Found: Integer;
begin
  if ClearCurrentFields then
    Fields.Clear;

  if not Assigned(FDataset) then Exit;

  for f := 0 to FDataset.FieldCount-1 do
  begin
    Found := Fields.FindFieldIndex(FDataset.Fields[f].FieldName);

    if IncludeField(FDataset.Fields[f]) then
    begin
      // Always delete to have fields ordered like on Dataset :
      if Found <> -1 then
        Fields.Delete(Found);

      with Fields.Add do
      begin
        FieldName := FDataset.Fields[f].FieldName;
        Origin := '';

        if ColumnExists(FDataset.Fields[f].FieldName) then
          Origin := FieldName;

        if AnsiUppercase(PrimaryKey) = AnsiUppercase(FieldName) then
          IndexType := itPrimaryKey;
        AutoIncremental := FDataset.Fields[f].AutoGenerateValue = arAutoInc;
        FieldType.DataType := FDataset.Fields[f].DataType;
        if FDataset.Fields[f] is TStringField then
          FieldType.Size := TStringField(FDataset.Fields[f]).Size;

        // Set Integer Size by DataType :
        case FieldType.DataType of
          ftAutoInc:  FieldType.Size := 10;
          ftSmallint: FieldType.Size :=  4;
          ftInteger:  FieldType.Size := 10;
          ftLargeint: FieldType.Size := 10;
          ftWord:     FieldType.Size := 10;
          ftFloat:    FieldType.Size := 14;
          ftCurrency: FieldType.Size := 14;
          ftFMTBcd:   FieldType.Size := 18;
          {$IFDEF UNICODE}
          ftShortint: FieldType.Size :=  4;
          ftByte:     FieldType.Size :=  3;
          ftLongWord: FieldType.Size := 10;
          ftExtended: FieldType.Size := 14;
          {$ENDIF}
        end;
      end;
    end
    else begin
      // Remove if field in the list :
      if Found <> -1 then
        Fields.Delete(Found);
    end;
  end;
end;

procedure TcyDbxImportDataset.ImportRecords(const MaxErrors: Integer);
var
  aQuery: TSQLQuery;
  aProvider: TDatasetProvider;
  aDBXTable: TClientDataset;
  f: Integer;
begin
  if not Assigned(FDataset) then
    raise Exception.Create('No Dataset specified !');

  if not Assigned(SQLConnection) then
    raise Exception.Create('No SQLConnection specified !');

  if TableName = '' then
    raise Exception.Create('No tablename specified !');

  if not FDataset.Active then
    Exit;

  // Create Query and provider :
  aQuery := TSQLQuery.Create(Self.Owner);
  aQuery.SQLConnection := SQLConnection;
  aQuery.SQL.Text := 'SELECT * FROM `' + TableName + '`';
  aQuery.Active := true;  // In order to use FieldByName() ...

  // Put required to false on AutoInc fields :
  if PrimaryKey <> '' then
    if Assigned(aQuery.FindField(PrimaryKey)) then
      aQuery.FieldByName(PrimaryKey).Required := false;

  for f := 0 to Fields.Count-1 do
    if Fields[f].AutoIncremental then
      aQuery.FieldByName(Fields[f].FieldName).Required := false;

  // Create the provider :
  aProvider := TDataSetProvider.Create(Self.Owner);
  aProvider.Name := 'dspImportDataset';
  aProvider.Dataset := aQuery;

  // Create a TcyDbxTable to import records :
  aDBXTable := TClientDataset.Create(Self.Owner);
  aDBXTable.OnReconcileError := ImportReconcileErrorEvent;
  aDBXTable.ProviderName := aProvider.Name;
  aDBXTable.Active := true;

  FDataset.First;
  while not FDataset.Eof do
  begin
    aDBXTable.Append;
    try
      {$IFDEF UNICODE}
      aDBXTable.CopyFields(FDataset);
      {$ELSE}
      for f := 0 to FDataset.FieldCount-1 do
        if Assigned(aQuery.FindField(FDataset.Fields[f].FieldName)) then
          aDBXTable.FieldByName(FDataset.Fields[f].FieldName).AsString := FDataset.Fields[f].AsString;
      {$ENDIF}

    finally

    end;
    aDBXTable.Post;

    FDataset.Next;
  end;

  if aDBXTable.ChangeCount > 0 then
    aDBXTable.ApplyUpdates(-1);      // Will post changes at once to server

  aDBXTable.Active := false;
  aDBXTable.Free;

  aProvider.Free;

  aQuery.Free;
end;

end.
