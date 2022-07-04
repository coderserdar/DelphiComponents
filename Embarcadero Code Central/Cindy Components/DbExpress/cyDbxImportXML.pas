unit cyDbxImportXML;

{   Component(s):
    TcyDbxImportXML

    Description:
    TcyDbxImportXML allows
      - Create/ alter SQL table based on attached XML file or directly filling Fields property.
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

interface

uses Classes, SysUtils, Db, DBClient, Provider, SqlExpr, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} cyDBX, cyDbxBaseImport, XMLDoc, XMLIntf, Variants, Dialogs;

type
  TcyDbxImportXML = class(TcyDbxBaseImport)
  private
    FXMLFile: String;
    FTagRecord: string;
    FTagTable: string;
  protected
  public
    procedure GenerateFields(ClearCurrentFields: Boolean); override;
    procedure ImportRecords(const MaxErrors: Integer = -1); override;
  published
    property XMLFile: String read FXMLFile write FXMLFile;
    property TagTable: string read FTagTable write FTagTable;
    property TagRecord: string read FTagRecord write FTagRecord;
  end;

implementation

{ TcyDbxImportDataset }

procedure TcyDbxImportXML.GenerateFields(ClearCurrentFields: Boolean);
begin
  // XML schemas not handled for now
end;

procedure TcyDbxImportXML.ImportRecords(const MaxErrors: Integer);
var
  aQuery: TSQLQuery;
  aProvider: TDatasetProvider;
  aDBXTable: TClientDataset;
  r, f: Integer;

  aXmlDocument: TXmlDocument;
  NodeTable: IXMLNode;

      procedure AssignFieldValue(const aFieldName: string; const aFieldValue: OleVariant);
      begin
        if aFieldValue = Null then Exit;

        if Assigned(aQuery.FindField(aFieldName)) then
          aDBXTable.FieldByName(aFieldName).AsString := aFieldValue;
      end;

      function Find_NodeTable(fromList: IXMLNodeList): IXMLNode;
      var
        i: Integer;
      begin
        for i := 0 to fromList.Count-1 do
          if fromList[i].NodeName = FTagTable then
          begin
            Result := fromList[i];
            Break;
          end
          else begin
            Result := Find_NodeTable(fromList[i].ChildNodes);

            if Result <> Nil then
              Break;
          end;
      end;

      procedure ImportFieldsFromNode(const FieldPrefix: String; FromNode: IXMLNode);
      var
        i: Integer;
        FieldName: String;
        FieldValue: OleVariant;
      begin
        // Import current record field values :
        if FromNode.HasChildNodes then
          for i := 0 to FromNode.ChildNodes.Count-1 do    // Handling fields ...
          begin
            FieldName := FieldPrefix + FromNode.ChildNodes[i].NodeName;

            if FromNode.ChildNodes[i].ChildNodes.Count > 1 then   // New field structure !
            begin
              ImportFieldsFromNode(FieldName + '_', FromNode.ChildNodes[i]);
            end
            else begin
              FieldValue := FromNode.ChildNodes[i].NodeValue;
              AssignFieldValue(FieldName, FieldValue);
            end;
          end;
      end;

begin
  if FXMLFile = '' then
    raise Exception.Create('No XML file specified !');

  if FTagTable = '' then
    raise Exception.Create('No tag table specified !');

  if FTagRecord = '' then
    raise Exception.Create('No tag record specified !');

  if not Assigned(SQLConnection) then
    raise Exception.Create('No SQLConnection specified !');

  if TableName = '' then
    raise Exception.Create('No tablename specified !');

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


  aXmlDocument := TXmlDocument.Create(Self.Owner);  // Need Owner, otherwise we have an error !!!
  aXmlDocument.Options := [doNodeAutoCreate,doAttrNull,doAutoPrefix,doNamespaceDecl];
  aXmlDocument.ParseOptions := [];
  aXMLDocument.NodeIndentStr := '<1 space>';

  aXMLDocument.FileName := FXMLFile;

  try
    aXMLDocument.Active := true;

    if aXMLDocument.ChildNodes.Count <> 0 then
    begin
      NodeTable := Nil;

      // NodeTable := aXMLDocument.ChildNodes.FindNode(FTagTable);
      NodeTable := Find_NodeTable(aXMLDocument.ChildNodes);              // Case sensitive !

      if NodeTable <> Nil then
      begin
        for r := 0 to NodeTable.ChildNodes.Count-1 do
          if NodeTable.ChildNodes[r].NodeName = FTagRecord then          // Case sensitive !
          begin
            aDBXTable.Append;
            ImportFieldsFromNode('', NodeTable.ChildNodes[r]);
            aDBXTable.Post;
          end;
      end;
    end;
  finally
    aXMLDocument.Free;
  end;

  if aDBXTable.ChangeCount > 0 then
    aDBXTable.ApplyUpdates(-1);      // Will post changes at once to server

  aDBXTable.Active := false;
  aDBXTable.Free;

  aProvider.Free;

  aQuery.Free;
end;

end.
