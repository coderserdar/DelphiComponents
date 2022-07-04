{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  Cette source est seulement employée pour compiler et installer le paquet.
 }

unit Laz_SqlitePassDbo_Designtime; 

interface

uses
    SqlitePassIndexesDialog, SqlitePassSortByDialog, RegisterSqlitePassDbo, 
  SqlitePassCustomFieldDefsDialog, SqlitePassDataTypesDialog, 
  SqlitePassFiltersDialog, SqlitePassCreateNewDatabaseDialog, 
  SqlitePassCreateNewIndex, SqlitePassDataTypeOptions, 
  SqlitePassFieldDefsDialog, SqlitePassFilterValues, 
  SqlitePassMasterDetailFieldsDialog, SqlitePassLocateDialog, 
  SqlitePassFiltersDialogTemplate, SqlitePassIndexedByDialog, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RegisterSqlitePassDbo', @RegisterSqlitePassDbo.Register); 
end; 

initialization
  RegisterPackage('Laz_SqlitePassDbo_Designtime', @Register); 
end.
