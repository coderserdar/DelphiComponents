unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, colorado, cfields, colorx;

type
  TDataModule1 = class(TDataModule)
    DBCatalog1: TDBCatalog;
    DBTables1: TDBTables;
    DBColumns1: TDBColumns;
    ColumnProperties: TDBProperties;
    DBIndexes1: TDBIndexes;
    DBKeys1: TDBKeys;
    IndexColumns: TDBColumns;
    DBKeys1NAME: TStringField;
    DBKeys1TYPE: TStringField;
    DBKeys1RELATED_TABLE: TStringField;
    DBKeys1UPDATE_RULE: TStringField;
    DBKeys1DELETE_RULE: TStringField;
    IndexColumnsSORT_ORDER: TStringField;
    IndexColumnsNAME: TStringField;
    IndexColumnsCOLUMN: TStringField;
    IndexProperties: TDBProperties;
    KeyColumns: TDBColumns;
    StringField1: TStringField;
    KeyColumnsNAME: TStringField;
    KeyColumnsRELATED_COLUMN: TStringField;
    Connection1: TConnection;
    DBViews1: TDBViews;
    DBProcedures1: TDBProcedures;
    DBGroups1: TDBGroups;
    GroupUsers: TDBUsers;
    DBUsers1: TDBUsers;
    UserGroups: TDBGroups;
    UserPermissions: TDBPermissions;
    GroupPermissions: TDBPermissions;
    DBParameters1: TDBParameters;
    ColumnPropertiesNAME: TStringField;
    ColumnPropertiesTYPE: TStringField;
    ColumnPropertiesVALUE: TCVariantField;
    ColumnPropertiesREQUIRED: TBooleanField;
    ColumnPropertiesREADONLY: TBooleanField;
    ColumnPropertiesWRITEONLY: TBooleanField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.DFM}

end.
