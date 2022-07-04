{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit cschema;

interface
uses SysUtils;

type
     TSchemaParams = array[0..9] of string;

     TSchemaItem = record
       _Type: string;
       Index: integer;
       ParamCount: Byte;
       Params: TSchemaParams;
     end;

const SchemaCount = 40;

const SchemaItems : array [0..SchemaCount - 1] of TSchemaItem =
      (
        ( _TYPE : 'SchemaAsserts';
          INDEX: $0;
          PARAMCOUNT : 3;
          PARAMS : ('CONSTRAINT_CATALOG','CONSTRAINT_SCHEMA','CONSTRAINT_NAME','','','','','','','');
        ),
        ( _TYPE : 'Catalogs';
          INDEX: $1;
          PARAMCOUNT : 1;
          PARAMS : ('CATALOG_NAME','','','','','','','','','');
        ),
        ( _TYPE : 'CharacterSets';
          INDEX: $2;
          PARAMCOUNT : 3;
          PARAMS : ('CHARACTER_SET_CATALOG','CHARACTER_SET_SCHEMA','CHARACTER_SET_NAME','','','','','','','');
        ),
        ( _TYPE : 'Collations';
          INDEX: $3;
          PARAMCOUNT : 3;
          PARAMS : ('COLLATION_CATALOG','COLLATION_SCHEMA','COLLATION_NAME','','','','','','','');
        ),
        ( _TYPE : 'Columns';
          INDEX: $4;
          PARAMCOUNT : 4;
          PARAMS : ('TABLE_CATALOG','TABLE_SCHEMA','TABLE_NAME','COLUMN_NAME','','','','','','');
        ),
        ( _TYPE : 'CheckConstraints';
          INDEX: $5;
          PARAMCOUNT : 3;
          PARAMS : ('CONSTRAINT_CATALOG','CONSTRAINT_SCHEMA','CONSTRAINT_NAME','','','','','','','');
        ),
        ( _TYPE : 'ConstraintColumnUsage';
          INDEX: $6;
          PARAMCOUNT : 4;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'COLUMN_NAME','','','','','','');
        ),
        ( _TYPE : 'ConstraintTableUsage';
          INDEX: $7;
          PARAMCOUNT : 3;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME','','','','','','','');
        ),
        ( _TYPE : 'KeyColumnUsage';
          INDEX: $8;
          PARAMCOUNT : 7;
          PARAMS : ('CONSTRAINT_CATALOG', 'CONSTRAINT_SCHEMA', 'CONSTRAINT_NAME', 'TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'COLUMN_NAME','','','');
        ),
        ( _TYPE : 'ReferentialConstraints';
          INDEX: $9;
          PARAMCOUNT : 3;
          PARAMS : ('CONSTRAINT_CATALOG','CONSTRAINT_SCHEMA','CONSTRAINT_NAME','','','','','','','');
        ),
        ( _TYPE : 'TableConstraints';
          INDEX: $A;
          PARAMCOUNT : 7;
          PARAMS : ('CONSTRAINT_CATALOG', 'CONSTRAINT_SCHEMA', 'CONSTRAINT_NAME', 'TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'CONSTRAINT_TYPE','','','');
        ),
        ( _TYPE : 'ColumnsDomainUsage';
          INDEX: $B;
          PARAMCOUNT : 4;
          PARAMS : ('DOMAIN_CATALOG', 'DOMAIN_SCHEMA', 'DOMAIN_NAME', 'COLUMN_NAME','','','','','','');
        ),
        ( _TYPE : 'Indexes';
          INDEX: $C;
          PARAMCOUNT : 5;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'INDEX_NAME', 'TYPE', 'TABLE_NAME','','','','','');
        ),
        ( _TYPE : 'ColumnPrivileges';
          INDEX: $D;
          PARAMCOUNT : 6;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'COLUMN_NAME', 'GRANTOR', 'GRANTEE','','','','');
        ),
        ( _TYPE : 'TablePrivileges';
          INDEX: $E;
          PARAMCOUNT : 5;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'GRANTOR', 'GRANTEE','','','','','');
        ),
        ( _TYPE : 'UsagePrivileges';
          INDEX: $F;
          PARAMCOUNT : 6;
          PARAMS : ('OBJECT_CATALOG', 'OBJECT_SCHEMA', 'OBJECT_NAME', 'OBJECT_TYPE', 'GRANTOR', 'GRANTEE','','','','');
        ),
        ( _TYPE : 'Procedures';
          INDEX: $10;
          PARAMCOUNT : 4;
          PARAMS : ('PROCEDURE_CATALOG', 'PROCEDURE_SCHEMA', 'PROCEDURE_NAME', 'PROCEDURE_TYPE','','','','','','');
        ),
        ( _TYPE : 'Schemata';
          INDEX: $11;
          PARAMCOUNT : 3;
          PARAMS : ('CATALOG_NAME', 'SCHEMA_NAME', 'SCHEMA_OWNER','','','','','','','');
        ),
        ( _TYPE : 'SQLLanguages';
          INDEX: $12;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Statistics';
          INDEX: $13;
          PARAMCOUNT : 3;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME','','','','','','','');
        ),
        ( _TYPE : 'Tables';
          INDEX: $14;
          PARAMCOUNT : 4;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME', 'TABLE_TYPE','','','','','','');
        ),
        ( _TYPE : 'Translations';
          INDEX: $15;
          PARAMCOUNT : 3;
          PARAMS : ('TRANSLATION_CATALOG', 'TRANSLATION_SCHEMA', 'TRANSLATION_NAME','','','','','','','');
        ),
        ( _TYPE : 'ProviderTypes';
          INDEX: $16;
          PARAMCOUNT : 2;
          PARAMS : ('DATA_TYPE', 'BEST_MATCH','','','','','','','','');
        ),
        ( _TYPE : 'Views';
          INDEX: $17;
          PARAMCOUNT : 3;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME','','','','','','','');
        ),
        ( _TYPE : 'ViewColumnUsage';
          INDEX: $18;
          PARAMCOUNT : 3;
          PARAMS : ('VIEW_CATALOG', 'VIEW_SCHEMA', 'VIEW_NAME','','','','','','','');
        ),
        ( _TYPE : 'ViewTableUsage';
          INDEX: $19;
          PARAMCOUNT : 3;
          PARAMS : ('TABLE_CATALOG', 'TABLE_SCHEMA', 'TABLE_NAME','','','','','','','');
        ),
        ( _TYPE : 'ProcedureParameters';
          INDEX: $1A;
          PARAMCOUNT : 4;
          PARAMS : ('PROCEDURE_CATALOG', 'PROCEDURE_SCHEMA', 'PROCEDURE_NAME', 'PARAMETER_NAME','','','','','','');
        ),
        ( _TYPE : 'ForeignKeys';
          INDEX: $1B;
          PARAMCOUNT : 6;
          PARAMS : ('PK_TABLE_CATALOG', 'PK_TABLE_SCHEMA', 'PK_TABLE_NAME', 'FK_TABLE_CATALOG', 'FK_TABLE_SCHEMA', 'FK_TABLE_NAME','','','','');
        ),
        ( _TYPE : 'PrimaryKeys';
          INDEX: $1C;
          PARAMCOUNT : 3;
          PARAMS : ('PK_TABLE_CATALOG', 'PK_TABLE_SCHEMA', 'PK_TABLE_NAME','','','','','','','');
        ),
        ( _TYPE : 'ProcedureColumns';
          INDEX: $1D;
          PARAMCOUNT : 4;
          PARAMS : ('PROCEDURE_CATALOG', 'PROCEDURE_SCHEMA', 'PROCEDURE_NAME', 'COLUMN_NAME','','','','','','');
        ),
        ( _TYPE : 'DBInfoKeywords';
          INDEX: $1E;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'DBInfoLiterals';
          INDEX: $1F;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Cubes';
          INDEX: $20;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Dimensions';
          INDEX: $21;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Hierarchies';
          INDEX: $22;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Levels';
          INDEX: $23;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Measures';
          INDEX: $24;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Properties';
          INDEX: $25;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Members';
          INDEX: $26;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        ),
        ( _TYPE : 'Trustees';
          INDEX: $27;
          PARAMCOUNT : 0;
          PARAMS : ('','','','','','','','','','');
        )
      );

function SchemaItemByName(Name: string): Integer;

implementation

function SchemaItemByName(Name: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to SchemaCount - 1 do
      if UpperCase(SchemaItems [i]._Type) = UpperCase(Trim(Name)) then
         begin
           Result := i;
           Break;
         end;
end;

end.
