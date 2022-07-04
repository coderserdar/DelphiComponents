{   The unit SqlitePassErrorLang defines all the generic error messages for
    the library. You can easily set your own translation by copying or overwriting
    this file.


    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ---------------------------------------------------------------------------

    Messages localisation unit
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006-2010

    Major changes are indicated in the \Documentation\changes.pdf file
    Last update : 10.03.2010

   --------------------------------------------------------------------------- }


Unit SqlitePassErrorLang;

Interface
//******************************************************* DatabaseError Messages

ResourceString

CRLF = #13#10;

{ Generic messages }
Msg1000 = 'Error';
Msg1001 = 'Unknown';
Msg1002 = 'No Error';
Msg1100 = 'Select the Ellipsis button'; { Add designtime info on Object Inspector }

{ SqlitePassUtilsmessages }
SqlitePassListIndexError = 'Error while accessing list item %s';
SqlitePassListCapacityError= 'List capacity exceeded';
SqlitePassListCountError = 'List count error';

{ Sqlite library messages }
Msg3000 = 'The function %s was not found in the current installed Sqlite library';
Msg3001 = 'file %s not found in path';

{ SqlitePassDatabase messages }
Msg1005 = 'TSqlitePassDatabase.Open : Database property is not defined.';
Msg1007 = 'TSqlitePassDatabase.Open : Unable to open the database.';
Msg1010 = '%s : Cannot set the %s property while Database is connected.';
Msg1038 = 'Cannot find Database :';
Msg1041 = 'TSqlitePassDatabase.AttachDatabase : Database type mismatch.';
Msg1042 = 'TSqlitePassDatabase.SetFDatabaseType : Do you want to replace your DatatypeOptions by the default ones for this database type ? ';
Msg1044 =   'TSqlitePassDatabase.DataTypeOptions.LoadFromDatabase : Error while loading data from database. '
          + 'The "SqlitePass__DbSettings" table is corrupted or was created using an old version of this component. '
          + ' Do you want to remove the "SqlitePass__DbSettings" from your database ? (A new table will be created on database closing).';
Msg1046 = 'TSqlitePassDatabase.DeleteDatabase : Unable to delete the file %s .';

{ SqlitePassDatabasesAttached messages }
Msg1800 = 'TSqlitePassDatabasesAttached.DatabaseByName : Database %s not found.';


{ SqlitePassEngine }
Msg3010 = 'TSqlitePassEngine.BindParam : invalid format for blob data.';
Msg3011 = 'TSqlitePassEngine.BindValue : Unknown field type : %s';
Msg3012 = 'TSqlitePassEngine.CreateDatabase : the database was not created. The file %s already exists';
Msg3013 = 'Unable to execute SQL query : %s%s';
Msg3014 = 'Database engine is not available %s';
Msg3016 = 'Error while posting the record.';


{ SqlitePassDataset }
Msg2002 = 'TSqlitePassDataset.ExecSQL : Cannot execute SQL query while Database is not Assigned or Connected.';
Msg2003 = 'TSqlitePassDataset.ExecuteSQL : Cannot execute SQL query while Database is not Assigned or Connected.';
Msg2011 = 'TSqlitePassDataset.SetFDatabase : Cannot set Database while Dataset is active.';
Msg2012 = 'TSqlitePassDataset.SetFDatasetName : Cannot set DatasetName while Dataset is active.';
Msg2015 = 'TSqlitePassDataset.SetFSQL : Cannot set SQL while a Table, Query or View is selected.';
Msg2016 = 'TSqlitePassDataset.SetFSQL : Cannot set SQL while Dataset is selected.';
Msg2018 = 'TSqlitePassDataset.Open : Missing Database Property';
Msg1040 = 'TSqlitePassDataset.GetRecord, No record found, dataset is empty.';
Msg2019 = 'TSqlitePassDataset.Open : Missing DatasetName, or SQL is empty.';
Msg2020 = 'TSqlitePassDataset.Open : Unable to open Dataset because the Database is not connected';
Msg2021 = 'TSqlitePassDataset.Open : Unable to open Dataset because the Database is not connected. Check the components create order '
          + 'of your form. The database must be created before the datasets';
Msg2022 = 'TSqlitePassDataset.Post : Error while posting record. The PrimaryKey value is already present in the dataset or an internal constraint prevents data change...';
Msg2024 = 'TSqlitePassDataset.Delete : Error while deleting record.';
Msg2025 = 'TSqlitePassDataset.InitFieldDefs : Columns count from parsed SQL statement doesn''t match SQLite engine columns count.';
Msg2026 = 'TSqlitePassDataset.SetMasterDataSource : Datasource circular reference not supported';
Msg2027 = 'TSqlitePassDataset.InitFieldDefs : Cannot retrieve FieldDefs because dataset is closed.';
Msg2028 = 'TSqlitePassDataset.SetFDatasetName : This will clear all defined fields. Do ou want to continue ?';
Msg2029 = 'TSqlitePassDataset.InternalOpen : Cannot Open Dataset.';
Msg2030 = 'TSqlitePassDataset.SetFLookUpCache : Cannot set LookUpCache while Dataset is active.';
Msg2031 = 'TSqlitePassDataset : Cannot set % while Dataset is active.';
Msg2032 = 'TSqlitePassDataset.SetDataField : Cannot set data. This field is readonly.';
Msg2033 = 'TSqlitePassDataset.SetFWriteMode : Cannot set WriteMode while Dataset is active.';
Msg2040 = 'TSqlitePassDataset.ParseFilterText : Syntax error found while parsing filter text.';
Msg2045 = 'TSqlitePassDataset.SetFDatabase : Cannot set Database while Dataset is active.';
Msg2047 = 'TSqlitePassDataset.SetFLookupDisplayedRecordsOnly : Cannot set this property while Dataset is active.';
Msg2049 = 'TSqlitePassDataset.SetFCalcDisplayedRecordsOnly : Cannot set this property while Dataset is active.';


{ TSqlitePassDatasets }
Msg2070 = 'TSqlitePassDatasets.DatasetByName : Dataset %s not found.';

{ TSqlitePassFieldFilters }
Msg2100 = 'TSqlitePassFieldFilters.FilterByFieldName : Requested filter not found';

{ TSqlitePassTableDefs }
Msg3100 = 'TSqlitePassTableDefs.TableByName : Requested table not found';

{ TSqlitePassIndexes }
Msg3200 = 'TSqlitePassIndexDefs.IndexByName : Requested index not found';
Msg3210 = 'TSqlitePassIndexColumns.ColumnByName : Requested column not found';
Msg3220 = 'TSqlitePassIndex.SetFIndexName : %s is not a valid index name. '
         +'First character must be like [A..Z, a..z, _ ] and others like '
         +'[A..Z, a..z, 0..9, _ ]';


{ TSqlitePassQueryDefs }
Msg3300 = 'TSqlitePassQueryDefs.QueryByName : Requested query not found';

{ TSqlitePassViews }
Msg3400 = 'TSqlitePassViews.ViewByName : Requested view not found';

{ TSqlitePassTriggers }
Msg3500 = 'TSqlitePassTriggers.TriggerByName : Requested Trigger not found';

{ TSqlitePassFieldDefs }
Msg3600 = 'TSqlitePassFieldDefs.FieldByName : Requested FieldDef not found';

{ TSqlitePassTableDefs }
Msg3700 = 'TSqlitePassTableDefs.TableByName : Requested TableDef not found';

{ TSqlitePassStmtSections }
Msg3800 = 'TSqlitePassStmtSection.SectionByName : Requested Section not found';

{ TSqlitePassBlobStream }

{ TSqlitePassSelectStmt }
Msg3900 = 'TSqlitePassSelectStmt.BuildSchema : Table or field not found ';
Msg3910 = 'TSqlitePassSelectStmt.FieldDefs.FieldByName : Requested FieldDefByName not found';

{ TSqlitePassSelectStmts }
Msg3950 = 'TSqlitePassSelectStmts.SQLStmtByName : Requested SQLStmt not found';

{ TSqlitePassTranslator }
Msg3980 = 'TSqlitePassTranslator.GetDatabaseTableFieldDefs : Unable to retrieve fields definitions for table %s .';

{ TSqlitePassTranslator_Kexi }
Msg4000 = 'TSqlitePassTranslator_Kexi.GetDatabaseTableFieldDefs : Error while opening system dataset : %s';

implementation
end.
