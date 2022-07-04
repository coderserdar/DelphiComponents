{
=======================================================================

    KLIB v100
    Serious Software Made in Brazil
    Copyright 1997-1999 by KnowHow Informatica Ltda.


            home-page: www.knowhow-online.com.br
    technical support: support@knowhow-online.com.br
  general information: inf@knowhow-online.com.br


    Unless otherwise noted, all materials provided in this release
		are copyright © 1997-1999 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukdbResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{
--------------------------------------------------------------------------------
-------------------------------- ukdbTables.pas --------------------------------
--------------------------------------------------------------------------------
}

	sDBConfirmDelete = 'Confirm the exclusion?';

	sErrQUYNoDatabase = 'No database currently selected for query %s';
	sErrQUYInvOverlappedOperation = 'Cannot perform this operation in overlapped mode';

	sErrTBInvState = 'Cannot perform the Copy/Paste operation in the current dataset state';
	sErrTBInvDittoState = 'Cannot perform the Ditto operation in the current dataset state';
	sErrTBInvDittoEmptyTable = 'Cannot perform the Ditto operation on an empty table';

	sErrDBIInvDBIClass = 'Invalid DBIntegrityItem owner class';
	sErrDBIIntegrityError = 'An error occurred while checking referencial integrity %s at item %s';

	sErrDBIInvQueryAction = 'Invalid query action requested';
	sErrDBIInvLinkedFlds = 'Invalid linked fields value for integrity item %s';

	sErrDSInvAlias = 'Invalid SQL Alias or TableName';
	sErrDSInvSQLStatement = 'Invalid SQL Statement';
	sErrDSInvSQLStatementEx = 'Invalid SQL Statement (Error: "%s")';
	
{
--------------------------------------------------------------------------------
-------------------------------- ukdbEngines.pas -------------------------------
--------------------------------------------------------------------------------
}

{ DB Auditory Log Extensions }

	DSL_DATASET_NOTASSIGNED = '<Dataset not assigned>';
	DSL_DATASET_NOTDBDATASET = '<Dataset is not a DBDataSet>';
	DSL_DATASET_NOCUSTOMINFO = '<No custom info for Dataset>';
	DSL_DATASETOWNER_NOTASSIGNED = '<Dataset owner not assigned>';

	sErrDSAInvName = 'Invalid Dataset Actions name: %s';
	sErrDSAInvValue = 'Invalid Dataset Actions value: %s';

{
--------------------------------------------------------------------------------
--------------------------------- ukdbScript.pas -------------------------------
--------------------------------------------------------------------------------
}

	sQIDefName = 'QueryItem - (no name)';

	sErrSSInvQueryName = 'Script error: invalid query name: %s';
	sErrSSInvScriptRange = 'Script error: invalid script range (%d-%d)';

	sErrSSEInvCreateDB = 'Script error: could not create database because SQL script is empty';
	sErrSSEInvBuildInfos = 'Script error: could not build script due to incomplete information- either SessionName or DatabaseName missing';
	sErrSSEInvCreateDBName = 'Script error: could not create database due to incomplete information- source and destination DatabaseNames cannot be the same';
	sErrSSEInvDBNameLoading = 'Script error: the DatabaseName property read from the stream was invalid (%s)- script items were created with no DatabaseName (empty)';

{
--------------------------------------------------------------------------------
-------------------------------- ukdbClasses.pas -------------------------------
--------------------------------------------------------------------------------
}

	sErrInvTable = 'Invalid table: either the table object or its TableName property is invalid';

	sErrSBInvTbl = 'SQLBuild error: invalid table ID %d';
	sErrSBInvStr = 'SQLBuild error: cannot get %s of an empty script';
	sErrSBInvTblFlds = 'SQLBuild error: invalid fields for table %s';
	sErrSBInvIndexID = 'SQLBuild error: invalid index key ID %d for table %s';
	sErrSBInvTblPattern = 'SQLBuild error: invalid SQL table pattern at %d';
	sErrSBInvIdxPattern = 'SQLBuild error: invalid SQL index of table "%s" ("%d") pattern at "%d"';
	sErrSBInvSqlScriptType = 'SQLBuild error: invalid SQL Script Type %s';

	sErrDBCInvSOpen = 'DBSQLBulid error: could not open session %s';
	sErrDBCInvSName = 'DBSQLBulid error: invalid session name %s';
	sErrDBCInvDBOpen = 'DBSQLBulid error: could not open current database %s';
	sErrDBCInvDBName = 'DBSQLBulid error: invalid database name %s';
	sErrDBSBInvDBaseType = 'DBSQLBulid error: invalid DBase type- DBase does not support %d ID type';
	sErrDBSBInvTblSessionDB = 'DBSQLBulid error: invalid session and database name for table %s';

	sSBEmptyDB = '/* Empty Database */';
	sSBEmptyIdx = '/* No Indexes defined for table "%s" */'#13#10#13#10;
	sSBIdxEmptyName = '(NoIndex)';
	sSBInternalHeaderPattern = '/* KnowHow %0:s SQL Script Builder. Version 1.0. CopyRight 1999(c). */'#13#10#13#10'/* %1:s */'#13#10#13#10'%2:s';
	sSBInternalFooterPattern = '%0:s'#13#10#13#10'/* %1:s */'#13#10#13#10'/* KnowHow %2:s SQL Script Builder. Version 1.0. CopyRight 1999(c). */';

	sSQLBDBAnsiComment = 'Comments go here';
	sSQLBDBAnsiDBComment = 'Comments for Database go here..';

{
--------------------------------------------------------------------------------
--------------------------------- ukdbUtils.pas --------------------------------
--------------------------------------------------------------------------------
}

	sErrDBIInvSQLOperator = 'Invalid Operator for a SQL Statement';

	sErrDBInvFieldRegProc = 'Invalid knowhow field registration callback';
	sErrDBInvControlRegProc = 'Invalid knowhow dictionary control registration callback';
	sErrDBInvGetFldClassProc = 'Invalid knowhow get field class callback';

	sErrSBTypeNotImpl = 'Script build error: script type %s not yet implemented';
	sErrSBInvScriptToken = 'Script read error: control token(s) not found- this script was generated by a KnowHow SQL Build compliant product';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

  { 28/jan/2000 }

  sErrDBLEInvLogEngine = 'Invalid LogEngine: either it is nil, fields or records are empty. You should load a valid file before create the db log';
  sErrDBLEInvQuery = 'Invalid query: either there is no valid database name assigned to it or a table name that already exists';
  sErrDBLEInvFieldParams = 'Invalid query params: either the field and params list differs, or the defined query parameters and param list differs';

  sErrInvDBLogExecute = 'Abort requested on error while executing db item %s';
  sErrDBLOGInvArrayIndex = 'DB Log item Index out of bounds (%d)';

	sErrDBIInvFldCount = 'Invalid Valid Field count for dataset "%s"';
	sErrDSInvFldCount = 'Invalid field count. Cannot edit KeyFields';
	sErrDSInvFldTypes = 'Invalid Field Types';
	sErrDSInvDsgnFldName = 'The validate check mechanism found the error message: %s.'#13#10+
		' This message normally occurs if you try to assign LinkedFields values'+
		' at design time, via textual form assignment. You can do so, but be sure '+
		' that your are using correct field values. The linked fields will be "emptied"';

  
implementation

{

PS: This text must not be after END., because of a Delphi4 warning...

	Deprecated, Revision 100, July 26, 1999
	---------------------------------------

	ukdbTables.pas
		sErrDBInvCachedUpd = 'Cannot perform this operation if CachedUpdates are disabled';
		sErrQUYInvOperation = 'Cannot perform this operation on a closed query';
		sErrDSAInvalidName = 'Invalid KnowHow DataSet Action Name %d';
		sErrDSAInvalidValue = 'Invalid KnowHow DataSet Action Value %d';
		sErrDBIInvItems = 'Cannot evaluate intergrity because no items have been defined';
		sErrDBIInvItemName = 'Invalid Integrity item name (empty)';
		sErrDBIItemIDNotFound = 'Integrity item ID %d not found';
		sErrDBIItemNameNotFound = 'Integrity item name %s not found';
		sErrDBIDuplicateItemName = 'Duplicate integrity item name (%s)';
		sErrDBICascadeIntegrityErrror = 'An error occurred while checking cascade integrity %s at item %s';

	ukdbClasses.pas
		sErrSBInvPrimKeyID = 'SQLBuild error: invalid primary key ID for table %s';
		sSBEmptyIdxAll = '/* No Indexes defined */';

	ukdbUtils.pas
		sErrDSInvFldName = 'Invalid field name "%s" for DataSet "%s"';
		sErrDSInvPrimKeyFldName = 'Invalid primary key field name "%s" for DataSet "%s"';
		sErrDBInvRegFieldClass = 'Invalid registered field class "%s" into knowhow library';

}

end.

