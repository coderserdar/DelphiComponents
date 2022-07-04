{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukdbdConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	uksyConsts;

{
--------------------------------------------------------------------------------
----------------------------- Generic Constants --------------------------------
--------------------------------------------------------------------------------
}

const
	SCRIPT_VERBCOUNT = 4;
	SCRIPTEX_VERBCOUNT = 3;

	QUERY_ITEMS_DEFAULT_FILE_EXT = '.qyi';

	sDBLMDefExt = '.dlm';
	sDBLMFilter = 'DB Log Manager Resource Files (*.dlm)|*.dlm';
	sDBLMMergeTitle = 'Merge DB Log Manager options from file';
	sDBLMLoadTitle = 'Load DB Log Manager options from file';
	sDBLMSaveTitle = 'Save DB Log Manager options to file';

	sDBLMConfirmDel = 'The file "%s" already exists. Do you want to substitute this file?';

	DBLOGMANAGER_VERBCOUNT = 3;
	DBLOGMANAGER_VERBS: array[0..DBLOGMANAGER_VERBCOUNT - 1] of string =
	 ( 'Merge From File', 'Load From File', 'Save To File' );


{
--------------------------------------------------------------------------------
-------------------------- Generic Resource Strings ----------------------------
--------------------------------------------------------------------------------
}

resourcestring

	sRegDB = 'KnowHow DB';

	sSQLSSpace = '-';
	sSQLSLoad = 'Load &QueryItems From File';
	sSQLSSave = '&Save QueryItems To File';
	sSQLSPrint = '&Print Script';

	sSQLSFilter = 'Query Item Files (*.qyi)|*.qyi|All Files(*.*)|*.*';
	sSQLSTitle = 'Select a QueryItems File';

	sSQLSExBuild = '&Build Script';
	sSQLSExLoad = '&Load Scripts From File';
	sSQLSExCreateDB = '&Create DataBase';
	sSQLSExTitle = 'Open SQL File';
	sSQLSExFilter = 'SQL Files (*.sql)|*.sql|All Files(*.*)|*.*';

	sSQLSExCreateDBTitle = 'Creating DataBase using SQLScriptEx';
	sSQLSExCreateDBText = 'Choose a database alias for create entities';
	sSSEScriptPrint = 'Do you really want to print %d script items?';
	sErrSQLEXInvAvailDBNames = 'Script error: no destination database- you must select/create a database/alias first';
	sSQLReplaceFile = 'File "%s" already exists. OK to replace?';

	sErrDSAPropInvClass = 'Invalid class for DataSetActions property editor';

	sLinkedFldEmpty = '(Empty)';

	sSQLSInformPrint = 'Scripts printed sucessful';
	sSQLSInformSave  = 'Query Items saved sucessful';
	sSQLSInformLoad  = 'Query Items loaded sucessful';

	sSQLSExBuild2 = 'Query Items sucessfuly built';
	sSQLSExDBCreated = 'DataBase created sucessful';
	sSQLSExInformLoad  = 'Scripts loaded sucessful';

 	sWarnInvDBLogEngineTblName = 'You should set the table name for a ' +
 	  'new table. The table you select %s, already exists in database %s.' +
	  'When you convert the database this will raise an exception. All ' +
    'table names in the list above is the values you should not set, or ' +
    'if you do so, make sure to drop the table before call CreateDBLog method';

 	sWarnInvDBLogTblName = 'You should set the table name for a new table if ' +
    'DBLogType is dbltCreateInsert or for a existent one if it is dbltInsert.' +
    'The table you select %s are invalid selection in database %s either because ' +
    'it already exists and you will create it or because it does not exists and ' +
    'will insert records in it. When you convert the database this will raise an ' +
    'exception. For dbltCreateInsert, the table values in the list above should not' +
    'be set, otherwise this is the valid list.';

implementation

end.
