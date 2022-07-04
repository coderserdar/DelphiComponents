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

unit ukrResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{-------------------------------- ukrCtrls.pas ---------------------------------}

{Speed Controls}
	sErrSCInvAllowNull = 'SpeedControl error: current value is NULL';
	sErrSCInvFileMustExist = 'SpeedControl error: value points to a non existing file';
	sErrSCInvFolderMustExist = 'SpeedControl error: value points to a non existing folder';

{------------------------------- ukrDBCtrls.pas --------------------------------}

{TKDBNavigator}
	sDNNext = 'Next';
	sDNLast = 'Last';
	sDNSave = 'Save';
	sDNFirst = 'First';
	sDNPrior = 'Prior';
	sDNPrint = 'Print';
	sDNSearch = 'Search';
	sDNCancel = 'Cancel';
	sDNInsert = 'Insert';
	sDNDelete = 'Delete';
	sDNRefresh = 'Refresh';
	sErrDBNavInvFrmClass = 'Invalid form class (%s) for DBNavigator.NavEvents.%s.FormName';

{------------------------------- ukrClasses.pas --------------------------------}

	sErrInvMoveParams = 'CopyBuffer error: the copy parameters define an invalid stream operation';

{TKStringsArray}
	sErrStringsArrayIndex = 'StringsArray error: index out of bounds at row %d';
	sErrInvStringsArrayStream = 'StringsArray error: stream read error ar line %d';
	sDefaultStringsArrayPropertyName = 'StringsArray';

{TKBytes}
	sErrBytesOutRange = 'TKBytes error: index out of range';
	sErrBytesCannotGBit = 'TKBytes error: could not get bit';
	sErrBytesCannotSBit = 'TKBytes error: could not set bit';
	sErrBytesCannotGDiBit = 'TKBytes error: could not get dibit';
	sErrBytesCannotSDiBit = 'TKBytes error: could not set dibit';

{TKPCharStream}
	sErrPSInvPos = 'PCharStream error: invalid position %d';
	sErrPSInvIdx = 'PCharStream error: index out of range %d';
	sErrPSNotFound = 'PCharStream error: could not find PChar at index %d';

{TKCustomParser}

	sErrParsing = 'Parser error: %s on line %d';
	sErrLineLong = 'buffer line too long';
	sErrInvRelOp = 'invalid relational operator';
	sErrStrExpected = 'string expected';
	sErrNumExpected = 'number expected';
	sErrComtExpected = 'comment expected';
	sErrCharExpected = '"s" expected';
	sErrIdentExpected = 'identifier expected';
	sErrSymbolExpected = 'symbol expected';
	sErrNumberExpected = 'number expected';
	sErrStringExpected = 'string "%s" expected';

{TKCustomCodeParser}
	sErrInvPascalNumberToken = 'Invalid pascal number token (%s)';

{TKCaptionButtons}
	sErrKindAlreadyExists = 'Cannot create another button of the specified kind (%s).';

{TKPropList}
	sErrPLReadOnlyItems = 'PropList error: cannot assign to a read only property (Items)';

{TKStringStream}
	sErrSSCopyFromBuffer = 'StringStream error: could not copy %d bytes from source stream';

{------------------------------- ukrLanguage.pas -------------------------------}

	sErrInvLangValue = 'Invalid Language Value "%d"';
	sErrInvLangName  = 'Invalid Language Name "%s"';

{---------------------------------- ukrUtils.pas -----------------------------}

	sEmptyNodeText = '(Empty Node)';
	sEmptyItemText = '(Empty Item)';

	sErrBmpConv = 'Invalid bitmap conversion: could not convert bitmap %s to an icon';
	sErrIcoConv = 'Invalid icon conversion: could not convert icon %s to a bitmap';
	sErrInvTokenPairs = 'Invalid token balance: the string data are not balanced with token "%s"';

{------------------------------- ukrEngines.pas -----------------------------------}

{TKStreamData}
	sErrSDRead = 'StreamData error: could not read stream data (%s)';
	sErrSDWrite = 'StreamData error: could not write stream data (%s)';
	sErrSDInvBuffer = 'StreamData error: invalid buffer pointer (nil)';
	sErrSDInvBufferSize = 'StreamData error: invalid buffer size for stream buffer';
	sErrSDInvQuickSortOfs = 'StreamData error: invalid quicksort positions (start: %d; end: %d)';
	sErrSDInvNormalizeOfs = 'StreamData error: invalid (%s) StreamData normalization offset- current size is %d (start: %d; count: %d)';
	sErrSDCreateNewNilBuffer = 'StreamData error: cannot duplicate a nil buffer';
	sErrSDInvQuickSortRegion = 'StreamData error: invalid quick sort region';
	sErrSDInvBynarySearchRegion = 'StreamData error: invalid binary search region';

{TKLogEngine}
	sLEFields = 'fields';
	sLERecords = 'records';

	sRECORD_LOG_HEADER_TEXT  = 'Log record Nº';
	sRECORD_LOG_FOOTER_TEXT  = 'Total log records reported';
	sFIELDS_LOG_HEADER_TEXT  = 'Log Fields Report';
	sFIELDS_LOG_FOOTER_TEXT  = 'Total log fields reported';
	sFIELDS_LOG_FLDSIZE_TEXT = ' (FldSize)';

	sErrLEFldCountMismatch = 'LogEngine error: LogClassName %s; Engine(Name/ClassName) %s; LogFileFieldCount %d; EngineFieldCount %d';
	sErrLFInvalidFldSize = 'LogEngine error: custom field lengths must be larger than zero';
	sErrLFDuplicatedFldName = 'LogEngine error: duplicate field name (%s)';
	sErrLFWriteSignature = 'LogEngine error: could not write file signature field';
	sErrLFWriteVersion = 'LogEngine error: could not write version field';
	sErrLFWriteField = 'LogEngine error: could not write %s field to log file';
	sErrLFReadSignature = 'LogEngine error: error reading the file signature field';
	sErrLFInvalidSignature = 'LogEngine error: file signature mismatch- signature was unrecognizable';
	sErrLFReadVersion = 'LogEngine error: error reading the file version field';
	sErrLFInvalidVersion = 'LogEngine error: file version mismatch- unexpected version value';
	sErrLFInvalidLogEntry = 'LogEngine error: log entry setup failed (sizes of actual (%d) and calculated (%d) entries do not match';
	sErrLFWriteLogEntry = 'LogEngine error: could not write log entry to file';
	sErrLFWriteFieldCount = 'LogEngine error: could not write field count to file';
	sErrLFWriteFixedFieldCount = 'LogEngine error: could not write fixed field count to file';
	sErrLFWriteRecordCount = 'LogEngine error: could not write record count to file';
	sErrLFReadFieldsTable = 'LogEngine error: could not read fields table from file';
	sErrLFInvFldTblFldName = 'LogEngine error: could not read fields table because log field "%s" was not the one expected from the fields table (%s)';
	sErrLFInvFldTblFldSize =	'LogEngine error: could not read fields table because field "%s"''s field size (%d) was not the one expected from the fields table (%s)';
	sErrLFInvFldTblSize = 'LogEngine error: could not read fields table- the fields table size is not the one expected';
	sErrLERecordsNotEmpty = 'LogEngine error: record list is not empty';
	sErrLERecordsEmpty = 'LogEngine error: records list is empty';
	sErrLEFieldsNotEmpty = 'LogEngine error: field list is not empty';
	sErrLEFieldsEmpty = 'LogEngine error: fields list is empty';
	sErrLEReadLogEntry = 'LogEngine error: could not read log entry from file';
	sErrLERecordTooLong = 'LogEngine error: log entry too log';
	sErrLEInvalidLogFieldName = 'LogEngine error: invalid log field name (%s)';
	sErrLEEmptyLogEntries = 'LogEngine error: no %s currently loaded from log file %s';
	sErrLEReadFieldCount = 'LogEngine error: could not read field count from file';
	sErrLEReadFixedFieldCount = 'LogEngine error: could not read fixed field count from file';
	sErrLEReadRecordCount = 'LogEngine error: could not read record count from file';
	sErrLERecordCountMismatch = 'LogEngine error: invalid records found- %d records were read from the log file, but only %d were valid record entries';
	sErrLEInvLogRecSize = 'LogEngine error: invalid log record size';
	sErrLEInvLogHeaderSize = 'LogEngine error: invalid log header size';
	sErrLEFldTblNotFound = 'LogEngine error: fields table not found in log file %s';
	sErrLECannotClearNotLoadedFile = 'LogEngine error: cannot clear file (%s) if it is not loaded in memory';

{-------------------------------- ukrDBUtils.pas -------------------------------}

	sErrCopyPasteState = 'Dataset copy/paste error: operation only allowed if dataset state is browse';
	sErrCopyEmpty = 'Dataset copy error: dataset cannot be empty';

	sErrInvFldNo = 'The number of fields currently supported for this operation has been exceeded. FieldNo %d in dataset %s';
	sErrInvDBName = 'Invalid database name: database %s not found';
	sErrInvFldName = 'Invalid field name: field %s not found in dataset %s';
	sErrInvDataset = 'Invalid dataset object: the dataset parameter does not represent a valid dataset reference';
	sErrInvPKFldName = 'Invalid primary key field name: field "%s" is not part of table %s''s primary key';
	sErrInvDataSource = 'Invalid datasource object: the datasource parameter does not represent a valid datasource reference';
	sErrInvFldDefFldNo = 'The number of fields currently supported for this operation has been exceeded. FieldNo %d';
	sErrInvSessionName = 'Invalid session name: session %s not found';
	sErrInvDatasetEmpty = 'Dataset not empty: either the dataset parameter is invalid or references a non empty dataset';
	sErrInvDatasetEditing = 'Dataset not editing: either the dataset parameter is invalid, inactive, or not editing';
	sErrInvOriginQueryState = 'Invalid query state: cannot retrieve field origins for an inactive query (%s)';
	sErrInvOriginNULLSQLText = 'Invalid SQL text: cannot retrieve field origins if the SQL statement is NULL';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

	sErrInvDatasetReadOnly = 'Read only Dataset: either the dataset parameter is invalid or references a read only dataset';
	sErrInvTblName = 'Invalid table name: either database name %s or table name %s not found';
	sErrInvStoredProcName = 'Invalid stored procedure name: either database name %s or stored procedure name %s not found';

  sErrInvLERetriveRec = 'Invalid retrieve record: %s for logengine (%s)';

  sCheckListCheckAll = 'Check &all';
  sCheckListUnCheckAll = '&Uncheck all';

  sErrDSAssignError = 'Could not assign a %s data structure to a %s data structure';
  
implementation

{

PS: This text must not be after END., because of a Delphi4 warning...

	Deprecated, Revision 100, July 26, 1999
	---------------------------------------

?????
	sErrInvBuffering = 'An error occurred while copying the buffer';

ukrClasses.pas
	sErrInvStringsArrayStreamHeader = 'Invalid header found while reading StringsArray';
	sErrInvStringsArrayStreamFooter = 'Invalid footer found while reading StringsArray';
	sErrParseInvBin = 'Parser error: invalid binary value';


ukrEngines.pas
	sErrLEInvAutoClean = 'LogEngine error: can only clean a file after it is loaded in memory (file %s)';

}

end.
