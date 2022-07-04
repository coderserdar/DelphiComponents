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

unit ukdbdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{------------------------ TKCustomDataSetAuditor Comments ----------------------}

{
	TKCustomDataSetAuditor.OnRequestFileName: TKDatasetRequestFileNameEvent = procedure(
		Sender: TKCustomDatasetAuditor; Dataset: TDataset; var FileName: string;
		const FileDate: TDateTime; const FileSize: LongInt ) of object;
}

	sComDSAuditorReqFName =
	'» Parameters: Sender = TKCustomDataSetAuditor; Dataset = the dataset being'#13#10 +
	'    auditted; var FileName = the name of the file on which the data will be'#13#10 +
	'    logged; FileDate = the creation date of the current log file;'#13#10 +
	'    FileSize = the size of the current log file.'#13#10 +
	'» Invoked right before any data is logged to file.'#13#10 +
	'» Use this event to provide custom LogFile names for the data being logged.';

{
	TKCustomDataSetAuditor.OnDatasetLogError: TKDatasetLogErrorEvent = procedure(
		Sender: TKCustomDatasetAuditor; Dataset: TDataset; var LogErrorAction:
		TKLogErrorAction ) of object;

}

	sComDSLogError =
		'» Parameters: Sender = TKCustomDataSetAuditor; Dataset = the dataset being'#13#10 +
	'    auditted; var LogErrorAction = the action to proceed in response to this'#13#10 +
	'    error ( default = laRetry ).'#13#10 +
	'» Invoked after an open error occurs when trying to open/create the log file.'#13#10 +
	'» Use this event to respond to possible concurrency problems on a network log'#13#10 +
	'    file. If no handler is assigned to this event, the action is laSkip.';

{------------------------ TKTableDBIntegrityItem Comments ----------------------}

{
	TKTableDBIntegrityItem.OnEvaluateSQL: TKOnEvaluateSQL = procedure ( Sender: TKTable;
		Item: TKTableDBIntegrityItem; EvalSQL: TStrings ) of object;
}

	sComDBIntItemEvalSQL =
	'» Parameters: Item = a cascade or prohibit collection item; EvalSQL = '#13#10 +
	'    the default SQL statement generated for the integrity operation.'#13#10 +
	'» Invoked right after the default SQL statement for the operation is'#13#10 +
	'    generated, but before the SQL is actually executed.'#13#10 +
	'» Use this event to change the default behavior of the integrity checks'#13#10 +
	'    for your database. This event is particularly useful for maintaining'#13#10 +
	'    software-based relational integrity on file-based systems.';

{----------------------------- TKCascadeItem Comments --------------------------}

{
	TKCascadeItem.AfterDeletion: TNotifyEvent
}

	sComCascadeItemAfterDel =
	'» Parameters: Sender = TKCascadeCollectionItem.'#13#10 +
	'» Invoked right after both the cascade operation and the deletion'#13#10 +
	'    checking routine (if appropriate) have been executed.'#13#10 +
	'» Use this event for any processing that should occur after the'#13#10 +
	'    cascade operation has been properly carried out. If checking'#13#10 +
	'    deletion is necessary, use the CheckDeletion property to proceed'#13#10 +
	'    with it automatically.';

{-------------------------- TKTableDBIntegrity Comments ------------------------}

{
	TKTableDBIntegrity.OnIntegrityError: TKOnIntegrityError = procedure ( Sender: TKTable;
		Item: TKTableDBIntegrityItem; var IntegrityError: TKIntegrityError ) of object;
}

	sComDBIntIntError = '';

{--------------------- TKTable/TKQuery/TKQueryItem Comments --------------------}

{
	TKTable/TKQuery/TKQueryItem.BeforeDatasetAction: TKDataSetBeforeActionEvent =
		procedure ( Sender: TObject; DataSet: TDataSet; LogRecords: TStrings ) of object;
}

	sComKQuyTblBeforeDSAction =
	'» Parameters: Sender = TKCustomDatasetAuditor; DataSet = the dataset'#13#10 +
	'    on which the auditory is to take place; LogRecords = a list of custom'#13#10 +
	'    field values that can be managed by the user.'#13#10 +
	'» Invoked before any data is logged to the log file, before the'#13#10 +
	'    dataset action has occurred, but AFTER the BeforeXXX event of the'#13#10 +
	'    dataset has occurred.'#13#10 +
	'» Use this event to define custom data to log. Do not delete any items, '#13#10 +
	'    and DO NOT change the structure of this TStrings object; use the Values'#13#10 +
	'    property of LogRecords to include custom data to be logged. The actual'#13#10 +
	'    logging does not occur until the AfterDatasetAction has been called.'#13#10 +
	'    This event is particularly useful when the data of the dataset is not'#13#10 +
	'    available on the AfterDatasetAction event (a delete action, for instance).'#13#10 +
	'    Check the dataset''s CurrentAction to verify the action being logged.';

{
	TKTable/TKQuery/TKQueryItem.AfterDatasetAction: TKDataSetAfterActionEvent =
		procedure ( Sender: TObject; DataSet: TDataSet; var LogData: Boolean;
		LogRecords: TStrings ) of object;
}

	sComKQuyTblAfterDSAction =
	'» Parameters: Sender = TKCustomDatasetAuditor; DataSet = the dataset'#13#10 +
	'    on which the auditory is to take place; var LogData = determines if'#13#10 +
	'    the current log event should proceed with writing to the log file'#13#10 +
	'    ( default = true ); LogRecords = a list of custom field values that'#13#10 +
	'    can be managed by the user.'#13#10 +
	'» Invoked right before any data is logged to the log file, after the'#13#10 +
	'    dataset action has occurred, and AFTER the AfterXXX event of the'#13#10 +
	'    dataset has occurred.'#13#10 +
	'» Use this event to define custom data to log. Do not delete any items, '#13#10 +
	'    and DO NOT change the structure of this TStrings object; use the Values'#13#10 +
	'    property of LogRecords to include custom data to be logged. The actual'#13#10 +
	'    logging occurs right after this event returns, if LogData is true.'#13#10 +
	'    LogRecords strings will contain any custom data entered during the'#13#10 +
	'    BeforeDatasetAction event. Some dataset events DO NOT provide a BeforeXXX,'#13#10 +
	'    and therefore, will just invoke a AfterXXX event. Check the dataset''s'#13#10 +
	'    CurrentAction to verify the action being logged.';

{
	TKTable/TKQuery/TKQueryItem.OnKeyDown: TKDataSetKeyEvent = procedure( Sender: TObject;
		var Key: Word; Shift: TShiftState; var Handled: Boolean ) of object;
}

	sComKQuyTblOnKeyDown =
	'» Parameters: Sender = TDataset; var Key = the key that has been pressed;'#13#10 +
	'    Shift = the state of the keyboard control keys; var Handled = determines'#13#10 +
	'    if the message has been handled by the user- in which case the custom'#13#10 +
	'    will NOT occur ( default = false ).'#13#10 +
	'» Invoked after any key has been pressed.'#13#10 +
	'» Use this event to provide custom key management to the dataset or capture'#13#10 +
	'    key events for mere inspection.';

{------------------------- TKQuery/TKQueryItem Comments ------------------------}

{
	TKQuery/TKQueryItem.AfterCommit: TDatasetNotifyEvent
}

	sComKQuyAfterCommit =
	'» Parameters: Sender = TKQuery.'#13#10 +
	'» Invoked right after the entire commit process has finished- after'#13#10 +
	'    the StartTransaction, ApplyUpdates and Commit have all taken place.'#13#10 +
	'    This event will be called automatically after UpdateCount dataset events'#13#10 +
	'    have been put in cache by the BDE cached updates manager.'#13#10 +
	'» Use this event to provide custom processing after a block of data has been'#13#10 +
	'    committed to the database. This is the last event to occurr in the'#13#10 +
	'    autocommit process.';

{
	TKQuery/TKQueryItem.BeforeCommit: TDatasetNotifyEvent
}

	sComKQuyBeforeCommit =
	'» Parameters: Sender = TKQuery.'#13#10 +
	'» Invoked right before the entire commit process is started- before'#13#10 +
	'    the StartTransaction, ApplyUpdates and Commit take place.'#13#10 +
	'    This event will be called automatically after UpdateCount dataset events'#13#10 +
	'    have been put in cache by the BDE cached updates manager.'#13#10 +
	'» Use this event to provide custom processing before a block of data is'#13#10 +
	'    committed to the database. This is the first event to occurr in the'#13#10 +
	'    autocommit process.';

{
	TKQuery/TKQueryItem.OnRollBackChildren: TKDatasetRollBackEvent = procedure( Sender: TDataset;
		IsLastPost: Boolean; var SetEditState, Cancel: Boolean ) of object;
}

	sComKQuyOnRollBackChildren =
	'» Parameters: Sender = TKQuery; IsLastPost = determines if the last operation'#13#10 +
	'    processed by the query was a POST; var SetEditState = determines if the'#13#10 +
	'    state of the dataset after the Rollback operation will be dsEdit or dsBrowse;'#13#10 +
	'    ( default is defined by IsLastPost ); var Cancel = determines whether'#13#10 +
	'    a CancelUpdates will be issued by the dataset (default is defined by'#13#10 +
	'    FCancelOnRollback).'#13#10 +
	'» Invoked right after a rollback has happened during the autocommit process.'#13#10 +
	'» Use this event to decide how this dataset and other related datasets should'#13#10 +
	'     respond to the rollback.';

{
	TKQuery/TKQueryItem.OnUpdateChildren: TDatasetNotifyEvent
}

	sComKQuyOnUpdateChildren =
	'» Parameters: Sender = TKQuery.'#13#10 +
	'» Invoked rigth after the ApplyUpdates of the sender has been called,'#13#10 +
	'    but before the Commit of the database is actually called. Use this'#13#10 +
	'    event to apply updates of other datasets that need to post their data'#13#10 +
	'    within the same transaction as the sender''s ( ie, master-detail ).'#13#10 +
	'» Use this event to submit data from other datasets to the transaction'#13#10 +
	'    for the sender dataset. This is event occurrs within the autocommit'#13#10 +
	'    process.';

{
	TKQuery/TKQueryItem.OnExecSQL: TDataSetNotifyEvent
}

	sComKQuyOnExecSQL =
	'» Parameters: Sender = TKQuery.'#13#10 +
	'» Invoked right after a ExecSQL command is executed.'#13#10 +
	'» Use this event to provide any adequate processing after an ExecSQL'#13#10 +
	'    command is executed.';

{
	TKQuery/TKQueryItem.AfterChildrenApplyUpdates: TDatasetNotifyEvent
}

	sComKQuyAfterChildrenApplyUpd = '';

{
	TKQuery/TKQueryItem.BeforeChildrenApplyUpdates: TDatasetNotifyEvent
}

	sComKQuyBeforeChildrenApplyUpd = '';

{
	TKQuery/TKQueryItem.OnOverlappedCommit: TKQueryOverlappedEvent = procedure(
		Dataset: TDataset; var CanExecute: Boolean ) of object;
}

	sComKQuyOnOverlappedCommit = '';

{
	TKQuery/TKQueryItem.OnOverlappedEnd: TKQueryOverlappedEvent = procedure(
		Dataset: TDataset; var CanExecute: Boolean ) of object;
}

	sComKQuyOnOverlappedEnd = '';

{
	TKQuery/TKQueryItem.OnOverlappedRollback: TKQueryOverlappedEvent = procedure(
		Dataset: TDataset; var CanExecute: Boolean ) of object;
}

	sComKQuyOnOverlappedRollback = '';

{
	TKQuery/TKQueryItem.OnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent = procedure(
		Sender: TObject; op: TKCurrentOperation; var Integrity: TKCacheIntegrity ) of object;
}

	sComKQuyOnCheckCacheIntegrity = '';

{------------------------------ TKTable Comments -------------------------------}

{
	TKTable.OnCopyRecord: TDataSetNotifyEvent
}

	sComKTblOnCopyRecord =
	'» Parameters: Sender = TKTable.'#13#10 +
	'» Invoked right after a CopyRecord command is executed, either'#13#10 +
	'    explicitly or implicitly ( in response to Ditto ).'#13#10 +
	'» Use this event to provide any adequate processing after a record'#13#10 +
	'    is copied.';

{
	TKTable.OnDitto: TDataSetNotifyEvent
}

	sComKTblOnDitto =
	'» Parameters: Sender = TKTable.'#13#10 +
	'» Invoked right after a Ditto command is executed, either'#13#10 +
	'    explicitly or automatically ( depending on the DittoType ).'#13#10 +
	'» Use this event to provide any adequate processing for the Ditto'#13#10 +
	'    behaviour of the TKTable.';

{
	TKTable.OnIntegrityError: TKOnIntegrityError = procedure( Sender: TKTable;
		Item: TKTableDBIntegrityItem; var IntegrityError: TKIntegrityError ) of object;
}

	sComKTblOnIntegrityError =
	'» Parameters: Sender = TKTable; Item = the integrity item on which the'#13#10 +
	'    the error occurred; var IntegrityError = determine if the integrity'#13#10 +
	'    check should abort immediately, fail, or skip the current error'#13#10 +
	'    ignoring it ( default = ieFail ).'#13#10 +
	'» Invoked whenever an integrity operation fails. For a cascade,'#13#10 +
	'    if any deletion fails; for a prohibit operation, if any'#13#10 +
	'    records exist related to the sender''s current record.'#13#10 +
	'» Use this event to provide custom treatment of software-defined'#13#10 +
	'    integrity errors.';

{
	TKTable.OnPasteRecord: TDataSetNotifyEvent
}

	sComKTblOnPasteRecord =
	'» Parameters: Sender = TKTable.'#13#10 +
	'» Invoked right after a PasteRecord command is executed, either'#13#10 +
	'    explicitly or implicitly ( in response to Ditto ).'#13#10 +
	'» Use this event to provide any adequate processing after a record'#13#10 +
	'    is pasted.';

{------------------------- TKSQLScriptEx Commments -----------------------------}

{
	TKSQLScriptEx.OnTableBuild: TKSQLScritTableBuildEvent = procedure( Sender: TKSQLScriptEx;
		SQLBuilder: TKDBLocalSQLBuild; TableID: Integer ) of object;
}

	sComSQLSOnTableBuild =
	'» Parameters: Sender = TKSQLScriptEx; SQLBuilder = SQL Builder Object; '#13#10 +
	'    TableID = current table script being built.'#13#10 +
	'» Invoked after each table DDL SQL script has been built. A QueryItem'#13#10 +
	'    is already available and its SQL Statement is set.'#13#10 +
	'» Use this event to perform any custom action after the script construction.';

{
	TKSQLScriptEx.OnIndexBuild: TKSQLScritIndexBuildEvent = procedure( Sender: TKSQLScriptEx;
		SQLBuilder: TKDBLocalSQLBuild; IndexID: Integer ) of object;
}

	sComSQLSOnIndexBuild =
	'» Parameters: Sender = TKSQLScriptEx; SQLBuilder = SQL Builder Object; '#13#10 +
	'    IndexID = current index script being built'#13#10 +
	'» Invoked after each index DDL SQL script has been built. A QueryItem'#13#10 +
	'    is already available and its SQL Statement is set.'#13#10 +
	'» Use this event to perform any custom action after the script construction.';

{------------------------- TKDBLogEngine Commments -----------------------------}

{
	TKDBLogEngine.OnCreateTable: TKDBLogEngineSQLEvent = procedure( Sender: TKDBLogEngine;
    LogEngine: TKCustomLogEngine; var SQL: string; var Continue: Boolean ) of object;
}
  sComDBEngineCrtTbl = '';

{
	TKDBLogEngine.OnInsertRecords: TKDBLogEngineSQLEvent = procedure( Sender: TKDBLogEngine;
    LogEngine: TKCustomLogEngine; var SQL: string; var Continue: Boolean ) of object;
}
  sComDBEngineInsRec = '';

{
	TKDBLogEngine.OnFillParams: TKDBLogEngineFillParamsEvent = procedure( Sender: TKDBLogEngine;
    Query: TQuery; const RecordNo: Integer; Params: TStrings; var Continue: Boolean ) of object;
}
  sComDBEngineFillPrm = '';

{
	TKDBLogEngine.OnAbort: TNotifyEvent
}
  sComDBEngineAbort = '';

{
	TKDBLogEngine.OnSelectFields: TKDBLogEngineSelectFields = procedure( Sender: TKDBLogEngine;
    Fields: TStrings; var Continue: Boolean ) of object;
}
  sComDBEngineSelFlds = '';

{---------------------------- TKDBLog Commments --------------------------------}

{
	TKDBLog.OnAbort: TNotifyEvent
}
  sComDBLogAbort = '';

{
	TKDBLog.OnAfterExecute: TNotifyEvent
}
  sComDBLogAfterExecute = '';

{
	TKDBLog.OnBeforeExecute: TNotifyEvent
}
  sComDBLogBeforeExecute = '';

{
	TKDBLog.OnCreateTable: TKDBLogManagerSQLEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; LogEngine: TKCustomLogEngine; var SQL: string; var
    Continue: Boolean ) of object;
}
  sComDBLogCrtTbl = '';

{
	TKDBLog.OnInsertRecords: TKDBLogManagerSQLEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; LogEngine: TKCustomLogEngine; var SQL: string; var
    Continue: Boolean ) of object;
}
  sComDBLogInsRec = '';

{
	TKDBLog.OnFillParams: TKDBLogManagerFillParamsEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; Query: TQuery; const RecordNo: Integer; Params: TStrings; var
    Continue: Boolean ) of object;
}
  sComDBLogFillPrm = '';

{
	TKDBLog.OnSelectFields: TKDBLogManagerSelectFields = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; Fields: TStrings; var Continue: Boolean ) of object;
}
  sComDBLogSelFlds = '';

{
	TKDBLog.OnGetLogEngine: TKDBLogManagerGetLogEngineEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; var LogEngine: TKCustomLogEngine ) of object;
}
  sComDBLogGetLogEngine = '';

{------------------------ TKDBLogManager Commments -----------------------------}

{
	TKDBLogManager.OnLogAbort: TKDBLogManagerLogAbortEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; var AbortType: TKDBLogAbortType ) of object;
}
  sComDBLogManagerLogAbort = '';

{
	TKDBLogManager.OnGroupAbort: TKDBLogManagerGroupAbortEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog; var AbortGroup: Boolean ) of object;
}
  sComDBLogManagerGroupAbort = '';

{
	TKDBLogManager.OnLoadError: TKDBLogManagerLoadErrorEvent = procedure( Sender: TKDBLogManager;
    DBLog: TKDBLog ) of object;
}
  sComDBLogManagerLoadError = '';

implementation

end.


