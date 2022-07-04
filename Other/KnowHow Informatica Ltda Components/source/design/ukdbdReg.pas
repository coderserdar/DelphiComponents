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

unit ukdbdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

{
--------------------------------------------------------------------------------
------------------------------- Registration -----------------------------------
--------------------------------------------------------------------------------
}

procedure Register;

implementation

uses
	Classes, DsgnIntf, DB, DBTables, uksyShortCuts, uksyUtils, uksydClasses,
	ukrdConsts, ukdbUtils, ukdbClasses, ukdbTables, ukdbEngines, ukdbScript,
  ukdbdConsts, ukdbdMComments, ukdbdClasses;

procedure Register;
begin

{ Component Registration }
	RegisterComponents( sRegDB,
		[TKTable, TKQuery, TKTableAuditor, TKQueryAuditor, TKTableLogEngine,
		 TKQueryLogEngine, TKDBLogEngine, TKDBLogManager, TKSQLScript,
     TKSQLScriptEx, TKDBPrint] );

{ Component Editors }
	RegisterComponentEditor( TKSQLScript, TKSQLScriptEditor );
	RegisterComponentEditor( TKSQLScriptEx, TKSQLScriptEditorEx );
  RegisterComponentEditor( TKDBLogManager, TKDBLogManagerEditor );

{ Generic Property Editors - TKTable, TKQuery, TKQueryItem }

	RegisterPropertyEditor( TypeInfo( TKDataSetAction ), nil, '', TKDataSetActionProperty );
	RegisterPropertyEditor( TKKeyActions.ClassInfo, nil, 'KeyActions', TKKeyActionsProperty );
	RegisterPropertyEditor( TypeInfo( TKAggregateDataSetActions ), nil, 'DataSetActions',
		TKAggregateDataSetActionsProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomDBIntegrityItem , 'DataBaseName',
		TKDataBaseNameIntegrityItemProperty );
	RegisterPropertyEditor( TypeInfo( TStrings ), TKCustomDBIntegrityItem , 'LinkedFields',
		TKLinkedFieldsProperty );

{ DB Log Engine }    
	RegisterPropertyEditor( TypeInfo( string ), TKDBLogEngine, 'TableName',
		TKDBLogEngineTableNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKDBLog, 'TableName',
		TKDBLogTableNameProperty );

{ Table Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKTableDBIntegrityItem, 'TableName',
		TKTableNameIntegrityItemProperty );

{ Query Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKQuery, 'KeyFields',
		TKQueryKeyFieldsProperty );

{ SQLScript Property Editors }
	RegisterPropertyEditor( TypeInfo( TComponent ), TKQueryItem, 'LinkTo', TKQueryItemLinkToProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKQueryItem, 'DatabaseName', TKQueryItemDataBaseNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKQueryItem, 'SessionName', TKQueryItemSessionNameProperty );
	RegisterPropertyEditor( TypeInfo( TStrings ), TKQueryItem, 'SQL', TKSQLStringsProperty );
	RegisterPropertyEditor( TypeInfo( TStrings ), TKPUpdateObject, '', TKSQLStringsProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKQueryItem, 'KeyFields', TKQueryKeyFieldsProperty );

{ SQLScriptEx Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKSQLScriptEx, 'DatabaseName', TKSQLScriptExDatabaseNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSQLScriptEx, 'SessionName', TKSQLScriptExSessionNameProperty );

{ ShortCut Constants }

	RegisterShortCuts( DEFAULT_KEYACTIONS );

{ Method Comments }

{------------------------ TKCustomDataSetAuditor Comments ----------------------}

	RegisterDefaultMethod( TypeInfo( TKDatasetRequestFileNameEvent ), TKCustomDataSetAuditor,
		'OnRequestFileName', sComDSAuditorReqFName );
	RegisterDefaultMethod( TypeInfo( TKDatasetLogErrorEvent ), TKCustomDataSetAuditor,
		'OnDatasetLogError', sComDSLogError );

{------------------------ TKTableDBIntegrityItem Comments ----------------------}

	RegisterDefaultMethod( TypeInfo( TKOnEvaluateSQL ), TKTableDBIntegrityItem,
		'OnEvaluateSQL', sComDBIntItemEvalSQL );

{----------------------------- TKCascadeItem Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCascadeItem, 'AfterDeletion',
		sComCascadeItemAfterDel );

{-------------------------- TKTableDBIntegrity Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TKOnIntegrityError ), TKTableDBIntegrity,
		'OnIntegrityError', sComDBIntIntError );

{--------------------- TKTable/TKQuery/TKQueryItem Comments --------------------}

	RegisterDefaultMethod( TypeInfo( TKDataSetBeforeActionEvent ), TKTable, 'BeforeDatasetAction',
		sComKQuyTblBeforeDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetBeforeActionEvent ), TKQuery, 'BeforeDatasetAction',
		sComKQuyTblBeforeDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetBeforeActionEvent ), TKQueryItem, 'BeforeDatasetAction',
		sComKQuyTblBeforeDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetAfterActionEvent ), TKTable, 'AfterDatasetAction',
		sComKQuyTblAfterDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetAfterActionEvent ), TKQuery, 'AfterDatasetAction',
		sComKQuyTblAfterDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetAfterActionEvent ), TKQueryItem, 'AfterDatasetAction',
		sComKQuyTblAfterDSAction );
	RegisterDefaultMethod( TypeInfo( TKDataSetKeyEvent ), TKTable, 'OnKeyDown',
		sComKQuyTblOnKeyDown );
	RegisterDefaultMethod( TypeInfo( TKDataSetKeyEvent ), TKQuery, 'OnKeyDown',
		sComKQuyTblOnKeyDown );
	RegisterDefaultMethod( TypeInfo( TKDataSetKeyEvent ), TKQueryItem, 'OnKeyDown',
		sComKQuyTblOnKeyDown );

{------------------------- TKQuery/TKQueryItem Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery, 'AfterCommit',
		sComKQuyAfterCommit );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem, 'AfterCommit',
		sComKQuyAfterCommit );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery, 'BeforeCommit',
		sComKQuyBeforeCommit );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem, 'BeforeCommit',
		sComKQuyBeforeCommit );
	RegisterDefaultMethod( TypeInfo( TKDataSetRollBackEvent ), TKQuery, 'OnRollBackChildren',
		sComKQuyOnRollBackChildren );
	RegisterDefaultMethod( TypeInfo( TKDataSetRollBackEvent ), TKQueryItem, 'OnRollBackChildren',
		sComKQuyOnRollBackChildren );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery, 'OnUpdateChildren',
		sComKQuyOnUpdateChildren );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem, 'OnUpdateChildren',
		sComKQuyOnUpdateChildren );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery, 'OnExecSQL',
		sComKQuyOnExecSQL );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem, 'OnExecSQL',
		sComKQuyOnExecSQL );

	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery,
		'AfterChildrenApplyUpdates', sComKQuyAfterChildrenApplyUpd );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem,
		'AfterChildrenApplyUpdates', sComKQuyAfterChildrenApplyUpd );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQuery,
		'BeforeChildrenApplyUpdates', sComKQuyBeforeChildrenApplyUpd );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKQueryItem,
		'BeforeChildrenApplyUpdates', sComKQuyBeforeChildrenApplyUpd );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQuery,
		'OnOverlappedCommit', sComKQuyOnOverlappedCommit );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQueryItem,
		'OnOverlappedCommit', sComKQuyOnOverlappedCommit );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQuery,
		'OnOverlappedEnd', sComKQuyOnOverlappedEnd );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQueryItem,
		'OnOverlappedEnd', sComKQuyOnOverlappedEnd );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQuery,
		'OnOverlappedRollback', sComKQuyOnOverlappedRollback );
	RegisterDefaultMethod( TypeInfo( TKQueryOverlappedEvent ), TKQueryItem,
		'OnOverlappedRollback', sComKQuyOnOverlappedRollback );
	RegisterDefaultMethod( TypeInfo( TKDatasetCacheIntegrityEvent ), TKQuery,
		'OnCheckCacheIntegrity', sComKQuyOnCheckCacheIntegrity );
	RegisterDefaultMethod( TypeInfo( TKDatasetCacheIntegrityEvent ), TKQueryItem,
		'OnCheckCacheIntegrity', sComKQuyOnCheckCacheIntegrity );

{------------------------------- TKTable Events --------------------------------}

	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKTable, 'OnCopyRecord',
		sComKTblOnCopyRecord );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKTable, 'OnDitto',
		sComKTblOnDitto );
	RegisterDefaultMethod( TypeInfo( TKOnIntegrityError ), TKTable, 'OnIntegrityError',
		sComKTblOnIntegrityError );
	RegisterDefaultMethod( TypeInfo( TDataSetNotifyEvent ), TKTable, 'OnPasteRecord',
		sComKTblOnPasteRecord );

{---------------------------- TKSQLScriptEx Events ------------------------------}

	RegisterDefaultMethod( TypeInfo( TKSQLScritTableBuildEvent ), TKSQLScriptEx,
		'OnTableBuild',	sComSQLSOnTableBuild );
	RegisterDefaultMethod( TypeInfo( TKSQLScritIndexBuildEvent ), TKSQLScriptEx,
		'OnIndexBuild',	sComSQLSOnIndexBuild );

{------------------------- TKDBLogEngine Commments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDBLogEngineSQLEvent ), TKDBLogEngine,
		'OnCreateTable',	sComDBEngineCrtTbl );
	RegisterDefaultMethod( TypeInfo( TKDBLogEngineSQLEvent ), TKDBLogEngine,
		'OnInsertRecords',	sComDBEngineInsRec );
	RegisterDefaultMethod( TypeInfo( TKDBLogEngineFillParamsEvent ), TKDBLogEngine,
		'OnFillParams',	sComDBEngineFillPrm );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBLogEngine, 'OnAbort',
    sComDBEngineAbort );
	RegisterDefaultMethod( TypeInfo( TKDBLogEngineSelectFields ), TKDBLogEngine,
		'OnSelectFields',	sComDBEngineSelFlds );

{---------------------------- TKDBLog Commments --------------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBLog, 'OnAbort',
    sComDBLogAbort );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBLog, 'AfterExecute',
    sComDBLogAfterExecute );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBLog, 'BeforeExecute',
    sComDBLogBeforeExecute );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerSQLEvent ), TKDBLog,
		'OnCreateTable',	sComDBLogCrtTbl );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerSQLEvent ), TKDBLog,
		'OnInsertRecords',	sComDBLogInsRec );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerFillParamsEvent ), TKDBLog,
		'OnFillParams',	sComDBLogFillPrm );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerSelectFields ), TKDBLog,
  	'OnSelectFields',	sComDBLogSelFlds );
  RegisterDefaultMethod( TypeInfo( TKDBLogManagerGetLogEngineEvent ), TKDBLog,
  	'OnGetLogEngine',	sComDBLogGetLogEngine );

{------------------------- TKDBLogManager Commments ----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDBLogManagerLogAbortEvent ), TKDBLogManager, 'OnLogAbort',
    sComDBLogManagerLogAbort );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerGroupAbortEvent ), TKDBLogManager, 'OnGroupAbort',
    sComDBLogManagerGroupAbort );
	RegisterDefaultMethod( TypeInfo( TKDBLogManagerLoadErrorEvent ), TKDBLogManager, 'OnLoadError',
    sComDBLogManagerLoadError );

end;

end.
