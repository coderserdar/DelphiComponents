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

unit ukedReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	DsgnIntf, Classes, Controls, uksydClasses, uksydUtils, ukrMessages, ukrdClasses,
	ukeUtils, ukeClasses, ukeEngines, ukedConsts, ukedMComments, ukedClasses;

procedure Register;
begin

{ Components }
	RegisterComponents( sRegExtended,
		[TKDFMData, TKExcept, TKExceptLogEngine, TKDFMResource, TKCreateProcess,
		 TKFileScanner, TKWordParser, TKExpiration, TKStringsComponent,
		 TKStringsArrayComponent, TKVersionControl, TKMessageSpy, TKDCC32,
		 TKHexDump, TKTextDump, TKMathSolver, TKPropertyExpression] );

{ Registry Hidden Item }
	{ RegisterHiddenComponents( sRegExtended, [TKDFMData] ); }

{ Property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKVersionControl, 'Version', TKVersionProperty );
	RegisterPropertyEditor( TypeInfo( TKCompileNumber ), TKVersionControl, 'CompileNumber',
		TKCompileNumberProperty );

	RegisterPropertyEditor( TypeInfo( Cardinal ), TKDFMData, 'DataSize', TKHexaProperty );

	RegisterPropertyEditor( TypeInfo( TDateTime ), TKExpiration, 'ValidDateTime', TKDateTimeProperty );

	RegisterPropertyEditor( TKExceptStringsArray.ClassInfo, TKExcept, 'Messages',
		TKStringsArrayProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomCreateProcess, 'InitialDirectory',
		TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomCreateProcess, 'CommandPath',
		TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomCreateProcess, 'CommandLine',
		TKFileNameProperty );

	RegisterPropertyEditor( TypeInfo( TKMessageGroup ), TKMessageSpy, 'Messages',
		TKMessagesGroupProperty );
	RegisterPropertyEditor( TypeInfo( TIdentMsgMapEntryEnum ), nil, '', TKMsgEnumProperty );
	RegisterPropertyEditor( TypeInfo( TControl ), TKMessageSpy, 'Control',
		TKControlNameProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCompileItem, 'DCUOutPutDir',
		TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCompileItem, 'EXEOutPutDir',
		TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCompileItem, 'InitialDirectory',
		TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCompileItem, 'CompileFileName',
		TKCompileFileNameProperty );
	RegisterPropertyEditor( TypeInfo( Cardinal ), TKCompileItem, 'ImageBaseAddr', TKHexaProperty );
	RegisterPropertyEditor( TypeInfo( Cardinal ), TKCompileItem, 'MinStackSize', TKHexaProperty );
	RegisterPropertyEditor( TypeInfo( Cardinal ), TKCompileItem, 'MaxStackSize', TKHexaProperty );
	RegisterPropertyEditor( TypeInfo( TStrings ), TKCompileItem, '', TKTStringsProperty );
	RegisterPropertyEditor( TypeInfo( Byte ), TKCompileItem, 'ItemOptions', TKCompileItemOptProperty );
  RegisterPropertyEditor( TKDFMResourceStream.ClassInfo, TKDFMResource, 'Resource', TKDFMResourceStreamProperty );

  RegisterPropertyEditor( TypeInfo( TStrings ), TKCustomMathSolver, 'Identifiers', TKMathTermProperty );
  RegisterPropertyEditor( TypeInfo( TStrings ), TKCustomMathSolver, 'Expressions', TKMathTermProperty );

  RegisterMathPropertyEditor( TypeInfo( Integer ), TControl, 'Left' );
  RegisterMathPropertyEditor( TypeInfo( Integer ), TControl, 'Top' );
  RegisterMathPropertyEditor( TypeInfo( Integer ), TControl, 'Height' );
  RegisterMathPropertyEditor( TypeInfo( Integer ), TControl, 'Width' );

{ Component Editor }
	RegisterComponentEditor( TKDCC32, TKDCC32Editor );
	RegisterComponentEditor( TKVersionControl, TKVersionControlEditor );
	RegisterComponentEditor( TKDFMResource, TKDFMResourceComponentEditor );
	RegisterComponentEditor( TKStringsArrayComponent, TKStringsArrayComponentEditor );
	RegisterComponentEditor( TKPropertyExpression, TKPropertyExpressionEditor );

{ Method Comments }

{-------------------------- TKMessageSpy Comments ------------------------------}

	RegisterDefaultMethod( TypeInfo( TKSpyConfirmLog ), TKMessageSpy, 'OnConfirmLog',
		sComSpyConfirmLog );

{-------------------------- TKFileScanner Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKScanEvent ), TKFileScanner, 'OnScan',
		sComFileScan );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKFileScanner, 'AfterStartScan',
		sComFSAfterStartScan );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKFileScanner, 'BeforeStartScan',
		sComFSBeforeStartScan );

{--------------------------- TKExpiration Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKExpirationNotifyEvent ), TKExpiration,
		'OnExpire', sComExpirationExpire );

{----------------------- TKCustomCreateProcess Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TKCreateProcessExecuteEvent ), TKCustomCreateProcess,
		'OnExecute', sComCreateProcessExecute );

{------------------------- TKCompileItem Comments ------------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCompileItem, 'BeforeCompile',
		sComDCC32ItemBeforeCompile );
	RegisterDefaultMethod( TypeInfo( TKDCC32GroupAbortEvent ), TKCompileItem, 'AfterCompile',
		sComDCC32ItemAfterCompile );

{-------------------------------- TKDCC32 Comments  ----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDCC32GroupAbortEvent ), TKDCC32, 'OnGroupAbort',
		sComDCC32GroupAbort );
	RegisterDefaultMethod( TypeInfo( TKDCC32LoadItemErrorEvent ), TKDCC32, 'OnLoadError',
		sComDCC32LoadError );

{----------------------------- TKExcept Comments -------------------------------}

	RegisterDefaultMethod( TypeInfo( TKOnExceptionEvent ), TKExcept, 'OnException',
		sComExptException );
	RegisterDefaultMethod( TypeInfo( TKOnTranslateEvent ), TKExcept, 'OnExceptionTranslate',
		sComExptTranslate );
	RegisterDefaultMethod( TypeInfo( TKOnFeedBackEvent ), TKExcept, 'OnExceptionFeedBack',
		sComExptFeedBack );
	RegisterDefaultMethod( TypeInfo( TKExceptRequestFileNameEvent ), TKExcept,
		'OnRequestFileName', sComExptRequestFileName );
	RegisterDefaultMethod( TypeInfo( TKExceptLogErrorEvent ), TKExcept, 'OnExceptLogError',
		sComExptLogError );

{------------------------------ TKCustomDump Comments  -------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomDump, 'OnSearchAddress',
		sComCustomDumpSearchAddr );

{------------------------------ TKCustomMathSolver Comments  -------------------------}

	RegisterDefaultMethod( TypeInfo( TKMathSolverEvent ), TKCustomMathSolver, 'OnSolve',
		sComMathSvSolve );
	RegisterDefaultMethod( TypeInfo( TKMathSolverIdentEvent ), TKCustomMathSolver, 'OnSolveIdent',
		sComMathSvSolveIdent );
	RegisterDefaultMethod( TypeInfo( TKMathSolverFuncEvent ), TKCustomMathSolver, 'OnSolveFunc',
		sComMathSvSolveFunc );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMathSolver, 'OnExpressionsChanged',
		sComMathSvExprCh );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMathSolver, 'OnIdentifiersChanged',
		sComMathSvIdentCh );

{----------------------- TKPropertyExpression Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKPropertyExpressionNotifySolveEvent ),
    TKPropertyExpression, 'OnNotifySolve', sComPropExprNotifySolve );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPropertyExpression, 'OnLock',
    sComPropExprLock );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPropertyExpression, 'OnUnLock',
    sComPropExprUnLock );

end;

end.
