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

unit ukedMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{-------------------------- TKMessageSpy Comments ------------------------------}

{
	TKMessageSpy.OnConfirmLog: TKSpyConfirmLog = procedure( Sender: TKMessageSpy;
		const Message: TMessage; const sMsg: string; IsWndProc: Boolean;
		var Confirm: Boolean ) of object; 
}

	sComSpyConfirmLog =
	'» Parameters: Sender = TKMessageSpy; Message = current message being'#13#10 +
	'    processed by the MessageSpy; sMsg = the string translation of the'#13#10 +
	'    captured message; IsWndProc = if true, the event was called right before'#13#10 +
	'    the WndProc of the object, but, if it is false, the event was called'#13#10 +
	'    from within the DefWndProc of the object; var Confirm = set confirm to'#13#10 +
	'    false in order to avoid this message from being logged ( default = true ).'#13#10 +
	'» Invoked for each message ( matching the current filter ) received by the object.'#13#10 +
	'» Use this event to determine if logging is appropriate. Avoid processing long'#13#10 +
	'    tasks within this handler.';

{-------------------------- TKFileScanner Comments -----------------------------}

{
	TKFileScanner.OnScan: TKScanEvent = procedure( Sender: TObject; const FilePath:
		string; CurLevel: Word; FileInfo: TFileInfo; var Cancel: Boolean ) of object;
}
	sComFileScan =
	'» Parameters: Sender = TKFileScanner; FilePath = current path being'#13#10 +
	'    processed by the file scanner; CurLevel = current level of the'#13#10 +
	'    path being processed by the file scanner, indicating the current'#13#10 +
	'    position in the directory hierarchy down from the original path;'#13#10 +
	'    FileInfo = current file''s information; var Cancel = set Cancel to'#13#10 +
	'    false in order to cancel scanning right away ( default = false ).'#13#10 +
	'» Invoked for each file the file scanner finds according to the file masks.'#13#10 +
	'» Use this event to take proper action for every file you need to process.';

{
	TKFileScanner.AfterStartScan: TNotifyEvent
}
	sComFSAfterStartScan = '';

{
	TKFileScanner.BeforeStartScan: TNotifyEvent
}
	sComFSBeforeStartScan = '';

{--------------------------- TKExpiration Comments -----------------------------}

{
	TKExpiration.OnExpire: TKExpirationNotifyEvent = procedure ( Sender: TKExpiration;
		var Message: string ) of object;
}
	sComExpirationExpire = '';

{----------------------- TKCustomCreateProcess Comments ------------------------}

{
	TKCustomCreateProcess.OnExecute: TKCreateProcessExecuteEvent = procedure(
		Sender: TKCustomCreateProcess; pi: TProcessInformation ) of object;
}
	sComCreateProcessExecute = '';

{------------------------- TKCompileItem Comments ------------------------------}

{
	TKCompileItem.BeforeCompile: TNotifyEvent
}
	sComDCC32ItemBeforeCompile = '';

{
	TKCompileItem.AfterCompile: TNotifyEvent
}
	sComDCC32ItemAfterCompile = '';
	
{-------------------------------- TKDCC32 Comments  ----------------------------}

{
	TKDCC32.OnGroupAbort: TKDCC32GroupAbortEvent = procedure ( Sender: TKDCC32;
		Item: TKCompileItem; const GroupIndex: Cardinal; var AbortGroup: Boolean ) of object;
}
	sComDCC32GroupAbort = '';

{
	TKDCC32.OnLoadError: TKDCC32LoadItemErrorEvent = procedure ( Sender: TKDCC32;
		Item: TKCompileItem; const GroupIndex: Cardinal ) of object;
}
	sComDCC32LoadError = '';

{----------------------------- TKExcept Comments -------------------------------}

{
	TKExcept.OnException: TKOnExceptionEvent =	procedure( Sender: TKExcept; E: Exception;
		Known: Boolean;	var Display, Feedback: Boolean ) of object;
}
	sComExptException = '';

{
	TKExcept.OnExceptionTranslate: TKOnTranslateEvent = procedure ( Sender: TKExcept;
		E: Exception; var MsgTranslated: Boolean ) of object;
}
	sComExptTranslate = '';

{
	TKExcept.OnExceptionFeedBack: TKOnFeedBackEvent = procedure( Sender: TKExcept;
		E: Exception; const ExceptOriginal: string; var Comment: string ) of object;
}
	sComExptFeedBack = '';

{
	TKExcept.OnRequestFileName: TKExceptRequestFileNameEvent = procedure( Sender: TKExcept;
		E: Exception; var FileName: string;	const FileDate: TDateTime;
		const FileSize: LongInt ) of object;
}
	sComExptRequestFileName = '';

{
	TKExcept.OnExceptLogError: TKExceptLogErrorEvent = procedure( Sender: TKExcept;
		E: Exception; var LogErrorAction: TKLogErrorAction ) of object;
}
	sComExptLogError = '';

{-------------------------- TKCustomDump Comments ------------------------------}

{
	TKCustomDump.OnSearchAddress: TNotifyEvent;
}
	sComCustomDumpSearchAddr = '';

{------------------------ TKCustomMathSolver Comments --------------------------}

{
	TKCustomMathSolver.OnSolve: TKCustomMathSolverEvent = procedure( Sender: TKCustomMathSolver;
		Expr: TKMathExpression; ExprKind: TKMathExprKind; const StrExpr: string;
		const ReturnValue: Extended ) of object;

}
	sComMathSvSolve = '';

{
	TKCustomMathSolver.OnSolveIdent: TKCustomMathSolverIdentEvent = procedure( Sender: TKCustomMathSolver;
		Expr: TKMathExpression; const IdentName: string; var IdentValue: Extended ) of object;

}
	sComMathSvSolveIdent = '';

{
	TKCustomMathSolver.OnSolveFunc: TKCustomMathSolverFuncEvent = procedure( Sender: TKCustomMathSolver;
		Expr: TKMathExpression; const FuncName: string; FuncParams: TKExtendedStack;
		var FuncReturn: Extended ) of object;

}
	sComMathSvSolveFunc = '';

{
  TKCustomMathSolver.OnExpressionsChanged: TNotifyEvent
}
	sComMathSvExprCh = '';

{
  TKCustomMathSolver.OnIdentifiersChanged: TNotifyEvent
}
  sComMathSvIdentCh = '';

{----------------------- TKPropertyExpression Comments -------------------------}

{
	TKPropertyExpression.OnNotifySolve: TKPropertyExpressionNotifySolveEvent = procedure(
    Sender: TKPropertyExpression; ExprIdx: Integer; Obj: TPersistent; PropInfo: PPropInfo;
    const EvalValue: Extended ) of object;

}
	sComPropExprNotifySolve = '';

{
	TKPropertyExpression.OnLock: TNotifyEvent

}
	sComPropExprLock = '';

{
	TKPropertyExpression.OnUnLock: TNotifyEvent

}
	sComPropExprUnLock = '';

implementation

end.
