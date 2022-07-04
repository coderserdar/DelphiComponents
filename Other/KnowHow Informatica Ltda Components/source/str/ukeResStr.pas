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

unit ukeResStr;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{----------------------------------- ukeClasses.pas ----------------------------}

{ MessageSpy Messages }
	sErrInvMsgEnum = 'MessageSpy error: invalid MessageEnum format %s';

	sErrMSInvWndProc = 'MessageSpy error: invalid wrapper window procedure';
	sErrMSInvDefWndProc = 'MessageSpy error: invalid wrapper default window procedure';
	sErrMSInvFileName = 'MessageSpy error: invalid log file name';
	sErrMSInvStream = 'MessageSpy error: stream has not been loaded yet- could not save messages';

{ Mask/WordParser messages }
	sErrFSInvPath = 'FileScanner error: cannot scan %s- path is invalid';
	sMInvalidMask = 'FileScanner error: invalid mask with params (%s and %s)';
	sFSProgressTitle = 'Scanning Files';
	sFSProgressPrompt = 'Current Progress...';
	sErrMLIndexOutRange = 'MaskList error: index out of range';

{ Expiration }
	sErrExprTimeExpr = 'Time Expired for "%s" version';

{ Version Control }
	sErrVCInvInstance = 'VersionControl error: only one TKVersionControl instance allowed per container';
	sErrTooMuchCompilation = 'VersionControl error: too many compilations for this version- try upgrading the system version';
	sVCVersionPattern = '%2.2d.%2.2d.%4.4d %s';

{ DCC32 }
	sErrCIInvFileName = 'The compile filename %s must exists, and must be an unit file (.pas), a project (.dpr), or a package file (.dpk)';
	sErrCIInvUnitAliases = 'There are invalid unit names in the unit aliases list. The values must be in the format OldUnit=NewUnit';
	sErrCIAbortCompile = 'Abort requested on error while compiling item "%s"';
	sErrCICannotLoadFromStream = 'Stream Error: cannot load the compile item from stream';
	sErrCICouldNotParseDOF = 'Could not parse the DOF file for a non project file name ("%s")';
	sErrCIInvArrayCompileIndex = 'Compile item Index out of bounds ("%d")';
	sErrDCC32InvLoadIniStrm = 'Some error(s) occured while trying to load item from ini file. There was(were) %d error(s) with the following message(s):'#13#10'%s'; 

{ Shell CommandLine }
	sKnowhowShell = 'KnowHow Command Shell v1.0';
	sKnowhowSep   = '--------------------------';

	sShellHelp = 'Help';
	sShellHelpCmd = #13#10'Command: %s'#13#10#13#10'Syntax: %s'#13#10#13#10;
	sShellNoHelpCmd = 'No help associated with this command';
	sShellUnknownCmd = #13#10'Unknown command: %s';
	sShellHelpCmdUsage = '%s <command>'#13#10'Use this command to learn the syntax and/or usage of a given shell command';
	sShellCmdSyntaxError = #13#10'Syntax error for command ''%s''.'#13#10#13#10'Syntax: %s'#13#10#13#10;

	sErrShellException = #13#10'A %s exception was raised while executing "%s" with the following message: %s'#13#10;
	sErrShellNotConsole = 'Shell error: cannot instantiate a TKShell object because this is not a console application';
	sErrCmdLnUnCloseQuote = 'Shell error: unclosed quotations %s';
	sErrShellConsoleExists = 'Shell error: a Shell object has already been created';

{ Extended Parsers }
  sErrInvSpecial = 'invalid special token';
  sErrParseInvStr = 'invalid string constant';
  sErrParseInvBin = 'Invalid binary value';
  
{ TKCmdLineLexer }
	sErrInvSwitch = 'CmdLineLexer error: invalid command switch';
	sErrInvAppName = 'CmdLineLexer error: invalid application name (%s)';

{ Math Parser/Lexer/Solver }
	sErrInvMathOp = 'invalid mathematical operator';
	sErrInvMathExpr = 'expression expected';
	sErrInvFloatOp = 'Cannot perform this operation with a float number';
	sErrInvMathOpKind = 'Invalid math operation %s. Expression excepted';

	sErrInvMathFuncExprSolve = 'Could not solve the function expression %s outside a function definition';
	sErrInvFuncParamCount = 'Invalid function parameter count: %d parameter(s) expected for function %s but %d found';
  sErrInvDefFunc = 'Invalid default function %s: function cannot be evaluated';
  sErrInvPEDefFuncOwner = 'Invalid owner class %s for property expression default function solving';

	sErrInvMathFunc = 'Invalid math function %s at expression %d';
  sErrInvMathIdent = 'Invalid math identifier %s at expression %d';
  sErrInvMathIdentType = 'Invalid math identifier %s type at expression %d (Type: %d)';
  sErrInvMathExprType = 'Invalid (left side) math expression %s at %d (Type: %d)';

{----------------------------------- ukeUtils.pas ------------------------------}

	sMessagesInvName = 'Invalid KnowHow® message mapping name (%s)';
	sMessagesInvValue = 'Invalid KnowHow® message mapping value (%d)';

{---------------------------------- ukeEngines.pas -----------------------------}

	sErrExInvExceptAddr = 'ExceptLogFile error: invalid except error address';
	sErrExInvFilekMapCreation = 'ExceptLogFile error: KnowHow® map file creation failure (%s)';
	sErrExInvMultiStrExceptEditor = 'ExceptLogFile error: management error';

	sExceptMessage = 'An error occurred in your application with the given message: ';

{
 ===============================================================================
	 End of Revision 100, July 26, 1999
 ===============================================================================
}

	{ August 27, 1999 }
	
	sErrInvCustomDumpAddr = 'Invalid file Address. Could not perform this operation';
	sErrInvCustomDumpOffSet = 'Invalid address offset bound %d. It must be between 0..%d';
	sErrInvHexDumpHexAddr = 'Invalid hexa address: $%.8x';

  { January, 24, 2000 }

  sDefFuncFormula = '<No func formula>';
  sDefFuncComment = '<No func comment>';
  sDefIdentComment = '<No ident comment>';
  sDefGroupComment = '<No group comment>';

  sErrInvMathSolverExpr = 'Could not open %s. It must have at least one expression';
  sErrInvMathExprRegFuncParamCnt = 'Could not register solver function %s. Declared (%d) and passed (%d) parameter list count differ';
  sErrInvMathExprRegFuncOpenParams = 'Could not register solver function %s. Open array functions MUST have at least one parameter';

	sErrPEInvInstance = 'Property Expression error: only one TKPropertyExpression instance allowed per container';
  sErrPEInvExprAssign = 'There are some invalid expressions in the expression set. %d original expressions would be converted to %d valid ones. Check the missed expressions and make it correct (this warning appears only at design-time)';

  sErrInvRelativeAnchorParams = 'Could not evaluate relative expressions: invalid parameters';
  sErrInvRelativeAnchorInvProp = 'Could not evaluate relative expressions: property %s is invalid or not found';
  sErrInvRelativeAnchorOwnerComps = 'Could not evaluate relative expressions: anchor owner component %d does not has a valid name (property %s)';

{ January, 31, 2000 }

  sCancelExpr = 'Are you sure you want to cancel expression?';
  sFunParamEval = 'Function %s; Param(%d) %s';
  sNoHelp = 'There is no help information for this item';

  sFunctions = 'Functions';
  sIdentName = 'Identifier Name for %s';
  sIdentifiers = 'Identifiers';
  sMathOperators = 'Operators';
  sRegistered = 'Registered';
  sComponents = 'Components';

implementation

{

PS: This text must not be after END., because of a Delphi4 warning...

	Deprecated, Revision 100, July 26, 1999
	---------------------------------------

ukeClasses.pas
	sSMOnlyOne = 'Only one instance of %s allowed';
	sErrInvCmdName = 'Invalid command name (%s)';

}

end.

