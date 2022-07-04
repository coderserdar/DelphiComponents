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

unit ukedConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	uksyConsts, ukrMessages;

resourcestring

	sRegExtended = 'KnowHow Extended';

{------------------------------ ukedClasses.pas --------------------------------}

	sErrVerCtrlInvAddInReg = 'An unexpected error occured while trying to register the AddIn notifier for version control %s';
	sErrVerCtrlInvAddInUnReg = 'An unexpected error occured while trying to unregister the AddIn notifier for version control %s';

	sDFMResComp = 'Select Resource';

	sErrDPEInvModReg = 'An unexpected error occured while trying to register the module notifier for property expression component %s';
	sErrDPEInvModUnReg = 'An unexpected error occured while trying to unregister the module notifier for property expression component %s';

	sPropertyExprRegPropEdtVerb = 'Register Property Editor';
	sPropertyExprRegPropEdtEnumVerb = 'Enum Reg. Properties';
  sPropertyExprRelativeExprsVerb = 'Create Relative Expressions';
	sPropertyExprClearExprsVerb = 'Clear Property Expressions';
  sPropertyExprLockExprsVerb = 'Lock Expressions';
  sPropertyExprUnLockExprsVerb = 'UnLock Expressions';

	sPropExprRegPropTypeInfoInt = 'The type of your property is integer ?';
	sPropExprRegPropPrompt = 'Select a property name';
	sPropExprRegPropEdt = 'WARNING!!!'#13#10'Are you sure to register a new property editor for all controls?'#13#10'There is no "unregister" method, so if you would like to unregister this property editor you MUST close the delphi IDE';
  sPropExprWarnNoRegMathProps = 'Could not create relative expressions. There is no math property editors registered';
	sPropExprClearExprs = 'Are you sure do you want to delete all property expressions ?';
  sPropExprRelativeExprs = 'Are you sure do you want to create relative expressions? All old expressions will be deleted';
	sPropExprRegInt = 'Properties register for integer types: ';
	SPropExprRegFloat = 'Properties register for float types: ';
  sPropExprUseCtrlParent = 'Relative to Parent(Y) or Owner(N) ?';

  sErrInvMathPropTypeInfo = 'Could install math property for %s. Invalid type info';
  sErrInvMathPropAlreadyInst = 'Property editor for %s of type kind %s already instaled';
  sErrInvMathPropEnumList = 'Could not enumerate math property editors. There is no one instaled';

  sErrInvMathExprExprEdtCls = 'Invalid math solver class %s for expression editor';
  sErrMathIdentError = 'Math Identifier (''%s'') error: either it already exists, is empty or not valid';

  sMathValueNotEval = '<not eval.>';

  sPropExprEdEvalFmtTitle = 'Experession Evaluation Format';
  sPropExprEdEvalFmtPrompt = 'Select a format string (MUST be FormatFloat compatible)';
  sPropExprEdEvalFmt = 'The current evaluate format is: %s';

{----------------------------- ukedfDCC32Opt.pas -------------------------------}

	COMPILE_ITEM_FORM_CAPTION = 'Compile Item %s options';

	sErrInvSaveDefItem = 'An error occured while trying to save the default compliation item. Item Name "%s"; Original message "%s"';
	sErrInvLoadDefItem = 'An error occured while trying to loead the default compliation item. Original message "%s" (DefFileName %s)';

	sDCC32DOFParseReplace = 'Parse the DOF file will replace the actual options for the compile item "%s". Are you sure to subscribe the item options?';

	sCompFNFilter = 'Delphi Files (*.dpr, *.dpk)|*.dpr;*.dpk|Delphi Project File (*.dpr)|*.dpr|Delphi Package File (*.dpk)|*.dpk|Delphi Unit File (*.pas)|*.pas';

	sBrowseDir = 'Choose a directory';
	sDiscardChanges = 'If you cancel, all changes will be discarted.'#13#10'Are you sure to cancel?';

	sCompFNLoad = 'Load Delphi File';
	sRTPFilter = 'Run Time Packages (.dpl)|*.dpl';
	sRTPTitle = 'Load RunTime Packages';

	sErrInvOption = 'Cannot extract the correct compiler switch';
	sErrInvSwitch = 'Cannot extract the correct compiler option';

	sCIConfirmAppend = 'Do you want to load the default options to item "%s"?';

	sDCC32DefExt = '.dcc';
	sDCC32Filter = 'DCC32 Resource Files (*.dcc)|*.dcc';
	sDCC32MergeTitle = 'Merge DCC32 options from file';
	sDCC32LoadTitle = 'Load DCC32 options from file';
	sDCC32SaveTitle = 'Save DCC32 options to file';

	sDCC32ConfirmDel = 'The file "%s" already exists. Do you want to substitute this file?';

	sDCC32DOFParseSucessful = 'DOF file sucessful parsed for compile item %s';

const

	DFM_RES_VERBCOUNT = 1;
	DFM_RES_DEFEXT = '*.bmp';
	DFM_RES_FILTER = 'Image Files (*.bmp;*.gif;*.jpg;*.pcx;*.emf;*.ico)|*.bmp;' +
		'*.gif;*.jpg;*.pcx;*.emf;*.ico|Multimedia files (*.wav;*.avi)|*.wav;*.avi|' +
		'Textual files (*.pas;*.inc;*.int;*.sql;*.txt;*.log;*.bat)|' +
		'*.pas;*.inc;*.int;*.sql;*.txt;*.log;*.bat|All Files(*.*)|*.*';


	DCC32_VERBCOUNT = 3;
	DCC32_VERB: array[0..DCC32_VERBCOUNT - 1] of string[15] =
	 ( 'Merge From File', 'Load From File', 'Save To File' );

	COMPILE_ITEM_DEF_NAME = 'kcidef.dcc';

	EDT_INITDIR = 0;
	EDT_EXEDIR  = 1;
	EDT_DCUDIR  = 2;
	EDT_UNITDIR = 3;
	EDT_RESDIR  = 4;
	EDT_INCDIR  = 5;
	EDT_OBJDIR  = 6;

	imeDescription: array[imeWM..imeCN] of string[22] =
	(
		'Window Messages',          { imeWM  }
		'Button Messages',          { imeBM  }
		'ListBox Notifications',    { imeLBN }
		'ListBox Messages',         { imeLBM }
		'ComboBox Notifications',   { imeCBN }
		'ComboBox Messages',        { imeCBM }
		'Edit Notifications',       { imeEN  }
		'Edit Messages',            { imeEM  }
		'Scroll Bar Messages',      { imeSBM }
		'Dialog Messages',          { imeDM  }
		'Status Bar Commands',      { imeSBC }
		'StatusBar Messages',       { imeSM2 }
		'Component Messages',       { imeCM  }
		'Control Notifications'     { imeCN  }
	);

 PROPEXPR_LOCK_VERB: array[Boolean] of Pointer =
   ( @sPropertyExprUnLockExprsVerb, @sPropertyExprLockExprsVerb );

 PROPEXPRED_DEF_FLTFMTCHARSET = [CH_GRIDLING, CH_PLUS_SIGN, CH_MINUS_SIGN, CH_DOTMARK,
   CH_LIST_TOKEN, CH_COMMA, '0', 'E', 'e'];

 PROPEXPRED_BASE_REGKEY = KNOWHOW_PACKAGES_BASE_REGKEY + 'Ext\' +
   KNOWHOW_PROPERTY_EDITOR_SECTION + 'PropExprEd';
 PROPEXPRED_EVAL_FMT = 'EvalFmt';

implementation

end.