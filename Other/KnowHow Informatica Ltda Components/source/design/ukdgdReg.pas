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

unit ukdgdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	DsgnIntf, Classes, uksydClasses, ukrClasses, ukrCtrls, ukrdClasses,
	ukrdMComments, ukdgClasses, ukdgUtils, ukdgdConsts, ukdgdClasses,
	ukdgdMComments;

procedure Register;
begin

{ Components }
	RegisterComponents( sRegDialogs, [TKDialog, TKShowDialog, TKInputDialog,
		TKInputListDialog, TKInputCheckListDialog,	TKProgressDialog, TKLogonDialog,
		TKPasswordDialog, TKPrintDialog, TKBackupDialog, TKCryptoDialog, TKDualListDialog,
		TKSpeedFloatDialog, TKSpeedIntegerDialog, TKSpeedHexaDialog, TKSpeedDateDialog,
		TKSpeedTimeDialog, TKSpeedDateTimeDialog, TKSpeedTextDialog] );

{ Component Editor }
	RegisterComponentEditor( TKBaseDialog, TKDialogEditor );

{ Property Editors }

	RegisterPropertyEditor( TypeInfo( Extended ), TKSpeedFloatDialog, '', TKSpeedFloatProperty );

	RegisterPropertyEditor( TypeInfo( LongInt ), TKSpeedIntegerDialog, '', TKSpeedIntegerProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKSpeedHexaDialog, '', TKSpeedHexaProperty );

	RegisterPropertyEditor( TypeInfo( TDateTime ), TKDateSpeedProperties, '', TKSpeedDateDialogProperty );
	RegisterPropertyEditor( TypeInfo( TDateTime ), TKTimeSpeedProperties, '', TKSpeedTimeDialogProperty );
	RegisterPropertyEditor( TypeInfo( TDateTime ), TKDateTimeSpeedProperties, '', TKSpeedDateTimeDialogProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKSpeedFloatDialog, 'EditFormat', TKFloatFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSpeedFloatDialog, 'DisplayFormat', TKFloatFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKSpeedIntegerDialog, 'EditFormat', TKIntegerFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSpeedIntegerDialog, 'DisplayFormat', TKIntegerFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKSpeedDateDialog, 'EditFormat', TKDateFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSpeedDateDialog, 'DisplayFormat', TKDateFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKSpeedTimeDialog, 'EditFormat', TKTimeFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSpeedTimeDialog, 'DisplayFormat', TKTimeFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKSpeedDateTimeDialog, 'EditFormat', TKDateTimeFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKSpeedDateTimeDialog, 'DisplayFormat', TKDateTimeFormatProperty );

{ We *MUST* register the property editor distinctly for the two selection properties }
	RegisterPropertyEditor( TypeInfo( TKBitsWrapper ), TKDualListDialog, 'DestSel', TKDualListDialogBitsProperty );
	RegisterPropertyEditor( TypeInfo( TKBitsWrapper ), TKDualListDialog, 'SourceSel', TKDualListDialogBitsProperty );

{ Method Comments }
  
{-------------------------- TKBaseDialog Comments ------------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogCheckEvent ), TKBaseDialog, 'OnCheckParams',
		sComBaseDlgCheckParams );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKBaseDialog, 'AfterCheckParams',
		sComBaseDlgAfterCheckParams );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKBaseDialog, 'BeforeCheckParams',
		sComBaseDlgBeforeCheckParams );

{------------------------- TKCustomDialog Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogExecuteEvent ), TKCustomDialog, 'AfterExecute',
		sComCustomDlgAfterExec );
	RegisterDefaultMethod( TypeInfo( TKDialogExecuteEvent ), TKCustomDialog, 'BeforeExecute',
		sComCustomDlgBeforeExec );

{---------------------------- TKDialog Comments --------------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogEvent ), TKDialog, 'OnPlaceButtons',
		sComDlgPlaceBtns );
	RegisterDefaultMethod( TypeInfo( TKDialogEvent ), TKDialog, 'OnPlaceControls',
		sComDlgPlaceCtrls );
	RegisterDefaultMethod( TypeInfo( TKDialogEvent ), TKDialog, 'OnPrepareForm',
		sComDlgPrepareFrm );
	RegisterDefaultMethod( TypeInfo( TKDialogEvent ), TKDialog, 'OnUnPrepareForm',
		sComDlgUnPrepareFrm );

{------------------------- TKProgressDialog Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogExecuteEvent ), TKProgressDialog, 'OnVisualProgress',
		sComPrgsDlgVisualPrgs );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKProgressDialog, 'OnProgress',
		sComPrgsDlgPrgs );

{------------------------- TKSpeedFloatDialog Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TKFormatFloatTextEvent ), TKSpeedFloatDialog,
		'OnFormatDisplayText', sComSpFltFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatFloatTextEvent ), TKSpeedFloatDialog,
		'OnFormatEditText', sComSpFltFmtEdt );

{---------------------- TKCustomSpeedIntegerDialog Comments --------------------}

	RegisterDefaultMethod( TypeInfo( TKFormatIntegerTextEvent ), TKCustomSpeedIntegerDialog,
		'OnFormatDisplayText', sComSpIntFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatIntegerTextEvent ), TKCustomSpeedIntegerDialog,
		'OnFormatEditText', sComSpIntFmtEdt );

{----- TKSpeedDateDialog/TKSpeedTimeDialog/TKSpeedDateTimeDialog Comments -----}

	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedDateDialog,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedDateDialog,
		'OnFormatEditText', sComSpDTFmtEdt );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedTimeDialog,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedTimeDialog,
		'OnFormatEditText', sComSpDTFmtEdt );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedDateTimeDialog,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKSpeedDateTimeDialog,
		'OnFormatEditText', sComSpDTFmtEdt );

{-------------------------- TKSpeedTextDialog Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKSpeedTextDialog, 'OnButtonClick',
		sComSpCtrlBtnClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKSpeedTextDialog, 'OnCheckValue',
		sComSpTxtCheckValue );

{-------------------------- TKCustomLogonDialog Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKCustomLogonDialog, 'OnCancel',
		sComLogOnDlgCancel );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKCustomLogonDialog, 'OnFail',
		sComLogOnDlgFail );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKCustomLogonDialog, 'OnLastFail',
		sComLogOnDlgLastFail );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKCustomLogonDialog, 'OnSucceed',
		sComLogOnDlgSucceed );

{------------------------- TKSystemDialog Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKStayOnTopSystemDialogEvent ), TKSystemDialog,
		'OnButtonClick', sComSysDlgBtnClk );
		
{---------------------------- TKLogonDialog Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKValidateLogonEvent ), TKLogonDialog, 'OnValidate',
		sComLogOnDlgValidate );

{--------------------------- TKPasswordDialog Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKValidatePasswordEvent ), TKPasswordDialog, 'OnValidate',
		sComPwdDlgValidate );
	RegisterDefaultMethod( TypeInfo( TKDialogNotifyEvent ), TKPasswordDialog, 'OnPasswordMismatch',
		sComPwdDlgPwdMismatch );

{---------------------------- TKPrintDialog Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPrintDialog, 'OnPrint',
		sComPrintDlgPrint );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPrintDialog, 'OnPreview',
		sComPrintDlgPreview );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPrintDialog, 'OnCancel',
		sComPrintDlgCancel );

{---------------------------- TKBackupDialog Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogActionEvent ), TKBackupDialog, 'OnBackup',
		sComBackupDlgBackup );
	RegisterDefaultMethod( TypeInfo( TKDialogActionEvent ), TKBackupDialog, 'OnRestore',
		sComBackupDlgRestore );

{---------------------------- TKCryptoDialog Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKDialogActionEvent ), TKCryptoDialog, 'OnEncipher',
		sComCryptoDlgEncipher );
	RegisterDefaultMethod( TypeInfo( TKDialogActionEvent ), TKCryptoDialog, 'OnDecipher',
		sComCryptoDlgDecipher );

{----------------------- TKCustomDualListDialog Comments -----------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomDualListDialog,
		'OnSourceListClick', sComDualLstDlgListClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomDualListDialog,
		'OnDestinationListClick', sComDualLstDlgListClick );
	RegisterDefaultMethod( TypeInfo( TKDualListButtonClickEvent ), TKCustomDualListDialog,
		'OnDualListButtonClick', sComDualLstDlgBtnClick );

{------------------------ TKDualListEditDialog Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDualListEditDialog,
		'OnEditButtonClick', sComDualLstEdtDlgEdtBtnClick );

end;

end.
