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

unit ukdgdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{-------------------------- TKBaseDialog Comments ------------------------------}

{
	TKBaseDialog.OnCheckParams: TKDialogCheckEvent = procedure( Sender: TKBaseDialog;
		var ParamsChecked: Boolean ) of object;
}

	sComBaseDlgCheckParams = '';

{
	TKBaseDialog.AfterCheckParams: TKDialogNotifyEvent = procedure( Sender: TKBaseDialog ) of object;
}

  sComBaseDlgAfterCheckParams = '';

{
	TKBaseDialog.BeforeCheckParams: TKDialogNotifyEvent = procedure( Sender: TKBaseDialog ) of object;
}	

	sComBaseDlgBeforeCheckParams = '';

{------------------------- TKCustomDialog Comments -----------------------------}

{
	TKCustomDialog.AfterExecute: TKDialogExecuteEvent = procedure( Sender: TKCustomDialog;
		AForm: TKCustomDialogForm ) of object;
}

	sComCustomDlgAfterExec = '';

{
	TKCustomDialog.BeforeExecute: TKDialogExecuteEvent = procedure( Sender: TKCustomDialog;
		AForm: TKCustomDialogForm ) of object;
}

	sComCustomDlgBeforeExec = '';

{---------------------------- TKDialog Comments --------------------------------}

{
	TKDialog.OnPlaceButtons: TKDialogEvent = procedure( Sender: TKDialog;
		Form: TKDialogForm ) of object;
}

	sComDlgPlaceBtns = '';

{
	TKDialog.OnPlaceControls: TKDialogEvent = procedure( Sender: TKDialog;
		Form: TKDialogForm ) of object;
}

	sComDlgPlaceCtrls = '';

{
	TKDialog.OnPrepareForm: TKDialogEvent = procedure( Sender: TKDialog;
		Form: TKDialogForm ) of object;
}

	sComDlgPrepareFrm = '';

{
	TKDialog.OnUnPreprareForm: TKDialogEvent = procedure( Sender: TKDialog;
		Form: TKDialogForm ) of object;
}

	sComDlgUnPrepareFrm = '';
                      
{------------------------- TKSystemDialog Comments -----------------------------}

{
	TKSystemDialog.OnButtonClick: TKStayOnTopSystemDialogEvent = procedure( Sender:
		TKSystemDialog; Button: TBitBtn; var Close: Boolean ) of object;
}

	sComSysDlgBtnClk = '';

{------------------------- TKProgressDialog Comments -----------------------------}

{
	TKProgressDialog.OnVisualProgress: TKVisualProgressEvent = procedure(
		Sender: TKProgressDialog; var Status: string; var Percent: TKPercent;
		var Cancel: Boolean; var ProgressInfoStyle: TKProgressInfoStyle ) of object;
}

	sComPrgsDlgVisualPrgs = '';

{
	TKProgressDialog.OnProgress: TNotifyEvent
}

	sComPrgsDlgPrgs = '';

{-------------------------- TKCustomLogonDialog Comments -------------------------}

{
	TKCustomLogonDialog.OnCancel: TKDialogNotifyEvent

}
	sComLogOnDlgCancel = '';

{
	TKCustomLogonDialog.OnFail: TKDialogNotifyEvent

}
	sComLogOnDlgFail = '';

{
	TKCustomLogonDialog.OnLastFail: TKDialogNotifyEvent

}
	sComLogOnDlgLastFail = '';

{
	TKCustomLogonDialog.OnSucceed: TKDialogNotifyEvent

}
	sComLogOnDlgSucceed = '';

{---------------------------- TKLogonDialog Comments ---------------------------}

{
	TKLogonDialog.OnValidate: TKValidateLogonEvent = procedure( Sender: TKLogonDialog;
		const Logon, Password: string; var IsValid: Boolean ) of object;

}
	sComLogOnDlgValidate = '';

{--------------------------- TKPasswordDialog Comments -------------------------}

{
	TKPasswordDialog.OnValidate: TKValidatePasswordEvent = procedure( Sender: TKPasswordDialog;
		const Logon, Password, NewPassword: string; var IsValid: Boolean ) of object;

}
	sComPwdDlgValidate = '';

{
	TKPasswordDialog.OnPasswordMismatch: TKDialogNotifyEvent

}
	sComPwdDlgPwdMismatch = '';

{---------------------------- TKPrintDialog Comments ---------------------------}

{
	TKPrintDialog.OnPrint: TNotifyEvent

}
	sComPrintDlgPrint = '';

{
	TKPrintDialog.OnPreview: TNotifyEvent

}
	sComPrintDlgPreview = '';

{
	TKPrintDialog.OnCancel: TNotifyEvent

}
	sComPrintDlgCancel = '';

{---------------------------- TKBackupDialog Comments ---------------------------}

{
	TKBackupDialog.OnBackup: TKDialogActionEvent = procedure( Sender: TKCustomActionDialog;
		CallBack: TKRefreshActionDlgProc ) of object;

}
	sComBackupDlgBackup = '';

{
	TKBackupDialog.OnRestore: TKDialogActionEvent = procedure( Sender: TKCustomActionDialog;
		CallBack: TKRefreshActionDlgProc ) of object;

}
	sComBackupDlgRestore = '';

{---------------------------- TKCryptoDialog Comments ---------------------------}

{
	TKCryptoDialog.OnEncipher: TKDialogActionEvent = procedure( Sender: TKCustomActionDialog;
		CallBack: TKRefreshActionDlgProc ) of object;

}
	sComCryptoDlgEncipher = '';

{
	TKCryptoDialog.OnDecipher: TKDialogActionEvent = procedure( Sender: TKCustomActionDialog;
		CallBack: TKRefreshActionDlgProc ) of object;

}
	sComCryptoDlgDecipher = '';

{----------------------- TKCustomDualListDialog Comments -----------------------}

{
	TKCustomDualListDialog.OnSourceListClick: TNotifyEvent;
	TKCustomDualListDialog.OnDestinationListClick: TNotifyEvent;

}
	sComDualLstDlgListClick = '';

{
	TKCustomDualListDialog.OnDualListButtonClick: TKDualListButtonClickEvent = procedure (
		Sender: TKCustomDualListDialog; ButtonClicked: TKDualListButtonKind ) of object;

}
	sComDualLstDlgBtnClick = '';

{------------------------ TKDualListEditDialog Comments ------------------------}

{
	TKDualListEditDialog.OnEditButtonClick: TNotifyEvent;

}
	sComDualLstEdtDlgEdtBtnClick = '';

implementation

(*

Get from ukrdMComments.pas (kernel)

{------------------------- TKSpeedFloatDialog Comments ------------------------}

{
	TKSpeedFloatDialog.OnFormatDisplayText: TKFormatFloatTextEvent = procedure(
		Sender: TObject; const AValue: Extended; const AFormat: string; var AText:
		string ) of object;

}
	sComSFltDlgFmtDisp = '';

{
	TKSpeedFloatDialog.OnFormatEditText: TKFormatFloatTextEvent = procedure(
		Sender: TObject; const AValue: Extended; const AFormat: string; var AText:
		string ) of object;

}
	sComSFltDlgFmtEdt = '';

{---------------------- TKCustomSpeedIntegerDialog Comments --------------------}

{
	TKCustomSpeedIntegerDialog.OnFormatDisplayText: TKFormatIntegerTextEvent = procedure(
		Sender: TObject; const AValue: Integer; const AFormat: string; var AText:
		string ) of object;

}
	sComSIntDlgFmtDisp = '';

{
	TKCustomSpeedIntegerDialog.OnFormatEditText: TKFormatIntegerTextEvent = procedure(
		Sender: TObject; const AValue: Integer; const AFormat: string; var AText:
		string ) of object;

}
	sComSIntDlgFmtEdt = '';

{----- TKSpeedDateDialog/TKSpeedTimeDialog/TKSpeedDateTimeDialog Comments -----}

{
	TKSpeedDateTimeDialog.OnFormatDisplayText: TKFormatDateTimeTextEvent = procedure(
		Sender: TObject; const AValue: TDateTime; const AFormat: string; var AText:
		string ) of object;

}
	sComSpDTDlgFmtDisp = '';

{
	TKSpeedDateTimeDialog.OnFormatEditText: TKFormatDateTimeTextEvent = procedure(
		Sender: TObject; const AValue: TDateTime; const AFormat: string; var AText:
		string ) of object;

}
	sComSpDTDlgFmtEdt = '';

{-------------------------- TKSpeedTextDialog Comments -------------------------}

{
	TKSpeedTextDialog.OnButtonClick: TNotifyEvent

}

	sComSpTxtDlgBtnClick = '';

{
	TKSpeedTextDialog.OnCheckValue: TKCheckTextEvent = procedure( Sender: TObject;
		var NewValue: string ) of object;

}

	sComSpTxtDlgCheckValue = '';

*)

end.
