program pkRebuild;

uses
	uksyClasses in 's:\v100\source\system\uksyClasses.pas',
	uksyConsts in 's:\v100\source\system\uksyConsts.pas',
	uksyPackReg in 's:\v100\source\system\uksyPackReg.pas',
	uksyRegCheck in 's:\v100\source\system\uksyRegCheck.pas',
	uksyShortCuts in 's:\v100\source\system\uksyShortCuts.pas',
	uksyTypes in 's:\v100\source\system\uksyTypes.pas',
	uksyUtils in 's:\v100\source\system\uksyUtils.pas',

	ukrClasses in 's:\v100\source\kernel\ukrClasses.pas',
	ukrConsts in 's:\v100\source\kernel\ukrConsts.pas',
	ukrCtrls in 's:\v100\source\kernel\ukrCtrls.pas',
	ukrDBCtrls in 's:\v100\source\kernel\ukrDBCtrls.pas',
	ukrDBUtils in 's:\v100\source\kernel\ukrDBUtils.pas',
	ukrEngines in 's:\v100\source\kernel\ukrEngines.pas',
	ukrfGradEdit in 's:\v100\source\kernel\ukrfGradEdit.pas' {frmGradientProps},
	ukrLanguage in 's:\v100\source\kernel\ukrLanguage.pas',
	ukrMessages in 's:\v100\source\kernel\ukrMessages.pas',
	ukrPackReg in 's:\v100\source\kernel\ukrPackReg.pas',
	ukrTypes in 's:\v100\source\kernel\ukrTypes.pas',
	ukrUtils in 's:\v100\source\kernel\ukrUtils.pas',

	uksConsts in 's:\v100\source\std\uksConsts.pas',
	uksCtrls in 's:\v100\source\std\uksCtrls.pas',
	uksPackReg in 's:\v100\source\std\uksPackReg.pas',
	uksUtils in 's:\v100\source\std\uksUtils.pas',

	ukdbClasses in 's:\v100\source\db\ukdbClasses.pas',
	ukdbConsts in 's:\v100\source\db\ukdbConsts.pas',
	ukdbEngines in 's:\v100\source\db\ukdbEngines.pas',
	ukdbfDataSetActions in 's:\v100\source\db\ukdbfDataSetActions.pas' {frmKDataSetActions},
	ukdbfKeyFields in 's:\v100\source\db\ukdbfKeyFields.pas' {frmKeyFields},
	ukdbfLinkedFields in 's:\v100\source\db\ukdbfLinkedFields.pas' {frmLinkedFields},
	ukdbPackReg in 's:\v100\source\db\ukdbPackReg.pas',
	ukdbScript in 's:\v100\source\db\ukdbScript.pas',
	ukdbTables in 's:\v100\source\db\ukdbTables.pas',
	ukdbUtils in 's:\v100\source\db\ukdbUtils.pas',

	ukdcConsts in 's:\v100\source\dbctrls\ukdcConsts.pas',
	ukdcCtrls in 's:\v100\source\dbctrls\ukdcCtrls.pas',
	ukdcPackReg in 's:\v100\source\dbctrls\ukdcPackReg.pas',
	ukdcUtils in 's:\v100\source\dbctrls\ukdcUtils.pas',

	ukeClasses in 's:\v100\source\ext\ukeClasses.pas',
	ukeConsts in 's:\v100\source\ext\ukeConsts.pas',
	ukeEngines in 's:\v100\source\ext\ukeEngines.pas',
	ukefAddMathExpr in 's:\v100\source\ext\ukefAddMathExpr.pas' {frmAddExpr},
	ukePackReg in 's:\v100\source\ext\ukePackReg.pas',
	ukeTypes in 's:\v100\source\ext\ukeTypes.pas',
	ukeUtils in 's:\v100\source\ext\ukeUtils.pas',

	ukwClasses in 's:\v100\source\winAPI\ukwClasses.pas',
	ukwConsts in 's:\v100\source\winAPI\ukwConsts.pas',
	ukwCtrls in 's:\v100\source\winAPI\ukwCtrls.pas',
	ukwPackReg in 's:\v100\source\winAPI\ukwPackReg.pas',
	ukwUtils in 's:\v100\source\winAPI\ukwUtils.pas',

	ukdgClasses in 's:\v100\source\dialogs\ukdgClasses.pas',
	ukdgConsts in 's:\v100\source\dialogs\ukdgConsts.pas',
	ukdgPackReg in 's:\v100\source\dialogs\ukdgPackReg.pas',
	ukdgUtils in 's:\v100\source\dialogs\ukdgUtils.pas',

	ukddClasses in 's:\v100\source\dbdialogs\ukddClasses.pas',
	ukddConsts in 's:\v100\source\dbdialogs\ukddConsts.pas',
	ukddPackReg in 's:\v100\source\dbdialogs\ukddPackReg.pas',
	ukddUtils in 's:\v100\source\dbdialogs\ukddUtils.pas',

	ukfClasses in 's:\v100\source\forms\ukfClasses.pas',
	ukfConsts in 's:\v100\source\forms\ukfConsts.pas',
	ukfCtrls in 's:\v100\source\forms\ukfCtrls.pas',
	ukfUtils in 's:\v100\source\forms\ukfUtils.pas',

	ukcyBlowFish in 's:\v100\source\crypto\ukcyBlowFish.pas',
	ukcyClasses in 's:\v100\source\crypto\ukcyClasses.pas',
	ukcyConsts in 's:\v100\source\crypto\ukcyConsts.pas',
	ukcyIntLib in 's:\v100\source\crypto\ukcyIntLib.pas',
	ukcyMD5 in 's:\v100\source\crypto\ukcyMD5.pas',
	ukcyPackReg in 's:\v100\source\crypto\ukcyPackReg.pas',
	ukcyTypes in 's:\v100\source\crypto\ukcyTypes.pas',
	ukcyUtils in 's:\v100\source\crypto\ukcyUtils.pas',

	ukbcClasses in 's:\v100\source\barcode\ukbcClasses.pas',
	ukbcConsts in 's:\v100\source\barcode\ukbcConsts.pas',
	ukbcCtrls in 's:\v100\source\barcode\ukbcCtrls.pas',
	ukbcDBCtrls in 's:\v100\source\barcode\ukbcDBCtrls.pas',
	ukbcPackReg in 's:\v100\source\barcode\ukbcPackReg.pas',
	ukbcQRCtrls in 's:\v100\source\barcode\ukbcQRCtrls.pas',
	ukbcTypes in 's:\v100\source\barcode\ukbcTypes.pas',
	ukbcUtils in 's:\v100\source\barcode\ukbcUtils.pas',

	ukbcdConsts in 's:\v100\source\design\ukbcdConsts.pas',
	ukbcdQRReg in 's:\v100\source\design\ukbcdQRReg.pas',
	ukbcdReg in 's:\v100\source\design\ukbcdReg.pas',
	ukbcdUtils in 's:\v100\source\design\ukbcdUtils.pas',
	ukcydConsts in 's:\v100\source\design\ukcydConsts.pas',
	ukcydClasses in 's:\v100\source\design\ukcydClasses.pas',
	ukcydMComments in 's:\v100\source\design\ukcydMComments.pas',
	ukcydReg in 's:\v100\source\design\ukcydReg.pas',
	ukcydUtils in 's:\v100\source\design\ukcydUtils.pas',
	ukdbdConsts in 's:\v100\source\design\ukdbdConsts.pas',
	ukdbdClasses in 's:\v100\source\design\ukdbdClasses.pas',
	ukdbdMComments in 's:\v100\source\design\ukdbdMComments.pas',
	ukdbdReg in 's:\v100\source\design\ukdbdReg.pas',
	ukdbdUtils in 's:\v100\source\design\ukdbdUtils.pas',
	ukdcdConsts in 's:\v100\source\design\ukdcdConsts.pas',
	ukdcdMComments in 's:\v100\source\design\ukdcdMComments.pas',
	ukdcdReg in 's:\v100\source\design\ukdcdReg.pas',
	ukdcdUtils in 's:\v100\source\design\ukdcdUtils.pas',
	ukdddConsts in 's:\v100\source\design\ukdddConsts.pas',
	ukdddMComments in 's:\v100\source\design\ukdddMComments.pas',
	ukdddReg in 's:\v100\source\design\ukdddReg.pas',
	ukdddUtils in 's:\v100\source\design\ukdddUtils.pas',
	ukdgdConsts in 's:\v100\source\design\ukdgdConsts.pas',
	ukdgdClasses in 's:\v100\source\design\ukdgdClasses.pas',
	ukdgdMComments in 's:\v100\source\design\ukdgdMComments.pas',
	ukdgdReg in 's:\v100\source\design\ukdgdReg.pas',
	ukdgdUtils in 's:\v100\source\design\ukdgdUtils.pas',
	ukedClasses in 's:\v100\source\design\ukedClasses.pas',
	ukedConsts in 's:\v100\source\design\ukedConsts.pas',
	ukedfDCC32Opt in 's:\v100\source\design\ukedfDCC32Opt.pas' {frmDCC32ItemOpt},
	ukedfMathExpr in 's:\v100\source\design\ukedfMathExpr.pas' {frmPropExprEd},
	ukedfMsgEnums in 's:\v100\source\design\ukedfMsgEnums.pas' {frmMsgEnums},
	ukedMComments in 's:\v100\source\design\ukedMComments.pas',
	ukedReg in 's:\v100\source\design\ukedReg.pas',
	ukedUtils in 's:\v100\source\design\ukedUtils.pas',
	ukexdReg in 's:\v100\source\design\ukexdReg.pas',
	ukfdClasses in 's:\v100\source\design\ukfdClasses.pas',
	ukfdConsts in 's:\v100\source\design\ukfdConsts.pas',
	ukfdReg in 's:\v100\source\design\ukfdReg.pas',
	ukrdClasses in 's:\v100\source\design\ukrdClasses.pas',
	ukrdConsts in 's:\v100\source\design\ukrdConsts.pas',
	ukrdfCharSet in 's:\v100\source\design\ukrdfCharSet.pas' {frmCharSet},
	ukrdfStrArray in 's:\v100\source\design\ukrdfStrArray.pas' {frmStringsArrayEditor},
	ukrdMComments in 's:\v100\source\design\ukrdMComments.pas',
	ukrdReg in 's:\v100\source\design\ukrdReg.pas',
	ukrdUtils in 's:\v100\source\design\ukrdUtils.pas',
	uksdClasses in 's:\v100\source\design\uksdClasses.pas',
	uksdConsts in 's:\v100\source\design\uksdConsts.pas',
	uksdMComments in 's:\v100\source\design\uksdMComments.pas',
	uksdReg in 's:\v100\source\design\uksdReg.pas',
	uksdUtils in 's:\v100\source\design\uksdUtils.pas',
	uksydClasses in 's:\v100\source\design\uksydClasses.pas',
	uksydConsts in 's:\v100\source\design\uksydConsts.pas',
	uksydfStrEdit in 's:\v100\source\design\uksydfStrEdit.pas' {frmStrEditDlg},
	uksydMComments in 's:\v100\source\design\uksydMComments.pas',
	uksydReg in 's:\v100\source\design\uksydReg.pas',
	uksydTypes in 's:\v100\source\design\uksydTypes.pas',
	uksydUtils in 's:\v100\source\design\uksydUtils.pas',
	ukwdClasses in 's:\v100\source\design\ukwdClasses.pas',
	ukwdConsts in 's:\v100\source\design\ukwdConsts.pas',
	ukwdMComments in 's:\v100\source\design\ukwdMComments.pas',
	ukwdReg in 's:\v100\source\design\ukwdReg.pas',
	ukwdUtils in 's:\v100\source\design\ukwdUtils.pas',

	ukexClasses in 's:\v100\source\experts\ukexClasses.pas',
	ukexConsts in 's:\v100\source\experts\ukexConsts.pas',
	ukexPackReg in 's:\v100\source\experts\ukexPackReg.pas',
	ukexUtils in 's:\v100\source\experts\ukexUtils.pas',

	uksyResStr in 's:\v100\source\str\uksyResStr.pas',
	ukrresstr in 's:\v100\source\str\ukrresstr.pas',
	uksResStr in 's:\v100\source\str\uksResStr.pas',
	ukdbResStr in 's:\v100\source\str\ukdbResStr.pas',
	ukeResStr in 's:\v100\source\str\ukeResStr.pas',
	ukwResStr in 's:\v100\source\str\ukwResStr.pas',
	ukdgResStr in 's:\v100\source\str\ukdgResStr.pas',
	ukddResStr in 's:\v100\source\str\ukddResStr.pas',
	ukfResStr in 's:\v100\source\str\ukfResStr.pas',
	ukcyResStr in 's:\v100\source\str\ukcyResStr.pas',
	ukbcResStr in 's:\v100\source\str\ukbcResStr.pas';

begin
end.

//	ukrdInternal in 's:\v100\source\design\ukrdInternal.pas',
//	ukdbdInternal in 's:\v100\source\design\ukdbdInternal.pas',
//	ukdbdInternal in 's:\v100\source\design\ukdbdInternal.pas',
//	ukrdInternal in 's:\v100\source\design\ukrdInternal.pas',
//	uksydInternal in 's:\v100\source\design\uksydInternal.pas',