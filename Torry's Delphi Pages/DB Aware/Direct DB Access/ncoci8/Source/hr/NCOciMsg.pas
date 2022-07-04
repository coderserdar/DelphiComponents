{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.03.04 / 05.03.2000                        }
{Comment:   NC OCI8 VCL: string resources               }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{Croatian translationr: Goran Kliska , gorankli@usa.net }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciMsg;

interface

resourcestring
    // NCOciWrapper
    msgOCINotInstalled = 'NOE1/INIT - OCI nije pravilno instaliran na ovom raèunalu';
    msgOCIBadVersion = 'NOE2/INIT - OCI verzija [%s]. NCOCI8 zahtjeva verziju 8.0.3 ili noviju';
    msgOCINotLoaded = 'NOE3/INIT - Greška kod uèitavanja OCI dll. %s';
    msgOCIOutOfCount = 'NOE4/VAR - Index izvan opsega matrice';
    msgOCIBadValueSize = 'NOE5/VAR - Loša ili nedefinirana velièina vrijednosti varijable';
    msgOCIBadArrayLen = 'NOE6/VAR - Loša ili nedefinirana velièina matrice (variable array length)';
    msgOCIBadValueType = 'NOE7/VAR - Loša ili nedefinirana variabla [%s]  tipa %d';
    msgOCIUnsupValueType = 'NOE8/VAR - Nepodržan tip varijable';
    // NOE9/VAR
    msgOCIDataToLarge = 'NOE10/VAR - Podatak prevelik za varijablu';
    msgOCIVarWithoutStmt = 'NOE11/VAR - Variabla zahtjeva pripremljenu naredbu ';
    msgOCIBadVarType = 'NOE12/VAR - Loš ili nedefiniran tip parametra za varijablu';
    msgOCIUndefPWVar = 'NOE13/STMT - Piece wiese operation for unknown variable';
    msgOCIBadTypePWVar = 'NOE14/STMT - Piece wiese operation for invalid variable';
    msgOCIFuncNotFound = 'NOE15/INIT - Entry point [%s] not found in OCI DLL ';
    msgOCINumInvVarArray = 'NOE16/NUM - Pogrešna VARARRAY velièina. Can not get number from variant';
    msgOCINumInvVar = 'NOE17/NUM - Invalid variant type. Can not get number from variant';
    msgOCITooLongGTRID = 'NOE18/TX - Maximum length (%d) of GTRID exceeded - %d';
    msgOCITooLongBQUAL = 'NOE19/TX - Maximum length (%d) of BQUAL exceeded - %d';
    msgOCITooLongTXName = 'NOE20/TX - Maximum length (%d) of transaction name exceeded - %d';

    // NCOciBuff
    msgOCIBuffMBOpen = 'NOE50/CRS - OCI kursor mora bit otvoren';
    msgOCIBuffMBClose = 'NOE51/CRS - OCI kursor mora bit zatvoren';
    msgOCIBadDT4CU = 'NOE52/CRS - Cannot use cached updates for OCI cursor with BLOB define variables';
    msgOCIInvalidVarIndex = 'NOE53/CRS - Invalid define variable position';
    msgOCIInvalidRNO = 'NOE60/DEADCRS - Neispravan broj zapisa';
    msgOCIModifyNotDelphiBuff = 'NOE61/DEADCRS - TOCIDataSet buffer operation with non [rsDelphiBuff] buffer';
    msgOCICantModifyBuff = 'NOE62/DEADCRS - Can not delete/modify [rsEmpty, rsNew*Applyed, rsDelphiBuff] buffer';
    msgOCIInvalidBmk = 'NOE63/DEADCRS - Invalid bookmark';
    msgOCIApplyFailed = 'NOE64/DEADCRS - Apply of cached updates failed';
    msgOCIRecurseApply = 'NOE65/DEADCRS - Cannot recurse apply cached updates';
    msgOCINoApplyCallback = 'NOE66/DEADCRS - To apply updates need callback';
    msgOCIMustBeBidir = 'NOE67/DEADCRS - OCI cursor must be bidirectional';

    // NCOciParams
    msgOCIBadParamVal = 'NOE80/PAR - Can not assign value to parameter';
    msgOCIComplexType = 'NOE81/PAR - For complex type params use specific methods';
    msgOCIMacroNotFound = 'NOE82/PAR - Macros [%s] not found';
{*}    msgOCICantAssignByRef = 'NOE83/PAR - Can''t assign value to parameter by reference';
{*}    msgOCIParamIsNotHandle = 'NOE84/PAR - Param is not of handle type';

    // NCOciDB
    msgOCIDBmbActive = 'NOE100/DB - Baza podataka mora biti aktivna';
    msgOCIDBmbInactive = 'NOE101/DB - Baza podataka mora biti zatvorena';
    msgOCIDBLoginRetries = 'NOE102/DB -I nakon %d pokušaja nije uspjelo spajanje na ORACLE bazu podataka';
    msgOCIDBUnknown = 'NOE103/DB - Baza podataka [%s] je nepoznata';
    msgOCIStmtCantDescribe = 'NOE104/Q - Can not describe select list for a statement without rowset';
{*} msgOCIDBTNManyClBraces = 'NOE105/DB - Too many close braces in names file after alias [%s]';
    msgInvFetchPar = 'NOE107/FP - Invalid fetch parameter [%s] value';
{*} msgStmtExecFailed = 'NOE108/SDS - Statement execution failed';
    msgDataSetNotEditing = 'NOE110/DS - Tablica nije u modu za ispravke';
    msgOCICachUpdMBAct = 'NOE111/DS - Cached updates must be active';
    msgOCIExpNotFnd = 'NOE112/DS - Found uncompiled expression. Reopen dataset';
    msgOCIRecConstCompFail = 'NOE113/DS - Compilation of record constraint failed. %s';
    msgOCIFieldConstCompFail = 'NOE114/DS - Compilation of field constraint failed. %s';
    msgOCIFieldDefCompFail = 'NOE115/DS - Compilation of field default value failed. %s';
    msgOCIRecConstFail = 'NOE116/DS - Slog ne udovoljava postavljenom ogranièenju/pravilu. %s';
    msgOCIFieldConstFail = 'NOE117/DS - Polje ne udovoljava postavljenom ogranièenju/pravilu. %s';
    msgOCIFieldDefFail = 'NOE118/DS - Neispravna predodreðena (ponuðena) vrijednost za polje . %s';
{*} msgOCICantExecOper = 'NOE119/DS - Can not %s. It is disabled';
    msgOCIBlobReadOnly = 'NOE120/BLOB - BLOB field [%s] readonly';
    msgOCILongStreamInvalid = 'NOE121/BLOB - TOCILongStream can''t be used for field [%s]';
    msgOCILOBStreamInvalid = 'NOE122/BLOB - TOCILobStream can''t be used for field [%s]';
    msgOCIFieldIsNotBLOB = 'NOE123/BLOB - Field is not a BLOB field';
    msgOCIStmtCantOpen = 'NOE127/Q - Can not call Open for a statement without rowset';
    msgOCIStmtCantExec = 'NOE128/Q - Can not call ExecSQL for statement with rowset';
    msgOCIKeyFieldsEmpty = 'NOE129/Q - Osvježavanje podataka nije moguæe. Nisu definirana indeksna polja';
    msgOCINotPLSQLObj = 'NOE130/SP - [%s] is not a callable PL/SQL object';
    msgOCIPLSQLObjNameEmpty = 'NOE131/SP - PL/SQL object name is empty';
    msgOCIPLSQLNotRemote = 'NOE132/SP - Remote PL/SQL objects not supported (use TOCIQuery)';
    msgOCIPLSQLNameError = 'NOE133/SP - Cannot parse PL/SQL objects name';
    msgOCINotPackageProc = 'NOE134/SP - [%s] is not a proc of package [%s]';
    msgOCIBadTableType = 'NOE135/SP - Parameter with type TABLE OF BOOLEAN/RECORD not supported (use TOCIQuery)';
    msgOCITransMBInAct = 'NOE136/TX - Transakcija mora bit zatvorena (neaktivna)';
    msgOCIDBNameMismatch = 'NOE137/TX - DatabaseName mismatch - [%s] and [%s]';
    msgOCITransMBStarted = 'NOE138/TX - For Suspend transaction must be started explicitly';
    msgOCICannotChangeTM = 'NOE139/TX - Can''t connect other TM because current have active transaction';
    msgOCICantChProp = 'NOE140/UPD - Can not change property, when updates pending';
{*}    msgOCIParentDSRequired = 'NOE141/NDS - Parent data set valid info required';
{*} msgOCIUnNamedRecParam = 'NOE142/SP - Parameter with type RECORD must be of named type (use TOCIQuery)';
{*} SOCIOperations = 'Lock;Unlock;Update;Insert;Delete;Refresh';

    // NCOciFilter
    msgOCIFilterNotFound = 'NOE150/FLT - Filter [%s] nije pronaðen';
{$IFNDEF OCI_D4}
    SExprNoLParen = '''('' expected but %s found';
    SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
    SExprTypeMis = 'Type mismatch in expression';
    SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
    SExprNoArith = 'Arithmetic in filter expressions not supported';
    SExprNotAgg = 'Expression is not an aggregate expression';
    SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
    SExprEmptyInList = 'IN predicate list may not be empty';
    SInvalidKeywordUse = 'Invalid use of keyword';
{$ENDIF}    

    // NCOciReg
    SOCILoginCategoryName = 'OCI Prijava';
    SOCILoginCategoryDesc = 'OCI Postavke i/ili dogaðaji prijave ';
    SOCIResourceCategoryName = 'OCI Resource';
    SOCIResourceCategoryDesc = 'OCI Resource properties and/or events';
    SOCITransactCategoryName = 'OCI Transakcija';
    SOCITransactCategoryDesc = 'OCI Transakcijske postavke i/ili dogaðaji';

    // NCOciUpdateSQL
    msgOCICantGenQuery = 'NOE170/UPS - Can not generate %s command for data set';
    msgOCIRecordDeleted = 'NOE171/UPS - Slog je u meðuvremenu obrisao drugi korisnik';
    msgOCIRecordChanged = 'NOE172/UPS - Slog je u meðuvremenu promjenio drugi korisnik';
    msgOCIRecordLocked = 'NOE173/UPS - Zapis je zakljuèao drugi korisnik';
    msgOCICantUseSQL4Refresh = 'NOE174/UPS - SQL naredba (upit) for refresh command';

    // NCOciMove
    msgOCIDPColumnAccessError = '';
    msgOCIDPColumnNotFound = '';
    msgOCIDPTabNameError = '';
    msgOCIDPNotRemote = '';
    msgOCIDPMustBeUnprepared = '';

    // NCOciBDE
{*} msgOCIBDEMBInActive = 'NOE180/BDE BDE connection must be inactive';

    // NCOciCompNamer
{*} SCNBadPropName = 'Invalid property name [%s]';
{*} SCNBadStripPref = 'Error in StripPrefixs. Missing [}]';
{*} SCNEnterText = 'Upiši tekst:';
{*} SCNBadVFormat = 'Missing argument for [V] format in [%s]';
{*} SCNUnknownCommand = 'Encountered unknown format item in [%s]';
{*} SCNNilComp = 'Undefined component';
{*} SCNUnknownFormat = 'For class [%s] format is missed';
{*} SCNCallExpertNow = '<Call expert now ...>';
{*} SCNUseExpertForRename = 'Use expert to rename multiple components';

implementation

end.

