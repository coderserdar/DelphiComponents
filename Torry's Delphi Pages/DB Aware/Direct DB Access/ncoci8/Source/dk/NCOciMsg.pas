{********************************************************}
{File:               NCOciBuff.PAS                       }
{Revision:           0.03.05 / 17.08.2000                }
{Comment:            NC OCI8 VCL: string resources       }
{Copyright:          (c) 1999-2000, Dmitry Arefiev       }
{Author:             Dmitry Arefiev, darefiev@da-soft.com}
{Danish translation: Kurt Bilde, kub@sam.sdu.dk          }
{********************************************************}
{$I NCOciDef.inc}

unit NCOciMsg;

interface

resourcestring
    // NCOciWrapper
    msgOCINotInstalled = 'NOE1/INIT - OCI er ikke korrekt installeret på denne maskine';
    msgOCIBadVersion = 'NOE2/INIT - Forkert OCI version [%s]. NCOCI8 anvender mindst ver. 8.0.3';
    msgOCINotLoaded = 'NOE3/INIT - Fejl ved indlæsning af OCI dll. %s';
    msgOCIOutOfCount = 'NOE4/VAR - Indeks er overløbet array størrelsen';
    msgOCIBadValueSize = 'NOE5/VAR - Forkert eller udefinerede variabel værdistørrelse';
    msgOCIBadArrayLen = 'NOE6/VAR - Forkert eller udefinerede variabel array længde';
    msgOCIBadValueType = 'NOE7/VAR - Forkert eller udefinerede variabel [%s] værditype %d';
    msgOCIUnsupValueType = 'NOE8/VAR - Ikke supporterede variabel datatype';
    // NOE9/VAR
    msgOCIDataToLarge = 'NOE10/VAR - Data er for stor til variabel';
    msgOCIVarWithoutStmt = 'NOE11/VAR - Variabel kræver forberedt beskrivelse for operation';
    msgOCIBadVarType = 'NOE12/VAR - Forkert eller udefinerede variabel param type';
    msgOCIUndefPWVar = 'NOE13/STMT - Stykvis operation for ukendt variabel';
    msgOCIBadTypePWVar = 'NOE14/STMT - Stykvis operation for ugyldig variabel';
    msgOCIFuncNotFound = 'NOE15/INIT - Indgangspunkt [%s] ikke fundet i OCI DLL';
    msgOCINumInvVarArray = 'NOE16/NUM - Ugyldig VARARRAY størrelse. Kan ikke udlæse nummer fra variant';
    msgOCINumInvVar = 'NOE17/NUM - Ugyldig variant type. Kan ikke udlæse nummer fra variant';
    msgOCITooLongGTRID = 'NOE18/TX - Maksimum længde (%d) af GTRID overstiger - %d';
    msgOCITooLongBQUAL = 'NOE19/TX - Maksimum længde (%d) af BQUAL overstiger - %d';
    msgOCITooLongTXName = 'NOE20/TX - Maksimum længde (%d) af transaktionsnavn overstiger - %d';

    // NCOciBuff
    msgOCIBuffMBOpen = 'NOE50/CRS - OCI cursor skal være åbnet';
    msgOCIBuffMBClose = 'NOE51/CRS - OCI cursor skal være lukket';
    msgOCIBadDT4CU = 'NOE52/CRS - Kan ikke anvende cached opdateringer for OCI cursor med BLOB definerede variable';
    msgOCIInvalidVarIndex = 'NOE53/CRS - Ugyldig define variabel position';
    msgOCIInvalidRNO = 'NOE60/DEADCRS - Ugyldigt rekord-nummer';
    msgOCIModifyNotDelphiBuff = 'NOE61/DEADCRS - TOCIDataSet buffer-operation med ikke [rsDelphiBuff] buffer';
    msgOCICantModifyBuff = 'NOE62/DEADCRS - Kan ikke slette/modificere [rsEmpty, rsNew*Applyed, rsDelphiBuff] buffer';
    msgOCIInvalidBmk = 'NOE63/DEADCRS - Ugyldigt bogmærke';
    msgOCIApplyFailed = 'NOE64/DEADCRS - Tilføjelse af cached opdateringer mislykkes';
    msgOCIRecurseApply = 'NOE65/DEADCRS - Kan ikke genskabe tilføjede cached opdateringer';
    msgOCINoApplyCallback = 'NOE66/DEADCRS - For at tilføje opdateringer behøves callback';
    msgOCIMustBeBidir = 'NOE67/DEADCRS - OCI cursor skal være bidirektional';

    // NCOciParams
    msgOCIBadParamVal = 'NOE80/PAR - Kan ikke knytte værdi til parameter';
    msgOCIComplexType = 'NOE81/PAR - For kompleks type params brug specifik metode';
    msgOCIMacroNotFound = 'NOE82/PAR - Makroer [%s] er ikke fundet';
    msgOCICantAssignByRef = 'NOE83/PAR - Kan ikke tilknytte værdi til parameter med reference';
    msgOCIParamIsNotHandle = 'NOE84/PAR - Parameter er ikke af typen handle';

    // NCOciDB
    msgOCIDBmbActive = 'NOE100/DB - DataBase skal være aktiv';
    msgOCIDBmbInactive = 'NOE101/DB - DataBase skal være inaktiv';
    msgOCIDBLoginRetries = 'NOE102/DB - Kunne ikke skabe forbindelse til ORACLE efter %d forsøg';
    msgOCIDBUnknown = 'NOE103/DB - DataBase [%s] er ukendt';
    msgOCIStmtCantDescribe = 'NOE104/Q - Kan ikke beskrive selectlisten for en beskrivelse uden rækkesæt';
{*} msgOCIDBTNManyClBraces = 'NOE105/DB - Too many close braces in names file after alias [%s]';
    msgInvFetchPar = 'NOE107/FP - Ugyldig fetch parameter [%s] værdi';
{*} msgStmtExecFailed = 'NOE108/SDS - Statement execution failed';
    msgDataSetNotEditing = 'NOE110/DS - DataSet er ikke i editeringsmodus';
    msgOCICachUpdMBAct = 'NOE111/DS - Chached opdateringer skal være aktiv';
    msgOCIExpNotFnd = 'NOE112/DS - Der blev fundet en ukompileret betingelse. Genåbn datasæt';
    msgOCIRecConstCompFail = 'NOE113/DS - Kompilering af recordens konstantværdi er mislykkedes. %s';
    msgOCIFieldConstCompFail = 'NOE114/DS - Kompilering af feltes konstantværdi er mislykkedes. %s';
    msgOCIFieldDefCompFail = 'NOE115/DS - Kompilering af field default værdi er mislykkedes. %s';
    msgOCIRecConstFail = 'NOE116/DS - Recordens konstantværdi fejlbehæftet. %s';
    msgOCIFieldConstFail = 'NOE117/DS - Feltes konstantværdi fejlbehæftet. %s';
    msgOCIFieldDefFail = 'NOE118/DS - Feltes default værdi fejlbehæftet. %s';
    msgOCICantExecOper = 'NOE119/DS - Kan ikke %s. Det er disabled';
    msgOCIBlobReadOnly = 'NOE120/BLOB - BLOB felt [%s] er skrivebeskyttet';
    msgOCILongStreamInvalid = 'NOE121/BLOB - TOCILongStream kan ikke anvendes på felt [%s]';
    msgOCILOBStreamInvalid = 'NOE122/BLOB - TOCILobStream kan ikke anvedes på felt [%s]';
    msgOCIFieldIsNotBLOB = 'NOE123/BLOB - Feltet er ikke et BLOB felt';
    msgOCIStmtCantOpen = 'NOE127/Q - Kan ikke kalde Open for en beskrivelse uden rækkesæt';
    msgOCIStmtCantExec = 'NOE128/Q - Kan ikke kalde ExecSQL for en beskrivelse uden rækkesæt';
    msgOCIKeyFieldsEmpty = 'NOE129/Q - Kan ikke genopfriske rækkesæt, udefinerede nøglefelt';
    msgOCINotPLSQLObj = 'NOE130/SP - [%s] er ikke et kaldbart PL/SQL objekt';
    msgOCIPLSQLObjNameEmpty = 'NOE131/SP - PL/SQL objekt navn er tomt';
    msgOCIPLSQLNotRemote = 'NOE132/SP - Remote PL/SQL objekts er ikke understøttet (brug TOCIQuery)';
    msgOCIPLSQLNameError = 'NOE133/SP - Kan ikke fortolke PL/SQL objekt navn';
    msgOCINotPackageProc = 'NOE134/SP - [%s] er ikke en proc fra pakken [%s]';
    msgOCIBadTableType = 'NOE135/SP - Parameter med typen TABLE OF BOOLEAN/RECORD er ikke understøttet (brug TOCIQuery)';
    msgOCITransMBInAct = 'NOE136/TX - Transaktion skal være inaktive';
    msgOCIDBNameMismatch = 'NOE137/TX - DatabaseNavn uoverensstemmelse - [%s] og [%s]';
    msgOCITransMBStarted = 'NOE138/TX - For Suspend transaktion skal være startede eksplicit';
    msgOCICannotChangeTM = 'NOE139/TX - Kan ikke forbinde en anden TM, fordi der p.t. er en aktive transaktion';
    msgOCICantChProp = 'NOE140/UPD - Kan ikke ændre element, når opdateringer venter';
    msgOCIParentDSRequired = 'NOE141/NDS - Gyldigt Parent data sæt info påkrævet';
{*} msgOCIUnNamedRecParam = 'NOE142/SP - Parameter with type RECORD must be of named type (use TOCIQuery)';
{*} SOCIOperations = 'Lock;Unlock;Update;Insert;Delete;Refresh';

    // NCOciFilter
    msgOCIFilterNotFound = 'NOE150/FLT - Filter [%s] er ikke fundet';
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
    SOCILoginCategoryName = 'OCI Login';
    SOCILoginCategoryDesc = 'OCI Login elementer og/eller handlinger';
    SOCIResourceCategoryName = 'OCI Resource';
    SOCIResourceCategoryDesc = 'OCI Resource elementer og/eller handlinger';
    SOCITransactCategoryName = 'OCI Transaktion';
    SOCITransactCategoryDesc = 'OCI Transaktion elementer og/eller handlinger';

    // NCOciUpdateSQL
    msgOCICantGenQuery = 'NOE170/UPS - Kan ikke skabe %s kommando for datasæt';
    msgOCIRecordDeleted = 'NOE171/UPS - Rekord er blevet slettet af en anden bruger';
    msgOCIRecordChanged = 'NOE172/UPS - Rekord er blevet ændret af en anden bruger';
    msgOCIRecordLocked = 'NOE173/UPS - Rekord er låst af en anden bruger';
    msgOCICantUseSQL4Refresh = 'NOE174/UPS - Kan ikke anvende den specificerede SQL til genopfrisk kommando';

    // NCOciMove
    msgOCIDPColumnAccessError = '';
    msgOCIDPColumnNotFound = '';
    msgOCIDPTabNameError = '';
    msgOCIDPNotRemote = '';
    msgOCIDPMustBeUnprepared = '';

    // NCOciBDE
{*} msgOCIBDEMBInActive = 'NOE180/BDE BDE connection must be inactive';

    // NCOciCompNamer
    SCNBadPropName = 'Ugyldig enhedsnavn [%s]';
    SCNBadStripPref = 'Fejl i StripPrefixs. Mangler [}]';
    SCNEnterText = 'Indskriv tekst:';
    SCNBadVFormat = 'Mangler argument for [V] format i [%s]';
    SCNUnknownCommand = 'Der blev fundet ukendt formatenhed  i [%s]';
    SCNNilComp = 'Udefinerede komponent';
    SCNUnknownFormat = 'For klassen [%s] mangler formatet';
    SCNCallExpertNow = '<Kald ekspert nu ...>';
    SCNUseExpertForRename = 'Brug ekspert for at omdøbe multiple komponenter';

implementation

end.

