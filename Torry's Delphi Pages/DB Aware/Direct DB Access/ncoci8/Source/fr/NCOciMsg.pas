{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.03.05 / 17.08.2000                        }
{Comment:   NC OCI8 VCL: string resources               }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{Translation: Philippe Bonnefoy Cudraz                  }
{             pbonnefoy@caramail.com                    }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciMsg;

interface

resourcestring
    // NCOciWrapper
    msgOCINotInstalled = 'NOE1/INIT - OCI n''est pas installé correctement.';
    msgOCIBadVersion = 'NOE2/INIT - Mauvaise version OCI [%s]. Version minimale 8.0.3';
    msgOCINotLoaded = 'NOE3/INIT - Erreur au chargement de la dll OCI. %s';
    msgOCIOutOfCount = 'NOE4/VAR - Débordement d''index';
    msgOCIBadValueSize = 'NOE5/VAR - Mauvaise taille';
    msgOCIBadArrayLen = 'NOE6/VAR - Mauvaise longueur';
    msgOCIBadValueType = 'NOE7/VAR - Mauvaise variable [%s] valeur de type %d';
    msgOCIUnsupValueType = 'NOE8/VAR - Type de variable non supporté';
    // NOE9/VAR
    msgOCIDataToLarge = 'NOE10/VAR - Données trop grandes pour la variable';
    msgOCIVarWithoutStmt = 'NOE11/VAR - La variable nécessite un ordre compilé';
    msgOCIBadVarType = 'NOE12/VAR - Anomalie sur le type du paramètre';
    msgOCIUndefPWVar = 'NOE13/STMT - Piece wiese operation for unknown variable';
    msgOCIBadTypePWVar = 'NOE14/STMT - Piece wiese operation for invalid variable';
    msgOCIFuncNotFound = 'NOE15/INIT - Point d''entrée [%s] inconnu dans la dll OCI';
    msgOCINumInvVarArray = 'NOE16/NUM - Taille VARARRAY incorrecte. Valeur inaccessible depuis un variant';
    msgOCINumInvVar = 'NOE17/NUM - Type de variant incorrect. Valeur inaccessible depuis un variant';
    msgOCITooLongGTRID = 'NOE18/TX - Débordement de longueur (%d) pour GTRID - %d';
    msgOCITooLongBQUAL = 'NOE19/TX - Débordement de longueur (%d) pour BQUAL - %d';
    msgOCITooLongTXName = 'NOE20/TX - Dépassement de la longueur (%d) du nom de transaction - %d';

    // NCOciBuff
    msgOCIBuffMBOpen = 'NOE50/CRS - Le curseur OCI doit être ouvert';
    msgOCIBuffMBClose = 'NOE51/CRS - Le curseur OCI doit être fermé';
    msgOCIBadDT4CU = 'NOE52/CRS - Mise à jour bufferisée impossible d''un curseur OCI contenant des BLOB';
    msgOCIInvalidVarIndex = 'NOE53/CRS - Position incorrecte de la variable';
    msgOCIInvalidRNO = 'NOE60/DEADCRS - Numéro d''enregistrement invalide';
    msgOCIModifyNotDelphiBuff = 'NOE61/DEADCRS - Action sur un buffer TOCIDataSet avec un buffer non [rsDelphiBuff]';
    msgOCICantModifyBuff = 'NOE62/DEADCRS - Suppression/Modification impossible d''un buffer [rsEmpty, rsNew*Applyed, rsDelphiBuff]';
    msgOCIInvalidBmk = 'NOE63/DEADCRS - Bookmark invalide';
    msgOCIApplyFailed = 'NOE64/DEADCRS - Anomalie lors de la validation des données bufferisées';
    msgOCIRecurseApply = 'NOE65/DEADCRS - Validation récursive de données bufferisées interdite';
    msgOCINoApplyCallback = 'NOE66/DEADCRS - Callback indispensable pour la mise à jour';
    msgOCIMustBeBidir = 'NOE67/DEADCRS - Le curseur OCI doit être bidirectionel';

    // NCOciParams
    msgOCIBadParamVal = 'NOE80/PAR - Valeur inapplicable au paramètre';
    msgOCIComplexType = 'NOE81/PAR - Pour des types complexes de paramètres, utilisez des méthodes spécifiques';
    msgOCIMacroNotFound = 'NOE82/PAR - Macro [%s] inconnue';
    msgOCICantAssignByRef = 'NOE83/PAR - Paramètres par référence invariables';
    msgOCIParamIsNotHandle = 'NOE84/PAR - Le paramètre n''est pas un handle';

    // NCOciDB
    msgOCIDBmbActive = 'NOE100/DB - La base doit être active';
    msgOCIDBmbInactive = 'NOE101/DB - La base doit être inactive';
    msgOCIDBLoginRetries = 'NOE102/DB - Connexion à ORACLE impossible après %d tentatives';
    msgOCIDBUnknown = 'NOE103/DB - Base de données [%s] inconnue';
    msgOCIStmtCantDescribe = 'NOE104/Q - Pas de select list pour un ordre ne retournant pas des données';
{*} msgOCIDBTNManyClBraces = 'NOE105/DB - Too many close braces in names file after alias [%s]';
    msgInvFetchPar = 'NOE107/FP - Paramètre pour lecture [%s] invalide';
{*} msgStmtExecFailed = 'NOE108/SDS - Statement execution failed';
    msgDataSetNotEditing = 'NOE110/DS - Le dataset n''est pas en mode Edition';
    msgOCICachUpdMBAct = 'NOE111/DS - les mises à jour bufferisées doivent être actives';
    msgOCIExpNotFnd = 'NOE112/DS - Ordre non compilé. Ouvrez à nouveau le dataset';
    msgOCIRecConstCompFail = 'NOE113/DS - Rejet lors de la compilation de la contrainte "enregistrement". %s';
    msgOCIFieldConstCompFail = 'NOE114/DS - Rejet lors de la compilation de la contrainte "champs". %s';
    msgOCIFieldDefCompFail = 'NOE115/DS - Rejet lors de la compilation de la valeur par défaut. %s';
    msgOCIRecConstFail = 'NOE116/DS - Rejet sur une contrainte au niveau Enregistrement. %s';
    msgOCIFieldConstFail = 'NOE117/DS - Rejet sur une contrainte au niveau champs. %s';
    msgOCIFieldDefFail = 'NOE118/DS - Rejet sur la valeur par défaut du champs. %s';
    msgOCICantExecOper = 'NOE119/DS - Opération %s désactivée.';
    msgOCIBlobReadOnly = 'NOE120/BLOB - Le champs BLOB [%s] est en lecture seule';
    msgOCILongStreamInvalid = 'NOE121/BLOB - TOCILongStream ne peut s''utiliser avec le champs [%s]';
    msgOCILOBStreamInvalid = 'NOE122/BLOB - TOCILobStream ne peut s''utiliser avec le champs [%s]';
    msgOCIFieldIsNotBLOB = 'NOE123/BLOB - Ce champs n''est pas de type BLOB';
    msgOCIStmtCantOpen = 'NOE127/Q - OPEN invalide en absence d''ensemble de données';
    msgOCIStmtCantExec = 'NOE128/Q - ExecSQL invalide en présence d''un ensemble de données';
    msgOCIKeyFieldsEmpty = 'NOE129/Q - impossible d''exécuter l''ordre SQL, clé non définie';
    msgOCINotPLSQLObj = 'NOE130/SP - [%s] n''est pas un objet PL/SQL utilisable';
    msgOCIPLSQLObjNameEmpty = 'NOE131/SP - Objet PL/SQL sans nom';
    msgOCIPLSQLNotRemote = 'NOE132/SP - Objet PL/SQL déporté non gérés (utilisez TOCIQuery)';
    msgOCIPLSQLNameError = 'NOE133/SP - Analyse du nom de l''objet PL/SQL impossible';
    msgOCINotPackageProc = 'NOE134/SP - [%s] n''est pas un module du paquet [%s]';
    msgOCIBadTableType = 'NOE135/SP - Paramètre de type TABLE OF BOOLEAN/RECORD non géré (utilisez TOCIQuery)';
    msgOCITransMBInAct = 'NOE136/TX - La transaction doit être inactive. ';
    msgOCIDBNameMismatch = 'NOE137/TX - Erreur sur le nom de la base de données - [%s] et [%s]';
    msgOCITransMBStarted = 'NOE138/TX - Les transactions suspendues doit être démarrées explicitement';
    msgOCICannotChangeTM = 'NOE139/TX - Connexion d''une autre TM interdite car celle en cours possède une transaction active';
    msgOCICantChProp = 'NOE140/UPD - Propriété inaltérable en cours de modification';
    msgOCIParentDSRequired = 'NOE141/NDS - Fournir une info valide sur le dataset parent. ';
{*} msgOCIUnNamedRecParam = 'NOE142/SP - Parameter with type RECORD must be of named type (use TOCIQuery)';
{*} SOCIOperations = 'Lock;Unlock;Update;Insert;Delete;Refresh';

    // NCOciFilter
    msgOCIFilterNotFound = 'NOE150/FLT - Filtre [%s] inconnu';
{$IFNDEF OCI_D4}
    SExprNoLParen = '''('' attendue mais %s trouvé';
    SExprNoRParenOrComma = ''')'' ou '','' attendues mais %s trouvé';
    SExprTypeMis = 'Type invalide dans l''expression';
    SExprBadScope = 'Mélange d''agrégat et de détail interdit';
    SExprNoArith = 'Filtres utilisant des expressions arithmétiques non supportés';
    SExprNotAgg = 'L''expression n''est pas de type Agrégat';
    SExprNoAggFilter = 'Les agrégats sont indisponibles dans les filtres';
    SExprEmptyInList = 'IN fourni sans valeur';
    SInvalidKeywordUse = 'Mot clé invalide';
{$ENDIF}

    // NCOciReg
    SOCILoginCategoryName = 'OCI Login';
    SOCILoginCategoryDesc = 'OCI Login propriété et/ou évênement';
    SOCIResourceCategoryName = 'OCI Ressource';
    SOCIResourceCategoryDesc = 'OCI Ressource propriété et/ou évênement';
    SOCITransactCategoryName = 'OCI Transaction';
    SOCITransactCategoryDesc = 'OCI Transaction propriété et/ou évênement';

    // NCOciUpdateSQL
    msgOCICantGenQuery = 'NOE170/UPS - Commande %s interdite sur le data set';
    msgOCIRecordDeleted = 'NOE171/UPS - L''enregistrement a été supprimé par un autre utilisateur';
    msgOCIRecordChanged = 'NOE172/UPS - L''enregistrement a été modifié par un autre utilisateur';
    msgOCIRecordLocked = 'NOE173/UPS - Enregistrement bloqué par un autre utilisateur';
    msgOCICantUseSQL4Refresh = 'NOE174/UPS - Ordre SQL inadéquate pour un raffraîchissement';

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
{*} SCNEnterText = 'Enter text:';
{*} SCNBadVFormat = 'Missing argument for [V] format in [%s]';
{*} SCNUnknownCommand = 'Encountered unknown format item in [%s]';
{*} SCNNilComp = 'Undefined component';
{*} SCNUnknownFormat = 'For class [%s] format is missed';
{*} SCNCallExpertNow = '<Call expert now ...>';
{*} SCNUseExpertForRename = 'Use expert to rename multiple components';

implementation

end.
