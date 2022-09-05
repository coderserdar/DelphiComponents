{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Constant messages used by Zeos              }
{                                                         }
{ This unit contains all the messages that are output by  }
{ ZEOS methods. One of the given language can be activated}
{ by setting the language in ->                           }
{ ZEOS.inc (e.g.: $DEFINE GERMAN).                        }
{ If no language is defined english will be used.         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZMessages;

interface

{$I ZCore.inc}

uses ZCompatibility;

procedure loadmessages();

const
{$IFDEF FRENCH}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !FRENCH}
{$IFDEF PORTUGUESE}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !PORTUGUESE}
{$IFDEF DUTCH}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !DUTCH}
{$IFDEF GERMAN}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !GERMAN}
{$IFDEF SPANISH}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !SPANISH}
{$IFDEF ROMANA}
  cCodePage = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
{$ELSE !ROMANA}
{$IFDEF INDONESIAN}
  cCodePage = 20127; {US-ASCII (7-bit)}
{$ELSE !INDONESIAN}
{$IFDEF RUSSIAN}
  cCodePage = 1251; {Microsoft Windows Codepage 1251 (Cyrl)}
{$ELSE !RUSSIAN}
{$IFDEF CZECH}
  cCodePage = 1250; {Microsoft Windows Codepage 1250 (East European)}
{$ELSE !CZECH}
{$IFDEF POLISH}
  cCodePage = 1250; {Microsoft Windows Codepage 1250 (East European)}
{$ELSE !POLISH}
cCodePage = 20127; {US-ASCII (7-bit)}
{$ENDIF POLISH} // POLISH
{$ENDIF CZECH} // CZECH
{$ENDIF RUSSIAN}
{$ENDIF INDONESIAN}
{$ENDIF ROMANA}
{$ENDIF SPANISH}
{$ENDIF GERMAN}
{$ENDIF DUTCH}
{$ENDIF PORTUGUESE}
{$ENDIF FRENCH}

resourcestring

  {$IFNDEF WITH_RTLCONSTS_SInvalidGuidArray}
    SInvalidGuidArray = 'Byte-Array or Buffer for GUID must have exact %s Bytes';
  {$ENDIF}
  cSLibraryNotCompatible = 'Client-Library %s found but could not be loaded. Check compile-target and library compatibility!';
//--- added by Serge Girard --------------------------------------------------------
{$IFDEF FRENCH}
  cSSQLError1 = 'Erreur SQL: %s';
  cSSQLError2 = 'Erreur SQL: %s Code: %d';
  cSSQLError3 = 'Erreur SQL: %s Code: %d SQL: %s';
  cSSQLError4 = 'Erreur SQL: %s Code: %d Message: %s';

  cSListCapacityError = 'Capacit� de liste hors limite (%d)';
  cSListCountError = 'Compteur de liste (count) hors limite (%d)';
  cSListIndexError = 'Index de liste hors limite (%d)';

  cSClonningIsNotSupported = 'Le clonage n''est pas support� pour cette classe';
  cSImmutableOpIsNotAllowed = 'L''op�ration n''est pas permise sur des collections non modifiables';
  cSStackIsEmpty = 'La pile est vide';
  cSVariableWasNotFound = 'Variable "%s" non trouv�e';
  cSFunctionWasNotFound = 'Fonction "%s" non trouv�e';
  cSInternalError = 'Erreur interne';
  cSSyntaxErrorNear = 'Erreur de syntaxe proche de "%s"';
  cSSyntaxError = 'Erreur de syntaxe';
  cSUnknownSymbol = 'Symbole inconnu "%s"';
  cSUnexpectedExprEnd = 'Fin d''expression impr�vue';
  cSRightBraceExpected = ') attendue';
  cSParametersError = '%d param�tres attendus mais %d ont �t� trouv�s';
  cSExpectedMoreParams = 'Plus de deux param�tres sont attendus';
  cSInvalidVarByteArray = 'Tableau de VarByte non valide';
  cSVariableAlreadyExists = 'La variable "%s" existe d�j�';
  cSTypesMismatch = 'Types non concordants';
  cSUnsupportedVariantType = 'Type variant non support�';
  cSUnsupportedOperation = 'Op�ration non support�e';

  cSTokenizerIsNotDefined = 'l''objet Tokenizer n''est pas d�fini';
  cSLibraryNotFound = 'Acune des biblioth�ques dynamiques ne peut �tre trouv�e ou charg�e: %s !'#10#13'Utilisez TZConnection.LibraryLocation si l''emplacement est incorrect.';
  cSEncodeDateIsNotSupported = 'Cette version ne supporte pas isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Cette version ne supporte pas isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Cette version ne supporte pas isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Cette version ne supporte pas isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Cette version ne supporte pas isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Cette version ne supporte pas isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Ne peut r�cup�rer l''ensemble de donn�es r�sultant';
  cSRowBufferIsNotAssigned = 'Le buffer de ligne n''est pas assign�';
  cSColumnIsNotAccessable = 'Colonne d''index %d inaccessible';
  cSConvertionIsNotPossible = 'Conversion impossible de la colonne %d de %s vers %s';
  cSCanNotAccessBlobRecord = 'Ne peut acc�der au blob de la colonne %d avec le type %s';
  cSRowDataIsNotAvailable = 'Ligne de donn�es non disponible';
  cSResolverIsNotSpecified = 'L''objet Resolver n''est pas indiqu�';
  cSResultsetIsAlreadyOpened = 'L''ensemble r�sultat est d�j� ouvert';
  cSCanNotUpdateEmptyRow = 'Ne peut mettre � jour une ligne vide';
  cSCanNotUpdateDeletedRow = 'Ne peut mettre � jour une ligne supprim�e';
  cSCanNotDeleteEmptyRow = 'Ne peut supprimer un ligne vide';
  cSCannotUseCommit = 'Vous ne pouvez pas utiliser COMMIT en mode AUTOCOMMIT';
  cSCannotUseRollBack = 'Vous ne pouvez pas utiliser ROLLBACK en mode AUTOCOMMIT';
  cSCanNotUpdateComplexQuery = 'Ne peut mettre � jour une requ�te complexe impliquant plus d''une table';
  cSCanNotUpdateThisQueryType = 'Ne peut mettre � jour ce type de requ�te';
  cSDriverWasNotFound = 'Le driver de base de donn�es demand� n''a pas �t� trouv�';
  cSCanNotConnectToServer = 'Ne peut se connecter au serveur SQL';
  cSTableIsNotSpecified = 'La table n''est pas sp�cifi�e';
  cSLiveResultSetsAreNotSupported = 'Une requ�te actualisable n''est pas support�e par cette classe';
  cSInvalidInputParameterCount = 'Le nombre de param�tres attendu est inf�rieur au pr�vu';
  cSIsolationIsNotSupported = 'Niveau d''isolation de transaction non support�';
  cSColumnWasNotFound = 'Colonne de nom "%s" non trouv�e';
  cSWrongTypeForBlobParameter = 'Type incorrect pour le param�tre Blob';
  cSIncorrectConnectionURL = 'Connexion URL: %s incorrect';
  cSUnsupportedProtocol = 'Protocole: %s non support�';
  cSUnsupportedByDriver    = 'Le driver d''origine ne supporte pas cette fonctionnalit�: [%s]';

  cSConnectionIsNotOpened = 'Connexion non encore ouverte';
  cSInvalidOpInAutoCommit = 'Op�ration non valide en mode AutoCommit';
  cSInvalidOpInNonAutoCommit = 'Op�ration non valide si le mode n''est pas AutoCommit';
  cSInvalidOpPrepare = 'Pr�parer une transaction n''est possible qu''en en d�marrant une (Starttransaction) d''abord (!)';

  cSConnectionIsNotAssigned = 'La connexion � la base donn�es n''est pas indiqu�';
  cSQueryIsEmpty = 'La requ�te SQL est vide';
  cSCanNotExecuteMoreQueries = 'Ne peut ex�cuter plus d''une requ�te';
  cSOperationIsNotAllowed1 = 'Cette op�ration n''est pas permise en mode FORWARD ONLY';
  cSOperationIsNotAllowed2 = 'Cette op�ration n''est pas permise en mode READ ONLY';
  cSOperationIsNotAllowed3 = 'Cette op�ration n''est pas permise en mode %s';
  cSOperationIsNotAllowed4 = 'Cette op�ration n''est pas permise en mode sur un ensemble de donn�es ferm�';
  cSNoMoreRecords = 'Plus d''enregistrements dans l''ensemble de donn�es';
  cSCanNotOpenResultSet = 'Ne peut ouvrir un ensemble de donn�es';
  cSCanNotOpenDataSetWhenDestroying ='Ne peut ouvrir un ensemble de donn�es alors que l''�tat du composant est dsDestroying';
  cSCircularLink = 'Lien circulaire cr�� par le Datasource';
  cSBookmarkWasNotFound = 'Le marque page (Bookmark) n''a pas �t� trouv�';
  cSIncorrectSearchFieldsNumber = 'Nombre incorrect de valeurs de recherche';
  cSInvalidOperationInTrans = 'Op�ration invalide dans un mode de transaction explicite';
  cSIncorrectSymbol = 'Symbole incorrect dans la liste des champs "%s".';
  cSIncorrectToken = 'Token incorrect suivi par ":"';
  cSIncorrectParamChar = 'Valeur non valide pour ParamChar';

  cSSelectedTransactionIsolation = 'Le niveau d''isolation de transaction s�lectionn� n''est pas support�';
  cSDriverNotSupported = 'Driver non support� %s';
  cSPattern2Long = 'Le Pattern est trop long';
  cSDriverNotCapableOutParameters = 'Le Driver n''est pas capable d''utiliser des param�tres';
  cSStatementIsNotAllowed = 'D�claration non permise';
  cSStoredProcIsNotAllowed = 'La proc�dure stock�e n''est pas permise';
  cSCannotPerformOperation = 'Ne peut effectuer cette op�ration sur une ensemble de donn�es ferm�';
  cSInvalidState = '�tat non valide';
  cSErrorConvertion = 'Erreur de conversion';
  cSDataTypeDoesNotSupported = 'Type de donn�e non support�';
  cSUnsupportedParameterType = 'Type de param�tre non support�';
  cSUnsupportedDataType = 'Type de donn�e non support�';
  cSErrorConvertionField = 'Erreur de conversion pour le champ "%s" vers le type SQL "%s"';
  cSBadOCI = 'Mauvaise version OCI [%s] . Version 8.0.3 ou plus ancienne requise';
  cSConnect2AsUser = 'Connexion � "%s" en tant qu''utilisateur "%s"';
  cSUnknownError = 'Erreur inconnue';
  cSFieldNotFound1 = 'Champ "%s" non trouv�';
  cSFieldNotFound2 = 'Champ %d non trouv�';

  cSLoginPromptFailure = 'Ne peut trouver le dialogue d''identification par d�faut. Ajoutez ,S.V.P. DBLogDlg dans la section uses section de votre fichier principal.';

  cSPropertyQuery = 'La requ�te peut prendre un certain temps sur des bases de donn�es importantes!';
  cSPropertyTables = 'Vous devriez la limiter via Catalogue et/ou Sch�ma.';
  cSPropertyColumns = 'Vous devriez la limiter via Catalogue, Sch�ma et/ou Nom de Table.';
  cSPropertyProcedures = 'Vous devriez la limiter via Catalogue et/ou Schema.';
  cSPropertySequences = 'Vous devriez la limiter via Catalogue et/ou Schema.';
  cSPropertyExecute = 'La Requ�te doit-elle s''ex�cuter quand m�me?';

  cSFormTest = '�diteur SQL ZEOS Test';
  cSButtonClose = '&Fermer';
  cSFormEditor = '�diteur SQL ZEOS';
  cSTabSheetSelect = 'Select SQL';
  cSMenuLoad = 'Charger';
  cSMenuSave = 'Sauver';
  cSButtonGenerate = '&G�n�rer';
  cSButtonCheck = '&V�rifier';
  cSButtonTest = '&Tester';
  cSButtonOk = '&OK';
  cSButtonCancel = 'A&nnuler';
  cSTableAlias = 'T&able alias';
  cSReplaceSQL = '&Remplacer le SQL';
  cSDialogOpenTitle = 'Ouvrir fichier SQL';
  cSDialogSaveTitle = 'Sauver dans un fichier SQL';
  cSSQLEditor = '�diteur SQL';
  cSDatabaseDialog = 'Ouvrir base existante';

  cSUpdateSQLNoResult = '"Update Refresh SQL" ne fourni aucun ensemble de r�sultat';
  cSUpdateSQLRefreshStatementcount ='La d�claration de l''"Update Refresh SQL" ne peut �tre qu''unique';

  {$IFDEF FPC}
  cSNotEditing = 'L''ensemble de donn�es n''est ni en modification ni en insertion';
  cSFieldTypeMismatch = 'Diff�rence de type pour le champ ''%s'', attendu: %s trouv�: %s';
  cSFieldSizeMismatch = 'Diff�rence de taille pour le champ ''%s'', attendue: %d trouv�e: %d';
  {$ENDIF}
  cSNeedField               = 'Le champ %s est requis, mais non renseign�.';

  cSFailedtoInitPrepStmt   = 'La d�claration a �chou�e � l''initialisation';
  cSFailedtoPrepareStmt    = 'La d�claration a �chou�e durant le processus de pr�paration';
  cSFailedToBindAllValues  = 'L''application a �chou� � pr�-relier toutes les valeurs';
  cSAttemptExecOnBadPrep   = 'Tentative d''ex�cuter une d�claration avant une pr�paration r�ussie.';
  cSBindingFailure         = '�chec � relier l''ensemble des param�tres';
  cSPreparedStmtExecFailure = 'La pr�paration de la d�claration a �chou�';
  cSBoundVarStrIndexMissing = 'Nom de la variable de relation "%s" inexistant';
  cSBindVarOutOfRange      = 'Index de la variable de relation hors limite: %d';
  cSFailedToBindResults    = 'L''application a �chou� � lier l''ensemble r�sultat';

//FOS+ 07112006
  cSRefreshRowOnlySupportedWithUpdateObject = 'La m�thode "refreshrow" n''est permise qu''avec un objet de mise � jour(Update)';
  cSMustBeInBrowseMode = 'Op�ration uniquement permise dans l''�tat dsBROWSE';

  cSUnKnownParamDataType = 'Param.DataType inconnu';
  cSFieldReadOnly        = ' A un champ en lecture seule on ne peut assigner une valeur : %s';
  cSInvalidUpdateCount     = '%d enregistrement(s) mis � jour. Un seul urait du l''�tre.';

  cSRowBufferWidthExceeded ='La taille du buffer de lignes a �t� d�pass�e. Essayez d''utiliser moins ou de plus longues colonnes dans la requ�te SQL.';
  cSPreviousResultStillOpen = 'L''ensemble de donn�es r�sultat pr�c�dent de cette instruction est encore ouvert';
//--- end added by Serge Girard ------------------------------------
{$ELSE !FRENCH}
// -> ms, 09/05/2005
{$IFDEF PORTUGUESE}
  cSSQLError1 = 'Erro SQL: %s';
  cSSQLError2 = 'Erro SQL: %s C�digo: %d';
  cSSQLError3 = 'Erro SQL: %s C�digo: %d SQL: %s';
  cSSQLError4 = 'Erro SQL: %s C�digo: %d Mensagem: %s';

  cSListCapacityError = 'Capacidade da Lista fora do limite (%d)';
  cSListCountError = 'Contagem da Lista fora do limite (%d)';
  cSListIndexError = '�ndice da Lista fora do limite (%d)';

  cSClonningIsNotSupported = 'Clonagem n�o � suportada por esta classe';
  cSImmutableOpIsNotAllowed = 'A opera��o n�o � permitida para cole��o imut�vel';
  cSStackIsEmpty = 'Pilha est� vazia';
  cSVariableWasNotFound = 'Vari�vel "%s" n�o foi encontrada';
  cSFunctionWasNotFound = 'Function "%s" n�o foi encontrada';
  cSInternalError = 'Erro interno';
  cSSyntaxErrorNear = 'Erro de sintaxe pr�ximo a "%s"';
  cSSyntaxError = 'Erro de sintaxe';
  cSUnknownSymbol = 'S�mbolo desconhecido "%s"';
  cSUnexpectedExprEnd = 'Final inesperado de express�o';
  cSRightBraceExpected = ') esperado';
  cSParametersError = 'Esperado %d par�metros mas foi encontrado %d';
  cSExpectedMoreParams = 'Esperado mais que 2 par�metros';
  cSInvalidVarByteArray = 'VarByte array inv�lido';
  cSVariableAlreadyExists = 'Vari�vel "%s" j� existe';
  cSTypesMismatch = 'Tipos n�o combinam';
  cSUnsupportedVariantType = 'Tipo variante n�o suportado';
  cSUnsupportedOperation = 'Opera��o n�o suportada';

  cSTokenizerIsNotDefined = 'Sinalizador n�o definido';
  cSLibraryNotFound = 'Nenhuma biblioteca din�mica da lista %s foi encontrada';
  cSEncodeDateIsNotSupported = 'Esta vers�o n�o suporta isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Esta vers�o n�o suporta supported isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Esta vers�o n�o suporta supported isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'N�o foi poss�vel obter os dados do ResultSet';
  cSRowBufferIsNotAssigned = 'Buffer da Linha n�o atribu�do';
  cSColumnIsNotAccessable = 'Coluna com �ndice %d n�o � acess�vel';
  cSConvertionIsNotPossible = 'A convers�o da coluna %d de %s para %s n�o � poss�vel';
  cSCanNotAccessBlobRecord = 'N�o � poss�vel acessar um registro BLOB na coluna %d com o tipo %s';
  cSRowDataIsNotAvailable = 'Dados na Linha n�o dispon�veis';
  cSResolverIsNotSpecified = 'Resolver n�o foi especificado para este ResultSet';
  cSResultsetIsAlreadyOpened = 'ResultSet j� est� aberto';
  cSCanNotUpdateEmptyRow = 'N�o � poss�vel atualizar uma linha vazia';
  cSCanNotUpdateDeletedRow = 'N�o � poss�vel atualizar uma linha apagada';
  cSCanNotDeleteEmptyRow = 'N�o � poss�vel apagar uma linha vazia';
  cSCannotUseCommit = 'Voc� n�o pode usar Commit no modo AutoCommit';
  cSCannotUseRollBack = 'Voc� n�o pode usar Rollback no modo AutoCommit';
  cSCanNotUpdateComplexQuery = 'N�o � poss�vel atualizar uma query complexa com mais de uma tabela';
  cSCanNotUpdateThisQueryType = 'N�o � poss�vel atualizar este tipo de query';
  cSDriverWasNotFound = 'O driver de banco de dados requisitado n�o foi encontrado';
  cSCanNotConnectToServer = 'N�o foi poss�vel conectar ao servidor SQL';
  cSTableIsNotSpecified = 'Tabela n�o especificada';
  cSLiveResultSetsAreNotSupported = 'Live query n�o � suportado por esta classe';
  cSInvalidInputParameterCount = 'A contagem do par�metro de entrada � menor que o esperado';
  cSIsolationIsNotSupported = 'O n�vel de isolamento da Transa��o n�o � suportado';
  cSColumnWasNotFound = 'Coluna com o nome "%s" n�o foi encontrada';
  cSWrongTypeForBlobParameter = 'Tipo errado para par�metro Blob';
  cSIncorrectConnectionURL = 'Conex�o incorreta URL: %s';
  cSUnsupportedProtocol = 'Protocolo n�o suportado: %s';
  cSUnsupportedByDriver    = 'O Driver n�o suporta este recurso nativamente: [%s]';

  cSConnectionIsNotOpened = 'Conex�o ainda n�o est� aberta.';
  cSInvalidOpInAutoCommit = 'Opera��o inv�lida no modo AutoCommit.';
  cSInvalidOpInNonAutoCommit = 'Opera��o inv�lida quando o modo AutoCommit � False.';
  cSInvalidOpPrepare = 'Prepare transaction somente � poss�vel ap�s comandar StartTransaction';

  cSConnectionIsNotAssigned = 'Componente de conex�o de banco de dados n�o atribu�do';
  cSQueryIsEmpty = 'A consulta SQL est� vazia';
  cSCanNotExecuteMoreQueries = 'N�o � poss�vel executar mais que uma query';
  cSOperationIsNotAllowed1 = 'Opera��o n�o permitida no modo FORWARD ONLY';
  cSOperationIsNotAllowed2 = 'Opera��o n�o permitida no modo READ ONLY';
  cSOperationIsNotAllowed3 = 'Opera��o n�o permitida no modo %s';
  cSOperationIsNotAllowed4 = 'Opera��o n�o permitida para DataSet fechado';
  cSNoMoreRecords = 'Nenhum registro no ResultSet';
  cSCanNotOpenResultSet = 'N�o foi poss�vel abrir o ResultSet';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'DataSource possui um link circular';
  cSBookmarkWasNotFound = 'Bookmark n�o foi encontrado';
  cSIncorrectSearchFieldsNumber = 'N�mero incorreto de valores de campos de procura';
  cSInvalidOperationInTrans = 'Opera��o inv�lida no modo de transa��o expl�cita';
  cSIncorrectSymbol = 'S�mbolo incorreto na lista de campos "%s".';
  cSIncorrectToken = 'Sinal incorreto seguido por ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'O n�vel selecionado do isolamento da transa��o n�o � suportado';
  cSDriverNotSupported = 'Driver n�o suportado %s';
  cSPattern2Long = 'Padr�o � muito longo';
  cSDriverNotCapableOutParameters = 'O Driver n�o suporta a passagem de par�metros';
  cSStatementIsNotAllowed = 'Declara��o n�o permitida';
  cSStoredProcIsNotAllowed = 'A stored procedure n�o � permitida';
  cSCannotPerformOperation = 'N�o � poss�vel executar a opera��o num ResultSet fechado';
  cSInvalidState = 'Estado inv�lido';
  cSErrorConvertion = 'Erro de convers�o';
  cSDataTypeDoesNotSupported = 'Tipo de dado n�o suportado';
  cSUnsupportedParameterType = 'Tipo de par�metro n�o suportado';
  cSUnsupportedDataType = 'Tipo de dado n�o suportado';
  cSErrorConvertionField = 'Erro de convers�o para do campo "%s" para SQLType "%s"';
  cSBadOCI = 'Vers�o de OCI incompat�vel [% s]. Requer 8.0.3 ou mais antigo';
  cSConnect2AsUser = 'Conecte "% s" como usu�rio "% s"';
  cSUnknownError = 'Erro desconhecido';
  cSFieldNotFound1 = 'Campo "%s" n�o foi encontrado';
  cSFieldNotFound2 = 'Campo %d n�o foi encontrado';

  cSLoginPromptFailure = 'N�o foi poss�vel encontrar o di�logo padr�o de login. Por favor adicione DBLogDlg para a se��o uses de seu arquivo principal.';

  cSPropertyQuery = 'A Query poder� demorar em bancos de dados grandes!';
  cSPropertyTables = 'Voc� deveria limitar por Catalogo e/ou Esquema.';
  cSPropertyColumns = 'Voc� deveria limitar por Catalogo, Esquema e/ou Tabela.';
  cSPropertyProcedures = 'Voc� deveria limitar por Catalogo e/ou Esquema.';
  cSPropertySequences = 'Voc� deveria limitar por Catalogo e/ou Esquema..';
  cSPropertyExecute = 'Executar a Query de qualquer maneira?';

  cSFormTest = 'Teste Editor ZEOS SQL';
  cSButtonClose = '&Fechar';
  cSFormEditor = 'Editor ZEOS SQL';
  cSTabSheetSelect = 'SQL Select';
  cSMenuLoad = 'Carregar';
  cSMenuSave = 'Salvar';
  cSButtonGenerate = '&Gerar';
  cSButtonCheck = '&Verificar';
  cSButtonTest = '&Testar';
  cSButtonOk = '&OK';
  cSButtonCancel = '&Cancelar';
  cSTableAlias = '&Alias Tabela';
  cSReplaceSQL = '&Substituir SQL';
  cSDialogOpenTitle = 'Abrir Arquivo SQL';
  cSDialogSaveTitle = 'Salvar Arquivo SQL';
  cSSQLEditor = 'Editor SQL';
  cSDatabaseDialog = 'Abrir Banco de Dados existente';

  cSUpdateSQLNoResult = 'SQL Update Refresh resultou num conjunto vazio';
  cSUpdateSQLRefreshStatementcount ='Usar somente 1 declara��o SQL para Update Refresh';
  {$IFDEF FPC}
  cSNotEditing = 'Dataset n�o est� em modo de edi��o ou inser��o';
  cSFieldTypeMismatch = 'Tipo inv�lido para o campo ''%s'', esperado: %s atual: %s';
  cSFieldSizeMismatch = 'Tamanho Inv�lido para o campo ''%s'', esperado: %d atual: %d';
  {$ENDIF}
  cSNeedField               = 'O campo %s � obrigat�rio, mas n�o foi preenchido.';

  cSFailedtoInitPrepStmt   = 'A declara��o preparada falhou ao inicializar';
  cSFailedtoPrepareStmt    = 'A declara��o falhou durante o processo de preparo';
  cSFailedToBindAllValues  = 'A Aplica��o falhou na tradu��o de todos os valores';
  cSAttemptExecOnBadPrep   = 'Tentativa de executar uma declara��o que n�o foi corretamente preparada';
  cSBindingFailure         = 'Falha ao traduzir o conjunto de par�metros';
  cSPreparedStmtExecFailure = 'A declara��o preparada falhou ao executar';
  cSBoundVarStrIndexMissing = '�ndice de texto "%s" da vari�vel de limite n�o existe';
  cSBindVarOutOfRange      = '�ndice da vari�vel de limite fora de alcance: %d';
  cSFailedToBindResults    = 'A Aplica��o falhou ao tratar o result set';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'O m�todo RefreshRow somente � suportado com um update object';
  cSMustBeInBrowseMode = 'A Opera��o � permitida somente no modo dsBrowse';

  cSUnKnownParamDataType = 'Param.DataType � de tipo desconhecido';
  cSFieldReadOnly        = 'O campo %d � somente leitura e n�o p�de receber dados';
  cSInvalidUpdateCount   = '%d registro(s) atualizados. Apenas um registro deveria ter sido atualizado.';

  cSRowBufferWidthExceeded ='O tamanho do buffer para linhas (Rows) foi excedido. Tente usar menos ou mais colunas na query SQL';
{$ELSE}

{$IFDEF DUTCH}
  cSSQLError1 = 'SQL Fout: %s';
  cSSQLError2 = 'SQL Fout: %s Code: %d';
  cSSQLError3 = 'SQL Fout: %s Code: %d SQL: %s';
  cSSQLError4 = 'SQL Fout: %s Code: %d Bericht: %s';

  cSListCapacityError = 'Lijst capaciteit buiten bereik (%d)';
  cSListCountError = 'Lijst aantal buiten bereik (%d)';
  cSListIndexError = 'Lijst index buiten bereik (%d)';

  cSClonningIsNotSupported = 'Kloonen worden niet ondersteund in deze klasse';
  cSImmutableOpIsNotAllowed = 'Deze operatie is niet ondersteund voor immutable collection';
  cSStackIsEmpty = 'Stack is leeg';
  cSVariableWasNotFound = 'Variabele "%s" niet gevonden';
  cSFunctionWasNotFound = 'Functie "%s" niet gevonden';
  cSInternalError = 'Interne fout';
  cSSyntaxErrorNear = 'Syntaxis fout bij "%s"';
  cSSyntaxError = 'Syntaxis fout';
  cSUnknownSymbol = 'Onbekend symbool "%s"';
  cSUnexpectedExprEnd = 'Onverwacht einde van de expressie';
  cSRightBraceExpected = ') verwacht';
  cSParametersError = 'Verwacht worden %d parameters maar er zijn er %d gevonden';
  cSExpectedMoreParams = 'Meer dan 2 parameters werden verwacht';
  cSInvalidVarByteArray = 'Ongeldig VarByte array';
  cSVariableAlreadyExists = 'Variabele "%s" bestaat al';
  cSTypesMismatch = 'Types komen niet overeen';
  cSUnsupportedVariantType = 'Niet ondersteund variant type';
  cSUnsupportedOperation = 'Niet ondersteunde operatie';

  cSTokenizerIsNotDefined = 'Tokenizer is niet gedefinieerd';
  cSLibraryNotFound = 'DLL van de lijst %s werd niet gevonden';
  cSEncodeDateIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_date niet';
  cSEncodeTimeIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_time niet';
  cSEncodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_timestamp niet';
  cSDecodeDateIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_date niet';
  cSDecodeTimeIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_time niet';
  cSDecodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_timestamp niet';

  cSCanNotRetrieveResultSetData = 'Kan ResultSet data niet ophalen';
  cSRowBufferIsNotAssigned = 'Row buffer is niet toegekend';
  cSColumnIsNotAccessable = 'Kolom met index %d is niet bereikbaar';
  cSConvertionIsNotPossible = 'Conversie is niet mogelijk voor kolom %d van %s tot %s';
  cSCanNotAccessBlobRecord = 'Kan het blob record in kolom %d met type %s niet benaderen';
  cSRowDataIsNotAvailable = 'Rij data is niet beschikbaar';
  cSResolverIsNotSpecified = 'Resolver is niet gespecificeerd voor deze ResultSet';
  cSResultsetIsAlreadyOpened = 'ResultSet is al geopend';
  cSCanNotUpdateEmptyRow = 'Kan een lege rij niet updaten';
  cSCanNotUpdateDeletedRow = 'Kan een verwijderde rij niet updaten';
  cSCanNotDeleteEmptyRow = 'Kan een lege rij niet verwijderen';
  cSCannotUseCommit = 'Commit in autocommit mode is niet mogelijk';
  cSCannotUseRollBack = 'Rollback in autocommit mode is niet mogelijk';
  cSCanNotUpdateComplexQuery = 'Kan een complexe query met meerdere tabellen niet updaten';
  cSCanNotUpdateThisQueryType = 'Kan dit query type niet updaten';
  cSDriverWasNotFound = 'Gevraagde database driver is niet gevonden';
  cSCanNotConnectToServer = 'Kan geen verbinding maken met de SQL server';
  cSTableIsNotSpecified = 'Tabel is niet gespecifieerd';
  cSLiveResultSetsAreNotSupported = 'Live query is niet ondersteund door deze klasse';
  cSInvalidInputParameterCount = 'Input parameter aantal is lager dan verwacht';
  cSIsolationIsNotSupported = 'Transactie isolatie niveau wordt niet ondersteund';
  cSColumnWasNotFound = 'Kolom met naam "%s" bestaat niet';
  cSWrongTypeForBlobParameter = 'Verkeerde type voor Blob parameter';
  cSIncorrectConnectionURL = 'Ongeldige connectie URL: %s';
  cSUnsupportedProtocol = 'Niet ondersteund protocol: %s';
  cSUnsupportedByDriver    = 'De driver ondersteunt deze functie niet: [%s]';

  cSConnectionIsNotOpened = 'Verbinding is niet gemaakt.';
  cSInvalidOpInAutoCommit = 'Ongeldige operatie in AutoCommit mode.';
  cSInvalidOpInNonAutoCommit = 'Ongeldige operatie in non AutoCommit mode.';
  cSInvalidOpPrepare = 'Transactie voorbereiden is enkel mogelijk bij de eerste aanroep van Starttransaction!';

  cSConnectionIsNotAssigned = 'Database connectie component is niet toegekend';
  cSQueryIsEmpty = 'SQL Query is leeg';
  cSCanNotExecuteMoreQueries = 'Kan niet meerdere queries uitvoeren';
  cSOperationIsNotAllowed1 = 'Bewerking is niet toegestaan in FORWARD ONLY mode';
  cSOperationIsNotAllowed2 = 'Bewerking is niet toegestaan in READ ONLY mode';
  cSOperationIsNotAllowed3 = 'Bewerking is niet toegestaan in %s mode';
  cSOperationIsNotAllowed4 = 'Bewerking is niet toegestaan voor gesloten dataset';
  cSNoMoreRecords = 'Geen records meer aanwezig in ResultSet';
  cSCanNotOpenResultSet = 'Kan een ResultSet niet openen';
  cSCanNotOpenDataSetWhenDestroying ='Kan een Dataset niet openen wanneer de componentstate=dsDestroying';
  cSCircularLink = 'Databron maakt een oneindige verbindingslus';
  cSBookmarkWasNotFound = 'Bookmark niet gevonden';
  cSIncorrectSearchFieldsNumber = 'Incorrect aantal zoekvelden';
  cSInvalidOperationInTrans = 'Ongeldige operatie in explicit transaction mode';
  cSIncorrectSymbol = 'Ongeldig symbool in veld lijst "%s".';
  cSIncorrectToken = 'Ongeldig teken gevolgd door ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'Geselecteerd transactie isolatie niveau niet ondersteund';
  cSDriverNotSupported = 'Driver niet ondersteund %s';
  cSPattern2Long = 'Patroon is te lang';
  cSDriverNotCapableOutParameters = 'Driver ondersteunt geen out parameters';
  cSStatementIsNotAllowed = 'Statement is niet toegestaan';
  cSStoredProcIsNotAllowed = 'Stored procedures zijn niet toegestaan';
  cSCannotPerformOperation = 'Kan operatie niet uitvoeren op een gesloten ResultSet';
  cSInvalidState = 'Ongeldige status';
  cSErrorConvertion = 'Conversiefout';
  cSDataTypeDoesNotSupported = 'Data type is niet onderstuend';
  cSUnsupportedParameterType = 'Niet ondersteund parameter type';
  cSUnsupportedDataType = 'Niet ondersteund data type';
  cSErrorConvertionField = 'Conversie fout voor veld "%s" naar SQLType "%s"';
  cSBadOCI = 'Ongeschikte OCI version [%s]. Vereist is 8.0.3 of nieuwer';
  cSConnect2AsUser = 'Verbinden met "%s" als gebruiker "%s"';
  cSUnknownError = 'Onbekende fout';
  cSFieldNotFound1 = 'Veld "%s" niet gevonden';
  cSFieldNotFound2 = 'Veld %d niet gevonden';

  cSLoginPromptFailure = 'Kan de standaard login prompt niet vinden.  Voeg DBLogDlg toe aan de uses sectie.';

  cSPropertyQuery = 'De Query kan enige tijd duren bij grote databases!';
  cSPropertyTables = 'Limiet op Catalog en/of Schema is vereist.';
  cSPropertyColumns = 'Limiet op Catalog, Schema en/of tablenaam is vereist.';
  cSPropertyProcedures = 'Limiet op Catalog en/of Schema is vereist.';
  cSPropertySequences = 'Limiet op Catalog en/of Schema is vereist.';
  cSPropertyExecute = 'Dient de Query toch te worden uitgevoerd?';

  cSFormTest = 'ZEOS SQL Editor Test';
  cSButtonClose = '&Sluiten';
  cSFormEditor = 'ZEOS SQL Editor';
  cSTabSheetSelect = 'Select SQL';
  cSMenuLoad = 'Laden';
  cSMenuSave = 'Opslaan';
  cSButtonGenerate = '&Genereren';
  cSButtonCheck = 'C&heck';
  cSButtonTest = '&Test';
  cSButtonOk = '&OK';
  cSButtonCancel = '&Annuleren';
  cSTableAlias = 'Tabel al&ias';
  cSReplaceSQL = '&Vervang SQL';
  cSDialogOpenTitle = 'SQL Bestand Openen';
  cSDialogSaveTitle = 'SQL Bestand Opslaan';
  cSSQLEditor = 'SQL Editor';
  cSDatabaseDialog = 'Open bestaande database';

  cSUpdateSQLNoResult = 'Der zuvor aktualisierte SQL liefert kein Resultset zur�ck';
  cSUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count moet 1 zijn';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset is niet in edit of insert modus';
  cSFieldTypeMismatch = 'Type mismatch voor veld ''%s'', verwacht: %s actueel: %s';
  cSFieldSizeMismatch = 'Size mismatch voor veld ''%s'', verwacht: %d actueel: %d';
  {$ENDIF}
  cSNeedField               = 'Veld %s is verplicht, maar niet ingevuld.';

  cSFailedtoInitPrepStmt   = 'Initialisatie van Prepared statement mislukt';
  cSFailedtoPrepareStmt    = 'Statement mislukt tijdens prepare';
  cSFailedToBindAllValues  = 'Pre-bind van alle waarden is mislukt';
  cSAttemptExecOnBadPrep   = 'Poging om een statement uit te voeren voor een succesvolle prepare';
  cSBindingFailure         = 'Binding van parameterset mislukt';
  cSPreparedStmtExecFailure = 'Uitvoeren van Prepared statement mislukt';
  cSBoundVarStrIndexMissing = 'Tekst index van bound variable bestaat niet: "%s"';
  cSBindVarOutOfRange      = 'Bound variable index buiten bereik: %d';
  cSFailedToBindResults    = 'Binding van resultaat mislukt';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'De refreshrow methode is enkel ondersteund vooreen update object';
  cSMustBeInBrowseMode = 'Bewerking is enkel toegestaan in dsBROWSE status';

  cSUnKnownParamDataType = 'Param.DataType is onbekend';
  cSFieldReadOnly        = 'Readonly veld kan geen waarde toegewezen krijgen: %d';
  cSInvalidUpdateCount     = '%d record(s) gewijzigd. Slechts 1 record had gewijzigd mogen zijn.';

  cSRowBufferWidthExceeded ='Rij buffer grootte overschreden. Probeer minder kolommen te gebruiken in je SQL query.';
{$ELSE}
// <- ms, 09/05/2005

// -> ms, 03/05/2005
{$IFDEF GERMAN}
  cSSQLError1 = 'SQL Fehler: %s';
  cSSQLError2 = 'SQL Fehler: %s Code: %d';
  cSSQLError3 = 'SQL Fehler: %s Code: %d SQL: %s';
  cSSQLError4 = 'SQL Fehler: %s Code: %d Meldung: %s';

  cSListCapacityError = 'Die Listenkapazit�t �bersteigt die definierte Grenze (%d)';
  cSListCountError = 'Der Listenz�hler ist au�erhalb seiner definierten Grenzen (%d)';
  cSListIndexError = 'Der Listenindex ist au�erhalb der definierten Grenzen (%d)';

  cSClonningIsNotSupported = 'Diese Klasse kann nicht geklont werden';
  cSImmutableOpIsNotAllowed = 'Diese Operation ist bei nicht �nderbaren Collections nicht erlaubt';
  cSStackIsEmpty = 'Der Stack ist leer';
  cSVariableWasNotFound = 'Die Variable "%s" wurde nicht gefunden';
  cSFunctionWasNotFound = 'Die Funktion "%s" wurde nicht gefunden';
  cSInternalError = 'Interner Fehler';
  cSSyntaxErrorNear = 'Syntax Fehler bei "%s"';
  cSSyntaxError = 'Syntax Fehler';
  cSUnknownSymbol = 'Unbekanntes Symbol "%s"';
  cSUnexpectedExprEnd = 'Unerwartetes Ende des Ausdrucks';
  cSRightBraceExpected = ') erwartet';
  cSParametersError = 'Es werden %d Parameter erwartet, aber nur %d Parameter gefunden';
  cSExpectedMoreParams = 'Es werden mehr als zwei Parameter erwartet';
  cSInvalidVarByteArray = 'Ung�ltiges VarByte Array';
  cSVariableAlreadyExists = 'Die Variable "%s" existiert bereits';
  cSTypesMismatch = 'Inkompatible Typen';
  cSUnsupportedVariantType = 'Nicht unterst�tzter Variant-Typ';
  cSUnsupportedOperation = 'Nicht unterst�tzte Operation';
  cSUnsupportedByDriver    = 'Der Treiber unterst�tzt dieses Feature nicht von haus aus: [%s]';

  cSTokenizerIsNotDefined = 'Tokenizer wurde nicht definiert';
  cSLibraryNotFound = 'Es wurde keine der in %s gelisteten DLL''s gefunden';
  cSEncodeDateIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_date" nicht';
  cSEncodeTimeIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_time" nicht';
  cSEncodeTimestampIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_timestamp" nicht';
  cSDecodeDateIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_date" nicht';
  cSDecodeTimeIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_time" nicht';
  cSDecodeTimestampIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_timestamp" nicht';

  cSCanNotRetrieveResultSetData = 'Die Ergebnismenge kann nicht ermittelt werden';
  cSRowBufferIsNotAssigned = 'Der Zeilen-Buffer ist nicht zugewiesen';
  cSColumnIsNotAccessable = 'Auf die Spalte (Tabellenfeld) mit dem Index %d kann nicht zugegriffen werden';
  cSConvertionIsNotPossible = 'Eine Konvertierung der Spalte (Tabellenfeld) %d von %s bis %s kann nicht durchgef�hrt werden';
  cSCanNotAccessBlobRecord = 'Auf den BLOB-Datensatz in Spalte (Tabellenfeld) %d vom Typ %s kann nicht zugegriffen werden';
  cSRowDataIsNotAvailable = 'Die Zeilendaten (Datensatzdaten) sind nicht verf�gbar';
  cSResolverIsNotSpecified = 'F�r diese Ergebnismenge wurde kein sog. "Resolver" angegeben';
  cSResultsetIsAlreadyOpened = 'Die Ergebnismenge ist bereits ge�ffnet';
  cSCanNotUpdateEmptyRow = 'Eine leere Datenzeile kann nicht aktualisiert werden';
  cSCanNotUpdateDeletedRow = 'Eine gel�schte Datenzeile kann nicht aktualisiert werden';
  cSCanNotDeleteEmptyRow = 'Eine leere Datenzeile kann nicht gel�scht werden';
  cSCannotUseCommit = 'COMMIT kann im AUTOCOMMIT-Modus nicht verwendet werden';
  cSCannotUseRollBack = 'ROLLBACK kann im AUTOCOMMIT-Modus nicht verwendet werden';
  cSCanNotUpdateComplexQuery = 'Ein Query, dessen Ergebnismenge aus mehr als einer Tabelle stammt, kann nicht aktualisiert werden';
  cSCanNotUpdateThisQueryType = 'Diese Art von Queries kann nicht aktualisiert werden';
  cSDriverWasNotFound = 'Der angegebene Datenbanktreiber wurde nicht gefunden';
  cSCanNotConnectToServer = 'Kann keine Verbindung zum SQL Server herstellen';
  cSTableIsNotSpecified = 'Tabelle ist nicht spezifiziert';
  cSLiveResultSetsAreNotSupported = 'Ein "Live Query" wird von dieser Klasse nicht unterst�tzt';
  cSInvalidInputParameterCount = 'Es wurden weniger Eingabeparameter angegeben, als erwartet';
  cSIsolationIsNotSupported = 'Der gew�hlte Trasaktions-Isolationslevel wird nicht unterst�tzt';
  cSColumnWasNotFound = 'Eine Tabellenspalte namens "%s" wurde nicht gefunden';
  cSWrongTypeForBlobParameter = 'Falscher Typ f�r einen BLOB-Parameter';
  cSIncorrectConnectionURL = 'Falsche Verbindungs-URL: %s';
  cSUnsupportedProtocol = 'Nicht unterst�tztes Protokoll: %s';

  cSConnectionIsNotOpened = 'Die Verbindung zur Datenbank ist noch nicht hergestellt';
  cSInvalidOpInAutoCommit = 'Ung�ltige Operation im AUTOCOMMIT-Modus';
  cSInvalidOpInNonAutoCommit = 'Ung�ltige Operation au�erhalb des AUTOCOMMIT-Modus';
  cSInvalidOpPrepare = 'Transaktion vorzubereiten ist nur beim ersten Aufruf von Starttransaction m�glich!';

  cSConnectionIsNotAssigned = 'Die Datenbank-Verbindungskomponente ist nicht angegeben';
  cSQueryIsEmpty = 'SQL Query leer';
  cSCanNotExecuteMoreQueries = 'Mehr als ein Query kann nicht abgearbeitet werden';
  cSOperationIsNotAllowed1 = 'Die Operation ist im FORWARD ONLY Modus nicht erlaubt';
  cSOperationIsNotAllowed2 = 'Die Operation ist im READ ONLY Modus nicht erlaubt';
  cSOperationIsNotAllowed3 = 'Die Operation ist im %s Modus nicht erlaubt';
  cSOperationIsNotAllowed4 = 'Die Operation ist bei einem geschlossenen DataSet nicht erlaubt';
  cSNoMoreRecords = 'Es gibt keine weiteren Datens�tze in der Ergebnismenge';
  cSCanNotOpenResultSet = 'Die Ergebnismenge kann nicht ge�ffnet werden';
  cSCanNotOpenDataSetWhenDestroying ='Dataset kann nicht im Komponenten-Status dsDestroying ge�ffnet werden';
  cSCircularLink = 'Die DataSource hat einen zirkul�ren Verweis';
  cSBookmarkWasNotFound = 'Das Lesezeichen (Bookmark) wurde nicht gefunden';
  cSIncorrectSearchFieldsNumber = 'Die Anzahl der Suchfeldwerte ist nicht korrekt';
  cSInvalidOperationInTrans = 'Ung�ltige Operatio im Zustand einer expliziten Transaktion';
  cSIncorrectSymbol = 'Falsches Symbol in der Feldliste "%s".';
  cSIncorrectToken = 'Falsches Token gefolgt von ":"';
  cSIncorrectParamChar = 'Ung�ltiger Wert f�r Parameter-Indikator';

  cSSelectedTransactionIsolation = 'Der gew�hlte Transaktions-Isolationslevel wird nicht unterst�tzt';
  cSDriverNotSupported = 'Der Treiber wird nicht unterst�tzt: %s';
  cSPattern2Long = 'Das Muster (Pattern) ist zu lang';
  cSDriverNotCapableOutParameters = 'Der Treiber beherrscht keine Parameter';
  cSStatementIsNotAllowed = 'Diese Anweisung ist nicht erlaubt';
  cSStoredProcIsNotAllowed = 'Diese Stored Procedure ist nicht erlaubt';
  cSCannotPerformOperation = 'Auf eine geschlossene Ergebnismenge k�nnen keine Operationen ausgef�hrt werden';
  cSInvalidState = 'Ung�ltiger Status';
  cSErrorConvertion = 'Konvertierungsfehler';
  cSDataTypeDoesNotSupported = 'Der Datentyp wird nicht unterst�tzt';
  cSUnsupportedParameterType = 'Der Parametertyp wird nicht unterst�tzt';
  cSUnsupportedDataType = 'Der Datentyp wird nicht unterst�tzt';
  cSErrorConvertionField = 'Konvertierungsfehler bei Feld "%s" nach SQL-Typ "%s"';
  cSBadOCI = 'Die OCI Version 8.0.3 (oder �lter) wird ben�tigt! Aktuelle Version: %s';
  cSConnect2AsUser = 'Verbinde zu "%s" als User "%s"';
  cSUnknownError = 'Unbekannter Fehler';
  cSFieldNotFound1 = 'Das Feld "%s" wurde nicht gefunden';
  cSFieldNotFound2 = 'Das Feld %d wurde nicht gefunden';

  cSLoginPromptFailure = 'Der Standard-Login-Dialog konnte nicht gefunden werden. Bitte DBLogDlg in die USES-Sektion der Haupt-Unit hinzuf�gen';

  cSPropertyQuery = 'Die Abfrage kann bei gro�en Datenbanken eine Weile dauern!';
  cSPropertyTables = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  cSPropertyColumns = 'Sie sollte durch die Angabe von Catalog, Schema und/oder Tabellenname eingeschr�nkt werden.';
  cSPropertyProcedures = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  cSPropertySequences = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  cSPropertyExecute = 'Soll die Abfrage trotzdem ausgef�hrt werden?';

  cSFormTest = 'ZEOS SQL Editor Test';
  cSButtonClose = '&Schlie�en';
  cSFormEditor = 'ZEOS SQL Editor';
  cSTabSheetSelect = 'SQL aus&w�hlen';
  cSMenuLoad = '�ffnen';
  cSMenuSave = 'Speichern';
  cSButtonGenerate = '&Generieren';
  cSButtonCheck = 'Syntax &Pr�fen';
  cSButtonTest = 'Befehl &Testen';
  cSButtonOk = '&OK';
  cSButtonCancel = '&Abbruch';
  cSTableAlias = 'Tabllen-Alias';
  cSReplaceSQL = 'SQL &ersetzen';
  cSDialogOpenTitle = 'SQL Script �ffnen';
  cSDialogSaveTitle = 'SQL Script speichern';
  cSSQLEditor = 'SQL Editor';
  cSDatabaseDialog = 'Existierende Datenbank �ffnen';

  cSUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  cSUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  cSNotEditing = 'Das DataSet ist nicht im "�ndern" oder "Einf�ge" Modus.';
  cSFieldTypeMismatch = 'Der Typ f�r Feld ''%s'' stimmt nicht. Erwartet wird %s der Typ ist aber momentan %s';
  cSFieldSizeMismatch = 'Die Gr��e des Feldes ''%s'' stimmt nicht. Erwartet wird  %d die Gr��e ist aber momentan %d';
  {$ENDIF}
  cSNeedField               = 'Feld %s ben�tigt einen Wert, welcher nicht zugewiesen wurde.';

  cSFailedtoInitPrepStmt   = 'Die Initialisierung f�r vorbereitete Abfrage ist gescheitert';
  cSFailedtoPrepareStmt    = 'Abfrage ist w�rend des Vorbereitungsprozesses gescheitert.';
  cSFailedToBindAllValues  = 'Anwendung konnte nicht alle Werte �bergeben';
  cSAttemptExecOnBadPrep   = 'Es wurde versucht eine nicht erfolgreich vorbereitete Abfrage auszuf�hren';
  cSBindingFailure         = 'Konnte nicht alle ausgew�hlten Parameter der Abfrage binden';
  cSPreparedStmtExecFailure = 'Vorbeitet Abfrage scheiterte beim Ausf�hren';
  cSBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  cSBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  cSFailedToBindResults    = 'Translate: Application failed to bind to the result set';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  cSMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  cSUnKnownParamDataType = 'Unbekannter Parameter-Datentyp';
  cSFieldReadOnly          = 'Einem "Nur-Lesen" Feld kann kein Wert zugewiesen werden: %d';
  cSInvalidUpdateCount     = '%d Datens�tze ge�ndert. Exakt ein Datensatz sollte ge�ndert werden.';

  cSRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}
  // -> fduenas, 28/06/2005
{$IFDEF SPANISH} //Spanish translations
  cSSQLError1 = 'Error SQL: %s';
  cSSQLError2 = 'Error SQL: %s C�digo: %d';
  cSSQLError3 = 'Error SQL: %s C�digo: %d SQL: %s';
  cSSQLError4 = 'Error SQL: %s C�digo: %d Mensage: %s';

  cSListCapacityError = 'List capacity fuera de l�mites (%d)';
  cSListCountError = 'List count fuera de l�mites (%d)';
  cSListIndexError = 'List index fuera de l�mites (%d)';

  cSClonningIsNotSupported = 'La Clonaci�n no est� soportada por esta clase';
  cSImmutableOpIsNotAllowed = 'Operaci�n no permitida en colecciones no modificables';
  cSStackIsEmpty = 'La Pila (Stack) est� vac�a';
  cSVariableWasNotFound = 'Variable "%s" no encontrada';
  cSFunctionWasNotFound = 'Funci�n "%s" no encontrada';
  cSInternalError = 'Error interno';
  cSSyntaxErrorNear = 'Error de sintaxis cerca de "%s"';
  cSSyntaxError = 'Error de sintaxis';
  cSUnknownSymbol = 'S�mbolo "%s" desconocido';
  cSUnexpectedExprEnd = 'Fin de expresi�n inesperado';
  cSRightBraceExpected = ') esperado';
  cSParametersError = 'Se esperaban %d par�metros pero solo %d fueron encontrados';
  cSExpectedMoreParams = 'Se esperaban m�s de dos par�metros';
  cSInvalidVarByteArray = 'Arreglo VarByte inv�lido';
  cSVariableAlreadyExists = 'La variable "%s" ya existe';
  cSTypesMismatch = 'Los Tipos no coinciden';
  cSUnsupportedVariantType = 'Tipo de Variant no soportando';
  cSUnsupportedOperation = 'Operaci�n no soportada';

  cSTokenizerIsNotDefined = 'El objeto Tokenizer no est� definido';
  cSLibraryNotFound = 'Ninguna librer�a din�mica de la lista %s fue encontrada';
  cSEncodeDateIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'No se pueden obtener datos del Resultset';
  cSRowBufferIsNotAssigned = 'Buffer de l�nea no asignado';
  cSColumnIsNotAccessable = 'La columna con �ndice %d no est� accesible';
  cSConvertionIsNotPossible = 'La conversi�n no es posible para la columna %d de %s a %s';
  cSCanNotAccessBlobRecord = 'No se puede accesar al registro del blob en la columna %d con tipo %s';
  cSRowDataIsNotAvailable = 'Datos de l�nea no disponibles';
  cSResolverIsNotSpecified = 'El objeto Resolver no est� especificado para este ResultSet';
  cSResultsetIsAlreadyOpened = 'El Resultset ya est� abierto';
  cSCanNotUpdateEmptyRow = 'No se puede actualizar una l�nea vac�a';
  cSCanNotUpdateDeletedRow = 'No se puede actualizar una l�nea borrada';
  cSCanNotDeleteEmptyRow = 'No se puede borrar una l�nea vac�a';
  cSCannotUseCommit = 'No se puede usar COMMIT en modo AUTOCOMMIT';
  cSCannotUseRollBack = 'No se puede usar ROLLBACK en modo AUTOCOMMIT';
  cSCanNotUpdateComplexQuery = 'No se puede actualizar una consulta compleja que haga referencia a m�s de una tabla';
  cSCanNotUpdateThisQueryType = 'No se puede actualizar este tipo de consulta';
  cSDriverWasNotFound = 'No se encontr� el controlador de base de datos solicitado';
  cSCanNotConnectToServer = 'No puede conectarse al servidor SQL';
  cSTableIsNotSpecified = 'La Tabla no est� especificada';
  cSLiveResultSetsAreNotSupported = 'La consulta actualizable no es soportada por esta clase';
  cSInvalidInputParameterCount = 'El n�mero de par�metros de tipo Input es menor al esperado';
  cSIsolationIsNotSupported = 'Nivel de aislamiento de transacci�n no soportado';
  cSColumnWasNotFound = 'Columna con nombre "%s" no encontrada';
  cSWrongTypeForBlobParameter = 'Tipo incorrecto para el par�metro Blob';
  cSIncorrectConnectionURL = 'URL de conexi�n incorrecta: %s';
  cSUnsupportedProtocol = 'Protocolo no soportado: %s';
  cSUnsupportedByDriver    = 'Translate: Driver can not support this feature natively: [%s]';

  cSConnectionIsNotOpened = 'La conexi�n no ha sido abierta todav�a';
  cSInvalidOpInAutoCommit = 'Operaci�n inv�lida en modo AutoCommit';
  cSInvalidOpInNonAutoCommit = 'Operaci�n inv�lida en modo No-AutoCommit';
  cSInvalidOpPrepare = 'Translate : Prepare transaction only possible on matching first(!) Starttransaction';

  cSConnectionIsNotAssigned = 'El componente de conexi�n a base de datos no est� asigando';
  cSQueryIsEmpty = 'La Consulta SQL est� vac�a';
  cSCanNotExecuteMoreQueries = 'No se puede ejecutar m�s de una consulta';
  cSOperationIsNotAllowed1 = 'Operaci�n no permitida en modo FORWARD ONLY';
  cSOperationIsNotAllowed2 = 'Operaci�n no permitida en modo READ ONLY (Solo lectura)';
  cSOperationIsNotAllowed3 = 'Operaci�n no permitida en modo %s';
  cSOperationIsNotAllowed4 = 'Operaci�n no permitida en un dataset cerrado';
  cSNoMoreRecords = 'No hay m�s registros en el Resultset';
  cSCanNotOpenResultSet = 'No se puede abrir el Resultset';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'Datasource hace una referencia c�clica';
  cSBookmarkWasNotFound = 'Bookmark no encontrado';
  cSIncorrectSearchFieldsNumber = 'N�mero incorrecto de valores de b�squeda';
  cSInvalidOperationInTrans = 'Operaci�n inv�lida en modo de transacci�n expl�cita';
  cSIncorrectSymbol = 'S�mbolo incorrecto en la lista de campos "%s".';
  cSIncorrectToken = 'Token incorrecto seguido de ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'El Nivel seleccionado de aislamiento de transacci�n no est� soportado';
  cSDriverNotSupported = 'Controlador %s no soportado';
  cSPattern2Long = 'Patr�n de b�squeda demasiado largo';
  cSDriverNotCapableOutParameters = 'El controlador no tiene cualidades para manejar par�metros';
  cSStatementIsNotAllowed = 'Sentencia no permitida';
  cSStoredProcIsNotAllowed = 'El procedimiento alamacenado no est� permitido';
  cSCannotPerformOperation = 'No se puede efectuar la operaci�n en un resultset cerrado';
  cSInvalidState = 'Estado Inv�lido';
  cSErrorConvertion = 'Error de conversi�n';
  cSDataTypeDoesNotSupported = 'Tipo de datos no soportado';
  cSUnsupportedParameterType = 'Tipo de par�metro no soportado';
  cSUnsupportedDataType = 'Tipo de datos no soportado';
  cSErrorConvertionField = 'Error de conversi�n del campo "%s" al Tipo SQL "%s"';
  cSBadOCI = 'Versi�n de OCI [%s] no aceptable. Se requiere versi�n 8.0.3 o menor';
  cSConnect2AsUser = 'Conectando a "%s" como usuario "%s"';
  cSUnknownError = 'Error desconocido';
  cSFieldNotFound1 = 'Campo "%s" no encontrado';
  cSFieldNotFound2 = 'Campo %d no encontrado';

  cSLoginPromptFailure = 'Cuadro de Di�logo por omisi�n para autenticaci�n no encontrado.'+#10#13+
                        'Por favor agregue la unidad DBLogDlg a la secci�n uses de la unidad principal de su proyecto.';

  cSPropertyQuery = '�La Consulta puede tardar un poco en bases de datos extensas!';
  cSPropertyTables = 'Deber�a limitarlas mediante Catalog y/o Schema.';
  cSPropertyColumns = 'Deber�a limitarlas mediante Catalog, Schema y/o TableName.';
  cSPropertyProcedures = 'Deber�a limitarlos mediante Catalog y/or Schema.';
  cSPropertySequences = 'Deber�a limitarlos mediante Catalog y/or Schema.';
  cSPropertyExecute = '�Desea ejecutar la consulta de todos modos?';

  cSFormTest = 'Prueba del Editor ZEOS SQL';
  cSButtonClose = '&Cerrar';
  cSFormEditor = 'Editor ZEOS SQL';
  cSTabSheetSelect = 'Seleccionar SQL';
  cSMenuLoad = 'Cargar...';
  cSMenuSave = 'Guardar...';
  cSButtonGenerate = '&Generar';
  cSButtonCheck = 'C&hecar';
  cSButtonTest = 'Pro&bar';
  cSButtonOk = '&Aceptar';
  cSButtonCancel = '&Cancelar';
  cSTableAlias = 'A&lias de la tabla';
  cSReplaceSQL = '&Reemplazar SQL';
  cSDialogOpenTitle = 'Abrir archivo SQL';
  cSDialogSaveTitle = 'Guardar archivo SQL';
  cSSQLEditor = 'Editor SQL';
  cSDatabaseDialog = 'Abrir base de datos existente';

  cSUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  cSUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  cSNotEditing = 'El Dataset no se encuentra en modo de edici�n o inserci�n';
  cSFieldTypeMismatch = 'El Tipo de dato no coincide para el campo ''%s'', se espera: %s, actual: %s';
  cSFieldSizeMismatch = 'El Tama�o de dato no coincide para el campo ''%s'', se espera: %d, actual: %d';
  {$ENDIF}
  cSNeedField               = 'Translate: Field %s is required, but not supplied.';

  cSFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  cSFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  cSFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  cSAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  cSBindingFailure         = 'Translate: Failed to bind parameter set';
  cSPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  cSBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  cSBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  cSFailedToBindResults    = 'Translate: Application failed to bind to the result set';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  cSMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  cSUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';
  cSFieldReadOnly          = 'Translate : Readonly field can''t be assigned a value: %d';
  cSInvalidUpdateCount     = 'Translate : %d record(s) updated. Only one record should have been updated.';

  cSRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}

{$IFDEF ROMANA}

  SSQLError1 = 'SQL Eroare: %s';
  cSSQLError2 = 'SQL Eroare: %s Cod: %d';
  cSSQLError3 = 'SQL Eroare: %s Cod: %d SQL: %s';
  cSSQLError4 = 'SQL Eroare: %s Cod: %d Mesaj: %s';

  cSListCapacityError = 'Capacitatea listei este �n afara limitelor (%d)';
  cSListCountError = 'Contorul listei este �n afara limitelor (%d)';
  cSListIndexError = 'Indexul listei este �n afara limitelor (%d)';

  cSClonningIsNotSupported = 'Clonning nu este suportat de aceast� clas�';
  cSImmutableOpIsNotAllowed = 'Opera�ia nu este permis� ori colec�ia nu este modificabil�';
  cSStackIsEmpty = 'Stiva este goal�';
  cSVariableWasNotFound = 'Variabila "%s" nu a fost g�sit�';
  cSFunctionWasNotFound = 'Func�ia "%s" nu a fost g�sit�';
  cSInternalError = 'Eroare Intern�';
  cSSyntaxErrorNear = 'Eroare de sintax� l�ng� "%s"';
  cSSyntaxError = 'Eroare de sintax�';
  cSUnknownSymbol = 'Simbol necunoscut "%s"';
  cSUnexpectedExprEnd = 'Final nea�teptat pentru expresie';
  cSRightBraceExpected = ') a�teptat';
  cSParametersError = 'parametrul %d a fost a�teptat dar %d a fost g�sit';
  cSExpectedMoreParams = 'Mai nult de doi parametrii sunt a�tepta�i';
  cSInvalidVarByteArray = 'Arie VarByte invalid�';
  cSVariableAlreadyExists = 'Variabila "%s" deja exist�';
  cSTypesMismatch = 'Tip nepotrivit';
  cSUnsupportedVariantType = 'Tip variant neasteptat';
  cSUnsupportedOperation = 'Opera�ie nesuportat�';

  cSTokenizerIsNotDefined = 'Simbolistica nu este definit�';
  cSLibraryNotFound = 'None of the dynamic libraries can be found: %s';
  cSEncodeDateIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Nu pot returna  Resultset data';
  cSRowBufferIsNotAssigned = 'Row buffer nu este asignat';
  cSColumnIsNotAccessable = 'Column with index %d nu este accesibil';
  cSConvertionIsNotPossible = 'Conversia nu este posibil� pentru coloana %d din %s �n %s';
  cSCanNotAccessBlobRecord = 'Nu pot aceesa �nregistrarea blob �n coloana %d cu tipul %s';
  cSRowDataIsNotAvailable = 'Row data nu este disponibil';
  cSResolverIsNotSpecified = 'Resolver nu este specificat pentru acest ResultSet';
  cSResultsetIsAlreadyOpened = 'Resultset este deja deschis�';
  cSCanNotUpdateEmptyRow = 'Nu pot updata o �nregistrare goal�';
  cSCanNotUpdateDeletedRow = 'Nu pot updata o �nregistrare �tears�';
  cSCanNotDeleteEmptyRow = 'Nu pot �terge o �nregistrare goal�';
  cSCannotUseCommit = 'Nu po�i folosi COMMIT �n modul AUTOCOMMIT ';
  cSCannotUseRollBack = 'Nu po�i folosi ROLLBACK �n modul AUTOCOMMIT ';
  cSCanNotUpdateComplexQuery = 'Nu pot updata un query complex cu mai mult de un tabel';
  cSCanNotUpdateThisQueryType = 'Nu pot updata acest tip de query';
  cSDriverWasNotFound = 'Driverul pentru baza de date nu a fost g�sit';
  cSCanNotConnectToServer = 'Nu ma pot conecta la serverul SQL';
  cSTableIsNotSpecified = 'Tbelul nu este specificat';
  cSLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  cSInvalidInputParameterCount = 'Input parameter count is less then expected';
  cSIsolationIsNotSupported = 'Transaction isolation level nu este suportat';
  cSColumnWasNotFound = 'Coloana cu numele "%s" nu a fost f�sit�';
  cSWrongTypeForBlobParameter = 'Tip gre�it pentru parametru Blob';
  cSIncorrectConnectionURL = 'Conexiune URL incorect�: %s';
  cSUnsupportedProtocol = 'Protocol nesuportat: %s';
  cSUnsupportedByDriver    = 'Driver nu poate suporta aceast� facilitate : [%s]';

  cSConnectionIsNotOpened = 'Conexiune nu este deschis� inc�';
  cSInvalidOpInAutoCommit = 'Opera�ie invalid� �n modul AutoCommit';
  cSInvalidOpInNonAutoCommit = 'Opera�ie invalid� �n modul non AutoCommit ';
  cSInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

  cSConnectionIsNotAssigned = 'Nu este asignat� o component� Database connection';
  cSQueryIsEmpty = 'SQL Query este gol';
  cSCanNotExecuteMoreQueries = 'Nu pot executa mai mult de un query';
  cSOperationIsNotAllowed1 = 'Opera�ia nu este permis� �n modul FORWARD ONLY ';
  cSOperationIsNotAllowed2 = 'Opera�ia nu este permis� �n modul READ ONLY';
  cSOperationIsNotAllowed3 = 'Opera�ia nu este permis� �n modul %s ';
  cSOperationIsNotAllowed4 = 'Opera�ia nu este permis� pentru �n dataset �nchis';
  cSNoMoreRecords = 'Nu mai sunt �nregistr�ri �n Resultset';
  cSCanNotOpenResultSet = 'Nu pot deschide Resultset';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'Datasource makes a circular link';
  cSBookmarkWasNotFound = 'Bookmark nu a fost g�sit';
  cSIncorrectSearchFieldsNumber = 'Num�r incorect of search field values';
  cSInvalidOperationInTrans = 'Opera�ie invalid� �n modul explicit transaction';
  cSIncorrectSymbol = 'Simbol incorect �n lista de c�mpuri  "%s".';
  cSIncorrectToken = 'Incorect token dup� ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  cSDriverNotSupported = 'Driver nesuportat %s';
  cSPattern2Long = 'Pattern is too long';
  cSDriverNotCapableOutParameters = 'Driver nu este capabil s� m�nuie parametrii';
  cSStatementIsNotAllowed = 'Statement nu sunt permise';
  cSStoredProcIsNotAllowed = 'The stored proc nu sunt permise';
  cSCannotPerformOperation = 'Nu se pot face opera�ii cu Resultset �nchis';
  cSInvalidState = 'Stare invalid�';
  cSErrorConvertion = 'Eroare de conversie';
  cSDataTypeDoesNotSupported = 'Tip de dat� nesuportat';
  cSUnsupportedParameterType = 'Tip parametru nesuportat';
  cSUnsupportedDataType = 'Tip dat� nesuportat';
  cSErrorConvertionField = 'Eroare de conversie pentru c�mpul "%s" �n TipSQL "%s"';
  cSBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  cSConnect2AsUser = 'Conectare la "%s" ca utlizator "%s"';
  cSUnknownError = 'Eroare necunoscut�';
  cSFieldNotFound1 = 'C�mpul "%s" nu a fost g�sit';
  cSFieldNotFound2 = 'C�mpul %d nu a fost g�sit';

  cSLoginPromptFailure = 'Nu g�sesc fereastra de dialog implicit� pentru login. V� rog ad�uga�i DBLogDlg �n sec�iunea uses.';

  cSPropertyQuery = 'The Query may last a while on large databases!';
  cSPropertyTables = 'You should limit it by Catalog and/or Schema.';
  cSPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  cSPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  cSPropertySequences = 'You should limit it by Catalog and/or Schema.';
  cSPropertyExecute = 'Query va fi executat� oricum?';

  cSFormTest = 'ZEOS SQL Editor Test';
  cSButtonClose = '�n&chide';
  cSFormEditor = 'ZEOS SQL Editor';
  cSTabSheetSelect = 'Select SQL';
  cSMenuLoad = 'Deschide';
  cSMenuSave = 'Salvare';
  cSButtonGenerate = '&Generare';
  cSButtonCheck = 'Verificare';
  cSButtonTest = '&Test';
  cSButtonOk = '&OK';
  cSButtonCancel = 'Revo&care';
  cSTableAlias = 'T&able alias';
  cSReplaceSQL = '&Replace SQL';
  cSDialogOpenTitle = 'Deschide Fi�ier SQL';
  cSDialogSaveTitle = 'Salveaz� Fi�ier SQL';
  cSSQLEditor = 'SQL Editor';
  cSDatabaseDialog = 'Deschide baz� date existent�';

  cSUpdateSQLNoResult = '"Update Refresh SQL" furnizat nu este un recordset';
  cSUpdateSQLRefreshStatementcount ='Declara�ia "Update Refresh SQL" ca num�r trebuie s� fie una';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset nu este �n modul de editare sau inserare';
  cSFieldTypeMismatch = 'Tip nepotrivit pentru c�mpul ''%s'', a�teptat: %s actual: %s';
  cSFieldSizeMismatch = 'Dimensiune nepotrivit� pentru c�mpul  ''%s'', a�teptat: %d actual: %d';
  {$ENDIF}
  cSNeedField               = 'Translate: Field %s is required, but not supplied.';

  cSFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  cSFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  cSFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  cSAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  cSBindingFailure         = 'Translate: Failed to bind parameter set';
  cSPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  cSBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  cSBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  cSFailedToBindResults    = 'Translate: Application failed to bind to the result set';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  cSMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  cSUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

  cSRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
  // <-- added by tohenk
  {$ELSE}
  {$IFDEF INDONESIAN}
  cSSQLError1 = 'Kesalahan SQL: %s';
  cSSQLError2 = 'Kesalahan SQL: %s Kode: %d';
  cSSQLError3 = 'Kesalahan SQL: %s Kode: %d SQL: %s';
  cSSQLError4 = 'Kesalahan SQL: %s Kode: %d Pesan: %s';

  cSListCapacityError = 'Kapasitas List diluar jangkauan (%d)';
  cSListCountError = 'Jumlah List diluar jangkauan (%d)';
  cSListIndexError = 'Indeks List diluar jangkauan (%d)';

  cSClonningIsNotSupported = 'Class ini tidak mendukung kloning';
  cSImmutableOpIsNotAllowed = 'Operasi tidak diperkenankan pada koleksi yang tidak dapat diubah';
  cSStackIsEmpty = 'Stack kosong';
  cSVariableWasNotFound = 'Variabel "%s" tidak ada';
  cSFunctionWasNotFound = 'Fungsi "%s" tidak ada';
  cSInternalError = 'Kesalahan internal';
  cSSyntaxErrorNear = 'Kesalahan Syntax di dekat "%s"';
  cSSyntaxError = 'Kesalahan Syntax';
  cSUnknownSymbol = 'Simbol tidak dikenali "%s"';
  cSUnexpectedExprEnd = 'Tidak dibutuhkan, akhir dari ekspresi';
  cSRightBraceExpected = ') dibutuhkan';
  cSParametersError = '%d parameter dibutuhkan tapi terdapat %d parameter';
  cSExpectedMoreParams = 'Dibutuhkan lebih dari dua parameter';
  cSInvalidVarByteArray = 'array VarByte tidak valid';
  cSVariableAlreadyExists = 'Variabel "%s" sudah ada';
  cSTypesMismatch = 'Tipe tidak sesuai';
  cSUnsupportedVariantType = 'Tipe variant tidak didukung';
  cSUnsupportedOperation = 'Operasi tidak didukung';

  cSTokenizerIsNotDefined = 'Tokenizer belum ditentukan';
  cSLibraryNotFound = 'Tidak ada library ditemukan: %s';
  cSEncodeDateIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Tidak dapat mengambil data Resultset';
  cSRowBufferIsNotAssigned = 'Row buffer tidak disediakan';
  cSColumnIsNotAccessable = 'Kolom dengan indeks %d tidak dapat diakses';
  cSConvertionIsNotPossible = 'Konversi tidak dimungkinkan pada kolom %d dari %s ke %s';
  cSCanNotAccessBlobRecord = 'Tidak dapat mengakses rekord `blob` pada kolom %d dengan tipe %s';
  cSRowDataIsNotAvailable = 'Data Row tidak tersedia';
  cSResolverIsNotSpecified = 'Resolver belum ditentukan pada ResultSet ini';
  cSResultsetIsAlreadyOpened = 'Resultset sudah terbuka';
  cSCanNotUpdateEmptyRow = 'Tidak dapat meng-update row kosong';
  cSCanNotUpdateDeletedRow = 'Tidak dapat meng-update row terhapus';
  cSCanNotDeleteEmptyRow = 'Tidak dapat meng-hapus row kosong';
  cSCannotUseCommit = 'COMMIT tidak dapat digunakan pada mode AUTOCOMMIT';
  cSCannotUseRollBack = 'ROLLBACK tidak dapat digunakan pada mode AUTOCOMMIT';
  cSCanNotUpdateComplexQuery = 'Tidak dapat meng-update query kompleks dengan lebih dari satu tabel';
  cSCanNotUpdateThisQueryType = 'Tidak dapat meng-update query dengan tipe ini';
  cSDriverWasNotFound = 'Driver database yang diminta tidak ada';
  cSCanNotConnectToServer = 'Tidak dapat terhubung ke server SQL';
  cSTableIsNotSpecified = 'Tabel belum ditentukan';
  cSLiveResultSetsAreNotSupported = 'Live query tidak didukung oleh Class ini';
  cSInvalidInputParameterCount = 'Jumlah parameter Input kurang dari yang dibutuhkan';
  cSIsolationIsNotSupported = 'Level Isolasi Transaksi tidak didukung';
  cSColumnWasNotFound = 'Kolom dengan nama "%s" tidak ada';
  cSWrongTypeForBlobParameter = 'Salah tipe untuk parameter Blob';
  cSIncorrectConnectionURL = 'Salah koneksi URL: %s';
  cSUnsupportedProtocol = 'Protokol tidak didukung: %s';
  cSUnsupportedByDriver    = 'Driver tidak mendukung fitur: [%s]';

  cSConnectionIsNotOpened = 'Koneksi belum dibuka';
  cSInvalidOpInAutoCommit = 'Operasi tidak valid pada mode AUTOCOMMIT';
  cSInvalidOpInNonAutoCommit = 'Operasi tidak valid pada mode non AUTOCOMMIT';
  cSInvalidOpPrepare = 'Persiapan transaksi hanya mungkin pada (!) Starttransaction pertama';

  cSConnectionIsNotAssigned = 'Komponen koneksi Database tidak ditentukan';
  cSQueryIsEmpty = 'Query SQL kosong';
  cSCanNotExecuteMoreQueries = 'Tidak dapat meng-eksekusi lebih dari satu query';
  cSOperationIsNotAllowed1 = 'Operasi tidak diperkenankan pada mode FORWARD ONLY';
  cSOperationIsNotAllowed2 = 'Operasi tidak diperkenankan pada mode READ ONLY';
  cSOperationIsNotAllowed3 = 'Operasi tidak diperkenankan pada mode %s';
  cSOperationIsNotAllowed4 = 'Operasi tidak diperkenankan pada dataset tertutup';
  cSNoMoreRecords = 'Tidak ada rekord lagi pada Resultset';
  cSCanNotOpenResultSet = 'Tidak dapat membuka Resultset';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'Terjadi hubungan Datasource circular';
  cSBookmarkWasNotFound = 'Bookmark tidak ada';
  cSIncorrectSearchFieldsNumber = 'Salah jumlah nilai field pada pencarian';
  cSInvalidOperationInTrans = 'Operasi tidak valid pada mode explicit transaction';
  cSIncorrectSymbol = 'Simbol salah pada daftar field "%s".';
  cSIncorrectToken = 'Token salah setelah ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'Level Isolasi Transaksi terpilih tidak didukung';
  cSDriverNotSupported = 'Driver tidak mendukung %s';
  cSPattern2Long = 'Pola terlalu panjang';
  cSDriverNotCapableOutParameters = 'Driver tidak mampu menangani parameter';
  cSStatementIsNotAllowed = 'Statement tidak diperbolehkan';
  cSStoredProcIsNotAllowed = 'StoredProc tidak diperbolehkan';
  cSCannotPerformOperation = 'Tidak dapat melakukan operasi pada Resultset tertutup';
  cSInvalidState = 'Sate tidak valid';
  cSErrorConvertion = 'Kesalahan konversi';
  cSDataTypeDoesNotSupported = 'Tipe Data tidak didukung';
  cSUnsupportedParameterType = 'Tidak mendukung tipe parameter';
  cSUnsupportedDataType = 'Tidak mendukung tipe data';
  cSErrorConvertionField = 'Kesalahan konversi field "%s" ke Tipe SQL "%s"';
  cSBadOCI = 'OCI version [%s] tidak sah. Dibutuhkan versi 8.0.3 atau terdahulu';
  cSConnect2AsUser = 'Koneksi ke "%s" dengan user "%s"';
  cSUnknownError = 'Kesalahan tidak diketahui';
  cSFieldNotFound1 = 'Field "%s" tidak ada';
  cSFieldNotFound2 = 'Field %d tidak ada';

  cSLoginPromptFailure = 'Tidak ada dialog Login default. Silahkan tambahkan DBLogDlg ke klausula `uses` pada file utama.';

  cSPropertyQuery = 'Query mungkin berlangsung lama pada database besar!';
  cSPropertyTables = 'Batasi dengan Katalog data/atau Skema.';
  cSPropertyColumns = 'Batasi dengan Katalog, Skema dan/atau Nama Tabel.';
  cSPropertyProcedures = 'Batasi dengan Katalog dan/atau Skema.';
  cSPropertySequences = 'Batasi dengan Katalog dan/atau Skema.';
  cSPropertyExecute = 'Apakah Query jadi dieksekusi?';

  cSFormTest = 'Tes Editor SQLZEOS';
  cSButtonClose = '&Tutup';
  cSFormEditor = 'Editor SQL ZEOS';
  cSTabSheetSelect = 'SQL Select';
  cSMenuLoad = 'Ambil';
  cSMenuSave = 'Simpan';
  cSButtonGenerate = '&Generate';
  cSButtonCheck = '&Cek';
  cSButtonTest = 'T&es';
  cSButtonOk = '&OK';
  cSButtonCancel = '&Batal';
  cSTableAlias = 'Alias T&abel';
  cSReplaceSQL = 'SQL &Replace';
  cSDialogOpenTitle = 'Buka File SQL';
  cSDialogSaveTitle = 'Simpan File SQL';
  cSSQLEditor = 'Editor SQL';
  cSDatabaseDialog = 'Buka database yang tersedia';

  cSUpdateSQLNoResult = 'Tidak ada Resultset pada Update Refresh SQL';
  cSUpdateSQLRefreshStatementcount ='Jumlah Statement pada Update Refresh SQL harus 1';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset tidak dalam mode edit atau sisip';
  cSFieldTypeMismatch = 'Tipe tidak sesuai pada field ''%s'', seharusnya: %s aktual: %s';
  cSFieldSizeMismatch = 'Ukuran tidak sesuai pada field ''%s'', seharusnya: %d aktual: %d';
  {$ENDIF}
  cSNeedField               = 'Field %s diperlukan, namun tidak disediakan.';

  cSFailedtoInitPrepStmt   = 'Gagal inisialisasi Prepared statement';
  cSFailedtoPrepareStmt    = 'Statemen gagal sewaktu proses persiapan';
  cSFailedToBindAllValues  = 'Aplikasi gagal dalam penggabungan pendahuluan semua nilai';
  cSAttemptExecOnBadPrep   = 'Percobaan eksekusi statemen dilakukan sebelum persiapan berhasil.';
  cSBindingFailure         = 'Gagal menggabungkan parameter';
  cSPreparedStmtExecFailure = 'Prepared Statement gagal dieksekusi';
  cSBoundVarStrIndexMissing = 'Teks variabel indeks "%s" tidak ada';
  cSBindVarOutOfRange      = 'Variabel indeks diluar jangkauan: %d';
  cSFailedToBindResults    = 'Aplikasi gagal pada penggabungan ke Resultset';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = 'Metode RefreshRow hanya didukung oleh obyek Update';
  cSMustBeInBrowseMode = 'Operasi hanya diperbolehkan pada status dsBrowse';

  cSUnKnownParamDataType = 'Param.DataType tidak dikenal';
  cSFieldReadOnly          = 'Field readonly tidak dapat diberikan nilai: %d';
  cSInvalidUpdateCount     = '%d rekord terupdate. Seharusnya hanya satu rekord yang terupdate.';

  cSRowBufferWidthExceeded = 'Lebar buffer baris terlampaui. Coba kurangi atau gunakan kolom yang lebih panjang dalam query SQL.';
  // <--- end added by tohenk
  //--- begin added by ORMADA --------------------------------------------------
{$ELSE}
{$IFDEF RUSSIAN}
  cSSQLError1                               = '������ � SQL ���������: %s';
  cSSQLError2                               = '������ � SQL ���������: %s ��� ������: %d';
  cSSQLError3                               = '������ � SQL ���������: %s ��� ������: %d SQL: %s';
  cSSQLError4                               = '������ � SQL ���������: %s ��� ������: %d ���������: %s';

  cSListCapacityError                       = '������ ������ ����� �� ������� (%d)';
  cSListCountError                          = '������� ������ ����� �� ������� (%d)';
  cSListIndexError                          = '������ ������ ����� �� ������� (%d)';

  cSClonningIsNotSupported                  = '������ ����� �� ������������ ������������';
  cSImmutableOpIsNotAllowed                 = '�������� �� �������������� �� ���������� ����������';
  cSStackIsEmpty                            = '���� ����';
  cSVariableWasNotFound                     = '�������� "%s" �� �������';
  cSFunctionWasNotFound                     = '������� "%s" �� �������';
  cSInternalError                           = '��������� ������';
  cSSyntaxErrorNear                         = '������ � ���������� "%s"';
  cSSyntaxError                             = '������ � ����������';
  cSUnknownSymbol                           = '����������� ������ "%s"';
  cSUnexpectedExprEnd                       = '����������� ����� ���������';
  cSRightBraceExpected                      = ') ���������';
  cSParametersError                         = '��������� %d ����������, ������� %d';
  cSExpectedMoreParams                      = '��������� ����� 2-� ����������';
  cSInvalidVarByteArray                     = '�������� ������ (VarByte)';
  cSVariableAlreadyExists                   = '�������� "%s" ��� ����������';
  cSTypesMismatch                           = '������������ �����';
  cSUnsupportedVariantType                  = '���������������� ���������� (variant) ���';
  cSUnsupportedOperation                    = '���������������� ��������';

  cSTokenizerIsNotDefined                   = '����� �� ����������';
  cSLibraryNotFound                         = '�� ����� ������������ ���������� �� �������: %s';
  cSEncodeDateIsNotSupported                = '��� ������ �� ������������ isc_encode_sql_date';
  cSEncodeTimeIsNotSupported                = '��� ������ �� ������������ isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported           = '��� ������ �� ������������ isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported                = '��� ������ �� ������������ isc_decode_sql_date';
  cSDecodeTimeIsNotSupported                = '��� ������ �� ������������ isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported           = '��� ������ �� ������������ isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData             = '���������� �������� ����� ������ (Resultset)';
  cSRowBufferIsNotAssigned                  = '�� �������� ������ ������';
  cSColumnIsNotAccessable                   = '���������� ������� � �������� %d';
  cSConvertionIsNotPossible                 = '����������� ���������� ��� ������� %d � %s �� %s';
  cSCanNotAccessBlobRecord                  = '���������� �������� ������ � blob ������ � ������� %d � ����� %s';
  cSRowDataIsNotAvailable                   = '���������� ������ ������';
  cSResolverIsNotSpecified                  = '��� ������� ������ ������ (ResultSet) �� ����� Resolver';
  cSResultsetIsAlreadyOpened                = '����� ������ (Resultset) ��� ������';
  cSCanNotUpdateEmptyRow                    = '���������� �������� ������ ������';
  cSCanNotUpdateDeletedRow                  = '���������� �������� �������� ������';
  cSCanNotDeleteEmptyRow                    = '���������� ������� ������ ������';
  cSCannotUseCommit                         = '���������� ������������ COMMIT � AUTOCOMMIT ������';
  cSCannotUseRollBack                       = '���������� ������������ ROLLBACK � AUTOCOMMIT ������';
  cSCanNotUpdateComplexQuery                = '���������� �������� ����������� ������ � ����� ��� ����� ��������';
  cSCanNotUpdateThisQueryType               = '���������� �������� ���� ��� �������';
  cSDriverWasNotFound                       = '��������� ������� �� �� ������';
  cSCanNotConnectToServer                   = '���������� ������������ � SQL �������';
  cSTableIsNotSpecified                     = '������� �� ������';
  cSLiveResultSetsAreNotSupported           = '����� ����� ������ �� �������������� ���� �������';
  cSInvalidInputParameterCount              = '���������� ������� ���������� is ������ ��� ���������';
  cSIsolationIsNotSupported                 = '������� �������� ����������� �� ��������������';
  cSColumnWasNotFound                       = '�� ������ ������� � ������ "%s"';
  cSWrongTypeForBlobParameter               = '�������� ��� ��� Blob ����������';
  cSIncorrectConnectionURL                  = '�������� ���� (URL) ��� �����������: %s';
  cSUnsupportedProtocol                     = '���������������� ��������: %s';
  cSUnsupportedByDriver                     = '������� �� ������������ ������ ����������� : [%s]';

  cSConnectionIsNotOpened                   = '����������� �� �������';
  cSInvalidOpInAutoCommit                   = '�������� �������� � ������ ����������������� (AutoCommit)';
  cSInvalidOpInNonAutoCommit                = '�������� �������� � ������ �� ����������������� (non AutoCommit)';
  cSInvalidOpPrepare                        = '���������� ����������� �������� ������ ��� ������ �������������(!) StartTransaction';

  cSConnectionIsNotAssigned                 = '����������� � �� �� ������';
  cSQueryIsEmpty                            = 'SQL ������ ����';
  cSCanNotExecuteMoreQueries                = '���������� ��������� ����� ������ �������';
  cSOperationIsNotAllowed1                  = '�������� �� �������������� � ������ ������ ����� (FORWARD ONLY)';
  cSOperationIsNotAllowed2                  = '�������� �� �������������� � ������ ������ ��� ������ (READ ONLY)';
  cSOperationIsNotAllowed3                  = '�������� �� �������������� � %s ������';
  cSOperationIsNotAllowed4                  = '�������� �� �������������� �� �������� ������ ������';
  cSNoMoreRecords                           = '� ������ ������ (Resultset) ��� �������';
  cSCanNotOpenResultSet                     = '���������� ������� ����� ������ (Resultset)';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink                            = '�������� ������ (Datasource) ����� ����������� ������';
  cSBookmarkWasNotFound                     = '������� (Bookmark) �� �������';
  cSIncorrectSearchFieldsNumber             = '������������ �����  Incorrect number of search field values';
  cSInvalidOperationInTrans                 = '�������� �������� � ������ �����������';
  cSIncorrectSymbol                         = '�������� ������ � ������ ����� "%s".';
  cSIncorrectToken                          = '�������� ���� ����� ":"';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation            = '��������� ������� �������� ����������� �� ��������������';
  cSDriverNotSupported                      = '������� �� �������������� %s';
  cSPattern2Long                            = '������� ������� �������';
  cSDriverNotCapableOutParameters           = '������� �� �������� ��������� �����������';
  cSStatementIsNotAllowed                   = '��������� �� ��������������';
  cSStoredProcIsNotAllowed                  = '�������� ��������� �� ���������';
  cSCannotPerformOperation                  = '���������� ��������� �������� �� �������� ������ ������ (Resultset)';
  cSInvalidState                            = '�������� ���������';
  cSErrorConvertion                         = '������ ��������������';
  cSDataTypeDoesNotSupported                = '��� ������ �� ��������������';
  cSUnsupportedParameterType                = '���������������� ��� ���������';
  cSUnsupportedDataType                     = '���������������� ��� ������';
  cSErrorConvertionField                    = '������ ����������� ��� ���� "%s" � SQLType "%s"';
  cSBadOCI                                  = '�������� ������ OCI [%s]. ����������� ������ 8.0.3 ��� ����';
  cSConnect2AsUser                          = '���������� ������������ � "%s" ������������� "%s"';
  cSUnknownError                            = '����������� ������';
  cSFieldNotFound1                          = '���� "%s" �� �������';
  cSFieldNotFound2                          = '���� %d �� �������';

  cSLoginPromptFailure                      = '���������� ����� ������ ����������� �� ����������. �������� ������ DBLogDlg � ������ uses �������� ������������ ������.';

  cSPropertyQuery                           = '��� ����� ���� ��������� ������ ���� �� ������� The Query may last a while on large databases!';
  cSPropertyTables                          = '������� ���������� ���������(Catalog) �/��� ������ (Schema)';
  cSPropertyColumns                         = '������� ���������� ��������� (Catalog), ������ (Schema) �/��� �������� (TableName).';
  cSPropertyProcedures                      = '������� ���������� ���������(Catalog) �/��� ������ (Schema).';
  cSPropertySequences                       = '������� ���������� ���������(Catalog) �/��� ������ (Schema).';
  cSPropertyExecute                         = '�� ����� ��������� ������ ?';

  cSFormTest                                = 'ZEOS SQL ���� ���������';
  cSButtonClose                             = '&�������';
  cSFormEditor                              = 'ZEOS SQL ��������';
  cSTabSheetSelect                          = '����� SQL';
  cSMenuLoad                                = '���������';
  cSMenuSave                                = '���������';
  cSButtonGenerate                          = '&�������������';
  cSButtonCheck                             = '�&��������';
  cSButtonTest                              = '&����';
  cSButtonOk                                = '&��';
  cSButtonCancel                            = '&������';
  cSTableAlias                              = '�&�������� �������';
  cSReplaceSQL                              = '&�������� SQL';
  cSDialogOpenTitle                         = '������� SQL ����';
  cSDialogSaveTitle                         = '��������� SQL ����';
  cSSQLEditor                               = 'SQL ��������';
  cSDatabaseDialog                          = '������� ������������ ��';

  cSUpdateSQLNoResult                       = '� ���������� ���������� (Refresh) ������ �� ��������';
  cSUpdateSQLRefreshStatementcount          = 'Refresh ������ ������ ���� ������ ����';

{$IFDEF FPC}
  cSNotEditing                              = '����� ������ (Dataset) �� � ������ �������������� ��� �������';
  cSFieldTypeMismatch                       = '������������ ���� ��� ���� ''%s'', ��������� %s ������: %s';
  cSFieldSizeMismatch                       = '������ ���� ''%s'' �� ���������, ���������: %d ������: %d';
{$ENDIF}
  cSNeedField               = 'Translate: Field %s is required, but not supplied.';

  cSFailedtoInitPrepStmt                    = '��������� ���������������� �������������� ���������';
  cSFailedtoPrepareStmt                     = '������ ���������� ��������� � �������� ����������';
  cSFailedToBindAllValues                   = '������ ��� ���-����������� ��������';
  cSAttemptExecOnBadPrep                    = '������� ��������� ��������� �� �������� ����������.';
  cSBindingFailure                          = '������ ��� ���������� ���������';
  cSPreparedStmtExecFailure                 = '��������� ��������� �������������� ���������';
  cSBoundVarStrIndexMissing                 = '����������� �� ����� � �������� "%s" �� ����������';
  cSBindVarOutOfRange                       = '������ ����������� ����� �� ������� : %d';
  cSFailedToBindResults                     = '��������� �������(bind) ��������� ����������';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

  cSRefreshRowOnlySupportedWithUpdateObject = '����� ���������� ������ (RefreshRow) �������������� ������ ��� ���������� �������';
  cSMustBeInBrowseMode                      = '�������� ������������ ������ � ������ ��������� (dsBROWSE)';

  cSUnKnownParamDataType                    = '����������� ���� ��������� (Param.DataType)';
  //--- end added by ORMADA ----------------------------------------------------
  cSFieldReadOnly          = 'Translate : Readonly field can''t be assigned a value: %d';
  cSInvalidUpdateCount     = 'Translate : %d record(s) updated. Only one record should have been updated.';

  cSRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}

//--- added by Petr Stasiak - pestasoft.com ------------------------------------
{$IFDEF CZECH}
  cSSQLError1 = 'SQL chyba: %s';
  cSSQLError2 = 'SQL chyba: %s k�d: %d';
  cSSQLError3 = 'SQL chyba: %s k�d: %d SQL: %s';
  cSSQLError4 = 'SQL chyba: %s k�d: %d Hl�en�: %s';

  cSListCapacityError = 'Kapacita seznamu je mimo rozsah (%d)';
  cSListCountError = 'Po�et seznam� je mimo rozsah (%d)';
  cSListIndexError = 'Index v seznamu je mimo rozsah (%d)';

  cSClonningIsNotSupported = 'Klonov�n� nen� v t�to t��d� podporov�no';
  cSImmutableOpIsNotAllowed = 'Tato operace nen� povolena na nem�niteln� "collections"';
  cSStackIsEmpty = 'Z�sobn�k je pr�zdn�';
  cSVariableWasNotFound = 'Prom�n� "%s" neexistuje';
  cSFunctionWasNotFound = 'Funkce "%s" neexistuje';
  cSInternalError = 'Intern� chyba';
  cSSyntaxErrorNear = 'Chybn� syntaxe "%s"';
  cSSyntaxError = 'Chybn� syntaxe';
  cSUnknownSymbol = 'Nezn�m� symbol "%s"';
  cSUnexpectedExprEnd = 'Neo�ek�van� konec v�razu';
  cSRightBraceExpected = ') o�ek�v�n(o/a/y)';
  cSParametersError = '%d parametr� o�ek�v�no, ale %d existuje';
  cSExpectedMoreParams = 'Je o�ek�v�no v�ce, ne� 2 parametry';
  cSInvalidVarByteArray = 'Nespr�vn� VarByte array';
  cSVariableAlreadyExists = 'Prom�n� "%s" ji� existuje';
  cSTypesMismatch = 'Nesouhlasn� typy';
  cSUnsupportedVariantType = 'Nepodporovan� typ variant';
  cSUnsupportedOperation = 'Nepodporovan� operace';

  cSTokenizerIsNotDefined = 'Nen� definov�n "Tokenizer"';
  cSLibraryNotFound = 'Neexistuje dll knihovna(y): %s';
  cSEncodeDateIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Nelze z�skat data "Resultset"';
  cSRowBufferIsNotAssigned = 'Nen� p�i�azen ��dkov� buffer';
  cSColumnIsNotAccessable = 'Sloupec s indexem %d nen� p��stupn�';
  cSConvertionIsNotPossible = 'P�evod sloupce %d  nen� mo�n� z %s na %s';
  cSCanNotAccessBlobRecord = 'Nelze p�istupovat k blob z�znamu ze zloupce %d p�es typ %s';
  cSRowDataIsNotAvailable = '��dkov� data nejsou p��stupn�';
  cSResolverIsNotSpecified = 'Nen� specifikov�n "rozklada�" pro tento v�sledek';
  cSResultsetIsAlreadyOpened = '"Resultset" byl ji� otev�en';
  cSCanNotUpdateEmptyRow = 'Nelze aktualizovat pr�zdn� ��dek';
  cSCanNotUpdateDeletedRow = 'Nelze aktualizovat smazan� ��dek';
  cSCanNotDeleteEmptyRow = 'Nelze vymazat pr�zdn� ��dek';
  cSCannotUseCommit = 'Nepou��vejte COMMIT v m�du AUTOCOMMIT';
  cSCannotUseRollBack = 'Nelze pou��t ROLLBACK v AUTOCOMMIT m�du';
  cSCanNotUpdateComplexQuery = 'Nelze aktualizovat komplexn� dotaz pro v�ce, ne� jednu tabulku';
  cSCanNotUpdateThisQueryType = 'Nelze aktualizovat tento typ dotazu';
  cSDriverWasNotFound = 'Po�adovan� datab�zov� ovlada� nenalezen';
  cSCanNotConnectToServer = 'Nezda�ilo se p�ipojen� k SQL serveru';
  cSTableIsNotSpecified = 'Tabulka nen� specifikov�na';
  cSLiveResultSetsAreNotSupported = '"�iv�" dotaz nen� podporov�n v t�to t��d�';
  cSInvalidInputParameterCount = 'Po�et vstupn�ch parametr� neodpov�d� o�ek�van�mu po�tu';
  cSIsolationIsNotSupported = 'M�ra izolace transakce nen� podporov�na';
  cSColumnWasNotFound = 'Sloupec s n�zvem "%s" neexistuje';
  cSWrongTypeForBlobParameter = 'Nespr�vn� typ pro Blob parametr';
  cSIncorrectConnectionURL = 'Nespr�vn� tvar URL adresy: %s';
  cSUnsupportedProtocol = 'Nepodporovan� protokol: %s';
  cSUnsupportedByDriver    = 'Ovlada� nepodporuje tuto vlastnost: [%s]';

  cSConnectionIsNotOpened = 'Spojen� nen� otev�eno';
  cSInvalidOpInAutoCommit = 'Nespr�vn� operace v m�du AutoCommit';
  cSInvalidOpInNonAutoCommit = 'Nespr�vn� operace v m�du NE AutoCommit';
  cSInvalidOpPrepare = '"Prepare" transakce je mo�n� pouze jako prvn�! Starttransaction';

  cSConnectionIsNotAssigned = 'Nen� p�i�azen komponent "connection"';
  cSQueryIsEmpty = 'SQL dotaz je pr�zdn�';
  cSCanNotExecuteMoreQueries = 'Nelze spustit v�ce, ne� 1 dotaz';
  cSOperationIsNotAllowed1 = 'Operace nen� povolena v m�du "FORWARD ONLY"';
  cSOperationIsNotAllowed2 = 'Operace nen� povolena v m�du "READ ONLY"';
  cSOperationIsNotAllowed3 = 'Operace nen� povolena v m�du "%s"';
  cSOperationIsNotAllowed4 = 'Operace nen� povolena pro zav�en� zdroj dat (dataset)';
  cSNoMoreRecords = 'Nejsou dal�� z�znamy';
  cSCanNotOpenResultSet = 'Nelze otev��t v�sledek dotazu';
  cSCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'Datasource vytv��� cyklick� dotaz';
  cSBookmarkWasNotFound = 'Z�lo�ka neexistuje';
  cSIncorrectSearchFieldsNumber = 'Nespr�vn� po�et vyhled�van�ch polo�ek';
  cSInvalidOperationInTrans = 'Nespr�vn� operace v explicitn�m transak�n�m m�du';
  cSIncorrectSymbol = 'Nespr�vn� symbol v seznamu polo�ek "%s".';
  cSIncorrectToken = 'Za ":" n�sleduje nespr�vn� znak';
  cSIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'Vybran� m�ra izolace transakc� nen� podporov�na';
  cSDriverNotSupported = 'Ovlada� %s nen� podporov�n';
  cSPattern2Long = 'Pattern je p��li� dlouh�';
  cSDriverNotCapableOutParameters = 'Ovlada� nen� schopen p�ij�mat parametry';
  cSStatementIsNotAllowed = 'P��kaz nen� povolen';
  cSStoredProcIsNotAllowed = '"stored proc" nen� povolena';
  cSCannotPerformOperation = 'Nelze prov�st operaci na uzav�en�m v�sledku dotazu (Resultset)';
  cSInvalidState = 'Nespr�vn� stav';
  cSErrorConvertion = 'Chyba p�evodu';
  cSDataTypeDoesNotSupported = 'Tento typ dat nen� podporov�n';
  cSUnsupportedParameterType = 'Nepodporovan� typ parametru';
  cSUnsupportedDataType = 'Nepodporovan� typ dat';
  cSErrorConvertionField = 'Chyba p�evodu sloupce "%s" na SQLTyp "%s"';
  cSBadOCI = '�patn� verze OCI [%s]. Je vy�adov�na 8.0.3 nebo star��';
  cSConnect2AsUser = 'P�ipojit k "%s" jako "%s"';
  cSUnknownError = 'Nezn�m� chyba';
  cSFieldNotFound1 = 'Sloupec "%s" neexistuje';
  cSFieldNotFound2 = 'Sloupec %d neexistuje';

  cSLoginPromptFailure = 'Nelze naj�t v�choz� p�ihla�ovac� dialog. Pros�m p�idejte DBLogDlg do sekce USES va�eho zdrojov�ho souboru.';

  cSPropertyQuery = 'Dotaz m��e b�t posledn� u vlelk�ch datab�z�!';
  cSPropertyTables = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  cSPropertyColumns = 'M�lo by b�t limitov�no katalogem, sch�matem a/nebo n�zvem tabulky.';
  cSPropertyProcedures = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  cSPropertySequences = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  cSPropertyExecute = 'M� se dotaz p�esto vykonat?';

  cSFormTest = 'ZEOS SQL Editor Test';
  cSButtonClose = '&Zav��t';
  cSFormEditor = 'ZEOS SQL Editor';
  cSTabSheetSelect = 'Select SQL';
  cSMenuLoad = 'Na��st';
  cSMenuSave = 'Ulo�it';
  cSButtonGenerate = '&Generovat';
  cSButtonCheck = '&Kontrola';
  cSButtonTest = '&Test';
  cSButtonOk = '&OK';
  cSButtonCancel = 'Z&ru�it';
  cSTableAlias = '&Alias tabulky';
  cSReplaceSQL = 'Nah&radit SQL';
  cSDialogOpenTitle = 'Otev��t SQL soubor';
  cSDialogSaveTitle = 'Ulo�it SQL soubor';
  cSSQLEditor = 'SQL Editor';
  cSDatabaseDialog = 'Otev��t existuj�c� datab�zi';

  cSUpdateSQLNoResult = 'Update Refresh SQL nevr�tilo ��dn� v�sledek';
  cSUpdateSQLRefreshStatementcount ='Po�et Update Refresh SQL p��kaz� mus� b�t 1';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset nen� v edita�n�m (edit), ani vkl�dac�m (insert) re�imu';
  cSFieldTypeMismatch = 'Nespr�vn� typ pro sloupec ''%s'', o�ek�v�no: %s aktu�ln�: %s';
  cSFieldSizeMismatch = 'Nespr�vn� velikost sloupce ''%s'', o�ek�v�no: %d aktu�ln�: %d';
  {$ENDIF}
  cSNeedField               = 'Sloupce %s je po�adov�n, ale nezad�n.';

  cSFailedtoInitPrepStmt   = 'P�ipravovan� p��kaz nelze inicializovat';
  cSFailedtoPrepareStmt    = 'P��kaz selhal b�hem p��pravy procesu';
  cSFailedToBindAllValues  = 'Aplikace zkolabovala p�ed p��pravou v�ech hodnot';
  cSAttemptExecOnBadPrep   = 'Pokou��te sespustit p��kaz p�ed dokon�en�m jeho p��pravy.';
  cSBindingFailure         = 'Chyba p�i z�sk�v�n� sady parametr�';
  cSPreparedStmtExecFailure = 'P�ipravovan� p��kaz selhal p�i vykon�v�n�';
  cSBoundVarStrIndexMissing = 'Index textov� prom�n� "%s" neexistuje';
  cSBindVarOutOfRange      = 'Index promen� je mimo rozsah: %d';
  cSFailedToBindResults    = 'Aplikace selhala p�i z�sk�v�n� v�sledk� dotazu';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

//FOS+ 07112006
  cSRefreshRowOnlySupportedWithUpdateObject = 'Metoda "refreshrow" je podporov�na pouze v "update object"';
  cSMustBeInBrowseMode = 'Operace je povolena pouze ve stavu dsBROWSE';

  cSUnKnownParamDataType = 'Nezn�m� parametr.typ dat (Param.DataType)';
  cSFieldReadOnly        = 'Sloupec pouze pro �ten� nem��e b�t p�i�azen k hodnot�: %d';
  cSInvalidUpdateCount     = '%d z�znam(�) aktualizov�no. Pouze jeden z�znam byl zm�n�n.';

  cSRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
//--- end added by Petr Stasiak - pestasoft.com ------------------------------------

{$ELSE}

//--- added by pawelsel --------------------------------------------------------
{$IFDEF POLISH}
  cSSQLError1 = 'B��d SQL: %s';
  cSSQLError2 = 'B��d SQL: %s Kod: %d';
  cSSQLError3 = 'B��d SQL: %s Kod: %d SQL: %s';
  cSSQLError4 = 'B��d SQL: %s Kod: %d Komunikat: %s';

  cSListCapacityError = 'Przekroczona pojemno�� listy (%d)';
  cSListCountError = 'Licznik listy poza zakresem (%d)';
  cSListIndexError = 'Indeks listy poza zakresem (%d)';

  cSClonningIsNotSupported = 'Ta klasa nie obs�uguje klonowania';
  cSImmutableOpIsNotAllowed = 'Niedozwolona operacja na niezmienialnych kolekcjach';
  cSStackIsEmpty = 'Stos jest pusty';
  cSVariableWasNotFound = 'Nie znaleziono zmiennej "%s"';
  cSFunctionWasNotFound = 'Nie znaleziono funkcji "%s"';
  cSInternalError = 'B��d wewn�trzny';
  cSSyntaxErrorNear = 'B��d sk�adni przy "%s"';
  cSSyntaxError = 'B��d sk�adni';
  cSUnknownSymbol = 'Nieznany symbol "%s"';
  cSUnexpectedExprEnd = 'Nieoczekiwany koniec wyra�enia';
  cSRightBraceExpected = 'Oczekiwano znaku )';
  cSParametersError = 'Oczekiwana ilo�� parametr�w: %d, znaleziono: %d';
  cSExpectedMoreParams = 'Oczekiwano wi�cej ni� dwa parametry';
  cSInvalidVarByteArray = 'B��dna tablica VarByte';
  cSVariableAlreadyExists = 'Zmienna "%s" ju� istnieje';
  cSTypesMismatch = 'Niezgodno�� typ�w';
  cSUnsupportedVariantType = 'Nieznany typ danych';
  cSUnsupportedOperation = 'Nieznana operacja';

  cSTokenizerIsNotDefined = 'Nie zdefiniowano tokenizera';
  cSLibraryNotFound = 'Nie znaleziono �adnej z bibliotek dynamicznych: %s';
  cSEncodeDateIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Nie mo�na pobra� danych wynikowych';
  cSRowBufferIsNotAssigned = 'Nie przypisano bufora wiersza';
  cSColumnIsNotAccessable = 'Kolumna o numerze %d jest niedost�pna';
  cSConvertionIsNotPossible = 'Konwersja kolumny o numerze %d z %s na %s jest niemo�liwa';
  cSCanNotAccessBlobRecord = 'Brak dost�pu do rekordu typu blob w kolumnie %d z typem %s';
  cSRowDataIsNotAvailable = 'Dane wiersza s� niedost�pne';
  cSResolverIsNotSpecified = 'Ten ResultSet nie ma okre�lonego Resolver-a';
  cSResultsetIsAlreadyOpened = 'ResultSet jest ju� otwarty';
  cSCanNotUpdateEmptyRow = 'Nie mo�na aktualizowa� pustego wiersza';
  cSCanNotUpdateDeletedRow = 'Nie mo�na aktualizowa� usuni�tego wiersza';
  cSCanNotDeleteEmptyRow = 'Nie mo�na usun�� pustego wiersza';
  cSCannotUseCommit = 'Nie mo�na u�y� COMMIT w trybie AUTOCOMMIT';
  cSCannotUseRollBack = 'Nie mo�na u�y� ROLLBACK w trybie AUTOCOMMIT';
  cSCanNotUpdateComplexQuery = 'Nie mo�na aktualizowa� zapytania z�o�onego z wi�cej ni� jednej tabeli';
  cSCanNotUpdateThisQueryType = 'Nie mo�na aktualizowa� tego typu zapytania';
  cSDriverWasNotFound = 'Nie znaleziono wymaganego sterownika bazy danych';
  cSCanNotConnectToServer = 'Nie mo�na po��czy� si� z serwerem SQL';
  cSTableIsNotSpecified = 'Nie okre�lono tabeli';
  cSLiveResultSetsAreNotSupported = '"Live query" nie jest obs�ugiwane przez t� klas�';
  cSInvalidInputParameterCount = 'Liczba parametr�w wej�ciowych jest mniejsza ni� oczekiwana';
  cSIsolationIsNotSupported = 'Poziom izolacji transakcji nie jest obs�ugiwany';
  cSColumnWasNotFound = 'Nie znaleziono kolumny o nazwie "%s"';
  cSWrongTypeForBlobParameter = 'B��dny typ parametru Blob';
  cSIncorrectConnectionURL = 'B��dny URL po��czenia: %s';
  cSUnsupportedProtocol = 'Nieobs�ugiwany protok�: %s';
  cSUnsupportedByDriver    = 'Sterownik nie obs�uguje tej w�a�ciwo�ci natywnie: [%s]';

  cSConnectionIsNotOpened = 'Jeszcze nie nawi�zano po��czenia';
  cSInvalidOpInAutoCommit = 'B��dna operacja w trybie AutoCommit';
  cSInvalidOpInNonAutoCommit = 'B��dna operacja przy wy��czonym AutoCommit';
  cSInvalidOpPrepare = 'Przygotowanie transakcji mo�liwe jest tylko przy pierwszym(!) Starttransaction';

  cSConnectionIsNotAssigned = 'Nie przypisano komponentu po��czenia do bazy danych';
  cSQueryIsEmpty = 'Zapytanie SQL jest puste';
  cSCanNotExecuteMoreQueries = 'Nie mo�na wykona� wi�cej ni� jednego zapytania';
  cSOperationIsNotAllowed1 = 'Niedozwolona operacja w trybie FORWARD ONLY';
  cSOperationIsNotAllowed2 = 'Niedozwolona operacja w trybie READ ONLY';
  cSOperationIsNotAllowed3 = 'Niedozwolona operacja w trybie %s';
  cSOperationIsNotAllowed4 = 'Niedozwolona operacja przy zamkni�tym �r�dle danych';
  cSNoMoreRecords = 'Nie ma ju� wi�cej rekord�w wynikowych';
  cSCanNotOpenResultSet = 'Nie mo�na otworzy� danych wynikowych';
  cSCanNotOpenDataSetWhenDestroying ='Nie mo�na otworzy� dataset gdy componentstate to dsDestroying';
  cSCircularLink = 'Datasource tworzy powi�zanie cykliczne';
  cSBookmarkWasNotFound = 'Nie znaleziono zak�adki (Bookmark)';
  cSIncorrectSearchFieldsNumber = 'B��dna liczba p�l do wyszukiwania';
  cSInvalidOperationInTrans = 'B��dna operacja w trybie transakcji';
  cSIncorrectSymbol = 'B��dny symbol w li�cie p�l "%s".';
  cSIncorrectToken = 'B��dny wyraz za ":"';
  cSIncorrectParamChar = 'B��dna warto�� dla ParamChar';

  cSSelectedTransactionIsolation = 'Wybrany poziom izolacji transakcji nie jest obs�ugiwany';
  cSDriverNotSupported = 'Nie obs�ugiwany sterownik %s';
  cSPattern2Long = 'Wzorzec jest zbyt d�ugi';
  cSDriverNotCapableOutParameters = 'Sterownik nie potrafi obs�u�y� parametr�w';
  cSStatementIsNotAllowed = 'Niedozwolone wyra�enie';
  cSStoredProcIsNotAllowed = 'Niedozwolona procedura sk�adowana';
  cSCannotPerformOperation = 'Nie mo�na wykona� operacji na zamkni�tym zbiorze danych';
  cSInvalidState = 'B��dny stan';
  cSErrorConvertion = 'B��d konwersji';
  cSDataTypeDoesNotSupported = 'Nieobs�ugiwany typ danych';
  cSUnsupportedParameterType = 'Nieobs�ugiwany typ parametru';
  cSUnsupportedDataType = 'Nieobs�ugiwany typ danych';
  cSErrorConvertionField = 'B��d konwersji pola "%s" na SQLType "%s"';
  cSBadOCI = 'Z�a wersja OCI [%s]. Wymagana wersja 8.0.3 lub starsza';
  cSConnect2AsUser = 'Po��czenie z "%s" jako u�ytkownik "%s"';
  cSUnknownError = 'Nieznany b��d';
  cSFieldNotFound1 = 'Nie znaleziono pola "%s"';
  cSFieldNotFound2 = 'Nie znaleziono pola %d';

  cSLoginPromptFailure = 'Nie znaleziono domy�lnego dialogu logowania. Prosz� doda� DBLogDlg do sekcji uses g��wnego pliku aplikacji.';

  cSPropertyQuery = 'Zapytanie mo�e chwil� potrwa� na wi�kszej bazie danych!';
  cSPropertyTables = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  cSPropertyColumns = 'Powiniene� u�ci�li� Katalog, Schemat i/lub Nazw� Tabeli.';
  cSPropertyProcedures = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  cSPropertySequences = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  cSPropertyExecute = 'Czy mimo to wykona� zapytanie?';

  cSFormTest = 'Test Edytora SQL ZEOS';
  cSButtonClose = '&Zamknij';
  cSFormEditor = 'Edytor SQL ZEOS';
  cSTabSheetSelect = 'Wyb�r SQL';
  cSMenuLoad = '�aduj';
  cSMenuSave = 'Zapisz';
  cSButtonGenerate = '&Generuj';
  cSButtonCheck = '&Sprawd�';
  cSButtonTest = '&Test';
  cSButtonOk = '&OK';
  cSButtonCancel = 'A&nuluj';
  cSTableAlias = '&Alias tabeli';
  cSReplaceSQL = 'Za&mie� SQL';
  cSDialogOpenTitle = 'Otw�rz plik SQL';
  cSDialogSaveTitle = 'Zapisz plik SQL';
  cSSQLEditor = 'Edytor SQL';
  cSDatabaseDialog = 'Otw�rz istniej�c� baz�';

  cSUpdateSQLNoResult = 'Update Refresh SQL nie zwr�ci�o �adnych danych';
  cSUpdateSQLRefreshStatementcount ='Wyra�enie Update Refresh SQL musi zwr�ci� 1 rekord danych';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset nie jest w trybie "edit" lub "insert"';
  cSFieldTypeMismatch = 'Niezgodno�� typ�w dla pola ''%s'', oczekiwano: %s otrzymano: %s';
  cSFieldSizeMismatch = 'Niezgodno�� rozmiar�w pola ''%s'', oczekiwano: %d otrzymano: %d';
  {$ENDIF}
  cSNeedField               = 'Pole %s jest wymagane.';

  cSFailedtoInitPrepStmt   = 'Nie uda�o si� zainicjalizowa� przygotowanego zapytania';
  cSFailedtoPrepareStmt    = 'B��d w wyra�eniu podczas procesu przygotowania';
  cSFailedToBindAllValues  = 'B��d aplikacji podczas przypisywania danych';
  cSAttemptExecOnBadPrep   = 'Pr�ba uruchomienia wyra�enia przed zako�czeniem przygotowywania.';
  cSBindingFailure         = 'B��d przypisywania zbioru parametr�w';
  cSPreparedStmtExecFailure = 'B��d wykonania przygotowanego zapytania';
  cSBoundVarStrIndexMissing = 'Nie istnieje zmienna licznikowa "%s"';
  cSBindVarOutOfRange      = 'Warto�� zmiennej licznikowej poza zakresem: %d';
  cSFailedToBindResults    = 'B��d aplikacji podczas ��czenia do wynik�w zapytania';
  cSPreviousResultStillOpen = 'Poprzedni zbi�r wynikowy tego wyra�enia jest nadal otwarty';

//FOS+ 07112006
  cSRefreshRowOnlySupportedWithUpdateObject = 'Metoda refreshrow jest obs�ugiwana tylko przez obiekt typu "update"';
  cSMustBeInBrowseMode = 'Operacja jest dozwolona tylko w stanie dsBROWSE';

  cSUnKnownParamDataType = 'Nieznany Param.DataType';
  cSFieldReadOnly        = 'Nie mo�na przypisa� do pola tylko do odczytu warto�ci: %d';
  cSInvalidUpdateCount     = 'Liczba zaktualizowanych rekord�w: %d. tylko jeden rekord powinien by� zaktualizowany.';

  cSRowBufferWidthExceeded ='Przekroczono rozmiar bufora. Spr�buj u�y� mniejszej liczby kolumn lub d�u�szych kolumn w zapytaniu SQL.';

{$ELSE} // default: ENGLISH

  cSSQLError1 = 'SQL Error: %s';
  cSSQLError2 = 'SQL Error: %s Code: %d';
  cSSQLError3 = 'SQL Error: %s Code: %d SQL: %s';
  cSSQLError4 = 'SQL Error: %s Code: %d Message: %s';

  cSListCapacityError = 'List capacity out of bounds (%d)';
  cSListCountError = 'List count out of bounds (%d)';
  cSListIndexError = 'List index out of bounds (%d)';

  cSClonningIsNotSupported = 'Clonning is not supported by this class';
  cSImmutableOpIsNotAllowed = 'The operation is not allowed on not changeable collections';
  cSStackIsEmpty = 'Stack is empty';
  cSVariableWasNotFound = 'Variable "%s" was not found';
  cSFunctionWasNotFound = 'Function "%s" was not found';
  cSInternalError = 'Internal error';
  cSSyntaxErrorNear = 'Syntax error near "%s"';
  cSSyntaxError = 'Syntax error';
  cSUnknownSymbol = 'Unknown symbol "%s"';
  cSUnexpectedExprEnd = 'Unexpected end of expression';
  cSRightBraceExpected = ') expected';
  cSParametersError = '%d parameters were expected but %d were found';
  cSExpectedMoreParams = 'More than two parameters are expected';
  cSInvalidVarByteArray = 'Invalid VarByte array';
  cSVariableAlreadyExists = 'Variable "%s" already exists';
  cSTypesMismatch = 'Types mismatch';
  cSUnsupportedVariantType = 'Unsupported variant type';
  cSUnsupportedOperation = 'Unsupported operation';

  cSTokenizerIsNotDefined = 'Tokenizer is not defined';
  cSLibraryNotFound = 'None of the dynamic libraries can be found or is not loadable: %s !'#10#13'Use TZConnection.LibraryLocation if the location is invalid.';
  cSEncodeDateIsNotSupported = 'This version does not support isc_encode_sql_date';
  cSEncodeTimeIsNotSupported = 'This version does not support isc_encode_sql_time';
  cSEncodeTimestampIsNotSupported = 'This version does not support isc_encode_sql_timestamp';
  cSDecodeDateIsNotSupported = 'This version does not support isc_decode_sql_date';
  cSDecodeTimeIsNotSupported = 'This version does not support isc_decode_sql_time';
  cSDecodeTimestampIsNotSupported = 'This version does not support isc_decode_sql_timestamp';

  cSCanNotRetrieveResultSetData = 'Cannot retrieve Resultset data';
  cSRowBufferIsNotAssigned = 'Row buffer is not assigned';
  cSColumnIsNotAccessable = 'Column with index %d is not accessable';
  cSConvertionIsNotPossible = 'Convertion is not possible for column %d from %s to %s';
  cSCanNotAccessBlobRecord = 'Cannot access blob record in column %d with type %s';
  cSRowDataIsNotAvailable = 'Row data is not available';
  cSResolverIsNotSpecified = 'Resolver is not specified for this ResultSet';
  cSResultsetIsAlreadyOpened = 'Resultset is already open';
  cSCanNotUpdateEmptyRow = 'Cannot update an empty row';
  cSCanNotUpdateDeletedRow = 'Cannot update a deleted row';
  cSCanNotDeleteEmptyRow = 'Cannot delete an empty row';
  cSCannotUseCommit = 'You cannot use COMMIT in AUTOCOMMIT mode';
  cSCannotUseRollBack = 'You cannot use ROLLBACK in AUTOCOMMIT mode';
  cSCanNotUpdateComplexQuery = 'Cannot update a complex query with more then one table';
  cSCanNotUpdateThisQueryType = 'Cannot update this query type';
  cSDriverWasNotFound = 'Requested database driver was not found';
  cSCanNotConnectToServer = 'Cannot connect to SQL server';
  cSTableIsNotSpecified = 'Table is not specified';
  cSLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  cSInvalidInputParameterCount = 'Input parameter count is less then expected';
  cSIsolationIsNotSupported = 'Transaction isolation level is not supported';
  cSColumnWasNotFound = 'Column with name "%s" was not found';
  cSWrongTypeForBlobParameter = 'Wrong type for Blob parameter';
  cSIncorrectConnectionURL = 'Incorrect connection URL: %s';
  cSUnsupportedProtocol = 'Unsupported protocol: %s';
  cSUnsupportedByDriver    = 'Driver can not support this feature natively: [%s]';

  cSConnectionIsNotOpened = 'Connection is not opened yet';
  cSInvalidOpInAutoCommit = 'Invalid operation in AutoCommit mode';
  cSInvalidOpInNonAutoCommit = 'Invalid operation in non AutoCommit mode';
  cSInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

  cSConnectionIsNotAssigned = 'Database connection component is not assigned';
  cSQueryIsEmpty = 'SQL Query is empty';
  cSCanNotExecuteMoreQueries = 'Cannot execute more then one query';
  cSOperationIsNotAllowed1 = 'Operation is not allowed in FORWARD ONLY mode';
  cSOperationIsNotAllowed2 = 'Operation is not allowed in READ ONLY mode';
  cSOperationIsNotAllowed3 = 'Operation is not allowed in %s mode';
  cSOperationIsNotAllowed4 = 'Operation is not allowed for closed dataset';
  cSNoMoreRecords = 'No more records in the Resultset';
  cSCanNotOpenResultSet = 'Can not open a Resultset';
  cSCanNotOpenDataSetWhenDestroying ='Cannot open a dataset when the componentstate is dsDestroying';
  cSCircularLink = 'Datasource makes a circular link';
  cSBookmarkWasNotFound = 'Bookmark was not found';
  cSIncorrectSearchFieldsNumber = 'Incorrect number of search field values';
  cSInvalidOperationInTrans = 'Invalid operation in explicit transaction mode';
  cSIncorrectSymbol = 'Incorrect symbol in field list "%s".';
  cSIncorrectToken = 'Incorrect token followed by ":"';
  cSIncorrectParamChar = 'Invalid value for ParamChar';

  cSSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  cSDriverNotSupported = 'Driver not supported %s';
  cSPattern2Long = 'Pattern is too long';
  cSDriverNotCapableOutParameters = 'Driver is not capable to handle parameters';
  cSStatementIsNotAllowed = 'Statement is not allowed';
  cSStoredProcIsNotAllowed = 'The stored proc is not allowed';
  cSCannotPerformOperation = 'Can not perform operation on closed Resultset';
  cSInvalidState = 'Invalid state';
  cSErrorConvertion = 'Convertion error';
  cSDataTypeDoesNotSupported = 'Data type is not supported';
  cSUnsupportedParameterType = 'Unsupported parameter type';
  cSUnsupportedDataType = 'Unsupported data type';
  cSErrorConvertionField = 'Conversion error for field "%s" to SQLType "%s"';
  cSBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  cSConnect2AsUser = 'Connect to "%s" as user "%s"';
  cSUnknownError = 'Unknown error';
  cSFieldNotFound1 = 'Field "%s" was not found';
  cSFieldNotFound2 = 'Field %d was not found';

  cSLoginPromptFailure = 'Can not find default login prompt dialog. Please add DBLogDlg to the uses section of your main file.';

  cSPropertyQuery = 'The Query may last a while on large databases!';
  cSPropertyTables = 'You should limit it by Catalog and/or Schema.';
  cSPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  cSPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  cSPropertySequences = 'You should limit it by Catalog and/or Schema.';
  cSPropertyExecute = 'Should the Query be executed anyway?';

  cSFormTest = 'ZEOS SQL Editor Test';
  cSButtonClose = '&Close';
  cSFormEditor = 'ZEOS SQL Editor';
  cSTabSheetSelect = 'Select SQL';
  cSMenuLoad = 'Load';
  cSMenuSave = 'Save';
  cSButtonGenerate = '&Generate';
  cSButtonCheck = 'C&heck';
  cSButtonTest = '&Test';
  cSButtonOk = '&OK';
  cSButtonCancel = '&Cancel';
  cSTableAlias = 'T&able alias';
  cSReplaceSQL = '&Replace SQL';
  cSDialogOpenTitle = 'Open SQL File';
  cSDialogSaveTitle = 'Save SQL File';
  cSSQLEditor = 'SQL Editor';
  cSDatabaseDialog = 'Open existing database';

  cSUpdateSQLNoResult = 'Update Refresh SQL delivered no resultset';
  cSUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  cSNotEditing = 'Dataset not in edit or insert mode';
  cSFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  cSFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}
  cSNeedField               = 'Field %s is required, but not supplied.';

  cSFailedtoInitPrepStmt   = 'Prepared statement failed to initialize';
  cSFailedtoPrepareStmt    = 'Statement failed during prepare process';
  cSFailedToBindAllValues  = 'Application failed to pre-bind all values';
  cSAttemptExecOnBadPrep   = 'Attempt made to execute a statement before a successful preparation.';
  cSBindingFailure         = 'Failed to bind parameter set';
  cSPreparedStmtExecFailure = 'Prepared statement failed to execute';
  cSBoundVarStrIndexMissing = 'Bound variable text index "%s" does not exist';
  cSBindVarOutOfRange      = 'Bound variable index out of range: %d';
  cSFailedToBindResults    = 'Application failed to bind to the result set';
  cSPreviousResultStillOpen = 'Previous resultset of this statement is still open';

//FOS+ 07112006
  cSRefreshRowOnlySupportedWithUpdateObject = 'The refreshrow method is only supported with an update object';
  cSMustBeInBrowseMode = 'Operation is only allowed in dsBROWSE state';

  cSUnKnownParamDataType = 'Unknown Param.DataType';
  cSFieldReadOnly        = 'Readonly field can''t be assigned a value: %s';
  cSInvalidUpdateCount     = '%d record(s) updated. Only one record should have been updated.';

  cSRowBufferWidthExceeded ='Row buffer width exceeded. Try using fewer or longer columns in SQL query.';

{$ENDIF} // POLISH

{$ENDIF} // CZECH

{$ENDIF} // RUSSIAN

{$ENDIF}   // INDONESIAN <--- added by tohenk

{$ENDIF}   // ROMANA

{$ENDIF} //SPANISH

{$ENDIF} // GERMAN

{$ENDIF} // DUTCH

{$ENDIF} // PORTUGUESE
{$ENDIF FRENCH}
type
  TMessageToRaw = function(const AMessage: String; Const RawCP: Word): RawByteString;

var
  MessageCodePage: Word;
  SSQLError1: String;
  SSQLError2: String;
  SSQLError3: String;
  SSQLError4: String;

  SListCapacityError: String;
  SListCountError: String;
  SListIndexError: String;

  SClonningIsNotSupported: String;
  SImmutableOpIsNotAllowed: String;
  SStackIsEmpty: String;
  SVariableWasNotFound: String;
  SFunctionWasNotFound: String;
  SInternalError: String;
  SSyntaxErrorNear: String;
  SSyntaxError: String;
  SUnknownSymbol: String;
  SUnexpectedExprEnd: String;
  SRightBraceExpected: String;
  SParametersError: String;
  SExpectedMoreParams: String;
  SInvalidVarByteArray: String;
  SVariableAlreadyExists: String;
  STypesMismatch: String;
  SUnsupportedVariantType: String;
  SUnsupportedOperation: String;

  STokenizerIsNotDefined: String;
  SLibraryNotFound: String;
  SLibraryNotCompatible: String;
  SEncodeDateIsNotSupported: String;
  SEncodeTimeIsNotSupported: String;
  SEncodeTimestampIsNotSupported: String;
  SDecodeDateIsNotSupported: String;
  SDecodeTimeIsNotSupported: String;
  SDecodeTimestampIsNotSupported: String;

  SCanNotRetrieveResultSetData: String;
  SRowBufferIsNotAssigned: String;
  SColumnIsNotAccessable: String;
  SConvertionIsNotPossible: String;
  SCanNotAccessBlobRecord: String;
  SRowDataIsNotAvailable: String;
  SResolverIsNotSpecified: String;
  SResultsetIsAlreadyOpened: String;
  SCanNotUpdateEmptyRow: String;
  SCanNotUpdateDeletedRow: String;
  SCanNotDeleteEmptyRow: String;
  SCannotUseCommit: String;
  SCannotUseRollBack: String;
  SCanNotUpdateComplexQuery: String;
  SCanNotUpdateThisQueryType: String;
  SDriverWasNotFound: String;
  SCanNotConnectToServer: String;
  STableIsNotSpecified: String;
  SLiveResultSetsAreNotSupported: String;
  SInvalidInputParameterCount: String;
  SIsolationIsNotSupported: String;
  SColumnWasNotFound: String;
  SWrongTypeForBlobParameter: String;
  SIncorrectConnectionURL: String;
  SUnsupportedProtocol: String;
  SUnsupportedByDriver   : String;

  SConnectionIsNotOpened: String;
  SInvalidOpInAutoCommit: String;
  SInvalidOpInNonAutoCommit: String;
  SInvalidOpPrepare: String;

  SConnectionIsNotAssigned: String;
  SQueryIsEmpty: String;
  SCanNotExecuteMoreQueries: String;
  SOperationIsNotAllowed1: String;
  SOperationIsNotAllowed2: String;
  SOperationIsNotAllowed3: String;
  SOperationIsNotAllowed4: String;
  SNoMoreRecords: String;
  SCanNotOpenResultSet: String;
  SCanNotOpenDataSetWhenDestroying: String;
  SCircularLink: String;
  SBookmarkWasNotFound: String;
  SIncorrectSearchFieldsNumber: String;
  SInvalidOperationInTrans: String;
  SIncorrectSymbol: String;
  SIncorrectToken: String;
  SIncorrectParamChar: String;

  SSelectedTransactionIsolation: String;
  SDriverNotSupported: String;
  SPattern2Long: String;
  SDriverNotCapableOutParameters: String;
  SStatementIsNotAllowed: String;
  SStoredProcIsNotAllowed: String;
  SCannotPerformOperation: String;
  SInvalidState: String;
  SErrorConvertion: String;
  SDataTypeDoesNotSupported: String;
  SUnsupportedParameterType: String;
  SUnsupportedDataType: String;
  SErrorConvertionField: String;
  SBadOCI: String;
  SConnect2AsUser: String;
  SUnknownError: String;
  SFieldNotFound1: String;
  SFieldNotFound2: String;

  SLoginPromptFailure: String;

  SPropertyQuery: String;
  SPropertyTables: String;
  SPropertyColumns: String;
  SPropertyProcedures: String;
  SPropertySequences: String;
  SPropertyExecute: String;

  SFormTest: String;
  SButtonClose: String;
  SFormEditor: String;
  STabSheetSelect: String;
  SMenuLoad: String;
  SMenuSave: String;
  SButtonGenerate: String;
  SButtonCheck: String;
  SButtonTest: String;
  SButtonOk: String;
  SButtonCancel: String;
  STableAlias: String;
  SReplaceSQL: String;
  SDialogOpenTitle: String;
  SDialogSaveTitle: String;
  SSQLEditor: String;
  SDatabaseDialog: String;

  SUpdateSQLNoResult: String;
  SUpdateSQLRefreshStatementcount: String;
  {$IFDEF FPC}
  SNotEditing: String;
  SFieldTypeMismatch: String;
  SFieldSizeMismatch: String;
  {$ENDIF}
  SNeedField: String;

  SFailedtoInitPrepStmt: String;
  SFailedtoPrepareStmt: String;
  SFailedToBindAllValues: String;
  SAttemptExecOnBadPrep: String;
  SBindingFailure: String;
  SPreparedStmtExecFailure: String;
  SBoundVarStrIndexMissing: String;
  SBindVarOutOfRange: String;
  SFailedToBindResults: String;
  SPreviousResultStillOpen: String;


  SRefreshRowOnlySupportedWithUpdateObject: String;
  SMustBeInBrowseMode: String;

  SUnKnownParamDataType: String;
  SFieldReadOnly: String;
  SInvalidUpdateCount: String;

  SRowBufferWidthExceeded: String;

implementation

procedure loadmessages;
begin
  MessageCodePage := cCodePage;

  SSQLError1 := cSSQLError1;
  SSQLError2 := cSSQLError2;
  SSQLError3 := cSSQLError3;
  SSQLError4 := cSSQLError4;

  SListCapacityError := cSListCapacityError;
  SListCountError := cSListCountError;
  SListIndexError := cSListIndexError;

  SClonningIsNotSupported := cSClonningIsNotSupported;
  SImmutableOpIsNotAllowed := cSImmutableOpIsNotAllowed;
  SStackIsEmpty := cSStackIsEmpty;
  SVariableWasNotFound := cSVariableWasNotFound;
  SFunctionWasNotFound := cSFunctionWasNotFound;
  SInternalError := cSInternalError;
  SSyntaxErrorNear := cSSyntaxErrorNear;
  SSyntaxError := cSSyntaxError;
  SUnknownSymbol := cSUnknownSymbol;
  SUnexpectedExprEnd := cSUnexpectedExprEnd;
  SRightBraceExpected := cSRightBraceExpected;
  SParametersError := cSParametersError;
  SExpectedMoreParams := cSExpectedMoreParams;
  SInvalidVarByteArray := cSInvalidVarByteArray;
  SVariableAlreadyExists := cSVariableAlreadyExists;
  STypesMismatch := cSTypesMismatch;
  SUnsupportedVariantType := cSUnsupportedVariantType;
  SUnsupportedOperation := cSUnsupportedOperation;

  STokenizerIsNotDefined := cSTokenizerIsNotDefined;
  SLibraryNotFound := cSLibraryNotFound;
  SLibraryNotCompatible := cSLibraryNotCompatible;
  SEncodeDateIsNotSupported := cSEncodeDateIsNotSupported;
  SEncodeTimeIsNotSupported := cSEncodeTimeIsNotSupported;
  SEncodeTimestampIsNotSupported := cSEncodeTimestampIsNotSupported;
  SDecodeDateIsNotSupported := cSDecodeDateIsNotSupported;
  SDecodeTimeIsNotSupported := cSDecodeTimeIsNotSupported;
  SDecodeTimestampIsNotSupported := cSDecodeTimestampIsNotSupported;

  SCanNotRetrieveResultSetData := cSCanNotRetrieveResultSetData;
  SRowBufferIsNotAssigned := cSRowBufferIsNotAssigned;
  SColumnIsNotAccessable := cSColumnIsNotAccessable;
  SConvertionIsNotPossible := cSConvertionIsNotPossible;
  SCanNotAccessBlobRecord := cSCanNotAccessBlobRecord;
  SRowDataIsNotAvailable := cSRowDataIsNotAvailable;
  SResolverIsNotSpecified := cSResolverIsNotSpecified;
  SResultsetIsAlreadyOpened := cSResultsetIsAlreadyOpened;
  SCanNotUpdateEmptyRow := cSCanNotUpdateEmptyRow;
  SCanNotUpdateDeletedRow := cSCanNotUpdateDeletedRow;
  SCanNotDeleteEmptyRow := cSCanNotDeleteEmptyRow;
  SCannotUseCommit := cSCannotUseCommit;
  SCannotUseRollBack := cSCannotUseRollBack;
  SCanNotUpdateComplexQuery := cSCanNotUpdateComplexQuery;
  SCanNotUpdateThisQueryType := cSCanNotUpdateThisQueryType;
  SDriverWasNotFound := cSDriverWasNotFound;
  SCanNotConnectToServer := cSCanNotConnectToServer;
  STableIsNotSpecified := cSTableIsNotSpecified;
  SLiveResultSetsAreNotSupported := cSLiveResultSetsAreNotSupported;
  SInvalidInputParameterCount := cSInvalidInputParameterCount;
  SIsolationIsNotSupported := cSIsolationIsNotSupported;
  SColumnWasNotFound := cSColumnWasNotFound;
  SWrongTypeForBlobParameter := cSWrongTypeForBlobParameter;
  SIncorrectConnectionURL := cSIncorrectConnectionURL;
  SUnsupportedProtocol := cSUnsupportedProtocol;
  SUnsupportedByDriver := cSUnsupportedByDriver;

  SConnectionIsNotOpened := cSConnectionIsNotOpened;
  SInvalidOpInAutoCommit := cSInvalidOpInAutoCommit;
  SInvalidOpInNonAutoCommit := cSInvalidOpInNonAutoCommit;
  SInvalidOpPrepare := cSInvalidOpPrepare;

  SConnectionIsNotAssigned := cSConnectionIsNotAssigned;
  SQueryIsEmpty := cSQueryIsEmpty;
  SCanNotExecuteMoreQueries := cSCanNotExecuteMoreQueries;
  SOperationIsNotAllowed1 := cSOperationIsNotAllowed1;
  SOperationIsNotAllowed2 := cSOperationIsNotAllowed2;
  SOperationIsNotAllowed3 := cSOperationIsNotAllowed3;
  SOperationIsNotAllowed4 := cSOperationIsNotAllowed4;
  SNoMoreRecords := cSNoMoreRecords;
  SCanNotOpenResultSet := cSCanNotOpenResultSet;
  SCanNotOpenDataSetWhenDestroying := cSCanNotOpenDataSetWhenDestroying;
  SCircularLink := cSCircularLink;
  SBookmarkWasNotFound := cSBookmarkWasNotFound;
  SIncorrectSearchFieldsNumber := cSIncorrectSearchFieldsNumber;
  SInvalidOperationInTrans := cSInvalidOperationInTrans;
  SIncorrectSymbol := cSIncorrectSymbol;
  SIncorrectToken := cSIncorrectToken;
  SIncorrectParamChar := cSIncorrectParamChar;

  SSelectedTransactionIsolation := cSSelectedTransactionIsolation;
  SDriverNotSupported := cSDriverNotSupported;
  SPattern2Long := cSPattern2Long;
  SDriverNotCapableOutParameters := cSDriverNotCapableOutParameters;
  SStatementIsNotAllowed := cSStatementIsNotAllowed;
  SStoredProcIsNotAllowed := cSStoredProcIsNotAllowed;
  SCannotPerformOperation := cSCannotPerformOperation;
  SInvalidState := cSInvalidState;
  SErrorConvertion := cSErrorConvertion;
  SDataTypeDoesNotSupported := cSDataTypeDoesNotSupported;
  SUnsupportedParameterType := cSUnsupportedParameterType;
  SUnsupportedDataType := cSUnsupportedDataType;
  SErrorConvertionField := cSErrorConvertionField;
  SBadOCI := cSBadOCI;
  SConnect2AsUser := cSConnect2AsUser;
  SUnknownError := cSUnknownError;
  SFieldNotFound1 := cSFieldNotFound1;
  SFieldNotFound2 := cSFieldNotFound2;

  SLoginPromptFailure := cSLoginPromptFailure;

  SPropertyQuery := cSPropertyQuery;
  SPropertyTables := cSPropertyTables;
  SPropertyColumns := cSPropertyColumns;
  SPropertyProcedures := cSPropertyProcedures;
  SPropertySequences := cSPropertySequences;
  SPropertyExecute := cSPropertyExecute;

  SFormTest := cSFormTest;
  SButtonClose := cSButtonClose;
  SFormEditor := cSFormEditor;
  STabSheetSelect := cSTabSheetSelect;
  SMenuLoad := cSMenuLoad;
  SMenuSave := cSMenuSave;
  SButtonGenerate := cSButtonGenerate;
  SButtonCheck := cSButtonCheck;
  SButtonTest := cSButtonTest;
  SButtonOk := cSButtonOk;
  SButtonCancel := cSButtonCancel;
  STableAlias := cSTableAlias;
  SReplaceSQL := cSReplaceSQL;
  SDialogOpenTitle := cSDialogOpenTitle;
  SDialogSaveTitle := cSDialogSaveTitle;
  SSQLEditor := cSSQLEditor;
  SDatabaseDialog := cSDatabaseDialog;

  SUpdateSQLNoResult := cSUpdateSQLNoResult;
  SUpdateSQLRefreshStatementcount := cSUpdateSQLRefreshStatementcount;
  {$IFDEF FPC}
  SNotEditing := cSNotEditing;
  SFieldTypeMismatch := cSFieldTypeMismatch;
  SFieldSizeMismatch := cSFieldSizeMismatch;
  {$ENDIF}
  SNeedField := cSNeedField;

  SFailedtoInitPrepStmt := cSFailedtoInitPrepStmt;
  SFailedtoPrepareStmt := cSFailedtoPrepareStmt;
  SFailedToBindAllValues := cSFailedToBindAllValues;
  SAttemptExecOnBadPrep := cSAttemptExecOnBadPrep;
  SBindingFailure := cSBindingFailure;
  SPreparedStmtExecFailure := cSPreparedStmtExecFailure;
  SBoundVarStrIndexMissing := cSBoundVarStrIndexMissing;
  SBindVarOutOfRange := cSBindVarOutOfRange;
  SFailedToBindResults := cSFailedToBindResults;
  SPreviousResultStillOpen := cSPreviousResultStillOpen;

  SRefreshRowOnlySupportedWithUpdateObject := cSRefreshRowOnlySupportedWithUpdateObject;
  SMustBeInBrowseMode := cSMustBeInBrowseMode;

  SUnKnownParamDataType := cSUnKnownParamDataType;
  SFieldReadOnly := cSFieldReadOnly;
  SInvalidUpdateCount := cSInvalidUpdateCount;

  SRowBufferWidthExceeded := cSRowBufferWidthExceeded;
end;

initialization
  loadmessages;
end.




