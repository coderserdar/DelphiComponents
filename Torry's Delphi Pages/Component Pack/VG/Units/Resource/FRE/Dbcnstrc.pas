{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DBCnstRC - French DBConsts messages           }
{                                                       }
{         Copyright (c) 1997, 1998                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit DBCnstRC;

interface
uses SysUtils, Forms, Dialogs;

procedure InitResStringsDBConsts;
procedure FreeResStringsDBConsts;

implementation
uses DBConsts, vgUtils;

{$IFDEF _D3_}

resourcestring
  SDataSetOpen                   = 'Ne peut effectuer cette opération sur un dataset ouvert';
  SDataSetClosed                 = 'Ne peut effectuer cette opération sur un dataset fermé';

{$IFDEF _D4_}
  SFieldNotFound                 = 'Le champ ''%s'' est introuvable';
{$ELSE}
  SFieldNotFound                 = '%s: le champ ''%s'' est introuvable';
{$ENDIF}
  SDataSetMissing                = 'Le champ ''%s'' n''a pas de dataset';
  SDataSetEmpty                  = 'Ne peut effectuer cette opération sur un dataset vide';
  SFieldTypeMismatch             = 'Le champ ''%s'' n''est pas du type attendu';

  SFieldAccessError              = 'Ne peut accéder au champ ''%s'' de type %s';
  SFieldValueError               = 'Valeur incorrecte pour le champ ''%s''';
  SFieldRangeError               = '%g n''est pas une valeur valide pour le champ ''%s''. L''étendu permise est de %g à %g';
  SInvalidIntegerValue           = '''%s'' n''est pas une valeur entière valide pour le champ ''%s''';
  SInvalidBoolValue              = '''%s'' n''est pas une valeur booléenne valide pour le champ ''%s''';
  SInvalidFloatValue             = '''%s'' n''est pas une valeur flottante valide pour le champ ''%s''';

  SNotEditing                    = 'Le dataset n''est pas en mode édition ou insertion';

  SDataSetReadOnly               = 'Ne peut modifier un dataset en lecture seule';

  SInvalidCalcType               = 'Le champ ''%s'' ne peut pas être un champ calculé ou référencé';
  SFieldReadOnly                 = 'Le champ ''%s'' ne peut pas être modifié';
  SNotIndexField                 = 'Le champ ''%s'' n''est pas indexé ou ne peut être modifié';
  SInvalidVarByteArray           = 'Type ou taille de variant invalide';
  SFieldOutOfRange               = 'La valeur du champ ''%s'' est en dehors des limites';
  SBCDOverflow                   = '(Overflow/Surcharge)';
  SFieldRequired                 = 'Le champ ''%s'' doit avoir une valeur';

  SInvalidFieldSize              = 'Taille du champ invalide';


  SFieldIndexError               = 'L''index du champ est hors limite';

  SNoFieldIndexes                = 'Pas d''index actif';
  SIndexFieldMissing             = 'Ne peut accéder au champ indexé ''%s''';
  SFieldNameMissing              = 'Nom du champ manquant';
  SDuplicateFieldName            = 'Nom de champ dupliqué ''%s''';
  SDuplicateIndexName            = 'Nom d''index dupliqué ''%s''';
  SNoIndexForFields              = '''%s'' n''a pas d''index pour le champ ''%s''';
  SCircularDataLink              = 'Lien circulaire non-autorisé';

  STextFalse                     = 'Faux';
  STextTrue                      = 'Vrai';

  SFirstRecord                   = 'Premier enregistrement';
  SPriorRecord                   = 'Enregistrement précédent';
  SNextRecord                    = 'Enregistrement suivant';
  SLastRecord                    = 'Dernier enregistrement';
  SInsertRecord                  = 'Insérer un enregistrement';
  SDeleteRecord                  = 'Effacer un enregistrement';
  SEditRecord                    = 'Editer un enregistrement';
  SPostEdit                      = 'Appliquer les modifications';
  SCancelEdit                    = 'Annuler les modifications';
  SRefreshRecord                 = 'Refraîchir les données';
  SDeleteRecordQuestion          = 'Effacer les enregistrements ?';
  SDeleteMultipleRecordsQuestion = 'Effacer tous les enregistrements sélectionnés ?';

  STooManyColumns                = 'La grille demande d''afficher plus de 256 colonnes';
  SLookupInfoError               = 'L''information de r©f©rence pour le champ "%s" est incompl¨te';
  SDataSourceChange              = 'Le DataSource ne peut ªtre chang©';

  SExprTermination               = 'Expression filtre terminée incorrectement';
  SExprNameError                 = 'Nom de champ non-terminé';
  SExprStringError               = 'Constante chaîne non-terminée';
  SExprInvalidChar               = 'Caractère de filtre incorrect : ''%s''';
  SExprNoRParen                  = ''')'' attendu mais %s trouvé';
  SExprExpected                  = 'Expression attendue mais "%s" trouvé';
  SExprBadField                  = 'Le champ ''%s'' ne peut être utilisé dans une expression filtre';
  SExprBadNullTest               = 'NULL seulement permis avec ''='' et ''<>''';
  SExprRangeError                = 'Constante hors limites';
  SExprNotBoolean                = 'Le champ ''%s'' n''est pas un booléen';
  SExprIncorrect                 = 'Expression filtre formatée incorrectement';
  SExprNothing                   = 'Rien';

  SDataSourceFixed               = 'Opération non permie dans une DBCtrlGrid';
  SNotReplicatable               = 'Le contrôle ne peut être utilisé dans une DBCtrlGrid';
  SPropDefByLookup               = 'Propriété déjà définie dans le champ référencé';

{$ENDIF}

procedure InitResStringsDBConsts;
begin
{$IFDEF _D3_}
  CopyResString(@SDataSetOpen                   , @DBConsts.SDataSetOpen                   , True);
  CopyResString(@SDataSetClosed                 , @DBConsts.SDataSetClosed                 , True);
  CopyResString(@SUnknownFieldType              , @DBConsts.SUnknownFieldType              , True);
  CopyResString(@SFieldNotFound                 , @DBConsts.SFieldNotFound                 , True);
  CopyResString(@SDataSetMissing                , @DBConsts.SDataSetMissing                , True);
  CopyResString(@SDataSetEmpty                  , @DBConsts.SDataSetEmpty                  , True);
  CopyResString(@SFieldTypeMismatch             , @DBConsts.SFieldTypeMismatch             , True);
  CopyResString(@SFieldRangeError               , @DBConsts.SFieldRangeError               , True);
  CopyResString(@SInvalidIntegerValue           , @DBConsts.SInvalidIntegerValue           , True);
  CopyResString(@SInvalidFloatValue             , @DBConsts.SInvalidFloatValue             , True);
  CopyResString(@SInvalidBoolValue              , @DBConsts.SInvalidBoolValue              , True);
  CopyResString(@SNotEditing                    , @DBConsts.SNotEditing                    , True);
  CopyResString(@SDataSetReadOnly               , @DBConsts.SDataSetReadOnly               , True);
  CopyResString(@SFieldReadOnly                 , @DBConsts.SFieldReadOnly                 , True);
  CopyResString(@SNotIndexField                 , @DBConsts.SNotIndexField                 , True);
  CopyResString(@SFieldRequired                 , @DBConsts.SFieldRequired                 , True);
  CopyResString(@SFieldAccessError              , @DBConsts.SFieldAccessError              , True);
  CopyResString(@SFieldValueError               , @DBConsts.SFieldValueError               , True);
  CopyResString(@SInvalidCalcType               , @DBConsts.SInvalidCalcType               , True);
  CopyResString(@SInvalidFieldSize              , @DBConsts.SInvalidFieldSize              , True);
  CopyResString(@SCircularDataLink              , @DBConsts.SCircularDataLink              , True);
  CopyResString(@SFieldIndexError               , @DBConsts.SFieldIndexError               , True);
  CopyResString(@SNoFieldIndexes                , @DBConsts.SNoFieldIndexes                , True);
  CopyResString(@SIndexFieldMissing             , @DBConsts.SIndexFieldMissing             , True);
  CopyResString(@SDuplicateFieldName            , @DBConsts.SDuplicateFieldName            , True);
  CopyResString(@SDuplicateIndexName            , @DBConsts.SDuplicateIndexName            , True);
  CopyResString(@SFieldNameMissing              , @DBConsts.SFieldNameMissing              , True);
  CopyResString(@SNoIndexForFields              , @DBConsts.SNoIndexForFields              , True);
  CopyResString(@STextFalse                     , @DBConsts.STextFalse                     , True);
  CopyResString(@STextTrue                      , @DBConsts.STextTrue                      , True);
  CopyResString(@SFirstRecord                   , @DBConsts.SFirstRecord                   , True);
  CopyResString(@SPriorRecord                   , @DBConsts.SPriorRecord                   , True);
  CopyResString(@SNextRecord                    , @DBConsts.SNextRecord                    , True);
  CopyResString(@SLastRecord                    , @DBConsts.SLastRecord                    , True);
  CopyResString(@SInsertRecord                  , @DBConsts.SInsertRecord                  , True);
  CopyResString(@SDeleteRecord                  , @DBConsts.SDeleteRecord                  , True);
  CopyResString(@SEditRecord                    , @DBConsts.SEditRecord                    , True);
  CopyResString(@SPostEdit                      , @DBConsts.SPostEdit                      , True);
  CopyResString(@SCancelEdit                    , @DBConsts.SCancelEdit                    , True);
  CopyResString(@SRefreshRecord                 , @DBConsts.SRefreshRecord                 , True);
  CopyResString(@SDeleteRecordQuestion          , @DBConsts.SDeleteRecordQuestion          , True);
  CopyResString(@SDeleteMultipleRecordsQuestion , @DBConsts.SDeleteMultipleRecordsQuestion , True);
  CopyResString(@STooManyColumns                , @DBConsts.STooManyColumns                , True);
  CopyResString(@SLookupInfoError               , @DBConsts.SLookupInfoError               , True);
  CopyResString(@SDataSourceChange              , @DBConsts.SDataSourceChange              , True);
  CopyResString(@SExprTermination               , @DBConsts.SExprTermination               , True);
  CopyResString(@SExprNameError                 , @DBConsts.SExprNameError                 , True);
  CopyResString(@SExprStringError               , @DBConsts.SExprStringError               , True);
  CopyResString(@SExprInvalidChar               , @DBConsts.SExprInvalidChar               , True);
  CopyResString(@SExprNoRParen                  , @DBConsts.SExprNoRParen                  , True);
  CopyResString(@SExprExpected                  , @DBConsts.SExprExpected                  , True);
  CopyResString(@SExprBadField                  , @DBConsts.SExprBadField                  , True);
  CopyResString(@SExprBadNullTest               , @DBConsts.SExprBadNullTest               , True);
  CopyResString(@SExprRangeError                , @DBConsts.SExprRangeError                , True);
  CopyResString(@SExprNotBoolean                , @DBConsts.SExprNotBoolean                , True);
  CopyResString(@SExprIncorrect                 , @DBConsts.SExprIncorrect                 , True);
  CopyResString(@SExprNothing                   , @DBConsts.SExprNothing                   , True);
  CopyResString(@SNotReplicatable               , @DBConsts.SNotReplicatable               , True);
  CopyResString(@SPropDefByLookup               , @DBConsts.SPropDefByLookup               , True);
  CopyResString(@SDataSourceFixed               , @DBConsts.SDataSourceFixed               , True);
  CopyResString(@SFieldOutOfRange               , @DBConsts.SFieldOutOfRange               , True);
  CopyResString(@SBCDOverflow                   , @DBConsts.SBCDOverflow                   , True);
  CopyResString(@SInvalidVarByteArray           , @DBConsts.SInvalidVarByteArray           , True);
{$ENDIF}
end;

procedure FreeResStringsDBConsts;
begin
{$IFDEF _D3_}
  RestoreResString(@DBConsts.SDataSetOpen                   );
  RestoreResString(@DBConsts.SDataSetClosed                 );
  RestoreResString(@DBConsts.SUnknownFieldType              );
  RestoreResString(@DBConsts.SFieldNotFound                 );
  RestoreResString(@DBConsts.SDataSetMissing                );
  RestoreResString(@DBConsts.SDataSetEmpty                  );
  RestoreResString(@DBConsts.SFieldTypeMismatch             );
  RestoreResString(@DBConsts.SFieldRangeError               );
  RestoreResString(@DBConsts.SInvalidIntegerValue           );
  RestoreResString(@DBConsts.SInvalidFloatValue             );
  RestoreResString(@DBConsts.SInvalidBoolValue              );
  RestoreResString(@DBConsts.SNotEditing                    );
  RestoreResString(@DBConsts.SDataSetReadOnly               );
  RestoreResString(@DBConsts.SFieldReadOnly                 );
  RestoreResString(@DBConsts.SNotIndexField                 );
  RestoreResString(@DBConsts.SFieldRequired                 );
  RestoreResString(@DBConsts.SFieldAccessError              );
  RestoreResString(@DBConsts.SFieldValueError               );
  RestoreResString(@DBConsts.SInvalidCalcType               );
  RestoreResString(@DBConsts.SInvalidFieldSize              );
  RestoreResString(@DBConsts.SCircularDataLink              );
  RestoreResString(@DBConsts.SFieldIndexError               );
  RestoreResString(@DBConsts.SNoFieldIndexes                );
  RestoreResString(@DBConsts.SIndexFieldMissing             );
  RestoreResString(@DBConsts.SDuplicateFieldName            );
  RestoreResString(@DBConsts.SDuplicateIndexName            );
  RestoreResString(@DBConsts.SFieldNameMissing              );
  RestoreResString(@DBConsts.SNoIndexForFields              );
  RestoreResString(@DBConsts.STextFalse                     );
  RestoreResString(@DBConsts.STextTrue                      );
  RestoreResString(@DBConsts.SFirstRecord                   );
  RestoreResString(@DBConsts.SPriorRecord                   );
  RestoreResString(@DBConsts.SNextRecord                    );
  RestoreResString(@DBConsts.SLastRecord                    );
  RestoreResString(@DBConsts.SInsertRecord                  );
  RestoreResString(@DBConsts.SDeleteRecord                  );
  RestoreResString(@DBConsts.SEditRecord                    );
  RestoreResString(@DBConsts.SPostEdit                      );
  RestoreResString(@DBConsts.SCancelEdit                    );
  RestoreResString(@DBConsts.SRefreshRecord                 );
  RestoreResString(@DBConsts.SDeleteRecordQuestion          );
  RestoreResString(@DBConsts.SDeleteMultipleRecordsQuestion );
  RestoreResString(@DBConsts.STooManyColumns                );
  RestoreResString(@DBConsts.SLookupInfoError               );
  RestoreResString(@DBConsts.SDataSourceChange              );
  RestoreResString(@DBConsts.SExprTermination               );
  RestoreResString(@DBConsts.SExprNameError                 );
  RestoreResString(@DBConsts.SExprStringError               );
  RestoreResString(@DBConsts.SExprInvalidChar               );
  RestoreResString(@DBConsts.SExprNoRParen                  );
  RestoreResString(@DBConsts.SExprExpected                  );
  RestoreResString(@DBConsts.SExprBadField                  );
  RestoreResString(@DBConsts.SExprBadNullTest               );
  RestoreResString(@DBConsts.SExprRangeError                );
  RestoreResString(@DBConsts.SExprNotBoolean                );
  RestoreResString(@DBConsts.SExprIncorrect                 );
  RestoreResString(@DBConsts.SExprNothing                   );
  RestoreResString(@DBConsts.SNotReplicatable               );
  RestoreResString(@DBConsts.SPropDefByLookup               );
  RestoreResString(@DBConsts.SDataSourceFixed               );
  RestoreResString(@DBConsts.SFieldOutOfRange               );
  RestoreResString(@DBConsts.SBCDOverflow                   );
  RestoreResString(@DBConsts.SInvalidVarByteArray           );
{$ENDIF}
end;

{$IFDEF _D3_}
initialization

finalization
  FreeResStringsDBConsts;
{$ENDIF}
end.
