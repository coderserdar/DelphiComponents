{*******************************************************}
{                                                       }
{       Vladimir Gaitanoff Delphi VCL Library           }
{       DBCnstRC - Italian DBConsts messages            }
{       Translated by Alex Zanello alexza@mail.nauta.it }
{       Copyright (c) 1997, 1998                        }
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
  SDataSetOpen                   = 'Operazione impossibile su un dataset aperto';
  SDataSetClosed                 = 'Operazione impossibile su un dataset chiuso';

{$IFDEF _D4_}
  SFieldNotFound                 = 'Non trovo il campo ''%s''';
{$ELSE}
  SFieldNotFound                 = '%s: Non trovo il campo  ''%s''';
{$ENDIF}
  SDataSetMissing                = 'Il campo ''%s'' non ha dataset';
  SDataSetEmpty                  = 'Operazione impossibile su un dataset vuoto';
  SFieldTypeMismatch             = 'Il campo ''%s'' non è del tipo atteso';

  SFieldAccessError              = 'Impossibile accedere al campo ''%s'' di tipo %s';
  SFieldValueError               = 'Valore non corretto per il campo ''%s''';
  SFieldRangeError               = '%g non è un valore valido per il campo ''%s''. La gamma permessa va da %g a %g';
  SInvalidIntegerValue           = '''%s'' non è un valore intero valido per il campo ''%s''';
  SInvalidBoolValue              = '''%s'' non è un valore boolean valido per il campo ''%s''';
  SInvalidFloatValue             = '''%s'' non è un valore float valido per il campo ''%s''';

  SNotEditing                    = 'Il dataset non è in modo modifica o inserimento';

  SDataSetReadOnly               = 'Impossibile modificare un dataset a sola lettura';

  SInvalidCalcType               = 'Il campo ''%s'' non può essere calcolato o di lookup';
  SFieldReadOnly                 = 'Il campo ''%s'' non può essere modificato';
  SNotIndexField                 = 'Il campo ''%s'' non è indicizzato o non può essere modificato';
  SInvalidVarByteArray           = 'Tipo o dimensione di variant non valido';
  SFieldOutOfRange               = 'Il valore del campo ''%s'' è fuori limiti';
  SBCDOverflow                   = '(Overflow/Superamento limiti)';
  SFieldRequired                 = 'Il campo ''%s'' deve avere un valore';

  SInvalidFieldSize              = 'Dimensione non valida del campo';


  SFieldIndexError               = 'L''indice del campo è fuori limiti';

  SNoFieldIndexes                = 'Nessun indice attivo';
  SIndexFieldMissing             = 'Impossibile accedere al campo indicizzato ''%s''';
  SFieldNameMissing              = 'Nome di campo mancante';
  SDuplicateFieldName            = 'Nome di campo duplicato ''%s''';
  SDuplicateIndexName            = 'Nome di indice duplicato ''%s''';
  SNoIndexForFields              = '''%s'' non c''è indice per il campo ''%s''';
  SCircularDataLink              = 'Riferimento circolare non consentito';

  STextFalse                     = 'Falso';
  STextTrue                      = 'Vero';

  SFirstRecord                   = 'Primo record';
  SPriorRecord                   = 'Record precedente';
  SNextRecord                    = 'Record successivo';
  SLastRecord                    = 'Ultimo record';
  SInsertRecord                  = 'Inserisco un record';
  SDeleteRecord                  = 'Cancello un record';
  SEditRecord                    = 'Modifico un record';
  SPostEdit                      = 'Applico le modifiche';
  SCancelEdit                    = 'Annullo le modifiche';
  SRefreshRecord                 = 'Rinfresco i dati';
  SDeleteRecordQuestion          = 'Cancello il record ?';
  SDeleteMultipleRecordsQuestion = 'Cancello tutti i record selezionati ?';

  STooManyColumns                = 'Più di 256 colonne nella grid';
  SLookupInfoError               = 'L''informazione di riferimento per il campo "%s" è incompleta';
  SDataSourceChange              = 'Il DataSource non può essere modificato';

  SExprTermination               = 'Espressione filtro terminata in modo sbagliato';
  SExprNameError                 = 'Nome di campo non terminato';
  SExprStringError               = 'Stringa costante non terminata';
  SExprInvalidChar               = 'Carattere filtro non corretto : ''%s''';
  SExprNoRParen                  = ''')'' attesa ma %s trovata';
  SExprExpected                  = 'Espressione attesa ma "%s" trovata';
  SExprBadField                  = 'Il campo ''%s'' non può essere usato in un filtro';
  SExprBadNullTest               = 'NULL è permesso solo con ''='' e ''<>''';
  SExprRangeError                = 'Costante fuori limiti';
  SExprNotBoolean                = 'Il campo ''%s'' non è booleano';
  SExprIncorrect                 = 'Espressione filtro mal formattata';
  SExprNothing                   = 'Niente';

  SDataSourceFixed               = 'Operazione non permessa in una DBCtrlGrid';
  SNotReplicatable               = 'Controllo non utilizzabile in una DBCtrlGrid';
  SPropDefByLookup               = 'Proprietà già definita nel campo lookup';

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
