{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DBCnstRC - Russian DBConsts messages          }
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
  SDataSetOpen                   = 'Bewerking kan niet op een open dataset uitgevoerd worden';
  SDataSetClosed                 = 'Bewerking kan niet op een gesloten dataset uitgevoerd worden';

{$IFDEF _D4_}
  SFieldNotFound                 = 'Veld ''%s'' niet gevonden';
{$ELSE}
  SFieldNotFound                 = '%s: Veld ''%s'' niet gevonden';
{$ENDIF}
  SDataSetMissing                = 'Veld ''%s'' heeft geen dataset';
  SDataSetEmpty                  = 'Bewerking kan niet op een lege dataset uitgevoerd worden';
  SFieldTypeMismatch             = 'Veld ''%s'' is niet van een verwacht type';

  SFieldAccessError              = 'Veld kan niet gebruikt worden ''%s'' als type %s';
  SFieldValueError               = 'Ongeldige waarde voor veld ''%s''';
  SFieldRangeError               = '%g is geen geldige waarde voor veld ''%s''. Het toegestane bereik is %g tot %g';
  SInvalidIntegerValue           = '''%s'' is geen geldige integer waarde voor veld ''%s''';
  SInvalidBoolValue              = '''%s'' is geen geldige boolean waarde voor veld ''%s''';
  SInvalidFloatValue             = '''%s'' is geen geldige float waarde voor veld ''%s''';

  SNotEditing                    = 'Dataset is niet in de toevoegen of bewerken modus';

  SDataSetReadOnly               = 'Een alleen lezen dataset kan niet gewijzigd worden';

  SInvalidCalcType               = 'Veld ''%s'' kan geen look up of berekend veld zijn';
  SFieldReadOnly                 = 'Veld ''%s'' kan niet worden aangepast';
  SNotIndexField                 = 'Veld ''%s'' is niet geindexeerd en kan niet aangepast worden';
  SInvalidVarByteArray           = 'Ongeldige variant type of grootte';
  SFieldOutOfRange               = 'Waarde van veld ''%s'' valt buiten toegestane bereik';
  SBCDOverflow                   = '(Overflow)';
  SFieldRequired                 = 'Veld ''%s'' moet een waarde bevatten';

  SInvalidFieldSize              = 'Ongeldige veld grootte';


  SFieldIndexError               = 'Veld index valt buiten toegestane bereik';

  SNoFieldIndexes                = 'No index currently active';
  SIndexFieldMissing             = 'Kan het index veld ''%s'' niet aanspreken';
  SFieldNameMissing              = 'Veld naam niet aanwezig';
  SDuplicateFieldName            = 'Dubbele veldnaam ''%s''';
  SDuplicateIndexName            = 'Dubbele index naam ''%s''';
  SNoIndexForFields              = '''%s'' heeft geen index op de velden ''%s''';
  SCircularDataLink              = 'Circulaire datalinks zijn niet toegestaan';

  STextFalse                     = 'Onwaar';
  STextTrue                      = 'Waar';

  SFirstRecord                   = 'Eerste record';
  SPriorRecord                   = 'Vorige record';
  SNextRecord                    = 'Volgende record';
  SLastRecord                    = 'Laatste record';
  SInsertRecord                  = 'Record invoegen';
  SDeleteRecord                  = 'Record wissen';
  SEditRecord                    = 'Record bewerken';
  SPostEdit                      = 'Bewerkingen opslaan';
  SCancelEdit                    = 'Bewerkingen annuleren';
  SRefreshRecord                 = 'Data verversen';
  SDeleteRecordQuestion          = 'Record wissen?';
  SDeleteMultipleRecordsQuestion = 'All geselecteerde records wissen?';

  STooManyColumns                = 'Grid wil meer dan 256 kolommen laten zien, hetgeen meer is dan toegestaan';
  SLookupInfoError               = 'Lookup informatie voor veld ''%s'' is incompleet';
  SDataSourceChange              = 'DataSource kan niet aangepast worden';

  SExprTermination               = 'Filter expressie is niet correct afgesloten met een teken';
  SExprNameError                 = 'Veldnaam is niet correct afgesloten met een teken';';
  SExprStringError               = 'String constante is niet correct afgesloten met een teken';
  SExprInvalidChar               = 'Ongeldig karakter in filter expressie: ''%s''';
  SExprNoRParen                  = ''')'' verwacht maar %s gevonden';
  SExprExpected                  = 'Expressie verwacht maar %s gevonden';
  SExprBadField                  = 'Veld ''%s'' kan niet in een filter expressie gebruikt worden';
  SExprBadNullTest               = 'NULL is alleen toegestaan met ''='' en ''<>''';
  SExprRangeError                = 'Constante valt buiten toegestane bereik';
  SExprNotBoolean                = 'Veld ''%s'' is niet van het type Boolean';
  SExprIncorrect                 = 'Onjuist gebouwde filter expressie';
  SExprNothing                   = 'niets';

  SDataSourceFixed               = 'Bewerking niet toegestaan in een DBCtrlGrid';
  SNotReplicatable               = 'Control kan niet in een DBCtrlGrid gebruitk worden';
  SPropDefByLookup               = 'Property reeds gedefinieerd door een lookup field';

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
