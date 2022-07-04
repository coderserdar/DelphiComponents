{ Copyright (C) 1998-2006, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization
  freeware SMComponent library
}
unit SMCnst;

interface

{Italian strings}
const
  strMessage = 'Stampa...';
  strSaveChanges = 'Salvare le modifiche?';
  strErrSaveChanges = 'Impossibile salvare i dati! Controllare la connessione al server o il metodo di verifica dei dati.';
  strDeleteWarning = 'Candellare la tabella %s?';
  strEmptyWarning = 'Svuotare la tabella %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Aggiungi record',
    'Inserisci record',
    'Modifica record',
    'Cancella record',
    '-',
    'Stampa ...',
    'Esporta ...',
    'Filtra ...',
    'Cerca ...',
    '-',
    'Salva modifiche',
    'Annulla modifiche',
    'Rileggi',
    '-',
    'Seleziona / Deseleziona record',
       'Seleziona record',
       'Seleziona tutti i record',
       '-',
       'Deseleziona record',
       'Deseleziona tutti i record',
    '-',
    'Salva colonne',
    'Ripristina colonne',
    '-',
    'Impostazioni...');

const //for TSMSetDBGridDialog
   SgbTitle = 'Titolo';
   SgbData = ' Dati ';
   STitleCaption = 'Intestazione:';
   STitleAlignment = 'Allineamento:';
   STitleColor = 'Sfondo:';
   STitleFont = 'Primo piano:';
   SWidth = 'Altezza:';
   SWidthFix = 'Caratteri';
   SAlignLeft = 'Sinistra';
   SAlignRight = 'Destra';
   SAlignCenter = 'Centro';

const //for TSMDBFilterDialog
  strEqual = 'uguale';
  strNonEqual = 'diverso';
  strNonMore = 'minore o uguale';
  strNonLess = 'maggiore  o uguale';
  strLessThan = 'minore';
  strLargeThan = 'maggiore';
  strExist = 'vuoto';
  strNonExist = 'non vuoto';
  strIn = 'nella lista';
  strBetween = 'compreso tra';
  strLike = 'contiene'; 

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Campo';
  strCondition = 'Condizione';
  strValue = 'Valore';

  strAddCondition = ' Imposta altre condizioni:';
  strSelection = ' Seleziona i record in base alle seguenti condizioni:';

  strAddToList = 'Aggiungi';
  strEditInList = 'Modifica';
  strDeleteFromList = 'Elimina';

  strTemplate = 'Finestra filtri';
  strFLoadFrom = 'Carica da...';
  strFSaveAs = 'Salva in..';
  strFDescription = 'Descrizione';
  strFFileName = 'Nome file';
  strFCreate = 'Creato: %s';
  strFModify = 'Modificato: %s';
  strFProtect = 'Protetto da modifiche';
  strFProtectErr = 'Il file e protetto!';

const //for SMDBNavigator
  SFirstRecord = 'Primo record';
  SPriorRecord = 'Record precedente';
  SNextRecord = 'Record successivo';
  SLastRecord = 'Ultimo record';
  SInsertRecord = 'Inserisci record';
  SCopyRecord = 'Copia record';
  SDeleteRecord = 'Cancella record';
  SEditRecord = 'Modifica record';
  SFilterRecord = 'Filtri';
  SFindRecord = 'Ricerca';
  SPrintRecord = 'Stampa record';
  SExportRecord = 'Esporta i records';
  SImportRecord = 'Importa i records';
  SPostEdit = 'Salva modifiche';
  SCancelEdit = 'Annulla modifiche';
  SRefreshRecord = 'Aggiorna';
  SChoice = 'Scegli un record';
  SClear = 'Annulla la scelta di un record';
  SDeleteRecordQuestion = 'Cancella il record?';
  SDeleteMultipleRecordsQuestion = 'Cancellare i record selezionati?';
  SRecordNotFound = 'Record non trovato';

  SFirstName = 'Primo';
  SPriorName = 'Precedente';
  SNextName = 'Successivo';
  SLastName = 'Ultimo';
  SInsertName = 'Nuovo';
  SCopyName = 'Copia';
  SDeleteName = 'Elimina';
  SEditName = 'Modifica';
  SFilterName = 'Filtro';
  SFindName = 'Trova';
  SPrintName = 'Stampa';
  SExportName = 'Esporta';
  SImportName = 'Importa';
  SPostName = 'Salva';
  SCancelName = 'Annulla';
  SRefreshName = 'Aggiorna';
  SChoiceName = 'Scegli';
  SClearName = 'Cancella';

  SBtnOk = '&OK';
  SBtnCancel = '&Annulla';
  SBtnLoad = 'Apri';
  SBtnSave = 'Salva';
  SBtnCopy = 'Copia';
  SBtnPaste = 'Incolla';
  SBtnClear = 'Svuota';

  SRecNo = 'rec.';
  SRecOf = ' di ';

const //for EditTyped
  etValidNumber = 'numero valido';
  etValidInteger = 'numero intero valido';
  etValidDateTime = 'data/ora valida';
  etValidDate = 'data valida';
  etValidTime = 'ora valida';
  etValid = 'valido';
  etIsNot = 'non e un';
  etOutOfRange = 'Il valore %s non e compreso in %s..%s';

   SApplyAll = 'Applica a tutti';   

  SNoDataToDisplay = '<Nessun dato da visualizzare>';



implementation

end.
