{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com
  Const strings for localization  freeware SMComponent library}

{translation by
- sam francke s.j.francke@hccnet.nl
- Martijn Terwoert www.smarty.nl 
- Hubert Duijvewaardt <hubert@compsol.be> }
unit SMCnst;

interface

{Dutch strings}
const
  strMessage = 'Print...';
  strSaveChanges = 'Wilt u werkelijk de veranderingen bewaren op de Database Server?';
  strErrSaveChanges = 'Kan data niet bewaren ! Check Server verbinding of validate de data.';
  strDeleteWarning = 'Wilt u werkelijk tabel %s verwijderen ?';
  strEmptyWarning = 'Wilt u werkelijk tabel %s leegmaken?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Record toevoegen',
    'Record invoegen',
    'Wijzig record',
    'Verwijder record',
    '-',
    'Print ...',
    'Export ...',
	'Filter ...',
    'Zoeken ...',
    '-',
    'Bewaar veranderingen',
    'Doe veranderingen teniet',
    'Verversen',
    '-',
    'Selecteer/Deselecteer records',
       'Selecteer record',
       'Selecteer alle records',
       '-',
       'Deselecteer record',
       'Deselecteer alle records',
    '-',
    'Bewaar kolom layout',
    'Herstel kolom layout',
    '-',
    'Setup...');

const //for TSMSetDBGridDialog
  SgbTitle = ' titel ';
  SgbData = ' gegevens ';
  STitleCaption = 'kop:';
  STitleAlignment = 'uitlijnen:';
  STitleColor = 'achtergrond:';
  STitleFont = 'font:';
  SWidth = 'breedte:';
  SWidthFix = 'letters';
  SAlignLeft = 'links uitlijnen';
  SAlignRight = 'rechts uitlijnen';
  SAlignCenter = 'centreren';
 
const //for TSMDBFilterDialog
  strEqual = 'gelijk';
  strNonEqual = 'niet gelijk';
  strNonMore = 'niet groter';
  strNonLess = 'niet minder';
  strLessThan = 'minder dan';
  strLargeThan = 'groter dan';
  strExist = 'leeg';
  strNonExist = 'niet leeg';
  strIn = 'in lijst';
  strBetween = 'tussen';
  strLike = 'zelfde';
  
  strOR = 'of';
  strAND = 'en';

  strField = 'Veld';
  strCondition = 'Conditie';
  strValue = 'Waarde';

  strAddCondition = ' Stel aanvullende condities in:';
  strSelection = ' Selecteer de records volgens deze condities:';

  strAddToList = 'Toevoegen aan lijst';
  strEditInList = 'Bewerken in lijst';
  strDeleteFromList = 'Delete van lijst';

  strTemplate = 'Filter template dialoog';
  strFLoadFrom = 'Openen...';
  strFSaveAs = 'Opslaan als..';
  strFDescription = 'Beschrijving';
  strFFileName = 'Bestands naam';
  strFCreate = 'Gemaakt: %s';
  strFModify = 'Aangepast: %s';
  strFProtect = 'Beveilig tegen overschrijven';
  strFProtectErr = 'Bestand is beveiligd!';

const //for SMDBNavigator
  SFirstRecord = 'eerste record';
  SPriorRecord = 'vorige record';
  SNextRecord = 'volgende record';
  SLastRecord = 'laatste record';
  SInsertRecord = 'voeg record in';
  SCopyRecord = 'kopieer record';
  SDeleteRecord = 'verwijder record';
  SEditRecord = 'wijzig record';
  SFilterRecord = 'filter voorwaarden';
  SFindRecord = 'zoek het record';
  SPrintRecord = 'print de records';
  SExportRecord = 'exporteer de records';
  SImportRecord = 'importeer de records';
  SPostEdit = 'bewaar veranderingen';
  SCancelEdit = 'doe veranderingen teniet';
  SRefreshRecord = 'ververs data';
  SChoice = 'kies record';
  SClear = 'Clear record keuze';
  SDeleteRecordQuestion = 'verwijder record?';
  SDeleteMultipleRecordsQuestion = 'alle geselecteerde records verwijderen?';
  SRecordNotFound = 'geen record gevonden';

  SFirstName = 'eerste';
  SPriorName = 'vorige';
  SNextName = 'volgende';
  SLastName = 'laatste';
  SInsertName = 'invoegen';
  SCopyName = 'kopieer';
  SDeleteName = 'verwijder';
  SEditName = 'wijzig';
  SFilterName = 'filter';
  SFindName = 'vind';
  SPrintName = 'print';
  SExportName = 'export';
  SImportName = 'import';
  SPostName = 'bewaar';
  SCancelName = 'afbreken';
  SRefreshName = 'ververs';
  SChoiceName = 'kies';
  SClearName = 'maak leeg';

  SBtnOk = 'OK';
  SBtnCancel = 'annuleren';
  SBtnLoad = 'openen';
  SBtnSave = 'opslaan';
  SBtnCopy = 'kopieren';
  SBtnPaste = 'plakken';
  SBtnClear = 'Clear';
 
  SRecNo = 'rec.';
  SRecOf = ' van ';

const //for EditTyped
  etValidNumber = 'geldig nummer';
  etValidInteger = 'geldig integer nummer';
  etValidDateTime = 'geldige datum/tijd';
  etValidDate = 'geldige datum';
  etValidTime = 'geldige tijd';
  etValid = 'geldig(e)';
  etIsNot = 'is geen';
  etOutOfRange = 'waarde %s buiten bereik %s..%s';

  SApplyAll = 'Toepassen op Alle';

  SNoDataToDisplay = '<No data to display>';
  
implementation

end.
