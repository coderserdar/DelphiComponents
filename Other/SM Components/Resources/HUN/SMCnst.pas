unit SMCnst;

interface

{Hungarian strings}
const
  strMessage = 'Nyomtatás...';
  strSaveChanges = 'A változásokat valóban tárolni akarod az adatbázis szerveren?';
  strErrSaveChanges = 'Az adatokat nem lehet menteni! Ellenõrizd a szerver kapcsolatot vagy az adatot!';
  strDeleteWarning = 'Valóban törölni akarod a(z) %s táblát?';
  strEmptyWarning = 'Valóban üríteni akarod a(z) %s táblát?';

const
  PopUpCaption: array [0..22] of string[44] =
   ('Új rekord',
    'Rekord beszúrás',
    'Rekord javítás',
    'Rekord törlés',
    '-',
    'Nyomtatás ...',
    'Export ...',
    '-',
    'Javítások mentése',
    'Javítások elhagyása',
    'Frissítés',
    '-',
    'Rekordok kijelölése',
       'Rekord jelölése',
       'Minden rekord jelölése',
       '-',
       'A rekord jelölés megszüntetése',
       'Az összes rekord jelölés megszüntetése',
    '-',
    'Az oszlop helyzetének mentése',
    'Az oszlop helyzetének visszaállítása',
    '-',
    'Beállítás...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Cím ';
  SgbData = ' Adat ';
  STitleCaption = 'Felirat:';
  STitleAlignment = 'Kijelölés:';
  STitleColor = 'Háttér:';
  STitleFont = 'Betû:';
  SWidth = 'Szélesség:';
  SWidthFix = 'karakter';
  SAlignLeft = 'bal';
  SAlignRight = 'jobb';
  SAlignCenter = 'közép';
  // added Varga Zoltán varga.zoltan@nml.hu
  SApplyAll ='Mindre';

const //for TSMDBFilterDialog
  strEqual = '= egyenlõ';
  strNonEqual = '<> nem egyenlõ';
  strNonMore = 'nem nagyobb';
  strNonLess = 'nem kisebb';
  strLessThan = '< kisebb';
  strLargeThan = '> nagyobb';
  strExist = 'üres';
  strNonExist = 'nem üres';
  strIn = 'listában';
  strBetween = 'között';

  strOR = 'VAGY';
  strAND = 'ÉS';

  strField = 'Mezõ';
  strCondition = 'Feltétel';
  strValue = 'Érték';

  strAddCondition = ' A következõ feltétel megadása:';
  strSelection = ' A rekord kijelölése a következõ feltétel szerint:';

  strAddToList = 'Hozzáadás a listához';
  strEditInList = 'Lista szerkesztése';
  strDeleteFromList = 'Törlés a listából';

  strTemplate = 'Szürõ dialógus';
  strFLoadFrom = 'Betölt...';
  strFSaveAs = 'Mentés más néven..';
  strFDescription = 'Lerás';
  strFFileName = 'Állomány név';
  strFCreate = 'Létrehozva: %s';
  strFModify = 'Módosítva: %s';
  strFProtect = 'Írásvédett';
  strFProtectErr = 'Védett állomány!';

const //for SMDBNavigator
  SFirstRecord = 'Elsõ rekord';
  SPriorRecord = 'Elõzõ rekord';
  SNextRecord = 'Következõ rekord';
  SLastRecord = 'Utolsó rekord';
  SInsertRecord = 'Rekord beszúrása';
  SCopyRecord = 'Rekord másolása';
  SDeleteRecord = 'Rekord törlése';
  SEditRecord = 'Rekord javítása';
  SFilterRecord = 'Szûrõ feltétel';
  SFindRecord = 'Rekord keresés';
  SPrintRecord = 'Rekord nyomtsatás';
  SExportRecord = 'Rekord exportálása';
  SPostEdit = 'Változások mentése';
  SCancelEdit = 'Változások elhagyása';
  SRefreshRecord = 'Adatok frissítése';
  SChoice = 'Rekord kijelölés';
  SClear = 'Rekord kijelölés megszüntetése';
  SDeleteRecordQuestion = 'Törlöd a rekordot?';
  SDeleteMultipleRecordsQuestion = 'Valóban törölni akarod a kijelölt rekordokat?';
  SRecordNotFound = 'nincs találat';

  SFirstName = 'Elsõ';
  SPriorName = 'Elõzõ';
  SNextName = 'Következõ';
  SLastName = 'Utolsó';
  SInsertName = 'Beszúr';
  SCopyName = 'Másol';
  SDeleteName = 'Töröl';
  SEditName = 'Javít';
  SFilterName = 'Szûrõ';
  SFindName = 'Keres';
  SPrintName = 'Nyomtat';
  SExportName = 'Export';
  SPostName = 'Ment';
  SCancelName = 'Elhagy';
  SRefreshName = 'Frissít';
  SChoiceName = 'Választ';
  SClearName = 'Töröl';

  SBtnOk = '&OK';
  SBtnCancel = '&Elhagy';
  SBtnLoad = 'Tölt';
  SBtnSave = 'Ment';
  SBtnCopy = 'Másol';
  SBtnPaste = 'Beilleszt';
  SBtnClear = 'Töröl';

  SRecNo = ' # ';
  SRecOf = ' / ';

const //for EditTyped
  etValidNumber = 'érvényes szám';
  etValidInteger = 'érvényes egész szám';
  etValidDateTime = 'érvényes dátum/idõ';
  etValidDate = 'érvényes dátum';
  etValidTime = 'érvényes idõ';
  etValid = 'érvényes';
  etIsNot = 'nem egy';
  etOutOfRange = 'Az érték %s az értékhatáron kívül esik %s..%s';

  dbanOf = '';

implementation

end.
