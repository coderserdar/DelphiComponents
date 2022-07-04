{ Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization
  freeware SMComponent library
}
unit SMCnst;

interface

{Serbian strings}
{Na srpskom jeziku == On Serbian language} //Translated to serbian by TRX...
const
  strMessage = 'Stampaj...';
  strSaveChanges = 'Dali ste sigurni da zelite da snimite promene na serveru?';
  strErrSaveChanges = 'Ne mogu da snimim podatke! Proverite konekciju sa serverom ili proverite podatke.';
  strDeleteWarning = 'Dali stvarno zelite da izbrisete tabelu %s?';
  strEmptyWarning = 'Dali stvarno zelite da ispraznite tabelu %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Dodaj ekord',
    'Ubaci rekord',
    'Obradi rekord',
    'Izbrisi rekord',
    '-',
    'Stampa ...',
    'Izvoz ...',
    'Filter ...',
    'Potraga ...',
    '-',
    'Snimi promene',
    'Odbaci promene',
    'Osvezi',
    '-',
    'Selektuj/Odselektuj rekorde',
       'Selektuj rekord',
       'Selektuj sve rekorde',
       '-',
       'Odselektuj rekord',
       'Odselektuj sve rekorde',
    '-',
    'Snimi raspored kolni',
    'Ucitaj raspored kolni',
    '-',
    'Podesi...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titl ';
  SgbData = ' Podatci ';
  STitleCaption = 'Naslov:';
  STitleAlignment = 'Poravnanje:';
  STitleColor = 'Pozadina:';
  STitleFont = 'Tip slova:';
  SWidth = 'Sirina:';
  SWidthFix = 'Karakteri';
  SAlignLeft = 'Levo';
  SAlignRight = 'Desno';
  SAlignCenter = 'Centar';
  
const //for TSMDBFilterDialog
  strEqual = 'jednako';
  strNonEqual = 'nije jednako';
  strNonMore = 'ne vece';
  strNonLess = 'ne manje';
  strLessThan = 'manje od';
  strLargeThan = 'vece od';
  strExist = 'prazno';
  strNonExist = 'nije prazno';
  strIn = 'u listi';
  strBetween = 'izmedju';
  strLike = 'kao';

  strOR = 'ILI';
  strAND = 'I';

  strField = 'Polje';
  strCondition = 'Uslov';
  strValue = 'Vrednost';

  strAddCondition = ' Definisite dodatni uslov:';
  strSelection = ' Izbirajte podatke po sledecim uslovima:';

  strAddToList = 'Dodaj u listu';
  strEditInList = 'Obradi u listi';
  strDeleteFromList = 'Izbrisi sa liste';

  strTemplate = 'Dijalog filtera sablona';
  strFLoadFrom = 'Ucitaj iz...';
  strFSaveAs = 'Snimi kao..';
  strFDescription = 'Opis';
  strFFileName = 'Ime fajla';
  strFCreate = 'Napravljen: %s';
  strFModify = 'Obradjivan: %s';
  strFProtect = 'Zastititi';
  strFProtectErr = 'Fajl je zasticen!';

const //for SMDBNavigator
  SFirstRecord = 'Prvi rekord';
  SPriorRecord = 'Prethodni rekord';
  SNextRecord = 'Sledeci rekord';
  SLastRecord = 'Zadnji rekord';
  SInsertRecord = 'Ubaci rekord';
  SCopyRecord = 'Kopiraj rekord';
  SDeleteRecord = 'Izbrisi rekord';
  SEditRecord = 'Obradi rekord';
  SFilterRecord = 'Filtriraj uslove';
  SFindRecord = 'Potraga za rekordom';
  SPrintRecord = 'Stampanje rekorda';
  SExportRecord = 'Izvoz rekords';
  SPostEdit = 'Snimi promene';
  SCancelEdit = 'Odbaci promene';
  SRefreshRecord = 'Osvezi podatke';
  SChoice = 'Izbiraj rekord';
  SClear = 'Izbiraj rekord za izbrisati';
  SDeleteRecordQuestion = 'Izbrisati rekord?';
  SDeleteMultipleRecordsQuestion = 'Dali stvarno zelite da izbrisete oznacene rekorde?';
  SRecordNotFound = 'Rekord nije pronadjen';

  SFirstName = 'Prvi';
  SPriorName = 'Prethodni';
  SNextName = 'Sledeci';
  SLastName = 'Zadnji';
  SInsertName = 'Ubaciti';
  SCopyName = 'Kopirati';
  SDeleteName = 'Izbrisati';
  SEditName = 'Obraditi';
  SFilterName = 'Filtrirati';
  SFindName = 'Pronaci';
  SPrintName = 'Stampati';
  SExportName = 'Izvoz';
  SPostName = 'Snimiti';
  SCancelName = 'Odustati';
  SRefreshName = 'Osveziti';
  SChoiceName = 'Birati';
  SClearName = 'Isprazniti';

  SBtnOk = '&OK';
  SBtnCancel = '&Odustati';
  SBtnLoad = 'Ucitati';
  SBtnSave = 'Snimiti';
  SBtnCopy = 'Kopirati';
  SBtnPaste = 'Prebaciti';
  SBtnClear = 'Isprazniti';

  SRecNo = 'rek.';
  SRecOf = ' od ';

const //for EditTyped
  etValidNumber = 'Valjan broj';
  etValidInteger = 'Valjan ceo broj';
  etValidDateTime = 'Valjan datum/vreme';
  etValidDate = 'Valjan datum';
  etValidTime = 'Valjano vreme';
  etValid = 'Valjan';
  etIsNot = 'nije';
  etOutOfRange = 'Vrednost %s je izvan ranga %s..%s';

  SApplyAll = 'Primeni na sve';

  SNoDataToDisplay = '<Nema podataka za prikaz>';

implementation

end.
