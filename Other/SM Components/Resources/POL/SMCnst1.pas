{ Copyright (C) 1998-2006, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization
  freeware SMComponent library
}
unit SMCnst;

interface

{English strings}
const
  strMessage = 'Drukuj...';
  strSaveChanges = 'Napewno zapisaæ zmiany w bazie danych?';
  strErrSaveChanges = 'Nie mozna zapisac dancyh! Sprawdz po³¹czenie z serwerem i poprawnoœæ danych.';
  strDeleteWarning = 'Napewno usun¹æ tabelê %s?';
  strEmptyWarning = 'Napewno wyczyœciæ tabelê %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Dodaj rekord',
    'Wpisz rekord',
    'Edytuj rekord',
    'Usuñ rekord',
    '-',
    'Drukuj ...',
    'Exportuj ...',
    'Filtry ...',
    'Szukaj ...',
    '-',
    'Zapisz zmiany',
    'Cofnij zmiany',
    'Odswiez',
    '-',
    'Zaznacz/Odznacz rekordy',
       'Zaznacz rekord',
       'Zaznacz wszystkie rekordy',
       '-',
       'Odznacz rekord',
       'Odznacz wszystkie rekordy',
    '-',
    'Zapisz ustawienia kolumn',
    'Przywróæ ustawienia kolumn',
    '-',
    'Ustawienia...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Tytu³ ';
  SgbData = ' Dane ';
  STitleCaption = 'Tytu³:';
  STitleAlignment = 'Wyrównanie:';
  STitleColor = 'T³o:'; 
  STitleFont = 'Font:';
  SWidth = 'Szerokoœæ:';
  SWidthFix = 'znaki';
  SAlignLeft = 'do lewej';
  SAlignRight = 'do prawej';
  SAlignCenter = 'wyœrodkuj';
  
const //for TSMDBFilterDialog
  strEqual = 'równe';
  strNonEqual = 'nie równe';
  strNonMore = 'nie wiêksze';
  strNonLess = 'nie mniejsze';
  strLessThan = 'mniejsze ni¿';
  strLargeThan = 'wiêksze ni¿';
  strExist = 'puste';
  strNonExist = 'nie puste';
  strIn = 'na liœcie';
  strBetween = 'miêdzy';
  strLike = 'podobne do';

  strOR = 'lub';
  strAND = 'i';

  strField = 'Pole';
  strCondition = 'Warunek';
  strValue = 'Wartoœæ';

  strAddCondition = ' Zdefiniuj dodatkowy warunek:';
  strSelection = ' Zaznacz rekordy zgodne z warunkiem:';

  strAddToList = 'Dodaj do listy';
  strEditInList = 'Edytuj na liœcie';
  strDeleteFromList = 'Usuñ z listy';

  strTemplate = 'Przyk³adowy dialog filtrowania';
  strFLoadFrom = 'Otwórz z ...';
  strFSaveAs = 'Zapisz jako..';
  strFDescription = 'Opis';
  strFFileName = 'Nazwa pliku';
  strFCreate = 'Utworzono: %s';
  strFModify = 'Zmodyfikowano: %s';
  strFProtect = 'Tylko do odczytu';
  strFProtectErr = 'Plik zabezpieczony przed zapisem!';

const //for SMDBNavigator
  SFirstRecord = 'Pierwszy rekord';
  SPriorRecord = 'Poprzedni rekord';
  SNextRecord = 'Nastêpny rekord';
  SLastRecord = 'Ostatni rekord';
  SInsertRecord = 'Wpisz rekord';
  SCopyRecord = 'Kopiuj rekord';
  SDeleteRecord = 'Usuñ rekord';
  SEditRecord = 'Edytuj rekord';
  SFilterRecord = 'Filtry warunków';
  SFindRecord = 'Szukaj rekordów';
  SPrintRecord = 'Drukuj rekordy';
  SExportRecord = 'Exportuj rekordy';
  SImportRecord = 'Importuj rekordy';
  SPostEdit = 'Zapisz zmiany';
  SCancelEdit = 'Anuluj zmiany';
  SRefreshRecord = 'Odœwie¿ dane';
  SChoice = 'Wybierz rekord';
  SClear = 'Wyczyœæ wybrany rekord';
  SDeleteRecordQuestion = 'Usun¹æ rekord?';
  SDeleteMultipleRecordsQuestion = 'Napewno usun¹æ wybrane rekordy?';
  SRecordNotFound = 'Nie znaleziono rekordu';

  SFirstName = 'Pierwszy';
  SPriorName = 'Poprzedni';
  SNextName = 'Nastêpny';
  SLastName = 'Ostatni';
  SInsertName = 'Wpisz';
  SCopyName = 'Kopiuj';
  SDeleteName = 'usuñ';
  SEditName = 'Edytuj';
  SFilterName = 'Filtr';
  SFindName = 'ZnajdŸ';
  SPrintName = 'Drukuj';
  SExportName = 'Exportuj';
  SImportName = 'Importuj';
  SPostName = 'Zapisz';
  SCancelName = 'Anuluj';
  SRefreshName = 'Odœwie¿';
  SChoiceName = 'Wybierz';
  SClearName = 'Wyczyœæ';

  SBtnOk = '&OK';
  SBtnCancel = '&Anuluj';
  SBtnLoad = 'Otwórz';
  SBtnSave = 'Zapisz';
  SBtnCopy = 'Kopiuj';
  SBtnPaste = 'Wklej';
  SBtnClear = 'Wyczyœæ';

  SRecNo = 'rec.';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = 'poprawny numer';
  etValidInteger = 'poprawna liczba';
  etValidDateTime = 'poprawna data\czas';
  etValidDate = 'poprawna data';
  etValidTime = 'poprawny czas';
  etValid = 'poprawne';
  etIsNot = 'nie jest';
  etOutOfRange = 'Wartoœæ %s jest z poza zakresu %s..%s';

  SApplyAll = 'Zapisz wszystkie';

  SNoDataToDisplay = '<Brak danych do wyœwietlenia>';

implementation

end.
