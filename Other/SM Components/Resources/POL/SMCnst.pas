unit SMCnst;

interface

{Polish strings}
const
  strMessage = 'Wydruk...';
  strSaveChanges = 'Proszê potwierdziæ zapis zmian na serwerze.';
  strErrSaveChanges = 'Brak mo¿liwoœci zapisu danych! SprawdŸ po³¹czenie z serwerem lub poprawnoœæ danych.';
  strDeleteWarning = 'Proszê potwierdziæ usuniêcie tabeli %s.';
  strEmptyWarning = 'Proszê potwierdziæ usuniêcie rekordów z tabeli %s.';

const
  PopUpCaption: array [0..24] of string[40] =
   ('Dodawanie rekordu',
    'Wstawianie rekordu',
    'Edycja rekordu',
    'Kasowanie rekordu',
    '-',
    'Wydruk ...',
    'Eksport ...',
    'Filtr ...',
    'Wyszukiwanie ...',
    '-',
    'Zapis zmian',
    'Odrzucenie zmian',
    'Odœwie¿enie',
    '-',
    'Wybór/Odrzucenie wyboru rekordów',
       'Wybór rekordu',
       'Wybór wszystkich rekordów',
       '-',
       'Odrzucenie wyboru rekordu',
       'Odrzucenie wyboru wszystkich rekordów',
    '-',
    'Zapis uk³adu kolumn',
    'Przywrócenie uk³adu kolumn',
    '-',
    'Ustawienia...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Tytu³ ';
  SgbData = ' Data ';
  STitleCaption = 'Napis:';
  STitleAlignment = 'Wyrównanie:';
  STitleColor = 'T³o:';
  STitleFont = 'Czcionka:';
  SWidth = 'Szerokoœæ:';
  SWidthFix = 'znaki';
  SAlignLeft = 'Do lewej';
  SAlignRight = 'Do prawej';
  SAlignCenter = 'Wyœrodkowanie';

const //for TSMDBFilterDialog
  strEqual = 'równy';
  strNonEqual = 'ró¿ny';
  strNonMore = 'nie wiêkszy ni¿';
  strNonLess = 'nie mniejszy ni¿';
  strLessThan = 'mniejszy ni¿';
  strLargeThan = 'wiêkszy ni¿';
  strExist = 'pusty';
  strNonExist = 'nie pusty';
  strIn = 'na liœcie';
  strBetween = 'pomiêdzy';
  strLike = 'zawiera';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Pole';
  strCondition = 'Warunek';
  strValue = 'Wartoœæ';

  strAddCondition = ' Zdefiniuj warunek dodatkowy:';
  strSelection = ' Wybierz rekordy za pomoc¹ dodatkowych warunków:';

  strAddToList = 'Dodaj do listy';
  strEditInList = 'Edytuj w liœcie';
  strDeleteFromList = 'Usuñ z listy';

  strTemplate = 'Okno do okreœlenia szablonu filtra';
  strFLoadFrom = 'WprowadŸ z ...';
  strFSaveAs = 'Zapisz jako...';
  strFDescription = 'Opis';
  strFFileName = 'Nazwa zbioru';
  strFCreate = 'Utworzono: %s';
  strFModify = 'Zmodyfikowano: %s';
  strFProtect = 'Zabezpieczenie przed zapisem';
  strFProtectErr = 'Zbiór jest zabezpieczony!';

const //for SMDBNavigator
  SFirstRecord = 'Pierwszy rekord';
  SPriorRecord = 'Poprzedni rekord';
  SNextRecord = 'Nastêpny rekord';
  SLastRecord = 'Ostatni rekord';
  SInsertRecord = 'Wstawianie rekordu';
  SCopyRecord = 'Kopiowanie rekordu';
  SDeleteRecord = 'Kasowanie rekordu';
  SEditRecord = 'Edycja rekordu';
  SFilterRecord = 'Warunki filtra';
  SFindRecord = 'Wyszukanie rekordu';
  SPrintRecord = 'Wydruk rekordów';
  SExportRecord = 'Eksport rekordów';
  SImportRecord = 'Import rekordów';
  SPostEdit = 'Zapis zmian';
  SCancelEdit = 'Uniewa¿nienie zmian';
  SRefreshRecord = 'Odœwie¿enie danych';
  SChoice = 'Wybierz rekord';
  SClear = 'Uniewa¿nij wybór rekordu';
  SDeleteRecordQuestion = 'Proszê potwierdziæ usuniêcie rekordu.';
  SDeleteMultipleRecordsQuestion = 'Proszê potwierdziæ usuniêcie wybranych rekordów.';
  SRecordNotFound = 'Nie znaleziono rekordu.';

  SFirstName = 'Pierwszy';
  SPriorName = 'Poprzedni';
  SNextName = 'Nastêpny';
  SLastName = 'Ostatni';
  SInsertName = 'Wstawienie';
  SCopyName = 'Kopiowanie';
  SDeleteName = 'Kasowanie';
  SEditName = 'Edycja';
  SFilterName = 'Filtr';
  SFindName = 'Wyszukanie';
  SPrintName = 'Wydruk';
  SExportName = 'Eksport';
  SImportName = 'Import';
  SPostName = 'Zapis';
  SCancelName = 'Anulowanie';
  SRefreshName = 'Odœwie¿enie';
  SChoiceName = 'Wybór';
  SClearName = 'Wyzerowanie';

  SBtnOk = '&OK';
  SBtnCancel = '&Anuluj';
  SBtnLoad = 'Otwarcie';
  SBtnSave = 'Zapis';
  SBtnCopy = 'Kopiowanie';
  SBtnPaste = 'Wklejenie';
  SBtnClear = 'Wyzerowanie';

  SRecNo = 'rek.';
  SRecOf = ' z ';

const //for EditTyped
  etValidNumber = 'poprawna liczba';
  etValidInteger = 'poprawna liczba ca³kowita';
  etValidDateTime = 'poprawna data/czas';
  etValidDate = 'poprawna data';
  etValidTime = 'poprawny czas';
  etValid = 'poprawny';
  etIsNot = 'nie jest';
  etOutOfRange = 'Wartoœæ %s jest poza zakresem %s..%s';

const //for DMDBAccessNavigator
  dbanOf = 'z';

  SApplyAll = 'Zastosuj do ca³oœci';
  
  SNoDataToDisplay = '<Brak danych do wyœwietlenia>';

implementation

end.
