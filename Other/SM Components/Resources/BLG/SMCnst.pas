unit SMCnst;

interface

{Bulgarian strings }
//translated by Nikolay Marinov marinov@bg-soft.com
const
  strMessage = 'Печат...';
  strSaveChanges = 'Наистина ли желаете да съхраните промените на сървъра?';
  strErrSaveChanges = 'Не може да се запишат промените! Проверете връзката със сървъра или валидността на данните.';
  strDeleteWarning = 'Наистина ли желаете да изтриете таблицата %s?';
  strEmptyWarning = 'Наистина ли желаете да изтриете всички записи от таблицата %s?';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Добави запис',
    'Вмъкни запис',
    'Редактирай запис',
    'Изтрий запис',
    '-',
    'Печат ...',
    'Експорт ...',
    '-',
    'Съхрани промените',
    'Откажи промените',
    'Освежи данните',
    '-',
    'Маркирай/Размаркирай записи',
       'Маркирай запис',
       'Маркирай всички записи',
       '-',
       'Размаркира запис',
       'Размаркирай всички записи',
    '-',
    'Запази изгледа на колоната',
    'Възстанови изгледа на колоната',
    '-',
    'Setup...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Заглавие ';
  SgbData = ' Данни ';
  STitleCaption = 'Надпис:';
  STitleAlignment = 'Подравняване:';
  STitleColor = 'Основа:'; 
  STitleFont = 'Шрифт:';
  SWidth = 'Ширина:';
  SWidthFix = 'букви';
  SAlignLeft = 'ляво';
  SAlignRight = 'дясно';
  SAlignCenter = 'центрирано';
  
const //for TSMDBFilterDialog
  strEqual = 'равно';
  strNonEqual = 'различно';
  strNonMore = 'не по-голямо';
  strNonLess = 'не по-малко';
  strLessThan = 'по-малко от';
  strLargeThan = 'по-голямо от';
  strExist = 'празно';
  strNonExist = 'не е празно';
  strIn = 'в лист';
  strBetween = 'между';

  strOR = 'ИЛИ';
  strAND = 'И';

  strField = 'Поле';
  strCondition = 'Условие';
  strValue = 'Стойност';

  strAddCondition = ' Дефиниране на допълнителни условия:';
  strSelection = ' Маркирай записи при следните условия:';

  strAddToList = 'Добави в листа';
  strDeleteFromList = 'Изтрий от листа';

  strTemplate = 'Филтърни шаблони прозорец';
  strFLoadFrom = 'Вземи от...';
  strFSaveAs = 'Съхрани като..';
  strFDescription = 'Описание';
  strFFileName = 'Име на файл';
  strFCreate = 'Създаден: %s';
  strFModify = 'Променен: %s';
  strFProtect = 'Защита от презапис';
  strFProtectErr = 'Файлът е защитен!';

const //for SMDBNavigator
  SFirstRecord = 'Първи запис';
  SPriorRecord = 'Предходен запис';
  SNextRecord = 'Следваш запис';
  SLastRecord = 'Последен запис';
  SInsertRecord = 'Вмъкни запис';
  SCopyRecord = 'Копирай запис';
  SDeleteRecord = 'Изтрий запис';
  SEditRecord = 'Промени запис';
  SFilterRecord = 'Условия за филтър';
  SFindRecord = 'Търсене на запис';
  SPrintRecord = 'Печат на записи';
  SExportRecord = 'Експорт на записи';
  SPostEdit = 'Съхрани промените';
  SCancelEdit = 'Отказ на промените';
  SRefreshRecord = 'Освежи данните';
  SChoice = 'Избери запис';
  SClear = 'Изчисти избраното';
  SDeleteRecordQuestion = 'Изтриване на запис?';
  SDeleteMultipleRecordsQuestion = 'Наистина ли желаете да изтриете маркираните записи?';
  SRecordNotFound = 'Записът не е намерен';

  SFirstName = 'Първи';
  SPriorName = 'Предходен';
  SNextName = 'Следващ';
  SLastName = 'Последен';
  SInsertName = 'Вмъкни';
  SCopyName = 'Копирай';
  SDeleteName = 'Изтрий';
  SEditName = 'Редактирай';
  SFilterName = 'Филтър';
  SFindName = 'Намери';
  SPrintName = 'Печат';
  SExportName = 'Експорт';
  SPostName = 'Запиши';
  SCancelName = 'Отказ';
  SRefreshName = 'Освежи';
  SChoiceName = 'Избери';
  SClearName = 'Изчисти';

  SBtnOk = '&OK';
  SBtnCancel = '&Отказ';
  SBtnLoad = 'Зареди';
  SBtnSave = 'Запиши';
  SBtnCopy = 'Копирай';
  SBtnPaste = 'Вмъкни';
  SBtnClear = 'Изчисти';

  SRecNo = 'запис.';
  SRecOf = ' от ';

const //for EditTyped
  etValidNumber = 'валидно число';
  etValidInteger = 'валидно цяло число';
  etValidDateTime = 'валидна дата/час';
  etValidDate = 'валидна дата';
  etValidTime = 'валиден час';
  etValid = 'валиден';
  etIsNot = 'не е';
  etOutOfRange = 'Стойността %s е вън от допустимото %s..%s';

implementation

end.
