unit bvLocalization;

interface

resourcestring
      StrErrorChangeLevel='Ошибка при смене Level!'+#13+'Не указана база данных для файла!';
      StrErrorCannotUnLock='Ошибка при разблокировке таблицы';
      StrErrorCannotReadState='Не могу считать состояние таблицы!';
      StrErrorCannotLock='Не могу заблокировать таблицу ';

      StrErrorTableIsClosed='Таблица не открыта.';
      StrErrorCannotWriteToThisFormat= 'Не знаю способов сохранить в таблице указанного формата';
      StrErrorCannotSaveData='Не могу сохранить данные';

      StrErrorNotDefinedGrid='Не определена сетка.';
      StrErrorNotDefinedObject='Не определен объект, к которому обращается сетка.';
      StrErrorNotDefinedTable='Не определена таблица, к которой обращается сетка.';
      StrErrorNotDefinedSource='Не указано, где сохранять таблицу!';

      StrErrorForConfirmPasswords='2 версии одного пароля не совпадают.'+#13+#13
                 +'угадайте - какая из них неправильная? ;-)))';

      StrErrorNotFoundFile='Не найден файл ';

      StrContinue='Повторить';

      StrSavingTable='Идет сохранение таблицы...';
      StrCopyingOfFiles='Идет копирование файлов';
      StrCopying='Идет копирование';


      /////////////



      StrBookMarks='Закладки';

      StrOkOperation='Операция выполнена успешно.';

      StrBDEAlias='Системная рабочая область (BDE)';
      StrTable='Таблица';

      StrNewPassword='Новый пароль';
      StrMaskOfPassword='в целях безопасности покажем его звездочками';
      StrConfirmPassword='Подтверждение пароля';
      StrChangePassword='Сменить пароль';
      StrChangePasswordForm='Изменение паролей в таблицах';

      StrEditorCaption='Редактор таблицы';

      StrDelete='Удалить';
      StrInsert='Вставить';
      StrReadOnly='Только для чтения';

      StrIncFont='Увеличить шрифт';
      StrDecFont='Уменьшить шрифт';

      StrByDefault='По умолчанию';
      StrCancel='Отменить';
      StrOptions='Опции';
      StrColumns='Колонки';
      StrStrippedRows='Чередующиеся строки';
      StrFixedCols='Закрепленные колонки';
      StrTitleMinHeight='Миним. высота заголовка';
      StrCellRows='Высота ячейки';

      StrEnter2Tab='ENTER -> TAB';
      StrTitle='Заголовок';
      StrContents='Содержание';
      StrColor='Цвет';
      StrWindowColor='под цвет окна';
      StrEach='каждый';
      StrY='-й';
      StrAlwaysShowEditor='Всегда показывать редактируемую ячейку';
      StrAlwaysShowSelection='Всегда показывать выделенную ячейку';
      StrCellHint='Подсказка для ячеек, в которых не помещается их содержимое';
      StrHorzLines='Прорисовывать горизонтальные простые линии';
      StrVertLines='Прорисовывать вертикальные простые линии';
      StrIndicator='Показывать индикатор';
      StrTabStop='Выход при табуляции';
      StrMultiSelect='Выделение нескольких линий';
      StrSelectRows='Выделять строку целиком';
      StrTitleHint='Показывать заголовок';


resourcestring
      BoolTrueText='Да';
      BoolFalseText='Нет';

resourcestring
      StrLeftAlignMent='Левое';
      StrCenterAlignMent='По центру';
      StrRightAlignMent='Правое';

resourcestring
      StrField='Поле';
      StrColumn='Колонка';
      StrWidth='Ширина';
      StrVisible='Видимо';
      StrAlignment='Выравнивание';
      StrTitleAlignment='Выравн.Заголовка';
      StrwhatNumber='Что это за число?';
      StrFutureSaving='Данные изменения придут в действие только после перезагрузки формы';

resourcestring
      StrConfirm='Подтверждение';
      StrConfirmOperation='Подтвердите операцию';
      StrYes='Да';
      StrNO='Нет';

resourcestring
      cftPrinter='Отправить печать сразу на принтер?';
      cfButtOk='Принтер';
      cfButtCancel='Экран';

resourcestring
      StrYesForAll='Да, для всех!';

resourcestring
      StrCopyTab2TabCaption='Копирование таблиц  (<--)';

      StrGetVarCaption='Восстановить поля';
      StrSetVarCaption='Сохранить поля';
      StrConfirmChangesInFieldMap='Сохранить изменения в настройках полей?';

      StrAccept='Принять';
      StrCompSelCols='Сопоставить выделенные колонки';
      StrUnCompSelCols='Открепить выделенные колонки';

      StrClearAll='Очистить все';
      StrVar='Вариант:';
      StrFieldsInSource='Поля в источнике';
      StrFieldsInDestination='Поля в приемнике';
      StrChoiseFields='Сопоставить поля:';
      StrFilter='Фильтр';

resourcestring
      StrErrorReadProperties= 'Ошибка при проверке настроек таблиц!';
      StrMessage='Сообщение';

resourcestring
      StrQuickPrint='Быстрая печать';
      StrSaveToFile='Сохранить в файле';
      strSeeList='Просмотр списка';

      StrNotFound='Ничего не нашел';
      StrFinderCaption='Поиск в таблице';
      StrTextCaption='Текст:';
      StrOneColumnCaption='Только по данной колонке';
      StrWholeWordsOnly='Только слово целиком';
      StrMatchCase='С учетом регистра';
      StrDirection='Направление';
      StrUp='Вверх';
      StrDown='Вниз';
      StrFind='Поиск (F3)';

      StrError='Ошибка';

      StrNewColumn='Новая колонка';
      StrChoiceField='Укажите поле:';

      StrSaveColumns='Сохранить колонки:';
      StrSelectAll='Пометить все';
      StrSelectNone='Снять все пометки';
      StrShowResult='Показать результирующую таблицу';
      StrUseBDEDriver='Использовать BDE-драйвер';



      StrTitleOfColumn='Заголовок колонки';
      StrRestoreProperties='Загрузить настройки';
      StrVersion='Версия таблицы';

      StrOpenedFrom='открыта из ';
      StrSavingToExcel='Сохранение в Excel';

      StrCheckPath='Проверьте правильность указанного пути!';
      StrPrintFile='Печать файла';
      StrConfirmReplaceFile='Заменить существующий файл?';
      StrConfirmClearFile='Очистить файл?';

      StrSeeDoc='  Просмотр документа';
      StrFile='Файл';
      StrClose='Закрыть';
      StrOpen='Открыть';
      StrClear='Очистить';
      StrSave='Сохранить';
      StrPrint='Печать';
      StrSeeTable='Просмотр таблицы';


const
      ArrNavigatorHints:array[1..10] of string=
       (
        'Первая запись',
        'Предыдущая запись',
        'Следующая запись',
        'Последняя запись',
        'Добавить',
        'Удалить',
        'Редактировать',
        'Подтвердить изменения',
        'Отменить изменения',
        'Обновить информацию'
       );


const
     ArrMonth:array[1..12] of string=(
      'Января',
      'Февраля',
      'Марта',
      'Апреля',
      'Мая',
      'Июня',
      'Июля',
      'Августа',
      'Сентября',
      'Октября',
      'Ноября',
      'Декабря'
      );

resourcestring
      StrBadDataTimeFormat='Неправильный формат даты или номера!';
      StrBadIntegerFormat='Неправильный формат целого';
      StrBadFormatOfNumber='Неправильный формат числа';
      StrBadFormatOfCurrency='Неправильный денежный формат';

const
      AltChars:set of char=['А','Б','В','Г','Д','Е','Ж','З','И','Й','К',
              'Л','М','Н','О','П','Р','С','Т','У','Ф','Х',
              'Ц','Ч','Ш','Щ','Ъ','Ы','Ь','Э','Ю','Я'];

      AltSmallChars:set of char=['а','б','в','г','д','е','ж','з','и','й','к',
              'л','м','н','о','п','р','с','т','у','ф','х',
              'ц','ч','ш','щ','ъ','ы','ь','э','ю','я'];

resourcestring
     StrOneMoment='Одну минутку';
     StrIsWorkPleaseWait='Идет работа. Пожалуйста, ждите!';

     StrFont='Шрифт';

     StrGridEditor='Редактор таблицы';

     StrExit='Выход';

     StrErrorSetupNotIndicated ='Не указана настройка';

     StrErrorSetupNotFound = 'Данная настройка пока еще не существует'+#13+
                'Она будет создана после удачного приема файла';


implementation

end.
