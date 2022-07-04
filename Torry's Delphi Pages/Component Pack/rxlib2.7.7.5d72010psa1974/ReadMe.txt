RxLib Библиотека компонентов Delphi VCL Extensions (RX) Library, 
разработанная Федором Кожевниковым, Сергеем Королевым и Игорем 
Павлюком, представляет собой бесплатный свободно распространяемый 
(freeware) продукт, пользующийся заслуженной популярностью как в 
нашей стране, так и за рубежом.

Проект прекратил свое развитие. Библиотека была включена в состав 
JEDI Visual Component Library. 
Домашняя страница http://homepages.borland.com/jedi/jvcl/

Моя собственная адаптация под Delphi 2009-2010. Основные моменты.
=====================================================================

Я НЕ ГАРАНТИРУЮ, что АДАПТИРОВАНО ВСЁ!!! Но все же ...

Везде где заметил проверил корректность использования строковых типов 
string, Char, PChar. Особенно в случае использования их как буферов.
Особенно подверглись изменению следующие модули: 

1. Модуль RxRichEd.pas:
  - полностью переработаны методы внутреннего класса TRichEditStrings 
    для работы с файлами/потоками LoadFromFile, LoadFromStream, 
    SaveToFile, SaveToStream в соответствии с новыми веяниями CG2009 
    (поддержка соответствующих переопределенных методов с 
    параметром Encoding: TEncoding)
  - Свойство TRxCustomRichEdit.StreamMode - для CG2009 исключен из 
    множества доступных флагов флаг smUnicode:   
    TRichStreamMode = (smSelection, smPlainRtf, 
                       smNoObjects{$IFNDEF RX_D12}, smUnicode{$ENDIF});
    TRichStreamModes = set of TRichStreamMode;
    При этом соотвествующий режим в классе TRichEditStrings включен 
    по умолчанию.

2. Модуль rxDbutils.pas:
  - заменены типы:
    TBookmark заменен на Pointer;
    TBookmarkStr заменен на TBookmark;
    PChar заменен на TRecordBuffer (не везде, а по смыслу). 
    Синтаксис:
    {$IFDEF RX_D12}
      TBookmarkType = TBookmark;
      TBookmarkPointerType = Pointer;
      TBuffer = TRecordBuffer;
    {$ELSE}
      TBookmarkType = TBookmarkStr;
      TBookmarkPointerType = TBookmark;
      TBuffer = PChar;
    {$ENDIF}
    Везде по тексту соответствующие типы заменены на TBookmarkType, 
    TBuffer, TBookmarkPointerType для совместимости с прежними 
    версиями Delphi.

3. Модуль RxMemDS.pas:
  - заменены типы (аналогично пункту 2):
    {$IFDEF RX_D12}
      TBlobDataArray = array of TBlobData;
      TBlobDataArrayType = TBlobDataArray;
      TBlobDataType = TBlobData;
    {$ELSE}
      TMemBlobData = AnsiString;
      TMemBlobArray = array[0..0] of TMemBlobData;
      TBlobDataArrayType = ^TMemBlobArray;
      TBlobDataType = TMemBlobData;
      PMemBlobArray = ^TMemBlobArray;
    {$ENDIF}   
    
4. Модуль rxCheckItm.pas:
  - исправлена ошибка в редакторе свойства Items компоненты CheckListBox.

5. Остальные изменения - см. History.txt.

Результат: 
- Пакеты компирируются без ошибок, хинтов и варнингов. 
- Демосы из стандартной поставки Rxdemo, Riched2, Gifanm32 
  компилируются и нормально работают. Остальные демосы настолько 
  морально устарели, что не стал париться...
- Мои рабочие проекты работают. 

===============================================================
Адаптировано: psa1974 
Обратная связь:
http://forum.ru-board.com/
http://www.dumpz.ru/
